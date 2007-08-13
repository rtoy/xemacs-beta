;;; gdb.el --- run gdb under Emacs

;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.
;; Keywords: c, unix, tools, debugging

;; Some ideas are due to Masanobu.

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Description of GDB interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using gdb to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command GDB to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Gdb will be invoked on this file, in a
;; window named *gdb-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the gdb command next on \M-n
;; (def-gdb next "\M-n")

;; This causes the emacs command gdb-next to be defined, and runs
;; gdb-display-frame after the command.

;; gdb-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the gdb window.  For example after a gdb-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the gdb buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; gdb-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.

;;; Code:

(require 'comint)
(require 'shell)

(condition-case nil
    (if (featurep 'toolbar)
	(require 'eos-toolbar "sun-eos-toolbar"))
  (error nil))

(defvar gdb-last-frame)
(defvar gdb-delete-prompt-marker)
(defvar gdb-filter-accumulator)
(defvar gdb-last-frame-displayed-p)
(defvar gdb-arrow-extent nil)
(or (fboundp 'make-glyph) (fset 'make-glyph 'identity)) ; work w/ pre beta v12
(defvar gdb-arrow-glyph (make-glyph "=>"))

(make-face 'gdb-arrow-face)
(or (face-differs-from-default-p 'gdb-arrow-face)
   ;; Usually has a better default value than highlight does
   (copy-face 'isearch 'gdb-arrow-face))

;; Hooks can side-effect extent arg to change extent properties
(defvar gdb-arrow-extent-hooks '())

(defvar gdb-prompt-pattern "^>\\|^(.*gdb[+]?) *\\|^---Type <return> to.*--- *"
  "A regexp to recognize the prompt for gdb or gdb+.") 

(defvar gdb-mode-map nil
  "Keymap for gdb-mode.")

(defvar gdb-toolbar
  '([eos::toolbar-stop-at-icon
     gdb-toolbar-break
     t
     "Stop at selected position"]
    [eos::toolbar-stop-in-icon
     gdb-toolbar-break
     t
     "Stop in function whose name is selected"]
    [eos::toolbar-clear-at-icon
     gdb-toolbar-clear
     t
     "Clear at selected position"]
    [eos::toolbar-evaluate-icon
     nil
     nil
     "Evaluate selected expression; shows in separate XEmacs frame"]
    [eos::toolbar-evaluate-star-icon
     nil
     nil
     "Evaluate selected expression as a pointer; shows in separate XEmacs frame"]
    [eos::toolbar-run-icon
     gdb-run
     t
     "Run current program"]
    [eos::toolbar-cont-icon
     gdb-cont
     t
     "Continue current program"]
    [eos::toolbar-step-into-icon
     gdb-step
     t
     "Step into (aka step)"]
    [eos::toolbar-step-over-icon
     gdb-next
     t
     "Step over (aka next)"]
    [eos::toolbar-up-icon
     gdb-up
     t
     "Stack Up (towards \"cooler\" - less recently visited - frames)"]
    [eos::toolbar-down-icon
     gdb-down
     t
     "Stack Down (towards \"warmer\" - more recently visited - frames)"]
    [eos::toolbar-fix-icon	nil	nil	"Fix (not available with gdb)"]
    [eos::toolbar-build-icon
     toolbar-compile
     t
     "Build (aka make -NYI)"]
    ))

(if gdb-mode-map
   nil
  (setq gdb-mode-map (make-sparse-keymap))
  (set-keymap-name gdb-mode-map 'gdb-mode-map)
  (set-keymap-parents gdb-mode-map (list comint-mode-map))
  (define-key gdb-mode-map "\C-l" 'gdb-refresh)
  (define-key gdb-mode-map "\C-c\C-c" 'gdb-control-c-subjob)
  (define-key gdb-mode-map "\t" 'comint-dynamic-complete)
  (define-key gdb-mode-map "\M-?" 'comint-dynamic-list-completions))

(define-key ctl-x-map " " 'gdb-break)
(define-key ctl-x-map "&" 'send-gdb-command)

;;Of course you may use `def-gdb' with any other gdb command, including
;;user defined ones.   

(defmacro def-gdb (name key &optional doc &rest forms)
  (let* ((fun (intern (format "gdb-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
	  (nconc (list 'defun fun '(arg)
		       (or doc "")
		       '(interactive "p")
		       (list 'gdb-call cstr))
		 forms)
	  (and key (list 'define-key 'gdb-mode-map key  (list 'quote fun))))))

(def-gdb "step"   "\M-s" "Step one source line with display"
  (gdb-delete-arrow-extent))
(def-gdb "stepi"  "\M-i" "Step one instruction with display"
  (gdb-delete-arrow-extent))
(def-gdb "finish" "\C-c\C-f" "Finish executing current function"
  (gdb-delete-arrow-extent))
(def-gdb "run" nil "Run the current program"
  (gdb-delete-arrow-extent))

;;"next" and "cont" were bound to M-n and M-c in Emacs 18, but these are
;;poor choices, since M-n is used for history navigation and M-c is
;;capitalize-word.  These are defined without key bindings so that users
;;may choose their own bindings.
(def-gdb "next"   "\C-c\C-n" "Step one source line (skip functions)"
  (gdb-delete-arrow-extent))
(def-gdb "cont"   "\C-c\M-c" "Proceed with the program"
  (gdb-delete-arrow-extent))

(def-gdb "up"     "\C-c<" "Go up N stack frames (numeric arg) with display")
(def-gdb "down"   "\C-c>" "Go down N stack frames (numeric arg) with display")

(defvar gdb-display-mode nil
  "Minor mode for gdb frame display")
(or (assq 'gdb-display-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((gdb-display-mode " Frame"))))))

(defun gdb-display-mode (&optional arg)
  "Toggle GDB Frame display mode
With arg, turn display mode on if and only if arg is positive.
In the display minor mode, source file are displayed in another
window for repective \\[gdb-display-frame] commands."
  (interactive "P")
  (setq gdb-display-mode (if (null arg)
			     (not gdb-display-mode)
			   (> (prefix-numeric-value arg) 0))))


(defun gdb-mode ()
  "Major mode for interacting with an inferior Gdb process.
The following commands are available:

\\{gdb-mode-map}

\\[gdb-display-frame] displays in the other window
the last line referred to in the gdb buffer. See also
\\[gdb-display-mode].

\\[gdb-step],\\[gdb-next], and \\[gdb-nexti] in the gdb window,
call gdb to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[gdb-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[gdb-display-frame] display frames file in other window
\\[gdb-step] advance one line in program
\\[send-gdb-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map gdb-mode-map)
  (when (not (boundp 'c-mode-syntax-table))
    (require 'cc-mode))
  (set-syntax-table c-mode-syntax-table)
  (make-local-variable 'gdb-last-frame-displayed-p)
  (make-local-variable 'gdb-last-frame)
  (make-local-variable 'gdb-delete-prompt-marker)
  (make-local-variable 'gdb-display-mode)
  (make-local-variable' gdb-filter-accumulator)
  (setq gdb-last-frame nil
        gdb-delete-prompt-marker nil
        gdb-filter-accumulator nil
	gdb-display-mode t
        major-mode 'gdb-mode
        mode-name "Inferior GDB"
        comint-prompt-regexp gdb-prompt-pattern
        gdb-last-frame-displayed-p t)
  (set (make-local-variable 'shell-dirtrackp) t)
  ;;(make-local-variable 'gdb-arrow-extent)
  (and (extentp gdb-arrow-extent)
       (delete-extent gdb-arrow-extent))
  (setq gdb-arrow-extent nil)
  ;; XEmacs change:
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'gdb-delete-arrow-extent nil t)
  (setq comint-input-sentinel 'shell-directory-tracker)
  (run-hooks 'gdb-mode-hook))

(defun gdb-delete-arrow-extent ()
  (let ((inhibit-quit t))
    (if gdb-arrow-extent
        (delete-extent gdb-arrow-extent))
    (setq gdb-arrow-extent nil)))

(defvar current-gdb-buffer nil)

;;;###autoload
(defvar gdb-command-name "gdb"
  "Pathname for executing gdb.")

;;;###autoload
(defun gdb (path &optional corefile)
  "Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  (interactive "FRun gdb on file: ")
  (setq path (file-truename (expand-file-name path)))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*gdb-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (apply 'make-comint
	   (concat "gdb-" file)
	   (substitute-in-file-name gdb-command-name)
	   nil
	   "-fullname"
	   "-cd" default-directory
	   file
	   (and corefile (list corefile)))
    (set-process-filter (get-buffer-process (current-buffer)) 'gdb-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gdb-sentinel)
    ;; XEmacs change: turn on gdb mode after setting up the proc filters
    ;; for the benefit of shell-font.el
    (gdb-mode)
    (gdb-set-buffer)))

;;;####autoload
(defun gdb-with-core (file corefile)
  "Debug a program using a corefile."
  (interactive "fProgram to debug: \nfCore file to use: ")
  (gdb file corefile))

(defun gdb-set-buffer ()
  (cond ((eq major-mode 'gdb-mode)
	 (setq current-gdb-buffer (current-buffer))
	 (if (featurep 'eos-toolbar)
	     (set-specifier default-toolbar (cons (current-buffer)
						  gdb-toolbar))))))


;; This function is responsible for inserting output from GDB
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that GDB prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun gdb-filter (proc string)
  (let ((inhibit-quit t))
    (save-current-buffer
     (set-buffer (process-buffer proc))
     (if gdb-filter-accumulator
	 (gdb-filter-accumulate-marker
	  proc (concat gdb-filter-accumulator string))
       (gdb-filter-scan-input proc string)))))

(defun gdb-filter-accumulate-marker (proc string)
  (setq gdb-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq gdb-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq gdb-last-frame-displayed-p nil)
		  (gdb-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq gdb-filter-accumulator string)))
	(gdb-filter-insert proc "\032")
	(gdb-filter-scan-input proc (substring string 1)))
    (setq gdb-filter-accumulator string)))

(defun gdb-filter-scan-input (proc string)
  (if (equal string "")
      (setq gdb-filter-accumulator nil)
    (let ((start (string-match "\032" string)))
      (if start
	  (progn (gdb-filter-insert proc (substring string 0 start))
		 (gdb-filter-accumulate-marker proc
					       (substring string start)))
	(gdb-filter-insert proc string)))))

(defun gdb-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc))))
    (save-excursion
      ;; Insert the text, moving the process-marker.
      (goto-char (process-mark proc))
      (insert-before-markers string)
      (set-marker (process-mark proc) (point))
      (gdb-maybe-delete-prompt)
      ;; Check for a filename-and-line number.
      (gdb-display-frame
       ;; Don't display the specified file
       ;; unless (1) point is at or after the position where output appears
       ;; and (2) this buffer is on the screen.
       (or output-after-point
           (not (get-buffer-window (current-buffer))))
       ;; Display a file only when a new filename-and-line-number appears.
       t))
    (if moving (goto-char (process-mark proc))))

  (let (s)
    (if (and (should-use-dialog-box-p)
	     (setq s (or (string-match " (y or n) *\\'" string)
			 (string-match " (yes or no) *\\'" string))))
	(gdb-mouse-prompt-hack (substring string 0 s) (current-buffer))))
  )

(defun gdb-mouse-prompt-hack (prompt buffer)
  (popup-dialog-box
   (list prompt
	 (vector "Yes"    (list 'gdb-mouse-prompt-hack-answer 't   buffer) t)
	 (vector "No"     (list 'gdb-mouse-prompt-hack-answer 'nil buffer) t)
	 nil
	 (vector "Cancel" (list 'gdb-mouse-prompt-hack-answer 'nil buffer) t)
	 )))

(defun gdb-mouse-prompt-hack-answer (answer buffer)
  (let ((b (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (goto-char (process-mark (get-buffer-process buffer)))
	  (delete-region (point) (point-max))
	  (insert (if answer "yes" "no"))
	  (comint-send-input))
      (set-buffer b))))

(defun gdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 ;(setq overlay-arrow-position nil) -- done by kill-buffer-hook
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
         (gdb-delete-arrow-extent)
	 ;; Fix the mode line.
	 (setq modeline-process
	       (concat ": gdb " (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gdb buffer.
	     (set-buffer obuf))))))


(defun gdb-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (gdb-display-frame))

(defun gdb-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from GDB.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (gdb-set-buffer)
  (and gdb-last-frame (not nodisplay)
       gdb-display-mode
       (or (not gdb-last-frame-displayed-p) (not noauto))
       (progn (gdb-display-line (car gdb-last-frame) (cdr gdb-last-frame))
	      (setq gdb-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun gdb-display-line (true-file line &optional select-method)
  ;; FILE to display
  ;; LINE number to highlight and make visible
  ;; SELECT-METHOD 'source, 'debugger, or 'none.  (default is 'debugger)
  (and (null select-method) (setq select-method 'debugger))
  (let* ((pre-display-buffer-function nil) ; screw it, put it all in one screen
	 (pop-up-windows t)
	 (source-buffer (find-file-noselect true-file))
	 (source-window (display-buffer source-buffer))
	 (debugger-window (get-buffer-window current-gdb-buffer))
         (extent gdb-arrow-extent)
	 pos)
    ;; XEmacs change: make sure we find a window displaying the source file
    ;; even if we are already sitting in it when a breakpoint is hit.
    ;; Otherwise the t argument to display-buffer will prevent it from being
    ;; displayed.
    (save-excursion 
      (cond ((eq select-method 'debugger)
	     ;; might not already be displayed
	     (setq debugger-window (display-buffer current-gdb-buffer))
	     (select-window debugger-window))
	    ((eq select-method 'source)
	     (select-window source-window))))
    (and extent
	 (not (eq (extent-object extent) source-buffer))
	 (setq extent (delete-extent extent)))
    (or extent
        (progn
          (setq extent (make-extent 1 1 source-buffer))
          (set-extent-face extent 'gdb-arrow-face)
	  (set-extent-begin-glyph extent gdb-arrow-glyph)
          (set-extent-begin-glyph-layout extent 'whitespace)
          (set-extent-priority extent 2000)
          (setq gdb-arrow-extent extent)))
    (save-current-buffer
      (set-buffer source-buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(set-window-point source-window (point))
	(setq pos (point))
        (end-of-line)
        (set-extent-endpoints extent pos (point))
        (run-hook-with-args 'gdb-arrow-extent-hooks extent))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    ;; Added by Stig.  It caused lots of problems for several users
    ;; and since its purpose is unclear it is getting commented out.
    ;;(and debugger-window
    ;; (set-window-point debugger-window pos))
    ))

(defun gdb-call (command)
  "Invoke gdb COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  ;; Record info on the last prompt in the buffer and its position.
  ;; This is used in  gdb-maybe-delete-prompt
  ;; to prevent multiple prompts from accumulating.
  (save-excursion
    (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
    (let ((pt (point)))
      (beginning-of-line)
      (setq gdb-delete-prompt-marker
	    (if (= (point) pt)
		nil
	      (list (point-marker) (- pt (point))
		    (buffer-substring (point) pt))))))
  (gdb-set-buffer)
  (process-send-string (get-buffer-process current-gdb-buffer)
	       (concat command "\n")))

(defun gdb-maybe-delete-prompt ()
  (if gdb-delete-prompt-marker
      ;; Get the string that we used as the prompt before.
      (let ((prompt (nth 2 gdb-delete-prompt-marker))
	    (length (nth 1 gdb-delete-prompt-marker)))
	;; Position after it.
	(goto-char (+ (car gdb-delete-prompt-marker) length))
	;; Delete any duplicates of it which follow right after.
	(while (and (<= (+ (point) length) (point-max))
		    (string= prompt
			     (buffer-substring (point) (+ (point) length))))
	  (delete-region (point) (+ (point) length)))
	;; If that didn't take us to where output is arriving,
	;; we have encountered something other than a prompt,
	;; so stop trying to delete any more prompts.
	(if (not (= (point)
		    (process-mark (get-buffer-process current-gdb-buffer))))
	    (progn
	      (set-marker (car gdb-delete-prompt-marker) nil)
	      (setq gdb-delete-prompt-marker nil))))))

(defun gdb-break (temp)
  "Set GDB breakpoint at this source line.  With ARG set temporary breakpoint."
  (interactive "P")
  (let* ((file-name (file-name-nondirectory buffer-file-name))
	 (line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat (if temp "tbreak " "break ") file-name ":"
		      (int-to-string line))))
    (set-buffer current-gdb-buffer)
    (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
    (delete-region (point) (point-max))
    (insert cmd)
    (comint-send-input)
    ;;(process-send-string (get-buffer-process current-gdb-buffer) cmd)
    ))

(defun gdb-clear ()
  "Set GDB breakpoint at this source line."
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
	 (line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat "clear " file-name ":"
		      (int-to-string line))))
    (set-buffer current-gdb-buffer)
    (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
    (delete-region (point) (point-max))
    (insert cmd)
    (comint-send-input)
    ;;(process-send-string (get-buffer-process current-gdb-buffer) cmd)
    ))

(defun gdb-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (point)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(point)))
     (cond (found (forward-char 2)
		  (buffer-substring found
				    (progn (re-search-forward "[^0-9a-f]")
					   (forward-char -1)
					   (point))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (point)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (point)))))))


(defvar gdb-commands nil
  "List of strings or functions used by send-gdb-command.
It is for customization by you.")

(defun send-gdb-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the gdb buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list gdb-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of gdb-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg gdb-commands)))
    (setq addr (gdb-read-address))
    (if (eq (current-buffer) current-gdb-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-gdb-buffer)
    (goto-char (point-max))
    (insert comm)))

(defun gdb-control-c-subjob ()
  "Send a Control-C to the subprocess."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer))
		       "\C-c"))

(defun gdb-toolbar-break ()
  (interactive)
  (save-excursion
    (message (car gdb-last-frame))
    (set-buffer (find-file-noselect (car gdb-last-frame)))
    (gdb-break nil)))

(defun gdb-toolbar-clear ()
  (interactive)
  (save-excursion
    (message (car gdb-last-frame))
    (set-buffer (find-file-noselect (car gdb-last-frame)))
    (gdb-clear)))

(provide 'gdb)

;;; gdb.el ends here
