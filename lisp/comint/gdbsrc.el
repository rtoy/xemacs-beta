;;; gdbsrc.el -- Source-based (as opposed to comint-based) debugger
;;      interaction mode eventually, this will be unified with GUD
;; 	(after gud works reliably w/ XEmacs...)
;; Keywords: c, unix, tools, debugging

;; Copyright (C) 1990 Debby Ayers <ayers@austin.ibm.com>, and
;;		      Rich Schaefer <schaefer@asc.slb.com>
;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.
;; 
;; This file is part of XEmacs.
;; 
;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Based upon code for version18 by Debra Ayers <ayers@austin.ibm.com>

;;;  GDBSRC::
;;;  Gdbsrc extends the emacs GDB interface to accept gdb commands issued
;;;  from the source code buffer.  Gdbsrc behaves similar to gdb except
;;;  now most debugging may be done from the source code using the *gdb*
;;;  buffer to view output. Supports a point and click model under X to
;;;  evaluate source code expressions (no more typing long variable names).
;;; 
;;; Supports C source at the moment but C++ support will be added if there
;;; is sufficient interest.
;;; 

;; GDBSRC::Gdb Source Mode Interface description.
;; Gdbsrc extends the emacs GDB interface to accept gdb commands issued
;; from the source code buffer. Gdbsrc behaves similar to gdb except now all 
;; debugging may be done from the currently focused source buffer using 
;; the *gdb* buffer to view output.

;; When source files are displayed through gdbsrc, buffers are put in 
;; gdbsrc-mode minor mode. This mode puts the buffer in read-only state
;; and sets up a special key and mouse map to invoke communication with
;; the current gdb process. The minor mode may be toggled on/off as needed.
;; (ESC-T) 

;; C-expressions may be evaluated by gdbsrc by simply pointing at text in the
;; current source buffer with the mouse or by centering the cursor over text
;; and typing a single key command. ('p' for print, '*' for print *).

;; As code is debugged and new buffers are displayed, the focus of gdbsrc
;; follows to each new source buffer. Makes debugging fun. (sound like a
;; commercial or what!)
;; 

;; Current Listing ::
;;key		binding					Comment
;;---		-------					-------
;;
;; r               gdb-return-from-src	GDB return command
;; n               gdb-next-from-src	GDB next command
;; b               gdb-back-from-src	GDB back command
;; w               gdb-where-from-src	GDB where command
;; f               gdb-finish-from-src	GDB finish command
;; u               gdb-up-from-src      GDB up command
;; d               gdb-down-from-src	GDB down command
;; c               gdb-cont-from-src	GDB continue command
;; i               gdb-stepi-from-src	GDB step instruction command
;; s               gdb-step-from-src	GDB step command
;; ?               gdb-whatis-c-sexp	GDB whatis command for data at
;;					     buffer point
;; x               gdbsrc-delete        GDB Delete all breakpoints if no arg
;;					     given or delete arg (C-u arg x)
;; m               gdbsrc-frame         GDB Display current frame if no arg,
;;					     given or display frame arg
;; *               gdb-*print-c-sexp	GDB print * command for data at
;;					       buffer point
;; !               gdbsrc-goto-gdb		Goto the GDB output buffer
;; p               gdb-print-c-sexp	GDB print * command for data at
;;					     buffer point
;; g               gdbsrc-goto-gdb		Goto the GDB output buffer
;; t               gdbsrc-mode		Toggles Gdbsrc mode (turns it off)
;; 
;; C-c C-f         gdb-finish-from-src	GDB finish command
;; 
;; C-x SPC         gdb-break		Set break for line with point
;; ESC t           gdbsrc-mode		Toggle Gdbsrc mode
;;
;; Local Bindings for buffer when you exit Gdbsrc minor mode
;;
;; C-x SPC         gdb-break		Set break for line with point
;; ESC t           gdbsrc-mode		Toggle Gdbsrc mode
;;

;;; (eval-when-compile
;;;   (or noninteractive
;;;       (progn 
;;;         (message "ONLY compile gdbsrc except with -batch because of advice")
;;;         (ding)
;;;       )))

(require 'gdb "gdb")			; NOT gud!  (yet...)

(defvar gdbsrc-active-p t
  "*Set to nil if you do not want source files put in gdbsrc-mode")

(defvar gdbsrc-call-p nil
  "True if gdb command issued from a source buffer")

(defvar gdbsrc-associated-buffer nil
  "Buffer name of attached gdb process")

(defvar gdbsrc-mode nil
  "Indicates whether buffer is in gdbsrc-mode or not")
(make-variable-buffer-local 'gdbsrc-mode)

(defvar gdbsrc-global-mode nil
  "Indicates whether global gdbsrc bindings are in effect or not")

(defvar gdb-prompt-pattern "^[^)#$%>\n]*[)#$%>] *"
  "A regexp for matching the end of the gdb prompt")

;;; bindings

(defvar gdbsrc-global-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'gdbsrc-global-map)
    (define-key map "\C-x " 'gdb-break)
    (define-key map "\M-\C-t" 'gdbsrc-mode)
    (define-key map "\M-\C-g" 'gdbsrc-goto-gdb)

    ;; middle button to select and print expressions...
    (define-key map '(meta button2)       'gdbsrc-print-csexp)
    (define-key map '(meta shift button2) 'gdbsrc-*print-csexp)
    ;; left button to position breakpoints
    (define-key map '(meta button1)       'gdbsrc-set-break)
    (define-key map '(meta shift button1) 'gdbsrc-set-tbreak-continue)
    map)
  "Global minor keymap that is active whenever gdbsrc is running.")

(add-minor-mode 'gdbsrc-global-mode " GdbGlobal" gdbsrc-global-map)

(defvar gdbsrc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-name map 'gdbsrc-mode-map)
    ;; inherit keys from global gdbsrc map just in case that somehow gets turned off.
    (set-keymap-parents map (list gdbsrc-global-map))
    (define-key map "\C-x\C-q" 'gdbsrc-mode) ; toggle read-only
    (define-key map "\C-c\C-c" 'gdbsrc-mode)
    (define-key map "b" 'gdb-break)
    (define-key map "g" 'gdbsrc-goto-gdb)
    (define-key map "!" 'gdbsrc-goto-gdb)
    (define-key map "p" 'gdb-print-c-sexp)
    (define-key map "*" 'gdb-*print-c-sexp)
    (define-key map "?" 'gdb-whatis-c-sexp)
    (define-key map "R" 'gdbsrc-reset)
    map)
  "Minor keymap for buffers in gdbsrc-mode")

(add-minor-mode 'gdbsrc-mode " GdbSrc" gdbsrc-mode-map)

(defvar gdbsrc-toolbar
  '([eos::toolbar-stop-at-icon
     gdb-break
     t
     "Stop at selected position"]
    [eos::toolbar-stop-in-icon
     gdb-break
     t
     "Stop in function whose name is selected"]
    [eos::toolbar-clear-at-icon
     gdbsrc-delete
     t
     "Clear at selected position"]
    [eos::toolbar-evaluate-icon
     gdb-print-c-sexp
     t
     "Evaluate selected expression; shows in separate XEmacs frame"]
    [eos::toolbar-evaluate-star-icon
     gdb-*print-c-sexp
     t
     "Evaluate selected expression as a pointer; shows in separate XEmacs frame"]
    [eos::toolbar-run-icon
     gdbsrc-run
     t
     "Run current program"]
    [eos::toolbar-cont-icon
     gdbsrc-cont
     t
     "Continue current program"]
    [eos::toolbar-step-into-icon
     gdbsrc-step
     t
     "Step into (aka step)"]
    [eos::toolbar-step-over-icon
     gdbsrc-next
     t
     "Step over (aka next)"]
    [eos::toolbar-up-icon
     gdbsrc-up
     t
     "Stack Up (towards \"cooler\" - less recently visited - frames)"]
    [eos::toolbar-down-icon
     gdbsrc-down
     t
     "Stack Down (towards \"warmer\" - more recently visited - frames)"]
    [eos::toolbar-fix-icon
     nil
     nil
     "Fix (not available with gdb)"]
    [eos::toolbar-build-icon
     toolbar-compile
     t
     "Build (aka make -NYI)"]
    ))

(defmacro def-gdb-from-src (gdb-command key &optional doc &rest forms)
  "Create a function that will call GDB-COMMAND with KEY."
  (let* ((fname (format "gdbsrc-%s" gdb-command))
	 (cstr (list 'if 'arg
		     (list 'format "%s %s" gdb-command '(prefix-numeric-value arg))
		     gdb-command))
	 fun)
    (while (string-match " " fname)
      (aset fname (match-beginning 0) ?-))
    (setq fun (intern fname))
    
     (list 'progn
	   (nconc (list 'defun fun '(arg)
			(or doc "")
			'(interactive "P")
			(list 'gdb-call-from-src cstr))
		  forms)
	   (list 'define-key 'gdbsrc-mode-map key  (list 'quote fun)))))

(def-gdb-from-src "step"   "s" "Step one instruction in src"
  (gdb-delete-arrow-extent))
(def-gdb-from-src "stepi"  "i" "Step one source line (skip functions)"
  (gdb-delete-arrow-extent))
(def-gdb-from-src "cont"   "c" "Continue with display"
  (gdb-delete-arrow-extent))
(def-gdb-from-src "down"   "d" "Go down N stack frames (numeric arg) ")
(def-gdb-from-src "up"     "u" "Go up N stack frames (numeric arg)")
(def-gdb-from-src "finish" "f" "Finish frame")
(def-gdb-from-src "where"  "w" "Display (N frames of) backtrace")
(def-gdb-from-src "next"   "n" "Step one line with display"
  (gdb-delete-arrow-extent))
(def-gdb-from-src "run"    "r" "Run program from start"
  (gdb-delete-arrow-extent))
(def-gdb-from-src "return" "R" "Return from selected stack frame")
(def-gdb-from-src "disable" "x" "Disable all breakpoints")
(def-gdb-from-src "delete" "X" "Delete all breakpoints")
(def-gdb-from-src "quit"   "Q" "Quit gdb."
  (gdb-delete-arrow-extent))
(def-gdb-from-src "info locals" "l" "Show local variables")
(def-gdb-from-src "info break"  "B" "Show breakpoints")
(def-gdb-from-src ""  "\r" "Repeat last command")
(def-gdb-from-src "frame"  "m" "Show frame if no arg, with arg go to frame")

;;; code

;;;###autoload
(defun gdbsrc (path &optional core-or-pid)
  "Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug."
  (interactive (let ((file (read-file-name "Program to debug: " nil nil t)))
		 (cond ((numberp current-prefix-arg)
			(list file (int-to-string current-prefix-arg)))
		       (current-prefix-arg
			(list file (read-file-name "Core file: " nil nil t)))
		       (t (list file)))
		 ))
  ;; FIXME - this is perhaps an uncool thing to do --Stig
  (delete-other-windows)
  (split-window-vertically)
  (other-window 0)

  (gdb path core-or-pid)
  (local-set-key 'button2 'gdbsrc-select-or-yank)
  (setq mode-motion-hook 'gdbsrc-mode-motion)
  ;; XEmacs change:
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'gdbsrc-reset nil t))

(defun gdbsrc-global-mode ()
  ;; this can be used as a hook for gdb-mode....
  (or current-gdb-buffer
      (and (eq major-mode 'gdb-mode)	; doesn't work w/ energize yet
	   (setq current-gdb-buffer (current-buffer))
	   ;; XEmacs change:
	   (progn
	     (make-local-hook 'kill-buffer-hook)
	     (add-hook 'kill-buffer-hook 'gdbsrc-reset nil t)))
      (error "Cannot determine current-gdb-buffer"))
;;;   (set-process-filter 
;;;    (get-buffer-process current-gdb-buffer) 'gdbsrc-mode-filter)
;;;   (set-process-sentinel 
;;;    (get-buffer-process current-gdb-buffer) 'gdbsrc-mode-sentinel)
  ;; gdbsrc-global-mode was set to t here but that tended to piss
  ;; people off
  (setq gdbsrc-global-mode nil
	gdbsrc-active-p	   t
	gdbsrc-call-p	   nil
	gdbsrc-mode	   nil)
  (message "Gbd source mode active"))
 
(add-hook 'gdb-mode-hook 'gdbsrc-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gdb Source minor mode.
;;; 

(defvar gdbsrc-associated-buffer nil
  "The gdb buffer to send commands to.")
(defvar gdbsrc-initial-readonly  'undefined
  "read-only status of buffer when not in gdbsrc-mode")
(defvar gdbsrc-old-toolbar nil
  "saved toolbar for buffer")

(defun gdbsrc-mode (arg &optional quiet)
  "Minor mode for interacting with gdb from a c source file.
With arg, turn gdbsrc-mode on iff arg is positive.  In gdbsrc-mode,
you may send an associated gdb buffer commands from the current buffer
containing c source code."
  (interactive "P")
  (setq gdbsrc-mode
	(if (null arg)
	    (not gdbsrc-mode)
	  (> (prefix-numeric-value arg) 0)))

  (cond (gdbsrc-mode
	 (cond ((not (local-variable-p 'gdbsrc-initial-readonly (current-buffer)))
		(set (make-local-variable 'gdbsrc-initial-readonly)
		     buffer-read-only)
		(set (make-local-variable 'gdbsrc-associated-buffer)
		     current-gdb-buffer)
		(if (featurep 'toolbar)
		    (set (make-local-variable 'gdbsrc-old-toolbar)
			 (specifier-specs default-toolbar (current-buffer))))
		)
	       )
	 (if (featurep 'toolbar)
	     (set-specifier default-toolbar (cons (current-buffer)
						  gdbsrc-toolbar)))
	 (setq buffer-read-only t)
	 (or quiet (message "Entering gdbsrc-mode...")))
	(t
	 (and (local-variable-p 'gdbsrc-initial-readonly (current-buffer))
	      (progn
		(if (featurep 'toolbar)
		    (if gdbsrc-old-toolbar
			(set-specifier default-toolbar
				       (cons (current-buffer)
					     gdbsrc-old-toolbar))
		      (remove-specifier default-toolbar (current-buffer))))
		(kill-local-variable 'gdbsrc-old-toolbar)
		(setq buffer-read-only gdbsrc-initial-readonly)
		(kill-local-variable 'gdbsrc-initial-readonly)
		(kill-local-variable 'gdbsrc-associated-buffer)
		))
	 (or quiet (message "Exiting gdbsrc-mode..."))))
  (redraw-modeline t))

;;
;; Sends commands to gdb process.

(defun gdb-call-from-src (command)
  "Send associated gdb process COMMAND displaying source in this window."
  (setq gdbsrc-call-p t)
  (let ((buf (or gdbsrc-associated-buffer current-gdb-buffer)))
    (or (buffer-name buf)
	(error "GDB buffer deleted"))
    (pop-to-buffer buf))
  (goto-char (point-max))
  (beginning-of-line)
  ;; Go past gdb prompt 
  (re-search-forward
   gdb-prompt-pattern (save-excursion (end-of-line) (point))  t)
  ;; Delete any not-supposed-to-be-there text
  (delete-region (point) (point-max)) 
  (insert command)
  (comint-send-input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define Commands for GDB SRC Mode Buffer
;;;

;;; ;; #### - move elsewhere
(or (fboundp 'event-buffer)
    (defun event-buffer (event)
      "Return buffer assocaited with EVENT, or nil."
      (let ((win (event-window event)))
	(and win (window-buffer win)))))

(defun set-gdbsrc-mode-motion-extent (st en action)
  ;; by Stig@hackvan.com
  (let ((ex  (make-extent st en)))
    (set-extent-face ex 'highlight)
    (set-extent-property ex 'gdbsrc t)
    (set-extent-property ex 'action action)
    (setq mode-motion-extent ex)))

(defun nuke-mode-motion-extent ()
  ;; by Stig@hackvan.com
  (cond (mode-motion-extent
	 (delete-extent mode-motion-extent)
	 (setq mode-motion-extent nil))))

(defun looking-at-any (regex-list)
  ;; by Stig@hackvan.com
  (catch 'found
    (while regex-list
      (and (looking-at (car regex-list))
	   (throw 'found t))
      (setq regex-list (cdr regex-list)))))

(defconst gdb-breakpoint-patterns
  '(
    ;; when execution stops...
    ;;Breakpoint 1, XlwMenuRedisplay (w=0x4d2e00, ev=0xefffe3f8, region=0x580e60)
    ;;    at /net/stig/src/xemacs/lwlib/xlwmenu.c:2518
    "^[BW][ra][et][ac][kh]point [0-9]+, .*\\(\n\\s .*\\)*"
    ;; output of the breakpoint command:
    ;;Breakpoint 1 at 0x19f5c8: file /net/stig/src/xemacs/lwlib/xlwmenu.c, line 2715.
    "^[BW][ra][et][ac][kh]point [0-9]+ at .*: file \\([^ ,\n]+\\), line \\([0-9]+\\)."
    ;;Num Type           Disp Enb Address    What
    ;;1   breakpoint     keep y   0x0019ee60 in XlwMenuRedisplay
    ;;                                       at /net/stig/src/xemacs/lwlib/xlwmenu.c:2518
    "^[0-9]+\\s +[bw][ra][et][ac][kh]point.* in .*\\(\n\\s +\\)?at [^ :\n]+:[0-9]+\\(\n\\s .*\\)*"
    )
  "list of patterns to match gdb's various ways of displaying a breakpoint")

(defun gdbsrc-make-breakpoint-action (string)
  ;; by Stig@hackvan.com
  (if (or (string-match "file \\([^ ,\n]+\\), line \\([0-9]+\\)" string)
	  (string-match "at \\([^ :\n]+\\):\\([0-9]+\\)" string))
      (list 'gdbsrc-display
	    (match-string 1 string)
	    (string-to-int (match-string 2 string)))))

(defconst gdb-stack-frame-pattern
  ;;#9  0x62f08 in emacs_Xt_next_event (emacs_event=0x4cf804)
  ;;    at /net/stig/src/xemacs/src/event-Xt.c:1778
  "^#\\([0-9]+\\)\\s +\\(0x[0-9a-f]+ in .*\\|.*\\sw+.* (.*) at .*\\)\\(\n\\s .*\\)*"
  "matches the first line of a gdb stack frame and all continuation lines.
subex 1 is frame number.")

(defun gdbsrc-mode-motion (ee)
  ;; by Stig@hackvan.com
  (save-excursion
    (set-buffer (event-buffer ee))
    (save-excursion
      (if (not (event-point ee))
	  (nuke-mode-motion-extent)
	(goto-char (event-point ee))
	(beginning-of-line)
	(while (and (not (bobp)) (eq ?  (char-syntax (following-char))))
	  (forward-line -1))
	(if (extent-at (point) (current-buffer) 'gdbsrc)
	    nil
	  (nuke-mode-motion-extent)
	  (cond ((looking-at-any gdb-breakpoint-patterns)
		 (set-gdbsrc-mode-motion-extent
		  (match-beginning 0)
		  (match-end 0)
		  (gdbsrc-make-breakpoint-action (match-string 0))))
		((looking-at gdb-stack-frame-pattern)
		 (set-gdbsrc-mode-motion-extent
		  (match-beginning 0)
		  (match-end 0)
		  (list 'gdbsrc-frame
			(string-to-int (match-string 1)))))
		)))
      )))
  
(defun gdbsrc-display (file line)
  ;; by Stig@hackvan.com
  (select-window (display-buffer (find-file-noselect file)))
  (goto-line line))

(defun click-inside-selection-p (click)
  (or (click-inside-extent-p click primary-selection-extent)
      (click-inside-extent-p click zmacs-region-extent)
      ))

(defun click-inside-extent-p (click extent)
  "Returns non-nil if the button event is within the bounds of the primary
selection-extent, nil otherwise."
  ;; stig@hackvan.com
  (let ((ewin (event-window click))
	(epnt (event-point click)))
    (and ewin
	 epnt
	 extent
	 (eq (window-buffer ewin)
	     (extent-object extent))
	 (extent-start-position extent)
	 (> epnt (extent-start-position extent))
	 (> (extent-end-position extent) epnt))))

(defun point-inside-extent-p (extent)
  "Returns non-nil if the point is within or just after the bounds of the
primary selection-extent, nil otherwise."
  ;; stig@hackvan.com
  (and extent		; FIXME - I'm such a sinner...
       (eq (current-buffer) 
	   (extent-object extent))
       (> (point) (extent-start-position extent))
       (>= (extent-end-position extent) (point))))

(defun gdbsrc-select-or-yank (ee)
  ;; by Stig@hackvan.com
  (interactive "e")
  (let ((action (save-excursion
		  (set-buffer (event-buffer ee))
		  (and mode-motion-extent
		       (click-inside-extent-p ee mode-motion-extent)
		       (extent-property mode-motion-extent 'action)))
		))
    (if action
	(eval action)
      (mouse-yank ee))))

(defvar gdb-print-format ""
  "Set this variable to a valid format string to print c-sexps in a
different way (hex,octal, etc).")

(defun gdb-print-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with print command."
  (interactive)
  (let* ((tag (find-c-sexp))
	 (command (concat "print " gdb-print-format tag)))
    (gdb-call-from-src command)))

(defun gdb-*print-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with the print * command."
  (interactive)
  (let* ((tag (find-c-sexp))
	(command (concat "print " gdb-print-format "*"  tag)))
    (gdb-call-from-src  command)))
 
(defun gdb-whatis-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with the whatis command. "
  (interactive)
  (let* ((tag (gdbsrc-selection-or-sexp))
	 (command (concat "whatis " tag)))
    (gdb-call-from-src command)))

(defun gdbsrc-goto-gdb ()
  "Hop back and forth between the gdb interaction buffer and the gdb source
buffer.  "
  ;; by Stig@hackvan.com
  (interactive)
  (let ((gbuf (or gdbsrc-associated-buffer current-gdb-buffer)))
    (cond ((eq (current-buffer) gbuf)
	   (and gdb-arrow-extent
		(extent-object gdb-arrow-extent)
		(progn (pop-to-buffer (extent-object gdb-arrow-extent))
		       (goto-char (extent-start-position gdb-arrow-extent)))))
	  ((buffer-name gbuf) (pop-to-buffer gbuf))
	  ((y-or-n-p "No debugger.  Start a new one? ")
	         (call-interactively 'gdbsrc))
	  (t (error "No gdb buffer."))
	  )))

(defvar gdbsrc-last-src-buffer nil)

(defun gdbsrc-goto-src ()
  (interactive)
  (let* ((valid (and gdbsrc-last-src-buffer
		     (memq gdbsrc-last-src-buffer (buffer-list))))
	 (win (and valid
		   (get-buffer-window gdbsrc-last-src-buffer))))
    (cond (win (select-window win))
	  (valid (pop-to-buffer gdbsrc-last-src-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following functions are used to extract the closest surrounding
;;;  c expression from point
;;;
(defun back-sexp ()
  "Version of backward-sexp that catches errors"
  (condition-case nil
      (backward-sexp)
    (error t)))

(defun forw-sexp ()
  "Version of forward-sexp that catches errors"
  (condition-case nil
     (forward-sexp)
    (error t)))

(defun sexp-compound-sep (span-start span-end)
  "Returns '.' for '->' & '.', returns ' ' for white space,
returns '?' for other puctuation"  
  (let ((result ? )
	(syntax))
    (while (< span-start span-end)
      (setq syntax (char-syntax (char-after span-start)))
      (cond
       ((= syntax ? ) t)
       ((= syntax ?.) (setq syntax (char-after span-start))
	(cond 
	 ((= syntax ?.) (setq result ?.))
	 ((and (= syntax ?-) (= (char-after (+ span-start 1)) ?>))
	  (setq result ?.)
	  (setq span-start (+ span-start 1)))
	 (t (setq span-start span-end)
	    (setq result ??)))))
      (setq span-start (+ span-start 1)))
    result 
    )
  )

(defun sexp-compound (first second)
  "Returns non-nil if the concatenation of two S-EXPs result in a Single C 
token. The two S-EXPs are represented as a cons cells, where the car 
specifies the point in the current buffer that marks the begging of the 
S-EXP and the cdr specifies the character after the end of the S-EXP
Link S-Exps of the form:
      Sexp -> SexpC
      Sexp . Sexp
      Sexp (Sexp)        Maybe exclude if first Sexp is: if, while, do, for, switch
      Sexp [Sexp]
      (Sexp) Sexp
      [Sexp] Sexp"
  (let ((span-start (cdr first))
	(span-end (car second))
	(syntax))
    (setq syntax (sexp-compound-sep span-start span-end))
    (cond
     ((= (car first) (car second)) nil)
     ((= (cdr first) (cdr second)) nil)
     ((= syntax ?.) t)
     ((= syntax ? )
	 (setq span-start (char-after (- span-start 1)))
	 (setq span-end (char-after span-end))
	 (cond
	  ((= span-start ?) ) t )
	  ((= span-start ?] ) t )
          ((= span-end ?( ) t )
	  ((= span-end ?[ ) t )
	  (t nil))
	 )
     (t nil))
    )
  )

(defun sexp-cur ()
  "Returns the  S-EXP that Point is a member, Point is set to begging of S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((p (point)) (begin) (end))
    (back-sexp)
    (setq begin (point))
    (forw-sexp)
    (setq end (point))
    (if (>= p end) 
	(progn
	 (setq begin p)
	 (goto-char p)
	 (forw-sexp)
	 (setq end (point))
	 )
      )
    (goto-char begin)
    (cons begin end)
    )
  )

(defun sexp-prev ()
  "Returns the previous S-EXP, Point is set to begging of that S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((begin) (end))
    (back-sexp)
    (setq begin (point))
    (forw-sexp)
    (setq end (point))
    (goto-char begin)
    (cons begin end))
)

(defun sexp-next ()
  "Returns the following S-EXP, Point is set to begging of that S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((begin) (end))
    (forw-sexp)
    (forw-sexp)
    (setq end (point))
    (back-sexp)
    (setq begin (point))
    (cons begin end)
    )
  )

(defun find-c-sexp ()
  "Returns the Complex  S-EXP that surrounds Point"
  (interactive)
  (save-excursion
    (let ((p) (sexp) (test-sexp))
      (setq p (point))
      (setq sexp (sexp-cur))
      (setq test-sexp (sexp-prev))
      (while (sexp-compound test-sexp sexp)
	(setq sexp (cons (car test-sexp) (cdr sexp)))
	(goto-char (car sexp))
	(setq test-sexp (sexp-prev))
	)
      (goto-char p)
      (setq test-sexp (sexp-next))
      (while (sexp-compound sexp test-sexp)
	(setq sexp (cons (car sexp) (cdr test-sexp)))
	(setq test-sexp (sexp-next))
	)
      (buffer-substring (car sexp) (cdr sexp))
      )
    )
  )

(defun gdbsrc-selection-or-sexp (&optional ee)
  ;; FIXME - fix this docstring
  "If the EVENT is within the primary selection, then return the selected
text, otherwise parse the expression at the point of the mouse click and
return that.  If EVENT is nil, then return the C sexp at point."
  ;; stig@hackvan.com
  (cond ((or (and ee (click-inside-selection-p ee))
	     (and (not ee) (point-inside-selection-p)))
	 (replace-in-string (extent-string primary-selection-extent) "\n\\s *" " "))
	(ee 
	 (gdbsrc-get-csexp-at-click ee))
	(t
	 (find-c-sexp))
	))

(defun gdbsrc-get-csexp-at-click (ee) 
  "Returns the containing s-expression located at the mouse cursor to point."
  ;; "
  ;; by Stig@hackvan.com
  (let ((ewin (event-window ee))
	(epnt (event-point ee)))
    (or (and ewin epnt)
	(error "Must click within a window"))
    (save-excursion
      (set-buffer (window-buffer ewin))
      (save-excursion
	(goto-char epnt)
	(find-c-sexp)))))

(defun gdbsrc-print-csexp (&optional ee)
  (interactive) 
  (or ee (setq ee current-mouse-event))
  (gdb-call-from-src
	 (concat "print "  gdb-print-format (gdbsrc-selection-or-sexp ee))))

(defun gdbsrc-*print-csexp (&optional ee)
  (interactive) 
  (or ee (setq ee current-mouse-event))
  (gdb-call-from-src
   (concat "print *"  gdb-print-format (gdbsrc-selection-or-sexp ee))))

;; (defun gdbsrc-print-region (arg)
;;   (let (( command  (concat "print " gdb-print-format (x-get-cut-buffer))))
;;     (gdb-call-from-src command)))
;; 
;; (defun gdbsrc-*print-region (arg)
;;   (let (( command  (concat "print *" gdb-print-format (x-get-cut-buffer))))
;;     (gdb-call-from-src command)))

(defun gdbsrc-file:lno ()
  "returns \"file:lno\" specification for location of point. "
  ;; by Stig@hackvan.com
  (format "%s:%d"
	  (file-name-nondirectory buffer-file-name)
	  (save-restriction
	    (widen)
	    (1+ (count-lines (point-min)
			     (save-excursion (beginning-of-line) (point)))))
	  ))

(defun gdbsrc-set-break (ee)
  "Sets a breakpoint.  Click on the selection and it will set a breakpoint
using the selected text.  Click anywhere in a source file, and it will set
a breakpoint at that line number of that file."
  ;; by Stig@hackvan.com
  ;; there is already gdb-break, so this only needs to work with mouse clicks.
  (interactive "e") 
  (gdb-call-from-src
   (concat "break "
	   (if (click-inside-selection-p ee)
	       (extent-string primary-selection-extent)
	     (mouse-set-point ee)
	     (or buffer-file-name (error "No file in window"))
	     (gdbsrc-file:lno)
	     ))))

(defun gdbsrc-set-tbreak-continue (&optional ee)
  "Set a temporary breakpoint at the position of the mouse click and then
continues.  This can be bound to either a key or a mouse button."
  ;; by Stig@hackvan.com
  (interactive)
  (or ee (setq ee current-mouse-event))
  (and ee (mouse-set-point ee))
  (gdb-call-from-src (concat "tbreak " (gdbsrc-file:lno)))
  (gdb-call-from-src "c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions extended from gdb.el for gdbsrc.
;;
;; gdbsrc-set-buffer - added a check to set buffer to gdbsrc-associated-buffer
;;                  to handle multiple gdb sessions being driven from src
;;                  files.

(require 'advice)

(defadvice gdb-set-buffer (after gdbsrc activate) ; ()
  "Advised to work from a source buffer instead of just the gdb buffer."
  ;; by Stig@hackvan.com
  ;; the operations below have tests which are disjoint from the tests in
  ;; the original `gdb-set-buffer'.  Current-gdb-buffer cannot be set twice.
  (and gdbsrc-call-p
       gdbsrc-associated-buffer
       (setq current-gdb-buffer gdbsrc-associated-buffer)))

(defadvice gdb-display-line (around gdbsrc activate)
  ;; (true-file line &optional select-method)
  "Advised to select the source buffer instead of the gdb-buffer"
  ;; by Stig@hackvan.com
  (ad-set-arg 2 'source) ; tell it not to select the gdb window
  ad-do-it
  (save-excursion
    (let* ((buf (extent-object gdb-arrow-extent))
	   (win (get-buffer-window buf)))
      (setq gdbsrc-last-src-buffer buf)
      (select-window win)
      (set-window-point win (extent-start-position gdb-arrow-extent))
      (set-buffer buf))
    (and gdbsrc-active-p
	 (not gdbsrc-mode)
	 (not (eq (current-buffer) current-gdb-buffer))
	 (gdbsrc-mode 1))))

(defadvice gdb-filter (after gdbsrc activate) ; (proc string)
  ;; by Stig@hackvan.com
  ;; if we got a gdb prompt and it wasn't a gdbsrc command, then it's gdb
  ;; hitting a breakpoint or having a core dump, so bounce back to the gdb
  ;; window.
  (let* ((selbuf (window-buffer (selected-window)))
	 win)
    ;; if we're at a gdb prompt, then display the buffer
    (and (save-match-data (string-match gdb-prompt-pattern (ad-get-arg 1)))
	 (prog1
	     (not gdbsrc-call-p)
	   (setq gdbsrc-call-p nil))
	 (setq win (display-buffer current-gdb-buffer))
	 ;; if we're not in either the source buffer or the gdb buffer,
	 ;; then select the window too...
	 (not (eq selbuf current-gdb-buffer))
	 (not (eq selbuf gdbsrc-last-src-buffer))
	 (progn
	   (ding nil 'warp)
	   (select-window win)))
    ))

(defun gdbsrc-reset ()
  ;; tidy house and turn off gdbsrc-mode in all buffers
  ;; by Stig@hackvan.com
  (gdb-delete-arrow-extent)
  (setq gdbsrc-global-mode nil)
  (mapcar #'(lambda (buffer) 
	      (set-buffer buffer)
	      (cond ((eq gdbsrc-associated-buffer current-gdb-buffer)
		     (gdbsrc-mode -1))))
	  (buffer-list)))

(defadvice gdb-sentinel (after gdbsrc freeze) ; (proc msg)
  ;; by Stig@hackvan.com
  (gdbsrc-reset)
  (message "Gdbsrc finished"))

(provide 'gdbsrc)
