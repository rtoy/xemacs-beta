;;; sun-eos-debugger.el --- Implements the XEmacs/SPARCworks Debugger interface

;; Copyright (C) Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks Debugger dbx

;;; Commentary:
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

;; debugger buffer

(require 'eos-common   "sun-eos-common")
(require 'eos-debugger "sun-eos-debugger")
(require 'eos-menubar  "sun-eos-menubar")

(defvar eos::debugger-buffer "*Eos Debugger Log*"
  "name of buffer where to log debugger activity; see eos::use-debugger-buffer")
(defvar eos::dbx-buffer nil)
(defvar eos::key-mode 'none "Style of key mode interaction for Eos")

(defun eos::ensure-debugger-buffer ()
  ;; will ensure a debugger buffer, with the proper major mode
  (let ((buf (get-buffer eos::debugger-buffer)))
    (if buf
	(switch-to-buffer buf)
      (setq buf (get-buffer-create eos::debugger-buffer))
      (set-buffer buf)
      (eos::debugger-mode)
      (toggle-read-only -1)		; writeable
      (eos::insert-string-as-extent "[Debugger] " t (get-face 'bold))
      (toggle-read-only 1)		; read-only
      )))

(defun eos::synchronize-debugger-buffer ()
  ;; ensure all views of this buffer are at the end
  (eos::ensure-debugger-buffer)
  (let ((x (point-max)))
    (goto-char x)
    (mapcar (function
	     (lambda (win)
	       (set-window-point win x)))
	    (get-buffer-window-list eos::debugger-buffer))
    ))

(defvar eos::debugger-mode-map nil)

(if eos::debugger-mode-map
    nil
  (progn
    (setq eos::debugger-mode-map (make-keymap))
    (set-keymap-name eos::debugger-mode-map 'eos::debugger-mode-map)
    (define-key eos::debugger-mode-map [(meta p)] 'eos::debugger-previous-cmd)
    (define-key eos::debugger-mode-map [(meta n)] 'eos::debugger-next-cmd)
    (define-key eos::debugger-mode-map [return] 'eos::debugger-send-cmd)
    ))

(defun eos::debugger-mode ()
  (interactive)
  "local mode"
  (kill-all-local-variables)    
  (setq major-mode 'eos::debugger-mode)
  (setq mode-name "eos::debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map eos::debugger-mode-map))


;; Handling of command lists

(defvar eos::current-command nil "Current command navigated; as an extent")
(defvar eos::last-command nil "last command sent to debugger, as an extent")

(defun eos::debugger-previous-cmd ()
  ;; present the previous command
  (interactive)
  (save-excursion
    (let ((xt nil))
      (if (null eos::current-command)
	  (setq xt eos::last-command)
	(setq xt (extent-property 
		  eos::current-command
		  'previous-command)))
      (if xt
	  (progn
	    (eos::debugger-delete-last-cmd-line)
	    (goto-char (point-max))
	    (insert (buffer-substring
		     (extent-start-position xt)
		     (1- (extent-end-position xt)) ; remove <CR>
		     ))
	    (setq eos::current-command xt))
	(error "no previous command")
	))
    ))

(defun eos::debugger-next-cmd ()
  ;; present the next command
  (interactive)
  (save-excursion
    (let ((xt nil))
      (if (null eos::current-command)
	  (error "no next command")
	(setq xt (extent-property 
		  eos::current-command
		  'next-command)))
      (eos::debugger-delete-last-cmd-line)
      (if xt
	  (progn
	    (goto-char (point-max))
	    (insert (buffer-substring
		     (extent-start-position xt)
		     (1- (extent-end-position xt)) ; remove <CR>
		     ))
	    (setq eos::current-command xt))
	(setq eos::current-command nil)
	))
    ))

(defun eos::debugger-delete-last-cmd-line ()
  ;; delete the last command line, not yet inputed, returns that cmd line
  (goto-char (point-max))
  (let ((e (point)))
    (beginning-of-line)
    (let* ((xt (extent-at (point)))
	   (p (extent-end-position xt))
	   (str (buffer-substring p e))
	   )
      (delete-region p e)
      str
      )))

(defun eos::debugger-send-cmd ()
  ;; send the message in the current line
  (interactive)
  (end-of-line)
  (let ((e (point)))
    (beginning-of-line)
    (let* ((xt (extent-at (point)))
	   (p (extent-end-position xt))
	   (str (buffer-substring p e))
	   )
      (delete-region p e)
      (eos::send-spider-current-do-msg (concat str "\n"))
      (goto-char (point-max))
      (setq eos::current-command nil)
      )))

;; client
;;

(defun get-buffer-window-list (buffer)
  ;; like get-buffer-window except that will generate a list of windows
  ;; instead of just the first one"
  (let* ((buf (get-buffer buffer))
	 (win1 (next-window nil 'foo t t))
	 (win win1)
	 (first t)
	 (ret nil)
	 )
    (if (null buf)
	nil
      (while (or
	      (and first win)
	      (not (or first (equal win win1)))
	      )
	(setq first nil)
	(if (equal
	     buf
	     (window-buffer win))
	    (setq ret (cons win ret)))
	(setq win (next-window win t t t))
	)
      ret)))

(defun eos::dbx-process ()
  ;; Returns nil, or the corresponding process where to insert
  (let ((pl (process-list))
	(found-proc nil)
	)
    (while (and pl (null found-proc))
      (let* ((proc (car pl))
	     (name (process-name proc))
	     )
	(if (and (>= (length name) 3)
		 (equal (substring name 0 3) "Eos"))
	    (setq found-proc proc)
	  (setq pl (cdr pl))
	  )
	))
    found-proc
    ))

(defun eos::insert-echo (process string)
  (if (null process)
      nil
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char (point-max))
;;      (let ((beg (point)))
;;	(insert-before-markers string))
      (insert-before-markers string)
      (if (process-mark process)
	  (set-marker (process-mark process) (point-max))))
    (if (eq (process-buffer process)
	    (current-buffer))
	(goto-char (point-max)))
    ))


(defun eos::insert-on-debugger-buffer (msg rdonly face &optional previous-command)
  ;; will insert MSG at end of debugger buffer with RDONLY property and with FACE. 
  ;; If PREVIOUS-COMMAND is given, the newly created extent will be doubly linked into this one
  ;; using 'previous-command and 'next-command properties
  (save-window-excursion
  (let ((fr (selected-frame))
	(buf (current-buffer))
	(xt nil))
    (eos::ensure-debugger-buffer)
    (toggle-read-only -1)		; not read-only 
    (eos::insert-echo (eos::dbx-process) msg)
    (setq xt (eos::insert-string-as-extent msg rdonly face))
    (if previous-command
	(progn
	  (set-extent-property xt 'previous-command previous-command)
	  (set-extent-property previous-command 'next-command xt)
	  ))
    (toggle-read-only 1)		; now read-only 
    (switch-to-buffer buf)
    (select-frame fr)
    xt
  ))
  )

(defun eos::insert-string-as-extent (msg rdonly face)
  ;; insert MSG as a extent with RDONLY and FACE.  Returns the extent
  (let ((here nil)
	(xt nil))
    (goto-char (point-max))
    (setq here (point))
    (insert msg)
    (setq xt (make-extent here (point) nil))
    (if rdonly
	(progn
	  (set-extent-property xt 'read-only t)
	  (set-extent-property xt 'duplicable nil)
	  ))
    (set-extent-face xt face)
    (eos::synchronize-debugger-buffer)
    xt
    ))


(require 'comint)

(defvar eos::dbx-program "dbx")
(defvar eos::dbx-switches (list "-editor"))

(defun eos::expand-file-name (file)
  ;; expand file name depending on first character
  (cond
   ((null file)
    nil)
   ((eq (elt file 0) ?~)
    (expand-file-name file))
   ((eq (elt file 0) ?$)
    (substitute-in-file-name file))
   (t file)))

(defun eos::read-dbx-request (program switches)
  ;; will prompt to the user with PROGRAM and SWITCHES, let her modify this
  ;; and then will read the result and split it into program and switches.
  (let* ((prompt
	  (concat program " " (mapconcat 'identity switches " ")))
	 (ret (read-from-minibuffer "Run dbx as: " prompt))
	 (ret2 (split-string ret " ")))
    ;; some testing
    (cons (car ret2) (cdr ret2))
  ))

(defun eos::dbx ()
;; Run an inferior dbx -editor process, with I/O through buffer *Eos Dbx*.
;; If buffer exists but dbx process is not running, make new dbx.
;; If buffer exists and dbx process is running, 
;; just switch to buffer `*Eos Dbx*'.
  (let ((buffer "*Eos Dbx*")
	(buffer-name "Eos Dbx")
	(input nil))
    (cond ((not (comint-check-proc buffer))
	   (setq input (eos::read-dbx-request eos::dbx-program
					      eos::dbx-switches))
	   (setq eos::dbx-program (car input))
	   (setq eos::dbx-switches (cdr input))
	   (message "Starting Dbx subprocess")
	   (setq buffer
		 (set-buffer
		  (apply 'make-comint 
			 buffer-name
			 (eos::expand-file-name eos::dbx-program)
			 nil
			 (mapcar 'eos::expand-file-name eos::dbx-switches))))
	   (comint-mode)
	   (if (and (eq (device-type (frame-device (selected-frame))) 'tty)
		    (eq eos::key-mode 'none)
		    (yes-or-no-p 
		     "Do you want the prefix map activated?"))
	       (eos::set-key-mode 'prefix))
	   (setq eos::dbx-or-debugger 'dbx)
	   (setq eos::dbx-buffer (current-buffer))
	   (make-local-variable 'kill-buffer-hook)
	   (setq kill-buffer-hook
		 (list (function (lambda ()
				   (cond
				    ((null (eos::dbx-process)) t)
				    ((not (eq (process-status (eos::dbx-process)) 'run)) t)
				    ((yes-or-no-p
					  "Warning! Killing this buffer will kill a dbx process, proceed? ")
				     (eos::internal-clear-annotations t t t t))
				    (t (error "kill-buffer aborted!")))
				   ))))
	   )
	  (t
	   (message "Reusing existing dbx buffer and dbx process")))
    (switch-to-buffer buffer)
  ))


;; Actions to start a debugger in the background.

(defvar eos::debugger-process nil
  "Debugger process for the background.  Only one per XEmacs")

(defvar eos::dbx-or-debugger nil)

(defun eos::start-debugger ()
  "Start an \"debugger -editor\" in the background. Will ask for confirmation if
XEmacs somehow believes there is already one running"
  (interactive)
  (if (and (or (not (processp eos::debugger-process))
	       (not (eq (process-status eos::debugger-process) 'run))
	       (yes-or-no-p
		"Warning! XEmacs believes there already is a debugger -editor, proceed? "))
	   (or (not (eos::dbx-process))
	       (not (eq (process-status (eos::dbx-process)) 'run))
	       (yes-or-no-p
		"Warning! XEmacs believes there already is a dbx -editor, proceed? ")))
      (progn
	(setq eos::debugger-process
	      (start-process "*eos debugger*" nil "debugger" "-editor"))
	(message "Starting Debugger subprocess")
	(eos::select-debugger-frame (selected-frame))
	(setq eos::dbx-or-debugger 'debugger)
	)))

;; Ditto for dbx.

(defun eos::start-dbx ()
  "Start an \"dbx -editor\" as a subprocess. Will ask for confirmation if
XEmacs somehow believes there is already one running"
  (interactive)
  (if (and (or (not (processp eos::debugger-process))
	       (not (eq (process-status eos::debugger-process) 'run))
	       (yes-or-no-p
		"Warning! XEmacs believes there already is a debugger -editor, proceed? "))
	   (or (not (eos::dbx-process))
	       (not (eq (process-status (eos::dbx-process)) 'run))
	       (yes-or-no-p
		"Warning! XEmacs believes there already is a dbx -editor, proceed? ")))
      (progn
	(eos::select-debugger-frame (selected-frame))
	(eos::dbx)
	)))


;;
;; Communication commands
;;

(defun eos::spider-do-callback (msg pat)
  ;; Callback after processing a spider_do request
  (eos::insert-on-debugger-buffer
   (format "%s" (get-tooltalk-message-attribute msg 'arg_val 2))
   t
   (get-face 'bold))
  (destroy-tooltalk-message msg)
  )

(defvar eos::last-command-was-print nil "(eos:: internal)")

(defun eos::spro_spider_output (msg pat)
  ;; For spider output
  (let ((s (get-tooltalk-message-attribute msg 'arg_val 1))
	(err (get-tooltalk-message-attribute msg 'arg_val 2))
	)
    (message (format "%s" s))
    (eos::insert-on-debugger-buffer (format "%s" s)
				    t
				    (get-face 'default))
    (if (and err (not (string-equal err "")))
	(eos::insert-on-debugger-buffer
	 (insert (format "STDERR> %s" err))
	 t
	 (get-face 'default))
      )
    (destroy-tooltalk-message msg)))

(defun eos::spro_spider_output-common (msg pat)
  ;; For spider output
  (if eos::last-command-was-print
      (eos::spro_spider_print_output msg pat)
    (eos::spro_spider_output msg pat)))

(defmacro eos::spider-tt-args (cmd spider-id clique-id)
  (` (list
      'class TT_REQUEST
      'address TT_HANDLER
      'scope TT_SESSION
      'handler (, spider-id)
      'op "SPRO_SPIDER_DO"
      'callback 'eos::spider-do-callback
      'args (list
	     (list 'TT_IN (, clique-id) "Context_ID")
	     (list 'TT_IN (, cmd) "string")
	     (list 'TT_OUT))
      )))

(defun eos::send-spider-do-msg (cmd spider-id clique-id)
  ;; Send CMD, a string, to SPIDER-ID, using CLIQUE-ID
  (let ((msg (make-tooltalk-message
	      (eos::spider-tt-args cmd spider-id clique-id))))
    (setq eos::last-command
	  (eos::insert-on-debugger-buffer
	   cmd
	   t
	   (get-face 'italic)
	   eos::last-command))
    (setq eos::current-command eos::last-command)
    (send-tooltalk-message msg)
    (destroy-tooltalk-message msg)
    ))

(defvar eos::no-connection-box
      '("XEmacs does not know the ID of a debugger to connect to.
You may need to reissue a debug or attach command from the debugger.
Consult the introduction to Eos (Help->SPARCworks...) for more details."
	       ["Dismiss" (message "Command aborted") t]))

(defun eos::send-spider-current-do-msg (cmd)
  ;; Send CMD to the current dbx engine using the current debugger clique;
  ;;The cmd ends in a new-line.
  (if (null eos::current-debugger-clique-id)
      (popup-dialog-box eos::no-connection-box)
    (eos::send-spider-do-msg cmd
			     eos::current-dbx-proc-id
			     eos::current-debugger-clique-id)))

(defun eos::dbx-cmd (arg) 
  "Send CMD to the current dbx engine using the current debugger clique;
The cmd does not end in a new-line; a new-line will be added"
  (interactive "sDbx cmd: ")
  (eos::send-spider-current-do-msg (concat arg "\n")))


;;
;; Extra patterns

(defvar eos::dbx-extra-pattern-list nil)

(defun eos::debugger-extra-startup ()
  ;; Actions to do at startup for eos-debugger-extra.el
    (setq eos::dbx-extra-pattern-list	; list of extra TT patterns
	  (eos::create-debugger-extra-patterns))
    (eos::ensure-available-print-frame)
    (eos::define-prefix-map)		; initialize keymap
  )

(defun eos::create-debugger-extra-patterns ()
  ;; returns a list of patterns
  (list
   (make-an-observer "SPRO_SPIDER_OUTPUT" 'eos::spro_spider_output-common)
   ))

(defun eos::register-debugger-extra-patterns ()
  ;; register additional dbx patterns
    (mapcar 'register-tooltalk-pattern eos::dbx-extra-pattern-list))

(defun eos::unregister-debugger-extra-patterns ()
  ;; unregister additional dbx patterns
  (mapcar 'unregister-tooltalk-pattern eos::dbx-extra-pattern-list))

;;
;; Common commands
;;


(defun eos::type () (interactive)
  (if (eq eos::dbx-or-debugger 'debugger)
      (call-interactively 'eos::dbx-cmd)
    (if (buffer-live-p eos::dbx-buffer)
	(switch-to-buffer eos::dbx-buffer)
      (message "no dbx subprocess buffer known"))))

(defun eos::run () (interactive) (eos::dbx-cmd "run"))
(defun eos::fix () (interactive) (eos::dbx-cmd "fix"))
(defun eos::build () (interactive) (eos::dbx-cmd "make"))

(defun eos::cont () (interactive) (eos::dbx-cmd "cont"))
(defun eos::cont-and-dismiss () (interactive)
  (eos::dismiss-print-frame) (eos::cont))
(defun eos::clear-all () (interactive) (eos::dbx-cmd "clear"))
(defun eos::next () (interactive) (eos::dbx-cmd "next"))
(defun eos::next-and-dismiss () (interactive)
  (eos::dismiss-print-frame) (eos::next))
(defun eos::step () (interactive) (eos::dbx-cmd "step"))
(defun eos::step-and-dismiss () (interactive)
  (eos::dismiss-print-frame) (eos::step))
(defun eos::step-up () (interactive) (eos::dbx-cmd "step up"))

(defun eos::up () (interactive)  (eos::dbx-cmd "up" ))
(defun eos::down () (interactive) (eos::dbx-cmd "down"))
(defun eos::pop () (interactive) (eos::dbx-cmd "pop"))


(defun eos::stop-at ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (null name) (error "Buffer has no associated file"))
    (eos::dbx-cmd
     (format "stop at \"%s\":%d" name (eos::line-at (point))))
    ))

(defun eos::clear-at ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (null name) (error "Buffer has no associated file"))
    (eos::dbx-cmd
     (format "clear \"%s\":%d" name (eos::line-at (point))))
	 ))

(defun eos::stop-in ()
  (interactive)
  (eos::dbx-cmd
   (format "stop in %s"
	   (if (eq 'x (device-type (selected-device)))
	       (x-get-selection)
	     (buffer-substring (point) (mark)))
	   ))
   (setq zmacs-region-stays t))

(defun eos::func ()
  (interactive)
  (eos::dbx-cmd
   (format "func %s"
	   (if (eq 'x (device-type (selected-device)))
	       (x-get-selection)
	     (buffer-substring (point) (mark)))
	   ))
  (setq zmacs-region-stays t))

(defun eos::cont-to ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (null name) (error "Buffer has no associated file"))
    (eos::dbx-cmd
     (format "stop at \"%s\":%d -temp; cont" name (eos::line-at (point))))
    ))

(defun eos::print-normal ()
  (interactive)
  (eos::dbx-cmd
   (format "print  %s"
	   (if (eq 'x (device-type (selected-device)))
	       (x-get-selection)
	     (buffer-substring (point) (mark)))
	   ))
  (setq zmacs-region-stays t))

(defun eos::print*-normal ()
  (interactive)
  (eos::dbx-cmd
   (format "print  *(%s)"
	   (if (eq 'x (device-type (selected-device)))
	       (x-get-selection)
	     (buffer-substring (point) (mark)))
	   ))
  (setq zmacs-region-stays t))

;; specialization for print commands

(defun eos::send-spider-print-msg (expr)
  ;; Print EXPR using separate frame
  (setq eos::last-command-was-print t)
  (eos::dbx-cmd (format "print %s" expr)))

(defun eos::send-spider-print*-msg (expr)
  ;; Send *EXPR using separate frame
  (setq eos::last-command-was-print t)
  (eos::dbx-cmd (format "print *(%s)" expr)))

(defun eos::print () (interactive)
 (eos::send-spider-print-msg
  (if (eq 'x (device-type (selected-device)))
      (x-get-selection)
    (buffer-substring (point) (mark)))
  )
 (setq zmacs-region-stays t))

(defun eos::print* () (interactive)
 (eos::send-spider-print*-msg
  (if (eq 'x (device-type (selected-device)))
      (x-get-selection)
    (buffer-substring (point) (mark)))
  )
 (setq zmacs-region-stays t))


;;
;;
;; Print on separate frame


(defun eos::buffer-line-size (buffer)
  (interactive)
  (or (bufferp buffer)
      (setq buffer (current-buffer)))
  (save-excursion
    (switch-to-buffer buffer)
    (eos::line-at (point-max))))

;;
;; Handling of a collection of print frames
;; (currently only one)

(defvar eos::print-frame nil "Frame for prints")
(defvar eos::print-buffer " *Eos Print Output*" "Buffer for prints")

(defun eos::new-available-print-frame()
  ;; returns an available print frame
  ;; currently just returns the one frame
  (require 'eos-toolbar  "sun-eos-toolbar")
  (let ((scr (selected-frame))
	(buf (current-buffer)))

    ;; create frames
    (if (and 
	 (frame-live-p eos::print-frame)
	 (or (not (frame-live-p eos::debugger-frame))
	     (not (eq eos::print-frame
		      eos::debugger-frame))))
	(progn
	  (make-frame-visible eos::print-frame)
	  eos::print-frame)
      (setq eos::print-frame (make-frame))
      ;; no modeline visible...
      (set-face-background 'modeline 
			   (face-background (get-face 'default))
			   eos::print-frame)
      (set-face-foreground 'modeline 
			   (face-background (get-face 'default))
			   eos::print-frame)
      ;; there is redundancy below.
      (select-frame eos::print-frame)
      (switch-to-buffer eos::print-buffer)
      (set-buffer-menubar nil)
      (add-spec-to-specifier (eos::toolbar-position) eos::print-toolbar (selected-frame))
      (add-spec-to-specifier has-modeline-p nil (selected-frame))
      (select-frame scr)
      (switch-to-buffer buf)
      eos::print-frame
      )))

;; set delete-frame-hook and check for this frame... then do 



(defun eos::ensure-available-print-frame ()
  ;; ensures that there is at least one available print frame
  t)

(defun eos::show-print-frame ()
  (interactive)
  (setq eos::print-frame (eos::new-available-print-frame))
  (select-frame eos::print-frame)
  (switch-to-buffer eos::print-buffer)
  (set-frame-height eos::print-frame
		     (+ 1 (eos::buffer-line-size eos::print-buffer)))
  (goto-char (point-min))
    )

(defun eos::dismiss-print-frame ()
  (interactive)
  (if (frame-live-p eos::print-frame)
      (progn
	(make-frame-invisible eos::print-frame)
	(select-frame (car (visible-frame-list))))))
;;
;; print output
;;

(defun eos::spro_spider_print_output (msg pat)
  ;; For spider print output (switched with spro_spider_output
  (let ((buf (current-buffer))
	(scr (selected-frame)))
    (save-excursion			; does not work in callbacks?
      (switch-to-buffer eos::print-buffer)
      (delete-region (point-min) (point-max))
      (goto-char (point-max))
      (insert (format "%s" (get-tooltalk-message-attribute msg
							   'arg_val 1)))
      (let ((err (get-tooltalk-message-attribute msg
						 'arg_val 2)))
	(if (and err (not (string-equal err "")))
	    (insert (format "STDERR> %s" err))))
      (eos::show-print-frame)
      (select-frame scr)
      (switch-to-buffer buf)
      )
    (destroy-tooltalk-message msg)
    (setq eos::last-command-was-print nil)
    ))


;; User interface

(defvar eos::prefix-map (make-keymap))

(defun eos::define-prefix-map ()

  (define-key eos::prefix-map "%" 'eos::dbx-cmd)
  (define-key eos::prefix-map "r" 'eos::run)
  (define-key eos::prefix-map "f" 'eos::fix)

  (define-key eos::prefix-map "p" 'eos::print)
  (define-key eos::prefix-map "\C-p" 'eos::print*)

  (define-key eos::prefix-map "c" 'eos::cont)
  (define-key eos::prefix-map "b" 'eos::stop-at)
  (define-key eos::prefix-map "\C-b" 'eos::clear-at)

  (define-key eos::prefix-map "n" 'eos::next)
  (define-key eos::prefix-map "s" 'eos::step)
  (define-key eos::prefix-map "\C-s" 'eos::step-up)

  (define-key eos::prefix-map "u" 'eos::up)
  (define-key eos::prefix-map "d" 'eos::down)

)

(defun eos::set-key-mode (mode)
  ;; Set the key MODE to either 'none, 'prefix, or 'function
  (setq eos::key-mode mode)
  (cond
   ((eq eos::key-mode 'none)
    (define-key global-map "\C-cd" nil)
    (eos::remove-function-keys)
    (add-submenu nil (append '("SPARCworks") eos::short-menu))
    )
   ((eq eos::key-mode 'prefix)
    (define-key global-map "\C-cd" eos::prefix-map)
    (eos::remove-function-keys)
    (add-submenu nil (append '("SPARCworks") eos::long-menu))
    )
   ((eq eos::key-mode 'function)
    (define-key global-map "\C-cd" nil)
    (eos::add-function-keys)
    (add-submenu nil (append '("SPARCworks") eos::long-menu))
    )
   (t
    (error "unimplemented")
    )))

(defun eos::add-function-keys ()
  (interactive)

  ;;
  (global-set-key [f6] 'eos::dbx-cmd)
  (global-set-key [(control f6)] 'eos::run)
  (global-set-key [(shift f6)] 'eos::fix)
  ;;
  (global-set-key [f7] 'eos::print)
  (global-set-key [(control f7)] 'eos::print*)
  (global-set-key [(shift f7)] 'eos::dismiss-print-frame)
  ;;
  (global-set-key [f8] 'eos::cont)
  (global-set-key [(control f8)] 'eos::stop-at)
  (global-set-key [(shift f8)] 'eos::clear-at)
  ;;
  (global-set-key [f9] 'eos::next)
  (global-set-key [(control f9)] 'eos::step)
  (global-set-key [(shift f9)] 'eos::step-up)
  ;;
  )

(defun eos::remove-function-keys ()
  (interactive)

  ;;
  (global-set-key [f6] nil)
  (global-set-key [(control f6)] nil)
  (global-set-key [(shift f6)] nil)
  ;;
  (global-set-key [f7] nil)
  (global-set-key [(control f7)] nil)
  (global-set-key [(shift f7)] nil)
  ;;
  (global-set-key [f8] nil)
  (global-set-key [(control f8)] nil)
  (global-set-key [(shift f8)] nil)
  ;;
  (global-set-key [f9] nil)
  (global-set-key [(control f9)] nil)
  (global-set-key [(shift f9)] nil)
  ;;
  )

;; Provides popup access

(defvar eos::popup-mode nil)
(defvar eos::saved-global-popup-menu nil)

(defun eos::toggle-popup-menu ()
  ;; Toggle whether to use or not popup menus for SPARCworks
  (interactive)
  (if eos::popup-mode
      (setq global-popup-menu eos::saved-global-popup-menu)
    (eos::push-popup-menu))
  (setq eos::popup-mode (null eos::popup-mode))
  )

(defun eos::push-popup-menu ()
  (setq eos::saved-global-popup-menu global-popup-menu)
  (setq global-popup-menu
	(append
	 '("SPARCworks Command"
	   ["Stop At" eos::stop-at t]
	   ["Clear At" eos::clear-at t]
	   ["Stop In" eos::stop-in t]
	   ["Cont To" eos::cont-to t]
	   ["Print" eos::print t]
	   ["Print*" eos::print* t]
	   "---"
	   ["Read a Dbx Command" eos::dbx-cmd t]
	   "---")
	 (list
	  eos::saved-global-popup-menu))
	))

(provide 'eos-debugger)

;;; sun-eos-debugger.el ends here
