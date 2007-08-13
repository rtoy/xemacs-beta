;;; help.el --- help commands for XEmacs.
;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:
 
;; This code implements XEmacs's on-line help system, the one invoked by
;;`M-x help-for-help'.
 
;;; Code:

;#### FSFmacs 
;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
;(eval-when-compile (require 'help-macro))

(defvar help-map (let ((map (make-sparse-keymap)))
                   (set-keymap-name map 'help-map)
                   (set-keymap-prompt
                     map (purecopy (gettext "(Type ? for further options)")))
                   map)
  "Keymap for characters following the Help key.")

;; global-map definitions moved to keydefs.el
(fset 'help-command help-map)

(define-key help-map (vector help-char) 'help-for-help)
(define-key help-map "?" 'help-for-help)
(define-key help-map 'help 'help-for-help)

(define-key help-map "\C-l" 'describe-copying) ; on \C-c in FSFmacs
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-w" 'describe-no-warranty)
(define-key help-map "a" 'hyper-apropos) ; 'command-apropos in FSFmacs
(define-key help-map "A" 'command-apropos)

(define-key help-map "b" 'describe-bindings)
(define-key help-map "B" 'describe-beta)
(define-key help-map "\C-p" 'describe-pointer)

(define-key help-map "c" 'describe-key-briefly)
(define-key help-map "k" 'describe-key)

(define-key help-map "d" 'describe-function)
(define-key help-map "e" 'describe-last-error)
(define-key help-map "f" 'describe-function)

(define-key help-map "F" 'xemacs-local-faq)

;;; Setup so Hyperbole can be autoloaded from a key.
;;; Choose a key on which to place the Hyperbole menus.
;;; For most people this key binding will work and will be equivalent
;;; to {C-h h}.
;;;
(or (where-is-internal 'hyperbole)
    (where-is-internal 'hui:menu)
    (define-key help-map "h" 'hyperbole))
(autoload 'hyperbole "hsite" "Hyperbole info manager menus." t)

(define-key help-map "i" 'info)
(define-key help-map '(control i) 'Info-query)
;; FSFmacs has Info-goto-emacs-command-node on C-f, no binding
;; for Info-elisp-ref
(define-key help-map '(control c) 'Info-goto-emacs-command-node)
(define-key help-map '(control k) 'Info-goto-emacs-key-command-node)
(define-key help-map '(control f) 'Info-elisp-ref)

(define-key help-map "l" 'view-lossage)

(define-key help-map "m" 'describe-mode)

(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "n" 'view-emacs-news)

(define-key help-map "p" 'finder-by-keyword)
(autoload 'finder-by-keyword "finder"
  "Find packages matching a given keyword." t)

(define-key help-map "s" 'describe-syntax)

(define-key help-map "t" 'help-with-tutorial)

(define-key help-map "w" 'where-is)

(define-key help-map "v" 'describe-variable)

(if (fboundp 'view-last-error)
    (define-key help-map "e" 'view-last-error))


(define-key help-map "q" 'help-quit)

;#### This stuff was an attempt to have font locking and hyperlinks in the
;help buffer, but it doesn't really work.  Some of this stuff comes from
;FSF Emacs; but the FSF Emacs implementation is rather broken, as usual.
;What needs to happen is this:
;
; -- we probably need a "hyperlink mode" from which help-mode is derived.
; -- this means we probably need multiple inheritance of modes!
;    Thankfully this is not hard to implement; we already have the
;    ability for a keymap to have multiple parents.  However, we'd
;    have to define any multiply-inherited-from modes using a standard
;    `define-mode' construction instead of manually doing it, because
;    we don't want each guy calling `kill-all-local-variables' and
;    messing up the previous one.
; -- we need to scan the buffer ourselves (not from font-lock, because
;    the user might not have font-lock enabled) and highlight only
;    those words that are *documented* functions and variables (and
;    probably excluding words without dashes in them unless enclosed
;    in quotes, so that common words like "list" and "point" don't
;    become hyperlinks.
; -- we should *not* use font-lock keywords like below.  Instead we
;    should add the font-lock stuff ourselves during the scanning phase,
;    if font-lock is enabled in this buffer. 

;(defun help-follow-reference (event extent user-data)
;  (let ((symbol (intern-soft (extent-string extent))))
;    (cond ((and symbol (fboundp symbol))
;	   (describe-function symbol))
;	  ((and symbol (boundp symbol))
;	   (describe-variable symbol))
;	  (t nil))))

;(defvar help-font-lock-keywords
;  (let ((name-char "[-+a-zA-Z0-9_*]") (sym-char "[-+a-zA-Z0-9_:*]"))
;    (list
;     ;;
;     ;; The symbol itself.
;     (list (concat "\\`\\(" name-char "+\\)\\(:\\)?")
;	   '(1 (if (match-beginning 2)
;		   'font-lock-function-name-face
;		 'font-lock-variable-name-face)
;	       nil t))
;     ;;
;     ;; Words inside `' which tend to be symbol names.
;     (list (concat "`\\(" sym-char sym-char "+\\)'")
;	   1 '(prog1
;		  'font-lock-reference-face
;		(add-list-mode-item (match-beginning 1)
;			       (match-end 1)
;			       nil
;			       'help-follow-reference))
;	   t)
;     ;;
;     ;; CLisp `:' keywords as references.
;     (list (concat "\\<:" sym-char "+\\>") 0 'font-lock-reference-face t)))
;  "Default expressions to highlight in Help mode.")

;(put 'help-mode 'font-lock-defaults '(help-font-lock-keywords))

(define-derived-mode help-mode view-major-mode "Help"
  "Major mode for viewing help text.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  )

(define-key help-mode-map "q" 'help-mode-quit)

(defun help-mode-quit ()
  "Exits from help mode, possibly restoring the previous window configuration."
  (interactive)
  (cond ((local-variable-p 'help-window-config (current-buffer))
         (let ((config help-window-config))
	   (kill-local-variable 'help-window-config)
	   (bury-buffer)
	   (set-window-configuration config)))
        ((one-window-p)
	 (bury-buffer))
        (t
         (delete-window))))

(defun help-quit ()
  (interactive)
  nil)

;; This is a grody hack of the same genotype as `advertised-undo'; if the
;; bindings of Backspace and C-h are the same, we want the menubar to claim
;; that `info' in invoked with `C-h i', not `BS i'.

(defun deprecated-help-command ()
  (interactive)
  (if (eq 'help-command (key-binding "\C-h"))
      (setq unread-command-event (character-to-event ?\C-h))
    (help-for-help)))

;;(define-key global-map 'backspace 'deprecated-help-command)

;; TUTORIAL arg is XEmacs addition
(defun help-with-tutorial (&optional tutorial)
  "Select the XEmacs learn-by-doing tutorial.
Optional arg TUTORIAL specifies the tutorial file; default is \"TUTORIAL\"."
  (interactive)
  (if (null tutorial)
      (setq tutorial "TUTORIAL"))
  (let ((file (expand-file-name (concat "~/" tutorial))))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name tutorial data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (let ((n (- (window-height (selected-window))
		  (count-lines (point-min) (point))
		  6)))
	(if (< n 12)
	    (newline n)
	  ;; Some people get confused by the large gap.
	  (newline (/ n 2))
	  (insert "[Middle of page left blank for didactic purposes.  "
		  "Text continues below]")
	  (newline (- n (/ n 2)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

;; used by describe-key and describe-key-briefly

(defun key-or-menu-binding (key &optional menu-flag)
  ;; KEY          is any value returned by next-command-event
  ;; MENU-FLAG    is a symbol that should be set to T if KEY is a menu event,
  ;;		  or NIL otherwise
  (let (defn)
    (and menu-flag (set menu-flag nil))
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key))
	     (or (misc-user-event-p (aref key 0))
		 (eq (car-safe (aref key 0)) 'menu-selection)))
	(let ((event (aref key 0)))
	  (setq defn (if (eventp event)
			 (list (event-function event) (event-object event))
		       (cdr event)))
	  (and menu-flag (set menu-flag t))
	  (if (eq (car defn) 'eval)
	      (setq defn (car (cdr defn))))
	  (if (eq (car-safe defn) 'call-interactively)
	      (setq defn (car (cdr defn))))
	  (if (and (consp defn) (null (cdr defn)))
	      (setq defn (car defn))))
      ;; else
      (setq defn (key-binding key)))
    ;; kludge: if a toolbar button was pressed on, try to find the
    ;; binding of the toolbar button.
    (if (and (eq defn 'press-toolbar-button)
	     (vectorp key)
	     (button-press-event-p (aref key (1- (length key)))))
	;; wait for the button release.  We're on shaky ground here ...
	(let ((event (next-command-event))
	      button)
	  (if (and (button-release-event-p event)
		   (event-over-toolbar-p event)
		   (eq 'release-and-activate-toolbar-button
		       (key-binding (vector event)))
		   (setq button (event-toolbar-button event)))
	      (toolbar-button-callback button)
	    ;; if anything went wrong, try returning the binding of
	    ;; the button-up event, of the original binding
	    (or (key-or-menu-binding (vector event))
		defn)))
      ;; no toolbar kludge
      defn)
    ))

(defun describe-key-briefly (key)
  "Print the name of the function KEY invokes.  KEY is a string."
  (interactive "kDescribe key briefly: ")
  (let (defn menup)
    (setq defn (key-or-menu-binding key 'menup))    
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      ;; If it's a keyboard macro which trivially invokes another command,
      ;; document that instead.
      (if (or (stringp defn) (vectorp defn))
	  (setq defn (or (key-binding defn)
			 defn)))
      (let ((last-event (and (vectorp key)
			     (aref key (1- (length key))))))
	(message (if (or (button-press-event-p last-event)
			 (button-release-event-p last-event))
		     (gettext "%s at that spot runs the command %s")
		   (gettext "%s runs the command %s"))
		 ;; This used to say 'This menu item' but it could also
		 ;; be a scrollbar event.  We can't distinguish at the
		 ;; moment.
		 (if menup "This item" (key-description key))
		 (if (symbolp defn) defn (prin1-to-string defn)))))))

;; #### this is a horrible piece of shit function that should
;; not exist.  In FSF 19.30 this function has gotten three times
;; as long and has tons and tons of dumb shit checking
;; special-display-buffer-names and such crap.  I absolutely
;; refuse to insert that Ebolification here.  I wanted to delete
;; this function entirely but Mly bitched.
;;
;; If your user-land code calls this function, rewrite it to
;; call with-displaying-help-buffer.

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (funcall
	(or function 'message)
	(concat
         (substitute-command-keys
          (if (one-window-p t)
              (if pop-up-windows
                  (gettext "Type \\[delete-other-windows] to remove help window.")
                (gettext "Type \\[switch-to-buffer] RET to remove help window."))
   (gettext "Type \\[switch-to-buffer-other-window] RET to restore the other window.")))
         (substitute-command-keys
          (gettext "  \\[scroll-other-window] to scroll the help."))))))

(defvar help-selects-help-window t
  "*If nil, use the \"old Emacs\" behavior for Help buffers.
This just displays the buffer in another window, rather than selecting
the window.")

(defvar help-window-config nil)

;; Use this function for displaying help when C-h something is pressed
;; or in similar situations.  Do *not* use it when you are displaying
;; a help message and then prompting for input in the minibuffer --
;; this macro usually selects the help buffer, which is not what you
;; want in those situations.

;;; ### Should really be a macro (as suggested above) to eliminate the
;;; requirement of caller to code a lambda form in THUNK -- mrb
(defun with-displaying-help-buffer (thunk)
  (let ((winconfig (current-window-configuration))
        (was-one-window (one-window-p)))
    (prog1 (with-output-to-temp-buffer "*Help*"
             (prog1 (funcall thunk)
               (save-excursion
                 (set-buffer standard-output)
                 (help-mode))))
      (let ((helpwin (get-buffer-window "*Help*")))
        (if helpwin
            (progn
              (save-excursion
                (set-buffer (window-buffer helpwin))
                (set (make-local-variable 'help-window-config) winconfig))
              (if help-selects-help-window
                  (select-window helpwin))
              (cond ((eq helpwin (selected-window))
                     (message
                      (substitute-command-keys "Type \\[help-mode-quit] to remove help window, \\[scroll-up] to scroll the help.")))
                    (was-one-window
                     (message
                      (substitute-command-keys "Type \\[delete-other-windows] to remove help window, \\[scroll-other-window] to scroll the help.")))
                    (t
                     (message
                      (substitute-command-keys "Type \\[switch-to-buffer-other-window] to restore the other window, \\[scroll-other-window] to scroll the help."))))
	      (when temp-buffer-shrink-to-fit
		(shrink-window-if-larger-than-buffer helpwin))))))))

(defun describe-key (key)
  "Display documentation of the function invoked by KEY.
KEY is a string, or vector of events.
When called interactively, KEY may also be a menu selection."
  (interactive "kDescribe key: ")
  (let ((defn (key-or-menu-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (with-displaying-help-buffer
       (lambda ()
;	 (princ (key-description key))
;	 (princ " runs the command ")
	 (prin1 defn)
	 (princ ":\n")
	 (cond ((or (stringp defn) (vectorp defn))
		(let ((cmd (key-binding defn)))
		  (if (not cmd)
		      (princ "a keyboard macro")
		    (progn
		      (princ (format "a keyboard macro which runs the command %s:\n\n"
				     cmd))
		      (princ cmd)
		      (princ "\n")
		      (if (documentation cmd) (princ (documentation cmd)))))))
	       ((and (consp defn) (not (eq 'lambda (car-safe defn))))
		(princ "\n")
		(let ((describe-function-show-arglist nil))
		  (describe-function-1 (car defn) standard-output)))
	       ((documentation defn)
		(princ (documentation defn)))
	       (t
		(princ "not documented"))))))))

(defun describe-mode ()
  "Display documentation of current major mode and minor modes.
For this to work correctly for a minor mode, the mode's indicator variable
\(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     ;; XEmacs change: print the major-mode documentation before
     ;; the minor modes.
     (princ mode-name)
     (princ " mode:\n")
     (princ (documentation major-mode))
     (princ "\n\n----\n\n")
     (let ((minor-modes minor-mode-alist))
       (while minor-modes
	 (let* ((minor-mode (car (car minor-modes)))
		(indicator (car (cdr (car minor-modes)))))
	   ;; Document a minor mode if it is listed in minor-mode-alist,
	   ;; bound locally in this buffer, non-nil, and has a function
	   ;; definition.
	   (if (and (boundp minor-mode)
		    (symbol-value minor-mode)
		    (fboundp minor-mode))
	       (let ((pretty-minor-mode minor-mode))
		 (if (string-match "-mode\\'" (symbol-name minor-mode))
		     (setq pretty-minor-mode
			   (capitalize
			    (substring (symbol-name minor-mode)
				       0 (match-beginning 0)))))
		 (while (and (consp indicator) (extentp (car indicator)))
		   (setq indicator (cdr indicator)))
		 (while (and indicator (symbolp indicator))
		   (setq indicator (symbol-value indicator)))
		 (princ (format "%s minor mode (indicator%s):\n"
				pretty-minor-mode indicator))
		 (princ (documentation minor-mode))
		 (princ "\n\n----\n\n"))))
	 (setq minor-modes (cdr minor-modes)))))))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of XEmacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "DISTRIB" data-directory)))

(defun describe-beta ()
  "Display info on how to deal with Beta versions of XEmacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "BETA" data-directory))
  (goto-char (point-min)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of XEmacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "COPYING" data-directory))
  (goto-char (point-min)))

(defun describe-pointer ()
  "Show a list of all defined mouse buttons, and their definitions."
  (interactive)
  (describe-bindings nil t))

(defun describe-project ()
  "Display info on the GNU project."
  (interactive)
  (find-file-read-only
   (expand-file-name "GNU" data-directory))
  (goto-char (point-min)))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty XEmacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun describe-bindings (&optional prefix mouse-only-p)
  "Show a list of all defined keys, and their definitions.
The list is put in a buffer, which is displayed.
If the optional argument PREFIX is supplied, only commands which
start with that sequence of keys are described.
If the second argument (prefix arg, interactively) is non-null
then only the mouse bindings are displayed."
  (interactive (list nil current-prefix-arg))
  (with-displaying-help-buffer
   (lambda ()
     (describe-bindings-1 prefix mouse-only-p))))

(defun describe-bindings-1 (&optional prefix mouse-only-p)
  (let ((heading (if mouse-only-p
            (gettext "button          binding\n------          -------\n")
            (gettext "key             binding\n---             -------\n")))
        (buffer (current-buffer))
        (minor minor-mode-map-alist)
        (local (current-local-map))
        (shadow '()))
    (set-buffer standard-output)
    (while minor
      (let ((sym (car (car minor)))
            (map (cdr (car minor))))
        (if (symbol-value-in-buffer sym buffer nil)
            (progn
              (insert (format "Minor Mode Bindings for `%s':\n"
                              sym)
                      heading)
              (describe-bindings-internal map nil shadow prefix mouse-only-p)
              (insert "\n")
              (setq shadow (cons map shadow))))
        (setq minor (cdr minor))))
    (if local
        (progn
          (insert "Local Bindings:\n" heading)
          (describe-bindings-internal local nil shadow prefix mouse-only-p)
          (insert "\n")
          (setq shadow (cons local shadow))))
    (insert "Global Bindings:\n" heading)
    (describe-bindings-internal (current-global-map)
                                nil shadow prefix mouse-only-p)
    (set-buffer buffer)))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let* ((key (this-command-keys))
	 (prefix (make-vector (1- (length key)) nil))
	 i)
    (setq i 0)
    (while (< i (length prefix))
      (aset prefix i (aref key i))
      (setq i (1+ i)))
    (with-displaying-help-buffer
     (lambda ()
       (princ "Key bindings starting with ")
       (princ (key-description prefix))
       (princ ":\n\n")
       (describe-bindings-1 prefix nil)))))

;; Make C-h after a prefix, when not specifically bound, 
;; run describe-prefix-bindings.
(setq prefix-help-command 'describe-prefix-bindings)

(defun view-emacs-news ()
  "Display info on recent changes to XEmacs."
  (interactive)
  (require 'outl-mouse)
  (find-file (expand-file-name "NEWS" data-directory)))

(defun xemacs-www-page ()
  "Go to the XEmacs World Wide Web page."
  (interactive)
  (funcall browse-url-browser-function "http://www.xemacs.org/"))

(defun xemacs-www-faq ()
  "View the latest and greatest XEmacs FAQ using the World Wide Web."
  (interactive)
  (funcall browse-url-browser-function "http://www.xemacs.org/faq/index.html"))

(defun xemacs-local-faq ()
  "View the local copy of the XEmacs FAQ.
If you have access to the World Wide Web, you should use `xemacs-www-faq'
instead, to ensure that you get the most up-to-date information."
  (interactive)
  (save-window-excursion
    (info)
    (Info-find-node "xemacs-faq" "Top"))
  (switch-to-buffer "*info*"))

(defun view-lossage ()
  "Display last 100 input keystrokes and last 100 or so minibuffer messages."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     (princ (key-description (recent-keys)))
     (save-excursion
       (set-buffer standard-output)
       (goto-char (point-min))
       (insert "Recent keystrokes:\n\n")
       (while (progn (move-to-column 50) (not (eobp)))
	 (search-forward " " nil t)
	 (insert "\n")))
     ;; XEmacs addition
     (princ "\n\n\nRecent minibuffer messages (most recent first):\n\n")
     (save-excursion
       (let ((buffer (get-buffer " *Message-Log*"))
	     (count 0)
	     oldpoint)
	 (set-buffer buffer)
	 (goto-char (point-max))
	 (set-buffer standard-output)
	 (while (and (> (point buffer) (point-min buffer))
		     (< count 100))
	   (setq oldpoint (point buffer))
	   (forward-line -1 buffer)
	   (insert-buffer-substring buffer (point buffer) oldpoint)
	   (setq count (1+ count))))))))

(define-function 'help 'help-for-help)
;; #### FSF calls `make-help-screen' here.  We need to port `help-macro.el'.
(defun help-for-help ()
  "You have typed \\[help-for-help], the help character.  Type a Help option:
\(Use SPC or DEL to scroll through this text.  Type \\<help-map>\\[help-quit] to exit the Help command.)

\\[hyper-apropos]	Give a substring, and see a hypertext list of
        functions and variables that contain that substring.
	See also the `apropos'  command.
\\[command-apropos]	Give a substring, and see a list of commands
        (functions interactively callable) that contain that substring.
\\[describe-bindings]	Display table of all key bindings.
\\[describe-key-briefly]	Type a command key sequence;
        it prints the function name that sequence runs.
\\[Info-goto-emacs-command-node]	Type a function name;
 	it takes you to the Info node for that command.
\\[describe-function]	Type a function name and get documentation of it.
\\[Info-elisp-ref]	Type a function name and jump to the full documentation
	in the XEmacs Lisp Programmer's Manual.
\\[xemacs-local-faq]	To view a local copy of the XEmacs FAQ.
\\[info]	The  info  documentation reader.
\\[Info-query]	Info reader, prompt for topic name.
\\[describe-key]	Type a command key sequence;
        it displays the full documentation.
\\[Info-goto-emacs-key-command-node]	Type a command key sequence;
        it takes you to the Info node for the command bound to that key.
\\[view-lossage]	Shows last 100 characters you typed.
\\[describe-mode]	Print documentation of current major mode,
        which describes the commands peculiar to it.
\\[view-emacs-news]	Shows emacs news file.
\\[finder-by-keyword]	Find packages matching a given topic keyword.
\\[describe-pointer]	Display table of all mouse-button bindings.
\\[describe-syntax]	Display contents of syntax table, plus explanations
\\[help-with-tutorial]	Select the XEmacs learn-by-doing tutorial.
\\[describe-variable]	Type name of a variable;
        it displays the variable's documentation and value.
\\[where-is]	Type command name;
        it prints which keystrokes invoke that command.
\\[describe-distribution]	XEmacs ordering information.
\\[describe-copying]	print XEmacs copying permission (General Public License).
\\[view-emacs-news]	print print news of recent XEmacs changes.
\\[describe-no-warranty]	print information on absence of warranty for XEmacs."
  (interactive)
  (let ((help-key (copy-event last-command-event))
	event char)
    (message (gettext "A B C F I K L M N P S T V W C-c C-d C-n C-w.  Type %s again for more help: ")
	     ;; arrgh, no room for "C-i C-k C-f" !!
	     (single-key-description help-key))
    (setq event (next-command-event)
	  char (event-to-character event))
    (if (or (equal event help-key)
	    (eq char ??)
	    (eq 'help-command (key-binding event)))
	(save-window-excursion
	  (switch-to-buffer "*Help*")
	  ;; #### I18N3 should mark buffer as output-translating
	  (delete-other-windows)
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (insert (documentation 'help-for-help)))
	  (goto-char (point-min))
	  (while (or (equal event help-key)
		     (eq char ??)
		     (eq 'help-command (key-binding event))
		     (eq char ? )
		     (eq 'scroll-up (key-binding event))
		     (eq char ?\177)
		     (and (not (eq char ?b))
			  (eq 'scroll-down (key-binding event))))
	    (if (or (eq char ? )
		    (eq 'scroll-up (key-binding event)))
		(scroll-up))
	    (if (or (eq char ?\177)
		    (and (not (eq char ?b))
			 (eq 'scroll-down (key-binding event))))
		(scroll-down))
	    ;; write this way for I18N3 snarfing
	    (if (pos-visible-in-window-p (point-max))
		(message "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f: ")
	      (message "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f or Space to scroll: "))
	    (let ((cursor-in-echo-area t))
	      (setq event (next-command-event event)
		    char (or (event-to-character event) event))))))
    (let ((defn (or (lookup-key help-map (vector event))
 		    (and (numberp char)
 			 (lookup-key help-map (make-string 1 (downcase char)))))))
      (message nil)
      (if defn
 	  (call-interactively defn)
 	(ding)))))

;; Return a function which is called by the list containing point.
;; If that gives no function, return a function whose name is around point.
;; If that doesn't give a function, return nil.
(defun function-called-at-point ()
  (or (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) (fboundp obj) obj))))
	(error nil))
      (condition-case ()
	  (let ((stab (syntax-table)))
	    (unwind-protect
		(save-excursion
		  (set-syntax-table emacs-lisp-mode-syntax-table)
		  (or (not (zerop (skip-syntax-backward "_w")))
		      (eq (char-syntax (following-char)) ?w)
		      (eq (char-syntax (following-char)) ?_)
		      (forward-sexp -1))
		  (skip-chars-forward "`'")
		  (let ((obj (read (current-buffer))))
		    (and (symbolp obj) (fboundp obj) obj)))
	      (set-syntax-table stab)))
	(error nil))))

(defvar describe-function-show-arglist t  ; default to nil for the non-hackers?
  "*If true, then describe-function will show its arglist if the function is
not an autoload.")

(defun describe-function-find-file (function)
  (and (boundp 'load-history) ; not standardly bound in XEmacs
       (let ((files load-history)
	     file)
	 (while files
	   (if (memq function (cdr (car files)))
	       (setq file (car (car files)) files nil))
	   (setq files (cdr files)))
	 file)))

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
    (let* ((fn (function-called-at-point))
           (val (let ((enable-recursive-minibuffers t))
                  (completing-read
                    (if fn
                        (format (gettext "Describe function (default %s): ")
				fn)
                        (gettext "Describe function: "))
                    obarray 'fboundp t))))
      (list (if (equal val "") fn (intern val)))))
  (with-displaying-help-buffer
   (lambda ()
     (describe-function-1 function standard-output)
     (save-excursion
       (set-buffer standard-output)
       ;; Return the text we displayed.
       (buffer-string)))))

(defun function-obsolete-p (function)
  "Return non-nil if FUNCTION is obsolete."
  (not (null (get function 'byte-obsolete-info))))

(defun function-obsoleteness-doc (function)
  "If FUNCTION is obsolete, return a string describing this."
  (let ((obsolete (get function 'byte-obsolete-info)))
    (if obsolete
	(format "Obsolete; %s"
		(if (stringp (car obsolete))
		    (car obsolete)
		  (format "use `%s' instead." (car obsolete)))))))

(defun function-compatible-p (function)
  "Return non-nil if FUNCTION is present for Emacs compatibility."
  (not (null (get function 'byte-compatible-info))))

(defun function-compatibility-doc (function)
  "If FUNCTION is Emacs compatible, return a string describing this."
  (let ((compatible (get function 'byte-compatible-info)))
    (if compatible
	(format "Emacs Compatible; %s"
		(if (stringp (car compatible))
		    (car compatible)
		  (format "use `%s' instead." (car compatible)))))))

;Here are all the possibilities below spelled out, for the benefit
;of the I18N3 snarfer.
;
;(gettext "a built-in function")
;(gettext "an interactive built-in function")
;(gettext "a built-in macro")
;(gettext "an interactive built-in macro")
;(gettext "a compiled Lisp function")
;(gettext "an interactive compiled Lisp function")
;(gettext "a compiled Lisp macro")
;(gettext "an interactive compiled Lisp macro")
;(gettext "a Lisp function")
;(gettext "an interactive Lisp function")
;(gettext "a Lisp macro")
;(gettext "an interactive Lisp macro")
;(gettext "a mocklisp function")
;(gettext "an interactive mocklisp function")
;(gettext "a mocklisp macro")
;(gettext "an interactive mocklisp macro")
;(gettext "an autoloaded Lisp function")
;(gettext "an interactive autoloaded Lisp function")
;(gettext "an autoloaded Lisp macro")
;(gettext "an interactive autoloaded Lisp macro")

(defun describe-function-1 (function stream &optional nodoc)
  (prin1 function stream)
  (princ ": " stream)
  (let* ((def function)
	 file-name
         (doc (or (documentation function)
                  (gettext "not documented")))
	 aliases home kbd-macro-p fndef macrop)
    (while (symbolp def)
      (or (eq def function)
	  (if aliases
	      ;; I18N3 Need gettext due to concat
	      (setq aliases (concat aliases 
				    (format "\n     which is an alias for %s, "
					    (symbol-name def))))
	    (setq aliases (format "an alias for %s, " (symbol-name def)))))
      (setq def (symbol-function def)))
    (if (compiled-function-p def)
	(setq home (compiled-function-annotation def)))
    (if (eq 'macro (car-safe def))
	(setq fndef (cdr def)
	      macrop t)
      (setq fndef def))
    (if describe-function-show-arglist
        (if (cond ((eq 'autoload (car-safe fndef))
                   nil)
                  ((eq 'lambda (car-safe fndef))
                   (princ (or (nth 1 fndef) "()") stream)
                   t)
                  ((compiled-function-p fndef)
                   (princ (or (compiled-function-arglist fndef) "()") stream)
                   t)
                  ((and (subrp fndef)
                        (string-match "[\n\t ]*\narguments: ?\\((.*)\\)\n?\\'"
                                      doc))
                   (princ (substring doc (match-beginning 1) (match-end 1))
                          stream)
                   (setq doc (substring doc 0 (match-beginning 0)))
                   t)
                  (t
                   nil))
            (princ "\n  -- " stream)))
    (if aliases (princ aliases stream))
    (let ((int #'(lambda (string an-p macro-p)
		   (princ (format
			   (gettext (concat
				     (cond ((commandp def)
					    "an interactive ")
					   (an-p "an ")
					   (t "a "))
				     "%s"
				     (if macro-p " macro" " function")))
			   string)
			  stream))))
      (cond ((or (stringp def) (vectorp def))
             (princ "a keyboard macro." stream)
	     (setq kbd-macro-p t))
            ((subrp fndef)
             (funcall int "built-in" nil macrop))
            ((compiled-function-p fndef)
             (funcall int "compiled Lisp" nil macrop))
;	     XEmacs -- we handle aliases above.
;            ((symbolp fndef)
;             (princ (format "alias for `%s'"
;			    (prin1-to-string def)) stream))
            ((eq (car-safe fndef) 'lambda)
             (funcall int "Lisp" nil macrop))
            ((eq (car-safe fndef) 'mocklisp)
             (funcall int "mocklisp" nil macrop))
            ((eq (car-safe def) 'autoload)
	     (setq file-name (elt def 1))
	     (funcall int "autoloaded Lisp" t (elt def 4)))
            (t
             nil)))
    (or file-name
	(setq file-name (describe-function-find-file function)))
    (if file-name
	(princ (format ".\n  -- loads from \"%s\"" file-name) stream))
    (if home
	(princ (format ".\n  -- loaded from %s" home)))
    (princ ".")
    (terpri)
    (cond (kbd-macro-p
	   (princ "These characters are executed:\n\n\t" stream)
	   (princ (key-description def) stream)
	   (cond ((setq def (key-binding def))
		  (princ (format "\n\nwhich executes the command %s.\n\n" def) stream)
		  (describe-function-1 def stream))))
	  (nodoc nil)
	  (t
	   ;; tell the user about obsoleteness.
	   ;; If the function is obsolete and is aliased, don't
	   ;; even bother to report the documentation, as a further
	   ;; encouragement to use the new function.
	   (let ((obsolete (function-obsoleteness-doc function))
		 (compatible (function-compatibility-doc function)))
	     (if obsolete
		 (progn
		   (princ obsolete stream)
		   (terpri stream)
		   (terpri stream)))
	     (if compatible
		 (progn
		   (princ compatible stream)
		   (terpri stream)
		   (terpri stream)))
	     (if (not (and obsolete aliases))
		 (progn
		   (princ doc stream)
		   (or (eq ?\n (aref doc (1- (length doc))))
		       (terpri)))))))))


(defun describe-function-arglist (function)
  (interactive (list (or (function-called-at-point)
			 (error "no function call at point"))))
  (let ((b nil))
    (unwind-protect
	(save-excursion
	  (set-buffer (setq b (get-buffer-create " *arglist*")))
	  (buffer-disable-undo b)
	  (erase-buffer)
	  (describe-function-1 function b t)
	  (goto-char (point-min))
	  (end-of-line)
	  (or (eobp) (delete-char 1))
	  (just-one-space)
	  (end-of-line)
	  (message (buffer-substring (point-min) (point))))
      (and b (kill-buffer b)))))


(defun variable-at-point ()
  (condition-case ()
      (let ((stab (syntax-table)))
	(unwind-protect
	    (save-excursion
	      (set-syntax-table emacs-lisp-mode-syntax-table)
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (boundp obj) obj)))
	  (set-syntax-table stab)))
    (error nil)))

(defun variable-obsolete-p (variable)
  "Return non-nil if VARIABLE is obsolete."
  (not (null (get variable 'byte-obsolete-variable))))

(defun variable-obsoleteness-doc (variable)
  "If VARIABLE is obsolete, return a string describing this."
  (let ((obsolete (get variable 'byte-obsolete-variable)))
    (if obsolete
	(format "Obsolete; %s"
		(if (stringp obsolete)
		    obsolete
		  (format "use `%s' instead." obsolete))))))

(defun variable-compatible-p (variable)
  "Return non-nil if VARIABLE is Emacs compatible."
  (not (null (get variable 'byte-compatible-variable))))

(defun variable-compatibility-doc (variable)
  "If VARIABLE is Emacs compatible, return a string describing this."
  (let ((compatible (get variable 'byte-compatible-variable)))
    (if compatible
	(format "Emacs Compatible; %s"
		(if (stringp compatible)
		    compatible
		  (format "use `%s' instead." compatible))))))

(defun built-in-variable-doc (variable)
  "Return a string describing whether VARIABLE is built-in."
  (let ((type (built-in-variable-type variable)))
    (cond ((eq type 'integer) "a built-in integer variable")
	  ((eq type 'const-integer) "a built-in constant integer variable")
	  ((eq type 'boolean) "a built-in boolean variable")
	  ((eq type 'const-boolean) "a built-in constant boolean variable")
	  ((eq type 'object) "a simple built-in variable")
	  ((eq type 'const-object) "a simple built-in constant variable")
	  ((eq type 'const-specifier) "a built-in constant specifier variable")
	  ((eq type 'current-buffer) "a built-in buffer-local variable")
	  ((eq type 'const-current-buffer)
	   "a built-in constant buffer-local variable")
	  ((eq type 'default-buffer)
	   "a built-in default buffer-local variable")
	  ((eq type 'selected-console) "a built-in console-local variable")
	  ((eq type 'const-selected-console)
	   "a built-in constant console-local variable")
	  ((eq type 'default-console)
	   "a built-in default console-local variable")
	  (type "an unknown type of built-in variable?")
	  (t "a variable declared in Lisp"))))

(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive 
   (let* ((v (variable-at-point))
          (val (let ((enable-recursive-minibuffers t))
                 (completing-read
                   (if v
                       (format "Describe variable (default %s): " v)
                       (gettext "Describe variable: "))
                   obarray 'boundp t))))
     (list (if (equal val "") v (intern val)))))
  (with-displaying-help-buffer
   (lambda ()
     (let ((origvar variable)
	   aliases)
       (let ((print-escape-newlines t))
	 (while (variable-alias variable)
	   (let ((newvar (variable-alias variable)))
	     (if aliases
		 ;; I18N3 Need gettext due to concat
		 (setq aliases
		       (concat aliases 
			       (format ",\n     which is an alias for %s"
				       (symbol-name newvar))))
	       (setq aliases
		     (format "%s is an alias for %s"
			     (symbol-name variable)
			     (symbol-name newvar))))
	     (setq variable newvar)))
	 (if aliases
	     (princ (format "%s.\n" aliases)))
	 (if (not (boundp variable))
	     (princ (format "%s is void" variable))
	   (princ (format "%s's value is " variable))
	   (prin1 (symbol-value variable)))
	 (terpri)
	 (princ "  -- ")
	 (princ (built-in-variable-doc variable))
	 (princ ".")
	 (terpri)
	 (cond ((local-variable-p variable (current-buffer))
		(let* ((void (cons nil nil))
		       (def (condition-case nil
				(default-value variable)
			      (error void))))
		  (princ "This value is specific to the current buffer.")
		  (terpri)
		  (if (local-variable-p variable nil)
		      (progn
			(princ "(Its value is local to each buffer.)")
			(terpri)))
		  (if (if (eq def void)
			  (boundp variable)
			(not (eq (symbol-value variable) def)))
		      ;; #### I18N3 doesn't localize properly!
		      (progn (princ "Its default-value is ")
			     (if (eq def void)
				 (princ "void.")
			       (prin1 def))
			     (terpri)))))
	       ((local-variable-p variable (current-buffer) t)
		(princ "Setting it would make its value buffer-local.\n")
		(terpri))))
       (terpri)
       (princ "Documentation:")
       (terpri)
       (let ((doc (documentation-property variable 'variable-documentation))
	     (obsolete (variable-obsoleteness-doc origvar))
	     (compatible (variable-compatibility-doc origvar)))
	 (if obsolete
	     (progn
	       (princ obsolete)
	       (terpri)
	       (terpri)))
	 (if compatible
	     (progn
	       (princ compatible)
	       (terpri)
	       (terpri)))
	 ;; don't bother to print anything if variable is obsolete and aliased.
	 (if (or (not obsolete) (not aliases))
	     (if doc
		 ;; note: documentation-property calls substitute-command-keys.
		 (princ doc)
	       (princ "not documented as a variable."))))
       (save-excursion
	 (set-buffer standard-output)
	 ;; Return the text we displayed.
	 (buffer-string))))))

(defun where-is (definition)
  "Print message listing key sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function definition."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     ;; #### should use `read-command'
     (setq val (completing-read (if fn
				    (format "Where is command (default %s): " fn)
				  "Where is command: ")
				obarray 'commandp t nil
				'read-command-history))
     (list (if (equal val "")
	       fn (intern val)))))
  (let ((keys (where-is-internal definition)))
    (if keys
	(message "%s is on %s" definition
                 (mapconcat 'key-description
                            (sort keys #'(lambda (x y)
                                           (< (length x) (length y))))
                            ", "))
        (message "%s is not on any keys" definition)))
  nil)

(defun locate-library (library &optional nosuffix)
  "Show the full path name of XEmacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
  (interactive "sLocate library: \nP")
  ;; Let's accept both symbols and strings, since they're often equivalent
  (when (symbolp library)
    (setq library (symbol-name library)))
  ;; XEmacs: We have the nifty `locate-file' so we use it.
  (let ((file (locate-file library load-path (if nosuffix nil ".elc:.el:"))))
    (when (interactive-p)
      (if file
	  (message "Library is file %s" file)
	(message "No library %s in search path" library)))
    file))

;; Functions ported from C into Lisp in XEmacs

(defun describe-syntax ()
  "Describe the syntax specifications in the syntax table.
The descriptions are inserted in a buffer, which is then displayed."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     ;; defined in syntax.el
     (describe-syntax-table (syntax-table) standard-output))))

(defun list-processes ()
  "Display a list of all processes.
\(Any processes listed as Exited or Signaled are actually eliminated
after the listing is made.)"
  (interactive)
  (with-output-to-temp-buffer "*Process List*"
    (set-buffer standard-output)
    (buffer-disable-undo standard-output)
    (make-local-variable 'truncate-lines)
    (setq truncate-lines t)
    (let ((stream standard-output))
      ;;      00000000001111111111222222222233333333334444444444
      ;;      01234567890123456789012345678901234567890123456789
      ;; rewritten for I18N3.  This one should stay rewritten
      ;; so that the dashes will line up properly.
      (princ "Proc         Status   Buffer         Tty         Command\n----         ------   ------         ---         -------\n" stream)
      (let ((tail (process-list)))
        (while tail
          (let* ((p (car tail))
                 (pid (process-id p))
                 (s (process-status p)))
            (setq tail (cdr tail))
            (princ (format "%-13s" (process-name p)) stream)
            ;(if (and (eq system-type 'vax-vms)
            ;         (eq s 'signal)
            ;         (< (process-exit-status p) NSIG))
            ;    (princ (aref sys_errlist (process-exit-status p)) stream))
            (princ s stream)
            (if (and (eq s 'exit) (/= (process-exit-status p) 0))
                (princ (format " %d" (process-exit-status p)) stream))
            (if (memq s '(signal exit closed))
                ;; Do delete-exited-processes' work
                (delete-process p))
            (indent-to 22 1)            ;####
            (let ((b (process-buffer p)))
              (cond ((not b)
                     (princ "(none)" stream))
                    ((not (buffer-name b))
                     (princ "(killed)" stream))
                    (t
                     (princ (buffer-name b) stream))))
            (indent-to 37 1)            ;####
            (let ((tn (process-tty-name p)))
              (cond ((not tn)
                     (princ "(none)" stream))
                    (t
                     (princ (format "%s" tn) stream))))
            (indent-to 49 1)            ;####
            (if (not (integerp pid))
                (progn
                  (princ "network stream connection " stream)
                  (princ (car pid) stream)
                  (princ "@" stream)
                  (princ (cdr pid) stream))
                (let ((cmd (process-command p)))
                  (while cmd
                    (princ (car cmd) stream)
                    (setq cmd (cdr cmd))
                    (if cmd (princ " " stream)))))
            (terpri stream)))))))
