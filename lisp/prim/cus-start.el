;;; cus-start.el --- define customization properties of builtins.
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: internal

;;; Commentary:
;;
;; Must be run before the user has changed the value of any options!

;;; Code:

(require 'custom)

(defun custom-start-quote (sexp)
  ;; This is copied from `cus-edit.el'.
  "Quote SEXP iff it is not self quoting."
  (if (or (memq sexp '(t nil))
	  (and (symbolp sexp)
	       (eq (aref (symbol-name sexp) 0) ?:))
	  (and (listp sexp)
	       (memq (car sexp) '(lambda)))
	  (stringp sexp)
	  (numberp sexp)
	  (and (fboundp 'characterp)
	       (characterp sexp)))
      sexp
    (list 'quote sexp)))

;; The file names below are unreliable, as they are from Emacs 19.34.
(let ((all '(;; abbrev.c 
	     (abbrev-all-caps abbrev-mode boolean)
	     (pre-abbrev-expand-hook abbrev-mode hook)
	     ;; alloc.c
	     (gc-cons-threshold alloc integer)
	     ;; buffer.c
	     (modeline-format modeline sexp) ;Hard to do right.
	     (default-major-mode internal function)
	     (case-fold-search matching boolean)
	     (fill-column fill integer)
	     (left-margin fill integer)
	     (tab-width editing-basics integer)
	     (ctl-arrow display boolean)
	     (truncate-lines display boolean)
	     (selective-display display 
				(choice (const :tag "off" nil)
					(integer :tag "space"
						 :format "%v"
						 1)
					(const :tag "on" t)))
	     (selective-display-ellipses display boolean)
	     ;; callproc.c
	     (shell-file-name execute file)
	     (exec-path execute
			(repeat (choice (const :tag "default" nil)
					(file :format "%v"))))
	     ;; dired.c
	     (completion-ignored-extensions dired 
					    (repeat (string :format "%v")))
	     ;; dispnew.el
	     (visible-bell display boolean)
	     (no-redraw-on-reenter display boolean)
	     ;; eval.c
	     (max-specpdl-size limits integer)
	     (max-lisp-eval-depth limits integer)
	     (stack-trace-on-error debug
				   (choice (const :tag "off")
					   (repeat :menu-tag "When"
						   :value (nil)
						   (symbol :format "%v"))
					   (const :tag "always" t)))
	     (debug-on-error debug 
			     (choice (const :tag "off")
				     (repeat :menu-tag "When"
					     :value (nil)
					     (symbol :format "%v"))
				     (const :tag "always" t)))
	     (debug-on-quit debug choice)
	     ;; frame.c
	     (default-frame-plist frames
	       (repeat (cons :format "%v"
			     (symbol :tag "Parameter")
			     (sexp :tag "Value"))))
	     ;; indent.c
	     (indent-tabs-mode fill boolean)
	     ;; keyboard.c
	     (meta-prefix-char keyboard character)
	     (auto-save-interval auto-save integer)
	     (echo-keystrokes minibuffer boolean)
	     (help-char keyboard character)
	     ;; lread.c
	     (load-path environment 
			(repeat (choice :tag "Directory"
					(const :tag "default" nil)
					(directory :format "%v"))))
	     ;; process.c
	     (delete-exited-processes proces-basics boolean)
	     ;; syntax.c
	     (parse-sexp-ignore-comments editing-basics boolean)
	     (words-include-escapes editing-basics boolean)
	     ;; window.c
	     (temp-buffer-show-function windows function)
	     (next-screen-context-lines windows boolean)
	     (window-min-height windows integer)
	     (window-min-width windows integer)
	     ;; xdisp.c
	     (scroll-step windows integer)
	     (truncate-partial-width-windows display boolean)
	     ;; xfns.c
	     (x-bitmap-file-path installation
				 (repeat (directory :format "%v")))))
      this symbol group type)
  (while all 
    (setq this (car all)
	  all (cdr all)
	  symbol (nth 0 this)
	  group (nth 1 this)
	  type (nth 2 this))
    (if (not (boundp symbol))
	;; This is loaded so early, there is no message
	(if (fboundp 'message)
	    ;; If variables are removed from C code, give an error here!
	    (message "Intrinsic `%S' not bound" symbol))
      ;; This is called before any user can have changed the value.
      (put symbol 'factory-value 
	   (list (custom-start-quote (default-value symbol))))
      ;; Add it to the right group.
      (custom-add-to-group group symbol 'custom-variable)
      ;; Set the type.
      (put symbol 'custom-type type))))

;; This is to prevent it from being reloaded by `cus-load.el'.
(provide 'cus-start)

;;; cus-start.el ends here.
