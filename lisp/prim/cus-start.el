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
(let ((all '(;; boolean
	     (abbrev-all-caps abbrev boolean)
	     (allow-deletion-of-last-visible-frame frames boolean)
	     (debug-on-quit debug boolean)
	     (delete-auto-save-files auto-save boolean)
	     (delete-exited-processes processes-basics boolean)
	     (indent-tabs-mode editing-basics boolean)
	     (load-ignore-elc-files maint boolean)
	     (load-warn-when-source-newer maint boolean)
	     (load-warn-when-source-only maint boolean)
	     (modifier-keys-are-sticky keyboard boolean)
	     (no-redraw-on-reenter display boolean)
	     (scroll-on-clipped-lines display boolean)
	     (truncate-partial-width-windows display boolean)
	     (visible-bell sound boolean)
	     (x-allow-sendevents x boolean)
	     (zmacs-regions editing-basics boolean)
	     ;; integer
	     (auto-save-interval auto-save integer)
	     (bell-volume sound integer)
	     (echo-keystrokes keyboard integer)
	     (gc-cons-threshold alloc integer)
	     (next-screen-context-lines display integer)
	     (scroll-step windows integer)
	     (window-min-height windows integer)
	     (window-min-width windows integer)
	     ;; object
	     (auto-save-file-format auto-save
				    (choice (const :tag "Normal" t)
					    (repeat (symbol :tag "Format"))))
	     (completion-ignored-extensions minibuffer
					    (repeat
					     (string :format "%v")))
	     (debug-on-error debug  (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (debug-on-signal debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (exec-path processes-basics (repeat
					  (choice :tag "Directory"
						  (const :tag "Default" nil)
						  (directory :format "%v"))))
	     (file-name-handler-alist data (repeat
					    (cons regexp
						  (function :tag "Handler"))))
	     (shell-file-name execute file)
	     (stack-trace-on-error debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (stack-trace-on-signal debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     ;; buffer-local
	     (case-fold-search matching boolean)
	     (ctl-arrow display (choice (integer 160)
					(sexp :tag "160 (default)"
					      :format "%t\n")))
	     (fill-column fill integer)
	     (left-margin fill integer)
	     (tab-width editing-basics integer)
	     (truncate-lines display boolean)
	     ;; not documented as user-options, but should still be
	     ;; customizable:
	     (default-frame-plist frames (repeat
					  (list :inline t
						:format "%v"
						(symbol :tag "Parameter")
						(sexp :tag "Value"))))
	     (help-char keyboard character)
	     (max-lisp-eval-depth limits integer)
	     (max-specpdl-size limits integer)
	     (meta-prefix-char keyboard character)
	     (parse-sexp-ignore-comments editing-basics boolean)
	     (selective-display display 
				(choice (const :tag "off" nil)
					(integer :tag "space"
						 :format "%v"
						 1)
					(const :tag "on" t)))
	     (selective-display-ellipses display boolean)
	     (signal-error-on-buffer-boundary internal boolean)
	     (words-include-escapes editing-basics boolean)
	     (temp-buffer-show-function
	      windows (radio (function-item :tag "Temp Buffers Always in Same Frame"
					    :format "%t\n"
					    show-temp-buffer-in-current-frame)
			     (const :tag "Temp Buffers Like Other Buffers" nil)
			     (function :tag "Other")))))
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
      (put symbol 'standard-value 
	   (list (custom-start-quote (default-value symbol))))
      ;; Add it to the right group.
      (custom-add-to-group group symbol 'custom-variable)
      ;; Set the type.
      (put symbol 'custom-type type))))

;; This is to prevent it from being reloaded by `cus-load.el'.
(provide 'cus-start)

;;; cus-start.el ends here.
