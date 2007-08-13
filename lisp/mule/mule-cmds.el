;;; mule-cmds.el --- Commands for multilingual environment

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: mule, multilingual

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

;;; Code:

;;; MULE related key bindings and menus.

(defvar mule-keymap (make-sparse-keymap "MULE")
  "Keymap for MULE (Multilingual environment) specific commands.")
(fset 'mule-prefix mule-keymap)

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" 'mule-prefix)

(define-key mule-keymap "f" 'set-buffer-file-coding-system)
(define-key mule-keymap "F" 'set-default-buffer-file-coding-system) ; XEmacs
(define-key mule-keymap "t" 'set-terminal-coding-system)
(define-key mule-keymap "k" 'set-keyboard-coding-system)
(define-key mule-keymap "p" 'set-current-process-coding-system)
(define-key mule-keymap "P" 'set-default-process-coding-system) ; XEmacs
(define-key mule-keymap "\C-\\" 'select-input-method)
(define-key mule-keymap "c" 'list-coding-system-briefly) ; XEmacs
(define-key mule-keymap "C" 'list-coding-system)	 ; XEmacs
(define-key mule-keymap "r" 'toggle-display-direction)	 ; XEmacs
(define-key mule-keymap "l" 'set-language-environment)

(define-key help-map "\C-L" 'describe-language-support)
(define-key help-map "L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "C" 'describe-current-coding-system)
(define-key help-map "h" 'view-hello-file)

;; Menu for XEmacs were moved to x11/x-menubar.el.


;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

(defun view-hello-file ()
  "Display the HELLO file which list up many languages and characters."
  (interactive)
  ;; We have to decode the file in any environment.
  (let ((coding-system-for-read 'iso-2022-7))
    (find-file-read-only (expand-file-name "HELLO" data-directory))))


;;; Language support staffs.

(defvar primary-language "English"
  "Name of a user's primary language.
Emacs provide various language supports based on this variable.")

(defvar language-info-alist nil
  "Alist of language names vs the corresponding information of various kind.
Each element looks like:
	(LANGUAGE-NAME . ((KEY . INFO) ...))
where LANGUAGE-NAME is a string,
KEY is a symbol denoting the kind of information,
INFO is any Lisp object which contains the actual information related
to KEY.")

(defun get-language-info (language-name key)
  "Return the information for LANGUAGE-NAME of the kind KEY.
KEY is a symbol denoting the kind of required information."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((lang-slot (assoc-ignore-case language-name language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (language-name key info)
  "Set for LANGUAGE-NAME the information INFO under KEY.
KEY is a symbol denoting the kind of information.
INFO is any Lisp object which contains the actual information.

Currently, the following KEYs are used by Emacs:

charset: list of symbols whose values are charsets specific to the language.

coding-system: list of coding systems specific to the langauge.

tutorial: a tutorial file name written in the language.

sample-text: one line short text containing characters of the language.

documentation: t or a string describing how Emacs supports the language.
      If a string is specified, it is shown before any other information
      of the language by the command `describe-language-environment'.

setup-function: a function to call for setting up environment
       convenient for a user of the language.

If KEY is documentation or setup-function, you can also specify
a cons cell as INFO, in which case, the car part should be
a normal value as INFO for KEY (as described above),
and the cdr part should be a symbol whose value is a menu keymap
in which an entry for the language is defined.  But, only the car part
is actually set as the information.

We will define more KEYs in the future.  To avoid conflict,
if you want to use your own KEY values, make them start with `user-'."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let (lang-slot key-slot)
    (setq lang-slot (assoc language-name language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list language-name)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    ;; Setup menu.
    (cond ((eq key 'documentation)
           ;; (define-key-after mule-describe-language-support-map
           ;;   (vector (intern language-name))
           ;;   (cons language-name info)
           ;;   t)
	   (eval-after-load "x-menubar"
	     `(add-menu-button '("Mule" "Describe Language Support")
			       (vector ,language-name ',info t)))
	   )
	  ((eq key 'setup-function)
	   ;; (define-key-after mule-set-language-environment-map
           ;;   (vector (intern language-name))
           ;;   (cons language-name info)
           ;;   t)
	   (eval-after-load "x-menubar"
	     `(add-menu-button '("Mule" "Set Language Environment")
			       (vector ,language-name ',info t)))
           ))
    ))

(defun set-language-info-alist (language-name alist)
  "Set for LANGUAGE-NAME the information in ALIST.
ALIST is an alist of KEY and INFO.  See the documentation of
`set-langauge-info' for the meanings of KEY and INFO."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (while alist
    (set-language-info language-name (car (car alist)) (cdr (car alist)))
    (setq alist (cdr alist))))

(defun read-language-name (key prompt &optional default)
  "Read language name which has information for KEY, prompting with PROMPT.
DEFAULT is the default choice of language.
This returns a language name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(function (lambda (elm) (assq key elm)))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (get-language-info name key))
	name)))

;;; Multilingual input methods.

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defvar leim-list-header (format "\
;;; %s -- list of LEIM (Library of Emacs Input Method)
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; in the same directory as this file.  Loading this file registeres
;; the whole input methods in Emacs.
;;
;; Each entry has the form:
;;   (register-input-method
;;    INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC
;;    TITLE DESCRIPTION
;;    ARG ...)
;; See the function `register-input-method' for the meanings of arguments.
;;
;; If this directory is included in load-path, Emacs automatically
;; loads this file at startup time.

"
				 leim-list-file-name)
  "Header to be inserted in LEIM list file.")

(defvar leim-list-entry-regexp "^(register-input-method"
  "Regexp matching head of each entry in LEIM list file.
See also the variable `leim-list-header'")

(defvar update-leim-list-functions
  '(quail-update-leim-list-file)
  "List of functions to call to update LEIM list file.
Each function is called with one arg, LEIM directory name.")

(defun update-leim-list-file (&rest dirs)
  "Update LEIM list file in directories DIRS."
  (let ((functions update-leim-list-functions))
    (while functions
      (apply (car functions) dirs)
      (setq functions (cdr functions)))))

(defvar current-input-method nil
  "The current input method for multilingual text.
If nil, that means no input method is activated now.")
(make-variable-buffer-local 'current-input-method)
(put 'current-input-method 'permanent-local t)

(defvar current-input-method-title nil
  "Title string of the current input method shown in mode line.")
(make-variable-buffer-local 'current-input-method-title)
(put 'current-input-method-title 'permanent-local t)

(defcustom default-input-method nil
  "*Default input method for multilingual text.
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :group 'mule)

(defvar input-method-history nil
  "History list for some commands that read input methods.")
(make-variable-buffer-local 'input-method-history)
(put 'input-method-history 'permanent-local t)

(defvar inactivate-current-input-method-function nil
  "Function to call for inactivating the current input method.
Every input method should set this to an appropriate value when activated.
This function is called with no argument.

This function should never change the value of `current-input-method'.
It is set to nil by the function `inactivate-input-method'.")
(make-variable-buffer-local 'inactivate-current-input-method-function)
(put 'inactivate-current-input-method-function 'permanent-local t)

(defvar describe-current-input-method-function nil
  "Function to call for describing the current input method.
This function is called with no argument.")
(make-variable-buffer-local 'describe-current-input-method-function)
(put 'describe-current-input-method-function 'permanent-local t)

(defvar input-method-alist nil
  "Alist of input method names vs the corresponding information to use it.
Each element has the form:
	(INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC TITLE DESCRIPTION ...)
See the function `register-input-method' for the meanings of each elements.")

(defun register-input-method (input-method language-name &rest args)
  "Register INPUT-METHOD as an input method for LANGUAGE-NAME.
INPUT-METHOD and LANGUAGE-NAME are symbols or strings.
The remaining arguments are:
	ACTIVATE-FUNC, TITLE, DESCRIPTION, and ARG ...
 where,
ACTIVATE-FUNC is a function to call for activating this method.
TITLE is a string shown in mode-line while this method is active,
DESCRIPTION is a string describing about this method,
Arguments to ACTIVATE-FUNC are INPUT-METHOD and ARGs."
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (let ((info (cons language-name args))
	(slot (assoc input-method input-method-alist)))
    (if slot
	(setcdr slot info)
      (setq slot (cons input-method info))
      (setq input-method-alist (cons slot input-method-alist)))))

(defun read-input-method-name (prompt &optional default inhibit-null)
  "Read a name of input method from a minibuffer prompting with PROMPT.
If DEFAULT is non-nil, use that as the default,
  and substitute it into PROMPT at the first `%s'.
If INHIBIT-NULL is non-nil, null input signals an error.

The return value is a string."
  (if default
      (setq prompt (format prompt default)))
  (let* ((completion-ignore-case t)
	 ;; This binding is necessary because input-method-history is
	 ;; buffer local.
	 (input-method (completing-read prompt input-method-alist
					nil t nil 'input-method-history)
		       ;;default)
		       ))
    (if (> (length input-method) 0)
	input-method
      ;; If we have a default, use it, otherwise check inhibit-null
      (if default
	  default
	(if inhibit-null
	    (error "No valid input method is specified"))))))

(defun activate-input-method (input-method)
  "Turn INPUT-METHOD on.
If some input method is already on, turn it off at first."
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
    (inactivate-input-method))
  (unless current-input-method
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (apply (nth 2 slot) input-method (nthcdr 5 slot))
      (setq current-input-method input-method)
      (setq current-input-method-title (nth 3 slot))
      (run-hooks 'input-method-activate-hook))))

(defun inactivate-input-method ()
  "Turn off the current input method."
  (when current-input-method
    (if input-method-history
	(unless (string= current-input-method (car input-method-history))
	  (setq input-method-history
		(cons current-input-method
		      (delete current-input-method input-method-history))))
      (setq input-method-history (list current-input-method)))
    (unwind-protect
	(funcall inactivate-current-input-method-function)
      (unwind-protect
	  (run-hooks 'input-method-inactivate-hook)
	(setq current-input-method nil
	      current-input-method-title nil)))))

(defun select-input-method (input-method)
  "Select and turn on INPUT-METHOD.
This sets the default input method to what you specify,
and turn it on for the current buffer."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (activate-input-method input-method)
  (setq default-input-method input-method))

(defun toggle-input-method (&optional arg)
  "Turn on or off a multilingual text input method for the current buffer.

With arg, read an input method from minibuffer and turn it on.

Without arg, if some input method is currently activated, turn it off,
else turn on an input method selected last time
or the default input method (see `default-input-method').

When there's no input method to turn on, turn on what read from minibuffer."
  (interactive "P")
  (if (eq arg 1)
      (setq arg nil))
  (let* ((default (or (car input-method-history) default-input-method)))
    (if (and current-input-method (not arg))
	(inactivate-input-method)
      (activate-input-method
       (if (or arg (not default))
	   (read-input-method-name
	    (if default "Input method (default %s): " "Input method: " )
	    default t)  
	 default))
      (or default-input-method
	  (setq default-input-method current-input-method)))))

(defun describe-input-method (input-method)
  "Describe  input method INPUT-METHOD."
  (interactive
   (list (read-input-method-name
	  "Describe input method (default, current choice): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (null input-method)
      (describe-current-input-method)
    (with-output-to-temp-buffer "*Help*"
      (let ((elt (assoc input-method input-method-alist)))
	(princ (format "Input method: %s (`%s' in mode line) for %s\n  %s\n"
		       input-method (nth 3 elt) (nth 1 elt) (nth 4 elt)))))))

(defun describe-current-input-method ()
  "Describe the input method currently in use."
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 (cdr current-input-method))
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input
					input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
initially.
Optional 3rd argument INPUT-METHOD specifies the input method
to be activated instead of the one selected last time.  It is a symbol
or a string."
  (setq input-method
	(or input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current-input-method input-method))
    (read-string prompt initial-input nil nil t)))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag t
  "*If this flag is non-nil, input methods give extra guidance.

The extra guidance is done by showing list of available keys in echo
area.

For complex input methods such as `chinese-py' and `japanese',
when you use the input method in the minibuffer, the guidance is
shown at the bottom short window (split from the existing window).
For simple input methods, guidance is not shown
when you are in the minibuffer."
  :type 'boolean
  :group 'mule)

(defcustom input-method-highlight-flag t
  "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence."
  :type 'boolean
  :group 'mule)

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inacitvated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behaviour of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporalily.  After the key is handled, the input method is 
back on.
But, if this flag is non-nil, the input method is never back on.")


;;; Language specific setup functions.
;; (defun set-language-environment (language-name)
;;   "Setup a user's environment for LANGUAGE-NAME.
;; 
;; To setup, a fucntion returned by:
;;   (get-language-info LANGUAGE-NAME 'setup-function)
;; is called."
;;   (interactive (list (read-language-name 'setup-function "Language: ")))
;;   (let (func)
;;     (if (or (null language-name)
;;             (null (setq func
;;                         (get-language-info language-name 'setup-function))))
;;         (error "No way to setup environment for the specified language"))
;;     (funcall func)))

;; Print all arguments with `princ', then print "\n".
(defsubst princ-list (&rest args)
  (while args (princ (car args)) (setq args (cdr args)))
  (princ "\n"))

(defun describe-language-support (language-name)
  "Describe how Emacs supports LANGUAGE-NAME.

For that, a function returned by:
  (get-language-info LANGUAGE-NAME 'describe-function)
is called."
  (interactive (list (read-language-name 'documentation "Language: ")))
  (let (func)
    (if (or (null language-name)
	    (null (setq func
			(get-language-info language-name 'describe-function))))
	(error "No documentation for the specified language"))
    (funcall func)))

;; Print LANGUAGE-NAME specific information such as input methods,
;; charsets, and coding systems.  This function is intended to be
;; called from various describe-LANGUAGE-support functions defined in
;; lisp/language/LANGUAGE.el.
(defun describe-language-support-internal (language-name)
  (with-output-to-temp-buffer "*Help*"
    (let ((doc (get-language-info language-name 'documentation)))
      (if (stringp doc)
	  (princ-list doc)))
    (princ "-----------------------------------------------------------\n")
    (princ-list "List of items specific to "
		language-name
		" support")
    (princ "-----------------------------------------------------------\n")
    (let ((str (get-language-info language-name 'sample-text)))
      (if (stringp str)
	  (progn
	    (princ "<sample text>\n")
	    (princ-list "  " str))))
    (princ "<input methods>\n")
    (let ((l (get-language-info language-name 'input-method)))
      (while l
	(princ-list "  " (car (car l)))
	(setq l (cdr l))))
    (princ "<character sets>\n")
    (let ((l (get-language-info language-name 'charset)))
      (if (null l)
	  (princ-list "  nothing specific to " language-name)
	(while l
	  (princ-list "  " (car l) ": "
		      (charset-description (car l)))
	  (setq l (cdr l)))))
    (princ "<coding systems>\n")
    (let ((l (get-language-info language-name 'coding-system)))
      (if (null l)
	  (princ-list "  nothing specific to " language-name)
	(while l
	  (princ-list "  " (car l) ":\n\t"
		      (coding-system-docstring (car l)))
	  (setq l (cdr l)))))))

;;; Charset property

;; (defsubst get-charset-property (charset propname)
;;   "Return the value of CHARSET's PROPNAME property.
;; This is the last value stored with
;; `(put-charset-property CHARSET PROPNAME VALUE)'."
;;   (plist-get (charset-plist charset) propname))

;; (defsubst put-charset-property (charset propname value)
;;   "Store CHARSETS's PROPNAME property with value VALUE.
;; It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
;;   (set-charset-plist charset
;;                      (plist-put (charset-plist charset) propname value)))

;;; Character code property
;; (put 'char-code-property-table 'char-table-extra-slots 0)

;; (defvar char-code-property-table
;;   (make-char-table 'char-code-property-table)
;;   "Char-table containing a property list of each character code.
;; 
;; See also the documentation of `get-char-code-property' and
;; `put-char-code-property'")

;; (defun get-char-code-property (char propname)
;;   "Return the value of CHAR's PROPNAME property in `char-code-property-table'."
;;   (let ((plist (aref char-code-property-table char)))
;;     (if (listp plist)
;;         (car (cdr (memq propname plist))))))

;; (defun put-char-code-property (char propname value)
;;   "Store CHAR's PROPNAME property with VALUE in `char-code-property-table'.
;; It can be retrieved with `(get-char-code-property CHAR PROPNAME)'."
;;   (let ((plist (aref char-code-property-table char)))
;;     (if plist
;;         (let ((slot (memq propname plist)))
;;           (if slot
;;               (setcar (cdr slot) value)
;;             (nconc plist (list propname value))))
;;       (aset char-code-property-table char (list propname value)))))

;;; mule-cmds.el ends here
