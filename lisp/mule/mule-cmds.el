;;; mule-cmds.el --- Commands for multilingual environment -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

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

;; Note: Some of the code here is now in code-cmds.el

;;; Code:

;;; MULE related key bindings and menus.

;; Preserve the old name
(defvaralias 'mule-keymap 'coding-keymap)

(define-key mule-keymap "x" 'set-selection-coding-system)
(define-key mule-keymap "X" 'set-next-selection-coding-system)
(define-key mule-keymap "\C-\\" 'set-input-method)
;;(define-key mule-keymap "c" 'list-coding-system-briefly) ; XEmacs
(define-key mule-keymap "C" 'describe-coding-system)	 ; XEmacs
(define-key mule-keymap "r" 'toggle-display-direction)	 ; XEmacs
(define-key mule-keymap "l" 'set-language-environment)

(define-key help-map "L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "h" 'view-hello-file)

;; Menu for XEmacs were moved to menubar-items.el.


;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

;; Original mapping will be altered by set-keyboard-coding-system.
(define-key global-map [(meta \#)] 'ispell-word)	;originally "$"
;; (define-key global-map [(meta {)] 'insert-parentheses) ;originally "("

;;; This is no good because people often type Shift-SPC
;;; meaning to type SPC.  -- rms.
;;; ;; Here's an alternative key binding for X users (Shift-SPACE).
;;; (define-key global-map '(shift space) 'toggle-input-method)

(defun view-hello-file ()
  "Display the HELLO file which list up many languages and characters."
  (interactive)
  ;; We have to decode the file in any environment.
  (let ((coding-system-for-read 'iso-2022-7bit))
    (find-file-read-only (expand-file-name "HELLO" data-directory))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Language Support Functions                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar language-info-alist nil
  "Alist of language environment definitions.
Each element looks like:
	(LANGUAGE-NAME . ((PROP . VALUE) ...))
where LANGUAGE-NAME is a string, the name of the language environment,
PROP is a symbol denoting a property, and VALUE is the data associated
with PROP.
See `set-language-info' for documentation on PROP and VALUE.")

(defun get-language-info (lang-env prop)
  "Return information listed under PROP for language environment LANG-ENV.
PROP is a symbol denoting a property.
For a list of useful values for PROP and their meanings,
see `set-language-info'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((lang-slot (assoc-ignore-case lang-env language-info-alist)))
    (if lang-slot
	(cdr (assq prop (cdr lang-slot))))))

(defun set-language-info (lang-env prop value)
  "Modify part of the definition of language environment LANG-ENV.
Specifically, this stores the information VALUE under PROP
in the definition of this language environment.
PROP is a symbol denoting a property, and VALUE is the value of that property.

Meaningful values for PROP include

  documentation      VALUE is documentation of what this language environment
                     is meant for, and how to use it.

  charset            VALUE is a list of the character sets used by this
                     language environment.

  sample-text        VALUE is one line of text,
                     written using those character sets,
                     appropriate for this language environment.

  setup-function     VALUE is a function to call to switch to this
                     language environment.

  exit-function      VALUE is a function to call to leave this
                     language environment.

  coding-system      VALUE is a list of coding systems that are good
                     for saving text written in this language environment.
                     This list serves as suggestions to the user;
                     in effect, as a kind of documentation.

  coding-priority    VALUE is a list of coding systems for this language
                     environment, in order of decreasing priority.
                     This is used to set up the coding system priority
                     list when you switch to this language environment.

  input-method       VALUE is a default input method for this language
                     environment.

  features           VALUE is a list of features requested in this
                     language environment.

  tutorial           VALUE is a tutorial file name written in the language.

  locale             VALUE is a list of locale expressions, which serve
                     two purposes: (1) Determining the language
                     environment from the current system locale at
                     startup, and (2) determining how to set the system
                     locale when the language environment is changed.
                     Each expression will be tried in turn, and should
                     be a string (for case (1), the string is matched
                     against the current locale using the regular
                     expression \"^STRING[^A-Za-z0-9]\"; for case (2),
                     the string is passed directly to
                     `set-current-locale' until a non-nil result is
                     returned), or a function of one argument.  For
                     case (1), this argument will be a locale, and the
                     function should return t or nil to indicate
                     whether this locale matches the language
                     environment; for case (2), the argument will be
                     nil, and the function should call
                     `set-current-locale' itself and return the set
                     locale string if the locale was successfully set,
                     and nil otherwise.

                     NOTE: This property is *NOT* used under MS Windows;
                     instead, the `mswindows-locale' property is used.

  cygwin-locale      VALUE specifies a general Unix-style C library
                     locale that will be used to initialize the LANG
                     environment variable under MS Windows native, when the
                     system cannot test out the locales specified in the
                     `locale' property.  This is so that Cygwin programs
                     can be run from an MS Windows native XEmacs.  If not
                     specified, the last entry in `locale' will be used.

  native-coding-system   VALUE is a single coding-system expression, or a
                     list of such expressions.  These expressions are
                     used to compute the operating system's native
                     coding system, i.e. the coding system to be used
                     as the alias for `native' and `file-name'.  This
                     specifies the coding system used for text
                     exchanged with the operating system, such as file
                     names, environment variables, subprocess
                     arguments, etc.  Each expression should be either
                     a symbol naming a coding system or a function
                     (anything that is `functionp') of one argument,
                     which is passed the current locale corresponding
                     to this language environment and should return a
                     coding system or nil.  Each expression is tried in
                     turn until a coding system is obtained.  If there
                     is no non-nil result, or no value is specified for
                     this property, the first coding system listed
                     under the `coding-system' property is used.

                     NOTE: This is *NOT* used under MS Windows.
                     Instead, `mswindows-multibyte-system-default'
                     is always used, since the system default code
                     page is what the Win32 API routines make use
                     of, and this cannot be changed. (We get around
                     this by using the Unicode versions whenever
                     possible -- i.e. on Windows NT/2000, and on
                     Windows 9x with the few API's that support
                     Unicode.)

  mswindows-locale   VALUE is an element of the form MSWINDOWS-LOCALE, or
                     a list of such elements.  Each element is an MS
                     Windows locale, of the form that can be passed to
                     `mswindows-set-current-locale'.  This property is used
                     both to determine the current language environment at
                     startup (by matching MSWINDOWS-LOCALE against the
                     value returned by `mswindows-user-default-locale') and
                     to set the values of `set-current-locale' and
                     `mswindows-set-current-locale' when the current
                     language environment is changed. (The correct CLIB
                     locale can always be generated by passing in the
                     SUBLANG, with dashes in place of underscores, or the
                     LANG if there's no SUBLANG.  The return value will be
                     the canonicalized locale, in proper CLIB form.)

                     If there is no value for this property, the MS Windows
                     locale is assumed to have the same name as the
                     language environment."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let (lang-slot prop-slot)
    (setq lang-slot (assoc lang-env language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list lang-env)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq prop-slot (assq prop lang-slot))
    (if (null prop-slot)		; If no slot for the prop, add it.
	(progn
	  (setq prop-slot (list prop))
	  (setcdr lang-slot (cons prop-slot (cdr lang-slot)))))
    (setcdr prop-slot value)))

(defun set-language-info-alist (lang-env alist &optional parents)
  "Store ALIST as the definition of language environment LANG-ENV.
ALIST is an alist of properties and values.  See the documentation of
`set-language-info' for the allowed properties."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  ;; FSF has 30 lines of unbelievably ugly code to set up the menus
  ;; appropriately.  We just use a filter.
  (while alist
    (set-language-info lang-env (car (car alist)) (cdr (car alist)))
    (setq alist (cdr alist)))
  lang-env)

(defun read-language-name (key prompt &optional default)
  "Read a language environment name which has information for KEY.
If KEY is nil, read any language environment.
Prompt with PROMPT.  DEFAULT is the default choice of language environment.
This returns a language environment name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(and key
				     (function (lambda (elm) (assq key elm))))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (or (not key)
		 (get-language-info name key)))
	name)))

;;; Multilingual input methods.

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defvar leim-list-header (format 
";;; %s -- list of LEIM (Library of Emacs Input Method)
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; in the same directory as this file.  Loading this file registers
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
  "*Default input method for multilingual text (a string).
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :group 'mule
  :type '(choice (const nil) string))

(put 'input-method-function 'permanent-local t)

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
  "Alist mapping input method names to information used by the LEIM API.
Elements have the form (METHOD LANGUAGE ACTIVATOR TITLE DESCRIPTION ARGS...).
Use `register-input-method' to add input methods to the database.  See its
documentation for the meanings of the elements.")

(defun register-input-method (method language
			      ;; #### shouldn't be optional, but need to
			      ;; audit callers
			      &optional activator title description
			      &rest args)
  "Register METHOD as an input method for language environment LANGUAGE.

METHOD and LANGUAGE may be symbols or strings.
ACTIVATOR is the function called to activate this method.  METHOD (the
  invocation name) and ARGS are passed to the function on activation.
TITLE is a string to show in the mode line when this method is active.
DESCRIPTION is a string describing this method and what it is good for.
Optional ARGS, if any, are stored and passed as arguments to ACTIVATOR.

When registering a new Quail input method, the input method title should be
the one given in the third parameter of `quail-define-package' (if the values
are different, the string specified in this function takes precedence).

The information provided is registered in `input-method-alist'.  The commands
`describe-input-method' and `list-input-methods' use this database to show
information about input methods without loading them."
  (if (symbolp language)
      (setq language (symbol-name language)))
  (if (symbolp method)
      (setq method (symbol-name method)))
  (let ((info (append (list language activator title description) args))
	(slot (assoc method input-method-alist)))
    (if slot
	(setcdr slot info)
      (setq slot (cons method info))
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
					nil t nil 'input-method-history
					default)))
    (if (and input-method (symbolp input-method))
	(setq input-method (symbol-name input-method)))
    (if (> (length input-method) 0)
	input-method
      (if inhibit-null
	  (error "No valid input method is specified")))))

(defun activate-input-method (input-method)
  "Switch to input method INPUT-METHOD for the current buffer.
If some other input method is already active, turn it off first.
If INPUT-METHOD is nil, deactivate any current input method."
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
      (inactivate-input-method))
  (unless (or current-input-method (null input-method))
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (let ((func (nth 2 slot)))
	(if (functionp func)
	    (apply (nth 2 slot) input-method (nthcdr 5 slot))
	  (if (and (consp func) (symbolp (car func)) (symbolp (cdr func)))
	      (progn
		(require (cdr func))
		(apply (car func) input-method (nthcdr 5 slot)))
	    (error "Can't activate input method `%s'" input-method))))
      (setq current-input-method input-method)
      (setq current-input-method-title (nth 3 slot))
      (unwind-protect
	  (run-hooks 'input-method-activate-hook)
	(force-mode-line-update)))))

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
	      current-input-method-title nil)
	(force-mode-line-update)))))

(defun set-input-method (input-method)
  "Select and activate input method INPUT-METHOD for the current buffer.
This also sets the default input method to the one you specify."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (activate-input-method input-method)
  (setq default-input-method input-method))

(defun toggle-input-method (&optional arg)
  "Turn on or off a multilingual text input method for the current buffer.

With no prefix argument, if an input method is currently activated,
turn it off.  Otherwise, activate an input method -- the one most
recently used, or the one specified in `default-input-method', or
the one read from the minibuffer.

With a prefix argument, read an input method from the minibuffer and
turn it on.

The default is to use the most recent input method specified
\(not including the currently active input method, if any)."
  (interactive "P")
  (if (and current-input-method (not arg))
      (inactivate-input-method)
    (let ((default (or (car input-method-history) default-input-method)))
      (if (and arg default (equal current-input-method default)
	       (> (length input-method-history) 1))
	  (setq default (nth 1 input-method-history)))
      (activate-input-method
       (if (or arg (not default))
	   (progn
	     (read-input-method-name
	      (if default "Input method (default %s): " "Input method: " )
	      default t))
	 default))
      (or default-input-method
	  (setq default-input-method current-input-method)))))

(defun describe-input-method (input-method)
  "Describe input method INPUT-METHOD."
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
		 current-input-method)
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
initially.
Optional 3rd argument INPUT-METHOD specifies the input method
to be activated instead of the one selected last time.  It is a symbol
or a string."
  (setq input-method
	(or input-method
	    current-input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((prev-input-method current-input-method))
    (unwind-protect
	(progn
	  (activate-input-method input-method)
	  ;; FSF Emacs
	  ;; (read-string prompt initial-input nil nil t)
	  (read-string prompt initial-input nil))
      (activate-input-method prev-input-method))))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag 'default
  "*A flag to control extra guidance given by input methods.
The value should be nil, t, `complex-only', or `default'.

The extra guidance is done by showing list of available keys in echo
area.  When you use the input method in the minibuffer, the guidance
is shown at the bottom short window (split from the existing window).

If the value is t, extra guidance is always given, if the value is
nil, extra guidance is always suppressed.

If the value is `complex-only', only complex input methods such as
`chinese-py' and `japanese' give extra guidance.

If the value is `default', complex input methods always give extra
guidance, but simple input methods give it only when you are not in
the minibuffer.

See also the variable `input-method-highlight-flag'."
  :type '(choice (const t) (const nil) (const complex-only) (const default))
  :group 'mule)

(defcustom input-method-highlight-flag t
  "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence.
See also the variable `input-method-verbose-flag'."
  :type 'boolean
  :group 'mule)

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inactivated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")

(defvar input-method-exit-on-first-char nil
  "This flag controls a timing when an input method returns.
Usually, the input method does not return while there's a possibility
that it may find a different translation if a user types another key.
But, it this flag is non-nil, the input method returns as soon as
the current key sequence gets long enough to have some valid translation.")

(defvar input-method-use-echo-area nil
  "This flag controls how an input method shows an intermediate key sequence.
Usually, the input method inserts the intermediate key sequence,
or candidate translations corresponding to the sequence,
at point in the current buffer.
But, if this flag is non-nil, it displays them in echo area instead.")

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behavior of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporarily.  After that key, the input method is re-enabled.
But, if this flag is non-nil, the input method is never back on.")


(defvar set-language-environment-hook nil
  "Normal hook run after some language environment is set.

When you set some hook function here, that effect usually should not
be inherited to another language environment.  So, you had better set
another function in `exit-language-environment-hook' (which see) to
cancel the effect.")

(defvar exit-language-environment-hook nil
  "Normal hook run after exiting from some language environment.
When this hook is run, the variable `current-language-environment'
is still bound to the language environment being exited.

This hook is mainly used for canceling the effect of
`set-language-environment-hook' (which-see).")

;; bogus FSF function setup-specified-language-support.

(defcustom current-language-environment "English"
  "The last language environment specified with `set-language-environment'.
This variable should be set only with \\[customize], which is equivalent
to using the function `set-language-environment'."
  :link '(custom-manual "(emacs)Language Environments")
  :set (lambda (symbol value) (set-language-environment value))
  :get (lambda (x)
	 (or (car-safe (assoc-ignore-case
			(if (symbolp current-language-environment)
			    (symbol-name current-language-environment)
			  current-language-environment)
			language-info-alist))
	     "English"))
  :type (cons 'choice (mapcar (lambda (lang)
				(list 'const (car lang)))
			      language-info-alist))
  :initialize 'custom-initialize-default
  :group 'mule
  :type 'string)

(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system autodetection priority, the default buffer
coding system, the default input method, the system locale, and other
relevant language properties.  LANGUAGE-NAME should be a string, the
name of a language environment.  For example, \"Latin-1\" specifies
the language environment for the major languages of Western Europe."
  (interactive (list (read-language-name
		      nil
		      "Set language environment (default, English): ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (or (assoc-ignore-case language-name language-info-alist)
      (error 'invalid-argument "Language environment not defined"
	     language-name))
  (if current-language-environment
      (let ((func (get-language-info current-language-environment
				     'exit-function)))
	(run-hooks 'exit-language-environment-hook)
	(if (fboundp func) (funcall func))))
  (setq current-language-environment language-name)
  (let ((default-eol-type (coding-system-eol-type
			   default-buffer-file-coding-system)))
    (reset-coding-categories-to-default)
    (set-locale-for-language-environment language-name)
    (set-language-environment-coding-systems language-name default-eol-type))

  (finish-set-language-environment language-name))

(defun finish-set-language-environment (language-name)
  ;; Internal function.  Only what's here is called at startup, once the
  ;; first language environment is determined.  The above stuff was already
  ;; taken care of very early in the startup sequence, in a special
  ;; fashion.
  (let ((input-method (get-language-info language-name 'input-method)))
    (when input-method
      (setq default-input-method input-method)
      (if input-method-history
	  (setq input-method-history
		(cons input-method
		      (delete input-method input-method-history))))))
  ;; (let ((nonascii (get-language-info language-name 'nonascii-translation))
  ;;       (dos-table
  ;;        (if (eq window-system 'pc)
  ;;            (intern
  ;;             (concat "cp" dos-codepage "-nonascii-translation-table")))))
  ;;   (cond
  ;;    ((char-table-p nonascii)
  ;;     (setq nonascii-translation-table nonascii))
  ;;    ((and (eq window-system 'pc) (boundp dos-table))
  ;;     ;; DOS terminals' default is to use a special non-ASCII translation
  ;;     ;; table as appropriate for the installed codepage.
  ;;     (setq nonascii-translation-table (symbol-value dos-table)))
  ;;    ((charsetp nonascii)
  ;;     (setq nonascii-insert-offset (- (make-char nonascii) 128)))))

  ;; (setq charset-origin-alist
  ;;       (get-language-info language-name 'charset-origin-alist))

  ;; Unibyte setups if necessary.
  ;; (unless default-enable-multibyte-characters
  ;;   ;; Syntax and case table.
  ;;   (let ((syntax (get-language-info language-name 'unibyte-syntax)))
  ;;     (if syntax
  ;;         (let ((set-case-syntax-set-multibyte nil))
  ;;           (load syntax nil t))
  ;;       ;; No information for syntax and case.  Reset to the defaults.
  ;;       (let ((syntax-table (standard-syntax-table))
  ;;             (case-table (standard-case-table))
  ;;             (ch (if (eq window-system 'pc) 128 160)))
  ;;         (while (< ch 256)
  ;;           (modify-syntax-entry ch " " syntax-table)
  ;;           (aset case-table ch ch)
  ;;           (setq ch (1+ ch)))
  ;;         (set-char-table-extra-slot case-table 0 nil)
  ;;         (set-char-table-extra-slot case-table 1 nil)
  ;;         (set-char-table-extra-slot case-table 2 nil))
  ;;       (set-standard-case-table (standard-case-table))
  ;;       (let ((list (buffer-list)))
  ;;         (while list
  ;;           (with-current-buffer (car list)
  ;;             (set-case-table (standard-case-table)))
  ;;           (setq list (cdr list))))))
  ;;   ;; Display table and coding system for terminal.
  ;;   (let ((coding (get-language-info language-name 'unibyte-display)))
  ;;     (if coding
  ;;         (standard-display-european-internal)
  ;;       (standard-display-default (if (eq window-system 'pc) 128 160) 255)
  ;;       (aset standard-display-table 146 nil))
  ;;     (or (eq window-system 'pc)
  ;;         (set-terminal-coding-system coding))))

  (let ((required-features (get-language-info language-name 'features)))
    (while required-features
      (require (car required-features))
      (setq required-features (cdr required-features))))
  (let ((func (get-language-info language-name 'setup-function)))
    (if (fboundp func)
	(funcall func)))

  ;; Fit the charsets preferences in unicode conversions for the
  ;; language environment.
  (set-language-unicode-precedence-list
   (get-language-info language-name 'charset))

  (run-hooks 'set-language-environment-hook)
  (force-mode-line-update t))

;; (defun standard-display-european-internal ()
;;   ;; Actually set up direct output of non-ASCII characters.
;;   (standard-display-8bit (if (eq window-system 'pc) 128 160) 255)
;;   ;; Unibyte Emacs on MS-DOS wants to display all 8-bit characters with
;;   ;; the native font, and codes 160 and 146 stand for something very
;;   ;; different there.
;;   (or (and (eq window-system 'pc) (not default-enable-multibyte-characters))
;;       (progn
;;         ;; Make non-line-break space display as a plain space.
;;         ;; Most X fonts do the wrong thing for code 160.
;;         (aset standard-display-table 160 [32])
;;         ;; Most Windows programs send out apostrophe's as \222.  Most X fonts
;;         ;; don't contain a character at that position.  Map it to the ASCII
;;         ;; apostrophe.
;;         (aset standard-display-table 146 [39]))))

;; bogus FSF function describe-specified-language-support.

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive
   (list (read-language-name
	  'documentation
	  "Describe language environment (default, current choice): ")))
  (if (null language-name)
      (setq language-name current-language-environment))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((doc (get-language-info language-name 'documentation)))
    (flet ((princ-list (&rest args)
		       (while args (princ (car args)) (setq args (cdr args)))
		       (princ "\n")))
      (with-output-to-temp-buffer "*Help*"
	(princ-list language-name " language environment" "\n")
	(if (stringp doc)
	    (progn
	      (princ-list doc)
	      (terpri)))
	(let ((str (get-language-info language-name 'sample-text)))
	  (if (stringp str)
	      (progn
		(princ "Sample text:\n")
		(princ-list "  " str)
		(terpri))))
	(let ((input-method (get-language-info language-name 'input-method))
	      (l (copy-sequence input-method-alist)))
	  (princ "Input methods")
	  (when input-method
	    (princ (format " (default, %s)" input-method))
	    (setq input-method (assoc input-method input-method-alist))
	    (setq l (cons input-method (delete input-method l))))
	  (princ ":\n")
	  (while l
	    (if (string= language-name (nth 1 (car l)))
		(princ-list "  " (car (car l))
			    (format " (`%s' in mode line)" (nth 3 (car l)))))
	    (setq l (cdr l))))
	(terpri)
	(princ "Character sets:\n")
	(let ((l (get-language-info language-name 'charset)))
	  (if (null l)
	      (princ-list "  nothing specific to " language-name)
	    (while l
	      (princ-list "  " (car l) ": "
			  (charset-description (car l)))
	      (setq l (cdr l)))))
	(terpri)
	(princ "Coding systems:\n")
	(let ((l (get-language-info language-name 'coding-system)))
	  (if (null l)
	      (princ-list "  nothing specific to " language-name)
	    (while l
	      (princ			; (format "  %s (`%c' in mode line):\n\t%s\n"
	       ;; In XEmacs, `coding-system-mnemonic' returns string.
	       (format "  %s (`%s' in English, `%s' in mode line):\n\t%s\n"
		       (car l)
		       (coding-system-description (car l))
		       (coding-system-mnemonic (car l))
		       (or (coding-system-documentation (car l))
			   "Not documented."))			)
	      ;; (let ((aliases (coding-system-get (car l) 'alias-coding-systems)))
	      ;;   (when aliases
	      ;;     (princ "\t")
	      ;;     (princ (cons 'alias: (cdr aliases)))
	      ;;     (terpri)))
	      (setq l (cdr l)))))))))

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

(defvar char-code-property-table
  (make-char-table 'generic)
  "Char-table containing a property list of each character code.

See also the documentation of `get-char-code-property' and
`put-char-code-property'")
;;   (let ((plist (aref char-code-property-table char)))
(defun get-char-code-property (char propname)
  "Return the value of CHAR's PROPNAME property in `char-code-property-table'."
  (let ((plist (get-char-table char char-code-property-table)))
    (if (listp plist)
	(car (cdr (memq propname plist))))))

(defun put-char-code-property (char propname value)
  "Store CHAR's PROPNAME property with VALUE in `char-code-property-table'.
It can be retrieved with `(get-char-code-property CHAR PROPNAME)'."
  (let ((plist (get-char-table char char-code-property-table)))
    (if plist
	(let ((slot (memq propname plist)))
	  (if slot
	      (setcar (cdr slot) value)
	    (nconc plist (list propname value))))
      (put-char-table char (list propname value) char-code-property-table)
      )))


;; Pretty description of encoded string

;; Alist of ISO 2022 control code vs the corresponding mnemonic string.
;; (defvar iso-2022-control-alist
;;   '((?\x1b . "ESC")
;;     (?\x0e . "SO")
;;     (?\x0f . "SI")
;;     (?\x8e . "SS2")
;;     (?\x8f . "SS3")
;;     (?\x9b . "CSI")))

;; (defun encoded-string-description (str coding-system)
;;   "Return a pretty description of STR that is encoded by CODING-SYSTEM."
;;   (setq str (string-as-unibyte str))
;;   (let ((char (aref str 0))
;;         desc)
;;     (when (< char 128)
;;       (setq desc (or (cdr (assq char iso-2022-control-alist))
;;                      (char-to-string char)))
;;       (let ((i 1)
;;             (len (length str))) 
;;         (while (< i len)
;;           (setq char (aref str i))
;;           (if (>= char 128)
;;               (setq desc nil i len)
;;             (setq desc (concat desc " "
;;                                (or (cdr (assq char iso-2022-control-alist))
;;                                    (char-to-string char)))
;;                   i (1+ i))))))
;;     (or desc
;;         (mapconcat (function (lambda (x) (format "0x%02x" x))) str " "))))

;; (defun encode-coding-char (char coding-system)
;;   "Encode CHAR by CODING-SYSTEM and return the resulting string.
;; If CODING-SYSTEM can't safely encode CHAR, return nil."
;;   (if (cmpcharp char)
;;       (setq char (car (decompose-composite-char char 'list))))
;;   (let ((str1 (char-to-string char))
;;         (str2 (make-string 2 char))
;;         (safe-charsets (and coding-system
;;                             (coding-system-get coding-system 'safe-charsets)))
;;         enc1 enc2 i1 i2)
;;     (when (or (eq safe-charsets t)
;;               (memq (char-charset char) safe-charsets))
;;       ;; We must find the encoded string of CHAR.  But, just encoding
;;       ;; CHAR will put extra control sequences (usually to designate
;;       ;; ASCII charset) at the tail if type of CODING is ISO 2022.
;;       ;; To exclude such tailing bytes, we at first encode one-char
;;       ;; string and two-char string, then check how many bytes at the
;;       ;; tail of both encoded strings are the same.
;; 
;;       (setq enc1 (string-as-unibyte (encode-coding-string str1 coding-system))
;;             i1 (length enc1)
;;             enc2 (string-as-unibyte (encode-coding-string str2 coding-system))
;;             i2 (length enc2))
;;       (while (and (> i1 0) (= (aref enc1 (1- i1)) (aref enc2 (1- i2))))
;;         (setq i1 (1- i1) i2 (1- i2)))
;; 
;;       ;; Now (substring enc1 i1) and (substring enc2 i2) are the same,
;;       ;; and they are the extra control sequences at the tail to
;;       ;; exclude.
;;       (substring enc2 0 i2))))


;; #### The following section is utter junk from mule-misc.el.
;; I've deleted everything that's not referenced in mule-packages and
;; not in FSF 20.6; there's no point in keeping old namespace-polluting
;; Mule 2.3 crap around. --ben

(defvar self-insert-after-hook nil
  "Hook to run when extended self insertion command exits.  Should take
two arguments START and END corresponding to character position.")

(make-variable-buffer-local 'self-insert-after-hook)

(defun delete-text-in-column (from to)
  "Delete the text between column FROM and TO (exclusive) of the current line.
Nil of FORM or TO means the current column.

If there's a character across the borders, the character is replaced
with the same width of spaces before deleting."
  (save-excursion
    (let (p1 p2)
      (if from
	  (progn
	    (setq p1 (move-to-column from))
	    (if (> p1 from)
		(progn
		  (delete-char -1)
		  (insert-char ?  (- p1 (current-column)))
		  (forward-char (- from p1))))))
      (setq p1 (point))
      (if to
	  (progn
	    (setq p2 (move-to-column to))
	    (if (> p2 to)
		(progn
		  (delete-char -1)
		  (insert-char ?  (- p2 (current-column)))
		  (forward-char (- to p2))))))
      (setq p2 (point))
      (delete-region p1 p2))))

(defun cancel-undo-boundary ()
  "Cancel undo boundary."
  (if (and (consp buffer-undo-list)
	   (null (car buffer-undo-list)))
      (setq buffer-undo-list (cdr buffer-undo-list))))

(defun define-egg-environment (env-sym doc-string enable-function)
  "Define a new language environment for egg, named by ENV-SYM.
DOC-STRING should be a string describing the environment.
ENABLE-FUNCTION should be a function of no arguments that will be called
when the language environment is made current."
  (put env-sym 'egg-environ-doc-string doc-string)
  (put env-sym 'set-egg-environ enable-function))


;; Init code.

;; auto-language-alist deleted.  We have a more sophisticated system,
;; with the locales stored in the language data.

;; Initialised with an eval-when-compile in mule/general-late.el, which is
;; compiled after all the language support--and, thus, minority Chinese
;; coding systems and so on--has been loaded.
(defvar posix-charset-to-coding-system-hash nil
  "A map from the POSIX locale charset versions of the defined coding
systems' names, with all alpha-numeric characters removed, to the actual
coding system names.  Used at startup when working out which coding system
should be the default for the locale.  ")

(defun parse-posix-locale-string (locale-string)
  "Return values \(LANGUAGE REGION CHARSET MODIFIERS\) given LOCALE-STRING.

LOCALE-STRING should be a POSIX locale.  If it cannot be parsed as such, this
function returns nil. "
  (let (language region charset modifiers locinfo)
    (setq locale-string (downcase locale-string))
    (cond ((string-match
	    #r"^\([a-z0-9]\{2,2\}\)\(_[a-z0-9]\{2,2\}\)?\(\.[^@]*\)?\(@.*\)?$"
	    locale-string)
	   (setq language (match-string 1 locale-string)
		 region (match-string 2 locale-string)
		 charset (match-string 3 locale-string)
		 modifiers (match-string 4 locale-string)
		 region (and region (replace-in-string region "^_" ""))
		 charset (and charset (replace-in-string charset #r"^\." ""))
		 modifiers (and modifiers
				(replace-in-string modifiers "^@" "")))
	   (when (and modifiers (equal modifiers "euro") (null charset))
	     ;; Not ideal for Latvian, say, but I don't have any locales
	     ;; where the @euro modifier doesn't mean ISO-8859-15 in the 956
	     ;; I have.
	     (setq charset "iso-8859-15"))
	   (values language region charset modifiers))
	  ((and (string-match "^[a-z0-9]+$" locale-string)
		(assoc-ignore-case locale-string language-info-alist))
	   (setq language (get-language-info locale-string 'locale)
		 language (if (listp language) (car language) language))
	   (values language region charset modifiers))
	  ((string-match #r"^\([a-z0-9]+\)\.\([a-z0-9]+\)$" locale-string)
	   (when (assoc-ignore-case
		  (setq locinfo (match-string 1 locale-string))
		  language-info-alist)
	     (setq language (get-language-info locinfo 'locale)
		   language (if (listp language) (car language) language)))
	   (setq charset (match-string 2 locale-string))
	   (values language region charset modifiers)))))

(defun create-variant-language-environment (langenv coding-system)
  "Create a variant of LANGENV with CODING-SYSTEM as its coding systems.

The coding systems in question are those described in the
`set-language-info' docstring with the property names of
`native-coding-system' and `coding-system'.  The name of the new language
environment is the name of the old language environment, followed by
CODING-SYSTEM in parentheses.  Returns the name of the new language
environment.  "
  (check-coding-system coding-system)
  (if (symbolp langenv) (setq langenv (symbol-name langenv)))
  (unless (setq langenv
		(assoc-ignore-case langenv language-info-alist))
    (error 'wrong-type-argument "Not a known language environment"))
  (set-language-info-alist
   (if (string-match " ([^)]+)$" (car langenv))
       (replace-match (format " (%s)" 
                              (upcase (symbol-name
                                       (coding-system-name coding-system))))
                      nil nil langenv)
     (format "%s (%s)" (car langenv)
             (upcase (symbol-name (coding-system-name coding-system)))))
   (destructive-plist-to-alist 
    (plist-put (plist-put (alist-to-plist (cdr langenv)) 'native-coding-system
			  coding-system) 'coding-system
			  (cons coding-system
				(cdr (assoc 'coding-system (cdr langenv))))))))

(defun get-language-environment-from-locale (locale)
  "Convert LOCALE into a language environment.
LOCALE is a C library locale string, as returned by `current-locale'.
Uses the `locale' property of the language environment."
  (block langenv
    (multiple-value-bind (language region charset modifiers)
	(parse-posix-locale-string locale)
      (let ((case-fold-search t)
	    (desired-coding-system
	     (and charset (gethash (replace-in-string charset "[^a-z0-9]" "")
                                   posix-charset-to-coding-system-hash)))
	    lang locs)
	(dolist (langcons language-info-alist)
	  (setq lang (car langcons)
		locs (get-language-info lang 'locale))
	  (dolist (loc (if (listp locs) locs (list locs)))
	    (cond ((functionp loc)
		   (if (funcall loc locale)
		       (return-from langenv lang)))
		  ((stringp loc)
		   (when (or (equal loc language)
			     (string-match
			      (format "^%s\\([^A-Za-z0-9]\\|$\\)" loc)
			      locale))
		     (if (or (null desired-coding-system)
			     (and desired-coding-system
				  (eq desired-coding-system
				      (get-language-info
				       lang
				       'native-coding-system))))
			 (return-from langenv lang)
		       (return-from langenv
			 (create-variant-language-environment
			  lang desired-coding-system))))))))))))

(defun mswindows-get-language-environment-from-locale (ms-locale)
  "Convert MS-LOCALE (an MS Windows locale) into a language environment.
MS-LOCALE is in the format recognized by `set-mswindows-current-locale' --
i.e. a language string or a cons (LANG . SUBLANG).  Note: This is NOT the
same as the C library locale format (see `set-current-locale')!

This looks up the `mswindows-locale' property of all language environments;
if nothing matching is found, it looks for a language environment with the
same name (modulo case differences) as the LANG part of the locale."
  (or (consp ms-locale) (setq ms-locale (cons ms-locale "DEFAULT")))
  (or (block langenv
	(dolist (langcons language-info-alist)
	  (let* ((lang (car langcons))
		 (mswlocs (get-language-info lang 'mswindows-locale))
		 (mswlocs (if (and (consp mswlocs)
				   (listp (cdr mswlocs)))
			      mswlocs (list mswlocs))))
	    (dolist (loc mswlocs)
	      (or (consp loc) (setq loc (cons loc "DEFAULT")))
	      (if (equalp loc ms-locale)
		  (return-from langenv lang))))))
      (dolist (langcons language-info-alist)
	(let* ((lang (car langcons)))
	  (if (equalp lang (car ms-locale))
	      (return-from nil lang))))))

(defun get-native-coding-system-from-language-environment (langenv locale)
  "Return the native coding system appropriate for LANGENV.
LANGENV is a string naming a language environment.  May use the LOCALE
\(which should be the C library LOCALE corresponding to LANGENV) to
determine the correct coding system. (For example, in the Japanese language
environment, there are multiple encodings in use: euc-jp, shift-jis, jis7,
jis8, iso-2022-jp, etc.  The LOCALE may tell which one is correct.)

Specifically: Under X, the returned value is determined from these two.
Under MS Windows, the native coding system must be set from the default
system locale and is not influenced by LOCALE. (In other words, a program
can't set the text encoding used to communicate with the OS.  To get around
this, we use Unicode whenever available, i.e. on Windows NT always and on
Windows 9x whenever a Unicode version of a system call is available.)"
  (if (eq system-type 'windows-nt)
      ;; should not apply to Cygwin, I don't think
      'mswindows-multibyte-system-default
    (let ((ncod (get-language-info langenv 'native-coding-system)))
      (if (or (functionp ncod) (not (listp ncod)))
	  (setq ncod (list ncod)))
      (let ((native
	     (dolist (try-native ncod)
	       (let ((result
		      (if (functionp try-native)
			  (funcall try-native locale)
			try-native)))
		 (if result (return result))))))
	(or native (car (get-language-info langenv 'coding-system))
	    'raw-text)))))

(defun get-coding-system-from-locale (locale)
  "Return the coding system corresponding to a locale string."
  (get-native-coding-system-from-language-environment
   (get-language-environment-from-locale locale) locale))

(defvar mswindows-langenv-to-locale-table (make-hash-table)
  "Table mapping language environments to associated MS Windows locales.
There may be more than one MS Windows locale that maps to a given language
environment, so once we've made the mapping, we record it here when we need
to make the reverse mapping.  For example, all MS Windows locales with
language ENGLISH will map to language environment English, and when the
user starts up in such a locale, switches to another language environment
and then back to English, we want the same locale again.")

(defun set-locale-for-language-environment (langenv)
  "Sets the current system locale as appropriate for LANGENV.
LANGENV is a language environment.  The locale is determined by looking at
the 'locale (or maybe 'mswindows-locale) property of LANGENV, and then
setting it using `set-current-locale' and maybe also
`mswindows-set-current-locale'.  Also sets the LANG environment variable.
Returns non-nil if successfully set the locale(s)."
  (flet ((mswindows-get-and-set-locale-from-langenv (langenv)
	   ;; find the mswindows locale for the langenv, make it current,
	   ;; and return it.  first we check the langenv-to-locale table
	   ;; ...
	   (let ((ms-locale
		  (gethash langenv mswindows-langenv-to-locale-table)))
	     (if ms-locale (progn
			  (declare-fboundp (mswindows-set-current-locale
					    ms-locale))
			  ms-locale)
	       ;; ... if not, see if the langenv specifies any locale(s).
	       ;; if not, construct one from the langenv name.
	       (let* ((mslocs (get-language-info langenv 'mswindows-locale))
		      (mslocs (or mslocs (cons (upcase langenv) "DEFAULT")))
		      (mslocs (if (and (consp mslocs)
					(listp (cdr mslocs)))
				   mslocs (list mslocs))))
		 (dolist (msloc mslocs)
		   ;; Sometimes a language with DEFAULT is different from
		   ;; with SYS_DEFAULT, and on my system
		   ;; (set-current-locale "chinese") is NOT the same as
		   ;; (set-current-locale "chinese-default")!  The latter
		   ;; gives Taiwan (DEFAULT), the former PRC (SYS_DEFAULT).
		   ;; In the interests of consistency, we always use DEFAULT.
		   (or (consp msloc) (setq msloc (cons msloc "DEFAULT")))
		   (when (condition-case nil
			     (progn
			       (declare-fboundp (mswindows-set-current-locale
						 msloc))
			       t)
			   (error nil))
		     (return msloc))))))))
    (if (eq system-type 'windows-nt)
	(let ((ms-locale (mswindows-get-and-set-locale-from-langenv langenv)))
	  (when ms-locale
	    ;; also need to set the clib locale.
	    (or (set-current-locale
		 ;; if the locale is '("DUTCH" . "DUTCH_BELGIAN"),
		 ;; try "DUTCH-BELGIAN". (Case is insignificant;
		 ;; "dutch-belgian" works just as well.)  This type
		 ;; of transformation should always work, and you
		 ;; get back the canonicalized version -- in this
		 ;; case "Dutch_Belgium.1252".  Note the futility of
		 ;; trying to construct "Belgium" directly from
		 ;; "BELGIAN".
		 ;;
		 ;; BUT ...  We actually have to be trickier.
		 ;; ("SPANISH" . "SPANISH_DOMINICAN_REPUBLIC") needs
		 ;; to end up as "SPANISH-DOMINICAN REPUBLIC"; any
		 ;; other punctuation makes it fail (you either get
		 ;; Spain for the country, or nil).
		 ;;
		 ;; assume it's DEFAULT or NEUTRAL (or something else
		 ;; without the language in it?) and prepend the
		 ;; language.
		 (if (string-match "_" (cdr ms-locale))
		     (replace-in-string
		      (replace-match "-" nil nil (cdr ms-locale)) "_" " ")
		   (format "%s-%s" (car ms-locale) (cdr ms-locale))))
		;; ???? huh ???? if failure, just try the language
		;; name.
		(set-current-locale (car ms-locale))))
	  ;; also set LANG, for the benefit of Cygwin subprocesses.
	  (let* ((cygloc (or (get-language-info langenv 'cygwin-locale)
			     (get-language-info langenv 'locale)))
		 (cygloc (if (listp cygloc) (car (last cygloc)) cygloc)))
	    (if (and cygloc (stringp cygloc)) (setenv "LANG" cygloc)))
	  (not (null ms-locale)))

      ;; not MS Windows native.

      ;; Cygwin is as usual an unholy mixture -- C library locales
      ;; that follow Unix conventions, but also MS Windows locales.
      ;; So set the MS Windows locale, and then try to find a Unix
      ;; locale.
      (when (eq system-type 'cygwin32)
	(mswindows-get-and-set-locale-from-langenv langenv))
      (let ((locs (get-language-info langenv 'locale)))
	(dolist (loc (if (listp locs) locs (list locs)))
	  (let ((retval
		 (cond ((functionp loc) (funcall loc nil))
		       ((stringp loc) (set-current-locale loc))
		       (t nil))))
	    (when retval
	      (setenv "LANG" retval)
	      (return t))))))))

(defun set-language-environment-coding-systems (language-name
						&optional eol-type)
  "Do various coding system setups for language environment LANGUAGE-NAME.
This function assumes that the locale for LANGUAGE-NAME has been set using
`set-current-locale'.

The optional arg EOL-TYPE specifies the eol-type of the default value
of buffer-file-coding-system set by this function."

;; The following appeared as the third paragraph of the doc string for this
;; function, but it's not in FSF 21.1, and it's not true, since we call
;; reset-coding-categories-to-default before calling this function.  ####
;; Should we rethink this?

; Note that `coding-priority-list' is not reset first; thus changing language
; environment allows recognition of coding systems from previously set language
; environments.  (This will not work if the desired coding systems are from the
; same category.  E.g., starting with a Hebrew language environment, ISO 8859-8
; will be recognized.  If you shift to Russian, ISO 8859-8 will be shadowed by
; ISO 8859-5, and cannot be automatically recognized without resetting the
; language environment to Hebrew.  However, if you shift from Japanese to
; Russian, ISO-2022-JP will continue to be automatically recognized, since
; ISO-8859-5 and ISO-2022-JP are different coding categories.)"

  (flet ((maybe-change-coding-system-with-eol (codesys eol-type)
	   ;; if the EOL type specifies a specific type of ending,
	   ;; then add that ending onto the given CODESYS; otherwise,
	   ;; return CODESYS unchanged.
	   (if (memq eol-type '(lf crlf cr unix dos mac))
	       (coding-system-change-eol-conversion codesys eol-type)
	     codesys)))

    ;; initialize category mappings and priority list.
    (let* ((priority (get-language-info language-name 'coding-priority))
	   (default-coding (car priority)))
      (if priority
	(let ((categories (mapcar 'coding-system-category priority))
	      category checked-categories)
	  (while priority
	    (unless (memq (setq category (car categories)) checked-categories)
	      (set-coding-category-system category (car priority))
	      (setq checked-categories (cons category checked-categories)))
	    (setq priority (cdr priority)
		  categories (cdr categories)))
	  (set-coding-priority-list (nreverse checked-categories))
	  ))

      ;; set the default buffer coding system from the first element of the
      ;; list in the `coding-priority' property, under Unix.  Under Windows, it
      ;; should stay at `mswindows-multibyte', which will reference the current
      ;; code page. ([Does it really make sense to set the Unix default
      ;; that way?  NOTE also that it's not the same as the native coding
      ;; system for the locale, which is correct -- the form we choose for text
      ;; files should not necessarily have any relevance to whether we're in a
      ;; Shift-JIS, EUC-JP, JIS, or other Japanese locale.])
      ;;
      ;;     On Unix--with the exception of Mac OS X--there is no way to
      ;;     know for certain what coding system to use for file names, and
      ;;     the environment is the best guess. If a particular user's
      ;;     preferences differ from this, then that particular user needs
      ;;     to edit ~/.xemacs/init.el. Aidan Kehoe, Sun Nov 26 18:11:31 CET
      ;;     2006. OS X uses an almost-normal-form version of UTF-8. 

      (unless (memq system-type '(windows-nt cygwin32))
	(set-default-buffer-file-coding-system
	 (maybe-change-coding-system-with-eol default-coding eol-type))))
    ;; (setq default-sendmail-coding-system default-coding)

    ;; set the native coding system and the default process-output system.
    (let ((native (get-native-coding-system-from-language-environment
		   language-name (current-locale))))

      (condition-case nil
	  (define-coding-system-alias 'native
	    (maybe-change-coding-system-with-eol native eol-type))
	(error
	 (warn "Invalid native-coding-system %s in language environment %s"
	       native language-name)))
      (define-coding-system-alias 'file-name 'native)
      ;; Set the default keyboard and terminal coding systems to the native
      ;; coding system of the language environment. 
      ;;
      (setq keyboard-coding-system native
	    terminal-coding-system native)

      ;; And do the same for any TTYs. 
      (dolist (con (console-list))
	(when (eq 'tty (device-type (car (console-device-list con))))
	  ;; Calling set-input-mode at the same time would be a sane thing
	  ;; to do here. I would prefer to default to accepting eight bit
	  ;; input and not using the top bit for Meta.
	  (set-console-tty-coding-system con native)))

      ;; process output should not have EOL conversion.  under MS Windows
      ;; and Cygwin, this screws things up (`cmd' is fine with just LF and
      ;; `bash' chokes on CR-LF).
      (setq default-process-coding-system
	    (cons (car default-process-coding-system) native)))))

(defun init-locale-at-early-startup ()
  "Don't call this."
  ;; Called directly from the C code in intl.c, very early in the startup
  ;; sequence.  Don't call this!!!  The main purpose is to set things up
  ;; so that non-ASCII strings of all sorts (e.g. file names, command-line
  ;; arguments, environment variables) can be correctly processed during
  ;; the rest of the startup sequence.  As a result, this will almost
  ;; certainly be the FIRST Lisp code called when a dumped XEmacs is run,
  ;; and it's called before ANY of the external environment is initialized.
  ;; Thus, it cannot interact at all with the outside world, make any
  ;; system calls, etc! (Except for `set-current-locale'.)
  ;;
  ;; NOTE: The following are the basic settings we have to deal with when
  ;; changing the language environment;
  ;;
  ;; -- current C library locale
  ;; -- under MS Windows, current MS Windows locale
  ;; -- LANG environment variable
  ;; -- native/file-name coding systems
  ;; -- subprocess write coding system (cdr of default-process-coding-system)
  ;; -- coding categories (for detection)

  (let (langenv)
    ;; under ms windows (any):
    (if (memq system-type '(windows-nt cygwin32))
      (let ((userdef (declare-fboundp (mswindows-user-default-locale)))
	    (sysdef (declare-fboundp (mswindows-system-default-locale))))
	;; (1) current langenv comes from user-default locale.
	(setq langenv (mswindows-get-language-environment-from-locale
		       userdef))
	;; (2) init the langenv-to-locale table.
	(puthash (mswindows-get-language-environment-from-locale sysdef)
		 sysdef mswindows-langenv-to-locale-table)
	;; user-default second in langenv-to-locale table so it will
	;; override the system-default if the two are different but both
	;; map to the same language environment
	(puthash langenv userdef mswindows-langenv-to-locale-table)
	;; (3) setup C lib locale, MS Windows locale, LANG environment
	;;     variable.  Note that under Cygwin we are ignoring the
	;;     passed-in LANG environment variable for the moment -- it's
	;;     usually wrong anyway and just says "C". #### Perhaps we
	;;     should reconsider.
	(and langenv (set-locale-for-language-environment langenv))
	;; (4) override current MS Windows locale with the user-default
	;;     locale.  Always init the MS Windows locale from the
	;;     user-default locale even if the langenv doesn't correspond;
	;;     we might not be able to find a langenv for the user-default
	;;     locale but we should still use the right code page, etc.
	(declare-fboundp (mswindows-set-current-locale userdef)))
      ;; Unix:
      (let (locstring)
	;; Init the POSIX locale from the environment--this calls the C
	;; library's setlocale(3).
	(set-current-locale "")
	;; Can't let locstring be the result of (set-current-locale "")
	;; because that can return a more detailed string than we know how
	;; to handle.
	(setq locstring (current-locale)
	      ;; assume C lib locale and LANG env var are set correctly.
	      ;; use them to find the langenv.
	      langenv
 	      (and locstring (get-language-environment-from-locale
 			      locstring)))))
    ;; All systems:
    (unless langenv (setq langenv "English"))
    (setq current-language-environment langenv)
    ;; Setup various coding systems and categories.
    (let ((default-eol-type (coding-system-eol-type
			     default-buffer-file-coding-system)))
      (reset-language-environment)
      (set-language-environment-coding-systems langenv default-eol-type))))

(defun init-mule-at-startup ()
  "Initialize MULE environment at startup.  Don't call this."

  (when (not load-unicode-tables-at-dump-time)
    (load-unicode-tables))

  ;; This is called (currently; might be moved earlier) from startup.el,
  ;; after the basic GUI systems have been initialized, and just before the
  ;; init file gets read in.  It needs to finish up initializing the
  ;; current language environment.  Very early in the startup procedure we
  ;; determined the default language environment from the locale, and
  ;; bootstrapped the native, file-name and process I/O coding systems.
  ;; Now we need to do it over `the right away'.
  (finish-set-language-environment current-language-environment)

  ;; Load a (localizable) locale-specific init file, if it exists.
  ;; We now use the language environment name, NOT the locale,
  ;; whose name varies from system to system.
  (load (format "%s%s/locale-start"
		(locate-data-directory "start-files")
		current-language-environment)
	t t)

  ;; #### the rest is junk that should be deleted.
  
  (when current-language-environment
    ;; rman seems to be incompatible with encoded text
    (setq Manual-use-rosetta-man nil))
  
  ;; Register available input methods by loading LEIM list file.
  (load "leim-list.el" 'noerror 'nomessage 'nosuffix)
  )

;; Code deleted: init-mule-tm (Enable the tm package by default)

;;; mule-cmds.el ends here
