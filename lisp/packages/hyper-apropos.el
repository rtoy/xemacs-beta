;;; hyper-apropos.el --- Hypertext emacs lisp documentation interface.

;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1996 Ben Wing.

;; Maintainer: Jonathan Stigelman <Stig@hackvan.com>
;; Keywords: lisp, tools, help, docs, matching

;; This file is part of XEmacs.

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;  based upon emacs-apropos.el by Frank C. Guida <fcg@philabs.philips.com>
;;
;;  Rather than run apropos and print all the documentation at once,
;;  I find it easier to view a "table of contents" first, then
;;  get the details for symbols as you need them.
;;
;;  This version of apropos prints two lists of symbols matching the
;;  given regexp:  functions/macros and variables/constants.
;;
;;  The user can then do the following:
;;
;;      - add an additional regexp to narrow the search
;;      - display documentation for the current symbol
;;      - find the tag for the current symbol
;;      - show any keybindings if the current symbol is a command
;;	- invoke functions
;;	- set variables
;;
;;  An additional feature is the ability to search the current tags
;;  table, allowing you to interrogate functions not yet loaded (this
;;  isn't available with the standard package).
;;
;;  Mouse bindings and menus are provided for XEmacs.
;;
;; additions by Ben Wing <wing@666.com> July 1995:
;; added support for function aliases, made programmer's apropos be the
;; default, various other hacking.

;;; Code:

(or (fboundp 'pprint)
    (progn (autoload 'pp "pp")
	   (fset 'pprint 'pp)))
;;(require 'tags "etags")

;;;###autoload
(defvar hypropos-show-brief-docs t
  "*If non-nil, `hyper-apropos' will display some documentation in the
\"*Hyper Apropos*\" buffer.  Setting this to nil will speed up searches.")

(defvar hypropos-prettyprint-long-values t
  "*If non-nil, then try to beautify the printing of very long values.")

;; I changed this to true because I think it's more useful this way. --ben

(defvar hypropos-programming-apropos t
  "*If non-nil, then `hyper-apropos' takes a bit longer and generates more
output.  If nil, then only functions that are interactive and variables that
are user variables are found by `hyper-apropos'.")

(defvar hypropos-prev-wconfig)

;; #### - move this to subr.el
(or (fboundp 'event-buffer)
    (defun event-buffer (event)
      "Returns the buffer associated with event, or nil."
      (let ((win (event-window event)))
	(and win (window-buffer win)))))

(defmacro eval-in-buffer (buffer &rest forms)
  "Evaluate FORMS in BUFFER."
  (` (let ((_unwind_buf_ (current-buffer)))
       (unwind-protect
	   (progn (set-buffer (, buffer))
		  (,@ forms))
	 (set-buffer _unwind_buf_)))))
(put 'eval-in-buffer 'lisp-indent-function 'defun)
	 
;; #### - move to faces.el
(defmacro init-face (face &rest init-forms)
  "Make a FACE if it doesn't already exist.  Then if it does not differ from
the default face, execute INIT-FORMS to initialize the face.  While the
init-forms are executing, the symbol `this' is bound to the face-object
being initialized." 
  (` (let ((this (make-face (, face))))	; harmless if the face is already there
     (or (face-differs-from-default-p this)
	 (, (cons 'progn init-forms))))))
(put 'init-face 'lisp-indent-function 'defun)

(init-face 'hyperlink
  (copy-face 'bold this)
  ;;(set-face-underline-p this nil) -- dog slow and ugly
  (condition-case nil
      (set-face-foreground this "blue")
    (error nil)))
(init-face 'documentation
  (let* ((ff-instance (face-font-instance 'default))
	(ff (and ff-instance (font-instance-name ff-instance))))
    (cond ((and ff (string-match "courier" ff))
	   ;; too wide unless you shrink it
	   ;; (copy-face 'italic this) fugly.
	   ;; (make-face-smaller this) fugly.
	   ))
    (condition-case nil
	(set-face-foreground this "firebrick")
      (error (copy-face 'italic this)))))

;; mucking with the sizes of fonts (perhaps with the exception of courier or
;; misc) is a generally losing thing to do.  Changing the size of 'clean'
;; really loses, for instance...

(init-face 'major-heading
  (copy-face 'bold this)
  (make-face-larger this)
  (make-face-larger this))
(init-face 'section-heading
  (copy-face 'bold this)
  (make-face-larger this))
(init-face 'heading
  (copy-face 'bold this))
(init-face 'standout
  (copy-face 'italic this))

(init-face 'warning
  (copy-face 'bold this)
  (and (eq (device-type) 'x)
       (eq (device-class) 'color)
       (set-face-foreground this "red")))

(defvar hypropos-help-map (let ((map (make-sparse-keymap)))
			    (suppress-keymap map)
			    (set-keymap-name map 'hypropos-help-map)
			    ;; movement
			    (define-key map " "     'scroll-up)
			    (define-key map "b"     'scroll-down)
			    (define-key map "/"     'isearch-forward)
			    (define-key map "?"     'isearch-backward)
			    ;; follow links
			    (define-key map "\r"    'hypropos-get-doc)
			    (define-key map "s"     'hypropos-set-variable)
			    (define-key map "t"     'hypropos-find-tag)
			    (define-key map "l"     'hypropos-last-help)
			    (define-key map [button2] 'hypropos-mouse-get-doc)
			    (define-key map [button3] 'hypropos-popup-menu)
			    ;; for the totally hardcore...
			    (define-key map "D"     'hypropos-disassemble)
			    ;; administrativa
			    (define-key map "a"     'hyper-apropos)
			    (define-key map "n"     'hyper-apropos)
			    (define-key map "q"     'hypropos-quit)
			    map
			    )
  "Keybindings for both the *Hyper Help* buffer and the *Hyper Apropos* buffer")

(defvar hypropos-map (let ((map (make-sparse-keymap)))
		       (set-keymap-name map 'hypropos-map)
		       (set-keymap-parents map (list hypropos-help-map))
		       ;; slightly differrent scrolling...
		       (define-key map " "     'hypropos-scroll-up)
		       (define-key map "b"     'hypropos-scroll-down)
		       ;; act on the current line...
		       (define-key map "w"     'hypropos-where-is)
		       (define-key map "i"     'hypropos-invoke-fn)
		       (define-key map "s"     'hypropos-set-variable)
		       ;; more administrativa...
		       (define-key map "P"     'hypropos-toggle-programming-flag)
		       (define-key map "k"     'hypropos-add-keyword)
		       (define-key map "e"     'hypropos-eliminate-keyword)
		       map
		       )
  "Keybindings for the *Hyper Apropos* buffer.
This map inherits from `hypropos-help-map.'")

(defvar hyper-apropos-mode-hook nil
  "*User function run after hyper-apropos mode initialization.  Usage:
\(setq hyper-apropos-mode-hook '(lambda () ... your init forms ...)).")

;; ---------------------------------------------------------------------- ;;

(defconst hypropos-junk-regexp "^Apropos\\|^Functions\\|^Variables\\|^$")

(defvar hypropos-currently-showing nil)	; symbol documented in help buffer now
(defvar hypropos-help-history nil)	; chain of symbols followed as links in
					; help buffer
(defvar hypropos-last-regexp nil)	; regex used for last apropos
(defconst hypropos-apropos-buf "*Hyper Apropos*")
(defconst hypropos-help-buf "*Hyper Help*")

;;;###autoload
(defun hyper-apropos (regexp toggle-apropos)
  "Display lists of functions and variables matching REGEXP
in buffer \"*Hyper Apropos*\".  If optional prefix arg is given, then the value
of `hypropos-programming-apropos' is toggled for this search.
See also `hyper-apropos-mode'."
  (interactive "sList symbols matching regexp: \nP")
  (or (memq major-mode '(hyper-apropos-mode hyper-help-mode))
      (setq hypropos-prev-wconfig (current-window-configuration)))
  (if (string= "" regexp)
      (if (get-buffer hypropos-apropos-buf)
	  (if toggle-apropos
	      (hypropos-toggle-programming-flag)
	    (message "Using last search results"))
	(error "Be more specific..."))
    (let (flist vlist)
      (set-buffer (get-buffer-create hypropos-apropos-buf))
      (setq buffer-read-only nil)
      (erase-buffer)
      (if toggle-apropos
	  (set (make-local-variable 'hypropos-programming-apropos)
	       (not (default-value 'hypropos-programming-apropos))))
      (if (not hypropos-programming-apropos)
	  (setq flist (apropos-internal regexp 'commandp)
		vlist (apropos-internal regexp 'user-variable-p))
	;; #### - add obsolete functions/variables here...
	;; #### - 'variables' may be unbound !!!
	(setq flist (apropos-internal regexp 'fboundp)
	      vlist (apropos-internal regexp 'boundp)))
      (insert-face (format "Apropos search for: %S\n\n" regexp) 'major-heading)
      (insert-face "* = command (M-x) or user-variable.\n" 'documentation)
      (insert-face "a = autoloaded, b = byte-compiled, i = internal, l = lambda, m = macro.\n\n" 'documentation)
      (insert-face "Functions and Macros:\n\n" 'major-heading)
      (hypropos-grok-functions flist)
      (insert-face "\n\nVariables and Constants:\n\n" 'major-heading)
      (hypropos-grok-variables vlist)
      (goto-char (point-min))
      ))
  (switch-to-buffer hypropos-apropos-buf)
  (hyper-apropos-mode regexp))

(defun hypropos-toggle-programming-flag ()
  (interactive)
  (eval-in-buffer hypropos-apropos-buf
    (set (make-local-variable 'hypropos-programming-apropos)
	 (not hypropos-programming-apropos)))
  (message "Re-running apropos...")
  (hyper-apropos hypropos-last-regexp nil))

(defun hypropos-grok-functions (fns)
  (let (fn bind type)
    (while (setq fn (car fns))
      (setq bind (symbol-function fn)
	    type (cond ((subrp bind) ?i)
		       ((compiled-function-p bind) ?b)
		       ((consp bind) (or (cdr
					  (assq (car bind) '((autoload . ?a)
							     (lambda . ?l)
							     (macro . ?m))))
					 ??))
		       (t ? )))
      (insert type (if (commandp fn) "* " "  "))
      (insert-face (format "%-30S" fn) 'hyperlink)
      (and hypropos-show-brief-docs
	   (if (function-obsolete-p fn)
	       (insert-face " - Obsolete." 'documentation)
	     (let ((doc (documentation fn)))
	       (if (not doc)
		   (insert-face " - Not documented." 'documentation)
		 (insert-face (concat " - "
				      (substring doc 0
						 (string-match "\n" doc)))
			      'documentation)))))
      (insert ?\n)
      (setq fns (cdr fns))
      )))

(defun hypropos-grok-variables (vars)
  (let (var userp)
    (while (setq var (car vars))
      (setq userp (user-variable-p var)
	    vars (cdr vars))
      (insert (if userp " * " "   "))
      (insert-face (format "%-30S" var) 'hyperlink)
      (and hypropos-show-brief-docs
	   (if (variable-obsolete-p var)
	       (insert-face " - Obsolete." 'documentation)
	     (let ((doc (documentation-property var 'variable-documentation)))
	       (if (not doc)
		   (insert-face " - Not documented." 'documentation)
		 (insert-face (concat " - "
				      (substring doc (if userp 1 0)
						 (string-match "\n" doc)))
			      'documentation)))))
      (insert ?\n)
      )))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-mode (regexp)
  "Improved apropos mode for displaying Emacs documentation.  Function and
variable names are displayed in the buffer \"*Hyper Apropos*\".  

Functions are preceded by a single character to indicates their types:
    a = autoloaded, b = byte-compiled, i = internal, l = lambda, m = macro.
Interactive functions are also preceded by an asterisk.
Variables are preceded by an asterisk if they are user variables.

General Commands:

  	SPC	- scroll documentation or apropos window forward
  	  b	- scroll documentation or apropos window backward
	  k     - eliminate all hits that don't contain keyword
	  n	- new search
  	  /	- isearch-forward
  	  q	- quit and restore previous window configuration
  
  Operations for Symbol on Current Line:
  
      	RET 	- toggle display of symbol's documentation
		  (also on button2 in xemacs)
  	  w     - show the keybinding if symbol is a command
  	  i	- invoke function on current line
  	  s	- set value of variable on current line
	  t	- display the C or lisp source (find-tag)"
  (delete-other-windows)
  (setq mode-name "Hyper-Apropos"
	major-mode 'hyper-apropos-mode
	buffer-read-only t
	truncate-lines t
	hypropos-last-regexp regexp
	modeline-buffer-identification (concat "Hyper Apropos: "
					       "\"" regexp "\""))
  (setq mode-motion-hook 'mode-motion-highlight-line)
  (use-local-map hypropos-map)
  (run-hooks 'hyper-apropos-mode-hook))

;; ---------------------------------------------------------------------- ;;

;;;###autoload
(defun hyper-describe-variable (symbol)
  "Hypertext drop-in replacement for `describe-variable'.
See also `hyper-apropos' and `hyper-describe-function'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (interactive 
   (let* ((v (variable-at-point))
          (val (let ((enable-recursive-minibuffers t))
                 (completing-read
		  (if v
		      (format "Describe variable (default %s): " v)
		    "Describe variable: ")
		  obarray 'boundp t))))
     (list (if (string= val "") v (intern-soft val)))))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (or (memq major-mode '(hyper-apropos-mode hyper-help-mode))
	(setq hypropos-prev-wconfig (current-window-configuration)))
    (hypropos-get-doc symbol t)))

;;;###autoload
(defun hyper-describe-function (symbol)
  "Hypertext replacement for `describe-function'.  Unlike `describe-function'
in that the symbol under the cursor is the default if it is a function.
See also `hyper-apropos' and `hyper-describe-variable'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (interactive
   (let (fn val)
     (setq fn (hypropos-this-symbol))	; symbol under point
     (or (fboundp fn)
	 (setq fn (function-called-at-point)))
     (setq val (let ((enable-recursive-minibuffers t))
		 (completing-read
		  (if fn 
		      (format "Describe function (default %s): " fn)
		    "Describe function: ")
		  obarray 'fboundp t)))
     (list (if (equal val "") fn (intern-soft val)))))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (or (memq major-mode '(hyper-apropos-mode hyper-help-mode))
	(setq hypropos-prev-wconfig (current-window-configuration)))
    (hypropos-get-doc symbol t)))

(defun hypropos-last-help (arg)
  "Go back to the last symbol documented in the *Hyper Help* buffer."
  (interactive "P")
  (let ((win (get-buffer-window hypropos-help-buf))
	(n (prefix-numeric-value arg)))
    (cond ((and (not win) (not arg))
	   ;; don't alter the help-history, just redisplay
	   )
	  ((<= (length hypropos-help-history) n)
	   ;; go back as far as we can...
	   (setcdr (nreverse hypropos-help-history) nil))
	  (t
	   (setq hypropos-help-history (nthcdr n hypropos-help-history))))
    (hypropos-get-doc (car hypropos-help-history) t)))

(defun hypropos-get-doc (&optional symbol force type)
  ;; #### - update this docstring
  "Toggle display of documentation for the symbol on the current line."
  ;; SYMBOL is the symbol to document.  FORCE, if non-nil, means to
  ;; regenerate the documentation even if it already seems to be there.  And
  ;; TYPE, if present, forces the generation of only variable documentation
  ;; or only function documentation.  Normally, if both are present, then
  ;; both will be generated.
  ;;
  ;; TYPES TO IMPLEMENT: obsolete face
  ;;
  (interactive)
  (or symbol
      (setq symbol (hypropos-this-symbol)))
  (or type
      (setq type '(function variable face)))
  (if (and (eq hypropos-currently-showing symbol)
	   (get-buffer hypropos-help-buf)
	   (get-buffer-window hypropos-help-buf)
	   (not force))
      ;; we're already displaying this help, so toggle its display.
      (delete-windows-on hypropos-help-buf)
    ;; OK, we've got to refresh and display it...
    (or (eq symbol (car hypropos-help-history))
	(setq hypropos-help-history
	      (if (eq major-mode 'hyper-help-mode)
		  ;; if we're following a link in the help buffer, then
		  ;; record that in the help history.
		  (cons symbol hypropos-help-history)
		;; otherwise clear the history because it's a new search.
		(list symbol))))
    (save-excursion
      (set-buffer (get-buffer-create hypropos-help-buf))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((standard-output (current-buffer))
	    ok beg desc
	    ftype macrop fndef
	    keys val doc
	    obsolete aliases alias-desc)
	(insert-face (format "`%s'\n\n" symbol) 'major-heading)
	(and (memq 'function type)
	     (fboundp symbol)
	     (progn 
	       (setq ok t
		     fndef (symbol-function symbol))
	       (while (symbolp fndef)
		 (setq aliases (cons fndef aliases))
		 (setq fndef (symbol-function fndef)))
	       (if (eq 'macro (car-safe fndef))
		   (setq macrop t
			 fndef (cdr fndef)))
	       (setq aliases (nreverse aliases))
	       ;; #### - the gods of internationalization shall strike me down!
	       (while aliases
		 (if alias-desc
		     (setq alias-desc (concat alias-desc ",\nwhich is ")))
		 (setq alias-desc (concat alias-desc
					  (format "an alias for `%s'"
						  (car aliases))))
		 (setq aliases (cdr aliases)))
	       (setq ftype (cond ((subrp fndef)                   'subr)
				 ((compiled-function-p fndef)     'bytecode)
				 ((eq (car-safe fndef) 'autoload) 'autoload)
				 ((eq (car-safe fndef) 'lambda)	  'lambda))
		     desc (concat (if (commandp symbol) "interactive ")
				  (cdr (assq ftype
					     '((subr     . "built-in ")
					       (bytecode . "compiled Lisp ")
					       (autoload . "autoloaded Lisp ")
					       (lambda   . "Lisp "))))
				  (if macrop "macro" "function")
				  ))
	       (if alias-desc
		   (setq desc (concat alias-desc
				      (if (memq (aref desc 0)
						'(?a ?e ?i ?o ?u))
					  ", an " ", a ")
				      desc)))
	       (aset desc 0 (upcase (aref desc 0))) ; capitalize
	       (insert-face desc 'section-heading)
	       (and (eq ftype 'autoload)
		    (insert (format ", (autoloaded from \"%s\")"
				    (nth 1 fndef))))
	       ;; #### - should also show local binding in some other
	       ;; buffer so that this function can be used in place of
	       ;; describe-function and describe-variable.
	       (if (setq keys (where-is-internal symbol (current-global-map)
						 nil nil nil))
		   (insert (format ", (globally bound to %s)"
				   (mapconcat
				    #'(lambda (x)
					(format "\"%s\""
						(key-description x)))
				    (sort keys #'(lambda (x y)
						   (< (length x) (length y))))
				    ", "))))
	       (insert ":\n\n")
	       (setq beg (point)
		     doc (or (documentation symbol) "function not documented"))
	       (insert-face "arguments: " 'heading)
	       (cond ((eq ftype 'lambda)
		      (princ (or (nth 1 fndef) "()")))
		     ((eq ftype 'bytecode)
		      (princ (or (if (fboundp 'compiled-function-arglist)
				     (compiled-function-arglist fndef)
				   (aref fndef 0)) "()")))
		     ((and (eq ftype 'subr)
			   (string-match
			    "[\n\t ]*\narguments: ?\\((.*)\\)\n?\\'"
			    doc))
		      (insert (substring doc
					 (match-beginning 1)
					 (match-end 1)))
		      (setq doc (substring doc 0 (match-beginning 0))))
		     (t (princ "[not available]")))
	       (insert "\n\n")
	       (let ((new
		      ;; cookbook from bytecomp.el
		      (get symbol 'byte-obsolete-info)))
		 (and new
		      (insert-face
		       (format "%s is an obsolete function; %s\n\n" symbol
			       (if (stringp (car new))
				   (car new)
				 (format "use %s instead." (car new))))
		       'warning)))
	       (insert-face doc 'documentation)
	       (indent-rigidly beg (point) 1)
	       (insert"\n\n")
	       ))
	(and (memq 'variable type)
	     (boundp symbol)
	     (progn 
	       (setq ok t)
	       (insert-face (if (user-variable-p symbol)
				"User variable"
			      "Variable")
			    'section-heading)
	       (and (local-variable-p symbol nil t)
		    (insert ", local when set"))
	       (insert ":\n\n")
	       (setq beg (point)
		     val (prin1-to-string (symbol-value symbol))
		     doc (or (documentation-property
			      symbol 'variable-documentation)
			     "variable not documented"))
	       
	       (let ((ob (get symbol 'byte-obsolete-variable)))
		 (setq obsolete
		       (and ob (format "%s is an obsolete variable; %s\n\n"
				       symbol
				       (if (stringp ob)
					   ob
					 (format "use %s instead." ob))))))
	       ;; generally, the value of the variable is short and the
	       ;; documentation of the variable long, so it's desirable
	       ;; to see all of the value and the start of the
	       ;; documentation.  Some variables, though, have huge and
	       ;; nearly meaningless values that force you to page
	       ;; forward just to find the doc string.  That is
	       ;; undesirable.
	       (if (< (length val) 69)	; 80 cols.  docstrings assume this.
		   (progn (insert-face "value: " 'heading)
			  (insert (format "%s\n\n" val))
			  (and obsolete (insert-face obsolete 'warning))
			  (insert-face doc 'documentation))
		 (insert "(see below for value)\n\n")
		 (and obsolete (insert-face obsolete 'warning))
		 (insert-face doc 'documentation)
		 (insert "\n\n")
		 (insert-face "value: " 'heading)
		 (if hypropos-prettyprint-long-values
		     (let ((pp-print-readably nil))
		       (pprint (symbol-value symbol)))
		   (insert val)))
	       (indent-rigidly beg (point) 2)
	       ))
	(and (memq 'face type)
	     (find-face symbol)
	     (progn
	       (setq ok t)
	       ;; #### - add some code here
	       (insert "Face documentation is \"To be implemented.\"\n\n")
	       )
	     )
	(or ok (insert-face "symbol is not currently bound" 'heading)))
      (goto-char (point-min)) 
      ;; pop up window and shrink it if it's wasting space
      (shrink-window-if-larger-than-buffer
       (display-buffer (current-buffer))) 
      (hyper-help-mode))    )
  (setq hypropos-currently-showing symbol))

; -----------------------------------------------------------------------------

(defun hyper-help-mode ()
  "Major mode for hypertext XEmacs help.  In this mode, you can quickly
follow links between back and forth between the documentation strings for
different variables and functions.  Common commands:

\\{hypropos-help-map}"
  (setq mode-motion-hook 'hypropos-highlight-lisp-symbol
	buffer-read-only t
	major-mode	     'hyper-help-mode
	mode-name	     "Hyper-Help")
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map hypropos-help-map))

(defun hypropos-highlight-lisp-symbol (event)
  ;; mostly copied from mode-motion-highlight-internal
  (let* ((window (event-window event))
	 (buffer (and window (window-buffer window)))
	 (point (and buffer (event-point event)))
	 st en sym highlight-p)
    (if buffer
	(progn
	  (set-buffer buffer)
	  (if point
	      (save-excursion
		(goto-char point)
		(setq st (save-excursion
			   (skip-syntax-backward "w_")
			   (skip-chars-forward "`")
			   (point))
		      en (save-excursion
			   (goto-char st)
			   (skip-syntax-forward "w_")
			   (skip-chars-backward ".")
			   (point))
		      sym (and (not (eq st en))
			       (intern-soft (buffer-substring st en)))
		      highlight-p (and sym
				       (or (boundp sym)
					   (fboundp sym))))
		(if highlight-p
		    (if mode-motion-extent
		      (set-extent-endpoints mode-motion-extent st en)
		    (setq mode-motion-extent (make-extent st en))
		    (set-extent-property mode-motion-extent 'highlight t))
		  (and mode-motion-extent
			 (progn (delete-extent mode-motion-extent)
				(setq mode-motion-extent nil)))
		  ))
	    ;; not over text; zero the extent.
	    (if (and mode-motion-extent (extent-buffer mode-motion-extent)
		     (not (eq (extent-start-position mode-motion-extent)
			      (extent-end-position mode-motion-extent))))
		(set-extent-endpoints mode-motion-extent 1 1)))))))


;; ---------------------------------------------------------------------- ;;

(defun hypropos-scroll-up ()
  "Scroll up the \"*Hyper Help*\" buffer if it's visible, or scroll this window up."
  (interactive)
  (let ((win (get-buffer-window hypropos-help-buf))
	(owin (selected-window)))
    (if win
	(progn
	  (select-window win)
	  (condition-case nil
	       (scroll-up nil)
	      (error (goto-char (point-max))))
	  (select-window owin))
      (scroll-up nil))))

(defun hypropos-scroll-down ()
  "Scroll down the \"*Hyper Help*\" buffer if it's visible, or scroll this window down."
  (interactive)
  (let ((win (get-buffer-window hypropos-help-buf))
	(owin (selected-window)))
    (if win
	(progn
	  (select-window win)
	  (condition-case nil
	       (scroll-down nil)
	      (error (goto-char (point-max))))
	  (select-window owin))
      (scroll-down nil))))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-mouse-get-doc (event)
  "Get the documentation for the symbol the mouse is on."
  (interactive "e")
  (mouse-set-point event)
  (save-excursion
    (let ((symbol (hypropos-this-symbol)))
      (if symbol
	  (hypropos-get-doc symbol)
	(error "Click on a symbol")))))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-add-keyword (pattern)
  "Use additional keyword to narrow regexp match.
Deletes lines which don't match PATTERN."
  (interactive "sAdditional Keyword: ")
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (keep-lines (concat pattern "\\|" hypropos-junk-regexp))
      )))

(defun hypropos-eliminate-keyword (pattern)
  "Use additional keyword to eliminate uninteresting matches.
Deletes lines which match PATTERN."
  (interactive "sKeyword to eliminate: ")
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (flush-lines pattern))
      ))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-this-symbol ()
  (save-excursion
    (cond ((eq major-mode 'hyper-apropos-mode)
	   (beginning-of-line)
	   (if (looking-at hypropos-junk-regexp)
	       nil
	     (forward-char 3)
	     (read (point-marker))))
	  (t
	   (let* ((st (progn
			(skip-syntax-backward "w_")
			;; !@(*$^%%# stupid backquote implementation!!!
			(skip-chars-forward "`")
			(point)))
		  (en (progn
			(skip-syntax-forward "w_")
			(skip-chars-backward ".")
			(point))))
	     (and (not (eq st en))
		  (intern-soft (buffer-substring st en))))))))

(defun hypropos-where-is (symbol)
  "Find keybinding for symbol on current line."
  (interactive (list (hypropos-this-symbol)))
  (where-is symbol))

(defun hypropos-invoke-fn (fn)
  "Interactively invoke the function on the current line."
  (interactive (list (hypropos-this-symbol)))
  (cond ((not (fboundp fn))
	 (error "%S is not a function" fn))
	(t (call-interactively fn))))

;;;###autoload
(defun hypropos-set-variable (var val)
  "Interactively set the variable on the current line."
  (interactive
   (let ((var (save-excursion
		(and (eq major-mode 'hypropos-help-mode)
		     (goto-char (point-min)))
		(hypropos-this-symbol))))
     (or (boundp var)
	 (setq var (completing-read "Set variable: "
				    obarray 'boundp t)))
     (hypropos-get-doc var t)
     (list var
	   (let ((prop (get var 'variable-interactive))
		 (print-readably t)
		 (val (symbol-value var)))
	     (if prop
		 (call-interactively (list 'lambda '(arg)
					   (list 'interactive prop)
					   'arg))
	       (eval-minibuffer
		(format "Set `%s' to value (evaluated): " var)
		(format (if (or (consp val)
				(and (symbolp val)
				     (not (memq val '(t nil)))))
			    "'%s" "%s")
			(prin1-to-string val))))))
     ))
  (set var val)
  (hypropos-get-doc var t))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-find-tag (&optional tag-name)
  "Find the tag for the symbol on the current line in other window.  In
order for this to work properly, the variable `tag-table-alist' or
`tags-file-name' must be set so that a TAGS file with tags for the emacs
source is found for the \"*Hyper Apropos*\" buffer."
  (interactive)
  ;; there ought to be a default tags file for this...
  (or tag-name (setq tag-name (symbol-name (hypropos-this-symbol))))
  (find-tag-other-window (list tag-name)))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-disassemble (sym)
  "Disassemble FUN if it is byte-coded.  If it's a lambda, prettyprint it."
  (interactive (list (hypropos-this-symbol)))
  (let ((fun sym) (trail nil) macrop)
    (while (and (symbolp fun) (not (memq fun trail)))
      (setq trail (cons fun trail)
	    fun (symbol-function fun)))
    (and (symbolp fun)
	 (error "Loop detected in function binding of `%s'" fun))
    (setq macrop (and  (consp fun)
		       (eq 'macro (car fun))))
    (cond ((compiled-function-p (if macrop (cdr fun) fun))
	   (disassemble fun)
	   (set-buffer "*Disassemble*")
	   (goto-char (point-min))
	   (forward-sexp 2)
	   (insert (format " for function `%S'" sym))
	   )
	  ((consp fun)
	   (with-output-to-temp-buffer "*Disassemble*"
	     (pprint (if macrop
			 (cons 'defmacro (cons sym (cdr (cdr fun))))
		       (cons 'defun (cons sym (cdr fun))))))
	   (set-buffer "*Disassemble*")
	   (emacs-lisp-mode))
	  ((or (vectorp fun) (stringp fun))
	   ;; #### - do something fancy here
	   (with-output-to-temp-buffer "*Disassemble*"
	     (princ (format "%s is a keyboard macro:\n\n\t" sym))
	     (prin1 fun)))
	  (t
	   (error "Sorry, cannot disassemble `%s'" sym)))))

;; ---------------------------------------------------------------------- ;;

(defun hypropos-quit ()
  (interactive)
  "Quit Hyper Apropos and restore original window config."
  (let ((buf (get-buffer hypropos-apropos-buf)))
    (and buf (bury-buffer buf)))
  (set-window-configuration hypropos-prev-wconfig))

;; ---------------------------------------------------------------------- ;;

;;;###autoload
(defun hypropos-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ((sym (hypropos-this-symbol))
	 (notjunk (not (null sym)))
	 (command-p (commandp sym))
	 (variable-p (and sym (boundp sym)))
	 (function-p (fboundp sym))
	 (apropos-p (eq 'hyper-apropos-mode
			(save-excursion (set-buffer (event-buffer event))
					major-mode)))
	 (name (if sym (symbol-name sym) ""))
	 (hypropos-menu
	  (delete
	   nil
	   (list (concat "Hyper-Help: " name)
	    (vector "Display documentation" 'hypropos-get-doc   notjunk)
	    (vector "Set variable"	'hypropos-set-variable	variable-p)
	    (vector "Show keys for"     'hypropos-where-is      command-p)
	    (vector "Invoke command"	'hypropos-invoke-fn	command-p)
	    (vector "Find tag"		'hypropos-find-tag	notjunk)
	    (and apropos-p
		 ["Add keyword..." hypropos-add-keyword	t])
	    (and apropos-p
		 ["Eliminate keyword..." hypropos-eliminate-keyword  t])
	    (if apropos-p
		["Programmers' Apropos" hypropos-toggle-programming-flag
		 :style toggle :selected hypropos-programming-apropos]
	      ["Programmers' Help" hypropos-toggle-programming-flag
	       :style toggle :selected hypropos-programming-apropos])
	    (and hypropos-programming-apropos
		 (vector "Disassemble function"
			 'hypropos-disassemble
			 function-p))
	    ["Help"                     describe-mode           t]
	    ["Quit"			hypropos-quit		t]
	    ))))
    (popup-menu hypropos-menu)))

(provide 'hyper-apropos)

;; end of hyper-apropos.el

