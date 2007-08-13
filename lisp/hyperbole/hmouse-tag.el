;;!emacs
;;
;; FILE:         hmouse-tag.el
;; SUMMARY:      Smart Key support of programming language tags location.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, hypermedia, mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:    24-Aug-91
;; LAST-MOD:     17-Feb-97 at 15:31:50 by Bob Weiner
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hpath)
(require 'hbut)

(if (cond ((or (featurep 'etags) (featurep 'tags))
	   nil)
	  ((or hyperb:lemacs-p hyperb:emacs19-p)
	   ;; Force use of .elc file here since otherwise the bin/etags
	   ;; executable might be found in a user's load-path by the load
	   ;; command.
	   (or (load "etags.elc" t nil t)
	       (load "tags-fix" t)))
	  ((load "tags" t)))
    (provide 'tags))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar smart-asm-include-dirs nil
  "*Ordered list of directories to search for assembly language include files.
Each directory must end with a directory separator.")

(defconst smart-asm-include-regexp
  "[ \t*#|;]*\\(include\\|lib\\)[ \t]+\\([^ \t\n\^M]+\\)"
  "Regexp to match to assembly language include file lines.
Include keyword matched is grouping 1.  File name is grouping 2 but may be
missing its suffix, so add \".ins\" or \".inc\" if need be.
Examples include:
       INCLUDE GLOBALS
         should jump to file \"globals.ins\"
       lib conditionals_equ.inc
         should include \"conditionals_equ.inc\"")

(defvar smart-c-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C/C++ preprocessor.
Each directory must end with a directory separator.  See also
'smart-c-include-dirs'.")

(defvar smart-c-include-dirs nil
  "*Ordered list of directories to search for C/C++ include files.
Each directory must end with a directory separator.  Directories normally
searched by the C/C++ pre-processor should be set instead in
`smart-c-cpp-include-dirs'.")

(defvar smart-c-use-lib-man nil
  "When non-nil makes `smart-c' and `smart-c++' display man pages for recognized lib symbols.
When nil, `smart-c' and `smart-c++' look up only symbols defined in an etags
TAGS file.

Create the file ~/.CLIBS-LIST and populate it with the full pathnames (one per
line) of all of the C/C++ libraries whose symbols you want to match against.
Your MANPATH environment variable must include paths for the man pages of
these libraries also.

Your smart-clib-sym executable script must output a 1 if a symbol is from a
C/C++ library listed in ~/.CLIBS-LIST or 0 if not!  Otherwise, don't set this
variable to t.")

(defconst smart-c-include-regexp
  "[ \t/*]*#[ \t]*\\(include\\|import\\)[ \t]+\\([\"<]\\)\\([^\">]+\\)[\">]"
  "Regexp to match to C, C++, or Objective-C include file lines.
Include keyword matched is grouping 1.  Type of include, user-specified via
double quote, or system-related starting with `<' is given by grouping 2.
File name is grouping 3.")

(defvar smart-java-package-dirs
  (and (fboundp 'getenv) (getenv "JAVA_HOME")
       (list (expand-file-name "src/" (file-name-as-directory (getenv "JAVA_HOME")))))
  "*Ordered list of directories to search for imported Java packages.
Each directory must end with a directory separator.")

(defconst smart-java-package-regexp
  "[ \t/*]*\\(package\\|import\\)[ \t]+\\([^; \t\n\r\f]+\\)"
  "Regexp to match to Java `package' and `import' lines.
Keyword matched is grouping 1.  Referent is grouping 2.")

(defvar smart-emacs-tags-file nil
  "*Full path name of etags file for GNU Emacs source.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun smart-asm (&optional identifier next)
  "Jumps to the definition of optional assembly IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching assembly tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on an include statement, the include file is displayed;
     Look for include file in directory list `smart-asm-include-dirs'.
 (2) on an identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories."

  (interactive)
  (or
   (if identifier nil (smart-asm-include-file))
   (let ((tag (or identifier (smart-asm-at-tag-p))))
     ;; Set free variable tags-file-name so that next `find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for `%s' in `%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (smart-tags-display tag next)
	   (message "Found definition for `%s'." tag))
       (error (message "`%s' not found in `%s'." tag tags-file-name)
	      (beep))))))

;;;###autoload
(defun smart-asm-at-tag-p ()
  "Return assembly tag name that point is within, else nil."
  (let* ((identifier-chars "_.$a-zA-Z0-9")
	 (identifier (concat "[_.$a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (point) (match-end 0))
	   (point) (match-end 0))))))

(defun smart-c (&optional identifier next)
  "Jumps to the definition of optional C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on a C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (or
   (if identifier nil (smart-c-include-file))
   (let ((tag (or identifier (smart-c-at-tag-p))))
     ;; Set free variable tags-file-name so that next `find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for `%s' in `%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (smart-tags-display tag next)
	   (message "Found definition for `%s'." tag))
       (error
	(if (not smart-c-use-lib-man)
	    (progn (message "`%s' not found in `%s'." tag tags-file-name)
		   (beep))
	  (message "Checking if `%s' is a C library function..." tag)
	  (if (smart-library-symbol tag)
	      (progn (message "Displaying C library man page for `%s'." tag)
		     (manual-entry tag))
	    (message "`%s' not found in `%s' or C libraries."
		     tag tags-file-name)
	    (beep))))))))

;;;###autoload
(defun smart-c-at-tag-p ()
  "Return C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (point) (match-end 0))
	   (point) (match-end 0))))))

;;;###autoload
(defun smart-c++ (&optional identifier next)
  "Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (if (fboundp 'c++-to-definition)
      ;; Only fboundp if OO-Browser has been loaded.
      (smart-c++-oo-browser)
    (or
     (if identifier nil (smart-c-include-file))
     (let ((tag (or identifier (smart-c++-at-tag-p))))
       ;; Set free variable tags-file-name so that next `find-tag' command uses
       ;; whatever tags file is set here.
       (setq tags-file-name (smart-tags-file buffer-file-name))
       (message "Looking for `%s' in `%s'..." tag tags-file-name)
       (condition-case ()
	   (progn
	     (smart-tags-display tag next)
	     (message "Found definition for `%s'." tag))
	 (error
	  (if (not smart-c-use-lib-man)
	      (progn (message "`%s' not found in `%s'." tag tags-file-name)
		     (beep))
	    (message "Checking if `%s' is a C++ library function..." tag)
	    (if (smart-library-symbol tag)
		(progn (message "Displaying C++ library man page for `%s'." tag)
		       (manual-entry tag))
	      (message "`%s' not found in `%s' or C++ libraries."
		       tag tags-file-name)
	      (beep)))))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-c++-oo-browser (&optional junk)
  "Jumps to the definition of selected C++ construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown.

 (2) and (3) require that an OO-Browser Environment has been loaded with
     the {M-x br-env-load RET} command."

  (interactive)
  (c++-to-definition t))

(defun smart-c++-at-tag-p ()
  "Return C++ tag name that point is within, else nil."
  (let* ((identifier-chars "_:~<>a-zA-Z0-9")
	 (identifier (concat "\\([_~:<a-zA-Z][" identifier-chars "]*"
			     "[ \t]*[^]) \t:;.,?~{}][^[( \t:;.,~^!|?{}]?[=*]?\\)[ \t\n]*(")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (match-beginning 1) (match-end 1))
	   (match-beginning 1) (match-end 1))))))

(defun smart-emacs-lisp-mode-p ()
  "Return t if in a mode which uses Emacs Lisp symbols."
  (or (memq major-mode '(emacs-lisp-mode lisp-interaction-mode debugger-mode))
      ;; Emacs Lisp symbols appear in Help buffers frequently.
      (string-match "Help\\*$" (buffer-name))))

(defun smart-fortran (&optional identifier next)
  "Jumps to the definition of optional Fortran IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Fortran tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If on a Fortran identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories."
  (interactive)
  (let ((tag (or identifier (smart-fortran-at-tag-p))))
    ;; Set free variable tags-file-name so that next `find-tag' command uses
    ;; whatever tags file is set here.
    (setq tags-file-name (smart-tags-file buffer-file-name))
    (message "Looking for `%s' in `%s'..." tag tags-file-name)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'." tag))
      (error
       (message "`%s' not found in `%s'." tag tags-file-name)
       (beep)))))

;;;###autoload
(defun smart-fortran-at-tag-p ()
  "Return Fortran tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (point) (match-end 0))
	   (point) (match-end 0))))))

;;;###autoload
(defun smart-java (&optional identifier next)
  "Jumps to the definition of optional Java IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Java tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-dirs'.
 (3) on an Java identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories."

  (interactive)
  (if (fboundp 'java-to-definition)
      ;; Only fboundp if OO-Browser has been loaded.
      (smart-java-oo-browser)
    (or
     (if identifier nil (or (smart-java-cross-reference) (smart-java-packages)))
     (let ((tag (or identifier (smart-java-at-tag-p))))
       ;; Set free variable tags-file-name so that next `find-tag' command uses
       ;; whatever tags file is set here.
       (setq tags-file-name (smart-tags-file buffer-file-name))
       (message "Looking for `%s' in `%s'..." tag tags-file-name)
       (condition-case ()
	   (progn
	     (smart-tags-display tag next)
	     (message "Found definition for `%s'." tag))
	 (error (progn (message "`%s' not found in `%s'." tag tags-file-name)
		       (beep))))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-java-oo-browser (&optional junk)
  "Jumps to the definition of selected Java construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-dirs'.
 (3) within a method declaration, its definition is displayed;
 (4) on a class name, the class definition is shown."

  (interactive)
  (or (smart-java-cross-reference)
      (smart-java-packages)
      (java-to-definition t)))

;;;###autoload
(defun smart-java-at-tag-p ()
  "Return Java tag name that point is within, else nil."
  (let* ((identifier-chars "_$.a-zA-Z0-9")
	 (identifier
	  (concat "[_$a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (point) (match-end 0))
	   (point) (match-end 0))))))

(defun smart-lisp (&optional next)
  "Jumps to the definition of any selected Lisp construct.
If on an Emacs Lisp require, load, or autoload clause and `find-library'
from load-library package by Hallvard Furuseth (hallvard@ifi.uio.no) has
been loaded, jumps to library source, if possible.

Otherwise, the construct must be found within an `etags' generated tag file
in the current directory or any of its ancestor directories in order for its
definition to be located.

Optional NEXT means jump to next matching Lisp tag.  When matching to an Emacs
Lisp tag using `wtags' (Bob Weiner's personal modifications to `etags'),
there is no next tag, so display documentation for current tag instead.

This command assumes that its caller has already checked that the key was
pressed in an appropriate buffer and has moved the cursor to the selected
buffer."

  (interactive)
  ;; Handle `require', `load', and `autoload' clauses in Emacs Lisp.
  (or (and (fboundp 'find-library)
	   (smart-emacs-lisp-mode-p)
	   (let ((req)
		 (opoint (point)))
	     (setq req (and (search-backward "\(" nil t)
			    (looking-at (concat
					 "(\\(require\\|load\\|autoload\\)"
					 "[ \t]+.*['\"]"
					 "\\([^][() \t\n\^M`'\"]+\\)"))))
	     (goto-char opoint)
	     (if req (progn
		       (setq req (buffer-substring (match-beginning 2)
						   (match-end 2)))
		       (hpath:display-buffer (current-buffer))
		       (find-library req)
		       t))))
      (let ((tag (smart-lisp-at-tag-p)))
	;; Set free variable tags-file-name so that next `find-tag' command
	;; uses whatever tags file is set here.
	(setq tags-file-name (smart-tags-file default-directory))
	;; This part only works properly for Emacs Lisp, so is conditionalized.
	(if (and next (smart-emacs-lisp-mode-p) (featurep 'wtags))
	    (progn (setq tag (intern tag))
		   (cond ((fboundp tag) (describe-function tag))
			 ((boundp tag) (describe-variable tag))
			 (t (error "(smart-lisp): Unbound symbol: %s" tag))))
	  (condition-case ()
	      (smart-tags-display tag next)
	    (error (if (equal tags-file-name smart-emacs-tags-file)
		       (progn (message "`%s' not found in `%s'."
				       tag tags-file-name)
			      (beep))
		     (setq tags-file-name smart-emacs-tags-file)
		     (smart-tags-display tag next))))))))

(defun smart-lisp-at-tag-p ()
  "Returns Lisp tag name that point is within, else nil.
Returns nil when point is within a Lisp `def' keyword."
  (let* ((identifier-chars "-_*:+%$#!<>a-zA-Z0-9")
	 (identifier (concat "[-<*a-zA-Z][" identifier-chars "]*"))
	 (opoint (point)))
    (save-excursion
      (beginning-of-line)
      (if (and (looking-at "\\(;*[ \t]*\\)?(def[^- \n\t]+")
	       (< opoint (match-end 0)))
	  nil
	(goto-char opoint)
	(skip-chars-backward identifier-chars)
	(if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (point) (match-end 0))
	   (point) (match-end 0)))))))

;;;###autoload
(defun smart-lisp-mode-p ()
  "Return t if in a mode which uses Lisp symbols."
  (or (smart-emacs-lisp-mode-p)
      (memq major-mode '(lisp-mode scheme-mode))))

;;;###autoload
(defun smart-objc (&optional identifier next)
  "Jumps to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  
  (if (fboundp 'objc-to-definition)
      ;; Only fboundp if OO-Browser has been loaded.
      (smart-objc-oo-browser)
    (or
     (if identifier nil (smart-c-include-file))
     (let ((tag (or identifier (smart-objc-at-tag-p))))
       ;; Set free variable tags-file-name so that next `find-tag' command uses
       ;; whatever tags file is set here.
       (setq tags-file-name (smart-tags-file buffer-file-name))
       (message "Looking for `%s' in `%s'..." tag tags-file-name)
       (condition-case ()
	   (progn
	     (smart-tags-display tag next)
	     (message "Found definition for `%s'." tag))
	 (error
	  (if (not smart-c-use-lib-man)
	      (progn (message "`%s' not found in `%s'." tag tags-file-name)
		     (beep))
	    (message
	     "Checking if `%s' is an Objective-C library function..." tag)
	    (if (smart-library-symbol tag)
		(progn
		  (message
		   "Displaying Objective-C library man page for `%s'." tag)
		  (manual-entry tag))
	      (message "`%s' not found in `%s' or Objective-C libraries."
		       tag tags-file-name)
	      (beep)))))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-objc-oo-browser (&optional junk)
  "Jumps to the definition of selected Objective-C construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown.

 (2) and (3) require that an OO-Browser Environment has been loaded with
     the {M-x br-env-load RET} command."

  (interactive)
  (objc-to-definition t))

(defun smart-objc-at-tag-p ()
  "Return Objective-C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier
	  (concat "\\([-+][ \t]*\\)?\\([_a-zA-Z][" identifier-chars "]*\\)")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (smart-flash-tag
	   (buffer-substring (match-beginning 2) (match-end 2))
	   (match-beginning 2) (match-end 2))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun smart-asm-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in `smart-asm-include-dirs' and add suffix \".ins\" or
\".inc\" to filename if it lacks a suffix." 
  (let ((opoint (point)))
    ;; Some assemblers utilize the C preprocessor, so try that first.
    (cond ((smart-c-include-file))
	  ((progn (beginning-of-line)
		  (looking-at smart-asm-include-regexp))
	   (let ((file (buffer-substring (match-beginning 2) (match-end 2)))
		 (path)
		 (dir-list smart-asm-include-dirs))
	     (goto-char opoint)
	     (setq dir-list (cons (file-name-directory buffer-file-name)
				  dir-list))
	     (if (string-match "\\." file)
		 (setq file (regexp-quote file))
	       (setq file (concat (regexp-quote file) "\\.in[sc]$")))
	     (while dir-list
	       (setq dir-list
		     (if (setq path (car (directory-files
					  (car dir-list) t file)))
			 nil
		       (cdr dir-list))))
	     ;;
	     ;; If path exists, display file
	     ;;
	     (if path
		 (if (and (file-readable-p path)
			  (progn
			    (hpath:find path)
			    (cond ((featurep 'asm-mode) t)
				  ((load "asm-mode" nil 'nomessage)
				   (provide 'asm-mode))
				  (t
				   (beep)
				   (message
				    "(smart-asm-include-file):  asm-mode undefined.")
				   nil
				   ))))
		     nil
		   (beep)
		   (message "(smart-asm-include-file):  `%s' unreadable." path))
	       (beep)
	       (message "(smart-asm-include-file):  `%s' not found." file))
	     path))
	  ;; not on an include file line
	  (t (goto-char opoint)
	     nil))))


(defun smart-c-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in `smart-c-cpp-include-dirs' and in directory list
`smart-c-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at smart-c-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring (match-beginning 2)
					    (1+ (match-beginning 2)))))
	      (file (buffer-substring (match-beginning 3) (match-end 3)))
	      (path)
	      (dir-list smart-c-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (= incl-type ?<)
			     (append dir-list smart-c-cpp-include-dirs)
			   (cons (file-name-directory buffer-file-name)
				 dir-list)))
	  (while dir-list
	    (setq path (expand-file-name file (car dir-list))
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (and (file-readable-p path)
		       (progn
			 (hpath:find path)
			 (cond ((or (featurep 'cc-mode)
				    (featurep 'c-mode))
				t)
			       ((or (load "cc-mode" 'missing-ok 'nomessage)
				    (load "c-mode" 'missing-ok 'nomessage))
				(provide 'c-mode))
			       (t
				(beep)
				(message
				 "(smart-c-include-file):  c-mode undefined.")
				nil
				))))
		  nil
		(beep)
		(message "(smart-c-include-file):  `%s' unreadable." path))
	    (beep)
	    (message "(smart-c-include-file):  `%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))

(defun smart-flash-tag (tag start end)
  "Tries to flash TAG at START to END in buffer, to indicate that it is serving as a hyperlink button.
Returns TAG."
  ;; Button flashing code might not yet have been loaded if the whole
  ;; Hyperbole system has not been started.
  (if (fboundp 'hui:but-flash)
      (progn
	(ibut:label-set tag start end)
	(hui:but-flash)))
  tag)

(defun smart-java-cross-reference ()
  "If within a Java @see comment, displays the associated definition for editing and returns non-nil, else nil.
Non-nil is returned even if the @see referent cannot be found.

Does nothing if the `oo-browser' command is undefined, since it requires that
package for class and feature lookups."
  ;;
  ;; Valid forms of @see cross-references are:
  ;;    * @see #getComponent                        - current class attribute
  ;;    * @see #waitForAll()                        - current class method, no arguments
  ;;    * @see #checkID(int, boolean)               - current class method, with arguments
  ;;    * @see java.awt.ColorModel#getRGBdefault    - library class method
  ;;    * @see Component#paintAll                   - class method
  ;;    * @see java.awt.GridBagLayout               - library class
  ;;    * @see Container                            - class
  ;;
  ;; For simplicity sake, this code ignores the library path given with any
  ;; class in favor of the OO-Browser's lookup tables.  It also ignores any
  ;; parameters associated with a method, and thus cannot distinguish between
  ;; methods with the same name within a single class, which we believe to be
  ;; fairly bad form anyway.
  ;;
  (let ((opoint (point)))
    (if (and (eq major-mode 'java-mode) buffer-file-name
	     (fboundp 'br-env-load)
	     (or (looking-at "@see[ \t]+")
		 (and (re-search-backward "[@\n\r\f]" nil t)
		      (looking-at "@see[ \t]+"))))
	(let ((class) (feature))
	  ;; Ignore any library path preceding a classname (grouping 1)
	  (cond
	   ((looking-at
	     "@see[ \t]+\\(#\\)?\\([^][(){} \t\n\r\f#]+[.]\\)?\\([^][(){} \t\n\r\f#.]+\\)[][(){} \t\n\r\f]")
	    (if (match-beginning 1)
		(setq class nil
		      feature (buffer-substring (match-beginning 3)
						(match-end 3)))
	      (setq class (buffer-substring (match-beginning 3) (match-end 3))
		    feature nil)))
	   ((looking-at
	     "@see[ \t]+\\([^][(){} \t\n\r\f#]+[.]\\)?\\([^][(){} \t\n\r\f#.]+\\)#\\([^][(){} \t\n\r\f#.]+\\)")
	    (setq class (buffer-substring (match-beginning 2)
					  (match-end 2))
		  feature (buffer-substring (match-beginning 3)
					    (match-end 3)))))
	  ;; Return to original point.
	  (goto-char opoint)
	  ;; Lookup class / feature.
	  (cond
	   ((and (null class) (null feature))
	    ;; Invalid or unrecognized @see format, so ignore.
	    (message "(smart-java-cross-reference): Invalid @see cross-reference format.")
	    (beep)
	    t)
	   ;; Ensure that a Java OO-Browser environment has been loaded.
	   (t (if (or (and (boundp 'br-lang-prefix)
			   (equal br-lang-prefix "java-")
			   (boundp 'br-env-file) (stringp br-env-file)
			   (null br-env-spec))
		      ;; Load an existing Environment based on current
		      ;; buffer or prompt to build one.  This also
		      ;; loads the "br-java.el" library in which the
		      ;; `java-class-def-regexp' variable used below
		      ;; is defined.
		      (and (br-env-load
			    (smart-tags-file
			     buffer-file-name
			     (if (boundp 'br-env-default-file)
				 br-env-default-file "OOBR")))
			   (equal br-lang-prefix "java-")))
		  (cond ((null feature)
			 (br-edit nil class))
			(t
			 (if (null class)
			     (if (save-excursion
				   (or (re-search-backward java-class-def-regexp nil t)
				       (re-search-forward java-class-def-regexp nil t)))
				 (setq class (buffer-substring
					      (match-beginning java-class-def-name-grpn)
					      (match-end java-class-def-name-grpn)))
			       (error "(smart-java-cross-reference): This @see must be in a class definition.")))
			 (br-edit-feature class feature t)))
		(error "(smart-java-cross-reference): The OO-Browser failed to load a Java environment.")))))
      ;; Return to original point.
      (goto-char opoint)
      nil)))

(defun smart-java-library-path (library-name)
  "Search up directory tree from current directory for a match to LIBRARY-NAME."
  (let ((path default-directory)
	(library-path)
	(library-regexp (if (string-match "\\.\\|'//" library-name)
			    (regexp-quote
			     (concat (file-name-as-directory "")
				     (substring library-name 0 (match-beginning 0))
				     (file-name-as-directory "")))))
	(start 0))
    ;; Return rightmost match to first part of library-name.
    (if library-regexp
	(while (string-match library-regexp path start)
	  (setq start (1+ (match-beginning 0))
		library-path (substring path 0 (match-beginning 0)))))
    library-path))

(defun smart-java-packages ()
  "If point is on a `package' or `import' line, this tries to display the associated referent.
Returns non-nil iff on such a line, even if the referent is not found.
Look for packages in `smart-java-package-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at smart-java-package-regexp)
	(let ((keyword-type (buffer-substring
			     (match-beginning 1) (match-end 1)))
	      (referent (buffer-substring (match-beginning 2) (match-end 2)))
	      (found)
	      (subpath)
	      dir-list path subfile)
	  (goto-char opoint)
	  (if (string-equal keyword-type "package")
	      (let ((library-path (smart-java-library-path referent)))
		(if library-path
		    (hpath:find (expand-file-name 
				 (hypb:replace-match-string
				  "\\." referent (file-name-as-directory "") t)
				 library-path))
		  ;; Show the current directory, which should contain this package.
		  (hpath:find default-directory)))
	    ;; This is an `import' statement.  If it includes a *, show the
	    ;; associated library directory, otherwise, show the specific
	    ;; package.
	    (if (string-match "\\.\\*" referent)
		(setq subfile (substring referent 0 (match-beginning 0))
		      subfile (hypb:replace-match-string
			       "\\." subfile (file-name-as-directory "") t))
	      (setq subpath (hypb:replace-match-string
			     "\\." referent (file-name-as-directory "") t)
		    subfile (concat subpath ".java")))
	    ;;
	    ;; Try to find the path containing referent.
	    ;;
	    ;; Search up the current directory tree for a possible matching
	    ;; directory below which the referent library might live and add
	    ;; this to smart-java-package-dirs for searching.
	    (let ((library-path (smart-java-library-path referent)))
	      (if library-path
		  (setq dir-list (cons library-path smart-java-package-dirs))))

	    (while dir-list
	      (setq path (expand-file-name subfile (car dir-list))
		    dir-list (if (setq found (file-exists-p path))
				 nil
			       (cdr dir-list))))
	    (if (and (not found) subpath hyperb:microcruft-os-p)
		;; Try .jav suffix.
		(progn (setq subfile (concat subpath ".jav")
			     dir-list smart-java-package-dirs)
		       (while dir-list
			 (setq path (expand-file-name subfile (car dir-list))
			       dir-list (if (setq found (file-exists-p path))
					    nil
					  (cdr dir-list))))))
	    ;;
	    ;; If found, display file
	    ;;
	    (if found
		(if (file-readable-p path)
		    (hpath:find path)
		  (beep)
		  (message "(smart-java-packages):  `%s' unreadable." path))
	      (beep)
	      (message "(smart-java-packages):  `%s' not found." referent))
	    path))
      (goto-char opoint)
      nil)))

(defun smart-library-symbol (tag)
  "Return non-nil if TAG is a library symbol listed in cache of such symbols.
See the \"${hyperb:dir}/smart-clib-sym\" script for more information."
  (let ((buf (get-buffer-create "*junk*"))
	(found))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (call-process (expand-file-name "smart-clib-sym" hyperb:dir)
		    nil buf nil tag)
      (setq found (string-equal (buffer-substring 1 2) "1"))
      (set-buffer-modified-p nil)
      (kill-buffer buf)
      found)))

(defun smart-tags-display (tag next)
  (if next (setq tag nil))
  (let ((func (or (if (fboundp 'find-tag-internal) 'find-tag-internal)
		  (if (fboundp 'find-tag-noselect) 'find-tag-noselect)))
	;; For XEmacs
	(tags-always-exact t)
	;; For Emacs 19
	(find-tag-tag-order (if (boundp 'find-tag-tag-order)
				find-tag-tag-order)))
    (if find-tag-tag-order
	(if next nil (setq find-tag-tag-order '(tag-exact-match-p)))
      ;; For InfoDock (XEmacs may also take this branch), force exact match.
      (if (stringp tag) (setq tag (list tag))))
    (if (and func (funcall func tag))
	(hpath:display-buffer (current-buffer)))
    ;; Signals an error if tag is not found which is caught by many callers
    ;; of this function.
    (find-tag tag)))

;;;###autoload
(defun smart-tags-file-path (file)
  "Expand relative FILE name by looking it up in the nearest tags file.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file."
  (or (cond ((or (file-exists-p file) (file-name-absolute-p file)) file)
	    (t (let ((tags-file (smart-tags-file default-directory))
		     (file-regexp
		      (concat "\^L\n\\(.*/\\)?" (regexp-quote file) ",")))
		 (if tags-file
		     (progn
		       (set-buffer (find-file-noselect tags-file))
		       (goto-char (point-min))
		       (if (re-search-forward file-regexp nil t)
			   (expand-file-name
			    (buffer-substring (1- (match-end 0))
					      (progn (beginning-of-line)
						     (point))))))))))
      file))

;;;###autoload
(defun smart-tags-file (curr-filename &optional name-of-tags-file)
  "Return appropriate tags file name for CURR-FILENAME or `tags-file-name'.
Optional NAME-OF-TAGS-FILE is the literal filename for which to look."
  (let ((path curr-filename)
	(tags-file))
    (while (and
	    (stringp path)
	    (setq path (file-name-directory path))
	    (setq path (directory-file-name path))
	    ;; Not at root directory
	    (not (string-match ":?/\\'" path))
	    ;; No tags file
	    (not (file-exists-p
		  (setq tags-file (expand-file-name (or name-of-tags-file "TAGS") path)))))
      (setq tags-file nil))
    (if (and (not tags-file)
	     (stringp curr-filename)
	     (smart-emacs-lisp-mode-p)
	     (let ((path (file-name-directory curr-filename)))
	       (delq nil (mapcar
			  (function
			   (lambda (p)
			     (and p (equal (file-name-as-directory p)
					   path))))
			  load-path))))
	(setq tags-file smart-emacs-tags-file))
    (or tags-file tags-file-name
	(call-interactively 'visit-tags-table))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hmouse-tag)
