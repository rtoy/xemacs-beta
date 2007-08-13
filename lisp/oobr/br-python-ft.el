;;!emacs
;;
;; FILE:         br-python-ft.el
;; SUMMARY:      Python OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     python, oop, tools
;;
;; AUTHOR:       Harri Pasanen, based on the C++ feature browser 
;;               by Bob Weiner
;; ORG:          Tekla Oy
;;
;; ORIG-DATE:    5-Apr-96
;; LAST-MOD:     1-May-96
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;    There may still be traces of C++ origin in this file.
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-python)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar python-import-dirs '("/usr/local/lib/python/")
  "Ordered list of module directories by default searched by python 
interpreter. Each directory must end with a directory separator.")

(defconst python-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst python-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has ben regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "\\`\\([^%s \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)%s"
	  python-type-tag-separator python-type-tag-separator br-feature-type-regexp
	  python-type-tag-separator python-type-tag-separator)
 "Regexp matching the fields of a Python feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun python-add-default-classes ()
  ;; Add to 'system' class table.
  ;; Add this default class for global functions
  (br-add-class "[functions]" br-null-path nil))

(defun python-feature-implementors (name)
  "Return unsorted list of Python feature tags which implement feature NAME.
This includes classes which define the interface for NAME as a pure virtual
function."
  (python-feature-matches (concat "^" (regexp-quote name) "$")))


(defun python-feature-signature-to-name (signature &optional with-class for-display)
  "Extracts the feature name from SIGNATURE.
The feature's class name is dropped from signature unless optional WITH-CLASS
is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type character is
prepended to the name for display in a browser listing."
  (let ((name))
    (cond
     ;; member
     ((string-match python-tag-fields-regexp signature)
      (setq name (substring signature (match-beginning (if for-display 2 3))
			    (match-end 3)))
      (if with-class
	  (setq name (concat
		      (substring signature (match-beginning 1) (match-end 1))
		      "." name)))
      ;; Remove any trailing whitespace.
      (br-delete-space name))
     ;;
     ;; unknown
     (t ;; Remove any trailing whitespace and add display prefix.
      (setq name (br-delete-space signature))
      (if for-display (python-feature-add-prefix name "" signature) name)))))

(defun python-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (python-routine-p class-or-signature)
      (progn
	(if (br-in-browser) (br-to-view-window))
	(br-feature-found-p (br-feature-file class-or-signature)
			    class-or-signature))))

(defun python-list-features (class &optional indent)
  "Return sorted list of Python feature tags lexically defined in CLASS."
  (let ((obuf (current-buffer))
	(features)
	(class-tag (concat "\n" class python-type-tag-separator))
	feature)
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (if (or (null indent) (<= indent 2))
	;; Include all features.
	(while (search-forward class-tag nil t)
	  (setq features (cons (br-feature-current) features)))
      ;; Omit friend features which are not inherited since indent > 2.
      (let ((friend-regexp (format "%s%% " python-type-tag-separator)))
	(while (search-forward class-tag nil t)
	  (setq feature (br-feature-current))
	  (or (string-match friend-regexp feature)
	      (setq features (cons feature features))))))
    (set-buffer obuf)
    (python-sort-features (nreverse features))))

(defun python-routine-p (str)
  (string-match "([^\)]*)" str))

(defun python-scan-features ()
  "Return reverse ordered list of Python function definitions in current 
buffer.  Assume point is at beginning of widened buffer.
'[functions]@- foo@foo(arguments)'"
  (save-excursion
    (let ((routines) class name rout)
      (while (re-search-forward python-routine-def nil t)
	(setq class "[functions]"
	      name (buffer-substring (match-beginning python-feature-name-grpn)
				     (match-end python-feature-name-grpn))
	      rout (python-feature-normalize 
		    (concat "def " name (python-scan-arguments)) class name)
	      routines (cons rout routines)))
      routines)))

(defun python-scan-arguments()
  "Return the functions arguments, point is assumed to be at the start of them"
  (let ((opoint (point)))
    (progn
      (search-forward ":" nil t)
      (buffer-substring opoint (point)))))

(defun python-sort-features (routine-list)
  (sort routine-list 'python-feature-lessp))

(defun python-to-definition (&optional other-win)
  "If point is on an import statement, look for the module file.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((python-import-file other-win))
     (t	(beep)
	(message
	 "(OO-Browser):  Select a module from import statement display its source.")
	nil))))

(defun python-store-class-info (class)
  "Lookup Python doc-string for class or method/function"
 (setq python-docstring (python-lookup-docstring class)))


(defun python-insert-class-info ()
  "Use the info facility to display Python doc-strings"
  (interactive)
  (insert python-docstring))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun python-lookup-docstring (class)
  "Looks up a docstring for any browser listing entry."
  (let ((entry class)
	(filename nil)
	(feature-sig nil)
	(docstring nil))
      (cond ((br-find-feature-entry)
	     (progn
	       (setq feature-sig (br-feature-get-signature))
	       (setq filename (br-feature-file feature-sig))))
	    ((and (setq entry (br-find-class-name))
		  (br-class-in-table-p entry))
	     (setq filename (br-class-path entry)))
	    (t (error "(OO-Browser): Entry may be referenced but not defined in the Environment.")))
      (if filename
	  (setq docstring 
		(python-get-docstring-from-source entry feature-sig filename)))
      (if docstring
	  docstring
	(concat class " does not have a documentation string."))))

(defun python-get-file-buffer (filename)
  "Insert FILENAME contents into a temporary buffer and select buffer.
Does not run any find-file hooks.  Marks buffer read-only to prevent
any accidental editing."
  (let ((buf (get-buffer-create *br-tmp-buffer*)))
    (set-buffer buf)
    (buffer-disable-undo buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename t)))

(defun python-get-docstring-from-source (entry feature-sig filename)
  "Scan source for docstring for entry.  If feature-sig non nil, locate 
feature, otherwise entry is the class"
  (let ((no-kill (get-file-buffer filename))
	(docstring nil))
    (if no-kill
	(set-buffer no-kill)
      (python-get-file-buffer filename))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (point-min))
	(if feature-sig
	    (if (python-feature-locate-p feature-sig)
		(setq docstring (python-extract-docstring))
	      nil)
	  (if (re-search-forward (python-class-definition-regexp entry) nil t)
	      (setq docstring (python-extract-docstring))
	    nil))))
    (if (not no-kill)
	(kill-buffer *br-tmp-buffer*))
    docstring))

(defun python-extract-docstring ()
  "Return the documentation string for the class or method at point, or
nil if it does not exist"
  (search-forward ":" nil t)
  (if (looking-at
       (concat python-empty-line "+" 
	       whitespace python-string-start))
      (progn 
	(let ((start (match-end 0))
	      (end-quote (buffer-substring (match-beginning 4) (match-end 4))))
	  (goto-char start)
	  (search-forward end-quote nil t)
	  (buffer-substring start (match-beginning 0))))
    nil))

(defconst python-string-start
  (concat 
   "\\("
   "'''"                ; triple single-quoted
   "\\|"		; or
   "\"\"\""	        ; triple double-quoted
   "\\|"		; or
   "'"		        ; single-quoted, not empty
   "\\|"		; or
   "\""	                ; double-quoted, not empty
   "\\)")
  "regexp matching python string literal starting quotes")

(defconst python-empty-line
  (concat 
   "\\("
   "\\(" whitespace "\n\\)" 
   "\\|"
   "\\(" whitespace "#.*$\\)"
   "\\)")
  "regexp matching an empty python line, which can be a comment line")

(defun python-feature-decl ()
  (if (looking-at python-class-decl)
      nil
    (looking-at python-feature-decl)))

(defun py-count-triple-quotes-forward ()
  "Count the number of trible quotes from the point to eof"
  (let ((count 0))
    (while (re-search-forward "'''\\|\"\"\"" nil t)
      (setq count (1+ count)))
    count))

(defun python-within-string-p ()
  "Return non-nil if point is within a multi-line python string."
  (save-excursion
    (if (= (% (py-count-triple-quotes-forward) 2) 1)
	t
      nil)))

(defun python-feature-lessp (routine1 routine2)
  (string-lessp (python-feature-signature-to-name routine1)
		(python-feature-signature-to-name routine2)))

(defun python-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP."
  ;; Ensure match to feature names only; also handle "^" and "$" meta-chars
  (setq regexp
	(concat (format "^[^%s \n]+%s%s "
			python-type-tag-separator python-type-tag-separator
			br-feature-type-regexp)
		(if (equal (substring regexp 0 1) "^")
		    (progn (setq regexp (substring regexp 1)) nil)
		  python-identifier-chars)
		(if (equal (substring regexp -1) "$")
		    (substring regexp 0 -1)
		  (concat regexp python-identifier-chars))
		python-type-tag-separator))
  (save-excursion
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (let ((features))
      (while (re-search-forward regexp nil t)
	(setq features (cons (br-feature-current) features)))
      features)))

(defun python-feature-normalize (routine class name)
  (setq class (br-delete-space class)
	name (concat "- " name)
	routine (concat class python-type-tag-separator 
			name python-type-tag-separator 
			(br-delete-space routine)))
  routine)

(defun python-feature-tag-class (signature)
  "Extract the class name from SIGNATURE."
  (cond ((string-match python-type-tag-separator signature)
	 (substring signature 0 (match-beginning 0)))
	(t "")))

(defun python-feature-tags-lookup (class-list ftr-pat &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-PAT.
Use routine tags table to locate a match.  Caller must use 'set-buffer'
to restore prior buffer when a match is not found."
  (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
  (let  ((classes class-list)
	 (found-ftr)
	 (ftr-regexp)
	 (class)
	 (ftr-path))
    (if (or (null class-list) (equal class-list '(nil)))
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      ftr-regexp (funcall ftr-pat class)
	      ftr-path (br-feature-def-file ftr-regexp)
	      found-ftr (if ftr-path
			    (br-edit-feature (br-feature-current)
					     ftr-path other-win))
	      classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(python-feature-tags-lookup
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun python-files-with-source (class)
  "Use CLASS to compute set of files that match to a Python source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 python-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (concat dir f)))
		   files)))))

(defun python-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching FTR-PAT."
  ;; If no class, search for non-member function.
  (or class-list (setq class-list '(nil)))
  (let ((obuf (current-buffer)))
    (prog1
	(if (and br-feature-tags-file
		 (file-exists-p br-feature-tags-file)
		 (file-readable-p br-feature-tags-file))
	    (python-feature-tags-lookup class-list ftr-pat other-win)
	  ;; Only works if features are in same directory as class def.
	  (python-scan-ancestors-feature class-list ftr-pat other-win))
      (set-buffer obuf))))

(defun python-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "\]\[ \t\n;,.\(\){}*&-")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))


(defun python-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward python-class-def-regexp nil t)
	  (progn (goto-char (match-beginning python-class-def-derived-grpn))
		 (setq class (python-normalize-class-match nil))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint)
		      class))))))

(defun python-get-feature-tags (routine-file &optional routine-list)
  "Scan Python ROUTINE-FILE and hold routine tags in 'br-feature-tags-file'.
Assume ROUTINE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  Optional ROUTINE-LIST can be
provided so that a non-standard scan function can be used before calling
this function."
  (interactive)
  (let ((obuf (current-buffer)))
    (or routine-list
	(setq routine-list (python-sort-features (nreverse
						  (python-scan-features)))))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    ;; Delete any prior routine tags associated with routine-file
    (if (search-forward routine-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point))
		 )))
    (if routine-list
	(progn (insert "\^L\n" routine-file "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       routine-list)
	       ))
    (set-buffer obuf)))

(defun python-find-module-name ()
  "Return current word as a potential module name."
  (save-excursion
    (let ((start))
      (forward-char 1)
      (skip-chars-backward python-identifier-chars)
      (setq start (point))
      (skip-chars-forward python-identifier-chars)
      (buffer-substring start (point)))))

(defun python-import-file (&optional other-win)
  "If point is on an import module line, try to display module.
Return non-nil iff an import file line, even if file is not found.
Look for include file in directory list 'python-import-dirs'"
  (let ((opoint (point)))
    (beginning-of-line)
    (if (and (looking-at python-import-regexp)
	     (goto-char opoint))
	(let ((file (concat (python-find-module-name) ".py"))
	      (path)
	      (dir-list (append python-lib-search-dirs python-sys-search-dirs 
				python-import-dirs))
	      (found))
	  (setq dir-list (cons (file-name-directory buffer-file-name)
			       dir-list))
	  (while dir-list
	    (setq path (concat (car dir-list) file)
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If not found in normal include dirs, check all Env paths also.
	  ;;
	  (if (not found)
	      (let ((paths (delq nil (hash-map 'cdr br-paths-htable))))
		(while paths
		  (setq path (car paths))
		  (if (string-equal (file-name-nondirectory path) file)
		      (setq found t paths nil)
		    (setq paths (cdr paths))))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (file-readable-p path)
		  (progn
		    (funcall br-edit-file-function path other-win)
		    (if (not (fboundp 'br-lang-mode))
			(python-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser): Module file '%s' unreadable." path))
	    (beep)
	    (message "(OO-Browser):  Module file '%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))

(defun python-locate-feature (ftr class ftr-pat &optional other-win)
  ;; 'class' may = nil, implying non-member function
  (or class (setq class "[functions]"))
  (let ((def-class))
    (if (and ftr-pat
	     (setq def-class
		   (python-find-ancestors-feature (list class)
					       ftr-pat other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Member `%s` of class '%s' inherited from class '%s'."
		     ftr class def-class))
	       t))))

(defun python-scan-ancestors-feature (class-list ftr-pat &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-PAT.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (ftr-regexp)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (python-files-with-source class)
	      ftr-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(python-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun python-scan-features-in-class (class start end)
  "Return reverse ordered list of Python routine definitions within CLASS def.
START and END give buffer region to search."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((routines) rout name type)
	;;
	;; Get member definitions
	;;
	(while (re-search-forward python-routine-def-in-class nil t)
	  (setq start (match-beginning 0)
		name (buffer-substring
		      (match-beginning python-feature-name-grpn)
		      (match-end python-feature-name-grpn))
		rout (python-feature-normalize 
		      (concat "def " name (python-scan-arguments)) class name)
		routines (cons rout routines)))
	routines))))

(defun python-skip-past-comments ()
  "Skip over comments immediately following point."
  (skip-chars-forward " \t\n")
  (while
      (cond ((looking-at "#")
	     (equal (forward-line 1) 0))
	    (t nil))))

(defun python-skip-to-statement ()
  (if (re-search-backward "^[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

(defun python-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;;
  ;; first move to the proper class implementation if feature-tag does not
  ;; include a <class>:: part and this is not a [default-class], so that if
  ;; two classes in the same file have the same feature signature, we still
  ;; end up at the right one.
  (if (string-match python-tag-fields-regexp feature-tag)
      (let ((class (substring feature-tag (match-beginning 1) (match-end 1))))
	(setq feature-tag (substring feature-tag (match-end 0)))
	(if regexp-flag
	    (if (not (string-match "\\`\\\\\\[\\|::" feature-tag))
		(re-search-forward (python-class-definition-regexp class t)
				   nil t))
	  (if (not (string-match "\\`\\[\\|::" feature-tag))
	      (re-search-forward (python-class-definition-regexp class)
				 nil t)))))
  (let ((found) (start))
    ;; Now look for feature expression.
    (or regexp-flag (setq feature-tag
			  (python-feature-signature-to-regexp feature-tag)))
    (while (and (re-search-forward feature-tag nil t)
		(setq start (match-beginning 0))
		(not (setq found (not 
				  (if (python-within-string-p)
				      (progn (search-forward "*/" nil t)
					     t)))))))
    (if found
	(progn (goto-char start)
	       (skip-chars-forward " \t\n")
	       (python-to-comments-begin)
	       (recenter 0)
	       (goto-char start)
	       t))))

(defun python-feature-name-to-regexp (name)
  "Converts routine NAME into a regular expression matching the routine's name tag."
  (setq name (python-feature-signature-to-regexp name))
  (aset name (1- (length name)) ?\()  ;; Match only to functions
  name)


(defun python-feature-signature-to-regexp (signature)
  "Given a Python SIGNATURE, return regexp used to match to its definition."
  (setq signature (regexp-quote signature))
  (let ((prefix-info
	 (if (string-match python-tag-fields-regexp signature)
	     (prog1 (substring signature (match-beginning 0) (match-end 0))
	       (setq signature (substring signature (match-end 0)))))))
    (let ((pat) (i 0) (c) (len (length signature)))
      (while (< i len)
	(setq c (aref signature i)
	      pat (cond ((= c ? )
			 ;; Allow for possible single line comment
			 ;; following any whitespace, e.g. following
			 ;; each routine argument.
			 (concat pat "[ \t\n\^M]*\\(//.*\\)?"))
			(t
			 (concat pat (char-to-string c))))
	      i (1+ i)))
      (setq pat (concat prefix-info pat)))))




;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar python-docstring ""
  "Documentation string for python class, method or function.")

(defconst python-code-file-regexp "\\.py\\"
  "Regular expression matching a unique part of Python source (non-header) file name and no others.")

(defconst python-import-regexp
  (concat "\\([ \t]*import[ \t]+\\)\\|"
	  "\\([ \t]*from[ \t]+"
	  python-identifier 
	  "[ \t]+import[ \t]+\\)")
  "Regexp to match to Python import statement
of include, user-specified via double quote, or system-related starting with
'<' is given by grouping 1.")

(defconst python-feature-name-grpn 1)

(defconst python-routine-def
  (concat "^def[ \t]+" python-identifier whitespace)
  "Matches global python function definition. group 1 gives the function name.
On return the point is at the starting '(' for parameters")

(defconst python-routine-def-in-class
  (concat "^[ \t]+def[ \t]+" python-identifier)
  "Matches python class method. group 1 gives the function name.
On return the point is at the starting '(' for parameters")

(defconst python-decl-template-grpn 3)
(defconst python-class-name-grpn 5)

(defconst python-stringlit
  (concat
   "'\\([^'\n\\]\\|\\\\.\\)*'"		; single-quoted
   "\\|"				; or
   "\"\\([^\"\n\\]\\|\\\\.\\)*\"")	; double-quoted
  "regexp matching a Python string literal")


(provide 'br-python-ft)
