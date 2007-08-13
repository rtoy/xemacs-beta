;;; tmpl-minor-mode.el --- Template Minor Mode
;;;
;;; $Id: tmpl-minor-mode.el,v 1.5 1997/03/26 22:42:40 steve Exp $
;;;
;;; Copyright (C) 1993 - 1997  Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;; Keywords: data tools
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Commentary:
;;;
;;;	This file contains functions to expand templates.
;;;	Look at the file templates-syntax.doc for the syntax of the 
;;;	templates.
;;;	There are the following 2 interactive functions to expand
;;;	templates:
;;;		tmpl-expand-templates-in-region
;;;		tmpl-expand-templates-in-buffer
;;;	The following two interactive functions are to escape the 
;;;	unescaped special template signs:
;;;		tmpl-escape-tmpl-sign-in-region
;;;		tmpl-escape-tmpl-sign-in-buffer
;;;	The following function ask for a name of a template file, inserts
;;;	the template file and expands the templates:
;;;		tmpl-insert-template-file
;;;	If you want to use keystrokes to call the above functions, you must
;;;	switch the minor mode tmpl-mode on with `tmpl-minor-mode'. After
;;;	that, the following keys are defined:
;;;		`C-c x' 	= tmpl-expand-templates-in-region
;;;		`C-c C-x' 	= tmpl-expand-templates-in-buffer
;;;		`C-c ESC'	= tmpl-escape-tmpl-sign-in-region
;;;		`C-c C-ESC'	= tmpl-escape-tmpl-sign-in-buffer
;;; 	Type again `M-x tmpl-minor-mode' to switch the template minor mode off.
;;;
;;;	This file needs also the file adapt.el !
;;;
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp directories and the following
;;;	lisp command in your .emacs:
;;;		(load-library "templates")
;;;

(require 'adapt)


(defvar tmpl-template-dir-list nil
  "*A list of directories with the template files.
If it is nil, then the default-directory will be used. 
If more than one directory is given, then the
template filenames should differ in all directories.

This variable is used in the commands for inserting templates.
Look at `tmpl-insert-template-file-from-fixed-dirs' and
at `tmpl-insert-template-file'. The command `tmpl-insert-template-file'
uses only the car of the list (if it is a list).")

(defvar tmpl-automatic-expand t
  "*An inserted template will be automaticly expanded, if this is t.

This variable is used in the commands for inserting templates.
Look at `tmpl-insert-template-file-from-fixed-dirs' and
at `tmpl-insert-template-file'.")

(defvar tmpl-filter-regexp ".*\\.tmpl$"
  "*Regexp for filtering out non template files in a directory.
It is used in `tmpl-insert-template-file-from-fixed-dirs' to allow
only the selecting of files, which are matching the regexp. 
If it is nil, then the Filter \".*\\.tmpl$\" is used.
Set it to \".*\" if you want to disable the filter function or
use the command `tmpl-insert-template-file'.")

(defvar tmpl-history-variable-name 'tmpl-history-variable
  "The name of the history variable.
The history variable is used by the commands `tmpl-insert-template-file'
and `tmpl-insert-template-file-from-fixed-dirs'.")

(defvar tmpl-history-variable nil
  "The history variable. See also `tmpl-history-variable-name'.")

(defvar tmpl-sign "\000" "Sign which marks a template expression.")


(defvar tmpl-name-lisp "LISP" "Name of the lisp templates.")


(defvar tmpl-name-command "COMMAND" "Name of the emacs command templates.")


(defvar tmpl-name-comment "C" "Name of a comment template.")


(defvar tmpl-attribute-delete-line 'DELETE-LINE 
  "Attribute name of the attribute `delete-line`.")


(defvar tmpl-attribute-dont-delete 'DONT-DELETE
  "Attribute name of the attribute `dont-delete`.")


(defvar tmpl-end-template "END" "End of a template.")


(defvar tmpl-white-spaces " 	
" "String with white spaces.")


(defmacro tmpl-save-excursion (&rest body)
  "Put `save-excursion' and `save-window-excursion' around the body."
  (`(save-excursion
      (, (cons 'save-window-excursion
	       body)))))


(defun tmpl-current-line ()
  "Returns the current line number."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


;(defun mapcar* (f &rest args)
;  "Apply FUNCTION to successive cars of all ARGS, until one ends.
;Return the list of results."
;  (if (not (memq 'nil args))              ; If no list is exhausted,
;      (cons (apply f (mapcar 'car args))  ; Apply function to CARs.
;	    (apply 'mapcar* f             ; Recurse for rest of elements.
;		   (mapcar 'cdr args)))))
;
;(defmacro tmpl-error (&rest args)
;  "Widen the buffer and signal an error.
;Making error message by passing all args to `error',
;which passes all args to format."
;  (widen)
;  (error args))


(defun tmpl-search-next-template-sign (&optional dont-unescape)
  "Search the next template sign after the current point.
It returns t, if a template is found and nil otherwise.
If DONT-UNESCAPE is t, then the escaped template signs are not unescaped."
  (if (search-forward tmpl-sign nil t)
	(if (or (eq (point) (point-max))
		(not (string= tmpl-sign
			      (buffer-substring (point) (+ (length tmpl-sign) 
							   (point))))))
	    t
	  (if (not dont-unescape)
	      (delete-char (length tmpl-sign))
	    (forward-char))
	  (tmpl-search-next-template-sign dont-unescape))))


(defun tmpl-get-template-tag ()
  "Return a string with the template tag.
That is the string from the current point to the next `tmpl-sign',
without the tmpl-sign. The point is set after the `tmpl-sign'."
  (let ((template-start (point)))
    (if (tmpl-search-next-template-sign)
	(buffer-substring template-start (- (point) (length tmpl-sign)))
      nil)))


(defun tmpl-get-template-name (template-string)
  "Returns the name of the template in the TEMPLATE-STRING."
  (let* ((start (string-match (concat "[^"
				      tmpl-white-spaces
				      "]")
			      template-string))
	 (end (string-match (concat "["
				    tmpl-white-spaces
				    "]")
			    template-string start)))
    (if end
	(substring template-string start end)
      (substring template-string start))))


(defun tmpl-get-template-attribute-list (template-string)
  "Returns the attribute list (as a lisp list) from the template-string."
  (let* ((start (string-match (concat "[^"
				      tmpl-white-spaces
				      "]")
			      template-string)))
    (setq start (string-match (concat "["
				      tmpl-white-spaces
				      "]")
			      template-string start))
    (if start
	(car (read-from-string template-string start))
      nil)))


(defun template-delete-template (begin-of-template template-attribute-list)
  "Delete the current template from BEGIN-OF-TEMPLATE to the current point."
  (tmpl-save-excursion
    (if (or (not (assoc tmpl-attribute-dont-delete template-attribute-list))
	    (not (car (cdr (assoc tmpl-attribute-dont-delete 
				  template-attribute-list)))))
	(if (and (assoc tmpl-attribute-delete-line template-attribute-list)
		 (car (cdr (assoc tmpl-attribute-delete-line
				  template-attribute-list))))
	    (let ((end-of-template (point))
		  (diff 1))
	      (skip-chars-forward " \t") ; Skip blanks and tabs
	      (if (string= "\n" (buffer-substring (point) (1+ (point)))) 
		  (progn
		    (setq diff 0) ; don't delete the linefeed at the beginnig
		    (setq end-of-template (1+ (point)))))
	      (goto-char begin-of-template)
	      (skip-chars-backward " \t") ; Skip blanks and tabs
	      (if (eq (point) (point-min))
		  (delete-region (point) end-of-template)
		(if (string= "\n" (buffer-substring (1- (point)) (point)))
		    (delete-region (- (point) diff) end-of-template)
		  (delete-region begin-of-template end-of-template))))
	  (delete-region begin-of-template (point))))))


(defun tmpl-expand-comment-template (begin-of-template template-attribute-list)
  "Expand the comment template, which starts at the point BEGIN-OF-TEMPLATE.
TEMPLATE-ATTRIBUTE-LIST is the attribute list of the template."
  (end-of-line)
  (template-delete-template begin-of-template template-attribute-list))
;  (tmpl-save-excursion
;    (if (or (not (assoc tmpl-attribute-dont-delete template-attribute-list))
;	    (not (car (cdr (assoc tmpl-attribute-dont-delete 
;				  template-attribute-list)))))
;	(if (and (assoc tmpl-attribute-delete-line template-attribute-list)
;		 (car (cdr (assoc tmpl-attribute-delete-line
;				  template-attribute-list))))
;	    ;; Delete the whole line
;	    (let ((end-of-region (progn (end-of-line) (point)))
;		  (start-of-region begin-of-template)) ; ausgetauscht
;	      (delete-region start-of-region end-of-region)
;	      (delete-char 1))
;	  ;; Delete only the comment
;	  (let ((end-of-region (progn
;				 (end-of-line)
;				 (point)))
;		(start-of-region (progn (goto-char begin-of-template)
;					(point))))
;	    (delete-region start-of-region end-of-region))))))
  

(defun tmpl-get-template-argument ()
  "Return the Text between a start tag and the end tag as symbol.
The point must be after the `templ-sign' of the start tag.
After this function has returned, the point is after the
first `templ-sign' of the end tag."
  (let ((start-of-argument-text (progn (skip-chars-forward tmpl-white-spaces) 
				       (point))))
    (if (tmpl-search-next-template-sign)
	(car (read-from-string (buffer-substring start-of-argument-text
						 (- (point) 
						    (length tmpl-sign)))))
      (widen)
      (error "Error Before Line %d: First Template Sign Of End Tag Missing !"
	     (tmpl-current-line)))))


(defun tmpl-make-list-of-words-from-string (string)
  "Return a list of words which occur in the string."
  (cond ((or (not (stringp string)) (string= "" string))
	 ())
	(t (let* ((end-of-first-word (string-match 
				      (concat "[" 
					      tmpl-white-spaces 
					      "]") 
				      string))
		  (rest-of-string (substring string (1+ 
						     (or 
						      end-of-first-word
						      (1- (length string)))))))
	     (cons (substring string 0 end-of-first-word)
		   (tmpl-make-list-of-words-from-string 
		    (substring rest-of-string (or 
					       (string-match 
						(concat "[^" 
							tmpl-white-spaces 
							"]")
						rest-of-string)
					       0))))))))


(defun tmpl-get-template-end-tag ()
  "Return a list with the elements of the following end tag.
The point must be after the first `templ-sign' of the end tag.
After this function has returned, the point is after the
last `templ-sign' of the end tag."
  (let* ((start-point (progn (skip-chars-forward tmpl-white-spaces) 
			     (point)))
	 (end-tag-string (if (tmpl-search-next-template-sign)
			     (buffer-substring start-point
					       (- (point) (length tmpl-sign)))
			   (widen)
			   (error "Error Before Line %d: Last Template Sign Of End Tag Missing !" 
				  (tmpl-current-line)))))
    (tmpl-make-list-of-words-from-string end-tag-string)
  ))


(defun tmpl-expand-command-template (begin-of-template template-attribute-list)
  "Expand the command template, which starts at the point BEGIN-OF-TEMPLATE.
TEMPLATE-ATTRIBUTE-LIST is the attribute list of the template."
  (let ((template-argument (tmpl-get-template-argument))
	(template-end-tag (tmpl-get-template-end-tag)))
    (if (equal (list tmpl-end-template tmpl-name-command)
	       template-end-tag)
	(tmpl-save-excursion
	  (save-restriction
	    (widen)
	    (template-delete-template begin-of-template 
				      template-attribute-list)
	    (command-execute template-argument)))
      (widen)
      (error "ERROR in Line %d: Wrong Template Command End Tag"
	     (tmpl-current-line)))))


(defun tmpl-expand-lisp-template (begin-of-template template-attribute-list)
  "Expand the lisp template, which starts at the point BEGIN-OF-TEMPLATE.
TEMPLATE-ATTRIBUTE-LIST is the attribute list of the template."
  (let ((template-argument (tmpl-get-template-argument))
	(template-end-tag (tmpl-get-template-end-tag)))
    (if (equal (list tmpl-end-template tmpl-name-lisp)
	       template-end-tag)
	(tmpl-save-excursion
	  (save-restriction
	    (widen)
	    (template-delete-template begin-of-template 
				      template-attribute-list)
	    (eval template-argument)))
      (widen)
      (error "ERROR in Line %d: Wrong Template Lisp End Tag"
	     (tmpl-current-line)))))


(defun tmpl-expand-template-at-point ()
  "Expand the template at the current point.
The point must be after the sign ^@."
  (let ((begin-of-template (- (point) (length tmpl-sign)))
	(template-tag (tmpl-get-template-tag)))
    (if (not template-tag) 
	(progn
	  (widen)
	  (error "ERROR In Line %d: End Sign Of Template Tag Missing !"
		 (tmpl-current-line)))
      (let ((template-name (tmpl-get-template-name template-tag))
	    (template-attribute-list (tmpl-get-template-attribute-list 
				      template-tag)))
	(cond ((not template-name)
	       (widen)
	       (error "ERROR In Line %d: No Template Name"
		      (tmpl-current-line)))
	      ((string= tmpl-name-comment template-name)
	       ;; comment template found
	       (tmpl-expand-comment-template begin-of-template
					     template-attribute-list))
	      ((string= tmpl-name-command template-name)
	       ;; command template found
	       (tmpl-expand-command-template begin-of-template
					     template-attribute-list))
	      ((string= tmpl-name-lisp template-name)
	       ;; lisp template found
	       (tmpl-expand-lisp-template begin-of-template
					     template-attribute-list))
	      (t (widen)
		 (error "ERROR In Line %d: Wrong Template Name (%s) !"
			(tmpl-current-line) template-name)))))))

;;;###autoload
(defun tmpl-expand-templates-in-region (&optional begin end)
  "Expands the templates in the region from BEGIN to END.
If BEGIN and END are nil, then the current region is used."
  (interactive)
  (tmpl-save-excursion
    (narrow-to-region (or begin (region-beginning))
		      (or end (region-end)))
    (goto-char (point-min))
    (while (tmpl-search-next-template-sign)
      (tmpl-expand-template-at-point))
    (widen)))


;;;###autoload
(defun tmpl-expand-templates-in-buffer ()
  "Expands all templates in the current buffer."
  (interactive)
  (tmpl-expand-templates-in-region (point-min) (point-max)))


(defun tmpl-escape-tmpl-sign-in-region (&optional begin end)
  "Escapes all `tmpl-sign' with a `tmpl-sign' in the region from BEGIN to END.
If BEGIN and END are nil, then the active region between mark and point is 
used."
  (interactive)
    (save-excursion
      (narrow-to-region (or begin (region-beginning))
			(or end (region-end)))
      (goto-char (point-min))
      (while (tmpl-search-next-template-sign t)
	(insert tmpl-sign))
      (widen)))


(defun tmpl-escape-tmpl-sign-in-buffer ()
  "Escape all `tmpl-sign' with a `tmpl-sign' in the buffer."
  (interactive)
  (tmpl-escape-tmpl-sign-in-region (point-min) (point-max)))

(defun tmpl-get-table-with-template-files (template-dirs filter-regexp)
  "Returns a table (alist) with all filenames matching the FILTER-REGEXP.
It searches the files in in all directories of the list TEMPLATE-DIRS.
The alist looks like:
 '((file-name . file-name-with-path) ...)"
  (cond ((not template-dirs) '())
	((file-accessible-directory-p (car template-dirs))
	 (append (mapcar '(lambda (file)
			    (cons (file-name-nondirectory file) file))
			 (directory-files (car template-dirs)
					  t
					  filter-regexp
					  nil
					  t))
		 (tmpl-get-table-with-template-files (cdr template-dirs)
						     filter-regexp)))
	(t (tmpl-get-table-with-template-files (cdr template-dirs)
					       filter-regexp))))

(defun tmpl-insert-change-dir-entry (table)
  "Insert the entry '(\"Change the directory\" . select-other-directory)."
  (cons '("Change the directory" . select-other-directory)
	table))

(defun tmpl-read-template-directory (prompt default-direcory)
  "Reads a template directory.
If the directory isn't already in `tmpl-template-dirs', the it asks,
if it should be added to it."
  (let ((directory (expand-file-name
		    (read-directory-name prompt
					 (or default-direcory
					     (default-directory))))))
    (when (and (not (member* directory tmpl-template-dir-list :test 'string=))
	       (y-or-n-p 
		(format 
		 "Add %s permanent to the template directory list? "
		 directory)))
      (setq tmpl-template-dir-list (cons directory tmpl-template-dir-list)))
    directory))

(defun tmpl-read-template-filename (&optional
				    template-dirs
				    filter-regexp
				    history-variable)
  "Reads interactive the name of a template file.
The TEMPLATE-DIRS is a list of directories with template files.
Note: The template filenames should differ in all directories.
If it is nil, then the default-directory is used. 
FILTER-REGEXP can be used to allow only the selecting of files,
which are matching the regexp. If FILTER-REGEXP is nil, then the
Filter \".*\\.tmpl$\" is used.
HISTROY-VARIABLE contains the last template file names."
  (let ((filter (or filter-regexp ".*\\.tmpl$"))
	(directories (or template-dirs (list (default-directory))))
	(table nil)
	(file nil)
	(answer nil)
	(anser-not-ok t)
	(internal-history (mapcar '(lambda (path)
				     (file-name-nondirectory path))
				  (eval history-variable))))
    (while anser-not-ok
      (setq table (tmpl-get-table-with-template-files directories filter))
      (while (not table)
	;; Try another filter (all files)
	(setq table (tmpl-get-table-with-template-files directories ".*"))
	(unless table
	  (setq directories (list (tmpl-read-template-directory
				   "No files found, try another directory: "
				   (car directories))))))
      (setq table (tmpl-insert-change-dir-entry table))
      (setq answer (completing-read "Templatefile: "
				    table
				    nil
				    t
				    nil
				    'internal-history))
      (setq file (cdr (assoc* answer table :test 'string=)))
      (setq anser-not-ok (equal file 'select-other-directory))
      (when anser-not-ok
	(setq directories (list (tmpl-read-template-directory
				 "Directory with Templatefiles: "
				 (car directories))))))
    (unless (or (not history-variable)
;		(string= answer (car internal-history)))
		(string= file (car (eval history-variable))))
      (set history-variable (cons file (eval history-variable))))
    file))



;;;###autoload
(defun tmpl-insert-template-file-from-fixed-dirs (file)
  "Inserts a template FILE and expands it, if `tmpl-automatic-expand' is t.
This command tries to read the template file from a list of
predefined directories (look at `tmpl-template-dir-list') and it filters
the contents of these directories with the regular expression
`tmpl-filter-regexp' (look also at this variable). 
The command uses a history variable, which could be changed with the
variable `tmpl-history-variable-name'.

The user of the command is able to change interactively to another
directory by entering at first the string \"Change the directory\".
This may be too difficult for the user. Therefore another command
called `tmpl-insert-template-file' exist, which doesn't use fixed
directories and filters."
  (interactive
   (list (tmpl-read-template-filename tmpl-template-dir-list
				      tmpl-filter-regexp
				      tmpl-history-variable-name)))
  (insert-file (expand-file-name file))
  (if tmpl-automatic-expand
      (tmpl-expand-templates-in-region (point) (mark t)))
  file)


;;;###autoload
(defun tmpl-insert-template-file (file)
  "Inserts a template FILE and expand it, if `tmpl-automatic-expand' is t.
Look also at `tmpl-template-dir-list', to specify a default template directory.
You should also take a look at `tmpl-insert-template-file-from-fixed-dirs'
which has additional advantages (and disadvantages :-).

ATTENTION: The interface of this function has changed. The old 
function had the argument list (&optional TEMPLATE-DIR AUTOMATIC-EXPAND).
The variables `tmpl-template-dir-list' and `tmpl-automatic-expand' must
now be used instead of the args TEMPLATE-DIR and AUTOMATIC-EXPAND."
  (interactive 
   (list
    (let* ((default-directory (or (car tmpl-template-dir-list)
				  default-directory))
	   (filename (read-file-name "Templatefile: "
				     (if (listp tmpl-template-dir-list)
					 (car tmpl-template-dir-list))
				     nil
				     t
				     nil
				     tmpl-history-variable-name))
	   (directory (expand-file-name (file-name-directory filename))))
      (when (and (not (member* directory
			       tmpl-template-dir-list
			       :test 'string=))
		 (y-or-n-p 
		  (format 
		   "Add %s permanent to the template directory list? "
		   directory)))
	(setq tmpl-template-dir-list (cons directory tmpl-template-dir-list)))
      filename)))
  (insert-file (expand-file-name file))
  (if tmpl-automatic-expand
      (tmpl-expand-templates-in-buffer))
  file)

;(defun tmpl-insert-template-file (&optional template-dir automatic-expand)
;  "Insert a template file and expand it, if AUTOMATIC-EXPAND is t.
;The TEMPLATE-DIR is the directory with the template files."
;  (interactive)
;  (insert-file
;   (expand-file-name
;    (read-file-name "Templatefile: "
;		    template-dir
;		    nil
;		    t)))
;  (if automatic-expand
;      (tmpl-expand-templates-in-buffer)))


;;; Definition of the minor mode tmpl

(defvar tmpl-minor-mode nil
  "*t, if the minor mode tmpl-mode is on and nil otherwise.")


(make-variable-buffer-local 'tmpl-minor-mode)
;(set-default 'tmpl-minor-mode nil)


(defvar tmpl-old-local-map nil
  "Local keymap, before the minor-mode tmpl was switched on.")


(make-variable-buffer-local 'tmpl-old-local-map)



(defvar tmpl-minor-mode-map nil
  "*The keymap for the minor mode tmpl-mode.")


(make-variable-buffer-local 'tmpl-minor-mode-map)


(if (adapt-xemacsp)
    (defun tmpl-define-minor-mode-keymap ()
      "Defines the minor mode keymap."
      (define-key tmpl-minor-mode-map [(control c) (control c) x] 
	'tmpl-expand-templates-in-region)
      (define-key tmpl-minor-mode-map [(control c) (control c) (control x)] 
	'tmpl-expand-templates-in-buffer)
      (define-key tmpl-minor-mode-map [(control c) (control c) e] 
	'tmpl-escape-tmpl-sign-in-region)
      (define-key tmpl-minor-mode-map
	[(control c) (control c) (control e)]
	'tmpl-escape-tmpl-sign-in-buffer))
  (defun tmpl-define-minor-mode-keymap ()
    "Defines the minor mode keymap."
    (define-key tmpl-minor-mode-map [?\C-c ?\C-c ?x] 
      'tmpl-expand-templates-in-region)
    (define-key tmpl-minor-mode-map [?\C-c ?\C-c ?\C-x] 
      'tmpl-expand-templates-in-buffer)
    (define-key tmpl-minor-mode-map [?\C-c ?\C-c ?e] 
      'tmpl-escape-tmpl-sign-in-region)
    (define-key tmpl-minor-mode-map [?\C-c ?\C-c ?\C-e]
      'tmpl-escape-tmpl-sign-in-buffer))
    )


(defun tmpl-minor-mode ()
  "Toggle the minor mode tmpl-mode."
  (interactive)
  (if tmpl-minor-mode
      (progn
	(setq tmpl-minor-mode nil)
	(use-local-map tmpl-old-local-map)
	(setq tmpl-old-local-map nil))
    (setq tmpl-minor-mode t)
    (setq tmpl-old-local-map (current-local-map))
    (if tmpl-old-local-map
	(setq tmpl-minor-mode-map (copy-keymap tmpl-old-local-map))
      (setq tmpl-minor-mode-map nil)
      (setq tmpl-minor-mode-map (make-keymap))
      (set-keymap-name tmpl-minor-mode-map 'minor-mode-map))
    (tmpl-define-minor-mode-keymap)
    (use-local-map tmpl-minor-mode-map)))


(setq minor-mode-alist (cons '(tmpl-minor-mode " TMPL") minor-mode-alist))


(provide 'tmpl-minor-mode)
