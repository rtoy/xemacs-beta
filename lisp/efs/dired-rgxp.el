;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:           dired-rgxp.el
;; Dired Version: #Revision: 7.9 $
;; RCS:
;; Description:   Commands for running commands on files whose names
;;                match a regular expression.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-rgxp)
(require 'dired)

;;; Variables

(defvar dired-flagging-regexp nil)
;; Last regexp used to flag files.

;;; Utility functions

(defun dired-do-create-files-regexp
  (file-creator operation arg regexp newname &optional whole-path marker-char)
  ;; Create a new file for each marked file using regexps.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Matches each marked file against REGEXP and constructs the new
  ;;   filename from NEWNAME (like in function replace-match).
  ;; Optional arg WHOLE-PATH means match/replace the whole pathname
  ;;   instead of only the non-directory part of the file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (name-constructor
	  (if whole-path
	      (list 'lambda '(from)
		    (list 'let
			  (list (list 'to
				      (list 'dired-string-replace-match
					    regexp 'from newname)))
			  (list 'or 'to
				(list 'dired-log
				      '(buffer-name (current-buffer))
				      "%s: %s did not match regexp %s\n"
				      operation 'from regexp))
			  'to))
	    (list 'lambda '(from)
		  (list 'let
			(list (list 'to
				    (list 'dired-string-replace-match regexp
					  '(file-name-nondirectory from)
					  newname)))
			(list 'or 'to
			      (list 'dired-log '(buffer-name (current-buffer))
				    "%s: %s did not match regexp %s\n"
				    operation '(file-name-nondirectory from)
				    regexp))
			'(and to
			      (expand-file-name
			       to (file-name-directory from)))))))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
	 (rename-regexp-help-form (format "\
Type SPC or `y' to %s one match, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					  (downcase operation)
					  (downcase operation)))
	 (query (list 'lambda '(from to)
		      (list 'let
			    (list (list 'help-form
					rename-regexp-help-form))
			    (list 'dired-query
				  '(quote dired-file-creator-query)
				  operation-prompt
				  '(dired-abbreviate-file-name from)
				  '(dired-abbreviate-file-name to))))))
    (dired-create-files
     file-creator operation fn-list name-constructor marker-char query)))

(defun dired-mark-read-regexp (operation)
  ;; Prompt user about performing OPERATION.
  ;; Read and return list of: regexp newname arg whole-path.
  (let* ((whole-path
	  (equal 0 (prefix-numeric-value current-prefix-arg)))
	 (arg
	  (if whole-path nil current-prefix-arg))
	 (regexp
	  (dired-read-with-history
	   (concat (if whole-path "Path " "") operation " from (regexp): ")
	   dired-flagging-regexp 'dired-regexp-history))
	 (newname
	  (read-string
	   (concat (if whole-path "Path " "") operation " " regexp " to: ")
	   (and (not whole-path) (dired-dwim-target-directory)))))
    (list regexp newname arg whole-path)))

;;; Marking file names matching a regexp.

(defun dired-mark-files-regexp (regexp &optional marker-char omission-files-p)
  "\\<dired-mode-map>Mark all files matching REGEXP for use in later commands.

A prefix argument \\[universal-argument] means to unmark them instead.

A prefix argument 0 means to mark the files that would me omitted by \\[dired-omit-toggle].
A prefix argument 1 means to unmark the files that would be omitted by \\[dired-omit-toggle].

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use \"\\.o$\" for
object files--just `.o' will mark more than you might think.  The files \".\"
and \"..\" are never marked.
"
  (interactive
   (let ((unmark (and (not (eq current-prefix-arg 0)) current-prefix-arg))
	 (om-files-p (memq current-prefix-arg '(0 1)))
	 regexp)
     (if om-files-p
	 (setq regexp (dired-omit-regexp))
       (setq regexp (dired-read-with-history
		    (concat (if unmark "Unmark" "Mark")
			    " files (regexp): ") nil
			    'dired-regexp-history)))
     (list regexp (if unmark ?\ ) om-files-p)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (and fn (string-match regexp (file-name-nondirectory fn)))))
     (if omission-files-p
	 "omission candidate file"
       "matching file"))))

(defun dired-flag-files-regexp (regexp)
  "In dired, flag all files containing the specified REGEXP for deletion.
The match is against the non-directory part of the filename.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (list (dired-read-with-history
		      "Flag for deletion (regexp): " nil
		      'dired-regexp-history)))
  (dired-mark-files-regexp regexp dired-del-marker))

(defun dired-mark-extension (extension &optional marker-char)
  "Mark all files with a certain extension for use in later commands.
A `.' is not prepended to the string entered."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive "sMark files with extension: \nP")
  (or (listp extension)
      (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
	   "\\("
	   (mapconcat 'regexp-quote extension "\\|")
	   "\\)$")
   marker-char))

(defun dired-flag-extension (extension)
  "In dired, flag all files with a certain extension for deletion.
A `.' is not prepended to the string entered."
  (interactive "sFlag files with extension: ")
  (dired-mark-extension extension dired-del-marker))

(defun dired-cleanup (program)
  "Flag for deletion dispensable files created by PROGRAM.
See variable `dired-cleanup-alist'."
  (interactive
   (list
    (let ((dired-cleanup-history (append dired-cleanup-history
					 (mapcar 'car dired-cleanup-alist))))
      (dired-completing-read
       "Cleanup files for: " dired-cleanup-alist nil t nil
       'dired-cleanup-history))))
  (dired-flag-extension (cdr (assoc program dired-cleanup-alist))))

;;; Commands on marked files whose names also match a regexp.

(defun dired-do-rename-regexp (regexp newname &optional arg whole-path)
  "Rename marked files containing REGEXP to NEWNAME.
As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.
With a zero prefix arg, renaming by regexp affects the complete
  pathname - usually only the non-directory part of file names is used
  and changed."
  (interactive (dired-mark-read-regexp "Rename"))
  (dired-do-create-files-regexp
   (function dired-rename-file)
   "Rename" arg regexp newname whole-path dired-keep-marker-rename))

(defun dired-do-copy-regexp (regexp newname &optional arg whole-path)
  "Copy all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "Copy"))
  (dired-do-create-files-regexp
   (function dired-copy-file)
   (if dired-copy-preserve-time "Copy [-p]" "Copy")
   arg regexp newname whole-path dired-keep-marker-copy))

(defun dired-do-hardlink-regexp (regexp newname &optional arg whole-path)
  "Hardlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "HardLink"))
  (dired-do-create-files-regexp
   (function add-name-to-file)
   "HardLink" arg regexp newname whole-path dired-keep-marker-hardlink))

(defun dired-do-symlink-regexp (regexp newname &optional arg whole-path)
  "Symlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "SymLink"))
  (dired-do-create-files-regexp
   (function make-symbolic-link)
   "SymLink" arg regexp newname whole-path dired-keep-marker-symlink))

(defun dired-do-relsymlink-regexp (regexp newname &optional whole-path)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-rename-regexp' and `dired-do-relsymlink'
  for more info."
  (interactive (dired-mark-read-regexp "RelSymLink"))
  (dired-do-create-files-regexp
   (function dired-make-relative-symlink)
   "RelSymLink" nil regexp newname whole-path dired-keep-marker-symlink))

;;;; Modifying the case of file names.

(defun dired-create-files-non-directory
  (file-creator basename-constructor operation arg)
  ;; Perform FILE-CREATOR on the non-directory part of marked files
  ;; using function BASENAME-CONSTRUCTOR, with query for each file.
  ;; OPERATION like in dired-create-files, ARG like in dired-get-marked-files.
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (dired-get-marked-files nil arg)
     (function
      (lambda (from)
	(let ((to (concat (file-name-directory from)
			  (funcall basename-constructor
				   (file-name-nondirectory from)))))
	  (and (let ((help-form (format "\
Type SPC or `y' to %s one file, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					(downcase operation)
					(downcase operation))))
		 (dired-query 'rename-non-directory-query
			      (concat operation " `%s' to `%s'")
			      (dired-make-relative from)
			      (dired-make-relative to)))
	       to))))
     dired-keep-marker-rename)))

(defun dired-rename-non-directory (basename-constructor operation arg)
  (dired-create-files-non-directory
   (function dired-rename-file)
   basename-constructor operation arg))

(defun dired-upcase (&optional arg)
  "Rename all marked (or next ARG) files to upper case."
  (interactive "P")
  (dired-rename-non-directory (function upcase) "Rename upcase" arg))

(defun dired-downcase (&optional arg)
  "Rename all marked (or next ARG) files to lower case."
  (interactive "P")
  (dired-rename-non-directory (function downcase) "Rename downcase" arg))

;;; end of dired-rgxp.el
