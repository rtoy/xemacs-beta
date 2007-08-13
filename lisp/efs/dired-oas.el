;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-oas.el
;; Dired Version: $Revision: 1.1 $
;; RCS:
;; Description:   dired odds and sods. Dired functions not usually needed.
;;                This file is not a reference to the Organization of
;;                American States.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't require or provide anything, as this file is just an archive.

(defun dired-sort-on-size ()
  "Sorts a dired listing on file size.
If your ls cannot sort on size, this is useful as `dired-after-readin-hook':
    \(setq dired-after-readin-hook 'dired-sort-on-size\)"
  (require 'sort)
  (goto-char (point-min))
  (dired-goto-next-file)		; skip `total' line
  (beginning-of-line)
  (sort-subr t 'forward-line 'end-of-line 'dired-get-file-size))

(defun dired-directories-of (files)
  ;; Return unique list of parent directories of FILES.
  (let (dirs dir file)
    (while files
      (setq file (car files)
	    files (cdr files)
	    dir (file-name-directory file))
      (or (member dir dirs)
	  (setq dirs (cons dir dirs))))
    dirs))

(defun dired-parse-ls-show ()
  (interactive)
   (let (inode s mode size uid gid nlink time name sym)
     (if (dired-parse-ls)
	 (message "%s" (list inode s mode nlink uid gid size time name sym))
       (message "Not on a file line."))))

(defun dired-files-same-directory (file-list &optional absolute)
  "If all files in LIST are in the same directory return it, otherwise nil.
Returned name has no trailing slash.  \"Same\" means file-name-directory of
the files are string=.  File names in LIST must all be absolute or all be
relative.  Implicitly, relative file names are in default-directory.  If
optional ABS is non-nil, the returned name will be absolute, otherwise the
returned name will be absolute or relative as per the files in LIST."
  (let ((dir (file-name-directory (car file-list))))
    (if (memq nil (mapcar (function
			   (lambda (file)
			     (string= dir (file-name-directory file))))
			  file-list))
	nil
      (directory-file-name
       (if (or (not absolute) (and dir (file-name-absolute-p dir)))
	   (or dir "")
	 (concat default-directory dir))))))
