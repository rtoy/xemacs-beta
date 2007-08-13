;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-uu.el
;; Dired Version: $Revision: 1.1 $
;; RCS:
;; Description:   Commands for uuencoding/uudecoding marked files.
;; Author:        Sandy Rutherford <sandy@math.ubc.ca>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-uu)
(require 'dired)

(defvar dired-uu-files-to-decode nil)
;; Fluid var to pass data inside dired-create-files.

(defun dired-uucode-file (file ok-flag)
  ;; uuencode or uudecode FILE.
  ;; Don't really support the ok-flag, but needed for compatibility
  (let ((handler (find-file-name-handler file 'dired-uucode-file)))
    (cond (handler
	   (funcall handler 'dired-uucode-file file ok-flag))
	  ((or (file-symlink-p file) (file-directory-p file))
	   nil)
	  (t
	   (if (assoc file dired-uu-files-to-decode)
	       (let ((default-directory (file-name-directory file)))
		 (if (dired-check-process
		      (concat "Uudecoding " file) shell-file-name "-c"
		      (format "uudecode %s" file))
		    (signal 'file-error (list "Error uudecoding" file))))
	     (let ((nfile (concat file ".uu")))
	       (if (dired-check-process
		   (concat "Uuencoding " file) shell-file-name "-c"
		   (format "uuencode %s %s > %s"
			   file (file-name-nondirectory file) nfile))
		   (signal 'file-error (list "Error uuencoding" file)))))))))

(defun dired-uucode-out-file (file)
  ;; Returns the name of the output file for the uuencoded FILE.
  (let ((buff (get-buffer-create " *dired-check-process output*"))
	(case-fold-search t))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (if (string-equal "18." (substring emacs-version 0 3))
	  (call-process "head" file buff nil "-n" "1")
	(insert-file-contents file nil 0 80))
      (goto-char (point-min))
      (if (looking-at "begin [0-9]+ \\([^\n]*\\)\n")
	  (expand-file-name
	   (buffer-substring (match-beginning 1) (match-end 1))
	   (file-name-directory file))
	nil))))

(defun dired-do-uucode (&optional arg files to-decode)
  "Uuencode or uudecode marked (or next ARG) files."
  (interactive
   (let* ((dir (dired-current-directory))
	  (files (dired-get-marked-files nil current-prefix-arg))
	  (arg (prefix-numeric-value current-prefix-arg))
	  (total (length files))
	  rfiles decoders ofile decode encode hint-p)
     (mapcar
      (function
       (lambda (fn)
	 (if (setq ofile (dired-uucode-out-file fn))
	     (setq decoders (cons (cons fn ofile) decoders)))))
      files)
     (setq decode (length decoders)
	   encode (- total decode)
	   hint-p (not (or (zerop decode) (zerop encode))))
     (setq rfiles
	   (mapcar
	    (function
	     (lambda (fn)
	       (if hint-p
		   (concat
		    (if (assoc fn decoders) " [de] " " [en] ")
		    (dired-make-relative fn dir t))
		 (dired-make-relative fn dir t))))
	    files))
     (or (memq 'uuencode dired-no-confirm)
	 (dired-mark-pop-up nil 'uuencode rfiles 'y-or-n-p
			    (cond
			     ((null decoders)
			      (if (= encode 1)
				  (format "Uuencode %s? " (car rfiles))
				(format "Uuencode %d file%s? "
					encode (dired-plural-s encode))))
			     ((zerop encode)
			      (if (= decode 1)
				  (format "Uudecode %s? " (car rfiles))
				(format "Uudecode %d file%s? "
					decode (dired-plural-s decode))))
			     (t
			      (format "Uudecode %d and uuencode %d file%s? "
				      decode encode (dired-plural-s encode)))))
	 (setq arg 0))
     (list arg files decoders)))
  (let ((dired-uu-files-to-decode to-decode)
	out-file)
    (if (not (zerop arg))  
	(dired-create-files
	 'dired-uucode-file
	 "Uuencode or Uudecode"
	 files
	 (function
	  (lambda (fn)
	    (if (setq out-file (assoc fn dired-uu-files-to-decode))
		(cdr out-file)
	      (concat fn ".uu"))))
	 dired-keep-marker-uucode nil t))))

;;; end of dired-uu.el
