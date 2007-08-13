;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-l19.11.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  efs support for XEemacs, versions 19.11, and later.
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Tue Aug  2 17:40:32 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 18:34:33 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'efs-l19\.11)
(require 'efs-cu)
(require 'default-dir)
(require 'efs-ovwrt)

(defconst efs-l19\.11-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;; Functions requiring special defs. for these lemacs versions.

(defun efs-abbreviate-file-name (filename &optional hack-homedir)
  ;; lucid emacs version of abbreviate-file-name for remote files.
  (let (file-name-handler-alist)
    (if (and hack-homedir (efs-ftp-path filename))
	;; Do replacements from directory-abbrev-alist
	(apply 'efs-unexpand-parsed-filename
	       (efs-ftp-path (abbreviate-file-name filename nil)))
      (abbreviate-file-name filename hack-homedir))))

(defun efs-relativize-filename (file &optional dir new)
  "Abbreviate the given filename relative to DIR .
If DIR is nil, use the value of `default-directory'. If the
optional parameter NEW is given and the non-directory parts match, only return
the directory part of the file."
  (let* ((dir (or dir default-directory))
	 (dlen (length dir))
	 (result file))
    (and (> (length file) dlen)
	 (string-equal (substring file 0 dlen) dir)
	 (setq result (substring file dlen)))
    (and new
	 (string-equal (file-name-nondirectory result)
		       (file-name-nondirectory new))
	 (or (setq result (file-name-directory result))
	     (setq result "./")))
    (abbreviate-file-name result t)))

(defun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because Lucid and FSF do this differently.
  (setq buffer-file-name filename)
  (if (and efs-compute-remote-buffer-file-truename
	   (memq (efs-host-type (car (efs-ftp-path filename)))
		 efs-unix-host-types))
      (compute-buffer-file-truename)
    (setq buffer-file-truename filename)))

;; Do we need to do anything about compute-buffer-file-truename, or
;; will the handler for file-truename handle this automatically?  I suppose
;; that efs-compute-remote-buffer-file-truename should really apply to
;; compute-buffer-file-truename, and not file-truename, but then we would
;; have to do deal with the fact that this function doesn't exist in GNU Emacs.

;; Only Lucid Emacs has this function.  Why do we need both this and
;; set-visited-file-modtime?

(defun efs-set-buffer-modtime (buffer &optional time)
  ;; For buffers visiting remote files, set the buffer modtime.
  (or time
      (progn
	(setq time
	      (let* ((file (save-excursion
			     (set-buffer buffer) buffer-file-name))
		     (parsed (efs-ftp-path file)))
		(efs-get-file-mdtm (car parsed) (nth 1 parsed)
				   (nth 2 parsed) file)))
	(if time
	    (setq time (cons (car time) (nth 1 time)))
	  (setq time '(0 . 0)))))
  (let (file-name-handler-alist)
    (set-buffer-modtime buffer time)))

;;; Need to add access to the file-name-handler-alist to these functions.

(defun efs-l19\.11-set-buffer-modtime (buffer &optional time)
  "Documented as original"
  (let ((handler (save-excursion
		   (set-buffer buffer)
		   (and buffer-file-name
			(find-file-name-handler buffer-file-name
						'set-buffer-modtime)))))
    (if handler
	(funcall handler 'set-buffer-modtime buffer time)
      (let (file-name-handler-alist)
	(efs-real-set-buffer-modtime buffer time)))))

(efs-overwrite-fn "efs" 'set-buffer-modtime 'efs-l19\.11-set-buffer-modtime)

(defun efs-l19\.11-backup-buffer ()
  "Documented as original"
  (if buffer-file-name
      (let ((handler (find-file-name-handler buffer-file-name 'backup-buffer)))
	(if handler
	    (funcall handler 'backup-buffer)
	  (let (file-name-handler-alist)
	    (efs-real-backup-buffer))))))

(efs-overwrite-fn "efs" 'backup-buffer 'efs-l19\.11-backup-buffer)

(defun efs-l19\.11-create-file-buffer (file)
  "Documented as original"
  (let ((handler (find-file-name-handler file 'create-file-buffer)))
    (if handler
	(funcall handler 'create-file-buffer file)
      (let (file-name-handler-alist)
	(efs-real-create-file-buffer file)))))

(efs-overwrite-fn "efs" 'create-file-buffer 'efs-l19\.11-create-file-buffer)

(defun efs-l19\.11-abbreviate-file-name (filename &optional hack-homedir)
  "Documented as original"
  (let ((handler (find-file-name-handler filename 'abbreviate-file-name)))
    (if handler
	(funcall handler 'abbreviate-file-name filename hack-homedir)
      (let (file-name-handler-alist)
	(efs-real-abbreviate-file-name filename hack-homedir)))))

(efs-overwrite-fn "efs" 'abbreviate-file-name
		  'efs-l19\.11-abbreviate-file-name)

(defun efs-l19\.11-recover-file (file)
  "Documented as original"
  (interactive
   (let ((prompt-file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and prompt-file
	  (setq file-name (file-name-nondirectory prompt-file)
		file-dir (file-name-directory prompt-file)))
     (list (read-file-name "Recover file: "
			   file-dir nil nil file-name))))
  (let* ((file (expand-file-name file))
	 (handler (or (find-file-name-handler file 'recover-file)
		      (find-file-name-handler 
		       (let ((buffer-file-name file))
			 (make-auto-save-file-name))
		       'recover-file))))
    (if handler
	(funcall handler 'recover-file file)
      (efs-real-recover-file file))))

(efs-overwrite-fn "efs" 'recover-file 'efs-l19\.11-recover-file)

(defun efs-l19\.11-substitute-in-file-name (filename)
  "Documented as original."
  (let ((handler (find-file-name-handler filename 'substitute-in-file-name)))
    (if handler
	(funcall handler 'substitute-in-file-name filename)
      (let (file-name-handler-alist)
	(efs-real-substitute-in-file-name filename)))))

(efs-overwrite-fn "efs" 'substitute-in-file-name
		  'efs-l19\.11-substitute-in-file-name)

;;; For the file-name-handler-alist

(put 'set-buffer-modtime 'efs 'efs-set-buffer-modtime)

;;; end of efs-l19.11.el
