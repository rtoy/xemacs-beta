;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-x19.15.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  efs support for XEmacs, versions 19.15, and later.
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Tue Aug  2 17:40:32 1994 by sandy on ibm550
;; Modified:     by Mike Sperber
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'efs-x19\.15)
(require 'efs-cu)
(require 'default-dir)
(require 'efs-ovwrt)

(defconst efs-x19\.15-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;; Functions requiring special defs. for these XEmacs versions.

(defun efs-abbreviate-file-name (filename &optional hack-homedir)
  ;; XEmacs version of abbreviate-file-name for remote files.
  (let (file-name-handler-alist)
    (if (and hack-homedir (efs-ftp-path filename))
	;; Do replacements from directory-abbrev-alist
	(apply 'efs-unexpand-parsed-filename
	       (efs-ftp-path (abbreviate-file-name filename nil)))
      (abbreviate-file-name filename hack-homedir))))

(defun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because XEmacs and FSF do this differently.
  (setq buffer-file-name filename)
  (if (and efs-compute-remote-buffer-file-truename
	   (memq (efs-host-type (car (efs-ftp-path filename)))
		 efs-unix-host-types))
      (compute-buffer-file-truename)
    (setq buffer-file-truename filename)))

;; Only XEmacs has this function.  Why do we need both this and
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

;;; For the file-name-handler-alist

(put 'set-buffer-modtime 'efs 'efs-set-buffer-modtime)

;;; end of efs-x19.15.el
