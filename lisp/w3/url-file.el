;;; url-file.el --- File retrieval code
;; Author: wmperry
;; Created: 1997/01/24 14:32:50
;; Version: 1.9
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993-1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url-vars)
(require 'mule-sysdp)
(require 'url-parse)

(defun url-insert-possibly-compressed-file (fname &rest args)
  ;; Insert a file into a buffer, checking for compressed versions.
  (let ((compressed nil)
	;;
	;; F*** *U** **C* ***K!!!
	;; We cannot just use insert-file-contents-literally here, because
	;; then we would lose big time with ange-ftp.  *sigh*
	(crypt-encoding-alist nil)
	(jka-compr-compression-info-list nil)
	(jam-zcat-filename-list nil)
	(file-coding-system-for-read mule-no-coding-system)
	(coding-system-for-read mule-no-coding-system))
    (setq compressed 
	  (cond
	   ((file-exists-p fname) nil)
	   ((file-exists-p (concat fname ".Z"))
	    (setq fname (concat fname ".Z")))
	   ((file-exists-p (concat fname ".gz"))
	    (setq fname (concat fname ".gz")))
	   ((file-exists-p (concat fname ".z"))
	    (setq fname (concat fname ".z")))
	   (t
	    (error "File not found %s" fname))))
    (if (or (not compressed) url-inhibit-uncompression)
	(apply 'insert-file-contents fname args)
      (let* ((extn (url-file-extension fname))
	     (code (cdr-safe (assoc extn url-uncompressor-alist)))
	     (decoder (cdr-safe (assoc code mm-content-transfer-encodings))))
	(cond
	 ((null decoder) 
	  (apply 'insert-file-contents fname args))
	 ((stringp decoder)
	  (apply 'insert-file-contents fname args)
	  (message "Decoding...")
	  (call-process-region (point-min) (point-max) decoder t t nil)
	  (message "Decoding... done."))
	 ((listp decoder)
	  (apply 'call-process-region (point-min) (point-max)
		 (car decoder) t t t (cdr decoder)))
	 ((and (symbolp decoder) (fboundp decoder))
	  (apply 'insert-file-contents fname args)
	  (message "Decoding...")
	  (funcall decoder (point-min) (point-max))
	  (message "Decoding... done."))
	 (t
	  (error "Malformed entry for %s in `mm-content-transfer-encodings'"
		 code))))))
  (set-buffer-modified-p nil))

(defun url-format-directory (dir)
  ;; Format the files in DIR into hypertext
  (let ((files (directory-files dir nil)) file
	div attr mod-time size typ title)
    (if (and url-directory-index-file
	     (file-exists-p (expand-file-name url-directory-index-file dir))
	     (file-readable-p (expand-file-name url-directory-index-file dir)))
	(save-excursion
	  (set-buffer url-working-buffer)
	  (erase-buffer)
	  (insert-file-contents-literally
	   (expand-file-name url-directory-index-file dir)))
      (save-excursion
	(if (string-match "/\\([^/]+\\)/$" dir)
	    (setq title (concat ".../" (url-match dir 1) "/"))
	  (setq title "/"))
	(setq div (1- (length files)))
	(set-buffer url-working-buffer)
	(erase-buffer)
	(insert "<html>\n"
		" <head>\n"
		"  <title>" title "</title>\n"
		" </head>\n"
		" <body>\n"
		"  <div>\n"
		"   <h1 align=center> Index of " title "</h1>\n"
		"   <pre>\n"
		"       Name                     Last modified                Size\n</pre>"
		"<hr>\n   <pre>\n")
	(while files
	  (url-lazy-message "Building directory list... (%d%%)"
			    (/ (* 100 (- div (length files))) div))
	  (setq file (expand-file-name (car files) dir)
		attr (file-attributes file)
		file (car files)
		mod-time (nth 5 attr)
		size (nth 7 attr)
		typ (or (mm-extension-to-mime (url-file-extension file)) ""))
	  (setq file (url-hexify-string file))
	  (if (equal '(0 0) mod-time) ; Set to null if unknown or
	      (setq mod-time "Unknown                 ")
	    (setq mod-time (current-time-string mod-time)))
	  (if (or (equal size 0) (equal size -1) (null size))
	      (setq size "   -")
	    (setq size
		  (cond
		   ((< size 1024) (concat "   " "1K"))
		   ((< size 1048576) (concat "   "
					     (int-to-string
					      (max 1 (/ size 1024))) "K"))
		   (t
		    (let* ((megs (max 1 (/ size 1048576)))
			   (kilo (/ (- size (* megs 1048576)) 1024)))
		      (concat "   "  (int-to-string megs)
			      (if (> kilo 0)
				  (concat "." (int-to-string kilo))
				"") "M"))))))
	  (cond
	   ((or (equal "." (car files))
		(equal "/.." (car files)))
	    nil)
	   ((equal ".." (car files))
	    (if (not (= ?/ (aref file (1- (length file)))))
		(setq file (concat file "/"))))
	   ((stringp (nth 0 attr))	; Symbolic link handling
	    (insert "[LNK] <a href=\"./" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((nth 0 attr)		; Directory handling
	    (insert "[DIR] <a href=\"./" file "/\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "image" typ)
	    (insert "[IMG] <a href=\"./" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "application" typ)
	    (insert "[APP] <a href=\"./" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "text" typ)
	    (insert "[TXT] <a href=\"./" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   (t
	    (insert "[UNK] <a href=\"./" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n")))
	  (setq files (cdr files)))
	(insert "   </pre>\n"
		"  </div>\n"
		" </body>\n"
		"</html>\n"
		"<!-- Automatically generated by URL v" url-version
		" -->\n")))))

(defun url-host-is-local-p (host)
  "Return t iff HOST references our local machine."
  (let ((case-fold-search t))
    (or
     (null host)
     (string= "" host)
     (equal (downcase host) (downcase (system-name)))
     (and (string-match "^localhost$" host) t)
     (and (not (string-match (regexp-quote ".") host))
	  (equal (downcase host) (if (string-match (regexp-quote ".")
						   (system-name))
				     (substring (system-name) 0
						(match-beginning 0))
				   (system-name)))))))
     
(defun url-file (url)
  ;; Find a file
  (let* ((urlobj (url-generic-parse-url url))
	 (user (url-user urlobj))
	 (pass (url-password urlobj))
	 (site (url-host urlobj))
	 (file (url-unhex-string (url-filename urlobj)))
	 (dest (url-target urlobj))
	 (filename (if (or user (not (url-host-is-local-p site)))
		       (concat "/" (or user "anonymous") "@" site ":" file)
		     file)))

    (if (and file (url-host-is-local-p site)
	     (memq system-type '(ms-windows ms-dos windows-nt os2)))
	(let ((x (1- (length file)))
	      (y 0))
	  (while (<= y x)
	    (if (= (aref file y) ?\\ )
		(aset file y ?/))
	    (setq y (1+ y)))))

    (url-clear-tmp-buffer)
    (and user pass
	 (cond
	  ((featurep 'ange-ftp)
	   (ange-ftp-set-passwd site user pass))
	  ((or (featurep 'efs) (featurep 'efs-auto))
	   (efs-set-passwd site user pass))
	  (t
	   nil)))
    (cond
     ((file-directory-p filename)
      (if url-use-hypertext-dired
	  (progn
	    (if (string-match "/$" filename)
		nil
	      (setq filename (concat filename "/")))
	    (if (string-match "/$" file)
		nil
	      (setq file (concat file "/")))
	    (url-set-filename urlobj file)
	    (url-format-directory filename))
	(progn
	  (if (get-buffer url-working-buffer)
	      (kill-buffer url-working-buffer))
	  (find-file filename))))
     ((and (boundp 'w3-dump-to-disk) (symbol-value 'w3-dump-to-disk))
      (cond
       ((file-exists-p filename) nil)
       ((file-exists-p (concat filename ".Z"))
	(setq filename (concat filename ".Z")))
       ((file-exists-p (concat filename ".gz"))
	(setq filename (concat filename ".gz")))
       ((file-exists-p (concat filename ".z"))
	(setq filename (concat filename ".z")))
       (t
	(error "File not found %s" filename)))
      (cond
       ((url-host-is-local-p site)
	(copy-file
	 filename 
	 (read-file-name "Save to: " nil (url-basepath filename t)) t))
       ((featurep 'ange-ftp)
	(ange-ftp-copy-file-internal
	 filename
	 (expand-file-name
	  (read-file-name "Save to: " nil (url-basepath filename t))) t
	 nil t nil t))
       ((or (featurep 'efs) (featurep 'efs-auto))
	(let ((new (expand-file-name
		    (read-file-name "Save to: " nil
				    (url-basepath filename t)))))
	  (efs-copy-file-internal filename (efs-ftp-path filename)
				  new (efs-ftp-path new)
				  t nil 0 nil 0 nil)))
       (t (copy-file
	   filename 
	   (read-file-name "Save to: " nil (url-basepath filename t)) t)))
      (if (get-buffer url-working-buffer)
	  (kill-buffer url-working-buffer)))
     (t
      (let ((viewer (mm-mime-info
		     (mm-extension-to-mime (url-file-extension file))))
	    (errobj nil))
	(if (or url-source		; Need it in a buffer
		(and (symbolp viewer)
		     (not (eq viewer 'w3-default-local-file)))
		(stringp viewer))
	    (condition-case errobj
		(url-insert-possibly-compressed-file filename t)
	      (error
	       (url-save-error errobj)
	       (url-retrieve (concat "www://error/nofile/" file))))))))
    (setq url-current-type (if site "ftp" "file")
	  url-current-object urlobj
	  url-find-this-link dest
	  url-current-user user
	  url-current-server site
	  url-current-mime-type (mm-extension-to-mime
				 (url-file-extension file))
	  url-current-file file)))

(fset 'url-ftp 'url-file)

(provide 'url-file)
