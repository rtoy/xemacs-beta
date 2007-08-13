;;; url-file.el --- File retrieval code
;; Author: wmperry
;; Created: 1997/02/10 16:16:46
;; Version: 1.13
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
	   ((file-exists-p fname)
	    (if (string-match "\\.\\(z\\|gz\\|Z\\)$" fname)
		(case (intern (match-string 1 fname))
		  ((z gz)
		   (setq url-current-mime-headers (cons
						   (cons
						    "content-transfer-encoding"
						    "gzip")
						   url-current-mime-headers)))
		  (Z
		   (setq url-current-mime-headers (cons
						   (cons
						    "content-transfer-encoding"
						    "compress")
						   url-current-mime-headers))))
	      nil))
	   ((file-exists-p (concat fname ".Z"))
	    (setq fname (concat fname ".Z")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "compress")
						 url-current-mime-headers)))
	   ((file-exists-p (concat fname ".gz"))
	    (setq fname (concat fname ".gz")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "gzip")
						 url-current-mime-headers)))
	   ((file-exists-p (concat fname ".z"))
	    (setq fname (concat fname ".z")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "gzip")
						 url-current-mime-headers)))
	   (t
	    (error "File not found %s" fname))))
    (apply 'insert-file-contents fname args)
    (set-buffer-modified-p nil)))

(defvar url-dired-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'url-dired-find-file)
    (if url-running-xemacs
	(define-key map [button2] 'url-dired-find-file-mouse)
      (define-key map [mouse-2] 'url-dired-find-file-mouse))
    map)
  "Keymap used when browsing directories.")

(defvar url-dired-minor-mode nil
  "Whether we are in url-dired-minor-mode")

(make-variable-buffer-local 'url-dired-minor-mode)

(defun url-dired-find-file ()
  "In dired, visit the file or directory named on this line, using Emacs-W3."
  (interactive)
  (w3-open-local (dired-get-filename)))

(defun url-dired-find-file-mouse (event)
  "In dired, visit the file or directory name you click on, using Emacs-W3."
  (interactive "@e")
    (if (event-point event)
	(progn
	  (goto-char (event-point event))
	  (url-dired-find-file))))

(defun url-dired-minor-mode (&optional arg)
  "Minor mode for directory browsing with Emacs-W3."
  (interactive "P")
  (cond
   ((null arg)
    (setq url-dired-minor-mode (not url-dired-minor-mode)))
   ((equal 0 arg)
    (setq url-dired-minor-mode nil))
   (t
    (setq url-dired-minor-mode t))))

(add-minor-mode 'url-dired-minor-mode " URL" url-dired-minor-mode-map)

(defun url-format-directory (dir)
  ;; Format the files in DIR into hypertext
  (if (and url-directory-index-file
	   (file-exists-p (expand-file-name url-directory-index-file dir))
	   (file-readable-p (expand-file-name url-directory-index-file dir)))
      (save-excursion
	(set-buffer url-working-buffer)
	(erase-buffer)
	(insert-file-contents-literally
	 (expand-file-name url-directory-index-file dir)))
    (kill-buffer (current-buffer))
    (find-file dir)
    (url-dired-minor-mode t)))
;   (let ((files (directory-files dir nil)) file
;	  div attr mod-time size typ title desc)
;      (save-excursion
;	(if (string-match "/\\([^/]+\\)/$" dir)
;	    (setq title (concat ".../" (url-match dir 1) "/"))
;	  (setq title "/"))
;	(setq div (1- (length files)))
;	(set-buffer url-working-buffer)
;	(erase-buffer)
;	(insert "<html>\n"
;		" <head>\n"
;		"  <title>" title "</title>\n"
;		" </head>\n"
;		" <body>\n"
;		"   <h1 align=center> Index of " title "</h1>\n"
;		"   <table border=0>\n"
;		"    <tr><th>Name<th>Last Modified<th>Size</tr>\n"
;		"    <tr><td colspan=3><hr></tr>\n")
;	(while files
;	  (url-lazy-message "Building directory list... (%d%%)"
;			    (/ (* 100 (- div (length files))) div))
;	  (setq file (expand-file-name (car files) dir)
;		attr (file-attributes file)
;		file (car files)
;		mod-time (nth 5 attr)
;		size (nth 7 attr)
;		typ (or (mm-extension-to-mime (url-file-extension file)) ""))
;	  (setq file (url-hexify-string file))
;	  (if (equal '(0 0) mod-time)	; Set to null if unknown or
;	      (setq mod-time "Unknown")
;	    (setq mod-time (current-time-string mod-time)))
;	  (if (or (equal size 0) (equal size -1) (null size))
;	      (setq size "-")
;	    (setq size
;		  (cond
;		   ((< size 1024) "1K")
;		   ((< size 1048576) (concat (int-to-string
;					      (max 1 (/ size 1024))) "K"))
;		   (t
;		    (let* ((megs (max 1 (/ size 1048576)))
;			   (kilo (/ (- size (* megs 1048576)) 1024)))
;		      (concat (int-to-string megs)
;			      (if (> kilo 0)
;				  (concat "." (int-to-string kilo))
;				"") "M"))))))
;	  (cond
;	   ((or (equal "." (car files))
;		(equal "/.." (car files)))
;	    (setq desc nil))
;	   ((equal ".." (car files))
;	    (if (not (= ?/ (aref file (1- (length file)))))
;		(setq file (concat file "/"))))
;	   ((stringp (nth 0 attr))	; Symbolic link handling
;	    (setq desc "[LNK]"))
;	   ((nth 0 attr)		; Directory handling
;	    (setq desc "[DIR]"))
;	   ((string-match "image" typ)
;	    (setq desc "[IMG]"))
;	   ((string-match "application" typ)
;	    (setq desc "[APP]"))
;	   ((string-match "text" typ)
;	    (setq desc "[TXT]"))
;	   ((auto-save-file-name-p (car files))
;	    (setq desc "[BAK]"))
;	   (t
;	    (setq desc "[UNK]")))
;	  (if desc
;	      (insert "<tr><td>" desc " <a href=\"./" file "\">" (car files)
;		      "</a><td>" mod-time "<td><p align=right>" size
;		      "</tr>\n"))
;	  (setq files (cdr files)))
;	(insert "   </table>\n"
;		" </body>\n"
;		"</html>\n"
;		"<!-- Automatically generated by URL v" url-version
;		" -->\n")))

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
      (if (string-match "/$" filename)
	  nil
	(setq filename (concat filename "/")))
      (if (string-match "/$" file)
	  nil
	(setq file (concat file "/")))
      (url-set-filename urlobj file)
      (url-format-directory filename))
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
