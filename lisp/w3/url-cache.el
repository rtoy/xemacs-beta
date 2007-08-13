;;; url-cache.el --- Uniform Resource Locator retrieval tool
;; Author: wmperry
;; Created: 1997/02/20 15:33:47
;; Version: 1.3
;; Keywords: comm, data, processes, hypermedia

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
(require 'md5)

;; Cache manager
(defun url-cache-file-writable-p (file)
  "Follows the documentation of file-writable-p, unlike file-writable-p."
  (and (file-writable-p file)
       (if (file-exists-p file)
           (not (file-directory-p file))
         (file-directory-p (file-name-directory file)))))
                
(defun url-prepare-cache-for-file (file)
  "Makes it possible to cache data in FILE.
Creates any necessary parent directories, deleting any non-directory files
that would stop this.  Returns nil if parent directories can not be
created.  If FILE already exists as a non-directory, it changes
permissions of FILE or deletes FILE to make it possible to write a new
version of FILE.  Returns nil if this can not be done.  Returns nil if
FILE already exists as a directory.  Otherwise, returns t, indicating that
FILE can be created or overwritten."

  ;; COMMENT: We don't delete directories because that requires
  ;; recursively deleting the directories's contents, which might
  ;; eliminate a substantial portion of the cache.

  (cond
   ((url-cache-file-writable-p file)
    t)
   ((file-directory-p file)
    nil)
   (t
    (catch 'upcff-tag
      (let ((dir (file-name-directory file))
            dir-parent dir-last-component)
        (if (string-equal dir file)
            ;; *** Should I have a warning here?
            ;; FILE must match a pattern like /foo/bar/, indicating it is a
            ;; name only suitable for a directory.  So presume we won't be
            ;; able to overwrite FILE and return nil.
            (throw 'upcff-tag nil))
        
        ;; Make sure the containing directory exists, or throw a failure
        ;; if we can't create it.
        (if (file-directory-p dir)
            nil
          (or (fboundp 'make-directory)
              (throw 'upcff-tag nil))
          (make-directory dir t)
          ;; make-directory silently fails if there is an obstacle, so
          ;; we must verify its results.
          (if (file-directory-p dir)
              nil
            ;; Look at prefixes of the path to find the obstacle that is
            ;; stopping us from making the directory.  Unfortunately, there
            ;; is no portable function in Emacs to find the parent directory
            ;; of a *directory*.  So this code may not work on VMS.
            (while (progn
                     (if (eq ?/ (aref dir (1- (length dir))))
                         (setq dir (substring dir 0 -1))
                       ;; Maybe we're on VMS where the syntax is different.
                       (throw 'upcff-tag nil))
                     (setq dir-parent (file-name-directory dir))
                     (not (file-directory-p dir-parent)))
              (setq dir dir-parent))
            ;; We have found the longest path prefix that exists as a
            ;; directory.  Deal with any obstacles in this directory.
            (if (file-exists-p dir)
                (condition-case nil
                    (delete-file dir)
                  (error (throw 'upcff-tag nil))))
            (if (file-exists-p dir)
                (throw 'upcff-tag nil))
            ;; Try making the directory again.
            (setq dir (file-name-directory file))
            (make-directory dir t)
            (or (file-directory-p dir)
                (throw 'upcff-tag nil))))

        ;; The containing directory exists.  Let's see if there is
        ;; something in the way in this directory.
        (if (url-cache-file-writable-p file)
            (throw 'upcff-tag t)
          (condition-case nil
              (delete-file file)
            (error (throw 'upcff-tag nil))))

        ;; The return value, if we get this far.
        (url-cache-file-writable-p file))))))

(defvar url-cache-ignored-protocols
  '("www" "about" "https" "mailto")
  "*A list of protocols that we should never cache.")

(defun url-cache-cachable-p (obj)
  ;; return t iff the current buffer is cachable
  (cond
   ((null obj)				; Something horribly confused
    nil)
   ((member (url-type obj) url-cache-ignored-protocols)
    ;; We have been told to ignore this type of object
    nil)
   ((and (member (url-type obj) '("file" "ftp")) (not (url-host obj)))
    ;; We never want to cache local files... what's the point?
    nil)
   ((member (url-type obj) '("http" "https"))
    (let* ((status (cdr-safe (assoc "status" url-current-mime-headers)))
	   (class (if status (/ status 100) 0)))
      (case class
	(2				; Various 'OK' statuses
	 (memq status '(200)))
	(otherwise nil))))
   (t
    nil)))

;;;###autoload
(defun url-store-in-cache (&optional buff)
  "Store buffer BUFF in the cache"
  (if (and buff (get-buffer buff))
      nil
    (save-excursion
      (and buff (set-buffer buff))
      (if (not (url-cache-cachable-p url-current-object))
	  nil
	(let* ((fname (url-create-cached-filename (url-view-url t)))
	       (fname-hdr (concat fname ".hdr"))
	       (info (mapcar (function (lambda (var)
					 (cons (symbol-name var)
					       (symbol-value var))))
			     '( url-current-content-length
				url-current-object
				url-current-isindex
				url-current-mime-encoding
				url-current-mime-headers
				url-current-mime-type
				))))
	  (cond ((and (url-prepare-cache-for-file fname)
		      (url-prepare-cache-for-file fname-hdr))
		 (write-region (point-min) (point-max) fname nil 5)
		 (set-buffer (get-buffer-create " *cache-tmp*"))
		 (erase-buffer)
		 (insert "(setq ")
		 (mapcar
		  (function
		   (lambda (x)
		     (insert (car x) " "
			     (cond ((null (setq x (cdr x))) "nil")
				   ((stringp x) (prin1-to-string x))
				   ((listp x) (concat "'" (prin1-to-string x)))
				   ((vectorp x) (prin1-to-string x))
				   ((numberp x) (int-to-string x))
				   (t "'???")) "\n")))
		  info)
		 (insert ")\n")
		 (write-region (point-min) (point-max) fname-hdr nil 5))))))))
	
	     
;;;###autoload
(defun url-is-cached (url)
  "Return non-nil if the URL is cached."
  (let* ((fname (url-create-cached-filename url))
	 (attribs (file-attributes fname)))
    (and fname				; got a filename
	 (file-exists-p fname)		; file exists
	 (not (eq (nth 0 attribs) t))	; Its not a directory
	 (nth 5 attribs))))		; Can get last mod-time
    
(defun url-create-cached-filename-using-md5 (url)
  (if url
      (expand-file-name (md5 url)
			(concat url-temporary-directory "/"
				(user-real-login-name)))))

;;;###autoload
(defun url-create-cached-filename (url)
  "Return a filename in the local cache for URL"
  (if url
      (let* ((url url)
	     (urlobj (if (vectorp url)
			 url
		       (url-generic-parse-url url)))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (nreverse
		      (delq nil
			    (mm-string-to-tokens
			     (or hostname "localhost") ?.))))))
	     (fname    (url-filename urlobj)))
	(if (and fname (/= (length fname) 0) (= (aref fname 0) ?/))
	    (setq fname (substring fname 1 nil)))
	(if fname
	    (let ((slash nil))
	      (setq fname
		    (mapconcat
		     (function
		      (lambda (x)
			(cond
			 ((and (= ?/ x) slash)
			  (setq slash nil)
			  "%2F")
			 ((= ?/ x)
			  (setq slash t)
			  "/")
			 (t
			  (setq slash nil)
			  (char-to-string x))))) fname ""))))

	(if (and fname (memq system-type '(ms-windows ms-dos windows-nt))
		 (string-match "\\([A-Za-z]\\):[/\\]" fname))
	    (setq fname (concat (url-match fname 1) "/"
				(substring fname (match-end 0)))))
	
	(setq fname (and fname
			 (mapconcat
			  (function (lambda (x)
				      (if (= x ?~) "" (char-to-string x))))
			  fname ""))
	      fname (cond
		     ((null fname) nil)
		     ((or (string= "" fname) (string= "/" fname))
		      url-directory-index-file)
		     ((= (string-to-char fname) ?/)
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			(substring fname 1 nil)))
		     (t
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			fname))))

	;; Honor hideous 8.3 filename limitations on dos and windows
	;; we don't have to worry about this in Windows NT/95 (or OS/2?)
	(if (and fname (memq system-type '(ms-windows ms-dos)))
	    (let ((base (url-file-extension fname t))
		  (ext  (url-file-extension fname nil)))
	      (setq fname (concat (substring base 0 (min 8 (length base)))
				  (substring ext  0 (min 4 (length ext)))))
	      (setq host-components
		    (mapcar
		     (function
		      (lambda (x)
			(if (> (length x) 8)
			    (concat 
			     (substring x 0 8) "."
			     (substring x 8 (min (length x) 11)))
			  x)))
		     host-components))))

	(and fname
	     (expand-file-name fname
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-temporary-directory))))))

;;;###autoload
(defun url-extract-from-cache (fnam)
  "Extract FNAM from the local disk cache"
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (setq url-current-mime-viewer nil)
  (insert-file-contents-literally fnam)
  (load (concat (if (memq system-type '(ms-windows ms-dos os2))
		    (url-file-extension fnam t)
		  fnam) ".hdr") t t)) 

;;;###autoload
(defun url-cache-expired (url mod)
  "Return t iff a cached file has expired."
  (if (not (string-match url-nonrelative-link url))
      t
    (let* ((urlobj (url-generic-parse-url url))
	   (type (url-type urlobj)))
      (cond
       (url-standalone-mode
	(not (file-exists-p (url-create-cached-filename urlobj))))
       ((string= type "http")
	(if (not url-standalone-mode) t
	  (not (file-exists-p (url-create-cached-filename urlobj)))))
       ((not (fboundp 'current-time))
	t)
       ((member type '("file" "ftp"))
	(if (or (equal mod '(0 0)) (not mod))
	      (return t)
	    (or (> (nth 0 mod) (nth 0 (current-time)))
		(> (nth 1 mod) (nth 1 (current-time))))))
       (t nil)))))

(provide 'url-cache)
