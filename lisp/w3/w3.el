;;; w3.el --- Main functions for emacs-w3 on all platforms/versions
;; Author: wmperry
;; Created: 1996/08/19 03:30:47
;; Version: 1.22
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is a major mode for browsing documents written in Hypertext Markup ;;;
;;; Language (HTML).  These documents are typicallly part of the World Wide ;;;
;;; Web (WWW), a project to create a global information net in hypertext    ;;;
;;; format.				                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; first start by making sure the load path is properly set.  This code
;;; is mostly taken from calc-2.02b
;;;
;;; this allows you to put the following in your .emacs file, instead of
;;; having to know what the load-path for the w3 files is.
;;;
;;;     (autoload 'w3 "w3/w3" "WWW Browser" t)

;;; If w3 files exist on the load-path, we're all set.
(let ((name (and (fboundp 'w3)
		 (eq (car-safe (symbol-function 'w3)) 'autoload)
		 (nth 1 (symbol-function 'w3))))
      (p load-path))
  (while (and p (not (file-exists-p
		      (expand-file-name "w3-vars.elc" (car p)))))
    (setq p (cdr p)))
  (or p
;;; If w3 is autoloaded using a path name, look there for w3 files.
;;; This works for both relative ("w3/w3.elc") and absolute paths.
      (and name (file-name-directory name)
	   (let ((p2 load-path)
		 (name2 (concat (file-name-directory name)
				"w3-vars.elc")))
	     (while (and p2 (not (file-exists-p
				  (expand-file-name name2 (car p2)))))
	       (setq p2 (cdr p2)))
	     (if p2
		 (setq load-path (nconc load-path
					(list
					 (directory-file-name
					  (file-name-directory
					   (expand-file-name
					    name (car p2)))))))))))
  )


(load-library "w3-sysdp")
(or (featurep 'efs)
    (featurep 'efs-auto)
    (condition-case ()
	(require 'ange-ftp)
      (error nil)))

(require 'cl)
(require 'w3-vars)
(eval-and-compile
  (require 'w3-draw))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for printing out roman numerals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-decimal-to-roman (n)
  ;; Convert from decimal to roman numerals
  (let ((curmod 1000)
	(str "")
	(j 7)
	i2 k curcnt)
    (while (>= curmod 1)
      (if (>= n curmod)
	  (progn
	    (setq curcnt (/ n curmod)
		  n (- n (* curcnt curmod)))
	    (if (= 4 (% curcnt 5))
		(setq i2 (+ j (if (> curcnt 5) 1 0))
		      str (format "%s%c%c" str
				  (aref w3-roman-characters (1- j))
				  (aref w3-roman-characters i2)))
	      (progn
		(if (>= curcnt 5)
		    (setq str (format "%s%c" str (aref w3-roman-characters j))
			  curcnt (- curcnt 5)))
		(setq k 0)
		(while (< k curcnt)
		  (setq str (format "%s%c" str
				    (aref w3-roman-characters (1- j)))
			k (1+ k)))))))
      (setq curmod (/ curmod 10)
	    j (- j 2)))
    str))

(defun w3-decimal-to-alpha (n)
  ;; Convert from decimal to alphabetical (a, b, c, ..., aa, ab,...)
  (cond
   ((< n 1) (char-to-string ?Z))
   ((<= n 26) (char-to-string (+ ?A (1- n))))
   (t (concat (char-to-string (+ ?A (1- (/ n 27))))
	      (w3-decimal-to-alpha (% n 26))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for compatibility with XMosaic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse out the Mosaic documents-menu file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-docs-menu ()
  ;; Parse the Mosaic documents menu
  (let ((tmp-menu (append '((separator)) w3-starting-documents
			  '((separator))))
	real-menu x y name url)
    (if (or (not (file-exists-p w3-documents-menu-file))
	    (not (file-readable-p w3-documents-menu-file)))
	nil
      (save-excursion
	(set-buffer (get-buffer-create " *w3-temp*"))
	(erase-buffer)
	(insert-file-contents w3-documents-menu-file)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (not (looking-at "-+$"))
	      (setq x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    name (prog1
			     (buffer-substring x y)
			   (delete-region x (min (1+ y) (point-max))))
		    x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    url (prog1
			    (buffer-substring x y)
			  (delete-region x (min (1+ y) (point-max))))
		    tmp-menu (if (rassoc url tmp-menu) tmp-menu
			       (cons (cons name url) tmp-menu)))
	    (setq tmp-menu (cons '(separator) tmp-menu))
	    (delete-region (point-min) (min (1+ (progn (end-of-line)
						       (point)))
					    (point-max)))))
	(kill-buffer (current-buffer))))
    (if (equal (car (car tmp-menu)) "") (setq tmp-menu (cdr tmp-menu)))
    (while tmp-menu
      (setq real-menu (cons (if (equal 'separator (car (car tmp-menu)))
				"--------"
			      (vector (car (car tmp-menu))
				      (list 'w3-fetch
					    (if (listp (cdr (car tmp-menu)))
						(car (cdr (car tmp-menu)))
					      (cdr (car tmp-menu)))) t))
			    real-menu)
	    tmp-menu (cdr tmp-menu)))
    (setq w3-navigate-menu (append w3-navigate-menu real-menu
				   (list "-----")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to pass files off to external viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-start-viewer (fname cmd &optional view)
  "Start a subprocess, named FNAME, executing CMD
If third arg VIEW is non-nil, show the output in a buffer when
the subprocess exits."
  (if view (save-excursion
	     (set-buffer (get-buffer-create view))
	     (erase-buffer)))
  (let ((proc
	 (start-process fname view (or shell-file-name
				       (getenv "ESHELL")
				       (getenv "SHELL")
				       "/bin/sh") "-c" cmd)))
    proc))

(defun w3-viewer-filter (proc string)
  ;; A process filter for asynchronous external viewers
  (let ((buff (get-buffer-create (url-generate-new-buffer-name
				  (symbol-name
				   (read (nth 2 (process-command proc))))))))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (insert string)
      (set-process-buffer proc buff)
      (set-process-filter proc nil))))

(defun w3-viewer-sentinel (proc string)
  ;; Delete any temp files left from a viewer process.
  (let ((fname (process-name proc))
	(buffr (process-buffer proc))
	(status (process-exit-status proc)))
    (if buffr
	(w3-notify-when-ready buffr))
    (and (/= 0 status)
	 (funcall url-confirmation-func
		  (format "Viewer for %s failed... save to disk? " fname))
	 (copy-file fname (read-file-name "Save as: ") t))
    (if (and (file-exists-p fname)
	     (file-writable-p fname))
	(delete-file fname))))

(defun w3-notify-when-ready (buff)
  "Notify the user when BUFF is ready.
See the variable `w3-notify' for the different notification behaviors."
  (if (stringp buff) (setq buff (get-buffer buff)))
  (cond
   ((null buff) nil)
   ((eq w3-notify 'newframe)
    ;; Since we run asynchronously, perhaps while Emacs is waiting for input,
    ;; we must not leave a different buffer current.
    ;; We can't rely on the editor command loop to reselect
    ;; the selected window's buffer.
    (save-excursion
      (set-buffer buff)
      (make-frame)))
   ((eq w3-notify 'bully)
    (pop-to-buffer buff)
    (delete-other-windows))
   ((eq w3-notify 'semibully)
    (switch-to-buffer buff))
   ((eq w3-notify 'aggressive)
    (pop-to-buffer buff))
   ((eq w3-notify 'friendly)
    (display-buffer buff 'not-this-window))
   ((eq w3-notify 'polite)
    (beep)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   ((eq w3-notify 'quiet)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   (t (message ""))))

(defun w3-pass-to-viewer ()
  ;; Pass a w3 buffer to a viewer
  (set-buffer url-working-buffer)
  (let* ((info  url-current-mime-viewer) 	   ; All the MIME viewer info
	 (view (cdr-safe (assoc "viewer" info))) ; How to view this file
	 (url (url-view-url t))
	 (fmt  (cdr-safe (assoc "nametemplate" info)))) ; Template for name
    (cond
     (fmt nil)
     ((cdr-safe (assoc "type" info))
      (setq fmt (mm-type-to-file (cdr-safe (assoc "type" info))))
      (if fmt (setq fmt (concat "%s" (car fmt)))
	(setq fmt (concat "%s" (url-file-extension url-current-file))))))
    (if (null view)
	(setq view 'indented-text-mode))
    (cond
     ((symbolp view)
      (if (not (memq view '(w3-prepare-buffer w3-print w3-source
					      w3-default-local-file
					      mm-multipart-viewer)))
	  (let ((bufnam (url-generate-new-buffer-name
			 (file-name-nondirectory
			  (or url-current-file "Unknown")))))
	    (if (string= bufnam "")
		(setq bufnam (url-generate-new-buffer-name
			      (url-view-url t))))
	    (rename-buffer bufnam)
	    ;; Make the URL show in list-buffers output
	    (make-local-variable 'list-buffers-directory)
	    (setq list-buffers-directory (url-view-url t))
	    (set-buffer-modified-p nil)
	    (buffer-enable-undo)
	    (funcall view)
	    (w3-notify-when-ready bufnam))
	(funcall view)))
     ((stringp view)
      (let ((fname (url-generate-unique-filename fmt))
	    (proc nil)
	    (file-coding-system url-mule-no-coding-system))
	(if (url-file-directly-accessible-p (url-view-url t))
	    (make-symbolic-link url-current-file fname t)
	  (write-region (point-min) (point-max) fname))
	(if (get-buffer url-working-buffer)
	    (kill-buffer url-working-buffer))
	(setq view (mm-viewer-unescape view fname url))
	(message "Passing to viewer %s " view)
	(setq proc (w3-start-viewer fname view))
	(set-process-filter proc 'w3-viewer-filter)
	(set-process-sentinel proc 'w3-viewer-sentinel)))
     ((listp view)
      (set-buffer-modified-p nil)
      (buffer-enable-undo)
      (eval view))
     (t
      (message "Unknown viewer specified: %s" view)
      (w3-notify-when-ready url-working-buffer)))))

(defun w3-save-binary-file ()
  "Save a buffer to disk - this is used when `w3-dump-to-disk' is non-nil"
  ;; Ok, this is truly fucked.  In XEmacs, if you use the mouse to select
  ;; a URL that gets saved via this function, read-file-name will pop up a
  ;; dialog box for file selection.  For some reason which buffer we are in
  ;; gets royally screwed (even with save-excursions and the whole nine
  ;; yards).  SO, we just keep the old buffer name around and away we go.
  (let ((old-buff (current-buffer))
	(file (read-file-name "Filename to save as: "
			      (or mm-download-directory "~/")
			      (url-remove-compressed-extensions
			       (file-name-nondirectory (url-view-url t)))
			      nil
			      (url-remove-compressed-extensions
			       (file-name-nondirectory (url-view-url t)))))
	(require-final-newline nil))
    (set-buffer old-buff)
    (let ((mc-flag t)
	  (file-coding-system url-mule-no-coding-system))
      (write-region (point-min) (point-max) file))
    (kill-buffer (current-buffer))))

(defun w3-build-url (protocol)
  "Build a url for PROTOCOL, return it as a string"
  (interactive (list (cdr (assoc (completing-read
				  "Protocol: "
				  w3-acceptable-protocols-alist nil t)
				 w3-acceptable-protocols-alist))))
  (let (user host port file)
    (cond
     ((null protocol) (error "Protocol is unknown to me!"))
     ((string= protocol "news")
      (setq host (read-string "Enter news server name, or blank for default: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Newgroup name or Message-ID: ")))
     ((string= protocol "mailto") (setq file (read-string "E-mail address: ")))
     ((string= protocol "http")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!")))
     ((string= protocol "file")
      (if (funcall url-confirmation-func "Local file?")
	  (setq file (read-file-name "Local File: " nil nil t))
	(setq user (read-string "Login as user (blank=anonymous): ")
	      host (read-string "Remote machine name: "))
	(and (string= user "") (setq user "anonymous"))
	(and (string= host "") (error "Must specify a remote machine!"))
	(setq file (read-file-name "File: " (format "/%s@%s:" user host)
				   nil t)
	      file (substring file (length (format "/%s@%s:" user host))))))
     ((or (string= protocol "telnet")
	  (string= protocol "tn3270"))
      (setq user (read-string "Login as user (blank=none): ")
	    host (read-string "Remote machine name: ")
	    port (read-string "Port number (blank=23): "))
      (and (string= "" port) (setq port nil))
      (and (string= "" user) (setq user nil))
      (and (string= "" host) (error "Must specify a host machine!")))
     ((string= protocol "gopher")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!"))))
    (message "%s:%s%s"
	     protocol
	     (if (null host) "" (concat "//" host
					(if (null port) "" (concat ":" port))))
	     (if (= ?/ (string-to-char file)) file (concat "/" file)))))

;;;###autoload
(defun w3-open-local (fname)
  "Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document.  If it is a directory, and url-use-hypertext-dired
is non-nil, then an HTML directory listing is created on the fly.
Otherwise, dired-mode is used to visit the buffer."
  (interactive "FLocal file: ")
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch (concat "file:" fname)))

;;;###autoload
(defun w3-find-file (fname)
  "Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document.  If it is a directory, and url-use-hypertext-dired
is non-nil, then an HTML directory listing is created on the fly.
Otherwise, dired-mode is used to visit the buffer."
  (interactive "FLocal file: ")
  (w3-open-local fname))
 
;;;###autoload
(defun w3-fetch-other-frame (&optional url)
  "Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive (list (w3-read-url-with-default)))
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame)
	 (not (eq (device-type) 'tty)))
    (let ((frm (make-frame)))
      (select-frame frm)
      (delete-other-windows)
      (w3-fetch url)))
   (t (w3-fetch url))))

(defun w3-fetch-other-window (&optional url)
  "Attempt to follow the hypertext reference under point in a new window.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive (list (w3-read-url-with-default)))
  (split-window)
  (w3-fetch url))

(defun w3-url-completion-function (string predicate function)
  (if (not w3-setup-done) (w3-do-setup))
  (cond
   ((null function)
    (cond
     ((get 'url-gethash 'sysdep-defined-this)
      ;; Cheat!  If we know that these are the sysdep-defined version
      ;; of hashtables, they are an obarray.
      (try-completion string url-global-history-hash-table predicate))
     ((url-hashtablep url-global-history-hash-table)
      (let ((list nil))
	(url-maphash (function (lambda (key val)
				 (setq list (cons (cons (symbol-name key) val)
						  list))))
		     url-global-history-hash-table)
	(try-completion string (nreverse list) predicate)))
     (t nil)))
   ((eq function t)
    (cond
     ((get 'url-gethash 'sysdep-defined-this)
      ;; Cheat!  If we know that these are the sysdep-defined version
      ;; of hashtables, they are an obarray.
      (all-completions string url-global-history-hash-table predicate))
     ((url-hashtablep url-global-history-hash-table)
      (let ((stub (concat "^" (regexp-quote string)))
	    (retval nil))
	(url-maphash
	 (function
	  (lambda (url time)
	    (setq url (symbol-name url))
	    (if (string-match stub url)
		(setq retval (cons url retval)))))
	 url-global-history-hash-table)
	retval))
     (t nil)))
   ((eq function 'lambda)
    (and (url-hashtablep url-global-history-hash-table)
	 (url-gethash string url-global-history-hash-table)
	 t))))

(defun w3-read-url-with-default ()
  (url-do-setup)
  (let* ((completion-ignore-case t)
	 (default
	   (if (eq major-mode 'w3-mode)
	       (if (and current-prefix-arg (w3-view-this-url t))
		   (w3-view-this-url t)
		 (url-view-url t))
	     (url-get-url-at-point)))
	 (url nil))
    (if (not default)
	(setq default "http://www."))
    (setq url
	  (completing-read "URL: "  'w3-url-completion-function
			   nil nil default))
    (if (string= url "")
	(setq url (if (eq major-mode 'w3-mode)
		      (if (and current-prefix-arg (w3-view-this-url t))
			  (w3-view-this-url t)
			(url-view-url t))
		    (url-get-url-at-point))))
    url))

;;;###autoload
(defun w3-fetch (&optional url)
  "Retrieve a document over the World Wide Web.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The document should be specified by its fully specified
Uniform Resource Locator.  The document will be parsed, printed, or
passed to an external viewer as appropriate.  Variable
`mm-mime-info' specifies viewers for particular file types."
  (interactive (list (w3-read-url-with-default)))
  (if (not w3-setup-done) (w3-do-setup))
  (if (boundp 'w3-working-buffer)
      (setq w3-working-buffer url-working-buffer))
  (if (and (boundp 'command-line-args-left)
	   command-line-args-left
	   (string-match url-nonrelative-link (car command-line-args-left)))
      (setq url (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left)))
  (if (equal url "") (error "No document specified!"))
  ;; legal use for relative URLs ?
  (if (string-match "^www:[^/].*" url)
      (setq url (concat (file-name-directory url-current-file)
 			(substring url 4))))
  ;; In the common case, this is probably cheaper than searching.
  (while (= (string-to-char url) ? )
    (setq url (substring url 1)))
  (cond
   ((= (string-to-char url) ?#)
    (w3-relative-link url))
   ((or (and (interactive-p) current-prefix-arg) w3-dump-to-disk)
    (w3-download-url url))
   (t
    (let ((x (url-view-url t))
	  (lastbuf (current-buffer))
	  (buf (url-buffer-visiting url)))
      (and x (or (string= "file:nil" x) (string= "" x))
	   (setq x nil))
      (if (or (not buf)
	      (cond
	       ((not (equal (downcase (or url-request-method "GET")) "get")) t)
	       ((memq w3-reuse-buffers '(no never reload)) t)
	       ((memq w3-reuse-buffers '(yes reuse always)) nil)
	       (t
		(if (and w3-reuse-buffers (not (eq w3-reuse-buffers 'ask)))
		    (progn
		      (ding)
		      (message
		       "Warning: Invalid value for variable w3-reuse-buffers: %s"
		       (prin1-to-string w3-reuse-buffers))
		      (sit-for 2)))
		(not (funcall url-confirmation-func
			      (format "Reuse URL in buffer %s? "
				      (buffer-name buf)))))))
	  (let ((cached (url-retrieve url)))
	    (if w3-track-last-buffer
		(setq w3-last-buffer (get-buffer url-working-buffer)))
	    (if (get-buffer url-working-buffer)
		(cond
		 ((and url-be-asynchronous (string-match "^http:" url)
		       (not cached))
		  (save-excursion
		    (set-buffer url-working-buffer)
		    (if x
			(w3-add-urls-to-history x (url-view-url t)))
		    (setq w3-current-last-buffer lastbuf)))
		 (t
		  (w3-add-urls-to-history x url)
		  (w3-sentinel lastbuf)))))
	(if w3-track-last-buffer
	    (setq w3-last-buffer buf))
	(let ((w3-notify (if (memq w3-notify '(newframe bully 
					       semibully aggressive))
			     w3-notify
			   'aggressive)))
	  (w3-notify-when-ready buf))
	(if (string-match "#\\(.*\\)" url)
	    (progn
	      (push-mark (point) t)
	      (w3-find-specific-link (url-match url 1))))
	(message "Reusing URL.  To reload, type %s."
		 (substitute-command-keys "\\[w3-reload-document]")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History for forward/back buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-node-history nil "History for forward and backward jumping")

(defun w3-plot-course ()
  "Show a map of where the user has been in this session of W3. !!!!NYI!!!"
  (interactive)
  (error "Sorry, w3-plot-course is not yet implemented."))

(defun w3-forward-in-history ()
  "Go forward in the history from this page"
  (interactive)
  (let* ((thisurl (url-view-url t))
	 (node (assoc (if (string= "" thisurl) (current-buffer) thisurl)
		      w3-node-history))
	 (url (cdr node))
	 (w3-reuse-buffers 'yes))
    (cond
     ((null url) (error "No forward found for %s" thisurl))
     ((and (bufferp url) (buffer-name url))
      (switch-to-buffer url))
     ((stringp url)
      (w3-fetch url))
     ((bufferp url)
      (setq w3-node-history (delete node w3-node-history))
      (error "Killed buffer in history, removed."))
     (t
      (error "Something is very wrong with the history!")))))

(defun w3-backward-in-history ()
  "Go backward in the history from this page"
  (interactive)
  (let* ((thisurl (url-view-url t))
	 (node (rassoc (if (string= thisurl "") (current-buffer) thisurl)
			  w3-node-history))
	 (url (car node))
	 (w3-reuse-buffers 'yes))
    (cond
     ((null url) (error "No backward found for %s" thisurl))
     ((and (bufferp url) (buffer-name url))
      (switch-to-buffer url))
     ((stringp url)
      (w3-fetch url))
     ((bufferp url)
      (setq w3-node-history (delete node w3-node-history))
      (error "Killed buffer in history, removed."))
     (t
      (error "Something is very wrong with the history!")))))

(defun w3-add-urls-to-history (referer url)
  "REFERER is the url we followed this link from.  URL is the link we got to."
  (let ((node (assoc referer w3-node-history)))
    (if node
	(setcdr node url)
      (setq w3-node-history (cons (cons referer url) w3-node-history)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-describe-entities ()
  "Show an DTD fragment listing all the entities currently defined."
  (interactive)
  (switch-to-buffer (get-buffer-create "W3 Entities"))
  (let ((buffer-file-name (concat (make-temp-name "entities") ".dtd")))
    (set-auto-mode))
  (erase-buffer)
  (let (entity)
    (mapatoms
     (function
      (lambda (x)
	(setq entity (get x 'html-entity-expansion))
	(if entity
	    (insert (format "<!entity %s %s \"%s\">\n" x (car entity)
			    (cdr entity))))))))
  (goto-char (point-min)))

(defun w3-executable-exists-in-path (exec &optional path)
  (let ((paths (if (consp path)
		   path
		 (mm-string-to-tokens (or path
					  (getenv "PATH")
					  (concat
					   "/usr/bin:/bin:/usr/local/bin:"
					   "/usr/bin/X11:"
					   (expand-file-name "~/bin"))) ?:)))
	(done nil))
    (while (and paths (not done))
      (if (file-exists-p (expand-file-name exec (car paths)))
	  (setq done t))
      (setq paths (cdr paths)))
    done))

(defun w3-document-information (&optional buff)
  "Display information on the document in buffer BUFF"
  (interactive)
  (if (interactive-p)
      (let ((w3-notify 'friendly))
	(if (get-buffer "Document Information")
	    (kill-buffer (get-buffer "Document Information")))
	(w3-fetch "about:document"))
    (setq buff (or buff (current-buffer)))
    (save-excursion
      (set-buffer buff)
      (let* ((url (url-view-url t))
	     (cur-links w3-current-links)
	     (title (buffer-name))
	     (lastmod (or (cdr-safe (assoc "last-modified"
					   url-current-mime-headers))
			  (and (member url-current-type '("file" "ftp"))
			       (nth 5 (url-file-attributes url)))))
	     (hdrs url-current-mime-headers))
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-current-can-be-cached nil
	      url-current-type "about"
	      url-current-file "document")
	(erase-buffer)
	(cond
	 ((stringp lastmod) nil)
	 ((equal '(0 . 0) lastmod) (setq lastmod nil))
	 ((consp lastmod) (setq lastmod (current-time-string lastmod)))
	 (t (setq lastmod nil)))
	(insert "<html>\n"
		" <head>\n"
		"  <title>Document Information</title>\n"
		" </head>\n"
		" <body\n"
		"  <h1 align=\"center\">Document Information</h1>\n"
		"  <hr>\n"
		"  <pre>\n"
		"           Title: " title "\n"
		"        Location: " url "\n"
		"   Last Modified: " (or lastmod "None Given") "\n"
		"  </pre>\n")
	(if hdrs
	    (let* ((maxlength (car (sort (mapcar (function (lambda (x)
							     (length (car x))))
						 hdrs)
					 '>)))
		   (fmtstring (format "%%%ds: %%s" maxlength)))
	      (insert "  <hr label=\" MetaInformation \" textalign=\"left\">\n"
		      "  <pre>\n"
		      (mapconcat
		       (function
			(lambda (x)
			  (if (/= (length (car x)) 0)
			      (format fmtstring
				      (capitalize (car x))
				      (if (numberp (cdr x))
					  (int-to-string (cdr x))
					(cdr x))))))
		       (sort hdrs
			     (function
			      (lambda (x y) (string-lessp (car x) (car y)))))
		       "\n")
		      "  </pre>\n")))
	(if cur-links
	    (while cur-links
	      (let* ((tmp (car cur-links))
		     (label (car tmp))
		     (nodes (cdr tmp))
		     (links nil)
		     (maxlength (car (sort (mapcar
					    (function (lambda (x)
							(length (car x))))
						   nodes)
					   '>)))
		     (fmtstring (format "%%%ds: %%s" maxlength)))
		(insert "  \n"
			"  <hr width=\"50%\" label=\" "
			label " \" align=\"left\" textalign=\"left\">\n"
			"  <pre>\n")
		(while nodes
		  (setq label (car (car nodes))
			links (cdr (car nodes))
			nodes (cdr nodes))
		  (while links
		    (insert (format "  %15s -- <a href=\"%s\">%s</a>\n"
				    label (car links) (car links)))
		    (setq links (cdr links)
			  label "")))
		(insert "  </pre>\n"))
	      (setq cur-links (cdr cur-links))))
	(insert " </body>\n"
		"</html>\n")))))

(defun w3-truncate-menu-item (string)
  (if (<= (length string) w3-max-menu-width)
      string
    (concat (substring string 0 w3-max-menu-width) "$")))

(defun w3-use-starting-documents ()
  "Use the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (let ((w3-hotlist w3-starting-documents))
    (w3-use-hotlist)))

(defun w3-show-starting-documents ()
  "Show the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch "www://auto/starting-points"))

(defun w3-insert-formatted-url (p)
  "Insert a formatted url into a buffer.  With prefix arg, insert the url
under point."
  (interactive "P")
  (let (buff str)
    (cond
     (p
      (setq p (widget-at (point)))
      (or p (error "No url under point"))
      (setq str (format "<A HREF=\"%s\">%s</A>" (widget-get p 'href)
			(read-string "Link text: "
				     (buffer-substring
                                      (widget-get p :from)
                                      (widget-get p :to))))))
     (t
      (setq str (format "<A HREF=\"%s\">%s</A>" (url-view-url t)
			(read-string "Link text: " (buffer-name))))))
    (setq buff (read-buffer "Insert into buffer: " nil t))
    (if buff
	(save-excursion
	  (set-buffer buff)
	  (insert str))
      (message "Cancelled."))))

(defun w3-first-n-items (l n)
  "Return the first N items from list L"
  (let ((x 0)
	y)
    (if (> n (length l))
	(setq y l)
      (while (< x n)
	(setq y (nconc y (list (nth x l)))
	      x (1+ x))))
    y))

(defun w3-widget-button-press ()
  (interactive)
  (if (widget-at (point))
      (widget-button-press (point))))

(defun w3-widget-button-click (e)
  (interactive "@e")
  (if (widget-at (event-point e))
      (widget-button-click e)))
   
(defun w3-breakup-menu (menu-desc max-len)
  (if (> (length menu-desc) max-len)
      (cons (cons "More..." (w3-first-n-items menu-desc max-len))
	    (w3-breakup-menu (nthcdr max-len menu-desc) max-len))
    menu-desc))

;;;###autoload
(defun w3-maybe-follow-link-mouse (e)
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point"
  (interactive "e")
  (save-excursion
    (mouse-set-point e)
    (w3-maybe-follow-link)))

;;;###autoload
(defun w3-maybe-follow-link ()
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point"
  (interactive)
  (require 'w3)
  (if (not w3-setup-done) (w3-do-setup))
  (let* ((widget (widget-at (point)))
         (url1 (and widget (widget-get widget 'href)))
         (url2 (url-get-url-at-point)))
    (cond
      (url1 (w3-follow-link))
      ((and url2 (string-match url-nonrelative-link url2)) (w3-fetch url2))
      (t (message "No URL could be found!")))))

;;;###autoload
(defun w3-follow-url-at-point-other-frame (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch-other-frame url))))

;;;###autoload
(defun w3-follow-url-at-point (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch url))))

;;;###autoload
(defun w3-batch-fetch ()
  "Fetch all the URLs on the command line and save them to files in
the current directory.  The first argument after the -f w3-batch-fetch
on the command line should be a string specifying how to save the
information retrieved.  If it is \"html\", then the page will be
unformatted when it is written to disk.  If it is \"text\", then the
page will be formatted before it is written to disk.  If it is
\"binary\" it will not mess with the file extensions, and just save
the data in raw binary format.  If none of those, the default is
\"text\", and the first argument is treated as a normal URL."
  (if (not w3-setup-done) (w3-do-setup))
  (if (not noninteractive)
      (error "`w3-batch-fetch' is to be used only with -batch"))
  (let ((fname "")
        (curname "")
	(x 0)
	(args command-line-args-left)
	(w3-strict-width 80)
	(w3-delimit-emphasis nil)
	(w3-delimit-links nil)
	(retrieval-function 'w3-fetch)
	(file-format "text")
	(header "")
	(file-extn ".txt"))
    (setq file-format (downcase (car args)))
    (cond
     ((string= file-format "html")
      (message "Saving all text as raw HTML...")
      (setq retrieval-function 'url-retrieve
	    file-extn ".html"
	    header "<BASE HREF=\"%s\">"
	    args (cdr args)))
     ((string= file-format "binary")
      (message "Saving as raw binary...")
      (setq retrieval-function 'url-retrieve
	    file-extn ""
	    args (cdr args)))
     ((string= file-format "text")
      (setq header "Text from: %s\n---------------\n")
      (message "Saving all text as formatted...")
      (setq args (cdr args)))
     (t
      (setq header "Text from: %s\n---------------\n")
      (message "Going with default, saving all text as formatted...")))
    (while args
      (funcall retrieval-function (car args))
      (goto-char (point-min))
      (if buffer-read-only (toggle-read-only))
      (insert (format header (car args)))
      (setq fname (url-basepath url-current-file t))
      (if (string= file-extn "") nil
	(setq fname (url-file-extension fname t)))
      (if (string= (url-strip-leading-spaces fname) "")
	  (setq fname "root"))
      (setq curname fname)
      (while (file-exists-p (concat curname file-extn))
	(setq curname (concat fname x)
	      x (1+ x)))
      (setq fname (concat curname file-extn))
      (write-region (point-min) (point-max) fname)
      (setq args (cdr args)))))

(defun w3-fix-spaces (x)
  "Remove spaces/tabs at the beginning of a string,
and convert newlines into spaces."
  (url-convert-newlines-to-spaces
   (url-strip-leading-spaces
    (url-eat-trailing-space x))))

(defun w3-reload-all-files ()
  "Reload all w3 files"
  (interactive)
  (setq w3-setup-done nil
	url-setup-done nil
	w3-hotlist nil
	url-mime-accept-string nil)
  (let ((x '(w3 w3-mule w3-e19 w3-xem20 mm url w3-xemac w3-toolbar font)))
    (while x
      (setq features (delq (car x) features)
	    x (cdr x)))
    (require 'w3))
  (w3-do-setup)
  (url-do-setup)
  )

(defun w3-source-document-at-point ()
  "View source to the document pointed at by link under point"
  (interactive)
  (w3-source-document t))

(defun w3-my-safe-copy-face (old new locale)
  (let ((fore (face-foreground old))
	(back (face-background old))
	(bpxm (face-background-pixmap old))
	(font (face-font old))
	(font-spec (get old 'font-specification)))
    (if (color-specifier-p fore)
	(setq fore (color-name fore)))
    (if (color-specifier-p back)
	(setq back (color-name back)))
    (if (font-specifier-p font)
	(setq font (font-name font)))
    (and fore (set-face-foreground new fore locale))
    (and back (set-face-background new back locale))
    (and bpxm (set-face-background-pixmap new bpxm locale))
    (and (or font-spec font) (set-face-font new (or font-spec font) locale))
    new))

(defun w3-source-document (under)
  "View this document's source"
  (interactive "P")
  (let* ((url (if under (w3-view-this-url) (url-view-url t)))
	 (fil (if under nil url-current-file))
	 (tag '$html-source)		; For the stylesheet info
	 (args nil)			; For the stylesheet info
	 (face nil)			; For the stylesheet info
	 (src
	  (cond
	   ((or (null url) (string= url "file:nil"))
	    (error "Not a w3 buffer!"))
	   ((and under (null url)) (error "No link at point!"))
	   ((and (not under) (equal url-current-mime-type "text/plain"))
	    (buffer-string))
	   ((and (not under) w3-current-source) w3-current-source)
	   (t
	    (prog2
		(url-retrieve url)
		(buffer-string)
	      (setq fil (or fil url-current-file))
	      (kill-buffer (current-buffer))))))
	 (tmp (url-generate-new-buffer-name url)))
    (if (and url (get-buffer url))
	(cond
	 ((memq w3-reuse-buffers '(no never reload))
	  (kill-buffer url))
	 ((memq w3-reuse-buffers '(yes reuse always))
	  (w3-notify-when-ready (get-buffer url))
	  (setq url nil))
	 ((funcall url-confirmation-func
		   (concat "Source for " url " found, reuse? "))
	  (w3-notify-when-ready (get-buffer url)))))
    (if (not url) nil
      (setq face (and w3-current-stylesheet (cdr (w3-face-for-element))))
      (set-buffer (get-buffer-create tmp))
      (insert src)
      (put-text-property (point-min) (point-max) 'face face)
      (put-text-property (point-min) (point-max) 'w3-base url)
      (goto-char (point-min))
      (setq buffer-file-truename nil
	    buffer-file-name nil)
      ;; Null filename bugs `set-auto-mode' in Mule ...
      (condition-case ()
 	  (set-auto-mode)
	(error nil))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (w3-notify-when-ready (get-buffer tmp))))
  (run-hooks 'w3-source-file-hook))

(defun w3-mail-document-under-point ()
  "Mail the document pointed to by the hyperlink under point."
  (interactive)
  (w3-mail-current-document t))

(defun w3-mail-current-document (under &optional format)
  "Mail the current-document to someone"
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")
			("Formatted Text")
			("PostScript")
			("LaTeX Source")
			)
		  nil t)))
	 (url (cond
	       ((stringp under) under)
	       (under (w3-view-this-url t))
	       (t (url-view-url t))))
	 (content-type "text/plain; charset=iso-8859-1")
	 (str
	  (save-excursion
	    (cond
	     ((and (equal "HTML Source" format) under)
	      (setq content-type "text/html; charset=iso-8859-1")
	      (let ((url-source t))
		(url-retrieve url)))
	     ((equal "HTML Source" format)
	      (setq content-type "text/html; charset=iso-8859-1")
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create url-working-buffer))
		    (erase-buffer)
		    (insert x))
		(url-retrieve url)))
	     ((and under (equal "PostScript" format))
	      (setq content-type "application/postscript")
	      (w3-fetch url)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(w3-print-with-ps-print (current-buffer)
					'ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((equal "PostScript" format)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(setq content-type "application/postscript")
		(w3-print-with-ps-print (current-buffer)
					'ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((and under (equal "Formatted Text" format))
	      (setq content-type "text/plain; charset=iso-8859-1")
	      (w3-fetch url))
	     ((equal "Formatted Text" format)
	      (setq content-type "text/plain; charset=iso-8859-1"))
	     ((and under (equal "LaTeX Source" format))
	      (let ((old-asynch url-be-asynchronous))
		(setq content-type "application/x-latex; charset=iso-8859-1")
		(setq-default url-be-asynchronous nil)
		(url-retrieve url)
		(setq-default url-be-asynchronous old-asynch)
		(w3-parse-tree-to-latex (w3-parse-buffer (current-buffer) t)
					url)))
	     ((equal "LaTeX Source" format)
	      (setq content-type "application/x-latex; charset=iso-8859-1")
	      (w3-parse-tree-to-latex w3-current-parse url)))
	    (buffer-string))))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-subject)
    (insert format " from URL " url "\n"
	    "Mime-Version: 1.0\n"
	    "Content-transfer-encoding: 8bit\n"
	    "Content-type: " content-type)

    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (insert (if (equal "HTML Source" format)
		(format "<BASE HREF=\"%s\">" url) "")
	    str)
    (mail-to)))

(defun w3-internal-use-history (hist-item)
  ;; Go to the link in the history
  (let ((url (nth 0 hist-item))
	(buf (nth 1 hist-item))
	(pnt (nth 2 hist-item)))
    (cond
     ((null buf)			; Find a buffer with same url
      (let ((x (buffer-list))
	    (found nil))
	(while (and x (not found))
	  (save-excursion
	    (set-buffer (car x))
	    (setq found (string= (url-view-url t) url))
	    (if (not found) (setq x (cdr x)))))
	(cond
	 (found
	  (switch-to-buffer (car x))
	  (if (number-or-marker-p pnt) (goto-char pnt)))
	 (t
	  (w3-fetch url)))))
     ((buffer-name buf)			; Reuse the old buffer if possible
      (switch-to-buffer buf)
      (if (number-or-marker-p pnt) (goto-char pnt))
      (if (and url (= ?# (string-to-char url)))	; Destination link
	  (progn
	    (goto-char (point-min))
	    (w3-find-specific-link (substring url 1 nil)))))
     (url (url-maybe-relative url))		; Get the link
     (t (message "Couldn't understand whats in the history.")))))

(defun w3-relative-link (url)
  (if (equal "#" (substring url 0 1))
      (progn
	(push-mark (point) t)
	(goto-char (point-min))
	(w3-find-specific-link (substring url 1 nil)))
    (w3-fetch (url-expand-file-name url))))

(defun w3-maybe-eval ()
  ;; Maybe evaluate a buffer of emacs lisp code
  (if (funcall url-confirmation-func "This is emacs-lisp code, evaluate it?")
      (eval-buffer (current-buffer))
    (emacs-lisp-mode)))

(defun w3-build-continuation ()
  ;; Build a series of functions to be run on this file
  (save-excursion
    (set-buffer url-working-buffer)
    (let ((cont w3-default-continuation)
	  (extn (url-file-extension url-current-file)))
      (if (assoc extn url-uncompressor-alist)
	  (setq extn (url-file-extension
		      (substring url-current-file 0 (- (length extn))))))
      (if w3-source
	  (setq url-current-mime-viewer '(("viewer" . w3-source))))
      (if (not url-current-mime-viewer)
	  (setq url-current-mime-viewer
		(mm-mime-info (or url-current-mime-type
				  (mm-extension-to-mime extn)) nil 5)))
      (if url-current-mime-viewer
	  (setq cont (append cont '(w3-pass-to-viewer)))
	(setq cont (append cont (list w3-default-action))))
      cont)))

(defun w3-use-links ()
  "Select one of the <LINK> tags from this document and fetch it."
  (interactive)
  (and (not w3-current-links)
       (error "No links defined for this document."))
  (w3-fetch "about:document"))

(defun w3-find-this-file ()
  "Do a find-file on the currently viewed html document if it is a file: or
ftp: reference"
  (interactive)
  (cond
   ((and (or (null url-current-type) (equal url-current-type "file"))
	 (eq major-mode 'w3-mode))
    (if w3-mutable-windows
	(find-file-other-window url-current-file)
      (find-file url-current-file)))
   ((equal url-current-type "ftp")
    (if w3-mutable-windows
	(find-file-other-window
	 (format "/%s@%s:%s" url-current-user url-current-server
		 url-current-file))
      (find-file
       (format "/%s@%s:%s" url-current-user url-current-server
	       url-current-file))))
   (t (message "Sorry, I can't get that file so you can alter it."))))

(defun w3-insert-this-url (pref-arg)
  "Insert the current url in another buffer, with prefix ARG,
insert URL under point"
  (interactive "P")
  (let ((thebuf (get-buffer (read-buffer "Insert into buffer: ")))
	(oldbuf (current-buffer))
	(url (if pref-arg (w3-view-this-url t) (url-view-url t))))
    (if (and url (not (equal "Not on a link!" url)))
	(progn
	  (set-buffer thebuf)
	  (insert url)
	  (set-buffer oldbuf))
      (message "Not on a link!"))))

(defun w3-show-hotlist ()
  "View the hotlist in hypertext form"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist)
      (error "Sorry, no hotlist is in memory.")
    (let ((x (url-buffer-visiting "www:/auto/hotlist")))
      (while x
	(kill-buffer x)
	(setq x (url-buffer-visiting "www:/auto/hotlist"))))
    (w3-fetch "www://auto/hotlist")))

(defun url-maybe-relative (url)
  "Take a url and either fetch it, or resolve relative refs, then fetch it"
  (cond
   ((not
     (string-match url-nonrelative-link url))
    (w3-relative-link url))
   (t (w3-fetch url))))

(defun w3-in-assoc (elt list)
  "Check to see if ELT matches any of the regexps in the car elements of LIST"
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (stringp (car (car list)))
	   (not (string= (car (car list)) ""))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun w3-goto-last-buffer ()
  "Go to last WWW buffer visited"
  (interactive)
  (if w3-current-last-buffer
      (w3-notify-when-ready w3-current-last-buffer)
    (message "No previous buffer found.")))

(fset 'w3-replace-regexp 'url-replace-regexp)

;;;###autoload
(defun w3-preview-this-buffer ()
  "See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc."
  (interactive)
  (w3-fetch (concat "www://preview/" (buffer-name))))

(defun w3-edit-source ()
  "Edit the html document just retrieved"
  (set-buffer url-working-buffer)
  (let ((ttl (format "Editing %s Annotation: %s"
		     (cond
		      ((eq w3-editing-annotation 'group) "Group")
		      ((eq w3-editing-annotation 'personal) "Personal")
		      (t "Unknown"))
		     (url-basepath url-current-file t)))
	(str (buffer-string)))
    (set-buffer (get-buffer-create ttl))
    (insert str)
    (kill-buffer url-working-buffer)))

(defun w3-source ()
  "Show the source of a file"
  (let ((tmp (buffer-name (generate-new-buffer "Document Source"))))
    (set-buffer url-working-buffer)
    (kill-buffer tmp)
    (rename-buffer tmp)
    ;; Make the URL show in list-buffers output
    (make-local-variable 'list-buffers-directory)
    (setq list-buffers-directory (url-view-url t))
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (w3-notify-when-ready (get-buffer tmp))))

(defun w3-sentinel (&optional proc string)
  (set-buffer url-working-buffer)
  (if (or (stringp proc)
	  (bufferp proc)) (setq w3-current-last-buffer proc))
  (if (boundp 'after-change-functions)
      (remove-hook 'after-change-functions 'url-after-change-function))
  (if url-be-asynchronous
      (progn
	(url-clean-text)
	(cond
	 ((not (get-buffer url-working-buffer)) nil)
	 ((url-mime-response-p) (url-parse-mime-headers)))
	(if (not url-current-mime-type)
	    (setq url-current-mime-type (or (mm-extension-to-mime
					     (url-file-extension
					      url-current-file))
					    "text/html")))))
  (let ((x (w3-build-continuation))
	(done-mule-conversion nil))
    (while x
      (if (and (featurep 'mule) (not (eq 'url-uncompress (car x)))
	       (not done-mule-conversion))
	  (progn
            (if (string-match "^www:" (url-view-url t))
                (setq w3-mime-list-for-code-conversion nil))
	    (w3-convert-code-for-mule url-current-mime-type)
	    (setq done-mule-conversion t)))
      (funcall (car x))
      (setq x (cdr x)))))

(defun w3-show-history-list ()
  "Format the url-history-list prettily and show it to the user"
  (interactive)
  (w3-fetch "www://auto/history"))

(defun w3-save-as (&optional type)
  "Save a document to the local disk"
  (interactive)
  (let* ((completion-ignore-case t)
	 (format (or type (completing-read
			   "Format: "
			   '(("HTML Source") ("Formatted Text")
			     ("LaTeX Source") ("Binary"))
			   nil t)))
	(fname (expand-file-name
		(read-file-name "File name: " default-directory)))
	(url (url-view-url t)))
    (cond
     ((equal "Binary" format)
      (if (not w3-current-source)
	  (let ((url-be-asynchronous nil))
	    (url-retrieve url))))
     ((equal "HTML Source" format)
      (if (not w3-current-source)
	  (let ((url-be-asynchronous nil))
	    (url-retrieve url))		; Get the document if necessary
	(let ((txt w3-current-source))
	  (set-buffer (get-buffer-create url-working-buffer))
	  (insert txt)))
      (goto-char (point-min))
      (insert (format "<BASE HREF=\"%s\">\n" url)))
     ((or (equal "Formatted Text" format)
	  (equal "" format))
      nil)				; Do nothing - we have the text already
     ((equal "LaTeX Source" format)
      (w3-parse-tree-to-latex w3-current-parse url)
      (insert-buffer url-working-buffer)))
    (write-region (point-min) (point-max) fname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to parse out <A> tags and replace it with a hyperlink zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-popup-image-info (url)
  (interactive)
  (let* ((glyph (cdr-safe (assoc url w3-graphics-list)))
       image w h d info)
    (save-excursion
      (if (or (not glyph) (not (glyphp glyph)))
        (error "No information available."))
      (setq image (glyph-image-instance glyph))
      (if (or (not image) (not (image-instance-p image)))
        (error "No information available."))
      (setq w (glyph-width glyph)
          h (glyph-height glyph)
          d (image-instance-depth image)
          info (url-popup-info url)
          )
      (set-buffer (get-buffer-create "*Image Info*"))
      (erase-buffer)
      (insert
       "Information for: " url "\n"
       (make-string (1- (window-width)) ?-)
       (format "\n%-20s: %s\n" "Type" (image-instance-type image))
       (format "%-20s: %d x %d\n" "Dimensions" w h)
       (format "%-20s: %d-bit\n" "Color" d))
      (set-extent-begin-glyph (make-extent (point) (point)) glyph)
      (insert
       "\n"
       (make-string (1- (window-width)) ?-)
       (or info ""))
      (display-buffer (current-buffer) t))))
               
(defun w3-popup-info (&optional url)
  "Show information about the link under point. (All SGML attributes)"
  (interactive (list (w3-read-url-with-default)))
  (let (dat widget)
    (if (interactive-p)
	nil
      (setq widget (widget-at (point))
	    dat (and widget (widget-get widget 'attributes))))
    (if url
	(save-excursion
	  (set-buffer (get-buffer-create "*Header Info*"))
	  (erase-buffer)
	  (insert "URL: " url "\n" (make-string (1- (window-width)) ?-) "\n")
	  (if (and dat (listp dat))
	      (insert
	       "Link attributes:\n"
	       (make-string (1- (window-width)) ?-) "\n"
	       (mapconcat
		(function
		 (lambda (info)
		   (format "%20s :== %s" (car info) (or (cdr info) "On"))))
		dat "\n")
	       "\n" (make-string (1- (window-width)) ?-) "\n"))
	  (insert (save-excursion (url-popup-info url)))
	  (goto-char (point-min))
	  (display-buffer (current-buffer) t))
      (message "No URL to get information on!"))))

(fset 'w3-document-information-this-url 'w3-popup-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for logging of bad HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-reconstruct-tag (tagname desc)
  (concat "<" tagname " "
	  (mapconcat
	   (function (lambda (x)
		       (if (cdr x)
			   (concat (car x) "=\"" (cdr x) "\"")
			 (car x)))) desc " ") ">"))

(defun w3-debug-if-found (regexp type desc)
  (and w3-debug-html
       (save-excursion
	 (if (re-search-forward regexp nil t)
	     (w3-log-bad-html type desc)))))

(defun w3-log-bad-html (type desc)
  ;; Log bad HTML to the buffer specified by w3-debug-buffer
  (if w3-debug-html
      (save-excursion
	(set-buffer (get-buffer-create w3-debug-buffer))
	(goto-char (point-max))
	(insert (make-string (1- (window-width)) w3-horizontal-rule-char) "\n")
	(cond
	 ((stringp type) (insert type "\n" desc "\n"))
	 ((eq type 'bad-quote)
	  (insert "Unterminated quoting character in SGML attribute value.\n"
		  desc "\n"))
	 ((eq type 'no-quote)
	  (insert "Unquoted SGML attribute value.\n" desc "\n"))
	 ((eq type 'no-textarea-end)
	  (insert "Unterminated <textarea> tag.\n"
		  (w3-reconstruct-tag "textarea" desc) "\n"))
	 ((eq type 'bad-link-tag)
	  (insert "Must specify either REL or REV with a <link> tag.\n"
		  (w3-reconstruct-tag "link" desc) "\n"))
	 ((eq type 'no-a-end)
	  (insert "Unterminated <a> tag.\n"
		  (w3-reconstruct-tag "a" desc) "\n"))
	 ((eq type 'no-form-end)
	  (insert "Unterminated <form> tag.\n"
		  (w3-reconstruct-tag "form" desc) "\n"))
	 ((eq type 'bad-base-tag)
	  (insert "Malformed <base> tag.\n"
		  (w3-reconstruct-tag "base" desc) "\n"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle formatting an html buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-insert-headers ()
  ;; Insert some HTTP/1.0 headers if necessary
  (url-lazy-message "Inserting HTTP/1.0 headers...")
  (let ((hdrs (if (eq t w3-show-headers) (mapcar 'car url-current-mime-headers)
		w3-show-headers))
	x y)
    (goto-char (setq y (point-max)))
    (while hdrs
      (if (setq x (w3-in-assoc (car hdrs) url-current-mime-headers))
	  (insert "<LI> <B>" (car x) "</B>: " (url-insert-entities-in-string
					       (if (numberp (cdr x))
						   (int-to-string (cdr x))
						 (cdr x)))))
      (setq hdrs (cdr hdrs)))
    (if (= y (point-max))
	nil
      (insert "</UL>")
      (goto-char y)
      (url-lazy-message "Inserting HTTP/1.0 headers... done.")
      (insert "<HR><UL>"))))

(defun w3-add-delayed-graphic (widget)
  ;; Add a delayed image for the current buffer.
  (setq w3-delayed-images (cons widget w3-delayed-images)))


(defun w3-load-flavors ()
  ;; Load the correct zone/font info for each flavor of emacs
  (cond
   ((and w3-running-xemacs (eq system-type 'ms-windows))
    (error "WinEmacs no longer supported."))
   (w3-running-xemacs (require 'w3-xemac))
   (w3-running-FSF19  (require 'w3-e19))
   (t
    (error "Unable to determine the capabilities of this emacs.")))
  (cond
   ((boundp 'MULE)
    (require 'w3-mule))
   ((featurep 'mule)
    (require 'w3-xem20)
    ))
  (condition-case ()
      (require 'w3-site-init)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-submit-bug ()
  "Submit a bug on Emacs-w3"
  (interactive)
  (require 'reporter)
  (and (yes-or-no-p "Do you really want to submit a bug on Emacs-w3? ")
       (let ((url (url-view-url t))
	     (vars '(window-system
		     window-system-version
		     system-type
		     ange-ftp-version
		     url-gateway-method
		     efs-version
		     ange-ftp-version
		     url-version
		     url-be-asynchronous
		     url)))
	 (if (and url (string= url "file:nil")) (setq url nil))
	 (mapcar
	  (function
	   (lambda (x)
	     (if (not (and (boundp x) (symbol-value x)))
		 (setq vars (delq x vars))))) vars)
	 (reporter-submit-bug-report w3-bug-address
				     (concat "WWW v" w3-version-number " of "
					     w3-version-date)
				     vars
				     nil nil
				     "Description of Problem:"))))

(defalias 'w3-bug 'w3-submit-bug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for searching						    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-nuke-spaces-in-search (x)
  "Remove spaces from search strings . . ."
  (let ((new ""))
    (while (not (equal x ""))
      (setq new (concat new (if (= (string-to-char x) 32) "+"
			      (substring x 0 1)))
	    x (substring x 1 nil)))
    new))

(defun w3-search ()
  "Perform a search, if this is a searchable index."
  (interactive)
  (or w3-current-isindex
      (error "Not a searchable index (via <isindex>)"))
  (let* (querystring			; The string to send to the server
	 (data
	  (cond
	   ((null w3-current-isindex)
	    (let ((rels (mapcar
			 (function
			  (lambda (data)
			    (if (assoc "rel" data) data)))
			 w3-current-links))
		  val)
	      (while rels
		(if (string-match "useindex"
				  (or (cdr (assoc "rel" (car rels))) ""))
		    (setq val (cdr (assoc "href" (car rels)))
			  rels nil))
		(setq rels (cdr rels)))
	      (cons val "Search on (+ separates keywords): ")))
	   ((eq w3-current-isindex t)
	    (cons (url-view-url t) "Search on (+ separates keywords): "))
	   ((consp w3-current-isindex)
	    w3-current-isindex)
	   (t nil)))
	 index)
    (if (null data) (error "Not a searchable index!"))
    (setq index (car data))
    (setq querystring (w3-nuke-spaces-in-search (read-string (cdr data))))
    (if (string-match "\\(.*\\)\\?.*" index)
	(setq index (url-match index 1)))
    (w3-fetch
     (concat index (if (= ?? (string-to-char (substring index -1 nil)))
		       "" "?") querystring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto documentation, etc                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-help ()
  "Print documentation on w3 mode."
  (interactive)
  (w3-fetch "about:"))

(defun w3-version (&optional here)
  "Show the version number of W3 in the minibuffer.
If optional argument HERE is non-nil, insert info at point."
  (interactive "P")
  (let ((version-string 
         (format "WWW %s, URL %s, MM %s" 
                 w3-version-number 
                 url-version
                 mm-version)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;;;###autoload
(defun w3 ()
  "Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable w3-default-homepage.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (and w3-track-last-buffer
	   (bufferp w3-last-buffer)
	   (buffer-name w3-last-buffer))
      (progn
	(switch-to-buffer w3-last-buffer)
	(message "Reusing buffer.  To reload, type %s."
		 (substitute-command-keys "\\[w3-reload-document]")))
    (cond
     ((null w3-default-homepage) (call-interactively 'w3-fetch))
     ((not (stringp w3-default-homepage))
      (error "Invalid setting for w3-default-homepage: %S"
	     w3-default-homepage))
     ((not (string-match ".*:.*" w3-default-homepage))
      (w3-fetch (concat "file:" w3-default-homepage)))
     (t
      (w3-fetch w3-default-homepage)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leftover stuff that didn't quite fit into url.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-generate-error (type data)
  ;; Generate an HTML error buffer for error TYPE with data DATA.
  (cond
   ((equal type "nofile")
    (let ((error (save-excursion
		  (set-buffer (get-buffer-create " *url-error*"))
		  (buffer-string))))
      (if (string= "" error)
	  (setq error
		(format (concat "The file %s could not be found.  "
				"Either it does not exist, or it "
				"is unreadable.") data)))
      (insert "<html>\n <head>\n"
	    "  <title>Error</title>\n"
	    " </head>\n <body>\n"
	    "  <h1>Error accessing " data "</h1>\n"
	    "  <hr>\n  <p>"
	    error
	    "\n  </p>\n")))
   ((equal type "nobuf")
    (insert "<title>Error</title>\n"
	    "<H1>No buffer " data " found</h1>\n"
	    "<HR>\n"
	    "The buffer " data " could not be found.  It has either\n"
	    "been killed or renamed.\n"))
   ((equal type "nohist")
    (insert "<TITLE>Error</TITLE>\n"
	    "<H1>No history items found.</H1>\n"
	    "<HR>\n"
	    "There is no history list available at this time.  Either\n"
	    "you have not visited any nodes, or the variable <i>\n"
	    "url-keep-history</i> is nil.\n"))
   )
  (insert "<hr>\n"
	  "If you feel this is a bug in Emacs-W3, <a href=\"mailto:"
	  w3-bug-address "\">send mail to " w3-bug-address
	  "</a>\n<hr>"))

(defun w3-generate-auto-html (type)
  ;; Generate one of several automatic html pages
  (setq url-current-mime-type "text/html"
	url-current-mime-headers '(("content-type" . "text/html")))
  (cond
   ((equal type "hotlist")
    (let ((tmp (reverse w3-hotlist)))
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> Hotlist </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div>\n\t\t\t<h1>Hotlist from " w3-hotlist-file
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert  "\t\t\t\t<li> <a href=\"" (car (cdr (car tmp)))
		 "\">" (url-insert-entities-in-string
			(car (car tmp))) "</a></li>\n")
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div>\n\t</body>\n</html>\n")))
   ((equal type "starting-points")
    (let ((tmp w3-starting-documents))
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> Starting Points </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div>\n\t\t\t<h1>Starting Point on the Web"
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert (format "\t\t\t\t<li> <a href=\"%s\">%s</a></li>\n"
			(car (cdr (car tmp)))
			(car (car tmp))))
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div>\n\t</body>\n</html>\n")))
   ((equal type "history")
    (if (not url-history-list)
	(url-retrieve "www://error/nohist")
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> History List For This Session of W3</title>"
	      "\n\t</head>\n\t<body>\n\t\t<div>\n\t\t\t<h1>"
	      "History List For This Session of W3</h1>\n\t\t\t<ol>\n")
      (url-maphash
       (function
	(lambda (url desc)
	  (insert (format "\t\t\t\t<li> <a href=\"%s\">%s</a>\n"
			  url (url-insert-entities-in-string desc)))))
       url-history-list)
      (insert "\n\t\t\t</ol>\n\t\t</div>\n\t</body>\n</html>\n")))))

(defun w3-internal-handle-preview (buffer)
  (setq buffer (get-buffer buffer))
  (let ((base (get-text-property (point-min) 'w3-base buffer)))
    (if base
	(setq base (url-generic-parse-url base)))
    (insert-buffer buffer)
    (if (not base)
	(setq url-current-type "file"
	      url-current-server nil
	      url-current-file (buffer-file-name buffer))
      (setq url-current-object base
	    url-current-type (url-type base)
	    url-current-user (url-user base)
	    url-current-port (url-port base)
	    url-current-server (url-host base)
	    url-current-file (url-filename base)))))

(defun w3-internal-url (url)
  ;; Handle internal urls (previewed buffers, etc)
  (if (not (string-match "www:/+\\([^/]+\\)/\\(.*\\)" url))
      (w3-fetch "www://error/")
    (let ((type (url-match url 1))
	  (data (url-match url 2)))
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-current-type "www"
	    url-current-server type
	    url-current-file data)
      (cond
       ((equal type "preview")		; Previewing a document
	(if (get-buffer data)		; Buffer still exists
	    (w3-internal-handle-preview data)
	  (url-retrieve (concat "www://error/nobuf/" data))))
       ((equal type "error")		; Error message
	(if (string-match "\\([^/]+\\)/\\(.*\\)" data)
	    (w3-generate-error (url-match data 1) (url-match data 2))
	  (w3-generate-error data "")))
       ((equal type "auto")		; Hotlist or help stuff
	(w3-generate-auto-html data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff for good local file handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-ff (file)
  "Find a file in any window already displaying it, otherwise just as
display-buffer, and using this function"
  (if (not (eq 'tty (device-type)))
      (let ((f (window-frame (display-buffer (find-file-noselect file)))))
	(set-mouse-position f 1 0)
	(raise-frame f)
	(unfocus-frame))
    (display-buffer (find-file-noselect file))))

(defun w3-default-local-file()
  "Use find-file to open the local file"
  (w3-ff url-current-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition							    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-search-forward (string)
  (interactive "sSearch: ")
  (setq w3-last-search-item string)
  (if (and (not (search-forward string nil t))
	   (funcall url-confirmation-func
		    "End of document reached; continue from beginning? "))
      (progn
	(goto-char (point-min))
	(w3-search-forward string))))

(defun w3-search-again ()
  (interactive)
  (if (and w3-last-search-item
	   (stringp w3-last-search-item))
      (if (and (not (search-forward w3-last-search-item nil t))
	       (funcall url-confirmation-func
			"End of document reached; continue from beginning? "))
	  (progn
	    (goto-char (point-min))
	    (w3-search-again)))))

(defun w3-find-specific-link (link)
  (let ((pos (assq (intern link) w3-id-positions)))
    (if pos
	(progn
	  (goto-char (cdr pos))
	  (if (and (eolp) (not (eobp)))
	      (forward-char 1)))
      (error "Link #%s not found." link))))

  (defun w3-force-reload-document ()
  "Reload the current document.  Take it from the network, even if
cached and in local mode."
  (let ((url-standalone-mode nil))
    (w3-reload-document)))

(defun w3-reload-document ()
  "Reload the current document"
  (interactive)
  (let ((tmp (url-view-url t))
	(pnt (point))
	(window-start (progn
			(move-to-window-line 0)
			(point)))
	(url-request-extra-headers '(("Pragma" . "no-cache"))))
    (kill-buffer (current-buffer))
    (w3-fetch tmp)
    (goto-char pnt)
    (set-window-start (selected-window) (min window-start (point-max)))))

(defun w3-leave-buffer ()
  "Bury this buffer, but don't kill it."
  (interactive)
  (let ((x w3-current-last-buffer))
    (bury-buffer nil)
    (if (and (bufferp x) (buffer-name x))
	(w3-notify-when-ready x))))

(defun w3-quit (&optional mega)
  "Quit WWW mode"
  (interactive "P")
  (if mega
      (mapcar
       (function
	(lambda (x)
	  (save-excursion
	    (set-buffer (get-buffer x))
	    (if (eq major-mode 'w3-mode)
		(w3-quit nil)))))
       (buffer-list))
    (let ((x w3-current-last-buffer))
      (kill-buffer (current-buffer))
      (if (and (bufferp x) (buffer-name x))
	  (w3-notify-when-ready x)))))

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link under point"
  (interactive)
  (let* ((widget (widget-at (point)))
	 (href (and widget (widget-get widget 'href))))
    (cond
     ((and no-show href)
      href)
     (href
      (message "%s" (url-truncate-url-for-viewing href)))
     (no-show
      nil)
     (t
      nil))))

(defun w3-load-delayed-images ()
    "Load inlined images that were delayed, if any."
  (interactive)
  (let ((w3-delay-image-loads nil)
	(todo w3-delayed-images))
    (setq w3-delayed-images nil)
    (while todo
      (w3-maybe-start-image-download (car todo))
      (setq todo (cdr todo)))))

(defun w3-save-this-url ()
  "Save url under point in the kill ring"
  (interactive)
  (w3-save-url t))

(defun w3-save-url (under-pt)
  "Save current url in the kill ring"
  (interactive "P")
  (let ((x (cond
	    ((stringp under-pt) under-pt)
	    (under-pt (w3-view-this-url t))
	    (t (url-view-url t)))))
    (if x
	(progn
	  (setq kill-ring (cons x kill-ring))
	  (setq kill-ring-yank-pointer kill-ring)
	  (message "Stored URL in kill-ring.")
	  (if (fboundp 'w3-store-in-clipboard)
	      (w3-store-in-clipboard x)))
      (error "No URL to store."))))

(fset 'w3-end-of-document 'end-of-buffer)
(fset 'w3-start-of-document 'beginning-of-buffer)

(defun w3-scroll-up (&optional lines)
  "Scroll forward in View mode, or exit if end of text is visible.
No arg means whole window full.  Arg is number of lines to scroll."
  (interactive "P")
  (if (and (pos-visible-in-window-p (point-max))
	   ;; Allow scrolling backward at the end of the buffer.
	   (or (null lines)
	       (> lines 0)))
      nil
    (let ((view-lines (1- (window-height))))
      (setq lines
	    (if lines (prefix-numeric-value lines)
	      view-lines))
      (if (>= lines view-lines)
	  (scroll-up nil)
	(if (>= (- lines) view-lines)
	    (scroll-down nil)
	  (scroll-up lines)))
      (cond ((pos-visible-in-window-p (point-max))
	     (goto-char (point-max))
	     (recenter -1)))
      (move-to-window-line -1)
      (beginning-of-line))))

(defun w3-mail-document-author ()
  "Send mail to the author of this document, if possible."
  (interactive)
  (let ((x w3-current-links)
	(y nil)
	(found nil))
    (setq found (cdr-safe (assoc "reply-to" url-current-mime-headers)))
    (if (and found (not (string-match url-nonrelative-link found)))
	(setq found (concat "mailto:" found)))
    (while (and x (not found))
      (setq y (car x)
	    x (cdr x)
	    found (cdr-safe (assoc "made" y))))
    (if found
	(let ((possible nil))
	  (setq x (car found))		; Fallback if no mail(to|server) found
	  (while found
	    (if (string-match "^mail[^:]+:" (car found))
		(setq possible (cons (car found) possible)))
	    (setq found (cdr found)))
	  (case (length possible)
	    (0				; No mailto links found
	     (w3-fetch x))		; fall back onto first 'made' link
	    (1				; Only one found, get it
	     (w3-fetch (car possible)))
	    (otherwise
	     (w3-fetch (completing-read "Choose an address: "
					(mapcar 'list possible)
					nil t (car possible))))))
      (message "Could not automatically determine authors address, sorry.")
      (sit-for 1)
      (w3-fetch (concat "mailto:"
			(read-string "Email address: "
				     (if url-current-server
					 (concat "@" url-current-server))))))))

(defun w3-kill-emacs-func ()
  "Routine called when exiting emacs.  Do miscellaneous clean up."
  (and (eq url-keep-history t)
       url-global-history-hash-table
       (url-write-global-history))
  (message "Cleaning up w3 storage...")
  (let ((x (nconc
	    (and (file-exists-p w3-temporary-directory)
		 (directory-files w3-temporary-directory t "url-tmp.*"))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t
				  (concat "url"
					  (int-to-string
					   (user-real-uid)) ".*")))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t "url-tmp.*")))))
    (while x
      (condition-case ()
	  (delete-file (car x))
	(error nil))
      (setq x (cdr x))))
  (message "Cleaning up w3 storage... done."))

(cond
 ((fboundp 'display-warning)
  (fset 'w3-warn 'display-warning))
 ((fboundp 'warn)
  (defun w3-warn (class message &optional level)
    (if (and (eq class 'html)
	     (not w3-debug-html))
	nil
      (warn "(%s/%s) %s" class (or level 'warning) message))))
 (t
  (defun w3-warn (class message &optional level)
    (if (and (eq class 'html)
	     (not w3-debug-html))
	nil
      (save-excursion
	(set-buffer (get-buffer-create "*W3-WARNINGS*"))
	(goto-char (point-max))
	(save-excursion
	  (insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
	(display-buffer (current-buffer)))))))

(defun w3-internal-expander (urlobj defobj)
  ;; URL Expansion routine for internally handled routines
  (url-identity-expander urlobj defobj))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
WIDGET and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively."
  (let ((cur (point-min))
	(widget nil)
	(url nil))
    (while (setq cur (next-single-property-change cur 'button))
      (setq widget (widget-at cur))
      ;; Check to see if its a push widget, its got the correct callback,
      ;; and actually has a URL.  Remember the url as a side-effect of the
      ;; test for later use.
      (if (and (eq (car widget) 'push)
	       (eq (widget-get widget :notify) 'w3-follow-hyperlink)
	       (setq url (widget-get widget 'href)))
	  (funcall function widget maparg)))))

(defun w3-emit-image-warnings-if-necessary ()
  (if (and (not w3-delay-image-loads)
	   (fboundp 'w3-insert-graphic)
	   (or (not (featurep 'gif))
	       (not (featurep 'jpeg)))
	   (not (w3-executable-exists-in-path "ppmtoxpm"))
	   (not (or
		 (w3-executable-exists-in-path "pbmtoxbm")
		 (w3-executable-exists-in-path "ppmtoxbm"))))
      (w3-warn
       'image
       (concat
	"Could not find some vital ppm utilities in exec-path.\n"
	"This probably means that you will be unable to view any\n"
	"inlined images other than: "
	(mapconcat
	 (function
	  (lambda (x)
	    (if (featurep x) (concat (symbol-name x) ",\n"))))
	 '(png jpg gif xpm xbm) "")
	"\n\n"
	"If you do not have the PPM utilities from either the PBMPLUS\n"
	"or NETPBM distributions installed on your machine, then\n"
	"please set the variable `w3-delay-image-loads' to t with a\n"
	"line like:\n\n"
	"\t(setq w3-delay-image-loads t)\n\n"
	"in your ~/.emacs file.\n\n"
	"You can find the NETPBM utilities in:\n"
	"\tftp://ftp.cs.indiana.edu/pub/elisp/w3/images/\n"
	))))

(defun w3-refresh-stylesheets ()
  "Reload all stylesheets."
  (interactive)
  (setq w3-user-stylesheet nil
	w3-face-cache nil)
  (w3-find-default-stylesheets)
  (w3-style-post-process-stylesheet w3-user-stylesheet))

(defun w3-find-default-stylesheets ()
  (let* ((lightp (w3-color-light-p 'default))
	 (longname (if lightp "stylesheet-light" "stylesheet-dark"))
	 (shortname (if lightp "light.css" "dark.css"))
	 (directories (list
		       data-directory
		       (concat data-directory "w3/")
		       (file-name-directory (locate-library "w3"))
		       w3-configuration-directory))
	 (total-found 0)
	 (possible (append
		    (apply
		     'append
		     (mapcar
		      (function
		       (lambda (dir)
			 (list
			  (expand-file-name shortname dir)
			  (expand-file-name longname dir)
			  (expand-file-name "stylesheet" dir)
			  (expand-file-name "default.css" dir))))
		      directories))
		    (list w3-default-stylesheet)))
	 (remember possible)
	 (old-asynch (default-value 'url-be-asynchronous))
	 (found nil)
	 (cur nil)
	 (url nil))
    (setq-default url-be-asynchronous nil)
    (while possible
      (setq cur (car possible)
	    possible (cdr possible)
	    found (and cur (file-exists-p cur) (file-readable-p cur)
		       (not (file-directory-p cur)) cur))
      (if found
	  (setq total-found (1+ total-found)
		w3-user-stylesheet (car
				    (w3-style-parse-css
				     (concat "file:" cur) nil
				     w3-user-stylesheet)))))
    (setq-default url-be-asynchronous old-asynch)
    (if (= 0 total-found)
	(w3-warn
	 'style
	 (concat
	 "No stylesheets found!  Check configuration! DANGER DANGER!\n"
	 "Emacs-W3 checked for its stylesheet in the following places\n"
	 "and did not find one.  This means that some formatting will\n"
	 "be wrong, and most colors and fonts will not be set up correctly.\n"
	 "------\n"
	 (mapconcat 'identity remember "\n")
	 "------")))))

;;;###autoload
(defun w3-do-setup ()
  "Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs."
  (url-do-setup)
  (url-register-protocol 'about 'w3-about 'url-identity-expander)
  (url-register-protocol 'www 'w3-internal-url 'w3-internal-expander)
  (w3-load-flavors)
  (w3-setup-version-specifics)
  (setq w3-default-configuration-file (expand-file-name 
				       (or w3-default-configuration-file
					   "profile")
				       w3-configuration-directory))
					   

  (if (and w3-default-configuration-file
	   (file-exists-p w3-default-configuration-file))
      (condition-case e
	  (load w3-default-configuration-file nil t)
	(error
	 (let ((buf-name " *Configuration Error*"))
	   (if (get-buffer buf-name)
	       (kill-buffer (get-buffer buf-name)))
	   (display-error e (get-buffer-create buf-name))
	   (save-excursion
	     (switch-to-buffer-other-window buf-name)
	     (shrink-window-if-larger-than-buffer))
	   (w3-warn 'configuration
		    (format (eval-when-compile
			      (concat
			       "Configuration file `%s' contains an error.\n"
			       "Please consult the `%s' buffer for details."))
			    w3-default-configuration-file buf-name))))))
	       
  (setq w3-netscape-configuration-file
	(cond
	 (w3-netscape-configuration-file
	  w3-netscape-configuration-file)
	 ((memq system-type '(ms-dos ms-windows))
	  (expand-file-name "~/NETSCAPE.CFG"))
	 (t (expand-file-name "~/.netscape/preferences"))))

  (if (and (eq w3-user-colors-take-precedence 'guess)
	   (not (eq (device-type) 'tty))
	   (not (eq (device-class) 'mono)))
      (progn
	(setq w3-user-colors-take-precedence t)
	(w3-warn
	 'html
	 "Disabled document color specification because of mono display.")))

  (w3-refresh-stylesheets)
  (if (not url-global-history-file)
      (setq url-global-history-file
	    (expand-file-name "history"
			      w3-configuration-directory)))

  (if (and w3-use-netscape-configuration-file
	   w3-netscape-configuration-file
	   (fboundp 'w3-read-netscape-config))
      (w3-read-netscape-config w3-netscape-configuration-file))
      
  (add-minor-mode 'w3-netscape-emulation-minor-mode " NS"
		  w3-netscape-emulation-minor-mode-map)
  (add-minor-mode 'w3-annotation-minor-mode " Annotating"
		  w3-annotation-minor-mode-map)
  (add-minor-mode 'w3-lynx-emulation-minor-mode " Lynx"
		  w3-annotation-minor-mode-map)
  
  (setq url-package-version w3-version-number
	url-package-name "Emacs-W3")

  (w3-emit-image-warnings-if-necessary)
  (if (eq w3-color-use-reducing 'guess)
      (setq w3-color-use-reducing
	    (cond
	     ((eq (device-type) 'tty) nil)
	     ((fboundp 'device-class)
	      (not (and (memq (device-class) '(TrueColor true-color))
			(<= 16 (or (device-bitplanes) 0)))))
	     (t t))))
		   
  (cond
   ((memq system-type '(ms-dos ms-windows))
    (setq w3-documents-menu-file (or w3-documents-menu-file
				     (expand-file-name "~/mosaic.mnu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hot"))
	  w3-personal-annotation-directory (or w3-personal-annotation-directory
					       (expand-file-name
						"~/mosaic.ann"))))
   ((memq system-type '(axp-vms vax-vms))
    (setq w3-documents-menu-file
	  (or w3-documents-menu-file
	      (expand-file-name "decw$system_defaults:documents.menu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hotlist-default"))
	  w3-personal-annotation-directory
	  (or w3-personal-annotation-directory
	      (expand-file-name "~/mosaic-annotations/"))))
   (t 
    (setq w3-documents-menu-file
	  (or w3-documents-menu-file
	      (expand-file-name "/usr/local/lib/mosaic/documents.menu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/.mosaic-hotlist-default"))
	  w3-personal-annotation-directory
	  (or w3-personal-annotation-directory
	      (expand-file-name "~/.mosaic-personal-annotations")))))
  
  (if (eq w3-delimit-emphasis 'guess)
      (setq w3-delimit-emphasis
	    (and (not w3-running-xemacs)
		 (not (and w3-running-FSF19
			   (memq (device-type) '(x ns pm)))))))

  (if (eq w3-delimit-links 'guess)
      (setq w3-delimit-links
	    (and (not w3-running-xemacs)
		 (not (and w3-running-FSF19
			   (memq (device-type) '(x ns pm)))))))

  ; Set up a hook that will save the history list when
  ; exiting emacs
  (add-hook 'kill-emacs-hook 'w3-kill-emacs-func)

  (mm-parse-mailcaps)
  (mm-parse-mimetypes)

  ; Load in the hotlist if they haven't set it already
  (or w3-hotlist (w3-parse-hotlist))

  ; Load in their personal annotations if they haven't set them already
  (or w3-personal-annotations (w3-parse-personal-annotations))

  ; Set the default home page, honoring their defaults, then
  ; the standard WWW_HOME, then default to the documentation @ IU
  (or w3-default-homepage
      (setq w3-default-homepage
	    (or (getenv "WWW_HOME")
		"http://www.cs.indiana.edu/elisp/w3/docs.html")))

  ; Set up the documents menu
  (w3-parse-docs-menu)

  ; Set up the entity definition for PGP and PEM authentication

  (run-hooks 'w3-load-hook)
  (setq w3-setup-done t))

(defun w3-mark-link-as-followed (ext dat)
  ;; Mark a link as followed
  (let* ((st (w3-zone-start ext))
	 (nd (w3-zone-end ext))
	 (tag 'a)
	 (args (list (cons 'class "visited")))
	 (face (cdr (w3-face-for-element))))
    (w3-add-zone st nd face dat t)))

(defun w3-only-links ()
  (let* (result temp)
    (if (widget-at (point-min))
	(setq result (list (widget-at (point-min)))))
    (setq temp (w3-next-widget (point-min)))
    (while temp
      (if (widget-get temp 'href)
	  (setq result (cons temp result)))
      (setq temp (w3-next-widget (widget-get temp :to))))
    result))

(defun w3-download-callback (fname buff)
  (if (and (get-buffer buff) (buffer-name buff))
      (save-excursion
	(set-buffer buff)
	(let ((require-final-newline nil)
	      (file-name-handler-alist nil)
	      (write-file-hooks nil)
	      (write-contents-hooks nil)
	      (mc-flag t)
	      (file-coding-system url-mule-no-coding-system))
	  (write-file fname)
	  (message "Download of %s complete." (url-view-url t))
	  (sit-for 3)
	  (kill-buffer buff)))))

(defun w3-download-url (url)
  (let* ((old-asynch url-be-asynchronous)
	 (url-inhibit-uncompression t)
	 (url-mime-accept-string "*/*")
	 (urlobj (url-generic-parse-url url))
	 (url-working-buffer
	  (generate-new-buffer (concat " *" url " download*")))
	 (stub-fname (url-remove-compressed-extensions
		      (url-basepath (or (url-filename urlobj) "") t)))
	 (fname (read-file-name "Filename to save as: "
				(or mm-download-directory "~/")
				stub-fname
				nil
				stub-fname)))
    (setq-default url-be-asynchronous t)
    (save-excursion
      (set-buffer url-working-buffer)
      (setq url-current-callback-data (list fname (current-buffer))
	    url-be-asynchronous t
	    url-current-callback-func 'w3-download-callback)
      (url-retrieve url))
    (setq-default url-be-asynchronous old-asynch)))

;;;###autoload
(defun w3-follow-link-other-frame (&optional p)
  "Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame))
    (let ((frm (make-frame)))
      (select-frame frm)
      (w3-follow-link p)))
   (t (w3-follow-link p))))

;;;###autoload
(defun w3-follow-link (&optional p)
  "Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive "P")
  (let* ((widget (widget-at (point)))
	 (href (and widget (widget-get widget 'href))))
    (cond
     ((null href) nil)
     ((or p w3-dump-to-disk)
      (w3-download-url href))
     (t
      (w3-fetch href)))))

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it"
  (interactive)
  (let (links-alist
	link-at-point
	choice
	(completion-ignore-case t))
    (setq link-at-point (widget-at (point))
	  link-at-point (and
			 link-at-point
			 (widget-get link-at-point 'href)
			 (w3-fix-spaces
			  (buffer-substring
			   (car (widget-get link-at-point 'title))
			   (cdr (widget-get link-at-point 'title))))))
    (w3-map-links (function
		   (lambda (widget arg)
		     (setq links-alist	(cons
					 (cons
					  (w3-fix-spaces
					   (buffer-substring-no-properties
					    (widget-get widget :from)
					    (widget-get widget :to)))
					  (widget-get widget 'href))
					 links-alist)))))
    (if (not links-alist) (error "No links in current document."))
    (setq links-alist (sort links-alist (function
					 (lambda (x y)
					   (string< (car x) (car y))))))
    ;; Destructively remove duplicate entries from links-alist.
    (let ((remaining-links links-alist))
      (while remaining-links
	(if (equal (car remaining-links) (car (cdr remaining-links)))
	    (setcdr remaining-links (cdr (cdr remaining-links)))
	  (setq remaining-links (cdr remaining-links)))))
    (setq choice (completing-read
		  (if link-at-point
		      (concat "Link (default "
			      (if (< (length link-at-point) 20)
				  link-at-point
				(concat
				 (substring link-at-point 0 17) "..."))
			      "): ")
		    "Link: ") links-alist nil t))
    (if (string= choice "")
	(w3-follow-link)
      (w3-fetch (cdr (assoc choice links-alist))))))

(defun w3-widget-motion-hook (widget)
  (assert widget nil "Bad data to w3-widget-motion-hook!  Bad hook bad!")
  (case w3-echo-link
    (text
     (message "%s" (w3-fix-spaces (buffer-substring (widget-get widget :from)
						    (widget-get widget :to)))))
    (url
     (if (widget-get widget 'href)
	 (message "%s" (widget-get widget 'href))))
    (otherwise nil)))

(defun w3-mode ()
  "Mode for viewing HTML documents.  If called interactively, will
display the current buffer as HTML.

Current keymap is:
\\{w3-mode-map}"
  (interactive)
  (or w3-setup-done (w3-do-setup))
  (if (interactive-p)
      (w3-preview-this-buffer)
    (let ((tmp (mapcar (function (lambda (x) (cons x (symbol-value x))))
		       w3-persistent-variables)))
      (kill-all-local-variables)
      (use-local-map w3-mode-map)
      (setq major-mode 'w3-mode)
      (setq mode-name "WWW")
      (mapcar (function (lambda (x) (set-variable (car x) (cdr x)))) tmp)
      (w3-mode-version-specifics)
      (w3-menu-install-menus)
      (make-local-hook 'widget-motion-hook)
      (add-hook 'widget-motion-hook 'w3-widget-motion-hook)
      (run-hooks 'w3-mode-hook)
      (widget-setup)
      (setq url-current-passwd-count 0
	    mode-line-format w3-modeline-format)
      (if (and w3-current-isindex (equal url-current-type "http"))
	  (setq mode-line-process "-Searchable")))))

(require 'mm)
(require 'url)
(require 'url-hash)
(require 'w3-parse)
(require 'w3-draw)
(require 'w3-auto)
(require 'w3-emulate)
(require 'w3-menu)
(require 'w3-mouse)
(provide 'w3)
