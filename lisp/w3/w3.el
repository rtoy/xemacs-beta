;;; w3.el --- Main functions for emacs-w3 on all platforms/versions
;; Author: wmperry
;; Created: 1997/03/07 16:44:12
;; Version: 1.93
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
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


(require 'w3-sysdp)
(require 'mule-sysdp)

(or (featurep 'efs)
    (featurep 'efs-auto)
    (condition-case ()
	(require 'ange-ftp)
      (error nil)))

(require 'cl)
(require 'css)
(require 'w3-vars)
(eval-and-compile
  (require 'w3-display))


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
      (if fmt
	  (setq fmt (concat "%s" (car fmt)))
	(setq fmt (concat "%s" (url-file-extension
				(url-filename url-current-object)))))))
    (if (null view)
	(setq view 'indented-text-mode))
    (cond
     ((symbolp view)
      (if (not (memq view '(w3-prepare-buffer w3-print w3-source
					      w3-default-local-file
					      mm-multipart-viewer)))
	  (let ((bufnam (url-generate-new-buffer-name
			 (file-name-nondirectory
			  (or (url-filename url-current-object)
			      "Unknown")))))
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
	    (proc nil))
	(if (url-file-directly-accessible-p (url-view-url t))
	    (make-symbolic-link (url-filename url-current-object) fname t)
	  (mule-write-region-no-coding-system (point-min) (point-max) fname))
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
    (mule-write-region-no-coding-system (point-min) (point-max) file)
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
hypertext document."
  (interactive "FLocal file: ")
  (setq fname (expand-file-name fname))
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch (concat "file:" fname)))

;;;###autoload
(defun w3-find-file (fname)
  "Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document."
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

;; Ripped off from red gnus
(defun w3-find-etc-directory (package &optional file)
  "Go through the path and find the \".../etc/PACKAGE\" directory.
If FILE, find the \".../etc/PACKAGE\" file instead."
  (let ((path load-path)
	dir result)
    ;; We try to find the dir by looking at the load path,
    ;; stripping away the last component and adding "etc/".
    (while path
      (if (and (car path)
	       (file-exists-p
		(setq dir (concat
			   (file-name-directory
			    (directory-file-name (car path)))
			   "etc/" package 
			   (if file "" "/"))))
	       (or file (file-directory-p dir)))
	  (setq result dir
		path nil)
	(setq path (cdr path))))
    result))

(defun w3-url-completion-function (string predicate function)
  (if (not w3-setup-done) (w3-do-setup))
  (cond
   ((eq function nil)
    (let ((list nil))
      (cl-maphash (function (lambda (key val)
			      (setq list (cons (cons key val)
					       list))))
		  url-global-history-hash-table)
      (try-completion string (nreverse list) predicate)))
   ((eq function t)
    (let ((stub (concat "^" (regexp-quote string)))
	  (retval nil))
      (cl-maphash
       (function
	(lambda (url time)
	  (if (string-match stub url)
	      (setq retval (cons url retval)))))
       url-global-history-hash-table)
      retval))
   ((eq function 'lambda)
    (and url-global-history-hash-table
	 (cl-gethash string url-global-history-hash-table)
	 t))
   (t
    (error "w3-url-completion-function very confused."))))

(defun w3-read-url-with-default ()
  (url-do-setup)
  (let* ((completion-ignore-case t)
	 (default
	   (cond
	    ((null w3-fetch-with-default) nil)
	    ((eq major-mode 'w3-mode)
	     (or (and current-prefix-arg (w3-view-this-url t))
		 (url-view-url t)))
	    ((url-get-url-at-point)
	     (url-get-url-at-point))
	    (t "http://www.")))
	 (url nil))
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
(defun w3-fetch (&optional url target)
  "Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead."
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
      (setq url (concat (file-name-directory (url-filename
					      url-current-object))
 			(substring url 4))))
  ;; In the common case, this is probably cheaper than searching.
  (while (= (string-to-char url) ? )
    (setq url (substring url 1)))
  (or target (setq target w3-base-target))
  (if (stringp target)
      (setq target (intern (downcase target))))
  (and target
       (let ((window-distance (cdr-safe (assq target w3-target-window-distances))))
	 (if (numberp window-distance)
	     (other-window window-distance)
	   (error "target %S not found." target))))
  (cond
   ((= (string-to-char url) ?#)
    (w3-relative-link url))
   ((or (and (interactive-p) current-prefix-arg) w3-dump-to-disk)
    (w3-download-url url))
   (t
    (let ((x (url-view-url t))
	  (lastbuf (current-buffer))
	  (buf (url-buffer-visiting url)))
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
	  (let* ((status (url-retrieve url))
		 (cached (car status))
		 (url-working-buffer (cdr status)))
	    (if w3-track-last-buffer
		(setq w3-last-buffer (get-buffer url-working-buffer)))
	    (if (get-buffer url-working-buffer)
		(cond
		 ((and url-be-asynchronous (not cached))
		  (save-excursion
		    (set-buffer url-working-buffer)
		    (if x
			(w3-history-push x (url-view-url t)))
		    (setq w3-current-last-buffer lastbuf)))
		 (t
		  (w3-history-push x url)
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
(defvar w3-history-stack nil
  "History stack viewing history.
This is an assoc list, with the oldest items first.
Each element is a cons cell of (url . timeobj), where URL
is the normalized URL (default ports removed, etc), and TIMEOBJ is
a standard Emacs time.  See the `current-time' function documentation
for information on this format.")

(defun w3-history-find-url-internal (url)
  "Search in the history list for URL.
Returns a cons cell, where the car is the 'back' node, and
the cdr is the 'next' node."
  (let* ((node (assoc url w3-history-stack))
	 (next (cadr (memq node w3-history-stack)))
	 (last nil)
	 (temp nil)
	 (todo w3-history-stack))
    ;; Last node is a little harder to find without using back links
    (while (and (not last) todo)
      (if (string= (caar todo) url)
	  (setq last (or temp 'none))
	(setq temp (pop todo))))
    (cons (if (not (symbolp last)) last)
	  next)))

(defun w3-history-forward ()
  "Go forward in the history from this page"
  (interactive)
  (let ((next (cadr (w3-history-find-url-internal (url-view-url t))))
	(w3-reuse-buffers 'yes))
    (if next
	(w3-fetch next))))

(defun w3-history-backward ()
  "Go backward in the history from this page"
  (interactive)
  (let ((last (caar (w3-history-find-url-internal (url-view-url t))))
	(w3-reuse-buffers 'yes))
    (if last
	(w3-fetch last))))

(defun w3-history-push (referer url)
  "REFERER is the url we followed this link from.  URL is the link we got to."
  (if (not referer)
      (setq w3-history-stack (list (cons url (current-time))))
    (let ((node (memq (assoc referer w3-history-stack) w3-history-stack)))
      (if node
	  (setcdr node (list (cons url (current-time))))
	(setq w3-history-stack (append w3-history-stack
				       (list
					(cons url (current-time)))))))))

(defalias 'w3-add-urls-to-history 'w3-history-push)
(defalias 'w3-backward-in-history 'w3-history-backward)
(defalias 'w3-forward-in-history 'w3-history-forward)


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
	     (case-fold-search t)
	     (possible-lastmod (save-excursion
				 (goto-char (point-min))
				 (if (re-search-forward "^Last modified:\\(.*\\)" nil t)
				     (buffer-substring (match-beginning 1)
						       (match-end 1)))))
	     (attributes (url-file-attributes url))
	     (lastmod (or (cdr-safe (assoc "last-modified"
					   url-current-mime-headers))
			  (nth 5 attributes)))
	     (hdrs url-current-mime-headers)
	     (size (or (cdr (assoc "content-length" url-current-mime-headers))
		       (point-max)))
	     (info w3-current-metainfo))
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-current-can-be-cached nil)
	(erase-buffer)
	(cond
	 ((stringp lastmod) nil)
	 ((equal '(0 . 0) lastmod) (setq lastmod possible-lastmod))
	 ((consp lastmod) (setq lastmod (current-time-string lastmod)))
	 (t (setq lastmod possible-lastmod)))
	(insert "<html>\n"
		" <head>\n"
		"  <title>Document Information</title>\n"
		" </head>\n"
		" <body\n"
		"  <table border>\n"
		"   <tr><th colspan=2>Document Information</th></tr>\n"
		"   <tr><td>Title:</td><td>" title "</td></tr>\n"
		"   <tr><td>Location:</td><td>" url "</td></tr>\n"
		"   <tr><td>Size:</td><td>" (url-pretty-length
					     (if (stringp size)
						 (string-to-int size)
					       size)) "</td></tr>\n"
		"   <tr><td>Last Modified:</td><td>" (or lastmod "None Given")
		"</td></tr>\n")
	(if hdrs
	    (let* ((maxlength (car (sort (mapcar (function (lambda (x)
							     (length (car x))))
						 hdrs)
					 '>)))
		   (fmtstring (format "   <tr><td align=right>%%%ds:</td><td>%%s</td></tr>" maxlength)))
	      (insert "  <tr><th>MetaInformation</th></tr>\n"
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
		       "\n"))))

	;; FIXME!!! Need to reimplement showing rel/rev links for the new
	;; storage format.
	
	(if info
	    (let* ((maxlength (car (sort (mapcar (function (lambda (x)
							     (length (car x))))
						 info)
					 '>)))
		   (fmtstring (format "   <tr><td>%%%ds:</td><td>%%s</td></tr>" maxlength)))
	      (insert "   <tr><th>Miscellaneous Variables</th></tr>\n")
	      (while info
		(insert (format fmtstring (capitalize (caar info))
				(cdar info)) "\n")
		(setq info (cdr info))
		)
	      )
	  )
	(insert "  </table>\n"
		" </body>\n"
		"</html>\n")))))

(defun w3-truncate-menu-item (string)
  (if (<= (length string) w3-max-menu-width)
      string
    (concat (substring string 0 w3-max-menu-width) "$")))

(defun w3-insert-formatted-url (p)
  "Insert a formatted url into a buffer.  With prefix arg, insert the url
under point."
  (interactive "P")
  (let (buff str)
    (cond
     (p
      (setq p (widget-at (point)))
      (or p (error "No url under point"))
      (setq str (format "<a href=\"%s\">%s</a>" (widget-get p 'href)
			(read-string "Link text: "
				     (buffer-substring
                                      (widget-get p :from)
                                      (widget-get p :to))))))
     (t
      (setq str (format "<a href=\"%s\">%s</a>" (url-view-url t)
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
  (cond
   ((and (event-point e)
	 (widget-at (event-point e)))
    (widget-button-click e))
   ((and (fboundp 'event-glyph)
	 (event-glyph e)
	 (glyph-property (event-glyph e) 'widget))
    (widget-button-click e))))
   
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
  (let ((x '(w3 mule-sysdp w3-e19 mm url w3-xemac w3-toolbar font)))
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

(defun w3-source-document (under)
  "View this document's source"
  (interactive "P")
  (let* ((url (if under (w3-view-this-url) (url-view-url t)))
	 (src
	  (cond
	   ((null url)
	    (error "No URL found!"))
	   ((and under (null url)) (error "No link at point!"))
	   ((and (not under) (equal url-current-mime-type "text/plain"))
	    (buffer-string))
	   ((and (not under) w3-current-source) w3-current-source)
	   (t
	    (prog2
		(url-retrieve url)
		(buffer-string)
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
      (set-buffer (get-buffer-create tmp))
      (insert src)
      (put-text-property (point-min) (point-max) 'w3-base url)
      (goto-char (point-min))
      (setq buffer-file-truename url
	    buffer-file-name url)
      ;; Null filename bugs `set-auto-mode' in Mule ...
      (condition-case ()
 	  (set-auto-mode)
	(error nil))
      (setq buffer-file-truename nil
	    buffer-file-name nil)
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
	 (case-fold-search t)
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
		(ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((equal "PostScript" format)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(setq content-type "application/postscript")
		(ps-spool-buffer-with-faces)
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
		(w3-parse-tree-to-latex (w3-parse-buffer (current-buffer))
					url)))
	     ((equal "LaTeX Source" format)
	      (setq content-type "application/x-latex; charset=iso-8859-1")
	      (w3-parse-tree-to-latex w3-current-parse url)))
	    (buffer-string))))
    (funcall w3-mail-command)
    (mail-subject)
    (if (and (boundp 'mime/editor-mode-flag) mime/editor-mode-flag)
        (insert format " from <URL: " url ">")
      (insert format " from <URL: " url ">\n"
              "Mime-Version: 1.0\n"
              "Content-transfer-encoding: 8bit\n"
              "Content-type: " content-type))
    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (if (and (boundp 'mime/editor-mode-flag) mime/editor-mode-flag)
        (insert (format mime-tag-format content-type) "\n"))
    (save-excursion
      (insert str))
    (cond ((equal "HTML Source" format)
           (if (or (search-forward "<head>" nil t)
		   (search-forward "<html>" nil t))
	       (insert "\n"))
           (insert (format "<base href=\"%s\">" url))))
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
	  (extn (url-file-extension
		 (url-filename url-current-object))))
      (if (assoc extn url-uncompressor-alist)
	  (setq extn (url-file-extension
		      (substring (url-filename url-current-object)
				 0 (- (length extn))))))
      (if w3-source
	  (setq url-current-mime-viewer '(("viewer" . w3-source))))
      (if (not url-current-mime-viewer)
	  (setq url-current-mime-viewer
		(mm-mime-info (or url-current-mime-type
				  (mm-extension-to-mime extn)) nil 5)))
      (if url-current-mime-viewer
	  (setq cont (append cont '(w3-pass-to-viewer)))
	(setq cont (append cont (list 'w3-prepare-buffer))))
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
  (or url-current-object
      (error "Not a URL-based buffer"))
  (let ((type (url-type url-current-object)))
    (cond
     ((equal type "file")
      (find-file (url-filename url-current-object)))
     ((equal type "ftp")
      (find-file
       (format "/%s@%s:%s"
	       (url-user url-current-object)
	       (url-host url-current-object)
	       (url-filename url-current-object))))
     (t (message "Sorry, I can't get that file so you can alter it.")))))

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

(defvar w3-mime-list-for-code-conversion
  '("text/plain" "text/html")
  "List of MIME types that require Mules' code conversion.")

(defun w3-convert-code-for-mule (mmtype)
  "Convert current data into the appropriate coding system"
  (and (or (not mmtype)
	   (member mmtype w3-mime-list-for-code-conversion))
       (mule-code-convert-region
	(point-min) (point-max)
	(mule-detect-coding-version (point-min) (point-max)))))

(defun w3-sentinel (&optional proc string)
  (set-buffer url-working-buffer)
  (if (or (stringp proc)
	  (bufferp proc)) (setq w3-current-last-buffer proc))
  (remove-hook 'after-change-functions 'url-after-change-function)
  (if url-be-asynchronous
      (progn
	(url-clean-text)
	(cond
	 ((not (get-buffer url-working-buffer)) nil)
	 ((url-mime-response-p) (url-parse-mime-headers)))
	(if (not url-current-mime-type)
	    (setq url-current-mime-type (or (mm-extension-to-mime
					     (url-file-extension
					      (url-filename
					       url-current-object)))
					    "text/html")))))
  (if (not (string-match "^www:" (or (url-view-url t) "")))
      (w3-convert-code-for-mule url-current-mime-type))
      
  (let ((x (w3-build-continuation))
	(url (url-view-url t)))
    (while x
      (funcall (pop x)))))

(defun w3-show-history-list ()
  "Format the url-history-list prettily and show it to the user"
  (interactive)
  (w3-fetch "www://auto/history"))

(defun w3-save-as (&optional type)
  "Save a document to the local disk"
  (interactive)
  (save-excursion
    (let* ((completion-ignore-case t)
	   (format (or type (completing-read
			     "Format: "
			     '(("HTML Source")
			       ("Formatted Text")
			       ("LaTeX Source")
			       ("PostScript")
			       ("Binary"))
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
	      (url-retrieve url))	; Get the document if necessary
	  (let ((txt w3-current-source))
	    (set-buffer (get-buffer-create url-working-buffer))
	    (erase-buffer)
	    (insert txt)))
	(goto-char (point-min))
	(if (re-search-forward "<head>" nil t)
	    (insert "\n"))
	(insert (format "<BASE HREF=\"%s\">\n" url)))
       ((or (equal "Formatted Text" format)
	    (equal "" format))
	nil)				; Do nothing - we have the text already
       ((equal "PostScript" format)
	(let ((ps-spool-buffer-name " *w3-temp*"))
	  (if (get-buffer ps-spool-buffer-name)
	      (kill-buffer ps-spool-buffer-name))
	  (ps-spool-buffer-with-faces)
	  (set-buffer ps-spool-buffer-name)))
       ((equal "LaTeX Source" format)
	(w3-parse-tree-to-latex w3-current-parse url)
	(insert-buffer url-working-buffer)))
      (write-region (point-min) (point-max) fname))))


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
  (if (featurep 'emacspeak)
      (condition-case ()
	  (progn
	    (require 'dtk-css-speech)
	    (require 'w3-speak))))
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
  (let* (querystring			; The string to send to the server
	 (data
	  (cond
	   ((null w3-current-isindex)
	    (let ((rels (cdr-safe (assq 'rel w3-current-links)))
		  val cur)
	      (while rels
		(setq cur (car rels)
		      rels (cdr rels))
		(if (and (or (string-match "^isindex$" (car cur))
			     (string-match "^index$" (car cur)))
			 (plist-get (cadr cur) 'href))
		    (setq val (plist-get (cadr cur) 'href)
			  rels nil))
		)
	      (if val
		  (cons val "Search on (+ separates keywords): "))))
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
  (setq url-current-mime-type "text/html")
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
   ((equal type "history")
    (if (not url-history-list)
	(url-retrieve "www://error/nohist")
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> History List For This Session of W3</title>"
	      "\n\t</head>\n\t<body>\n\t\t<div>\n\t\t\t<h1>"
	      "History List For This Session of W3</h1>\n\t\t\t<ol>\n")
      (cl-maphash
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
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil))
    (if (not base)
	(setq url-current-object
	      (url-generic-parse-url (concat "file:"
					     (buffer-file-name buffer))))
      (setq url-current-object base))))

(defun w3-internal-url (url)
  ;; Handle internal urls (previewed buffers, etc)
  (if (not (string-match "www:/+\\([^/]+\\)/\\(.*\\)" url))
      (w3-fetch "www://error/")
    (let ((type (url-match url 1))
	  (data (url-match url 2)))
      (set-buffer (get-buffer-create url-working-buffer))
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
  (w3-ff (url-filename url-current-object)))

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
     (widget
      (widget-echo-help (point)))
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
	(setq found (list (concat "mailto:" found))))
    (while (and x (not found))
      (setq y (car x)
	    x (cdr x)
	    found (cdr-safe (assoc "made" y))))
    (if found
	(let ((possible nil)
	      (href nil))
	  (setq x (car found))		; Fallback if no mail(to|server) found
	  (while found
	    (setq href (plist-get (pop found) 'href))
	    (if (and href (string-match "^mail[^:]+:" href))
		(setq possible (cons href possible))))
	  (case (length possible)
	    (0				; No mailto links found
	     (w3-fetch x))		; fall back onto first 'made' link
	    (1				; Only one found, get it
	     (w3-fetch (car possible)))
	    (otherwise
	     (w3-fetch (completing-read "Choose an address: "
					(mapcar 'list possible)
					nil t (car possible))))))
      (message "Could not automatically determine authors address, sorry."))))

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
	(parent nil))
    (while (setq cur (next-single-property-change cur 'button))
      (setq widget (widget-at cur)
	    parent (and widget (widget-get widget :parent)))
      ;; Check to see if its a push widget, its got the correct callback,
      ;; and actually has a URL.  Remember the url as a side-effect of the
      ;; test for later use.
      (cond
       ((and widget (widget-get widget 'href))
	(funcall function widget maparg))
       ((and parent (widget-get parent 'href))
	(funcall function parent maparg))
       (t nil)))))

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
  )

(defvar w3-loaded-stylesheets nil
  "A list of all the stylesheets Emacs-W3 loaded at startup.")

(defun w3-find-default-stylesheets ()
  (setq w3-loaded-stylesheets nil)
  (let* ((lightp (w3-color-light-p 'default))
	 (longname (if lightp "stylesheet-light" "stylesheet-dark"))
	 (shortname (if lightp "light.css" "dark.css"))
	 (directories (list
		       data-directory
		       (concat data-directory "w3/")
		       (expand-file-name "../../w3" data-directory)
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
		w3-loaded-stylesheets (cons cur w3-loaded-stylesheets)
		w3-user-stylesheet (css-parse (concat "file:" cur) nil
				     w3-user-stylesheet))))
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
  (add-minor-mode 'w3-lynx-emulation-minor-mode " Lynx"
		  w3-lynx-emulation-minor-mode-map)
  
  (setq url-package-version w3-version-number
	url-package-name "Emacs-W3")

  (w3-emit-image-warnings-if-necessary)
		   
  (cond
   ((memq system-type '(ms-dos ms-windows))
    (setq w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hot"))
	  ))
   ((memq system-type '(axp-vms vax-vms))
    (setq w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hotlist-default"))
	  ))
   (t 
    (setq w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/.mosaic-hotlist-default"))
	  )))
  
  ; Set up a hook that will save the history list when
  ; exiting emacs
  (add-hook 'kill-emacs-hook 'w3-kill-emacs-func)

  (mm-parse-mailcaps)
  (mm-parse-mimetypes)

  ; Load in the hotlist if they haven't set it already
  (or w3-hotlist (w3-parse-hotlist))

  ; Set the default home page, honoring their defaults, then
  ; the standard WWW_HOME, then default to the documentation @ IU
  (or w3-default-homepage
      (setq w3-default-homepage
	    (or (getenv "WWW_HOME")
		"http://www.cs.indiana.edu/elisp/w3/docs.html")))

  ; Set up the entity definition for PGP and PEM authentication

  (run-hooks 'w3-load-hook)
  (setq w3-setup-done t))

(defun w3-mark-link-as-followed (ext dat)
  ;; Mark a link as followed
  (message "Reimplement w3-mark-link-as-followed"))

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
	      (enable-multibyte-characters t) ; mule 2.4
	      (buffer-file-coding-system mule-no-coding-system) ; mule 2.4
	      (file-coding-system mule-no-coding-system) ; mule 2.3
	      (mc-flag t))		; mule 2.3
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
	 (stub-fname (url-basepath (or (url-filename urlobj) "") t))
	 (dir (or mm-download-directory "~/"))
	 (fname (expand-file-name
		 (read-file-name "Filename to save as: "
				 dir
				 stub-fname
				 nil
				 stub-fname) dir)))
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

;;; FIXME!  Need to rewrite these so that we can pass a predicate to 
(defun w3-widget-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (widget-forward arg))

(defun w3-widget-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (w3-widget-forward (- arg)))

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
			 (widget-get link-at-point :from)
			 (widget-get link-at-point :to)
			 (w3-fix-spaces
			  (buffer-substring
			   (widget-get link-at-point :from)
			   (widget-get link-at-point :to)))))
    (w3-map-links (function
		   (lambda (widget arg)
		     (if (and (widget-get widget :from)
			      (widget-get widget :to))
			 (setq links-alist (cons
					    (cons
					     (w3-fix-spaces
					      (buffer-substring-no-properties
					       (widget-get widget :from)
					       (widget-get widget :to)))
					     (widget-get widget 'href))
					    links-alist))))))
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
    (let ((match (try-completion choice links-alist)))
      (cond
       ((eq t match)			; We have an exact match
	(setq choice (cdr (assoc choice links-alist))))
       ((stringp match)
	(setq choice (cdr (assoc match links-alist))))
       (t (setq choice nil)))
      (if choice
	  (w3-fetch choice)))))

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
      (run-hooks 'w3-mode-hook)
      (widget-setup)
      (setq url-current-passwd-count 0
	    inhibit-read-only nil
	    truncate-lines t
	    mode-line-format w3-modeline-format)
      (if w3-current-isindex
	  (setq mode-line-process "-Searchable")))))

(require 'mm)
(require 'url)
(require 'w3-parse)
(require 'w3-display)
(require 'w3-auto)
(require 'w3-emulate)
(require 'w3-menu)
(require 'w3-mouse)
(provide 'w3)
