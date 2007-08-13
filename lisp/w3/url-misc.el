;;; url-misc.el --- Misc Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1997/01/21 21:14:56
;; Version: 1.9
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
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
(require 'url-parse)
(autoload 'Info-goto-node "info" "" t)

(defun url-info (url)
  ;; Fetch an info node
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (let* ((data (url-generic-parse-url url))
	 (fname (url-filename data))
	 (node (or (url-target data) "Top")))
    (if (and fname node)
	(Info-goto-node (concat "(" fname ")" node))
      (error "Malformed url: %s" url))))

(defun url-finger (url)
  ;; Find a finger reference
  (setq url-current-mime-headers '(("content-type" . "text/html"))
	url-current-mime-type "text/html")
  (set-buffer (get-buffer-create url-working-buffer))
  (let* ((urlobj (if (vectorp url) url
		   (url-generic-parse-url url)))
	 (host (or (url-host urlobj) "localhost"))
	 (port (or (url-port urlobj)
		   (cdr-safe (assoc "finger" url-default-ports))))
	 (user (url-unhex-string (url-filename urlobj)))
	 (proc (url-open-stream "finger" url-working-buffer host
				(string-to-int port))))
    (if (stringp proc)
	(message "%s" proc)
      (process-kill-without-query proc)
      (if (= (string-to-char user) ?/)
	  (setq user (substring user 1 nil)))
      (goto-char (point-min))
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>Finger information for " user "@" host "</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <h1>Finger information for " user "@" host "</h1>\n"
	      "  <hr>\n"
	      "  <pre>\n")
      (process-send-string proc (concat user "\r\n"))
      (while (memq (url-process-status proc) '(run open))
	(url-after-change-function)
	(url-accept-process-output proc))
      (goto-char (point-min))
      (url-replace-regexp "^Process .* exited .*code .*$" "")
      (goto-char (point-max))
      (insert "  </pre>\n"
	      " </body>\n"
	      "</html>\n"))))

(defun url-do-terminal-emulator (type server port user)
  (terminal-emulator
   (generate-new-buffer (format "%s%s" (if user (concat user "@") "") server))
   (case type
     (rlogin "rlogin")
     (telnet "telnet")
     (tn3270 "tn3270")
     (otherwise
      (error "Unknown terminal emulator required: %s" type)))
   (if user
       (case type
	 (rlogin
	  (list server "-l" user))
	 (telnet
	  (if user (message "Please log in as user: %s" user))
	  (if port
	      (list server port)
	    (list server)))
	 (tn3270
	  (if user (message "Please log in as user: %s" user))
	  (list server))))))

(defun url-generic-emulator-loader (url)
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (or (string-match "^\\([^:]+\\):/*\\(.*@\\)*\\([^/]*\\)/*" url)
      (error "Invalid URL: %s" url))
  (let* ((type (intern (downcase (match-string 1 url))))
	 (server (match-string 3 url))
	 (name (if (match-beginning 2)
		   (substring url (match-beginning 2) (1- (match-end 2)))))
	 (port (if (string-match ":" server)
		   (prog1
		       (substring server (match-end 0))
		     (setq server (substring server 0 (match-beginning 0)))))))
    (url-do-terminal-emulator type server port name)))

(fset 'url-rlogin 'url-generic-emulator-loader)
(fset 'url-telnet 'url-generic-emulator-loader)
(fset 'url-tn3270 'url-generic-emulator-loader)

(defun url-proxy (url)
  ;; Retrieve URL from a proxy.
  ;; Expects `url-using-proxy' to be bound to the specific proxy to use."
  (let (
	(urlobj (url-generic-parse-url url))
	(proxyobj (url-generic-parse-url url-using-proxy)))
    (url-http url-using-proxy url)
    (setq url-current-type (url-type urlobj)
	  url-current-user (url-user urlobj)
	  url-current-port (or (url-port urlobj)
			       (cdr-safe (assoc url-current-type
						url-default-ports)))
	  url-current-server (url-host urlobj)
	  url-current-file (url-filename urlobj))))

(defun url-x-exec (url)
  ;; Handle local execution of scripts.
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (string-match "x-exec:/+\\([^/]+\\)\\(/.*\\)" url)
  (let ((process-environment process-environment)
	(executable (url-match url 1))
	(path-info (url-match url 2))
	(query-string nil)
	(safe-paths url-local-exec-path)
	(found nil)
	(y nil)
	)
    (setq url-current-server executable
	  url-current-file path-info)
    (if (string-match "\\(.*\\)\\?\\(.*\\)" path-info)
	(setq query-string (url-match path-info 2)
	      path-info (url-match path-info 1)))
    (while (and safe-paths (not found))
      (setq y (expand-file-name executable (car safe-paths))
	    found (and (file-exists-p y) (file-executable-p y) y)
	    safe-paths (cdr safe-paths)))
    (if (not found)
	(url-retrieve (concat "www://error/nofile/" executable))
      (setq process-environment
	    (append
	     (list
	      "SERVER_SOFTWARE=x-exec/1.0"
	      (concat "SERVER_NAME=" (system-name))
	      "GATEWAY_INTERFACE=CGI/1.1"
	      "SERVER_PROTOCOL=HTTP/1.0"
	      "SERVER_PORT="
	      (concat "REQUEST_METHOD=" url-request-method)
	      (concat "HTTP_ACCEPT="
		      (mapconcat
		       (function
			(lambda (x)
			  (cond
			   ((= x ?\n) (setq y t) "")
			   ((= x ?:) (setq y nil) ",")
			   (t (char-to-string x))))) url-mime-accept-string
		       ""))
	      (concat "PATH_INFO=" (url-unhex-string path-info))
	      (concat "PATH_TRANSLATED=" (url-unhex-string path-info))
	      (concat "SCRIPT_NAME=" executable)
	      (concat "QUERY_STRING=" (url-unhex-string query-string))
	      (concat "REMOTE_HOST=" (system-name)))
	     (if (assoc "content-type" url-request-extra-headers)
		 (concat "CONTENT_TYPE=" (cdr
					  (assoc "content-type"
						 url-request-extra-headers))))
	     (if url-request-data
		 (concat "CONTENT_LENGTH=" (length url-request-data)))
	     process-environment))
      (and url-request-data (insert url-request-data))
      (setq y (call-process-region (point-min) (point-max) found t t))
      (goto-char (point-min))
      (delete-region (point) (progn (skip-chars-forward " \t\n") (point)))
      (cond
       ((url-mime-response-p) nil)	; Its already got an HTTP/1.0 header
       ((null y)			; Weird exit status, whassup?
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: " url-package-name "/x-exec\n"))	
       ((= 0 y)				; The shell command was successful
	(insert "HTTP/1.0 200 Document follows\n"
		"Server: " url-package-name "/x-exec\n"))	
       (t				; Non-zero exit status is bad bad bad
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: " url-package-name "/x-exec\n"))))))

;; ftp://ietf.org/internet-drafts/draft-masinter-url-data-02.txt
(defun url-data (url)
  (set-buffer (get-buffer-create url-working-buffer))
  (let ((content-type nil)
	(encoding nil)
	(data nil))
    (cond
     ((string-match "^data:\\([^;,]*\\);*\\([^,]*\\)," url)
      (setq content-type (match-string 1 url)
	    encoding (match-string 2 url)
	    data (url-unhex-string (substring url (match-end 0))))
      (if (= 0 (length content-type)) (setq content-type "text/plain"))
      (if (= 0 (length encoding)) (setq encoding "8bit")))
     (t nil))
    (setq url-current-content-length (length data)
	  url-current-mime-type content-type
	  url-current-mime-encoding encoding
	  url-current-mime-headers (list (cons "content-type" content-type)
					 (cons "content-encoding" encoding)))
    (and data (insert data))))

(provide 'url-misc)
