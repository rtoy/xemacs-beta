;;; Simple POP (RFC 1460) client for VM
;;; Copyright (C) 1993, 1994 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-pop)

;; Nothing fancy here.
;; Our goal is to drag the mail from the POP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
(defun vm-pop-move-mail (source destination)
  (let ((process nil)
	(folder-type vm-folder-type)
	(saved-password t)
	(m-per-session vm-pop-messages-per-session)
	(b-per-session vm-pop-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(popdrop (vm-safe-popdrop-string source))
	mailbox-count mailbox-size message-size response
	n retrieved retrieved-bytes process-buffer)
    (unwind-protect
	(catch 'done
	  (if handler
	      (throw 'done
		     (funcall handler 'vm-pop-move-mail source destination)))
	  (setq process (vm-pop-make-session source))
	  (or process (throw 'done nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    ;; find out how many messages are in the box.
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process)
		  mailbox-count (nth 0 response)
		  mailbox-size (nth 1 response))
	    ;; forget it if the command fails
	    ;; or if there are no messages present.
	    (if (or (null mailbox-count)
		    (< mailbox-count 1))
		(throw 'done nil))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved 0 retrieved-bytes 0)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (if (or vm-pop-max-message-size
		      b-per-session)
		  (progn
		    (vm-pop-send-command process (format "LIST %d" n))
		    (setq message-size
			  (vm-pop-read-list-response process))))
	      (if (and (integerp vm-pop-max-message-size)
		       (> message-size vm-pop-max-message-size)
		       (progn
			 (setq response
			       (if vm-pop-ok-to-ask
				   (vm-pop-ask-about-large-message process
								   message-size
								   n)
				 'skip))
			 (not (eq response 'retrieve))))
		  (if (eq response 'delete)
		      (progn
			(message "Deleting message %d..." n)
			(vm-pop-send-command process (format "DELE %d" n))
			(and (null (vm-pop-read-response process))
			     (throw 'done (not (equal retrieved 0)))))
		    (if vm-pop-ok-to-ask
			(message "Skipping message %d..." n)
	     (message "Skipping message %d in %s, too large (%d > %d)..."
		      n popdrop message-size vm-pop-max-message-size)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count popdrop)
		(vm-pop-send-command process (format "RETR %d" n))
		(and (null (vm-pop-read-response process))
		     (throw 'done (not (equal retrieved 0))))
		(and (null (vm-pop-retrieve-to-crashbox process destination))
		     (throw 'done (not (equal retrieved 0))))
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(vm-pop-send-command process (format "DELE %d" n))
		;; DELE can't fail but Emacs or this code might
		;; blow a gasket and spew filth down the
		;; connection, so...
		(and (null (vm-pop-read-response process))
		     (throw 'done (not (equal retrieved 0)))))
	      (vm-increment n))
	     (not (equal retrieved 0)) ))
      (if process
	  (vm-pop-end-session process)))))

(defun vm-pop-check-mail (source)
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	response)
    (unwind-protect
	(save-excursion
	  (catch 'done
	    (if handler
		(throw 'done
		       (funcall handler 'vm-pop-check-mail source)))
	    (setq process (vm-pop-make-session source))
	    (or process (throw 'done nil))
	    (set-buffer (process-buffer process))
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process))
	    (if (null response)
		nil
	      (not (equal 0 (car response))))))
      (and process (vm-pop-end-session process)))))

(defun vm-pop-make-session (source)
  (let ((process-to-shutdown nil)
	process
	(saved-password t)
	(popdrop (vm-safe-popdrop-string source))
	greeting timestamp
	host port auth user pass source-list process-buffer)
    (unwind-protect
	(catch 'done
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]+\\):?")
		host (nth 0 source-list)
		port (nth 1 source-list)
		auth (nth 2 source-list)
		user (nth 3 source-list)
		pass (nth 4 source-list))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in POP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in POP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error
	       "No authentication method in POP maildrop specification, \"%s\""
	       source))
	  (if (null user)
	      (error "No user in POP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in POP maildrop specification, \"%s\""
		     source))
	  (if (equal pass "*")
	      (progn
		(setq pass (car (cdr (assoc source vm-pop-passwords))))
		(if (null pass)
		    (if (null vm-pop-ok-to-ask)
			(progn (message "Need password for %s" popdrop)
			       (throw 'done nil))
		      (setq pass
			    (vm-read-password
			     (format "POP password for %s: "
				     popdrop))
			    vm-pop-passwords (cons (list source pass)
						   vm-pop-passwords)
			    saved-password t)))))
	  ;; get the trace buffer
	  (setq process-buffer
		(get-buffer-create (format "trace of POP session to %s" host)))
	  ;; Tell XEmacs/MULE not to mess with the text.
	  (and (fboundp 'set-file-coding-system)
	       (set-file-coding-system 'binary t))
	  ;; clear the trace buffer of old output
	  (save-excursion
	    (set-buffer process-buffer)
	    (erase-buffer))
	  ;; open the connection to the server
	  (setq process (open-network-stream "POP" process-buffer host port))
	  (and (null process) (throw 'done nil))
	  (process-kill-without-query process)
	  (save-excursion
	    (set-buffer process-buffer)
	    (make-local-variable 'vm-pop-read-point)
	    (setq vm-pop-read-point (point-min))
	    (if (null (setq greeting (vm-pop-read-response process t)))
		(progn (delete-process process)
		       (throw 'done nil)))
	    (setq process-to-shutdown process)
	    ;; authentication
	    (cond ((equal auth "pass")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "PASS %s" pass))
		   (if (null (vm-pop-read-response process))
		       (progn
			 (if saved-password
			     (setq vm-pop-passwords
				   (delete (list source pass)
					   vm-pop-passwords)))
			 (throw 'done nil))))
		  ((equal auth "rpop")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "RPOP %s" pass))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  ((equal auth "apop")
		   (setq timestamp (vm-parse greeting "[^<]+\\(<[^>]+>\\)")
			 timestamp (car timestamp))
		   (if (null timestamp)
		       (progn
			 (goto-char (point-max))
   (insert-before-markers "<<< ooops, no timestamp found in greeting! >>>\n")
			 (throw 'done nil)))
		   (vm-pop-send-command
		    process
		    (format "APOP %s %s"
			    user
			    (vm-pop-md5 (concat timestamp pass))))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  (t (error "Don't know how to authenticate with %s" auth)))
	    (setq process-to-shutdown nil)
	    process ))
      (if process-to-shutdown
	  (vm-pop-end-session process-to-shutdown)))))

(defun vm-pop-end-session (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (vm-pop-send-command process "QUIT")
    (vm-pop-read-response process)
    (if (fboundp 'add-async-timeout)
	(add-async-timeout 2 'delete-process process)
      (run-at-time 2 nil 'delete-process process))))

(defun vm-pop-send-command (process command)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert-before-markers "PASS <omitted>\r\n")
    (insert-before-markers command "\r\n"))
  (setq vm-pop-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun vm-pop-read-response (process &optional return-response-string)
  (let ((case-fold-search nil)
	 match-end)
    (goto-char vm-pop-read-point)
    (while (not (search-forward "\r\n" nil t))
      (accept-process-output process)
      (goto-char vm-pop-read-point))
    (setq match-end (point))
    (goto-char vm-pop-read-point)
    (if (not (looking-at "+OK"))
	(progn (setq vm-pop-read-point match-end) nil)
      (setq vm-pop-read-point match-end)
      (if return-response-string
	  (buffer-substring (point) match-end)
	t ))))

(defun vm-pop-read-past-dot-sentinel-line (process)
  (let ((case-fold-search nil))
    (goto-char vm-pop-read-point)
    (while (not (search-forward "^.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let ((opoint (point)))
	(accept-process-output process)
	(goto-char opoint)))
    (setq vm-pop-read-point (point))))

(defun vm-pop-read-stat-response (process)
  (let ((response (vm-pop-read-response process t))
	list)
    (setq list (vm-parse response "\\([^ ]+\\) *"))
    (list (string-to-int (nth 1 list)) (string-to-int (nth 2 list)))))

(defun vm-pop-read-list-response (process)
  (let ((response (vm-pop-read-response process t)))
    (string-to-int (nth 2 (vm-parse response "\\([^ ]+\\) *")))))

(defun vm-pop-ask-about-large-message (process size n)
  (let ((work-buffer nil)
	(pop-buffer (current-buffer))
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (vm-pop-send-command process (format "TOP %d %d" n 0))
	    (if (vm-pop-read-response process)
		(progn
		  (setq start vm-pop-read-point)
		  (vm-pop-read-past-dot-sentinel-line process)
		  (setq end vm-pop-read-point)
		  (setq work-buffer (generate-new-buffer "*pop-glop*"))
		  (set-buffer work-buffer)
		  (insert-buffer-substring pop-buffer start end)
		  (forward-line -1)
		  (delete-region (point) (point-max))
		  (vm-pop-cleanup-region (point-min) (point-max))
		  (vm-display-buffer work-buffer)
		  (setq minibuffer-scroll-window (selected-window))
		  (goto-char (point-min))
		  (if (re-search-forward "^Received:" nil t)
		      (progn
			(goto-char (match-beginning 0))
			(vm-reorder-message-headers
			 nil vm-visible-headers
			 vm-invisible-header-regexp)))
		  (set-window-point (selected-window) (point))))
	    (if (y-or-n-p (format "Message %d, size = %d, retrieve? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from popdrop? " n size))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-pop-retrieve-to-crashbox (process crash)
  (let ((start vm-pop-read-point) end)
    (goto-char start)
    (while (not (re-search-forward "^\\.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let ((opoint (point)))
	(accept-process-output process)
	(goto-char opoint)))
    (setq vm-pop-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker))
    (vm-pop-cleanup-region start end)
    ;; Some POP servers strip leading and trailing message
    ;; separators, some don't.  Figure out what kind we're
    ;; talking to and do the right thing.
    (if (eq (vm-get-folder-type nil start end) 'unknown)
	(progn
	  (vm-munge-message-separators vm-folder-type start end)
	  (goto-char start)
	  ;; avoid the consing and stat() call for all but babyl
	  ;; files, since this will probably slow things down.
	  ;; only babyl files have the folder header, and we
	  ;; should only insert it if the crash box is empty.
	  (if (and (eq vm-folder-type 'babyl)
		   (let ((attrs (file-attributes crash)))
		     (or (null attrs) (equal 0 (nth 7 attrs)))))
	      (let ((opoint (point)))
		(vm-convert-folder-header nil vm-folder-type)
		;; if start is a marker, then it was moved
		;; forward by the insertion.  restore it.
		(setq start opoint)
		(goto-char start)
		(vm-skip-past-folder-header)))
	  (insert (vm-leading-message-separator))
	  ;; this will not find the trailing message separator but
	  ;; for the Content-Length stuff counting from eob is
	  ;; the same thing in this case.
	  (vm-convert-folder-type-headers nil vm-folder-type)
	  (goto-char end)
	  (insert-before-markers (vm-trailing-message-separator))))
    ;; Set file type to binary for DOS/Windows.  I don't know if
    ;; this is correct to do or not; it depends on whether the
    ;; the CRLF or the LF newline convention is used on the inbox
    ;; associated with this crashbox.  This setting assumes the LF
    ;; newline convention is used.
    (let ((buffer-file-type t))
      (write-region start end crash t 0))
    (delete-region start end)
    t ))

(defun vm-pop-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t))
    (goto-char start)
    ;; chop leading dots
    (while (and (< (point) end) (re-search-forward "^\\."  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-pop-md5 (string)
  (let ((buffer nil))
    (unwind-protect
	(save-excursion
	  (setq buffer (generate-new-buffer "*vm-work*"))
	  (set-buffer buffer)
	  (insert string)
	  (call-process-region (point-min) (point-max)
			       "/bin/sh" t buffer nil
			       shell-command-switch vm-pop-md5-program)
	  ;; MD5 digest is 32 chars long
	  ;; mddriver adds a newline to make neaten output for tty
	  ;; viewing, make sure we leave it behind.
	  (vm-buffer-substring-no-properties (point-min) (+ (point-min) 32)))
      (and buffer (kill-buffer buffer)))))
