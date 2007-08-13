;;; url-mail.el,v --- Mail Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1996/06/03 15:04:49
;; Version: 1.5
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
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

(require 'url-vars)
(require 'url-parse)

(defmacro url-mailserver-skip-chunk ()
  (` (while (and (not (looking-at "/"))
		 (not (eobp)))
       (forward-sexp 1))))

(defun url-mail (&rest args)
  (interactive "P")
  (or (apply 'mail args)
      (error "Mail aborted")))
  
(defun url-mailto (url)
  ;; Send mail to someone
  (if (not (string-match "mailto:/*\\(.*\\)" url))
      (error "Malformed mailto link: %s" url))
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (let ((to (url-unhex-string
	     (substring url (match-beginning 1) (match-end 1))))
	(url (url-view-url t)))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (mail-to)
    (insert (concat to "\nX-URL-From: " url))
    (mail-subject)
    (if (not url-request-data)
	nil				; Not automatic posting
      (insert "Automatic submission from "
	      url-package-name "/" url-package-version)
      (if url-request-extra-headers
	  (progn
	    (goto-char (point-min))
	    (insert
	     (mapconcat
	      (function
	       (lambda (x)
		 (concat (capitalize (car x)) ": " (cdr x) "\n")))
	      url-request-extra-headers ""))))
      (goto-char (point-max))
      (insert url-request-data)
      (mail-send-and-exit nil))))

(defun url-mailserver (url)
  ;; Send mail to someone, much cooler/functional than mailto
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (set-buffer (get-buffer-create " *mailserver*"))
  (erase-buffer)
  (insert url)
  (goto-char (point-min))
  (set-syntax-table url-mailserver-syntax-table)
  (skip-chars-forward "^:")		; Get past mailserver
  (skip-chars-forward ":")		; Get past :
  ;; Handle some ugly malformed URLs, but bitch about it.
  (if (looking-at "/")
      (progn
	(url-warn 'url "Invalid mailserver URL... attempting to cope.")
	(skip-chars-forward "/")))
  
  (let ((save-pos (point))
	(url (url-view-url t))
	(rfc822-addr nil)
	(subject nil)
	(body nil))
    (url-mailserver-skip-chunk)
    (setq rfc822-addr (buffer-substring save-pos (point)))
    (forward-char 1)
    (setq save-pos (point))
    (url-mailserver-skip-chunk)
    (setq subject (buffer-substring save-pos (point)))
    (if (not (eobp))
	(progn				; There is some text to use
	  (forward-char 1)		; as the body of the message
	  (setq body (buffer-substring (point) (point-max)))))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (mail-to)
    (insert (concat rfc822-addr
		    (if (and url (not (string= url "")))
			(concat "\nX-URL-From: " url) "")
		    "\nX-User-Agent: " url-package-name "/"
		    url-package-version))
    (mail-subject)
    ;; Massage the subject from URLEncoded garbage
    ;; Note that we do not allow any newlines in the subject,
    ;; as recommended by the Internet Draft on the mailserver
    ;; URL - this means the document author cannot spoof additional
    ;; header lines, which is a 'Good Thing'
    (if subject
	(progn
	  (setq subject (url-unhex-string subject))
	  (let ((x (1- (length subject)))
		(y 0))
	    (while (<= y x)
	      (if (memq (aref subject y) '(?\r ?\n))
		  (aset subject y ? ))
	      (setq y (1+ y))))))
    (insert subject)
    (if url-request-extra-headers
	(progn
	  (goto-char (point-min))
	  (insert
	   (mapconcat
	    (function
	     (lambda (x)
	       (concat (capitalize (car x)) ": " (cdr x) "\n")))
	    url-request-extra-headers ""))))
    (goto-char (point-max))
    ;; Massage the body from URLEncoded garbage
    (if body
	(let ((x (1- (length body)))
	      (y 0))
	  (while (<= y x)
	    (if (= (aref body y) ?/)
		(aset body y ?\n))
	    (setq y (1+ y)))
	  (setq body (url-unhex-string body))))
    (and body (insert body))
    (and url-request-data (insert url-request-data))
    (if (and (or body url-request-data)
	     (funcall url-confirmation-func
		      (concat "Send message to " rfc822-addr "? ")))
	(mail-send-and-exit nil))))    

(provide 'url-mail)
