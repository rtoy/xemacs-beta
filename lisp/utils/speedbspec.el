;;; speedbspec --- Buffer specialized configurations for speedbar

;; Copyright (C) 1997 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: file, tags, tools
;; X-RCS: $Id: speedbspec.el,v 1.1 1997/06/29 23:13:34 steve Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide some mode-specific
;; displays for some existing emacs modes.
;;
;;   To provide special service to all the modes supported by this file,
;; put the following in your .emacs file.
;;
;; (require 'speedbspec)
;;
;;   This will load in the known functions, and the mode-enabling code
;; into 'change-major-mode-hook.
;;
;;   This file requires speedbar.

;;; Change log:
;;  0.1 - Initial revision requiring speedbar 0.5

;;; Code:
(require 'speedbar)

;;; Generic add-new-special-mode stuff
;;
(defvar speedbar-localized-buffer-queue nil
  "List of buffers to localize for speedbar.")

(defun speedbar-add-localized-speedbar-support-to-q ()
  "Add speedbar support to all buffers in `speedbar-localized-buffer-queue'."
  (remove-hook 'post-command-hook
	       'speedbar-add-localized-speedbar-support-to-q)
  (while speedbar-localized-buffer-queue
    (speedbar-add-localized-speedbar-support
     (car speedbar-localized-buffer-queue))
    (setq speedbar-localized-buffer-queue
	  (cdr speedbar-localized-buffer-queue))))

(defun speedbar-add-localized-speedbar-support (buffer)
  "Add localized speedbar support to BUFFER's mode if it is available."
  (if (not (buffer-live-p buffer))
      nil
    (save-excursion
      (set-buffer buffer)
      (save-match-data
	(let ((ms (symbol-name major-mode))
	      v tmp)
	  (if (not (string-match "-mode$" ms))
	      nil ;; do nothing to broken mode
	    (setq ms (substring ms 0 (match-beginning 0)))
	    (setq v (intern-soft (concat ms "-speedbar-buttons")))
	    (if (not v)
		nil ;; do nothing if not defined
	      (make-local-variable 'speedbar-special-mode-expansion-list)
	      (setq speedbar-special-mode-expansion-list (list v))
	      (setq v (intern-soft (concat ms "-speedbar-menu-items")))
	      (if (not v)
		  nil ;; don't add special menus
		(make-local-variable 'speedbar-easymenu-definition-special)
		(setq speedbar-easymenu-definition-special
		      (symbol-value v))))))))))
  
(defun speedbar-change-major-mode ()
  "Run when the major mode is changed."
  (setq speedbar-localized-buffer-queue
	(add-to-list 'speedbar-localized-buffer-queue (current-buffer)))
  (add-hook 'post-command-hook 'speedbar-add-localized-speedbar-support-to-q))

(add-hook 'change-major-mode-hook 'speedbar-change-major-mode)
(add-hook 'find-file-hooks 'speedbar-change-major-mode)

;;; Info specific code
;;
(defvar Info-last-speedbar-node nil
  "Last node viewed with speedbar in the form '(NODE FILE).")

(defvar Info-speedbar-menu-items
  '(["Browse Item On Line" speedbar-edit-line t])
  "Additional menu-items to add to speedbar frame.")

(defun Info-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for."
  (goto-char (point-min))
  (if (and (looking-at "<Directory>")
	   (save-excursion
	     (set-buffer buffer)
	     (and (equal (car Info-last-speedbar-node) Info-current-node)
		  (equal (cdr Info-last-speedbar-node) Info-current-file))))
      nil
    (erase-buffer)
    (speedbar-insert-button "<Directory>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-directory)
    (speedbar-insert-button "<Top>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-top-node)
    (speedbar-insert-button "<Last>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-last)
    (speedbar-insert-button "<Up>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-up)
    (speedbar-insert-button "<Next>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-next)
    (speedbar-insert-button "<Prev>" 'info-xref 'highlight
			    'Info-speedbar-button
			    'Info-prev)
    (let ((completions nil))
      (save-excursion
	(set-buffer buffer)
	(setq Info-last-speedbar-node
	      (cons Info-current-node Info-current-file))
	(goto-char (point-min))
	;; Always skip the first one...
	(re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	  (setq completions (cons (buffer-substring (match-beginning 1)
						    (match-end 1))
				  completions))))
      (setq completions (nreverse completions))
      (while completions
	(speedbar-make-tag-line nil nil nil nil
				(car completions) 'Info-speedbar-menu
				nil 'info-node 0)
	(setq completions (cdr completions))))))

(defun Info-speedbar-button (text token indent)
  "Called when user clicks <Directory> from speedbar.
TEXT, TOKEN, and INDENT are unused."
  (speedbar-with-attached-buffer
   (funcall token)
   (setq Info-last-speedbar-node nil)
   (speedbar-update-contents)))

(defun Info-speedbar-menu (text token indent)
  "Goto the menu node specified in TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (Info-menu text)
   (setq Info-last-speedbar-node nil)
   (speedbar-update-contents)))

;;; RMAIL specific code
;;
(defvar rmail-speedbar-last-user nil
  "The last user to be displayed in the speedbar.")

(defvar rmail-speedbar-menu-items
  '(["Browse Item On Line" speedbar-edit-line t]
    ["Move message to folder" rmail-move-message-to-folder-on-line
     (save-excursion (beginning-of-line)
		     (looking-at "<M> "))])
  "Additional menu-items to add to speedbar frame.")

(defun rmail-speedbar-buttons (buffer)
  "Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder."
  (let ((from nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (not (re-search-forward "^Reply-To: " nil t))
	  (if (not (re-search-forward "^From:? " nil t))
	      (setq from t)))
      (if from
	  nil
	(setq from (buffer-substring (point) (save-excursion
					       (end-of-line)
					       (point))))))
    (goto-char (point-min))
    (if (and (looking-at "Reply to:")
	     (equal from rmail-speedbar-last-user))
	nil
      (setq rmail-speedbar-last-user from)
      (erase-buffer)
      (insert "Reply To:\n")
      (if (stringp from)
	  (speedbar-insert-button from 'speedbar-directory-face 'highlight
				  'rmail-speedbar-button 'rmail-reply))
      (insert "Folders:\n")
      (let* ((case-fold-search nil)
	     (df (directory-files (save-excursion (set-buffer buffer)
						  default-directory)
				  nil "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$")))
	(while df
	  (speedbar-insert-button "<M>" 'speedbar-button-face 'highlight
				  'rmail-speedbar-move-message (car df))
	  (speedbar-insert-button (car df) 'speedbar-file-face 'highlight
				  'rmail-speedbar-find-file nil t)
	  (setq df (cdr df)))))))

(defun rmail-speedbar-button (text token indent)
  "Execute an rmail command specified by TEXT.
The command used is TOKEN.  INDENT is not used."
  (speedbar-with-attached-buffer
   (funcall token t)))

(defun rmail-speedbar-find-file (text token indent)
  "Load in the rmail file TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Loading in RMAIL file %s..." text)
   (find-file text)))

(defun rmail-move-message-to-folder-on-line ()
  "If the current line is a folder, move current message to it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "<M> " (save-excursion (end-of-line) (point)) t)
	(progn
	  (forward-char -2)
	  (speedbar-do-function-pointer)))))

(defun rmail-speedbar-move-message (text token indent)
  "From button TEXT, copy current message to the rmail file specified by TOKEN.
TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Moving message to %s" token)
   (rmail-output-to-rmail-file token)))

;;; W3 speedbar help
(defvar w3-speedbar-last-buffer nil
  "The last buffer shown by w3-speedbar.")

(defun w3-speedbar-buttons (buffer)
  "Create speedbar buttons for the current web BUFFER displayed in w3 mode."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "History:") (equal w3-speedbar-last-buffer buffer))
	nil
      (setq w3-speedbar-last-buffer buffer)
      (erase-buffer)
      (let ((links (save-excursion (set-buffer buffer) (w3-only-links)))
	    (part nil))
	(insert "History:\n")
	;; This taken out of w3 which was used to create the history list,
	;; and is here modified to create the speedbar buttons
	(cl-maphash
	 (function
	  (lambda (url desc)
	    (speedbar-insert-button (w3-speedbar-shorten-button url)
				    'speedbar-directory-face 'highlight
				    'w3-speedbar-link url)))
	 url-history-list)
	(insert "Links:\n")
	(while links
	  (setq part (car (cdr (member 'href (car links))))
		links (cdr links))
	  (speedbar-insert-button (w3-speedbar-shorten-button part)
				  'speedbar-file-face 'highlight
				  'w3-speedbar-link part))))))
    
(defun w3-speedbar-shorten-button (button)
  "Takes text BUTTON and shortens it as much as possible."
  ;; I should make this more complex, but I'm not sure how...
  (let ((fnnd (file-name-nondirectory button)))
    (if (< 0 (length fnnd))
	fnnd
      (if (string-match "\\(ht\\|f\\)tp://" button)
	  (setq button (substring button (match-end 0))))
      (if (string-match "/$" button)
	  (setq button (substring button 0 (match-beginning 0))))
      button)))

(defun w3-speedbar-link (text token indent)
  "Follow link described by TEXT which has the URL TOKEN.
INDENT is not used."
  (speedbar-with-attached-buffer (w3-fetch token)))

(provide 'speedbspec)
;;; speedbspec ends here
