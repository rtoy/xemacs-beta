;;; tm-bbdb.el --- tm shared module for BBDB

;; Copyright (C) 1995,1996 Shuhei KOBAYASHI
;; Copyright (C) 1996 Artur Pioro

;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;         Artur Pioro <artur@flugor.if.uj.edu.pl>
;; Maintainer: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Version: $Id: tm-bbdb.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
;; Keywords: mail, news, MIME, multimedia, multilingual, BBDB

;; This file is part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when (compile)
  (ignore-errors
    (require 'bbdb)
    (require 'bbdb-com)))
(eval-when (load eval)
    (require 'bbdb)
    (require 'bbdb-com))
(require 'std11)
(require 'tm-ew-d)
(require 'tm-view)


;;; @ mail-extr
;;;

(defvar tm-bbdb/use-mail-extr t)

(defun tm-bbdb/extract-address-components (str)
  (let* ((ret     (std11-extract-address-components str))
         (phrase  (car ret))
         (address (car (cdr ret)))
         (methods tm-bbdb/canonicalize-full-name-methods))
    (while (and phrase methods)
      (setq phrase  (funcall (car methods) phrase)
            methods (cdr methods)))
    (if (string= address "") (setq address nil))
    (if (string= phrase "") (setq phrase nil))
    (list phrase address)
    ))

(or tm-bbdb/use-mail-extr
    (progn
      (require 'mail-extr) ; for `what-domain'
      (or (fboundp 'tm:mail-extract-address-components)
          (fset 'tm:mail-extract-address-components
                (symbol-function 'mail-extract-address-components)))
      (fset 'mail-extract-address-components
	    (symbol-function 'tm-bbdb/extract-address-components))
      ))


;;; @ bbdb-extract-field-value
;;;

(or (fboundp 'tm:bbdb-extract-field-value)
    (progn
      ;; (require 'bbdb-hooks) ; not provided.
      ;; (or (fboundp 'bbdb-extract-field-value) ; defined as autoload
      (or (fboundp 'bbdb-header-start)
          (load "bbdb-hooks"))
      (fset 'tm:bbdb-extract-field-value
            (symbol-function 'bbdb-extract-field-value))
      (defun bbdb-extract-field-value (field)
        (let ((value (tm:bbdb-extract-field-value field)))
          (and value
               (mime-eword/decode-string value))))
      ))


;;; @ full-name canonicalization methods
;;;

(defun tm-bbdb/canonicalize-spaces (str)
  (let (dest)
    (while (string-match "\\s +" str)
      (setq dest (cons (substring str 0 (match-beginning 0)) dest))
      (setq str (substring str (match-end 0)))
      )
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")
    ))

(defun tm-bbdb/canonicalize-dots (str)
  (let (dest)
    (while (string-match "\\." str)
      (setq dest (cons (substring str 0 (match-end 0)) dest))
      (setq str (substring str (match-end 0)))
      )
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")
    ))

(defvar tm-bbdb/canonicalize-full-name-methods
  '(mime-eword/decode-string
    tm-bbdb/canonicalize-dots
    tm-bbdb/canonicalize-spaces))


;;; @ BBDB functions for mime/viewer-mode
;;;

(defvar tm-bbdb/auto-create-p nil)

(defun tm-bbdb/update-record (&optional offer-to-create)
  "Return the record corresponding to the current MIME previewing message.
Creating or modifying it as necessary. A record will be created if
tm-bbdb/auto-create-p is non-nil, or if OFFER-TO-CREATE is non-nil and
the user confirms the creation."
  (save-excursion
    (if (and mime::article/preview-buffer
             (get-buffer mime::article/preview-buffer))
        (set-buffer mime::article/preview-buffer))
    (if bbdb-use-pop-up
        (tm-bbdb/pop-up-bbdb-buffer offer-to-create)
      (let* ((from (std11-field-body "From"))
             (addr (if from
		       (car (cdr (mail-extract-address-components from))))))
        (if (or (null from)
                (null addr)
                (string-match (bbdb-user-mail-names) addr))
            (setq from (or (std11-field-body "To") from))
	  )
        (if from
            (bbdb-annotate-message-sender
             from t
             (or (bbdb-invoke-hook-for-value tm-bbdb/auto-create-p)
                 offer-to-create)
             offer-to-create))
        ))))

(defun tm-bbdb/annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message."
  (interactive
   (list (if bbdb-readonly-p
             (error "The Insidious Big Brother Database is read-only.")
           (read-string "Comments: "))))
  (bbdb-annotate-notes (tm-bbdb/update-record t) string))

(defun tm-bbdb/edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (tm-bbdb/update-record t)
                    (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun tm-bbdb/show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (let ((record (tm-bbdb/update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))

(defun tm-bbdb/pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the MIME preview window(s),
displaying the record corresponding to the sender of the current message."
  (bbdb-pop-up-bbdb-buffer
   (function
    (lambda (w)
      (let ((b (current-buffer)))
        (set-buffer (window-buffer w))
        (prog1 (eq major-mode 'mime/viewer-mode)
          (set-buffer b))))))
  (let ((bbdb-gag-messages t)
        (bbdb-use-pop-up nil)
        (bbdb-electric-p nil))
    (let ((record (tm-bbdb/update-record offer-to-create))
          (bbdb-elided-display (bbdb-pop-up-elided-display))
          (b (current-buffer)))
      (bbdb-display-records (if record (list record) nil))
      (or record
          (delete-windows-on (get-buffer "*BBDB*")))
      (set-buffer b)
      record)))

(defun tm-bbdb/define-keys ()
  (let ((mime/viewer-mode-map (current-local-map)))
    (define-key mime/viewer-mode-map ";" 'tm-bbdb/edit-notes)
    (define-key mime/viewer-mode-map ":" 'tm-bbdb/show-sender)
    ))

(add-hook 'mime-viewer/define-keymap-hook 'tm-bbdb/define-keys)


;;; @ for signature.el
;;;

(defun signature/get-bbdb-sigtype (addr)
  "Extract sigtype information from BBDB."
  (let ((record (bbdb-search-simple nil addr)))
    (and record
         (bbdb-record-getprop record 'sigtype))
    ))

(defun signature/set-bbdb-sigtype (sigtype addr)
  "Add sigtype information to BBDB."
  (let* ((bbdb-notice-hook nil)
         (record (bbdb-annotate-message-sender 
                  addr t
                  (bbdb-invoke-hook-for-value 
                   bbdb/mail-auto-create-p)
                  t)))
    (if record
        (progn
          (bbdb-record-putprop record 'sigtype sigtype)
          (bbdb-change-record record nil))
      )))

(defun signature/get-sigtype-from-bbdb (&optional verbose)
  (let* ((to (std11-field-body "To"))
         (addr (and to
                    (car (cdr (mail-extract-address-components to)))))
         (sigtype (signature/get-bbdb-sigtype addr))
         return  
         )
    (if addr
        (if verbose
            (progn
              (setq return (signature/get-sigtype-interactively sigtype))
              (if (and (not (string-equal return sigtype))
                       (y-or-n-p
                        (format "Register \"%s\" for <%s>? " return addr))
                       )
                  (signature/set-bbdb-sigtype return addr)
                )
              return)
          (or sigtype
              (signature/get-signature-file-name))
          ))
    ))


;;; @ end
;;;

(provide 'tm-bbdb)

(run-hooks 'tm-bbdb-load-hook)

;;; end of tm-bbdb.el
