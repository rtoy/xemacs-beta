;;; tm-setup.el --- setup file for tm viewer.

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: tm-setup.el,v 1.2 1997/02/15 22:21:30 steve Exp $
;; Keywords: mail, news, MIME, multimedia, multilingual, encoded-word

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

(require 'tl-misc)


;;; @ for tm-view
;;;

(call-after-loaded
 'tm-view
 (function
  (lambda ()
    ;; for message/partial
    (require 'tm-partial)
    
    ;; for anonymous ftp
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/external-body")
		 ("access-type" . "anon-ftp")
		 (method . mime-article/decode-message/external-ftp)
		 ))
    (autoload 'mime-article/decode-message/external-ftp "tm-ftp")
    
    ;; for LaTeX
    (set-atype 'mime/content-decoding-condition
	       '((type . "text/x-latex")
		 (method . mime/decode-text/latex)
		 ))
    (set-atype 'mime/content-decoding-condition
	       '((type . "application/x-latex")
		 (method . mime/decode-text/latex)
		 ))
    ;;(set-atype 'mime/content-decoding-condition
    ;; 	'((type . "application/octet-stream")
    ;;		  ("type" . "latex")
    ;;		  (method . mime/decode-text/latex)
    ;;		  ))
    (autoload 'mime/decode-text/latex "tm-latex")
    )))


;; for image/* and X-Face
(defvar mime-setup-enable-inline-image
  (and window-system
       (or running-xemacs
	   (and (featurep 'mule)(module-installed-p 'bitmap))
	   ))
  "*If it is non-nil, tm-setup sets up to use tm-image.")

(if mime-setup-enable-inline-image
    (call-after-loaded 'tm-view
		       (function
			(lambda ()
			  (require 'tm-image)
			  )))
  )


(defvar mime-setup-enable-pgp
  (module-installed-p 'mailcrypt)
  "*If it is non-nil, tm-setup sets uf to use tm-pgp.")

;; for PGP
(if mime-setup-enable-pgp
    (call-after-loaded 'tm-view
		       (function
			(lambda ()
			  (require 'tm-pgp)
			  )))
  )


;;; @ for RMAIL
;;;

(defun tm-setup/load-rmail ()
  (or (and (boundp 'rmail-support-mime)
	   rmail-support-mime)
      (require 'tm-rmail)
      )
  (remove-hook 'rmail-mode-hook 'tm-setup/load-rmail)
  )

(call-after-loaded 'rmail 'tm-setup/load-rmail 'rmail-mode-hook)


;;; @ for mh-e
;;;

(let ((le (function
	   (lambda ()
	     (require 'tm-mh-e)
	     ))
	  ))
  (call-after-loaded 'mh-e le 'mh-folder-mode-hook)
  (if (not (featurep 'mh-e))
      (add-hook 'mh-letter-mode-hook le)
    ))


;;; @ for GNUS and Gnus
;;;

(if (featurep 'gnus)
    (if (boundp 'gnus-load-hook)
	(require 'gnus-mime)
      (require 'tm-gnus)
      )
  ;; for GNUS
  (defvar tm-setup/use-gnusutil nil)
  
  (defun tm-setup/load-GNUS ()
    (require 'tm-gnus)
    )
  
  (if (and (boundp 'MULE) tm-setup/use-gnusutil)
      (progn
	(add-hook 'gnus-Group-mode-hook (function gnusutil-initialize))
	(add-hook 'gnus-group-mode-hook (function gnusutil-initialize))
	(autoload 'gnusutil-initialize "gnusutil")
	(autoload 'gnusutil-add-group "gnusutil")
	(add-hook 'gnusutil-initialize-hook 'tm-setup/load-GNUS)
	)
    (add-hook 'gnus-Startup-hook 'tm-setup/load-GNUS 'append)
    (add-hook 'gnus-startup-hook 'tm-setup/load-GNUS 'append)
    )
  
  ;; for Gnus
  (defun tm-setup/load-gnus ()
    (let (gnus-load-hook)
      (remove-hook 'gnus-startup-hook 'tm-setup/load-GNUS)
      (require 'gnus-mime)
      ))
  
  (add-hook 'gnus-load-hook 'tm-setup/load-gnus)
  )


;;; @ end
;;;

(provide 'tm-setup)

;;; tm-setup.el ends here
