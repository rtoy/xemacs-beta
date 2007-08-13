;;; tm-edit-tipgp.el -- TinyPgp.el PGP interface

;; Copyright (C) 1996 Jari aalto

;; Author: Jari Aalto <jari.aalto@poboxes.com>
;; Version: $Id: tm-edit-tipgp.el,v 1.1 1996/12/28 21:12:31 steve Exp $
;; Keywords: mail, news, MIME, multimedia, multilingual, security, PGP

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

(require 'tinypgpa.el)

(defun tm:tipgp-process-region (result-buffer boundary)
  (let ((obuf (current-buffer))
	)
    (cond
     (boundary
      (goto-char (point-min))
      (insert (format "--%s\n" boundary))
      (goto-char (point-max))
      (insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
      (insert-buffer-substring result-buffer)
      (goto-char (point-max))
      (insert (format "\n--%s--\n" boundary))
      )
     (t
      (delete-region beg end)
      (goto-char beg)
      (insert-buffer-substring result-buffer)
      ))
  ))




(defun tm:tipgp-sign-region (start end &optional id unclear boundary)
  ;; start end	= Region
  ;; id		=
  ;; unclear	=
  ;; boundary	= pgp-sign-Multipart_Wed_Dec__4_11:14:41_1996-1

  (let (tipgp-:xpgp-header-mode		;Do not use X-Pgp signing
	passwd
    	ret
	)

    (setq passwd (tipgp-password-get-old "Sign pass phrase: "))


    ;;  The region is already narrowed by TM, so we pass the
    ;;  point-min point-max
    ;;
    ;;  The macro tipgp-run-in-tmp-buffer cpies the contents to
    ;;  another buffer and when signing is over, it will
    ;;  contain fully signed message
    ;;
    (setq
     ret
     (tipgp-run-in-tmp-buffer nil
       (tipgp-sign-region
	(point-min) (point-max) passwd 'verb
	(format
	 "+comment=\"Processed by TinyPgp.el %s\""
	 (strmatget "[0-9][0-9.]+" 0 tipgp-version-id)
	 )
	'noerr
	)))

    (if ret
	(tm:tipgp-process-region tipgp-:buffer-tmp-copy boundary))

    (cond
     ((and boundary ret)
      (goto-char (point-min))
      (insert
       (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-md5][7bit]]\n" boundary))
      ))
    ret
    ))



(defun tm:tipgp-pgp-encrypt-region (recipients start end &optional id sign)
  (let (;;  do not use these hooks while in TM

	tipgp-cmd-macro-before-hook
	tipgp-cmd-macro-after-hook

	;; TinyPgp.el: has separate encrypt and signing functions.
	;;
;;;	(mc-pgp-always-sign
;;;	 (if (eq sign 'maybe)
;;;	     mc-pgp-always-sign
;;;	   'never))

	(elist (ti::mt-email-from-string recipients))
	)
    (if (null elist)
	(error "TO,CC,BCC fields don't contain email addresses."))
    (tipgp-encrypt-region (point-min) (point-max) elist nil 'verb)
    ))


;;; @ end
;;;

(provide 'tm-edit-tipgp)

;;; tm-edit-tipgp.el ends here
