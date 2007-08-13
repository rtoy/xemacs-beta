;;; gnus-charset.el --- MIME charset extension for Gnus

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/6
;; Version:
;;	$Id: gnus-charset.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
;; Keywords: news, MIME, multimedia, multilingual, encoded-word

;; This file is not part of GNU Emacs yet.

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

(require 'gnus)

(defvar gnus-is-red-gnus-or-later
  (or (featurep 'gnus-load)
      (module-installed-p 'gnus-sum)
      ))


;;; @ newsgroup default charset
;;;

(defvar gnus-newsgroup-default-charset-alist nil)

(defun gnus-set-newsgroup-default-charset (newsgroup charset)
  "Set CHARSET for the NEWSGROUP as default MIME charset."
  (let* ((ng-regexp (concat "^" (regexp-quote newsgroup) "\\($\\|\\.\\)"))
	 (pair (assoc ng-regexp gnus-newsgroup-default-charset-alist))
	 )
    (if pair
	(setcdr pair charset)
      (setq gnus-newsgroup-default-charset-alist
	    (cons (cons ng-regexp charset)
		  gnus-newsgroup-default-charset-alist))
      )))


;;; @ for mule (Multilingual support)
;;;

(cond
 ((featurep 'mule)
  (require 'emu)
  (defvar nntp-open-binary-connection-function
    (if gnus-is-red-gnus-or-later
	;; maybe Red Gnus
	(if (boundp 'nntp-open-connection-function)
	    nntp-open-connection-function
	  'nntp-open-network-stream)
      ;; maybe Gnus 5.[01] or Gnus 5.[23]
      (if (boundp 'nntp-open-server-function)
	  nntp-open-server-function
	'nntp-open-network-stream)
      ))
  (defun nntp-open-network-stream-with-no-code-conversion (&rest args)
    (let ((proc (apply nntp-open-binary-connection-function args)))
      (set-process-input-coding-system proc *noconv*)
      proc))
  (if gnus-is-red-gnus-or-later
      (setq nntp-open-connection-function
	    'nntp-open-network-stream-with-no-code-conversion)
    (setq nntp-open-server-function
	  'nntp-open-network-stream-with-no-code-conversion)
    )
  (call-after-loaded
   'nnheader
   (lambda ()
     (defun nnheader-find-file-noselect (filename &optional nowarn rawfile)
       (as-binary-input-file (find-file-noselect filename nowarn rawfile))
       )
     (defun nnheader-insert-file-contents-literally
       (filename &optional visit beg end replace)
       (as-binary-input-file
	(insert-file-contents-literally filename visit beg end replace)
	))
     ))
  (call-after-loaded
   'nnmail
   (lambda ()
     (defun nnmail-find-file (file)
       "Insert FILE in server buffer safely. [gnus-charset.el]"
       (set-buffer nntp-server-buffer)
       (erase-buffer)
       (let ((format-alist nil)
             (after-insert-file-functions   ; for jam-code-guess
              (if (memq 'jam-code-guess-after-insert-file-function
                        after-insert-file-functions)
                  '(jam-code-guess-after-insert-file-function)))
	     )
	 (as-binary-input-file
	  (condition-case ()
	      (progn (insert-file-contents file) t)
	    (file-error nil))
	  )))
     ))
  (defun gnus-prepare-save-mail-function ()
    (setq file-coding-system *noconv*
	  coding-system-for-write 'no-conversion)
    )
  (add-hook 'nnmail-prepare-save-mail-hook
	    'gnus-prepare-save-mail-function)
  
  (gnus-set-newsgroup-default-charset "alt.chinese" 'hz-gb-2312)
  (gnus-set-newsgroup-default-charset "alt.chinese.text.big5" 'cn-big5)
  (gnus-set-newsgroup-default-charset "fj"	'iso-2022-jp-2)
  (gnus-set-newsgroup-default-charset "han"    	'euc-kr)
  (gnus-set-newsgroup-default-charset "hk"	'cn-big5)
  (gnus-set-newsgroup-default-charset "hkstar"	'cn-big5)
  (gnus-set-newsgroup-default-charset "relcom"	'koi8-r)
  (gnus-set-newsgroup-default-charset "tw"	'cn-big5)
  ))


;;; @ end
;;;

(provide 'gnus-charset)

;;; gnus-charset.el ends here
