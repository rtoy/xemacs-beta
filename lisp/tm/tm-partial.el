;;; tm-partial.el --- Grabbing all MIME "message/partial"s.

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: OKABE Yasuo @ Kyoto University
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: tm-partial.el,v 1.2 1996/12/28 21:03:15 steve Exp $ 
;; Keywords: mail, news, MIME, multimedia, message/partial

;; This file is a part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tm-view)
(require 'tm-play)

(defvar tm-partial/preview-article-method-alist nil)
   
;; display Article at the cursor in Subject buffer.
(defun tm-partial/preview-article (target)
  (let ((f (assq target tm-partial/preview-article-method-alist)))
    (if f
	(funcall (cdr f))
      (error "Fatal. Unsupported mode")
      )))

(defun mime-article/grab-message/partials (beg end cal)
  (interactive)
  (let* ((id (cdr (assoc "id" cal)))
	 (buffer (generate-new-buffer id))
	 (mother mime::article/preview-buffer)
	 (target (cdr (assq 'major-mode cal)))
	 (article-buffer (buffer-name (current-buffer)))
	 (subject-buf (eval (cdr (assq 'summary-buffer-exp cal))))
	 subject-id
	 (root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name)) mime/tmp-dir))
	 full-file)
    (setq root-dir (concat root-dir "/" (replace-as-filename id)))
    (setq full-file (concat root-dir "/FULL"))
    
    (if (null target)
	(error "%s is not supported. Sorry." target)
      )
    
    ;; if you can't parse the subject line, try simple decoding method
    (if (or (file-exists-p full-file)
	    (not (y-or-n-p "Merge partials?"))
	    )
	(progn
	  (kill-buffer buffer)
	  (mime-article/decode-message/partial beg end cal)
	  )
      (let (cinfo the-id parameters)
	(setq subject-id (std11-field-body "Subject"))
	(if (string-match "[0-9\n]+" subject-id)
	    (setq subject-id (substring subject-id 0 (match-beginning 0)))
	  )
	(pop-to-buffer subject-buf)
	(while (search-backward subject-id nil t)
	  )
	(catch 'tag
	  (while t
	    (tm-partial/preview-article target)
	    (pop-to-buffer article-buffer)
	    (switch-to-buffer mime::article/preview-buffer)
	    (setq cinfo
		  (mime::preview-content-info/content-info
		   (car mime::preview/content-list)))
	    (setq parameters (mime::content-info/parameters cinfo))
	    (setq the-id (assoc-value "id" parameters))
	    (if (equal the-id id)
		(progn
		  (switch-to-buffer article-buffer)
		  (mime-article/decode-message/partial
		   (point-min)(point-max) parameters)
		  (if (file-exists-p full-file)
		      (throw 'tag nil)
		    )
		  ))
	    (if (not (progn
		       (pop-to-buffer subject-buf)
		       (end-of-line)
		       (search-forward subject-id nil t)
		       ))
		(error "not found")
	      )
	    ))))))


;;; @ end
;;;

(provide 'tm-partial)

(run-hooks 'tm-partial-load-hook)

;;; tm-partial.el ends here
