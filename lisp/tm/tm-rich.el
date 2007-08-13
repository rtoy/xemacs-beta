;;;
;;; tm-rich.el --- text/enriched and text/richtext style
;;;                richtext filter for tm-view
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id: tm-rich.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $
;;; Keywords: mail, news, MIME, multimedia, richtext, enriched
;;;
;;; This file is part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'tm-view)

(defvar tm-rich/richtext-module
  (if (or running-emacs-19_29-or-later
	  running-xemacs-20
	  (and running-xemacs (>= emacs-minor-version 14)))
      'richtext
    'tinyrich))

(require tm-rich/richtext-module)


;;; @ content filters for tm-view
;;;

(defun mime-viewer/filter-text/richtext (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (assoc "charset" params))
	 ;; 1995/9/21 (c.f. tm-eng:105), 1995/10/3 (c.f. tm-eng:121)
	 ;;   modified by Eric Ding <ericding@San-Jose.ate.slb.com>
	 (beg (point-min)) (end (point-max))
	 )
    (remove-text-properties beg end '(face nil))
    (mime/decode-region encoding beg end)
    (if (and m (fboundp (setq m (cdr m))))
	(funcall m beg (point-max) charset encoding)
      (mime-viewer/default-code-convert-region beg (point-max)
					       charset encoding)
      )
    (richtext-decode beg (point-max))
    ))

(defun mime-viewer/filter-text/enriched (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (assoc "charset" params))
	 ;; 1995/9/21 (c.f. tm-eng:105), 1995/10/3 (c.f. tm-eng:121)
	 ;;   modified by Eric Ding <ericding@San-Jose.ate.slb.com>
	 (beg (point-min)) (end (point-max))
	 )
    (remove-text-properties beg end '(face nil))
    (mime/decode-region encoding beg end)
    (if (and m (fboundp (setq m (cdr m))))
	(funcall m beg (point-max) charset encoding)
      (mime-viewer/default-code-convert-region beg (point-max)
					       charset encoding)
      )
    (enriched-decode beg (point-max))
    ))


;;; @ setting
;;;

(set-alist 'mime-viewer/content-filter-alist
	   "text/richtext" (function mime-viewer/filter-text/richtext))

(set-alist 'mime-viewer/content-filter-alist
	   "text/enriched" (function mime-viewer/filter-text/enriched))


;;; @ end
;;;

(provide 'tm-rich)

(run-hooks 'tm-rich-load-hook)

;;; tm-rich.el ends here
