;;; texi-util.el --- Texinfo utility

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: texi-util.el,v 1.2 1996/12/22 00:29:31 steve Exp $
;; Keywords: Texinfo

;; This file is part of tl (Tiny Library).

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

(defun texinfo-all-menu-titles-update ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n@menu\n" nil t)
    (goto-char (match-end 0))
    (while (looking-at "* \\([^:]+\\)::")
      (let ((title (buffer-substring (match-beginning 1)(match-end 1)))
	    subj)
	(save-excursion
	  (let ((ret
		 (re-search-forward
		  (format
		   "@node %s.*\n@\\(chapter\\|\\(sub\\)*section\\) \\(.+\\)"
		   (regexp-quote title)))))
	    (if ret
		(let ((data (last (match-data) 2)))
		  (setq subj (buffer-substring (car data)
					       (car (cdr data))))
		  ))
	    ))
	(if subj
	    (or (string= subj title)
		(progn
		  (end-of-line)
		  (insert subj)
		  )))
	(end-of-line)
	(forward-char)
	))))

	       
;;; @ end
;;;

(provide 'texi-util)

;;; texi-util.el ends here
