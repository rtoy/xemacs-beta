;;; debug.el --- routines for debugging problems in XEmacs

;; Copyright (C) 2002 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:


(defun show-memory-usage ()
  "Show statistics about memory usage of various sorts in XEmacs."
  (interactive)
  (garbage-collect)
  (flet ((show-foo-stats (objtypename objlist memfun)
	   (let* ((hash (make-hash-table))
		  (first t)
		  types fmt
		  (objnamelen 25)
		  (linelen objnamelen)
		  (totaltotal 0))
	     (dolist (obj objlist)
	       (let ((total 0)
		     (stats (funcall memfun obj)))
		 (loop for (type . num) in stats while type do
		   (puthash type (+ num (or (gethash type hash) 0)) hash)
		   (incf total num)
		   (if first (push type types)))
		 (incf totaltotal total)
		 (when first
		   (setq types (nreverse types))
		   (setq fmt (concat
			      (format "%%-%ds" objnamelen)
			      (mapconcat
			       #'(lambda (type)
				   (let ((fieldlen
					  (max 8 (+ 2 (length
						       (symbol-name type))))))
				     (incf linelen fieldlen)
				     (format "%%%ds" fieldlen)))
			       types "")
			      (progn (incf linelen 8) "%8s\n")))
		   (princ "\n")
		   (princ (apply 'format fmt objtypename
				 (append types (list 'total))))
		   (princ (make-string linelen ?-))
		   (princ "\n"))
		 (let ((objname (format "%s" obj)))
		   (princ (apply 'format fmt (substring objname 0
							(min (length objname)
							     (1- objnamelen)))
				 (nconc (mapcar #'(lambda (type)
						    (cdr (assq type stats)))
						types)
					(list total)))))
		 (setq first nil)))
	     (princ "\n")
	     (princ (apply 'format fmt "total"
			   (nconc (mapcar #'(lambda (type)
					      (gethash type hash))
					  types)
				  (list totaltotal))))
	     totaltotal)))

    (let ((grandtotal 0))
      (with-output-to-temp-buffer "*memory usage*"
	(when-fboundp 'charset-list
	  (incf grandtotal
		(show-foo-stats 'charset (charset-list)
				#'charset-memory-usage))
	  (princ "\n"))
	(incf grandtotal
	      (show-foo-stats 'buffer (buffer-list) #'buffer-memory-usage))
	(princ "\n")
	(incf grandtotal
	      (show-foo-stats 'window (mapcan #'(lambda (fr)
						  (window-list fr t))
					      (frame-list))
			      #'window-memory-usage))
	(princ "\n")
	(let ((total 0)
	      (fmt "%-30s%10s\n"))
	  (princ (format fmt "object" "storage"))
	  (princ (make-string 40 ?-))
	  (princ "\n")
	  (map-plist #'(lambda (stat num)
			 (when (string-match "\\(.*\\)-storage$"
					     (symbol-name stat))
			   (incf total num)
			   (princ (format fmt
					  (match-string 1 (symbol-name stat))
					  num)))
			 (when (eq stat 'long-strings-total-length)
			   (incf total num)
			   (princ (format fmt stat num))))
		     (sixth (garbage-collect)))
	  (princ "\n")
	  (princ (format fmt "total" total))
	  (incf grandtotal total))

	(princ (format "\n\ngrand total: %s\n" grandtotal))
	grandtotal))))
