;;; diagnose.el --- routines for debugging problems in XEmacs

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
			      (progn (incf linelen 9) "%9s\n")))
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

    (let ((grandtotal 0)
	  (buffer "*memory usage*")
	  begin)
      (with-output-to-temp-buffer buffer
	(save-excursion
	  (set-buffer buffer)
	  (when-fboundp 'charset-list
	    (setq begin (point))
	    (incf grandtotal
		  (show-foo-stats 'charset (charset-list)
				  #'charset-memory-usage))
	    (sort-numeric-fields -1
				 (save-excursion
				   (goto-char begin)
				   (forward-line 2)
				   (point))
				 (save-excursion
				   (forward-line -2)
				   (point)))
	    (princ "\n"))
	  (setq begin (point))
	  (incf grandtotal
		(show-foo-stats 'buffer (buffer-list) #'buffer-memory-usage))
	  (sort-numeric-fields -1
			       (save-excursion
				 (goto-char begin)
				 (forward-line 3)
				 (point))
			       (save-excursion
				 (forward-line -2)
				 (point)))
	  (princ "\n")
	  (setq begin (point))
	  (incf grandtotal
		(show-foo-stats 'window (mapcan #'(lambda (fr)
						    (window-list fr t))
						(frame-list))
				#'window-memory-usage))
	  (sort-numeric-fields -1
			       (save-excursion
				 (goto-char begin)
				 (forward-line 3)
				 (point))
			       (save-excursion
				 (forward-line -2)
				 (point)))
	  (princ "\n")
	  (let ((total 0)
		(fmt "%-30s%10s\n"))
	    (setq begin (point))
	    (princ (format fmt "object" "storage"))
	    (princ (make-string 40 ?-))
	    (princ "\n")
	    (map-plist #'(lambda (stat num)
			   (when (string-match 
				  "\\(.*\\)-storage\\(-additional\\)?$"
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
	  (sort-numeric-fields -1
			       (save-excursion
				 (goto-char begin)
				 (forward-line 2)
				 (point))
			       (save-excursion
				 (forward-line -2)
				 (point)))

	  (princ (format "\n\ngrand total: %s\n" grandtotal)))
	grandtotal))))


(defun show-lrecord-stats ()
  "Show statistics about lrecord usage in XEmacs."
  (interactive)
  (garbage-collect)
  (let ((buffer "*lrecord statistics*")
	(plist (lrecord-stats))
	(fmt "%-30s%10s%10s\n")
	(grandtotal 0)
	begin)
  (flet ((show-stats (match-string)
	(princ (format fmt "object" "count" "storage"))
	(princ (make-string 50 ?-))
	(princ "\n")
	(let ((total-use 0)
	      (total-use-overhead 0)
	      (total-count 0))
	  (map-plist 
	   #'(lambda (stat num)
	       (when (string-match match-string
				   (symbol-name stat))
		 (let ((storage-use num)
		       (storage-use-overhead 
			(plist-get 
			 plist 
			 (intern (concat (match-string 1 (symbol-name stat))
					 "-storage-including-overhead"))))
		       (storage-count 
			(or (plist-get 
			     plist 
			     (intern 
			      (concat (match-string 1 (symbol-name stat)) 
				      "s-used")))
			    (plist-get 
			     plist 
			     (intern 
			      (concat (match-string 1 (symbol-name stat))
				      "es-used")))
			    (plist-get 
			     plist 
			     (intern 
			      (concat (match-string 1 (symbol-name stat))
				      "-used"))))))
		   (incf total-use storage-use)
		   (incf total-use-overhead (if storage-use-overhead 
						storage-use-overhead 
					      storage-use))
		   (incf total-count storage-count)
		   (princ (format fmt
				  (match-string 1 (symbol-name stat)) 
				  storage-count storage-use)))))
	   plist)
	  (princ "\n")
	  (princ (format fmt "total" 
			 total-count total-use-overhead))
	  (incf grandtotal total-use-overhead)
	  (sort-numeric-fields -1
			       (save-excursion
				 (goto-char begin)
				 (forward-line 2)
				 (point))
			       (save-excursion
				 (forward-line -2)
				 (point))))))
    (with-output-to-temp-buffer buffer
      (save-excursion
	(set-buffer buffer)
	(setq begin (point))
	(princ "Allocated with new allocator:\n")
	(show-stats "\\(.*\\)-storage$")
	(princ "\n\n")
	(setq begin (point))
	(princ "Allocated additionally:\n")
	(show-stats "\\(.*\\)-storage-additional$")
	(princ (format "\n\ngrand total: %s\n" grandtotal)))
      grandtotal))))
  

(defun show-mc-alloc-memory-usage ()
  "Show statistics about memory usage of the new allocator."
  (interactive)
  (garbage-collect)
  (let* ((stats (mc-alloc-memory-usage))
	 (page-size (first stats))
	 (heap-sects (second stats))
	 (used-plhs (third stats))
	 (unmanaged-plhs (fourth stats))
	 (free-plhs (fifth stats))
	 (globals (sixth stats))
	 (mc-malloced-bytes (seventh stats)))
    (with-output-to-temp-buffer "*memory usage*"
      (flet ((print-used-plhs (text plhs)
	       (let ((sum-n-pages 0)
		     (sum-used-n-cells 0)
		     (sum-used-space 0)
		     (sum-used-total 0)
		     (sum-total-n-cells 0)
		     (sum-total-space 0)
		     (sum-total-total 0)
		     (fmt "%7s%7s|%7s%9s%9s%4s|%7s%9s%9s%4s|%4s\n"))
		 (princ (format "%-14s|%-29s|%-29s|\n"
				text
				"       currently in use"
				"       total available"))
		 (princ (format fmt "cell-sz" "#pages" 
				"#cells" "space" "total" "% " 
				"#cells" "space" "total" "% " "% "))
		 (princ (make-string 79 ?-))
		 (princ "\n")
		 (while plhs
		   (let* ((elem (car plhs))
			  (cell-size (first elem))
			  (n-pages (second elem))
			  (used-n-cells (third elem))
			  (used-space (fourth elem))
			  (used-total (if (zerop cell-size)
					  (sixth elem)
					  (* cell-size used-n-cells)))
			  (used-eff (floor (if (not (zerop used-total))
					(* (/ (* used-space 1.0)
					      (* used-total 1.0))
					   100.0)
				      0)))
			  (total-n-cells (fifth elem))
			  (total-space (if (zerop cell-size)
					   used-space
					 (* cell-size total-n-cells)))
			  (total-total (sixth elem))
			  (total-eff (floor (if (not (zerop total-total))
						(* (/ (* total-space 1.0)
						      (* total-total 1.0))
						   100.0)
					      0)))
			  (eff (floor (if (not (zerop total-total))
					  (* (/ (* used-space 1.0)
						(* total-total 1.0))
					     100.0)
					0))))
		     (princ (format fmt 
				    cell-size n-pages used-n-cells used-space 
				    used-total used-eff total-n-cells 
				    total-space total-total total-eff eff))
		     (incf sum-n-pages n-pages)
		     (incf sum-used-n-cells used-n-cells)
		     (incf sum-used-space used-space)
		     (incf sum-used-total used-total)
		     (incf sum-total-n-cells total-n-cells)
		     (incf sum-total-space total-space)
		     (incf sum-total-total total-total))
		   (setq plhs (cdr plhs)))
		 (let ((avg-used-eff (floor (if (not (zerop sum-used-total))
					 (* (/ (* sum-used-space 1.0)
					       (* sum-used-total 1.0)) 
					    100.0)
				       0)))
		       (avg-total-eff (floor (if (not (zerop sum-total-total))
					  (* (/ (* sum-total-space 1.0)
						(* sum-total-total 1.0)) 
					     100.0)
					0)))
		       (avg-eff (floor (if (not (zerop sum-total-total))
					   (* (/ (* sum-used-space 1.0)
						 (* sum-total-total 1.0)) 
					      100.0)
					 0))))
		   (princ (format fmt "sum    " sum-n-pages sum-used-n-cells
				  sum-used-space sum-used-total avg-used-eff
				  sum-total-n-cells sum-total-space 
				  sum-total-total avg-total-eff avg-eff))
		   (princ "\n"))))


	     (print-free-plhs (text plhs)
	       (let ((sum-n-pages 0)
		     (sum-n-sects 0)
		     (sum-space 0)
		     (sum-total 0)
		     (fmt "%6s%10s |%7s%10s\n"))
		 (princ (format "%s\n" text))
		 (princ (format fmt "#pages" "space" "#sects" "total")) 
		 (princ (make-string 35 ?-))
		 (princ "\n")
		 (while plhs
		   (let* ((elem (car plhs))
			  (n-pages (first elem))
			  (n-sects (second elem))
			  (space (* n-pages page-size))
			  (total (* n-sects space)))
		     (princ (format fmt n-pages space n-sects total))
		     (incf sum-n-pages n-pages)
		     (incf sum-n-sects n-sects)
		     (incf sum-space space)
		     (incf sum-total total))
		   (setq plhs (cdr plhs)))
		 (princ (make-string 35 ?=))
		 (princ "\n")
		 (princ (format fmt sum-n-pages sum-space 
				sum-n-sects sum-total))
		 (princ "\n"))))

	(princ (format "%-12s%10s\n\n" "PAGE_SIZE" page-size))
	
	(print-used-plhs "USED HEAP" used-plhs)
	(princ "\n\n")

	(print-used-plhs "UNMANAGED HEAP" unmanaged-plhs)
	(princ "\n\n")
	
	(print-free-plhs "FREE HEAP" free-plhs)
	(princ "\n\n")
	
	(let ((fmt "%-30s%10s\n"))
	  (princ (format fmt "heap sections" ""))
	  (princ (make-string 40 ?-))
	  (princ "\n")
	  (princ (format fmt "number of heap sects" 
			 (first heap-sects)))
	  (princ (format fmt "used size" (second heap-sects)))
	  (princ (make-string 40 ?-))
	  (princ "\n")
	  (princ (format fmt "real size" (third heap-sects)))
	  (princ (format fmt "global allocator structs" globals))
	  (princ (make-string 40 ?-))
	  (princ "\n")
	  (princ (format fmt "real size + structs" 
			 (+ (third heap-sects) globals)))
	  (princ "\n")
	  (princ (make-string 40 ?=))
	  (princ "\n")
	  (princ (format fmt "grand total" mc-malloced-bytes)))
	
	(+ mc-malloced-bytes)))))
