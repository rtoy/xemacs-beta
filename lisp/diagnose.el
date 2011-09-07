;;; diagnose.el --- routines for debugging problems in XEmacs

;; Copyright (C) 2002, 2010 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:


(defun show-memory-usage ()
  "Show statistics about memory usage of various sorts in XEmacs."
  (interactive)
  (garbage-collect)
  (labels ((show-foo-stats (objtypename statname-plist cleanfun objlist
                            &optional objnamelen)
             (let* ((hash (make-hash-table))
                    (first t)
                    types origtypes fmt
                    (objnamelen (or objnamelen 25))
                    (linelen objnamelen)
                    (totaltotal 0))
               (loop for obj in objlist do
                 (let ((total 0)
                       (stats (object-memory-usage obj)))
                   ;; Pop off the slice describing the object itself's
                   ;; memory
                   (while (and stats (not (eq t (pop stats)))))
                   ;; Pop off the slice describing the associated
                   ;; non-Lisp-Object memory from the allocation
                   ;; perspective, so we can get to the slice describing
                   ;; the  memory grouped by type
                   (while (and stats (pop stats)))

                   (loop for (type . num) in (remq t stats) while type do
                     (if first (push type origtypes))
                     (setq type (getf statname-plist type type))
                     (puthash type (+ num (or (gethash type hash) 0)) hash)
                     (incf total num)
                     (if first (push type types)))
                   (incf totaltotal total)
                   (when first
                     (setq types (nreverse types))
                     (setq origtypes (nreverse origtypes))
                     (setq fmt (concat
                                (format "%%-%ds" objnamelen)
                                (mapconcat
                                 #'(lambda (type)
                                     (let ((fieldlen
                                            (max 7 (+ 2 (length
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
                   (let ((objname (format "%s" (funcall cleanfun obj))))
                     (princ (apply 'format fmt (substring objname 0
                                                          (min (length objname)
                                                               (1- objnamelen)))
                                   (nconc (mapcar #'(lambda (type)
                                                      (cdr (assq type stats)))
                                                  origtypes)
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
		  (show-foo-stats 'charset nil 'charset-name
				  (mapcar 'get-charset (charset-list))))
	    (when-fboundp 'sort-numeric-fields
	      (sort-numeric-fields -1
				   (save-excursion
				     (goto-char begin)
				     (forward-line 2)
				     (point))
				   (save-excursion
				     (forward-line -2)
				     (point))))
	    (princ "\n"))
	  (setq begin (point))
	  (incf grandtotal
		(show-foo-stats 'buffer nil 'buffer-name (buffer-list)))
	  (when-fboundp 'sort-numeric-fields
	    (sort-numeric-fields -1
				 (save-excursion
				   (goto-char begin)
				   (forward-line 3)
				   (point))
				 (save-excursion
				   (forward-line -2)
				   (point))))
	  (princ "\n")
	  (setq begin (point))
	  (incf grandtotal
		(show-foo-stats 'window
				'(line-start-cache line-st.
				  face-cache face
				  glyph-cache glyph
				  redisplay-structs redisplay
				  scrollbar-instances scrollbar
				  window-mirror mirror)
				#'(lambda (x)
				    (buffer-name (window-buffer x)))
				(mapcan #'(lambda (fr)
					    (window-list fr t))
					(frame-list))
				16))
          (when-fboundp 'sort-numeric-fields
            (sort-numeric-fields -1
                                 (save-excursion
                                   (goto-char begin)
                                   (forward-line 3)
                                   (point))
                                 (save-excursion
                                   (forward-line -2)
                                   (point))))
	  (princ "\n")
	  (let ((total 0)
		(fmt "%-30s%10s\n"))
	    (setq begin (point))
	    (princ (format fmt "object" "storage"))
	    (princ (make-string 40 ?-))
	    (princ "\n")
	    (map-plist #'(lambda (stat num)
			   (when (and
				  (not
				   (string-match 
				    "\\(.*\\)-ancillary-storage$"
				    (symbol-name stat)))
				  (string-match 
				   "\\(.*\\)-storage$"
				   (symbol-name stat)))
			     (incf total num)
			     (princ (format fmt
					    (match-string 1 (symbol-name stat))
					    num)))
			   )
		       (sixth (garbage-collect)))
	    (princ "\n")
	    (princ (format fmt "total" total))
	    (incf grandtotal total))
          (when-fboundp 'sort-numeric-fields
            (sort-numeric-fields -1
                                 (save-excursion
                                   (goto-char begin)
                                   (forward-line 2)
                                   (point))
                                 (save-excursion
                                   (forward-line -2)
                                   (point))))

	  (princ (format "\n\ngrand total: %s\n" grandtotal)))
	grandtotal))))


(defun show-object-memory-usage-stats ()
  "Show statistics about object memory usage in XEmacs."
  (interactive)
  (garbage-collect)
  (let ((buffer "*object memory usage statistics*")
	(plist (object-memory-usage-stats))
	(fmt "%-28s%10s%10s%10s%10s%10s\n")
	(grandtotal 0)
	begin)
    (labels
        ((show-stats (match-string)
           (princ (format "%28s%10s%40s\n" "" ""
                          "--------------storage---------------"))
           (princ (format fmt "object" "count" "object" "overhead"
                          "non-Lisp" "ancillary"))
           (princ (make-string 78 ?-))
           (princ "\n")
           (let ((total-use 0)
                 (total-non-lisp-use 0)
                 (total-use-overhead 0)
                 (total-use-with-overhead 0)
                 (total-count 0))
             (map-plist 
              #'(lambda (stat num)
                  (let ((symmatch
                         (and (string-match match-string (symbol-name stat))
                              (match-string 1 (symbol-name stat)))))
                    (when (and symmatch
                               (or (< (length symmatch) 9)
                                   (not (equal (substring symmatch -9)
                                               "-non-lisp")))
                               (or (< (length symmatch) 15)
                                   (not (equal (substring symmatch -15)
                                               "-lisp-ancillary"))))
                      (let* ((storage-use num)
                             (storage-use-overhead
                              (or (plist-get 
                                   plist 
                                   (intern (concat symmatch
                                                   "-storage-overhead")))
                                  0))
                             (storage-use-with-overhead
                              (or (plist-get 
                                   plist 
                                   (intern (concat
                                            symmatch
                                            "-storage-including-overhead")))
                                  (+ storage-use storage-use-overhead)))
                             (storage-use-overhead
                              (- storage-use-with-overhead storage-use))
                             (non-lisp-storage
                              (or (plist-get
                                   plist
                                   (intern (concat symmatch
                                                   "-non-lisp-storage")))
                                  0))
                             (lisp-ancillary-storage
                              (or (plist-get
                                   plist
                                   (intern (concat symmatch
                                                   "-lisp-ancillary-storage")))
                                  0))
                             (storage-count 
                              (or (loop for str in '("s-used" "es-used" "-used")
                                    for val = (plist-get
                                               plist
                                               (intern
                                                (concat symmatch str)))
                                    if val
                                    return val)
                                  (plist-get 
                                   plist 
                                   (intern 
                                    (concat (substring symmatch 0 -1)
                                            "ies-used")))
                                  )))
                        (incf total-use storage-use)
                        (incf total-use-overhead storage-use-overhead)
                        (incf total-use-with-overhead storage-use-with-overhead)
                        (incf total-non-lisp-use non-lisp-storage)
                        (incf total-count (or storage-count 0))
                        (and (> storage-use-with-overhead 0)
                             (princ (format fmt symmatch 
                                            (or storage-count "unknown")
                                            storage-use
                                            storage-use-overhead
                                            non-lisp-storage
                                            lisp-ancillary-storage)))))))
              plist)
             (princ "\n")
             (princ (format fmt "total" 
                            total-count total-use total-use-overhead
                            total-non-lisp-use ""))
             (incf grandtotal total-use-with-overhead)
             (incf grandtotal total-non-lisp-use)
             (when-fboundp 'sort-numeric-fields
               (sort-numeric-fields -4
                                    (save-excursion
                                      (goto-char begin)
                                      (forward-line 4)
                                      (point))
                                    (save-excursion
                                      (forward-line -2)
                                      (point)))))))
      (with-output-to-temp-buffer buffer
      (save-excursion
	(set-buffer buffer)
	(setq begin (point))
	(princ "Allocated with lisp allocator or related:\n")
	(show-stats "\\(.*\\)-storage$")
	(princ (format "\n\ngrand total: %s\n" grandtotal)))
      grandtotal))))
  

(defun show-mc-alloc-memory-usage ()
  "Show statistics about memory usage of the new allocator."
  (interactive)
  (garbage-collect)
  (if-fboundp 'mc-alloc-memory-usage
      (let* ((stats (mc-alloc-memory-usage))
             (page-size (first stats))
             (heap-sects (second stats))
             (used-plhs (third stats))
             (free-plhs (fourth stats))
             (globals (fifth stats))
             (mc-malloced-bytes (sixth stats)))
        (with-output-to-temp-buffer "*mc-alloc memory usage*"
          (labels
              ((print-used-plhs (text plhs)
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
	
            (+ mc-malloced-bytes))))
    (message "mc-alloc not used in this XEmacs.")))


(defun show-gc-stats ()
  "Show statistics about garbage collection cycles."
  (interactive)
  (if-fboundp 'gc-stats
      (let ((buffer "*garbage collection statistics*")
            (plist (gc-stats))
            (fmt "%-9s %16s %12s %12s %12s %12s\n"))
        (labels
            ((plist-get-stat (category field)
               (let ((stat (plist-get plist (intern (concat category field)))))
                 (if stat
                     (format "%.0f" stat)
                   "-")))
             (show-stats (category)
               (princ (format fmt category
                              (plist-get-stat category "-total")
                              (plist-get-stat category "-in-last-gc")
                              (plist-get-stat category "-in-this-gc")
                              (plist-get-stat category "-in-last-cycle")
                              (plist-get-stat category "-in-this-cycle")))))
          (with-output-to-temp-buffer buffer
            (save-excursion
              (set-buffer buffer)
              (princ (format "%s %g\n" "Current phase"
                             (plist-get plist 'phase)))
              (princ (make-string 78 ?-))
              (princ "\n")
              (princ (format fmt "stat" "total" "last-gc" "this-gc" 
                             "last-cycle" "this-cycle"))
              (princ (make-string 78 ?-))
              (princ "\n")
              (show-stats "n-gc")
              (show-stats "n-cycles")
              (show-stats "enqueued")
              (show-stats "dequeued")
              (show-stats "repushed")
              (show-stats "enqueued2")
              (show-stats "dequeued2")
              (show-stats "finalized")
              (show-stats "freed")
              (plist-get plist 'n-gc-total)))))
    (error 'void-function "gc-stats not available.")))
