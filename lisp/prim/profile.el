;;; profile.el --- basic profiling commands for XEmacs

;; Copyright (C) 1996 Ben Wing.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

(defun pretty-print-profiling-info (&optional info)
  "Print profiling info INFO to standard output in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
`get-profiling-info'."
  (if info (setq info (copy-alist info))
    (setq info (get-profiling-info)))
  (setq info (nreverse (sort info #'cdr-less-than-cdr)))
  (princ "Function                                               Count        %\n")
  (princ "---------------------------------------------------------------------\n")
  (let ((sum 0.0)
	(info2 info))
    (while info2
      (setq sum (+ sum (cdar info2)))
      (setq info2 (cdr info2)))
    (while info
      (let ((f (caar info)))
	(princ (format "%-50s%10d   %6.3f\n" f (cdar info)
		       (* 100 (/ (cdar info) sum)))))
      (setq info (cdr info)))))
