;;; iso8859-1.el --- Set syntax table for Latin 1

;; Copyright (C) 1992, 1997, 2006 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Created: 19-aug-92
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with:  Not in FSF. 

;;; Commentary:

;; created by jwz, 19-aug-92.
;; Sets the case table for the ISO-8859/1 character set.
;; Used to set the syntax table. 

;;; Code:

(defconst iso8859/1-case-table nil
  "The case table for ISO-8859/1 characters.")

;;; This macro expands into
;;;  (setq iso8859/1-case-table (purecopy '("..." nil nil nil)))
;;; doing the computation of the case table at compile-time.

((macro
  . (lambda (&rest pairs)
      (let ((downcase (make-string 256 0))
	    (i 0))
	(while (< i 256)
	  (aset downcase i (if (and (>= i ?A) (<= i ?Z)) (+ i 32) i))
	  (setq i (1+ i)))
	(while pairs
	  (aset downcase (car (car pairs)) (car (cdr (car pairs))))
	  (setq pairs (cdr pairs)))
	(cons 'setq
	      (cons 'iso8859/1-case-table
		    (list
		     (list 'quote
			   (list downcase nil nil nil))))))))
 
 (?\300  ?\340)		; Agrave
 (?\301  ?\341)		; Aacute
 (?\302  ?\342)		; Acircumflex
 (?\303  ?\343)		; Atilde
 (?\304  ?\344)		; Adiaeresis
 (?\305  ?\345)		; Aring
 (?\306  ?\346)		; AE
 (?\307  ?\347)		; Ccedilla
 (?\310  ?\350)		; Egrave
 (?\311  ?\351)		; Eacute
 (?\312  ?\352)		; Ecircumflex
 (?\313  ?\353)		; Ediaeresis
 (?\314  ?\354)		; Igrave
 (?\315  ?\355)		; Iacute
 (?\316  ?\356)		; Icircumflex
 (?\317  ?\357)		; Idiaeresis
 (?\320  ?\360)		; ETH
 (?\321  ?\361)		; Ntilde
 (?\322  ?\362)		; Ograve
 (?\323  ?\363)		; Oacute
 (?\324  ?\364)		; Ocircumflex
 (?\325  ?\365)		; Otilde
 (?\326  ?\366)		; Odiaeresis
 (?\330  ?\370)		; Ooblique
 (?\331  ?\371)		; Ugrave
 (?\332  ?\372)		; Uacute
 (?\333  ?\373)		; Ucircumflex
 (?\334  ?\374)		; Udiaeresis
 (?\335  ?\375)		; Yacute
 (?\336  ?\376)		; THORN
 )

(set-standard-case-table (mapcar 'copy-sequence iso8859/1-case-table))

(setq-default ctl-arrow 'iso-8859/1)

(provide 'iso8859-1)

;;; iso8859-1.el ends here
