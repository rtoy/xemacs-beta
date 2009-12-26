;;; disp-table.el --- functions for dealing with char tables.

;; Copyright (C) 1987, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2005 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: i18n, internal

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; #### Needs work.

;; Rewritten for XEmacs July 1995, Ben Wing.
;; November 1998?, display tables generalized to char/range tables, Hrvoje
;; Niksic.
;; February 2005, rewrite this file to handle generalized display tables,
;; Ben Wing.

;;; Code:

(defun describe-display-table (dt)
  "Describe the display table DT in a help buffer."
  (with-displaying-help-buffer
   (lambda ()
     (princ "\nCharacter display glyph sequences:\n")
     (flet ((describe-display-table-entry
	      (entry stream)
	      ;; #### Write better version
	      (princ entry stream))
	    (describe-display-table-range
	      (first last entry)
	      (if (eq first last)
		  (princ (format "%s\t\t"
				 (single-key-description (int-char first))))
		(let ((str (format "%s - %s"
				   (single-key-description (int-char first))
				   (single-key-description (int-char last)))))
		  (princ str)
		  (princ (make-string (max (- 2 (/ (length str)
						   tab-width)) 1) ?\t))))
	      (describe-display-table-entry entry standard-output)
	      (terpri)))
       (cond ((vectorp dt)
	      (save-excursion
	       (let ((vector (make-vector 256 nil))
		     (i 0))
		 (while (< i 256)
		   (aset vector i (aref dt i))
		   (incf i))
		 ;; FSF calls `describe-vector' here, but it is so incredibly
		 ;; lame a function for that name that I cannot bring myself
		 ;; to port it.  Here is what `describe-vector' does:
		 (terpri)
		 (let ((old (aref vector 0))
		       (oldpos 0)
		       (i 1))
		   (while (<= i 256)
		     (when (or (= i 256)
			       (not (equal old (aref vector i))))
		       (describe-display-table-range oldpos (1- i) old)
		       (or (= i 256)
			   (setq old (aref vector i)
				 oldpos i)))
		     (incf i))))))
	     ((char-table-p dt)
	      (describe-char-table dt 'map-char-table
	       'describe-display-table-entry
	       standard-output))
	     ((range-table-p dt)
	      (map-range-table
	       #'(lambda (beg end value)
		   (describe-display-table-range beg end value))
	       dt)))))))

;;;###autoload
(defun describe-current-display-table (&optional domain)
  "Describe the display table in use in the selected window and buffer."
  (interactive)
  (or domain (setq domain (selected-window)))
  (let ((disptab (specifier-instance current-display-table domain)))
    (if disptab
	(describe-display-table disptab)
      (message "No display table"))))

;;;###autoload
(defun make-display-table ()
  "Return a new, empty display table.
Modify a display table using `put-display-table'.  Look up in display tables
using `get-display-table'.  The exact format of display tables and their
specs is described in `current-display-table'."
  ;; #### This should do something smarter.
  ;; #### Should use range table but there are bugs in range table and
  ;; perhaps in callers not expecting this.
  ;(make-range-table 'start-closed-end-closed)
  ;(make-vector 256 nil)
  ;; #### Should be type `display-table'
  (make-char-table 'generic))

(defun display-table-p (object)
  "Return t if OBJECT is a display table.
See `make-display-table'."
  (or (and (vectorp object) (= (length object) 256))
      (and (char-table-p object) (memq (char-table-type object)
				       '(char generic display)))
      (range-table-p object)))

;; #### we need a generic frob-specifier function.
;; #### this also needs to be redone like frob-face-property.

;; Let me say one more time how much dynamic scoping sucks.

;; #### Need more thinking about basic primitives for modifying a specifier.
;; cf `modify-specifier-instances'.

(defun frob-display-table (fdt-function fdt-locale &optional tag-set)
  (or fdt-locale (setq fdt-locale 'global))
  (or (specifier-spec-list current-display-table fdt-locale tag-set)
      (add-spec-to-specifier current-display-table (make-display-table)
			     fdt-locale tag-set))
  (add-spec-list-to-specifier
   current-display-table
   (list (cons fdt-locale
	       (mapcar
		(lambda (fdt-x)
                  (funcall fdt-function (cdr fdt-x))
                  fdt-x)
		(cdar (specifier-spec-list current-display-table
					   fdt-locale tag-set)))))))

(defun put-display-table-range (l h spec display-table)
  "Display characters in range L .. H, inclusive, in DISPLAY-TABLE using SPEC.
Display tables are described in `current-display-table'."
  (check-argument-type 'display-table-p display-table)
  (cond ((vectorp display-table)
	 (while (<= l h)
	   (aset display-table l spec)
	   (setq l (1+ l))))
	((char-table-p display-table)
	 (while (<= l h)
	   (put-char-table l spec display-table)
	   (setq l (1+ l))))
	((range-table-p display-table)
	 (put-range-table l h spec display-table))))

(defun put-display-table (ch spec display-table)
  "Display character spec CH in DISPLAY-TABLE using SPEC.
CH can be a character, a charset, or t for all characters.
Display tables are described in `current-display-table'."
  (cond ((eq ch t)
	 (cond ((vectorp display-table)
		(put-display-table-range 0 (1- (length display-table)) spec
					 display-table))
	       ((range-table-p display-table)
		; major hack
		(put-display-table-range 0 (string-to-int "3FFFFFFF" 16)
					 spec display-table))
	       ((char-table-p display-table)
		(put-char-table t spec display-table))))
	((charsetp ch)
	 (cond ((vectorp display-table)
		;; #### fix
		nil)
	       ((range-table-p display-table)
		;; #### fix
		nil)
	       ((char-table-p display-table)
		(put-char-table ch spec display-table))))
	(t (put-display-table-range ch ch spec display-table))))

(defun get-display-table (char display-table)
  "Return SPEC of CHAR in DISPLAY-TABLE.
See `current-display-table'."
  (check-argument-type 'display-table-p display-table)
  (cond ((vectorp display-table)
	 (aref display-table char))
	((char-table-p display-table)
	 (get-char-table char display-table))
	((range-table-p display-table)
	 (get-range-table char display-table))))

(defun standard-display-8bit-1 (dt l h)
  (while (<= l h)
    (put-display-table l (char-to-string l) dt)
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-8bit (l h &optional locale)
  "Display characters in the range L to H literally."
  (frob-display-table
   (lambda (x)
     (standard-display-8bit-1 x l h))
   locale))

(defun standard-display-default-1 (dt l h)
  (while (<= l h)
    (put-display-table l nil dt)
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-default (l h &optional locale)
  "Display characters in the range L to H using the default notation."
  (frob-display-table
   (lambda (x)
     (standard-display-default-1 x l h))
   locale))

;;;###autoload
(defun standard-display-ascii (c s &optional locale)
  "Display character C using printable string S."
  (frob-display-table
   (lambda (x)
     (put-display-table c s x))
   locale))

;;;###autoload
(defun standard-display-g1 (c sc &optional locale)
  "Display character C as character SC in the g1 character set.
This only has an effect on TTY devices and assumes that your terminal uses
the SO/SI characters."
  (frob-display-table
   (lambda (x)
     (put-display-table c (concat "\016" (char-to-string sc) "\017") x))
   locale
   'tty))

;;;###autoload
(defun standard-display-graphic (c gc &optional locale)
  "Display character C as character GC in graphics character set.
This only has an effect on TTY devices and assumes VT100-compatible escapes."
  (frob-display-table
   (lambda (x)
     (put-display-table c (concat "\e(0" (char-to-string gc) "\e(B") x))
   locale
   'tty))

;;; #### the FSF equivalent of this makes this character be displayed
;;; in the 'underline face.  There's no current way to do this with
;;; XEmacs display tables.

;;;###autoload
(defun standard-display-underline (c uc &optional locale)
  "Display character C as character UC plus underlining."
  (frob-display-table
   (lambda (x)
     (put-display-table c (concat "\e[4m" (char-to-string uc) "\e[m") x))
   locale
   'tty))

;;;###autoload
(defun standard-display-european (arg &optional locale)
  "Toggle display of European characters encoded with ISO 8859.
When enabled, characters in the range of 160 to 255 display not
as octal escapes, but as accented characters.
With prefix argument, enable European character display iff arg is positive."
  (interactive "P")
  (frob-display-table
   (lambda (x)
     (if (or (<= (prefix-numeric-value arg) 0)
             (and (null arg)
                  (equal (get-display-table 160 x) (char-to-string 160))))
         (standard-display-default-1 x 160 255)
       (standard-display-8bit-1 x 160 255)))
   locale))

(provide 'disp-table)

;;; disp-table.el ends here
