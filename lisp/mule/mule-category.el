;;; mule-category.el --- category functions for XEmacs/Mule. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2010 Ben Wing.

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

;;; Commentary:

;; Functions for working with category tables, which are a particular
;; type of char table.  Some function names / arguments should be
;; parallel with syntax tables.

;; Written by Ben Wing <ben@xemacs.org>.  The initialization code
;; at the end of this file comes from Mule.
;; Some bugfixes by Jareth Hein <jhod@po.iijnet.or.jp>

;;; Code:

(defvar defined-category-hashtable (make-hash-table :size 50))

(defun define-category (designator doc-string &optional table)
  "Make a new category whose designator is DESIGNATOR.
DESIGNATOR should be a visible letter of ' ' thru '~'.
DOC-STRING is a doc string for the category.
Letters of 'a' thru 'z' are already used or kept for the system.
The category should be defined only in category table TABLE, which defaults
to the current buffer's category table, but this is not implemented.  "
  ;; #### Implement the limiting of the definition. 
  (check-argument-type 'category-designator-p designator)
  (check-argument-type 'stringp doc-string)
  (setq table (or table (category-table)))
  (check-argument-type 'category-table-p table)
  (puthash designator doc-string defined-category-hashtable))

(defun undefine-category (designator)
  "Undefine DESIGNATOR as a designator for a category."
  (check-argument-type 'category-designator-p designator)
  (remhash designator defined-category-hashtable))

(defun defined-category-p (designator)
  "Return non-nil if DESIGNATOR is a designator for a defined category."
  (and (category-designator-p designator)
       (gethash designator defined-category-hashtable)))

(defun defined-category-list ()
  "Return a list of the currently defined categories.
Categories are given by their designators."
  (hash-table-key-list defined-category-hashtable))

(defun undefined-category-designator ()
  "Return an undefined category designator, or nil if there are none."
  (let ((a 32) found)
    (while (and (< a 127) (not found))
      (unless (gethash a defined-category-hashtable)
	(setq found (make-char 'ascii a)))
      (setq a (1+ a)))
    found))

(defun category-doc-string (designator)
  "Return the doc-string for the category denoted by DESIGNATOR."
  (check-argument-type 'defined-category-p designator)
  (gethash designator defined-category-hashtable))

(defun modify-category-entry (char-range designator &optional category-table reset)
  "Add a category to the categories associated with CHAR-RANGE.
CHAR-RANGE is a single character or a range of characters,
 as per `put-char-table'.
The category is given by a designator character.
The changes are made in CATEGORY-TABLE, which defaults to the current
 buffer's category table.
If optional fourth argument RESET is non-nil, previous categories associated
 with CHAR-RANGE are removed before adding the specified category."
  (check-argument-type 'defined-category-p designator)
  (modify-category-entry-internal char-range designator category-table reset))

(defun char-category-list (character &optional category-table)
  "Return a list of the categories that CHARACTER is in.
CATEGORY-TABLE defaults to the current buffer's category table.
The categories are given by their designators."
  (or category-table (setq category-table (category-table)))
  (check-argument-type 'category-table-p category-table)
  (let (list)
    (map-category-table
     #'(lambda (char desig)
	 (push desig list))
     category-table character)
    (nreverse list)))

(put 'with-category-table 'lisp-indent-function 1)

(defmacro with-category-table (category-table &rest body)
  `(let ((current-category-table (category-table)))
     (set-category-table ,category-table)
     (unwind-protect
	 (progn ,@body)
       (set-category-table current-category-table))))

(defun describe-category ()
  "Describe the category specifications in the category table.
The descriptions are inserted in a buffer, which is then displayed."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     (describe-category-table (category-table) standard-output))))

(defun describe-category-table (table stream)
  (let (first-char
	last-char
	prev-val
	(chartab (make-char-table 'generic)))
    (flet ((describe-one (first last value stream)
	     (if value
		 (progn
		   (if (equal first last)
		       (cond ((vectorp first)
			      (princ (format "%s, row %d"
					     (charset-name
					      (aref first 0))
					     (aref first 1))
				     stream))
			     ((charsetp first)
			      (princ (charset-name first) stream))
			     (t (princ first stream)))
		     (cond ((vectorp first)
			    (princ (format "%s, rows %d .. %d"
					   (charset-name
					    (aref first 0))
					   (aref first 1)
					   (aref last 1))
				   stream))
			   (t
			    (princ (format "%s .. %s" first last)
				   stream))))
		   (describe-category-code value stream)))))
      ;; We want to list things character-by-character.  So create a char table
      ;; whose entries are lists of the designators of the categories
      ;; that include that character.
      (message "Mapping over category table ...")
      (map-category-table
       #'(lambda (char desig)
	   (if (characterp char)
	       (let ((list (get-char-table char chartab)))
		 (put-char-table char (cons desig list) chartab))
	     (let (list)
	       (map-char-table
		#'(lambda (char2 desig2)
		    (push (cons char2 desig2) list)
		    nil)
		chartab char)
	       (put-char-table char (list desig) chartab)
	       (loop for (char2 . desig2) in (nreverse list) do
		 (put-char-table char2 (cons desig desig2) chartab))))
	   nil)
       table)
      (message "Mapping over char table ...")
      (map-char-table
       #'(lambda (range value)
	   (cond ((null first-char)
		  (setq first-char range
			last-char range
			prev-val value))
		 ((and (or
			(and (characterp range)
			     (characterp last-char)
			     (eq (char-charset range)
				 (char-charset last-char))
			     (= (char-to-int last-char)
				(1- (char-to-int range))))
			(and (vectorp range)
			     (vectorp last-char)
			     (eq (aref range 0) (aref last-char 0))
			     (= (aref last-char 1) (1- (aref range 1)))))
		       (equal value prev-val))
		  (setq last-char range))
		 (t
		  (describe-one first-char last-char prev-val stream)
		  (setq first-char range
			last-char range
			prev-val value)))
	   nil)
       chartab)
      (if first-char
	  (describe-one first-char last-char prev-val stream)))))

(defun describe-category-code (code stream)
  (let ((standard-output (or stream standard-output))
	(code (nreverse code)))
    (princ "\tin categories: ")
    (if (null code)
	(princ "(none)")
      (let (already-matched)
	(loop for c in code do
	  (if (not already-matched)
	      (setq already-matched t)
	    (princ " "))
	  (princ c))
	(loop for c in code do
	  (princ (format "\n\t\tmeaning: %s"
			 (category-doc-string c))))))
    (terpri)))

(defconst predefined-category-list
  `((latin-iso8859-1	?l "Latin-1 through Latin-5 character set")
    (latin-iso8859-2	?l)
    (latin-iso8859-3	?l)
    (latin-iso8859-4	?l)
    (latin-iso8859-9	?l)
    (cyrillic-iso8859-5 ?y "Cyrillic character set")
    (arabic-iso8859-6	?b "Arabic character set")
    (greek-iso8859-7	?g "Greek character set")
    (hebrew-iso8859-8	?w "Hebrew character set")
    (katakana-jisx0201	?k "Japanese 1-byte Katakana character set")
    (latin-jisx0201	?r "Japanese 1-byte Roman character set")
    (japanese-jisx0208-1978 ?j "Japanese 2-byte character set (old)")
    (japanese-jisx0208	?j "Japanese 2-byte character set")
    (japanese-jisx0212	?j)
    (chinese-gb2312	?c "Chinese GB (China, PRC) 2-byte character set")
    ;;@@#### This messes up things because it has ASCII and other chars in it.
    ;;We need to incorporate the GNU Emacs stuff in their characters.el, which has
    ;;much better category definitions.
    ;;(chinese-cns11643-1	?t "Chinese Taiwan (CNS or Big5) 2-byte character set")
    (chinese-cns11643-2	?t "Chinese Taiwan (CNS or Big5) 2-byte character set")
    (chinese-big5-1	?t)
    (chinese-big5-2	?t)
    (korean-ksc5601	?h "Hangul (Korean) 2-byte character set")
    (jit-ucs-charset-0  ?J "Just-in-time-allocated Unicode character")
    )
  "List of predefined categories.
Each element is a list of a charset, a designator, and maybe a doc string.")

(let (i l)
  (define-category ?a "ASCII character set.")
  (define-category ?l "Latin-1 through Latin-5 character set")
  (setq i 32)
  (while (< i 127)
    (modify-category-entry i ?a)
    (modify-category-entry i ?l)
    (setq i (1+ i)))
  (setq l predefined-category-list)
  (while l
    (when (find-charset (caar l))
      (if (and (nth 2 (car l))
               (not (defined-category-p (nth 2 (car l)))))
          (define-category (nth 1 (car l)) (nth 2 (car l))))
      (modify-category-entry (car (car l)) (nth 1 (car l)) nil t))
    (setq l (cdr l))))

;;; Setting word boundary.

(setq word-combining-categories
      ;; XEmacs; we should change to defining scripts, as does GNU, once
      ;; unicode-internal is the default, and placing word boundaries
      ;; between different scripts, not different charsets, by default.
      ;; Then we can remove the jit-ucs-charset-0 entry above and all the
      ;; entries containing ?J in this list.
      ;;
      ;; These entries are a bit heuristic, working on the assumption that
      ;; characters that will be just-in-time-allocated will not be East
      ;; Asian in XEmacs, and there's also no mechanism to apply the ?J
      ;; category to further newly-created JIT categories.
      '((?l . ?l) (?J . ?l) (?l . ?J) (?J . ?y) (?y . ?J) (?J . ?b) (?b . ?J)
        (?J . ?g) (?J . ?w) (?w . ?J)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?A . ?K)			; Alpha numeric - Katakana
	(?A . ?C)			; Alpha numeric - Chinese
	(?H . ?A)			; Hiragana - Alpha numeric
	(?H . ?K)			; Hiragana - Katakana
	(?H . ?C)			; Hiragana - Chinese
	(?K . ?A)			; Katakana - Alpha numeric
	(?K . ?C)			; Katakana - Chinese
	(?C . ?A)			; Chinese - Alpha numeric
	(?C . ?K)			; Chinese - Katakana
	))

;;; At the present, I know Japanese and Chinese text can
;;; break line at any point under a restriction of 'kinsoku'.
;;; #### SJT this needs to be set by language environments and probably should
;;; be buffer-local---strategy for dealing with this: check all $language.el
;;; files and also mule-base/$language-utils.el files for variables set;
;;; these should be made buffer local and some kind of a- or p-list of vars
;;; to be set for a language environment created. 
(defvar word-across-newline "\\(\\cj\\|\\cc\\|\\ct\\)"
  "Regular expression of such characters which can be a word across newline.")

(defvar ascii-char "[\40-\176]")
(defvar ascii-space "[ \t]")
(defvar ascii-symbols "[\40-\57\72-\100\133-\140\173-\176]")
(defvar ascii-numeric "[\60-\71]")
(defvar ascii-English-Upper "[\101-\132]")
(defvar ascii-English-Lower "[\141-\172]")
(defvar ascii-alphanumeric "[\60-\71\101-\132\141-\172]")

(defvar kanji-char "\\cj")
(defvar kanji-space "$B!!(B")
(defvar kanji-symbols "\\cS")
(defvar kanji-numeric "[$B#0(B-$B#9(B]")
(defvar kanji-English-Upper "[$B#A(B-$B#Z(B]")
(defvar kanji-English-Lower  "[$B#a(B-$B#z(B]")
(defvar kanji-hiragana "\\cH")
(defvar kanji-katakana "\\cK")
;; @@#### HACK FIXME.  The Unicode mappings for these double-width Kanji
;; characters are the regular Greek/Cyrillic equivalents, and so saving/loading
;; this file using Unicode-internal messes things up.  To get around this
;; for the moment, decode things on-the-fly.
(defvar kanji-Greek-Upper
  (decode-coding-string "[\033$B&!\033(B-\033$B&8\033(B]" 'iso-2022-7bit))
(defvar kanji-Greek-Lower
  (decode-coding-string "[\033$B&A\033(B-\033$B&X\033(B]" 'iso-2022-7bit))
(defvar kanji-Russian-Upper
  (decode-coding-string "[\033$B'!\033(B-\033$B'A\033(B]" 'iso-2022-7bit))
(defvar kanji-Russian-Lower
  (decode-coding-string "[\033$B'Q\033(B-\033$B'q\033(B]" 'iso-2022-7bit))
(defvar kanji-Kanji-1st-Level  "[$B0!(B-$BOS(B]")
(defvar kanji-Kanji-2nd-Level  "[$BP!(B-$Bt$(B]")

(defvar kanji-kanji-char "\\(\\cH\\|\\cK\\|\\cC\\)")
