;; Syntax-table hacking stuff, moved from syntax.c
;; Copyright (C) 1993 Free Software Foundation, Inc.

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

;;; Synched up with: FSF 19.28.
;;; Note: FSF does not have a file syntax.el.  This stuff is
;;; in syntax.c.  See comments there about not merging past 19.28.

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
It inherits all letters and control characters from the standard
syntax table; other characters are copied from the standard syntax table."
  (if oldtable
      (copy-syntax-table oldtable)
    (let ((table (copy-syntax-table))
	  i)
      (setq i 0)
      (while (<= i 31)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i 128)
      (while (<= i 255)
	(aset table i 13)
	(setq i (1+ i)))
      table)))

(defun modify-syntax-entry (char spec &optional table)
  "Set syntax for character CHAR according to string S.
The syntax is changed only for table TABLE, which defaults to
 the current buffer's syntax table.
The first character of S should be one of the following:
  Space    whitespace syntax.    w   word constituent.
  _        symbol constituent.   .   punctuation.
  \(        open-parenthesis.     \)   close-parenthesis.
  \"        string quote.         \\   character-quote.
  $        paired delimiter.     '   expression quote or prefix operator.
  <	   comment starter.	 >   comment ender.
  /           character-quote.      @   inherit from `standard-syntax-table'.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of S is the matching parenthesis,
 used only if the first character is `(' or `)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, 5, 6, 7, 8, p, a, and b.
 1 means C is the first of a two-char comment start sequence of style a.
 2 means C is the second character of such a sequence.
 3 means C is the first of a two-char comment end sequence of style a.
 4 means C is the second character of such a sequence.
 5 means C is the first of a two-char comment start sequence of style b.
 6 means C is the second character of such a sequence.
 7 means C is the first of a two-char comment end sequence of style b.
 8 means C is the second character of such a sequence.
 p means C is a prefix character for `backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.
 a means C is comment starter or comment ender for comment style a (default)
 b means C is comment starter or comment ender for comment style b."
  (interactive 
   ;; I really don't know why this is interactive
   ;; help-form should at least be made useful whilst reading the second arg
   "cSet syntax for character: \nsSet syntax for %c to: ")
  (cond ((syntax-table-p table))
        ((not table)
         (setq table (syntax-table)))
        (t
         (setq table
	       (wrong-type-argument 'syntax-table-p table))))
  (let* ((code nil)
         (bflag nil)
         (b3 0)
         i)
    (setq code (string-match (regexp-quote (char-to-string (elt spec 0)))
			     (syntax-designator-chars)))
    (or code 
        (error "Invalid syntax designator: %S" spec))
    (setq i 2)
    (while (< i (length spec))
      (let ((ch (elt spec i)))
        (setq i (1+ i))
        (cond ((= ch ?1)
               (setq b3 (logior b3 128)))
              ((= ch ?2)
               (setq b3 (logior b3 32)))
              ((= ch ?3)
               (setq b3 (logior b3 8)))
              ((= ch ?4)
               (setq b3 (logior b3 2)))
              ((= ch ?5)
               (setq b3 (logior b3 64)))
              ((= ch ?6)
               (setq b3 (logior b3 16)))
              ((= ch ?7)
               (setq b3 (logior b3 4)))
              ((= ch ?8)
               (setq b3 (logior b3 1)))
              ((= ch ?a)
               (cond ((= (elt spec 0) ?<)
                      (setq b3 (logior b3 128)))
                     ((= (elt spec 0) ?>)
                      (setq b3 (logior b3 8)))))
              ((= ch ?b)
               (cond ((= (elt spec 0) ?<)
                      (setq b3 (logior b3 64)
                            bflag t))
                     ((= (elt spec 0) ?>)
                      (setq b3 (logior b3 4)
                            bflag t))))
              ((= ch ?p)
               (setq code (logior code (lsh 1 7))))
              ((= ch ?\ )
               ;; ignore for compatibility
               )
              (t
               (error "Invalid syntax description flag: %S" spec)))))
    ;; default single char style is a if b has not been seen
    (if (not bflag)
        (cond ((= (elt spec 0) ?<)
               (setq b3 (logior b3 128)))
              ((= (elt spec 0) ?>)
               (setq b3 (logior b3 8)))))
    (aset table
          char
          (logior code
                  (if (and (> (length spec) 1)
                           ;; tough luck if you want to make space a paren!
                           (/= (elt spec 1) ?\  ))
                      ;; tough luck if you want to make \000 a paren!
                      (lsh (elt spec 1) 8)
                      0)
                  (lsh b3 16)))
    nil))

;(defun test-xm ()
;  (let ((o (copy-syntax-table))
;        (n (copy-syntax-table))
;        (codes (syntax-designator-chars))
;        (flags "12345678abp"))
;    (while t
;      (let ((spec (concat (char-to-string (elt codes
;						(random (length codes))))))
;                          (if (= (random 4) 0)
;                              "b"
;                              " ")
;                          (let* ((n (random 4))
;                                 (s (make-string n 0)))
;                            (while (> n 0)
;                              (setq n (1- n))
;                              (aset s n (aref flags (random (length flags)))))
;                            s))))
;        (message "%S..." spec)
;        (modify-syntax-entry ?a spec o)
;        (xmodify-syntax-entry ?a spec n)
;        (or (= (aref o ?a) (aref n ?a))
;            (error "%s"
;                   (format "fucked with %S: %x %x"
;                           spec (aref o ?a) (aref n ?a))))))))


(defun describe-syntax-table (table stream)
  (let* (;(limit (cond ((numberp ctl-arrow) ctl-arrow)
;		      ((memq ctl-arrow '(t nil)) 256)
;		      (t 160)))
	 (describe-one #'(lambda (first last)
			   (let* ((tem (text-char-description first))
				  (pos (length tem)))
			     (princ tem stream)
			     (if (> last first)
				 (progn
				   (princ " .. " stream)
				   (setq tem (text-char-description last))
				   (princ tem stream)
				   (setq pos (+ pos (length tem) 4))))
			     (while (progn (write-char ?\  stream)
					   (setq pos (1+ pos))
					   (< pos 16))))
			   (describe-syntax-code (elt table first) stream))))
    (let ((range 0)
          (i 0)
          (code (elt table 0)))
      (while (cond ((= i (length table))
                    (funcall describe-one (1- i) (1- i))
                    nil)
                   ((eq code (elt table i))
                    t)
                   (t
                    (funcall describe-one range (1- i))
                    (setq code (elt table i)
                          range i)
                    t))
        (setq i (1+ i))))))

(defun describe-syntax-code (code stream)
  (let ((codes (syntax-designator-chars))
	(invalid (gettext "**invalid**")) ;(empty "") ;constants
	(standard-output (or stream standard-output))
	;; #### I18N3 should temporarily set buffer to output-translatable
        (in #'(lambda (string)
                (princ ",\n\t\t\t\t ")
                (princ string))))
    (if (or (not (integerp code))
            (> (logand code 127) (length codes)))
        (princ invalid)
      (let* ((spec (elt codes (logand code 127)))
	     (match (logand (lsh code -8) 255))
	     (b3 (lsh code -16))
	     (start1  (/= 0 (logand b3 128))) ;logtest!
	     (start1b (/= 0 (logand b3  64)))
	     (start2  (/= 0 (logand b3  32)))
	     (start2b (/= 0 (logand b3  16)))
	     (end1    (/= 0 (logand b3   8)))
	     (end1b   (/= 0 (logand b3   4)))
	     (end2    (/= 0 (logand b3   2)))
	     (end2b   (/= 0 (logand b3   1)))
	     (prefix  (/= 0 (logand code 128)))
	     (single-char-p (or (= spec ?<) (= spec ?>)))
	     )
        (write-char spec)
	(write-char (if (= 0 match) 32 match))
;;	(if start1 (if single-char-p (write-char ?a) (write-char ?1)))
	(if start1 (if single-char-p (write-char ? ) (write-char ?1)))
	(if start2 (write-char ?2))
;;	(if end1 (if single-char-p (write-char ?a) (write-char ?3)))
	(if end1 (if single-char-p (write-char ? ) (write-char ?3)))
	(if end2 (write-char ?4))
	(if start1b (if single-char-p (write-char ?b) (write-char ?5)))
	(if start2b (write-char ?6))
	(if end1b (if single-char-p (write-char ?b) (write-char ?7)))
	(if end2b (write-char ?8))
	(if prefix (write-char ?p))

        (princ "\tmeaning: ")
        (princ (aref ["whitespace" "punctuation" "word-constituent"
		      "symbol-constituent" "open-paren" "close-paren"
		      "expression-prefix" "string-quote" "paired-delimiter"
		      "escape" "character-quote" "comment-begin" "comment-end"
		      "inherit" "extended-word-constituent"]
		     (logand code 127)))

        (if (/= 0 match)
            (progn
              (princ ", matches ")
	      (princ (text-char-description match))))
	(if start1
	    (if single-char-p
		(princ ", style A")
              (funcall in (gettext "first character of comment-start sequence A"))))
	(if start2
	    (funcall in (gettext "second character of comment-start sequence A")))
	(if end1
	    (if single-char-p
		(princ ", style A")
              (funcall in (gettext "first character of comment-end sequence A"))))
	(if end2
	    (funcall in (gettext "second character of comment-end sequence A")))
	(if start1b
	    (if single-char-p
		(princ ", style B")
              (funcall in (gettext "first character of comment-start sequence B"))))
	(if start2b
	    (funcall in (gettext "second character of comment-start sequence B")))
	(if end1b
	    (if single-char-p
		(princ ", style B")
              (funcall in (gettext "first character of comment-end sequence B"))))
	(if end2b
	    (funcall in (gettext "second character of comment-end sequence B")))
	(if prefix
	    (funcall in (gettext "prefix character for `backward-prefix-chars'")))))
    (terpri stream)))

(defun symbol-near-point ()
  "Return the first textual item to the nearest point."
  (interactive)
  ;alg stolen from etag.el
  (save-excursion
	(if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	    (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	      (forward-char 1)))
	(while (looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(if (re-search-backward "\\sw\\|\\s_" nil t)
	    (regexp-quote
	     (progn (forward-char 1)
		    (buffer-substring (point)
				      (progn (forward-sexp -1)
					     (while (looking-at "\\s'")
					       (forward-char 1))
					     (point)))))
	  nil)))
