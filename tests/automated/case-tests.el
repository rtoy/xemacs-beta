;;; -*- coding: iso-8859-1 -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.
;; Copyright (C) 2010 Ben Wing.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Created: 2000
;; Keywords: tests

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

;; Test case-table related functionality.

(defvar pristine-case-table nil
  "The standard case table, without manipulation from case-tests.el")

(setq pristine-case-table (or
			   ;; This is the compiled run; we've retained
			   ;; it from the interpreted run.
			   pristine-case-table 
			   ;; This is the interpreted run; set it.
			   (copy-case-table (standard-case-table))))

(Assert (case-table-p (standard-case-table)))
;; Old case table test.
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       nil nil nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       nil nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       (make-string 256 ?c)
		       nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       (make-string 256 ?c)
		       (make-string 256 ?d))))
(Assert (not (case-table-p (list (make-string 256 ?a)
				 (make-string 256 ?b)
				 (make-string 256 ?c)
				 (make-string 254 ?d)))))
(Assert (not (case-table-p (list (make-string 256 ?a)))))

(Assert (case-table-p (set-case-table (current-case-table))))

(defvar string-0-through-32
  (let ((result (make-string 33 (int-to-char 0))))
    (dotimes (i 33)
      (aset result i (int-to-char i)))
    result)
  "String containing characters from code point 0 (NUL) through 32 (SPC).")

(defvar string-127-through-160
  (let ((result (make-string 34 (int-to-char 0))))
    (dotimes (i 34)
      (aset result i (int-to-char (+ 127 i))))
    result)
  "String containing characters from code point 127 (DEL) through 160
\(no-break-space).")

;; Case table sanity check.
(let ((downcase-string
       (concat string-0-through-32
	       "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
	       string-127-through-160
		"¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))
       (upcase-string
	(concat string-0-through-32
		"!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~"
		string-127-through-160
		"¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))
       (table (standard-case-table)))
  (dotimes (i 256)
    (Assert-eq (get-case-table 'downcase (int-to-char i) table)
		(aref downcase-string i))
    (Assert-eq (get-case-table 'upcase (int-to-char i) table)
		(aref upcase-string i))))

(Check-Error-Message error "Char case must be downcase or upcase"
		     (get-case-table 'foo ?a (standard-case-table)))

(Assert
 (string=
  (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(Assert
 (string=
  (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(Assert
 (string=
  (upcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))

(Assert
 (string=
  (upcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))

(Assert
 (string=
  (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
  "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz"))

(Assert
 (string=
  (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz"))

(Assert
 (string=
  (downcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))

(Assert
 (string=
  (downcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))

;; Old case table format test.
(with-temp-buffer
  (set-case-table
   (list
    (concat string-0-through-32
	     "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
	     string-127-through-160
	     "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
     nil nil nil))
  (Assert
   (string=
    (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
    "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (Assert
   (string=
    (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")))

(with-temp-buffer
  (insert "Test Buffer")
  (let ((case-fold-search t))
    (goto-char (point-min))
    (Assert-eq (search-forward "test buffer" nil t) 12)
    (goto-char (point-min))
    (Assert-eq (search-forward "Test buffer" nil t) 12)
    (goto-char (point-min))
    (Assert-eq (search-forward "Test Buffer" nil t) 12)

    (setq case-fold-search nil)
    (goto-char (point-min))
    (Assert (not (search-forward "test buffer" nil t)))
    (goto-char (point-min))
    (Assert (not (search-forward "Test buffer" nil t)))
    (goto-char (point-min))
    (Assert-eq (search-forward "Test Buffer" nil t) 12)))

(with-temp-buffer
  (insert "abcdefghijklmnäopqrstuÄvwxyz")
  ;; case insensitive
  (Assert (not (search-forward "ö" nil t)))
  (goto-char (point-min))
  (Assert-eq 16 (search-forward "ä" nil t))
  (Assert-eq 24 (search-forward "ä" nil t))
  (goto-char (point-min))
  (Assert-eq 16 (search-forward "Ä" nil t))
  (Assert-eq 24 (search-forward "Ä" nil t))
  (goto-char (point-max))
  (Assert-eq 23 (search-backward "ä" nil t))
  (Assert-eq 15 (search-backward "ä" nil t))
  (goto-char (point-max))
  (Assert-eq 23 (search-backward "Ä" nil t))
  (Assert-eq 15 (search-backward "Ä" nil t))
  ;; case sensitive
  (setq case-fold-search nil)
  (goto-char (point-min))
  (Assert (not (search-forward "ö" nil t)))
  (goto-char (point-min))
  (Assert-eq 16 (search-forward "ä" nil t))
  (Assert (not (search-forward "ä" nil t)))
  (goto-char (point-min))
  (Assert-eq 24 (search-forward "Ä" nil t))
  (goto-char 16)
  (Assert-eq 24 (search-forward "Ä" nil t))
  (goto-char (point-max))
  (Assert-eq 15 (search-backward "ä" nil t))
  (goto-char 15)
  (Assert (not (search-backward "ä" nil t)))
  (goto-char (point-max))
  (Assert-eq 23 (search-backward "Ä" nil t))
  (Assert (not (search-backward "Ä" nil t))))

(with-temp-buffer
  (insert "aaaaäÄäÄäÄäÄäÄbbbb")
  (goto-char (point-min))
  (Assert-eq 15 (search-forward "ää" nil t 5))
  (goto-char (point-min))
  (Assert (not (search-forward "ää" nil t 6)))
  (goto-char (point-max))
  (Assert-eq 5 (search-backward "ää" nil t 5))
  (goto-char (point-max))
  (Assert (not (search-backward "ää" nil t 6))))

(when (featurep 'mule)
  (let* ((hiragana-a (make-char 'japanese-jisx0208 36 34))
	 (a-diaeresis ?ä)
	 (case-table (copy-case-table (standard-case-table)))
	 (str-hiragana-a (char-to-string hiragana-a))
	 (str-a-diaeresis (char-to-string a-diaeresis))
	 (string (concat str-hiragana-a str-a-diaeresis)))
    (put-case-table-pair hiragana-a a-diaeresis case-table)
    (with-temp-buffer
      (set-case-table case-table)
      (insert hiragana-a "abcdefg" a-diaeresis)
      ;; forward
      (goto-char (point-min))
      (Assert (not (search-forward "ö" nil t)))
      (goto-char (point-min))
      (Assert-eq 2 (search-forward str-hiragana-a nil t))
      (goto-char (point-min))
      (Assert-eq 2 (search-forward str-a-diaeresis nil t))
      (goto-char (1+ (point-min)))
      (Assert-eq (point-max)
		  (search-forward str-hiragana-a nil t))
      (goto-char (1+ (point-min)))
      (Assert-eq (point-max)
		  (search-forward str-a-diaeresis nil t))
      ;; backward
      (goto-char (point-max))
      (Assert (not (search-backward "ö" nil t)))
      (goto-char (point-max))
      (Assert-eq (1- (point-max)) (search-backward str-hiragana-a nil t))
      (goto-char (point-max))
      (Assert-eq (1- (point-max)) (search-backward str-a-diaeresis nil t))
      (goto-char (1- (point-max)))
      (Assert-eq 1 (search-backward str-hiragana-a nil t))
      (goto-char (1- (point-max)))
      (Assert-eq 1 (search-backward str-a-diaeresis nil t))
      (replace-match "a")
      (Assert (looking-at (format "abcdefg%c" a-diaeresis))))
    (with-temp-buffer
      (set-case-table case-table)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (goto-char (point-min))
      (Assert-eq 11 (search-forward string nil t 5))
      (goto-char (point-min))
      (Assert (not (search-forward string nil t 6)))
      (goto-char (point-max))
      (Assert-eq 1 (search-backward string nil t 5))
      (goto-char (point-max))
      (Assert (not (search-backward string nil t 6))))))

;; Bug reported in http://mid.gmane.org/y9lk5lu5orq.fsf@deinprogramm.de from
;; Michael Sperber. Fixed 2008-01-29.
(with-string-as-buffer-contents "\n\nDer beruhmte deutsche Flei\xdf\n\n"
  (goto-char (point-min))
  (Assert (search-forward "Flei\xdf")))

(with-temp-buffer
  (let ((target "M\xe9zard")
        (debug-xemacs-searches 1))
    (Assert (not (search-forward target nil t)))
    (insert target)
    (goto-char (point-min))
    ;; #### search-algorithm-used is simple-search after the following,
    ;; which shouldn't be necessary; it should be possible to use
    ;; Boyer-Moore. 
    ;;
    ;; But searches for ASCII strings in buffers with nothing above ?\xFF
    ;; use Boyer Moore with the current implementation, which is the
    ;; important thing for the Gnus use case.
    (Assert= (1+ (length target)) (search-forward target nil t))))

(Skip-Test-Unless
 (boundp 'debug-xemacs-searches) ; normal when we have DEBUG_XEMACS
 "not a DEBUG_XEMACS build"
 "checks that the algorithm chosen by #'search-forward is relatively sane"
 (let ((debug-xemacs-searches 1))
   (with-temp-buffer
     (set-case-table pristine-case-table)
     (insert "\n\nDer beruhmte deutsche Fleiss\n\n")
     (goto-char (point-min))
     (Assert (search-forward "Fleiss"))
     (delete-region (point-min) (point-max))
     (insert "\n\nDer beruhmte deutsche Flei\xdf\n\n")
     (goto-char (point-min))
     (Assert (search-forward "Flei\xdf"))
     (Assert-eq 'boyer-moore search-algorithm-used)
     (delete-region (point-min) (point-max))
     (when (featurep 'mule)
       (insert "\n\nDer beruhmte deutsche Flei\xdf\n\n")
       (goto-char (point-min))
       (Assert 
        (search-forward (format "Fle%c\xdf"
                                (make-char 'latin-iso8859-9 #xfd))))
       (Assert-eq 'boyer-moore search-algorithm-used)
       (insert (make-char 'latin-iso8859-9 #xfd))
       (goto-char (point-min))
       (Assert (search-forward "Flei\xdf"))
       (Assert-eq 'simple-search search-algorithm-used) 
       (goto-char (point-min))
       (Assert (search-forward (format "Fle%c\xdf"
                                       (make-char 'latin-iso8859-9 #xfd))))
       (Assert-eq 'simple-search search-algorithm-used)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Torture test, using all the non-"full" mappings from the Unicode case
;; tables. (Full mappings are those that involve sequences of more than one
;; character, e.g. lowercase German ß (esszet) -> uppercase sequence "SS").

(defun char-as-unicode-escape (ch)
  "Return a string corresponding to the Unicode read-expression of CH.
For example, if CH is ?\\u00F1, the return value will be the string
\"?\\\\u00F1\"."
  (let ((uni (char-to-unicode ch)))
    (if (< uni #x10000) (format "?\\u%04X" uni) (format "?\\U%08X" uni))))

(let* ((uni-mappings
	'(;  UPPER   LOWER
	  (?\u0041 ?\u0061) ;; LATIN CAPITAL LETTER A
	  (?\u0042 ?\u0062) ;; LATIN CAPITAL LETTER B
	  (?\u0043 ?\u0063) ;; LATIN CAPITAL LETTER C
	  (?\u0044 ?\u0064) ;; LATIN CAPITAL LETTER D
	  (?\u0045 ?\u0065) ;; LATIN CAPITAL LETTER E
	  (?\u0046 ?\u0066) ;; LATIN CAPITAL LETTER F
	  (?\u0047 ?\u0067) ;; LATIN CAPITAL LETTER G
	  (?\u0048 ?\u0068) ;; LATIN CAPITAL LETTER H
	  (?\u0049 ?\u0069) ;; LATIN CAPITAL LETTER I
;;; WARNING: Unhandled Turkish mapping:
;;;        0049; T; 0131; # LATIN CAPITAL LETTER I
	  (?\u004A ?\u006A) ;; LATIN CAPITAL LETTER J
	  (?\u004B ?\u006B) ;; LATIN CAPITAL LETTER K
	  (?\u004C ?\u006C) ;; LATIN CAPITAL LETTER L
	  (?\u004D ?\u006D) ;; LATIN CAPITAL LETTER M
	  (?\u004E ?\u006E) ;; LATIN CAPITAL LETTER N
	  (?\u004F ?\u006F) ;; LATIN CAPITAL LETTER O
	  (?\u0050 ?\u0070) ;; LATIN CAPITAL LETTER P
	  (?\u0051 ?\u0071) ;; LATIN CAPITAL LETTER Q
	  (?\u0052 ?\u0072) ;; LATIN CAPITAL LETTER R
	  (?\u0053 ?\u0073) ;; LATIN CAPITAL LETTER S
	  (?\u0054 ?\u0074) ;; LATIN CAPITAL LETTER T
	  (?\u0055 ?\u0075) ;; LATIN CAPITAL LETTER U
	  (?\u0056 ?\u0076) ;; LATIN CAPITAL LETTER V
	  (?\u0057 ?\u0077) ;; LATIN CAPITAL LETTER W
	  (?\u0058 ?\u0078) ;; LATIN CAPITAL LETTER X
	  (?\u0059 ?\u0079) ;; LATIN CAPITAL LETTER Y
	  (?\u005A ?\u007A) ;; LATIN CAPITAL LETTER Z
	  (?\u00B5 ?\u03BC) ;; MICRO SIGN
	  (?\u00C0 ?\u00E0) ;; LATIN CAPITAL LETTER A WITH GRAVE
	  (?\u00C1 ?\u00E1) ;; LATIN CAPITAL LETTER A WITH ACUTE
	  (?\u00C2 ?\u00E2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
	  (?\u00C3 ?\u00E3) ;; LATIN CAPITAL LETTER A WITH TILDE
	  (?\u00C4 ?\u00E4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
	  (?\u00C5 ?\u00E5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
	  (?\u00C6 ?\u00E6) ;; LATIN CAPITAL LETTER AE
	  (?\u00C7 ?\u00E7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
	  (?\u00C8 ?\u00E8) ;; LATIN CAPITAL LETTER E WITH GRAVE
	  (?\u00C9 ?\u00E9) ;; LATIN CAPITAL LETTER E WITH ACUTE
	  (?\u00CA ?\u00EA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
	  (?\u00CB ?\u00EB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
	  (?\u00CC ?\u00EC) ;; LATIN CAPITAL LETTER I WITH GRAVE
	  (?\u00CD ?\u00ED) ;; LATIN CAPITAL LETTER I WITH ACUTE
	  (?\u00CE ?\u00EE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
	  (?\u00CF ?\u00EF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
	  (?\u00D0 ?\u00F0) ;; LATIN CAPITAL LETTER ETH
	  (?\u00D1 ?\u00F1) ;; LATIN CAPITAL LETTER N WITH TILDE
	  (?\u00D2 ?\u00F2) ;; LATIN CAPITAL LETTER O WITH GRAVE
	  (?\u00D3 ?\u00F3) ;; LATIN CAPITAL LETTER O WITH ACUTE
	  (?\u00D4 ?\u00F4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
	  (?\u00D5 ?\u00F5) ;; LATIN CAPITAL LETTER O WITH TILDE
	  (?\u00D6 ?\u00F6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
	  (?\u00D8 ?\u00F8) ;; LATIN CAPITAL LETTER O WITH STROKE
	  (?\u00D9 ?\u00F9) ;; LATIN CAPITAL LETTER U WITH GRAVE
	  (?\u00DA ?\u00FA) ;; LATIN CAPITAL LETTER U WITH ACUTE
	  (?\u00DB ?\u00FB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
	  (?\u00DC ?\u00FC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
	  (?\u00DD ?\u00FD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
	  (?\u00DE ?\u00FE) ;; LATIN CAPITAL LETTER THORN
;;; WARNING: Unhandled full mapping:
;;;   00DF; F; 0073 0073; # LATIN SMALL LETTER SHARP S
	  (?\u0100 ?\u0101) ;; LATIN CAPITAL LETTER A WITH MACRON
	  (?\u0102 ?\u0103) ;; LATIN CAPITAL LETTER A WITH BREVE
	  (?\u0104 ?\u0105) ;; LATIN CAPITAL LETTER A WITH OGONEK
	  (?\u0106 ?\u0107) ;; LATIN CAPITAL LETTER C WITH ACUTE
	  (?\u0108 ?\u0109) ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
	  (?\u010A ?\u010B) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
	  (?\u010C ?\u010D) ;; LATIN CAPITAL LETTER C WITH CARON
	  (?\u010E ?\u010F) ;; LATIN CAPITAL LETTER D WITH CARON
	  (?\u0110 ?\u0111) ;; LATIN CAPITAL LETTER D WITH STROKE
	  (?\u0112 ?\u0113) ;; LATIN CAPITAL LETTER E WITH MACRON
	  (?\u0114 ?\u0115) ;; LATIN CAPITAL LETTER E WITH BREVE
	  (?\u0116 ?\u0117) ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
	  (?\u0118 ?\u0119) ;; LATIN CAPITAL LETTER E WITH OGONEK
	  (?\u011A ?\u011B) ;; LATIN CAPITAL LETTER E WITH CARON
	  (?\u011C ?\u011D) ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
	  (?\u011E ?\u011F) ;; LATIN CAPITAL LETTER G WITH BREVE
	  (?\u0120 ?\u0121) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
	  (?\u0122 ?\u0123) ;; LATIN CAPITAL LETTER G WITH CEDILLA
	  (?\u0124 ?\u0125) ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
	  (?\u0126 ?\u0127) ;; LATIN CAPITAL LETTER H WITH STROKE
	  (?\u0128 ?\u0129) ;; LATIN CAPITAL LETTER I WITH TILDE
	  (?\u012A ?\u012B) ;; LATIN CAPITAL LETTER I WITH MACRON
	  (?\u012C ?\u012D) ;; LATIN CAPITAL LETTER I WITH BREVE
	  (?\u012E ?\u012F) ;; LATIN CAPITAL LETTER I WITH OGONEK
;;; WARNING: Unhandled full mapping:
;;;   0130; F; 0069 0307; # LATIN CAPITAL LETTER I WITH DOT ABOVE
;;; WARNING: Unhandled Turkish mapping:
;;;        0130; T; 0069; # LATIN CAPITAL LETTER I WITH DOT ABOVE
	  (?\u0132 ?\u0133) ;; LATIN CAPITAL LIGATURE IJ
	  (?\u0134 ?\u0135) ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
	  (?\u0136 ?\u0137) ;; LATIN CAPITAL LETTER K WITH CEDILLA
	  (?\u0139 ?\u013A) ;; LATIN CAPITAL LETTER L WITH ACUTE
	  (?\u013B ?\u013C) ;; LATIN CAPITAL LETTER L WITH CEDILLA
	  (?\u013D ?\u013E) ;; LATIN CAPITAL LETTER L WITH CARON
	  (?\u013F ?\u0140) ;; LATIN CAPITAL LETTER L WITH MIDDLE DOT
	  (?\u0141 ?\u0142) ;; LATIN CAPITAL LETTER L WITH STROKE
	  (?\u0143 ?\u0144) ;; LATIN CAPITAL LETTER N WITH ACUTE
	  (?\u0145 ?\u0146) ;; LATIN CAPITAL LETTER N WITH CEDILLA
	  (?\u0147 ?\u0148) ;; LATIN CAPITAL LETTER N WITH CARON
;;; WARNING: Unhandled full mapping:
;;;   0149; F; 02BC 006E; # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
	  (?\u014A ?\u014B) ;; LATIN CAPITAL LETTER ENG
	  (?\u014C ?\u014D) ;; LATIN CAPITAL LETTER O WITH MACRON
	  (?\u014E ?\u014F) ;; LATIN CAPITAL LETTER O WITH BREVE
	  (?\u0150 ?\u0151) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
	  (?\u0152 ?\u0153) ;; LATIN CAPITAL LIGATURE OE
	  (?\u0154 ?\u0155) ;; LATIN CAPITAL LETTER R WITH ACUTE
	  (?\u0156 ?\u0157) ;; LATIN CAPITAL LETTER R WITH CEDILLA
	  (?\u0158 ?\u0159) ;; LATIN CAPITAL LETTER R WITH CARON
	  (?\u015A ?\u015B) ;; LATIN CAPITAL LETTER S WITH ACUTE
	  (?\u015C ?\u015D) ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
	  (?\u015E ?\u015F) ;; LATIN CAPITAL LETTER S WITH CEDILLA
	  (?\u0160 ?\u0161) ;; LATIN CAPITAL LETTER S WITH CARON
	  (?\u0162 ?\u0163) ;; LATIN CAPITAL LETTER T WITH CEDILLA
	  (?\u0164 ?\u0165) ;; LATIN CAPITAL LETTER T WITH CARON
	  (?\u0166 ?\u0167) ;; LATIN CAPITAL LETTER T WITH STROKE
	  (?\u0168 ?\u0169) ;; LATIN CAPITAL LETTER U WITH TILDE
	  (?\u016A ?\u016B) ;; LATIN CAPITAL LETTER U WITH MACRON
	  (?\u016C ?\u016D) ;; LATIN CAPITAL LETTER U WITH BREVE
	  (?\u016E ?\u016F) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
	  (?\u0170 ?\u0171) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
	  (?\u0172 ?\u0173) ;; LATIN CAPITAL LETTER U WITH OGONEK
	  (?\u0174 ?\u0175) ;; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
	  (?\u0176 ?\u0177) ;; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
	  (?\u0178 ?\u00FF) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
	  (?\u0179 ?\u017A) ;; LATIN CAPITAL LETTER Z WITH ACUTE
	  (?\u017B ?\u017C) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
	  (?\u017D ?\u017E) ;; LATIN CAPITAL LETTER Z WITH CARON
	  (?\u017F ?\u0073) ;; LATIN SMALL LETTER LONG S
	  (?\u0181 ?\u0253) ;; LATIN CAPITAL LETTER B WITH HOOK
	  (?\u0182 ?\u0183) ;; LATIN CAPITAL LETTER B WITH TOPBAR
	  (?\u0184 ?\u0185) ;; LATIN CAPITAL LETTER TONE SIX
	  (?\u0186 ?\u0254) ;; LATIN CAPITAL LETTER OPEN O
	  (?\u0187 ?\u0188) ;; LATIN CAPITAL LETTER C WITH HOOK
	  (?\u0189 ?\u0256) ;; LATIN CAPITAL LETTER AFRICAN D
	  (?\u018A ?\u0257) ;; LATIN CAPITAL LETTER D WITH HOOK
	  (?\u018B ?\u018C) ;; LATIN CAPITAL LETTER D WITH TOPBAR
	  (?\u018E ?\u01DD) ;; LATIN CAPITAL LETTER REVERSED E
	  (?\u018F ?\u0259) ;; LATIN CAPITAL LETTER SCHWA
	  (?\u0190 ?\u025B) ;; LATIN CAPITAL LETTER OPEN E
	  (?\u0191 ?\u0192) ;; LATIN CAPITAL LETTER F WITH HOOK
	  (?\u0193 ?\u0260) ;; LATIN CAPITAL LETTER G WITH HOOK
	  (?\u0194 ?\u0263) ;; LATIN CAPITAL LETTER GAMMA
	  (?\u0196 ?\u0269) ;; LATIN CAPITAL LETTER IOTA
	  (?\u0197 ?\u0268) ;; LATIN CAPITAL LETTER I WITH STROKE
	  (?\u0198 ?\u0199) ;; LATIN CAPITAL LETTER K WITH HOOK
	  (?\u019C ?\u026F) ;; LATIN CAPITAL LETTER TURNED M
	  (?\u019D ?\u0272) ;; LATIN CAPITAL LETTER N WITH LEFT HOOK
	  (?\u019F ?\u0275) ;; LATIN CAPITAL LETTER O WITH MIDDLE TILDE
	  (?\u01A0 ?\u01A1) ;; LATIN CAPITAL LETTER O WITH HORN
	  (?\u01A2 ?\u01A3) ;; LATIN CAPITAL LETTER OI
	  (?\u01A4 ?\u01A5) ;; LATIN CAPITAL LETTER P WITH HOOK
	  (?\u01A6 ?\u0280) ;; LATIN LETTER YR
	  (?\u01A7 ?\u01A8) ;; LATIN CAPITAL LETTER TONE TWO
	  (?\u01A9 ?\u0283) ;; LATIN CAPITAL LETTER ESH
	  (?\u01AC ?\u01AD) ;; LATIN CAPITAL LETTER T WITH HOOK
	  (?\u01AE ?\u0288) ;; LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
	  (?\u01AF ?\u01B0) ;; LATIN CAPITAL LETTER U WITH HORN
	  (?\u01B1 ?\u028A) ;; LATIN CAPITAL LETTER UPSILON
	  (?\u01B2 ?\u028B) ;; LATIN CAPITAL LETTER V WITH HOOK
	  (?\u01B3 ?\u01B4) ;; LATIN CAPITAL LETTER Y WITH HOOK
	  (?\u01B5 ?\u01B6) ;; LATIN CAPITAL LETTER Z WITH STROKE
	  (?\u01B7 ?\u0292) ;; LATIN CAPITAL LETTER EZH
	  (?\u01B8 ?\u01B9) ;; LATIN CAPITAL LETTER EZH REVERSED
	  (?\u01BC ?\u01BD) ;; LATIN CAPITAL LETTER TONE FIVE
	  (?\u01C4 ?\u01C6) ;; LATIN CAPITAL LETTER DZ WITH CARON
	  (?\u01C5 ?\u01C6) ;; LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
	  (?\u01C7 ?\u01C9) ;; LATIN CAPITAL LETTER LJ
	  (?\u01C8 ?\u01C9) ;; LATIN CAPITAL LETTER L WITH SMALL LETTER J
	  (?\u01CA ?\u01CC) ;; LATIN CAPITAL LETTER NJ
	  (?\u01CB ?\u01CC) ;; LATIN CAPITAL LETTER N WITH SMALL LETTER J
	  (?\u01CD ?\u01CE) ;; LATIN CAPITAL LETTER A WITH CARON
	  (?\u01CF ?\u01D0) ;; LATIN CAPITAL LETTER I WITH CARON
	  (?\u01D1 ?\u01D2) ;; LATIN CAPITAL LETTER O WITH CARON
	  (?\u01D3 ?\u01D4) ;; LATIN CAPITAL LETTER U WITH CARON
	  (?\u01D5 ?\u01D6) ;; LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
	  (?\u01D7 ?\u01D8) ;; LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
	  (?\u01D9 ?\u01DA) ;; LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
	  (?\u01DB ?\u01DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
	  (?\u01DE ?\u01DF) ;; LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
	  (?\u01E0 ?\u01E1) ;; LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
	  (?\u01E2 ?\u01E3) ;; LATIN CAPITAL LETTER AE WITH MACRON
	  (?\u01E4 ?\u01E5) ;; LATIN CAPITAL LETTER G WITH STROKE
	  (?\u01E6 ?\u01E7) ;; LATIN CAPITAL LETTER G WITH CARON
	  (?\u01E8 ?\u01E9) ;; LATIN CAPITAL LETTER K WITH CARON
	  (?\u01EA ?\u01EB) ;; LATIN CAPITAL LETTER O WITH OGONEK
	  (?\u01EC ?\u01ED) ;; LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
	  (?\u01EE ?\u01EF) ;; LATIN CAPITAL LETTER EZH WITH CARON
;;; WARNING: Unhandled full mapping:
;;;   01F0; F; 006A 030C; # LATIN SMALL LETTER J WITH CARON
	  (?\u01F1 ?\u01F3) ;; LATIN CAPITAL LETTER DZ
	  (?\u01F2 ?\u01F3) ;; LATIN CAPITAL LETTER D WITH SMALL LETTER Z
	  (?\u01F4 ?\u01F5) ;; LATIN CAPITAL LETTER G WITH ACUTE
	  (?\u01F6 ?\u0195) ;; LATIN CAPITAL LETTER HWAIR
	  (?\u01F7 ?\u01BF) ;; LATIN CAPITAL LETTER WYNN
	  (?\u01F8 ?\u01F9) ;; LATIN CAPITAL LETTER N WITH GRAVE
	  (?\u01FA ?\u01FB) ;; LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
	  (?\u01FC ?\u01FD) ;; LATIN CAPITAL LETTER AE WITH ACUTE
	  (?\u01FE ?\u01FF) ;; LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
	  (?\u0200 ?\u0201) ;; LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
	  (?\u0202 ?\u0203) ;; LATIN CAPITAL LETTER A WITH INVERTED BREVE
	  (?\u0204 ?\u0205) ;; LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
	  (?\u0206 ?\u0207) ;; LATIN CAPITAL LETTER E WITH INVERTED BREVE
	  (?\u0208 ?\u0209) ;; LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
	  (?\u020A ?\u020B) ;; LATIN CAPITAL LETTER I WITH INVERTED BREVE
	  (?\u020C ?\u020D) ;; LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
	  (?\u020E ?\u020F) ;; LATIN CAPITAL LETTER O WITH INVERTED BREVE
	  (?\u0210 ?\u0211) ;; LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
	  (?\u0212 ?\u0213) ;; LATIN CAPITAL LETTER R WITH INVERTED BREVE
	  (?\u0214 ?\u0215) ;; LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
	  (?\u0216 ?\u0217) ;; LATIN CAPITAL LETTER U WITH INVERTED BREVE
	  (?\u0218 ?\u0219) ;; LATIN CAPITAL LETTER S WITH COMMA BELOW
	  (?\u021A ?\u021B) ;; LATIN CAPITAL LETTER T WITH COMMA BELOW
	  (?\u021C ?\u021D) ;; LATIN CAPITAL LETTER YOGH
	  (?\u021E ?\u021F) ;; LATIN CAPITAL LETTER H WITH CARON
	  (?\u0220 ?\u019E) ;; LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
	  (?\u0222 ?\u0223) ;; LATIN CAPITAL LETTER OU
	  (?\u0224 ?\u0225) ;; LATIN CAPITAL LETTER Z WITH HOOK
	  (?\u0226 ?\u0227) ;; LATIN CAPITAL LETTER A WITH DOT ABOVE
	  (?\u0228 ?\u0229) ;; LATIN CAPITAL LETTER E WITH CEDILLA
	  (?\u022A ?\u022B) ;; LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
	  (?\u022C ?\u022D) ;; LATIN CAPITAL LETTER O WITH TILDE AND MACRON
	  (?\u022E ?\u022F) ;; LATIN CAPITAL LETTER O WITH DOT ABOVE
	  (?\u0230 ?\u0231) ;; LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
	  (?\u0232 ?\u0233) ;; LATIN CAPITAL LETTER Y WITH MACRON
	  (?\u023A ?\u2C65) ;; LATIN CAPITAL LETTER A WITH STROKE
	  (?\u023B ?\u023C) ;; LATIN CAPITAL LETTER C WITH STROKE
	  (?\u023D ?\u019A) ;; LATIN CAPITAL LETTER L WITH BAR
	  (?\u023E ?\u2C66) ;; LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
	  (?\u0241 ?\u0242) ;; LATIN CAPITAL LETTER GLOTTAL STOP
	  (?\u0243 ?\u0180) ;; LATIN CAPITAL LETTER B WITH STROKE
	  (?\u0244 ?\u0289) ;; LATIN CAPITAL LETTER U BAR
	  (?\u0245 ?\u028C) ;; LATIN CAPITAL LETTER TURNED V
	  (?\u0246 ?\u0247) ;; LATIN CAPITAL LETTER E WITH STROKE
	  (?\u0248 ?\u0249) ;; LATIN CAPITAL LETTER J WITH STROKE
	  (?\u024A ?\u024B) ;; LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
	  (?\u024C ?\u024D) ;; LATIN CAPITAL LETTER R WITH STROKE
	  (?\u024E ?\u024F) ;; LATIN CAPITAL LETTER Y WITH STROKE
	  (?\u0345 ?\u03B9) ;; COMBINING GREEK YPOGEGRAMMENI
	  (?\u0370 ?\u0371) ;; GREEK CAPITAL LETTER HETA
	  (?\u0372 ?\u0373) ;; GREEK CAPITAL LETTER ARCHAIC SAMPI
	  (?\u0376 ?\u0377) ;; GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
	  (?\u0386 ?\u03AC) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
	  (?\u0388 ?\u03AD) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
	  (?\u0389 ?\u03AE) ;; GREEK CAPITAL LETTER ETA WITH TONOS
	  (?\u038A ?\u03AF) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
	  (?\u038C ?\u03CC) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
	  (?\u038E ?\u03CD) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
	  (?\u038F ?\u03CE) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
;;; WARNING: Unhandled full mapping:
;;;   0390; F; 03B9 0308 0301; # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
	  (?\u0391 ?\u03B1) ;; GREEK CAPITAL LETTER ALPHA
	  (?\u0392 ?\u03B2) ;; GREEK CAPITAL LETTER BETA
	  (?\u0393 ?\u03B3) ;; GREEK CAPITAL LETTER GAMMA
	  (?\u0394 ?\u03B4) ;; GREEK CAPITAL LETTER DELTA
	  (?\u0395 ?\u03B5) ;; GREEK CAPITAL LETTER EPSILON
	  (?\u0396 ?\u03B6) ;; GREEK CAPITAL LETTER ZETA
	  (?\u0397 ?\u03B7) ;; GREEK CAPITAL LETTER ETA
	  (?\u0398 ?\u03B8) ;; GREEK CAPITAL LETTER THETA
	  (?\u0399 ?\u03B9) ;; GREEK CAPITAL LETTER IOTA
	  (?\u039A ?\u03BA) ;; GREEK CAPITAL LETTER KAPPA
	  (?\u039B ?\u03BB) ;; GREEK CAPITAL LETTER LAMDA
	  (?\u039C ?\u03BC) ;; GREEK CAPITAL LETTER MU
	  (?\u039D ?\u03BD) ;; GREEK CAPITAL LETTER NU
	  (?\u039E ?\u03BE) ;; GREEK CAPITAL LETTER XI
	  (?\u039F ?\u03BF) ;; GREEK CAPITAL LETTER OMICRON
	  (?\u03A0 ?\u03C0) ;; GREEK CAPITAL LETTER PI
	  (?\u03A1 ?\u03C1) ;; GREEK CAPITAL LETTER RHO
	  (?\u03A3 ?\u03C3) ;; GREEK CAPITAL LETTER SIGMA
	  (?\u03A4 ?\u03C4) ;; GREEK CAPITAL LETTER TAU
	  (?\u03A5 ?\u03C5) ;; GREEK CAPITAL LETTER UPSILON
	  (?\u03A6 ?\u03C6) ;; GREEK CAPITAL LETTER PHI
	  (?\u03A7 ?\u03C7) ;; GREEK CAPITAL LETTER CHI
	  (?\u03A8 ?\u03C8) ;; GREEK CAPITAL LETTER PSI
	  (?\u03A9 ?\u03C9) ;; GREEK CAPITAL LETTER OMEGA
	  (?\u03AA ?\u03CA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
	  (?\u03AB ?\u03CB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
;;; WARNING: Unhandled full mapping:
;;;   03B0; F; 03C5 0308 0301; # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
	  (?\u03C2 ?\u03C3) ;; GREEK SMALL LETTER FINAL SIGMA
	  (?\u03CF ?\u03D7) ;; GREEK CAPITAL KAI SYMBOL
	  (?\u03D0 ?\u03B2) ;; GREEK BETA SYMBOL
	  (?\u03D1 ?\u03B8) ;; GREEK THETA SYMBOL
	  (?\u03D5 ?\u03C6) ;; GREEK PHI SYMBOL
	  (?\u03D6 ?\u03C0) ;; GREEK PI SYMBOL
	  (?\u03D8 ?\u03D9) ;; GREEK LETTER ARCHAIC KOPPA
	  (?\u03DA ?\u03DB) ;; GREEK LETTER STIGMA
	  (?\u03DC ?\u03DD) ;; GREEK LETTER DIGAMMA
	  (?\u03DE ?\u03DF) ;; GREEK LETTER KOPPA
	  (?\u03E0 ?\u03E1) ;; GREEK LETTER SAMPI
	  (?\u03E2 ?\u03E3) ;; COPTIC CAPITAL LETTER SHEI
	  (?\u03E4 ?\u03E5) ;; COPTIC CAPITAL LETTER FEI
	  (?\u03E6 ?\u03E7) ;; COPTIC CAPITAL LETTER KHEI
	  (?\u03E8 ?\u03E9) ;; COPTIC CAPITAL LETTER HORI
	  (?\u03EA ?\u03EB) ;; COPTIC CAPITAL LETTER GANGIA
	  (?\u03EC ?\u03ED) ;; COPTIC CAPITAL LETTER SHIMA
	  (?\u03EE ?\u03EF) ;; COPTIC CAPITAL LETTER DEI
	  (?\u03F0 ?\u03BA) ;; GREEK KAPPA SYMBOL
	  (?\u03F1 ?\u03C1) ;; GREEK RHO SYMBOL
	  (?\u03F4 ?\u03B8) ;; GREEK CAPITAL THETA SYMBOL
	  (?\u03F5 ?\u03B5) ;; GREEK LUNATE EPSILON SYMBOL
	  (?\u03F7 ?\u03F8) ;; GREEK CAPITAL LETTER SHO
	  (?\u03F9 ?\u03F2) ;; GREEK CAPITAL LUNATE SIGMA SYMBOL
	  (?\u03FA ?\u03FB) ;; GREEK CAPITAL LETTER SAN
	  (?\u03FD ?\u037B) ;; GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
	  (?\u03FE ?\u037C) ;; GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
	  (?\u03FF ?\u037D) ;; GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
	  (?\u0400 ?\u0450) ;; CYRILLIC CAPITAL LETTER IE WITH GRAVE
	  (?\u0401 ?\u0451) ;; CYRILLIC CAPITAL LETTER IO
	  (?\u0402 ?\u0452) ;; CYRILLIC CAPITAL LETTER DJE
	  (?\u0403 ?\u0453) ;; CYRILLIC CAPITAL LETTER GJE
	  (?\u0404 ?\u0454) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
	  (?\u0405 ?\u0455) ;; CYRILLIC CAPITAL LETTER DZE
	  (?\u0406 ?\u0456) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
	  (?\u0407 ?\u0457) ;; CYRILLIC CAPITAL LETTER YI
	  (?\u0408 ?\u0458) ;; CYRILLIC CAPITAL LETTER JE
	  (?\u0409 ?\u0459) ;; CYRILLIC CAPITAL LETTER LJE
	  (?\u040A ?\u045A) ;; CYRILLIC CAPITAL LETTER NJE
	  (?\u040B ?\u045B) ;; CYRILLIC CAPITAL LETTER TSHE
	  (?\u040C ?\u045C) ;; CYRILLIC CAPITAL LETTER KJE
	  (?\u040D ?\u045D) ;; CYRILLIC CAPITAL LETTER I WITH GRAVE
	  (?\u040E ?\u045E) ;; CYRILLIC CAPITAL LETTER SHORT U
	  (?\u040F ?\u045F) ;; CYRILLIC CAPITAL LETTER DZHE
	  (?\u0410 ?\u0430) ;; CYRILLIC CAPITAL LETTER A
	  (?\u0411 ?\u0431) ;; CYRILLIC CAPITAL LETTER BE
	  (?\u0412 ?\u0432) ;; CYRILLIC CAPITAL LETTER VE
	  (?\u0413 ?\u0433) ;; CYRILLIC CAPITAL LETTER GHE
	  (?\u0414 ?\u0434) ;; CYRILLIC CAPITAL LETTER DE
	  (?\u0415 ?\u0435) ;; CYRILLIC CAPITAL LETTER IE
	  (?\u0416 ?\u0436) ;; CYRILLIC CAPITAL LETTER ZHE
	  (?\u0417 ?\u0437) ;; CYRILLIC CAPITAL LETTER ZE
	  (?\u0418 ?\u0438) ;; CYRILLIC CAPITAL LETTER I
	  (?\u0419 ?\u0439) ;; CYRILLIC CAPITAL LETTER SHORT I
	  (?\u041A ?\u043A) ;; CYRILLIC CAPITAL LETTER KA
	  (?\u041B ?\u043B) ;; CYRILLIC CAPITAL LETTER EL
	  (?\u041C ?\u043C) ;; CYRILLIC CAPITAL LETTER EM
	  (?\u041D ?\u043D) ;; CYRILLIC CAPITAL LETTER EN
	  (?\u041E ?\u043E) ;; CYRILLIC CAPITAL LETTER O
	  (?\u041F ?\u043F) ;; CYRILLIC CAPITAL LETTER PE
	  (?\u0420 ?\u0440) ;; CYRILLIC CAPITAL LETTER ER
	  (?\u0421 ?\u0441) ;; CYRILLIC CAPITAL LETTER ES
	  (?\u0422 ?\u0442) ;; CYRILLIC CAPITAL LETTER TE
	  (?\u0423 ?\u0443) ;; CYRILLIC CAPITAL LETTER U
	  (?\u0424 ?\u0444) ;; CYRILLIC CAPITAL LETTER EF
	  (?\u0425 ?\u0445) ;; CYRILLIC CAPITAL LETTER HA
	  (?\u0426 ?\u0446) ;; CYRILLIC CAPITAL LETTER TSE
	  (?\u0427 ?\u0447) ;; CYRILLIC CAPITAL LETTER CHE
	  (?\u0428 ?\u0448) ;; CYRILLIC CAPITAL LETTER SHA
	  (?\u0429 ?\u0449) ;; CYRILLIC CAPITAL LETTER SHCHA
	  (?\u042A ?\u044A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
	  (?\u042B ?\u044B) ;; CYRILLIC CAPITAL LETTER YERU
	  (?\u042C ?\u044C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
	  (?\u042D ?\u044D) ;; CYRILLIC CAPITAL LETTER E
	  (?\u042E ?\u044E) ;; CYRILLIC CAPITAL LETTER YU
	  (?\u042F ?\u044F) ;; CYRILLIC CAPITAL LETTER YA
	  (?\u0460 ?\u0461) ;; CYRILLIC CAPITAL LETTER OMEGA
	  (?\u0462 ?\u0463) ;; CYRILLIC CAPITAL LETTER YAT
	  (?\u0464 ?\u0465) ;; CYRILLIC CAPITAL LETTER IOTIFIED E
	  (?\u0466 ?\u0467) ;; CYRILLIC CAPITAL LETTER LITTLE YUS
	  (?\u0468 ?\u0469) ;; CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
	  (?\u046A ?\u046B) ;; CYRILLIC CAPITAL LETTER BIG YUS
	  (?\u046C ?\u046D) ;; CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
	  (?\u046E ?\u046F) ;; CYRILLIC CAPITAL LETTER KSI
	  (?\u0470 ?\u0471) ;; CYRILLIC CAPITAL LETTER PSI
	  (?\u0472 ?\u0473) ;; CYRILLIC CAPITAL LETTER FITA
	  (?\u0474 ?\u0475) ;; CYRILLIC CAPITAL LETTER IZHITSA
	  (?\u0476 ?\u0477) ;; CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
	  (?\u0478 ?\u0479) ;; CYRILLIC CAPITAL LETTER UK
	  (?\u047A ?\u047B) ;; CYRILLIC CAPITAL LETTER ROUND OMEGA
	  (?\u047C ?\u047D) ;; CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
	  (?\u047E ?\u047F) ;; CYRILLIC CAPITAL LETTER OT
	  (?\u0480 ?\u0481) ;; CYRILLIC CAPITAL LETTER KOPPA
	  (?\u048A ?\u048B) ;; CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
	  (?\u048C ?\u048D) ;; CYRILLIC CAPITAL LETTER SEMISOFT SIGN
	  (?\u048E ?\u048F) ;; CYRILLIC CAPITAL LETTER ER WITH TICK
	  (?\u0490 ?\u0491) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
	  (?\u0492 ?\u0493) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE
	  (?\u0494 ?\u0495) ;; CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
	  (?\u0496 ?\u0497) ;; CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
	  (?\u0498 ?\u0499) ;; CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
	  (?\u049A ?\u049B) ;; CYRILLIC CAPITAL LETTER KA WITH DESCENDER
	  (?\u049C ?\u049D) ;; CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
	  (?\u049E ?\u049F) ;; CYRILLIC CAPITAL LETTER KA WITH STROKE
	  (?\u04A0 ?\u04A1) ;; CYRILLIC CAPITAL LETTER BASHKIR KA
	  (?\u04A2 ?\u04A3) ;; CYRILLIC CAPITAL LETTER EN WITH DESCENDER
	  (?\u04A4 ?\u04A5) ;; CYRILLIC CAPITAL LIGATURE EN GHE
	  (?\u04A6 ?\u04A7) ;; CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
	  (?\u04A8 ?\u04A9) ;; CYRILLIC CAPITAL LETTER ABKHASIAN HA
	  (?\u04AA ?\u04AB) ;; CYRILLIC CAPITAL LETTER ES WITH DESCENDER
	  (?\u04AC ?\u04AD) ;; CYRILLIC CAPITAL LETTER TE WITH DESCENDER
	  (?\u04AE ?\u04AF) ;; CYRILLIC CAPITAL LETTER STRAIGHT U
	  (?\u04B0 ?\u04B1) ;; CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
	  (?\u04B2 ?\u04B3) ;; CYRILLIC CAPITAL LETTER HA WITH DESCENDER
	  (?\u04B4 ?\u04B5) ;; CYRILLIC CAPITAL LIGATURE TE TSE
	  (?\u04B6 ?\u04B7) ;; CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
	  (?\u04B8 ?\u04B9) ;; CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
	  (?\u04BA ?\u04BB) ;; CYRILLIC CAPITAL LETTER SHHA
	  (?\u04BC ?\u04BD) ;; CYRILLIC CAPITAL LETTER ABKHASIAN CHE
	  (?\u04BE ?\u04BF) ;; CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
	  (?\u04C0 ?\u04CF) ;; CYRILLIC LETTER PALOCHKA
	  (?\u04C1 ?\u04C2) ;; CYRILLIC CAPITAL LETTER ZHE WITH BREVE
	  (?\u04C3 ?\u04C4) ;; CYRILLIC CAPITAL LETTER KA WITH HOOK
	  (?\u04C5 ?\u04C6) ;; CYRILLIC CAPITAL LETTER EL WITH TAIL
	  (?\u04C7 ?\u04C8) ;; CYRILLIC CAPITAL LETTER EN WITH HOOK
	  (?\u04C9 ?\u04CA) ;; CYRILLIC CAPITAL LETTER EN WITH TAIL
	  (?\u04CB ?\u04CC) ;; CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
	  (?\u04CD ?\u04CE) ;; CYRILLIC CAPITAL LETTER EM WITH TAIL
	  (?\u04D0 ?\u04D1) ;; CYRILLIC CAPITAL LETTER A WITH BREVE
	  (?\u04D2 ?\u04D3) ;; CYRILLIC CAPITAL LETTER A WITH DIAERESIS
	  (?\u04D4 ?\u04D5) ;; CYRILLIC CAPITAL LIGATURE A IE
	  (?\u04D6 ?\u04D7) ;; CYRILLIC CAPITAL LETTER IE WITH BREVE
	  (?\u04D8 ?\u04D9) ;; CYRILLIC CAPITAL LETTER SCHWA
	  (?\u04DA ?\u04DB) ;; CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
	  (?\u04DC ?\u04DD) ;; CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
	  (?\u04DE ?\u04DF) ;; CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
	  (?\u04E0 ?\u04E1) ;; CYRILLIC CAPITAL LETTER ABKHASIAN DZE
	  (?\u04E2 ?\u04E3) ;; CYRILLIC CAPITAL LETTER I WITH MACRON
	  (?\u04E4 ?\u04E5) ;; CYRILLIC CAPITAL LETTER I WITH DIAERESIS
	  (?\u04E6 ?\u04E7) ;; CYRILLIC CAPITAL LETTER O WITH DIAERESIS
	  (?\u04E8 ?\u04E9) ;; CYRILLIC CAPITAL LETTER BARRED O
	  (?\u04EA ?\u04EB) ;; CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
	  (?\u04EC ?\u04ED) ;; CYRILLIC CAPITAL LETTER E WITH DIAERESIS
	  (?\u04EE ?\u04EF) ;; CYRILLIC CAPITAL LETTER U WITH MACRON
	  (?\u04F0 ?\u04F1) ;; CYRILLIC CAPITAL LETTER U WITH DIAERESIS
	  (?\u04F2 ?\u04F3) ;; CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
	  (?\u04F4 ?\u04F5) ;; CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
	  (?\u04F6 ?\u04F7) ;; CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
	  (?\u04F8 ?\u04F9) ;; CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
	  (?\u04FA ?\u04FB) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
	  (?\u04FC ?\u04FD) ;; CYRILLIC CAPITAL LETTER HA WITH HOOK
	  (?\u04FE ?\u04FF) ;; CYRILLIC CAPITAL LETTER HA WITH STROKE
	  (?\u0500 ?\u0501) ;; CYRILLIC CAPITAL LETTER KOMI DE
	  (?\u0502 ?\u0503) ;; CYRILLIC CAPITAL LETTER KOMI DJE
	  (?\u0504 ?\u0505) ;; CYRILLIC CAPITAL LETTER KOMI ZJE
	  (?\u0506 ?\u0507) ;; CYRILLIC CAPITAL LETTER KOMI DZJE
	  (?\u0508 ?\u0509) ;; CYRILLIC CAPITAL LETTER KOMI LJE
	  (?\u050A ?\u050B) ;; CYRILLIC CAPITAL LETTER KOMI NJE
	  (?\u050C ?\u050D) ;; CYRILLIC CAPITAL LETTER KOMI SJE
	  (?\u050E ?\u050F) ;; CYRILLIC CAPITAL LETTER KOMI TJE
	  (?\u0510 ?\u0511) ;; CYRILLIC CAPITAL LETTER REVERSED ZE
	  (?\u0512 ?\u0513) ;; CYRILLIC CAPITAL LETTER EL WITH HOOK
	  (?\u0514 ?\u0515) ;; CYRILLIC CAPITAL LETTER LHA
	  (?\u0516 ?\u0517) ;; CYRILLIC CAPITAL LETTER RHA
	  (?\u0518 ?\u0519) ;; CYRILLIC CAPITAL LETTER YAE
	  (?\u051A ?\u051B) ;; CYRILLIC CAPITAL LETTER QA
	  (?\u051C ?\u051D) ;; CYRILLIC CAPITAL LETTER WE
	  (?\u051E ?\u051F) ;; CYRILLIC CAPITAL LETTER ALEUT KA
	  (?\u0520 ?\u0521) ;; CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
	  (?\u0522 ?\u0523) ;; CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
	  (?\u0524 ?\u0525) ;; CYRILLIC CAPITAL LETTER PE WITH DESCENDER
	  (?\u0531 ?\u0561) ;; ARMENIAN CAPITAL LETTER AYB
	  (?\u0532 ?\u0562) ;; ARMENIAN CAPITAL LETTER BEN
	  (?\u0533 ?\u0563) ;; ARMENIAN CAPITAL LETTER GIM
	  (?\u0534 ?\u0564) ;; ARMENIAN CAPITAL LETTER DA
	  (?\u0535 ?\u0565) ;; ARMENIAN CAPITAL LETTER ECH
	  (?\u0536 ?\u0566) ;; ARMENIAN CAPITAL LETTER ZA
	  (?\u0537 ?\u0567) ;; ARMENIAN CAPITAL LETTER EH
	  (?\u0538 ?\u0568) ;; ARMENIAN CAPITAL LETTER ET
	  (?\u0539 ?\u0569) ;; ARMENIAN CAPITAL LETTER TO
	  (?\u053A ?\u056A) ;; ARMENIAN CAPITAL LETTER ZHE
	  (?\u053B ?\u056B) ;; ARMENIAN CAPITAL LETTER INI
	  (?\u053C ?\u056C) ;; ARMENIAN CAPITAL LETTER LIWN
	  (?\u053D ?\u056D) ;; ARMENIAN CAPITAL LETTER XEH
	  (?\u053E ?\u056E) ;; ARMENIAN CAPITAL LETTER CA
	  (?\u053F ?\u056F) ;; ARMENIAN CAPITAL LETTER KEN
	  (?\u0540 ?\u0570) ;; ARMENIAN CAPITAL LETTER HO
	  (?\u0541 ?\u0571) ;; ARMENIAN CAPITAL LETTER JA
	  (?\u0542 ?\u0572) ;; ARMENIAN CAPITAL LETTER GHAD
	  (?\u0543 ?\u0573) ;; ARMENIAN CAPITAL LETTER CHEH
	  (?\u0544 ?\u0574) ;; ARMENIAN CAPITAL LETTER MEN
	  (?\u0545 ?\u0575) ;; ARMENIAN CAPITAL LETTER YI
	  (?\u0546 ?\u0576) ;; ARMENIAN CAPITAL LETTER NOW
	  (?\u0547 ?\u0577) ;; ARMENIAN CAPITAL LETTER SHA
	  (?\u0548 ?\u0578) ;; ARMENIAN CAPITAL LETTER VO
	  (?\u0549 ?\u0579) ;; ARMENIAN CAPITAL LETTER CHA
	  (?\u054A ?\u057A) ;; ARMENIAN CAPITAL LETTER PEH
	  (?\u054B ?\u057B) ;; ARMENIAN CAPITAL LETTER JHEH
	  (?\u054C ?\u057C) ;; ARMENIAN CAPITAL LETTER RA
	  (?\u054D ?\u057D) ;; ARMENIAN CAPITAL LETTER SEH
	  (?\u054E ?\u057E) ;; ARMENIAN CAPITAL LETTER VEW
	  (?\u054F ?\u057F) ;; ARMENIAN CAPITAL LETTER TIWN
	  (?\u0550 ?\u0580) ;; ARMENIAN CAPITAL LETTER REH
	  (?\u0551 ?\u0581) ;; ARMENIAN CAPITAL LETTER CO
	  (?\u0552 ?\u0582) ;; ARMENIAN CAPITAL LETTER YIWN
	  (?\u0553 ?\u0583) ;; ARMENIAN CAPITAL LETTER PIWR
	  (?\u0554 ?\u0584) ;; ARMENIAN CAPITAL LETTER KEH
	  (?\u0555 ?\u0585) ;; ARMENIAN CAPITAL LETTER OH
	  (?\u0556 ?\u0586) ;; ARMENIAN CAPITAL LETTER FEH
;;; WARNING: Unhandled full mapping:
;;;   0587; F; 0565 0582; # ARMENIAN SMALL LIGATURE ECH YIWN
	  (?\u10A0 ?\u2D00) ;; GEORGIAN CAPITAL LETTER AN
	  (?\u10A1 ?\u2D01) ;; GEORGIAN CAPITAL LETTER BAN
	  (?\u10A2 ?\u2D02) ;; GEORGIAN CAPITAL LETTER GAN
	  (?\u10A3 ?\u2D03) ;; GEORGIAN CAPITAL LETTER DON
	  (?\u10A4 ?\u2D04) ;; GEORGIAN CAPITAL LETTER EN
	  (?\u10A5 ?\u2D05) ;; GEORGIAN CAPITAL LETTER VIN
	  (?\u10A6 ?\u2D06) ;; GEORGIAN CAPITAL LETTER ZEN
	  (?\u10A7 ?\u2D07) ;; GEORGIAN CAPITAL LETTER TAN
	  (?\u10A8 ?\u2D08) ;; GEORGIAN CAPITAL LETTER IN
	  (?\u10A9 ?\u2D09) ;; GEORGIAN CAPITAL LETTER KAN
	  (?\u10AA ?\u2D0A) ;; GEORGIAN CAPITAL LETTER LAS
	  (?\u10AB ?\u2D0B) ;; GEORGIAN CAPITAL LETTER MAN
	  (?\u10AC ?\u2D0C) ;; GEORGIAN CAPITAL LETTER NAR
	  (?\u10AD ?\u2D0D) ;; GEORGIAN CAPITAL LETTER ON
	  (?\u10AE ?\u2D0E) ;; GEORGIAN CAPITAL LETTER PAR
	  (?\u10AF ?\u2D0F) ;; GEORGIAN CAPITAL LETTER ZHAR
	  (?\u10B0 ?\u2D10) ;; GEORGIAN CAPITAL LETTER RAE
	  (?\u10B1 ?\u2D11) ;; GEORGIAN CAPITAL LETTER SAN
	  (?\u10B2 ?\u2D12) ;; GEORGIAN CAPITAL LETTER TAR
	  (?\u10B3 ?\u2D13) ;; GEORGIAN CAPITAL LETTER UN
	  (?\u10B4 ?\u2D14) ;; GEORGIAN CAPITAL LETTER PHAR
	  (?\u10B5 ?\u2D15) ;; GEORGIAN CAPITAL LETTER KHAR
	  (?\u10B6 ?\u2D16) ;; GEORGIAN CAPITAL LETTER GHAN
	  (?\u10B7 ?\u2D17) ;; GEORGIAN CAPITAL LETTER QAR
	  (?\u10B8 ?\u2D18) ;; GEORGIAN CAPITAL LETTER SHIN
	  (?\u10B9 ?\u2D19) ;; GEORGIAN CAPITAL LETTER CHIN
	  (?\u10BA ?\u2D1A) ;; GEORGIAN CAPITAL LETTER CAN
	  (?\u10BB ?\u2D1B) ;; GEORGIAN CAPITAL LETTER JIL
	  (?\u10BC ?\u2D1C) ;; GEORGIAN CAPITAL LETTER CIL
	  (?\u10BD ?\u2D1D) ;; GEORGIAN CAPITAL LETTER CHAR
	  (?\u10BE ?\u2D1E) ;; GEORGIAN CAPITAL LETTER XAN
	  (?\u10BF ?\u2D1F) ;; GEORGIAN CAPITAL LETTER JHAN
	  (?\u10C0 ?\u2D20) ;; GEORGIAN CAPITAL LETTER HAE
	  (?\u10C1 ?\u2D21) ;; GEORGIAN CAPITAL LETTER HE
	  (?\u10C2 ?\u2D22) ;; GEORGIAN CAPITAL LETTER HIE
	  (?\u10C3 ?\u2D23) ;; GEORGIAN CAPITAL LETTER WE
	  (?\u10C4 ?\u2D24) ;; GEORGIAN CAPITAL LETTER HAR
	  (?\u10C5 ?\u2D25) ;; GEORGIAN CAPITAL LETTER HOE
	  (?\u1E00 ?\u1E01) ;; LATIN CAPITAL LETTER A WITH RING BELOW
	  (?\u1E02 ?\u1E03) ;; LATIN CAPITAL LETTER B WITH DOT ABOVE
	  (?\u1E04 ?\u1E05) ;; LATIN CAPITAL LETTER B WITH DOT BELOW
	  (?\u1E06 ?\u1E07) ;; LATIN CAPITAL LETTER B WITH LINE BELOW
	  (?\u1E08 ?\u1E09) ;; LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
	  (?\u1E0A ?\u1E0B) ;; LATIN CAPITAL LETTER D WITH DOT ABOVE
	  (?\u1E0C ?\u1E0D) ;; LATIN CAPITAL LETTER D WITH DOT BELOW
	  (?\u1E0E ?\u1E0F) ;; LATIN CAPITAL LETTER D WITH LINE BELOW
	  (?\u1E10 ?\u1E11) ;; LATIN CAPITAL LETTER D WITH CEDILLA
	  (?\u1E12 ?\u1E13) ;; LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
	  (?\u1E14 ?\u1E15) ;; LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
	  (?\u1E16 ?\u1E17) ;; LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
	  (?\u1E18 ?\u1E19) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
	  (?\u1E1A ?\u1E1B) ;; LATIN CAPITAL LETTER E WITH TILDE BELOW
	  (?\u1E1C ?\u1E1D) ;; LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
	  (?\u1E1E ?\u1E1F) ;; LATIN CAPITAL LETTER F WITH DOT ABOVE
	  (?\u1E20 ?\u1E21) ;; LATIN CAPITAL LETTER G WITH MACRON
	  (?\u1E22 ?\u1E23) ;; LATIN CAPITAL LETTER H WITH DOT ABOVE
	  (?\u1E24 ?\u1E25) ;; LATIN CAPITAL LETTER H WITH DOT BELOW
	  (?\u1E26 ?\u1E27) ;; LATIN CAPITAL LETTER H WITH DIAERESIS
	  (?\u1E28 ?\u1E29) ;; LATIN CAPITAL LETTER H WITH CEDILLA
	  (?\u1E2A ?\u1E2B) ;; LATIN CAPITAL LETTER H WITH BREVE BELOW
	  (?\u1E2C ?\u1E2D) ;; LATIN CAPITAL LETTER I WITH TILDE BELOW
	  (?\u1E2E ?\u1E2F) ;; LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
	  (?\u1E30 ?\u1E31) ;; LATIN CAPITAL LETTER K WITH ACUTE
	  (?\u1E32 ?\u1E33) ;; LATIN CAPITAL LETTER K WITH DOT BELOW
	  (?\u1E34 ?\u1E35) ;; LATIN CAPITAL LETTER K WITH LINE BELOW
	  (?\u1E36 ?\u1E37) ;; LATIN CAPITAL LETTER L WITH DOT BELOW
	  (?\u1E38 ?\u1E39) ;; LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
	  (?\u1E3A ?\u1E3B) ;; LATIN CAPITAL LETTER L WITH LINE BELOW
	  (?\u1E3C ?\u1E3D) ;; LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
	  (?\u1E3E ?\u1E3F) ;; LATIN CAPITAL LETTER M WITH ACUTE
	  (?\u1E40 ?\u1E41) ;; LATIN CAPITAL LETTER M WITH DOT ABOVE
	  (?\u1E42 ?\u1E43) ;; LATIN CAPITAL LETTER M WITH DOT BELOW
	  (?\u1E44 ?\u1E45) ;; LATIN CAPITAL LETTER N WITH DOT ABOVE
	  (?\u1E46 ?\u1E47) ;; LATIN CAPITAL LETTER N WITH DOT BELOW
	  (?\u1E48 ?\u1E49) ;; LATIN CAPITAL LETTER N WITH LINE BELOW
	  (?\u1E4A ?\u1E4B) ;; LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
	  (?\u1E4C ?\u1E4D) ;; LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
	  (?\u1E4E ?\u1E4F) ;; LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
	  (?\u1E50 ?\u1E51) ;; LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
	  (?\u1E52 ?\u1E53) ;; LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
	  (?\u1E54 ?\u1E55) ;; LATIN CAPITAL LETTER P WITH ACUTE
	  (?\u1E56 ?\u1E57) ;; LATIN CAPITAL LETTER P WITH DOT ABOVE
	  (?\u1E58 ?\u1E59) ;; LATIN CAPITAL LETTER R WITH DOT ABOVE
	  (?\u1E5A ?\u1E5B) ;; LATIN CAPITAL LETTER R WITH DOT BELOW
	  (?\u1E5C ?\u1E5D) ;; LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
	  (?\u1E5E ?\u1E5F) ;; LATIN CAPITAL LETTER R WITH LINE BELOW
	  (?\u1E60 ?\u1E61) ;; LATIN CAPITAL LETTER S WITH DOT ABOVE
	  (?\u1E62 ?\u1E63) ;; LATIN CAPITAL LETTER S WITH DOT BELOW
	  (?\u1E64 ?\u1E65) ;; LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
	  (?\u1E66 ?\u1E67) ;; LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
	  (?\u1E68 ?\u1E69) ;; LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
	  (?\u1E6A ?\u1E6B) ;; LATIN CAPITAL LETTER T WITH DOT ABOVE
	  (?\u1E6C ?\u1E6D) ;; LATIN CAPITAL LETTER T WITH DOT BELOW
	  (?\u1E6E ?\u1E6F) ;; LATIN CAPITAL LETTER T WITH LINE BELOW
	  (?\u1E70 ?\u1E71) ;; LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
	  (?\u1E72 ?\u1E73) ;; LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
	  (?\u1E74 ?\u1E75) ;; LATIN CAPITAL LETTER U WITH TILDE BELOW
	  (?\u1E76 ?\u1E77) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
	  (?\u1E78 ?\u1E79) ;; LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
	  (?\u1E7A ?\u1E7B) ;; LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
	  (?\u1E7C ?\u1E7D) ;; LATIN CAPITAL LETTER V WITH TILDE
	  (?\u1E7E ?\u1E7F) ;; LATIN CAPITAL LETTER V WITH DOT BELOW
	  (?\u1E80 ?\u1E81) ;; LATIN CAPITAL LETTER W WITH GRAVE
	  (?\u1E82 ?\u1E83) ;; LATIN CAPITAL LETTER W WITH ACUTE
	  (?\u1E84 ?\u1E85) ;; LATIN CAPITAL LETTER W WITH DIAERESIS
	  (?\u1E86 ?\u1E87) ;; LATIN CAPITAL LETTER W WITH DOT ABOVE
	  (?\u1E88 ?\u1E89) ;; LATIN CAPITAL LETTER W WITH DOT BELOW
	  (?\u1E8A ?\u1E8B) ;; LATIN CAPITAL LETTER X WITH DOT ABOVE
	  (?\u1E8C ?\u1E8D) ;; LATIN CAPITAL LETTER X WITH DIAERESIS
	  (?\u1E8E ?\u1E8F) ;; LATIN CAPITAL LETTER Y WITH DOT ABOVE
	  (?\u1E90 ?\u1E91) ;; LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
	  (?\u1E92 ?\u1E93) ;; LATIN CAPITAL LETTER Z WITH DOT BELOW
	  (?\u1E94 ?\u1E95) ;; LATIN CAPITAL LETTER Z WITH LINE BELOW
;;; WARNING: Unhandled full mapping:
;;;   1E96; F; 0068 0331; # LATIN SMALL LETTER H WITH LINE BELOW
;;; WARNING: Unhandled full mapping:
;;;   1E97; F; 0074 0308; # LATIN SMALL LETTER T WITH DIAERESIS
;;; WARNING: Unhandled full mapping:
;;;   1E98; F; 0077 030A; # LATIN SMALL LETTER W WITH RING ABOVE
;;; WARNING: Unhandled full mapping:
;;;   1E99; F; 0079 030A; # LATIN SMALL LETTER Y WITH RING ABOVE
;;; WARNING: Unhandled full mapping:
;;;   1E9A; F; 0061 02BE; # LATIN SMALL LETTER A WITH RIGHT HALF RING
	  (?\u1E9B ?\u1E61) ;; LATIN SMALL LETTER LONG S WITH DOT ABOVE
;;; WARNING: Unhandled full mapping:
;;;   1E9E; F; 0073 0073; # LATIN CAPITAL LETTER SHARP S
	  (?\u1E9E ?\u00DF) ;; LATIN CAPITAL LETTER SHARP S
	  (?\u1EA0 ?\u1EA1) ;; LATIN CAPITAL LETTER A WITH DOT BELOW
	  (?\u1EA2 ?\u1EA3) ;; LATIN CAPITAL LETTER A WITH HOOK ABOVE
	  (?\u1EA4 ?\u1EA5) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
	  (?\u1EA6 ?\u1EA7) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
	  (?\u1EA8 ?\u1EA9) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
	  (?\u1EAA ?\u1EAB) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
	  (?\u1EAC ?\u1EAD) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
	  (?\u1EAE ?\u1EAF) ;; LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
	  (?\u1EB0 ?\u1EB1) ;; LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
	  (?\u1EB2 ?\u1EB3) ;; LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
	  (?\u1EB4 ?\u1EB5) ;; LATIN CAPITAL LETTER A WITH BREVE AND TILDE
	  (?\u1EB6 ?\u1EB7) ;; LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
	  (?\u1EB8 ?\u1EB9) ;; LATIN CAPITAL LETTER E WITH DOT BELOW
	  (?\u1EBA ?\u1EBB) ;; LATIN CAPITAL LETTER E WITH HOOK ABOVE
	  (?\u1EBC ?\u1EBD) ;; LATIN CAPITAL LETTER E WITH TILDE
	  (?\u1EBE ?\u1EBF) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
	  (?\u1EC0 ?\u1EC1) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
	  (?\u1EC2 ?\u1EC3) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
	  (?\u1EC4 ?\u1EC5) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
	  (?\u1EC6 ?\u1EC7) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
	  (?\u1EC8 ?\u1EC9) ;; LATIN CAPITAL LETTER I WITH HOOK ABOVE
	  (?\u1ECA ?\u1ECB) ;; LATIN CAPITAL LETTER I WITH DOT BELOW
	  (?\u1ECC ?\u1ECD) ;; LATIN CAPITAL LETTER O WITH DOT BELOW
	  (?\u1ECE ?\u1ECF) ;; LATIN CAPITAL LETTER O WITH HOOK ABOVE
	  (?\u1ED0 ?\u1ED1) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
	  (?\u1ED2 ?\u1ED3) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
	  (?\u1ED4 ?\u1ED5) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
	  (?\u1ED6 ?\u1ED7) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
	  (?\u1ED8 ?\u1ED9) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
	  (?\u1EDA ?\u1EDB) ;; LATIN CAPITAL LETTER O WITH HORN AND ACUTE
	  (?\u1EDC ?\u1EDD) ;; LATIN CAPITAL LETTER O WITH HORN AND GRAVE
	  (?\u1EDE ?\u1EDF) ;; LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
	  (?\u1EE0 ?\u1EE1) ;; LATIN CAPITAL LETTER O WITH HORN AND TILDE
	  (?\u1EE2 ?\u1EE3) ;; LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
	  (?\u1EE4 ?\u1EE5) ;; LATIN CAPITAL LETTER U WITH DOT BELOW
	  (?\u1EE6 ?\u1EE7) ;; LATIN CAPITAL LETTER U WITH HOOK ABOVE
	  (?\u1EE8 ?\u1EE9) ;; LATIN CAPITAL LETTER U WITH HORN AND ACUTE
	  (?\u1EEA ?\u1EEB) ;; LATIN CAPITAL LETTER U WITH HORN AND GRAVE
	  (?\u1EEC ?\u1EED) ;; LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
	  (?\u1EEE ?\u1EEF) ;; LATIN CAPITAL LETTER U WITH HORN AND TILDE
	  (?\u1EF0 ?\u1EF1) ;; LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
	  (?\u1EF2 ?\u1EF3) ;; LATIN CAPITAL LETTER Y WITH GRAVE
	  (?\u1EF4 ?\u1EF5) ;; LATIN CAPITAL LETTER Y WITH DOT BELOW
	  (?\u1EF6 ?\u1EF7) ;; LATIN CAPITAL LETTER Y WITH HOOK ABOVE
	  (?\u1EF8 ?\u1EF9) ;; LATIN CAPITAL LETTER Y WITH TILDE
	  (?\u1EFA ?\u1EFB) ;; LATIN CAPITAL LETTER MIDDLE-WELSH LL
	  (?\u1EFC ?\u1EFD) ;; LATIN CAPITAL LETTER MIDDLE-WELSH V
	  (?\u1EFE ?\u1EFF) ;; LATIN CAPITAL LETTER Y WITH LOOP
	  (?\u1F08 ?\u1F00) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI
	  (?\u1F09 ?\u1F01) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA
	  (?\u1F0A ?\u1F02) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
	  (?\u1F0B ?\u1F03) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
	  (?\u1F0C ?\u1F04) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
	  (?\u1F0D ?\u1F05) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
	  (?\u1F0E ?\u1F06) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
	  (?\u1F0F ?\u1F07) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
	  (?\u1F18 ?\u1F10) ;; GREEK CAPITAL LETTER EPSILON WITH PSILI
	  (?\u1F19 ?\u1F11) ;; GREEK CAPITAL LETTER EPSILON WITH DASIA
	  (?\u1F1A ?\u1F12) ;; GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
	  (?\u1F1B ?\u1F13) ;; GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
	  (?\u1F1C ?\u1F14) ;; GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
	  (?\u1F1D ?\u1F15) ;; GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
	  (?\u1F28 ?\u1F20) ;; GREEK CAPITAL LETTER ETA WITH PSILI
	  (?\u1F29 ?\u1F21) ;; GREEK CAPITAL LETTER ETA WITH DASIA
	  (?\u1F2A ?\u1F22) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
	  (?\u1F2B ?\u1F23) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
	  (?\u1F2C ?\u1F24) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
	  (?\u1F2D ?\u1F25) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
	  (?\u1F2E ?\u1F26) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
	  (?\u1F2F ?\u1F27) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
	  (?\u1F38 ?\u1F30) ;; GREEK CAPITAL LETTER IOTA WITH PSILI
	  (?\u1F39 ?\u1F31) ;; GREEK CAPITAL LETTER IOTA WITH DASIA
	  (?\u1F3A ?\u1F32) ;; GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
	  (?\u1F3B ?\u1F33) ;; GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
	  (?\u1F3C ?\u1F34) ;; GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
	  (?\u1F3D ?\u1F35) ;; GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
	  (?\u1F3E ?\u1F36) ;; GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
	  (?\u1F3F ?\u1F37) ;; GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
	  (?\u1F48 ?\u1F40) ;; GREEK CAPITAL LETTER OMICRON WITH PSILI
	  (?\u1F49 ?\u1F41) ;; GREEK CAPITAL LETTER OMICRON WITH DASIA
	  (?\u1F4A ?\u1F42) ;; GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
	  (?\u1F4B ?\u1F43) ;; GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
	  (?\u1F4C ?\u1F44) ;; GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
	  (?\u1F4D ?\u1F45) ;; GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
;;; WARNING: Unhandled full mapping:
;;;   1F50; F; 03C5 0313; # GREEK SMALL LETTER UPSILON WITH PSILI
;;; WARNING: Unhandled full mapping:
;;;   1F52; F; 03C5 0313 0300; # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
;;; WARNING: Unhandled full mapping:
;;;   1F54; F; 03C5 0313 0301; # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
;;; WARNING: Unhandled full mapping:
;;;   1F56; F; 03C5 0313 0342; # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
	  (?\u1F59 ?\u1F51) ;; GREEK CAPITAL LETTER UPSILON WITH DASIA
	  (?\u1F5B ?\u1F53) ;; GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
	  (?\u1F5D ?\u1F55) ;; GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
	  (?\u1F5F ?\u1F57) ;; GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
	  (?\u1F68 ?\u1F60) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI
	  (?\u1F69 ?\u1F61) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA
	  (?\u1F6A ?\u1F62) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
	  (?\u1F6B ?\u1F63) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
	  (?\u1F6C ?\u1F64) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
	  (?\u1F6D ?\u1F65) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
	  (?\u1F6E ?\u1F66) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
	  (?\u1F6F ?\u1F67) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1F80; F; 1F00 03B9; # GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F81; F; 1F01 03B9; # GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F82; F; 1F02 03B9; # GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F83; F; 1F03 03B9; # GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F84; F; 1F04 03B9; # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F85; F; 1F05 03B9; # GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F86; F; 1F06 03B9; # GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F87; F; 1F07 03B9; # GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F88; F; 1F00 03B9; # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
	  (?\u1F88 ?\u1F80) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F89; F; 1F01 03B9; # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
	  (?\u1F89 ?\u1F81) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8A; F; 1F02 03B9; # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
	  (?\u1F8A ?\u1F82) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8B; F; 1F03 03B9; # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
	  (?\u1F8B ?\u1F83) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8C; F; 1F04 03B9; # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
	  (?\u1F8C ?\u1F84) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8D; F; 1F05 03B9; # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
	  (?\u1F8D ?\u1F85) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8E; F; 1F06 03B9; # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1F8E ?\u1F86) ;; GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F8F; F; 1F07 03B9; # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1F8F ?\u1F87) ;; GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F90; F; 1F20 03B9; # GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F91; F; 1F21 03B9; # GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F92; F; 1F22 03B9; # GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F93; F; 1F23 03B9; # GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F94; F; 1F24 03B9; # GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F95; F; 1F25 03B9; # GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F96; F; 1F26 03B9; # GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F97; F; 1F27 03B9; # GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F98; F; 1F20 03B9; # GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
	  (?\u1F98 ?\u1F90) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F99; F; 1F21 03B9; # GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
	  (?\u1F99 ?\u1F91) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9A; F; 1F22 03B9; # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
	  (?\u1F9A ?\u1F92) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9B; F; 1F23 03B9; # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
	  (?\u1F9B ?\u1F93) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9C; F; 1F24 03B9; # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
	  (?\u1F9C ?\u1F94) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9D; F; 1F25 03B9; # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
	  (?\u1F9D ?\u1F95) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9E; F; 1F26 03B9; # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1F9E ?\u1F96) ;; GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1F9F; F; 1F27 03B9; # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1F9F ?\u1F97) ;; GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA0; F; 1F60 03B9; # GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA1; F; 1F61 03B9; # GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA2; F; 1F62 03B9; # GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA3; F; 1F63 03B9; # GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA4; F; 1F64 03B9; # GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA5; F; 1F65 03B9; # GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA6; F; 1F66 03B9; # GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA7; F; 1F67 03B9; # GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA8; F; 1F60 03B9; # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
	  (?\u1FA8 ?\u1FA0) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FA9; F; 1F61 03B9; # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
	  (?\u1FA9 ?\u1FA1) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAA; F; 1F62 03B9; # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
	  (?\u1FAA ?\u1FA2) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAB; F; 1F63 03B9; # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
	  (?\u1FAB ?\u1FA3) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAC; F; 1F64 03B9; # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
	  (?\u1FAC ?\u1FA4) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAD; F; 1F65 03B9; # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
	  (?\u1FAD ?\u1FA5) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAE; F; 1F66 03B9; # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1FAE ?\u1FA6) ;; GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FAF; F; 1F67 03B9; # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
	  (?\u1FAF ?\u1FA7) ;; GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FB2; F; 1F70 03B9; # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FB3; F; 03B1 03B9; # GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FB4; F; 03AC 03B9; # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FB6; F; 03B1 0342; # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1FB7; F; 03B1 0342 03B9; # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
	  (?\u1FB8 ?\u1FB0) ;; GREEK CAPITAL LETTER ALPHA WITH VRACHY
	  (?\u1FB9 ?\u1FB1) ;; GREEK CAPITAL LETTER ALPHA WITH MACRON
	  (?\u1FBA ?\u1F70) ;; GREEK CAPITAL LETTER ALPHA WITH VARIA
	  (?\u1FBB ?\u1F71) ;; GREEK CAPITAL LETTER ALPHA WITH OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FBC; F; 03B1 03B9; # GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
	  (?\u1FBC ?\u1FB3) ;; GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
	  (?\u1FBE ?\u03B9) ;; GREEK PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FC2; F; 1F74 03B9; # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FC3; F; 03B7 03B9; # GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FC4; F; 03AE 03B9; # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FC6; F; 03B7 0342; # GREEK SMALL LETTER ETA WITH PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1FC7; F; 03B7 0342 03B9; # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
	  (?\u1FC8 ?\u1F72) ;; GREEK CAPITAL LETTER EPSILON WITH VARIA
	  (?\u1FC9 ?\u1F73) ;; GREEK CAPITAL LETTER EPSILON WITH OXIA
	  (?\u1FCA ?\u1F74) ;; GREEK CAPITAL LETTER ETA WITH VARIA
	  (?\u1FCB ?\u1F75) ;; GREEK CAPITAL LETTER ETA WITH OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FCC; F; 03B7 03B9; # GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
	  (?\u1FCC ?\u1FC3) ;; GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FD2; F; 03B9 0308 0300; # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
;;; WARNING: Unhandled full mapping:
;;;   1FD3; F; 03B9 0308 0301; # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FD6; F; 03B9 0342; # GREEK SMALL LETTER IOTA WITH PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1FD7; F; 03B9 0308 0342; # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
	  (?\u1FD8 ?\u1FD0) ;; GREEK CAPITAL LETTER IOTA WITH VRACHY
	  (?\u1FD9 ?\u1FD1) ;; GREEK CAPITAL LETTER IOTA WITH MACRON
	  (?\u1FDA ?\u1F76) ;; GREEK CAPITAL LETTER IOTA WITH VARIA
	  (?\u1FDB ?\u1F77) ;; GREEK CAPITAL LETTER IOTA WITH OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FE2; F; 03C5 0308 0300; # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
;;; WARNING: Unhandled full mapping:
;;;   1FE3; F; 03C5 0308 0301; # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FE4; F; 03C1 0313; # GREEK SMALL LETTER RHO WITH PSILI
;;; WARNING: Unhandled full mapping:
;;;   1FE6; F; 03C5 0342; # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1FE7; F; 03C5 0308 0342; # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
	  (?\u1FE8 ?\u1FE0) ;; GREEK CAPITAL LETTER UPSILON WITH VRACHY
	  (?\u1FE9 ?\u1FE1) ;; GREEK CAPITAL LETTER UPSILON WITH MACRON
	  (?\u1FEA ?\u1F7A) ;; GREEK CAPITAL LETTER UPSILON WITH VARIA
	  (?\u1FEB ?\u1F7B) ;; GREEK CAPITAL LETTER UPSILON WITH OXIA
	  (?\u1FEC ?\u1FE5) ;; GREEK CAPITAL LETTER RHO WITH DASIA
;;; WARNING: Unhandled full mapping:
;;;   1FF2; F; 1F7C 03B9; # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FF3; F; 03C9 03B9; # GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FF4; F; 03CE 03B9; # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
;;; WARNING: Unhandled full mapping:
;;;   1FF6; F; 03C9 0342; # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
;;; WARNING: Unhandled full mapping:
;;;   1FF7; F; 03C9 0342 03B9; # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
	  (?\u1FF8 ?\u1F78) ;; GREEK CAPITAL LETTER OMICRON WITH VARIA
	  (?\u1FF9 ?\u1F79) ;; GREEK CAPITAL LETTER OMICRON WITH OXIA
	  (?\u1FFA ?\u1F7C) ;; GREEK CAPITAL LETTER OMEGA WITH VARIA
	  (?\u1FFB ?\u1F7D) ;; GREEK CAPITAL LETTER OMEGA WITH OXIA
;;; WARNING: Unhandled full mapping:
;;;   1FFC; F; 03C9 03B9; # GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
	  (?\u1FFC ?\u1FF3) ;; GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
	  (?\u2126 ?\u03C9) ;; OHM SIGN
	  (?\u212A ?\u006B) ;; KELVIN SIGN
	  (?\u212B ?\u00E5) ;; ANGSTROM SIGN
	  (?\u2132 ?\u214E) ;; TURNED CAPITAL F
	  (?\u2160 ?\u2170) ;; ROMAN NUMERAL ONE
	  (?\u2161 ?\u2171) ;; ROMAN NUMERAL TWO
	  (?\u2162 ?\u2172) ;; ROMAN NUMERAL THREE
	  (?\u2163 ?\u2173) ;; ROMAN NUMERAL FOUR
	  (?\u2164 ?\u2174) ;; ROMAN NUMERAL FIVE
	  (?\u2165 ?\u2175) ;; ROMAN NUMERAL SIX
	  (?\u2166 ?\u2176) ;; ROMAN NUMERAL SEVEN
	  (?\u2167 ?\u2177) ;; ROMAN NUMERAL EIGHT
	  (?\u2168 ?\u2178) ;; ROMAN NUMERAL NINE
	  (?\u2169 ?\u2179) ;; ROMAN NUMERAL TEN
	  (?\u216A ?\u217A) ;; ROMAN NUMERAL ELEVEN
	  (?\u216B ?\u217B) ;; ROMAN NUMERAL TWELVE
	  (?\u216C ?\u217C) ;; ROMAN NUMERAL FIFTY
	  (?\u216D ?\u217D) ;; ROMAN NUMERAL ONE HUNDRED
	  (?\u216E ?\u217E) ;; ROMAN NUMERAL FIVE HUNDRED
	  (?\u216F ?\u217F) ;; ROMAN NUMERAL ONE THOUSAND
	  (?\u2183 ?\u2184) ;; ROMAN NUMERAL REVERSED ONE HUNDRED
	  (?\u24B6 ?\u24D0) ;; CIRCLED LATIN CAPITAL LETTER A
	  (?\u24B7 ?\u24D1) ;; CIRCLED LATIN CAPITAL LETTER B
	  (?\u24B8 ?\u24D2) ;; CIRCLED LATIN CAPITAL LETTER C
	  (?\u24B9 ?\u24D3) ;; CIRCLED LATIN CAPITAL LETTER D
	  (?\u24BA ?\u24D4) ;; CIRCLED LATIN CAPITAL LETTER E
	  (?\u24BB ?\u24D5) ;; CIRCLED LATIN CAPITAL LETTER F
	  (?\u24BC ?\u24D6) ;; CIRCLED LATIN CAPITAL LETTER G
	  (?\u24BD ?\u24D7) ;; CIRCLED LATIN CAPITAL LETTER H
	  (?\u24BE ?\u24D8) ;; CIRCLED LATIN CAPITAL LETTER I
	  (?\u24BF ?\u24D9) ;; CIRCLED LATIN CAPITAL LETTER J
	  (?\u24C0 ?\u24DA) ;; CIRCLED LATIN CAPITAL LETTER K
	  (?\u24C1 ?\u24DB) ;; CIRCLED LATIN CAPITAL LETTER L
	  (?\u24C2 ?\u24DC) ;; CIRCLED LATIN CAPITAL LETTER M
	  (?\u24C3 ?\u24DD) ;; CIRCLED LATIN CAPITAL LETTER N
	  (?\u24C4 ?\u24DE) ;; CIRCLED LATIN CAPITAL LETTER O
	  (?\u24C5 ?\u24DF) ;; CIRCLED LATIN CAPITAL LETTER P
	  (?\u24C6 ?\u24E0) ;; CIRCLED LATIN CAPITAL LETTER Q
	  (?\u24C7 ?\u24E1) ;; CIRCLED LATIN CAPITAL LETTER R
	  (?\u24C8 ?\u24E2) ;; CIRCLED LATIN CAPITAL LETTER S
	  (?\u24C9 ?\u24E3) ;; CIRCLED LATIN CAPITAL LETTER T
	  (?\u24CA ?\u24E4) ;; CIRCLED LATIN CAPITAL LETTER U
	  (?\u24CB ?\u24E5) ;; CIRCLED LATIN CAPITAL LETTER V
	  (?\u24CC ?\u24E6) ;; CIRCLED LATIN CAPITAL LETTER W
	  (?\u24CD ?\u24E7) ;; CIRCLED LATIN CAPITAL LETTER X
	  (?\u24CE ?\u24E8) ;; CIRCLED LATIN CAPITAL LETTER Y
	  (?\u24CF ?\u24E9) ;; CIRCLED LATIN CAPITAL LETTER Z
	  (?\u2C00 ?\u2C30) ;; GLAGOLITIC CAPITAL LETTER AZU
	  (?\u2C01 ?\u2C31) ;; GLAGOLITIC CAPITAL LETTER BUKY
	  (?\u2C02 ?\u2C32) ;; GLAGOLITIC CAPITAL LETTER VEDE
	  (?\u2C03 ?\u2C33) ;; GLAGOLITIC CAPITAL LETTER GLAGOLI
	  (?\u2C04 ?\u2C34) ;; GLAGOLITIC CAPITAL LETTER DOBRO
	  (?\u2C05 ?\u2C35) ;; GLAGOLITIC CAPITAL LETTER YESTU
	  (?\u2C06 ?\u2C36) ;; GLAGOLITIC CAPITAL LETTER ZHIVETE
	  (?\u2C07 ?\u2C37) ;; GLAGOLITIC CAPITAL LETTER DZELO
	  (?\u2C08 ?\u2C38) ;; GLAGOLITIC CAPITAL LETTER ZEMLJA
	  (?\u2C09 ?\u2C39) ;; GLAGOLITIC CAPITAL LETTER IZHE
	  (?\u2C0A ?\u2C3A) ;; GLAGOLITIC CAPITAL LETTER INITIAL IZHE
	  (?\u2C0B ?\u2C3B) ;; GLAGOLITIC CAPITAL LETTER I
	  (?\u2C0C ?\u2C3C) ;; GLAGOLITIC CAPITAL LETTER DJERVI
	  (?\u2C0D ?\u2C3D) ;; GLAGOLITIC CAPITAL LETTER KAKO
	  (?\u2C0E ?\u2C3E) ;; GLAGOLITIC CAPITAL LETTER LJUDIJE
	  (?\u2C0F ?\u2C3F) ;; GLAGOLITIC CAPITAL LETTER MYSLITE
	  (?\u2C10 ?\u2C40) ;; GLAGOLITIC CAPITAL LETTER NASHI
	  (?\u2C11 ?\u2C41) ;; GLAGOLITIC CAPITAL LETTER ONU
	  (?\u2C12 ?\u2C42) ;; GLAGOLITIC CAPITAL LETTER POKOJI
	  (?\u2C13 ?\u2C43) ;; GLAGOLITIC CAPITAL LETTER RITSI
	  (?\u2C14 ?\u2C44) ;; GLAGOLITIC CAPITAL LETTER SLOVO
	  (?\u2C15 ?\u2C45) ;; GLAGOLITIC CAPITAL LETTER TVRIDO
	  (?\u2C16 ?\u2C46) ;; GLAGOLITIC CAPITAL LETTER UKU
	  (?\u2C17 ?\u2C47) ;; GLAGOLITIC CAPITAL LETTER FRITU
	  (?\u2C18 ?\u2C48) ;; GLAGOLITIC CAPITAL LETTER HERU
	  (?\u2C19 ?\u2C49) ;; GLAGOLITIC CAPITAL LETTER OTU
	  (?\u2C1A ?\u2C4A) ;; GLAGOLITIC CAPITAL LETTER PE
	  (?\u2C1B ?\u2C4B) ;; GLAGOLITIC CAPITAL LETTER SHTA
	  (?\u2C1C ?\u2C4C) ;; GLAGOLITIC CAPITAL LETTER TSI
	  (?\u2C1D ?\u2C4D) ;; GLAGOLITIC CAPITAL LETTER CHRIVI
	  (?\u2C1E ?\u2C4E) ;; GLAGOLITIC CAPITAL LETTER SHA
	  (?\u2C1F ?\u2C4F) ;; GLAGOLITIC CAPITAL LETTER YERU
	  (?\u2C20 ?\u2C50) ;; GLAGOLITIC CAPITAL LETTER YERI
	  (?\u2C21 ?\u2C51) ;; GLAGOLITIC CAPITAL LETTER YATI
	  (?\u2C22 ?\u2C52) ;; GLAGOLITIC CAPITAL LETTER SPIDERY HA
	  (?\u2C23 ?\u2C53) ;; GLAGOLITIC CAPITAL LETTER YU
	  (?\u2C24 ?\u2C54) ;; GLAGOLITIC CAPITAL LETTER SMALL YUS
	  (?\u2C25 ?\u2C55) ;; GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
	  (?\u2C26 ?\u2C56) ;; GLAGOLITIC CAPITAL LETTER YO
	  (?\u2C27 ?\u2C57) ;; GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
	  (?\u2C28 ?\u2C58) ;; GLAGOLITIC CAPITAL LETTER BIG YUS
	  (?\u2C29 ?\u2C59) ;; GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
	  (?\u2C2A ?\u2C5A) ;; GLAGOLITIC CAPITAL LETTER FITA
	  (?\u2C2B ?\u2C5B) ;; GLAGOLITIC CAPITAL LETTER IZHITSA
	  (?\u2C2C ?\u2C5C) ;; GLAGOLITIC CAPITAL LETTER SHTAPIC
	  (?\u2C2D ?\u2C5D) ;; GLAGOLITIC CAPITAL LETTER TROKUTASTI A
	  (?\u2C2E ?\u2C5E) ;; GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
	  (?\u2C60 ?\u2C61) ;; LATIN CAPITAL LETTER L WITH DOUBLE BAR
	  (?\u2C62 ?\u026B) ;; LATIN CAPITAL LETTER L WITH MIDDLE TILDE
	  (?\u2C63 ?\u1D7D) ;; LATIN CAPITAL LETTER P WITH STROKE
	  (?\u2C64 ?\u027D) ;; LATIN CAPITAL LETTER R WITH TAIL
	  (?\u2C67 ?\u2C68) ;; LATIN CAPITAL LETTER H WITH DESCENDER
	  (?\u2C69 ?\u2C6A) ;; LATIN CAPITAL LETTER K WITH DESCENDER
	  (?\u2C6B ?\u2C6C) ;; LATIN CAPITAL LETTER Z WITH DESCENDER
	  (?\u2C6D ?\u0251) ;; LATIN CAPITAL LETTER ALPHA
	  (?\u2C6E ?\u0271) ;; LATIN CAPITAL LETTER M WITH HOOK
	  (?\u2C6F ?\u0250) ;; LATIN CAPITAL LETTER TURNED A
	  (?\u2C70 ?\u0252) ;; LATIN CAPITAL LETTER TURNED ALPHA
	  (?\u2C72 ?\u2C73) ;; LATIN CAPITAL LETTER W WITH HOOK
	  (?\u2C75 ?\u2C76) ;; LATIN CAPITAL LETTER HALF H
	  (?\u2C7E ?\u023F) ;; LATIN CAPITAL LETTER S WITH SWASH TAIL
	  (?\u2C7F ?\u0240) ;; LATIN CAPITAL LETTER Z WITH SWASH TAIL
	  (?\u2C80 ?\u2C81) ;; COPTIC CAPITAL LETTER ALFA
	  (?\u2C82 ?\u2C83) ;; COPTIC CAPITAL LETTER VIDA
	  (?\u2C84 ?\u2C85) ;; COPTIC CAPITAL LETTER GAMMA
	  (?\u2C86 ?\u2C87) ;; COPTIC CAPITAL LETTER DALDA
	  (?\u2C88 ?\u2C89) ;; COPTIC CAPITAL LETTER EIE
	  (?\u2C8A ?\u2C8B) ;; COPTIC CAPITAL LETTER SOU
	  (?\u2C8C ?\u2C8D) ;; COPTIC CAPITAL LETTER ZATA
	  (?\u2C8E ?\u2C8F) ;; COPTIC CAPITAL LETTER HATE
	  (?\u2C90 ?\u2C91) ;; COPTIC CAPITAL LETTER THETHE
	  (?\u2C92 ?\u2C93) ;; COPTIC CAPITAL LETTER IAUDA
	  (?\u2C94 ?\u2C95) ;; COPTIC CAPITAL LETTER KAPA
	  (?\u2C96 ?\u2C97) ;; COPTIC CAPITAL LETTER LAULA
	  (?\u2C98 ?\u2C99) ;; COPTIC CAPITAL LETTER MI
	  (?\u2C9A ?\u2C9B) ;; COPTIC CAPITAL LETTER NI
	  (?\u2C9C ?\u2C9D) ;; COPTIC CAPITAL LETTER KSI
	  (?\u2C9E ?\u2C9F) ;; COPTIC CAPITAL LETTER O
	  (?\u2CA0 ?\u2CA1) ;; COPTIC CAPITAL LETTER PI
	  (?\u2CA2 ?\u2CA3) ;; COPTIC CAPITAL LETTER RO
	  (?\u2CA4 ?\u2CA5) ;; COPTIC CAPITAL LETTER SIMA
	  (?\u2CA6 ?\u2CA7) ;; COPTIC CAPITAL LETTER TAU
	  (?\u2CA8 ?\u2CA9) ;; COPTIC CAPITAL LETTER UA
	  (?\u2CAA ?\u2CAB) ;; COPTIC CAPITAL LETTER FI
	  (?\u2CAC ?\u2CAD) ;; COPTIC CAPITAL LETTER KHI
	  (?\u2CAE ?\u2CAF) ;; COPTIC CAPITAL LETTER PSI
	  (?\u2CB0 ?\u2CB1) ;; COPTIC CAPITAL LETTER OOU
	  (?\u2CB2 ?\u2CB3) ;; COPTIC CAPITAL LETTER DIALECT-P ALEF
	  (?\u2CB4 ?\u2CB5) ;; COPTIC CAPITAL LETTER OLD COPTIC AIN
	  (?\u2CB6 ?\u2CB7) ;; COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
	  (?\u2CB8 ?\u2CB9) ;; COPTIC CAPITAL LETTER DIALECT-P KAPA
	  (?\u2CBA ?\u2CBB) ;; COPTIC CAPITAL LETTER DIALECT-P NI
	  (?\u2CBC ?\u2CBD) ;; COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
	  (?\u2CBE ?\u2CBF) ;; COPTIC CAPITAL LETTER OLD COPTIC OOU
	  (?\u2CC0 ?\u2CC1) ;; COPTIC CAPITAL LETTER SAMPI
	  (?\u2CC2 ?\u2CC3) ;; COPTIC CAPITAL LETTER CROSSED SHEI
	  (?\u2CC4 ?\u2CC5) ;; COPTIC CAPITAL LETTER OLD COPTIC SHEI
	  (?\u2CC6 ?\u2CC7) ;; COPTIC CAPITAL LETTER OLD COPTIC ESH
	  (?\u2CC8 ?\u2CC9) ;; COPTIC CAPITAL LETTER AKHMIMIC KHEI
	  (?\u2CCA ?\u2CCB) ;; COPTIC CAPITAL LETTER DIALECT-P HORI
	  (?\u2CCC ?\u2CCD) ;; COPTIC CAPITAL LETTER OLD COPTIC HORI
	  (?\u2CCE ?\u2CCF) ;; COPTIC CAPITAL LETTER OLD COPTIC HA
	  (?\u2CD0 ?\u2CD1) ;; COPTIC CAPITAL LETTER L-SHAPED HA
	  (?\u2CD2 ?\u2CD3) ;; COPTIC CAPITAL LETTER OLD COPTIC HEI
	  (?\u2CD4 ?\u2CD5) ;; COPTIC CAPITAL LETTER OLD COPTIC HAT
	  (?\u2CD6 ?\u2CD7) ;; COPTIC CAPITAL LETTER OLD COPTIC GANGIA
	  (?\u2CD8 ?\u2CD9) ;; COPTIC CAPITAL LETTER OLD COPTIC DJA
	  (?\u2CDA ?\u2CDB) ;; COPTIC CAPITAL LETTER OLD COPTIC SHIMA
	  (?\u2CDC ?\u2CDD) ;; COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
	  (?\u2CDE ?\u2CDF) ;; COPTIC CAPITAL LETTER OLD NUBIAN NGI
	  (?\u2CE0 ?\u2CE1) ;; COPTIC CAPITAL LETTER OLD NUBIAN NYI
	  (?\u2CE2 ?\u2CE3) ;; COPTIC CAPITAL LETTER OLD NUBIAN WAU
	  (?\u2CEB ?\u2CEC) ;; COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
	  (?\u2CED ?\u2CEE) ;; COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
	  (?\uA640 ?\uA641) ;; CYRILLIC CAPITAL LETTER ZEMLYA
	  (?\uA642 ?\uA643) ;; CYRILLIC CAPITAL LETTER DZELO
	  (?\uA644 ?\uA645) ;; CYRILLIC CAPITAL LETTER REVERSED DZE
	  (?\uA646 ?\uA647) ;; CYRILLIC CAPITAL LETTER IOTA
	  (?\uA648 ?\uA649) ;; CYRILLIC CAPITAL LETTER DJERV
	  (?\uA64A ?\uA64B) ;; CYRILLIC CAPITAL LETTER MONOGRAPH UK
	  (?\uA64C ?\uA64D) ;; CYRILLIC CAPITAL LETTER BROAD OMEGA
	  (?\uA64E ?\uA64F) ;; CYRILLIC CAPITAL LETTER NEUTRAL YER
	  (?\uA650 ?\uA651) ;; CYRILLIC CAPITAL LETTER YERU WITH BACK YER
	  (?\uA652 ?\uA653) ;; CYRILLIC CAPITAL LETTER IOTIFIED YAT
	  (?\uA654 ?\uA655) ;; CYRILLIC CAPITAL LETTER REVERSED YU
	  (?\uA656 ?\uA657) ;; CYRILLIC CAPITAL LETTER IOTIFIED A
	  (?\uA658 ?\uA659) ;; CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
	  (?\uA65A ?\uA65B) ;; CYRILLIC CAPITAL LETTER BLENDED YUS
	  (?\uA65C ?\uA65D) ;; CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
	  (?\uA65E ?\uA65F) ;; CYRILLIC CAPITAL LETTER YN
	  (?\uA662 ?\uA663) ;; CYRILLIC CAPITAL LETTER SOFT DE
	  (?\uA664 ?\uA665) ;; CYRILLIC CAPITAL LETTER SOFT EL
	  (?\uA666 ?\uA667) ;; CYRILLIC CAPITAL LETTER SOFT EM
	  (?\uA668 ?\uA669) ;; CYRILLIC CAPITAL LETTER MONOCULAR O
	  (?\uA66A ?\uA66B) ;; CYRILLIC CAPITAL LETTER BINOCULAR O
	  (?\uA66C ?\uA66D) ;; CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
	  (?\uA680 ?\uA681) ;; CYRILLIC CAPITAL LETTER DWE
	  (?\uA682 ?\uA683) ;; CYRILLIC CAPITAL LETTER DZWE
	  (?\uA684 ?\uA685) ;; CYRILLIC CAPITAL LETTER ZHWE
	  (?\uA686 ?\uA687) ;; CYRILLIC CAPITAL LETTER CCHE
	  (?\uA688 ?\uA689) ;; CYRILLIC CAPITAL LETTER DZZE
	  (?\uA68A ?\uA68B) ;; CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
	  (?\uA68C ?\uA68D) ;; CYRILLIC CAPITAL LETTER TWE
	  (?\uA68E ?\uA68F) ;; CYRILLIC CAPITAL LETTER TSWE
	  (?\uA690 ?\uA691) ;; CYRILLIC CAPITAL LETTER TSSE
	  (?\uA692 ?\uA693) ;; CYRILLIC CAPITAL LETTER TCHE
	  (?\uA694 ?\uA695) ;; CYRILLIC CAPITAL LETTER HWE
	  (?\uA696 ?\uA697) ;; CYRILLIC CAPITAL LETTER SHWE
	  (?\uA722 ?\uA723) ;; LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
	  (?\uA724 ?\uA725) ;; LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
	  (?\uA726 ?\uA727) ;; LATIN CAPITAL LETTER HENG
	  (?\uA728 ?\uA729) ;; LATIN CAPITAL LETTER TZ
	  (?\uA72A ?\uA72B) ;; LATIN CAPITAL LETTER TRESILLO
	  (?\uA72C ?\uA72D) ;; LATIN CAPITAL LETTER CUATRILLO
	  (?\uA72E ?\uA72F) ;; LATIN CAPITAL LETTER CUATRILLO WITH COMMA
	  (?\uA732 ?\uA733) ;; LATIN CAPITAL LETTER AA
	  (?\uA734 ?\uA735) ;; LATIN CAPITAL LETTER AO
	  (?\uA736 ?\uA737) ;; LATIN CAPITAL LETTER AU
	  (?\uA738 ?\uA739) ;; LATIN CAPITAL LETTER AV
	  (?\uA73A ?\uA73B) ;; LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
	  (?\uA73C ?\uA73D) ;; LATIN CAPITAL LETTER AY
	  (?\uA73E ?\uA73F) ;; LATIN CAPITAL LETTER REVERSED C WITH DOT
	  (?\uA740 ?\uA741) ;; LATIN CAPITAL LETTER K WITH STROKE
	  (?\uA742 ?\uA743) ;; LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
	  (?\uA744 ?\uA745) ;; LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
	  (?\uA746 ?\uA747) ;; LATIN CAPITAL LETTER BROKEN L
	  (?\uA748 ?\uA749) ;; LATIN CAPITAL LETTER L WITH HIGH STROKE
	  (?\uA74A ?\uA74B) ;; LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
	  (?\uA74C ?\uA74D) ;; LATIN CAPITAL LETTER O WITH LOOP
	  (?\uA74E ?\uA74F) ;; LATIN CAPITAL LETTER OO
	  (?\uA750 ?\uA751) ;; LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
	  (?\uA752 ?\uA753) ;; LATIN CAPITAL LETTER P WITH FLOURISH
	  (?\uA754 ?\uA755) ;; LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
	  (?\uA756 ?\uA757) ;; LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
	  (?\uA758 ?\uA759) ;; LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
	  (?\uA75A ?\uA75B) ;; LATIN CAPITAL LETTER R ROTUNDA
	  (?\uA75C ?\uA75D) ;; LATIN CAPITAL LETTER RUM ROTUNDA
	  (?\uA75E ?\uA75F) ;; LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
	  (?\uA760 ?\uA761) ;; LATIN CAPITAL LETTER VY
	  (?\uA762 ?\uA763) ;; LATIN CAPITAL LETTER VISIGOTHIC Z
	  (?\uA764 ?\uA765) ;; LATIN CAPITAL LETTER THORN WITH STROKE
	  (?\uA766 ?\uA767) ;; LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
	  (?\uA768 ?\uA769) ;; LATIN CAPITAL LETTER VEND
	  (?\uA76A ?\uA76B) ;; LATIN CAPITAL LETTER ET
	  (?\uA76C ?\uA76D) ;; LATIN CAPITAL LETTER IS
	  (?\uA76E ?\uA76F) ;; LATIN CAPITAL LETTER CON
	  (?\uA779 ?\uA77A) ;; LATIN CAPITAL LETTER INSULAR D
	  (?\uA77B ?\uA77C) ;; LATIN CAPITAL LETTER INSULAR F
	  (?\uA77D ?\u1D79) ;; LATIN CAPITAL LETTER INSULAR G
	  (?\uA77E ?\uA77F) ;; LATIN CAPITAL LETTER TURNED INSULAR G
	  (?\uA780 ?\uA781) ;; LATIN CAPITAL LETTER TURNED L
	  (?\uA782 ?\uA783) ;; LATIN CAPITAL LETTER INSULAR R
	  (?\uA784 ?\uA785) ;; LATIN CAPITAL LETTER INSULAR S
	  (?\uA786 ?\uA787) ;; LATIN CAPITAL LETTER INSULAR T
	  (?\uA78B ?\uA78C) ;; LATIN CAPITAL LETTER SALTILLO
;;; WARNING: Unhandled full mapping:
;;;   FB00; F; 0066 0066; # LATIN SMALL LIGATURE FF
;;; WARNING: Unhandled full mapping:
;;;   FB01; F; 0066 0069; # LATIN SMALL LIGATURE FI
;;; WARNING: Unhandled full mapping:
;;;   FB02; F; 0066 006C; # LATIN SMALL LIGATURE FL
;;; WARNING: Unhandled full mapping:
;;;   FB03; F; 0066 0066 0069; # LATIN SMALL LIGATURE FFI
;;; WARNING: Unhandled full mapping:
;;;   FB04; F; 0066 0066 006C; # LATIN SMALL LIGATURE FFL
;;; WARNING: Unhandled full mapping:
;;;   FB05; F; 0073 0074; # LATIN SMALL LIGATURE LONG S T
;;; WARNING: Unhandled full mapping:
;;;   FB06; F; 0073 0074; # LATIN SMALL LIGATURE ST
;;; WARNING: Unhandled full mapping:
;;;   FB13; F; 0574 0576; # ARMENIAN SMALL LIGATURE MEN NOW
;;; WARNING: Unhandled full mapping:
;;;   FB14; F; 0574 0565; # ARMENIAN SMALL LIGATURE MEN ECH
;;; WARNING: Unhandled full mapping:
;;;   FB15; F; 0574 056B; # ARMENIAN SMALL LIGATURE MEN INI
;;; WARNING: Unhandled full mapping:
;;;   FB16; F; 057E 0576; # ARMENIAN SMALL LIGATURE VEW NOW
;;; WARNING: Unhandled full mapping:
;;;   FB17; F; 0574 056D; # ARMENIAN SMALL LIGATURE MEN XEH
	  (?\uFF21 ?\uFF41) ;; FULLWIDTH LATIN CAPITAL LETTER A
	  (?\uFF22 ?\uFF42) ;; FULLWIDTH LATIN CAPITAL LETTER B
	  (?\uFF23 ?\uFF43) ;; FULLWIDTH LATIN CAPITAL LETTER C
	  (?\uFF24 ?\uFF44) ;; FULLWIDTH LATIN CAPITAL LETTER D
	  (?\uFF25 ?\uFF45) ;; FULLWIDTH LATIN CAPITAL LETTER E
	  (?\uFF26 ?\uFF46) ;; FULLWIDTH LATIN CAPITAL LETTER F
	  (?\uFF27 ?\uFF47) ;; FULLWIDTH LATIN CAPITAL LETTER G
	  (?\uFF28 ?\uFF48) ;; FULLWIDTH LATIN CAPITAL LETTER H
	  (?\uFF29 ?\uFF49) ;; FULLWIDTH LATIN CAPITAL LETTER I
	  (?\uFF2A ?\uFF4A) ;; FULLWIDTH LATIN CAPITAL LETTER J
	  (?\uFF2B ?\uFF4B) ;; FULLWIDTH LATIN CAPITAL LETTER K
	  (?\uFF2C ?\uFF4C) ;; FULLWIDTH LATIN CAPITAL LETTER L
	  (?\uFF2D ?\uFF4D) ;; FULLWIDTH LATIN CAPITAL LETTER M
	  (?\uFF2E ?\uFF4E) ;; FULLWIDTH LATIN CAPITAL LETTER N
	  (?\uFF2F ?\uFF4F) ;; FULLWIDTH LATIN CAPITAL LETTER O
	  (?\uFF30 ?\uFF50) ;; FULLWIDTH LATIN CAPITAL LETTER P
	  (?\uFF31 ?\uFF51) ;; FULLWIDTH LATIN CAPITAL LETTER Q
	  (?\uFF32 ?\uFF52) ;; FULLWIDTH LATIN CAPITAL LETTER R
	  (?\uFF33 ?\uFF53) ;; FULLWIDTH LATIN CAPITAL LETTER S
	  (?\uFF34 ?\uFF54) ;; FULLWIDTH LATIN CAPITAL LETTER T
	  (?\uFF35 ?\uFF55) ;; FULLWIDTH LATIN CAPITAL LETTER U
	  (?\uFF36 ?\uFF56) ;; FULLWIDTH LATIN CAPITAL LETTER V
	  (?\uFF37 ?\uFF57) ;; FULLWIDTH LATIN CAPITAL LETTER W
	  (?\uFF38 ?\uFF58) ;; FULLWIDTH LATIN CAPITAL LETTER X
	  (?\uFF39 ?\uFF59) ;; FULLWIDTH LATIN CAPITAL LETTER Y
	  (?\uFF3A ?\uFF5A) ;; FULLWIDTH LATIN CAPITAL LETTER Z
	  (?\U00010400 ?\U00010428) ;; DESERET CAPITAL LETTER LONG I
	  (?\U00010401 ?\U00010429) ;; DESERET CAPITAL LETTER LONG E
	  (?\U00010402 ?\U0001042A) ;; DESERET CAPITAL LETTER LONG A
	  (?\U00010403 ?\U0001042B) ;; DESERET CAPITAL LETTER LONG AH
	  (?\U00010404 ?\U0001042C) ;; DESERET CAPITAL LETTER LONG O
	  (?\U00010405 ?\U0001042D) ;; DESERET CAPITAL LETTER LONG OO
	  (?\U00010406 ?\U0001042E) ;; DESERET CAPITAL LETTER SHORT I
	  (?\U00010407 ?\U0001042F) ;; DESERET CAPITAL LETTER SHORT E
	  (?\U00010408 ?\U00010430) ;; DESERET CAPITAL LETTER SHORT A
	  (?\U00010409 ?\U00010431) ;; DESERET CAPITAL LETTER SHORT AH
	  (?\U0001040A ?\U00010432) ;; DESERET CAPITAL LETTER SHORT O
	  (?\U0001040B ?\U00010433) ;; DESERET CAPITAL LETTER SHORT OO
	  (?\U0001040C ?\U00010434) ;; DESERET CAPITAL LETTER AY
	  (?\U0001040D ?\U00010435) ;; DESERET CAPITAL LETTER OW
	  (?\U0001040E ?\U00010436) ;; DESERET CAPITAL LETTER WU
	  (?\U0001040F ?\U00010437) ;; DESERET CAPITAL LETTER YEE
	  (?\U00010410 ?\U00010438) ;; DESERET CAPITAL LETTER H
	  (?\U00010411 ?\U00010439) ;; DESERET CAPITAL LETTER PEE
	  (?\U00010412 ?\U0001043A) ;; DESERET CAPITAL LETTER BEE
	  (?\U00010413 ?\U0001043B) ;; DESERET CAPITAL LETTER TEE
	  (?\U00010414 ?\U0001043C) ;; DESERET CAPITAL LETTER DEE
	  (?\U00010415 ?\U0001043D) ;; DESERET CAPITAL LETTER CHEE
	  (?\U00010416 ?\U0001043E) ;; DESERET CAPITAL LETTER JEE
	  (?\U00010417 ?\U0001043F) ;; DESERET CAPITAL LETTER KAY
	  (?\U00010418 ?\U00010440) ;; DESERET CAPITAL LETTER GAY
	  (?\U00010419 ?\U00010441) ;; DESERET CAPITAL LETTER EF
	  (?\U0001041A ?\U00010442) ;; DESERET CAPITAL LETTER VEE
	  (?\U0001041B ?\U00010443) ;; DESERET CAPITAL LETTER ETH
	  (?\U0001041C ?\U00010444) ;; DESERET CAPITAL LETTER THEE
	  (?\U0001041D ?\U00010445) ;; DESERET CAPITAL LETTER ES
	  (?\U0001041E ?\U00010446) ;; DESERET CAPITAL LETTER ZEE
	  (?\U0001041F ?\U00010447) ;; DESERET CAPITAL LETTER ESH
	  (?\U00010420 ?\U00010448) ;; DESERET CAPITAL LETTER ZHEE
	  (?\U00010421 ?\U00010449) ;; DESERET CAPITAL LETTER ER
	  (?\U00010422 ?\U0001044A) ;; DESERET CAPITAL LETTER EL
	  (?\U00010423 ?\U0001044B) ;; DESERET CAPITAL LETTER EM
	  (?\U00010424 ?\U0001044C) ;; DESERET CAPITAL LETTER EN
	  (?\U00010425 ?\U0001044D) ;; DESERET CAPITAL LETTER ENG
	  (?\U00010426 ?\U0001044E) ;; DESERET CAPITAL LETTER OI
	  (?\U00010427 ?\U0001044F) ;; DESERET CAPITAL LETTER EW
	  ))
       (uni-casetab (loop
		      with case-table = (make-case-table)
		      for (uc lc) in uni-mappings
		      do (put-case-table-pair uc lc case-table)
		      finally return case-table))
       ;; All lowercase
       (lower (with-output-to-string
		(loop for (uc lc) in uni-mappings do (princ lc))))
       ;; All uppercase
       (upper (with-output-to-string
		(loop for (uc lc) in uni-mappings do (princ lc))))
       ;; For each pair, lower followed by upper
       (lowerupper (with-output-to-string
		     (loop for (uc lc) in uni-mappings
		       do (princ lc) (princ uc))))
       ;; For each pair, upper followed by lower
       (upperlower (with-output-to-string
		     (loop for (uc lc) in uni-mappings
		       do (princ uc) (princ lc))))
       )
  (with-case-table uni-casetab
    (Assert-equalp lower upper)
    (Assert-equalp lowerupper upperlower)
    (Assert-equal lower (downcase upper))
    (Assert-equal upper (downcase lower))
    (Assert-equal lower (downcase upper))
    (Assert-equal upper (downcase lower))
    (Assert-equal (downcase lower) (downcase (downcase lower)))
    (Assert-equal (upcase lowerupper) (upcase upperlower))
    (Assert-equal (downcase lowerupper) (downcase upperlower))
    (with-temp-buffer
      (set-case-table uni-casetab)
      (loop for (str1 str2) in `((,lower ,upper)
				 (,lowerupper ,upperlower)
				 (,upper ,lower)
				 (,upperlower ,lowerupper))
	do
	(erase-buffer)
	(Assert= (point-min) 1)
	(Assert= (point) 1)
	(insert str1)
	(let ((point (point))
	      (case-fold-search t))
	  (Assert= (length str1) (1- point))
	  (goto-char (point-min))
	  (Assert-eql (search-forward str2 nil t) point)))
      (loop for (uc lc) in uni-mappings do
	(loop for (ch1 ch2) in `((,uc ,lc)
				 (,lc ,uc))
	  do
	  (erase-buffer)
	  (insert ?a)
	  (insert ch1)
	  (insert ?b)
	  (goto-char (point-min))
	  (Assert-eql (search-forward (char-to-string ch2) nil t) 3
		      (format "Case-folded searching doesn't equate %s and %s"
			      (char-as-unicode-escape ch1)
			      (char-as-unicode-escape ch2))))))))
