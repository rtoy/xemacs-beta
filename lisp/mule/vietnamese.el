;;; vietnamese.el --- Support for Vietnamese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2002 Ben Wing.

;; Keywords: multilingual, Vietnamese

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

;;; Commentary:

;; For Vietnamese, the character sets VISCII and VSCII are supported.

;;; Code:

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/vietnamese.el
(modify-syntax-entry 'vietnamese-viscii-lower "w")
(modify-syntax-entry 'vietnamese-viscii-upper "w")

||||||| /DOCUME~1/Ben/LOCALS~2/Temp/vietnamese.el~base.hkfFmQ
;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case"
	      '(dimension
		1
		registry "VISCII1.1"
		chars 96
		columns 1
		direction l2r
		final ?1
		graphic 1
		short-name "VISCII lower"
		long-name "VISCII lower-case"
		))

(make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case"
	      '(dimension
		1
		registry "VISCII1.1"
		chars 96
		columns 1
		direction l2r
		final ?2
		graphic 1
		short-name "VISCII upper"
		long-name "VISCII upper-case"
		))

(modify-syntax-entry 'vietnamese-viscii-lower "w")
(modify-syntax-entry 'vietnamese-viscii-upper "w")

=======
;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case"
	      '(dimension
		1
		registries ["VISCII1.1"]
		chars 96
		columns 1
		direction l2r
		final ?1
		graphic 1
		short-name "VISCII lower"
		long-name "VISCII lower-case"
		))

(make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case"
	      '(dimension
		1
		registries ["VISCII1.1"]
		chars 96
		columns 1
		direction l2r
		final ?2
		graphic 1
		short-name "VISCII upper"
		long-name "VISCII upper-case"
		))

>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/vietnamese.el~other.c7yjQY
(define-category ?v "Vietnamese character.")
(modify-category-entry 'vietnamese-viscii-lower ?v)
(modify-category-entry 'vietnamese-viscii-upper ?v)

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/vietnamese.el
(eval-and-compile

(defvar viet-viscii-decode-table
  [;; VISCII is a full 8-bit code.
   0 1 ?,2F(B 3 4 ?,2G(B ?,2g(B 7 8 9 10 11 12 13 14 15
   16 17 18 19 ?,2V(B 21 22 23 24 ?,2[(B 26 27 28 29 ?,2\(B 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2U(B ?,2!(B ?,2"(B ?,2#(B ?,2$(B ?,2%(B ?,2&(B ?,2'(B ?,2((B ?,2)(B ?,2*(B ?,2+(B ?,2,(B ?,2-(B ?,2.(B ?,2/(B
   ?,20(B ?,21(B ?,22(B ?,25(B ?,2~(B ?,2>(B ?,26(B ?,27(B ?,28(B ?,2v(B ?,2w(B ?,2o(B ?,2|(B ?,2{(B ?,2x(B ?,2O(B
   ?,2u(B ?,1!(B ?,1"(B ?,1#(B ?,1$(B ?,1%(B ?,1&(B ?,1'(B ?,1((B ?,1)(B ?,1*(B ?,1+(B ?,1,(B ?,1-(B ?,1.(B ?,1/(B
   ?,10(B ?,11(B ?,12(B ?,2^(B ?,2=(B ?,15(B ?,16(B ?,17(B ?,18(B ?,2q(B ?,2Q(B ?,2W(B ?,2X(B ?,1=(B ?,1>(B ?,2_(B
   ?,2`(B ?,2a(B ?,2b(B ?,2c(B ?,2d(B ?,2e(B ?,1F(B ?,1G(B ?,2h(B ?,2i(B ?,2j(B ?,2k(B ?,2l(B ?,2m(B ?,2n(B ?,1O(B
   ?,2p(B ?,1Q(B ?,2r(B ?,2s(B ?,2t(B ?,1U(B ?,1V(B ?,1W(B ?,1X(B ?,2y(B ?,2z(B ?,1[(B ?,1\(B ?,2}(B ?,1^(B ?,1_(B
   ?,1`(B ?,1a(B ?,1b(B ?,1c(B ?,1d(B ?,1e(B ?,1f(B ?,1g(B ?,1h(B ?,1i(B ?,1j(B ?,1k(B ?,1l(B ?,1m(B ?,1n(B ?,1o(B
   ?,1p(B ?,1q(B ?,1r(B ?,1s(B ?,1t(B ?,1u(B ?,1v(B ?,1w(B ?,1x(B ?,1y(B ?,1z(B ?,1{(B ?,1|(B ?,1}(B ?,1~(B ?,2f(B ]
  "Vietnamese VISCII decoding table.")

(defvar viet-viscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (char-to-charset-codepoint (aref viet-viscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (- (nth 1 char-component) 128) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (- (nth 1 char-component) 128) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VISCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

(defvar viet-vscii-decode-table
  [;; VSCII is a full 8-bit code.
   0 ?,2z(B ?,2x(B 3 ?,2W(B ?,2X(B ?,2f(B 7 8 9 10 11 12 13 14 15
   16 ?,2Q(B ?,2_(B ?,2O(B ?,2V(B ?,2[(B ?,2}(B ?,2\(B 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2`(B ?,2d(B ?,2c(B ?,2a(B ?,2U(B ?,2#(B ?,2'(B ?,2h(B ?,2k(B ?,2((B ?,2i(B ?,2)(B ?,2.(B ?,2l(B ?,2o(B ?,2n(B
   ?,2m(B ?,28(B ?,2r(B ?,2v(B ?,2u(B ?,2s(B ?,2w(B ?,25(B ?,26(B ?,27(B ?,2^(B ?,2>(B ?,2~(B ?,2y(B ?,2|(B ?,2{(B
   160 ?,2e(B ?,2b(B ?,2j(B ?,2t(B ?,2=(B ?,2_(B ?,2p(B ?,1e(B ?,1b(B ?,1j(B ?,1t(B ?,1>(B ?,1y(B ?,1p(B ?,2"(B
   192 193 194 195 196 ?,1`(B ?,1d(B ?,1c(B ?,1a(B ?,1U(B ?,2F(B ?,1"(B ?,1F(B ?,1G(B ?,1!(B ?,2G(B
   ?,2!(B ?,2%(B ?,2&(B ?,2g(B ?,2%(B ?,2+(B ?,1#(B ?,1%(B ?,1&(B ?,1g(B ?,1$(B ?,1'(B ?,1h(B ?,2,(B ?,1k(B ?,1((B
   ?,1i(B ?,1)(B ?,1+(B ?,1,(B ?,1-(B ?,1*(B ?,1.(B ?,1l(B ?,1o(B ?,2-(B ?,2*(B ?,20(B ?,1n(B ?,1m(B ?,18(B ?,1r(B
   ?,21(B ?,1v(B ?,1u(B ?,1s(B ?,1w(B ?,10(B ?,11(B ?,12(B ?,1/(B ?,15(B ?,16(B ?,17(B ?,1^(B ?,1>(B ?,1~(B ?,1y(B
   ?,22(B ?,1|(B ?,1{(B ?,1z(B ?,1x(B ?,1W(B ?,1X(B ?,1f(B ?,1Q(B ?,1q(B ?,1O(B ?,1V(B ?,1[(B ?,1}(B ?,1\(B ?,2/(B]
  "Vietnamese VSCII decoding table.")

(defvar viet-vscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (char-to-charset-codepoint (aref viet-vscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (- (nth 1 char-component) 128) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (- (nth 1 char-component) 128) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VSCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

)

(when (featurep 'ccl)

(define-ccl-program ccl-decode-viscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-viscii-decode-table))
     ))
  "CCL program to decode VISCII 1.1")

;; Multibyte form of a Vietnamese character is as follows (3-byte):
;;   LEADING-CODE-PRIVATE-11 LEADING-CODE-EXTENDED-11 POSITION-CODE
;; where LEADING-CODE-EXTENDED-11 for Vietnamese is
;; `vietnamese-viscii-lower' or `vietnamese-viscii-upper'.

(defvar leading-code-private-11 #x9E)

(define-ccl-program ccl-encode-viscii
  `(1
     ((read r0)
      (loop
       (if (r0 < 128)
	   ;; ASCII
	   (write-read-repeat r0)
	 ;; not ASCII
	 (if (r0 != ,leading-code-private-11)
	     ;; not Vietnamese
	     (write-read-repeat r0)
	   ((read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	     (;; Vietnamese lower
	      (read r0)
	      (r0 -= 128)
	      (write-read-repeat r0 ,(car viet-viscii-encode-table)))
	     (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		 (;; Vietnamese upper
		  (read r0)
		  (r0 -= 128)
		  (write-read-repeat r0 ,(cdr viet-viscii-encode-table)))
	       ;; not Vietnamese
	       (write-read-repeat r0)))))))))
  "CCL program to encode VISCII 1.1")

(define-ccl-program ccl-encode-viscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-viscii-encode-table))
      (r1 = r1 ,(cdr viet-viscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VISCII 1.1 font")

(define-ccl-program ccl-decode-vscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-vscii-decode-table))
     ))
  "CCL program to decode VSCII-1.")

(define-ccl-program ccl-encode-vscii
  `(1
    ((read r0)
     (loop
      (if (r0 < 128)
	  ;; ASCII
	  (write-read-repeat r0)
	;; not ASCII 
	(if (r0 != ,leading-code-private-11)
	    ;; not Vietnamese
	    (write-read-repeat r0)
	  (read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
		   (;; Vietnamese lower
		    (read r0)
		    (r0 -= 128)
		    (write-read-repeat r0 ,(car viet-vscii-encode-table)))
		   (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		       (;; Vietnamese upper
			(read r0)
			(r0 -= 128)
			(write-read-repeat r0 ,(cdr viet-vscii-encode-table)))
		     ;; not Vietnamese
		     (write-read-repeat r0))))))))
  "CCL program to encode VSCII-1.")

(define-ccl-program ccl-encode-vscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-vscii-encode-table))
      (r1 = r1 ,(cdr viet-vscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VSCII-1 font.")


(make-coding-system
 'viscii 'ccl
 "VISCII 1.1 (Vietnamese)"
 `(mnemonic "VISCII"
   decode ccl-decode-viscii
   encode ccl-encode-viscii))

;; it is not correct, but XEmacs doesn't have `ccl' category...
(coding-system-put 'viscii 'category 'iso-8-1)

;; (make-coding-system
;;  'vietnamese-viscii 4 ?V
;;  "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)"
;;  '(ccl-decode-viscii . ccl-encode-viscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (mime-charset . viscii)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'viscii 'vietnamese-viscii)

(make-coding-system
 'vscii 'ccl
 "VSCII 1.1 (Vietnamese)"
 `(mnemonic "VSCII"
   decode ccl-decode-vscii
   encode ccl-encode-vscii))

;; (make-coding-system
;;  'vietnamese-vscii 4 ?v
;;  "8-bit encoding for Vietnamese VSCII-1"
;;  '(ccl-decode-vscii . ccl-encode-vscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'vscii 'vietnamese-vscii)

) ; (featurep 'ccl)

(make-coding-system
 'viqr 'no-conversion
 "VIQR (Vietnamese)"
 '(mnemonic "VIQR"
   eol-type lf
   post-read-conversion viqr-post-read-conversion
   pre-write-conversion viqr-pre-write-conversion))

;; (make-coding-system
;;  'vietnamese-viqr 0 ?q
;;  "Vietnamese latin transcription (VIQR)"
;;  nil
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (post-read-conversion . viqr-post-read-conversion)
;;    (pre-write-conversion . viqr-pre-write-conversion)
;;    (charset-origin-alist
;;     (vietnamese-viscii-lower "VISCII" viet-encode-viscii-char)
;;     (vietnamese-viscii-upper "VISCII" viet-encode-viscii-char))))

;; (define-coding-system-alias 'viqr 'vietnamese-viqr)

(when (featurep 'ccl)

;; For VISCII users
(set-charset-ccl-program 'vietnamese-viscii-lower
			 'ccl-encode-viscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper
			 'ccl-encode-viscii-font)
;; For VSCII users
(set-charset-ccl-program 'vietnamese-viscii-lower 'ccl-encode-vscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper 'ccl-encode-vscii-font)

;; (setq font-ccl-encoder-alist
;;       (cons (cons "viscii" ccl-encode-viscii-font) font-ccl-encoder-alist))

;; (setq font-ccl-encoder-alist
;;       (cons (cons "vscii" ccl-encode-vscii-font) font-ccl-encoder-alist))

) ; (featurep 'ccl)

(defvar viet-viscii-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length viet-viscii-decode-table)))
    (while (< i len)
      (let ((ch (aref viet-viscii-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their VISCII code.")
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/vietnamese.el~base.hkfFmQ
(eval-and-compile

(defvar viet-viscii-decode-table
  [;; VISCII is a full 8-bit code.
   0 1 ?,2F(B 3 4 ?,2G(B ?,2g(B 7 8 9 10 11 12 13 14 15
   16 17 18 19 ?,2V(B 21 22 23 24 ?,2[(B 26 27 28 29 ?,2\(B 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2U(B ?,2!(B ?,2"(B ?,2#(B ?,2$(B ?,2%(B ?,2&(B ?,2'(B ?,2((B ?,2)(B ?,2*(B ?,2+(B ?,2,(B ?,2-(B ?,2.(B ?,2/(B
   ?,20(B ?,21(B ?,22(B ?,25(B ?,2~(B ?,2>(B ?,26(B ?,27(B ?,28(B ?,2v(B ?,2w(B ?,2o(B ?,2|(B ?,2{(B ?,2x(B ?,2O(B
   ?,2u(B ?,1!(B ?,1"(B ?,1#(B ?,1$(B ?,1%(B ?,1&(B ?,1'(B ?,1((B ?,1)(B ?,1*(B ?,1+(B ?,1,(B ?,1-(B ?,1.(B ?,1/(B
   ?,10(B ?,11(B ?,12(B ?,2^(B ?,2=(B ?,15(B ?,16(B ?,17(B ?,18(B ?,2q(B ?,2Q(B ?,2W(B ?,2X(B ?,1=(B ?,1>(B ?,2_(B
   ?,2`(B ?,2a(B ?,2b(B ?,2c(B ?,2d(B ?,2e(B ?,1F(B ?,1G(B ?,2h(B ?,2i(B ?,2j(B ?,2k(B ?,2l(B ?,2m(B ?,2n(B ?,1O(B
   ?,2p(B ?,1Q(B ?,2r(B ?,2s(B ?,2t(B ?,1U(B ?,1V(B ?,1W(B ?,1X(B ?,2y(B ?,2z(B ?,1[(B ?,1\(B ?,2}(B ?,1^(B ?,1_(B
   ?,1`(B ?,1a(B ?,1b(B ?,1c(B ?,1d(B ?,1e(B ?,1f(B ?,1g(B ?,1h(B ?,1i(B ?,1j(B ?,1k(B ?,1l(B ?,1m(B ?,1n(B ?,1o(B
   ?,1p(B ?,1q(B ?,1r(B ?,1s(B ?,1t(B ?,1u(B ?,1v(B ?,1w(B ?,1x(B ?,1y(B ?,1z(B ?,1{(B ?,1|(B ?,1}(B ?,1~(B ?,2f(B ]
  "Vietnamese VISCII decoding table.")

(defvar viet-viscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (split-char (aref viet-viscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (nth 1 char-component) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (nth 1 char-component) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VISCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

(defvar viet-vscii-decode-table
  [;; VSCII is a full 8-bit code.
   0 ?,2z(B ?,2x(B 3 ?,2W(B ?,2X(B ?,2f(B 7 8 9 10 11 12 13 14 15
   16 ?,2Q(B ?,2_(B ?,2O(B ?,2V(B ?,2[(B ?,2}(B ?,2\(B 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2`(B ?,2d(B ?,2c(B ?,2a(B ?,2U(B ?,2#(B ?,2'(B ?,2h(B ?,2k(B ?,2((B ?,2i(B ?,2)(B ?,2.(B ?,2l(B ?,2o(B ?,2n(B
   ?,2m(B ?,28(B ?,2r(B ?,2v(B ?,2u(B ?,2s(B ?,2w(B ?,25(B ?,26(B ?,27(B ?,2^(B ?,2>(B ?,2~(B ?,2y(B ?,2|(B ?,2{(B
   160 ?,2e(B ?,2b(B ?,2j(B ?,2t(B ?,2=(B ?,2_(B ?,2p(B ?,1e(B ?,1b(B ?,1j(B ?,1t(B ?,1>(B ?,1y(B ?,1p(B ?,2"(B
   192 193 194 195 196 ?,1`(B ?,1d(B ?,1c(B ?,1a(B ?,1U(B ?,2F(B ?,1"(B ?,1F(B ?,1G(B ?,1!(B ?,2G(B
   ?,2!(B ?,2%(B ?,2&(B ?,2g(B ?,2%(B ?,2+(B ?,1#(B ?,1%(B ?,1&(B ?,1g(B ?,1$(B ?,1'(B ?,1h(B ?,2,(B ?,1k(B ?,1((B
   ?,1i(B ?,1)(B ?,1+(B ?,1,(B ?,1-(B ?,1*(B ?,1.(B ?,1l(B ?,1o(B ?,2-(B ?,2*(B ?,20(B ?,1n(B ?,1m(B ?,18(B ?,1r(B
   ?,21(B ?,1v(B ?,1u(B ?,1s(B ?,1w(B ?,10(B ?,11(B ?,12(B ?,1/(B ?,15(B ?,16(B ?,17(B ?,1^(B ?,1>(B ?,1~(B ?,1y(B
   ?,22(B ?,1|(B ?,1{(B ?,1z(B ?,1x(B ?,1W(B ?,1X(B ?,1f(B ?,1Q(B ?,1q(B ?,1O(B ?,1V(B ?,1[(B ?,1}(B ?,1\(B ?,2/(B]
  "Vietnamese VSCII decoding table.")

(defvar viet-vscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (split-char (aref viet-vscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (nth 1 char-component) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (nth 1 char-component) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VSCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

)

(define-ccl-program ccl-decode-viscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-viscii-decode-table))
     ))
  "CCL program to decode VISCII 1.1")

;; Multibyte form of a Vietnamese character is as follows (3-byte):
;;   LEADING-CODE-PRIVATE-11 LEADING-CODE-EXTENDED-11 POSITION-CODE
;; where LEADING-CODE-EXTENDED-11 for Vietnamese is
;; `vietnamese-viscii-lower' or `vietnamese-viscii-upper'.

(defvar leading-code-private-11 #x9E)

(define-ccl-program ccl-encode-viscii
  `(1
     ((read r0)
      (loop
       (if (r0 < 128)
	   ;; ASCII
	   (write-read-repeat r0)
	 ;; not ASCII
	 (if (r0 != ,leading-code-private-11)
	     ;; not Vietnamese
	     (write-read-repeat r0)
	   ((read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	     (;; Vietnamese lower
	      (read r0)
	      (r0 -= 128)
	      (write-read-repeat r0 ,(car viet-viscii-encode-table)))
	     (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		 (;; Vietnamese upper
		  (read r0)
		  (r0 -= 128)
		  (write-read-repeat r0 ,(cdr viet-viscii-encode-table)))
	       ;; not Vietnamese
	       (write-read-repeat r0)))))))))
  "CCL program to encode VISCII 1.1")

(define-ccl-program ccl-encode-viscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-viscii-encode-table))
      (r1 = r1 ,(cdr viet-viscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VISCII 1.1 font")

(define-ccl-program ccl-decode-vscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-vscii-decode-table))
     ))
  "CCL program to decode VSCII-1.")

(define-ccl-program ccl-encode-vscii
  `(1
    ((read r0)
     (loop
      (if (r0 < 128)
	  ;; ASCII
	  (write-read-repeat r0)
	;; not ASCII 
	(if (r0 != ,leading-code-private-11)
	    ;; not Vietnamese
	    (write-read-repeat r0)
	  (read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
		   (;; Vietnamese lower
		    (read r0)
		    (r0 -= 128)
		    (write-read-repeat r0 ,(car viet-vscii-encode-table)))
		   (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		       (;; Vietnamese upper
			(read r0)
			(r0 -= 128)
			(write-read-repeat r0 ,(cdr viet-vscii-encode-table)))
		     ;; not Vietnamese
		     (write-read-repeat r0))))))))
  "CCL program to encode VSCII-1.")

(define-ccl-program ccl-encode-vscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-vscii-encode-table))
      (r1 = r1 ,(cdr viet-vscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VSCII-1 font.")


(make-coding-system
 'viscii 'ccl
 "VISCII 1.1 (Vietnamese)"
 `(mnemonic "VISCII"
   decode ccl-decode-viscii
   encode ccl-encode-viscii))

;; it is not correct, but XEmacs doesn't have `ccl' category...
(coding-system-put 'viscii 'category 'iso-8-1)

;; (make-coding-system
;;  'vietnamese-viscii 4 ?V
;;  "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)"
;;  '(ccl-decode-viscii . ccl-encode-viscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (mime-charset . viscii)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'viscii 'vietnamese-viscii)

(make-coding-system
 'vscii 'ccl
 "VSCII 1.1 (Vietnamese)"
 `(mnemonic "VSCII"
   decode ccl-decode-vscii
   encode ccl-encode-vscii))

;; (make-coding-system
;;  'vietnamese-vscii 4 ?v
;;  "8-bit encoding for Vietnamese VSCII-1"
;;  '(ccl-decode-vscii . ccl-encode-vscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'vscii 'vietnamese-vscii)

(make-coding-system
 'viqr 'no-conversion
 "VIQR (Vietnamese)"
 '(mnemonic "VIQR"
   eol-type lf
   post-read-conversion viqr-post-read-conversion
   pre-write-conversion viqr-pre-write-conversion))

;; (make-coding-system
;;  'vietnamese-viqr 0 ?q
;;  "Vietnamese latin transcription (VIQR)"
;;  nil
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (post-read-conversion . viqr-post-read-conversion)
;;    (pre-write-conversion . viqr-pre-write-conversion)
;;    (charset-origin-alist
;;     (vietnamese-viscii-lower "VISCII" viet-encode-viscii-char)
;;     (vietnamese-viscii-upper "VISCII" viet-encode-viscii-char))))

;; (define-coding-system-alias 'viqr 'vietnamese-viqr)

;; For VISCII users
(set-charset-ccl-program 'vietnamese-viscii-lower
			 'ccl-encode-viscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper
			 'ccl-encode-viscii-font)
;; For VSCII users
(set-charset-ccl-program 'vietnamese-viscii-lower 'ccl-encode-vscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper 'ccl-encode-vscii-font)

;; (setq font-ccl-encoder-alist
;;       (cons (cons "viscii" ccl-encode-viscii-font) font-ccl-encoder-alist))

;; (setq font-ccl-encoder-alist
;;       (cons (cons "vscii" ccl-encode-vscii-font) font-ccl-encoder-alist))

(defvar viet-viscii-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length viet-viscii-decode-table)))
    (while (< i len)
      (let ((ch (aref viet-viscii-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their VISCII code.")
=======
(make-coding-system 
 'viscii 'fixed-width "VISCII 1.1 (Vietnamese)"
 '(unicode-map
   ((#x02 ?\u1EB2) ;; CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (#x05 ?\u1EB4) ;; CAPITAL LETTER A WITH BREVE AND TILDE
    (#x06 ?\u1EAA) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (#x14 ?\u1EF6) ;; CAPITAL LETTER Y WITH HOOK ABOVE
    (#x19 ?\u1EF8) ;; CAPITAL LETTER Y WITH TILDE
    (#x1E ?\u1EF4) ;; CAPITAL LETTER Y WITH DOT BELOW
    (#x80 ?\u1EA0) ;; CAPITAL LETTER A WITH DOT BELOW
    (#x81 ?\u1EAE) ;; CAPITAL LETTER A WITH BREVE AND ACUTE
    (#x82 ?\u1EB0) ;; CAPITAL LETTER A WITH BREVE AND GRAVE
    (#x83 ?\u1EB6) ;; CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (#x84 ?\u1EA4) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#x85 ?\u1EA6) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#x86 ?\u1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#x87 ?\u1EAC) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#x88 ?\u1EBC) ;; CAPITAL LETTER E WITH TILDE
    (#x89 ?\u1EB8) ;; CAPITAL LETTER E WITH DOT BELOW
    (#x8A ?\u1EBE) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#x8B ?\u1EC0) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#x8C ?\u1EC2) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#x8D ?\u1EC4) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (#x8E ?\u1EC6) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#x8F ?\u1ED0) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#x90 ?\u1ED2) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#x91 ?\u1ED4) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#x92 ?\u1ED6) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (#x93 ?\u1ED8) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#x94 ?\u1EE2) ;; CAPITAL LETTER O WITH HORN AND DOT BELOW
    (#x95 ?\u1EDA) ;; CAPITAL LETTER O WITH HORN AND ACUTE
    (#x96 ?\u1EDC) ;; CAPITAL LETTER O WITH HORN AND GRAVE
    (#x97 ?\u1EDE) ;; CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (#x98 ?\u1ECA) ;; CAPITAL LETTER I WITH DOT BELOW
    (#x99 ?\u1ECE) ;; CAPITAL LETTER O WITH HOOK ABOVE
    (#x9A ?\u1ECC) ;; CAPITAL LETTER O WITH DOT BELOW
    (#x9B ?\u1EC8) ;; CAPITAL LETTER I WITH HOOK ABOVE
    (#x9C ?\u1EE6) ;; CAPITAL LETTER U WITH HOOK ABOVE
    (#x9D ?\u0168) ;; CAPITAL LETTER U WITH TILDE
    (#x9E ?\u1EE4) ;; CAPITAL LETTER U WITH DOT BELOW
    (#x9F ?\u1EF2) ;; CAPITAL LETTER Y WITH GRAVE
    (#xA0 ?\u00D5) ;; CAPITAL LETTER O WITH TILDE
    (#xA1 ?\u1EAF) ;; SMALL LETTER A WITH BREVE AND ACUTE
    (#xA2 ?\u1EB1) ;; SMALL LETTER A WITH BREVE AND GRAVE
    (#xA3 ?\u1EB7) ;; SMALL LETTER A WITH BREVE AND DOT BELOW
    (#xA4 ?\u1EA5) ;; SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#xA5 ?\u1EA7) ;; SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#xA6 ?\u1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#xA7 ?\u1EAD) ;; SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#xA8 ?\u1EBD) ;; SMALL LETTER E WITH TILDE
    (#xA9 ?\u1EB9) ;; SMALL LETTER E WITH DOT BELOW
    (#xAA ?\u1EBF) ;; SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#xAB ?\u1EC1) ;; SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#xAC ?\u1EC3) ;; SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#xAD ?\u1EC5) ;; SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (#xAE ?\u1EC7) ;; SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#xAF ?\u1ED1) ;; SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#xB0 ?\u1ED3) ;; SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#xB1 ?\u1ED5) ;; SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#xB2 ?\u1ED7) ;; SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (#xB3 ?\u1EE0) ;; CAPITAL LETTER O WITH HORN AND TILDE
    (#xB4 ?\u01A0) ;; CAPITAL LETTER O WITH HORN
    (#xB5 ?\u1ED9) ;; SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#xB6 ?\u1EDD) ;; SMALL LETTER O WITH HORN AND GRAVE
    (#xB7 ?\u1EDF) ;; SMALL LETTER O WITH HORN AND HOOK ABOVE
    (#xB8 ?\u1ECB) ;; SMALL LETTER I WITH DOT BELOW
    (#xB9 ?\u1EF0) ;; CAPITAL LETTER U WITH HORN AND DOT BELOW
    (#xBA ?\u1EE8) ;; CAPITAL LETTER U WITH HORN AND ACUTE
    (#xBB ?\u1EEA) ;; CAPITAL LETTER U WITH HORN AND GRAVE
    (#xBC ?\u1EEC) ;; CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (#xBD ?\u01A1) ;; SMALL LETTER O WITH HORN
    (#xBE ?\u1EDB) ;; SMALL LETTER O WITH HORN AND ACUTE
    (#xBF ?\u01AF) ;; CAPITAL LETTER U WITH HORN
    (#xC0 ?\u00C0) ;; CAPITAL LETTER A WITH GRAVE
    (#xC1 ?\u00C1) ;; CAPITAL LETTER A WITH ACUTE
    (#xC2 ?\u00C2) ;; CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 ?\u00C3) ;; CAPITAL LETTER A WITH TILDE
    (#xC4 ?\u1EA2) ;; CAPITAL LETTER A WITH HOOK ABOVE
    (#xC5 ?\u0102) ;; CAPITAL LETTER A WITH BREVE
    (#xC6 ?\u1EB3) ;; SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (#xC7 ?\u1EB5) ;; SMALL LETTER A WITH BREVE AND TILDE
    (#xC8 ?\u00C8) ;; CAPITAL LETTER E WITH GRAVE
    (#xC9 ?\u00C9) ;; CAPITAL LETTER E WITH ACUTE
    (#xCA ?\u00CA) ;; CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB ?\u1EBA) ;; CAPITAL LETTER E WITH HOOK ABOVE
    (#xCC ?\u00CC) ;; CAPITAL LETTER I WITH GRAVE
    (#xCD ?\u00CD) ;; CAPITAL LETTER I WITH ACUTE
    (#xCE ?\u0128) ;; CAPITAL LETTER I WITH TILDE
    (#xCF ?\u1EF3) ;; SMALL LETTER Y WITH GRAVE
    (#xD0 ?\u0110) ;; CAPITAL LETTER D WITH STROKE
    (#xD1 ?\u1EE9) ;; SMALL LETTER U WITH HORN AND ACUTE
    (#xD2 ?\u00D2) ;; CAPITAL LETTER O WITH GRAVE
    (#xD3 ?\u00D3) ;; CAPITAL LETTER O WITH ACUTE
    (#xD4 ?\u00D4) ;; CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 ?\u1EA1) ;; SMALL LETTER A WITH DOT BELOW
    (#xD6 ?\u1EF7) ;; SMALL LETTER Y WITH HOOK ABOVE
    (#xD7 ?\u1EEB) ;; SMALL LETTER U WITH HORN AND GRAVE
    (#xD8 ?\u1EED) ;; SMALL LETTER U WITH HORN AND HOOK ABOVE
    (#xD9 ?\u00D9) ;; CAPITAL LETTER U WITH GRAVE
    (#xDA ?\u00DA) ;; CAPITAL LETTER U WITH ACUTE
    (#xDB ?\u1EF9) ;; SMALL LETTER Y WITH TILDE
    (#xDC ?\u1EF5) ;; SMALL LETTER Y WITH DOT BELOW
    (#xDD ?\u00DD) ;; CAPITAL LETTER Y WITH ACUTE
    (#xDE ?\u1EE1) ;; SMALL LETTER O WITH HORN AND TILDE
    (#xDF ?\u01B0) ;; SMALL LETTER U WITH HORN
    (#xE0 ?\u00E0) ;; SMALL LETTER A WITH GRAVE
    (#xE1 ?\u00E1) ;; SMALL LETTER A WITH ACUTE
    (#xE2 ?\u00E2) ;; SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 ?\u00E3) ;; SMALL LETTER A WITH TILDE
    (#xE4 ?\u1EA3) ;; SMALL LETTER A WITH HOOK ABOVE
    (#xE5 ?\u0103) ;; SMALL LETTER A WITH BREVE
    (#xE6 ?\u1EEF) ;; SMALL LETTER U WITH HORN AND TILDE
    (#xE7 ?\u1EAB) ;; SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (#xE8 ?\u00E8) ;; SMALL LETTER E WITH GRAVE
    (#xE9 ?\u00E9) ;; SMALL LETTER E WITH ACUTE
    (#xEA ?\u00EA) ;; SMALL LETTER E WITH CIRCUMFLEX
    (#xEB ?\u1EBB) ;; SMALL LETTER E WITH HOOK ABOVE
    (#xEC ?\u00EC) ;; SMALL LETTER I WITH GRAVE
    (#xED ?\u00ED) ;; SMALL LETTER I WITH ACUTE
    (#xEE ?\u0129) ;; SMALL LETTER I WITH TILDE
    (#xEF ?\u1EC9) ;; SMALL LETTER I WITH HOOK ABOVE
    (#xF0 ?\u0111) ;; SMALL LETTER D WITH STROKE
    (#xF1 ?\u1EF1) ;; SMALL LETTER U WITH HORN AND DOT BELOW
    (#xF2 ?\u00F2) ;; SMALL LETTER O WITH GRAVE
    (#xF3 ?\u00F3) ;; SMALL LETTER O WITH ACUTE
    (#xF4 ?\u00F4) ;; SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 ?\u00F5) ;; SMALL LETTER O WITH TILDE
    (#xF6 ?\u1ECF) ;; SMALL LETTER O WITH HOOK ABOVE
    (#xF7 ?\u1ECD) ;; SMALL LETTER O WITH DOT BELOW
    (#xF8 ?\u1EE5) ;; SMALL LETTER U WITH DOT BELOW
    (#xF9 ?\u00F9) ;; SMALL LETTER U WITH GRAVE
    (#xFA ?\u00FA) ;; SMALL LETTER U WITH ACUTE
    (#xFB ?\u0169) ;; SMALL LETTER U WITH TILDE
    (#xFC ?\u1EE7) ;; SMALL LETTER U WITH HOOK ABOVE
    (#xFD ?\u00FD) ;; SMALL LETTER Y WITH ACUTE
    (#xFE ?\u1EE3) ;; SMALL LETTER O WITH HORN AND DOT BELOW
    (#xFF ?\u1EEE)) ;; CAPITAL LETTER U WITH HORN AND TILDE
   mnemonic "VISCII"))
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/vietnamese.el~other.c7yjQY

(set-language-info-alist
 "Vietnamese" '((charset vietnamese-viscii-lower vietnamese-viscii-upper)
		(coding-system viscii)
		(coding-priority viscii)
		(locale "vietnamese" "vi")
                ;; Not available in packages. 
		;; (input-method . "vietnamese-viqr")
		(features viet-util)
		(sample-text . "Vietnamese (Ti,1*(Bng Vi,1.(Bt)	Ch,1`(Bo b,1U(Bn")
		(documentation . "\
For Vietnamese, Emacs uses special charsets internally.
They can be decoded from and encoded to VISCC, VSCII, and VIQR.
Current setting put higher priority to the coding system VISCII than VSCII.
If you prefer VSCII, please do: (prefer-coding-system 'vietnamese-vscii)")
		))

;;; vietnamese.el ends here
