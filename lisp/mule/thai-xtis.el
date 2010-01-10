;;; thai-xtis.el --- Support for Thai (XTIS) -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: TAKAHASHI Naoto <ntakahas@etl.go.jp>
;;         MORIOKA Tomohiko <tomo@etl.go.jp>
;; Created: 1998-03-27 for Emacs-20.3 by TAKAHASHI Naoto
;;	    1999-03-29 imported and modified for XEmacs	by MORIOKA Tomohiko

;; Keywords: mule, multilingual, Thai, XTIS

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

;; For Thai, the pre-composed character set proposed by
;; Virach Sornlertlamvanich <virach@links.nectec.or.th> is supported.

;;; Code:

;moved to mule-charset.el.
;(make-internal-charset 'thai-xtis "Precomposed Thai (XTIS by Virach)." ...

(make-internal-charset
 'thai-iso8859-11
 "Right-Hand Part of Latin/Thai Alphabet (ISO/IEC 8859-11)"
 '(dimension 1
   registries ["ISO8859-11"]
   chars 96
   ;final ?T @@#### What is the final byte for this?
   graphic 1
   unicode-map ("unicode/unicode-consortium/ISO8859/8859-11.TXT" #xA0)
   short-name "Thai (ISO8859-11)"
   long-name "RHP of Thai (ISO 8859-11)"))

(define-category ?x "Precomposed Thai character.")
(modify-category-entry 'thai-xtis ?x)

(when (featurep 'xemacs)
  (let ((deflist	'(;; chars	syntax
			  (((33 . 78) 80 82 83 (96 . 101) (112 . 121)) "w")
			  ((79 102 95 111 122 123) "_"))))
    (loop for (chars syntax) in deflist do
      (loop for ch in chars do
	(let (from to)
	  (if (consp ch)
	      (setq from (car ch) to (cdr ch))
	    (setq from ch to ch))
	  (loop for i from from to to do
	    (modify-syntax-entry (vector 'thai-xtis i) syntax))))))
  (put-charset-property 'thai-xtis 'preferred-coding-system 'tis-620)
  )

;; @@#### This entire file is bogus.  Do Thai the normal way.

;; This is the ccl-decode-thai-xtis automaton.
;;
;; "WRITE x y" == (insert (make-char 'thai-xtis x y))
;; "write x" == (insert x)
;; rx' == (tis620-to-thai-xtis-second-byte-bitpattern rx)
;; r3 == "no vower nor tone"
;; r4 == (charset-id 'thai-xtis)
;; 
;;          |               input (= r0)
;;   state  |--------------------------------------------
;;          |  consonant  |    vowel    |    tone
;; ---------+-------------+-------------+----------------
;;  r1 == 0 | r1 = r0     | WRITE r0,r3 | WRITE r0,r3
;;  r2 == 0 |             |             |
;; ---------+-------------+-------------+----------------
;;  r1 == C | WRITE r1,r3 | r2 = r0'    | WRITE r1,r3|r0'
;;  r2 == 0 | r1 = r0     |             | r1 = 0
;; ---------+-------------+-------------+----------------
;;  r1 == C | WRITE r1,r2 | WRITE r1,r2 | WRITE r1,r2|r0'
;;  r2 == V | r1 = r0     | WRITE r0,r3 | r1 = r2 = 0
;;          | r2 = 0      | r1 = r2 = 0 |
;; 
;; 
;;          |               input (= r0) 
;;   state  |-----------------------------------------
;;          |    symbol   |    ASCII    |     EOF
;; ---------+-------------+-------------+-------------
;;  r1 == 0 | WRITE r0,r3 | write r0    |
;;  r2 == 0 |             |             |
;; ---------+-------------+-------------+-------------
;;  r1 == C | WRITE r1,r3 | WRITE r1,r3 | WRITE r1,r3
;;  r2 == 0 | WRITE r0,r3 | write r0    |
;;          | r1 = 0      | r1 = 0      |
;; ---------+-------------+-------------+-------------
;;  r1 == C | WRITE r1,r2 | WRITE r1,r2 | WRITE r1,r2
;;  r2 == V | WRITE r0,r3 | write r0    |
;;          | r1 = r2 = 0 | r1 = r2 = 0 |


(eval-and-compile

;; input  : r5 = 1st byte, r6 = 2nd byte
;; Their values will be destroyed.
(define-ccl-program ccl-thai-xtis-write
  '(0
    ((r5 = ((r5 & #x7F) << 7))
     (r6 = ((r6 & #x7F) | r5))
     (write-multibyte-character r4 r6))))

(define-ccl-program ccl-thai-xtis-consonant
  '(0
    (if (r1 == 0)
	(r1 = r0)
      (if (r2 == 0)
	  ((r5 = r1) (r6 = r3) (call ccl-thai-xtis-write)
	   (r1 = r0))
	((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	 (r1 = r0)
	 (r2 = 0))))))

(define-ccl-program ccl-thai-xtis-vowel
  '(0
    ((if (r1 == 0)
	 ((r5 = r0) (r6 = r3) (call ccl-thai-xtis-write))
       ((if (r2 == 0)
	    (r2 = ((r0 - 204) << 3))
	  ((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	   (r5 = r0) (r6 = r3) (call ccl-thai-xtis-write)
	   (r1 = 0)
	   (r2 = 0))))))))

(define-ccl-program ccl-thai-xtis-vowel-d1
  '(0
    ((if (r1 == 0)
	 ((r5 = r0) (r6 = r3) (call ccl-thai-xtis-write))
       ((if (r2 == 0)
	    (r2 = #x38)
	  ((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	   (r5 = r0) (r6 = r3) (call ccl-thai-xtis-write)
	   (r1 = 0)
	   (r2 = 0))))))))

(define-ccl-program ccl-thai-xtis-vowel-ee
  '(0
    ((if (r1 == 0)
	 ((r5 = r0) (r6 = r3) (call ccl-thai-xtis-write))
       ((if (r2 == 0)
	    (r2 = #x78)
	  ((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	   (r5 = r0) (r6 = r3) (call ccl-thai-xtis-write)
	   (r1 = 0)
	   (r2 = 0))))))))

(define-ccl-program ccl-thai-xtis-tone
  '(0
    (if (r1 == 0)
	((r5 = r0) (r6 = r3) (call ccl-thai-xtis-write))
      (if (r2 == 0)
	  ((r5 = r1) (r6 = ((r0 - #xE6) | r3)) (call ccl-thai-xtis-write)
	   (r1 = 0))
	((r5 = r1) (r6 = ((r0 - #xE6) | r2)) (call ccl-thai-xtis-write)
	 (r1 = 0)
	 (r2 = 0))))))

(define-ccl-program ccl-thai-xtis-symbol
  '(0
    (if (r1 == 0)
	((r5 = r0) (r6 = r3) (call ccl-thai-xtis-write))
      (if (r2 == 0)
	  ((r5 = r1) (r6 = r3) (call ccl-thai-xtis-write)
	   (r5 = r0) (r6 = r3) (call ccl-thai-xtis-write)
	   (r1 = 0))
	((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	 (r5 = r0) (r6 = r3) (call ccl-thai-xtis-write)
	 (r1 = 0)
	 (r2 = 0))))))

(define-ccl-program ccl-thai-xtis-ascii
  '(0
    (if (r1 == 0)
	(write r0)
      (if (r2 == 0)
	  ((r5 = r1) (r6 = r3) (call ccl-thai-xtis-write)
	   (write r0)
	   (r1 = 0))
	((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write)
	 (write r0)
	 (r1 = 0)
	 (r2 = 0))))))

(define-ccl-program ccl-thai-xtis-eof
  '(0
    (if (r1 != 0)
	(if (r2 == 0)
	    ((r5 = r1) (r6 = r3) (call ccl-thai-xtis-write))
	  ((r5 = r1) (r6 = r2) (call ccl-thai-xtis-write))))))

(define-ccl-program ccl-decode-thai-xtis
  `(4
    ((read r0)
     (r1 = 0)
     (r2 = 0)
     (r3 = #x30)
     (r4 = ,(charset-id 'thai-xtis))
     (loop
      (if (r0 < 161)
	  (call ccl-thai-xtis-ascii)
	(branch (r0 - 161)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-consonant)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-vowel-d1)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		(call ccl-thai-xtis-vowel)
		nil
		nil
		nil
		nil
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-tone)
		(call ccl-thai-xtis-vowel-ee)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		(call ccl-thai-xtis-symbol)
		nil
		nil
		nil))
      (read r0)
      (repeat)))

    (call ccl-thai-xtis-eof)))

)

(defconst leading-code-private-21 #x9F)

(define-ccl-program ccl-encode-thai-xtis
  `(1
    ((read r0)
     (loop
      (if (r0 == ,leading-code-private-21)
	  ((read r1)
	   (if (r1 == ,(charset-id 'thai-xtis))
	       ((read r0)
		(write r0)
		(read r0)
		(r1 = (r0 & 7))
		(r0 = ((r0 - #xB0) >> 3))
		(if (r0 != 0)
		    (write r0 [0 209 212 213 214 215 216 217 218 238]))
		(if (r1 != 0)
		    (write r1 [0 231 232 233 234 235 236 237]))
		(read r0)
		(repeat))
	     ((write r0 r1)
	      (read r0)
	      (repeat))))
	(write-read-repeat r0))))))

(make-coding-system
 'tis-620 'ccl
 "TIS620 (Thai)"
 `(mnemonic "TIS620"
   decode ccl-decode-thai-xtis
   encode ccl-encode-thai-xtis
   safe-charsets (ascii thai-xtis)
   documentation "external=tis620, internal=thai-xtis"))
(coding-system-put 'tis-620 'category 'iso-8-1)

(set-language-info-alist
 "Thai-XTIS"
 '((charset thai-xtis)
   (coding-system tis-620 iso-2022-7bit)
   (tutorial . "TUTORIAL.th")
   (tutorial-coding-system . tis-620)
   (coding-priority tis-620 iso-2022-7bit)
   (sample-text . "$(?!:(B")
   (documentation . t)))

;; thai-xtis.el ends here.
