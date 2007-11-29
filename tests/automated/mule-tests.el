;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainers: Hrvoje Niksic <hniksic@xemacs.org>,
;;              Martin Buchholz <martin@xemacs.org>
;; Created: 1999
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

;; Test some Mule functionality (most of these remain to be written) .
;; See test-harness.el for instructions on how to run these tests.

;; This file will be (read)ed by a non-mule XEmacs, so don't use
;; literal non-Latin1 characters.  Use (make-char) instead.

(require 'bytecomp)

;;-----------------------------------------------------------------
;; Test whether all legal chars may be safely inserted to a buffer.
;;-----------------------------------------------------------------

(defun test-chars (&optional for-test-harness)
  "Insert all characters in a buffer, to see if XEmacs will crash.
This is done by creating a string with all the legal characters
in [0, 2^21) range, inserting it into the buffer, and checking
that the buffer's contents are equivalent to the string.

If FOR-TEST-HARNESS is specified, a temporary buffer is used, and
the Assert macro checks for correctness."
  (let ((max (expt 2 (if (featurep 'mule) 21 8)))
	(list nil)
	(i 0))
    (while (< i max)
      (and (not for-test-harness)
	   (zerop (% i 1000))
	   (message "%d" i))
      (and (int-char i)
	   ;; Don't aset to a string directly because random string
	   ;; access is O(n) under Mule.
	   (setq list (cons (int-char i) list)))
      (setq i (1+ i)))
    (let ((string (apply #'string (nreverse list))))
      (if for-test-harness
	  ;; For use with test-harness, use Assert and a temporary
	  ;; buffer.
	  (with-temp-buffer
	    (insert string)
	    (Assert (equal (buffer-string) string)))
	;; For use without test harness: use a normal buffer, so that
	;; you can also test whether redisplay works.
	(switch-to-buffer (get-buffer-create "test"))
	(erase-buffer)
	(buffer-disable-undo)
	(insert string)
	(assert (equal (buffer-string) string))))))

;; It would be really *really* nice if test-harness allowed a way to
;; run a test in byte-compiled mode only.  It's tedious to have
;; time-consuming tests like this one run twice, once interpreted and
;; once compiled, for no good reason.
(test-chars t)

(defun unicode-code-point-to-utf-8-string (code-point)
  "Convert a Unicode code point to the equivalent UTF-8 string. 
This is a naive implementation in Lisp.  "
  (check-argument-type 'natnump code-point)
  (check-argument-range code-point 0 #x1fffff)
  (if (< code-point #x80)
      (format "%c" code-point)
    (if (< code-point #x800)
	(format "%c%c" 
		;; ochars[0] = 0xC0 | (input & ~(0xFFFFF83F)) >> 6;
		(logior #xc0 (lsh (logand code-point #x7c0) -6))
		;; ochars[1] = 0x80 | input & ~(0xFFFFFFC0);
		(logior #x80 (logand code-point #x3f)))
      (if (< code-point #x00010000)
	  (format "%c%c%c" 
		  ;; ochars[0] = 0xE0 | (input >> 12) & ~(0xFFFFFFF0); 
		  (logior #xe0 (logand (lsh code-point -12) #x0f))
		  ;; ochars[1] = 0x80 | (input >> 6) & ~(0xFFFFFFC0); 
		  (logior #x80 (logand (lsh code-point -6) #x3f))
		  ;; ochars[2] = 0x80 | input & ~(0xFFFFFFC0); 
		  (logior #x80 (logand code-point #x3f)))
	(if (< code-point #x200000)
	    (format "%c%c%c%c" 
		    ;; ochars[0] = 0xF0 | (input >> 18) & ~(0xFFFFFFF8)
		    (logior #xF0 (logand (lsh code-point -18) #x7))
		    ;; ochars[1] = 0x80 | (input >> 12) & ~(0xFFFFFFC0);
		    (logior #x80 (logand (lsh code-point -12) #x3f))
		    ;; ochars[2] = 0x80 | (input >> 6) & ~(0xFFFFFFC0); 
		    (logior #x80 (logand (lsh code-point -6) #x3f))
		    ;; ochars[3] = 0x80 | input & ~(0xFFFFFFC0); 
		    (logior #x80 (logand code-point #x3f))))))))

;;----------------------------------------------------------------
;; Test that revert-buffer resets the modiff
;; Bug reported 2007-06-20 <200706201902.32191.scop@xemacs.org>.
;; Fixed 2007-06-22 <18043.2793.611745.734215@parhasard.net>.
;;----------------------------------------------------------------

(let ((test-file-name
       ;; The Gnus people, when they call #'make-temp-name, then loop,
       ;; checking if the corresponding file exists. Our #'make-temp-name
       ;; already does this loop, and the Gnus approach doesn't bring
       ;; anything; there remains a race condition if you can predict the
       ;; path name. The path name in question depends on the process ID and
       ;; a (weak) PRNG seeded with the seconds to the power of the
       ;; milliseconds of some instant close to the startup time of this
       ;; XEmacs; without being able to read the address space of this
       ;; XEmacs, or monitor what stat() calls it does, it is not predictable.
       ;;
       ;; The really kosher way to do this is to merge GNU's make-temp-file
       ;; and use that. It basically has the functionality of the Unix
       ;; mkstemp.
       (make-temp-name (expand-file-name "tXfXsKc" (temp-directory))))
      revert-buffer-function
      kill-buffer-hook)		; paranoia
  (find-file test-file-name)
  (erase-buffer)
  (insert "a string\n")
  (Silence-Message (save-buffer 0))
  (insert "more text\n")
  (revert-buffer t t)
  ;; Just "find-file" with autodetect coding didn't fail for me, but it does
  ;; fail under test harness.  Still we'll redo the test with an explicit
  ;; coding system just in case.
  (Assert (not (buffer-modified-p)))
  (kill-buffer nil)
  (when (find-coding-system 'utf-8)
    (find-file test-file-name 'utf-8)
    (insert "more text\n")
    (revert-buffer t t)
    (Assert (not (buffer-modified-p)))
    (kill-buffer nil))
  (delete-file test-file-name))

;;-----------------------------------------------------------------
;; Test string modification functions that modify the length of a char.
;;-----------------------------------------------------------------

(when (featurep 'mule)
  ;;---------------------------------------------------------------
  ;; Test fillarray
  ;;---------------------------------------------------------------
  (macrolet
      ((fillarray-test
	(charset1 charset2)
	(let ((char1 (make-char charset1 69))
	      (char2 (make-char charset2 69)))
	  `(let ((string (make-string 1000 ,char1)))
	     (fillarray string ,char2)
	     (Assert (eq (aref string 0) ,char2))
	     (Assert (eq (aref string (1- (length string))) ,char2))
	     (Assert (eq (length string) 1000))))))
    (fillarray-test ascii latin-iso8859-1)
    (fillarray-test ascii latin-iso8859-2)
    (fillarray-test latin-iso8859-1 ascii)
    (fillarray-test latin-iso8859-2 ascii))

  ;; Test aset
  (let ((string (string (make-char 'ascii 69) (make-char 'latin-iso8859-2 69))))
    (aset string 0 (make-char 'latin-iso8859-2 42))
    (Assert (eq (aref string 1) (make-char 'latin-iso8859-2 69))))

  ;;---------------------------------------------------------------
  ;; Test coding system functions
  ;;---------------------------------------------------------------

  ;; Create alias for coding system without subsidiaries
  (Assert (coding-system-p (find-coding-system 'binary)))
  (Assert (coding-system-canonical-name-p 'binary))
  (Assert (not (coding-system-alias-p 'binary)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Check-Error-Message
   error "Symbol is the canonical name of a coding system and cannot be redefined"
   (define-coding-system-alias 'binary 'iso8859-2))
  (Check-Error-Message
   error "Symbol is not a coding system alias"
   (coding-system-aliasee 'binary))

  (define-coding-system-alias 'mule-tests-alias 'binary)
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'binary (coding-system-aliasee 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-mac)))

  (define-coding-system-alias 'mule-tests-alias (get-coding-system 'binary))
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'binary (coding-system-aliasee 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-mac)))

  (define-coding-system-alias 'nested-mule-tests-alias 'mule-tests-alias)
  (Assert (coding-system-alias-p 'nested-mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'nested-mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'nested-mule-tests-alias)))
  (Assert (eq (coding-system-aliasee 'nested-mule-tests-alias) 'mule-tests-alias))
  (Assert (eq 'mule-tests-alias (coding-system-aliasee 'nested-mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-mac)))

  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'nested-mule-tests-alias))
  (Check-Error-Message
   error "No such coding system"
   (define-coding-system-alias 'no-such-coding-system 'no-such-coding-system))
  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'mule-tests-alias))

  (define-coding-system-alias 'nested-mule-tests-alias nil)
  (define-coding-system-alias 'mule-tests-alias nil)
  (Assert (coding-system-p (find-coding-system 'binary)))
  (Assert (coding-system-canonical-name-p 'binary))
  (Assert (not (coding-system-alias-p 'binary)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Check-Error-Message
   error "Symbol is the canonical name of a coding system and cannot be redefined"
   (define-coding-system-alias 'binary 'iso8859-2))
  (Check-Error-Message
   error "Symbol is not a coding system alias"
   (coding-system-aliasee 'binary))

  (define-coding-system-alias 'nested-mule-tests-alias nil)
  (define-coding-system-alias 'mule-tests-alias nil)

  ;; Create alias for coding system with subsidiaries
  (define-coding-system-alias 'mule-tests-alias 'iso-8859-7)
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'iso-8859-7 (coding-system-aliasee 'mule-tests-alias)))
  (Assert (coding-system-alias-p 'mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'mule-tests-alias-mac))

  (define-coding-system-alias 'mule-tests-alias (get-coding-system 'iso-8859-7))
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'iso-8859-7 (coding-system-aliasee 'mule-tests-alias)))
  (Assert (coding-system-alias-p 'mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'mule-tests-alias-mac))
  (Assert (eq (find-coding-system 'mule-tests-alias-mac)
	      (find-coding-system 'iso-8859-7-mac)))

  (define-coding-system-alias 'nested-mule-tests-alias 'mule-tests-alias)
  (Assert (coding-system-alias-p 'nested-mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'nested-mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7)
	      (get-coding-system 'nested-mule-tests-alias)))
  (Assert (eq (coding-system-aliasee 'nested-mule-tests-alias) 'mule-tests-alias))
  (Assert (eq 'mule-tests-alias (coding-system-aliasee 'nested-mule-tests-alias)))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-mac))
  (Assert (eq (find-coding-system 'nested-mule-tests-alias-unix)
	      (find-coding-system 'iso-8859-7-unix)))

  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'nested-mule-tests-alias))
  (Check-Error-Message
   error "No such coding system"
   (define-coding-system-alias 'no-such-coding-system 'no-such-coding-system))
  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'mule-tests-alias))

  ;; Test dangling alias deletion
  (define-coding-system-alias 'mule-tests-alias nil)
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-dos)))

  ;;---------------------------------------------------------------
  ;; Test strings waxing and waning across the 8k BIG_STRING limit (see alloc.c)
  ;;---------------------------------------------------------------
  (defun charset-char-string (charset)
    (let (lo hi string n (gc-cons-threshold most-positive-fixnum))
      (if (= (charset-chars charset) 94)
	  (setq lo 33 hi 126)
	(setq lo 32 hi 127))
      (if (= (charset-dimension charset) 1)
	  (progn
	    (setq string (make-string (1+ (- hi lo)) ??))
	    (setq n 0)
	    (loop for j from lo to hi do
	      (progn
		(aset string n (make-char charset j))
		(incf n)))
	    (garbage-collect)
	    string)
	(progn
	  (setq string (make-string (* (1+ (- hi lo)) (1+ (- hi lo))) ??))
	  (setq n 0)
	  (loop for j from lo to hi do
	    (loop for k from lo to hi do
	      (progn
		(aset string n (make-char charset j k))
		(incf n))))
	  (garbage-collect)
	  string))))

  ;; The following two used to crash xemacs!
  (Assert (charset-char-string 'japanese-jisx0208))
  (aset (make-string 9003 ??) 1 (make-char 'latin-iso8859-1 77))

  (let ((greek-string (charset-char-string 'greek-iso8859-7))
	(string (make-string (* 96 60) ??)))
    (loop for j from 0 below (length string) do
      (aset string j (aref greek-string (mod j 96))))
    (loop for k in '(0 1 58 59) do
      (Assert (equal (substring string (* 96 k) (* 96 (1+ k))) greek-string))))

  (let ((greek-string (charset-char-string 'greek-iso8859-7))
	(string (make-string (* 96 60) ??)))
   (loop for j from (1- (length string)) downto 0 do
     (aset string j (aref greek-string (mod j 96))))
   (loop for k in '(0 1 58 59) do
     (Assert (equal (substring string (* 96 k) (* 96 (1+ k))) greek-string))))

  (let ((ascii-string (charset-char-string 'ascii))
	(string (make-string (* 94 60) (make-char 'greek-iso8859-7 57))))
   (loop for j from 0 below (length string) do
      (aset string j (aref ascii-string (mod j 94))))
    (loop for k in '(0 1 58 59) do
      (Assert (equal (substring string (* 94 k) (+ 94 (* 94 k))) ascii-string))))

  (let ((ascii-string (charset-char-string 'ascii))
	(string (make-string (* 94 60) (make-char 'greek-iso8859-7 57))))
    (loop for j from (1- (length string)) downto 0 do
      (aset string j (aref ascii-string (mod j 94))))
    (loop for k in '(0 1 58 59) do
      (Assert (equal (substring string (* 94 k) (* 94 (1+ k))) ascii-string))))

  ;;---------------------------------------------------------------
  ;; Test file-system character conversion (and, en passant, file ops)
  ;;---------------------------------------------------------------
  (let* ((dstroke (make-char 'latin-iso8859-2 80))
	 (latin2-string (make-string 4 dstroke))
	 (prefix (concat (file-name-as-directory
			  (file-truename (temp-directory)))
			 latin2-string))
	 (file-name-coding-system
	  ;; 'iso-8859-X doesn't work on darwin (as of "Panther" 10.3), it
	  ;; seems to know that file-name-coding-system is definitely utf-8
	  (if (string-match "darwin" system-configuration)
	      'utf-8
	    'iso-8859-2))
         ;; make-temp-name does stat(), which on OS X requires that you
         ;; normalise, where open() will normalise for you. Previously we
         ;; used scaron as the Latin-2 character, and make-temp-name errored
         ;; on OS X. LATIN CAPITAL LETTER D WITH STROKE does not decompose.
         (name1 (make-temp-name prefix))
         (name2 (make-temp-name prefix)))
    ;; This is how you suppress output from `message', called by `write-region'
    (Assert (not (equal name1 name2)))
    (Assert (not (file-exists-p name1)))
    (Silence-Message
     (write-region (point-min) (point-max) name1))
    (Assert (file-exists-p name1))
    (when (fboundp 'make-symbolic-link)
      (make-symbolic-link name1 name2)
      (Assert (file-exists-p name2))
      (Assert (equal (file-truename name2) name1))
      (Assert (equal (file-truename name1) name1)))
    (ignore-file-errors (delete-file name1) (delete-file name2)))

  ;; Add many more file operation tests here...

  ;;---------------------------------------------------------------
  ;; Test Unicode-related functions
  ;;---------------------------------------------------------------
  (let* ((scaron (make-char 'latin-iso8859-2 57)))
    ;; Used to try #x0000, but you can't change ASCII or Latin-1
    (loop for code in '(#x0100 #x2222 #x4444 #xffff) do
      (progn
	(set-unicode-conversion scaron code)
	(Assert (eq code (char-to-unicode scaron)))
	(Assert (eq scaron (unicode-to-char code '(latin-iso8859-2))))))
  
    (Check-Error wrong-type-argument (set-unicode-conversion scaron -10000)))

  (dolist (utf-8-char 
	   '("\xc6\x92"		  ;; U+0192 LATIN SMALL LETTER F WITH HOOK
	     "\xe2\x81\x8a"	  ;; U+204A TIRONIAN SIGN ET
	     "\xe2\x82\xae"	  ;; U+20AE TUGRIK SIGN
	     "\xf0\x9d\x92\xbd"	  ;; U+1D4BD MATHEMATICAL SCRIPT SMALL H
	     "\xf0\x9d\x96\x93"   ;; U+1D593 MATHEMATICAL BOLD FRAKTUR SMALL N
	     "\xf0\xaf\xa8\x88"   ;; U+2FA08 CJK COMPATIBILITY FOR U+4BCE
	     "\xf4\x8f\xbf\xbd")) ;; U+10FFFD <Plane 16 Private Use, Last>
    (let* ((xemacs-character (car (append 
				  (decode-coding-string utf-8-char 'utf-8) 
				  nil)))
	   (xemacs-charset (car (split-char xemacs-character))))

      ;; Trivial test of the UTF-8 support of the escape-quoted character set. 
      (Assert (equal (decode-coding-string utf-8-char 'utf-8)
		     (decode-coding-string (concat "\033%G" utf-8-char)
					   'escape-quoted)))

      ;; Check that the reverse mapping holds. 
      (Assert (equal (unicode-code-point-to-utf-8-string 
		      (encode-char xemacs-character 'ucs))
		     utf-8-char))

      ;; Check that, if this character has been JIT-allocated, it is encoded
      ;; in escape-quoted using the corresponding UTF-8 escape. 
      (when (charset-property xemacs-charset 'encode-as-utf-8)
	(Assert (equal (concat "\033%G" utf-8-char)
		       (encode-coding-string xemacs-character 'escape-quoted)))
	(Assert (equal (concat "\033%G" utf-8-char)
		       (encode-coding-string xemacs-character 'ctext))))))

  (loop
    for (code-point encoded) 
    in '((#x10000 "\xd8\x00\xdc\x00")
         (#x10FFFD "\xdb\xff\xdf\xfd"))
    do (Assert (equal (encode-coding-string 
                       (decode-char 'ucs code-point) 'utf-16)
                      encoded)))
         
  ;;---------------------------------------------------------------
  ;; Regression test for a couple of CCL-related bugs. 
  ;;---------------------------------------------------------------

  (let ((ccl-vector [0 0 0 0 0 0 0 0 0]))
    (define-ccl-program ccl-write-two-control-1-chars 
      `(1 
	((r0 = ,(charset-id 'control-1))
	 (r1 = 0) 
	 (write-multibyte-character r0 r1) 
	 (r1 = 31) 
	 (write-multibyte-character r0 r1))) 
      "CCL program that writes two control-1 multibyte characters.") 
 
    (Assert (equal 
	     (ccl-execute-on-string 'ccl-write-two-control-1-chars  
				    ccl-vector "") 
	     (format "%c%c" (make-char 'control-1 0) 
		     (make-char 'control-1 31))))

    (define-ccl-program ccl-unicode-two-control-1-chars 
      `(1 
	((r0 = ,(charset-id 'control-1))
	 (r1 = 31) 
	 (mule-to-unicode r0 r1) 
	 (r4 = r0) 
	 (r3 = ,(charset-id 'control-1))
	 (r2 = 0) 
	 (mule-to-unicode r3 r2))) 
      "CCL program that writes two control-1 UCS code points in r3 and r4")

    ;; Re-initialise the vector, mainly to clear the instruction counter,
    ;; which is its last element.
    (setq ccl-vector [0 0 0 0 0 0 0 0 0])
 
    (ccl-execute-on-string 'ccl-unicode-two-control-1-chars ccl-vector "") 
 
    (Assert (and (eq (aref ccl-vector 3)  
                   (encode-char (make-char 'control-1 0) 'ucs)) 
               (eq (aref ccl-vector 4)  
                   (encode-char (make-char 'control-1 31) 'ucs)))))


  ;; Test the 8 bit fixed-width coding systems for round-trip
  ;; compatibility with themselves.
  (loop
    for coding-system in (coding-system-list)
    with all-possible-octets = (apply #'string
				      (loop for i from ?\x00 to ?\xFF
					collect i))
    do
    (when (and (coding-system-get coding-system '8-bit-fixed)
	       ;; Don't check the coding systems with autodetect, they are
	       ;; not round-trip compatible for the possible line-ending
	       ;; characters.
	       (string-match #r"-\(unix\|dos\|mac\)$"
			     (symbol-name coding-system)))
      ;; These coding systems are round-trip compatible with themselves.
      (Assert (equal (encode-coding-string 
		      (decode-coding-string all-possible-octets
					    coding-system)
		      coding-system)
		     all-possible-octets))))

  ;;---------------------------------------------------------------
  ;; Test charset-in-* functions
  ;;---------------------------------------------------------------
  (with-temp-buffer
    (insert-file-contents (locate-data-file "HELLO"))
    (Assert (equal 
             ;; The sort is to make the algorithm of charsets-in-region
             ;; irrelevant.
             (sort (charsets-in-region (point-min) (point-max))
                   #'string<)
             '(arabic-1-column arabic-2-column ascii chinese-big5-1
               chinese-gb2312 cyrillic-iso8859-5 ethiopic greek-iso8859-7
               hebrew-iso8859-8 japanese-jisx0208 japanese-jisx0212
               katakana-jisx0201 korean-ksc5601 latin-iso8859-1
               latin-iso8859-2 thai-xtis vietnamese-viscii-lower)))
    (Assert (equal 
             (sort (charsets-in-string (buffer-substring (point-min)
							 (point-max)))
                   #'string<)
             '(arabic-1-column arabic-2-column ascii chinese-big5-1
               chinese-gb2312 cyrillic-iso8859-5 ethiopic greek-iso8859-7
               hebrew-iso8859-8 japanese-jisx0208 japanese-jisx0212
               katakana-jisx0201 korean-ksc5601 latin-iso8859-1
               latin-iso8859-2 thai-xtis vietnamese-viscii-lower))))

  ;;---------------------------------------------------------------
  ;; Language environments, and whether the specified values are sane.
  ;;---------------------------------------------------------------
  (loop
    for language in (mapcar #'car language-info-alist)
    with language-input-method = nil
    do
    ;; s-l-e can call #'require, which says "Loading ..."
    (Silence-Message (set-language-environment language))
    (Assert (equal language current-language-environment))

    (setq language-input-method
	  (get-language-info language 'input-method))
    (when (and language-input-method
               ;; #### Not robust, if more input methods besides canna are
               ;; in core.  The intention of this is that if *any* of the
               ;; packages' input methods are available, we check that *all*
               ;; of the language environments' input methods actually
               ;; exist, which goes against the spirit of non-monolithic
               ;; packages. But I don't have a better approach to this.
               (> (length input-method-alist) 1))
      (Assert (assoc language-input-method input-method-alist))
      (Skip-Test-Unless
       (assoc language-input-method input-method-alist)
       "input method unavailable"
       (format "check that IM %s can be activated" language-input-method)
       ;; s-i-m can load files.
       (Silence-Message
	(set-input-method language-input-method))
       (Assert (equal language-input-method current-input-method))))

    (dolist (charset (get-language-info language 'charset))
      (Assert (charsetp (find-charset charset))))
    (dolist (coding-system (get-language-info language 'coding-system))
      (Assert (coding-system-p (find-coding-system coding-system))))
    (dolist (coding-system (get-language-info language
                                              'native-coding-system))
      ;; We don't have the appropriate POSIX locales to test with a
      ;; native-coding-system that is a function.
      (unless (functionp coding-system)
	(Assert (coding-system-p (find-coding-system coding-system))))))

  (with-temp-buffer
    (flet
        ((Assert-elc-is-escape-quoted ()
           "Assert the current buffer has an escape-quoted cookie if compiled."
           (save-excursion
             (let ((byte-compile-result (byte-compile-from-buffer
                                         (current-buffer) nil nil))
                   (temporary-file-name (make-temp-name
                                         (expand-file-name "zjPQ2Pk"
                                                           (temp-directory)))))
               (byte-compile-insert-header
                temporary-file-name
                (current-buffer)
                byte-compile-result)
               (Assert (string-match
                        "^;;;###coding system: escape-quoted"
                        (buffer-substring nil nil byte-compile-result))))))
         (Assert-elc-has-no-specified-encoding ()
           "Assert the current buffer has no coding cookie if compiled."
           (save-excursion
             (let ((byte-compile-result (byte-compile-from-buffer
                                         (current-buffer) nil nil))
                   (temporary-file-name (make-temp-name
                                         (expand-file-name "zjPQ2Pk"
                                                           (temp-directory)))))
               (byte-compile-insert-header
                temporary-file-name
                (current-buffer)
                byte-compile-result)
               (Assert (not (string-match
                             ";;;###coding system:"
                             (buffer-substring nil nil
                                               byte-compile-result))))))))
      (insert 
       ;; Create a buffer with Unicode escapes. The #'read call is at
       ;; runtime, because this file may be compiled and read in a non-Mule
       ;; XEmacs. (But it won't be run.)
       (read 
        "#r\" (defvar testing-mule-compilation-handling 
            (string ?\\u371E   ;; kDefinition beautiful; pretty, used 
                              ;; in girl's name
                ?\\U0002A6A9   ;; kDefinition	(Cant.) sound of shouting
                ?\\U0002A65B   ;; kDefinition	(Cant.) decayed teeth; 
                              ;; tongue-tied
                ?\\U00010400   ;; DESERET CAPITAL LETTER LONG I
                    ?\\u3263)) ;; CIRCLED HANGUL RIEUL \""))

      (Assert-elc-is-escape-quoted)
      (delete-region (point-min) (point-max))

      (insert
       ;; This time, the buffer will contain the actual characters, because of
       ;; u flag to the #r. 
       (read 
        "#ru\" (defvar testing-mule-compilation-handling 
            (string ?\\u371E   ;; kDefinition beautiful; pretty, used 
                              ;; in girl's name
                ?\\U0002A6A9   ;; kDefinition	(Cant.) sound of shouting
                ?\\U0002A65B   ;; kDefinition	(Cant.) decayed teeth; 
                              ;; tongue-tied
                ?\\U00010400   ;; DESERET CAPITAL LETTER LONG I
                    ?\\u3263)) ;; CIRCLED HANGUL RIEUL \""))
    
      (Assert-elc-is-escape-quoted)
      (delete-region (point-min) (point-max))

      (insert
       ;; Just a single four character escape. 
       (read
        "#r\" (defvar testing-mule-compilation-handling 
            (string ?\\u371E))   ;; kDefinition beautiful; pretty, used\""))

      (Assert-elc-is-escape-quoted)
      (delete-region (point-min) (point-max))

      (insert
       ;; Just a single eight character escape. 
       (read 
        "#r\" (defvar testing-mule-compilation-handling 
            (string ?\\U0002A65B))   ;; kDefinition (Cant.) decayed teeth;\""))

      (Assert-elc-is-escape-quoted)
      (delete-region (point-min) (point-max))

      (insert
       ;; A single latin-1 hex digit escape No run-time #'read call,
       ;; non-Mule can handle this too.
       #r" (defvar testing-mule-compilation-handling 
         (string ?\xab))   ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK")
      
      (Assert-elc-has-no-specified-encoding)
      (delete-region (point-min) (point-max))

      (insert
       ;; A single latin-1 character. No run-time #'read call.
       #ru" (defvar testing-mule-compilation-handling 
        (string ?\u00AB))   ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK\")")
      
      (Assert-elc-has-no-specified-encoding)
      (delete-region (point-min) (point-max))

      (insert
       ;; Just ASCII. No run-time #'read call
       #r" (defvar testing-mule-compilation-handling 
            (string ?A))   ;; LATIN CAPITAL LETTER A")
      
      (Assert-elc-has-no-specified-encoding)
      (delete-region (point-min) (point-max))

      ;; This bug exists because the coding-cookie insertion code looks at
      ;; the input buffer, not the output buffer.
      ;;
      ;; It looks at the input buffer because byte-compile-dynamic and
      ;; byte-compile-dynamic-docstrings currently need to be
      ;; unconditionally turned off for Mule files, since dynamic
      ;; compilation of function bodies and docstrings fails if you can't
      ;; call (point) and trivially get the byte offset in the file.
      ;;
      ;; And to unconditionally turn those two features off, you need to
      ;; know before byte-compilation whether the byte-compilation output
      ;; file contains non-Latin-1 characters, or perhaps to check after
      ;; compilation and redo; but we don't do the latter.
      ;; 
      ;; To fix this bug, we need to add Mule support to
      ;; byte-compile-dynamic and byte-compile-dynamic-docstrings. Or drop
      ;; support for those features entirely.
      (insert
       "(defvar testing-mule-compilation-handling (eval-when-compile
	(decode-char 'ucs #x371e))) ;; kDefinition beautiful; pretty, used\"")
      (Known-Bug-Expect-Failure
       (Assert-elc-is-escape-quoted))
      (delete-region (point-min) (point-max))))
  )
