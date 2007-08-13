;;;
;;; $Id: tl-num.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
;;;
;;; by MORIOKA Tomohiko <morioka@jaist.ac.jp>, 1993/10/4
;;;

(require 'emu)
(require 'tl-seq)


;;; @ n base
;;;

(defun char-to-int (chr)
  "Convert n base character CHR to integer (n <= 36). [tl-num]"
  (cond ((and (<= ?0 chr)(<= chr ?9)) (- chr ?0))
	((and (<= ?A chr)(<= chr ?Z)) (+ (- chr ?A) 10))
	((and (<= ?a chr)(<= chr ?z)) (+ (- chr ?a) 10))
	))

(defun int-to-char (n)
  "Convert integer N to n base character (n <= 36). [tl-num]"
  (if (< n 10)
      (+ ?0 n)
    (+ ?A (- n 10))
    ))

(defun base-seq-to-int (base seq)
  "Convert n base number sequence SEQ to number. [tl-num]"
  (foldl (function
	  (lambda (n m)
	    (+ (* n base) m)
	    ))
	 0 seq))

(defun base-char-seq-to-int (base seq)
  "Convert n base char sequence SEQ to number. [tl-num]"
  (foldl (function
	  (lambda (n chr)
	    (+ (* n base)(char-to-int chr))
	    ))
	 0 seq))

   
;;; @ Hex
;;;

(defun hex-char-to-number (chr)
  "Convert hex character CHR to number. [tl-num]"
  (cond ((and (<= ?0 chr)(<= chr ?9)) (- chr ?0))
	((and (<= ?A chr)(<= chr ?F)) (+ (- chr ?A) 10))
	((and (<= ?a chr)(<= chr ?f)) (+ (- chr ?a) 10))
	))

(defalias 'number-to-hex-char 'int-to-char)

(defun hex-seq-to-int (seq)
  "Convert hex number sequence SEQ to integer. [tl-num]"
  (base-seq-to-int 16 seq)
  )

(defun hex-char-seq-to-int (seq)
  "Convert hex char sequence SEQ to integer. [tl-num]"
  (base-char-seq-to-int 16 seq)
  )


;;; @ end
;;;

(provide 'tl-num)
