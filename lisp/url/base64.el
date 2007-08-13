;;; base64.el,v --- Base64 encoding functions
;; Author: wmperry
;; Created: 1996/04/22 15:08:08
;; Version: 1.7
;; Keywords: extensions

;;; LCD Archive Entry:
;;; base64.el|William M. Perry|wmperry@spry.com|
;;; Package for encoding/decoding base64 data (MIME)|
;;; 1996/04/22 15:08:08|1.7|Location Undetermined
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base 64 encoding functions
;;; This code was converted to lisp code by me from the C code in
;;; ftp://cs.utk.edu/pub/MIME/b64encode.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar base64-code-string
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Character set used for base64 decoding")

(defvar base64-decode-vector
  (let ((vec (make-vector 256 nil))
	(i 0)
	(case-fold-search nil))
    (while (< i 256)
      (aset vec i (string-match (regexp-quote (char-to-string i))
				base64-code-string))
      (setq i (1+ i)))
    vec))

(defvar base64-max-line-length 64)

;(defun b0 (x) (aref base64-code-string (logand (lsh x -18) 63)))
;(defun b1 (x) (aref base64-code-string (logand (lsh x -12) 63)))
;(defun b2 (x) (aref base64-code-string (logand (lsh x -6) 63)))
;(defun b3 (x) (aref base64-code-string (logand x 63)))

(defmacro b0 (x) (` (aref base64-code-string (logand (lsh (, x) -18) 63))))
(defmacro b1 (x) (` (aref base64-code-string (logand (lsh (, x) -12) 63))))
(defmacro b2 (x) (` (aref base64-code-string (logand (lsh (, x) -6) 63))))
(defmacro b3 (x) (` (aref base64-code-string (logand (, x) 63))))

(defun base64-encode (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns a string that is
broken into `base64-max-line-length' byte lines."
  (or str (setq str (buffer-string)))
  (let ((x (base64-encode-internal str))
	(y ""))
    (while (> (length x) base64-max-line-length)
      (setq y (concat y (substring x 0 base64-max-line-length) "\n")
	    x (substring x base64-max-line-length nil)))
    (setq y (concat y x))
    y))

(defun base64-encode-internal (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns the entire string,
not broken up into `base64-max-line-length' byte lines."
  (let (
	(word 0)			; The word to translate
	w1 w2 w3
	)
    (cond
     ((> (length str) 3)
      (concat
       (base64-encode-internal (substring str 0 3))
       (base64-encode-internal (substring str 3 nil))))
     ((= (length str) 3)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    w3 (aref str 2)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  (logand w3 255)))
      (format "%c%c%c%c" (b0 word) (b1 word) (b2 word) (b3 word)))
     ((= (length str) 2)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  0))
      (format "%c%c%c=" (b0 word) (b1 word) (b2 word)))
     ((= (length str) 1)
      (setq w1 (aref str 0)
	    word (logior
		  (lsh (logand w1 255) 16)
		  0))
      (format "%c%c==" (b0 word) (b1 word)))
     (t ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base64 decoding functions
;;; Most of the decoding code is courtesy Francesco Potorti`
;;; <F.Potorti@cnuce.cnr.it>
;;; this is much faster than my original code - thanks!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun base64-decode-region (beg end)
  (interactive "r")
  (barf-if-buffer-read-only)
  (let
      ((exchange (= (point) beg))
       (endchars 0)
       (list) (code))
    (goto-char beg)
    (while (< (point) end)
      (setq list (mapcar
		  (function
		   (lambda (c)
		     (cond
		      ((aref base64-decode-vector c))
		      ((char-equal c ?=)
		       (setq endchars (1+ endchars))
		       0)
		      (nil
		       (error
			"Character %c does not match Mime base64 coding" c)))))
		  (buffer-substring (point) (+ (point) 4))))
      (setq code (+ (nth 3 list) (lsh (nth 2 list) 6)
		    (lsh (nth 1 list) 12) (lsh (car list) 18)))
      (delete-char 4)
      (cond
       ((zerop endchars)
	(insert (% (lsh code -16) 256) (% (lsh code -8) 256) (% code 256)))
       ((= endchars 1)
	(insert (% (lsh code -16) 256) (% (lsh code -8) 256))
	(setq end (point)))
       ((= endchars 2)
	(insert (% (lsh code -16) 256))
	(setq end (point))))
      (if (char-equal (following-char) ?\n)
	  (progn (delete-char 1)
		 (setq end (- end 2)))
	(setq end (1- end))))
    ))
;    (if exchange
;	(exchange-point-and-mark))))

(defun base64-decode (st &optional nd)
  "Do base64 decoding on string STR and return the original string.
If given buffer positions, destructively decodes that area of the
current buffer."
  (let ((replace-p nil)
	(retval nil))
    (if (stringp st)
	nil
      (setq st (prog1
		   (buffer-substring st (or nd (point-max)))
		 (delete-region st (or nd (point-max))))
	    replace-p t))
    (setq retval
	  (save-excursion
	    (set-buffer (get-buffer-create " *b64decode*"))
	    (erase-buffer)
	    (insert st)
	    (goto-char (point-min))
	    (while (re-search-forward "\r*\n" nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (base64-decode-region (point-min) (point-max))
	    (buffer-string)))
    (if replace-p (insert retval))
    retval))

(provide 'base64)
