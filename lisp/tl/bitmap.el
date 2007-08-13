;; bitmap.el -- bitmap (xbm) file handler on Mule

;; Copyright (C) 1992 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1996 UENO Hiroshi
;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: Ken'ichi HANDA <handa@etl.go.jp>
;;         Hiroshi Ueno <jl07715@yamato.ibm.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: bitmap.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
;; Keywords: bitmap, xbm, X-Face, Mule

;; This file is part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Code:

(require 'tl-822)

(defvar lc-bitmap
  (new-private-character-set 2 1 3 0 ?0 0 "BITMAP 8x16" "bitmap")
  "Leading character for BITMAP.8x16.")

(mapcar (lambda (fontset)
	  (if (= (fontset-pixel-size fontset) 16)
	      (set-fontset-font
	       fontset lc-bitmap
	       "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0")
	    ))
	(fontset-list))

;; Block (all bits set) character
(defvar bitmap-block (make-character lc-bitmap 32 33))

;; Simple examples:
;;	(bitmap-compose "00FF00FF00FF00FF00FF00FF00FF00FF")
;;	(bitmap-compose
;;	  "FF00FF00FF00FF00FF00FF00FF00FF00AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

(defun read-hexa (str)
  (let ((result 0) (i 0) (max (length str)))
    (while (< i max)
      (let ((ch (aref str i)))
	(cond((and (<= ?0 ch) (<= ch ?9))
	      (setq result (+ (* result 16) (- ch ?0))))
	     ((and (<= ?a ch) (<= ch ?f))
	      (setq result (+ (* result 16) (+ (- ch ?a) 10))))
  	     ((and (<= ?A ch) (<= ch ?F))
	      (setq result (+ (* result 16) (+ (- ch ?A) 10)))))
	(setq i (1+ i))))
    result))

(defun bitmap-compose (hex)
  "Return a string of composite characters which represents BITMAP-PATTERN.
BITMAP-PATTERN is a string of hexa decimal for 8x16 dot-pattern.
For example the pattern \"0081814242242442111124244242818100\" is
 for a bitmap of shape something like 'X' character."
  (let* ((len (/ (length hex) 2))
	 (bytes (char-bytes lc-bitmap))
	 (cmpstr "")
	 (buf (make-string 64 0))
	 block-flag i j row code c1 c2)
    (setq i 0 j 0 block-flag t)
    (while (< i len)
      (setq row (read-hexa (substring hex (* i 2) (+ (* i 2) 2))))
      (if block-flag
	  (setq block-flag (= row 255)))
      (if (/= row 0)
	  (progn
	    (setq code (+ (* (% i 16) 255) row -1))
	    (setq c1 (+ (/ code 96) 33)
		  c2 (+ (% code 96) 32))
	    (sset buf j (make-character lc-bitmap c1 c2))
	    (setq j (+ j bytes))))
      (setq i (1+ i))
      (if (or (= (% i 16) 0) (>= i len))
	  (setq cmpstr
		(concat cmpstr
			(if (and block-flag (= j 64))
			    (char-to-string bitmap-block)
			  (if (= j 0)
			      " "
			    (compose-string (substring buf 0 j)))))
		block-flag t
		j 0)))
    cmpstr))


;;; @ BDF
;;;

;; Internal variables  -- declared here to reduce garbage collection.
(defconst *hex* (vector (make-string 96 0) (make-string 96 0)))
(defconst *hex-len* (length *hex*))
(defconst *cmp* (make-vector *hex-len* nil))

(defun bdf-to-bitmap (bdf)
  "Set *cmp* a vector of string for BDF.
BDF is a vector of string, each elements corresponds to a line of bitmap
of difinition of a character glyph in bdf file."
  (let ((width (length (aref bdf 0)))
	(height (length bdf))
	i j)
    (if (or (/= (/ (+ height 15) 16) *hex-len*)
	    (/= width (length (aref *hex* 0))))
	(progn
	  (setq *hex-len* (/ (+ height 15) 16))
	  (setq *hex* (make-vector *hex-len* nil))
	  (setq *cmp* (make-vector *hex-len* nil))
	  (setq i 0)
	  (while (< i *hex-len*)
	    (aset *hex* i (make-string 96 0))
	    (setq i (1+ i)))))
    (setq j 0)
    (while (< j width)
      (setq i 0)
      (while (< i (* *hex-len* 16))
	(aset (aref *hex* (/ i 16))
	      (+ (* (/ j 2) 32) (* (% i 16) 2) (% j 2))
	      (if (< i height) (aref (aref bdf i) j) 0))
	(setq i (1+ i)))
      (setq j (1+ j)))
    (setq i 0)
    (while (< i *hex-len*)
      (aset *cmp* i (bitmap-compose (aref *hex* i)))
      (setq i (1+ i)))
    *cmp*
    ))


;;; @ XBM
;;;

(defun bitmap-show-xbm (buf)
  "Show bitmap in buffer BUF. Very slow! [bitmap.el]"
  (let ((hexa-string "0123456789ABCDEF")
	(reverse-bit '[0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15])
	i j w h bitmap cmp c temp)
    (save-excursion
      (set-buffer buf)
      (goto-char 1)
      (search-forward "width " nil t)
      (setq w (read (current-buffer)))
      (goto-char 1)
      (search-forward "height " nil t)
      (setq h (read (current-buffer)))
      (search-forward "0x" nil t)
      (setq bitmap (make-vector h 0))
      (setq cmp (make-vector (/ (+ h 15) 16) nil))
      (setq j 0)
      (setq w (/ (+ w 7) 8))
      (while (< j h)
	(aset bitmap j (make-vector w 0))
	(setq j (1+ j)))
      (setq j 0)
      (message "%dx%d" w h)
      (while (< j h)
	(setq i 0)
	(while (< i w)
	  (setq temp (buffer-substring (point) (+ (point) 2)))
	  (aset (aref bitmap j) i temp)
	  (setq c (read-hexa temp))
	  (aset temp 0 (aref hexa-string (aref reverse-bit (% c 16))))
	  (aset temp 1 (aref hexa-string (aref reverse-bit (/ c 16))))
	  (setq i (1+ i))
	  (search-forward "0x" nil t))
	(setq j (1+ j)))
      (message "bitmap translating...")
      (setq i 0)
      (while (< i w)
	(setq j 0)
	(while (< j h)
	  (aset cmp (/ j 16)
		(concat (aref cmp (/ j 16))
			(aref (aref bitmap j) i)))
	  (setq j (1+ j)))
	(if (> (% h 16) 0)
	    (aset cmp (/ h 16)
		  (concat (aref cmp (/ h 16))
			  (make-string (* (- 16 (% h 16)) 2) ?0))))
	(setq i (1+ i)))
      (message "cmp created"))
    (setq j 0)
    (while (< j (length cmp))
      (insert (bitmap-compose (aref cmp j)) ?\n)
      ;;(insert (aref cmp j) ?\n)
      (setq j (1+ j)))))

(defun bitmap-read-xbm (file)
  "Read .xbm file and show the bitmap.
Very slow! [bitmap.el]"
  (interactive "fBitmap-file: ")
  (bitmap-show-xbm (find-file-noselect (expand-file-name file)))
  )


;;; @ X-Face
;;;

(defvar bitmap-uncompface-program "uncompface")

(defun bitmap-decode-x-face ()
  (save-restriction
    (rfc822/narrow-to-header)
    (goto-char (point-min))
    (if (re-search-forward "^X-Face:[ \t]*" nil t)
	(let ((p (match-beginning 0))
	      (beg (match-end 0))
	      (end (rfc822/field-end))
	      (cur-buf (current-buffer))
	      )
	  (if (< end (point-max))
	      (setq end (1+ end))
	    )
	  (save-restriction
	    (narrow-to-region p end)
	    (delete-region p beg)
	    (call-process-region p (point-max)
				 bitmap-uncompface-program t t nil)
	    (let (i k k+6 cmp temp)
	      (goto-char (point-min))
	      (search-forward "0x" nil t)
	      (setq cmp (make-vector 18 nil))
	      (setq i 0)
	      (while (< i 48)
		(setq k (* (/ i 16) 6))
		(setq k+6 (+ k 6))
		(while (< k k+6)
		  (setq temp (buffer-substring (point) (+ (point) 2)))
		  (aset cmp k (concat (aref cmp k) temp))
		  (setq k (1+ k))
		  (setq temp (buffer-substring (+ (point) 2) (+ (point) 4)))
		  (aset cmp k (concat (aref cmp k) temp))
		  (setq k (1+ k))
		  (search-forward "0x" nil t)
		  )
		(setq i (1+ i)))
	      (delete-region (point-min)(point-max))
	      (insert "X-Face: ")
	      (setq k 0)
	      (while (< k 6)
		(insert (bitmap-compose (aref cmp k)))
		(setq k (1+ k))
		)
	      (insert ?\n)
	      (setq i 1)
	      (while (< i 3)
		(insert "        ")
		(setq k (* i 6)
		      k+6 (+ k 6))
		(while (< k k+6)
		  (insert (bitmap-compose (aref cmp k)))
		  (setq k (1+ k))
		  )
		(insert ?\n)
		(setq i (1+ i))
		)))))))


;;; @ end
;;;

(provide 'bitmap)

;;; bitmap.el ends here
