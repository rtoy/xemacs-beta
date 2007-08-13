;;; char-table.el --- display table of charset

;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: char-table.el,v 1.1 1997/01/30 02:27:29 steve Exp $
;; Keywords: character, Emacs/mule

;; This file is not part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'char-util)

(defun char-position-to-string (charset r l &optional plane)
  (char-to-string
   (if plane
       (make-character charset plane (row-line-to-char r l))
     (make-character charset (row-line-to-char r l))
     )))

(defun char-table-1 (charset r l plane)
  (let ((str (char-position-to-string charset r l plane)))
    (concat
     (let ((i 0)
	   (len (- 3 (string-columns str)))
	   (dest ""))
       (while (< i len)
	 (setq dest (concat dest " "))
	 (setq i (1+ i))
	 )
       dest) str)))

(defun show-94-table (charset &optional plane ofs)
  (if (null ofs)
      (setq ofs 0)
    )
  (princ "======================================================\n")
  (princ (format
	  "[%3x]: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n"
	  (or plane 0)))
  (princ "-----+------------------------------------------------\n")
  (let ((j 2))
    (princ (format "%2x%x :   " (or plane 0) (* (+ j ofs) 16)))
    (let ((k 1))
      (while (< k 16)
	(princ (char-table-1 charset j k plane))
	(setq k (+ k 1))
	)
      (princ "\n")
      )
    (setq j 3)
    (while (< j 7)
      (princ (format "%2x%x :" (or plane 0) (* (+ j ofs) 16)))
      (let ((k 0))
	(while (< k 16)
	  (princ (char-table-1 charset j k plane))
	  (setq k (+ k 1))
	  )
	(princ "\n")
	)
      (setq j (+ j 1))
      )
    (princ (format "%2x%x :" (or plane 0) (* (+ j ofs) 16)))
    (let ((k 0))
      (while (< k 15)
	(princ (char-table-1 charset j k plane))
	(setq k (+ k 1))
	)
      (princ "\n")
      )
    ))

(defun show-96-table (charset &optional plane ofs)
  (if (null ofs)
      (setq ofs 0)
    )
  (princ "======================================================\n")
  (princ (format
	  "[%3x]: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n"
	  (or plane 0)))
  (princ "-----+------------------------------------------------\n")
  (let ((j 2))
    (while (< j 8)
      (princ (format "%2x%x :" (or plane 0) (* (+ j ofs) 16)))
      (let ((k 0))
	(while (< k 16)
	  (princ (char-table-1 charset j k plane))
	  (setq k (+ k 1))
	  )
	(princ "\n")
	)
      (setq j (1+ j))
      )))

(defun show-94x94-table (charset)
  (let ((i 33))
    (while (< i 127)
      (show-94-table charset i)
      (setq i (1+ i))
      )))

(defun show-96x96-table (charset)
  (let ((i 32))
    (while (< i 128)
      (show-96-table charset i)
      (setq i (1+ i))
      )))

(defun show-char-table (charset)
  (let ((cc (charset-chars charset))
	(cd (charset-dimension charset))
	)
    (cond ((= cd 1)
	   (cond ((= cc 94)
		  (show-94-table charset)
		  )
		 ((= cc 96)
		  (show-96-table charset)
		  ))
	   )
	  ((= cd 2)
	   (cond ((= cc 94)
		  (show-94x94-table charset)
		  )
		 ((= cc 96)
		  (show-96x96-table charset)
		  ))
	   ))))


;;; @ end
;;;

(provide 'char-table)

;;; char-table.el ends here
