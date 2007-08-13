;;; char-table.el --- display table of charset

;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: char-table.el,v 1.3 1997/06/29 23:13:25 steve Exp $
;; Keywords: character, mule

;; This file is part of tl (Tiny Library).

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

(defsubst char-position-to-string (charset r l &optional plane)
  (char-to-string
   (if plane
       (make-char charset plane (+ (* r 16) l))
     (make-char charset (+ (* r 16) l))
     )))

(defsubst char-table-1 (charset r l plane)
  (let* ((str (char-position-to-string charset r l plane))
	 (lp (- 3 (string-width str)))
	 (rp (/ lp 2)))
    (setq lp
	  (if (= (mod lp 2) 0)
	      rp
	    (1+ rp)))
    (concat (make-string lp ? ) str (make-string rp ? ))
    ))

(defun insert-94-charset-table (charset &optional plane ofs)
  (if (null ofs)
      (setq ofs 0)
    )
  (insert (format
	  "[%02x]│ 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n"
	  (or plane 0)))
  (insert "──┼────────────────────────\n")
  (let ((j 2))
    (insert (format "%02x%x│   " (or plane 0) (* (+ j ofs) 16)))
    (let ((k 1))
      (while (< k 16)
	(insert (char-table-1 charset j k plane))
	(setq k (+ k 1))
	)
      (insert "\n")
      )
    (setq j 3)
    (while (< j 7)
      (insert (format "%02x%x│" (or plane 0) (* (+ j ofs) 16)))
      (let ((k 0))
	(while (< k 16)
	  (insert (char-table-1 charset j k plane))
	  (setq k (+ k 1))
	  )
	(insert "\n")
	)
      (setq j (+ j 1))
      )
    (insert (format "%02x%x│" (or plane 0) (* (+ j ofs) 16)))
    (let ((k 0))
      (while (< k 15)
	(insert (char-table-1 charset j k plane))
	(setq k (+ k 1))
	)
      (insert "\n")
      )
    ))

(defun insert-96-charset-table (charset &optional plane ofs)
  (if (null ofs)
      (setq ofs 0)
    )
  (insert (format
	  "[%02x]│ 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n"
	  (or plane 0)))
  (insert "──┼────────────────────────\n")
  (let ((j 2))
    (while (< j 8)
      (insert (format "%02x%x│" (or plane 0) (* (+ j ofs) 16)))
      (let ((k 0))
	(while (< k 16)
	  (insert (char-table-1 charset j k plane))
	  (setq k (+ k 1))
	  )
	(insert "\n")
	)
      (setq j (1+ j))
      )))

(defun insert-94x94-charset-table (charset)
  (insert-94-charset-table charset 33)
  (let ((i 34))
    (while (< i 127)
      (insert "━━┿━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert-94-charset-table charset i)
      (setq i (1+ i))
      )))

(defun insert-96x96-charset-table (charset)
  (insert-96-charset-table charset 32)
  (let ((i 33))
    (while (< i 128)
      (insert "━━┿━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert-96-charset-table charset i)
      (setq i (1+ i))
      )))

(defun insert-charset-table (charset)
  "Insert character table of CHARSET."
  (insert "━━┯━━━━━━━━━━━━━━━━━━━━━━━━\n")
  (let ((cc (charset-chars charset))
	(cd (charset-dimension charset))
	)
    (cond ((= cd 1)
	   (cond ((= cc 94)
		  (insert-94-charset-table charset)
		  )
		 ((= cc 96)
		  (insert-96-charset-table charset)
		  ))
	   )
	  ((= cd 2)
	   (cond ((= cc 94)
		  (insert-94x94-charset-table charset)
		  )
		 ((= cc 96)
		  (insert-96x96-charset-table charset)
		  ))
	   )))
  (insert "━━┷━━━━━━━━━━━━━━━━━━━━━━━━\n")
  )

;;;###autoload
(defun view-charset (charset)
  "Display character table of CHARSET."
  (interactive
   (list
    (let ((charset-alist
	   (mapcar (function
		    (lambda (charset)
		      (cons (charset-doc-string charset) charset)
		      ))
		   (charset-list))))
      (cdr (assoc (completing-read "What charset: "
				   charset-alist nil t nil)
		  charset-alist))
      )))
  (let* ((desc (charset-doc-string charset))
	 (buf (concat "*Charset table for "
		      (charset-doc-string charset)
		      "*")))
    (unless (get-buffer buf)
      (let ((the-buf (current-buffer)))
	(set-buffer (get-buffer-create buf))
	(insert (format "%s (%s)\n" desc charset))
	(let ((msg (format "Generating char table for %s..." desc)))
	  (message msg)
	  (insert-charset-table charset)
	  (message "%s Done." msg)
	  )
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(set-buffer the-buf)
	))
    (view-buffer buf)
    ))


;;; @ end
;;;

(provide 'char-table)

;;; char-table.el ends here
