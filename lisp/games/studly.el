;;; studly.el --- StudlyCaps (tm)(r)(c)(xxx)

;; This is in the public domain, since it was distributed
;; by its author without a copyright notice in 1986.

;; Keywords: games

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; Functions to studlycapsify a region, word, or buffer.  Possibly the
;; esoteric significance of studlycapsification escapes you; that is,
;; you suffer from autostudlycapsifibogotification.  Too bad.

;;; Code:

(defun studlify-region (begin end)
  "Studlify-case the region"
  (interactive "*r")
  (save-excursion
    (goto-char begin)
    (setq begin (point))
    (while (and (<= (point) end)
		(not (looking-at "\\W*\\'")))
      (forward-word 1)
      (backward-word 1)
      (setq begin (max (point) begin))
      (forward-word 1)
      (let ((offset 0)
	    (word-end (min (point) end))
	    c)
	(goto-char begin)
	(while (< (point) word-end)
	  (setq offset (+ offset (following-char)))
	  (forward-char 1))
	(setq offset (+ offset (following-char)))
	(goto-char begin)
	(while (< (point) word-end)
	  (setq c (following-char))
	  (if (and (= (% (+ c offset) 4) 2)
		   (let ((ch (following-char)))
		     (or (and (>= ch ?a) (<= ch ?z))
			 (and (>= ch ?A) (<= ch ?Z)))))
	      (progn
		(delete-char 1)
		(insert (logxor c ? ))))
	  (forward-char 1))
	(setq begin (point))))))

(defun studlify-word (count)
  "Studlify-case the current word, or COUNT words if given an argument"
  (interactive "*p")
  (let ((begin (point)) end rb re)
    (forward-word count)
    (setq end (point))
    (setq rb (min begin end) re (max begin end))
    (studlify-region rb re)))

(defun studlify-buffer ()
  "Studlify-case the current buffer"
  (interactive "*")
  (studlify-region (point-min) (point-max)))

;;; studly.el ends here
