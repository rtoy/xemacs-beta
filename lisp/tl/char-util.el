;;; char-util.el --- character utility

;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: char-util.el,v 1.1 1997/01/30 02:27:29 steve Exp $
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

(defun row-line-to-char (r l)
  (int-char (+ (* r 16) l))
  )

(defun row-line-to-string (r l)
  (char-to-string (row-line-to-char r l))
  )

(defun print-row-line (r l)
  (interactive (and (looking-at "\\([0-9]+\\)/\\([0-9]+\\)")
		    (list (string-to-number
			   (buffer-substring (match-beginning 1)
					     (match-end 1)))
			  (string-to-number
			   (buffer-substring (match-beginning 2)
					     (match-end 2)))
			  )))
  (message (row-line-to-string r l))
  )

(defun char-to-row-line-form (chr)
  (setq chr (char-int chr))
  (format "%d/%d" (/ chr 16)(mod chr 16))
  )

(defun char-to-byte-list (chr)
  (let ((rest (mapcar (function identity)
		      (char-to-string chr))
	      ))
    (if (cdr rest)
	(cons (car rest)
	      (mapcar (lambda (byte)
			(logand byte 127)
			)
		      (cdr rest)))
      (cons 'ascii rest)
      )))

(defun char-to-row-cell-form (chr)
  (let ((cl (char-to-byte-list chr)))
    (if (= (length cl) 2)
	(char-to-row-line-form (nth 1 cl))
      (format "%02d-%02d" (- (nth 1 cl) 32)(- (nth 2 cl) 32))
      )))

(defun show-char-info (char)
  (interactive (list (char-after (point))))
  (let ((cl (char-to-byte-list char)))
    (message (format "%s: %s %s"
		     (charset-description (car cl))
		     (mapconcat (lambda (byte)
				  (format "%02x" byte)
				  )
				(cdr cl) "")
		     (if (= (length cl) 2)
			 (char-to-row-line-form (nth 1 cl))
		       (format "%02d-%02d" (- (nth 1 cl) 32)(- (nth 2 cl) 32))
		       )
		     ))))


;;; @ end
;;;

(provide 'char-util)

;;; char-util.el ends here
