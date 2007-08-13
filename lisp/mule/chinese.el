;;; chinese.el --- Chinese specific setup for XEmacs/Mule (not pre-loaded).

;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Mule 2.3.

;;; 92.3.5   Created for Mule Ver.0.9.0 by K.Handa <handa@etl.go.jp>

;; #### This does not work yet in XEmacs.

;; Hz/ZW encoding stuffs
(defvar hz2gb-gb-designation "\e$A")
(defvar hz2gb-ascii-designation "\e(B")
(defvar hz2gb-line-continuation nil)

;;;###autoload
(defun hz2gb-buffer ()
  "Decode the text in the current buffer, assumed to be HZ/ZW-encoded."
  (interactive)
  (hz2gb-region (point-min) (point-max)))	

;;;###autoload
(defun hz2gb-region (beg end)
  "Decode HZ/ZW-encoded text between point and mark.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch to be decoded."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; "~\n" -> "\n"
      (goto-char (point-min))
      (while (search-forward "~" nil t)
	(if (= (following-char) ?\n) (delete-char -1))
	(if (not (eobp)) (forward-char 1)))
      ;; "^zW...\n" -> Chinese text
      ;; "~{...~}"  -> Chinese Text
      (goto-char (point-min))
      (let (chinese-found)
	(while (re-search-forward "~{\\|^zW" nil t)
	  (if (= (char-after (match-beginning 0)) ?z)
	      ;; ZW -> junet
	      (progn
		(delete-char -2)
		(insert hz2gb-gb-designation)
		(end-of-line)
		(insert hz2gb-ascii-designation))
	    ;; Hz -> junet
	    (delete-char -2)
	    (insert hz2gb-gb-designation)
	    (if (re-search-forward "\\(~}\\)\\|\\(\n\\)" nil t)
		(if (match-beginning 1)
		    (replace-match hz2gb-ascii-designation)
		  (if (not hz2gb-line-continuation)
		      (progn
			(goto-char (match-beginning 2))
			(insert hz2gb-ascii-designation))))))
	  (setq chinese-found t))
	(if chinese-found
	    (decode-coding-region (point-min) (point-max) 'junet)))
      ;; "~~" -> "~"
      (goto-char (point-min))
      (while (search-forward "~~" nil t) (delete-char -1)))))

;;;###autoload
(defun gb2hz-buffer ()
  "Convert whole text in the current buffer
from mule internal encoding to HZ encoding."
  (interactive) (gb2hz-region (point-min) (point-max)))	

;;;###autoload
(defun gb2hz-region (beg end)
  "Encode text between point and mark in HZ/ZW encoding.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch to be encoded."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; "~" -> "~~"
      (goto-char (point-min))
      (while (search-forward "~" nil t)	(insert ?~))
      ;; Chinese text -> "~{...~}"
      (goto-char (point-min))
      (if (re-search-forward "\\cc" nil t)
	  (let (p)
	    (goto-char (match-beginning 0))
	    (setq p (point))
	    (encode-coding-region p (point-max) 'junet)
	    (goto-char p)
	    (while (search-forward hz2gb-gb-designation nil t)
	      (delete-char -3)
	      (insert "~{"))
	    (goto-char p)
	    (while (search-forward hz2gb-ascii-designation nil t)
	      (delete-char -3)
	      (insert "~}"))
	    (goto-char p)))
      )))

;;;
(provide 'chinese)
