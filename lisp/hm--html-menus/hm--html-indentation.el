;;; hm--html-indentation.el
;;; v1.00;  9-Feb-1997
;;; Copyright (C) 1997 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 1, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	Defines functions for the indentation.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories.
;;;

(defun hm--html-point-between-strings-p (string-1
					 string-2
					 &optional boundary)
  "Returns non nil, if the current point is between STRING-1 and STRING-2."
  (when (and (re-search-backward (concat "\\("
					 (regexp-quote string-1)
					 "\\)\\|\\("
					 (regexp-quote string-2)
					 "\\)")
				 boundary
				 t)
	     (match-string 1))
    (point)))

(defun hm--html-in-comment-p ()
  "Checks if the current point is in a comment block.
If this is the case, then the start point of the comment is returned.
Otherwise nil is returned."
  (save-excursion
    (hm--html-point-between-strings-p comment-start comment-end)))

(defun hm--html-previous-line-start ()
  "Returns the start of the previous non blank line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (point)))

(defun hm--html-look-at-comment-end-p ()
  "T, if the current line starts with the comment end."
  (looking-at (regexp-quote comment-end)))

(defun hm--html-column-of-previous-regexp (regexp)
  "Returns the column of the start of the previous REGEXP.
It searches backward until the REGEXP is found. If no
REGEXP is found, then it returns 0."
  (save-excursion
    (if (re-search-backward regexp nil t)
	(current-column)
      0)))

(defun hm--html-look-at-end-tag-p ()
  "Returns the end tag name if the point is at the start of an end tag.
nil is returned otherwise."
  (when (looking-at "\\(<[ \t\n]*/[ \t\n]*\\)\\([^ \t\n>]+\\)")
    (match-string 2)))
    

(defun hm--html-previous-line-indentation ()
  "Returns the indentation of the previous non blank line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (back-to-indentation)
    (current-column)))

(defun hm--html-in-tag-p ()
  "Checks if the current point is in a tag.
If this is the case, then the start point of the tag is returned.
Otherwise nil is returned."
  (save-excursion
    (let ((start (re-search-backward "\\(<\\)\\|\\(>\\)" nil t)))
      (when (match-string 1)
	start))))

(defun hm--html-return-beginning-of-line ()
  "Returns the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun hm--html-return-end-of-line ()
  "Returns the end of the current line."
  (save-excursion
    (end-of-line)
    (point)))

(defun hm--html-paramter-column-in-line-after-point (point)
  "Returns the column where the second non blank text after POINT starts.
This point must be in the line with POINT otherwise it returns nil."
  (save-excursion
    (goto-char point)
    (when (re-search-forward "<[ \t]*[^ \t]+[ \t]"
			     (hm--html-return-end-of-line)
			     t)
      (when (looking-at "[^\n]")
	(current-column)))))

(defun hm--html-column-of-point (point)
  "Returns the column of the POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun hm--html-search-previous-tag-in-current-line ()
  "Searches tags from the `(point)' to the beginning of the line.
It returns nil, if there is no tag and the tag name, if there is
a tag. The tag name contains a leading /, if it is an end tag."
  (when (re-search-backward ">" (hm--html-return-beginning-of-line) t)
    (when (re-search-backward
	   "\\(<[ \t\n]*\\(/?\\)\\([ \t\n]*[^> \t\n]+\\)[^>]*\\)"
	   nil
	   t)
      (concat (match-string 2) (match-string 3)))))

(defun hm--html-search-start-tag (tag-name until)
  "Searches start tag backwards from the current point until the point UNTIL.
The name of the tag is TAG-NAME. After this function the point is at UNTIL
 (then it returns nil) or at the start of the tag, then it returns t."
  (if (re-search-backward (concat "\\(<[ \t\n]*\\)\\(/?\\)\\("
				  tag-name 
				  "\\)\\([^>]*>\\)") until t)
      (if (string= "/" (match-string 2))
	  (progn
	    (hm--html-search-start-tag tag-name until)
	    (hm--html-search-start-tag tag-name until)) 
	t)
    (goto-char until)
    nil))

(defun hm--html-is-one-element-tag-p (tag-name)
  "Returns t, if the tag with the tag-name is a one element tag."
  (assoc :hm--html-one-element-tag
	 (cdr (assoc* (downcase tag-name)
		      hm--html-tag-name-alist
		      :test 'string=))))

(defun hm--html-calculate-indent-according-to-previous-tags ()
  "Calculate the indent according to the previous tags in this line.
If no tags are found, then nil is returned."
  (save-excursion
    (let ((tag (hm--html-search-previous-tag-in-current-line)))
      (cond ((not tag) nil)
	    
	    ((eq ?/ (elt tag 0)) ; end tag found
	     (if (hm--html-search-start-tag 
		  (substring tag 1)
		  (point-min))
		 (or (hm--html-calculate-indent-according-to-previous-tags)
		     (progn
		       (backward-to-indentation 0)
		       (current-column)))
	       0)) ; it may be that the current indentation is better here

	    ((hm--html-is-one-element-tag-p tag) ; one element tag
	     (or (hm--html-calculate-indent-according-to-previous-tags)
		 (progn
		   (backward-to-indentation 0)
		   (current-column))))

	    (t ; start tag found
	     (+ (current-column) hm--html-inter-tag-indent))))))


(defun hm--html-calculate-indent ()
  "Calculate the indentation of the current line."
  (let ((match-point)
	(tag))
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (cond ((eq (count-lines (point-min) (point)) 0) 0) ; Filestart
	    
	    ((setq match-point (hm--html-in-comment-p)) ; in a comment
	     (if (>= match-point (hm--html-previous-line-start)) ; 1. line
		 (if (hm--html-look-at-comment-end-p)
		     (hm--html-column-of-previous-regexp
		      (regexp-quote comment-start))
		   (+ (hm--html-column-of-previous-regexp
		       (regexp-quote comment-start))
		      hm--html-comment-indent))
	       (if (hm--html-look-at-comment-end-p)
		   (- (hm--html-previous-line-indentation)
		      hm--html-comment-indent)
		 (hm--html-previous-line-indentation))))
	    
	    ((setq tag (hm--html-look-at-end-tag-p)) ; look at end tag
	     (hm--html-search-start-tag tag (point-min))
	     (current-column))
	    
	    ((looking-at ">")
	     (hm--html-column-of-previous-regexp "<"))

	    ((setq match-point (hm--html-in-tag-p))
	     (if (>= match-point (hm--html-previous-line-start)) ; 1. line
		 (or (hm--html-paramter-column-in-line-after-point match-point)
		     (+ (hm--html-column-of-point match-point)
			hm--html-intra-tag-indent))
	       (hm--html-previous-line-indentation)))

	    (t (or (save-excursion  ; check previous line
		     (skip-chars-backward " \t\n")
		     (hm--html-calculate-indent-according-to-previous-tags))
		   (hm--html-previous-line-indentation)))
	    ))))

(defun hm--html-indent-line ()
  "Indent the current line line."
  (interactive)
  (unless hm--html-disable-indentation
    (indent-line-to (max 0 (hm--html-calculate-indent)))))

;;; Indentation

(defun hm--html-indent-region (begin end)
  "Indents the region between BEGIN and END according to the major mode."
  (interactive "d\nm")
  (when (< end begin)
    (let ((a end))
      (setq end begin)
      (setq begin a)))
  (save-excursion
    (goto-char begin)
    (let ((old-point))
      (while (and (<= (point) end)
		  (not (eq (point) old-point)))
	(setq old-point (point))
	(indent-according-to-mode)
	(forward-line)
	))))


(provide 'hm--html-indentation)
