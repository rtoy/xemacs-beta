;;; smiley.el --- displaying smiley faces
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Wes Hardaker <hardaker@ece.ucdavis.edu>
;; Keywords: fun

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;; comments go here.
;;

;;; Test smileys:  :-] :-o :-) ;-) :-\ :-| :-d :-P 8-| :-(

;; To use:
;; (require 'smiley)
;; (add-hook 'gnus-article-display-hook 'gnus-smiley-display t)

;; The smilies were drawn by Joe Reiss <joe@jreiss.async.vt.edu>. 

(require 'annotations)
(require 'messagexmas)
(eval-when-compile (require 'cl))

(defvar smiley-data-directory (message-xmas-find-glyph-directory "smilies")
  "Location of the smiley faces files.")

;; Notice the subtle differences in the regular expessions in the two alists below

(defvar smiley-deformed-regexp-alist
  '(("\\(:-*[<�]+\\)\\W" 1 "FaceAngry.xpm")
    ("\\(:-+\\]+\\)\\W" 1 "FaceGoofy.xpm")
    ("\\(:-*D\\)\\W" 1 "FaceGrinning.xpm")
    ("\\(:-*[)>}�]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(:-*[/\\\"]\\)[^/]" 1 "FaceIronic.xpm")
    ("\\([8|]-*[|Oo%]\\)\\W" 1 "FaceKOed.xpm")
    ("\\([:|]-*#+\\)\\W" 1 "FaceNyah.xpm")
    ("\\(:-*[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(:-*[Oo\*]\\)\\W" 1 "FaceStartled.xpm")
    ("\\(:-*|\\)\\W" 1 "FaceStraight.xpm")
    ("\\(:-*p\\)\\W" 1 "FaceTalking.xpm")
    ("\\(:-*d\\)\\W" 1 "FaceTasty.xpm")
    ("\\(;-*[>)}�]+\\)\\W" 1 "FaceWinking.xpm")
    ("\\(:-*[Vv�]\\)\\W" 1 "FaceWry.xpm")
    ("\\([:|]-*P\\)\\W" 1 "FaceYukky.xpm"))
  "Normal and deformed faces for smilies.")

(defvar smiley-nosey-regexp-alist
  '(("\\(:-+[<�]+\\)\\W" 1 "FaceAngry.xpm")
    ("\\(:-+\\]+\\)\\W" 1 "FaceGoofy.xpm")
    ("\\(:-+D\\)\\W" 1 "FaceGrinning.xpm")
    ("\\(:-+[}�]+\\)\\W" 1 "FaceHappy.xpm")
    ("\\(:-*)+\\)\\W" 1 "FaceHappy.xpm") ;; The exception that confirms the rule
    ("\\(:-+[/\\\"]+\\)\\W" 1 "FaceIronic.xpm")
    ("\\([8|]-+[|Oo%]\\)\\W" 1 "FaceKOed.xpm")
    ("\\([:|]-+#+\\)\\W" 1 "FaceNyah.xpm")
    ("\\(:-+[({]+\\)\\W" 1 "FaceSad.xpm")
    ("\\(:-+[Oo\*]\\)\\W" 1 "FaceStartled.xpm")
    ("\\(:-+|\\)\\W" 1 "FaceStraight.xpm")
    ("\\(:-+p\\)\\W" 1 "FaceTalking.xpm")
    ("\\(:-+d\\)\\W" 1 "FaceTasty.xpm")
    ("\\(;-+[>)}�]+\\)\\W" 1 "FaceWinking.xpm")
    ("\\(:-+[Vv�]\\)\\W" 1 "FaceWry.xpm")
    ("\\([:|]-+P\\)\\W" 1 "FaceYukky.xpm"))
  "Smileys with noses. These get less false matches.")

(defvar smiley-regexp-alist smiley-deformed-regexp-alist
  "A list of regexps to map smilies to real images.
Defaults to the content of smiley-deformed-regexp-alist.
An alternative smiley-nose-regexp-alist that
matches less aggresively is available.")

(defvar smiley-flesh-color "yellow"
  "Flesh color.")

(defvar smiley-features-color "black"
  "Features color.")

(defvar smiley-tongue-color "red"
  "Tongue color.")

(defvar smiley-circle-color "black"
  "Circle color.")

(defvar smiley-glyph-cache nil)
(defvar smiley-running-xemacs (string-match "XEmacs" emacs-version))

(defun smiley-create-glyph (smiley pixmap)
  (and
   smiley-running-xemacs
   (or
    (cdr-safe (assoc pixmap smiley-glyph-cache))
    (let* ((xpm-color-symbols 
	    (and (featurep 'xpm)
		 (append `(("flesh" ,smiley-flesh-color)
			   ("features" ,smiley-features-color)
			   ("tongue" ,smiley-tongue-color))
			 xpm-color-symbols)))
	   (glyph (make-glyph
		   (list
		    (cons 'x (expand-file-name pixmap smiley-data-directory))
		    (cons 'tty smiley)))))
      (setq smiley-glyph-cache (cons (cons pixmap glyph) smiley-glyph-cache))
      (set-glyph-face glyph 'default)
      glyph))))

;;;###autoload
(defun smiley-region (beg end)
  "Smilify the region between point and mark."
  (interactive "r")
  (smiley-buffer (current-buffer) beg end))

;;;###autoload
(defun smiley-buffer (&optional buffer st nd)
  (interactive)
  (when (featurep 'x)
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (let ((buffer-read-only nil)
	    (alist smiley-regexp-alist)
	    entry regexp beg group file)
	(goto-char (or st (point-min)))
	(setq beg (point))
	;; loop through alist
	(while (setq entry (pop alist))
	  (setq regexp (car entry)
		group (cadr entry)
		file (caddr entry))
	  (goto-char beg)
	  (while (re-search-forward regexp nd t)
	    (let* ((start (match-beginning group))
		   (end (match-end group))
		   (glyph (smiley-create-glyph (buffer-substring start end)
					       file)))
	      (when glyph
		(mapcar 'delete-annotation (annotations-at end))
		(let ((ext (make-extent start end)))
		  (set-extent-property ext 'invisible t)
		  (set-extent-property ext 'end-open t)
		  (set-extent-property ext 'intangible t))
		(make-annotation glyph end 'text)
		(when (smiley-end-paren-p start end)
		  (make-annotation ")" end 'text))
		(goto-char end)))))))))

(defun smiley-end-paren-p (start end)
  "Try to guess whether the current smiley is an end-paren smiley."
  (save-excursion
    (goto-char start)
    (when (and (re-search-backward "[()]" nil t)
	       (= (following-char) ?\()
	       (goto-char end)
	       (or (not (re-search-forward "[()]" nil t))
		   (= (char-after (1- (point))) ?\()))
      t)))

;;;###autoload    
(defun gnus-smiley-display ()
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    ;; We skip the headers.
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max)))
    (smiley-buffer (current-buffer) (point))))

(provide 'smiley)

;;; smiley.el ends here