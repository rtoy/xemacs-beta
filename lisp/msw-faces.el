;;; msw-faces.el --- mswindows-specific face stuff.

;;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;;; Copyright (C) 1995, 1996, 2002 Ben Wing.

;; Author: Jamie Zawinski
;; Modified by:  Chuck Thompson
;; Modified by:  Ben Wing
;; Modified by:  Martin Buchholz
;; Rewritten for mswindows by:  Jonathan Harris

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

;; This file does the magic to parse mswindows font names, and make sure that
;; the default and modeline attributes of new frames are specified enough.

(defun mswindows-init-global-faces ()
  (set-face-font 'gui-element "MS Sans Serif:Regular:8" nil 'mswindows))

(defun mswindows-init-device-faces (device)
  (let ((color-default (device-system-metric device 'color-default))
	(color-3d-face (device-system-metric device 'color-3d-face)))
    ; Force creation of the default face font so that if it fails we get
    ; an error now instead of a crash at frame creation.
    (unless (face-font-instance 'default device)
      (error "Can't find a suitable default font"))

    ;; Don't set them on the device because then the global setting won't
    ;; override them.
    ;; #### Use device tags if we have multiple msprinter devices. (can we?)
    (if (car color-default)
	(set-face-foreground 'default (car color-default) nil
			     (device-type device)))
    (if (cdr color-default)
	(set-face-background 'default (cdr color-default) nil
			     (device-type device)))
    (if (car color-3d-face)
	(set-face-foreground 'gui-element (car color-3d-face) nil
			     (device-type device)))
    (if (cdr color-3d-face)
	(set-face-background 'gui-element (cdr color-3d-face) nil
			     (device-type device)))
    ))

(defun mswindows-init-frame-faces (frame)
  )

;; Other functions expect these regexps
(let
    ((- 		":")
     (fontname	"\\([a-zA-Z ]*\\)")				; 1
     (style	"\\(\\(?:[a-zA-Z]+\\(?: +[a-zA-Z]+\\)*\\)?\\)") ; 2
     (pointsize	"\\([0-9]*\\)")					; 3
     (effects	"\\(\\(?:[a-zA-Z]+\\(?: +[a-zA-Z]+\\)*\\)?\\)")	; 4
     ;; must match "OEM/DOS"
     (charset	"\\([a-zA-Z/ ]*\\)")				; 5
     )
  (defconst mswindows-font-regexp
    (concat "^" fontname - style - pointsize - effects - charset "$"))
  (defconst mswindows-font-regexp-missing-1
    (concat "^" fontname - style - pointsize - effects "$"))
  (defconst mswindows-font-regexp-missing-2
    (concat "^" fontname - style - pointsize "$"))
  (defconst mswindows-font-regexp-missing-3
    (concat "^" fontname - style "$"))
  (defconst mswindows-font-regexp-missing-4
    (concat "^" fontname "$"))
)

;;; Fill in missing parts of a font spec. This is primarily intended as a
;;; helper function for the functions below.
;;; mswindows fonts look like:
;;;	fontname[:[weight][ slant][:pointsize[:effects]]][:charset]
;;; A minimal mswindows font spec looks like:
;;;	Courier New
;;; A maximal mswindows font spec looks like:
;;;	Courier New:Bold Italic:10:underline strikeout:Western
(defun mswindows-canonicalize-font-name (font)
  "Given a mswindows font or font name, return its name in canonical form.
This adds missing colons and fills in the style field with \"Regular\".
This does *NOT* fill in the point size or charset fields, because in those
cases an empty field is not equivalent to any particular field value, but a
wildcard allowing for any possible value (charset Western and point size 10
are chosen first, if they exist)."
  (if (font-instance-p font) (setq font (font-instance-name font)))
  ;; fill in missing colons
  (setq font
	(cond ((string-match mswindows-font-regexp font) font)
	      ((string-match mswindows-font-regexp-missing-1 font)
	       (concat font ":"))
	      ((string-match mswindows-font-regexp-missing-2 font)
	       (concat font "::"))
	      ((string-match mswindows-font-regexp-missing-3 font)
	       (concat font ":::"))
	      ((string-match mswindows-font-regexp-missing-4 font)
	       (concat font "::::"))
	      (t "::::")))
  (or (string-match mswindows-font-regexp font) (error "can't parse %S" font))
  (if (equal "" (match-string 2 font))
      (concat (substring font 0 (match-beginning 2)) "Regular"
	      (substring font (match-beginning 2)))
    font))

(defun mswindows-parse-font-style (style)
  ;; Parse a style into a cons (WEIGHT . SLANT).  WEIGHT will never be the
  ;; empty string (it may be "Regular"), but SLANT will be empty for
  ;; non-italic.
  (save-match-data
    (let ((case-fold-search t))
      (cond ((equalp style "Italic") '("Regular" . "Italic"))
	    ((string-match "^\\([a-zA-Z ]+?\\) +Italic$" style)
	     (cons (match-string 1 style) "Italic"))
	    (t (cons style ""))))))

(defun mswindows-construct-font-style (weight slant)
  ;; Construct the style from WEIGHT and SLANT.  Opposite of
  ;; mswindows-parse-font-style.
  (cond ((and (equal slant "") (equal weight "")) "Regular")
	((equal slant "") weight)
	((or (equalp weight "Regular") (equal weight "")) slant)
	(t (concat weight " " slant))))

(defun mswindows-frob-font-style (font which)
  ;; Given a font name or font instance, return a name with the style field
  ;; (which includes weight and/or slant) changed according to WHICH, a plist.
  ;; If no entry found, don't change.
  (if (null font) nil
    (setq font (mswindows-canonicalize-font-name font))
    (or (string-match mswindows-font-regexp font)
	(error "can't parse %S" font))
    (let* ((style (match-string 2 font))
	   (style-rep
	    (save-match-data
	      (or (loop for (x y) on which by #'cddr
		    if (string-match (concat "^" x "$") style)
		    return (replace-match y nil nil style))
		  style))))
      (concat (substring font 0 (match-beginning 2))
	      style-rep
	      (substring font (match-end 2))))))

(defun mswindows-frob-font-style-and-sizify (font which &optional device)
  (if (null font) nil
    (let* ((oldwidth (if (font-instance-p font) (font-instance-width font)
		       (let ((fi (make-font-instance font device t)))
			 (and fi (font-instance-width fi)))))
	   (newname (mswindows-frob-font-style font which))
	   (newfont (make-font-instance newname device t)))
      ;; Hack!  On MS Windows, bold fonts (even monospaced) are often wider
      ;; than the equivalent non-bold font.  Making the bold font one point
      ;; smaller usually makes it the same width (maybe at the expense of
      ;; making it one pixel shorter).  Do the same trick in both directions.
      (when (font-instance-p newfont)
	(let ((newerfont newfont))
	   (block nil
	     (while (and newerfont oldwidth)
	       (setq newfont newerfont)
	       (cond ((< (font-instance-width newfont) oldwidth)
		      (setq newerfont
			    (make-font-instance
			     (mswindows-find-larger-font newfont device)
			     device t))
		      (if (and newerfont
			       (> (font-instance-width newerfont) oldwidth))
			  (return nil)))
		     ((> (font-instance-width newfont) oldwidth)
		      (setq newerfont
			    (make-font-instance
			     (mswindows-find-smaller-font newfont device)
			     device t))
		      (if (and newerfont
			       (< (font-instance-width newerfont) oldwidth))
			  (return nil)))
		     (t (return nil))))))
	(if (font-instance-p newfont) (font-instance-name newfont) newfont)))))

(defconst mswindows-nonbold-weight-regexp
  ;; He looked so, so cool with the ultra light dangling from his mouth as
  ;; his fingers spun out demisemiquavers from the keyboard ...
  "\\(Regular\\|Thin\\|Extra Light\\|Ultra Light\\|Light\\|Normal\\|Medium\\|Semi Bold\\|Demi Bold\\)"
  )
(defconst mswindows-bold-weight-regexp
  "\\(Semi Bold\\|Demi Bold\\|Bold\\|Extra Bold\\|Ultra Bold\\|Heavy\\|Black\\)"
  )

(defconst mswindows-make-font-bold-mapper
  `(,mswindows-nonbold-weight-regexp "Bold"
    "Italic" "Bold Italic"
    ,(concat mswindows-nonbold-weight-regexp " Italic") "Bold Italic"))

(defconst mswindows-make-font-nonbold-mapper
  `(,mswindows-bold-weight-regexp "Regular"
    ,(concat mswindows-bold-weight-regexp " Italic") "Italic"))

(defconst mswindows-make-font-italic-mapper
  '("\\(.*\\)Italic" "\\1Italic"
    "\\(.*\\)" "\\1 Italic"))

(defconst mswindows-make-font-unitalic-mapper
  '("Italic" "Regular"
    "\\(.*\\) Italic" "\\1"))

(defconst mswindows-make-font-bold-italic-mapper
  `(,mswindows-nonbold-weight-regexp "Bold Italic"
    ,(concat mswindows-nonbold-weight-regexp " Italic") "Bold Italic"
    "Italic" "Bold Italic"
    ,mswindows-bold-weight-regexp "\\1 Italic"))

(defun mswindows-make-font-bold (font &optional device)
  "Given a mswindows font specification, this attempts to make a bold font.
If it fails, it returns nil."
  (mswindows-frob-font-style-and-sizify font mswindows-make-font-bold-mapper
					device))

(defun mswindows-make-font-unbold (font &optional device)
  "Given a mswindows font specification, this attempts to make a non-bold font.
If it fails, it returns nil."
  (mswindows-frob-font-style-and-sizify font mswindows-make-font-nonbold-mapper
					device))

(defun mswindows-make-font-italic (font &optional device)
  "Given a mswindows font specification, this attempts to make an `italic'
font. If it fails, it returns nil."
  (try-font-name (mswindows-frob-font-style
		  font mswindows-make-font-italic-mapper) device))

(defun mswindows-make-font-unitalic (font &optional device)
  "Given a mswindows font specification, this attempts to make a non-italic
font. If it fails, it returns nil."
  (try-font-name (mswindows-frob-font-style
		  font mswindows-make-font-unitalic-mapper) device))

(defun mswindows-make-font-bold-italic (font &optional device)
  "Given a mswindows font specification, this attempts to make a `bold-italic'
font. If it fails, it returns nil."
  (mswindows-frob-font-style-and-sizify font
					mswindows-make-font-bold-italic-mapper
					device))

(defun mswindows-available-font-sizes (font device)
  (if (font-instance-p font) (setq font (font-instance-name font)))
  (setq font (mswindows-canonicalize-font-name font))
  (or (string-match mswindows-font-regexp font) (error "Can't parse %S" font))
  ;; turn pointsize into wildcard
  (setq font
	(concat (substring font 0 (match-beginning 3))
		(substring font (match-end 3) (match-end 0))))
  (sort
   (delq nil
	 (mapcar #'(lambda (name)
		     (and (string-match mswindows-font-regexp name)
			  (string-to-int (substring name (match-beginning 3)
						    (match-end 3)))))
		 (list-fonts font device)))
   #'<))

(defun mswindows-frob-font-size (font up-p device)
  (if (stringp font) (setq font (make-font-instance font device)))
  (let* ((name (font-instance-name font))
	 (truename (font-instance-truename font))
	 (available (and truename
			 (mswindows-available-font-sizes truename device))))
    (if (null available) nil
      (or (string-match mswindows-font-regexp truename)
	  (error "can't parse %S" truename))
      (let ((old-size (string-to-int
		       (substring truename
				  (match-beginning 3) (match-end 3)))))
	(or (> old-size 0) (error "font truename has 0 pointsize?"))
	(or (string-match mswindows-font-regexp name)
	    (error "can't parse %S" name))
	(let ((newsize
	       ;; scalable fonts: change size by 1 point.
	       (if (= 0 (car available))
		   (if (and (not up-p) (= 1 old-size)) nil
		     (if up-p (1+ old-size) (1- old-size)))
		 ;; non-scalable fonts: take the next available size.
		 (if up-p
		     (loop for tail on available
		       if (eql (car tail) old-size)
		       return (cadr tail))
		   (loop for tail on available
		     if (eql (cadr tail) old-size)
		     return (car tail))))))
	    (and newsize
	      (concat (substring name 0 (match-beginning 3))
		      (int-to-string newsize)
		      (substring name (match-end 3) (match-end 0)))))))))

(defun mswindows-find-smaller-font (font &optional device)
  "Loads a new version of the given font (or font name) 1 point smaller.
Returns the font if it succeeds, nil otherwise."
  (mswindows-frob-font-size font nil device))

(defun mswindows-find-larger-font (font &optional device)
  "Loads a new version of the given font (or font name) 1 point larger.
Returns the font if it succeeds, nil otherwise."
  (mswindows-frob-font-size font t device))
