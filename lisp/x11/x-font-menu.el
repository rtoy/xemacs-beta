;; x-font-menu.el --- Managing menus of X fonts.

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

;; Author: Jamie Zawinski <jwz@lucid.com>
;; Restructured by: Jonathan Stigelman <Stig@hackvan.com>

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;;
;;; Creates three menus, "Font", "Size", and "Weight", and puts them on the
;;; "Options" menu.  The contents of these menus are the superset of those
;;; properties available on any fonts, but only the intersection of the three
;;; sets is selectable at one time.
;;;
;;; Known Problems:
;;; ===============
;;; Items on the Font menu are selectable if and only if that font exists in
;;; the same size and weight as the current font.  This means that some fonts
;;; are simply not reachable from some other fonts - if only one font comes
;;; in only one point size (like "Nil", which comes only in 2), you will never
;;; be able to select it.  It would be better if the items on the Fonts menu
;;; were always selectable, and selecting them would set the size to be the
;;; closest size to the current font's size.
;;;
;;; This attempts to change all other faces in an analagous way to the change
;;; that was made to the default face; if it can't, it will skip over the face.
;;; However, this could leave incongruous font sizes around, which may cause
;;; some nonreversibility problems if further changes are made.  Perhaps it
;;; should remember the initial fonts of all faces, and derive all subsequent
;;; fonts from that initial state.
;;;
;;; xfontsel(1) is a lot more flexible (but probably harder to understand).
;;;
;;; The code to construct menus from all of the x11 fonts available from the
;;; server is autoloaded and executed the very first time that one of the Font
;;; menus is selected on each device.  That is, if XEmacs has frames on two
;;; different devices, then separate font menu information will be maintained
;;; for each X display.  If the font path changes after emacs has already
;;; asked the X server on a particular display for its list of fonts, this
;;; won't notice.  Also, the first time that a font menu is posted on each
;;; display will entail a lengthy delay, but that's better than slowing down
;;; XEmacs startup.  At any time (i.e.: after a font-path change or
;;; immediately after device creation), you can call
;;; `reset-device-font-menus' to rebuild the menus from all currently
;;; available fonts.
;;;
;;; There is knowledge here about the regexp match numbers in `x-font-regexp',
;;; `x-font-regexp-foundry-and-family', and
;;; `x-font-regexp-registry-and-encoding' defined in x-faces.el.
;;;
;;; There are at least three kinds of fonts under X11r5:
;;;
;;; - bitmap fonts, which can be assumed to look as good as possible;
;;; - bitmap fonts which have been (or can be) automatically scaled to
;;;   a new size, and which almost always look awful;
;;; - and true outline fonts, which should look ok at any size, but in
;;;   practice (on at least some systems) look awful at any size, and
;;;   even in theory are unlikely ever to look as good as non-scaled
;;;   bitmap fonts.
;;;
;;; It would be nice to get this code to look for non-scaled bitmap fonts
;;; first, then outline fonts, then scaled bitmap fonts as a last resort.
;;; But it's not clear to me how to tell them apart based on their truenames
;;; and/or the result of XListFonts().  I welcome any and all explanations
;;; of the subtleties involved...
;;;
;;;
;;; If You Think You'Re Seeing A Bug:
;;; =================================
;;; When reporting problems, send the following information:
;;;
;;; - Exactly what behavior you're seeing;
;;; - The output of the `xlsfonts' program;
;;; - The value of the variable `device-fonts-cache';
;;; - The values of the following expressions, both before and after
;;;   making a selection from any of the fonts-related menus:
;;;	(face-font 'default)
;;;	(font-truename   (face-font 'default))
;;;	(font-properties (face-font 'default))
;;; - The values of the following variables after making a selection:
;;;	font-menu-preferred-resolution
;;;	font-menu-preferred-registry
;;;
;;; There is a common misconception that "*-courier-medium-r-*-11-*", also
;;; known as "-adobe-courier-medium-r-normal--11-80-100-100-m-60-iso8859-1",
;;; is an 11-point font.  It is not -- it is an 11-pixel font at 100dpi,
;;; which is an 8-point font (the number after -11- is the size in tenths
;;; of points).  So if you expect to be seeing an "11" entry in the "Size"
;;; menu and are not, this may be why.

;;; Code:

;; #### - implement these...
;;
;;; (defvar font-menu-ignore-proportional-fonts nil
;;;   "*If non-nil, then the font menu will only show fixed-width fonts.")

;;;###autoload
(defvar font-menu-ignore-scaled-fonts t
  "*If non-nil, then the font menu will try to show only bitmap fonts.")

;;;###autoload
(defvar font-menu-this-frame-only-p nil
  "*If non-nil, then changing the default font from the font menu will only
affect one frame instead of all frames.")

;; only call XListFonts (and parse) once per device.
;; ( (device . [parsed-list-fonts family-menu size-menu weight-menu]) ...)
(defvar device-fonts-cache nil)

(defconst font-menu-preferred-registry nil) 
(defconst font-menu-preferred-resolution nil)

(defconst fonts-menu-junk-families
  (purecopy
   (mapconcat
    #'identity
    '("cursor" "glyph" "symbol"	; Obvious losers.
      "\\`Ax...\\'"		; FrameMaker fonts - there are just way too
				;  many of these, and there is a different
				;  font family for each font face!  Losers.
				;  "Axcor" -> "Applix Courier Roman",
				;  "Axcob" -> "Applix Courier Bold", etc.
      )
    "\\|"))
  "A regexp matching font families which are uninteresting (e.g. cursor fonts).")

(defun hack-font-truename (fn)
  "Filter the output of `font-instance-truename' to deal with Japanese fontsets."
  (if (string-match "," (font-instance-truename fn))
      (let ((fpnt (nth 8 (split-string (font-instance-name fn) "-")))
	    (flist (split-string (font-instance-truename fn) ","))
	    ret)
	(while flist
	  (if (string-equal fpnt (nth 8 (split-string (car flist) "-")))
	      (progn (setq ret (car flist)) (setq flist nil))
	    (setq flist (cdr flist))
	    ))
	ret)
    (font-instance-truename fn)))

;;;###autoload
(fset 'install-font-menus 'reset-device-font-menus)
(make-obsolete 'install-font-menus 'reset-device-font-menus)

(defvar x-font-regexp-ja nil
  "This is used to filter out fonts that don't work in the locale.
It must be set at run-time.")

(defun vassoc (key valist)
  "Search VALIST for a vector whose first element is equal to KEY.
See also `assoc'."
  ;; by Stig@hackvan.com
  (let (el)
    (catch 'done
      (while (setq el (pop valist))
	(and (equal key (aref el 0))
	     (throw 'done el))))))

;;;###autoload
(defun reset-device-font-menus (&optional device debug)
  "Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system, 
or if you change your font path, you can call this to re-initialize the menus."
  ;; by Stig@hackvan.com
  ;; #### - this should implement a `menus-only' option, which would
  ;; recalculate the menus from the cache w/o having to do list-fonts again.
  (message "Getting list of fonts from server... ")
  (if (or noninteractive
	  (not (or device (setq device (selected-device))))
	  (not (eq (device-type device) 'x)))
      nil
    (if (and (getenv "LANG")
	     (string-match "^\\(ja\\|japanese\\)$"
			   (getenv "LANG")))
	;; #### - this is questionable behavior left over from the I18N4 code.
	(setq x-font-regexp-ja "jisx[^-]*-[^-]*$"
	      font-menu-preferred-registry '("*" . "*")
	      font-menu-preferred-resolution '("*" . "*")))
    (let ((all-fonts nil)
	  (case-fold-search t)
	  name family size weight entry monospaced-p
	  dev-cache
	  (cache nil)
	  (families nil)
	  (sizes nil)
	  (weights nil))
      (cond ((stringp debug)		; kludge
	     (setq all-fonts (split-string debug "\n")))
	    (t
	     (setq all-fonts
		   (or debug
		       (list-fonts "*-*-*-*-*-*-*-*-*-*-*-*-*-*" device)))))
      (while (setq name (pop all-fonts))
	(when (and (or (not x-font-regexp-ja)
		       (string-match x-font-regexp-ja name))
		   (string-match x-font-regexp name))
	  (setq weight (capitalize (match-string 1 name))
		size   (string-to-int (match-string 6 name)))
	  (or (string-match x-font-regexp-foundry-and-family name)
	      (error "internal error"))
	  (setq family (capitalize (match-string 1 name)))
	  (or (string-match x-font-regexp-spacing name)
	      (error "internal error"))
	  (setq monospaced-p (string= "m" (match-string 1 name)))
	  (unless (string-match fonts-menu-junk-families family)
	    (setq entry (or (vassoc family cache)
			    (car (setq cache
				       (cons (vector family nil nil t)
					     cache)))))
	    (or (member family families)
		(setq families (cons family families)))
	    (or (member weight weights)
		(setq weights (cons weight weights)))
	    (or (member weight (aref entry 1))
		(aset entry 1 (cons weight (aref entry 1))))
	    (or (member size sizes)
		(setq sizes (cons size sizes)))
	    (or (member size (aref entry 2))
		(aset entry 2 (cons size (aref entry 2))))
	    (aset entry 3 (and (aref entry 3) monospaced-p))
	    )))
      ;;
      ;; Hack scalable fonts.
      ;; Some fonts come only in scalable versions (the only size is 0)
      ;; and some fonts come in both scalable and non-scalable versions
      ;; (one size is 0).  If there are any scalable fonts at all, make
      ;; sure that the union of all point sizes contains at least some
      ;; common sizes - it's possible that some sensible sizes might end
      ;; up not getting mentioned explicitly.
      ;;
      (if (member 0 sizes)
	  (let ((common '(60 80 100 120 140 160 180 240)))
	    (while common
	      (or;;(member (car common) sizes)   ; not enough slack
	       (let ((rest sizes)
		     (done nil))
		 (while (and (not done) rest)
		   (if (and (> (car common) (- (car rest) 5))
			    (< (car common) (+ (car rest) 5)))
		       (setq done t))
		   (setq rest (cdr rest)))
		 done)
	       (setq sizes (cons (car common) sizes)))
	      (setq common (cdr common)))
	    (setq sizes (delq 0 sizes))))

      (setq families (sort families 'string-lessp)
	    weights (sort weights 'string-lessp)
	    sizes (sort sizes '<))

      (let ((rest cache))
	(while rest
	  (aset (car rest) 1 (sort (aref (car rest) 1) 'string-lessp))
	  (aset (car rest) 2 (sort (aref (car rest) 2) '<))
	  (setq rest (cdr rest))))

      (message "Getting list of fonts from server... done.")

      (setq dev-cache (assq device device-fonts-cache))
      (or dev-cache
	  (setq dev-cache (car (push (list device) device-fonts-cache))))
      (setcdr dev-cache
	      (vector
	       cache
	       (mapcar #'(lambda (x)
			   (vector x
				   (list 'font-menu-set-font x nil nil)
				   ':style 'radio ':active nil ':selected nil))
		       families)
	       (mapcar #'(lambda (x)
			   (vector (if (/= 0 (% x 10))
				       ;; works with no LISP_FLOAT_TYPE
				       (concat (int-to-string (/ x 10)) "."
					       (int-to-string (% x 10)))
				     (int-to-string (/ x 10)))
				   (list 'font-menu-set-font nil nil x)
				   ':style 'radio ':active nil ':selected nil))
		       sizes)
	       (mapcar #'(lambda (x)
			   (vector x
				   (list 'font-menu-set-font nil x nil)
				   ':style 'radio ':active nil ':selected nil))
		       weights)))
      (cdr dev-cache))))

(defsubst font-menu-truename (face)
  (hack-font-truename
   (if (featurep 'mule)
       (face-font-instance face nil 'ascii)
     (face-font-instance face))))

;;; Extract a font family from a face.
;;; Use the user-specified one if possible.
;;; If the user didn't specify one (with "*", for example)
;;; get the truename and use the guaranteed family from that.
(defun font-menu-family (face)
  (let ((dcache (cdr (assq (selected-device) device-fonts-cache)))
	(name (font-instance-name (face-font-instance face)))
	(family nil))
    (when (string-match x-font-regexp-foundry-and-family name)
      (setq family (capitalize (match-string 1 name))))
    (when (not (and family (vassoc family (aref dcache 0))))
      (setq name (font-menu-truename face))
      (string-match x-font-regexp-foundry-and-family name)
      (setq family (capitalize (match-string 1 name))))
    family))

;;;###autoload
(defun font-menu-family-constructor (ignored)
  ;; by Stig@hackvan.com
  (if (not (eq 'x (device-type (selected-device))))
      '(["Cannot parse current font" ding nil])
    (let* ((dcache (cdr (assq (selected-device) device-fonts-cache)))
	   (name (font-menu-truename 'default))
	   (case-fold-search t)
	   family weight size		; parsed from current font
	   entry			; font cache entry
	   f)
      (or dcache
	  (setq dcache (reset-device-font-menus (selected-device))))
      (if (not (string-match x-font-regexp name))
	  ;; couldn't parse current font
	  '(["Cannot parse current font" ding nil])
	(setq weight (capitalize (match-string 1 name)))
	(setq size (string-to-number (match-string 6 name)))
	(setq family (font-menu-family 'default))
	(setq entry (vassoc family (aref dcache 0)))
	(mapcar #'(lambda (item)
		    ;;
		    ;; Items on the Font menu are enabled iff that font
		    ;; exists in the same size and weight as the current
		    ;; font (scalable fonts exist in every size).  Only the
		    ;; current font is marked as selected.
		    ;;
		    (setq f (aref item 0)
			  entry (vassoc f (aref dcache 0)))
		    (if (and (member weight (aref entry 1))
			     (or (member size (aref entry 2))
				 (and (not font-menu-ignore-scaled-fonts)
				      (member 0 (aref entry 2)))))
			(enable-menu-item item)
		      (disable-menu-item item))
		    (if (equal family f)
			(select-toggle-menu-item item)
		      (deselect-toggle-menu-item item))
		    item)
		(aref dcache 1)))
      )))

;;;###autoload
(defun font-menu-size-constructor (ignored)
  ;; by Stig@hackvan.com
  (if (not (eq 'x (device-type (selected-device))))
      '(["Cannot parse current font" ding nil])
    (let ((dcache (cdr (assq (selected-device) device-fonts-cache)))
	  (name (font-menu-truename 'default))
	  (case-fold-search t)
	  family size			; parsed from current font
	  entry				; font cache entry
	  s)
      (or dcache
	  (setq dcache (reset-device-font-menus (selected-device))))
      (if (not (string-match x-font-regexp name))
	  ;; couldn't parse current font
	  '(["Cannot parse current font" ding nil])
	(setq size (string-to-number (match-string 6 name)))
	(setq family (font-menu-family 'default))
	(setq entry (vassoc family (aref dcache 0)))
	(mapcar
         (lambda (item)
           ;;
           ;; Items on the Size menu are enabled iff current font has
           ;; that size.  Only the size of the current font is
           ;; selected.  (If the current font comes in size 0, it is
           ;; scalable, and thus has every size.)
           ;;
           (setq s (nth 3 (aref item 1)))
           (if (or (member s (aref entry 2))
                   (and (not font-menu-ignore-scaled-fonts)
                        (member 0 (aref entry 2))))
               (enable-menu-item item)
             (disable-menu-item item))
           (if (eq size s)
               (select-toggle-menu-item item)
             (deselect-toggle-menu-item item))
           item)
         (aref dcache 2)))
      )))

;;;###autoload
(defun font-menu-weight-constructor (ignored)
  ;; by Stig@hackvan.com
  (if (not (eq 'x (device-type (selected-device))))
      '(["Cannot parse current font" ding nil])
    (let ((dcache (cdr (assq (selected-device) device-fonts-cache)))
	  (name (font-menu-truename 'default))
	  (case-fold-search t)
	  family weight			; parsed from current font
	  entry				; font cache entry
	  w)
      (or dcache
	  (setq dcache (reset-device-font-menus (selected-device))))
      (if (not (string-match x-font-regexp name))
	  ;; couldn't parse current font
	  '(["Cannot parse current font" ding nil])
	(setq weight (capitalize (match-string 1 name)))
	(setq family (font-menu-family 'default))
	(setq entry (vassoc family (aref dcache 0)))
	(mapcar #'(lambda (item)
		    ;; Items on the Weight menu are enabled iff current font
		    ;; has that weight.  Only the weight of the current font
		    ;; is selected.
		    (setq w (aref item 0))
		    (if (member w (aref entry 1))
			(enable-menu-item item)
		      (disable-menu-item item))
		    (if (equal weight w)
			(select-toggle-menu-item item)
		      (deselect-toggle-menu-item item))
		    item)
		(aref dcache 3)))
      )))


;;; Changing font sizes

(defun font-menu-set-font (family weight size)
  ;; This is what gets run when an item is selected from any of the three
  ;; fonts menus.  It needs to be rather clever.
  ;; (size is measured in 10ths of points.)
  (let ((faces (delq 'default (face-list)))
	(default-name (font-menu-truename 'default))
	(case-fold-search t)
	new-default-face-font
	from-family from-weight from-size)
    ;;
    ;; First, parse out the default face's font.
    ;;
    (setq from-family (font-menu-family 'default))
    (or (string-match x-font-regexp default-name)
	(signal 'error (list "couldn't parse font name" default-name)))
    (setq from-weight (capitalize (match-string 1 default-name)))
    (setq from-size (match-string 6 default-name))
    (setq new-default-face-font
	  (font-menu-load-font (or family from-family)
			       (or weight from-weight)
			       (or size   from-size)
			       default-name))
    (while faces
      (cond ((face-font-instance (car faces))
	     (message "Changing font of `%s'..." (car faces))
	     (condition-case c
		 (font-menu-change-face (car faces)
					from-family from-weight from-size
					family weight size)
	       (error
		(display-error c nil)
		(sit-for 1)))))
      (setq faces (cdr faces)))
    ;; Set the default face's font after hacking the other faces, so that
    ;; the frame size doesn't change until we are all done.

    ;;; WMP - we need to honor font-menu-this-frame-only-p here!
    (set-face-font 'default new-default-face-font
		   (and font-menu-this-frame-only-p (selected-frame)))
    (message "Font %s" (face-font-name 'default))))


(defun font-menu-change-face (face
			      from-family from-weight from-size
			      to-family   to-weight   to-size)
  (or (symbolp face) (signal 'wrong-type-argument (list 'symbolp face)))
  (let* ((name (font-menu-truename face))
	 (case-fold-search t)
	 face-family
	 face-weight
	 face-size)
    ;; First, parse out the face's font.
    (or (string-match x-font-regexp-foundry-and-family name)
	(signal 'error (list "couldn't parse font name" name)))
    (setq face-family (capitalize (match-string 1 name)))
    (or (string-match x-font-regexp name)
	(signal 'error (list "couldn't parse font name" name)))
    (setq face-weight (match-string 1 name))
    (setq face-size (match-string 6 name))

    ;; If this face matches the old default face in the attribute we
    ;; are changing, then change it to the new attribute along that
    ;; dimension.  Also, the face must have its own global attribute.
    ;; If its value is inherited, we don't touch it.  If any of this
    ;; is not true, we leave it alone.
    (if (and (face-font face 'global)
	     (cond 
	      (to-family (equal face-family from-family))
	      (to-weight (equal face-weight from-weight))
	      (to-size   (equal face-size from-size))))
	(set-face-font face
		       (font-menu-load-font (or to-family face-family)
					    (or to-weight face-weight)
					    (or to-size   face-size)
					    name)
		       (and font-menu-this-frame-only-p
			    (selected-frame)))
      nil)))


(defun font-menu-load-font (family weight size from-font)
  (and (numberp size) (setq size (int-to-string size)))
  (let ((case-fold-search t)
	slant other-slant
	registry encoding resx resy)
    (or (string-match x-font-regexp-registry-and-encoding from-font)
	(signal 'error (list "couldn't parse font name" from-font)))
    (setq registry (match-string 1 from-font)
	  encoding (match-string 2 from-font))

    (or (string-match x-font-regexp from-font)
	(signal 'error (list "couldn't parse font name" from-font)))
    (setq slant (capitalize (match-string 2 from-font))
	  resx  (match-string 7 from-font)
	  resy  (match-string 8 from-font))
    (setq other-slant (cond ((equal slant "O") "I") ; oh, bite me.
			    ((equal slant "I") "O")
			    (t nil)))
    ;;
    ;; Remember these values for the first font we switch away from
    ;; (the original default font).
    ;;
    (or font-menu-preferred-resolution
	(setq font-menu-preferred-resolution (cons resx resy)))
    (or font-menu-preferred-registry
	(setq font-menu-preferred-registry (cons registry encoding)))
    ;;
    ;; Now we know all the interesting properties of the font we want.
    ;; Let's see what we can actually *get*.
    ;;
    (or ;; First try the default resolution, registry, and encoding.
        (make-font-instance
	 (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
		 "-" (car font-menu-preferred-resolution)
		 "-" (cdr font-menu-preferred-resolution)
		 "-*-*-"
		 (car font-menu-preferred-registry) "-"
		 (cdr font-menu-preferred-registry))
	 nil t)
	;; Then try that in the other slant.
	(and other-slant
	     (make-font-instance
	      (concat "-*-" family "-" weight "-" other-slant
		      "-*-*-*-" size
		      "-" (car font-menu-preferred-resolution)
		      "-" (cdr font-menu-preferred-resolution)
		      "-*-*-"
		      (car font-menu-preferred-registry) "-"
		      (cdr font-menu-preferred-registry))
	      nil t))
	;; Then try the default resolution and registry, any encoding.
	(make-font-instance
	 (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
		 "-" (car font-menu-preferred-resolution)
		 "-" (cdr font-menu-preferred-resolution)
		 "-*-*-"
		 (car font-menu-preferred-registry) "-*")
	 nil t)
	;; Then try that in the other slant.
	(and other-slant
	     (make-font-instance
	      (concat "-*-" family "-" weight "-" other-slant
		      "-*-*-*-" size
		      "-" (car font-menu-preferred-resolution)
		      "-" (cdr font-menu-preferred-resolution)
		      "-*-*-"
		      (car font-menu-preferred-registry) "-*")
	      nil t))
	;; Then try the default registry and encoding, any resolution.
	(make-font-instance
	 (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
		 "-*-*-*-*-"
		 (car font-menu-preferred-registry) "-"
		 (cdr font-menu-preferred-registry))
	 nil t)
	;; Then try that in the other slant.
	(and other-slant
	     (make-font-instance
	      (concat "-*-" family "-" weight "-" other-slant
		      "-*-*-*-" size
		      "-*-*-*-*-"
		      (car font-menu-preferred-registry) "-"
		      (cdr font-menu-preferred-registry))
	      nil t))
	;; Then try the default registry, any encoding or resolution.
	(make-font-instance
	 (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
		 "-*-*-*-*-"
		 (car font-menu-preferred-registry) "-*")
	 nil t)
	;; Then try that in the other slant.
	(and other-slant
	     (make-font-instance
	      (concat "-*-" family "-" weight "-" slant "-*-*-*-"
		      size "-*-*-*-*-"
		      (car font-menu-preferred-registry) "-*")
	      nil t))
	;; Then try anything in the same slant, and error if it fails...
	(and other-slant
	     (make-font-instance
	      (concat "-*-" family "-" weight "-" slant "-*-*-*-"
		      size "-*-*-*-*-*-*")))
	(make-font-instance
	 (concat "-*-" family "-" weight "-" (or other-slant slant)
		 "-*-*-*-" size "-*-*-*-*-*-*"))
	)))

(defun flush-device-fonts-cache (device)
  ;; by Stig@hackvan.com
  (let ((elt (assq device device-fonts-cache)))
    (and elt
	 (setq device-fonts-cache (delq elt device-fonts-cache)))))

(add-hook 'delete-device-hook 'flush-device-fonts-cache)

(provide 'x-font-menu)

;;; x-font-menu.el ends here
