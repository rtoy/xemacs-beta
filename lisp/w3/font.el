;;; font.el --- New font model
;; Author: wmperry
;; Created: 1996/08/11 16:40:36
;; Version: 1.8
;; Keywords: faces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995, 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The emacsen compatibility package - load it up before anything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (load-library "w3-sysdp")
  (require 'cl))

(require 'disp-table)
(if (not (fboundp '<<))   (fset '<< 'lsh))
(if (not (fboundp '&))    (fset '& 'logand))
(if (not (fboundp '|))    (fset '| 'logior))
(if (not (fboundp '~))    (fset '~ 'lognot))
(if (not (fboundp '>>))   (defun >> (value count) (<< value (- count))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lots of variables / keywords for use later in the program
;;; Not much should need to be modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst font-running-xemacs (string-match "XEmacs" (emacs-version))
  "Whether we are running in XEmacs or not.")

(defmacro defkeyword (keyword &optional docstring)
  (list 'defconst keyword (list 'quote keyword)
	(or docstring "A keyword")))

(defconst font-window-system-mappings
  '((x        . (x-font-create-name x-font-create-object))
    (ns       . (ns-font-create-name ns-font-create-object))
    (tty      . (tty-font-create-plist tty-font-create-object)))
  "An assoc list mapping device types to the function used to create
a font name from a font structure.")

(defconst ns-font-weight-mappings
  '((:extra-light . "extralight")
    (:light       . "light")
    (:demi-light  . "demilight")
    (:medium      . "medium")
    (:normal      . "normal")
    (:demi-bold   . "demibold")
    (:bold        . "bold")
    (:extra-bold  . "extrabold"))
  "An assoc list mapping keywords to actual NeXTstep specific
information to use")

(defconst x-font-weight-mappings
  '((:extra-light . "extralight")
    (:light       . "light")
    (:demi-light  . "demilight")
    (:demi        . "demi")
    (:book        . "book")
    (:medium      . "medium")
    (:normal      . "normal")
    (:demi-bold   . "demibold")
    (:bold        . "bold")
    (:extra-bold  . "extrabold"))
  "An assoc list mapping keywords to actual Xwindow specific strings
for use in the 'weight' field of an X font string.")

(defconst font-possible-weights
  (mapcar 'car x-font-weight-mappings))

(defvar font-rgb-file nil
  "Where the RGB file was found.")

(defvar font-maximum-slippage "1pt"
  "How much a font is allowed to vary from the desired size.")

(defvar font-family-mappings
  '(
    ("serif"        . ("new century schoolbook"
		       "utopia"
		       "charter"
		       "times"
		       "lucidabright"
		       "garamond"
		       "palatino"
		       "times new roman"
		       "baskerville"
		       "bookman"
		       "bodoni"
		       "computer modern"
		       "rockwell"
		       ))
    ("sans-serif"   . ("lucida"
		       "helvetica"
		       "gills-sans"
		       "avant-garde"
		       "univers"
		       "optima"))
    ("elfin"        . ("tymes"))
    ("monospace"    . ("courier"
		       "fixed"
		       "lucidatypewriter"
		       "clean"
		       "terminal"))
    ("cursive"      . ("sirene"
		       "zapf chancery"))
    )
  "A list of font family mappings.")

(defkeyword :family "Keyword specifying the font family of a FONTOBJ.")

(defkeyword :weight "Keyword specifying the font weight of a FONTOBJ.")
 (defkeyword :extra-light)
 (defkeyword :light)
 (defkeyword :demi-light)
 (defkeyword :medium)
 (defkeyword :normal)
 (defkeyword :demi-bold)
 (defkeyword :bold)
 (defkeyword :extra-bold)

(defkeyword :style "Keyword specifying the font style of a FONTOBJ.")
(defkeyword :size "Keyword specifying the font size of a FONTOBJ.")
(defkeyword :registry "Keyword specifying the registry of a FONTOBJ.")
(defkeyword :encoding "Keyword specifying the encoding of a FONTOBJ.")

(eval-when-compile
  (defmacro define-new-mask (attr mask)
    (`
     (progn
       (defconst (, (intern (format "font-%s-mask" attr))) (<< 1 (, mask))
	 (, (format
	     "Bitmask for whether a font is to be rendered in %s or not."
	     attr)))
       (defun (, (intern (format "font-%s-p" attr))) (fontobj)
	 (, (format "Whether FONTOBJ will be renderd in `%s' or not." attr))
	 (if (/= 0 (& (font-style fontobj)
		      (, (intern (format "font-%s-mask" attr)))))
	     t
	   nil))
       (defun (, (intern (format "font-set-%s-p" attr))) (fontobj val)
	 (, (format "Set whether FONTOBJ will be renderd in `%s' or not."
		    attr))
	 (if val
	     (set-font-style fontobj (| (font-style fontobj)
					(, (intern
					    (format "font-%s-mask" attr)))))
	   (set-font-style fontobj (logxor (font-style fontobj)
					   (, (intern
					       (format "font-%s-mask"
						       attr)))))))
       ))))

(let ((mask 0))
  (define-new-mask bold        (setq mask (1+ mask)))
  (define-new-mask italic      (setq mask (1+ mask)))
  (define-new-mask oblique     (setq mask (1+ mask)))
  (define-new-mask dim         (setq mask (1+ mask)))
  (define-new-mask underline   (setq mask (1+ mask)))
  (define-new-mask overline    (setq mask (1+ mask)))
  (define-new-mask linethrough (setq mask (1+ mask)))
  (define-new-mask strikethru  (setq mask (1+ mask)))
  (define-new-mask reverse     (setq mask (1+ mask)))
  (define-new-mask blink       (setq mask (1+ mask)))
  (define-new-mask smallcaps   (setq mask (1+ mask)))
  (define-new-mask bigcaps     (setq mask (1+ mask)))
  (define-new-mask dropcaps    (setq mask (1+ mask))))

(defvar font-caps-display-table
  (let ((table (make-display-table))
	(i 0))
    ;; Standard ASCII characters
    (while (< i 26)
      (aset table (+ i ?a) (+ i ?A))
      (setq i (1+ i)))
    ;; Now ISO translations
    (setq i 224)
    (while (< i 247)			;; Agrave - Ouml
      (aset table i (- i 32))
      (setq i (1+ i)))
    (setq i 248)
    (while (< i 255)			;; Oslash - Thorn
      (aset table i (- i 32))
      (setq i (1+ i)))
    table))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unique (list)
  (let ((retval)
	(cur))
    (while list
      (setq cur (car list)
	    list (cdr list))
      (if (member cur retval)
	  nil
	(setq retval (cons cur retval))))
    (nreverse retval)))

(defun font-higher-weight (w1 w2)
  (let ((index1 (length (memq w1 font-possible-weights)))
	(index2 (length (memq w2 font-possible-weights))))
    (cond
     ((<= index1 index2)
      (or w1 w2))
     ((not w2)
      w1)
     (t
      w2))))

(defun font-spatial-to-canonical (spec &optional device)
  "Convert SPEC (in inches, millimeters, points, or picas) into pixels"
  ;; 1 in = 25.4 mm = 72 pt = 6 pa
  (if (numberp spec)
      spec
    (let ((num nil)
	  (type nil)
	  ;; If for any reason we get null for any of this, default
	  ;; to 1024x768 resolution on a 17" screen
	  (pix-width (float (or (device-pixel-width device) 1024)))
	  (mm-width (float (or (device-mm-width device) 293)))
	  (retval nil))
      (cond
       ((string-match "^ *\\([-+*/]\\) *" spec) ; math!  whee!
	(let ((math-func (intern (match-string 1 spec)))
	      (other (font-spatial-to-canonical
		      (substring spec (match-end 0) nil)))
	      (default (font-spatial-to-canonical
			(font-default-size-for-device device))))
	  (if (fboundp math-func)
	      (setq type "px"
		    spec (int-to-string (funcall math-func default other)))
	    (setq type "px"
		  spec (int-to-string other)))))
       ((string-match "[^0-9.]+$" spec)
	(setq type (substring spec (match-beginning 0))
	      spec (substring spec 0 (match-beginning 0))))
       (t
	(setq type "px"
	      spec spec)))
      (setq num (string-to-number spec))
      (cond
       ((member type '("pixel" "px" "pix"))
	(setq retval num
	      num nil))
       ((member type '("point" "pt"))
	(setq retval (+ (* (/ pix-width mm-width)
			   (/ 25.4 72.0)
			   num))))
       ((member type '("pica" "pa"))
	(setq retval (* (/ pix-width mm-width)
			(/ 25.4 6.0)
			num)))
       ((member type '("inch" "in"))
	(setq retval (* (/ pix-width mm-width)
			(/ 25.4 1.0)
			num)))
       ((string= type "mm")
	(setq retval (* (/ pix-width mm-width)
			num)))
       ((string= type "cm")
	(setq retval (* (/ pix-width mm-width)
			10
			num)))
       (t (setq retval num))
       )
      retval)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main interface routines - constructors and accessor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-font (&rest args)
  (vector :family
	  (if (stringp (nth 1 (memq :family args)))
	      (list (nth 1 (memq :family args)))
	    (nth 1 (memq :family args)))
	  :weight
	  (nth 1 (memq :weight args))
	  :style
	  (if (numberp (nth 1 (memq :style args)))
	      (nth 1 (memq :style args))
	    0)
	  :size
	  (nth 1 (memq :size args))
	  :registry
	  (nth 1 (memq :registry args))
	  :encoding
	  (nth 1 (memq :encoding args))))

(defsubst set-font-family (fontobj family)
  (aset fontobj 1 family))

(defsubst set-font-weight (fontobj weight)
  (aset fontobj 3 weight))

(defsubst set-font-style (fontobj style)
  (aset fontobj 5 style))

(defsubst set-font-size (fontobj size)
  (aset fontobj 7 size))

(defsubst set-font-registry (fontobj reg)
  (aset fontobj 9 reg))

(defsubst set-font-encoding (fontobj enc)
  (aset fontobj 11 enc))

(defsubst font-family (fontobj)
  (aref fontobj 1))

(defsubst font-weight (fontobj)
  (aref fontobj 3))

(defsubst font-style (fontobj)
  (aref fontobj 5))

(defsubst font-size (fontobj)
  (aref fontobj 7))

(defsubst font-registry (fontobj)
  (aref fontobj 9))

(defsubst font-encoding (fontobj)
  (aref fontobj 11))

(defun font-create-name (fontobj &optional device)
  (let* ((type (device-type device))
	 (func (car (cdr-safe (assq type font-window-system-mappings)))))
    (and func (fboundp func) (funcall func fontobj device))))

(defun font-create-object (fontname &optional device)
  (let* ((type (device-type device))
	 (func (car (cdr (cdr-safe (assq type font-window-system-mappings))))))
    (and func (fboundp func) (funcall func fontname device))))

(defun font-combine-fonts-internal (fontobj-1 fontobj-2)
  (let ((retval (make-font))
	(size-1 (and (font-size fontobj-1)
		     (font-spatial-to-canonical (font-size fontobj-1))))
	(size-2 (and (font-size fontobj-2)
		     (font-spatial-to-canonical (font-size fontobj-2)))))
    (set-font-weight retval (font-higher-weight (font-weight fontobj-1)
						(font-weight fontobj-2)))
    (set-font-family retval (unique (append (font-family fontobj-1)
					    (font-family fontobj-2))))
    (set-font-style retval (| (font-style fontobj-1) (font-style fontobj-2)))
    (set-font-registry retval (or (font-registry fontobj-1)
				  (font-registry fontobj-2)))
    (set-font-encoding retval (or (font-encoding fontobj-1)
				  (font-encoding fontobj-2)))
    (set-font-size retval (cond
			   ((and size-1 size-2 (>= size-2 size-1))
			    (font-size fontobj-2))
			   ((and size-1 size-2)
			    (font-size fontobj-1))
			   (size-1
			    (font-size fontobj-1))
			   (size-2
			    (font-size fontobj-2))
			   (t nil)))

    retval))

(defun font-combine-fonts (&rest args)
  (cond
   ((null args)
    (error "Wrong number of arguments to font-combine-fonts"))
   ((= (length args) 1)
    (car args))
   (t
    (let ((retval (font-combine-fonts-internal (nth 0 args) (nth 1 args))))
      (setq args (cdr (cdr args)))
      (while args
	(setq retval (font-combine-fonts-internal retval (car args))
	      args (cdr args)))
      retval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (TTY-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tty-font-create-object (fontname &optional device)
  )

(defun tty-font-create-plist (fontobj &optional device)
  (let ((styles (font-style fontobj))
	(weight (font-weight fontobj)))
    (list
     (cons 'underline (font-underline-p fontobj))
     (cons 'highlight (if (or (font-bold-p fontobj)
			      (memq weight '(:bold :demi-bold))) t))
     (cons 'dim       (font-dim-p fontobj))
     (cons 'blinking  (font-blink-p fontobj))
     (cons 'reverse   (font-reverse-p fontobj)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (X-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar font-x-font-regexp (or (and font-running-xemacs
				    (boundp 'x-font-regexp)
				    x-font-regexp)
 (let
     ((- 		"[-?]")
      (foundry		"[^-]*")
      (family 		"[^-]*")
      (weight		"\\(bold\\|demibold\\|medium\\|black\\)")
      (weight\?		"\\([^-]*\\)")
      (slant		"\\([ior]\\)")
      (slant\?		"\\([^-]?\\)")
      (swidth		"\\([^-]*\\)")
      (adstyle		"\\([^-]*\\)")
      (pixelsize	"\\(\\*\\|[0-9]+\\)")
      (pointsize	"\\(\\*\\|0\\|[0-9][0-9]+\\)")
      (resx		"\\([*0]\\|[0-9][0-9]+\\)")
      (resy		"\\([*0]\\|[0-9][0-9]+\\)")
      (spacing		"[cmp?*]")
      (avgwidth		"\\(\\*\\|[0-9]+\\)")
      (registry		"[^-]*")
      (encoding	"[^-]+")
      )
   (concat "\\`\\*?[-?*]"
	   foundry - family - weight\? - slant\? - swidth - adstyle -
	   pixelsize - pointsize - resx - resy - spacing - avgwidth -
	   registry - encoding "\\'"
	   ))))

(defun x-font-create-object (fontname &optional device)
  (if (or (not (stringp fontname))
	  (not (string-match font-x-font-regexp fontname)))
      (make-font)
    (let ((family nil)
	  (style nil)
	  (size nil)
	  (weight  (match-string 1 fontname))
	  (slant   (match-string 2 fontname))
	  (swidth  (match-string 3 fontname))
	  (adstyle (match-string 4 fontname))
	  (pxsize  (match-string 5 fontname))
	  (ptsize  (match-string 6 fontname))
	  (retval nil)
	  (case-fold-search t)
	  )
      (if (not (string-match x-font-regexp-foundry-and-family fontname))
	  nil
	(setq family (list (match-string 1 fontname))))
      (if (string= "*" weight)  (setq weight  nil))
      (if (string= "*" slant)   (setq slant   nil))
      (if (string= "*" swidth)  (setq swidth  nil))
      (if (string= "*" adstyle) (setq adstyle nil))
      (if (string= "*" pxsize)  (setq pxsize  nil))
      (if (string= "*" ptsize)  (setq ptsize  nil))
      (if ptsize (setq size (format "%dpt" (/ (string-to-int ptsize) 10))))
      (if (and (not size) pxsize) (setq size (concat pxsize "px")))
      (if weight (setq weight (intern-soft (concat ":" (downcase weight)))))
      (if (and adstyle (not (equal adstyle "")))
	  (setq family (append family (list adstyle))))
      (setq retval (make-font :family family
			      :weight weight
			      :size size))
      (font-set-bold-p retval (eq :bold weight))
      (cond
       ((null slant) nil)
       ((member slant '("i" "I"))
	(font-set-italic-p retval t))
       ((member slant '("o" "O"))
	(font-set-oblique-p retval t)))
      retval)))

(defun x-font-families-for-device (&optional device no-resetp)
  (condition-case ()
      (require 'x-font-menu)
    (error nil))
  (or device (setq device (selected-device)))
  (if (boundp 'device-fonts-cache)
      (let ((menu (or (cdr-safe (assq device device-fonts-cache)))))
	(if (and (not menu) (not no-resetp))
	    (progn
	      (reset-device-font-menus device)
	      (x-font-families-for-device device t))
	  (let ((scaled (mapcar (function (lambda (x) (if x (aref x 0))))
				(aref menu 0)))
		(normal (mapcar (function (lambda (x) (if x (aref x 0))))
				(aref menu 1))))
	    (sort (unique (nconc scaled normal)) 'string-lessp))))
    (mapcar 'car font-family-mappings)))

(defvar font-default-cache nil)

(defun font-default-font-for-device (&optional device)
  (or device (setq device (selected-device)))
  (if font-running-xemacs
      (font-truename
       (make-font-specifier
	(face-font-name 'default device)))
    (cdr-safe (assq 'font (frame-parameters device)))))

(defun font-default-object-for-device (&optional device)
  (let ((font (font-default-font-for-device device)))
    (or (cdr-safe 
	 (assoc font font-default-cache))
	(progn
	  (setq font-default-cache (cons (cons font
					       (font-create-object font))
					 font-default-cache))
	  (cdr-safe (assoc font font-default-cache))))))

(defun font-default-family-for-device (&optional device)
  (or device (setq device (selected-device)))
  (font-family (font-default-object-for-device device)))

(defun font-default-size-for-device (&optional device)
  (or device (setq device (selected-device)))
  ;; face-height isn't the right thing (always 1 pixel too high?)
  ;; (if font-running-xemacs
  ;;    (format "%dpx" (face-height 'default device))
  (font-size (font-default-object-for-device device)))
       
(defun x-font-create-name (fontobj &optional device)
  (if (and (not (or (font-family fontobj)
		    (font-weight fontobj)
		    (font-size fontobj)
		    (font-registry fontobj)
		    (font-encoding fontobj)))
	   (not (font-bold-p fontobj))
	   (not (font-italic-p fontobj)))
      (face-font 'default)
    (or device (setq device (selected-device)))
    (let ((family (or (font-family fontobj)
		      (font-default-family-for-device device)
		      (x-font-families-for-device device)))
	  (weight (or (font-weight fontobj) :medium))
	  (style (font-style fontobj))
	  (size (or (font-size fontobj) (font-default-size-for-device device)))
	  (registry (or (font-registry fontobj) "*"))
	  (encoding (or (font-encoding fontobj) "*")))
      (if (stringp family)
	  (setq family (list family)))
      (setq weight (font-higher-weight weight
				       (and (font-bold-p fontobj) :bold)))
      (if (stringp size)
	  (setq size (truncate (font-spatial-to-canonical size device))))
      (setq weight (or (cdr-safe (assq weight x-font-weight-mappings)) "*"))
      (let ((done nil)			; Did we find a good font yet?
	    (font-name nil)		; font name we are currently checking
	    (cur-family nil)		; current family we are checking
	    )
	(while (and family (not done))
	  (setq cur-family (car family)
		family (cdr family))
	  (if (assoc cur-family font-family-mappings)
	      ;; If the family name is an alias as defined by
	      ;; font-family-mappings, then append those families
	      ;; to the front of 'family' and continue in the loop.
	      (setq family (append
			    (cdr-safe (assoc cur-family
					     font-family-mappings))
			    family))
	    ;; Not an alias for a list of fonts, so we just check it.
	    ;; First, convert all '-' to spaces so that we don't screw up
	    ;; the oh-so wonderful X font model.  Wheee.
	    (let ((x (length cur-family)))
	      (while (> x 0)
		(if (= ?- (aref cur-family (1- x)))
		    (aset cur-family (1- x) ? ))
		(setq x (1- x))))
	    (setq font-name (format "-*-%s-%s-%s-*-*-%s-*-*-*-*-*-%s-%s"
				    cur-family weight
				    (if (font-italic-p fontobj)
					"i"
				      "r")
				    (if size (int-to-string size) "*")
				    registry
				    encoding
				    )
		  done (try-font-name font-name device))))
	(if done font-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (NS-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-font-families-for-device (&optional device no-resetp)
  ;; For right now, assume we are going to have the same storage for
  ;; device fonts for NS as we do for X.  Is this a valid assumption?
  (or device (setq device (selected-device)))
  (let ((menu (or (cdr-safe (assq device device-fonts-cache)))))
    (if (and (not menu) (not no-resetp))
	(progn
	  (reset-device-font-menus device)
	  (ns-font-families-for-device device t))
      (let ((scaled (mapcar (function (lambda (x) (if x (aref x 0))))
			    (aref menu 0)))
	    (normal (mapcar (function (lambda (x) (if x (aref x 0))))
			    (aref menu 1))))
	(sort (unique (nconc scaled normal)) 'string-lessp)))))

(defun ns-font-create-name (fontobj &optional device)
  (let ((family (or (font-family fontobj)
		    (ns-font-families-for-device device)))
	(weight (or (font-weight fontobj) :medium))
	(style (or (font-style fontobj) (list :normal)))
	(size (font-size fontobj))
	(registry (or (font-registry fontobj) "*"))
	(encoding (or (font-encoding fontobj) "*")))
    ;; Create a font, wow!
    (if (stringp family)
	(setq family (list family)))
    (if (symbolp style)
	(setq style (list style)))
    (setq weight (font-higher-weight weight (car-safe (memq :bold style))))
    (if (stringp size)
	(setq size (font-spatial-to-canonical size device)))
    (setq weight (or (cdr-safe (assq weight ns-font-weight-mappings))
		     "medium"))
    (let ((done nil)			; Did we find a good font yet?
	  (font-name nil)		; font name we are currently checking
	  (cur-family nil)		; current family we are checking
	  )
      (while (and family (not done))
	(setq cur-family (car family)
	      family (cdr family))
	(if (assoc cur-family font-family-mappings)
	    ;; If the family name is an alias as defined by
	    ;; font-family-mappings, then append those families
	    ;; to the front of 'family' and continue in the loop.
	    (setq family (append
			  (cdr-safe (assoc cur-family
					   font-family-mappings))
			  family))
	  ;; CARL: Need help here - I am not familiar with the NS font
	  ;; model
	  (setq font-name "UNKNOWN FORMULA GOES HERE"
		done (try-font-name font-name device))))
      (if done font-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now overwrite the original copy of set-face-font with our own copy that
;;; can deal with either syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-set-face-font (&optional face font &rest args)
  (if (interactive-p)
      (call-interactively 'font-original-set-face-font)
    (cond
     ((and (vectorp font) (= (length font) 12))
      (let ((font-name (font-create-name font)))
	(set-face-property face 'font-specification font)
	(cond
	 ((null font-name)		; No matching font!
	  nil)
	 ((listp font-name)		; For TTYs
	  (let (cur)
	    (while font-name
	      (setq cur (car font-name)
		    font-name (cdr font-name))
	      (apply 'set-face-property face (car cur) (cdr cur) args))))
	 (font-running-xemacs
	  (apply 'font-original-set-face-font face font-name args)
	  (apply 'set-face-underline-p face (font-underline-p font) args)
	  (if (and (or (font-smallcaps-p font) (font-bigcaps-p font))
		   (fboundp 'set-face-display-table))
	      (apply 'set-face-display-table
		     face font-caps-display-table args))
	  (apply 'set-face-property face 'strikethru (or
						      (font-linethrough-p font)
						      (font-strikethru-p font))
		 args))
	 (t
	  (condition-case nil
	      (apply 'font-original-set-face-font face font-name args)
	    (error
	     (let ((args (car-safe args)))
	       (and (or (font-bold-p font)
			(memq (font-weight font) '(:bold :demi-bold)))
		    (make-face-bold face args t))
	       (and (font-italic-p font) (make-face-italic face args t)))))
	  (apply 'set-face-underline-p face (font-underline-p font) args)))))
     (t
      ;; Let the original set-face-font signal any errors
      (set-face-property face 'font-specification nil)
      (apply 'font-original-set-face-font face font args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now for emacsen specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-update-device-fonts (device)
  ;; Update all faces that were created with the 'font' package
  ;; to appear correctly on the new device.  This should be in the
  ;; create-device-hook.  This is XEmacs 19.12+ specific
  (let ((faces (face-list 2))
	(cur nil)
	(font nil)
	(font-spec nil))
    (while faces
      (setq cur (car faces)
	    faces (cdr faces)
	    font-spec (face-property cur 'font-specification))
      (if font-spec
	  (set-face-font cur font-spec device)))))

(defun font-update-one-face (face &optional device-list)
  ;; Update FACE on all devices in DEVICE-LIST
  ;; DEVICE_LIST defaults to a list of all active devices
  (setq device-list (or device-list (device-list)))
  (if (devicep device-list)
      (setq device-list (list device-list)))
  (let* ((cur-device nil)
	 (font-spec (face-property face 'font-specification))
	 (font nil))
    (if (not font-spec)
	;; Hey!  Don't mess with fonts we didn't create in the
	;; first place.
	nil
      (while device-list
	(setq cur-device (car device-list)
	      device-list (cdr device-list))
	(if (not (device-live-p cur-device))
	    ;; Whoah!
	    nil
	  (if font-spec
	      (set-face-font face font-spec cur-device)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various color related things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((fboundp 'display-warning)
  (fset 'font-warn 'display-warning))
 ((fboundp 'w3-warn)
  (fset 'font-warn 'w3-warn))
 ((fboundp 'url-warn)
  (fset 'font-warn 'url-warn))
 ((fboundp 'warn)
  (defun font-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun font-warn (class message &optional level)
    (save-excursion
      (set-buffer (get-buffer-create "*W3-WARNINGS*"))
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))

(defun font-lookup-rgb-components (color)
  "Lookup COLOR (a color name) in rgb.txt and return a list of RGB values.
The list (R G B) is returned, or an error is signaled if the lookup fails."
  (let ((lib-list (if (boundp 'x-library-search-path)
		      x-library-search-path
		    ;; This default is from XEmacs 19.13 - hope it covers
		    ;; everyone.
		    (list "/usr/X11R6/lib/X11/"
			  "/usr/X11R5/lib/X11/"
			  "/usr/lib/X11R6/X11/"
			  "/usr/lib/X11R5/X11/"
			  "/usr/local/X11R6/lib/X11/"
			  "/usr/local/X11R5/lib/X11/"
			  "/usr/local/lib/X11R6/X11/"
			  "/usr/local/lib/X11R5/X11/"
			  "/usr/X11/lib/X11/"
			  "/usr/lib/X11/"
			  "/usr/local/lib/X11/"
			  "/usr/X386/lib/X11/"
			  "/usr/x386/lib/X11/"
			  "/usr/XFree86/lib/X11/"
			  "/usr/unsupported/lib/X11/"
			  "/usr/athena/lib/X11/"
			  "/usr/local/x11r5/lib/X11/"
			  "/usr/lpp/Xamples/lib/X11/"
			  "/usr/openwin/lib/X11/"
			  "/usr/openwin/share/lib/X11/")))
	(file font-rgb-file)
	r g b)
    (if (not file)
	(while lib-list
	  (setq file (expand-file-name "rgb.txt" (car lib-list)))
	  (if (file-readable-p file)
	      (setq lib-list nil
		    font-rgb-file file)
	    (setq lib-list (cdr lib-list)
		  file nil))))
    (if (null file)
	(list 0 0 0)
      (save-excursion
	(set-buffer (find-file-noselect file))
	(if (not (= (aref (buffer-name) 0) ? ))
	    (rename-buffer (generate-new-buffer-name " *rgb-tmp-buffer*")))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (if (re-search-forward (format "\t%s$" (regexp-quote color)) nil t)
		(progn
		  (beginning-of-line)
		  (setq r (* (read (current-buffer)) 256)
			g (* (read (current-buffer)) 256)
			b (* (read (current-buffer)) 256)))
	      (font-warn 'color (format "No such color: %s" color))
	      (setq r 0
		    g 0
		    b 0))
	    (list r g b) ))))))

(defun font-hex-string-to-number (string)
  "Convert STRING to an integer by parsing it as a hexadecimal number."
  (let ((conv-list '((?0 . 0) (?a . 10) (?A . 10)
		     (?1 . 1) (?b . 11) (?B . 11)
		     (?2 . 2) (?c . 12) (?C . 12)
		     (?3 . 3) (?d . 13) (?D . 13)
		     (?4 . 4) (?e . 14) (?E . 14)
		     (?5 . 5) (?f . 15) (?F . 15)
		     (?6 . 6) 
		     (?7 . 7)
		     (?8 . 8)
		     (?9 . 9)))
	(n 0)
	(i 0)
	(lim (length string)))
    (while (< i lim)
      (setq n (+ (* n 16) (or (cdr (assq (aref string i) conv-list)) 0))
	    i (1+ i)))
    n ))

(defun font-parse-rgb-components (color)
  "Parse RGB color specification and return a list of integers (R G B).
#FEFEFE and rgb:fe/fe/fe style specifications are parsed."
  (let ((case-fold-search t)
	r g b str)
  (cond ((string-match "^#[0-9a-f]+$" color)
	 (cond
	  ((= (length color) 4)
	   (setq r (font-hex-string-to-number (substring color 1 2))
		 g (font-hex-string-to-number (substring color 2 3))
		 b (font-hex-string-to-number (substring color 3 4))
		 r (* r 4096)
		 g (* g 4096)
		 b (* b 4096)))
	  ((= (length color) 7)
	   (setq r (font-hex-string-to-number (substring color 1 3))
		 g (font-hex-string-to-number (substring color 3 5))
		 b (font-hex-string-to-number (substring color 5 7))
		 r (* r 256)
		 g (* g 256)
		 b (* b 256)))
	  ((= (length color) 10)
	   (setq r (font-hex-string-to-number (substring color 1 4))
		 g (font-hex-string-to-number (substring color 4 7))
		 b (font-hex-string-to-number (substring color 7 10))
		 r (* r 16)
		 g (* g 16)
		 b (* b 16)))
	  ((= (length color) 13)
	   (setq r (font-hex-string-to-number (substring color 1 5))
		 g (font-hex-string-to-number (substring color 5 9))
		 b (font-hex-string-to-number (substring color 9 13))))
	  (t
	   (font-warn 'color (format "Invalid RGB color specification: %s"
				     color))
	   (setq r 0
		 g 0
		 b 0))))
	((string-match "rgb:\\([0-9a-f]+\\)/\\([0-9a-f]+\\)/\\([0-9a-f]+\\)"
		       color)
	 (if (or (> (- (match-end 1) (match-beginning 1)) 4)
		 (> (- (match-end 2) (match-beginning 2)) 4)
		 (> (- (match-end 3) (match-beginning 3)) 4))
	     (error "Invalid RGB color specification: %s" color)
	   (setq str (match-string 1 color)
		 r (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str))))
		 str (match-string 2 color)
		 g (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str))))
		 str (match-string 3 color)
		 b (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str)))))))
	(t
	 (font-warn 'html (format "Invalid RGB color specification: %s"
				color))
	 (setq r 0
	       g 0
	       b 0)))
  (list r g b) ))

(defsubst font-rgb-color-p (obj)
  (and (vectorp obj)
       (= (length obj) 4)
       (eq (aref obj 0) 'rgb)))

(defsubst font-rgb-color-red (obj) (aref obj 1))
(defsubst font-rgb-color-green (obj) (aref obj 2))
(defsubst font-rgb-color-blue (obj) (aref obj 3))

(defun font-color-rgb-components (color)
  "Return the RGB components of COLOR as a list of integers (R G B).
16-bit values are always returned.
#FEFEFE and rgb:fe/fe/fe style color specifications are parsed directly
into their components.
RGB values for color names are looked up in the rgb.txt file.
The variable x-library-search-path is use to locate the rgb.txt file."
  (let ((case-fold-search t))
    (cond
     ((font-rgb-color-p color)
      (list (* 65535 (font-rgb-color-red color))
	    (* 65535 (font-rgb-color-green color))
	    (* 65535 (font-rgb-color-blue color))))
     ((and (vectorp color) (= 3 (length color)) (floatp (aref color 0)))
      (list (* 65535 (aref color 0))
 	    (* 65535 (aref color 1))
 	    (* 65535 (aref color 2))))
     ((and (vectorp color) (= 3 (length color)))
      (list (aref color 0) (aref color 1) (aref color 2)))
     ((and (listp color) (= 3 (length color)) (floatp (car color)))
      (mapcar (function (lambda (x) (* x 65535))) color))
     ((and (listp color) (= 3 (length color)))
      color)
     ((or (string-match "^#" color)
	  (string-match "^rgb:" color))
      (font-parse-rgb-components color))
     ((string-match "\\([0-9.]+\\)[ \t]\\([0-9.]+\\)[ \t]\\([0-9.]+\\)"
		    color)
      (let ((r (string-to-number (match-string 1 color)))
	    (g (string-to-number (match-string 2 color)))
	    (b (string-to-number (match-string 3 color))))
	(if (floatp r)
	    (setq r (round (* 255 r))
		  g (round (* 255 g))
		  b (round (* 255 b))))
	(font-parse-rgb-components (format "#%02x%02x%02x" r g b))))
     (t
      (font-lookup-rgb-components color)))))

(defsubst font-tty-compute-color-delta (col1 col2)
  (+ 
   (* (- (aref col1 0) (aref col2 0))
      (- (aref col1 0) (aref col2 0)))
   (* (- (aref col1 1) (aref col2 1))
      (- (aref col1 1) (aref col2 1)))
   (* (- (aref col1 2) (aref col2 2))
      (- (aref col1 2) (aref col2 2)))))

(defun font-tty-find-closest-color (r g b)
  ;; This is basically just a lisp copy of allocate_nearest_color
  ;; from objects-x.c from Emacs 19
  ;; We really should just check tty-color-list, but unfortunately
  ;; that does not include any RGB information at all.
  ;; So for now we just hardwire in the default list and call it
  ;; good for now.
  (setq r (/ r 65535.0)
	g (/ g 65535.0)
	b (/ b 65535.0))
  (let* ((color_def (vector r g b))
	 (colors [([1.0 1.0 1.0] . "white")
		  ([0.0 1.0 1.0] . "cyan")
		  ([1.0 0.0 1.0] . "magenta")
		  ([0.0 0.0 1.0] . "blue")
		  ([1.0 1.0 0.0] . "yellow")
		  ([0.0 1.0 0.0] . "green")
		  ([1.0 0.0 0.0] . "red")
		  ([0.0 0.0 0.0] . "black")])
	 (no_cells (length colors))
	 (x 1)
	 (nearest 0)
	 (nearest_delta 0)
	 (trial_delta 0))
    (setq nearest_delta (font-tty-compute-color-delta (car (aref colors 0))
						      color_def))
    (while (/= no_cells x)
      (setq trial_delta (font-tty-compute-color-delta (car (aref colors x))
						      color_def))
      (if (< trial_delta nearest_delta)
	  (setq nearest x
		nearest_delta trial_delta))
      (setq x (1+ x)))
    (cdr-safe (aref colors nearest))))

(defun font-normalize-color (color &optional device)
  "Return an RGB tuple, given any form of input.  If an error occurs, black
is returned."
  (cond
   ((eq (device-type device) 'x)
    (apply 'format "#%04x%04x%04x" (font-color-rgb-components color)))
   ((eq (device-type device) 'tty)
    (apply 'font-tty-find-closest-color (font-color-rgb-components color)))
   ((eq (device-type device) 'ns)
    (let ((vals (mapcar (function (lambda (x) (>> x 8)))
			(font-color-rgb-components color))))
      (apply 'format "RGB%02x%02x%02ff" vals)))
   (t "black")))

(defun font-set-face-background (&optional face color &rest args)
  (interactive)
  (if (interactive-p)
      (call-interactively 'font-original-set-face-background)
    (cond
     ((font-rgb-color-p color)
      (apply 'font-original-set-face-background face
	     (font-normalize-color color) args))
     (t
      (apply 'font-original-set-face-background face color args)))))

(defun font-set-face-foreground (&optional face color &rest args)
  (interactive)
  (if (interactive-p)
      (call-interactively 'font-original-set-face-foreground)
    (cond
     ((font-rgb-color-p color)
      (apply 'font-original-set-face-foreground face
	     (font-normalize-color color) args))
     (t
      (apply 'font-original-set-face-foreground face color args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do the actual overwriting of some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro font-overwrite-fn (func)
  (` (let ((our-func (intern (format "font-%s" (, func))))
	   (new-func (intern (format "font-original-%s" (, func))))
	   (old-func (and (fboundp (, func)) (symbol-function (, func)))))
       (if (not (fboundp new-func))
	   (progn
	     (if old-func
		 (fset new-func old-func)
	       (fset new-func 'ignore))
	     (fset (, func) our-func))))))

(font-overwrite-fn 'set-face-foreground)
(font-overwrite-fn 'set-face-background)
(font-overwrite-fn 'set-face-font)

(provide 'font)
