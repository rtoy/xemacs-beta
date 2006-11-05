;;; x-font-menu.el --- Managing menus of X fonts.

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Sun Microsystems

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Restructured by: Jonathan Stigelman <Stig@hackvan.com>
;; Mule-ized by: Martin Buchholz
;; More restructuring for MS-Windows by Andy Piper <andy@xemacs.org>

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
;;; Code:

;; #### - implement these...
;;
;;; (defvar font-menu-ignore-proportional-fonts nil
;;;   "*If non-nil, then the font menu will only show fixed-width fonts.")

(require 'font-menu)

(when (featurep 'xft-fonts)
  (require 'fontconfig))

(globally-declare-boundp
 '(x-font-regexp
   x-font-regexp-foundry-and-family
   x-font-regexp-spacing))

(globally-declare-fboundp
 '(charset-registries))

(defvar x-font-menu-registry-encoding nil
  "Registry and encoding to use with font menu fonts.")

(defvar x-fonts-menu-junk-families
  (mapconcat
   #'identity
   '("cursor" "glyph" "symbol"	; Obvious losers.
     "\\`Ax...\\'"		; FrameMaker fonts - there are just way too
				;  many of these, and there is a different
				;  font family for each font face!  Losers.
				;  "Axcor" -> "Applix Courier Roman",
				;  "Axcob" -> "Applix Courier Bold", etc.
     )
   "\\|")
  "Regexp matching font families which should not be menu-selectable.
E.g. cursor fonts.")

(defun hack-font-truename (fn)
  ;; #### Are "font sets" XFontSets?
  ;; #### Is this useful if not configure'd --with-xfs?
  ;; #### This is duplicated in gtk-font-menu.el.
  "Filter the output of `font-instance-truename' to deal with font sets."
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

(defvar x-font-regexp-ascii nil
  "This is used to filter out font families that can't display ASCII text.
It must be set at run-time.")

;; #### move these to font-menu.el, and maybe make them defcustoms
(defvar font-menu-common-sizes
  '(60 80 100 110 120 130 140 150 160 170 180 200 220 240 300 360)
  "List of commonly desired font sizes in decipoints.")

;;;###autoload
(defun x-reset-device-font-menus (device &optional debug)
  (if (featurep 'xft-fonts)
      (x-reset-device-font-menus-xft device debug)
    (x-reset-device-font-menus-core device debug)))

(defun fc-make-font-menu-entry (family)
  (let ((weights (fc-find-available-weights-for-family family)))
    (vector
     family
     (mapcar 
      '(lambda (weight-symbol) 
	 (let ((pair (assoc weight-symbol
			    '((:light "Light")
			      (:medium "Medium")
			      (:demibold "Demibold")
			      (:bold "Bold")
			      (:black "Black")))))
	   (if pair (cadr pair))))
      weights)
     '(0)
     nil)))

(defun x-reset-device-font-menus-xft (device &optional debug)
  (let* ((families-1 (fc-find-available-font-families device))
	 (families (delete-if (lambda (x)
				(string-match x-fonts-menu-junk-families x))
			      (sort families-1 'string-lessp)))
	 (data
	  (vector
	   (mapcar 'fc-make-font-menu-entry families)
	   (mapcar 
	    '(lambda (family)
	       (vector family `(font-menu-set-font ,family nil nil)
		       :style 'radio :active nil :selected nil))
	    families)
	   (mapcar
	    '(lambda (size)
	       (vector
		(number-to-string size)
		`(font-menu-set-font nil nil ,size)
		:style 'radio :active nil :selected nil))
	    ;; common size list in decipoints, fontconfig wants points
	    (mapcar (lambda (x) (/ x 10)) font-menu-common-sizes))
	   (mapcar
	    '(lambda (weight)
	       (vector
		weight
		`(font-menu-set-font nil ,weight nil)
		:style 'radio :active nil :selected nil))
	    '("Light" "Medium" "Demibold" "Bold" "Black"))))
	 ;; get or initialize the entry for device
	 (dev-cache (or (assq device device-fonts-cache)
			(car (push (list device) device-fonts-cache)))))
    ;; update the device-fonts-cache entry for device in place
    (setcdr dev-cache data)
    data))

(defun x-reset-device-font-menus-core (device &optional debug)
  "Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system, 
or if you change your font path, you can call this to re-initialize the menus."
  ;; by Stig@hackvan.com
  ;; #### - this should implement a `menus-only' option, which would
  ;; recalculate the menus from the cache w/o having to do font-list again.
  (unless x-font-regexp-ascii
    (setq x-font-regexp-ascii (elt (charset-registries 'ascii) 0)))
  (setq x-font-menu-registry-encoding
	(if (featurep 'mule) "*-*" "iso8859-1"))
  (let ((case-fold-search t)
	family size weight entry monospaced-p
	dev-cache cache families sizes weights)
    (dolist (name (cond ((null debug)	; debugging kludge
			 (font-list "*-*-*-*-*-*-*-*-*-*-*-*-*-*" device
				     font-menu-max-number))
			((stringp debug) (split-string debug "\n"))
			(t debug)))
      (when (and (string-match x-font-regexp-ascii name)
		 (string-match x-font-regexp name))
	(setq weight (capitalize (match-string 1 name))
	      size   (string-to-int (match-string 6 name)))
	(or (string-match x-font-regexp-foundry-and-family name)
	    (error "internal error"))
	(setq family (capitalize (match-string 1 name)))
	(or (string-match x-font-regexp-spacing name)
	    (error "internal error"))
	(setq monospaced-p (string= "m" (match-string 1 name)))
	(unless (string-match x-fonts-menu-junk-families family)
	  (setq entry (or (vassoc family cache)
			  (car (setq cache
				     (cons (vector family nil nil t)
					   cache)))))
	  (or (member family families) (push family families))
	  (or (member weight weights)  (push weight weights))
	  (or (member size   sizes)    (push size   sizes))
	  (or (member weight (aref entry 1)) (push weight (aref entry 1)))
	  (or (member size   (aref entry 2)) (push size   (aref entry 2)))
	  (aset entry 3 (and (aref entry 3) monospaced-p)))))
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
	(let ((common font-menu-common-sizes))
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
	  weights  (sort weights 'string-lessp)
	  sizes    (sort sizes '<))
    
    (dolist (entry cache)
      (aset entry 1 (sort (aref entry 1) 'string-lessp))
      (aset entry 2 (sort (aref entry 2) '<)))

    (setq dev-cache (assq device device-fonts-cache))
    (or dev-cache
	(setq dev-cache (car (push (list device) device-fonts-cache))))
    (setcdr
     dev-cache
     (vector
      cache
      (mapcar (lambda (x)
		(vector x
			(list 'font-menu-set-font x nil nil)
			':style 'radio ':active nil ':selected nil))
	      families)
      (mapcar (lambda (x)
		(vector (if (/= 0 (% x 10))
			    (number-to-string (/ x 10.0))
			  (number-to-string (/ x 10)))
			(list 'font-menu-set-font nil nil x)
			':style 'radio ':active nil ':selected nil))
	      sizes)
      (mapcar (lambda (x)
		(vector x
			(list 'font-menu-set-font nil x nil)
			':style 'radio ':active nil ':selected nil))
	      weights)))
    (cdr dev-cache)))

;; Extract font information from a face.  We examine both the
;; user-specified font name and the canonical (`true') font name.
;; These can appear to have totally different properties.
;; For examples, see the prolog above.

;; We use the user-specified one if possible, else use the truename.
;; If the user didn't specify one (with "-dt-*-*", for example)
;; get the truename and use the possibly suboptimal data from that.
;;;###autoload
(defun x-font-menu-font-data (face dcache)
   (let* ((case-fold-search t)
 	 (domain (if font-menu-this-frame-only-p
		     (selected-frame)
		   (selected-device)))
	 (name (font-instance-name (face-font-instance face domain))))
     (if (featurep 'xft-fonts)
	 (if (xlfd-font-name-p name)
	     ;; #### this call to x-font-menu-font-data-core originally
	     ;; had 4 args, and that's probably the right way to go
	     (x-font-menu-font-data-core face dcache)
	   (x-font-menu-font-data-xft face dcache name (selected-device)))
       ;; #### this one, too
       (x-font-menu-font-data-core face dcache))))

(defun x-font-menu-font-data-xft (face dcache name domain)
  ;; DOMAIN is expected to be a device.
  (let* ((truename (font-instance-truename
		    (face-font-instance face domain
					(if (featurep 'mule) 'ascii))))
	 entry)
    (if (xlfd-font-name-p truename)
	(progn
	  nil)
      (progn
	(let* ((pattern (fc-font-match domain (fc-name-parse name)))
	       (family (and pattern
			    (fc-pattern-get-family pattern 0))))
	  (if (fc-pattern-get-successp family)
	    (setq entry (vassoc family (aref dcache 0))))
	  (if (null entry)
	      (make-vector 5 nil)
	    (let ((weight (fc-pattern-get-weight pattern 0))
		  (size (fc-pattern-get-size pattern 0))
		  (slant (fc-pattern-get-slant pattern 0)))
	      (vector
	       entry
	       (if (fc-pattern-get-successp family) 
		   family)
	       (if (fc-pattern-get-successp size)
		   size)
	       (if (fc-pattern-get-successp weight)
		   (fc-font-weight-translate-to-string weight))
	       (if (fc-pattern-get-successp slant)
		   (fc-font-slant-translate-to-string slant))))))))))

(defun x-font-menu-font-data-core (face dcache)
  (let* ((case-fold-search t)
	 (domain (if font-menu-this-frame-only-p
				  (selected-frame)
				(selected-device)))
	 (name (font-instance-name (face-font-instance face domain)))
	 (truename (font-instance-truename
		    (face-font-instance face domain
					(if (featurep 'mule) 'ascii))))
	 family size weight entry slant)
    (when (string-match x-font-regexp-foundry-and-family name)
      (setq family (capitalize (match-string 1 name)))
      (setq entry (vassoc family (aref dcache 0))))
    (when (and (null entry)
	       (string-match x-font-regexp-foundry-and-family truename))
      (setq family (capitalize (match-string 1 truename)))
      (setq entry  (vassoc family (aref dcache 0))))

    (if (null entry)
	(make-vector 5 nil)

      (when (string-match x-font-regexp name)
	(setq weight (capitalize    (match-string 1 name)))
	(setq size   (string-to-int (match-string 6 name))))
      
      (when (string-match x-font-regexp truename)
	(when (not (member weight (aref entry 1)))
	  (setq weight (capitalize (match-string 1 truename))))
	(when (not (member size   (aref entry 2)))
	  (setq size (string-to-int (match-string 6 truename))))
	(setq slant (capitalize (match-string 2 truename))))
      
      (vector entry family size weight slant))))

(defun x-font-menu-load-font (family weight size slant resolution)
  (if (featurep 'xft-fonts)
      (x-font-menu-load-font-xft family weight size slant resolution)
    (x-font-menu-load-font-core family weight size slant resolution)))

(defun x-font-menu-load-font-xft (family weight size slant resolution)
  (let ((pattern (make-fc-pattern)))
    (fc-pattern-add-family pattern family)
    (if weight 
	(fc-pattern-add-weight pattern
			       (fc-font-weight-translate-from-string weight)))
    (if size
	(fc-pattern-add-size pattern size))
    (if slant
	(fc-pattern-add-slant pattern
			      (fc-font-slant-translate-from-string slant)))
    (make-font-instance (fc-name-unparse pattern))))

(defun x-font-menu-load-font-core (family weight size slant resolution)
  "Try to load a font with the requested properties.
The weight, slant and resolution are only hints."
  (when (integerp size) (setq size (int-to-string size)))
  (let (font)
    (catch 'got-font
      (dolist (weight (list weight "*"))
	(dolist (slant
		 (cond ((string-equal slant "O") '("O" "I" "*"))
		       ((string-equal slant "I") '("I" "O" "*"))
		       ((string-equal slant "*") '("*"))
		       (t (list slant "*"))))
	  (dolist (resolution
		   (if (string-equal resolution "*-*")
		       (list resolution)
		     (list resolution "*-*")))
	    (when (setq font
			(make-font-instance
			 (concat  "-*-" family "-" weight "-" slant "-*-*-*-"
				  size "-" resolution "-*-*-"
				  x-font-menu-registry-encoding)
			 nil t))
	      (throw 'got-font font))))))))

(provide 'x-font-menu)

;;; x-font-menu.el ends here
