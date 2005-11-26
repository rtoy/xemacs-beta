;;; fontconfig.el --- New font model, NG

;; Copyright (c) 2003 Eric Knauel and Matthias Neubauer
;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Authors:	Eric Knauel <knauel@informatik.uni-tuebingen.de>
;;		Matthias Neubauer <neubauer@informatik.uni-freiburg.de>
;;		Stephen J. Turnbull <stephen@xemacs.org>
;; Created:	27 Oct 2003
;; Updated:	05 Mar 2005 by Stephen J. Turnbull
;; Keywords: faces

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

;;; Synched up with: Not in GNU

;;; Commentary:

;; This file is one of the pillars of the face refactoring effort
;; (another will be colorconfig.el, and there may be others).

;; The overall plan is to have a sensible modern model for values of
;; each of the components of a face (starting with fonts and colors),
;; implemented in a single module.  Of course we must be able to
;; convert such values to appropriate descriptors for any device type
;; on the one hand, but on the other it seems unreasonable to force
;; users to deal with a large number of different (and arcane, in the
;; case of XLFD) naming formats.

;; This file implements font specification.  We call a specification a
;; *pattern* to conform to fontconfig usage.  The internal
;; representation of a pattern will have Keith Packard's fontconfig
;; API.  For one, there is a robust and free C implementation, which
;; is available as a package for all platforms supported by X.org or
;; XFree86.  For another, it seems to be capable of representing any
;; specification of any of the font models I know.  Third, on X
;; platforms that internal representation can be passed verbatim to
;; libXft to get high quality TrueType fonts rendered with
;; anti-aliasing and hinting.

;; We will support the following user interfaces:

;;   1. fontconfig font names
;;   2. X Logical Font Descriptions (XLFD)
;;   3. MS Windows font names
;;   4. Mac OS X font names

;; and possibly others (such as ad hoc abbreviations used in older X11
;; implementations).  This is called the *fontname UI* (for the
;; platform) to distinguish it from XEmacs's internal model
;; (fontconfig patterns) and the API for rendering engines (called the
;; *font API* for the engine).

;; We will support the following rendering engine APIs:

;;   1. fontconfig patterns (the native language of Xft); to emphasize
;;      the engine-specific nature, we will call these *Xft fonts*
;;   2. XLFD strings
;;   3. MS Windows font names

;; and possibly others (such as Mac OS X font names).  This is called
;; the *font API* (for the platform) to distinguish it from XEmacs's
;; internal model (fontconfig *patterns*) and the names used by users
;; (called the *fontname UI* for the platform).


;; TODO (possible enhancements)
;; 1.  add a more complete docstring for properties as such (would be a
;;     hash table?) to describe things like special symbolic values, and
;;     Emacs-relevant semantics
;; 2.  add a special value defining macro for constants

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The fontconfig pattern API
;;
;; The basic interfaces are defined as API wrappers in C in xft-font.c
;; #### which should be renamed to fontconfig.c.  These are prefixed
;; with "fc-pattern-".  These are
;;
;;   fc-pattern-p
;;   fc-pattern-create
;;   fc-pattern-duplicate
;;   fc-pattern-add
;;   fc-pattern-del
;;   fc-pattern-get
;;   fc-pattern-destroy

;; We provide a LISP-y alias, `make-fc-pattern' for the pattern
;; constructor function `fc-pattern-create'.  #### It might make sense
;; to generalize `make-fc-pattern' by allowing a plist of properties
;; as an optional argument.  We also provide accessors
;; `fc-pattern-get-PROPERTY' and mutators `fc-pattern-add-PROPERTY'
;; for each of the standard properties used by Xft, which overlap
;; substantially with the properties defined by X11.  #### We probably
;; should provide `fc-pattern-delete-PROPERTY', too.

(defalias 'make-fc-pattern 'fc-pattern-create)

(defmacro fc-define-property (property type docfrag &optional obsolete-p)
  "Define PROPERTY as a fontconfig font property of type TYPE using DOCFRAG.

A font property is a key in a fontconfig pattern and is associated with
one or more values of a given type.  This macro creates wrappers around
`fc-pattern-get' and `fc-pattern-add' for PROPERTY.  Wrappers are
preferred to use of primitives with a string as the OBJECT argument because
typos in wrappers result in \"not fboundp\" errors, while a typo in a string
produces a silent null return.

PROPERTY is a string.
TYPE is a symbol indicating the type of the property value.  It is used only
to modify formatting of the wrapper function docstrings.
DOCFRAG is a string which briefly describes the use of the property, and is
interpolated into a format to create the doctstrings.
OBSOLETE-P if non-nil marks the property as pertaining only to older versions
of fontconfig or Xft.  This merely adds a deprecation to the docstrings.

This macro defines an accessor named `fc-pattern-get-PROPERTY' which takes
a fontconfig pattern object and an integer as arguments, and returns the
value associated with PROPERTY and ID in the pattern object.  Since it is
not possible to associate a value to PROPERTY for a particular ID, it is
not very useful to interpret the values associated with a given id for
different properties as being linked to each other in some way.

A mutator `fc-pattern-add-PROPERTY' which takes a fontconfig pattern object
and a value as arguments, and adds the value to the property with the next
id.  The type of the value is recognized by `fc-pattern-add', and the id
is chosen by the fontconfig implementation."

  `(progn
    (defsubst ,(intern (concat "fc-pattern-get-" property))
      (pattern id)
      ,(format "\
Return %s %s fc pattern PATTERN %s.%s

This function is a convenience wrapper for `fc-pattern-get'.
See `fc-pattern-get' for documentation of patterns, ids, and error returns."
	       (if (eq type 'boolean)
		   "t"
		 docfrag)
	       (if (eq type 'boolean)
		   "if"
		 "associated with id ID in")
	       (if (eq type 'boolean)
		   docfrag
		 (format "as a%s %s" (if (eq type 'integer) "n" "") type))
	       (if obsolete-p "
\(Obsolete, only available on systems using Xft version 1.)"
		 ""))
      (fc-pattern-get pattern ,property id))

    (defsubst ,(intern (concat "fc-pattern-add-" property))
      (pattern value)
      ,(format "\
Add VALUE to the %s property of fontconfig pattern PATTERN.%s

The type of VALUE should be %s.

This function is a convenience wrapper for `fc-pattern-add'.
See `fc-pattern-add' for documentation of patterns, values, and error returns."
	       property
	       (if obsolete-p "
\(Obsolete, only available on systems using Xft version 1.)"
		 "")
	       type)
      (fc-pattern-add pattern ,property value))
    ,property))

;; define the standard properties for Xft v.2 here
(fc-define-property "antialias" boolean "the font supports antialiasing")
(fc-define-property "dpi" float "the design resolution")
(fc-define-property "family" string "the font family")
(fc-define-property "file" string "the file containing glyph data")
(fc-define-property "foundry" string "the vendor")
(fc-define-property "index" integer "the index of the glyph set")
(fc-define-property "minspace" boolean "has a minimum spacing")
(fc-define-property "outline" boolean "is an outline font")
(fc-define-property "pixelsize" float "the size in pixels")
(fc-define-property "rasterizer" string "the name of the rasterizing engine")
(fc-define-property "rgba" integer "the subpixel rendering capabilities")
(fc-define-property "scalable" boolean "is scalable")
(fc-define-property "scale" float "the scale factor")
(fc-define-property "size" float "the size in points")
(fc-define-property "slant" integer "the slant")
(fc-define-property "spacing" integer "the spacing model")
(fc-define-property "style" string "the typographic face or style")
(fc-define-property "weight" integer "the weight")
(fc-define-property "xlfd" string "the XLFD (full name in X11)")

;; Xft v.1 properties (marked as obsolete)
(fc-define-property "encoding" string "the encoding" t)
(fc-define-property "charwidth" integer "the average character width" t)
(fc-define-property "charheight" integer "the average character height" t)
(fc-define-property "core" boolean "represents a core font" t)
(fc-define-property "render" boolean "represents a render (Xft) font" t)


(defconst fc-font-name-property-family "family")
(defconst fc-font-name-property-style "style")
(defconst fc-font-name-property-slant "slant")
(defconst fc-font-name-property-weight "weight")
(defconst fc-font-name-property-size "size")
(defconst fc-font-name-property-pixelsize "pixelsize")
(defconst fc-font-name-property-spacing "spacing")
(defconst fc-font-name-property-foundry "foundry")
(defconst fc-font-name-property-antialias "antialias")
(defconst fc-font-name-property-xlfd "xlfd")
(defconst fc-font-name-property-file "file")
(defconst fc-font-name-property-index "index")
(defconst fc-font-name-property-rasterizer "rasterizer")
(defconst fc-font-name-property-outline "outline")
(defconst fc-font-name-property-scalable "scalable")
(defconst fc-font-name-property-rgba "rgba")
(defconst fc-font-name-property-minspace "minspace")
(defconst fc-font-name-property-dpi "dpi")

;; Xft version 1 only
;;(defconst fc-font-name-property-encoding "encoding")
;;(defconst fc-font-name-property-charwidth "charwidth")
;;(defconst fc-font-name-property-charheight "charheight")
;;(defconst fc-font-name-property-core "core")
;;(defconst fc-font-name-property-render "render")


(defconst fc-pattern-selector-mapping
  `((,fc-font-name-property-family        . fc-pattern-get-family)
    (,fc-font-name-property-style         . fc-pattern-get-style)
    (,fc-font-name-property-slant         . fc-pattern-get-slant)
    (,fc-font-name-property-weight        . fc-pattern-get-weight)
    (,fc-font-name-property-size          . fc-pattern-get-size)
    (,fc-font-name-property-pixelsize     . fc-pattern-get-pixelsize)
    (,fc-font-name-property-spacing       . fc-pattern-get-spacing)
    (,fc-font-name-property-foundry       . fc-pattern-get-foundry)
    (,fc-font-name-property-antialias     . fc-pattern-get-antialias)
    (,fc-font-name-property-xlfd          . fc-pattern-get-xlfd)
    (,fc-font-name-property-file          . fc-pattern-get-file)
    (,fc-font-name-property-index         . fc-pattern-get-index)
    (,fc-font-name-property-rasterizer    . fc-pattern-get-rasterizer)
    (,fc-font-name-property-outline       . fc-pattern-get-outline)
    (,fc-font-name-property-scalable      . fc-pattern-get-scalable)
    (,fc-font-name-property-rgba          . fc-pattern-get-rgba)
    (,fc-font-name-property-minspace      . fc-pattern-get-minspace)
    (,fc-font-name-property-dpi           . fc-pattern-get-dpi)
    ;; Xft version 1 only
    ;; (,fc-font-name-property-encoding      . fc-pattern-get-encoding)
    ;; (,fc-font-name-property-charwidth     . fc-pattern-get-char-width)
    ;; (,fc-font-name-property-charheight    . fc-pattern-get-char-height)
    ;; (,fc-font-name-property-core          . fc-pattern-get-core)
    ;; (,fc-font-name-property-render        . fc-pattern-get-render)
    ))

(defvar fc-find-available-font-families-fc-fonts-only t
  "If `fc-find-available-font-families-fc-fonts-only' is set to `t',
`fc-find-available-font-families' will ignore core fonts.")

(defconst fc-font-name-slant-roman 0)
(defconst fc-font-name-slant-italic 100)
(defconst fc-font-name-slant-oblique 110)

(defconst fc-font-name-slant-mapping
  `((,fc-font-name-slant-roman   . :roman)
    (,fc-font-name-slant-italic  . :italic)
    (,fc-font-name-slant-oblique . :oblique)))

(defconst fc-font-name-slant-mapping-string
  `((,fc-font-name-slant-roman   . "R")
    (,fc-font-name-slant-roman   . "I")
    (,fc-font-name-slant-roman   . "O")))

(defconst fc-font-name-slant-mapping-string-reverse
  `(("R" . ,fc-font-name-slant-roman)
    ("I" . ,fc-font-name-slant-italic)
    ("O" . ,fc-font-name-slant-oblique)))

(defconst fc-font-name-slant-mapping-reverse
  `((:roman   . ,fc-font-name-slant-roman)
    (:italic  . ,fc-font-name-slant-italic)
    (:oblique . ,fc-font-name-slant-oblique)))

(defun fc-font-slant-translate-from-constant (number)
  "Translate the Xft font slant constant NUMBER to symbol."
  (let ((pair (assoc number fc-font-name-slant-mapping)))
    (if pair (cdr pair))))

(defun fc-font-slant-translate-from-symbol (symbol)
  "Translate SYMBOL (`:roman', `:italic' or `:oblique') to the
corresponding Xft font slant constant."
  (let ((pair (assoc symbol fc-font-name-slant-mapping-reverse)))
    (if pair (cdr pair))))

(defun fc-font-slant-translate-to-string (num-or-sym)
  (let* ((constant (if (symbolp num-or-sym)
		       (cdr (assoc num-or-sym fc-font-name-slant-mapping-reverse))
		     num-or-sym))
	 (pair (assoc constant fc-font-name-slant-mapping-string)))
    (if pair (cdr pair))))

(defun fc-font-slant-translate-from-string (str)
  (let ((pair (assoc str fc-font-name-slant-mapping-string-reverse)))
    (if pair (cdr pair))))

(defconst fc-font-name-weight-light 0)
(defconst fc-font-name-weight-regular 80)
(defconst fc-font-name-weight-medium 100)
(defconst fc-font-name-weight-demibold 180)
(defconst fc-font-name-weight-bold 200)
(defconst fc-font-name-weight-black 210)

(defconst fc-font-name-weight-mapping
  `((,fc-font-name-weight-light    . :light)
    (,fc-font-name-weight-regular  . :regular)
    (,fc-font-name-weight-medium   . :medium)
    (,fc-font-name-weight-demibold . :demibold)
    (,fc-font-name-weight-bold     . :bold)
    (,fc-font-name-weight-black    . :black)))

(defconst fc-font-name-weight-mapping-string
  `((,fc-font-name-weight-light    . "Light")
    (,fc-font-name-weight-regular  . "Regular")
    (,fc-font-name-weight-medium   . "Medium")
    (,fc-font-name-weight-demibold . "Demibold")
    (,fc-font-name-weight-bold     . "Bold")
    (,fc-font-name-weight-black    . "Black")))

(defconst fc-font-name-weight-mapping-string-reverse
  `(("Light"    . ,fc-font-name-weight-light)
    ("Regular"  . ,fc-font-name-weight-regular)
    ("Medium"   . ,fc-font-name-weight-medium)
    ("Demibold" . ,fc-font-name-weight-demibold)
    ("Bold"     . ,fc-font-name-weight-bold)
    ("Black"    . ,fc-font-name-weight-black)))

(defconst fc-font-name-weight-mapping-reverse
  `((:light    . ,fc-font-name-weight-light)
    (:regular  . ,fc-font-name-weight-regular)
    (:medium   . ,fc-font-name-weight-medium)
    (:demibold . ,fc-font-name-weight-demibold)
    (:bold     . ,fc-font-name-weight-bold)
    (:black    . ,fc-font-name-weight-black)))

(defun fc-font-weight-translate-from-constant (number)
  "Translate a Xft font weight constant NUMBER to symbol."
  (let ((pair (assoc number fc-font-name-weight-mapping)))
    (if pair (cdr pair))))

(defun fc-font-weight-translate-from-symbol (symbol) 
  "Translate SYMBOL (`:light', `:regular', `:medium', `:demibold',
`:bold' or `:black') to the corresponding Xft font weight constant."
  (let ((pair (assoc symbol fc-font-name-weight-mapping-reverse)))
    (if pair (cdr pair))))

(defun fc-font-weight-translate-to-string (num-or-sym)
  (let* ((constant (if (symbolp num-or-sym)
		       (cdr (assoc num-or-sym fc-font-name-weight-mapping-reverse))
		     num-or-sym))
	 (pair (assoc constant fc-font-name-weight-mapping-string)))
    (if pair (cdr pair))))

(defun fc-font-weight-translate-from-string (str)
  (let ((pair (assoc str fc-font-name-weight-mapping-string-reverse)))
    (if pair (cdr pair))))

(defun fc-copy-pattern-partial (pattern attribute-list)
  "Return a copy of PATTERN restricted to ATTRIBUTE-LIST.

PATTERN is a fontconfig pattern object.
ATTRIBUTE-LIST is a list of strings denoting font properties.
A new object is allocated and returned."
  (let ((new (make-fc-pattern))
	(attrs attribute-list))
    ;;; We demand proper tail recursion!
    (while (not (null attrs))
      (let ((get (intern (concat "fc-pattern-get-" (car attrs))))
	    (set (intern (concat "fc-pattern-add-" (car attrs)))))
	(if (and (fboundp get) (fboundp set))
	    (funcall set new (funcall get pattern 0))
	  (warn "property '%s' not defined, ignoring" (car attrs))))
      (setq attrs (cdr attrs)))
    new))

(defun fc-pattern-get-all-attributes (fc-pattern fc-pattern-get-function)
  (let ((count 0)
	res end val)
    (while (not end)
      (setq val (funcall fc-pattern-get-function fc-pattern count))
      (if (or (equal val 'fc-result-no-id)
	      (equal val 'fc-result-no-match))
	  (setq end t)
	(setq res (append res (list val))
	      count (+ count 1))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The fontconfig fontname UI
;;
;; The basic interfaces are defined as API wrappers in C in xft-font.c
;; #### which should be renamed to fontconfig.c.  These are prefixed
;; with "fc-name-".  These are
;;
;;   fc-name-parse
;;   fc-name-unparse
;;   xft-name-unparse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The fontconfig font API
;;
;; The basic interfaces are defined as API wrappers in C in xft-font.c
;; #### which should be renamed to fontconfig.c.  These are prefixed
;; with "fc-font-".  These are
;;
;;   fc-font-match
;;   fc-list-fonts-pattern-objects
;;   fc-font-sort
;;   fc-font-real-pattern

;; #### it might make sense to generalize `fc-try-font' by having a
;; global variable that contains a list of font name parsers.  They are
;; tried in order, and the first one to return an fc-pattern is matched.

(defun fc-try-font (font &optional device)
  "Return list of pattern objects matching FONT on DEVICE.

FONT may be a fontconfig pattern object or a fontconfig font name (a string).
Optional DEVICE is the device object to query, defaulting to the currently
selected device."
  (fc-list-fonts-pattern-objects (or device (default-x-device))
				 (if (fc-pattern-p font) 
				     font
				   (fc-name-parse font))
				 nil))

(defun fc-find-available-font-families (&optional device filter-fun)
  "Find all available font families."
  (let ((device (or device (default-x-device)))
	(pattern (make-fc-pattern))
	(objectset '("family" "style")))
;     Xft2: does not work anymore
;     (if (not fc-find-available-font-families-fc-fonts-only)
; 	(fc-pattern-add pattern fc-font-name-property-core t))
;     (fc-objectset-add objectset fc-font-name-property-encoding)
    (let* ((all-fonts
	    (fc-list-fonts-pattern-objects device pattern objectset)))
      (fc-delete-duplicates
       (mapcar
	'(lambda (pattern)
	   (fc-pattern-get-family pattern 0))
	(if filter-fun
	    (fc-filter all-fonts filter-fun)
	  all-fonts))))))

; Xft2: does not work anymore
; (defun fc-find-available-font-families-non-mule (&optional device)
;   (fc-find-available-font-families
;    device
;    '(lambda (pattern)
;       (let ((encodings (fc-pattern-get-all-attributes 
; 			pattern 'fc-pattern-get-encoding)))
; 	;; Be sure that the font support ISO-8859-1
; 	(member "iso8859-1" encodings)))))

(defun fc-find-available-weights-for-family (family &optional style device)
  "Find available weights for font FAMILY."
  (let* ((device (or device (default-x-device)))
	 (pattern (make-fc-pattern))
	 (objectset '("weight")))
    (fc-pattern-add pattern fc-font-name-property-family family)
    (if style
	(fc-pattern-add pattern fc-font-name-property-style style))
    (mapcar
     '(lambda (pattern)
	(let ((fc-weight-constant (fc-pattern-get-weight pattern 0)))
	  (if fc-weight-constant
	      (fc-font-weight-translate-from-constant fc-weight-constant))))
     (fc-list-fonts-pattern-objects device pattern objectset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The XLFD fontname UI
;;

;;   xlfd-font-name-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions
;;

(defun fc-pattern-get-successp (result)
  (and (not (equal result 'fc-result-no-match))
       (not (equal result 'fc-result-no-id))
       (not (equal result 'fc-internal-error))))

;;; DELETE-DUPLICATES and REMOVE-DUPLICATES from cl-seq.el do not
;;; seem to work on list of strings...
;;; #### Presumably just use :test 'equal!
(defun fc-delete-duplicates (l)
  (let ((res nil)
	(in l))
    (while (not (null in))
      (if (not (member (car in) res))
	  (setq res (append res (list (car in)))))
      (setq in (cdr in)))
    res))

;; #### Use delete-if with :test 'equal.
(defun fc-filter (l fun)
  (let ((res nil)
	(in l))
    (while (not (null in))
      (if (funcall fun (car in))
	  (setq res (append res (list (car in)))))
      (setq in (cdr in)))
    res))

(provide 'fontconfig)

;;; fontconfig.el ends here
