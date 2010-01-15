;;; font-mgr.el --- Lisp emulation of fontconfig for X fonts.

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author:	Stephen J. Turnbull <stephen@xemacs.org>
;; Created:	12 Apr 2006 by Stephen J. Turnbull

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Synched up with: Not in GNU Emacs.

;; Commentary
;; This module provides the Lisp interface to fonts in X11, including Xft,
;; but (at least at first) not GTK+ or Qt.  It is a substitute for the
;; C library fontconfig, whose interface is provided in ../src/font-mgr.c.

;; static FcChar8 *fc_standard_properties[] = {
;;   "antialias", "aspect", "autohint", "charset", "dpi", "family", "file",
;;   "foundry", "ftface", "globaladvance", "hinting", "index", "lang",
;;   "minspace", "outline", "pixelsize", "rasterizer", "rgba", "scalable",
;;   "scale", "size", "slant", "spacing", "style", "verticallayout", "weight",
;;   /* obsolete after Xft v. 1 */
;;   "charwidth", "charheight", "core", "encoding", "render"

;; #### should we wrap the world in `(unless (featurep 'font-mgr) ... )'?

(provide 'font-mgr)

(defvar xft-xlfd-font-regexp
  (concat
   ;; XLFD specifies ISO 8859-1 encoding, but we can't handle non-ASCII
   ;; in Mule when this function is called.  So use HPC.
   ;; (xe_xlfd_prefix "\\(\\+[\040-\176\240-\377]*\\)?-")
   ;; (xe_xlfd_opt_text "\\([\040-\044\046-\176\240-\377]*\\)")
   ;; (xe_xlfd_text "\\([\040-\044\046-\176\240-\377]+\\)")
   "\\`"
   "\\(\\+[\040-\176]*\\)?-"		; prefix
   "\\([^-]+\\)"			; foundry
   "-"
   "\\([^-]+\\)"			; family
   "-"
   "\\([^-]+\\)"			; weight
   "-"
   "\\([0-9ior?*][iot]?\\)"		; slant
   "-"
   "\\([^-]+\\)"			; swidth
   "-"
   "\\([^-]*\\)"			; adstyle
   "-"
   "\\([0-9?*]+\\|\\[[ 0-9+~.e?*]+\\]\\)"    ; pixelsize
   "-"
   "\\([0-9?*]+\\|\\[[ 0-9+~.e?*]+\\]\\)"    ; pointsize
   "-"
   "\\([0-9?*]+\\)"			; resx
   "-"
   "\\([0-9?*]+\\)"			; resy
   "-"
   "\\([cmp?*]\\)"			; spacing
   "-"
   "~?"					; avgwidth
   "\\([0-9?*]+\\)"
   "-"
   "\\([^-]+\\)"			; registry
   "-"
   "\\([^-]+\\)"			; encoding
   "\\'")
  "The regular expression used to match XLFD font names.")

(defun fc-pattern-p (object)
  "Returns t if OBJECT is of type fc-pattern, nil otherwise."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-pattern-create ()
  "Return a new, empty fc-pattern object."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-name-parse (fontname)
  "Parse an Fc font name and return its representation as a fc pattern object."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-name-unparse (pattern)
  "Unparse an fc pattern object to a string."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-pattern-duplicate (pattern)
  "Make a copy of the fc pattern object PATTERN and return it."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-pattern-add (pattern property value)
  "Add attributes to the pattern object PATTERN.  PROPERTY is a string naming
the attribute to add, VALUE the value for this attribute.

VALUE may be a string, integer, float, or symbol, in which case the value
will be added as an FcChar8[], int, double, or FcBool respectively."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-pattern-del (pattern property)
  "Remove attribute PROPERTY from fc pattern object PATTERN."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

;; Generic interface to FcPatternGet()
;; Don't support the losing symbol-for-property interface.

(defun fc-pattern-get (pattern property &optional id type)
  "From PATTERN, extract PROPERTY for the ID'th member, of type TYPE.

PATTERN is an Xft \(fontconfig) pattern object.
PROPERTY is a string naming an fontconfig font property.
Optional ID is a nonnegative integer indexing the list of values for PROPERTY
  stored in PATTERN, defaulting to 0 (the first value).
Optional TYPE is a symbol, one of 'string, 'boolean, 'integer, 'float,
  'double, 'matrix, 'charset, or 'void, corresponding to the FcValue types.
  \('float is an alias for 'double).

The Lisp types returned will conform to TYPE:
  string		string
  boolean		`t' or `nil'
  integer		integer
  double \(float)	float
  matrix		not implemented
  charset		not implemented
  void			not implemented

Symbols with names of the form \"fc-result-DESCRIPTION\" are returned when
the desired value is not available.  These are

  fc-result-type-mismatch       the value found has an unexpected type
  fc-result-no-match            there is no such attribute
  fc-result-no-id               there is no value for the requested ID

The types of the following standard properties are predefined by fontconfig.
The symbol 'fc-result-type-mismatch will be returned if the object exists but
TYPE does not match the predefined type.  It is best not to specify a type
for predefined properties, as a mistake here ensures error returns on the
correct type.

Each standard property has a convenience accessor defined in fontconfig.el,
named in the form \"fc-pattern-get-PROPERTY\".  The convenience functions are
preferred to `fc-pattern-get' since a typo in the string naming a property
will result in a silent null return, while a typo in a function name will
usually result in a compiler or runtime \"not fboundp\" error.  You may use
`defsubst' to define convenience functions for non-standard properties.

family         String  Font family name 
style          String  Font style. Overrides weight and slant 
slant          Int     Italic, oblique or roman 
weight         Int     Light, medium, demibold, bold or black 
size           Double  Point size 
aspect         Double  Stretches glyphs horizontally before hinting 
pixelsize      Double  Pixel size 
spacing        Int     Proportional, monospace or charcell 
foundry        String  Font foundry name 
antialias      Bool    Whether glyphs can be antialiased 
hinting        Bool    Whether the rasterizer should use hinting 
verticallayout Bool    Use vertical layout 
autohint       Bool    Use autohinter instead of normal hinter 
globaladvance  Bool    Use font global advance data 
file           String  The filename holding the font 
index          Int     The index of the font within the file 
ftface         FT_Face Use the specified FreeType face object 
rasterizer     String  Which rasterizer is in use 
outline        Bool    Whether the glyphs are outlines 
scalable       Bool    Whether glyphs can be scaled 
scale          Double  Scale factor for point->pixel conversions 
dpi            Double  Target dots per inch 
rgba           Int     unknown, rgb, bgr, vrgb, vbgr, none - subpixel geometry 
minspace       Bool    Eliminate leading from line spacing 
charset        CharSet Unicode chars encoded by the font 
lang           String  List of RFC-3066-style languages this font supports

The FT_Face, Matrix, CharSet types are unimplemented, so the corresponding
properties are not accessible from Lisp at this time.  If the value of a
property returned has type FT_Face, FcCharSet, or FcMatrix,
`fc-result-type-mismatch' is returned.

The following properties which were standard in Xft v.1 are obsolete in
Xft v.2:  encoding, charwidth, charheight, core, and render."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun fc-font-match (device pattern)
  "Return the font on DEVICE that most closely matches PATTERN.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern object.
Returns a fontconfig pattern object representing the closest match to the
given pattern, or an error code.  Possible error codes are
`fc-result-no-match' and `fc-result-no-id'."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

;; #### fix this name to correspond to Ben's new nomenclature
(defun fc-list-fonts-pattern-objects (device pattern properties)
  "Return a list of fonts on DEVICE that match PATTERN for PROPERTIES.
Each font is represented by a fontconfig pattern object.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern to be matched.
PROPERTIES is a list of property names (strings) that should match.

#### DEVICE is unused, ignored, and may be removed if it's not needed to
match other font-listing APIs."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

;; #### maybe this can/should be folded into fc-list-fonts-pattern-objects?
(defun fc-font-sort (device pattern &optional trim nosub)
  "Return a list of all fonts sorted by proximity to PATTERN.
Each font is represented by a fontconfig pattern object.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern to be matched.
Optional argument TRIM, if non-nil, means to trim trailing fonts that do not
contribute new characters to the union repertoire.

#### Optional argument NOSUB, if non-nil, suppresses some of the usual
property substitutions.  DON'T USE THIS in production code, it is intended
for exploring behavior of fontconfig and will be removed when this code is
stable.

#### DEVICE is unused, ignored, and may be removed if it's not needed to
match other font-listing APIs."
  (error 'unimplemented "font-mgr library is experimental and incomplete"))

(defun xlfd-font-name-p (fontname)
  "Check whether the string FONTNAME is a XLFD font name."
  (save-match-data
    (string-match xft-xlfd-font-regexp fontname)))

;; FcPatternPrint: there is no point in having wrappers fc-pattern-print,
;; Ffc_pattern_print since this function prints to stdout.

;;; end font-mgr.el
