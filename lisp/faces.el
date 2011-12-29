;;; faces.el --- Lisp interface to the C "face" structure

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 1995, 1996, 2002, 2005 Ben Wing
;; Copyright (C) 2010 Didier Verna

;; Author: Ben Wing <ben@xemacs.org>
;; Keywords: faces, internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not synched with FSF.  Almost completely divergent.

;;; Commentary:

;; This file is dumped with XEmacs.

;; face implementation #1 (used Lisp vectors and parallel C vectors;
;; FSFmacs still uses this) authored by Jamie Zawinski <jwz@jwz.org>
;; pre Lucid-Emacs 19.0.

;; face implementation #2 (used one face object per frame per face)
;; authored by Jamie Zawinski for 19.9.

;; face implementation #3 (use one face object per face) originally
;; authored for 19.12 by Chuck Thompson <cthomp@cs.uiuc.edu>,
;; rewritten by Ben Wing with the advent of specifiers.


;;; Some stuff in FSF's faces.el is in our x-faces.el.

;;; Code:

;; To elude the warnings for font functions. (Normally autoloaded when
;; font-create-object is called)
(eval-when-compile (require 'font))

(defgroup faces nil
  "Support for multiple text attributes (fonts, colors, ...)
Such a collection of attributes is called a \"face\"."
  :group 'emacs)


(defun read-face-name (prompt)
  (let (face)
    (while (eql (length face) 0) ; nil or ""
      (setq face (completing-read prompt
				  (mapcar (lambda (x) (list (symbol-name x)))
					  (face-list))
				  nil t)))
    (intern face)))

(defun face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what "-instance")))
	 (face (read-face-name (format "Set %s of face: " what)))
	 (default (if (fboundp fn)
		      ;; #### we should distinguish here between
		      ;; explicitly setting the value to be the
		      ;; same as the default face's value, and
		      ;; not setting a value at all.
		      (funcall fn face)))
	 (value (if bool
		    (y-or-n-p (format "Should face %s be %s? "
				      (symbol-name face) bool))
		  (read-string (format "Set %s of face %s to: "
				       what (symbol-name face))
		   (cond ((font-instance-p default)
			  (font-instance-name default))
			 ((color-instance-p default)
			  (color-instance-name default))
			 ((image-instance-p default)
			  (image-instance-file-name default))
			 ((face-background-placement-instance-p default)
			  (symbol-name default))
			 (t default))))))
    (list face (if (equal value "") nil value))))

(defconst built-in-face-specifiers
  (built-in-face-specifiers)
  "A list of the built-in face properties that are specifiers.")

(defun face-property (face property &optional locale tag-set exact-p)
  "Return FACE's value of the given PROPERTY.

NOTE: If you are looking for the \"value\" of a built-in face property
  (`foreground', `background', `font', `background-pixmap', etc.), you
  are probably better off calling `face-property-instance'.  The return
  value of `face-property' for built-in properties describes the original
  specification used to determine the face property, which may be nil,
  a list of instantiators, or something else that is unexpected.  For
  example, if you ask for a face property in a particular buffer (by
  specifying a buffer for LOCALE), you will get a non-nil return value
  only if a buffer-local specification for that particular buffer had
  previously been given.

For a full list of built-in property names and their semantics, see
  `set-face-property'.

If LOCALE is omitted, the FACE's actual value for PROPERTY will be
  returned.  In this case, this function appears to behave rather
  differently depending on whether PROPERTY is a built-in face property of
  a user-defined face property.  This is because the most basic value of a
  user-defined property is simply whatever was set using
  `set-face-property', but for a built-in property it's always a specifier,
  which is an abstract object encapsulating all the specifications for that
  particular property.

LOCALE, if supplied, will generally be a buffer, frame or
  `global' (for the global value), but there are other possibilities -- see
  the following paragraph.  This mostly applies to built-in properties.  In
  this case, the return value will not be a specifier object but the
  specification(s) for the given locale or locale type will be returned
  (equivalent to calling `specifier-specs' on the specifier).
  (Technically, the same thing happens if the basic value of a user-
  defined property is a specifier, although this usage is rare.)

The return value will be a list of instantiators (e.g. strings
  specifying a font or color name), or a list of specifications, each
  of which is a cons of a locale and a list of instantiators.
  Specifically, if LOCALE is a particular locale (a buffer, window,
  frame, device, or `global'), a list of instantiators for that locale
  will be returned.  Otherwise, if LOCALE is a locale type (one of
  the symbols `buffer', `window', `frame', or `device'), the specifications
  for all locales of that type will be returned.  Finally, if LOCALE is
  `all', the specifications for all locales of all types will be returned.

The specifications in a specifier determine what the value of
  PROPERTY will be in a particular \"domain\" or set of circumstances,
  which is typically a particular Emacs window -- which in turn defines
  a buffer (the buffer in the window), a frame (the frame that the window
  is in), and a device (the device that the frame is in).  The value is
  derived from the instantiator associated with the most specific
  locale (in the order buffer, window, frame, device, and `global')
  that matches the domain in question.  In other words, given a domain
  (i.e. an Emacs window, usually), the specifier for PROPERTY will
  first be searched for a specification whose locale is the buffer
  contained within that window; then for a specification whose locale
  is the window itself; then for a specification whose locale is the
  frame that the window is contained within; etc.  The first
  instantiator that is valid for the domain (usually this means that
  the instantiator is recognized by the device [i.e. MS Windows, the X
  server or TTY device]) will be \"instantiated\", which generates
  a Lisp object encapsulating the original instantiator and the underlying
  window-system object describing the property.  The function
  `face-property-instance' actually does all this."

  (setq face (get-face face))
  (let ((value (get face property)))
    (if (and locale
	     (or (memq property built-in-face-specifiers)
		 (specifierp value)))
	(setq value (specifier-specs value locale tag-set exact-p)))
    value))

(defun convert-face-property-into-specifier (face property)
  "Convert PROPERTY on FACE into a specifier, if it's not already."
  (setq face (get-face face))
  (let ((specifier (get face property)))
    ;; if a user-property does not have a specifier but a
    ;; locale was specified, put a specifier there.
    ;; If there was already a value there, convert it to a
    ;; specifier with the value as its `global' instantiator.
    (unless (specifierp specifier)
      (let ((new-specifier (make-specifier 'generic)))
	(if (or (not (null specifier))
		;; make sure the nil returned from `get' wasn't
		;; actually the value of the property
		(null (get face property t)))
	    (add-spec-to-specifier new-specifier specifier))
	(setq specifier new-specifier)
	(put face property specifier)))))

(defun face-property-instance (face property
			       &optional domain default no-fallback)
  "Return the instance of FACE's PROPERTY in the specified DOMAIN.

Under most circumstances, DOMAIN will be a particular window,
  and the returned instance describes how the specified property
  actually is displayed for that window and the particular buffer
  in it.  Note that this may not be the same as how the property
  appears when the buffer is displayed in a different window or
  frame, or how the property appears in the same window if you
  switch to another buffer in that window; and in those cases,
  the returned instance would be different.

The returned instance will typically be a color-instance,
  font-instance, or image-instance object, and you can query
  it using the appropriate object-specific functions.  For example,
  you could use `color-instance-rgb-components' to find out the
  RGB (red, green, and blue) components of how the `background'
  property of the `highlight' face is displayed in a particular
  window.  The results might be different from the results
  you would get for another window (perhaps the user
  specified a different color for the frame that window is on;
  or perhaps the same color was specified but the window is
  on a different X server, and that X server has different RGB
  values for the color from this one).

DOMAIN defaults to the selected window if omitted.

DOMAIN can be a frame or device, instead of a window.  The value
  returned for a such a domain is used in special circumstances
  when a more specific domain does not apply; for example, a frame
  value might be used for coloring a toolbar, which is conceptually
  attached to a frame rather than a particular window.  The value
  is also useful in determining what the value would be for a
  particular window within the frame or device, if it is not
  overridden by a more specific specification.

If PROPERTY does not name a built-in property, its value will
  simply be returned unless it is a specifier object, in which case
  it will be instanced using `specifier-instance'.

Optional arguments DEFAULT and NO-FALLBACK are the same as in
  `specifier-instance'."

  (setq face (get-face face))
  (let ((value (get face property)))
    (if (specifierp value)
	(setq value (specifier-instance value domain default no-fallback)))
    value))

(defun face-property-matching-instance (face property matchspec
					&optional domain default
					no-fallback)
  "Return the instance of FACE's PROPERTY matching MATCHSPEC in DOMAIN.
Currently MATCHSPEC is used only for the 'font property, when its value
should be a cons \(CHARSET . STAGE) \(see `specifier-matching-instance'
for a full description of the matching process).  This allows you to
retrieve a font that can be used to display a particular charset, rather
than just any font.  For backward compatibility, MATCHSPEC may be a
charset, which is interpreted as \(CHARSET . final).

See `face-property-instance' for usage of the other arguments."

  (setq face (get-face face))
  ;; For compatibility with 21.4-oriented code, eg, x-symbol-mule.el.
  (when (charsetp matchspec)
    (setq matchspec (cons matchspec 'final)))
  (let ((value (get face property)))
    (when (specifierp value)
      (setq value (specifier-matching-instance value matchspec domain
					       default no-fallback)))
    value))

(defun set-face-property (face property value &optional locale tag-set
			  how-to-add)
  "Change a property of FACE.

NOTE: If you want to remove a property from a face, use `remove-face-property'
  rather than attempting to set a value of nil for the property.

For built-in properties, the actual value of the property is a
  specifier and you cannot change this; but you can change the
  specifications within the specifier, and that is what this function
  will do.  For user-defined properties, you can use this function
  to either change the actual value of the property or, if this value
  is a specifier, change the specifications within it.

If PROPERTY is a built-in property, the specifications to be added to
  this property can be supplied in many different ways:

  -- If VALUE is a simple instantiator (e.g. a string naming a font or
     color) or a list of instantiators, then the instantiator(s) will
     be added as a specification of the property for the given LOCALE
     (which defaults to `global' if omitted).
  -- If VALUE is a list of specifications (each of which is a cons of
     a locale and a list of instantiators), then LOCALE must be nil
     (it does not make sense to explicitly specify a locale in this
     case), and specifications will be added as given.
  -- If VALUE is a specifier (as would be returned by `face-property'
     if no LOCALE argument is given), then some or all of the
     specifications in the specifier will be added to the property.
     In this case, the function is really equivalent to
     `copy-specifier' and LOCALE has the same semantics (if it is
     a particular locale, the specification for the locale will be
     copied; if a locale type, specifications for all locales of
     that type will be copied; if nil or `all', then all
     specifications will be copied).

HOW-TO-ADD should be either nil or one of the symbols `prepend',
  `append', `remove-tag-set-prepend', `remove-tag-set-append', `remove-locale',
  `remove-locale-type', or `remove-all'.  See `copy-specifier' and
  `add-spec-to-specifier' for a description of what each of
  these means.  Most of the time, you do not need to worry about
  this argument; the default behavior usually is fine.

In general, it is OK to pass an instance object (e.g. as returned
  by `face-property-instance') as an instantiator in place of
  an actual instantiator.  In such a case, the instantiator used
  to create that instance object will be used (for example, if
  you set a font-instance object as the value of the `font'
  property, then the font name used to create that object will
  be used instead).  If some cases, however, doing this
  conversion does not make sense, and this will be noted in
  the documentation for particular types of instance objects.

If PROPERTY is not a built-in property, then this function will
  simply set its value if LOCALE is nil.  However, if LOCALE is
  given, then this function will attempt to add VALUE as the
  instantiator for the given LOCALE, using `add-spec-to-specifier'.
  If the value of the property is not a specifier, it will
  automatically be converted into a `generic' specifier.


The following symbols have predefined meanings:

 foreground         The foreground color of the face.
                    For valid instantiators, see `make-color-specifier'.

 foreback           The foreground color of the face's background pixmap,
                    when the pixmap is a bitmap.
                    Only used by faces on X and MS Windows devices.
                    For valid instantiators, see `make-color-specifier'.

 background         The background color of the face.
                    For valid instantiators, see `make-color-specifier'.

 font               The font used to display text covered by this face.
                    For valid instantiators, see `make-font-specifier'.

 display-table      The display table of the face.
                    This should be a vector of 256 elements.

 background-pixmap  The pixmap displayed in the background of the face.
                    Only used by faces on X and MS Windows devices.
                    For valid instantiators, see `make-image-specifier'.

 background-placement  The placement of the face's background pixmap.
                    Only used by faces on X devices.
                    For valid instantiators,
                    see `make-face-background-placement-specifier'.

 underline          Underline all text covered by this face.
                    For valid instantiators, see `make-face-boolean-specifier'.

 strikethru         Draw a line through all text covered by this face.
                    For valid instantiators, see `make-face-boolean-specifier'.

 highlight          Highlight all text covered by this face.
                    Only used by faces on TTY devices.
                    For valid instantiators, see `make-face-boolean-specifier'.

 dim                Dim all text covered by this face.
                    For valid instantiators, see `make-face-boolean-specifier'.

 blinking           Blink all text covered by this face.
                    Only used by faces on TTY devices.
                    For valid instantiators, see `make-face-boolean-specifier'.

 reverse            Reverse the foreground and background colors.
                    Only used by faces on TTY devices.
                    For valid instantiators, see `make-face-boolean-specifier'.

 shrink             Shrink the face to the actual text on the line instead of
                    covering the whole line until the right border of the
                    window.  The effect will only be visible if the face has
                    a non default background.
                    For valid instantiators, see `make-face-boolean-specifier'.

 inherit	    Face name or face object from which to inherit attributes,
                    or a list of such elements.  Attributes from inherited
                    faces are merged into the face like an underlying face
                    would be, with higher priority than underlying faces.

 doc-string         Description of what the face's normal use is.
                    NOTE: This is not a specifier, unlike all
                    the other built-in properties, and cannot
                    contain locale-specific values."

  (setq face (get-face face))
  (if (memq property built-in-face-specifiers)
      (set-specifier (get face property) value locale tag-set how-to-add)

    ;; This section adds user defined properties.
    (if (not locale)
	(put face property value)
      (convert-face-property-into-specifier face property)
      (add-spec-to-specifier (get face property) value locale tag-set
			     how-to-add)))
  value)

(defun remove-face-property (face property &optional locale tag-set exact-p)
  "Remove a property from FACE.
For built-in properties, this is analogous to `remove-specifier'.
See `remove-specifier' for the meaning of the LOCALE, TAG-SET, and EXACT-P
arguments."
  (or locale (setq locale 'all))
  (if (memq property built-in-face-specifiers)
      (remove-specifier (face-property face property) locale tag-set exact-p)
    (if (eq locale 'all)
	(remprop (get-face face) property)
      (convert-face-property-into-specifier face property)
      (remove-specifier (face-property face property) locale tag-set
			exact-p))))

(defun reset-face (face &optional locale tag-set exact-p)
  "Clear all existing built-in specifications from FACE.
This makes FACE inherit all its display properties from `default'.
WARNING: Be absolutely sure you want to do this!!!  It is a dangerous
operation and is not undoable.

The arguments LOCALE, TAG-SET and EXACT-P are the same as for
`remove-specifier'."
  ;; Don't reset the default face. 
  (unless (eq 'default face)
    (dolist (x built-in-face-specifiers nil)
      (remove-specifier (face-property face x) locale tag-set exact-p))))

(defun set-face-parent (face parent &optional locale tag-set how-to-add)
  "Set the parent of FACE to PARENT, for all properties.
This makes all properties of FACE inherit from PARENT."
  (setq parent (get-face parent))
  (mapc (lambda (x)
          (set-face-property face x (vector parent) locale tag-set
                             how-to-add))
        (set-difference built-in-face-specifiers
                        '(display-table background-pixmap inherit)))
  (set-face-background-pixmap face (vector 'inherit :face parent)
			      locale tag-set how-to-add)
  nil)

(defun face-doc-string (face)
  "Return the documentation string for FACE."
  (face-property face 'doc-string))

(defun set-face-doc-string (face doc-string)
  "Change the documentation string of FACE to DOC-STRING."
  (interactive (face-interactive "doc-string"))
  (set-face-property face 'doc-string doc-string))

(defun face-font-name (face &optional domain charset)
  "Return the font name of FACE in DOMAIN, or nil if it is unspecified.
DOMAIN is as in `face-font-instance'.

Font names are strings, as described in `make-font-specifier'."
  (let ((f (face-font-instance face domain charset)))
    (and f (font-instance-name f))))

(defun face-font (face &optional locale tag-set exact-p)
  "Return the font spec of FACE in LOCALE, or nil if it is unspecified.

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual font being used.  If you want to know the
actual font used in a particular domain, use `face-font-instance', or
`face-font-name' for its name (i.e. the instantiator used to create it).

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'font locale tag-set exact-p))

(defun face-font-instance (face &optional domain charset)
  "Return the instance of FACE's font in DOMAIN.

Return value will be a font instance object; query its properties using
`font-instance-name', `font-instance-height', `font-instance-width', etc.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the font appears in that
  particular window and buffer will be returned.

CHARSET is a Mule charset (meaning return the font used for that charset) or
nil (meaning return the font used for ASCII.)

See `face-property-instance' for more information."
  (if (null charset)
      (face-property-instance face 'font domain)
    (let (matchspec)
      ;; get-charset signals an error if its argument doesn't have an
      ;; associated charset.
      (setq charset (if-fboundp 'get-charset
                        (get-charset charset)
                      (error 'unimplemented "Charset support not available"))
	    matchspec (cons charset nil))
      (or (null (setcdr matchspec 'initial))
	  (face-property-matching-instance 
	   face 'font matchspec domain)
	  (null (setcdr matchspec 'final))
	  (face-property-matching-instance
	   face 'font matchspec domain)))))

(defun set-face-font (face font &optional locale tag-set how-to-add)
  "Change the font of FACE to FONT in LOCALE.

FACE may be either a face object or a symbol representing a face.

FONT should be an instantiator (see `make-font-specifier'; a common
  instantiator is a platform-dependent string naming the font), a list
  of instantiators, an alist of specifications (each mapping a locale
  to an instantiator list), or a font specifier object.

If FONT is an alist, LOCALE must be omitted.  If FONT is a specifier
  object, LOCALE can be a locale, a locale type, `all', or nil; see
  `copy-specifier' for its semantics.  Common LOCALEs are buffer
  objects, window objects, device objects and `global'.  Otherwise
  LOCALE specifies the locale under which the specified
  instantiator(s) will be added, and defaults to `global'.

See `set-face-property' for more information."
  (interactive (face-interactive "font"))
  (set-face-property face 'font font locale tag-set how-to-add))

(defun face-foreground (face &optional locale tag-set exact-p)
  "Return the foreground spec of FACE in LOCALE, or nil if it is unspecified.

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual foreground being used.  If you want to know the
actual foreground color used in a particular domain, use
`face-foreground-instance', or `face-foreground-name' for its name
\(i.e. the instantiator used to create it).

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'foreground locale tag-set exact-p))

(defun face-foreground-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's foreground in DOMAIN.

Return value will be a color instance object; query its properties using
`color-instance-name' or `color-instance-rgb-properties'.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the foreground appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'foreground domain default no-fallback))

(defun face-foreground-name (face &optional domain default no-fallback)
  "Return the name of FACE's foreground color in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the foreground appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (color-instance-name (face-foreground-instance
			face domain default no-fallback)))

(defun set-face-foreground (face color &optional locale tag-set how-to-add)
  "Change the foreground color of FACE to COLOR in LOCALE.

FACE may be either a face object or a symbol representing a face.

COLOR should be an instantiator (see `make-color-specifier'), a list of
  instantiators, an alist of specifications (each mapping a locale to
  an instantiator list), or a color specifier object.

If COLOR is an alist, LOCALE must be omitted.  If COLOR is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-face-property' for more information."
  (interactive (face-interactive "foreground"))
  (set-face-property face 'foreground color locale tag-set how-to-add))

(defun face-foreback (face &optional locale tag-set exact-p)
  "Return the foreback spec of FACE in LOCALE, or nil if it is unspecified.

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual foreback being used.  If you want to know the
actual foreback color used in a particular domain, use
`face-foreback-instance', or `face-foreback-name' for its name
\(i.e. the instantiator used to create it).

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'foreback locale tag-set exact-p))

(defun face-foreback-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's foreback in DOMAIN.

Return value will be a color instance object; query its properties using
`color-instance-name' or `color-instance-rgb-properties'.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the foreback appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'foreback domain default no-fallback))

(defun face-foreback-name (face &optional domain default no-fallback)
  "Return the name of FACE's foreback color in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the foreback appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (color-instance-name (face-foreback-instance
			face domain default no-fallback)))

(defun set-face-foreback (face color &optional locale tag-set how-to-add)
  "Change the foreback color of FACE to COLOR in LOCALE.

FACE may be either a face object or a symbol representing a face.

COLOR should be an instantiator (see `make-color-specifier'), a list of
  instantiators, an alist of specifications (each mapping a locale to
  an instantiator list), or a color specifier object.

If COLOR is an alist, LOCALE must be omitted.  If COLOR is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-face-property' for more information."
  (interactive (face-interactive "foreback"))
  (set-face-property face 'foreback color locale tag-set how-to-add))

(defun face-background (face &optional locale tag-set exact-p)
  "Return the background color of FACE in LOCALE, or nil if it is unspecified.

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual background being used.  If you want to know the
actual background color used in a particular domain, use
`face-background-instance', or `face-background-name' for its name
\(i.e. the instantiator used to create it).

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'background locale tag-set exact-p))

(defun face-background-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's background in DOMAIN.

Return value will be a color instance object; query its properties using
`color-instance-name' or `color-instance-rgb-properties'.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'background domain default no-fallback))

(defun face-background-name (face &optional domain default no-fallback)
  "Return the name of FACE's background color in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (color-instance-name (face-background-instance
			face domain default no-fallback)))

(defun set-face-background (face color &optional locale tag-set how-to-add)
  "Change the background color of FACE to COLOR in LOCALE.

FACE may be either a face object or a symbol representing a face.

COLOR should be an instantiator (see `make-color-specifier'), a list of
  instantiators, an alist of specifications (each mapping a locale to
  an instantiator list), or a color specifier object.

If COLOR is an alist, LOCALE must be omitted.  If COLOR is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-face-property' for more information."
  (interactive (face-interactive "background"))
  (set-face-property face 'background color locale tag-set how-to-add))

(defun face-background-pixmap (face &optional locale tag-set exact-p)
  "Return the background pixmap spec of FACE in LOCALE, or nil if unspecified.
This property is only used on window system devices.

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual background pixmap being used.  If you want to
know the actual background pixmap used in a particular domain, use
`face-background-pixmap-instance'.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'background-pixmap locale tag-set exact-p))

(defun face-background-pixmap-instance (face &optional domain default
					     no-fallback)
  "Return the instance of FACE's background pixmap in DOMAIN.

Return value will be an image instance object; query its properties using
`image-instance-instantiator' (the original instantiator used to create
the image, which may be a complex beast -- see `make-image-specifier'),
`image-instance-file-name' (the file, if any, from which the image was
created), `image-instance-height', etc.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'background-pixmap domain default no-fallback))

(defun set-face-background-pixmap (face pixmap &optional locale tag-set
					how-to-add)
  "Change the background pixmap of FACE to PIXMAP in LOCALE.
This property is only used on window system devices.

FACE may be either a face object or a symbol representing a face.

PIXMAP should be an instantiator (see `make-image-specifier'), a list
  of instantiators, an alist of specifications (each mapping a locale
  to an instantiator list), or an image specifier object.

If PIXMAP is an alist, LOCALE must be omitted.  If PIXMAP is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-face-property' for more information."
  (interactive (face-interactive "background-pixmap"))
  (set-face-property face 'background-pixmap pixmap locale tag-set how-to-add))

(defvar background-pixmap-file-history nil
  ;; History for `set-face-background-pixmap-file'
  )

(defun set-face-background-pixmap-file (face file)
  "Read (and set) the background pixmap of FACE from FILE.
This function is a simplified version of `set-face-background-pixmap',
designed for interactive use."
  (interactive
   (let* ((face (read-face-name "Set background pixmap of face: "))
	  (default (and (face-background-pixmap-instance face)
			(image-instance-file-name
			 (face-background-pixmap-instance face))))
	  (file (read-file-name
		 (format "Set background pixmap of face %s to: "
			 (symbol-name face))
		 nil default t nil
		      'background-pixmap-file-history)))
     (list face (if (equal file "") nil file))))
  (set-face-property face 'background-pixmap file))

(defun face-background-placement (face &optional domain default no-fallback)
  "Return FACE's background placement in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property face 'background-placement domain default no-fallback))

(defun set-face-background-placement (face placement &optional locale tag-set
				      how-to-add)
  "Change the background-placement property of FACE to PLACEMENT.
PLACEMENT is normally a background-placement instantiator; see
`make-face-background-placement-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
HOW-TO-ADD arguments."
  (interactive (face-interactive "background placement"))
  ;; When called non-interactively (for example via custom), PLACEMENT is
  ;; expected to be a symbol. -- dvl
  (unless (symbolp placement)
    (setq placement (intern placement)))
  (set-face-property face 'background-placement placement locale tag-set
		     how-to-add))

(defun face-background-placement-instance (face &optional domain default
					   no-fallback)
  "Return FACE's background-placement instance in DOMAIN.
Return value will be a background-placement instance object.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
and an instance object describing the background placement in that particular
window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'background-placement domain default
			  no-fallback))

(defun face-background-placement-instance-p (object)
  "Return t if OBJECT is a face-background-placement instance."
  (or (eq object 'absolute) (eq object 'relative)))

(defun face-display-table (face &optional locale tag-set exact-p)
  "Return the display table spec of FACE in LOCALE, or nil if unspecified..

NOTE: This returns a locale-specific specification, not any sort of value
corresponding to the actual display table being used.  If you want to
know the actual display table used in a particular domain, use
`face-display-table-instance'.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'display-table locale tag-set exact-p))

(defun face-display-table-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's display table in DOMAIN.

Return value will be a vector, char table or range table; see
`current-display-table'.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and the actual display table used in that particular window and buffer
  will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'display-table domain default no-fallback))

(defun set-face-display-table (face display-table &optional locale tag-set
			       how-to-add)
  "Change the display table of FACE to DISPLAY-TABLE in LOCALE.
DISPLAY-TABLE should be a vector as returned by `make-display-table'.

See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
  HOW-TO-ADD arguments."
  (interactive (face-interactive "display-table"))
  (set-face-property face 'display-table display-table locale tag-set
		     how-to-add))

;; The following accessors and mutators are, IMHO, good
;; implementation.  Cf. with `make-face-bold'.

(defun face-underline-p (face &optional domain default no-fallback)
  "Return t if FACE is underlined in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'underline domain default no-fallback))

(defun set-face-underline-p (face underline-p &optional locale tag-set
			     how-to-add)
  "Change the underline property of FACE to UNDERLINE-P.
UNDERLINE-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "underline-p" "underlined"))
  (set-face-property face 'underline underline-p locale tag-set how-to-add))

(defun face-strikethru-p (face &optional domain default no-fallback)
  "Return t if FACE is strikethru-d (i.e. struck through) in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'strikethru domain default no-fallback))

(defun set-face-strikethru-p (face strikethru-p &optional locale tag-set
			      how-to-add)
  "Change whether FACE is strikethru-d (i.e. struck through) in LOCALE.
STRIKETHRU-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "strikethru-p" "strikethru-d"))
  (set-face-property face 'strikethru strikethru-p locale tag-set how-to-add))

(defun face-highlight-p (face &optional domain default no-fallback)
  "Return t if FACE is highlighted in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'highlight domain default no-fallback))

(defun set-face-highlight-p (face highlight-p &optional locale tag-set
			     how-to-add)
  "Change whether FACE is highlighted in LOCALE (TTY locales only).
HIGHLIGHT-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "highlight-p" "highlighted"))
  (set-face-property face 'highlight highlight-p locale tag-set how-to-add))

(defun face-dim-p (face &optional domain default no-fallback)
  "Return t if FACE is dimmed in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'dim domain default no-fallback))

(defun set-face-dim-p (face dim-p &optional locale tag-set how-to-add)
  "Change whether FACE is dimmed in LOCALE.
DIM-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "dim-p" "dimmed"))
  (set-face-property face 'dim dim-p locale tag-set how-to-add))

(defun face-blinking-p (face &optional domain default no-fallback)
  "Return t if FACE is blinking in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'blinking domain default no-fallback))

(defun set-face-blinking-p (face blinking-p &optional locale tag-set
			    how-to-add)
  "Change whether FACE is blinking in LOCALE (TTY locales only).
BLINKING-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "blinking-p" "blinking"))
  (set-face-property face 'blinking blinking-p locale tag-set how-to-add))

(defun face-reverse-p (face &optional domain default no-fallback)
  "Return t if FACE is reversed in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'reverse domain default no-fallback))

(defun set-face-reverse-p (face reverse-p &optional locale tag-set how-to-add)
  "Change whether FACE is reversed in LOCALE (TTY locales only).
REVERSE-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "reverse-p" "reversed"))
  (set-face-property face 'reverse reverse-p locale tag-set how-to-add))

(defun face-shrink-p (face &optional domain default no-fallback)
  "Return t if FACE is shrinked in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'shrink domain default no-fallback))

(defun set-face-shrink-p (face shrink-p &optional locale tag-set how-to-add)
  "Change whether FACE is shrinked in LOCALE.
SHRINK-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "shrink-p" "shrinked"))
  (set-face-property face 'shrink shrink-p locale tag-set how-to-add))


(defun face-property-equal (face1 face2 prop domain)
  (equal (face-property-instance face1 prop domain)
	 (face-property-instance face2 prop domain)))

(defun face-equal-loop (props face1 face2 domain)
  (while (and props
	      (face-property-equal face1 face2 (car props) domain))
    (setq props (cdr props)))
  (null props))

(defun face-equal (face1 face2 &optional domain)
  "Return t if FACE1 and FACE2 will display in the same way in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (if (null domain) (setq domain (selected-window)))
  (if (not (valid-specifier-domain-p domain))
      (error "Invalid specifier domain"))
  (let ((device (dfw-device domain))
	(common-props '(foreground background
			font display-table underline
			dim inherit shrink))
	(win-props '(foreback background-pixmap background-placement
		     strikethru))
	(tty-props '(highlight blinking reverse)))

    ;; First check the properties which are used in common between the
    ;; x and tty devices.  Then, check those properties specific to
    ;; the particular device type.
    (and (face-equal-loop common-props face1 face2 domain)
	 (cond ((eq 'tty (device-type device))
		(face-equal-loop tty-props face1 face2 domain))
	       ((console-on-window-system-p (device-console device))
		(face-equal-loop win-props face1 face2 domain))
	       (t t)))))

(defun face-differs-from-default-p (face &optional domain)
  "Return t if FACE will display differently from the default face in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (not (face-equal face 'default domain)))

(defun try-font-name (name &optional device)
  "Return NAME if it's a valid font name on DEVICE, else nil."
  ;; yes, name really should be here twice.
  (and name (make-font-instance name device t) name))



(defcustom face-frob-from-locale-first nil
  "*If non nil, use kludgy way of frobbing fonts suitable for non-mule
multi-charset environments."
  :group 'faces
  :type 'boolean)

;; This function is a terrible, disgusting hack!!!!  Need to
;; separate out the font elements as separate face properties!

;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!

;; When we are initializing a device, it won't be selected; we communicate
;; the device to consider as selected using this variable.
(defvar Face-frob-property-device-considered-current nil)

(defun Face-frob-property (face locale tag-set exact-p
			   unfrobbed-face frobbed-face
			   win-prop tty-props
			   frob-mapping standard-face-mapping)
  ;; implement the semantics of `make-face-bold' et al.  FACE, LOCALE, TAG-SET
  ;; and EXACT-P are as in that call.  UNFROBBED-FACE and FROBBED-FACE are
  ;; what we expect the original face and the result to look like,
  ;; respectively.  TTY-PROPS is a list of face properties to frob in place
  ;; of `font' for TTYs.  FROB-MAPPING is either a plist mapping device
  ;; types to functions of two args (NAME DEVICE) that will frob the
  ;; instantiator to NAME as appropriate for DEVICE's type (this includes
  ;; TTYs #### TTYs are not passed the device, just the symbol 'tty), or a
  ;; function to handle the mapping for all device types.
  ;; STANDARD-FACE-MAPPING is an alist of mappings of inheritance
  ;; instantiators to be replaced with other inheritance instantiators, meant
  ;; for e.g. converting [bold] into [bold-italic].

  ;; #### it would be nice if this function could be generalized to be
  ;; a general specifier frobber.  but so much of what it does is specific
  ;; to faces -- e.g. handling of inheritance, standard faces,
  ;; special-casing in various ways for tty's, etc.  i've already extracted
  ;; as much of the functionality as i can into subfunctions in the
  ;; heuristic section of specifier.el.

  ;; #### Note: The old code was totally different (and there was much less
  ;; of it).  It didn't bother with trying to frob all the instantiators,
  ;; or handle inheritance vectors as instantiators, or do something
  ;; sensible with buffer locales, or many other things. (It always, or
  ;; usually, did a specifier-instance and frobbed the result.) But it did
  ;; do three things we don't:
  ;;
  ;; (1) Map over all devices when processing global or buffer locales.
  ;;     Should we be doing this in stages 2 and/or 3?  The fact that we
  ;;     now process all fallback instantiators seems to make this less
  ;;     necessary, at least for global locales.
  ;;
  ;; (2) Remove all instantiators tagged with `default' when putting the
  ;;     instantiators back.  I don't see why this is necessary, but maybe
  ;;     it is.
  ;;
  ;; (3) Pay attention to the face-frob-from-locale-first variable. ####
  ;;     I don't understand its purpose.  Undocumented hacks like this,
  ;;     clearly added after-the-fact, don't deserve to live.  DOCUMENT
  ;;     THIS SHIT!

  (labels
      (

       ;; non-nil if either instantiator non-nil, or nil instantiators allowed.
       (nil-instantiator-ok (inst devtype-spec)
	 (or inst (eq devtype-spec 'tty)))

       ;; if LOCALE is a global locale (all, nil, global), return 'global,
       ;; else nil.
       (global-locale (locale)
	 (and (memq locale '(all nil global)) 'global))

       ;; Given a locale and the inst-list from that locale, frob the
       ;; instantiators according to FROB-MAPPING, a plist mapping device
       ;; types to functions that frob instantiators of that device type.
       ;; NOTE: TAG-SET and FROB-MAPPING from environment.
       (frob-face-inst-list (locale inst-list prop devtype-spec)
	 (let* ((ffpdev Face-frob-property-device-considered-current)
		(results
		 ;; for each inst-pair, frob it (the result will be 0 or
		 ;; more inst-pairs; we may get more than one if, e.g. the
		 ;; instantiator specifies inheritance and we expand the
		 ;; inheritance); then nconc the results together
		 (loop for (tag-set . x) in inst-list
		   for devtype = (derive-device-type-from-locale-and-tag-set
				  locale tag-set devtype-spec ffpdev)
		   ;; devtype may be nil if it fails to match DEVTYPE-SPEC
		   if devtype
		   if (let* ((mapper
			      (cond ((functionp frob-mapping) frob-mapping)
				    ((plist-get frob-mapping devtype))
				    (t (error 'unimplemented "mapper" devtype))))
			     (result
			      (cond
			       ;; if a vector ...
			       ((vectorp x)
				(let ((change-to
				       (cdr (assoc x standard-face-mapping))))
				  (cond
				   ;; (1) handle standard mappings/null vectors
				   ((or change-to (null (length x)))
				    (list (cons tag-set
						(cond ((eq change-to t) x)
						      (change-to)
						      (t x)))))
				   ;; (2) inheritance vectors.  retrieve the
				   ;; inherited value and recursively frob.
				   ;; stick the tag-set into the result.
				   (t (let*
					  ((subprop
					    (if (> (length x) 1) (elt x 1)
					      prop))
					   (subinsts
					    (frob-face-inst-list
					     locale
					     (cdar
					      (specifier-spec-list
					       (face-property (elt x 0)
							      subprop)))
					     subprop devtype-spec)))
					;; #### we don't currently handle
					;; the "reverse the sense" flag on
					;; tty inheritance vectors.
					(add-tag-to-inst-list subinsts
							      tag-set))))))
			       ;; (3) not a vector.  just process it.
			       (t
				(let ((value
				       (if (eq devtype-spec 'tty)
					   ;; #### not quite right but need
					   ;; two args to match documentation
					   ;; mostly we just ignore TTYs so
					   ;; for now just pass the devtype
					   (funcall mapper x 'tty)
					 (funcall mapper x
						  (derive-domain-from-locale
						   locale devtype-spec
						   ffpdev)))))
				  (and (nil-instantiator-ok value devtype-spec)
				       (list (cons tag-set value))))))))
			;; if we're adding to a tty, we need to tag our
			;; additions with `tty'; see [note 1] below.  we leave
			;; the old spec in place, however -- if e.g. we're
			;; italicizing a font that was always set to be
			;; underlined, even on window systems, then we still
			;; want the underline there.  unless we put the old
			;; spec back, the underline will disappear, since
			;; the new specs are all tagged with `tty'.  this
			;; doesn't apply to the [note 1] situations below
			;; because there we're just adding, not substituting.
			(if (and (eq 'tty devtype-spec)
				 (not (or (eq 'tty tag-set)
					  (memq 'tty tag-set))))
			    (nconc (add-tag-to-inst-list result 'tty)
				   (list (cons tag-set x)))
			  result))
		   nconc it)))
	   (delete-duplicates results :test #'equal)))

       ;; Frob INST-LIST, which came from LOCALE, and put the new value back
       ;; into SP at LOCALE.  THUNK is a cons of (PROP . DEVTYPE-SPEC), the
       ;; property being processed and whether this is a TTY property or a
       ;; win property.
       (frob-locale (sp locale inst-list thunk)
	 (let ((newinst (frob-face-inst-list locale inst-list
					     (car thunk) (cdr thunk))))
	   (remove-specifier sp locale tag-set exact-p)
	   (add-spec-list-to-specifier sp (list (cons locale newinst))))
	 ;; map-specifier should keep going
	 nil)

       ;; map over all specified locales in LOCALE; for each locale,
       ;; frob the instantiators in that locale in the specifier in both
       ;; WIN-PROP and TTY-PROPS in FACE.  Takes values from environment.
       (map-over-locales (locale)
	 (map-specifier (get face win-prop) #'frob-locale locale
			(cons win-prop 'window-system)
			tag-set exact-p)
	 (loop for prop in tty-props do
	   (map-specifier (get face prop) #'frob-locale locale
			  (cons prop 'tty)
			  tag-set exact-p)))

       ;; end of labels
       )

    (declare (inline global-locale nil-instantiator-ok))
    ;; the function itself

    (let* ((ffpdev Face-frob-property-device-considered-current)
	   (do-later-stages
	    (or (global-locale locale)
		(valid-specifier-domain-p locale)
		(bufferp locale)))
	   (domain (and do-later-stages
			(derive-domain-from-locale locale 'window-system
						   ffpdev)))
	   (check-differences
	    (and unfrobbed-face frobbed-face domain
		 (not (memq (face-name face)
			    '(default bold italic bold-italic)))))
	   (orig-instance
	    (and check-differences
		 (face-property-instance face win-prop domain))))

      ;; first do the frobbing
      (setq face (get-face face))
      (map-over-locales locale)

      (when do-later-stages

	(if (global-locale locale) (setq locale 'global))

	;; now do the second stage -- if there's nothing there, try
	;; harder to find an instantiator, and frob it.
	(let (do-something)
	  (loop for prop in (cons win-prop tty-props)
	    for propspec = (get face prop)
	    for devtype-spec = (if (eq prop win-prop) 'window-system 'tty)
	    if propspec
	    do
	    (or (specifier-spec-list propspec locale)
		(let ((doit (derive-specifier-specs-from-locale
			     propspec locale devtype-spec ffpdev
			     ;; #### does this make sense?  When no tags
			     ;; given, frob the whole list of fallbacks when
			     ;; global, else just retrieve a current-device
			     ;; value.  this tries to mirror normal practices,
			     ;; where with no tags you want everything frobbed,
			     ;; but with a tag you want only the tag frobbed
			     ;; and hence you probably don't want lots and lots
			     ;; of items there. (#### Perhaps the best way --
			     ;; or at least a way with some theoretical
			     ;; justifiability -- is to fetch the fallbacks
			     ;; that match the TAG-SET/EXACT-P, and if none,
			     ;; fall back onto doing the selected-device
			     ;; trick.)
			     (and (not tag-set) (not exact-p)))))
		  (if (and (not doit) (eq locale 'global))
		      (error
		       "No fallback for specifier property %s in face %s???"
		       prop face))
		  ;; [note 1] whenever we add to a tty property,
		  ;; make sure we tag our additions with `tty' to
		  ;; avoid accidentally messing things up on window
		  ;; systems (e.g. when making things italic we
		  ;; don't want to set the underline property on
		  ;; window systems)
		  (when doit
		    (add-spec-list-to-specifier
		     propspec
		     (list (cons locale
				 (add-tag-to-inst-list
				  doit
				  (append (if (listp tag-set) tag-set
					    (list tag-set))
					  (if (eq devtype-spec 'tty) '(tty)))
				  ))))
		    (setq do-something t)))))
	  (when do-something
	    (map-over-locales (or (global-locale locale) locale))))

	;; then do the third stage -- check for whether we have to do
	;; the inheritance trick.

	(when (and check-differences
		   (let ((new-instance
			  (face-property-instance face win-prop domain)))
		     (and
		      (equal orig-instance new-instance)
		      (equal orig-instance
			     (face-property-instance unfrobbed-face win-prop
						     domain)))))
	  (set-face-property face win-prop (vector frobbed-face)
			     (or (global-locale locale) locale) tag-set))))))

;; WE DEMAND FOUNDRY FROBBING!

;; Family frobbing
;; Thx Jan Vroonhof, Ref xemacs-beta <87oflypbum.fsf@petteflet.ntlworld.com>
;; Brainlessly derived from make-face-size by Stephen; don't blame Jan.
;; I'm long since flown to Rio, it does you little good to blame me, either.
(defun make-face-family (face family &optional locale tags exact-p)
  "Set FACE's family to FAMILY in LOCALE, if possible."
  (interactive (list (read-face-name "Set family of which face: ")
		     (read-string "Family to set: ")))

  (Face-frob-property face locale tags exact-p
		      nil nil 'font nil
		      ;; #### this code is duplicated in make-face-size
		      `(lambda (f d)
			 ;; keep the dependency on font.el for now
			 ;; #### The filter on null d is a band-aid.
			 ;; Frob-face-property should not be passing in
			 ;; null devices.
			 (unless (or (null d) (eq d 'tty))
			   (let ((fo (font-create-object f d)))
			     (set-font-family fo ,family)
			     (font-create-name fo d))))
		      nil))

;; Style (ie, typographical face) frobbing
(defun make-face-bold (face &optional locale tags exact-p)
  "Make FACE bold in LOCALE, if possible.
This will attempt to make the font bold for window-system locales and will
set the highlight flag for TTY locales.

The actual behavior of this function is somewhat messy, in an attempt to
get more intuitive behavior in quite a lot of different circumstances. (You
might view this as indicative of design failures with specifiers, but in
fact almost all code that attempts to interface to humans and produce
\"intuitive\" results gets messy, particularly with a system as complicated
as specifiers, whose complexity results from an attempt to work well in
many different circumstances.)

The meaning of LOCALE is the same as for `specifier-spec-list', i.e.:

-- If LOCALE is nil, omitted, or `all', this will attempt to \"frob\" all
   font specifications for FACE to make them appear bold (i.e. the
   specifications are replaced with equivalent specifications, where the
   font names have been changed to the closest bold font).

-- If LOCALE is a locale type \(`buffer', `window', etc.), this frobs all
   font specifications for locales of that type.

-- If LOCALE is a particular locale, this frobs all font specifications for
   that locale.

If TAGS is given, this only processes instantiators whose tag set includes
all tags mentioned in TAGS.  In addition, if EXACT-P is non-nil, only
instantiators whose tag set exactly matches TAGS are processed; otherwise,
additional tags may be present in the instantiator's tag set.

This function proceeeds in three stages.

STAGE 1: Frob the settings that are already present.
STAGE 2: (if called for) Ensure that *some* setting exists in the locale
         that was given, finding it in various ways and frobbing it as in
         stage 1.  This ensures that there is an actual setting for
         the locale, so you will get the expected buffer-local/frame-local
         behavior -- changes to the global value, to other locales, won't
         affect this locale, (b) the face will actually look bold in
         the locale.
STAGE 3: (if called for)

The way the frobbing works depends on the device type -- first on whether
or not it's TTY, and second, if it's a window-system device type, on which
particular window-system device type.  For locales with a specific device
type, we do the frobbing in the context of that device type -- this means
that for TTY device types we set the highlight flag, and for window-system
device types we modify the font spec according to the rules for font specs
of that device type.  For global locales, we may process both the highlight
flag and the font specs (depending on the device types compiled into this
XEmacs).  When processing font specs, we check the tag set associated with
each font spec to see if it's specific to a particular device type; if so,
we frob it in the context of that type, else we use the type of the current
device. (A hack, but works well in practice -- and if a new device is
created, we will automatically frob all the standard fonts to make sure
they display OK on that device.)

If LOCALE is not a locale type, and both TAGS and EXACT-P are omitted, we
do further frobbing in an attempt to give more intuitive behavior.

First, if there are no specifications in LOCALE (if LOCALE is `all', we act
as if it were `global' for this step), we do our utmost to put a
specification there; otherwise, this function will have no effect.  For
device, frame, or window locales, the face's font is instantiated using the
locale as a domain, and the resulting font is frobbed and added back as a
specification for this locale.  If LOCALE is `global', we retrieve the
fallback specs and frob them.  If LOCALE is a buffer, things get tricky
since you can't instantiate a specifier in a buffer domain \(the buffer can
appear in multiple places, or in different places over time, so this
operation is not well-defined).  We used to signal an error in this case,
but now we instead try to do something logical so that we work somewhat
similarly to buffer-local variables.  Specifically, we use
`get-buffer-window' to find a window viewing the buffer, and if there is
one, use this as a domain to instantiate the font, and frob the resulting
value.  Otherwise, we use the selected window for the same purpose.

Finally, if the frobbing didn't actually make the font look any different
in whatever domain we instantiated the font in (this happens, for example,
if your font specification is already bold or has no bold equivalent; note
that in this step, we use the selected device in place of `global' or `all'
-- another hack, but works well in practice since there's usually only one
device), and the font currently looks like the font of the `default' face,
it is set to inherit from the `bold' face.

NOTE: For the other functions defined below, the identity of these two
standard faces mentioned in the previous paragraph, and the TTY properties
that are modified, may be different, and whether the TTY property or
properties are set or unset may be different.  For example, for
`make-face-unitalic', the last sentence in the previous paragraph would
read \"... and the font currently looks like the font of the `italic' face,
it is set to inherit from the `default' face.\", and the second sentence in
the first paragraph would read \"This will attempt to make the font
non-italic for window-system locales and will unset the underline flag for
TTY locales.\"

Here's a table indicating the behavior differences with the different
functions:

function                face1     face2         tty-props            tty-val
----------------------------------------------------------------------------
make-face-bold          default   bold          highlight            t
make-face-italic        default   italic        underline            t
make-face-bold-italic   default   bold-italic   highlight,underline  t
make-face-unbold        bold      default       highlight            nil
make-face-unitalic      italic    default       underline            nil
"
  (interactive (list (read-face-name "Make which face bold: ")))
  (Face-frob-property face locale tags exact-p
		      'default 'bold 'font '(highlight)
		      '(tty		(lambda (f d) t)
			x		x-make-font-bold
			gtk		gtk-make-font-bold
			mswindows	mswindows-make-font-bold
			msprinter	mswindows-make-font-bold)
		      '(([default] . [bold])
			([bold] . t)
			([italic] . [bold-italic])
			([bold-italic] . t))))

(defun make-face-italic (face &optional locale tags exact-p)
  "Make FACE italic in LOCALE, if possible.
This will attempt to make the font italic for X/MS Windows locales and
will set the underline flag for TTY locales.  See `make-face-bold' for
the semantics of the LOCALE argument and for more specifics on exactly
how this function works."
  (interactive (list (read-face-name "Make which face italic: ")))
  (Face-frob-property face locale tags exact-p
		      'default 'italic 'font '(underline)
		      '(tty		(lambda (f d) t)
			x		x-make-font-italic
			gtk		gtk-make-font-italic
			mswindows	mswindows-make-font-italic
			msprinter	mswindows-make-font-italic)
		      '(([default] . [italic])
			([bold] . [bold-italic])
			([italic] . t)
			([bold-italic] . t))))

(defun make-face-bold-italic (face &optional locale tags exact-p)
  "Make FACE bold and italic in LOCALE, if possible.
This will attempt to make the font bold-italic for X/MS Windows
locales and will set the highlight and underline flags for TTY
locales.  See `make-face-bold' for the semantics of the LOCALE
argument and for more specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face bold-italic: ")))
  (Face-frob-property face locale tags exact-p
		      'default 'bold-italic 'font '(underline highlight)
		      '(tty		(lambda (f d) t)
			x		x-make-font-bold-italic
			gtk		gtk-make-font-bold-italic
			mswindows	mswindows-make-font-bold-italic
			msprinter	mswindows-make-font-bold-italic)
		      '(([default] . [italic])
			([bold] . [bold-italic])
			([italic] . [bold-italic])
			([bold-italic] . t))))


(defun make-face-unbold (face &optional locale tags exact-p)
  "Make FACE non-bold in LOCALE, if possible.
This will attempt to make the font non-bold for X/MS Windows locales
and will unset the highlight flag for TTY locales.  See
`make-face-bold' for the semantics of the LOCALE argument and for more
specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face non-bold: ")))
  (Face-frob-property face locale tags exact-p
		      'bold 'default 'font '(highlight)
		      '(tty		(lambda (f d) nil)
			x		x-make-font-unbold
			gtk		gtk-make-font-unbold
			mswindows	mswindows-make-font-unbold
			msprinter	mswindows-make-font-unbold)
		      '(([default] . t)
			([bold] . [default])
			([italic] . t)
			([bold-italic] . [italic]))))

(defun make-face-unitalic (face &optional locale tags exact-p)
  "Make FACE non-italic in LOCALE, if possible.
This will attempt to make the font non-italic for X/MS Windows locales
and will unset the underline flag for TTY locales.  See
`make-face-bold' for the semantics of the LOCALE argument and for more
specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face non-italic: ")))
  (Face-frob-property face locale tags exact-p
		      'italic 'default 'font '(underline)
		      '(tty		(lambda (f d) nil)
			x		x-make-font-unitalic
			gtk		gtk-make-font-unitalic
			mswindows	mswindows-make-font-unitalic
			msprinter	mswindows-make-font-unitalic)
		      '(([default] . t)
			([bold] . t)
			([italic] . [default])
			([bold-italic] . [bold]))))


;; Size frobbing
;; Thx Jan Vroonhof, Ref xemacs-beta <87oflypbum.fsf@petteflet.ntlworld.com>
;; Jan had a separate helper function
(defun make-face-size (face size &optional locale tags exact-p)
  "Adjust FACE to SIZE in LOCALE, if possible."
  (interactive (list (read-face-name "Set size of which face: ")
		     (read-number "Size to set: " t 10)))
  (Face-frob-property face locale tags exact-p
		      nil nil 'font nil
		      ;; #### this code is duplicated in make-face-family
		      `(lambda (f d)
			 ;; keep the dependency on font.el for now
			 ;; #### The filter on null d is a band-aid.
			 ;; Frob-face-property should not be passing in
			 ;; null devices.
			 (unless (or (null d) (eq d 'tty))
			   (let ((fo (font-create-object f d)))
			     (set-font-size fo ,size)
			     (font-create-name fo d))))
		      nil))

;; Why do the following two functions lose so badly in so many
;; circumstances?

(defun make-face-smaller (face &optional locale tags exact-p)
  "Make the font of FACE be smaller, if possible.
LOCALE works as in `make-face-bold' et al., but the ``inheriting-
from-the-bold-face'' operations described there are not done
because they don't make sense in this context."
  (interactive (list (read-face-name "Shrink which face: ")))
  (Face-frob-property face locale tags exact-p
		      nil nil 'font nil
		      '(x		x-find-smaller-font
			gtk		gtk-find-smaller-font
			mswindows	mswindows-find-smaller-font
			msprinter	mswindows-find-smaller-font)
		      nil))

(defun make-face-larger (face &optional locale tags exact-p)
  "Make the font of FACE be larger, if possible.
See `make-face-smaller' for the semantics of the LOCALE argument."
  (interactive (list (read-face-name "Enlarge which face: ")))
  (Face-frob-property face locale tags exact-p
		      nil nil 'font nil
		      '(x		x-find-larger-font
			gtk		gtk-find-larger-font
			mswindows	mswindows-find-larger-font
			msprinter	mswindows-find-larger-font)
		      nil))

(defun invert-face (face &optional locale)
  "Swap the foreground and background colors of the face."
  (interactive (list (read-face-name "Invert face: ")))
  (if (valid-specifier-domain-p locale)
      (let ((foreface (face-foreground-instance face locale)))
	(set-face-foreground face (face-background-instance face locale)
			     locale)
	(set-face-background face foreface locale))
    (let ((forespec (copy-specifier (face-foreground face) nil locale)))
      (copy-specifier (face-background face) (face-foreground face) locale)
      (copy-specifier forespec (face-background face) locale))))


;;; Convenience functions

(defun face-ascent (face &optional domain charset)
  "Return the ascent of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-ascent (face-font face) domain charset))

(defun face-descent (face &optional domain charset)
  "Return the descent of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-descent (face-font face) domain charset))

(defun face-width (face &optional domain charset)
  "Return the width of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-width (face-font face) domain charset))

(defun face-height (face &optional domain charset)
  "Return the height of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (+ (face-ascent face domain charset) (face-descent face domain charset)))

(defun face-proportional-p (face &optional domain charset)
  "Return t if FACE is proportional in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-proportional-p (face-font face) domain charset))


;; Functions that used to be in cus-face.el, but logically go here.

(defcustom frame-background-mode nil
  "*The brightness of the background.
Set this to the symbol dark if your background color is dark, light if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you."
  :group 'faces
  :type '(choice (choice-item dark)
		 (choice-item light)
		 (choice-item :tag "Auto" nil)))

;; The old variable that many people still have in .emacs files.
(define-obsolete-variable-alias 'custom-background-mode
  'frame-background-mode)

(defun get-frame-background-mode (frame)
  "Detect background mode for FRAME."
  (let* ((color-instance (face-background-instance 'default frame))
	 (mode (condition-case nil
		   (if (< (apply '+ (color-instance-rgb-components
				     color-instance)) 65536)
		       'dark 'light)
		 ;; Here, we get an error on a TTY (Return value from
		 ;; color-instance-rgb-components is nil), and on the
		 ;; initial stream device (Return value from
		 ;; face-background-instance is nil).  As we don't have a
		 ;; good way of detecting whether a TTY is light or dark,
		 ;; we'll guess it's dark.
		 (error 'dark))))
    (set-frame-property frame 'background-mode mode)
    mode))

(defun extract-custom-frame-properties (frame)
  "Return a plist with the frame properties of FRAME used by custom."
  (list 'type (or (frame-property frame 'display-type)
		  (device-type (frame-device frame)))
	'class (device-class (frame-device frame))
	'background (or frame-background-mode
			(get-frame-background-mode frame))))

(defcustom init-face-from-resources t
  "If non nil, attempt to initialize faces from the resource database."
  :group 'faces
  :type 'boolean)

;; Old name, used by custom.  Also, FSFmacs name.
(defvaralias 'initialize-face-resources 'init-face-from-resources)

;; Make sure all custom setting are added with this tag so we can
;; identify-them
(define-specifier-tag 'custom)

(defun face-spec-set (face spec &optional frame tags)
  "Set FACE's face attributes according to the first matching entry in SPEC.
If optional FRAME is non-nil, set it for that frame only.
If it is nil, then apply SPEC to each frame individually.
See `defface' for information about SPEC."
  (if frame
      (progn
	(reset-face face frame tags)
	(face-display-set face spec frame tags)
	(init-face-from-resources face frame))
    (let ((frames (relevant-custom-frames)))
      (reset-face face nil tags)
      ;; This should not be needed. We only remove our own specifiers
      ;; (if (and (eq 'default face) (featurep 'x))
      ;;	  (x-init-global-faces))
      (face-display-set face spec nil tags)
      (while frames
	(face-display-set face spec (car frames) tags)
	(pop frames))
      (init-face-from-resources face))))

(defun face-display-set (face spec &optional frame tags)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (while spec
    (let ((display (caar spec))
	  (atts (cadar spec)))
      (pop spec)
      (when (face-spec-set-match-display display frame)
	;; Avoid creating frame local duplicates of the global face.
	(unless (and frame (eq display (get face 'custom-face-display)))
	  (apply 'face-custom-attributes-set face frame tags atts))
	(unless frame
	  (put face 'custom-face-display display))
	(setq spec nil)))))

(defvar default-custom-frame-properties nil
  "The frame properties used for the global faces.
Frames not matching these properties should have frame local faces.
The value should be nil, if uninitialized, or a plist otherwise.
See `defface' for a list of valid keys and values for the plist.")

(defun get-custom-frame-properties (&optional frame)
  "Return a plist with the frame properties of FRAME used by custom.
If FRAME is nil, return the default frame properties."
  (cond (frame (extract-custom-frame-properties frame))
	(default-custom-frame-properties)
	(t
	 (setq default-custom-frame-properties
	       (extract-custom-frame-properties (selected-frame))))))

(defun face-spec-update-all-matching (spec display plist)
  "Update all entries in the face spec that could match display to
have the entries from the new plist and return the new spec."
  (mapcar
   (lambda (e)
     (let ((entries (car e))
	   (options (cadr e))
	   (match t)
	   dplist
	   (new-options plist)
	   )
       (unless (eq display t)
	 (mapc (lambda (arg)
		 (setq dplist (plist-put dplist (car arg) (cadr arg))))
	       display))
       (unless (eq entries t)
	 (mapc (lambda (arg)
		 (setq match (and match (eq (cadr arg)
					    (plist-get
					      dplist (car arg)
					      (cadr arg))))))
	       entries))
       (if (not match)
	   e
	 (while new-options
	   (setq options
		 (plist-put options (car new-options) (cadr new-options)))
	   (setq new-options (cddr new-options)))
	 (list entries options))))
   (copy-sequence spec)))



(defun face-spec-set-match-display (display &optional frame)
  "Return non-nil if DISPLAY matches FRAME.
DISPLAY is part of a spec such as can be used in `defface'.
If FRAME is nil or omitted, the selected frame is used."
  (if (eq display t)
      t
    (let* ((props (get-custom-frame-properties frame))
	   (type (plist-get props 'type))
	   (class (plist-get props 'class))
	   (background (plist-get props 'background))
	   (match t)
	   (entries display)
	   entry req options)
      (while (and entries match)
	(setq entry (car entries)
	      entries (cdr entries)
	      req (car entry)
	      options (cdr entry)
	      match (case req
		      (type       (memq type options))
		      (class      (memq class options))
		      (background (memq background options))
		      ;; `display-color-cells' can return nil (eg, TTYs).
		      ;; If so, assume monochrome.
		      (min-colors (>= (or (display-color-cells frame) 2)
				      (car options)))
		      (t (warn "Unknown req `%S' with options `%S'"
			       req options)
			 nil))))
      match)))

(defun relevant-custom-frames ()
  "List of frames whose custom properties differ from the default."
  (let ((relevant nil)
	(default (get-custom-frame-properties))
	(frames (frame-list))
	frame)
    (while frames
      (setq frame (car frames)
	    frames (cdr frames))
      (unless (equal default (get-custom-frame-properties frame))
	(push frame relevant)))
    relevant))

(defun initialize-custom-faces (&optional frame)
  "Initialize all custom faces for FRAME.
If FRAME is nil or omitted, initialize them for all frames."
  (mapc (lambda (symbol)
	  (let ((spec (or (get symbol 'saved-face)
			  (get symbol 'face-defface-spec))))
	    (when spec
	      ;; No need to init-face-from-resources -- code in
	      ;; `init-frame-faces' does it already.
	      (face-display-set symbol spec frame))))
	(face-list)))

(defun custom-initialize-frame (frame)
  "Initialize frame-local custom faces for FRAME if necessary."
  (unless (equal (get-custom-frame-properties)
		 (get-custom-frame-properties frame))
    (initialize-custom-faces frame)))

(defun startup-initialize-custom-faces ()
  "Reset faces created by defface.  Only called at startup.
Don't use this function in your program."
  (when default-custom-frame-properties
    ;; Reset default value to the actual frame, not stream.
    (setq default-custom-frame-properties
	  (extract-custom-frame-properties (selected-frame)))
    ;; like initialize-custom-faces but removes property first.
    (mapc (lambda (symbol)
	    (let ((spec (or (get symbol 'saved-face)
			    (get symbol 'face-defface-spec))))
	      (when spec
		;; Reset faces created during auto-autoloads loading.
		(reset-face symbol)
		;; And set it according to the spec.
		(face-display-set symbol spec nil))))
	  (face-list))))


(defun make-empty-face (name &optional doc-string temporary)
  "Like `make-face', but doesn't query the resource database."
  (let ((init-face-from-resources nil))
    (make-face name doc-string temporary)))

(defun init-face-from-resources (face &optional locale)
  "Initialize FACE from the resource database.
If LOCALE is specified, it should be a frame, device, or `global', and
the face will be resourced over that locale.  Otherwise, the face will
be resourced over all possible locales (i.e. all frames, all devices,
and `global')."
  (cond ((null init-face-from-resources)
	 ;; Do nothing.
	 )
	((not locale)
	 ;; Global, set for all frames.
	 (progn
	   (init-face-from-resources face 'global)
	   (let ((devices (device-list)))
	     (while devices
	       (init-face-from-resources face (car devices))
	       (setq devices (cdr devices))))
	   (let ((frames (frame-list)))
	     (while frames
	       (init-face-from-resources face (car frames))
	       (setq frames (cdr frames))))))
	(t
	 ;; Specific.
	 (let ((devtype (cond ((devicep locale) (device-type locale))
			      ((framep locale) (frame-type locale))
			      (t nil))))
	   (cond ((or (and (not devtype) (featurep 'x)) (eq 'x devtype))
		  (declare-fboundp (x-init-face-from-resources face locale)))
		 ((or (not devtype) (eq 'tty devtype))
		  ;; Nothing to do for TTYs?
		  ))))))

(defun init-device-faces (device)
  ;; First, add any device-local face resources.
  (when init-face-from-resources
    (loop for face in (face-list) do
	  (init-face-from-resources face device))
    ;; Then do any device-specific initialization.
    (cond ((eq 'x (device-type device))
	   (declare-fboundp (x-init-device-faces device)))
	  ((eq 'gtk (device-type device))
	   (declare-fboundp (gtk-init-device-faces device)))
	  ((eq 'mswindows (device-type device))
	   (declare-fboundp (mswindows-init-device-faces device)))
	  ;; Nothing to do for TTYs?
	  )
    (or (eq 'stream (device-type device))
	(init-other-random-faces device))))

(defun init-frame-faces (frame)
  (when init-face-from-resources
    ;; First, add any frame-local face resources.
    (loop for face in (face-list) do
	  (init-face-from-resources face frame))
    ;; Then do any frame-specific initialization.
    (cond ((eq 'x (frame-type frame))
	   (declare-fboundp (x-init-frame-faces frame)))
	  ((eq 'gtk (frame-type frame))
	   (declare-fboundp (gtk-init-frame-faces frame)))
	  ((eq 'mswindows (frame-type frame))
	   (declare-fboundp (mswindows-init-frame-faces frame)))
	  ;; Is there anything which should be done for TTY's?
	  )))

;; Called when the first device created.

(defun init-global-faces (device)
  (let ((Face-frob-property-device-considered-current device))
    ;; Look for global face resources.
    (loop for face in (face-list) do
      (init-face-from-resources face 'global))
    ;; Further frobbing.
    (and (featurep 'x) (declare-fboundp (x-init-global-faces)))
    (and (featurep 'gtk) (declare-fboundp (gtk-init-global-faces)))
    (and (featurep 'mswindows) (declare-fboundp (mswindows-init-global-faces)))

    ;; for bold and the like, make the global specification be bold etc.
    ;; if the user didn't already specify a value.  These will also be
    ;; frobbed further in init-other-random-faces.
    (unless (face-font 'bold 'global)
      (make-face-bold 'bold 'global))
    ;;
    (unless (face-font 'italic 'global)
      (make-face-italic 'italic 'global))
    ;;
    (unless (face-font 'bold-italic 'global)
      (make-face-bold-italic 'bold-italic 'global)
      (unless (face-font 'bold-italic 'global)
	(copy-face 'bold 'bold-italic)
	(make-face-italic 'bold-italic)))

    (when (face-equal 'bold 'bold-italic device)
      (copy-face 'italic 'bold-italic)
      (make-face-bold 'bold-italic))))


;; These warnings are there for a reason.  Just specify your fonts
;; correctly.  Deal with it.  Additionally, one can use
;; `log-warning-minimum-level' instead of this.
;(defvar inhibit-font-complaints nil
;  "Whether to suppress complaints about incomplete sets of fonts.")

(defun face-complain-about-font (face device)
  (if (symbolp face) (setq face (symbol-name face)))
;;  (if (not inhibit-font-complaints)
  ;; complaining for printers is generally annoying.
  (unless (device-printer-p device)
    (display-warning
	'font
      (let ((default-name (face-font-name 'default device)))
	(format "%s: couldn't deduce %s %s version of the font
%S.

Please specify X resources to make the %s face
visually distinguishable from the default face.
For example, you could add one of the following to $HOME/Emacs:

XEmacs.%s.attributeFont: -dt-*-medium-i-*
or
XEmacs.%s.attributeForeground: hotpink\n"
		invocation-name
		(if (string-match "\\`[aeiouAEIOU]" face) "an" "a")
		face
		default-name
		face
		face
		face
		)))))


;; #### This is quite a mess.  We should use the custom mechanism for
;; most of this stuff.  Currently we don't do it, because Custom
;; doesn't use specifiers (yet.)  FSF does it the Right Way.

;; For instance, the definition of `bold' should be something like
;; (defface bold ((t (:bold t))) "Bold text.") -- and `:bold t' should
;; make sure that everything works properly.

(defun init-other-random-faces (device)
  "Initialize the colors and fonts of the bold, italic, bold-italic,
zmacs-region, list-mode-item-selected, highlight, primary-selection,
secondary-selection, and isearch faces when each device is created.  If
you want to add code to do stuff like this, use the create-device-hook."

  ;; try to make 'bold look different from the default on this device.
  ;; If that doesn't work at all, then issue a warning.
  (unless (face-differs-from-default-p 'bold device)
    (make-face-bold 'bold device)
    (unless (face-differs-from-default-p 'bold device)
      (make-face-unbold 'bold device)
      (unless (face-differs-from-default-p 'bold device)
	;; the luser specified one of the bogus font names
	(face-complain-about-font 'bold device))))

  ;; Similar for italic.
  ;; It's unreasonable to expect to be able to make a font italic all
  ;; the time.  For many languages, italic is an alien concept.
  ;; Basically, because italic is not a globally meaningful concept,
  ;; the use of the italic face should really be obsoleted.

  ;; I disagree with above.  In many languages, the concept of capital
  ;; letters is just as alien, and yet we use them.  Italic is here to
  ;; stay.  -hniksic

  ;; In a Solaris Japanese environment, there just aren't any italic
  ;; fonts - period.  CDE recognizes this reality, and fonts
  ;; -dt-interface user-medium-r-normal-*-*-*-*-*-*-*-*-* don't come
  ;; in italic versions.  So we first try to make the font bold before
  ;; complaining.
  (unless (face-differs-from-default-p 'italic device)
    (make-face-italic 'italic device)
    (unless (face-differs-from-default-p 'italic device)
      (make-face-bold 'italic device)
      (unless (face-differs-from-default-p 'italic device)
	(face-complain-about-font 'italic device))))

  ;; similar for bold-italic.
  (unless (face-differs-from-default-p 'bold-italic device)
    (make-face-bold-italic 'bold-italic device)
    ;; if we couldn't get a bold-italic version, try just bold.
    (unless (face-differs-from-default-p 'bold-italic device)
      (make-face-bold 'bold-italic device)
      ;; if we couldn't get bold or bold-italic, then that's probably because
      ;; the default font is bold, so make the `bold-italic' face be unbold.
      (unless (face-differs-from-default-p 'bold-italic device)
	(make-face-unbold 'bold-italic device)
	(make-face-italic 'bold-italic device)
	(unless (face-differs-from-default-p 'bold-italic device)
	  ;; if that didn't work, try plain italic
	  ;; (can this ever happen? what the hell.)
	  (make-face-italic 'bold-italic device)
	  (unless (face-differs-from-default-p 'bold-italic device)
	    ;; then bitch and moan.
	    (face-complain-about-font 'bold-italic device))))))

  ;; Set the text-cursor colors unless already specified.
  (when (and (not (eq 'tty (device-type device)))
	     (not (face-background 'text-cursor 'global))
	     (face-property-equal 'text-cursor 'default 'background device))
    (set-face-background 'text-cursor [default foreground] 'global
			 nil 'append))
  (when (and (not (eq 'tty (device-type device)))
	     (not (face-foreground 'text-cursor 'global))
	     (face-property-equal 'text-cursor 'default 'foreground device))
    (set-face-foreground 'text-cursor [default background] 'global
			 nil 'append))

  ;; The faces buffers-tab, modeline-mousable and modeline-buffer-id all
  ;; inherit directly from modeline; they require that modeline's details be
  ;; specified, that it not use fallbacks, otherwise *they* use the general
  ;; fallback of the default face instead, which clashes with the gui
  ;; element faces. So take the modeline face information from its
  ;; fallbacks, themselves ultimately set up in faces.c:
  (loop
    for face-property in '(foreground foreback background 
			   background-pixmap background-placement)
    do (when (and (setq face-property (face-property 'modeline face-property))
                  (null (specifier-instance face-property device nil t))
                  (specifier-instance face-property device))
         (set-specifier face-property
                        (or (specifier-specs (specifier-fallback
                                              face-property))
                            ;; This will error at startup if the
                            ;; corresponding C fallback doesn't exist,
                            ;; which is well and good.
                            (specifier-fallback (specifier-fallback
                                                 face-property))))))
  nil)

;; New function with 20.1, suggested by Per Abrahamsen, coded by Kyle
;; Jones and Hrvoje Niksic.
(defun set-face-stipple (face pixmap &optional frame)
  "Change the stipple pixmap of FACE to PIXMAP.
This is an Emacs compatibility function; consider using
set-face-background-pixmap instead.

PIXMAP should be a string, the name of a file of pixmap data.
The directories listed in the variables `x-bitmap-file-path' and
`mswindows-bitmap-file-path' under X and MS Windows respectively
are searched.

Alternatively, PIXMAP may be a list of the form (WIDTH HEIGHT
DATA) where WIDTH and HEIGHT are the size in pixels, and DATA is
a string, containing the raw bits of the bitmap.  XBM data is
expected in this case, other types of image data will not work.

If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (while (not (find-face face))
    (setq face (wrong-type-argument 'facep face)))
  (let ((bitmap-path
	 (ecase (console-type)
	   (x         (declare-boundp x-bitmap-file-path))
	   (mswindows (declare-boundp mswindows-bitmap-file-path))))
	instantiator)
    (while
	(null
	 (setq instantiator
	       (cond ((stringp pixmap)
		      (let ((file (if (file-name-absolute-p pixmap)
				      pixmap
				    (locate-file pixmap bitmap-path
						 '(".xbm" "")))))
			(and file
			     `[xbm :file ,file])))
		     ((and (listp pixmap) (eql (length pixmap) 3))
		      `[xbm :data ,pixmap])
		     (t nil))))
      ;; We're signaling a continuable error; let's make sure the
      ;; function `stipple-pixmap-p' at least exists.
      (labels ((stipple-pixmap-p (pixmap)
                 (or (stringp pixmap)
                     (and (listp pixmap) (eql (length pixmap) 3)))))
	(setq pixmap (signal 'wrong-type-argument
			     (list #'stipple-pixmap-p pixmap)))))
    (check-type frame (or null frame))
    (set-face-background-pixmap face instantiator frame)))


;; Create the remaining standard faces now.  This way, packages that we dump
;; can reference these faces as parents.
;;
;; The default, modeline, left-margin, right-margin, text-cursor,
;; and pointer faces are created in C.

(make-face 'bold "Bold text.")
(make-face 'italic "Italic text.")
(make-face 'bold-italic "Bold-italic text.")
(make-face 'underline "Underlined text.")
(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t 'global '(default)))
(make-face 'zmacs-region "Used on highlighted region between point and mark.")
(make-face 'isearch "Used on region matched by isearch.")
(make-face 'isearch-secondary "Face to use for highlighting all matches.")
(make-face 'list-mode-item-selected
	   "Face for the selected list item in list-mode.")
(make-face 'highlight "Highlight face.")
(make-face 'primary-selection "Primary selection face.")
(make-face 'secondary-selection "Secondary selection face.")

;; Several useful color faces.
(dolist (color '(red green blue yellow))
  (make-face color (concat (symbol-name color) " text."))
  (set-face-foreground color (symbol-name color) nil 'color))

;; Make some useful faces.  This happens very early, before creating
;; the first non-stream device.

(set-face-background 'text-cursor
		     '(((win default) . "Red3"))
		     'global)

;; some older X servers don't recognize "darkseagreen2"
(set-face-background 'highlight
		     '(((win default color) . "darkseagreen2")
		       ((win default color) . "green")
		       ((win default grayscale) . "gray53"))
		     'global)
(set-face-background-pixmap 'highlight
			    '(((win default mono) . "gray1"))
			    'global)

;; We need to set this face to not shrink *explicitely* in order to force
;; covering a shrinked selection. -- dvl
(set-face-shrink-p 'zmacs-region nil)
(set-face-background 'zmacs-region
		     '(((win default color) . "gray65")
		       ((win default grayscale) . "gray65"))
		     'global)
(set-face-background-pixmap 'zmacs-region
			    '(((win default mono) . "gray3"))
			    'global)

(set-face-background 'list-mode-item-selected
		     '(((win default color) . "gray68")
		       ((win default grayscale) . "gray68")
		       ((win default mono) . [default foreground]))
		     'global)
(set-face-foreground 'list-mode-item-selected
		     '(((win default mono) . [default background]))
		     'global)

(set-face-background 'primary-selection
		     '(((win default color) . "gray65")
		       ((win default grayscale) . "gray65"))
		     'global)
(set-face-background-pixmap 'primary-selection
			    '(((win default mono) . "gray3"))
			    'global)

(set-face-background 'secondary-selection
		     '(((win default color) . "paleturquoise")
		       ((win default color) . "green")
		       ((win default grayscale) . "gray53"))
		     'global)
(set-face-background-pixmap 'secondary-selection
			    '(((win default mono) . "gray1"))
			    'global)

(set-face-background 'isearch
		     '(((win default color) . "paleturquoise")
		       ((win default color) . "green"))
		     'global)

;; #### This should really, I mean *really*, be converted to some form
;; of `defface' one day.
(set-face-foreground 'isearch-secondary
		     '(((win default color) . "red3"))
		     'global)

;; Define some logical color names to be used when reading the pixmap files.
(and-boundp 
    'xpm-color-symbols
  (featurep 'xpm)
  (setq xpm-color-symbols
        (list
         '("foreground" (face-foreground 'default))
         '("background" (face-background 'default))
         `("backgroundToolBarColor"
           ,(if (featurep 'x)
		'(or (x-get-resource "backgroundToolBarColor"
				     "BackgroundToolBarColor" 'string
				     nil nil 'warn)
		  (face-background 'toolbar))
	      '(face-background 'toolbar)))
         `("foregroundToolBarColor"
           ,(if (featurep 'x)
		'(or (x-get-resource "foregroundToolBarColor"
				     "ForegroundToolBarColor" 'string
				     nil nil 'warn)
		  (face-foreground 'toolbar))
	      '(face-foreground 'toolbar))))))

(when (featurep 'tty)
  (set-face-highlight-p 'bold                    t 'global '(default tty))
  (set-face-underline-p 'italic                  t 'global '(default tty))
  (set-face-highlight-p 'bold-italic             t 'global '(default tty))
  (set-face-underline-p 'bold-italic             t 'global '(default tty))
  (set-face-highlight-p 'highlight               t 'global '(default tty))
  (set-face-reverse-p   'text-cursor             t 'global '(default tty))
  (set-face-reverse-p   'modeline                t 'global '(default tty))
  (set-face-reverse-p   'zmacs-region            t 'global '(default tty))
  (set-face-reverse-p   'primary-selection       t 'global '(default tty))
  (set-face-underline-p 'secondary-selection     t 'global '(default tty))
  (set-face-reverse-p   'list-mode-item-selected t 'global '(default tty))
  (set-face-reverse-p   'isearch                 t 'global '(default tty))
  )

;;; faces.el ends here
