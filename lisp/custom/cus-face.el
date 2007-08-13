;;; cus-face.el -- Support for Custom faces.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Hrvoje Niksic <hniksic@srce.hr>
;; Keywords: help, faces
;; Version: 1.9960-x
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;; This file should probably be dissolved, and code moved to faces.el,
;; like Stallman did.

;;; Code:

(require 'custom)

;; To elude the warnings for font functions.
(eval-when-compile
  (require 'font))

;;;###autoload
(defcustom frame-background-mode nil
  "*The brightness of the background.
Set this to the symbol dark if your background color is dark, light if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you."
  :group 'faces
  :type '(choice (choice-item dark) 
		 (choice-item light)
		 (choice-item :tag "Auto" nil)))


;; Originally, this did much more stuff, and cached the results.  The
;; trouble is that, if user changes the bg color of a frame's default
;; face, the cache wouldn't get updated.  This version should be fast
;; enough for use without caching, I think.
(defun get-frame-background-mode (frame)
  "Detect background mode for FRAME."
  (let* ((color-instance (face-background-instance 'default frame))
	 (mode (condition-case nil
		   (if (< (apply '+ (color-instance-rgb-components
				     color-instance)) 65536)
		       'dark
		     'light)
	     ;; We'll get an error on a TTY; TTY-s are generally dark.
		 (error 'dark))))
    ;(set-frame-property 'background-mode mode)
    mode))

;;;###autoload
(defcustom initialize-face-resources t
  "If non nil, allow X resources to initialize face properties.
This only affects faces declared with `defface', and only X11 frames."
  :group 'faces
  :type 'boolean)

(defun initialize-face-resources (face &optional frame)
  "Initialize face according to the X11 resources.
This might overwrite existing face properties.
Does nothing when the variable initialize-face-resources is nil."
  (when initialize-face-resources
    (make-face-x-resource-internal face frame t)))

;;(if (string-match "XEmacs" emacs-version)
;;    ;; Xemacs.
;;    (defun custom-invert-face (face &optional frame)
;;      "Swap the foreground and background colors of face FACE.
;;If the colors are not specified in the face, use the default colors."
;;      (interactive (list (read-face-name "Reverse face: ")))
;;      (let ((fg (color-name (face-foreground face frame) frame))
;;	    (bg (color-name (face-background face frame) frame)))
;;	(set-face-foreground face bg frame)
;;	(set-face-background face fg frame)))
;;  ;; Emacs.
;;  (defun custom-invert-face (face &optional frame)
;;    "Swap the foreground and background colors of face FACE.
;;If the colors are not specified in the face, use the default colors."
;;    (interactive (list (read-face-name "Reverse face: ")))
;;    (let ((fg (or (face-foreground face frame)
;;		  (face-foreground 'default frame)
;;		  (frame-property (or frame (selected-frame))
;;				  'foreground-color)
;;		  "black"))
;;	  (bg (or (face-background face frame)
;;		  (face-background 'default frame)
;;		  (frame-property (or frame (selected-frame))
;;				  'background-color)
;;		  "white")))
;;      (set-face-foreground face bg frame)
;;      (set-face-background face fg frame))))

(defun custom-extract-frame-properties (frame)
  "Return a plist with the frame properties of FRAME used by custom."
  (list 'type (device-type (frame-device frame))
	'class (device-class (frame-device frame))
	'background (or frame-background-mode
			(frame-property frame 'background-mode)
			(get-frame-background-mode frame))))

;;; Declaring a face.

;;;###autoload
(defun custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  (when (fboundp 'load-gc)
    ;; This should be allowed, using specifiers.
    (error "Attempt to declare a face during dump"))
  (unless (get face 'face-defface-spec)
    (put face 'face-defface-spec spec)
    (unless (find-face face)
      ;; If the user has already created the face, respect that.
      (let ((value (or (get face 'saved-face) spec))
	    (frames (custom-relevant-frames))
	    frame)
	;; Create global face.
	(make-empty-face face)
	(custom-face-display-set face value)
	;; Create frame local faces
	(while frames
	  (setq frame (car frames)
		frames (cdr frames))
	  (custom-face-display-set face value frame))
	(initialize-face-resources face)))
    (when (and doc (null (face-doc-string face)))
      (set-face-doc-string face doc))
    (custom-handle-all-keywords face args 'custom-face)
    (run-hooks 'custom-define-hook))
  face)

(defun custom-face-background (face &optional frame)
  "Return the background color name of face FACE, or nil if unspecified."
  (color-instance-name (specifier-instance (face-background face) frame)))

(defun custom-face-foreground (face &optional frame)
  "Return the background color name of face FACE, or nil if unspecified."
  (color-instance-name (specifier-instance (face-foreground face) frame)))

;;; Font Attributes.

(defconst custom-face-attributes
  '((:bold (boolean :tag "Bold"
		    :help-echo "Control whether a bold font should be used.")
	   custom-set-face-bold 
	   custom-face-bold)
    (:italic (boolean :tag "Italic"
		      :help-echo "\
Control whether an italic font should be used.")
	     custom-set-face-italic
	     custom-face-italic)
    (:underline (boolean :tag "Underline"
			 :help-echo "\
Control whether the text should be underlined.")
		set-face-underline-p
		face-underline-p)
    (:foreground (color :tag "Foreground"
			:value ""
			:help-echo "Set foreground color.")
		 set-face-foreground
		 custom-face-foreground)
    (:background (color :tag "Background"
			:value ""
			:help-echo "Set background color.")
		 set-face-background
		 custom-face-background)
    ;;    (:invert (const :format "Invert Face\n" 
    ;;		    :sibling-args (:help-echo "
    ;;Reverse the foreground and background color.
    ;;If you haven't specified them for the face, the default colors will be used.")
    ;;		    t)
    ;;	     (lambda (face value &optional frame)
    ;;	       ;; We don't use VALUE.
    ;;	       (custom-invert-face face frame)))
    (:stipple (editable-field :format "Stipple: %v"
			      :help-echo "Name of background bitmap file.")
	      set-face-stipple custom-face-stipple))
  "Alist of face attributes. 

The elements are of the form (KEY TYPE SET GET) where KEY is a symbol
identifying the attribute, TYPE is a widget type for editing the
attibute, SET is a function for setting the attribute value, and GET is a function for getiing the attribute value. 

The SET function should take three arguments, the face to modify, the
value of the attribute, and optionally the frame where the face should
be changed.

The GET function should take two arguments, the face to examine, and
optonally the frame where the face should be examined.")

(defun custom-face-attributes-set (face frame &rest atts)
  "For FACE on FRAME set the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, set the default face."
  (while atts 
    (let* ((name (nth 0 atts))
	   (value (nth 1 atts))
	   (fun (nth 2 (assq name custom-face-attributes))))
      (setq atts (cdr (cdr atts)))
      (condition-case nil
	  (funcall fun face value frame)
	(error nil)))))

(defun custom-face-attributes-get (face frame)
  "For FACE on FRAME get the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, use the default face."
  (condition-case nil
      ;; Attempt to get `font.el' from w3.
      (require 'font)
    (error nil))
  (let ((atts custom-face-attributes)
	att result get)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    get (nth 3 att))
      (condition-case nil
	  ;; This may fail if w3 doesn't exists.
	  (when get
	    (let ((answer (funcall get face frame)))
	      (unless (equal answer (funcall get 'default frame))
		(when (widget-apply (nth 1 att) :match answer)
		  (setq result (cons (nth 0 att) (cons answer result)))))))
	(error nil)))
    result))

(defun custom-set-face-bold (face value &optional frame)
  "Set the bold property of FACE to VALUE."
  (if value
      (make-face-bold face frame)
    (make-face-unbold face frame)))

(defun custom-face-bold (face &rest args)
  "Return non-nil if the font of FACE is bold."
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (font-bold-p fontobj)))

(defun custom-set-face-italic (face value &optional frame)
  "Set the italic property of FACE to VALUE."
  (if value
      (make-face-italic face frame)
    (make-face-unitalic face frame)))

(defun custom-face-italic (face &rest args)
  "Return non-nil if the font of FACE is italic."
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (font-italic-p fontobj)))

(defun custom-face-stipple (face &rest args)
  "Return the name of the stipple file used for FACE."
  (let ((image  (apply 'specifier-instance 
		       (face-background-pixmap face) args)))
    (and image 
	 (image-instance-file-name image))))

(defun custom-set-face-font-size (face size &rest args)
  "Set the font of FACE to SIZE"
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (set-font-size fontobj size)
    (apply 'font-set-face-font face fontobj args)))

(defun custom-face-font-size (face &rest args)
  "Return the size of the font of FACE as a string."
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (format "%s" (font-size fontobj))))

(defun custom-set-face-font-family (face family &rest args)
  "Set the font of FACE to FAMILY."
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (set-font-family fontobj family)
    (apply 'font-set-face-font face fontobj args)))

(defun custom-face-font-family (face &rest args)
  "Return the name of the font family of FACE."
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (font-family fontobj)))

(setq custom-face-attributes
      (append '((:family (editable-field :format "Font Family: %v"
					 :help-echo "\
Name of font family to use (e.g. times).") 
			 custom-set-face-font-family
			 custom-face-font-family)
		(:size (editable-field :format "Size: %v"
				       :help-echo "\
Text size (e.g. 9pt or 2mm).")
		       custom-set-face-font-size
		       custom-face-font-size)
		(:strikethru (toggle :format "%[Strikethru%]: %v\n"
				     :help-echo "\
Control whether the text should be strikethru.")
			     set-face-strikethru-p
			     face-strikethru-p))
	      custom-face-attributes))
;;; Frames.

(defun face-spec-set (face spec &optional frame)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC.

Clear all existing attributes first."
  (copy-face 'custom-face-empty face frame)
  (custom-face-display-set face spec frame))

(defun custom-face-display-set (face spec &optional frame)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (while spec 
    (let* ((entry (car spec))
	   (display (nth 0 entry))
	   (atts (nth 1 entry)))
      (setq spec (cdr spec))
      (when (face-spec-set-match-display display frame)
	;; Avoid creating frame local duplicates of the global face.
	(unless (and frame (eq display (get face 'custom-face-display)))
	  (apply 'custom-face-attributes-set face frame atts))
	(unless frame
	  (put face 'custom-face-display display))
	(setq spec nil)))))

(defvar custom-default-frame-properties nil
  "The frame properties used for the global faces.
Frames who doesn't match these propertiess should have frame local faces.
The value should be nil, if uninitialized, or a plist otherwise.  
See `defface' for a list of valid keys and values for the plist.")

(defun custom-get-frame-properties (&optional frame)
  "Return a plist with the frame properties of FRAME used by custom.
If FRAME is nil, return the default frame properties."
  (cond (frame
	 ;; Try to get from cache.
	 (let ((cache (frame-property frame 'custom-properties)))
	   (unless cache
	     ;; Oh well, get it then.
	     (setq cache (custom-extract-frame-properties frame))
	     ;; and cache it...
	     (modify-frame-parameters frame 
				      (list (cons 'custom-properties cache))))
	   cache))
	(custom-default-frame-properties)
	(t
	 (setq custom-default-frame-properties
	       (custom-extract-frame-properties (selected-frame))))))

(defun face-spec-set-match-display (display frame)
  "Non-nil iff DISPLAY matches FRAME.
If FRAME is nil, the current FRAME is used."
  ;; This is a kludge to get started, we really should use specifiers!
  (if (eq display t)
      t
    (let* ((props (custom-get-frame-properties frame))
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
	      match (cond ((eq req 'type)
			   (memq type options))
			  ((eq req 'class)
			   (memq class options))
			  ((eq req 'background)
			   (memq background options))
			  (t
			   (warn "Unknown req `%S' with options `%S'"
				 req options)
			   nil))))
      match)))

(defun custom-relevant-frames ()
  "List of frames whose custom properties differ from the default."
  (let ((relevant nil)
	(default (custom-get-frame-properties))
	(frames (frame-list))
	frame)
    (while frames
      (setq frame (car frames)
	    frames (cdr frames))
      (unless (equal default (custom-get-frame-properties frame))
	(push frame relevant)))
    relevant))

(defun custom-initialize-faces (&optional frame)
  "Initialize all custom faces for FRAME.
If FRAME is nil or omitted, initialize them for all frames."
  (mapc (lambda (symbol)
	  (let ((spec (or (get symbol 'saved-face)
			  (get symbol 'face-defface-spec))))
	    (when spec 
	      (custom-face-display-set symbol spec frame)
	      (initialize-face-resources symbol frame))))
	(face-list)))

;;;###autoload
(defun custom-initialize-frame (&optional frame)
  "Initialize local faces for FRAME if necessary.
If FRAME is missing or nil, the first member of (frame-list) is used."
  (unless frame
    (setq frame (car (frame-list))))
  (unless (equal (custom-get-frame-properties) 
		 (custom-get-frame-properties frame))
    (custom-initialize-faces frame)))

;;; Initializing.

(make-face 'custom-face-empty)

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC will be stored as the saved value for FACE.  If NOW is present
and non-nil, FACE will also be created according to SPEC.

See `defface' for the format of SPEC."
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((face (nth 0 entry))
		(spec (nth 1 entry))
		(now (nth 2 entry)))
	    (put face 'saved-face spec)
	    (when now
	      (put face 'force-face t))
	    (when (or now (find-face face))
	      (face-spec-set face spec))
	    (setq args (cdr args)))
	;; Old format, a plist of FACE SPEC pairs.
	(let ((face (nth 0 args))
	      (spec (nth 1 args)))
	  (put face 'saved-face spec))
	(setq args (cdr (cdr args)))))))

;;; The End.

(provide 'cus-face)

;; cus-face.el ends here
