;;; sysdep.el --- consolidate Emacs-version dependencies in one file.

;; Copyright (c) 1995 - 1997 Ben Wing.

;; Author: Ben Wing <wing@666.com>, William Perry <wmperry@cs.indiana.edu>
;; Keywords: lisp, tools
;; Version: 0.003

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs.  The idea is that we make it look like we're running
;; the latest version of XEmacs (currently 19.12) by emulating all the
;; missing functions.

;; #### This file does not currently do any advising but should.
;; Unfortunately, advice.el is a hugely big package.  Is any such
;; thing as `advice-lite' possible?

;; #### - This package is great, but its role needs to be thought out a bit
;; more.  Sysdep will not permit programs written for the old XEmacs API to
;; run on new versions of XEmacs.  Sysdep is a backward-compatibility
;; package for the latest and greatest XEmacs API.  It permits programmers
;; to use the latest XEmacs functionality and still have their programs run
;; on older versions of XEmacs...perhaps even on FSF Emacs.  It should NEVER
;; ever need to be loaded in the newest XEmacs.  It doesn't even make sense
;; to put it in the lisp/utils part of the XEmacs distribution because it's
;; real purpose is to be distributed with packages like w3 which take
;; advantage of the latest and greatest features of XEmacs but still need to
;; be run on older versions.  --Stig

;; Any packages that wish to use this file should load it using
;; `load-library'.  It will not load itself if a version of sysdep.el
;; that is at least as recent has already been loaded, but will
;; load over an older version of sysdep.el.  It will attempt to
;; not redefine functions that have already been custom-redefined,
;; but will redefine a function if the supplied definition came from
;; an older version of sysdep.el.

;; Packages such as w3 that wish to include this file with the package
;; should rename it to something unique, such as `w3-sysdep.el', and
;; load it with `load-library'.  That will ensure that no conflicts
;; arise if more than one package in the load path provides a version
;; of sysdep.el.  If multiple packages load sysdep.el, the most recent
;; version will end up loaded; as long as I'm careful not to
;; introduce bugs in previously working definitions, this should work
;; fine.

;; You may well discover deficiencies in this file as you use it.
;; The preferable way of dealing with this is to send me a patch
;; to sysdep.el; that way, the collective body of knowledge gets
;; increased.

;; IMPORTANT: leave the version string in the format X.XXX (e.g. 1.001)
;; so that string comparisons to other versions work properly.

(defconst sysdep-potential-version "0.003")

;; this macro means: define the function, but only if either it
;; wasn't bound before, or the supplied binding comes from an older
;; version of sysdep.el.  That way, user-supplied bindings don't
;; get overridden.

;; note: sysdep-defalias is often more useful than this function,
;; esp. since you can do load-time conditionalizing and can
;; optionally leave the function undefined. (e.g. frame functions
;; in v18.)

(defmacro sysdep-defun (function &rest everything-else)
  (` (cond ((and (not (fboundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this
		 sysdep-potential-version)
	    (defun (, function) (,@ everything-else))))))

(defmacro sysdep-defvar (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or 
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defvar (, function) (,@ everything-else))))))

(defmacro sysdep-defconst (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defconst (, function) (,@ everything-else))))))

;; similar for fset and defalias.  No need to quote as the argument
;; is already quoted.

(defmacro sysdep-fset (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def))
	    (put (, function) 'sysdep-defined-this t)
	    (fset (, function) (, def))))))

(defmacro sysdep-defalias (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def)
		 (or (listp (, def))
		     (and (symbolp (, def))
			  (fboundp (, def)))))
	    (put (, function) 'sysdep-defined-this t)
	    (defalias (, function) (, def))))))

;; bootstrapping: defalias and define-function don't exist
;; in older versions of lemacs

(sysdep-fset 'defalias 'fset)
(sysdep-defalias 'define-function 'defalias)

;; useful ways of determining what version is running
;; emacs-major-version and emacs-minor-version are
;; already defined in recent versions of FSF Emacs and XEmacs

(sysdep-defconst emacs-major-version
		 ;; will string-match ever fail?  If so, assume 19.0.
		 ;; (should we assume 18.something?)
		 (if (string-match "^[0-9]+" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 0) (match-end 0)))
		   19))

(sysdep-defconst emacs-minor-version
		 (if (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 1) (match-end 1)))
		   0))

(sysdep-defconst sysdep-running-xemacs
		 (or (string-match "Lucid" emacs-version)
		     (string-match "XEmacs" emacs-version)))

(sysdep-defconst window-system nil)
(sysdep-defconst window-system-version 0)

(sysdep-defvar list-buffers-directory nil)
(sysdep-defvar x-library-search-path (`
				      ("/usr/X11R6/lib/X11/"
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
				       "/usr/openwin/share/lib/X11/"
				       (, data-directory)
				       )
				      )
  "Search path used for X11 libraries.")

;; frame-related stuff.

(sysdep-defalias 'buffer-dedicated-frame 'buffer-dedicated-screen)
(sysdep-defalias 'deiconify-frame
  (cond ((fboundp 'deiconify-screen) 'deiconify-screen)
	;; make-frame-visible will be defined as necessary
	(t 'make-frame-visible)))
(sysdep-defalias 'delete-frame 'delete-screen)
(sysdep-defalias 'event-frame 'event-screen)
(sysdep-defalias 'event-glyph-extent 'event-glyph)
(sysdep-defalias 'find-file-other-frame 'find-file-other-screen)
(sysdep-defalias 'find-file-read-only-other-frame
  'find-file-read-only-other-screen)
(sysdep-defalias 'frame-height 'screen-height)
(sysdep-defalias 'frame-iconified-p 'screen-iconified-p)
(sysdep-defalias 'frame-left-margin-width 'screen-left-margin-width)
(sysdep-defalias 'frame-list 'screen-list)
(sysdep-defalias 'frame-live-p
  (cond ((fboundp 'screen-live-p) 'screen-live-p)
	((fboundp 'live-screen-p) 'live-screen-p)
	;; #### not sure if this is correct (this is for Epoch)
	;; but gnuserv.el uses it this way
	((fboundp 'screenp) 'screenp)))
(sysdep-defalias 'frame-name 'screen-name)
(sysdep-defalias 'frame-parameters 'screen-parameters)
(sysdep-defalias 'frame-pixel-height 'screen-pixel-height)
(sysdep-defalias 'frame-pixel-width 'screen-pixel-width)
(sysdep-defalias 'frame-right-margin-width 'screen-right-margin-width)
(sysdep-defalias 'frame-root-window 'screen-root-window)
(sysdep-defalias 'frame-selected-window 'screen-selected-window)
(sysdep-defalias 'frame-totally-visible-p 'screen-totally-visible-p)
(sysdep-defalias 'frame-visible-p 'screen-visible-p)
(sysdep-defalias 'frame-width 'screen-width)
(sysdep-defalias 'framep 'screenp)
(sysdep-defalias 'get-frame-for-buffer 'get-screen-for-buffer)
(sysdep-defalias 'get-frame-for-buffer-noselect 'get-screen-for-buffer-noselect)
(sysdep-defalias 'get-other-frame 'get-other-screen)
(sysdep-defalias 'iconify-frame 'iconify-screen)
(sysdep-defalias 'lower-frame 'lower-screen)
;(sysdep-defalias 'mail-other-frame 'mail-other-screen)

(sysdep-defalias 'make-frame
  (cond ((fboundp 'make-screen)
	 (function (lambda (&optional parameters device)
		     (make-screen parameters))))
	((fboundp 'x-create-screen)
	 (function (lambda (&optional parameters device)
		     (x-create-screen parameters))))))

(sysdep-defalias 'make-frame-invisible 'make-screen-invisible)
(sysdep-defalias 'make-frame-visible
  (cond ((fboundp 'make-screen-visible) 'make-screen-visible)
	((fboundp 'mapraised-screen) 'mapraised-screen)
	((fboundp 'x-remap-window)
	 (lambda (&optional x)
	   (x-remap-window)
	   (accept-process-output)))))
(sysdep-defalias 'modify-frame-parameters 'modify-screen-parameters)
(sysdep-defalias 'new-frame 'new-screen)
(sysdep-defalias 'next-frame 'next-screen)
(sysdep-defalias 'next-multiframe-window 'next-multiscreen-window)
(sysdep-defalias 'other-frame 'other-screen)
(sysdep-defalias 'previous-frame 'previous-screen)
(sysdep-defalias 'previous-multiframe-window 'previous-multiscreen-window)
(sysdep-defalias 'raise-frame
  (cond ((fboundp 'raise-screen) 'raise-screen)
	((fboundp 'mapraise-screen) 'mapraise-screen)))
(sysdep-defalias 'redraw-frame 'redraw-screen)
(sysdep-defalias 'select-frame 'select-screen)
(sysdep-defalias 'selected-frame 'selected-screen)
(sysdep-defalias 'set-buffer-dedicated-frame 'set-buffer-dedicated-screen)
(sysdep-defalias 'set-frame-height 'set-screen-height)
(sysdep-defalias 'set-frame-left-margin-width 'set-screen-left-margin-width)
(sysdep-defalias 'set-frame-position 'set-screen-position)
(sysdep-defalias 'set-frame-right-margin-width 'set-screen-right-margin-width)
(sysdep-defalias 'set-frame-size 'set-screen-size)
(sysdep-defalias 'set-frame-width 'set-screen-width)
(sysdep-defalias 'show-temp-buffer-in-current-frame 'show-temp-buffer-in-current-screen)
(sysdep-defalias 'switch-to-buffer-other-frame 'switch-to-buffer-other-screen)
(sysdep-defalias 'visible-frame-list 'visible-screen-list)
(sysdep-defalias 'window-frame 'window-screen)
(sysdep-defalias 'x-create-frame 'x-create-screen)
(sysdep-defalias 'x-set-frame-icon-pixmap 'x-set-screen-icon-pixmap)
(sysdep-defalias 'x-set-frame-pointer 'x-set-screen-pointer)
(sysdep-defalias 'x-display-color-p 'x-color-display-p)
(sysdep-defalias 'x-display-grayscale-p 'x-grayscale-display-p)
(sysdep-defalias 'menu-event-p 'misc-user-event-p)

;; WMP - commention these out so that Emacs 19 doesn't get screwed by them.
;; In particular, this makes the 'custom' package blow up quite well.
;;(sysdep-defun add-submenu (menu-path submenu &optional before)
;;  "Add a menu to the menubar or one of its submenus.
;;If the named menu exists already, it is changed.
;;MENU-PATH identifies the menu under which the new menu should be inserted.
;; It is a list of strings; for example, (\"File\") names the top-level \"File\"
;; menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
;; If MENU-PATH is nil, then the menu will be added to the menubar itself.
;;SUBMENU is the new menu to add.
;; See the documentation of `current-menubar' for the syntax.
;;BEFORE, if provided, is the name of a menu before which this menu should
;; be added, if this menu is not on its parent already.  If the menu is already
;; present, it will not be moved."
;;  (add-menu menu-path (car submenu) (cdr submenu) before))

;;(sysdep-defun add-menu-button (menu-path menu-leaf &optional before)
;;  "Add a menu item to some menu, creating the menu first if necessary.
;;If the named item exists already, it is changed.
;;MENU-PATH identifies the menu under which the new menu item should be inserted.
;; It is a list of strings; for example, (\"File\") names the top-level \"File\"
;; menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
;;MENU-LEAF is a menubar leaf node.  See the documentation of `current-menubar'.
;;BEFORE, if provided, is the name of a menu item before which this item should
;; be added, if this item is not on the menu already.  If the item is already
;; present, it will not be moved."
;; (add-menu-item menu-path (aref menu-leaf 0) (aref menu-leaf 1)
;;		(aref menu-leaf 2) before))

(sysdep-defun make-glyph (&optional spec-list)
  (if (and spec-list (cdr-safe (assq 'x spec-list)))
      (make-pixmap (cdr-safe (assq 'x spec-list)))))

(sysdep-defalias 'face-list 'list-faces)

(sysdep-defun set-keymap-parent (keymap new-parent)
  (let ((tail keymap))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

(sysdep-defun facep (face)
  "Return t if X is a face name or an internal face vector."
  ;; CAUTION!!! This is Emacs 19.x, for x <= 28, specific
  ;; I know of no version of Lucid Emacs or XEmacs that did not have
  ;; facep.  Even if they did, they are unsupported, so big deal.
  (if (not window-system)
      nil				; FIXME if FSF ever does TTY faces
    (and (or (internal-facep face)
	     (and (symbolp face) (assq face global-face-data)))
	 t)))

(sysdep-defun set-face-property (face property value &optional locale
				      tag-set how-to-add)
  "Change a property of FACE."
  (and (symbolp face)
       (put face property value)))

(sysdep-defun face-property (face property &optional locale tag-set exact-p)
  "Return FACE's value of the given PROPERTY."
  (and (symbolp face) (get face property)))

;;; Additional text property functions.

;; The following three text property functions are not generally available (and
;; it's not certain that they should be) so they are inlined for speed.
;; The case for `fillin-text-property' is simple; it may or not be generally
;; useful.  (Since it is used here, it is useful in at least one place.;-)
;; However, the case for `append-text-property' and `prepend-text-property' is
;; more complicated.  Should they remove duplicate property values or not?  If
;; so, should the first or last duplicate item remain?  Or the one that was
;; added?  In our implementation, the first duplicate remains.

(sysdep-defun fillin-text-property (start end setprop markprop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end markprop nil object)) next)
    (while start
      (setq next (next-single-property-change start markprop object end))
      (put-text-property start next setprop value object)
      (put-text-property start next markprop value object)
      (setq start (text-property-any next end markprop nil object)))))

;; This function (from simon's unique.el) is rewritten and inlined for speed.
;(defun unique (list function)
;  "Uniquify LIST, deleting elements using FUNCTION.
;Return the list with subsequent duplicate items removed by side effects.
;FUNCTION is called with an element of LIST and a list of elements from LIST,
;and should return the list of elements with occurrences of the element removed,
;i.e., a function such as `delete' or `delq'.
;This function will work even if LIST is unsorted.  See also `uniq'."
;  (let ((list list))
;    (while list
;      (setq list (setcdr list (funcall function (car list) (cdr list))))))
;  list)

(sysdep-defun unique (list)
  "Uniquify LIST, deleting elements using `delq'.
Return the list with subsequent duplicate items removed by side effects."
  (let ((list list))
    (while list
      (setq list (setcdr list (delq (car list) (cdr list))))))
  list)

;; A generalisation of `facemenu-add-face' for any property, but without the
;; removal of inactive faces via `facemenu-discard-redundant-faces' and special
;; treatment of `default'.  Uses `unique' to remove duplicate property values.
(sysdep-defun prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (unique (append val (if (listp prev) prev (list prev))))
       object)
      (setq start next))))

(sysdep-defun append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (unique (append (if (listp prev) prev (list prev)) val))
       object)
      (setq start next))))

;; Property list functions
;;
(sysdep-defun plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects."
  (let ((node (memq prop plist)))
    (if node
	(setcar (cdr node) val)
      (setq plist (cons prop (cons val plist))))
    plist))

(sysdep-defun plist-get (plist prop)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
  (while (and plist (not (eq (car plist) prop)))
    (setq plist (cdr (cdr plist))))
  (and plist (car (cdr plist))))

;; Device functions
;; By wmperry@cs.indiana.edu
;; This is a complete implementation of all the device-* functions found in
;; XEmacs 19.14.  A 'device' for Emacs 19 is just a frame, from which we can
;; determine the connection to an X display, etc.

(sysdep-defalias 'selected-device 'ignore)
(sysdep-defalias 'device-or-frame-p 'framep)
(sysdep-defalias 'device-console 'ignore)
(sysdep-defalias 'device-sound-enabled-p 'ignore)
(sysdep-defalias 'device-live-p 'frame-live-p)
(sysdep-defalias 'devicep 'framep)
(sysdep-defalias 'frame-device 'identity)
(sysdep-defalias 'redisplay-device 'redraw-frame)
(sysdep-defalias 'redraw-device 'redraw-frame)
(sysdep-defalias 'select-device 'select-frame)
(sysdep-defalias 'set-device-class 'ignore)

(sysdep-defun make-device (type connection &optional props)
  "Create a new device of type TYPE, attached to connection CONNECTION.

The valid values for CONNECTION are device-specific; however,
CONNECTION is generally a string. (Specifically, for X devices,
CONNECTION should be a display specification such as \"foo:0\", and
for TTY devices, CONNECTION should be the filename of a TTY device
file, such as \"/dev/ttyp4\", or nil to refer to XEmacs' standard
input/output.)

PROPS, if specified, should be a plist of properties controlling
device creation.

If CONNECTION specifies an already-existing device connection, that
device is simply returned; no new device is created, and PROPS
have no effect."
  (cond
   ((and (eq type 'x) connection)
    (make-frame-on-display connection props))
   ((eq type 'x)
    (make-frame props))
   ((eq type 'tty)
    nil)
   (t
    (error "Unsupported device-type: %s" type))))

(sysdep-defun make-frame-on-device (type connection &optional props)
  "Create a frame of type TYPE on CONNECTION.
TYPE should be a symbol naming the device type, i.e. one of

x	An X display.  CONNECTION should be a standard display string
	such as \"unix:0\", or nil for the display specified on the
	command line or in the DISPLAY environment variable.  Only if
	support for X was compiled into	XEmacs.
tty	A standard TTY connection or terminal.  CONNECTION should be
	a TTY device name such as \"/dev/ttyp2\" (as determined by
	the Unix command `tty') or nil for XEmacs' standard input
	and output (usually the TTY in which XEmacs started).  Only
	if support for TTY's was compiled into XEmacs.
ns	A connection to a machine running the NeXTstep windowing
	system.  Not currently implemented.
win32	A connection to a machine running Microsoft Windows NT or
	Windows 95.  Not currently implemented.
pc	A direct-write MS-DOS frame.  Not currently implemented.

PROPS should be an plist of properties, as in the call to `make-frame'.

If a connection to CONNECTION already exists, it is reused; otherwise,
a new connection is opened."
  (make-device type connection props))

(sysdep-defun make-tty-device (&optional tty terminal-type)
  "Create a new device on TTY.
  TTY should be the name of a tty device file (e.g. \"/dev/ttyp3\" under
SunOS et al.), as returned by the `tty' command.  A value of nil means
use the stdin and stdout as passed to XEmacs from the shell.
  If TERMINAL-TYPE is non-nil, it should be a string specifying the
type of the terminal attached to the specified tty.  If it is nil,
the terminal type will be inferred from the TERM environment variable."
  (make-device 'tty tty (list 'terminal-type terminal-type)))

(sysdep-defun make-x-device (&optional display)
  (make-device 'x display))

(sysdep-defun set-device-selected-frame (device frame)
  "Set the selected frame of device object DEVICE to FRAME.
If DEVICE is nil, the selected device is used.
If DEVICE is the selected device, this makes FRAME the selected frame."
  (select-frame frame))

(sysdep-defun set-device-baud-rate (device rate)
  "Set the output baud rate of DEVICE to RATE.
On most systems, changing this value will affect the amount of padding
and other strategic decisions made during redisplay."
  (setq baud-rate rate))

(sysdep-defun dfw-device (obj)
  "Given a device, frame, or window, return the associated device.
Return nil otherwise."
  (cond
   ((windowp obj)
    (window-frame obj))
   ((framep obj)
    obj)
   (t
    nil)))

(sysdep-defun event-device (event)
  "Return the device that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
  (dfw-device (posn-window (event-start event))))

(sysdep-defun find-device (connection &optional type)
  "Look for an existing device attached to connection CONNECTION.
Return the device if found; otherwise, return nil.

If TYPE is specified, only return devices of that type; otherwise,
return devices of any type. (It is possible, although unlikely,
that two devices of different types could have the same connection
name; in such a case, the first device found is returned.)"
  (let ((devices (device-list))
	(retval nil))
    (while (and devices (not nil))
      (if (equal connection (device-connection (car devices)))
	  (setq retval (car devices)))
      (setq devices (cdr devices)))
    retval))

(sysdep-defalias 'get-device 'find-device)

(sysdep-defun device-baud-rate (&optional device)
  "Return the output baud rate of DEVICE."
  baud-rate)

(sysdep-defun device-on-window-system-p (&optional device)
  "Return non-nil if DEVICE is on a window system.
This generally means that there is support for the mouse, the menubar,
the toolbar, glyphs, etc."
  (and (cdr-safe (assq 'display (frame-parameters device))) t))

(sysdep-defun device-name (&optional device)
  "Return the name of the specified device."
  ;; doesn't handle the 19.29 multiple X display stuff yet
  ;; doesn't handle NeXTStep either
  (cond
   ((null window-system) "stdio")
   ((getenv "DISPLAY")
    (let ((str (getenv "DISPLAY"))
	  (x (1- (length (getenv "DISPLAY"))))
	  (y 0))
      (while (/= y x)
	(if (or (= (aref str y) ?:)
		(= (aref str y) ?.))
	    (aset str y ?-))
	(setq y (1+ y)))
      str))
   (t "stdio")))

(sysdep-defun device-connection (&optional device)
  "Return the connection of the specified device.
DEVICE defaults to the selected device if omitted"
  (or (cdr-safe (assq 'display (frame-parameters device))) "stdio"))

(sysdep-defun device-frame-list (&optional device)
  "Return a list of all frames on DEVICE.
If DEVICE is nil, the selected device will be used."
  (let ((desired (device-connection device)))
    (filtered-frame-list (function (lambda (x) (equal (device-connection x)
						      desired))))))
(sysdep-defun device-list ()
  "Return a list of all devices"
  (let ((seen nil)
	(cur nil)
	(conn nil)
	(retval nil)
	(not-heard (frame-list)))
    (while not-heard
      (setq cur (car not-heard)
	    conn (device-connection cur)
	    not-heard (cdr not-heard))
      (if (member conn seen)
	  nil				; Already got it
	(setq seen (cons conn seen)	; Whoo hoo, a new one!
	      retval (cons cur retval))))
    retval))

(sysdep-defvar delete-device-hook nil
  "Function or functions to call when a device is deleted.
One argument, the to-be-deleted device.")

(sysdep-defun delete-device (device &optional force)
  "Delete DEVICE, permanently eliminating it from use.
Normally, you cannot delete the last non-minibuffer-only frame (you must
use `save-buffers-kill-emacs' or `kill-emacs').  However, if optional
second argument FORCE is non-nil, you can delete the last frame. (This
will automatically call `save-buffers-kill-emacs'.)"
  (let ((frames (device-frame-list device)))
    (run-hook-with-args 'delete-device-hook device)
    (while frames
      (delete-frame (car frames) force)
      (setq frames (cdr frames)))))

(sysdep-defalias 'device-color-cells
  (cond
   ((null window-system) 'ignore)
   ((fboundp 'display-color-cells) 'display-color-cells)
   ((fboundp 'x-display-color-cells) 'x-display-color-cells)
   ((fboundp 'ns-display-color-cells) 'ns-display-color-celles)
   (t 'ignore)))

(sysdep-defun try-font-name (fontname &rest args)
  (cond
   ((eq window-system 'x) (car-safe (x-list-fonts fontname)))
   ((eq window-system 'ns) (car-safe (ns-list-fonts fontname)))
   (t nil)))

(sysdep-defalias 'device-pixel-width
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-pixel-width))
    'x-display-pixel-width)
   ((and (eq window-system 'ns) (fboundp 'ns-display-pixel-width))
    'ns-display-pixel-width)
   (t 'ignore)))

(sysdep-defalias 'device-pixel-height
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-pixel-height))
    'x-display-pixel-height)
   ((and (eq window-system 'ns) (fboundp 'ns-display-pixel-height))
    'ns-display-pixel-height)
   (t 'ignore)))

(sysdep-defalias 'device-mm-width
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-mm-width))
    'x-display-mm-width)
   ((and (eq window-system 'ns) (fboundp 'ns-display-mm-width))
    'ns-display-mm-width)
   (t 'ignore)))

(sysdep-defalias 'device-mm-height
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-mm-height))
    'x-display-mm-height)
   ((and (eq window-system 'ns) (fboundp 'ns-display-mm-height))
    'ns-display-mm-height)
   (t 'ignore)))

(sysdep-defalias 'device-bitplanes
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-planes))
    'x-display-planes)
   ((and (eq window-system 'ns) (fboundp 'ns-display-planes))
    'ns-display-planes)
   (t 'ignore)))

(sysdep-defalias 'device-class
  (cond
   ;; First, Xwindows
   ((and (eq window-system 'x) (fboundp 'x-display-visual-class))
    (function
     (lambda (&optional device)
       (let ((val (symbol-name (x-display-visual-class device))))
	 (cond
	  ((string-match "color" val) 'color)
	  ((string-match "gray-scale" val) 'grayscale)
	  (t 'mono))))))
   ;; Now, Presentation-Manager under OS/2
   ((and (eq window-system 'pm) (fboundp 'pm-display-visual-class))
    (function
     (lambda (&optional device)
       (let ((val (symbol-name (pm-display-visual-class device))))
	 (cond
	  ((string-match "color" val) 'color)
	  ((string-match "gray-scale" val) 'grayscale)
	  (t 'mono))))))
   ;; A slightly different way of doing it under OS/2
   ((and (eq window-system 'pm) (fboundp 'pm-display-color-p))
    (function
     (lambda (&optional device)
       (if (pm-display-color-p)
	   'color
	 'mono))))
   ((fboundp 'number-of-colors)
    (function
     (lambda (&optional device)
       (if (= 2 (number-of-colors))
	   'mono
	 'color))))
   ((and (eq window-system 'x) (fboundp 'x-color-p))
    (function
     (lambda (&optional device)
       (if (x-color-p)
	   'color
	 'mono))))
   ((and (eq window-system 'ns) (fboundp 'ns-display-visual-class))
    (function
     (lambda (&optional device)
       (let ((val (symbol-name (ns-display-visual-class))))
	 (cond
	  ((string-match "color" val) 'color)
	  ((string-match "gray-scale" val) 'grayscale)
	  (t 'mono))))))
   (t (function (lambda (&optional device) 'mono)))))

(sysdep-defun device-class-list ()
  "Returns a list of valid device classes."
  (list 'color 'grayscale 'mono))

(sysdep-defun valid-device-class-p (class)
  "Given a CLASS, return t if it is valid.
Valid classes are 'color, 'grayscale, and 'mono."
  (memq class (device-class-list)))

(sysdep-defun device-or-frame-type (device-or-frame)
  "Return the type (e.g. `x' or `tty') of DEVICE-OR-FRAME.
DEVICE-OR-FRAME should be a device or a frame object.  See `device-type'
for a description of the possible types."
  (if (or (cdr-safe (assq 'display (frame-parameters device-or-frame)))
	  (cdr-safe (assq 'window-id (frame-parameters device-or-frame))))
      window-system
    'tty))

(sysdep-defun device-type (&optional device)
  "Return the type of the specified device (e.g. `x' or `tty').
Value is `tty' for a tty device (a character-only terminal),
`x' for a device which is a connection to an X server,
'ns' for a device which is a connection to a NeXTStep dps server,
'win32' for a Windows-NT window,
'pm' for an OS/2 Presentation Manager window,
'intuition' for an Amiga screen"
  (device-or-frame-type device))

(sysdep-defun device-type-list ()
  "Return a list of valid console types."
  (if window-system
      (list window-system 'tty)
    (list 'tty)))

(sysdep-defun valid-device-type-p (type)
  "Given a TYPE, return t if it is valid."
  (memq type (device-type-list)))


;; Extent stuff
(sysdep-fset 'delete-extent 'delete-overlay)
(sysdep-fset 'extent-end-position 'overlay-end)
(sysdep-fset 'extent-start-position 'overlay-start)
(sysdep-fset 'set-extent-endpoints 'move-overlay)
(sysdep-fset 'set-extent-property 'overlay-put)
(sysdep-fset 'make-extent 'make-overlay)

(sysdep-defun extent-property (extent property &optional default)
  (or (overlay-get extent property) default))

(sysdep-defun extent-at (pos &optional object property before at-flag)
  (let ((tmp (overlays-at (point)))
	ovls)
    (if property
	(while tmp
	  (if (extent-property (car tmp) property)
	      (setq ovls (cons (car tmp) ovls)))
	  (setq tmp (cdr tmp)))
      (setq ovls tmp
	    tmp nil))
    (car-safe
     (sort ovls
	   (function
	    (lambda (a b)
	      (< (- (extent-end-position a) (extent-start-position a))
		 (- (extent-end-position b) (extent-start-position b)))))))))

(sysdep-defun overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG
or between BEG and END."
  (let ((ovls (overlay-lists))
	tmp retval)
    (if (< end beg)
	(setq tmp end
	      end beg
	      beg tmp))
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq tmp (car ovls)
	    ovls (cdr ovls))
      (if (or (and (<= (overlay-start tmp) end)
		   (>= (overlay-start tmp) beg))
	      (and (<= (overlay-end tmp) end)
		   (>= (overlay-end tmp) beg)))
	  (setq retval (cons tmp retval))))
    retval))

(sysdep-defun map-extents (function &optional object from to
				    maparg flags property value)
  (let ((tmp (overlays-in (or from (point-min))
			  (or to (point-max))))
	ovls)
    (if property
	(while tmp
	  (if (extent-property (car tmp) property)
	      (setq ovls (cons (car tmp) ovls)))
	  (setq tmp (cdr tmp)))
      (setq ovls tmp
	    tmp nil))
    (catch 'done
      (while ovls
	(setq tmp (funcall function (car ovls) maparg)
	      ovls (cdr ovls))
	(if tmp
	    (throw 'done tmp))))))

;; misc
(sysdep-fset 'make-local-hook 'make-local-variable)

(sysdep-defun buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without text properties, as a string."
  (format "%s" (buffer-substring beg end)))
  
(sysdep-defun symbol-value-in-buffer (symbol buffer &optional unbound-value)
  "Return the value of SYMBOL in BUFFER, or UNBOUND-VALUE if it is unbound."
  (save-excursion
    (set-buffer buffer)
    (if (not (boundp symbol))
	unbound-value
      (symbol-value symbol))))

(sysdep-defun insert-file-contents-literally
  (file &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((file-name-handler-alist nil)
	(find-file-hooks nil))
    (insert-file-contents file visit beg end replace)))

(sysdep-defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(sysdep-defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.
TOGGLE is a symbol which is used as the variable which toggle the minor mode,
NAME is the name that should appear in the modeline (it should be a string
beginning with a space), KEYMAP is a keymap to make active when the minor
mode is active, and AFTER is the toggling symbol used for another minor
mode.  If AFTER is non-nil, then it is used to position the new mode in the
minor-mode alists.  TOGGLE-FUN specifies an interactive function that
is called to toggle the mode on and off; this affects what appens when
button2 is pressed on the mode, and when button3 is pressed somewhere
in the list of modes.  If TOGGLE-FUN is nil and TOGGLE names an
interactive function, TOGGLE is used as the toggle function.

Example:  (add-minor-mode 'view-minor-mode \" View\" view-mode-map)"
  (if (not (assq toggle minor-mode-alist))
      (setq minor-mode-alist (cons (list toggle name) minor-mode-alist)))
  (if (and keymap (not (assq toggle minor-mode-map-alist)))
      (setq minor-mode-map-alist (cons (cons toggle keymap)
				       minor-mode-map-alist))))

(sysdep-defvar x-font-regexp-foundry-and-family
  (let ((- 		"[-?]")
	(foundry		"[^-]+")
	(family 		"[^-]+")
	)
    (concat "\\`[-?*]" foundry - "\\(" family "\\)" -)))

(sysdep-defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(sysdep-defun add-hook (hook-var function &optional at-end)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
      (if (not (boundp hook-var)) (set hook-var nil))
      (let ((old (symbol-value hook-var)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (setq old (list old)))
	(if (member function old)
	    nil
	  (set hook-var
	       (if at-end
		   (append old (list function)) ; don't nconc
		 (cons function old))))))

(sysdep-defalias 'valid-color-name-p
  (cond
   ((fboundp 'x-valid-color-name-p)	; XEmacs/Lucid
    'x-valid-color-name-p)
   ((and window-system
	 (fboundp 'color-defined-p))	; NS/Emacs 19
    'color-defined-p)
   ((and window-system
	 (fboundp 'pm-color-defined-p))
    'pm-color-defined-p)
   ((and window-system
	 (fboundp 'x-color-defined-p))	; Emacs 19
    'x-color-defined-p)
   ((fboundp 'get-color)		; Epoch
    (function (lambda (color)
		(let ((x (get-color color)))
		  (if x
		      (setq x (progn
				(free-color x)
				t)))
		  x))))
   (t 'identity)))			; All others

;; Misc.
(sysdep-defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))

(sysdep-defun member (elt list)
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(sysdep-defun rassoc (key list)
  (let ((found nil))
    (while (and list (not found))
      (if (equal (cdr (car list)) key) (setq found (car list)))
      (setq list (cdr list)))
    found))

(sysdep-defun display-error (error-object stream)
  "Display `error-object' on `stream' in a user-friendly way."
  (funcall (or (let ((type (car-safe error-object)))
		 (catch 'error
		   (and (consp error-object)
			(symbolp type)
			;;(stringp (get type 'error-message))
			(consp (get type 'error-conditions))
			(let ((tail (cdr error-object)))
			  (while (not (null tail))
			    (if (consp tail)
				(setq tail (cdr tail))
			      (throw 'error nil)))
			  t)
			;; (check-type condition condition)
			(get type 'error-conditions)
			;; Search class hierarchy
			(let ((tail (get type 'error-conditions)))
			  (while (not (null tail))
			    (cond ((not (and (consp tail)
					     (symbolp (car tail))))
				   (throw 'error nil))
				  ((get (car tail) 'display-error)
				   (throw 'error (get (car tail)
						      'display-error)))
				  (t
				   (setq tail (cdr tail)))))
			  ;; Default method
			  (function
			   (lambda (error-object stream)
			     (let ((type (car error-object))
				   (tail (cdr error-object))
				   (first t))
			       (if (eq type 'error)
				   (progn (princ (car tail) stream)
					  (setq tail (cdr tail)))
				 (princ (or (get type 'error-message) type)
					stream))
			       (while tail
				 (princ (if first ": " ", ") stream)
				 (prin1 (car tail) stream)
				 (setq tail (cdr tail)
				       first nil)))))))))
	       (function
		(lambda (error-object stream)
		  (princ "Peculiar error " stream)
		  (prin1 error-object stream))))
	   error-object stream))

(sysdep-defun decode-time (&optional specified-time)
  (let* ((date (current-time-string specified-time))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and dateinfo (timezone-parse-time (aref dateinfo 3)))))
    (list (aref timeinfo 2) (aref timeinfo 1)
	  (aref timeinfo 0) (aref dateinfo 2)
	  (aref dateinfo 1) (aref dateinfo 0)
	  "unknown" nil 0)))

(sysdep-defun find-face (face)
  (car-safe (memq face (face-list))))

(sysdep-defun set-marker-insertion-type (marker type)
  "Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it."
  nil)

;; window functions

;; not defined in v18
(sysdep-defun eval-buffer (bufname &optional printflag)
  (interactive)
  (save-excursion
    (set-buffer bufname)
    (eval-current-buffer)))

(sysdep-defun window-minibuffer-p (window)
  "Returns non-nil if WINDOW is a minibuffer window."
  (eq window (minibuffer-window)))

(sysdep-defun window-live-p (window)
  "Returns t if OBJ is a window which is currently visible."
  (and (windowp window)
       (window-point window)))

(provide 'w3-sysdp)
;;; sysdep.el ends here

;;;(sysdep.el) Local Variables:
;;;(sysdep.el) eval: (put 'sysdep-defun 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defalias 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defconst 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defvar 'lisp-indent-function 'defun)
;;;(sysdep.el) End:
