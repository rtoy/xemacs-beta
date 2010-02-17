;;; compat.el --- Mechanism for non-intrusively providing compatibility funs.

;; Copyright (C) 2000, 2002 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: Ben Wing
;; Keywords: internal

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

;;; Synched up with: Not in FSF.

;;; Authorship:

; Written May 2000 by Ben Wing.

;;; Commentary:

;; The idea is to provide emulation of API's in a namespace-clean way.  Lots of packages are filled with declarations such as

;; (defalias 'gnus-overlay-get 'extent-property)

; There should be a single package to provide such compatibility code.  The
; tricky part is how to do it in a clean way, without packages interfering
; with each other.

; The basic usage of compat is:

; (1) Each package copies compat.el and renames it, e.g. gnus-compat.el.

; (2) `compat' defines various API's that can be activated.  To use them in a
;     file, first place code like this at the top of the file:

;(let ((compat-current-package 'Gnus))
;  (require 'gnus-compat))

; then wrap the rest of the code like this:

; (Gnus-compat-wrap '(overlays events)

;;; Commentary

;; blah

;;; Code

;(defun random-module-my-fun (bar baz)
;  ...
;  (overlay-put overlay 'face 'bold)
;  ...
;)
;
;(defun ...
;)
;
;
;
;
;) ;; end of (Gnus-compat)

;;;; random-module.el ends here

; (3) What this does is implement the requested API's (in this case, the
;     overlay API from GNU Emacs and event API from XEmacs) in whichever
;     version of Emacs is running, with names such as
;     `Gnus-compat-overlay-put', and then it uses `macrolet' to map the
;     generic names in the wrapped code into namespace-clean names.  The
;     result of loading `gnus-compat' leaves around only functions beginning
;     with `Gnus-compat' (or whatever prefix was specified in
;     `compat-current-package').  This way, various packages, with various
;     versions of `compat' as part of them, can coexist, with each package
;     running the version of `compat' that it's been tested with.  The use of
;     `macrolet' ensures that only code that's lexically wrapped -- not code
;     that's called from that code -- is affected by the API mapping.

;; Typical usage:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Wrap modules that define compatibility functions like this:     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(compat-define-group 'fsf-compat)

;(compat-define-functions 'fsf-compat

;(defun overlay-put (overlay prop value)
;  "Set property PROP to VALUE in overlay OVERLAY."
;  (set-extent-property overlay prop value))

;(defun make-overlay (beg end &optional buffer front-advance rear-advance)
;  ...)

;...

;) ;; end of (compat-define-group 'fsf-compat)

;;;; overlay.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Wrap modules that use the compatibility functions like this:    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(let ((compat-current-package 'gnus))
;  (require 'gnus-compat))
;
;(gnus-compat 'fsf-compat
;
;; Code:
;;
;;
;(defun random-module-my-fun (bar baz)
;  ...
;  (overlay-put overlay 'face 'bold)
;  ...
;)
;
;(defun ...
;)
;
;
;
;
;) ;; end of (compat 'fsf-compat)

;;;; random-module.el ends here

(defvar compat-current-package)

(eval-when-compile
  (setq compat-current-package 'compat))

;; #### not yet working
'(

(defmacro compat-define-compat-functions (&rest body)
  "Define the functions of the `compat' package in a namespace-clean way.
This relies on `compat-current-package' being set.  If `compat-current-package'
is equal to the symbol `foo', and within BODY is something like

\(defmacro compat-define-group (group)
  ...
)

then this turns into

\(defmacro foo-compat-define-group (group)
  ...
)

and all calls are replaced accordingly.




Functions such as
compatibility functions in GROUP.
You should simply wrap this around the code that defines the functions.
Any functions and macros defined at top level using `defun' or `defmacro'
will be noticed and added to GROUP.  Other top-level code will be executed
normally.  All code and definitions in this group can safely reference any
other functions in this group -- the code is effectively wrapped in a
`compat' call.  You can call `compat-define-functions' more than once, if
necessary, for a single group.

What actually happens is that the functions and macros defined here are in
fact defined using names prefixed with GROUP.  To use these functions,
wrap any calling code with the `compat' macro, which lexically renames
the function and macro calls appropriately."
  (let ((prefix (if (boundp 'compat-current-package)
		    compat-current-package
		  (error
		   "`compat-current-package' must be defined when loading this module")))
	(defs-to-munge '(defun defmacro))
	mappings)
    (if (symbolp prefix) (setq prefix (symbol-name prefix)))
    ;; first, note all defuns and defmacros
    (let (fundef
	  (body-tail body))
      (while body-tail
	(setq fundef (car body-tail))
	(when (and (consp fundef) (memq (car fundef) defs-to-munge))
	  (push (cons (second fundef) (third fundef)) mappings))
	(setq body-tail (cdr body-tail))))
    ;; now, munge the definitions with the new names
    (let (fundef
	  (body-tail body)
	  result
	  defs)
      (while body-tail
	(setq fundef (car body-tail))
	(push
	 (cond ((and (consp fundef) (memq (car fundef) defs-to-munge))
		(nconc (list (car fundef)
			     (intern (concat prefix "-"
					     (symbol-name (second fundef))))
			     (third fundef))
		       (nthcdr 3 fundef)))
	       (t fundef))
	 result)
	(setq body-tail (cdr body-tail)))
      (setq result (nreverse result))
      ;; now, generate the munged code, with the references to the functions
      ;; macroletted
      (mapc
       #'(lambda (acons)
	   (let ((fun (car acons))
		 (args (cdr acons)))
	     (push
	      (list fun args
		    (nconc
		     (list 'list
			   (list 'quote 
				 (intern (concat prefix "-"
						 (symbol-name fun)))))
		     args))
	      defs)))
       mappings)
      ;; it would be cleaner to use `lexical-let' instead of `let', but that
      ;; causes function definitions to have obnoxious, unreadable junk in
      ;; them.  #### Move `lexical-let' into C!!!
      `(macrolet ((compat-current-package () ,compat-current-package)
		  ,@defs)
	 ,@result))))

(compat-define-compat-functions

(defun compat-hash-table (group)
  (get group 'compat-table))

(defun compat-make-hash-table (group)
  (put group 'compat-table (make-hash-table)))

(defmacro compat-define-group (group &rest body)
  "Define GROUP as a group of compatibility functions.
This macro should wrap individual Individual functions are defined using `compat-define-functions'.
Once defined, the functions can be used by wrapping your code in the
`compat' macro.

If GROUP is already defined, nothing happens."
  (let ((group (eval group)))
    (or (hash-table-p (compat-hash-table group))
	(compat-make-hash-table group))))

(defmacro compat-clear-functions (group)
  "Clear all defined functions and macros out of GROUP."
  (let ((group (eval group)))
    (clrhash (compat-hash-table group))))

(defmacro compat-defun (args &rest body)

(defmacro compat-define-function (props name arglist &rest body)
  "Define a compatibility function.
PROPS are properties controlling how the function should be defined.
control how the  should simply wrap this around the code that defines the functions.
Any functions and macros defined at top level using `defun' or `defmacro'
will be noticed and added to GROUP.  Other top-level code will be executed
normally.  All code and definitions in this group can safely reference any
other functions in this group -- the code is effectively wrapped in a
`compat' call.  You can call `compat-define-functions' more than once, if
necessary, for a single group.

What actually happens is that the functions and macros defined here are in
fact defined using names prefixed with GROUP.  To use these functions,
wrap any calling code with the `compat' macro, which lexically renames
the function and macro calls appropriately."
  (let ((group (eval group))
	(defs-to-munge '(defun defmacro))
	)
    (let (fundef
	  (body-tail body))
      (while body-tail
	(setq fundef (car body-tail))
	(when (and (consp fundef) (memq (car fundef) defs-to-munge))
	  (puthash (second fundef) (third fundef) (compat-hash-table group)))
	(setq body-tail (cdr body-tail))))
    (let (fundef
	  (body-tail body)
	  result)
      (while body-tail
	(setq fundef (car body-tail))
	(push
	 (cond ((and (consp fundef) (memq (car fundef) defs-to-munge))
		(nconc (list (car fundef)
			      (intern (concat (symbol-name group) "-"
					      (symbol-name (second fundef))))
			      (third fundef))
			(nthcdr 3 fundef)))
	       (t fundef))
	 result)
	(setq body-tail (cdr body-tail)))
      (nconc (list 'compat-wrap (list 'quote group)) (nreverse result)))))

(defvar compat-active-groups nil)

(defun compat-fboundp (groups fun)
  "T if FUN is either `fboundp' or one of the compatibility funs in GROUPS.
GROUPS is a list of compatibility groups as defined using
`compat-define-group'."
  (or (fboundp fun)
      (block nil
	(mapcar #'(lambda (group)
		    (if (gethash fun (compat-hash-table group))
			(return t)))
		groups))))

(defmacro compat-wrap-runtime (groups &rest body))

(defmacro compat-wrap (groups &rest body)
  "Make use of compatibility functions and macros in GROUPS.
GROUPS is a symbol, an API group, or list of API groups.  Each API group
defines a set of functions, macros, variables, etc. and that will (or
should ideally) work on all recent versions of both GNU Emacs and XEmacs,
and (to some extent, depending on how the functions were designed) on older
version.  When this function is used, it will generally not be named
`compat-wrap', but have some name such as `Gnus-compat-wrap', if this is
wrapping something in `gnus'. (The renaming happened when the `compat'
package was loaded -- see discussion at top).

To use `compat' in your package (assume your package is `gnus'), you first
have to do a bit if setup.

-- Copy and rename compat.el, e.g. to `gnus-compat.el'.  The name must be
   globally unique across everything on the load path (that means all
   packages).
-- Incude this file in your package.  It will not interfere with any other
   versions of compat (earlier, later, etc.) provided in other packages
   and similarly renamed.

To make use of the API's provided:

-- First place code like this at the top of the file, after the copyright
   notices and comments:

\(let ((compat-current-package 'Gnus))
  (require 'gnus-compat))

-- then wrap the rest of the code like this, assuming you want access to
   the GNU Emacs overlays API, and the XEmacs events API:

\(Gnus-compat-wrap '(overlays xem-events)

...
...
...

\(defun gnus-random-fun (overlay baz)
  ...
  (overlay-put overlay 'face 'bold)
  ...
)

...
...

\(defun gnus-random-fun-2 (event)
  (interactive "e")
  (let ((x (event-x event))
	(y (event-y event)))
    ...
    )
  )

) ;; end of (Gnus-compat)

;;;; random-module.el ends here

Both the requested API's will be implemented whichever version of Emacs
\(GNU Emacs, XEmacs, etc.) is running, and (with limitations) on older
versions as well.  Furthermore, the API's are provided *ONLY* to code
that's actually, lexically wrapped by `compat-wrap' (or its renamed
version).  All other code, including code that's called by the wrapped
code, is not affected -- e.g. if we're on XEmacs, and `overlay-put' isn't
normally defined, then it won't be defined in code other than the wrapped
code, even if the wrapped code calls that code.  Clever, huh?

What happens is that the `compat-wrap' actually uses `macrolet' to
inline-substitute calls to `overlay-put' to (in this case)
`Gnus-compat-overlay-put', which was defined when `gnus-compat' was loaded.

What happens is that is implement the requested API's (in this case, the
overlay API from GNU Emacs and event API from XEmacs) in whichever
version of Emacs is running, with names such as
`Gnus-compat-overlay-put', and then it uses `macrolet' to map the
generic names in the wrapped code into namespace-clean names.  The
result of loading `gnus-compat' leaves around only functions beginning
with `Gnus-compat' (or whatever prefix was specified in
`compat-current-package').  This way, various packages, with various
versions of `compat' as part of them, can coexist, with each package
running the version of `compat' that it's been tested with.  The use of
`macrolet' ensures that only code that's lexically wrapped -- not code
that's called from that code -- is affected by the API mapping.

Before using `compat' 

For any file where you want to make use of one or more API's provided by
`compat', first do this:

Wrap a call to `compat-wrap' around your entire file, like this:

;; First, you copied compat.el into your package -- we're assuming \"gnus\" --
;; and renamed it, e.g. gnus-compat.el.  Now we load it and tell it to
;; use `Gnus' as the prefix for all stuff it defines. (Use a capital letter
;; or some similar convention so that these names are not so easy to see.)

\(let ((current-compat-package 'Gnus))
  (require 'gnus-compat))

;; The function `compat-wrap' was mapped to `Gnus-compat-wrap'.  The idea
;; is that the raw functions beginning with `compat-' are never actually
;; defined.  They may appear as function calls inside of functions, but
;; they will always be mapped to something beginning with the given prefix.

\(Gnus-compat-wrap '(overlays xem-events)

 ...

)

You should simply wrap this around the code that uses the functions
and macros in GROUPS.  Typically, a call to `compat' should be placed
at the top of an ELisp module, with the closing parenthesis at the
bottom; use this in place of a `require' statement.  Wrapped code can
be either function or macro definitions or other ELisp code, and
wrapped function or macro definitions need not be at top level.  All
calls to the compatibility functions or macros will be noticed anywhere
within the wrapped code.  Calls to `fboundp' within the wrapped code
will also behave correctly when called on compatibility functions and
macros, even though they would return nil elsewhere (including in code
in other modules called dynamically from the wrapped code).

The functions and macros define in GROUP are actually defined under
prefixed names, to avoid namespace clashes and bad interactions with
other code that calls `fboundp'.  All calls inside of the wrapped code
to the compatibility functions and macros in GROUP are lexically
mapped to the prefixed names.  Since this is a lexical mapping, code
in other modules that is called by functions in this module will not
be affected."
  (let ((group (eval group))
	defs)
    (maphash
     #'(lambda (fun args)
	 (push
	  (list fun args
		(nconc
		 (list 'list
		       (list 'quote 
			     (intern (concat (symbol-name group) "-"
					     (symbol-name fun)))))
		 args))
	  defs))
     (compat-hash-table group))
    ;; it would be cleaner to use `lexical-let' instead of `let', but that
    ;; causes function definitions to have obnoxious, unreadable junk in
    ;; them.  #### Move `lexical-let' into C!!!
    `(let ((compat-active-groups (cons ',group compat-active-groups)))
       (macrolet ((fboundp (fun) `(compat-fboundp ',compat-active-groups ,fun))
		  ,@defs)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Define the compat groups                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(compat-define-group 'overlays

(defun-compat overlayp (object)
  "Return t if OBJECT is an overlay."
  (and (extentp object)
       (extent-property object 'overlay)))

(defun-compat make-overlay (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.
If omitted, BUFFER defaults to the current buffer.
BEG and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the
front delimiter advance when text is inserted there.
The fifth arg REAR-ADVANCE, if non-nil, makes the
rear delimiter advance when text is inserted there."
  (if (null buffer)
      (setq buffer (current-buffer))
    (check-argument-type 'bufferp buffer))
  (when (> beg end)
    (setq beg (prog1 end (setq end beg))))

  (let ((overlay (make-extent beg end buffer)))
    (set-extent-property overlay 'overlay t)
    (if front-advance
	(set-extent-property overlay 'start-open t)
      (set-extent-property overlay 'start-closed t))
    (if rear-advance
	(set-extent-property overlay 'end-closed t)
      (set-extent-property overlay 'end-open t))

    overlay))

(defun-compat move-overlay (overlay beg end &optional buffer)
  "Set the endpoints of OVERLAY to BEG and END in BUFFER.
If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
buffer."
  (check-argument-type 'overlayp overlay)
  (if (null buffer)
      (setq buffer (extent-object overlay)))
  (if (null buffer)
      (setq buffer (current-buffer)))
  (check-argument-type 'bufferp buffer)
  (and (= beg end)
       (extent-property overlay 'evaporate)
       (delete-overlay overlay))
  (when (> beg end)
    (setq beg (prog1 end (setq end beg))))
  (set-extent-endpoints overlay beg end buffer)
  overlay)

(defun-compat delete-overlay (overlay)
  "Delete the overlay OVERLAY from its buffer."
  (check-argument-type 'overlayp overlay)
  (detach-extent overlay)
  nil)

(defun-compat overlay-start (overlay)
  "Return the position at which OVERLAY starts."
  (check-argument-type 'overlayp overlay)
  (extent-start-position overlay))

(defun-compat overlay-end (overlay)
  "Return the position at which OVERLAY ends."
  (check-argument-type 'overlayp overlay)
  (extent-end-position overlay))

(defun-compat overlay-buffer (overlay)
  "Return the buffer OVERLAY belongs to."
  (check-argument-type 'overlayp overlay)
  (extent-object overlay))

(defun-compat overlay-properties (overlay)
  "Return a list of the properties on OVERLAY.
This is a copy of OVERLAY's plist; modifying its conses has no effect on
OVERLAY."
  (check-argument-type 'overlayp overlay)
  (extent-properties overlay))

(defun-compat overlays-at (pos)
  "Return a list of the overlays that contain position POS."
  (overlays-in pos pos))

(defun-compat overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG
or between BEG and END."
  (if (featurep 'xemacs)
      (mapcar-extents #'identity nil nil beg end
		      'all-extents-closed-open 'overlay)
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
      retval)))

(defun-compat next-overlay-change (pos)
  "Return the next position after POS where an overlay starts or ends.
If there are no more overlay boundaries after POS, return (point-max)."
  (let ((next (point-max))
	tmp)
    (map-extents
     (lambda (overlay ignore)
	    (when (or (and (< (setq tmp (extent-start-position overlay)) next)
			   (> tmp pos))
		      (and (< (setq tmp (extent-end-position overlay)) next)
			   (> tmp pos)))
	      (setq next tmp))
       nil)
     nil pos nil nil 'all-extents-closed-open 'overlay)
    next))

(defun-compat previous-overlay-change (pos)
  "Return the previous position before POS where an overlay starts or ends.
If there are no more overlay boundaries before POS, return (point-min)."
  (let ((prev (point-min))
	tmp)
    (map-extents
     (lambda (overlay ignore)
       (when (or (and (> (setq tmp (extent-end-position overlay)) prev)
		      (< tmp pos))
		 (and (> (setq tmp (extent-start-position overlay)) prev)
		      (< tmp pos)))
	 (setq prev tmp))
       nil)
     nil nil pos nil 'all-extents-closed-open 'overlay)
    prev))

(defun-compat overlay-lists ()
  "Return a pair of lists giving all the overlays of the current buffer.
The car has all the overlays before the overlay center;
the cdr has all the overlays after the overlay center.
Recentering overlays moves overlays between these lists.
The lists you get are copies, so that changing them has no effect.
However, the overlays you get are the real objects that the buffer uses."
  (or (boundp 'xemacs-internal-overlay-center-pos)
      (overlay-recenter (1+ (/ (- (point-max) (point-min)) 2))))
  (let ((pos xemacs-internal-overlay-center-pos)
	before after)
    (map-extents (lambda (overlay ignore)
		   (if (> pos (extent-end-position overlay))
		       (push overlay before)
		     (push overlay after))
		   nil)
		 nil nil nil nil 'all-extents-closed-open 'overlay)
    (cons (nreverse before) (nreverse after))))

(defun-compat overlay-recenter (pos)
  "Recenter the overlays of the current buffer around position POS."
  (set (make-local-variable 'xemacs-internal-overlay-center-pos) pos))

(defun-compat overlay-get (overlay prop)
  "Get the property of overlay OVERLAY with property name PROP."
  (check-argument-type 'overlayp overlay)
  (let ((value (extent-property overlay prop))
	category)
    (if (and (null value)
	     (setq category (extent-property overlay 'category)))
	(get category prop)
      value)))

(defun-compat overlay-put (overlay prop value)
  "Set one property of overlay OVERLAY: give property PROP value VALUE."
  (check-argument-type 'overlayp overlay)
  (cond ((eq prop 'evaporate)
	 (set-extent-property overlay 'detachable value))
	((eq prop 'before-string)
	 (set-extent-property overlay 'begin-glyph
			      (make-glyph (vector 'string :data value))))
	((eq prop 'after-string)
	 (set-extent-property overlay 'end-glyph
			      (make-glyph (vector 'string :data value))))
	((eq prop 'local-map)
	 (set-extent-property overlay 'keymap value))
	((memq prop '(window insert-in-front-hooks insert-behind-hooks
			     modification-hooks))
	 (error "cannot support overlay '%s property under XEmacs"
		prop)))
  (set-extent-property overlay prop value))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; extents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias-compat 'delete-extent 'delete-overlay)
(defalias-compat 'extent-end-position 'overlay-end)
(defalias-compat 'extent-start-position 'overlay-start)
(defalias-compat 'set-extent-endpoints 'move-overlay)
(defalias-compat 'set-extent-property 'overlay-put)
(defalias-compat 'make-extent 'make-overlay)

(defun-compat extent-property (extent property &optional default)
  (or (overlay-get extent property) default))

(defun-compat extent-at (pos &optional object property before at-flag)
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

(defun-compat map-extents (function &optional object from to
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; extents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

) ;; group overlays

) ;; compat-define-compat-functions

(fmakunbound 'compat-define-compat-functions)

)