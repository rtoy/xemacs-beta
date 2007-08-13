;;; overlay.el --- overlay support.

;;;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Joe Nuspl <nuspl@sequent.com>
;; Maintainer: XEmacs Development Team (in <hniksic@srce.hr> incarnation)
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

;;; Commentary:

;; Unlike the text-properties interface, these functions are in fact
;; totally useless in XEmacs.  They are a more or less straightforward
;; interface to the much better extent API, provided exclusively for
;; GNU Emacs compatibility.  If you notice an incompatibility not
;; mentioned below, be sure to mention it.  Anyways, you should really
;; not use this.

;; Known incompatibilities with the FSF interface:

;; 1. There is not an `overlay' type.  Any extent with non-nil
;;    'overlay property is considered an "overlay".
;;
;; 2. Some features of FSF overlays have not been implemented in
;;    extents (or are unneeded).  Specifically, those are the
;;    following special properties: window, insert-in-front-hooks,
;;    insert-behind-hooks, and modification-hooks.  Some of these will
;;    probably be implemented for extents in the future.
;;
;; 3. In FSF, beginning and end of an overlay are markers, which means
;;    that you can use `insert-before-markers' to change insertion
;;    property of overlay.  It will not work in this emulation, and we
;;    have no plans of providing it.
;;
;; 4. The `overlays-in' and `overlays-at' functions in some cases
;;    don't work as they should.  To be fixed RSN.
;;
;; 5. Finally, setting or modification of overlay properties specific
;;    to extents will have unusual results.  While (overlay-put
;;    overlay 'start-open t) does nothing under FSF, it has a definite
;;    effect under XEmacs.  This is solved by simply avoiding such
;;    names (see `set-extent-property' for a list).

;; Some functions were broken; fixed-up by Hrvoje Niksic, June 1997.


;;; Code:

(defun overlayp (object)
  "Return t if OBJECT is an overlay."
  (and (extentp object)
       (extent-property object 'overlay)))

(defun make-overlay (beg end &optional buffer front-advance rear-advance)
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

(defun move-overlay (overlay beg end &optional buffer)
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

(defun delete-overlay (overlay)
  "Delete the overlay OVERLAY from its buffer."
  (check-argument-type 'overlayp overlay)
  (detach-extent overlay)
  nil)

(defun overlay-start (overlay)
  "Return the position at which OVERLAY starts."
  (check-argument-type 'overlayp overlay)
  (extent-start-position overlay))

(defun overlay-end (overlay)
  "Return the position at which OVERLAY ends."
  (check-argument-type 'overlayp overlay)
  (extent-end-position overlay))

(defun overlay-buffer (overlay)
  "Return the buffer OVERLAY belongs to."
  (check-argument-type 'overlayp overlay)
  (extent-object overlay))

(defun overlay-properties (overlay)
  "Return a list of the properties on OVERLAY.
This is a copy of OVERLAY's plist; modifying its conses has no effect on
OVERLAY."
  (check-argument-type 'overlayp overlay)
  (extent-properties overlay))

(defun overlays-at (pos)
  "Return a list of the overlays that contain position POS."
  (overlays-in pos pos))

(defun overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG
or between BEG and END."
  (mapcar-extents #'identity nil nil beg end nil 'overlay))

(defun next-overlay-change (pos)
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
     nil pos nil nil nil 'overlay)
    next))

(defun previous-overlay-change (pos)
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
     nil nil pos nil nil 'overlay)
    prev))

(defun overlay-lists ()
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
		 nil nil nil nil nil 'overlay)
    (cons (nreverse before) (nreverse after))))

(defun overlay-recenter (pos)
  "Recenter the overlays of the current buffer around position POS."
  (set (make-local-variable 'xemacs-internal-overlay-center-pos) pos))

(defun overlay-get (overlay prop)
  "Get the property of overlay OVERLAY with property name PROP."
  (check-argument-type 'overlayp overlay)
  (let ((value (extent-property overlay prop))
	category)
    (if (and (null value)
	     (setq category (extent-property overlay 'category)))
	(get category prop)
      value)))

(defun overlay-put (overlay prop value)
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

(provide 'overlay)

;;; overlay.el ends here
