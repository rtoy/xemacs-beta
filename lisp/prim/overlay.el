;;; overlay.el --- overlay support.

;;;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Maintainer: XEmacs
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
  (let (overlay temp)
    (if (null buffer)
	(setq buffer (current-buffer))
      (check-argument-type 'bufferp buffer))

    (if (> beg end)
	(setq temp beg
	      beg end
	      end temp))

    (setq overlay (make-extent beg end buffer))
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
  (let (temp)
    (check-argument-type 'overlayp overlay)
    (if (null buffer)
	(setq buffer (extent-object overlay)))
    
    (if (null buffer)
	(setq buffer (current-buffer)))
    
    (check-argument-type 'bufferp buffer)
    
    (if (and (= beg end)
	     (not (null (extent-property overlay 'evaporate))))
	(delete-overlay overlay)

      (if (> beg end)
	  (setq temp beg
		beg end
		end temp))

      (set-extent-endpoints overlay beg end buffer)
      overlay)))

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
  (let (lst)
    (mapcar (function
	     (lambda (overlay)
	       (and (extent-property overlay 'overlay)
		    (setq lst (append lst (list overlay))))))
	    (extent-list nil beg end))
    lst))

(defun next-overlay-change (pos)
  "Return the next position after POS where an overlay starts or ends.
If there are no more overlay boundaries after POS, return (point-max)."
  (let ((next (point-max))
	end)
    (mapcar (function
	     (lambda (overlay)
	       (if (< (setq end (extent-end-position overlay)) next)
		   (setq next end))))
	    (overlays-in pos end))
    next))

(defun previous-overlay-change (pos)
  "Return the previous position before POS where an overlay starts or ends.
If there are no more overlay boundaries before POS, return (point-min)."
  (let ((prev (point-min))
	beg)
    (mapcar (function
	     (lambda (overlay)
	       (if (and (> (setq beg (extent-start-position overlay)) prev)
			(< beg pos))
		   (setq prev beg))))
	    (overlays-in prev pos))
    prev))

(defun overlay-lists ()
  "Return a pair of lists giving all the overlays of the current buffer.
The car has all the overlays before the overlay center;
the cdr has all the overlays after the overlay center.
Recentering overlays moves overlays between these lists.
The lists you get are copies, so that changing them has no effect.
However, the overlays you get are the real objects that the buffer uses."
  (if (not (boundp 'xemacs-internal-overlay-center-pos))
      (overlay-recenter (/ (- (point-max) (point-min)) 2)))
  (let ((pos xemacs-internal-overlay-center-pos)
	before after)
    (mapcar
     (function
      (lambda (overlay)
	(if (extent-property overlay 'overlay)
	    (if (> pos (extent-end-position overlay))
		(setq before (append before (list overlay)))
	      (setq after (append after (list overlay)))))))
     (extent-list))
    (list before after)))

(defun overlay-recenter (pos)
  "Recenter the overlays of the current buffer around position POS."
  (set (make-local-variable 'xemacs-internal-overlay-center-pos) pos))

(defun overlay-get (overlay prop)
  "Get the property of overlay OVERLAY with property name PROP."
  (check-argument-type 'overlayp overlay)
  (extent-property overlay prop))

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
	((memq prop '(window insert-in-front-hooks insert-behind-hooks
			     modification-hooks))
	 (error "cannot support overlay '%s property under XEmacs"
		prop)))
  (set-extent-property overlay prop value))

(provide 'overlay)

;;; overlay.el ends here
