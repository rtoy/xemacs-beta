;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3-widget.el,v --- An image widget
;; Author: wmperry
;; Created: 1996/05/29 03:11:24
;; Version: 1.14
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
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
;;; This is a widget that will do the best it can with an image.
;;;
;;; It can handle all the common occurences of images on the world wide web
;;; 1. A plain image - displays either a glyph of the image, or the
;;;    alternative text
;;; 2. A hyperlinked image - an image that is also a hypertext link to
;;;    another page.  Displays either a glyph of the image, or the
;;;    alternative text.  When activated with the mouse or the keyboard,
;;;    the 'href' property of the widget is retrieved.
;;; 3. Server side imagemaps - an image that has hotzones that lead to
;;;    different areas.  Unfortunately, we cannot tell where the links go
;;;    from the client - all processing is done by the server.  Displays
;;;    either a glyph of the image, or the alternative text.  When activated
;;;    with the mouse or the keyboard, the coordinates clicked on are
;;;    sent to the remote server as HREF?x,y.  If the link is activated
;;;    by the keyboard, then 0,0 are sent as the coordinates.
;;; 4. Client side imagemaps - an image that has hotzones that lead to
;;;    different areas.  All processing is done on the client side, so
;;;    we can actually show a decent representation on a TTY.  Displays
;;;    either a glyph of the image, or a drop-down-list of the destinations
;;;    These are either URLs (http://foo/...) or alternative text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'widget)
(require 'w3-vars)
(require 'w3-mouse)

(defvar w3-image-widget-keymap (make-sparse-keymap)
  "Keymap used over glyphs in an image widget")

(define-key w3-image-widget-keymap (vector w3-mouse-button1)
  'w3-image-widget-button-press)
(define-key w3-image-widget-keymap (vector w3-mouse-button2)
  'w3-image-widget-button-press)
  
(define-widget 'image 'default
  "A fairly complex image widget."
  :convert-widget 'w3-image-widget-convert
  :value-to-internal (lambda (widget value) value)
  :value-to-external (lambda (widget value) value)
  :value-set 'w3-image-widget-value-set
  :create 'w3-image-widget-create
  :delete 'w3-image-widget-delete
  :value-create 'w3-image-widget-value-create
  :value-delete 'w3-image-widget-value-delete
  :value-get 'w3-image-widget-value-get
  :notify 'w3-image-widget-notify
  )

(defun w3-image-widget-convert (widget)
  (let ((args (widget-get widget :args)))
    (widget-put widget :args nil)
    (while args
      (widget-put widget (car args) (cadr args))
      (setq args (cddr args)))
    widget))

(defun w3-image-widget-value-get (widget)
  (let ((children (widget-get widget :children)))
    (and (car children)
	 (widget-apply (car children) :value-get))))

(defun w3-image-widget-create (widget)
  ;; Create an image widget at point in the current buffer
  (let ((where (widget-get widget 'where)))
    (cond
     ((null where)
      (setq where (set-marker (make-marker) (point))))
     ((markerp where)
      nil)
     ((integerp where)
      (setq where (set-marker (make-marker) where)))
     (t
      (error "IMPOSSIBLE position in w3-image-widget-create: %s" where)))
    (widget-put widget 'where where))
  (w3-image-widget-value-create widget))

(defun w3-image-widget-value-set (widget value)
  ;; Recreate widget with new value.
  (save-excursion
    (w3-image-widget-delete widget)
    (if (w3-glyphp value)
	(widget-put widget 'glyph value)
      (widget-put widget :value value))
    (widget-apply widget :create)))

(defsubst w3-image-widget-usemap (widget)
  (let ((usemap (widget-get widget 'usemap)))
    (if (listp usemap)
	usemap
      (if (and usemap (string-match "^#" usemap))
	  (setq usemap (substring usemap 1 nil)))
      (cdr-safe (assoc usemap w3-imagemaps)))))

(defun w3-image-widget-callback (widget widget-ignore &optional event)
  (and (widget-get widget 'href) (w3-fetch (widget-get widget 'href))))
       
(defun w3-image-widget-value-create (widget)
  ;; Insert the printed representation of the value
  (let (
	(href (widget-get widget 'href))
	(server-map (widget-get widget 'ismap))
	(client-map (w3-image-widget-usemap widget))
	(where (or (widget-get widget 'where) (point)))
	(glyph (widget-get widget 'glyph))
	(alt (widget-get widget 'alt))
	(real-widget nil)
	(invalid-glyph nil)
	)

    ;; Specifier-instance will signal an error if we have an invalid
    ;; image specifier, which would be the case if we get screwed up
    ;; data back from a URL somewhere.
    
    (setq invalid-glyph (and glyph (condition-case ()
				       (if (specifier-instance
					    (glyph-image glyph))
					   nil)
				     (error t))))
    (if (or (not glyph) invalid-glyph)
	;; Do a TTY or delayed image version of the image.
	(save-excursion
	  (if (= 0 (length alt)) (setq alt nil))
	  (goto-char where)
	  (cond
	   (client-map
	    (let* ((default nil)
		   (options (mapcar
			     (function
			      (lambda (x)
				(if (eq (aref x 0) 'default)
				    (setq default (aref x 2)))
				(if (and (not default) (stringp (aref x 2)))
				    (setq default (aref x 2)))
				(list 'choice-item
				      :format "%[%t%]"
				      :tag (or (aref x 3) (aref x 2))
				      :value (aref x 2)))) client-map)))
	      (setq real-widget
		    (apply 'widget-create 'choice
			   :tag (or (widget-get widget :tag) "Imagemap")
			   :notify (widget-get widget :notify)
			   :value default options))))
	   ((and server-map (stringp href))
	    (setq real-widget
		  (widget-create 'push :tag alt
				 :delete 'widget-default-delete
				 :value href
				 :notify (widget-get widget :notify))))
	   (href
	    (setq real-widget
		  (widget-create 'push :tag (or alt "Image")
				 :value href
				 :delete 'widget-default-delete
				 :notify 'w3-image-widget-callback)))
	   (alt
	    (setq real-widget (widget-create 'item :format "%v" :value alt))))
	  (if (not real-widget)
	      nil
	    (widget-put real-widget 'usemap (widget-get widget 'usemap))
	    (widget-put real-widget 'href href)
	    (widget-put real-widget 'ismap server-map)
	    (widget-put real-widget :parent widget)
	    (widget-put widget :children (list real-widget))))
      ;;; Actually use the image
      (let ((extent (or (widget-get widget 'extent)
			(make-extent where where))))
	(set-extent-endpoints extent where where)
	(widget-put widget 'extent extent)
	(widget-put widget :children nil)
	(set-extent-property extent 'keymap w3-image-widget-keymap)
	(set-extent-property extent 'begin-glyph glyph)
	(set-extent-property extent 'help-echo (cond
						((and href (or client-map
							       server-map))
						 (format "%s [map]" href))
						(href href)
						(t nil)))
	(set-glyph-property glyph 'widget widget)))))

(defun w3-image-widget-delete (widget)
  ;; Remove the widget from the buffer
  (let ((extent (widget-get widget 'extent))
	(child  (car (widget-get widget :children))))
    (cond
     (extent				; Remove a glyph
      (delete-extent extent))
     (child				; Remove a child widget
      (widget-apply child :delete))
     (t					; Doh!  Do nothing.
      nil))))     

(if (fboundp 'mouse-event-p)
    (fset 'w3-mouse-event-p 'mouse-event-p)
  (fset 'w3-mouse-event-p 'ignore))

(if (fboundp 'glyphp)
    (fset 'w3-glyphp 'glyphp)
  (fset 'w3-glyphp 'ignore))

(defun w3-image-widget-button-press (event)
  (interactive "@e")
  (let* ((glyph (and event (w3-mouse-event-p event) (event-glyph event)))
	 (widget (and glyph (glyph-property glyph 'widget))))
    (w3-image-widget-notify widget widget event)))    

(defun w3-image-widget-notify (widget widget-changed &optional event)
  ;; Happens when anything changes
  (let* ((glyph (and event (w3-mouse-event-p event) (event-glyph event)))
	 (x (and glyph (event-glyph-x-pixel event)))
	 (y (and glyph (event-glyph-y-pixel event)))
	 (ismap  (widget-get widget 'ismap))
	 (usemap (w3-image-widget-usemap widget))
	 (href   (widget-get widget 'href))
	 (value  (widget-value widget))
	 )
    (cond
     ((and glyph usemap)		; Do the client-side imagemap stuff
      (setq href (w3-point-in-map (vector x y) usemap nil))
      (if href
	  (w3-fetch href)
	(message "No destination found for %d,%d" x y)))
     ((and glyph x y ismap)		; Do the server-side imagemap stuff
      (w3-fetch (format "%s?%d,%d" href x y)))
     (usemap				; Dummed-down tty client side imap
      (w3-fetch value))
     (ismap				; Do server-side dummy imagemap for tty
      (w3-fetch (concat href "?0,0")))
     ((stringp href)			; Normal hyperlink
      (w3-fetch href))
     (t					; Huh?
      nil))))

(provide 'w3-widget)
