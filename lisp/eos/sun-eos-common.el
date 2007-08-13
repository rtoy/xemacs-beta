;; Copyright (C) 1995, Sun Microsystems
;;
;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;; Common routines for EOS

(defvar eos::version "1.5.2"
  "Version of Eos")

(defvar eos::left-margin-width 5
  "size of left margin")

(defvar eos::stop-color "red"
  "foreground color for stop signs")
(defvar eos::solid-arrow-color "purple"
  "foreground color for solid arrow")
(defvar eos::hollow-arrow-color "purple"
  "foreground color for hollow arrow")
(defvar eos::sbrowse-arrow-color "blue"
  "foreground color for browser glyphs")

(defun eos::recompute-presentation ()
  (set-face-foreground 'stop-face eos::stop-color)
  (set-face-foreground 'solid-arrow-face eos::solid-arrow-color)
  (set-face-foreground 'hollow-arrow-face eos::hollow-arrow-color)
  (set-face-foreground 'sbrowse-arrow-face eos::sbrowse-arrow-color)
  )

;;

(defvar eos::displayed-initial-message nil
  "Whether we have shown the initial display message")

(defconst eos::startup-message-lines
  '("Please send feedback to eos-comments@cs.uiuc.edu."
    "The latest Eos news are under SPARCworks->News"
    "See Options->SPARCworks for configuration and Help->SPARCworks for help"
    ))

;; copied from vm

(defun eos::display-initial-message ()
  ;; Display initial Eos message - REMOVED
  )

(defun eos-old::display-initial-message ()
  ;; Display initial Eos message
  (if (not eos::displayed-initial-message)
      (let ((lines eos::startup-message-lines))
	(message "Eos %s, Copyright (C) 1995 Sun MicroSystems"
		 eos::version)
	(setq eos::displayed-initial-message t)
	(while (and (sit-for 3) lines)
	  (message (car lines))
	  (setq lines (cdr lines))))
    (message "")))

;; misc

(defun eos::line-at (pos)
  ;; At what line is POS
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

;; frame-specific enabling
;;
;; will maintain at most one frame to debugger, one to sbrowser
;; annotations have a type, either
;;
;;	sbrowser
;;	debugger-solid-arrow
;;	debugger-holow-arrow
;;	debugger-stop
;;	debugger-visit
;;
;; adding an annotation of type sbrowser will be only on frame sbrowser
;; adding an annotation of type debugger will be only on frame debugger
;;
;; turn off patterns when there is no frame.


;;;
;;; Common ToolTalk function
;;;

(defun make-an-observer (op callback)
  (let ((pattern-desc
	 (list
	  'category 'TT_OBSERVE
	  'scope 'TT_SESSION
	  'class 'TT_NOTICE
	  'op op
	  'callback callback)))
    (make-tooltalk-pattern pattern-desc)
    ))

;;;
;;; Frame management
;;;

(defun eos::log (msg)
  (if (fboundp 'ut-log-text)
      (ut-log-text "eos version: %s; %s" eos::version msg)))

(defvar eos::sbrowser-frame nil)
(defvar eos::debugger-frame nil)

(defun eos::update-specifiers (type old-frame new-frame)
  ;; Change the database for annotations of TYPE, so that OLD-FRAME is
  ;; now using the alternate specifier, while NEW-FRAME uses the main one
  (let* ((device-type (device-type (selected-device)))
	 (g (eos::annotation-get-glyph type device-type))
	 (im (and (glyphp g) (glyph-image g)))
	 (new-instantiator (eos::annotation-get-inst type device-type))
	 (alt-instantiator (eos::annotation-get-inst-alt type device-type))
	 )
    (if (eq device-type 'x)
	(progn
	  (if (frame-live-p old-frame)
	      (progn
		(remove-specifier im old-frame)
		(add-spec-to-specifier im alt-instantiator old-frame)))
	  (if new-frame
	      (progn
		(add-spec-to-specifier im new-instantiator new-frame)
	  ))))))


(defun eos::select-sbrowser-frame (frame)
  (require 'eos-toolbar  "sun-eos-toolbar")
  (let ((toolbar (eos::toolbar-position)))
    (eos::display-initial-message)
    ;; logging
    (if frame
	(eos::log "selected frame for sbrowser")
      (eos::log "unselected frame for sbrowser"))
    ;; TT patterns
    (cond
     ((and (null eos::sbrowser-frame) frame)
      (eos::register-sbrowser-patterns))
     ((and (null frame) eos::sbrowser-frame)
      (eos::unregister-sbrowser-patterns)))
    ;; adjust  toolbars
    (if (frame-live-p eos::sbrowser-frame)
	(remove-specifier toolbar eos::sbrowser-frame))
    (if (frame-live-p eos::debugger-frame)
	(remove-specifier toolbar eos::debugger-frame))
    ;; then add
    (cond
     ((and (frame-live-p eos::debugger-frame) (frame-live-p frame)
	   (equal eos::debugger-frame frame))
      (add-spec-to-specifier toolbar eos::debugger-sbrowser-toolbar frame))
     ((and (frame-live-p eos::debugger-frame) (frame-live-p frame))
      (add-spec-to-specifier toolbar eos::sbrowser-toolbar frame)
      (add-spec-to-specifier toolbar eos::debugger-toolbar eos::debugger-frame))
     ((frame-live-p frame)
      (add-spec-to-specifier toolbar eos::sbrowser-toolbar frame))
     ((frame-live-p eos::debugger-frame)
      (add-spec-to-specifier toolbar eos::debugger-toolbar eos::debugger-frame))
     )
    ;; adjust specifiers for glyphs
    (eos::update-specifiers 'sbrowser eos::sbrowser-frame frame)
    (if (frame-live-p eos::sbrowser-frame)
	(progn
	  (remove-specifier use-left-overflow eos::sbrowser-frame)
	  (remove-specifier left-margin-width eos::sbrowser-frame)))
    (if (frame-live-p frame)
	(progn
	  (add-spec-to-specifier use-left-overflow t frame)
	  (add-spec-to-specifier left-margin-width eos::left-margin-width frame)
	  (add-spec-to-specifier left-margin-width 0 (minibuffer-window))))
    (if (frame-live-p eos::debugger-frame)
	(progn
	  (add-spec-to-specifier use-left-overflow t eos::debugger-frame)
	  (add-spec-to-specifier left-margin-width eos::left-margin-width eos::debugger-frame)
	  (add-spec-to-specifier left-margin-width 0 (minibuffer-window))))
    ;;
    (setq eos::sbrowser-frame frame)
    (set-menubar-dirty-flag)
    ))

(defun eos::select-debugger-frame (frame)
  (require 'eos-toolbar  "sun-eos-toolbar")
  (let ((toolbar (eos::toolbar-position)))
    (eos::display-initial-message)
    (save-excursion
      (eos::ensure-debugger-buffer)
      (bury-buffer))
    ;; logging
    (if frame
	(eos::log "selected frame for debugger")
      (eos::log "unselected frame for debugger"))
    ;; TT patterns
    (cond
     ((and (null eos::debugger-frame) frame)
      (eos::register-debugger-patterns)
      (eos::register-visit-file-pattern))
     ((and (null frame) eos::debugger-frame)
      (eos::unregister-debugger-patterns)
      (eos::unregister-visit-file-pattern)))
    ;; adjust toolbars, remove
    (if (frame-live-p eos::sbrowser-frame)
	(remove-specifier toolbar eos::sbrowser-frame))
    (if (frame-live-p eos::debugger-frame)
	(remove-specifier toolbar eos::debugger-frame))
    ;; then add
    (cond
     ((and (frame-live-p eos::sbrowser-frame) (frame-live-p frame)
	   (equal eos::sbrowser-frame frame))
      (add-spec-to-specifier toolbar eos::debugger-sbrowser-toolbar frame))
     ((and (frame-live-p eos::sbrowser-frame) (frame-live-p frame))
      (add-spec-to-specifier toolbar eos::debugger-toolbar frame)
      (add-spec-to-specifier toolbar eos::sbrowser-toolbar eos::sbrowser-frame))
     ((frame-live-p frame)
      (add-spec-to-specifier toolbar eos::debugger-toolbar frame))
     ((frame-live-p eos::sbrowser-frame)
      (add-spec-to-specifier toolbar eos::sbrowser-toolbar eos::sbrowser-frame))
     )
    ;; update glyph specifiers
    (eos::update-specifiers 'debugger-solid-arrow eos::debugger-frame frame)
    (eos::update-specifiers 'debugger-hollow-arrow eos::debugger-frame frame)
    (eos::update-specifiers 'debugger-stop eos::debugger-frame frame)
    (if (frame-live-p eos::debugger-frame)
	(progn
	  (remove-specifier use-left-overflow eos::debugger-frame)
	  (remove-specifier left-margin-width eos::debugger-frame)))
    (if (frame-live-p frame)
	(progn
	  (add-spec-to-specifier use-left-overflow t frame)
	  (add-spec-to-specifier left-margin-width eos::left-margin-width frame)
	  (add-spec-to-specifier left-margin-width 0 (minibuffer-window))))
    (if (frame-live-p eos::sbrowser-frame)
	(progn
	  (add-spec-to-specifier use-left-overflow t eos::sbrowser-frame)
	  (add-spec-to-specifier left-margin-width eos::left-margin-width eos::sbrowser-frame)
	  (add-spec-to-specifier left-margin-width 0 (minibuffer-window))))
    ;;
    (setq eos::debugger-frame frame)
    (set-menubar-dirty-flag)
    ))

;; HERE  use file-truename

(defun eos::select-frame (type)
  ;; Select a frame; return nil if should skip
  (cond ((eq type 'sbrowser) 
	 (if (frame-live-p eos::sbrowser-frame)
	     eos::sbrowser-frame
	   (message "selecting destroyed frame; will ignore")
	   (eos::select-sbrowser-frame nil)
	   nil))
	((or (eq type 'debugger-solid-arrow)
	     (eq type 'debugger-hollow-arrow)
	     (eq type 'debugger-stop)
	     (eq type 'debugger-visit))
	 (if (frame-live-p eos::debugger-frame)
	     eos::debugger-frame
	   (message "selecting destroyed frame; will ignore")
	   (eos::select-debugger-frame nil)
	   nil))
	(t (selected-frame))))

(defun eos::select-window (win)
  ;; Will select a window if it is not showing neither of eos::debugger-buffer or
  ;; eos::toolbar-buffer"
  (let ((name (buffer-name (window-buffer win))))
    (if (and (>= (length name) 4)
	     (equal (substring name 0 4) "*Eos"))
	nil
      (select-window win)
      (throw 'found t)
      )))

(defun eos::find-line (file line type)
  ;; Show FILE at LINE; returns frame or nil if inappropriate
  ;; if type is nil
  (if (eos::null-file file)
      (selected-frame)
    (let ((sc (eos::select-frame type))
	  (win (selected-window)))
      (if (null sc)
	  nil
	(select-frame sc)
	(if (catch 'found
	      (eos::select-window (selected-window))
	      (walk-windows 'eos::select-window)
	       nil)
	    nil				; do nothing, already there
	  (select-window win)
	  (split-window-vertically)
	  (other-window 1)
	  )
	(switch-to-buffer (find-file-noselect file t)) ;; no warn!
	(if (eq (device-type) 'x) (x-disown-selection))
	(goto-line line)
	sc
	))))

(defun eos::null-file (file)
  ;; returns t if FILE is nil or the empty string
  (or (null file) (equal file "")))

;;;
;;; Annotation handling
;;;

(defun eos::valid-annotation (annotation)
  ;; returns t if ANNOTATION is an annotation and its buffer exists
  (and (annotationp annotation)
       (bufferp (extent-buffer annotation))
       (buffer-name (extent-buffer annotation)))
  )

(defvar eos::annotation-list nil
  "list of annotations set")

(defun eos::add-to-annotation-list (ann type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (setq eos::annotation-list (cons ann
				      eos::annotation-list))
  )

(defun eos::remove-from-annotation-list (ann type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (setq eos::annotation-list (delq ann eos::annotation-list))
  )

(defun eos::remove-all-from-annotation-list (type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (mapcar (function (lambda (annot)
		      (if (extent-live-p annot)
			  (delete-annotation annot))))
	  eos::annotation-list)
  (setq eos::annotation-list nil))

(defun eos::add-annotation (type file line uid)
  (let ((anot nil)
	(fr (selected-frame))
	(win (selected-window))
	)
      (if (eos::null-file file)
	  (setq anot nil)
	(if (null (eos::find-line file line type))
	    (error "No frame to select"))
	(let* ((device-type (device-type (selected-device)))
	       (graphics (eos::annotation-get-glyph type device-type))
	       (face (eos::annotation-get-face type device-type))
	       )
	  (setq anot (make-annotation graphics (point) 'whitespace))
	  (set-annotation-data anot uid)
	  (set-extent-face anot face)
	  (eos::add-to-annotation-list anot type)
	  ))
      (select-frame fr)
      (select-window win)
      anot
  ))

(defun eos::compare-uid (extent uid)
  (and (annotationp extent)
       (equal (annotation-data extent) uid)
       extent))

(defun eos::delete-annotation (type file line uid)
  ;; ignore file and line, they are here for backward compatibility
  (let ((anot nil)
	(alist eos::annotation-list)
	)
    (if (not (eq type 'debugger-stop))
	(error "not implemented"))
    (while (and alist
		(not (equal (annotation-data (car alist)) uid)))
      (setq alist (cdr alist)))
    (if (null alist)
	(error "Event UID not found; ignored")
      (setq anot (car alist))
      (delete-annotation anot)
      (eos::remove-from-annotation-list anot type))
    ))

;; probably type should not be given here... (already stored in the annotation-data
;; field)  but it is a bit more robust this way.

(defun eos::make-annotation-visible (annotation file line type)
  ;; returns nil or moves the ANNOTATION to FILE and LINE; annotation is of TYPE
  (let ((back nil)
	(fr (selected-frame))
	(win (selected-window))
	)
    ;;    (save-window-excursion
    (if (not (eos::null-file file))
	(progn
	  (if (eos::valid-annotation annotation)
	      (detach-extent annotation) ; should operate on annotations
	    )
	  (if (null (eos::find-line file line type))
		(error "No frame to select"))
	  (let* ((device-type (device-type (selected-device)))
		 (graphics (eos::annotation-get-glyph type device-type))
		 (face (eos::annotation-get-face type device-type))
		 )
	    (if (and (eos::valid-annotation annotation)
		     (extent-detached-p annotation))
		(progn
		  (setq back (insert-extent annotation (point) (point) t))
		  (set-annotation-glyph back graphics 'whitespace)
		  )
	      (setq back (make-annotation graphics (point) 'whitespace))
	      )
	    (set-annotation-data back type)
	    (set-extent-face back face)
	    )))
    ;;      )
    (if (not (eq (selected-frame) fr))
	(select-frame fr))
    (select-window win)
    back
    ))

(defun eos::make-annotation-invisible (annotation)
  ;; make this ANNOTATION invisible
  (if (eos::valid-annotation annotation)
      (detach-extent annotation)	;;  should operate on annotations
  ))


;; mapping between annotation types and their screen representations.

(defvar eos::alist-annotation-glyph nil) ; assoc list of annotation type
					;  device type, and glyph
(defvar eos::alist-annotation-inst nil) ; assoc list of annotation type
					;  device type, and instantiator
(defvar eos::alist-annotation-inst-alt nil) ; alternate assoc list of annotation type
					;  device type, and instantiator

(defvar eos::alist-annotation-face nil)  ;;  assoc list of annotation type,
				       ;; device type and face

;; PUBLIC

;; TBD! merge both instance lists.

(defun eos::annotation-set-inst (annotation-type device-type inst inst-alt)
  "define the instantiator for ANNOTATION-TYPE on DEVICE-TYPE to be
INST for the frame enabled for this type and INST-ALT for other frames"
  (interactive)
  (setq eos::alist-annotation-inst
	(cons (cons (cons annotation-type device-type) inst)
	      eos::alist-annotation-inst))
  (setq eos::alist-annotation-inst-alt
	(cons (cons (cons annotation-type device-type) inst-alt)
	      eos::alist-annotation-inst-alt))  )

(defun eos::annotation-set-face (annotation-type device-type face-1 face-2)
  "define the face for ANNOTATION-TYPE on DEVICE-TYPE to be
FACE-1 for the frame enabled for this type and FACE-2 for other frames"
  (interactive)
  (setq eos::alist-annotation-face
	(cons (cons (cons annotation-type device-type) face-1)
	      eos::alist-annotation-face))
  )

;; PRIVATE

(defun eos::annotation-get-glyph (annotation-type device-type)
  ;; Get the glyph for ANNOTATION-TYPE on DEVICE-TYPE
  (interactive)
  (let ((found (assoc (cons annotation-type device-type)
		      eos::alist-annotation-glyph)))
    (if found
	(cdr found)
      (let ((inst (eos::annotation-get-inst annotation-type device-type))
	    (alt-inst (eos::annotation-get-inst-alt annotation-type device-type))
	    (glyph nil)
	    (frame (selected-frame)))
	(if (null inst)
	    nil
	  (setq glyph (make-glyph `((global . (nil . ,alt-inst)))))
	  (add-spec-to-specifier (glyph-image glyph) inst frame)
	  (setq eos::alist-annotation-glyph
		(cons (cons (cons annotation-type device-type) glyph)
		    eos::alist-annotation-glyph))
	  glyph))
      )))

(defun eos::annotation-get-inst (annotation-type device-type)
  ;; Get the primary instantiator for ANNOTATION-TYPE on DEVICE-TYPE
  (interactive)
  (let ((found (assoc (cons annotation-type device-type)
		      eos::alist-annotation-inst)))
    (if found
	(cdr found)
      nil)))

(defun eos::annotation-get-inst-alt (annotation-type device-type)
  ;; Get the alternate instantiator for ANNOTATION-TYPE on DEVICE-TYPE
  (interactive)
  (let ((found (assoc (cons annotation-type device-type)
		      eos::alist-annotation-inst-alt)))
    (if found
	(cdr found)
      nil)))

(defun eos::annotation-get-face (annotation-type device-type)
  ;; Get the face for ANNOTATION-TYPE on DEVICE-TYPE 
  (interactive)
  (let ((found (assoc (cons annotation-type device-type)
		      eos::alist-annotation-face))
	)
    (if found
	(cdr found)
      nil
      ))
  )


(defun eos::common-startup () )
;;


(provide 'eos-common)
