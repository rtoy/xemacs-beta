;;; w3-display.el --- display engine v99999
;; Author: wmperry
;; Created: 1997/01/31 04:26:17
;; Version: 1.115
;; Keywords: faces, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file is part of GNU Emacs.
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(require 'css)
(require 'font)
(require 'w3-widget)
(require 'w3-imap)

(autoload 'sentence-ify "flame")
(autoload 'string-ify "flame")
(autoload '*flame "flame")
(if (not (fboundp 'flatten)) (autoload 'flatten "flame"))
(defvar w3-cookie-cache nil)

(defmacro w3-d-s-var-def (var)
  (` (make-variable-buffer-local (defvar (, var) nil))))

(w3-d-s-var-def w3-display-open-element-stack)
(w3-d-s-var-def w3-display-alignment-stack)
(w3-d-s-var-def w3-display-list-stack)
(w3-d-s-var-def w3-display-form-id)
(w3-d-s-var-def w3-display-whitespace-stack)
(w3-d-s-var-def w3-display-font-family-stack)
(w3-d-s-var-def w3-display-font-weight-stack)
(w3-d-s-var-def w3-display-font-variant-stack)
(w3-d-s-var-def w3-display-font-size-stack)
(w3-d-s-var-def w3-face-color)
(w3-d-s-var-def w3-face-background-color)
(w3-d-s-var-def w3-active-faces)
(w3-d-s-var-def w3-active-voices)
(w3-d-s-var-def w3-current-form-number)
(w3-d-s-var-def w3-face-font-family)
(w3-d-s-var-def w3-face-font-weight)
(w3-d-s-var-def w3-face-font-variant)
(w3-d-s-var-def w3-face-font-size)
(w3-d-s-var-def w3-face-font-family)
(w3-d-s-var-def w3-face-font-size)
(w3-d-s-var-def w3-face-font-style)
(w3-d-s-var-def w3-face-font-spec)
(w3-d-s-var-def w3-face-text-decoration)
(w3-d-s-var-def w3-face-face)
(w3-d-s-var-def w3-face-descr)
(w3-d-s-var-def w3-face-pixmap)
(w3-d-s-var-def w3-display-css-properties)

(eval-when-compile
  (defmacro w3-get-attribute (attr)
    (` (cdr-safe (assq (, attr) args))))
  
  (defmacro w3-get-face-info (info)
    (let ((var (intern (format "w3-face-%s" info))))
      (` (push (w3-get-style-info (quote (, info)) node (car (, var)))
	       (, var)))))

  (defmacro w3-pop-face-info (info)
    (let ((var (intern (format "w3-face-%s" info))))
      (` (pop (, var)))))

  (defmacro w3-get-all-face-info ()
    (`
     (progn
       (w3-get-face-info font-family)
       (w3-get-face-info font-style)
       (w3-get-face-info font-weight)
       (w3-get-face-info font-variant)
       (w3-get-face-info font-size)
       (w3-get-face-info text-decoration)
       ;;(w3-get-face-info pixmap)
       (w3-get-face-info color)
       (w3-get-face-info background-color)
       (setq w3-face-font-spec (make-font
				:weight (car w3-face-font-weight)
				:family (car w3-face-font-family)
				:size (car w3-face-font-size))))))

  (defmacro w3-pop-all-face-info ()
    (`
     (progn
       (w3-pop-face-info font-family)
       (w3-pop-face-info font-weight)
       (w3-pop-face-info font-variant)
       (w3-pop-face-info font-size)
       (w3-pop-face-info font-style)
       (w3-pop-face-info text-decoration)
       ;;(w3-pop-face-info pixmap)
       (w3-pop-face-info color)
       (w3-pop-face-info background-color))))

  )

(defvar w3-display-same-buffer nil)
(defvar w3-face-cache nil  "Cache for w3-face-for-element")
(defvar w3-face-index 0)
(defvar w3-image-widgets-waiting nil)

(make-variable-buffer-local 'w3-last-fill-pos)

(defconst w3-fill-prefixes-vector
  (let ((len 0)
        (prefix-vector (make-vector 80 nil)))
    (while (< len 80)
      (aset prefix-vector len (make-string len ? ))
      (setq len (1+ len)))
    prefix-vector))

(defconst w3-line-breaks-vector
  (let ((len 0)
	(breaks-vector (make-vector 10 nil)))
    (while (< len 10)
      (aset breaks-vector len (make-string len ?\n))
      (setq len (1+ len)))
    breaks-vector))

(defun w3-pause ()
  (cond
   (w3-running-FSF19 (sit-for 0))
   (w3-running-xemacs
    (sit-for 0))
   ;; (if (and (not (sit-for 0)) (input-pending-p))
   ;;	(condition-case ()
   ;;	    (dispatch-event (next-command-event))
   ;;	  (error nil)))
   (t (sit-for 0))))

(defmacro w3-get-pad-string (len)
  (` (cond
      ((< (, len) 0)
       "")
      ((< (, len) 80)
       (aref w3-fill-prefixes-vector (, len)))
      (t (make-string (, len) ? )))))

(defsubst w3-set-fill-prefix-length (len)
  (setq fill-prefix (if (< len (- (or w3-strict-width (window-width)) 4))
			(w3-get-pad-string len)
		      (url-warn
		       'html
		       "Runaway indentation!  Too deep for window width!")
		      fill-prefix)))

(defsubst w3-get-style-info (info node &optional default)
  (or (cdr-safe (assq info w3-display-css-properties)) default))

(defun w3-decode-area-coords (str)
  (let (retval)
    (while (string-match "\\([ \t0-9]+\\),\\([ \t0-9]+\\)" str)
      (setq retval (cons (vector (string-to-int (match-string 1 str))
				 (string-to-int (match-string 2 str))) retval)
	    str (substring str (match-end 0) nil)))
    (if (string-match "\\([0-9]+\\)" str)
	(setq retval (cons (vector (+ (aref (car retval) 0)
				      (string-to-int (match-string 1 str)))
				   (aref (car retval) 1)) retval)))
    (nreverse retval)))

(defun w3-normalize-color (color)
  (cond
   ((valid-color-name-p color)
    color)
   ((valid-color-name-p (concat "#" color))
    (concat "#" color))
   ((string-match "[ \t\r\n]" color)
    (w3-normalize-color
     (mapconcat (function (lambda (x) (if (memq x '(?\t ?\r ?\n ? )) ""
					(char-to-string x)))) color "")))
   ((valid-color-name-p (font-normalize-color color))
    (font-normalize-color color))
   (t
    (w3-warn 'html (format "Bad color specification: %s" color))
    nil)))

(defsubst w3-voice-for-element (node)
  (if (featurep 'emacspeak)
      (let (family gain left right pitch pitch-range stress richness voice)
	(setq family (w3-get-style-info 'voice-family node)
	      gain (w3-get-style-info 'gain node)
	      left (w3-get-style-info 'left-volume node)
	      right (w3-get-style-info 'right-volume node)
	      pitch (w3-get-style-info 'pitch node)
	      pitch-range (w3-get-style-info 'pitch-range node)
	      stress (w3-get-style-info 'stress node)
	      richness (w3-get-style-info 'richness node))
	(if (or family gain left right pitch pitch-range stress richness)
	    (setq voice (dtk-personality-from-speech-style
			 (make-dtk-speech-style :family (or family 'paul)
						:gain (or gain 5)
						:left-volume (or left 5)
						:right-volume (or right 5)
						:average-pitch (or pitch 5)
						:pitch-range (or pitch-range 5)
						:stress (or stress 5)
						:richness (or richness 5))))
	  (setq voice nil))
	(or voice (car w3-active-voices)))))

(defun w3-make-face-emacs19 (name &optional doc-string temporary)
  "Defines and returns a new FACE described by DOC-STRING.
If the face already exists, it is unmodified.
If TEMPORARY is non-nil, this face will cease to exist if not in use."
  (make-face name))

(cond
 ((not (fboundp 'make-face))
  (fset 'w3-make-face 'ignore))
 (w3-running-xemacs
  (fset 'w3-make-face 'make-face))
 (t
  (fset 'w3-make-face 'w3-make-face-emacs19)))

(defsubst w3-face-for-element (node)
  (w3-get-all-face-info)
  (if (car w3-face-text-decoration)
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-text-decoration)))
  (if w3-face-font-variant
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-font-variant)))
  (if w3-face-font-style
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-font-style)))
  (setq w3-face-descr (list w3-face-font-spec
			    (car w3-face-color)
			    (car w3-face-background-color))
	w3-face-face (cdr-safe (assoc w3-face-descr w3-face-cache)))
  (if (or w3-face-face (not (or (car w3-face-color)
				(car w3-face-background-color)
				w3-face-font-spec)))
      nil				; Do nothing, we got it already
    (setq w3-face-face
	  (w3-make-face (intern (format "w3-style-face-%05d" w3-face-index))
			"An Emacs-W3 face... don't edit by hand." t)
	  w3-face-index (1+ w3-face-index))
    (if w3-face-font-spec
	(set-face-font w3-face-face w3-face-font-spec))
    (if (car w3-face-color)
	(set-face-foreground w3-face-face (car w3-face-color)))
    (if (car w3-face-background-color)
	(set-face-background w3-face-face (car w3-face-background-color)))
    ;;(set-face-background-pixmap w3-face-face w3-face-pixmap)
    (setq w3-face-cache (cons
			 (cons w3-face-descr w3-face-face)
			 w3-face-cache)))
  w3-face-face)

(defun w3-normalize-spaces (string)
  ;; nuke spaces in the middle
  (while (string-match "[ \t\r\n][ \r\t\n]+" string)
    (setq string (concat (substring string 0 (1+ (match-beginning 0)))
			 (substring string (match-end 0)))))

  ;; nuke spaces at the beginning
  (if (string-match "^[ \t\r\n]+" string)
      (setq string (substring string (match-end 0))))

  ;; nuke spaces at the end
  (if (string-match "[ \t\n\r]+$" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defvar w3-bullets
  '((disc   . ?*)
    (circle . ?o)
    (square . ?#)
    (none   . ? )
    )
  "*An assoc list of unordered list types mapping to characters to use
as the bullet character.")    


(defsubst w3-display-line-break (n)
  (if (or
       (memq (car w3-display-whitespace-stack) '(pre nowrap)) ; Been told
       (= w3-last-fill-pos (point))
       (> w3-last-fill-pos (point-max)))
      (if (/= (preceding-char) ?\n) (setq n (1+ n))) ; at least put one line in
    (let ((fill-column (max (1+ (length fill-prefix)) fill-column))
	  width)
      (case (car w3-display-alignment-stack)
	(center
	 (fill-region-as-paragraph w3-last-fill-pos (point))
	 (center-region w3-last-fill-pos (point-max)))
	((justify full)
	 (fill-region-as-paragraph w3-last-fill-pos (point) t))
	(right
	 (fill-region-as-paragraph w3-last-fill-pos (point))
	 (goto-char w3-last-fill-pos)
	 (catch 'fill-exit
	   (while (re-search-forward ".$" nil t)
	     (if (>= (setq width (current-column)) fill-column)
		 nil			; already justified, or error
	       (beginning-of-line)
	       (insert-char ?  (- fill-column width))
	       (end-of-line)
	       (if (eobp)
		   (throw 'fill-exit t))
	       (condition-case ()
		   (forward-char 1)
		 (error (throw 'fill-exit t))))))
	 )
	(otherwise			; Default is left justification
	 (fill-region-as-paragraph w3-last-fill-pos (point)))
	))
    (setq n (1- n)))
  (setq w3-last-fill-pos (point-max))
  (insert (cond
	   ((<= n 0) "")
	   ((< n 10)
	    (aref w3-line-breaks-vector n))
	   (t
	    (make-string n ?\n)))))

(defsubst w3-munge-line-breaks-p ()
  (eq (car w3-display-whitespace-stack) 'pre))

(defvar w3-display-nil-face (w3-make-face nil "Stub face... don't ask." t))

(defvar w3-scratch-start-point nil)

(defsubst w3-handle-string-content (string)
  (setq w3-scratch-start-point (point))
  (insert string)
  (if (w3-munge-line-breaks-p)
      (progn
	(goto-char w3-scratch-start-point)
	(if (not (search-forward "\n" nil t))
	    (subst-char-in-region w3-scratch-start-point (point-max) ?\r ?\n)
	  (subst-char-in-region w3-scratch-start-point (point-max) ?\r ? )))
    (goto-char w3-scratch-start-point)
    (while (re-search-forward
	    " [ \t\n\r]+\\|[\t\n\r][ \t\n\r]*"
	    nil 'move)
      (replace-match " "))
    (goto-char w3-scratch-start-point)
    (if (and (memq (preceding-char) '(?  ?\t ?\r ?\n))
	     (looking-at "[ \t\r\n]"))
	(delete-region (point)
		       (progn
			 (skip-chars-forward " \t\r\n")
			 (point)))))
  (goto-char (point-max))
  (add-text-properties w3-scratch-start-point
		       (point) (list 'face w3-active-faces 'duplicable t))
  (if (car w3-active-voices)
      (add-text-properties w3-scratch-start-point (point)
			   (list 'personality (car w3-active-voices))))
  )

(defun w3-display-get-cookie (args)
  (if (not (fboundp 'cookie))
      "Sorry, no cookies today."
    (let* ((href (or (w3-get-attribute 'href) (w3-get-attribute 'src)))
	   (fname (or (cdr-safe (assoc href w3-cookie-cache))
		      (url-generate-unique-filename "%s.cki")))
	   (st (or (cdr-safe (assq 'start args)) "Loading cookies..."))
	   (nd (or (cdr-safe (assq 'end args)) "Loading cookies... done.")))
      (if (not (file-exists-p fname))
	  (save-excursion
	    (set-buffer (generate-new-buffer " *cookie*"))
	    (url-insert-file-contents href)
	    (write-region (point-min) (point-max) fname 5)
	    (setq w3-cookie-cache (cons (cons href fname) w3-cookie-cache))))
      (cookie fname st nd))))

(defun w3-widget-echo (widget &rest ignore)
  (let ((url (widget-get widget 'href))
	(name (widget-get widget 'name))
	(text (buffer-substring (widget-get widget :from)
				(widget-get widget :to)))
	(title (widget-get widget 'title))
	(check w3-echo-link)
	(msg nil))
    (if url
	(setq url (url-truncate-url-for-viewing url)))
    (if name
	(setq name (concat "anchor:" name)))
    (if (not (listp check))
	(setq check (cons check '(title url text name))))
    (catch 'exit
      (while check
	(and (boundp (car check))
	     (stringp (symbol-value (car check)))
	     (throw 'exit (symbol-value (car check))))
	(pop check)))))

(defun w3-follow-hyperlink (widget &rest ignore)
  (let* ((target (widget-get widget 'target))
	 (href (widget-get widget 'href)))
    (if target (setq target (intern (downcase target))))
    (case target
      ((_blank external)
       (w3-fetch-other-frame href))
      (_top
       (delete-other-windows)
       (w3-fetch href))
      (otherwise
       (w3-fetch href)))))

(defun w3-balloon-help-callback (object &optional event)
  (let* ((widget (widget-at (extent-start-position object)))
	 (href (and widget (widget-get widget 'href))))
    (if href
	(url-truncate-url-for-viewing href)
      nil)))


;; Various macros
(eval-when-compile
  (defmacro w3-expand-url (url)
    (`
     (url-expand-file-name (, url)
			   (cdr-safe
			    (assoc
			     (cdr-safe
			      (assq 'base args)) w3-base-alist)))))

  (defmacro w3-handle-empty-tag ()
    (`
     (progn
       (push (cons tag args) w3-display-open-element-stack)
       (push content content-stack)
       (setq content nil))))

  (defmacro w3-handle-content (node)
    (`
     (progn
       (push (cons tag args) w3-display-open-element-stack)
       (push content content-stack)
       (setq content (nth 2 node)))))

  (defmacro w3-display-handle-list-type ()
    (`
     (case (car break-style)
       (list-item
	(let ((list-style (w3-get-style-info 'list-style-type node))
	      (list-num (if (car w3-display-list-stack)
			    (incf (car w3-display-list-stack))
			  1))
	      (margin (1- (car left-margin-stack)))
	      (indent (w3-get-style-info 'text-indent node 0)))
	  (if (> indent 0)
	      (setq margin (+ margin indent))
	    (setq margin (max 0 (- margin indent))))
	  (beginning-of-line)
	  (case list-style
	    ((disc circle square)
	     (insert (format (format "%%%dc" margin)
			     (or (cdr-safe (assq list-style w3-bullets))
				 ?o))))
	    ((decimal lower-roman upper-roman lower-alpha upper-alpha)
	     (let ((x (case list-style
			(lower-roman
			 (w3-decimal-to-roman list-num))
			(upper-roman
			 (upcase
			  (w3-decimal-to-roman list-num)))
			(lower-alpha
			 (w3-decimal-to-alpha list-num))
			(upper-alpha
			 (upcase
			  (w3-decimal-to-alpha list-num)))
			(otherwise
			 (int-to-string list-num)))))
	       (insert (format (format "%%%ds." margin) x))
	       )
	     )
	    (otherwise
	     (insert (w3-get-pad-string margin)))
	    )
	  )
	)
       (otherwise
	(insert (w3-get-pad-string (+ (car left-margin-stack)
				      (w3-get-style-info 'text-indent node 0)))))
       )
     )
    )

  (defmacro w3-display-set-margins ()
    (`
     (progn
       (push (+ (w3-get-style-info 'margin-left node 0)
		(car left-margin-stack)) left-margin-stack)
       (push (-
	      (car right-margin-stack)
	      (w3-get-style-info 'margin-right node 0)) right-margin-stack)
       (setq fill-column (car right-margin-stack))
       (w3-set-fill-prefix-length (car left-margin-stack))
       (w3-display-handle-list-type))))

  (defmacro w3-display-restore-margins ()
    (`
     (progn
       (pop right-margin-stack)
       (pop left-margin-stack))))

  (defmacro w3-display-handle-break ()
    (`
     (case (car break-style)
       (block				; Full paragraph break
	(if (eq (cadr break-style) 'list-item)
	    (setf (cadr break-style) 'line)
	  (w3-display-line-break 1))
	(w3-display-set-margins)
	(push
	 (w3-get-style-info 'white-space node
			    (car w3-display-whitespace-stack))
	 w3-display-whitespace-stack)
	(push
	 (or (w3-get-attribute 'align)
	     (w3-get-style-info 'text-align node
				(car w3-display-alignment-stack)))
	 w3-display-alignment-stack)
	(and w3-do-incremental-display (w3-pause)))
       ((line list-item)		; Single line break
	(w3-display-line-break 0)
	(w3-display-set-margins)
	(push
	 (w3-get-style-info 'white-space node
			    (car w3-display-whitespace-stack))
	 w3-display-whitespace-stack)
	(push
	 (w3-get-style-info 'text-align node
			    (or (w3-get-attribute 'align)
				(car w3-display-alignment-stack)))
	 w3-display-alignment-stack))
       (otherwise			; Assume 'inline' rendering as default
	nil))
     )
    )

  (defmacro w3-display-handle-end-break ()
    (`
     (case (pop break-style)
       (block				; Full paragraph break
	(w3-display-line-break 1)
	(w3-display-restore-margins)
	(pop w3-display-whitespace-stack)
	(pop w3-display-alignment-stack)
	(and w3-do-incremental-display (w3-pause)))
       ((line list-item)		; Single line break
	(w3-display-restore-margins)
	(w3-display-line-break 0)
	(pop w3-display-whitespace-stack)
	(pop w3-display-alignment-stack))      
       (otherwise			; Assume 'inline' rendering as default
	nil))
     )
    )
  )

;; <link> handling
(defun w3-parse-link (args)
  (let* ((type (if (w3-get-attribute 'rel) 'rel 'rev))
	 (desc (w3-get-attribute type))
	 (dc-desc (and desc (downcase desc))) ; canonical case
	 (dest (w3-get-attribute 'href))
	 (plist (alist-to-plist args))
	 (node-1 (assq type w3-current-links))
	 (node-2 (and node-1 desc (or (assoc desc
					     (cdr node-1))
				      (assoc dc-desc
					     (cdr node-1)))))
	 )
    ;; Canonicalize the case of link types we may look for
    ;; specifically (toolbar etc.) since that's done with
    ;; assoc.  See `w3-mail-document-author' and
    ;; `w3-link-toolbar', at least.
    (if (member dc-desc w3-defined-link-types)
	(setq desc dc-desc))
    (if dest				; ignore if HREF missing
	(cond
	 (node-2			; Add to old value
	  (setcdr node-2 (cons plist (cdr node-2))))
	 (node-1			; first rel/rev
	  (setcdr node-1 (cons (cons desc (list plist))
			       (cdr node-1))))
	 (t (setq w3-current-links
		  (cons (cons type (list (cons desc (list plist))))
			w3-current-links)))))
    (setq desc (and desc (intern dc-desc)))
    (case desc
      ((style stylesheet)
       (w3-handle-style plist))
      (otherwise
       )
      )
    )
  )


;; Image handling
(defun w3-maybe-start-image-download (widget)
  (let* ((src (widget-get widget 'src))
	 (cached-glyph (w3-image-cached-p src)))
    (if (and cached-glyph (widget-glyphp cached-glyph))
	(setq w3-image-widgets-waiting (cons widget w3-image-widgets-waiting))
      (cond
       ((or w3-delay-image-loads	; Delaying images
	    (not (fboundp 'valid-specifier-domain-p)) ; Can't do images
	    (eq (device-type) 'tty))	; Why bother?
	(w3-add-delayed-graphic widget))
       ((not (w3-image-loadable-p src nil)) ; Hey, we can't load it!
	(w3-warn 'images (format "Skipping image %s" (url-basepath src t)))
	(w3-add-delayed-graphic widget))
       (t				; Grab the images
	(let (
	      (url-request-method "GET")
	      (old-asynch url-be-asynchronous)
	      (url-request-data nil)
	      (url-request-extra-headers nil)
	      (url-source t)
	      (url-mime-accept-string (substring
				       (mapconcat
					(function
					 (lambda (x)
					   (if x
					       (concat (car x) ",")
					     "")))
					w3-allowed-image-types "")
				       0 -1))
	      (url-working-buffer (generate-new-buffer-name " *W3GRAPH*")))
	  (setq-default url-be-asynchronous t)
	  (setq w3-graphics-list (cons (cons src (make-glyph))
				       w3-graphics-list))
	  (save-excursion
	    (set-buffer (get-buffer-create url-working-buffer))
	    (setq url-current-callback-data (list widget)
		  url-be-asynchronous t
		  url-current-callback-func 'w3-finalize-image-download)
	    (url-retrieve src))
	  (setq-default url-be-asynchronous old-asynch)))))))

(defun w3-finalize-image-download (widget)
  (let ((glyph nil)
	(url (widget-get widget 'src))
	(node nil)
	(buffer (widget-get widget 'buffer)))
    (message "Enhancing image...")
    (setq glyph (image-normalize (cdr-safe (assoc url-current-mime-type
						  w3-image-mappings))
				 (buffer-string)))
    (message "Enhancing image... done")
    (kill-buffer (current-buffer))
    (cond
     ((w3-image-invalid-glyph-p glyph)
      (setq glyph nil)
      (w3-warn 'image (format "Reading of %s failed." url)))
     ((eq (aref glyph 0) 'xbm)
      (let ((temp-fname (url-generate-unique-filename "%s.xbm")))
	(save-excursion
	  (set-buffer (generate-new-buffer " *xbm-garbage*"))
	  (erase-buffer)
	  (insert (aref glyph 2))
	  (setq glyph temp-fname)
	  (write-region (point-min) (point-max) temp-fname)
	  (kill-buffer (current-buffer)))
	(setq glyph (make-glyph (list (cons 'x glyph))))
	(condition-case ()
	    (delete-file temp-fname)
	  (error nil))))
     (t
      (setq glyph (make-glyph glyph))))
    (setq node (assoc url w3-graphics-list))
    (cond
     ((and node glyph)
      (set-glyph-image (cdr node) (glyph-image glyph)))
     (glyph
      (setq w3-graphics-list (cons (cons url glyph) w3-graphics-list)))
     (t nil))

    (if (and (buffer-name buffer)	; Dest. buffer exists
	     (widget-glyphp glyph))	; got a valid glyph
	(save-excursion
	  (set-buffer buffer)
	  (if (eq major-mode 'w3-mode)
	      (widget-value-set widget glyph)
	    (setq w3-image-widgets-waiting
		  (cons widget w3-image-widgets-waiting)))))))

(defmacro w3-node-visible-p ()
  (` (not (eq (car break-style) 'none))))

(defmacro w3-handle-image ()
  (`
   (let* ((height (w3-get-attribute 'height))
	  (width (w3-get-attribute 'width))
	  (src (or (w3-get-attribute 'src) "Error Image"))
	  (our-alt (cond
		    ((null w3-auto-image-alt) "")
		    ((eq t w3-auto-image-alt)
		     (concat "[IMAGE(" (url-basepath src t) ")] "))
		    ((stringp w3-auto-image-alt)
		     (format w3-auto-image-alt (url-basepath src t)))))
	  (alt (or (w3-get-attribute 'alt) our-alt))
	  (ismap (and (assq 'ismap args) 'ismap))
	  (usemap (w3-get-attribute 'usemap))
	  (base (w3-get-attribute 'base))
	  (href (and hyperlink-info (widget-get (cadr hyperlink-info) 'href)))
	  (widget nil)
	  (align (or (w3-get-attribute 'align)
		     (w3-get-style-info 'vertical-align node))))
     (if (assq '*table-autolayout w3-display-open-element-stack)
	 (insert alt)
       (setq widget (widget-create 'image
				   :value-face w3-active-faces
				   'src src ; Where to load the image from
				   'alt alt ; Textual replacement
				   'ismap ismap ; Is it a server-side map?
				   'usemap usemap ; Is it a client-side map?
				   'href href ; Hyperlink destination
				   ))
       (widget-put widget 'buffer (current-buffer))
       (w3-maybe-start-image-download widget)
       (goto-char (point-max))))))

;; The table handling

(defvar w3-display-table-cut-words-p nil
  "*Whether to cut words that are oversized in table cells")
  
(defvar w3-display-table-force-borders nil
  "*Whether to always draw table borders")

(defun w3-display-table-cut ()
  (save-excursion
    (goto-char (point-min))
    (let ((offset -1))
      (while (< offset 0)
  	(end-of-line)
  	(setq offset (- fill-column (current-column)))
  	(cond ((< offset 0)
  	       (condition-case nil
  		   (progn (forward-char offset)
  			  (insert ?\n))
 		 (error (setq offset 0))))
  	      ((not (eobp))
  	       (forward-line 1)
  	       (setq offset -1)))))))


(defun w3-display-fix-widgets ()
  ;; Make markers belong to the right buffer
  (save-excursion
    (let ((st (point-min))
 	  (nd nil)
	  (widget nil) parent
 	  (to-marker nil)
 	  (from-marker nil))
      (while (setq st (next-single-property-change st 'button))
 	(setq nd (or (next-single-property-change st 'button) (point-max))
 	      widget (widget-at st)
 	      to-marker (and widget (widget-get widget :to))
 	      from-marker (and widget (widget-get widget :from))
 	      parent (and widget (widget-get widget :parent))
 	      )
	(if (not widget)
	    nil
	  (widget-put widget :from (set-marker (make-marker) st))
	  (widget-put widget :to   (set-marker (make-marker) nd))
	  (if (not parent)
	      nil
	    (widget-put parent :from (set-marker (make-marker) st))
	    (widget-put parent :to   (set-marker (make-marker) nd))))
 	(if (condition-case ()
 		(get-text-property (1+ nd) 'button)
 	      (error nil))
 	    (setq st nd)
 	  (setq st (min (point-max) (1+ nd))))))))

(defun w3-size-of-tree (tree minmax)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      ;; XXX fill-column set to 1 fails when fill-prefix is set
      ;; XXX setting fill-column at all isn't really right
      ;; for example <hr>s shouldn't be especially wide
      ;; we should set a flag that makes w3 never wrap a line
      (let ((fill-column (cond ((eq minmax 'min)
				3)
			       ((eq minmax 'max)
				400))) 
	    (fill-prefix "")
	    (w3-last-fill-pos (point-min))
	    a retval
	    (w3-do-incremental-display nil)
	    (hr-regexp  (concat "^"
				(regexp-quote 
				 (make-string 5 w3-horizontal-rule-char)) 
				"*$"))
	    )
	;;(push 'left  w3-display-alignment-stack)
	(push (if (eq minmax 'max) 'nowrap) w3-display-whitespace-stack)
	(while tree
	  (push (cons '*td args) w3-display-open-element-stack)
	  (w3-display-node (pop tree)))
	(pop w3-display-whitespace-stack)
	(goto-char (point-min))
	(while (re-search-forward hr-regexp nil t)
	  (replace-match "" t t))
	(goto-char (point-min))
	(while (not (eobp))
	  ;; loop invariant: at beginning of uncounted line
	  (end-of-line)
	  (skip-chars-backward " ")
	  (setq retval (cons (current-column)
			     retval))
	  (beginning-of-line 2))
	(if (= (point-min) (point-max))
	    (setq retval 0)
	  (setq retval (apply 'max (cons 0 retval))))
	(delete-region (point-min) (point-max))
	retval))))

(defun w3-display-table-dimensions (node)
  ;; fill-column sets maximum width
  (let (min-vector
	max-vector
	rows cols
	;;(w3-form-elements (and (boundp 'w3-form-elements) w3-form-elements))
	(table-info (assq 'w3-table-info (cadr node)))) 
    
    (if table-info 
	(setq min-vector (nth 1 table-info)
	      max-vector (nth 2 table-info)
	      rows       (nth 3 table-info)
	      cols       (nth 4 table-info))

      (push (cons '*table-autolayout args) w3-display-open-element-stack)
      (let (content
	    cur
	    (table-spans (list nil))	; don't make this '(nil) 
	    ptr
	    col
	    constraints
	    
	    colspan rowspan min max)
	(setq content (nth 2 node))
	(setq rows 0 cols 0)
	(while content
	  (setq cur (pop content))
	  (if (stringp cur)
	      nil
	    (case (car cur)
	      (tr
	       (setq col 0)
	       (setq rows (1+ rows))
	       (setq ptr table-spans)
	       (mapcar
		(function
		 (lambda (td)
		   (setq colspan (string-to-int (or (cdr-safe (assq 'colspan (nth 1 td))) "1"))
			 rowspan (string-to-int (or (cdr-safe (assq 'rowspan (nth 1 td))) "1"))
			 min  (w3-size-of-tree  (nth 2 td) 'min)
			 max  (w3-size-of-tree  (nth 2 td) 'max)
			 )
		   (while (eq (car-safe (car-safe (cdr ptr))) col)
		     (setq col (+ col (cdr (cdr (car (cdr ptr))))))
		     (if (= 0 (decf (car (cdr (car (cdr ptr))))))
			 (pop (cdr ptr))
		       (setq ptr (cdr ptr))))
		   (push (list col colspan min max)
			 constraints)
		   (if (= rowspan 1) nil
		     (push (cons col (cons (1- rowspan) colspan)) (cdr ptr))
		     (setq ptr (cdr ptr)))
		   (setq col (+ col colspan))
		   ))
		(nth 2 cur))
	       (while (cdr ptr)
		 (if (= 0 (decf (car (cdr (car (cdr ptr))))))
		     (pop (cdr ptr))
		   (setq ptr (cdr ptr))))
	       (setq cols (max cols col))
	       )
	      (caption
	       nil)
	      (otherwise
	       (setq content (nth 2 cur)))
	      )
	    )
	  )
	(setq constraints (sort constraints
				(function
				 (lambda (a b)
				   (< (cadr a) (cadr b)))))
	      min-vector (make-vector cols 0)
	      max-vector (make-vector cols 0))
	(let (start end i mincellwidth maxcellwidth)
	  (mapcar (function (lambda (c)
			      (cond ((= (cadr c) 1) 
				     (aset min-vector (car c) 
					   (max (aref min-vector (car c))
						(nth 2 c)))
				     (aset max-vector (car c) 
					   (max (aref max-vector (car c))
						(nth 3 c))))
				    (t 
				     (setq start (car c)
					   end (+ (car c) (cadr c))
					   mincellwidth 0
					   maxcellwidth 0
					   i start)
				     (while (< i end)
				       (setq mincellwidth (+ mincellwidth
							     (aref min-vector i))
					     maxcellwidth (+
							   maxcellwidth
							   (aref max-vector i))
					     i (1+ i)))
				     (setq i start)
				     (if (= mincellwidth 0)
					 ;; if existing width is 0 divide evenly
					 (while (< i end)
					   (aset min-vector i
						 (/ (nth 2 c) (cadr c)))
					   (aset max-vector i
						 (/ (nth 3 c) (cadr c)))
					   (setq i (1+ i)))
				       ;; otherwise weight it by existing widths
				       (while (< i end)
					 (aset min-vector i
					       (max (aref min-vector i)
						    (/ (* (nth 2 c)
							  (aref min-vector i))
						       mincellwidth)))
					 (aset max-vector i
					       (max (aref max-vector i)
						    (/ (* (nth 3 c)
							  (aref max-vector i))
						       maxcellwidth)))
					 (setq i (1+ i))))
				     ))))
		  constraints)))
      (push (cons 'w3-table-info
		  (list min-vector max-vector rows cols))
	    (cadr node))
      (pop w3-display-open-element-stack))
    
    (let (max-width
	  min-width 
	  ret-vector
	  col
	  )
    

      (setq max-width (apply '+ (append max-vector (list cols 1))))
      (setq min-width (apply '+ (append min-vector (list cols 1))))

      ;; the comments in the cond are excerpts from rfc1942 itself
      (cond 
       ;;   1.  The minimum table width is equal to or wider than the available
       ;;       space. In this case, assign the minimum widths and allow the
       ;;       user to scroll horizontally. For conversion to braille, it will
       ;;       be necessary to replace the cells by references to notes
       ;;       containing their full content. By convention these appear
       ;;       before the table.
       ((>= min-width fill-column)
	(setq ret-vector min-vector))
     
       ;;   2.  The maximum table width fits within the available space. In
       ;;       this case, set the columns to their maximum widths.
       ((<= max-width fill-column)
	(setq ret-vector max-vector))
     
       ;;   3.  The maximum width of the table is greater than the available
       ;;       space, but the minimum table width is smaller. In this case,
       ;;       find the difference between the available space and the minimum
       ;;       table width, lets call it W. Lets also call D the difference
       ;;       between maximum and minimum width of the table.
     
       ;;       For each column, let d be the difference between maximum and
       ;;       minimum width of that column. Now set the column's width to the
       ;;       minimum width plus d times W over D. This makes columns with
       ;;       large differences between minimum and maximum widths wider than
       ;;       columns with smaller differences.
       (t
	(setq ret-vector (make-vector cols 0))
	(let ((W (- fill-column min-width))
	      (D (- max-width min-width))
	      d extra)
	  (setq col 0)
	  (while (< col (length ret-vector))
	    (setq d (- (aref max-vector col)
		       (aref min-vector col)))
	    (aset ret-vector col 
		  (+ (aref min-vector col)
		     (/ (* d W) D)))
	    (setq col (1+ col)))
	  (setq extra (- fill-column
			 (apply '+ (append ret-vector
					   (list (length ret-vector) 1))))
		col 0)
	  (while (and (< col (length ret-vector)) (> extra 0))
	    (if (= 1 (- (aref max-vector col) (aref ret-vector col) ))
		(aset ret-vector col (1+ (aref ret-vector col))))
	    (setq extra (1- extra)
		  col (1+ col)))
	  )))
      (list rows cols ret-vector))))

(defvar w3-table-ascii-border-chars
  [?  ?  ?  ?/ ?  ?- ?\\ ?- ?  ?\\ ?| ?| ?/ ?- ?| ?-]
  "Vector of ascii characters to use to draw table borders.
w3-table-unhack-border-chars uses this to restore w3-table-border-chars.")

(defvar w3-table-border-chars w3-table-ascii-border-chars
  "Vector of characters to use to draw table borders.
If you set this you should set w3-table-ascii-border-chars to the same value
so that w3-table-unhack-borders can restore the value if necessary.

A reasonable value is [?  ?  ?  ?/ ?  ?- ?\\\\ ?^ ?  ?\\\\ ?| ?< ?/ ?- ?> ?-]
Though i recommend replacing the ^ with - and the < and > with |")

(defsubst w3-table-lookup-char (l u r b)
  (aref w3-table-border-chars (logior (if l 1 0)
				      (if u 2 0)
				      (if r 4 0)
				      (if b 8 0))))

(defun w3-table-hack-borders nil
  "Try to find the best set of characters to draw table borders with.
I definitely recommend trying this on X. 
On a console, this can trigger some Emacs display bugs.

I haven't tried this on XEmacs or any window-system other than X."
  (interactive)
  (case (device-type)
    (x
     (let ((id (or (and (find-face 'w3-table-hack-x-face)
			(face-id 'w3-table-hack-x-face))
		   (progn
		     (make-face 'w3-table-hack-x-face)
		     (set-face-font 'w3-table-hack-x-face
				    (make-font :family "terminal"))
		     (face-id 'w3-table-hack-x-face)))))
       (if (not (face-differs-from-default-p 'w3-table-hack-x-face))
	   nil
	 (aset standard-display-table 1 (vector (+ (* 256 id) ?l)))
	 (aset standard-display-table 2 (vector (+ (* 256 id) ?q)))
	 (aset standard-display-table 3 (vector (+ (* 256 id) ?k)))
	 (aset standard-display-table 4 (vector (+ (* 256 id) ?t)))
	 (aset standard-display-table 5 (vector (+ (* 256 id) ?n)))
	 (aset standard-display-table 6 (vector (+ (* 256 id) ?u)))
	 (aset standard-display-table 7 (vector (+ (* 256 id) ?m)))
	 (aset standard-display-table 8 (vector (+ (* 256 id) ?x)))
	 (aset standard-display-table 11 (vector (+ (* 256 id) ?j)))
	 (aset standard-display-table 14 (vector (+ (* 256 id) ?v)))
	 (aset standard-display-table 15 (vector (+ (* 256 id) ?w)))
	 (setq w3-table-border-chars [?  ?  ?  11 ?  2 7 14 ?  3 8 6 1 15 4 5])
	 (setq w3-horizontal-rule-char 2))))
    (tty
     (standard-display-g1 1 108)	; ulcorner 
     (standard-display-g1 2 113)	; hline
     (standard-display-g1 3 107)	; urcorner
     (standard-display-g1 4 116)	; leftt
     (standard-display-g1 5 110)	; intersection
     (standard-display-g1 6 117)	; rightt
     (standard-display-g1 7 109)	; llcorner
     (standard-display-g1 8 120)	; vline
     (standard-display-g1 11 106)	; lrcorner
     (standard-display-g1 14 118)	; upt
     (standard-display-g1 15 119)	; downt
     (setq w3-table-border-chars [?  ?  ?  11 ?  2 7 14 ?  3 8 6 1 15 4 5])
     (setq w3-horizontal-rule-char 2))
    (otherwise
     (error "Unknown window-system, can't do any better than ascii borders")))
  )
  
(defun w3-table-unhack-borders nil
  (interactive)
  (w3-table-excise-hack (buffer-list))
  (standard-display-default 1 15)
  (setq w3-table-border-chars w3-table-ascii-border-chars)
  (setq w3-horizontal-rule-char ?-))

(defun w3-table-excise-hack (buffs)
  "Replace hacked characters with ascii characters in buffers BUFFS.
Should be run before restoring w3-table-border-chars to ascii characters."
  (interactive (list (list (current-buffer))))
  (let ((inhibit-read-only t)
	(tr (make-string 16 ? ))
	(i 0))
    (while (< i (length tr))
      (aset tr i i)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (length w3-table-border-chars))
      (if (< (aref w3-table-border-chars i) 16)
	  (aset tr 
		(aref w3-table-border-chars i)
		(aref w3-table-ascii-border-chars i)))
      (setq i (1+ i)))
    (mapcar (function (lambda (buf)
			(save-excursion
			  (set-buffer buf)
			  (if (eq major-mode 'w3-mode)
			      (translate-region (point-min)
						(point-max)
						tr)))))
	    buffs)))

(defun w3-display-table (node)
  (let* ((dimensions (w3-display-table-dimensions node))
	 (num-cols (max (cadr dimensions) 1))
	 (num-rows (max (car dimensions) 1))
	 (column-dimensions (caddr dimensions))
	 (table-width (apply '+ (append column-dimensions (list num-cols 1)))))
    (cond
     ((or (<= (cadr dimensions) 0) (<= (car dimensions) 0))
      ;; We have an invalid table
      nil)
     ((assq '*table-autolayout w3-display-open-element-stack)
      ;; don't bother displaying the table if all we really need is the size
      (progn (insert-char ?T table-width) (insert "\n")))
     (t
      (let* ((tag  (nth 0 node))
	     (args (nth 1 node))
	     (border-node (cdr-safe (assq 'border args)))
	     (border (or w3-display-table-force-borders
			 (and border-node
			      (or (/= 0 (string-to-int border-node))
				  (string= "border" border-node)))))
	     (w3-table-border-chars
	      (if border
		  w3-table-border-chars
		(make-vector (length w3-table-border-chars) ? )))
	     valign align 
	     (content (nth 2 node))
	     (avgwidth (/ (- fill-column num-cols num-cols) num-cols))
	     (formatted-cols (make-vector num-cols nil))
	     (table-rowspans (make-vector num-cols 0))
	     (table-colspans (make-vector num-cols 1))
	     (prev-colspans  (make-vector num-cols 0))
	     (prev-rowspans  (make-vector num-cols 0))
	     (table-colwidth (make-vector num-cols 0))
	     (fill-prefix "")
	     (height nil)
	     (cur-height nil)
	     (cols nil)
	     (rows nil)
	     (row 0)
	     (this-rectangle nil)
	     (i 0)
	     )

	(push (cons tag args) w3-display-open-element-stack)

	(if (memq 'nowrap w3-display-whitespace-stack)
	    (setq fill-prefix "")
	  (case (car w3-display-alignment-stack)
	    (center
	     (w3-set-fill-prefix-length
	      (max 0 (/ (- fill-column table-width) 2))))
	    (right
	     (w3-set-fill-prefix-length
	      (max 0 (- fill-column table-width))))
	    (t
	     (setq fill-prefix ""))))
	(while content
	  (case (caar content)
	    (tr
	     (setq w3-display-css-properties (css-get
					      (nth 0 (car content))
					      (nth 1 (car content))
					      w3-current-stylesheet
					      w3-display-open-element-stack))
	     (setq cols (nth 2 (car content))
		   valign (or (cdr-safe (assq 'valign (nth 1 (car content))))
			      (w3-get-style-info 'vertical-align node))
		   align  (or (cdr-safe (assq 'align  (nth 1 (car content))))
			      (w3-get-style-info 'text-align node))
		   content (cdr content)
		   row (1+ row))
	     (if (and valign (stringp valign))
		 (setq valign (intern (downcase valign))))
	     ;; this is iffy
	     ;;(if align (push (intern (downcase align)) w3-display-alignment-stack))
	     (save-excursion
	       (save-restriction
		 (narrow-to-region (point) (point))
		 (setq fill-column avgwidth
		       inhibit-read-only t
		       w3-last-fill-pos (point-min)
		       i 0)
		 ;; skip over columns that have leftover content
		 (while (and (< i num-cols)
			     (/= 0 (aref table-rowspans i)))
		   (setq i (+ i (max 1 (aref table-colspans i)))))
		 (while cols
		   (let* ((node (car cols))
			  (attributes (nth 1 node))
			  (colspan (string-to-int
				    (or (cdr-safe (assq 'colspan attributes))
					"1")))
			  (rowspan (string-to-int
				    (or (cdr-safe (assq 'rowspan attributes))
					"1")))
			  fill-column column-width
			  (fill-prefix "")
			  (w3-do-incremental-display nil)
			  (indent-tabs-mode nil)
			  c e
			  )

		     (aset table-colspans i colspan)
		     (aset table-rowspans i rowspan)

		     (setq fill-column 0)
		     (setq c i
			   e (+ i colspan))
		     (while (< c e)
		       (setq fill-column (+ fill-column 
					    (aref column-dimensions c)
					    1)
			     c (1+ c)))
		     (setq fill-column (1- fill-column))
		     (aset table-colwidth i fill-column)

		     (setq w3-last-fill-pos (point-min))
		     (push (cons (nth 0 node) (nth 1 node))
			   w3-display-open-element-stack)
		     (w3-display-node node)
		     (setq fill-column (aref table-colwidth i))
		     (if w3-display-table-cut-words-p
			 (w3-display-table-cut))
		     (setq cols (cdr cols))
		     (goto-char (point-min))
		     (skip-chars-forward "\t\n\r")
		     (beginning-of-line)
		     (delete-region (point-min) (point))
		     (goto-char (point-max))
		     (skip-chars-backward " \t\n\r")
		     (delete-region (point) (point-max))
		     (if (>= fill-column (current-column))
			 (insert-char ?  (- fill-column (current-column))))
		     (aset formatted-cols i (extract-rectangle (point-min) (point-max)))
		     (delete-region (point-min) (point-max))
		     (let ((j (1- colspan)))
		       (while (> j 0)
			 (aset table-colspans (+ i j) 0)
			 (setq j (1- j))))		
		     (setq i (+ i colspan))
		     ;; skip over columns that have leftover content
		     (while (and (< i num-cols)
				 (/= 0 (aref table-rowspans i)))
		       (setq i (+ i (max 1 (aref table-colspans i)))))
		     ))

		 ;; finish off the columns
		 (while (< i num-cols)
		   (aset table-colwidth i (aref column-dimensions i))
		   (aset table-colspans i 1)
		   (setq i (1+ i))
		   (while (and (< i num-cols)
			       (/= 0 (aref table-rowspans i)))
		     (setq i (+ i (max 1 (aref table-colspans i))))))

		 ;; on the last row empty any pending rowspans per the rfc
		 (if content nil
		   (fillarray table-rowspans 1)) 

		 ;; Find the tallest rectangle that isn't a rowspanning cell
		 (setq height 0 
		       i 0)
		 (while (< i num-cols)
		   (if (= 1 (aref table-rowspans i))
		       (setq height (max height (length (aref formatted-cols i)))))
		   (setq i (+ i (max 1 (aref table-colspans i)))))

		 ;; Make all rectangles the same height
		 (setq i 0)
		 (while (< i num-cols)
		   (setq this-rectangle (aref formatted-cols i))
		   (if (> height (length this-rectangle))
		       (let ((colspan-fill-line
			      (make-string (aref table-colwidth i) ? )))
			 (case valign
			   ((center middle)
			    (aset formatted-cols i
				  (append (make-list (/ (- height (length this-rectangle)) 2) 
						     colspan-fill-line)
					  this-rectangle)))
			   (bottom
			    (aset formatted-cols i 
				  (append (make-list (- height (length this-rectangle))
						     colspan-fill-line)
					  this-rectangle))))))
		   (setq i (+ i (max 1 (aref table-colspans i)))))))
	     

	     ;; fix broken colspans (this should only matter on illegal tables)
	     (setq i 0)
	     (while (< i num-cols)
	       (if (= (aref table-colspans i) 0)
		   (aset table-colspans i 1))
	       (setq i (+ i (aref table-colspans i))))

	     ;; Insert a separator 
	     (insert fill-prefix)
	     (setq i 0)
	     (let (rflag bflag tflag lflag)
	       (while (< i num-cols)

		 (setq rflag (= (aref prev-rowspans i) 0))
		 (setq bflag (/= (aref table-colspans i) 0))
		 (setq tflag (/= (aref prev-colspans  i) 0))

		 (insert (w3-table-lookup-char lflag tflag rflag bflag))
		 (setq lflag t)
		 (cond ((= (aref prev-rowspans i) 0)
			(insert-char (w3-table-lookup-char t nil t nil) 
				     (aref column-dimensions i))
			(setq i (1+ i)))
		       ((car (aref formatted-cols i))
			(insert (pop (aref formatted-cols i)))
			(setq lflag nil)
			(setq i (+ i (max (aref table-colspans i)
					  (aref prev-colspans  i) 1))))
		       (t
			(insert-char ?  (aref table-colwidth i))
			(setq lflag nil)
			(setq i (+ i (max (aref table-colspans i)
					  (aref prev-colspans  i) 1))))))
	       (insert (w3-table-lookup-char lflag (/= row 1) nil t) "\n"))

	     ;; recalculate height (in case we've shortened a rowspanning cell
	     (setq height 0 
		   i 0)
	     (while (< i num-cols)
	       (if (= 1 (aref table-rowspans i))
		   (setq height (max height (length (aref formatted-cols i)))))
	       (setq i (+ i (max 1 (aref table-colspans i)))))

	     ;; Insert a row back in original buffer
	     (while (> height 0)
	       (insert fill-prefix (w3-table-lookup-char nil t nil t))
	       (setq i 0)
	       (while (< i num-cols)
		 (if (car (aref formatted-cols i))
		     (insert (pop (aref formatted-cols i))) 
		   (insert-char ?  (aref table-colwidth i))) 
		 (insert (w3-table-lookup-char nil t nil t))
		 (setq i (+ i (max (aref table-colspans i) 1))))
	       (insert "\n")
	       ;;(and w3-do-incremental-display (w3-pause))
	       (setq height (1- height)))
	     
	     (setq i 0)
	     (while (< i num-cols)
	       (if (> (aref table-rowspans i) 0)
		   (decf (aref table-rowspans i)))
	       (incf i))
	     
	     (setq prev-rowspans (copy-seq table-rowspans))
	     (setq prev-colspans (copy-seq table-colspans))
	 
	     (and w3-do-incremental-display (w3-pause))

	     )
	    (caption
	     (let ((left (length fill-prefix))
		   (fill-prefix "")
		   (fill-column table-width)
		   (start (point)))
	       (w3-display-node (pop content))
	       (indent-rigidly start (point) left)))
	    (otherwise			
	     (delete-horizontal-space)
	     (setq content (nth 2 (car content))))
	    ))
	(if (= (length column-dimensions) 0) nil
	  (insert fill-prefix)
	  (setq i 0)
	  (let (tflag lflag)
	    (while (< i num-cols)
	      (setq tflag (/= (aref prev-colspans  i) 0))
	      (insert (w3-table-lookup-char lflag tflag t nil))
	      (setq lflag t)
	      (insert-char (w3-table-lookup-char t nil t nil) 
			   (aref column-dimensions i))
	      (setq i (1+ i)))
	    (insert (w3-table-lookup-char t t nil nil) "\n")))
	)
      (pop w3-display-open-element-stack)))))



(defun w3-display-create-unique-id ()
  (let* ((date (current-time-string))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and date (timezone-parse-time (aref dateinfo 3)))))
    (if (and dateinfo timeinfo)
	(concat (aref dateinfo 0)	; Year
		(aref dateinfo 1)	; Month
		(aref dateinfo 2)	; Day
		(aref timeinfo 0)	; Hour
		(aref timeinfo 1)	; Minute 
		(aref timeinfo 2)	; Second
		)
      "HoplesSLYCoNfUSED")))

(defun w3-display-chop-into-table (node cols)
  ;; Chop the content of 'node' up into 'cols' columns suitable for inclusion
  ;; as the content of a table
  (let ((content (nth 2 node))
	(items nil)
	(rows nil))
    (setq cols (max cols 1))
    (while content
      (push (list 'td nil (list (pop content))) items)
      (if (= (length items) cols)
	  (setq rows (cons (nreverse items) rows)
		items nil)))
    (if items				; Store any leftovers
	(setq rows (cons (nreverse items) rows)
	      items nil))
    (while rows
      (push (list 'tr nil (pop rows)) items))
    items))

(defun w3-display-normalize-form-info (args)
  (let* ((plist (alist-to-plist args))
	 (type (intern (downcase
			(or (plist-get plist 'type) "text"))))
	 (name (plist-get plist 'name))
	 (value (or (plist-get plist 'value) ""))
	 (size (if (plist-get plist 'size)
		   (string-to-int (plist-get plist 'size))))
	 (maxlength (if (plist-get plist 'maxlength)
			(string-to-int
			 (plist-get plist 'maxlength))))
	 (default value)
	 (checked (assq 'checked args)))
    (if (memq type '(checkbox radio)) (setq default checked))
    (if (and (eq type 'checkbox) (string= value ""))
	(setq value "on"))
    (if (and (not (memq type '(submit reset button)))
	     (not name))
	(setq name (symbol-name type)))
    (while (and name (string-match "[\r\n]+" name))
      (setq name (concat (substring name 0 (match-beginning 0))
			 (substring name (match-end 0) nil))))
    (setq plist (plist-put plist 'type type)
	  plist (plist-put plist 'name name)
	  plist (plist-put plist 'value value)
	  plist (plist-put plist 'size size)
	  plist (plist-put plist 'default default)
	  plist (plist-put plist 'internal-form-number w3-current-form-number)
	  plist (plist-put plist 'action w3-display-form-id)
	  plist (plist-put plist 'maxlength maxlength))
    plist))

(defun w3-display-node (node &optional nofaces)
  (let (
	(content-stack (list (list node)))
	(right-margin-stack (list fill-column))
	(left-margin-stack (list 0))
	node
	insert-before
	insert-after
	tag
	args
	content
	hyperlink-info
	break-style
	cur
	id
	class
	)
    (while content-stack
      (setq content (pop content-stack))
      (pop w3-active-faces)
      (pop w3-active-voices)
      (case (car (pop w3-display-open-element-stack))
	;; Any weird, post-display-of-content stuff for specific tags
	;; goes here.   Couldn't think of any better way to do this when we
	;; are iterative.  *sigh*
	(a
	 (if (not hyperlink-info)
	     nil
	   (add-text-properties (car hyperlink-info) (point)
				(list
				 'mouse-face 'highlight
				 'duplicable t
				 'start-open t
				 'end-open t
				 'rear-nonsticky t
				 'help-echo 'w3-balloon-help-callback
				 'balloon-help 'w3-balloon-help-callback))
	   (fillin-text-property (car hyperlink-info) (point)
				 'button 'button (cadr hyperlink-info))
	   (widget-put (cadr hyperlink-info) :to (set-marker
						  (make-marker) (point))))
	 (setq hyperlink-info nil))
	((ol ul dl dir menu)
	 (pop w3-display-list-stack))
	(otherwise
	 nil))
      (if (car insert-after)
	  (w3-handle-string-content (car insert-after)))
      (pop insert-after)
      (w3-display-handle-end-break)
      (w3-pop-all-face-info)
      ;; Handle the element's content
      (while content
	(if (stringp (car content))
	    (w3-handle-string-content (pop content))
	  (setq node (pop content)
		tag (nth 0 node)
		args (nth 1 node)
		id (or (w3-get-attribute 'name)
		       (w3-get-attribute 'id))
		)
	  ;; This little bit of magic takes care of inline styles.
	  ;; Evil Evil Evil, but it appears to work.
	  (if (w3-get-attribute 'style)
	      (let ((unique-id (or (w3-get-attribute 'id)
				   (w3-display-create-unique-id)))
		    (sheet "")
		    (class (assq 'class args)))
		(setq sheet (format "%s.%s { %s }\n" tag unique-id
				    (w3-get-attribute 'style)))
		(if class
		    (setcdr class (cons unique-id (cdr class)))
		  (setf (nth 1 node) (cons (cons 'class (list unique-id))
					   (nth 1 node))))
		(setf (nth 1 node) (cons (cons 'id unique-id) (nth 1 node)))
		(w3-handle-style (list 'data sheet
				       'notation "css"))))
	  (setq w3-display-css-properties (css-get
					   (nth 0 node)
					   (nth 1 node)
					   w3-current-stylesheet
					   w3-display-open-element-stack))
	  (if nofaces
	      nil
	    (push (w3-face-for-element node) w3-active-faces)
	    (push (w3-voice-for-element node) w3-active-voices))
	  (push (w3-get-style-info 'display node) break-style)
	  (push (w3-get-style-info 'insert-after node) insert-after)
	  (setq insert-before (w3-get-style-info 'insert-before node))
	  (w3-display-handle-break)
	  (if (w3-node-visible-p)
	      nil
	    (setq insert-before nil
		  tag '*invisible)
	    (setcar insert-after nil))
	  (if insert-before
	      (w3-handle-string-content insert-before))
	  (setq insert-before nil)
	  (if id
	      (setq w3-id-positions (cons
				     (cons (intern id)
					   (set-marker (make-marker)
						       (point-max)))
				     w3-id-positions)))
	  (case tag
	    (a				; Hyperlinks
	     (let* (
		    (title (w3-get-attribute 'title))
		    (name (or (w3-get-attribute 'id)
			      (w3-get-attribute 'name)))
		    (btdt nil)
		    class
		    (before nil)
		    (after nil)
		    (face nil)
		    (voice nil)
		    (st nil))
	       (setq st (point)
		     hyperlink-info (list
				     st
				     (append 
				      (list 'link :args nil
					    :value "" :tag ""
					    :action 'w3-follow-hyperlink
					    :from
					    (set-marker (make-marker) st)
					    :help-echo 'w3-widget-echo
					    )
				      (alist-to-plist args))))
	       (w3-handle-content node)
	       )
	     )
	    ((ol ul dl menu)
	     (push 0 w3-display-list-stack)
	     (w3-handle-content node))
	    (dir
	     (push 0 w3-display-list-stack)
	     (setq node
		   (list tag args
			 (list
			  (list 'table nil
				(w3-display-chop-into-table node 3)))))
	     (w3-handle-content node))
	    (multicol
	     (setq node (list tag args
			      (list
			       (list 'table nil
				     (w3-display-chop-into-table node 2)))))
	     (w3-handle-content node))
	    (img			; inlined image
	     (w3-handle-image)
	     (w3-handle-empty-tag))
	    (script			; Scripts
	     (w3-handle-empty-tag))
	    ((embed object)		; Embedded images/content
	     (w3-handle-content node)
	     )
	    (hr				; Cause line break & insert rule
	     (let* ((perc (or (w3-get-attribute 'width)
			      (w3-get-style-info 'width node)
			      "100%"))
		    (rule nil)
		    (width nil))
	       (setq perc (/ (min (string-to-int perc) 100) 100.0)
		     width (* fill-column perc)
		     rule (make-string (max (truncate width) 0)
				       w3-horizontal-rule-char)
		     node (list 'hr nil (list rule)))
	       (w3-handle-content node)))
	    (map			; Client side imagemaps
	     (let ((name (or (w3-get-attribute 'name)
			     (w3-get-attribute 'id)
			     "unnamed"))
		   (areas
		    (mapcar
		     (function
		      (lambda (node)
			(let* ((args (nth 1 node))
			       (type (downcase (or
						(w3-get-attribute 'shape)
						"rect")))
			       (coords (w3-decode-area-coords
					(or (cdr-safe
					     (assq 'coords args)) "")))
			       (alt (w3-get-attribute 'alt))
			       (href (if (assq 'nohref args)
					 t
				       (or (w3-get-attribute 'src)
					   (w3-get-attribute 'href))))
			       )
			  (vector type coords href alt))
			)
		      )
		     (nth 2 node))))
	       (setq w3-imagemaps (cons (cons name areas) w3-imagemaps)))
	     (w3-handle-empty-tag)
	     )
	    (note
	     ;; Ewwwwhhh.  Looks gross, but it works.  This converts a
	     ;; <note> into a two-cell table, so that things look all
	     ;; pretty.
	     (setq node
		   (list 'note nil
			 (list
			  (list 'table nil
				(list
				 (list 'tr nil
				       (list
					(list 'td (list 'align 'right)
					      (list
					       (concat
						(or (w3-get-attribute 'role)
						    "CAUTION") ":")))
					(list 'td nil
					      (nth 2 node)))))))))
	     (w3-handle-content node)
	     )
	    (table
	     (w3-display-table node)
	     (setq w3-last-fill-pos (point))
	     (w3-handle-empty-tag)
	     )
	    (isindex
	     (let ((prompt (or (w3-get-attribute 'prompt)
			       "Search on (+ separates keywords): "))
		   action node)
	       (setq action (or (w3-get-attribute 'src)
				(w3-get-attribute 'href)
				(url-view-url t)))
	       (if (and prompt (string-match "[^: \t-]+$" prompt))
		   (setq prompt (concat prompt ": ")))
	       (setq node
		     (list 'isindex nil
			   (list
			    (list 'hr nil nil)
			    (list 'form
				  (list (cons 'action action)
					(cons 'enctype
					      "application/x-w3-isindex")
					(cons 'method "get"))
				  (list
				   prompt
				   (list 'input
					 (list (cons 'type "text")
					       (cons 'name "isindex"))))))))
	       (w3-handle-content node)
	       (setq w3-current-isindex (cons action prompt)))
	     )
	    (*document
	     (let ((info (mapcar (lambda (x) (cons x (symbol-value x)))
				 w3-persistent-variables)))
	       (if (not w3-display-same-buffer)
		   (set-buffer (generate-new-buffer "Untitled")))
	       (setq w3-current-form-number 0
		     w3-display-open-element-stack nil
		     w3-last-fill-pos (point-min)
		     fill-column (min (- (or w3-strict-width (window-width))
					 w3-right-margin)
				      (or w3-maximum-line-length
					  (window-width))))
	       (switch-to-buffer (current-buffer))
	       (buffer-disable-undo (current-buffer))
	       (mapcar (function (lambda (x) (set (car x) (cdr x)))) info)
	       ;; ACK!  We don't like filladapt mode!
	       (set (make-local-variable 'filladapt-mode) nil)
	       (set (make-local-variable 'adaptive-fill-mode) nil)
	       (set (make-local-variable 'voice-lock-mode) t)
	       (setq w3-current-stylesheet (css-copy-stylesheet
					    w3-user-stylesheet)
		     w3-last-fill-pos (point)
		     fill-column (min (- (or w3-strict-width (window-width))
					 w3-right-margin)
				      (or w3-maximum-line-length
					  (window-width)))
		     fill-prefix "")
	       (set (make-local-variable 'inhibit-read-only) t))
	     (w3-handle-content node)
	     )
	    (*invisible
	     (w3-handle-empty-tag))
	    (meta
	     (let* ((equiv (cdr-safe (assq 'http-equiv args)))
		    (value (w3-get-attribute 'content))
		    (name  (w3-get-attribute 'name))
		    (node  (and equiv (assoc (setq equiv (downcase equiv))
					     url-current-mime-headers))))
	       (if equiv
		   (setq url-current-mime-headers (cons
						   (cons equiv value)
						   url-current-mime-headers)))
	       (if name
		   (setq w3-current-metainfo (cons
					      (cons name value)
					      w3-current-metainfo)))

	       ;; Special-case the Set-Cookie header
	       (if (and equiv (string= (downcase equiv) "set-cookie"))
		   (url-cookie-handle-set-cookie value))
	       ;; Special-case the refresh header
	       (if (and equiv (string= (downcase equiv) "refresh"))
		   (url-handle-refresh-header value)))
	     (w3-handle-empty-tag)
	     )
	    (link
	     ;; This doesn't handle blank-separated values per the RFC.
	     (w3-parse-link args)
	     (w3-handle-empty-tag))
	    (title
	     (let ((potential-title "")
		   (content (nth 2 node)))
	       (while content
		 (setq potential-title (concat potential-title (car content))
		       content (cdr content)))
	       (setq potential-title (w3-normalize-spaces potential-title))
	       (if (or w3-display-same-buffer
		       (string-match "^[ \t]*$" potential-title))
		   nil
		 (rename-buffer (generate-new-buffer-name
				 (w3-fix-spaces potential-title)))))
	     (w3-handle-empty-tag))
	    (form
	     (setq w3-current-form-number (1+ w3-current-form-number))
	     (let* (
		    (action (w3-get-attribute 'action))
		    (url nil))
	       (if (not action)
		   (setq args (cons (cons 'action (url-view-url t)) args)))
	       (setq w3-display-form-id (cons
					 (cons 'form-number
					       w3-current-form-number)
					 args))
	       (w3-handle-content node)))
	    (keygen
	     (w3-form-add-element 'keygen
				  (or (w3-get-attribute 'name)
				      (w3-get-attribute 'id)
				      "keygen")
				  nil	; value
				  nil	; size
				  nil	; maxlength
				  nil   ; default
				  w3-display-form-id ; action
				  nil	; options
				  w3-current-form-number
				  (w3-get-attribute 'id) ; id
				  nil	; checked
				  (car w3-active-faces)))
	    (input
	     (w3-form-add-element
	      (w3-display-normalize-form-info args)
	      (car w3-active-faces))
	     (w3-handle-empty-tag)
	     )
	    (select
	     (let* ((plist (w3-display-normalize-form-info args))
		    (tmp nil)
		    (multiple (assq 'multiple args))
		    (value nil)
		    (name (plist-get plist 'name))
		    (options (mapcar
			      (function
			       (lambda (n)
				 (setq tmp (w3-normalize-spaces
					    (apply 'concat (nth 2 n)))
				       tmp (cons tmp
						 (or
						  (cdr-safe
						   (assq 'value (nth 1 n)))
						  tmp)))
				 (if (assq 'selected (nth 1 n))
				     (setq value (car tmp)))
				 tmp))
			      (nth 2 node))))
	       (if (not value)
		   (setq value (caar options)))
	       (setq plist (plist-put plist 'value value))
	       (if multiple
		   (progn
		     (setq options
			   (mapcar
			    (function
			     (lambda (opt)
			       (list 'div nil
				     (list
				      (list 'input
					    (list (cons 'name name)
						  (cons 'type "checkbox")
						  (cons 'value (car opt))))
				      " " (car opt) (list 'br nil nil)))))
			    options))
		     (setq node (list 'p nil options))
		     (w3-handle-content node))
		 (setq plist (plist-put plist 'type 'option)
		       plist (plist-put plist 'options options))
		 (w3-form-add-element plist (car w3-active-faces))
		 ;; This should really not be necessary, but some versions
		 ;; of the widget library leave point _BEFORE_ the menu
		 ;; widget instead of after.
		 (goto-char (point-max))
		 (w3-handle-empty-tag))))
	    (textarea
	     (let* ((plist (w3-display-normalize-form-info args))
		    (value (w3-normalize-spaces
			    (apply 'concat (nth 2 node)))))
	       (setq plist (plist-put plist 'type 'multiline)
		     plist (plist-put plist 'value value))
	       (w3-form-add-element plist (car w3-active-faces)))
	     (w3-handle-empty-tag)
	     )
	    (style
	     (w3-handle-style (alist-to-plist
			       (cons (cons 'data (apply 'concat (nth 2 node)))
				     (nth 1 node))))
	     (w3-handle-empty-tag))
	    ;; Emacs-W3 stuff that cannot be expressed in a stylesheet
	    (pinhead
	     ;; This check is so that we don't screw up table auto-layout
	     ;; by changing our text midway through the parse/layout/display
	     ;; steps.
	     (if (nth 2 node)
		 nil
	       (setcar (cddr node)
		       (list
			(if (fboundp 'yow)
			    (yow)
			  "AIEEEEE!  I am having an UNDULATING EXPERIENCE!"))))
	     (w3-handle-content node))
	    (flame
	     (if (nth 2 node)
		 nil
	       (setcar
		(cddr node)
		(list
		 (condition-case ()
		     (concat
		      (sentence-ify
		       (string-ify
			(append-suffixes-hack (flatten (*flame))))))
		   (error
		    "You know, everything is really a graphics editor.")))))
	     (w3-handle-content node))
	    (cookie
	     (if (nth 2 node)
		 nil
	       (setcar
		(cddr node)
		(list
		 (w3-display-get-cookie args))))
	     (w3-handle-content node))
	    ;; Generic formatting - all things that can be fully specified
	    ;; by a CSS stylesheet.
	    (otherwise
	     (w3-handle-content node))
	    )				; case tag
	  )				; stringp content
	)				; while content
      )					; while content-stack
    )
  )

(defun w3-draw-tree (tree)
  ;; The main entry point - wow complicated
  (setq w3-current-stylesheet w3-user-stylesheet)
  (while tree
    (w3-display-node (car tree))
    (setq tree (cdr tree)))
  (w3-display-fix-widgets)
  (w3-form-resurrect-widgets))

(defun time-display (&optional tree)
  ;; Return the # of seconds it took to draw 'tree'
  (let ((st (nth 1 (current-time)))
	(nd nil))
    (w3-draw-tree (or tree w3-last-parse-tree))
    (setq nd (nth 1 (current-time)))
    (- nd st)))


(defsubst w3-finish-drawing ()
  (if (and (boundp 'w3-image-widgets-waiting) w3-image-widgets-waiting)
      (let (url glyph widget)
	(while w3-image-widgets-waiting
	  (setq widget (car w3-image-widgets-waiting)
		w3-image-widgets-waiting (cdr w3-image-widgets-waiting)
		url (widget-get widget 'src)
		glyph (cdr-safe (assoc url w3-graphics-list)))
	  (widget-value-set widget glyph)))
    ;;(w3-handle-annotations)
    ;;(w3-handle-headers)
    )
  )

(defun w3-region (st nd)
  (if (not w3-setup-done) (w3-do-setup))
  (let* ((source (buffer-substring st nd))
	 (w3-display-same-buffer t)
	 (parse nil))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-region*"))
      (erase-buffer)
      (insert source)
      (setq parse (w3-parse-buffer (current-buffer))))
    (narrow-to-region st nd)
    (delete-region (point-min) (point-max))
    (w3-draw-tree parse)
    (w3-finish-drawing)))

(defun w3-refresh-buffer ()
  (interactive)
  (let ((parse w3-current-parse)
	(inhibit-read-only t)
	(w3-display-same-buffer t))
    (if (not parse)
	(error "Could not find the parse tree for this buffer.  EEEEK!"))
    (erase-buffer)
    (w3-draw-tree parse)
    (w3-finish-drawing)
    (w3-mode)
    (set-buffer-modified-p nil)))

(defun w3-prepare-buffer (&rest args)
  ;; The text/html viewer - does all the drawing and displaying of the buffer
  ;; that is necessary to go from raw HTML to a good presentation.
  (let* ((source (buffer-string))
	 (source-buf (current-buffer))
	 (parse (w3-parse-buffer source-buf)))
    (set-buffer-modified-p nil)
    (w3-draw-tree parse)
    (kill-buffer source-buf)
    (set-buffer-modified-p nil)
    (setq w3-current-source source
	  w3-current-parse parse)
    (w3-finish-drawing)
    (w3-mode)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (if url-keep-history
	(let ((url (url-view-url t)))
	  (if (not url-history-list)
	      (setq url-history-list (make-hash-table :size 131 :test 'equal)))
	  (cl-puthash url (buffer-name) url-history-list)
	  (if (fboundp 'w3-shuffle-history-menu)
	      (w3-shuffle-history-menu)))))
  )

(provide 'w3-display)
