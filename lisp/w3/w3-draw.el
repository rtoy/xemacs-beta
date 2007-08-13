;;; w3-draw.el --- Emacs-W3 drawing functions for new display engine
;; Author: wmperry
;; Created: 1996/08/25 17:12:32
;; Version: 1.17
;; Keywords: faces, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
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
;;; This function will take a stream of HTML from w3-parse-buffer
;;; and draw it out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3-vars)
(require 'w3-imap)
(require 'w3-widget)
(require 'widget)
(require 'cl)

(if (featurep 'mule) (fset 'string-width 'length))

(defmacro w3-get-state (tag)
  (or (symbolp tag)
      (error "Bad argument: %s" tag))
  (let ((index (length (memq tag w3-state-locator-variable))))
    (` (aref w3-state-vector (, index)))))
(put 'w3-get-state 'edebug-form-spec '(symbolp))

(defmacro w3-put-state (tag val)
  (or (symbolp tag)
      (error "Bad argument: %s" tag))
  (let ((index (length (memq tag w3-state-locator-variable))))
    (` (aset w3-state-vector (, index) (, val)))))
(put 'w3-put-state 'edebug-form-spec '(symbolp form))

(defsubst w3-push-alignment (align)
  (if align
      (w3-put-state :align (cons (cons tag align) (w3-get-state :align)))))

(defsubst w3-pop-alignment ()
  (let ((flubber (memq (assq tag (w3-get-state :align))
		       (w3-get-state :align))))
    (cond
     ((null flubber) nil)
     ((cdr flubber)
      (w3-put-state :align (cdr flubber)))
     (t (w3-put-state :align nil)))))

(defsubst w3-current-alignment ()
  (cdr-safe (car-safe (w3-get-state :align))))

(defconst w3-fill-prefixes-vector
  (let ((len 0)
        (prefix-vector (make-vector 80 nil)))
    (while (< len 80)
      (aset prefix-vector len (make-string len ? ))
      (setq len (1+ len)))
    prefix-vector))

(defsubst w3-set-fill-prefix-length (len)
  (setq fill-prefix (if (< len (- (or w3-strict-width (window-width)) 4))
			(if (< len 80)
			    (aref w3-fill-prefixes-vector len)
			  (make-string len ? ))
		      (url-warn
		       'html
		       "Runaway indentation!  Too deep for window width!")
		      fill-prefix)))

(defsubst w3-get-default-style-info (info)
  (and w3-current-stylesheet
       (or
	;; Check for tag/id|name first!
	(cdr-safe (assq info
			(cdr-safe
			 (assoc (or (cdr-safe (assq 'id args))
				    (cdr-safe (assq 'name args)))
				(cdr-safe
				 (assq tag w3-current-stylesheet))))))

	;; Check for tag/class next
	(cdr-safe (assq info
			(cdr-safe
			 (assoc (cdr-safe (assq 'class args))
				(cdr-safe
				 (assq tag w3-current-stylesheet))))))

	;; Then for global stuff with 'class'
	(cdr-safe (assq info
			(cdr-safe
			 (assoc (cdr-safe (assq 'class args))
				(cdr-safe
				 (assq 'doc w3-current-stylesheet))))))
     
	;; Fall back on the default styles for just this tag.
	(cdr-safe (assq info
			(cdr-safe
			 (assq 'internal
			       (cdr-safe
				(assq tag w3-current-stylesheet)))))))))

(defsubst w3-normalize-color (color)
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

(defun w3-pause ()
  (cond
   (w3-running-FSF19 (sit-for 0))
   (w3-running-xemacs
    (if (and (not (sit-for 0)) (input-pending-p))
	(condition-case ()
	    (dispatch-event (next-command-event))
	  (error nil))))
   (t (sit-for 0))))

(defvar w3-end-tags
  '((/ul   . ul)
    (/lit  . lit)
    (/li   . li)
    (/h1   . h1)
    (/h2   . h2)
    (/h3   . h3)
    (/h4   . h4)
    (/h5   . h5)
    (/h6   . h6)
    (/font0 . font0)
    (/font1 . font1)
    (/font2 . font2)
    (/font3 . font3)
    (/font4 . font4)
    (/font5 . font5)
    (/font6 . font6)
    (/font7 . font7)
    (/ol   . ol)
    (/dl   . dl)
    (/menu . menu)
    (/dir  . dir)
    (/a    . a)))

(defvar w3-face-cache nil
  "Cache for w3-face-for-element")

(defsubst w3-voice-for-element ()
  (let ((temporary-voice (w3-get-default-style-info 'voice-spec)))
    (and temporary-voice (cons tag temporary-voice))))

(defsubst w3-face-for-element ()
  (let* ((font-spec  (w3-get-default-style-info 'font-spec))
	 (foreground (w3-get-default-style-info 'color))
	 (background (w3-get-default-style-info 'background))
	 ;;(pixmap     (w3-get-default-style-info 'pixmap))
	 (descr (list font-spec foreground background))
	 (face (cdr-safe (assoc descr w3-face-cache))))
    (if (or face (not (or foreground background font-spec)))
	nil				; Do nothing, we got it already
      (setq face (intern (format "%s" descr)))
      (cond
       ((not (fboundp 'make-face)) nil)	; Do nothing
       ((and (fboundp 'face-property)	; XEmacs 19.14
	     (not (get 'face-property 'sysdep-defined-this)))
	(setq face (make-face face
			      "An Emacs-W3 face... don't edit by hand." t)))
       (t (make-face face)))

      (and font-spec (set-face-font face font-spec))
      (and foreground (set-face-foreground face foreground))
      (and background (set-face-background face background))
      ;(set-face-background-pixmap face pixmap)
      (setq w3-face-cache (cons (cons descr face) w3-face-cache)))
    (cons tag face)))

(defun w3-handle-single-tag (tag &optional args)
  (save-excursion
    (and w3-draw-buffer (set-buffer w3-draw-buffer))
    (let ((opos (point))
	  (id (and (listp args)
		   (or (cdr-safe (assq 'name args))
		       (cdr-safe (assq 'id args))))))
      
      ;; This allows _ANY_ tag, whether it is known or not, to be
      ;; the target of a # reference in a URL
      (if id
	  (progn
	    (setq w3-id-positions (cons
				   (cons (intern id)
					 (set-marker (make-marker)
						     (point-max)))
				   w3-id-positions))))

      (if (and (listp args) (cdr-safe (assq 'style args)))
	  (let ((unique-id (or id (url-create-unique-id)))
		(sheet ""))
	    (setq sheet (format "%s.%s { %s }\n" tag unique-id
				(cdr-safe (assq 'style args)))
		  args (cons (cons 'id unique-id) args))
	    
	    (w3-handle-style (list (cons 'data sheet)
				   (cons 'notation "css")))))
      (goto-char (point-max))
      (if (and (w3-get-state :next-break)
	       (not (memq tag
			  '(p h1 h2 h3 h4 h5 h6 ol ul dl menu dir pre))))
	  (w3-handle-p))
      (w3-put-state :next-break nil)
      (setq w3-current-formatter (get tag 'w3-formatter))
      (cond
       ((eq 'w3-handle-text w3-current-formatter)
	(w3-handle-text args))
       (t
	(let ((data-before nil)
	      (data-after nil))
	  (if (and (not (eq tag 'text)) w3-current-stylesheet)
	      (progn
		(setq data-before (w3-get-default-style-info
				   'insert.before))
		(let ((tag (cdr-safe (assq tag w3-end-tags))))
		  (setq data-after (and tag
					(w3-get-default-style-info
					 'insert.after))))))
	  (if data-before (w3-handle-text data-before))
	  (setq w3-current-formatter (get tag 'w3-formatter))
	  (cond
	   ((eq w3-current-formatter 'ack) nil)
	   ((null w3-current-formatter) (w3-handle-unknown-tag tag args))
	   (t (funcall w3-current-formatter args)))
	  (if data-after (w3-handle-text data-after)))))
      (if (not (eq tag 'text))
	  (setq w3-last-tag tag))
      (goto-char opos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up basic fonts/stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		      
(defun w3-init-state ()
  ;; Reset the state of an HTML drawing buffer
  (setq w3-state-vector (copy-sequence w3-state-vector))
  (setq w3-current-stylesheet (copy-tree w3-user-stylesheet))
  (let* ((tag 'html)
	 (args nil)
	 (face (cdr (w3-face-for-element))))
    (if (not face)
	(setq tag 'body
	      face (cdr (w3-face-for-element))))
    (and face
	 (if (not (fboundp 'valid-specifier-locale-p))
	     nil
	   (w3-my-safe-copy-face face 'default (current-buffer)))))
  (setq w3-form-labels nil)
  (make-local-variable 'w3-image-widgets-waiting)
  (make-local-variable 'w3-active-voices)
  (make-local-variable 'inhibit-read-only)
  (setq w3-image-widgets-waiting nil
	inhibit-read-only t)
  (if (not (get 'w3-state 'init)) (w3-draw-setup))
  (fillarray w3-state-vector 0)
  (w3-put-state :bogus nil)		; Make all fake ones return nil
  (w3-put-state :text-mangler nil)	; Any text mangling routine 
  (w3-put-state :next-break nil)	; Next item needs a paragraph break
  (w3-put-state :background nil)	; Netscapism - gag
  (w3-put-state :table nil)		; Table args
  (w3-put-state :figdata nil)		; Data for <fig> tag
  (w3-put-state :figalt nil)		; Alt data for <fig> tag
  (w3-put-state :pre-start nil)		; Where current <pre> seg starts
  (w3-put-state :zone nil)		; Zone of current href?
  (w3-put-state :center nil)		; netscape tag
  (w3-put-state :select nil)		; Data for current select field
  (w3-put-state :options nil)		; Options in current select field
  (w3-put-state :nofill nil)		; non-nil if in pre or xmp
  (w3-put-state :nowrap nil)		; non-nil if in <p nowrap>
  (w3-put-state :href nil)		; Current link destination
  (w3-put-state :name nil)		; Current link ID tag
  (w3-put-state :image nil)		; Current image destination
  (w3-put-state :form nil)		; Current form information
  (w3-put-state :optarg nil)		; Option arguments
  (w3-put-state :w3-graphic nil)	; Image stuff for non-xemacs
  (w3-put-state :lists '())		; Types of list currently in.
  (w3-put-state :align nil)		; Current alignment of paragraphs
  (w3-put-state :title nil)		; Whether we can have a title or not
  (w3-put-state :seen-this-url nil)	; whether we have seen this url or not
  (w3-put-state :needspace 'never)	; Spacing info
  (setq w3-active-faces nil)		; Face attributes to use
  (setq w3-active-voices nil)		; voice attributes to use
  )

(defun w3-draw-setup ()
  (put 'w3-state 'init t)
  (w3-init-state))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping HTML tags to functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'lit 'w3-formatter 'w3-handle-pre)
(put '/lit 'w3-formatter 'w3-handle-/pre)
(put 'li 'w3-formatter 'w3-handle-list-item)
(put 'ul 'w3-formatter 'w3-handle-list-opening)
(put 'ol 'w3-formatter 'w3-handle-list-opening)
(put 'dl 'w3-formatter 'w3-handle-list-opening)
(put '/dl 'w3-formatter 'w3-handle-list-ending)
(put '/ul 'w3-formatter 'w3-handle-list-ending)
(put '/ol 'w3-formatter 'w3-handle-list-ending)
(put 'menu 'w3-formatter 'w3-handle-list-opening)
(put '/menu 'w3-formatter 'w3-handle-list-ending)
(put 'dir 'w3-formatter 'w3-handle-list-opening)
(put '/dir 'w3-formatter 'w3-handle-list-ending)
(put 'dt 'w3-formatter 'w3-handle-table-term)
(put 'dd 'w3-formatter 'w3-handle-table-definition)
(put 'a 'w3-formatter 'w3-handle-hyperlink)
(put '/a 'w3-formatter 'w3-handle-hyperlink-end)
(put 'h1 'w3-formatter 'w3-handle-header)
(put 'h2 'w3-formatter 'w3-handle-header)
(put 'h3 'w3-formatter 'w3-handle-header)
(put 'h4 'w3-formatter 'w3-handle-header)
(put 'h5 'w3-formatter 'w3-handle-header)
(put 'h6 'w3-formatter 'w3-handle-header)
(put '/h1 'w3-formatter 'w3-handle-header-end)
(put '/h2 'w3-formatter 'w3-handle-header-end)
(put '/h3 'w3-formatter 'w3-handle-header-end)
(put '/h4 'w3-formatter 'w3-handle-header-end)
(put '/h5 'w3-formatter 'w3-handle-header-end)
(put '/h6 'w3-formatter 'w3-handle-header-end)
(put 'img 'w3-formatter 'w3-handle-image)
(put 'kill_sgml 'w3-formatter 'w3-handle-kill-sgml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main drawing routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-unknown-tag (tag args)
  ;; A generic formatter for an unkown HTML tag.  This will only be
  ;; called if a formatter was not found in TAGs property list.
  ;; If a function named `w3-handle-TAG' is defined, then it will be
  ;; stored in TAGs property list, so it will be found next time
  ;; the tag is run across.

  (let ((handler (intern-soft (concat "w3-handle-" (symbol-name tag))))
	(end-tag-p (= (string-to-char (symbol-name tag)) ?/)))

    ;; This stores the info in w3-end-tags for future use by the display
    ;; engine.
    (if end-tag-p
	(setq w3-end-tags (cons (cons tag
				      (intern (substring (symbol-name tag)
							 1)))
				w3-end-tags)))

    ;; For proper use of stylesheets, if no tag is found, then we should
    ;; at least call w3-handle-emphasis
    (cond
     ((and handler (fboundp handler))
      (put tag 'w3-formatter handler)
      (funcall handler args))
     (end-tag-p
      (put tag 'w3-formatter 'w3-handle-emphasis-end)
      (w3-handle-emphasis-end args))
     (t 
      (put tag 'w3-formatter 'w3-handle-emphasis)
      (w3-handle-emphasis args)))))

(defun w3-handle-text (&optional args)
  ;; This is the main workhorse of the display engine.
  ;; It will figure out how a chunk of text should be displayed and
  ;; put all the necessary extents/overlays/regions around it."
  (or args (error "Impossible"))
  (if (string= args "")
      (w3-put-state :needspace nil)
    (let ((st (point))
	  (mangler (w3-get-state :text-mangler))
	  (sym nil))
      (insert args)
      ;;(goto-char st)
      (cond ((w3-get-state :nofill)
	     (goto-char st)
	     (if (not (search-forward "\n" nil t))
		 (subst-char-in-region st (point-max) ?\r ?\n)
	       (subst-char-in-region st (point-max) ?\r ?  ))
	     (goto-char (point-max)))
            (t
	     (goto-char st)
             (while (re-search-forward
                     " [ \t\n\r]+\\|[\t\n\r][ \t\n\r]*"
                     nil 'move)
               (replace-match " "))
             (goto-char st)
             (if (and (= ?  (following-char))
                      (or (bolp)
                          (eq 'never (w3-get-state :needspace))))
                 (delete-char 1))
             (goto-char (point-max))))
      (and mangler w3-delimit-emphasis
	   (fboundp mangler) (funcall mangler st (point)))
      (let ((faces nil)
	    (todo w3-active-faces)
	    (voices w3-active-voices)
	    (val nil)
	    (cur nil))
	(while todo
	  (setq cur (car todo)
		todo (cdr todo))
	  (cond
	   ((symbolp cur)
	    nil)
	   ((listp (cdr-safe cur))
	    (let ((x (cdr cur)))
	      (while x
		(if (not (memq (car x) faces))
		    (setq faces (cons (car x) faces)))
		(setq x (cdr x)))))
	   ((and (consp cur) (not (memq (cdr cur) faces)))
	    (setq faces (cons (cdr cur) faces)))
	   (t nil)))
	(add-text-properties st (point) (list 'face faces))
	(if (car voices)
	    (add-text-properties st (point) (list 'personality (cdar voices))))
	)
      (if (not (memq (char-after (1- (point))) '(?  ?.)))
	  (w3-put-state :needspace t))
      )))

(defun w3-handle-plaintext (&optional args)
  (let ((x (w3-get-state :nofill)))
    (w3-put-state :nofill t)
    (and args (cdr-safe (assq 'data args))
	 (w3-handle-text (cdr-safe (assq 'data args))))
    (setq w3-last-fill-pos (point))))

(defun w3-handle-/plaintext (&optional args)
  (w3-put-state :nofill nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paragraph breaks, and other things that can cause linebreaks and
;;; alignment changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-header (&optional args)
  ;; Handle the creation of a header (of any level).  Causes a full
  ;; paragraph break. 
  (w3-handle-emphasis args)
  (let ((name (or (cdr-safe (assq 'name args))
		  (cdr-safe (assq 'id args))))
	(align (cdr-safe (assq 'align args)))
	(mangler (nth 2 (cdr-safe (assq tag w3-header-chars-assoc)))))
    (w3-handle-p)
    (if align
	(setq align (intern (downcase align)))
      (setq align (w3-get-default-style-info 'align)))
    (let ((tag 'p))
      (w3-pop-alignment))
    (w3-push-alignment align)
    (w3-put-state :text-mangler mangler)
    (if name (w3-put-state :name name))))

(defun w3-handle-header-end (&optional args)
  ;; Handle the closing of a header (of any level).  Causes a full
  ;; paragraph break.
  (w3-handle-emphasis-end)
  (let ((mangler (w3-get-state :text-mangler)))
    (and mangler (funcall mangler nil nil t)))
  (w3-put-state :text-mangler nil)
  (goto-char (point-max))
  (w3-handle-p)
  (let* ((info (car-safe (w3-get-state :lists)))
	 (type (and info (car-safe info))))
    (if (and type fill-prefix)
	(insert fill-prefix (cond
			     ((memq type '(ol dl)) "    ")
			     (t "  ")))))
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment)))

(defun w3-handle-pre (&optional args)
  ;; Marks the start of a preformatted section of text.  No paragraph
  ;; filling should be done from this point until a matching /pre has
  ;; been encountered.
  (w3-handle-p)
  (w3-put-state :nofill t)
  (w3-put-state :needspace t)
  (w3-put-state :pre-start (set-marker (make-marker) (point)))
  )

(defun w3-handle-xmp (&optional args)
  ;; Marks the start of a preformatted section of text.  No paragraph
  ;; filling should be done from this point until a matching /pre has
  ;; been encountered.
  (w3-handle-p)
  (w3-put-state :nofill t)
  (w3-put-state :needspace t)
  (w3-put-state :pre-start (set-marker (make-marker) (point)))
  (if (and args (cdr-safe (assq 'data args)))
      (progn
	(w3-handle-text (cdr-safe (assq 'data args)))
	(w3-handle-/xmp))))

(defun w3-handle-/pre (&optional args)
  (if (not (w3-get-state :nofill))
      (w3-handle-p)
    (w3-put-state :nofill nil)
    (let* ((info (car-safe (w3-get-state :lists)))
	   (type (and info (car-safe info)))
	   (st (w3-get-state :pre-start)))
      (if (not (bolp)) (insert "\n"))
      (if (and type fill-prefix st)
	  (progn
	    (save-excursion
	      (goto-char st)
	      (while (re-search-forward "^" nil t)
		(insert fill-prefix (cond
				     ((memq type '(ol dl)) "    ")
				     (t "  ")))))
	    (setq w3-last-fill-pos (point))
	    (insert fill-prefix (cond
				 ((memq type '(ol dl)) "    ")
				 (t "  "))))
	(setq w3-last-fill-pos (point))))
    (let ((tag 'p))
      (w3-handle-p))
    (setq w3-active-faces nil)
    (w3-put-state :pre-start nil)))

(fset 'w3-handle-/xmp 'w3-handle-/pre)

(defun w3-handle-blockquote (&optional args)
  ;; Start a section of quoted text.  This is done by causing the text
  ;; to be indented from the right and left margins.  Nested
  ;; blockquotes will cause further indentation.
  (let ((align (or (w3-get-default-style-info 'align) 'indent)))
    (w3-handle-p)
    (w3-push-alignment align))
  (w3-put-state :fillcol fill-column)
  (setq fill-column (max (- (or fill-column
				(1- (or w3-strict-width (window-width)))) 8)
			 10)))

(defun w3-handle-/blockquote (&optional args)
  (w3-handle-paragraph)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment))
  (setq fill-column (or (w3-get-state :fillcol) (1- (or w3-strict-width
							 (window-width)))))
  (w3-put-state :fillcol nil))

(defun w3-handle-align (&optional args)
  ;; Cause a single line break (like <BR>) and replace the current
  ;; alignment.
  (let ((align (intern (or (cdr-safe (assq 'role args))
			   (cdr-safe (assq 'align args))
			   (cdr-safe (assq 'style args))))))
    (w3-handle-paragraph)
    (w3-push-alignment align)))

(defun w3-handle-/align (&optional args)
  (w3-handle-paragraph)
  (w3-pop-alignment))

(defun w3-handle-hr (&optional args)
  ;; Cause a line break and insert a horizontal rule across the page.
  (w3-handle-paragraph)
  (let* ((perc (or (cdr-safe (assq 'width args))
		   (w3-get-default-style-info 'width)
		   "100%"))
	 (old-align (w3-current-alignment))
	 (talign (or (cdr-safe (assq 'textalign args))
		     (cdr-safe (assq 'text-align args))
		     (w3-get-default-style-info 'textalign)
		     (w3-get-default-style-info 'text-align)
		     (and old-align (symbol-name old-align))
		     "center"))
	 (text (cdr-safe (assq 'label args)))
	 (align (or (cdr-safe (assq 'align args))
		    (w3-get-default-style-info 'align)
		    old-align
		    'center))
	 (rule nil)
	 (width nil))
    (if (stringp talign)
	(setq talign (intern (downcase talign))))
    (if (stringp align)
	(setq align (intern (downcase align))))
    (w3-push-alignment align)

    (setq perc (min (string-to-int perc) 100)
	  width (/ (* (- (or w3-strict-width
			     (window-width))
			 w3-right-border) perc) 100))
    (if text
	(cond
	 ((>= (length text) width)
	  (setq rule (concat "-" text "-")))
	 ((eq talign 'right)
	  (setq rule (concat (make-string (- width 1 (length text))
					  w3-horizontal-rule-char)
			     text "-")))
	 ((eq talign 'center)
	  (let ((half (make-string (/ (- width (length text)) 2)
				   w3-horizontal-rule-char)))
	    (setq rule (concat half text half))))
	 ((eq talign 'left)
	  (setq rule (concat "-" text (make-string (- width 1
						      (length text))
						   w3-horizontal-rule-char)))))
      (setq rule (make-string width w3-horizontal-rule-char)))
    (w3-handle-text rule)
    (condition-case ()
	(w3-handle-paragraph)
      (error nil))
    (w3-pop-alignment)
    (setq w3-last-fill-pos (point))
    (let* ((info (car-safe (w3-get-state :lists)))
	   (type (and info (car-safe info)))
	   (cur (w3-current-alignment)))
      (cond
       ;;((eq cur 'indent)
       ;;(insert (make-string w3-indent-level ? )))
       ((and type fill-prefix (eq w3-last-tag 'dt))
	(insert fill-prefix))
       ((and type fill-prefix)
	(insert fill-prefix (if (eq type 'ol) "    " "  ")))
       (t nil)))))

(defun w3-handle-/p (&optional args)
  ;; Marks the end of a paragraph.  Only causes a paragraph break if
  ;; it is not followed by another paragraph or similar markup
  ;; (headers, list openings, etc) that will already cause a new
  ;; paragraph to be started.
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-handle-p)
    (w3-pop-alignment)))

(defun w3-handle-p (&optional args)
  (if (or (not (memq w3-last-tag '(li tr td th dt dd)))
	  (memq tag '(ol ul dl menu dir)))
      (let ((name (or (cdr-safe (assq 'name args))
		      (cdr-safe (assq 'id args))))
	    (align (cdr-safe (assoc 'align args))))
	(w3-handle-emphasis-end)
	(w3-handle-emphasis args)
	(w3-handle-paragraph)
	(w3-put-state :nowrap (assq 'nowrap args))
	(setq align (if align
			(intern (downcase align))
		      (w3-get-default-style-info 'align)))
	(and (eq tag 'p) (progn
			   (w3-pop-alignment)
			   (w3-push-alignment align)))
	(if (not (bobp))
	    (progn
	      (insert (cond
		       ((and (eolp) (bolp)) "\n")
		       ((eolp) "\n\n")
		       (t "\n")))
	      (setq w3-last-fill-pos (point))
	      (cond
	       ((null fill-prefix))
	       ((string= fill-prefix ""))
	       ((eq (car (car (w3-get-state :lists))) 'ol)
		(insert fill-prefix "    "))
	       (t (insert fill-prefix "  ")))))
	(if name (w3-put-state :name name)))))

(defun w3-handle-br (&optional args)
  ;; Cause a single line break.
  ;; The alignment will only effect the chunk of text (generally to
  ;; the last <br> or <p> tag) immediately before the <br>.  After
  ;; that, the alignment will revert to the containers alignment.
  (w3-handle-paragraph)
  (let* ((info (car-safe (w3-get-state :lists)))
	 (type (and info (car-safe info)))
	 (cur (w3-current-alignment)))
    (cond
     ;;((eq cur 'indent)
     ;;(insert (make-string w3-indent-level ? )))
     ((and type fill-prefix (eq w3-last-tag 'dt))
      (insert fill-prefix))
     ((and type fill-prefix)
      (insert fill-prefix (if (eq type 'ol) "    " "  ")))
     (t nil))))

(defun w3-handle-paragraph (&optional args)
  (if (not (bobp))
      (let ((align (w3-current-alignment))
            (fill-prefix fill-prefix))
	(cond
         ((eq align 'indent)
          (w3-set-fill-prefix-length
           (+ (length fill-prefix);; works even if fill-prefix is nil
              w3-indent-level)))
         ((null fill-prefix)
          (setq fill-prefix ""))
         ((string= fill-prefix ""))
	 ((eq (car (car (w3-get-state :lists))) 'ol)
          (w3-set-fill-prefix-length (+ 4 (length fill-prefix))))
         (t
          (w3-set-fill-prefix-length (+ 2 (length fill-prefix)))))
	(if (eq align 'indent)
	    (progn
	      (goto-char w3-last-fill-pos)
	      (insert fill-prefix)
	      (goto-char (point-max))))
	(if (and (> (current-column) fill-column)
		 (not (w3-get-state :nowrap))
		 (not (w3-get-state :nofill)))
	    (fill-region-as-paragraph w3-last-fill-pos (point)
				      (eq align 'justify)))
	(if (not w3-last-fill-pos)
	    (setq w3-last-fill-pos (point-min)))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")
	(delete-region (point) (point-max))
	(if (< w3-last-fill-pos (point))
	    (cond
	     ((or (eq align 'center) (w3-get-state :center))
	      (center-region w3-last-fill-pos (point)))
	     ((eq align 'right)
	      (let ((x (point)))
		(catch 'fill-exit
		  (save-excursion
		    (goto-char w3-last-fill-pos)
		    (while (re-search-forward "$" x t)
		      (if (/= (current-column) fill-column)
			  (let ((buff (- fill-column (current-column))))
			    (beginning-of-line)
			    (setq x (+ x buff))
			    (if (> buff 0)
				(insert (make-string buff ? )))
			    (end-of-line))
			(end-of-line))
		      (if (eobp) (throw 'fill-exit t))
		      (condition-case ()
			  (forward-char 1)
			(error (throw 'fill-exit t))))))))))
	(insert "\n")
	(setq w3-last-fill-pos (point))
	(w3-put-state :needspace 'never))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List handling code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-list-ending (&optional args)
  ;; Handles all the list terminators (/ol /ul /dl).
  ;; This just fills the last paragrpah, then reduces the depth in
  ;; `w3-state' and truncates `fill-prefix'"
  (w3-handle-paragraph)
  (w3-put-state :depth (max 0 (1- (w3-get-state :depth))))
  (w3-put-state :next-break t)
  (w3-set-fill-prefix-length (* (w3-get-state :depth) w3-indent-level))
  (w3-put-state :lists (cdr (w3-get-state :lists)))
  (if (/= 0 (length fill-prefix))
      (insert fill-prefix "  ")))

(defun w3-handle-list-opening (&optional args)
  ;; Handles all the list openers (ol ul dl).
  ;; This just fills the last paragraph, then increases the depth in
  ;; `w3-state' and adds to `fill-prefix'
  (w3-handle-p)
  (let ((style (and (not (assq 'style args))
		    (w3-get-default-style-info 'style))))
    (if style
	(setq args (cons (cons 'style style) args))))
  ;; Default VALUE attribute for OL is 1.
  (if (eq tag 'ol)
      (or (assq 'value args)
          (setq args (cons (cons 'value 1) args))))
  (w3-put-state :depth (1+ (w3-get-state :depth)))
  (w3-set-fill-prefix-length (* (w3-get-state :depth) w3-indent-level))
  (insert "\n\n" fill-prefix "  ")
  (w3-put-state :lists (cons (cons tag (copy-alist args))
			      (w3-get-state :lists))))

(defun w3-handle-table-definition (&optional args)
  (w3-handle-paragraph)
  (insert fill-prefix "  "))

(defun w3-handle-table-term (&optional args)
  (w3-handle-paragraph)
  (insert "\n" fill-prefix))

(defun w3-handle-list-item (&optional args)
  (w3-handle-paragraph)
  (let* ((info (car (w3-get-state :lists)))
	 (type (car info))
	 (endr (or (nth (1- (or (w3-get-state :depth) 1))
			(cdr (or (assoc type w3-list-chars-assoc)
				 (car w3-list-chars-assoc))))
		   "*")))
    (setq info (cdr info))
    (cond
     ((assq 'plain info)
      ;; We still need to indent from the left margin for lists without
      ;; bullets.  This is especially important with nested lists.
      ;; Question: Do we want this to be equivalent to replacing the
      ;; bullet by a space (" ") or by indenting so that the text starts
      ;; where the bullet would have been?  I've chosen the latter after
      ;; looking at both kinds of output.
      (insert fill-prefix))
     ((eq type 'ol)
      (let ((next (or (assq 'seqnum info) (assq 'value info)))
	    (type (cdr-safe (assq 'style info)))
	    (uppr (assq 'upper info))
	    (tokn nil))
	(if (stringp (cdr next)) (setcdr next (string-to-int (cdr next))))
	(cond
	 ((or (assq 'roman info)
	      (member type '("i" "I")))
	  (setq tokn (concat
		      (w3-pad-string (w3-decimal-to-roman (cdr next)) 3 ?
				     'left)
		      endr)))
	 ((or (assq 'arabic info)
	      (member type '("a" "A")))
	  (setq tokn (concat (w3-pad-string
			      (w3-decimal-to-alpha (cdr next)) 3 ?  'left)
			     endr)))
	 (t
	  (setq tokn (concat (w3-pad-string (int-to-string (cdr next))
					    2 ?  'left)
			     endr))))
	(if (assq 'uppercase info)
	    (setq tokn (upcase tokn)))
	(insert fill-prefix tokn " ")
	(setcdr next (1+ (cdr next)))
	(w3-put-state :needspace 'never)))
     (t
      (insert fill-prefix endr " ")))))

(defun w3-pad-string (str len pad side)
  ;; Pads a string STR to a certain length LEN, using fill character
  ;; PAD by concatenating PAD to SIDE of the string.
  (let ((strlen (length str)))
    (cond
     ((>= strlen len) str)
     ((eq side 'right) (concat str (make-string (- len strlen) pad)))
     ((eq side 'left)  (concat (make-string (- len strlen) pad) str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routines to handle character-level formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-q (&optional args)
  (w3-handle-emphasis)
  (w3-handle-text (or (w3-get-default-style-info 'startquote) "\"")))

(defun w3-handle-/q (&optional args)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-handle-text (or (w3-get-default-style-info 'endquote) "\"")))
  (w3-handle-emphasis-end))

(defun w3-handle-emphasis (&optional args)
  ;; Generic handler for character-based emphasis.  Increments the state
  ;; of TAG (which must be bound by the calling procedure).  This
  ;; checks all the various stylesheet mechanisms that may cause an
  ;; alignment shift as well.
  (let ((align (or (w3-get-default-style-info 'align)
		   (and (eq tag 'address) w3-right-justify-address 'right))))
    (if (and align (not (memq tag '(h1 h2 h3 h4 h5 h6))))
	(progn
	  (w3-handle-paragraph)
	  (w3-push-alignment align))))
  (let* ((spec (and w3-delimit-emphasis (assoc tag w3-style-tags-assoc)))
	 (class (cdr-safe (assq 'class args)))
	 (face (w3-face-for-element))
	 (voice (w3-voice-for-element))
	 (beg (and spec (car (cdr spec)))))
    (if spec
	(insert beg))
    (if voice
	(setq w3-active-voices (cons voice w3-active-voices)))
    (if face
	(setq w3-active-faces (cons face w3-active-faces)))))

(defun w3-handle-emphasis-end (&optional args)
  ;; Generic handler for ending character-based emphasis.  Decrements
  ;; the state of TAG (which must be bound by the calling procedure).
  ;; Stylesheet mechanisms may cause arbitrary alignment changes.
  (let* ((tag (cdr-safe (assq tag w3-end-tags)))
	 (spec (and w3-delimit-emphasis (assq tag w3-style-tags-assoc)))
	 (end (and spec (cdr (cdr spec)))))
    (if (assq tag w3-active-voices)
	(setq w3-active-voices (cdr (memq (assq tag w3-active-voices)
					  w3-active-voices)))
      (setq w3-active-voices (delq tag w3-active-voices)))
    (if (assq tag w3-active-faces)
	(setq w3-active-faces (cdr (memq (assq tag w3-active-faces)
					 w3-active-faces)))
      (setq w3-active-faces (delq tag w3-active-faces)))
    (if spec (insert end))
    (if (eq tag 'address)
	(progn
	  (w3-handle-paragraph)
	  (w3-pop-alignment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML 3.0 compliance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-math (&optional args)
  (w3-handle-br)
  (w3-handle-text "[START MATH - Not Implemented (Yet)]")
  (w3-handle-br))

(defun w3-handle-/math (&optional args)
  (w3-handle-br)
  (w3-handle-text "[END MATH]")
  (w3-handle-br))

(defun w3-handle-tr (&optional args)
  (w3-handle-br))

(defun w3-handle-/tr (&optional args)
  (w3-handle-br))

(defun w3-handle-td (&optional args)
  (w3-handle-text " | "))

(defun w3-handle-/td (&optional args)
  (w3-handle-text " | "))

(defun w3-handle-th (&optional args)
  (w3-handle-text " | "))

(defun w3-handle-/th (&optional args)
  (w3-handle-text " | "))

(defun w3-handle-table (&optional args)
  (w3-handle-br))

(defun w3-handle-/table (&optional args)
  (w3-handle-br))

(defun w3-handle-div (&optional args)
  (let ((align (cdr-safe (assq 'align args))))
    (w3-handle-emphasis args)
    (w3-handle-paragraph)
    (setq align (and align (intern (downcase align))))
    (w3-push-alignment align)))

(defun w3-handle-/div (&optional args)
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assq tag w3-end-tags))))
    (w3-handle-paragraph)
    (w3-pop-alignment)))

(defun w3-handle-note (&optional args)
  (w3-handle-emphasis)
  (w3-handle-paragraph)
  (let ((align (or (w3-get-default-style-info 'align) 'indent)))
    (w3-push-alignment align))
  (w3-handle-text (concat (or (cdr-safe (assq 'role args)) "CAUTION") ":")))

(defun w3-handle-/note (&optional args)
  (w3-handle-paragraph)
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment)))

(defun w3-handle-fig (&optional args)
  (w3-put-state :figdata args)
  (w3-put-state :figalt (set-marker (make-marker) (point)))
  )

(defun w3-handle-caption (&optional args)
  )

(defun w3-handle-/caption (&optional args)
  )

(defun w3-handle-/fig (&optional args)
  (let* ((data (w3-get-state :figdata))
	 (src (cdr-safe (assq 'src data)))
	 (aln (cdr-safe (assq 'align data)))
	 (alt (if (w3-get-state :figalt)
		  (prog1
		      (buffer-substring (w3-get-state :figalt) (point))
		    (delete-region (w3-get-state :figalt) (point)))))
	 (ack nil))
    (setq w3-last-fill-pos (point))
    (if (not src)
	(w3-warn 'html "Malformed <fig> tag.")
      (setq ack (list (cons 'src src)
		      (cons 'alt alt)
		      (cons 'align aln)))
      (w3-handle-pre nil)
      (w3-handle-image ack)
      (w3-handle-/pre nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Netscape Compatibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For some reason netscape treats </br> like <br> - ugh.
(fset 'w3-handle-/br 'w3-handle-br)

(defun w3-create-blank-pixmap (width height)
  (let ((retval
	 (concat "/* XPM */\n"
		 "static char *pixmap[] = {\n"
		 ;;"/* width height num_colors chars_per_pixel */\n"
		 (format "\"    %d   %d   2     1\",\n" width height)
		 ;;"/* colors */\n"
		 "\". c #000000 s background\",\n"
		 "\"# c #FFFFFF s foreground\",\n"
		 ;;"/* pixels /*\n"
		 ))
	(line (concat "\"" (make-string width ?.) "\"")))
    (while (/= 1 height)
      (setq retval (concat retval line ",\n")
	    height (1- height)))
    (concat retval line "\n};")))

(defun w3-handle-spacer (&optional args)
  (let ((type (cdr-safe (assq 'type args)))
	(size (cdr-safe (assq 'size args)))
	(w (or (cdr-safe (assq 'width args)) 1))
	(h (or (cdr-safe (assq 'height args)) 1))
	(align (cdr-safe (assq 'align args)))
	(glyph nil))
    (condition-case ()
	(setq glyph (make-glyph
		     (vector 'xpm :data (w3-create-blank-pixmap w h))))
      (error nil))
    )
  )

(defun w3-handle-font (&optional args)
  (let* ((sizearg (cdr-safe (assq 'size args)))
	 (sizenum (cond
		   ((null sizearg) nil)
		   ((= ?+ (string-to-char sizearg))
		    (min (+ 3 (string-to-int (substring sizearg 1))) 7))
		   ((= ?- (string-to-char sizearg))
		    (max (- 3 (string-to-int (substring sizearg 1))) 0))
		   ((string= sizearg (int-to-string (string-to-int sizearg)))
		    (string-to-int sizearg))
		   (t nil)))
	 (family (cdr-safe (assq 'face args)))
	 (color (cdr-safe (assq 'color args)))
	 (normcolor (if color (w3-normalize-color color)))
	 (w3-current-stylesheet  (list
				  (list 'font
					(list 'internal
					      (cons 'font-family family)
					      (cons 'font-size-index sizenum)
					      (cons 'foreground normcolor))))))
    (w3-style-post-process-stylesheet w3-current-stylesheet)
    (w3-handle-emphasis args)))

(defun w3-handle-/font (&optional args)
  (w3-handle-emphasis-end))

(defun w3-handle-center (&optional args)
  (w3-handle-paragraph)
  (w3-push-alignment 'center))

(defun w3-handle-/center (&optional args)
  (w3-handle-paragraph)
  (let ((tag 'center))
    (w3-pop-alignment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bonus HTML Tags just for fun :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-embed (&optional args)
  ;; This needs to be reimplemented!!!
  )

(defun w3-handle-blink (&optional args)
  ;; Keep track of all the buffers with blinking in them, and do GC
  ;; of this list whenever a new <blink> tag is encountered.  The
  ;; timer checks this list to see if any of the buffers are visible,
  ;; and only blinks the face if there are any visible.  This cuts
  ;; down tremendously on the amount of X traffic, and frame !@#!age
  ;; due to lots of face munging.
  (w3-handle-emphasis args)
  (let ((buffs w3-blinking-buffs)
	(name1 (buffer-name))
	(name2 nil)
	(add t))
    (setq w3-blinking-buffs nil)
    ;; Get rid of old buffers
    (while buffs
      (setq name2 (buffer-name (car buffs)))
      (if (null name2)
	  nil
	(setq w3-blinking-buffs (cons (car buffs) w3-blinking-buffs))
	(if (string= name1 name2)
	    (setq add nil)))
      (setq buffs (cdr buffs)))
    (if add
	(setq w3-blinking-buffs (cons (current-buffer) w3-blinking-buffs)))))

(defun w3-handle-/blink (&optional args)
  (w3-handle-emphasis-end args))

(defun w3-handle-peek (&optional args)
  ;; Handle the peek tag.  Valid attributes are:
  ;; VARIABLE:: any valid lisp variable
  ;; If VARIABLE is bound and non-nil, then the value of the variable is
  ;; inserted at point.  This can handle variables whos values are any
  ;; arbitrary lisp type.
  (let* ((var-name (cdr-safe (assq 'variable args)))
	 (var-sym  (and var-name (intern var-name)))
	 (val      (and var-sym (boundp var-sym) (symbol-value var-sym))))
    (cond
     ((null val) nil)
     ((stringp val) (w3-handle-text val))
     (t (w3-handle-text (format "%S" val))))))

(defun w3-rotate-region (st nd &optional rotation)
  "Ceasar rotate a region between ST and ND using ROTATION as the
amount to rotate the text.  Defaults to caesar (13)."
  (setq rotation (or rotation 13))
  (save-excursion
    (let (x)
      (while (< st nd)
	(setq x (char-after st))
	(cond
	 ((and (>= x ?a) (<= x ?z))
	  (setq x (- x ?a)
		x (char-to-string (+ (% (+ x rotation) 26) ?a))))
	 ((and (>= x ?A) (<= x ?Z))
	  (setq x (- x ?A)
		x (char-to-string (+ (% (+ x rotation) 26) ?A))))
	 (t (setq x nil)))
	(if x (progn (goto-char st) (delete-char 1) (insert x)))
	(setq st (1+ st))))))

(defun w3-handle-kill-sgml (&optional args)
  (w3-handle-text "SGML is the spawn of evil!  It must be stopped!"))

(defun w3-handle-secret (&optional args)
  (if (fboundp 'valid-specifier-locale-p)
      (let ((tag 'rot13))
	(w3-handle-emphasis))
    (w3-put-state :secret (set-marker (make-marker) (point)))))

(defun w3-handle-/secret (&optional args)
  "Close a secret region of text."
  (if (fboundp 'valid-specifier-locale-p)
      (let ((tag '/rot13))
	(w3-handle-emphasis-end))
    (if (integer-or-marker-p (w3-get-state :secret))
	(progn
	  (w3-rotate-region (w3-get-state :secret) (point))
	  (w3-put-state :secret nil)))))

(defun w3-handle-hype (&optional args)
  (if (and (or (featurep 'nas-sound) (featurep 'native-sound))
	   (assoc 'hype sound-alist))
      (play-sound 'hype 100)
    (w3-handle-text "Hey, has Marca A. told you how cool he is?")))

(defun w3-handle-yogsothoth (&optional args)
  (w3-handle-image (list (cons 'src "href-to-yogsothoth-pic")
			 (cons 'alt "YOGSOTHOTH LIVES!!!"))))

(defun w3-handle-roach (&optional args)
  (w3-handle-text "Man, I am so wasted..."))

(defun w3-handle-/roach (&optional args)
  (w3-handle-text (concat "So, you wanna get some "
			  (or (cdr-safe (assq 'munchy args))
			      "nachos") "? ")))

(defun w3-invert-face (face)
  (let ((buffs w3-blinking-buffs)
	(blink nil)
	(buff nil))
    (if buffs
	(while buffs
	  (setq buff (car buffs))
	  (cond
	   ((bufferp buff)
	    (if (buffer-name buff)
		(setq buff (car buffs))
	      (setq buff nil)))
	   ((stringp buff)
	    (setq buff (get-buffer buff)))
	   (t
	    (setq buff nil)))
	  (setq buffs (cdr buffs)
		buff (and buff (get-buffer-window buff 'visible))
		buff (and buff (window-live-p buff)))
	  (if buff (setq buffs nil
			 blink t))))
    (if blink (invert-face face))))

(autoload 'sentence-ify "flame")
(autoload 'string-ify "flame")
(autoload '*flame "flame")
(if (not (fboundp 'flatten)) (autoload 'flatten "flame"))

(defvar w3-cookie-cache nil)

(defun w3-handle-cookie (&optional args)
  (if (not (fboundp 'cookie))
      (w3-handle-text "Sorry, no cookies today.")
    (let* ((url-working-buffer (url-generate-new-buffer-name " *cookie*"))
	   (href (url-expand-file-name
		  (or (cdr-safe (assq 'src args))
		      (cdr-safe (assq 'href args)))
		  (cdr-safe (assoc (cdr-safe (assq 'base args))
				   w3-base-alist))))
	   (fname (or (cdr-safe (assoc href w3-cookie-cache))
		      (url-generate-unique-filename "%s.cki")))
	   (st (or (cdr-safe (assq 'start args)) "Loading cookies..."))
	   (nd (or (cdr-safe (assq 'end args))
		   "Loading cookies... done.")))
      (if (not (assoc href w3-cookie-cache))
	  (save-excursion
	    (url-clear-tmp-buffer)
	    (setq url-be-asynchronous nil)
	    (url-retrieve href)
	    (url-uncompress)
	    (write-region (point-min) (point-max) fname 5)
	    (setq w3-cookie-cache (cons (cons href fname) w3-cookie-cache))))
      (w3-handle-text (cookie fname st nd)))))

(defun w3-handle-flame (&optional args)
  (condition-case ()
      (w3-handle-text
       (concat
	(sentence-ify
	 (string-ify
	  (append-suffixes-hack (flatten (*flame)))))))
    (error nil)))

(defun w3-handle-pinhead (&optional args)
  (if (fboundp 'yow)
      (w3-handle-text (yow))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Client-side Imagemaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-map (&optional args)
  (w3-put-state :map (cons (or (cdr-safe (assq 'name args))
				     (cdr-safe (assq 'id args))
				     "unnamed") nil)))

(defun w3-handle-/map (&optional args)
  (and (w3-get-state :map)
       (setq w3-imagemaps (cons (w3-get-state :map) w3-imagemaps)))
  (w3-put-state :map nil))

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

(defun w3-handle-area (&optional args)
  (let ((type (downcase (or (cdr-safe (assq 'shape args)) "rect")))
	(coords (w3-decode-area-coords (or (cdr-safe (assq 'coords args)) "")))
	(alt (cdr-safe (assq 'alt args)))
	(href (if (assq 'nohref args)
		  t
		(url-expand-file-name
		 (or (cdr-safe (assq 'src args))
		     (cdr-safe (assq 'href args)))
		 (cdr-safe (assoc (cdr-safe (assq 'base args))
				  w3-base-alist)))))
	(map (w3-get-state :map)))
    ;; data structure in storage is a vector
    ;; if (href == t) then no action should be taken
    ;; [ type coordinates href (hopefully)descriptive-text]
    (setcdr map (cons (vector type coords href alt) (cdr map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags that don't really get drawn, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-/html (&optional args)
  ;; Technically, we are not supposed to have any text outside the
  ;; html element, so start ignoring everything.
  (put 'text 'w3-formatter 'ack))

(defun w3-handle-body (&optional args)
  (if (not w3-user-colors-take-precedence)
      (let* ((vlink (cdr-safe (assq 'vlink args)))
	     (alink (cdr-safe (assq 'alink args)))
	     (link (cdr-safe (assq 'link args)))
	     (text (cdr-safe (assq 'text args)))
	     (backg (cdr-safe (assq 'background args)))
	     (rgb (or (cdr-safe (assq 'bgcolor args))
		      (cdr-safe (assq 'rgb args))))
	     (temp-face nil)
	     (sheet ""))
	(setq backg (url-expand-file-name
		     backg
		     (cdr-safe (assoc (cdr-safe (assq 'base args))
				      w3-base-alist))))
	(if (or text rgb backg)
	    (progn
	      (setq sheet "html {")
	      (if text  (setq sheet (format "%scolor: %s; " sheet
					    (w3-normalize-color text))))
	      (if rgb   (setq sheet (format "%sbackground: %s; "
					    sheet (w3-normalize-color rgb))))
	      (if backg (setq sheet (format "%sbackdrop: %s; "
					    sheet backg)))
	      (setq sheet (concat sheet " }\n"))))
	(if link
	    (setq sheet (format "%sa.link { color: %s }\n" sheet
				(w3-normalize-color link))))
	(if vlink
	    (setq sheet (format "%sa.visited { color: %s }\n" sheet
				(w3-normalize-color vlink))))
	(if alink
	    (setq sheet (format "%sa.active { color: %s }\n" sheet
				(w3-normalize-color alink))))
	(if (/= (length sheet) 0)
	    (w3-handle-style (list (cons 'data sheet)
				   (cons 'notation "css")))))))

(defun w3-handle-cryptopts (&optional args)
  (put 'text 'w3-formatter 'ack))

(defun w3-handle-/cryptopts (&optional args)
  (put 'text 'w3-formatter nil))

(defun w3-handle-certs (&optional args)
  (put 'text 'w3-formatter 'ack))

(defun w3-handle-/certs (&optional args)
  (put 'text 'w3-formatter nil))

(defun w3-handle-base (&optional args)
  (setq w3-base-alist (cons
		       (cons (or (cdr-safe (assq 'name args))
				 (cdr-safe (assq 'id args)))
			     (or (cdr-safe (assq 'href args))
				 (cdr-safe (assq 'src args))
				 (url-view-url t)))
		       w3-base-alist)))

(defun w3-handle-isindex (&optional args)
  (let ((prompt (or (cdr-safe (assq 'prompt args))
		    "Search on (+ separates keywords): "))
	action)
    (setq action (url-expand-file-name
		  (or (cdr-safe (assq 'src args))
		      (cdr-safe (assq 'href args))
		      (url-view-url t))
		  (cdr-safe (assoc (cdr-safe (assq 'base args))
				   w3-base-alist))))
    (if (and prompt (string-match "[^: \t-]+$" prompt))
	(setq prompt (concat prompt ": ")))
    (if w3-use-forms-index
	(progn
	  (w3-handle-hr)
	  (w3-handle-form (list (cons 'action action)
				(cons 'enctype "application/x-w3-isindex")
				(cons 'method "get")))
	  (w3-handle-text (concat prompt " "))
	  (w3-handle-input (list (cons 'type "text")
				 (cons 'name "isindex")))))
    (setq w3-current-isindex (cons action prompt))))

(defun w3-handle-meta (&optional args)
  (let* ((equiv (cdr-safe (assq 'http-equiv args)))
	 (value (cdr-safe (assq 'content args)))
	 (node  (and equiv (assoc (setq equiv (downcase equiv))
				  url-current-mime-headers))))
    (if equiv
	(setq url-current-mime-headers (cons (cons equiv value)
					     url-current-mime-headers)))
    ;; Special-case the Set-Cookie header
    (if (and equiv (string= (downcase equiv) "set-cookie"))
	(url-cookie-handle-set-cookie value))
    ;; Special-case the refresh header
    (if (and equiv (string= (downcase equiv) "refresh"))
	(url-handle-refresh-header value))))

(defun w3-handle-link (&optional args)
  (let* ((dest (cdr-safe (assq 'href args)))
	 (type (if (assq 'rel args) "Parent of" "Child of"))
	 (desc (or (cdr-safe (assq 'rel args))
		   (cdr-safe (assq 'rev args))))
	 (node-1 (assoc type w3-current-links))
	 (node-2 (and node-1 desc (assoc desc (cdr node-1))))
	 (base (cdr-safe (assq 'base args))))
    (if dest
	(progn
	  (setq dest (url-expand-file-name
		      dest
		      (cdr-safe (assoc base w3-base-alist))))
	  (cond
	   (node-2			; Add to old value
	    (setcdr node-2 (cons dest (cdr node-2))))
	   (node-1			; first rel/rev
	    (setcdr node-1 (cons (cons desc (list dest)) (cdr node-1))))
	   (t (setq w3-current-links
		    (cons (cons type (list (cons desc (list dest))))
			  w3-current-links))))
	  (if (and dest desc (member (downcase desc)
					 '("style" "stylesheet")))
	      (w3-handle-style (list (cons 'src dest))))))))

(defun w3-maybe-start-image-download (widget)
  (let* ((src (widget-get widget 'src))
	 (cached-glyph (w3-image-cached-p src)))
    (if (and cached-glyph (w3-glyphp cached-glyph))
	(setq w3-image-widgets-waiting (cons widget w3-image-widgets-waiting))
      (cond
       ((or w3-delay-image-loads (not (fboundp 'valid-specifier-domain-p)))
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
    (if node
	(set-glyph-image (cdr node) (glyph-image glyph))
      (setq w3-graphics-list (cons (cons url glyph) w3-graphics-list)))

    (if (and (buffer-name buffer)	; Dest. buffer exists
	     (w3-glyphp glyph))		; got a valid glyph
	(save-excursion
	  (set-buffer buffer)
	  (if (eq major-mode 'w3-mode)
	      (widget-value-set widget glyph)
	    (setq w3-image-widgets-waiting
		  (cons widget w3-image-widgets-waiting)))))))

(defun w3-handle-image (&optional args)
  (let* ((parms args)
	 (height (cdr-safe (assq 'height parms)))
	 (width (cdr-safe (assq 'width parms)))
	 (src (or (cdr-safe (assq 'src parms))
		  "Error Image"))
	 (our-alt (cond
		   ((null w3-auto-image-alt) "")
		   ((eq t w3-auto-image-alt)
		    (concat "[IMAGE(" (url-basepath src t) ")] "))
		   ((stringp w3-auto-image-alt)
		    (format w3-auto-image-alt (url-basepath src t)))))
	 (alt (or (cdr-safe (assq 'alt parms))
		  our-alt))
	 (ismap (and (assq 'ismap args) 'ismap))
	 (usemap (cdr-safe (assq 'usemap args)))
	 (dest (w3-get-state :href))
	 (base (cdr-safe (assq 'base args)))
	 (widget nil)
	 (zone (w3-get-state :zone))
	 (align (intern (or (cdr-safe (assq 'align parms)) "middle"))))
    (setq src (url-expand-file-name src
				    (cdr-safe (assoc base w3-base-alist))))
    (if dest
	(w3-handle-hyperlink-end))
    (setq widget
	  (widget-create 'image
			 'src src	; Where to load the image from
			 'alt alt	; Textual replacement
			 'ismap ismap	; Is it a server-side map?
			 'usemap usemap	; Is it a client-side map?
			 'href dest	; Hyperlink destination
			 ))
    (widget-put widget 'buffer (current-buffer))
    (w3-maybe-start-image-download widget)
    (goto-char (point-max))
    (if dest
	(w3-handle-hyperlink (list (cons 'href dest))))))

(defun w3-handle-title (&optional args)
  (if (w3-get-state :title)
      (w3-put-state :title nil))
  (put 'text 'w3-formatter 'w3-handle-title-text))

(defun w3-handle-title-text (&optional args)
  (w3-put-state :title
       (concat (w3-get-state :title) args)))

(defun w3-handle-/title (&optional args)
  (put 'text 'w3-formatter nil)
  (let ((ttl (w3-get-state :title)))
    (if (not (stringp ttl))
	nil
      (setq ttl (w3-fix-spaces ttl))
      (if (and ttl (string= ttl ""))
	  (setq ttl (w3-fix-spaces (url-view-url t))))
      (rename-buffer (url-generate-new-buffer-name ttl))
      ;; Make the URL show in list-buffers output
      (make-local-variable 'list-buffers-directory)
      (setq list-buffers-directory (url-view-url t))
      (w3-put-state :title t))))

(fset 'w3-handle-/head 'w3-handle-/title)

(defun w3-handle-hyperlink (&optional args)
  (let* ((href-node (assq 'href args))
	 (href (cdr href-node))
	 (title (cdr-safe (assq 'title args)))
	 (base (cdr-safe (assq 'base args)))
	 (name (or (cdr-safe (assq 'id args))
		   (cdr-safe (assq 'name args)))))
    (if href
	(progn
	  (setq href (url-expand-file-name href (cdr-safe
						 (assoc base w3-base-alist))))
	  (setcdr href-node href)))
    (w3-put-state :seen-this-url (url-have-visited-url href))
    (w3-put-state :zone (point))
    (w3-put-state :link-args args)
    (if title (w3-put-state :link-title title))
    (if href (w3-put-state :href href))
    (if name (w3-put-state :name name))))

(defun w3-follow-hyperlink (widget &rest ignore)
  (let* ((target (widget-get widget 'target))
	 (href (widget-get widget 'href))
	 (tag 'a)
	 (args '((class . "visited")))
	 (face (cdr (w3-face-for-element)))
	 (old-face (and (widget-get widget :from)
			(get-text-property (widget-get widget :from) 'face)))
	 (faces (cond
		 ((and old-face (consp old-face)) (cons face old-face))
		 (old-face (cons face (list old-face)))
		 (t (list face)))))	       
    (if target (setq target (intern (downcase target))))
    (put-text-property (widget-get widget :from) (widget-get widget :to)
		       'face faces)
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

(defun w3-handle-hyperlink-end (&optional args)
  (let* ((href (w3-get-state :href))
	 (old-args (w3-get-state :link-args))
	 (name (w3-get-state :name))
	 (zone (w3-get-state :zone))
	 (btdt (and href (w3-get-state :seen-this-url)))
	 (tag 'a)
	 (args (list (cons 'class (if btdt "visited" "link"))))
	 (face (cdr (w3-face-for-element)))
	 (old-face (and zone (get-text-property zone 'face)))
	 (faces (cond
		 ((and old-face (consp old-face)) (cons face old-face))
		 (old-face (cons face (list old-face)))
		 (t (list face)))))
    (if (not href)
	nil
      (add-text-properties zone (point)
			   (list 'mouse-face 'highlight
				 'button
				 (append 
				  (list 'push :args nil :value "" :tag ""
					:notify 'w3-follow-hyperlink
					:from (set-marker (make-marker) zone)
					:to (set-marker (make-marker) (point))
					)
				  (alist-to-plist old-args))
				 'face faces
				 'balloon-help 'w3-balloon-help-callback
				 'title (cons
					 (set-marker (make-marker) zone)
					 (set-marker (make-marker) (point)))
				 'help-echo href))
      (w3-put-state :zone nil)
      (w3-put-state :href nil)
      (w3-put-state :name nil)
      (if (and w3-link-info-display-function
	       (fboundp w3-link-info-display-function))
	  (let ((info (condition-case ()
			  (funcall w3-link-info-display-function href)
			(error nil))))
	    (if (and info (stringp info))
		(w3-handle-text info)))))))

(defvar w3-tab-alist nil
  "An assoc list of tab stops and their respective IDs")
(make-variable-buffer-local 'w3-tab-alist)

(defun w3-handle-tab (&optional args)
  (let* ((id (cdr-safe (assq 'id args)))
	 (to (cdr-safe (assq 'to args)))
	 (pos (cdr-safe (assoc to w3-tab-alist))))
    (cond
     (id				; Define a new tab stop
      (setq w3-tab-alist (cons (cons id (current-column)) w3-tab-alist)))
     ((and to pos)			; Go to a currently defined tabstop
      (while (<= (current-column) pos)
	(insert " ")))
     (to				; Tabstop 'to' is no defined yet
      (w3-warn 'html (format "Unkown tab stop -- `%s'" to)))
     (t					; Just do a tab
      (insert (make-string w3-indent-level ? ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Some bogus shit for pythia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-margin (&optional args)
  (if (assq 'reset args)
      (w3-handle-/blockquote nil)
    (w3-handle-blockquote nil)))
  
(fset 'w3-handle-l 'w3-handle-br)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Guts of the forms interface for the new display engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-form (&optional args)
  (let ((actn (cdr-safe (assq 'action args)))
	(enct (cdr-safe (assq 'enctype args)))
	(meth (cdr-safe (assq 'method args))))
    (if (not meth) (setq args (cons (cons 'method "GET") args)))
    (if (not actn)
	(setq args (cons (cons 'action
			       (or
				(cdr-safe (assoc (cdr-safe (assq 'base args))
						 w3-base-alist))
				(url-view-url t))) args))
      (setcdr (assq 'action args)
	      (url-expand-file-name
	       actn
	       (cdr-safe (assoc (cdr-safe (assq 'base args))
				w3-base-alist)))))
    (if (not enct)
	(setq args
	      (cons (cons 'enctype "application/x-www-form-urlencoded")
		    args)))
    (w3-put-state :form args)))

(defun w3-handle-/form (&optional args)
  (w3-handle-paragraph)
  (w3-put-state :form nil)
  (w3-put-state :formnum (1+ (w3-get-state :formnum)))
  )

(defun w3-handle-keygen (&optional args)
  (w3-form-add-element 'keygen 
		       (or (cdr-safe (assq 'name args)) "")
		       nil
		       nil
		       1000
		       nil
		       (w3-get-state :form)
		       nil
		       (w3-get-state :formnum)
		       nil
		       (w3-face-for-element)))

(defun w3-handle-input (&optional args)
  (if (or (not (w3-get-state :form))
	  (w3-get-state :select))
      (w3-warn
       'html
       "<input> outside of a <form> or inside <select> construct - ERROR!!")
    (let* ((type (intern (downcase (or (cdr-safe (assq 'type args)) "text"))))
	   (name (cdr-safe (assq 'name args)))
	   (value (or (cdr-safe (assq 'value args)) ""))
	   (size (string-to-int (or (cdr-safe (assq 'size args)) "20")))
	   (maxlength (cdr (assoc 'maxlength args)))
	   (default value)
	   (action (w3-get-state :form))
	   (options)
	   (num (w3-get-state :formnum))
	   (id (cdr-safe (assq 'id args)))
	   (checked (assq 'checked args))
	   (face (w3-face-for-element)))
      (if (and (string-match "^[ \t\n\r]+$" value)
	       (not (eq type 'hidden)))
	  (setq value ""))
      (if maxlength (setq maxlength (string-to-int maxlength)))
      (if (and name (string-match "[\r\n]" name))
	  (setq name (mapconcat (function
				 (lambda (x)
				   (if (memq x '(?\r ?\n))
				       ""
				     (char-to-string x))))
				name "")))
      (if (memq type '(checkbox radio)) (setq default checked))
      (if (and (eq type 'checkbox) (string= value ""))
	  (setq value "on"))
      (w3-form-add-element type name value size maxlength default action
			   options num id checked face))))

(defun w3-handle-/select (&optional args)
  (if (not (and (w3-get-state :form)
		(w3-get-state :select)))
      (w3-warn 'html
	       "</select> outside of a <form> or <select> construct - ERROR!!")
    (put 'text 'w3-formatter 'w3-handle-text)
    (let* ((args (w3-get-state :select))
	   (tag 'input)
	   (face (w3-face-for-element))
	   (opts (w3-get-state :options))
	   (form (w3-get-state :form))
	   (max-size nil)
	   (type "OPTION")
	   (default nil)
	   (tmp nil)
	   (id (cdr-safe (assq 'id args)))
	   (checked nil)
	   )
      (setq tmp (reverse opts))
      (if (assq 'multiple args)
	  (let ((tag 'ul)		; Convert to a list of checkboxes
		(nam (or (cdr-safe (assq 'name args)) "option"))
		(old (w3-get-state :align))
		(first nil))
	    (w3-put-state :options nil)
	    (w3-put-state :select nil)
	    (w3-handle-list-opening)
	    (w3-put-state :align nil)
	    (while tmp
	      (w3-handle-list-item)
	      (w3-handle-input (list (cons 'type "checkbox")
				     (cons 'name nam)
				     (cons 'value
					   (or (cdr-safe
						(assq 'value (car tmp)))
					       (cdr-safe
						(assoc 'ack (car tmp)))
					       "unknown"))
				     (if (or (assq 'checked (car tmp))
					     (assq 'selected (car tmp)))
					 (cons 'checked "checked"))))
	      (w3-handle-text (concat " " (or
					   (cdr-safe (assq 'ack (car tmp)))
					   "unknown")))
	      (setq tmp (cdr tmp)))
	    (w3-handle-list-ending)
	    (w3-put-state :align old))
	(while (and (not default) tmp)
	  (if (or (assq 'checked (car tmp))
		  (assq 'selected (car tmp)))
	      (setq default (car tmp)))
	  (setq tmp (cdr tmp)))
	(setq default (cdr (assq 'ack (or default
					    (nth (1- (length opts)) opts))))
	      checked (mapcar
		       (function
			(lambda (x)
			  (cons (cdr-safe (assq 'ack x))
				(or (cdr-safe (assq 'value x))
				    (cdr-safe (assq 'ack x))))))
		       opts)
	      max-size (car (sort (mapcar
				   (function
				    (lambda (x)
				      (length (cdr-safe (assq 'ack x)))))
				   opts)
				  '>)))
	(if (and form args opts)
	    (let ((pos (point))
		  (siz (max max-size
			    (string-to-int
			     (or (cdr-safe (assq 'size args)) "0")))))
	      (w3-form-add-element 'option 
				   (or (cdr-safe (assq 'name args)) "option")
				   default
				   siz 
				   (string-to-int
				    (or (cdr-safe (assq 'maxlength args))
					"1000"))
				   default
				   (w3-get-state :form)
				   checked
				   (w3-get-state :formnum)
				   nil checked face)))))
    (w3-put-state :options nil)
    (w3-put-state :select nil)))

(defun w3-handle-option-data (&optional args)
  (let ((text (cond
	       ((null args) nil)
	       ((stringp args) args)
	       ((listp args) (mapconcat 'identity args " ")))))
    (if text
	(progn
	  (setq text (url-strip-leading-spaces
		      (url-eat-trailing-space text)))
	  (w3-put-state :options (cons (cons (cons 'ack text)
					      (w3-get-state :optargs))
					(w3-get-state :options))))))
  (put 'text 'w3-formatter 'w3-handle-text))
			   
(defun w3-handle-option (&optional args)
  (if (not (and (w3-get-state :form)
		(w3-get-state :select)))
      (w3-warn 'html
	       "<option> outside of a <form> or <select> construct - ERROR!!")
    (w3-put-state :optargs args)
    (put 'text 'w3-formatter 'w3-handle-option-data)))
			     
(defun w3-handle-select (&optional args)
  (if (not (w3-get-state :form))
      (w3-warn 'html "<select> outside of a <FORM> construct - ERROR!!")
    (w3-put-state :select args))
  )

(defun w3-handle-textarea (&optional args)
  (if (not (w3-get-state :form))
      (w3-warn 'html "<textarea> outside of a <FORM> construct - ERROR!!")
    (let ((node (assq 'maxlength args)))
      (cond
       ((null node)
	(setq args (cons (cons 'maxlength nil) args)))
       ((null (cdr-safe node))
	nil)
       ((string= (downcase (cdr-safe node)) "unlimited")
	(setcdr node nil))))
    (let* (
	   (face (let ((tag 'input)
		       (args nil))
		   (w3-face-for-element)))
	   (value (cdr-safe (assq 'data args)))
	   (type "TEXTAREA")
	   (name (cdr-safe (assq 'name args)))
	   (size (string-to-int (or (cdr-safe (assq 'size args)) "20")))
	   (maxlength (string-to-int
		       (or (cdr (assq 'maxlength args)) "10000")))
	   (default nil)
	   (action (w3-get-state :form))
	   (options)
	   (pos)
	   (num (w3-get-state :formnum))
	   (id (cdr-safe (assq 'id args)))
	   (checked (assq 'checked args)))
      (setq default value
	    pos (point))
      (put 'text 'w3-formatter 'w3-handle-text)
      (w3-form-add-element 'multiline name value size maxlength default
			   action options num id checked face))))

(defun w3-handle-label-text (&optional args)
  (setcdr (w3-get-state :label-text)
	  (concat (cdr (w3-get-state :label-text)) args))
  (w3-handle-text args))

(defun w3-handle-/label (&optional args)
  (let ((num (w3-get-state :formnum))
	(dat (w3-get-state :label-text)))
    (setq w3-form-labels (cons (cons (format "%d:%s" num (car dat))
				     (cdr dat))
			       w3-form-labels))
    (put 'text 'w3-formatter 'w3-handle-text)))

(defun w3-handle-label (&optional args)
  (if (not (w3-get-state :form))
      (w3-warn 'html "<label> outside of a <FORM> construct - ERROR!!")
    (put 'text 'w3-formatter 'w3-handle-label-text)
    (w3-put-state :label-text (cons (or (cdr-safe (assq 'for args))
					"Unknown label") ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For displaying the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-show-buffer ()
  (let ((potential-title
	 (and (not (w3-get-state :title))
	      (url-generate-new-buffer-name
	       (url-basepath url-current-file t)))))
    (if (and potential-title (string= potential-title ""))
	(setq potential-title
	      (url-generate-new-buffer-name url-current-file)))
    (if (and potential-title (not (string= potential-title "")))
	(rename-buffer potential-title)))
  (setq inhibit-read-only nil)
  (if url-find-this-link
      (w3-find-specific-link url-find-this-link))
  (let* ((tag 'html)
	 (args nil)
	 (face (cdr (w3-face-for-element))))
    (if (not face)
	(setq tag 'body
	      face (cdr (w3-face-for-element))))
    (and face
	 (if (not (fboundp 'valid-specifier-locale-p))
	     nil
	   (w3-my-safe-copy-face face 'default (current-buffer))))))

(defun w3-parse-header-link-items ()
  ;; Parse `url-current-mime-headers' and look for any <link> items
  (let ((items url-current-mime-headers)
	(node nil)
	(url nil)
	(type nil)
	(args nil)
	(title nil)
	(label nil))
    (while items
      (setq node (car items)
	    items (cdr items))
      (if (string= (car node) "link")
	  (progn
	    (setq args (mm-parse-args (cdr node))
		  type (if (assoc "rel" args) "rel" "rev")
		  label (cdr-safe (assoc type args))
		  title (cdr-safe (assoc "title" args))
		  url (car-safe (rassoc nil args)))
	    (if (string-match "^<.*>$" url)
		(setq url (substring url 1 -1)))
	    (and url label type
		 (w3-handle-link (list (cons "href" url)
				       (cons type label)
				       (cons "title" title)))))))))
     
(defun w3-refresh-buffer (&rest args)
  "Redraw the current buffer - this does not refetch or reparse the current
document, but uses the stored parse data."
  (interactive)
  (let ((buffer-read-only nil))
    (if (get-buffer url-working-buffer)
	(kill-buffer url-working-buffer))
    (error "Not yet reimplemented... sorry.")))

(defun w3-prepare-buffer (&rest args)
  ;; The text/html viewer - does all the drawing and displaying of the buffer
  ;; that is necessary to go from raw HTML to a good presentation.
  (let ((active-minibuffer-window
	 (if (minibuffer-window-active-p (minibuffer-window))
	     (minibuffer-window))))
    (let ((pop-up-windows nil))
      (if active-minibuffer-window
	  (let* ((current-buffer (current-buffer))
		 (window (get-buffer-window current-buffer t)))
	    (cond (window
		   (and (fboundp 'select-frame)
			(fboundp 'window-frame)
			(select-frame (window-frame window)))
		   (select-window window))
		  ((and (fboundp 'selected-frame)
			(fboundp 'window-frame)
			(eq (selected-frame)
			    (window-frame (minibuffer-window))))
		   ;; on minibuffer-only-frame
		   (select-frame (previous-frame))
		   (select-window (frame-first-window (selected-frame))))
		  ((fboundp 'frame-first-window)
		   (select-window (frame-first-window))))
	    (set-buffer current-buffer))))
    (let* ((source (buffer-string))
	   (parse (w3-preparse-buffer (current-buffer)))
	   (buff (car parse)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (set-buffer buff)
      (setq w3-current-source source
	    w3-current-parse w3-last-parse-tree)
      (w3-parse-header-link-items)
      (save-excursion
	(goto-char (point-max))
	(w3-handle-paragraph)
	(if (and (boundp 'w3-image-widgets-waiting) w3-image-widgets-waiting)
	    (let (url glyph widget)
	      (while w3-image-widgets-waiting
		(setq widget (car w3-image-widgets-waiting)
		      w3-image-widgets-waiting (cdr w3-image-widgets-waiting)
		      url (widget-get widget 'src)
		      glyph (cdr-safe (assoc url w3-graphics-list)))
		(widget-value-set widget glyph))))
	(w3-mode)
	(w3-handle-annotations)
	(w3-handle-headers)
	(set-buffer-modified-p nil)
	)
      (switch-to-buffer (current-buffer))
      (or active-minibuffer-window
	  (let ((window nil)
		(pop-up-windows nil))
	    (display-buffer (current-buffer))
	    (if (or w3-running-FSF19 w3-running-xemacs)
		(setq window (get-buffer-window (current-buffer) t))
	      (setq window (get-buffer-window (current-buffer))))
	    (select-window window)
	    (if (and (fboundp 'select-frame)
		     (fboundp 'window-frame))
		(select-frame (window-frame window)))))
      (goto-char (point-min))
      (w3-show-buffer)
      (if url-keep-history
	  (let ((url (url-view-url t)))
	    (if (not (url-hashtablep url-history-list))
		(setq url-history-list (url-make-hashtable 131)))
	    (url-puthash url (buffer-name) url-history-list)
	    (if (fboundp 'w3-shuffle-history-menu)
		(w3-shuffle-history-menu)))))
    (cond (active-minibuffer-window
	   (select-window active-minibuffer-window)
	   (sit-for 0)))))

(defun w3-handle-headers ()
  ;; Insert any headers the user wants to see into the current buffer.
  (let ((show w3-show-headers)
	(cur nil)
	(hdrs nil)
	(tag 'ol)
	(header nil)
	(w3-last-fill-pos (point-max))
	(val nil)
	(first t))
    (goto-char (point-max))
    (if (eq show t) (setq show '(".*")))
    (while show
      (setq cur (car show)
	    show (cdr show)
	    hdrs url-current-mime-headers)
      (while hdrs
	(setq header (car (car hdrs))
	      val (cdr (car hdrs))
	      hdrs (cdr hdrs))
	(if (numberp val) (setq val (int-to-string val)))
	(if (and (/= 0 (length header))
		 (string-match cur header))
	    (progn
	      (if first
		  (progn
		    (w3-handle-hr)
		    (w3-handle-list-opening '(("value" . 1)))
		    (setq tag 'li
			  first nil)))
	      (w3-handle-list-item)
	      (w3-handle-text (concat (capitalize header)
				      ": " val))))))
    (if (not first)			; We showed some headers
	(setq tag '/ol
	      tag (w3-handle-list-ending)))))

(defun w3-handle-annotations ()
  ;; Insert personal annotations into the current buffer
  (let ((annos (w3-fetch-personal-annotations))
	(tag nil))
    (if (not annos)
	nil				; No annotations
      (goto-char (cond
		  ((eq w3-annotation-position 'bottom) (point-max))
		  ((eq w3-annotation-position 'top) (point-min))
		  (t (message "Bad value for w3-annotation-position")
		     (point-max))))
      (w3-handle-div '((class . "annotations")))
      (w3-handle-hr '((width . "75%")
		      (label . " Personal Annotations ")
		      (align . "center")))
      (setq tag 'ol)
      (w3-handle-list-opening)
      (while annos
	(w3-handle-list-item)
	(w3-handle-hyperlink (list (cons 'href (car (car annos)))))
	(w3-handle-text (cdr (car annos)))
	(w3-handle-hyperlink-end)
	(setq annos (cdr annos)))
      (w3-handle-list-ending)
      (w3-handle-hr '((width . "75%")
		      (align . "center")))
      (w3-handle-/div)
      )))

(defun w3-fetch-personal-annotations ()
  ;; Grab any personal annotations for the current url
  (let ((url  (url-view-url t))
	(anno w3-personal-annotations)
	(annolist nil))
    (if (assoc url anno)
	(while anno
	  (if (equal (car (car anno)) url)
	      (setq annolist
		    (cons
		     (cons
		      (format "file:%s%s/PAN-%s.html"
			      (if (= ?/ (string-to-char
					 w3-personal-annotation-directory)) ""
				"/")
			      w3-personal-annotation-directory
			      (car (car (cdr (car anno)))))
		      (car (cdr (car (cdr (car anno))))))
		     annolist)))
	  (setq anno (cdr anno))))
    annolist))

(defun w3-normalize-spaces (string)
  ;; nuke spaces at the beginning
  (if (string-match "^[ \t\r\n]+" string)
      (setq string (substring string (match-end 0))))

  ;; nuke spaces in the middle
  (while (string-match "[ \t\r\n][ \r\t\n]+" string)
    (setq string (concat (substring string 0 (1+ (match-beginning 0)))
			 (substring string (match-end 0)))))

  ;; nuke spaces at the end
  (if (string-match "[ \t\n\r]+$" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun w3-upcase-region (st nd &optional end)
  (and st nd (upcase-region st nd)))

(provide 'w3-draw)

