;;; w3-forms.el --- Emacs-w3 forms parsing code for new display engine
;; Author: wmperry
;; Created: 1997/01/02 20:20:29
;; Version: 1.32
;; Keywords: faces, help, comm, data, languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996 Free Software Foundation, Inc.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMS processing for html 2.0/3.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (require 'w3-display)
  (require 'widget))

(require 'w3-vars)
(require 'mule-sysdp)

;; A form entry area is a vector
;; [ type name default-value value maxlength options widget]
;; Where:
;;          type = symbol defining what type of form entry area it is
;;                 (ie: file, radio)
;;          name = the name of the form element
;; default-value = the value this started out with

(defsubst w3-form-element-type          (obj) (aref obj 0))
(defsubst w3-form-element-name          (obj) (aref obj 1))
(defsubst w3-form-element-default-value (obj) (aref obj 2))
(defsubst w3-form-element-value         (obj) (aref obj 3))
(defsubst w3-form-element-size          (obj) (aref obj 4))
(defsubst w3-form-element-maxlength     (obj) (aref obj 5))
(defsubst w3-form-element-options       (obj) (aref obj 6))
(defsubst w3-form-element-action        (obj) (aref obj 7))
(defsubst w3-form-element-widget        (obj) (aref obj 8))

(defsubst w3-form-element-set-type          (obj val) (aset obj 0 val))
(defsubst w3-form-element-set-name          (obj val) (aset obj 1 val))
(defsubst w3-form-element-set-default-value (obj val) (aset obj 2 val))
(defsubst w3-form-element-set-value         (obj val) (aset obj 3 val))
(defsubst w3-form-element-set-size          (obj val) (aset obj 4 val))
(defsubst w3-form-element-set-maxlength     (obj val) (aset obj 5 val))
(defsubst w3-form-element-set-options       (obj val) (aset obj 6 val))
(defsubst w3-form-element-set-action        (obj val) (aset obj 7 val))
(defsubst w3-form-element-set-widget        (obj val) (aset obj 8 val))

;; The main function - this adds a single widget to the form
(defun w3-form-add-element (type name value size maxlength default
				 action options number id checked
				 face)
  (let* ((name (or name (case type
			      ((submit reset) nil)
			      (otherwise (symbol-name type)))))
	 (el (vector type
		     name
		     default
		     value
		     size
		     maxlength
		     options
		     action nil))
	 (size (if size
		   (+ 2 size)
		 (case type
		   ((checkbox radio) 3)
		   ((reset submit)
		    (+ 2 (length (or value (symbol-name type)))))
		   (multiline 21)
		   (hidden nil)
		   (otherwise 22))))
	 )
    (if size
	(set-text-properties (point)
			     (progn (insert-char ?T size) (point))
			     (list 'w3-form-info el
				   'start-open t
				   'end-open t
				   'rear-nonsticky t)))))

(defun w3-form-resurrect-widgets ()
  (let ((st (point-min))
	info nd node action)
    (while st
      (if (setq info (get-text-property st 'w3-form-info))
	  (progn
	    (setq nd (next-single-property-change st 'w3-form-info)
		  action (w3-form-element-action info)
		  node (assoc action w3-form-elements))
	    (goto-char st)
	    (delete-region st nd)
	    (if (not (w3-form-element-size info))
		(w3-form-element-set-size info 20))
	    (if node
		(setcdr node (cons info (cdr node)))
	      (setq w3-form-elements (cons (cons action (list info))
					   w3-form-elements)))
	    (w3-form-add-element-internal info)
	    (setq st (next-single-property-change st 'w3-form-info)))
	(setq st (next-single-property-change st 'w3-form-info))))))

(defun w3-form-add-element-internal (el)
  (let* ((widget nil)
	 (buffer-read-only nil)
	 (inhibit-read-only t)
	 (widget-creation-function nil))
    (setq widget-creation-function (or (get (w3-form-element-type el)
					    'w3-widget-creation-function)
				       'w3-form-default-widget-creator)
	  widget (funcall widget-creation-function el nil))
    (if (not widget)
	nil
      (w3-form-element-set-widget el widget)
      (widget-put widget 'w3-form-data el))))

;; These properties tell the add-element function how to actually create
;; each type of widget.
(put 'checkbox  'w3-widget-creation-function 'w3-form-create-checkbox)
(put 'multiline 'w3-widget-creation-function 'w3-form-create-multiline)
(put 'radio     'w3-widget-creation-function 'w3-form-create-radio-button)
(put 'reset     'w3-widget-creation-function 'w3-form-create-submit-button)
(put 'submit    'w3-widget-creation-function 'w3-form-create-submit-button)
(put 'hidden    'w3-widget-creation-function 'ignore)
(put 'file      'w3-widget-creation-function 'w3-form-create-file-browser)
(put 'option    'w3-widget-creation-function 'w3-form-create-option-list)
(put 'keygen    'w3-widget-creation-function 'w3-form-create-keygen-list)
(put 'button    'w3-widget-creation-function 'w3-form-create-button)
(put 'image	'w3-widget-creation-function 'w3-form-create-image)

(defun w3-form-create-checkbox (el face)
  (widget-create 'checkbox :value-face face
		 (and (w3-form-element-default-value el) t)))

(defun w3-form-create-radio-button (el face)
  (let* ((name (w3-form-element-name el))
	 (formobj (cdr (assoc name w3-form-radio-elements)))
	 (widget nil)
	 )
    (if formobj
	(progn
	  (setq widget (w3-form-element-widget formobj))
	  (widget-radio-add-item widget
				 (list 'item
				       :format "%t"
				       :tag ""
				       :value (w3-form-element-value el)))
	  (if (w3-form-element-default-value el)
	      (widget-value-set widget (w3-form-element-value el)))
	  nil)
      (setq widget (widget-create 'radio-button-choice
				  :value (w3-form-element-value el)
				  (list 'item
					:format "%t"
					:tag ""
					:value (w3-form-element-value el)))
	    w3-form-radio-elements (cons (cons name el)
					 w3-form-radio-elements))
      widget)))

(defun w3-form-create-button (el face)
  ;; This handles dealing with the bogus Netscape 'button' input type
  ;; that lots of places have been using to slap javascript shit onto
  (let ((val (w3-form-element-value el)))
    (if (or (not val) (string= val ""))
	(setq val "Push Me"))
    (widget-create 'push-button :notify 'ignore :button-face face val)))

(defun w3-form-create-image (el face)
  (let ((widget (widget-create 'push-button
			       :notify 'w3-form-submit/reset-callback
			       :value "Form-Image")))
    widget))

(defun w3-form-create-submit-button (el face)
  (let ((val (w3-form-element-value el)))
    (if (or (not val) (string= val ""))
	(setq val (if (eq (w3-form-element-type el) 'submit)
		      "Submit"
		    "Reset")))
    (widget-create 'push-button
		   :notify 'w3-form-submit/reset-callback
		   :button-face face val)))

(defun w3-form-create-file-browser (el face)
  (widget-create 'file :value-face face :value (w3-form-element-value el)))

(defvar w3-form-valid-key-sizes
  '(
    ("1024 (Premium)" . 1024)
    ("896 (Regular)" . 896)
    ("768 (Unleaded)" . 768)
    ("512 (Low Grade)" . 512)
    ("508 (Woos)" . 508)
    ("256 (Test Grade)" . 256)
    )
  "An assoc list of available key sizes and meaningful descriptions.")
   
(defun w3-form-create-keygen-list (el face)
  (let ((tmp w3-form-valid-key-sizes)
	(longest 0)
	(options nil))
    (while tmp
      (if (> (length (caar tmp)) longest)
	  (setq longest (length (caar tmp))))
      (setq options (cons (list 'choice-item :tag (caar tmp)
				:value (cdar tmp)) options)
	    tmp (cdr tmp)))
    (apply 'widget-create 'menu-choice
	   :value 1024
	   :ignore-case t
	   :tag "Key Length"
	   :size (1+ longest)
	   :value-face face
	   options)))

(defun w3-form-create-option-list (el face)
  (let ((widget (apply 'widget-create 'menu-choice
		       :value (w3-form-element-value el)
		       :ignore-case t
		       :tag "Choose"
		       :format "%v"
		       :size (w3-form-element-size el)
		       :value-face face
		       (mapcar
			(function
			 (lambda (x)
			   (list 'choice-item :format "%[%t%]"
				 :tag (car x) :value (car x))))
			(w3-form-element-options el)))))
    (widget-value-set widget (w3-form-element-value el))
    widget))

;(defun w3-form-create-multiline (el face)
;  ;; FIX THIS! - need to padd out with newlines or something...
;  (widget-create 'field :value-face face (w3-form-element-value el)))

(defun w3-form-create-multiline (el face)
  (widget-create 'push-button :notify 'w3-do-text-entry "Multiline text area"))

(defun w3-form-default-widget-creator (el face)
  (widget-create 'link
		 :notify 'w3-form-default-button-callback
		 :size (w3-form-element-size el)
		 :tag (mule-truncate-string (w3-form-element-value el)
					    (w3-form-element-size el) ?_)
		 :value-face face
		 (w3-form-element-value el)))

(defun w3-form-default-button-callback (widget &rest ignore)
  (let* ((obj (widget-get widget 'w3-form-data))
	 (typ (w3-form-element-type obj))
	 (def (widget-value widget))
	 (val nil)
	 )
    (case typ
      (password
       (setq val (funcall url-passwd-entry-func "Password: " def))
       (widget-put widget :tag (mule-truncate-string
				(make-string (length val) ?*)
				(w3-form-element-size obj) ?_)))
      (otherwise
       (setq val (read-string
		  (concat (capitalize (symbol-name typ)) ": ") def))
       (widget-put widget :tag (mule-truncate-string
				val (w3-form-element-size obj) ?_))))
    (widget-value-set widget val))
  (apply 'w3-form-possibly-submit widget ignore))

(defun w3-form-possibly-submit (widget &rest ignore)
  (let* ((formobj (widget-get widget 'w3-form-data))
	 (ident (w3-form-element-action formobj))
	 (widgets (w3-all-widgets ident))
	 (text-fields 0)
	 (text-p nil))
    ;;
    ;; Gack.  Netscape auto-submits forms of one text field
    ;; here we go through the list of widgets in this form and
    ;; determine which are not submit/reset/button inputs.
    ;; If the # == 1, then submit the form.
    ;;
    (while widgets
      (setq text-fields (+
			 text-fields
			 (case (w3-form-element-type (car widgets))
			   ((submit reset image button)
			    0)
			   (text
			    (setq text-p t)
			    1)
			   (otherwise
			    1)))
	    widgets (cdr widgets)))
    (if (and (= text-fields 1) text-p)
	(w3-submit-form ident))))

(defun w3-form-submit/reset-callback (widget &rest ignore)
  (let* ((formobj (widget-get widget 'w3-form-data))
	 (w3-submit-button formobj))
    (case (w3-form-element-type formobj)
      (submit (w3-submit-form (w3-form-element-action formobj)))
      (reset  (w3-revert-form (w3-form-element-action formobj)))
      (image  (w3-submit-form (w3-form-element-action formobj)))
      (otherwise
       (error
	"Impossible widget type %s triggered w3-form-submit/reset-callback"
	(w3-form-element-type formobj))))))

(defun w3-do-text-entry (widget &rest ignore)
  (let* ((data (list widget (current-buffer)))
	 (formobj (widget-get widget 'w3-form-data))
	 (buff (get-buffer-create (format "Form Entry: %s"
					  (w3-form-element-name formobj)))))
    (switch-to-buffer-other-window buff)
    (indented-text-mode)
    (erase-buffer)
    (if (w3-form-element-value formobj)
	(insert (w3-form-element-value formobj)))
    (setq w3-current-last-buffer data)
    (message "Press C-c C-c when finished with text entry.")
    (local-set-key "\C-c\C-c" 'w3-finish-text-entry)))

(defun w3-finish-text-entry ()
  (interactive)
  (if w3-current-last-buffer
      (let* ((widget (nth 0 w3-current-last-buffer))
	     (formobj (widget-get widget 'w3-form-data))
	     (buff (nth 1 w3-current-last-buffer))
	     (valu (buffer-string))
	     (inhibit-read-only t)
	     )
	(local-set-key "\C-c\C-c" 'undefined)
	(kill-buffer (current-buffer))
	(condition-case ()
	    (delete-window)
	  (error nil))
	(if (not (and buff (bufferp buff) (buffer-name buff)))
	    (message "Could not find the form buffer for this text!")
	  (switch-to-buffer buff)
	  (w3-form-element-set-value formobj valu)))))

(defsubst w3-all-widgets (actn)
  ;; Return a list of data entry widgets in form number ACTN
  (cdr-safe (assoc actn w3-form-elements)))

(defun w3-revert-form (actn)
  (save-excursion
    (let* ((formobjs (w3-all-widgets actn))
	   (inhibit-read-only t)
	   deft type widget formobj)
      (while formobjs
	(setq formobj (car formobjs)
	      widget (w3-form-element-widget formobj)
	      formobjs (cdr formobjs)
	      deft (w3-form-element-default-value formobj)
	      type (w3-form-element-type formobj))
	(case type
	  ((submit reset image) nil)
	  (radio
	   ;; Ack - how!?
	   )
	  (checkbox
	   (if deft
	       (widget-value-set widget t)
	     (widget-value-set widget nil)))
	  (file
	   (widget-value-set widget deft))
	  (otherwise
	   (widget-value-set widget deft)))))))

(defun w3-form-encode-helper (formobjs)
  (let (
	(submit-button-data w3-submit-button)
	formobj result widget temp type)
    (while formobjs
      (setq formobj (car formobjs)
	    type (w3-form-element-type formobj)
	    widget (w3-form-element-widget formobj)
	    formobjs (cdr formobjs)
	    temp (case type
		   (reset nil)
		   (image
		    (if (and (eq submit-button-data formobj)
			     (w3-form-element-name formobj))
			(setq result (append
				      (list
				       (cons
					(concat (w3-form-element-name formobj)
						".x") "0")
				       (cons
					(concat (w3-form-element-name formobj)
						".y") "0"))
				      result)))
		    nil)
		   (submit
		    (if (and (eq submit-button-data formobj)
			     (w3-form-element-name formobj))
			(cons (w3-form-element-name formobj)
			      (w3-form-element-value formobj))))
		   (radio
		    (let* ((radio-name (w3-form-element-name formobj))
			   (radio-object (cdr-safe
					  (assoc radio-name
						 w3-form-radio-elements)))
			   (chosen-widget (and radio-object
					       (widget-radio-chosen
						(w3-form-element-widget
						 radio-object)))))
		      (if (assoc radio-name result)
			  nil
			(cons radio-name (widget-value chosen-widget)))))
		   (checkbox
		    (if (widget-value widget)
			(cons (w3-form-element-name formobj)
			      (w3-form-element-value formobj))))
		   (file
		    (let ((dat nil)
			  (fname (widget-value widget)))
		      (save-excursion
			(set-buffer (get-buffer-create " *w3-temp*"))
			(erase-buffer)
			(setq dat
			      (condition-case ()
				  (insert-file-contents-literally fname)
				(error (concat "Error accessing " fname))))
			(cons (w3-form-element-name formobj) dat))))
		   (option
		    (cons (w3-form-element-name formobj)
			  (cdr-safe
			   (assoc (widget-value widget)
				  (w3-form-element-options formobj)))))
		   (keygen
		    (cons (w3-form-element-name formobj)
			  (format "Should create a %d bit RSA key"
				  (widget-value widget))))
		   ((multiline hidden)
		    (cons (w3-form-element-name formobj)
			  (w3-form-element-value formobj)))
		   (otherwise
		    (cons (w3-form-element-name formobj)
			  (widget-value widget)))))
      (if temp
	  (setq result (cons temp result))))
    result))

(defun w3-form-encode-make-mime-part (id data separator)
  (concat separator "\nContent-id: " id
	  "\nContent-length: " (length data)
	  "\n\n" data))

(defun w3-form-encode-multipart/x-www-form-data (formobjs)
  ;; Create a multipart form submission.
  ;; Returns a cons of two strings.  Car is the separator used.
  ;; cdr is the body of the MIME message."
  (let ((separator "---some-separator-for-www-form-data"))
    (cons separator
	  (mapconcat
	   (function
	    (lambda (formobj)
	      (w3-form-encode-make-mime-part (car formobj) (cdr formobj)
					     separator)))
	   (w3-form-encode-helper formobjs)
	   "\n"))))

(fset 'w3-form-encode-multipart/form-data
      'w3-form-encode-multipart/x-www-form-data)
(fset 'w3-form-encode- 'w3-form-encode-application/x-www-form-urlencoded)

(defun w3-next-widget (pos)
  (let* ((next (cond ((get-text-property pos 'button)
		      (next-single-property-change pos 'button))
		     ((get-text-property pos 'field)
		      (next-single-property-change pos 'field))
		     (t pos)))
	 (button (and next (next-single-property-change next 'button)))
	 (field  (and next (next-single-property-change next 'field))))
    (setq next
	  (cond
	   ((and button field) (min button field))
	   (button button)
	   (field field)
	   (t nil)))
    (and next
	 (or (get-text-property next 'button)
	     (get-text-property next 'field)))))

(defun w3-form-encode (result &optional enctype)
  "Create a string suitably encoded for a URL request."
  (let ((func (intern (concat "w3-form-encode-" enctype))))
    (if (fboundp func)
	(funcall func result)
      (w3-warn 'html (format "Bad encoding type for form data: %s" enctype))
      (w3-form-encode-application/x-www-form-urlencoded result))))

(defun w3-form-encode-text/plain (result)
  (let ((query ""))
    (setq query
	  (mapconcat
	   (function
	    (lambda (widget)
	      (let ((nam (car widget))
		    (val (cdr widget)))
		(if (string-match "\n" nam)
		    (setq nam (mapconcat
			       (function
				(lambda (x)
				  (if (= x ?\n) "," (char-to-string x))))
			       nam "")))
		(concat nam " " val))))
	   (w3-form-encode-helper result) "\n"))
    query))

(defun w3-form-encode-application/x-w3-wais (result)
  (cdr (car (w3-form-encode-helper result))))

(defun w3-form-encode-application/x-gopher-query (result)
  (concat "\t" (cdr (car (w3-form-encode-helper result)))))

(defun w3-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  (mapconcat
   (function
    (lambda (char)
      (cond
       ((= char ?  ) "+")
       ((memq char url-unreserved-chars) (char-to-string char))
       (t (upcase (format "%%%02x" char))))))
    (mule-encode-string chunk) ""))

(defun w3-form-encode-application/x-www-form-urlencoded (result)
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   (w3-form-encode-helper result) "&"))

(defun w3-form-encode-application/x-w3-isindex (result)
  (let* ((info (w3-form-encode-helper result))
	 (query (cdr-safe (assoc "isindex" info))))
    (if query
	(url-hexify-string query)
      "")))

(defun w3-form-encode-application/gopher-ask-block (result)
  (let ((query ""))
    ;;; gopher+ will expect all the checkboxes/etc, even if they are
    ;;; not turned on.  Should still ignore RADIO boxes that are not
    ;;; active though.
  (while result
    (if (and (not (and (string= (nth 2 (car result)) "RADIO")
		       (not (nth 6 (car result)))))
	     (not (member (nth 2 (car result)) '("SUBMIT" "RESET"))))
	(setq query (format "%s\r\n%s" query (nth 5 (car result)))))
    (setq result (cdr result)))
  (concat query "\r\n.\r\n")))

(defun w3-submit-form (ident)
  ;; Submit form entry fields matching ACTN as their action identifier.
  (let* ((result (w3-all-widgets ident))
	 (enctype (or (cdr (assq 'enctype ident))
		      "application/x-www-form-urlencoded"))
	 (query (w3-form-encode result enctype))
	 (themeth (upcase (or (cdr (assq 'method ident)) "get")))
	 (theurl (cdr (assq 'action ident))))
    (if (and (string= "GET" themeth)
	     (string-match "\\([^\\?]*\\)\\?" theurl))
	(setq theurl (url-match theurl 1)))
    (cond
     ((or (string= "POST" themeth)
	  (string= "PUT" themeth))
      (if (consp query)
	  (setq enctype (concat enctype "; separator=\""
				(substring (car query) 3 nil)
				"\"")
		query (cdr query)))
      (let ((url-request-method themeth)
	    (url-request-data query)
	    (url-request-extra-headers
	     (cons (cons "Content-type" enctype) url-request-extra-headers)))
	(w3-fetch theurl)))
     ((string= "GET" themeth)
      (let ((theurl (concat theurl (if (string-match "gopher" enctype)
				       "" "?") query)))
	(w3-fetch theurl)))
     (t
      (w3-warn 'html (format "Unknown submit method: %s" themeth))
      (let ((theurl (concat theurl "?" query)))
	(w3-fetch theurl))))))

(provide 'w3-forms)
