;;; css.el -- Cascading Style Sheet parser
;; Author: wmperry
;; Created: 1996/12/26 16:49:58
;; Version: 1.18
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996 Free Software Foundation, Inc.
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (require 'cl)
  (require 'font)
  )

;; CBI = Cant Be Implemented - due to limitations in emacs/xemacs
;; NYI = Not Yet Implemented - due to limitations of space/time
;; NYPI = Not Yet Partially Implemented - possible partial support, eventually

(defconst css-properties
  '(;; Property name    Inheritable?   Type of data
    [font-family      nil            string-list]
    [font-style       nil            string]
    [font-variant     nil            symbol-list]
    [font-weight      nil            weight]
    [font-size        nil            length]
    [font             nil            font]
    [color            nil            color]
    [background       nil            color]
    [word-spacing     nil            length] ; CBI
    [letter-spacing   nil            length] ; CBI
    [text-decoration  nil            symbol-list]
    [vertical-align   nil            symbol] ; CBI
    [text-transform   nil            string]
    [text-align       t              symbol]
    [text-indent      t              length] ; NYI
    [line-height      t              length] ; CBI
    [margin           nil            margin]
    [margin-left      nil            margin]
    [margin-right     nil            margin]
    [margin-top       nil            margin]
    [margin-bottom    nil            margin]
    [padding          nil            padding]
    [padding-left     nil            padding]
    [padding-right    nil            padding]
    [padding-top      nil            padding]
    [padding-bottom   nil            padding]
    [border           nil            border]
    [border-left      nil            border]
    [border-right     nil            border]
    [border-top       nil            border]
    [border-bottom    nil            border]
    [width            nil            length] ; NYPI
    [height           nil            length] ; NYPI
    [float            nil            symbol]
    [clear            nil            symbol]
    [display          nil            symbol]
    [list-style       t              symbol] ;!! can't specify 'inside|outside'
    [white-space      t              symbol]

    ;; These are for specifying speech properties
    [voice-family     t              string]
    [gain             t              integer]
    [left-volume      t              integer]
    [right-volume     t              integer]
    [pitch            t              integer]
    [pitch-range      t              integer]
    [stress           t              integer]
    [richness         t              integer]
    )
  "A description of the various CSS properties and how to interpret them.")

(mapcar
 (lambda (entry)
   (put (aref entry 0) 'css-inherit (aref entry 1))
   (put (aref entry 0) 'css-type    (aref entry 2)))
 css-properties)

(defconst css-weights
  '(nil					;never used
    :extra-light
    :light
    :demi-light
    :medium
    :normal
    :demi-bold
    :bold
    :extra-bold
    )
  "List of CSS font weights.")

(defvar css-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "The syntax table for parsing stylesheets")

(modify-syntax-entry ?' "\"" css-syntax-table)
(modify-syntax-entry ?` "\"" css-syntax-table)
(modify-syntax-entry ?{ "(" css-syntax-table)
(modify-syntax-entry ?} ")" css-syntax-table)

(eval-when-compile
  (defvar css-scratch-val nil)
  (defvar css-scratch-id nil)
  (defvar css-scratch-class nil)
  (defvar css-scratch-possibles nil)
  (defvar css-scratch-current nil)
  (defvar css-scratch-classes nil)
  (defvar css-scratch-class-match nil)
  (defvar css-scratch-current-rule nil)
  (defvar css-scratch-current-value nil)
  )

(defconst css-running-xemacs
  (string-match "XEmacs" (emacs-version))
  "Whether we are running in XEmacs or not.")

(defvar css-ie-compatibility t
  "Whether we want to do Internet Explorer 3.0 compatible parsing of
CSS stylesheets.")

(defsubst css-replace-regexp (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun css-contextual-match (rule stack)
  (let ((ancestor)
	(p-args)
	(p-class)
	(matched t))
    (while rule
      (setq ancestor (assq (caar rule) stack))
      (if (not ancestor)
	  (setq rule nil
		matched nil)
	(setq p-args (cdr ancestor)
	      p-class (or (cdr-safe (assq 'class p-args)) t))
	(if (not (equal p-class (cdar rule)))
	    (setq matched nil
		  rule nil)))
      (setq rule (cdr rule)))
    matched))

(defsubst css-get-internal (tag args)
  (declare (special tag sheet element-stack default))
  (setq css-scratch-id (or (cdr-safe (assq 'id args))
			   (cdr-safe (assq 'name args)))
	css-scratch-class (or (cdr-safe (assq 'class args)) t)  
	css-scratch-possibles (cl-gethash tag sheet))
  (while css-scratch-possibles
    (setq css-scratch-current (car css-scratch-possibles)
	  css-scratch-current-rule (car css-scratch-current)
	  css-scratch-current-value (cdr css-scratch-current)
	  css-scratch-classes (if (listp (car css-scratch-current-rule))
				  (cdar css-scratch-current-rule)
				(cdr css-scratch-current-rule))
	  css-scratch-class-match t
	  css-scratch-possibles (cdr css-scratch-possibles))
    (if (eq t css-scratch-classes)
	(setq css-scratch-classes nil))
    (if (eq t css-scratch-class)
	(setq css-scratch-class nil))
    (while css-scratch-classes
      (if (not (member (pop css-scratch-classes) css-scratch-class))
	  (setq css-scratch-class-match nil
		css-scratch-classes nil)))
    (cond
     ((and (listp (car css-scratch-current-rule)) css-scratch-class-match)
      ;; Contextual!
      (setq css-scratch-current-rule (cdr css-scratch-current-rule))
      (if (css-contextual-match css-scratch-current-rule element-stack)
	  (setq css-scratch-val
		(append css-scratch-val css-scratch-current-value)))
      )
     (css-scratch-class-match
      (setq css-scratch-val (append css-scratch-val css-scratch-current-value))
      )
     (t
      nil))
    )
  )

(defsubst css-get (tag args &optional sheet element-stack)
  (setq css-scratch-val nil
	css-scratch-class (or (cdr-safe (assq 'class args)) t))

  ;; check for things without the class
  (if (listp css-scratch-class)
      (css-get-internal tag nil))

  ;; check for global class values
  (css-get-internal '*document args)

  ;; Now check for things with the class - they will be stuck on the front
  ;; of the list, which will mean we do the right thing
  (css-get-internal tag args)

  ;; Defaults are up to the calling application to provide
  css-scratch-val)

(defun css-ancestor-get (info ancestors sheet)
  ;; Inheritable property, check ancestors
  (let (cur)
    (while ancestors
      (setq cur (car ancestors)
 	    css-scratch-val (css-get info (car cur) (cdr cur) sheet)
 	    ancestors (if css-scratch-val nil (cdr ancestors)))))
  css-scratch-val)  

(defun css-split-selector (tag)
  ;; Return a list 
  (cond
   ((string-match " " tag)		; contextual
    (let ((tags (split-string tag "[ \t]+"))
	  (result nil))
      (while tags
	(setq result (cons (css-split-selector (car tags)) result)
	      tags (cdr tags)))
      result))
   ((string-match "[:\\.]" tag)
    (let ((tag (if (= (match-beginning 0) 0)
		   '*document
		 (intern (downcase (substring tag 0 (match-beginning 0))))))
	  (rest (substring tag (match-beginning 0) nil))
	  (classes nil))
      (while (string-match "^[:\\.][^:\\.]+" rest)
	(if (= ?. (aref rest 0))
	    (setq classes (cons (substring rest 1 (match-end 0)) classes))
	  (setq classes (cons (substring rest 0 (match-end 0)) classes)))
	(setq rest (substring rest (match-end 0) nil)))
      (setq classes (sort classes 'string-lessp))
      (cons tag classes)))
   ((string-match "^#" tag)		; id selector
    (cons '*document tag))
   (t
    (cons (intern (downcase tag)) t)
    )
   )
  )

(defun css-applies-to (st nd)
  (let ((results nil)
	(save-pos nil))
    (narrow-to-region st nd)
    (goto-char st)
    (skip-chars-forward " \t\r\n")
    (while (not (eobp))
      (setq save-pos (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " \r\t\n")
      (setq results (cons (css-split-selector
			   (buffer-substring save-pos (point))) results))
      (skip-chars-forward ", \t\r\n"))
    (widen)
    results))

(defun css-split-font-shorthand (font)
  ;; [<font-weight> || <font-style>]? <font-size> [ / <line-height> ]? <font-family>
  (let (weight size height family retval)
    (if (not (string-match " *\\([0-9.]+[^ /]+\\)" font))
	(error "Malformed font shorthand: %s" font))
    (setq weight (if (/= 0 (match-beginning 0))
		     (substring font 0 (match-beginning 0)))
	  size (match-string 1 font)
	  font (substring font (match-end 0) nil))
    (if (string-match " */ *\\([^ ]+\\) *" font)
	;; they specified a line-height as well
	(setq height (match-string 1 font)
	      family (substring font (match-end 0) nil))
      (if (string-match "^[ \t]+" font)
	  (setq family (substring font (match-end 0) nil))
	(setq family font)))
    (if weight (setq retval (cons (cons 'font-weight weight) retval)))
    (if size   (setq retval (cons (cons 'font-size size) retval)))
    (if height (setq retval (cons (cons 'line-height height) retval)))
    (if family (setq retval (cons (cons 'font-family family) retval)))
    retval))

(defun css-expand-length (spec)
  (cond
   ((not (stringp spec)) spec)
   ((string-equal spec "auto") nil)
   ((string-match "\([0-9]+\)%" spec)	; A percentage
    nil)
   ((string-match "\([0-9]+\)e[mn]" spec) ; Character based
    (string-to-int (substring spec (match-beginning 1) (match-end 1))))
   (t
    (truncate (font-spatial-to-canonical spec)))
   )
  )

(defsubst css-unhex-char (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defsubst css-pow (x n)
  (apply '* (make-list n x)))

(defun css-unhex (x)
  (let ((ord (length x))
	(rval 0))
    (while (> ord 0)
      (setq rval (+ rval
		    (* (css-pow 16 (- (length x) ord))
		       (css-unhex-char (aref x (1- ord)))))
	    ord (1- ord)))
    rval))

(defun css-expand-color (color)
  (cond
   ((string-match "^#" color)
    (let (r g b)
      (cond
       ((string-match "^#...$" color)
	;; 3-char rgb spec, expand out to six chars by replicating
	;; digits, not adding zeros.
	(setq r (css-unhex (make-string 2 (aref color 1)))
	      g (css-unhex (make-string 2 (aref color 2)))
	      b (css-unhex (make-string 2 (aref color 3)))))
       ((string-match "^#\\(..\\)\\(..\\)\\(..\\)$" color)
	(setq r (css-unhex (match-string 1 color))
	      g (css-unhex (match-string 2 color))
	      b (css-unhex (match-string 3 color))))
       (t
	(setq color (substring color 1))
	(let* ((n (/ (length color) 3))
	       (max (float (css-pow 16 n))))
	  (setq r (css-unhex (substring color 0 n))
		g (css-unhex (substring color n (* n 2)))
		b (css-unhex (substring color (* n 2) (* n 3)))
		r (round (* (/ r max) 255))
		g (round (* (/ g max) 255))
		b (round (* (/ b max) 255))))))
      (setq color (vector 'rgb r g b))))
   ((string-match "^rgb *( *\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\) *) *$" color)
    ;; rgb(r,g,b) 0 - 255, cutting off at 255
    (setq color (vector
		 'rgb
		 (min (string-to-int (match-string 1 color)) 255)
		 (min (string-to-int (match-string 2 color)) 255)
		 (min (string-to-int (match-string 3 color)) 255))))
   ((string-match "^rgb *( *\\([0-9]+\\) *%[, ]+\\([0-9]+\\) *%[, ]+\\([0-9]+\\) *% *) *$" color)
    ;; rgb(r%,g%,b%) 0 - 100%, cutting off at 100%
    (let ((r (min (string-to-number (match-string 1 color)) 100.0))
	  (g (min (string-to-number (match-string 2 color)) 100.0))
	  (b (min (string-to-number (match-string 3 color)) 100.0)))
      (setq r (round (* r 2.55))
	    g (round (* g 2.55))
	    b (round (* b 2.55))
	    color (vector 'rgb r g b))))
   ((string-match "url *(\\([^ )]+\\) *)" color)
    ;; A picture in the background
    (let ((pixmap (match-string 1 color))
	  (attributes nil))
      (setq color (concat (substring color 0 (match-beginning 0))
			  (substring color (match-end 0) nil))
	    attributes (split-string color " "))
      )
    )
    (t
     ;; Hmmm... pass it through unmangled and hope the underlying
     ;; windowing system can handle it.
     )
    )
  color
  )

(defun css-expand-value (type value)
  (case type
    ((symbol integer)			; Read it in
     (setq value (read (downcase value))))
    (symbol-list
     (setq value (downcase value)
	   value (split-string value "[ ,]+")
	   value (mapcar 'intern value)))
    (string-list
     (setq value (split-string value " *, *")))
    (color				; A color, possibly with URLs
     (setq value (css-expand-color value)))
    (length				; Pixels, picas, ems, etc.
     (setq value (css-expand-length value)))
    (font				; Font shorthand
     (setq value (css-split-font-shorthand value)))
    ((margin padding)			; length|percentage|auto {1,4}
     (setq value (split-string value "[ ,]+"))
     (if (/= 1 (length value))
	 ;; More than one value - a shortcut
	 (let* ((top (intern (format "%s-top" type)))
		(bottom (intern (format "%s-bottom" type)))
		(left (intern (format "%s-left" type)))
		(right (intern (format "%s-right" type))))
	   (setq top (cons top (css-expand-length (nth 0 value)))
		 right (cons right (css-expand-length (nth 1 value)))
		 bottom (cons bottom (css-expand-length (nth 2 value)))
		 left (cons left (css-expand-length (nth 3 value)))
		 value (list top right bottom left)))
       (setq value (css-expand-length (car value)))))
    (border
     (cond
      ((member (downcase value) '("none" "dotted" "dashed" "solid"
				  "double" "groove" "ridge" "inset" "outset"))
       (setq value (intern (downcase value))))
      ((string-match "^[0-9]+" value)
       (setq value (font-spatial-to-canonical value)))
      (t nil)))
    (weight				; normal|bold|bolder|lighter|[1-9]00
     (if (string-match "^[0-9]+" value)
	 (setq value (/ (read value) 100)
	       value (or (nth value css-weights) :bold))
       (setq value (intern (downcase (concat ":" value))))))
    (otherwise				; Leave it as is
     t)
    )
  value
  )

(defun css-parse-args (st &optional nd)
  ;; Return an assoc list of attribute/value pairs from a CSS style entry
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	)
    (save-excursion
      (if (stringp st)
	  (progn
	    (set-buffer (get-buffer-create " *css-style-temp*"))
	    (set-syntax-table css-syntax-table)
	    (erase-buffer)
	    (insert st)
	    (setq st (point-min)
		  nd (point-max)))
	(set-syntax-table css-syntax-table))
      (save-restriction
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward ";, \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t:=,;")
	  (downcase-region name-pos (point))
	  (setq name (intern (buffer-substring name-pos (point))))
	  (skip-chars-forward " \t\n")
	  (if (not (eq (char-after (point)) ?:)) ; There is no value
	      (setq value nil)
	    (skip-chars-forward " \t\n:")
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(if css-ie-compatibility
					    (skip-chars-forward "^;")
					  (skip-chars-forward "^,;"))
					(skip-chars-backward " \t")
					(point)))))))
	  (setq value (css-expand-value (get name 'css-type) value))
	  (if (eq (get name 'css-type) 'font)
	      (setq results (append value results))
	    (setq results (cons (cons name value) results)))
	  (skip-chars-forward ";, \n\t"))
	results))))

(defun css-handle-import ()
  (let ((url nil)
	(save-pos (point)))
    (if (looking-at "'\"")
	(condition-case ()
	    (forward-sexp 1)
	  (error (skip-chars-forward "^ \t\r\n;")))
      (skip-chars-forward "^ \t\r\n;"))
    (setq url (url-expand-file-name (buffer-substring save-pos (point))))
    (skip-chars-forward "\"; \t\r\n")
    (setq save-pos (point))
    (let ((url-working-buffer (generate-new-buffer-name " *styleimport*"))
	  (url-mime-accept-string
	   "text/css ; level=2")
	  (sheet nil))
      (save-excursion
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-be-asynchronous nil)
	(url-retrieve url)
	(css-clean-buffer)
	(setq sheet (buffer-string))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (insert sheet)
      (goto-char save-pos))))

(defun css-clean-buffer ()
  ;; Nuke comments, etc.
  (goto-char (point-min))
  (let ((save-pos nil))
    (while (search-forward "/*" nil t)
      (setq save-pos (- (point) 2))
      (delete-region save-pos
		     (if (search-forward "*/" nil t)
			 (point)
		       (end-of-line)
		       (point)))))
  (goto-char (point-min))
  (delete-matching-lines "^[ \t\r]*$")	; Nuke blank lines
  (css-replace-regexp "^[ \t\r]+" "")	; Nuke whitespace at beg. of line
  (css-replace-regexp "[ \t\r]+$" "")	; Nuke whitespace at end of line
  (goto-char (point-min)))

(defun css-active-device-types (&optional device)
  (let ((types (list 'normal 'default (if css-running-xemacs 'xemacs 'emacs)))
	(type (device-type device)))
    (cond
     ((featurep 'emacspeak)
      (setq types (cons 'speech types)))
     ((eq type 'tty)
      (if (and (fboundp 'tty-color-list)
	       (/= 0 (length (tty-color-list))))
	  (setq types (cons 'ansi-tty types))
	(setq types (cons 'tty types))))
     ((eq 'color (device-class))
      (if (not (device-bitplanes))
	  (setq types (cons 'color types))
	(setq types
	      (append
	       (list (intern (format "%dbit-color"
				     (device-bitplanes)))
		     (intern (format "%dbit"
				     (device-bitplanes)))
		     'color) types))
	(if (= 24 (device-bitplanes))
	    (setq types (cons 'truecolor types)))))
     ((eq 'grayscale (device-class))
      (setq types (append (list (intern (format "%dbit-grayscale"
						(device-bitplanes)))
				'grayscale)
			  types)))
     ((eq 'mono (device-class))
      (setq types (append (list 'mono 'monochrome) types)))
     (t
      (setq types (cons 'unknown types))))
    types))

(defmacro css-rule-specificity-internal (rule)
  (`
   (progn
     (setq tmp (cdr (, rule)))
     (if (listp tmp)
	 (while tmp
	   (if (= ?# (aref (car tmp) 0))
	       (incf a)
	     (incf b))
	   (setq tmp (cdr tmp)))))))

(defsubst css-specificity (rule)
  ;; To find specificity, according to the september 1996 CSS draft
  ;; a = # of ID attributes in the selector
  ;; b = # of class attributes in the selector
  ;; c = # of tag names in the selector
  (let ((a 0) (b 0) (c 0) cur tmp)
    (if (not (listp (car rule)))
	(css-rule-specificity-internal rule)
      (setq c (length rule))
      (while rule
	(css-rule-specificity-internal (pop rule))))
    (+ (* 100 a) (* 10 b) c)
    )
  )

(defun css-copy-stylesheet (sheet)
  (let ((new (make-hash-table :size (hash-table-count sheet))))
    (cl-maphash
     (function
      (lambda (k v)
	(cl-puthash k (copy-tree v) new))) sheet)
    new))

(defsubst css-store-rule (attrs applies-to)
  (declare (special sheet))
  (let (rules cur tag node)
    (while applies-to
      (setq cur (pop applies-to)
	    tag (car cur))
      (if (listp tag)
	  (setq tag (car tag)))
      (setq rules (cl-gethash tag sheet))
      (cond
       ((null rules)
	;; First rule for this tag.  Create new ruleset
	(cl-puthash tag (list (cons cur attrs)) sheet))
       ((setq node (assoc cur rules))
	;; Similar rule already exists, splice in our information
	(setcdr node (append attrs (cdr node))))
       (t
	;; First rule for this particular combination of tag/ancestors/class.
	;; Slap it onto the existing set of rules and push back into sheet.
	(setq rules (cons (cons cur attrs) rules))
	(cl-puthash tag rules sheet))
       )
      )
    )
  )

(defun css-parse (fname &optional string inherit)
  (let (
	(url-mime-accept-string
	 "text/css ; level=2")
	(save-pos nil)
	(applies-to nil)		; List of tags to apply style to
	(attrs nil)			; List of name/value pairs
	(att nil)
	(cur nil)
	(val nil)
	(device-type nil)
	(active-device-types (css-active-device-types (selected-device)))
	(sheet inherit))
    (if (not sheet)
	(setq sheet (make-hash-table :size 13 :test 'eq)))
    (save-excursion
      (set-buffer (get-buffer-create
		   (generate-new-buffer-name " *style*")))
      (set-syntax-table css-syntax-table)
      (erase-buffer)
      (if fname (url-insert-file-contents fname))
      (goto-char (point-max))
      (if string (insert string))
      (css-clean-buffer)
      (goto-char (point-min))
      (while (not (eobp))
	(setq save-pos (point))
	(cond
	 ;; *sigh* SGML comments are being used to 'hide' data inlined
	 ;; with the <style> tag from older browsers.
	 ((or (looking-at "<!--+")	; begin
	      (looking-at "--+>"))	; end
	  (goto-char (match-end 0)))
	 ;; C++ style comments, and we are doing IE compatibility
	 ((and (looking-at "//") css-ie-compatibility)
	  (end-of-line))
	 ;; Pre-Processor directives
	 ((looking-at "[ \t\r]*@\\([^ \t\r\n]\\)")
	  (let ((directive nil))
	    (skip-chars-forward " @\t\r") ; Past any leading whitespace
	    (setq save-pos (point))
	    (skip-chars-forward "^ \t\r\n") ; Past the @ directive
	    (downcase-region save-pos (point))
	    (setq directive (buffer-substring save-pos (point)))
	    (skip-chars-forward " \t\r") ; Past any trailing whitespace
	    (setq save-pos (point))
	    (cond
	     ((string= directive "import")
	      (css-handle-import))
	     (t
	      (message "Unknown directive in stylesheet: @%s" directive)))))
	 ;; Giving us some output device information
	 ((looking-at "[ \t\r]*:\\([^: \n]+\\):")
	  (downcase-region (match-beginning 1) (match-end 1))
	  (setq device-type (intern (buffer-substring (match-beginning 1)
						      (match-end 1))))
	  (goto-char (match-end 0))
	  (if (not (memq device-type active-device-types))
	      ;; Not applicable to us... skip the info
	      (progn
		(if (re-search-forward ":[^:{ ]*:" nil t)
		    (goto-char (match-beginning 0))
		  (goto-char (point-max))))))
	 ;; Default is to treat it like a stylesheet declaration
	 (t
	  (skip-chars-forward "^{")
	  ;;(downcase-region save-pos (point))
	  (setq applies-to (css-applies-to save-pos (point)))
	  (skip-chars-forward "^{")
	  (setq save-pos (point))
	  (condition-case ()
	      (forward-sexp 1)
	    (error (goto-char (point-max))))
	  (end-of-line)
	  (skip-chars-backward "\r}")
	  (subst-char-in-region save-pos (point) ?\n ? )
	  (subst-char-in-region save-pos (point) ?\r ? )
	  ;; This is for not choking on garbage at the end of the buffer.
	  ;; I get bit by this every once in a while when going through my
	  ;; socks gateway.
	  (if (eobp)
	      nil
	    (setq attrs (css-parse-args (1+ save-pos) (point)))
	    (skip-chars-forward "}\r\n")
	    (css-store-rule attrs applies-to))
	  )
	 )
	(skip-chars-forward " \t\r\n"))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    sheet)
  )

;; Tools for pretty-printing an existing stylesheet.
(defun css-rule-name (rule)
  (cond
   ((listp (car rule))			; Contextual
    (mapconcat 'css-rule-name 
	       (reverse rule) " "))
   ((listp (cdr rule))			; More than one class
    (let ((classes (cdr rule))
	  (rval (symbol-name (car rule))))
      (while classes
	(setq rval (concat rval
			   (if (= (aref (car classes) 0) ?:)
			       (pop classes)
			     (concat "." (pop classes))))))
      rval))
   (t
    (symbol-name (car rule)))))

(defun css-display (sheet)
  (with-output-to-temp-buffer "CSS Stylesheet"
    (set-buffer standard-output)
    (indented-text-mode)
    (insert "# Stylesheet auto-regenerated by css.el\n#\n"
	    "# This is a mixture of the default stylesheet and any\n"
	    "# styles specified by the document.  The rules are in no\n"
	    "# particular order.\n\n")
    (let (tmp cur goal-col)
      (cl-maphash
       (function
	(lambda (k v)
	  (while v
	    (setq cur (pop v))
	    (insert (css-rule-name (car cur)))
	    (insert " { ")
	    (setq goal-col (point))
	    (insert "\n")
	    ;; Display the rules
	    (setq tmp (cdr cur))
	    (let (prop val)
	      (while tmp
		(setq prop (caar tmp)
		      val (cdar tmp)
		      tmp (cdr tmp))
		(case (get prop 'css-type)
		  (symbol-list
		   (setq val (mapconcat 'symbol-name val ",")))
		  (weight
		   (setq val (substring (symbol-name val) 1 nil)))
		  (otherwise
		   nil)
		  )
		(insert (format "  %s: %s;\n" prop val))))
	    (insert "}\n\n");
	    )))
       sheet))))

(provide 'css)
