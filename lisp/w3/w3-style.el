;;; w3-style.el --- Emacs-W3 binding style sheet mechanism
;; Author: wmperry
;; Created: 1996/08/12 03:10:30
;; Version: 1.13
;; Keywords: faces, hypermedia

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
;;; A style sheet mechanism for emacs-w3
;;;
;;; This will eventually be able to under DSSSL[-lite] as well as the
;;; experimental W3C mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font)
(require 'w3-keyword)
(require 'cl)

(defvar w3-style-css-syntax-table
  (copy-syntax-table mm-parse-args-syntax-table)
  "The syntax table for parsing stylesheets")

(defvar w3-style-ie-compatibility nil
  "*Whether we want to do Internet Explorer 3.0 compatible parsing of
CSS stylesheets.")

(defun w3-style-css-parse-args (st &optional nd defines)
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
	    (set-buffer (get-buffer-create " *w3-style-temp*"))
	    (set-syntax-table w3-style-css-syntax-table)
	    (erase-buffer)
	    (insert st)
	    (setq st (point-min)
		  nd (point-max)))
	(set-syntax-table w3-style-css-syntax-table))
      (save-restriction
	(if (< nd st)
	    (narrow-to-region nd nd)
	  (narrow-to-region st nd))
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward ";, \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t:=,;")
	  (downcase-region name-pos (point))
	  (setq name (buffer-substring name-pos (point)))
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
					(if w3-style-ie-compatibility
					    (skip-chars-forward "^;")
					  (skip-chars-forward "^,;"))
					(skip-chars-backward " \t")
					(point)))))))
	  (setq results (cons (cons name value) results))
	  (skip-chars-forward ";, \n\t"))
	results))))

(defvar w3-style-css-define-table nil)

(defun w3-style-css-handle-define ()
  (let ((name nil)
	(save-pos (point))
	(retval nil))
    (skip-chars-forward "^ \t\r\n") ; Past the name token
    (downcase-region save-pos (point))
    (setq name (buffer-substring save-pos (point)))
    (skip-chars-forward "= \t\r")
    (setq save-pos (point))
    (skip-chars-forward "^;")
    (setq retval (cons name (buffer-substring save-pos (point))))
    (skip-chars-forward " \t\r\n")
    retval))

(defun w3-style-css-handle-import ()
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
    (let ((url-working-buffer (url-generate-new-buffer-name " *styleimport*"))
	  (url-mime-accept-string
	   "text/css ; level=2")
	  (sheet nil))
      (save-excursion
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-be-asynchronous nil)
	(url-retrieve url)
	(w3-style-css-clean)
	(setq sheet (buffer-string))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (insert sheet)
      (goto-char save-pos))))

(defun w3-style-css-clean ()
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
  (w3-replace-regexp "^[ \t\r]+" "")	; Nuke whitespace at beg. of line
  (w3-replace-regexp "[ \t\r]+$" "")	; Nuke whitespace at end of line
  (goto-char (point-min)))

(defun w3-style-css-applies-to (st nd)
  (let ((results nil)
	(save-pos nil))
    (narrow-to-region st nd)
    (goto-char st)
    (skip-chars-forward " \t\r\n")
    (while (not (eobp))
      (setq save-pos (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " \r\t\n")
      (setq results (cons (buffer-substring save-pos (point)) results))
      (skip-chars-forward ", \t\r\n"))
    (widen)
    results))

(defun w3-style-active-device-types (&optional device)
  (let ((types (list 'normal 'default (if w3-running-xemacs 'xemacs 'emacs)))
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

(defun w3-style-parse-css (fname &optional string inherit)
  (let (
	(url-mime-accept-string
	 "text/css ; level=2")
	(save-pos nil)
	(applies-to nil)		; List of tags to apply style to
	(attrs nil)			; List of name/value pairs
	(tag nil)
	(att nil)
	(cur nil)
	(val nil)
	(class nil)
	(defines nil)
	(device-type nil)
	(active-device-types (w3-style-active-device-types (selected-device)))
	(sheet inherit))
    (save-excursion
      (set-buffer (get-buffer-create
		   (url-generate-new-buffer-name " *style*")))
      (set-syntax-table w3-style-css-syntax-table)
      (erase-buffer)
      (if fname (url-insert-file-contents fname))
      (goto-char (point-max))
      (if string (insert string))
      (w3-style-css-clean)
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
	 ((and (looking-at "//") w3-style-ie-compatibility)
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
	     ((string= directive "define")
	      (let ((retval (w3-style-css-handle-define)))
		(and defines
		     (setq defines (cons retval defines)))))
	     ((string= directive "import")
	      (w3-style-css-handle-import))
	     (t
	      (w3-warn 'style (format "Unknown directive: @%s" directive)
		       'warning)))))
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
	  (setq applies-to (w3-style-css-applies-to save-pos (point)))
	  (skip-chars-forward "^{")
	  (setq save-pos (point))
	  (forward-sexp 1)
	  (end-of-line)
	  (skip-chars-backward "\r}")
	  (subst-char-in-region save-pos (point) ?\n ? )
	  (subst-char-in-region save-pos (point) ?\r ? )
	  (setq attrs (w3-style-css-parse-args (1+ save-pos)
					   (point) defines))
	  (skip-chars-forward "}\r\n")
	  (while applies-to
	    (setq cur (car applies-to)
		  applies-to (cdr applies-to))
	    (cond
	     ((string-match "\\([^.]*\\)\\.\\(.*\\)" cur) ; Normal class
	      (setq tag (intern (downcase (match-string 1 cur)))
		    class (match-string 2 cur)))
	     ((string-match "\\(.*\\):\\(.*\\)" cur) ; Pseudo class
	      (setq tag (intern (downcase (match-string 1 cur)))
		    class (match-string 2 cur)))
	     (t				; No class - global
	      (setq tag (intern (downcase cur))
		    class 'internal)))
	    (let ((loop attrs))
	      (while loop
		(if (stringp (car (car loop)))
		    (setcar (car loop) (intern (car (car loop)))))
		(setq att (car (car loop))
		      val (cdr (car loop))
		      loop (cdr loop))
		(case att
		 ((align textalign text-align display white-space)
		  (setq val (intern (downcase val))))
		 ((indent left-margin right-margin top-margin bottom-margin)
		  (setq val (string-to-int val)))
		 (otherwise
		  nil))
		(let* ((node-1 (assq tag sheet))
		       (node-2 (and node-1 (assoc class node-1)))
		       (node-3 (and node-2 (assq att node-2))))
		  (cond
		   ((not node-1)	; New top-level element
		    (setq sheet (cons (cons tag (list (cons class
							    (list
							     (cons att val)))))
				      sheet)))
		   ((and node-1 (not node-2)) ; New class for existing element
		    (setcdr node-1 (cons (cons class (list (cons att val)))
					 (cdr node-1))))
		   ((and node-2 (not node-3)) ; attribute/value on old class
		    (setcdr node-2 (cons (cons att val) (cdr node-2))))
		   (node-3		; Replace existing attribute value
		    (setcdr node-3 val)))))))))
	(skip-chars-forward " \t\r\n"))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (cons sheet defines)))


(defvar w3-style-font-size-mappings
  '(("xx-small" . 0)
    ("x-small"  . 1)
    ("small"    . 2)
    ("medium"   . 3)
    ("large"    . 4)
    ("x-large"  . 5)
    ("xx-large" . 6)
    )
  "A list of font size mappings")

(defvar w3-style-font-weight-mappings
  '(("-3" . :extra-light)
    ("-2" . :light)
    ("-1" . :demi-light)
    ("0"  . :medium)
    ("1"  . :normal)
    ("2"  . :demi-bold)
    ("3"  . :bold)
    ("4"  . :extrabold)
    ("bold"       . :bold)
    ("demi-light" . :demi-light)
    ("demi-bold"  . :demi-bold)
    ("extra-bold" . :extra-bold)
    ("extra-light". :extra-light)
    )
  "A list of font weight mappings.")

(defun w3-style-font-size-for-index (index)
  (if (stringp index)
      (setq index (or
		   (cdr-safe (assoc (downcase index)
				    w3-style-font-size-mappings))
		      3)))
  (setq index (- index 3))
  (let ((scaler (if (> index 0)
		    1.44
		  0.695))
	(size 12))
    (setq index (abs index))
    (while (/= index 0)
      (setq size (* size scaler)
	    index (1- index)))
    ;; This rounds to the nearest '10'
    (format "%dpt" (* 10 (round (/ size 10))))))

(defsubst w3-style-speech-normalize-number (num)
  (if num (% (abs (read num)) 9)))

(defun w3-generate-stylesheet-voices (sheet)
  (let ((todo sheet)
	cur cur-classes
	node family gain
	left right pitch
	pitch-range stress
	richness voice
	)
    (while todo
      (setq cur (car todo)
	    cur-classes (cdr cur)
	    todo (cdr todo))
      (while cur-classes
	(setq node (cdr (car cur-classes))
	      cur (car cur-classes)
	      cur-classes (cdr cur-classes)
	      family (cdr-safe (assq 'voice-family node))
	      family (if family (intern (downcase family)))
	      gain (w3-style-speech-normalize-number
		    (cdr-safe (assq 'gain node)))
	      left (w3-style-speech-normalize-number
		    (cdr-safe (assq 'left-volume node)))
	      right (w3-style-speech-normalize-number
		     (cdr-safe (assq 'right-volume node)))
	      pitch (w3-style-speech-normalize-number
		     (cdr-safe (assq 'pitch node)))
	      pitch-range (w3-style-speech-normalize-number
			   (cdr-safe (assq 'pitch-range node)))
	      stress (w3-style-speech-normalize-number
		      (cdr-safe (assq 'stress node)))
	      richness (w3-style-speech-normalize-number
			(cdr-safe (assq 'richness node))))
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
	(if voice (setcdr cur (cons (cons 'voice-spec voice) (cdr cur))))
	)
      )
    )
  )

(defun w3-style-post-process-stylesheet (sheet)
  (w3-generate-stylesheet-faces sheet)
  (if (featurep 'emacspeak)
      (w3-generate-stylesheet-voices w3-user-stylesheet)))

(defun w3-style-css-split-font-shorthand (font)
  ;; [<font-weight> || <font-style>]? <font-size> [ / <line-height> ]? <font-family>
  (let (weight size height family)
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
      (setq family (url-strip-leading-spaces font)))
    (list weight size height family)))

(defun w3-generate-stylesheet-faces (sheet)
  (let ((todo sheet)
	(cur nil)
	(cur-classes nil)
	(node nil)
	(fore nil)
	(back nil)
	(pixmap nil)
	(font nil)
	(family nil)
	(decoration nil)
	(style nil)
	(size nil)
	(index nil)
	(shorthand nil)
	(weight nil)
	(break-style nil))
    (while todo
      (setq cur (car todo)
	    cur-classes (cdr cur)
	    todo (cdr todo))
      (while cur-classes
	(setq node (cdr (car cur-classes))
	      cur (car cur-classes)
	      cur-classes (cdr cur-classes)
	      fore (cdr-safe (assq 'color node))
	      back (cdr-safe (assq 'background node))
	      decoration (cdr-safe (assq 'text-decoration node))
	      pixmap (cdr-safe (assq 'backdrop node))
	      index (cdr-safe (assq 'font-size-index node))
	      size (or (and index (w3-style-font-size-for-index index))
		       (cdr-safe (assq 'font-size node)))
	      family (cdr-safe (assq 'font-family node))
	      weight (cdr-safe (assq 'font-weight node))
	      weight (or (cdr-safe (assoc weight
					 w3-style-font-weight-mappings))
			 weight)
	      style (cdr-safe (assq 'font-style node))
	      shorthand (cdr-safe (assq 'font node)))

	;; Make sure all 'break' items get intern'd
	(if (or style decoration)
	    (setq style (concat style decoration)))
	(setq break-style (assq 'break node))
	(if (and (cdr break-style) (stringp (cdr break-style)))
	    (setcdr break-style (intern (cdr break-style))))
	(if shorthand
	    (progn
	      (setq shorthand (w3-style-css-split-font-shorthand shorthand))
	      (setq weight (or (nth 0 shorthand) weight)
		    size (or (nth 1 shorthand) size)
		    family (or (nth 3 shorthand) family)
		    weight (or (cdr-safe
				(assoc weight
				       w3-style-font-weight-mappings))
			       weight))))
	(if style
	    (setq style (mapcar
			 (function
			  (lambda (x)
			    (while (string-match "-" x)
			      (setq x (concat
				       (substring x 0 (match-beginning 0))
				       (substring x (match-end 0) nil))))
			    (intern-soft
			     (concat "font-set-" (downcase x) "-p"))))
			 (delete "" (split-string style "[ \t&,]")))))
	(if family (setq family (delete "" (split-string family ","))))
	(if (or family weight style size)
	    (progn
	      (setq font (make-font :family family :weight weight :size size))
	      (while style
		(and (fboundp (car style))
		     (funcall (car style) font t))
		(setq style (cdr style))))
	  (setq font nil))
	(if font (setcdr cur (cons (cons 'font-spec font) (cdr cur))))
	(if fore (setcdr cur (cons (cons 'foreground fore) (cdr cur))))
	(if back (setcdr cur (cons (cons 'background back) (cdr cur))))
	)
      )
    )
  )

(defun w3-handle-style (&optional args)
  (let ((fname (or (cdr-safe (assq 'href args))
		   (cdr-safe (assq 'src args))
		   (cdr-safe (assq 'uri args))))
	(type (downcase (or (cdr-safe (assq 'notation args))
			    "experimental")))
	(url-working-buffer " *style*")
	(base (cdr-safe (assq 'base args)))
	(stylesheet nil)
	(defines nil)
	(cur-sheet w3-current-stylesheet)
	(string (cdr-safe (assq 'data args))))
    (if fname (setq fname (url-expand-file-name fname
						(cdr-safe
						 (assoc base w3-base-alist)))))
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (erase-buffer)
      (setq url-be-asynchronous nil)
      (cond
       ((member type '("experimental" "arena" "w3c-style" "css"))
	(let ((data (w3-style-parse-css fname string cur-sheet)))
	  (setq stylesheet (nth 0 data)
		defines (nth 1 data))))
       (t
	(w3-warn 'html "Unknown stylesheet notation: %s" type))))
    (setq w3-current-stylesheet stylesheet)
    (w3-style-post-process-stylesheet w3-current-stylesheet)))

(defun w3-display-stylesheet (&optional sheet)
  (interactive)
  (if (not sheet) (setq sheet w3-current-stylesheet))
  (with-output-to-temp-buffer "W3 Stylesheet"
    (set-buffer standard-output)
    (emacs-lisp-mode)
    (require 'pp)
    (pp sheet (current-buffer))))

(provide 'w3-style)
