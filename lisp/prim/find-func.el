;;; find-func.el --- find the definition of the Emacs Lisp function near point

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Jens Petersen <petersen@kurims.kyoto-u.ac.jp>
;; Maintainer: petersen@kurims.kyoto-u.ac.jp
;; Keywords: emacs-lisp, functions
;; Created: 97/07/25
;; URL: <http://www.kurims.kyoto-u.ac.jp/~petersen/emacs-lisp/>

;; $Id: find-func.el,v 1.2 1997/10/31 14:53:07 steve Exp $

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; The funniest thing about this is that I can't imagine why a package
;; so obviously useful as this hasn't been written before!!
;;
;; Put this file in your `load-path', byte-compile it and add the
;; following code in your init file:
;;
;; ;;; find-func
;; (load "find-func")
;; (global-set-key [(control ?c) ?f] 'find-function)
;; (global-set-key [(control ?c) ?4 ?f] 'find-function-other-window)
;; (global-set-key [(control ?c) ?5 ?f] 'find-function-other-frame)
;; (global-set-key [(control ?c) ?k] 'find-function-on-key)
;;
;; and away you go!  It does pretty much what you would expect,
;; putting the cursor at the definition of the function at point.
;;
;; In XEmacs the source file of dumped functions is recorded (and can
;; be accessed with the function `compiled-function-annotation', which
;; doesn't exist in Emacs), so in XEmacs non-primitive dumped
;; functions can also be found.  Unfortunately this is not possible in
;; Emacs.  It would be nice if the location of primitive functions in
;; the C code was also recorded!

;; The code is adapted from `describe-function', `describe-key'
;; ("help.el") and `fff-find-loaded-emacs-lisp-function' (Noah Friedman's
;; "fff.el").

;;; To do:
;;
;; o improve handling of advice'd functions? (at the moment it goes to
;; the advice, not the actual definition)
;;
;; o `find-function-other-frame' is not quite right when the function
;; is in the current buffer.
;;
;;;; Code:

(defgroup find-function nil
  "Find the definition of the Emacs Lisp function near point."
  :group 'lisp)

;;; User variables:

(defcustom find-function-source-path nil
  "The default list of directories where find-function searches.

If this variable is `nil' then find-function searches `load-path' by
default."
  :type '(choice (const :tag "Use `load-path'" nil)
		 (repeat :tag "Directories"
			 :menu-tag "List"
			 :value ("")
			 directory))
  :group 'find-function)


;;; Functions:

(defun find-function-noselect (function)
  "Returns a pair `(buffer . point)' pointing to the definition of FUNCTION.

Finds the Emacs Lisp library containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'."
  (if (not function)
      (error "You didn't specify a function"))
  (and (subrp (symbol-function function))
       (error "%s is a primitive function" function))
  (let ((def (symbol-function function))
	library aliases)
    (while (symbolp def)
      (or (eq def function)
	  (if aliases
	      (setq aliases (concat aliases
				    (format ", which is an alias for %s"
					    (symbol-name def))))
	    (setq aliases (format "an alias for %s" (symbol-name def)))))
      (setq function (symbol-function function)
	    def (symbol-function function)))
    (if aliases
	(message aliases))
    (setq library
	  (cond ((eq (car-safe def) 'autoload)
		 (nth 1 def))
		((describe-function-find-file function))
		((compiled-function-p def)
		 (substring (compiled-function-annotation def) 0 -4))
		((eq 'macro (car-safe def))
		 (and (compiled-function-p (cdr def))
		      (substring (compiled-function-annotation (cdr def)) 0 -4)))))
    (if (null library)
	(error (format "Don't know where `%s' is defined" function)))
    (if (string-match "\\.el\\(c\\)\\'" library)
	(setq library (substring library 0 (match-beginning 1))))
    (let* ((path find-function-source-path)
	   (filename (if (file-exists-p library)
			 library
		       (if (string-match "\\(\\.el\\)\\'" library)
			   (setq library (substring library 0
						    (match-beginning
						     1))))
		       (or (locate-library (concat library ".el") t path)
			   (locate-library library t path)))))
      (if (not filename)
	  (error "The library \"%s\" is not in the path." library))
      (with-current-buffer (find-file-noselect filename)
	(save-match-data
	  (let (;; avoid defconst, defgroup, defvar (any others?)
		(regexp
		 (format "^\\s-*(def[^cgv\W]\\w+\\*?\\s-+%s\\s-" function))
		(syntable (syntax-table)))
	    (set-syntax-table emacs-lisp-mode-syntax-table)
	    (goto-char (point-min))
	    (if (prog1
		    (re-search-forward regexp nil t)
		  (set-syntax-table syntable))
		(progn
		  (beginning-of-line)
		  (cons (current-buffer) (point)))
	      (error "Cannot find definition of `%s'" function))))))))

(defun find-function-read-function ()
  "Read and return a function, defaulting to the one near point.

`function-at-point' is used to select the default function."
  (let ((fn (function-at-point))
	(enable-recursive-minibuffers t)
	val)
    (setq val (completing-read
	       (if fn
		   (format "Find function (default %s): " fn)
		 "Find function: ")
	       obarray 'fboundp t nil 'function-history))
    (list (if (equal val "")
	      fn (intern val)))))

(defun find-function-do-it (function switch-fn)
  "Find Emacs Lisp FUNCTION in a buffer and display it with SWITCH-FN.
Point is saved in the buffer if it is one of the current buffers."
  (let ((orig-point (point))
	(orig-buffers (buffer-list))
	(buffer-point (find-function-noselect function)))
    (when buffer-point
      (funcall switch-fn (car buffer-point))
      (when (memq (car buffer-point) orig-buffers)
	(push-mark orig-point))
      (goto-char (cdr buffer-point))
      (recenter 0))))

;;;###autoload
(defun find-function (function)
  "Find the definition of the function near point in the current window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'."
  (interactive (find-function-read-function))
  (find-function-do-it function 'switch-to-buffer))

;;;###autoload
(defun find-function-other-window (function)
  "Find the definition of the function near point in the other window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'."
  (interactive (find-function-read-function))
  (find-function-do-it function 'switch-to-buffer-other-window))

;;;###autoload
(defun find-function-other-frame (function)
  "Find the definition of the function near point in the another frame.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'."
  (interactive (find-function-read-function))
  (find-function-do-it function 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-function-on-key (key)
  "Find the function that KEY invokes.  KEY is a string.
Point is saved if FUNCTION is in the current buffer."
  (interactive "kFind function on key: ")
  (let ((defn (key-or-menu-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (if (and (consp defn) (not (eq 'lambda (car-safe defn))))
	  (message "runs %s" (prin1-to-string defn))
	(find-function-other-window defn)))))

;;;###autoload
(defun find-function-at-point ()
  "Find directly the function at point in the other window."
  (interactive)
  (let ((symb (function-at-point)))
    (when symb
      (find-function-other-window symb))))

;; (define-key ctl-x-map "F" 'find-function) ; conflicts with `facemenu-keymap'

;;;###autoload
(define-key ctl-x-4-map "F" 'find-function-other-window)
;;;###autoload
(define-key ctl-x-5-map "F" 'find-function-other-frame)
;;;###autoload
(define-key ctl-x-map "K" 'find-function-on-key)

(provide 'find-func)
;;; find-func.el ends here
