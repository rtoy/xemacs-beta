;;; w3-style.el --- Emacs-W3 binding style sheet mechanism
;; Author: wmperry
;; Created: 1996/12/13 18:01:46
;; Version: 1.23
;; Keywords: faces, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
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
;;; A style sheet mechanism for emacs-w3
;;;
;;; This will eventually be able to under DSSSL[-lite] as well as the
;;; experimental W3C mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font)
(require 'w3-keyword)
(require 'cl)
(require 'css)



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
       ((member type '("experimental" "arena" "w3c-style" "css" "text/css"))
	(setq stylesheet (css-parse fname string cur-sheet)))
       (t
	(w3-warn 'html "Unknown stylesheet notation: %s" type))))
    (setq w3-current-stylesheet stylesheet)
    )
  )

(defun w3-display-stylesheet (&optional sheet)
  (interactive)
  (if (not sheet) (setq sheet w3-current-stylesheet))
  (css-display sheet))

(provide 'w3-style)
