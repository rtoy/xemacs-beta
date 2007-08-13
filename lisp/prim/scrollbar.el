;; Scrollbar support.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

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

;;; Synched up with: Not in FSF. (Completely divergent from FSF scroll-bar.el)

(defun init-scrollbar-from-resources (locale)
  (if (and (featurep 'x)
	   (or (eq locale 'global)
	       (eq 'x (device-or-frame-type locale)))
	   (x-init-scrollbar-from-resources locale))))

;;
;; vertical scrollbar functions
;;


;;
;; horizontal scrollbar functions
;;

(defun scrollbar-char-left (window)
  "Function called when the char-left arrow on the scrollbar is clicked.
This is the little arrow to the left of the scrollbar.  One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window (- (window-hscroll window) 1))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-char-right (window)
  "Function called when the char-right arrow on the scrollbar is clicked.
This is the little arrow to the right of the scrollbar.  One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window (+ (window-hscroll window) 1))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-page-left (window)
  "Function called when the user gives the \"page-left\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\) One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window (- (window-hscroll window)
				     (- (window-width window) 2)))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-page-right (window)
  "Function called when the user gives the \"page-right\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\) One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window (+ (window-hscroll window)
				     (- (window-width window) 2)))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-to-left (window)
  "Function called when the user gives the \"to-left\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\). One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window 0)
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-to-right (window)
  "Function called when the user gives the \"to-right\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\). One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (if (not (window-live-p window))
      nil
    (scrollbar-set-hscroll window 'max)
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-horizontal-drag (data)
  "Function called when the user drags the horizontal scrollbar thumb.
One argument is passed, a cons containing the scrollbar's window and a value
representing how many columns the thumb is slid over.  You can advise
this function to change the scrollbar behavior."
  (let ((window (car data))
	(value (cdr data)))
    (if (not (or (window-live-p window) (integerp value)))
	nil
      (scrollbar-set-hscroll window value)
      (setq zmacs-region-stays t)
      nil)))
