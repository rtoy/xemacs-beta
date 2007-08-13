;;; lazy-shot.el --- Lazy font locking for XEmacs

;; Copyright (C) 1997 Jan Vroonhof

;; Author: Jan Vroonhof <vroonhof@math.ethz.ch>
;; Keywords: languages, faces

;; This file is part of XEmacs

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

;;; Synched up with:  Not synched.

;;; Commentary:

;; This versions has basic demand lock functionality. Somebody please
;; sync further with lazy-lock v2 from FSF, customize etc.
;;
;;
;; Idea for the stealth lock function:
;;
;;
;; On an Idle itimer
;;    Loop over all buffers with lazy-lock set
;;       mapcar-extent in the region  (point) point-max for
;;                      one-shot-function property
;;         If not found do the same for [point-min,point]
;;         font-lock the found region and delete the extent

;;; Code:

(require 'font-lock)

(defvar lazy-shot-mode nil)


(defgroup lazy-shot nil
  "Lazy-shot customizations"
  :group 'tools
  :prefix "lazy-shot-")

(defcustom lazy-shot-step-size 1024	; Please test diffent sizes
  "Minimum size of each fontification shot."
  :type 'integer
  :group 'lazy-shot)

;;;###autoload
(defun lazy-shot-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive."
  (interactive "P")
  (set (make-local-variable 'lazy-shot-mode)
       (and (if arg (> (prefix-numeric-value arg) 0) (not lazy-shot-mode))))
  (cond ((and lazy-shot-mode (not font-lock-mode))
	 ;; Turned on `lazy-shot-mode' rather than `font-lock-mode'.
	 (let ((font-lock-support-mode 'lazy-shot-mode))
	   (font-lock-mode t)))
	(lazy-shot-mode
	 ;; Turn ourselves on.
	 (lazy-shot-install))
	(t
	 ;; Turn ourselves off.
	 (lazy-shot-unstall))))

;;;###autoload
(defun turn-on-lazy-shot ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-shot-mode t))


(defun lazy-shot-shot-function (extent)
   "Lazy lock the extent when it has become visisble"
   (let ((start (extent-start-position extent))
         (end   (extent-end-position extent))
	 (buffer (extent-object extent)))
     (delete-extent extent)
     (with-current-buffer buffer
       (save-excursion 
	 ;; This magic should really go into font-lock-fonity-region
	 (goto-char start)
	 (unless (bolp)
	   (beginning-of-line)
	   (setq start (point)))
	 (goto-char end)
	 (unless (bolp)
	   (forward-line)
	   (setq end (point)))
	 (display-message 'progress
			  (format "Lazy-shot fontifying from %s to %s in %s"
				  start end buffer))
	 (save-match-data
	   (font-lock-fontify-region start end))))))

(defun lazy-shot-install-extent (spos epos &optional buffer)
  "Make an extent that will lazy-shot if it is displayed"
     (let ((extent (make-extent spos epos buffer)))
       (when extent
         (set-extent-one-shot-function extent
                       'lazy-shot-shot-function))
       extent))

(defun lazy-shot-next-line (pos &optional buffer)
  "Return the next end-of-line from POS in BUFFER."
  (save-excursion
    (goto-char pos buffer)
    (forward-line 1 buffer)
    (point buffer)))

(defun lazy-shot-install-extents (fontifying)
  ;;
  ;; Add hook if lazy-shot.el is deferring or is fontifying on scrolling.
  (when fontifying
    (let ((max (point-max)))
      (do* ((start (point-min) end)
	    (end (min max (lazy-shot-next-line (+ start lazy-shot-step-size)))
		 (min max (lazy-shot-next-line (+ start lazy-shot-step-size)))))
	  ((>= start max))
	(lazy-shot-install-extent start end)))))

(defun lazy-shot-install ()
  (make-local-variable 'font-lock-fontified)
  (setq font-lock-fontified t)
  (lazy-shot-install-extents font-lock-fontified))

(defun lazy-shot-unstall ()
  ;;
  ;; Remove the extents.
  (map-extents 
     (lambda (e arg) (delete-extent e) nil) 
     nil nil nil nil nil 'one-shot-function 'lazy-shot-shot-function)
  ;;
  ;; Remove the fontification hooks.
  (remove-hook 'after-change-functions 'lazy-shot-defer-after-change t)
  ;;
  ;; If Font Lock mode is still enabled, reinstall its hook.
  (when font-lock-mode
    (add-hook 'after-change-functions 'font-lock-after-change-function nil t)))


(provide 'lazy-shot)

;;; lazy-shot.el ends here
