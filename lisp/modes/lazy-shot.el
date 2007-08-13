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

;;; Synched up with:  Not in FSF (mostly equivalent to lazy-lock 2.09
;;; in FSF 20.2).

;;; Commentary:

;;; This is an experimental demand based font-lock implemenation.  It
;;; is almost equal in functionality and interface to lazy-lock 2.09
;;; Does somebody really need defer-locking?
;;;
;;; To use: put
;;;    (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)
;;; in .emacs (.xemacs/init.el).  Do not use in combination with
;;; lazy-lock.

;;; It is exprimental in the sense that it relies on C support from
;;; the redisplay engine, that is experimental.  The code in this file
;;; is more or less finished.  The C code support experimental because
;;; the current design is rumoured to be ugly.  Secondly because
;;; XEmacs does actually display the "un-font-locked" parts of the
;;; buffer first, the user notices flashing as the buffer is repainted 
;;; with color/fonts.

;;; Code:

(require 'font-lock)
(require 'itimer)

(defvar lazy-shot-mode nil)


(defgroup lazy-shot nil
  "Lazy-shot customizations"
  :group 'tools
  :group 'faces
  :prefix "lazy-shot-")

(defcustom lazy-shot-minimum-size 0
    "*Minimum size of a buffer for demand-driven fontification.
On-demand fontification occurs if the buffer size is greater than this value.
If nil, means demand-driven fontification is never performed."
    :type '(choice (const :tag "Off" nil)
		   (integer :tag "Size"))
    :group 'lazy-shot)


(defcustom lazy-shot-step-size 1024	; Please test diffent sizes
  "Minimum size of each fontification shot."
  :type 'integer
  :group 'lazy-shot)

(defcustom lazy-shot-stealth-time 30
  "*Time in seconds to delay before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means stealth fontification is never performed.

The value of this variable is used when Lazy Shot mode is turned on."
  :type '(choice (const :tag "Off" nil)
		 (number :tag "Time"))
  :group 'lazy-shot)

(defcustom lazy-shot-stealth-lines (if font-lock-maximum-decoration 100 250)
  "*Maximum size of a chunk of stealth fontification.
Each iteration of stealth fontification can fontify this number of lines.
To speed up input response during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable."
  :type 'integer
  :group 'lazy-shot)

(defcustom lazy-shot-stealth-nice
   (/ (float 1) (float 8))
  "*Time in seconds to pause between chunks of stealth fontification.
Each iteration of stealth fontification is separated by this amount of time.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable."
  :type 'number
  :group 'lazy-shot)

(defcustom lazy-shot-verbose (not (null font-lock-verbose))
  "*If non-nil, means demand fontification should show status messages."
  :type 'boolean
  :group 'lazy-shot)

(defcustom lazy-shot-stealth-verbose (not (null lazy-shot-verbose))
  "*If non-nil, means stealth fontification should show status messages."
  :type 'boolean
  :group 'lazy-shot)



;;;###autoload
(defun lazy-shot-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive."
  (interactive "P")
  (let ((was-on lazy-shot-mode))
    (set (make-local-variable 'lazy-shot-mode)
	 (and (if arg (> (prefix-numeric-value arg) 0) (not lazy-shot-mode))))
    (cond ((and lazy-shot-mode (not font-lock-mode))
	   ;; Turned on `lazy-shot-mode' rather than `font-lock-mode'.
	   (let ((font-lock-support-mode 'lazy-shot-mode))
	     (font-lock-mode t)))
	  (lazy-shot-mode
	   ;; Turn ourselves on.
	   (lazy-shot-install))
	  (was-on
	   ;; Turn ourselves off.
	   (lazy-shot-unstall)))))

(custom-add-option 'font-lock-mode-hook 'turn-on-lazy-lock)

;;;###autoload
(defun turn-on-lazy-shot ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-shot-mode t))

  ;; Can we do something intelligent here?
  ;; I would want to set-extent-end-position start on extents that
  ;; only partially overlap!
(defun lazy-shot-clean-up-extents (start end)
  "Make sure there are no lazy-shot-extens betweeen START and END.
This improves efficiency and C-g behavior."
  ;; Be carefull this function is typically called with inhibit-quit!
  (map-extents (lambda (e b) (delete-extent e))
	       nil start end nil 'start-and-end-in-region 'initial-redisplay-function
	       'lazy-shot-redisplay-function))
	         
(defun lazy-shot-redisplay-function (extent)
   "Lazy lock the EXTENT when it has become visisble."
   (lazy-shot-lock-extent extent nil))


(defun lazy-shot-lock-extent (extent stealth)
  "Font-lock the EXTENT. Called from redisplay-trigger functions and
stealth locking functions"
   (when (extent-live-p extent)
     (let ((start (extent-start-position extent))
	   (end   (extent-end-position extent))
	   (buffer (extent-object extent)))
       (delete-extent extent)
       (lazy-shot-fontify-internal buffer start end
				      (or lazy-shot-verbose 
					  (and stealth
					       lazy-shot-stealth-verbose))
				      (if stealth "stealthy " "")))))

(defun lazy-shot-fontify-internal (buffer start end verbose message)
  (save-excursion
    ;; Should inhibit quit here
    (set-buffer buffer) ;; with-current-buffer is silly here
    ;; This magic should really go into font-lock-fonity-region
    (goto-char start)
    (setq start (point-at-bol))
    (goto-char end)
    (setq end (point-at-bol 2))
    (lazy-shot-clean-up-extents start end)
    ;; and a allow quit here
    (if verbose
	(display-message 'progress
	  (format "Lazy-shot fontifying %sfrom %s to %s in %s"
		     message start end buffer)))
    (save-match-data
      (font-lock-fontify-region start end))))

;; Note this is suboptimal but works for now. It is not called that often.
(defun lazy-shot-fontify-region (start end &optional buffer)
  (lazy-shot-fontify-internal (or buffer (current-buffer))
					start end lazy-shot-verbose
					"on request "))

(defun lazy-shot-stealth-lock (buffer)
  "Find an extent to lazy lock in buffer."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((extent t))
	  (while (and extent (sit-for lazy-shot-stealth-nice))
	    (setq extent
		  (or   ;; First after point
		   (map-extents (lambda (e n) e)  nil (point) nil nil nil
				'initial-redisplay-function
				'lazy-shot-redisplay-function)
		   ;; Then before it
		   (map-extents (lambda (e n) e) nil nil (point) nil nil
				'initial-redisplay-function
				'lazy-shot-redisplay-function)))
	    (if extent
		(lazy-shot-lock-extent extent t)
	      (delete-itimer current-itimer)
	      (setq lazy-shot-stealth-timer nil)))))
    (delete-itimer current-itimer)))
    
(defun lazy-shot-install-extent (spos epos &optional buffer)
  "Make an extent that will lazy-shot if it is displayed."
     (let ((extent (make-extent spos epos buffer)))
       (when extent
         (set-extent-initial-redisplay-function extent
                       'lazy-shot-redisplay-function))
       extent))


(defun lazy-shot-install-extents (fontifying)
  ;;
  ;; Add hook if lazy-shot.el is deferring or is fontifying on scrolling.
  (when fontifying
    (let ((max (point-max))
	  start)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (setq start (point))
	  (goto-char (min max (+ start lazy-shot-step-size)))
	  (forward-line 1)
	  (lazy-shot-install-extent start (point)))))))

(defun lazy-shot-install-timer (fontifying)
  (when (and lazy-shot-stealth-time fontifying)
    (make-variable-buffer-local 'lazy-shot-stealth-timer)
    (setq lazy-shot-stealth-timer 
      (start-itimer (format "lazy shot for %s" (current-buffer))
		     'lazy-shot-stealth-lock lazy-shot-stealth-time
		     lazy-shot-stealth-time
		     t t (current-buffer)))))


(defun lazy-shot-install ()
  (make-local-variable 'font-lock-fontified)
  (setq font-lock-fontified (and lazy-shot-minimum-size
				 (>= (buffer-size) lazy-shot-minimum-size))) 
  (lazy-shot-install-extents font-lock-fontified)
  (lazy-shot-install-timer font-lock-fontified)
  (add-hook 'font-lock-after-fontify-buffer-hook
	    'lazy-shot-unstall-after-fontify))

;; Kludge needed untill lazy-lock-fontify-region is more intelligent
(defun lazy-shot-unstall-after-fontify ()
  (lazy-shot-unstall 1))

(defun lazy-shot-unstall (&optional no-fontify)
  ;; Stop the timer
  (when (and (boundp 'lazy-shot-stealth-timer) lazy-shot-stealth-timer)
    (delete-itimer lazy-shot-stealth-timer)
    (setq lazy-shot-stealth-timer nil))
  ;; Remove the extents.
  (map-extents 
     (lambda (e arg) (delete-extent e) nil) 
     nil nil nil nil nil 'initial-redisplay-function 'lazy-shot-redisplay-function)
  (when (and font-lock-mode (not no-fontify))
    (save-restriction
      (widen)
      (lazy-shot-fontify-region (point-min) (point-max)))))

(provide 'lazy-shot)

;;; lazy-shot.el ends here
