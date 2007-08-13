;;; with-timeout.el --- timeout hackery

;; Copyright (C) 1992 Free Software Foundation, Inc.
;; Keywords: extensions

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

;;; Synched up with: Not in FSF.

(defun with-timeout-timer (tag)
  ;; I'm pretty sure the condition-case isn't really necessary here,
  ;; but it doesn't hurt.
  (condition-case () (throw tag nil) (no-catch nil)))

;;;###autoload
(defun with-timeout-internal (with-timeout-seconds with-timeout-tag
                               with-timeout-body with-timeout-forms)
  (let ((with-timeout-timeout nil))
    (unwind-protect
         (progn
           (setq with-timeout-timeout (add-timeout with-timeout-seconds
                                                   'with-timeout-timer
                                                   with-timeout-tag))
           (let ((value (catch with-timeout-tag
                          (prog1 (funcall with-timeout-body)
                            (setq with-timeout-tag nil)))))
             (if with-timeout-tag
                 (funcall with-timeout-forms)
		 value)))
      (if with-timeout-timeout
          (disable-timeout with-timeout-timeout)))))

;;;###autoload
(defmacro with-timeout (seconds-and-timeout-forms &rest body)
  "Usage: (with-timeout (seconds &rest timeout-forms) &rest body)
This is just like progn, but if the given number of seconds expires before
the body returns, then timeout-forms are evaluated and returned instead.
The body won't be interrupted in the middle of a computation: the check for 
the timer expiration only occurs when body does a redisplay, or prompts the
user for input, or calls accept-process-output."
  (let ((seconds (car seconds-and-timeout-forms))
	(timeout-forms (cdr seconds-and-timeout-forms)))
    (` (with-timeout-internal (, seconds) '(, (make-symbol "_with_timeout_"))
                              #'(lambda () (progn (,@ body)))
                              #'(lambda () (progn (,@ timeout-forms)))))))

(put 'with-timeout 'lisp-indent-function 1)

;;;###autoload
(defun yes-or-no-p-with-timeout (timeout prompt &optional default-value)
  "Just like yes-or-no-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE."
  (with-timeout (timeout
		 (message (concat prompt "(yes or no) Timeout to "
				  (if default-value "Yes" "No")))
		 default-value)
    (yes-or-no-p prompt)))

;;;###autoload
(defun y-or-n-p-with-timeout (timeout prompt &optional default-value)
  "Just like y-or-n-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE."
  (with-timeout (timeout
		 (message (concat prompt "(yes or no) Timeout to "
				  (if default-value "Yes" "No")))
		 default-value)
    (y-or-n-p prompt)))
