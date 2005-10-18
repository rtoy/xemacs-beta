;;; next-error.el --- Next error support framework

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Synched up with: FSF 22.0.50.1 (CVS)

(defgroup next-error nil
  "`next-error' support framework."
  :group 'compilation
  :version "22.1")

(defface next-error
  '((t (:inherit region)))
  "Face used to highlight next error locus."
  :group 'next-error
  :version "22.1")

(defcustom next-error-highlight 0.1
  "*Highlighting of locations in selected source buffers.
If number, highlight the locus in `next-error' face for given time in seconds.
If t, use persistent overlays fontified in `next-error' face.
If nil, don't highlight the locus in the source buffer.
If `fringe-arrow', indicate the locus by the fringe arrow."
  :type '(choice (number :tag "Delay")
                 (const :tag "Persistent overlay" t)
                 (const :tag "No highlighting" nil)
                 (const :tag "Fringe arrow" 'fringe-arrow))
  :group 'next-error
  :version "22.1")

(defcustom next-error-highlight-no-select 0.1
  "*Highlighting of locations in non-selected source buffers.
If number, highlight the locus in `next-error' face for given time in seconds.
If t, use persistent overlays fontified in `next-error' face.
If nil, don't highlight the locus in the source buffer.
If `fringe-arrow', indicate the locus by the fringe arrow."
  :type '(choice (number :tag "Delay")
                 (const :tag "Persistent overlay" t)
                 (const :tag "No highlighting" nil)
                 (const :tag "Fringe arrow" 'fringe-arrow))
  :group 'next-error
  :version "22.1")

(defcustom next-error-hook nil
  "*List of hook functions run by `next-error' after visiting source file."
  :type 'hook
  :group 'next-error)

(defvar next-error-highlight-timer nil)

;(defvar next-error-overlay-arrow-position nil)
;(put 'next-error-overlay-arrow-position 'overlay-arrow-string "=>")
;(add-to-list 'overlay-arrow-variable-list 'next-error-overlay-arrow-position)

(defvar next-error-last-buffer nil
  "The most recent `next-error' buffer.
A buffer becomes most recent when its compilation, grep, or
similar mode is started, or when it is used with \\[next-error]
or \\[compile-goto-error].")

(defvar next-error-function nil
  "Function to use to find the next error in the current buffer.
The function is called with 2 parameters:
ARG is an integer specifying by how many errors to move.
RESET is a boolean which, if non-nil, says to go back to the beginning
of the errors before moving.
Major modes providing compile-like functionality should set this variable
to indicate to `next-error' that this is a candidate buffer and how
to navigate in it.")

(make-variable-buffer-local 'next-error-function)

(defsubst next-error-buffer-p (buffer
			       &optional avoid-current
			       extra-test-inclusive
			       extra-test-exclusive)
  "Test if BUFFER is a `next-error' capable buffer.

If AVOID-CURRENT is non-nil, treat the current buffer
as an absolute last resort only.

The function EXTRA-TEST-INCLUSIVE, if non-nil, is called in each buffer
that normally would not qualify.  If it returns t, the buffer
in question is treated as usable.

The function EXTRA-TEST-EXCLUSIVE, if non-nil is called in each buffer
that would normally be considered usable.  If it returns nil,
that buffer is rejected."
  (and (buffer-name buffer)		;First make sure it's live.
       (not (and avoid-current (eq buffer (current-buffer))))
       (with-current-buffer buffer
	 (if next-error-function   ; This is the normal test.
	     ;; Optionally reject some buffers.
	     (if extra-test-exclusive
		 (funcall extra-test-exclusive)
	       t)
	   ;; Optionally accept some other buffers.
	   (and extra-test-inclusive
		(funcall extra-test-inclusive))))))

(defun next-error-find-buffer (&optional avoid-current
					 extra-test-inclusive
					 extra-test-exclusive)
  "Return a `next-error' capable buffer.
If AVOID-CURRENT is non-nil, treat the current buffer
as an absolute last resort only.

The function EXTRA-TEST-INCLUSIVE, if non-nil, is called in each buffer
that normally would not qualify.  If it returns t, the buffer
in question is treated as usable.

The function EXTRA-TEST-EXCLUSIVE, if non-nil is called in each buffer
that would normally be considered usable.  If it returns nil,
that buffer is rejected."
  (or
   ;; 1. If one window on the selected frame displays such buffer, return it.
   (let ((window-buffers
          (delete-dups
           (delq nil (mapcar (lambda (w)
                               (if (next-error-buffer-p
				    (window-buffer w)
                                    avoid-current
                                    extra-test-inclusive extra-test-exclusive)
                                   (window-buffer w)))
                             (window-list))))))
     (if (eq (length window-buffers) 1)
         (car window-buffers)))
   ;; 2. If next-error-last-buffer is an acceptable buffer, use that.
   (if (and next-error-last-buffer
            (next-error-buffer-p next-error-last-buffer avoid-current
                                 extra-test-inclusive extra-test-exclusive))
       next-error-last-buffer)
   ;; 3. If the current buffer is acceptable, choose it.
   (if (next-error-buffer-p (current-buffer) avoid-current
			    extra-test-inclusive extra-test-exclusive)
       (current-buffer))
   ;; 4. Look for any acceptable buffer.
   (let ((buffers (buffer-list)))
     (while (and buffers
                 (not (next-error-buffer-p
		       (car buffers) avoid-current
		       extra-test-inclusive extra-test-exclusive)))
       (setq buffers (cdr buffers)))
     (car buffers))
   ;; 5. Use the current buffer as a last resort if it qualifies,
   ;; even despite AVOID-CURRENT.
   (and avoid-current
	(next-error-buffer-p (current-buffer) nil
			     extra-test-inclusive extra-test-exclusive)
	(progn
	  (message "This is the only next-error capable buffer")
	  (current-buffer)))
   ;; 6. Give up.
   (error "No next-error capable buffer found")))

;;;###autoload
(defun next-error (&optional arg reset)
  "Visit next `next-error' message and corresponding source code.

If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages.
Just \\[universal-argument] as a prefix means reparse the error message buffer
and start at the first error.

The RESET argument specifies that we should restart from the beginning.

\\[next-error] normally uses the most recently started
compilation, grep, or occur buffer.  It can also operate on any
buffer with output from the \\[compile], \\[grep] commands, or,
more generally, on any buffer in Compilation mode or with
Compilation Minor mode enabled, or any buffer in which
`next-error-function' is bound to an appropriate function.
To specify use of a particular buffer for error messages, type
\\[next-error] in that buffer when it is the only one displayed
in the current frame.

Once \\[next-error] has chosen the buffer for error messages, it
runs `next-error-hook' with `run-hooks', and stays with that buffer
until you use it in some other buffer which uses Compilation mode
or Compilation Minor mode.

See variables `compilation-parse-errors-function' and
\`compilation-error-regexp-alist' for customization ideas."
  (interactive "P")
  (if (consp arg) (setq reset t arg nil))
  (when (setq next-error-last-buffer (next-error-find-buffer))
    ;; we know here that next-error-function is a valid symbol we can funcall
    (with-current-buffer next-error-last-buffer
      (funcall next-error-function (prefix-numeric-value arg) reset)
      (run-hooks 'next-error-hook))))

(defalias 'goto-next-locus 'next-error)
(defalias 'next-match 'next-error)

(defun previous-error (&optional n)
  "Visit previous `next-error' message and corresponding source code.

Prefix arg N says how many error messages to move backwards (or
forwards, if negative).

This operates on the output from the \\[compile] and \\[grep] commands."
  (interactive "p")
  (next-error (- (or n 1))))

(defun first-error (&optional n)
  "Restart at the first error.
Visit corresponding source code.
With prefix arg N, visit the source code of the Nth error.
This operates on the output from the \\[compile] command, for instance."
  (interactive "p")
  (next-error n t))

(defun next-error-no-select (&optional n)
  "Move point to the next error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Finds and highlights the source line like \\[next-error], but does not
select the source buffer."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n))
  (pop-to-buffer next-error-last-buffer))

(defun previous-error-no-select (&optional n)
  "Move point to the previous error in the `next-error' buffer and highlight match.
Prefix arg N says how many error messages to move backwards (or
forwards, if negative).
Finds and highlights the source line like \\[previous-error], but does not
select the source buffer."
  (interactive "p")
  (next-error-no-select (- (or n 1))))

;;; Internal variable for `next-error-follow-mode-post-command-hook'.
(defvar next-error-follow-last-line nil)

(define-minor-mode next-error-follow-minor-mode
  "Minor mode for compilation, occur and diff modes.
When turned on, cursor motion in the compilation, grep, occur or diff
buffer causes automatic display of the corresponding source code
location."
  :group 'next-error :init-value nil :lighter " Fol"
  (if (not next-error-follow-minor-mode)
      (remove-hook 'post-command-hook 'next-error-follow-mode-post-command-hook t)
    (add-hook 'post-command-hook 'next-error-follow-mode-post-command-hook nil t)
    (make-local-variable 'next-error-follow-last-line)))

;;; Used as a `post-command-hook' by `next-error-follow-mode'
;;; for the *Compilation* *grep* and *Occur* buffers.
(defun next-error-follow-mode-post-command-hook ()
  (unless (equal next-error-follow-last-line (line-number-at-pos))
    (setq next-error-follow-last-line (line-number-at-pos))
    (condition-case nil
	(let ((compilation-context-lines nil))
	  (setq compilation-current-error (point))
	  (next-error-no-select 0))
      (error t))))

(provide 'next-error)
