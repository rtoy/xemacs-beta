;;;
;;; mu-replace.el --- a replacing utility for GNU Emacs
;;;
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id: mu-replace.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
;;; Keywords: replace
;;;
;;; This file is part of tl (Tiny Library).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Commentary:
;;;
;;; - How to install.
;;;   1. bytecompile this file and copy it to the apropriate directory.
;;;   2. put the following lines to your ~/.emacs:
;;;		(autoload 'edit-replace-region "mu-replace" nil t)
;;; - How to use.
;;;   1. mark in beginning of region you want to replace.
;;;   2. go to end of region you want to replace.
;;;   3. type M-x edit-replace-region [CR]
;;;      then entering to ``edit-replace mode''.
;;;   4. edit replacement string.
;;;   5. type C-c C-c then specified region will be replaced.
;;;
;;; Code:

(defvar edit-replace-mode-map nil)
(if (null edit-replace-mode-map)
    (progn
      (setq edit-replace-mode-map (copy-keymap text-mode-map))
      (define-key edit-replace-mode-map
	"\C-c\C-c" (function edit-replace-query-replace))
      ))

(make-variable-buffer-local 'edit-replace-original-buffer)
(make-variable-buffer-local 'edit-replace-start-point)
(make-variable-buffer-local 'edit-replace-end-point)

(defvar edit-replace-original-buffer nil)
(defvar edit-replace-start-point nil)
(defvar edit-replace-end-point nil)

(defun edit-replace-region (beg end &optional str)
  (interactive "r")
  (let ((the-buf (current-buffer))
	(buf (get-buffer-create " *edit-replace*")))
    (pop-to-buffer buf)
    (setq major-mode 'edit-replace)
    (setq mode-name "edit for replace")
    (use-local-map edit-replace-mode-map)
    (setq edit-replace-original-buffer the-buf)
    (setq edit-replace-start-point beg)
    (setq edit-replace-end-point end)
    ))

(defun edit-replace-query-replace ()
  (interactive)
  (let ((beg edit-replace-start-point)
	(end edit-replace-end-point)
	str
	(rstr (buffer-string))
	)
    (switch-to-buffer edit-replace-original-buffer)
    (setq str (buffer-substring beg end))
    (goto-char beg)
    (query-replace str rstr)
    ))

;;; mu-replace.el ends here
