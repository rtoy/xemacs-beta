;;; profile.el --- basic profiling commands for XEmacs

;; Copyright (C) 1996 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: internal

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; `profile' macro and `profile-key-sequence' added in June 1997 by
;; hniksic.


;;; Code:

;;;###autoload
(defun pretty-print-profiling-info (&optional info stream)
  "Print profiling info INFO to STREAM in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
`get-profiling-info'.
If STREAM is omitted, either current buffer or standard output are used,
 depending on whether the function was called interactively or not."
  (interactive)
  (if info
      (setq info (copy-alist info))
    (setq info (get-profiling-info)))
  (let ((standard-output (or stream (if (interactive-p)
					(current-buffer)
				      standard-output))))
    (setq info (nreverse (sort info #'cdr-less-than-cdr)))
    (princ "Function                                               Count        %\n")
    (princ "---------------------------------------------------------------------\n")
    (let ((sum 0.0))
      (dolist (info2 info)
	(incf sum (cdr info2)))
      (while info
	(let ((f (caar info)))
	  (princ (format "%-50s%10d   %6.3f\n" f (cdar info)
			 (* 100 (/ (cdar info) sum)))))
	(pop info)))))

;;;###autoload
(defmacro profile (&rest forms)
  "Turn on profiling, execute FORMS and restore profiling state.
Profiling state here means that if profiling was not in effect when
PROFILE was called, it will be turned off after FORMS are evaluated.
Otherwise, profiling will be left running.

Returns the profiling info, printable by `pretty-print-profiling-info'."
  `(progn
     (if (profiling-active-p)
	 (progn
	   ,@forms)
       (unwind-protect
	   (progn
	     (start-profiling)
	     ,@forms)
	 (stop-profiling)))
     (get-profiling-info)))

(put 'profile 'lisp-indent-function 0)

;;;###autoload
(defun profile-key-sequence (keys)
  "Dispatch the key sequence KEYS and profile the execution.
KEYS can be a vector of keypress events, a keypress event, or a character.
The function returns the profiling info."
  (interactive "kProfile keystroke: ")
  (and (characterp keys)
       (setq keys (character-to-event keys)))
  (or (vectorp keys)
      (setq keys (vector keys)))
  (profile
    (mapc 'dispatch-event keys)))

;;; profile.el ends here
