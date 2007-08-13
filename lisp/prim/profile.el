;;; profile.el --- basic profiling commands for XEmacs

;; Copyright (C) 1996 Ben Wing, (C) 1997 Free Software Foundation.

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

;; In addition to Lisp-based `elp', XEmacs contains a set of
;; primitives able to profile evaluation of Lisp functions, created by
;; the illustrious Ben Wing.  The functions in this file can be used
;; to gain easy access to the internal profiling functions.

;; The profiler works by catching "ticks" (actually SIGPROF signals),
;; and looking at the current Lisp function, at the time of each tick.
;; The output of this process is an alist with keys being the
;; functions, and values being the number of ticks per function.  From
;; this, `pretty-print-profiling-info' easily extracts the total
;; number of ticks, and the percentage CPU time of each function.

;; Unless stated otherwise, profiling info is being accumulated (the
;; current info is returned by `get-profiling-info').  Use
;; `clear-profiling-info' to break the accumulation chain.

;; Caveats (ELP users should read this):
;; 1) The time reported is function time, rather than
;;    function+descendants time;
;; 2) The Time/ms is CPU time (user+kernel), not the real time;
;; 3) Only the actuall funcalls are profiled.  If a subr Ffoo calls
;;    Fbar using Fbar (), only Ffoo will appear in the profile.

;; A typical profiling session consists of using `clear-profiling-info'
;; followed by `profile' or `profile-key-sequence', followed by
;; `pretty-print-profiling-info'.

;; For instance, to see where Gnus spends time when generating Summary
;; buffer, go to the group buffer, and press `M-x clear-profiling-info'
;; followed by `M-x profile-key-sequence RET SPC'.


;;; Code:

;;;###autoload
(defun pretty-print-profiling-info (&optional info stream)
  "Print profiling info INFO to STREAM in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
 `get-profiling-info'.
If STREAM is omitted, either a *Profiling Results* buffer or standard
 output are used, depending on whether the function was called
 interactively or not."
  (interactive)
  (setq info (if info
		 (copy-alist info)
	       (get-profiling-info)))
  (when (and (not stream)
	     (interactive-p))
    (pop-to-buffer (get-buffer-create "*Profiling Results*"))
    (erase-buffer))
  (let* ((standard-output (or stream (if (interactive-p)
					 (current-buffer)
				       standard-output)))
	 (maxfunlen (max (length "Function Name")
			 (apply 'max (mapcar (lambda (sym)
					       (length (symbol-name
							(car sym))))
					     info))))
	 (formatstr (format "%%-%ds" maxfunlen)))
    (setq info (nreverse (sort info #'cdr-less-than-cdr)))
    (princ (format (concat formatstr "    Ticks    %%/Total\n")
		   "Function Name"))
    (princ (make-string maxfunlen ?=))
    (princ "    =====    =======\n")
    (let ((sum 0.0))
      (dolist (info2 info)
	(incf sum (cdr info2)))
      (while info
	(let ((f (caar info)))
	  (princ (format (concat formatstr "    %-5d    %-6.3f\n")
			 f (cdar info) (* 100 (/ (cdar info) sum)))))
	(pop info))
      (princ (make-string maxfunlen ?-))
      (princ "--------------------\n")
      (princ (format (concat formatstr "    %-5d    %-6.2f\n")
		     "Total" sum 100.0))
      (princ (format "\n\nOne tick = %g ms\n"
		     (/ default-profiling-interval 1000.0)))))
  (when (and (not stream)
	     (interactive-p))
    (goto-char (point-min))))

;; Is it really necessary for this to be a macro?
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
