;;; mule-sysdp.el --- consolidate MULE-version dependencies in one file.

;; Copyright (c) 1996, 1997 William Perry

;; Author: William Perry <wmperry@cs.indiana.edu>
;; Keywords: lisp, tools

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs with and without Mule support.

(require 'cl)

(defconst mule-sysdep-version (if (featurep 'mule)
				  (cond
				   ((string-match "XEmacs" emacs-version)
				    'xemacs)
				   ((and
				     (boundp 'mule-version)
				     (string-match "[0-9]+\\.[0-9]+"
						   mule-version))
				    (string-to-number (substring
						       mule-version
						       (match-beginning 0)
						       (match-end 0))))
				   (t 2.3))
				0)
  "What version of mule we are running under.")

(defconst mule-retrieval-coding-system
  (case mule-sysdep-version
    (2.3 *euc-japan*)
    (2.4 'coding-system-euc-japan)
    (3.0 'euc-japan)
    (xemacs 'euc-japan)
    (otherwise nil))
  "Default retrieval coding system for packages that use this package.")

(defconst mule-no-coding-system
  (case mule-sysdep-version
    (2.3 *noconv*)
    (2.4 'no-conversion)
    (3.0 'no-conversion)
    (xemacs 'no-conversion)
    (otherwise nil))
  "Coding system that means no coding system should be used.")

(defun mule-detect-coding-version (st nd)
  (case mule-sysdep-version
    (2.3 (code-detect-region (point-min) (point-max)))
    ((2.4 3.0 xemacs)
     (detect-coding-region (point-min) (point-max)))
    (otherwise nil)))

(defun mule-code-convert-region (st nd code)
  (if (and (listp code) (car code))
      (setq code (car code)))
  (case mule-sysdep-version
    (2.3
     (set 'mc-flag t)
     (code-convert-region (point-min) (point-max) code *internal*)
     (set-file-coding-system code))
    (2.4
     (setq enable-multibyte-characters t)
     (if (memq code '(autodetect coding-system-automatic))
	 nil
       (decode-coding-region st nd code)
       (set-buffer-file-coding-system code)))
    (3.0
     (setq enable-multibyte-characters t)
     (if (memq code '(autodetect automatic-conversion))
	 nil
       (or code (setq code 'automatic-conversion))
       (decode-coding-region st nd code)
       (set-buffer-file-coding-system code)))
    (xemacs
     (if (and (listp code) (not (car code)))
	 (setq code 'autodetect))
     (decode-coding-region (point-min) (point-max) code)
     (set-buffer-file-coding-system code))
    (otherwise
     nil)))

(defun mule-inhibit-code-conversion (proc)
  (if (process-buffer proc)
      (save-excursion
	(set-buffer (process-buffer proc))
	(set 'mc-flag nil)
	(set 'enable-multibyte-characters nil)))
  (case mule-sysdep-version
    ((3.0 2.4 2.3)
     (set-process-coding-system proc mule-no-coding-system
				mule-no-coding-system))
    (xemacs
     (set-process-input-coding-system proc mule-no-coding-system)
     (set-process-input-coding-system proc mule-no-coding-system))))

(defun mule-write-region-no-coding-system (st nd file)
  (let ((enable-multibyte-characters t)
	(coding-system-for-write 'no-conversion)
	(file-coding-system mule-no-coding-system)
	(buffer-file-coding-system mule-no-coding-system)
	(mc-flag t))
    (case mule-sysdep-version
      (2.3 (write-region st nd file nil nil nil *noconv*))
      (otherwise
       (write-region st nd file)))))

(defun mule-encode-string (str)
  (case mule-sysdep-version
    (2.3
     (code-convert-string str *internal* mule-retrieval-coding-system))
    ((2.4 3.0 xemacs)
     (encode-coding-string str mule-retrieval-coding-system))
    (otherwise
     str)))

(defun mule-decode-string (str)
  (and str
       (case mule-sysdep-version
	 ((2.4 3.0 xemacs)
	  (decode-coding-string str mule-retrieval-coding-system))
	 (2.3
	  (code-convert-string str *internal* mule-retrieval-coding-system))
	 (otherwise
	  str))))

(defun mule-truncate-string (str len &optional pad)
  "Truncate string STR so that string-width of STR is not greater than LEN.
 If width of the truncated string is less than LEN, and if a character PAD is
 defined, add padding end of it."
  (case mule-sysdep-version
    ((2.4 3.0)
     (let ((cl (string-to-vector str)) (n 0) (sw 0))
       (if (<= (string-width str) len) str
	 (while (<= (setq sw (+ (char-width (aref cl n)) sw)) len)
	   (setq n (1+ n)))
	 (string-match (make-string n ?.) str)
	 (setq str (substring str 0 (match-end 0))))
       (if pad (concat str (make-string (- len (string-width str)) pad)) str)))
    (2.3
     (let ((cl (string-to-char-list str)) (n 0) (sw 0))
       (if (<= (string-width str) len) str
	 (while (<= (setq sw (+ (char-width (nth n cl)) sw)) len)
	   (setq n (1+ n)))
	 (string-match (make-string n ?.) str)
	 (setq str (substring str 0 (match-end 0))))
       (if pad (concat str (make-string (- len (string-width str)) pad)) str)))
    (otherwise
     (concat (if (> (length str) len) (substring str 0 len) str)
	     (if (or (null pad) (> (length str) len))
		 ""
	       (make-string (- len (length str)) pad))))))

(defun mule-make-iso-character (char)
  (if (<= char 127)
      char
    (case mule-sysdep-version
      (2.3 (make-character lc-ltn1 char))
      (2.4 (make-char charset-latin-iso8859-1 char))
      (3.0 (make-char 'latin-iso8859-1 char))
      (xemacs char)
      (otherwise char))))

(case mule-sysdep-version
  ((2.3 2.4 3.0 xemacs) nil)
  (otherwise (fset 'string-width 'length)))

(and
 (boundp 'MULE)
 (not (featurep 'mule))
 (provide 'mule))

(provide 'mule-sysdp)
