;;; column.el --- display line and column in the mode line

;; Copyright (C) 1993 Per Abrahamsen.
;; Copyright abandoned.  This file is donated to the public domain.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Hacked for XEmacs by Richard Lee <rlee@vienna.itd.sterling.com>
;; Version: 0.2
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;;; Synched up with: Not in FSF.

;;; Commentary:

;; LCD Archive Entry:
;; column|Per Abrahamsen|abraham@iesd.auc.dk|
;; Display line and column in the mode line|
;; 1993-12-31|0.2|~/misc/column.el.Z|

;; Requires FSF Emacs 19.  ***** or Lucid Emacs 19.6+  -- Richard Lee

;;; Change Log:
;;
;; Tue Jan 04 19:51:15 1994     Richard Lee *****
;;      * Hacks for Lemacs:
;;           changed display-column-after from ")%--" to ")%----"
;;           defined current-line (function and variable)
;;           changed display-column-format to use current-line instead of %l
;; Fri Dec 31 13:46:41 1993
;;      * Change mode-line-format directly instead of using a minor mode.
;; Thu Dec 16 14:57:15 1993
;;      * Removed (require 'lucid) as unnecessary.
;; Fri Aug 13 02:06:18 1993	Per Abrahamsen
;;      * Made current-column buffer local.
;; Tue Aug 10 10:00:00 1993	Per Abrahamsen
;;      * Created.

;; This version should display column and line number the same place as
;; line-number-mode.  Activate with 

;;	M-x display-column-mode RET

;; For FSF Emacs 19 only.  You can get line+.el or linenumber.el from the
;; emacs lisp archive if you have another version of Emacs.  Not tested.

;;; Code:

;; String containing current column as last evaluated.
(defvar current-column "0")
(defvar current-line   "0")
(make-variable-buffer-local 'current-column)
(make-variable-buffer-local 'current-line)

;; Returns the vertical position of point _relative to beginning of buffer_
;; (as opposed to the current-line example in the gnu-emacs 19 info page on
;; Text Lines, which does it relative to top of screen.) ***** -- Richard Lee
;; ben: lines begin at 1, not 0!
(defun current-line ()
  "Return the vertical position of point in the selected window.
   First line in the buffer is 1."
  (1+ (+ (count-lines 1 (point))
	 (if (= (current-column) 0) 1 0)
	 -1)))

;; Function updating the string containing the current column.
(defvar update-column-function 
  (function (lambda ()
	      (setq current-column (int-to-string (current-column)))
	      (setq current-line   (int-to-string (current-line)))
	      (set-buffer-modified-p (buffer-modified-p)))))

(defvar display-column-mode nil
  "Show current column and line in mode line if non-nil.")

(defvar display-column-format '(current-line "/" current-column "--")
  "Format for displaying the line and column in the mode line.")

;; Entry for column mode in mode line.
(defconst display-column-entry
  (list 'display-column-mode (cons "" display-column-format)))

(defvar display-column-after ")%]----"
  "Display column after this element in the mode line.")

;; Add display-column-format to mode-line-format after display-column-after.
(or (member display-column-entry mode-line-format)
    (let ((entry (member display-column-after mode-line-format)))
      (setcdr entry (cons display-column-entry (cdr entry)))))

(defun remove (it list)
  (cond ((null list) nil)
        ((eq it (car list)) (cdr list))
	(t (setcdr list (remove it (cdr list))) list)))  

;;;###autoload
(defun display-column-mode (&optional arg)
  "Toggle display column mode.
With prefix arg, turn display column mode on iff arg is positive.

When display column mode is on, the current column and line number are
displayed in the mode line."
  (interactive "P")
  (if (or (and (null arg) display-column-mode)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if display-column-mode
	  (progn
	    (remove-hook 'post-command-hook update-column-function)
	    (setq display-column-mode nil)
	    (set-buffer-modified-p (buffer-modified-p))))
    ;;Turn it on
    (if display-column-mode
	()
      (add-hook 'post-command-hook update-column-function)
      (setq display-column-mode t))))

(provide 'column)

;;; column.el ends here
