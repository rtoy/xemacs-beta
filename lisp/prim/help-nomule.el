;;; help-nomule.el --- Help functions when not in Mule

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: help, internal

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(defconst tutorial-supported-languages
  '(("French" fr iso-8859-1)
    ("German" de iso-8859-1)
    ("Norwegian" no iso-8859-1)
    ("Croatian" hr iso-8859-2))
  "Alist of supported languages in TUTORIAL files.
Add languages here, as more are translated.")

;; TUTORIAL arg is XEmacs addition
(defun help-with-tutorial (&optional tutorial language)
  "Select the XEmacs learn-by-doing tutorial.
Optional arg TUTORIAL specifies the tutorial file; default is \"TUTORIAL\".
With a prefix argument, choose the language."
  (interactive "i\nP")
  (or tutorial
      (setq tutorial "TUTORIAL"))
  (when (and language (consp language))
    (let ((completion-ignore-case t))
      (setq language (assoc (completing-read "Language: "
					     tutorial-supported-languages
					     nil t)
			    tutorial-supported-languages))))
  (when language
    (setq tutorial (format "%s.%s" tutorial (cadr language))))
  (let ((file (expand-file-name tutorial "~")))
    (delete-other-windows)
    (let ((buffer (or (get-file-buffer file)
		      (create-file-buffer file)))
	  (window-configuration (current-window-configuration)))
      (condition-case error-data
	  (progn
	    (switch-to-buffer buffer)
	    (setq buffer-file-name file)
	    (setq default-directory (expand-file-name "~/"))
	    (setq buffer-auto-save-file-name nil)
	    ;; Because of non-Mule users, TUTORIALs are not coded
	    ;; independently, so we must guess the coding according to
	    ;; the language.
	    (let ((coding-system-for-read (nth 2 language)))
	      (insert-file-contents (expand-file-name tutorial
						      data-directory)))
	    (goto-char (point-min))
	    (search-forward "\n<<")
	    (delete-region (point-at-bol) (point-at-eol))
	    (let ((n (- (window-height (selected-window))
			(count-lines (point-min) (point))
			6)))
	      (if (< n 12)
		  (newline n)
		;; Some people get confused by the large gap.
		(newline (/ n 2))
		(insert "[Middle of page left blank for didactic purposes.  "
			"Text continues below]")
		(newline (- n (/ n 2)))))
	    (goto-char (point-min))
	    (set-buffer-modified-p nil))
	;; TUTORIAL was not found: kill the buffer and restore the
	;; window configuration.
	(file-error (kill-buffer buffer)
		    (set-window-configuration window-configuration)
		    ;; Now, signal the error
		    (signal (car error-data) (cdr error-data)))))))


(provide 'help-nomule)

;;; help-nomule.el ends here