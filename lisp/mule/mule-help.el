;;; mule-help.el --- Mule-ized Help functions 

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@altair.xemacs.org>
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

;;; Synched up with: Emacs 20.1

;;; Commentary:

;; 

;;; Code:

;; TUTORIAL arg is XEmacs addition
(defun help-with-tutorial (&optional arg tutorial)
  "Select the Emacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With arg, you are asked to select which language."
  (interactive "P")
  (let (lang filename file)
    (if arg
	(or (setq lang (read-language-name 'tutorial "Language: "))
	    (error "No tutorial file of the specified language"))
      (setq lang current-language-environment))
    (setq filename (or (get-language-info lang 'tutorial)
		       (or tutorial "TUTORIAL")))
    (setq file (expand-file-name (concat "~/" filename)))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name filename data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
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
      (set-buffer-modified-p nil))))


(provide 'mule-help)

;;; mule-help.el ends here