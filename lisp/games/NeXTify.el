;;; NeXTify.el --- Character insertion variation

;; Copyright status unknown

;; Author: Unknown
;; Keywords: games

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

;;; Code:
(defun SeLF-insert-command (arg)
  "Insert the character you TyPE.
Whichever character you TyPE to run ThIS command is inserted."
  (interactive "p")
  (let ((p (point))
	(case-fold-search nil))
    (self-insert-command arg)
    (save-excursion
      (goto-char p)
      (skip-chars-backward " \t\r\n")
      (if (condition-case () (forward-char -4) (error t))
	  nil
	(if (looking-at "\\<[A-Za-z][a-z][a-z][a-z][^A-Za-z]")
	    (progn
	      (insert (upcase (following-char))) (delete-char 1)
	      (forward-char 1)
	      (insert (upcase (following-char))) (delete-char 1)
	      (insert (upcase (following-char))) (delete-char 1)))))))

(define-key text-mode-map " " 'SeLF-insert-command)
(define-key text-mode-map "," 'SeLF-insert-command)
(define-key text-mode-map "." 'SeLF-insert-command)
(define-key text-mode-map "!" 'SeLF-insert-command)
(define-key text-mode-map "-" 'SeLF-insert-command)
(define-key text-mode-map "_" 'SeLF-insert-command)
(define-key text-mode-map ";" 'SeLF-insert-command)
(define-key text-mode-map ":" 'SeLF-insert-command)

;;; NeXTify.el ends here
