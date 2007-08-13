;;; dired-chmod.el - interactive editing of file permissions in Dired listings.

;;; Copyright (C) 1995 Russell Ritchie, <Russell.Ritchie@gssec.bt.co.uk>

;; Keywords: dired extensions, faces, interactive chmod

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

;;; To turn this on do: 
;;;  (require 'dired-chmod)
;;;  (add-hook 'dired-after-readin-hook 'dired-permissions-highlight)

(require 'dired)			;

(defvar dired-permissions-regexp "[-r][-w][-Ssx][-r][-w][-sx][-r][-w][-xst]"
  "Regexp matching the file permissions part of a dired line.")

(defvar dired-pre-permissions-regexp "^. [0-9 	]*[-d]"
  "Regexp matching the preamble to file permissions part of a dired line.
This shouldn't match socket or symbolic link lines (which aren't editable).")

(or (find-face 'dired-face-permissions)
    (and
     (make-face 'dired-face-permissions)
     (set-face-foreground 'dired-face-permissions '(color . "mediumorchid")
			  nil nil 'append)
     (set-face-underline-p 'dired-face-permissions '((mono . t)
						     (grayscale . t)) nil
						     nil 'append)))

(defun dired-activate-extent (extent keys fn)
  (let ((keymap (make-sparse-keymap)))
    (while keys
      (define-key keymap (car keys) fn)
      (setq keys (cdr keys)))
    (set-extent-face extent 'dired-face-permissions)
    (set-extent-property extent 'keymap keymap)
    (set-extent-property extent 'highlight t)
    (set-extent-property
     extent 'help-echo
     "Type rsStwx to set file permissions to taste interactively.")))

(defun dired-chmod-do-chmod (state)
  (let* ((file (dired-get-filename))
	 (operation (concat "chmod" " " state " " file))
	 (failure (apply (function dired-check-process)
			 operation "chmod" state (list file)))
	 (here (point)))
    (dired-do-redisplay)
    (goto-char (+ here 1))
    (dired-make-permissions-interactive)
    (if failure
	(dired-log-summary
	 (message "%s: error - type W to see why." operation)))))

(defun dired-u-r ()
  (interactive)
  (if (equal (event-key last-command-event) ?r)
      (dired-chmod-do-chmod "u+r")
    (dired-chmod-do-chmod "u-r")))

(defun dired-u-w ()
  (interactive)
  (if (equal (event-key last-command-event) ?w)
      (dired-chmod-do-chmod "u+w")
    (dired-chmod-do-chmod "u-w")))

(defun dired-u-x ()
  (interactive)
  (let ((key (event-key last-command-event)))
    (cond ((equal key ?s) (dired-chmod-do-chmod "u+s"))
	  ((equal key ?S) (dired-chmod-do-chmod "u+S"))
	  ((equal key ?x) (dired-chmod-do-chmod "u+x"))
	  (t (dired-chmod-do-chmod (cond ((looking-at "s") "u-s")
					 ((looking-at "S") "u-S")
					 ((looking-at "x") "u-x")
					 (t "u-x")))))))

(defun dired-g-r ()
  (interactive)
  (if (equal (event-key last-command-event) ?r)
      (dired-chmod-do-chmod "g+r")
    (dired-chmod-do-chmod "g-r")))

(defun dired-g-w ()
  (interactive)
  (if (equal (event-key last-command-event) ?w)
      (dired-chmod-do-chmod "g+w")
    (dired-chmod-do-chmod "g-w")))

(defun dired-g-x ()
  (interactive)
  (let ((key (event-key last-command-event)))
    (cond ((equal key ?s) (dired-chmod-do-chmod "g+s"))
	  ((equal key ?x) (dired-chmod-do-chmod "g+x"))
	  (t (dired-chmod-do-chmod (if (looking-at "s") "g-s" "g-x"))))))

(defun dired-o-r ()
  (interactive)
  (if (equal (event-key last-command-event) ?r)
      (dired-chmod-do-chmod "o+r")
    (dired-chmod-do-chmod "o-r")))

(defun dired-o-w ()
  (interactive)
  (if (equal (event-key last-command-event) ?w)
      (dired-chmod-do-chmod "o+w")
    (dired-chmod-do-chmod "o-w")))

(defun dired-o-x ()
  (interactive)
  (let ((key (event-key last-command-event)))
    (cond ((equal key ?s) (dired-chmod-do-chmod "o+s"))
	  ((equal key ?t) (dired-chmod-do-chmod "o+t"))
	  ((equal key ?x) (dired-chmod-do-chmod "o+x"))
	  (t (dired-chmod-do-chmod (cond ((looking-at "s") "o-s")
					 ((looking-at "t") "o-t")
					 ((looking-at "x") "o-x")
					 (t "o-x")))))))

;;;###autoload
(defun dired-make-permissions-interactive ()
  (save-excursion
    (beginning-of-line 0)
    (if (and (re-search-forward dired-pre-permissions-regexp (end-of-line) t)
	     (looking-at dired-permissions-regexp))
	(let* ((start (point))
	       (u-r-extent (make-extent start (+ start 1)))
	       (u-w-extent (make-extent (+ start 1) (+ start 2)))
	       (u-x-extent (make-extent (+ start 2) (+ start 3)))
	       (g-r-extent (make-extent (+ start 3) (+ start 4)))
	       (g-w-extent (make-extent (+ start 4) (+ start 5)))
	       (g-x-extent (make-extent (+ start 5) (+ start 6)))
	       (o-r-extent (make-extent (+ start 6) (+ start 7)))
	       (o-w-extent (make-extent (+ start 7) (+ start 8)))
	       (o-x-extent (make-extent (+ start 8) (+ start 9))))
	  (dired-activate-extent u-r-extent '(r space) 'dired-u-r)
	  (dired-activate-extent u-w-extent '(w space) 'dired-u-w)
	  (dired-activate-extent u-x-extent '(s S x space) 'dired-u-x)
	  (dired-activate-extent g-r-extent '(r space) 'dired-g-r)
	  (dired-activate-extent g-w-extent '(w space) 'dired-g-w)
	  (dired-activate-extent g-x-extent '(s x space) 'dired-g-x)
	  (dired-activate-extent o-r-extent '(r space) 'dired-o-r)
	  (dired-activate-extent o-w-extent '(w space) 'dired-o-w)
	  (dired-activate-extent o-x-extent '(s t x space) 'dired-o-x)))))

(defun dired-permissions-highlight ()
  (message "Highlighting permissions...")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (and (not (eolp))
	   (dired-make-permissions-interactive))
      (forward-line 1))
    (message "Highlighting permissions...done")))

(provide 'dired-chmod) 

;; dired-chmod.el ends here.
