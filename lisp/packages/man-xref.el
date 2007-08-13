;;; man-xref.el --- cross reference selection functions for man mode

;; Author:  Mark Hood <hood@eng.sun.com>
;; @(#)man-xref.el	1.15	

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

;;; Synched up with: FSF 19.35.

;;; Commentary:

;; This package is an add-on to the man.el that comes with Emacs
;; 19.34.  It renders manpage cross references in bold, sets them up
;; for mouse highlighting, and allows selection via keystrokes or
;; mouse.  All strings matching Man-reference-regexp in the text of
;; the man page are set up, in addition to the ones in the See Also
;; section.

;; To use this package, put something like the following in your Emacs
;; initialization file.  This example causes tab and M-tab to go to
;; the next and previous manual cross references, causes carriage
;; return to display a man page for the reference under point, and
;; allows mouse button 2 to invoke a man page display.

;; (add-hook 'Man-mode-hook
;;	  '(lambda ()
;;	     (Man-mouseify-xrefs)
;;	     (define-key Man-mode-map "\r" 'Man-do-manual-reference)
;;	     (define-key Man-mode-map "\t" 'Man-next-manual-reference)
;;	     (define-key Man-mode-map "\e\t" 'Man-prev-manual-reference)
;;	     (define-key Man-mode-map [mouse-2] 'Man-mouse-manual-reference)
;;	     ))
;;
;; (autoload 'Man-mouseify-xrefs "~/emacs/man-xref")

(eval-when-compile (require 'cl))

(defvar Man-word-syntax "w_()" "Syntax for words in a man buffer.")

(defun Man-current-word ()
  "Return word under point, using `Man-word-syntax' for word syntax."
  (save-excursion
    (let ((s (+ (point) (skip-syntax-backward Man-word-syntax))))
      (skip-syntax-forward Man-word-syntax)
      (buffer-substring s (point)))))

(defun Man-prev-word-hyphen-p ()
  "Return nil if previous word is not hyphenated.
Non-nil value is the buffer position of the beginning of the hyphenated word."
  (save-excursion
    (skip-syntax-backward Man-word-syntax)
    (skip-chars-backward " \t")
    (cond ((and (> (point) (1+ (point-min)))
		(string-equal "-\n" (buffer-substring (- (point) 2) (point))))
	   (backward-char)
	   (skip-syntax-backward Man-word-syntax)
	   (point)))))

(defun Man-next-manual-reference ()
  "Move point to the beginning of the next manual reference."
  (interactive)
  (let ((current (point))
	(end (re-search-forward (concat "[ \t]" Man-reference-regexp) nil t))
	(start (or (Man-prev-word-hyphen-p) (1+ (match-beginning 0)))))
    (cond ((eq end nil))
	  ((> start current)
	   (goto-char start))
	  ;; current is in the pre-hyphen portion of a hyphenated reference
	  ((re-search-forward Man-reference-regexp nil t)
	   (goto-char (or (Man-prev-word-hyphen-p) (match-beginning 0))))
	  ((goto-char current)))))

(defun Man-prev-manual-reference ()
  "Move point to the beginning of the previous manual reference."
  (interactive)
  (if (re-search-backward (concat "[ \t]" Man-reference-regexp) nil t)
      (goto-char (or (Man-prev-word-hyphen-p) (1+ (match-beginning 0))))))

(defun Man-mouseify-xrefs ()
  "Render man cross references in bold font and set up mouse highlighting.
Add these cross references to `Man-refpages-alist'."
  (let (start end xref hyphen alist)
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward Man-reference-regexp nil t)
      (setq start (match-beginning 0))
      (setq end (match-end 0))
      (setq xref (buffer-substring start end))
      (cond ((setq hyphen (Man-prev-word-hyphen-p))
	     (setq start hyphen)
	     (goto-char hyphen)
	     (setq xref (concat (substring (Man-current-word) 0 -1) xref))
	     (goto-char end)))
      (setq Man-refpages-alist (cons (list xref) Man-refpages-alist))
      (Man-boldify-mouse-region start end))
    (setq Man-refpages-alist
	  (sort Man-refpages-alist
		(function (lambda (a b) (string< (car a) (car b))))))
    ;; delete duplicate entries in the alist
    (setq alist Man-refpages-alist)
    (while alist
      (cond ((string= (car (car alist)) (car (car (cdr alist))))
	     (setcdr alist (cdr (cdr alist))))
	    ((setq alist (cdr alist)))))
    (goto-char (point-min))
    (forward-line 1)))

(defun Man-mouse-manual-reference (mouse)
  "Move point to mouse position and run `Man-getpage-in-background' there."
  (interactive "e")
  (select-window (car (car (cdr mouse))))
  (goto-char (car (cdr (car (cdr mouse)))))
  (Man-do-manual-reference))

(defun Man-do-manual-reference ()
  "Run `Man-getpage-in-background' on cross reference under point.
Word under point is checked for a match with `Man-reference-regexp'.  
If point is not over a word, try to use previous word for a match."
  (interactive)
  (save-excursion
    (let ((xref (Man-current-word)) (hyphen (Man-prev-word-hyphen-p)))
      (if (and (zerop (length xref))
	       (setq xref " ")
	       (skip-syntax-backward " ")
	       (not (eq (point) (point-min))))
	  (Man-do-manual-reference)
	(cond ((string-equal "-" (substring xref -1))
	       (skip-syntax-forward Man-word-syntax)
	       (skip-syntax-forward " ")
	       (setq xref (concat (substring xref 0 -1) (Man-current-word))))
	      (hyphen
	       (goto-char hyphen)
	       (setq xref (concat (substring (Man-current-word) 0 -1) xref))))
	(if (string-match Man-reference-regexp xref)
	    (Man-getpage-in-background
	     (Man-translate-references
	      (substring xref (match-beginning 0) (match-end 0))))
	  (message "No cross reference found under point."))))))

(eval-and-compile
  (when (string-match "XEmacs\\|Lucid" emacs-version)
    (fset 'make-overlay 'make-extent)
    (fset 'overlay-put 'set-extent-property)))

(defun Man-boldify-mouse-region (beg end)
  "Render region text in bold with mouse highlighting."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'bold)
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'hilit t)))

