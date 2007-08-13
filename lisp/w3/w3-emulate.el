;;; w3-emulate.el --- All variable definitions for emacs-w3
;; Author: wmperry
;; Created: 1996/06/30 18:05:22
;; Version: 1.2
;; Keywords: comm, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Provide emulations of various other web browsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, we emulate Netscape 2.x
;; ------------------------------
;; This entails mainly a few new keybindings.
;; Alt-S    == Save As
;; Alt-M    == New Mail Message
;; Alt-N    == New Window
;; Alt-L    == Open Location
;; Alt-O    == Open File
;; Alt-P    == Print
;; Alt-Q    == Quit
;; Alt-F    == Search
;; Alt-G    == Search Again
;; Alt-R    == Reload
;; Alt-I    == Load Images
;; Alt-A    == Add Bookmark
;; Alt-B    == Show Bookmark Window
;; Alt-H    == Show History Window
;; Alt-Left == Back
;; Alt-Right== Forward
;; Right    == Scroll left
;; Left     == Scroll right
;; Up       == Smooth scroll up
;; Down     == Smooth scroll down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key w3-netscape-emulation-minor-mode-map "\M-s" 'w3-save-as)
(define-key w3-netscape-emulation-minor-mode-map "\M-m" 'w3-mailto)
(define-key w3-netscape-emulation-minor-mode-map "\M-n" 'make-frame)
(define-key w3-netscape-emulation-minor-mode-map "\M-l" 'w3-fetch)
(define-key w3-netscape-emulation-minor-mode-map "\M-o" 'w3-open-local)
(define-key w3-netscape-emulation-minor-mode-map "\M-p" 'w3-print-this-url)
(define-key w3-netscape-emulation-minor-mode-map "\M-q" 'w3-quit)
(define-key w3-netscape-emulation-minor-mode-map "\M-f" 'w3-search-forward)
(define-key w3-netscape-emulation-minor-mode-map "\M-g" 'w3-search-again)
(define-key w3-netscape-emulation-minor-mode-map "\M-r" 'w3-reload-document)
(define-key w3-netscape-emulation-minor-mode-map "\M-i" 'w3-load-delayed-images)
(define-key w3-netscape-emulation-minor-mode-map "\M-a" 'w3-hotlist-add-document)
(define-key w3-netscape-emulation-minor-mode-map "\M-b" 'w3-show-hotlist)
(define-key w3-netscape-emulation-minor-mode-map "\M-h" 'w3-show-history-list)

(define-key w3-netscape-emulation-minor-mode-map [up]
  (function (lambda () (interactive) (scroll-down 1))))
(define-key w3-netscape-emulation-minor-mode-map [down]
  (function (lambda () (interactive) (scroll-up 1))))
(define-key w3-netscape-emulation-minor-mode-map [right] 'scroll-left)
(define-key w3-netscape-emulation-minor-mode-map [left] 'scroll-right)
(define-key w3-netscape-emulation-minor-mode-map [(meta left)]
  'w3-backward-in-history)
(define-key w3-netscape-emulation-minor-mode-map [(meta right)]
  'w3-forward-in-history)

(defun turn-on-netscape-emulation ()
  (interactive)
  (w3-lynx-emulation-minor-mode 0)
  (w3-netscape-emulation-minor-mode 1))

(defun w3-netscape-emulation-minor-mode (&optional arg)
  "Minor mode for emulating netscape key navigation."
  (interactive "P")
  (cond
   ((null arg)
    (setq w3-netscape-emulation-minor-mode
	  (not w3-netscape-emulation-minor-mode))
    (if w3-netscape-emulation-minor-mode
	(setq w3-lynx-emulation-minor-mode nil)))
   ((= 0 arg)
    (setq w3-netscape-emulation-minor-mode nil))
   (t
    (setq w3-lynx-emulation-minor-mode nil
	  w3-netscape-emulation-minor-mode t)))
  )

(defsubst w3-skip-word ()
  (skip-chars-forward "^ \t\n\r")
  (skip-chars-forward " \t"))

(defun w3-read-netscape-config (&optional fname)
  "Read in a netscape-style configuration file."
  (interactive "fNetscape configuration file: ")
  (if (not (and (file-exists-p fname)
		(file-readable-p fname)))
      (error "Could not read %s" fname))
  (let ((results nil)
	(tag nil)
	(val nil)
	(var nil)
	(save-pos nil))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-tmp*"))
      (erase-buffer)
      (insert-file-contents-literally fname)
      (goto-char (point-min))
      (skip-chars-forward "^ \t\r\n")	; Skip tag line
      (skip-chars-forward " \t\r\n")	; Skip blank line(s)
      (while (not (eobp))
	(setq save-pos (point))
	(skip-chars-forward "^:")
	(upcase-region save-pos (point))
	(setq tag (buffer-substring save-pos (point)))
	(skip-chars-forward ":\t ")
	(setq save-pos (point))
	(skip-chars-forward "^\r\n")
	(setq val (if (= save-pos (point))
		      nil
		    (buffer-substring save-pos (point))))
	(cond
	 ((null val) nil)
	 ((string-match "^[0-9]+$" val)
	  (setq val (string-to-int val)))
	 ((string= "false" (downcase val))
	  (setq val nil))
	 ((string= "true" (downcase val))
	  (setq val t))
	 (t nil))
	(skip-chars-forward " \t\n\r")
	(setq results (cons (cons tag val) results))))
    (while results
      (setq tag (car (car results))
	    val (cdr (car results))
	    var (cdr-safe (assoc tag w3-netscape-variable-mappings))
	    results (cdr results))
      (cond
       ((eq var 'w3-delay-image-loads) (set var (not val)))
       (var (set var val))
       (t nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now, lets try Lynx
;; ------------------
;; A few keybindings and modifications to some default functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-lynx-emulation ()
  (interactive)
  (w3-netscape-emulation-minor-mode 0)
  (w3-lynx-emulation-minor-mode 1))

(defun w3-lynx-emulation-minor-mode (&optional arg)
  "Minor mode for emulating lynx key navigation."
  (interactive "P")
  (cond
   ((null arg)
    (setq w3-lynx-emulation-minor-mode
	  (not w3-lynx-emulation-minor-mode))
    (if w3-lynx-emulation-minor-mode
	(setq w3-netscape-emulation-minor-mode nil)))
   ((= 0 arg)
    (setq w3-lynx-emulation-minor-mode nil))
   (t
    (setq w3-lynx-emulation-minor-mode t
	  w3-netscape-emulation-minor-mode nil))))

(define-key w3-lynx-emulation-minor-mode-map "+"    'w3-scroll-up)
(define-key w3-lynx-emulation-minor-mode-map "-"    'scroll-down)
(define-key w3-lynx-emulation-minor-mode-map "b"    'scroll-down)
(define-key w3-lynx-emulation-minor-mode-map "a"    'w3-hotlist-add-document)
(define-key w3-lynx-emulation-minor-mode-map "c"    'w3-mail-document-author)
(define-key w3-lynx-emulation-minor-mode-map "e"    'w3-edit-source)
(define-key w3-lynx-emulation-minor-mode-map "g"    'w3-fetch)
(define-key w3-lynx-emulation-minor-mode-map "i"    'ignore)
(define-key w3-lynx-emulation-minor-mode-map "m"    'w3)
(define-key w3-lynx-emulation-minor-mode-map "o"    'ignore)
(define-key w3-lynx-emulation-minor-mode-map "p"    'w3-print-this-url)
(define-key w3-lynx-emulation-minor-mode-map "q"    'w3-quit)
(define-key w3-lynx-emulation-minor-mode-map "/"    'w3-search-forward)
(define-key w3-lynx-emulation-minor-mode-map "s"    'w3-search-forward)
(define-key w3-lynx-emulation-minor-mode-map "n"    'w3-search-again)
(define-key w3-lynx-emulation-minor-mode-map "v"    'w3-show-hotlist)
(define-key w3-lynx-emulation-minor-mode-map "="    'w3-document-information)
(define-key w3-lynx-emulation-minor-mode-map "\C-r" 'w3-reload-document)
(define-key w3-lynx-emulation-minor-mode-map "\C-w" 'w3-refresh-buffer)
(define-key w3-lynx-emulation-minor-mode-map "\\"   'w3-source-document)
(define-key w3-lynx-emulation-minor-mode-map "!"    'shell)
(define-key w3-lynx-emulation-minor-mode-map [up]   'w3-back-link)
(define-key w3-lynx-emulation-minor-mode-map [down] 'w3-forward-link)
(define-key w3-lynx-emulation-minor-mode-map [right] 'w3-follow-link)
(define-key w3-lynx-emulation-minor-mode-map [left] 'w3-backward-in-history)

(provide 'w3-emulate)

;;; Local Variables:
;;; truncate-lines: t
;;; End:
