;;; hm--html-mode --- Major mode for editing HTML documents for the WWW
;;;
;;; Keywords: hypermedia languages help docs wp
;;;
;;; $Id: hm--html-mode.el,v 1.2 1997/02/15 22:21:04 steve Exp $
;;;
;;; Copyright (C) 1996, 1997 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Commentary:
;;; Description:
;;;
;;;	This file defines the hm--html-mode, a mode for editing html
;;;	files. It is the main file of the package hm--html-menus.
;;;	Previous releases had used the file html-mode.el from Marc
;;;	Andreessen. In that times the mode was called html-mode. I've
;;;	changed the name of the mode to distinquish it from other
;;;	html modes. But feel free to set a 
;;;		(defalias 'hm--html-mode 'html-mode)
;;;	to get back the old name of the mode.
;;;
;;;	In the earlier releases of the package the main file was
;;;	hm--html-menu.el. This has been changed to hm--html-mode.el.
;;; 
;;; Installation: 
;;;   
;;;	Put this file and all the other files of the package
;;;	in one of your load path directories and the
;;;	following lines in your .emacs:
;;;	
;;;	(autoload 'hm--html-mode "hm--html-mode" "HTML major mode." t)
;;;
;;;	(or (assoc "\\.html$" auto-mode-alist)
;;;         (setq auto-mode-alist (cons '("\\.html$" . hm--html-mode) 
;;;				        auto-mode-alist)))
;;;	If there is already another html-mode (like psgml in the XEmacs
;;;	19.14, then you must put the following instead of the last form
;;;	in your .emacs:
;;;	(setq auto-mode-alist (cons '("\\.html$" . hm--html-mode) 
;;;				        auto-mode-alist))
;;;
;;;	Look at the file hm--html-configuration for further installation
;;;     points.
;;;	
;;;


(require 'font-lock)
(require 'adapt)
(require 'hm--date)
(require 'hm--html)
(hm--html-load-config-files)
(require 'hm--html-indentation)
(require 'hm--html-menu)
(require 'hm--html-drag-and-drop)
;(hm--html-load-config-files) ; Load the system and user configuration files
(require 'hm--html-keys)


;;; The package version
(defconst hm--html-menus-package-maintainer "muenkel@tnt.uni-hannover.de")

(defconst hm--html-menus-package-name "hm--html-menus")

(defconst hm--html-menus-package-version "5.1")
  

;;; Generate the help buffer faces
(hm--html-generate-help-buffer-faces)

;;; syntax table

(defvar hm--html-mode-syntax-table nil
  "Syntax table used while in html mode.")

(if hm--html-mode-syntax-table
    ()
  (setq hm--html-mode-syntax-table (make-syntax-table))
;  (modify-syntax-entry ?\" ".   " hm--html-mode-syntax-table)
;  (modify-syntax-entry ?\\ ".   " hm--html-mode-syntax-table)
;  (modify-syntax-entry ?'  "w   " hm--html-mode-syntax-table)
  (modify-syntax-entry ?\\ "." hm--html-mode-syntax-table)
  (modify-syntax-entry ?'  "w" hm--html-mode-syntax-table)
  (modify-syntax-entry ?<  "(>" hm--html-mode-syntax-table)
  (modify-syntax-entry ?>  ")<" hm--html-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" hm--html-mode-syntax-table)
  (modify-syntax-entry ?=  "."  hm--html-mode-syntax-table))


;;; abbreviation table

(defvar hm--html-mode-abbrev-table nil
  "Abbrev table used while in html mode.")

(define-abbrev-table 'hm--html-mode-abbrev-table ())

;;; the hm--html-mode

(defvar hm--html-mode-name-string "HTML"
  "The hm--html-mode name string.")

;;;###autoload
(defun hm--html-mode ()
  "Major mode for editing HTML hypertext documents.  
Special commands:\\{hm--html-mode-map}
Turning on hm--html-mode calls the value of the variable hm--html-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map hm--html-mode-map)
  (setq mode-name hm--html-mode-name-string)
  (setq major-mode 'hm--html-mode)
  (setq local-abbrev-table hm--html-mode-abbrev-table)
  (set-syntax-table hm--html-mode-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "<!--" comment-end "-->")
  (make-local-variable 'sentence-end)
  (setq sentence-end "[<>.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*")
  (setq indent-line-function 'hm--html-indent-line)
  (setq idd-actions hm--html-idd-actions)
  (hm--install-html-menu hm--html-mode-pulldown-menu-name)
  (make-variable-buffer-local 'write-file-hooks)
  (add-hook 'write-file-hooks 'hm--html-maybe-new-date-and-changed-comment)
;  (make-local-variable 'font-lock-keywords)
;  (setq font-lock-keywords-case-fold-search t)
;  (setq font-lock-keywords hm--html-font-lock-keywords)
  (put major-mode 'font-lock-defaults '((hm--html-font-lock-keywords
					 hm--html-font-lock-keywords-1
					 hm--html-font-lock-keywords-2)
					t
					t
					nil
					nil
					))
  (run-hooks 'hm--html-mode-hook))

;;;; Minor Modes

;;; hm--html-region-mode 

(defvar hm--html-region-mode nil
  "T, if the region is active in the `hm--html-mode'.")

(make-variable-buffer-local 'hm--html-region-mode)

(add-minor-mode 'hm--html-region-mode " Region" hm--html-region-mode-map)

(if (adapt-xemacsp)

    (defun hm--html-region-mode (&optional arg)
      "Toggle 'hm--html-region-mode'.
With ARG, turn hm--html-region-mode on iff ARG is positive.

If the `major-mode' isn't the `hm--html-mode' then the minor
mode is switched off, regardless of the ARG and the state
of `hm--html-region-mode'."
      (interactive "P")
      (setq zmacs-regions-stays t)
      (setq hm--html-region-mode
	    (and (eq major-mode 'hm--html-mode)
		 (if (null arg) (not hm--html-region-mode)
		   (> (prefix-numeric-value arg) 0))))
      )

    (defun hm--html-region-mode (&optional arg)
      "Toggle 'hm--html-region-mode'.
With ARG, turn hm--html-region-mode on iff ARG is positive.

If the `major-mode' isn't the `hm--html-mode' then the minor
mode is switched off, regardless of the ARG and the state
of `hm--html-region-mode'."
      (interactive "P")
      (setq hm--html-region-mode
	    (and (eq major-mode 'hm--html-mode)
		 (if (null arg) (not hm--html-region-mode)
		   (> (prefix-numeric-value arg) 0))))
      (if hm--html-region-mode
	  (define-key hm--html-mode-map
	    hm--html-emacs19-popup-noregion-menu-button
	    nil)
	(if hm--html-expert
	    (define-key hm--html-mode-map
	      hm--html-emacs19-popup-noregion-menu-button
	      hm--html-menu-noregion-expert-map)
	  (define-key hm--html-mode-map
	      hm--html-emacs19-popup-noregion-menu-button
	      hm--html-menu-noregion-novice-map)))
      )

    )

;(or (assq 'hm--html-region-mode minor-mode-alist)
;    (setq minor-mode-alist
;	  (purecopy
;	   (append minor-mode-alist
;		   '((hm--html-region-mode " Region"))))))



;(defun hm--html-region-mode (on)
;  "Turns the minor mode hm--html-region-mode on or off.
;The function turns the hm--html-region-mode on, if ON is t and off otherwise."
;  (if (eq major-mode 'hm--html-mode)
;      ;;(string= mode-name "HTML")
;      (if on
;	  ;; html-region-mode on
;	  (progn
;	    (setq hm--html-region-mode t)
;	    (use-local-map hm--html-region-mode-map))
;	;; html-region-mode off
;	(setq hm--html-region-mode nil)
;	(use-local-map hm--html-mode-map))))




;;; hm--html-minor-mode

(defvar hm--html-minor-mode nil
  "Non-nil, if the `hm--html-minor-mode' is active.")

(make-variable-buffer-local 'hm--html-minor-mode)

(add-minor-mode 'hm--html-minor-mode " HM-HTML" hm--html-minor-mode-map)

;;;###autoload
(defun hm--html-minor-mode (&optional arg)
  "Toggle hm--html-minor-mode.
With arg, turn hm--html-minor-mode on iff arg is positive."
  (interactive "P")
  (setq hm--html-minor-mode
	(if (null arg) (not hm--html-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if hm--html-minor-mode
      (hm--install-html-menu hm--html-minor-mode-pulldown-menu-name)
    (if (and current-menubar (assoc hm--html-minor-mode-pulldown-menu-name
				    current-menubar))
	(delete-menu-item (list hm--html-minor-mode-pulldown-menu-name))))
  )
  

;;; hm--html-minor-region-mode

(defvar hm--html-minor-region-mode nil
  "Non-nil, if the `hm--html-minor-region-mode' is active.")

(make-variable-buffer-local 'hm--html-minor-region-mode)

(add-minor-mode 'hm--html-minor-region-mode 
		" Region" 
		hm--html-minor-region-mode-map)


(if (adapt-xemacsp)

    (defun hm--html-minor-region-mode (&optional arg)
      "Toggle `hm--html-minor-region-mode'.
With arg, turn `hm--html-minor-region-mode' on iff arg is positive.

But however, if the `hm--html-minor-mode' isn't active, then it
turns `hm--html-minor-region-mode' off."
      (interactive "P")
      (setq zmacs-regions-stays t)
      (setq hm--html-minor-region-mode
	    (and hm--html-minor-mode
		 (if (null arg) (not hm--html-minor-region-mode)
		   (> (prefix-numeric-value arg) 0))))
      )

    (defun hm--html-minor-region-mode (&optional arg)
      "Toggle `hm--html-minor-region-mode'.
With arg, turn `hm--html-minor-region-mode' on iff arg is positive.

But however, if the `hm--html-minor-mode' isn't active, then it
turns `hm--html-minor-region-mode' off."
      (interactive "P")
      (setq hm--html-minor-region-mode
	    (and hm--html-minor-mode
		 (if (null arg) (not hm--html-minor-region-mode)
		   (> (prefix-numeric-value arg) 0))))
      (if hm--html-minor-region-mode
	  (define-key hm--html-minor-mode-map
	    hm--html-emacs19-popup-noregion-menu-button
	    nil)
	(if hm--html-expert
	    (define-key hm--html-minor-mode-map
	      hm--html-emacs19-popup-noregion-menu-button
	      hm--html-menu-noregion-expert-map)
	  (define-key hm--html-minor-mode-map
	      hm--html-emacs19-popup-noregion-menu-button
	      hm--html-menu-noregion-novice-map)))
      )
    )

  

;;; Hook function for toggling the region minor modes
(defun hm--html-switch-region-modes-on ()
  "Switches the region minor modes of the hm--html-menus package on.
This function should be only be used for the `zmacs-activate-region-hook'
or for the `activate-mark-hook'."
  (hm--html-region-mode 1)
  (hm--html-minor-region-mode 1))

(defun hm--html-switch-region-modes-off ()
  "Switches the region minor modes of the hm--html-menus package on.
This function should be only be used for the `zmacs-deactivate-region-hook'
or for the `deactivate-mark-hook'."
  (hm--html-region-mode -1)
  (hm--html-minor-region-mode -1))
    

;;; Run the load hook
(run-hooks 'hm--html-load-hook)


;;; Announce the feature hm--html-configuration
(provide 'hm--html-mode)
