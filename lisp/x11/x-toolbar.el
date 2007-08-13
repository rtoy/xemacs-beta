;; Toolbar support for X.
;; Copyright (C) 1994 Andy Piper <andyp@parallax.demon.co.uk>
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 1996 Ben Wing <wing@666.com>

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

;;
;; toolbar ispell variables and defuns
;;

(defun toolbar-ispell ()
  "Intelligently spell the region or buffer."
  (interactive)
  (if (region-active-p)
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))

;;
;; toolbar mail variables and defuns
;;

(defvar toolbar-use-separate-mail-frame nil
  "If non-nil run mail in a separate frame.")

(defvar toolbar-mail-frame nil
  "The frame in which mail is displayed.")

(defvar toolbar-mail-command 'vm
  "The mail reader to run.")

(defun toolbar-mail ()
  "Run mail in a separate frame."
  (interactive)
  (if (not toolbar-use-separate-mail-frame)
      (funcall toolbar-mail-command)
    (if (or (not toolbar-mail-frame)
	    (not (frame-live-p toolbar-mail-frame)))
	(progn
	  (setq toolbar-mail-frame (make-frame))
	  (add-hook 'vm-quit-hook
		    '(lambda ()
		       (save-excursion
			 (if (frame-live-p toolbar-mail-frame)
			     (delete-frame toolbar-mail-frame)))))
	  (select-frame toolbar-mail-frame)
	  (raise-frame toolbar-mail-frame)
	  (funcall toolbar-mail-command)))
    (if (frame-iconified-p toolbar-mail-frame)
	(deiconify-frame toolbar-mail-frame))
    (select-frame toolbar-mail-frame)
    (raise-frame toolbar-mail-frame)))

;;
;; toolbar info variables and defuns
;;

(defvar toolbar-info-frame nil
  "The frame in which info is displayed.")

(defun toolbar-info ()
  "Run info in a separate frame."
  (interactive)
  (if (or (not toolbar-info-frame)
	  (not (frame-live-p toolbar-info-frame)))
      (progn
	(setq toolbar-info-frame (make-frame))
	(select-frame toolbar-info-frame)
	(raise-frame toolbar-info-frame)))
  (if (frame-iconified-p toolbar-info-frame)
      (deiconify-frame toolbar-info-frame))
  (select-frame toolbar-info-frame)
  (raise-frame toolbar-info-frame)
  (info))

;;
;; toolbar debug variables and defuns
;;

(defun toolbar-debug ()
  (interactive)
  (if (featurep 'eos-debugger)
      (call-interactively 'eos::start-debugger)
    (require 'gdbsrc)
    (call-interactively 'gdbsrc))
  )

(defvar compile-command)

(defun toolbar-compile ()
  "Run compile without having to touch the keyboard."
  (interactive)
  (require 'compile)
  (popup-dialog-box
   `(,(concat "Compile:\n        " compile-command)
     ["Compile" (compile compile-command) t]
     ["Edit command" compile t]
     nil
     ["Cancel" (message "Quit") t])))

;;
;; toolbar news variables and defuns
;;

(defvar toolbar-news-frame nil
  "The frame in which news is displayed.")

(defun toolbar-news ()
  "Run GNUS in a separate frame."
  (interactive)
  (if (or (not toolbar-news-frame)
	  (not (frame-live-p toolbar-news-frame)))
      (progn
	(setq toolbar-news-frame (make-frame))
	(add-hook 'gnus-exit-gnus-hook
		  '(lambda ()
		     (if (frame-live-p toolbar-news-frame)
			 (delete-frame toolbar-news-frame))))
	(select-frame toolbar-news-frame)
	(raise-frame toolbar-news-frame)
	(gnus)))
  (if (frame-iconified-p toolbar-news-frame)
      (deiconify-frame toolbar-news-frame))
  (select-frame toolbar-news-frame)
  (raise-frame toolbar-news-frame))

(defvar toolbar-last-win-icon nil "A `last-win' icon set.")
(defvar toolbar-next-win-icon nil "A `next-win' icon set.")
(defvar toolbar-file-icon     nil "A `file' icon set.")
(defvar toolbar-folder-icon   nil "A `folder' icon set")
(defvar toolbar-disk-icon     nil "A `disk' icon set.")
(defvar toolbar-printer-icon  nil "A `printer' icon set.")
(defvar toolbar-cut-icon      nil "A `cut' icon set.")
(defvar toolbar-copy-icon     nil "A `copy' icon set.")
(defvar toolbar-paste-icon    nil "A `paste' icon set.")
(defvar toolbar-undo-icon     nil "An `undo' icon set.")
(defvar toolbar-spell-icon    nil "A `spell' icon set.")
(defvar toolbar-replace-icon  nil "A `replace' icon set.")
(defvar toolbar-mail-icon     nil "A `mail' icon set.")
(defvar toolbar-info-icon     nil "An `info' icon set.")
(defvar toolbar-compile-icon  nil "A `compile' icon set.")
(defvar toolbar-debug-icon    nil "A `debugger' icon set.")
(defvar toolbar-news-icon     nil "A `news' icon set.")

;;; each entry maps a variable to the prefix used.

(defvar init-x-toolbar-list
  '((toolbar-last-win-icon . "last-win")
    (toolbar-next-win-icon . "next-win")
    (toolbar-file-icon     . "file")
    (toolbar-folder-icon   . "folder")
    (toolbar-disk-icon     . "disk")
    (toolbar-printer-icon  . "printer")
    (toolbar-cut-icon      . "cut")
    (toolbar-copy-icon     . "copy")
    (toolbar-paste-icon    . "paste")
    (toolbar-undo-icon     . "undo")
    (toolbar-spell-icon    . "spell")
    (toolbar-replace-icon  . "replace")
    (toolbar-mail-icon     . "mail")
    (toolbar-info-icon     . "info-def")
    (toolbar-compile-icon  . "compile")
    (toolbar-debug-icon    . "debug")
    (toolbar-news-icon     . "news")))

(defun init-x-toolbar ()
  (mapcar
   (lambda (cons)
     (let ((prefix (expand-file-name (cdr cons) toolbar-icon-directory)))
       (set (car cons)
	    (if (featurep 'xpm)
		(toolbar-make-button-list
		 (concat prefix "-up.xpm")
		 nil
		 (concat prefix "-xx.xpm")
		 (concat prefix "-cap-up.xpm")
		 nil
		 (concat prefix "-cap-xx.xpm"))
	      (toolbar-make-button-list
	       (concat prefix "-up.xbm")
	       (concat prefix "-dn.xbm")
	       (concat prefix "-xx.xbm")
	       )))))
   init-x-toolbar-list)
  ;; do this now because errors will occur if the icon symbols
  ;; are not initted
  (set-specifier default-toolbar initial-toolbar-spec))
  
(defvar initial-toolbar-spec
  '(;;[toolbar-last-win-icon	pop-window-configuration
    ;;(frame-property (selected-frame)
    ;;		'window-config-stack) t	"Most recent window config"]
    ;; #### Illicit knowledge?
    ;; #### These don't work right - not consistent!
    ;; I don't know what's wrong; perhaps `selected-frame' is wrong
    ;; sometimes when this is evaluated.  Note that I even tried to
    ;; kludge-fix this by calls to `set-specifier-dirty-flag' in
    ;; pop-window-configuration and such.
    
    ;;[toolbar-next-win-icon	unpop-window-configuration
    ;;(frame-property (selected-frame)
    ;;	'window-config-unpop-stack) t "Undo \"Most recent window config\""]
    ;; #### Illicit knowledge?
    
    [toolbar-file-icon		find-file	t	"Open a file"	]
    [toolbar-folder-icon	dired		t	"View directory"]
    [toolbar-disk-icon		save-buffer	t	"Save buffer"	]
    [toolbar-printer-icon	lpr-buffer	t	"Print buffer"	]
    [toolbar-cut-icon		x-kill-primary-selection t "Kill region"]
    [toolbar-copy-icon		x-copy-primary-selection t "Copy region"]
    [toolbar-paste-icon		x-yank-clipboard-selection t
				"Paste from clipboard"]
    [toolbar-undo-icon		undo		t	"Undo edit"	]
    [toolbar-spell-icon		toolbar-ispell	t	"Spellcheck"	]
    [toolbar-replace-icon	query-replace	t	"Replace text"	]
    [toolbar-mail-icon		toolbar-mail	t	"Mail"		]
    [toolbar-info-icon		toolbar-info	t	"Information"	]
    [toolbar-compile-icon	toolbar-compile	t	"Compile"	]
    [toolbar-debug-icon		toolbar-debug	t	"Debug"		]
    [toolbar-news-icon		toolbar-news	t	"News"		])
  "The initial toolbar for a buffer.")

(defun x-init-toolbar-from-resources (locale)
  (x-init-specifier-from-resources
   top-toolbar-height 'natnum locale
   '("topToolBarHeight" . "TopToolBarHeight"))
  (x-init-specifier-from-resources
   bottom-toolbar-height 'natnum locale
   '("bottomToolBarHeight" . "BottomToolBarHeight"))
  (x-init-specifier-from-resources
   left-toolbar-width 'natnum locale
   '("leftToolBarWidth" . "LeftToolBarWidth"))
  (x-init-specifier-from-resources
   right-toolbar-width 'natnum locale
   '("rightToolBarWidth" . "RightToolBarWidth")))
