;;; x-toolbar.el -- Runtime initialization of XEmacs toolbar

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1994 Andy Piper <andyp@parallax.demon.co.uk>
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 1996 Ben Wing <wing@666.com>

;; Maintainer: XEmacs development team
;; Keywords: frames, dumped

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

;;; Synched up:  Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs (when X and toolbar support is compiled in).

;; Miscellaneous toolbar functions, useful for users to redefine, in
;; order to get different behaviour.

;;; Code:

(eval-when-compile
  (require 'pending-del))

(defgroup toolbar nil
  "Configure XEmacs Toolbar functions and properties"
  :group 'environment)


(defun toolbar-not-configured ()
  (ding)
  (message "Configure the item via `M-x customize RET toolbar RET'"))

(defcustom toolbar-open-function 'find-file
  "*Function to call when the open icon is selected."
  :type '(radio (function-item find-file)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-open ()
  (interactive)
  (call-interactively toolbar-open-function))

(defcustom toolbar-dired-function 'dired
  "*Function to call when the dired icon is selected."
  :type '(radio (function-item dired)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-dired ()
  (interactive)
  (call-interactively toolbar-dired-function))

(defcustom toolbar-save-function 'save-buffer
  "*Function to call when the save icon is selected."
  :type '(radio (function-item save-buffer)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-save ()
  (interactive)
  (call-interactively toolbar-save-function))

(defcustom toolbar-print-function 'lpr-buffer
  "*Function to call when the print icon is selected."
  :type '(radio (function-item lpr-buffer)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-print ()
  (interactive)
  (call-interactively toolbar-print-function))

(defcustom toolbar-cut-function 'x-kill-primary-selection
  "*Function to call when the cut icon is selected."
  :type '(radio (function-item x-kill-primary-selection)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-cut ()
  (interactive)
  (call-interactively toolbar-cut-function))

(defcustom toolbar-copy-function 'x-copy-primary-selection
  "*Function to call when the copy icon is selected."
  :type '(radio (function-item x-copy-primary-selection)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-copy ()
  (interactive)
  (call-interactively toolbar-copy-function))

(defcustom toolbar-paste-function 'x-yank-clipboard-selection
  "*Function to call when the paste icon is selected."
  :type '(radio (function-item x-yank-clipboard-selection)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-paste ()
  (interactive)
  ;; This horrible kludge is for pending-delete to work correctly.
  (and (boundp 'pending-delete)
       pending-delete
       (let ((this-command toolbar-paste-function))
	 (pending-delete-pre-hook)))
  (call-interactively toolbar-paste-function))

(defcustom toolbar-undo-function 'undo
  "*Function to call when the undo icon is selected."
  :type '(radio (function-item undo)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-undo ()
  (interactive)
  (call-interactively toolbar-undo-function))

(defcustom toolbar-replace-function 'query-replace
  "*Function to call when the replace icon is selected."
  :type '(radio (function-item query-replace)
                (function :tag "Other"))
  :group 'toolbar)

(defun toolbar-replace ()
  (interactive)
  (call-interactively toolbar-replace-function))

;;
;; toolbar ispell variables and defuns
;;

(defun toolbar-ispell-internal ()
  (interactive)
     (if (region-active-p)
	 (ispell-region (region-beginning) (region-end))
       (ispell-buffer)))

(defcustom toolbar-ispell-function 'toolbar-ispell-internal
  "*Function to call when the ispell icon is selected."
  :type '(radio (function-item toolbar-ispell-internal)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-ispell ()
  "Intelligently spell the region or buffer."
  (interactive)
  (call-interactively toolbar-ispell-function))

;;
;; toolbar mail variables and defuns
;;

;; This used to be a macro that expanded its arguments to a form that
;; called `call-process'.  With the advent of customize, it's better
;; to have it as a defun, to make customization easier.
(defun toolbar-external (process &rest args)
  (interactive)
  (apply 'call-process process nil 0 nil args))

(defcustom toolbar-mail-commands-alist
  `((not-configured . toolbar-not-configured)
    (vm		. vm)
    (gnus	. gnus-no-server)
    (rmail	. rmail)
    (mh		. mh-rmail)
    (pine	. (toolbar-external "xterm" "-e" "pine")) ; *gag*
    (elm	. (toolbar-external "xterm" "-e" "elm"))
    (mutt	. (toolbar-external "xterm" "-e" "mutt"))
    (exmh	. (toolbar-external "exmh"))
    (netscape	. (toolbar-external "netscape" "mailbox:")))
  "*Alist of mail readers and their commands.
The car of each alist element is the mail reader, and the cdr is the form
used to start it."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Mailer") (function :tag "Start with")))
  :group 'toolbar)

(defcustom toolbar-mail-reader 'not-configured
  "*Mail reader toolbar will invoke.
The legal values are the keys from `toolbar-mail-command-alist', which
 should be used to add new mail readers.
Mail readers known by default are vm, gnus, rmail, mh, pine, elm,
 mutt, exmh and netscape."
  :type '(choice (const :tag "Not Configured" not-configured)
		 (const vm) (const gnus) (const rmail) (const mh)
		 (const pine) (const elm) (const mutt) (const exmh)
		 (const netscape)
		 (symbol :tag "Other"
			 :validate (lambda (wid)
				     (if (assq (widget-value wid)
					       toolbar-mail-commands-alist)
					 nil
				       (widget-put wid :error
						   "Unknown mail reader")
				       wid))))
  :group 'toolbar)


(defun toolbar-mail ()
  "Run mail in a separate frame."
  (interactive)
  (let ((command (assq toolbar-mail-reader toolbar-mail-commands-alist)))
    (if (not command)
	(error "Uknown mail reader %s" toolbar-mail-reader))
    (funcall (cdr command))))

;;
;; toolbar info variables and defuns
;;

(defvar toolbar-info-frame nil
  "The frame in which info is displayed.")

(defcustom Info-frame-plist 
    (append (list 'width 80)
	    (let ((h (plist-get default-frame-plist 'height)))
	      (when h (list 'height h))))
    "Frame plist for the Info frame."
  :type '(repeat (group :inline t
		  (symbol :tag "Property")
		  (sexp :tag "Value")))
  :group 'info)

(defun toolbar-info ()
  "Run info in a separate frame."
  (interactive)
  (if (or (not toolbar-info-frame)
	  (not (frame-live-p toolbar-info-frame)))
      (progn
	(setq toolbar-info-frame (make-frame Info-frame-plist))
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
    (call-interactively 'gdbsrc)))

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

(defcustom toolbar-news-commands-alist
  `((not-configured . toolbar-not-configured)
    (gnus	. toolbar-gnus)			; M-x all-hail-gnus
    (rn		. (toolbar-external "xterm" "-e" "rn"))
    (nn		. (toolbar-external "xterm" "-e" "nn"))
    (trn	. (toolbar-external "xterm" "-e" "trn"))
    (xrn	. (toolbar-external "xrn"))
    (slrn	. (toolbar-external "xterm" "-e" "slrn"))
    (pine	. (toolbar-external "xterm" "-e" "pine")) ; *gag*
    (tin	. (toolbar-external "xterm" "-e" "tin")) ; *gag*
    (netscape	. (toolbar-external "netscape" "news:")))
  "*Alist of news readers and their commands.
The car of each alist element the pair is the news reader, and the cdr
is the form used to start it."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Reader") (sexp :tag "Start with")))
  :group 'toolbar)

(defcustom toolbar-news-reader 'not-configured
  "*News reader toolbar will invoke.
The legal values are the keys from `toolbar-news-command-alist', which should
 be used to add new news readers.
Newsreaders known by default are gnus, rn, nn, trn, xrn, slrn, pine
 and netscape."
  :type '(choice (const :tag "Not Configured" not-configured)
		 (const gnus) (const rn) (const nn) (const trn)
		 (const xrn) (const slrn) (const pine) (const tin)
		 (const netscape)
		 (symbol :tag "Other"
			 :validate (lambda (wid)
				     (if (assq (widget-value wid)
					       toolbar-news-commands-alist)
					 nil
				       (widget-put wid :error
						   "Unknown news reader")
				       wid))))
  :group 'toolbar)

(defcustom toolbar-news-use-separate-frame t
  "*Whether Gnus is invoked in a separate frame."
  :type 'boolean
  :group 'toolbar)

(defvar toolbar-news-frame nil
  "The frame in which news is displayed.")

(defvar toolbar-news-frame-properties nil
  "The properties of the frame in which news is displayed.")

(defun toolbar-gnus ()
  "Run Gnus in a separate frame."
  (interactive)
  (when (or (not toolbar-news-frame)
	    (not (frame-live-p toolbar-news-frame)))
    (setq toolbar-news-frame (make-frame toolbar-news-frame-properties))
    (add-hook 'gnus-exit-gnus-hook
	      (lambda ()
		(when (frame-live-p toolbar-news-frame)
		  (if (cdr (frame-list))
		      (delete-frame toolbar-news-frame))
                  (setq toolbar-news-frame nil))))
    (select-frame toolbar-news-frame)
    (raise-frame toolbar-news-frame)
    (gnus))
  (if (frame-iconified-p toolbar-news-frame)
      (deiconify-frame toolbar-news-frame))
  (select-frame toolbar-news-frame)
  (raise-frame toolbar-news-frame))

(defun toolbar-news ()
  "Run News (in a separate frame??)."
  (interactive)
  (let ((command (assq toolbar-news-reader toolbar-news-commands-alist)))
    (if (not command)
	(error "Unknown news reader %s" toolbar-news-reader))
    (funcall (cdr command))))

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
  (toolbar-add-item-data init-x-toolbar-list )
  ;; do this now because errors will occur if the icon symbols
  ;; are not initted
  (set-specifier default-toolbar initial-toolbar-spec))
  
(defun toolbar-add-item-data ( icon-list &optional icon-dir )
  (if (eq icon-dir nil)
      (setq icon-dir toolbar-icon-directory))
  (mapcar
   (lambda (cons)
     (let ((prefix (expand-file-name (cdr cons)  icon-dir)))
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
   icon-list  )
  )

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

    [toolbar-file-icon		toolbar-open	t	"Open a file"]
    [toolbar-folder-icon	toolbar-dired	t	"View directory"]
    [toolbar-disk-icon		toolbar-save	t	"Save buffer"]
    [toolbar-printer-icon	toolbar-print	t	"Print buffer"]
    [toolbar-cut-icon		toolbar-cut	t	"Kill region"]
    [toolbar-copy-icon		toolbar-copy	t	"Copy region"]
    [toolbar-paste-icon		toolbar-paste	t	"Paste from clipboard"]
    [toolbar-undo-icon		toolbar-undo	t	"Undo edit"]
    [toolbar-spell-icon		toolbar-ispell	t	"Spellcheck"]
    [toolbar-replace-icon	toolbar-replace	t	"Replace text"]
    [toolbar-mail-icon		toolbar-mail	t	"Mail"]
    [toolbar-info-icon		toolbar-info	t	"Information"]
    [toolbar-compile-icon	toolbar-compile	t	"Compile"]
    [toolbar-debug-icon		toolbar-debug	t	"Debug"]
    [toolbar-news-icon		toolbar-news	t	"News"]
)
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

;;; x-toolbar.el ends here
