;;!emacs
;;
;; FILE:         wconfig.el
;; SUMMARY:      Saves and yanks from save ring of window configurations.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     frames, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:    15-Mar-89
;; LAST-MOD:      9-Dec-96 at 18:39:50 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1989-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   This library provides two unrelated means of managing window
;;   configurations, (the set of windows and associated buffers within a
;;   frame).  The first means associates a name with each stored window
;;   configuration.  The name can then be used to retrieve the window
;;   configuration later.  The following functions provide this behavior:
;;
;;      wconfig-add-by-name
;;      wconfig-delete-by-name
;;      wconfig-restore-by-name
;;
;;   The second means of window configuration management is through the use
;;   of a ring structure, just like the Emacs kill ring except the elements
;;   stored are window configurations instead of textual regions.  The
;;   following functions support storage and sequential retrieval of window
;;   configurations:
;;
;;      wconfig-ring-save
;;      wconfig-yank-pop
;;      wconfig-delete-pop
;;
;;   None of this information is stored between Emacs sessions, so your
;;   window configurations will last only through a single session of use.
;;
;;   Based in part on kill-ring code from simple.el.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Recommended key bindings
;;; ************************************************************************

;;; Set up in local "hyperbole.el".

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************
(require 'hargs)
(require 'set)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst wconfig-ring-max 10
  "*Maximum length of window configuration ring before oldest elements are deleted.")

(defvar wconfig-names (set:create)
  "Set of (name . window-configuration) elements.")

(defvar wconfig-ring nil
  "List of window configurations saved in a ring.")

(defvar wconfig-ring-yank-pointer nil
  "The tail of the window configuration ring whose car is the last thing yanked.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; Handling of name associations with each stored window configuration.
;;;###autoload
(defun wconfig-add-by-name (name)
  "Saves the current window configuration under the string NAME.
When called interactively and a window configuration already exists under
NAME, confirms whether or not to replace it."
  (interactive "sName for current window configuration: ")
  (or (stringp name)
      (error "(wconfig-add-by-name): NAME argument is not a string: %s" name))
  (let ((set:equal-op (function (lambda (key elt)
				  (equal key (car elt))))))
    (if (or (not (interactive-p))
	    (not (set:member name wconfig-names))
	    (y-or-n-p
	      (format "Replace existing '%s' window configuration: " name)))
	(progn (setq wconfig-names
		     (set:replace name (current-window-configuration)
				  wconfig-names))
	       (if (interactive-p)
		   (message "Window configuration '%s' saved.  Use 'wconfig-restore-by-name' to restore." name))))))

;;;###autoload
(defun wconfig-delete-by-name (name)
  "Deletes window configuration saved under NAME."
  (interactive (list (hargs:read-match "Delete window configuration named: "
				       wconfig-names nil t)))
  (or (stringp name)
      (error "(wconfig-delete-by-name): NAME argument is not a string: %s" name))
  (let ((set:equal-op (function (lambda (key elt)
				  (equal key (car elt))))))
    (setq wconfig-names (set:remove name wconfig-names))))

;;;###autoload
(defun wconfig-restore-by-name (name)
  "Restores window configuration saved under NAME."
  (interactive (list (hargs:read-match "Restore window configuration named: "
				       wconfig-names nil t)))
  (or (stringp name)
      (error "(wconfig-restore-by-name): NAME argument is not a string: %s" name))
  (let ((wconfig (set:get name wconfig-names)))
    (if wconfig
	(set-window-configuration wconfig)
      (error "(wconfig-restore-by-name): No window configuration named '%s'" name))))

;;; Window configuration ring management (like text kill ring).
;;;###autoload
(defun wconfig-delete-pop ()
  "Replaces current window config with most recently saved config in ring.
Then deletes this new configuration from the ring."
  (interactive)
  (if (not wconfig-ring)
      (error "(wconfig-delete-pop): Window configuration save ring is empty")
    (set-window-configuration (car wconfig-ring))
    (and (eq wconfig-ring wconfig-ring-yank-pointer)
	 (setq wconfig-ring-yank-pointer (cdr wconfig-ring)))
    (setq wconfig-ring (cdr wconfig-ring))))

;;;###autoload
(defun wconfig-ring-save ()
  "Saves the current window configuration onto the save ring.
Use {\\[wconfig-yank-pop]} to restore it at a later time."
  (interactive)
  (setq wconfig-ring (cons (current-window-configuration) wconfig-ring))
  (if (> (length wconfig-ring) wconfig-ring-max)
      (setcdr (nthcdr (1- wconfig-ring-max) wconfig-ring) nil))
  (setq wconfig-ring-yank-pointer wconfig-ring)
  (wconfig-rotate-yank-pointer (1- (length wconfig-ring-yank-pointer)))
  (if (interactive-p)
      (message
	"Window configuration saved.  Use 'wconfig-yank-pop' to restore.")))

(defun wconfig-rotate-yank-pointer (arg)
  "Rotates the yanking point prefix ARG elements in the window configuration save ring.
Interactively, default value of ARG = 1."
  (interactive "p")
  (let ((length (length wconfig-ring)))
    (if (zerop length)
	(error "(wconfig-rotate-yank-pointer): Window configuration save ring is empty")
      (setq wconfig-ring-yank-pointer
	    (nthcdr (% (+ arg (- length (length wconfig-ring-yank-pointer)))
		       length)
		    wconfig-ring)))))

;;;###autoload
(defun wconfig-yank-pop (n)
  "Replaces current window config with prefix arg Nth prior one in save ring.
Interactively, default value of N = 1, meaning the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the oldest
one comes the newest one."
  (interactive "p")
  (wconfig-rotate-yank-pointer n)
  (set-window-configuration (car wconfig-ring-yank-pointer)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(run-hooks 'wconfig-load-hook)

(provide 'wconfig)
