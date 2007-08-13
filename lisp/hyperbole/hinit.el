;;!emacs
;;
;; FILE:         hinit.el
;; SUMMARY:      Standard initializations for Hyperbole hypertext system.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:     1-Oct-91 at 02:32:51
;; LAST-MOD:     17-Feb-97 at 16:03:46 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1997, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar   hyperb:host-domain nil
  "<@domain-name> for current host.  Set automatically by `hyperb:init'.")

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hvar)

(mapcar 'require '(hui-mouse hypb hui hui-mini hbmap hibtypes))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(if (not (fboundp 'br-in-browser))
    ;; Then the OO-Browser is not loaded, so we can never be within the
    ;; browser.  Define this as a dummy function that always returns nil
    ;; until the OO-Browser is ever loaded.
    (defun br-in-browser ()
      "Always returns nil since the OO-Browser is not loaded."
      nil))

(defun hyperb:init ()
  "Standard configuration routine for Hyperbole."
  (interactive)
  (run-hooks 'hyperb:init-hook)
  (hyperb:check-dir-user)
  (or hyperb:host-domain (setq hyperb:host-domain (hypb:domain-name)))
  (hyperb:act-set)
  ;;
  ;; Save button attribute file whenever same dir file is saved and
  ;; `ebut:hattr-save' is non-nil.
  ;;
  (var:append 'write-file-hooks '(hattr:save))
  ;;
  (hyperb:init-menubar))

(defun hyperb:init-menubar ()
  "Add a pulldown menu for Hyperbole, if appropriate."
  (and hyperb:window-system
       (or hyperb:lemacs-p
	   (if hyperb:emacs19-p
	       (require 'lmenu)))
       (require 'hui-menu)
       ;; XEmacs or Emacs19 under a window system; add Hyperbole menu to
       ;; menubar.
       (hyperbole-menubar-menu)))

(defun hyperb:act-set ()
  "COORDINATION IS NOT YET OPERATIONAL.  hui-coord.el IS NOT INCLUDED.
Sets Hyperbole action command to uncoordinated or coordinated operation.
Coordinated is used when `hcoord:hosts' is a non-nil list.
See \"hui-coord.el\"."
  (interactive)
  (fset 'hyperb:act (if (and (boundp 'hcoord:hosts) hcoord:hosts)
		     'hcoord:act 'hbut:act)))


;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hyperb:check-dir-user ()
  "Ensures `hbmap:dir-user' exists and is writable or signals an error."
  (if (or (null hbmap:dir-user) (not (stringp hbmap:dir-user))
	  (and (setq hbmap:dir-user (file-name-as-directory
				     (expand-file-name hbmap:dir-user)))
	       (file-directory-p hbmap:dir-user)
	       (not (file-writable-p (directory-file-name hbmap:dir-user)))))
      (error
       "(hyperb:init): `hbmap:dir-user' must be a writable directory name."))
  (let ((hbmap:dir-user (directory-file-name hbmap:dir-user)))
    (or (file-directory-p hbmap:dir-user)   ;; Exists and is writable.
	(let* ((parent-dir (file-name-directory
			    (directory-file-name hbmap:dir-user))))
	  (cond
	   ((not (file-directory-p parent-dir))
	    (error
	     "(hyperb:init): `hbmap:dir-user' parent dir does not exist."))
	   ((not (file-writable-p parent-dir))
	    (error
	     "(hyperb:init): `hbmap:dir-user' parent directory not writable."))
	   ((or (if (fboundp 'make-directory)
		    (progn (make-directory hbmap:dir-user) t))
		(hypb:call-process-p "mkdir" nil nil hbmap:dir-user))
	    (or (file-writable-p hbmap:dir-user)
		(or (progn (hypb:chmod '+ 700 hbmap:dir-user)
			   (file-writable-p hbmap:dir-user))
		    (error "(hyperb:init): Can't write to 'hbmap:dir-user'.")
		    )))
	   (t (error "(hyperb:init): `hbmap:dir-user' create failed."))))))
  t)

(provide 'hinit)

