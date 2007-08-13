;;!emacs
;;
;; FILE:         hui-menu.el
;; SUMMARY:      InfoDock/Emacs menubar menu of Hyperbole commands.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:    28-Oct-94 at 10:59:44
;; LAST-MOD:     26-Oct-95 at 23:10:38 by Bob Weiner
;;
;; Copyright (C) 1994-1995 Free Software Foundation, Inc.
;;
;; This file is part of Hyperbole.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'wrolo-menu)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Add Hyperbole menu to menubar.
(defun hyperbole-menubar-menu ()
  "Add the Hyperbole menu to the global menubar."
  (if hyperb:emacs19-p (require 'lmenu))
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar)
	   (not (car (find-menu-item current-menubar '("Hyperbole")))))
      (let ((add-before (if (and (boundp 'infodock-menubar-type)
				 (eq infodock-menubar-type 'menubar-infodock))
			    "Key" nil)))
	(if (fboundp 'add-submenu)
	    (add-submenu nil infodock-hyperbole-menu add-before)
	  (add-menu nil (car infodock-hyperbole-menu)
		    (cdr infodock-hyperbole-menu) add-before)))))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; Don't change this name; doing so will break the way InfoDock
;;; initializes the Hyperbole menu.
(defconst infodock-hyperbole-menu
  (delq nil
	(list
	 "Hyperbole"
	 '["Browse-Manual"      (id-info "(hyperbole.info)Top") t]
	 "----"
	 '["Activate-Button-at-Point" hui:hbut-act t]
	 '["Back-to-Prior-Location" (hhist:remove current-prefix-arg) t]
	 '("Button-File"
	   ["Manual"  (id-info "(hyperbole.info)Button Files") t]
	   "----"
	   ["Edit-Per-Directory-File" (find-file hbmap:filename) t]
	   ["Edit-Personal-File" (find-file
				  (expand-file-name
				   hbmap:filename hbmap:dir-user)) t]
	   )
	 '("Documentation"
	   ["Manual"      (id-info "(hyperbole.info)Top") t]
	   "----"
	   ["Copyright"      (id-info "(hyperbole.info)Top") t]
	   ["Demonstration"  (find-file-read-only
			      (expand-file-name "DEMO" hyperb:dir)) t]
	   ["Manifest"       (find-file-read-only
			      (expand-file-name "MANIFEST" hyperb:dir)) t]
	   ["Glossary"    (id-info "(hyperbole.info)Glossary") t]
	   ["Mail-Lists"  (id-info "(hyperbole.info)Mail Lists") t]
	   ["New-Features" (progn
			     (hact 'link-to-regexp-match
				   "\\*[ \t]+What's New" 2
				   (expand-file-name "README" hyperb:dir))
			     (setq buffer-read-only nil)
			     (toggle-read-only)) t]
	   ["Smart-Key-Summary" (id-browse-file (hypb:mouse-help-file)) t]
	   )
	 '("Explicit-Button"
	   ["Manual"   (id-info "(hyperbole.info)Explicit Buttons") t]
	   "----"
	   ["Activate-at-Point" hui:hbut-act t]
	   ["Create" hui:ebut-create t]
	   ["Delete" hui:ebut-delete t]
	   ["Edit"   hui:ebut-modify t]
	   ("Help"  
	    ["Manual"   (id-info "(hyperbole.info)Location") t]
	    "----"
	    ["Buffer-Buttons"   (hui:hbut-report -1) t]
	    ["Current-Button"   (hui:hbut-report)    t]
	    ["Ordered-Buttons"  (hui:hbut-report 1)  t]
	    )
	   ["Modify" hui:ebut-modify t]
	   ["Rename" hui:ebut-rename t]
	   ["Search" hui:ebut-search t]
	   )
	 '("Global-Button"
	   ["Manual" (id-info "(hyperbole.info)Global Buttons") t]
	   "----"
	   ["Activate" gbut:act t]
	   ["Create" hui:gbut-create t]
	   ["Edit"   hui:gbut-modify t]
	   ["Help"   gbut:help t]
	   ["Modify" hui:gbut-modify t]
	   )
	 '("Implicit-Button"
	   ["Manual"   (id-info "(hyperbole.info)Implicit Buttons") t]
	   "----"
	   ["Activate-at-Point"    hui:hbut-act t]
	   ["Delete-Type"         (hui:htype-delete 'ibtypes) t]
	   ["Help"   hui:hbut-help t]
	   ["Types"  (hui:htype-help 'ibtypes 'no-sort) t]
	   )
	 '("Mail-Lists"
	   ["Manual" (id-info "(hyperbole.info)Suggestion or Bug Reporting")
	    t]
	   "----"
	   ["Change-Hyperbole-Address"
	    (hmail:compose "hyperbole-request@hub.ucsb.edu"
			   '(hact 'hyp-request)) t]
	   ["Change-Hyperbole-Announce-Address"
	    (hmail:compose "hyperbole-request@hub.ucsb.edu"
			   '(hact 'hyp-request)) t]
	   ["Mail-to-Hyperbole-List"
	    (hmail:compose "hyperbole@hub.ucsb.edu" '(hact 'hyp-config)) t]
	   )
	 (if hyperb:kotl-p
	     '("Outline"
	       ["Manual" (id-info "(hyperbole.info)Outliner") t]
	       ["Example"   (find-file-read-only
			     (expand-file-name
			      "EXAMPLE.kotl" (concat hyperb:dir "kotl/")))
		t]
	       "----"
	       ["Create-File"    kfile:find t]
	       ["View-File"      kfile:view t]
	       "----"
	       ["Collapse-Tree" (progn (kotl-mode:is-p)
				       (kotl-mode:hide-tree
					(kcell-view:label))) t]
	       ["Create-Link" klink:create t]
	       ["Expand-All-Trees" kotl-mode:show-all t]
	       ["Expand-Tree" (progn (kotl-mode:is-p)
				     (kotl-mode:show-tree
				      (kcell-view:label))) t]
	       ["Show-Top-Level-Only" kotl-mode:hide-body t]
	       ))
	 infodock-wrolo-menu
	 '("Types"
	   ["Action-Types-Manual"
	    (id-info "(hyperbole.info)Action Types") t]
	   ["Implicit-Button-Types-Manual"
	    (id-info "(hyperbole.info)Implicit Buttons") t]
	   "----"
	   ["Action-Types"      (hui:htype-help 'actypes) t]
	   ["Implicit-Button-Types" (hui:htype-help 'ibtypes 'no-sort) t]
	   )
	 '("Window-Configuration"
	   ["Manual" (id-info "(hyperbole.info)Window Configurations") t]
	   "----"
	   ["Name-Configuration" wconfig-add-by-name     t]
	   ["Delete-Name"        wconfig-delete-by-name  t]
	   ["Restore-Name"       wconfig-restore-by-name t]
	   "----"
	   ["Pop-from-Ring"      wconfig-delete-pop      t]
	   ["Save-to-Ring"       wconfig-ring-save       t]
	   ["Yank-from-Ring"     wconfig-yank-pop        t]
	   )
	 '["Quit" (progn
		    ;; Delete Hyperbole menu item from all menubars.
		    (mapcar
		     (function
		      (lambda (buf)
			(set-buffer buf)
			(if (assoc "Hyperbole" current-menubar)
			    (delete-menu-item '("Hyperbole")))))
		     (buffer-list))
		    ;;
		    ;; Remove Hyperbole button comment from future
		    ;; outgoing mail.
		    (if (boundp 'smail:comment)
			(setq smail:comment "")))
	   t]
	 )))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hui-menu)
