;;!emacs
;;
;; FILE:         hui-menu.el
;; SUMMARY:      InfoDock/Emacs menubar menu of Hyperbole commands.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:    28-Oct-94 at 10:59:44
;; LAST-MOD:     19-Feb-97 at 10:50:57 by Bob Weiner
;;
;; Copyright (C) 1994, 1995, 1996, 1997  Free Software Foundation, Inc.
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

;; Ensure that this variable is defined to avert any error within
;; the Customization menu.
(defvar highlight-headers-follow-url-netscape-new-window nil
  "*Whether to make Netscape create a new window when a URL is sent to it.")

(defconst hui-menu-options
  (append '("Display-Referents-in"
	    "----"
	    "----")
	  (mapcar (function (lambda (sym)
			      (vector
			       (capitalize (symbol-name sym))
			       (` (setq hpath:display-where '(, sym)))
			       :style 'radio
			       :selected (` (eq hpath:display-where
						'(, sym))))))
		  (mapcar 'car hpath:display-where-alist))
	  '("----"
	    "Display-URLs-in"
	    "----"
	    "----"
	    ["Here"
	     (setq action-key-url-function 'w3-fetch
		   highlight-headers-follow-url-function
		   action-key-url-function)
	     :style radio
	     :selected (eq action-key-url-function 'w3-fetch)]
	    ["Current-Netscape-Window"
	     (setq action-key-url-function
		   'highlight-headers-follow-url-netscape
		   highlight-headers-follow-url-function
		   action-key-url-function
		   highlight-headers-follow-url-netscape-new-window
		   nil)
	     :style radio
	     :selected
	     (and (eq action-key-url-function
		      'highlight-headers-follow-url-netscape)
		  (not highlight-headers-follow-url-netscape-new-window))]
	    ["New-Netscape-Window"
	     (setq action-key-url-function
		   'highlight-headers-follow-url-netscape
		   highlight-headers-follow-url-function
		   action-key-url-function
		   highlight-headers-follow-url-netscape-new-window
		   t)
	     :style radio
	     :selected
	     (and (eq action-key-url-function
		      'highlight-headers-follow-url-netscape)
		  highlight-headers-follow-url-netscape-new-window)]
	    ["Mosaic"
	     (setq action-key-url-function
		   'highlight-headers-follow-url-mosaic
		   highlight-headers-follow-url-function
		   action-key-url-function)
	     :style radio
	     :selected (eq action-key-url-function
			   'highlight-headers-follow-url-mosaic)]
	    )
	  '("----"
	    "Smart-Key-Press-at-Eol"
	    "----"
	    "----"
	    ["Scrolls-a-Windowful"
	     (setq smart-scroll-proportional nil)
	     :style radio :selected (null smart-scroll-proportional)]
	    ["Scrolls-Proportionally"
	     (setq smart-scroll-proportional t)
	     :style radio :selected smart-scroll-proportional]
	    ))
  "Untitled menu of Hyperbole options.")

;;; Don't change this name; doing so will break the way InfoDock
;;; initializes the Hyperbole menu.
(defconst infodock-hyperbole-menu
  (delq nil
	(list
	 "Hyperbole"
	 '["About" (hypb:display-file-with-logo
		    (expand-file-name "ABOUT" hyperb:dir)) t]
	 '["Manual"      (id-info "(hyperbole.info)Top") t]
	 "----"
	 '["Activate-Button-at-Point" hui:hbut-current-act t]
	 '["Back-to-Prior-Location" (hhist:remove current-prefix-arg) t]
	 '("Button-File"
	   ["Manual"  (id-info "(hyperbole.info)Button Files") t]
	   "----"
	   ["Edit-Per-Directory-File" (find-file hbmap:filename) t]
	   ["Edit-Personal-File" (find-file
				  (expand-file-name
				   hbmap:filename hbmap:dir-user)) t]
	   )
	 (cons "Customization" hui-menu-options)
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
	   :filter hui-menu-explicit-buttons
	   ["Activate" hui:hbut-act t]
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
	   :filter hui-menu-global-buttons
	   ["Create" hui:gbut-create t]
	   ["Edit"   hui:gbut-modify t]
	   ["Help"   gbut:help t]
	   ["Modify" hui:gbut-modify t]
	   )
	 '("Implicit-Button"
	   ["Manual"   (id-info "(hyperbole.info)Implicit Buttons") t]
	   "----"
	   ["Activate-at-Point"    hui:hbut-current-act t]
	   ["Delete-Type"         (hui:htype-delete 'ibtypes) t]
	   ["Help"   hui:hbut-help t]
	   ["Types"  (hui:htype-help 'ibtypes 'no-sort) t]
	   )
	 '("Mail-Lists"
	   ["Manual" (id-info "(hyperbole.info)Suggestion or Bug Reporting")
	    t]
	   "----"
	   ["Change-Hyperbole-Address"
	    (hmail:compose "hyperbole-request@infodock.com"
			   '(hact 'hyp-request)) t]
	   ["Change-Hyperbole-Announce-Address"
	    (hmail:compose "hyperbole-request@infodock.com"
			   '(hact 'hyp-request)) t]
	   ["Mail-to-Hyperbole-List"
	    (hmail:compose "hyperbole@infodock.com" '(hact 'hyp-config)) t]
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
;;; Private functions
;;; ************************************************************************

(defvar hui-menu-max-list-length 24
  "Positive integer that caps the length of a dynamic menu list.")

(defvar hui-menu-order-explicit-buttons t
  "When non-nil (default), explicit button menu list is lexicographically ordered.
Otherwise, explicit buttons are listed in their order of appearance within
the current buffer.")

;; List explicit buttons in the current buffer for menu activation.
(defun hui-menu-explicit-buttons (rest-of-menu)
  (delq nil
	(append
	 '(["Manual"   (id-info "(hyperbole.info)Explicit Buttons") t]
	   "----")
	 (let ((labels (ebut:list))
	       (cutoff))
	   (if labels
	       (progn
		 ;; Cutoff list if too long.
		 (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
		     (setcdr cutoff nil))
		 (delq nil
		       (append
			'("----"
			  ["Alphabetize-List"
			   (setq hui-menu-order-explicit-buttons 
				 (not hui-menu-order-explicit-buttons))
			   :style toggle :selected hui-menu-order-explicit-buttons]
			  "Activate:")
			(mapcar (function (lambda (label)
					    (vector label `(ebut:act ,label) t)))
				(if hui-menu-order-explicit-buttons
				    (sort labels 'string-lessp)
				  labels))
			(if cutoff '(". . ."))
			'("----" "----"))))))
	 rest-of-menu)))

;; List existing global buttons for menu activation.
(defun hui-menu-global-buttons (rest-of-menu)
  (delq nil
	(append
	 '(["Manual" (id-info "(hyperbole.info)Global Buttons") t]
	   "----")
	 (let ((labels (gbut:label-list))
	       (cutoff))
	   (if labels
	       (progn
		 ;; Cutoff list if too long.
		 (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
		     (setcdr cutoff nil))
		 (delq nil
		       (append
			'("----" "Activate:")
			(mapcar (function (lambda (label)
					    (vector label `(gbut:act ,label) t)))
				(sort labels 'string-lessp))
			(if cutoff '(". . ."))
			'("----" "----"))))))
	 rest-of-menu)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hui-menu)
