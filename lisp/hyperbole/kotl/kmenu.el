;;!emacs
;;
;; FILE:         kmenu.el
;; SUMMARY:      Pulldown and popup menus for kotl-mode, the Hyperbole Outliner.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, outlines, wp
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:    28-Mar-94 at 11:22:09
;; LAST-MOD:     22-Oct-96 at 01:30:50 by Bob Weiner
;;
;; Copyright (C) 1994-1995 Free Software Foundation, Inc.
;;
;; This file is part of Hyperbole.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; This definition is used by InfoDock only.
(defconst id-menubar-kotl
  '(
    ("Koutline"
     ["All-Cells-Attributes" (kotl-mode:cell-help nil -1)  t]
     ["Help"                describe-mode                  t]
     ["Manual"              (id-info "(hyperbole.info)Outliner") t]
     "----"
     ["Find (Open)"         find-file                      t]
     ["Find-Read-Only"      find-file-read-only            t]
     ["Save"                save-buffer                    t]
     ["Toggle-Read-Only"    toggle-read-only               t]
     ["Write (Save as)"     kfile:write                    t]
     "----"
     ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
     )
    ("Edit"
     ["Set-Cell-Attribute"  kotl-mode:set-cell-attribute   t]
     "----"
     ["Add-Child"           kotl-mode:add-child            t]
     ["Add-Cell"            kotl-mode:add-cell             t]
     ["Add-Parent"          kotl-mode:add-parent           t]
     ["Append-Cell"         kotl-mode:append-cell          t]
     ["Split-Cell"          kotl-mode:split-cell           t]
     "----"
     ["Kill-to-Cell-End"    kotl-mode:kill-contents        t]
     ["Kill-Tree"           kotl-mode:kill-tree            t]
     ["Yank"                kotl-mode:yank                 t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     "----"
     ["Fill"                kotl-mode:fill-cell            t]
     ["Fill-Paragraph"      kotl-mode:fill-paragraph       t]
     ["Set-Fill-Prefix"     kotl-mode:set-fill-prefix      t]
     )
    ("Jump-to"
     ["Cell"                kotl-mode:goto-cell            t]
     "----"
     ["Cell-Beginning"      kotl-mode:beginning-of-cell    t]
     ["Cell-End"            kotl-mode:end-of-cell          t]
     "----"
     ["Child"               kotl-mode:down-level           t]
     ["Parent"              kotl-mode:up-level             t]
     "----"
     ["Next-Cell"           kotl-mode:next-cell            t]
     ["Prev-Cell"           kotl-mode:previous-cell        t]
     "----"
     ["Next-Same-Level"     kotl-mode:forward-cell         t]
     ["Prev-Same-Level"     kotl-mode:backward-cell        t]
     "----"
     ["First-Sibling"       kotl-mode:first-sibling        t]
     ["Last-Sibling"        kotl-mode:last-sibling         t]
     "----"
     ["Beginning-of-Tree"   kotl-mode:beginning-of-tree    t]
     ["End-of-Tree"         kotl-mode:end-of-tree          t]
     "----"
     ["First-Cell"          kotl-mode:beginning-of-buffer  t]
     ["Last-Cell"           kotl-mode:end-of-buffer        t]
     )
    ("Label-Type"
     ["Alphanumeric (Default)"  (kview:set-label-type kview 'alpha)  t]
     ["Legal"                   (kview:set-label-type kview 'legal)  t]
     ["None"                    (kview:set-label-type kview 'no)     t]
     ["Partial-Alpha"           (kview:set-label-type kview 'partial-alpha) t]
     ["Permanent-Idstamp"       (kview:set-label-type kview 'id)     t]
     ["Stars"                   (kview:set-label-type kview 'star) t]
     )
    ("Link"
     ["Add-at-Point"        klink:create                   t]
     )
    ("Tree"
     ["Copy-to-Buffer"      kotl-mode:copy-to-buffer       t]
     ["Demote"              kotl-mode:demote-tree          t]
     ["Kill"                kotl-mode:kill-tree            t]
     ["Mail"                kotl-mode:mail-tree            t]
     ["Promote"             kotl-mode:promote-tree         t]
     ["Show-Attributes"     (kotl-mode:cell-help nil 2)   t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     )
    ("View"
     ["Set-View-Spec"       kvspec:activate                t]
     ["Toggle-Blank-Lines"  kvspec:toggle-blank-lines      t]
     "----"
     ["Set-Cell-Attribute"   kotl-mode:set-cell-attribute  t]
     ["Show-Cell-Attributes" (kotl-mode:cell-help)        t]
     ["All-Cells-Attributes" (kotl-mode:cell-help nil -1) t]
     ["Show-Tree-Attributes" (kotl-mode:cell-help nil 2)  t]
     "----"
     ["Hide (Collapse)"     kotl-mode:hide-tree            t]
     ["Hide-Levels"         kotl-mode:hide-sublevels       t]
     ["Hide-Subtree"        kotl-mode:hide-subtree         t]
     ["Overview"            kotl-mode:overview             t]
     "----"
     ["Show (Expand)"       kotl-mode:show-tree            t]
     ["Show-All"            kotl-mode:show-all             t]
     ["Show-Subtree"        kotl-mode:show-subtree         t]
     ["Show-Top-Level-Only" kotl-mode:top-cells            t]
     )
    ))

;;; This definition is used by InfoDock and XEmacs.
(defconst id-popup-kotl-menu
  '("Koutline"
    ["All-Cells-Attributes" (kotl-mode:cell-help nil -1)  t]
    ["Help"                describe-mode                  t]
    ["Manual"              (id-info "(hyperbole.info)Outliner") t]
    "----"
    ("Edit"
     ["Set-Cell-Attribute"  kotl-mode:set-cell-attribute   t]
     "----"
     ["Add-Child"           kotl-mode:add-child            t]
     ["Add-Cell"            kotl-mode:add-cell             t]
     ["Add-Parent"          kotl-mode:add-parent           t]
     ["Append-Cell"         kotl-mode:append-cell          t]
     ["Split-Cell"          kotl-mode:split-cell           t]
     "----"
     ["Kill-to-Cell-End"    kotl-mode:kill-contents        t]
     ["Kill-Tree"           kotl-mode:kill-tree            t]
     ["Yank"                kotl-mode:yank                 t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     "----"
     ["Fill"                kotl-mode:fill-cell            t]
     ["Fill-Paragraph"      kotl-mode:fill-paragraph       t]
     ["Set-Fill-Prefix"     kotl-mode:set-fill-prefix      t]
     )
    ("Jump-to"
     ["Cell"                kotl-mode:goto-cell            t]
     "----"
     ["Cell-Beginning"      kotl-mode:beginning-of-cell    t]
     ["Cell-End"            kotl-mode:end-of-cell          t]
     "----"
     ["Child"               kotl-mode:down-level           t]
     ["Parent"              kotl-mode:up-level             t]
     "----"
     ["Next-Cell"           kotl-mode:next-cell            t]
     ["Prev-Cell"           kotl-mode:previous-cell        t]
     "----"
     ["Next-Same-Level"     kotl-mode:forward-cell         t]
     ["Prev-Same-Level"     kotl-mode:backward-cell        t]
     "----"
     ["First-Sibling"       kotl-mode:first-sibling        t]
     ["Last-Sibling"        kotl-mode:last-sibling         t]
     "----"
     ["Beginning-of-Tree"   kotl-mode:beginning-of-tree    t]
     ["End-of-Tree"         kotl-mode:end-of-tree          t]
     "----"
     ["First-Cell"          kotl-mode:beginning-of-buffer  t]
     ["Last-Cell"           kotl-mode:end-of-buffer        t]
     )
    ("Label-Type"
     ["Alphanumeric (Default)"  (kview:set-label-type kview 'alpha)  t]
     ["Legal"                   (kview:set-label-type kview 'legal)  t]
     ["None"                    (kview:set-label-type kview 'no)     t]
     ["Partial-Alpha"           (kview:set-label-type kview 'partial-alpha) t]
     ["Permanent-Idstamp"       (kview:set-label-type kview 'id)     t]
     ["Stars"                   (kview:set-label-type kview 'star) t]
     )
    ("Link"
     ["Add-at-Point"        klink:create                   t]
     )
    ("Tree"
     ["Copy-to-Buffer"      kotl-mode:copy-to-buffer       t]
     ["Demote"              kotl-mode:demote-tree          t]
     ["Kill"                kotl-mode:kill-tree            t]
     ["Mail"                kotl-mode:mail-tree            t]
     ["Promote"             kotl-mode:promote-tree         t]
     ["Show-Attributes"     (kotl-mode:cell-help nil 2)   t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     )
    ("View"
     ["Set-View-Spec"       kvspec:activate                t]
     ["Toggle-Blank-Lines"  kvspec:toggle-blank-lines      t]
     "----"
     ["Set-Cell-Attribute"   kotl-mode:set-cell-attribute  t]
     ["Show-Cell-Attributes" (kotl-mode:cell-help)        t]
     ["All-Cells-Attributes" (kotl-mode:cell-help nil -1) t]
     ["Show-Tree-Attributes" (kotl-mode:cell-help nil 2)  t]
     "----"
     ["Hide (Collapse)"     kotl-mode:hide-tree            t]
     ["Hide-Levels"         kotl-mode:hide-sublevels       t]
     ["Hide-Subtree"        kotl-mode:hide-subtree         t]
     ["Overview"            kotl-mode:overview             t]
     "----"
     ["Show (Expand)"       kotl-mode:show-tree            t]
     ["Show-All"            kotl-mode:show-all             t]
     ["Show-Subtree"        kotl-mode:show-subtree         t]
     ["Show-Top-Level-Only" kotl-mode:top-cells            t]
     )
    "----"
    ["Find (Open)"         find-file                      t]
    ["Find-Read-Only"      find-file-read-only            t]
    ["Save"                save-buffer                    t]
    ["Toggle-Read-Only"    toggle-read-only               t]
    ["Write (Save as)"     kfile:write                    t]
    "----"
    ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
    ))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; This definition is used only by XEmacs and Emacs19.
(defun kotl-menubar-menu ()
  "Add a Koutline menu to the menubar for each koutline buffer."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-kotl-menu))
	(hyperb:lemacs-p
	 (define-key kotl-mode-map 'button3 'kotl-popup-menu))
	(t ;; hyperb:emacs19-p
	 (define-key kotl-mode-map [down-mouse-3] 'kotl-popup-menu)
	 (define-key kotl-mode-map [mouse-3] nil)))
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar)
	   (not (car (find-menu-item current-menubar '("Koutline")))))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(if (fboundp 'add-submenu)
	    (add-submenu nil id-popup-kotl-menu)
	  (add-menu nil (car id-popup-kotl-menu) (cdr id-popup-kotl-menu))))))

;;; This definition is used only by XEmacs and Emacs19.
(defun kotl-popup-menu (event)
  "Popup the Koutline buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (if (fboundp 'popup-mode-menu)
      (popup-mode-menu)
    (popup-menu id-popup-kotl-menu)))

(cond ((null hyperb:window-system))
      ((fboundp 'id-menubar-set)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'kotl-mode 'id-menubar-kotl))
      (hyperb:lemacs-p
       ;; XEmacs under a window system
       (add-hook 'kotl-mode-hook 'kotl-menubar-menu))
      (hyperb:emacs19-p
       ;; Emacs 19 under a window system
       (require 'lmenu)
       (add-hook 'kotl-mode-hook 'kotl-menubar-menu)))

(provide 'kmenu)
