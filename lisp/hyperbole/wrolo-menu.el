;;!emacs
;;
;; FILE:         wrolo-menu.el
;; SUMMARY:      Pulldown and popup menus of Hyperbole rolodex commands.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, matching, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:    28-Oct-94 at 10:59:44
;; LAST-MOD:     31-Oct-95 at 18:45:24 by Bob Weiner
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

;;; This definition is used by InfoDock and XEmacs.
(defconst infodock-wrolo-menu
  '("Rolodex"
    ["Manual"            (id-tool-invoke id-man-rolodex) t]
    "----"
    ["Add-Entry"         (id-tool-invoke 'rolo-add) t]
    ["Delete-Entry"      (id-tool-invoke 'rolo-kill) t]
    ["Display-Prior-Matches" (id-tool-invoke 'rolo-display-matches) t]
    ["Edit-Entry"        (id-tool-invoke 'rolo-edit) t]
    ["Edit-Rolodex"      (id-tool-invoke
			  '(progn (require 'wrolo)
				  (find-file (car rolo-file-list))
				  (setq buffer-read-only nil)))
     t]
    ["Insert-Entry-at-Point" (id-tool-invoke 'rolo-yank) t]
    ["Mail-to-Address"   (id-tool-invoke 'rolo-mail-to) t]
    ["Search-for-Regexp" (id-tool-invoke 'rolo-grep)  t]
    ["Search-for-String" (id-tool-invoke 'rolo-fgrep) t]
    ["Search-for-Word"   (id-tool-invoke 'rolo-word)  t]
    ["Sort-Entries"      (id-tool-invoke 'rolo-sort)  t]
    ))

;;; This definition is used by InfoDock only.
(defconst id-menubar-wrolo
  (list
   '("Wrolo"
     ["Help"                describe-mode                  t]
     ["Manual"              (id-info "(hyperbole.info)Rolo Keys") t]
     "----"
     ["Toggle-Read-Only"    toggle-read-only               t]
     ["Write (Save as)"     write-file                     t]
     "----"
     ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
     )
   '["Edit-Entry-at-Point"  rolo-edit-entry         t]
    ["Mail-to-Address"      (id-tool-invoke 'rolo-mail-to) t]
   '("Move"
     ["Scroll-Backward"     scroll-down             t]
     ["Scroll-Forward"      scroll-up               t]
     ["To-Beginning"        beginning-of-buffer     t]
     ["To-End"              end-of-buffer           t]
     "----"
     ["To-Next-Entry"          outline-next-visible-heading t]
     ["To-Next-Same-Level"     outline-forward-same-level t]
     ["To-Previous-Entry"      outline-previous-visible-heading t]
     ["To-Previous-Same-Level" outline-backward-same-level t]
     ["Up-a-Level"             outline-up-heading t]
     )
   '("Outline"
     ["Hide (Collapse)"      hide-subtree           t]
     ["Show (Expand)"        show-subtree           t]
     ["Show-All"             show-all               t]
     ["Show-Only-First-Line" hide-body              t]
     )
   '["Next-Match"          rolo-next-match         t]
   '["Previous-Match"      rolo-previous-match     t]
   infodock-wrolo-menu
   ))

;;; This definition is used by InfoDock and XEmacs.
(defconst id-popup-wrolo-menu
  (list
    "Wrolo"
    '["Help"                describe-mode           t]
    '["Manual"              (id-info "(hyperbole.info)Rolo Keys") t]
    "----"
    '["Edit-Entry-at-Point" rolo-edit-entry         t]
    "----"
    '["Next-Match"          rolo-next-match         t]
    '["Previous-Match"      rolo-previous-match     t]
    "----"
    '("Move"
      ["Scroll-Backward"     scroll-down             t]
      ["Scroll-Forward"      scroll-up               t]
      ["To-Beginning"        beginning-of-buffer     t]
      ["To-End"              end-of-buffer           t]
      "----"
      ["To-Next-Entry"          outline-next-visible-heading t]
      ["To-Next-Same-Level"     outline-forward-same-level t]
      ["To-Previous-Entry"      outline-previous-visible-heading t]
      ["To-Previous-Same-Level" outline-backward-same-level t]
      ["Up-a-Level"             outline-up-heading t]
      )
    '("Outline"
      ["Hide (Collapse)"      hide-subtree           t]
      ["Show (Expand)"        show-subtree           t]
      ["Show-All"             show-all               t]
      ["Show-Only-First-Line" hide-body              t]
      )
    infodock-wrolo-menu
    "----"
    '["Quit"                (id-tool-quit 'rolo-quit) t]
    ))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; This definition is used only by XEmacs and Emacs19.
(defun wrolo-menubar-menu ()
  "Add a Hyperbole Rolodex menu to the rolodex match buffer menubar."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-wrolo-menu))
	(hyperb:lemacs-p
	 (define-key wrolo-mode-map 'button3 'wrolo-popup-menu))
	(t ;; hyperb:emacs19-p
	 (define-key wrolo-mode-map [down-mouse-3] 'wrolo-popup-menu)
	 (define-key wrolo-mode-map [mouse-3] nil)))
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar)
	   (not (car (find-menu-item current-menubar '("Wrolo")))))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(if (fboundp 'add-submenu)
	    (add-submenu nil id-popup-wrolo-menu)
	  (add-menu nil (car id-popup-wrolo-menu)
		    (cdr id-popup-wrolo-menu))))))

;;; This definition is used only by XEmacs and Emacs19.
(defun wrolo-popup-menu (event)
  "Popup the Hyperbole Rolodex match buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (if (fboundp 'popup-mode-menu)
      (popup-mode-menu)
    (popup-menu id-popup-wrolo-menu)))

(cond ((null hyperb:window-system))
      ((and (featurep 'infodock) (fboundp 'id-menubar-set))
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'wrolo-mode 'id-menubar-wrolo))
      (hyperb:lemacs-p
       ;; XEmacs under a window system
       (add-hook 'wrolo-mode-hook 'wrolo-menubar-menu))
      (hyperb:emacs19-p
       ;; Emacs 19 under a window system
       (require 'lmenu)
       (add-hook 'wrolo-mode-hook 'wrolo-menubar-menu)))

(provide 'wrolo-menu)
