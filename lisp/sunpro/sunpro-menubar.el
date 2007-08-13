;;; sunpro-menubar.el --- Initialize the SunPro menubar

;; Copyright (C) 1993, 1994 Sun Microsystems, Inc

;; Author:	Aaron Endelman <endelman@Eng.Sun.COM>
;; Maintainer:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
;; Created:	93/09/13 15:16:24

;; Keywords:	SunPro menubar initialization

;;; Commentary:
;;  Creates the default SunPro menubars.

;;; To Do:

;;; Code:

(defconst sunpro-menubar
 (purecopy-menubar			;the simple, new user menubar
  (list
   '("File"
     ["New"			sunpro-new-buffer       t]
     ["Open:"			find-file	        t]
     ["Include File:"		insert-file		t]
     "-----"
     ["Save"			save-buffer		t nil]
     ["Save As:"		write-file		t]
     ["Revert..."		revert-buffer		t nil]
     "-----"
     ["Print"		        lpr-buffer		t nil]
     "-----"
     ["Close"		        delete-frame		t]
     ["Exit XEmacs"		save-buffers-kill-emacs	t]
     )
   
   '("Edit"
     ["Undo"			advertised-undo		t]
     "-----"
     ["Cut"			x-kill-primary-selection   t]
     ["Copy"			x-copy-primary-selection   t]
     ["Paste"			x-yank-clipboard-selection t]
     ["Delete"			x-delete-primary-selection t]
     "-----"
     ["Select Block"		mark-paragraph 		t]
     ["Select All"		mark-whole-buffer	t]
     )
   
   '("View"
     ["New View"                make-frame             t]
     "-----"
     ["Split Window"		(split-window)		t]
     ["Unsplit Window"		delete-other-windows    t]
     ["Close Buffer"		(kill-buffer nil)	t nil]
     "-----! before list all buffers"
     ["List All Buffers"	 list-buffers		t]
     )
     
   '("Find"
     ["Forward:"		sunpro-search-forward	t]
     ["Backward:"		sunpro-search-backward	t]
     ["And Replace:"		sunpro-query-replace	t]
     )

   ;; Copy the options menu from the default menubar
  (car (find-menu-item default-menubar '("Options")))

   '("Utilities"
     ["Cancel Command"		(keyboard-quit)	t]
     "-----"
     ["Execute Macro"		call-last-kbd-macro last-kbd-macro]
     ["Start Macro Recording"	start-kbd-macro     (not defining-kbd-macro)]
     ["End Macro Recording"	end-kbd-macro	    defining-kbd-macro]
     "-----"
     ["Spell"		ispell-buffer	t]
     ["Sort"		sort-lines	t]
     "-----"
     ["Format Paragraph  "	fill-paragraph	t]
     "-----"
     ["Goto Line:"		goto-line	t]
     )
   
   ;; the following is supposed to be here!  It ensures that the
   ;; Help item is always the rightmost item.

    nil		; the partition: menus after this are flushright

    '("Help"	["About XEmacs..."	about-xemacs		t]
		"-----"
		["XEmacs WWW Page"	xemacs-www-page		t]
		["XEmacs FAQ via WWW"	xemacs-www-faq		t]
		"-----"
		["Info"			info			t]
		["Describe Mode"	describe-mode		t]
 		["Hyper Apropos..."	hyper-apropos		t]
		["Command Apropos..."	command-apropos		t]
		["Full Apropos..."	apropos			t]
		["List Keybindings"	describe-bindings	t]
		["Describe Key..."	describe-key		t]
		["Describe Function..."	describe-function	t]
		["Describe Variable..."	describe-variable	t]
		"-----"
		["Unix Manual..."	manual-entry		t]
		["XEmacs Tutorial"	help-with-tutorial	t]
		["XEmacs News"		view-emacs-news		t]
		))))

(set-menubar sunpro-menubar)

(defconst programmer-menu '(["Programmer Menus" 
			     (toggle-programmer-menus) 
			     :style toggle 
			     :selected programmer-menus-p]
			    ["-----! before save options" nil t]))
(setq save-options-menu-item
      (car (find-menu-item default-menubar '("Options" "Save Options"))))
(delete-menu-item '("Options" "Save Options"))
(add-menu () "Options" (append 
			 (cdr (car
			       (find-menu-item default-menubar '("Options"))))
			 programmer-menu
			 (list save-options-menu-item)))

;;;
;;; helper commands
;;;

(defun sunpro-new-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled")))

(defun sunpro-new-window ()
  (interactive)
  (switch-to-buffer-other-frame (generate-new-buffer "Untitled")))

(defun sunpro-clone-buffer ()
  (interactive)
    (let
	((old (current-buffer)))
      (switch-to-buffer (generate-new-buffer (buffer-name old)))
    (insert-buffer old)))

(defun sunpro-search-forward ()
  (interactive)
  (if isearch-mode (isearch-repeat-forward)
    (x-isearch-maybe-with-region)))

(defun sunpro-search-backward ()
  (interactive)
  (if isearch-mode (isearch-repeat-backward)
    (x-isearch-maybe-with-region t)))

(put 'sunpro-search-forward 'isearch-command t)
(put 'sunpro-search-backward 'isearch-command t)

(defun sunpro-query-replace ()
  (interactive)
  (call-interactively 'query-replace))

(defun sunpro-menu-quit ()
  "Abort minibuffer input if any."
  (while (not (zerop (minibuffer-depth)))
    (abort-recursive-edit)))

(defvar programmer-menus-p nil)
(defvar sccs-or-vc-menus 'sccs
  "Choose to use the SCCS or the VC menu.")

(defun toggle-programmer-menus ()
  (interactive)
  (if programmer-menus-p
      (progn
	(if (equal sccs-or-vc-menus 'sccs)
	    (delete-menu-item '("SCCS"))
	  (delete-menu-item '("VC")))
	(delete-menu-item '("SPARCworks"))
	(delete-menu-item '("Options" "SPARCworks"))
	(delete-menu-item '("Options" "-----! before save options"))
	(delete-menu-item '("Help" "SPARCworks"))
	(setq programmer-menus-p nil))
    (progn
      (require 'eos-load "sun-eos-load")
      (eos::start)
      (if (equal sccs-or-vc-menus 'sccs)
	  (progn
	    (delete-menu-item '("VC"))
	    (require 'sccs)
	    (add-menu '() "SCCS" (cdr sccs-menu)))
	(progn
	  (require 'vc)
	  (delete-menu-item '("SCCS"))
	  (add-menu '() "VC" vc-default-menu)))
      (setq programmer-menus-p t))))

(defun sunpro-build-buffers-menu-hook ()
  "For use as a value of activate-menubar-hook.
This function changes the contents of the \"View\" menu to add
at the end the current set of buffers.  Only the most-recently-used few buffers
will be listed on the menu, for efficiency reasons.  You can control how
many buffers will be shown by setting `buffers-menu-max-size'.
You can control the text of the menu items by redefining the function
`format-buffers-menu-line'."
  (let ((buffer-menu (car (find-menu-item current-menubar '("View"))))
	buffers)
    (if (not buffer-menu)
	nil
      (setq buffer-menu (cdr buffer-menu))
      (setq buffers (buffer-list))

      (if (and (integerp buffers-menu-max-size)
	       (> buffers-menu-max-size 1))
	  (if (> (length buffers) buffers-menu-max-size)
	      (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

      (setq buffers (build-buffers-menu-internal buffers))
      (setq buffers (append (delq nil buffers)))
      ;; slightly (only slightly) more efficient to not install the menubar
      ;; if it hasn't visibly changed.
      (let ((tail (member "-----! before list all buffers" (cdr buffer-menu)))
	    )
	(if tail
	    (if (equal buffers (cdr tail))
		t  ; return t meaning "no change"
	      (setcdr tail buffers)
	      nil)
	  ;; only the first time
	  (add-menu nil "View" (append buffer-menu
					  '("-----! before list all buffers")
					  buffers))
	  nil
	  )))))

(add-hook 'activate-menubar-hook 'sunpro-build-buffers-menu-hook)

;;; sunpro-menubar.el ends here
