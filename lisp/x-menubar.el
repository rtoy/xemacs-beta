;;; x-menubar.el --- Menubar and popup-menu support for X.

;; Copyright (C) 1991-1995, 1997-1998 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996 Ben Wing.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

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
;; along with Xmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is dumped with XEmacs (when X11 and menubar support is compiled
;; in).

;;; Code:

;;; Warning-free compile
(eval-when-compile
  (defvar language-environment-list)
  (defvar bookmark-alist)
  (defvar language-info-alist)
  (defvar current-language-environment)
  (defvar tutorial-supported-languages))

(defconst default-menubar
  (purecopy-menubar
   ;; note backquote.
   `(
     ("File"
      ["Open..." find-file]
      ["Open in Other Window..." find-file-other-window]
      ["Open in New Frame..." find-file-other-frame]
      ["Insert File..." insert-file]
      ["View File..." view-file]
      "------"
      ["Save" save-buffer
       :active (buffer-modified-p)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      ["Save As..." write-file]
      ["Save Some Buffers" save-some-buffers]
      "-----"
      ["Print Buffer" lpr-buffer
       :active (fboundp 'lpr-buffer)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      ["Pretty-Print Buffer" ps-print-buffer-with-faces
       :active (fboundp 'ps-print-buffer-with-faces)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      "-----"
      ["New Frame" make-frame]
      ["Frame on Other Display..." make-frame-on-display]
      ["Delete Frame" delete-frame
       :active (not (eq (next-frame (selected-frame) 'nomini 'window-system)
			(selected-frame)))]
      "-----"
      ["Split Window" split-window-vertically]
      ["Un-Split (Keep This)" delete-other-windows
       :active (not (one-window-p t))]
      ["Un-Split (Keep Others)" delete-window
       :active (not (one-window-p t))]
      "-----"
      ["Revert Buffer" revert-buffer
       :active (or buffer-file-name revert-buffer-function)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      ["Delete Buffer" kill-this-buffer
       :active t
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      "-----"
      ["Exit XEmacs" save-buffers-kill-emacs]
      )

     ("Edit"
      ["Undo" advertised-undo
       :active (and (not (eq buffer-undo-list t))
		    (or buffer-undo-list pending-undo-list))
       :suffix (if (or (eq last-command 'undo)
		       (eq last-command 'advertised-undo))
		       "More" "")]
      ["Redo" redo
       :included (fboundp 'redo)
       :active (not (or (eq buffer-undo-list t)
			 (eq last-buffer-undo-list nil)
			 (not (or (eq last-buffer-undo-list buffer-undo-list)
				  (and (null (car-safe buffer-undo-list))
				       (eq last-buffer-undo-list
					   (cdr-safe buffer-undo-list)))))
			 (or (eq buffer-undo-list pending-undo-list)
			     (eq (cdr buffer-undo-list) pending-undo-list))))
       :suffix (if (eq last-command 'redo) "More" "")]
      ["Cut" x-kill-primary-selection
       :active (and (eq 'x (device-type (selected-device)))
		    (x-selection-owner-p))]
      ["Copy" x-copy-primary-selection
       :active (and (eq 'x (device-type (selected-device)))
		    (x-selection-owner-p))]
      ["Paste" x-yank-clipboard-selection
       :active (and (eq 'x (device-type (selected-device)))
		    (x-selection-exists-p 'CLIPBOARD))]
      ["Clear" x-delete-primary-selection
       :active (and (eq 'x (device-type (selected-device)))
		    (x-selection-owner-p))]
      "----"
      ["Search..." isearch-forward]
      ["Search Backward..." isearch-backward]
      ["Replace..." query-replace]
      "----"
      ["Search (Regexp)..." isearch-forward-regexp]
      ["Search Backward (Regexp)..." isearch-backward-regexp]
      ["Replace (Regexp)..." query-replace-regexp]
      "----"
      ["Goto Line..." goto-line]
      ["What Line" what-line]
      ("Bookmarks"
       :filter bookmark-menu-filter)
      "----"
      ["Start Macro Recording" start-kbd-macro
       :active (not defining-kbd-macro)]
      ["End Macro Recording" end-kbd-macro
       :active defining-kbd-macro]
      ["Execute Last Macro" call-last-kbd-macro
       :active last-kbd-macro]
      "----"
      ["Show Message Log" show-message-log]
      )

     ,@(if (featurep 'mule)
	   '(("Mule"
	      ("Describe language support")
	      ("Set language environment")
	      "--"
	      ["Toggle input method" toggle-input-method]
	      ["Select input method" select-input-method]
	      ["Describe input method" describe-input-method]
	      "--"
	      ["Describe current coding systems"
	       describe-current-coding-system]
	      ["Set coding system of buffer file"
	       set-buffer-file-coding-system]
	      ;; not implemented yet
	      ["Set coding system of terminal"
	       set-terminal-coding-system :active nil]
	      ;; not implemented yet
	      ["Set coding system of keyboard"
	       set-keyboard-coding-system :active nil]
	      ;; not implemented yet
	      ["Set coding system of process"
	       set-current-process-coding-system :active nil]
	      "--"
	      ["Show character table" view-charset-by-menu]
	      ;; not implemented yet
	      ["Show diagnosis for MULE" mule-diag :active nil]
	      ["Show many languages" view-hello-file])))

     ("Apps"
      ["Read Mail (VM)..." vm
       :active (fboundp 'vm)]
      ["Read Mail (MH)..." (mh-rmail t)
       :active (fboundp 'mh-rmail)]
      ["Send mail..." mail
       :active (fboundp 'mail)]
      ["Usenet News" gnus
       :active (fboundp 'gnus)]
      ["Browse the Web" w3
       :active (fboundp 'w3)]
      ["Gopher" gopher
       :active (fboundp 'gopher)]
      "----"
      ["Spell-Check Buffer" ispell-buffer
       :active (fboundp 'ispell-buffer)]
      ["Toggle VI emulation" toggle-viper-mode
       :active (fboundp 'toggle-viper-mode)]
      "----"
      ("Calendar"
       ["3-Month Calendar" calendar
	:active (fboundp 'calendar)]
       ["Diary" diary
	:active (fboundp 'diary)]
       ["Holidays" holidays
	:active (fboundp 'holidays)]
       ;; we're all pagans at heart ...
       ["Phases of the Moon" phases-of-moon
	:active (fboundp 'phases-of-moon)]
       ["Sunrise/Sunset" sunrise-sunset
	:active (fboundp 'sunrise-sunset)])

      ("Games"
       ["Mine Game" xmine
	:active (fboundp 'xmine)]
       ["Tetris" tetris
	:active (fboundp 'tetris)]
       ["Sokoban" sokoban
	:active (fboundp 'sokoban)]
       ["Quote from Zippy" yow
	:active (fboundp 'yow)]
       ["Psychoanalyst" doctor
	:active (fboundp 'doctor)]
       ["Psychoanalyze Zippy!" psychoanalyze-pinhead
	:active (fboundp 'psychoanalyze-pinhead)]
       ["Random Flames" flame
	:active (fboundp 'flame)]
       ["Dunnet (Adventure)" dunnet
	:active (fboundp 'dunnet)]
       ["Towers of Hanoi" hanoi
	:active (fboundp 'hanoi)]
       ["Game of Life" life
	:active (fboundp 'life)]
       ["Multiplication Puzzle" mpuz
	:active (fboundp 'mpuz)]))

     ("Options"
      ("Customize"
       ("Emacs" :filter (lambda (&rest junk)
			  (cdr (custom-menu-create 'emacs))))
       ["Group..." customize-group]
       ["Variable..." customize-variable]
       ["Face..." customize-face]
       ["Saved..." customize-saved]
       ["Set..." customize-customized]
       ["Apropos..." customize-apropos]
       ["Browse..." customize-browse])
      ["Read Only" (toggle-read-only)
       :style toggle :selected buffer-read-only]
      ("Editing Options"
       ["Overstrike" (progn
		       (overwrite-mode current-prefix-arg)
		       (setq-default overwrite-mode overwrite-mode))
	:style toggle :selected overwrite-mode]
       ["Case Sensitive Search" (progn
				  (setq case-fold-search
					(not case-fold-search))
				  (setq-default case-fold-search
						case-fold-search))
	:style toggle :selected (not case-fold-search)]
       ["Case Matching Replace" (setq case-replace (not case-replace))
	:style toggle :selected case-replace]
       ["Auto Delete Selection" (pending-delete-mode
				 (if pending-delete-mode 0 1))
	:style toggle
	:selected (and (boundp 'pending-delete-mode) pending-delete-mode)
	:active (fboundp 'pending-delete-mode)]
       ["Active Regions" (setq zmacs-regions (not zmacs-regions))
	:style toggle :selected zmacs-regions]
       ["Mouse Paste At Text Cursor" (setq mouse-yank-at-point
					   (not mouse-yank-at-point))
	:style toggle :selected mouse-yank-at-point]
       ["Require Newline At End" (setq require-final-newline
				       (or (eq require-final-newline 'ask)
					   (not require-final-newline)))
	:style toggle :selected (eq require-final-newline 't)]
       ["Add Newline When Moving Past End" (setq next-line-add-newlines
						 (not next-line-add-newlines))
	:style toggle :selected next-line-add-newlines]
       )
      ("General Options"
       ["Teach Extended Commands" (setq teach-extended-commands-p
					(not teach-extended-commands-p))
	:style toggle :selected teach-extended-commands-p]
       ["Debug On Error" (setq debug-on-error (not debug-on-error))
	:style toggle :selected debug-on-error]
       ["Debug On Quit" (setq debug-on-quit (not debug-on-quit))
	:style toggle :selected debug-on-quit]
       )
      ("Printing Options"
       ["Command-Line Switches for `lpr'/`lp'..."
	(setq lpr-switches
	      (read-expression "Switches for `lpr'/`lp': "
			       (format "%S" lpr-switches)))
	(boundp 'lpr-switches)]
       ("Pretty-Print Paper Size"
	["Letter"
	 (setq ps-paper-type 'letter)
	 :style radio
	 :selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'letter))
	 :active (fboundp 'ps-print-buffer)]
	["Letter-small"
	 (setq ps-paper-type 'letter-small)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'letter-small))
	 :active (fboundp 'ps-print-buffer)]
	["Legal"
	 (setq ps-paper-type 'legal)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'legal))
	 :active (fboundp 'ps-print-buffer)]
	["Statement"
	 (setq ps-paper-type 'statement)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'statement))
	 :active (fboundp 'ps-print-buffer)]
	["Executive"
	 (setq ps-paper-type 'executive)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'executive))
	 :active (fboundp 'ps-print-buffer)]
	["Tabloid"
	 (setq ps-paper-type 'tabloid)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'tabloid))
	 :active (fboundp 'ps-print-buffer)]
	["Ledger"
	 (setq ps-paper-type 'ledger)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'ledger))
	 :active (fboundp 'ps-print-buffer)]
	["A3"
	 (setq ps-paper-type 'a3)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'a3))
	 :active (fboundp 'ps-print-buffer)]
	["A4"
	 (setq ps-paper-type 'a4)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'a4))
	 :active (fboundp 'ps-print-buffer)]
	["A4small"
	 (setq ps-paper-type 'a4small)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'a4small))
	 :active (fboundp 'ps-print-buffer)]
	["B4"
	 (setq ps-paper-type 'b4)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'b4))
	 :active (fboundp 'ps-print-buffer)]
	["B5"
	 (setq ps-paper-type 'b5)
	 :style radio
	 :selected (and (boundp 'ps-paper-type)
			(eq ps-paper-type 'b5))
	 :active (fboundp 'ps-print-buffer)]
	)
       ["Color Printing"
	(when (boundp 'ps-print-color-p)
	  (if ps-print-color-p
	      (progn
		(setq ps-print-color-p nil)
		(when (and (boundp 'original-face-background)
			   original-face-background)
		  (set-face-background 'default original-face-background)))
	    (setq original-face-background (face-background-instance 'default))
	    (set-face-background 'default "white")
	    (setq ps-print-color-p t)))
	:style toggle :selected (and (boundp 'ps-print-color-p)
				     ps-print-color-p)
	:active (fboundp 'ps-print-buffer)])
      ("\"Other Window\" Location"
       ["Always in Same Frame"
	(setq get-frame-for-buffer-default-instance-limit nil)
	:style radio
	:selected (null get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (2 Frames Max)"
	(setq get-frame-for-buffer-default-instance-limit 2)
	:style radio
	:selected (eq 2 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (3 Frames Max)"
	(setq get-frame-for-buffer-default-instance-limit 3)
	:style radio
	:selected (eq 3 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (4 Frames Max)"
	(setq get-frame-for-buffer-default-instance-limit 4)
	:style radio
	:selected (eq 4 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (5 Frames Max)"
	(setq get-frame-for-buffer-default-instance-limit 5)
	:style radio
	:selected (eq 5 get-frame-for-buffer-default-instance-limit)]
       ["Always Create New Frame"
	(setq get-frame-for-buffer-default-instance-limit 0)
	:style radio
	:selected (eq 0 get-frame-for-buffer-default-instance-limit)]
       "-----"
       ["Temp Buffers Always in Same Frame"
	(setq temp-buffer-show-function 'show-temp-buffer-in-current-frame)
	:style radio
	:selected (eq temp-buffer-show-function
		      'show-temp-buffer-in-current-frame)]
       ["Temp Buffers Like Other Buffers"
	(setq temp-buffer-show-function nil)
	:style radio
	:selected (null temp-buffer-show-function)]
       "-----"
       ["Make current frame gnuserv target"
	(setq gnuserv-frame
	      (if (equal gnuserv-frame (selected-frame))
		  'new
		(selected-frame)))
	:style radio
	:selected (and (boundp 'gnuserv-frame)
		       (equal gnuserv-frame (selected-frame)))]
       )

      "-----"
      ("Syntax Highlighting"
       ["In This Buffer" (font-lock-mode)
	:style toggle :selected (and (boundp 'font-lock-mode) font-lock-mode)
	:active (fboundp 'font-lock-mode)]
       ["Automatic" (if (not (featurep 'font-lock))
			(progn
			  (setq font-lock-auto-fontify t)
			  (require 'font-lock))
		      (setq font-lock-auto-fontify
			    (not font-lock-auto-fontify)))
	:style toggle
	:selected (and (featurep 'font-lock) font-lock-auto-fontify)
	:active (fboundp 'font-lock-mode)]
       "-----"
       ["Fonts" (progn (require 'font-lock)
		       (font-lock-use-default-fonts)
		       (setq font-lock-use-fonts t
			     font-lock-use-colors nil)
		       (font-lock-mode 1))
	:style radio
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       font-lock-use-fonts)
	:active (fboundp 'font-lock-mode)]
       ["Colors" (progn (require 'font-lock)
			(font-lock-use-default-colors)
			(setq font-lock-use-colors t
			      font-lock-use-fonts nil)
			(font-lock-mode 1))
	:style radio
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       font-lock-use-colors)
	:active (fboundp 'font-lock-mode)]
       "-----"
       ["Least" (if (or (and (not (integerp font-lock-maximum-decoration))
			     (not (eq t font-lock-maximum-decoration)))
			(and (integerp font-lock-maximum-decoration)
			     (<= font-lock-maximum-decoration 0)))
		    nil
		  (setq font-lock-maximum-decoration nil)
		  (font-lock-recompute-variables))
	:style radio
	:active (and (boundp 'font-lock-mode) font-lock-mode)
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       (or (and (not (integerp font-lock-maximum-decoration))
				(not (eq t font-lock-maximum-decoration)))
			   (and (integerp font-lock-maximum-decoration)
				(<= font-lock-maximum-decoration 0))))]
       ["More" (if (and (integerp font-lock-maximum-decoration)
			(= 1 font-lock-maximum-decoration))
		   nil
		 (setq font-lock-maximum-decoration 1)
		 (font-lock-recompute-variables))
	:style radio
	:active (and (boundp 'font-lock-mode) font-lock-mode)
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       (integerp font-lock-maximum-decoration)
		       (= 1 font-lock-maximum-decoration))]
       ["Even More" (if (and (integerp font-lock-maximum-decoration)
			     (= 2 font-lock-maximum-decoration))
			nil
		      (setq font-lock-maximum-decoration 2)
		      (font-lock-recompute-variables))
	:style radio
	:active (and (boundp 'font-lock-mode) font-lock-mode)
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       (integerp font-lock-maximum-decoration)
		       (= 2 font-lock-maximum-decoration))]
       ["Most" (if (or (eq font-lock-maximum-decoration t)
		       (and (integerp font-lock-maximum-decoration)
			    (>= font-lock-maximum-decoration 3)))
		   nil
		 (setq font-lock-maximum-decoration t)
		 (font-lock-recompute-variables))
	:style radio
	:active (and (boundp 'font-lock-mode) font-lock-mode)
	:selected (and (boundp 'font-lock-mode)
		       font-lock-mode
		       (or (eq font-lock-maximum-decoration t)
			   (and (integerp font-lock-maximum-decoration)
				(>= font-lock-maximum-decoration 3))))]
       "-----"
       ["Lazy" (progn (require 'lazy-shot)
		      (if (and (boundp 'lazy-shot-mode) lazy-shot-mode)
			  (progn
			    (lazy-shot-mode 0)
			    ;; this shouldn't be necessary so there has to
			    ;; be a redisplay bug lurking somewhere (or
			    ;; possibly another event handler bug)
			    (redraw-modeline)
			    (remove-hook 'font-lock-mode-hook
					 'turn-on-lazy-shot))
			(if font-lock-mode
			    (progn
			      (lazy-shot-mode 1)
			      (redraw-modeline)
			      (add-hook 'font-lock-mode-hook
					'turn-on-lazy-shot)))))
	:active (and (boundp 'font-lock-mode)
		     (boundp 'lazy-shot-mode)
		     font-lock-mode)
	:style toggle
	:selected (and (boundp 'lazy-shot-mode) lazy-shot-mode)]
       ["Caching" (progn (require 'fast-lock)
			 (if fast-lock-mode
			     (progn
			       (fast-lock-mode 0)
			       ;; this shouldn't be necessary so there has to
			       ;; be a redisplay bug lurking somewhere (or
			       ;; possibly another event handler bug)
			       (redraw-modeline))
			   (if font-lock-mode
			       (progn
				 (fast-lock-mode 1)
				 (redraw-modeline)))))
	:active (and (boundp 'font-lock-mode) font-lock-mode)
	:style toggle
	:selected (and (boundp 'fast-lock-mode) fast-lock-mode)]
       )
      ("Paren Highlighting"
       ["None" (paren-set-mode -1)
	:style radio :selected (and (boundp 'paren-mode) (not paren-mode))
	:active (fboundp 'paren-set-mode)]
       ["Blinking Paren" (paren-set-mode 'blink-paren)
	:style radio :selected (and (boundp 'paren-mode)
				    (eq paren-mode 'blink-paren))
	:active (fboundp 'paren-set-mode)]
       ["Steady Paren" (paren-set-mode 'paren)
	:style radio :selected (and (boundp 'paren-mode)
				    (eq paren-mode 'paren))
	:active (fboundp 'paren-set-mode)]
       ["Expression" (paren-set-mode 'sexp)
	:style radio :selected (and (boundp 'paren-mode)
				    (eq paren-mode 'sexp))
	:active (fboundp 'paren-set-mode)]
;;;       ["Nested Shading" (paren-set-mode 'nested)
;;;        :style radio :selected (eq paren-mode 'nested)]
       )
      "-----"
      ("Frame Appearance"
       ,@(if (featurep 'scrollbar)
	     '(["Scrollbars" (if (= (specifier-instance scrollbar-width) 0)
				 (progn
				   (set-specifier scrollbar-width 15)
				   (set-specifier scrollbar-height 15))
			       (set-specifier scrollbar-width 0)
			       (set-specifier scrollbar-height 0))
		:style toggle :selected (> (specifier-instance scrollbar-width) 0)]))
       ["3D Modeline"
	(progn
	  (if (zerop (specifier-instance modeline-shadow-thickness))
	      (set-specifier modeline-shadow-thickness 2)
	    (set-specifier modeline-shadow-thickness 0))
	  (redraw-modeline t))
	:style toggle :selected
	(let ((thickness
	       (specifier-instance modeline-shadow-thickness)))
	  (and (integerp thickness)
	       (> thickness 0)))]
       ["Truncate Lines" (progn
			   (setq truncate-lines (not truncate-lines))
			   (setq-default truncate-lines truncate-lines))
	:style toggle :selected truncate-lines]
       ["Bar Cursor" (progn
		       (setq bar-cursor
			     (if (not bar-cursor) 2 nil))
		       (force-cursor-redisplay))
	:style toggle :selected bar-cursor]
       ["Blinking Cursor" (blink-cursor-mode)
	:style toggle
	:selected (and (boundp 'blink-cursor-mode) blink-cursor-mode)]
       ["Frame-Local Font Menu" (setq font-menu-this-frame-only-p
				      (not font-menu-this-frame-only-p))
	:style toggle :selected (and (boundp 'font-menu-this-frame-only-p)
				     font-menu-this-frame-only-p)]
					;     ["Line Numbers" (line-number-mode nil)
					;      :style toggle :selected line-number-mode]
       )
      ("Menubar Appearance"
       ["Buffers Menu Length..."
	(progn
	  (setq buffers-menu-max-size
		(read-number
		 "Enter number of buffers to display (or 0 for unlimited): "))
	  (if (eq buffers-menu-max-size 0) (setq buffers-menu-max-size nil)))]
       ["Multi-Operation Buffers Sub-Menus"
	(setq complex-buffers-menu-p
	      (not complex-buffers-menu-p))
	:style toggle :selected complex-buffers-menu-p]
       ("Buffers Menu Sorting"
	["Most Recently Used"
	 (progn
	   (setq buffers-menu-sort-function nil)
	   (setq buffers-menu-grouping-function nil))
	 :style radio
	 :selected (null buffers-menu-sort-function)]
	["Alphabetically"
	 (progn
	   (setq buffers-menu-sort-function
		 'sort-buffers-menu-alphabetically)
	   (setq buffers-menu-grouping-function nil))
	 :style radio
	 :selected (eq 'sort-buffers-menu-alphabetically
		       buffers-menu-sort-function)]
	["By Major Mode, Then Alphabetically"
	 (progn
	   (setq buffers-menu-sort-function
		 'sort-buffers-menu-by-mode-then-alphabetically)
	   (setq buffers-menu-grouping-function
		 'group-buffers-menu-by-mode-then-alphabetically))
	 :style radio
	 :selected (eq 'sort-buffers-menu-by-mode-then-alphabetically
		       buffers-menu-sort-function)])
       ["Submenus for Buffer Groups"
	(setq buffers-menu-submenus-for-groups-p
	      (not buffers-menu-submenus-for-groups-p))
	:style toggle
	:selected buffers-menu-submenus-for-groups-p
	:active (not (null buffers-menu-grouping-function))]
       "---"
       ["Ignore Scaled Fonts" (setq font-menu-ignore-scaled-fonts
				    (not font-menu-ignore-scaled-fonts))
	:style toggle :selected (and (boundp 'font-menu-ignore-scaled-fonts)
				     font-menu-ignore-scaled-fonts)]
       )
      ,@(if (featurep 'toolbar)
	    '(("Toolbar Appearance"
	       ["Visible" (set-specifier default-toolbar-visible-p
					 (not (specifier-instance
					       default-toolbar-visible-p)))
		:style toggle
		:selected (specifier-instance default-toolbar-visible-p)]
	       ["Captioned" (set-specifier toolbar-buttons-captioned-p
					   (not (specifier-instance
						 toolbar-buttons-captioned-p)))
		:style toggle
		:selected
		(specifier-instance toolbar-buttons-captioned-p)]
	       ("Default Location"
		["Top" (set-default-toolbar-position 'top)
		 :style radio :selected (eq (default-toolbar-position) 'top)]
		["Bottom" (set-default-toolbar-position 'bottom)
		 :style radio :selected (eq (default-toolbar-position) 'bottom)]
		["Left" (set-default-toolbar-position 'left)
		 :style radio :selected (eq (default-toolbar-position) 'left)]
		["Right" (set-default-toolbar-position 'right)
		 :style radio :selected (eq (default-toolbar-position) 'right)]
		)
	       )))
      ("Mouse"
       ["Avoid-Text"
	(if (equal (device-type) 'x)
	    (if mouse-avoidance-mode
		(mouse-avoidance-mode 'none)
	      (mouse-avoidance-mode 'banish))
	  (beep)
	  (message "This option requires a window system."))
	:style toggle :selected (and (boundp 'mouse-avoidance-mode)
				     mouse-avoidance-mode
				     window-system)
	:active (fboundp 'mouse-avoidance-mode)]
       ["strokes-mode"
	(if (equal (device-type) 'x)
	    (strokes-mode)
	  (beep)
	  (message "This option requires a window system."))
	:style toggle :selected (and (boundp 'strokes-mode)
				     strokes-mode
				     window-system)
	:active (fboundp 'strokes-mode)])
      ("Open URLs With"
       ["Emacs-W3" (setq browse-url-browser-function 'browse-url-w3)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-w3))
	:active (and (fboundp 'browse-url-w3)
		     (fboundp 'w3-fetch))]
       ["Netscape" (setq browse-url-browser-function 'browse-url-netscape)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-netscape))
	:active (fboundp 'browse-url-netscape)]
       ["Mosaic" (setq browse-url-browser-function 'browse-url-mosaic)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-mosaic))
	:active (fboundp 'browse-url-mosaic)]
       ["Mosaic (CCI)" (setq browse-url-browser-function 'browse-url-cci)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-iximosaic))
	:active (fboundp 'browse-url-iximosaic)]
       ["IXI Mosaic" (setq browse-url-browser-function 'browse-url-iximosaic)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-iximosaic))
	:active (fboundp 'browse-url-iximosaic)]
       ["Lynx (xterm)" (setq browse-url-browser-function 'browse-url-lynx-xterm)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-lynx-xterm))
	:active (fboundp 'browse-url-lynx-xterm)]
       ["Lynx (xemacs)" (setq browse-url-browser-function 'browse-url-lynx-emacs)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-lynx-emacs))
	:active (fboundp 'browse-url-lynx-emacs)]
       ["Grail" (setq browse-url-browser-function 'browse-url-grail)
	:style radio
	:selected (and (boundp 'browse-url-browser-function)
		       (eq browse-url-browser-function 'browse-url-grail))
	:active (fboundp 'browse-url-grail)]
       )
      "-----"
      ["Browse Faces..." (customize-face nil)]
      ("Font"   :filter font-menu-family-constructor)
      ("Size"	:filter font-menu-size-constructor)
      ("Weight"	:filter font-menu-weight-constructor)
      "-----"
      ["Save Options" save-options-menu-settings]
      )

     ("Buffers"
      :filter buffers-menu-filter
      ["List All Buffers" list-buffers]
      "--"
      )

     ("Tools"
      ["Grep..." grep
       :active (fboundp 'grep)]
      ["Compile..." compile
       :active (fboundp 'compile)]
      ["Shell" shell
       :active (fboundp 'shell)]
      ["Shell Command..." shell-command
       :active (fboundp 'shell-command)]
      ["Shell Command on Region..." shell-command-on-region
       :active (and (fboundp 'shell-command-on-region) (region-exists-p))]
      ["Debug (GDB)..." gdb
       :active (fboundp 'gdb)]
      ["Debug (DBX)..." dbx
       :active (fboundp 'dbx)]
      "-----"
      ("Tags"
       ["Find Tag..." find-tag]
       ["Find Other Window..." find-tag-other-window]
       ["Next Tag..." (find-tag nil)]
       ["Next Other Window..." (find-tag-other-window nil)]
       ["Next File" next-file]
       "-----"
       ["Tags Search..." tags-search]
       ["Tags Replace..." tags-query-replace]
       ["Continue Search/Replace" tags-loop-continue]
       "-----"
       ["Pop stack" pop-tag-mark]
       ["Apropos..." tags-apropos]
       "-----"
       ["Set Tags Table File..." visit-tags-table]
       ))

     nil				; the partition: menus after this are flushright

     ("Help"
      ["About XEmacs..." about-xemacs]
      ("Basics"
       ["Installation" describe-installation
	:active (boundp 'Installation-string)]
       ;; Tutorials.
       ,(if (featurep 'mule)
	    ;; Mule tutorials.
	    (let ((lang language-info-alist)
		  submenu tut)
	      (while lang
		(and (setq tut (assq 'tutorial (car lang)))
		     (not (string= (caar lang) "ASCII"))
		     (setq
		      submenu
		      (cons
		       `[,(caar lang) (help-with-tutorial nil ,(cdr tut))]
		       submenu)))
		(setq lang (cdr lang)))
	      (append `("Tutorials"
			:filter tutorials-menu-filter
			["Default" help-with-tutorial t
			 ,(concat "(" current-language-environment ")")])
		      submenu))
	  ;; Non mule tutorials.
	  (let ((lang tutorial-supported-languages)
		submenu)
	    (while lang
	      (setq submenu
		    (cons
		     `[,(caar lang)
		       (help-with-tutorial ,(format "TUTORIAL.%s"
						    (cadr (car lang))))]
		     submenu))
	      (setq lang (cdr lang)))
	    (append '("Tutorials"
		      ["English" help-with-tutorial])
		    submenu)))
       ["News" view-emacs-news]
       ["Packages" finder-by-keyword]
       ["Splash" xemacs-splash-buffer])
      "-----"
      ("XEmacs FAQ"
       ["FAQ (local)" xemacs-local-faq]
       ["FAQ via WWW" xemacs-www-faq	(boundp 'browse-url-browser-function)]
       ["Home Page" xemacs-www-page		(boundp 'browse-url-browser-function)])
      ("Samples"
       ["Sample .emacs" (find-file (expand-file-name "sample.emacs"
						     data-directory))]
       ["Sample .Xdefaults" (find-file (expand-file-name "sample.Xdefaults"
							 data-directory))]
       ["Sample enriched" (find-file (expand-file-name "enriched.doc"
						       data-directory))])
      "-----"
      ("Lookup in Info"
       ["Key Binding..." Info-goto-emacs-key-command-node]
       ["Command..." Info-goto-emacs-command-node]
       ["Function..." Info-elisp-ref]
       ["Topic..." Info-query])
      ("Manuals"
       ["Info" info]
       ["Unix Manual..." manual-entry])
      ("Commands & Keys"
       ["Mode" describe-mode]
       ["Apropos..." hyper-apropos]
       ["Apropos Docs..." apropos-documentation]
       "-----"
       ["Key..." describe-key]
       ["Bindings" describe-bindings]
       ["Mouse Bindings" describe-pointer]
       ["Recent Keys" view-lossage]
       "-----"
       ["Function..." describe-function]
       ["Variable..." describe-variable]
       ["Locate Command..." where-is])
      "-----"
      ["Recent Messages" view-lossage]
      ("Misc"
       ["No Warranty" describe-no-warranty]
       ["XEmacs License" describe-copying]
       ["The Latest Version" describe-distribution])
      ["Submit Bug Report" send-pr]))))


(defun maybe-add-init-button ()
  "Don't call this.
Adds `Load .emacs' button to menubar when starting up with -q."
  ;; by Stig@hackvan.com
  (cond
   (init-file-user nil)
   ((file-exists-p (cond
		    ((eq system-type 'ms-dos)
		     (concat "~" (user-login-name) "/_emacs"))
		    ((eq system-type 'vax-vms)
		     "sys$login:.emacs")
		    (t
		     (concat "~" (user-login-name) "/.emacs"))))
    (add-menu-button nil
		     ["Load .emacs"
		      (progn (delete-menu-item '("Load .emacs"))
			     (load-user-init-file (user-login-name)))
		      ]
		     "Help"))
   (t nil)))

(add-hook 'before-init-hook 'maybe-add-init-button)


;;; The File menu

(defvar put-buffer-names-in-file-menu t)


;;; The Bookmarks menu

(defun bookmark-menu-filter (&rest ignore)
  (let ((definedp (and (boundp 'bookmark-alist)
		       bookmark-alist
		       t)))
    `(,(if definedp
	   '("Jump to Bookmark"
	     :filter (lambda (&rest junk)
		       (mapcar #'(lambda (bmk)
				   `[,bmk (bookmark-jump ',bmk)])
			       (bookmark-all-names))))
	 ["Jump to Bookmark" nil nil])
      ["Set bookmark" bookmark-set
       :active (fboundp 'bookmark-set)]
      "---"
      ["Insert contents" bookmark-menu-insert
       :active (fboundp 'bookmark-menu-insert)]
      ["Insert location" bookmark-menu-locate
       :active (fboundp 'bookmark-menu-locate)]
      "---"
      ["Rename bookmark" bookmark-menu-rename
       :active (fboundp 'bookmark-menu-rename)]
      ,(if definedp
	   '("Delete Bookmark"
	     :filter (lambda (&rest junk)
		       (mapcar #'(lambda (bmk)
				   `[,bmk (bookmark-delete ',bmk)])
			       (bookmark-all-names))))
	 ["Delete Bookmark" nil nil])
      ["Edit Bookmark List" bookmark-bmenu-list	,definedp]
      "---"
      ["Save bookmarks"        bookmark-save		,definedp]
      ["Save bookmarks as..."  bookmark-write		,definedp]
      ["Load a bookmark file" bookmark-load
       :active (fboundp 'bookmark-load)])))

;;; The Buffers menu

(defgroup buffers-menu nil
  "Customization of `Buffers' menu."
  :group 'menu)

(defcustom buffers-menu-max-size 25
  "*Maximum number of entries which may appear on the \"Buffers\" menu.
If this is 10, then only the ten most-recently-selected buffers will be
shown.  If this is nil, then all buffers will be shown.  Setting this to
a large number or nil will slow down menu responsiveness."
  :type '(choice (const :tag "Show all" nil)
		 (integer 10))
  :group 'buffers-menu)

(defcustom complex-buffers-menu-p nil
  "*If non-nil, the buffers menu will contain several commands.
Commands will be presented as submenus of each buffer line.  If this
is false, then there will be only one command: select that buffer."
  :type 'boolean
  :group 'buffers-menu)

(defcustom buffers-menu-submenus-for-groups-p nil
  "*If non-nil, the buffers menu will contain one submenu per group of buffers.
The grouping function is specified in `buffers-menu-grouping-function'.
If this is an integer, do not build submenus if the number of buffers
is not larger than this value."
  :type '(choice (const :tag "No Subgroups" nil)
		 (integer :tag "Max. submenus" 10)
		 (sexp :format "%t\n" :tag "Allow Subgroups" :value t))
  :group 'buffers-menu)

(defcustom buffers-menu-switch-to-buffer-function 'switch-to-buffer
  "*The function to call to select a buffer from the buffers menu.
`switch-to-buffer' is a good choice, as is `pop-to-buffer'."
  :type '(radio (function-item switch-to-buffer)
		(function-item pop-to-buffer)
		(function :tag "Other"))
  :group 'buffers-menu)

(defcustom buffers-menu-omit-function 'buffers-menu-omit-invisible-buffers
  "*If non-nil, a function specifying the buffers to omit from the buffers menu.
This is passed a buffer and should return non-nil if the buffer should be
omitted.  The default value `buffers-menu-omit-invisible-buffers' omits
buffers that are normally considered \"invisible\" (those whose name
begins with a space)."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'buffers-menu)

(defcustom buffers-menu-format-buffer-line-function 'format-buffers-menu-line
  "*The function to call to return a string to represent a buffer in the
buffers menu.  The function is passed a buffer and should return a string.
The default value `format-buffers-menu-line' just returns the name of
the buffer.  Also check out `slow-format-buffers-menu-line' which
returns a whole bunch of info about a buffer."
  :type 'function
  :group 'buffers-menu)

(defcustom buffers-menu-sort-function
  'sort-buffers-menu-by-mode-then-alphabetically
  "*If non-nil, a function to sort the list of buffers in the buffers menu.
It will be passed two arguments (two buffers to compare) and should return
T if the first is \"less\" than the second.  One possible value is
`sort-buffers-menu-alphabetically'; another is
`sort-buffers-menu-by-mode-then-alphabetically'."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'buffers-menu)

(defcustom buffers-menu-grouping-function
  'group-buffers-menu-by-mode-then-alphabetically
  "*If non-nil, a function to group buffers in the buffers menu together.
It will be passed two arguments, successive members of the sorted buffers
list after being passed through `buffers-menu-sort-function'.  It should
return non-nil if the second buffer begins a new group.  The return value
should be the name of the old group, which may be used in hierarchical
buffers menus.  The last invocation of the function contains nil as the
second argument, so that the name of the last group can be determined.

The sensible values of this function are dependent on the value specified
for `buffers-menu-sort-function'."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'buffers-menu)

(defun buffers-menu-omit-invisible-buffers (buf)
  "For use as a value of `buffers-menu-omit-function'.
Omits normally invisible buffers (those whose name begins with a space)."
  (not (null (string-match "\\` " (buffer-name buf)))))

(defun sort-buffers-menu-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts the buffers in alphabetical order by name, but puts buffers beginning
with a star at the end of the list."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (star1p (not (null (string-match "\\`*" nam1))))
	 (star2p (not (null (string-match "\\`*" nam2)))))
    (if (not (eq star1p star2p))
	(not star1p)
      (string-lessp nam1 nam2))))

(defun sort-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts first by major mode and then alphabetically by name, but puts buffers
beginning with a star at the end of the list."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (star1p (not (null (string-match "\\`*" nam1))))
	 (star2p (not (null (string-match "\\`*" nam2))))
	 (mode1 (symbol-value-in-buffer 'major-mode buf1))
	 (mode2 (symbol-value-in-buffer 'major-mode buf2)))
    (cond ((not (eq star1p star2p)) (not star1p))
	  ((and star1p star2p (string-lessp nam1 nam2)))
	  ((string-lessp mode1 mode2) t)
	  ((string-lessp mode2 mode1) nil)
	  (t (string-lessp nam1 nam2)))))

;; this version is too slow on some machines.
(defun slow-format-buffers-menu-line (buffer)
  "For use as a value of `buffers-menu-format-buffer-line-function'.
This returns a string containing a bunch of info about the buffer."
  (format "%s%s %-19s %6s %-15s %s"
	  (if (buffer-modified-p buffer) "*" " ")
	  (if (symbol-value-in-buffer 'buffer-read-only buffer) "%" " ")
	  (buffer-name buffer)
	  (buffer-size buffer)
	  (symbol-value-in-buffer 'mode-name buffer)
	  (or (buffer-file-name buffer) "")))

(defun format-buffers-menu-line (buffer)
  "For use as a value of `buffers-menu-format-buffer-line-function'.
This just returns the buffer's name."
  (buffer-name buffer))

(defun group-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-grouping-function'.
This groups buffers by major mode.  It only really makes sense if
`buffers-menu-sorting-function' is
`sort-buffers-menu-by-mode-then-alphabetically'."
  (cond ((string-match "\\`*" (buffer-name buf1))
	 (and (null buf2) "*Misc*"))
	((or (null buf2)
	     (string-match "\\`*" (buffer-name buf2))
	     (not (eq (symbol-value-in-buffer 'major-mode buf1)
		      (symbol-value-in-buffer 'major-mode buf2))))
	 (symbol-value-in-buffer 'mode-name buf1))
	(t nil)))

(defun buffer-menu-save-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (save-buffer)))

(defun buffer-menu-write-file (buffer)
  (save-excursion
    (set-buffer buffer)
    (write-file (read-file-name
		 (format "Write %s to file: "
			 (buffer-name (current-buffer)))))))

(defsubst build-buffers-menu-internal (buffers)
  (let (name line)
    (mapcar
     #'(lambda (buffer)
	 (if (eq buffer t)
	     "---"
	   (setq line (funcall buffers-menu-format-buffer-line-function
			       buffer))
	   (if complex-buffers-menu-p
	       (delq nil
		     (list line
			   (vector "Switch to Buffer"
				   (list buffers-menu-switch-to-buffer-function
					 (setq name (buffer-name buffer)))
				   t)
			   (if (eq buffers-menu-switch-to-buffer-function
				   'switch-to-buffer)
			       (vector "Switch to Buffer, Other Frame"
				       (list 'switch-to-buffer-other-frame
					     (setq name (buffer-name buffer)))
				       t)
			     nil)
			   (if (and (buffer-modified-p buffer)
				    (buffer-file-name buffer))
			       (vector "Save Buffer"
				       (list 'buffer-menu-save-buffer name) t)
			     ["Save Buffer" nil nil]
			     )
			   (vector "Save As..."
				   (list 'buffer-menu-write-file name) t)
			   (vector "Delete Buffer" (list 'kill-buffer name)
				   t)))
	     ;; ### We don't want buffer names to be translated,
	     ;; ### so we put the buffer name in the suffix.
	     ;; ### Also, avoid losing with non-ASCII buffer names.
	     ;; ### We still lose, however, if complex-buffers-menu-p. --mrb
	     (vector ""
		     (list buffers-menu-switch-to-buffer-function
			   (buffer-name buffer))
		     t line))))
     buffers)))

(defun buffers-menu-filter (menu)
  "This is the menu filter for the top-level buffers \"Buffers\" menu.
It dynamically creates a list of buffers to use as the contents of the menu.
Only the most-recently-used few buffers will be listed on the menu, for
efficiency reasons.  You can control how many buffers will be shown by
setting `buffers-menu-max-size'.  You can control the text of the menu
items by redefining the function `format-buffers-menu-line'."
  (let ((buffers (delete-if buffers-menu-omit-function (buffer-list))))
    (and (integerp buffers-menu-max-size)
	 (> buffers-menu-max-size 1)
	 (> (length buffers) buffers-menu-max-size)
	 ;; shorten list of buffers (not with submenus!)
	 (not (and buffers-menu-grouping-function
		   buffers-menu-submenus-for-groups-p))
	 (setcdr (nthcdr buffers-menu-max-size buffers) nil))
    (if buffers-menu-sort-function
	(setq buffers (sort buffers buffers-menu-sort-function)))
    (if (and buffers-menu-grouping-function
	     buffers-menu-submenus-for-groups-p
	     (or (not (integerp buffers-menu-submenus-for-groups-p))
		 (> (length buffers) buffers-menu-submenus-for-groups-p)))
	(let (groups groupnames current-group)
	  (mapl
	   #'(lambda (sublist)
	       (let ((groupname (funcall buffers-menu-grouping-function
					 (car sublist) (cadr sublist))))
		 (setq current-group (cons (car sublist) current-group))
		 (if groupname
		     (progn
		       (setq groups (cons (nreverse current-group)
					  groups))
		       (setq groupnames (cons groupname groupnames))
		       (setq current-group nil)))))
	   buffers)
	  (setq buffers
		(mapcar*
		 #'(lambda (groupname group)
		     (cons groupname (build-buffers-menu-internal group)))
		 (nreverse groupnames)
		 (nreverse groups))))
      (if buffers-menu-grouping-function
	  (progn
	    (setq buffers
		  (mapcon
		   #'(lambda (sublist)
		       (cond ((funcall buffers-menu-grouping-function
				       (car sublist) (cadr sublist))
			      (list (car sublist) t))
			     (t (list (car sublist)))))
		   buffers))
	    ;; remove a trailing separator.
	    (and (>= (length buffers) 2)
		 (let ((lastcdr (nthcdr (- (length buffers) 2) buffers)))
		   (if (eq t (cadr lastcdr))
		       (setcdr lastcdr nil))))))
      (setq buffers (build-buffers-menu-internal buffers)))
    (append menu buffers)
    ))

(defun language-environment-menu-filter (menu)
  "This is the menu filter for the \"Language Environment\" submenu."
  (mapcar (lambda (env-sym)
	    `[ ,(capitalize (symbol-name env-sym))
	       (set-language-environment ',env-sym)])
	  language-environment-list))


;;; The Options menu

(defvar options-save-faces nil
  "*Non-nil value means save-options will save information about faces.
A nil value means save-options will not save face information.
Set this non-nil only if you use M-x edit-faces to change face
settings.  If you use M-x customize-face or the \"Browse Faces...\"
menu entry, you will see a button in the Customize Face buffer that you
can use to permanently save your face changes.

M-x edit-faces is deprecated.  Support for it and this variable will
be discontinued in a future release.")

(defconst options-menu-saved-forms
  ;; This is really quite a kludge, but it gets the job done.
  ;;
  ;; remember that we have to conditionalize on default features
  ;; both in the forms to evaluate and in the forms output to
  ;; .emacs, in case the .emacs is loaded into an XEmacs with
  ;; different features.
  (purecopy
   '(
     ;; Editing Options menu.
     ;; put case-fold-search first to defeat a bug in the backquote
     ;; processing mechanism.  Feh!
     case-fold-search
     `(setq-default overwrite-mode ,(default-value 'overwrite-mode))
     (if (default-value 'overwrite-mode)
	 '(overwrite-mode 1))
     `(setq-default case-fold-search ,(default-value 'case-fold-search))
     case-replace
     (if (and (boundp 'pending-delete-mode)
	      pending-delete-mode)
	 '(pending-delete-mode 1))
     zmacs-regions
     mouse-yank-at-point
     require-final-newline
     next-line-add-newlines

     ;; General Options menu.
     teach-extended-commands-p
     ;; (#### not actually on Options menu)
     teach-extended-commands-timeout
     debug-on-error
     debug-on-quit

     ;; Printing Options menu.
     lpr-switches
     ps-print-color-p
     ps-paper-type

     ;; Other Window Location
     get-frame-for-buffer-default-instance-limit
     temp-buffer-show-function
     (if gnuserv-frame
	 '(setq gnuserv-frame (selected-frame)))

     ;; Syntax Highlighting
     font-lock-auto-fontify
     font-lock-use-fonts
     font-lock-use-colors
     font-lock-maximum-decoration
     font-lock-maximum-size
     ;; (#### the next two not on Options menu)
     font-lock-mode-enable-list
     font-lock-mode-disable-list
     ;; #### - this structure is clearly broken.  There's no way to ever
     ;; un-require font-lock via the menus.  --Stig
     (if (featurep 'font-lock)
	 '(require 'font-lock))
     (if (and (boundp 'font-lock-mode-hook)
	      (memq 'turn-on-fast-lock font-lock-mode-hook))
	 '(add-hook 'font-lock-mode-hook 'turn-on-fast-lock)
       '(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock))
     (if (and (boundp 'font-lock-mode-hook)
	      (memq 'turn-on-lazy-shot font-lock-mode-hook))
	 '(add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)
       '(remove-hook 'font-lock-mode-hook 'turn-on-lazy-shot))

     ;; Paren Highlighting
     (if paren-mode
	 `(progn (require 'paren) (paren-set-mode ',paren-mode)))

     ;; For specifiers, we only save global settings since the others
     ;; will belong to objects which only exist during this session.

     ;; Frame Appearance
     (if (featurep 'scrollbar)
	 `(if (featurep 'scrollbar)
	      (progn
		(add-spec-list-to-specifier
		 scrollbar-width
		 ',(specifier-spec-list scrollbar-width 'global))
		(add-spec-list-to-specifier
		 scrollbar-height
		 ',(specifier-spec-list scrollbar-height 'global)))))
     `(add-spec-list-to-specifier
       modeline-shadow-thickness
       ',(specifier-spec-list modeline-shadow-thickness 'global))
     `(setq-default truncate-lines ,(default-value 'truncate-lines))
     bar-cursor
     (if (and (boundp 'blink-cursor-mode) blink-cursor-mode)
	 '(blink-cursor-mode t))

     ;; Menubar Appearance
     buffers-menu-max-size
     complex-buffers-menu-p
     buffers-menu-sort-function
     buffers-menu-grouping-function
     buffers-menu-submenus-for-groups-p
     font-menu-ignore-scaled-fonts
     font-menu-this-frame-only-p

     ;; Toolbar Appearance
     (if (featurep 'toolbar)
	 `(if (featurep 'toolbar)
	      (progn
		(set-default-toolbar-position
		 ',(default-toolbar-position))
		(add-spec-list-to-specifier
		 default-toolbar-visible-p
		 ',(specifier-spec-list default-toolbar-visible-p 'global))
		(add-spec-list-to-specifier
		 toolbar-buttons-captioned-p
		 ',(specifier-spec-list toolbar-buttons-captioned-p
					'global)))))

     ;; mouse
     mouse-avoidance-mode

     ;; Open URLs With
     browse-url-browser-function

     ;; Now save all faces.

     ;; Setting this in lisp conflicts with X resources.  Bad move.  --Stig
     ;; (list 'set-face-font ''default (face-font-name 'default))
     ;; (list 'set-face-font ''modeline (face-font-name 'modeline))
     (if options-save-faces
	 (cons 'progn
	       (mapcar #'(lambda (face)
			   `(make-face ',face))
		       (save-options-non-customized-face-list))))

     (if options-save-faces
	 (cons 'progn
	       (apply 'nconc
		      (mapcar
		       #'(lambda (face)
			   (delq nil
				 (mapcar
				  #'(lambda (property)
				      (if (specifier-spec-list
					   (face-property face property))
					  `(add-spec-list-to-specifier
					    (face-property ',face ',property)
					    ',(save-options-specifier-spec-list
					       face property))))
				  (delq 'display-table
					(copy-sequence
					 built-in-face-specifiers)))))
		       (save-options-non-customized-face-list)))))

     ;; Mule-specific:
     (if (featurep 'mule)
	 `(if (featurep 'mule)
	      (set-language-environment ',current-language-environment)))
     ))
  "The variables to save; or forms to evaluate to get forms to write out.
This is used by `save-options-menu-settings' and should mirror the
options listed in the Options menu.")

(defun save-options-non-customized-face-list ()
  "Return a list of all faces that have not been 'customized'."
  (delq nil (mapcar '(lambda (face)
		       (unless (get face 'saved-face)
			 face))
		    (face-list))))

(defun save-options-specifier-spec-list (face property)
  (if (not (or (eq property 'font) (eq property 'color)))
      (specifier-spec-list (face-property face property) 'global)
    (let* ((retlist (specifier-spec-list (face-property face property)
					 'global))
	   (entry (cdr (car retlist)))
	   item)
      (while entry
	(setq item (car entry))
	(if (eq property 'font)
	    (if (font-instance-p (cdr item))
		(setcdr item (font-instance-name (cdr item))))
	  (if (color-instance-p (cdr item))
	      (setcdr item (color-instance-name (cdr item)))))
	(setq entry (cdr entry)))
      retlist)))

(defvar save-options-init-file nil
  "File into which to save forms to load the options file (nil for .emacs).
Normally this is nil, which means save into your .emacs file (the value
of `user-init-file'.")

(defvar save-options-file ".xemacs-options"
  "File to save options into.
This file is loaded from your .emacs file.
If this is a relative filename, it is put into the same directory as your
.emacs file.")

(defun save-options-menu-settings ()
  "Save the current settings of the `Options' menu to your `.emacs' file."
  (interactive)
  ;; we compute the actual filenames now because x-menubar is loaded
  ;; at dump time, when the identity of the user running XEmacs is not known.
  (let* ((actual-save-options-init-file
	  (or save-options-init-file
	      (and (not (equal user-init-file ""))
		   user-init-file)
	      (and (eq system-type 'ms-dos)
		   (concat "~" (user-login-name) "/_emacs"))
	      (concat "~" (user-login-name) "/.emacs")))
	 (actual-save-options-file
	  (abbreviate-file-name
	   (expand-file-name
	    save-options-file
	    (file-name-directory actual-save-options-init-file))
	   ;; Don't hack-homedir in abbreviate-file-name.  This will
	   ;; cause an incorrect expansion if the save-options variables
	   ;; have ~ in them.
	   ))
	 (init-output-buffer (find-file-noselect
			      actual-save-options-init-file))
	 init-output-marker
	 (options-output-buffer
	  (find-file-noselect actual-save-options-file))
	 options-output-marker)

    (save-excursion
      (set-buffer options-output-buffer)
      (erase-buffer)
      (setq options-output-marker (point-marker)))

    ;; run with current-buffer unchanged so that variables are evaluated in
    ;; the current context, instead of in the context of the ".emacs" buffer
    ;; or the ".xemacs-options" buffer.

    ;; first write out .xemacs-options.

    (let ((standard-output options-output-marker))
      (princ ";; -*- Mode: Emacs-Lisp -*-\n\n")
      (princ "(setq options-file-xemacs-version '(")
      (princ emacs-major-version)
      (princ " ")
      (princ emacs-minor-version)
      (princ "))\n")
      (let ((print-readably t)
	    (print-escape-newlines t))
	(mapcar #'(lambda (var)
		    (princ "  ")
		    (if (symbolp var)
			(prin1 (list 'setq-default var
				     (let ((val (symbol-value var)))
				       (if (or (memq val '(t nil))
					       (and (not (symbolp val))
						    (not (consp val))))
					   val
					 (list 'quote val)))))
		      (setq var (eval var))
		      (cond ((eq (car-safe var) 'progn)
			     (while (setq var (cdr var))
			       (prin1 (car var))
			       (princ "\n")
			       (if (cdr var) (princ "  "))
			       ))
			    (var
			     (prin1 var))))
		    (if var (princ "\n")))
		options-menu-saved-forms)
	))
    (set-marker options-output-marker nil)
    (save-excursion
      (set-buffer options-output-buffer)
      (save-buffer))

    ;; then fix .emacs.

    (save-excursion
      (set-buffer init-output-buffer)
      ;;
      ;; Find and delete the previously saved data, and position to write.
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^;; Options Menu Settings *\n" nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; End of Options Menu Settings *\\(\n\\|\\'\\)"
		 nil t)
		(error "can't find END of saved state in .emacs"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq init-output-marker (point-marker)))

    (let ((standard-output init-output-marker))
      (princ ";; Options Menu Settings\n")
      (princ ";; =====================\n")
      (princ "(cond\n")
      (princ " ((and (string-match \"XEmacs\" emacs-version)\n")
      (princ "       (boundp 'emacs-major-version)\n")
      (princ "       (or (and\n")
      (princ "            (= emacs-major-version 19)\n")
      (princ "            (>= emacs-minor-version 14))\n")
      (princ "           (= emacs-major-version 20))\n")
      (princ "       (fboundp 'load-options-file))\n")
      (princ "  (load-options-file \"")
      (princ actual-save-options-file)
      (princ "\")))\n")
      (princ ";; ============================\n")
      (princ ";; End of Options Menu Settings\n"))

    (set-marker init-output-marker nil)
    (save-excursion
      (set-buffer init-output-buffer)
      (save-buffer))
    ))


;;; The Help menu

(if (featurep 'mule)
    (defun tutorials-menu-filter (menu-items)
      ;; If there's a tutorial for the current language environment, make it
      ;; appear first as the default one. Otherwise, use the english one.
      (let* ((menu menu-items)
	     (item (pop menu-items)))
	(aset
	 item 3
	 (concat "("
		 (if (assoc
		      'tutorial
		      (assoc current-language-environment language-info-alist))
		     current-language-environment
		   "English")
		 ")"))
	menu)))


(set-menubar default-menubar)


;;; Popup menus.

(defconst default-popup-menu
  '("XEmacs Commands"
    ["Undo" advertised-undo
     :active (and (not (eq buffer-undo-list t))
		  (or buffer-undo-list pending-undo-list))
     :suffix (if (or (eq last-command 'undo)
		     (eq last-command 'advertised-undo))
		 "More" "")]
    ["Cut" x-kill-primary-selection
     :active (and (eq 'x (device-type (selected-device)))
		  (x-selection-owner-p))]
    ["Copy" x-copy-primary-selection
     :active (and (eq 'x (device-type (selected-device)))
		  (x-selection-owner-p))]
    ["Paste" x-yank-clipboard-selection
     :active (and (eq 'x (device-type (selected-device)))
		  (x-selection-exists-p 'CLIPBOARD))]
    ["Clear" x-delete-primary-selection
     :active (and (eq 'x (device-type (selected-device)))
		  (x-selection-owner-p))]
    "-----"
    ["Select Block" mark-paragraph]
    ["Split Window" (split-window)]
    ["Unsplit Window" delete-other-windows]
    ))

(defvar global-popup-menu nil
  "The global popup menu.  This is present in all modes.
See the function `popup-menu' for a description of menu syntax.")

(defvar mode-popup-menu nil
  "The mode-specific popup menu.  Automatically buffer local.
This is appended to the default items in `global-popup-menu'.
See the function `popup-menu' for a description of menu syntax.")
(make-variable-buffer-local 'mode-popup-menu)

;; In an effort to avoid massive menu clutter, this mostly worthless menu is
;; superceded by any local popup menu...
(setq-default mode-popup-menu default-popup-menu)

(defvar activate-popup-menu-hook nil
  "Function or functions run before a mode-specific popup menu is made visible.
These functions are called with no arguments, and should interrogate and
modify the value of `global-popup-menu' or `mode-popup-menu' as desired.
Note: this hook is only run if you use `popup-mode-menu' for activating the
global and mode-specific commands; if you have your own binding for button3,
this hook won't be run.")

(defun popup-mode-menu ()
  "Pop up a menu of global and mode-specific commands.
The menu is computed by combining `global-popup-menu' and `mode-popup-menu'."
  (interactive "@_")
  (run-hooks 'activate-popup-menu-hook)
  (popup-menu
   (cond ((and global-popup-menu mode-popup-menu)
	  (check-menu-syntax mode-popup-menu)
	  (let* ((title (car mode-popup-menu))
		 (items (cdr mode-popup-menu))
		 filters)
	    ;; Strip keywords from local menu for attaching them at the top
	    (while (and items
			(symbolp (car items)))
	      (setq items (append filters (list (car items))))
	      (setq items (cdr items)))
	    ;; If filters contains a keyword already present in
	    ;; `global-popup-menu' you will probably lose.
	    (append (list (car global-popup-menu))
		    filters
		    (cdr global-popup-menu)
		    '("---" "---")
		    (if popup-menu-titles (list title))
		    (if popup-menu-titles '("---" "---"))
		    items)))
	 (t
	  (or mode-popup-menu
	      global-popup-menu
	      (error "No menu here."))))))

(defun popup-buffer-menu (event)
  "Pop up a copy of the Buffers menu (from the menubar) where the mouse is clicked."
  (interactive "e")
  (let ((window (and (event-over-text-area-p event) (event-window event)))
	(bmenu nil))
    (or window
	(error "Pointer must be in a normal window"))
    (select-window window)
    (if current-menubar
	(setq bmenu (assoc "Buffers" current-menubar)))
    (if (null bmenu)
	(setq bmenu (assoc "Buffers" default-menubar)))
    (if (null bmenu)
	(error "Can't find the Buffers menu"))
    (popup-menu bmenu)))

(defun popup-menubar-menu (event)
  "Pop up a copy of menu that also appears in the menubar"
  ;; by Stig@hackvan.com
  (interactive "e")
  (let ((window (and (event-over-text-area-p event) (event-window event)))
	popup-menubar)
    (or window
	(error "Pointer must be in a normal window"))
    (select-window window)
    (and current-menubar (run-hooks 'activate-menubar-hook))
    ;; ##### Instead of having to copy this just to safely get rid of
    ;; any nil what we should really do is fix up the internal menubar
    ;; code to just ignore nil if generating a popup menu
    (setq popup-menubar (delete nil (copy-sequence (or current-menubar
						       default-menubar))))
    (popup-menu (cons "Menubar Menu" popup-menubar))
    ))

(global-set-key 'button3 'popup-mode-menu)
;; shift button3 and shift button2 are reserved for Hyperbole
(global-set-key '(meta control button3) 'popup-buffer-menu)
;; The following command is way too dangerous with Custom.
;; (global-set-key '(meta shift button3) 'popup-menubar-menu)

;; Here's a test of the cool new menu features (from Stig).

;;(setq mode-popup-menu
;;      '("Test Popup Menu"
;;        :filter cdr
;;        ["this item won't appear because of the menu filter" ding t]
;;        "--:singleLine"
;;        "singleLine"
;;        "--:doubleLine"
;;        "doubleLine"
;;        "--:singleDashedLine"
;;        "singleDashedLine"
;;        "--:doubleDashedLine"
;;        "doubleDashedLine"
;;        "--:noLine"
;;        "noLine"
;;        "--:shadowEtchedIn"
;;        "shadowEtchedIn"
;;        "--:shadowEtchedOut"
;;        "shadowEtchedOut"
;;        "--:shadowDoubleEtchedIn"
;;        "shadowDoubleEtchedIn"
;;        "--:shadowDoubleEtchedOut"
;;        "shadowDoubleEtchedOut"
;;        "--:shadowEtchedInDash"
;;        "shadowEtchedInDash"
;;        "--:shadowEtchedOutDash"
;;        "shadowEtchedOutDash"
;;        "--:shadowDoubleEtchedInDash"
;;        "shadowDoubleEtchedInDash"
;;        "--:shadowDoubleEtchedOutDash"
;;        "shadowDoubleEtchedOutDash"
;;        ))

(defun xemacs-splash-buffer ()
  "Redisplay XEmacs splash screen in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Splash*")))
    (set-buffer buffer)
    (erase-buffer buffer)
    (startup-splash-frame)
    (pop-to-buffer buffer)
    (delete-other-windows)))


(provide 'x-menubar)

;;; x-menubar.el ends here.
