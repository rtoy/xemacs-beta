;;; x-menubar.el --- Menubar and popup-menu support for X.

;; Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;;; Code:

;;; Warning-free compile
(eval-when-compile
  (defvar language-environment-list))

(defconst default-menubar
  (purecopy-menubar
   ;; note backquote.
   `(
     ("File"
      :filter file-menu-filter
      ["Open..."		find-file		t]
      ["Open in Other Window..." find-file-other-window	t]
      ["Open in New Frame..."	find-file-other-frame	t]
      ["Insert File..." 	insert-file		t]
      ["View File..."		view-file		t]
      "------"
      ["Save"			save-buffer		t  nil]
      ["Save As..."		write-file		t]
      ["Save Some Buffers"	save-some-buffers	t]
      "-----"
      ["Print Buffer"		lpr-buffer		t  nil]
      ["Pretty-Print Buffer"	ps-print-buffer-with-faces t  nil]
      "-----"
      ["New Frame"		make-frame		t]
      ["Frame on Other Display..."
				make-frame-on-display	t]
      ["Delete Frame"		delete-frame		t]
      "-----"
      ["Split Window"		split-window-vertically t]
      ["Un-Split (Keep This)"	delete-other-windows	(not (one-window-p t))]
      ["Un-Split (Keep Others)"	delete-window		(not (one-window-p t))]
      "-----"
      ["Revert Buffer"		revert-buffer		 t  nil]
      ["Delete Buffer"		kill-this-buffer	 t  nil]
      "-----"
      ["Exit XEmacs"		save-buffers-kill-emacs	t]
      )

     ("Edit"
      :filter edit-menu-filter
      ["Undo"			advertised-undo		   t]
      ["Cut"			x-kill-primary-selection   t]
      ["Copy"			x-copy-primary-selection   t]
      ["Paste"			x-yank-clipboard-selection t]
      ["Clear"			x-delete-primary-selection t]
      "----"
      ["Search..."		isearch-forward		t]
      ["Search Backward..."	isearch-backward	t]
      ["Replace..."		query-replace		t]
      "----"
      ["Search (Regexp)..."	isearch-forward-regexp	t]
      ["Search Backward (Regexp)..." isearch-backward-regexp t]
      ["Replace (Regexp)..."	query-replace-regexp	t]
      "----"
      ("Bookmarks"
       ["Jump to bookmark"  	bookmark-menu-jump	t]
       ["Set bookmark"  	bookmark-set		t]
       "---"
       ["Insert contents"  	bookmark-menu-insert	t]
       ["Insert location"  	bookmark-menu-locate	t]
       "---"
       ["Rename bookmark"  	bookmark-menu-rename	t]
       ["Delete bookmark"  	bookmark-menu-delete	t]
       ["Edit Bookmark List"    bookmark-bmenu-list	t]
       "---"
       ["Save bookmarks"        bookmark-save		t]
       ["Save bookmarks as..."  bookmark-write		t]
       ["Load a bookmark file"  bookmark-load		t])
      "----"
      ["Goto Line..."		goto-line		t]
      ["What Line"		what-line		t]
      "----"
      ["Start Macro Recording"	start-kbd-macro	      (not defining-kbd-macro)]
      ["End Macro Recording"	end-kbd-macro		defining-kbd-macro]
      ["Execute Last Macro"	call-last-kbd-macro	last-kbd-macro]
      )

     ("Apps"
      ["Read Mail (VM)..."	vm			t]
      ["Read Mail (MH)..."	(mh-rmail t)		t]
      ["Send mail..."		mail			t]
      ["Usenet News"		gnus			t]
      ["Browse the Web"		w3			t]
      ["Gopher"			gopher			t]
      ["Hyperbole..."		hyperbole		t]
      "----"
      ["Spell-Check Buffer"	ispell-buffer		t]
      ["Emulate VI"		viper-mode		t]
      "----"
      ("Calendar"
       ["3-Month Calendar"	calendar		t]
       ["Diary"			diary			t]
       ["Holidays"		holidays		t]
       ;; we're all pagans at heart ...
       ["Phases of the Moon"	phases-of-moon		t]
       ["Sunrise/Sunset"	sunrise-sunset		t]
       )
      ("Games"
       ["Quote from Zippy"	yow			t]
       ["Psychoanalyst"		doctor			t]
       ["Psychoanalyze Zippy!"	psychoanalyze-pinhead	t]
       ["Random Flames"		flame			t]
       ["Dunnet (Adventure)"	dunnet			t]
       ["Towers of Hanoi"	hanoi			t]
       ["Game of Life"		life			t]
       ["Multiplication Puzzle"	mpuz			t]
       ["Mine Game"		mine			t]
       )
      )

     ("Options"
      ["Read Only" (toggle-read-only)
       :style toggle :selected buffer-read-only]
      ("Editing Options"
       ["Overstrike" (progn
		       (overwrite-mode current-prefix-arg)
		       (setq-default overwrite-mode overwrite-mode))
	:style toggle :selected overwrite-mode]
       ["Case Sensitive Search" (progn
				  (setq case-fold-search (not case-fold-search))
				  (setq-default case-fold-search
						case-fold-search))
	:style toggle :selected (not case-fold-search)]
       ["Case Matching Replace" (setq case-replace (not case-replace))
	:style toggle :selected case-replace]
       ["Auto Delete Selection" (if (memq 'pending-delete-pre-hook
					  pre-command-hook)
				    (pending-delete-off nil)
				  (pending-delete-on nil))
	:style toggle
	:selected (memq 'pending-delete-pre-hook pre-command-hook)]
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
	t]
       ["Pretty-Print With Color"
	(setq ps-print-color-p (not ps-print-color-p))
	:style toggle :selected ps-print-color-p]
       ("Pretty-Print Paper Size"
	["Letter"
	 (setq ps-paper-type 'letter)
	 :style radio
	 :selected (eq ps-paper-type 'letter)]
	["Letter-small"
	 (setq ps-paper-type 'letter-small)
	 :style radio
	 :selected (eq ps-paper-type 'letter-small)]
	["Legal"
	 (setq ps-paper-type 'legal)
	 :style radio
	 :selected (eq ps-paper-type 'legal)]
	["Statement"
	 (setq ps-paper-type 'statement)
	 :style radio
	 :selected (eq ps-paper-type 'statement)]
	["Executive"
	 (setq ps-paper-type 'executive)
	 :style radio
	 :selected (eq ps-paper-type 'executive)]
	["Tabloid"
	 (setq ps-paper-type 'tabloid)
	 :style radio
	 :selected (eq ps-paper-type 'tabloid)]
	["Ledger"
	 (setq ps-paper-type 'ledger)
	 :style radio
	 :selected (eq ps-paper-type 'ledger)]
	["A3"
	 (setq ps-paper-type 'a3)
	 :style radio
	 :selected (eq ps-paper-type 'a3)]
	["A4"
	 (setq ps-paper-type 'a4)
	 :style radio
	 :selected (eq ps-paper-type 'a4)]
	["A4small"
	 (setq ps-paper-type 'a4small)
	 :style radio
	 :selected (eq ps-paper-type 'a4small)]
	["B4"
	 (setq ps-paper-type 'b4)
	 :style radio
	 :selected (eq ps-paper-type 'b4)]
	["B5"
	 (setq ps-paper-type 'b5)
	 :style radio
	 :selected (eq ps-paper-type 'b5)]
	)
       )
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
		  nil
		(selected-frame)))
	:style radio
	:selected (equal gnuserv-frame (selected-frame))]
       )

      "-----"
      ("Syntax Highlighting" 
       ["In This Buffer" (font-lock-mode)
	:style toggle :selected font-lock-mode]
       ["Automatic" (if (not (featurep 'font-lock))
			   (progn
			     (setq font-lock-auto-fontify t)
			     (require 'font-lock))
			 (setq font-lock-auto-fontify
			       (not font-lock-auto-fontify)))
	:style toggle
	:selected (and (featurep 'font-lock) font-lock-auto-fontify)]
       "-----"
       ["Fonts" (progn (require 'font-lock)
		       (font-lock-use-default-fonts)
		       (setq font-lock-use-fonts t
			     font-lock-use-colors nil)
		       (font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       font-lock-use-fonts)]
       ["Colors" (progn (require 'font-lock)
			(font-lock-use-default-colors)
			(setq font-lock-use-colors t 
			      font-lock-use-fonts nil)
			(font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       font-lock-use-colors)]
       "-----"
       ["Least" (if (or (and (not (integerp font-lock-maximum-decoration))
			     (not (eq t font-lock-maximum-decoration)))
			(and (integerp font-lock-maximum-decoration)
			     (<= font-lock-maximum-decoration 0)))
		    nil
		  (setq font-lock-maximum-decoration nil)
		  (font-lock-recompute-variables))
	:style radio
	:active font-lock-mode
	:selected (and font-lock-mode
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
	:active font-lock-mode
	:selected (and font-lock-mode
		       (integerp font-lock-maximum-decoration)
		       (= 1 font-lock-maximum-decoration))]
       ["Even More" (if (and (integerp font-lock-maximum-decoration)
			     (= 2 font-lock-maximum-decoration))
			nil
		      (setq font-lock-maximum-decoration 2)
		      (font-lock-recompute-variables))
	:style radio
	:active font-lock-mode
	:selected (and font-lock-mode
		       (integerp font-lock-maximum-decoration)
		       (= 2 font-lock-maximum-decoration))]
       ["Most" (if (or (eq font-lock-maximum-decoration t)
		       (and (integerp font-lock-maximum-decoration)
			    (>= font-lock-maximum-decoration 3)))
		   nil
		 (setq font-lock-maximum-decoration t)
		 (font-lock-recompute-variables))
	:style radio
	:active font-lock-mode
	:selected (and font-lock-mode
		       (or (eq font-lock-maximum-decoration t)
			   (and (integerp font-lock-maximum-decoration)
				(>= font-lock-maximum-decoration 3))))]
       "-----"
       ["Lazy" (progn (require 'lazy-lock)
		      (if (and (boundp 'lazy-lock-mode) lazy-lock-mode)
			  (progn
			    (lazy-lock-mode 0)
			    ;; this shouldn't be necessary so there has to
			    ;; be a redisplay bug lurking somewhere (or
			    ;; possibly another event handler bug)
			    (redraw-modeline)
			    (remove-hook 'font-lock-mode-hook
					 'turn-on-lazy-lock))
			(if font-lock-mode
			    (progn
			      (lazy-lock-mode 1)
			      (redraw-modeline)
			      (add-hook 'font-lock-mode-hook
					'turn-on-lazy-lock)))))
	:active font-lock-mode
	:style toggle
	:selected (and (boundp 'lazy-lock-mode) lazy-lock-mode)]
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
	:active font-lock-mode
	:style toggle
	:selected fast-lock-mode]
       )
      ("Paren Highlighting"
       ["None" (paren-set-mode -1)
	:style radio :selected (not paren-mode)]
       ["Blinking Paren" (paren-set-mode 'blink-paren)
	:style radio :selected (eq paren-mode 'blink-paren)]
       ["Steady Paren" (paren-set-mode 'paren)
	:style radio :selected (eq paren-mode 'paren)]
       ["Expression" (paren-set-mode 'sexp)
	:style radio :selected (eq paren-mode 'sexp)]
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
	:style toggle :selected font-menu-this-frame-only-p]
;     ["Line Numbers" (line-number-mode nil)
;      :style toggle :selected line-number-mode]
      )
      ("Menubar Appearance"
       ["Buffers Menu Length..."
	(progn
	  (setq buffers-menu-max-size
		(read-number
		 "Enter number of buffers to display (or 0 for unlimited): "))
	  (if (eq buffers-menu-max-size 0) (setq buffers-menu-max-size nil)))
	t]
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
	:style toggle :selected font-menu-ignore-scaled-fonts]
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
	:style toggle :selected (and mouse-avoidance-mode window-system)])
      ("Open URLs With"
       ["Emacs-W3" (setq browse-url-browser-function 'browse-url-w3)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-w3)]
       ["Netscape" (setq browse-url-browser-function 'browse-url-netscape)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-netscape)]
       ["Mosaic" (setq browse-url-browser-function 'browse-url-mosaic)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-mosaic)]
       ["Mosaic (CCI)" (setq browse-url-browser-function 'browse-url-cci)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-iximosaic)]
       ["IXI Mosaic" (setq browse-url-browser-function 'browse-url-iximosaic)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-iximosaic)]
       ["Lynx (xterm)" (setq browse-url-browser-function 'browse-url-lynx-xterm)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-lynx-xterm)]
       ["Lynx (xemacs)" (setq browse-url-browser-function 'browse-url-lynx-emacs)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-lynx-emacs)]
       ["Grail" (setq browse-url-browser-function 'browse-url-grail)
	:style radio
	:selected (eq browse-url-browser-function 'browse-url-grail)]
      )
      "-----"
      ["Edit Faces..." edit-faces t]
      ("Font"   :filter font-menu-family-constructor)
      ("Size"	:filter font-menu-size-constructor)
      ("Weight"	:filter font-menu-weight-constructor)
      ,@(if (featurep 'mule)
	    '("-----"
	      ("Language Environment"
	       :filter language-environment-menu-filter)))
      "-----"
      ["Save Options" save-options-menu-settings t]
      )
     
     ("Buffers"
      :filter buffers-menu-filter
      ["List All Buffers" list-buffers t]
      "--"
      )
     
     ("Tools"
      ["Grep..."		grep			t]
      ["Compile..."		compile			t]
      ["Shell"			shell			t]
      ["Shell Command..."	shell-command		t]
      ["Shell Command on Region..." shell-command-on-region (region-exists-p)]
      ["Debug (GDB)..."		gdb			t]
      ["Debug (DBX)..."		dbx			t]
      "-----"
      ["OO-Browser..."		oobr			t]
      ("Tags"
       ["Find Tag..."		find-tag		t]
       ["Find Other Window..."	find-tag-other-window	t]
       ["Next Tag..."		(find-tag nil)		t]
       ["Next Other Window..."	(find-tag-other-window nil) t]
       ["Next File"		next-file		t]
       "-----"
       ["Tags Search..."	tags-search		t]
       ["Tags Replace..."	tags-query-replace	t]
       ["Continue Search/Replace" tags-loop-continue	t]
       "-----"
       ["Pop stack"		pop-tag-mark		t]
       ["Apropos..."		tags-apropos		t]
       "-----"
       ["Set Tags Table File..." visit-tags-table	t]
       ))

     nil		; the partition: menus after this are flushright

     ("Help"
      ["About XEmacs..."	about-xemacs		t]
      ("Basics"
       ["Tutorial"		help-with-tutorial	t]
       ["News"			view-emacs-news		t]
       ["Packages"		finder-by-keyword	t]
       ["Splash"		xemacs-splash-buffer	t])
      "-----"
      ("XEmacs FAQ"
       ["FAQ"			xemacs-local-faq	t]
       ["FAQ via WWW" 		xemacs-www-faq	t]
       ["Home Page"		xemacs-www-page		t])
      ("Samples"
       ["Sample"			(find-file
					 (expand-file-name "sample.emacs"
							   data-directory))
	t ".emacs"]
       ["Sample"			(find-file
					 (expand-file-name "sample.Xdefaults"
							   data-directory))
	t ".Xdefaults"]
       ["Sample"			(find-file
					 (expand-file-name "enriched.doc"
							   data-directory))
	t "enriched"])
      "-----"
      ("Lookup in Info"
       ["Key Binding..."	Info-goto-emacs-key-command-node t]
       ["Command..."		Info-goto-emacs-command-node t]
       ["Function..."		Info-elisp-ref		t]
       ["Topic..."		Info-query		t])
      ("Manuals"
       ["Info"			info			t]
       ["Unix Manual..."	manual-entry		t])
      ("Commands & Keys"
       ["Mode"			describe-mode		t]
       ["Apropos..."		hyper-apropos		t]
       ["Apropos Docs..."	apropos-documentation	t]
       "-----"
       ["Key..."		describe-key		t]
       ["Bindings"		describe-bindings	t]
       ["Mouse Bindings"	describe-pointer	t]
       ["Recent Keys"		view-lossage		t]
       "-----"
       ["Function..."		describe-function	t]
       ["Variable..."		describe-variable	t]
       ["Locate Command..."	where-is		t])
      "-----"
      ["Recent Messages"	view-lossage		t]
      ("Misc"
       ["No Warranty"		describe-no-warranty	t]
       ["XEmacs License"	describe-copying	t]
       ["The Latest Version"	describe-distribution	t])
      ,custom-help-menu
      )
     )))


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
		      t]
		     "Help"))
   (t nil)))

(add-hook 'before-init-hook 'maybe-add-init-button)


;;; The File and Edit menus

(defvar put-buffer-names-in-file-menu t)

;; The sensitivity part of this function could be done by just adding forms
;; to evaluate to the menu items themselves; that would be marginally less
;; efficient but not perceptibly so (I think).  But in order to change the
;; names of the Undo menu item and the various things on the File menu item,
;; we need to use a hook.

(defun file-menu-filter (menu-items)
  "Incrementally update the file menu.
This function changes the arguments and sensitivity of these File menu items:

  Delete Buffer  has the name of the current buffer appended to it.
  Print Buffer   has the name of the current buffer appended to it.
  Pretty-Print Buffer
		 has the name of the current buffer appended to it.
  Save           has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer is modified.
  Revert Buffer  has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer has a file.
  Delete Frame   sensitive only when there is more than one frame.

The name of the current buffer is only appended to the menu items if
`put-buffer-names-in-file-menu' is non-nil.  This behavior is the default."
  (let* ((bufname (buffer-name))
	 (result menu-items)		; save pointer to start of menu.
	 name
	 item)
    ;; the contents of the menu items in the file menu are destructively
    ;; modified so that there is as little consing as possible.  This is okay.
    ;; As soon as the result is returned, it is converted to widget_values
    ;; inside lwlib and the lisp menu-items can be safely modified again. 
    (while (setq item (pop menu-items))
      (if (vectorp item)
	  (progn
	    (setq name (aref item 0))
	    (and put-buffer-names-in-file-menu
		 (member name '("Save" "Revert Buffer" "Print Buffer"
				"Pretty-Print Buffer" "Delete Buffer"))
		 (>= 4 (length item))
		 (aset item 3 bufname))
	    (and (string= "Save" name)
		 (aset item 2 (buffer-modified-p)))
	    (and (string= "Revert Buffer" name)
		 (aset item 2 (not (not (or buffer-file-name
					    revert-buffer-function)))))
	    (and (string= "Delete Frame" name)
		 (aset item 2 (not (eq (next-frame) (selected-frame)))))
	    )))
    result))

(defun edit-menu-filter (menu-items)
  "For use as an incremental menu construction filter.
This function changes the sensitivity of these Edit menu items:

  Cut    sensitive only when emacs owns the primary X Selection.
  Copy   sensitive only when emacs owns the primary X Selection.
  Clear  sensitive only when emacs owns the primary X Selection.
  Paste  sensitive only when there is an owner for the X Clipboard Selection.
  Undo   sensitive only when there is undo information.
         While in the midst of an undo, this is changed to \"Undo More\"."
  (let* (item
	name
	(result menu-items)		; save pointer to head of list
	(x-dev (eq 'x (device-type (selected-device))))
	(emacs-owns-selection-p (and x-dev (x-selection-owner-p)))
	(clipboard-exists-p (and x-dev (x-selection-exists-p 'CLIPBOARD)))
;;;       undo-available undoing-more
;;;       (undo-info-available (not (null (and (not (eq t buffer-undo-list))
;;;                                 (if (eq last-command 'undo)
;;;                                     (setq undoing-more
;;;                                           (and (boundp 'pending-undo-list)
;;;                                          pending-undo-list)
;;;                                   buffer-undo-list))))))
	undo-name undo-state
	)
    ;; As with file-menu-filter, menu-items are destructively modified.
    ;; This is OK.
    (while (setq item (pop menu-items))
      (if (vectorp item)
	  (progn
	    (setq name (aref item 0))
	    (and (member name '("Cut" "Copy" "Clear"))
		 (aset item 2 emacs-owns-selection-p))
	    (and (string= name "Paste")
		 (aset item 2 clipboard-exists-p))
	    (and (member name '("Undo" "Undo More"))
		 (progn
		   ;; we could also do this with the third field of the item.
		   (if (eq last-command 'undo)
		       (setq undo-name "Undo More"
			     undo-state (not (null (and (boundp 'pending-undo-list)
							pending-undo-list))))
		     (setq undo-name "Undo"
			   undo-state (and (not (eq buffer-undo-list t))
					   (not (null
						 (or buffer-undo-list
						     (and (boundp 'pending-undo-list)
							  pending-undo-list)))))))
		   (if buffer-read-only (setq undo-state nil))
		   (aset item 0 undo-name)
		   (aset item 2 undo-state)
		   ))
      )))
    result))


;;; The Buffers menu

(defvar buffers-menu-max-size 25
  "*Maximum number of entries which may appear on the \"Buffers\" menu.
If this is 10, then only the ten most-recently-selected buffers will be
shown.  If this is nil, then all buffers will be shown.  Setting this to
a large number or nil will slow down menu responsiveness.")

(defvar complex-buffers-menu-p nil
  "*If true, the buffers menu will contain several commands, as submenus
of each buffer line.  If this is false, then there will be only one command:
select that buffer.")

(defvar buffers-menu-submenus-for-groups-p nil
  "*If true, the buffers menu will contain one submenu per group of buffers,
if a grouping function is specified in `buffers-menu-grouping-function'.
If this is an integer, do not build submenus if the number of buffers
is not larger than this value.")

(defvar buffers-menu-switch-to-buffer-function 'switch-to-buffer
  "*The function to call to select a buffer from the buffers menu.
`switch-to-buffer' is a good choice, as is `pop-to-buffer'.")

(defvar buffers-menu-omit-function 'buffers-menu-omit-invisible-buffers
"*If non-nil, a function specifying the buffers to omit from the buffers menu.
This is passed a buffer and should return non-nil if the buffer should be
omitted.  The default value `buffers-menu-omit-invisible-buffers' omits
buffers that are normally considered \"invisible\" (those whose name
begins with a space).")

(defvar buffers-menu-format-buffer-line-function 'format-buffers-menu-line
  "*The function to call to return a string to represent a buffer in the
buffers menu.  The function is passed a buffer and should return a string.
The default value `format-buffers-menu-line' just returns the name of
the buffer.  Also check out `slow-format-buffers-menu-line' which
returns a whole bunch of info about a buffer.")

(defvar buffers-menu-sort-function
  'sort-buffers-menu-by-mode-then-alphabetically
  "*If non-nil, a function to sort the list of buffers in the buffers menu.
It will be passed two arguments (two buffers to compare) and should return
T if the first is \"less\" than the second.  One possible value is
`sort-buffers-menu-alphabetically'; another is
`sort-buffers-menu-by-mode-then-alphabetically'.")

(defvar buffers-menu-grouping-function
  'group-buffers-menu-by-mode-then-alphabetically
  "*If non-nil, a function to group buffers in the buffers menu together.
It will be passed two arguments, successive members of the sorted buffers
list after being passed through `buffers-menu-sort-function'.  It should
return non-nil if the second buffer begins a new group.  The return value
should be the name of the old group, which may be used in hierarchical
buffers menus.  The last invocation of the function contains nil as the
second argument, so that the name of the last group can be determined.

The sensible values of this function are dependent on the value specified
for `buffers-menu-sort-function'.")

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
	       (set-language-environment ',env-sym) t])
	  language-environment-list))


;;; The Options menu

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
     (if (memq 'pending-delete-pre-hook pre-command-hook)
	 '(progn
	    (require 'pending-del)
	    (pending-delete-on nil)))
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
	      (memq 'turn-on-lazy-lock font-lock-mode-hook))
	 '(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
       '(remove-hook 'font-lock-mode-hook 'turn-on-lazy-lock))

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

     (cons 'progn
	   (mapcar #'(lambda (face)
		       `(make-face ',face))
		   (face-list)))

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
		   (face-list))))

     ;; Mule-specific:
     (if (featurep 'mule)
	 `(if (featurep 'mule)
	      (set-language-environment ',(current-language-environment))))
     ))
  "The variables to save; or forms to evaluate to get forms to write out.
This is used by `save-options-menu-settings' and should mirror the
options listed in the Options menu.")

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
  "Saves the current settings of the `Options' menu to your `.emacs' file."
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


(set-menubar default-menubar)


;;; Popup menus.

(defconst default-popup-menu
  '("XEmacs Commands"
    :filter edit-menu-filter
    ["Undo"		advertised-undo		t]
    ["Cut"		x-kill-primary-selection   t]
    ["Copy"		x-copy-primary-selection   t]
    ["Paste"		x-yank-clipboard-selection t]
    ["Clear"            x-delete-primary-selection t]
    "-----"
    ["Select Block"	mark-paragraph 		t]
    ["Split Window"	(split-window)		t]
    ["Unsplit Window" 	delete-other-windows	t]
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
	  (let ((title (car mode-popup-menu))
		(items (cdr mode-popup-menu)))
	    (append global-popup-menu
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
(global-set-key '(meta shift button3) 'popup-menubar-menu)

;; Here's a test of the cool new menu features (from Stig).

;(setq mode-popup-menu
;      '("Test Popup Menu"
;        :filter cdr
;        ["this item won't appear because of the menu filter" ding t]
;        "--:singleLine"
;        "singleLine"
;        "--:doubleLine"
;        "doubleLine"
;        "--:singleDashedLine"
;        "singleDashedLine"
;        "--:doubleDashedLine"
;        "doubleDashedLine"
;        "--:noLine"
;        "noLine"
;        "--:shadowEtchedIn"
;        "shadowEtchedIn"
;        "--:shadowEtchedOut"
;        "shadowEtchedOut"
;        "--:shadowDoubleEtchedIn"
;        "shadowDoubleEtchedIn"
;        "--:shadowDoubleEtchedOut"
;        "shadowDoubleEtchedOut"
;        "--:shadowEtchedInDash"
;        "shadowEtchedInDash"
;        "--:shadowEtchedOutDash"
;        "shadowEtchedOutDash"
;        "--:shadowDoubleEtchedInDash"
;        "shadowDoubleEtchedInDash"
;        "--:shadowDoubleEtchedOutDash"
;        "shadowDoubleEtchedOutDash"
;        ))

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
