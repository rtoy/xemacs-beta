;;; menubar-items.el --- Menubar and popup-menu content for XEmacs.

;; Copyright (C) 1991-1995, 1997-1998 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003 Ben Wing.
;; Copyright (C) 1997 MORIOKA Tomohiko.

;; Maintainer: XEmacs Development Team
;; Keywords: frames, extensions, internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Authorship:

;; Created c. 1991 for Lucid Emacs.  Originally called x-menubar.el.
;;   Contained four menus -- File, Edit, Buffers, Help.
;;   Dynamic menu changes possible only through activate-menubar-hook.
;;   Also contained menu manipulation funs, e.g. find-menu-item, add-menu.
;; Options menu added for 19.9 by Jamie Zawinski, late 1993.
;; Major reorganization c. 1994 by Ben Wing; added many items and moved
;;   some items to two new menus, Apps and Tools. (for 19.10?)
;; Generic menubar functions moved to new file, menubar.el, by Ben Wing,
;;   1995, for 19.12; also, creation of current buffers menu options,
;;   and buffers menu changed from purely most-recent to sorted alphabetical,
;;   by mode.  Also added mode-popup-menu support.
;; New API (add-submenu, add-menu-button) and menu filter support added
;;   late summer 1995 by Stig, for 19.13.  Also popup-menubar-menu.
;; Renamed to menubar-items.el c. 1998, with MS Win support.
;; Options menu rewritten to use custom c. 1999 by ? (Jan Vroonhof?).
;; Major reorganization Mar. 2000 by Ben Wing; added many items and changed
;;   top-level menus to File, Edit, View, Cmds, Tools, Options, Buffers.
;; Accelerator spec functionality added Mar. 2000 by Ben Wing.

;;; Commentary:

;; This file is dumped with XEmacs (when window system and menubar support is
;; compiled in).

;;; Code:

(defun Menubar-items-truncate-history (list count label-length)
  "Truncate a history LIST to first COUNT items.
Return a list of (label value) lists with labels truncated to last
LABEL-LENGTH characters of value."
  (mapcar #'(lambda (x)
	      (if (<= (length x) label-length)
                  (list x x)
                (list
                 (concat "..." (substring x (- label-length))) x)))
	  (if (<= (length list) count)
	      list
	    (butlast list (- (length list) count)))))


(defun coding-system-menu-filter (fun active &optional dots)
  "Filter for menu entries with a submenu listing all coding systems.
This is for operations that take a coding system as an argument.  FUN
should be a function of one argument, which will be a coding system symbol.
ACTIVE should be a function of one argument (again, a coding system symbol),
returning a boolean describing whether the entry is active.  If DOTS is given,
the menu entries will have three dots appended.

Write your filter like this:

:filter
  (lambda (menu)
    (lambda (entry) ...)
    (lambda (entry) ...))
"
  (menu-split-long-menu-and-sort
   (mapcar
    #'(lambda (_csmf_entry)
	`[ ,(concat (coding-system-description _csmf_entry)
		    (if dots "..." ""))
	   (funcall ,fun ',_csmf_entry)
	   :active (funcall ,active ',_csmf_entry)
	   ])
    (delete-if
     #'(lambda (name)
	 (or (coding-system-alias-p name)
	     (not (eq name (coding-system-name
			    (coding-system-base name))))))
     (coding-system-list)))))

(defconst default-menubar
  ;; This is backquoted; a lambda with a preceding , will be byte-compiled.
   `(("%_File"
      ["%_Open..." find-file]
      ["Open in Other %_Window..." find-file-other-window]
      ["Open in New %_Frame..." find-file-other-frame]
      ("Open with Specified %_Encoding"
       :filter
       ,#'(lambda (menu)
            (coding-system-menu-filter
             (lambda (entry)
               (let ((coding-system-for-read entry))
                 (call-interactively 'find-file)))
             (lambda (entry) t)
             t)))
      ["%_Hex Edit File..." hexl-find-file
       :active (fboundp 'hexl-find-file)]
      ["%_Insert File..." insert-file]
      ["%_View File..." view-file]
      "------"
      ["%_New Frame" make-frame]
      ["Frame on Other %_Display..." make-frame-on-display
       :active (fboundp 'make-frame-on-display)]
      ["%_Close Frame" delete-frame
       :active (not (eq (next-frame (selected-frame) 'nomini 'window-system)
			(selected-frame)))]
      "-----"
      ["%_Save" save-buffer
       :active (buffer-modified-p)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      ["Save %_As..." write-file]
      ["Save So%_me Buffers" save-some-buffers]
      "-----"
      ,@(if (valid-specifier-tag-p 'msprinter)
	    '(["Page Set%_up..." generic-page-setup]))
      ["%_Print" generic-print-buffer
       :active (or (valid-specifier-tag-p 'msprinter)
		   (and (not (eq system-type 'windows-nt))
			(fboundp 'lpr-region)))
       :suffix (if (region-active-p) "Selection..."
		 (if put-buffer-names-in-file-menu (concat (buffer-name) "...")
		   "..."))]
      ,@(unless (valid-specifier-tag-p 'msprinter)
	  '(["Prett%_y-Print" ps-print-buffer-with-faces
	     :active (fboundp 'ps-print-buffer-with-faces)
	     :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]))
      "-----"
      ["%_Revert Buffer" revert-buffer
       :active (or buffer-file-name revert-buffer-function)
       :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
      ("Rever%_t/Recover"
       ("Revert Buffer with Specified %_Encoding"
	:filter
	,#'(lambda (menu)
	     (coding-system-menu-filter
	      (lambda (entry)
		(let ((coding-system-for-read entry))
		  (revert-buffer)))
	      (lambda (entry) (or buffer-file-name revert-buffer-function))
	      t)))
       ["Re%_cover Buffer from Autosave" (recover-file buffer-file-name)
	:active buffer-file-name
	:suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
       ["Recover %_Session..." recover-session]
       )
      "-----"
      ["E%_xit XEmacs" save-buffers-kill-emacs]
      )

     ("%_Edit"
     ["%_Undo" undo
      :active (and (not (eq buffer-undo-list t))
		   (or buffer-undo-list pending-undo-list))
      :suffix (if (eq last-command 'undo) "More" "")]
     ["%_Redo" redo
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
     "----"
     ["Cu%_t" kill-primary-selection
      :active (selection-owner-p)]
     ["%_Copy" copy-primary-selection
      :active (selection-owner-p)]
     ["%_Paste" yank-clipboard-selection
      :active (selection-exists-p 'CLIPBOARD)]
     ["%_Delete" delete-primary-selection
      :active (selection-owner-p)]
     "----"
     ["Select %_All" mark-whole-buffer]
     ["Select Pa%_ge" mark-page]
     ["Select Paragrap%_h" mark-paragraph]
     ["Re%_select Region" activate-region :active (mark t)]
     "----"
     ["%_Find..." make-search-dialog]
     ["R%_eplace..." query-replace]
     ["Replace (Rege%_xp)..." query-replace-regexp]
     ["List %_Matching Lines..." list-matching-lines]
     )

     ("%_View"
     ["%_Split Window" split-window-vertically]
     ["S%_plit Window (Side by Side)" split-window-horizontally]
     ["%_Un-Split (Keep This)" delete-other-windows
      :active (not (one-window-p t))]
     ["Un-Split (Keep %_Others)" delete-window
      :active (not (one-window-p t))]
     ["Balance %_Windows" balance-windows
      :active (not (one-window-p t))]
     ["Shrink Window to %_Fit" shrink-window-if-larger-than-buffer]
     "----"
     ("N%_arrow"
      ["%_Narrow to Region" narrow-to-region :active (region-exists-p)]
      ["Narrow to %_Page" narrow-to-page]
      ["Narrow to %_Defun" narrow-to-defun]
      "----"
      ["%_Widen" widen :active (or (/= (point-min) 1)
				   (/= (point-max) (1+ (buffer-size))))]
      )
     "----"
     ["%_Goto Line..." goto-line]
     ["Beginning of %_Defun" beginning-of-defun]
     ["%_End of Defun" end-of-defun]
     ["%_Count Lines in Buffer" count-lines-buffer
      :included (not (region-active-p))]
     ["%_Count Lines in Region" count-lines-region
      :included (region-active-p)]
     "----"
     ["%_Jump to Previous Mark" (set-mark-command t)
      :active (mark t)]
     ["Se%_t Bookmark" bookmark-set
      :active (fboundp 'bookmark-set)]
     ("%_Bookmarks"
      :filter
      ,#'(lambda (menu)
	   (let ((alist (and-boundp 'bookmark-alist
			  bookmark-alist)))
	     (if (not alist)
		 menu
	       (let ((items
		      (submenu-generate-accelerator-spec
		       (mapcar #'(lambda (bmk)
				   `[,bmk (bookmark-jump ',bmk)])
			       (declare-fboundp (bookmark-all-names))))))
		 (append menu '("---") items)))))
      "---"
      ["Insert %_Contents" bookmark-menu-insert
       :active (fboundp 'bookmark-menu-insert)]
      ["Insert L%_ocation" bookmark-menu-locate
       :active (fboundp 'bookmark-menu-locate)]
      "---"
      ["%_Rename Bookmark" bookmark-menu-rename
       :active (fboundp 'bookmark-menu-rename)]
      ("%_Delete Bookmark"
       :filter ,#'(lambda (menu)
		    (submenu-generate-accelerator-spec
		     (mapcar #'(lambda (bmk)
				 `[,bmk (bookmark-delete ',bmk)])
			     (declare-fboundp (bookmark-all-names))))))
      ["%_Edit Bookmark List" bookmark-bmenu-list
       :active (and-boundp 'bookmark-alist bookmark-alist)]
      "---"
      ["%_Save Bookmarks"        bookmark-save
       :active (and-boundp 'bookmark-alist bookmark-alist)]
      ["Save Bookmarks %_As..."  bookmark-write
       :active (and-boundp 'bookmark-alist bookmark-alist)]
      ["%_Load a Bookmark File" bookmark-load
       :active (fboundp 'bookmark-load)]
      )
     )

     ("C%_mds"
     ["Repeat Last Comple%_x Command..." repeat-complex-command]
     ["E%_valuate Lisp Expression..." eval-expression]
     ["Execute %_Named Command..." execute-extended-command]
     "----"
     ["Start %_Defining Macro" start-kbd-macro
      :included (not defining-kbd-macro)]
     ["Stop %_Defining Macro" end-kbd-macro
      :included defining-kbd-macro]
     ["%_Execute Last Macro" call-last-kbd-macro
      :active last-kbd-macro]
     ("Other %_Macro"
      ["Edit %_Last Macro" edit-last-kbd-macro
       :active last-kbd-macro]
      ["%_Edit Macro..." edit-kbd-macro]
      ["%_Append to Last Macro" (start-kbd-macro t)
       :active (and (not defining-kbd-macro) last-kbd-macro)]
      "---"
      ["%_Name Last Macro..." name-last-kbd-macro
       :active last-kbd-macro]
      ["Assign Last Macro to %_Key..." assign-last-kbd-macro-to-key
       :active (and last-kbd-macro
		    (fboundp 'assign-last-kbd-macro-to-key))]
      "---"
      ["E%_xecute Last Macro on Region Lines"
       :active (and last-kbd-macro (region-exists-p))]
      "---"
      ["%_Query User During Macro" kbd-macro-query
       :active defining-kbd-macro]
      ["Enter %_Recursive Edit During Macro" (kbd-macro-query t)
       :active defining-kbd-macro]
      "---"
      ["%_Insert Named Macro into Buffer..." insert-kbd-macro]
      ["Read Macro from Re%_gion" read-kbd-macro
       :active (region-exists-p)]
      )
     "----"
     ["D%_ynamic Abbrev Expand" dabbrev-expand]
     ["Define %_Global Abbrev for " add-global-abbrev
      :suffix	(truncate-string-to-width (abbrev-string-to-be-defined nil)
					  40 nil nil t)]
     ("Other %_Abbrev"
      ["Dynamic Abbrev %_Complete" dabbrev-completion]
      ["Dynamic Abbrev Complete in %_All Buffers" (dabbrev-completion 16)]
      "----"
      "----"
      ["%_Define Global Abbrev for " add-global-abbrev
       :suffix	(truncate-string-to-width (abbrev-string-to-be-defined nil)
					  40 nil nil t)]
      ["Define %_Mode-Specific Abbrev for " add-mode-abbrev
       :suffix	(truncate-string-to-width (abbrev-string-to-be-defined nil)
					  40 nil nil t)]
      ["Define Global Ex%_pansion for " inverse-add-global-abbrev
       :suffix	(truncate-string-to-width
		 (inverse-abbrev-string-to-be-defined 1)
		 40 nil nil t)]
      ["Define Mode-Specific Expa%_nsion for " inverse-add-mode-abbrev
       :suffix	(truncate-string-to-width
		 (inverse-abbrev-string-to-be-defined 1)
		 40 nil nil t)]
      "---"
      ["E%_xpand Abbrev" expand-abbrev]
      ["Expand Abbrevs in Re%_gion" expand-region-abbrevs
       :active (region-exists-p)]
      ["%_Unexpand Last Abbrev" unexpand-abbrev
       :active (and (stringp last-abbrev-text)
		    (> last-abbrev-location 0))]
      "---"
      ["%_Kill All Abbrevs" kill-all-abbrevs]
      ["%_Insert All Abbrevs into Buffer" insert-abbrevs]
      ["%_List Abbrevs" list-abbrevs]
      "---"
      ["%_Edit Abbrevs" edit-abbrevs]
      ["%_Redefine Abbrevs from Buffer" edit-abbrevs-redefine
       :active (eq major-mode 'edit-abbrevs-mode)]
      "---"
      ["%_Save Abbrevs As..." write-abbrev-file]
      ["L%_oad Abbrevs..." read-abbrev-file]
      )
     "---"
     ["%_Cut Rectangle" kill-rectangle]
     ["%_Prefix Rectangle..." string-rectangle]
     ("Other %_Rectangles/Register"
      ["%_Yank Rectangle" yank-rectangle]
      ["Rectangle %_to Register" copy-rectangle-to-register]
      ["Rectangle %_from Register" insert-register]
      ["%_Delete Rectangle" clear-rectangle]
      ["%_Open Rectangle" open-rectangle]
      ["Rectangle %_Mousing"
       (customize-set-variable	'mouse-track-rectangle-p
				(not mouse-track-rectangle-p))
       :style toggle :selected mouse-track-rectangle-p]
      "---"
      ["%_Copy to Register..." copy-to-register :active (region-exists-p)]
      ["%_Append to Register..." append-register :active (region-exists-p)]
      ["%_Insert Register..." insert-register]
      "---"
      ["%_Save Point to Register" point-to-register]
      ["%_Jump to Register"  register-to-point]
      )
     "---"
     ["%_Sort Lines in Region" sort-lines :active (region-exists-p)]
     ["%_Uppercase Region or Word" upcase-region-or-word]
     ["%_Lowercase Region or Word" downcase-region-or-word]
     ["%_Indent Region or Balanced Expression"
      indent-region-or-balanced-expression]
     ["%_Fill Paragraph or Region" fill-paragraph-or-region]
     ("Other %_Text Commands"
      ["Sort %_Paragraphs in Region" sort-paragraphs :active (region-exists-p)]
      ["Sort Pa%_ges in Region" sort-pages :active (region-exists-p)]
      ["Sort C%_olumns in Region" sort-columns :active (region-exists-p)]
      ["Sort %_Regexp..." sort-regexp-fields :active (region-exists-p)]
      "---"
      ["%_Capitalize Region" capitalize-region :active (region-exists-p)]
      ["Title-C%_ase Region" capitalize-region-as-title
       :active (region-exists-p)]
      "----"
      ["C%_enter Region or Paragraph"
       (if (region-active-p) (center-region) (center-line))]
      ["Center %_Line" center-line]
      "---"
      ["%_Indent Region Rigidly" indent-rigidly :active (region-exists-p)]
      ["In%_dent To Column..." indent-to-column]
      "---"
      ["%_Untabify (Tabs to Spaces)" untabify :active (and (region-exists-p)
							   (fboundp 'untabify))]
      ["%_Tabify (Spaces to Tabs)" tabify :active (and (region-exists-p)
						       (fboundp 'tabify))]
      ["Tab to Tab %_Stop" tab-to-tab-stop]
      ["Edit Ta%_b Stops" edit-tab-stops]
      )
     "---"
     ("%_Tags"
      ["%_Find Tag..." find-tag]
      ["Find %_Other Window..." find-tag-other-window]
      ["%_Next Tag..." (find-tag nil)]
      ["N%_ext Other Window..." (find-tag-other-window nil)]
      ["Next %_File" next-file]
      "-----"
      ["Tags %_Search..." tags-search]
      ["Tags %_Replace..." tags-query-replace]
      ["%_Continue Search/Replace" tags-loop-continue]
      "-----"
      ["%_Pop stack" pop-tag-mark]
      ["%_Apropos..." tags-apropos]
      "-----"
      ["%_Set Tags Table File..." visit-tags-table]
      )
     )

     ;; #### Delete this entire menu as soon as the new package source is
     ;; committed.
     ("%_Tools"
     ("%_Packages"
      ("%_Set Download Site"
       ("%_Official Releases"
	:filter ,#'(lambda (&rest junk)
		     (menu-split-long-menu
		      (submenu-generate-accelerator-spec
		       (package-ui-download-menu)))))
       ("%_Pre-Releases"
	:filter ,#'(lambda (&rest junk)
		     (menu-split-long-menu
		      (submenu-generate-accelerator-spec
		       (package-ui-pre-release-download-menu)))))
       ("%_Site Releases"
	:filter ,#'(lambda (&rest junk)
		     (menu-split-long-menu
		      (submenu-generate-accelerator-spec
		       (package-ui-site-release-download-menu))))))
      "--:shadowEtchedIn"
      ["%_Update Package Index" package-get-update-base]
      ["%_List and Install" pui-list-packages]
      ["U%_pdate Installed Packages" package-get-update-all]
      ["%_Help" (Info-goto-node "(xemacs)Packages")])
     ("%_Internet"
      ["Read Mail %_1 (VM)..." vm
       :active (fboundp 'vm)]
      ["Read Mail %_2 (MH)..." (mh-rmail t)
       :active (fboundp 'mh-rmail)]
      ["Send %_Mail..." compose-mail
       :active (fboundp 'compose-mail)]
      ["Usenet %_News" gnus
       :active (fboundp 'gnus)]
      ["Browse the %_Web" w3
       :active (fboundp 'w3)])
     "---"
     ("%_Grep"
      :filter
      ,#'(lambda (menu)
	   (if-boundp 'grep-history
	       (if grep-history
		   (let ((items
			  (submenu-generate-accelerator-spec
			   (mapcar #'(lambda (label-value)
				       (vector (first label-value)
					       (list 'grep
						     (second label-value))))
				   (Menubar-items-truncate-history
				    grep-history 10 50)))))
		     (append menu '("---") items))
		 menu)
	     menu))
      ["%_Grep..." grep :active (fboundp 'grep)]
      ["%_Kill Grep" kill-compilation
       :active (and (fboundp 'kill-compilation)
		    (fboundp 'compilation-find-buffer)
		    (let ((buffer (condition-case nil
				      (compilation-find-buffer)
				    (error nil))))
		      (and buffer (get-buffer-process buffer))))]
      "---"
      ["Grep %_All Files in Current Directory..."
       grep-all-files-in-current-directory
       :active (fboundp 'grep-all-files-in-current-directory)]
      ["Grep All Files in Current Directory %_Recursively..."
       grep-all-files-in-current-directory-and-below
       :active (fboundp 'grep-all-files-in-current-directory-and-below)]
      "---"
      ["Grep %_C and C Header Files in Current Directory..."
       (progn
	 (require 'compile)
	 (let ((grep-command
		(cons (concat grep-command " *.[chCH]"
					; i wanted to also use *.cc and *.hh.
					; see long comment below under Perl.
			      )
		      (length grep-command))))
	   (call-interactively 'grep)))
       :active (fboundp 'grep)]
      ["Grep C Hea%_der Files in Current Directory..."
       (progn
	 (require 'compile)
	 (let ((grep-command
		(cons (concat grep-command " *.[hH]"
					; i wanted to also use *.hh.
					; see long comment below under Perl.
			      )
		      (length grep-command))))
	   (call-interactively 'grep)))
       :active (fboundp 'grep)]
      ["Grep %_E-Lisp Files in Current Directory..."
       (progn
	 (require 'compile)
	 (let ((grep-command
		(cons (concat grep-command " *.el")
		      (length grep-command))))
	   (call-interactively 'grep)))
       :active (fboundp 'grep)]
      ["Grep %_Perl Files in Current Directory..."
       (progn
	 (require 'compile)
	 (let ((grep-command
		(cons (concat grep-command " *.pl"
					; i wanted to use this:
					; " *.pl *.pm *.am"
					; but grep complains if it can't
					; match anything in a glob, and
					; that screws other things up.
					; perhaps we need to first scan
					; each separate glob in the directory
					; to see if there are any files in
					; that glob, and if not, omit it.
			      )
		      (length grep-command))))
	   (call-interactively 'grep)))
       :active (fboundp 'grep)]
      ["Grep %_HTML Files in Current Directory..."
       (progn
	 (require 'compile)
	 (let ((grep-command
		(cons (concat grep-command " *.*htm*")
		      (length grep-command))))
	   (call-interactively 'grep)))
       :active (fboundp 'grep)]
      "---"
      ["%_Next Match" next-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["Pre%_vious Match" previous-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["%_First Match" first-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["G%_oto Match" compile-goto-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      "---"
      ["%_Set Grep Command..."
       (progn
	 (require 'compile)
	 (customize-set-variable
	  'grep-command
	  (read-shell-command "Default Grep Command: " grep-command)))
       :active (fboundp 'grep)
       ]
      )
     ("%_Compile"
      :filter
      ,#'(lambda (menu)
	   (if-boundp 'compile-history
	       (if compile-history
		   (let ((items
			  (submenu-generate-accelerator-spec
			   (mapcar #'(lambda (label-value)
				       (vector (first label-value)
					       (list 'compile
						     (second label-value))))
				   (Menubar-items-truncate-history
				    compile-history 10 50)))))
		     (append menu '("---") items))
		 menu)
	     menu))
      ["%_Compile..." compile :active (fboundp 'compile)]
      ["%_Repeat Compilation" recompile :active (fboundp 'recompile)]
      ["%_Kill Compilation" kill-compilation
       :active (and (fboundp 'kill-compilation)
		    (fboundp 'compilation-find-buffer)
		    (let ((buffer (condition-case nil
				      (compilation-find-buffer)
				    (error nil))))
		      (and buffer (get-buffer-process buffer))))]
      "---"
      ["%_Next Error" next-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["Pre%_vious Error" previous-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["%_First Error" first-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      ["G%_oto Error" compile-goto-error
       :active (and (fboundp 'compilation-errors-exist-p)
		    (compilation-errors-exist-p))]
      )
     ("%_Debug"
      ["%_GDB..." gdb
       :active (fboundp 'gdb)]
      ["%_DBX..." dbx
       :active (fboundp 'dbx)])
     ("%_Shell"
      ["%_Shell" shell
       :active (fboundp 'shell)]
      ["S%_hell Command..." shell-command
       :active (fboundp 'shell-command)]
      ["Shell Command on %_Region..." shell-command-on-region
       :active (and (fboundp 'shell-command-on-region) (region-exists-p))])

     ("%_Tags"
      ["%_Find Tag..." find-tag]
      ["Find %_Other Window..." find-tag-other-window]
      ["%_Next Tag..." (find-tag nil)]
      ["N%_ext Other Window..." (find-tag-other-window nil)]
      ["Next %_File" next-file]
      "-----"
      ["Tags %_Search..." tags-search]
      ["Tags %_Replace..." tags-query-replace]
      ["%_Continue Search/Replace" tags-loop-continue]
      "-----"
      ["%_Pop stack" pop-tag-mark]
      ["%_Apropos..." tags-apropos]
      "-----"
      ["%_Set Tags Table File..." visit-tags-table]
      )

     "----"

     ("Ca%_lendar"
      ["%_3-Month Calendar" calendar
       :active (fboundp 'calendar)]
      ["%_Diary" diary
       :active (fboundp 'diary)]
      ["%_Holidays" holidays
       :active (fboundp 'holidays)]
      ;; we're all pagans at heart ...
      ["%_Phases of the Moon" phases-of-moon
       :active (fboundp 'phases-of-moon)]
      ["%_Sunrise/Sunset" sunrise-sunset
       :active (fboundp 'sunrise-sunset)])

     ("Ga%_mes"
      ["%_Mine Game" xmine
       :active (fboundp 'xmine)]
      ["%_Tetris" tetris
       :active (fboundp 'tetris)]
      ["%_Sokoban" sokoban
       :active (fboundp 'sokoban)]
      ["Quote from %_Zippy" yow
       :active (fboundp 'yow)]
      ["%_Psychoanalyst" doctor
       :active (fboundp 'doctor)]
      ["Ps%_ychoanalyze Zippy!" psychoanalyze-pinhead
       :active (fboundp 'psychoanalyze-pinhead)]
      ["%_Random Flames" flame
       :active (fboundp 'flame)]
      ["%_Dunnet (Adventure)" dunnet
       :active (fboundp 'dunnet)]
      ["Towers of %_Hanoi" hanoi
       :active (fboundp 'hanoi)]
      ["Game of %_Life" life
       :active (fboundp 'life)]
      ["M%_ultiplication Puzzle" mpuz
       :active (fboundp 'mpuz)])

     "----"
     )

;      ("%_Tools"
;       :filter behavior-menu-filter)

     ("%_Options"
     ("%_Advanced (Customize)"
      ("%_Emacs" :filter ,#'(lambda (&rest junk)
			      (cdr (custom-menu-create 'emacs))))
      ["%_Group..." customize-group]
      ["%_Variable..." customize-variable]
      ["%_Face..." customize-face]
      ["%_Saved..." customize-saved]
      ["Se%_t..." customize-customized]
      ["%_Apropos..." customize-apropos]
      ["%_Browse..." customize-browse])
     "---"
     ("%_Editing"
      ["This Buffer %_Read Only" (toggle-read-only)
       :style toggle :selected buffer-read-only]
      ["%_Yank/Kill Interact With Clipboard"
       (if (eq interprogram-cut-function 'own-clipboard)
	   (progn
	     (customize-set-variable 'interprogram-cut-function nil)
	     (customize-set-variable 'interprogram-paste-function nil))
	 (customize-set-variable 'interprogram-cut-function 'own-clipboard)
	 (customize-set-variable 'interprogram-paste-function 'get-clipboard))
       :style toggle
       :selected (eq interprogram-cut-function 'own-clipboard)]
      ["%_Overstrike"
       (progn
	 (setq overwrite-mode (if overwrite-mode nil 'overwrite-mode-textual))
	 (customize-set-variable 'overwrite-mode overwrite-mode))
       :style toggle :selected overwrite-mode]
      ["%_Abbrev Mode"
       (customize-set-variable 'abbrev-mode
			       (not (default-value 'abbrev-mode)))
       :style toggle
       :selected (default-value 'abbrev-mode)]
      ["Active Re%_gions"
       (customize-set-variable 'zmacs-regions (not zmacs-regions))
       :style toggle :selected zmacs-regions]
      "---"
      ["%_Case Sensitive Search"
       (customize-set-variable 'case-fold-search
			       (setq case-fold-search (not case-fold-search)))
       :style toggle :selected (not case-fold-search)]
      ["Case %_Matching Replace"
       (customize-set-variable 'case-replace (not case-replace))
       :style toggle :selected case-replace]
      "---"
      ("%_Newline at End of File..."
       ["%_Don't Require"
	(customize-set-variable 'require-final-newline nil)
	:style radio :selected (not require-final-newline)]
       ["%_Require"
	(customize-set-variable 'require-final-newline t)
	:style radio :selected (eq require-final-newline t)]
       ["%_Ask"
	(customize-set-variable 'require-final-newline 'ask)
	:style radio :selected (and require-final-newline
				    (not (eq require-final-newline t)))])
      ["Add Newline When Moving Past %_End"
       (customize-set-variable 'next-line-add-newlines
			       (not next-line-add-newlines))
       :style toggle :selected next-line-add-newlines])
     ("%_Keyboard and Mouse"
      ["%_Delete Key Deletes Selection"
       (customize-set-variable 'pending-delete-mode (not pending-delete-mode))
       :style toggle
       :selected (and (boundp 'pending-delete-mode) pending-delete-mode)
       :active (boundp 'pending-delete-mode)]
      ["`kill-%_word' Stores in Clipboard"
       (customize-set-variable 'kill-word-into-kill-ring
			       (not kill-word-into-kill-ring))
       :style toggle
       :selected kill-word-into-kill-ring]
      ["`kill-%_line' Kills Whole Line at Beg"
       (customize-set-variable 'kill-whole-line (not kill-whole-line))
       :style toggle
       :selected kill-whole-line]
      ["Size for %_Block-Movement Commands..."
       (customize-set-variable 'block-movement-size
			       (read-number "Block Movement Size: "
					    t block-movement-size))]
      ["%_VI Emulation"
       (progn
	 (toggle-viper-mode)
	 (customize-set-variable 'viper-mode viper-mode))
       :style toggle :selected (and (boundp 'viper-mode) viper-mode)
       :active (fboundp 'toggle-viper-mode)]
      "----"
      ["S%_hifted Motion Keys Select Region"
       (customize-set-variable 'shifted-motion-keys-select-region
			       (not shifted-motion-keys-select-region))
       :style toggle
       :selected shifted-motion-keys-select-region]
      ["%_After Shifted Motion, Unshifted Motion Keys Deselect"
       (customize-set-variable 'unshifted-motion-keys-deselect-region
			       (not unshifted-motion-keys-deselect-region))
       :style toggle
       :selected unshifted-motion-keys-deselect-region]
      "----"
      ["%_Set Key..." global-set-key]
      ["%_Unset Key..." global-unset-key]
      "---"
      ["%_Mouse Paste at Text Cursor (not Clicked Location)"
       (customize-set-variable 'mouse-yank-at-point (not mouse-yank-at-point))
       :style toggle :selected mouse-yank-at-point]
      "---"
      ["%_Teach Extended Commands"
       (customize-set-variable 'teach-extended-commands-p
			       (not teach-extended-commands-p))
       :style toggle :selected teach-extended-commands-p]
      )
     ("%_Printing"
      ["Set Printer %_Name for Generic Print Support..."
       (customize-set-variable
	'printer-name
	(read-string "Set printer name: " printer-name))]
      "---"
      ["Command-Line %_Switches for `lpr'/`lp'..."
       ;; better to directly open a customization buffer, since the value
       ;; must be a list of strings, which is somewhat complex to prompt for.
       (customize-variable 'lpr-switches)
       (boundp 'lpr-switches)]
      ("%_Pretty-Print Paper Size"
       ["%_Letter"
	(customize-set-variable 'ps-paper-type 'letter)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'letter))
	:active (boundp 'ps-paper-type)]
       ["Lette%_r-Small"
	(customize-set-variable 'ps-paper-type 'letter-small)
	:style radio
	:selected (and (boundp 'ps-paper-type)
		       (eq ps-paper-type 'letter-small))
	:active (boundp 'ps-paper-type)]
       ["Le%_gal"
	(customize-set-variable 'ps-paper-type 'legal)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'legal))
	:active (boundp 'ps-paper-type)]
       ["%_Statement"
	(customize-set-variable 'ps-paper-type 'statement)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'statement))
	:active (boundp 'ps-paper-type)]
       ["%_Executive"
	(customize-set-variable 'ps-paper-type 'executive)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'executive))
	:active (boundp 'ps-paper-type)]
       ["%_Tabloid"
	(customize-set-variable 'ps-paper-type 'tabloid)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'tabloid))
	:active (boundp 'ps-paper-type)]
       ["Le%_dger"
	(customize-set-variable 'ps-paper-type 'ledger)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'ledger))
	:active (boundp 'ps-paper-type)]
       ["A%_3"
	(customize-set-variable 'ps-paper-type 'a3)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'a3))
	:active (boundp 'ps-paper-type)]
       ["%_A4"
	(customize-set-variable 'ps-paper-type 'a4)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'a4))
	:active (boundp 'ps-paper-type)]
       ["A4s%_mall"
	(customize-set-variable 'ps-paper-type 'a4small)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'a4small))
	:active (boundp 'ps-paper-type)]
       ["B%_4"
	(customize-set-variable 'ps-paper-type 'b4)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'b4))
	:active (boundp 'ps-paper-type)]
       ["%_B5"
	(customize-set-variable 'ps-paper-type 'b5)
	:style radio
	:selected (and (boundp 'ps-paper-type) (eq ps-paper-type 'b5))
	:active (boundp 'ps-paper-type)]
       )
      ["%_Color Printing"
       (cond (ps-print-color-p
	      (customize-set-variable 'ps-print-color-p nil)
	      ;; I'm wondering whether all this muck is useful.
	      (and (boundp 'original-face-background)
		   original-face-background
		   (set-face-background 'default original-face-background)))
	     (t
	      (customize-set-variable 'ps-print-color-p t)
	      (setq original-face-background
		    (face-background-instance 'default))
	      (set-face-background 'default "white")))
       :style toggle
       :selected (and (boundp 'ps-print-color-p) ps-print-color-p)
       :active (boundp 'ps-print-color-p)])
     ("%_Internet"
      ("%_Compose Mail With"
       ["VM mail package"
	(customize-set-variable 'mail-user-agent 'vm-user-agent)
	:style radio
	:selected (eq mail-user-agent 'vm-user-agent)
	:active (get 'vm-user-agent 'composefunc)]
       ["Bare-bones Emacs Mailer"
	(customize-set-variable 'mail-user-agent 'sendmail-user-agent)
	:style radio
	:selected (eq mail-user-agent 'sendmail-user-agent)]
       ["MH"
	(customize-set-variable 'mail-user-agent 'mh-e-user-agent)
	:style radio
	:selected (eq mail-user-agent 'mh-e-user-agent)
	:active (get 'mh-e-user-agent 'composefunc)]
       ["GNUS"
	(customize-set-variable 'mail-user-agent 'message-user-agent)
	:style radio
	:selected (eq mail-user-agent 'message-user-agent)
	:active (get 'message-user-agent 'composefunc)]
       )
      ["Set My %_Email Address..."
       (customize-set-variable
	'user-mail-address
	(read-string "Set email address: " user-mail-address))]
      ["Set %_Machine Email Name..."
       (customize-set-variable
	'mail-host-address
	(read-string "Set machine email name: " mail-host-address))]
      ["Set %_SMTP Server..."
       (progn
	 (require 'smtpmail)
	 (customize-set-variable
	  'smtpmail-smtp-server
	  (read-string "Set SMTP server: " smtpmail-smtp-server)))
       :active (and (boundp 'send-mail-function)
		    (eq send-mail-function 'smtpmail-send-it))]
      ["SMTP %_Debug Info"
       (progn
	 (require 'smtpmail)
	 (customize-set-variable 'smtpmail-debug-info
				 (not smtpmail-debug-info)))
       :style toggle
       :selected (and (boundp 'smtpmail-debug-info) smtpmail-debug-info)
       :active (and (boundp 'send-mail-function)
		    (eq send-mail-function 'smtpmail-send-it))])
     ("%_Troubleshooting"
      ["%_Debug on Error [not saved]"
       (setq debug-on-error (not debug-on-error))
       :style toggle :selected debug-on-error]
      ["Debug on %_Quit [not saved]"
       (setq debug-on-quit (not debug-on-quit))
       :style toggle :selected debug-on-quit]
      ["Debug on S%_ignal [not saved]"
       (setq debug-on-signal (not debug-on-signal))
       :style toggle :selected debug-on-signal]
      ["%_Stack Trace on Error [not saved]"
       (setq stack-trace-on-error (not stack-trace-on-error))
       :style toggle :selected stack-trace-on-error]
      ["Stack Trace on Si%_gnal [not saved]"
       (setq stack-trace-on-signal (not stack-trace-on-signal))
       :style toggle :selected stack-trace-on-signal]
      )
     ("Encodin%_g"
      ["Automatic %_EOL Detection"
       (customize-set-variable 'eol-detection-enabled-p
			       (not eol-detection-enabled-p))
       :style toggle
       :selected eol-detection-enabled-p
       :included (not (memq system-type '(windows-nt cygwin32)))]
      ("Set Coding System of %_Buffer File"
       :filter
       ,#'(lambda (menu)
	    (coding-system-menu-filter
	     (lambda (entry)
	       (set-buffer-file-coding-system entry))
	     (lambda (entry) t))))
      ;; not implemented yet
      ("Set Coding System of %_Terminal"
       :filter
       ,#'(lambda (menu)
	    (coding-system-menu-filter
	     (lambda (entry)
	       (set-terminal-coding-system entry))
	     (lambda (entry) nil)))
       )
      ;; not implemented yet
      ("Set Coding System of %_Keyboard"
       :filter
       ,#'(lambda (menu)
	    (coding-system-menu-filter
	     (lambda (entry)
	       (set-keyboard-coding-system entry))
	     (lambda (entry) nil))))
      ("Set Coding System of %_Process"
       :filter
       ,#'(lambda (menu)
	    (coding-system-menu-filter
	     (lambda (entry)
	       (set-buffer-process-coding-system entry entry))
	     (lambda (entry) (get-buffer-process (current-buffer)))))))
     ,@(when (featurep 'mule)
	 `(("Internationa%_l"
	    ("Set %_Language Environment"
	     :filter
	     ,#'(lambda (menu)
		  (menu-split-long-menu-and-sort
		   (mapcar #'(lambda (entry)
			       `[ ,(car entry)
				  (set-language-environment ',(car entry))
				  :style radio
				  :selected
				  ,(equal (car entry)
					  current-language-environment)])
			   language-info-alist)
		   )))
	    ["%_Toggle Input Method" toggle-input-method]
	    ["Select %_Input Method" set-input-method]
	    )))
     "-----"
     ("%_Display"
      ,@(if (featurep 'scrollbar)
	    '(["%_Scrollbars"
	       (customize-set-variable 'scrollbars-visible-p
				       (not scrollbars-visible-p))
	       :style toggle
	       :selected scrollbars-visible-p]))
      ["%_Wrap Long Lines"
       (progn ;; becomes buffer-local
	 (setq truncate-lines (not truncate-lines))
	 (customize-set-variable 'truncate-lines truncate-lines))
       :style toggle
       :selected (not truncate-lines)]
      "----"
      ["%_3D Modeline"
       (customize-set-variable 'modeline-3d-p
			       (not modeline-3d-p))
       :style toggle
       :selected modeline-3d-p]
      ("Modeline %_Horizontal Scrolling"
       ["%_None"
	(customize-set-variable 'modeline-scrolling-method nil)
	:style radio
	:selected (not modeline-scrolling-method)]
       ["As %_Text"
	(customize-set-variable 'modeline-scrolling-method t)
	:style radio
	:selected (eq modeline-scrolling-method t)]
       ["As %_Scrollbar"
	(customize-set-variable 'modeline-scrolling-method 'scrollbar)
	:style radio
	:selected (eq modeline-scrolling-method 'scrollbar)]
       )
      ,@(if (featurep 'toolbar)
	    '("---"
	      ["%_Toolbars Visible"
	       (customize-set-variable 'toolbar-visible-p
				       (not toolbar-visible-p))
	       :style toggle
	       :selected toolbar-visible-p]
	      ["Toolbars Ca%_ptioned"
	       (customize-set-variable 'toolbar-captioned-p
				       (not toolbar-captioned-p))
	       :style toggle
	       :active toolbar-visible-p
	       :selected toolbar-captioned-p]
	      ("Default Toolba%_r Location"
	       ["%_Top"
		(customize-set-variable 'default-toolbar-position 'top)
		:style radio
		:active toolbar-visible-p
		:selected (eq default-toolbar-position 'top)]
	       ["%_Bottom"
		(customize-set-variable 'default-toolbar-position 'bottom)
		:style radio
		:active toolbar-visible-p
		:selected (eq default-toolbar-position 'bottom)]
	       ["%_Left"
		(customize-set-variable 'default-toolbar-position 'left)
		:style radio
		:active toolbar-visible-p
		:selected (eq default-toolbar-position 'left)]
	       ["%_Right"
		(customize-set-variable 'default-toolbar-position 'right)
		:style radio
		:active toolbar-visible-p
		:selected (eq default-toolbar-position 'right)]
	       )
	      ))
      ,@(if (featurep 'gutter)
	    '("---"
	      ["B%_uffers Tab Visible"
	       (customize-set-variable 'gutter-buffers-tab-visible-p
				       (not gutter-buffers-tab-visible-p))
	       :style toggle
	       :selected gutter-buffers-tab-visible-p]
	      ("Default %_Gutter Location"
	       ["%_Top"
		(customize-set-variable 'default-gutter-position 'top)
		:style radio
		:selected (eq default-gutter-position 'top)]
	       ["%_Bottom"
		(customize-set-variable 'default-gutter-position 'bottom)
		:style radio
		:selected (eq default-gutter-position 'bottom)]
	       ["%_Left"
		(customize-set-variable 'default-gutter-position 'left)
		:style radio
		:selected (eq default-gutter-position 'left)]
	       ["%_Right"
		(customize-set-variable 'default-gutter-position 'right)
		:style radio
		:selected (eq default-gutter-position 'right)]
	       )
	      ))
      "-----"
      ["%_Blinking Cursor"
       (customize-set-variable 'blink-cursor-mode (not blink-cursor-mode))
       :style toggle
       :selected (and (boundp 'blink-cursor-mode) blink-cursor-mode)
       :active (boundp 'blink-cursor-mode)]
      ["Bl%_ock Cursor"
       (progn
	 (customize-set-variable 'bar-cursor nil)
	 (force-cursor-redisplay))
       :style radio
       :selected (null bar-cursor)]
      ["Bar Cursor (%_1 Pixel)"
       (progn
	 (customize-set-variable 'bar-cursor t)
	 (force-cursor-redisplay))
       :style radio
       :selected (eq bar-cursor t)]
      ["Bar Cursor (%_2 Pixels)"
       (progn
	 (customize-set-variable 'bar-cursor 2)
	 (force-cursor-redisplay))
       :style radio
       :selected (and bar-cursor (not (eq bar-cursor t)))]
      "----"
      ("Pa%_ren Highlighting"
       ["%_None"
	(customize-set-variable 'paren-mode nil)
	:style radio
	:selected (and (boundp 'paren-mode) (not paren-mode))
	:active (boundp 'paren-mode)]
       ["%_Blinking Paren"
	(customize-set-variable 'paren-mode 'blink-paren)
	:style radio
	:selected (and (boundp 'paren-mode) (eq paren-mode 'blink-paren))
	:active (boundp 'paren-mode)]
       ["%_Steady Paren"
	(customize-set-variable 'paren-mode 'paren)
	:style radio
	:selected (and (boundp 'paren-mode) (eq paren-mode 'paren))
	:active (boundp 'paren-mode)]
       ["%_Expression"
	(customize-set-variable 'paren-mode 'sexp)
	:style radio
	:selected (and (boundp 'paren-mode) (eq paren-mode 'sexp))
	:active (boundp 'paren-mode)]
       ;;	 ["Nes%_ted Shading"
       ;;	  (customize-set-variable 'paren-mode 'nested)
       ;;	  :style radio
       ;;	  :selected (and (boundp 'paren-mode) (eq paren-mode 'nested))
       ;;	  :active (boundp 'paren-mode)]
       )
      "------"
      ["%_Line Numbers"
       (progn
	 (customize-set-variable 'line-number-mode (not line-number-mode))
	 (redraw-modeline))
       :style toggle :selected line-number-mode]
      ["%_Column Numbers"
       (progn
	 (customize-set-variable 'column-number-mode
				 (not column-number-mode))
	 (redraw-modeline))
       :style toggle :selected column-number-mode]

      ("\"Other %_Window\" Location"
       ["%_Always in Same Frame"
	(customize-set-variable
	 'get-frame-for-buffer-default-instance-limit nil)
	:style radio
	:selected (null get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (%_2 Frames Max)"
	(customize-set-variable 'get-frame-for-buffer-default-instance-limit
				2)
	:style radio
	:selected (eq 2 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (%_3 Frames Max)"
	(customize-set-variable 'get-frame-for-buffer-default-instance-limit
				3)
	:style radio
	:selected (eq 3 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (%_4 Frames Max)"
	(customize-set-variable 'get-frame-for-buffer-default-instance-limit
				4)
	:style radio
	:selected (eq 4 get-frame-for-buffer-default-instance-limit)]
       ["Other Frame (%_5 Frames Max)"
	(customize-set-variable 'get-frame-for-buffer-default-instance-limit
				5)
	:style radio
	:selected (eq 5 get-frame-for-buffer-default-instance-limit)]
       ["Always Create %_New Frame"
	(customize-set-variable 'get-frame-for-buffer-default-instance-limit
				0)
	:style radio
	:selected (eq 0 get-frame-for-buffer-default-instance-limit)]
       "-----"
       ["%_Temp Buffers Always in Same Frame"
	(customize-set-variable 'temp-buffer-show-function
				'show-temp-buffer-in-current-frame)
	:style radio
	:selected (eq temp-buffer-show-function
		      'show-temp-buffer-in-current-frame)]
       ["Temp Buffers %_Like Other Buffers"
	(customize-set-variable 'temp-buffer-show-function nil)
	:style radio
	:selected (null temp-buffer-show-function)]
       "-----"
       ["%_Make Current Frame Gnuserv Target"
	(customize-set-variable 'gnuserv-frame (if (eq gnuserv-frame t) nil
						 t))
	:style toggle
	:selected (and (boundp 'gnuserv-frame) (eq gnuserv-frame t))
	:active (boundp 'gnuserv-frame)]
       )
      )
     ("%_Menubars"
      ["%_Frame-Local Font Menu"
       (customize-set-variable 'font-menu-this-frame-only-p
			       (not font-menu-this-frame-only-p))
       :style toggle
       :selected (and (boundp 'font-menu-this-frame-only-p)
		      font-menu-this-frame-only-p)]
      ["%_Alt/Meta Selects Menu Items"
       (if (eq menu-accelerator-enabled 'menu-force)
	   (customize-set-variable 'menu-accelerator-enabled nil)
	 (customize-set-variable 'menu-accelerator-enabled 'menu-force))
       :style toggle
       :selected (eq menu-accelerator-enabled 'menu-force)]
      "----"
      ["Buffers Menu %_Length..."
       (customize-set-variable
	'buffers-menu-max-size
	;; would it be better to open a customization buffer ?
	(let ((val
	       (read-number
		"Enter number of buffers to display (or 0 for unlimited): ")))
	  (if (eq val 0) nil val)))]
      ["%_Multi-Operation Buffers Sub-Menus"
       (customize-set-variable 'complex-buffers-menu-p
			       (not complex-buffers-menu-p))
       :style toggle
       :selected complex-buffers-menu-p]
      ["S%_ubmenus for Buffer Groups"
       (customize-set-variable 'buffers-menu-submenus-for-groups-p
			       (not buffers-menu-submenus-for-groups-p))
       :style toggle
       :selected buffers-menu-submenus-for-groups-p]
      ["%_Verbose Buffer Menu Entries"
       (if (eq buffers-menu-format-buffer-line-function
	       'slow-format-buffers-menu-line)
	   (customize-set-variable 'buffers-menu-format-buffer-line-function
				   'format-buffers-menu-line)
	 (customize-set-variable 'buffers-menu-format-buffer-line-function
				 'slow-format-buffers-menu-line))
       :style toggle
       :selected (eq buffers-menu-format-buffer-line-function
		     'slow-format-buffers-menu-line)]
      ("Buffers Menu %_Sorting"
       ["%_Most Recently Used"
	(progn
	  (customize-set-variable 'buffers-menu-sort-function nil)
	  (customize-set-variable 'buffers-menu-grouping-function nil))
	:style radio
	:selected (null buffers-menu-sort-function)]
       ["%_Alphabetically"
	(progn
	  (customize-set-variable 'buffers-menu-sort-function
				  'sort-buffers-menu-alphabetically)
	  (customize-set-variable 'buffers-menu-grouping-function nil))
	:style radio
	:selected (eq 'sort-buffers-menu-alphabetically
		      buffers-menu-sort-function)]
       ["%_By Major Mode, Then Alphabetically"
	(progn
	  (customize-set-variable
	   'buffers-menu-sort-function
	   'sort-buffers-menu-by-mode-then-alphabetically)
	  (customize-set-variable
	   'buffers-menu-grouping-function
	   'group-buffers-menu-by-mode-then-alphabetically))
	:style radio
	:selected (eq 'sort-buffers-menu-by-mode-then-alphabetically
		      buffers-menu-sort-function)])
      "---"
      ["%_Ignore Scaled Fonts"
       (customize-set-variable 'font-menu-ignore-scaled-fonts
			       (not font-menu-ignore-scaled-fonts))
       :style toggle
       :selected (and (boundp 'font-menu-ignore-scaled-fonts)
		      font-menu-ignore-scaled-fonts)]
      )
     ("S%_yntax Highlighting"
      ["%_In This Buffer"
       (progn ;; becomes buffer local
	 (font-lock-mode)
	 (customize-set-variable 'font-lock-mode font-lock-mode))
       :style toggle
       :selected (and (boundp 'font-lock-mode) font-lock-mode)
       :active (boundp 'font-lock-mode)]
      ["%_Automatic"
       (customize-set-variable 'font-lock-auto-fontify
			       (not font-lock-auto-fontify))
       :style toggle
       :selected (and (boundp 'font-lock-auto-fontify) font-lock-auto-fontify)
       :active (fboundp 'font-lock-mode)]
      "-----"
      ["Force %_Rehighlight in this Buffer"
       (customize-set-variable 'font-lock-auto-fontify
			       (not font-lock-auto-fontify))
       :style toggle
       :selected (and (boundp 'font-lock-auto-fontify) font-lock-auto-fontify)
       :active (fboundp 'font-lock-mode)]
      "-----"
      ["%_Fonts"
       (progn
	 (require 'font-lock)
	 (font-lock-use-default-fonts)
	 (customize-set-variable 'font-lock-use-fonts t)
	 (customize-set-variable 'font-lock-use-colors nil)
	 (font-lock-mode 1))
       :style radio
       :selected (and (boundp 'font-lock-use-fonts) font-lock-use-fonts)
       :active (fboundp 'font-lock-mode)]
      ["%_Colors"
       (progn
	 (require 'font-lock)
	 (font-lock-use-default-colors)
	 (customize-set-variable 'font-lock-use-colors t)
	 (customize-set-variable 'font-lock-use-fonts nil)
	 (font-lock-mode 1))
       :style radio
       :selected (and (boundp 'font-lock-use-colors) font-lock-use-colors)
       :active (boundp 'font-lock-mode)]
      "-----"
      ["%_1 Least"
       (progn
	 (require 'font-lock)
	 (if (or (and (not (integerp font-lock-maximum-decoration))
		      (not (eq t font-lock-maximum-decoration)))
		 (and (integerp font-lock-maximum-decoration)
		      (<= font-lock-maximum-decoration 0)))
	     nil
	   (customize-set-variable 'font-lock-maximum-decoration nil)
	   (font-lock-recompute-variables)))
       :style radio
       :active (fboundp 'font-lock-mode)
       :selected (and (boundp 'font-lock-maximum-decoration)
		      (or (and (not (integerp font-lock-maximum-decoration))
			       (not (eq t font-lock-maximum-decoration)))
			  (and (integerp font-lock-maximum-decoration)
			       (<= font-lock-maximum-decoration 0))))]
      ["%_2 More"
       (progn
	 (require 'font-lock)
	 (if (and (integerp font-lock-maximum-decoration)
		  (= 1 font-lock-maximum-decoration))
	     nil
	   (customize-set-variable 'font-lock-maximum-decoration 1)
	   (font-lock-recompute-variables)))
       :style radio
       :active (fboundp 'font-lock-mode)
       :selected (and (boundp 'font-lock-maximum-decoration)
		      (integerp font-lock-maximum-decoration)
		      (= 1 font-lock-maximum-decoration))]
      ["%_3 Even More"
       (progn
	 (require 'font-lock)
	 (if (and (integerp font-lock-maximum-decoration)
		  (= 2 font-lock-maximum-decoration))
	     nil
	   (customize-set-variable 'font-lock-maximum-decoration 2)
	   (font-lock-recompute-variables)))
       :style radio
       :active (fboundp 'font-lock-mode)
       :selected (and (boundp 'font-lock-maximum-decoration)
		      (integerp font-lock-maximum-decoration)
		      (= 2 font-lock-maximum-decoration))]
      ["%_4 Most"
       (progn
	 (require 'font-lock)
	 (if (or (eq font-lock-maximum-decoration t)
		 (and (integerp font-lock-maximum-decoration)
		      (>= font-lock-maximum-decoration 3)))
	     nil
	   (customize-set-variable 'font-lock-maximum-decoration t)
	   (font-lock-recompute-variables)))
       :style radio
       :active (fboundp 'font-lock-mode)
       :selected (and (boundp 'font-lock-maximum-decoration)
		      (or (eq font-lock-maximum-decoration t)
			  (and (integerp font-lock-maximum-decoration)
			       (>= font-lock-maximum-decoration 3))))]
      "-----"
      ["Lazy %_Lock"
       (progn ;; becomes buffer local
	 (lazy-lock-mode)
	 (customize-set-variable 'lazy-lock-mode lazy-lock-mode)
	 ;; this shouldn't be necessary so there has to
	 ;; be a redisplay bug lurking somewhere (or
	 ;; possibly another event handler bug)
	 (redraw-modeline))
       :active (and (boundp 'font-lock-mode) (boundp 'lazy-lock-mode)
		    font-lock-mode)
       :style toggle
       :selected (and (boundp 'lazy-lock-mode) lazy-lock-mode)]
      ["Lazy %_Shot"
       (progn ;; becomes buffer local
	 (lazy-shot-mode)
	 (customize-set-variable 'lazy-shot-mode lazy-shot-mode)
	 ;; this shouldn't be necessary so there has to
	 ;; be a redisplay bug lurking somewhere (or
	 ;; possibly another event handler bug)
	 (redraw-modeline))
       :active (and (boundp 'font-lock-mode) (boundp 'lazy-shot-mode)
		    font-lock-mode)
       :style toggle
       :selected (and (boundp 'lazy-shot-mode) lazy-shot-mode)]
      ["Cac%_hing"
       (progn ;; becomes buffer local
	 (fast-lock-mode)
	 (customize-set-variable 'fast-lock-mode fast-lock-mode)
	 ;; this shouldn't be necessary so there has to
	 ;; be a redisplay bug lurking somewhere (or
	 ;; possibly another event handler bug)
	 (redraw-modeline))
       :active (and (boundp 'font-lock-mode) (boundp 'fast-lock-mode)
		    font-lock-mode)
       :style toggle
       :selected (and (boundp 'fast-lock-mode) fast-lock-mode)]
      )
     ("%_Font" :filter font-menu-family-constructor)
     ("Font Si%_ze" :filter font-menu-size-constructor)
     ;;      ("Font Weig%_ht" :filter font-menu-weight-constructor)
     ["Edit Fa%_ces..." (customize-face nil)]
     "-----"
     ["Edit I%_nit File"
      ;; #### there should be something that holds the name that the init
      ;; file should be created as, when it's not present.
      (let* ((el-file (or user-init-file "~/.xemacs/init.el"))
             (position (position ?. el-file :from-end t))
             el-file-directory)
	(if (not (mismatch el-file ".elc"
                           :start1 (or position (length el-file))))
	    (setq el-file
		  (subseq user-init-file 0 -1)))
        (unless (file-directory-p
                 (setq el-file-directory (file-name-directory el-file)))
          (message "Creating %s... " el-file-directory)
          (make-directory el-file-directory t)
          (message "Creating %s... done." el-file-directory))
	(find-file el-file)
	(or (eq major-mode 'emacs-lisp-mode)
	    (emacs-lisp-mode)))]
     ["%_Save Options to Custom File" customize-save-customized]
     )

     ("%_Buffers"
     :filter buffers-menu-filter
     ["Go To %_Previous Buffer" switch-to-other-buffer]
     ["Go To %_Buffer..." switch-to-buffer]
     "----"
     ["%_List All Buffers" list-all-buffers]
     ["%_Delete Buffer" kill-this-buffer
      :suffix (if put-buffer-names-in-file-menu (buffer-name) "")]
     "----"
     )

     nil	      ; the partition: menus after this are flushright

     ("%_Help"
     ["%_About XEmacs and Contributors" about-xemacs]
     ("%_More about XEmacs"
      ["What's %_New in XEmacs" view-emacs-news]
      ["XEmacs %_License" describe-copying]
      ["%_No Warranty" describe-no-warranty]
      ["%_Obtaining the Latest Version" describe-distribution]
      ["View %_Splash Screen" xemacs-splash-buffer]
      ["%_Home Page (www.xemacs.org)" xemacs-www-page
       :active (fboundp 'browse-url)])
     ["B%_eta Info" describe-beta
      :included (search "beta" emacs-version)]
     "-----"
     ("%_Info (Online Docs)"
      ["%_Info Contents" (Info-goto-node "(dir)")]
      ["%_How to Use Info" (Info-goto-node "(Info)")]
      "-----"
      ["XEmacs %_User's Manual" (Info-goto-node "(XEmacs)")]
      ["%_Getting Started with XEmacs" (Info-goto-node "(New-Users-Guide)")]
      ["XEmacs %_Lisp Reference Manual" (Info-goto-node "(Lispref)")]
      ["All About %_Packages" (Info-goto-node "(xemacs)Packages")]
      ["Find %_Packages" finder-by-keyword]
      ["%_XEmacs Internals Manual" (Info-goto-node "(Internals)")]
      "-----"
      ["Lookup %_Key Sequence in User's Manual..."
       Info-goto-emacs-key-command-node]
      ["Lookup %_Command in User's Manual..." Info-goto-emacs-command-node]
      ["Lookup %_Function in Lisp Reference..." Info-elisp-ref]
      "-----"
      ["Find %_Topic in User's Manual/Lispref..."
       Info-search-index-in-xemacs-and-lispref]
      ["%_Search Text in User's Manual..." Info-search-text-in-xemacs]
      ["S%_earch Text in Lisp Reference..."
       Info-search-text-in-lispref]
      )
     ("XEmacs %_FAQ"
      ["%_FAQ (local)" xemacs-local-faq]
      ["FAQ via %_WWW" xemacs-www-faq
       :active (fboundp 'browse-url)])
     ("%_Tutorials"
      :filter tutorials-menu-filter)
     ("%_Samples"
      ["View Sample %_init.el" view-sample-init-el
       :active (locate-data-file "sample.init.el")]
      ["View Sample .%_gtkrc"
       (Help-find-file (locate-data-file "sample.gtkrc"))
       :included (featurep 'gtk)
       :active (locate-data-file "sample.gtkrc")]
      ["View Sample .%_Xresources"
       (Help-find-file (locate-data-file "sample.Xresources"))
       :included (featurep 'x)
       :active (locate-data-file "sample.Xresources")]
      ["View Sample %_enriched.doc"
       (Help-find-file (locate-data-file "enriched.doc"))
       :active (locate-data-file "enriched.doc")])
     ("%_Commands, Variables, Keys"
      ["Describe %_Mode" describe-mode]
      ["%_Apropos..." hyper-apropos]
      ["%_Command-Only Apropos..." command-hyper-apropos]
      ["Apropos %_Docs..." apropos-documentation]
      "-----"
      ["Describe %_Key..." describe-key]
      ["Show %_Bindings" describe-bindings]
      ["Show M%_ouse Bindings" describe-pointer]
      "-----"
      ["Describe %_Function..." describe-function]
      ["Describe %_Variable..." describe-variable]
      ["%_Locate Command in Keymap..." where-is])
     ,@(when (featurep 'mule)
	 `(("Internationa%_l"
	    ("Describe %_Language Support"
	     :filter
	     ,#'(lambda (menu)
		  (menu-split-long-menu-and-sort
		   (mapcar #'(lambda (entry)
			       `[ ,(car entry)
				  (describe-language-environment
				   ',(car entry))
				  :style radio
				  :selected
				  ,(equal (car entry)
					  current-language-environment)])
			   language-info-alist)
		   )))
	    ["Describe %_Input Method" describe-input-method]
	    ["Describe Current %_Coding Systems"
	     describe-current-coding-system]
	    ["Show Character %_Table" view-charset-by-menu]
	    ;; not implemented yet
	    ["Show %_Diagnosis for MULE" mule-diag :active nil]
	    ["Show \"%_hello\" in Many Languages" view-hello-file]
	    )))
     ["%_Unix Manual" manual-entry]
     "-----"
     ["%_Current Installation Info" describe-installation
      :active (boundp 'Installation-string)]
     ["%_Known Problems" view-xemacs-problems ]
     ("Recent History"
      ["%_Messages" (view-lossage t)]
      ["%_Keystrokes" view-lossage]
      ["%_Warnings" view-warnings])
     ["Send %_Bug Report..." report-xemacs-bug
      :active (fboundp 'report-xemacs-bug)]))
   "The default XEmacs menubar.
See the documentation for `current-menubar' for details of the syntax
used here.")


(defun init-menubar-at-startup ()
  "Don't call this.
Adds `Load init files' button to menubar when starting up with -q."
  ;; load-user-init-file finds the init files itself now
  (when (not load-user-init-file-p)
    (add-menu-button
     nil
     ["%_Load init files"
      (progn
	(mapc #'(lambda (buf)
		 (with-current-buffer buf
		   (delete-menu-item '("Load init files"))))
	      (buffer-list))
	(load-user-init-file))
      ]
     "Help")))


;;; The File menu

(defvar put-buffer-names-in-file-menu t)

;;; The Buffers menu

(defgroup buffers-menu nil
  "Customization of `Buffers' menu."
  :group 'menu)

(defvar buffers-menu-omit-chars-list '(?b ?p ?l ?d))

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
  "*The function to call to return a string to represent a buffer in
the buffers menu.  The function is passed a buffer and a number
\(starting with 1) indicating which buffer line in the menu is being
processed and should return a string containing an accelerator
spec. (Check out `menu-item-generate-accelerator-spec' as a convenient
way of generating the accelerator specs.) The default value
`format-buffers-menu-line' just returns the name of the buffer and
uses the number as the accelerator.  Also check out
`slow-format-buffers-menu-line' which returns a whole bunch of info
about a buffer.

Very old versions of this function prototype only expected one argument, not
two.  This implementation will fail when supplied with only one argument."
  :type 'function
  :group 'buffers-menu)

(defcustom buffers-menu-sort-function
  'sort-buffers-menu-by-mode-then-alphabetically
  "*If non-nil, a function to sort the list of buffers in the buffers menu.
It will be passed two arguments (two buffers to compare) and should return
t if the first is \"less\" than the second.  One possible value is
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

(defcustom list-all-buffers-function 'list-buffers
  "*Function that `list-all-buffers' calls."
  :type '(choice (const list-buffers)
		 (const ibuffer)
		 (const ibuffer-other-window)
		 function)
  :group 'buffers-menu)

(defun sort-buffers-menu-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts the buffers in alphabetical order by name, but puts buffers beginning
with a star at the end of the list."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
         (len1 (length nam1))
         (len2 (length nam2))
	 (inv1p (eql ?\ (and (> len1 0) (aref nam1 0))))
	 (inv2p (eql ?\ (and (> len2 0) (aref nam2 0))))
	 (star1p (eql ?* (and (> len1 0) (aref nam1 0))))
         (star2p (eql ?* (and (> len2 0) (aref nam2 0)))))
    (cond ((not (eq inv1p inv2p))
	   (not inv1p))
	  ((not (eq star1p star2p))
	   (not star1p))
	  (t
	   (string-lessp nam1 nam2)))))

(defun sort-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts first by major mode and then alphabetically by name, but puts buffers
beginning with a star at the end of the list."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (first1 (elt nam1 0))
	 (first2 (elt nam2 0))
	 (inv1p (eql ?\  first1))
	 (inv2p (eql ?\  first2))
	 (star1p (eql ?* first1))
         (star2p (eql ?* first2))
	 compare-strings)
    (cond ((not (eq inv1p inv2p))
	   (not inv1p))
	  ((not (eq star1p star2p))
	   (not star1p))
	  ((and star1p star2p (prog2 
				  (setq compare-strings
					(compare-strings nam1 0 nil
							 nam2 0 nil t))
				  (and (fixnump compare-strings)
				       (minusp compare-strings)))))
	  ((eq (setq compare-strings
                     (compare-strings (symbol-name
                                       (symbol-value-in-buffer
                                        'major-mode buf1))
                                      0 nil
                                      (symbol-name
                                       (symbol-value-in-buffer
                                        'major-mode buf2))
                                      0 nil t))
	       t)
	   (setq compare-strings (compare-strings nam1 0 nil
						  nam2 0 nil t))
	   (and (fixnump compare-strings) (minusp compare-strings)))
	  (t (minusp compare-strings)))))

;; this version is too slow on some machines.
;; (vintage 1990, that is)
(defun slow-format-buffers-menu-line (buffer n)
  "For use as a value of `buffers-menu-format-buffer-line-function'.
This returns a string containing a bunch of info about the buffer."
  (concat (menu-item-generate-accelerator-spec n buffers-menu-omit-chars-list)
	  (format "%s%s %-19s %6s %-15s %s"
		  (if (buffer-modified-p buffer) "*" " ")
		  (if (symbol-value-in-buffer 'buffer-read-only buffer)
		      "%" " ")
		  (buffer-name buffer)
		  (buffer-size buffer)
		  (symbol-value-in-buffer 'mode-name buffer)
		  (or (buffer-file-name buffer) ""))))

(defun format-buffers-menu-line (buffer n)
  "For use as a value of `buffers-menu-format-buffer-line-function'.
This just returns the buffer's name."
  (concat (menu-item-generate-accelerator-spec n buffers-menu-omit-chars-list)
	  (buffer-name buffer)))

(defun group-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-grouping-function'.
This groups buffers by major mode.  It only really makes sense if
`buffers-menu-sorting-function' is
`sort-buffers-menu-by-mode-then-alphabetically'."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
         (len1 (length nam1))
         (len2 (length nam2)))
    (cond ((eql ?* (and (> len1 0) (aref nam1 0)))
           (and (null buf2) "*Misc*"))
          ((or (null buf2)
               (eql ?* (and (> len2 0) (aref nam2 0)))
               (not (eq (symbol-value-in-buffer 'major-mode buf1)
                        (symbol-value-in-buffer 'major-mode buf2))))
           (symbol-value-in-buffer 'mode-name buf1))
          (t nil))))

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

(defun buffers-menu-filter (menu)
  "This is the menu filter for the top-level buffers \"Buffers\" menu.
It dynamically creates a list of buffers to use as the contents of the menu.
Only the most-recently-used few buffers will be listed on the menu, for
efficiency reasons.  You can control how many buffers will be shown by
setting `buffers-menu-max-size'.  You can control the text of the menu
items by redefining the function `format-buffers-menu-line'."
  (let ((buffers (delete-if buffers-menu-omit-function (buffer-list)))
        (n 0) tail)
    (and buffers-menu-max-size (> buffers-menu-max-size 1)
	 (not (and buffers-menu-grouping-function
                   buffers-menu-submenus-for-groups-p))
	 ;; Shorten list of buffers (not with submenus!)
         (setf tail (nthcdr buffers-menu-max-size buffers))
         (setf (cdr tail) nil)) ;; Only set tail's cdr if tail is non-nil.
    (labels
        ((build-buffers-menu-internal-mapper (buffer)
           (if (eq buffer t)
               "---"
             (let ((buffer-name (buffer-name buffer))
                   (line (funcall buffers-menu-format-buffer-line-function
                                  buffer (setf n (1+ n)))))
               (if complex-buffers-menu-p
                   `(,line
                     ["S%_witch to Buffer"
                      (,buffers-menu-switch-to-buffer-function ,buffer-name) t]
                     ,@(if (eq buffers-menu-switch-to-buffer-function
                               'switch-to-buffer)
                           `(["Switch to Buffer, Other %_Frame"
                              (switch-to-buffer-other-frame ,buffer-name) t]))
                     ["%_Save Buffer"
                      ,@(if (and (buffer-modified-p buffer)
                                 (buffer-file-name buffer))
                            `((buffer-menu-save-buffer ,buffer-name) t)
                            (list nil nil))]
                     ["Save %_As..." (buffer-menu-write-file ,buffer-name) t]
                     ["%_Delete Buffer" (kill-buffer ,buffer-name) t])
                 ;; We don't want buffer names to be translated, so we put the
                 ;; buffer name in the suffix.  Also, avoid losing with
                 ;; non-ASCII buffer names.
                 ;; #### We still lose, however, if complex-buffers-menu-p.
                 ;; --mrb
                 `["" (,buffers-menu-switch-to-buffer-function ,buffer-name) t
                   ,line]))))
         (build-buffers-menu-internal (buffers)
           (setq n 0) ;; Wouldn't it be nice to have a good implementation of
		      ;; lexical scope.
           (mapcar #'build-buffers-menu-internal-mapper buffers)))
      (declare (inline build-buffers-menu-internal))
      (if buffers-menu-sort-function
          (setq buffers (sort buffers buffers-menu-sort-function)))
      (if (and buffers-menu-grouping-function
               buffers-menu-submenus-for-groups-p
               (or (not (fixnump buffers-menu-submenus-for-groups-p))
                   ;; (> (length buffers)
                   ;; buffers-menu-submenus-for-groups-p)))
                   (nthcdr buffers-menu-submenus-for-groups-p buffers)))
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
                  ;; Do build-buffers-menu-internal on each entry, and return
                  ;; a reversed list with the group name.
                  (loop for groupname in groupnames
                        for group in groups
                        with result = nil
                        do (push (cons groupname
                                       (build-buffers-menu-internal group))
                                 result)
                        finally return result)))
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
                ;; Remove any trailing separator.
                (when (nthcdr 1 buffers) ;; (>= (length buffers) 2)
                  (setq tail (nthcdr (- (length buffers) 2) buffers))
                  (if (eq t (cadr tail))
                      (setf (cdr tail) nil)))))
          (setq buffers (build-buffers-menu-internal buffers)))
      (append menu buffers))))

(defun list-all-buffers ()
  "Display a list of buffers.  Calls `list-all-buffers-function'."
  (interactive)
  (funcall (if (functionp list-all-buffers-function)
	       list-all-buffers-function
	     'list-buffers)))


;;; The Help menu

(defun tutorials-menu-filter (menu-items)
  (declare (special language-info-alist
		    current-language-environment
		    tutorial-supported-languages))
  (append
   (if (featurep 'mule)
       (if (assq 'tutorial
		 (assoc current-language-environment language-info-alist))
	   `([,(concat "%_Default (" current-language-environment ")")
	      help-with-tutorial]))
     '(["%_English" help-with-tutorial]))
   (submenu-generate-accelerator-spec
    (if (featurep 'mule)
	;; Mule tutorials.
	(mapcan #'(lambda (lang)
		    (let ((tut (assq 'tutorial lang)))
		      (and tut
			   (not (equal (car lang) "ASCII"))
			   ;; skip current language, since we already
			   ;; included it first
			   (not (equal (car lang)
					 current-language-environment))
                           ;; Hackish approach; if a language environment
                           ;; doesn't have associated locale information,
                           ;; it's not the preferred implementation for that
                           ;; language. Don't use it.
                           (assq 'locale lang)
			   `([,(car lang)
			      (help-with-tutorial nil ,(car lang))]))))
		language-info-alist)
      ;; Non mule tutorials.
      (mapcar #'(lambda (lang)
		  `[,(car lang)
		    (help-with-tutorial nil ,(car lang))])
	      tutorial-supported-languages)))))

(set-menubar default-menubar)

(when (featurep 'menubar-items)
  (loop for buf being the buffers do
    (or (eq default-menubar (symbol-value-in-buffer 'current-menubar buf))
	(with-current-buffer buf
	  (condition-case nil
	      (funcall major-mode)
	    (error (setq current-menubar default-menubar)))))))


;;; Popup menus.

(defconst default-popup-menu
  '("XEmacs Commands"
    ["%_Split Window" split-window-vertically]
    ["S%_plit Window (Side by Side)" split-window-horizontally]
    ["%_Un-Split (Keep This)" delete-other-windows
     :active (not (one-window-p t))]
    ["Un-Split (Keep %_Others)" delete-window
     :active (not (one-window-p t))]
    ))

;; In an effort to avoid massive menu clutter, this mostly worthless menu is
;; superseded by any local popup menu...
(setq-default mode-popup-menu default-popup-menu)


;;; backwards compatibility
(provide 'x-menubar)
(provide 'menubar-items)

;;; menubar-items.el ends here.
