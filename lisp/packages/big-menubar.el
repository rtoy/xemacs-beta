;; big-menubar.el --- an alternate menubar

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Dror Caspi <dcaspi@qualcomm.com>
;; Modified by: jwz and allender and haydens@ll.mit.edu
;; Rewritten by: mrb in the differential programming style
;; Keywords: mouse menubar

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Code:

(let* ((current-menubar (default-value 'current-menubar))
       (edit-menu (assoc "Edit" current-menubar)))

  (unless (assoc "Transpose" edit-menu)	; Idempotence
    ;; Replace Macro buttons with Macro submenu
    (delete-menu-item '("Edit" "Start Macro Recording"))
    (delete-menu-item '("Edit" "End Macro Recording"))
    (delete-menu-item '("Edit" "Execute Last Macro"))

    (nconc
     edit-menu
     '(("Macro"
	["Start Macro Recording"	start-kbd-macro (not defining-kbd-macro)]
	["End Macro Recording"		end-kbd-macro	    defining-kbd-macro]
	["Name Last Macro..."		name-last-kbd-macro	last-kbd-macro]
	["Insert Macro in Buffer..."	insert-kbd-macro		     t]
	["Execute Last Macro"		call-last-kbd-macro	last-kbd-macro]
	)
       ;; Add other useful Edit submenus
       "---"
       ("Mark"
	["Here"			set-mark-command		t]
	["Word"			mark-word			t]
	["Sentence"		mark-end-of-sentence		t]
	["Paragraph"		mark-paragraph			t]
	["Page"			mark-page			t]
	["Balanced Expression"	mark-sexp			t]
	["Lisp Function"	mark-defun			t]
	["C Function"		mark-c-function			t]
	["Whole Buffer"		mark-whole-buffer		t]
	)
       "---"
       ("Search"
	["Forward..."		isearch-forward			t]
	["Backward..."		isearch-backward		t]
	"---"
	["Regexp Forward..."	isearch-forward-regexp		t]
	["Regexp Backward..."	isearch-backward-regexp		t]
	"---"
	["Words Forward..."	word-search-forward		t]
	["Words Backward..."	word-search-backward		t]
	)
       ("Replace"
	["Query..."		query-replace			t]
	["Regexp Query..."	query-replace-regexp		t]
	"---"
	["All..."		replace-string			t]
	["Regexp All..."	replace-regexp			t]
	)
       "---"
       ("Transpose"
	["Characters"		transpose-chars			t]
	["Words"		transpose-words			t]
	["Lines"		transpose-lines			t]
	["Sentences"		transpose-sentences		t]
	["Paragraphs"		transpose-paragraphs		t]
	["Balanced Expressions" transpose-sexps			t]
	)
       "---"
       ("Register"
	["Copy to Register..."	copy-to-register		(mark)]
	["Paste Register..."	insert-register			t]
	"---"
	["Save Point to Register"	point-to-register	t]
	["Jump to Register"		register-to-point	t]
	)
       ("Rectangles"
	["Kill Rectangle"		kill-rectangle			t]
	["Yank Rectangle"		yank-rectangle			t]
	["Rectangle to Register"	copy-rectangle-to-register	t]
	["Rectangle from Register"	insert-register			t]
	["Clear Rectangle"		clear-rectangle			t]
	["Open Rectangle"		open-rectangle			t]
	["Rectangle Mousing"
	 (setq mouse-track-rectangle-p (not mouse-track-rectangle-p))
	 :style toggle :selected mouse-track-rectangle-p]
	)
       "---"
       ("Sort"
	["Lines"		sort-lines		(mark)]
	["Paragraphs"		sort-paragraphs		(mark)]
	["Pages"		sort-pages		(mark)]
	["Columns"		sort-columns		(mark)]
	["Regexp..."		sort-regexp-fields	(mark)]
	)
       ("Center"
	["Line"			center-line		t]
	["Paragraph"		center-paragraph	t]
	["Region"		center-region		(mark)]
	)
       ("Indent"
	["As Previous Line"	indent-relative		t]
	["To Column..."		indent-to-column	t]
	"---"
	["Region"		indent-region		(mark)]
	["Balanced Expression"	indent-sexp		t]
	["C Expression"		indent-c-exp		t]
	)
       "---"
       ("Narrow"
	["To Region"		narrow-to-region	(mark)]
	["To Page"		narrow-to-page		t]
	"---"
	["Cancel"		widen
	 (not (and (= (point-min) 1) (= (point-max) (1+ (buffer-size)))))]
	)))

    ;; Motion Submenu
    (add-submenu
     nil
     '("Motion"
       ["Goto Mark"			exchange-point-and-mark	(mark t)]
       ["Goto Line..."			goto-line		t]
       "---"
       ["End of Balanced Parentheses ( )"	forward-list	t]
       ["Beginning of Balanced Parentheses ( )"	backward-list	t]
       ["Next Opening Parenthesis ("	down-list		t]
       ["Previous Opening Parenthesis (" backward-up-list	t]
       ["Next Closing Parenthesis )"	up-list			t]
       "---"
       ["End of Balanced Expression"	forward-sexp		t]
       ["Beginning of Balanced Expression" backward-sexp	t]
       "---"
       ["End of Function"		end-of-defun		t]
       ["Beginning of Function"		beginning-of-defun	t]
       "---"
       ["Next Page"			forward-page		t]
       ["Previous Page"			backward-page		t]
       "---"
       ["End of Buffer"			end-of-buffer		t]
       ["Beginning of Buffer"		beginning-of-buffer	t]
       "---"
       ["Save Current Position..."	point-to-register	t]
       ["Goto Saved Position..."	register-to-point	t]
       "---"
       ["Set Marker..."			set-user-marker		t]
       ["Goto Marker..."		goto-user-marker	t]
       ["List Markers"			list-markers		t]
       "---"
       ["Set Goal Column"		set-goal-column		t]
       ["Cancel Goal Column"		(set-goal-column t)	goal-column]
       )
     "Apps")

    ;; Replace Compile button with Compile menu
    (add-submenu
     '("Tools")
     '("Compile"
       ["Compile..."		compile			t]
       ["Kill Compilation"	kill-compilation	t]
       "---"
       ["Next Error"		next-error		t]
       ["Previous Error"	previous-error		t]
       ["Goto Error"		compile-goto-error	t]
       )
     "Compile...")
    (delete-menu-item '("Tools" "Compile..."))

    ;; Replace Spell-Check button with Spell Check menu
    (add-submenu
     '("Apps")
     '("Spell Check"
       ["Word"			ispell-word		t]
       ["Complete Word"		ispell-complete-word	t]
       ["Region"		ispell-region		t]
       ["Whole Buffer"		ispell-buffer		t]
       )
     "Spell-Check Buffer")
    (delete-menu-item '("Apps" "Spell-Check Buffer"))

    ;; Add Hex Edit button
    (nconc
     (assoc "Tools" current-menubar)
     '("---"
       ["Hex Edit File..." hexl-find-file t]))
    ))

(provide 'big-menubar)
