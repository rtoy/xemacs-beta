;;;  hm--html-menu.el: A menu for the html-mode.
;;;  v4.60; 17 Feb 1996
;;;  Copyright (C) 1993, 1994, 1995, 1996  Heiko Muenkel
;;;  email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	Defines pulldown and popup menus for the html mode.
;;;	This file requires the following files:
;;;		hm--html.el
;;;		hm--date.el
;;;		hm--html-configuration.el
;;;		adapt.el
;;;		html-mode.el
;;;	The file html-mode.el is the html mode file from Marc Andreessen.
;;;
;;;	You should also have the w3 package from William M. Perry, for
;;;	browsing html- files in the xemacs and the program Xmosaic together
;;;	with the file html-view.el from Ron Tapia for browsing html- files
;;;	in the Xmosaic.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories and the
;;;	following lines in your .emacs:
;;;	
;;;	(autoload 'html-mode "hm--html-menu" "HTML major mode." t)
;;;	(or (assoc "\\.html$" auto-mode-alist)
;;;         (setq auto-mode-alist (cons '("\\.html$" . html-mode) 
;;;				        auto-mode-alist)))
;;;
;;;	Look at the file hm--html-configuration for further installation
;;;     points.
;;;


(provide 'hm--html-menu)
(require 'html-mode)
(require 'hm--html)
(require 'adapt)


;;
;; Menu "HTML"
;;

(defvar hm--html-pulldown-menu nil "*A List with the HTML-Menu.")
(defvar hm--html-menu-region-expert nil "*A List with the HTML-Menu.")
(defvar hm--html-menu-region-novice nil "*A List with the HTML-Menu.")
(defvar hm--html-menu-noregion-expert nil "*A List with the HTML-Menu.")
(defvar hm--html-menu-noregion-novice nil "*A List with the HTML-Menu.")

(setq hm--html-menu-noregion-expert
       '("HTML Noregion Expert Menu"
	 ("Anchors"
	  ["Html link..." hm--html-add-html-link t]
	  ["Info link..." hm--html-add-info-link t]
	  ["Gopher link..." hm--html-add-gopher-link t]
	  ["File link..." hm--html-add-file-link t]
	  "----"
	  ["Ftp link..." hm--html-add-ftp-link t]
	  ["News link..." hm--html-add-news-link t]
	  ["Mailbox link..." hm--html-add-mail-link t]
	  ["Mailto link..." hm--html-add-mailto-link t]
	  ["Wais link (direct)..." hm--html-add-direct-wais-link t]
	  ["Wais link (gateway)..." hm--html-add-wais-link t]
	  "----"
	  ["Proggate link..." hm--html-add-proggate-link t]
	  ["Local Proggate link..." hm--html-add-local-proggate-link t]
	  ["General link..." html-add-normal-link t]
	  "----"
	  ["Link target..." hm--html-add-link-target t]
	  )
	 ("Frame"
	  ["Full html frame..." hm--html-add-full-html-frame t]
	  ["Frame template..." 
	   (hm--html-insert-template hm--html-frame-template-file)
	   (file-exists-p hm--html-frame-template-file)]
	  "----"
	  ["Html" hm--html-add-html t]
	  ["Head" hm--html-add-head t]
	  ["Body" hm--html-add-body t]
	  "----"
	  ["Title and Header..." hm--html-add-title-and-header t]
	  ["Title..." hm--html-add-title t]
	  ["Header..." hm--html-add-header t]
	  ["Node Link..." hm--html-add-normal-node-link t]
	  ["Address" html-add-address t]
	  ["Signature" hm--html-add-signature t]
	  "----"
	  ["Created comment" hm--html-insert-created-comment t]
	  ["Changed comment" hm--html-insert-changed-comment t]
	  ["New date in title" hm--html-new-date t]
	  )
	 ("Structure"
	  ["Menu or list item" html-add-list-or-menu-item t]
	  ["Menu" html-add-menu t]
	  ["Unordered list" html-add-list t]
	  ["Ordered list" hm--html-add-numberlist t]
	  ["Directory list" hm--html-add-directory-list t]
	  "----"
	  ["Description list" html-add-description-list t]
	  ["Description title" hm--html-add-description-title t]
	  ["Description entry" hm--html-add-only-description-entry t]
	  ["Description title + entry" html-add-description-entry t]
	  "----"
	  ["Table..." hm--html-add-table t]
	  ["Table title..." hm--html-add-table-title t]
	  ["Table header..." hm--html-add-table-header t]
	  ["Table first row..." hm--html-add-first-table-row t]
	  ["Table additional row..." hm--html-add-additional-table-row t]
	  ("Additional Commands"
	   ["Table row frame..." hm--html-add-row-frame t]
	   ["Table header entry..." hm--html-add-header-entry t]
	   ["Table row entry..." hm--html-add-row-entry t]
	   ["Span columns..." hm--html-table-add-colspan-attribute t]
	   ["Span rows..." hm--html-table-add-rowspan-attribute t]
	   )
	  "----"
	  ["Paragraph container" hm--html-add-paragraph t]
	  ["Paragraph start tag" hm--html-add-paragraph-separator t]
	  ["New line" hm--html-add-line-break t]
	  ["Horizontal rule" hm--html-add-horizontal-rule t]
	  )
	 ("Formating Paragraphs"
	  ["Without links" html-add-plaintext t]
	  ["With links" hm--html-add-preformated t]
	  "----"
          ["Blockquote" html-add-blockquote t]
	  ["Listing" html-add-listing t]
	  ["Abstract" hm--html-add-abstract t]
	  )
	 ("Formatting Words"
	  ["Bold" hm--html-add-bold t]
	  ["Italic" hm--html-add-italic t]
	  ["Underline" hm--html-add-underline t]
          ["Typewriter" html-add-fixed t]
	  ["Strikethru" hm--html-add-strikethru t]
	  ["Superscript" hm--html-add-superscript t]
	  ["Subscript" hm--html-add-subscript t]
	  ;;	  ["Render" hm--html-add-render t]
	  "----"
          ["Emphasized" html-add-emphasized t]
          ["Strong" html-add-strong t]
	  "----"
	  ("Computing"
	   ["Definition" hm--html-add-definition t]
	   ["Keyboard" html-add-keyboard t]
	   ["Command" hm--html-add-command t]
	   ["Argument" hm--html-add-argument t]
	   ["Option" hm--html-add-option t]
	   ["Variable" html-add-variable t]
	   ["Instance" hm--html-add-instance t]
	   ["Code" hm--html-add-code t]
	   ["Sample" html-add-sample t]
	   )
	  ("Literature"
	   ["Quote" hm--html-add-quote t]
	   ["Acronym" hm--html-add-acronym t]
	   ["Abbrevation" hm--html-add-abbrevation t]
	   ["Citation" html-add-citation t]
	   ["Literature" hm--html-add-literature t]
	   ["Publication" hm--html-add-publication t]
	   ["ISBN" hm--html-add-isbn t]
	   )
	  ("Person"
	   ["Person" hm--html-add-person t]
	   ["Author" hm--html-add-author t]
	   ["Editor" hm--html-add-editor t]
	   ["Credits" hm--html-add-credits t]
	   ["Copyright" hm--html-add-copyright t]
	   )
	  "----"
	  ["Footnote" hm--html-add-footnote t]
	  ["Margin" hm--html-add-margin t]
	  "----"
	  ["HTML Comment" hm--html-add-comment t]
	  )
	 ("Include"
	  ["Top aligned image..." hm--html-add-image-top t]
	  ["Middle aligned image..." hm--html-add-image-middle t]
	  ["Bottom aligned image..." hm--html-add-image-bottom t]
;	  "----"
;	  ["File..."  hm--html-add-server-side-include-file t]
;	  ["Command..." hm--html-add-server-side-include-command t]
;	  ["Command with isindex parameter..." 
;	   hm--html-add-server-side-include-command-with-isindex-parameter
;	   t]
	  )
	 ("Forms"
	  ["Form..." hm--html-add-form t]
	  "----"
	  ["Text field..." hm--html-form-add-input-text t]
	  ["Password field..." hm--html-form-add-input-password t]
	  ["Isindex field..." hm--html-form-add-input-isindex t]
	  ["Integer field..." hm--html-form-add-input-integer t]
	  ["Float field..." hm--html-form-add-input-float t]
	  ["Date field..." hm--html-form-add-input-date t]
	  ["Url field..." hm--html-form-add-input-url t]
	  ["Scribble field..." hm--html-form-add-input-scribble t]
	  "----"
	  ["Checkbox button..." hm--html-form-add-input-checkbox t]
	  ["Radio button..." hm--html-form-add-input-radio t]
	  ["Reset button..." hm--html-form-add-input-reset t]
	  ["Submit button..." hm--html-form-add-input-submit t]
	  ["Image button..." hm--html-form-add-input-image t]
	  ["Audio button..." hm--html-form-add-input-audio t]
	  "----"
	  ["Option Menu..." hm--html-form-add-select-option-menu t]
	  ["Scrolled List..." hm--html-form-add-select-scrolled-list t]
	  ["Option..." hm--html-form-add-select-option t]
	  "----"
	  ["Textarea..." hm--html-form-add-textarea t]
	  )
	 ))


(setq hm--html-menu-noregion-novice
       '("HTML No-region Novice Menu"
	 ("Anchors"
	  ["Html link..." hm--html-add-html-link t]
	  ["File link..." hm--html-add-file-link t]
	  )
	 ("Frame"
	  ["Full html frame..." hm--html-add-full-html-frame t]
	  "----"
	  ["Title and Header..." hm--html-add-title-and-header t]
	  ["Signature" hm--html-add-signature t]
	  )
	 ("Structure"
	  ["Menu item" html-add-list-or-menu-item t]
	  ["Menu" html-add-menu t]
	  "----"
	  ["Paragraph Container" hm--html-add-paragraph t]
	  )
	 ("Formating Paragraphs"
	  ["Without links" html-add-plaintext t]
	  ["With links" hm--html-add-preformated t]
	  )
	 ("Formatting Words"
	  ["Bold" hm--html-add-bold t]
	  ["Italic" hm--html-add-italic t]
	  ["Underline" hm--html-add-underline t]
          ["Typewriter" html-add-fixed t]
	  )))

(setq hm--html-menu-region-expert
       '("HTML Region Expert Menu"
	 ("Anchors"
	  ["Html link..." hm--html-add-html-link-to-region t]
	  ["Info link..." hm--html-add-info-link-to-region t]
	  ["Gopher link..." hm--html-add-gopher-link-to-region t]
	  ["File link..." hm--html-add-file-link-to-region t]
	  "----"
	  ["Ftp link..." hm--html-add-ftp-link-to-region t]
	  ["News link..." hm--html-add-news-link-to-region t]
	  ["Mailbox link..." hm--html-add-mail-link-to-region t]
	  ["Mailto link..." hm--html-add-mailto-link-to-region t]
	  ["WAIS link (direct)..." hm--html-add-direct-wais-link-to-region t]
	  ["WAIS link (gateway)..." hm--html-add-wais-link-to-region t]
	  "----"
	  ["Proggate link..." hm--html-add-proggate-link-to-region t]
	  ["Local Proggate link..." 
	   hm--html-add-local-proggate-link-to-region 
	   t]
	  ["General link..." hm--html-add-normal-link-to-region t]
	  "----"
	  ["Link target..." html-add-reference-to-region t]
	  )
	 ("Frame"
	  ["Full html frame..." hm--html-add-full-html-frame-with-region t]
	  "----"
	  ["Head" hm--html-add-head-to-region t]
	  ["Body" hm--html-add-body-to-region t]
	  "----"
	  ["Title and Header..." hm--html-add-title-and-header-to-region t]
	  ["Title" hm--html-add-title-to-region t]
	  ["Header..." hm--html-add-header-to-region t]
	  ["Address" hm--html-add-address-to-region t]
	  )
	 ("Structure"
	  ["Menu" hm--html-add-menu-to-region t]
	  ["Unordered list" hm--html-add-list-to-region t]
	  ["Ordered list" hm--html-add-numberlist-to-region t]
	  ["Directory list" hm--html-add-directorylist-to-region t]
	  "----"
	  ["Description list" hm--html-add-description-list-to-region t]
	  "----"
	  ["Table..." hm--html-add-table-to-region t]
	  ["Table Title..." hm--html-add-table-title-to-region t]
	  ("Additional Commands"
	   ["Table row frame..." hm--html-add-row-frame-to-region t]
	   )
	  "----"
	  ["Paragraph container" hm--html-add-paragraph-to-region t]
	  )
	 ("Formatting Paragraphs"
	  ["Without links" hm--html-add-plaintext-to-region t]
	  ["With links" hm--html-add-preformated-to-region t]
	  "----"
          ["Blockquote" hm--html-add-blockquote-to-region t]
	  ["Listing" hm--html-add-listing-to-region t]
	  ["Abstract" hm--html-add-abstract-to-region t]
	  )
	 ("Formatting Words"
	  ["Bold" hm--html-add-bold-to-region t]
	  ["Italic" hm--html-add-italic-to-region t]
	  ["Underline" hm--html-add-underline-to-region t]
          ["Typewriter" hm--html-add-fixed-to-region t]
	  ["Strikethru" hm--html-add-strikethru-to-region t]
	  ["Superscript" hm--html-add-superscript-to-region t]
	  ["Subscript" hm--html-add-subscript-to-region t]
	  ;;	  ["Render" hm--html-add-render-to-region t]
	  "----"
          ["Emphasized" hm--html-add-emphasized-to-region t]
          ["Strong" hm--html-add-strong-to-region t]
	  "----"
	  ("Computing"
	   ["Definition" hm--html-add-definition-to-region t]
	   ["Keyboard" hm--html-add-keyboard-to-region t]
	   ["Command" hm--html-add-command-to-region t]
	   ["Argument" hm--html-add-argument-to-region t]
	   ["Option" hm--html-add-option-to-region t]
	   ["Variable" hm--html-add-variable-to-region t]	  
	   ["Instance" hm--html-add-instance-to-region t]
	   ["Code" hm--html-add-code-to-region t]
	   ["Sample" hm--html-add-sample-to-region t]
	   )
	  ("Literature"
	   ["Quote" hm--html-add-quote-to-region t]
	   ["Acronym" hm--html-add-acronym-to-region t]
	   ["Abbrevation" hm--html-add-abbrevation-to-region t]
	   ["Citation" hm--html-add-citation-to-region t]
	   ["Literature" hm--html-add-literature-to-region t]
	   ["Publication" hm--html-add-publication-to-region t]
	   ["ISBN" hm--html-add-isbn-to-region t]
	   )
	  ("Person"
	   ["Person" hm--html-add-person-to-region t]
	   ["Author" hm--html-add-author-to-region t]
	   ["Editor" hm--html-add-editor-to-region t]
	   ["Credits" hm--html-add-credits-to-region t]
	   ["Copyright" hm--html-add-copyright-to-region t]
	   )
	  "----"
	  ["Footnote" hm--html-add-footnote-to-region t]
	  ["Margin" hm--html-add-margin-to-region t]
	  "----"
	  ["HTML Comment" hm--html-add-comment-to-region t]
	  )
	 ("Forms"
	  ["Form..." hm--html-add-form-to-region t])
	 ))


(setq hm--html-menu-region-novice
      '("HTML Region Novice Menu"
	 ("Anchors"
	  ["Html link..." hm--html-add-html-link-to-region t]
	  ["File link..." hm--html-add-file-link-to-region t]
	  )
	 ("Frame"
	  ["Full html frame..." hm--html-add-full-html-frame-with-region t]
	  "----"
	  ["Title and Header..." hm--html-add-title-and-header-to-region t]
	  )
	 ("Structure"
	  ["Menu" hm--html-add-menu-to-region t]
	  )
	 ("Formatting Paragraphs"
	  ["Without links" hm--html-add-plaintext-to-region t]
	  ["With links" hm--html-add-preformated-to-region t]
	  )
	 ("Formatting Words"
	  ["Bold" hm--html-add-bold-to-region t]
	  ["Italic" hm--html-add-italic-to-region t]
	  ["Underline" hm--html-add-underline-to-region t]
          ["Typewriter" hm--html-add-fixed-to-region t]
	  )
	 ))



(setq hm--html-pulldown-menu
      '("HTML Config Menu"
	("Set popup menu"
	 ["Novice menu" hm--html-use-novice-menu t]
	 ["Expert menu" hm--html-use-expert-menu t]
	 ["Marcs menu" hm--html-use-marcs-menu t]
	 )
;	("Highlighting"
;	 ["Toggle font lock mode" font-lock-mode (adapt-xemacsp)]
;	 ["Fontify buffer" font-lock-fontify-buffer (adapt-xemacsp)]
;	 ["Set font lock color..." 
;	  hm--html-set-font-lock-color 
;	  (or (adapt-xemacsp) (adapt-emacs19p))]
;	 "----"
;	 ["Toggle use highlighting" 
;	  hm--html-toggle-use-highlighting 
;	  html-use-highlighting]
;	 )
;	"----"
	["Reload config files" hm--html-load-config-files t]
	["Templates ..." hm--html-insert-template t]
	"----"
	["Remove numeric names" hm--html-remove-numeric-names t]
	["Quotify hrefs" html-quotify-hrefs t]
	"----"
	["Submit bug report..." hm--html-submit-bug-report t]
	["WWW Package Docs" hm--html-view-www-package-docu t]
	"----"
	("Preview Document"     
	 ["Netscape view buffer" (hm--html-send-buffer-to-netscape 
				  (current-buffer)) t]
	 "----"
	 ["Xmosaic start" html-view-start-mosaic t]
	 ["Xmosaic view buffer" html-view-view-buffer t]
	 ["Xmosaic view file" html-view-view-file t]
	 ["Xmosaic goto url" html-view-goto-url t]
	 ["Xmosaic get display" html-view-get-display t]
	 "----"
	 ["W3 start" w3 t]
	 ["W3 view buffer" w3-preview-this-buffer t]
	 ["W3 open remote file..." w3-fetch t]
	 ["W3 open local..." w3-open-local t]
	 ["W3 use hotlist..." w3-use-hotlist t]
	 )
	))
	

(if (adapt-xemacsp)
    (defun hm--install-html-menu ()
      (if (and current-menubar (not (assoc "HTML" current-menubar)))
	  (progn
	    (set-buffer-menubar (copy-sequence current-menubar))
	    (add-menu nil "HTML" (cdr hm--html-pulldown-menu))))) 
  (defun hm--install-html-menu ()
    (if (and current-menubar (not (assoc "HTML" current-menubar)))
	(progn
	  (set-buffer-menubar current-menubar)))
;	  (setq lucid-menubar-map nil)
;	  (make-local-variable 'lucid-menubar-map)
;	  (set-buffer-menubar (copy-sequence current-menubar))
;	  (make-local-variable 'lucid-menubar-map)
;	  (make-local-variable 'current-menubar)))
    (add-menu nil "HTML" (cdr hm--html-pulldown-menu))
      ))

;    (add-menu nil "HTML" (cdr hm--html-pulldown-menu))))


;(defun hm--popup-html-menu (event)
;  "Pops the HTML- menu up."
;  (interactive "@e")
;  (if hm--html-marc
;      (popup-menu html-menu)
;    (if hm--html-expert
;	(if hm--region-active
;	    (popup-menu hm--html-menu-region-expert)
;	  (popup-menu hm--html-menu-noregion-expert))
;      (if hm--region-active
;	  (popup-menu hm--html-menu-region-novice)
;	(popup-menu hm--html-menu-noregion-novice)))))


(defun hm--popup-html-menu (event)
  "Pops the HTML- menu up, if no region is active."
  (interactive "@e")
  (if hm--html-marc
      (popup-menu html-menu)
    (if hm--html-expert
	(popup-menu hm--html-menu-noregion-expert)
      (popup-menu hm--html-menu-noregion-novice))))


(defun hm--popup-html-menu-region (event)
  "Pops the HTML- menu up, if a region is active."
  (interactive "@e")
  (if hm--html-marc
      (popup-menu html-menu)
    (if hm--html-expert
	(popup-menu hm--html-menu-region-expert)
      (popup-menu hm--html-menu-region-novice))))


(defun hm--html-use-novice-menu ()
  "Changes the HTML popup menu to the novice menu."
  (interactive)
  (setq hm--html-expert nil)
  (setq hm--html-marc nil)
;  (define-key html-mode-map '(button3) 'hm--popup-html-menu)
;  (define-key html-region-mode-map '(button3) 'hm--popup-html-menu)
  )	


(defun hm--html-use-expert-menu ()
  "Changes the HTML popup menu to the expert menu."
  (interactive)
  (setq hm--html-expert t)
  (setq hm--html-marc nil)
;  (define-key html-mode-map '(button3) 'hm--popup-html-menu)
;  (define-key html-region-mode-map '(button3) 'hm--popup-html-menu)
  )


(defun hm--html-use-marcs-menu ()
  "Changes the HTML popup menu to Marc Andreessens menu."
  (interactive)
  (setq hm--html-marc t)
;  (define-key html-mode-map '(button3) 'hm--popup-html-menu)
;  (define-key html-region-mode-map '(button3) 'hm--popup-html-menu)
  )


;(define-key html-mode-map '(button3) 'hm--popup-html-menu)
;(define-key html-region-mode-map '(button3) 'hm--popup-html-menu)

(add-hook 'html-mode-hook 'hm--install-html-menu)


(defvar hm--html-menu-load-hook nil
  "*Hook variable to execute functions after loading the file hm--html-menu.")


(run-hooks 'hm--html-menu-load-hook)

