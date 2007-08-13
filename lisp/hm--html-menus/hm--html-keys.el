;;; hm--html-keys.el
;;; v1.50; 17-Feb-1996
;;; Copyright (C) 1995, 1996 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 1, or (at your option)
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
;;;	Defines the new keybindigs for the hm--html-menus package.
;;;	At the moment it will only be used, if the variable
;;;	'hm--html-use-old-keymap' in hm--html-configuration.el
;;;	is set to nil, which is the default.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories.
;;;

(provide 'hm--html-keys)


(defvar hm--html-noregion-anchor-map nil
  "Noregion sub keymap for inserting anchors.")

(if hm--html-noregion-anchor-map
    ()
  (setq hm--html-noregion-anchor-map (make-sparse-keymap))
  (define-key hm--html-noregion-anchor-map "h" 'hm--html-add-html-link)
  (define-key hm--html-noregion-anchor-map "i" 'hm--html-add-info-link)
  (define-key hm--html-noregion-anchor-map "g" 'hm--html-add-gopher-link)
  (define-key hm--html-noregion-anchor-map "f" 'hm--html-add-file-link)
  (define-key hm--html-noregion-anchor-map "\C-f" 'hm--html-add-ftp-link)
  (define-key hm--html-noregion-anchor-map "n" 'hm--html-add-news-link)
  (define-key hm--html-noregion-anchor-map "m" 'hm--html-add-mail-link)
;  (define-key hm--html-noregion-anchor-map "\C-m" 'hm--html-add-mailto-link)
  (define-key hm--html-noregion-anchor-map 
    [(control m)] 'hm--html-add-mailto-link)
  (define-key hm--html-noregion-anchor-map "w" 'hm--html-add-direct-wais-link)
  (define-key hm--html-noregion-anchor-map "\C-w" 'hm--html-add-wais-link)
  (define-key hm--html-noregion-anchor-map "p" 'hm--html-add-proggate-link)
  (define-key hm--html-noregion-anchor-map 
    "\C-p" 'hm--html-add-local-proggate-link)
  (define-key hm--html-noregion-anchor-map "l" 'html-add-normal-link)
  (define-key hm--html-noregion-anchor-map "t" 'hm--html-add-link-target)
  )

(defvar hm--html-region-anchor-map nil
  "Region sub keymap for inserting anchors.")

(if hm--html-region-anchor-map
    ()
  (setq hm--html-region-anchor-map (make-sparse-keymap))
  (define-key hm--html-region-anchor-map "h" 'hm--html-add-html-link-to-region)
  (define-key hm--html-region-anchor-map "i" 'hm--html-add-info-link-to-region)
  (define-key hm--html-region-anchor-map 
    "g" 'hm--html-add-gopher-link-to-region)
  (define-key hm--html-region-anchor-map "f" 'hm--html-add-file-link-to-region)
  (define-key hm--html-region-anchor-map 
    "\C-f" 'hm--html-add-ftp-link-to-region)
  (define-key hm--html-region-anchor-map "n" 'hm--html-add-news-link-to-region)
  (define-key hm--html-region-anchor-map "m" 'hm--html-add-mail-link-to-region)
;  (define-key hm--html-region-anchor-map 
;    "\C-m" 'hm--html-add-mailto-link-to-region)
  (define-key hm--html-region-anchor-map
    [(control m)] 'hm--html-add-mailto-link-to-region)
  (define-key hm--html-region-anchor-map 
    "w" 'hm--html-add-direct-wais-link-to-region)
  (define-key hm--html-region-anchor-map 
    "\C-w" 'hm--html-add-wais-link-to-region)
  (define-key hm--html-region-anchor-map 
    "p" 'hm--html-add-proggate-link-to-region)
  (define-key hm--html-region-anchor-map 
    "\C-p" 'hm--html-add-local-proggate-link-to-region)
  (define-key hm--html-region-anchor-map 
    "l" 'hm--html-add-normal-link-to-region)
  (define-key hm--html-region-anchor-map 
    "t" 'html-add-reference-to-region)
  )

(defvar hm--html-noregion-frame-map nil
  "Noregion sub keymap for inserting frame elements.")

(if hm--html-noregion-frame-map
    ()
  (setq	hm--html-noregion-frame-map (make-sparse-keymap))
  (define-key hm--html-noregion-frame-map "f" 'hm--html-add-full-html-frame)
  (define-key hm--html-noregion-frame-map [(control h)] 'hm--html-add-html) 
  (define-key hm--html-noregion-frame-map [(meta h)] 'hm--html-add-head)
  (define-key hm--html-noregion-frame-map "b" 'hm--html-add-body)
  (define-key hm--html-noregion-frame-map 
    [(control t)] 'hm--html-add-title-and-header)
  (define-key hm--html-noregion-frame-map "t" 'hm--html-add-title)
  (define-key hm--html-noregion-frame-map "h" 'hm--html-add-header)
  (define-key hm--html-noregion-frame-map "n" 'hm--html-add-normal-node-link)
  (define-key hm--html-noregion-frame-map "a" 'html-add-address)
  (define-key hm--html-noregion-frame-map "s" 'hm--html-add-signature)
  (define-key hm--html-noregion-frame-map 
    [(control c)] 'hm--html-insert-created-comment)
  (define-key hm--html-noregion-frame-map "c" 'hm--html-insert-changed-comment)
  (define-key hm--html-noregion-frame-map "d" 'hm--html-new-date)
  )

(defvar hm--html-region-frame-map nil
  "Region sub keymap for inserting frame elements.")

(if hm--html-region-frame-map
    ()
  (setq hm--html-region-frame-map (make-sparse-keymap))
  (define-key hm--html-region-frame-map 
    "f" 'hm--html-add-full-html-frame-with-region)
  (define-key hm--html-region-frame-map 
    [(meta h)] 'hm--html-add-head-to-region)
  (define-key hm--html-region-frame-map "b" 'hm--html-add-body-to-region)
  (define-key hm--html-region-frame-map 
    [(control t)] 'hm--html-add-title-and-header-to-region)
  (define-key hm--html-region-frame-map "t" 'hm--html-add-title-to-region)
  (define-key hm--html-region-frame-map "h" 'hm--html-add-header-to-region)
  (define-key hm--html-region-frame-map "a" 'hm--html-add-address-to-region)
  )

(defvar hm--html-noregion-structure-map nil
  "Noregion sub keymap for inserting entities.")

(if hm--html-noregion-structure-map
    ()
  (setq hm--html-noregion-structure-map (make-sparse-keymap))
  (define-key hm--html-noregion-structure-map "i" 'html-add-list-or-menu-item)
  (define-key hm--html-noregion-structure-map "m" 'html-add-menu)
  (define-key hm--html-noregion-structure-map "u" 'html-add-list)
  (define-key hm--html-noregion-structure-map "o" 'hm--html-add-numberlist)
  (define-key hm--html-noregion-structure-map "d" 'hm--html-add-directory-list)
  (define-key hm--html-noregion-structure-map 
    "\C-dl" 'html-add-description-list)
  (define-key hm--html-noregion-structure-map 
    "\C-dt" 'hm--html-add-description-title)
  (define-key hm--html-noregion-structure-map 
    "\C-de" 'hm--html-add-only-description-entry)
  (define-key hm--html-noregion-structure-map 
    "\C-d\C-t" 'html-add-description-entry)
  (define-key hm--html-noregion-structure-map
    "\C-tt" 'hm--html-add-table)
  (define-key hm--html-noregion-structure-map 
    "\C-t\C-t" 'hm--html-add-table-title)
  (define-key hm--html-noregion-structure-map 
    "\C-th" 'hm--html-add-table-header)
  (define-key hm--html-noregion-structure-map 
    "\C-tr" 'hm--html-add-first-table-row)
  (define-key hm--html-noregion-structure-map 
    "\C-t\C-r" 'hm--html-add-additional-table-row)
  (define-key hm--html-noregion-structure-map "p" 'hm--html-add-paragraph)
  (define-key hm--html-noregion-structure-map 
    "\C-p" 'hm--html-add-paragraph-separator)
  (define-key hm--html-noregion-structure-map "\C-m" 'hm--html-add-line-break)
  (define-key hm--html-noregion-structure-map 
    "h" 'hm--html-add-horizontal-rule)
  )

(defvar hm--html-region-structure-map nil
  "Region sub keymap for inserting entities.")

(if hm--html-region-structure-map
    ()
  (setq hm--html-region-structure-map (make-sparse-keymap))
  (define-key hm--html-region-structure-map "m" 'hm--html-add-menu-to-region)
  (define-key hm--html-region-structure-map "u" 'hm--html-add-list-to-region)
  (define-key hm--html-region-structure-map 
    "o" 'hm--html-add-numberlist-to-region)
  (define-key hm--html-region-structure-map 
    "d" 'hm--html-add-directory-list-to-region)
  (define-key hm--html-region-structure-map 
    "\C-dl" 'html-add-description-list-to-region)
  (define-key hm--html-region-structure-map
    "\C-tt" 'hm--html-add-table-to-region)
  (define-key hm--html-region-structure-map 
    "\C-t\C-t" 'hm--html-add-table-title-to-region)
  (define-key hm--html-region-structure-map 
    "p" 'hm--html-add-paragraph-to-region)
  )

(defvar hm--html-noregion-formating-paragraph-map nil
  "Noregion sub keymap for inserting paragraph formating elements.")

(if hm--html-noregion-formating-paragraph-map
    ()
  (setq hm--html-noregion-formating-paragraph-map (make-sparse-keymap))
  (define-key hm--html-noregion-formating-paragraph-map
    "o" 'html-add-plaintext)
  (define-key hm--html-noregion-formating-paragraph-map
    "w" 'hm--html-add-preformated)
  (define-key hm--html-noregion-formating-paragraph-map
    "b" 'html-add-blockquote)
  (define-key hm--html-noregion-formating-paragraph-map
    "l" 'html-add-listing)
  (define-key hm--html-noregion-formating-paragraph-map
    "a" 'hm--html-add-abstract)
  )

(defvar hm--html-region-formating-paragraph-map nil
  "Region sub keymap for inserting paragraph formating elements.")

(if hm--html-region-formating-paragraph-map
    ()
  (setq hm--html-region-formating-paragraph-map (make-sparse-keymap))
  (define-key hm--html-region-formating-paragraph-map
    "o" 'hm--html-add-plaintext-to-region)
  (define-key hm--html-region-formating-paragraph-map
    "w" 'hm--html-add-preformated-to-region)
  (define-key hm--html-region-formating-paragraph-map
    "b" 'hm--html-add-blockquote-to-region)
  (define-key hm--html-region-formating-paragraph-map
    "l" 'hm--html-add-listing-to-region)
  (define-key hm--html-region-formating-paragraph-map
    "a" 'hm--html-add-abstract-to-region)
  )

;(defvar hm--html-noregion-entity-map nil
;  "Noregion sub keymap for inserting entities.")

;(if hm--html-noregion-entity-map
;    ()
;  (setq hm--html-noregion-entity-map (make-sparse-keymap))
;  )

;(defvar hm--html-region-entity-map nil
;  "Region sub keymap for inserting entities.")

;(if hm--html-region-entity-map
;    ()
;  (setq hm--html-region-entity-map (make-sparse-keymap))
;  )

;(defvar hm--html-noregion-logical-map nil
;  "Noregion sub keymap for inserting logical text formating elements.")

;(if hm--html-noregion-logical-map
;    ()
;  (setq hm--html-noregion-logical-map (make-sparse-keymap))
;  )

;(defvar hm--html-region-logical-map nil
;  "Region sub keymap for inserting logical text formating elements.")

;(if hm--html-region-logical-map
;    ()
;  (setq hm--html-region-logical-map (make-sparse-keymap))
;  )

(defvar hm--html-noregion-formating-word-map nil
  "Norgion sub keymap for inserting physical text formating elements.")

(if hm--html-noregion-formating-word-map
    ()
  (setq hm--html-noregion-formating-word-map (make-sparse-keymap))
  (define-key hm--html-noregion-formating-word-map
    "b" 'hm--html-add-bold)
  (define-key hm--html-noregion-formating-word-map
    "i" 'hm--html-add-italic)
  (define-key hm--html-noregion-formating-word-map
    "u" 'hm--html-add-underline)
  (define-key hm--html-noregion-formating-word-map
    "t" 'html-add-fixed)
  (define-key hm--html-noregion-formating-word-map
    "s" 'hm--html-add-strikethru)
  (define-key hm--html-noregion-formating-word-map
    "\C-p" 'hm--html-add-superscript)
  (define-key hm--html-noregion-formating-word-map
    "\C-b" 'hm--html-add-subscript)
  (define-key hm--html-noregion-formating-word-map
    "e" 'html-add-emphasized)
  (define-key hm--html-noregion-formating-word-map
    "\C-s" 'html-add-strong)
  )

(defvar hm--html-region-formating-word-map nil
  "Region sub keymap for inserting word text formating elements.")

(if hm--html-region-formating-word-map
    ()
  (setq hm--html-region-formating-word-map (make-sparse-keymap))
  (define-key hm--html-region-formating-word-map
    "b" 'hm--html-add-bold-to-region)
  (define-key hm--html-region-formating-word-map
    "i" 'hm--html-add-italic-to-region)
  (define-key hm--html-region-formating-word-map
    "u" 'hm--html-add-underline-to-region)
  (define-key hm--html-region-formating-word-map
    "t" 'hm--html-add-fixed-to-region)
  (define-key hm--html-region-formating-word-map
    "s" 'hm--html-add-strikethru-to-region)
  (define-key hm--html-region-formating-word-map
    "\C-p" 'hm--html-add-superscript-to-region)
  (define-key hm--html-region-formating-word-map
    "\C-b" 'hm--html-add-subscript-to-region)
  (define-key hm--html-region-formating-word-map
    "e" 'hm--html-add-emphasized-to-region)
  (define-key hm--html-region-formating-word-map
    "\C-s" 'hm--html-add-strong-to-region)
  )

;(defvar hm--html-noregion-header-map nil
;  "Noregion sub keymap for inserting header elements.")

;(if hm--html-noregion-header-map
;    ()
;  (setq hm--html-noregion-header-map (make-sparse-keymap))
;  )

;(defvar hm--html-region-header-map nil
;  "Region sub keymap for inserting header elements.")

;(if hm--html-region-header-map
;    ()
;  (setq hm--html-region-header-map (make-sparse-keymap))
;  )

;(defvar hm--html-noregion-head-map nil
;  "Noregion sub keymap for inserting head elements.")

;(if hm--html-noregion-head-map
;    ()
;  (setq hm--html-noregion-head-map (make-sparse-keymap))
;  )

;(defvar hm--html-region-head-map nil
;  "Region sub keymap for inserting head elements.")

;(if hm--html-region-head-map
;    ()
;  (setq hm--html-region-head-map (make-sparse-keymap))
;  )

;(defvar hm--html-noregion-list-map nil
;  "Noregion sub keymap for inserting lists.")

;(if hm--html-noregion-list-map
;    ()
;  (setq hm--html-noregion-list-map (make-sparse-keymap))
;  )

;(defvar hm--html-region-list-map nil
;  "Region sub keymap for inserting lists.")

;(if hm--html-region-list-map
;    ()
;  (setq hm--html-region-list-map (make-sparse-keymap))
;  )

(defvar hm--html-noregion-graphics-map nil
  "Noregion sub keymap for inserting graphics (images).")

(if hm--html-noregion-graphics-map
    ()
  (setq hm--html-noregion-graphics-map (make-sparse-keymap))
  )

(defvar hm--html-region-graphics-map nil
  "Region sub keymap for inserting graphics (images).")

(if hm--html-region-graphics-map
    ()
  (setq hm--html-region-graphics-map (make-sparse-keymap))
  )

(defvar hm--html-noregion-text-elements-map nil
  "Noregion sub keymap for inserting text elements.")

(if hm--html-noregion-text-elements-map
    ()
  (setq hm--html-noregion-text-elements-map (make-sparse-keymap))
  )

(defvar hm--html-region-text-elements-map nil
  "Region sub keymap for inserting text elements.")

(if hm--html-region-text-elements-map
    ()
  (setq hm--html-region-text-elements-map (make-sparse-keymap))
  )

(defvar hm--html-noregion-forms-map nil
  "Noregion sub keymap for inserting forms.")

(if hm--html-noregion-forms-map
    ()
  (setq hm--html-noregion-forms-map (make-sparse-keymap))
  )

(defvar hm--html-region-forms-map nil
  "Region sub keymap for inserting forms.")

(if hm--html-region-forms-map
    ()
  (setq hm--html-region-forms-map (make-sparse-keymap))
  )

(defvar hm--html-region-sub-map-1 nil
  "Region sub keymap for the html-mode.")

(if hm--html-region-sub-map-1
    ()
  (setq hm--html-region-sub-map-1 (make-sparse-keymap))
;  (define-key hm--html-region-sub-map-1 "\C-e" hm--html-region-entity-map)
;  (define-key hm--html-region-sub-map-1 "\C-l" hm--html-region-logical-map)
;  (define-key hm--html-region-sub-map-1 "\C-p" hm--html-region-physical-map)
;  (define-key hm--html-region-sub-map-1 "\C-h" hm--html-region-header-map)
  (define-key hm--html-region-sub-map-1 "\C-o" hm--html-region-forms-map)
;  (define-key hm--html-region-sub-map-1 "\C-l" hm--html-region-list-map)
  (define-key hm--html-region-sub-map-1 "\C-a" hm--html-region-anchor-map)
  (define-key hm--html-region-sub-map-1 "\C-i" hm--html-region-graphics-map)
  (define-key hm--html-region-sub-map-1 
    "\C-t" hm--html-region-text-elements-map)
;  (define-key hm--html-region-sub-map-1 "\C-b" hm--html-region-head-map)
  (define-key hm--html-region-sub-map-1 "\C-f" hm--html-region-frame-map)
  (define-key hm--html-region-sub-map-1 "\C-s" hm--html-region-structure-map)
  (define-key hm--html-region-sub-map-1 
    "\C-p" hm--html-region-formating-paragraph-map)
  (define-key hm--html-region-sub-map-1
    "\C-w" hm--html-region-formating-word-map)
  )

(defvar hm--html-noregion-sub-map-1 nil
  "Noregion sub keymap for the html-mode.")

(if hm--html-noregion-sub-map-1
    ()
  (setq hm--html-noregion-sub-map-1 (make-sparse-keymap))
  

;  (define-key hm--html-noregion-sub-map-1 "\C-e" hm--html-noregion-entity-map)
;  (define-key hm--html-noregion-sub-map-1 
;    "\C-l" hm--html-noregion-logical-map)
;  (define-key hm--html-noregion-sub-map-1 
;    "\C-p" hm--html-noregion-physical-map)
;  (define-key hm--html-noregion-sub-map-1 
;	[(control h)] hm--html-noregion-header-map)
  (define-key hm--html-noregion-sub-map-1 "\C-o" hm--html-noregion-forms-map)
;  (define-key hm--html-noregion-sub-map-1 "\C-l" hm--html-noregion-list-map)
  (define-key hm--html-noregion-sub-map-1 "\C-a" hm--html-noregion-anchor-map)
  (define-key hm--html-noregion-sub-map-1 
    [(control i)] hm--html-noregion-graphics-map)
  (define-key hm--html-noregion-sub-map-1 
    "\C-t" hm--html-noregion-text-elements-map)
;  (define-key hm--html-noregion-sub-map-1 "\C-b" hm--html-noregion-head-map)
  (define-key hm--html-noregion-sub-map-1 "\C-f" hm--html-noregion-frame-map)
  (define-key hm--html-noregion-sub-map-1 
    "\C-s" hm--html-noregion-structure-map)
  (define-key hm--html-noregion-sub-map-1
    "\C-p" hm--html-noregion-formating-paragraph-map)
  (define-key hm--html-noregion-sub-map-1
    "\C-w" hm--html-noregion-formating-word-map)
  )

(defvar hm--html-region-sub-map nil
  "Region sub keymap for the html-mode.")

(if hm--html-region-sub-map
    ()
  (setq hm--html-region-sub-map (make-sparse-keymap))
;  (define-key hm--html-region-sub-map "\C-n" hm--html-noregion-sub-map-1)
;  (define-key hm--html-region-sub-map "\C-r" hm--html-region-sub-map-1)
  (define-key hm--html-region-sub-map "\M-n" hm--html-noregion-sub-map-1)
  (define-key hm--html-region-sub-map "\M-r" hm--html-region-sub-map-1)

  (if (adapt-emacs19p)
      (map-keymap '(lambda (key-description-list binding)
		     (define-key hm--html-region-sub-map
		       (single-key-description key-description-list) binding))
		  hm--html-region-sub-map-1)
    (map-keymap '(lambda (key-description-list binding)
		   (define-key hm--html-region-sub-map
		     key-description-list binding))
		hm--html-region-sub-map-1)
    )
  )

(defvar hm--html-noregion-sub-map nil
  "Noregion keymap for the html-mode.")

(if hm--html-noregion-sub-map
    ()
  (setq hm--html-noregion-sub-map (make-sparse-keymap))
;  (define-key hm--html-noregion-sub-map "\C-n" hm--html-noregion-sub-map-1)
;  (define-key hm--html-noregion-sub-map "\C-r" hm--html-region-sub-map-1)
  (define-key hm--html-noregion-sub-map "\M-n" hm--html-noregion-sub-map-1)
  (define-key hm--html-noregion-sub-map "\M-r" hm--html-region-sub-map-1)

  (if (adapt-emacs19p)
      (map-keymap '(lambda (key-description-list binding)
		     (define-key hm--html-region-sub-map
		       (single-key-description key-description-list) binding))
		  hm--html-region-sub-map-1)
    (map-keymap '(lambda (key-description-list binding)
		   (define-key hm--html-noregion-sub-map
		     key-description-list binding))
		hm--html-noregion-sub-map-1)
    )
  )

(defvar hm--html-mode-map nil
  "Normal and noregion keymap for the html-mode.")

(if hm--html-mode-map
    ()
  (setq hm--html-mode-map (make-sparse-keymap))
  (define-key hm--html-mode-map "\C-c" hm--html-noregion-sub-map)
  (if (adapt-xemacsp)
      (define-key hm--html-mode-map '(button3) 'hm--popup-html-menu)
    (define-key hm--html-mode-map [mouse-3] 'hm--popup-html-menu))
  (if hm--html-bind-latin-1-char-entities
      (progn
	(define-key hm--html-mode-map [adiaeresis] 'hm--html_ae)
	(define-key hm--html-mode-map [odiaeresis] 'hm--html_oe) 
	(define-key hm--html-mode-map [udiaeresis] 'hm--html_ue)
  	(define-key hm--html-mode-map [aring]      'hm--html_aa)
	(define-key hm--html-mode-map [Adiaeresis] 'hm--html_Ae) 
	(define-key hm--html-mode-map [Odiaeresis] 'hm--html_Oe) 
	(define-key hm--html-mode-map [Udiaeresis] 'hm--html_Ue)
  	(define-key hm--html-mode-map [Aring]      'hm--html_Aa)
	(define-key hm--html-mode-map [ediaeresis] 'hm--html_ediaeresis) 
	(define-key hm--html-mode-map [Ediaeresis] 'hm--html_Ediaeresis) 
	(define-key hm--html-mode-map [idiaeresis] 'hm--html_idiaeresis) 
	(define-key hm--html-mode-map [Idiaeresis] 'hm--html_Idiaeresis) 
	(define-key hm--html-mode-map [ssharp] 'hm--html_sz) 
	(define-key hm--html-mode-map [aacute] 'hm--html_aacute) 
	(define-key hm--html-mode-map [eacute] 'hm--html_eacute) 
	(define-key hm--html-mode-map [iacute] 'hm--html_iacute) 
	(define-key hm--html-mode-map [oacute] 'hm--html_oacute) 
	(define-key hm--html-mode-map [uacute] 'hm--html_uacute) 
	(define-key hm--html-mode-map [Aacute] 'hm--html_Aacute) 
	(define-key hm--html-mode-map [Eacute] 'hm--html_Eacute) 
	(define-key hm--html-mode-map [Iacute] 'hm--html_Iacute) 
	(define-key hm--html-mode-map [Oacute] 'hm--html_Oacute) 
	(define-key hm--html-mode-map [Uacute] 'hm--html_Uacute) 
	(define-key hm--html-mode-map [agrave] 'hm--html_agrave) 
	(define-key hm--html-mode-map [egrave] 'hm--html_egrave) 
	(define-key hm--html-mode-map [igrave] 'hm--html_igrave) 
	(define-key hm--html-mode-map [ograve] 'hm--html_ograve) 
	(define-key hm--html-mode-map [ugrave] 'hm--html_ugrave) 
	(define-key hm--html-mode-map [Agrave] 'hm--html_Agrave) 
	(define-key hm--html-mode-map [Egrave] 'hm--html_Egrave) 
	(define-key hm--html-mode-map [Igrave] 'hm--html_Igrave) 
	(define-key hm--html-mode-map [Ograve] 'hm--html_Ograve) 
	(define-key hm--html-mode-map [Ugrave] 'hm--html_Ugrave) 
	(define-key hm--html-mode-map [ccedilla] 'hm--html_ccedilla) 
	(define-key hm--html-mode-map [Ccedilla] 'hm--html_Ccedilla) 
	(define-key hm--html-mode-map [acircumflex] 'hm--html_acircumflex) 
	(define-key hm--html-mode-map [ecircumflex] 'hm--html_ecircumflex) 
	(define-key hm--html-mode-map [icircumflex] 'hm--html_icircumflex) 
	(define-key hm--html-mode-map [ocircumflex] 'hm--html_ocircumflex) 
	(define-key hm--html-mode-map [ucircumflex] 'hm--html_ucircumflex) 
	(define-key hm--html-mode-map [Acircumflex] 'hm--html_Acircumflex) 
	(define-key hm--html-mode-map [Ecircumflex] 'hm--html_Ecircumflex) 
	(define-key hm--html-mode-map [Icircumflex] 'hm--html_Icircumflex) 
	(define-key hm--html-mode-map [Ocircumflex] 'hm--html_Ocircumflex) 
	(define-key hm--html-mode-map [Ucircumflex] 'hm--html_Ucircumflex)
	(define-key hm--html-mode-map [atilde] 'hm--html_atilde) 
	(define-key hm--html-mode-map [otilde] 'hm--html_otilde) 
	(define-key hm--html-mode-map [ntilde] 'hm--html_ntilde) 
	(define-key hm--html-mode-map [Atilde] 'hm--html_Atilde) 
	(define-key hm--html-mode-map [Otilde] 'hm--html_Otilde) 
	(define-key hm--html-mode-map [Ntilde] 'hm--html_Ntilde) 
	(define-key hm--html-mode-map [eth] 'hm--html_eth) 
	(define-key hm--html-mode-map [ETH] 'hm--html_Eth) 
	(define-key hm--html-mode-map [thorn] 'hm--html_thorn) 
	(define-key hm--html-mode-map [THORN] 'hm--html_Thorn) 
	))
  (define-key hm--html-mode-map "<" 'hm--html-smart-less-than)
  (define-key hm--html-mode-map ">" 'hm--html-smart-greater-than)
  (define-key hm--html-mode-map "&" 'html-ampersand)
  )

(defvar hm--html-region-mode-map nil
  "Region keymap for the html-mode.")

(if hm--html-region-mode-map
    ()
  (setq hm--html-region-mode-map (make-sparse-keymap))
  (define-key hm--html-region-mode-map "\C-c" hm--html-region-sub-map)
  (if (adapt-xemacsp)
      (define-key hm--html-region-mode-map 
	'(button3) 'hm--popup-html-menu-region)
    (define-key hm--html-region-mode-map 
	[mouse-3] 'hm--popup-html-menu-region))
  ;; It maybe a better idea to set the following to undefine in this list...
  (if hm--html-bind-latin-1-char-entities
      (progn
	(define-key hm--html-region-mode-map [adiaeresis] 'hm--html_ae)
	(define-key hm--html-region-mode-map [odiaeresis] 'hm--html_oe) 
	(define-key hm--html-region-mode-map [udiaeresis] 'hm--html_ue)
	(define-key hm--html-region-mode-map [aring]      'hm--html_aa)
	(define-key hm--html-region-mode-map [Adiaeresis] 'hm--html_Ae) 
	(define-key hm--html-region-mode-map [Odiaeresis] 'hm--html_Oe) 
	(define-key hm--html-region-mode-map [Udiaeresis] 'hm--html_Ue)
 	(define-key hm--html-region-mode-map [Aring]      'hm--html_Aa)
	(define-key hm--html-region-mode-map 
	  [ediaeresis] 'hm--html_ediaeresis) 
	(define-key hm--html-region-mode-map 
	  [Ediaeresis] 'hm--html_Ediaeresis) 
	(define-key hm--html-region-mode-map 
	  [idiaeresis] 'hm--html_idiaeresis) 
	(define-key hm--html-region-mode-map 
	  [Idiaeresis] 'hm--html_Idiaeresis) 
	(define-key hm--html-region-mode-map [ssharp] 'hm--html_sz) 
	(define-key hm--html-region-mode-map [aacute] 'hm--html_aacute) 
	(define-key hm--html-region-mode-map [eacute] 'hm--html_eacute) 
	(define-key hm--html-region-mode-map [iacute] 'hm--html_iacute) 
	(define-key hm--html-region-mode-map [oacute] 'hm--html_oacute) 
	(define-key hm--html-region-mode-map [uacute] 'hm--html_uacute) 
	(define-key hm--html-region-mode-map [Aacute] 'hm--html_Aacute) 
	(define-key hm--html-region-mode-map [Eacute] 'hm--html_Eacute) 
	(define-key hm--html-region-mode-map [Iacute] 'hm--html_Iacute) 
	(define-key hm--html-region-mode-map [Oacute] 'hm--html_Oacute) 
	(define-key hm--html-region-mode-map [Uacute] 'hm--html_Uacute) 
	(define-key hm--html-region-mode-map [agrave] 'hm--html_agrave) 
	(define-key hm--html-region-mode-map [egrave] 'hm--html_egrave) 
	(define-key hm--html-region-mode-map [igrave] 'hm--html_igrave) 
	(define-key hm--html-region-mode-map [ograve] 'hm--html_ograve) 
	(define-key hm--html-region-mode-map [ugrave] 'hm--html_ugrave) 
	(define-key hm--html-region-mode-map [Agrave] 'hm--html_Agrave) 
	(define-key hm--html-region-mode-map [Egrave] 'hm--html_Egrave) 
	(define-key hm--html-region-mode-map [Igrave] 'hm--html_Igrave) 
	(define-key hm--html-region-mode-map [Ograve] 'hm--html_Ograve) 
	(define-key hm--html-region-mode-map [Ugrave] 'hm--html_Ugrave) 
	(define-key hm--html-region-mode-map [ccedilla] 'hm--html_ccedilla) 
	(define-key hm--html-region-mode-map [Ccedilla] 'hm--html_Ccedilla) 
	(define-key hm--html-region-mode-map 
	  [acircumflex] 'hm--html_acircumflex) 
	(define-key hm--html-region-mode-map 
	  [ecircumflex] 'hm--html_ecircumflex) 
	(define-key hm--html-region-mode-map 
	  [icircumflex] 'hm--html_icircumflex) 
	(define-key hm--html-region-mode-map 
	  [ocircumflex] 'hm--html_ocircumflex) 
	(define-key hm--html-region-mode-map 
	  [ucircumflex] 'hm--html_ucircumflex) 
	(define-key hm--html-region-mode-map 
	  [Acircumflex] 'hm--html_Acircumflex) 
	(define-key hm--html-region-mode-map 
	  [Ecircumflex] 'hm--html_Ecircumflex) 
	(define-key hm--html-region-mode-map 
	  [Icircumflex] 'hm--html_Icircumflex) 
	(define-key hm--html-region-mode-map 
	  [Ocircumflex] 'hm--html_Ocircumflex) 
	(define-key hm--html-region-mode-map 
	  [Ucircumflex] 'hm--html_Ucircumflex)
	(define-key hm--html-region-mode-map [atilde] 'hm--html_atilde) 
	(define-key hm--html-region-mode-map [otilde] 'hm--html_otilde) 
	(define-key hm--html-region-mode-map [ntilde] 'hm--html_ntilde) 
	(define-key hm--html-region-mode-map [Atilde] 'hm--html_Atilde) 
	(define-key hm--html-region-mode-map [Otilde] 'hm--html_Otilde) 
	(define-key hm--html-region-mode-map [Ntilde] 'hm--html_Ntilde) 
	(define-key hm--html-region-mode-map [eth] 'hm--html_eth) 
	(define-key hm--html-region-mode-map [ETH] 'hm--html_Eth) 
	(define-key hm--html-region-mode-map [thorn] 'hm--html_thorn) 
	(define-key hm--html-region-mode-map [THORN] 'hm--html_Thorn) 
	))
  (define-key hm--html-region-mode-map "<" 'html-less-than)
  (define-key hm--html-region-mode-map ">" 'html-greater-than)
  (define-key hm--html-region-mode-map "&" 'html-ampersand)
  )


;;; -----

;(use-local-map hm--html-mode-map)

(or (assq 'hm--html-region-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((hm--html-region-mode " Region"))))))



(defun html-region-mode (on)
  "Turns the minor mode html-region-mode on or off.
The function turns the html-region-mode on, if ON is t and off otherwise."
  (if (string= mode-name "HTML")
      (if on
	  ;; html-region-mode on
	  (progn
	    (setq hm--html-region-mode t)
	    (use-local-map hm--html-region-mode-map))
	;; html-region-mode off
	(setq hm--html-region-mode nil)
	(use-local-map hm--html-mode-map))))

