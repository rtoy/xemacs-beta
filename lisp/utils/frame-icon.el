;; frame-icon.el - set up mode-specific icons for each frame under XEmacs

;; Author: Michael Lamoureux <lamour@engin.umich.edu>
;; Keywords: lisp, extensions
;; date created: 8/3/93

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Modified by Bob Weiner <weiner@infodock.com>, 1/13/94
;;   Handle XEmacs 19.8 pixmaps properly. 
;;   Also added in more mode settings and added many new bitmaps.
;;   Renamed from icon.el to frame-icon.el.
;;   Made all definitions start with the same prefix, 'icon-'.
;;   Added a provide clause.
;;
;; Modified by Bob Weiner, 2/24/95, to handle XEmacs 19.12.
;;   Added set of unmap-frame-hook.
;;
;; Modified by Bob Weiner, 7/17/95, to handle XEmacs 19.12.
;;   Changed to use new image-handling protocols and added backwards
;;   compatibility functions for new image functions.
;;
;; Modified by Bob Weiner, 7/18/95.
;;   Added icon-suffix variable so can use .xbm or .xpm icons.
;;
;; Most Icons were extracted from: /export.lcs.mit.edu:/contrib/AIcons
;;

;;; Code:

(defvar icon-directory (concat data-directory "frame-icon/")
  "Directory of icons used by frame-icon.el.")

(defvar icon-suffix ".xbm"
  "Must be .xbm or .xpm, depending on the format of the icons in icon-directory.")

(defconst icon-mode-alist
  '(
    (default . "question")
    ;; For testing
    (fundamental-mode . "match")
    ;;
    (archie-mode . "archie")
    (asm-mode . "nuke")
    (bbdb-mode . "eye")
    (bookmark-menu-mode . "finder")
    (Buffer-menu-mode . "help")
    (c++-mode . "c++")
    (c++-c-mode . "escherknot")
    (c-mode . "c")
    (awk-mode . "escherknot")
    (cvs-mode . "tree")
    (f90-mode . "wizard")
    (xrdb-mode . "RIP")
    ;;
    (calc-edit-mode . "cray")
    (calc-keypad . "cray")
    (calc-mode . "cray")
    (calc-trail-mode . "cray")
    (MacEdit-mode . "cray")
    ;;
    (calendar-mode . "calendar")
    (comint-mode . "terminal")
    (perl-mode . "perl")
    (csh-mode . "manpage2")
    (db-edit-mode . "filing")
    (db-view-mode . "filing")
    (dired-mode . "filing")
    (doctor-mode . "ying-yang-48")
    (edit-faces-mode  . "eye")
    (Edit-options-mode . "swissknife")
    (emacs-lisp-mode . "elisp")
    (fortran-mode    . "RIP")
    (gdb-mode        . "bug-48")
    (gud-mode        . "bug-48")
    (gnus-article-mode . "news")
    (gnus-group-mode . "news")
    (gnus-summary-mode . "news")
    (gopher-mode . "gopher")
    (html-mode . "xmosaic")
    (indented-text-mode . "page")
    (Info-mode . "help")
    (java-mode . "coffee")
    (kotl-mode . "kotl")
    (lisp-interaction-mode . "swissknife")
    (lisp-mode . "lisp")
    (lock-mode . "termlock")
    (mail-mode . "scroll2")
    (Manual-mode . "manpage")
    (man-mode . "manpage")
    (news-reply-mode . "match")
    (outline-mode . "outline")
    (perl-mode . "perl")
    (edit-picture . "splat")
    (pm-fdr-mode . "mail")
    (pm-group-mode . "news")
    (pm-msg-edit-mode . "mail")
    (pm-msgsumm-mode . "mail")
    (pm-mode . "mail")
    (rdb-mode . "question")
    (rmail-mode . "mail")
    (rmail-edit-mode . "mail")
    (rmail-summary-mode . "mail")
    (scheme-interaction-mode . "swissknife")
    (scheme-mode . "lisp")
    (shell-mode . "terminal")
    (sm-manual-mode . "manpage")
    (sql-mode . "sql")
    (tcl-mode . "radioactive")
    (telnet-mode . "rlogin")
    (texinfo-mode . "texinfo")
    (text-mode . "page")
    (unix-apropos-mode . "manpage")
    (ups-mode . "hourglass") ; process listing mode
    (vi-mode   . "stopsign")
    (vip-mode   . "stopsign")
    (vkill-mode . "load")
    (vrml-mode . "drawing")
    (vm-mode . "scroll2")
    (vm-summary-mode . "scroll2")
    (w3-mode . "world")
    (waisq-mode . "library")
    (wordstar-mode . "words")
    (wrolo-mode . "phone")
    ;;
    (ams-tex-mode . "tex-48")
    (foiltex-mode . "tex-48")
    (latex-mode . "tex-48")
    (LaTeX-mode . "tex-48")
    (plain-tex-mode . "tex-48")
    (plain-TeX-mode . "tex-48")
    (slitex-mode . "tex-48")
    (tex-mode . "tex-48")
    )
  "Alist of (major-mode . non-suffixed-icon-file-name) elements.
Used to set frame icons based upon the current major mode.
For use with icon-set-frame.  See also the variable, 'icon-suffix'.")

(or (fboundp 'image-instance-p) (fset 'image-instance-p 'pixmapp))
(or (fboundp 'image-instance-file-name)
    (fset 'image-instance-file-name 'pixmap-file-name))
(or (fboundp 'make-glyph) (fset 'make-glyph 'make-pixmap))

(defun icon-set-frame (iconified-frame)
  "Set icon for selected frame according to the values in icon-mode-alist."
  (save-excursion
    (if (framep iconified-frame)
	(select-frame iconified-frame))
    (let* ((icon-sym (intern (concat "icon-" (symbol-name major-mode))))
	   (pix (and (boundp icon-sym) (symbol-value icon-sym)))
	   (image (or (cdr (assq major-mode icon-mode-alist))
		      (cdr (assq 'default icon-mode-alist))))
	   (image-file (expand-file-name (concat image icon-suffix)
					 icon-directory)))
      (cond ((and (image-instance-p pix)
		  (equal image-file (image-instance-file-name pix)))
	     nil)
	    (t
	     ;; Ensure we don't create a copy of a pixmap already in
	     ;; icon-list due to use in a different major-mode.
	     (setq pix (set icon-sym
			    (car (delq
				  nil
				  (mapcar
				   (function
				    (lambda (pixmap)
				      (if (equal (image-instance-file-name
						  pixmap)
						 image-file)
					  pixmap)))
				   icon-list)))))
	     ;; If pix is nil, there was no entry in icon-list, so create a
	     ;; new one.
	     (or (image-instance-p pix)
		 (setq pix 
		       (glyph-image-instance
			(set icon-sym (make-glyph image-file)))
		       icon-list (cons pix icon-list)))))
      (x-set-frame-icon-pixmap
       (if (framep iconified-frame)
	   iconified-frame
	 ;; unpatched XEmacs 19.6
	 (selected-frame))
       pix))))

(defvar icon-list nil
  "List of existing pixmap objects used as frame icons by frame-icon.el.")

;; Hook in so icons will be selected at iconify time
(if (string-match "XEmacs" emacs-version)
    (add-hook 'unmap-frame-hook 'icon-set-frame) ;; XEmacs 19.12
  (add-hook 'unmap-screen-hook 'icon-set-frame)) ;; Lemacs 19.10


(provide 'frame-icon)

;;; frame-icon.el ends here
