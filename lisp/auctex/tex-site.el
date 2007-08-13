;;; tex-site.el - Select AUC TeX as your tex mode.

;; Copyright (C) 1991 Kresten Krab Thorup 
;; Copyright (C) 1993, 1994, 1997 Per Abrahamsen 

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.auc.dk>
;; Version: 9.7p (XEmacs)
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains variables customized for the local site.

;; It also contains all necessary autoloads, so the user can simple
;; enable AUC TeX by putting (load "tex-site") in his .emacs file,
;; or the administrator can insert it in the site-start.el file.
;;
;; The ideal place for this file is in the `site-lisp' directory.

;;; Code:

(defvar no-doc
  "This function is part of AUC TeX, but has not yet been loaded.
Full documentation will be available after autoloading the function."
  "Documentation for autoload functions.")

;;; Customization:
;;
;; Copy variables you need to change from the start of `tex.el' and
;; insert them here.

;;; Autoloads:

;; This hook will store bibitems when you save a BibTeX buffer.
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)

;; Load from AUC TeX files instead of standard files.
(autoload 'tex-mode "tex" no-doc t)
(autoload 'plain-tex-mode "tex" no-doc t)
(autoload 'tex-insert-quote "tex" no-doc t)
(autoload 'texinfo-mode "tex-info" no-doc t)
(autoload 'latex-mode "latex" no-doc t)

(provide 'tex-site)

;;; tex-site.el ends here
