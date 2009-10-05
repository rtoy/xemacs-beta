;; This file is part of XEmacs.
;;
;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
;; Boston, MA 02111-1301, USA.  */

(require 'gtk-extra)

(gtk-define-test
 "Color Combo" extra color-combo nil
 (let ((combo (gtk-color-combo-new)))
   (gtk-box-pack-start window combo nil nil 0)))

(gtk-define-test
 "Directory Tree" extra dirtree nil
 (let ((dir (gtk-dir-tree-new)))
   (gtk-box-pack-start window dir nil nil 0)
   (gtk-dir-tree-open-dir dir "/")))

(gtk-define-test
 "File List" extra filelist nil
 (let ((scrolled (gtk-scrolled-window-new nil nil))
       (list (gtk-file-list-new 32 2 "/")))
   (gtk-scrolled-window-add-with-viewport scrolled list)
   (put scrolled 'height 200)
   (gtk-box-pack-start window scrolled t t 0)))

(gtk-define-test
 "Font Combo" extra fontcombo nil
 (let ((fc (gtk-font-combo-new)))
   (gtk-box-pack-start window fc t t 0)))
   
