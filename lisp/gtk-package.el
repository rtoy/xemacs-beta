;; gtk-package.el --- GTK version of package-ui
;;
;; Copyright (C) 2000, 2001 William M. Perry
;;
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

;; A GTK version of package-ui.el

(globally-declare-fboundp
 '(gtk-window-new
   gtk-hbox-new gtk-container-add gtk-widget-show-all))

(require 'package-get)
(require 'package-ui)

(defun package-gtk-edit-sites ()
  (let ((window (gtk-window-new 'toplevel))
	(box (gtk-hbox-new nil 5)))
    (gtk-container-add window box)
    (gtk-widget-show-all window)))
