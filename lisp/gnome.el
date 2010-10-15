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

(globally-declare-fboundp
 '(gtk-type-from-name
   gtk-import-function-internal
   gtk-call-function))

(defvar gnome-init-called nil)

(defun gnome-init (app-id app-version argv)
  (mapc 'dll-load
	'("libgnomesupport.so"
	  "libgnome.so"
	  "libgnomeui.so"
	  "libesd.so"
	  "libaudiofile.so"
	  "libart_lgpl.so"))
  (if (and (not (noninteractive)) (not gnome-init-called)
	   (= (gtk-type-from-name "GnomeApp") 0))      
      (prog1
	  (gtk-call-function (gtk-import-function-internal
			      'gint "gnome_init" '(GtkString GtkString gint GtkArrayOfString))
			     (list app-id app-version (length argv) argv))
	(setq gnome-init-called t))))

(require 'gnome-widgets)
(provide 'gnome)
