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

(gtk-define-test
 "Embedded XEmacs frame" xemacs-frame t
 (setq window (gtk-window-new 'toplevel))
 (let ((table (gtk-table-new 5 3 nil))
       (label nil)
       (entry nil)
       (frame (gtk-frame-new "Type mail message here...")))
   (gtk-container-add window table)

   (setq label (gtk-label-new "To: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 0 1 nil nil 0 0)
   (gtk-table-attach table entry 1 2 0 1 '(fill) '(fill) 0 0)

   (setq label (gtk-label-new "CC: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 1 2 nil nil 0 0)
   (gtk-table-attach table entry 1 2 1 2 '(fill) '(fill) 0 0)

   (setq label (gtk-label-new "Subject: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 2 3 nil nil 0 0)
   (gtk-table-attach table entry 1 2 2 3 '(fill) '(fill) 0 0)

   (gtk-table-attach table frame 0 2 3 4 '(expand fill) '(expand fill) 5 5)
   
   (gtk-widget-show-all window)
   (gdk-flush)
   (make-frame (list 'window-id frame
		     'unsplittable t
		     'menubar-visible-p nil
		     'default-toolbar-visible-p nil))))
