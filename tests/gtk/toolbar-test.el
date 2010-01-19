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

(require 'gtk-widgets)
(require 'gnome-widgets)

(defvar gnomeified-toolbar
  ;; [CAPTION TOOLTIP ICON CALLBACK ENABLED]
  '(["Open" "Open a file" new toolbar-open t]
    ["Dired" "Edit a directory" open toolbar-dired t]
    ["Save" "Save buffer" save toolbar-save t]
    ["Print" "Print Buffer" print toolbar-print t]
    ["Cut" "Kill region" cut toolbar-cut t]
    ["Copy" "Copy region" copy toolbar-copy t]
    ["Paste" "Paste from clipboard" paste toolbar-paste t]
    ["Undo" "Undo edit" undo toolbar-undo t]
    ["Spell" "Check spelling" spellcheck toolbar-ispell t]
    ["Replace" "Search & Replace" srchrpl toolbar-replace t]
    ["Mail" "Read mail" mail toolbar-mail t]
    ; info
    ; compile
    ; debug
    ; news
    ))

(setq x (gtk-toolbar-new 'horizontal 'both))
(gnome-app-set-toolbar (frame-property nil 'shell-widget) x)

(mapc (lambda (descr)
	(gtk-toolbar-append-item x
				 (aref descr 0)
				 (aref descr 1)
				 ""
				 (gnome-stock-pixmap-widget-new x (aref descr 2))
				 `(lambda (&rest ignored)
				    (,(aref descr 3)))))
      gnomeified-toolbar)
