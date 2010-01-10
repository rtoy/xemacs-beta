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

(defvar gtk-torture-test-toolbar-open-active-p t)

(defvar gtk-torture-test-toolbar
  '([toolbar-file-icon
     (lambda ()
       (setq gtk-torture-test-toolbar-open-active-p (not gtk-torture-test-toolbar-open-active-p)))
     gtk-torture-test-toolbar-open-active-p
     "Dynamic enabled-p slot... broken in XEmacs 21.1.x"]
    [:size 35 :style 3d]
    [toolbar-folder-icon toolbar-dired t "Edit a directory"]
    [:size 35 :style 2d]
    [toolbar-news-icon toolbar-news t "Read news"]
    nil
    [toolbar-info-icon toolbar-info t "Info documentation"]
    ))

(defun gtk-torture-test-toolbar ()
  (interactive)
  (switch-to-buffer (get-buffer-create "Toolbar testing"))
  (set-specifier default-toolbar gtk-torture-test-toolbar (current-buffer))
  (set-specifier default-toolbar-visible-p t (current-buffer)))
