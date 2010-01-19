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

(globally-declare-fboundp
 '(gtk-import-function-internal
   gtk-call-function
   gtk-type-name
   gtk-import-function))

(globally-declare-boundp
 '(gtk-enumeration-info))

(gtk-import-function nil "gdk_flush")

(defun gtk-describe-enumerations ()
  "Show a list of all GtkEnum or GtkFlags objects available from lisp."
  (interactive)
  (set-buffer (get-buffer-create "*GTK Enumerations*"))
  (erase-buffer)
  (let ((separator (make-string (- (window-width) 3) ?-)))
    (maphash (lambda (key val)
	       (insert
		separator "\n"
		(if (stringp key)
		    key
		  (gtk-type-name key)) "\n")
	       (mapc (lambda (cell)
		       (insert (format "\t%40s == %d\n" (car cell) (cdr cell)))) val))
	     gtk-enumeration-info))
  (goto-char (point-min))
  (display-buffer (current-buffer)))
