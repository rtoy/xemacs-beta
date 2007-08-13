;;; x-scrollbar.el --- scrollbar resourcing and such.

;;; Copyright (C) 1995 Sun Microsystems.
;;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: Ben Wing <wing@666.com>

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun x-init-scrollbar-from-resources (locale)
  (x-init-specifier-from-resources
   scrollbar-width 'natnum locale
   '("scrollBarWidth" . "ScrollBarWidth")
   ;; The name strings are wrong, but the scrollbar name is
   ;; non-deterministic so it is a poor way to set a resource
   ;; for the scrollbar anyhow.
   (cond ((featurep 'athena-scrollbars)
	  '("scrollbar.thickness" . "ScrollBar.Thickness"))
	 ((featurep 'lucid-scrollbars)
	  '("scrollbar.width" . "XlwScrollBar.Width"))
	 ((featurep 'motif-scrollbars)
	  '("scrollbar.width" . "XmScrollBar.Width"))))
  ;; Athena scrollbars accept either 'thickness' or 'width'.
  ;; If any of the previous resources succeeded, the following
  ;; call does nothing; so there's no harm in doing it all the
  ;; time.
  (if (featurep 'athena-scrollbars)
      (x-init-specifier-from-resources
       scrollbar-width 'natnum locale
       '("scrollbar.width" . "ScrollBar.Width")))
      
  ;; lather, rinse, repeat.
  (x-init-specifier-from-resources
   scrollbar-height 'natnum locale
   '("scrollBarHeight" . "ScrollBarHeight")
   ;; The name strings are wrong, but the scrollbar name is
   ;; non-deterministic so it is a poor way to set a resource
   ;; for the scrollbar anyhow.
   (cond ((featurep 'athena-scrollbars)
	  '("scrollbar.thickness" . "ScrollBar.Thickness"))
	 ((featurep 'lucid-scrollbars)
	  '("scrollbar.height" . "XlwScrollBar.Height"))
	 ((featurep 'motif-scrollbars)
	  '("scrollbar.height" . "XmScrollBar.Height"))))
  ;; Athena scrollbars accept either 'thickness' or 'height'.
  ;; If any of the previous resources succeeded, the following
  ;; call does nothing; so there's no harm in doing it all the
  ;; time.
  (if (featurep 'athena-scrollbars)
      (x-init-specifier-from-resources
       scrollbar-height 'natnum locale
       '("scrollbar.height" . "ScrollBar.Height"))))

