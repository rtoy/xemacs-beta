;;; x-scrollbar.el --- scrollbar resourcing and such.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when X and menubar support is compiled in).

;;; Code:

(globally-declare-fboundp
 (unless (valid-device-type-p 'x) 
   '(x-init-specifier-from-resources x-get-resource)))

(globally-declare-boundp
 (unless (featurep 'scrollbar)
   '(scrollbar-width scrollbar-height scrollbar-on-top-p
     scrollbar-on-left-p)))

(defun x-init-scrollbar-from-resources (locale)
  (x-init-specifier-from-resources
   (specifier-fallback scrollbar-width) 'natnum locale
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
       (specifier-fallback scrollbar-width) 'natnum locale
       '("scrollbar.width" . "ScrollBar.Width")))
      
  ;; lather, rinse, repeat.
  (x-init-specifier-from-resources
   (specifier-fallback scrollbar-height) 'natnum locale
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
       (specifier-fallback scrollbar-height) 'natnum locale
       '("scrollbar.height" . "ScrollBar.Height")))

  ;; Now do ScrollBarPlacement.scrollBarPlacement
  (let ((case-fold-search t)
	(resval (x-get-resource "ScrollBarPlacement" "scrollBarPlacement"
				'string locale nil 'warn)))
    (cond
     ((null resval))
     ((string-match "^top[_-]left$" resval)
      (set-specifier scrollbar-on-top-p t locale)
      (set-specifier scrollbar-on-left-p t locale))
     ((string-match "^top[_-]right$" resval)
      (set-specifier scrollbar-on-top-p t locale)
      (set-specifier scrollbar-on-left-p nil locale))
     ((string-match "^bottom[_-]left$" resval)
      (set-specifier scrollbar-on-top-p nil locale)
      (set-specifier scrollbar-on-left-p t locale))
     ((string-match "^bottom[_-]right$" resval)
      (set-specifier scrollbar-on-top-p nil locale)
      (set-specifier scrollbar-on-left-p nil locale))
     (t
      (display-warning 'resource
	(format "Illegal value '%s' for scrollBarPlacement resource" resval)))))

)

;;; x-scrollbar.el ends here
