;;; mule-x-init.el --- initialization code for X Windows under MULE -*- coding: iso-2022-7bit; -*-
;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1996, 2002 Ben Wing <ben@xemacs.org>

;; Author: various
;; Keywords: mule X11

;; This file is part of XEmacs.
;;
;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;;; Work around what is arguably a Sun CDE bug.

;; #### This is unused, apparently.
(defun x-use-halfwidth-roman-font (fullwidth-charset roman-registry)
  "Maybe set charset registry of the 'ascii charset to ROMAN-REGISTRY.

Do this only if:
 - the current display is an X device
 - the displayed width of FULLWIDTH-CHARSET is twice the displayed
   width of the 'ascii charset, but only when using ROMAN-REGISTRY.

Traditionally, Asian characters have been displayed so that they
occupy exactly twice the screen space of ASCII (`halfwidth')
characters.  On many systems, e.g. Sun CDE systems, this can only be
achieved by using a national variant roman font to display ASCII."
  (flet ((charset-font-width (charset)
	   (font-instance-width
	    (face-font-instance 'default (selected-device) charset)))
	 
	 (twice-as-wide (cs1 cs2)
	   (let ((width1 (charset-font-width cs1))
		 (width2 (charset-font-width cs2)))
	     (and width1 width2 (eq (+ width1 width1) width2)))))

    (when (eq 'x (device-type))
      (let ((original-registries (charset-registries 'ascii)))
        (condition-case nil
            (unless (twice-as-wide 'ascii fullwidth-charset)
              (set-charset-registries 'ascii (vector roman-registry))
              (unless (twice-as-wide 'ascii fullwidth-charset)
                ;; Restore if roman-registry didn't help
                (set-charset-registries 'ascii original-registries)))
          (error (set-charset-registries 'ascii original-registries)))))))

;;;;
