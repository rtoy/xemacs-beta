;;; europe-theme.el --- Settings for European users

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Jan Vroonhof <jan@xemacs.org>

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;;autoload 
(deftheme europe
  "Settings for European users."
  :set-variable-settings
    "This variable has a value appropriate for European users."
  :set-variable-settings
    "This has been forceed to the value appropriate for European users.")

(custom-theme-set-variables 'europe
   '(sentence-end-double-space nil)
   '(ps-paper-type (quote a4)))

(provide-theme 'europe)
