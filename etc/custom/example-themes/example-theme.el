;;; example-theme.el --- An example customize theme

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
;; MA 02111-1301, USA.

;;;autoload 
(deftheme example
  "A sample theme for customize theme support."
  :variable-set-string "This variable has been made an example.")

(custom-theme-load-themes 'example
    'europe)

(custom-theme-set-variables 'example
 '(iswitchb-prompt-newbuffer nil))

(provide-theme 'example)


  
