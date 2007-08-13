;;; cc-auto.el --- autoloads for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.11
;; Keywords:   c languages oop

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(autoload 'c-mode "cc-mode"
  "Major mode for editing K&R and ANSI C code." t)

(autoload 'c++-mode "cc-mode"
  "Major mode for editing C++ code." t)

(autoload 'objc-mode "cc-mode"
  "Major mode for editing Objective C code." t)

(autoload 'java-mode "cc-mode"
  "Major mode for editing Java code." t)

(autoload 'c-add-style "cc-styles"
  "Adds a style to `c-style-alist', or updates an existing one." t)


;; This comment was here before me:
;;
;;     cmacexp is lame because it uses no preprocessor symbols.  It
;;     isn't very extensible either -- hardcodes /lib/cpp.
;;
;; I add it here only because c-mode had it -- BAW
;;
(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)


(provide 'cc-auto)
;;; cc-auto.el ends here
