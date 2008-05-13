;;; behavior-defs.el --- definitions of specific behaviors

;; Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

;; Author: Ben Wing
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Authorship:

;; Created July 2000 by Ben Wing.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

(define-behavior-group 'tty)
(define-behavior-group 'toolbars)
(define-behavior-group 'menus)
(define-behavior-group 'mouse)
(define-behavior-group 'editing)
(define-behavior-group 'keyboard)
(define-behavior-group 'files)
(define-behavior-group 'games)
(define-behavior-group 'processes)
(define-behavior-group 'display)
(define-behavior-group 'programming)
(define-behavior-group 'international)
(define-behavior-group 'buffers-and-windows)
(define-behavior-group 'internet)

(define-behavior 'compose-mail
  "Not documented."
  :group 'internet
  :commands
  '(["Send %_Mail..." compose-mail]))
