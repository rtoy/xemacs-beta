;;; behavior-defs.el --- definitions of specific behaviors

;; Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

;; Author: Ben Wing
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;; Define the mwheel behavior here, so it can be disabled by users' init files
;; without dumping mwheel.el.
(define-behavior 'mwheel
  "Enable the use of the mouse wheel, if present.  On by default.

In XEmacs, wheel events arrive as button4/button5 events, and these are
automatically set up to do scrolling in the expected way.  The details of how
the scrolling works can be controlled by `mouse-wheel-scroll-amount',
`mouse-wheel-follow-mouse', `mouse-wheel-progressive-speed',
`mouse-wheel-scroll-up-function', `mouse-wheel-down-function'.  See the
documentation of those variables.

There is normally no need to explicitly configure this behavior.  If as a user
you would like to disable the mouse wheel, call `(disable-behavior 'mwheel)',
or `(mwheel-install 'uninstall)'.  The latter syntax is compatible with GNU
Emacs."
  :group 'mouse
  :short-doc "Mouse wheel support for X Windows"
  :enable 'mwheel-install
  :disable #'(lambda () (mwheel-install -1)))

;; Don't call #'enable-behavior at dump time, that would needlessly force
;; dumping of mwheel.el.
(push 'mwheel enabled-behavior-list)

;;; behavior-defs.el ends here
