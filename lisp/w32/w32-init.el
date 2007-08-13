;;; w32-init.el --- initialization code for win32
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for win32 by: Jonathan Harris

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

(defvar w32-win-initted nil)
(defvar w32-pre-win-initted nil)
(defvar w32-post-win-initted nil)

(defun init-pre-w32-win ()
  "Initialize win32 GUI at startup (pre).  Don't call this."
  (unless w32-pre-win-initted
    (setq w32-pre-win-initted t)))

(defun init-w32-win ()
  "Initialize win32 GUI at startup.  Don't call this."
  (unless w32-win-initted
    (init-pre-w32-win)
    (make-w32-device)
    (init-post-w32-win (selected-console))
    (setq w32-win-initted t)))

(defun init-post-w32-win (console)
  "Initialize win32 GUI at startup (post).  Don't call this."
  (unless w32-post-win-initted
    (setq w32-post-win-initted t)))

