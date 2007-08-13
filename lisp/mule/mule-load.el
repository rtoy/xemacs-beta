;;; mule-load.el --- Load up all pre-loaded Mule Lisp files.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Commentary:

;; This file is obsoleted by dumped-lisp.el and will be removed shortly.

;; (load-gc) is defined in loadup.el

;; Load these two first, to set up the most basic stuff.
(load-gc "mule-charset")
(load-gc "mule-coding")
;; Handle I/O of files with extended characters.
(load-gc "mule-files")
;; Load the remaining basic files.
(load-gc "mule-category")
(load-gc "mule-ccl")
(load-gc "mule-misc")
(load-gc "kinsoku")
(when (featurep 'x)
  (load-gc "mule-x-init"))
(load-gc "mule-cmds") ; to sync with Emacs 20.1

;; after this goes the specific lisp routines for a particular input system
;; 97.2.5 JHod Shouldn't these go into a site-load file to allow site
;; or user switching of input systems???
;(if (featurep 'wnn)
;    (progn
;      (load-gc "egg")
;      (load-gc "egg-wnn")
;      (setq egg-default-startup-file "eggrc-wnn")))

;; (if (and (boundp 'CANNA) CANNA)
;;     (load-gc "canna")
;;   )

;; Now load files to set up all the different languages/environments
;; that Mule knows about.

(load-gc "arabic-hooks")
(load-gc "language/chinese")
(load-gc "language/cyrillic")
(load-gc "language/english")
(load-gc "ethiopic-hooks")
(load-gc "language/european")
(load-gc "language/greek")
(load-gc "hebrew-hooks")
(load-gc "language/japanese")
(load-gc "language/korean")
(load-gc "language/misc-lang")
(load-gc "language/thai")
(load-gc "vietnamese-hooks-1")
(load-gc "vietnamese-hooks-2")

;; Set up the XEmacs environment for Mule.
;; Assumes the existence of various stuff above.
(load-gc "mule-init")

;; Enable Mule capability for Gnus, mail, etc...
;; Moved to sunpro-load.el - the default only for Sun.
;;(load-gc "mime-setup")

;;; mule-load.el ends here
