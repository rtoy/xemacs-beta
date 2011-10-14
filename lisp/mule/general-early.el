;;; general-early.el --- General Mule code that needs to be run early when
;;                       dumping.
;; Copyright (C) 2006 Free Software Foundation
;; Copyright (C) 2010 Ben Wing.

;; Author: Aidan Kehoe

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; Utility function that is used by various script files and will be
;; removed in general-late.
(defun setup-case-pairs (charset pairs)
  ;; Under Unicode-internal, don't do anything here, because we set up all
  ;; the case pairs at once in uni-case-conv.el
  (unless (and (featurep 'unicode-internal)
	       (featurep 'use-unidata-case-tables))
    (loop 
      for (uc lc) in pairs 
      with table = (standard-case-table)
      do (put-case-table-pair
	  (make-char charset uc) (make-char charset lc) table))))
