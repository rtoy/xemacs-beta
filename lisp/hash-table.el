;;; hash-table.el --- hash-table utility functions

;; Copyright (C) 2000 Ben Wing.

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

(defun hash-table-key-list (hash-table)
  "Return a list of all keys in HASH-TABLE."
  (let (lis)
    (maphash #'(lambda (key val)
		 (push key lis))
	     hash-table)
    (nreverse lis)))

(defun hash-table-value-list (hash-table)
  "Return a list of all values in HASH-TABLE."
  (let (lis)
    (maphash #'(lambda (key val)
		 (push val lis))
	     hash-table)
    (nreverse lis)))

(defun hash-table-key-value-alist (hash-table)
  "Return an alist of (KEY . VALUE) for all keys and values in HASH-TABLE."
  (let (lis)
    (maphash #'(lambda (key val)
		 (push (cons key val) lis))
	     hash-table)
    (nreverse lis)))

(defun hash-table-key-value-plist (hash-table)
  "Return a plist for all keys and values in HASH-TABLE.
A plist is a simple list containing alternating keys and values."
  (let (lis)
    (maphash #'(lambda (key val)
		 (push key lis)
		 (push val lis))
	     hash-table)
    (nreverse lis)))
