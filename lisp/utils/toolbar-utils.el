;;; toolbar-utils.el --- Toolbar utility functions for XEmacs

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: Jeff Miller <jmiller@smart.net>
;; Keywords: extensions

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Based largely on edit-toolbar.el by Peter D. Pezaris <pez@dwwc.com>

;;; Code:

;;;###autoload
(defun restore-initial-toolbar ()
  "Restores the default toolbar defined by initial-toolbar-spec."
  (interactive)
  (set-specifier default-toolbar initial-toolbar-spec)
  )

;;;###autoload
(defun toolbar-add-item (item &optional index &optional toolbar-spec)
  "Add a toolbar item ITEM at the first location of the toolbar specifier. 
Optionally, can specify an INDEX position to insert the ITEM.  The default is
to use default-toolbar, but a different specifier can by specified with 
TOOLBAR-SPEC."
  (if (eq  toolbar-spec nil )
      (setq toolbar-spec default-toolbar))
  (let* ((toolbar (specifier-instance toolbar-spec)))
    (if(or (eq index nil) (eq index 0))
	(setq toolbar (cons item toolbar))
      (setcdr (nthcdr (- index 1) toolbar)
	      (cons item (nthcdr index toolbar))))
    (set-specifier toolbar-spec toolbar)
    ))


;;;###autoload
(defun toolbar-kill-item-pos ( index &optional toolbar-spec) 
  "Remove a toolbar item ITEM at the first location of the toolbar specifier.  
Optionally, can specify an INDEX position where to remove the ITEM.  The 
default is to use default-toolbar, but a different specifier can by 
specified with TOOLBAR-SPEC."
  (if (eq toolbar-spec nil ) 
      (setq toolbar-spec default-toolbar)) 
  (let* ((toolbar (specifier-instance toolbar-spec)) 
	 (item (nth index toolbar))) 
    (if (eq index 0) 
	(setq toolbar(cdr toolbar)) 
      (setcdr (nthcdr (1- index) toolbar) 
	      (nthcdr (1+ index) toolbar))) 
    (set-specifier toolbar-spec toolbar) 
    ))

;;;###autoload
(defun toolbar-kill-item ( item &optional toolbar-spec)
  "Remove a toolbar item ITEM at the first location of the toolbar specifier.  
Optionally, can specify an ITEM to remove.  The ITEM must be in form of a 
vector.  The default is to use default-toolbar, but a different specifier 
can by specified with TOOLBAR-SPEC."
  (if (eq  toolbar-spec nil )
      (setq toolbar-spec default-toolbar))
	      (let* ((toolbar (specifier-instance toolbar-spec)) )
    (eval item)
    (set-specifier toolbar-spec (delete item toolbar))
    ))


(provide 'toolbar-utils)

;;; toolbar-utils.el ends here
