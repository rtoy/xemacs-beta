;;!emacs
;;
;; FILE:         kproperty.el
;; SUMMARY:      Wrapper for koutline text property implementations.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     outlines, wp
;;
;; AUTHOR:       Bob Weiner
;;
;; ORIG-DATE:    7/27/93
;; LAST-MOD:     27-Oct-95 at 22:45:55 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1993, 1994, 1995  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(load (if hyperb:emacs19-p "kprop-em" "kprop-xe"))

(provide 'kproperty)
