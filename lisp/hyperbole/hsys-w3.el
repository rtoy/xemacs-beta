;;!emacs
;;
;; FILE:         hsys-w3.el
;; SUMMARY:      Hyperbole support for Emacs W3 World-Wide Web (WWW) browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     comm, help, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Apr-94 at 17:17:39 by Bob Weiner
;; LAST-MOD:     10-Mar-97 at 12:17:08 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1994, 1995  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   This module defines an implicit button type and associated action and
;;   help types.  A press of the Action Key on a unified resource locator
;;   (URL) displays the referent for the URL.  A press of the Help Key on a
;;   URL displays a history list of previously browsed WWW documents.  Press
;;   the Action Key on any item from the history list to display it.
;;
;;   This requires the Emacs W3 World-Wide-Web browser available from:
;;     ftp://cs.indiana.edu/pub/elisp/w3/.
;;
;;   It assumes that you have set up to have w3 auto-loaded according to the
;;   setup instructions included with W3.  Specifically, `w3-fetch' should be
;;   autoloaded.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;;; Requires that 'w3' or other web browser code that is called be available.

;;; ************************************************************************
;;; Public functions and types
;;; ************************************************************************

(defib www-url ()
  "When not in a w3 buffer, follow any non-ftp url (link) at point.
The variable, `action-key-url-function,' can be used to customize the url
browser that is used."
  (if (not (eq major-mode 'w3-mode))
      (let ((link-and-pos (hpath:www-at-p t)))
	(if link-and-pos
	    (progn (ibut:label-set link-and-pos)
		   (hact 'www-url (car link-and-pos)))))))

(defact www-url (url)
  "Follows a link given by URL.
The variable, `action-key-url-function,' can be used to customize the url
browser that is used."
  (interactive "sURL to follow: ")
  (or (stringp url)
      (error "(www-url): Link label must be given as a string."))
  (and (symbolp action-key-url-function)
       (memq action-key-url-function
	     '(highlight-headers-follow-url-netscape
	       highlight-headers-follow-url-mosaic))
       (require 'highlight-headers))
  (if window-system
      (funcall action-key-url-function url)
    (w3-fetch url)))

(defun www-url:help (&optional but)
  "Displays history list of www nodes previously visited with the W3 browser."
  (interactive)
  (if (fboundp 'w3-show-history-list)
      (hact 'w3-show-history-list)
    (hact 'error "(www-url:help): W3 must be loaded to display WWW history")))

(provide 'hsys-w3)
