;;!emacs
;;
;; FILE:         hsys-wais.el
;; SUMMARY:      Hyperbole support for WAIS browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     comm, help, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     5-Nov-91 at 20:53:26
;; LAST-MOD:     30-Oct-95 at 22:48:21 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991, 1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   For information on WAIS, see ftp://ftp.wais.com:/pub/.
;;   The freeware/ subdirectory there contains free source code to support
;;   WAIS on most standard architectures.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; Autoload wais.
(autoload 'wais              "wais" "Client-server full-text retrieval"  t)
(autoload 'waisd-mode        "wais" "Wide Area Information Services"     t)
(autoload 'wais-select-question "wais" "Select a new WAIS question."     t)
(autoload 'wais-create-question "wais" "Create a new WAIS question."     t)

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(defib wais-smart ()
  "Handles context-sensitive Smart Key in WAIS buffers."
  (if (featurep 'wais)
      (let ((b (buffer-name)))
	(cond ((string-match ": Find Documents On\\|: Similar To" b)
	       (hact 'wais-smart 'wais-query))
	      ((equal "*Source List*" b)
	       (hact 'wais-smart 'source-menu-view))
	      ((string-match ": On Sources" b)
	       (hact 'wais-smart
		     (if (eobp) 'wais-view-source 'source-menu-view)))
	      ((string-match ": Results" b)
	       (hact 'wais-smart 'wais-edit))
	      ))))

;;; ************************************************************************
;;; Public button action types
;;; ************************************************************************

(defact wais-ques (question-name)
  "Loads a Wais Question QUESTION-NAME, displays it with WAIS Emacs interface."
  (interactive "sWAIS Question name: ")
  (if (or (featurep 'wais) (load "wais"))
      (progn
	(display-question question-name)
	(wais)
	(display-question question-name)
	)
    (error "(wais-ques): WAIS interface not available so can't load question.")
    ))

(defact wais-smart (interactive-func)
  "Performs INTERACTIVE-FUNC in a Wais buffer."
  (call-interactively interactive-func))

(provide 'hsys-wais)
