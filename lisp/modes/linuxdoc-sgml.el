;;; linuxdoc-sgml.el --- sgml-mode enhancements for linuxdoc

;; Copyright (C) 1996 by Free Software Foundation, Inc.

;; Author: Arun Sharma <asharma@sco.com>
;; Keywords: docs, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Installation:
;;     Make sure that this file is in your load-path and put this line
;;     in your .emacs.
;;     (autoload 'linuxdoc-sgml-mode "linuxdoc-sgml" t t)
;;     (setq auto-mode-alist (cons '("\\.sgml$" . linuxdoc-sgml-mode)
;;                                  auto-mode-alist))
;; 
;; Optionally:
;;     (add-hook 'linuxdoc-sgml-mode-hook 'turn-on-font-lock)
;; 
;; Caveat: I've had problems getting sgml/html code to work with
;;         lazy-lock. Currently under investigation.
;;; Code:

(require 'sgml-mode)

(defvar  linuxdoc-sgml-tag-alist 
  (let* ((htmlurl '(("ftp:") ("file:") ("finger:")
		    ("gopher:") ("http:") ("mailto:") ("news:")
		    ("rlogin:") ("telnet:") ("tn3270:") ("wais:")
		    ("/cgi-bin/")))
	 (name '(str))
	 (id '(str)))
    `(("abstract" \n)
      ("article" \n)
      ("author" t)
      ("bf")
      ("date" t)
      ("descrip" \n)
      ("enum" \n)
      ("footnote")
      ("htmlurl" t ("url" ,@htmlurl) ("name" ,@name))
      ("item" t)
      ("itemize" \n)
      ("label" ("id" ,@id))
      ("p" t)
      ("quote" \n)
      ("ref" t ("id") ("name" ,@name))
      ("sect"   (t  (setq str (read-input "Sect: ")) "\n<p>\n"))
      ("sect1"  (t  (setq str (read-input "Sect1: ")) "\n<p>\n"))
      ("sect2"  (t  (setq str (read-input "Sect2: ")) "\n<p>\n"))
      ("sect3"  (t  (setq str (read-input "Sect3: ")) "\n<p>\n"))
      ("sect4"  (t  (setq str (read-input "Sect4: ")) "\n<p>\n"))
      ("tag //" t)
      ("title" (t  (setq str (read-input "Title: ")) "\n"))
      ("toc" t)
      ("tscreen")
      ("tt" (nil (setq str (read-input "Text: "))))
      ("url" t ("url" ,@htmlurl) ("name" ,@name))
      ("verb" \n)))
    "Linuxdoc specific tags")

(defvar  linuxdoc-sgml-tag-help 
  '(("abstract" . "Abstract of the document") 
    ("article" . "Beginning of the article")
    ("author" . "Name of the Author")
    ("bf" . "Bold font")     
    ("date" . "Date")
    ("descrip" . "Description environment")
    ("enum" . "Enumerated items")
    ("footnote" . "Footnotes")
    ("htmlurl" . "Insert a URL that shows up only in the HTML version")
    ("item" . "An enumerated or unordered item")
    ("itemize" . "Unordered list")
    ("label" . "A label for cross reference")
    ("p" . "Marks the end of the sect* tag")
    ("quote" . "Quote a piece of text")
    ("ref" . "Cross reference")
    ("sect"  . "Main section heading") 
    ("sect1" . "Level 1 section heading")
    ("sect2" . "Level 2 section heading")
    ("sect3" . "Level 3 section heading") 
    ("sect4" . "Level 4 section heading")
    ("tag //"   . "A description tag")    
    ("title" . "Title of the document")
    ("toc" . "The table of contents")
    ("tscreen" . "Indents the text and uses tt font")
    ("tt" . "Uses the tt font")     
    ("url" . "Insert a URL")
    ("verb" . "The text will be typed verbatim"))
  "Help for linuxdoc specific tags")

(defvar linuxdoc-sgml-tag-face-alist
  '(("abstract" . underline)
    ("article" . italic)
    ("author" . italic)
    ("bf" . bold)
    ("date" . italic)
    ("descrip" . font-lock-reference-face)
    ("enum" . font-lock-type-face)
    ("footnote" . font-lock-keyword-face)
    ("htmlurl" . font-lock-string-face)
    ("item" . font-lock-function-name-face)
    ("itemize" . font-lock-type-face)
    ("label" . font-lock-comment-face)
    ("p" . default)
    ("quote" . underline)
    ("ref" . font-lock-comment-face)
    ("sect"  . underline)
    ("sect1" . underline)
    ("sect2" . underline)
    ("sect3" . underline)
    ("sect4" . underline)
    ("tag"   . font-lock-function-name-face)
    ("title" . underline)
    ("toc" . default)
    ("tscreen" . underline)
    ("tt" . underline)
    ("url" . font-lock-string-face)
    ("verb" . underline))
    "Value of `sgml-tag-face-alist' for linuxdoc-sgml mode.")

(defvar linuxdoc-sgml-font-lock-keywords
    '(("<\\([^>]*\\)>" . font-lock-comment-face))
    "Patterns to highlight in LD-SGML buffers.")

;;;###autoload
(defun linuxdoc-sgml-mode ()
  "Major mode based on SGML mode for editing linuxdoc-sgml documents.
See the documentation on sgml-mode for more info. This mode
understands the linuxdoc-sgml tags."
  (interactive)
  (sgml-mode-common linuxdoc-sgml-tag-face-alist nil)
  (use-local-map sgml-mode-map)
  (make-local-variable 'sgml-tag-alist)
  (make-local-variable 'sgml-face-tag-alist)
  (make-local-variable 'sgml-tag-help)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'sgml-font-lock-keywords)
  (setq mode-name "LD-SGML"
        major-mode 'linuxdoc-sgml-mode
	sgml-tag-alist linuxdoc-sgml-tag-alist
	sgml-face-tag-alist linuxdoc-sgml-tag-face-alist
	sgml-tag-help linuxdoc-sgml-tag-help
	outline-regexp "^.*<sect[1-4]\\>"
	outline-heading-end-regexp "<p>"
	sgml-font-lock-keywords-1 (append sgml-font-lock-keywords-1
					  linuxdoc-sgml-font-lock-keywords
					  sgml-font-lock-keywords)
					  
	outline-level (lambda ()
			(char-after (1- (match-end 0)))))
  (run-hooks 'linuxdoc-sgml-mode-hook))


(provide 'linuxdoc-sgml)

;;; linuxdoc-sgml.el ends here
