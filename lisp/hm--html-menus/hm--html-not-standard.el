;;; hm--html-not-standard.el
;;; v1.00; 22-Feb-1997
;;; Copyright (C) 1997 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	This file contains lisp code for the insertation of non standard
;;;	HTML 3.2 elements. I don't think, that's a good idea to use this
;;;	elements in any HTML documents :-)
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories.
;;;
;;;	Put a (require 'hm--html-not-standard) in your .emacs
;;;
;;;     Look at the files hm--html-mode.el and hm--html-configuration
;;;     for further installation points.
;;;

;(require 'hm--html-mode)

(defun hm--html-add-server-side-include-command-with-parameter (command 
								parameter)
  "This function adds a server side include command directive in the buffer.
The directive is only supported by the NCSA http daemon."
  (interactive (list 
		(completing-read 
		 "Include Command: "
		 hm--html-server-side-include-command-with-parameter-alist)
		(read-string "Parameterlist sepearted by '?': ")))
  (if (string= command "")
      (error "ERROR: No command specified !")
    (if (string= parameter "")
	(error "ERROR: No parameter specified !")
      (if (= ?| (string-to-char command))
	  (if (= ?? (string-to-char parameter))
	      (insert "<INC SRVURL \"" command parameter "\">")
	    (insert "<INC SRVURL \"" command "?" parameter "\">"))
	(if (= ?? (string-to-char parameter))
	    (insert "<INC SRVURL \"|" command parameter "\">")
	  (insert "<INC SRVURL \"|" command "?" parameter "\">"))))))


(defun hm--html-add-server-side-include-command-with-isindex-parameter 
  (command)
  "This function adds a server side include command directive in the buffer.
The include command uses the \"isindex\"- parameter for the specified command."
  (interactive (list 
		(completing-read "Include Command: "
				 hm--html-server-side-include-command-alist)))
  (hm--html-add-server-side-include-command command t))


(defun hm--html-add-server-side-include-command (command &optional srvurl)
  "This function adds a server side include command directive in the buffer.
The directive is only supported by the NCSA http daemon.
If SRVURL is t, then the attribute srvurl instead of srv is used for the 
include command. With srvurl, the include command uses the \"isindex\"-
parameter for the specified command."
  (interactive (list 
		(completing-read "Include Command: "
				 hm--html-server-side-include-command-alist)))
  (let ((attribute (if srvurl "SRVURL" "SRV")))
    (if (string= command "")
	(error "ERROR: No command specified !")
      (if (= ?| (string-to-char command))
	  (insert "<INC " attribute" \"" command "\">")
	(insert "<INC " attribute " \"|" command "\">")))))


(defun hm--html-add-server-side-include-file (file)
  "This function adds a server side include file directive in the buffer.
The directive is only supported by the NCSA http daemon."
  (interactive "FInclude File: ")
  (if (string= file "")
      (error "ERROR: No filename specified !")
    (insert "<INC SRV \"" file "\">")))
  

(defun hm--html-add-plaintext ()
  "Adds the HTML tags for plaintext."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<XMP>"
		     'hm--html-insert-end-tag-with-newline
		     "</XMP>"))


(defun hm--html-add-plaintext-to-region ()
  "Adds the HTML tags for plaintext to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<XMP>"
			       'hm--html-insert-end-tag-with-newline
			       "</XMP>"))


(defun hm--html-add-abstract ()
  "Adds the HTML tags for abstract text at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<ABSTRACT>"
		     'hm--html-insert-end-tag-with-newline
		     "</ABSTRACT>"))


(defun hm--html-add-abstract-to-region ()
  "Adds the HTML tags for abstract text to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<ABSTRACT>"
			       'hm--html-insert-end-tag-with-newline
			       "</ABSTRACT>"))


(defun hm--html-add-quote ()
  "Adds the HTML tags for Quote at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<Q>"
		     'hm--html-insert-end-tag
		     "</Q>"))


(defun hm--html-add-quote-to-region ()
  "Adds the HTML tags for Quote to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<Q>"
			       'hm--html-insert-end-tag
			       "</Q>"))


(defun hm--html-add-person ()
  "Adds the HTML tags for Person at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<PERSON>"
		     'hm--html-insert-end-tag
		     "</PERSON>"))


(defun hm--html-add-person-to-region ()
  "Adds the HTML tags for Person to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<PERSON>"
			       'hm--html-insert-end-tag
			       "</PERSON>"))


(defun hm--html-add-instance ()
  "Adds the HTML tags for Instance at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<INS>"
		     'hm--html-insert-end-tag
		     "</INS>"))


(defun hm--html-add-instance-to-region ()
  "Adds the HTML tags for Instance to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<INS>"
			       'hm--html-insert-end-tag
			       "</INS>"))


(defun hm--html-add-publication ()
  "Adds the HTML tags for Publication at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<PUB>"
		     'hm--html-insert-end-tag
		     "</PUB>"))


(defun hm--html-add-publication-to-region ()
  "Adds the HTML tags for Publication to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<PUB>"
			       'hm--html-insert-end-tag
			       "</PUB>"))


(defun hm--html-add-author ()
  "Adds the HTML tags for Author at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<AUTHOR>"
		     'hm--html-insert-end-tag
		     "</AUTHOR>"))


(defun hm--html-add-author-to-region ()
  "Adds the HTML tags for Author to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<AUTHOR>"
			       'hm--html-insert-end-tag
			       "</AUTHOR>"))


(defun hm--html-add-editor ()
  "Adds the HTML tags for Editor at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<EDITOR>"
		     'hm--html-insert-end-tag
		     "</EDITOR>"))


(defun hm--html-add-editor-to-region ()
  "Adds the HTML tags for Editor to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<EDITOR>"
			       'hm--html-insert-end-tag
			       "</EDITOR>"))


(defun hm--html-add-credits ()
  "Adds the HTML tags for Credits at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<CREDITS>"
		     'hm--html-insert-end-tag
		     "</CREDITS>"))


(defun hm--html-add-credits-to-region ()
  "Adds the HTML tags for Credits to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<CREDITS>"
			       'hm--html-insert-end-tag
			       "</CREDITS>"))


(defun hm--html-add-copyright ()
  "Adds the HTML tags for Copyright at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<COPYRIGHT>"
		     'hm--html-insert-end-tag
		     "</COPYRIGHT>"))


(defun hm--html-add-copyright-to-region ()
  "Adds the HTML tags for Copyright to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<COPYRIGHT>"
			       'hm--html-insert-end-tag
			       "</COPYRIGHT>"))


(defun hm--html-add-isbn ()
  "Adds the HTML tags for ISBN at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<ISBN>"
		     'hm--html-insert-end-tag
		     "</ISBN>"))


(defun hm--html-add-isbn-to-region ()
  "Adds the HTML tags for ISBN to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<ISBN>"
			       'hm--html-insert-end-tag
			       "</ISBN>"))


(defun hm--html-add-acronym ()
  "Adds the HTML tags for Acronym at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<ACRONYM>"
		     'hm--html-insert-end-tag
		     "</ACRONYM>"))


(defun hm--html-add-acronym-to-region ()
  "Adds the HTML tags for Acronym to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<ACRONYM>"
			       'hm--html-insert-end-tag
			       "</ACRONYM>"))


(defun hm--html-add-abbrevation ()
  "Adds the HTML tags for Abbrevation at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<ABBREV>"
		     'hm--html-insert-end-tag
		     "</ABBREV>"))


(defun hm--html-add-abbrev-to-region ()
  "Adds the HTML tags for Abbrev to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<ABBREV>"
			       'hm--html-insert-end-tag
			       "</ABBREV>"))


(defun hm--html-add-command ()
  "Adds the HTML tags for Command at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<CMD>"
		     'hm--html-insert-end-tag
		     "</CMD>"))


(defun hm--html-add-command-to-region ()
  "Adds the HTML tags for Command to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<CMD>"
			       'hm--html-insert-end-tag
			       "</CMD>"))


(defun hm--html-add-argument ()
  "Adds the HTML tags for Argument at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<ARG>"
		     'hm--html-insert-end-tag
		     "</ARG>"))


(defun hm--html-add-argument-to-region ()
  "Adds the HTML tags for Argument to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<ARG>"
			       'hm--html-insert-end-tag
			       "</ARG>"))


(defun hm--html-add-literature ()
  "Adds the HTML tags for Literature at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<LIT>"
		     'hm--html-insert-end-tag
		     "</LIT>"))


(defun hm--html-add-literature-to-region ()
  "Adds the HTML tags for Literature to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<LIT>"
			       'hm--html-insert-end-tag
			       "</LIT>"))


(defun hm--html-add-footnote ()
  "Adds the HTML tags for Footnote at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<FOOTNOTE>"
		     'hm--html-insert-end-tag
		     "</FOOTNOTE>"))


(defun hm--html-add-footnote-to-region ()
  "Adds the HTML tags for Footnote to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<FOOTNOTE>"
			       'hm--html-insert-end-tag
			       "</FOOTNOTE>"))


(defun hm--html-add-margin ()
  "Adds the HTML tags for Margin at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<MARGIN>"
		     'hm--html-insert-end-tag
		     "</MARGIN>"))


(defun hm--html-add-margin-to-region ()
  "Adds the HTML tags for Margin to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<MARGIN>"
			       'hm--html-insert-end-tag
			       "</MARGIN>"))


(defun hm--html-add-listing ()
  "Adds the HTML tags for listing."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<LISTING>"
		     'hm--html-insert-end-tag-with-newline
		     "</LISTING>"))


(defun hm--html-add-listing-to-region ()
  "Adds the HTML tags for listing to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<LISTING>"
			       'hm--html-insert-end-tag-with-newline
			       "</LISTING>"))


(provide 'hm--html-not-standard)
