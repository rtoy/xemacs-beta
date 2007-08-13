;;; hm--html-configuration.el - Configurationfile for the html-mode
;;;
;;; $Id: hm--html-configuration.el,v 1.7 1997/07/26 22:09:45 steve Exp $
;;;
;;; Copyright (C) 1993 - 1997  Heiko Muenkel
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
;;;	This file is for the system wide configuration of the html mode.
;;;	User specific configuration should be done in the file
;;;	~/.hm--html-configuration.el, which precedes the settings in
;;;	this file.
;;;	All settings in this file are done with defvar's, therefore
;;;	you could overwrite them also with the function setq in your
;;;	.emacs or default.el and so on.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load path directories or
;;;	set the environment variable HTML_CONFIG_FILE to this file.
;;;	For example: 
;;;       setenv HTML_CONFIG_FILE "~/data/hm--htm-environment.el"
;;;	  if you have put this file in the directory "~/data/"
;;;

;(require 'adapt)

(defgroup hm--html nil
  "A package for writing HTML pages.
It provides a major mode and a minor mode. The minor mode can be
used together with the psgml html-mode."
  :group 'hypermedia)

(defgroup hm--html-files nil
  "hm--html configuration files."
  :group 'hm--html)

(defgroup hm--html-document-information nil
  "Variables relating to the insertation of document information.
This contains the user name of the document author, his signature,
the creation and change dates, the HTML doctype and the meta element."
  :group 'hm--html)

(defgroup hm--html-menus nil
  "Variables relating to the pulldown and popup menus."
  :group 'hm--html)

(defgroup hm--html-links nil
  "Variables relating to the insertation of links."
  :group 'hm--html)

(defgroup hm--html-templates nil
  "Variables relating to inserting HTML templates."
  :group 'hm--html)

(defgroup hm--html-keys nil
  "Variables relating to the key and mouse bindings and drag and drop."
  :group 'hm--html)

(defgroup hm--html-display nil
  "Variables relating to the display of the HTML sources and the previewing."
  :group 'hm--html)

(defgroup hm--html-hooks nil
  "Hooks relating to the hm--html modes."
  :group 'hm--html)

(defgroup hm--html-indentation nil
  "Variables relating to the indentation in the `hm--html-mode'."
  :group 'hm--html)


;;; The User config file (an proposal of Manoj Srivastava)
(defcustom hm--html-user-config-file nil
  "*The location of the users config file.
This variable will only be used, if no environment variable
\"HTML_USER_CONFIG_FILE\" is set. 
Example value: \"~/.hm--html-configuration.el\".

If this is set to nil and no \"HTML_USER_CONFIG_FILE\" is set,
then the file ~/.hm--html-configuration.el will be used. In this case
also the variable `init-file-user' will be respected."
  :group 'hm--html-files
  :type '(choice (const :tag "~/.hm--html-configuration.el" :value nil)
		 file))

;;; The site specific config file
(defcustom hm--html-site-config-file nil
  "*The location of a site specific config file.
This variable will only be used, if no environment variable
\"HTML_SITE_CONFIG_FILE\" is set."
  :group 'hm--html-files
  :type '(choice (const :tag "No Site Specific Configuration" :value nil)
		 file))

;;; Chose the initial popup menu
(defcustom hm--html-expert nil
  "*t    : Use the HTML expert popup menu,
nil : Use the HTML novice (simple) menu.

NOTE: In the Emacs 19 you should set this variable only before 
      loading the mode."
  :group 'hm--html-menus
  :type '(choice (const :tag "Use Expert Popup Menu" :value t)
		 (const :tag "Use Novice Popup Menu" :value nil)))

;;; Your Signature

(defcustom hm--html-signature-file nil 
  "*Your Signature file.
For example: \"http://www.tnt.uni-hannover.de:80/data/info/www/tnt/info/tnt/whois/muenkel.html\"."
  :group 'hm--html-document-information
  :type '(choice (const :tag "No Signature file" :value nil)
		 string))


(defcustom hm--html-username nil
  "*Your Name for the signature. For example: \"Heiko Münkel\"."
  :group 'hm--html-document-information
  :type '(choice (const :tag "Use Value Of `(user-full-name)'" :value nil)
		 string))


;;; HTML Doctype
(defcustom hm--html-html-doctype-version "-//W3C//DTD HTML 3.2 Final//EN"
  "*The HTML version. This is used in the doctype element."
  :group 'hm--html-document-information
  :type 'string)


;;; Your favorite server (eg: the name of the host of your own http server)
;;; This is used in some other variables

(defcustom hm--html-favorite-http-server-host-name "www.tnt.uni-hannover.de"
  "*The name of your favorite http server host. It must be specified !"
  :group 'hm--html-links
  :type 'string)


;;; For links to Info Gateways

(defcustom hm--html-info-hostname:port-alist
  '(("www.tnt.uni-hannover.de:8005"))
  "*Alist with hostnames and ports for the Info gateway."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-info-hostname:port-default "www.tnt.uni-hannover.de:8005"
  "*Default hostname with port for the Info gateway."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-info-path-alist '((1 . "/appl/lemacs/Global/emacs/info")
				      (2 . "/appl/emacs/info")
				      (3 . "/appl/gnu/Global/info")
				      (4 . "/appl/emacs-19/Global/info")
				      (5 . "/"))
  "*Alist with directories for the Info gateway."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For links to WAIS Gateways

(defcustom hm--html-wais-hostname:port-alist '(("www.tnt.uni-hannover.de:8001")
					       ("info.cern.ch:8001"))
  "*Alist with hostnames and ports for the WAIS gateway."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-wais-hostname:port-default "www.tnt.uni-hannover.de:8001"
  "*Default hostname with port for the WAIS gateway."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-wais-servername:port-alist 
  '(("wais.tnt.uni-hannover.de:210")
    ("daedalus.tnt.uni-hannover.de:21408")
    ("ikarus.tnt.uni-hannover.de:21401"))
  "*Alist with servernames and ports for the WAIS gateway."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-wais-servername:port-default "www.tnt.uni-hannover.de:210"
  "*Default servername with port for the WAIS gateway."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-wais-path-alist nil
  "*Alist with directories for the wais gateway."
  :group 'hm--html-links
  :type '(repeat string))


;;; For links to HTML servers

(defcustom hm--html-html-hostname:port-alist '(("www.tnt.uni-hannover.de:80")
					       ("vxcrna.cern.ch:80")
					       ("www.ncsa.uiuc.edu:80"))
  "*Alist with hostnames and ports for the HTML server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-html-hostname:port-default "www.tnt.uni-hannover.de:80"
  "*Default hostname with port for the HTML server."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-html-path-alist '((1 . "/data/info/www/tnt/")
				      (2 . "/data/info/www/")
				      (3 . "/data/info/")
				      (4 . "/data/")
				      (5 . "/appl/")
				      (6 . "/project/")
				      (7 . "~/")
				      (8 . "/"))
  "*Alist with directories for the HTML server."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For links to file gateways

(defcustom hm--html-file-path-alist '((1 . "/data/info/www/tnt/")
				      (2 . "/data/info/www/")
				      (3 . "/data/info/")
				      (4 . "/data/")
				      (5 . "/appl/")
				      (6 . "/project/")
				      (7 . "~/")
				      (8 . "/"))
  "*Alist with directories for the file gateway."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For links to ftp servers

(defcustom hm--html-ftp-hostname:port-alist
  '(("ftp.tnt.uni-hannover.de")
    ("ftp.rrzn.uni-hannover.de")
    ("wega.informatik.uni-hannover.de")
    ("rusmv1.rus.uni-stuttgart.de")
    ("export.lcs.mit.edu")
    )
  "*Alist with hostnames and ports for the ftp server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-ftp-hostname:port-default "ftp.rrzn.uni-hannover.de"
  "*Default hostname with port for the ftp server."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-ftp-path-alist '((1 . "/pub")
				     (2 . "/pub/gnu")
				     (3 . "/pub/linux")
				     (4 . "/pub/unix")
				     (5 . "/incoming")
				     (6 . "/"))
  "*Alist with directories for the ftp server."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For links to gopher servers

(defcustom hm--html-gopher-hostname:port-alist
  '(("newsserver.rrzn.uni-hannover.de:70")
    ("solaris.rz.tu-clausthal.de:70")
    ("veronica.scs.unr.edu:70")
    ("pinus.slu.se:70")
    ("sunic.sunet.se:70")
    )
  "*Alist with hostnames and ports for the gopher server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-gopher-hostname:port-default
  "newsserver.rrzn.uni-hannover.de:70"
  "*Default hostname with port for the gopher server."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-gopher-doctype-alist '(("/1")
					("/11")
					("/00"))
  "*Alist with doctype strings for the gopher server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-gopher-doctype-default "/1"
  "*Default doctype string for the gopher server."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-gopher-anchor-alist
  '(("veronica")
    ("Wide%20Area%20Information%20Services%20databases")
    ("Subject%20Tree"))
  "*Alist with directories for the gopher server."
  :group 'hm--html-links
  :type '(repeat string))


;;; For the links to the Program Gateway

(defcustom hm--html-proggate-hostname:port-alist
  '(("www.tnt.uni-hannover.de:8007")
    )
  "*Alist with hostnames and ports for the proggate server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-proggate-hostname:port-default
  "www.tnt.uni-hannover.de:8007"
  "*Default hostname with port for the proggate server."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-proggate-allowed-file "/appl/www/bin/proggate.allowed"
  "*The filename (with path) of the proggate allowed file."
  :group 'hm--html-links
  :type 'file)


;;; For links to the Local Program Gatewy

(defcustom hm--html-local-proggate-path-alist '((1 . "/bin/")
						(2 . "/usr/bin/")
						(3 . "/usr/local/bin/")
						(4 . "/appl/util/bin/")
						(5 . "/appl/gnu/Global/bin/")
						(6 . "/")
						(7 . "/appl/")
						(8 . "~/appl/Global/bin/")
						(9 . "~/"))
  "*Alist with directories for the local program gateway."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For links to the mail gateway

(defcustom hm--html-mail-hostname:port-alist '(("www.tnt.uni-hannover.de:8003")
					       )
  "*Alist with hostnames and ports for the mail gateway."
  :group 'hm--html-links
  :type '(repeat string))

(defcustom hm--html-mail-hostname:port-default "www.tnt.uni-hannover.de:8003"
  "*Default hostname with port for the mail gateway."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-mail-path-alist '((1 . "~/data/docs/mail")
				      (2 . "~/data/docs/news")
				      (3 . "~/docs/mail")
				      (4 . "~/docs/news")
				      (5 . "~/mail")
				      (6 . "~/news")
				      (7 . "~/")
				      (8 . "/data/info/mail")
				      (9 . "/data/info/news")
				      (10 . "/"))
  "*Alist with directories for the mail gateway."
  :group 'hm--html-links
  :type '(repeat string))


;;; For mailto links

(defcustom hm--html-mailto-alist '(("muenkel@tnt.uni-hannover.de"))
  "*Alist with mail adresses for the mailto alist.
The value of `user-mail-address' will also be added by the package to
this alist."
  :group 'hm--html-links
  :type '(repeat string))


;;; For the server side include directive
;;; not sure, if these directives works on any server

(defcustom hm--html-server-side-include-command-alist '(("/bin/date")
							("/usr/bin/finger")
							("/bin/df"))
  "*Alist with commands for the server side include directive.
These commands needs no parameter."
  :group 'hm--html-links
  :type '(repeat string))
	
(defcustom hm--html-server-side-include-command-with-parameter-alist
  '(("/usr/bin/man")
    ("/usr/bin/finger")
    ("/usr/bin/ls")
    ("/bin/cat"))
  "*Alist with commands for the server side include directive.
These commands needs parameters."
  :group 'hm--html-links
  :type '(repeat string))
	

;;; Alist with URL'S for FORMS and IMAGE tags

(defcustom hm--html-url-alist 
  (list
   '("http://hoohoo.ncsa.uiuc.edu/htbin-post/post-query"
     POST)
   '("http://hoohoo.ncsa.uiuc.edu/htbin/query"
     GET)
   (list 
    (concat "http://" 
	    hm--html-favorite-http-server-host-name
	    "/")
    'IMAGE))
  "*Alist with URL's for FORMS and IMAGE tags. 
The cdr of each list contains symbols, which specifys the use of the
URL."
  :group 'hm--html-links
  :type '(repeat cons))


;;; For the marking of examples in the help buffer

(defcustom hm--html-help-foreground "red"
  "The foreground color to highlight examples."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-help-background nil
  "The background color to highlight examples."
  :group 'hm--html-links
  :type 'string)

(defcustom hm--html-help-font (face-font 'bold)
  "The font to highlight examples."
  :group 'hm--html-links
  :type 'string)


;;; For the Templates

(defcustom hm--html-template-dir "/data/info/www/tnt/guide/templates"
  "*A directory with templatefiles.
It is now also possible to use it as a list of directories.
Look at the variable `tmpl-template-dir-list' for further descriptions."
  :group 'hm--html-templates
  :type 'directory)

(if (listp hm--html-template-dir)
    (unless (file-exists-p (car hm--html-template-dir))
      ;; Use a system directory, if the above one doesn't exist
      ;; This may only be useful, in the XEmacs >= 19.12
      (setq hm--html-template-dir (cons (concat data-directory
						"../lisp/hm--html-menus/")
					hm--html-template-dir)))
  (unless (file-exists-p hm--html-template-dir)
    ;; Use a system directory, if the above one doesn't exist
    ;; This may only be useful, in the XEmacs >= 19.12
    (setq hm--html-template-dir (concat data-directory
					"../lisp/hm--html-menus/"))))

(defcustom hm--html-frame-template-file (concat data-directory
					     "../lisp/hm--html-menus/"
					     "frame.html.tmpl")
  "File, which is used as template for a html frame."
  :group 'hm--html-templates
  :type 'file)

(defcustom hm--html-automatic-expand-templates t
  "*Automatic expansion of templates. This feature needs the file
tmpl-minor-mode.el from Heiko Muenkel (muenkel@tnt.uni-hannover.de),
which is distributed with the package hm--html-menus."
  :group 'hm--html-templates
  :type 'boolean)

(defcustom hm--html-template-filter-regexp ".*\\.html\\.tmpl$"
  "*Regexp for filtering out non template files in a directory."
  :group 'hm--html-templates
  :type 'string)

;;; for deleting the automounter path-prefix
(defcustom hm--html-delete-wrong-path-prefix '("/tmp_mnt" "/phys/[^/]+")
  "If non nil, it specifies path-prefixes, which should be deleted in pathes.
The Sun automounter adds a temporary prefix to the automounted directories
 (At our site the prefix is /tmp_mnt). But you can't select such a path, if 
the automounter has currently not mounted the directory and so you can't
follow a html-link, which consists of such a path. To overcome this behaviour,
you can set this variable to the prefix (eg. \"/tmp_mnt\"). After that, the
prefix should be stripped from the pathes during the creation of the links.
ATTENTION: This variable is used as regular expression !
It can be set to a string or to a list of strings."
  :group 'hm--html-links
  :type '(repeat string))


;;; For insertation of created and changed comments and automatic
;;; date update in the title line and a visible modification date

(defcustom hm--html-automatic-create-title-date t
  "*t => A date string will be inserted in the title line.
This will be updated each time before file saving, if
`hm--html-automatic-update-title-date' is also set to t."
  :group 'hm--html-document-information
  :type 'boolean)

(defcustom hm--html-automatic-update-title-date t
  "*t   => The date in the title line will be updated before filesaving.
nil => No automatic update of the date."
  :group 'hm--html-document-information
  :type 'boolean)

(define-obsolete-variable-alias 'hm--html-automatic-new-date
  'hm--html-automatic-update-title-date)

(defcustom hm--html-automatic-changed-comment t
  "*t   => A \"changed comment\" line will be added before filesaving.
nil => No automatic insertation of a \"changed comment\" line."
  :group 'hm--html-document-information
  :type 'boolean)

(defcustom hm--html-changed-comment-prefix "Changed by: "
  "*The prefix text of the \"changed comment\" lines."
  :group 'hm--html-document-information
  :type 'string)

(defcustom hm--html-created-comment-prefix "Created by: "
  "*The prefix text of the \"created comment\" lines."
  :group 'hm--html-document-information
  :type 'string)

(defcustom hm--html-comment-infix nil
  "*The infix (second part) of the \"changed/created comment\" lines.
By default, if this variable is nil, the username is used.
Then the infix looks like \"Heiko Münkel, \".
Set it to an empty string, if you don't want to have your name
in the comments."
  :group 'hm--html-document-information
  :type '(choice (const :tag "Use The Username" :value nil)
		 string))

(defcustom hm--html-automatic-created-comment t
  "*t   => A \"created comment\" line will be added.
nil => No automatic insertation of a \"created comment\" line."
  :group 'hm--html-document-information
  :type 'boolean)

(defcustom hm--html-automatic-create-modified-line nil
  "*t => Inserts a visible \"modified\" line with the current date.
Visible means, that it is not a HTML comment."
  :group 'hm--html-document-information
  :type 'boolean)

(defcustom hm--html-automatic-update-modified-line nil
  "*t => Updates a visible \"modified\" line with the current date.
Visible means, that it is not a HTML comment."
  :group 'hm--html-document-information
  :type 'boolean)

(defcustom hm--html-modified-prefix "Modified: "
  "*Prefix of the last modified entry."
  :group 'hm--html-document-information
  :type 'string)

(defcustom hm--html-modified-start-tag "<EM>"
  "*Start tag of the modified line.
If you change this, you'll need to change also
`hm--html-modified-end-tag'."
  :group 'hm--html-document-information
  :type '(choice (const :tag "Emphasized" :value "<EM>")
		 (const :tag "Strong" :value "<STRONG>")
		 (const :tag "No Tags" :value "")
		 (const :tag "Bold" :value "<B>")
		 (const :tag "Italic" :value "<I>")
		 (const :tag "Typewriter" :value "<TT>")
		 (const :tag "Small" :value "<SMALL>")
		 (const :tag "Big" :value "<BIG>")
		 (const :tag "Underline" :value "<U>")
		 string))

(defcustom hm--html-modified-end-tag "</EM>"
  "*End tag of the modified line.
If you change this, you'll need to change also
`hm--html-modified-start-tag'."
  :group 'hm--html-document-information
  :type '(choice (const :tag "Emphasized" :value "</EM>")
		 (const :tag "Strong" :value "</STRONG>")
		 (const :tag "No Tags" :value "")
		 (const :tag "Bold" :value "</B>")
		 (const :tag "Italic" :value "</I>")
		 (const :tag "Typewriter" :value "</TT>")
		 (const :tag "Small" :value "</SMALL>")
		 (const :tag "Big" :value "</BIG>")
		 (const :tag "Underline" :value "</U>")
		 string))

(defcustom hm--html-modified-insert-before "</body>"
  "Insert modified line before this string.
The search will be done from the end to the beginning."
  :group 'hm--html-document-information
  :type 'string)


;;; Keybindings:

(defcustom hm--html-bind-latin-1-char-entities t
  "Set this to nil, if you don't want to use the ISO Latin 1 character entities.
This is only useful, if `hm--html-use-old-keymap' is set to nil. It is only 
used during loading the html package the first time."
  :group 'hm--html-keys
  :type 'boolean)


;;; The drag and drop interface
(defcustom hm--html-idd-create-relative-links t
  "If t, then the hm--html-idd-* functions are creating relative links.
Otherwise absolute links are used. The idd functions are used for
drag and drop."
  :group 'hm--html-keys
  :type 'boolean)

(defcustom hm--html-idd-actions
  '((nil (((idd-if-major-mode-p . dired-mode)
	   (idd-if-dired-file-on-line-p . ".*\\.\\(gif\\)\\|\\(jpg\\)"))
	  hm--html-idd-add-include-image-from-dired-line)
	 (((idd-if-major-mode-p . dired-mode)
	   (idd-if-dired-no-file-on-line-p . nil))
	  hm--html-idd-add-file-link-to-file-on-dired-line)
	 (((idd-if-major-mode-p . dired-mode)
	   (idd-if-dired-no-file-on-line-p . t))
	  hm--html-idd-add-file-link-to-directory-of-buffer)
	 (((idd-if-major-mode-p . w3-mode)
	   (idd-if-url-at-point-p . t))
	  hm--html-idd-add-html-link-from-w3-buffer-point)
	 (((idd-if-major-mode-p . w3-mode))
	  hm--html-idd-add-html-link-to-w3-buffer)
	 (((idd-if-local-file-p . t))
	  hm--html-idd-add-file-link-to-buffer)))
  "The action list for the destination mode `hm--html-mode'.
Look at the description of the variable idd-actions."
  :group 'hm--html-keys
  :type 'list)


;;; The font lock keywords

(defcustom hm--html-font-lock-keywords-1
  (list
   '("<!--.*-->" . font-lock-comment-face)
   '("<[^>]*>" . font-lock-keyword-face)
;   '("<[^>=]*href[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\"" 1 font-lock-string-face t)
;   '("<[^>=]src[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\"" 1 font-lock-string-face t)
   '("<[^>=]*\\(href\\|src\\)[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\"" 
     2 font-lock-string-face t))
  "Subdued level highlighting for hm--html-mode."
  :group 'hm--html-display
  :type '(repeat cons))

(defcustom hm--html-font-lock-keywords-2
  (append hm--html-font-lock-keywords-1
	  (list
	   '(">\\([^<]+\\)</a>" 1 font-lock-reference-face)
	   '("</b>\\([^<]+\\)</b>" 1 bold)
	   '("</i>\\([^<]+\\)</i>" 1 italic)
	   ))
  "Gaudy level highlighting for hm--html-mode."
  :group 'hm--html-display
  :type '(repeat cons))

(defcustom hm--html-font-lock-keywords hm--html-font-lock-keywords-1
  "Default expressions to highlight in the hm--html-mode."
  :group 'hm--html-display
  :type '(repeat cons))



;;; The Prefix- Key for the keytables
(defcustom hm--html-minor-mode-prefix-key "\C-z"
  "The prefix key for the keytables in the `hm--html-minor-mode'."
  :group 'hm--html-keys
  :type 'string)

(defcustom hm--html-mode-prefix-key "\C-c"
  "The prefix key for the hm--html keys in the `hm--html-mode'."
  :group 'hm--html-keys
  :type 'string)


;;; The pulldown menu names
(defcustom hm--html-minor-mode-pulldown-menu-name "HM-HTML"
  "The name of the pulldown menu in the minor html mode."
  :group 'hm--html-menus
  :type 'string
  )

(defcustom hm--html-mode-pulldown-menu-name "HTML"
  "The name of the pulldown menu in the major html mode."
  :group 'hm--html-menus
  :type 'string)


;;; The hook variables
(defcustom hm--html-load-hook nil
  "*Hook variable to execute functions after loading the package."
  :group 'hm--html-hooks
  :type 'hook)

(defcustom hm--html-mode-hook nil
  "*This hook will be called each time, when the hm--html-mode is invoked."
  :group 'hm--html-hooks
  :type 'hook)


;;; For the file html-view.el
;;; There are also some other variables in hmtl-view.el
;;; Look at that file, if you've trouble with the functions
;;; to preview the html document with the Mosaic
(defcustom html-view-mosaic-command "/sol/www/bin/mosaic"
  "The command that runs Mosaic on your system."
  :group 'hm--html-display
  :type '(choice (const :tag "mosaic" :value "mosaic")
		 (const :value "/usr/local/bin/mosaic")
		 file))

(defcustom html-sigusr1-signal-value 16
  "Value for the SIGUSR1 signal on your system.  
See, usually, /usr/include/sys/signal.h.
 	SunOS 4.1.x	: (setq html-sigusr1-signal-value 30)
	SunOS 5.x	: (setq html-sigusr1-signal-value 16)
	Linux		: (setq html-sigusr1-signal-value 10))"
  :group 'hm--html-display
  :type '(choice (const :tag "On SunOS 4.1.x" :value 30)
		 (const :tag "On SunOS 5.x" :value 16)
		 (const :tag "On Linux" :value 10)
		 integer))


;;; Meta information
(defcustom hm--html-meta-name-alist '(("Expires") ("Keys") ("Author"))
  "*Alist with possible names for the name or http-equiv attribute of meta."
  :group 'hm--html-document-information
  :type '(repeat (list (choice (const "Expires")
			       (const "Keys")
			       (const "Author")
			       string))))

;;; indentation

(defcustom hm--html-disable-indentation nil
  "*Set this to t, if you want to disable the indentation in the hm--html-mode.
And may be send me (muenkel@tnt.uni-hannover.de) a note, why you've
done this."
  :group 'hm--html-indentation
  :type 'boolean)

(defcustom hm--html-inter-tag-indent 2
  "*The indentation after a start tag."
  :group 'hm--html-indentation
  :type 'integer)

(defcustom hm--html-comment-indent 5
  "*The indentation of a comment."
  :group 'hm--html-indentation
  :type 'integer)

(defcustom hm--html-intra-tag-indent 2
  "*The indentation after the start of a tag."
  :group 'hm--html-indentation
  :type 'integer)

(defcustom hm--html-tag-name-alist
  '(("!--" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("!doctype" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("isindex" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (prompt)))
    ("base" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes (href))
     (:hm--html-optional-attributes nil))
    ("meta" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes (content))
     (:hm--html-optional-attributes (http-equiv name)))
    ("link" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (href rel rev title)))
    ("hr" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align noshade size width)))
    ("input" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes
      (type name value checked size maxlength src align)))
    ("img" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes (src))
     (:hm--html-optional-attributes
      (alt align height width border hspace vspace usemap ismap)))
    ("param" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes (name))
     (:hm--html-optional-attributes (value)))
    ("br" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (clear)))
    ("basefont" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes size))
    ("area" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes (alt))
     (:hm--html-optional-attributes (shape coords href nohref)))
    ("option" (:hm--html-one-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (selected value)))

    ("html" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("head" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("body" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (bgcolor text link vlink alink background))
     )
    ("h1" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("h2" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("h3" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("h4" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("h5" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("h6" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("address" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("p" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("ul" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (type compact)))
    ("ol" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (type start compact)))
    ("dl" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (compact)))
    ("li" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (type (value "ol"))))
    ("dt" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("dd" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("dir" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (compact)))
    ("menu" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (compact)))
    ("pre" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (width)))
    ("div" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("center" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("blockquote" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("form" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (action method enctype)))
    ("select" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes (name))
     (:hm--html-optional-attributes (size multiple)))
    ("textarea" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes (name rows cols))
     (:hm--html-optional-attributes nil))
    ("table" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes
      (align width border cellspacing cellpading)))
    ("caption" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align)))
    ("tr" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (align valign)))
    ("th" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes
      (nowrap rowspan colspan align valign width height)))
    ("td" (:hm--html-one-or-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes
      (nowrap rowspan colspan align valign width height)))
    ("tt" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("i" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("b" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("u" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("strike" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("big" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("small" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("sub" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("sup" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("em" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("strong" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("dfn" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("code" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("samp" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("kbd" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("var" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("cite" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("a" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (name href rel rev title)))
    ("applet" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes (code width height))
     (:hm--html-optional-attributes (codebase alt name align hspace vspace)))
    ("font" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes (size color)))
    ("map" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes (name))
     (:hm--html-optional-attributes nil))
    ("style" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    ("script" (:hm--html-two-element-tag t)
     (:hm--html-required-attributes nil)
     (:hm--html-optional-attributes nil))
    )
  "An alist with tag names known by the `hm--html-mode'.
CURRENTLY THIS LIST MIGHT NOT CONTAIN ALL TAGS!!!!.

It is used to determine, if a tag is a one element tag or not.

In the future it should also be used to get possible parameters of
the tag.

Use lower case characters in this list!!!!"
  :group 'hm--html-indentation
  :type 'list)
;  :type '(repeat lisp))
;  :type '(repeat (list string
;		       (list (const
;				:tag "Element with one tag"
;				:value (:hm--html-one-element-tag t))
;			       (const
;				:tag "Element with two tags"
;				:value (:hm--html-two-element-tag t))
;			       (const
;				:tag "Element with one or two tags"
;				:value (:hm--html-one-or-two-element-tag t))
;			       )
;		       (list :format "%t%v"
;			     :tag ""
;			     (const :format ""
;				    :value :hm--html-required-attributes)
;			     (repeat :tag "Repeat Required Attributes"
;				     symbol))
;		       (list :format "%t%v"
;			     :tag ""
;			     (const :format ""
;				    :value :hm--html-optional-attributes)
;			     (repeat :tag "Repeat Optional Attributes"
;				     symbol))
;		       )))


;;; Announce the feature hm--html-configuration
(provide 'hm--html-configuration)

