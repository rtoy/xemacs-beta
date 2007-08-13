;;; hm--html-configuration.el - Configurationfile for the html-mode
;;;
;;; $Id: hm--html-configuration.el,v 1.1.1.2 1996/12/18 03:46:47 steve Exp $
;;;
;;; Copyright (C) 1993, 1994, 1995, 1996  Heiko Muenkel
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
;;;	~/.hm--html-configuration.el, which preceeds the settings in
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


;;; The User config file (an proposal of Manoj Srivastava)
(defvar hm--html-user-config-file nil
  "*The location of the users config file.
This variable will only be used, if no environment variable
\"HTML_USER_CONFIG_FILE\" is set. 
Example value: \"~/.hm--html-configuration.el\".")


;;; Chose the initial popup menu
(defvar hm--html-expert nil
  "*t    : Use the HTML expert popup menu,
nil : Use the HTML novice (simple) menu.

NOTE: In the Emacs 19 you should set this variable only before 
      loading the mode.")

;;; Your Signature

(defvar hm--html-signature-file nil 
  "*Your Signature file.
For example: \"http://www.tnt.uni-hannover.de:80/data/info/www/tnt/info/tnt/whois/muenkel.html\".")


(defvar hm--html-username nil
  "*Your Name for the signature. For example: \"Heiko Münkel\".")


;;; Your favorite server (eg: the name of the host of your own http server)
;;; This is used in some other variables

(defvar hm--html-favorite-http-server-host-name "www.tnt.uni-hannover.de"
  "*The name of your favorite http server host. It must be specified !")


;;; For links to Info Gateways

(defvar hm--html-info-hostname:port-alist '(("www.tnt.uni-hannover.de:8005"))
  "*Alist with hostnames and ports for the Info gateway.")

(defvar hm--html-info-hostname:port-default "www.tnt.uni-hannover.de:8005"
  "*Default hostname with port for the Info gateway.")

(defvar hm--html-info-path-alist '((1 . "/appl/lemacs/Global/emacs/info")
				   (2 . "/appl/emacs/info")
				   (3 . "/appl/gnu/Global/info")
				   (4 . "/appl/emacs-19/Global/info")
				   (5 . "/"))
  "*Alist with directories for the Info gateway.")


;;; For links to WAIS Gateways

(defvar hm--html-wais-hostname:port-alist '(("www.tnt.uni-hannover.de:8001")
					    ("info.cern.ch:8001"))
  "*Alist with hostnames and ports for the WAIS gateway.")

(defvar hm--html-wais-hostname:port-default "www.tnt.uni-hannover.de:8001"
  "*Default hostname with port for the WAIS gateway.")

(defvar hm--html-wais-servername:port-alist 
  '(("wais.tnt.uni-hannover.de:210")
    ("daedalus.tnt.uni-hannover.de:21408")
    ("ikarus.tnt.uni-hannover.de:21401"))
  "*Alist with servernames and ports for the WAIS gateway.")

(defvar hm--html-wais-servername:port-default "www.tnt.uni-hannover.de:210"
  "*Default servername with port for the WAIS gateway.")

(defvar hm--html-wais-path-alist nil
  "*Alist with directories for the wais gateway.")


;;; For links to HTML servers

(defvar hm--html-html-hostname:port-alist '(("www.tnt.uni-hannover.de:80")
					    ("vxcrna.cern.ch:80")
					    ("www.ncsa.uiuc.edu:80"))
  "*Alist with hostnames and ports for the HTML server.")

(defvar hm--html-html-hostname:port-default "www.tnt.uni-hannover.de:80"
  "*Default hostname with port for the HTML server.")

(defvar hm--html-html-path-alist '((1 . "/data/info/www/tnt/")
				   (2 . "/data/info/www/")
				   (3 . "/data/info/")
				   (4 . "/data/")
				   (5 . "/appl/")
				   (6 . "/project/")
				   (7 . "~/")
				   (8 . "/"))
  "*Alist with directories for the HTML server.")


;;; For links to file gateways

(defvar hm--html-file-path-alist '((1 . "/data/info/www/tnt/")
				   (2 . "/data/info/www/")
				   (3 . "/data/info/")
				   (4 . "/data/")
				   (5 . "/appl/")
				   (6 . "/project/")
				   (7 . "~/")
				   (8 . "/"))
  "*Alist with directories for the file gateway.")


;;; For links to ftp servers

(defvar hm--html-ftp-hostname:port-alist '(("ftp.tnt.uni-hannover.de")
					   ("ftp.rrzn.uni-hannover.de")
					   ("wega.informatik.uni-hannover.de")
					   ("rusmv1.rus.uni-stuttgart.de")
					   ("export.lcs.mit.edu")
					   )
  "*Alist with hostnames and ports for the ftp server.")

(defvar hm--html-ftp-hostname:port-default "ftp.rrzn.uni-hannover.de"
  "*Default hostname with port for the ftp server.")

(defvar hm--html-ftp-path-alist '((1 . "/pub")
				  (2 . "/pub/gnu")
				  (3 . "/pub/linux")
				  (4 . "/pub/unix")
				  (5 . "/incoming")
				  (6 . "/"))
  "*Alist with directories for the ftp server.")


;;; For links to gopher servers

(defvar hm--html-gopher-hostname:port-alist
  '(("newsserver.rrzn.uni-hannover.de:70")
    ("solaris.rz.tu-clausthal.de:70")
    ("veronica.scs.unr.edu:70")
    ("pinus.slu.se:70")
    ("sunic.sunet.se:70")
    )
  "*Alist with hostnames and ports for the gopher server.")

(defvar hm--html-gopher-doctype-alist '(("/1")
					("/11")
					("/00"))
  "*Alist with doctype strings for the gopher server.")

(defvar hm--html-gopher-doctype-default "/1"
  "*Default doctype string for the gopher server.")

(defvar hm--html-gopher-hostname:port-default
  "newsserver.rrzn.uni-hannover.de:70"
  "*Default hostname with port for the gopher server.")

(defvar hm--html-gopher-anchor-alist
  '(("veronica")
    ("Wide%20Area%20Information%20Services%20databases")
    ("Subject%20Tree"))
  "*Alist with directories for the gopher server.")


;;; For the links to the Program Gateway

(defvar hm--html-proggate-hostname:port-alist
  '(("www.tnt.uni-hannover.de:8007")
    )
  "*Alist with hostnames and ports for the proggate server.")

(defvar hm--html-proggate-hostname:port-default "www.tnt.uni-hannover.de:8007"
  "*Default hostname with port for the proggate server.")

(defvar hm--html-proggate-allowed-file "/appl/www/bin/proggate.allowed"
  "*The filename (with path) of the proggate allowed file.")


;;; For links to the Local Program Gatewy

(defvar hm--html-local-proggate-path-alist '((1 . "/bin/")
					     (2 . "/usr/bin/")
					     (3 . "/usr/local/bin/")
					     (4 . "/appl/util/bin/")
					     (5 . "/appl/gnu/Global/bin/")
					     (6 . "/")
					     (7 . "/appl/")
					     (8 . "~/appl/Global/bin/")
					     (9 . "~/"))
  "*Alist with directories for the local program gateway.")


;;; For links to the mail gateway

(defvar hm--html-mail-hostname:port-alist '(("www.tnt.uni-hannover.de:8003")
					    )
  "*Alist with hostnames and ports for the mail gateway.")

(defvar hm--html-mail-hostname:port-default "www.tnt.uni-hannover.de:8003"
  "*Default hostname with port for the mail gateway.")

(defvar hm--html-mail-path-alist '((1 . "~/data/docs/mail")
				   (2 . "~/data/docs/news")
				   (3 . "~/docs/mail")
				   (4 . "~/docs/news")
				   (5 . "~/mail")
				   (6 . "~/news")
				   (7 . "~/")
				   (8 . "/data/info/mail")
				   (9 . "/data/info/news")
				   (10 . "/"))
  "*Alist with directories for the mail gateway.")


;;; For mailto links

(defvar hm--html-mailto-alist '(("muenkel@tnt.uni-hannover.de"))
  "*Alist with mail adresses for the mailto alist.
The value of `user-mail-address' will also be added by the package to
this alist.")


;;; For the server side include directive
;;; not sure, if these directives works on any server

(defvar hm--html-server-side-include-command-alist '(("/bin/date")
						     ("/usr/bin/finger")
						     ("/bin/df"))
  "*Alist with commands for the server side include directive.
These commands needs no parameter.")
	
(defvar hm--html-server-side-include-command-with-parameter-alist
  '(("/usr/bin/man")
    ("/usr/bin/finger")
    ("/usr/bin/ls")
    ("/bin/cat"))
  "*Alist with commands for the server side include directive.
These commands needs parameters.")
	

;;; Alist with URL'S for FORMS and IMAGE tags

(defvar hm--html-url-alist 
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
URL.")


;;; For the marking of examples in the help buffer

(defvar hm--html-help-foreground "red"
  "The foreground color to highlight examples.")

(defvar hm--html-help-background nil
  "The background color to highlight examples.")

(defvar hm--html-help-font (face-font 'bold)
  "The font to highlight examples.")


;;; For the Templates

(defvar hm--html-template-dir "/data/info/www/tnt/guide/templates"
  "*A directory with templatefiles")

(if (not (file-exists-p hm--html-template-dir))
    ;; Use a system directory, if the above one doesn't exist
    ;; This is only useful, in the XEmacs 19.12
    (setq hm--html-template-dir (concat data-directory
					"../lisp/hm--html-menus/")))

(defvar hm--html-frame-template-file (concat hm--html-template-dir
					     "frame.tmpl")
  "File, which is used as template for a html frame.")

(defvar hm--html-automatic-expand-templates t
  "*Automatic expansion of templates. This feature needs the file
tmpl-minor-mode.el from Heiko Muenkel (muenkel@tnt.uni-hannover.de),
which is distributed with the package hm--html-menus.")


;;; for deleting the automounter path-prefix
(defvar hm--html-delete-wrong-path-prefix '("/tmp_mnt" "/phys/[^/]+")
  "If non nil, it specifies path-prefixes, which should be deleted in pathes.
The Sun automounter adds a temporary prefix to the automounted directories
 (At our site the prefix is /tmp_mnt). But you can't select such a path, if 
the automounter has currently not mounted the directory and so you can't
follow a html-link, which consists of such a path. To overcome this behaviour,
you can set this variable to the prefix (eg. \"/tmp_mnt\"). After that, the
prefix should be stripped from the pathes during the creation of the links.
ATTENTION: This variable is used as regular expression !
It can be set to a string or to a list of strings.")


;;; For insertation of created and changed comments and automatic
;;; date update in the title line

(defvar hm--html-automatic-new-date t
  "*t   => The date in the title line will be updated before filesaving.
nil => No automatic update of the date.")

(defvar hm--html-automatic-changed-comment t
  "*t   => A \"changed comment\" line will be added before filesaving.
nil => No automatic insertation of a \"changed comment\" line.")

(defvar hm--html-automatic-created-comment t
  "*t   => A \"created comment\" line will be added.
nil => No automatic insertation of a \"created comment\" line.")


;;; Keybindings:

(defvar hm--html-bind-latin-1-char-entities t
  "Set this to nil, if you don't want to use the ISO Latin 1 charcter entities.
This is only useful, if `hm--html-use-old-keymap' is set to nil. It is only 
used during loading the html package the first time.")


;;; The drag and drop interface
(defvar hm--html-idd-create-relative-links t
  "If t, then the hm--html-idd-* functions are creating relative links.
Otherwise absolute links are used. The idd functions are used for
drag and drop.")

(defvar hm--html-idd-actions
  '((nil (((idd-major-mode-p . dired-mode)
	   (idd-dired-file-on-line-p . ".*\\.\\(gif\\)\\|\\(jpq\\)"))
	  hm--html-idd-add-include-image-from-dired-line)
	 (((idd-major-mode-p . dired-mode)
	   (idd-dired-no-file-on-line-p . nil))
	  hm--html-idd-add-file-link-to-file-on-dired-line)
	 (((idd-major-mode-p . dired-mode)
	   (idd-dired-no-file-on-line-p . t))
	  hm--html-idd-add-file-link-to-directory-of-buffer)
	 (((idd-major-mode-p . w3-mode)
	   (idd-url-at-point-p . t))
	  hm--html-idd-add-html-link-from-w3-buffer-point)
	 (((idd-major-mode-p . w3-mode))
	  hm--html-idd-add-html-link-to-w3-buffer)
	 (((idd-local-file-p . t))
	  hm--html-idd-add-file-link-to-buffer)))
  "The action list for the source mode `hm--html-mode'.
Look at the description of the variable idd-actions")


;;; The font lock keywords

(defvar hm--html-font-lock-keywords
  (list
   '("\\(<!--.*-->\\)\\|\\(<[^>]*>\\)+" . font-lock-comment-face)
   '("[Hh][Rr][Ee][Ff]=\"\\([^\"]*\\)\"" 1 font-lock-string-face t)
   '("[Ss][Rr][Cc]=\"\\([^\"]*\\)\"" 1 font-lock-string-face t))
  "Default expressions to highlight in the hm--html-mode.")


;;; The Prefix- Key for the keytables
(defvar hm--html-minor-mode-prefix-key "\C-z"
  "The prefix key for the keytables in the `hm--html-minor-mode'.")

(defvar hm--html-mode-prefix-key "\C-c"
  "The prefix key for the hm--html keys in the `hm--html-mode'.")


;;; The pulldown menu names
(defvar hm--html-minor-mode-pulldown-menu-name "HM-HTML"
  "The name of the pulldown menu in the minor html mode.")

(defvar hm--html-mode-pulldown-menu-name "HTML"
  "The name of the pulldown menu in the major html mode.")


;;; The hook variables
(defvar hm--html-load-hook nil
  "*Hook variable to execute functions after loading the package.")

(defvar hm--html-mode-hook nil
  "This hook will be called each time, when the hm--html-mode is invoked.")


;;; For the file html-view.el
;;; There are also some other variables in hmtl-view.el
;;; Look at that file, if you've trouble with the functions
;;; to preview the html document with the Mosaic
(defvar html-view-mosaic-command "/sol/www/bin/mosaic"
  "The command that runs Mosaic on your system")

(defvar html-sigusr1-signal-value 16
  "Value for the SIGUSR1 signal on your system.  
See, usually, /usr/include/sys/signal.h.
 	SunOS 4.1.x	: (setq html-sigusr1-signal-value 30)
	SunOS 5.x	: (setq html-sigusr1-signal-value 16)
	Linux		: (setq html-sigusr1-signal-value 10))")


;;; Announce the feature hm--html-configuration
(provide 'hm--html-configuration)

