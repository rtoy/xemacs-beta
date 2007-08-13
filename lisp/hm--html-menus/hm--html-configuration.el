;;; hm--html-configuration.el - Configurationfile for the html-mode
;;; v3.6; 17 Sep 1995
;;; Copyright (C) 1993, 1994, 1995  Heiko Muenkel
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
;;;	This file is for the configuration of the html mode.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load path directories or
;;;	set the environment variable HTML_CONFIG_FILE to this file.
;;;	For example: 
;;;       setenv HTML_CONFIG_FILE "~/data/hm--htm-environment.el"
;;;	  if you have put this file in the directory "~/data/"
;;;


(provide 'hm--html-configuration)
(require 'adapt)


;; The User config file (an proposal of Manoj Srivastava)
(defvar hm--html-user-config-file nil
  "*The location of the users config file.
This variable will only be used, if no environment variable
\"HTML_USER_CONFIG_FILE\" is set. 
Example value: \"~/.hm--html-configuration.el\".")


;; Chose the initial popup menu


(defvar hm--html-expert nil
  "*t    : Use the HTML expert popup menu,
nil : Use the HTML novice (simple) menu.
The variable 'hm--html-marc must be nil. If 'hm--html-marc is
not nil, this variable has no effect.")


(defvar hm--html-marc nil
  "*t    : Use the HTML popup menu from Marc Andreessen.")


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

(defvar hm--html-info-hostname:port-alist nil
  "*Alist with hostnames and ports for the Info gateway.")

(setq hm--html-info-hostname:port-alist '(("www.tnt.uni-hannover.de:8005")))

(defvar hm--html-info-hostname:port-default nil
  "*Default hostname with port for the Info gateway.")

(setq hm--html-info-hostname:port-default "www.tnt.uni-hannover.de:8005")

(defvar hm--html-info-path-alist nil
  "*Alist with directories for the Info gateway.")

(setq hm--html-info-path-alist '((1 . "/appl/lemacs/Global/emacs/info")
				 (2 . "/appl/emacs/info")
				 (3 . "/appl/gnu/Global/info")
				 (4 . "/appl/emacs-19/Global/info")
				 (5 . "/")))


;;; For links to WAIS Gateways

(defvar hm--html-wais-hostname:port-alist nil
  "*Alist with hostnames and ports for the WAIS gateway.")

(setq hm--html-wais-hostname:port-alist '(("www.tnt.uni-hannover.de:8001")
					  ("info.cern.ch:8001")))

(defvar hm--html-wais-hostname:port-default nil
  "*Default hostname with port for the WAIS gateway.")

(setq hm--html-wais-hostname:port-default "www.tnt.uni-hannover.de:8001")

(defvar hm--html-wais-servername:port-alist nil
  "*Alist with servernames and ports for the WAIS gateway.")

(setq hm--html-wais-servername:port-alist 
      '(("wais.tnt.uni-hannover.de:210")
	("daedalus.tnt.uni-hannover.de:21408")
	("ikarus.tnt.uni-hannover.de:21401")))

(defvar hm--html-wais-servername:port-default nil
  "*Default servername with port for the WAIS gateway.")

(setq hm--html-wais-servername:port-default "www.tnt.uni-hannover.de:210")

(defvar hm--html-wais-path-alist nil
  "*Alist with directories for the wais gateway.")

(setq hm--html-wais-path-alist nil)


;;; For links to HTML servers

(defvar hm--html-html-hostname:port-alist nil
  "*Alist with hostnames and ports for the HTML server.")

(setq hm--html-html-hostname:port-alist '(("www.tnt.uni-hannover.de:80")
					  ("vxcrna.cern.ch:80")
					  ("www.ncsa.uiuc.edu:80")))

(defvar hm--html-html-hostname:port-default nil
  "*Default hostname with port for the HTML server.")

(setq hm--html-html-hostname:port-default "www.tnt.uni-hannover.de:80")

(defvar hm--html-html-path-alist nil
  "*Alist with directories for the HTML server.")

(setq hm--html-html-path-alist '((1 . "/data/info/www/tnt/")
				 (2 . "/data/info/www/")
				 (3 . "/data/info/")
				 (4 . "/data/")
				 (5 . "/appl/")
				 (6 . "/project/")
				 (7 . "~/")
				 (8 . "/")))



;;; For links to file gateways

(defvar hm--html-file-path-alist nil
  "*Alist with directories for the file gateway.")

(setq hm--html-file-path-alist '((1 . "/data/info/www/tnt/")
				 (2 . "/data/info/www/")
				 (3 . "/data/info/")
				 (4 . "/data/")
				 (5 . "/appl/")
				 (6 . "/project/")
				 (7 . "~/")
				 (8 . "/")))


;;; For links to ftp servers

(defvar hm--html-ftp-hostname:port-alist nil
  "*Alist with hostnames and ports for the ftp server.")

(setq hm--html-ftp-hostname:port-alist '(("ftp.tnt.uni-hannover.de")
					 ("ftp.rrzn.uni-hannover.de")
					 ("wega.informatik.uni-hannover.de")
					 ("rusmv1.rus.uni-stuttgart.de")
					 ("export.lcs.mit.edu")
					 ))

(defvar hm--html-ftp-hostname:port-default nil
  "*Default hostname with port for the ftp server.")

(setq hm--html-ftp-hostname:port-default "ftp.rrzn.uni-hannover.de")

(defvar hm--html-ftp-path-alist nil
  "*Alist with directories for the ftp server.")

(setq hm--html-ftp-path-alist '((1 . "/pub")
				(2 . "/pub/gnu")
				(3 . "/pub/linux")
				(4 . "/pub/unix")
				(5 . "/incoming")
				(6 . "/")))


;;; For links to gopher servers

(defvar hm--html-gopher-hostname:port-alist nil
  "*Alist with hostnames and ports for the gopher server.")

(setq hm--html-gopher-hostname:port-alist 
      '(("newsserver.rrzn.uni-hannover.de:70")
	("solaris.rz.tu-clausthal.de:70")
	("veronica.scs.unr.edu:70")
	("pinus.slu.se:70")
	("sunic.sunet.se:70")
	))

(defvar hm--html-gopher-doctype-alist nil
  "*Alist with doctype strings for the gopher server.")

(setq hm--html-gopher-doctype-alist
      '(("/1")
	("/11")
	("/00")))

(defvar hm--html-gopher-doctype-default nil
  "*Default doctype string for the gopher server.")

(setq hm--html-gopher-doctype-default "/1")

(defvar hm--html-gopher-hostname:port-default nil
  "*Default hostname with port for the gopher server.")

(setq hm--html-gopher-hostname:port-default 
      "newsserver.rrzn.uni-hannover.de:70")

(defvar hm--html-gopher-anchor-alist nil
  "*Alist with directories for the gopher server.")

(setq hm--html-gopher-anchor-alist 
      '(("veronica")
	("Wide%20Area%20Information%20Services%20databases")
	("Subject%20Tree")))


;;; For the links to the Program Gateway

(defvar hm--html-proggate-hostname:port-alist nil
  "*Alist with hostnames and ports for the proggate server.")

(setq hm--html-proggate-hostname:port-alist '(("www.tnt.uni-hannover.de:8007")
					      ))

(defvar hm--html-proggate-hostname:port-default nil
  "*Default hostname with port for the proggate server.")

(setq hm--html-proggate-hostname:port-default "www.tnt.uni-hannover.de:8007")


(defvar hm--html-proggate-allowed-file nil
  "*The filename (with path) of the proggate allowed file.")


(setq hm--html-proggate-allowed-file "/appl/www/bin/proggate.allowed")


;;; For links to the Local Program Gatewy

(defvar hm--html-local-proggate-path-alist nil
  "*Alist with directories for the local program gateway.")


(setq hm--html-local-proggate-path-alist '((1 . "/bin/")
					   (2 . "/usr/bin/")
					   (3 . "/usr/local/bin/")
					   (4 . "/appl/util/bin/")
					   (5 . "/appl/gnu/Global/bin/")
					   (6 . "/")
					   (7 . "/appl/")
					   (8 . "~/appl/Global/bin/")
					   (9 . "~/")))


;;; For links to the mail gateway

(defvar hm--html-mail-hostname:port-alist nil
  "*Alist with hostnames and ports for the mail gateway.")

(setq hm--html-mail-hostname:port-alist '(("www.tnt.uni-hannover.de:8003")
					  ))

(defvar hm--html-mail-hostname:port-default nil
  "*Default hostname with port for the mail gateway.")

(setq hm--html-mail-hostname:port-default "www.tnt.uni-hannover.de:8003")

(defvar hm--html-mail-path-alist nil
  "*Alist with directories for the mail gateway.")

(setq hm--html-mail-path-alist '((1 . "~/data/docs/mail")
				 (2 . "~/data/docs/news")
				 (3 . "~/docs/mail")
				 (4 . "~/docs/news")
				 (5 . "~/mail")
				 (6 . "~/news")
				 (7 . "~/")
				 (8 . "/data/info/mail")
				 (9 . "/data/info/news")
				 (10 . "/")))

;;; For mailto links

(defvar hm--html-mailto-alist nil
  "*Alist with mail adresses for the mailto alist.
The value of `user-mail-address' will also be added by the package to
this alist.")

(setq hm--html-mailto-alist '(("muenkel@tnt.uni-hannover.de")))


;;; For the server side include directive

(defvar hm--html-server-side-include-command-alist nil
  "*Alist with commands for the server side include directive.
These commands needs no parameter.")

(setq hm--html-server-side-include-command-alist
      '(("/bin/date")
	("/usr/bin/finger")
	("/bin/df")))
	
(defvar hm--html-server-side-include-command-with-parameter-alist nil
  "*Alist with commands for the server side include directive.
These commands needs parameters.")

(setq hm--html-server-side-include-command-with-parameter-alist
      '(("/usr/bin/man")
	("/usr/bin/finger")
	("/usr/bin/ls")
	("/bin/cat")))
	

;;; Alist with URL'S for FORMS and IMAGE tags

(defvar hm--html-url-alist nil
  "*Alist with URL's for FORMS and IMAGE tags. 
The cdr of each list contains symbols, which specifys the use of the
URL.")

(setq hm--html-url-alist (list
			  '("http://hoohoo.ncsa.uiuc.edu/htbin-post/post-query"
			    POST)
			  '("http://hoohoo.ncsa.uiuc.edu/htbin/query"
			    GET)
			  (list 
			   (concat "http://" 
				   hm--html-favorite-http-server-host-name
				   "/")
			   'IMAGE)))


;;; For the marking of examples in the help buffer

(defvar hm--html-help-foreground "red"
  "The foreground color to highlight examples.")

(defvar hm--html-help-background nil
  "The background color to highlight examples.")

(defvar hm--html-help-font (face-font 'bold)
  "The font to highlight examples.")


;(if (not (face-foreground 'hm--html-help-face))
;    (set-face-foreground 'hm--html-help-face "red"))


;;; For the Templates


(defvar hm--html-template-dir nil
  "*A directory with templatefiles")


(setq hm--html-template-dir "/data/info/www/tnt/guide/templates/") 

(if (not (file-exists-p hm--html-template-dir))
    ;; Use a system directory, if the above one doesn't exist
    ;; This is only useful, in the XEmacs 19.12
    (setq hm--html-template-dir (concat data-directory
					"../lisp/hm--html-menus/")))


(defvar hm--html-frame-template-file nil
  "File, which is used as template for a html frame.")

(setq hm--html-frame-template-file (concat hm--html-template-dir
					   "frame.tmpl"))
  

(defvar hm--html-automatic-expand-templates nil
  "*Automatic expansion of templates. This feature needs the file
tmpl-minor-mode.el from Heiko Muenkel (muenkel@tnt.uni-hannover.de),
which is distributed with the package hm--html-menus.")


(setq hm--html-automatic-expand-templates t)


;;; For font lock mode
;(defvar hm--html-font-lock-color 
;  (if (facep 'font-lock-comment-face)
;      (face-foreground 'font-lock-comment-face))
;  "*The color for the html font lock.")


;(setq hm--html-font-lock-color "grey80")


;;; for deleting the automounter path-prefix
(defvar hm--html-delete-wrong-path-prefix nil
  "If non nil, it specifies path-prefixes, which should be deleted in pathes.
The Sun automounter adds a temporary prefix to the automounted directories
 (At our site the prefix is /tmp_mnt). But you can't select such a path, if 
the automounter has currently not mounted the directory and so you can't
follow a html-link, which consists of such a path. To overcome this behaviour,
you can set this variable to the prefix (eg. \"/tmp_mnt\"). After that, the
prefix should be stripped from the pathes during the creation of the links.
ATTENTION: This variable is used as regular expression !
It can be set to a string or to a list of strings.")

(setq hm--html-delete-wrong-path-prefix '("/tmp_mnt" "/phys/[^/]+"))


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



;;; For the file html-mode.el

(setq html-document-previewer "/appl/www/bin/mosaic")


;;; For the file html-view.el

(setq html-view-mosaic-command "/appl/www/bin/mosaic")


;;; For the files html-mode.el and html-view.el

;; Value for the SIGUSR1 signal on your system.  See, usually,
;; /usr/include/sys/signal.h.
;; 	SunOS 4.1.x	: (setq html-sigusr1-signal-value 30)
;;	Linux		: (setq html-sigusr1-signal-value 10)
(setq html-sigusr1-signal-value 30)


;;; Keybindings:

(defvar hm--html-bind-latin-1-char-entities t
  "Set this to nil, if you don't want to use the ISO Latin 1 charcter entities.
This is only useful, if `hm--html-use-old-keymap' is set to nil. It is only 
used during loading the html package the first time.")

(defvar hm--html-use-old-keymap nil
  "Set this to t, if you want to use the old keymap from Marc.
This variable and the choice to use the old map will be deleted 
in the future.")

(if (not hm--html-use-old-keymap)
    ;; The new map
    (require 'hm--html-keys)

  ;; Additional bindings to the old map

;;; Keytable html-mode-map (see also the file html-mode.el)

(define-key html-mode-map "\C-cr" 'hm--html-add-link-target)
(define-key html-mode-map "\C-ch" 'hm--html-add-header)
(define-key html-mode-map "\C-ct" 'hm--html-add-title)
(define-key html-mode-map "\C-cn" 'hm--html-add-numberlist)
(define-key html-mode-map "\C-c\C-p" 'hm--html-add-preformated)
(define-key html-mode-map "\C-c\C-b" 'hm--html-add-bold)
(define-key html-mode-map "\C-c\C-i" 'hm--html-add-italic)
(define-key html-mode-map "\C-c\M-h" 'hm--html-add-html-link)
(define-key html-mode-map "\C-c\M-i" 'hm--html-add-info-link)
(define-key html-mode-map "\C-c\M-g" 'hm--html-add-gopher-link)
(define-key html-mode-map "\C-c\M-f" 'hm--html-add-file-link)
(define-key html-mode-map "\C-c\M-n" 'hm--html-add-news-link)
(define-key html-mode-map "\C-c\M-m" 'hm--html-add-mail-link)
;(define-key html-mode-map "\C-c\M-w" 'hm--html-add-wais-link)
(define-key html-mode-map "\C-c\M-w" 'hm--html-add-direct-wais-link)
(define-key html-mode-map "\C-c\M-t" 'hm--html-add-ftp-link)
(if (adapt-xemacsp)
    (define-key html-mode-map '(button3) 'hm--popup-html-menu)
  (define-key html-mode-map [mouse-3] 'hm--popup-html-menu)
  ;;(define-key html-mode-map [S-mouse-3] 'hm--popup-html-menu)
  ;;(define-key html-mode-map [C-mouse-3] 'hm--popup-html-menu-region)
  )

;;; Keytable html-region-mode-map (for the minor mode region-mode-map
;;; This minor mode is active, if a region is active

(if html-region-mode-map
    ()
  (setq html-region-mode-map (make-sparse-keymap))
  (define-key html-region-mode-map "\t" 'tab-to-tab-stop)
  (define-key html-region-mode-map "\C-ca" 'hm--html-add-address-to-region)
  (define-key html-region-mode-map "\C-cb" 'hm--html-add-blockquote-to-region)
  (define-key html-region-mode-map "\C-cc" 'hm--html-add-code-to-region)
  (define-key html-region-mode-map "\C-cd" 
    'hm--html-add-description-list-to-region)
  (define-key html-region-mode-map "\C-ch" 'hm--html-add-header-to-region)
  (define-key html-region-mode-map "\C-cl" 'html-add-normal-link-to-region)
  (define-key html-region-mode-map "\C-cm" 'hm--html-add-menu-to-region)
  (define-key html-region-mode-map "\C-cn" 'hm--html-add-numberlist-to-region)
  (define-key html-region-mode-map "\C-cr" 'html-add-reference-to-region)
  (define-key html-region-mode-map "\C-cs" 'hm--html-add-list-to-region)
  (define-key html-region-mode-map "\C-ct" 'hm--html-add-title-to-region)
  (define-key html-region-mode-map "\C-cx" 'hm--html-add-plaintext-to-region)
  (define-key html-region-mode-map "\C-c\C-b" 'hm--html-add-bold-to-region)
  (define-key html-region-mode-map "\C-c\C-c" 'hm--html-add-citation-to-region)
  (define-key html-region-mode-map "\C-c\C-e" 
    'hm--html-add-emphasized-to-region)
  (define-key html-region-mode-map "\C-c\C-f" 'hm--html-add-fixed-to-region)
  (define-key html-region-mode-map "\C-c\C-i" 'hm--html-add-italic-to-region)
  (define-key html-region-mode-map "\C-c\C-k" 'hm--html-add-keyboard-to-region)
  (define-key html-region-mode-map "\C-c\C-l" 'hm--html-add-listing-to-region)
  (define-key html-region-mode-map "\C-c\C-m" 'hm--html-add-sample-to-region)
  (define-key html-region-mode-map "\C-c\C-p"
    'hm--html-add-preformated-to-region)
  (define-key html-region-mode-map "\C-c\C-s" 'hm--html-add-strong-to-region)
  (define-key html-region-mode-map "\C-c\C-v" 'hm--html-add-variable-to-region)
  (define-key html-region-mode-map "<" 'html-less-than)
  (define-key html-region-mode-map ">" 'html-greater-than)
  (define-key html-region-mode-map "&" 'html-ampersand)
  (define-key html-region-mode-map "\C-c\C-rl" 'html-add-normal-link-to-region)
  (define-key html-region-mode-map "\C-c\C-rr" 'html-add-reference-to-region)
  (define-key html-region-mode-map "\C-c\M-h" 
    'hm--html-add-html-link-to-region)
  (define-key html-region-mode-map "\C-c\M-i" 
    'hm--html-add-info-link-to-region)
  (define-key html-region-mode-map "\C-c\M-g" 
    'hm--html-add-gopher-link-to-region)
  (define-key html-region-mode-map "\C-c\M-f" 
    'hm--html-add-file-link-to-region)
  (define-key html-region-mode-map "\C-c\M-n" 
    'hm--html-add-news-link-to-region)
  (define-key html-region-mode-map "\C-c\M-m"
    'hm--html-add-mail-link-to-region)
;  (define-key html-region-mode-map "\C-c\M-w" 
;    'hm--html-add-wais-link-to-region)
  (define-key html-region-mode-map "\C-c\M-w" 
    'hm--html-add-direct-wais-link-to-region)
  (define-key html-region-mode-map "\C-c\M-t" 
    'hm--html-add-ftp-link-to-region)
  (if (adapt-xemacsp)
      (define-key html-region-mode-map '(button3) 'hm--popup-html-menu-region)
    (define-key html-region-mode-map [mouse-3] 'hm--popup-html-menu-region))
)

)

