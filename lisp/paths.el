;;; paths.el --- define pathnames for use by various Emacs commands.

;; Copyright (C) 1986, 1988, 1993, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; This file is dumped with XEmacs.

;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-start.el.  Do not change this file.

;;; Code:

;Note: FSF's version is:
;(defvar Info-default-directory-list
;  (let ((start (list "/usr/local/lib/info/"
;		      ;; This comes second so that, if it is the same
;		      ;; as configure-info-directory (which is usually true)
;		      ;; and Emacs has been installed (also usually true)
;		      ;; then the list will end with two copies of this;
;		      ;; which means that the last dir file Info-insert-dir
;		      ;; finds will be the one in this directory.
;		      "/usr/local/info/"))
;	 (configdir (file-name-as-directory configure-info-directory)))
;    (setq start (nconc start (list configdir)))
;    start)
;  "List of directories to search for Info documentation files.
;They are searched in the order they are given in this list.
;Therefore, the directory of Info files that come with Emacs
;normally should come last (so that local files override standard ones).")

;Our commented-out version is:
;(defvar Info-default-directory-list
;  (let ((start (list "/usr/local/info/"
;		     "/usr/local/lib/info/"))
;	(configdir (file-name-as-directory configure-info-directory)))
;    (or (member configdir start)
;	(setq start (nconc start (list configdir))))
;    (or (member (expand-file-name "../info/" data-directory) start)
;	(setq start
;	      (nconc start
;		     (list (expand-file-name "../info/" data-directory)))))
;    start)
;  "List of directories to search for Info documentation files.")

(defvar news-path "/usr/spool/news/"
  "The root directory below which all news files are stored.")

(defvar news-inews-program nil
  "Program to post news.")

;(defvar gnus-default-nntp-server ""
;  ;; set this to your local server
;  "The name of the host running an NNTP server.
;If it is a string such as \":DIRECTORY\", then ~/DIRECTORY
;is used as a news spool.  `gnus-nntp-server' is initialized from NNTPSERVER
;environment variable or, if none, this value.")

;(defvar gnus-nntp-service "nntp"
;  "NNTP service name, usually \"nntp\" or 119).
;Go to a local news spool if its value is nil, in which case `gnus-nntp-server'
;should be set to `(system-name)'.")

(defvar gnus-local-domain nil
  "*Your domain name without a host name: for example, \"ai.mit.edu\".
The DOMAINNAME environment variable is used instead if defined.
If the function `system-name' returns a fully qualified domain name,
there is no need to set this variable.")

(defvar gnus-local-organization nil
  "*The name of your organization, as a string.
The `ORGANIZATION' environment variable is used instead if defined.")

(defvar mh-progs nil
  "Directory containing MH commands.")

(defvar mh-lib nil
  "Directory of MH library.")

(defvar rmail-file-name (purecopy "~/RMAIL")
  "Name of user's primary mail file.")

(defvar gnus-startup-file (purecopy "~/.newsrc")
  "The file listing groups to which user is subscribed.
Will use `gnus-startup-file'-SERVER instead if exists.")

(defconst rmail-spool-directory nil
  "Name of directory used by system mailer for delivering new mail.
Its name should end with a slash.")

(defconst sendmail-program nil
  "Program used to send messages.")

(defconst remote-shell-program nil
  "Program used to execute shell commands on a remote machine.")

(defconst term-file-prefix
  (purecopy (if (eq system-type 'vax-vms) "[.term]" "term/"))
  "If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")

(defconst manual-program nil
  "Program to run to print man pages.")

(defconst abbrev-file-name 
  (purecopy (if (eq system-type 'vax-vms)
		"~/abbrev.def"
	      "~/.abbrev_defs"))
  "*Default name of file to read abbrevs from.")

(defconst directory-abbrev-alist
  (list
   ;;
   ;; This matches the default Sun automounter temporary mount points.  These
   ;; temporary mount points may go away, so it's important that we only try
   ;; to read files under the "advertised" mount point, rather than the
   ;; temporary one, or it will look like files have been deleted on us.
   ;; Whoever came up with this design is clearly a moron of the first order,
   ;; but now we're stuck with it, no doubt until the end of time.
   ;;
   ;; For best results, automounter junk should go near the front of this
   ;; list, and other user translations should come after it.
   ;;
   ;; You may need to change this if you're not running the Sun automounter,
   ;; if you're not running in the default configuration.  Because the
   ;; designers (and I use that term loosely) of the automounters failed to
   ;; provide any uniform way of disambiguating a pathname, emacs needs to
   ;; have knowledge about exactly how the automounter mangles pathnames
   ;; (and this knowledge is basically impossible to derive at run-time.)
   ;;
   (cons (purecopy "\\`/tmp_mnt/") (purecopy "/"))
   ))

;; Formerly, the values of these variables were computed once
;; (at dump time).  However, with the advent of pre-compiled binaries
;; and homebrewed systems such as Linux where who knows where the
;; hell the various programs may be located (if they even exist at all),
;; it's clear that we need to recompute these values at run time.
;; In typical short-sightedness, site administrators have been told up
;; till now to do `setq's in site-init.el, which is run only once --
;; at dump time.  So we have to do contortions to make sure we don't
;; override values set in site-init.el.

(defun initialize-xemacs-paths ()
  "Initialize the XEmacs path variables from the environment.
Called automatically at dump time and run time.  Do not call this.
Will not override settings in site-init.el or site-run.el."
  (let ((l #'(lambda (var value)
	       (let ((origsym (intern (concat "paths-el-original-"
					      (symbol-name var)))))
		 (if (running-temacs-p)
		     (progn
		       (set var value)
		       (set origsym value))
		   (and (eq (symbol-value var) (symbol-value origsym))
			(set var value)))))))
    (funcall
     l 'news-inews-program
     (cond ((file-exists-p "/usr/bin/inews") "/usr/bin/inews")
	   ((file-exists-p "/usr/local/inews") "/usr/local/inews")
	   ((file-exists-p "/usr/local/bin/inews") "/usr/local/bin/inews")
	   ((file-exists-p "/usr/lib/news/inews") "/usr/lib/news/inews")
	   (t "inews")))

    (funcall
     l 'mh-progs
     (cond ((file-directory-p "/usr/bin/mh") "/usr/bin/mh/") ;Ultrix 4.2
	   ((file-directory-p "/usr/new/mh") "/usr/new/mh/") ;Ultrix <4.2
	   ((file-directory-p "/usr/local/bin/mh") "/usr/local/bin/mh/")
	   ((file-directory-p "/usr/local/mh") "/usr/local/mh/")
	   (t "/usr/local/bin/")))

    (funcall
     l 'mh-libs
     (cond ((file-directory-p "/usr/lib/mh") "/usr/lib/mh/") ;Ultrix 4.2
	   ((file-directory-p "/usr/new/lib/mh")
	    "/usr/new/lib/mh/") ;Ultrix <4.2
	   ((file-directory-p "/usr/local/lib/mh") "/usr/local/lib/mh/")
	   (t "/usr/local/bin/mh/")))

    (funcall
     l 'rmail-spool-directory
     (cond ((string-match "^[^-]+-[^-]+-sco3.2v4" system-configuration)
	    "/usr/spool/mail/")
	   ;; On The Bull DPX/2 /usr/spool/mail is used although 
	   ;; it is usg-unix-v.
	   ((string-match "^m68k-bull-sysv3" system-configuration)
	    "/usr/spool/mail/")
	   ;; SVR4 and recent BSD are said to use this.
	   ;; Rather than trying to know precisely which systems use it,
	   ;; let's assume this dir is never used for anything else.
	   ((file-exists-p "/var/mail")
	    "/var/mail/")
	   ((memq system-type '(dgux hpux usg-unix-v unisoft-unix rtu irix))
	    "/usr/mail/")
	   ((memq system-type '(linux))
	    "/var/spool/mail/")
	   (t "/usr/spool/mail/")))

    (funcall
     l 'sendmail-program
     (cond
      ((file-exists-p "/usr/lib/sendmail") "/usr/lib/sendmail")
      ((file-exists-p "/usr/sbin/sendmail") "/usr/sbin/sendmail")
      ((file-exists-p "/usr/ucblib/sendmail") "/usr/ucblib/sendmail")
      (t "fakemail")))		;In ../etc, to interface to /bin/mail.

    (funcall
     l 'remote-shell-program
     (cond
      ;; Some systems use rsh for the remote shell; others use that
      ;; name for the restricted shell and use remsh for the remote
      ;; shell.  Let's try to guess based on what we actually find
      ;; out there.  The restricted shell is almost certainly in
      ;; /bin or /usr/bin, so it's probably safe to assume that an
      ;; rsh found elsewhere is the remote shell program.  The
      ;; converse is not true: /usr/bin/rsh could be either one, so
      ;; check that last.
      ((file-exists-p "/usr/ucb/remsh") "/usr/ucb/remsh")
      ((file-exists-p "/usr/bsd/remsh") "/usr/bsd/remsh")
      ((file-exists-p "/bin/remsh") "/bin/remsh")
      ((file-exists-p "/usr/bin/remsh") "/usr/bin/remsh")
      ((file-exists-p "/usr/local/bin/remsh") "/usr/local/bin/remsh")
      ((file-exists-p "/usr/ucb/rsh") "/usr/ucb/rsh")
      ((file-exists-p "/usr/bsd/rsh") "/usr/bsd/rsh")
      ((file-exists-p "/usr/local/bin/rsh") "/usr/local/bin/rsh")
      ((file-exists-p "/usr/bin/rcmd") "/usr/bin/rcmd")
      ((file-exists-p "/bin/rcmd") "/bin/rcmd")
      ((file-exists-p "/bin/rsh") "/bin/rsh")
      ((file-exists-p "/usr/bin/rsh") "/usr/bin/rsh")
      (t "rsh")))

    (funcall
     l 'manual-program
     ;; Solaris 2 has both of these files; prefer /usr/ucb/man
     ;; because the other has nonstandard argument conventions.
     (if (file-exists-p "/usr/ucb/man")
	 "/usr/ucb/man" "/usr/bin/man")))
)

(if (running-temacs-p)
    (initialize-xemacs-paths))

;;; paths.el ends here
