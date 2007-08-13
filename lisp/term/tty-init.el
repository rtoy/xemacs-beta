;;; tty-init.el --- initialization code for tty's
;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing <wing@666.com>.

;; Author: various
;; Keywords: terminals

;;; This file is part of XEmacs.
;;;
;;; XEmacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; XEmacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(defvar pre-tty-win-initted nil)

;; called both from init-tty-win and from the C code.
(defun init-pre-tty-win ()
  "Initialize TTY at startup (pre).  Don't call this."
  (if (not pre-tty-win-initted)
      (progn
	(let ((esc (char-to-string 27)))
	  (register-tty-color "black" (concat esc "[30m") (concat esc "[40m"))
	  (register-tty-color "red" (concat esc "[31m") (concat esc "[41m"))
	  (register-tty-color "green" (concat esc "[32m") (concat esc "[42m"))
	  (register-tty-color "yellow" (concat esc "[33m") (concat esc "[43m"))
	  (register-tty-color "blue" (concat esc "[34m") (concat esc "[44m"))
	  (register-tty-color "magenta" (concat esc "[35m")
			      (concat esc "[45m"))
	  (register-tty-color "cyan" (concat esc "[36m") (concat esc "[46m"))
	  (register-tty-color "white" (concat esc "[37m") (concat esc "[47m"))

	  ;;  define some additional tty colors 
	  (register-tty-color "darkgrey"      "\e[1;30m" "\e[1;40m") 
	  (register-tty-color "brightred"     "\e[1;31m" "\e[1;41m") 
	  (register-tty-color "brightgreen"   "\e[1;32m" "\e[1;42m") 
	  (register-tty-color "brightyellow"  "\e[1;33m" "\e[1;43m") 
	  (register-tty-color "brightblue"    "\e[1;34m" "\e[1;44m") 
	  (register-tty-color "brightmagenta" "\e[1;35m" "\e[1;45m") 
	  (register-tty-color "brightcyan"    "\e[1;36m" "\e[1;46m") 
	  (register-tty-color "brightwhite"   "\e[1;37m" "\e[1;47m") 
	  )
	(setq pre-tty-win-initted t))))

;; called both from init-tty-win and from the C code.
;; we have to do this for every created TTY console.
(defun init-post-tty-win (console)
  "Initialize TTY at console creation time (post).  Don't call this."
  ;; load the appropriate term-type-specific Lisp file.
  ;; we don't do this at startup here so that the user can
  ;; override term-file-prefix. (startup.el does it after
  ;; loading the init file.)
  (and init-file-loaded
       ;; temporarily select the console so that the changes
       ;; to function-key-map are made for the right console.
       (let ((foobar (selected-console)))
	 (unwind-protect
	     (progn
	       (select-console console)
	       (load-terminal-library))
	   (select-console foobar)))))

(defvar tty-win-initted nil)

(defun init-tty-win ()
  "Initialize TTY at startup.  Don't call this."
  (if (not tty-win-initted)
      (progn
	(init-pre-tty-win)
	(make-tty-device nil nil)
	(init-post-tty-win (selected-console))
	(setq tty-win-initted t)))
  )

(defun make-frame-on-tty (tty &optional parms)
  "Create a frame on the TTY connection named TTY.
TTY should be a TTY device such as \"/dev/ttyp3\" (as returned by the `tty'
command in that TTY), or nil for the standard input/output of the running
XEmacs process.

PROPS should be an plist of properties, as in the call to `make-frame'.

This function opens a connection to the TTY or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on TTY: ")
  (if (equal tty "") (setq tty nil))
  (make-frame-on-device 'tty tty parms))
