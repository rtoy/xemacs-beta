;;; gtk-init.el --- initialization code for mswindows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for Gtk by: William Perry

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

(globally-declare-boundp
 '(gtk-initial-argv-list gtk-initial-geometry))

(globally-declare-fboundp
 '(gtk-keysym-on-keyboard-p))

(defvar gtk-early-lisp-options-file "~/.xemacs/gtk-options.el"
  "Path where GTK-specific early options should be stored.

This allows the user to set initial geometry without using GNOME and session
management, and, since it is read before GTK is initialized, it avoids
window flicker on resizing.

It is normally not useful to change without recompiling XEmacs.")

(defvar gtk-command-switch-alist
  '(
    ;; GNOME Options
    ("--disable-sound" . nil)
    ("--enable-sound"  . nil)
    ("--espeaker"      . t)

    ;; GTK Options
    ("--gdk-debug"    . t)
    ("--gdk-no-debug" . t)
    ("--display"      . t)
    ("--sync"         . nil)
    ("--no-xshm"      . nil)
    ("--name"         . t)
    ("--class"        . t)
    ("--gxid_host"    . t)
    ("--gxid_port"    . t)
    ("--xim-preedit"  . t)
    ("--xim-status"   . t)
    ("--gtk-debug"    . t)
    ("--gtk-no-debug" . t)
    ("--gtk-module"   . t)

    ;; Glib options
    ("--g-fatal-warnings" . nil)

    ;; Session management options
    ("--sm-client-id"     . t)
    ("--sm-config-prefix" . t)
    ("--sm-disable"       . t)
    )

  "An assoc list of command line args that should be in gtk-initial-argv-list.
This is necessary because GTK and GNOME consider it a fatal error if they
receive unknown command line arguments (perfectly reasonable).  But this
means that if the user specifies a file name on the command line they will
be unable to start.  So we filter the command line and allow only items in
this list in.

The CDR of the assoc list is whether it accepts an argument.  For the
moment, all options are in GNU long form.")

(defvar make-device-early-gtk-entry-point-called-p nil
  "Whether `make-device-early-gtk-entry-point' has been called, at least once.

Much of the GTK-specific Lisp init code should only be called the first time
a GTK device is created; this variable allows for that.")

(defvar make-device-late-gtk-entry-point-called-p nil
  "Whether `make-device-late-gtk-entry-point' has been called, at least once.

Much of the GTK-specific Lisp init code should only be called the first time
a GTK device is created; this variable allows for that.")

(defun make-device-early-gtk-entry-point ()
  "Entry point to set up the Lisp environment before GTK device creation."
  (unless make-device-early-gtk-entry-point-called-p
    (setq initial-frame-plist
          (and initial-frame-unmapped-p '(initially-unmapped t))
          gtk-initial-argv-list
          (cons (car command-line-args) (gtk-filter-arguments))
	  gtk-initial-geometry
          (nth 1 (member "-geometry" command-line-args-left))
	  make-device-early-gtk-entry-point-called-p t)
    (unless vanilla-inhibiting
      (load gtk-early-lisp-options-file t t t))))

(defun gtk-init-handle-geometry (arg)
  "Set up initial geometry info for GTK devices."
  (setq gtk-initial-geometry (pop command-line-args-left)))

(defun make-device-late-gtk-entry-point (device)
  "Entry-Point to do any Lisp-level GTK device-specific initialization."
  (unless make-device-late-gtk-entry-point-called-p
    (setq make-device-late-gtk-entry-point-called-p t)))

(defun gtk-filter-arguments ()
  (let ((accepted nil)
	(rejected nil)
	(todo nil))
    (setq todo (mapcar (lambda (argdesc)
			 (if (cdr argdesc)
			     ;; Need to look for --foo=bar
			     (concat "^" (car argdesc) "=")
			   ;; Just a simple arg
			   (concat "^" (regexp-quote (car argdesc)) "$")))
		       gtk-command-switch-alist))

    (while command-line-args-left
      (if (catch 'found
	    (mapc (lambda (r)
		    (if (string-match r (car command-line-args-left))
			(throw 'found t))) todo)
	    (mapc (lambda (argdesc)
		    (if (cdr argdesc)
			;; This time we only care about argument items
			;; that take an argument.  We'll check to see if
			;; someone used --foo bar instead of --foo=bar
			(if (string-match (concat "^" (car argdesc) "$") (car command-line-args-left))
			    ;; Yup!  Need to push
			    (progn
			      (push (pop command-line-args-left) accepted)
			      (throw 'found t)))))
		  gtk-command-switch-alist)
	    nil)
	  (push (pop command-line-args-left) accepted)
	(push (pop command-line-args-left) rejected)))
    (setq command-line-args-left (nreverse rejected))
    (nreverse accepted)))

(push '("-geometry" . gtk-init-handle-geometry) command-switch-alist)

