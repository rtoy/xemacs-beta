;;; gtk-init.el --- initialization code for mswindows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for Gtk by: William Perry

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
  (gtk-initialize-compose device)
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

;;; Stuff to get compose keys working on GTK
(eval-when-compile
  (defmacro gtk-define-dead-key (key map device)
    `(when (gtk-keysym-on-keyboard-p ',key device)
       (define-key function-key-map [,key] ',map))))

(defun gtk-initialize-compose (device)
  "Enable compose processing"
  (autoload 'compose-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-acute-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-grave-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-cedilla-map    "gtk-compose" nil t 'keymap)
  (autoload 'compose-diaeresis-map  "gtk-compose" nil t 'keymap)
  (autoload 'compose-circumflex-map "gtk-compose" nil t 'keymap)
  (autoload 'compose-tilde-map	    "gtk-compose" nil t 'keymap)

  (when (gtk-keysym-on-keyboard-p 'multi-key device)
    (define-key function-key-map [multi-key] 'compose-map))

  ;; The dead keys might really be called just about anything, depending
  ;; on the vendor.  MIT thinks that the prefixes are "SunFA_", "D", and
  ;; "hpmute_" for Sun, DEC, and HP respectively.  However, OpenWindows 3
  ;; thinks that the prefixes are "SunXK_FA_", "DXK_", and "hpXK_mute_".
  ;; And HP (who don't mention Sun and DEC at all) use "XK_mute_".
  ;; Go figure.

  ;; Presumably if someone is running OpenWindows, they won't be using
  ;; the DEC or HP keysyms, but if they are defined then that is possible,
  ;; so in that case we accept them all.

  ;; If things seem not to be working, you might want to check your
  ;; /usr/lib/X11/XKeysymDB file to see if your vendor has an equally
  ;; mixed up view of what these keys should be called.

  ;; Canonical names:
  (gtk-define-dead-key acute			compose-acute-map device)
  (gtk-define-dead-key grave			compose-grave-map device)
  (gtk-define-dead-key cedilla			compose-cedilla-map device)
  (gtk-define-dead-key diaeresis		compose-diaeresis-map device)
  (gtk-define-dead-key circumflex		compose-circumflex-map device)
  (gtk-define-dead-key tilde			compose-tilde-map device)
  (gtk-define-dead-key degree			compose-ring-map device)

  ;; Sun according to MIT:
  (gtk-define-dead-key SunFA_Acute		compose-acute-map device)
  (gtk-define-dead-key SunFA_Grave		compose-grave-map device)
  (gtk-define-dead-key SunFA_Cedilla		compose-cedilla-map device)
  (gtk-define-dead-key SunFA_Diaeresis		compose-diaeresis-map device)
  (gtk-define-dead-key SunFA_Circum		compose-circumflex-map device)
  (gtk-define-dead-key SunFA_Tilde		compose-tilde-map device)

  ;; Sun according to OpenWindows 2:
  (gtk-define-dead-key Dead_Grave		compose-grave-map device)
  (gtk-define-dead-key Dead_Circum		compose-circumflex-map device)
  (gtk-define-dead-key Dead_Tilde		compose-tilde-map device)

  ;; Sun according to OpenWindows 3:
  (gtk-define-dead-key SunXK_FA_Acute		compose-acute-map device)
  (gtk-define-dead-key SunXK_FA_Grave		compose-grave-map device)
  (gtk-define-dead-key SunXK_FA_Cedilla		compose-cedilla-map device)
  (gtk-define-dead-key SunXK_FA_Diaeresis	compose-diaeresis-map device)
  (gtk-define-dead-key SunXK_FA_Circum		compose-circumflex-map device)
  (gtk-define-dead-key SunXK_FA_Tilde		compose-tilde-map device)

  ;; DEC according to MIT:
  (gtk-define-dead-key Dacute_accent		compose-acute-map device)
  (gtk-define-dead-key Dgrave_accent		compose-grave-map device)
  (gtk-define-dead-key Dcedilla_accent		compose-cedilla-map device)
  (gtk-define-dead-key Dcircumflex_accent	compose-circumflex-map device)
  (gtk-define-dead-key Dtilde			compose-tilde-map device)
  (gtk-define-dead-key Dring_accent		compose-ring-map device)

  ;; DEC according to OpenWindows 3:
  (gtk-define-dead-key DXK_acute_accent		compose-acute-map device)
  (gtk-define-dead-key DXK_grave_accent		compose-grave-map device)
  (gtk-define-dead-key DXK_cedilla_accent	compose-cedilla-map device)
  (gtk-define-dead-key DXK_circumflex_accent	compose-circumflex-map device)
  (gtk-define-dead-key DXK_tilde		compose-tilde-map device)
  (gtk-define-dead-key DXK_ring_accent		compose-ring-map device)

  ;; HP according to MIT:
  (gtk-define-dead-key hpmute_acute		compose-acute-map device)
  (gtk-define-dead-key hpmute_grave		compose-grave-map device)
  (gtk-define-dead-key hpmute_diaeresis		compose-diaeresis-map device)
  (gtk-define-dead-key hpmute_asciicircum	compose-circumflex-map device)
  (gtk-define-dead-key hpmute_asciitilde	compose-tilde-map device)

  ;; Empirically discovered on Linux XFree86 MetroX:
  (gtk-define-dead-key usldead_acute		compose-acute-map device)
  (gtk-define-dead-key usldead_grave		compose-grave-map device)
  (gtk-define-dead-key usldead_diaeresis	compose-diaeresis-map device)
  (gtk-define-dead-key usldead_asciicircum	compose-circumflex-map device)
  (gtk-define-dead-key usldead_asciitilde	compose-tilde-map device)

  ;; HP according to OpenWindows 3:
  (gtk-define-dead-key hpXK_mute_acute		compose-acute-map device)
  (gtk-define-dead-key hpXK_mute_grave		compose-grave-map device)
  (gtk-define-dead-key hpXK_mute_diaeresis	compose-diaeresis-map device)
  (gtk-define-dead-key hpXK_mute_asciicircum	compose-circumflex-map device)
  (gtk-define-dead-key hpXK_mute_asciitilde	compose-tilde-map device)

  ;; HP according to HP-UX 8.0:
  (gtk-define-dead-key XK_mute_acute		compose-acute-map device)
  (gtk-define-dead-key XK_mute_grave		compose-grave-map device)
  (gtk-define-dead-key XK_mute_diaeresis	compose-diaeresis-map device)
  (gtk-define-dead-key XK_mute_asciicircum	compose-circumflex-map device)
  (gtk-define-dead-key XK_mute_asciitilde	compose-tilde-map device)

  ;; Xfree86 seems to use lower case and a hyphen
  (gtk-define-dead-key dead-acute		compose-acute-map device)
  (gtk-define-dead-key dead-grave		compose-grave-map device)
  (gtk-define-dead-key dead-cedilla		compose-cedilla-map device)
  (gtk-define-dead-key dead-diaeresis		compose-diaeresis-map device)
  (gtk-define-dead-key dead-circum		compose-circumflex-map device)
  (gtk-define-dead-key dead-circumflex		compose-circumflex-map device)
  (gtk-define-dead-key dead-tilde		compose-tilde-map device))

