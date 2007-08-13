;;; loadup.el --- load up standardly loaded Lisp files for XEmacs.

;; It is not a good idea to edit this file.  Use site-init.el or site-load.el
;; instead.
;;
;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1996 Richard Mlynarik.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Keywords: internal

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Last synched with FSF 19.30, with wild divergence since.

;;; Commentary:

;; This is loaded into a bare Emacs to make a dumpable one.

;;; Code:

(if (fboundp 'error)
    (error "loadup.el already loaded!"))

(define-function 'defalias 'define-function)

(call-with-condition-handler
      ;; This is awfully damn early to be getting an error, right?
      'really-early-error-handler
 #'(lambda ()
     ;; message not defined yet ...
     (external-debugging-output (format "\nUsing load-path %s" load-path))

     ;; We don't want to have any undo records in the dumped XEmacs.
     (buffer-disable-undo (get-buffer "*scratch*"))

     ;; lread.c (or src/Makefile.in.in) has prepended "${srcdir}/../lisp/prim"
     ;; to load-path, which is how this file has been found.  At this point,
     ;; enough of emacs has been initialized that we can call directory-files
     ;; and get the rest of the dirs (so that we can dump stuff from modes/
     ;; and packages/.)
     ;;
     (let ((temp-path (expand-file-name ".." (car load-path))))
       (setq source-directory temp-path)
       (setq load-path (nconc (directory-files temp-path t "^[^-.]"
					       nil 'dirs-only)
			      (cons temp-path load-path))))

     (setq load-warn-when-source-newer t ; set to nil at the end
	   load-warn-when-source-only  t)

     ;; garbage collect after loading every file in an attempt to
     ;; minimize the size of the dumped image (if we don't do this,
     ;; there will be lots of extra space in the data segment filled
     ;; with garbage-collected junk)
     (defmacro load-gc (file)
       (list 'prog1 (list 'load file) '(garbage-collect)))
     (load-gc "backquote") 		; needed for defsubst etc.
     (load-gc "bytecomp-runtime")	; define defsubst
     (load-gc "subr") 			; load the most basic Lisp functions
     (load-gc "replace") 		; match-string used in version.el.
     (load-gc "version.el")		; Ignore compiled-by-mistake version.elc
     (load-gc "cl")
     (load-gc "cmdloop")
     (or (fboundp 'recursive-edit) (load-gc "cmdloop1"))
     (load-gc "keymap")
     (load-gc "syntax")
     (load-gc "device")
     (load-gc "console")
     (load-gc "obsolete")
     (load-gc "specifier")
     (load-gc "faces")	; must be loaded before any make-face call
     ;;(load-gc "facemenu") #### not yet ported
     (load-gc "glyphs")
     (load-gc "objects")
     (load-gc "extents")
     (load-gc "events")
     (load-gc "text-props")
     (load-gc "process")
     (load-gc "frame") ; move up here cause some stuff needs it here
     (load-gc "map-ynp")
     (load-gc "simple")
     (load-gc "keydefs") ; Before loaddefs so that keymap vars exist.
     (load-gc "abbrev")
     (load-gc "derived")
     (load-gc "minibuf")
     (load-gc "list-mode")
     (load-gc "modeline") ; needs simple.el to be loaded first
     ;; If SparcWorks support is included some additional packages are
     ;; dumped which would normally have autoloads.  To avoid
     ;; duplicate doc string warnings, SparcWorks uses a separate
     ;; autoloads file with the dumped packages removed.
     ;; After fixing, eos/loaddefs-eos and loaddefs appear identical?!!
     ;; So just make loaddefs-eos go away...
     ;;(load-gc (if (featurep 'sparcworks) "eos/loaddefs-eos" "loaddefs"))
     (load-gc "font") ; required by widget
     (load-gc "widget")
     (load-gc "custom") ; Before loaddefs so that defcustom exists
     (load-gc "loaddefs")
     (load-gc "misc")
     (load-gc "profile")
     (load-gc "help")
     ;; (load-gc "hyper-apropos")  Soon...
     (when (not (featurep 'mule))
       (load-gc "files-nomule"))
     (load-gc "files")
     (load-gc "lib-complete")
     (load-gc "format")
     (load-gc "indent")
     (load-gc "isearch-mode")
     (load-gc "buffer")
     (load-gc "buff-menu")
     (load-gc "undo-stack")
     (load-gc "window")
     (load-gc "paths.el")		; don't get confused if paths compiled.
     (load-gc "startup")
     (load-gc "lisp")
     (load-gc "page")
     (load-gc "register")
     (load-gc "iso8859-1")		; This must be before any modes
                                        ; (sets standard syntax table.)
     (load-gc "paragraphs")
     (load-gc "lisp-mode")
     (load-gc "text-mode")
     (load-gc "fill")
     ;; (load-gc "cc-mode")		; as FSF goes so go we ..
     ;; (load-gc "scroll-in-place")	; We're not ready for this :-(
     ;; we no longer load buff-menu automatically.
     ;; it will get autoloaded if needed.
     
     (cond  ; Differences based on system-type
      ((eq system-type 'vax-vms)
       (load-gc "vmsproc")
       (load-gc "vms-patch"))
      ((eq system-type 'windows-nt)
       (load-gc "ls-lisp")
       (load-gc "winnt"))
      ((eq system-type 'ms-dos)
       (load-gc "ls-lisp")
       (load-gc "dos-fns")
       (load-gc "disp-table")))	; needed to setup ibm-pc char set,
				; see internal.el
     (when (featurep 'lisp-float-type)
       (load-gc "float-sup"))
     (load-gc "itimer") ; for vars auto-save-timeout and auto-gc-threshold
     (if (featurep 'toolbar)
	 (load-gc "toolbar")
       ;; else still define a few functions.
       (defun toolbar-button-p    (obj) "No toolbar support." nil)
       (defun toolbar-specifier-p (obj) "No toolbar support." nil))
     (when (featurep 'scrollbar)
       (load-gc "scrollbar"))
     (when (featurep 'menubar)
       (load-gc "menubar"))
     (when (featurep 'dialog)
       (load-gc "dialog"))
     (when (featurep 'mule)
       (load-gc "mule-load.el"))
     (when (featurep 'window-system)
       (load-gc "gui")
       (load-gc "mode-motion")
       (load-gc "mouse"))
     (when (featurep 'x)
       ;; preload the X code, for faster startup.
       (when (featurep 'menubar)
	 (load-gc "x-menubar")
	 ;; autoload this.
	 ;;(load-gc "x-font-menu")
	 )
       (load-gc "x-faces")
       (load-gc "x-iso8859-1")
       (load-gc "x-mouse")
       (load-gc "x-select")
       (when (featurep 'scrollbar)
	 (load-gc "x-scrollbar"))
       (load-gc "x-misc")
       (load-gc "x-init")
       (when (featurep 'toolbar)
	 (load-gc "x-toolbar"))
       )
     (when (featurep 'tty)
       ;; preload the TTY init code.
       (load-gc "tty-init"))
     (when (featurep 'tooltalk)
       (load-gc "tooltalk/tooltalk-load"))
     (load-gc "vc-hooks")
     (load-gc "ediff-hook")
     (load-gc "fontl-hooks")
     (load-gc "auto-show")
     (when (featurep 'energize)
       (load-gc "energize/energize-load.el"))
     (when (featurep 'sparcworks)
       (load-gc "sunpro/sunpro-load.el"))
     (fmakunbound 'load-gc)
     )) ;; end of call-with-condition-handler


(setq load-warn-when-source-newer t ; set to t at top of file
      load-warn-when-source-only nil)

(setq debugger 'debug)

(when (member "no-site-file" command-line-args)
  (setq site-start-file nil))

;; If you want additional libraries to be preloaded and their
;; doc strings kept in the DOC file rather than in core,
;; you may load them with a "site-load.el" file.
;; But you must also cause them to be scanned when the DOC file
;; is generated.  For VMS, you must edit ../../vms/makedoc.com.
;; For other systems, you must edit ../../src/Makefile.in.in.
(if (load "site-load" t)
    (garbage-collect))

;;FSFmacs randomness
;;(if (fboundp 'x-popup-menu)
;;    (precompute-menubar-bindings))
;;; Turn on recording of which commands get rebound,
;;; for the sake of the next call to precompute-menubar-bindings.
;(setq define-key-rebound-commands nil)


;; Note: all compiled Lisp files loaded above this point
;; must be among the ones parsed by make-docfile
;; to construct DOC.  Any that are not processed
;; for DOC will not have doc strings in the dumped XEmacs.

;; Don't bother with these if we're running temacs, i.e. if we're
;; just debugging don't waste time finding doc strings.

;; purify-flag is nil if called from loadup-el.el.
(when purify-flag
  (message "Finding pointers to doc strings...")
  (Snarf-documentation "DOC")
  (message "Finding pointers to doc strings...done")
  (Verify-documentation))

;; Note: You can cause additional libraries to be preloaded
;; by writing a site-init.el that loads them.
;; See also "site-load" above.
(if (stringp site-start-file)
    (load "site-init" t))
(setq current-load-list nil)
(garbage-collect)

;;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

;; Dump into the name `xemacs' (only)
(when (member "dump" command-line-args)
    (message "Dumping under the name xemacs")
  (condition-case () (delete-file "xemacs") (file-error nil))
  (when (fboundp 'really-free)
    (really-free))
  (dump-emacs "xemacs" "temacs")
  (kill-emacs))

(when (member "run-temacs" command-line-args)
  (message "\nBootstrapping from temacs...")
  (setq purify-flag nil)
  ;; Remove all args up to and including "run-temacs"
  (apply #'run-emacs-from-temacs (cdr (member "run-temacs" command-line-args)))
  ;; run-emacs-from-temacs doesn't actually return anyway.
  (kill-emacs))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

;; XEmacs change
;; If you are using 'recompile', then you should have used -l loadup-el.el
;; so that the .el files always get loaded (the .elc files may be out-of-
;; date or bad).
(when (member "recompile" command-line-args)
  (let ((command-line-args-left (cdr (member "recompile" command-line-args))))
    (batch-byte-recompile-directory)
    (kill-emacs)))

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(when (not (fboundp 'dump-emacs))
  ;; Avoid loading loadup.el a second time!
  (setq command-line-args (cdr (cdr command-line-args)))
  (eval top-level))

;;; loadup.el ends here
