;; loadup.el --- load up standardly loaded Lisp files for XEmacs.

;; Copyright (C) 1985, 1986, 1992, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Richard Mlynarik.
;; Copyright (C) 1995, 1996, 2003 Ben Wing.

;; Maintainer: XEmacs Development Team
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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Last synched with FSF 19.30, with wild divergence since.

;;; Commentary:

;; If you are wanting to add files to be dumped into your local version of
;; XEmacs, DO NOT add them here.  Use site-init.el or site-load.el instead.

;; ***Note the docstrings for the variables in this file. They follow the
;; conventions described in lib-src/make-docfile.c, and any new variables or
;; functions added to this file should follow those conventions too, since
;; this file is always loaded uncompiled, and the byte-compiler never gets a
;; chance to format the docstrings in the way make-docfile.c understands.

;; This is loaded into a bare XEmacs to make a dumpable one.

;;; Code:

;; Help debug problems.
(setq stack-trace-on-error t
      load-always-display-messages t)

;(princ (format "command-line-args: %s\n" command-line-args))
;(princ (format "configure-lisp-directory: %S\n" configure-lisp-directory))
;(princ (format "configure-data-directory: %S\n" configure-data-directory))
;(princ (format "lisp-directory: %S\n" lisp-directory))

(when (fboundp 'error)
  (error "loadup.el already loaded!"))

(defconst running-xemacs t "\
Non-nil when the current emacs is XEmacs.")

;; Can't make this constant for now because it causes an error in
;; update-elc.el. 
(defvar source-lisp (file-name-directory (expand-file-name (nth 2 command-line-args))) "\
Root of tree containing the Lisp source code for the current build. 
Differs from `lisp-directory' if this XEmacs has been installed. ")

(defconst build-directory (expand-file-name ".." invocation-directory) "\
Root of tree containing object files and executables produced by build. 
Differs from `source-directory' if configured with --srcdir option, a practice 
recommended for developers.")

(defconst source-directory (expand-file-name ".." source-lisp)  "\
Root of tree containing source code for the current build. 
Used during loadup and for documenting source of symbols defined in C.")

(defvar preloaded-file-list nil "\
List of Lisp files preloaded into the XEmacs binary image,
with the exception of `loadup.el'.")

;(start-profiling)

(let ((gc-cons-threshold
       ;; setting it low makes loadup incredibly fucking slow.
       ;; no need to do it when not dumping.
       (if (and purify-flag
		(not (memq 'quick-build internal-error-checking)))
	   30000 3000000)))

;; really-early-error-handler outputs a stack trace so let's not do it
;; twice.
(let ((stack-trace-on-error nil))
  
;; This is awfully damn early to be getting an error, right?
(call-with-condition-handler 'really-early-error-handler
    #'(lambda ()
	(setq load-path (list source-lisp))
	(setq module-load-path (list 
				(expand-file-name "modules" build-directory)))

	;; message not defined yet ...
	(external-debugging-output (format "\nUsing load-path %s" load-path))
	(external-debugging-output (format "\nUsing module-load-path %s"
					   module-load-path))

	;; We don't want to have any undo records in the dumped XEmacs.
	(buffer-disable-undo (get-buffer "*scratch*"))

	;; lread.c (or src/Makefile.in.in) has prepended
	;; "${srcdir}/../lisp/" to load-path, which is how this file
	;; has been found.  At this point, enough of XEmacs has been
	;; initialized that we can start dumping "standard" lisp.
	;; Dumped lisp from external packages is added when we search
	;; the package path.
	;; #### This code is duplicated in two other places.
	(let ((temp-path (expand-file-name "." (car load-path))))
	  (setq load-path (nconc (mapcar
				  #'(lambda (i) (concat i "/"))
				  (directory-files temp-path t "^[^-.]"
						   nil 'dirs-only))
				 (cons (file-name-as-directory temp-path)
				       load-path))))

	(setq load-warn-when-source-only  t) ; Set to nil at the end

	;; garbage collect after loading every file in an attempt to
	;; minimize the size of the dumped image (if we don't do this,
	;; there will be lots of extra space in the data segment filled
	;; with garbage-collected junk)
	(defun pureload (file)
	  (let ((full-path
		 (locate-file file load-path
			      (if load-ignore-elc-files
				  '(".el" "") '(".elc" ".el" "")))))
	    (if full-path
		(prog1
		  (load full-path)
		  ;; but garbage collection really slows down loading.
		  (unless (memq 'quick-build internal-error-checking)
		    (garbage-collect)))
	      (external-debugging-output (format "\nLoad file %s: not found\n"
						 file))
	      ;; Uncomment in case of trouble
	      ;;(print (format "late-package-hierarchies: %S" late-package-hierarchies))
	      ;;(print (format "guessed-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name #'paths-emacs-root-p)))
	      ;;(print (format "guessed-data-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name #'paths-emacs-data-root-p)))
	      nil)))

	(load (expand-file-name "dumped-lisp.el" source-lisp))

	(let ((files preloaded-file-list)
	      file)
	  (while (setq file (car files))
	    (unless (pureload file)
	      (external-debugging-output "Fatal error during load, aborting")
	      (kill-emacs 1))
	    (setq files (cdr files)))
	  (when (not (featurep 'toolbar))
	    ;; else still define a few functions.
	    (defun toolbar-button-p    (obj) "No toolbar support." nil)
	    (defun toolbar-specifier-p (obj) "No toolbar support." nil))
	  (fmakunbound 'pureload))

	(packages-load-package-dumped-lisps late-package-load-path)

	)) ;; end of call-with-condition-handler

) ; (let ((stack-trace-on-error nil)))

;; Fix up the preloaded file list
(setq preloaded-file-list (mapcar #'file-name-sans-extension
				  preloaded-file-list))

(setq load-warn-when-source-only nil)

(setq debugger 'debug)

(when (member "no-site-file" command-line-args)
  (setq site-start-file nil))

;; If you want additional libraries to be preloaded and their
;; doc strings kept in the DOC file rather than in core,
;; you may load them with a "site-load.el" file.
;; But you must also cause them to be scanned when the DOC file
;; is generated.  For MS Windows, you must edit ../nt/xemacs.mak.
;; For other systems, you must edit ../src/Makefile.in.in.
(when (load "site-load" t)
  (garbage-collect)
)

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
(when (stringp site-start-file)
  (load "site-init" t))
;; Add information from this file to the load history:
(setq load-history (cons (nreverse current-load-list) load-history)
      ;; Clear current-load-list; this (and adding information to
      ;; load-history) is normally done in lread.c after reading the
      ;; entirety of a file, something which never happens for loadup.el.
      current-load-list nil)
;; Make the path to this file look a little nicer: 
(setcar (car load-history) (file-truename (caar load-history)))

(garbage-collect)

;;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

) ;; (let ((gc-cons-threshold [frequent garbage collection when dumping])))

;(stop-profiling)

;; Dump into the name `xemacs' (only)
(when (member "dump" command-line-args)
  (message "Dumping under the name xemacs")
  ;; This is handled earlier in the build process.
  ;; (condition-case () (delete-file "xemacs") (file-error nil))
  (when (fboundp 'really-free)
    (really-free))
  ;; Make sure we don't dump with debugging messages turned on.
  (setq stack-trace-on-error nil
	load-always-display-messages nil)
  (dump-emacs
   (cond
    ((featurep 'infodock) "infodock")
    ;; #### BILL!!!
    ;; If we want to dump under a name other than `xemacs', do that here!
    ;; ((featurep 'gtk) "xemacs-gtk")
    (t "xemacs"))
   "temacs")
  (kill-emacs))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

(when (member "run-temacs" command-line-args)
  (message "\nBootstrapping from temacs...")
  ;; Remove all args up to and including "run-temacs"
  (apply #'run-emacs-from-temacs (cdr (member "run-temacs" command-line-args)))
  ;; run-emacs-from-temacs doesn't actually return anyway.
  (kill-emacs))

;; XEmacs change
;; If you are using 'recompile', then you should have used -l loadup-el.el
;; so that the .el files always get loaded (the .elc files may be out-of-
;; date or bad).
(when (member "recompile" command-line-args)
  (setq command-line-args-left (cdr (member "recompile" command-line-args)))
  (batch-byte-recompile-directory)
  (kill-emacs))

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(when (not (fboundp 'dump-emacs))
  ;; Avoid loading loadup.el a second time!
  (setq command-line-args (cdr (cdr command-line-args)))
  (eval top-level))

;;; loadup.el ends here
