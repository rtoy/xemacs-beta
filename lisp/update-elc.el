;;; update-elc.el --- Bytecompile out-of-date dumped files

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Sun Microsystems, Inc.
;; Copyright (C) 2001, 2003 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Based On: Original by Steven L Baur <steve@xemacs.org>
;; Maintainer: XEmacs Development Team
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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Byte compile the .EL files necessary to dump out xemacs.
;; Also update the auto-autoloads.el files.

;; Use this file like this:

;; temacs -batch -l ../lisp/update-elc.el $lisp

;; where $lisp comes from the Makefile.  .elc files listed in $lisp will
;; cause the corresponding .el file to be compiled.  .el files listed in
;; $lisp will be ignored.

;; (the idea here is that you can bootstrap if your .ELC files
;; are missing or badly out-of-date)

;; See also update-elc-2.el

;;; Code:

;; Help debug problems.
(setq stack-trace-on-error t
      load-always-display-messages t)

(defvar processed nil)
(defvar update-elc-files-to-compile nil)
(defvar need-to-rebuild-autoloads nil)
(defvar need-to-rebuild-mule-autoloads nil)
(defvar need-to-recompile-autoloads nil)
(defvar need-to-recompile-mule-autoloads nil)
(defvar undumped-exe nil)
;(defvar dumped-exe nil)
(defvar dumped-exe-out-of-date-wrt-dump-files nil)
;(defvar dumped-exe-out-of-date-wrt-undumped-exe nil)

;(setq update-elc-files-to-compile
;      (delq nil
;	    (mapcar (function
;		     (lambda (x)
;		       (if (string-match "\.elc$" x)
;			   (let ((src (substring x 0 -1)))
;			     (if (file-newer-than-file-p src x)
;				 (progn
;				   (and (file-exists-p x)
;					(null (file-writable-p x))
;					(set-file-modes x (logior (file-modes x) 128)))
;				   src))))))
;		    ;; -batch gets filtered out.
;		    (nthcdr 3 command-line-args))))

(let ((build-root (expand-file-name ".." invocation-directory)))
  (setq load-path (list (expand-file-name "lisp" build-root))))

(load "very-early-lisp.el")
(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "dump-paths.el") ;; #### take out in my fixup ws

;; Lisp files loaded in order to byte compile anything.  If any are out of
;; date, we need to load them as .el's, byte compile them, and reload as
;; .elc's.
(defvar lisp-files-needed-for-byte-compilation
  '("bytecomp"
    "byte-optimize"))

;; Lisp files not in `lisp-files-needed-for-byte-compilation' that need
;; early byte compilation.  These are files loaded by update-elc.el in
;; order to do the compilation of all the rest of the files.
(defvar lisp-files-needing-early-byte-compilation
  '(;"easy-mmode"
    "autoload"
    "shadow"
    "cl-macs"))

(defvar unbytecompiled-lisp-files
  '("paths.el"
    "dumped-lisp.el"
    "dumped-pkg-lisp.el"
    "raw-process.el"
    "version.el"
    "very-early-lisp.el")
  "Lisp files that should not be byte compiled.")

(defvar lisp-files-ignored-when-checking-for-autoload-updating
  '("custom-load.el"
    "auto-autoloads.el")
  "Lisp files that should not trigger auto-autoloads rebuilding.")

(defun update-elc-chop-extension (file)
  (if (string-match "\\.elc?$" file)
      (substring file 0 (match-beginning 0))
    file))

;; we used to call packages-list-autoloads here, but it's false generality.
;; we need to handle each autoload file differently and there are only
;; two of them.

(let (preloaded-file-list site-load-packages files-to-process)
  
  (load (expand-file-name "../lisp/dumped-lisp.el"))

  (setq dumped-exe
	(cond ((file-exists-p "../src/xemacs.exe") "../src/xemacs.exe")
	      ((file-exists-p "../src/xemacs") "../src/xemacs")
	      (t nil)))

  ;; Not currently used but might be at some point.
;   (let ((temacs-exe
; 	 (cond ((file-exists-p "../src/temacs.exe") "../src/temacs.exe")
; 	       ((file-exists-p "../src/temacs") "../src/temacs")
; 	       (t nil)))
; 	(data-file
; 	 (cond ((file-exists-p "../src/xemacs.dmp") "../src/xemacs.dmp")
; 	       (t nil))))

;     ;; two setups here:
;     ;; (1) temacs.exe is undumped, dumped into xemacs.exe.  Happens with
;     ;;     unexec, but also with pdump under MS Windows native, since
;     ;;     the dumped data is stored as a resource in the xemacs.exe
;     ;;     executable.
;     ;; (2) xemacs.exe is dumped or undumped.  Running `xemacs -nd' gets
;     ;;     you the equivalent of `temacs'.  Dumping creates a file
;     ;;     `xemacs.dmp'.

;     (setq dumped-exe-out-of-date-wrt-undumped-exe
; 	  (cond ((not dumped-exe) t)
; 		(temacs-exe (file-newer-than-file-p temacs-exe dumped-exe))
; 		((not data-file) t)
; 		(t (file-newer-than-file-p dumped-exe data-file))))
;     (setq dumped-exe-exists (or (and temacs-exe dumped-exe)
; 				(and data-file dumped-exe))))

  ;; Path setup
  (let ((package-preloaded-file-list
	 (packages-collect-package-dumped-lisps late-package-load-path)))
 
    (setq preloaded-file-list
 	  (append package-preloaded-file-list
 		  preloaded-file-list
 		  packages-hardcoded-lisp)))

  (load (concat default-directory "../site-packages") t t)
  (setq preloaded-file-list
	(append packages-hardcoded-lisp
		preloaded-file-list
		site-load-packages))
  ;; bytecomp, byte-optimize, autoload, etc. are mentioned specially
  ;; in the lisp-files-need* variables.
  (setq files-to-process (append lisp-files-needed-for-byte-compilation
				 lisp-files-needing-early-byte-compilation
				 preloaded-file-list))
  (while files-to-process
    (let* ((arg (car files-to-process))
	   (arg-is-preloaded (member arg preloaded-file-list))
	   (arg-sans-extension (update-elc-chop-extension arg))
	   (full-arg (locate-library arg-sans-extension))
	   (full-arg-sans-extension
	    (if (null full-arg)
		(progn
		  (print (format "Error: Library file %s not found" arg))
		  (backtrace)
		  ;; Uncomment in case of trouble
		  ;;(print (format "late-packages: %S" late-packages))
                  ;;(print (format "guessed-roots: %S"
                  ;;               (paths-find-emacs-roots
                  ;;                invocation-directory invocation-name)))
		  (kill-emacs))
	      (update-elc-chop-extension full-arg)))
	   (full-arg-el (concat full-arg-sans-extension ".el"))
	   (full-arg-elc (concat full-arg-sans-extension ".elc"))
	   (full-arg-dir (file-name-directory full-arg-el)))
	   
      ;; (print full-arg-el)

      ;; now check if .el or .elc is newer than the dumped exe.
      ;; if so, need to redump.
      (when (and dumped-exe arg-is-preloaded
		 ;; no need to check for existence of either of the files
		 ;; because of the definition of file-newer-than-file-p.
		 (or (file-newer-than-file-p full-arg-el dumped-exe)
		     (file-newer-than-file-p full-arg-elc dumped-exe)))
	(setq dumped-exe-out-of-date-wrt-dump-files t))

      (if (and (not (member (file-name-nondirectory arg)
			    unbytecompiled-lisp-files))
	       (not (member full-arg-el processed))
	       ;; no need to check for existence of either of the files
	       ;; because of the definition of file-newer-than-file-p.
	       (file-newer-than-file-p full-arg-el full-arg-elc))
	  (setq processed (cons full-arg-el processed)))

      (setq files-to-process (cdr files-to-process))))

  ;; Check if we need to rebuild the auto-autoloads.el files -- that is,
  ;; if ANY .el files have changed.
  (let ((dirs-to-check '("../lisp" "../lisp/mule")))
    (while dirs-to-check
      (let* ((dir (car dirs-to-check))
	     (full-dir (expand-file-name dir))
	     (all-files-in-dir (directory-files full-dir t "\\.el$" nil t))
	     (autoload-file
	      (expand-file-name "auto-autoloads.el" full-dir))
	     (autoload-is-mule (equal dir "../lisp/mule")))
	(while all-files-in-dir
	  (let* ((full-arg (car all-files-in-dir)))
	    ;; custom-load.el always gets regenerated so don't let that
	    ;; trigger us.
	    (when (and (not
			(member
			 (file-name-nondirectory full-arg)
			 lisp-files-ignored-when-checking-for-autoload-updating
			 ))
		       (file-newer-than-file-p full-arg autoload-file))
	      (if autoload-is-mule
		  (setq need-to-rebuild-mule-autoloads t)
		(setq need-to-rebuild-autoloads t))))
	  (setq all-files-in-dir (cdr all-files-in-dir))))
      (setq dirs-to-check (cdr dirs-to-check))))

  (if dumped-exe-out-of-date-wrt-dump-files
      (condition-case nil
	  (write-region-internal "foo" nil "../src/NEEDTODUMP")
	(file-error nil)))

  )

(when (or need-to-rebuild-autoloads
	  ;; no real need for the following check either, because if the file
	  ;; doesn't exist, need-to-rebuild-autoloads gets set above.  but
	  ;; it's only one call, so it won't slow things down much and it keeps
	  ;; the logic cleaner.
	  (not (file-exists-p "../lisp/auto-autoloads.el"))
	  ;; no need to check for file-exists of .elc due to definition
	  ;; of file-newer-than-file-p
	  (file-newer-than-file-p "../lisp/auto-autoloads.el"
				  "../lisp/auto-autoloads.elc"))
  (setq need-to-recompile-autoloads t))

(when (or need-to-rebuild-mule-autoloads
	  ;; not necessary but ...  see comment above.
	  (not (file-exists-p "../lisp/mule/auto-autoloads.el"))
	  ;; no need to check for file-exists of .elc due to definition
	  ;; of file-newer-than-file-p
	  (file-newer-than-file-p "../lisp/mule/auto-autoloads.el"
				  "../lisp/mule/auto-autoloads.elc"))
  (setq need-to-recompile-mule-autoloads t))

(when (not (featurep 'mule))
  ;; sorry charlie.
  (setq need-to-rebuild-mule-autoloads nil
	need-to-recompile-mule-autoloads nil))

(setq update-elc-files-to-compile (append update-elc-files-to-compile
					  (nreverse processed)))

;(print update-elc-files-to-compile)

(let ((do-autoload-commands
       (append
	(if (or need-to-rebuild-autoloads
		need-to-rebuild-mule-autoloads)
	    '("-l" "autoload"))
	(if need-to-rebuild-autoloads
	    '("-f" "autoload-update-directory-autoloads"
	      "auto" "../lisp"))
	(if need-to-rebuild-mule-autoloads
	    '("-f" "autoload-update-directory-autoloads"
	      "mule" "../lisp/mule"))
	(if need-to-recompile-autoloads
	    '("-f" "batch-byte-compile-one-file"
	      "../lisp/auto-autoloads.el"))
	(if need-to-recompile-mule-autoloads
	    '("-f" "batch-byte-compile-one-file"
		   "../lisp/mule/auto-autoloads.el")))))
  (condition-case nil
      (delete-file "../src/REBUILD_AUTOLOADS")
    (file-error nil))
  (cond ((and (not update-elc-files-to-compile)
	      (not need-to-rebuild-autoloads)
	      (not need-to-rebuild-mule-autoloads)
	      (not need-to-recompile-autoloads)
	      (not need-to-recompile-mule-autoloads))
	 ;; (1) Nothing to do at all.  BYTECOMPILE_CHANGE is used (only by
	 ;;     the Unix makefile) to indicate whether some files needed
	 ;;     for dump got recompiled, and hence the executable must be
	 ;;     redumped.  We remove it if there were no files to compile.
	 (condition-case nil
	     (delete-file "../src/BYTECOMPILE_CHANGE")
	   (file-error nil)))
	((not update-elc-files-to-compile)
	 ;; (2) We have no files to byte-compile, but we do need to
	 ;;     regenerate and compile the auto-autoloads file, so signal
	 ;;     update-elc-2 to do it.  This is much faster than loading
	 ;;     all the .el's and doing it here. (We only need to rebuild
	 ;;     the autoloads here when we have files to compile, since
	 ;;     they may depend on the updated autoloads.)
	 (condition-case nil
	     (write-region-internal "foo" nil "../src/REBUILD_AUTOLOADS")
	   (file-error nil))
	 (condition-case nil
	     (delete-file "../src/BYTECOMPILE_CHANGE")
	   (file-error nil)))
	(t
	 (let ((bc-bootstrap
		(mapcar #'(lambda (arg) 
			    (concat (update-elc-chop-extension
				     (locate-library arg)) ".el"))
			lisp-files-needed-for-byte-compilation))
	       (bootstrap-other
		(mapcar #'(lambda (arg) 
			    (concat (update-elc-chop-extension
				     (locate-library arg)) ".el"))
			lisp-files-needing-early-byte-compilation)))
	   (setq inhibit-autoloads t)
	   ;; if bytecomp or byte-optimize need recompiling, then load
	   ;; the .el version of them first, recompile them, and reload
	   ;; the .elc versions to recompile everything else (so we won't
	   ;; be waiting until the cows come home).  we need to set
	   ;; load-ignore-elc-files because byte-optimize gets autoloaded
	   ;; from bytecomp.
	   (let ((recompile-bc-bootstrap
		  (apply #'nconc
			 (mapcar
			  #'(lambda (arg)
			      (when (member arg update-elc-files-to-compile)
				(append '("-f" "batch-byte-compile-one-file")
					(list arg))))
			  bc-bootstrap)))
		 (recompile-bootstrap-other
		  (apply #'nconc
			 (mapcar
			  #'(lambda (arg)
			      (when (member arg update-elc-files-to-compile)
				(append '("-f" "batch-byte-compile-one-file")
					(list arg))))
			  bootstrap-other))))
	     (mapc-internal
	      #'(lambda (arg)
		  (setq update-elc-files-to-compile
			(delete arg update-elc-files-to-compile)))
	      (append bc-bootstrap bootstrap-other))
	     (setq command-line-args
		   (append
		    '("-l" "loadup-el.el" "run-temacs"
		      "-batch" "-no-packages" "-no-autoloads"
		      "-eval" "(setq stack-trace-on-error t)"
		      "-eval" "(setq load-always-display-messages t)")
		    (when recompile-bc-bootstrap
		      (append
		       '("-eval" "(setq load-ignore-elc-files t)"
			 "-l" "bytecomp")
		       recompile-bc-bootstrap
		       '("-eval" "(setq load-ignore-elc-files nil)")))
		    '("-l" "bytecomp")
		    ;; likewise, recompile autoload.el etc. if out-of-date.
		    recompile-bootstrap-other
		    ;; then generate autoloads for lisp and maybe lisp/mule.
		    do-autoload-commands
		    ;; now load the autoloads and compile alles anderes.
		    '("-eval" "(setq inhibit-autoloads nil)"
		      "-f" "startup-load-autoloads"
		      "-f" "batch-byte-compile")
		    update-elc-files-to-compile
		    ))))

	 ;;(print command-line-args)
	 (load "loadup-el.el"))))

;(kill-emacs)

;;; update-elc.el ends here
