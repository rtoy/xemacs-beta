;;; update-elc.el --- Bytecompile out-of-date dumped files

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Sun Microsystems, Inc.
;; Copyright (C) 2001 Ben Wing.

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
;; Use this file like this:

;; temacs -batch -l ../lisp/update-elc.el $lisp

;; where $lisp comes from the Makefile.  .elc files listed in $lisp will
;; cause the corresponding .el file to be compiled.  .el files listed in
;; $lisp will be ignored.

;; (the idea here is that you can bootstrap if your .ELC files
;; are missing or badly out-of-date)

;; See also update-elc-2.el

;;; Code:

(defvar processed nil)
(defvar update-elc-files-to-compile nil)
(defvar need-to-rebuild-autoloads nil)
(defvar need-to-rebuild-mule-autoloads nil)
(defvar need-to-recompile-autoloads nil)
(defvar need-to-recompile-mule-autoloads nil)

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

(load "very-early-lisp" nil t)

(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "dump-paths.el")

(defun update-elc-chop-extension (file)
  (if (string-match "\\.elc?$" file)
      (substring file 0 (match-beginning 0))
    file))

;; we used to call packages-list-autoloads here, but it's false generality.
;; we need to handle each autoload file differently and there are only
;; two of them.

(let (preloaded-file-list site-load-packages need-to-dump dumped-exe
			  files-to-process)
  
  (load (expand-file-name "../lisp/dumped-lisp.el"))

  (setq dumped-exe
	(cond ((file-exists-p "../src/xemacs.exe") "../src/xemacs.exe")
	      ((file-exists-p "../src/xemacs") "../src/xemacs")
	      (t nil)))

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
  ;; bytecomp, byte-optimize, and autoload are mentioned in
  ;; packages-useful-lisp.
  (setq files-to-process (append packages-useful-lisp preloaded-file-list))
  (while files-to-process
    (let* ((arg (car files-to-process))
	   (arg-is-preloaded (member arg preloaded-file-list))
	   (arg-sans-extension (update-elc-chop-extension arg))
	   (full-arg (locate-library arg-sans-extension))
	   (full-arg-sans-extension
	    (if (null full-arg)
		(progn
		  (print (format "Error: Library file %s not found" arg))
		  ;; Uncomment in case of trouble
		  ;;(print (format "late-packages: %S" late-packages))
                  ;;(print (format "guessed-roots: %S"
                  ;;               (paths-find-emacs-roots
                  ;;                invocation-directory invocation-name)))
		  (kill-emacs))
	      (update-elc-chop-extension full-arg)))
	   (full-arg-el (concat full-arg-sans-extension ".el"))
	   (full-arg-elc (concat full-arg-sans-extension ".elc"))
	   (full-arg-dir (file-name-directory full-arg-el))
	   (autoload-file (expand-file-name "auto-autoloads.el" full-arg-dir))
	   (autoload-is-mule (string-match "[/\\]mule[/\\]$" full-arg-dir)))
	   
      ;; (print full-arg-el)

      ;; now check if .el or .elc is newer than the dumped exe.
      ;; if so, need to redump.
      (when (and dumped-exe arg-is-preloaded
		 (or (and (file-exists-p full-arg-el)
			  (file-newer-than-file-p full-arg-el dumped-exe))
		     (and (file-exists-p full-arg-elc)
			  (file-newer-than-file-p full-arg-elc dumped-exe))))
	(setq need-to-dump t))

      (when (or (not (file-exists-p autoload-file))
		(and (file-exists-p full-arg-el)
		     (file-newer-than-file-p full-arg-el autoload-file)))
	(if autoload-is-mule
	    (setq need-to-rebuild-mule-autoloads t)
	  (setq need-to-rebuild-autoloads t)))

      (if (and (not (member (file-name-nondirectory arg)
			    packages-unbytecompiled-lisp))
	       (not (member full-arg-el processed))
	       (file-exists-p full-arg-el)
	       (or (not (file-exists-p full-arg-elc))
		   (file-newer-than-file-p full-arg-el full-arg-elc)))
	  (setq processed (cons full-arg-el processed)))

      (setq files-to-process (cdr files-to-process))))

  (if need-to-dump
      (condition-case nil
	  (write-region-internal "foo" nil "../src/NEEDTODUMP")
	(file-error nil)))

  )

(when (or need-to-rebuild-autoloads
	  (not (file-exists-p "../lisp/auto-autoloads.el"))
	  (not (file-exists-p "../lisp/auto-autoloads.elc"))
	  (file-newer-than-file-p "../lisp/auto-autoloads.el"
				  "../lisp/auto-autoloads.elc"))
  (setq need-to-recompile-autoloads t))

(when (or need-to-rebuild-mule-autoloads
	  (not (file-exists-p "../lisp/mule/auto-autoloads.el"))
	  (not (file-exists-p "../lisp/mule/auto-autoloads.elc"))
	  (file-newer-than-file-p "../lisp/mule/auto-autoloads.el"
				  "../lisp/mule/auto-autoloads.elc"))
  (setq need-to-recompile-mule-autoloads t))

(when (not (featurep 'mule))
  ;; sorry charlie.
  (setq need-to-rebuild-mule-autoloads nil
	need-to-recompile-mule-autoloads nil))

(setq update-elc-files-to-compile (append update-elc-files-to-compile
					  (nreverse processed)))

;; (print update-elc-files-to-compile)

(if (and (not update-elc-files-to-compile)
	 (not need-to-rebuild-autoloads)
	 (not need-to-rebuild-mule-autoloads)
	 (not need-to-recompile-autoloads)
	 (not need-to-recompile-mule-autoloads))
    (condition-case nil
	(delete-file "../src/NOBYTECOMPILE")
      (file-error nil))
  (let* (
	 (bytecomp-arg (concat (update-elc-chop-extension
				(locate-library "bytecomp")) ".el"))
	 (byte-opt-arg (concat (update-elc-chop-extension
				(locate-library "byte-optimize")) ".el"))
	 (autoload-arg (concat (update-elc-chop-extension
				(locate-library "autoload")) ".el"))
	 )
    (setq inhibit-autoloads t)
    (setq command-line-args
	  (append
	   '("-l" "loadup-el.el" "run-temacs" "-batch" "-q" "-no-site-file")
	   ;; if bytecomp or byte-optimize need recompiling, then load
	   ;; the .el version of them first, recompile them, and reload
	   ;; the .elc versions to recompile everything else (so we won't
	   ;; be waiting until the cows come home).  we need to set
	   ;; load-ignore-elc-files because byte-optimize gets autoloaded
	   ;; from bytecomp.
	   (if (or (member bytecomp-arg update-elc-files-to-compile)
		   (member byte-opt-arg update-elc-files-to-compile))
	       (append
		'("-eval" "(setq load-ignore-elc-files t)" "-l" "bytecomp")
		(if (member bytecomp-arg update-elc-files-to-compile)
		    (append '("-f" "batch-byte-compile-one-file")
			    (list bytecomp-arg)))
		(if (member byte-opt-arg update-elc-files-to-compile)
		    (append '("-f" "batch-byte-compile-one-file")
			    (list byte-opt-arg)))
		'("-eval" "(setq load-ignore-elc-files nil)")))
	   '("-l" "bytecomp")
	   ;; likewise, recompile autoload.el if out-of-date.
	   (if (member autoload-arg update-elc-files-to-compile)
	       (append '("-f" "batch-byte-compile-one-file")
		       (list autoload-arg)))
	   ;; then generate autoloads for lisp and maybe lisp/mule.
	   (if (or need-to-rebuild-autoloads
		   need-to-rebuild-mule-autoloads)
	       '("-l" "autoload"))
	   (if need-to-rebuild-autoloads
	       '("-f" "batch-force-update-one-directory" "../lisp"))
	   (if need-to-rebuild-mule-autoloads
	       '("-f" "batch-force-update-one-directory" "../lisp/mule"))
	   (if need-to-recompile-autoloads
	       '("-f" "batch-byte-compile-one-file"
		 "../lisp/auto-autoloads.el"))
	   (if need-to-recompile-mule-autoloads
	       '("-f" "batch-byte-compile-one-file"
		 "../lisp/mule/auto-autoloads.el"))
	   ;; now load the autoloads and compile alles anderes.
	   '("-eval" "(setq inhibit-autoloads nil)"
	     "-f" "startup-load-autoloads"
	     "-f" "batch-byte-compile")
	   (delete autoload-arg
		   (delete byte-opt-arg
			   (delete bytecomp-arg
				   update-elc-files-to-compile))))))
  (load "loadup-el.el"))

(kill-emacs)

;;; update-elc.el ends here
