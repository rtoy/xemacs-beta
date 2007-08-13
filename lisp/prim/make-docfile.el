;;; make-docfile.el --- Cache docstrings in external file

;; Copyright (C) 1985, 1986, 1992-1995, 1997 Free Software Foundation, Inc.

;; Author: Unknown
;; Maintainer: Steven L Baur <steve@altair.xemacs.org>
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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This is a front-end to the make-docfile program that gathers up all the
;; lisp files that will be dumped with XEmacs.  It would probably be best
;; to just move make-docfile.c completely to lisp and be done with it.

;;; Code:

(defvar options nil)
(defvar processed nil)
(defvar docfile nil)
(defvar docfile-buffer nil)
(defvar site-file-list nil)
(defvar docfile-out-of-date nil)

;; BOGUS
(defvar find-file-hooks nil)

;; Gobble up the stuff we don't wish to pass on.
(setq command-line-args (cdr (cdr (cdr (cdr command-line-args)))))

;; First gather up the command line options.
(let (done)
  (while (and (null done) command-line-args)
    (let ((arg (car command-line-args)))
      (cond ((or (string-equal arg "-o") ; Specify DOC file name
		 (string-equal arg "-a") ; Append to DOC file
		 (string-equal arg "-d")) ; Set working directory
	     (if (string-equal arg "-o")
		 (setq docfile (car (cdr command-line-args))))
	     (setq options (cons arg options))
	     (setq options (cons (car (cdr command-line-args)) options)))
	    ((string-equal arg "-i") ; Set site files to scan
	     (setq site-file-list (car (cdr command-line-args))))
	    (t (setq done t)))
      (if (null done)
	  (setq command-line-args (cdr (cdr command-line-args)))))))
(setq options (nreverse options))

;; (print (concat "Options: " (prin1-to-string options)))

;; Next process the list of C files.
(while command-line-args
  (let ((arg (car command-line-args)))
    (if (null (member arg processed))
	(progn
	  (if (and (null docfile-out-of-date)
		   (file-newer-than-file-p arg docfile))
	      (setq docfile-out-of-date t))
	  (setq processed (cons arg processed)))))
  (setq command-line-args (cdr command-line-args)))

;; Then process the list of Lisp files.
(define-function 'defalias 'define-function)
(let ((temp-path (expand-file-name ".." (car load-path))))
  (setq load-path (nconc (directory-files temp-path t "^[^-.]"
					  nil 'dirs-only)
			 (cons temp-path load-path))))

;; Then process the autoloads
(setq autoload-file-name "auto-autoloads.elc")
(setq source-directory (concat default-directory "../lisp"))
;; (print (concat "Source directory: " source-directory))
(require 'packages)

;; We must have some lisp support at this point

;(load "backquote")
;(load "bytecomp-runtime")
;(load "subr")
;(load "replace")
;(load "version.el")
;(load "cl")

;; (load "featurep")

(let (dumped-lisp-packages)
 (load (concat default-directory "../lisp/prim/dumped-lisp.el"))
 (setq dumped-lisp-packages
       (append dumped-lisp-packages packages-hardcoded-lisp))
 (while dumped-lisp-packages
   (let ((arg0 (packages-add-suffix (car dumped-lisp-packages)))
	 arg)
     (setq arg (locate-library arg0))
     (if (null arg)
	 (princ (format "Error:  dumped file %s does not exist\n" arg))
       (if (null (member arg processed))
	   (progn
	     (if (and (null docfile-out-of-date)
		      (file-newer-than-file-p arg docfile))
		 (setq docfile-out-of-date t))
	     (setq processed (cons arg processed)))))
     (setq dumped-lisp-packages (cdr dumped-lisp-packages)))))

;; Finally process the list of site-loaded files.
(if site-file-list
    (let (site-load-packages)
      (load site-file-list t t)
      (while site-load-packages
	(let ((arg (car site-load-packages)))
	  (if (not (member arg processed))
	      (progn
		(if (and (null docfile-out-of-date)
			 (file-newer-than-file-p arg docfile))
		    (setq docfile-out-of-date t))
		(setq processed (cons arg processed)))))
	(setq site-load-packages (cdr site-load-packages)))))

(packages-find-packages package-path t)

(let ((autoloads (list-autoloads-path)))
  ;; (print (concat "Autoloads: " (prin1-to-string autoloads)))
  (while autoloads
    (let ((arg (car autoloads)))
      (if (null (member arg processed))
	  (progn
	    ;; (print arg)
	    (if (and (null docfile-out-of-date)
		     (file-newer-than-file-p arg docfile))
		(setq docfile-out-of-date t))
	    (setq processed (cons arg processed))))
      (setq autoloads (cdr autoloads)))))

;; Now fire up make-docfile and we're done

(setq processed (nreverse processed))

;; (print (prin1-to-string (append options processed)))

(if docfile-out-of-date
    (progn
      (princ "Spawning make-docfile ...")
      ;; (print (prin1-to-string (append options processed)))

      (setq exec-path (list (concat default-directory "../lib-src")))

      ;; (locate-file-clear-hashing nil)
      (if (eq system-type 'berkeley-unix)
	  ;; Suboptimal, but we have a unresolved bug somewhere in the
	  ;; low-level process code
	  (call-process-internal
	   "/bin/csh"
	   nil
	   t
	   nil
	   "-fc"
	   (mapconcat
	    'identity
	    (append
	     (list (concat default-directory "../lib-src/make-docfile"))
	     options processed)
	    " "))
	;; (print (prin1-to-string (append options processed)))
	(apply 'call-process-internal
	       ;; (concat default-directory "../lib-src/make-docfile")
	       "make-docfile"
	       nil
	       t
	       nil
	       (append options processed)))

      (princ "Spawning make-docfile ...done\n")
      ;; (write-region-internal (point-min) (point-max) "/tmp/DOC")
      )
  (princ "DOC file is up to date\n"))

(kill-emacs)

;;; make-docfile.el ends here
