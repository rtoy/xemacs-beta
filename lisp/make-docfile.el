;;; make-docfile.el --- Cache docstrings in external file

;; Copyright (C) 1985, 1986, 1992-1995, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003 Ben Wing.

;; Author: Unknown
;; Maintainer: Steven L Baur <steve@xemacs.org>
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

;; Help debug problems.
(setq stack-trace-on-error t
      load-always-display-messages t)

(defvar options nil)
(defvar processed nil)
(defvar docfile nil)
(defvar docfile-buffer nil)
(defvar site-file-list nil)
(defvar docfile-out-of-date nil)

(defun message (fmt &rest args)
  (princ (apply #'format fmt args))
  (terpri))

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
		 (setq docfile (expand-file-name (car (cdr command-line-args)))))
	     (setq options (cons arg options))
	     (setq options (cons (expand-file-name (car (cdr command-line-args))) options)))
	    ((string-equal arg "-i") ; Set site files to scan
	     (setq site-file-list (car (cdr command-line-args))))
	    (t (setq done t)))
      (if (null done)
	  (setq command-line-args (cdr (cdr command-line-args)))))))
(setq options (nreverse options))

;; (message (concat "Options: " (prin1-to-string options)))

;; insert-file-contents-internal calls out to `format-decode' afterwards,
;; so it must be defined.  if non-zero, it tries to be a bunch more stuff
;; so say, "NOOOOOOOOOOOOO!  Basta!  Ca soufit!   Enough, already, OK?"
(defun format-decode (fuck me harder) 0)

;; Next process the list of C files.
(defun process-args (args)
  (while args
    (let ((arg (car args)))
      ;; When called from xemacs.mak, we need to do some frobbing on the
      ;; args given to us -- remove NEEDTODUMP and make-docfile.exe,
      ;; convert .obj files into .c files in the source directory,
      ;; handle response files (beginning with @, specifying arguments),
      ;; due to line-length limitations in the shell.
      (if (string-match "^@" arg)
	  ;; MS Windows response file
	  ;; no generate-new-buffer so use its implementation.
	  (let ((buf (get-buffer-create (generate-new-buffer-name "foo"))))
	    (set-buffer buf)
	    (insert-file-contents-internal (substring arg 1))
	    ;; now majorly grind up the response file.
	    ;; backslashes get doubled, quotes around strings,
	    ;; get rid of pesky CR's and NL's, and put parens around
	    ;; the whole thing so we have a valid list of strings.
	    (goto-char (point-max))
	    (insert "\")")
	    (goto-char (point-min))
	    (insert "(\"")
	    (while (search-forward "\\" nil t)
	      (replace-match "\\\\" nil t))
	    (goto-char (point-min))
	    (while (search-forward "\n" nil t)
	      (replace-match "" nil t))
	    (goto-char (point-min))
	    (while (search-forward "\r" nil t)
	      (replace-match "" nil t))
	    (goto-char (point-min))
	    (while (search-forward " " nil t)
	      (replace-match "\" \"" nil t))
	    (goto-char (point-min))
	    (process-args (read buf)))
	;; remove NEEDTODUMP and make-docfile.exe, convert .obj files into
	;; .c files in the source directory.
	(when (and (not (string-match "\\(NEEDTODUMP\\|\\.exe$\\)" arg))
		   (not (member arg processed)))
	  (when (string-match "\\(.*\\)\\.obj$" arg)
	    (setq arg (concat (file-name-nondirectory
			       ;; no match-string so use its implementation.
			       (substring arg (match-beginning 1)
					  (match-end 1)))
			      ".c")))
	  (if (and (null docfile-out-of-date)
		   (file-newer-than-file-p arg docfile))
	      (setq docfile-out-of-date t))
	  (setq processed (cons arg processed))))
      (setq args (cdr args)))))

(process-args command-line-args)

;; Then process the list of Lisp files.
(let ((build-root (expand-file-name ".." invocation-directory)))
  (setq load-path (list (expand-file-name "lisp" build-root))))

;; Then process the autoloads
(setq autoload-file-name "auto-autoloads.elc")
(load "very-early-lisp.el")
(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "dump-paths.el")
(load "raw-process.el")

(let (preloaded-file-list)
  (load (expand-file-name "../lisp/dumped-lisp.el"))

  (let ((package-preloaded-file-list
	 (packages-collect-package-dumped-lisps late-package-load-path)))

    (setq preloaded-file-list
	  (append package-preloaded-file-list
		  preloaded-file-list
		  packages-hardcoded-lisp)))

  (while preloaded-file-list
    (let ((arg0 (packages-add-suffix (car preloaded-file-list)))
	  arg)
      (setq arg (locate-library arg0))
      (if (null arg)
	  (progn
	  (message "Error: dumped file %s does not exist" arg0)
	  ;; Uncomment in case of difficulties
	  ;;(message "late-packages: %S" late-packages)
	  ;;(message "guessed-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name #'paths-emacs-root-p))
	  ;;(message "guessed-data-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name #'paths-emacs-data-root-p))
	  )
	(if (null (member arg processed))
	    (progn
	      (if (and (null docfile-out-of-date)
		       (file-newer-than-file-p arg docfile))
		  (setq docfile-out-of-date t))
	      (setq processed (cons arg processed)))))
      (setq preloaded-file-list (cdr preloaded-file-list)))))

;; Finally process the list of site-loaded files.
(if site-file-list
    (let (site-load-packages)
      (load site-file-list t t)
      (while site-load-packages
	(let ((arg (car site-load-packages)))
	  (if (null (member arg processed))
	      (progn
		(if (and (null docfile-out-of-date)
			 (file-newer-than-file-p arg docfile))
		    (setq docfile-out-of-date t))
		(setq processed (cons arg processed)))))
	(setq site-load-packages (cdr site-load-packages)))))

;(let ((autoloads (packages-list-autoloads-path)))
;  ;; (message (concat "Autoloads: " (prin1-to-string autoloads)))
;  (while autoloads
;    (let ((arg (car autoloads)))
;      (if (null (member arg processed))
;	  (progn
;	    ;; (message arg)
;	    (if (and (null docfile-out-of-date)
;		     (file-newer-than-file-p arg docfile))
;		(setq docfile-out-of-date t))
;	    (setq processed (cons arg processed))))
;      (setq autoloads (cdr autoloads)))))

;; Now fire up make-docfile and we're done

(setq processed (nreverse processed))

(terpri)

;(message (prin1-to-string (append options processed)))

(when docfile-out-of-date
  (condition-case nil
      (delete-file docfile)
    (error nil))
  (message "Spawning make-docfile ...")
  ;; (message (prin1-to-string (append options processed)))

  (setq exec-path (list (concat default-directory "../lib-src")))

  ;; (locate-file-clear-hashing nil)
  (if (memq system-type '(berkeley-unix next-mach))
      ;; Suboptimal, but we have a unresolved bug somewhere in the
      ;; low-level process code.  #### Now that we've switched to using
      ;; the regular asynch process code, we should try removing this.
      (call-process-internal
       "/bin/csh"
       nil
       t
       nil
       "-fc"
       (mapconcat
	#'identity
	(append
	 (list (concat default-directory "../lib-src/make-docfile"))
	 options processed)
	" "))
    ;; (message (prin1-to-string (append options processed)))
    (apply 'call-process-internal
	   ;; (concat default-directory "../lib-src/make-docfile")
	   "make-docfile"
	   nil
	   t
	   nil
	   (append options processed)))

  (message "Spawning make-docfile ...done")
  ;; (write-region-internal (point-min) (point-max) "/tmp/DOC")
  )
(message "DOC file is up to date")

(kill-emacs)

;;; make-docfile.el ends here
