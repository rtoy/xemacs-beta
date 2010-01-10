;;; update-elc-2.el --- Recompile remaining .el files, post-dumping

;; Copyright (C) 1997 by Free Software Foundation, Inc.
;; Copyright (C) 2000, 2003 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>, based on cleantree.el by
;;         Steven L Baur <steve@xemacs.org>
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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file should be used after XEmacs has been dumped, to recompile
;; all remaining out-of-date .els and clean up orphaned .elcs.  It should
;; be called as
;;
;;   xemacs -batch -vanilla -l update-elc-2.el -f batch-update-elc-2 ${dirname}
;;
;; where ${dirname} is the directory tree to recompile, usually `lisp'.
;;
;; Note that this is very different from update-elc.el, which is called
;; BEFORE dumping, handles only the files needed to dump, and is called
;; from temacs instead of xemacs.
;;
;; The original cleantree.el had the comment: This code is derived
;; from Gnus based on a suggestion by David Moore <dmoore@ucsd.edu>

;;; Code:

;; Help debug problems.
(setq stack-trace-on-error t
      load-always-display-messages t)

(defvar update-elc-ignored-dirs
  `("." ".." "CVS" "SCCS" "RCS" ,@(unless (featurep 'mule) '("mule"))))

(defvar update-elc-ignored-files
  ;; note: entries here are regexps
  '("^," ;; #### huh?
    "^paths\\.el$"
    "^loadup\\.el$"
    "^loadup-el\\.el$"
    "^update-elc\\.el$"
    "^update-elc-2\\.el$"
    "^dumped-lisp\\.el$"
    "^make-docfile\\.el$"
    "^site-start\\.el$"
    "^site-load\\.el$"
    "^site-init\\.el$"))

(defvar dirfiles-table (make-hash-table :test 'equal))

;; SEEN accumulates the list of already-handled dirs.
(defun do-update-elc-2 (dir compile-stage-p seen)
  (setq dir (file-name-as-directory dir))
  ;; Only scan this sub-tree if we haven't been here yet.
  (unless (member (file-truename dir) seen)
    (push (file-truename dir) seen)

    (let ((files (or (gethash dir dirfiles-table)
		     (directory-files dir t nil t))))

      ;; Do this directory.
      (if compile-stage-p
	  ;; Stage 2: Recompile necessary .els
	  (dolist (file files)
	    (when (string-match "\\.el$" file)
	      (let ((file-c (concat file "c")))
		(when (and (not (member file-c files))
			   ;; no need to check for out-of-date-ness because
			   ;; that was already done, and .elc removed.
			   (let (ignore)
			     (mapcar
			      #'(lambda (regexp)
				  (if (string-match
				       regexp
				       (file-name-nondirectory file))
				      (setq ignore t)))
			      update-elc-ignored-files)
			     (not ignore)))
		  (byte-compile-file file)))))

	;; Stage 1.
	;; Remove out-of-date elcs
	(let (deleted)
	  (dolist (file files)
	    (when (string-match "\\.el$" file)
	      (let ((file-c (concat file "c")))
		(when (and (member file-c files)
			   (file-newer-than-file-p file file-c))
		  (message "Removing out-of-date %s" file-c)
		  (delete-file file-c)
		  (push file-c deleted)))))

	;; Remove elcs without corresponding el
	(dolist (file-c files)
	  (when (string-match "\\.elc$" file-c)
	    (let ((file (replace-in-string file-c "c$" "")))
	      (when (not (member file files))
		(message "Removing %s; no corresponding .el" file-c)
		(delete-file file-c)
		(push file-c deleted)))))

	(setq files (set-difference files deleted :test 'equal))))

      (puthash dir files dirfiles-table)

      ;; We descend recursively.  On my Windows machine, it is much faster
      ;; to call directory-files again to recompute than to call
      ;; file-directory-p on each member of the files list.
      (dolist (dir (directory-files dir t nil t 'dir))
	(when (not (member (file-name-nondirectory dir)
			   update-elc-ignored-dirs))
	  (do-update-elc-2 dir compile-stage-p seen))))))

(defun batch-update-elc-2 ()
  (defvar command-line-args-left)
  (unless noninteractive
    (error "`batch-update-elc-2' is to be used only with -batch"))
  (let ((dir (car command-line-args-left)))
    ;; don't depend on being able to autoload `update-autoload-files'!
    (load "autoload")
    (load "bytecomp")
    (load "byte-optimize")
    ;; #### the API used here is deprecated, convert to one with explicit
    ;; arguments when it is available
    ;; update-elc.el signals us to rebuild the autoloads when necessary.
    ;; in some cases it will rebuild the autoloads itself, but doing it this
    ;; way is slow, so we avoid it when possible.
    (when (file-exists-p (expand-file-name "REBUILD_AUTOLOADS"
					   invocation-directory))
      ;; if we were instructed to rebuild the autoloads, force the file
      ;; to be touched even w/o changes; otherwise, we won't ever stop
      ;; being told to rebuild them.
      (update-autoload-files dir "auto" nil t)
      (byte-recompile-file (expand-file-name "auto-autoloads.el" dir) 0)
      (when (featurep 'mule)
	(let ((muledir (expand-file-name "../lisp/mule" (file-truename dir))))
	  ;; force here just like above.
	  (update-autoload-files muledir "mule" nil t)
	  (byte-recompile-file (expand-file-name "auto-autoloads.el" dir) 0))))
    (when (featurep 'modules)
      (let* ((moddir (expand-file-name "../modules" (file-truename dir)))
	     (autofile (expand-file-name "auto-autoloads.el" moddir)))
	(update-autoload-files 
	 (delete (concat (file-name-as-directory moddir) ".")
		 (delete (concat (file-name-as-directory moddir) "..")
			 (directory-files moddir t nil nil 0)))
	 "modules" autofile)
	(byte-recompile-file autofile 0)))
    ;; now load the (perhaps newly rebuilt) autoloads; we were called with
    ;; -no-autoloads so they're not already loaded.
    (load (expand-file-name "auto-autoloads" lisp-directory))
    (when (featurep 'mule)
      (load (expand-file-name "mule/auto-autoloads" lisp-directory)))
    ;; We remove all the bad .elcs before any byte-compilation, because
    ;; there may be dependencies between one .el and another (even across
    ;; directories), and we don't want to load an out-of-date .elc while
    ;; byte-compiling a file.
    (message "Removing old or spurious .elcs in directory tree `%s'..." dir)
    (do-update-elc-2 dir nil nil)
    (message "Removing old or spurious .elcs in directory tree `%s'...done"
	     dir)
    (message "Recompiling updated .els in directory tree `%s'..." dir)
    (do-update-elc-2 dir t nil)
    (message "Recompiling updated .els in directory tree `%s'...done" dir)
    ;; likewise here.
    (load "cus-dep")
    (Custom-make-dependencies dir)
    (byte-recompile-file (expand-file-name "custom-load.el" dir) 0)
    (when (featurep 'mule)
      (Custom-make-dependencies (expand-file-name "mule" dir))
      (byte-recompile-file (expand-file-name "mule/custom-load.el" dir) 0)
      ;; See the eval-when-compile in the definition of
      ;; Installation-file-coding-system; if the file name sniffing or the
      ;; available coding systems have changed, version.elc should be
      ;; rebuilt.
      (byte-recompile-file (expand-file-name "version.el" dir) 0)))
  (setq command-line-args-left nil))

;;; update-elc-2.el ends here
