;;; make-docfile.el --- Cache docstrings in external file

;; Copyright (C) 1985, 1986, 1992-1995, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003 Ben Wing.

;; Author: Unknown
;; Maintainer: XEmacs Development Team
;; Keywords: internal

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

(defvar build-directory (expand-file-name ".." invocation-directory))
(defvar build-lib-src (expand-file-name "lib-src" build-directory))
(defvar source-lisp (file-name-directory (expand-file-name
					  (nth 2 command-line-args))))
(defvar source-src (expand-file-name "../src" source-lisp))

(defun message (fmt &rest args)
  (apply #'format-into standard-output fmt args)
  (write-char ?\n))

;; Gobble up the stuff we don't wish to pass on.
(setq command-line-args (cdr (cdr (cdr (cdr command-line-args)))))

;; First gather up the command line options.
(let (done)
  (while (and (eq done nil) command-line-args)
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
      (if (eq done nil)
	  (setq command-line-args (cdr (cdr command-line-args)))))))
(setq options (nreverse options))

;; (message (concat "Options: " (prin1-to-string options)))

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
	    (insert-file-contents-internal (subseq arg 1))
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
	(when (and (eq (string-match "\\(NEEDTODUMP\\|\\.exe$\\)" arg) nil)
		   (eq (member arg processed) nil))
	  (when (string-match "\\(.*\\)\\.obj$" arg)
	    (setq arg (expand-file-name
		       (concatenate
                        'string 
                        (file-name-nondirectory
                         ;; no match-string so use its implementation.
                         (subseq arg (match-beginning 1) (match-end 1)))
			".c")
		       source-src)))
	  (if (and (eq docfile-out-of-date nil)
		   (file-newer-than-file-p arg docfile))
	      (setq docfile-out-of-date t))
	  (setq processed (cons arg processed))))
      (setq args (cdr args)))))

;; Then process the list of Lisp files.
(process-args command-line-args)

(setq load-path (list source-lisp))

;; Then process the autoloads
(setq autoload-file-name "auto-autoloads.elc")
(defvar custom-declare-variable-list nil) ; unclean
(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "raw-process.el")

(let (preloaded-file-list arg0 arg package-preloaded-file-list absolute)
  (load (expand-file-name "dumped-lisp.el" source-lisp))

  (setq package-preloaded-file-list
	(packages-collect-package-dumped-lisps late-package-load-path)
	preloaded-file-list
	(append package-preloaded-file-list
		preloaded-file-list
		packages-hardcoded-lisp)
	  
	processed (cons "-d" processed)
	processed (cons source-lisp processed)
	;; Include loadup.el, which is never in preloaded-file-list:
	processed (cons "loadup.el" processed))

  (while preloaded-file-list
    (setq arg0 (packages-add-suffix (car preloaded-file-list))
	  arg (locate-library arg0)
          absolute arg)
    (if (eq arg nil)
	(progn
	  (message "Error: dumped file %s does not exist" arg0)
	  ;; Uncomment in case of difficulties
          ;(message "late-package-hierarchies: %S"
          ;         late-package-hierarchies)
          ;(message "guessed-roots: %S" (paths-find-emacs-roots
          ;                              invocation-directory
          ;                              invocation-name
          ;                              #'paths-emacs-root-p))
          ;(message "guessed-data-roots: %S" (paths-find-emacs-roots
          ;                                   invocation-directory
          ;                                   invocation-name
          ;                                   #'paths-emacs-data-root-p))
          )
      (when (equal arg (expand-file-name arg0 source-lisp))
	;; Use relative paths where possible, since this makes file lookup
	;; in an installed XEmacs easier:
	(setq arg arg0))
      (if (eq (member arg processed) nil)
	  (progn
	    (if (and (eq docfile-out-of-date nil)
                     ;; We need to check the absolute path here:
		     (file-newer-than-file-p absolute docfile))
		(setq docfile-out-of-date t))
	    (setq processed (cons arg processed)))))
    (setq preloaded-file-list (cdr preloaded-file-list))))

;; Finally process the list of site-loaded files.
(if site-file-list
    (let (site-load-packages)
      (load site-file-list t t)
      (while site-load-packages
	(let ((arg (car site-load-packages)))
	  (if (eq (member arg processed) nil)
	      (progn
		(if (and (eq docfile-out-of-date nil)
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

(write-sequence "\n")

;(message (prin1-to-string (append options processed)))

(when docfile-out-of-date
  (condition-case nil
      (delete-file docfile)
    (error nil))
  (message "Spawning make-docfile ...")
  ;; (message (prin1-to-string (append options processed)))

  (setq exec-path (list build-lib-src))

  ;; (locate-file-clear-hashing nil)
  ;; (message (prin1-to-string (append options processed)))
  (let* ((standard-error (get-buffer-create
                          (generate-new-buffer-name "stderr")))
         (status
          (apply 'call-process-internal
                 ;; exec-path is set.
                 ;; (expand-file-name "make-docfile" build-lib-src)
                 "make-docfile"
                 nil
                 ;; Wouldn't it be nice to have streams as process input and
                 ;; output.
                 (list t standard-error)
                 nil
                 (append options processed)))
         (numeric-status (if (integerp status) status 1)))
    (write-sequence (buffer-substring nil nil standard-error)
                    'external-debugging-output)
    (message "Spawning make-docfile ... %s"
             (if (equal 0 numeric-status) "done" status))
    (kill-emacs numeric-status)))

(kill-emacs)

;;; make-docfile.el ends here
