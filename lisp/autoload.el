;;; autoload.el --- maintain autoloads in auto-autoloads files.

;; Copyright (C) 1991, 1992, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1996, 2000, 2002, 2003 Ben Wing.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Keywords: maint

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; This code keeps auto-autoloads.el files up to date.  It interprets
;; magic cookies (of the form ";;;###autoload" in Lisp source files
;; and "/* ###autoload */" in C source files) in various useful ways.

;; Usage
;; =====

;; Recommended usage for this library, as implemented in the core
;; build process, is

;; xemacs -no-packages -batch \
;;        -eval "(setq generated-autoload-file \"PATH\")" \
;;        -l autoload -f autoload-update-directory-autoloads PREFIX DIRECTORY

;; which causes XEmacs to update the file named by PATH from the .el
;; files in DIRECTORY (but not recursing into subdirectories) and (if
;; the autoload file is not already protected with a feature test) add
;; a check and provide for 'PREFIX-autoloads.  Currently there is no
;; sanity check for the provided feature; it is recommended that you
;; nuke any existing auto-autoloads.el before running the command.

;; There is not yet a recommended API for updating multiple directories
;; into a single auto-autoloads file.  Using the recipe above for each
;; DIRECTORY with the same PATH should work but has not been tested.
;; There is no API for recursing into subdirectories.  There probably
;; won't be; given the wide variety of ways that existing Lisp packages
;; arrange their files, and the fact that source packages and installed
;; packages have a somewhat different directory structure, this seems far
;; too risky.  Use a script or a Lisp library with an explicit list of
;; PATHs; see update-elc.el for how to do this without recursive invocation
;; of XEmacs).

;; The probable next step is to fix up the packages to use the
;; `autoload-update-directory-autoloads' API.  However, for backward
;; compatibility with XEmacs 21.4 and 21.1, this can't be done quickly.

;; For now the API used in update-elc-2.el:

;; (let* ((dir "DIRECTORY")
;;        (generated-autoload-file (expand-file-name "auto-autoloads.el" dir))
;;        (autoload-package-name "PREFIX"))
;;   (update-autoload-files (list muledir))
;;   (byte-recompile-file generated-autoload-file 0))

;; is available, but this ugly kludge is deprecated.  It will be removed
;; in favor of using proper arguments instead of special variables.

;; For backward compatibility the API used in the packages/XEmacs.rules:

;; xemacs -vanilla -batch -eval "$(AUTOLOAD_PACKAGE_NAME)" \
;;        -l autoload -f batch-update-directory $(AUTOLOAD_PATH)

;; is supported, and the implementation is unchanged.  However,
;; revision of the API (in a backward compatible way) and the
;; implementation are planned, and until those stabilize it is too
;; risky to use this version of XEmacs for package releases.

;; Implementation:
;; ===============

;; #### This section should be moved to the Internals manual, or
;; (maybe) the Lispref, and integrated with the information above.
;; Don't believe anything written here; the code is still a mess, and
;; this is a lot of guesswork.

;; Autoloads are used in a number of contexts, including core Lisp,
;; packaged Lisp, and ELLs (dynamically loadable compiled objects
;; providing Lisp functionality).  There two general strategies for
;; collecting autoloads.  The first is to put autoloads for a package
;; in a package-specific auto-autoloads file.  This is easy to
;; implement, and allows packages to be distributed with prebuilt
;; auto-autoloads files.  The second is to collect all the autoloads
;; in a single global auto-autoloads file.  This is alleged to speed
;; up initialization significantly, but requires care to ensure that
;; auto-autoloads files remain synchronized with the libraries.

;; The traditional logic for determining where to put autoload
;; definitions is complex and is now deprecated.  The special variable
;; `generated-autoload-file' is used to hold the path to the file, and
;; is initialized to the traditional (well, it's a new tradition with
;; XEmacs 20) $blddir/lisp/auto-autoloads.el.  However, this variable
;; may be bound by calling code, or may be generated at collect time
;; and I'm not even sure the initialized value was ever used any more.

;; Because there may be multiple auto-autoloads files in use (in XEmacs
;; 21.x with a full complement of packages there are dozens), and they may
;; contain initializations that would be dangerous to reexecute, each is
;; protected by a feature test.  By convention, the feature symbol is of
;; the form "NAME-autoloads".  For packages, the special variable
;; `autoload-package-name' is used to determine NAME.  In the core,
;; autoloads are defined in the modules (all of which are collected in a
;; single auto-autoloads file), using NAME=modules, in the lisp directory
;; using NAME=lisp, and in the lisp/mule directory, using NAME=mule, for
;; the autoloads feature.  These latter are computed by the autoloads
;; function at collect time.

;; ChangeLog:

;; See ./ChangeLog.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard file and directory names

;; `autoload-file-name' is defvar'd and initialized in packages.el,
;; which is loaded (and dumped) very early.  If you find it isn't, you
;; know what you're doing.

(defconst autoload-target-directory "../lisp/"
  "Standard directory containing autoload declaration file.

Use `generated-autoload-file' (q.v.) to change its installation location.")

;; Dynamic variables for communication among functions

(defvar generated-autoload-file
  (expand-file-name autoload-file-name lisp-directory)
  "*File `update-file-autoloads' puts autoloads into.
A .el file can set this in its local variables section to make its
autoloads go somewhere else.

Note that `batch-update-directory' binds this variable to its own value,
generally the file named by `autoload-file-name' in the directory being
updated.  XEmacs.rules setq's this variable for package autoloads.")

(define-obsolete-variable-alias 'autoload-package-name
  'autoload-feature-prefix)
(defvar autoload-feature-prefix nil
  "If non-nil, use this string to prefix the autoload feature name.

Usually a package name (from AUTOLOAD_PACKAGE_NAME, defined in XEmacs.rules
in the top of the package hierarchy), or \"auto\" (reserved for the core Lisp
auto-autoloads file).  Highest priority candidate except for an explicit
argument to `autoload-make-feature-name' (q.v.).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magic strings in source files

(defconst generate-autoload-cookie ";;;###autoload"
  "Magic comment indicating the following form should be autoloaded.
Used by `update-file-autoloads'.  This string should be
meaningless to Lisp (e.g., a comment).

This string is used:

;;;###autoload
\(defun function-to-be-autoloaded () ...)

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If it is followed by the string
\"immediate\", then the form on the following line will be copied
verbatim.  If there is further text on the line, that text will be
copied verbatim to `generated-autoload-file'.")

(defconst generate-c-autoload-cookie "/* ###autoload"
  "Magic C comment indicating the following form should be autoloaded.
Used by `update-file-autoloads'.  This string should be
meaningless to C (e.g., a comment).

This string is used:

/* ###autoload */
DEFUN (\"function-to-be-autoloaded\", ... )

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If there is further text on the line,
that text will be copied verbatim to `generated-autoload-file'.")

(defconst generate-c-autoload-module "/* ###module"
  "Magic C comment indicating the module containing autoloaded functions.
Since a module can consist of multiple C files, the module name may not be
the same as the C source file base name.  In that case, use this comment to
indicate the actual name of the module from which to autoload functions.")

(defconst generate-autoload-section-header "\f\n;;;### "
  "String inserted before the form identifying
the section of autoloads for a file.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing the source file text.
;; Autoloads in C source differ from those in Lisp source.  For historical
;; reasons, functions handling only Lisp don't have "lisp" in their names;
;; maybe this should be changed.

(defun make-autoload (form file)
  "Turn a definition generator FORM into an autoload for source file FILE.
Returns nil if FORM is not a defun, defun*, defmacro, defmacro*,
define-skeleton, or define-derived-mode."
  (let ((car (car-safe form)))
    (if (memq car '(defun defun* define-skeleton defmacro defmacro*
		     define-derived-mode))
	(let ((macrop (memq car '(defmacro defmacro*)))
	      name doc)
	  (setq form (cdr form)
		name (car form)
		;; Ignore the arguments.
		form (cdr (cond ((eq car 'define-skeleton)
				 form)
				((eq car 'define-derived-mode)
				 (cddr form))
				(t
				 (cdr form))))
		doc (car form))
	  (if (stringp doc)
	      (setq form (cdr form))
	    (setq doc nil))
	  (list 'autoload (list 'quote name) file doc
		(or (eq car 'define-skeleton)
		    (eq car 'define-derived-mode)
		    (eq (car-safe (car form)) 'interactive))
		(if macrop (list 'quote 'macro) nil)))
      nil)))

(defun make-c-autoload (module)
  "Make an autoload list for the DEFUN at point in MODULE.
Returns nil if the DEFUN is malformed."
  (and
   ;; Match the DEFUN
   (search-forward "DEFUN" nil t)
   ;; Match the opening parenthesis
   (progn
     (skip-syntax-forward " ")
     (eq (char-after) ?\())
   ;; Match the opening quote of the Lisp function name
   (progn
     (forward-char)
     (skip-syntax-forward " ")
     (eq (char-after) ?\"))
   ;; Extract the Lisp function name, interactive indicator, and docstring
   (let* ((func-name (let ((begin (progn (forward-char) (point))))
		       (search-forward "\"" nil t)
		       (backward-char)
		       (intern (buffer-substring begin (point)))))
	  (interact (progn
		      (search-forward "," nil t 4)
		      (skip-syntax-forward " ")
		      (not (eq (char-after) ?0))))
	  (begin (progn
		   (search-forward "/*" nil t)
		   (forward-line 1)
		   (point))))
     (search-forward "*/" nil t)
     (goto-char (match-beginning 0))
     (skip-chars-backward " \t\n\f")
     (list 'autoload (list 'quote func-name) module
	   (buffer-substring begin (point)) interact nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating autoloads for a single file

;;;###autoload
(defun generate-file-autoloads (file &optional funlist)
  "Insert at point an autoload section for FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (interactive "fGenerate autoloads for file: ")
  (cond ((string-match "\\.el$" file)
	 (generate-autoload-ish-1
	  file
	  (replace-in-string (file-name-nondirectory file) "\\.elc?$" "")
	  nil #'generate-file-autoloads-1
	  funlist))
	;; #### jj, are C++ modules possible?
	((string-match "\\.c$" file)
	 (generate-autoload-ish-1
	  file
	  (replace-in-string (file-name-nondirectory file) "\\.c$" "")
	  t #'generate-c-file-autoloads-1
	  funlist))
	(t
	 (error 'wrong-type-argument file "not a C or Elisp source file"))))

(defun* generate-autoload-ish-1 (file load-name literal fun-to-call &rest args)
  "Insert at point an autoload-type section for FILE.
If LITERAL, open the file literally, without decoding.
Calls FUN-TO-CALL to compute the autoloads, passing it OUTBUF, LOAD-NAME,
  TRIM-NAME, and ARGS."
  (let ((outbuf (current-buffer))
	(trim-name (autoload-trim-file-name file))
	(autoloads-done '())
	(print-length nil)
	(print-readably t) ; XEmacs
	(float-output-format nil)
	(visited (get-file-buffer file))
	;; (done-any nil)
	output-end)

    ;; If the autoload section we create here uses an absolute
    ;; pathname for FILE in its header, and then Emacs is installed
    ;; under a different path on another system,
    ;; `update-autoloads-here' won't be able to find the files to be
    ;; autoloaded.  So, if FILE is in the same directory or a
    ;; subdirectory of the current buffer's directory, we'll make it
    ;; relative to the current buffer's directory.
    (setq file (expand-file-name file))

    (save-excursion
      (unwind-protect
	  (progn
	    (let ((find-file-hooks nil)
		  (enable-local-variables nil))
	      (set-buffer (or visited (find-file-noselect file literal literal
							  )))
	      ;; This doesn't look right for C files, but it is.  The only
	      ;; place we need the syntax table is when snarfing the Lisp
	      ;; function name.
	      (set-syntax-table emacs-lisp-mode-syntax-table))
	    (unless (setq autoloads-done
			  (apply fun-to-call outbuf load-name trim-name args))
	      (return-from generate-autoload-ish-1))
	    )
	(unless visited
	  ;; We created this buffer, so we should kill it.
	  (kill-buffer (current-buffer)))
	(set-buffer outbuf)
	(setq output-end (point-marker))))
    (if t ;; done-any
	;; XEmacs -- always do this so that we cache the information
	;; that we've processed the file already.
	(progn
	  (insert generate-autoload-section-header)
	  (prin1 (list 'autoloads autoloads-done load-name trim-name)
		 outbuf)
	  (terpri outbuf)
	  ;;;; (insert ";;; Generated autoloads from "
	  ;;;;	  (autoload-trim-file-name file) "\n")
	  ;; Warn if we put a line in auto-autoloads.el
	  ;; that is long enough to cause trouble.
	  (when (< output-end (point))
	    (setq output-end (point-marker)))
	  (while (< (point) output-end)
	    ;; (let ((beg (point)))
	      (end-of-line)
	      ;; Emacs -- I still haven't figured this one out.
	      ;; (if (> (- (point) beg) 900)
		  ;; (progn
		    ;; (message "A line is too long--over 900 characters")
		    ;; (sleep-for 2)
		    ;; (goto-char output-end)))
	      ;; )
	    (forward-line 1))
	  (goto-char output-end)
	  (insert generate-autoload-section-trailer)))
    (or noninteractive ; XEmacs: only need one line in -batch mode.
	(message "Generating autoloads for %s...done" file))))

(defun* generate-file-autoloads-1 (outbuf load-name trim-name funlist)
  "Insert at point an autoload section for FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (let ((autoloads-done '())
	(dofiles (not (null funlist)))
	)

    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(unless (search-forward generate-autoload-cookie nil t)
	  (message "No autoloads found in %s" trim-name)
	  (return-from generate-file-autoloads-1 nil))

	(message "Generating autoloads for %s..." trim-name)
	(goto-char (point-min))
	(while (if dofiles funlist (not (eobp)))
	  (if (not dofiles)
	      (skip-chars-forward " \t\n\f")
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "(def\\(un\\|var\\|const\\|macro\\) "
		     (regexp-quote (symbol-name (car funlist)))
		     "\\s "))
	    (goto-char (match-beginning 0)))
	  (cond
	   ((or dofiles
		(looking-at (regexp-quote generate-autoload-cookie)))
	    (if dofiles
		nil
	      (search-forward generate-autoload-cookie)
	      (skip-chars-forward " \t"))
	    ;; (setq done-any t)
	    (if (or dofiles (eolp))
		;; Read the next form and make an autoload.
		(let* ((form (prog1 (read (current-buffer))
			       (or (bolp) (forward-line 1))))
		       (autoload (make-autoload form load-name))
		       (doc-string-elt (get (car-safe form)
					    'doc-string-elt)))
		  (if autoload
		      (setq autoloads-done (cons (nth 1 form)
						 autoloads-done))
		    (setq autoload form))
		  (print-autoload autoload doc-string-elt outbuf ""))
	      ;; Copy the rest of the line to the output.
	      (let ((begin (point)))
		;; (terpri outbuf)
		(cond ((looking-at "immediate\\s *$") ; XEmacs
		       ;; This is here so that you can automatically
		       ;; have small hook functions copied to
		       ;; auto-autoloads.el so that it's not necessary
		       ;; to load a whole file just to get a two-line
		       ;; do-nothing find-file-hook... --Stig
		       (forward-line 1)
		       (setq begin (point))
		       (forward-sexp)
		       (forward-line 1))
		      (t
		       (forward-line 1)))
		(princ (buffer-substring begin (point)) outbuf))))
	   ((looking-at ";")
	    ;; Don't read the comment.
	    (forward-line 1))
	   (t
	    (forward-sexp 1)
	    (forward-line 1)))
	  (if dofiles
	      (setq funlist (cdr funlist))))))
    autoloads-done))

(defun* generate-c-file-autoloads-1 (outbuf load-name trim-name funlist)
  "Insert at point an autoload section for the C file FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-c-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (let ((exists-p-format 
	 "(file-exists-p (expand-file-name \"%s.%s\" module-directory))")
	(autoloads-done '())
	)

    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	;; Is there a module name comment?
	(when (search-forward generate-c-autoload-module nil t)
	  (skip-chars-forward " \t")
	  (let ((begin (point)))
	    (skip-chars-forward "^ \t\n\f")
	    (setq load-name (buffer-substring begin (point)))))
	(if funlist
	    (progn
	      (message "Generating autoloads for %s..." trim-name)
	      (princ "(when (or\n       " outbuf)
	      (princ (format exists-p-format load-name "ell") outbuf)
	      (princ "\n       " outbuf)
	      (princ (format exists-p-format load-name "dll") outbuf)
	      (princ "\n       " outbuf)
	      (princ (format exists-p-format load-name "so") outbuf)
	      ;; close the princ'd `or' form
	      (princ ")\n       " outbuf)
	      (dolist (arg funlist)
		(goto-char (point-min))
		(re-search-forward
		 (concat "DEFUN (\""
			 (regexp-quote (symbol-name arg))
			 "\""))
		(goto-char (match-beginning 0))
		(let ((autoload (make-c-autoload load-name)))
		  (when autoload
		    (push (nth 1 (nth 1 autoload)) autoloads-done)
		    (print-autoload autoload 3 outbuf "  "))))
	      ;; close the princ'd `when' form
	      (princ ")" outbuf))
	  (goto-char (point-min))
	  (let ((match
		 (search-forward generate-c-autoload-cookie nil t)))
	    (unless match
	      (message "No autoloads found in %s" trim-name)
	      (return-from generate-c-file-autoloads-1 nil))
	    
	    (message "Generating autoloads for %s..." trim-name)
	    (princ "(when (or\n       " outbuf)
	    (princ (format exists-p-format load-name "ell") outbuf)
	    (princ "\n       " outbuf)
	    (princ (format exists-p-format load-name "dll") outbuf)
	    (princ "\n       " outbuf)
	    (princ (format exists-p-format load-name "so") outbuf)
	    ;; close the princ'd `or' form
	    (princ ")\n       " outbuf)
	    (while match
	      (forward-line 1)
	      (let ((autoload (make-c-autoload load-name)))
		(when autoload
		  (push (nth 1 (nth 1 autoload)) autoloads-done)
		  (print-autoload autoload 3 outbuf "  ")))
	      (setq match
		    (search-forward generate-c-autoload-cookie nil t)))
	    ;; close the princ'd `when' form
	    (princ ")" outbuf)))))
    autoloads-done))

;; Assorted utilities for generating  autoloads and pieces thereof

(defun print-autoload (autoload doc-string-elt outbuf margin)
  "Print an autoload form, handling special characters.
In particular, print docstrings with escapes inserted before left parentheses
at the beginning of lines and ^L characters."
  (if (and doc-string-elt (stringp (nth doc-string-elt autoload)))
      ;; We need to hack the printing because the doc-string must be
      ;; printed specially for make-docfile (sigh).
      (let* ((p (nthcdr (1- doc-string-elt) autoload))
	     (elt (cdr p))
	     (start-string (format "\n%s(" margin)))
	(setcdr p nil)
	(princ start-string outbuf)
	;; XEmacs change: don't let ^^L's get into
	;; the file or sorting is hard.
	(let ((print-escape-newlines t)
	      (p (save-excursion
		   (set-buffer outbuf)
		   (point)))
	      p2)
	  (mapcar #'(lambda (elt)
		      (prin1 elt outbuf)
		      (princ " " outbuf))
		  autoload)
	  (save-excursion
	    (set-buffer outbuf)
	    (setq p2 (point-marker))
	    (goto-char p)
	    (save-match-data
	      (while (search-forward "\^L" p2 t)
		(delete-char -1)
		(insert "\\^L")))
	    (goto-char p2)))
	(princ "\"\\\n" outbuf)
	(let ((begin (save-excursion
		       (set-buffer outbuf)
		       (point))))
	  (princ (substring (prin1-to-string (car elt)) 1) outbuf)
	  ;; Insert a backslash before each ( that appears at the beginning
	  ;; of a line in the doc string.
	  (save-excursion
	    (set-buffer outbuf)
	    (save-excursion
	      (while (search-backward start-string begin t)
		(forward-char 1)
		(insert "\\"))))
	  (if (null (cdr elt))
	      (princ ")" outbuf)
	    (princ " " outbuf)
	    (princ (substring (prin1-to-string (cdr elt)) 1) outbuf))
	  (terpri outbuf)
	  (princ margin outbuf)))
    ;; XEmacs change: another ^L hack
    (let ((p (save-excursion
	       (set-buffer outbuf)
	       (point)))
	  (print-escape-newlines t)
	  p2)
      (print autoload outbuf)
      (save-excursion
	(set-buffer outbuf)
	(setq p2 (point-marker))
	(goto-char p)
	(save-match-data
	  (while (search-forward "\^L" p2 t)
	    (delete-char -1)
	    (insert "\\^L")))
	(goto-char p2)))))

;;; Forms which have doc-strings which should be printed specially.
;;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;;; the doc-string in FORM.
;;;
;;; defvar and defconst should be also be marked in this way.  There is
;;; no interference from make-docfile, which only processes those files
;;; that are loaded into the dumped Emacs, and those files should
;;; never have anything autoloaded here.  Problems only occur with files
;;; which have autoloaded entries *and* are processed by make-docfile;
;;; there should be no such files.

(put 'autoload 'doc-string-elt 3)
(put 'defun    'doc-string-elt 3)
(put 'defun*   'doc-string-elt 3)
(put 'defvar   'doc-string-elt 3)
(put 'defconst 'doc-string-elt 3)
(put 'defmacro 'doc-string-elt 3)
(put 'defmacro* 'doc-string-elt 3)
(put 'define-skeleton 'doc-string-elt 3)
(put 'define-derived-mode 'doc-string-elt 4)

(defun autoload-trim-file-name (file)
  "Returns relative pathname of FILE including the last directory.

Hard-codes the directory separator as a forward slash."
  (setq file (expand-file-name file))
  (replace-in-string
   (file-relative-name file (file-name-directory
			     (directory-file-name
			      (file-name-directory file))))
   ;; #### is this a good idea?
   "\\\\" "/"))

;;;###autoload
(defun update-file-autoloads (file)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
This function is a no-op for an autoloads file (ie, a file whose name is
equal to `autoload-file-name')."
  (interactive "fUpdate autoloads for file: ")
  (setq file (expand-file-name file))
  (when (and (file-newer-than-file-p file generated-autoload-file)
	     (not (member (file-name-nondirectory file)
			  (list autoload-file-name))))

    (let ((load-name (replace-in-string (file-name-nondirectory file)
					"\\.\\(elc?\\|c\\)$"
					""))
	  (trim-name (autoload-trim-file-name file))
	  section-begin form)
      (save-excursion
	(let ((find-file-hooks nil))
	  (set-buffer (or (get-file-buffer generated-autoload-file)
			  (find-file-noselect generated-autoload-file))))
	;; Make sure we can scribble in it.
	(setq buffer-read-only nil)
	;; First delete all sections for this file.
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (setq section-begin (match-beginning 0))
	  (setq form (read (current-buffer)))
	  (when (string= (nth 2 form) load-name)
	    (search-forward generate-autoload-section-trailer)
	    (delete-region section-begin (point))))

	;; Now find insertion point for new section
	(block find-insertion-point
	  (goto-char (point-min))
	  (while (search-forward generate-autoload-section-header nil t)
	    (setq form (read (current-buffer)))
	    (when (string< trim-name (nth 3 form))
	      ;; Found alphabetically correct insertion point
	      (goto-char (match-beginning 0))
	      (return-from find-insertion-point))
	    (search-forward generate-autoload-section-trailer))
	  (when (eq (point) (point-min))	; No existing entries?
	    (goto-char (point-max))))	; Append.

	;; Add in new sections for file
	(generate-file-autoloads file))

      (when (interactive-p) (save-buffer)))))

;;;###autoload
(defun update-autoloads-here ()
  "Update sections of the current buffer generated by `update-file-autoloads'."
  (interactive)
  (let ((generated-autoload-file (buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward generate-autoload-section-header nil t)
	(let* ((form (condition-case ()
			 (read (current-buffer))
		       (end-of-file nil)))
	       (file (nth 3 form)))
	  ;; XEmacs change: if we can't find the file as specified, look
	  ;; around a bit more.
	  (cond ((and (stringp file)
		      (or (get-file-buffer file)
			  (file-exists-p file))))
		((and (stringp file)
		      (save-match-data
			(let ((loc (locate-file (file-name-nondirectory file)
						load-path)))
			  (if (null loc)
			      nil
			    (setq loc (expand-file-name
				       (autoload-trim-file-name loc)
				       ".."))
			    (if (or (get-file-buffer loc)
				    (file-exists-p loc))
				(setq file loc)
			      nil))))))
		(t
		 (setq file
		       (if (y-or-n-p
			    (format
			     "Can't find library `%s'; remove its autoloads? "
			     (nth 2 form) file))
			   t
			 (condition-case ()
			     (read-file-name
			      (format "Find `%s' load file: "
				      (nth 2 form))
			      nil nil t)
			   (quit nil))))))
	  (if file
	      (let ((begin (match-beginning 0)))
		(search-forward generate-autoload-section-trailer)
		(delete-region begin (point))))
	  (if (stringp file)
	      (generate-file-autoloads file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for batch updates

;;;###autoload
(defun autoload-update-directory-autoloads ()
  "Update the autoloads for a directory, using a specified feature prefix.
Must be used only with -batch.  The feature prefix and directory to update
are taken from the first and second elements of `command-line-args-left',
respectively, and they are then removed from `command-line-args-left'.

Runs `update-file-autoloads' on each file in the given directory.  Always
rewrites the autoloads file, even if unchanged.  Makes a feature name by
applying `autoload-make-feature-name' to the specified feature prefix.

#### The API and semantics of this function are subject to change."
  (unless noninteractive
    (error "autoload-batch-update-autoloads: may be used only with -batch"))
  (let* ((autoload-feature-prefix (car command-line-args-left))
	 (dir (cadr command-line-args-left))
	 (generated-autoload-file (expand-file-name autoload-file-name dir)))
    (update-autoload-files (list dir) t t)
    (setq command-line-args-left (cddr command-line-args-left))))

;;;###autoload
(defun update-autoload-files (files-or-dirs &optional all-into-one-file force)
  "Update all the autoload files associated with FILES-OR-DIRS.
FILES-OR-DIRS is a list of files and/or directories to be processed.

An appropriate autoload file is chosen and a feature constructed for
each element of FILES-OR-DIRS.  Fixup code testing for the autoload file's
feature and to provide the feature is added.

If optional ALL-INTO-ONE-FILE is non-`nil', `generated-autoload-file'
should be set to the name of an autoload file and all autoloads will be
placed in that file.  `autoload-feature-prefix' should be set to an
appropriate prefix which will be concatenated with \"-autoloads\" to
produce the feature name.  Otherwise the appropriate autoload file for
each file or directory (located in that directory, or in the directory of
the specified file) will be updated with the directory's or file's
autoloads and the protective forms will be added, and the files will be
saved.  Use of the default here is unreliable, and therefore deprecated.

Note that if some of FILES-OR-DIRS are directories, recursion goes only
one level deep.

If FORCE is non-nil, always save out the autoload files even if unchanged."
  (let ((defdir (directory-file-name default-directory))
	;; value for all-into-one-file
	(autoload-feature-name (autoload-make-feature-name))
	(enable-local-eval nil))	; Don't query in batch mode.
    (dolist (arg files-or-dirs)
      (setq arg (expand-file-name arg defdir))
      (cond
       ((file-directory-p arg)
	(message "Updating autoloads for directory %s..." arg)
	(update-autoloads-from-directory arg))
       ((file-exists-p arg)
	(update-file-autoloads arg))
       (t (error "No such file or directory: %s" arg)))
      (when (not all-into-one-file)
	(autoload-featurep-protect-autoloads
	 (autoload-make-feature-name
	  (file-name-nondirectory (directory-file-name arg))))
	(if force (set-buffer-modified-p
		   t (find-file-noselect generated-autoload-file)))))
    (when all-into-one-file
      (autoload-featurep-protect-autoloads autoload-feature-name)
      (if force (set-buffer-modified-p
		 t (find-file-noselect generated-autoload-file))))
    (save-some-buffers t)
    ))

;;;###autoload
(defun update-autoloads-from-directory (dir)
  "Update `generated-autoload-file' with all the current autoloads from DIR.
This runs `update-file-autoloads' on each .el and .c file in DIR.
Obsolete autoload entries for files that no longer exist are deleted.
Note that, if this function is called from `batch-update-directory',
`generated-autoload-file' was rebound in that function.

You don't really want to be calling this function.  Try using
`update-autoload-files' instead."
  (interactive "DUpdate autoloads for directory: ")
  (setq dir (expand-file-name dir))
  (let ((simple-dir (file-name-as-directory
		     (file-name-nondirectory
		      (directory-file-name dir))))
	(enable-local-eval nil))
    (save-excursion
      (let ((find-file-hooks nil))
	(set-buffer (find-file-noselect generated-autoload-file)))
      (goto-char (point-min))
      (while (search-forward generate-autoload-section-header nil t)
	(let* ((begin (match-beginning 0))
	       (form (condition-case ()
			 (read (current-buffer))
		       (end-of-file nil)))
	       (file (nth 3 form)))
	  (when (and (stringp file)
		     (string= (file-name-directory file) simple-dir)
		     (not (file-exists-p
			   (expand-file-name
			    (file-name-nondirectory file) dir))))
	    ;; Remove the obsolete section.
	    (search-forward generate-autoload-section-trailer)
	    (delete-region begin (point)))))
      ;; Update or create autoload sections for existing files.
      (mapcar 'update-file-autoloads
	      (directory-files dir t "^[^=].*\\.\\(el\\|c\\)$"))
      (unless noninteractive
	(save-buffer)))))

(defun autoload-featurep-protect-autoloads (sym)
  (save-excursion
    (set-buffer (find-file-noselect generated-autoload-file))
    (goto-char (point-min))
    (if (and (not (= (point-min) (point-max)))
	     (not (looking-at ";;; DO NOT MODIFY THIS FILE")))
	(progn
	  (insert ";;; DO NOT MODIFY THIS FILE\n")
	  (insert "(if (featurep '" sym ")")
	  (insert " (error \"Feature " sym " already loaded\"))\n")
	  (goto-char (point-max))
	  (insert "\n(provide '" sym ")\n")))))

(defun autoload-make-feature-name (&optional prefix)
  "Generate the feature name to protect this auto-autoloads file from PREFIX.

If PREFIX is nil, it defaults to the value of `autoload-feature-prefix' if
that is non-nil.

The feature name must be globally unique for this version of XEmacs,
including packages.

For backward compatibility, if PREFIX and `autoload-feature-prefix' are both
`nil', PREFIX is computed as the last directory component of
`generated-autoload-file'.  This is likely to result in non-uniqueness, so
do not use this feature."
  (concat
   (cond (prefix)
	 (autoload-feature-prefix)
	 ((stringp generated-autoload-file)
	  (message "Warning: autoload computing feature prefix.
You should specify it as an argument to `autoload-make-feature-name'.")
	  (file-name-nondirectory
	   (directory-file-name
	    (file-name-directory generated-autoload-file))))
	 (t (error 'invalid-argument
		   "Could not compute a feature name")))
   "-autoloads"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated entry points

;; A grep of the core and packages shows use of `batch-update-autoloads'
;; by XEmacs.rules, pcomplete, eshell, oort-gnus; `batch-update-directory'
;; by liece.

;; #### these entry points below are a big mess, especially the
;; first two.  there don't seem to be very many packages that use the
;; first one (the "all-into-one-file" variety), and do they actually
;; rely on this functionality? --ben
;; but XEmacs.rules does, though maybe it doesn't "rely" on it, and
;; modules do now, and that relies on it. --sjt

;;;###autoload
(defun batch-update-autoloads ()
  "Update the autoloads for the files or directories on the command line.
Runs `update-file-autoloads' on files and `update-directory-autoloads'
on directories.  Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke `xemacs -batch -f batch-update-autoloads *.el'.
The directory to which the auto-autoloads.el file must be the first parameter
on the command line."
  (unless noninteractive
    (error "batch-update-autoloads is to be used only with -batch"))
  (update-autoload-files command-line-args-left t)
  (kill-emacs 0))

;;;###autoload
(defun batch-update-directory ()
  "Update the autoloads for the directories on the command line.
Runs `update-file-autoloads' on each file in the given directory, and must
be used only with -batch.

Uses and removes the first element of `command-line-args-left'."
  (unless noninteractive
    (error "batch-update-directory is to be used only with -batch"))
  (update-autoload-files command-line-args-left)
  ;; (kill-emacs 0)
  (setq command-line-args-left nil))

;;;###autoload
(defun batch-update-one-directory ()
  "Update the autoloads for a single directory on the command line.
Runs `update-file-autoloads' on each file in the given directory, and must
be used only with -batch."
  (unless noninteractive
    (error "batch-update-one-directory is to be used only with -batch"))
  (let ((arg (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (update-autoload-files (list arg))))

;;;###autoload
(defun batch-force-update-one-directory ()
  "Update the autoloads for a single directory on the command line.
Runs `update-file-autoloads' on each file in the given directory, and must
be used only with -batch.  Always rewrites the autoloads file, even if
unchanged.

Uses and removes the first element of `command-line-args-left'."
  (unless noninteractive
    (error "batch-force-update-directory is to be used only with -batch"))
  (let ((arg (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (update-autoload-files (list arg) nil t)))

;; Declare obsolescence

(make-obsolete-variable 'autoload-target-directory
  "Don't use this.  Bind `generated-autoload-file' to an absolute path.")
(make-obsolete 'batch-update-autoloads
	       'autoload-update-directory-autoloads)
(make-obsolete 'batch-update-directory
	       'autoload-update-directory-autoloads)
(make-obsolete 'batch-update-one-directory
	       'autoload-update-directory-autoloads)
(make-obsolete 'batch-force-update-one-directory
	       'autoload-update-directory-autoloads)

(provide 'autoload)

;;; autoload.el ends here
