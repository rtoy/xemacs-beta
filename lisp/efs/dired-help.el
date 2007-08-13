;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-help.el
;; Dired Version: $Revision: 7.9 $
;; RCS:
;; Description:   Obtaining help for dired
;; Modified:      Sun Nov 20 21:10:47 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-help)
(require 'dired)
(autoload 'reporter-submit-bug-report "reporter")
(defvar reporter-version) ; For the byte-compiler.

;;; Constants

(defconst dired-bug-address "efs-bugs@cuckoo.hpl.hp.com")

(defvar dired-documentation nil)

;;; Functions

(defun dired-documentation ()
  (or dired-documentation
      (let ((18-p (string-equal "18." (substring emacs-version 0 3)))
	    (var-help-key (substitute-command-keys
			   (if (featurep 'ehelp)
			       "\\[electric-describe-variable]"
			     "\\[describe-variable]")))
	    (standard-output (get-buffer-create " dired-help-temp")))
	(save-excursion
	  (set-buffer standard-output)
	  (unwind-protect
	      (setq dired-documentation
		    (substitute-command-keys
		     (format "\\<dired-mode-map>The Directory Editor:

For more detailed help, type \\[universal-argument] \\[dired-describe-mode] to start the info
documentation browser.
 
In dired, you can edit a list of the files in a directory \(and optionally
its subdirectories in the `ls -lR' format\).

Editing a directory means that you can visit, rename, copy, compress,
load, byte-compile  files.  You can change files' attributes, run shell
commands on files, or insert subdirectories into the edit buffer.  You can
\"flag\" files for deletion or \"mark\" files for later commands, either one
file at a time or by all files matching certain criteria \(e.g., files that
match a certain regexp\).
 
You move throughout the buffer using the usual cursor motion commands.
Letters no longer insert themselves, but execute commands instead.  The
digits (0-9) are prefix arguments.
 
Most commands operate either on all marked files or on the current file if
no files are marked.  Use a numeric prefix argument to operate on the next
ARG files (or previous ARG if ARG < 0).  Use the prefix argument `1' to
operate on the current file only.  Prefix arguments override marks. Commands
which run a sub-process on a group of files will display a list of files
for which the sub-process failed.  Typing \\[dired-why] will try to tell
you what went wrong.
 
When editing several directories in one buffer, each directory acts as a
page, so \\[backward-page] and \\[forward-page] can be used to move between directories.

Summary of commands:

Motion Commands
      move up to previous line                             \\[dired-next-line]
      move down to next line                               \\[dired-previous-line]
      move up to previous directory line                   \\[dired-prev-dirline]
      move down to next directory line                     \\[dired-next-dirline]
      move up to previous subdirectory                     \\[dired-advertised-prev-subdir]
      move down to next subdirectory                       \\[dired-advertised-next-subdir]
      move to parent directory                             \\[dired-up-directory]
      move to first child subdirectory                     \\[dired-down-directory]

Immediate Actions on Files
      visit current file                                   \\[dired-advertised-find-file]
      visit current file in other window                   \\[dired-find-file-other-window]
      visit current file in other frame               %s
      display current file                                 \\[universal-argument] \\[dired-find-file-other-window]
      create a new subdirectory                            \\[dired-create-directory]      
      recover file from auto-save                          \\[dired-recover-file]

Marking & Unmarking Files
      mark a file or subdirectory for later commands       \\[dired-mark]
      unmark a file or all files of a subdirectory         \\[dired-unmark]
      unmark all marked files in a buffer                  \\[dired-unmark-all-files]
      count marks in buffer                                0 \\[dired-unmark-all-files]
      mark all directories                                 \\[dired-mark-directories]
      mark all executable files                            \\[dired-mark-executables]
      mark file names matching a regular expression        \\[dired-mark-files-regexp]

Commands on Files Marked or Specified by the Prefix
      rename a file or move files to another directory     \\[dired-do-rename]
      copy files                                           \\[dired-do-copy]
      delete marked (as opposed to flagged) files          \\[dired-do-delete]
      compress or uncompress files                         \\[dired-do-compress]
      uuencode or uudecode files                           \\[dired-do-uucode]
      grep files                                           \\[dired-do-grep]
      search for regular expression                        \\[dired-do-tags-search]
      query replace by regular expression                  \\[dired-do-tags-query-replace]
      byte-compile files                                   \\[dired-do-byte-compile]
      load files                                           \\[dired-do-load]
      shell command on files                               \\[dired-do-shell-command]
      operate shell command separately on each file        \\[universal-argument] \\[dired-do-shell-command]
      do as above, but in each file's directory            \\[universal-argument] \\[universal-argument] \\[dired-do-shell-command]
 
Flagging Files for Deletion (unmark commands remove delete flags)
      flag file for deletion                               \\[dired-flag-file-deletion]
      backup and remove deletion flag                      \\[dired-backup-unflag]
      flag all backup files (file names ending in ~)       \\[dired-flag-backup-files]
      flag all auto-save files                             \\[dired-flag-auto-save-files]
      clean directory of numeric backups                   \\[dired-clean-directory]
      execute the deletions requested (flagged files)      \\[dired-expunge-deletions]

Modifying the Dired Buffer
      insert a subdirectory in this buffer                 \\[dired-maybe-insert-subdir]
      removing a subdir listing                            \\[dired-kill-subdir]
      relist single file, marked files, or subdir          \\[dired-do-redisplay]
      re-read all directories (retains all marks)          \\[revert-buffer]
      toggle sorting of current subdir by name/date        \\[dired-sort-toggle-or-edit]
      report on current ls switches                        0 \\[dired-sort-toggle-or-edit]
      edit ls switches for current subdir                  1 \\[dired-sort-toggle-or-edit]
      edit default ls switches for new subdirs             2 \\[dired-sort-toggle-or-edit]
      sort all subdirs by name/date                        \\[universal-argument] \\[dired-sort-toggle-or-edit]
      edit the ls switches for all subdirs                 \\[universal-argument] \\[universal-argument] \\[dired-sort-toggle-or-edit]

Hiding File Lines
      toggle file omission in current subdir               \\[dired-omit-toggle]
      kill marked file lines                               \\[dired-do-kill-file-lines]

Help on Dired
      dired help (what you're reading)                     \\[dired-describe-mode]
      dired summary (short help)                           \\[dired-summary]
      dired info (full dired info manual)                  \\[universal-argument] \\[dired-describe-mode]
      apropos for dired commands                           \\[dired-apropos]
      apropos for dired variables                          \\[universal-argument] \\[dired-apropos]

Regular Expression Commands
      mark files with a regular expression                 \\[dired-mark-files-regexp]
      copy marked files by regexp                          \\[dired-do-copy-regexp]
      rename marked files by regexp                        \\[dired-do-rename-regexp]
      omit files by regexp                                 \\[dired-omit-expunge]
      downcase file names (rename to lowercase)            \\[dired-downcase]
      upcase files names (rename to uppercase)             \\[dired-upcase]

Comparing Files
      diff file at point with file at mark                 \\[dired-diff]
      diff file with its backup                            \\[dired-backup-diff]
      merge file at point with file at mark                \\[dired-emerge]
      same as above but use a common ancestor              \\[dired-emerge-with-ancestor]
      ediff file at point with file at mark                \\[dired-ediff]
      patch file at point                                  \\[dired-epatch]

Mouse Commands
%s

Miscellaneous
      quit dired                                           \\[dired-quit]
      insert current directory in minibuffer               \\[dired-get-target-directory] 

If the dired buffer gets confused, you can either type \\[revert-buffer] to read all
directories again, type \\[dired-do-redisplay] to relist a single file, the marked
files, or a subdirectory, or type \\[dired-build-subdir-alist] to parse 
the directory tree in the buffer again.
 
Customization Variables:
Use %s to obtain more information.

%s

Hook Variables:
Use %s to obtain more information.

%s

Keybindings:
\\{dired-mode-map}"

			     ;; arguments to format
			     (if 18-p
				 "Unavailable in Emacs 18"
			       "     \\[dired-find-file-other-frame]")
			     (if 18-p
				 "      Unavailable in Emacs 18"
			       "\
      find file with mouse                                 \\[dired-mouse-find-file]
      mark file at mouse                                   \\[dired-mouse-mark]
      flag for deletion file at mouse                      \\[dired-mouse-flag]
      menu of commands to visit a file                     \\[dired-visit-popup-menu]
      menu of operations to do on a file                   \\[dired-do-popup-menu]
      insert directory of mouse in minibuffer              \\[dired-mouse-get-target]
")
			     var-help-key
			     (progn
			       (erase-buffer)
			       (dired-format-columns-of-files
				(sort
				 (all-completions
				  "dired-" obarray
				  (function
				   (lambda (sym)
				     (and (user-variable-p sym)
					  (not (dired-hook-variable-p
						sym))))))
				 'string<) t)
			       (buffer-string))
			     var-help-key
			     (progn
			       (erase-buffer)
			       (dired-format-columns-of-files
				(sort
				 (all-completions
				  "dired-" obarray
				  (function
				   (lambda (sym)
				     (dired-hook-variable-p sym))))
				 'string<) t)
			       (buffer-string)))))
	    (kill-buffer " dired-help-temp"))))))

;;; Commands

(defun dired-describe-mode (&optional info)
  "Detailed description of dired mode.
With a prefix, runs the info documentation browser for dired."
  (interactive "P")
  ;; Getting dired documentation can be a bit slow.
  (if info
      (info "dired")
    (message "Building dired help...")
    (let* ((buff (get-buffer-create "*Help*"))
	   (standard-output buff)
	   (mess (dired-documentation)))
      (message "Building dired help... done")
      (if (featurep 'ehelp)
	  (with-electric-help
	   (function
	    (lambda ()
	      (princ mess)
	      nil))) ; return nil so ehelp puts us at the top of the buffer.
	(with-output-to-temp-buffer (buffer-name buff)
	  (princ mess)
	  (print-help-return-message))))))

(defun dired-apropos (string &optional var-p)
  "Does command apropos for dired commands.
With prefix does apropos for dired variables."
  (interactive
   (list
    (if current-prefix-arg
	(read-string "Dired variable apropos (regexp): ")
      (read-string "Dired command apropos (regexp): "))
    current-prefix-arg))
  (message "Doing dired %s apropos..." (if var-p "variable" "command"))
  (if (featurep 'ehelp)
      (with-electric-help
       (function
	(lambda ()
	  (dired-apropos-internal string var-p)
	  nil)))
    (with-output-to-temp-buffer "*Help*"
      (dired-apropos-internal string var-p)
      (or (print-help-return-message)
	  (message "Doing dired %s apropos...done"
		   (if var-p "variable" "command"))))))

(defun dired-apropos-internal (string &optional var-p)
  (let ((case-fold-search t)
	(names (sort (all-completions "dired-" obarray
				      (if var-p
					  'user-variable-p
					'commandp))
		     'string<))
	doc)
    (mapcar
     (function
      (lambda (x)
	(and (if var-p (user-variable-p (intern x)) (commandp (intern x)))
	     (progn
	       (setq doc (if var-p
			     (get (intern x) 'variable-documentation)
			   (documentation (intern x))))
	       (and doc (setq doc (substring doc 0 (string-match "\n" doc))))
	       (or (string-match string x)
		   (and doc (string-match string doc))))
	     (progn
	       (princ x)
	       (if var-p (princ " <V>:")
		 (princ " <F>:")
		 (princ (make-string (max 2 (- 30 (length x))) ?\ ))
		 (princ (dired-help-key-description (intern x))))
	       (princ "\n ")
	       (princ doc)
	       (princ "\n")))))
     names)))

(defun dired-help-key-description (fun)
  ;; Returns a help string of keys for fun.
  (let ((res (mapconcat 'key-description
			(where-is-internal fun dired-mode-map) ", ")))
    (if (string-equal res "")
	"\(not on any keys\)"
      res)))

(defun dired-summary ()
  "Display summary of basic dired commands in the minibuffer."
  (interactive)
  (let ((del (where-is-internal 'dired-flag-file-deletion dired-mode-map))
	(und (where-is-internal 'dired-unmark dired-mode-map))
	(exp (where-is-internal 'dired-expunge-deletions dired-mode-map))
	(fin (where-is-internal 'dired-advertised-find-file dired-mode-map))
	(oth (where-is-internal 'dired-find-file-other-window dired-mode-map))
	(ren (where-is-internal 'dired-do-rename dired-mode-map))
	(cop (where-is-internal 'dired-do-copy dired-mode-map))
	(hel (where-is-internal 'dired-describe-mode dired-mode-map)))
    (if (member "d" del)
	(setq del "d-elete")
      (setq del (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-flag-file-deletion] delete")))
    (if (member "u" und)
	(setq und "u-ndelete")
      (setq und (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-unmark] undelete")))
    (if (member "x" exp)
	(setq exp "x-punge")
      (setq exp (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-expunge-deletions] expunge")))
    (if (member "f" fin)
	(setq fin "f-ind")
      (setq fin (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-advertised-find-file] find")))
    (if (member "o" oth)
	(setq oth "o-ther window")
      (setq oth
	    (substitute-command-keys
	     "\\<dired-mode-map>\\[dired-find-file-other-window] other window")
	    ))
    (if (member "R" ren)
	(setq ren "R-ename")
      (setq ren (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-do-rename] rename")))
    (if (member "C" cop)
	(setq cop "C-opy")
      (setq cop (substitute-command-keys
		 "\\<dired-mode-map>\\[dired-do-copy] copy")))
    (if (member "h" hel)
	(setq hel "h-elp")
      (setq hel (substitute-command-keys
		 "\\<dired-mode-map>\\[describe-mode] help")))
    (message "%s, %s, %s, %s. %s, %s, %s, %s"
	     del und exp fin oth ren cop hel)))

(defun dired-hook-variable-p (sym)
  ;; Returns t if SYM is a hook variable.  Just looks at its name.
  (let ((name (symbol-name sym)))
    (and (>= (length name) 6)
	 (or (string-equal (substring name -5) "-hook")
	     (string-equal (substring name -6) "-hooks")))))

;;; Submitting bug reports.

(defun dired-report-bug ()
  "Submit a bug report for dired."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (or (boundp 'reporter-version)
	(setq reporter-version
	      "Your version of reporter is obsolete.  Please upgrade."))
    (reporter-submit-bug-report
     dired-bug-address "Dired"
     (cons
      'dired-version
      (nconc
       (mapcar
	'intern
	(sort
	 (let (completion-ignore-case)
	   (all-completions "dired-" obarray 'user-variable-p))
	 'string-lessp))
       (list 'reporter-version)))
     (function
      (lambda ()
	(save-excursion
	  (mail-position-on-field "subject")
	  (beginning-of-line)
	  (skip-chars-forward "^:\n")
	  (if (looking-at ": Dired;")
	      (progn
		(goto-char (match-end 0))
		(delete-char -1)
		(insert " " dired-version " bug:")))))))))

;;; end of dired-help.el
