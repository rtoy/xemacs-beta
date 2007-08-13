 ; -*- Emacs-Lisp -*-
;; DIRED commands for Emacs.
;; Copyright (C) 1985, 1986, 1991 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired.el
;; RCS:           
;; Dired Version: #Revision: 7.9 $
;; Description:   The DIRectory EDitor is for manipulating, and running
;;                commands on files in a directory.
;; Authors:       FSF,
;;                Sebastian Kremer <sk@thp.uni-koeln.de>,
;;                Sandy Rutherford <sandy@ibm550.sissa.it>
;;                Cast of thousands...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; 7-1993: Added special features for efs interaction and upgraded to Emacs 19.
;;         Sandy Rutherford <sandy@ibm550.sissa.it>

;;; Dired Version

(defconst dired-version (substring "#Revision: 7.9 $" 11 -2)
  "The revision number of Tree Dired (as a string).

Don't forget to mention this when reporting bugs to:
   
   efs-bugs@cuckoo.hpl.hp.com")

;; Global key bindings:
;; --------------------
;;
;; By convention, dired uses the following global key-bindings.
;; These may or may not already be set up in your local emacs. If not
;; then you will need to add them to your .emacs file, or the system
;; default.el file. We don't set them automatically here, as users may
;; have individual preferences.
;;
;; (define-key ctl-x-map "d" 'dired)
;; (define-key ctl-x-4-map "d" 'dired-other-window)
;; (define-key ctl-x-map "\C-j" 'dired-jump-back)
;; (define-key ctl-x-4-map "\C-j" 'dired-jump-back-other-window)
;; 
;; For V19 emacs only. (Make sure that the ctl-x-5-map exists.)
;; (define-key ctl-x-5-map "d" 'dired-other-frame)
;; (define-key Ctl-x-5-map "\C-j" 'dired-jump-back-other-frame)


;;; Grok the current emacs version
;;
;; Hopefully these two variables provide us with enough version sensitivity.

;; Make sure that we have a frame-width function
(or (fboundp 'frame-width) (fset 'frame-width 'screen-width))

;;; Requirements and provisions

(provide 'dired)
(require 'backquote) ; For macros.

;; Compatibility requirements for the file-name-handler-alist.
;; Testing against the string `Lucid' breaks InfoDock.  How many years has
;; it been since Lucid went away?
(let ((lucid-p (string-match "XEmacs" emacs-version))
      ver subver)
  (or (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
      (error "dired does not work with emacs version %s" emacs-version))
  (setq ver (string-to-int (substring emacs-version (match-beginning 1)
				      (match-end 1)))
	subver (string-to-int (substring emacs-version (match-beginning 2)
					 (match-end 2))))
  (cond
   ((= ver 18)
    (require 'emacs-19)
    (require 'fn-handler))
   ((and (= ver 19) (if lucid-p (< subver 10) (< subver 23)))
    (require 'fn-handler))
   ((< ver 18)
    (error "dired does not work with emacs version %s" emacs-version))))

;; We duplicate default-dir stuff to avoid its overwrites unless
;; they are explicitly requested.

(defvar default-directory-function nil
  "A function to call to compute the default-directory for the current buffer.
If this is nil, the function default-directory will return the value of the
variable default-directory.
Buffer local.")
(make-variable-buffer-local 'default-directory-function)

;;;###autoload
(defun default-directory ()
  " Returns the default-directory for the current buffer.
Will use the variable default-directory-function if it non-nil."
  (if default-directory-function
      (funcall default-directory-function)
    (if (string-match "XEmacs" emacs-version)
	(abbreviate-file-name default-directory t)
      (abbreviate-file-name default-directory))))

;;;;----------------------------------------------------------------
;;;; Customizable variables
;;;;----------------------------------------------------------------
;;
;; The funny comments are for autoload.el, to automagically update
;; loaddefs.

;;; Variables for compressing files.

;;;###autoload
(defvar dired-compression-method 'compress
  "*Type of compression program to use.
Give as a symbol.
Currently-recognized methods are: gzip pack compact compress.
To change this variable use \\[dired-do-compress] with a zero prefix.")

;;;###autoload
(defvar dired-compression-method-alist
  '((gzip     ".gz" ("gzip")          ("gzip" "-d") "-f")
    ;; Put compress before pack, so that it wins out if we are using
    ;; efs to work on a case insensitive OS. The -f flag does
    ;; two things in compress. No harm in giving it twice.
    (compress ".Z"  ("compress" "-f") ("compress" "-d") "-f")
    ;; pack support may not work well.  pack is too chatty and there is no way
    ;; to force overwrites.
    (pack     ".z"  ("pack" "-f")     ("unpack"))
    (compact  ".C"  ("compact")       ("uncompact")))
  
  "*Association list of compression method descriptions.
 Each element of the table should be a list of the form
 
     \(compress-type extension (compress-args) (decompress-args) force-flag\)
 
 where 
   `compress-type' is a unique symbol in the alist to which
      `dired-compression-method' can be set;
   `extension' is the file extension (as a string) used by files compressed
      by this method;
   `compress-args' is a list of the path of the compression program and
      flags to pass as separate arguments;
   `decompress-args' is a list of the path of the decompression
      program and flags to pass as separate arguments.
   `force-flag' is the switch to pass to the command to force overwriting
      of existing files.
 
 For example:
 
   \(setq dired-compression-method-alist
         \(cons '\(frobnicate \".frob\" \(\"frob\"\) \(\"frob\" \"-d\"\) \"-f\"\)
               dired-compression-method-alist\)\)
   => \(\(frobnicate \".frob\" \(\"frob\"\) \(\"frob\" \"-d\"\)\) 
       \(gzip \".gz\" \(\"gzip\"\) \(\"gunzip\"\)\)
       ...\)
 
 See also: dired-compression-method <V>")

;;; Variables for the ls program.

;;;###autoload
(defvar dired-ls-program "ls"
  "*Absolute or relative name of the ls program used by dired.")

;;;###autoload
(defvar dired-listing-switches "-al"
  "*Switches passed to ls for dired. MUST contain the `l' option.
Can contain even `F', `b', `i' and `s'.")

(defvar dired-ls-F-marks-symlinks
  (memq system-type '(aix-v3 hpux silicon-graphics-unix))
  ;; Both SunOS and Ultrix have system-type berkeley-unix. But
  ;; SunOS doesn't mark symlinks, but Ultrix does. Therefore,
  ;; can't grok this case.
  "*Informs dired about how ls -lF marks symbolic links.
Set this to t if `dired-ls-program' with -lF marks the name of the symbolic
link itself with a trailing @.

For example: If foo is a link pointing to bar, and \"ls -F bar\" gives 

     ...   bar -> foo

set this variable to nil. If it gives
    
      ...   bar@ -> foo

set this variable to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking ls program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t.

If you use efs, it will make this variable buffer-local, and control
it according to its assessment of how the remote host marks symbolic
links.")

(defvar dired-show-ls-switches nil
  "*If non-nil dired will show the dired ls switches on the modeline.
If nil, it will indicate how the files are sorted by either \"by name\" or
\"by date\". If it is unable to recognize the sorting defined by the switches,
then the switches will be shown explicitly on the modeline, regardless of the
setting of this variable.")

;;; Variables for other unix utility programs.

;; For most program names, don't use absolute paths so that dired
;; uses the user's value of the environment variable PATH. chown is
;; an exception as it is not always in the PATH.

;;;###autoload
(defvar dired-chown-program
  (if (memq system-type '(hpux dgux usg-unix-v linux)) "chown" "/etc/chown")
  "*Name of chown command (usually `chown' or `/etc/chown').")

;;;###autoload
(defvar dired-gnutar-program nil
  "*If non-nil, name of the GNU tar executable (e.g. \"tar\" or \"gnutar\").
GNU tar's `z' switch is used for compressed tar files.
If you don't have GNU tar, set this to nil: a pipe using `zcat' is then used.")

;;;###autoload
(defvar dired-unshar-program nil
  "*Set to the name of the unshar program, if you have it.")

;;; Markers

(defvar dired-keep-marker-rename t
  ;; Use t as default so that moved files `take their markers with them'
    "*Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character.")

(defvar dired-keep-marker-compress t
  "*Controls marking of compressed or uncompressed files.
If t, files keep their previous marks when they are compressed.
If a character, compressed or uncompressed files (whether previously
marked or not) are afterward marked with that character.")

(defvar dired-keep-marker-uucode ?U
  "*Controls marking of uuencoded or uudecoded files.
If t, files keep their previous marks when they are uuencoded.
If a character, uuencoded or uudecoded files (whether previously
marked or not) are afterward marked with that character.")

(defvar dired-keep-marker-copy ?C
  "*Controls marking of copied files.
If t, copied files are marked if and as the corresponding original files were.
If a character, copied files are unconditionally marked with that character.")

(defvar dired-keep-marker-hardlink ?H
  "*Controls marking of newly made hard links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

(defvar dired-keep-marker-symlink ?S
  "*Controls marking of newly made symbolic links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

(defvar dired-keep-marker-kill ?K
  "*When killed file lines are redisplayed, they will have this marker.
Setting this to nil means that they will not have any marker.")

(defvar dired-failed-marker-shell ?!
  "*If non-nil, a character with which to mark files of failed shell commands.
Applies to the command `dired-do-shell-command'.  Files for which the shell
command has a nonzero exit status will be marked with this character")

;;; Behavioral Variables

;;;###autoload
(defvar dired-local-variables-file ".dired"
  "*If non-nil, filename for local variables for Dired.
If Dired finds a file with that name in the current directory, it will
temporarily insert it into the dired buffer and run `hack-local-variables'.

Type \\[info] and `g' `(emacs)File Variables' `RET' for more info on
local variables.")

;; Usually defined in files.el. Define here anyway, to be safe.
;;;###autoload
(defvar dired-kept-versions 2
  "*When cleaning directory, number of versions to keep.")

;;;###autoload
(defvar dired-find-subdir nil
  "*Determines whether dired tries to lookup a subdir in existing buffers.
If non-nil, dired does not make a new buffer for a directory if it can be
found (perhaps as subdir) in some existing dired buffer. If there are several
dired buffers for a directory, then the most recently used one is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, C-x d RET will
toggle between those two.")

;;;###autoload
(defvar dired-use-file-transformers t
  "*Determines whether dired uses file transformers.
If non-nil `dired-do-shell-command' will apply file transformers to file names.
See \\[describe-function] for dired-do-shell-command for more information.")

;;;###autoload
(defvar dired-dwim-target nil
  "*If non-nil, dired tries to guess a default target directory.
This means that if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer.
The target is put in the prompt for file copy, rename, etc.")

;;;###autoload
(defvar dired-copy-preserve-time nil
  "*If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)\\<dired-mode-map>
Use `\\[dired-do-copy]' with a zero prefix argument to toggle its value.")

;;;###autoload
(defvar dired-no-confirm nil
  "*If non-nil, a list of symbols for commands dired should not confirm.
It can be a sublist of

  '(byte-compile chgrp chmod chown compress copy delete hardlink load
    move print shell symlink uncompress recursive-delete kill-file-buffer
    kill-dired-buffer patch create-top-dir revert-subdirs)

The meanings of most of the symbols are obvious.  A few exceptions:

    'compress applies to compression or decompression by any of the 
     compression program in `dired-compression-method-alist'.

    'kill-dired-buffer applies to offering to kill dired buffers for
     directories which have been deleted.

    'kill-file-buffer applies to offering to kill buffers visiting files
     which have been deleted.

    'recursive-delete applies to recursively deleting non-empty
     directories, and all of their contents.

    'create-top-dir applies to `dired-up-directory' creating a new top level
     directory for the dired buffer.

    'revert-subdirs applies to re-reading subdirectories which have 
     been modified on disk.

Note that this list also applies to remote files accessed with efs
or ange-ftp.")

;;;###autoload
(defvar dired-backup-if-overwrite nil
  "*Non-nil if Dired should ask about making backups before overwriting files.
Special value 'always suppresses confirmation.")

;;;###autoload
(defvar dired-omit-files nil
  "*If non-nil un-interesting files will be omitted from this dired buffer.
Use \\[dired-omit-toggle] to see these files. (buffer local)")
(make-variable-buffer-local 'dired-omit-files)

;;;###autoload
(defvar dired-mail-reader 'rmail
  "*Mail reader used by dired for dired-read-mail \(\\[dired-read-mail]\).
The symbols 'rmail and 'vm are the only two allowed values.")

(defvar dired-verify-modtimes t
  "*If non-nil dired will revert dired buffers for modified subdirectories.
See also dired-no-confirm <V>.")

;;;###autoload
(defvar dired-refresh-automatically t
  "*If non-nil, refresh dired buffers automatically after file operations.")

;;; File name regular expressions and extensions.

(defvar dired-trivial-filenames "^\\.\\.?$\\|^#"
  "*Regexp of files to skip when finding first file of a directory listing.
A value of nil means move to the subdir line.
A value of t means move to first file.")

(defvar dired-cleanup-alist
  (list
   '("tex" ".toc" ".log" ".aux" ".dvi")
   '("latex" ".toc" ".log" ".aux" ".idx" ".lof" ".lot" ".glo" ".dvi")
   '("bibtex" ".blg" ".bbl")
   '("texinfo" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs"
     ".tp" ".tps" ".vr" ".vrs")
   '("patch" ".rej" ".orig")
   '("backups" "~")
   (cons "completion-ignored-extensions" completion-ignored-extensions))
  "*Alist of extensions for temporary files created by various programs.
Used by `dired-cleanup'.")

(defvar dired-omit-extensions
  (let ((alist dired-cleanup-alist)
	x result)
    (while alist
      (setq x (cdr (car alist))
	    alist (cdr alist))
      (while x
	(or (member (car x) result)
	    (setq result (cons (car x) result)))
	(setq x (cdr x))))
    result)
  "*List of extensions for file names that will be omitted (buffer-local).
This only has effect when the subdirectory is in omission mode.
To make omission mode the default, set `dired-omit-files' to t.
See also `dired-omit-extensions'.")
(make-variable-buffer-local 'dired-omit-extensions)

(defvar dired-omit-regexps '("^#" "^\\.")
  "*File names matching these regexp may be omitted (buffer-local).
This only has effect when the subdirectory is in omission mode.
To make omission mode the default, set `dired-omit-files' to t.
This only has effect when `dired-omit-files' is t.
See also `dired-omit-extensions'.")
(make-variable-buffer-local 'dired-omit-regexps)

(defvar dired-filename-re-ext "\\..+$"   ; start from the first dot. last dot?
  "*Defines what is the extension of a file name.
\(match-beginning 0\) for this regexp in the file name without directory will
be taken to be the start of the extension.")

;;; Hook variables

(defvar dired-load-hook nil
  "Run after loading dired.
You can customize key bindings or load extensions with this.")

(defvar dired-grep-load-hook nil
  "Run after loading dired-grep.")

(defvar dired-mode-hook nil
  "Run at the very end of dired-mode.")

(defvar dired-before-readin-hook nil
  "Hook run before a dired buffer is newly read in, created,or reverted.")

(defvar dired-after-readin-hook nil
  "Hook run after each listing of a file or directory.
The buffer is narrowed to the new listing.")

(defvar dired-setup-keys-hook nil
  "Hook run when dired sets up its keymap.
This happens the first time that `dired-mode' is called, and runs after
`dired-mode-hook'.  This hook can be used to make alterations to the
dired keymap.")

;;; Internal variables
;;
;;  If you set these, know what you are doing.

;;; Marker chars.

(defvar dired-marker-char ?*            ; the answer is 42
					; life the universe and everything
  ;; so that you can write things like
  ;; (let ((dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In dired, character used to mark files for later commands.")
(make-variable-buffer-local 'dired-marker-char)

(defconst dired-default-marker dired-marker-char)
;; Stores the default value of dired-marker-char when dynamic markers
;; are being used.

(defvar dired-del-marker ?D
  "Character used to flag files for deletion.")

;; \017=^O for Omit - other packages can chose other control characters.
(defvar dired-omit-marker-char ?\017)
;; Marker used for omitted files.  Shouldn't be used by anything else.

(defvar dired-kill-marker-char ?\C-k)
;; Marker used by dired-do-kill.  Shouldn't be used by anything else.

;;; State variables

(defvar dired-mode-line-modified "-%s%s%s-"
  "*Format string to show the modification status of the buffer.")

(defvar dired-del-flags-number 0)
(make-variable-buffer-local 'dired-del-flags-number)
(defvar dired-marks-number 0)
(make-variable-buffer-local 'dired-marks-number)
(defvar dired-other-marks-number 0)
(make-variable-buffer-local 'dired-other-marks-number)

(defvar dired-marked-files nil
  "List of filenames from last `dired-copy-filename-as-kill' call.")

(defvar dired-directory nil
  "The directory name or shell wildcard that was used as argument to `ls'.
Local to each dired buffer.  May be a list, in which case the car is the
directory name and the cdr is the actual files to list.")
(make-variable-buffer-local 'dired-directory)

(defvar dired-internal-switches nil
  "The actual (buffer-local) value of `dired-listing-switches'.
The switches are represented as a list of characters.")
(make-variable-buffer-local 'dired-internal-switches)

(defvar dired-subdir-alist nil
  "Association list of subdirectories and their buffer positions.
Each subdirectory has an element: (DIRNAME . STARTMARKER).
The order of elements is the reverse of the order in the buffer.")
(make-variable-buffer-local 'dired-subdir-alist)

(defvar dired-curr-subdir-min 0)
;; Cache for modeline tracking of the cursor
(make-variable-buffer-local 'dired-curr-subdir-min)

(defvar dired-curr-subdir-max 0)
;; Cache for modeline tracking of the cursor
(make-variable-buffer-local 'dired-curr-subdir-max)

(defvar dired-subdir-omit nil)
;; Controls whether the modeline shows Omit.
(make-variable-buffer-local 'dired-subdir-omit)

(defvar dired-in-query nil)
;; let-bound to t when dired is in the process of querying the user.
;; This is to keep asynch messaging from clobbering the query prompt.

(defvar dired-overwrite-confirmed nil)
;; Fluid variable used to remember if a bunch of overwrites have been
;; confirmed.

(defvar dired-overwrite-backup-query nil)
;; Fluid var used to remember if backups have been requested for overwrites.

(defvar dired-file-creator-query nil)
;; Fluid var to remember responses to file-creator queries.

(defvar dired-omit-silent nil)
;; This is sometimes let-bound to t if messages would be annoying,
;; e.g., in dired-awrh.el. Binding to 0, only suppresses
;; \"(Nothing to omit)\" message.

(defvar dired-buffers nil
  ;; Enlarged by dired-advertise
  ;; Queried by function dired-buffers-for-dir. When this detects a
  ;; killed buffer, it is removed from this list.
  "Alist of directories and their associated dired buffers.")

(defvar dired-sort-mode nil
  "Whether Dired sorts by name, date, etc. 
\(buffer-local\)")
;; This is nil outside dired buffers so it can be used in the modeline
(make-variable-buffer-local 'dired-sort-mode)

(defvar dired-marker-stack nil
  "List of previously used dired marker characters.")
(make-variable-buffer-local 'dired-marker-stack)

(defvar dired-marker-stack-pointer 0)
;; Points to the current marker in the stack
(make-variable-buffer-local 'dired-marker-stack-pointer)

(defvar dired-marker-stack-cursor ?\  ; space
  "Character to use as a cursor in the dired marker stack.")

(defconst dired-marker-string ""
  "String version of `dired-marker-stack'.")
(make-variable-buffer-local 'dired-marker-string)

(defvar dired-modeline-tracking-cmds nil)
;; List of commands after which the modeline gets updated.

;;; Config. variables not usually considered fair game for the user.

(defvar dired-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defvar dired-log-buffer "*Dired log*")
;; Name of buffer used to log dired messages and errors.

;;; Assoc. lists

;; For pop ups and user input for file marking
(defvar dired-query-alist
  '((?\y . y) (?\040 . y)		; `y' or SPC means accept once
    (?n . n) (?\177 . n)		; `n' or DEL skips once
    (?! . yes)				; `!' accepts rest
    (?q. no) (?\e . no)			; `q' or ESC skips rest
    ;; None of these keys quit - use C-g for that.
    ))

(defvar dired-sort-type-alist
  ;; alist of sort flags, and the sort type, as a symbol.
  ;; Don't put ?r in here.  It's handled separately.
  '((?t . date) (?S . size) (?U . unsort) (?X . ext)))

;;; Internal regexps for examining ls listings.
;;
;; Many of these regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar dired-re-inode-size "[ \t0-9]*")
;; Regexp for optional initial inode and file size.
;; Must match output produced by ls' -i and -s flags.

(defvar dired-re-mark "^[^ \n\r]")
;; Regexp matching a marked line.
;; Important: the match ends just after the marker.

(defvar dired-re-maybe-mark "^. ")

(defvar dired-re-dir (concat dired-re-maybe-mark dired-re-inode-size "d"))
;; Matches directory lines

(defvar dired-re-sym (concat dired-re-maybe-mark dired-re-inode-size "l"))
;; Matches symlink lines

(defvar dired-re-exe;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		(concat dired-re-maybe-mark dired-re-inode-size x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))

(defvar dired-re-dot "^.* \\.\\.?/?$")	; with -F, might end in `/'
;; . and .. files

(defvar dired-re-month-and-time
  (concat
   " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|June?\\|July?\\|Aug\\|Sep\\|Oct\\|Nov\\|"
					; June and July are for HP-UX 9.0
   "Dec\\) [ 0-3][0-9]\\("
   " [012][0-9]:[0-6][0-9] \\|" ; time
   "  [12][90][0-9][0-9] \\|"   ; year on IRIX, NeXT, SunOS, ULTRIX, Apollo,
			        ; HP-UX, A/UX
   " [12][90][0-9][0-9]  \\)"   ; year on AIX
   ))
;; This regexp MUST match all the way to first character of the filename.
;; You can loosen it to taste, but then you might bomb on filenames starting
;; with a space. This will have to be modified for non-english month names.

(defvar dired-subdir-regexp
  "\\([\n\r]\n\\|\\`\\). \\([^\n\r]+\\)\\(:\\)\\(\\.\\.\\.\r\\|[\n\r]\\)")
  ;; Regexp matching a maybe hidden subdirectory line in ls -lR output.
  ;; Subexpression 2 is the subdirectory proper, no trailing colon.
  ;; Subexpression 3 must end right before the \n or \r at the end of
  ;; the subdir heading.  Matches headings after indentation has been done.

(defvar dired-unhandle-add-files nil)
;; List of files that the dired handler function need not add to dired buffers.
;; This is because they have already been added, most likely in
;; dired-create-files.  This is because dired-create-files add files with
;; special markers.

;;; history variables

(defvar dired-regexp-history nil
  "History list of regular expressions used in Dired commands.")

(defvar dired-chmod-history nil
  "History of arguments to chmod in dired.")

(defvar dired-chown-history nil
  "History of arguments to chown in dired.")

(defvar dired-chgrp-history nil
  "History of arguments to chgrp in dired.")

(defvar dired-cleanup-history nil
  "History of arguments to dired-cleanup.")

(defvar dired-goto-file-history nil)
;; History for dired-goto-file and dired-goto-subdir
(put 'dired-goto-file-history 'cursor-end t) ; for gmhist

(defvar dired-history nil)
;; Catch-all history variable for dired file ops without
;; their own history.

(defvar dired-op-history-alist
  ;; alist of dired file operations and history symbols
  '((chgrp . dired-chgrp-history) (chown . dired-chown-history)
    (chmod . dired-chmod-history) ))

;;; Tell the byte-compiler that we know what we're doing.
;;; Do we?

(defvar file-name-handler-alist)
(defvar inhibit-file-name-operation)
(defvar inhibit-file-name-handlers)
(defvar efs-dired-host-type)


;;;;------------------------------------------------------------------
;;;; Utilities
;;;;------------------------------------------------------------------ 

;;; Macros
;;
;;  Macros must be defined before they are used - for the byte compiler.

(defmacro dired-get-subdir-min (elt)
  ;; Returns the value of the subdir minumum for subdir with entry ELT in
  ;; dired-subdir-alist.
  (list 'nth 1 elt))

(defmacro dired-save-excursion (&rest body)
  ;; Saves excursions of the point (not buffer) in dired buffers.
  ;; It tries to be robust against deletion of the region about the point.
  ;; Note that this assumes only dired-style deletions.
  (let ((temp-bolm (make-symbol "bolm"))
	(temp-fnlp (make-symbol "fnlp"))
	(temp-offset-bol (make-symbol "offset-bol")))
    (` (let (((, temp-bolm) (make-marker))
	     (, temp-fnlp) (, temp-offset-bol))
	 (let ((bol (save-excursion (skip-chars-backward "^\n\r") (point))))
	   (set-marker (, temp-bolm) bol)
	   (setq (, temp-offset-bol) (- (point) bol)
		 (, temp-fnlp) (memq (char-after bol) '(?\n\ ?\r))))
	 (unwind-protect
	     (progn
	       (,@ body))
	   ;; Use the marker to try to find the right line, then move to
	   ;; the proper column.
	   (goto-char (, temp-bolm))
	   (and (not (, temp-fnlp))
		(memq (char-after (point)) '(?\n ?\r))
		;; The line containing the point got deleted. Note that this
		;; logic only works if we don't delete null lines, but we never
		;; do.
		(forward-line 1)) ; don't move into a hidden line.
	   (skip-chars-forward "^\n\r" (+ (point) (, temp-offset-bol))))))))

(put 'dired-save-excursion 'lisp-indent-hook 0)

(defun dired-substitute-marker (pos old new)
  ;; Change marker, re-fontify
  (subst-char-in-region pos (1+ pos) old new)
  (dired-move-to-filename))

(defmacro dired-mark-if (predicate msg)
  ;; Mark all files for which CONDITION evals to non-nil.
  ;; CONDITION is evaluated on each line, with point at beginning of line.
  ;; MSG is a noun phrase for the type of files being marked.
  ;; It should end with a noun that can be pluralized by adding `s'.
  ;; Return value is the number of files marked, or nil if none were marked.
  (let ((temp-pt (make-symbol "pt"))
	(temp-count (make-symbol "count"))
	(temp-msg (make-symbol "msg")))
    (` (let (((, temp-msg) (, msg))
	     ((, temp-count) 0)
	     (, temp-pt) buffer-read-only)
	 (save-excursion
	   (if (, temp-msg) (message "Marking %ss..." (, temp-msg)))
	   (goto-char (point-min))
	   (while (not (eobp))
	     (if (and (, predicate)
		      (not (char-equal (following-char) dired-marker-char)))
		 (progn
		   ;; Doing this rather than delete-char, insert
		   ;; avoids re-computing markers
		   (setq (, temp-pt) (point))
		   (dired-substitute-marker
		    (, temp-pt)
		    (following-char) dired-marker-char)
		   (setq (, temp-count) (1+ (, temp-count)))))
	     (forward-line 1))
	   (if (, temp-msg)
	       (message "%s %s%s %s%s."
			(, temp-count)
			(, temp-msg)
			(dired-plural-s (, temp-count))
			(if (eq dired-marker-char ?\040) "un" "")
			(if (eq dired-marker-char dired-del-marker)
			    "flagged" "marked"))))
	 (and (> (, temp-count) 0) (, temp-count))))))

(defmacro dired-map-over-marks (body arg &optional show-progress)
;;  Perform BODY with point somewhere on each marked line
;;  and return a list of BODY's results.
;;  If no marked file could be found, execute BODY on the current line.
;;  If ARG is an integer, use the next ARG (or previous -ARG, if ARG<0)
;;  files instead of the marked files.
;;  If ARG is t, only apply to marked files.  If there are no marked files,
;;  the result is a noop.
;;  If ARG is otherwise non-nil, use current file instead.
;;  If optional third arg SHOW-PROGRESS evaluates to non-nil,
;;  redisplay the dired buffer after each file is processed.
;;  No guarantee is made about the position on the marked line.
;;  BODY must ensure this itself if it depends on this.
;;  Search starts at the beginning of the buffer, thus the car of the list
;;  corresponds to the line nearest to the buffer's bottom.  This
;;  is also true for (positive and negative) integer values of ARG.
;;  To avoid code explosion, BODY should not be too long as it is
;;  expanded four times.
;;
;;  Warning: BODY must not add new lines before point - this may cause an
;;  endless loop.
;;  This warning should not apply any longer, sk  2-Sep-1991 14:10.
  (let ((temp-found (make-symbol "found"))
	(temp-results (make-symbol "results"))
	(temp-regexp (make-symbol "regexp"))
	(temp-curr-pt (make-symbol "curr-pt"))
	(temp-next-position (make-symbol "next-position")))
    (` (let (buffer-read-only case-fold-search (, temp-found) (, temp-results))
	 (dired-save-excursion
	   (if (and (, arg) (not (eq (, arg) t)))
	       (if (integerp (, arg))
		   (and (not (zerop (, arg)))
			(progn;; no save-excursion, want to move point.
			  (dired-repeat-over-lines
			   arg
			   (function (lambda ()
				       (if (, show-progress) (sit-for 0))
				       (setq (, temp-results)
					     (cons (, body)
						   (, temp-results))))))
			  (if (<  (, arg) 0)
			      (nreverse (, temp-results))
			    (, temp-results))))
		 ;; non-nil, non-integer ARG means use current file:
		 (list (, body)))
	     (let (((, temp-regexp)
		    (concat "^" (regexp-quote (char-to-string
					       dired-marker-char))))
		   (, temp-curr-pt) (, temp-next-position))
	       (save-excursion
		 (goto-char (point-min))
		 ;; remember position of next marked file before BODY
		 ;; can insert lines before the just found file,
		 ;; confusing us by finding the same marked file again
		 ;; and again and...
		 (setq (, temp-next-position)
		       (and (re-search-forward (, temp-regexp) nil t)
			    (point-marker))
		       (, temp-found) (not (null (, temp-next-position))))
		 (while (, temp-next-position)
		   (setq (, temp-curr-pt) (goto-char (, temp-next-position))
			 ;; need to get next position BEFORE body
			 (, temp-next-position)
			 (and (re-search-forward (, temp-regexp) nil t)
			      (point-marker)))
		   (goto-char (, temp-curr-pt))
		   (if (, show-progress) (sit-for 0))
		   (setq (, temp-results) (cons (, body) (, temp-results)))))
	       (if (, temp-found)
		   (, temp-results)
		 ;; Do current file, unless arg is t
		 (and (not (eq (, arg) t))
		      (list (, body)))))))))))

;;; General utility functions

(defun dired-buffer-more-recently-used-p (buffer1 buffer2)
  "Return t if BUFFER1 is more recently used than BUFFER2."
  (if (equal buffer1 buffer2)
      nil
    (let ((more-recent nil)
	  (list (buffer-list)))
      (while (and list
		  (not (setq more-recent (equal buffer1 (car list))))
		  (not (equal buffer2 (car list))))
	(setq list (cdr list)))
      more-recent)))

(defun dired-file-modtime (file)
  ;; Return the modtime of FILE, which is assumed to be already expanded
  ;; by expand-file-name.
  (let ((handler (find-file-name-handler file 'dired-file-modtime)))
    (if handler
	(funcall handler 'dired-file-modtime file)
      (nth 5 (file-attributes file)))))

(defun dired-set-file-modtime (file alist)
  ;; Set the modtime for FILE in the subdir alist ALIST.
  (let ((handler (find-file-name-handler file 'dired-set-file-modtime)))
    (if handler
	(funcall handler 'dired-set-file-modtime file alist)
      (let ((elt (assoc file alist)))
	(if elt
	    (setcar (nthcdr 4 elt) (nth 5 (file-attributes file))))))))

(defun dired-map-over-marks-check (fun arg op-symbol operation
				       &optional show-progress no-confirm)
  ;; Map FUN over marked files (with second ARG like in dired-map-over-marks)
  ;; and display failures.

  ;; FUN takes zero args.  It returns non-nil (the offending object, e.g.
  ;; the short form of the filename) for a failure and probably logs a
  ;; detailed error explanation using function `dired-log'.

  ;; OP-SYMBOL is s symbol representing the operation.
  ;; eg. 'compress

  ;; OPERATION is a string describing the operation performed (e.g.
  ;; "Compress").  It is used with `dired-mark-pop-up' to prompt the user
  ;; (e.g. with `Compress * [2 files]? ') and to display errors (e.g.
  ;; `Failed to compress 1 of 2 files - type y to see why ("foo")')

  ;; SHOW-PROGRESS if non-nil means redisplay dired after each file.

  (if (or no-confirm (dired-mark-confirm op-symbol operation arg))
      (let* ((total-list;; all of FUN's return values
	      (dired-map-over-marks (funcall fun) arg show-progress))
	     (total (length total-list))
	     (failures (delq nil total-list))
	     (count (length failures)))
	(if (not failures)
	    (message "%s: %d file%s." operation total (dired-plural-s total))
	  (message "Failed to %s %d of %d file%s - type y to see why %s"
		   operation count total (dired-plural-s total)
		   ;; this gives a short list of failed files in parens
		   ;; which may be sufficient for the user even
		   ;; without typing `W' for the process' diagnostics
		   failures)
	  ;; end this bunch of errors:
	  (dired-log-summary
	   (buffer-name (current-buffer))
	   (format
	    "Failed to %s %d of %d file%s"
	    operation count total (dired-plural-s total))
	   failures)))))

(defun dired-make-switches-string (list)
;; Converts a list of cracters to a string suitable for passing to ls.
  (concat "-" (mapconcat 'char-to-string list "")))

(defun dired-make-switches-list (string)
;; Converts a string of ls switches to a list of characters.
  (delq ?- (mapcar 'identity string)))

;; Cloning replace-match to work on strings instead of in buffer:
;; The FIXEDCASE parameter of replace-match is not implemented.
(defun dired-string-replace-match (regexp string newtext
					  &optional literal global)
  ;; Replace first match of REGEXP in STRING with NEWTEXT.
  ;; If it does not match, nil is returned instead of the new string.
  ;; Optional arg LITERAL means to take NEWTEXT literally.
  ;; Optional arg GLOBAL means to replace all matches.
  (if global
        (let ((result "") (start 0) mb me)
	  (while (string-match regexp string start)
	    (setq mb (match-beginning 0)
		  me (match-end 0)
		  result (concat result
				 (substring string start mb)
				 (if literal
				     newtext
				   (dired-expand-newtext string newtext)))
		  start me))
	  (if mb			; matched at least once
	      (concat result (substring string start))
	    nil))
    ;; not GLOBAL
    (if (not (string-match regexp string 0))
	nil
      (concat (substring string 0 (match-beginning 0))
	      (if literal newtext (dired-expand-newtext string newtext))
	      (substring string (match-end 0))))))

(defun dired-expand-newtext (string newtext)
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT, using match data.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
	(len (length newtext))
	(expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
	    (concat expanded-newtext
		    (let ((c (aref newtext pos)))
		      (if (= ?\\ c)
			  (cond ((= ?\& (setq c
					      (aref newtext
						    (setq pos (1+ pos)))))
				 (substring string
					    (match-beginning 0)
					    (match-end 0)))
				((and (>= c ?1) (<= c ?9))
				 ;; return empty string if N'th
				 ;; sub-regexp did not match:
				 (let ((n (- c ?0)))
				   (if (match-beginning n)
				       (substring string
						  (match-beginning n)
						  (match-end n))
				     "")))
				(t
				 (char-to-string c)))
			(char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

(defun dired-in-this-tree (file dir)
  ;;Is FILE part of the directory tree starting at DIR?
  (let ((len (length dir)))
    (and (>= (length file) len)
	 (string-equal (substring file 0 len) dir))))

(defun dired-tree-lessp (dir1 dir2)
  ;; Lexicographic order on pathname components, like `ls -lR':
  ;; DIR1 < DIR2 iff DIR1 comes *before* DIR2 in an `ls -lR' listing,
  ;;   i.e., iff DIR1 is a (grand)parent dir of DIR2,
  ;;   or DIR1 and DIR2 are in the same parentdir and their last
  ;;   components are string-lessp.
  ;; Thus ("/usr/" "/usr/bin") and ("/usr/a/" "/usr/b/") are tree-lessp.
  ;; string-lessp could arguably be replaced by file-newer-than-file-p
  ;;   if dired-internal-switches contained `t'.
  (let ((dir1 (file-name-as-directory dir1))
	(dir2 (file-name-as-directory dir2))
	(start1 1)
	(start2 1)
	comp1 comp2 end1 end2)
    (while (progn
	     (setq end1 (string-match "/" dir1 start1)
		   comp1 (substring dir1 start1 end1)
		   end2 (string-match "/" dir2 start2)
		   comp2 (substring dir2 start2 end2))
		   (and end1 end2 (string-equal comp1 comp2)))
      (setq start1 (1+ end1)
	    start2 (1+ end2)))
    (if (eq (null end1) (null end2))
	(string-lessp comp1 comp2)
      (null end1))))

;; So that we can support case-insensitive systems.
(fset 'dired-file-name-lessp 'string-lessp)


;;;; ------------------------------------------------------------------
;;;; Initializing Dired
;;;; ------------------------------------------------------------------

;;; Set the minor mode alist

(or (equal (assq 'dired-sort-mode minor-mode-alist)
	   '(dired-sort-mode dired-sort-mode))
    ;; Test whether this has already been done in case dired is reloaded
    ;; There may be several elements with dired-sort-mode as car.
    (setq minor-mode-alist
	  ;; cons " Omit" in first, so that it doesn't
	  ;; get stuck between the directory and sort mode on the
	  ;; mode line.
	  (cons '(dired-sort-mode dired-sort-mode)
		(cons '(dired-subdir-omit " Omit")
		      (cons '(dired-marker-stack dired-marker-string)
			    minor-mode-alist)))))

;;; Keymaps

(defvar dired-mode-map nil
  "Local keymap for dired-mode buffers.")
(defvar dired-regexp-map nil
  "Dired keymap for commands that use regular expressions.")
(defvar dired-diff-map nil
  "Dired keymap for diff and related commands.")
(defvar dired-subdir-map nil
  "Dired keymap for commands that act on subdirs, or the files within them.")

(defvar dired-keymap-grokked nil
  "Set to t after dired has grokked the global keymap.")

(defun dired-key-description (cmd &rest prefixes)
  ;; Return a key description string for a menu.  If prefixes are given,
  ;; they should be either strings, integers, or 'universal-argument.
  (let ((key (where-is-internal cmd dired-mode-map t)))
    (if key
	(key-description
	 (apply 'vconcat
		(append
		 (mapcar
		  (function
		   (lambda (x)
		     (cond ((eq x 'universal-argument)
			    (where-is-internal 'universal-argument
					       dired-mode-map t))
			   ((integerp x) (int-to-string x))
			   (t x))))
		  prefixes)
		 (list key))))
      "")))

(defun dired-grok-keys (to-command from-command)
  ;; Assigns to TO-COMMAND the keys for the global binding of FROM-COMMAND.
  ;; Does not clobber anything in the local keymap.  In emacs 19 should
  ;; use substitute-key-definition, but I believe that this will
  ;; clobber things in the local map.
  (let ((keys (where-is-internal from-command)))
    (while keys
      (condition-case nil
	  (if (eq (global-key-binding (car keys)) (key-binding (car keys)))
	      (local-set-key (car keys) to-command))
	(error nil))
      (setq keys (cdr keys)))))

(defun dired-grok-keymap ()
  ;; Initialize the dired keymaps.
  ;; This is actually done the first time that dired-mode runs.
  ;; We do it this late, to be sure that the user's global-keymap has
  ;; stabilized.
  (if dired-keymap-grokked
      () ; we've done it
    ;; Watch out for dired being invoked from the command line.
    ;; This is a bit kludgy, but so is the emacs startup sequence IMHO.
    (if (and term-setup-hook (boundp 'command-line-args-left))
	(progn
	  (if (string-equal "18." (substring emacs-version 0 3))
	      (funcall term-setup-hook)
	    (run-hooks 'term-setup-hook))
	  (setq term-setup-hook nil)))
    (setq dired-keymap-grokked t)
    (run-hooks 'dired-setup-keys-hook)
    (dired-grok-keys 'dired-next-line 'next-line)
    (dired-grok-keys 'dired-previous-line 'previous-line)
    (dired-grok-keys 'dired-undo 'undo)
    (dired-grok-keys 'dired-undo 'advertised-undo)
    (dired-grok-keys 'dired-scroll-up 'scroll-up)
    (dired-grok-keys 'dired-scroll-down 'scroll-down)
    (dired-grok-keys 'dired-beginning-of-buffer 'beginning-of-buffer)
    (dired-grok-keys 'dired-end-of-buffer 'end-of-buffer)
    (dired-grok-keys 'dired-next-subdir 'forward-paragraph)
    (dired-grok-keys 'dired-prev-subdir 'backward-paragraph)))

;; The regexp-map is used for commands using regexp's.
(if dired-regexp-map
    ()
  (setq dired-regexp-map (make-sparse-keymap))
  (define-key dired-regexp-map "C" 'dired-do-copy-regexp)
  ;; Not really a regexp, but does transform file names.
  (define-key dired-regexp-map "D" 'dired-downcase)
  (define-key dired-regexp-map "H" 'dired-do-hardlink-regexp)
  (define-key dired-regexp-map "R" 'dired-do-rename-regexp)
  (define-key dired-regexp-map "S" 'dired-do-symlink-regexp)
  (define-key dired-regexp-map "U" 'dired-upcase)
  (define-key dired-regexp-map "Y" 'dired-do-relsymlink-regexp)
  (define-key dired-regexp-map "c" 'dired-cleanup)
  (define-key dired-regexp-map "d" 'dired-flag-files-regexp)
  (define-key dired-regexp-map "e" 'dired-mark-extension)
  (define-key dired-regexp-map "m" 'dired-mark-files-regexp)
  (define-key dired-regexp-map "o" 'dired-add-omit-regexp)
  (define-key dired-regexp-map "x" 'dired-flag-extension)) ; a string, rather
					; than a regexp.

(if dired-diff-map
    ()
  (setq dired-diff-map (make-sparse-keymap))
  (define-key dired-diff-map "d" 'dired-diff)
  (define-key dired-diff-map "b" 'dired-backup-diff)
  (define-key dired-diff-map "m" 'dired-emerge)
  (define-key dired-diff-map "a" 'dired-emerge-with-ancestor)
  (define-key dired-diff-map "e" 'dired-ediff)
  (define-key dired-diff-map "p" 'dired-epatch))

(if dired-subdir-map
    ()
  (setq dired-subdir-map (make-sparse-keymap))
  (define-key dired-subdir-map "n" 'dired-redisplay-subdir)
  (define-key dired-subdir-map "m" 'dired-mark-subdir-files)
  (define-key dired-subdir-map "d" 'dired-flag-subdir-files)
  (define-key dired-subdir-map "z" 'dired-compress-subdir-files))

(fset 'dired-regexp-prefix dired-regexp-map)
(fset 'dired-diff-prefix dired-diff-map)
(fset 'dired-subdir-prefix dired-subdir-map)
(fset 'efs-dired-prefix (function (lambda ()
				    (interactive)
				    (error "efs-dired not loaded yet"))))

;; the main map
(if dired-mode-map
    nil
  ;; Force `f' rather than `e' in the mode doc:
  (fset 'dired-advertised-find-file 'dired-find-file)
  (fset 'dired-advertised-next-subdir 'dired-next-subdir)
  (fset 'dired-advertised-prev-subdir 'dired-prev-subdir)
  (setq dired-mode-map (make-keymap))
  (suppress-keymap dired-mode-map)
  ;; Commands to mark certain categories of files
  (define-key dired-mode-map "~" 'dired-flag-backup-files)
  (define-key dired-mode-map "#" 'dired-flag-auto-save-files)
  (define-key dired-mode-map "*" 'dired-mark-executables)
  (define-key dired-mode-map "." 'dired-clean-directory)
  (define-key dired-mode-map "/" 'dired-mark-directories)
  (define-key dired-mode-map "@" 'dired-mark-symlinks)
  (define-key dired-mode-map "," 'dired-mark-rcs-files)
  (define-key dired-mode-map "\M-(" 'dired-mark-sexp)
  (define-key dired-mode-map "\M-d" 'dired-mark-files-from-other-dired-buffer)
  (define-key dired-mode-map "\M-c" 'dired-mark-files-compilation-buffer)
  ;; Upper case keys (except ! and &) for operating on the marked files
  (define-key dired-mode-map "A" 'dired-do-tags-search)
  (define-key dired-mode-map "B" 'dired-do-byte-compile)
  (define-key dired-mode-map "C" 'dired-do-copy)
  (define-key dired-mode-map "E" 'dired-do-grep)
  (define-key dired-mode-map "F" 'dired-do-find-file)
  (define-key dired-mode-map "G" 'dired-do-chgrp)
  (define-key dired-mode-map "H" 'dired-do-hardlink)
  (define-key dired-mode-map "I" 'dired-do-insert-subdir)
  (define-key dired-mode-map "K" 'dired-do-kill-file-lines)
  (define-key dired-mode-map "L" 'dired-do-load)
  (define-key dired-mode-map "M" 'dired-do-chmod)
  (define-key dired-mode-map "N" 'dired-do-redisplay)
  (define-key dired-mode-map "O" 'dired-do-chown)
  (define-key dired-mode-map "P" 'dired-do-print)
  (define-key dired-mode-map "Q" 'dired-do-tags-query-replace)
  (define-key dired-mode-map "R" 'dired-do-rename)
  (define-key dired-mode-map "S" 'dired-do-symlink)
  (define-key dired-mode-map "T" 'dired-do-total-size)
  (define-key dired-mode-map "U" 'dired-do-uucode)
  (define-key dired-mode-map "W" 'dired-copy-filenames-as-kill)
  (define-key dired-mode-map "X" 'dired-do-delete)
  (define-key dired-mode-map "Y" 'dired-do-relsymlink)
  (define-key dired-mode-map "Z" 'dired-do-compress)
  (define-key dired-mode-map "!" 'dired-do-shell-command)
  (define-key dired-mode-map "&" 'dired-do-background-shell-command)
  ;; Make all regexp commands share a `%' prefix:
  (define-key dired-mode-map "%" 'dired-regexp-prefix)
  ;; Lower keys for commands not operating on all the marked files
  (define-key dired-mode-map "a" 'dired-apropos)
  (define-key dired-mode-map "c" 'dired-change-marks)
  (define-key dired-mode-map "d" 'dired-flag-file-deletion)
  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion-backup)
  (define-key dired-mode-map "e" 'dired-find-file)
  (define-key dired-mode-map "f" 'dired-advertised-find-file)
  (define-key dired-mode-map "g" 'revert-buffer)
  (define-key dired-mode-map "h" 'dired-describe-mode)
  (define-key dired-mode-map "i" 'dired-maybe-insert-subdir)
  (define-key dired-mode-map "k" 'dired-kill-subdir)
  (define-key dired-mode-map "m" 'dired-mark)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "q" 'dired-quit)
  (define-key dired-mode-map "r" 'dired-read-mail)
  (define-key dired-mode-map "s" 'dired-sort-toggle-or-edit)
  (define-key dired-mode-map "t" 'dired-get-target-directory)
  (define-key dired-mode-map "u" 'dired-unmark)
  (define-key dired-mode-map "v" 'dired-view-file)
  (define-key dired-mode-map "w" (if (fboundp 'find-file-other-frame)
				     'dired-find-file-other-frame
				   'dired-find-file-other-window))
  (define-key dired-mode-map "x" 'dired-expunge-deletions)
  (define-key dired-mode-map "y" 'dired-why)
  (define-key dired-mode-map "+" 'dired-create-directory)
  (define-key dired-mode-map "`" 'dired-recover-file)
  ;; dired-jump-back Should be in the global map, but put them here
  ;; too anyway.
  (define-key dired-mode-map "\C-x\C-j" 'dired-jump-back)
  (define-key dired-mode-map "\C-x4\C-j" 'dired-jump-back-other-window)
  (define-key dired-mode-map "\C-x5\C-j" 'dired-jump-back-other-frame)
  ;; Comparison commands
  (define-key dired-mode-map "=" 'dired-diff-prefix)
  ;; moving
  (define-key dired-mode-map "<" 'dired-prev-dirline)
  (define-key dired-mode-map ">" 'dired-next-dirline)
  (define-key dired-mode-map " "  'dired-next-line)
  (define-key dired-mode-map "n" 'dired-next-line)
  (define-key dired-mode-map "\C-n" 'dired-next-line)
  (define-key dired-mode-map "p" 'dired-previous-line)
  (define-key dired-mode-map "\C-p" 'dired-previous-line)
  (define-key dired-mode-map "\C-v" 'dired-scroll-up)
  (define-key dired-mode-map "\M-v" 'dired-scroll-down)
  (define-key dired-mode-map "\M-<" 'dired-beginning-of-buffer)
  (define-key dired-mode-map "\M->" 'dired-end-of-buffer)
  (define-key dired-mode-map "\C-m" 'dired-advertised-find-file)
  ;; motion by subdirectories
  (define-key dired-mode-map "^" 'dired-up-directory)
  (define-key dired-mode-map "\M-\C-u" 'dired-up-directory)
  (define-key dired-mode-map "\M-\C-d" 'dired-down-directory)
  (define-key dired-mode-map "\M-\C-n" 'dired-advertised-next-subdir)
  (define-key dired-mode-map "\M-\C-p" 'dired-advertised-prev-subdir)
  (define-key dired-mode-map "\C-j" 'dired-goto-subdir)
  ;; move to marked files
  (define-key dired-mode-map "\M-p" 'dired-prev-marked-file)
  (define-key dired-mode-map "\M-n" 'dired-next-marked-file)
  ;; hiding
  (define-key dired-mode-map "$" 'dired-hide-subdir)
  (define-key dired-mode-map "\M-$" 'dired-hide-all)
  ;; omitting
  (define-key dired-mode-map "\C-o" 'dired-omit-toggle)
  ;; markers
  (define-key dired-mode-map "\(" 'dired-set-marker-char)
  (define-key dired-mode-map "\)" 'dired-restore-marker-char)
  (define-key dired-mode-map "'" 'dired-marker-stack-left)
  (define-key dired-mode-map "\\" 'dired-marker-stack-right)
  ;; misc
  (define-key dired-mode-map "\C-i" 'dired-mark-prefix)
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map "\177" 'dired-backup-unflag)
  (define-key dired-mode-map "\C-_" 'dired-undo)
  (define-key dired-mode-map "\C-xu" 'dired-undo)
  (define-key dired-mode-map "\M-\C-?" 'dired-unmark-all-files)
  ;; The subdir map
  (define-key dired-mode-map "|" 'dired-subdir-prefix)
  ;; efs submap
  (define-key dired-mode-map "\M-e" 'efs-dired-prefix))



;;;;------------------------------------------------------------------
;;;; The dired command
;;;;------------------------------------------------------------------

;;; User commands:
;;; All of these commands should have a binding in the global keymap.

;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the resr as an explicit
list of files to make directory entries for.
\\<dired-mode-map>\
You can move around in it with the usual commands.
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-expunge-deletions].
Type \\[dired-describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh."
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-4-map "d" 'dired-other-window)
;;;###autoload
(defun dired-other-window (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but selects in another window."
  (interactive (dired-read-dir-and-switches "in other window "))
  (switch-to-buffer-other-window (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-5-map "d" 'dired-other-frame)
;;;###autoload
(defun dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))

;;;###autoload
(defun dired-noselect (dir-or-list &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dir-or-list (setq dir-or-list (expand-file-name default-directory)))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (let (dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq dirname (expand-file-name (directory-file-name dirname)))
    (if (file-directory-p dirname)
	(setq dirname (file-name-as-directory dirname)))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list)))
      (setq dir-or-list dirname))
    (dired-internal-noselect dir-or-list switches)))

;; Adapted from code by wurgler@zippysun.math.uakron.edu (Tom Wurgler).
;;;###autoload (define-key ctl-x-map "\C-j" 'dired-jump-back)
;;;###autoload
(defun dired-jump-back ()
  "Jump back to dired.
If in a file, dired the current directory and move to file's line.
If in dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
  buffer and try again."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
		   (directory-file-name (dired-current-directory))
		 buffer-file-name))
	 (dir (if file
		  (file-name-directory file)
		default-directory)))
    (dired dir)
    (if file (dired-really-goto-file file))))

;;;###autoload (define-key ctl-x-4-map "\C-j" 'dired-jump-back-other-window)
;;;###autoload
(defun dired-jump-back-other-window ()
  "Like \\[dired-jump-back], but to other window."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
		   (directory-file-name (dired-current-directory))
		 buffer-file-name))
	 (dir (if file
		  (file-name-directory file)
		default-directory)))
    (dired-other-window dir)
    (if file (dired-really-goto-file file))))

;;;###autoload (define-key ctl-x-5-map "\C-j" 'dired-jump-back-other-frame)
;;;###autoload
(defun dired-jump-back-other-frame ()
  "Like \\[dired-jump-back], but in another frame."
  (interactive)
  (let* ((file (if (eq major-mode 'dired-mode)
		   (directory-file-name (dired-current-directory))
		 buffer-file-name))
	 (dir (if file
		  (file-name-directory file)
		default-directory)))
    (dired-other-frame dir)
    (if file (dired-really-goto-file file))))

;;; Dired mode

;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defun dired-mode (&optional dirname switches)
  "\\<dired-mode-map>Dired mode is for \"editing\" directory trees.

For a simple one-line help message, type \\[dired-summary]
For a moderately detailed description of dired mode, type \\[dired-describe-mode]
For the full dired info tree, type \\[universal-argument] \\[dired-describe-mode]"
  ;; Not to be called interactively (e.g. dired-directory will be set
  ;; to default-directory, which is wrong with wildcards).
  (kill-all-local-variables)
  (use-local-map dired-mode-map)
  (setq major-mode 'dired-mode
	mode-name "Dired"
	case-fold-search nil
	buffer-read-only t
	selective-display t		; for subdirectory hiding
	selective-display-ellipses nil  ; for omit toggling
	mode-line-buffer-identification '("Dired: %12b")
	mode-line-modified (format dired-mode-line-modified "--" "--" "-")
	dired-directory (expand-file-name (or dirname default-directory))
	dired-internal-switches (dired-make-switches-list
			       (or switches dired-listing-switches)))
  (dired-advertise)			; default-directory is already set
  (set (make-local-variable 'revert-buffer-function)
       (function dired-revert))
  (set (make-local-variable 'default-directory-function)
       'dired-current-directory)
  (set (make-local-variable 'page-delimiter)
       "\n\n")
  (set (make-local-variable 'list-buffers-directory)
       dired-directory)
  ;; Will only do something in Emacs 19.
  (add-hook (make-local-variable 'kill-buffer-hook)
	    'dired-unadvertise-current-buffer)
  ;; Same here
  (if window-system
      (add-hook (make-local-variable 'post-command-hook)
		(function
		 (lambda ()
		   (if (memq this-command dired-modeline-tracking-cmds)
		       (dired-update-mode-line t))))))
  (dired-sort-other dired-internal-switches t)
  (dired-hack-local-variables)
  (run-hooks 'dired-mode-hook)
  ;; Run this after dired-mode-hook, in case that hook makes changes to
  ;; the keymap.
  (dired-grok-keymap))

;;; Internal functions for starting dired

(defun dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
	    (if current-prefix-arg
		(read-string "Dired listing switches: "
			     dired-listing-switches))
	    (let ((default-directory (default-directory)))
	      (read-file-name (format "Dired %s(directory): " str)
			      nil default-directory nil)))))

(defun dired-hack-local-variables ()
  "Parse, bind or evaluate any local variables for current dired buffer.
See variable `dired-local-variables-file'."
  (if (and dired-local-variables-file
	   (file-exists-p dired-local-variables-file))
      (let (buffer-read-only opoint )
	(save-excursion
	  (goto-char (point-max))
	  (setq opoint (point-marker))
	  (insert "\^L\n")
	  (insert-file-contents dired-local-variables-file))
	(let ((buffer-file-name dired-local-variables-file))
	  (condition-case err
	      (hack-local-variables)
	    (error (message "Error in dired-local-variables-file: %s" err)
		   (sit-for 1))))
	;; Must delete it as (eobp) is often used as test for last
	;; subdir in dired.el.
	(delete-region opoint (point-max))
	(set-marker opoint nil))))

;; Separate function from dired-noselect for the sake of dired-vms.el.
(defun dired-internal-noselect (dir-or-list &optional switches mode)
  ;; If there is an existing dired buffer for DIRNAME, just leave
  ;; buffer as it is (don't even call dired-revert).
  ;; This saves time especially for deep trees or with efs.
  ;; The user can type `g'easily, and it is more consistent with find-file.
  ;; But if SWITCHES are given they are probably different from the
  ;; buffer's old value, so call dired-sort-other, which does
  ;; revert the buffer.
  ;; If the user specifies a directory with emacs startup, eg.
  ;; emacs ~, dir-or-list may be unexpanded at this point.

  (let* ((dirname (expand-file-name (if (consp dir-or-list)
					(car dir-or-list)
				      dir-or-list)))
	 (buffer (dired-find-buffer-nocreate dir-or-list mode))
	 ;; note that buffer already is in dired-mode, if found
	 (new-buffer-p (not buffer))
	 (old-buf (current-buffer))
	 wildcard)
    (or buffer
	(let ((default-major-mode 'fundamental-mode))
	  ;; We don't want default-major-mode to run hooks and set auto-fill
	  ;; or whatever, now that dired-mode does not
	  ;; kill-all-local-variables any longer.
	  (setq buffer (create-file-buffer (directory-file-name dirname)))))
    (set-buffer buffer)
    (if (not new-buffer-p)		; existing buffer ...
	(progn
	  (if switches
	      (dired-sort-other
	       (if (stringp switches)
		   (dired-make-switches-list switches)
		 switches)))
	  (if dired-verify-modtimes (dired-verify-modtimes))
	  (if (and dired-find-subdir
		   (not (string-equal (dired-current-directory)
				      (file-name-as-directory dirname))))
	      (dired-initial-position dirname)))
      ;; Else a new buffer
      (if (file-directory-p dirname)
	  (setq default-directory dirname
		wildcard (consp dir-or-list))
	(setq default-directory (file-name-directory dirname)
	      wildcard t))
      (or switches (setq switches dired-listing-switches))
      (dired-mode dirname switches)
      ;; default-directory and dired-internal-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (let ((failed t))
	(unwind-protect
	    (progn (dired-readin dir-or-list buffer wildcard)
		   (setq failed nil))
	  ;; dired-readin can fail if parent directories are inaccessible.
	  ;; Don't leave an empty buffer around in that case.
	  (if failed (kill-buffer buffer))))
      ;; No need to narrow since the whole buffer contains just
      ;; dired-readin's output, nothing else.  The hook can
      ;; successfully use dired functions (e.g. dired-get-filename)
      ;; as the subdir-alist has been built in dired-readin.
      (run-hooks 'dired-after-readin-hook)
      ;; I put omit-expunge after the dired-after-readin-hook
      ;; in case that hook marks files. Does this make sense? Also, users
      ;; might want to set dired-omit-files in some incredibly clever
      ;; way depending on the contents of the directory... I don't know...
      (if dired-omit-files
	  (dired-omit-expunge nil t))
      (goto-char (point-min))
      (dired-initial-position dirname))
    (set-buffer old-buf)
    buffer))

(defun dired-find-buffer-nocreate (dir-or-list &optional mode)
  ;; Returns a dired buffer for DIR-OR-LIST. DIR-OR-LIST may be wildcard,
  ;; or a directory and alist of files.
  ;; If dired-find-subdir is non-nil, is satisfied with a dired
  ;; buffer containing DIR-OR-LIST as a subdirectory. If there is more
  ;; than one candidate, returns the most recently used.
  (if dired-find-subdir
      (let ((buffers (sort (delq (current-buffer)
				 (dired-buffers-for-dir dir-or-list t))
			   (function dired-buffer-more-recently-used-p))))
	(or (car buffers)
	    ;; Couldn't find another buffer. Will the current one do?
	    ;; It is up dired-initial-position to actually go to the subdir.
	    (and (or (equal dir-or-list dired-directory) ; covers wildcards
		     (and (stringp dir-or-list)
			  (not (string-equal
				dir-or-list
				(expand-file-name default-directory)))
			  (assoc (file-name-as-directory dir-or-list)
				 dired-subdir-alist)))
		 (current-buffer))))
    ;; Else just look through the buffer list.
    (let (found (blist (buffer-list)))
      (or mode (setq mode 'dired-mode))
      (save-excursion
	(while blist
	  (set-buffer (car blist))
	  (if (and (eq major-mode mode)
		   (equal dired-directory dir-or-list))
	      (setq found (car blist)
		    blist nil)
	    (setq blist (cdr blist)))))
      found)))

(defun dired-initial-position (dirname)
  ;; Where point should go in a new listing of DIRNAME.
  ;; Point assumed at beginning of new subdir line.
  (end-of-line)
  (if dired-find-subdir (dired-goto-subdir dirname))
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file))
  (dired-update-mode-line t))

(defun dired-readin (dir-or-list buffer &optional wildcard)
  ;; Read in a new dired buffer
  ;; dired-readin differs from dired-insert-subdir in that it accepts
  ;; wildcards, erases the buffer, and builds the subdir-alist anew
  ;; (including making it buffer-local and clearing it first).
  ;; default-directory and dired-internal-switches must be buffer-local
  ;; and initialized by now.
  ;; Thus we can test (equal default-directory dirname) instead of
  ;; (file-directory-p dirname) and save a filesystem transaction.
  ;; This is wrong, if dired-before-readin-hook changes default-directory
  ;; Also, we can run this hook which may want to modify the switches
  ;; based on default-directory, e.g. with efs to a SysV host
  ;; where ls won't understand -Al switches.
  (let (dirname other-dirs)
    (if (consp dir-or-list)
	(setq dir-or-list (dired-frob-dir-list dir-or-list)
	      other-dirs (cdr dir-or-list)
	      dir-or-list (car dir-or-list)
	      dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq dirname (expand-file-name dirname))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list))))
    (save-excursion
      (set-buffer buffer)
      (run-hooks 'dired-before-readin-hook)
      (message "Reading directory %s..." dirname)
      (let (buffer-read-only)
	(widen)
	(erase-buffer)
	(dired-readin-insert dir-or-list wildcard)
	(dired-indent-listing (point-min) (point-max))
	;; We need this to make the root dir have a header line as all
	;; other subdirs have:
	(goto-char (point-min))
	(dired-insert-headerline (expand-file-name default-directory)))
      (message "Reading directory %s...done" dirname)
      (set-buffer-modified-p nil)
      ;; Must first make alist buffer local and set it to nil because
      ;; dired-build-subdir-alist will call dired-clear-alist first
      (setq dired-subdir-alist nil)
      (if (memq ?R dired-internal-switches)
	  (dired-build-subdir-alist)
	;; no need to parse the buffer if listing is not recursive
	(dired-simple-subdir-alist))
      (if other-dirs
	  (mapcar
	   (function
	    (lambda (x)
	      (if (dired-in-this-tree (car x) dirname)
		  (dired-insert-subdir x))))
	   other-dirs)))))
  
;;; Subroutines of dired-readin

(defun dired-readin-insert (dir-or-list &optional wildcard)
  ;; Just insert listing for the passed-in directory or
  ;; directory-and-file list, assuming a clean buffer.
  (let* ((switches (dired-make-switches-string dired-internal-switches))
	 (dir-is-list (consp dir-or-list))
	 (dirname (if dir-is-list (car dir-or-list) dir-or-list)))
    (if wildcard
	(progn
	  (or (file-readable-p
	       (if dir-is-list
		   dirname
		 (directory-file-name (file-name-directory dirname))))
	      (error "Directory %s inaccessible or nonexistent" dirname))
	  ;; else assume it contains wildcards
	  (dired-insert-directory dir-or-list switches t)
	  (save-excursion
	    ;; insert wildcard instead of total line:
	    (goto-char (point-min))
	    (if dir-is-list
		(insert "list wildcard\n")
	      (insert "wildcard " (file-name-nondirectory dirname) "\n"))))
      (dired-insert-directory dir-or-list switches nil t))))

(defun dired-insert-directory (dir-or-list switches &optional wildcard full-p)
  ;; Do the right thing whether dir-or-list is atomic or not.  If it is,
  ;; insert all files listed in the cdr -- the car is the passed-in directory
  ;; list.
  (let ((opoint (point))
	(insert-directory-program dired-ls-program))
    (if (consp dir-or-list)
	(mapcar
	 (function
	  (lambda (x)
	    (insert-directory x switches wildcard)))
	 (cdr dir-or-list))
      (insert-directory dir-or-list switches wildcard full-p))
    (dired-insert-set-properties opoint (point)))
  (setq dired-directory dir-or-list))

(defun dired-frob-dir-list (dir-list)
  (let* ((top (file-name-as-directory (expand-file-name (car dir-list))))
	 (tail (cdr dir-list))
	 (result (list (list top)))
	 elt dir)
    (setq tail
	  (mapcar
	   (function
	    (lambda (x)
	      (directory-file-name (expand-file-name x top))))
	   tail))
    (while tail
      (setq dir (file-name-directory (car tail)))
      (if (setq elt (assoc dir result))
	  (nconc elt (list (car tail)))
	(nconc result (list (list dir (car tail)))))
      (setq tail (cdr tail)))
    result))

(defun dired-insert-headerline (dir);; also used by dired-insert-subdir
  ;; Insert DIR's headerline with no trailing slash, exactly like ls
  ;; would, and put cursor where dired-build-subdir-alist puts subdir
  ;; boundaries.
  (save-excursion (insert "  " (directory-file-name dir) ":\n")))

(defun dired-verify-modtimes ()
  ;; Check the modtimes of all subdirs.
  (let ((alist dired-subdir-alist)
	on-disk in-mem badies)
    (while alist
      (and (setq in-mem (nth 4 (car alist)))
	   (setq on-disk (dired-file-modtime (car (car alist))))
	   (not (equal in-mem on-disk))
	   (setq badies (cons (cons (car (car alist))
				    (nth 3 (car alist)))
			      badies)))
      (setq alist (cdr alist)))
    (and badies
	 (let* ((ofile (dired-get-filename nil t))
		(osub (and (null ofile) (dired-get-subdir)))
		(opoint (point))
		(ocol (current-column)))
	   (unwind-protect
	       (and
		(or (memq 'revert-subdirs dired-no-confirm)
		    (save-window-excursion
		      (let ((flist (mapcar
				    (function
				     (lambda (f)
				       (dired-abbreviate-file-name (car f))))
				    badies)))
			(switch-to-buffer (current-buffer))
		       (dired-mark-pop-up
			"*Stale Subdirectories*" 'revert-subdirs
			flist 'y-or-n-p
			(if (= (length flist) 1)
			    (concat "Subdirectory " (car flist)
				    " has changed on disk.  Re-list? ")
			  "Subdirectories have changed on disk.  Re-list? "))
		       )))
		(while badies
		  (dired-insert-subdir (car (car badies))
				       (cdr (car badies)) nil t)
		  (setq badies (cdr badies))))
	     ;; We can't use dired-save-excursion here, because we are
	     ;; rewriting the entire listing, and not just changing a single
	     ;; file line.
	     (or (if ofile
		     (dired-goto-file ofile)
		   (if osub
		       (dired-goto-subdir osub)))
		 (progn
		   (goto-char opoint)
		   (beginning-of-line)
		   (skip-chars-forward "^\n\r" (+ (point) ocol))))
	     (dired-update-mode-line t)
	     (dired-update-mode-line-modified t))))))

(defun dired-indent-listing (start end)
  ;; Indent a dired listing.
  (let (indent-tabs-mode)
    (indent-rigidly start end 2)
    ;; Quote any null lines that shouldn't be.
    (save-excursion
      (goto-char start)
      (while (search-forward "\n\n" end t)
	(forward-char -2)
	(if (looking-at dired-subdir-regexp)
	    (goto-char (match-end 3))
	  (progn
	    (forward-char 1)
	    (insert " ")))))))


;;;; ------------------------------------------------------------
;;;; Reverting a dired buffer, or specific file lines within it.
;;;; ------------------------------------------------------------

(defun dired-revert (&optional arg noconfirm)
  ;; Reread the dired buffer.  Must also be called after
  ;; dired-internal-switches have changed.
  ;; Should not fail even on completely garbaged buffers.
  ;; Preserves old cursor, marks/flags, hidden-p.
  (widen)				; just in case user narrowed
  (let ((opoint (point))
	(ofile (dired-get-filename nil t))
	(hidden-subdirs (dired-remember-hidden))
	;; switches for top-level dir
	(oswitches (or (nth 3 (nth (1- (length dired-subdir-alist))
				   dired-subdir-alist))
		       (delq ?R (copy-sequence dired-internal-switches))))
	;; all other subdirs
	(old-subdir-alist (cdr (reverse dired-subdir-alist)))
	(omitted-subdirs (dired-remember-omitted))
	;; do this after dired-remember-hidden, since this unhides
	(mark-alist (dired-remember-marks (point-min) (point-max)))
	(kill-files-p (save-excursion
			(goto-char (point))
			(search-forward
			 (concat (char-to-string ?\r)
				 (regexp-quote
				  (char-to-string
				   dired-kill-marker-char)))
			 nil t)))
	buffer-read-only)
    ;; This is bogus, as it will not handle all the ways that efs uses cache.
    ;; Better to just use the fact that revert-buffer-function is a
    ;; buffer-local variable, and reset it to something that knows about
    ;; cache.
    ;; (dired-uncache
    ;;   (if (consp dired-directory) (car dired-directory) dired-directory))
    ;; treat top level dir extra (it may contain wildcards)
    (let ((dired-after-readin-hook nil)
	  ;; don't run that hook for each subdir...
	  (dired-omit-files nil)
	  (dired-internal-switches oswitches))
      (dired-readin dired-directory (current-buffer)
		    ;; Don't test for wildcards by checking string=
		    ;; default-directory and dired-directory
		    ;; in case default-directory got munged.
		    (or (consp dired-directory)
			(null (file-directory-p dired-directory))))
      ;; The R-switch will clobber sorting of subdirs.
      ;; What is the right thing to do here?
      (dired-insert-old-subdirs old-subdir-alist))
    (dired-mark-remembered mark-alist)	; mark files that were marked
    (if kill-files-p (dired-do-hide dired-kill-marker-char))
    (run-hooks 'dired-after-readin-hook)	; no need to narrow
    ;; omit-expunge after the readin hook
    (save-excursion
      (mapcar (function (lambda (dir)
			  (if (dired-goto-subdir dir)
			      (dired-omit-expunge))))
	      omitted-subdirs))
    ;; hide subdirs that were hidden
    (save-excursion
      (mapcar (function (lambda (dir)
			  (if (dired-goto-subdir dir)
			      (dired-hide-subdir 1))))
	      hidden-subdirs))
    ;; Try to get back to where we were
    (or (and ofile (dired-goto-file ofile))
	(goto-char opoint))
    (dired-move-to-filename)
    (dired-update-mode-line t)
    (dired-update-mode-line-modified t)))

(defun dired-do-redisplay (&optional arg)
  "Redisplay all marked (or next ARG) files."
  (interactive "P")
  ;; message instead of making dired-map-over-marks show-progress is
  ;; much faster
  (dired-map-over-marks (let ((fname (dired-get-filename)))
			  (dired-uncache fname nil)
			  (message "Redisplaying %s..." fname)
			  (dired-update-file-line fname))
			arg)
  (dired-update-mode-line-modified t)
  (message "Redisplaying...done"))

(defun dired-redisplay-subdir (&optional arg)
  "Redisplay the current subdirectory.
With a prefix prompts for listing switches."
  (interactive "P")
  (let ((switches (and arg (dired-make-switches-list
			    (read-string "Switches for listing: "
					 (dired-make-switches-string
					  dired-internal-switches)))))
	(dir (dired-current-directory))
	(opoint (point))
	(ofile (dired-get-filename nil t)))
    (or switches
	(setq switches (nth 3 (assoc dir dired-subdir-alist))))
    (or switches
	(setq switches (delq ?R (copy-sequence dired-internal-switches))))
    (message "Redisplaying %s..." dir)
    (dired-uncache dir t)
    (dired-insert-subdir dir switches)
    (dired-update-mode-line-modified t)
    (or (and ofile (dired-goto-file ofile)) (goto-char opoint))
    (message "Redisplaying %s... done" dir)))

(defun dired-update-file-line (file)
  ;; Delete the current line, and insert an entry for FILE.
  ;; Does not update other dired buffers.  Use dired-relist-file for that.
  (let* ((start (save-excursion (skip-chars-backward "^\n\r") (point)))
	 (char (char-after start)))
    (dired-save-excursion
     ;; don't remember omit marks
     (if (memq char (list ?\040 dired-omit-marker-char))
	 (setq char nil))
     ;; Delete the current-line. Even though dired-add-entry will not
     ;; insert duplicates, the file for the current line may not be the same as
     ;; FILE. eg. dired-do-compress
     (delete-region (save-excursion (skip-chars-backward "^\n\r") (1- (point)))
		    (progn (skip-chars-forward "^\n\r") (point)))
     ;; dired-add-entry inserts at the end of the previous line.
     (forward-char 1)
     (dired-add-entry file char t))))

;;; Subroutines of dired-revert
;;; Some of these are also used when inserting subdirs.

;; Don't want to remember omit marks, in case omission regexps
;; were changed, before the dired-revert. If we don't unhide
;; omitted files, we won't see their marks. Therefore we use
;; dired-omit-unhide-region.

(defun dired-remember-marks (beg end)
  ;; Return alist of files and their marks, from BEG to END.
  (if selective-display			; must unhide to make this work.
      (let (buffer-read-only)
	(subst-char-in-region (point-min) (point-max) ?\r ?\n)
	(dired-do-hide dired-omit-marker-char)))
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (dired-get-filename nil t))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

(defun dired-mark-remembered (alist)
  ;; Mark all files remembered in ALIST.
  (let (elt fil chr)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    fil (car elt)
	    chr (cdr elt))
      (if (dired-goto-file fil)
	  (save-excursion
	    (beginning-of-line)
	    (dired-substitute-marker (point) (following-char) chr))))))

(defun dired-remember-hidden ()
  ;; Return a list of all hidden subdirs.
  (let ((l dired-subdir-alist) dir result min)
    (while l
      (setq dir (car (car l))
	    min (dired-get-subdir-min (car l))
	    l (cdr l))
      (if (and (>= min (point-min)) (<= min (point-max))
	       (dired-subdir-hidden-p dir))
	  (setq result (cons dir result))))
    result))

(defun dired-insert-old-subdirs (old-subdir-alist)
  ;; Try to insert all subdirs that were displayed before
  (let (elt dir switches)
    (while old-subdir-alist
      (setq elt (car old-subdir-alist)
	    old-subdir-alist (cdr old-subdir-alist)
	    dir (car elt)
	    switches (or (nth 3 elt) dired-internal-switches))
      (condition-case ()
	  (dired-insert-subdir dir switches)
	(error nil)))))

(defun dired-uncache (file dir-p)
  ;; Remove directory DIR from any directory cache.
  ;; If DIR-P is non-nil, then FILE is a directory
  (let ((handler (find-file-name-handler file 'dired-uncache)))
    (if handler
	(funcall handler 'dired-uncache file dir-p))))


;;;; -------------------------------------------------------------
;;;; Inserting subdirectories
;;;; -------------------------------------------------------------

(defun dired-maybe-insert-subdir (dirname &optional
					  switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to 
  refresh), else inserts it at its natural place (as ls -lR would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to ls -lR output."
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (dired-make-switches-list
	      (read-string "Switches for listing: "
			   (dired-make-switches-string
			    dired-internal-switches))))))
  (let ((opoint (point)))
    ;; We don't need a marker for opoint as the subdir is always
    ;; inserted *after* opoint.
    (setq dirname (file-name-as-directory dirname))
    (or (and (not switches)
	     (dired-goto-subdir dirname))
	(dired-insert-subdir dirname switches no-error-if-not-dir-p))
    ;; Push mark so that it's easy to find back.  Do this after the
    ;; insert message so that the user sees the `Mark set' message.
    (push-mark opoint)))

(defun dired-insert-subdir (dir-or-list &optional
					switches no-error-if-not-dir-p no-posn)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it at its natural place (as ls -lR would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to ls -lR output."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  ;; SWITCHES should be a list.
  ;; If NO-POSN is non-nil, doesn't bother position the point at
  ;; the first nontrivial file line.  This can be used as an efficiency
  ;; hack when calling this from a program.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (dired-make-switches-list
	      (read-string "Switches for listing: "
			   (dired-make-switches-string
			    dired-internal-switches))))))
  (let ((dirname (if (consp dir-or-list) (car dir-or-list) dir-or-list)))
    (setq dirname (file-name-as-directory (expand-file-name dirname)))
    (or (dired-in-this-tree dirname (expand-file-name default-directory))
	(error  "%s: not in this directory tree" dirname))
    (or no-error-if-not-dir-p
	(file-directory-p dirname)
	(error  "Attempt to insert a non-directory: %s" dirname))
    (if switches
	(or (dired-compatible-switches-p dired-internal-switches switches)
	    (error "Cannot have subdirs with %s and %s switches together."
		   (dired-make-switches-string dired-internal-switches)
		   (dired-make-switches-string switches)))
      (setq switches dired-internal-switches))
    (let ((elt (assoc dirname dired-subdir-alist))
	  mark-alist opoint-max buffer-read-only)
      (if (memq ?R switches)
	  ;; avoid duplicated subdirs
	  (progn
	    (setq mark-alist (dired-kill-tree dirname t))
	    (dired-insert-subdir-newpos dirname))
	(if elt
	    ;; If subdir is already present, remove it and remember its marks
	    (setq mark-alist (dired-insert-subdir-del elt))
	  ;; else move to new position
	  (dired-insert-subdir-newpos dirname)))
      (setq opoint-max (point-max))
      (condition-case nil
	  (dired-insert-subdir-doupdate
	   dirname (dired-insert-subdir-doinsert dir-or-list switches)
	   switches elt mark-alist)
	(quit ; watch out for aborted inserts
	 (and (= opoint-max (point-max))
	      (null elt)
	      (= (preceding-char) ?\n)
	      (delete-char -1))
	 (signal 'quit nil))))
    (or no-posn (dired-initial-position dirname))))

(defun dired-do-insert-subdir ()
  "Insert all marked subdirectories in situ that are not yet inserted.
Non-directories are silently ignored."
  (interactive)
  (let ((files (or (dired-get-marked-files)
		   (error "No files marked."))))
    (while files
      (if (file-directory-p (car files))
	  (save-excursion (dired-maybe-insert-subdir (car files))))
      (setq files (cdr files)))))

;;; Utilities for inserting subdirectories

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  (let ((alist dired-subdir-alist) elt dir new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt))
      (if (dired-tree-lessp dir new-dir)
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  (insert "\n")
  (point))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (dired-get-subdir-min element)))
    (goto-char begin-marker)
    ;; Are at beginning of subdir (and inside it!).  Now determine its end:
    (goto-char (dired-subdir-max))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))

(defun dired-insert-subdir-doinsert (dir-or-list switches)
  ;; Insert ls output after point and put point on the correct
  ;; position for the subdir alist.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  ;; SWITCHES should be a non-nil list.
  (let ((begin (point))
	(dirname (if (consp dir-or-list) (car dir-or-list) dir-or-list))
	end)
    (message "Reading directory %s..." dirname)
    (if (string-equal dirname (car (car (reverse dired-subdir-alist))))
	;; top level directory may contain wildcards:
	(let ((dired-internal-switches switches))
	  (dired-readin-insert dired-directory
			       (null (file-directory-p dired-directory))))
      (let ((switches (dired-make-switches-string switches))
	    (insert-directory-program dired-ls-program))
	(if (consp dir-or-list)
	    (progn
	      (insert "list wildcard\n")
	      (mapcar
	       (function
		(lambda (x)
		  (insert-directory x switches t)))
	       (cdr dir-or-list)))
	  (insert-directory dirname switches nil t))))
    (message "Reading directory %s...done" dirname)
    (setq end (point-marker))
    (dired-indent-listing begin end)
    (dired-insert-set-properties begin end)
    ;;  call dired-insert-headerline afterwards, as under VMS dired-ls
    ;;  does insert the headerline itself and the insert function just
    ;;  moves point.
    ;;  Need a marker for END as this inserts text.
    (goto-char begin)
    (dired-insert-headerline dirname)
    ;; point is now like in dired-build-subdir-alist
    (prog1
	(list begin (marker-position end))
      (set-marker end nil))))

(defun dired-insert-subdir-doupdate (dirname beg-end switches elt mark-alist)
  ;; Point is at the correct subdir alist position for ELT,
  ;; BEG-END is the subdir-region (as list of begin and end).
  ;; SWITCHES must be a non-nil list.
  (if (memq ?R switches)
      ;; This will remove ?R from switches on purpose.
      (let ((dired-internal-switches (delq ?R switches)))
	(dired-build-subdir-alist))
    (if elt
	(progn
	  (set-marker (dired-get-subdir-min elt) (point-marker))
	  (setcar (nthcdr 3 elt) switches)
	  (if dired-verify-modtimes
	      (dired-set-file-modtime dirname dired-subdir-alist)))
      (dired-alist-add dirname (point-marker) dired-omit-files switches)))
  (save-excursion
    (let ((begin (nth 0 beg-end))
	  (end (nth 1 beg-end)))
      (goto-char begin)
      (save-restriction
	(narrow-to-region begin end)
	;; hook may add or delete lines, but the subdir boundary
	;; marker floats
	(run-hooks 'dired-after-readin-hook)
	(if mark-alist (dired-mark-remembered mark-alist))
	(dired-do-hide dired-kill-marker-char)
	(if (if elt (nth 2 elt) dired-omit-files)
	    (dired-omit-expunge nil t))))))


;;;; --------------------------------------------------------------
;;;; Dired motion commands -- moving around in the dired buffer.
;;;; --------------------------------------------------------------
	
(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (condition-case err
      (next-line arg)
    (error
     (if (eobp)
	 (error "End of buffer")
       (error "%s" err))))
  (dired-move-to-filename)
  (dired-update-mode-line))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (dired-move-to-filename)
  (dired-update-mode-line))

(defun dired-scroll-up (arg)
  "Dired version of scroll up.
Scroll text of current window upward ARG lines; or near full screen if no ARG.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (scroll-up arg)
  (dired-move-to-filename)
  (dired-update-mode-line))

(defun dired-scroll-down (arg)
  "Dired version of scroll-down.
Scroll text of current window down ARG lines; or near full screen if no ARG.
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (scroll-down arg)
  (dired-move-to-filename)
  (dired-update-mode-line))

(defun dired-beginning-of-buffer (arg)
  "Dired version of `beginning of buffer'."
  (interactive "P")
  (beginning-of-buffer arg)
  (dired-update-mode-line))

(defun dired-end-of-buffer (arg)
  "Dired version of `end-of-buffer'."
  (interactive "P")
  (end-of-buffer arg)
  (while (not (or (dired-move-to-filename) (dired-get-subdir) (bobp)))
    (forward-line -1))
  (dired-update-mode-line t))

(defun dired-next-dirline (arg &optional opoint)
  "Goto ARG'th next directory file line."
  (interactive "p")
  (if dired-re-dir
      (progn
	(dired-check-ls-l)
	(or opoint (setq opoint (point)))
	(if (if (> arg 0)
		(re-search-forward dired-re-dir nil t arg)
	      (beginning-of-line)
	      (re-search-backward dired-re-dir nil t (- arg)))
	    (progn
	      (dired-move-to-filename)		; user may type `i' or `f'
	      (dired-update-mode-line))
	  (goto-char opoint)
	  (error "No more subdirectories")))))

(defun dired-prev-dirline (arg)
  "Goto ARG'th previous directory file line."
  (interactive "p")
  (dired-next-dirline (- arg)))

(defun dired-next-marked-file (arg &optional wrap opoint)
  "Move to the next marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (or opoint (setq opoint (point))) ; return to where interactively started
  (if (if (> arg 0)
	  (re-search-forward dired-re-mark nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-mark nil t (- arg)))
      (dired-move-to-filename)
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked file"))
      (message "(Wraparound for next marked file)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (dired-next-marked-file arg nil opoint)))
  (dired-update-mode-line))

(defun dired-prev-marked-file (arg &optional wrap)
  "Move to the previous marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (dired-next-marked-file (- arg) wrap)
  (dired-update-mode-line))

(defun dired-goto-file (file)
  "Goto file line of FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute pathname.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1				; let push-mark display its message
       (list
	(let* ((dired-completer-buffer (current-buffer))
	       (dired-completer-switches dired-internal-switches)
	       (stack (reverse
		       (mapcar (function
				(lambda (x)
				  (dired-abbreviate-file-name (car x))))
			       dired-subdir-alist)))
	       (initial (car stack))
	       (dired-goto-file-history (cdr stack))
	       dired-completer-cache)
	  (expand-file-name
	   (dired-completing-read "Goto file: "
				  'dired-goto-file-completer
				  nil t initial 'dired-goto-file-history))))
     (push-mark)))
  (setq file (directory-file-name file)) ; does no harm if no directory
  (let (found case-fold-search)
    (save-excursion
      (if (dired-goto-subdir (or (file-name-directory file)
				 (error "Need absolute pathname for %s"
					file)))
	  (let* ((base (file-name-nondirectory file))
		 ;; filenames are preceded by SPC, this makes
		 ;; the search faster (e.g. for the filename "-"!).
		 (search (concat " " (dired-make-filename-string base t)))
		 (boundary (dired-subdir-max))
		 fn)
	    (while (and (not found) (search-forward search boundary 'move))
	      ;; Match could have BASE just as initial substring or
	      ;; or in permission bits or date or
	      ;; not be a proper filename at all:
	      (if (and (setq fn (dired-get-filename 'no-dir t))
		       (string-equal fn base))
		  ;; Must move to filename since an (actually
		  ;; correct) match could have been elsewhere on the
		  ;; line (e.g. "-" would match somewhere in the
		  ;; permission bits).
		  (setq found (dired-move-to-filename)))))))
    (and found
	 ;; return value of point (i.e., FOUND):
	 (prog1
	     (goto-char found)
	   (dired-update-mode-line)))))

;;; Moving by subdirectories

(defun dired-up-directory (arg)
  "Move to the ARG'th (prefix arg) parent directory of current directory.
Always stays within the current tree dired buffer.  Will insert new
subdirectories if necessary."
  (interactive "p")
  (if (< arg 0) (error "Can't go up a negative number of directories!"))
  (or (zerop arg)
      (let* ((dir (dired-current-directory))
	     (n arg)
	     (up dir))
	(while (> n 0)
	  (setq up (file-name-directory (directory-file-name up))
		n (1- n)))
	(if (and (< (length up) (length dired-directory))
		 (dired-in-this-tree dired-directory up))
	    (if (or (memq 'create-top-dir dired-no-confirm)
		    (y-or-n-p
		     (format "Insert new top dir %s and rename buffer? "
			     (dired-abbreviate-file-name up))))
		(let ((newname (let (buff)
				  (unwind-protect
				      (buffer-name
				       (setq buff
					     (create-file-buffer
					      (directory-file-name up))))
				    (kill-buffer buff))))
		      (buffer-read-only nil))
		  (push-mark)
		  (widen)
		  (goto-char (point-min))
		  (insert-before-markers "\n")
		  (forward-char -1)
		  (dired-insert-subdir-doupdate
		   up (dired-insert-subdir-doinsert up dired-internal-switches)
		   dired-internal-switches nil nil)
		  (dired-initial-position up)
		  (rename-buffer newname)
		  (dired-unadvertise default-directory)
		  (setq default-directory up
			dired-directory up)
		  (dired-advertise)))
	  (dired-maybe-insert-subdir up)))))

(defun dired-down-directory ()
  "Go down in the dired tree.
Moves to the first subdirectory of the current directory, which exists in
the dired buffer.  Does not take a prefix argument."
  ;; What would a prefix mean here?
  (interactive)
  (let ((dir (dired-current-directory)) ; has slash
	(rest (reverse dired-subdir-alist))
	pos elt)
    (while rest
      (setq elt (car rest))
      (if (dired-in-this-tree (directory-file-name (car elt)) dir)
	  (setq rest nil
		pos (dired-goto-subdir (car elt)))
	(setq rest (cdr rest))))
    (prog1
	(if pos
	    (progn
	      (push-mark)
	      (goto-char pos))
	  (error "At the bottom"))
      (dired-update-mode-line t))))

(defun dired-next-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to next subdirectory, regardless of level."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (- (length dired-subdir-alist)
		   (length (memq (assoc this-dir dired-subdir-alist)
				 dired-subdir-alist))
		   arg))
    (setq pos (if (>= index 0)
		  (dired-get-subdir-min (nth index dired-subdir-alist))))
    (if pos
	(if no-skip
	    (goto-char pos)
	  (goto-char pos)
	  (skip-chars-forward "^\r\n")
	  (if (= (following-char) ?\r)
	      (skip-chars-backward "." (- (point) 3)))
	  (dired-update-mode-line t)
	  (point))
      (if no-error-if-not-found
	  nil				; return nil if not found
	(error "%s directory" (if (> arg 0) "Last" "First"))))))

(defun dired-prev-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0))))
  (dired-next-subdir (- arg) no-error-if-not-found no-skip))

(defun dired-goto-subdir (dir)
  "Goto end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden."
  (interactive
   (prog1				; let push-mark display its message
       (list
	(let* ((table (mapcar
		       (function
			(lambda (x)
			  (list (dired-abbreviate-file-name
				 (car x)))))
		       dired-subdir-alist))
	       (stack (reverse (mapcar 'car table)))
	       (initial (car stack))
	       (dired-goto-file-history (cdr stack)))
	  (expand-file-name
	   (dired-completing-read "Goto subdirectory " table nil t
				  initial 'dired-goto-file-history))))
     (push-mark)))
  (setq dir (file-name-as-directory dir))
  (let ((elt (assoc dir dired-subdir-alist)))
    (and elt
	 ;; need to make sure that we get where we're going.
	 ;; beware: narrowing might be in effect
	 (eq (goto-char (dired-get-subdir-min elt)) (point))
	 (progn
	   ;; dired-subdir-hidden-p and dired-add-entry depend on point being
	   ;; at either \n or looking-at ...\r after this function succeeds.
	   (skip-chars-forward "^\r\n")
	   (if (= (preceding-char) ?.)
	       (skip-chars-backward "." (- (point) 3)))
	   (if (interactive-p) (dired-update-mode-line))
	   (point)))))

;;; Internals for motion commands

(defun dired-update-mode-line (&optional force)
  "Updates the mode line in dired according to the position of the point.
Normally this uses a cache of the boundaries of the current subdirectory,
but if the optional argument FORCE is non-nil, then modeline is always
updated and the cache is recomputed."
  (if (or force
	  (>= (point) dired-curr-subdir-max)
	  (< (point) dired-curr-subdir-min))
      (let ((alist dired-subdir-alist)
	    min max)
	(while (and alist (< (point)
			     (setq min (dired-get-subdir-min (car alist)))))
	  (setq alist (cdr alist)
		max min))
	(setq dired-curr-subdir-max (or max (point-max-marker))
	      dired-curr-subdir-min (or min (point-min-marker))
	      dired-subdir-omit (nth 2 (car alist)))
	(dired-sort-set-modeline (nth 3 (car alist))))))

(defun dired-manual-move-to-filename (&optional raise-error bol eol)
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  ;; This is the UNIX version.
  ;; have to be careful that we don't move to omitted files
  (let (case-fold-search)
    
    (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
    (or bol (setq bol (progn (skip-chars-backward "^\r\n") (point))))
    
    (if (or (memq ?l dired-internal-switches)
	    (memq ?g dired-internal-switches))
	(if (and
	     (> (- eol bol) 17)         ; a valid file line must have at least
					; 17 chars. 2 leading, 10 perms,
					; separator, node #, separator, owner,
					; separator
	     (goto-char (+ bol 17))
	     (re-search-forward dired-re-month-and-time eol t))
	    (point)
	  (goto-char bol)
	  (if raise-error
	      (error "No file on this line")
	    nil))
      ;; else ls switches don't contain -l.
      ;; Note that even if we make dired-move-to-filename and
      ;; dired-move-to-end-of-filename (and thus dired-get-filename)
      ;; work, all commands that gleaned information from the permission
      ;; bits (like dired-mark-directories) will cease to work properly.
      (if (= bol eol)
	  (if raise-error
	      (error "No file on this line")
	    nil)
	;; skip marker, if any
	(goto-char bol)
	(forward-char))
      ;; If we not going to use the l switch, and use nstd listings,
      ;; then we must bomb on files starting with spaces.
      (skip-chars-forward " \t")
      (point))))

(defun dired-manual-move-to-end-of-filename (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  ;; This is the UNIX version.
  (let ((bof (point))
	file-type modes-start case-fold-search)
    (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
    (or bol (setq bol (save-excursion (skip-chars-backward "^\r\n") (point))))
    (and
     (null no-error)
     selective-display
     (eq (char-after (1- bol)) ?\r)
     (cond
      ((dired-subdir-hidden-p (dired-current-directory))
       (error
	(substitute-command-keys
	 "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
      ((error
	(substitute-command-keys
	 "File line is omitted. Type \\[dired-omit-toggle] to un-omit.")))))
    (if (or (memq ?l dired-internal-switches)
	    (memq ?g dired-internal-switches))
	(if (save-excursion
	      (goto-char bol)
	      (re-search-forward
	       "[^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ][-+ 0-9+]"
	       bof t))
	    (progn
	      (setq modes-start (match-beginning 0)
		    file-type (char-after modes-start))
	      ;; Move point to end of name:
	      (if (eq file-type ?l) ; symlink
		  (progn
		    (if (search-forward " -> " eol t)
			(goto-char (match-beginning 0))
		      (goto-char eol))
		    (and dired-ls-F-marks-symlinks
			 (eq (preceding-char) ?@) ; link really marked?
			 (memq ?F dired-internal-switches)
			 (forward-char -1))
		    (point))
		;; else not a symbolic link
		(goto-char eol)
		;; ls -lF marks dirs, sockets and executables with exactly
		;; one trailing character. -F may not actually be honored, 
		;; e.g. by an FTP ls in efs
		(and
		 (memq ?F dired-internal-switches)
		 (let ((char (preceding-char)))
		   (or (and (eq char ?*) (or
					  (memq
					   (char-after (+ modes-start 3))
					   '(?x ?s ?t))
					  (memq
					   (char-after (+ modes-start 6))
					   '(?x ?s ?t))
					  (memq
					   (char-after (+ modes-start 9))
					   '(?x ?s ?t))))
		       (and (eq char ?=) (eq file-type ?s))))
		 (forward-char -1))
		;; Skip back over /'s unconditionally.  It's not a valid
		;; file name character.
		(skip-chars-backward "/")
		(point)))
	  (and (null no-error)
	       (error "No file on this line")))
      
      ;; A brief listing
      (if (eq (point) eol)
	  (and (null no-error)
	       (error "No file on this line"))
	(goto-char eol)
	(if (and (memq (preceding-char) '(?@ ?* ?=))
		 (memq ?F dired-internal-switches))
	    ;; A guess, since without a long listing, we can't be sure.
	    (forward-char -1))
	(skip-chars-backward "/")
	(point)))))

(defun dired-goto-next-nontrivial-file ()
  ;; Position point on first nontrivial file after point.
  ;; Does not move into the next sudir.
  ;; If point is on a file line, moves to that file.
  ;; This does not move to omitted files.
  (skip-chars-backward "^\n\r")
  (if (= (preceding-char) ?\r)
      (forward-line 1))
  (let ((max (dired-subdir-max))
	file)
    (while (and (or (not (setq file (dired-get-filename 'no-dir t)))
		    (string-match dired-trivial-filenames file))
		(< (point) max))
      (forward-line 1)))
  (dired-move-to-filename))

(defun dired-goto-next-file ()
  ;; Doesn't move out of current subdir. Does go to omitted files.
  ;; Returns the starting position of the file, or nil if none found.
  (let ((max (dired-subdir-max))
	found)
    (while (and (null (setq found (dired-move-to-filename))) (< (point) max))
      (skip-chars-forward "^\n\r")
      (forward-char 1))
    found))

;; fluid vars used by dired-goto-file-completer
(defvar dired-completer-buffer nil)
(defvar dired-completer-switches nil)
(defvar dired-completer-cache nil)

(defun dired-goto-file-completer (string pred action)
  (save-excursion
    (set-buffer dired-completer-buffer)
    (let* ((saved-md (match-data))
	   (file (file-name-nondirectory string))
	   (dir (file-name-directory string))
	   (xstring (expand-file-name string))
	   (xdir (file-name-directory xstring))
	   (exact (dired-goto-file xstring)))
      (unwind-protect
	  (if (dired-goto-subdir xdir)
	      (let ((table (cdr (assoc xdir dired-completer-cache)))
		    fn result max)
		(or table
		    (progn
		      (setq table (make-vector 37 0))
		      (mapcar (function
			       (lambda (ent)
				 (setq ent (directory-file-name
					    (car ent)))
				 (if (string-equal
				      (file-name-directory ent) xdir)
				     (intern
				      (concat
				       (file-name-nondirectory ent) "/")
				      table))))
			      dired-subdir-alist)
		      (or (looking-at "\\.\\.\\.\n\r")
			  (progn
			    (setq max (dired-subdir-max))
			    (while (and
				    (< (point) max)
				    (not
				     (setq fn
					   (dired-get-filename 'no-dir t))))
			      (forward-line 1))
			    (if fn
				(progn
				  (or (intern-soft (concat fn "/") table)
				      (intern fn table))
				  (forward-line 1)
				  (while (setq fn
					       (dired-get-filename 'no-dir t))
				    (or (intern-soft (concat fn "/") table)
					(intern fn table))
				    (forward-line 1))))))
		      (setq dired-completer-cache (cons
						   (cons xdir table)
						   dired-completer-cache))))
		(cond
		 ((null action)
		  (setq result (try-completion file table))
		  (if exact
		      (if (stringp result)
			  string
			t)
		    (if (stringp result)
			(concat dir result)
		      result)))
		 ((eq action t)
		  (setq result (all-completions file table))
		  (if exact (cons "." result) result))
		 ((eq 'lambda action)
		  (and (or exact (intern-soft file table)))))))
	(store-match-data saved-md)))))

(defun dired-really-goto-file (file)
  ;; Goes to a file, even if it needs to insert it parent directory.
  (or (dired-goto-file file)
      (progn				; refresh and try again
	(dired-insert-subdir (file-name-directory file))
	(dired-goto-file file))))

(defun dired-between-files ()
  ;; Point must be at beginning of line
  (save-excursion (not (dired-move-to-filename nil (point)))))

(defun dired-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  ;; Skips file lines hidden with selective display.
  ;; BACKWARDS means move backwards after each action.  This is not the same
  ;; as a negative arg, as that skips the current line.
  (beginning-of-line)
  (let* ((advance (cond ((> arg 0) 1) ((< arg 0) -1) (t nil)))
	 (check-fun (if (eq advance 1) 'eobp 'bobp))
	 (n (if (< arg 0) (- arg) arg))
	 (wall (funcall check-fun))
	 (done wall))
    (while (not done)
      (if advance
	  (progn
	    (while  (not (or (save-excursion (dired-move-to-filename))
			     (setq wall (funcall check-fun))))
	      (forward-line advance))
	    (or wall
		(progn
		  (save-excursion (funcall function))
		  (forward-line advance)
		  (while (not (or (save-excursion (dired-move-to-filename))
				  (setq wall (funcall check-fun))))
		    (forward-line advance))
		  (setq done (or (zerop (setq n (1- n))) wall)))))
	(if (save-excursion (dired-move-to-filename))
	    (save-excursion (funcall function)))
	(setq done t))))
  (dired-move-to-filename)
  ;; Note that if possible the point has now been moved to the beginning of
  ;; the file name.
  (dired-update-mode-line))


;;;; ----------------------------------------------------------------
;;;; Miscellaneous dired commands
;;;; ----------------------------------------------------------------

(defun dired-quit ()
  "Bury the current dired buffer."
  (interactive)
  (bury-buffer))

(defun dired-undo ()
  "Undo in a dired buffer.
This doesn't recover lost files, it is just normal undo with temporarily
writeable buffer.  You can use it to recover marks, killed lines or subdirs."
  (interactive)
  (let ((lines (count-lines (point-min) (point-max)))
	buffer-read-only)
    (undo)
    ;; reset dired-subdir-alist, if a dir may have been affected
    ;; Is there a better way to guess this?
    (setq lines (- (count-lines (point-min) (point-max)) lines))
    (if (or (>= lines 2) (<= lines -2))
	(dired-build-subdir-alist)))
  (dired-update-mode-line-modified t)
  (dired-update-mode-line t))


;;;; --------------------------------------------------------
;;;; Immediate actions on files: visiting, viewing, etc.
;;;; --------------------------------------------------------

(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file (dired-get-filename)))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
	(or (dired-goto-subdir file)
	    (dired file))
      (view-file file))))

(defun dired-find-file-other-window (&optional displayp)
  "In dired, visit this file or directory in another window.
With a prefix, the file is displayed, but the window is not selected."
  (interactive "P")
  (if displayp
      (dired-display-file)
    (find-file-other-window (dired-get-filename))))

;; Only for Emacs 19
(defun dired-find-file-other-frame ()
  "In dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (dired-get-filename)))

(defun dired-display-file ()
  "In dired, displays this file or directory in the other window."
  (interactive)
  (display-buffer (find-file-noselect (dired-get-filename))))

;; After an idea by wurgler@zippysun.math.uakron.edu (Tom Wurgler).
(defun dired-do-find-file (&optional arg)
  "Visit all marked files at once, and display them simultaneously.
See also function `simultaneous-find-file'.
If you want to keep the dired buffer displayed, type \\[split-window-vertically] first.
If you want just the marked files displayed and nothing else, type \\[delete-other-windows] first."
  (interactive "P")
  (dired-simultaneous-find-file (dired-get-marked-files nil arg)))

(defun dired-simultaneous-find-file (file-list)
  "Visit all files in FILE-LIST and display them simultaneously.

The current window is split across all files in FILE-LIST, as evenly
as possible.  Remaining lines go to the bottommost window.

The number of files that can be displayed this way is restricted by
the height of the current window and the variable `window-min-height'."
  ;; It is usually too clumsy to specify FILE-LIST interactively
  ;; unless via dired (dired-do-find-file).
  (let ((size (/ (window-height) (length file-list))))
    (or (<= window-min-height size)
	(error "Too many files to visit simultaneously"))
    (find-file (car file-list))
    (setq file-list (cdr file-list))
    (while file-list
      ;; Split off vertically a window of the desired size
      ;; The upper window will have SIZE lines.  We select the lower
      ;; (larger) window because we want to split that again.
      (select-window (split-window nil size))
      (find-file (car file-list))
      (setq file-list (cdr file-list)))))

(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY."
  (interactive
   (list (read-file-name "Create directory: "
			 (dired-abbreviate-file-name
			  (dired-current-directory)))))
  (let ((expanded (expand-file-name directory)))
    (make-directory expanded)
    ;; Because this function is meant to be called interactively, it moves
    ;; the point.
    (dired-goto-file expanded)))

(defun dired-recover-file ()
  "Recovers file from its autosave file.
If the file is an autosave file, then recovers its associated file instead."
  (interactive)
  (let* ((file (dired-get-filename))
	 (name (file-name-nondirectory file))
	 (asp (auto-save-file-name-p name))
	 (orig (and
		asp
		(if (fboundp 'auto-save-original-name)
		    (auto-save-original-name file)
		  (error
		   "Need auto-save package to compute original file name."))))
	 (buff (if asp
		   (and orig (get-file-buffer orig))
		 (get-file-buffer file))))
    (and
     buff
     (buffer-modified-p buff)
     (or
      (yes-or-no-p
       (format
	"Recover file will erase the modified buffer %s.  Do it? "
	(buffer-name buff)))
      (error "Recover file aborted.")))
    (if asp
	(if orig
	    (recover-file orig)
	  (find-file file))
      (recover-file file))))


;;;; --------------------------------------------------------------------
;;;; Functions for extracting and manipulating file names
;;;; --------------------------------------------------------------------

(defun dired-make-filename-string (filename &optional reverse)
  ;; Translates the way that a file name appears in a buffer, to
  ;; how it is used in a path name.  This is useful for non-unix
  ;; support in efs.
  filename)

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
  name in result.  A value of t means use path name relative to
  `default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means return nil if no filename on
  this line, otherwise an error occurs."

  ;; Compute bol & eol once, rather than twice inside move-to-filename
  ;; and move-to-end-of-filename
  (let ((eol (save-excursion (skip-chars-forward "^\n\r") (point)))
	(bol (save-excursion (skip-chars-backward "^\r\n") (point)))
	case-fold-search file p1 p2)
    (save-excursion
      (and
       (setq p1 (dired-move-to-filename (not no-error-if-not-filep) bol eol))
       (setq p2 (if (eq system-type 'windows-nt) ; ignore carriage-return at eol
		    (1- (dired-move-to-end-of-filename no-error-if-not-filep bol eol))
		  (dired-move-to-end-of-filename no-error-if-not-filep bol eol)))
       (setq file (buffer-substring p1 p2))
       ;; Check if ls quoted the names, and unquote them.
       ;; Using read to unquote is much faster than substituting
       ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
       (cond ((memq ?b dired-internal-switches) ; System V ls
	      ;; This case is about 20% slower than without -b.
	      (setq file
		    (read
		     (concat "\""
			       ;; some ls -b don't escape quotes, argh!
			     ;; This is not needed for GNU ls, though.
			     (or (dired-string-replace-match
				    "\\([^\\]\\)\"" file "\\1\\\\\"")
				 file)
			     "\""))))
	       ;; If you do this, update dired-compatible-switches-p
	     ;; ((memq ?Q dired-internal-switches) ; GNU ls
	     ;;  (setq file (read file)))
	       )))
    (and file
	 (if (eq localp 'no-dir)
	     (dired-make-filename-string file)
	   (concat (dired-current-directory localp)
		   (dired-make-filename-string file))))))

(defun dired-make-relative (file &optional dir no-error)
  ;; Convert FILE (an *absolute* pathname) to a pathname relative to DIR.
  ;; FILE must be absolute, or this function will return nonsense.
  ;; If FILE is not in a subdir of DIR, an error is signalled,
  ;; unless NO-ERROR is t. Then, ".."'s are inserted to give
  ;; a relative representation of FILE wrto DIR
  ;; eg.     FILE = /vol/tex/bin/foo DIR = /vol/local/bin/
  ;;         results in  ../../tex/bin/foo
  ;; DIR must be expanded.
  ;; DIR defaults to default-directory.
  ;; DIR must be file-name-as-directory, as with all directory args in
  ;; elisp code.
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((flen (length file))
	(dlen (length dir)))
    (if (and (> flen dlen)
	     (string-equal (substring file 0 dlen) dir))
	(substring file dlen)
      ;; Need to insert ..'s
      (or no-error (error  "%s: not in directory tree growing at %s" file dir))
      (if (string-equal file dir)
	  "./"
	(let ((index 1)
	      (count 0))
	  (while (and (string-match "/" dir index)
		      (<= (match-end 0) flen)
		      (string-equal (substring file index (match-end 0))
				    (substring dir index (match-end 0))))
	    (setq index (match-end 0)))
	  (setq file (substring file index))
	  (if (and (/= flen index)
		   (not (string-match "/" file))
		   (< flen dlen)
		   (string-equal file (substring dir index flen))
		   (= (aref dir flen) ?/))
	      (setq file "."
		    count -1))
	  ;; count how many slashes remain in dir.
	  (while (string-match "/" dir index)
	    (setq index (match-end 0)
		  count (1+ count)))
	  (apply 'concat (nconc (make-list count "../") (list file))))))))

;;; Functions for manipulating file names.
;;
;;  Used by file tranformers.
;;  Define here rather than in dired-shell.el, as it wouldn't be
;;  unreasonable to use these elsewhere.

(defun dired-file-name-base (fn)
  "Returns the base name of FN.
This is the file without directory part, and extension. See the variable
`dired-filename-re-ext'."
  (setq fn (file-name-nondirectory fn))
  (if (string-match dired-filename-re-ext fn 1)
      (substring fn 0 (match-beginning 0))
    fn))

(defun dired-file-name-extension (fn)
  "Returns the extension for file name FN.
See the variable dired-filename-re-ext'."
  (setq fn (file-name-nondirectory fn))
  (if (string-match dired-filename-re-ext fn 1)
      (substring fn (match-beginning 0))
    ""))

(defun dired-file-name-sans-rcs-extension (fn)
  "Returns the file name FN without its RCS extension \",v\"."
  (setq fn (file-name-nondirectory fn))
  (if (string-match ",v$" fn 1)
      (substring fn 0 (match-beginning 0))
    fn))

(defun dired-file-name-sans-compress-extension (fn)
  "Returns the file name FN without the extension from compress or gzip."
  (setq fn (file-name-nondirectory fn))
  (if (string-match "\\.\\([zZ]\\|gz\\)$" fn 1)
      (substring fn (match-beginning 0))
    fn))


;;;; ---------------------------------------------------------------------
;;;; Working with directory trees.
;;;; ---------------------------------------------------------------------
;;;
;;;  This where code for the dired-subdir-alist is.

;;; Utility functions for dired-subdir-alist

(defun dired-normalize-subdir (dir)
  ;; Prepend default-directory to DIR if relative path name.
  ;; dired-get-filename must be able to make a valid filename from a
  ;; file and its directory DIR.
  ;; Fully expand everything.
  (file-name-as-directory
   (if (file-name-absolute-p dir)
       (expand-file-name dir)
     (expand-file-name dir (expand-file-name default-directory)))))

(defun dired-get-subdir ()
  ;;"Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)		; alist stores b-o-l positions
      (and (zerop (- (point)
		     (dired-get-subdir-min (assoc cur-dir
						  dired-subdir-alist))))
	   cur-dir))))

(defun dired-get-subdir-max (elt)
  ;; returns subdir max.
  (let ((pos (- (length dired-subdir-alist)
		(length (member elt dired-subdir-alist)))))
    (if (zerop pos)
	(point-max)
      (1- (dired-get-subdir-min (nth (1- pos) dired-subdir-alist))))))

(defun dired-clear-alist ()
  ;; Set all markers in dired-subdir-alist to nil.  Set the alist to nil too.
  (while dired-subdir-alist
    (set-marker (dired-get-subdir-min (car dired-subdir-alist)) nil)
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist
  (setq dired-subdir-alist
	(delq (assoc dir dired-subdir-alist) dired-subdir-alist)))

(defun dired-simple-subdir-alist ()
  ;; Build and return `dired-subdir-alist' assuming just the top level
  ;; directory to be inserted.  Don't parse the buffer.
  (setq dired-subdir-alist
	(list (list (expand-file-name default-directory)
		    (point-min-marker) dired-omit-files
		    dired-internal-switches nil)))
  (if dired-verify-modtimes
      (dired-set-file-modtime (expand-file-name default-directory)
			      dired-subdir-alist)))

(defun dired-build-subdir-alist ()
  "Build `dired-subdir-alist' by parsing the buffer and return its new value."
  (interactive)
  (let ((o-alist dired-subdir-alist)
	(count 0)
	subdir)
    (dired-clear-alist)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dired-subdir-regexp nil t)
	(setq count (1+ count))
	(apply 'dired-alist-add-1
	       (setq subdir (buffer-substring (match-beginning 2)
					      (match-end 2)))
	       ;; Put subdir boundary between lines.
	       (set-marker (make-marker) (match-end 1))
	       (let ((elt (assoc subdir o-alist)))
		 (if elt
		     (list (nth 2 elt) (nth 3 elt))
		   (list dired-omit-files dired-internal-switches)))))
      (if (interactive-p)
	  (message "%d director%s." count (if (= 1 count) "y" "ies")))
      ;; We don't need to sort it because it is in buffer order per
      ;; constructionem.  Return new alist:
      ;; pointers for current-subdir may be stale
      dired-subdir-alist)))

(defun dired-alist-add (dir new-marker &optional omit switches)
  ;; Add new DIR at NEW-MARKER.  Sort alist.
  (dired-alist-add-1 dir new-marker omit switches)
  (dired-alist-sort))

(defun dired-alist-add-1 (dir new-marker &optional omit switches)
  ;; Add new DIR at NEW-MARKER.  Don't sort.
  (let ((dir (dired-normalize-subdir dir)))
    (setq dired-subdir-alist
	  (cons (list dir new-marker omit switches nil) dired-subdir-alist))
    (if dired-verify-modtimes
	(dired-set-file-modtime dir dired-subdir-alist))))

(defun dired-alist-sort ()
  ;; Keep the alist sorted on buffer position.
  (setq dired-subdir-alist
	(sort dired-subdir-alist
	      (function (lambda (elt1 elt2)
			  (> (dired-get-subdir-min elt1)
			     (dired-get-subdir-min elt2)))))))

;;; Utilities for working with subdirs in the dired buffer

;; This function is the heart of tree dired.
;; It is called for each retrieved filename.
;; It could stand to be faster, though it's mostly function call
;; overhead.  Avoiding to funcall seems to save about 10% in
;; dired-get-filename.  Make it a defsubst?
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory'.
In this it returns \"\" for the top directory."
  (let* ((here (point))
	 (dir (catch 'done
		(mapcar (function
			 (lambda (x)
			   (if (<= (dired-get-subdir-min x) here)
			       (throw 'done (car x)))))
			dired-subdir-alist))))
    (if (listp dir) (error "dired-subdir-alist seems to be mangled"))
    (if localp
	(let ((def-dir (expand-file-name default-directory)))
	  (if (string-equal dir def-dir)
	      ""
	    (dired-make-relative dir def-dir)))
      dir)))

;; Subdirs start at the beginning of their header lines and end just
;; before the beginning of the next header line (or end of buffer).

(defun dired-subdir-min ()
  ;; Returns the minimum position of the current subdir
  (save-excursion
    (if (not (dired-prev-subdir 0 t t))
	(error "Not in a subdir!")
      (point))))

(defun dired-subdir-max ()
  ;; Returns the maximum position of the current subdir
  (save-excursion
    (if (dired-next-subdir 1 t t)
	(1- (point)) ; Do not include separating empty line.
      (point-max))))


;;;; --------------------------------------------------------
;;;; Deleting files
;;;; --------------------------------------------------------

(defun dired-flag-file-deletion (arg)
  "In dired, flag the current line's file for deletion.
With prefix arg, repeat over several lines.

If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "p")
  (dired-mark arg dired-del-marker))

(defun dired-flag-file-deletion-backup (arg)
  "Flag current file for deletion, and move to previous line.
With a prefix ARG, repeats this ARG times."
  (interactive "p")
  ;; Use dired-mark-file and not dired-mark, as this function
  ;; should do nothing special on subdir headers.
  (dired-mark-file (- arg) dired-del-marker))

(defun dired-flag-subdir-files ()
  "Flag all the files in the current subdirectory for deletion."
  (interactive)
  (dired-mark-subdir-files dired-del-marker))

(defun dired-unflag (arg)
  "In dired, remove a deletion flag from the current line's file.
Optional prefix ARG says how many lines to unflag."
  (interactive "p")
  (let (buffer-read-only)
    (dired-repeat-over-lines
     arg
     (function
      (lambda ()
	(if (char-equal (following-char) dired-del-marker)
	    (progn
	      (setq dired-del-flags-number (max (1- dired-del-flags-number) 0))
	      (dired-substitute-marker (point) dired-del-marker ?\ )))))))
  (dired-update-mode-line-modified))

(defun dired-backup-unflag (arg)
  "In dired, move up lines and remove deletion flag there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (dired-unflag (- arg)))

(defun dired-update-marker-counters (char &optional remove)
  (or (memq char '(?\  ?\n ?\r))
      (let ((counter (cond
		      ((char-equal char dired-del-marker)
		       'dired-del-flags-number)
		      ((char-equal char dired-marker-char)
		       'dired-marks-number)
		  ('dired-other-marks-number))))
	(if remove
	    (set counter (max (1- (symbol-value counter)) 0))
	  (set counter (1+ (symbol-value counter)))))))

(defun dired-update-mode-line-modified (&optional check)
  ;; Updates the value of mode-line-modified in dired.
  ;; Currently assumes that it's of the form "-%%-", where % sometimes
  ;; gets replaced by %.  Should allow some sort of config flag.
  ;; SET is t to set to -DD-, nil to set to -%%-, and 'check means
  ;; examine the buffer to find out.
  (if check
      (save-excursion
	(let (char)
	  (goto-char (point-min))
	  (setq dired-del-flags-number 0
		dired-marks-number 0
		dired-other-marks-number 0)
	  (while (not (eobp))
	    (setq char (following-char))
	    (cond
	     ((char-equal char dired-del-marker)
	      (setq dired-del-flags-number (1+ dired-del-flags-number)))
	     ((char-equal char dired-marker-char)
	      (setq dired-marks-number (1+ dired-marks-number)))
	     ((memq char '(?\  ?\n ?\r))
	      nil)
	     ((setq dired-other-marks-number (1+ dired-other-marks-number))))
	    (forward-line 1)))))
  (setq mode-line-modified
	(format dired-mode-line-modified
		(if (zerop dired-del-flags-number)
		    "--"
		  (format "%d%c" dired-del-flags-number dired-del-marker))
		(if (zerop dired-marks-number)
		    "--"
		  (format "%d%c" dired-marks-number dired-marker-char))
		(if (zerop dired-other-marks-number)
		    "-"
		  (int-to-string dired-other-marks-number))))
  (set-buffer-modified-p (buffer-modified-p)))

(defun dired-do-deletions (&optional nomessage)
  (dired-expunge-deletions))

(defun dired-expunge-deletions ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let ((files (let ((dired-marker-char dired-del-marker))
		 (dired-map-over-marks (cons (dired-get-filename) (point))
				       t))))
    (if files
	(progn
	  (dired-internal-do-deletions files nil dired-del-marker)
	  ;; In case the point gets left somewhere strange -- hope that
	  ;; this doesn't cause asynch troubles later.
	  (beginning-of-line)
	  (dired-goto-next-nontrivial-file)
	  (dired-update-mode-line-modified t)) ; play safe, it's cheap
      (message "(No deletions requested)"))))

(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files."
  ;; This is more consistent with the file marking feature than
  ;; dired-expunge-deletions.
  (interactive "P")
  (dired-internal-do-deletions
   ;; this may move point if ARG is an integer
   (dired-map-over-marks (cons (dired-get-filename) (point))
		   arg)
   arg)
  (beginning-of-line)
  (dired-goto-next-nontrivial-file))

(defun dired-internal-do-deletions (l arg &optional marker-char)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute (VMS needs this for logical search paths).
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (save-excursion
    (let ((files (mapcar (function car) l))
	  (count (length l))
	  (succ 0)
	  (cdir (dired-current-directory))
	  failures)
      ;; canonicalize file list for pop up
      (setq files (nreverse (mapcar (function
				     (lambda (fn)
				       (dired-make-relative fn cdir t)))
				    files)))
      (if (or (memq 'delete dired-no-confirm)
	      (dired-mark-pop-up
	       " *Files Flagged for Deletion*" 'delete files
	       dired-deletion-confirmer
	       (format "Delete %s "
		       (dired-mark-prompt arg files marker-char))))
	  (save-excursion
	    ;; files better be in reverse order for this loop!
	    (while l
	      (goto-char (cdr (car l)))
	      (condition-case err
		  (let ((fn (car (car l))))
		    ;; This test is equivalent to
		    ;; (and (file-directory-p fn)
		    ;;      (not (file-symlink-p fn)))
		    ;; but more efficient
		    (if (if (eq t (car (file-attributes fn)))
			    (if (<= (length (directory-files fn)) 2)
				(progn (delete-directory fn) t)
			      (and (or
				    (memq 'recursive-delete dired-no-confirm)
				    (funcall
				     dired-deletion-confirmer
				     (format "\
Recursively delete directory and files within %s? "
					     (dired-make-relative fn))))
				   (progn
				     (dired-recursive-delete-directory fn)
				     t)))
			  (progn (delete-file fn) t))
			(progn
			  (setq succ (1+ succ))
			  (message "%s of %s deletions" succ count)
			  (dired-clean-up-after-deletion fn))))
		(error;; catch errors from failed deletions
		 (dired-log (buffer-name (current-buffer)) "%s\n" err)
		 (setq failures (cons (car (car l)) failures))))
	      (setq l (cdr l)))))
      (if failures
	  (dired-log-summary
	   (buffer-name (current-buffer))
	   (format "%d of %d deletion%s failed:" (length failures) count
		   (dired-plural-s count))
	   failures)
	(if (zerop succ)
	    (message "(No deletions performed)")
	  (message "%d deletion%s done" succ (dired-plural-s succ)))))))

(defun dired-recursive-delete-directory (fn)
  ;; Recursively deletes directory FN, and all of its contents.
       (let* ((fn (expand-file-name fn))
	      (handler (find-file-name-handler
			fn 'dired-recursive-delete-directory)))
	 (if handler
	     (funcall handler 'dired-recursive-delete-directory fn)
	   (progn
	     (or (file-exists-p fn)
		 (signal
		  'file-error
		  (list "Removing old file name" "no such directory" fn)))
	     ;; Which is better, -r or -R?
	     (call-process "rm" nil nil nil "-r" (directory-file-name fn))
	     (and (file-exists-p fn)
		  (error "Failed to recusively delete %s" fn))))))

(defun dired-clean-up-after-deletion (fn)
  ;; Offer to kill buffer of deleted file FN.
  (let ((buf (get-file-buffer fn)))
    (and buf
	 (or (memq 'kill-file-buffer dired-no-confirm)
	     (funcall (function yes-or-no-p)
		      (format "Kill buffer of %s, too? "
			      (file-name-nondirectory fn))))
	 (save-excursion ; you never know where kill-buffer leaves you
	   (kill-buffer buf)))))

;;; Cleaning a directory -- flagging backups for deletion

(defun dired-clean-directory (keep &optional marker msg)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  (interactive "P")
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let* ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	 (late-retention (if (<= keep 0) dired-kept-versions keep))
	 (msg (or msg
		  (format
		   "Cleaning numerical backups (keeping %d late, %d old)"
		   late-retention early-retention)))
	 (trample-marker (or marker dired-del-marker))
	 (file-version-assoc-list))
    (message "%s..." msg)
    ;; Do this after messaging, as it may take a while.
    (setq file-version-assoc-list (dired-collect-file-versions))
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions to be deleted.
    (let ((fval file-version-assoc-list))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines (function
				 (lambda (fn)
				   (dired-trample-file-versions
				    fn file-version-assoc-list
				    trample-marker))))
    (message "%s...done" msg)))

(defun dired-collect-file-versions ()
  ;; If it looks like a file has versions, return a list of the versions.
  ;; The return value is ((FILENAME . (VERSION1 VERSION2 ...)) ...)
  (let (result)
    (dired-map-dired-file-lines
     (function
      (lambda (fn)
	(let* ((base-versions
		(concat (file-name-nondirectory fn) ".~"))
	       (bv-length (length base-versions))
	       (possibilities (file-name-all-completions
			       base-versions
			       (file-name-directory fn))))
	  (if possibilities
	      (setq result (cons (cons fn
				       (mapcar 'backup-extract-version
					       possibilities)) result)))))))
    result))

(defun dired-trample-file-versions (fn alist marker)
  ;; ALIST is an alist of filenames and versions used to determine
  ;; if each file should be flagged for deletion.
  ;; This version using file-name-sans-versions is probably a lot slower
  ;; than Sebastian's original, but it is more easily adaptable to non-unix.
  (let ((base (file-name-sans-versions fn))
	base-version-list bv-length)
    (and (not (string-equal base fn))
	 (setq base-version-list (assoc base alist))
	 (setq bv-length (string-match "[0-9]" fn (length base)))
	 (not (memq (backup-extract-version fn) base-version-list))
	 (progn (skip-chars-backward "^\n\r")
		(bolp)) ; make sure the preceding char isn't \r.
	 (dired-substitute-marker (point) (following-char) marker))))

(defun dired-map-dired-file-lines (fun)
  ;; Perform FUN with point at the end of each non-directory line.
  ;; FUN takes one argument, the filename (complete pathname).
  (dired-check-ls-l)
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (and dired-re-dir (looking-at dired-re-dir)))
	       (not (memq (following-char) '(?\n ?\n)))
	       (setq file (dired-get-filename nil t)) ; nil on non-file
	       (progn (skip-chars-forward "^\n\r")
		      (funcall fun file))))
	(forward-line 1)))))            ; this guarantees that we don't
					; operate on omitted files.


;;;; -----------------------------------------------------------
;;;; Confirmations and prompting the user.
;;;; -----------------------------------------------------------

(defun dired-plural-s (count)
  (if (= 1 count) "" "s"))

(defun dired-mark-prompt (arg files &optional marker-char)
  ;; Return a string for use in a prompt, either the current file
  ;; name, or the marker and a count of marked files.
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  (cond ((zerop arg) "[no files]")
		((> arg 0) "[following]")
		((< arg 0) "[preceding]"))
	(char-to-string (or marker-char dired-marker-char))))))

(defun dired-pop-to-buffer (buf)
  ;; Pop up buffer BUF.
  ;; Make its window fit its contents.
  (let ((window (selected-window))
	target-lines w2)
    (cond ;; if split-window-threshold is enabled, use the largest window
     ((and (> (window-height (setq w2 (get-largest-window)))
	      split-height-threshold)
	   (= (frame-width) (window-width w2)))
      (setq window w2))
     ;; if the least-recently-used window is big enough, use it
     ((and (> (window-height (setq w2 (get-lru-window)))
	      (* 2 window-min-height))
	   (= (frame-width) (window-width w2)))
      (setq window w2)))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-max))
      (skip-chars-backward "\n\r\t ")
      (setq target-lines (count-lines (point-min) (point)))
      ;; Don't forget to count the last line.
      (if (not (bolp))
	  (setq target-lines (1+ target-lines))))
    (if (<= (window-height window) (* 2 window-min-height))
	;; At this point, every window on the frame is too small to split.
	(setq w2 (display-buffer buf))
      (setq w2 (split-window
		window
		(max window-min-height
		     (- (window-height window)
			(1+ (max window-min-height target-lines)))))))
    (set-window-buffer w2 buf)
    (if (< (1- (window-height w2)) target-lines)
	(progn
	  (select-window w2)
	  (enlarge-window (- target-lines (1- (window-height w2))))))
    (set-window-start w2 1)))

(defun dired-mark-pop-up (bufname op-symbol files function &rest args)
  ;; Args BUFNAME OP-SYMBOL FILES FUNCTION &rest ARGS.
  ;; Return FUNCTION's result on ARGS after popping up a window (in a buffer
  ;; named BUFNAME, nil gives \" *Marked Files*\") showing the marked
  ;; files.  Uses function `dired-pop-to-buffer' to do that.
  ;; FUNCTION should not manipulate files.
  ;; It should only read input (an argument or confirmation).
  ;; The window is not shown if there is just one file or
  ;; OP-SYMBOL is a member of the list in `dired-no-confirm'.
  ;; FILES is the list of marked files.
  (if (memq op-symbol dired-no-confirm)
      (apply function args)
    (or bufname (setq bufname  " *Marked Files*"))
    (if (<= (length files) 1)
	(apply function args)
      (save-excursion
	(let ((standard-output (set-buffer (get-buffer-create bufname))))
	  (erase-buffer)
	  (dired-format-columns-of-files files)
	  (dired-remove-text-properties (point-min) (point-max))
	  (setq mode-line-format (format "       %s  [%d files]"
					 bufname (length files)))))
      (save-window-excursion
	(dired-pop-to-buffer bufname)
	(apply function args)))))

(defun dired-column-widths (columns list &optional across)
  ;; Returns the column widths for breaking LIST into
  ;; COLUMNS number of columns.
  (cond
   ((null list)
    nil)
   ((= columns 1)
    (list (apply 'max (mapcar 'length list))))
   ((let* ((len (length list))
	   (col-length (/ len columns))
	   (remainder (% len columns))
	   (i 0)
	   (j 0)
	   (max-width 0)
	   widths padding)
      (if (zerop remainder)
	  (setq padding 0)
	(setq col-length (1+ col-length)
	      padding (- columns remainder)))
      (setq list (nconc (copy-sequence list) (make-list padding nil)))
      (setcdr (nthcdr (1- (+ len padding)) list) list)
      (while (< i columns)
	(while (< j col-length)
	  (setq max-width (max max-width (length (car list)))
		list (if across (nthcdr columns list) (cdr list))
		j (1+ j)))
	(setq widths (cons (+ max-width 2) widths)
	      max-width 0
	      j 0
	      i (1+ i))
	(if across (setq list (cdr list))))
      (setcar widths (- (car widths) 2))
      (nreverse widths)))))
  
(defun dired-calculate-columns (list &optional across)
  ;; Returns a list of integers which are the column widths that best pack
  ;; LIST, a list of strings, onto the screen.
  (and list
       (let* ((width (1- (window-width)))
	      (columns (max 1 (/ width
				 (+ 2 (apply 'max (mapcar 'length list))))))
	      col-list last-col-list)
	 (while (<= (apply '+ (setq col-list
				    (dired-column-widths columns list across)))
		    width)
	   (setq columns (1+ columns)
		 last-col-list col-list))
	 (or last-col-list col-list))))

(defun dired-format-columns-of-files (files &optional across)
  ;; Returns the number of lines used.
  ;; If ACROSS is non-nil, sorts across rather than down the buffer, like
  ;; ls -x
  (and files
       (let* ((columns (dired-calculate-columns files across))
	      (ncols (length columns))
	      (ncols1 (1- ncols))
	      (nfiles (length files))
	      (nrows (+ (/ nfiles ncols)
			(if (zerop (% nfiles ncols)) 0 1)))
	      (space-left (- (window-width) (apply '+ columns) 1))
	      (i 0)
	      (j 0)
	      file padding stretch float-stretch)
	 (if (zerop ncols1)
	     (setq stretch 0
		   float-stretch 0)
	   (setq stretch (/ space-left ncols1)
		 float-stretch (% space-left ncols1)))
	 (setq files (nconc (copy-sequence files) ; fill up with empty fns
			    (make-list (- (* ncols nrows) nfiles) "")))
	 (setcdr (nthcdr (1- (length files)) files) files) ; make circular
	 (while (< j nrows)
	   (while (< i ncols)
	     (princ (setq file (car files)))
	     (setq padding (- (nth i columns) (length file)))
	     (or (= i ncols1)
		 (progn
		   (setq padding (+ padding stretch))
		   (if (< i float-stretch) (setq padding (1+ padding)))))
	     (princ (make-string padding ?\ ))
	     (setq files (if across (cdr files) (nthcdr nrows files))
		   i (1+ i)))
	   (princ "\n")
	   (setq i 0
		 j (1+ j))
	   (or across (setq files (cdr files))))
	 nrows)))

(defun dired-query (qs-var qs-prompt &rest qs-args)
  ;; Query user and return nil or t.
  ;; Store answer in symbol VAR (which must initially be bound to nil).
  ;; Format PROMPT with ARGS.
  ;; Binding variable help-form will help the user who types C-h.
  (let* ((char (symbol-value qs-var))
	 (action (cdr (assoc char dired-query-alist))))
    (cond ((eq 'yes action)
	   t)				; accept, and don't ask again
	  ((eq 'no action)
	   nil)				; skip, and don't ask again
	  (t;; no lasting effects from last time we asked - ask now
	   (let ((qprompt (concat qs-prompt
				  (if help-form
				      (format " [yn!q or %s] "
					      (key-description
					       (char-to-string help-char)))
				    " [ynq or !] ")))
		 (dired-in-query t)
		 elt)
	     ;; Actually it looks nicer without cursor-in-echo-area - you can
	     ;; look at the dired buffer instead of at the prompt to decide.
	     (apply 'message qprompt qs-args)
	     (setq char (set qs-var (read-char)))
	     (while (not (setq elt (assoc char dired-query-alist)))
	       (message "Invalid char - type %c for help." help-char)
	       (ding)
	       (sit-for 1)
	       (apply 'message qprompt qs-args)
	       (setq char (set qs-var (read-char))))
	     (memq (cdr elt) '(t y yes)))))))

(defun dired-mark-confirm (op-symbol operation arg)
  ;; Request confirmation from the user that the operation described
  ;; by OP-SYMBOL is to be performed on the marked files.
  ;; Confirmation consists in a y-or-n question with a file list
  ;; pop-up unless OP-SYMBOL is a member of `dired-no-confirm'.
  ;; OPERATION is a string describing the operation. Used for prompting
  ;; the user.
  ;; The files used are determined by ARG (like in dired-get-marked-files).
  (or (memq op-symbol dired-no-confirm)
      (let ((files (dired-get-marked-files t arg)))
	(dired-mark-pop-up nil op-symbol files (function y-or-n-p)
			   (concat  operation " "
				    (dired-mark-prompt arg files) "? ")))))

(defun dired-mark-read-file-name (prompt dir op-symbol arg files)
  (dired-mark-pop-up
   nil op-symbol files
   (function read-file-name)
   (format prompt (dired-mark-prompt arg files)) dir))

(defun dired-mark-read-string (prompt initial op-symbol arg files
				      &optional history-sym)
  ;; Reading arguments with history.
  ;; Read arguments for a mark command of type OP-SYMBOL,
  ;; perhaps popping up the list of marked files.
  ;; ARG is the prefix arg and indicates whether the files came from
  ;; marks (ARG=nil) or a repeat factor (integerp ARG).
  ;; If the current file was used, the list has but one element and ARG
  ;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).
  ;; PROMPT for a string, with INITIAL input.
  (dired-mark-pop-up
   nil op-symbol files
   (function
    (lambda (prompt initial)
      (let ((hist (or history-sym
		      (cdr (assq op-symbol dired-op-history-alist))
		      'dired-history)))
	(dired-read-with-history prompt initial hist))))
   (format prompt (dired-mark-prompt arg files)) initial))


;;;; ----------------------------------------------------------
;;;; Marking files.
;;;; ----------------------------------------------------------

(defun dired-mark (arg &optional char)
  "Mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] to remove the mark of the current file."
  (interactive "p")
  (if (dired-get-subdir)
      (dired-mark-subdir-files char)
    (dired-mark-file arg char)))

(defun dired-mark-file (arg &optional char)
  "Mark ARG files starting from the current file line.
Optional CHAR indicates a marker character to use."
  (let (buffer-read-only)
    (if (memq (or char dired-marker-char) '(?\  ?\n ?\r))
	(error "Invalid marker character %c" dired-marker-char))
    (or char (setq char dired-marker-char))
    (dired-repeat-over-lines
     arg
     (function
      (lambda ()
	(dired-update-marker-counters (following-char) t)
	(dired-substitute-marker (point) (following-char) char)
	(dired-update-marker-counters char))))
    (dired-update-mode-line-modified)))

(defun dired-mark-subdir-files (&optional char)
  "Mark all files except `.' and `..'."
  (interactive)
  (save-excursion
    (dired-mark-files-in-region (dired-subdir-min) (dired-subdir-max) char)))

(defun dired-unmark (arg)
  "Unmark the current (or next ARG) files.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "p")
  (let (buffer-read-only)
    (dired-repeat-over-lines
     arg
     (function
      (lambda ()
	(let ((char (following-char)))
	  (or (memq char '(?\  ?\n ?\r))
	      (progn
		(cond
		 ((char-equal char dired-marker-char)
		  (setq dired-marks-number (max (1- dired-marks-number) 0)))
		 ((char-equal char dired-del-marker)
		  (setq dired-del-flags-number
			(max (1- dired-del-flags-number) 0)))
		 ((setq dired-other-marks-number
			(max (1- dired-other-marks-number) 0))))
		(dired-substitute-marker (point) char ?\ )))))))
    (dired-update-mode-line-modified)))

(defun dired-mark-prefix (&optional arg)
  "Mark the next ARG files with the next character typed.
If ARG is negative, marks the previous files."
  (interactive "p")
  (if (sit-for echo-keystrokes)
      (cond
       ((or (= arg 1) (zerop arg))
	(message "Mark with character?"))
       ((< arg 0)
	(message "Mark %d file%s moving backwards?"
		 (- arg) (dired-plural-s (- arg))))
       ((> arg 1)
	(message "Mark %d following files with character?" arg))))
  (dired-mark arg (read-char)))

(defun dired-change-marks (old new)
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark files.
With a prefix, prompts for a mark to toggle. In other words, all unmarked
files receive that mark, and all files currently marked with that mark become
unmarked."
  ;; When used in a lisp program, setting NEW to nil means toggle the mark OLD.
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old nil)
	  (new nil)
	  (markers (dired-mark-list))
	  (default (cond ((null markers)
			  (error "No markers in buffer"))
			 ((= (length markers) 1)
			  (setq old (car markers)))
			 ((memq dired-marker-char markers)
			  dired-marker-char)
			 ;; picks the last one in the buffer. reasonable?
			 ((car markers)))))
     (or old (setq old
		   (progn
		     (if current-prefix-arg
			 (message "Toggle mark (default %c): " default)
		       (message "Change old mark (default %c): " default))
		     (read-char))))
     (if (memq old '(?\  ?\n ?\r)) (setq old default))
     (or current-prefix-arg
	 (setq new (progn
		     (message
		      "Change %c marks to new mark (RET means abort): " old)
		     (read-char))))
     (list old new)))
  (let ((old-count  (cond
		     ((char-equal old dired-marker-char)
		      'dired-marks-number)
		     ((char-equal old dired-del-marker)
		      'dired-del-flags-number)
		     ('dired-other-marks-number))))
    (if new
	(or (memq new '(?\  ?\n ?\r))
	    ;; \n and \r aren't valid marker chars. Assume that if the
	    ;; user hits return, he meant to abort the command.
	    (let ((string (format "\n%c" old))
		  (new-count  (cond
			       ((char-equal new dired-marker-char)
				'dired-marks-number)
			       ((char-equal new dired-del-marker)
				'dired-del-flags-number)
			       ('dired-other-marks-number)))
		  (buffer-read-only nil))
	      (save-excursion
		(goto-char (point-min))
		(while (search-forward string nil t)
		  (if (char-equal (preceding-char) old)
		      (progn
			(dired-substitute-marker (1- (point)) old new)
			(set new-count (1+ (symbol-value new-count)))
			(set old-count (max (1- (symbol-value old-count)) 0))))
		  ))))
      (save-excursion
	(let ((ucount 0)
	      (mcount 0)
	      (buffer-read-only nil))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (or (dired-between-files)
		(looking-at dired-re-dot)
		(cond
		 ((= (following-char) ?\ )
		  (setq mcount (1+ mcount))
		  (set old-count (1+ (symbol-value old-count)))
		  (dired-substitute-marker (point) ?\  old))
		 ((= (following-char) old)
		  (setq ucount (1+ ucount))
		  (set old-count (max (1- (symbol-value old-count)) 0))
		  (dired-substitute-marker (point) old ?\ ))))
	    (forward-line 1))
	  (message "Unmarked %d file%s; marked %d file%s with %c."
		   ucount (dired-plural-s ucount) mcount
		   (dired-plural-s mcount) old)))))
  (dired-update-mode-line-modified))

(defun dired-unmark-all-files (flag &optional arg)
  "Remove a specific mark or any mark from every file.
With prefix arg, query for each marked file.
Type \\[help-command] at that time for help.
With a zero prefix, only counts the number of marks."
  (interactive
   (let* ((cursor-in-echo-area t)
	  executing-kbd-macro) ; for XEmacs
     (list (and (not (eq current-prefix-arg 0))
		(progn (message "Remove marks (RET means all): ") (read-char)))
	   current-prefix-arg)))
  (save-excursion
    (let* ((help-form "\
Type SPC or `y' to unflag one file, DEL or `n' to skip to next,
`!' to unflag all remaining files with no more questions.")
	   (allp (memq flag '(?\n ?\r)))
	   (count-p (eq arg 0))
	   (count (if (or allp count-p)
		      (mapcar
		       (function
			(lambda (elt)
			(cons elt 0)))
		       (nreverse (dired-mark-list)))
		    0))
	   (msg "")
	   (no-query (or (not arg) count-p))
	   buffer-read-only case-fold-search query)
      (goto-char (point-min))
      (if (or allp count-p)
	  (while (re-search-forward dired-re-mark nil t)
	    (if (or no-query
		    (dired-query 'query "Unmark file `%s'? "
				 (dired-get-filename t)))
		(let ((ent (assq (preceding-char) count)))
		  (if ent (setcdr ent (1+ (cdr ent))))
		  (or count-p (dired-substitute-marker
			       (- (point) 1) (preceding-char) ?\ ))))
	    (forward-line 1))
	(while (search-forward (format "\n%c" flag) nil t)
	  (if (or no-query
		  (dired-query 'query "Unmark file `%s'? "
			       (dired-get-filename t)))
	      (progn
		(dired-substitute-marker (match-beginning 0) flag ?\ )
		(setq count (1+ count))))))
      (if (or allp count-p)
	  (mapcar
	   (function
	    (lambda (elt)
	      (or (zerop (cdr elt))
		  (setq msg (format "%s%s%d %c%s"
				    msg
				    (if (zerop (length msg))
					" "
				      ", ")
				    (cdr elt)
				    (car elt)
				    (if (= 1 (cdr elt)) "" "'s"))))))
	   count)
	(or (zerop count)
	    (setq msg (format " %d %c%s"
			      count flag (if (= 1 count) "" "'s")))))
      (if (zerop (length msg))
	  (setq msg " none")
	(or count-p (dired-update-mode-line-modified t)))
      (message "%s:%s" (if count-p "Number of marks" "Marks removed") msg))))

(defun dired-get-marked-files (&optional localp arg)
  "Return the marked files' names as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute pathnames.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG forces to use other files.  If ARG is an
  integer, use the next ARG files.  If ARG is otherwise non-nil, use
  current file.  Usually ARG comes from the current prefix arg."
  (save-excursion
    (nreverse (dired-map-over-marks (dired-get-filename localp) arg))))

;;; Utility functions for marking files

(defun dired-mark-files-in-region (start end &optional char)
  (let (buffer-read-only)
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (or char (setq char dired-marker-char))
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (if (and (/= (following-char) char)
	       (not (looking-at dired-re-dot))
	       (save-excursion
		 (dired-move-to-filename nil (point))))
	  (progn
	    (dired-update-marker-counters (following-char) t)
	    (dired-substitute-marker (point) (following-char) char)
	    (dired-update-marker-counters char)))
      (forward-line 1)))
  (dired-update-mode-line-modified))

(defun dired-mark-list ()
  ;; Returns a list of all marks currently used in the buffer.
  (let ((result nil)
	char)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(and (not (memq (setq char (following-char)) '(?\  ?\n ?\r)))
	     (not (memq char result))
	     (setq result (cons char result)))
	(forward-line 1)))
    result))

;;; Dynamic markers

(defun dired-set-current-marker-string ()
  "Computes and returns `dired-marker-string'."
  (prog1
      (setq dired-marker-string
	    (if dired-marker-stack
		(let* ((n (+ (length dired-marker-stack) 5))
		       (str (make-string n ?\ ))
		       (list dired-marker-stack)
		       (pointer dired-marker-stack-pointer))
		  (setq n (1- n))
		  (aset str n ?\])
		  (setq n (1- n))
		  (while list
		    (aset str n (car list))
		    (if (zerop pointer)
			(progn
			  (setq n (1- n))
			  (aset str n dired-marker-stack-cursor)))
		    (setq n (1- n)
			  pointer (1- pointer)
			  list (cdr list)))
		  (aset str n dired-default-marker)
		  (if (zerop pointer)
		      (aset str 2 dired-marker-stack-cursor))
		  (aset str 1 ?\[)
		  str)
	      ""))
    (set-buffer-modified-p (buffer-modified-p))))

(defun dired-set-marker-char (c)
  "Set the marker character to something else.
Use \\[dired-restore-marker-char] to restore the previous value."
  (interactive "cNew marker character: ")
  (and (memq c '(?\  ?\n ?\r)) (error "invalid marker char %c" c))
  (setq dired-marker-stack (cons c dired-marker-stack)
	dired-marker-stack-pointer 0
	dired-marker-char c)
  (dired-update-mode-line-modified t)
  (dired-set-current-marker-string))

(defun dired-restore-marker-char ()
  "Restore the marker character to its previous value.
Uses `dired-default-marker' if the marker stack is empty."
  (interactive)
  (setq dired-marker-stack (cdr dired-marker-stack)
	dired-marker-char (car dired-marker-stack)
	dired-marker-stack-pointer (min dired-marker-stack-pointer
					(length dired-marker-stack)))
  (or dired-marker-char
      (setq dired-marker-char dired-default-marker))
  (dired-set-current-marker-string)
  (dired-update-mode-line-modified t)
  (or dired-marker-stack (message "Marker is %c" dired-marker-char)))

(defun dired-marker-stack-left (n)
  "Moves the marker stack cursor to the left."
  (interactive "p")
  (let ((len (1+ (length dired-marker-stack))))
    (or dired-marker-stack (error "Dired marker stack is empty."))
    (setq dired-marker-stack-pointer
	  (% (+ dired-marker-stack-pointer n) len))
    (if (< dired-marker-stack-pointer 0)
	(setq dired-marker-stack-pointer (+ dired-marker-stack-pointer
					    len)))
    (dired-set-current-marker-string)
    (setq dired-marker-char
	  (if (= dired-marker-stack-pointer (1- len))
	      dired-default-marker
	    (nth dired-marker-stack-pointer dired-marker-stack))))
  (dired-update-mode-line-modified t))

(defun dired-marker-stack-right (n)
  "Moves the marker stack cursor to the right."
  (interactive "p")
  (dired-marker-stack-left (- n)))

;;; Commands to mark or flag files based on their characteristics or names.

(defun dired-mark-symlinks (&optional unflag-p)
  "Mark all symbolic links.
With prefix argument, unflag all those files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-sym) "symbolic link"))
  (dired-update-mode-line-modified t))

(defun dired-mark-directories (&optional unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unflag all those files."
  (interactive "P")
  (if dired-re-dir
      (progn
	(dired-check-ls-l)
	(let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
	  (dired-mark-if (and (looking-at dired-re-dir)
			      (not (looking-at dired-re-dot)))
			 "directory file"))))
  (dired-update-mode-line-modified t))

(defun dired-mark-executables (&optional unflag-p)
  "Mark all executable files.
With prefix argument, unflag all those files."
  (interactive "P")
  (if dired-re-exe
      (progn
	(dired-check-ls-l)
	(let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
	  (dired-mark-if (looking-at dired-re-exe) "executable file"))))
  (dired-update-mode-line-modified t))
    
(defun dired-flag-backup-files (&optional unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unflag these files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     (and (not (and dired-re-dir (looking-at dired-re-dir)))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (backup-file-name-p fn))))
     "backup file"))
  (dired-update-mode-line-modified t))

(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unflag those files instead."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     ;; It is less than general to check for ~ here,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
			  (eq (preceding-char) ?#))
	  (not (and dired-re-dir (looking-at dired-re-dir)))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (auto-save-file-name-p
		    (file-name-nondirectory fn)))))
     "auto save file"))
  (dired-update-mode-line-modified t))

(defun dired-mark-rcs-files (&optional unflag-p)
  "Mark all files that are under RCS control.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  ;; Returns failures, or nil on success.
  ;; Finding those with locks would require to peek into the ,v file,
  ;; depends slightly on the RCS version used and should be done
  ;; together with the Emacs RCS interface.
  ;; Unfortunately, there is no definitive RCS interface yet.
  (interactive "P")
  (message "%sarking RCS controlled files..." (if unflag-p "Unm" "M"))
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char))
	rcs-files wf failures count total)
    (mapcar				; loop over subdirs
     (function
      (lambda (dir)
	(or (equal (file-name-nondirectory (directory-file-name dir))
		   "RCS")
	    ;; skip inserted RCS subdirs
	    (setq rcs-files
		  (append (directory-files dir t ",v$") ; *,v and RCS/*,v
			  (let ((rcs-dir (expand-file-name "RCS" dir)))
			    (if (file-directory-p rcs-dir)
				(mapcar	; working files from ./RCS are in ./
				 (function
				  (lambda (x)
				    (expand-file-name x dir)))
				 (directory-files
				  (file-name-as-directory rcs-dir)
				  nil ",v$"))))
			  rcs-files)))))
     (mapcar (function car) dired-subdir-alist))
    (setq total (length rcs-files))
    (while rcs-files
      (setq wf (substring (car rcs-files) 0 -2)
	    rcs-files (cdr rcs-files))
      (save-excursion (if (dired-goto-file wf)
			  (dired-mark 1) ; giving a prefix avoids checking
					 ; for subdir line.
			(setq failures (cons wf failures)))))
    (dired-update-mode-line-modified t)
    (if (null failures)
	(message "%d RCS file%s %smarked."
		 total (dired-plural-s total) (if unflag-p "un" ""))
      (setq count (length failures))
      (dired-log-summary (buffer-name (current-buffer))
			 "RCS working file not found %s" failures)
      (message "%d RCS file%s: %d %smarked - %d not found %s."
	       total (dired-plural-s total) (- total count)
	       (if unflag-p "un" "") count failures))
    failures))


;;;; ------------------------------------------------------------
;;;; Logging failures
;;;; ------------------------------------------------------------

(defun dired-why ()
  "Pop up a buffer with error log output from Dired.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer dired-log-buffer)
      (let ((owindow (selected-window))
	    (window (display-buffer (get-buffer dired-log-buffer))))
	(unwind-protect
	    (progn
	      (select-window window)
	      (goto-char (point-max))
	      (recenter -1))
	  (select-window owindow)))))

(defun dired-log (buffer-name log &rest args)
  ;; Log a message or the contents of a buffer.
  ;; BUFFER-NAME is the name of the dired buffer to which the message applies.
  ;; If LOG is a string and there are more args, it is formatted with
  ;; those ARGS.  Usually the LOG string ends with a \n.
  ;; End each bunch of errors with (dired-log t): this inserts
  ;; current time and buffer, and a \f (formfeed).
  (or (stringp buffer-name) (setq buffer-name (buffer-name buffer-name)))
  (let ((obuf (current-buffer)))
    (unwind-protect			; want to move point
	(progn
	  (set-buffer (get-buffer-create dired-log-buffer))
	  (goto-char (point-max))
	  (let (buffer-read-only)
	    (cond ((stringp log)
		   (insert (if args
			       (apply (function format) log args)
			     log)))
		  ((bufferp log)
		   (insert-buffer log))
		  ((eq t log)
		   (insert "\n\t" (current-time-string)
			   "\tBuffer `" buffer-name "'\n\f\n")))))
      (set-buffer obuf))))

(defun dired-log-summary (buffer-name string failures)
  (message (if failures "%s--type y for details %s"
	     "%s--type y for details")
	   string failures)
  ;; Log a summary describing a bunch of errors.
  (dired-log buffer-name (concat "\n" string))
  (if failures (dired-log buffer-name "\n%s" failures))
  (dired-log buffer-name t))


;;;; -------------------------------------------------------
;;;; Sort mode of dired buffers.
;;;; -------------------------------------------------------

(defun dired-sort-type (list)
  ;; Returns the sort type of LIST, as a symbol.
  (let* ((list (reverse list))
	 (alist (sort
		 (mapcar (function
			  (lambda (x)
			    (cons (length (memq (car x) list)) (cdr x))))
			 dired-sort-type-alist)
		 (function
		  (lambda (x y)
		    (> (car x) (car y))))))
	 (winner (car alist)))
    (if (zerop (car winner))
	'name
      (cdr winner))))

(defun dired-sort-set-modeline (&optional switches)
  ;; Set modeline display according to dired-internal-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (or switches (setq switches dired-internal-switches))
  (setq dired-sort-mode
	(if dired-show-ls-switches
	    (concat " " (dired-make-switches-string
			 (or switches dired-internal-switches)))
	  (concat " by " (and (memq ?r switches) "rev-")
		  (symbol-name (dired-sort-type switches)))))
  ;; update mode line
  (set-buffer-modified-p (buffer-modified-p)))

(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle between sort by date/name for the current subdirectory.

With a 0 prefix argument, simply reports on the current switches.

With a prefix 1 allows the ls switches for the current subdirectory to be 
edited.

With a prefix 2 allows the default ls switches for newly inserted
subdirectories to be edited. 

With a prefix \\[universal-argument] allows you to sort the entire
buffer by either name or date.

With a prefix \\[universal-argument] \\[universal-argument] allows the default switches
for the entire buffer to be edited, and then reverts the buffer so that all
subdirectories are sorted according to these switches.

Note that although dired allows different ls switches to be used for 
different subdirectories, certain combinations of ls switches are incompatible.
If incompatible switches are detected, dired will offer to revert the buffer
to force the ls switches for all subdirectories to a single value.  If you
refuse to revert the buffer, any change of ls switches will be aborted."
  (interactive "P")
  (cond
   ((eq arg 0)
    ;; Report on switches
    (message "Switches for current subdir: %s.  Default for buffer: %s."
	     (dired-make-switches-string
	      (nth 3 (assoc (dired-current-directory) dired-subdir-alist)))
	      (dired-make-switches-string dired-internal-switches)))
   ((null arg)
    ;; Toggle between sort by date/name.
    (let* ((dir (dired-current-directory))
	   (curr (nth 3 (assoc dir dired-subdir-alist))))
      (dired-sort-other
       (if (eq (dired-sort-type curr) 'name)
	   (cons ?t curr)
	 (mapcar (function
		  (lambda (x)
		    (setq curr
			  (delq (car x) curr))))
		 dired-sort-type-alist)
	 curr)
       nil dir)))
   ((eq arg 1)
    ;; Edit switches for current subdir.
    (let* ((dir (dired-current-directory))
	   (switch-string
	    (read-string
	     "New ls switches for current subdir (must contain -l): "
	     (dired-make-switches-string
	      (nth 3 (assoc dir dired-subdir-alist)))))
	   (switches (dired-make-switches-list switch-string)))
      (if (dired-compatible-switches-p switches dired-internal-switches)
	  (dired-sort-other switches nil dir)
	(if (or
	     (memq 'sort-revert dired-no-confirm)
	     (y-or-n-p
	      (format
	       "Switches %s incompatible with default %s.  Revert buffer? "
	         switch-string
		 (dired-make-switches-string dired-internal-switches))))
	    (dired-sort-other switches nil nil)
	  (error "Switches unchanged.  Remain as %s." switch-string)))))
   ((eq arg 2)
    ;; Set new defaults for subdirs inserted in the future.
    (let* ((switch-string
	    (read-string
	     "Default ls switches for new subdirs (must contain -l): "
	     (dired-make-switches-string dired-internal-switches)))
	   (switches (dired-make-switches-list switch-string))
	   (alist dired-subdir-alist)
	   x bad-switches)
      (while alist
	(setq x (nth 3 (car alist))
	      alist (cdr alist))
	(or (dired-compatible-switches-p x switches)
	    (member x bad-switches)
	    (setq bad-switches (cons x bad-switches))))
      (if bad-switches
	  (if (or (memq 'sort-revert dired-no-confirm)
		  (y-or-n-p
		   (format
		    "Switches %s incompatible with %s.  Revert buffer? "
		    switch-string (mapconcat 'dired-make-switches-string
					      bad-switches ", "))))
	      (dired-sort-other switches nil nil)
	    (error "Default switches unchanged.  Remain as %s."
		   (dired-make-switches-string dired-internal-switches)))
	(dired-sort-other switches t nil))))
   ((or (equal arg '(4)) (eq arg 'date) (eq arg 'name))
    ;; Toggle the entire buffer name/data.
    (let ((cursor-in-echo-area t)
	  (switches (copy-sequence dired-internal-switches))
	  (type (and (symbolp arg) arg))
	  char)
      (while (null type)
	(message "Sort entire buffer according to (n)ame or (d)ate? ")
	(setq char (read-char)
	      type (cond
		    ((char-equal char ?d) 'date)
		    ((char-equal char ?n) 'name)
		    (t (message "Type one of n or d.") (sit-for 1) nil))))
      (mapcar (function
	       (lambda (x)
		 (setq switches
		       (delq (car x) switches))))
	      dired-sort-type-alist)
      (dired-sort-other
       (if (eq type 'date) (cons ?t switches) switches) nil nil)))
   ((equal arg '(16))
    ;; Edit the switches for the entire buffer.
    (dired-sort-other
     (dired-make-switches-list
      (read-string
       "Change ls switches for entire buffer to (must contain -l): "
       (dired-make-switches-string dired-internal-switches)))
     nil nil))
   (t
    ;; No idea what's going on.
    (error
     "Invalid prefix.  See %s dired-sort-toggle-or-edit."
     (substitute-command-keys
      (if (featurep 'ehelp)
	  "\\[electric-describe-function]"
	"\\[describe-function]"))))))

(defun dired-sort-other (switches &optional no-revert subdir)
  ;; Specify new ls SWITCHES for current dired buffer.
  ;; With optional second arg NO-REVERT, don't refresh the listing afterwards.
  ;; If subdir is non-nil, only changes the switches for the
  ;; sudirectory.
  (if subdir
      (let ((elt (assoc subdir dired-subdir-alist)))
	(if elt (setcar (nthcdr 3 elt) switches)))
    (setq dired-internal-switches switches))
  (or no-revert
      (cond
       
       (subdir
	(let ((ofile (dired-get-filename nil t))
	      (opoint (point)))
	  (message "Relisting %s..." subdir)
	  (dired-insert-subdir subdir switches)
	  (message "Relisting %s... done" subdir)
	  (or (and ofile (dired-goto-file ofile)) (goto-char opoint))))
       
       ((memq ?R switches)
	;; We are replacing a buffer with a giant recursive listing.
	(let ((opoint (point))
	      (ofile (dired-get-filename nil t))
	      (hidden-subdirs (dired-remember-hidden))
	      (mark-alist (dired-remember-marks (point-min) (point-max)))
	      (kill-files-p (save-excursion
			      (goto-char (point))
			      (search-forward
			       (concat (char-to-string ?\r)
				       (regexp-quote
					(char-to-string
					 dired-kill-marker-char)))
			       nil t)))
	      (omit-files (nth 2 (nth (1- (length dired-subdir-alist))
				      dired-subdir-alist)))
	      buffer-read-only)
	  (dired-readin dired-directory (current-buffer)
			(or (consp dired-directory)
			    (null (file-directory-p dired-directory))))
	  (dired-mark-remembered mark-alist)	; mark files that were marked
	  (if kill-files-p (dired-do-hide dired-kill-marker-char))
	  (if omit-files
	      (dired-omit-expunge nil t))
	  ;; hide subdirs that were hidden
	  (save-excursion
	    (mapcar (function (lambda (dir)
				(if (dired-goto-subdir dir)
				    (dired-hide-subdir 1))))
		    hidden-subdirs))
	  ;; Try to get back to where we were
	  (or (and ofile (dired-goto-file ofile))
	      (goto-char opoint))
	  (dired-move-to-filename)))
       
       (t
	;; Clear all switches in the subdir alist
	(setq dired-subdir-alist
	      (mapcar (function
		       (lambda (x)
			 (setcar (nthcdr 3 x) nil)
			 x))
		      dired-subdir-alist))
	(revert-buffer nil t))))
  (dired-update-mode-line t))

(defun dired-compatible-switches-p (list1 list2)
  ;; Returns t if list1 and list2 are allowed as switches in the same
  ;; dired buffer.
  (and (eq (null (or (memq ?l list1) (memq ?o list1) (memq ?g list1)))
	   (null (or (memq ?l list2) (memq ?o list2) (memq ?g list2))))
       (eq (null (memq ?F list1)) (null (memq ?F list2)))
       (eq (null (memq ?p list1)) (null (memq ?p list2)))
       (eq (null (memq ?b list1)) (null (memq ?b list2)))))

(defun dired-check-ls-l (&optional switches)
  ;; Check for long-style listings
  (let ((switches (or switches dired-internal-switches)))
    (or (memq ?l switches) (memq ?o switches) (memq ?g switches)
	(error "Dired needs -l, -o, or -g in ls switches"))))


;;;; --------------------------------------------------------------
;;;; Creating new files.
;;;; --------------------------------------------------------------
;;;
;;;  The dired-create-files paradigm is used for copying, renaming,
;;;  compressing, and making hard and soft links.

(defun dired-file-marker (file)
  ;; Return FILE's marker, or nil if unmarked.
  (save-excursion
    (and (dired-goto-file file)
	 (progn
	   (skip-chars-backward "^\n\r")
	   (and (not (= ?\040 (following-char)))
		(following-char))))))

;; The basic function for half a dozen variations on cp/mv/ln/ln -s.
(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char query
					implicit-to)
  ;; Create a new file for each from a list of existing files.  The user
  ;; is queried, dired buffers are updated, and at the end a success or
  ;; failure message is displayed
  
  ;; FILE-CREATOR must accept three args: oldfile newfile ok-if-already-exists
  ;; It is called for each file and must create newfile, the entry of
  ;; which will be added.  The user will be queried if the file already
  ;; exists.  If oldfile is removed by FILE-CREATOR (i.e, it is a
  ;; rename), it is FILE-CREATOR's responsibility to update dired
  ;; buffers.  FILE-CREATOR must abort by signalling a file-error if it
  ;; could not create newfile.  The error is caught and logged.
  
  ;; OPERATION (a capitalized string, e.g. `Copy') describes the
  ;; operation performed.  It is used for error logging.
  
  ;; FN-LIST is the list of files to copy (full absolute pathnames).
  
  ;; NAME-CONSTRUCTOR returns a newfile for every oldfile, or nil to
  ;; skip.  If it skips files, it is supposed to tell why (using dired-log).

  ;; Optional MARKER-CHAR is a character with which to mark every
  ;; newfile's entry, or t to use the current marker character if the
  ;; oldfile was marked.

  ;; QUERY is a function to use to prompt the user about creating a file.
  ;; It accepts two  args, the from and to files,
  ;; and must return nil or t. If QUERY is nil, then no user
  ;; confirmation will be requested.

  ;; If IMPLICIT-TO is non-nil, then the file constructor does not take
  ;; a to-file arg. e.g. compress.

  (let ((success-count 0)
	(total (length fn-list))
	failures skipped overwrite-query)
    ;; Fluid vars used for storing responses of previous queries must be
    ;; initialized.
    (dired-save-excursion
      (setq dired-overwrite-backup-query nil
	    dired-file-creator-query nil)
      (mapcar
       (function
	(lambda (from)
	  (let ((to (funcall name-constructor from)))
	    (if to
		(if (equal to from)
		    (progn
		      (dired-log (buffer-name (current-buffer))
				 "Cannot %s to same file: %s\n"
				 (downcase operation) from)
		      (setq skipped (cons (dired-make-relative from) skipped)))
		  (if (or (null query)
			  (funcall query from to))
		      (let* ((overwrite (let (jka-compr-enabled)
					  ;; Don't let jka-compr fool us.
					  (file-exists-p to)))
			     ;; for dired-handle-overwrite
			     (dired-overwrite-confirmed 
			      (and overwrite
				   (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
				     (dired-query 'overwrite-query
						  "Overwrite %s?" to))))
			     ;; must determine if FROM is marked before
			     ;; file-creator gets a chance to delete it
			     ;; (in case of a move).
			     (actual-marker-char
			      (cond  ((integerp marker-char) marker-char)
				     (marker-char (dired-file-marker from))
				     (t nil))))
			(if (and overwrite (null dired-overwrite-confirmed))
			    (setq skipped (cons (dired-make-relative from)
						skipped))
			  (condition-case err
			      (let ((dired-unhandle-add-files
				     (cons to dired-unhandle-add-files)))
				(if implicit-to
				    (funcall file-creator from
					     dired-overwrite-confirmed)
				  (funcall file-creator from to
					   dired-overwrite-confirmed))
				(setq success-count (1+ success-count))
				(message "%s: %d of %d"
					 operation success-count total)
				(dired-add-file to actual-marker-char))
			    (file-error		; FILE-CREATOR aborted
			     (progn
			       (setq failures (cons (dired-make-relative from)
						    failures))
			       (dired-log (buffer-name (current-buffer))
					  "%s `%s' to `%s' failed:\n%s\n"
					  operation from to err))))))
		    (setq skipped (cons (dired-make-relative from) skipped))))
	      (setq skipped (cons (dired-make-relative from) skipped))))))
       fn-list)
      (cond
       (failures
	(dired-log-summary
	 (buffer-name (current-buffer))
	 (format "%s failed for %d of %d file%s"
		 operation (length failures) total
		 (dired-plural-s total)) failures))
       (skipped
	(dired-log-summary
	 (buffer-name (current-buffer))
	 (format "%s: %d of %d file%s skipped"
		 operation (length skipped) total
		 (dired-plural-s total)) skipped))
       (t
	(message "%s: %s file%s."
		 operation success-count (dired-plural-s success-count)))))))
  
(defun dired-do-create-files (op-symbol file-creator operation arg
					     &optional marker-char
					     prompter how-to)
  ;; Create a new file for each marked file.
  ;; Prompts user for target, which is a directory in which to create
  ;;   the new files.  Target may be a plain file if only one marked
  ;;   file exists.
  ;; OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  ;;   will determine wether pop-ups are appropriate for this OP-SYMBOL.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; PROMPTER is a function of one-arg, the list of files, to return a prompt
  ;;     to use for dired-read-file-name. If it is nil, then a default prompt
  ;;     will be used.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  ;; Optional arg HOW-TO determines how to treat target:
  ;;   If HOW-TO is not given (or nil), and target is a directory, the
  ;;     file(s) are created inside the target directory.  If target
  ;;     is not a directory, there must be exactly one marked file,
  ;;     else error.
  ;;   If HOW-TO is t, then target is not modified.  There must be
  ;;     exactly one marked file, else error.
  ;; Else HOW-TO is assumed to be a function of one argument, target,
  ;;     that looks at target and returns a value for the into-dir
  ;;     variable.  The function dired-into-dir-with-symlinks is provided
  ;;     for the case (common when creating symlinks) that symbolic
  ;;     links to directories are not to be considered as directories
  ;;     (as file-directory-p would if HOW-TO had been nil).

  (let* ((fn-list (dired-get-marked-files nil arg))
	 (fn-count (length fn-list))
	 (cdir (dired-current-directory))
	 (target (expand-file-name
		   (dired-mark-read-file-name
		    (if prompter
			(funcall prompter fn-list)
		      (concat operation " %s to: "))
		    (dired-dwim-target-directory)
		    op-symbol arg (mapcar (function
					   (lambda (fn)
					     (dired-make-relative fn cdir t)))
					  fn-list))))
	 (into-dir (cond ((null how-to) (file-directory-p target))
			 ((eq how-to t) nil)
			 (t (funcall how-to target)))))
    (if (and (> fn-count 1)
	     (not into-dir))
	(error "Marked %s: target must be a directory: %s" operation target))
    ;; rename-file bombs when moving directories unless we do this:
    (or into-dir (setq target (directory-file-name target)))
    (dired-create-files
     file-creator operation fn-list
     (if into-dir			; target is a directory
	 (list 'lambda '(from)
	       (list 'expand-file-name '(file-name-nondirectory from) target))
       (list 'lambda '(from) target))
     marker-char)))

(defun dired-into-dir-with-symlinks (target)
  (and (file-directory-p target)
       (not (file-symlink-p target))))
;; This may not always be what you want, especially if target is your
;; home directory and it happens to be a symbolic link, as is often the
;; case with NFS and automounters.  Or if you want to make symlinks
;; into directories that themselves are only symlinks, also quite
;; common.
;; So we don't use this function as value for HOW-TO in
;; dired-do-symlink, which has the minor disadvantage of
;; making links *into* a symlinked-dir, when you really wanted to
;; *overwrite* that symlink.  In that (rare, I guess) case, you'll
;; just have to remove that symlink by hand before making your marked
;; symlinks.

(defun dired-handle-overwrite (to)
  ;; Save old version of a to be overwritten file TO.
  ;; `dired-overwrite-confirmed' and `dired-overwrite-backup-query'
  ;; are fluid vars from dired-create-files.
  (if (and dired-backup-if-overwrite
	   dired-overwrite-confirmed
	   (or (eq 'always dired-backup-if-overwrite)
	       (dired-query 'dired-overwrite-backup-query
			(format "Make backup for existing file `%s'? " to))))
      (let ((backup (car (find-backup-file-name to))))
	(rename-file to backup 0))))	; confirm overwrite of old backup

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in the next window, use
  ;; its current subdir, else use current subdir of this dired buffer.
  ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
  (let* ((this-dir (and (eq major-mode 'dired-mode)
			(dired-current-directory)))
	 (dwimmed
	  (if dired-dwim-target
	      (let* ((other-buf (window-buffer (next-window)))
		     (other-dir (save-excursion
				  (set-buffer other-buf)
				  (and (eq major-mode 'dired-mode)
				       (dired-current-directory)))))
		(or other-dir this-dir))
	    this-dir)))
    (and dwimmed (dired-abbreviate-file-name dwimmed))))

(defun dired-get-target-directory ()
  "Writes a copy of the current subdirectory into an active minibuffer."
  (interactive)
  (let ((mb (dired-get-active-minibuffer-window)))
    (if mb
	(let ((dir (dired-current-directory)))
	  (select-window mb)
	  (set-buffer (window-buffer mb))
	  (erase-buffer)
	  (insert dir))
      (error "No active minibuffer"))))

;;; Copying files

(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and the files are copied into that directory, retaining the same file names.

A zero prefix argument copies nothing.  But it toggles the
variable `dired-copy-preserve-time' (which see)."
  (interactive "P")
  (if (not (zerop (prefix-numeric-value arg)))
      (dired-do-create-files 'copy (function dired-copy-file)
			       (if dired-copy-preserve-time "Copy [-p]" "Copy")
			       arg dired-keep-marker-copy)
    (setq dired-copy-preserve-time (not dired-copy-preserve-time))
    (if dired-copy-preserve-time
	(message "Copy will preserve time.")
      (message "Copied files will get current date."))))

(defun dired-copy-file (from to ok-flag)
  (dired-handle-overwrite to)
  (copy-file from to ok-flag dired-copy-preserve-time))

;;; Renaming/moving files

(defun dired-do-rename (&optional arg)
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory.

A zero ARG moves no files but toggles `dired-dwim-target' (which see)."
  (interactive "P")
  (if (not (zerop (prefix-numeric-value arg)))
      (dired-do-create-files 'move (function dired-rename-file)
			       "Move" arg dired-keep-marker-rename
			       (function
				(lambda (list)
				  (if (= (length list) 1)
				      "Rename %s to: "
				    "Move %s to: "))))
    (setq dired-dwim-target (not dired-dwim-target))
    (message "dired-dwim-target is %s." (if dired-dwim-target "ON" "OFF"))))

(defun dired-rename-file (from to ok-flag)
  (dired-handle-overwrite to)
  (let ((insert (assoc (file-name-as-directory from) dired-subdir-alist)))
    (rename-file from to ok-flag) ; error is caught in -create-files
    ;; Silently rename the visited file of any buffer visiting this file.
    (dired-rename-update-buffers from to insert)))

(defun dired-rename-update-buffers (from to &optional insert)
  (if (get-file-buffer from)
      (save-excursion
	(set-buffer (get-file-buffer from))
	(let ((modflag (buffer-modified-p)))
	  (set-visited-file-name to)	; kills write-file-hooks
	  (set-buffer-modified-p modflag)))
    ;; It's a directory.  More work to do.
    (let ((blist (buffer-list))
	  (from-dir (file-name-as-directory from))
	  (to-dir (file-name-as-directory to)))
      (save-excursion
	(while blist
	  (set-buffer (car blist))
	  (setq blist (cdr blist))
	  (cond
	   (buffer-file-name
	    (if (dired-in-this-tree buffer-file-name from-dir)
		(let ((modflag (buffer-modified-p)))
		  (unwind-protect
		      (set-visited-file-name
		       (concat to-dir (substring buffer-file-name
						 (length from-dir))))
		    (set-buffer-modified-p modflag)))))
	   (dired-directory
	    (if (string-equal from-dir (expand-file-name default-directory))
		;; If top level directory was renamed, lots of things
		;; have to be updated.
		(progn
		  (dired-unadvertise from-dir)
		  (setq default-directory to-dir
			dired-directory
			;; Need to beware of wildcards.
			(expand-file-name 
			 (file-name-nondirectory dired-directory)
			 to-dir))
		  (let ((new-name (file-name-nondirectory
				   (directory-file-name dired-directory))))
		    ;; Try to rename buffer, but just leave old name if new
		    ;; name would already exist (don't try appending "<%d>")
		    ;; Why?  --sandy 19-8-94
		    (or (get-buffer new-name)
			(rename-buffer new-name)))
		  (dired-advertise))
	      (and insert
		   (assoc (file-name-directory (directory-file-name to))
			  dired-subdir-alist)
		   (dired-insert-subdir to))))))))))

;;; Making symbolic links

(defun dired-do-symlink (&optional arg)
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'symlink (function make-symbolic-link)
			   "SymLink" arg dired-keep-marker-symlink))

;; Relative symlinks:
;; make-symbolic no longer expands targets (as of at least 18.57),
;; so the code to call ln has been removed.

(defun dired-do-relsymlink (&optional arg)
  "Symlink all marked (or next ARG) files into a directory,
or make a symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/path/that/may/change/any/day/bar/foo"
  (interactive "P")
  (dired-do-create-files 'relsymlink (function dired-make-relative-symlink)
			 "RelSymLink" arg dired-keep-marker-symlink))

(defun dired-make-relative-symlink (target linkname
					   &optional ok-if-already-exists)
  "Make a relative symbolic link pointing to TARGET with name LINKNAME.
Three arguments: FILE1 FILE2 &optional OK-IF-ALREADY-EXISTS
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\""
  (interactive
   (let ((target (read-string "Make relative symbolic link to file: ")))
     (list
      target
      (read-file-name (format "Make relsymlink to file %s: " target))
      0)))
  (let* ((target (expand-file-name target))
	 (linkname (expand-file-name linkname))
	 (handler (or (find-file-name-handler
		       linkname 'dired-make-relative-symlink)
		      (find-file-name-handler
		       target 'dired-make-relative-symlink))))
    (if handler
	(funcall handler 'dired-make-relative-symlink target linkname
		 ok-if-already-exists)
      (setq target (directory-file-name target)
	    linkname (directory-file-name linkname))
      (make-symbolic-link
       (dired-make-relative target (file-name-directory linkname) t)
       linkname ok-if-already-exists))))

;;; Hard links -- adding names to files

(defun dired-do-hardlink (&optional arg)
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'hardlink (function add-name-to-file)
			   "HardLink" arg dired-keep-marker-hardlink))


;;;; ---------------------------------------------------------------
;;;; Running process on marked files
;;;; ---------------------------------------------------------------
;;;
;;;  Commands for shell processes are in dired-shell.el.

;;; Internal functions for running subprocesses,
;;; checking and logging of their errors.

(defun dired-call-process (program discard &rest arguments)
  ;; Run PROGRAM with output to current buffer unless DISCARD is t.
  ;; Remaining arguments are strings passed as command arguments to PROGRAM.
  ;; Returns program's exit status, as an integer.
  ;; This is a separate function so that efs can redefine it.
  (let ((return
	 (apply 'call-process program nil (not discard) nil arguments)))
    (if (and (not (equal shell-file-name program))
	     (integerp return))
	return
      ;; Fudge return code by looking for errors in current buffer.
      (if (zerop (buffer-size)) 0 1))))
	  
(defun dired-check-process (msg program &rest arguments)
  ;; Display MSG while running PROGRAM, and check for output.
  ;; Remaining arguments are strings passed as command arguments to PROGRAM.
  ;; On error, insert output in a log buffer and return the
  ;; offending ARGUMENTS or PROGRAM.
  ;; Caller can cons up a list of failed args.
  ;; Else returns nil for success.
  (let ((err-buffer (get-buffer-create " *dired-check-process output*"))
	(dir default-directory))
    (message "%s..." msg)
    (save-excursion
      ;; Get a clean buffer for error output:
      (set-buffer err-buffer)
      (erase-buffer)
      (setq default-directory dir)	; caller's default-directory
      (if (not
	   (eq 0 (apply (function dired-call-process) program nil arguments)))
	  (progn
	    (dired-log (buffer-name (current-buffer))
		       (concat program " " (prin1-to-string arguments) "\n"))
	    (dired-log (buffer-name (current-buffer)) err-buffer)
	    (or arguments program t))
	(kill-buffer err-buffer)
	(message "%s...done" msg)
	nil))))

;;; Changing file attributes

(defun dired-do-chxxx (attribute-name program op-symbol arg)
  ;; Change file attributes (mode, group, owner) of marked files and
  ;; refresh their file lines.
  ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
  ;; PROGRAM is the program used to change the attribute.
  ;; OP-SYMBOL is the type of operation (for use in dired-mark-pop-up).
  ;; ARG describes which files to use, like in dired-get-marked-files.
  (let* ((files (dired-get-marked-files t arg))
	 (new-attribute
	  (dired-mark-read-string
	   (concat "Change " attribute-name " of %s to: ")
	   nil op-symbol arg files))
	 (operation (concat program " " new-attribute))
	 (failures
	  (dired-bunch-files 10000 (function dired-check-process)
			     (list operation program new-attribute)
			     files)))
    (dired-do-redisplay arg);; moves point if ARG is an integer
    (if failures
	(dired-log-summary (buffer-name (current-buffer))
			   (format "%s: error" operation) nil))))

(defun dired-do-chmod (&optional arg)
  "Change the mode of the marked (or next ARG) files.
This calls chmod, thus symbolic modes like `g+w' are allowed."
  (interactive "P")
  (dired-do-chxxx "Mode" "chmod" 'chmod arg))

(defun dired-do-chgrp (&optional arg)
  "Change the group of the marked (or next ARG) files."
  (interactive "P")
  (dired-do-chxxx "Group" "chgrp" 'chgrp arg))

(defun dired-do-chown (&optional arg)
  "Change the owner of the marked (or next ARG) files."
  (interactive "P")
  (dired-do-chxxx "Owner" dired-chown-program 'chown arg))

;;; Utilities for running processes on marked files.

;; Process all the files in FILES in batches of a convenient size,
;; by means of (FUNCALL FUNCTION ARGS... SOME-FILES...).
;; Batches are chosen to need less than MAX chars for the file names,
;; allowing 3 extra characters of separator per file name.
(defun dired-bunch-files (max function args files)
  (let (pending
	(pending-length 0)
	failures)
    ;; Accumulate files as long as they fit in MAX chars,
    ;; then process the ones accumulated so far.
    (while files
      (let* ((thisfile (car files))
	     (thislength (+ (length thisfile) 3))
	     (rest (cdr files)))
	;; If we have at least 1 pending file
	;; and this file won't fit in the length limit, process now.
	(if (and pending (> (+ thislength pending-length) max))
	    (setq failures
		  (nconc (apply function (append args pending))
			 failures)
		  pending nil
		  pending-length 0))
	;; Do (setq pending (cons thisfile pending))
	;; but reuse the cons that was in `files'.
	(setcdr files pending)
	(setq pending files)
	(setq pending-length (+ thislength pending-length))
	(setq files rest)))
    (nconc (apply function (append args pending))
	   failures)))


;;;; ---------------------------------------------------------------
;;;; Calculating data or properties for marked files.
;;;; ---------------------------------------------------------------

(defun dired-do-total-size (&optional arg)
  "Show total size of all marked (or next ARG) files."
  (interactive "P")
  (let* ((result (dired-map-over-marks (dired-get-file-size) arg))
	 (total (apply (function +) result))
	 (num (length result)))
    (message "%d bytes (%d kB) in %s file%s"
	     total (/ total 1024) num (dired-plural-s num))
    total))

(defun dired-get-file-size ()
  ;; Returns the file size in bytes of the current file, as an integer.
  ;; Assumes that it is on a valid file line. It's the caller's responsibility
  ;; to ensure this. Assumes that match 0 for dired-re-month-and-time is
  ;; at the end of the file size.
  (dired-move-to-filename t)
  ;; dired-move-to-filename must leave match-beginning 0 at the start of
  ;; the date.
  (goto-char (match-beginning 0))
  (skip-chars-backward " ")
  (string-to-int (buffer-substring (point)
				   (progn (skip-chars-backward "0-9")
					  (point)))))

(defun dired-copy-filenames-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space, and may be copied into other buffers
with \\[yank].  The list of names is also stored in the variable 
`dired-marked-files' for possible manipulation in the *scratch* buffer.

With a 0 prefix argument, use the pathname relative to the top-level dired
directory for each marked file.

With a prefix \\[universal-argument], use the complete pathname of each 
marked file.

With a prefix \\[universal-argument] \\[universal-argument], copy the complete
file line.  In this case, the lines are separated by newlines.

If on a subdirectory headerline and no prefix argument given, use the
subdirectory name instead."
  (interactive "P")
  (let (res)
    (cond
     ((and (null arg) (setq res (dired-get-subdir)))
      (kill-new res)
      (message "Copied %s into kill ring." res))
     ((equal arg '(16))
      (setq dired-marked-files
	    (dired-map-over-marks
	     (concat " " ; Don't copy the mark.
		     (buffer-substring
		      (progn (beginning-of-line) (1+ (point)))
		      (progn (skip-chars-forward "^\n\r") (point))))
		     nil))
      (let ((len (length dired-marked-files)))
	(kill-new (concat
		   (mapconcat 'identity dired-marked-files "\n")
		   "\n"))
	(message "Copied %d file line%s into kill ring."
		 len (dired-plural-s len))))
     (t
      (setq dired-marked-files
	    (cond
	     ((null arg)
	      (dired-get-marked-files 'no-dir))
	     ((eq arg 0)
	      (dired-get-marked-files t))
	     ((integerp arg)
	      (dired-get-marked-files 'no-dir arg))
	     ((equal arg '(4))
	      (dired-get-marked-files))
	     (t (error "Invalid prefix %s" arg))))
      (let ((len (length dired-marked-files)))
	(kill-new (mapconcat 'identity dired-marked-files " "))
	(message "Copied %d file name%s into kill ring."
		 len (dired-plural-s len)))))))


;;;; -----------------------------------------------------------
;;;; Killing subdirectories
;;;; -----------------------------------------------------------
;;;
;;;  These commands actually remove text from the dired buffer.

(defun dired-kill-subdir (&optional remember-marks tree)
  "Remove all lines of current subdirectory.
Lower levels are unaffected.  If given a prefix when called interactively,
kills the entire directory tree below the current subdirectory."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive (list nil current-prefix-arg))
  (let ((cur-dir (dired-current-directory)))
    (if (string-equal cur-dir (expand-file-name default-directory))
	(error "Attempt to kill top level directory"))
    (if tree
	(dired-kill-tree cur-dir remember-marks)
      (let ((beg (dired-subdir-min))
	    (end (dired-subdir-max))
	    buffer-read-only)
	(prog1
	    (if remember-marks (dired-remember-marks beg end))
	  (goto-char beg)
	  (or (bobp) (forward-char -1)) ; gobble separator
	  (delete-region (point) end)
	  (dired-unsubdir cur-dir)
	  (dired-update-mode-line)
	  (dired-update-mode-line-modified t))))))

(defun dired-kill-tree (dirname &optional remember-marks)
  "Kill all proper subdirs of DIRNAME, excluding DIRNAME itself.
With optional arg REMEMBER-MARKS, return an alist of marked files."
  (interactive "DKill tree below directory: ")
  (let ((s-alist dired-subdir-alist) dir m-alist)
    (while s-alist
      (setq dir (car (car s-alist))
	    s-alist (cdr s-alist))
      (if (and (not (string-equal dir dirname))
	       (dired-in-this-tree dir dirname)
	       (dired-goto-subdir dir))
	  (setq m-alist (nconc (dired-kill-subdir remember-marks) m-alist))))
    (dired-update-mode-line)
    (dired-update-mode-line-modified t)
    m-alist))


;;;; ------------------------------------------------------------
;;;; Killing file lines
;;;; ------------------------------------------------------------
;;;
;;;  Uses selective diplay, rather than removing lines from the buffer.

(defun dired-do-kill-file-lines (&optional arg)
  "Kill all marked file lines, or those indicated by the prefix argument.
Killing file lines means hiding them with selective display.  Giving
a zero prefix redisplays all killed file lines."
  (interactive "P")
  (or selective-display
      (error "selective-display must be t for file line killing to work!"))
  (if (eq arg 0)
      (dired-do-unhide dired-kill-marker-char
		       "Successfully resuscitated %d file line%s."
		       dired-keep-marker-kill)
    (let ((files
	   (length
	    (dired-map-over-marks
	     (progn
	       (beginning-of-line)
	       (subst-char-in-region (1- (point)) (point) ?\n ?\r)
	       (dired-substitute-marker (point) (following-char)
					dired-kill-marker-char)
	       (dired-update-marker-counters dired-marker-char t)
	       t)
	     arg))))
      ;; Beware of extreme apparent save-excursion lossage here.
      (let ((opoint (point)))
	(skip-chars-backward "^\n\r")
	(if (= (preceding-char) ?\n)
	    (goto-char opoint)
	  (setq opoint (- opoint (point)))
	  (beginning-of-line)
	  (skip-chars-forward "^\n\r" (+ (point) opoint))))
      (dired-update-mode-line-modified)
      (message "Killed %d file line%s." files (dired-plural-s files)))))


;;;; ----------------------------------------------------------------
;;;; Omitting files.
;;;; ----------------------------------------------------------------

;; Marked files are never omitted.
;; Adapted from code submitted by:
;; Michael D. Ernst, mernst@theory.lcs.mit.edu, 1/11/91
;; Changed to work with selective display by Sandy Rutherford, 13/12/92.
;; For historical reasons, we still use the term expunge, although nothing
;; is expunged from the buffer.

(defun dired-omit-toggle (&optional arg)
  "Toggle between displaying and omitting files matching
`dired-omit-regexps' in the current subdirectory.
With a positive prefix, omits files in the entire tree dired buffer.
With a negative prefix, forces all files in the tree dired buffer to be
displayed."
  (interactive "P")
  (if arg
      (let ((arg (prefix-numeric-value arg)))
	(if (>= arg 0)
	    (dired-omit-expunge nil t)
	  (dired-do-unhide dired-omit-marker-char "")
	  (mapcar
	   (function
	    (lambda (elt)
	      (setcar (nthcdr 2 elt) nil)))
	   dired-subdir-alist)))
    (if (dired-current-subdir-omitted-p)
	(save-restriction
	  (narrow-to-region (dired-subdir-min) (dired-subdir-max))
	  (dired-do-unhide dired-omit-marker-char "")
	  (setcar (nthcdr 2 (assoc
			     (dired-current-directory) dired-subdir-alist))
		  nil)
	  (setq dired-subdir-omit nil))
      (dired-omit-expunge)
      (setq dired-subdir-omit t)))
  (dired-update-mode-line t))

(defun dired-current-subdir-omitted-p ()
  ;; Returns t if the current subdirectory is omited.
  (nth 2 (assoc (dired-current-directory) dired-subdir-alist)))

(defun dired-remember-omitted ()
  ;; Returns a list of omitted subdirs.
  (let ((alist dired-subdir-alist)
	result elt)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (if (nth 2 elt)
	  (setq result (cons (car elt) result))))
    result))

(defun dired-omit-expunge (&optional regexp full-buffer)
  ;; Hides all unmarked files matching REGEXP.
  ;; If REGEXP is nil or not specified, uses `dired-omit-regexps',
  ;; and also omits filenames ending in `dired-omit-extensions'.
  ;; If REGEXP is the empty string, this function is a no-op.
  (let ((omit-re (or regexp (dired-omit-regexp)))
	(alist dired-subdir-alist)
	elt min)
    (if (null omit-re)
	0
      (if full-buffer
	  (prog1
	      (dired-omit-region (point-min) (point-max) omit-re)
	    ;; Set omit property in dired-subdir-alist
	    (while alist
	      (setq elt (car alist)
		    min (dired-get-subdir-min elt)
		    alist (cdr alist))
	      (if (and (<= (point-min) min) (>= (point-max) min))
		  (setcar (nthcdr 2 elt) t))))
	(prog1
	    (dired-omit-region (dired-subdir-min) (dired-subdir-max) omit-re)
	  (setcar
	   (nthcdr 2 (assoc (dired-current-directory)
			    dired-subdir-alist))
	   t))))))

(defun dired-omit-region (start end regexp)
  ;; Omits files matching regexp in region. Returns count.
  (save-restriction
    (narrow-to-region start end)
    (let ((hidden-subdirs (dired-remember-hidden))
	  buffer-read-only count)
      (or selective-display
	  (error "selective-display must be t for file omission to work!"))
      (dired-omit-unhide-region start end)
      (let ((dired-marker-char dired-omit-marker-char)
	    ;; since all subdirs are now unhidden, this fakes
	    ;; dired-move-to-end-of-filename into working faster
	    (selective-display nil))
	(or dired-omit-silent
	    dired-in-query (message "Omitting..."))
	(if (dired-mark-unmarked-files regexp nil nil 'no-dir)
	    (setq count (dired-do-hide
			 dired-marker-char
			 (and (memq dired-omit-silent '(nil 0))
			      (not dired-in-query)
			      "Omitted %d line%s.")))
	  (or dired-omit-silent dired-in-query
	      (message "(Nothing to omit)"))))
      (save-excursion		;hide subdirs that were hidden
	(mapcar (function (lambda (dir)
			    (if (dired-goto-subdir dir)
				(dired-hide-subdir 1))))
		hidden-subdirs))
      count)))

(defun dired-omit-unhide-region (beg end)
  ;; Unhides hidden, but not marked files in the region.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(and (char-equal (following-char) ?\ )
	     (subst-char-in-region (1- (point)) (point) ?\r ?\n))))))

(defun dired-do-unhide (char &optional fmt marker)
  ;; Unhides files marked with CHAR. Optional FMT is a message
  ;; to be displayed. Note that after unhiding, we will need to re-hide
  ;; files belonging to hidden subdirs.
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
	  (string (concat "\r" (char-to-string char)))
	  (hidden-subdirs (dired-remember-hidden))
	  (new (if marker (concat "\n" (char-to-string marker)) "\n "))
	  buffer-read-only)
      (while (search-forward string nil t)
	(replace-match new)
	(setq count (1+ count)))
      (or (equal "" fmt)
	  (message (or fmt "Unhid %d line%s.") count (dired-plural-s count)))
      (goto-char (point-min))
      (mapcar (function (lambda (dir)
			  (if (dired-goto-subdir dir)
			      (dired-hide-subdir 1 t))))
	      hidden-subdirs)
      (if marker (dired-update-mode-line-modified t))
      count)))

(defun dired-do-hide (char &optional fmt)
  ;; Hides files marked with CHAR. Otional FMT is a message
  ;; to be displayed. FMT is a format string taking args the number
  ;; of hidden file lines, and dired-plural-s.
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
	  (string (concat "\n" (char-to-string char)))
	  buffer-read-only)
      (while (search-forward string nil t)
	(subst-char-in-region (match-beginning 0)
			      (1+ (match-beginning 0)) ?\n ?\r t)
	(setq count (1+ count)))
      (if fmt
	  (message fmt count (dired-plural-s count)))
      count)))

(defun dired-omit-regexp ()
  (let (rgxp)
    (if dired-omit-extensions
	(setq rgxp (concat
		    ".\\("
		    (mapconcat 'regexp-quote dired-omit-extensions "\\|")
		    "\\)$")))
    (if dired-omit-regexps
	(setq rgxp
	      (concat
	       rgxp
	       (and rgxp "\\|")
	       (mapconcat 'identity dired-omit-regexps "\\|"))))
    rgxp))

(defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp)
  ;; Marks unmarked files matching REGEXP, displaying MSG.
  ;; REGEXP is matched against the complete pathname, unless localp is
  ;; specified.
  ;; Does not re-mark files which already have a mark.
  ;; Returns t if any work was done, nil otherwise.
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char))
	fn)
    (dired-mark-if
     (and
      ;; not already marked
      (eq (following-char) ?\ )
      ;; uninteresting
      (setq fn (dired-get-filename localp t))
      (string-match regexp fn))
     msg)))

(defun dired-add-omit-regexp (rgxp &optional how)
  "Adds a new regular expression to the list of omit regular expresions.
With a non-zero numeric prefix argument, deletes a regular expresion from 
the list.

With a prefix argument \\[universal-argument], adds a new extension to
the list of file name extensions omitted.
With a prefix argument \\[universal-argument] \\[universal-argument], deletes
a file name extension from the list.

With a prefix 0, reports on the current omit regular expressions and 
extensions."
  (interactive
   (list
    (cond
     ((null current-prefix-arg)
      (read-string "New omit regular expression: "))
     ((equal '(4) current-prefix-arg)
      (read-string "New omit extension (\".\" is not implicit): "))
     ((equal '(16) current-prefix-arg)
      (completing-read
       "Remove from omit extensions (type SPACE for options): "
       (mapcar 'list dired-omit-extensions) nil t))
     ((eq 0 current-prefix-arg)
      nil)
     (t
      (completing-read
       "Remove from omit regexps (type SPACE for options): "
       (mapcar 'list dired-omit-regexps) nil t)))
    current-prefix-arg))
  (let (remove)
    (cond
     ((null how)
      (if (member rgxp dired-omit-regexps)
	  (progn
	    (describe-variable 'dired-omit-regexps)
	    (error "%s is already included in the list." rgxp))
	(setq dired-omit-regexps (cons rgxp dired-omit-regexps))))
     ((equal how '(4))
      (if (member rgxp dired-omit-extensions)
	  (progn
	    (describe-variable 'dired-omit-extensions)
	    (error "%s is already included in list." rgxp))
	(setq dired-omit-extensions (cons rgxp dired-omit-extensions))))
     ((equal how '(16))
      (let ((tail (member rgxp dired-omit-extensions)))
	(if tail
	    (setq dired-omit-extensions
		  (delq (car tail) dired-omit-extensions)
		  remove t)
	  (setq remove 'ignore))))
     ((eq 0 how)
      (setq remove 'ignore)
      (if (featurep 'ehelp)
	  (with-electric-help
	   (function
	    (lambda ()
	      (princ "Omit extensions (dired-omit-extensions <V>):\n")
	      (dired-format-columns-of-files dired-omit-extensions)
	      (princ "\n")
	      (princ "Omit regular expressions (dired-omit-regexps <V>):\n")
	      (dired-format-columns-of-files dired-omit-regexps)
	      nil)))
	(with-output-to-temp-buffer "*Help*"
	  (princ "Omit extensions (dired-omit-extensions <V>):\n")
	  (dired-format-columns-of-files dired-omit-extensions)
	  (princ "\n")
	  (princ "Omit regular expressions (dired-omit-regexps <V>):\n")
	  (dired-format-columns-of-files dired-omit-regexps)
	  (print-help-return-message))))
     (t
      (let ((tail (member rgxp dired-omit-regexps)))
	(if tail
	    (setq dired-omit-regexps (delq (car tail) dired-omit-regexps)
		  remove t)
	  (setq remove 'ignore)))))
    (or (eq remove 'ignore)
	(save-excursion
	  (mapcar
	   (function
	    (lambda (dir)
	      (if (dired-goto-subdir dir)
		  (progn
		    (if remove
			(save-restriction
			  (narrow-to-region
			   (dired-subdir-min) (dired-subdir-max))
			  (dired-do-unhide dired-omit-marker-char "")))
		    (dired-omit-expunge)))))
	   (dired-remember-omitted))))))



;;;; ----------------------------------------------------------------
;;;; Directory hiding.
;;;; ----------------------------------------------------------------
;;;
;;; To indicate a hidden subdir, we actually insert "..." in the buffer.
;;; Aside from giving the look of ellipses (even though
;;; selective-display-ellipses is nil), it allows us to tell the difference
;;; between a dir with a single omitted file, and a hidden subdir with one
;;; file.

(defun dired-subdir-hidden-p (dir)
  (save-excursion
    (and selective-display
	 (dired-goto-subdir dir)
	 (looking-at "\\.\\.\\.\r"))))

(defun dired-unhide-subdir ()
  (let (buffer-read-only)
    (goto-char (dired-subdir-min))
    (skip-chars-forward "^\n\r")
    (skip-chars-backward "." (- (point) 3))
    (if (looking-at "\\.\\.\\.\r") (delete-char 4))
    (dired-omit-unhide-region (point) (dired-subdir-max))))

(defun dired-hide-check ()
  (or selective-display
      (error "selective-display must be t for subdir hiding to work!")))

(defun dired-hide-subdir (arg &optional really)
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories.
With the optional argument REALLY, we always hide 
the subdir, regardless of dired-subdir-hidden-p."
  ;; The arg REALLY is needed because when we unhide
  ;; omitted files in a hidden subdir, we want to
  ;; re-hide the subdir, regardless of whether dired
  ;; thinks it's already hidden.
  (interactive "p")
  (dired-hide-check)
  (dired-save-excursion
    (while (>=  (setq arg (1- arg)) 0)
      (let* ((cur-dir (dired-current-directory))
	     (hidden-p (and (null really)
			    (dired-subdir-hidden-p cur-dir)))
	   (elt (assoc cur-dir dired-subdir-alist))
	   (end-pos (1- (dired-get-subdir-max elt)))
	   buffer-read-only)
	;; keep header line visible, hide rest
	(goto-char (dired-get-subdir-min elt))
	(skip-chars-forward "^\n\r")
	(skip-chars-backward "." (- (point) 3))
      (if hidden-p
	  (progn
	    (if (looking-at "\\.\\.\\.\r")
		(progn
		  (delete-char 3)
		  (setq end-pos (- end-pos 3))))
	    (dired-omit-unhide-region (point) end-pos))
	(if (looking-at "\\.\\.\\.\r")
	    (goto-char (match-end 0))
	  (insert "...")
	  (setq end-pos (+ end-pos 3)))
	(subst-char-in-region (point) end-pos ?\n ?\r)))
      (dired-next-subdir 1 t))))
  
(defun dired-hide-all (arg)
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  (interactive "P")
  (dired-hide-check)
  (let (buffer-read-only)
    (dired-save-excursion
      (if (let ((alist dired-subdir-alist)
		(hidden nil))
	    (while (and alist (null hidden))
	      (setq hidden (dired-subdir-hidden-p (car (car alist)))
		    alist (cdr alist)))
	    hidden)
	  ;; unhide
	  (let ((alist dired-subdir-alist))
	    (while alist
	      (goto-char (dired-get-subdir-min (car alist)))
	      (skip-chars-forward "^\n\r")
	      (delete-region (point) (progn (skip-chars-backward ".") (point)))
	      (setq alist (cdr alist)))
	    (dired-omit-unhide-region (point-min) (point-max)))
	;; hide
	(let ((alist dired-subdir-alist))
	  (while alist
	    (dired-goto-subdir (car (car alist)))
	    (dired-hide-subdir 1 t)
	    (setq alist (cdr alist))))))))


;;;; -----------------------------------------------------------------
;;;; Automatic dired buffer maintenance.
;;;; -----------------------------------------------------------------
;;;
;;;  Keeping Dired buffers in sync with the filesystem and with each
;;;  other.
;;;  When used with efs on remote directories, buffer maintainence is
;;;  done asynch.

(defun dired-buffers-for-dir (dir-or-list &optional check-wildcard)
;; Return a list of buffers that dired DIR-OR-LIST
;; (top level or in-situ subdir).
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers. If DIR-OR-LIST is a wildcard or list, returns any
;; dired buffers for which DIR-OR-LIST is equal to `dired-directory'.
;; If check-wildcard is non-nil, only returns buffers which contain dir-or-list
;; exactly, including the wildcard part.
  (let ((alist dired-buffers)
	(as-dir (and (stringp dir-or-list)
		     (file-name-as-directory dir-or-list)))
	result buff elt)
    (while alist
      (setq buff (cdr (setq elt (car alist)))
	    alist (cdr alist))
      ;; dired-in-this-tree is not fast. It doesn't pay to use this to check
      ;; whether the buffer is a good candidate.
      (if (buffer-name buff)
	  (save-excursion
	    (set-buffer buff)
	    (if (or (equal dir-or-list dired-directory) ; the wildcard case.
		    (and as-dir
			 (not (and check-wildcard
				   (string-equal
				    as-dir
				    (expand-file-name default-directory))))
			 (assoc as-dir dired-subdir-alist)))
		(setq result (cons buff result))))
	;; else buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers))))
    (or dired-buffers (dired-remove-from-file-name-handler-alist))
    result))

(defun dired-advertise ()
  ;; Advertise in variable `dired-buffers' that we dired `default-directory'.
  ;; With wildcards we actually advertise too much.
  ;; Also makes sure that we are installed in the file-name-handler-alist
  (prog1
      (let ((ddir (expand-file-name default-directory)))
	(if (memq (current-buffer) (dired-buffers-for-dir ddir))
	    t			    ; we have already advertised ourselves
	  (setq dired-buffers
		(cons (cons ddir (current-buffer))
		      dired-buffers))))
    ;; Do this last, otherwise the call to dired-buffers-for-dir will
    ;; remove dired-handler-fn from the file-name-handler-alist.
    ;; Strictly speaking, we only need to do this in th else branch of
    ;; the if statement.  We do it unconditionally as a sanity check.
    (dired-check-file-name-handler-alist)))

(defun dired-unadvertise (dir)
  ;; Remove DIR from the buffer alist in variable dired-buffers.
  ;; This has the effect of removing any buffer whose main directory is DIR.
  ;; It does not affect buffers in which DIR is a subdir.
  ;; Removing is also done as a side-effect in dired-buffer-for-dir.
  (setq dired-buffers
      (delq (assoc dir dired-buffers) dired-buffers))
  ;; If there are no more dired buffers, we are no longer needed in the
  ;; file-name-handler-alist.
  (or dired-buffers (dired-remove-from-file-name-handler-alist)))

(defun dired-unadvertise-current-buffer ()
  ;; Remove all references to the current buffer in dired-buffers.
  (setq dired-buffers
	(delq nil
	      (mapcar
	       (function
		(lambda (x)
		  (and (not (eq (current-buffer) (cdr x))) x)))
	       dired-buffers)))
  ;; If there are no more dired buffers, we are no longer needed in the
  ;; file-name-handler-alist.
  (or dired-buffers (dired-remove-from-file-name-handler-alist)))

(defun dired-fun-in-all-buffers (directory fun &rest args)
  ;; In all buffers dired'ing DIRECTORY, run FUN with ARGS.
  ;; Return list of buffers where FUN succeeded (i.e., returned non-nil).
  (let* ((buf-list (dired-buffers-for-dir directory))
	 (obuf (current-buffer))
	 (owin (selected-window))
	 (win owin)
	 buf windows success-list)
    (if buf-list
	(unwind-protect
	    (progn
	      (while (not (eq (setq win (next-window win)) owin))
		(and (memq (setq buf (window-buffer win)) buf-list)
		     (progn
		       (set-buffer buf)
		       (= (point) (window-point win)))
		     (setq windows (cons win windows))))
	      (while buf-list
		(setq buf (car buf-list)
		      buf-list (cdr buf-list))
		(set-buffer buf)
		(if (apply fun args)
		    (setq success-list (cons (buffer-name buf) success-list))))
	      ;; dired-save-excursion prevents lossage of save-excursion
	      ;; for point.  However, if dired buffers are displayed in
	      ;; other windows, the setting of window-point loses, and
	      ;; drags the point with it.  This should fix this.
	      (while windows
		(condition-case nil
		    (progn
		      (set-buffer (window-buffer (setq win (car windows))))
		      (set-window-point win (point)))
		  (error nil))
		(setq windows (cdr windows))))
	  (set-buffer obuf)))
    success-list))

(defun dired-find-file-place (subdir file)
  ;; Finds a position to insert in SUBDIR FILE. If it can't find SUBDIR,
  ;; returns nil.
  (let ((sort (dired-sort-type dired-internal-switches))
	(rev (memq ?r (nth 3 (assoc subdir dired-subdir-alist)))))
    (cond
     ((eq sort 'name)
      (if (dired-goto-subdir subdir)
	  (let ((max (dired-subdir-max))
		start end found)
	    (if (dired-goto-next-file)
		(progn
		  (skip-chars-forward "^\n\r")
		  (setq start (point))
		  (goto-char (setq end max))
		  (forward-char -1)
		  (skip-chars-backward "^\n\r")
		  ;; This loop must find a file.  At the very least, it will
		  ;; find the one found previously.
		  (while (not found)
		    (if (save-excursion (dired-move-to-filename nil (point)))
			(setq found t)
		      (setq end (point))
		      (forward-char -1)
		      (skip-chars-backward "^\n\r")))
		  (if rev
		      (while (< start end)
			(goto-char (/ (+ start end) 2))
			(if (dired-file-name-lessp
			     (or (dired-get-filename 'no-dir t)
				 (error
				  "Error in dired-find-file-place"))
			     file)
			    (setq end (progn
					(skip-chars-backward "^\n\r")
					(point)))
			  (setq start (progn
					(skip-chars-forward "^\n\r")
					(forward-char 1)
					(skip-chars-forward "^\n\r")
					(point)))))
		    (while (< start end)
		      (goto-char (/ (+ start end) 2))
		      (if (dired-file-name-lessp
			   file
			   (or (dired-get-filename 'no-dir t)
			       (error
				"Error in dired-find-file-place")))
			  (setq end (progn
					(skip-chars-backward "^\n\r")
					(point)))
			(setq start (progn
				      (skip-chars-forward "^\n\r")
				      (forward-char 1)
				      (skip-chars-forward "^\n\r")
				      (point))))))
		  (goto-char end))
	      (goto-char max))
	    t)))
     ((eq sort 'date)
      (if (dired-goto-subdir subdir)
	  (if rev
	      (goto-char (dired-subdir-max))
	    (dired-goto-next-file)
	    t)))
     ;; Put in support for other sorting types.
     (t
      (if (string-equal (dired-current-directory) subdir)
	  (progn
	    ;; We are already where we should be, except when
	    ;; point is before the subdir line or its total line.
	    (or (save-excursion (beginning-of-line) (dired-move-to-filename))
		(dired-goto-next-nontrivial-file)) ; in the header somewhere
	    t) ; return t, for found.
	(if (dired-goto-subdir subdir)
	    (progn
	      (dired-goto-next-nontrivial-file)
	      t)))))))

(defun dired-add-entry (filename &optional marker-char inplace)
  ;; Add a new entry for FILENAME, optionally marking it
  ;; with MARKER-CHAR (a character, else dired-marker-char is used).
  ;; Hidden subdirs are exposed if a file is added there.
  ;; 
  ;; This function now adds the new entry at the END of the previous line,
  ;; not the beginning of the current line.
  ;; Logically, we now think of the `newline' associated
  ;; with a fileline, as the one at the beginning of the line, not the end.
  ;; This makes it easier to keep track of omitted files.
  ;; 
  ;; Uses dired-save-excursion, so that it doesn't move the
  ;; point around. Especially important when it runs asynch.
  ;; 
  ;; If there is already an entry, delete the existing one before adding a
  ;; new one.  In this case, doesn't remember its mark.  Use
  ;; dired-update-file-line for that.
  ;; 
  ;; If INPLACE eq 'relist, then the new entry is put in the
  ;; same place as the old, if there was an old entry.
  ;; If INPLACE is t, then the file entry is put on the line
  ;; currently containing the point.  Otherwise, dired-find-file-place
  ;; attempts to determine where to put the file.

  (setq filename (directory-file-name filename))
  (dired-save-excursion
    (let ((oentry (save-excursion (dired-goto-file filename)))
	  (directory (file-name-directory filename))
	  (file-nodir (file-name-nondirectory filename))
	  buffer-read-only)
      (if oentry
	  ;; Remove old entry
	  (let ((opoint (point)))
	    (goto-char oentry)
	    (delete-region (save-excursion
			     (skip-chars-backward "^\r\n")
			     (dired-update-marker-counters (following-char) t)
			     (1- (point)))
			   (progn
			     (skip-chars-forward "^\r\n")
			     (point)))
	    ;; Move to right place to replace deleted line.
	    (cond ((eq inplace 'relist) (forward-char 1))
		  ((eq inplace t) (goto-char opoint)))
	    (dired-update-mode-line-modified)))
      (if (or (eq inplace t)
	      (and oentry (eq inplace 'relist))
	      ;; Tries to move the point to the right place.
	      ;; Returns t on success.
	      (dired-find-file-place directory file-nodir))
	  (let ((switches (dired-make-switches-string
			   (cons ?d dired-internal-switches)))
		b-of-l)
	    ;; Bind marker-char now, in case we are working asynch and
	    ;; dired-marker-char changes in the meantime.
	    (if (and marker-char (not (integerp marker-char)))
		(setq marker-char dired-marker-char))
	    ;; since we insert at the end of a line,
	    ;; backup to the end of the previous line.
	    (skip-chars-backward "^\n\r")
	    (forward-char -1)
	    (setq b-of-l (point))
	    (if (and (featurep 'efs-dired) efs-dired-host-type)
		;; insert asynch
		;; we call the efs version explicitly here,
		;; rather than let the handler-alist work for us
		;; because we want to pass extra args.
		;; Is there a cleaner way to do this?
		(efs-insert-directory filename ; don't expand `.' !
				      switches nil nil
				      t ; nowait
				      marker-char)
	      (let ((insert-directory-program dired-ls-program))
		(insert-directory filename switches nil nil))
	      (dired-after-add-entry b-of-l marker-char))
	    (if dired-verify-modtimes
		(dired-set-file-modtime directory dired-subdir-alist))
	    t))))) ; return t on success, else nil.

(defun dired-after-add-entry (start marker-char)
  ;; Does the cleanup of a dired entry after listing it.
  ;; START is the start of the new listing-line.
  ;; This is a separate function for the sake of efs.
  (save-excursion
    (goto-char start)
    ;; we make sure that the new line is bracketted by new-lines
    ;; so the user doesn't need to use voodoo in the
    ;; after-readin-hook.
    (insert ?\n)
    (dired-add-entry-do-indentation marker-char)
    (let* ((beg (dired-manual-move-to-filename t))
	   ;; error for strange output
	   (end (dired-manual-move-to-end-of-filename))
	   (filename (buffer-substring beg end)))
      ;; We want to have the non-directory part only.
      (delete-region beg end)
      ;; Any markers pointing to the beginning of the filename, will
      ;; still point there after this insertion. Should keep
      ;; save-excursion from losing.
      (setq beg (point))
      (insert (file-name-nondirectory filename))
      (dired-insert-set-properties beg (point))
      (dired-move-to-filename))
    ;; The subdir-alist is not affected so we can run it right now.
    (let ((omit (dired-current-subdir-omitted-p))
	  (hide (dired-subdir-hidden-p (dired-current-directory))))
      (if (or dired-after-readin-hook omit hide)
	  (save-excursion
	    (save-restriction
	      ;; Use start so that we get the new-line at
	      ;; the beginning of the line in case we want
	      ;; to hide the file. Don't need to test (bobp)
	      ;; here, since we never add a file at
	      ;; the beginning of the buffer.
	      (narrow-to-region start
				(save-excursion (forward-line 1) (point)))
	      (run-hooks 'dired-after-readin-hook)
	      (if omit
		  (let ((dired-omit-silent (or dired-omit-silent 0)))
		    (dired-omit-region (point-min) (point-max)
				       (dired-omit-regexp))))
	      (if hide
		  (subst-char-in-region (point-min) (1- (point-max))
					?\n ?\r))))))
    ;; clobber the extra newline at the end of the line
    (end-of-line)
    (delete-char 1)))

;; This is a separate function for the sake of nested dired format.
(defun dired-add-entry-do-indentation (marker-char)
  ;; two spaces or a marker plus a space:
  (insert (if marker-char
	      (let ((char (if (integerp marker-char)
			      marker-char
			    dired-marker-char)))
		(dired-update-marker-counters char)
		(dired-update-mode-line-modified)
		char)
	    ?\040)
	  ?\040))

(defun dired-remove-file (file)
  (let ((alist dired-buffers)
	buff)
    (save-excursion
      (while alist
	(setq buff (cdr (car alist)))
	(if (buffer-name buff)
	    (progn
	      (set-buffer buff)
	      (dired-remove-entry file))
	  (setq dired-buffers (delq (car alist) dired-buffers)))
	(setq alist (cdr alist))))
    (or dired-buffers (dired-remove-from-file-name-handler-alist))))

(defun dired-remove-entry (file)
  (let ((ddir (expand-file-name default-directory))
	(dirname (file-name-as-directory file)))
    (if (dired-in-this-tree ddir dirname)
	(if (or (memq 'kill-dired-buffer dired-no-confirm)
		(y-or-n-p (format "Kill dired buffer %s for %s, too? "
				  (buffer-name) dired-directory)))
	    (kill-buffer (current-buffer)))
      (if (dired-in-this-tree file ddir)
	  (let ((alist dired-subdir-alist))
	    (while alist
	      (if (dired-in-this-tree (car (car alist)) dirname)
		  (save-excursion
		    (goto-char (dired-get-subdir-min (car alist)))
		    (dired-kill-subdir)))
	      (setq alist (cdr alist)))
	    (dired-save-excursion
	      (and (dired-goto-file file)
		   (let (buffer-read-only)
		     (delete-region
		      (progn (skip-chars-backward "^\n\r")
			     (or (memq (following-char) '(\n \r ?\ ))
				 (progn
				   (dired-update-marker-counters
				    (following-char) t)
				   (dired-update-mode-line-modified)))
			     (1- (point)))
		      (progn (skip-chars-forward "^\n\r") (point)))
		     (if dired-verify-modtimes
			 (dired-set-file-modtime
			  (file-name-directory (directory-file-name file))
			  dired-subdir-alist))))))))))

(defun dired-add-file (filename &optional marker-char)
  (dired-fun-in-all-buffers
   (file-name-directory filename)
   (function dired-add-entry) filename marker-char))

(defun dired-relist-file (file)
  (dired-uncache file nil)
  (dired-fun-in-all-buffers (file-name-directory file)
			    (function dired-relist-entry) file))

(defun dired-relist-entry (file)
  ;; Relist the line for FILE, or just add it if it did not exist.
  ;; FILE must be an absolute pathname.
  (let* ((file (directory-file-name file))
	 (directory (file-name-directory file))
	 (dd (expand-file-name default-directory)))
    (if (assoc directory dired-subdir-alist)
	(if (or
	     ;; Not a wildcard
	     (equal dd dired-directory)
	     ;; Not top-level
	     (not (string-equal directory dd))
	     (and (string-equal directory
				(if (consp dired-directory)
				    (file-name-as-directory
				     (car dired-directory))
				  (file-name-directory dired-directory)))
		  (dired-file-in-wildcard-p dired-directory file)))
	    (let ((marker (save-excursion
			    (and (dired-goto-file file)
				 (dired-file-marker file)))))
	      ;; recompute omission
	      (if (eq marker dired-omit-marker-char)
		  (setq marker nil))
	      (dired-add-entry file marker 'relist))
	  ;; At least tell dired that we considered updating the buffer.
	  (if dired-verify-modtimes
	      (dired-set-file-modtime directory dired-subdir-alist))))))

(defun dired-file-in-wildcard-p (wildcard file)
  ;; Return t if a file is part of the listing for wildcard.
  ;; File should be the non-directory part only.
  ;; This version is slow, but meticulously correct.  Is it worth it?
  (if (consp wildcard)
      (let ((files (cdr wildcard))
	    (dir (car wildcard))
	    yep)
	(while (and files (not yep))
	  (setq yep (string-equal file (expand-file-name (car files) dir))
		files (cdr files)))
	yep)
    (let ((err-buff
	   (let ((default-major-mode 'fundamental-mode))
	     (get-buffer-create " *dired-check-process output*")))
	  (dir default-directory)
	  (process-connection-type nil))
      (save-excursion
	(set-buffer err-buff)
	(erase-buffer)
	(setq default-directory dir)
	(call-process shell-file-name nil t nil "-c"
		    (concat dired-ls-program " -d " wildcard " | "
			    "egrep '(^|/)" file "$'"))
	(/= (buffer-size) 0)))))

;; The difference between dired-add-file and dired-relist-file is that
;; the former creates the entry with a specific marker.  The later preserves
;; existing markers on a per buffer basis.  This is not the same as
;; giving dired-create-files a marker of t, which uses a marker in a specific
;; buffer to determine the marker for file line creation in all buffers.


;;;; ----------------------------------------------------------------
;;;; Applying Lisp functions to marked files.
;;;; ----------------------------------------------------------------

;;; Running tags commands on marked files.
;;
;; Written 8/30/93 by Roland McGrath <roland@gnu.ai.mit.edu>.
;; Requires tags.el as distributed with GNU Emacs 19.23, or later.

(defun dired-do-tags-search (regexp)
  "Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch marked files (regexp): ")
  (tags-search regexp '(dired-get-marked-files)))

(defun dired-do-tags-query-replace (from to &optional delimited)
  "Query-replace-regexp FROM with TO through all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query-replace
with the command \\[tags-loop-continue]."
  (interactive
   "sQuery replace in marked files (regexp): \nsQuery replace %s by: \nP")
  (tags-query-replace from to delimited '(dired-get-marked-files)))

;;; byte compiling

(defun dired-byte-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	 buffer-read-only failure)
    (condition-case err
	(save-excursion (byte-compile-file filename))
      (error
       (setq failure err)))
    ;; We should not need to update any file lines, as this will have
    ;; already been done by after-write-region-hook.
    (and failure
	 (progn
	   (dired-log (buffer-name (current-buffer))
		      "Byte compile error for %s:\n%s\n" filename failure)
	   (dired-make-relative filename)))))

(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-byte-compile) arg
			      'byte-compile "byte-compile" t))

;;; loading

(defun dired-load ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
      (load file nil nil t)
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log (buffer-name (current-buffer))
		 "Load error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-load) arg 'load "load" t))


;;;; ----------------------------------------------------------------
;;;; File Name Handler Alist
;;;; ----------------------------------------------------------------
;;;
;;;  Make sure that I/O functions maintain dired buffers.

(defun dired-remove-from-file-name-handler-alist ()
  ;; Remove dired from the file-name-handler-alist
  (setq file-name-handler-alist
	(delq nil
	      (mapcar
	       (function
		(lambda (x)
		  (and (not (eq (cdr x) 'dired-handler-fn))
		       x)))
	       file-name-handler-alist))))

(defun dired-check-file-name-handler-alist ()
  ;; Verify that dired is installed as the first item in the alist
  (and dired-refresh-automatically
       (or (eq (cdr (car file-name-handler-alist)) 'dired-handler-fn)
	   (setq file-name-handler-alist
		 (cons
		  '("." . dired-handler-fn)
		  (dired-remove-from-file-name-handler-alist))))))

(defun dired-handler-fn (op &rest args)
  ;; Function to update dired buffers after I/O.
  (prog1
      (let ((inhibit-file-name-handlers
	     (cons 'dired-handler-fn
		   (and (eq inhibit-file-name-operation op)
			inhibit-file-name-handlers)))
	    (inhibit-file-name-operation op))
	(apply op args))
    (let ((dired-omit-silent t)
	  (hf (get op 'dired)))
      (and hf (funcall hf args)))))

(defun dired-handler-fn-1 (args)
  (let ((to (expand-file-name (nth 1 args))))
    (or (member to dired-unhandle-add-files)
	(dired-relist-file to))))

(defun dired-handler-fn-2 (args)
  (let ((from (expand-file-name (car args)))
	(to (expand-file-name (nth 1 args))))
    ;; Don't remove the original entry if making backups.
    ;; Otherwise we lose marks.  I'm not completely happy with the
    ;; logic here.
    (or (and
	 (eq (nth 2 args) t) ; backups always have OK-IF-OVERWRITE t
	 (string-equal (car (find-backup-file-name from)) to))
	(dired-remove-file from))
    (or (member to dired-unhandle-add-files)
	(dired-relist-file to))))

(defun dired-handler-fn-3 (args)
  (let ((to (expand-file-name (nth 2 args))))
    (or (member to dired-unhandle-add-files)
	(dired-relist-file to))))

(defun dired-handler-fn-4 (args)
  (dired-remove-file (expand-file-name (car args))))

(defun dired-handler-fn-5 (args)
  (let ((to (expand-file-name (car args))))
    (or (member to dired-unhandle-add-files)
	(dired-relist-file to))))

(defun dired-handler-fn-6 (args)
  (let ((to (expand-file-name (nth 1 args)))
	(old (expand-file-name (car args))))
    (or (member to dired-unhandle-add-files)
	(dired-relist-file to))
    (dired-relist-file old)))

(put 'copy-file 'dired 'dired-handler-fn-1)
(put 'dired-make-relative-symlink 'dired 'dired-handler-fn-1)
(put 'make-symbolic-link 'dired 'dired-handler-fn-1)
(put 'add-name-to-file 'dired 'dired-handler-fn-6)
(put 'rename-file 'dired 'dired-handler-fn-2)
(put 'write-region 'dired 'dired-handler-fn-3)
(put 'delete-file 'dired 'dired-handler-fn-4)
(put 'delete-directory 'dired 'dired-handler-fn-4)
(put 'dired-recursive-delete-directory 'dired 'dired-handler-fn-4)
(put 'make-directory-internal 'dired 'dired-handler-fn-5)
(put 'set-file-modes 'dired 'dired-handler-fn-5)
  
;;;; ------------------------------------------------------------
;;;; Autoload land.
;;;; ------------------------------------------------------------

;;; Reading mail (dired-xy)

(autoload 'dired-read-mail "dired-xy"
	  "Reads the current file as a mail folder." t)
(autoload 'dired-vm "dired-xy" "Run VM on this file." t)
(autoload 'dired-rmail "dired-xy" "Run RMAIL on this file." t)

;;; Virtual dired (dired-vir)

(autoload 'dired-virtual "dired-vir"
	  "Put this buffer into virtual dired mode." t)

;;; Grep (dired-grep)

(autoload 'dired-do-grep "dired-grep" "Grep marked files for a pattern." t)

;;; Doing diffs (dired-diff)

(autoload 'dired-diff "dired-diff"
	  "Compare file at point with FILE using `diff'." t)
(autoload 'dired-backup-diff "dired-diff"
	  "Diff this file with its backup file or vice versa." t)
(autoload 'dired-emerge "dired-diff"
	  "Merge file at point with FILE using `emerge'." t)
(autoload 'dired-emerge-with-ancestor "dired-diff"
	  "Merge file at point with FILE, using a common ANCESTOR file." t)
(autoload 'dired-ediff "dired-diff" "Ediff file at point with FILE." t)
(autoload 'dired-epatch "dired-diff" "Patch file at point using `epatch'." t)

;;; Shell commands (dired-shell)

(autoload 'dired-do-print "dired-shell" "Print the marked (next ARG) files." t)
(autoload 'dired-run-shell-command "dired-shell" nil)
(autoload 'dired-do-shell-command "dired-shell"
	  "Run a shell command on the marked (or next ARG) files." t)
(autoload 'dired-do-background-shell-command "dired-shell"
	  "Run a background shell command on marked (or next ARG) files." t)

;;; Commands using regular expressions (dired-rgxp)

(autoload 'dired-mark-files-regexp "dired-rgxp"
	  "Mark all files whose names match REGEXP." t)
(autoload 'dired-flag-files-regexp "dired-rgxp"
	  "Flag for deletion all files whose names match REGEXP." t)
(autoload 'dired-mark-extension "dired-rgxp"
	  "Mark all files whose names have a given extension." t)
(autoload 'dired-flag-extension "dired-rgxp"
	  "Flag for deletion all files whose names have a given extension." t)
(autoload 'dired-cleanup "dired-rgxp"
	  "Flag for deletion dispensable files files created by PROGRAM." t)
(autoload 'dired-do-rename-regexp "dired-rgxp"
	  "Rename marked files whose names match a given regexp." t)
(autoload 'dired-do-copy-regexp "dired-rgxp"
	  "Copy marked files whose names match a given regexp." t)
(autoload 'dired-do-hardlink-regexp "dired-rgxp"
	  "Hardlink all marked files whose names match a regexp." t)
(autoload 'dired-do-symlink "dired-rgxp"
	  "Make a symbolic link to all files whose names match a regexp." t)
(autoload
 'dired-do-relsymlink-regexp "dired-rgxp"
 "Make a relative symbolic link to all files whose names match a regexp." t)
(autoload 'dired-upcase "dired-rgxp"
	  "Rename all marked (or next ARG) files to upper case." t)
(autoload 'dired-downcase "dired-rgxp"
	  "Rename all marked (or next ARG) files to lower case." t)

;;; Marking files from other buffers (dired-mob)

(autoload 'dired-mark-files-from-other-dired-buffer "dired-mob"
	  "Mark files which are marked in another dired buffer." t)
(autoload 'dired-mark-files-compilation-buffer "dired-mob"
	  "Mark the files mentioned in the compilation buffer." t)

;;; uuencoding (dired-uu)

(autoload 'dired-do-uucode "dired-uu" "Uuencode or uudecode marked files." t)

;;; Compressing (dired-cmpr)

(autoload 'dired-do-compress "dired-cmpr"
	  "Compress or uncompress marked files." t)
(autoload 'dired-compress-subdir-files "dired-cmpr"
	  "Compress uncompressed files in the current subdirectory." t)


;;; Marking files according to sexps

(autoload 'dired-mark-sexp "dired-sex"
	  "Mark files according to an sexpression." t)

;;; Help!

(autoload 'dired-summary "dired-help"
	  "Display summary of basic dired commands in the minibuffer." t)
(autoload 'dired-describe-mode "dired-help"
	  "Detailed description of dired mode.
With a prefix, runs the info documentation browser for dired." t)
(autoload 'dired-apropos "dired-help"
	  "Do command apropos help for dired commands.
With prefix does apropos help for dired variables." t)
(autoload 'dired-report-bug "dired-help" "Report a bug for dired." t)

;;;; --------------------------------------------------------------
;;;; Multi-flavour Emacs support
;;;; --------------------------------------------------------------

(let ((lucid-p (string-match "XEmacs" emacs-version))
      ver)
  (or (string-match "^\\([0-9]+\\)\\." emacs-version)
      (error "Weird emacs version %s" emacs-version))
  (setq ver (string-to-int (substring emacs-version (match-beginning 1)
				      (match-end 1))))

  ;; Reading with history.
  (if (>= ver 19)

      (defun dired-read-with-history (prompt initial history)
	(read-from-minibuffer prompt initial nil nil history))
    
    (defun dired-read-with-history (prompt initial history)
      (let ((minibuffer-history-symbol history)) ; for gmhist
	(read-string prompt initial))))
  
  ;; Completing read with history.
  (if (>= ver 19)

      (fset 'dired-completing-read 'completing-read)

    (defun dired-completing-read (prompt table &optional predicate
					 require-match initial-input history)
      (let ((minibuffer-history-symbol history)) ; for gmhist
	(completing-read prompt table predicate require-match
			 initial-input))))
  
  ;; Abbreviating file names.
  (if lucid-p
      (fset 'dired-abbreviate-file-name
	    ;; Lemacs has this extra hack-homedir arg
	    (function
	     (lambda (fn)
	       (abbreviate-file-name fn t))))
    (fset 'dired-abbreviate-file-name 'abbreviate-file-name))

  ;; Deleting directories
  ;; Check for pre 19.8 versions of lucid emacs.
  (if lucid-p
      (or (fboundp 'delete-directory)
	  (fset 'delete-directory 'remove-directory)))
  
  ;; Minibuffers
  (if (= ver 18)
    
      (defun dired-get-active-minibuffer-window ()
	(and (> (minibuffer-depth) 0)
	     (minibuffer-window)))
    
    (defun dired-get-active-minibuffer-window ()
      (let ((frames (frame-list))
	    win found)
	(while frames
	  (if (and (setq win (minibuffer-window (car frames)))
		   (minibuffer-window-active-p win))
	      (setq found win
		    frames nil)
	    (setq frames (cdr frames))))
	found)))

  ;; Text properties and menus.

  (cond
   (lucid-p
    (require 'dired-xemacs))
   ((>= ver 19)
    (require 'dired-fsf))
   (t
    ;; text property stuff doesn't work in V18
    (fset 'dired-insert-set-properties 'ignore)
    (fset 'dired-remove-text-properties 'ignore)
    (fset 'dired-set-text-properties 'ignore)
    (fset 'dired-move-to-filename 'dired-manual-move-to-filename)
    (fset 'dired-move-to-end-of-filename
	  'dired-manual-move-to-end-of-filename))))

;;; MULE

(if (or (boundp 'MULE) (featurep 'mule)) (load "dired-mule"))


;; Run load hook for user customization.
(run-hooks 'dired-load-hook)

;;; end of dired.el
