;;; DO NOT MODIFY THIS FILE
(if (featurep '-autoloads) (error "Already loaded"))

;;;### (autoloads nil "default-dir" "efs/default-dir.el")

(defvar default-directory-function nil "\
A function to call to compute the default-directory for the current buffer.
If this is nil, the function default-directory will return the value of the
variable default-directory.
Buffer local.")

;;;***

;;;### (autoloads (dired-jump-back-other-frame dired-jump-back-other-window dired-jump-back dired-noselect dired-other-frame dired-other-window dired default-directory) "dired" "efs/dired.el")

(autoload 'default-directory "dired" "\
 Returns the default-directory for the current buffer.
Will use the variable default-directory-function if it non-nil." nil nil)

(defvar dired-compression-method 'compress "\
*Type of compression program to use.
Give as a symbol.
Currently-recognized methods are: gzip pack compact compress.
To change this variable use \\[dired-do-compress] with a zero prefix.")

(defvar dired-compression-method-alist '((gzip ".gz" ("gzip") ("gzip" "-d") "-f") (compress ".Z" ("compress" "-f") ("compress" "-d") "-f") (pack ".z" ("pack" "-f") ("unpack")) (compact ".C" ("compact") ("uncompact"))) "\
*Association list of compression method descriptions.
 Each element of the table should be a list of the form
 
     (compress-type extension (compress-args) (decompress-args) force-flag)
 
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
 
   (setq dired-compression-method-alist
         (cons '(frobnicate \".frob\" (\"frob\") (\"frob\" \"-d\") \"-f\")
               dired-compression-method-alist))
   => ((frobnicate \".frob\" (\"frob\") (\"frob\" \"-d\")) 
       (gzip \".gz\" (\"gzip\") (\"gunzip\"))
       ...)
 
 See also: dired-compression-method <V>")

(defvar dired-ls-program "ls" "\
*Absolute or relative name of the ls program used by dired.")

(defvar dired-listing-switches "-al" "\
*Switches passed to ls for dired. MUST contain the `l' option.
Can contain even `F', `b', `i' and `s'.")

(defvar dired-chown-program (if (memq system-type '(hpux dgux usg-unix-v linux)) "chown" "/etc/chown") "\
*Name of chown command (usually `chown' or `/etc/chown').")

(defvar dired-gnutar-program nil "\
*If non-nil, name of the GNU tar executable (e.g. \"tar\" or \"gnutar\").
GNU tar's `z' switch is used for compressed tar files.
If you don't have GNU tar, set this to nil: a pipe using `zcat' is then used.")

(defvar dired-unshar-program nil "\
*Set to the name of the unshar program, if you have it.")

(defvar dired-local-variables-file ".dired" "\
*If non-nil, filename for local variables for Dired.
If Dired finds a file with that name in the current directory, it will
temporarily insert it into the dired buffer and run `hack-local-variables'.

Type \\[info] and `g' `(emacs)File Variables' `RET' for more info on
local variables.")

(defvar dired-kept-versions 2 "\
*When cleaning directory, number of versions to keep.")

(defvar dired-find-subdir nil "\
*Determines whether dired tries to lookup a subdir in existing buffers.
If non-nil, dired does not make a new buffer for a directory if it can be
found (perhaps as subdir) in some existing dired buffer. If there are several
dired buffers for a directory, then the most recently used one is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, C-x d RET will
toggle between those two.")

(defvar dired-use-file-transformers t "\
*Determines whether dired uses file transformers.
If non-nil `dired-do-shell-command' will apply file transformers to file names.
See \\[describe-function] for dired-do-shell-command for more information.")

(defvar dired-dwim-target nil "\
*If non-nil, dired tries to guess a default target directory.
This means that if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer.
The target is put in the prompt for file copy, rename, etc.")

(defvar dired-copy-preserve-time nil "\
*If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)\\<dired-mode-map>
Use `\\[dired-do-copy]' with a zero prefix argument to toggle its value.")

(defvar dired-no-confirm nil "\
*If non-nil, a list of symbols for commands dired should not confirm.
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

(defvar dired-backup-if-overwrite nil "\
*Non-nil if Dired should ask about making backups before overwriting files.
Special value 'always suppresses confirmation.")

(defvar dired-omit-files nil "\
*If non-nil un-interesting files will be omitted from this dired buffer.
Use \\[dired-omit-toggle] to see these files. (buffer local)")

(defvar dired-mail-reader 'rmail "\
*Mail reader used by dired for dired-read-mail (\\[dired-read-mail]).
The symbols 'rmail and 'vm are the only two allowed values.")

(defvar dired-refresh-automatically t "\
*If non-nil, refresh dired buffers automatically after file operations.")

(define-key ctl-x-map "d" 'dired)

(autoload 'dired "dired" "\
\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the resr as an explicit
list of files to make directory entries for.
\\<dired-mode-map>You can move around in it with the usual commands.
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-expunge-deletions].
Type \\[dired-describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh." t nil)

(define-key ctl-x-4-map "d" 'dired-other-window)

(autoload 'dired-other-window "dired" "\
\"Edit\" directory DIRNAME.  Like `dired' but selects in another window." t nil)

(define-key ctl-x-5-map "d" 'dired-other-frame)

(autoload 'dired-other-frame "dired" "\
\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame." t nil)

(autoload 'dired-noselect "dired" "\
Like `dired' but returns the dired buffer as value, does not select it." nil nil)

(define-key ctl-x-map "\C-j" 'dired-jump-back)

(autoload 'dired-jump-back "dired" "\
Jump back to dired.
If in a file, dired the current directory and move to file's line.
If in dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
  buffer and try again." t nil)

(define-key ctl-x-4-map "\C-j" 'dired-jump-back-other-window)

(autoload 'dired-jump-back-other-window "dired" "\
Like \\[dired-jump-back], but to other window." t nil)

(define-key ctl-x-5-map "\C-j" 'dired-jump-back-other-frame)

(autoload 'dired-jump-back-other-frame "dired" "\
Like \\[dired-jump-back], but in another frame." t nil)

;;;***

;;;### (autoloads (efs-ftp-path) "efs-cu" "efs/efs-cu.el")

(defvar efs-path-root-regexp "^/[^/:]+:" "\
Regexp to match the `/user@host:' root of an efs full path.")

(autoload 'efs-ftp-path "efs-cu" "\
Parse PATH according to efs-path-regexp.
Returns a list (HOST USER PATH), or nil if PATH does not match the format." nil nil)

;;;***

;;;### (autoloads (remote-path-file-handler-function) "efs-dump" "efs/efs-dump.el")

(or (assoc efs-path-root-regexp file-name-handler-alist) (setq file-name-handler-alist (cons (cons efs-path-root-regexp 'remote-path-file-handler-function) file-name-handler-alist)))

(autoload 'remote-path-file-handler-function "efs-dump" "\
Function to call special file handlers for remote files." nil nil)

;;;***

;;;### (autoloads nil "efs-fnh" "efs/efs-fnh.el")

(defvar allow-remote-paths t "\
*Set this to nil if you don't want remote paths to access
remote files.")

;;;***

;;;### (autoloads (efs-root-file-name-completion efs-root-file-name-all-completions efs-set-passwd) "efs-netrc" "efs/efs-netrc.el")

(autoload 'efs-set-passwd "efs-netrc" "\
For a given HOST and USER, set or change the associated PASSWORD." t nil)

(autoload 'efs-root-file-name-all-completions "efs-netrc" nil nil nil)

(autoload 'efs-root-file-name-completion "efs-netrc" nil nil nil)

;;;***

;;;### (autoloads (efs-report-bug) "efs-report" "efs/efs-report.el")

(autoload 'efs-report-bug "efs-report" "\
Submit a bug report for efs." t nil)

;;;***

;;;### (autoloads (efs-file-handler-function efs-nslookup-host efs-display-ftp-activity) "efs" "efs/efs.el")

(autoload 'efs-display-ftp-activity "efs" "\
Displays the number of active background ftp sessions in the modeline.
Uses the variable `efs-mode-line-format' to determine how this will be
displayed." t nil)

(autoload 'efs-nslookup-host "efs" "\
Attempt to resolve the given HOSTNAME using nslookup if possible." t nil)

(autoload 'efs-file-handler-function "efs" "\
Function to call special file handlers for remote files." nil nil)

;;;***

(provide '-autoloads)
