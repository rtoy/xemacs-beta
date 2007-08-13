;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'comint-autoloads))
    (progn

;;;### (autoloads (background) "background" "comint/background.el")

(autoload 'background "background" "\
Run COMMAND in the background like csh.  
A message is displayed when the job starts and finishes.  The buffer is in
comint mode, so you can send input and signals to the job.  The process object
is returned if anyone cares.  See also comint-mode and the variables
background-show and background-select.

Optional second argument BUFFER-NAME is a buffer to insert the output into.
If omitted, a buffer name is constructed from the command run." t nil)

;;;***

;;;### (autoloads (comint-dynamic-list-completions comint-dynamic-complete comint-run make-comint) "comint" "comint/comint.el")

(autoload 'make-comint "comint" "\
Make a comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM." nil nil)

(autoload 'comint-run "comint" "\
Run PROGRAM in a comint buffer and switch to it.
The buffer name is made by surrounding the file name of PROGRAM with `*'s.
The file name is used to make a symbol name, such as `comint-sh-hook', and any
hooks on this symbol are run in the buffer.
See `make-comint' and `comint-exec'." t nil)

(autoload 'comint-dynamic-complete "comint" "\
Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions' to perform
completion until a function returns non-nil, at which point completion is
assumed to have occurred." t nil)

(autoload 'comint-dynamic-list-completions "comint" "\
List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer." nil nil)

;;;***

;;;### (autoloads (gdb) "gdb" "comint/gdb.el")

(defvar gdb-command-name "gdb" "\
Pathname for executing gdb.")

(autoload 'gdb "gdb" "\
Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'." t nil)

;;;***

;;;### (autoloads (gdbsrc) "gdbsrc" "comint/gdbsrc.el")

(autoload 'gdbsrc "gdbsrc" "\
Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug." t nil)

;;;***

;;;### (autoloads (perldb xdb dbx sdb) "gud" "comint/gud.el")

(autoload 'sdb "gud" "\
Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'dbx "gud" "\
Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'xdb "gud" "\
Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory." t nil)

(autoload 'perldb "gud" "\
Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

;;;***

;;;### (autoloads nil "inf-lisp" "comint/inf-lisp.el")

(add-hook 'same-window-buffer-names "*inferior-lisp*")

;;;***

;;;### (autoloads (rlogin) "rlogin" "comint/rlogin.el")

(add-hook 'same-window-regexps "^\\*rlogin-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'rlogin "rlogin" "\
Open a network login connection to HOST via the `rlogin' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*rlogin-HOST*'
\(or `*rlogin-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*rlogin-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or 
buffer, it names the buffer to use.

The variable `rlogin-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `rlogin-explicit-args' is a list of arguments to give to
the rlogin when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `rlogin-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `rlogin-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `rlogin-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (shell) "shell" "comint/shell.el")

(defvar shell-prompt-pattern (purecopy "^[^#$%>\n]*[#$%>] *") "\
Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `comint-prompt-regexp' in the
shell buffer.

The pattern should probably not match more than one line.  If it does,
shell-mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file.")

(autoload 'shell "shell" "\
Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)

(add-hook 'same-window-buffer-names "*shell*")

;;;***

;;;### (autoloads (ssh) "ssh" "comint/ssh.el")

(add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'ssh "ssh" "\
Open a network login connection via `ssh' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `ssh'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*ssh-HOST*'
\(or `*ssh-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*ssh-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `ssh-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ssh-explicit-args' is a list of arguments to give to
the ssh when starting.  They are prepended to any arguments given in
INPUT-ARGS.

If the default value of `ssh-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ssh-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ssh-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (rsh telnet) "telnet" "comint/telnet.el")

(add-hook 'same-window-regexps "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'telnet "telnet" "\
Open a network login connection to host named HOST (a string).
With a prefix argument, prompts for the port name or number as well.
Communication with HOST is recorded in a buffer `*HOST-telnet*'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[rsh]'." t nil)

(add-hook 'same-window-regexps "\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)")

(autoload 'rsh "telnet" "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*rsh-HOST*'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[telnet]'." t nil)

;;;***

(provide 'comint-autoloads)
))
