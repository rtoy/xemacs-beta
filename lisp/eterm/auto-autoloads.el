;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'eterm-autoloads))
    (progn

;;;### (autoloads (term make-term) "term" "eterm/term.el")

(autoload 'make-term "term" "\
Make a term process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to 
the process.  Any more args are arguments to PROGRAM." nil nil)

(autoload 'term "term" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (tperldb txdb tdbx tsdb tgdb) "tgud" "eterm/tgud.el")

(autoload 'tgdb "tgud" "\
Run gdb on program FILE in buffer *tgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'tsdb "tgud" "\
Run sdb on program FILE in buffer *tgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'tdbx "tgud" "\
Run dbx on program FILE in buffer *tgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'txdb "tgud" "\
Run xdb on program FILE in buffer *tgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'tgud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory." t nil)

(autoload 'tperldb "tgud" "\
Run perldb on program FILE in buffer *tgud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

;;;***

;;;### (autoloads (tshell) "tshell" "eterm/tshell.el")

(defvar tshell-prompt-pattern "^[^#$%>\n]*[#$%>] *" "\
Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `term-prompt-regexp' in the 
shell buffer.

The pattern should probably not match more than one line.  If it does,
tshell-mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file.")

(autoload 'tshell "tshell" "\
Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Tshell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `tshell-mode'.
See also the variable `tshell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)

;;;***

(provide 'eterm-autoloads)
))
