;;; process.el --- commands for subprocesses; split out of simple.el

;; Copyright (C) 1985-7, 1993,4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 2000, 2001, 2002 Ben Wing.

;; Author: Ben Wing
;; Maintainer: XEmacs Development Team
;; Keywords: internal, processes, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30, except for setenv/getenv (synched with FSF
;;; 21.0.105).

;;; Authorship:

;; Created 1995 by Ben Wing during Mule work -- some commands split out
;; of simple.el and wrappers of *-internal functions created so they could
;; be redefined in a Mule world.
;; Lisp definition of call-process-internal added Mar. 2000 by Ben Wing.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:


(defgroup processes nil
  "Process, subshell, compilation, and job control support."
  :group 'external
  :group 'development)

(defgroup processes-basics nil
  "Basic stuff dealing with processes."
  :group 'processes)

(defgroup execute nil
  "Executing external commands."
  :group 'processes)

;; This may be changed to "/c" in win32-native.el.

(defvar shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument.")

(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
Variables `shell-file-name' and `shell-command-switch' are used to
start the process.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handled as usual in the shell."
  ;; We used to use `exec' to replace the shell with the command,
  ;; but that failed to handle (...) and semicolon, etc.
  (start-process name buffer shell-file-name shell-command-switch
		 (mapconcat #'identity args " ")))

(defun process-synchronize-point (proc)
  "Set the point(s) in buffer and stderr-buffer according to the process mark."
  ;; We need this because the documentation says to insert *BEFORE* point,
  ;; but we end up inserting after because only the process mark moves
  ;; forward, not point.  We synchronize after every place output might
  ;; happen, in sentinels, and in an unwind-protect, to make *SURE* that
  ;; point is correct. (We could do this more easily and perhaps more
  ;; safely using a process filter, but that would create a LOT of garbage
  ;; since all the data would get sent in strings.) We make this a separate
  ;; function, not an flet, due to dynamic binding problems -- the flet may
  ;; not still be in scope when the sentinel is called.
  (let ((pb (process-buffer proc))
	(pm (process-mark proc)))
    (if (and pb (buffer-live-p pb) (marker-buffer pm))
	(goto-char pm pb))
    (if (process-has-separate-stderr-p proc)
	(let ((pseb (process-stderr-buffer proc))
	      (psem (process-stderr-mark proc)))
	  (if (and pseb (not (eq pb pseb))
		   (buffer-live-p pseb)
		   (marker-buffer psem))
	      (goto-char psem pseb))))))

(defun call-process-internal (program &optional infile buffer display
				      &rest args)
  "Internal function to call PROGRAM synchronously in separate process.
Lisp callers should use `call-process' or `call-process-region'.

The program's input comes from file INFILE (nil means `/dev/null').
XEmacs feature: INFILE can also be a list of (BUFFER [START [END]]), i.e.
a list of one to three elements, consisting of a buffer and optionally
a start position or start and end position.  In this case, input comes
from the buffer, starting from START (defaults to the beginning of the
buffer) and ending at END (defaults to the end of the buffer).

Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
If BUFFER is a string, then find or create a buffer with that name,
then insert the output in that buffer, before point.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), a file name string, or (XEmacs feature)
a buffer object.  If STDERR-FILE is a buffer object (but not the name of
a buffer, since that would be interpreted as a file), the standard error
output will be inserted into the buffer before point.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If BUFFER is 0, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate and returns a numeric exit status
or a signal description string.  If you quit, the process is first killed
with SIGINT, then with SIGKILL if you quit again before the process exits.

Coding systems for the process are the same as for `start-process-internal'."
  (let (proc inbuf errbuf kill-inbuf kill-errbuf no-wait start end)
    ;; first set up an unwind-protect to clean everything up.  this will:
    ;;
    ;; -- kill the process. (when we're not waiting for it to finish, we
    ;;    set PROC to nil when we're ready to exit so this doesn't happen --
    ;;    if we're interrupted before we're ready to exit, we should still
    ;;    kill the process)
    ;; -- kill temporary buffers created to handle I/O to or from a file.
    ;;    KILL-INBUF/KILL-ERRBUF tell us if we should do so.
    ;;
    ;; note that we need to be *very* careful in this code to handle C-g
    ;; at any point.
    (unwind-protect
	(progn
	  ;; first handle INFILE.
	  (cond ((stringp infile)
		 (setq infile (expand-file-name infile))
		 (setq kill-inbuf t)
		 (setq inbuf (generate-new-buffer "*call-process*"))
		 ;; transfer the exact contents of the file to the process.
		 ;; we do that by reading in and writing out in
		 ;; binary. #### is this even correct?  should we be doing
		 ;; the same thing with stderr?  if so we'd need a way of
		 ;; controlling the stderr coding system separate from
		 ;; everything else.
		 (with-current-buffer inbuf
		   ;; Make sure this works with jka-compr
		   (let ((file-name-handler-alist nil))
		     (insert-file-contents-internal infile nil nil nil nil
						    'binary))
		   (setq start (point-min) end (point-max))))
		((consp infile)
		 (setq inbuf (get-buffer (car infile)))
		 (setq start (or (nth 1 infile) (point-min inbuf)))
		 (setq end (or (nth 2 infile) (point-max inbuf))))
		((null infile) nil)
		(t
		 (error 'wrong-type-argument
			"Must be filename or (BUFFER [START [END]])"
			infile)))
	  ;; now handle BUFFER
	  (let ((stderr (if (consp buffer) (second buffer) t)))
	    (if (consp buffer) (setq buffer (car buffer)))
	    (setq buffer
		  (cond ((null buffer) nil)
			((eq buffer t) (current-buffer))
			;; use integerp for compatibility with existing
			;; call-process rmsism.
			((integerp buffer) (setq no-wait t) nil)
			(t (get-buffer-create buffer))))
	    (when (and stderr (not (eq t stderr)))
	      ;; both ERRBUF and STDERR being non-nil indicates to the
	      ;; code below that STDERR is a file and we should write
	      ;; ERRBUF to it; so clear out STDERR if we don't want this.
	      (if (bufferp stderr) (setq errbuf stderr stderr nil)
		(setq stderr (expand-file-name stderr))
		(setq kill-errbuf t)
		(setq errbuf (generate-new-buffer "*call-process*"))))
	    ;; now start process.  using a pty causes all sorts of
	    ;; weirdness, at least under cygwin, when there's input. #### i
	    ;; don't know what's going wrong and whether it's a cygwin-only
	    ;; problem.  suffice to say that there were NO pty connections
	    ;; in the old version.
	    (let ((process-connection-type nil))
	      (setq proc
		    (apply 'start-process-internal "*call-process*"
			   (if (eq t stderr) buffer (list buffer errbuf))
			   program args)))
	    ;; see comment above where the data was read from the file.
	    (if kill-inbuf
		(set-process-output-coding-system proc 'binary))
	    ;; point mark/stderr-mark at the right place (by default it's
	    ;; end of buffer).
	    (if buffer
		(set-marker (process-mark proc) (point buffer) buffer))
	    (if errbuf
		(set-marker (process-stderr-mark proc) (point errbuf) errbuf))
	      ;; now do I/O, very carefully!  the unwind-protect makes sure
	      ;; to clear out the sentinel, since it does a `throw', which
	      ;; would have no catch (or writes to a file -- we only want
	      ;; this on normal exit)
	    (unwind-protect
		;; if not NO-WAIT, set a sentinel to return the exit
		;; status.  it will throw to this catch so we can exit
		;; properly.
		(catch 'call-process-done
		  (set-process-sentinel
		   proc
		   (cond
		    ((and no-wait errbuf stderr)
		     ;; we're trying really really hard to emulate
		     ;; the old call-process, which would save the
		     ;; stderr to a file even if discarding output.  so
		     ;; we set a sentinel to save the output when
		     ;; we finish.
		     ;;
		     ;; #### not clear if we should be doing this.
		     ;;
		     ;; NOTE NOTE NOTE: Due to the total bogosity of
		     ;; dynamic scoping, and the lack of closures, we
		     ;; have to be careful how we write the first
		     ;; sentinel below since it may be executed after
		     ;; this function has returned -- thus we fake a
		     ;; closure. (This doesn't apply to the second one,
		     ;; which only gets executed within the
		     ;; unwind-protect.)
		     `(lambda (proc status)
			(set-process-sentinel proc nil)
			(process-synchronize-point proc)
			(with-current-buffer ,errbuf
			  (write-region-internal
			   1 (1+ (buffer-size))
			   ,stderr
			   nil 'major-rms-kludge-city nil
			   coding-system-for-write))
			(kill-buffer ,errbuf)))
		    (no-wait nil)
		    (t
		     ;; normal sentinel: maybe write out stderr and return
		     ;; status.
		     #'(lambda (proc status)
			 (process-synchronize-point proc)
			 (when (and errbuf stderr)
			   (with-current-buffer errbuf
			     (write-region-internal
			      1 (1+ (buffer-size)) stderr
			      nil 'major-rms-kludge-city nil
			      coding-system-for-write)))
			 (cond ((eq 'exit (process-status proc))
				(set-process-sentinel proc nil)
				(throw 'call-process-done
				       (process-exit-status proc)))
			       ((eq 'signal (process-status proc))
				(set-process-sentinel proc nil)
				(throw 'call-process-done status)))))))
		  (if (not no-wait)
		      ;; we're waiting.  send the input and loop forever,
		      ;; handling process output and maybe redisplaying.
		      ;; exit happens through the sentinel or C-g.  if
		      ;; C-g, send SIGINT the first time, EOF if not
		      ;; already done so (might make the process exit),
		      ;; and keep waiting.  Another C-g will exit the
		      ;; whole function, and the unwind-protect will
		      ;; kill the process. (Hence the documented semantics
		      ;; of SIGINT/SIGKILL.)
		      (let (eof-sent)
			(condition-case nil
			    (progn
			      (when inbuf
				(process-send-region proc start end inbuf))
			      (process-send-eof proc)
			      (setq eof-sent t)
			      (while t
				(accept-process-output proc)
				(process-synchronize-point proc)
				(if display (sit-for 0))))
			  (quit
			   (process-send-signal 'SIGINT proc)
			   (unless eof-sent
			     (process-send-eof proc))
			   (while t
			     (accept-process-output proc)
			     (process-synchronize-point proc)
			     (if display (sit-for 0))))))
		    ;; discard and no wait: send the input, set PROC
		    ;; and ERRBUF to nil so that the unwind-protect
		    ;; forms don't erase the sentinel, kill the process,
		    ;; or kill ERRBUF (the sentinel does that), and exit.
		    (when inbuf
		      (process-send-region proc start end inbuf))
		    (process-send-eof proc)
		    (setq errbuf nil)
		    (setq proc nil)))
	      ;; inner unwind-protect, once we're ready to do I/O.
	      (when proc
		(set-process-sentinel proc nil)
		(process-synchronize-point proc)))))
      ;; outer unwind-protect forms, to make sure we always clean up.
      (if (and inbuf kill-inbuf) (kill-buffer inbuf))
      (if (and errbuf kill-errbuf) (kill-buffer errbuf))
      (condition-case nil
	  (if (and proc (process-live-p proc)) (kill-process proc))
	(error nil)))))


(defun shell-command (command &optional output-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.

If COMMAND ends in ampersand, execute it asynchronously.
The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

Otherwise, COMMAND is executed synchronously.  The output appears in the
buffer `*Shell Command Output*'.
If the output is one line, it is displayed in the echo area *as well*,
but it is nonetheless available in buffer `*Shell Command Output*',
even though that buffer is not automatically displayed.
If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in current buffer.  (This cannot be done asynchronously.)
In either case, the output is inserted after point (leaving mark after it)."
  (interactive (list (read-shell-command "Shell command: ")
		     current-prefix-arg))
  (if (and output-buffer
	   (not (or (bufferp output-buffer)  (stringp output-buffer))))
      (progn (barf-if-buffer-read-only)
	     (push-mark nil (not (interactive-p)))
	     ;; We do not use -f for csh; we will not support broken use of
	     ;; .cshrcs.  Even the BSD csh manual says to use
	     ;; "if ($?prompt) exit" before things which are not useful
	     ;; non-interactively.  Besides, if someone wants their other
	     ;; aliases for shell commands then they can still have them.
	     (call-process shell-file-name nil t nil
			   shell-command-switch command)
	     (exchange-point-and-mark t))
    ;; Preserve the match data in case called from a program.
    (save-match-data
      (if (string-match "[ \t]*&[ \t]*$" command)
	  ;; Command ending with ampersand means asynchronous.
	  (progn
	    (if-fboundp 'background
		(background (substring command 0
				       (match-beginning 0)))
	      (error
	       'unimplemented
	       "backgrounding a shell command requires package `background'")))
	    
	(shell-command-on-region (point) (point) command output-buffer)))))

;; We have a sentinel to prevent insertion of a termination message
;; in the buffer itself.
(defun shell-command-sentinel (process signal)
  (if (memq (process-status process) '(exit signal))
      (message "%s: %s."
	       (car (cdr (cdr (process-command process))))
	       (substring signal 0 -1))))

(defun shell-command-on-region (start end command
				      &optional output-buffer replace)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.

The noninteractive arguments are START, END, COMMAND, OUTPUT-BUFFER, REPLACE.
If REPLACE is non-nil, that means insert the output
in place of text from START to END, putting point and mark around it.

If the output is one line, it is displayed in the echo area,
but it is nonetheless available in buffer `*Shell Command Output*'
even though that buffer is not automatically displayed.
If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it)."
  (interactive (let ((string
		      ;; Do this before calling region-beginning
		      ;; and region-end, in case subprocess output
		      ;; relocates them while we are in the minibuffer.
		      (read-shell-command "Shell command on region: ")))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg)))
  (if (or replace
	  (and output-buffer
	       (not (or (bufferp output-buffer) (stringp output-buffer)))))
      ;; Replace specified region with output from command.
      (let ((swap (and replace (< start end))))
	;; Don't muck with mark unless REPLACE says we should.
	(goto-char start)
	(and replace (push-mark))
	(call-process-region start end shell-file-name t t nil
			     shell-command-switch command)
	(let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  (and shell-buffer (not (eq shell-buffer (current-buffer)))
	       (kill-buffer shell-buffer)))
	;; Don't muck with mark unless REPLACE says we should.
	(and replace swap (exchange-point-and-mark t)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
    (let ((buffer (get-buffer-create
		   (or output-buffer "*Shell Command Output*")))
	  (success nil)
	  (exit-status nil)
	  (directory default-directory))
      (unwind-protect
	  (if (eq buffer (current-buffer))
	      ;; If the input is the same buffer as the output,
	      ;; delete everything but the specified region,
	      ;; then replace that region with the output.
	      (progn (setq buffer-read-only nil)
		     (delete-region (max start end) (point-max))
		     (delete-region (point-min) (min start end))
		     (setq exit-status
			   (call-process-region (point-min) (point-max)
						shell-file-name t t nil
						shell-command-switch command))
		     (setq success t))
	    ;; Clear the output buffer,
	    ;; then run the command with output there.
	    (save-excursion
	      (set-buffer buffer)
	      (setq buffer-read-only nil)
	      ;; XEmacs change
	      (setq default-directory directory)
	      (erase-buffer))
	    (setq exit-status
		  (call-process-region start end shell-file-name
				       nil buffer nil
				       shell-command-switch command))
	    (setq success t))
	;; Report the amount of output.
	(let ((lines (save-excursion
		       (set-buffer buffer)
		       (if (= (buffer-size) 0)
			   0
			 (count-lines (point-min) (point-max))))))
	  (cond ((= lines 0)
		 (if success
		     (display-message
		      'command
		      (if (eql exit-status 0)
			  "(Shell command succeeded with no output)"
			"(Shell command failed with no output)")))
		 (kill-buffer buffer))
		((and success (= lines 1))
		 (message "%s"
			  (save-excursion
			    (set-buffer buffer)
			    (goto-char (point-min))
			    (buffer-substring (point)
					      (progn (end-of-line)
						     (point))))))
		(t
		 (set-window-start (display-buffer buffer) 1))))))))

(defun shell-quote-argument (argument)
  "Quote an argument for passing as argument to an inferior shell."
  (if (and (eq system-type 'windows-nt)
	   (let ((progname (downcase (file-name-nondirectory
				      shell-file-name))))
	     (or (equal progname "command.com")
		 (equal progname "cmd.exe"))))
      ;; the expectation is that you can take the result of
      ;; shell-quote-argument and pass it to as an arg to
      ;; (start-process shell-quote-argument ...) and have it end
      ;; up as-is in the program's argv[] array.  to do this, we
      ;; need to protect against both the shell's and the program's
      ;; quoting conventions (and our own conventions in
      ;; mswindows-construct-process-command-line!).  Putting quotes
      ;; around shell metachars gets through the last two, and applying
      ;; the normal VC runtime quoting works with practically all apps.
      (declare-fboundp (mswindows-quote-one-vc-runtime-arg argument t))
    (if (equal argument "")
	"\"\""
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (let ((result "") (start 0) end)
	(while (string-match "[^-0-9a-zA-Z_./]" argument start)
	  (setq end (match-beginning 0)
		result (concat result (substring argument start end)
			       "\\" (substring argument end (1+ end)))
		start (1+ end)))
	(concat result (substring argument start))))))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))

(defalias 'exec-to-string 'shell-command-to-string)


;; History list for environment variable names.
(defvar read-envvar-name-history nil)

(defun read-envvar-name (prompt &optional mustmatch)
  "Read environment variable name, prompting with PROMPT.
Optional second arg MUSTMATCH, if non-nil, means require existing envvar name.
If it is also not t, RET does not exit if it does non-null completion."
  (completing-read prompt
		   (mapcar (function
			    (lambda (enventry)
			      (list (substring enventry 0
					       (string-match "=" enventry)))))
			   process-environment)
		   nil mustmatch nil 'read-envvar-name-history))

;; History list for VALUE argument to setenv.
(defvar setenv-history nil)

(defun substitute-env-vars (string)
  "Substitute environment variables referred to in STRING.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.  Use `$$' to insert a single
dollar sign."
  (let ((start 0))
    (while (string-match
	    ;; XEmacs change - FSF use their rx macro to generate this regexp
	    "\\(?:\\$\\(\\(?:[a-zA-Z0-9_]\\)+\\)\\)\\|\\(?:\\${\\(\\(?:.\\|\n\\)*?\\)}\\)\\|\\$\\$"
	    string start)
      (cond ((match-beginning 1)
	     (let ((value (getenv (match-string 1 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    ((match-beginning 2)
	     (let ((value (getenv (match-string 2 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    (t
	     (setq string (replace-match "$" t t string)
		   start (+ (match-beginning 0) 1)))))
    string))

(defun setenv (variable &optional value unset substitute-env-vars)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  

UNSET, if non-nil, means to remove VARIABLE from the environment.
SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE using `substitute-env-vars'.

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.

This function works by modifying `process-environment'."
  (interactive
   (if current-prefix-arg
       (list (read-envvar-name "Clear environment variable: " 'exact) nil t)
     (let ((var (read-envvar-name "Set environment variable: " nil)))
       ;; Here finally we specify the args to call setenv with.
       (list var (read-from-minibuffer (format "Set %s to value: " var)
				       nil nil nil 'setenv-history
				       (getenv var))))))
  (if unset 
      (setq value nil)
    (if substitute-env-vars
	(setq value (substitute-env-vars value))))
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable)
    (let ((pattern (concat "\\`" (regexp-quote (concat variable "="))))
	  (case-fold-search nil)
	  (scan process-environment)
	  found)
      (if (string-equal "TZ" variable)
	  (set-time-zone-rule value))
      (while scan
	(cond ((string-match pattern (car scan))
	       (setq found t)
	       (if (eq nil value)
		   (setq process-environment (delq (car scan) process-environment))
		 (setcar scan (concat variable "=" value)))
	       (setq scan nil)))
	(setq scan (cdr scan)))
      (or found
	  (if value
	      (setq process-environment
		    (cons (concat variable "=" value)
			  process-environment)))))))

;; already in C.  Can't move it to Lisp too easily because it's needed
;; extremely early in the Lisp loadup sequence.

; (defun getenv (variable)
;   "Get the value of environment variable VARIABLE.
; VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
; the environment.  Otherwise, value is a string.
;
; This function consults the variable `process-environment'
; for its value."
;   (interactive (list (read-envvar-name "Get environment variable: " t)))
;   (let ((value (getenv-internal variable)))
;     (when (interactive-p)
;       (message "%s" (if value value "Not set")))
;     value))

(provide 'env) ;; Yuck.  Formerly the above were in env.el, which did this
	       ;; provide.

;;; process.el ends here
