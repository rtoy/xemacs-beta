;;; tshell.el --- specialized term.el for running the shell.

;; Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

;; Author: Per Bothner <bothner@cygnus.com>
;; Original comint version author: Olin Shivers <shivers@cs.cmu.edu>
;; Comint version maintainer: Simon Marshall <s.marshall@dcs.hull.ac.uk>
;; Keywords: processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)
;;;     - Simon Marshall (s.marshall@dcs.hull.ac.uk)

;;; This file defines a a shell-in-a-buffer package (shell mode) built
;;; on top of term mode.  This is actually cmushell with things
;;; renamed to replace its counterpart in Emacs 18.  cmushell is more
;;; featureful, robust, and uniform than the Emacs 18 version.

;;; Since this mode is built on top of the general command-interpreter-in-
;;; a-buffer mode (term mode), it shares a common base functionality, 
;;; and a common set of bindings, with all modes derived from term mode.
;;; This makes these modes easier to use.

;;; For documentation on the functionality provided by term mode, and
;;; the hooks available for customising it, see the file term.el.
;;; For further information on shell mode, see the comments below.

;;; Needs fixin:
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ;; Define C-c t to run my favorite command in shell mode:
;; (setq tshell-mode-hook
;;       '((lambda () 
;;           (define-key tshell-mode-map "\C-ct" 'favorite-cmd))))


;;; Brief Command Documentation:
;;;============================================================================
;;; Term Mode Commands: (common to tshell and all term-derived modes)
;;;
;;; m-p	    term-previous-input    	    Cycle backwards in input history
;;; m-n	    term-next-input  	    	    Cycle forwards
;;; m-r     term-previous-matching-input  Previous input matching a regexp
;;; m-R     term-previous-matching-input-from-input -"- matching input
;;; m-s     term-next-matching-input      Next input that matches
;;; m-S     term-next-matching-input-from-input     -"- matching input
;;; m-c-l   term-show-output		    Show last batch of process output
;;; return  term-send-input
;;; c-c c-a term-bol                      Beginning of line; skip prompt
;;; c-d	    term-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-u term-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c term-interrupt-subjob 	    ^c
;;; c-c c-z term-stop-subjob	    	    ^z
;;; c-c c-\ term-quit-subjob	    	    ^\
;;; c-c c-o term-kill-output		    Delete last batch of process output
;;; c-c c-r term-show-output		    Show last batch of process output
;;; c-c c-h term-dynamic-list-input-ring    List input history
;;;         term-send-invisible             Read line w/o echo & send to proc
;;;         term-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job
;;; term-mode-hook is the term mode hook.

;;; Tshell Mode Commands:
;;;         tshell			    Fires up the shell process
;;; tab     term-dynamic-complete	    Complete filename/command/history
;;; m-?     term-dynamic-list-filename-completions List completions in help buffer
;;; m-c-f   tshell-forward-command           Forward a shell command
;;; m-c-b   tshell-backward-command          Backward a shell command
;;; 	    dirs    			    Resync the buffer's dir stack
;;; 	    dirtrack-toggle                 Turn dir tracking on/off
;;; 	    tshell-strip-ctrl-m              Remove trailing ^Ms from output
;;;
;;; The tshell mode hook is tshell-mode-hook
;;; term-prompt-regexp is initialised to tshell-prompt-pattern, for backwards
;;; compatibility.

;;; Read the rest of this file for more information.

;;; SHELL.EL COMPATIBILITY
;;; Notes from when this was called cmushell, and was not the standard emacs
;;; shell package.
;;;============================================================================
;;; In brief: this package should have no trouble coexisting with shell.el.
;;; 
;;; Most customising variables -- e.g., explicit-shell-file-name -- are the
;;; same, so the users shouldn't have much trouble. Hooks have different
;;; names, however, so you can customise tshell mode differently from cmushell
;;; mode. You basically just have to remember to type M-x cmushell instead of
;;; M-x shell.
;;; 
;;; It would be nice if this file was completely plug-compatible with the old
;;; shell package -- if you could just name this file shell.el, and have it
;;; transparently replace the old one. But you can't.  Several other packages
;;; (tex-mode, background, dbx, gdb, kermit, monkey, prolog, telnet) are also
;;; clients of shell mode. These packages assume detailed knowledge of shell
;;; mode internals in ways that are incompatible with cmushell mode (mostly
;;; because of cmushell mode's greater functionality).  So, unless we are
;;; willing to port all of these packages, we can't have this file be a
;;; complete replacement for shell.el -- that is, we can't name this file
;;; shell.el, and its main entry point (shell), because dbx.el will break
;;; when it loads it in and tries to use it.
;;; 
;;; There are two ways to fix this. One: rewrite these other modes to use the
;;; new package. This is a win, but can't be assumed. The other, backwards
;;; compatible route, is to make this package non-conflict with shell.el, so
;;; both files can be loaded in at the same time. And *that* is why some
;;; functions and variables have different names: (cmushell),
;;; cmushell-mode-map, that sort of thing. All the names have been carefully
;;; chosen so that shell.el and cmushell.el won't tromp on each other.

;;; Customization and Buffer Variables
;;; ===========================================================================
;;; 

;;; Code:

(require 'term)

;;;###autoload
(defvar tshell-prompt-pattern "^[^#$%>\n]*[#$%>] *"
  "Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `term-prompt-regexp' in the 
shell buffer.

The pattern should probably not match more than one line.  If it does,
tshell-mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file.")

(defvar tshell-completion-fignore nil
  "*List of suffixes to be disregarded during file/command completion.
This variable is used to initialize `term-completion-fignore' in the shell
buffer.  The default is nil, for compatibility with most shells.
Some people like (\"~\" \"#\" \"%\").

This is a fine thing to set in your `.emacs' file.")  

(defvar tshell-delimiter-argument-list '(?\| ?& ?< ?> ?\( ?\) ?\;)
  "List of characters to recognise as separate arguments.
This variable is used to initialize `term-delimiter-argument-list' in the
shell buffer.  The default is (?\\| ?& ?< ?> ?\\( ?\\) ?\\;).

This is a fine thing to set in your `.emacs' file.")

(defvar tshell-dynamic-complete-functions
  '(term-replace-by-expanded-history
    tshell-dynamic-complete-environment-variable
    tshell-dynamic-complete-command
    tshell-replace-by-expanded-directory
    term-dynamic-complete-filename)
  "List of functions called to perform completion.
This variable is used to initialise `term-dynamic-complete-functions' in the
shell buffer.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-command-regexp "[^;&|\n]+"
  "*Regexp to match a single command within a pipeline.
This is used for directory tracking and does not do a perfect job.")

(defvar shell-completion-execonly t
  "*If non-nil, use executable files only for completion candidates.
This mirrors the optional behavior of tcsh.

Detecting executability of files may slow command completion considerably.")

(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.")

(defvar shell-pushd-tohome nil
  "*If non-nil, make pushd with no arg behave as \"pushd ~\" (like cd).
This mirrors the optional behavior of tcsh.")

(defvar shell-pushd-dextract nil
  "*If non-nil, make \"pushd +n\" pop the nth dir to the stack top.
This mirrors the optional behavior of tcsh.")

(defvar shell-pushd-dunique nil
  "*If non-nil, make pushd only add unique directories to the stack.
This mirrors the optional behavior of tcsh.")

(defvar shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd.")

;; explicit-shell-file-name nil is in term.el.

(defvar explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "*Args passed to inferior shell by M-x tshell, if the shell is csh.
Value is a list of strings, which may be nil.")

(defvar tshell-input-autoexpand 'history
  "*If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlit).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring.  See also `term-magic-space' and
`term-dynamic-complete'.

This variable supplies a default for `term-input-autoexpand',
for Tshell mode only.")

(defvar tshell-dirstack nil
  "List of directories saved by pushd in this buffer's shell.
Thus, this does not include the shell's current directory.")

(defvar tshell-dirtrackp t
  "Non-nil in a shell buffer means directory tracking is enabled.")

(defvar tshell-last-dir nil
  "Keep track of last directory for ksh `cd -' command.")

(defvar tshell-dirstack-query nil
  "Command used by `tshell-resync-dir' to query the shell.")

(defvar tshell-mode-map nil)
(cond ((not tshell-mode-map)
       (setq tshell-mode-map (copy-keymap term-mode-map))
       (define-key tshell-mode-map "\C-c\C-f" 'tshell-forward-command)
       (define-key tshell-mode-map "\C-c\C-b" 'tshell-backward-command)
       (define-key tshell-mode-map "\t" 'term-dynamic-complete)
       (define-key tshell-mode-map "\M-?"
	 'term-dynamic-list-filename-completions)
;;; XEmacs change [JTL]: We don't have define-key-after   
;;;		         (and we don't need it ...)
;;;       (define-key-after (lookup-key tshell-mode-map [menu-bar completion])
;;;	 [complete-env-variable] '("Complete Env. Variable Name" .
;;;				   tshell-dynamic-complete-environment-variable)
;;;	    'complete-file)
;;;	  (define-key-after (lookup-key tshell-mode-map [menu-bar completion])
;;;	    [expand-directory] '("Expand Directory Reference" .
;;;				 tshell-replace-by-expanded-directory)
;;;	    'complete-expand)
       ))

(defvar tshell-mode-hook '()
  "*Hook for customising Tshell mode.")


;;; Basic Procedures
;;; ===========================================================================
;;;

(defun tshell-mode ()
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies the current line (except
    for the prompt) to the end of the buffer and sends it.
M-x term-send-invisible reads a line of text without echoing it,
    and sends it to the shell.  This is useful for entering passwords.

If you accidentally suspend your process, use \\[term-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
M-x dirs queries the shell and resyncs Emacs' idea of what the current 
    directory stack is.
M-x dirtrack-toggle turns directory tracking on and off.

\\{tshell-mode-map}
Customization: Entry to this mode runs the hooks on `term-mode-hook' and
`tshell-mode-hook' (in that order).  Before each input, the hooks on
`term-input-filter-functions' are run.

Variables `shell-cd-regexp', `shell-pushd-regexp' and `shell-popd-regexp'
are used to match their respective commands, while `shell-pushd-tohome',
`shell-pushd-dextract' and `shell-pushd-dunique' control the behavior of the
relevant command.

Variables `term-completion-autolist', `term-completion-addsuffix',
`term-completion-recexact' and `term-completion-fignore' control the
behavior of file name, command name and variable name completion.  Variable
`shell-completion-execonly' controls the behavior of command name completion.
Variable `tshell-completion-fignore' is used to initialise the value of
`term-completion-fignore'.

Variables `term-input-ring-file-name' and `term-input-autoexpand' control
the initialisation of the input ring history, and history expansion.

Variables `term-output-filter-functions', a hook, and
`term-scroll-to-bottom-on-input' and `term-scroll-to-bottom-on-output'
control whether input and output cause the window to scroll to the end of the
buffer."
  (interactive)
  (term-mode)
  (setq major-mode 'tshell-mode)
  (setq mode-name "Shell")
  (use-local-map tshell-mode-map)
  (setq term-prompt-regexp tshell-prompt-pattern)
  (setq term-completion-fignore tshell-completion-fignore)
  (setq term-delimiter-argument-list tshell-delimiter-argument-list)
  (setq term-dynamic-complete-functions tshell-dynamic-complete-functions)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start term-prompt-regexp)
  (make-local-variable 'tshell-dirstack)
  (setq tshell-dirstack nil)
  (setq tshell-last-dir nil)
  (make-local-variable 'tshell-dirtrackp)
  (setq tshell-dirtrackp t)
  (add-hook 'term-input-filter-functions 'tshell-directory-tracker)
  (setq term-input-autoexpand tshell-input-autoexpand)
  ;; shell-dependent assignments.
  (let ((shell (file-name-nondirectory (car
		 (process-command (get-buffer-process (current-buffer)))))))
    (setq term-input-ring-file-name
	  (or (getenv "HISTFILE")
	      (cond ((string-equal shell "bash") "~/.bash_history")
		    ((string-equal shell "ksh") "~/.sh_history")
		    (t "~/.history"))))
    (if (equal term-input-ring-file-name "/dev/null")
	(setq term-input-ring-file-name nil))
    (setq tshell-dirstack-query
	  (if (string-match "^k?sh$" shell) "pwd" "dirs")))
  (run-hooks 'tshell-mode-hook)
  (term-read-input-ring t))

;;;###autoload
(defun tshell ()
  "Run an inferior shell, with I/O through buffer *shell*.
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

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (if (not (term-check-proc "*shell*"))
      (let* ((prog (or explicit-shell-file-name
		       (getenv "ESHELL")
		       (getenv "SHELL")
		       "/bin/sh"))		     
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	(set-buffer (apply 'make-term "shell" prog
			   (if (file-exists-p startfile) startfile)
			   (if (and xargs-name (boundp xargs-name))
			       (symbol-value xargs-name)
			     '("-i"))))
	(tshell-mode)))
  (switch-to-buffer "*shell*"))

;;; Directory tracking
;;; ===========================================================================
;;; This code provides the tshell mode input sentinel
;;;     TSHELL-DIRECTORY-TRACKER
;;; that tracks cd, pushd, and popd commands issued to the tshell, and
;;; changes the current directory of the tshell buffer accordingly.
;;;
;;; A better mechanism is now available:
;;; The standard term process filter supports a special escape command
;;;     \032 / <cwd> \n
;;; that the inferior can use to tell emacs what the current working
;;; directory is.
;;; All the inferior has to do is something like:
;;;     printf("\032/%s\n", PWD);
;;; Most modern shells can be programmed to emit this string easily.
;;; Hopefully, bash (at least) will be modified to do this automatically.
;;;
;;; So all this horrible directory-tracking machinary is now obsolete,
;;; but is kept at least until the standard GNU shells are modified
;;;
;;; This is basically a fragile hack, although it's more accurate than
;;; the version in Emacs 18's shell.el. It has the following failings:
;;; 1. It doesn't know about the cdpath shell variable.
;;; 2. It cannot infallibly deal with command sequences, though it does well
;;;    with these and with ignoring commands forked in another shell with ()s.
;;; 3. More generally, any complex command is going to throw it. Otherwise,
;;;    you'd have to build an entire shell interpreter in emacs lisp.  Failing
;;;    that, there's no way to catch shell commands where cd's are buried
;;;    inside conditional expressions, aliases, and so forth.
;;;
;;; The whole approach is a crock. Shell aliases mess it up. File sourcing
;;; messes it up. You run other processes under the shell; these each have
;;; separate working directories, and some have commands for manipulating
;;; their w.d.'s (e.g., the lcd command in ftp). Some of these programs have
;;; commands that do *not* affect the current w.d. at all, but look like they
;;; do (e.g., the cd command in ftp).  In shells that allow you job
;;; control, you can switch between jobs, all having different w.d.'s. So
;;; simply saying %3 can shift your w.d..
;;;
;;; The solution is to relax, not stress out about it, and settle for
;;; a hack that works pretty well in typical circumstances. Remember
;;; that a half-assed solution is more in keeping with the spirit of Unix, 
;;; anyway. Blech.

(defun tshell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with M-x dirtrack-toggle.
If emacs gets confused, you can resync with the shell with M-x dirs.

See variables `shell-cd-regexp', `shell-pushd-regexp', and `shell-popd-regexp',
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if tshell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match "^[;\\s ]*" str) ; skip whitespace
			      (match-end 0)))
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (term-arguments (substring str start end) 0 0)
		    arg1 (term-arguments (substring str start end) 1 1))
	      (cond ((eq (string-match shell-popd-regexp cmd) 0)
		     (tshell-process-popd (substitute-in-file-name arg1)))
		    ((eq (string-match shell-pushd-regexp cmd) 0)
		     (tshell-process-pushd (substitute-in-file-name arg1)))
		    ((eq (string-match shell-cd-regexp cmd) 0)
		     (tshell-process-cd (substitute-in-file-name arg1))))
	      (setq start (progn (string-match "[;\\s ]*" str end) ; skip again
				 (match-end 0)))))
	(error "Couldn't cd"))))

;;; popd [+n]
(defun tshell-process-popd (arg)
  (let ((num (or (tshell-extract-num arg) 0)))
    (cond ((and num (= num 0) tshell-dirstack)
	   (cd (car tshell-dirstack))
	   (setq tshell-dirstack (cdr tshell-dirstack))
	   (tshell-dirstack-message))
	  ((and num (> num 0) (<= num (length tshell-dirstack)))
	   (let* ((ds (cons nil tshell-dirstack))
		  (cell (nthcdr (1- num) ds)))
	     (rplacd cell (cdr (cdr cell)))
	     (setq tshell-dirstack (cdr ds))
	     (tshell-dirstack-message)))
	  (t
	   (error "Couldn't popd")))))

;; Return DIR prefixed with term-file-name-prefix as appropriate.
(defun tshell-prefixed-directory-name (dir)
  (if (= (length term-file-name-prefix) 0)
      dir
    (if (file-name-absolute-p dir)
	;; The name is absolute, so prepend the prefix.
	(concat term-file-name-prefix dir)
      ;; For a relative name we assume default-directory already has the prefix.
      (expand-file-name dir))))

;;; cd [dir]
(defun tshell-process-cd (arg)
  (let ((new-dir (cond ((zerop (length arg)) (concat term-file-name-prefix
						     "~"))
		       ((string-equal "-" arg) tshell-last-dir)
		       (t (tshell-prefixed-directory-name arg)))))
    (setq tshell-last-dir default-directory)
    (cd new-dir)
    (tshell-dirstack-message)))

;;; pushd [+n | dir]
(defun tshell-process-pushd (arg)
  (let ((num (tshell-extract-num arg)))
    (cond ((zerop (length arg))
	   ;; no arg -- swap pwd and car of stack unless shell-pushd-tohome
	   (cond (shell-pushd-tohome
		  (shell-process-pushd (concat term-file-name-prefix "~")))
		 (tshell-dirstack
		  (let ((old default-directory))
		    (cd (car tshell-dirstack))
		    (setq tshell-dirstack
			  (cons old (cdr tshell-dirstack)))
		    (tshell-dirstack-message)))
		 (t
		  (message "Directory stack empty."))))
	  ((numberp num)
	   ;; pushd +n
	   (cond ((> num (length tshell-dirstack))
		  (message "Directory stack not that deep."))
		 ((= num 0)
		  (error (message "Couldn't cd.")))
		 (shell-pushd-dextract
		  (let ((dir (nth (1- num) tshell-dirstack)))
		    (tshell-process-popd arg)
		    (tshell-process-pushd default-directory)
		    (cd dir)
		    (tshell-dirstack-message)))
		 (t
		  (let* ((ds (cons default-directory tshell-dirstack))
			 (dslen (length ds))
			 (front (nthcdr num ds))
			 (back (reverse (nthcdr (- dslen num) (reverse ds))))
			 (new-ds (append front back)))
		    (cd (car new-ds))
		    (setq tshell-dirstack (cdr new-ds))
		    (tshell-dirstack-message)))))
	  (t
	   ;; pushd <dir>
	   (let ((old-wd default-directory))
	     (cd (tshell-prefixed-directory-name arg))
	     (if (or (null shell-pushd-dunique)
		     (not (member old-wd tshell-dirstack)))
		 (setq tshell-dirstack (cons old-wd tshell-dirstack)))
	     (tshell-dirstack-message))))))

;; If STR is of the form +n, for n>0, return n. Otherwise, nil.
(defun tshell-extract-num (str)
  (and (string-match "^\\+[1-9][0-9]*$" str)
       (string-to-int str)))


(defun tshell-dirtrack-toggle ()
  "Turn directory tracking on and off in a shell buffer."
  (interactive)
  (setq tshell-dirtrackp (not tshell-dirtrackp))
  (message "Directory tracking %s" (if tshell-dirtrackp "ON" "OFF")))

;;; For your typing convenience:
(defalias 'dirtrack-toggle 'tshell-dirtrack-toggle)


(defun tshell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to 
`tshell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert tshell-dirstack-query) (insert "\n")
    (sit-for 0) ; force redisplay
    (term-send-string proc tshell-dirstack-query) 
    (term-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt)))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    ;; That's the dirlist. grab it & parse it.
    (let* ((dl (buffer-substring (match-beginning 0) (1- (match-end 0))))
	   (dl-len (length dl))
	   (ds '())			; new dir stack
	   (i 0))
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq ds (cons (concat term-file-name-prefix
			       (substring dl (match-beginning 1)
					  (match-end 1)))
		       ds))
	(setq i (match-end 0)))
      (let ((ds (nreverse ds)))
	(condition-case nil
	    (progn (cd (car ds))
		   (setq tshell-dirstack (cdr ds))
		   (tshell-dirstack-message))
	  (error (message "Couldn't cd.")))))))

;;; For your typing convenience:
(defalias 'dirs 'tshell-resync-dirs)


;;; Show the current dirstack on the message line.
;;; Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".
;;; (This isn't necessary if the dirlisting is generated with a simple "dirs".)
;;; All the commands that mung the buffer's dirstack finish by calling
;;; this guy.
(defun tshell-dirstack-message ()
  (let* ((msg "")
	 (ds (cons default-directory tshell-dirstack))
	 (home (expand-file-name (concat term-file-name-prefix "~/")))
	 (homelen (length home)))
    (while ds
      (let ((dir (car ds)))
	(and (>= (length dir) homelen) (string= home (substring dir 0 homelen))
	    (setq dir (concat "~/" (substring dir homelen))))
	;; Strip off term-file-name-prefix if present.
	(and term-file-name-prefix
	     (>= (length dir) (length term-file-name-prefix))
	     (string= term-file-name-prefix
		      (substring dir 0 (length term-file-name-prefix)))
	     (setq dir (substring dir (length term-file-name-prefix)))
	     (setcar ds dir))
	(setq msg (concat msg (directory-file-name dir) " "))
	(setq ds (cdr ds))))
    (message msg)))

(defun tshell-forward-command (&optional arg)
  "Move forward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (end-of-line nil) (point))))
    (if (re-search-forward (concat shell-command-regexp "\\([;&|][\t ]*\\)+")
			   limit 'move arg)
	(skip-syntax-backward " "))))


(defun tshell-backward-command (&optional arg)
  "Move backward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (term-bol nil) (point))))
    (if (> limit (point))
	(save-excursion (beginning-of-line) (setq limit (point))))
    (skip-syntax-backward " " limit)
    (if (re-search-backward
	 (format "[;&|]+[\t ]*\\(%s\\)" shell-command-regexp) limit 'move arg)
	(progn (goto-char (match-beginning 1))
	       (skip-chars-forward ";&|")))))


(defun tshell-dynamic-complete-command ()
  "Dynamically complete the command at point.
This function is similar to `term-dynamic-complete-filename', except that it
searches `exec-path' (minus the trailing emacs library path) for completion
candidates.  Note that this may not be the same as the shell's idea of the
path.

Completion is dependent on the value of `shell-completion-execonly', plus
those that effect file completion.  See `tshell-dynamic-complete-as-command'.

Returns t if successful."
  (interactive)
  (let ((filename (term-match-partial-filename)))
    (if (and filename
	     (save-match-data (not (string-match "[~/]" filename)))
	     (eq (match-beginning 0)
		 (save-excursion (tshell-backward-command 1) (point))))
	(prog2 (message "Completing command name...")
	    (tshell-dynamic-complete-as-command)))))


(defun tshell-dynamic-complete-as-command ()
  "Dynamically complete at point as a command.
See `tshell-dynamic-complete-filename'.  Returns t if successful."
  (let* ((filename (or (term-match-partial-filename) ""))
	 (pathnondir (file-name-nondirectory filename))
	 (paths (cdr (reverse exec-path)))
	 (cwd (file-name-as-directory (expand-file-name default-directory)))
	 (ignored-extensions
	  (and term-completion-fignore
	       (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
			  term-completion-fignore "\\|")))
	 (path "") (comps-in-path ()) (file "") (filepath "") (completions ()))
    ;; Go thru each path in the search path, finding completions.
    (while paths
      (setq path (file-name-as-directory (term-directory (or (car paths) ".")))
	    comps-in-path (and (file-accessible-directory-p path)
			       (file-name-all-completions pathnondir path)))
      ;; Go thru each completion found, to see whether it should be used.
      (while comps-in-path
	(setq file (car comps-in-path)
	      filepath (concat path file))
	(if (and (not (member file completions))
		 (not (and ignored-extensions
			   (string-match ignored-extensions file)))
		 (or (string-equal path cwd)
		     (not (file-directory-p filepath)))
		 (or (null shell-completion-execonly)
		     (file-executable-p filepath)))
	    (setq completions (cons file completions)))
	(setq comps-in-path (cdr comps-in-path)))
      (setq paths (cdr paths)))
    ;; OK, we've got a list of completions.
    (let ((success (let ((term-completion-addsuffix nil))
		     (term-dynamic-simple-complete pathnondir completions))))
      (if (and (memq success '(sole shortest)) term-completion-addsuffix
	       (not (file-directory-p (term-match-partial-filename))))
	  (insert " "))
      success)))


(defun tshell-match-partial-variable ()
  "Return the variable at point, or nil if non is found."
  (save-excursion
    (let ((limit (point)))
      (if (re-search-backward "[^A-Za-z0-9_{}]" nil 'move)
	  (or (looking-at "\\$") (forward-char 1)))
      ;; Anchor the search forwards.
      (if (or (eolp) (looking-at "[^A-Za-z0-9_{}$]"))
	  nil
	(re-search-forward "\\$?{?[A-Za-z0-9_]*}?" limit)
	(buffer-substring (match-beginning 0) (match-end 0))))))


(defun tshell-dynamic-complete-environment-variable ()
  "Dynamically complete the environment variable at point.
Completes if after a variable, i.e., if it starts with a \"$\".
See `tshell-dynamic-complete-as-environment-variable'.

This function is similar to `term-dynamic-complete-filename', except that it
searches `process-environment' for completion candidates.  Note that this may
not be the same as the interpreter's idea of variable names.  The main problem
with this type of completion is that `process-environment' is the environment
which Emacs started with.  Emacs does not track changes to the environment made
by the interpreter.  Perhaps it would be more accurate if this function was
called `tshell-dynamic-complete-process-environment-variable'.

Returns non-nil if successful."
  (interactive)
  (let ((variable (tshell-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
	(prog2 (message "Completing variable name...")
	    (tshell-dynamic-complete-as-environment-variable)))))


(defun tshell-dynamic-complete-as-environment-variable ()
  "Dynamically complete at point as an environment variable.
Used by `tshell-dynamic-complete-environment-variable'.
Uses `term-dynamic-simple-complete'."
  (let* ((var (or (tshell-match-partial-variable) ""))
	 (variable (substring var (or (string-match "[^$({]\\|$" var) 0)))
	 (variables (mapcar (function (lambda (x)
					(substring x 0 (string-match "=" x))))
			    process-environment))
	 (addsuffix term-completion-addsuffix)
	 (term-completion-addsuffix nil)
	 (success (term-dynamic-simple-complete variable variables)))
    (if (memq success '(sole shortest))
	(let* ((var (tshell-match-partial-variable))
	       (variable (substring var (string-match "[^$({]" var)))
	       (protection (cond ((string-match "{" var) "}")
				 ((string-match "(" var) ")")
				 (t "")))
	       (suffix (cond ((null addsuffix) "")
			     ((file-directory-p
			       (term-directory (getenv variable))) "/")
			     (t " "))))
	  (insert protection suffix)))
    success))


(defun tshell-replace-by-expanded-directory ()
  "Expand directory stack reference before point.
Directory stack references are of the form \"=digit\" or \"=-\".
See `default-directory' and `tshell-dirstack'.

Returns t if successful."
  (interactive)
  (if (term-match-partial-filename)
      (save-excursion
	(goto-char (match-beginning 0))
	(let ((stack (cons default-directory tshell-dirstack))
	      (index (cond ((looking-at "=-/?")
			    (length tshell-dirstack))
			   ((looking-at "=\\([0-9]+\\)")
			    (string-to-number
			     (buffer-substring
			      (match-beginning 1) (match-end 1)))))))
	  (cond ((null index)
		 nil)
		((>= index (length stack))
		 (error "Directory stack not that deep."))
		(t
		 (replace-match (file-name-as-directory (nth index stack)) t t)
		 (message "Directory item: %d" index)
		 t))))))

(provide 'tshell)

;;; tshell.el ends here
