;;; startup.el --- process XEmacs shell arguments

;; Copyright (C) 1985-1986, 1990, 1992-1995 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Maintainer: XEmacs
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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: FSF 19.28.

;;; Code:

(defun command-line-do-help (arg)
  "Print this message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (emacs-version))
    (princ "\n\n")
    (cond ((fboundp 'x-create-frame)
	   (princ "XEmacs")
	   (princ " accepts all standard X Toolkit command line options.\
  In addition,\nthe "))
	  (t (princ "The ")))
    (princ "following options are processed in the order encountered:\n\n")
    (let ((l command-switch-alist)
	  (insert (function (lambda (&rest x)
			      (princ "  ")
			      (let ((len 2))
				(while x
				  (princ (car x))
				  (setq len (+ len (length (car x))))
				  (setq x (cdr x)))
				(if (>= len 24)
				    (progn (terpri) (setq len 0)))
				(while (< len 24)
				  (princ " ")
				  (setq len (1+ len))))))))
      (while l
        (let ((name (car (car l)))
              (fn (cdr (car l)))
	      doc arg cons)
	  (cond
	   ((and (symbolp fn) (get fn 'undocumented)) nil)
	   (t
	    (setq doc (documentation fn))
	    (if (member doc '(nil "")) (setq doc "(undocumented)"))
	    (cond ((string-match "\n\\(<.*>\\)\n?\\'" doc)
		   ;; Doc of the form "The frobber switch\n<arg1> <arg2>"
		   (setq arg (substring doc (match-beginning 1) (match-end 1))
			 doc (substring doc 0 (match-beginning 0))))
		  ((string-match "\n+\\'" doc)
		   (setq doc (substring doc 0 (match-beginning 0)))))
	    (if (and (setq cons (rassq fn command-switch-alist))
		     (not (eq cons (car l))))
		(setq doc (format "Same as %s." (car cons))))
	    (if arg
		(funcall insert name " " arg)
	      (funcall insert name))
	    (princ doc)
	    (terpri))))
        (setq l (cdr l))))
    (princ "\
  +N <file>             Start displaying <file> at line N.

These options are processed only if they appear before all other options:

  -t <device>           Use TTY <device> instead of the terminal for input
                        and output.  This implies the -nw option.
  -batch                Execute noninteractively (messages go to stderr).
                        This option must be first in the list after -t.
  -nw                   Inhibit the use of any window-system-specific
                        display code: use the current tty.
  -debug-init           Enter the debugger if an error in the init file occurs.
  -unmapped             Do not map the initial frame.
  -no-site-file         Do not load the site-specific init file (site-start.el).
  -no-init-file         Do not load the user-specific init file (~/.emacs).
  -q                    Same as -no-init-file.
  -user <user>          Load user's init file instead of your own.
  -u <user>             Same as -user.")

    (princ "

Anything else is considered a file name, and is placed into a buffer for
editing.

XEmacs has an online tutorial and manuals.  Type ^Ht (Control-h t) after
starting XEmacs to run the tutorial.  Type ^Hi to enter the manual browser.\n")
    (kill-emacs 0)
    ))

;;; -batch, -t, and -nw are processed by main() in emacs.c and are 
;;; never seen by lisp code.

;;; -version and -help are special-cased as well: they imply -batch,
;;; but are left on the list for lisp code to process.


(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst startup-message-timeout 120)

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

;; #### FSFmacs randomness
;(defconst inhibit-startup-echo-area-message nil
;  "*Non-nil inhibits the initial startup echo area message.
;Inhibition takes effect only if your `.emacs' file contains
;a line of this form:
; (setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\")
;If your `.emacs' file is byte-compiled, use the following form instead:
; (eval '(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
;Thus, someone else using a copy of your `.emacs' file will see
;the startup message unless he personally acts to inhibit it.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

(defvar command-line-args-left nil
  "List of command-line args not yet processed.") ; bound by `command-line'

(defvar command-line-default-directory nil
  "Default directory to use for command line arguments.
This is normally copied from `default-directory' when XEmacs starts.")

(defvar before-init-hook nil
  "Functions to call after handling urgent options but before init files.
The frame system uses this to open frames to display messages while
XEmacs loads the user's initialization file.")

(defvar after-init-hook nil
  "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "Functions to be called after loading terminal-specific Lisp code.
See `run-hooks'.  This variable exists for users to set,
so as to override the definitions made by the terminal-specific file.
XEmacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment value TERM.")

(defvar window-setup-hook nil
  "Normal hook run to initialize window system display.
XEmacs runs this hook after processing the command line arguments and loading
the user's init file.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar init-file-user nil
  "Identity of user whose `.emacs' file is or was read.
The value is nil if no init file is being used; otherwise, it may be either
the null string, meaning that the init file was taken from the user that
originally logged in, or it may be a string containing a user's name.

In either of the latter cases, `(concat \"~\" init-file-user \"/\")'
evaluates to the name of the directory where the `.emacs' file was
looked for.

Setting `init-file-user' does not prevent Emacs from loading
`site-start.el'.  The only way to do that is to use `--no-site-file'.")

;; #### called `site-run-file' in FSFmacs

(defvar site-start-file (purecopy "site-start")
  "File containing site-wide run-time initializations.
This file is loaded at run-time before `~/.emacs'.  It contains inits
that need to be in place for the entire site, but which, due to their
higher incidence of change, don't make sense to load into XEmacs'
dumped image.  Thus, the run-time load order is: 1. file described in
this variable, if non-nil; 2. `~/.emacs'; 3. `default.el'.

Don't use the `site-start.el' file for things some users may not like.
Put them in `default.el' instead, so that users can more easily
override them.  Users can prevent loading `default.el' with the `-q'
option or by setting `inhibit-default-init' in their own init files,
but inhibiting `site-start.el' requires `--no-site-file', which
is less convenient.")

;(defconst iso-8859-1-locale-regexp "8859[-_]?1"
;  "Regexp that specifies when to enable the ISO 8859-1 character set.
;We do that if this regexp matches the locale name
;specified by the LC_ALL, LC_CTYPE and LANG environment variables.")

(defvar mail-host-address nil
  "*Name of this machine, for purposes of naming users.")

(defvar user-mail-address nil
  "*Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'.")

(defvar auto-save-list-file-prefix "~/.saves-"
  "Prefix for generating auto-save-list-file-name.
Emacs's pid and the system name will be appended to
this prefix to create a unique file name.")

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar init-file-loaded nil
  "True after the user's init file has been loaded (or suppressed with -q).
This will be true when `after-init-hook' is run and at all times
after, and will not be true at any time before.")

(defvar initial-frame-unmapped-p nil)


;;; default switches
;;; Note: these doc strings are semi-magical.

(defun command-line-do-funcall (arg)
  "Invoke the named lisp function with no arguments.
<function>"
  (let ((fn (intern (car command-line-args-left))))
    (setq command-line-args-left (cdr command-line-args-left))
    (funcall fn)))
(fset 'command-line-do-funcall-1 'command-line-do-funcall)
(put 'command-line-do-funcall-1 'undocumented t)

(defun command-line-do-eval (arg)
  "Evaluate the lisp form.  Quote it carefully.
<form>"
  (let ((form (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (eval (read form))))

(defun command-line-do-load (arg)
  "Load the named file of Lisp code into XEmacs.
<file>"
  (let ((file (car command-line-args-left)))
    ;; Take file from default dir if it exists there;
    ;; otherwise let `load' search for it.
    (if (file-exists-p (expand-file-name file))
	(setq file (expand-file-name file)))
    (load file nil t))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-do-insert (arg)
  "Insert file into the current buffer.
<file>"
  (insert-file-contents (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-do-kill (arg)
  "Exit XEmacs."
  (kill-emacs t))

(defun command-line-do-version (arg)
  "Print version info and exit."
  (princ (concat (emacs-version) "\n") 'external-debugging-output)
  (kill-emacs 0))

(setq command-switch-alist
      (purecopy
       '(("-help"	. command-line-do-help)
	 ("-version"	. command-line-do-version)
	 ("-funcall"	. command-line-do-funcall)
         ("-f"		. command-line-do-funcall)
	 ("-e"		. command-line-do-funcall-1)
	 ("-eval"	. command-line-do-eval)
	 ("-load"	. command-line-do-load)
	 ("-l"		. command-line-do-load)
	 ("-insert"	. command-line-do-insert)
	 ("-i"		. command-line-do-insert)
	 ("-kill"	. command-line-do-kill)
	 ;; Options like +35 are handled specially.
	 ;; Window-system, site, or package-specific code might add to this.
	 ;; X11 handles its options by letting Xt remove args from this list.
	 )))

;;; Processing the command line and loading various init files

(defun early-error-handler (&rest debugger-args)
  "You should probably not be using this."
  ;; Used as the debugger during XEmacs initialization; if an error occurs,
  ;; print some diagnostics, and kill XEmacs.

  ;; output the contents of the warning buffer, since it won't be seen
  ;; otherwise.
  ;; #### kludge!  The call to Feval forces the pending warnings to
  ;; get output.  There definitely needs to be a better way.
  (let ((buffer (eval (get-buffer-create "*Warnings*"))))
    (princ (buffer-substring (point-min buffer) (point-max buffer) buffer)
	   'external-debugging-output))

  (let ((string "Initialization error")
	(error (nth 1 debugger-args))
	(debug-on-error nil)
	(stream 'external-debugging-output))
    (if (null error)
	(princ string stream)
      (princ (concat "\n" string ": ") stream)
      (condition-case ()
	  (display-error error stream)
	(error (princ "<<< error printing error message >>>" stream)))
      (princ "\n" stream)
      (if (memq (car-safe error) '(void-function void-variable))
	  (princ "
	This probably means that XEmacs is picking up an old version of
	the lisp library, or that some .elc files are not up-to-date.\n"
		 stream)))
    (if (not suppress-early-error-handler-backtrace)
	(let ((print-length 1000)
	      (print-level 1000)
	      (print-escape-newlines t)
	      (print-readably nil))
	  (if (getenv "EMACSLOADPATH")
	      (princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
		     stream))
	  (princ (format "\nexec-directory is %S" exec-directory) stream)
	  (princ (format "\ndata-directory is %S" data-directory) stream)
	  (princ (format "\ndoc-directory is %S" doc-directory) stream)
	  (princ (format "\nload-path is %S" load-path) stream)
	  (princ "\n\n" stream)))
    (if (not suppress-early-error-handler-backtrace)
	(backtrace stream t)))
  (kill-emacs -1))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; Canonicalize HOME (PWD is canonicalized by init_buffer in buffer.c)
    (if (not (eq system-type 'vax-vms))
        (let ((value (getenv "HOME")))
          (if (and value
                   (< (length value) (length default-directory))
                   (equal (file-attributes default-directory)
                          (file-attributes value)))
              (setq default-directory (file-name-as-directory value)))))
    (setq default-directory (abbreviate-file-name default-directory))
    (initialize-xemacs-paths)
    (unwind-protect
	(command-line)
      ;; Do this again, in case .emacs defined more abbreviations.
      (setq default-directory (abbreviate-file-name default-directory))
      ;; Specify the file for recording all the auto save files of
      ;; this session.  This is used by recover-session.
      (setq auto-save-list-file-name
	    (expand-file-name
	     (format "%s%d-%s"
		     auto-save-list-file-prefix
		     (emacs-pid)
		     (system-name))))
      (run-hooks 'emacs-startup-hook)
      (and term-setup-hook
	   (run-hooks 'term-setup-hook))
      (setq term-setup-hook nil)
;      ;; Modify the initial frame based on what .emacs puts into
;      ;; ...-frame-alist.
      (frame-notice-user-settings)
;      ;;####FSFmacs junk
;      ;; Now we know the user's default font, so add it to the menu.
;      (if (fboundp 'font-menu-add-default)
;	  (font-menu-add-default))
      (and window-setup-hook
	   (run-hooks 'window-setup-hook))
      (setq window-setup-hook nil))
      ;;####FSFmacs junk
;      (or menubar-bindings-done
;	  (precompute-menubar-bindings))
    ))

;;####FSFmacs junk
;;; Precompute the keyboard equivalents in the menu bar items.
;(defun precompute-menubar-bindings ()
;  (if (eq window-system 'x)
;      (let ((submap (lookup-key global-map [menu-bar])))
;	(while submap
;	  (and (consp (car submap))
;	       (symbolp (car (car submap)))
;	       (stringp (car-safe (cdr (car submap))))
;	       (keymapp (cdr (cdr (car submap))))
;	       (x-popup-menu nil (cdr (cdr (car submap)))))
;	  (setq submap (cdr submap))))))

(defun command-line-early ()
  ;; This processes those switches which need to be processed before
  ;; starting up the window system.

  (setq command-line-default-directory default-directory)

  ;; See if we should import version-control from the environment variable.
  (let ((vc (getenv "VERSION_CONTROL")))
    (cond ((eq vc nil))			;don't do anything if not set
	  ((or (string= vc "t")
	       (string= vc "numbered"))
	   (setq version-control t))
	  ((or (string= vc "nil")
	       (string= vc "existing"))
	   (setq version-control nil))
	  ((or (string= vc "never")
	       (string= vc "simple"))
	   (setq version-control 'never))))

;;####FSFmacs
;  (if (let ((ctype
;	     ;; Use the first of these three envvars that has a nonempty value.
;	     (or (let ((string (getenv "LC_ALL")))
;		   (and (not (equal string "")) string))
;		 (let ((string (getenv "LC_CTYPE")))
;		   (and (not (equal string "")) string))
;		 (let ((string (getenv "LANG")))
;		   (and (not (equal string "")) string)))))
;	(and ctype
;	     (string-match iso-8859-1-locale-regexp ctype)))
;      (progn 
;	(standard-display-european t)
;	(require 'iso-syntax)))

  (let ((done nil))
    ;; Figure out which user's init file to load,
    ;; either from the environment or from the options.
    (setq init-file-user (if (noninteractive) nil (user-login-name)))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init-file-user (string= init-file-user (user-real-login-name))
	 (setq init-file-user ""))

    (while (and (not done) command-line-args-left)
      (let ((argi (car command-line-args-left)))
	(cond ((or (string-equal argi "-q")
		   (string-equal argi "-no-init-file"))
	       (setq init-file-user nil
		     command-line-args-left (cdr command-line-args-left)))
	      ((string-equal argi "-no-site-file")
	       (setq site-start-file nil
		     command-line-args-left (cdr command-line-args-left)))
	      ((or (string-equal argi "-u")
		   (string-equal argi "-user"))
	       (setq command-line-args-left (cdr command-line-args-left)
		     init-file-user (car command-line-args-left)
		     command-line-args-left (cdr command-line-args-left)))
              ((string-equal argi "-debug-init")
               (setq init-file-debug t
                     command-line-args-left (cdr command-line-args-left)))
              ((string-equal argi "-unmapped")
               (setq initial-frame-unmapped-p t
                     command-line-args-left (cdr command-line-args-left)))
 	      (t (setq done t)))))))


(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))

    (let ((debugger 'early-error-handler)
	  (debug-on-error t))
      (set-default-load-path)

      ;; Process magic command-line switches like -q and -u.  Do this
      ;; before creating the first frame because some of these switches
      ;; may affect that.  I think it's ok to do this before establishing
      ;; the X connection, and maybe someday things like -nw can be
      ;; handled here instead of down in C.
      (command-line-early)

      ;; Setup the toolbar icon directory
      (if (featurep 'toolbar)
          (init-toolbar-location))

      ;; Initialize the built-in glyphs and the default specifier
      ;; lists
      (if (not noninteractive)
	  (init-glyphs))

      ;; Run the window system's init function.  tty is considered to be
      ;; a type of window system for this purpose.  This creates the
      ;; initial (non stdio) device.
      (if (and initial-window-system (not noninteractive))
	  (funcall (intern (concat "init-"
				   (symbol-name initial-window-system)
				   "-win"))))

      ;; When not in batch mode, this creates the first visible frame,
      ;; and deletes the stdio device.
      (frame-initialize))

    ;;
    ;; We have normality, I repeat, we have normality.  Anything you still
    ;; can't cope with is therefore your own problem.  (And we don't need
    ;; to kill XEmacs for it.)
    ;;

    ;;; Load init files.
    (load-init-file)
    
    ;; If *scratch* exists and init file didn't change its mode, initialize it.
    (if (get-buffer "*scratch*")
	(save-excursion
	  (set-buffer "*scratch*")
	  (if (eq major-mode 'fundamental-mode)
	      (funcall initial-major-mode))))

    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    ;; Note that for any TTY's opened subsequently, the TTY init
    ;; code will run this.
    (if (and (eq 'tty (console-type))
	     (not (noninteractive)))
	(load-terminal-library))

    ;; Process the remaining args.
    (command-line-1)

    ;; it was turned on by default so that the warnings don't get displayed
    ;; until after the splash screen.
    (setq inhibit-warning-display nil)
    ;; If -batch, terminate after processing the command options.
    (if (noninteractive) (kill-emacs t))))

(defun load-terminal-library ()	      
  (and term-file-prefix
       (let ((term (getenv "TERM"))
	     hyphend)
	 (while (and term
		     (not (load (concat term-file-prefix term) t t)))
	   ;; Strip off last hyphen and what follows, then try again
	   (if (setq hyphend (string-match "[-_][^-_]+\\'" term))
	       (setq term (substring term 0 hyphend))
	     (setq term nil))))))

(defun load-user-init-file (init-file-user)
  ;; This function actually reads the init files.
  (if init-file-user
      (progn
	(setq user-init-file 
	      (cond 
	       ((eq system-type 'ms-dos)
		(concat "~" init-file-user "/_emacs"))
	       ((eq system-type 'vax-vms) 
		"sys$login:.emacs")
	       (t 
		(concat "~" init-file-user "/.emacs"))))
	(load user-init-file t t t)
	(or inhibit-default-init
	    (let ((inhibit-startup-message nil))
	      ;; Users are supposed to be told their rights.
	      ;; (Plus how to get help and how to undo.)
	      ;; Don't you dare turn this off for anyone
	      ;; except yourself.
	      (load "default" t t))))))

;;; Load user's init file and default ones.
(defun load-init-file ()
  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (if site-start-file
      (load site-start-file t t))

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  (let (debug-on-error-from-init-file
	debug-on-error-should-be-set
	(debug-on-error-initial
	 (if (eq init-file-debug t) 'startup init-file-debug)))
    (let ((debug-on-error debug-on-error-initial))
      (if init-file-debug
	  ;; Do this without a condition-case if the user wants to debug.
	  (load-user-init-file init-file-user)
	(condition-case error
	    (progn
	      (load-user-init-file init-file-user)
	      (setq init-file-had-error nil))
          (error
           (message "Error in init file: ")
           (display-error error nil)
	   (setq init-file-had-error t))))
      ;; If we can tell that the init file altered debug-on-error,
      ;; arrange to preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
	  (setq debug-on-error-should-be-set t
		debug-on-error-from-init-file debug-on-error)))
    (if debug-on-error-should-be-set
	(setq debug-on-error debug-on-error-from-init-file)))

  (setq init-file-loaded t)

  ;; Do this here in case the init file sets mail-host-address.
  (or user-mail-address
      (setq user-mail-address (concat (user-login-name) "@"
				      (or mail-host-address
					  (system-name)))))

  (run-hooks 'after-init-hook)
  nil)

(defun load-options-file (filename)
  "Load the file of saved options (from the Options menu) called FILENAME.
Currently this does nothing but call `load', but it might be redefined
in the future to support automatically converting older options files to
a new format, when variables have changed, etc."
  (load filename))

(defun command-line-1 ()
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not (noninteractive))
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))

	     ;; If there are no switches to process, run the term-setup-hook
	     ;; before displaying the copyright notice; there may be some need
	     ;; to do it before doing any output.  If we're not going to
	     ;; display a copyright notice (because other options are present)
	     ;; then this is run after those options are processed.
	     (run-hooks 'term-setup-hook)
	     ;; Don't let the hook be run twice.
	     (setq term-setup-hook nil)

             (let ((timeout nil))
               (unwind-protect
                    ;; Guts of with-timeout
                    (catch 'timeout
                      (setq timeout (add-timeout startup-message-timeout
                                                 #'(lambda (ignore)
                                                     (condition-case nil
                                                         (throw 'timeout t)
                                                       (error nil)))
                                                 nil))
                      (startup-splash-frame)
                      (or nil ;; (pos-visible-in-window-p (point-min))
                          (goto-char (point-min)))
                      (sit-for 0)
                      (setq unread-command-event (next-command-event)))
                 (if timeout (disable-timeout timeout))
                 (save-excursion
                   ;; In case the XEmacs server has already selected
                   ;; another buffer, erase the one our message is in.
		   (progn
		     (set-buffer (get-buffer "*scratch*"))
		     (erase-buffer)
		     (set-buffer-modified-p nil)))))))
    (let ((dir command-line-default-directory)
	  (file-count 0)
	  first-file-buffer
	  (line nil))
      (while command-line-args-left
	(let ((argi (car command-line-args-left))
	      tem)
	  (setq command-line-args-left (cdr command-line-args-left))
	  (or (cond (line 
		     nil)
		    ((setq tem (or (assoc argi command-switch-alist)
				   (and (string-match "\\`--" argi)
					(assoc (substring argi 1)
					       command-switch-alist))))
		     (funcall (cdr tem) argi)
		     t)
		    ((string-match "\\`\\+[0-9]+\\'" argi)
		     (setq line (string-to-int argi))
		     t)
		    ((or (equal argi "-") (equal argi "--"))
		     ;; "- file" means don't treat "file" as a switch
		     ;;  ("+0 file" has the same effect; "-" added
		     ;;   for unixoidiality).
		     ;; This is worthless; the `unixoid' way is "./file". -jwz
		     (setq line 0))
		    (t
		     nil))
	      (progn
		(setq file-count (1+ file-count))
		(setq argi (expand-file-name argi dir))
		(if (= file-count 1)
		    (setq first-file-buffer (progn (find-file argi)
						   (current-buffer)))
		  (if noninteractive
		      (find-file argi)
		    (find-file-other-window argi)))
		(or (null line)
		    (zerop line)
		    (goto-line line))
		(setq line 0)))))
      ;; If 3 or more files visited, and not all visible,
      ;; show user what they all are.
      (if (and (not noninteractive)
	       (> file-count 2))
	  (or (get-buffer-window first-file-buffer)
	      (progn (other-window 1)
		     (buffer-menu nil)))))))

(defvar startup-presentation-hack-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'startup-presentation-hack-keymap)
    (define-key map '[button1] 'startup-presentation-hack)
    (define-key map '[button2] 'startup-presentation-hack)
    map)
  "Putting yesterday in the future tomorrow.")

(defun startup-presentation-hack ()
  (interactive)
  (let ((e last-command-event))
    (and (button-press-event-p e)
         (setq e (extent-at (event-point e)
                            (event-buffer e)
                            'startup-presentation-hack))
         (setq e (extent-property e 'startup-presentation-hack))
         (if (consp e)
             (apply (car e) (cdr e))
           (progn
             (while (keymapp (indirect-function e))
               (let ((map e)
                     (overriding-local-map (indirect-function e)))
                 (setq e (read-key-sequence
                          (let ((p (keymap-prompt map t)))
                            (cond ((symbolp map)
                                   (if p 
                                       (format "%s %s " map p)
                                       (format "%s " map p)))
                                  (p)
                                  (t
                                   (prin1-to-string map))))))
                 (if (and (button-release-event-p (elt e 0))
                          (null (key-binding e)))
                     (setq e map)       ; try again
                     (setq e (key-binding e)))))
             (call-interactively e))))))

(defun startup-presentation-hack-help (e)
  (setq e (extent-property e 'startup-presentation-hack))
  (if (consp e)
      (format "Evaluate %S" e)
      (symbol-name e)))

(defun splash-frame-present-hack (e v)
;  (set-extent-property e 'mouse-face 'highlight)
;  (set-extent-property e 'keymap
;                       startup-presentation-hack-keymap)
;  (set-extent-property e 'startup-presentation-hack v)
;  (set-extent-property e 'help-echo
;                       'startup-presentation-hack-help))
  )

(defun splash-frame-present (l)
  (cond ((stringp l)
         (insert l))
        ((eq (car-safe l) 'face)
         ;; (face name string)
         (let ((p (point)))
           (splash-frame-present (elt l 2))
           (if (fboundp 'set-extent-face)
               (set-extent-face (make-extent p (point))
                                (elt l 1)))))
        ((eq (car-safe l) 'key)
         (let* ((c (elt l 1))
                (p (point))
                (k (where-is-internal c nil t)))
           (insert (if k (key-description k)
                       (format "M-x %s" c)))
           (if (fboundp 'set-extent-face)
               (let ((e (make-extent p (point))))
                 (set-extent-face e 'bold)
                 (splash-frame-present-hack e c)))))
        ((eq (car-safe l) 'funcall)
         ;; (funcall (fun . args) string)
         (let ((p (point)))
           (splash-frame-present (elt l 2))
           (if (fboundp 'set-extent-face)
               (splash-frame-present-hack (make-extent p (point))
                                           (elt l 1)))))
	((consp l)
	 (mapcar 'splash-frame-present l))
        (t
         (error "WTF!?"))))

(defun startup-center-spaces (glyph)
  ;; Return the number of spaces to insert in order to center
  ;; the given glyph (may be a string or a pixmap).
  ;; Assume spaces are as wide as avg-pixwidth.  
  ;; Won't be quite right for proportional fonts, but it's the best we can do.
  ;; Maybe the new redisplay will export something a glyph-width function.
  ;;; #### Yes, there is a glyph-width function but it isn't quite what
  ;;; #### this was expecting.  Or is it?
  ;; (An alternate way to get avg-pixwidth would be to use x-font-properties
  ;; and calculate RESOLUTION_X * AVERAGE_WIDTH / 722.7, but it's no better.)

  ;; This function is used in about.el too.
  (let* ((avg-pixwidth     (round (/ (frame-pixel-width) (frame-width))))
	 (fill-area-width  (* avg-pixwidth (- fill-column left-margin)))
	 (glyph-pixwidth   (cond ((stringp glyph) 
				  (* avg-pixwidth (length glyph)))
				 ;; #### the pixmap option should be removed
				 ;;((pixmapp glyph)
				 ;; (pixmap-width glyph))
				 ((glyphp glyph)
				  (glyph-width glyph))
				 (t
				  (error "startup-center-spaces: bad arg")))))
    (+ left-margin
       (round (/ (/ (- fill-area-width glyph-pixwidth) 2) avg-pixwidth)))))

(defun startup-splash-frame ()
  (let ((p (point)))
    (if (eq 'x (console-type (selected-console))) (insert "\n"))
    (indent-to (startup-center-spaces xemacs-logo))
    (set-extent-begin-glyph (make-extent (point) (point)) xemacs-logo)
    (if (eq 'x (console-type (selected-console)))
	(insert "\n\n")
      (insert "\n"))
    (splash-frame-present-hack (make-extent p (point)) 'about-xemacs))

  (insert "\n" (emacs-version) "\n")
  (let ((after-change-functions nil) ; no font-lock, thank you
	(l `((face bold-italic
"Copyright (C) 1985-1996 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1996 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois
Copyright (C) 1995-1996 Ben Wing.")
	     ,@(if (featurep 'sparcworks) '(
"\n\nSunSoft provides support for the SPARCworks/XEmacs EOS integration package
only.  All other XEmacs packages are provided to you \"AS IS\"."))
	     "\n\nType " (key describe-no-warranty) " to refer to the GPL "
	     "Version 2, dated June 1991, for full details.\n"
	     "You may give out copies of XEmacs; type "
	     (key describe-copying) " to see the conditions.\n"
	     "Type " (key describe-distribution)
	     " for information on getting the latest version."

             "\n\nType " (key help-command) " for help; "
             (key advertised-undo)
             " to undo changes.  (`C-' means use the CTRL key.)\n"
             "To get out of XEmacs, type " (key save-buffers-kill-emacs) ".\n"
             "Type " (key help-with-tutorial)
             " for a tutorial on using XEmacs.\n"
             "Type " (key info) " to enter Info, "
             "which you can use to read documentation.\n\n"
	     (face (bold red)
		   (
"For tips and answers to frequently asked questions, see the XEmacs FAQ.\n"
		    "(It's on the Help menu, or type "
		    (key xemacs-local-faq) " [a capital F!].)")))))
    (while l
      (splash-frame-present (car l))
      (setq l (cdr l))))
;  (let ((present-file
;         #'(lambda (f)
;             (splash-frame-present
;	      (list 'funcall
;		    (list 'find-file-other-window
;			  (expand-file-name f data-directory))
;		    f)))))
;    (insert "For customization examples, see the files ")
;    (funcall present-file "sample.emacs")
;    (insert " and ")
;    (funcall present-file "sample.Xdefaults")
;    (insert (format "\nin the directory %s." data-directory)))
  (set-buffer-modified-p nil))

;;;; Computing the default load-path, etc.
;;;
;;; This stuff is a complete mess and isn't nearly as general as it 
;;; thinks it is.  It should be rethunk.  In particular, too much logic
;;; is duplicated between the code that looks around for the various
;;; directories, and the code which suggests where to create the various
;;; directories once it decides they are missing.

;;; The source directory has this layout:
;;;
;;;    BUILD_ROOT/src/xemacs*			  argv[0]
;;;    BUILD_ROOT/xemacs*			  argv[0], possibly
;;;    BUILD_ROOT/lisp/
;;;    BUILD_ROOT/etc/				  data-directory
;;;    BUILD_ROOT/info/
;;;    BUILD_ROOT/lib-src/			  exec-directory, doc-directory
;;;    BUILD_ROOT/lock/
;;;
;;; The default tree created by "make install" has this layout:
;;;
;;;    PREFIX/bin/xemacs*	  		argv[0]
;;;    PREFIX/lib/xemacs-VERSION/lisp/
;;;    PREFIX/lib/xemacs-VERSION/etc/		  data-directory
;;;    PREFIX/lib/xemacs-VERSION/info/
;;;    PREFIX/lib/xemacs-VERSION/CONFIGURATION/	  exec-directory, doc-directory
;;;    PREFIX/lib/xemacs/lock/
;;;    PREFIX/lib/xemacs/site-lisp/
;;;
;;; The binary packages we ship have that layout, except that argv[0] has
;;; been moved one level deeper under the bin directory:
;;;
;;;    PREFIX/bin/CONFIGURATION/xemacs*
;;;
;;; The following code has to deal with at least the above three situations,
;;; and it should be possible for it to deal with more.  Though perhaps that
;;; does cover it all?  The trick is, when something is missing, realizing
;;; which of those three layouts is mostly in place, so that we can suggest
;;; the right directories in the error message.


;; extremely low-tech debugging, since this happens so early in startup.
;(or (fboundp 'orig-file-directory-p)
;    (fset 'orig-file-directory-p (symbol-function 'file-directory-p)))
;(defun file-directory-p (path)
;  (send-string-to-terminal (format "PROBING %S" path))
;  (let ((v (orig-file-directory-p path)))
;    (send-string-to-terminal (format " -> %S\n" v))
;    v))

(defun startup-make-version-dir ()
  (let ((version (and (string-match "\\`[^0-9]*\\([0-9]+\\.[0-9]+\\)"
				    emacs-version)
		      (substring emacs-version
				 (match-beginning 1) (match-end 1)))))
    (if (string-match "(beta *\\([0-9]+\\))" emacs-version)
	(setq version (concat version "-b"
			      (substring emacs-version (match-beginning 1)
					 (match-end 1)))))
    (if (string-match "(alpha *\\([0-9]+\\))" emacs-version)
	(setq version (concat version "-a"
			      (substring emacs-version (match-beginning 1)
					 (match-end 1)))))
    (concat "lib/xemacs-" version)))

(defun find-emacs-root-internal-1 (path lisp-p)
  (let ((dir (file-name-directory path)))
    (or
     ;;
     ;; If this directory is a plausible root of the XEmacs tree, return it.
     ;;
     (and (or (not lisp-p)
	      (file-directory-p (expand-file-name "lisp/prim" dir)))
	  (or (file-directory-p (expand-file-name "lib-src" dir))
	      (file-directory-p (expand-file-name system-configuration dir)))
	  dir)
     ;;
     ;; If the parent of this directory is a plausible root, use it.
     ;; (But don't do so recursively!)
     ;;
     (and (or (not lisp-p)
	      (file-directory-p (expand-file-name "../lisp/prim" dir)))
	  (or (file-directory-p (expand-file-name
				 (format "../%s" system-configuration)
				 dir))
	      (file-directory-p (expand-file-name "../lib-src" dir)))
	  (expand-file-name "../" dir))

     ;; 
     ;; (--run-in-place) Same thing, but from one directory level deeper.
     ;;
     (and (or (not lisp-p)
	      (file-directory-p (expand-file-name "../../lisp/prim" dir)))
	  (or (file-directory-p (expand-file-name
				 (format "../%s" system-configuration)
				 dir))
	      (file-directory-p 
	       (expand-file-name 
		(format "../../lib-src/%s" system-configuration) dir)))
	  (expand-file-name "../.." dir))

     ;; If ../lib/xemacs-<version> exists check it.
     ;; This is of the form "xemacs-19.10/" or "xemacs-19.10-b7/".
     ;;
     (let ((ver-dir (concat "../" (startup-make-version-dir))))
       (and (or (not lisp-p)
		(file-directory-p (expand-file-name
				   (format "%s/lisp/prim" ver-dir)
				   dir)))
	    (or (file-directory-p (expand-file-name
				   (format "%s/%s" ver-dir
					   system-configuration)
				   dir))
		(file-directory-p (expand-file-name
				   (format "%s/lib-src" ver-dir)
				   dir)))
	    (expand-file-name (file-name-as-directory ver-dir) dir)))
     ;;
     ;; Same thing, but one higher: ../../lib/xemacs-<version>.
     ;;
     (let ((ver-dir (concat "../../" (startup-make-version-dir))))
       (and (or (not lisp-p)
		(file-directory-p (expand-file-name
				   (format "%s/lisp/prim" ver-dir)
				   dir)))
	    (or (file-directory-p (expand-file-name
				   (format "%s/%s" ver-dir
					   system-configuration)
				   dir))
		(file-directory-p (expand-file-name
				   (format "%s/lib-src" ver-dir)
				   dir)))
	    (expand-file-name (file-name-as-directory ver-dir) dir)))
     ;;
     ;; If that doesn't work, and the XEmacs executable is a symlink, then
     ;; chase the link and try again there.
     ;;
     (and (setq path (file-symlink-p path))
	  (find-emacs-root-internal-1 (expand-file-name path dir) lisp-p))
     ;;
     ;; Otherwise, this directory just doesn't cut it.
     ;; Some bozos think they can use the 18.59 lisp directory with 19.*.
     ;; This is because they're not using their brains.  But it might be
     ;; nice to notice that that is happening and point them in the
     ;; general direction of a clue.
     ;;
     nil)))

(defun find-emacs-root-internal (path)
;;  (send-string-to-terminal (format "FINDING ROOT FOR %S\n" path))
  ;; first look for lisp/prim and lib-src; then just look for lib-src.
  ;; XEmacs can run (kind of) if the lisp directory is omitted, which
  ;; some people might want to do for space reasons.
  (or (find-emacs-root-internal-1 path t)
      (find-emacs-root-internal-1 path nil)
      ;; If we don't succeed we are going to crash and burn for sure.
      ;; Try some paths relative to prefix-directory if it isn't nil.
      ;; This is definitely necessary in cases such as when we're used
      ;; as a login shell since we can't determine the invocation
      ;; directory in that case.
      (find-emacs-root-internal-1
       (format "%s/bin/%s" prefix-directory invocation-name) t)
      (find-emacs-root-internal-1
       (format "%s/bin/%s" prefix-directory invocation-name) nil)
      (find-emacs-root-internal-1
       (format "%s/lib/%s" prefix-directory invocation-name) t)
      (find-emacs-root-internal-1
       (format "%s/lib/%s" prefix-directory invocation-name) nil)
      ))

(defun set-default-load-path ()
  ;; XEmacs -- Steven Baur says invocation directory is nil if you
  ;; try to use XEmacs as a login shell.
  (or invocation-directory (setq invocation-directory default-directory))
  (setq invocation-directory
	;; don't let /tmp_mnt/... get into the load-path or exec-path.
	(abbreviate-file-name invocation-directory))

  ;; #### FSFmacs recognizes environment vars EMACSLOCKDIR, etc.
  (let* ((root (find-emacs-root-internal (concat invocation-directory
						 invocation-name)))
	 (lisp (and root
		    (let ((f (expand-file-name "lisp" root)))
		      (and (file-directory-p f) f))))
	 (site-lisp (and root
			 (or
			  (let ((f (expand-file-name "xemacs/site-lisp" root)))
			    (and (file-directory-p f) f))
			  (let ((f (expand-file-name "../xemacs/site-lisp"
						     root)))
			    (and (file-directory-p f) f))
			  ;; the next two are for --run-in-place
			  (let ((f (expand-file-name "site-lisp" root)))
			    (and (file-directory-p f) f))
			  (let ((f (expand-file-name "lisp/site-lisp" root)))
			    (and (file-directory-p f) f))
			  )))
	 (lib-src (and root
		       (or
			(let ((f (expand-file-name
				  (concat "lib-src/" system-configuration)
				  root)))
			  (and (file-directory-p f) f))
			(let ((f (expand-file-name "lib-src" root)))
			  (and (file-directory-p f) f))
			(let ((f (expand-file-name system-configuration root)))
			  (and (file-directory-p f) f)))))
	 (etc  (and root
		    (let ((f (expand-file-name "etc" root)))
		      (and (file-directory-p f) f))))
	 (info (and root
		    (let ((f (expand-file-name "info" root)))
		      (and (file-directory-p f) (file-name-as-directory f)))))
	 (lock (and root
		    (boundp 'lock-directory)
		    (if (and lock-directory (file-directory-p lock-directory))
			(file-name-as-directory lock-directory)
		      (or
		       (let ((f (expand-file-name "xemacs/lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       (let ((f (expand-file-name "../xemacs/lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       (let ((f (expand-file-name "lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       ;; if none of them exist, make the "guess" be
		       ;; the one that set-default-load-path-warning
		       ;; will suggest.
		       (file-name-as-directory
			(expand-file-name "../xemacs/lock" root))
		       )))))
    ;; add site-lisp dir to load-path
    (if site-lisp
	(progn
	  ;; If the site-lisp dir isn't on the load-path, add it to the end.
	  (or (member site-lisp load-path)
	      (setq load-path (append load-path (list site-lisp))))
	  ;; Also add any direct subdirectories of the site-lisp directory
	  ;; to the load-path.  But don't add dirs whose names begin
	  ;; with dot or hyphen.
	  (let ((files (directory-files site-lisp nil "^[^-.]" nil 'dirs-only))
		file)
	    (while files
	      (setq file (car files))
	      (if (and (not (member file '("RCS" "CVS" "SCCS")))
		       (setq file (expand-file-name file site-lisp))
		       (not (member file load-path)))
		  (setq load-path
			(nconc load-path
			       (list (file-name-as-directory file)))))
	      (setq files (cdr files))))
	  ))
    ;; add lisp dir to load-path
    (if lisp
	(progn
	  ;; If the lisp dir isn't on the load-path, add it to the end.
	  (or (member lisp load-path)
	      (setq load-path (append load-path (list lisp))))
	  ;; Also add any direct subdirectories of the lisp directory
	  ;; to the load-path.  But don't add dirs whose names begin
	  ;; with dot or hyphen.
	  (let ((files (directory-files lisp nil "^[^-.]" nil 'dirs-only))
		file)
	    (while files
	      (setq file (car files))
	      (if (and (not (member file '("RCS" "CVS" "SCCS")))
		       (setq file (expand-file-name file lisp))
		       (not (member file load-path)))
		  (setq load-path
			(nconc load-path
			       (list (file-name-as-directory file)))))
	      (setq files (cdr files))))
	  ))

    ;; If running from the build directory, always prefer the exec-directory
    ;; that is here over the one that came from paths.h.
    (if (or (and (null exec-directory) lib-src)
	    (and (equal lib-src (expand-file-name "lib-src" root))
		 (not (equal exec-directory lib-src))))
	(setq exec-directory (file-name-as-directory lib-src)))
    (if (or (and (null doc-directory) lib-src)
	    (and (equal lib-src (expand-file-name "lib-src" root))
		 (not (equal doc-directory lib-src))))
	(setq doc-directory (file-name-as-directory lib-src)))

    (if exec-directory
	(or (member exec-directory exec-path)
	    (setq exec-path (append exec-path (list exec-directory)))))
    (if (or (and (null data-directory) etc)
	    (and (equal etc (expand-file-name "etc" root))
		 (not (equal data-directory etc))))
	(setq data-directory (file-name-as-directory etc)))



    ;; If `configure' specified an info dir, use it.
    (or (boundp 'Info-default-directory-list)
	(setq Info-default-directory-list nil))
    (cond (configure-info-directory
	   (setq configure-info-directory (file-name-as-directory
					   configure-info-directory))
	   (or (member configure-info-directory Info-default-directory-list)
	       (setq Info-default-directory-list
		     (append Info-default-directory-list
			     (list configure-info-directory))))))
    ;; If we've guessed the info dir, use that (too).
    (if (and info (not (member info Info-default-directory-list)))
	(setq Info-default-directory-list
	      (append Info-default-directory-list (list info))))

    ;; Default the lock dir to being a sibling of the data-directory.
    ;; If superlock isn't set, or is set to a file in a nonexistent
    ;; directory, derive it from the lock dir.
    (if (boundp 'lock-directory)
	(progn
	  (setq lock-directory lock)
	  (cond ((null lock-directory)
		 (setq superlock-file nil))
		((or (null superlock-file)
		     (not (file-directory-p
			   (file-name-directory superlock-file))))
		 (setq superlock-file
		       (expand-file-name "!!!SuperLock!!!"
					 lock-directory))))))

    (set-default-load-path-warning)))


(defun set-default-load-path-warning ()
  (let ((lock (if (boundp 'lock-directory) lock-directory 't))
	warnings message guess)
    (if (and (stringp lock) (not (file-directory-p lock)))
	(setq lock nil))
    (cond
     ((not (and exec-directory data-directory doc-directory load-path lock))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lock)
	    (setq warnings (cons "lock-directory" warnings)))
	(if (null exec-directory)
	    (setq warnings (cons "exec-directory" warnings)))
	(if (null data-directory)
	    (setq warnings (cons "data-directory" warnings)))
	(if (null doc-directory)
	    (setq warnings (cons "doc-directory" warnings)))
	(if (null load-path)
	    (setq warnings (cons "load-path" warnings)))
	(cond ((cdr (cdr warnings))
	       (setq message (apply 'format "%s, %s, and %s" warnings)))
	      ((cdr warnings)
	       (setq message (apply 'format "%s and %s" warnings)))
	      (t (setq message (format "variable %s" (car warnings)))))
	(insert "couldn't find an obvious default for " message
		", and there were no defaults specified in paths.h when "
		"XEmacs was built.  Perhaps some directories don't exist, "
		"or the XEmacs executable, " (concat invocation-directory
						     invocation-name)
		" is in a strange place?")
	(setq guess (or exec-directory
			data-directory
			doc-directory
			(car load-path)
			(and (string-match "/[^/]+\\'" invocation-directory)
			     (substring invocation-directory 0
					(match-beginning 0)))))
	(if (and guess
		 (or
		  ;; parent of a terminal bin/<configuration> pair (hack hack).
		  (string-match (concat "/bin/"
					(regexp-quote system-configuration)
					"/?\\'")
				guess)
		  ;; parent of terminal src, lib-src, etc, or lisp dir.
		  (string-match
		   "/\\(bin\\|src\\|lib-src\\|etc\\|lisp\\)[^/]*/?\\'"
				guess)))
	    (setq guess (substring guess 0 (match-beginning 0))))

	;; If neither the exec nor lisp dirs are around, then "guess" that
	;; the new configure-style lib dir should be used.  Otherwise, if
	;; only one of them appears to be missing, or it's just lock,
	;; then guess it to be a sibling of whatever already exists.
	(if (and (null exec-directory) (null load-path))
	    (setq guess (expand-file-name (startup-make-version-dir) guess)))

	(if (or (null exec-directory) (null load-path))
	    (insert
	     "\n\nWithout both exec-directory and load-path, XEmacs will "
	     "be very broken.  "))
	(if (and (null exec-directory) guess)
	    (insert
	     "Consider making a symbolic link from "
	     (expand-file-name system-configuration guess)
	     " to wherever the appropriate XEmacs exec-directory "
	     "directory is"))
	(if (and (null data-directory) guess)
	    (insert
	     (if exec-directory
		 "\n\nConsider making a symbolic link " ", and ")
	     "from "
	     (expand-file-name "etc" (if load-path
					 (file-name-directory
					  (directory-file-name
					   (car load-path)))
				       guess))
	     " to wherever the appropriate XEmacs data-directory is"))
	(if (and (null load-path) guess)
	    (insert
	     (if (and exec-directory data-directory)
		 "Consider making a symbolic link "
	       ", and ")
	     "from "
	     (expand-file-name "lisp" guess)
	     " to wherever the appropriate XEmacs lisp library is"))
	(insert ".")

	(if (null lock)
	    (progn
	      (insert
	       "\n\nWithout lock-directory set, file locking won't work.  ")
	      (if guess
		  (insert
		   "Consider creating "
		   (expand-file-name "../xemacs/lock"
				     (or (find-emacs-root-internal
					  (concat invocation-directory
						  invocation-name))
					 guess))
		   " as a directory or symbolic link for use as the lock "
		   "directory.  (This directory must be globally writable.)"
		   ))))

        (if (fboundp 'fill-region)
            ;; Might not be bound in the cold load environment...
	    (let ((fill-column 76))
	      (fill-region (point-min) (point-max))))
	(goto-char (point-min))
	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))


;;; startup.el ends here
