;;; startup.el --- process XEmacs shell arguments

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with XEmacs.

;; It handles the all aspects of startup once the C code has finished
;; initializing itself.  Entry from C is through the function set in
;; the `top-level' variable, which is normally `normal-top-level'.  At
;; the point that `normal-top-level' has been invoked:
;;
;; (1) the dumped Elisp files are available.  Either they were loaded
;;     during this invocation of temacs and it was then converted to
;;     XEmacs using the run-temacs mechanism, or (more likely) the
;;     loadup and dumping occurred at some point in the past and we
;;     just read in the dumped data.
;;
;; (2) All C subsystems have been initialized.
;;
;; (3) A "stream" device has been created, which does I/O over stdin
;;     and stdout.  This is the only device we have available and our
;;     only means of communication, other than disk files.
;;
;; (4) The command-line arguments have been sorted according to
;;     priority specs (this implies that the names of all arguments
;;     must be hard-coded into emacs.c), and certain low-level
;;     arguments such as -sd, -t, -nd, -nw, -batch, etc. have been
;;     processed by main_1() and removed. (NOTE: main_1() is the name
;;     in the source code, but in the object file it has some other
;;     name, such as xemacs_21_2_34_mips_sgi_irix6().) Certain other
;;     arguments such as -version and -help are partially-processed,
;;     triggering some special behavior but being left on the list for
;;     further processing by the Lisp code.
;;
;; The job of the code here is to process the remaining command-line
;; args, set up the various paths, locate where all the packages are
;; and set things up for them (initialize the load path, read in the
;; autoloads, etc.), read in the init files, display the splash
;; screen, and set up any remaining environment-dependent variables.

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst startup-message-timeout 12000) ; More or less disable the timeout

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

;; #### FSFmacs randomness
;;(defconst inhibit-startup-echo-area-message nil
;;  "*Non-nil inhibits the initial startup echo area message.
;;Inhibition takes effect only if your `.emacs' file contains
;;a line of this form:
;; (setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\")
;;If your `.emacs' file is byte-compiled, use the following form instead:
;; (eval '(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
;;Thus, someone else using a copy of your `.emacs' file will see
;;the startup message unless he personally acts to inhibit it.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

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
  "*Functions to call after loading the init file.
The call is not protected by a condition-case, so you can set `debug-on-error'
in the init file, and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "*Functions to be called after loading terminal-specific Lisp code.
See `run-hooks'.  This variable exists for users to set, so as to
override the definitions made by the terminal-specific file.  XEmacs
never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define the proper function and keypad keys
for use under X.  It is used in a fashion analogous to the environment
value TERM.")

(defvar window-setup-hook nil
  "Normal hook run to initialize window system display.
XEmacs runs this hook after processing the command line arguments and loading
the user's init file.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

;;; Path-related variables.
;;; NOTE: Many of them (`lisp-directory', `data-directory', etc.) are
;;; built-in.

(defvar emacs-roots nil
  "List of plausible roots of the XEmacs hierarchy.
This is a list of plausible directories in which to search for the important
directories used by XEmacs at run-time, for example `exec-directory',
`data-directory' and `lisp-directory'.

Normally set at startup by calling `paths-find-emacs-roots'.")

(defvar emacs-data-roots nil
  "List of plausible data roots of the XEmacs hierarchy.")

(defvar user-init-directory-base ".xemacs"
  "Base of directory where user-installed init files may go.")

(defvar user-init-directory
  (file-name-as-directory
   (paths-construct-path (list "~" user-init-directory-base)))
  "Directory where user-installed init files may go.")

(defvar user-init-file-base "init.el"
  "Default name of the user init file if uncompiled.
This should be used for migration purposes only.")

(defvar user-init-file-base-list '("init.el")
  "List of allowed init files in the user's init directory.
The first one found takes precedence.  .elc files do not need to be listed.")

(defvar user-home-init-file-base-list
  (append '(".emacs.el" ".emacs")
	  (and (eq system-type 'windows-nt)
	       '("_emacs.el" "_emacs")))
  "List of allowed init files in the user's home directory.
The first one found takes precedence.  .elc files do not need to be listed.")

(defvar load-home-init-file nil
  "Non-nil if XEmacs should load the init file from the home directory.
Otherwise, XEmacs will offer migration to the init directory.")

(defvar load-user-init-file-p t
  "Non-nil if XEmacs should load the user's init file.")

;; #### called `site-run-file' in FSFmacs

(defvar site-start-file "site-start"
  "File containing site-wide run-time initializations.
It is loaded at run-time before the user's init file (see `user-init-file').
It contains inits that need to be in place for the entire site, but
which, due to their higher incidence of change, don't make sense to
load into XEmacs's dumped image.  Thus, the run-time load order is:

  1. file described in this variable, if non-nil;
  2. the file computed by `find-user-init-file';
  3. `/path/to/xemacs/lisp/default.el'.

Don't use the `site-start.el' file for things some users may not like.
Put them in `default.el' instead, so that users can more easily
override them.  Users can prevent loading `default.el' with the `-q'
option or by setting `inhibit-default-init' in their own init files,
but inhibiting `site-start.el' requires `--no-site-file', which
is less convenient.")

;;(defconst iso-8859-1-locale-regexp "8859[-_]?1"
;;  "Regexp that specifies when to enable the ISO 8859-1 character set.
;;We do that if this regexp matches the locale name
;;specified by the LC_ALL, LC_CTYPE and LANG environment variables.")

(defcustom mail-host-address nil
  "*Name of this machine, for purposes of naming users."
  :type 'string
  :group 'mail)

(defcustom user-mail-address nil
  "*Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'."
  :type 'string
  :group 'mail)

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar init-file-loaded nil
  "True after the user's init file has been loaded (or suppressed with -q).
This will be true when `after-init-hook' is run and at all times
after, and will not be true at any time before.")

(defvar initial-frame-unmapped-p nil)



(defvar command-switch-alist
  '(("-help"	. command-line-do-help)
    ("-version" . command-line-do-version)
    ("-V"	. command-line-do-version)
    ("-funcall" . command-line-do-funcall)
    ("-f"	. command-line-do-funcall)
    ("-e"	. command-line-do-funcall-1)
    ("-eval"	. command-line-do-eval)
    ("-load"	. command-line-do-load)
    ("-l"	. command-line-do-load)
    ("-insert"	. command-line-do-insert)
    ("-i"	. command-line-do-insert)
    ("-kill"	. command-line-do-kill)
    ("-eol"     . command-line-do-enable-eol-detection)
    ("-enable-eol-detection" . command-line-do-enable-eol-detection)
    ;; Options like +35 are handled specially.
    ;; Window-system, site, or package-specific code might add to this.
    ;; X11 handles its options by letting Xt remove args from this list.
    )
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

;;; default switches
;;; Note: these doc strings are semi-magical.

(defun command-line-do-help (arg)
  "Print the XEmacs usage message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (concat "\n" (emacs-version) "\n\n"))
    (princ
     (if (featurep 'x)
	 (concat "When creating a window on an X display, "
		 (emacs-name)
		 " accepts all standard X Toolkit
command line options plus the following:
  -iconname <title>     Use title as the icon name.
  -mc <color>           Use color as the mouse color.
  -cr <color>           Use color as the text-cursor foregound color.
  -private              Install a private colormap.

In addition, the")
       "The"))
    (let ((l command-switch-alist)
	  (options " following options are accepted:

Display options:

  -nw                   Open the initial frame on the current TTY, instead of
                        a window system.
  -t <device>           Use TTY <device> instead of the current TTY for input
                        and output.  This implies the -nw option.
  -display <display>    Standard X option, to specify the display connection.
                        If this option is given, or if the environment
                        variable DISPLAY is set, an initial X frame will be
                        created.  Otherwise, an initial Windows frame will be
                        created if Windows support exists and neither -nw nor
                        -t is given.  Otherwise, a TTY frame is created.
  -unmapped             Do not display the initial frame.  Useful to create
                        a \"server\" that can accept `gnuclient' connections.

Noninteractive options:

  {-help}
  {-version}
  {-V}
  -batch                Execute noninteractively (messages go to stderr, no
                        initial frame created).
  {-funcall}
                        (The function may parse the rest of the command line
                        for its arguments.)
  {-f}
  {-eval}
  {-load}
  {-l}
  {-insert}
  {-i}
  {-kill}
  -sd                   Show dump ID.  Ignored when configured without --pdump.
  -nd                   Don't load the dump file.  Roughly like old temacs.
                        Ignored when configured without --pdump.

Initialization files:

  -no-init-file         Do not load the user-specific init file.
  -q                    Same as -no-init-file.
  -debug-init           Enter the debugger if an error in the init file occurs.
  -user-init-file <file>
                        Use <file> as init file.
  -user-init-directory <directory>
                        Use <directory> as init directory.
  -user <user>          Load user's init file instead of your own.
  -u <user>             Same as -user.
  -no-site-file         Do not load the site-specific init file
                        (site-start.el).

Package/module options:

  -vanilla		Equivalent to -q -no-site-file -no-early-packages.
                        Useful if you think some user-init or site-init code
                        is messing things up, or when running XEmacs in
                        batch mode.
  -no-autoloads		Do not load global symbol files (auto-autoloads) at
			startup.  Also implies `-vanilla'.
  -no-packages          Pretend like the packages don't exist.  Don't put
                        any packages in the load path or set up any package
                        autoloads.  Also implies `-vanilla'.  Use this when
                        running XEmacs in batch mode when you aren't using
                        any functionality in packages and want to make sure
                        that you get no interference from packages
                        (e.g. Lisp files that shadow core Lisp files).
  -no-early-packages	Do not process early packages.
  -debug-paths          Display info about the runtime values of various
                        directory variables (e.g. for loading packages).
  -no-site-modules      Do not search site-modules directories for modules
                        at startup.  Only applies when modules support is
                        compiled into XEmacs.

Encoding options:

  -eol                  Turn on EOL detection (only applies to Unix, no
                        international support; otherwise EOL detection is
                        already on).
  -nuni                 Under MS Windows, disable use of the Unicode versions
                        of API calls.  Not for Windows 95/98/ME.  This is
                        mostly only useful for debugging purposes.

Misc:

  +N <file>             Start displaying <file> at line N.
")
	  (insert (lambda (&rest x)
		    (let ((len 2))
		      (while x
			(princ (car x))
			(incf len (length (car x)))
			(setq x (cdr x)))
		      (when (>= len 24)
			(terpri) (setq len 0))
		      (while (< len 24)
			(princ " ")
			(incf len))))))
      (princ
       (with-temp-buffer
	 (insert options)
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
	       (goto-char (point-min))
	       (when (search-forward (format "{%s}" name) nil t)
		 (delete-region (match-beginning 0) (match-end 0))
		 (let ((standard-output (current-buffer)))
		   (if arg
		       (funcall insert name " " arg)
		     (funcall insert name))
		   (princ doc))))))
	   (setq l (cdr l)))
	 (buffer-string))))
    (princ (concat "\

Anything else is considered a file name, and is placed into a buffer for
editing.

" (emacs-name) " has an online tutorial and manuals.  Type ^Ht (Control-h t) after
starting XEmacs to run the tutorial.  Type ^Hi to enter the manual browser.
Type ^H^H^H (Control-h Control-h Control-h) to get more help options.\n"))

    (kill-emacs 0)
    ))

(defun command-line-do-funcall (arg)
  "Invoke the named lisp function with no arguments.
<function>"
  (funcall (intern (pop command-line-args-left))))
(fset 'command-line-do-funcall-1 'command-line-do-funcall)
(put 'command-line-do-funcall-1 'undocumented t)

(defun command-line-do-eval (arg)
  "Evaluate the lisp form.  Quote it carefully.
<form>"
  (eval (read (pop command-line-args-left))))

(defun command-line-do-load (arg)
  "Load the named file of Lisp code into XEmacs.
<file>"
  (let ((file (pop command-line-args-left)))
    ;; Take file from default dir if it exists there;
    ;; otherwise let `load' search for it.
    (if (file-exists-p (expand-file-name file))
	(setq file (expand-file-name file)))
    (load file nil t)))

(defun command-line-do-insert (arg)
  "Insert file into the current buffer.
<file>"
  (insert-file-contents (pop command-line-args-left)))

(defun command-line-do-kill (arg)
  "Exit XEmacs."
  (kill-emacs t))

(defun command-line-do-version (arg)
  "Print version info and exit."
  (princ (concat (emacs-version) "\n"))
  (kill-emacs 0))

(defun command-line-do-enable-eol-detection (arg)
  "Turn on EOL detection (only applies to Unix)."
  (set-eol-detection t))


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
    (when (not suppress-early-error-handler-backtrace)
      (let ((print-length 1000)
	    (print-level 1000)
	    (print-escape-newlines t)
	    (print-readably nil))
	(when (getenv "EMACSLOADPATH")
	  (princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
		 stream))
	(princ (format "\nexec-directory is %S" exec-directory) stream)
	(princ (format "\ndata-directory is %S" data-directory) stream)
	(princ (format "\ndata-directory-list is %S" data-directory-list) stream)
	(princ (format "\ndoc-directory is %S" doc-directory) stream)
	(princ (format "\nload-path is %S" load-path) stream)
	(princ "\n\n" stream)))
    (when (not suppress-early-error-handler-backtrace)
      (backtrace stream t)))
  (if-fboundp 'mswindows-message-box
      (mswindows-message-box "Initialization error"))
  (kill-emacs -1))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; Do this first for maximum likelihood of catching errors.  The main
    ;; purpose of this is so that debug-on-error can be set to catch errors
    ;; during normal noninteractive running.
    (when (getenv "XEMACSDEBUG")
      (eval (read (getenv "XEMACSDEBUG"))))
    ;; Canonicalize HOME (PWD is canonicalized by init_buffer in buffer.c)
    (let ((value (user-home-directory)))
      (if (and value
	       (< (length value) (length default-directory))
	       (equal (file-attributes default-directory)
		      (file-attributes value)))
	  (setq default-directory (file-name-as-directory value))))
    (setq default-directory (abbreviate-file-name default-directory))
    (initialize-xemacs-paths)

    (startup-set-invocation-environment)
    (startup-setup-paths (cond (inhibit-all-packages t)
			       (inhibit-early-packages '(early))
			       (t nil))
			 nil)
    (startup-setup-paths-warning)

    ;; Either we need to inhibit messages from do_autoloads, or this
    ;; should go into (command-line) after the initialization of the
    ;; frame?
    (startup-load-autoloads)

    (let (error-data)
      ;; if noninteractive, an error will kill us.  by catching and
      ;; resignalling, we don't accomplish much, but do make it difficult
      ;; to determine where the error really occurred.  when interactive,
      ;; however, an error processing the command line does NOT kill us;
      ;; instead, the error handler tries to display an error on the frame.
      ;; In that case, we must make sure that all the remaining initialization
      ;; gets done!!!
      ;;
      ;; #### A better solution in the interactive case is to use
      ;; call-with-condition-handler, which would let us do the rest of
      ;; the initialization AND allow the user to get an accurate backtrace.
      (if (noninteractive)
	  (command-line)
	(condition-case data
	    (command-line)
	  ;; catch non-error signals, especially quit
	  (t (setq error-data data))))
      ;; Do this again, in case the init file defined more abbreviations.
      (setq default-directory (abbreviate-file-name default-directory))
      ;; Specify the file for recording all the auto save files of
      ;; this session.  This is used by recover-session.
      (if auto-save-list-file-prefix
	  (setq auto-save-list-file-name
		(expand-file-name
		 (format "%s%d-%s"
			 auto-save-list-file-prefix
			 (emacs-pid)
			 (system-name)))))
      (run-hooks 'emacs-startup-hook)
      (and term-setup-hook
	   (run-hooks 'term-setup-hook))
      (setq term-setup-hook nil)
      ;;      ;; Modify the initial frame based on what the init file puts into
      ;;      ;; ...-frame-alist.
      (frame-notice-user-settings)
      ;;      ;;####FSFmacs junk
      ;;      ;; Now we know the user's default font, so add it to the menu.
      ;;      (if (fboundp 'font-menu-add-default)
      ;;	  (font-menu-add-default))
      (when window-setup-hook
	(run-hooks 'window-setup-hook))
      (setq window-setup-hook nil)
      (if error-data
	  ;; re-signal, and don't allow continuation as that will probably
          ;; wipe out the user's .emacs if she hasn't migrated yet!
	  (signal-error (car error-data) (cdr error-data))))

    (if load-user-init-file-p
	(maybe-migrate-user-init-file))
    ;; FSF calls precompute-menubar-bindings.  We don't mix menubars
    ;; and keymaps.
    ))

(defun command-line-early (args)
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
  ;;  (if (let ((ctype
  ;;	     ;; Use the first of these three envvars that has a nonempty value.
  ;;	     (or (let ((string (getenv "LC_ALL")))
  ;;		   (and (not (equal string "")) string))
  ;;		 (let ((string (getenv "LC_CTYPE")))
  ;;		   (and (not (equal string "")) string))
  ;;		 (let ((string (getenv "LANG")))
  ;;		   (and (not (equal string "")) string)))))
  ;;	(and ctype
  ;;	     (string-match iso-8859-1-locale-regexp ctype)))
  ;;      (progn
  ;;	(standard-display-european t)
  ;;	(require 'iso-syntax)))
  
  (if vanilla-inhibiting ;; set in main_1()
      (setq load-user-init-file-p nil
	    site-start-file nil)
    (setq load-user-init-file-p (not (noninteractive))))
  
  ;; Allow (at least) these arguments anywhere in the command line
  (macrolet ((long-argmatch (match)
	       ;; use a macro to avoid lots of concatting at runtime
	       `(or (string= arg ,match)
		    (string= arg ,(concat "-" match)))))
    (let ((new-args nil)
	  (arg      nil))
      (while args
	(setq arg (pop args))
      (cond
       ((or (string= arg "-q")
	    (long-argmatch "-no-init-file"))
	(setq load-user-init-file-p nil))
       ((long-argmatch "-no-site-file")
	(setq site-start-file nil))
       ((long-argmatch "-user-init-file")
	(setq user-init-file (pop args)))
       ((long-argmatch "-user-init-directory")
	(setq user-init-directory (file-name-as-directory (pop args))))
       ((or (string= arg "-u")
	    (long-argmatch "-user"))
	(let* ((user (pop args))
	       (home-user (concat "~" user)))
	  (setq user-init-directory (file-name-as-directory
				     (paths-construct-path
				      (list home-user user-init-directory-base))))
	  (setq user-init-file
		(find-user-init-file user-init-directory home-user))
	  (setq custom-file
		(make-custom-file-name user-init-file))))
       ((long-argmatch "-debug-init")
	(setq init-file-debug t))
       ((long-argmatch "-unmapped")
	(setq initial-frame-unmapped-p t))
       ((or (string= arg "--") (string= arg "-"))
	(while args
	  (push (pop args) new-args)))
       (t (push arg new-args))))

    (with-obsolete-variable 'init-file-user
      (setq init-file-user (and load-user-init-file-p "")))

    (nreverse new-args))))

(defconst initial-scratch-message "\
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, first visit that file with C-x C-f,
;; then enter the text in that file's own buffer. (C-x is the standard
;; XEmacs abbreviation for `Control+x', i.e. hold down the Control key
;; while hitting the x key.)
;;
;; For Lisp evaluation, type an expression, move to the end and hit C-j.

"
  "Initial message displayed in *scratch* buffer at startup.
If this is nil, no message will be displayed.")

(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))

    (let ((debugger 'early-error-handler)
	  (debug-on-error t))

      ;; Process magic command-line switches like -q and -u.  Do this
      ;; before creating the first frame because some of these switches
      ;; may affect that.  I think it's ok to do this before establishing
      ;; the X connection, and maybe someday things like -nw can be
      ;; handled here instead of down in C.
      (setq command-line-args-left (command-line-early command-line-args-left))

      (when (eq system-type 'windows-nt)
	(declare-fboundp (init-mswindows-at-startup)))

      ;; Setup the toolbar icon directory
      (when (featurep 'toolbar)
	(init-toolbar-location))

      (if (featurep 'toolbar)
	  (if (featurep 'infodock)
	      (require 'id-x-toolbar)
	    (init-toolbar)))

      ;; Run the window system's init function.  tty is considered to be
      ;; a type of window system for this purpose.  This creates the
      ;; initial (non stdio) device.
      (when (and initial-window-system (not noninteractive))
	(funcall (intern (concat "init-"
				 (symbol-name initial-window-system)
				 "-win"))))

      ;; When not in batch mode, this creates the first visible frame,
      ;; and deletes the stdio device.
      (frame-initialize))

    ;; Reinitialize faces if necessary.  This function changes face if
    ;; it is created during auto-autoloads loading.  Otherwise, it
    ;; does nothing.
    (startup-initialize-custom-faces)

    ;; A couple of other things need to be initted.
    ;; (RMS writes about internally using hooks for this, in reference
    ;; to frame-initialize and frame-notice-user-settings:
    ;;
    ;; These are now called explicitly at the proper times,
    ;; since that is easier to understand.
    ;; Actually using hooks within Emacs is bad for future maintenance. --rms.
    ;;
    ;; In this case, I completely agree. --ben
    (if (featurep 'menubar)
	(init-menubar-at-startup))
    ;; perhaps this should go earlier in the process?
    (if (featurep 'mule)
	(declare-fboundp (init-mule-at-startup)))

    ;;
    ;; We have normality, I repeat, we have normality.  Anything you still
    ;; can't cope with is therefore your own problem.  (And we don't need
    ;; to kill XEmacs for it.)
    ;;

    ;;; Load init files.
    (load-init-file)

    (with-current-buffer (get-buffer "*scratch*")
      (erase-buffer)
      ;; (insert initial-scratch-message)
      (set-buffer-modified-p nil)
      (when (eq major-mode 'fundamental-mode)
	(funcall initial-major-mode)))

    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    ;; Note that for any TTY's opened subsequently, the TTY init
    ;; code will run this.
    (when (and (eq 'tty (console-type))
	       (not (noninteractive)))
      (load-terminal-library))

    ;; Process the remaining args.
    (command-line-1)

    ;; it was turned on by default so that the warnings don't get displayed
    ;; until after the splash screen.
    (setq inhibit-warning-display nil)
    ;; If -batch, terminate after processing the command options.
    (when (noninteractive) (kill-emacs t))))

(defun load-terminal-library ()
  (when term-file-prefix
    (let ((term (getenv "TERM"))
	  hyphend)
      (while (and term
		  (not (load (concat term-file-prefix term) t t)))
	;; Strip off last hyphen and what follows, then try again
	(if (setq hyphend (string-match "[-_][^-_]+\\'" term))
	    (setq term (substring term 0 hyphend))
	  (setq term nil))))))

(defun find-init-file-1 (dir base-list)
  (catch 'found
    (dolist (file base-list)
      (let ((expanded (expand-file-name file dir)))
	  (if (string-match "el$" expanded)
	      (let* ((elc (concat expanded "c"))
		     (el-ok (file-readable-p expanded))
		     (elc-ok (file-readable-p elc)))
		(cond
		 ((and el-ok elc-ok (file-newer-than-file-p expanded elc))
		  (lwarn 'initialization 'warning
		    "\
The compiled initialization file `%s' exists
but is out-of-date with respect to the uncompiled initialization
file `%s'.  XEmacs will load the uncompiled
version.  You should correct the problem as soon as possible by
loading the uncompiled version and compiling it using
`M-x byte-compile-file' (or `Lisp->Byte-Compile This File' on
the menubar)."
		    elc expanded)
		  (throw 'found expanded))
		 (elc-ok (throw 'found elc))
		 (el-ok (throw 'found expanded))))
	    (when (file-readable-p 
		   (when (file-readable-p expanded)
		     (throw 'found expanded)))))))))

(defun find-user-init-directory-init-file (&optional init-directory)
  "Determine the user's init file if in the init directory."
  (find-init-file-1 (or init-directory user-init-directory)
		    user-init-file-base-list))

(defun find-user-home-directory-init-file (&optional home-directory)
  "Determine the user's init file if in the home directory."
  (find-init-file-1 (or home-directory "~")
		    user-home-init-file-base-list))

(defun find-user-init-file (&optional init-directory home-directory)
  "Determine the user's init file."
  (if load-home-init-file
      (find-user-home-directory-init-file home-directory)
    (or (find-user-init-directory-init-file init-directory)
	(find-user-home-directory-init-file home-directory))))

(defun maybe-migrate-user-init-file ()
  "Ask user if she wants to migrate the init file(s) to new location."
  (if (and (not load-home-init-file)
	   (not (find-user-init-directory-init-file user-init-directory))
	   (stringp user-init-file)
	   (file-readable-p user-init-file))
      (if (with-output-to-temp-buffer (help-buffer-name nil)
	    (progn
	      (princ "XEmacs recommends that the initialization code in
")
	      (princ user-init-file)
	      (princ "
be migrated to the ")
	      (princ user-init-directory)
	      (princ " directory.  XEmacs can
perform the migration automatically.

After the migration, init.el/init.elc holds user-written
initialization code.  Moreover the customize settings will be in
custom.el.

You can undo the migration at any time with
M-x maybe-unmigrate-user-init-file.

If you choose not to do this now, XEmacs will not ask you this
question in the future.  However, you can still make XEmacs
perform the migration at any time with M-x migrate-user-init-file.")
	      (show-temp-buffer-in-current-frame standard-output)
	      (yes-or-no-p-minibuf (concat "Migrate init file to "
					   user-init-directory
					   "? "))))
	  (progn
	    (migrate-user-init-file)
	    (maybe-create-compatibility-dot-emacs))
	(customize-save-variable 'load-home-init-file t))))

(defun maybe-create-compatibility-dot-emacs ()
  "Ask user if she wants to create a .emacs compatibility file."
  (if (with-output-to-temp-buffer (help-buffer-name nil)
	(progn
	  (princ "The initialization code has now been migrated to the ")
	  (princ user-init-directory)
	  (princ "directory.

For backwards compatibility with, for example, older versions of XEmacs,
XEmacs can create a special old-style .emacs file in your home
directory which will load the relocated initialization code.")
	  (show-temp-buffer-in-current-frame standard-output)
	  (yes-or-no-p-minibuf "Create compatibility .emacs? ")))
      (create-compatibility-dot-emacs)))

(defun migrate-user-init-file ()
  "Migrate the init file from the home directory."
  (interactive)
  (if (not (file-exists-p user-init-directory))
      (progn
	(message "Creating %s directory..." user-init-directory)
	(make-directory user-init-directory)))
  (message "Migrating custom file...")
  (customize-set-value 'load-home-init-file nil)
  (custom-migrate-custom-file (make-custom-file-name user-init-file
						     'force-new))
  (message "Moving init file...")
  (let ((new-user-init-file (expand-file-name user-init-file-base
					      user-init-directory)))
    (rename-file user-init-file new-user-init-file)
    (setq user-init-file new-user-init-file))
  (message "Migration done."))

(defun create-compatibility-dot-emacs ()
  "Create .emacs compatibility file for migrated setup."
  (message "Creating .emacs compatibility file.")
  (with-temp-file (expand-file-name ".emacs" "~")
    (insert ";;; XEmacs backwards compatibility file\n")
    (insert "(setq user-init-file\n")
    (insert "      (expand-file-name \"init.el\"\n")
    (insert "			(expand-file-name \".xemacs\" \"~\")))\n")
    (insert "(setq custom-file\n")
    (insert "      (expand-file-name \"custom.el\"\n")
    (insert "			(expand-file-name \".xemacs\" \"~\")))\n")
    (insert "\n")
    (insert "(load-file user-init-file)\n")
    (insert "(load-file custom-file)"))
  (message "Created .emacs compatibility file."))

(defun maybe-unmigrate-user-init-file ()
  "Possibly unmigrate the user's init and custom files."
  (interactive)
  (let ((dot-emacs-file-name (expand-file-name ".emacs" "~")))
    (if (and (not load-home-init-file)
	     (or (not (file-exists-p dot-emacs-file-name))
		 (yes-or-no-p-minibuf (concat "Overwrite " dot-emacs-file-name
					      "? "))))
      (unmigrate-user-init-file dot-emacs-file-name))))

(defun unmigrate-user-init-file (&optional target-file-name)
  "Unmigrate the user's init and custom files."
  (interactive)
  (let ((target-file-name
	 (or target-file-name (expand-file-name ".emacs" "~"))))
    (rename-file user-init-file target-file-name 'ok-if-already-exists)
    (setq user-init-file target-file-name)
    (let ((old-custom-file custom-file))
      (custom-migrate-custom-file target-file-name)
      (customize-save-variable 'load-home-init-file t)
      (delete-file old-custom-file))))

(defun load-user-init-file ()
  "This function actually reads the init file."
  (if (not user-init-file)
      (setq user-init-file
	    (find-user-init-file user-init-directory)))
  (if (not custom-file)
      (setq custom-file (make-custom-file-name user-init-file)))
  (if (and user-init-file
	   (file-readable-p user-init-file))
      (load user-init-file t t t))
  (if (and custom-file
	   (or (not user-init-file)
	       (not (string= custom-file user-init-file)))
	   (file-readable-p custom-file))
      (load custom-file t t t))
  (unless inhibit-default-init
    (let ((inhibit-startup-message nil))
      ;; Users are supposed to be told their rights.
      ;; (Plus how to get help and how to undo.)
      ;; Don't you dare turn this off for anyone except yourself.
      (load "default" t t))))

;;; #### move this comment into a docstring.  See site-init-file for some
;;; description of what it does.  Substitute a pointer to this function in
;;; site-init-file's docstring.
;;; Load user's init file and default ones.
(defun load-init-file ()
  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before the user's init file.  There is no point in
  ;; doing this after the user's init file; that is useless.
  (when site-start-file
    (load site-start-file t t))

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  (let (debug-on-error-from-init-file
	debug-on-error-should-be-set
	(debug-on-error-initial
	 (if (eq init-file-debug t) 'startup init-file-debug)))
    (let ((debug-on-error debug-on-error-initial))
      (if (and load-user-init-file-p init-file-debug)
	  (progn
	    ;; Do this without a condition-case if the user wants to debug.
	    (load-user-init-file))
	(condition-case nil
	    (call-with-condition-handler
		#'(lambda (__load_init_file_arg__)
		    (let ((errstr (error-message-string
				   __load_init_file_arg__)))
		      (message "Error in init file: %s" errstr)
		      (lwarn 'initialization 'error
			"\
An error has occurred while loading %s:

%s

Backtrace follows:

%s

To ensure normal operation, you should investigate the cause of the error
in your initialization file and remove it.  Use the `-debug-init' option
to XEmacs to enter the debugger when the error occurs and investigate the
exact problem."
			user-init-file errstr
			(backtrace-in-condition-handler-eliminating-handler
			 '__load_init_file_arg__)))
		    (setq init-file-had-error t))
		#'(lambda ()
		    (if load-user-init-file-p
			(load-user-init-file))
		    (setq init-file-had-error nil)))
	  (error nil)))
      ;; If we can tell that the init file altered debug-on-error,
      ;; arrange to preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
	  (setq debug-on-error-should-be-set t
		debug-on-error-from-init-file debug-on-error)))
    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file)))

  (setq init-file-loaded t)

  ;; Do this here in case the init file sets mail-host-address.
  ;; Don't do this here unless noninteractive, it is frequently wrong. -sb
  ;; (or user-mail-address
  (when noninteractive
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
  (cond
   ((null command-line-args-left)
    (unless noninteractive
      ;; If there are no switches to process, run the term-setup-hook
      ;; before displaying the copyright notice; there may be some need
      ;; to do it before doing any output.  If we're not going to
      ;; display a copyright notice (because other options are present)
      ;; then this is run after those options are processed.
      (run-hooks 'term-setup-hook)
      ;; Don't let the hook be run twice.
      (setq term-setup-hook nil)

      ;; Don't clobber a non-scratch buffer if init file
      ;; has selected it.
      (when (string= (buffer-name) "*scratch*")
	(unless (or inhibit-startup-message
		    (input-pending-p))
	  (let (tmout)
	    (unwind-protect
		;; Guts of with-timeout
		(catch 'tmout
		  (setq tmout (add-timeout startup-message-timeout
					   (lambda (ignore)
					     (condition-case nil
						 (throw 'tmout t)
					       (error nil)))
					   nil))
		  (display-splash-screen)
		  (or nil;; (pos-visible-in-window-p (point-min))
		      (goto-char (point-min)))
		  (sit-for 0)
		  (setq unread-command-event (next-command-event)))
	      (when tmout (disable-timeout tmout)))))
	(with-current-buffer (get-buffer "*scratch*")
	  ;; In case the XEmacs server has already selected
	  ;; another buffer, erase the one our message is in.
	  (erase-buffer)
	  (when (stringp initial-scratch-message)
	    (insert initial-scratch-message))
	  (set-buffer-modified-p nil)))))

   (t
    ;; Command-line-options exist
    (let ((dir command-line-default-directory)
	  (file-count 0)
	  (line nil)
	  (end-of-options nil)
	  file-p arg tem)
      (while command-line-args-left
	(setq arg (pop command-line-args-left))
	(cond
	 (end-of-options
	  (setq file-p t))
	 ((setq tem (when (eq (aref arg 0) ?-)
		      (or (assoc arg command-switch-alist)
			  (assoc (substring arg 1)
				 command-switch-alist))))
	  (funcall (cdr tem) arg))
	 ((string-match "\\`\\+[0-9]+\\'" arg)
	  (setq line (string-to-int arg)))
	 ;; "- file" means don't treat "file" as a switch
	 ;;  ("+0 file" has the same effect; "-" added
	 ;;   for unixoidiality).
	 ;; This is worthless; the `unixoid' way is "./file". -jwz
	 ((or (string= arg "-") (string= arg "--"))
	  (setq end-of-options t))
	 (t
	  (setq file-p t)))

	(when file-p
	  (setq file-p nil)
	  (incf file-count)
	  (setq arg (expand-file-name arg dir))
	  (cond
	   ((= file-count 1)
	    (find-file arg))
	   (noninteractive (find-file arg))
	   (t (find-file-other-window arg)))
	  (when line
	    (goto-line line)
	    (setq line nil))))))))


(defun startup-presentation-hack-help (e)
  (setq e (extent-property e 'startup-presentation-hack))
  (symbol-name e))

(defun startup-presentation-activate (ev ex)
  (call-interactively (extent-property ex 'startup-presentation-hack)))

(defun splash-screen-present-hack (e v)
;   (set-extent-property e 'mouse-face 'highlight)
;   (set-extent-property e 'startup-presentation-hack v)
;   (set-extent-property e 'help-echo
;     		       'startup-presentation-hack-help)
;   (set-extent-property e 'activate-function 'startup-presentation-activate)
  )

(defun splash-hack-version-string ()
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (re-search-forward "^XEmacs" nil t)
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-min))
      (when (re-search-forward " \\[Lucid\\]" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (when (re-search-forward "[^(][^)]*-[^)]*-" nil t)
	(delete-region (1+ (match-beginning 0)) (match-end 0))
	(insert "("))
      (goto-char (point-max))
      (search-backward " " nil t)
      (when (search-forward "." nil t)
	(delete-region (1- (point)) (point-max))))))

;; parse one page description (see `splash-screen-body') and display
;; at point.
(defun splash-screen-present (l)
  (cond ((stringp l)
         (insert l))
        ((eq (car-safe l) 'face)
         ;; (face name string)
         (let ((p (point)))
           (splash-screen-present (elt l 2))
	   (set-extent-face (make-extent p (point))
			    (elt l 1))))
        ((eq (car-safe l) 'key)
         (let* ((c (elt l 1))
                (p (point))
                (k (where-is-internal c nil t)))
           (insert (if k (key-description k)
		     (format "M-x %s" c)))
	   (let ((e (make-extent p (point))))
	     (set-extent-face e 'bold)
	     (splash-screen-present-hack e c))))
        ((eq (car-safe l) 'funcall)
         ;; (funcall (fun . args) string)
         (let ((p (point)))
           (splash-screen-present (elt l 2))
	   (splash-screen-present-hack (make-extent p (point))
				       (elt l 1))))
	((consp l)
	 (mapcar 'splash-screen-present l))
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
				 ((glyphp glyph)
				  (glyph-width glyph))
				 (t
				  (error "startup-center-spaces: bad arg")))))
    (+ left-margin
       (round (/ (/ (- fill-area-width glyph-pixwidth) 2) avg-pixwidth)))))

;; the splash screen originated in 19.10 as splash-screen-*.  When
;; Chuck made the global screen->frame change for 19.12, he
;; accidentally changed these too.  This randomness is getting on my
;; nerves, so let's fix it and provide minimal aliases for the
;; `locale' mule package. --ben

;; returns either of vector of page descriptions, each describing one
;; screenful of information, or just one such page descriptions  Each
;; page description is a list of textual elements describing how to
;; display a section of text.  The elements are processed in turn and
;; the results inserted one after the previous in a buffer.  Each
;; textual element is either:

;; -- a string, inserted as-is with no decoration.
;; -- a list of (face FACES "text"), where FACES is the name of a face
;;    or a list of such names, and specifies the face(s) used when
;;    displaying the text.
;; -- a list of (key COMMAND-NAME); the key sequence corresponding to
;;    the command will be inserted, in boldface.
;; -- a list of textual elements.

(defun splash-screen-window-body ()
  `(
     (face (blue bold underline)
	   "Useful Help-menu entries:\n\n")
     ,@(if (string-match "beta" emacs-version)
	   `((face bold "Beta Info:")
	     (face (red bold)
		   "                      This is an Experimental version of XEmacs.\n"))
	 `( ""))
     (face bold "XEmacs FAQ:")
     "                     Read the XEmacs FAQ.\n"
     (face bold "Info (Online Docs):")
     "             Read the on-line documentation.\n"
     (face bold "Tutorial:")
     "                       XEmacs tutorial.\n"
     (face bold "Samples->View Sample init.el:")
     "   A useful initialization file.\n"
     (face bold "About XEmacs:")
     "                   See who's developing XEmacs.\n"
     "\n"
     (face (bold blue) "XEmacs website:")
     "                 http://www.xemacs.org/\n\n"
     ,@(if (featurep 'sparcworks)
	   `( "\
Sun provides support for the WorkShop/XEmacs integration package only.
All other XEmacs packages are provided to you \"AS IS\".\n"
	      ,@(let ((lang (or (getenv "LC_ALL") (getenv "LC_MESSAGES")
				(getenv "LANG"))))
		  (if (and
		       (not (featurep 'mule)) ;; Already got mule?
		       lang ;; Non-English locale?
		       (not (string= lang "C"))
		       (not (string-match "^en" lang))
		       ;; Comes with Sun WorkShop
		       (locate-file "xemacs-mule" exec-path))
		      '( "\
This version of XEmacs has been built with support for Latin-1 languages only.
To handle other languages you need to run a Multi-lingual (`Mule') version of
XEmacs, by either running the command `xemacs-mule', or by using the X resource
`ESERVE*defaultXEmacsPath: xemacs-mule' when starting XEmacs from Sun WorkShop.
\n")))))
     (face italic "\
Copyright (C) 1985-1999 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1997 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois.
Copyright (C) 1995-2005 Ben Wing.\n")
     ))

(defun splash-screen-tty-body ()
  `(
    (face italic "[`C-' means the control key, `M-' means the meta key]\n\n")
     ,@(if (string-match "beta" emacs-version)
	   `((key describe-beta)
	     ": " (face (red bold)
			"This is an Experimental version of XEmacs.\n"))
	 `( "\n"))
     ((key xemacs-local-faq)
      ": Read the XEmacs FAQ. (A " (face underline "capital") " F!)\n")
     ((key info) ": Read the on-line documentation.\n")
     ((key help-command)
      ": Get help on using XEmacs.\n")
     ((key help-with-tutorial)
      ": Read the XEmacs tutorial.\n")
     ((key view-sample-init-el)
      ": View the sample init.el file.\n")
     ((key about-xemacs) ": See who's developing XEmacs.\n")
     ((key save-buffers-kill-emacs)
      ": exit XEmacs\n")
     "\n"
     (face (bold blue) "XEmacs website: ") 
     "http://www.xemacs.org/\n\n"
     (face italic "\
Copyright (C) 1985-1999 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1997 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois.
Copyright (C) 1995-2004 Ben Wing.")
;      ((key find-file) ": visit a file; ")
;      ((key save-buffer) ": save changes; ")
;      ((key undo) ": undo changes; ")
     ))

;; I really hate global variables, oh well.
;(defvar xemacs-startup-logo-function nil
;  "If non-nil, function called to provide the startup logo.
;This function should return an initialized glyph if it is used.")

;; This will hopefully go away when gettext is functional.
(defconst splash-screen-static-body
  `(,(emacs-version) "\n\n"))
;; temporary support for old locale files.
(define-obsolete-variable-alias 'splash-frame-static-body
  'splash-screen-static-body)

(defun display-splash-screen ()
  ;; display the splash screen in the current buffer and put it in the
  ;; current window.
  (let ((logo xemacs-logo)
	(buffer-read-only nil)
        (tty (eq 'tty (console-type))))
    (unless tty
      (insert "\n")
      (indent-to (startup-center-spaces logo))
      (set-extent-begin-glyph (make-extent (point) (point)) logo)
      ;;(splash-screen-present-hack (make-extent p (point)) 'about-xemacs))
      (insert "\n\n"))
    (splash-screen-present splash-screen-static-body)
    (splash-hack-version-string)
    (goto-char (point-max))
    (let* ((after-change-functions nil) ; no font-lock, thank you
	   (elements (cond (tty (splash-screen-tty-body))
			   (t (splash-screen-window-body)))))
      (pop-to-buffer (current-buffer))
      (delete-other-windows)
      (splash-screen-present elements)
      (set-buffer-modified-p nil))))

(defun xemacs-splash-buffer ()
  "Display XEmacs splash screen in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Splash*")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer buffer)
    (display-splash-screen)))

;;  (let ((present-file
;;         #'(lambda (f)
;;             (splash-screen-present
;;	      (list 'funcall
;;		    (list 'find-file-other-window
;;			  (expand-file-name f data-directory))
;;		    f)))))
;;    (insert "For customization examples, see the files ")
;;    (funcall present-file "sample.init.el")
;;    (insert " and ")
;;    (funcall present-file "sample.Xresources")
;;    (insert (format "\nin the directory %s." data-directory)))


(defun startup-set-invocation-environment ()
  ;; XEmacs -- Steven Baur says invocation directory is nil if you
  ;; try to use XEmacs as a login shell.
  (or invocation-directory (setq invocation-directory default-directory))
  (setq invocation-directory
	;; don't let /tmp_mnt/... get into the load-path or exec-path.
	(abbreviate-file-name invocation-directory)))

;;; High-level functions to set up the paths.

(defun startup-find-load-path (&optional inhibit-packages
			       set-global-package-paths)
  "Determine the value for `load-path'.
INHIBIT-PACKAGES says which types of packages, if any, to omit from the
returned value.  It can be `t' (omit all), one of the symbols `early',
`late', or `last', or a list of one or more of the symbols.

If SET-GLOBAL-PACKAGE-PATHS is non-nil, initialize the global package path
variables referring to the particular types of packages
(`early-package-hierarchies', `early-package-load-path',
`late-package-hierarchies', `late-package-load-path',
`last-package-hierarchies', `last-package-load-path')."
  (let (earlyp latep lastp earlyp-lp latep-lp lastp-lp)
    (apply #'(lambda (early late last)
	       (setq earlyp (and (not (memq 'early inhibit-packages)) early))
	       (setq latep (and (not (memq 'late inhibit-packages)) late))
	       (setq lastp (and (not (memq 'last inhibit-packages)) last)))
	   (packages-find-all-package-hierarchies
	    emacs-data-roots))

  (setq earlyp-lp (packages-find-package-load-path earlyp))
  (setq latep-lp (packages-find-package-load-path latep))
  (setq lastp-lp (packages-find-package-load-path lastp))

  (when set-global-package-paths
    (setq early-package-hierarchies earlyp
	  late-package-hierarchies latep
	  last-package-hierarchies lastp
	  early-package-load-path earlyp-lp
	  late-package-load-path latep-lp
	  last-package-load-path lastp-lp))

  (paths-construct-load-path emacs-roots earlyp-lp latep-lp lastp-lp
			     lisp-directory site-directory
			     mule-lisp-directory)))

(defun startup-setup-paths (&optional inhibit-packages called-early)
  "Setup all the various paths.
INHIBIT-PACKAGES says which types of packages, if any, to omit from the
returned value.  It can be `t' (omit all), one of the symbols `early',
`late', or `last', or a list of one or more of the symbols.

This function is idempotent, so call this as often as you like!"

  (setq debug-paths (or debug-paths
			(and (getenv "EMACSDEBUGPATHS")
			     t)))

  (setq emacs-roots (paths-find-emacs-roots invocation-directory invocation-name
					    #'paths-emacs-data-root-p))

  (setq emacs-data-roots (paths-find-emacs-roots invocation-directory invocation-name
						 #'paths-emacs-data-root-p))

  (if (null emacs-roots)
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))

	(insert "Couldn't find an obvious default for the root of the\n"
		"XEmacs hierarchy.")

	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)))

  (if (eq inhibit-packages t)
      (setq inhibit-packages '(early late last)))
  (if (not (listp inhibit-packages))
      (setq inhibit-packages (list inhibit-packages)))

  (when debug-paths
    (princ (format
"startup-setup-paths arguments:
  inhibit-packages: %S
  inhibit-site-lisp: %S
  called-early: %S
" inhibit-packages inhibit-site-lisp called-early)
	   'external-debugging-output)
    (princ (format
"emacs-roots:
%S
emacs-data-roots:
%S
user-init-directory: %S
configure-package-path: %S
" emacs-roots emacs-data-roots user-init-directory configure-package-path)
	   'external-debugging-output)
    )

  (setq lisp-directory (paths-find-lisp-directory emacs-roots))

  (if debug-paths
      (princ (format "lisp-directory:\n%S\n" lisp-directory)
	     'external-debugging-output))

  (if (featurep 'mule)
      (progn
	(setq mule-lisp-directory
	      (paths-find-mule-lisp-directory emacs-roots
					      lisp-directory))
	(if debug-paths
	    (princ (format "mule-lisp-directory:\n%S\n"
			   mule-lisp-directory)
		   'external-debugging-output)))
    (setq mule-lisp-directory '()))

  (setq site-directory (and (null inhibit-site-lisp)
			    (paths-find-site-lisp-directory emacs-roots)))

  (if (and debug-paths (null inhibit-site-lisp))
      (princ (format "site-directory:\n%S\n" site-directory)
	     'external-debugging-output))

  (setq load-path (startup-find-load-path inhibit-packages t))

  (when debug-paths
    (princ (format "early-package-hierarchies and early-package-load-path:\n%S\n%S\n"
		   early-package-hierarchies early-package-load-path)
	   'external-debugging-output)
    (princ (format "late-package-hierarchies and late-package-load-path:\n%S\n%S\n"
		   late-package-hierarchies late-package-load-path)
	   'external-debugging-output)
    (princ (format "last-package-hierarchies and last-package-load-path:\n%S\n%S\n"
		   last-package-hierarchies last-package-load-path)
	   'external-debugging-output))

  (if debug-paths
      (princ (format "load-path:\n%S\n" load-path)
            'external-debugging-output))
  (setq module-directory (paths-find-module-directory emacs-roots))
  (if debug-paths
      (princ (format "module-directory:\n%S\n" module-directory)
	     'external-debugging-output))
  (setq site-module-directory (and (null inhibit-site-modules)
				   (paths-find-site-module-directory
				    emacs-roots)))
  (if (and debug-paths (null inhibit-site-modules))
      (princ (format "site-module-directory:\n%S\n"
		     site-module-directory)
	     'external-debugging-output))

  (setq module-load-path (paths-construct-module-load-path
			  emacs-roots
			  module-directory
			  site-module-directory))

  (unless called-early
    (setq Info-directory-list
	  (paths-construct-info-path
	   emacs-roots
	   early-package-hierarchies late-package-hierarchies last-package-hierarchies))

    (if debug-paths
	(princ (format "Info-directory-list:\n%S\n" Info-directory-list)
	       'external-debugging-output))

    (setq exec-directory (paths-find-exec-directory emacs-roots))

    (if debug-paths
	(princ (format "exec-directory:\n%s\n" exec-directory)
	       'external-debugging-output))

    (setq exec-path
	  (paths-construct-exec-path emacs-roots exec-directory
				     early-package-hierarchies late-package-hierarchies
				     last-package-hierarchies))

    (if debug-paths
	(princ (format "exec-path:\n%S\n" exec-path)
	       'external-debugging-output))

    (setq doc-directory (paths-find-doc-directory emacs-roots))

    (if debug-paths
	(princ (format "doc-directory:\n%S\n" doc-directory)
	       'external-debugging-output))
    
    (setq data-directory (paths-find-data-directory emacs-roots))
    
    (if debug-paths
	(princ (format "data-directory:\n%S\n" data-directory)
	       'external-debugging-output))

    (setq data-directory-list (paths-construct-data-directory-list
			       data-directory early-package-hierarchies
			       late-package-hierarchies last-package-hierarchies))
    (if debug-paths
	(princ (format "data-directory-list:\n%S\n" data-directory-list)
	       'external-debugging-output))))

(defun startup-find-load-path-for-packages (packages)
  "Return a suitable load-path for PACKAGES.
PACKAGES is a list of package names (strings).  This looks for package
directories in the load path whose last component is one of the members of
PACKAGES."
  (mapcan
   #'(lambda (package)
       (and (member (file-name-nondirectory (directory-file-name package))
		    packages)
	    (list package)))
   (startup-find-load-path)))

; (defun startup-set-basic-packages-load-path ()
;   "#### This is a hack.  When recompiling .el files, we use -no-packages
; to avoid problems with packages shadowing standard Lisp files
; (e.g. unicode.el), but we really still need the stuff in xemacs-base and
; xemacs-devel."
;   (setq load-path (startup-find-load-path-for-packages
; 		   '("xemacs-base" "xemacs-devel"))))

(defun startup-setup-paths-warning ()
  (let ((warnings '()))
    (cond
     ((null (and lisp-directory exec-directory data-directory doc-directory
		 load-path))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lisp-directory) (push "lisp-directory" warnings))
	(if (and (featurep 'mule)
		 (null mule-lisp-directory))
	    (push "mule-lisp-directory" warnings))
	(if (null exec-directory) (push "exec-directory" warnings))
	(if (null data-directory) (push "data-directory" warnings))
	(if (null doc-directory)  (push "doc-directory"  warnings))
	(if (null load-path)      (push "load-path"      warnings))

	(insert "Couldn't find obvious defaults for:\n")
	(while warnings
	  (insert (car warnings) "\n")
	  (setq warnings (cdr warnings)))
	(insert "Perhaps some directories don't exist, "
		"or the XEmacs executable,\n" (concat invocation-directory
						     invocation-name)
		"\nis in a strange place?")

	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))


;;; Now actually set the paths up, for bootstrapping purposes.  This is run
;;; at early dump time and in certain cases where we use a minimal temacs
;;; to do useful things, like rebuild DOC.

(startup-setup-paths (if inhibit-all-packages t '(early last)) t)


(defun startup-load-autoloads ()
  (when (and (not inhibit-autoloads) lisp-directory)
    (load (expand-file-name (file-name-sans-extension autoload-file-name)
			    lisp-directory)
	  nil t)
    (when (featurep 'mule)
      (load (expand-file-name (file-name-sans-extension autoload-file-name)
			      (file-name-as-directory
			       (expand-file-name "mule" lisp-directory)))
	    nil t)))

  ;; Hey!  Let's use a packages-* function for a non-package purpose!
  (when (and (not inhibit-autoloads) (featurep 'modules))
    (packages-load-package-auto-autoloads module-load-path))

  (unless (or inhibit-autoloads inhibit-all-packages)
    (unless inhibit-early-packages
      (packages-load-package-auto-autoloads early-package-load-path))
    (packages-load-package-auto-autoloads late-package-load-path)
    (packages-load-package-auto-autoloads last-package-load-path)))

;;; startup.el ends here
