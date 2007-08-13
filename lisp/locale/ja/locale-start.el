(defun startup-splash-frame-body ()
  `("\n" ,(emacs-version) "\n"
    (face bold-italic "\
Copyright (C) 1985-1996 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1996 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois
Copyright (C) 1995-1996 Ben Wing\n\n")
    
    ,@(if (featurep 'sparcworks)
          `( "\
Sun provides support for the WorkShop/XEmacs integration package only.
All other XEmacs packages are provided to you \"AS IS\".
For full details, type " (key describe-no-warranty)
" to refer to the GPL Version 2, dated June 1991.\n\n"
,@(let ((lang (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG"))))
    (if (and
         (not (featurep 'mule))         ; Already got mule?
         (not (eq 'tty (console-type))) ; No Mule support on tty's yet
         lang                           ; Non-English locale?
         (not (string-equal lang "C"))
         (not (string-match "^en" lang))
         (locate-file "xemacs-mule" exec-path)) ; Comes with Sun WorkShop
        '( "\
This version of XEmacs has been built with support for Latin-1 languages only.
To handle other languages you need to run a Multi-lingual (`Mule') version of
XEmacs, by either running the command `xemacs-mule', or by using the X resource
`ESERVE*defaultXEmacsPath: xemacs-mule' when starting XEmacs from Sun WorkShop.\n\n"))))

        '("XEmacs comes with ABSOLUTELY NO WARRANTY; type "
          (key describe-no-warranty) " for full details.\n"))
    
    "You may give out copies of XEmacs; type "
    (key describe-copying) " to see the conditions.\n"
    "Type " (key describe-distribution)
    " for information on getting the latest version.\n\n"

    "Type " (key help-command) " or use the " (face bold "Help") " menu to get help.\n"
    "Type " (key advertised-undo) " to undo changes  (`C-' means use the Control key).\n"
    "To get out of XEmacs, type " (key save-buffers-kill-emacs) ".\n"
    "Type " (key help-with-tutorial) " for a tutorial on using XEmacs.\n"
    "Type " (key info) " to enter Info, "
    "which you can use to read online documentation.\n\n"
    (face (bold red) ( "\
For tips and answers to frequently asked questions, see the XEmacs FAQ.
\(It's on the Help menu, or type " (key xemacs-local-faq) " [a capital F!].\)"))))


(defun command-line-do-help (arg)
  "Print the $(BF|K\(B XEmacs usage message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (concat "\n" (emacs-version) "\n\n"))
    (princ
     (if (featurep 'x)
	 (concat "XEmacs accepts all standard X Toolkit command line options.\n"
		 "In addition, the")
       "The"))
    (princ " following options are accepted:

  -t <device>           Use TTY <device> instead of the terminal for input
                        and output.  This implies the -nw option.
  -nw                   Inhibit the use of any window-system-specific
                        display code: use the current tty.
  -batch                Execute noninteractively (messages go to stderr).
  -debug-init           Enter the debugger if an error in the init file occurs.
  -unmapped             Do not map the initial frame.
  -no-site-file         Do not load the site-specific init file (site-start.el).
  -no-init-file         Do not load the user-specific init file (~/.emacs).
  -q                    Same as -no-init-file.
  -user <user>          Load user's init file instead of your own.
  -u <user>             Same as -user.\n")
   (let ((l command-switch-alist)
	  (insert (lambda (&rest x)
		    (princ "  ")
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

Anything else is considered a file name, and is placed into a buffer for
editing.

XEmacs has an online tutorial and manuals.  Type ^Ht (Control-h t) after
starting XEmacs to run the tutorial.  Type ^Hi to enter the manual browser.
Type ^H^H^H (Control-h Control-h Control-h) to get more help options.\n")

    (kill-emacs 0)))
