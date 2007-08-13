;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'electric-autoloads))
    (progn

;;;### (autoloads (electric-buffer-list) "ebuff-menu" "electric/ebuff-menu.el")

(autoload 'electric-buffer-list "ebuff-menu" "\
Pops up a buffer describing the set of Emacs buffers.
Vaguely like ITS lunar select buffer; combining typeoutoid buffer
listing with menuoid buffer selection.

If the very next character typed is a space then the buffer list
window disappears.  Otherwise, one may move around in the buffer list
window, marking buffers to be selected, saved or deleted.

To exit and select a new buffer, type a space when the cursor is on
the appropriate line of the buffer-list window.  Other commands are
much like those of buffer-menu-mode.

Calls value of `electric-buffer-menu-mode-hook' on entry if non-nil.

Non-null optional arg FILES-ONLY means mention only file buffers.
When called from Lisp code, FILES-ONLY may be a regular expression,
in which case only buffers whose names match that expression are listed,
or an arbitrary predicate function.

\\{electric-buffer-menu-mode-map}" t nil)

;;;***

;;;### (autoloads (electric-command-history Electric-command-history-redo-expression) "echistory" "electric/echistory.el")

(autoload 'Electric-command-history-redo-expression "echistory" "\
Edit current history line in minibuffer and execute result.
With prefix arg NOCONFIRM, execute current line as-is without editing." t nil)

(autoload 'electric-command-history "echistory" "\
\\<electric-history-map>Major mode for examining and redoing commands from `command-history'.
This pops up a window with the Command History listing.
The number of command listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Combines typeout Command History list window with menu like selection
of an expression from the history for re-evaluation in the *original* buffer.

The history displayed is filtered by `list-command-history-filter' if non-nil.

Like Emacs-Lisp mode except that characters do not insert themselves and
Tab and Linefeed do not indent.  Instead these commands are provided:
\\{electric-history-map}

Calls the value of `electric-command-history-hook' if that is non-nil.
The Command History listing is recomputed each time this mode is invoked." t nil)

;;;***

;;;### (autoloads (electric-helpify with-electric-help) "ehelp" "electric/ehelp.el")

(autoload 'with-electric-help "ehelp" "\
Pop up an \"electric\" help buffer.
The arguments are THUNK &optional BUFFER NOERASE MINHEIGHT.
THUNK is a function of no arguments which is called to initialize the
contents of BUFFER.  BUFFER defaults to `*Help*'.  BUFFER will be
erased before THUNK is called unless NOERASE is non-nil.  THUNK will
be called while BUFFER is current and with `standard-output' bound to
the buffer specified by BUFFER.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those things.

After THUNK has been called, this function \"electrically\" pops up a window
in which BUFFER is displayed and allows the user to scroll through that buffer
in electric-help-mode. The window's height will be at least MINHEIGHT if
this value is non-nil.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those
things.

When the user exits (with `electric-help-exit', or otherwise) the help
buffer's window disappears (i.e., we use `save-window-excursion')
BUFFER is put into `default-major-mode' (or `fundamental-mode') when we exit." nil nil)

(autoload 'electric-helpify "ehelp" nil nil nil)

;;;***

;;;### (autoloads (Helper-help Helper-describe-bindings) "helper" "electric/helper.el")

(autoload 'Helper-describe-bindings "helper" "\
Describe local key bindings of current mode." t nil)

(autoload 'Helper-help "helper" "\
Provide help for current mode." t nil)

;;;***

(provide 'electric-autoloads)
))
