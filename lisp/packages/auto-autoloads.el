;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'packages-autoloads))
    (progn

;;;### (autoloads (add-log-current-defun change-log-mode add-change-log-entry-other-window add-change-log-entry find-change-log prompt-for-change-log-name) "add-log" "packages/add-log.el")

(autoload 'prompt-for-change-log-name "add-log" "\
Prompt for a change log name." nil nil)

(autoload 'find-change-log "add-log" "\
Find a change log file for \\[add-change-log-entry] and return the name.

Optional arg FILE-NAME specifies the file to use.
If FILE-NAME is nil, use the value of `change-log-default-name'.
If 'change-log-default-name' is nil, behave as though it were 'ChangeLog'
\(or whatever we use on this operating system).

If 'change-log-default-name' contains a leading directory component, then
simply find it in the current directory.  Otherwise, search in the current 
directory and its successive parents for a file so named.

Once a file is found, `change-log-default-name' is set locally in the
current buffer to the complete file name." nil nil)

(autoload 'add-change-log-entry "add-log" "\
Find change log file and add an entry for today.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  If nil, uses `change-log-default-name'.
Third arg OTHER-WINDOW non-nil means visit in other window.
Fourth arg NEW-ENTRY non-nil means always create a new entry at the front;
never append to an existing entry.  Today's date is calculated according to
`change-log-time-zone-rule' if non-nil, otherwise in local time." t nil)

(autoload 'add-change-log-entry-other-window "add-log" "\
Find change log file in other window and add an entry for today.
Optional arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.  If nil, uses `change-log-default-name'." t nil)

(define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)

(autoload 'change-log-mode "add-log" "\
Major mode for editing change logs; like Indented Text Mode.
Prevents numeric backups and sets `left-margin' to 8 and `fill-column' to 74.
New log entries are usually made with \\[add-change-log-entry] or \\[add-change-log-entry-other-window].
Each entry behaves as a paragraph, and the entries for one day as a page.
Runs `change-log-mode-hook'." t nil)

(defvar add-log-lisp-like-modes '(emacs-lisp-mode lisp-mode scheme-mode lisp-interaction-mode) "\
*Modes that look like Lisp to `add-log-current-defun'.")

(defvar add-log-c-like-modes '(c-mode c++-mode c++-c-mode objc-mode) "\
*Modes that look like C to `add-log-current-defun'.")

(defvar add-log-tex-like-modes '(TeX-mode plain-TeX-mode LaTeX-mode plain-tex-mode latex-mode) "\
*Modes that look like TeX to `add-log-current-defun'.")

(autoload 'add-log-current-defun "add-log" "\
Return name of function definition point is in, or nil.

Understands C, Lisp, LaTeX (\"functions\" are chapters, sections, ...),
Texinfo (@node titles), Perl, and Fortran.

Other modes are handled by a heuristic that looks in the 10K before
point for uppercase headings starting in the first column or
identifiers followed by `:' or `=', see variable
`add-log-current-defun-header-regexp'.

Has a preference of looking backwards." nil nil)

;;;***

;;;### (autoloads (apropos-documentation apropos-value apropos apropos-command) "apropos" "packages/apropos.el")

(fset 'command-apropos 'apropos-command)

(autoload 'apropos-command "apropos" "\
Shows commands (interactively callable functions) that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
variables." t nil)

(autoload 'apropos "apropos" "\
Show all bound symbols whose names match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show unbound
symbols and key bindings, which is a little more time-consuming.
Returns list of symbols and documentation found." t nil)

(autoload 'apropos-value "apropos" "\
Show all symbols whose value's printed image matches REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also looks
at the function and at the names and values of properties.
Returns list of symbols and values found." t nil)

(autoload 'apropos-documentation "apropos" "\
Show symbols whose documentation contain matches for REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also use
documentation that is not stored in the documentation file and show key
bindings.
Returns list of symbols and documentation found." t nil)

;;;***

;;;### (autoloads (define-auto-insert auto-insert) "autoinsert" "packages/autoinsert.el")

(autoload 'auto-insert "autoinsert" "\
Insert default contents into a new file if `auto-insert' is non-nil.
Matches the visited file name against the elements of `auto-insert-alist'." t nil)

(autoload 'define-auto-insert "autoinsert" "\
Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs." nil nil)

;;;***

;;;### (autoloads (mouse-avoidance-mode) "avoid" "packages/avoid.el")

(defvar mouse-avoidance-mode nil "\
Value is t or a symbol if the mouse pointer should avoid the cursor.
See function `mouse-avoidance-mode' for possible values.  Changing this
variable is NOT the recommended way to change modes; use that function 
instead.")

(autoload 'mouse-avoidance-mode "avoid" "\
Set cursor avoidance mode to MODE.
MODE should be one of the symbols `banish', `exile', `jump', `animate',
`cat-and-mouse', `proteus', or `none'.

If MODE is nil, toggle mouse avoidance between `none` and `banish'
modes.  Positive numbers and symbols other than the above are treated
as equivalent to `banish'; negative numbers and `-' are equivalent to `none'.

Effects of the different modes: 
 * banish: Move the mouse to the upper-right corner on any keypress.
 * exile: Move the mouse to the corner only if the cursor gets too close,
     and allow it to return once the cursor is out of the way.
 * jump: If the cursor gets too close to the mouse, displace the mouse
     a random distance & direction.
 * animate: As `jump', but shows steps along the way for illusion of motion.
 * cat-and-mouse: Same as `animate'.
 * proteus: As `animate', but changes the shape of the mouse pointer too.

Whenever the mouse is moved, the frame is also raised.

\(see `mouse-avoidance-threshold' for definition of \"too close\",
and `mouse-avoidance-nudge-dist' and `mouse-avoidance-nudge-var' for
definition of \"random distance\".)" t nil)

(add-minor-mode 'mouse-avoidance-mode " Avoid")

;;;***

;;;### (autoloads (blink-cursor-mode) "blink-cursor" "packages/blink-cursor.el")

(autoload 'blink-cursor-mode "blink-cursor" "\
Enable or disable a blinking cursor.
If TIMEOUT is nil, toggle on or off.
If TIMEOUT is t, enable with the previous timeout value.
If TIMEOUT is 0, disable.
If TIMEOUT is greater than 0, then the cursor will blink once
each TIMEOUT secs (can be a float)." t nil)

;;;***

;;;### (autoloads (bookmark-menu-delete bookmark-menu-rename bookmark-menu-locate bookmark-menu-jump bookmark-menu-insert bookmark-bmenu-list bookmark-load bookmark-save bookmark-write bookmark-delete bookmark-insert bookmark-rename bookmark-insert-location bookmark-relocate bookmark-jump bookmark-set) "bookmark" "packages/bookmark.el")

(if (symbolp (key-binding "r")) nil (progn (define-key ctl-x-map "rb" 'bookmark-jump) (define-key ctl-x-map "rm" 'bookmark-set) (define-key ctl-x-map "rl" 'bookmark-bmenu-list)))

(defvar bookmark-map nil "\
Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

(define-prefix-command 'bookmark-map)

(define-key bookmark-map "x" 'bookmark-set)

(define-key bookmark-map "m" 'bookmark-set)

(define-key bookmark-map "j" 'bookmark-jump)

(define-key bookmark-map "g" 'bookmark-jump)

(define-key bookmark-map "i" 'bookmark-insert)

(define-key bookmark-map "e" 'edit-bookmarks)

(define-key bookmark-map "f" 'bookmark-insert-location)

(define-key bookmark-map "r" 'bookmark-rename)

(define-key bookmark-map "d" 'bookmark-delete)

(define-key bookmark-map "l" 'bookmark-load)

(define-key bookmark-map "w" 'bookmark-write)

(define-key bookmark-map "s" 'bookmark-save)

(add-hook 'kill-emacs-hook (function (lambda nil (and (featurep 'bookmark) bookmark-alist (bookmark-time-to-save-p t) (bookmark-save)))))

(autoload 'bookmark-set "bookmark" "\
Set a bookmark named NAME inside a file.
If name is nil, then the user will be prompted.
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts the name of the last bookmark used in the buffer
\(as an aid in using a single bookmark name to track your progress
through a large file).  If no bookmark was used, then C-u inserts the
name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks (you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.)" t nil)

(autoload 'bookmark-jump "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and bookmark-jump
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record." t nil)

(autoload 'bookmark-relocate "bookmark" "\
Relocate BOOKMARK to another file (reading file name with minibuffer).
This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it." t nil)

(autoload 'bookmark-insert-location "bookmark" "\
Insert the name of the file associated with BOOKMARK.
Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'." t nil)

(defalias 'bookmark-locate 'bookmark-insert-location)

(autoload 'bookmark-rename "bookmark" "\
Change the name of OLD bookmark to NEW name.
If called from keyboard, prompt for OLD and NEW.  If called from
menubar, select OLD from a menu and prompt for NEW.

If called from Lisp, prompt for NEW if only OLD was passed as an
argument.  If called with two strings, then no prompting is done.  You
must pass at least OLD when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name." t nil)

(autoload 'bookmark-insert "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this." t nil)

(autoload 'bookmark-delete "bookmark" "\
Delete BOOKMARK from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there." t nil)

(autoload 'bookmark-write "bookmark" "\
Write bookmarks to a file (reading the file name with the minibuffer).
Don't use this in Lisp programs; use `bookmark-save' instead." t nil)

(autoload 'bookmark-save "bookmark" "\
Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-default-file'.  With a prefix arg, save it in file FILE
\(second argument).

If you are calling this from Lisp, the two arguments are PREFIX-ARG
and FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
`bookmark-load', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'." t nil)

(autoload 'bookmark-load "bookmark" "\
Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument REVERT is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs.bmk', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly." t nil)

(autoload 'bookmark-bmenu-list "bookmark" "\
Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying." t nil)

(defalias 'list-bookmarks 'bookmark-bmenu-list)

(defalias 'edit-bookmarks 'bookmark-bmenu-list)

(autoload 'bookmark-menu-insert "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-jump "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-locate "bookmark" "\
Insert the name of the file associated with BOOKMARK. 
\(This is not the same as the contents of that file).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-rename "bookmark" "\
Change the name of OLD-BOOKMARK to NEWNAME.  
If called from keyboard, prompts for OLD-BOOKMARK and NEWNAME.
If called from menubar, OLD-BOOKMARK is selected from a menu, and
prompts for NEWNAME. 
If called from Lisp, prompts for NEWNAME if only OLD-BOOKMARK was
passed as an argument.  If called with two strings, then no prompting
is done.  You must pass at least OLD-BOOKMARK when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-delete "bookmark" "\
Delete the bookmark named NAME from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(defvar menu-bar-bookmark-map (make-sparse-keymap "Bookmark functions"))

(defalias 'menu-bar-bookmark-map (symbol-value 'menu-bar-bookmark-map))

(define-key menu-bar-bookmark-map [load] '("Load a Bookmark File..." . bookmark-load))

(define-key menu-bar-bookmark-map [write] '("Save Bookmarks As..." . bookmark-write))

(define-key menu-bar-bookmark-map [save] '("Save Bookmarks" . bookmark-save))

(define-key menu-bar-bookmark-map [edit] '("Edit Bookmark List" . bookmark-bmenu-list))

(define-key menu-bar-bookmark-map [delete] '("Delete Bookmark" . bookmark-menu-delete))

(define-key menu-bar-bookmark-map [rename] '("Rename Bookmark" . bookmark-menu-rename))

(define-key menu-bar-bookmark-map [locate] '("Insert Location" . bookmark-menu-locate))

(define-key menu-bar-bookmark-map [insert] '("Insert Contents" . bookmark-menu-insert))

(define-key menu-bar-bookmark-map [set] '("Set Bookmark" . bookmark-set))

(define-key menu-bar-bookmark-map [jump] '("Jump to Bookmark" . bookmark-menu-jump))

;;;***

;;;### (autoloads nil "buff-menu" "packages/buff-menu.el")

(defvar list-buffers-directory nil)

(make-variable-buffer-local 'list-buffers-directory)

;;;***

;;;### (autoloads (command-history-mode list-command-history repeat-matching-complex-command) "chistory" "packages/chistory.el")

(autoload 'repeat-matching-complex-command "chistory" "\
Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you select
a form for evaluation.  If PATTERN is empty (or nil), every form in the
command history is offered.  The form is placed in the minibuffer for
editing and the result is evaluated." t nil)

(autoload 'list-command-history "chistory" "\
List history of commands typed to minibuffer.
The number of commands listed is controlled by `list-command-history-max'.
Calls value of `list-command-history-filter' (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode." t nil)

(autoload 'command-history-mode "chistory" "\
Major mode for examining commands from `command-history'.
The number of commands listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Use \\<command-history-map>\\[command-history-repeat] to repeat the command on the current line.

Otherwise much like Emacs-Lisp Mode except that there is no self-insertion
and digits provide prefix arguments.  Tab does not indent.
\\{command-history-map}
Calls the value of `command-history-hook' if that is non-nil.
The Command History listing is recomputed each time this mode is invoked." t nil)

;;;***

;;;### (autoloads nil "cmuscheme" "packages/cmuscheme.el")

(add-hook 'same-window-buffer-names "*scheme*")

;;;***

;;;### (autoloads (compare-windows) "compare-w" "packages/compare-w.el")

(autoload 'compare-windows "compare-w" "\
Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match.

This command pushes the mark in each window
at the prior location of point in that window.
If both windows display the same buffer,
the mark is pushed twice in that buffer:
first in the other window, then in the selected window.

A prefix arg means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also ignored." t nil)

;;;***

;;;### (autoloads (first-error previous-error next-error compilation-minor-mode grep compile) "compile" "packages/compile.el")

(defcustom compilation-mode-hook nil "*List of hook functions run by `compilation-mode' (see `run-hooks')." :type 'hook :group 'compilation)

(defcustom compilation-window-height nil "*Number of lines in a compilation window.  If nil, use Emacs default." :type '(choice (const nil) integer) :group 'compilation)

(defcustom compilation-buffer-name-function nil "Function to compute the name of a compilation buffer.\nThe function receives one argument, the name of the major mode of the\ncompilation buffer.  It should return a string.\nnil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'." :type 'function :group 'compilation)

(defcustom compilation-finish-function nil "*Function to call when a compilation process finishes.\nIt is called with two arguments: the compilation buffer, and a string\ndescribing how the process finished." :type 'function :group 'compilation)

(defcustom compilation-search-path '(nil) "*List of directories to search for source files named in error messages.\nElements should be directory names, not file names of directories.\nnil as an element means to try the default directory." :type '(repeat (choice (const :tag "Default" nil) directory)) :group 'compilation)

(autoload 'compile "compile" "\
Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name." t nil)

(autoload 'grep "compile" "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command." t nil)

(autoload 'compilation-minor-mode "compile" "\
Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
! \\{compilation-mode-map}" t nil)

(autoload 'next-error "compile" "\
Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally applies to the most recent compilation started,
but as long as you are in the middle of parsing errors from one compilation
output buffer, you stay with that compilation output buffer.

Use \\[next-error] in a compilation output buffer to switch to
processing errors from that compilation.

See variables `compilation-parse-errors-function' and
`compilation-error-regexp-alist' for customization ideas." t nil)

(define-key ctl-x-map "`" 'next-error)

(autoload 'previous-error "compile" "\
Visit previous compilation error message and corresponding source code.
This operates on the output from the \\[compile] command." t nil)

(autoload 'first-error "compile" "\
Reparse the error message buffer and start at the first error
Visit corresponding source code.
This operates on the output from the \\[compile] command." t nil)

;;;***

;;;### (autoloads (dabbrev-expand dabbrev-completion) "dabbrev" "packages/dabbrev.el")

(define-key global-map [(meta /)] 'dabbrev-expand)

(define-key global-map [(meta control /)] 'dabbrev-completion)

(autoload 'dabbrev-completion "dabbrev" "\
Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from C-u C-u),
then it searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already." t nil)

(autoload 'dabbrev-expand "dabbrev" "\
Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function'.

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]." t nil)

;;;***

;;;### (autoloads (diff-backup diff) "diff" "packages/diff.el")

(defcustom diff-switches "-c" "*A list of switches (strings) to pass to the diff program." :type '(choice string (repeat string)) :group 'diff)

(autoload 'diff "diff" "\
Find and display the differences between OLD and NEW files.
Interactively you are prompted with the current buffer's file name for NEW
and what appears to be its backup for OLD." t nil)

(autoload 'diff-backup "diff" "\
Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'." t nil)

;;;***

;;;### (autoloads (edit-faces) "edit-faces" "packages/edit-faces.el")

(autoload 'edit-faces "edit-faces" "\
Alter face characteristics by editing a list of defined faces.
Pops up a buffer containing a list of defined faces.

Editing commands:

\\{edit-faces-mode-map}" t nil)

;;;***

;;;### (autoloads (report-xemacs-bug) "emacsbug" "packages/emacsbug.el")

(autoload 'report-xemacs-bug "emacsbug" "\
Report a bug in XEmacs.
Prompts for bug subject.  Leaves you in a mail buffer." t nil)

;;;***

;;;### (autoloads (emerge-merge-directories emerge-revisions-with-ancestor emerge-revisions emerge-files-with-ancestor-remote emerge-files-remote emerge-files-with-ancestor-command emerge-files-command emerge-buffers-with-ancestor emerge-buffers emerge-files-with-ancestor emerge-files) "emerge" "packages/emerge.el")

(autoload 'emerge-files "emerge" "\
Run Emerge on two files." t nil)

(fset 'emerge 'emerge-files)

(autoload 'emerge-files-with-ancestor "emerge" "\
Run Emerge on two files, giving another file as the ancestor." t nil)

(autoload 'emerge-buffers "emerge" "\
Run Emerge on two buffers." t nil)

(autoload 'emerge-buffers-with-ancestor "emerge" "\
Run Emerge on two buffers, giving another buffer as the ancestor." t nil)

(autoload 'emerge-files-command "emerge" nil nil nil)

(autoload 'emerge-files-with-ancestor-command "emerge" nil nil nil)

(autoload 'emerge-files-remote "emerge" nil nil nil)

(autoload 'emerge-files-with-ancestor-remote "emerge" nil nil nil)

(autoload 'emerge-revisions "emerge" "\
Emerge two RCS revisions of a file." t nil)

(autoload 'emerge-revisions-with-ancestor "emerge" "\
Emerge two RCS revisions of a file, with another revision as ancestor." t nil)

(autoload 'emerge-merge-directories "emerge" nil t nil)

;;;***

;;;### (autoloads (tags-apropos list-tags tags-query-replace tags-search tags-loop-continue next-file find-tag-other-window find-tag visit-tags-table) "etags" "packages/etags.el")

(defcustom tags-build-completion-table 'ask "*If this variable is nil, then tags completion is disabled.\nIf this variable is t, then things which prompt for tags will do so with \n completion across all known tags.\nIf this variable is the symbol `ask', then you will be asked whether each\n tags table should be added to the completion list as it is read in.\n (With the exception that for very small tags tables, you will not be asked,\n since they can be parsed quickly.)" :type '(radio (const :tag "Disabled" nil) (const :tag "Complete All" t) (const :tag "Ask" ask)) :group 'etags)

(defcustom tags-always-exact nil "*If this variable is non-nil, then tags always looks for exact matches." :type 'boolean :group 'etags)

(defcustom tag-table-alist nil "*A list which determines which tags files should be active for a \ngiven buffer.  This is not really an association list, in that all \nelements are checked.  The CAR of each element of this list is a \npattern against which the buffer's file name is compared; if it \nmatches, then the CDR of the list should be the name of the tags\ntable to use.  If more than one element of this list matches the\nbuffer's file name, then all of the associated tags tables will be\nused.  Earlier ones will be searched first.\n\nIf the CAR of elements of this list are strings, then they are treated\nas regular-expressions against which the file is compared (like the\nauto-mode-alist).  If they are not strings, then they are evaluated.\nIf they evaluate to non-nil, then the current buffer is considered to\nmatch.\n\nIf the CDR of the elements of this list are strings, then they are\nassumed to name a TAGS file.  If they name a directory, then the string\n\"TAGS\" is appended to them to get the file name.  If they are not \nstrings, then they are evaluated, and must return an appropriate string.\n\nFor example:\n  (setq tag-table-alist\n	'((\"/usr/src/public/perl/\" . \"/usr/src/public/perl/perl-3.0/\")\n	 (\"\\\\.el$\" . \"/usr/local/emacs/src/\")\n	 (\"/jbw/gnu/\" . \"/usr15/degree/stud/jbw/gnu/\")\n	 (\"\" . \"/usr/local/emacs/src/\")\n	 ))\n\nThis means that anything in the /usr/src/public/perl/ directory should use\nthe TAGS file /usr/src/public/perl/perl-3.0/TAGS; and file ending in .el should\nuse the TAGS file /usr/local/emacs/src/TAGS; and anything in or below the\ndirectory /jbw/gnu/ should use the TAGS file /usr15/degree/stud/jbw/gnu/TAGS.\nA file called something like \"/usr/jbw/foo.el\" would use both the TAGS files\n/usr/local/emacs/src/TAGS and /usr15/degree/stud/jbw/gnu/TAGS (in that order)\nbecause it matches both patterns.\n\nIf the buffer-local variable `buffer-tag-table' is set, then it names a tags\ntable that is searched before all others when find-tag is executed from this\nbuffer.\n\nIf there is a file called \"TAGS\" in the same directory as the file in \nquestion, then that tags file will always be used as well (after the\n`buffer-tag-table' but before the tables specified by this list.)\n\nIf the variable tags-file-name is set, then the tags file it names will apply\nto all buffers (for backwards compatibility.)  It is searched first.\n" :type '(repeat (cons regexp sexp)) :group 'etags)

(autoload 'visit-tags-table "etags" "\
Tell tags commands to use tags table file FILE first.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory." t nil)

(autoload 'find-tag "etags" "\
*Find tag whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If called interactively with a numeric argument, searches for the next tag
in the tag table that matches the tagname used in the previous find-tag.
 If second arg OTHER-WINDOW is non-nil, uses another window to display
the tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember" t nil)

(autoload 'find-tag-other-window "etags" "\
*Find tag whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember" t nil)

(autoload 'next-file "etags" "\
Select next file among files in current tag table(s).

A first argument of t (prefix arg, if interactive) initializes to the
beginning of the list of files in the (first) tags table.  If the argument
is neither nil nor t, it is evalled to initialize the list of files.

Non-nil second argument NOVISIT means use a temporary buffer
to save time and avoid uninteresting warnings.

Value is nil if the file was already visited;
if the file was newly read in, the value is the filename." t nil)

(autoload 'tags-loop-continue "etags" "\
Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).
Two variables control the processing we do on each file:
the value of `tags-loop-scan' is a form to be executed on each file
to see if it is interesting (it returns non-nil if so)
and `tags-loop-operate' is a form to execute to operate on an interesting file
If the latter returns non-nil, we exit; otherwise we scan the next file." t nil)

(autoload 'tags-search "etags" "\
Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable `tag-table-alist'." t nil)

(autoload 'tags-query-replace "etags" "\
Query-replace-regexp FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable `tag-table-alist'." t nil)

(autoload 'list-tags "etags" "\
Display list of tags in file FILE.
FILE should not contain a directory spec
unless it has one in the tag table." t nil)

(autoload 'tags-apropos "etags" "\
Display list of all tags in tag table REGEXP matches." t nil)

;;;***

;;;### (autoloads (turn-on-fast-lock fast-lock-mode) "fast-lock" "packages/fast-lock.el")

(autoload 'fast-lock-mode "fast-lock" "\
Toggle Fast Lock mode.
With arg, turn Fast Lock mode on if and only if arg is positive and the buffer
is associated with a file.  Enable it automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'fast-lock-mode)

If Fast Lock mode is enabled, and the current buffer does not contain any text
properties, any associated Font Lock cache is used if its timestamp matches the
buffer's file, and its `font-lock-keywords' match those that you are using.

Font Lock caches may be saved:
- When you save the file's buffer.
- When you kill an unmodified file's buffer.
- When you exit Emacs, for all unmodified or saved buffers.
Depending on the value of `fast-lock-save-events'.
See also the commands `fast-lock-read-cache' and `fast-lock-save-cache'.

Use \\[font-lock-fontify-buffer] to fontify the buffer if the cache is bad.

Various methods of control are provided for the Font Lock cache.  In general,
see variable `fast-lock-cache-directories' and function `fast-lock-cache-name'.
For saving, see variables `fast-lock-minimum-size', `fast-lock-save-events',
`fast-lock-save-others' and `fast-lock-save-faces'." t nil)

(autoload 'turn-on-fast-lock "fast-lock" "\
Unconditionally turn on Fast Lock mode." nil nil)

(when (fboundp 'add-minor-mode) (defvar fast-lock-mode nil) (add-minor-mode 'fast-lock-mode nil))

;;;***

;;;### (autoloads (feedmail-send-it) "feedmail" "packages/feedmail.el")

(autoload 'feedmail-send-it "feedmail" nil nil nil)

;;;***

;;;### (autoloads (make-file-part) "file-part" "packages/file-part.el")

(autoload 'make-file-part "file-part" "\
Make a file part on buffer BUFFER out of the region.  Call it NAME.
This command creates a new buffer containing the contents of the
region and marks the buffer as referring to the specified buffer,
called the `master buffer'.  When the file-part buffer is saved,
its changes are integrated back into the master buffer.  When the
master buffer is deleted, all file parts are deleted with it.

When called from a function, expects four arguments, START, END,
NAME, and BUFFER, all of which are optional and default to the
beginning of BUFFER, the end of BUFFER, a name generated from
BUFFER's name, and the current buffer, respectively." t nil)

;;;***

;;;### (autoloads (font-lock-set-defaults-1 font-lock-fontify-buffer turn-off-font-lock turn-on-font-lock font-lock-mode) "font-lock" "packages/font-lock.el")

(defvar font-lock-auto-fontify t "\
*Whether font-lock should automatically fontify files as they're loaded.
This will only happen if font-lock has fontifying keywords for the major
mode of the file.  You can get finer-grained control over auto-fontification
by using this variable in combination with `font-lock-mode-enable-list' or
`font-lock-mode-disable-list'.")

(defvar font-lock-mode-enable-list nil "\
*List of modes to auto-fontify, if `font-lock-auto-fontify' is nil.")

(defvar font-lock-mode-disable-list nil "\
*List of modes not to auto-fontify, if `font-lock-auto-fontify' is t.")

(defvar font-lock-use-colors '(color) "\
*Specification for when Font Lock will set up color defaults.
Normally this should be '(color), meaning that Font Lock will set up
color defaults that are only used on color displays.  Set this to nil
if you don't want Font Lock to set up color defaults at all.  This
should be one of

-- a list of valid tags, meaning that the color defaults will be used
   when all of the tags apply. (e.g. '(color x))
-- a list whose first element is 'or and whose remaining elements are
   lists of valid tags, meaning that the defaults will be used when
   any of the tag lists apply.
-- nil, meaning that the defaults should not be set up at all.

\(If you specify face values in your init file, they will override any
that Font Lock specifies, regardless of whether you specify the face
values before or after loading Font Lock.)

See also `font-lock-use-fonts'.  If you want more control over the faces
used for fontification, see the documentation of `font-lock-mode' for
how to do it.")

(defvar font-lock-use-fonts '(or (mono) (grayscale)) "\
*Specification for when Font Lock will set up non-color defaults.

Normally this should be '(or (mono) (grayscale)), meaning that Font
Lock will set up non-color defaults that are only used on either mono
or grayscale displays.  Set this to nil if you don't want Font Lock to
set up non-color defaults at all.  This should be one of

-- a list of valid tags, meaning that the non-color defaults will be used
   when all of the tags apply. (e.g. '(grayscale x))
-- a list whose first element is 'or and whose remaining elements are
   lists of valid tags, meaning that the defaults will be used when
   any of the tag lists apply.
-- nil, meaning that the defaults should not be set up at all.

\(If you specify face values in your init file, they will override any
that Font Lock specifies, regardless of whether you specify the face
values before or after loading Font Lock.)

See also `font-lock-use-colors'.  If you want more control over the faces
used for fontification, see the documentation of `font-lock-mode' for
how to do it.")

(defvar font-lock-maximum-decoration nil "\
*If non-nil, the maximum decoration level for fontifying.
If nil, use the minimum decoration (equivalent to level 0).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c++-mode . 2) (c-mode . t) (t . 1))
means use level 2 decoration for buffers in `c++-mode', the maximum decoration
available for buffers in `c-mode', and level 1 decoration otherwise.")

(define-obsolete-variable-alias 'font-lock-use-maximal-decoration 'font-lock-maximum-decoration)

(defvar font-lock-maximum-size (* 250 1024) "\
*If non-nil, the maximum size for buffers for fontifying.
Only buffers less than this can be fontified when Font Lock mode is turned on.
If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c++-mode . 256000) (c-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in `c++-mode' or `c-mode', one
megabyte for buffers in `rmail-mode', and size is irrelevant otherwise.")

(defvar font-lock-keywords nil "\
*A list of the keywords to highlight.
Each element should be of the form:

 MATCHER
 (MATCHER . MATCH)
 (MATCHER . FACENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

FORM is an expression, whose value should be a keyword element,
evaluated when the keyword is (first) used in a buffer.  This feature
can be used to provide a keyword that can only be generated when Font
Lock mode is actually turned on.

For highlighting single items, typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items is to be highlighted following the
instance of another item (the anchor) then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (MATCH FACENAME OVERRIDE LAXMATCH)

Where MATCHER can be either the regexp to search for, a variable
containing the regexp to search for, or the function to call to make
the search (called with one argument, the limit of the search).  MATCH
is the subexpression of MATCHER to be highlighted.  FACENAME is either
a symbol naming a face, or an expression whose value is the face name
to use.  If you want FACENAME to be a symbol that evaluates to a face,
use a form like \"(progn sym)\".

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing fontification may
be overwritten.  If `keep', only parts not already fontified are highlighted.
If `prepend' or `append', existing fontification is merged with the new, in
which the new or existing fontification, respectively, takes precedence.
If LAXMATCH is non-nil, no error is signalled if there is no MATCH in MATCHER.

For example, an element of the form highlights (if not already highlighted):

 \"\\\\\\=<foo\\\\\\=>\"		Discrete occurrences of \"foo\" in the value of the
			variable `font-lock-keyword-face'.
 (\"fu\\\\(bar\\\\)\" . 1)	Substring \"bar\" within all occurrences of \"fubar\" in
			the value of `font-lock-keyword-face'.
 (\"fubar\" . fubar-face)	Occurrences of \"fubar\" in the value of `fubar-face'.
 (\"foo\\\\|bar\" 0 foo-bar-face t)
			Occurrences of either \"foo\" or \"bar\" in the value
			of `foo-bar-face', even if already highlighted.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

Where MATCHER is as for MATCH-HIGHLIGHT with one exception.  The limit of the
search is currently guaranteed to be (no greater than) the end of the line.
PRE-MATCH-FORM and POST-MATCH-FORM are evaluated before the first, and after
the last, instance MATCH-ANCHORED's MATCHER is used.  Therefore they can be
used to initialise before, and cleanup after, MATCHER is used.  Typically,
PRE-MATCH-FORM is used to move to some position relative to the original
MATCHER, before starting with MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might
be used to move, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face) (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

 Discrete occurrences of \"anchor\" in the value of `anchor-face', and subsequent
 discrete occurrences of \"item\" (on the same line) in the value of `item-face'.
 (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore \"item\" is
 initially searched for starting from the end of the match of \"anchor\", and
 searching for subsequent instance of \"anchor\" resumes from where searching
 for \"item\" concluded.)

Note that the MATCH-ANCHORED feature is experimental; in the future, we may
replace it with other ways of providing this functionality.

These regular expressions should not match text which spans lines.  While
\\[font-lock-fontify-buffer] handles multi-line patterns correctly, updating
when you edit the buffer does not, since it considers text one line at a time.

Be very careful composing regexps for this list;
the wrong pattern can dramatically slow things down!")

(make-variable-buffer-local 'font-lock-keywords)

(defvar font-lock-mode nil)

(defvar font-lock-mode-hook nil "\
Function or functions to run on entry to font-lock-mode.")

(autoload 'font-lock-mode "font-lock" "\
Toggle Font Lock Mode.
With arg, turn font-lock mode on if and only if arg is positive.

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Documentation strings (in Lisp-like languages) are displayed in
   `font-lock-doc-string-face';
 - Language keywords (\"reserved words\") are displayed in
   `font-lock-keyword-face';
 - Function names in their defining form are displayed in
   `font-lock-function-name-face';
 - Variable names in their defining form are displayed in
   `font-lock-variable-name-face';
 - Type names are displayed in `font-lock-type-face';
 - References appearing in help files and the like are displayed
   in `font-lock-reference-face';
 - Preprocessor declarations are displayed in
  `font-lock-preprocessor-face';

   and

 - Certain other expressions are displayed in other faces according
   to the value of the variable `font-lock-keywords'.

Where modes support different levels of fontification, you can use the variable
`font-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Font Lock mode on/off the buffer is fontified/defontified, though
fontification occurs only if the buffer is less than `font-lock-maximum-size'.
To fontify a buffer without turning on Font Lock mode, and regardless of buffer
size, you can use \\[font-lock-fontify-buffer].

See the variable `font-lock-keywords' for customization." t nil)

(autoload 'turn-on-font-lock "font-lock" "\
Unconditionally turn on Font Lock mode." nil nil)

(autoload 'turn-off-font-lock "font-lock" "\
Unconditionally turn off Font Lock mode." nil nil)

(autoload 'font-lock-fontify-buffer "font-lock" "\
Fontify the current buffer the way `font-lock-mode' would.
See `font-lock-mode' for details.

This can take a while for large buffers." t nil)

(autoload 'font-lock-set-defaults-1 "font-lock" nil nil nil)

(add-minor-mode 'font-lock-mode " Font")

;;;***

;;;### (autoloads (sc-mode) "generic-sc" "packages/generic-sc.el")

(autoload 'sc-mode "generic-sc" "\
Toggle sc-mode.
SYSTEM can be sccs, rcs or cvs.
Cvs requires the pcl-cvs package.

The following commands are available
\\[sc-next-operation]	perform next logical source control operation on current file
\\[sc-show-changes]	compare the version being edited with an older one
\\[sc-version-diff-file]	compare two older versions of a file
\\[sc-show-history]		display change history of current file
\\[sc-visit-previous-revision]	display an older revision of current file
\\[sc-revert-file]		revert buffer to last checked-in version
\\[sc-list-all-locked-files]		show all files locked in current directory
\\[sc-list-locked-files]		show all files locked by you in current directory
\\[sc-list-registered-files]		show all files under source control in current directory
\\[sc-update-directory]		get fresh copies of files checked-in by others in current directory
\\[sc-rename-file]		rename the current file and its source control file


While you are entering a change log message for a check in, sc-log-entry-mode
will be in effect.

Global user options:
    sc-diff-command	A list consisting of the command and flags
			to be used for generating context diffs.
    sc-mode-expert	suppresses some conformation prompts,
			notably for delta aborts and file saves.
    sc-max-log-size	specifies the maximum allowable size
			of a log message plus one.


When using SCCS you have additional commands and options

\\[sccs-insert-headers]		insert source control headers in current file

When you generate headers into a buffer using \\[sccs-insert-headers],
the value of sc-insert-headers-hook is called before insertion. If the
file is recognized a C or Lisp source, sc-insert-c-header-hook or
sc-insert-lisp-header-hook is called after insertion respectively.

    sccs-headers-wanted	which %-keywords to insert when adding
			headers with C-c h
    sccs-insert-static	if non-nil, keywords inserted in C files
			get stuffed in a static string area so that
			what(1) can see them in the compiled object code.

When using CVS you have additional commands

\\[sc-cvs-update-directory]	update the current directory using pcl-cvs
\\[sc-cvs-file-status]		show the CVS status of current file
" t nil)

;;;***

;;;### (autoloads (gnuserv-start gnuserv-running-p) "gnuserv" "packages/gnuserv.el")

(defcustom gnuserv-frame nil "*The frame to be used to display all edited files.\nIf nil, then a new frame is created for each file edited.\nIf t, then the currently selected frame will be used.\nIf a function, then this will be called with a symbol `x' or `tty' as the\nonly argument, and its return value will be interpreted as above." :tag "Gnuserv Frame" :type '(radio (const :tag "Create new frame each time" nil) (const :tag "Use selected frame" t) (function-item :tag "Use main Emacs frame" gnuserv-main-frame-function) (function-item :tag "Use visible frame, otherwise create new" gnuserv-visible-frame-function) (function-item :tag "Create special Gnuserv frame and use it" gnuserv-special-frame-function) (function :tag "Other")) :group 'gnuserv)

(autoload 'gnuserv-running-p "gnuserv" "\
Return non-nil if a gnuserv process is running from this XEmacs session." nil nil)

(autoload 'gnuserv-start "gnuserv" "\
Allow this Emacs process to be a server for client processes.
This starts a gnuserv communications subprocess through which
client \"editors\" (gnuclient and gnudoit) can send editing commands to 
this Emacs job.  See the gnuserv(1) manual page for more details.

Prefix arg means just kill any existing server communications subprocess." t nil)

;;;***

;;;### (autoloads (gopher-atpoint gopher) "gopher" "packages/gopher.el")

(autoload 'gopher "gopher" "\
Start a gopher session.  With C-u, prompt for a gopher server." t nil)

(autoload 'gopher-atpoint "gopher" "\
Try to interpret the text around point as a gopher bookmark, and dispatch
to that object." t nil)

;;;***

;;;### (autoloads (hexlify-buffer hexl-find-file hexl-mode) "hexl" "packages/hexl.el")

(autoload 'hexl-mode "hexl" "\
\\<hexl-mode-map>
A major mode for editing binary files in hex dump format.

This function automatically converts a buffer into the hexl format
using the function `hexlify-buffer'.

Each line in the buffer has an \"address\" (displayed in hexadecimal)
representing the offset into the file that the characters on this line
are at and 16 characters from the file (displayed as hexadecimal
values grouped every 16 bits) and as their ASCII values.

If any of the characters (displayed as ASCII characters) are
unprintable (control or meta characters) they will be replaced as
periods.

If `hexl-mode' is invoked with an argument the buffer is assumed to be
in hexl format.

A sample format:

  HEX ADDR: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f     ASCII-TEXT
  --------  ---- ---- ---- ---- ---- ---- ---- ----  ----------------
  00000000: 5468 6973 2069 7320 6865 786c 2d6d 6f64  This is hexl-mod
  00000010: 652e 2020 4561 6368 206c 696e 6520 7265  e.  Each line re
  00000020: 7072 6573 656e 7473 2031 3620 6279 7465  presents 16 byte
  00000030: 7320 6173 2068 6578 6164 6563 696d 616c  s as hexadecimal
  00000040: 2041 5343 4949 0a61 6e64 2070 7269 6e74   ASCII.and print
  00000050: 6162 6c65 2041 5343 4949 2063 6861 7261  able ASCII chara
  00000060: 6374 6572 732e 2020 416e 7920 636f 6e74  cters.  Any cont
  00000070: 726f 6c20 6f72 206e 6f6e 2d41 5343 4949  rol or non-ASCII
  00000080: 2063 6861 7261 6374 6572 730a 6172 6520   characters.are 
  00000090: 6469 7370 6c61 7965 6420 6173 2070 6572  displayed as per
  000000a0: 696f 6473 2069 6e20 7468 6520 7072 696e  iods in the prin
  000000b0: 7461 626c 6520 6368 6172 6163 7465 7220  table character 
  000000c0: 7265 6769 6f6e 2e0a                      region..

Movement is as simple as movement in a normal emacs text buffer.  Most
cursor movement bindings are the same (ie. Use \\[hexl-backward-char], \\[hexl-forward-char], \\[hexl-next-line], and \\[hexl-previous-line]
to move the cursor left, right, down, and up).

Advanced cursor movement commands (ala \\[hexl-beginning-of-line], \\[hexl-end-of-line], \\[hexl-beginning-of-buffer], and \\[hexl-end-of-buffer]) are
also supported.

There are several ways to change text in hexl mode:

ASCII characters (character between space (0x20) and tilde (0x7E)) are
bound to self-insert so you can simply type the character and it will
insert itself (actually overstrike) into the buffer.

\\[hexl-quoted-insert] followed by another keystroke allows you to insert the key even if
it isn't bound to self-insert.  An octal number can be supplied in place
of another key to insert the octal number's ASCII representation.

\\[hexl-insert-hex-char] will insert a given hexadecimal value (if it is between 0 and 0xFF)
into the buffer at the current point.

\\[hexl-insert-octal-char] will insert a given octal value (if it is between 0 and 0377)
into the buffer at the current point.

\\[hexl-insert-decimal-char] will insert a given decimal value (if it is between 0 and 255)
into the buffer at the current point.

\\[hexl-mode-exit] will exit hexl-mode.

Note: saving the file with any of the usual Emacs commands
will actually convert it back to binary format while saving.

You can use \\[hexl-find-file] to visit a file in hexl-mode.

\\[describe-bindings] for advanced commands." t nil)

(autoload 'hexl-find-file "hexl" "\
Edit file FILENAME in hexl-mode.
Switch to a buffer visiting file FILENAME, creating one in none exists." t nil)

(autoload 'hexlify-buffer "hexl" "\
Convert a binary buffer to hexl format.
This discards the buffer's undo information." t nil)

;;;***

;;;### (autoloads (hyper-apropos-popup-menu hyper-apropos-set-variable hyper-set-variable hyper-apropos-get-doc hyper-apropos-read-variable-symbol hyper-describe-function hyper-describe-variable hyper-describe-face hyper-describe-key-briefly hyper-describe-key hyper-apropos) "hyper-apropos" "packages/hyper-apropos.el")

(autoload 'hyper-apropos "hyper-apropos" "\
Display lists of functions and variables matching REGEXP
in buffer \"*Hyper Apropos*\".  If optional prefix arg is given, then the
value of `hyper-apropos-programming-apropos' is toggled for this search.
See also `hyper-apropos-mode'." t nil)

(autoload 'hyper-describe-key "hyper-apropos" nil t nil)

(autoload 'hyper-describe-key-briefly "hyper-apropos" nil t nil)

(autoload 'hyper-describe-face "hyper-apropos" "\
Describe face..
See also `hyper-apropos' and `hyper-describe-function'." t nil)

(autoload 'hyper-describe-variable "hyper-apropos" "\
Hypertext drop-in replacement for `describe-variable'.
See also `hyper-apropos' and `hyper-describe-function'." t nil)

(autoload 'hyper-describe-function "hyper-apropos" "\
Hypertext replacement for `describe-function'.  Unlike `describe-function'
in that the symbol under the cursor is the default if it is a function.
See also `hyper-apropos' and `hyper-describe-variable'." t nil)

(autoload 'hyper-apropos-read-variable-symbol "hyper-apropos" "\
Hypertext drop-in replacement for `describe-variable'.
See also `hyper-apropos' and `hyper-describe-function'." nil nil)

(define-obsolete-function-alias 'hypropos-read-variable-symbol 'hyper-apropos-read-variable-symbol)

(autoload 'hyper-apropos-get-doc "hyper-apropos" "\
Toggle display of documentation for the symbol on the current line." t nil)

(define-obsolete-function-alias 'hypropos-get-doc 'hyper-apropos-get-doc)

(autoload 'hyper-set-variable "hyper-apropos" nil t nil)

(autoload 'hyper-apropos-set-variable "hyper-apropos" "\
Interactively set the variable on the current line." t nil)

(define-obsolete-function-alias 'hypropos-set-variable 'hyper-apropos-set-variable)

(autoload 'hyper-apropos-popup-menu "hyper-apropos" nil t nil)

(define-obsolete-function-alias 'hypropos-popup-menu 'hyper-apropos-popup-menu)

;;;***

;;;### (autoloads (icomplete-minibuffer-setup icomplete-mode) "icomplete" "packages/icomplete.el")

(autoload 'icomplete-mode "icomplete" "\
Activate incremental minibuffer completion for this emacs session.
Deactivates with negative universal argument." t nil)

(autoload 'icomplete-minibuffer-setup "icomplete" "\
Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'." nil nil)

;;;***

;;;### (autoloads (dired-do-igrep-find dired-do-igrep igrep-find-define igrep-find igrep-define igrep) "igrep" "packages/igrep.el")

(autoload 'igrep "igrep" "\
*Run `grep` PROGRAM to match EXPRESSION in FILES.
The output is displayed in the *igrep* buffer, which \\[next-error] and
\\[compile-goto-error] parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

EXPRESSION is automatically delimited by `igrep-expression-quote-char'.

FILES is either a file name pattern (expanded by the shell named by
`shell-file-name') or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument (\\[universal-argument]) is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments (\\[universal-argument] \\[universal-argument]) are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments (\\[universal-argument] \\[universal-argument] \\[universal-argument]) are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-find' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES (and whose contents match
EXPRESSION)." t nil)

(autoload 'igrep-define "igrep" "\
Define ANALOGUE-COMMAND as an `igrep' analogue command.
Optional (VARIABLE VALUE) arguments specify temporary bindings for the command." nil 'macro)

(autoload 'igrep-find "igrep" "\
*Run `grep` via `find`; see \\[igrep] and `igrep-find'.
All arguments (including prefix arguments, when called interactively)
are handled by `igrep'." t nil)

(autoload 'igrep-find-define "igrep" "\
Define ANALOGUE-COMMAND-find as an `igrep' analogue `find` command.
Optional (VARIABLE VALUE) arguments specify temporary bindings for the command." nil 'macro)

(autoload 'dired-do-igrep "igrep" "\
*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) files." t nil)

(defalias 'dired-do-grep 'dired-do-igrep)

(autoload 'dired-do-igrep-find "igrep" "\
*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) directories." t nil)

(defalias 'dired-do-grep-find 'dired-do-igrep-find)

;;;***

;;;### (autoloads (Info-elisp-ref Info-emacs-key Info-goto-emacs-key-command-node Info-goto-emacs-command-node Info-emacs-command Info-search Info-visit-file Info-goto-node Info-query info) "info" "packages/info.el")

(autoload 'info "info" "\
Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer." t nil)

(autoload 'Info-query "info" "\
Enter Info, the documentation browser.  Prompt for name of Info file." t nil)

(autoload 'Info-goto-node "info" "\
Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME.
Actually, the following interpretations of NAME are tried in order:
    (FILENAME)NODENAME
    (FILENAME)     (using Top node)
    NODENAME       (in current file)
    TAGNAME        (see below)
    FILENAME       (using Top node)
where TAGNAME is a string that appears in quotes: \"TAGNAME\", in an
annotation for any node of any file.  (See `a' and `x' commands.)" t nil)

(autoload 'Info-visit-file "info" "\
Directly visit an info file." t nil)

(autoload 'Info-search "info" "\
Search for REGEXP, starting from point, and select node it's found in." t nil)

(autoload 'Info-emacs-command "info" "\
Look up an Emacs command in the Emacs manual in the Info system.
This command is designed to be used whether you are already in Info or not." t nil)

(autoload 'Info-goto-emacs-command-node "info" "\
Look up an Emacs command in the Emacs manual in the Info system.
This command is designed to be used whether you are already in Info or not." t nil)

(autoload 'Info-goto-emacs-key-command-node "info" "\
Look up an Emacs key sequence in the Emacs manual in the Info system.
This command is designed to be used whether you are already in Info or not." t nil)

(autoload 'Info-emacs-key "info" "\
Look up an Emacs key sequence in the Emacs manual in the Info system.
This command is designed to be used whether you are already in Info or not." t nil)

(autoload 'Info-elisp-ref "info" "\
Look up an Emacs Lisp function in the Elisp manual in the Info system.
This command is designed to be used whether you are already in Info or not." t nil)

;;;***

;;;### (autoloads (batch-info-validate Info-validate Info-split Info-tagify) "informat" "packages/informat.el")

(autoload 'Info-tagify "informat" "\
Create or update Info-file tag table in current buffer." t nil)

(autoload 'Info-split "informat" "\
Split an info file into an indirect file plus bounded-size subfiles.
Each subfile will be up to 50,000 characters plus one node.

To use this command, first visit a large Info file that has a tag
table.  The buffer is modified into a (small) indirect info file which
should be saved in place of the original visited file.

The subfiles are written in the same directory the original file is
in, with names generated by appending `-' and a number to the original
file name.  The indirect file still functions as an Info file, but it
contains just the tag table and a directory of subfiles." t nil)

(autoload 'Info-validate "informat" "\
Check current buffer for validity as an Info file.
Check that every node pointer points to an existing node." t nil)

(autoload 'batch-info-validate "informat" "\
Runs `Info-validate' on the files remaining on the command line.
Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-info-validate $info/ ~/*.info\"" nil nil)

;;;***

;;;### (autoloads (ispell-message ispell-minor-mode ispell-complete-word-interior-frag ispell-complete-word ispell-continue ispell-buffer ispell-region ispell-change-dictionary ispell-kill-ispell ispell-help ispell-word) "ispell" "packages/ispell.el")

(defcustom ispell-personal-dictionary nil "*File name of your personal spelling dictionary, or nil.\nIf nil, the default personal dictionary, \"~/.ispell_DICTNAME\" is used,\nwhere DICTNAME is the name of your default dictionary." :type 'file :group 'ispell)

(defvar ispell-dictionary-alist-1 '((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil) ("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil) ("british" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "british") nil) ("deutsch" "[a-zA-Z\"]" "[^a-zA-Z\"]" "[']" t ("-C") "~tex") ("deutsch8" "[a-zA-Zƒ÷‹‰ˆﬂ¸]" "[^a-zA-Zƒ÷‹‰ˆﬂ¸]" "[']" t ("-C" "-d" "deutsch") "~latin1") ("nederlands" "[A-Za-z¿-≈«»-œ“-÷Ÿ-‹‡-ÂÁË-ÔÒÚ-ˆ˘-¸]" "[^A-Za-z¿-≈«»-œ“-÷Ÿ-‹‡-ÂÁË-ÔÒÚ-ˆ˘-¸]" "[']" t ("-C") nil) ("nederlands8" "[A-Za-z¿-≈«»-œ“-÷Ÿ-‹‡-ÂÁË-ÔÒÚ-ˆ˘-¸]" "[^A-Za-z¿-≈«»-œ“-÷Ÿ-‹‡-ÂÁË-ÔÒÚ-ˆ˘-¸]" "[']" t ("-C") nil)))

(defvar ispell-dictionary-alist-2 '(("svenska" "[A-Za-z}{|\\133\\135\\\\]" "[^A-Za-z}{|\\133\\135\\\\]" "[']" nil ("-C") nil) ("svenska8" "[A-Za-zÂ‰ˆ≈ƒˆ]" "[^A-Za-zÂ‰ˆ≈ƒˆ]" "[']" nil ("-C" "-d" "svenska") "~list") ("norsk" "[A-Za-zÈÊ¯Â…∆ÿ≈]" "[^A-Za-zÈÊ¯Â…∆ÿ≈]" "[']" nil ("-C" "-d" "norsk") "~list") ("francais7" "[A-Za-z]" "[^A-Za-z]" "[`'^---]" t nil nil) ("francais" "[A-Za-z¿¬∆«»… ÀŒœ‘Ÿ€‹‡‚ÁËÈÍÎÓÔÙ˘˚¸]" "[^A-Za-z¿¬∆«»… ÀŒœ‘Ÿ€‹‡‚ÁËÈÍÎÓÔÙ˘˚¸]" "[---']" t nil "~list") ("francais-tex" "[A-Za-z¿¬∆«»… ÀŒœ‘Ÿ€‹‡‚ÁËÈÍÎÓÔÙ˘˚¸\\]" "[^A-Za-z¿¬∆«»… ÀŒœ‘Ÿ€‹‡‚ÁËÈÍÎÓÔÙ˘˚¸\\]" "[---'^`\"]" t nil "~tex") ("italiano" "[A-Za-z¿»…ÃÕŒ“Ÿ⁄‡ËÈÏÌÓÚ˘˙]" "[^A-Za-z¿»…ÃÕŒ“Ÿ⁄‡ËÈÏÌÓÚ˘˙]" "[']" t ("-d" "italiano") "~list") ("dansk" "[A-Z∆ÿ≈a-zÊ¯Â]" "[^A-Z∆ÿ≈a-zÊ¯Â]" "" nil ("-C") nil)))

(defvar ispell-dictionary-alist (append ispell-dictionary-alist-1 ispell-dictionary-alist-2) "\
An alist of dictionaries and their associated parameters.

Each element of this list is also a list:

\(DICTIONARY-NAME CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P
        ISPELL-ARGS EXTENDED-CHARACTER-MODE)

DICTIONARY-NAME is a possible value of variable `ispell-dictionary', nil
means the default dictionary.

CASECHARS is a regular expression of valid characters that comprise a
word.

NOT-CASECHARS is the opposite regexp of CASECHARS.

OTHERCHARS is a regular expression of other characters that are valid
in word constructs.  Otherchars cannot be adjacent to each other in a
word, nor can they begin or end a word.  This implies we can't check
\"Stevens'\" as a correct possessive and other correct formations.

Hint: regexp syntax requires the hyphen to be declared first here.

MANY-OTHERCHARS-P is non-nil if many otherchars are to be allowed in a
word instead of only one.

ISPELL-ARGS is a list of additional arguments passed to the ispell
subprocess.

EXTENDED-CHARACTER-MODE should be used when dictionaries are used which
have been configured in an Ispell affix file.  (For example, umlauts
can be encoded as \\\"a, a\\\", \"a, ...)  Defaults are ~tex and ~nroff
in English.  This has the same effect as the command-line `-T' option.
The buffer Major Mode controls Ispell's parsing in tex or nroff mode,
but the dictionary can control the extended character mode.
Both defaults can be overruled in a buffer-local fashion. See
`ispell-parsing-keyword' for details on this.

Note that the CASECHARS and OTHERCHARS slots of the alist should
contain the same character set as casechars and otherchars in the
language.aff file (e.g., english.aff).")

(defvar ispell-menu-map nil "\
Key map for ispell menu")

(defvar ispell-menu-xemacs nil "\
Spelling menu for XEmacs.")

(defconst ispell-menu-map-needed (and (not ispell-menu-map) (string-lessp "19" emacs-version) (not (string-match "XEmacs" emacs-version))))

(if ispell-menu-map-needed (let ((dicts (reverse (cons (cons "default" nil) ispell-dictionary-alist))) name) (setq ispell-menu-map (make-sparse-keymap "Spell")) (while dicts (setq name (car (car dicts)) dicts (cdr dicts)) (if (stringp name) (define-key ispell-menu-map (vector (intern name)) (cons (concat "Select " (capitalize name)) (list 'lambda nil '(interactive) (list 'ispell-change-dictionary name))))))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-change-dictionary] '("Change Dictionary" . ispell-change-dictionary)) (define-key ispell-menu-map [ispell-kill-ispell] '("Kill Process" . ispell-kill-ispell)) (define-key ispell-menu-map [ispell-pdict-save] '("Save Dictionary" lambda nil (interactive) (ispell-pdict-save t t))) (define-key ispell-menu-map [ispell-complete-word] '("Complete Word" . ispell-complete-word)) (define-key ispell-menu-map [ispell-complete-word-interior-frag] '("Complete Word Frag" . ispell-complete-word-interior-frag))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-continue] '("Continue Check" . ispell-continue)) (define-key ispell-menu-map [ispell-word] '("Check Word" . ispell-word)) (define-key ispell-menu-map [ispell-region] '("Check Region" . ispell-region)) (define-key ispell-menu-map [ispell-buffer] '("Check Buffer" . ispell-buffer))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-message] '("Check Message" . ispell-message)) (define-key ispell-menu-map [ispell-help] '("Help" lambda nil (interactive) (describe-function 'ispell-help))) (put 'ispell-region 'menu-enable 'mark-active) (fset 'ispell-menu-map (symbol-value 'ispell-menu-map))))

(defvar ispell-local-pdict ispell-personal-dictionary "\
A buffer local variable containing the current personal dictionary.
If non-nil, the value must be a string, which is a file name.

If you specify a personal dictionary for the current buffer which is
different from the current personal dictionary, the effect is similar
to calling \\[ispell-change-dictionary].  This variable is automatically
set when defined in the file with either `ispell-pdict-keyword' or the
local variable syntax.")

(define-key global-map [(meta ?\$)] 'ispell-word)

(autoload 'ispell-word "ispell" "\
Check spelling of word under or before the cursor.
If the word is not found in dictionary, display possible corrections
in a window allowing you to choose one.

With a prefix argument (or if CONTINUE is non-nil),
resume interrupted spell-checking of a buffer or region.

If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding) is checked when the cursor is not over a word.
When the optional argument QUIETLY is non-nil or `ispell-quietly' is non-nil
when called interactively, non-corrective messages are suppressed.

Word syntax described by `ispell-dictionary-alist' (which see).

This will check or reload the dictionary.  Use \\[ispell-change-dictionary]
or \\[ispell-region] to update the Ispell process." t nil)

(autoload 'ispell-help "ispell" "\
Display a list of the options available when a misspelling is encountered.

Selections are:

DIGIT: Replace the word with a digit offered in the *Choices* buffer.
SPC:   Accept word this time.
`i':   Accept word and insert into private dictionary.
`a':   Accept word for this session.
`A':   Accept word and place in `buffer-local dictionary'.
`r':   Replace word with typed-in value.  Rechecked.
`R':   Replace word with typed-in value. Query-replaced in buffer. Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Like `i', but allows one to include dictionary completion information.
`C-l':  redraws screen
`C-r':  recursive edit
`C-z':  suspend emacs or iconify frame" nil nil)

(autoload 'ispell-kill-ispell "ispell" "\
Kill current Ispell process (so that you may start a fresh one).
With NO-ERROR, just return non-nil if there was no Ispell running." t nil)

(autoload 'ispell-change-dictionary "ispell" "\
Change `ispell-dictionary' (q.v.) and kill old Ispell process.
A new one will be started as soon as necessary.

By just answering RET you can find out what the current dictionary is.

With prefix argument, set the default directory." t nil)

(autoload 'ispell-region "ispell" "\
Interactively check a region for spelling errors." t nil)

(autoload 'ispell-buffer "ispell" "\
Check the current buffer for spelling errors interactively." t nil)

(autoload 'ispell-continue "ispell" nil t nil)

(autoload 'ispell-complete-word "ispell" "\
Look up word before or under point in dictionary (see lookup-words command)
and try to complete it.  If optional INTERIOR-FRAG is non-nil then the word
may be a character sequence inside of a word.

Standard ispell choices are then available." t nil)

(autoload 'ispell-complete-word-interior-frag "ispell" "\
Completes word matching character sequence inside a word." t nil)

(autoload 'ispell-minor-mode "ispell" "\
Toggle Ispell minor mode.
With prefix arg, turn Ispell minor mode on iff arg is positive.
 
In Ispell minor mode, pressing SPC or RET
warns you if the previous word is incorrectly spelled." t nil)

(autoload 'ispell-message "ispell" "\
Check the spelling of a mail message or news post.
Don't check spelling of message headers except the Subject field.
Don't check included messages.

To abort spell checking of a message region and send the message anyway,
use the `x' or `q' command.  (Any subsequent regions will be checked.)
The `X' command aborts the message send so that you can edit the buffer.

To spell-check whenever a message is sent, include the appropriate lines
in your .emacs file:
   (add-hook 'message-send-hook 'ispell-message)
   (add-hook 'mail-send-hook  'ispell-message)
   (add-hook 'mh-before-send-letter-hook 'ispell-message)

You can bind this to the key C-c i in GNUS or mail by adding to
`news-reply-mode-hook' or `mail-mode-hook' the following lambda expression:
   (function (lambda () (local-set-key \"\\C-ci\" 'ispell-message)))" t nil)

;;;***

;;;### (autoloads (jka-compr-install toggle-auto-compression jka-compr-load) "jka-compr" "packages/jka-compr.el")

(autoload 'jka-compr-load "jka-compr" "\
Documented as original." nil nil)

(autoload 'toggle-auto-compression "jka-compr" "\
Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on).
If the argument MESSAGE is non-nil, it means to print a message
saying whether the mode is now on or off." t nil)

(autoload 'jka-compr-install "jka-compr" "\
Install jka-compr.
This adds entries to `file-name-handler-alist' and `auto-mode-alist'
and `inhibit-first-line-modes-suffixes'." nil nil)

;;;***

;;;### (autoloads (turn-on-lazy-lock lazy-lock-mode) "lazy-lock" "packages/lazy-lock.el")

(autoload 'lazy-lock-mode "lazy-lock" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive and the buffer
is at least `lazy-lock-minimum-size' characters long.

When Lazy Lock mode is enabled, fontification is demand-driven and stealthy:

 - Fontification occurs in visible parts of buffers when necessary.
   Occurs if there is no input after pausing for `lazy-lock-continuity-time'.

 - Fontification occurs in invisible parts when Emacs has been idle.
   Occurs if there is no input after pausing for `lazy-lock-stealth-time'.

If `lazy-lock-hide-invisible' is non-nil, text is not displayed until it is
fontified, otherwise it is displayed in `lazy-lock-invisible-foreground'.

See also variables `lazy-lock-walk-windows' and `lazy-lock-ignore-commands' for
window (scroll) fontification, and `lazy-lock-stealth-lines',
`lazy-lock-stealth-nice' and `lazy-lock-stealth-verbose' for stealth
fontification.

Use \\[lazy-lock-submit-bug-report] to send bug reports or feedback." t nil)

(autoload 'turn-on-lazy-lock "lazy-lock" "\
Unconditionally turn on Lazy Lock mode." nil nil)

(when (fboundp 'add-minor-mode) (defvar lazy-lock-mode nil) (add-minor-mode 'lazy-lock-mode nil))

;;;***

;;;### (autoloads (ledit-from-lisp-mode ledit-mode) "ledit" "packages/ledit.el")

(defconst ledit-save-files t "\
*Non-nil means Ledit should save files before transferring to Lisp.")

(defconst ledit-go-to-lisp-string "%?lisp" "\
*Shell commands to execute to resume Lisp job.")

(defconst ledit-go-to-liszt-string "%?liszt" "\
*Shell commands to execute to resume Lisp compiler job.")

(autoload 'ledit-mode "ledit" "\
\\<ledit-mode-map>Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  \\[ledit-save-defun]	-- record defun at or after point
	   for later transmission to Lisp job.
  \\[ledit-save-region] -- record region for later transmission to Lisp job.
  \\[ledit-go-to-lisp] -- transfer to Lisp job and transmit saved text.
  \\[ledit-go-to-liszt] -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)" t nil)

(autoload 'ledit-from-lisp-mode "ledit" nil nil nil)

;;;***

;;;### (autoloads (print-region lpr-region print-buffer lpr-buffer) "lpr" "packages/lpr.el")

(defcustom lpr-switches nil "*List of strings to pass as extra options for the printer program.\nSee `lpr-command'." :type '(repeat (string :tag "Argument")) :group 'lpr)

(defcustom lpr-command (if (memq system-type '(usg-unix-v dgux hpux irix)) "lp" "lpr") "*Name of program for printing a file." :type 'string :group 'lpr)

(autoload 'lpr-buffer "lpr" "\
Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload 'print-buffer "lpr" "\
Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload 'lpr-region "lpr" "\
Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload 'print-region "lpr" "\
Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

;;;***

;;;### (autoloads (make-command-summary) "makesum" "packages/makesum.el")

(autoload 'make-command-summary "makesum" "\
Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first." t nil)

;;;***

;;;### (autoloads (manual-entry) "man" "packages/man.el")

(autoload 'manual-entry "man" "\
Display the Unix manual entry (or entries) for TOPIC." t nil)

;;;***

;;;### (autoloads (metamail-region metamail-buffer metamail-interpret-body metamail-interpret-header) "metamail" "packages/metamail.el")

(autoload 'metamail-interpret-header "metamail" "\
Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all." t nil)

(autoload 'metamail-interpret-body "metamail" "\
Interpret a body part of a MIME message in current buffer.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all." t nil)

(autoload 'metamail-buffer "metamail" "\
Process current buffer through `metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

(autoload 'metamail-region "metamail" "\
Process current region through 'metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

;;;***

;;;### (autoloads (blink-paren paren-set-mode) "paren" "packages/paren.el")

(defcustom paren-mode nil "*Sets the style of parenthesis highlighting.\nValid values are nil, `blink-paren', `paren', and `sexp'.\n  nil		no parenthesis highlighting.\n  blink-paren	causes the matching paren to blink.\n  paren		causes the matching paren to be highlighted but not to blink.\n  sexp		whole expression enclosed by the local paren at its mate.\n  nested	(not yet implemented) use variable shading to see the\n		nesting of an expression.  Also groks regular expressions\n		and shell quoting.\n\nThis variable is global by default, but you can make it buffer-local and\nhighlight parentheses differently in different major modes." :type '(radio (const nil) (const blink-paren) (const paren) (const sexp) (const nested)) :group 'paren-matching)

(autoload 'paren-set-mode "paren" "\
Cycles through possible values for `paren-mode', force off with negative arg.
When called from lisp, a symbolic value for `paren-mode' can be passed directly.
See also `paren-mode' and `paren-highlight'." t nil)

(make-obsolete 'blink-paren 'paren-set-mode)

(autoload 'blink-paren "paren" "\
Obsolete.  Use `paren-set-mode' instead." t nil)

;;;***

;;;### (autoloads (pending-delete pending-delete-off pending-delete-on) "pending-del" "packages/pending-del.el")

(autoload 'pending-delete-on "pending-del" "\
Turn on pending delete.
When it is ON, typed text replaces the selection if the selection is active.
When it is OFF, typed text is just inserted at point." t nil)

(autoload 'pending-delete-off "pending-del" "\
Turn off pending delete.
When it is ON, typed text replaces the selection if the selection is active.
When it is OFF, typed text is just inserted at point." t nil)

(autoload 'pending-delete "pending-del" "\
Toggle automatic deletion of the selected region.
With a positive argument, turns it on.
With a non-positive argument, turns it off.
When active, typed text replaces the selection." t nil)

;;;***

;;;### (autoloads (ps-setup ps-nb-pages-region ps-nb-pages-buffer ps-line-lengths ps-despool ps-spool-region-with-faces ps-spool-region ps-spool-buffer-with-faces ps-spool-buffer ps-print-region-with-faces ps-print-region ps-print-buffer-with-faces ps-print-buffer) "ps-print" "packages/ps-print.el")

(defcustom ps-paper-type 'letter "*Specifies the size of paper to format for.\nShould be one of the paper types defined in `ps-page-dimensions-database', for\nexample `letter', `legal' or `a4'." :type '(symbol :validate (lambda (wid) (if (assq (widget-value wid) ps-page-dimensions-database) nil (widget-put wid :error "Unknown paper size") wid))) :group 'ps-print)

(defcustom ps-print-color-p (or (fboundp 'x-color-values) (fboundp 'color-instance-rgb-components)) "*If non-nil, print the buffer's text in color." :type 'boolean :group 'ps-print-color)

(autoload 'ps-print-buffer "ps-print" "\
Generate and print a PostScript image of the buffer.

When called with a numeric prefix argument (C-u), prompts the user for
the name of a file to save the PostScript image in, instead of sending
it to the printer.

More specifically, the FILENAME argument is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name.  If FILENAME is a
number, prompt the user for the name of the file to save in." t nil)

(autoload 'ps-print-buffer-with-faces "ps-print" "\
Generate and print a PostScript image of the buffer.
Like `ps-print-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values." t nil)

(autoload 'ps-print-region "ps-print" "\
Generate and print a PostScript image of the region.
Like `ps-print-buffer', but prints just the current region." t nil)

(autoload 'ps-print-region-with-faces "ps-print" "\
Generate and print a PostScript image of the region.
Like `ps-print-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values." t nil)

(autoload 'ps-spool-buffer "ps-print" "\
Generate and spool a PostScript image of the buffer.
Like `ps-print-buffer' except that the PostScript image is saved in a
local buffer to be sent to the printer later.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload 'ps-spool-buffer-with-faces "ps-print" "\
Generate and spool a PostScript image of the buffer.
Like `ps-spool-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload 'ps-spool-region "ps-print" "\
Generate a PostScript image of the region and spool locally.
Like `ps-spool-buffer', but spools just the current region.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload 'ps-spool-region-with-faces "ps-print" "\
Generate a PostScript image of the region and spool locally.
Like `ps-spool-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload 'ps-despool "ps-print" "\
Send the spooled PostScript to the printer.

When called with a numeric prefix argument (C-u), prompt the user for
the name of a file to save the spooled PostScript in, instead of sending
it to the printer.

More specifically, the FILENAME argument is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name.  If FILENAME is a
number, prompt the user for the name of the file to save in." t nil)

(autoload 'ps-line-lengths "ps-print" "\
*Display the correspondence between a line length and a font size,
using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s
\", length($0), $0}' | sort -r | head" t nil)

(autoload 'ps-nb-pages-buffer "ps-print" "\
*Display an approximate correspondence between a font size and the number
of pages the current buffer would require to print
using the current ps-print setup." t nil)

(autoload 'ps-nb-pages-region "ps-print" "\
*Display an approximate correspondence between a font size and the number
of pages the current region would require to print
using the current ps-print setup." t nil)

(autoload 'ps-setup "ps-print" "\
*Return the current setup" nil nil)

;;;***

;;;### (autoloads (remote-compile) "rcompile" "packages/rcompile.el")

(autoload 'remote-compile "rcompile" "\
Compile the current buffer's directory on HOST.  Log in as USER.
See \\[compile]." t nil)

;;;***

;;;### (autoloads (resume-suspend-hook) "resume" "packages/resume.el")

(autoload 'resume-suspend-hook "resume" "\
Clear out the file used for transmitting args when Emacs resumes." nil nil)

;;;***

;;;### (autoloads (install-shell-fonts) "shell-font" "packages/shell-font.el")

(autoload 'install-shell-fonts "shell-font" "\
Decorate the current interaction buffer with fonts.
This uses the faces called `shell-prompt', `shell-input' and `shell-output';
you can alter the graphical attributes of those with the normal
face-manipulation functions." nil nil)

;;;***

;;;### (autoloads (spell-string spell-region spell-word spell-buffer) "spell" "packages/spell.el")

(put 'spell-filter 'risky-local-variable t)

(autoload 'spell-buffer "spell" "\
Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped." t nil)

(autoload 'spell-word "spell" "\
Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and `query-replace' the entire buffer to substitute it." t nil)

(autoload 'spell-region "spell" "\
Like `spell-buffer' but applies only to region.
Used in a program, applies from START to END.
DESCRIPTION is an optional string naming the unit being checked:
for example, \"word\"." t nil)

(autoload 'spell-string "spell" "\
Check spelling of string supplied as argument." t nil)

;;;***

;;;### (autoloads (tar-mode) "tar-mode" "packages/tar-mode.el")

(autoload 'tar-mode "tar-mode" "\
Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands. 
Letters no longer insert themselves.
Type 'e' to pull a file out of the tar file and into its own buffer.
Type 'c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the 'e' command) and 
save it with Control-X Control-S, the contents of that buffer will be 
saved back into the tar-file buffer; in this way you can edit a file 
inside of a tar archive without extracting it and re-archiving it.

See also: variables tar-update-datestamp and tar-anal-blocksize.
\\{tar-mode-map}" nil nil)

;;;***

;;;### (autoloads (terminal-emulator) "terminal" "packages/terminal.el")

(autoload 'terminal-emulator "terminal" "\
Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program an keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behaviour
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started.

Presently with `termcap' only; if somebody sends us code to make this
work with `terminfo' we will try to use it." t nil)

;;;***

;;;### (autoloads (batch-texinfo-format texinfo-format-region texinfo-format-buffer) "texinfmt" "packages/texinfmt.el")

(autoload 'texinfo-format-buffer "texinfmt" "\
Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually." t nil)

(autoload 'texinfo-format-region "texinfmt" "\
Convert the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer." t nil)

(autoload 'batch-texinfo-format "texinfmt" "\
Runs  texinfo-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texinfo-format $docs/ ~/*.texinfo\"." nil nil)

;;;***

;;;### (autoloads (texinfo-sequential-node-update texinfo-every-node-update texinfo-update-node) "texnfo-upd" "packages/texnfo-upd.el")

(autoload 'texinfo-update-node "texnfo-upd" "\
Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

The functions for creating or updating nodes and menus, and their
keybindings, are:

    texinfo-update-node (&optional region-p)    \\[texinfo-update-node]
    texinfo-every-node-update ()                \\[texinfo-every-node-update]
    texinfo-sequential-node-update (&optional region-p)

    texinfo-make-menu (&optional region-p)      \\[texinfo-make-menu]
    texinfo-all-menus-update ()                 \\[texinfo-all-menus-update]
    texinfo-master-menu ()

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented. Its default value is 32." t nil)

(autoload 'texinfo-every-node-update "texnfo-upd" "\
Update every node in a Texinfo file." t nil)

(autoload 'texinfo-sequential-node-update "texnfo-upd" "\
Update one node (or many) in a Texinfo file with sequential pointers.

This function causes the `Next' or `Previous' pointer to point to the
immediately preceding or following node, even if it is at a higher or
lower hierarchical level in the document.  Continually pressing `n' or
`p' takes you straight through the file.

Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

This command makes it awkward to navigate among sections and
subsections; it should be used only for those documents that are meant
to be read like a novel rather than a reference, and for which the
Info `g*' command is inadequate." t nil)

;;;***

;;;### (autoloads (time-stamp-toggle-active time-stamp) "time-stamp" "packages/time-stamp.el")

(autoload 'time-stamp "time-stamp" "\
Update the time stamp string in the buffer.
If you put a time stamp template anywhere in the first 8 lines of a file,
it can be updated every time you save the file.  See the top of
`time-stamp.el' for a sample.  The template looks like one of the following:
    Time-stamp: <>
    Time-stamp: \" \"
The time stamp is written between the brackets or quotes, resulting in
    Time-stamp: <95/01/18 10:20:51 gildea>
Only does its thing if the variable  time-stamp-active  is non-nil.
Typically used on  write-file-hooks  for automatic time-stamping.
The format of the time stamp is determined by the variable  time-stamp-format.
The variables time-stamp-line-limit, time-stamp-start, and time-stamp-end
control finding the template." t nil)

(autoload 'time-stamp-toggle-active "time-stamp" "\
Toggle time-stamp-active, setting whether \\[time-stamp] updates a buffer.
With arg, turn time stamping on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (display-time) "time" "packages/time.el")

(defcustom display-time-day-and-date nil "*Non-nil means \\[display-time] should display day,date and time.\nThis affects the spec 'date in the variable display-time-form-list." :group 'display-time :type 'boolean)

(autoload 'display-time "time" "\
Display current time, load level, and mail flag in mode line of each buffer.
Updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
After each update, `display-time-hook' is run with `run-hooks'.
If `display-time-echo-area' is non-nil, the time is displayed in the
echo area instead of in the mode-line." t nil)

;;;***

;;;### (autoloads (ununderline-and-unoverstrike-region overstrike-region unoverstrike-region ununderline-region underline-region) "underline" "packages/underline.el")

(autoload 'underline-region "underline" "\
Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload 'ununderline-region "underline" "\
Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload 'unoverstrike-region "underline" "\
Remove all overstriking (character-backspace-character) in the region.
Called from program, takes two arguments START and END which specify the
range to operate on." t nil)

(autoload 'overstrike-region "underline" "\
Overstrike (character-backspace-character) all nonblank characters in
the region. Called from program, takes two arguments START and END which
specify the range to operate on." t nil)

(autoload 'ununderline-and-unoverstrike-region "underline" "\
Remove underlining and overstriking in the region.  Called from a program,
takes two arguments START and END which specify the range to operate on." t nil)

;;;***

;;;### (autoloads (ask-to-update-copyright update-copyright) "upd-copyr" "packages/upd-copyr.el")

(defcustom copyright-do-not-disturb "Free Software Foundation, Inc." "*If non-nil, the existing copyright holder is checked against this regexp.\nIf it does not match, then a new copyright line is added with the copyright\nholder set to the value of `copyright-whoami'." :type '(choice (const nil) string) :group 'copyright)

(defcustom copyright-whoami nil "*A string containing the name of the owner of new copyright notices." :type '(choice (const nil) string) :group 'copyright)

(defcustom copyright-notice-file nil "*If non-nil, replace copying notices with this file." :type '(choice (const nil) file) :group 'copyright)

(autoload 'update-copyright "upd-copyr" "\
Update the copyright notice at the beginning of the buffer
to indicate the current year.  If optional arg REPLACE is given
\(interactively, with prefix arg) replace the years in the notice
rather than adding the current year after them.
If `copyright-notice-file' is set, the copying permissions following the
copyright are replaced as well.

If optional third argument ASK is non-nil, the user is prompted for whether
or not to update the copyright.  If optional fourth argument ASK-YEAR is
non-nil, the user is prompted for whether or not to replace the year rather
than adding to it." t nil)

(autoload 'ask-to-update-copyright "upd-copyr" "\
If the current buffer contains a copyright notice that is out of date,
ask the user if it should be updated with `update-copyright' (which see).
Put this on write-file-hooks." nil nil)

;;;***

;;;### (autoloads (vc-update-change-log vc-rename-file vc-cancel-version vc-revert-buffer vc-print-log vc-retrieve-snapshot vc-create-snapshot vc-directory vc-insert-headers vc-version-other-window vc-diff vc-checkout vc-register vc-next-action vc-find-binary) "vc" "packages/vc.el")

(defvar vc-before-checkin-hook nil "\
*Normal hook (list of functions) run before a file gets checked in.  
See `run-hooks'.")

(defvar vc-checkin-hook nil "\
*Normal hook (List of functions) run after a checkin is done.
See `run-hooks'.")

(autoload 'vc-find-binary "vc" "\
Look for a command anywhere on the subprocess-command search path." nil nil)

(autoload 'vc-next-action "vc" "\
Do the next logical checkin or checkout operation on the current file.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.
   A prefix argument lets you specify the version number to use.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control and then retrieves a writable, locked copy for editing.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy." t nil)

(autoload 'vc-register "vc" "\
Register the current file into your version-control system." t nil)

(autoload 'vc-checkout "vc" "\
Retrieve a copy of the latest version of the given file." nil nil)

(autoload 'vc-diff "vc" "\
Display diffs between file versions.
Normally this compares the current file and buffer with the most recent 
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare." t nil)

(autoload 'vc-version-other-window "vc" "\
Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created." t nil)

(autoload 'vc-insert-headers "vc" "\
Insert headers in a file for use with your version-control system.
Headers desired are inserted at the start of the buffer, and are pulled from
the variable `vc-header-alist'." t nil)

(autoload 'vc-directory "vc" "\
Show version-control status of the current directory and subdirectories.
Normally it creates a Dired buffer that lists only the locked files
in all these directories.  With a prefix argument, it lists all files." t nil)

(autoload 'vc-create-snapshot "vc" "\
Make a snapshot called NAME.
The snapshot is made from all registered files at or below the current
directory.  For each file, the version level of its latest
version becomes part of the named configuration." t nil)

(autoload 'vc-retrieve-snapshot "vc" "\
Retrieve the snapshot called NAME.
This function fails if any files are locked at or below the current directory
Otherwise, all registered files are checked out (unlocked) at their version
levels in the snapshot." t nil)

(autoload 'vc-print-log "vc" "\
List the change log of the current buffer in a window." t nil)

(autoload 'vc-revert-buffer "vc" "\
Revert the current buffer's file back to the latest checked-in version.
This asks for confirmation if the buffer contents are not identical
to that version.
If the back-end is CVS, this will give you the most recent revision of
the file on the branch you are editing." t nil)

(autoload 'vc-cancel-version "vc" "\
Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards." t nil)

(autoload 'vc-rename-file "vc" "\
Rename file OLD to NEW, and rename its master file likewise." t nil)

(autoload 'vc-update-change-log "vc" "\
Find change log file and add entries from recent RCS/CVS logs.
Normally, find log entries for all registered files in the default
directory using `rcs2log', which finds CVS logs preferentially.
The mark is left at the end of the text prepended to the change log.

With prefix arg of C-u, only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any arguments are assumed to be filenames and are
passed to the `rcs2log' script after massaging to be relative to the
default directory." t nil)

;;;***

;;;### (autoloads (webjump) "webjump" "packages/webjump.el")

(autoload 'webjump "webjump" "\
Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Please submit bug reports and other feedback to the author, Neil W. Van Dyke
<nwv@acm.org>.

The latest version can be gotten from `http://www.cs.brown.edu/people/nwv/'.
That Web site also contains `webjump-plus.el', a larger and more frequently
updated sample WebJump hotlist." t nil)

;;;***

;;;### (autoloads (webster-www) "webster-www" "packages/webster-www.el")

(autoload 'webster-www "webster-www" "\
Look up a word in the Webster's dictionary at http://www.m-w.com using WWW." t nil)

;;;***

;;;### (autoloads (run-scheme) "xscheme" "packages/xscheme.el")

(defvar scheme-program-name "scheme" "\
*Program invoked by the `run-scheme' command.")

(defvar scheme-band-name nil "\
*Band loaded by the `run-scheme' command.")

(defvar scheme-program-arguments nil "\
*Arguments passed to the Scheme program by the `run-scheme' command.")

(autoload 'run-scheme "xscheme" "\
Run an inferior Scheme process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line." t nil)

;;;***

(provide 'packages-autoloads)
))
