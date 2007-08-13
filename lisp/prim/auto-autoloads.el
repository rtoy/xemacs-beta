;;; DO NOT MODIFY THIS FILE
(if (featurep 'prim-autoloads) (error "Already loaded"))

;;;### (autoloads (about-xemacs) "about" "prim/about.el")

(autoload 'about-xemacs "about" "\
Describe the True Editor and its minions." t nil)

;;;***

;;;### (autoloads (all-hail-emacs all-hail-xemacs praise-be-unto-emacs praise-be-unto-xemacs) "advocacy" "prim/advocacy.el")

(defvar xemacs-praise-sound-file "sounds/im_so_happy.au" "\
The name of an audio file containing something to play
when praising XEmacs")

(defvar xemacs-praise-message "All Hail XEmacs!\n" "\
What to praise XEmacs with")

(autoload 'praise-be-unto-xemacs "advocacy" "\
All Hail XEmacs!" t nil)

(autoload 'praise-be-unto-emacs "advocacy" nil t nil)

(autoload 'all-hail-xemacs "advocacy" "\
All Hail XEmacs!" t nil)

(autoload 'all-hail-emacs "advocacy" nil t nil)

;;;***

;;;### (autoloads (describe-buffer-case-table) "case-table" "prim/case-table.el")

(autoload 'describe-buffer-case-table "case-table" "\
Describe the case table of the current buffer." t nil)

;;;***

;;;### (autoloads (batch-remove-old-elc) "cleantree" "prim/cleantree.el")

(autoload 'batch-remove-old-elc "cleantree" nil nil nil)

;;;***

;;;### (autoloads (cancel-debug-on-entry debug-on-entry debug) "debug" "prim/debug.el")

(autoload 'debug "debug" "\
Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer." t nil)

(autoload 'debug-on-entry "debug" "\
Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it." t nil)

(autoload 'cancel-debug-on-entry "debug" "\
Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions." t nil)

;;;***

;;;### (autoloads (standard-display-european standard-display-underline standard-display-graphic standard-display-g1 standard-display-ascii standard-display-default standard-display-8bit make-display-table describe-current-display-table) "disp-table" "prim/disp-table.el")

(autoload 'describe-current-display-table "disp-table" "\
Describe the display table in use in the selected window and buffer." t nil)

(autoload 'make-display-table "disp-table" "\
Return a new, empty display table." nil nil)

(autoload 'standard-display-8bit "disp-table" "\
Display characters in the range L to H literally." nil nil)

(autoload 'standard-display-default "disp-table" "\
Display characters in the range L to H using the default notation." nil nil)

(autoload 'standard-display-ascii "disp-table" "\
Display character C using printable string S." nil nil)

(autoload 'standard-display-g1 "disp-table" "\
Display character C as character SC in the g1 character set.
This function assumes that your terminal uses the SO/SI characters;
it is meaningless for an X frame." nil nil)

(autoload 'standard-display-graphic "disp-table" "\
Display character C as character GC in graphics character set.
This function assumes VT100-compatible escapes; it is meaningless for an
X frame." nil nil)

(autoload 'standard-display-underline "disp-table" "\
Display character C as character UC plus underlining." nil nil)

(autoload 'standard-display-european "disp-table" "\
Toggle display of European characters encoded with ISO 8859.
When enabled, characters in the range of 160 to 255 display not
as octal escapes, but as accented characters.
With prefix argument, enable European character display iff arg is positive." t nil)

;;;***

;;;### (autoloads (setenv) "env" "prim/env.el")

(autoload 'setenv "env" "\
Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.

This function works by modifying `process-environment'." t nil)

;;;***

;;;### (autoloads (find-function-at-point find-function-on-key find-function-other-frame find-function-other-window find-function) "find-func" "prim/find-func.el")

(autoload 'find-function "find-func" "\
Find the definition of the function near point in the current window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `find-function-function') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'." t nil)

(autoload 'find-function-other-window "find-func" "\
Find the definition of the function near point in the other window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `find-function-function') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'." t nil)

(autoload 'find-function-other-frame "find-func" "\
Find the definition of the function near point in the another frame.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `find-function-function') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non `nil', otherwise in `load-path'." t nil)

(autoload 'find-function-on-key "find-func" "\
Find the function that KEY invokes.  KEY is a string.
Point is saved if FUNCTION is in the current buffer." t nil)

(autoload 'find-function-at-point "find-func" "\
Find directly the function at point in the other window." t nil)

(define-key ctl-x-4-map "F" 'find-function-other-window)

(define-key ctl-x-5-map "F" 'find-function-other-frame)

(define-key ctl-x-map "K" 'find-function-on-key)

;;;***

;;;### (autoloads nil "itimer-autosave" "prim/itimer-autosave.el")

;;;***

;;;### (autoloads nil "loaddefs" "prim/loaddefs.el")

;;;***

;;;### (autoloads (apply-macro-to-region-lines kbd-macro-query insert-kbd-macro name-last-kbd-macro) "macros" "prim/macros.el")

(autoload 'name-last-kbd-macro "macros" "\
Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid
editor command." t nil)

(autoload 'insert-kbd-macro "macros" "\
Insert in buffer the definition of kbd macro NAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when calling interactively).

This Lisp code will, when executed, define the kbd macro with the same
definition it has now.  If you say to record the keys, the Lisp code
will also rebind those keys to the macro.  Only global key bindings
are recorded since executing this Lisp code always makes global
bindings.

To save a kbd macro, visit a file of Lisp code such as your `~/.emacs',
use this command, and then save the file." t nil)

(autoload 'kbd-macro-query "macros" "\
Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, asks whether to continue running the macro.
Your options are: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the frame, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that." t nil)

(autoload 'apply-macro-to-region-lines "macros" "\
For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function }, 
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names.
" t nil)

;;;***

;;;### (autoloads (disable-command enable-command disabled-command-hook) "novice" "prim/novice.el")

(autoload 'disabled-command-hook "novice" nil nil nil)

(autoload 'enable-command "novice" "\
Allow COMMAND to be executed without special confirmation from now on.
The user's `custom-file' is altered so that this will apply
to future sessions." t nil)

(autoload 'disable-command "novice" "\
Require special confirmation to execute COMMAND from now on.
The user's `custom-file' is altered so that this will apply
to future sessions." t nil)

;;;***

;;;### (autoloads (edit-options list-options) "options" "prim/options.el")

(autoload 'list-options "options" "\
Display a list of XEmacs user options, with values and documentation." t nil)

(autoload 'edit-options "options" "\
Edit a list of XEmacs user option values.
Selects a buffer containing such a list,
in which there are commands to set the option values.
Type \\[describe-mode] in that buffer for a list of commands." t nil)

;;;***

;;;### (autoloads (package-admin-add-binary-package package-admin-add-single-file-package) "package-admin" "prim/package-admin.el")

(autoload 'package-admin-add-single-file-package "package-admin" "\
Install a single file Lisp package into XEmacs package hierarchy.
`file' should be the full path to the lisp file to install.
`destdir' should be a simple directory name.
The optional `pkg-dir' can be used to override the default package hiearchy
\(last package-path)." t nil)

(autoload 'package-admin-add-binary-package "package-admin" "\
Install a pre-bytecompiled XEmacs package into package hierarchy." t nil)

;;;***

;;;### (autoloads (profile-key-sequence profile profiling-results) "profile" "prim/profile.el")

(autoload 'profiling-results "profile" "\
Print profiling info INFO to STREAM in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
 `get-profiling-info'.
If STREAM is omitted, either a *Profiling Results* buffer or standard
 output are used, depending on whether the function was called
 interactively or not." t nil)

(autoload 'profile "profile" "\
Turn on profiling, execute FORMS and restore profiling state.
Profiling state here means that if profiling was not in effect when
PROFILE was called, it will be turned off after FORMS are evaluated.
Otherwise, profiling will be left running.

Returns the profiling info, printable by `profiling-results'." nil 'macro)

(autoload 'profile-key-sequence "profile" "\
Dispatch the key sequence KEYS and profile the execution.
KEYS can be a vector of keypress events, a keypress event, or a character.
The function returns the profiling info." t nil)

;;;***

;;;### (autoloads (clear-rectangle string-rectangle open-rectangle insert-rectangle yank-rectangle kill-rectangle extract-rectangle delete-extract-rectangle delete-rectangle) "rect" "prim/rect.el")

(autoload 'delete-rectangle "rect" "\
Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line starting with the line
where the region begins and ending with the line where the region ends." t nil)

(autoload 'delete-extract-rectangle "rect" "\
Delete contents of rectangle and return it as a list of strings.
Arguments START and END are the corners of the rectangle.
The value is list of strings, one for each line of the rectangle." nil nil)

(autoload 'extract-rectangle "rect" "\
Return contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle." nil nil)

(defvar killed-rectangle nil "\
Rectangle for yank-rectangle to insert.")

(autoload 'kill-rectangle "rect" "\
Delete rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use `delete-extract-rectangle'." t nil)

(autoload 'yank-rectangle "rect" "\
Yank the last killed rectangle with upper left corner at point." t nil)

(autoload 'insert-rectangle "rect" "\
Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.
After this command, the mark is at the upper left corner
and point is at the lower right corner." nil nil)

(autoload 'open-rectangle "rect" "\
Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle." t nil)

(autoload 'string-rectangle "rect" "\
Insert STRING on each line of the region-rectangle, shifting text right.
The left edge of the rectangle specifies the column for insertion.
This command does not delete or overwrite any existing text.

Called from a program, takes three args; START, END and STRING." t nil)

(autoload 'clear-rectangle "rect" "\
Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.
When called from a program, requires two args which specify the corners." t nil)

;;;***

;;;### (autoloads (reposition-window) "reposition" "prim/reposition.el")

(autoload 'reposition-window "reposition" "\
Make the current definition and/or comment visible.
Further invocations move it to the top of the window or toggle the
visibility of comments that precede it.
  Point is left unchanged unless prefix ARG is supplied.
  If the definition is fully onscreen, it is moved to the top of the
window.  If it is partly offscreen, the window is scrolled to get the
definition (or as much as will fit) onscreen, unless point is in a comment
which is also partly offscreen, in which case the scrolling attempts to get
as much of the comment onscreen as possible.
  Initially `reposition-window' attempts to make both the definition and
preceding comments visible.  Further invocations toggle the visibility of
the comment lines.
  If ARG is non-nil, point may move in order to make the whole defun
visible (if only part could otherwise be made so), to make the defun line
visible (if point is in code and it could not be made so, or if only
comments, including the first comment line, are visible), or to make the
first comment line visible (if point is in a comment)." t nil)

;;;***

;;;### (autoloads (reverse-region sort-columns sort-regexp-fields sort-fields sort-float-fields sort-numeric-fields sort-pages sort-paragraphs sort-lines sort-subr) "sort" "prim/sort.el")

(autoload 'sort-subr "sort" "\
General text sorting routine to divide buffer into records and sort them.
Arguments are REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN ENDKEYFUN.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN." nil nil)

(autoload 'sort-lines "sort" "\
Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-paragraphs "sort" "\
Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-pages "sort" "\
Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-numeric-fields "sort" "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.
If you want to sort floating-point numbers, try `sort-float-fields'." t nil)

(autoload 'sort-float-fields "sort" "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.  Specified field
must contain a floating point number in each line of the region.  With a
negative arg, sorts by the ARGth field counted from the right.  Called from a
program, there are three arguments: FIELD, BEG and END.  BEG and END specify
region to sort." t nil)

(autoload 'sort-fields "sort" "\
Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort." t nil)

(autoload 'sort-regexp-fields "sort" "\
Sort the region lexicographically as specified by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\"" t nil)

(autoload 'sort-columns "sort" "\
Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into reverse order.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

Note that `sort-columns' rejects text that contains tabs,
because tabs could be split across the specified columns
and it doesn't know how to handle that.  Also, when possible,
it uses the `sort' utility program, which doesn't understand tabs.
Use \\[untabify] to convert tabs to spaces before sorting." t nil)

(autoload 'reverse-region "sort" "\
Reverse the order of lines in a region.
From a program takes two point or marker arguments, BEG and END." t nil)

;;;***

;;;### (autoloads (load-default-sounds load-sound-file) "sound" "prim/sound.el")

(or sound-alist (setq sound-alist '((ready nil) (warp nil))))

(autoload 'load-sound-file "sound" "\
Read in an audio-file and add it to the sound-alist.

You can only play sound files if you are running on display 0 of the
console of a machine with native sound support or running a NetAudio
server and XEmacs has the necessary sound support compiled in.

The sound file must be in the Sun/NeXT U-LAW format, except on Linux,
where .wav files are also supported by the sound card drivers." t nil)

(autoload 'load-default-sounds "sound" "\
Load and install some sound files as beep-types, using
`load-sound-file'.  This only works if you're on display 0 of the
console of a machine with native sound support or running a NetAudio
server and XEmacs has the necessary sound support compiled in." t nil)

;;;***

;;;### (autoloads (tabify untabify) "tabify" "prim/tabify.el")

(autoload 'untabify "tabify" "\
Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

(autoload 'tabify "tabify" "\
Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

;;;***

;;;### (autoloads (ask-user-about-supersession-threat ask-user-about-lock) "userlock" "prim/userlock.el")

(autoload 'ask-user-about-lock "userlock" "\
Ask user what to do when he wants to edit FILE but it is locked by USER.
This function has a choice of three things to do:
  do (signal 'file-locked (list FILE USER))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can rewrite it to use any criterion you like to choose which one to do." nil nil)

(autoload 'ask-user-about-supersession-threat "userlock" "\
Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called." nil nil)

;;;***

(provide 'prim-autoloads)
