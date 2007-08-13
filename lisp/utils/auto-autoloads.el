;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'utils-autoloads))
    (progn

;;;### (autoloads (defadvice ad-add-advice) "advice" "utils/advice.el")

(defvar ad-redefinition-action 'warn "\
*Defines what to do with redefinitions during Advice de/activation.
Redefinition occurs if a previously activated function that already has an
original definition associated with it gets redefined and then de/activated.
In such a case we can either accept the current definition as the new
original definition, discard the current definition and replace it with the
old original, or keep it and raise an error.  The values `accept', `discard',
`error' or `warn' govern what will be done.  `warn' is just like `accept' but
it additionally prints a warning message.  All other values will be
interpreted as `error'.")

(defvar ad-default-compilation-action 'maybe "\
*Defines whether to compile advised definitions during activation.
A value of `always' will result in unconditional compilation, `never' will
always avoid compilation, `maybe' will compile if the byte-compiler is already
loaded, and `like-original' will compile if the original definition of the
advised function is compiled or a built-in function. Every other value will
be interpreted as `maybe'. This variable will only be considered if the 
COMPILE argument of `ad-activate' was supplied as nil.")

(autoload 'ad-add-advice "advice" "\
Adds a piece of ADVICE to FUNCTION's list of advices in CLASS.
If FUNCTION already has one or more pieces of advice of the specified
CLASS then POSITION determines where the new piece will go.  The value
of POSITION can either be `first', `last' or a number where 0 corresponds
to `first'.  Numbers outside the range will be mapped to the closest
extreme position.  If there was already a piece of ADVICE with the same
name, then the position argument will be ignored and the old advice
will be overwritten with the new one.
    If the FUNCTION was not advised already, then its advice info will be 
initialized.  Redefining a piece of advice whose name is part of the cache-id
will clear the cache." nil nil)

(autoload 'defadvice "advice" "\
Defines a piece of advice for FUNCTION (a symbol).
The syntax of `defadvice' is as follows:

  (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG...)
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY... )

FUNCTION ::= Name of the function to be advised.
CLASS ::= `before' | `around' | `after' | `activation' | `deactivation'.
NAME ::= Non-nil symbol that names this piece of advice.
POSITION ::= `first' | `last' | NUMBER. Optional, defaults to `first',
    see also `ad-add-advice'.
ARGLIST ::= An optional argument list to be used for the advised function
    instead of the argument list of the original.  The first one found in
    before/around/after-advices will be used.
FLAG ::= `protect'|`disable'|`activate'|`compile'|`preactivate'|`freeze'.
    All flags can be specified with unambiguous initial substrings.
DOCSTRING ::= Optional documentation for this piece of advice.
INTERACTIVE-FORM ::= Optional interactive form to be used for the advised
    function.  The first one found in before/around/after-advices will be used.
BODY ::= Any s-expression.

Semantics of the various flags:
`protect': The piece of advice will be protected against non-local exits in
any code that precedes it.  If any around-advice of a function is protected
then automatically all around-advices will be protected (the complete onion).

`activate': All advice of FUNCTION will be activated immediately if
FUNCTION has been properly defined prior to this application of `defadvice'.

`compile': In conjunction with `activate' specifies that the resulting
advised function should be compiled.

`disable': The defined advice will be disabled, hence, it will not be used 
during activation until somebody enables it.

`preactivate': Preactivates the advised FUNCTION at macro-expansion/compile
time.  This generates a compiled advised definition according to the current
advice state that will be used during activation if appropriate.  Only use
this if the `defadvice' gets actually compiled.

`freeze': Expands the `defadvice' into a redefining `defun/defmacro' according
to this particular single advice.  No other advice information will be saved.
Frozen advices cannot be undone, they behave like a hard redefinition of
the advised function.  `freeze' implies `activate' and `preactivate'.  The
documentation of the advised function can be dumped onto the `DOC' file
during preloading.

Look at the file `advice.el' for comprehensive documentation." nil 'macro)

;;;***

;;;### (autoloads (all-annotations annotation-list annotations-at annotations-in-region annotation-at annotationp delete-annotation make-annotation) "annotations" "utils/annotations.el")

(defvar make-annotation-hook nil "\
*Function or functions to run immediately after creating an annotation.")

(defvar before-delete-annotation-hook nil "\
*Function or functions to run immediately before deleting an annotation.")

(defvar after-delete-annotation-hook nil "\
*Function or functions to run immediately after deleting an annotation.")

(autoload 'make-annotation "annotations" "\
Create a marginal annotation, displayed using GLYPH, at position POS.
GLYPH may be either a glyph object or a string.  Use layout policy
LAYOUT and place the annotation in buffer BUFFER.  If POS is nil, point is
used.  If LAYOUT is nil, `whitespace' is used.  If BUFFER is nil, the
current buffer is used.  If WITH-EVENT is non-nil, then when an annotation
is activated, the triggering event is passed as the second arg to the
annotation function.  If D-GLYPH is non-nil then it is used as the glyph 
that will be displayed when button1 is down.  If RIGHTP is non-nil then
the glyph will be displayed on the right side of the buffer instead of the
left." nil nil)

(autoload 'delete-annotation "annotations" "\
Remove ANNOTATION from its buffer.  This does not modify the buffer text." nil nil)

(autoload 'annotationp "annotations" "\
T if OBJECT is an annotation." nil nil)

(autoload 'annotation-at "annotations" "\
Return the first annotation at POS in BUFFER.
BUFFER defaults to the current buffer.  POS defaults to point in BUFFER." nil nil)

(autoload 'annotations-in-region "annotations" "\
Return all annotations in BUFFER between START and END inclusively." nil nil)

(autoload 'annotations-at "annotations" "\
Return a list of all annotations at POS in BUFFER.
If BUFFER is nil, the current buffer is used.  If POS is nil, point is used." nil nil)

(autoload 'annotation-list "annotations" "\
Return a list of all annotations in BUFFER.
If BUFFER is nil, the current buffer is used." nil nil)

(autoload 'all-annotations "annotations" "\
Return a list of all annotations in existence." nil nil)

;;;***

;;;### (autoloads (batch-update-directory batch-update-autoloads update-autoloads-from-directory update-autoloads-here update-file-autoloads generate-file-autoloads) "autoload" "utils/autoload.el")

(autoload 'generate-file-autoloads "autoload" "\
Insert at point a loaddefs autoload section for FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used." t nil)

(autoload 'update-file-autoloads "autoload" "\
Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
This functions refuses to update autolaods files and custom loads." t nil)

(autoload 'update-autoloads-here "autoload" "\
Update sections of the current buffer generated by `update-file-autoloads'." t nil)

(autoload 'update-autoloads-from-directory "autoload" "\
Update `generated-autoload-file' with all the current autoloads from DIR.
This runs `update-file-autoloads' on each .el file in DIR.
Obsolete autoload entries for files that no longer exist are deleted." t nil)

(autoload 'batch-update-autoloads "autoload" "\
Update the autoloads for the files or directories on the command line.
Runs `update-file-autoloads' on files and `update-directory-autoloads'
on directories.  Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke `xemacs -batch -f batch-update-autoloads *.el'.
The directory to which the auto-autoloads.el and custom-load.el files must
be the first parameter on the command line." nil nil)

(autoload 'batch-update-directory "autoload" "\
Update the autoloads for the directory on the command line.
Runs `update-file-autoloads' on each file in the given directory, and must
be used only with -batch, and kills XEmacs on completion." nil nil)

;;;***

;;;### (autoloads (browse-url-lynx-emacs browse-url-lynx-xterm browse-url-w3 browse-url-iximosaic browse-url-grail browse-url-mosaic browse-url-netscape) "browse-url" "utils/browse-url.el")

(defcustom browse-url-browser-function 'browse-url-w3 "*Function to display the current buffer in a WWW browser.\nUsed by the `browse-url-at-point', `browse-url-at-mouse', and\n`browse-url-of-file' commands." :type '(radio (function-item browse-url-w3) (function-item browse-url-netscape) (function-item browse-url-mosaic) (function-item browse-url-cci) (function-item browse-url-iximosaic) (function-item browse-url-lynx-xterm) (function-item browse-url-lynx-emacs) (function-item browse-url-grail) (function :tag "Other" nil)) :group 'browse-url)

(autoload 'browse-url-netscape "browse-url" "\
Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of browse-url-new-window-p.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of browse-url-new-window-p." t nil)

(autoload 'browse-url-mosaic "browse-url" "\
Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-grail "browse-url" "\
Ask the Grail WWW browser to load URL.
Default to the URL around or before point.  Runs the program in the
variable `browse-url-grail'." t nil)

(autoload 'browse-url-iximosaic "browse-url" "\
Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-w3 "browse-url" "\
Ask the w3 WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-lynx-xterm "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  A new Lynx process is run
in an Xterm window." t nil)

(autoload 'browse-url-lynx-emacs "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  Run a new Lynx process in
an Emacs buffer." t nil)

;;;***

;;;### (autoloads (config-value config-value-hash-table) "config" "utils/config.el")

(autoload 'config-value-hash-table "config" "\
Returns hashtable of configuration parameters and their values." nil nil)

(autoload 'config-value "config" "\
Return the value of the configuration parameter CONFIG_SYMBOL." nil nil)

;;;***

;;;### (autoloads (docref-setup) "docref" "utils/docref.el")

(autoload 'docref-setup "docref" "\
Process docref cross-references in the current buffer.
See also \\(f@docref-subst)." t nil)

;;;***

;;;### (autoloads nil "easymenu" "utils/easymenu.el")

;;;***

;;;### (autoloads (edit-toolbar) "edit-toolbar" "utils/edit-toolbar.el")

(autoload 'edit-toolbar "edit-toolbar" "\
Alter toolbar characteristics by editing a buffer representing the current toolbar.
Pops up a buffer containing a list of the current toobar." t nil)

;;;***

;;;### (autoloads (format-kbd-macro kbd read-kbd-macro edit-named-kbd-macro edit-last-kbd-macro edit-kbd-macro) "edmacro" "utils/edmacro.el")

(define-key ctl-x-map "\C-k" 'edit-kbd-macro)

(autoload 'edit-kbd-macro "edmacro" "\
Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 100 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way." t nil)

(autoload 'edit-last-kbd-macro "edmacro" "\
Edit the most recently defined keyboard macro." t nil)

(autoload 'edit-named-kbd-macro "edmacro" "\
Edit a keyboard macro which has been given a name by `name-last-kbd-macro'." t nil)

(autoload 'read-kbd-macro "edmacro" "\
Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always." t nil)

(autoload 'kbd "edmacro" "\
Convert KEYS to the internal Emacs key representation." nil 'macro)

(autoload 'format-kbd-macro "edmacro" "\
Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is `1', put everything on one line.  If VERBOSE is omitted
or nil, use a compact 80-column format." nil nil)

;;;***

;;;### (autoloads (turn-on-eldoc-mode eldoc-mode) "eldoc" "utils/eldoc.el")

(defcustom eldoc-mode nil "*If non-nil, show the defined parameters for the elisp function near point.\n\nFor the emacs lisp function at the beginning of the sexp which point is\nwithin, show the defined parameters for the function in the echo area.\nThis information is extracted directly from the function or macro if it is\nin pure lisp.  If the emacs function is a subr, the parameters are obtained\nfrom the documentation string if possible.\n\nIf point is over a documented variable, print that variable's docstring\ninstead.\n\nThis variable is buffer-local." :type 'boolean :group 'eldoc)

(autoload 'eldoc-mode "eldoc" "\
*Enable or disable eldoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively." t nil)

(autoload 'turn-on-eldoc-mode "eldoc" "\
Unequivocally turn on eldoc-mode (see variable documentation)." t nil)

;;;***

;;;### (autoloads (elp-submit-bug-report elp-results elp-instrument-package elp-instrument-list elp-restore-function elp-instrument-function) "elp" "utils/elp.el")

(autoload 'elp-instrument-function "elp" "\
Instrument FUNSYM for profiling.
FUNSYM must be a symbol of a defined function." t nil)

(autoload 'elp-restore-function "elp" "\
Restore an instrumented function to its original definition.
Argument FUNSYM is the symbol of a defined function." t nil)

(autoload 'elp-instrument-list "elp" "\
Instrument for profiling, all functions in `elp-function-list'.
Use optional LIST if provided instead." t nil)

(autoload 'elp-instrument-package "elp" "\
Instrument for profiling, all functions which start with PREFIX.
For example, to instrument all ELP functions, do the following:

    \\[elp-instrument-package] RET elp- RET" t nil)

(autoload 'elp-results "elp" "\
Display current profiling results.
If `elp-reset-after-results' is non-nil, then current profiling
information for all instrumented functions are reset after results are
displayed." t nil)

(autoload 'elp-submit-bug-report "elp" "\
Submit via mail, a bug report on elp." t nil)

;;;***

;;;### (autoloads (list-colors-display facemenu-read-color list-text-properties-at facemenu-remove-special facemenu-remove-props facemenu-set-read-only facemenu-set-intangible facemenu-set-invisible facemenu-make-much-smaller facemenu-make-much-larger facemenu-make-smaller facemenu-make-larger facemenu-set-size-default facemenu-set-face-from-menu facemenu-set-background facemenu-set-foreground facemenu-set-face) "facemenu" "utils/facemenu.el")

(define-key ctl-x-map "F" 'facemenu-keymap)

(defvar facemenu-menu nil "\
Facemenu top-level menu keymap.")

(defvar facemenu-keymap (let ((map (make-sparse-keymap "Set face"))) (define-key map ?o 'facemenu-set-face) map) "\
Keymap for face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")

(autoload 'facemenu-set-face "facemenu" "\
Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is read with the minibuffer.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload 'facemenu-set-foreground "facemenu" "\
Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload 'facemenu-set-background "facemenu" "\
Set the background color of the region or next character typed.
The color is prompted for.  A face named `bg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload 'facemenu-set-face-from-menu "facemenu" "\
Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload 'facemenu-set-size-default "facemenu" nil t nil)

(autoload 'facemenu-make-larger "facemenu" nil t nil)

(autoload 'facemenu-make-smaller "facemenu" nil t nil)

(autoload 'facemenu-make-much-larger "facemenu" nil t nil)

(autoload 'facemenu-make-much-smaller "facemenu" nil t nil)

(autoload 'facemenu-set-invisible "facemenu" "\
Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-set-intangible "facemenu" "\
Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-set-read-only "facemenu" "\
Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-remove-props "facemenu" "\
Remove all text properties that facemenu added to region." t nil)

(autoload 'facemenu-remove-special "facemenu" "\
Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'." t nil)

(autoload 'list-text-properties-at "facemenu" "\
Pop up a buffer listing text-properties at LOCATION." t nil)

(autoload 'facemenu-read-color "facemenu" "\
Read a color using the minibuffer." nil nil)

(autoload 'list-colors-display "facemenu" "\
Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list
of colors that the current display can handle." t nil)

;;;***

;;;### (autoloads (floating-toolbar-from-extent-or-popup-mode-menu floating-toolbar-or-popup-mode-menu floating-toolbar) "floating-toolbar" "utils/floating-toolbar.el")

(autoload 'floating-toolbar "floating-toolbar" "\
Popup a toolbar near the current mouse position.
The toolbar instantiator used is taken from the 'floating-toolbar
property of any extent under the mouse.  If no such non-nil
property exists for any extent under the mouse, then the value of the
variable `floating-toolbar' is checked.  If its value si nil, then
no toolbar will be displayed.

This command should be bound to a button press event.

When called from a program, first arg EVENT should be the button
press event.  Optional second arg EXTENT-LOCAL-ONLY specifies
that only extent local toolbars should be used; this means the
`floating-toolbar' variable will not be consulted." t nil)

(autoload 'floating-toolbar-or-popup-mode-menu "floating-toolbar" "\
Like floating-toolbar, but if no toolbar is displayed
run popup-mode-menu." t nil)

(autoload 'floating-toolbar-from-extent-or-popup-mode-menu "floating-toolbar" "\
Like floating-toolbar-or-popup-mode-menu, but search only for an
extent local toolbar." t nil)

;;;***

;;;### (autoloads (enable-flow-control-on enable-flow-control) "flow-ctrl" "utils/flow-ctrl.el")

(autoload 'enable-flow-control "flow-ctrl" "\
Toggle flow control handling.
When handling is enabled, user can type C-s as C-\\, and C-q as C-^.
With arg, enable flow control mode if arg is positive, otherwise disable." t nil)

(autoload 'enable-flow-control-on "flow-ctrl" "\
Enable flow control if using one of a specified set of terminal types.
Use `(enable-flow-control-on \"vt100\" \"h19\")' to enable flow control
on VT-100 and H19 terminals.  When flow control is enabled,
you must type C-\\ to get the effect of a C-s, and type C-^
to get the effect of a C-q.

This function has no effect unless the current device is a tty.

The tty terminal type is determined from the TERM environment variable.
Trailing hyphens and everything following is stripped, so a TERM
value of \"vt100-nam\" is treated the same as \"vt100\"." nil nil)

;;;***

;;;### (autoloads (forms-find-file-other-window forms-find-file forms-mode) "forms" "utils/forms.el")

(autoload 'forms-mode "forms" "\
Major mode to visit files in a field-structured manner using a form.

Commands:                        Equivalent keys in read-only mode:
 TAB            forms-next-field          TAB
 \\C-c TAB       forms-next-field          
 \\C-c <         forms-first-record         <
 \\C-c >         forms-last-record          >
 \\C-c ?         describe-mode              ?
 \\C-c \\C-k      forms-delete-record
 \\C-c \\C-q      forms-toggle-read-only     q
 \\C-c \\C-o      forms-insert-record
 \\C-c \\C-l      forms-jump-record          l
 \\C-c \\C-n      forms-next-record          n
 \\C-c \\C-p      forms-prev-record          p
 \\C-c \\C-r      forms-search-backward      r
 \\C-c \\C-s      forms-search-forward       s
 \\C-c \\C-x      forms-exit                 x
" t nil)

(autoload 'forms-find-file "forms" "\
Visit a file in Forms mode." t nil)

(autoload 'forms-find-file-other-window "forms" "\
Visit a file in Forms mode in other window." t nil)

;;;***

;;;### (autoloads (unhide-copyleft-region hide-copyleft-region) "hide-copyleft" "utils/hide-copyleft.el")

(autoload 'hide-copyleft-region "hide-copyleft" "\
Make the legal drivel at the front of this file invisible.  Unhide it again
with C-u \\[hide-copyleft-region]." t nil)

(autoload 'unhide-copyleft-region "hide-copyleft" "\
If the legal nonsense at the top of this file is elided, make it visible again." nil nil)

;;;***

;;;### (autoloads (highlight-headers-follow-url highlight-headers-follow-url-mosaic highlight-headers-follow-url-netscape highlight-headers) "highlight-headers" "utils/highlight-headers.el")

(autoload 'highlight-headers "highlight-headers" "\
Highlight message headers between start and end.
Faces used:
  message-headers			the part before the colon
  message-header-contents		the part after the colon
  message-highlighted-header-contents	contents of \"special\" headers
  message-cited-text			quoted text from other messages

Variables used:

  highlight-headers-regexp			what makes a \"special\" header
  highlight-headers-citation-regexp		matches lines of quoted text
  highlight-headers-citation-header-regexp	matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)" nil nil)

(autoload 'highlight-headers-follow-url-netscape "highlight-headers" nil nil nil)

(autoload 'highlight-headers-follow-url-mosaic "highlight-headers" nil nil nil)

(autoload 'highlight-headers-follow-url "highlight-headers" nil t nil)

;;;***

;;;### (autoloads (make-hippie-expand-function hippie-expand) "hippie-exp" "utils/hippie-exp.el")

(defvar hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol) "\
The list of expansion functions tried in order by `hippie-expand'.
To change the behavior of `hippie-expand', remove, change the order of,
or insert functions in this list.")

(defvar hippie-expand-verbose t "\
*Non-nil makes `hippie-expand' output which function it is trying.")

(defvar hippie-expand-max-buffers nil "\
*The maximum number of buffers (apart from the current) searched.
If nil, all buffers are searched.")

(defvar hippie-expand-ignore-buffers '("^ \\*.*\\*$" dired-mode) "\
*A list specifying which buffers not to search (if not current).
Can contain both regexps matching buffer names (as strings) and major modes
\(as atoms)")

(autoload 'hippie-expand "hippie-exp" "\
Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.  
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument], 
undoes the expansion." t nil)

(autoload 'make-hippie-expand-function "hippie-exp" "\
Construct a function similar to `hippie-expand'.
Make it use the expansion functions in TRY-LIST.  An optional second
argument VERBOSE non-nil makes the function verbose." nil 'macro)

;;;***

;;;### (autoloads (id-select-double-click-hook id-select-and-kill-thing id-select-and-copy-thing id-select-goto-matching-tag id-select-thing-with-mouse id-select-thing) "id-select" "utils/id-select.el")

(autoload 'id-select-thing "id-select" "\
Mark the region selected by the syntax of the thing at point.
If invoked repeatedly, selects bigger and bigger things.
If `id-select-display-type' is non-nil, the type of selection is displayed in
the minibuffer." t nil)

(autoload 'id-select-thing-with-mouse "id-select" "\
Select a region based on the syntax of the character from a mouse click.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer." t nil)

(autoload 'id-select-goto-matching-tag "id-select" "\
If in a major mode listed in `id-select-markup-modes,' moves point to the start of the tag paired with the closest tag that point is within or precedes.
Returns t if point is moved, else nil.
Signals an error if no tag is found following point or if the closing tag
does not have a `>' terminator character." t nil)

(autoload 'id-select-and-copy-thing "id-select" "\
Copy the region surrounding the syntactical unit at point." t nil)

(autoload 'id-select-and-kill-thing "id-select" "\
Kill the region surrounding the syntactical unit at point." t nil)

(autoload 'id-select-double-click-hook "id-select" "\
Select a region based on the syntax of the character wherever the mouse is double-clicked.
If the double-click occurs at the same point as the last double-click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer." nil nil)

;;;***

;;;### (autoloads (unload-feature) "loadhist" "utils/loadhist.el")

(autoload 'unload-feature "loadhist" "\
Unload the library that provided FEATURE, restoring all its autoloads.
If the feature is required by any other loaded code, and optional FORCE
is nil, raise an error." t nil)

;;;***

;;;### (autoloads (what-domain mail-extract-address-components) "mail-extr" "utils/mail-extr.el")

(autoload 'mail-extract-address-components "mail-extr" "\
Given an RFC-822 ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
ADDRESS may be a string or a buffer.  If it is a buffer, the visible 
 (narrowed) portion of the buffer will be interpreted as the address.
 (This feature exists so that the clever caller might be able to avoid
 consing a string.)
If ADDRESS contains more than one RFC-822 address, only the first is
 returned.  Some day this function may be extended to extract multiple
 addresses, or perhaps return the position at which parsing stopped." nil nil)

(autoload 'what-domain "mail-extr" "\
Prompts for a mail domain, and prints the country it corresponds to
in the minibuffer." t nil)

;;;***

;;;### (autoloads (mail-fetch-field mail-file-babyl-p) "mail-utils" "utils/mail-utils.el")

(defvar mail-use-rfc822 nil "\
*If non-nil, use a full, hairy RFC822 parser on mail addresses.
Otherwise, (the default) use a smaller, somewhat faster, and
often correct parser.")

(autoload 'mail-file-babyl-p "mail-utils" nil nil nil)

(autoload 'mail-fetch-field "mail-utils" "\
Return the value of the header field FIELD-NAME.
The buffer is expected to be narrowed to just the headers of the message.
If second arg LAST is non-nil, use the last such field if there are several.
If third arg ALL is non-nil, concatenate all such fields with commas between." nil nil)

;;;***

;;;### (autoloads (read-passwd) "passwd" "utils/passwd.el")

(autoload 'read-passwd "passwd" "\
Prompts for a password in the minibuffer, and returns it as a string.
If PROMPT may be a prompt string or an alist of elements 
'(prompt . default).
If optional arg CONFIRM is true, then ask the user to type the password
again to confirm that they typed it correctly.
If optional arg DEFAULT is provided, then it is a string to insert as
the default choice (it is not, of course, displayed.)

If running under X, the keyboard will be grabbed (with XGrabKeyboard())
to reduce the possibility that eavesdropping is occuring.

When reading a password, all keys self-insert, except for:
\\<read-passwd-map>
	\\[read-passwd-erase-line]	Erase the entire line.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

The returned value is always a newly-created string.  No additional copies
of the password remain after this function has returned.

NOTE: unless great care is taken, the typed password will exist in plaintext
form in the running image for an arbitrarily long time.  Priveleged users may
be able to extract it from memory.  If emacs crashes, it may appear in the
resultant core file.

Some steps you can take to prevent the password from being copied around:

 - as soon as you are done with the returned string, destroy it with
   (fillarray string 0).  The same goes for any default passwords
   or password histories.

 - do not copy the string, as with concat or substring - if you do, be
   sure to keep track of and destroy all copies.

 - do not insert the password into a buffer - if you do, be sure to 
   overwrite the buffer text before killing it, as with the functions 
   `passwd-erase-buffer' or `passwd-kill-buffer'.  Note that deleting
   the text from the buffer does NOT necessarily remove the text from
   memory.

 - be careful of the undo history - if you insert the password into a 
   buffer which has undo recording turned on, the password will be 
   copied onto the undo list, and thus recoverable.

 - do not pass it as an argument to a shell command - anyone will be
   able to see it if they run `ps' at the right time.

Note that the password will be temporarily recoverable with the `view-lossage'
command.  This data will not be overwritten until another hundred or so 
characters are typed.  There's not currently a way around this." nil nil)

;;;***

;;;### (autoloads (pp-eval-last-sexp pp-eval-expression pp) "pp" "utils/pp.el")

(defalias 'pprint 'pp)

(autoload 'pp "pp" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)." nil nil)

(autoload 'pp-eval-expression "pp" "\
Evaluate EXPRESSION and pretty-print value into a new display buffer.
If the pretty-printed value fits on one line, the message line is used
instead.  Value is also consed on to front of variable  values 's
value." t nil)

(autoload 'pp-eval-last-sexp "pp" "\
Run `pp-eval-expression' on sexp before point (which see).
With argument, pretty-print output into current buffer.
Ignores leading comment characters." t nil)

;;;***

;;;### (autoloads (prettyexpand-all-sexp prettyexpand-sexp macroexpand-all-sexp macroexpand-sexp pp-plist pp-variable pp-function) "pretty-print" "utils/pretty-print.el")

(autoload 'pp-function "pretty-print" "\
Pretty print the function definition of SYMBOL in a separate buffer" t nil)

(autoload 'pp-variable "pretty-print" "\
Pretty print the variable value of SYMBOL in a separate buffer" t nil)

(autoload 'pp-plist "pretty-print" "\
Pretty print the property list of SYMBOL in a separate buffer" t nil)

(autoload 'macroexpand-sexp "pretty-print" "\
Macro expand the sexpression following point. Pretty print expansion in a
temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer." t nil)

(autoload 'macroexpand-all-sexp "pretty-print" "\
Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer." t nil)

(autoload 'prettyexpand-sexp "pretty-print" "\
Macro expand the sexpression following point. Pretty print expansion
in a temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer.  
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer." t nil)

(autoload 'prettyexpand-all-sexp "pretty-print" "\
Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer.
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer." t nil)

;;;***

;;;### (autoloads (reporter-submit-bug-report) "reporter" "utils/reporter.el")

(autoload 'reporter-submit-bug-report "reporter" nil nil nil)

;;;***

;;;### (autoloads (make-ring ringp) "ring" "utils/ring.el")

(autoload 'ringp "ring" "\
Returns t if X is a ring; nil otherwise." nil nil)

(define-compatible-function-alias 'ring-p 'ringp)

(autoload 'make-ring "ring" "\
Make a ring that can contain SIZE elements." nil nil)

;;;***

;;;### (autoloads (savehist-save savehist-load) "savehist" "utils/savehist.el")

(autoload 'savehist-load "savehist" "\
Load the minibuffer histories from `savehist-file'.
Unless NO-HOOK is specified, the function will also add the save function
to `kill-emacs-hook', thus ensuring that the minibuffer contents will be
saved before leaving Emacs.

This function should be normally used from your Emacs init file.  Since it
removes your current minibuffer histories, it is unwise to call it at any
other time." t nil)

(autoload 'savehist-save "savehist" "\
Save the histories from `savehist-history-variables' to `savehist-file'.
A variable will be saved if it is bound and non-nil." t nil)

;;;***

;;;### (autoloads (skeleton-pair-insert-maybe skeleton-insert skeleton-proxy skeleton-proxy-new define-skeleton) "skeleton" "utils/skeleton.el")

(defvar skeleton-filter 'identity "\
Function for transforming a skeleton proxy's aliases' variable value.")

(autoload 'define-skeleton "skeleton" "\
Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the skeleton, has a documentation to that effect.
INTERACTOR and ELEMENT ... are as defined under `skeleton-insert'." nil 'macro)

(autoload 'skeleton-proxy-new "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).
 
When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload 'skeleton-proxy "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload 'skeleton-insert "skeleton" "\
Insert the complex statement skeleton SKELETON describes very concisely.

With optional third REGIONS wrap first interesting point (`_') in skeleton
around next REGIONS words, if REGIONS is positive.  If REGIONS is negative,
wrap REGIONS preceding interregions into first REGIONS interesting positions
\(successive `_'s) in skeleton.  An interregion is the stretch of text between
two contiguous marked points.  If you marked A B C [] (where [] is the cursor)
in alphabetical order, the 3 interregions are simply the last 3 regions.  But
if you marked B A [] C, the interregions are B-A, A-[], []-C.

Optional fourth STR is the value for the variable `str' within the skeleton.
When this is non-`nil' the interactor gets ignored, and this should be a valid
skeleton element.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and indent according to mode
	_	interesting point, interregion here, point after termination
	>	indent line (or interregion if > _) according to major mode
	&	do next ELEMENT if previous moved point
	|	do next ELEMENT if previous didn't move point
	-num	delete num preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

Further elements can be defined via `skeleton-further-elements'.  ELEMENT may
itself be a SKELETON with an INTERACTOR.  The user is prompted repeatedly for
different inputs.  The SKELETON is processed as often as the user enters a
non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in such
a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.  Such an INTERACTOR may also a list of
strings with the subskeleton being repeated once for each string.

Quoted Lisp expressions are evaluated evaluated for their side-effect.
Other Lisp expressions are evaluated and the value treated as above.
Note that expressions may not return `t' since this implies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or `nil'
	input	initial input (string or cons with index) while reading str
	v1, v2	local variables for memorizing anything you want

When done with skeleton, but before going back to `_'-point call
`skeleton-end-hook' if that is non-`nil'." nil nil)

(autoload 'skeleton-pair-insert-maybe "skeleton" "\
Insert the character you type ARG times.

With no ARG, if `skeleton-pair' is non-nil, pairing can occur.  If the region
is visible the pair is wrapped around it depending on `skeleton-autowrap'.
Else, if `skeleton-pair-on-word' is non-nil or we are not before or inside a
word, and if `skeleton-pair-filter' returns nil, pairing is performed.

If a match is found in `skeleton-pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and `' for the
symmetrical ones, and the same character twice for the others." t nil)

;;;***

;;;### (autoloads (smtpmail-send-it) "smtpmail" "utils/smtpmail.el")

(autoload 'smtpmail-send-it "smtpmail" nil nil nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar" "utils/speedbar.el")

(defalias 'speedbar 'speedbar-frame-mode)

(autoload 'speedbar-frame-mode "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time." t nil)

(autoload 'speedbar-get-focus "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame." t nil)

;;;***

;;;### (autoloads nil "timezone" "utils/timezone.el")

(define-error 'invalid-date "Invalid date string")

;;;***

;;;### (autoloads (toolbar-kill-item toolbar-kill-item-pos toolbar-add-item restore-initial-toolbar) "toolbar-utils" "utils/toolbar-utils.el")

(autoload 'restore-initial-toolbar "toolbar-utils" "\
Restores the default toolbar defined by initial-toolbar-spec." t nil)

(autoload 'toolbar-add-item "toolbar-utils" "\
Add a toolbar item ITEM at the first location of the toolbar specifier. 
Optionally, can specify an INDEX position to insert the ITEM.  The default is
to use default-toolbar, but a different specifier can by specified with 
TOOLBAR-SPEC." nil nil)

(autoload 'toolbar-kill-item-pos "toolbar-utils" "\
Remove a toolbar item ITEM at the first location of the toolbar specifier.  
Optionally, can specify an INDEX position where to remove the ITEM.  The 
default is to use default-toolbar, but a different specifier can by 
specified with TOOLBAR-SPEC." nil nil)

(autoload 'toolbar-kill-item "toolbar-utils" "\
Remove a toolbar item ITEM at the first location of the toolbar specifier.  
Optionally, can specify an ITEM to remove.  The ITEM must be in form of a 
vector.  The default is to use default-toolbar, but a different specifier 
can by specified with TOOLBAR-SPEC." nil nil)

;;;***

;;;### (autoloads (tq-create) "tq" "utils/tq.el")

(autoload 'tq-create "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine." nil nil)

;;;***

;;;### (autoloads (trace-function-background trace-function) "trace" "utils/trace.el")

(defvar trace-buffer "*trace-output*" "\
*Trace output will by default go to that buffer.")

(autoload 'trace-function "trace" "\
Traces FUNCTION with trace output going to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! The trace BUFFER will popup whenever FUNCTION is called.
Do not use this to trace functions that switch buffers or do any other
display oriented stuff, use `trace-function-background' instead." t nil)

(autoload 'trace-function-background "trace" "\
Traces FUNCTION with trace output going quietly to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! Trace output will quietly go to BUFFER without changing
the window or buffer configuration at all." t nil)

;;;***

;;;### (autoloads (xbm-button-create) "xbm-button" "utils/xbm-button.el")

(autoload 'xbm-button-create "xbm-button" "\
Returns a list of XBM image instantiators for a button displaying TEXT.
The list is of the form
   (UP DOWN DISABLED)
where UP, DOWN, and DISABLED are the up, down and disabled image
instantiators for the button.

BORDER-THICKNESS specifies how many pixels should be used for the
borders on the edges of the buttons.  It should be a positive integer,
or 0 to mean no border." nil nil)

;;;***

;;;### (autoloads (xpm-button-create) "xpm-button" "utils/xpm-button.el")

(autoload 'xpm-button-create "xpm-button" "\
Returns a list of XPM image instantiators for a button displaying TEXT.
The list is of the form
   (UP DOWN DISABLED)
where UP, DOWN, and DISABLED are the up, down and disabled image
instantiators for the button.

SHADOW-THICKNESS specifies how many pixels should be used for the
shadows on the edges of the buttons.  It should be a positive integer,
or 0 to mean no shadows on the edges.
FG-COLOR is the color used to display the text.  It should be a string.
BG-COLOR is the background color the text will be displayed upon.
It should be a string." nil nil)

;;;***

(provide 'utils-autoloads)
))
