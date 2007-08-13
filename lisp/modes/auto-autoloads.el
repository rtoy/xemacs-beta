;;; DO NOT MODIFY THIS FILE
(if (featurep 'modes-autoloads) (error "Already loaded"))

;;;### (autoloads (autoconf-mode) "autoconf-mode" "modes/autoconf-mode.el")

(autoload 'autoconf-mode "autoconf-mode" "\
A major-mode to edit autoconf input files like configure.in
\\{autoconf-mode-map}
" t nil)

;;;***

;;;### (autoloads (awk-mode) "awk-mode" "modes/awk-mode.el")

(autoload 'awk-mode "awk-mode" "\
Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (bibtex-mode) "bibtex" "modes/bibtex.el")

(autoload 'bibtex-mode "bibtex" "\
Major mode for editing bibtex files.

\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and thus ignored by BibTeX.
The OPT string may be removed from a field with \\[bibtex-remove-OPT].
\\[bibtex-kill-optional-field] kills the current optional field entirely.
\\[bibtex-remove-double-quotes] removes the double-quotes around the text of
the current field.  \\[bibtex-empty-field] replaces the text of the current
field with the default \"\".

The command \\[bibtex-clean-entry] cleans the current entry, i.e. (i) removes
double-quotes from entirely numerical fields, (ii) removes OPT from all
non-empty optional fields, (iii) removes all empty optional fields, and (iv)
checks that no non-optional fields are empty.

Use \\[bibtex-find-text] to position the dot at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

The following may be of interest as well:

  Functions:
    find-bibtex-duplicates
    find-bibtex-entry-location
    hide-bibtex-entry-bodies
    sort-bibtex-entries
    validate-bibtex-buffer

  Variables:
    bibtex-clean-entry-zap-empty-opts
    bibtex-entry-field-alist
    bibtex-include-OPTannote
    bibtex-include-OPTcrossref
    bibtex-include-OPTkey
    bibtex-maintain-sorted-entries
    bibtex-mode-user-optional-fields

Fields:
    address
           Publisher's address
    annote
           Long annotation used for annotated bibliographies (begins sentence)
    author
           Name(s) of author(s), in BibTeX name format
    booktitle
           Book title when the thing being referenced isn't the whole book.
           For book entries, the title field should be used instead.
    chapter
           Chapter number
    crossref
	   The database key of the entry being cross referenced.
    edition
           Edition of a book (e.g., \"second\")
    editor
           Name(s) of editor(s), in BibTeX name format.
           If there is also an author field, then the editor field should be
           for the book or collection that the work appears in
    howpublished
            How something strange has been published (begins sentence)
    institution
           Sponsoring institution
    journal
           Journal name (macros are provided for many)
    key
           Alphabetizing and labeling key (needed when no author or editor)
    month
           Month (macros are provided)
    note
           To help the reader find a reference (begins sentence)
    number
           Number of a journal or technical report
    organization
           Organization (sponsoring a conference)
    pages
           Page number or numbers (use `--' to separate a range)
    publisher
           Publisher name
    school
           School name (for theses)
    series
           The name of a series or set of books.
           An individual book will also have its own title
    title
           The title of the thing being referenced
    type
           Type of a technical report (e.g., \"Research Note\") to be used
           instead of the default \"Technical Report\"
    volume
           Volume of a journal or multivolume work
    year
           Year---should contain only numerals
---------------------------------------------------------
Entry to this mode calls the value of bibtex-mode-hook if that value is
non-nil." t nil)

;;;***

;;;### (autoloads (c-comment-edit) "c-comment" "modes/c-comment.el")

(autoload 'c-comment-edit "c-comment" "\
Edit multi-line C comments.
This command allows the easy editing of a multi-line C comment like this:
   /*
    * ...
    * ...
    */
The comment may be indented or flush with the left margin.

If point is within a comment, that comment is used.  Otherwise the
comment to be edited is found by searching forward from point.

With one \\[universal-argument] searching starts after moving back one
  paragraph.
With two \\[universal-argument]'s searching starts at the beginning of the
  current or proceeding C function.
With three \\[universal-argument]'s searching starts at the beginning of the
  current page.
With four \\[universal-argument]'s searching starts at the beginning of the
  current buffer (clipping restrictions apply).

Once located, the comment is copied into a temporary buffer, the comment
leaders and delimiters are stripped away and the resulting buffer is
selected for editing.  The major mode of this buffer is controlled by
the variable `c-comment-edit-mode'.\\<c-comment-edit-map>

Use \\[c-comment-edit-end] when you have finished editing the comment.  The
comment will be inserted into the original buffer with the appropriate
delimiters and indention, replacing the old version of the comment.  If
you don't want your edited version of the comment to replace the
original, use \\[c-comment-edit-abort]." t nil)

;;;***

;;;### (autoloads (common-lisp-indent-function) "cl-indent" "modes/cl-indent.el")

(autoload 'common-lisp-indent-function "cl-indent" nil nil nil)

;;;***

;;;### (autoloads (c-macro-expand) "cmacexp" "modes/cmacexp.el")

(autoload 'c-macro-expand "cmacexp" "\
Expand C macros in the region, using the C preprocessor.
Normally display output in temp buffer, but
prefix arg means replace the region with it.

`c-macro-preprocessor' specifies the preprocessor to use.
Prompt for arguments to the preprocessor (e.g. `-DDEBUG -I ./include')
if the user option `c-macro-prompt-flag' is non-nil.

Noninteractive args are START, END, SUBST.
For use inside Lisp programs, see also `c-macro-expansion'." t nil)

;;;***

;;;### (autoloads (cperl-mode) "cperl-mode" "modes/cperl-mode.el")

(defalias 'perl-mode 'cperl-mode)

(autoload 'cperl-mode "cperl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>. When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion. The processing of < is special,
since most the time you mean \"less\". Cperl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate. You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:
   if, else, elsif, unless, while, until, for, and foreach.
=========(Disabled by default, see `cperl-electric-keywords'.)
The user types the keyword immediately followed by a space, which causes
the construct to be expanded, and the user is positioned where she is most
likely to want to be.
eg. when the user types a space following \"if\" the following appears in
the buffer:
            if () {     or   if ()
            }                 {
                              }
and the cursor is between the parentheses.  The user can then type some
boolean expression within the parens.  Having done that, typing
\\[cperl-linefeed] places you, appropriately indented on a new line
between the braces. If CPerl decides that you want to insert
\"English\" style construct like
            bite if angry;
it will not do any expansion. See also help on variable 
`cperl-extra-newline-before-brace'.

\\[cperl-linefeed] is a convenience replacement for typing carriage
return. It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like
            foreach (@lines) {print; print}
and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line. If you need a usual 
`newline-and-indent' behaviour, it is on \\[newline-and-indent], 
see documentation on `cperl-electric-linefeed'.

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on
font-lock-mode, `cperl-electric-lbrace-space' to t switches on
electric space between $ and {, `cperl-electric-parens-string' is the
string that contains parentheses that should be electric in CPerl (see
also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl. `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable. You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t. In this case you can switch separate options off
by setting them to `null'. Note that one may undo the extra whitespace
inserted by semis and braces in `auto-newline'-mode by consequent
\\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' (in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help]. 

It is possible to show this help automatically after some idle
time. This is regulated by variable `cperl-lazy-help-time'.  Default
with `cperl-hairy' is 5 secs idle time if the value of this variable
is nil.  It is also possible to switch this on/off from the
menu. Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of pod and
here-docs sections. In a future version results of scan may be used
for indentation too, currently they are used for highlighting only.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code. The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and 
    `cperl-auto-newline-after-colon' set. 
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for K&R and BSD indentation styles are
  `cperl-indent-level'                5    8
  `cperl-continued-statement-offset'  5    8
  `cperl-brace-offset'               -5   -8
  `cperl-label-offset'               -5   -8

If `cperl-indent-level' is 0, the statement after opening brace in column 0 is indented on `cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args." t nil)

;;;***

;;;### (autoloads (eiffel-mode) "eiffel3" "modes/eiffel3.el")

(autoload 'eiffel-mode "eiffel3" "\
Major mode for editing Eiffel programs." t nil)

;;;***

;;;### (autoloads (enriched-decode enriched-encode enriched-mode) "enriched" "modes/enriched.el")

(autoload 'enriched-mode "enriched" "\
Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.
Turning the mode on runs `enriched-mode-hook'.

More information about Enriched mode is available in the file 
etc/enriched.doc  in the Emacs distribution directory.

Commands:

\\<enriched-mode-map>\\{enriched-mode-map}" t nil)

(autoload 'enriched-encode "enriched" nil nil nil)

(autoload 'enriched-decode "enriched" nil nil nil)

;;;***

;;;### (autoloads (executable-self-display executable-set-magic) "executable" "modes/executable.el")

(autoload 'executable-set-magic "executable" "\
Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable." t nil)

(autoload 'executable-self-display "executable" "\
Turn a text file into a self-displaying Un*x command.
The magic number of such a command displays all lines but itself." t nil)

;;;***

;;;### (autoloads (hide-ifdef-mode) "hideif" "modes/hideif.el")

(add-minor-mode 'hide-ifdef-mode " Ifdef" 'hide-ifdef-mode-map)

(autoload 'hide-ifdef-mode "hideif" "\
Toggle Hide-Ifdef mode.  This is a minor mode, albeit a large one.
With ARG, turn Hide-Ifdef mode on if arg is positive, off otherwise.
In Hide-Ifdef mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of `hide-ifdef-env'
	is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.  
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}" t nil)

(defvar hide-ifdef-initially nil "\
*Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated.")

(defvar hide-ifdef-read-only nil "\
*Set to non-nil if you want buffer to be read-only while hiding text.")

(defvar hide-ifdef-lines nil "\
*Non-nil means hide the #ifX, #else, and #endif lines.")

;;;***

;;;### (autoloads (hs-minor-mode hs-hide-block hs-hide-all) "hideshow" "modes/hideshow.el")

(defcustom hs-minor-mode nil "Non-nil if using hideshow mode as a minor mode of some other mode.\nUse the command `hs-minor-mode' to toggle this variable." :type 'boolean :set (lambda (symbol value) (hs-minor-mode (or value 0))) :initialize 'custom-initialize-default :require 'hideshow :group 'hideshow)

(autoload 'hs-hide-all "hideshow" "\
Hides all top-level blocks, displaying only first and last lines.
It moves point to the beginning of the line, and it runs the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'." t nil)

(autoload 'hs-hide-block "hideshow" "\
Selects a block and hides it.  With prefix arg, reposition at end.
Block is defined as a sexp for lispish modes, mode-specific otherwise.
Comments are blocks, too.  Upon completion, point is at repositioned and
the normal hook `hs-hide-hook' is run.  See documentation for `run-hooks'." t nil)

(autoload 'hs-minor-mode "hideshow" "\
Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.  The variables
`selective-display' and `selective-display-ellipses' are set to t.
Last, the normal hook `hs-minor-mode-hook' is run; see the doc for `run-hooks'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands." t nil)

;;;***

;;;### (autoloads (icon-mode) "icon" "modes/icon.el")

(autoload 'icon-mode "icon" "\
Major mode for editing Icon code.
Expression and list commands understand all Icon brackets.
Tab indents for Icon code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{icon-mode-map}
Variables controlling indentation style:
 icon-tab-always-indent
    Non-nil means TAB in Icon mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 icon-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in Icon code.
 icon-indent-level
    Indentation of Icon statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 icon-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 icon-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `icon-continued-statement-offset'.
 icon-brace-offset
    Extra indentation for line if it starts with an open brace.
 icon-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Turning on Icon mode calls the value of the variable `icon-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (image-decode-xpm image-decode-png image-decode-gif image-decode-jpeg image-mode) "image-mode" "modes/image-mode.el")

(autoload 'image-mode "image-mode" "\
\\{image-mode-map}" t nil)

(autoload 'image-decode-jpeg "image-mode" "\
Decode JPEG image between START and END." nil nil)

(autoload 'image-decode-gif "image-mode" "\
Decode GIF image between START and END." nil nil)

(autoload 'image-decode-png "image-mode" "\
Decode PNG image between START and END." nil nil)

(autoload 'image-decode-xpm "image-mode" "\
Decode XPM image between START and END." nil nil)

;;;***

;;;### (autoloads (turn-on-lazy-shot lazy-shot-mode) "lazy-shot" "modes/lazy-shot.el")

(autoload 'lazy-shot-mode "lazy-shot" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive." t nil)

(autoload 'turn-on-lazy-shot "lazy-shot" "\
Unconditionally turn on Lazy Lock mode." nil nil)

;;;***

;;;### (autoloads (linuxdoc-sgml-mode) "linuxdoc-sgml" "modes/linuxdoc-sgml.el")

(autoload 'linuxdoc-sgml-mode "linuxdoc-sgml" "\
Major mode based on SGML mode for editing linuxdoc-sgml documents.
See the documentation on sgml-mode for more info. This mode
understands the linuxdoc-sgml tags." t nil)

;;;***

;;;### (autoloads (define-mail-alias build-mail-aliases mail-aliases-setup) "mail-abbrevs" "modes/mail-abbrevs.el")

(defcustom mail-abbrev-mailrc-file nil "Name of file with mail aliases.   If nil, ~/.mailrc is used." :type '(choice (const :tag "Default" nil) file) :group 'mail-abbrevs)

(defvar mail-aliases nil "\
Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(autoload 'mail-aliases-setup "mail-abbrevs" nil nil nil)

(autoload 'build-mail-aliases "mail-abbrevs" "\
Read mail aliases from .mailrc and set mail-aliases." nil nil)

(autoload 'define-mail-alias "mail-abbrevs" "\
Define NAME as a mail-alias that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas." t nil)

;;;***

;;;### (autoloads (makefile-mode) "make-mode" "modes/make-mode.el")

(autoload 'makefile-mode "make-mode" "\
Major mode for editing Makefiles.
This function ends by invoking the function(s) `makefile-mode-hook'.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

Makefile mode can be configured by modifying the following variables:

makefile-browser-buffer-name:
    Name of the macro- and target browser buffer.

makefile-target-colon:
    The string that gets appended to all target names
    inserted by `makefile-insert-target'.
    \":\" or \"::\" are quite common values.

makefile-macro-assign:
   The string that gets appended to all macro names
   inserted by `makefile-insert-macro'.
   The normal value should be \" = \", since this is what
   standard make expects. However, newer makes such as dmake
   allow a larger variety of different macro assignments, so you
   might prefer to use \" += \" or \" := \" .

makefile-tab-after-target-colon:
   If you want a TAB (instead of a space) to be appended after the
   target colon, then set this to a non-nil value.

makefile-browser-leftmost-column:
   Number of blanks to the left of the browser selection mark.

makefile-browser-cursor-column:
   Column in which the cursor is positioned when it moves
   up or down in the browser.

makefile-browser-selected-mark:
   String used to mark selected entries in the browser.

makefile-browser-unselected-mark:
   String used to mark unselected entries in the browser.

makefile-browser-auto-advance-after-selection-p:
   If this variable is set to a non-nil value the cursor
   will automagically advance to the next line after an item
   has been selected in the browser.

makefile-pickup-everything-picks-up-filenames-p:
   If this variable is set to a non-nil value then
   `makefile-pickup-everything' also picks up filenames as targets
   (i.e. it calls `makefile-find-filenames-as-targets'), otherwise
   filenames are omitted.

makefile-cleanup-continuations-p:
   If this variable is set to a non-nil value then makefile-mode
   will assure that no line in the file ends with a backslash
   (the continuation character) followed by any whitespace.
   This is done by silently removing the trailing whitespace, leaving
   the backslash itself intact.
   IMPORTANT: Please note that enabling this option causes makefile-mode
   to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \"it seems necessary\".

makefile-browser-hook:
   A function or list of functions to be called just before the
   browser is entered. This is executed in the makefile buffer.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a `.'.
   at the beginning of a line in Makefile mode." t nil)

;;;***

;;;### (autoloads (modula-2-mode) "modula2" "modes/modula2.el")

(autoload 'modula-2-mode "modula2" "\
This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing C-c
followed by the first character of the construct.
\\<m2-mode-map>
  \\[m2-begin] begin         \\[m2-case] case
  \\[m2-definition] definition    \\[m2-else] else
  \\[m2-for] for           \\[m2-header] header
  \\[m2-if] if            \\[m2-module] module
  \\[m2-loop] loop          \\[m2-or] or
  \\[m2-procedure] procedure     Control-c Control-w with
  \\[m2-record] record        \\[m2-stdio] stdio
  \\[m2-type] type          \\[m2-until] until
  \\[m2-var] var           \\[m2-while] while
  \\[m2-export] export        \\[m2-import] import
  \\[m2-begin-comment] begin-comment \\[m2-end-comment] end-comment
  \\[suspend-emacs] suspend Emacs     \\[m2-toggle] toggle
  \\[m2-compile] compile           \\[m2-next-error] next-error
  \\[m2-link] link

   `m2-indent' controls the number of spaces for each indentation.
   `m2-compile-command' holds the command to compile a Modula-2 program.
   `m2-link-command' holds the command to link a Modula-2 program." t nil)

;;;***

;;;### (autoloads (electric-nroff-mode nroff-mode) "nroff-mode" "modes/nroff-mode.el")

(autoload 'nroff-mode "nroff-mode" "\
Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs `text-mode-hook', then `nroff-mode-hook'.
Also, try `nroff-electric-mode', for automatically inserting
closing requests for requests that are used in matched pairs." t nil)

(autoload 'electric-nroff-mode "nroff-mode" "\
Toggle `nroff-electric-newline' minor mode.
`nroff-electric-newline' forces Emacs to check for an nroff request at the
beginning of the line, and insert the matching closing request if necessary.
This command toggles that mode (off->on, on->off), with an argument,
turns it on iff arg is positive, otherwise off." t nil)

(defvar nroff-electric-mode nil "\
Non-nil if in electric-nroff minor mode.")

(add-minor-mode 'nroff-electric-mode " Electric" nil nil 'electric-nroff-mode)

;;;***

;;;### (autoloads (outl-mouse-minor-mode outl-mouse-mode) "outl-mouse" "modes/outl-mouse.el")

(autoload 'outl-mouse-mode "outl-mouse" "\
Calls outline-mode, with outl-mouse extensions" t nil)

(autoload 'outl-mouse-minor-mode "outl-mouse" "\
Toggles outline-minor-mode, with outl-mouse extensions" t nil)

;;;***

;;;### (autoloads (outline-minor-mode outline-mode) "outline" "modes/outline.el")

(defvar outline-minor-mode nil "\
Non-nil if using Outline mode as a minor mode of some other mode.")

(make-variable-buffer-local 'outline-minor-mode)

(put 'outline-minor-mode 'permanent-local t)

(add-minor-mode 'outline-minor-mode " Outl")

(autoload 'outline-mode "outline" "\
Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines. 

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end 
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil." t nil)

(autoload 'outline-minor-mode "outline" "\
Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode." t nil)

;;;***

;;;### (autoloads (pascal-mode) "pascal" "modes/pascal.el")

(autoload 'pascal-mode "pascal" "\
Major mode for editing Pascal code. \\<pascal-mode-map>
TAB indents for Pascal code.  Delete converts tabs to spaces as it moves back.

\\[pascal-complete-word] completes the word around current point with respect to position in code
\\[pascal-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[pascal-mark-defun]	- Mark function.
\\[pascal-insert-block]	- insert begin ... end;
\\[pascal-star-comment]	- insert (* ... *)
\\[pascal-comment-area]	- Put marked area in a comment, fixing nested comments.
\\[pascal-uncomment-area]	- Uncomment an area commented with \\[pascal-comment-area].
\\[pascal-beg-of-defun]	- Move to beginning of current function.
\\[pascal-end-of-defun]	- Move to end of current function.
\\[pascal-goto-defun]	- Goto function prompted for in the minibuffer.
\\[pascal-outline]	- Enter pascal-outline-mode (see also pascal-outline).

Variables controlling indentation/edit style:

 pascal-indent-level      (default 3)
    Indentation of Pascal statements with respect to containing block.
 pascal-case-indent       (default 2)
    Indentation for case statements.
 pascal-auto-newline      (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 pascal-tab-always-indent (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pascal-auto-endcomments  (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 pascal-auto-lineup       (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables pascal-type-keywords, pascal-start-keywords and
pascal-separator-keywords.

Turning on Pascal mode calls the value of the variable pascal-mode-hook with
no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads nil "perl-mode" "modes/perl-mode.el")

;;;***

;;;### (autoloads (picture-mode) "picture" "modes/picture.el")

(autoload 'picture-mode "picture" "\
Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
The current direction is displayed in the modeline.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  \\[picture-move-down]	  Move vertically to SAME column in previous line.
  \\[picture-move-up]	  Move vertically to SAME column in next line.
  \\[picture-end-of-line]	  Move to column following last non-whitespace character.
  \\[picture-forward-column]	  Move right inserting spaces if required.
  \\[picture-backward-column]	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  \\[picture-backward-clear-column]  Clear (replace) ARG columns before point, moving back over them.
  \\[picture-clear-line]	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  \\[picture-open-line]	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  picture-mode-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys." t nil)

(defalias 'edit-picture 'picture-mode)

;;;***

;;;### (autoloads (postscript-mode) "postscript" "modes/postscript.el")

(autoload 'postscript-mode "postscript" "\
Major mode for editing PostScript files.

\\[ps-execute-buffer] will send the contents of the buffer to the NeWS
server using psh(1).  \\[ps-execute-region] sends the current region.
\\[ps-shell] starts an interactive psh(1) window which will be used for
subsequent \\[ps-execute-buffer] or \\[ps-execute-region] commands.

In this mode, TAB and \\[indent-region] attempt to indent code
based on the position of {}, [], and begin/end pairs.  The variable
ps-indent-level controls the amount of indentation used inside
arrays and begin/end pairs.  

\\{ps-mode-map}

\\[postscript-mode] calls the value of the variable postscript-mode-hook 
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (run-prolog inferior-prolog-mode prolog-mode) "prolog" "modes/prolog.el")

(autoload 'prolog-mode "prolog" "\
Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil." t nil)

(autoload 'inferior-prolog-mode "prolog" "\
Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of `prolog-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers
using the commands `send-region', `send-string' and \\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'.
'%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal." t nil)

(autoload 'run-prolog "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*." t nil)

;;;***

;;;### (autoloads (py-shell python-mode) "python-mode" "modes/python-mode.el")

(eval-when-compile (condition-case nil (progn (require 'cl) (require 'imenu)) (error nil)))

(autoload 'python-mode "python-mode" "\
Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by comment-region
py-python-command		shell command to invoke Python interpreter
py-scroll-process-buffer		always scroll Python process buffer
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if tab-width is changed" t nil)

(autoload 'py-shell "python-mode" "\
Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

See the docs for variable `py-scroll-buffer' for info on scrolling
behavior in the process window.

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter." t nil)

;;;***

;;;### (autoloads (reftex-mode turn-on-reftex) "reftex" "modes/reftex.el")

(autoload 'turn-on-reftex "reftex" "\
Turn on RefTeX minor mode." nil nil)

(autoload 'reftex-mode "reftex" "\
Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX.

Labels can be created with `\\[reftex-label]' and referenced with `\\[reftex-reference]'.
When referencing, you get a menu with all labels of a given type and
context of the label definition.  The selected label is inserted as a
\\ref macro.

Citations can be made with `\\[reftex-citation]' which will use a regular expression
to pull out a *formatted* list of articles from your BibTeX
database.  The selected citation is inserted as a \\cite macro.

A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `\\[reftex-toc]'.

Most command have help available on the fly.  This help is accessed by
pressing `?' to any prompt mentioning this feature.

Extensive documentation about RefTeX is in the file header of `reftex.el'.
You can view this information with `\\[reftex-show-commentary]'.

\\{reftex-mode-map}
Under X, these and other functions will also be available as `Ref' menu
on the menu bar.

------------------------------------------------------------------------------" t nil)

;;;***

;;;### (autoloads (rexx-mode) "rexx-mode" "modes/rexx-mode.el")

(autoload 'rexx-mode "rexx-mode" "\
Major mode for editing REXX code.
\\{rexx-mode-map}

Variables controlling indentation style:
 rexx-indent
	The basic indentation for do-blocks.
 rexx-end-indent
	The relative offset of the \"end\" statement. 0 places it in the
	same column as the statements of the block. Setting it to the same
	value as rexx-indent places the \"end\" under the do-line.
 rexx-cont-indent
	The indention for lines following \"then\", \"else\" and \",\"
	(continued) lines.
 rexx-tab-always-indent
	Non-nil means TAB in REXX mode should always reindent the current 
	line, regardless of where in the line the point is when the TAB
	command is used.

If you have set rexx-end-indent to a nonzero value, you probably want to
remap RETURN to rexx-indent-newline-indent. It makes sure that lines
indents correctly when you press RETURN.

An extensive abbreviation table consisting of all the keywords of REXX are
supplied. Expanded keywords are converted into upper case making it
easier to distinguish them. To use this feature the buffer must be in
abbrev-mode. (See example below.)

Turning on REXX mode calls the value of the variable rexx-mode-hook with
no args, if that value is non-nil.

For example:
\(setq rexx-mode-hook '(lambda ()
			(setq rexx-indent 4)
			(setq rexx-end-indent 4)
			(setq rexx-cont-indent 4)
			(local-set-key \"\\C-m\" 'rexx-indent-newline-indent)
			(abbrev-mode 1)
			))

will make the END aligned with the DO/SELECT. It will indent blocks and
IF-statements four steps and make sure that the END jumps into the
correct position when RETURN is pressed. Finally it will use the abbrev
table to convert all REXX keywords into upper case." t nil)

;;;***

;;;### (autoloads (resize-minibuffer-mode) "rsz-minibuf" "modes/rsz-minibuf.el")

(autoload 'resize-minibuffer-mode "rsz-minibuf" "\
Enable or disable resize-minibuffer mode.
A negative prefix argument disables this mode.  A positive argument or
argument of 0 enables it.

When this minor mode is enabled, the minibuffer is dynamically resized to
contain the entire region of text put in it as you type.

The variable `resize-minibuffer-mode' is set to t or nil depending on
whether this mode is active or not.

The maximum height to which the minibuffer can grow is controlled by the
variable `resize-minibuffer-window-max-height'.

The variable `resize-minibuffer-window-exactly' determines whether the
minibuffer window should ever be shrunk to make it no larger than needed to
display its contents.

When using a window system, it is possible for a minibuffer to be the sole
window in a frame.  Since that window is already its maximum size, the only
way to make more text visible at once is to increase the size of the frame.
The variable `resize-minibuffer-frame' controls whether this should be
done.  The variables `resize-minibuffer-frame-max-height' and
`resize-minibuffer-frame-exactly' are analogous to their window
counterparts." t nil)

;;;***

;;;### (autoloads (scheme-mode) "scheme" "modes/scheme.el")

(autoload 'scheme-mode "scheme" "\
Major mode for editing Scheme code.
Editing commands are similar to those of lisp-mode.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\".  For more information
see the documentation for xscheme-interaction-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of scheme-mode-hook
if that value is non-nil." t nil)

;;;***

;;;### (autoloads (scribe-mode) "scribe" "modes/scribe.el")

(autoload 'scribe-mode "scribe" "\
Major mode for editing files of Scribe (a text formatter) source.
Scribe-mode is similar text-mode, with a few extra commands added.
\\{scribe-mode-map}

Interesting variables:

scribe-fancy-paragraphs
  Non-nil makes Scribe mode use a different style of paragraph separation.

scribe-electric-quote
  Non-nil makes insert of double quote use `` or '' depending on context.

scribe-electric-parenthesis
  Non-nil makes an open-parenthesis char (one of `([<{')
  automatically insert its close if typed after an @Command form." t nil)

;;;***

;;;### (autoloads (mail-other-frame mail-other-window mail mail-mode user-mail-address) "sendmail" "modes/sendmail.el")

(defvar mail-from-style 'angles "\
*Specifies how \"From:\" fields look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>")

(defvar mail-self-blind nil "\
Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

(defvar mail-interactive nil "\
Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

(defvar mail-dir nil "\
*Default directory for saving messages.")

(defvar rmail-ignored-headers (purecopy (concat "^\\(" (mapconcat 'identity '("Sender:" "References:" "Return-Path:" "Received:" "[^: 	\n]*Message-ID:" "Errors-To:" "Path:" "Expires:" "Xref:" "Lines:" "Approved:" "Distribution:" "Content-Length:" "Mime-Version:" "Content-Type:" "Content-Transfer-Encoding:" "X400-Received:" "X400-Originator:" "X400-Mts-Identifier:" "X400-Content-Type:" "Content-Identifier:" "Status:" "Summary-Line:" "X-Attribution:" "Via:" "Sent-Via:" "Mail-From:" "Origin:" "Comments:" "Originator:" "NF-ID:" "NF-From:" "Posting-Version:" "Posted:" "Posted-Date:" "Date-Received:" "Relay-Version:" "Article-I\\.D\\.:" "NNTP-Version:" "NNTP-Posting-Host:" "X-Mailer:" "X-Newsreader:" "News-Software:" "X-Received:" "X-References:" "X-Envelope-To:" "X-VMS-" "Remailed-" "X-Plantation:" "X-Windows:" "X-Pgp-") "\\|") "\\)")) "\
*Gubbish header fields one would rather not see.")

(defvar mail-yank-ignored-headers (purecopy (concat rmail-ignored-headers "\\|" "^\\(" (mapconcat 'identity '("Resent-To:" "Resent-By:" "Resent-CC:" "To:" "Subject:" "In-Reply-To:") "\\|") "\\)")) "\
Delete these headers from old message when it's inserted in a reply.")

(defvar send-mail-function 'sendmail-send-it "\
Function to call to send the current buffer as mail.
The headers should be delimited by a line whose contents
match the variable `mail-header-separator'.")

(defvar mail-header-separator (purecopy "--text follows this line--") "\
*Line used to separate headers from text in messages being composed.")

(defvar mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
This can be an inbox file or an Rmail file.")

(defvar mail-default-reply-to nil "\
*Address to insert as default Reply-to field of outgoing messages.
If nil, it will be initialized from the REPLYTO environment variable
when you first send mail.")

(defvar mail-alias-file nil "\
*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.")

(defvar mail-yank-prefix "> " "\
*Prefix insert on lines of yanked message being replied to.
nil means use indentation.")

(defvar mail-signature nil "\
*Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `mail-signature-file'.")

(autoload 'user-mail-address "sendmail" "\
Query the user for his mail address, unless it is already known." t nil)

(autoload 'mail-mode "sendmail" "\
Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
	 C-c C-f C-f  move to FCC:	C-c C-f C-r  move to Reply-To:
C-c C-t  mail-text (move to beginning of message text).
C-c C-w  mail-signature (insert `mail-signature-file' file).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked).
C-c C-v  mail-sent-via (add a sent-via field for each To or CC)." t nil)

(autoload 'mail "sendmail" "\
Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

Optionally, the signature file `mail-signature-file' can be inserted at the
end; see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

The variable `mail-signature' controls whether the signature file
`mail-signature-file' is inserted immediately.

If `mail-signature' is nil, use \\[mail-signature] to insert the
signature in `mail-signature-file'.

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

The normal hook `mail-setup-hook' is run after the message is
initialized.  It can add more default fields to the message.

When calling from a program, the first argument if non-nil says
not to erase the existing contents of the `*mail*' buffer.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'." t nil)

(autoload 'mail-other-window "sendmail" "\
Like `mail' command, but display mail buffer in another window." t nil)

(autoload 'mail-other-frame "sendmail" "\
Like `mail' command, but display mail buffer in another frame." t nil)

(add-hook 'same-window-buffer-names "*mail*")

;;;***

;;;### (autoloads nil "sgml-mode" "modes/sgml-mode.el")

;;;***

;;;### (autoloads (latex-mode plain-tex-mode tex-mode) "tex-mode" "modes/tex-mode.el")

(autoload 'tex-mode "tex-mode" "\
Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls plain-tex-mode,
latex-mode, or slitex-mode, respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of tex-default-mode
is used." t nil)

(fset 'TeX-mode 'tex-mode)

(fset 'LaTeX-mode 'latex-mode)

(autoload 'plain-tex-mode "tex-mode" "\
Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
tex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Plain-tex mode calls the value of text-mode-hook, then the value of
tex-mode-hook, and then the value of plain-tex-mode-hook.  When the special
subshell is initiated, the value of tex-shell-hook is called." t nil)

(fset 'plain-TeX-mode 'plain-tex-mode)

(autoload 'latex-mode "tex-mode" "\
Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
latex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for LaTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Latex mode calls the value of text-mode-hook, then the value of
tex-mode-hook, and then the value of latex-mode-hook.  When the special
subshell is initiated, the value of tex-shell-hook is called." t nil)

;;;***

;;;### (autoloads (texinfo-mode) "texinfo" "modes/texinfo.el")

(autoload 'texinfo-mode "texinfo" "\
Major mode for editing Texinfo files.

  It has these extra commands:
\\{texinfo-mode-map}

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files with \\[makeinfo-buffer] or
the `makeinfo' program.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.  To see
what the Info version of a region of the Texinfo file will look like,
use \\[makeinfo-region], which runs `makeinfo' on the current region.

  You can show the structure of a Texinfo file with \\[texinfo-show-structure].
This command shows the structure of a Texinfo file by listing the
lines with the @-sign commands for @chapter, @section, and the like.
These lines are displayed in another window called the *Occur* window.
In that window, you can position the cursor over one of the lines and
use \\[occur-mode-goto-occurrence], to jump to the corresponding spot
in the Texinfo file.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[texinfo-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Also, Texinfo mode provides functions for automatically creating or
updating menus and node pointers.  These functions

  * insert the `Next', `Previous' and `Up' pointers of a node,
  * insert or update the menu for a section, and
  * create a master menu for a Texinfo source file.

Here are the functions:

    texinfo-update-node                \\[texinfo-update-node]
    texinfo-every-node-update          \\[texinfo-every-node-update]
    texinfo-sequential-node-update 

    texinfo-make-menu                  \\[texinfo-make-menu]
    texinfo-all-menus-update           \\[texinfo-all-menus-update]
    texinfo-master-menu

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented. 

Passed an argument (a prefix argument, if interactive), the
`texinfo-update-node' and `texinfo-make-menu' functions do their jobs
in the region.

To use the updating commands, you must structure your Texinfo file
hierarchically, such that each `@node' line, with the exception of the
Top node, is accompanied by some kind of section line, such as an
`@chapter' or `@section' line.

If the file has a `top' node, it must be called `top' or `Top' and
be the first node in the file.

Entering Texinfo mode calls the value of text-mode-hook, and then the
value of texinfo-mode-hook." t nil)

;;;***

;;;### (autoloads (vhdl-mode) "vhdl-mode" "modes/vhdl-mode.el")

(autoload 'vhdl-mode "vhdl-mode" "\
Major mode for editing VHDL code.
vhdl-mode $Revision: 1.14 $
To submit a problem report, enter `\\[vhdl-submit-bug-report]' from a
vhdl-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducable test case and send the message.

Note that the details of configuring vhdl-mode will soon be moved to the
accompanying texinfo manual.  Until then, please read the README file
that came with the vhdl-mode distribution.

The hook variable `vhdl-mode-hook' is run with no args, if that value is
bound and has a non-nil value.

Key bindings:
\\{vhdl-mode-map}" t nil)

;;;***

;;;### (autoloads (auto-view-mode view-major-mode view-mode view-minor-mode view-buffer-other-window view-file-other-window view-buffer view-file) "view-less" "modes/view-less.el")

(defvar view-minor-mode-map (let ((map (make-keymap))) (set-keymap-name map 'view-minor-mode-map) (suppress-keymap map) (define-key map "-" 'negative-argument) (define-key map " " 'scroll-up) (define-key map "f" 'scroll-up) (define-key map "b" 'scroll-down) (define-key map 'backspace 'scroll-down) (define-key map 'delete 'scroll-down) (define-key map "" 'view-scroll-lines-up) (define-key map "\n" 'view-scroll-lines-up) (define-key map "e" 'view-scroll-lines-up) (define-key map "j" 'view-scroll-lines-up) (define-key map "y" 'view-scroll-lines-down) (define-key map "k" 'view-scroll-lines-down) (define-key map "d" 'view-scroll-some-lines-up) (define-key map "u" 'view-scroll-some-lines-down) (define-key map "r" 'recenter) (define-key map "t" 'toggle-truncate-lines) (define-key map "N" 'view-buffer) (define-key map "E" 'view-file) (define-key map "P" 'view-buffer) (define-key map "!" 'shell-command) (define-key map "|" 'shell-command-on-region) (define-key map "=" 'what-line) (define-key map "?" 'view-search-backward) (define-key map "h" 'view-mode-describe) (define-key map "s" 'view-repeat-search) (define-key map "n" 'view-repeat-search) (define-key map "/" 'view-search-forward) (define-key map "\\" 'view-search-backward) (define-key map "g" 'view-goto-line) (define-key map "G" 'view-last-windowful) (define-key map "%" 'view-goto-percent) (define-key map "p" 'view-goto-percent) (define-key map "m" 'point-to-register) (define-key map "'" 'register-to-point) (define-key map "C" 'view-cleanup-backspaces) (define-key map "" 'view-quit) (define-key map "" 'view-quit-toggle-ro) (define-key map "q" 'view-quit) map))

(defvar view-mode-map (let ((map (copy-keymap view-minor-mode-map))) (set-keymap-name map 'view-mode-map) map))

(autoload 'view-file "view-less" "\
Find FILE, enter view mode.  With prefix arg OTHER-P, use other window." t nil)

(autoload 'view-buffer "view-less" "\
Switch to BUF, enter view mode.  With prefix arg use other window." t nil)

(autoload 'view-file-other-window "view-less" "\
Find FILE in other window, and enter view mode." t nil)

(autoload 'view-buffer-other-window "view-less" "\
Switch to BUFFER in another window, and enter view mode." t nil)

(autoload 'view-minor-mode "view-less" "\
Minor mode for viewing text, with bindings like `less'.
Commands are:
\\<view-minor-mode-map>
0..9	prefix args
-	prefix minus
\\[scroll-up]	page forward
\\[scroll-down]	page back
\\[view-scroll-lines-up]	scroll prefix-arg lines forward, default 1.
\\[view-scroll-lines-down]	scroll prefix-arg lines backward, default 1.
\\[view-scroll-some-lines-down]	scroll prefix-arg lines backward, default 10.
\\[view-scroll-some-lines-up]	scroll prefix-arg lines forward, default 10.
\\[what-line]	print line number
\\[view-mode-describe]	print this help message
\\[view-search-forward]	regexp search, uses previous string if you just hit RET
\\[view-search-backward]	as above but searches backward
\\[view-repeat-search]	repeat last search
\\[view-goto-line]	goto line prefix-arg, default 1
\\[view-last-windowful]	goto line prefix-arg, default last line
\\[view-goto-percent]	goto a position by percentage
\\[toggle-truncate-lines]	toggle truncate-lines
\\[view-file]	view another file
\\[view-buffer]	view another buffer
\\[view-cleanup-backspaces]	cleanup backspace constructions
\\[shell-command]	execute a shell command
\\[shell-command-on-region]	execute a shell command with the region as input
\\[view-quit]	exit view-mode, and bury the current buffer.

If invoked with the optional (prefix) arg non-nil, view-mode cleans up
backspace constructions.

More precisely:
\\{view-minor-mode-map}" t nil)

(autoload 'view-mode "view-less" "\
View the current buffer using view-minor-mode.  This exists to be 99.9%
compatible with the implementations of `view-mode' in view.el and older
versions of view-less.el." t nil)

(autoload 'view-major-mode "view-less" "\
View the current buffer using view-mode, as a major mode.
This function has a nonstandard name because `view-mode' is wrongly
named but is like this for compatibility reasons." t nil)

(autoload 'auto-view-mode "view-less" "\
If the file of the current buffer is not writable, call view-mode.
This is meant to be added to `find-file-hooks'." nil nil)

;;;***

(provide 'modes-autoloads)
