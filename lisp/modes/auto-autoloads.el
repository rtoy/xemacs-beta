;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'modes-autoloads))
    (progn

;;;### (autoloads nil "abbrev" "modes/abbrev.el")

;;;***

;;;### (autoloads (ada-make-filename-from-adaname ada-mode) "ada-mode" "modes/ada-mode.el")

(autoload 'ada-mode "ada-mode" "\
Ada Mode is the major mode for editing Ada code.

Bindings are as follows: (Note: 'LFD' is control-j.)

 Indent line                                          '\\[ada-tab]'
 Indent line, insert newline and indent the new line. '\\[newline-and-indent]'

 Re-format the parameter-list point is in             '\\[ada-format-paramlist]'
 Indent all lines in region                           '\\[ada-indent-region]'
 Call external pretty printer program                 '\\[ada-call-pretty-printer]'

 Adjust case of identifiers and keywords in region    '\\[ada-adjust-case-region]'
 Adjust case of identifiers and keywords in buffer    '\\[ada-adjust-case-buffer]'

 Call EXTERNAL pretty printer (if you have one)       '\\[ada-call-pretty-printer]'

 Fill comment paragraph                               '\\[ada-fill-comment-paragraph]'
 Fill comment paragraph and justify each line         '\\[ada-fill-comment-paragraph-justify]'
 Fill comment paragraph, justify and append postfix   '\\[ada-fill-comment-paragraph-postfix]'

 Next func/proc/task '\\[ada-next-procedure]'    Previous func/proc/task '\\[ada-previous-procedure]'
 Next package        '\\[ada-next-package]'  Previous package        '\\[ada-previous-package]'

 Goto matching start of current 'end ...;'            '\\[ada-move-to-start]'
 Goto end of current block                            '\\[ada-move-to-end]'

Comments are handled using standard GNU Emacs conventions, including:
 Start a comment                                      '\\[indent-for-comment]'
 Comment region                                       '\\[comment-region]'
 Uncomment region                                     '\\[ada-uncomment-region]'
 Continue comment on next line                        '\\[indent-new-comment-line]'

If you use imenu.el:
 Display index-menu of functions & procedures         '\\[imenu]'

If you use find-file.el:
 Switch to other file (Body <-> Spec)                 '\\[ff-find-other-file]'
                                                   or '\\[ff-mouse-find-other-file]
 Switch to other file in other window                 '\\[ada-ff-other-window]'
                                                   or '\\[ff-mouse-find-other-file-other-window]
 If you use this function in a spec and no body is available, it gets created
 with body stubs.

If you use ada-xref.el:
 Goto declaration:          '\\[ada-point-and-xref]' on the identifier
                         or '\\[ada-goto-declaration]' with point on the identifier
 Complete identifier:       '\\[ada-complete-identifier]'
 Execute Gnatf:             '\\[ada-gnatf-current]'" t nil)

(autoload 'ada-make-filename-from-adaname "ada-mode" "\
Determine the filename of a package/procedure from its own Ada name." t nil)

;;;***

;;;### (autoloads (archive-mode) "arc-mode" "modes/arc-mode.el")

(autoload 'archive-mode "arc-mode" "\
Major mode for viewing an archive file in a dired-like way.
You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the archive and into its own buffer;
or click mouse-2 on the file's line in the archive mode buffer.

If you edit a sub-file of this archive (as with the `e' command) and
save it, the contents of that buffer will be saved back into the
archive.

\\{archive-mode-map}" nil nil)

;;;***

;;;### (autoloads (asm-mode) "asm-mode" "modes/asm-mode.el")

(autoload 'asm-mode "asm-mode" "\
Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[asm-colon]	outdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]	tab to next tab stop.
\\[asm-newline]	newline, then tab to next tab stop.
\\[asm-comment]	smart placement of assembler comments.

The character used for making comments is set by the variable
`asm-comment-char' (which defaults to `?;').

Alternatively, you may set this variable in `asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Asm mode runs the hook `asm-mode-hook' at the end of initialization.

Special commands:
\\{asm-mode-map}
" t nil)

;;;***

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

;;;### (autoloads (f90-mode) "f90" "modes/f90.el")

(autoload 'f90-mode "f90" "\
Major mode for editing Fortran 90 code in free format.

\\[f90-indent-new-line] corrects current indentation and creates new indented line.
\\[f90-indent-line] indents the current line correctly. 
\\[f90-indent-subprogram] indents the current subprogram. 

Type `? or `\\[help-command] to display a list of built-in abbrevs for F90 keywords.

Key definitions:
\\{f90-mode-map}

Variables controlling indentation style and extra features:

 f90-do-indent
    Extra indentation within do blocks.  (default 3)
 f90-if-indent
    Extra indentation within if/select case/where/forall blocks. (default 3)
 f90-type-indent
    Extra indentation within type/interface/block-data blocks.  (default 3)
 f90-program-indent
    Extra indentation within program/module/subroutine/function blocks.
      (default 2)
 f90-continuation-indent
    Extra indentation applied to continuation lines.  (default 5)
 f90-comment-region
    String inserted by \\[f90-comment-region] at start of each line in 
    region.  (default \"!!!$\")
 f90-indented-comment-re
    Regexp determining the type of comment to be intended like code.
    (default \"!\")
 f90-directive-comment-re
    Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented.
    (default \"!hpf\\\\$\")
 f90-break-delimiters
    Regexp holding list of delimiters at which lines may be broken.
    (default \"[-+*/><=,% \\t]\")
 f90-break-before-delimiters
    Non-nil causes `f90-do-auto-fill' to break lines before delimiters.
    (default t)
 f90-beginning-ampersand 
    Automatic insertion of & at beginning of continuation lines. (default t)
 f90-smart-end 
    From an END statement, check and fill the end using matching block start.
    Allowed values are 'blink, 'no-blink, and nil, which determine
    whether to blink the matching beginning.) (default 'blink)
 f90-auto-keyword-case
    Automatic change of case of keywords. (default nil)
    The possibilities are 'downcase-word, 'upcase-word, 'capitalize-word.
 f90-leave-line-no
    Do not left-justify line numbers. (default nil)
 f90-startup-message
    Set to nil to inhibit message first time F90 mode is used. (default t)
 f90-keywords-re
    List of keywords used for highlighting/upcase-keywords etc.

Turning on F90 mode calls the value of the variable `f90-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (follow-delete-other-windows-and-split follow-mode turn-off-follow-mode turn-on-follow-mode) "follow" "modes/follow.el")

(add-minor-mode 'follow-mode nil 'follow-mode-map)

(autoload 'turn-on-follow-mode "follow" "\
Turn on Follow mode. Please see the function `follow-mode'." t nil)

(autoload 'turn-off-follow-mode "follow" "\
Turn off Follow mode. Please see the function `follow-mode'." t nil)

(autoload 'follow-mode "follow" "\
Minor mode which combines windows into one tall virtual window.

The feeling of a \"virtual window\" has been accomplished by the use
of two major techniques:

* The windows always displays adjacent sections of the buffer. 
  This means that whenever one window is moved, all the
  others will follow.  (Hence the name Follow Mode.)

* Should the point (cursor) end up outside a window, another 
  window displaying that point is selected, if possible.  This 
  makes it possible to walk between windows using normal cursor 
  movement commands.

Follow mode comes to its prime when used on a large screen and two
side-by-side window are used. The user can, with the help of Follow
mode, use two full-height windows as though they would have been
one. Imagine yourself editing a large function, or section of text,
and being able to use 144 lines instead of the normal 72... (your
mileage may vary).

To split one large window into two side-by-side windows, the commands
`\\[split-window-horizontally]' or `M-x follow-delete-other-windows-and-split' can be used.

Only windows displayed in the same frame follow each-other.

If the variable `follow-intercept-processes' is non-nil, Follow mode
will listen to the output of processes and redisplay accordingly.
\(This is the default.)

When Follow mode is switched on, the hook `follow-mode-hook'
is called.  When turned off, `follow-mode-off-hook' is called.

Keys specific to Follow mode:
\\{follow-mode-map}" t nil)

(autoload 'follow-delete-other-windows-and-split "follow" "\
Create two side by side windows and enter Follow Mode.

Execute this command to display as much as possible of the text
in the selected window.  All other windows, in the current 
frame, are deleted and the selected window is split in two
side-by-side windows. Follow Mode is activated, hence the 
two windows always will display two successive pages.
\(If one window is moved, the other one will follow.)

If ARG is positive, the leftmost window is selected.  If it negative,
the rightmost is selected.  If ARG is nil, the leftmost window is
selected if the original window is the first one in the frame.

To bind this command to a hotkey, place the following line
in your `~/.emacs' file, replacing [f7] by your favourite key:
    (global-set-key [f7] 'follow-delete-other-windows-and-split)" t nil)

;;;***

;;;### (autoloads (fortran-mode) "fortran" "modes/fortran.el")

(defcustom fortran-tab-mode-default nil "*Default tabbing/carriage control style for empty files in Fortran mode.\nA value of t specifies tab-digit style of continuation control.\nA value of nil specifies that continuation lines are marked\nwith a character in column 6." :type 'boolean :group 'fortran-indent)

(autoload 'fortran-mode "fortran" "\
Major mode for editing Fortran code.
\\[fortran-indent-line] indents the current Fortran line correctly. 
DO statements must not share a common CONTINUE.

Type ;? or ;\\[help-command] to display a list of built-in abbrevs for Fortran keywords.

Key definitions:
\\{fortran-mode-map}

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Fortran mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 fortran-do-indent
    Extra indentation within do blocks.  (default 3)
 fortran-if-indent
    Extra indentation within if blocks.  (default 3)
 fortran-structure-indent
    Extra indentation within structure, union, map and interface blocks.
    (default 3)
 fortran-continuation-indent
    Extra indentation applied to continuation statements.  (default 5)
 fortran-comment-line-extra-indent
    Amount of extra indentation for text within full-line comments. (default 0)
 fortran-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at `fortran-comment-line-extra-indent' beyond
           the value of `fortran-minimum-statement-indent-fixed' (for fixed
           format continuation style) or `fortran-minimum-statement-indent-tab'
           (for TAB format continuation style).
    relative  means indent at `fortran-comment-line-extra-indent' beyond the
 	      indentation for a line of code.
    (default 'fixed)
 fortran-comment-indent-char
    Single-character string to be inserted instead of space for
    full-line comment indentation.  (default \" \")
 fortran-minimum-statement-indent-fixed
    Minimum indentation for Fortran statements in fixed format mode. (def.6)
 fortran-minimum-statement-indent-tab
    Minimum indentation for Fortran statements in TAB format mode. (default 9)
 fortran-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 fortran-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible \"continue\"
    statements.  (default nil)
 fortran-blink-matching-if 
    Non-nil causes \\[fortran-indent-line] on an ENDIF statement to blink on
    matching IF.  Also, from an ENDDO statement, blink on matching DO [WHILE]
    statement.  (default nil)
 fortran-continuation-string
    Single-character string to be inserted in column 5 of a continuation
    line.  (default \"$\")
 fortran-comment-region
    String inserted by \\[fortran-comment-region] at start of each line in 
    region.  (default \"c$$$\")
 fortran-electric-line-number
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 fortran-break-before-delimiters
    Non-nil causes `fortran-fill' breaks lines before delimiters.
    (default t)
 fortran-startup-message
    Set to nil to inhibit message first time Fortran mode is used.

Turning on Fortran mode calls the value of the variable `fortran-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (hide-ifdef-mode) "hideif" "modes/hideif.el")

(add-minor-mode 'hide-ifdef-mode " Ifdef")

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

(defvar hs-minor-mode nil "\
Non-nil if using hideshow mode as a minor mode of some other mode.
Use the command `hs-minor-mode' to toggle this variable.")

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

;;;### (autoloads (imenu imenu-add-to-menubar) "imenu" "modes/imenu.el")

(defvar imenu-generic-expression nil "\
The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu-create-index-with-pattern'
to create a buffer index.

It is an alist with elements that look like this: (MENU-TITLE
REGEXP INDEX). 

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

For emacs-lisp-mode for example PATTERN would look like:

'((nil \"^\\\\s-*(def\\\\(un\\\\|subst\\\\|macro\\\\|advice\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2)
  (\"*Vars*\" \"^\\\\s-*(def\\\\(var\\\\|const\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2)
  (\"*Types*\" \"^\\\\s-*(def\\\\(type\\\\|struct\\\\|class\\\\|ine-condition\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2))

The variable is buffer-local.")

(make-variable-buffer-local 'imenu-generic-expression)

(autoload 'imenu-add-to-menubar "imenu" "\
Adds an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information." t nil)

(autoload 'imenu "imenu" "\
Jump to a place in the buffer chosen using a buffer menu or mouse menu.
See `imenu-choose-buffer-index' for more information." t nil)

;;;***

;;;### (autoloads (ksh-mode) "ksh-mode" "modes/ksh-mode.el")

(autoload 'ksh-mode "ksh-mode" "\
ksh-mode $Revision: 1.2 $ - Major mode for editing (Bourne, Korn or Bourne again)
shell scripts.
Special key bindings and commands:
\\{ksh-mode-map}
Variables controlling indentation style:
ksh-indent
    Indentation of ksh statements with respect to containing block.
    Default value is 2.
ksh-case-indent
    Additional indentation for statements under case items.
    Default value is nil which will align the statements one position 
    past the \")\" of the pattern.
ksh-case-item-offset
    Additional indentation for case items within a case statement.
    Default value is 2.
ksh-group-offset
    Additional indentation for keywords \"do\" and \"then\".
    Default value is -2.
ksh-brace-offset
    Additional indentation of \"{\" under functions or brace groupings.
    Default value is 0.
ksh-multiline-offset
   Additional indentation of line that is preceded of a line ending with a
   \\ to make it continue on next line.
ksh-tab-always-indent
    Controls the operation of the TAB key. If t (the default), always
    reindent the current line.  If nil, indent the current line only if
    point is at the left margin or in the line's indentation; otherwise
    insert a tab.
ksh-match-and-tell
    If non-nil echo in the minibuffer the matching compound command
    for the \"done\", \"}\", \"fi\", or \"esac\". Default value is t.

ksh-align-to-keyword
    Controls whether nested constructs align from the keyword or
    the current indentation. If non-nil, indentation will be relative to
    the column the keyword starts. If nil, indentation will be relative to
    the current indentation of the line the keyword is on.
    The default value is non-nil.

ksh-comment-regexp
  Regular expression used to recognize comments. Customize to support
  ksh-like languages. Default value is \"\\s *#\".

Style Guide.
 By setting
    (setq ksh-indent default-tab-width)
    (setq ksh-group-offset 0)

    The following style is obtained:

    if [ -z $foo ]
	    then
		    bar    # <-- ksh-group-offset is additive to ksh-indent
		    foo
    fi

 By setting
    (setq ksh-indent default-tab-width)
    (setq ksh-group-offset (- 0 ksh-indent))

    The following style is obtained:

    if [ -z $foo ]
    then
	    bar
	    foo
    fi

 By setting
    (setq ksh-case-item-offset 1)
    (setq ksh-case-indent nil)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ksh-case-item-offset
          baz;;         # <-- ksh-case-indent aligns with \")\"
     foobar) foo
             bar;;
    esac

 By setting
    (setq ksh-case-item-offset 1)
    (setq ksh-case-indent 6)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ksh-case-item-offset
           baz;;        # <-- ksh-case-indent
     foobar) foo
           bar;;
    esac
    

Installation:

 (setq ksh-mode-hook
      (function (lambda ()
         (font-lock-mode 1)             ;; font-lock the buffer
         (setq ksh-indent 8)
	 (setq ksh-group-offset -8)
	 (setq ksh-brace-offset -8)   
         (setq ksh-tab-always-indent t)
         (setq ksh-match-and-tell t)
         (setq ksh-align-to-keyword t)	;; Turn on keyword alignment
	 )))" t nil)

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
    Non-nil means automatically newline after semicolons and the punctuation mark
    after an end.
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

;;;### (autoloads (perl-mode) "perl-mode" "modes/perl-mode.el")

(autoload 'perl-mode "perl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all Perl brackets.
Tab indents for Perl code.
Comments are delimited with # ... \\n.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{perl-mode-map}
Variables controlling indentation style:
 perl-tab-always-indent
    Non-nil means TAB in Perl mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 perl-tab-to-comment
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move 
    to end-of-line, or if at end-of-line already, create a new comment.
 perl-nochange
    Lines starting with this regular expression are not auto-indented.
 perl-indent-level
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 perl-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 perl-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `perl-continued-statement-offset'.
 perl-brace-offset
    Extra indentation for line if it starts with an open brace.
 perl-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 perl-label-offset
    Extra indentation for line that is a label.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  perl-indent-level                5    8    0    2    4
  perl-continued-statement-offset  5    8    4    2    4
  perl-continued-brace-offset      0    0    0    0   -4
  perl-brace-offset               -5   -8    0    0    0
  perl-brace-imaginary-offset      0    0    4    0    0
  perl-label-offset               -5   -8   -2   -2   -2

Turning on Perl mode runs the normal hook `perl-mode-hook'." t nil)

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

;;;### (autoloads (reftex-add-to-label-alist reftex-mode turn-on-reftex) "reftex" "modes/reftex.el")

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

Extensive documentation about reftex is in the file header of `reftex.el'.

\\{reftex-mode-map}
Under X, these functions will also be available in a menu on the menu bar.

------------------------------------------------------------------------------" t nil)

(autoload 'reftex-add-to-label-alist "reftex" "\
Add label environment descriptions to `reftex-label-alist-external-add-ons'.
The format of ENTRY-LIST is exactly like `reftex-label-alist'.  See there 
for details.
This function makes it possible to support RefTeX from AUCTeX style files.
The entries in ENTRY-LIST will be processed after the user settings in
`reftex-label-alist', and before the defaults (specified in
`reftex-default-label-alist-entries').  Any changes made to
`reftex-label-alist-external-add-ons' will raise a flag to the effect that a
mode reset is done on the next occasion." nil nil)

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

(defgroup resize-minibuffer nil "Dynamically resize minibuffer to display entire contents" :group 'frames)

(defcustom resize-minibuffer-window-max-height nil "*Maximum size the minibuffer window is allowed to become.\nIf less than 1 or not a number, the limit is the height of the frame in\nwhich the active minibuffer window resides." :type '(choice (const nil) integer) :group 'resize-minibuffer)

(defcustom resize-minibuffer-window-exactly t "*If non-`nil', make minibuffer exactly the size needed to display all its contents.\nOtherwise, the minibuffer window can temporarily increase in size but\nnever get smaller while it is active." :type 'boolean :group 'resize-minibuffer)

(defcustom resize-minibuffer-frame nil "*If non-`nil' and the active minibuffer is the sole window in its frame, allow changing the frame height." :type 'boolean :group 'resize-minibuffer)

(defcustom resize-minibuffer-frame-max-height nil "*Maximum size the minibuffer frame is allowed to become.\nIf less than 1 or not a number, there is no limit.")

(defcustom resize-minibuffer-frame-exactly nil "*If non-`nil', make minibuffer frame exactly the size needed to display all its contents.\nOtherwise, the minibuffer frame can temporarily increase in size but\nnever get smaller while it is active." :type 'boolean :group 'resize-minibuffer)

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

(define-key ctl-x-map "m" 'mail)

(define-key ctl-x-4-map "m" 'mail-other-window)

(define-key ctl-x-5-map "m" 'mail-other-frame)

(add-hook 'same-window-buffer-names "*mail*")

;;;***

;;;### (autoloads (sh-mode) "sh-script" "modes/sh-script.el")

(put 'sh-mode 'mode-class 'special)

(autoload 'sh-mode "sh-script" "\
Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:

\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-while-getopts]	 while getopts loop
\\[sh-repeat]	 repeat loop
\\[sh-select]	 select loop
\\[sh-until]	 until loop
\\[sh-while]	 while loop

\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle." t nil)

(defalias 'shell-script-mode 'sh-mode)

;;;***

;;;### (autoloads (strokes-mode strokes-list-strokes strokes-load-user-strokes strokes-help strokes-describe-stroke strokes-do-complex-stroke strokes-do-stroke strokes-read-complex-stroke strokes-read-stroke strokes-global-set-stroke) "strokes" "modes/strokes.el")

(defvar strokes-mode nil "\
Non-nil when `strokes' is globally enabled")

(autoload 'strokes-global-set-stroke "strokes" "\
Interactively give STROKE the global binding as COMMAND.
Operated just like `global-set-key', except for strokes.
COMMAND is a symbol naming an interactively-callable function.  STROKE
is a list of sampled positions on the stroke grid as described in the
documentation for the `strokes-define-stroke' function." t nil)

(defalias 'global-set-stroke 'strokes-global-set-stroke)

(autoload 'strokes-read-stroke "strokes" "\
Read a simple stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
This function will display the stroke interactively as it is being
entered in the strokes buffer if the variable
`strokes-use-strokes-buffer' is non-nil.
Optional EVENT is currently not used, but hopefully will be soon." nil nil)

(autoload 'strokes-read-complex-stroke "strokes" "\
Read a complex stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
Note that a complex stroke allows the user to pen-up and pen-down.  This
is implemented by allowing the user to paint with button1 or button2 and
then complete the stroke with button3.
Optional EVENT is currently not used, but hopefully will be soon." nil nil)

(autoload 'strokes-do-stroke "strokes" "\
Read a simple stroke from the user and then exectute its comand.
This must be bound to a mouse event." t nil)

(autoload 'strokes-do-complex-stroke "strokes" "\
Read a complex stroke from the user and then exectute its command.
This must be bound to a mouse event." t nil)

(autoload 'strokes-describe-stroke "strokes" "\
Displays the command which STROKE maps to, reading STROKE interactively." t nil)

(defalias 'describe-stroke 'strokes-describe-stroke)

(autoload 'strokes-help "strokes" "\
Get instructional help on using the the `strokes' package." t nil)

(autoload 'strokes-load-user-strokes "strokes" "\
Load user-defined strokes from file named by `strokes-file'." t nil)

(defalias 'load-user-strokes 'strokes-load-user-strokes)

(autoload 'strokes-list-strokes "strokes" "\
Pop up a buffer containing a listing of all strokes defined in STROKE-MAP.
If STROKE-MAP is not given, `strokes-global-map' will be used instead." t nil)

(defalias 'list-strokes 'strokes-list-strokes)

(autoload 'strokes-mode "strokes" "\
Toggle strokes being enabled.
With ARG, turn strokes on if and only if ARG is positive or true.
Note that `strokes-mode' is a global mode.  Think of it as a minor
mode in all buffers when activated.
By default, strokes are invoked with mouse button-2.  You can define
new strokes with

> M-x global-set-stroke" t nil)

;;;***

;;;### (autoloads (tcl-help-on-word inferior-tcl tcl-mode) "tcl" "modes/tcl.el")

(autoload 'tcl-mode "tcl" "\
Major mode for editing Tcl code.
Expression and list commands understand all Tcl brackets.
Tab indents for Tcl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  tcl-indent-level
    Indentation of Tcl statements within surrounding block.
  tcl-continued-indent-level
    Indentation of continuation line relative to first line of command.

Variables controlling user interaction with mode (see variable
documentation for details):
  tcl-tab-always-indent
    Controls action of TAB key.
  tcl-auto-newline
    Non-nil means automatically newline before and after braces, brackets,
    and semicolons inserted in Tcl code.
  tcl-electric-hash-style
    Controls action of `#' key.
  tcl-use-hairy-comment-detector
    If t, use more complicated, but slower, comment detector.
    This variable is only used in GNU Emacs 19.
  tcl-use-smart-word-finder
    If not nil, use a smarter, Tcl-specific way to find the current
    word when looking up help on a Tcl command.

Turning on Tcl mode calls the value of the variable `tcl-mode-hook'
with no args, if that value is non-nil.  Read the documentation for
`tcl-mode-hook' to see what kinds of interesting hook functions
already exist.

Commands:
\\{tcl-mode-map}" t nil)

(autoload 'inferior-tcl "tcl" "\
Run inferior Tcl process.
Prefix arg means enter program name interactively.
See documentation for function `inferior-tcl-mode' for more information." t nil)

(autoload 'tcl-help-on-word "tcl" "\
Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'." t nil)

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

;;;### (autoloads (verilog-mode) "verilog-mode" "modes/verilog-mode.el")

(autoload 'verilog-mode "verilog-mode" "\
Major mode for editing Verilog code. \\<verilog-mode-map>
NEWLINE, TAB indents for Verilog code.  
Delete converts tabs to spaces as it moves back.
Supports highlighting.

Variables controlling indentation/edit style:

 verilog-indent-level           (default 3)
    Indentation of Verilog statements with respect to containing block.
 verilog-indent-level-module    (default 3)
    Absolute indentation of Module level Verilog statements. 
    Set to 0 to get initial and always statements lined up 
    on the left side of your screen.
 verilog-indent-level-declaration    (default 3)
    Indentation of declarations with respect to containing block. 
    Set to 0 to get them list right under containing block.
 verilog-indent-level-behavorial    (default 3)
    Indentation of first begin in a task or function block
    Set to 0 to get such code to linedup underneath the task or function keyword
 verilog-cexp-indent            (default 1)
    Indentation of Verilog statements broken across lines.
 verilog-case-indent            (default 2)
    Indentation for case statements.
 verilog-auto-newline           (default nil)
    Non-nil means automatically newline after semicolons and the punctation 
    mark after an end.
 verilog-auto-indent-on-newline (default t)
    Non-nil means automatically indent line after newline
 verilog-tab-always-indent      (default t)
    Non-nil means TAB in Verilog mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 verilog-indent-begin-after-if  (default t)
    Non-nil means to indent begin statements following a preceding
    if, else, while, for and repeat statements, if any. otherwise,
    the begin is lined up with the preceding token. If t, you get:
      if (a)
         begin
    otherwise you get:
      if (a)
      begin
 verilog-auto-endcomments       (default t)
    Non-nil means a comment /* ... */ is set after the ends which ends 
      cases, tasks, functions and modules.
    The type and name of the object will be set between the braces.
 verilog-minimum-comment-distance (default 40)
    Minimum distance between begin and end required before a comment
    will be inserted.  Setting this variable to zero results in every
    end aquiring a comment; the default avoids too many redundanet
    comments in tight quarters. 
 verilog-auto-lineup            (default `(all))
    List of contexts where auto lineup of :'s or ='s should be done.

Turning on Verilog mode calls the value of the variable verilog-mode-hook with
no args, if that value is non-nil.
Other useful functions are:
\\[verilog-complete-word]	-complete word with appropriate possibilities 
   (functions, verilog keywords...)
\\[verilog-comment-region]	- Put marked area in a comment, fixing 
   nested comments.
\\[verilog-uncomment-region]	- Uncomment an area commented with \\[verilog-comment-region].
\\[verilog-insert-block]	- insert begin ... end;
\\[verilog-star-comment]	- insert /* ... */
\\[verilog-mark-defun]	- Mark function.
\\[verilog-beg-of-defun]	- Move to beginning of current function.
\\[verilog-end-of-defun]	- Move to end of current function.
\\[verilog-label-be]	- Label matching begin ... end, fork ... join 
  and case ... endcase statements;
" t nil)

;;;***

;;;### (autoloads (vhdl-mode) "vhdl-mode" "modes/vhdl-mode.el")

(autoload 'vhdl-mode "vhdl-mode" "\
Major mode for editing VHDL code.
vhdl-mode $Revision: 1.2 $
To submit a problem report, enter `\\[vhdl-submit-bug-report]' from a
vhdl-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

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

;;;### (autoloads (vrml-mode) "vrml-mode" "modes/vrml-mode.el")

(autoload 'vrml-mode "vrml-mode" "\
Major mode for editing VRML code.
Expression and list commands understand all VRML brackets.
Tab indents for VRML code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  vrml-indent-level
    Indentation of VRML statements within surrounding block.

Variables controlling user interaction with mode (see variable
documentation for details):
  vrml-tab-always-indent
    Controls action of TAB key.
  vrml-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in VRML code.

Turning on VRML mode calls the value of the variable `vrml-mode-hook'
with no args, if that value is non-nil.  Read the documentation for
`vrml-mode-hook' to see what kinds of interesting hook functions
already exist.

Commands:
\\{vrml-mode-map}" t nil)

;;;***

;;;### (autoloads (winmgr-mode) "winmgr-mode" "modes/winmgr-mode.el")

(autoload 'winmgr-mode "winmgr-mode" "\
Major mode for editing winmgr config files." t nil)

;;;***

;;;### (autoloads (xpm-mode) "xpm-mode" "modes/xpm-mode.el")

(autoload 'xpm-mode "xpm-mode" "\
Treat the current buffer as an xpm file and colorize it.

  Shift-button-1 lets you paint by dragging the mouse.  Shift-button-1 on a
color definition line will change the current painting color to that line's
value.

  Characters inserted from the keyboard will NOT be colored properly yet.
Use the mouse, or do xpm-init (\\[xpm-init]) after making changes.

\\[xpm-add-color] Add a new color, prompting for character and value
\\[xpm-show-image] show the current image at the top of the buffer
\\[xpm-parse-color] parse the current line's color definition and add
   it to the color table.  Provided as a means of changing colors.
XPM minor mode bindings:
\\{xpm-mode-map}" t nil)

;;;***

;;;### (autoloads (xrdb-mode) "xrdb-mode" "modes/xrdb-mode.el")

(autoload 'xrdb-mode "xrdb-mode" "\
Major mode for editing xrdb config files" t nil)

;;;***

(provide 'modes-autoloads)
))
