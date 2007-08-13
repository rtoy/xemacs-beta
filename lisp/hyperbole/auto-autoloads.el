;;; DO NOT MODIFY THIS FILE
(if (featurep '-autoloads) (error "Already loaded"))

;;;### (autoloads (hmail:compose) "hmail" "hyperbole/hmail.el")

(autoload 'hmail:compose "hmail" "\
Compose mail with ADDRESS and evaluation of EXPR.
Optional SUBJECT and HELP message may also be given." t nil)

;;;***

;;;### (autoloads (Info-handle-in-note smart-info-assist smart-info) "hmous-info" "hyperbole/hmous-info.el")

(autoload 'smart-info "hmous-info" "\
Walks through Info documentation networks using one key or mouse key.

If key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the desired node is found;
 (3) the File entry of a Node Header (first line),       
       the 'Top' node within that file is found;
 (4) at the end of the current node, the Next node is found (this will
       descend subtrees if the function 'Info-global-next' is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled up one windowful.

Returns t if key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil." t nil)

(autoload 'smart-info-assist "hmous-info" "\
Walks through Info documentation networks using one assist-key or mouse assist-key.

If assist-key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (3) the File entry of a Node Header (first line),       
       the 'DIR' root-level node is found;
 (4) at the end of the current node, the Previous node is found (this will
       return from subtrees if the function 'Info-global-prev is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled down one windowful.

Returns t if assist-key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil." t nil)

(autoload 'Info-handle-in-note "hmous-info" "\
Follows an Info cross-reference.
If point is within the first line of an Info note (cross-reference), follows
cross-reference and returns t; otherwise returns nil." nil nil)

;;;***

;;;### (autoloads (hkey-help-show) "hmouse-drv" "hyperbole/hmouse-drv.el")

(autoload 'hkey-help-show "hmouse-drv" "\
Saves prior frame configuration if BUFFER displays help.  Displays BUFFER.

Optional second arg CURRENT-WINDOW non-nil forces display of buffer within
the current window.  By default, it is displayed in another window." nil nil)

;;;***

;;;### (autoloads (smart-tags-file smart-tags-file-path smart-objc smart-lisp-mode-p smart-java-at-tag-p smart-java smart-fortran-at-tag-p smart-c++ smart-c-at-tag-p smart-asm-at-tag-p) "hmouse-tag" "hyperbole/hmouse-tag.el")

(autoload 'smart-asm-at-tag-p "hmouse-tag" "\
Return assembly tag name that point is within, else nil." nil nil)

(autoload 'smart-c-at-tag-p "hmouse-tag" "\
Return C tag name that point is within, else nil." nil nil)

(autoload 'smart-c++ "hmouse-tag" "\
Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed." t nil)

(autoload 'smart-fortran-at-tag-p "hmouse-tag" "\
Return Fortran tag name that point is within, else nil." nil nil)

(autoload 'smart-java "hmouse-tag" "\
Jumps to the definition of optional Java IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Java tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-dirs'.
 (3) on an Java identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories." t nil)

(autoload 'smart-java-at-tag-p "hmouse-tag" "\
Return Java tag name that point is within, else nil." nil nil)

(autoload 'smart-lisp-mode-p "hmouse-tag" "\
Return t if in a mode which uses Lisp symbols." nil nil)

(autoload 'smart-objc "hmouse-tag" "\
Jumps to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed." t nil)

(autoload 'smart-tags-file-path "hmouse-tag" "\
Expand relative FILE name by looking it up in the nearest tags file.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file." nil nil)

(autoload 'smart-tags-file "hmouse-tag" "\
Return appropriate tags file name for CURR-FILENAME or `tags-file-name'.
Optional NAME-OF-TAGS-FILE is the literal filename for which to look." nil nil)

;;;***

;;;### (autoloads (hyperbole) "hui-mini" "hyperbole/hui-mini.el")

(autoload 'hyperbole "hui-mini" "\
Invokes default Hyperbole menu user interface when not already active.
Suitable for binding to a key, e.g. {C-h h}.
Non-interactively, returns t if menu is actually invoked by call, else nil.

Two optional arguments may be given to invoke alternative menus.
MENU (a symbol) specifies the menu to invoke from MENU-LIST, (a
Hyperbole menu list structure).  MENU defaults to 'hyperbole and MENU-LIST
to `hui:menus'.  See `hui:menus' definition for the format of the menu list
structure." t nil)

;;;***

;;;### (autoloads (var:append) "hvar" "hyperbole/hvar.el")

(autoload 'var:append "hvar" "\
Appends to value held by VAR-SYMBOL-NAME, LIST-TO-ADD.  Returns new value.
If VAR-SYMBOL-NAME is unbound, it is set to LIST-TO-ADD.
Often used to append to 'hook' variables." nil nil)

;;;***

;;;### (autoloads (hypb:display-file-with-logo hypb:configuration) "hypb" "hyperbole/hypb.el")

(autoload 'hypb:configuration "hypb" "\
Insert Emacs configuration information at the end of optional OUT-BUF or the current buffer." nil nil)

(autoload 'hypb:display-file-with-logo "hypb" "\
Display an optional text FILE with the InfoDock Associates logo prepended.
Without file, logo is prepended to the current buffer." nil nil)

;;;***

;;;### (autoloads nil "hyperbole" "hyperbole/hyperbole.el")

(defvar action-key-url-function 'w3-fetch "\
Value is a function of one argument, a url, which displays the url referent.
Possible values are:
  w3-fetch - display using the W3 Emacs web browser;
  highlight-headers-follow-url-netscape - display in Netscape;
  highlight-headers-follow-url-mosaic - display in Mosaic.")

(defvar kimport:mode-alist '((t . kimport:text) (outline-mode . kimport:star-outline)) "\
Alist of (major-mode . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called if the major mode of the import file matches the car of an element in
this list.  If there is no match, then `kimport:suffix-alist' is checked.  If
that yields no match, the element in this list whose car is 't is used.  It
normally does an import of a koutline or text file.

Each importation-function must take two arguments, a buffer/file to import
and a buffer/file into which to insert the imported elements and a third
optional argument, CHILDREN-P, which when non-nil means insert imported cells
as the initial set of children of the current cell, if any.

   outline-mode  - imported as an Emacs outline whose entries begin with
                   asterisks; 
   .kot
   .kotl         - imported as a structured koutline

   all others    - imported as text.")

(defvar kimport:suffix-alist '(("\\.otl$" . kimport:star-outline) ("\\.aug$" . kimport:aug-post-outline)) "\
Alist of (buffer-name-suffix-regexp . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called.  Each importation-function must take two arguments, a buffer/file to
import and a buffer/file into which to insert the imported elements and a
third optional argument, CHILDREN-P, which when non-nil means insert imported
cells as the initial set of children of the current cell, if any.

   .otl  - imported as an Emacs outline whose entries begin with asterisks;
   .kot
   .kotl - imported as a structured koutline
   .aug  - imported as an Augment post-numbered outline.")

;;;***

;;;### (autoloads (wconfig-yank-pop wconfig-ring-save wconfig-delete-pop wconfig-restore-by-name wconfig-delete-by-name wconfig-add-by-name) "wconfig" "hyperbole/wconfig.el")

(autoload 'wconfig-add-by-name "wconfig" "\
Saves the current window configuration under the string NAME.
When called interactively and a window configuration already exists under
NAME, confirms whether or not to replace it." t nil)

(autoload 'wconfig-delete-by-name "wconfig" "\
Deletes window configuration saved under NAME." t nil)

(autoload 'wconfig-restore-by-name "wconfig" "\
Restores window configuration saved under NAME." t nil)

(autoload 'wconfig-delete-pop "wconfig" "\
Replaces current window config with most recently saved config in ring.
Then deletes this new configuration from the ring." t nil)

(autoload 'wconfig-ring-save "wconfig" "\
Saves the current window configuration onto the save ring.
Use {\\[wconfig-yank-pop]} to restore it at a later time." t nil)

(autoload 'wconfig-yank-pop "wconfig" "\
Replaces current window config with prefix arg Nth prior one in save ring.
Interactively, default value of N = 1, meaning the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the oldest
one comes the newest one." t nil)

;;;***

;;;### (autoloads (rolo-logic) "wrolo-logic" "hyperbole/wrolo-logic.el")

(autoload 'rolo-logic "wrolo-logic" "\
Apply FUNC to all entries in optional IN-BUFS, display entries where FUNC is non-nil.
If IN-BUFS is nil, 'rolo-file-list' is used.  If optional COUNT-ONLY is
non-nil, don't display entries, return count of matching entries only.  If
optional INCLUDE-SUB-ENTRIES flag is non-nil, FUNC will be applied across all
sub-entries at once.  Default is to apply FUNC to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries unless
INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT flag is non-nil.
FUNC should use the free variables 'start' and 'end' which contain the limits
of the region on which it should operate.  Returns number of applications of
FUNC that return non-nil." t nil)

;;;***

;;;### (autoloads (rolo-yank rolo-toggle-datestamps rolo-sort rolo-kill rolo-grep rolo-fgrep rolo-edit rolo-display-matches rolo-add) "wrolo" "hyperbole/wrolo.el")

(autoload 'rolo-add "wrolo" "\
Adds a new entry in personal rolodex for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string." t nil)

(autoload 'rolo-display-matches "wrolo" "\
Display optional DISPLAY-BUF buffer of previously found rolodex matches.
If DISPLAY-BUF is nil, use the value in `rolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within after the display." t nil)

(autoload 'rolo-edit "wrolo" "\
Edits a rolodex entry given by optional NAME within `rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
With no NAME arg, simply displays FILE or first entry in `rolo-file-list' in an
editable mode.  NAME may be of the form: parent/child to edit child below a
parent entry which begins with the parent string." t nil)

(autoload 'rolo-fgrep "wrolo" "\
Display rolodex entries matching STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
ROLO-FILE or rolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil
means don't retrieve and don't display matching entries.  Optional NO-DISPLAY
non-nil means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list." t nil)

(autoload 'rolo-grep "wrolo" "\
Display rolodex entries matching REGEXP.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from optional ROLO-BUFS or
rolo-file-list.  Default is to find all matching entries.  Each entry is
displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional NO-DISPLAY non-nil
means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list." t nil)

(autoload 'rolo-kill "wrolo" "\
Kills a rolodex entry given by NAME within `rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Returns t if entry is killed, nil otherwise." t nil)

(autoload 'rolo-sort "wrolo" "\
Sorts up to 14 levels of entries in ROLO-FILE (default is personal rolo).
Assumes entries are delimited by one or more `*'characters.
Returns list of number of groupings at each entry level." t nil)

(autoload 'rolo-toggle-datestamps "wrolo" "\
Toggle whether datestamps are updated when rolodex entries are modified.
With optional ARG, turn them on iff ARG is positive." t nil)

(autoload 'rolo-yank "wrolo" "\
Inserts at point the first rolodex entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string." t nil)

;;;***

;;;### (autoloads (kfile:is-p kfile:view kfile:find) "kfile" "kotl/kfile.el")

(autoload 'kfile:find "kfile" "\
Find a file FILE-NAME containing a kotl or create one if none exists.
Return the new kview." t nil)

(autoload 'kfile:view "kfile" "\
View an existing kotl version-2 file FILE-NAME in a read-only mode." t nil)

(autoload 'kfile:is-p "kfile" "\
Iff current buffer contains an unformatted or formatted koutline, return file format version string, else nil." nil nil)

;;;***

;;;### (autoloads (kimport:text kimport:star-outline kimport:aug-post-outline kimport:file) "kimport" "kotl/kimport.el")

(autoload 'kimport:file "kimport" "\
Import a buffer or file IMPORT-FROM into the koutline in buffer or file OUTPUT-TO.

Any suffix in IMPORT-FROM's buffer name is used to determine the type of
importation.  All others are imported as text, one paragraph per cell.

See the documentation for the variable, `kimport:suffix-alist' for
information on specific importation formats." t nil)

(autoload 'kimport:aug-post-outline "kimport" "\
Insert Augment outline statements from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

If OUTPUT-TO is a new koutline, the first statement inserted will be the
first cell.  Otherwise, it will be the successor of the current cell.

Each statement to be imported is delimited by an Augment relative id at the
end of the statement.  \"1\" = level 1, \"1a\" = level 2 in outline and so
on." t nil)

(autoload 'kimport:star-outline "kimport" "\
Insert star outline nodes from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

\"* \" = level 1, \"** \" = level 2 in outline and so on." t nil)

(autoload 'kimport:text "kimport" "\
Insert text paragraphs from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

Text paragraphs are imported as a sequence of same level cells.  Koutlines
are imported with their structure intact.

The variable, `paragraph-start,' is used to determine paragraphs." t nil)

;;;***

;;;### (autoloads (klink:create) "klink" "kotl/klink.el")

(autoload 'klink:create "klink" "\
Insert at point an implicit link to REFERENCE.
REFERENCE should be a cell-ref or a string containing \"filename, cell-ref\".
See documentation for `kcell:ref-to-id' for valid cell-ref formats." t nil)

;;;***

;;;### (autoloads (kotl-mode) "kotl-mode" "kotl/kotl-mode.el")

(autoload 'kotl-mode "kotl-mode" "\
The major mode used to edit and view koutlines.
It provides the following keys:
\\{kotl-mode-map}" t nil)

;;;***

(provide '-autoloads)
