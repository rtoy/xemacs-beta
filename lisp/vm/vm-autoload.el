(provide 'vm-autoload)

(autoload (quote vm-delete-message) "vm-delete" "Add the `deleted' attribute to the current message.

The message will be physically deleted from the current folder the next
time the current folder is expunged.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are deleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are deleted, other messages are ignored." t nil)

(autoload (quote vm-delete-message-backward) "vm-delete" "Like vm-delete-message, except the deletion direction is reversed." t nil)

(autoload (quote vm-undelete-message) "vm-delete" "Remove the `deleted' attribute from the current message.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are undeleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are undeleted, other messages are ignored." t nil)

(autoload (quote vm-kill-subject) "vm-delete" "Delete all messages with the same subject as the current message.
Message subjects are compared after ignoring parts matched by
the variables vm-subject-ignored-prefix and vm-subject-ignored-suffix.

The optional prefix argument ARG specifies the direction to move
if vm-move-after-killing is non-nil.  The default direction is
forward.  A positive prefix argument means move forward, a
negative arugment means move backward, a zero argument means
don't move at all." t nil)

(autoload (quote vm-expunge-folder) "vm-delete" "Expunge messages with the `deleted' attribute.
For normal folders this means that the deleted messages are
removed from the message list and the message contents are
removed from the folder buffer.

For virtual folders, messages are removed from the virtual
message list.  If virtual mirroring is in effect for the virtual
folder, the corresponding real messages are also removed from real
message lists and the message contents are removed from real folders.

When invoked on marked messages (via vm-next-command-uses-marks),
only messages both marked and deleted are expunged, other messages are
ignored." t nil)

(autoload (quote vm-no-frills-encapsulate-message) "vm-digest" "Encapsulate a message M for forwarding, simply.
No message encapsulation standard is used.  The message is
inserted at point in the current buffer, surrounded by two dashed
start/end separator lines.  Point is not moved.

M should be a message struct for a real message, not a virtual message.
This is the message that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used." nil nil)

(autoload (quote vm-mime-encapsulate-messages) "vm-digest" "Encapsulate the messages in MESSAGE-LIST as per the MIME spec.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used.

Returns the multipart boundary parameter (string) that should be used
in the Content-Type header." nil nil)

(autoload (quote vm-mime-burst-message) "vm-digest" "Burst messages from the digest message M.
M should be a message struct for a real message.
MIME encoding is expected.  The message content type
must be either message/* or multipart/digest." nil nil)

(autoload (quote vm-mime-burst-layout) "vm-digest" nil nil nil)

(autoload (quote vm-rfc934-char-stuff-region) "vm-digest" "Quote RFC 934 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines beginning with `-' in the region have `- ' prepended to them." nil nil)

(autoload (quote vm-rfc934-char-unstuff-region) "vm-digest" "Unquote lines in between START and END as per RFC 934.
START and END are buffer positions in the current buffer.
Lines beginning with `- ' in the region have that string stripped
from them." nil nil)

(autoload (quote vm-rfc934-encapsulate-messages) "vm-digest" "Encapsulate the messages in MESSAGE-LIST as per RFC 934.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used." nil nil)

(autoload (quote vm-rfc1153-char-stuff-region) "vm-digest" "Quote RFC 1153 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines consisting only of 30 hyphens have the first hyphen
converted to a space." nil nil)

(autoload (quote vm-rfc1153-char-unstuff-region) "vm-digest" "Unquote lines in between START and END as per RFC 1153.
START and END are buffer positions in the current buffer.
Lines consisting only of a space following by 29 hyphens have the space
converted to a hyphen." nil nil)

(autoload (quote vm-rfc1153-encapsulate-messages) "vm-digest" "Encapsulate the messages in MESSAGE-LIST as per RFC 1153.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used." nil nil)

(autoload (quote vm-rfc1153-or-rfc934-burst-message) "vm-digest" "Burst messages from the digest message M.
M should be a message struct for a real message.
If RFC1153 is non-nil, assume the digest is of the form specified by
RFC 1153.  Otherwise assume RFC 934 digests." nil nil)

(autoload (quote vm-rfc934-burst-message) "vm-digest" "Burst messages from the RFC 934 digest message M.
M should be a message struct for a real message." nil nil)

(autoload (quote vm-rfc1153-burst-message) "vm-digest" "Burst messages from the RFC 1153 digest message M.
M should be a message struct for a real message." nil nil)

(autoload (quote vm-burst-digest) "vm-digest" "Burst the current message (a digest) into its individual messages.
The digest's messages are assimilated into the folder as new mail
would be.

Optional argument DIGEST-TYPE is a string that tells VM what kind
of digest the current message is.  If it is not given the value
defaults to the value of vm-digest-burst-type.  When called
interactively DIGEST-TYPE will be read from the minibuffer.

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be burst." t nil)

(autoload (quote vm-burst-rfc934-digest) "vm-digest" "Burst an RFC 934 style digest" t nil)

(autoload (quote vm-burst-rfc1153-digest) "vm-digest" "Burst an RFC 1153 style digest" t nil)

(autoload (quote vm-burst-mime-digest) "vm-digest" "Burst a MIME digest" t nil)

(autoload (quote vm-guess-digest-type) "vm-digest" "Guess the digest type of the message M.
M should be the message struct of a real message.
Returns either \"rfc934\", \"rfc1153\" or \"mime\"." nil nil)

(autoload (quote vm-digest-get-header-contents) "vm-digest" nil nil nil)

(autoload (quote vm-easy-menu-define) "vm-easymenu" "Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style 
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above." nil t)

(autoload (quote vm-easy-menu-do-define) "vm-easymenu" nil nil nil)

(autoload (quote vm-easy-menu-create-keymaps) "vm-easymenu" nil nil nil)

(autoload (quote vm-easy-menu-change) "vm-easymenu" "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of menu items, as in `vm-easy-menu-define'.
These items entirely replace the previous items in that map.

Call this from `activate-menubar-hook' to implement dynamic menus." nil nil)

(autoload (quote vm-easy-menu-remove) "vm-easymenu" nil nil nil)

(autoload (quote vm-easy-menu-add) "vm-easymenu" nil nil nil)

(autoload (quote vm-edit-message) "vm-edit" "Edit the current message.  Prefix arg means mark as unedited instead.
If editing, the current message is copied into a temporary buffer, and
this buffer is selected for editing.  The major mode of this buffer is
controlled by the variable vm-edit-message-mode.  The hooks specified
in vm-edit-message-hook are run just prior to returning control to the user
for editing.

Use C-c ESC when you have finished editing the message.  The message
will be inserted into its folder replacing the old version of the
message.  If you don't want your edited version of the message to
replace the original, use C-c C-] and the edit will be aborted." t nil)

(autoload (quote vm-edit-message-other-frame) "vm-edit" "Like vm-edit-message, but run in a newly created frame." t nil)

(autoload (quote vm-discard-cached-data) "vm-edit" "Discard cached information about the current message.
When VM gathers information from the headers of a message, it stores it
internally for future reference.  This command causes VM to forget this
information, and VM will be forced to search the headers of the message
again for these data.  VM will also have to decide again which headers
should be displayed and which should not.  Therefore this command is
useful if you change the value of vm-visible-headers or
vm-invisible-header-regexp in the midst of a VM session.

Numeric prefix argument N means to discard data from the current message
plus the next N-1 messages.  A negative N means discard data from the
current message and the previous N-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
data is discarded only from the marked messages in the current folder." t nil)

(autoload (quote vm-edit-message-end) "vm-edit" "End the edit of a message and copy the result to its folder." t nil)

(autoload (quote vm-edit-message-abort) "vm-edit" "Abort the edit of a message, forgetting changes to the message." t nil)

(autoload (quote vm-number-messages) "vm-folder" "Set the number-of and padded-number-of slots of messages
in vm-message-list.

If non-nil, START-POINT should point to a cons cell in
vm-message-list and the numbering will begin there, else the
numbering will begin at the head of vm-message-list.  If
START-POINT is non-nil the reverse-link-of slot of the message in
the cons must be valid and the message pointed to (if any) must
have a non-nil number-of slot, because it is used to determine
what the starting message number should be.

If non-nil, END-POINT should point to a cons cell in
vm-message-list and the numbering will end with the message just
before this cell.  A nil value means numbering will be done until
the end of vm-message-list is reached." nil nil)

(autoload (quote vm-set-numbering-redo-start-point) "vm-folder" "Set vm-numbering-redo-start-point to START-POINT if appropriate.
Also mark the current buffer as needing a display update.

START-POINT should be a cons in vm-message-list or just t.
 (t means start from the beginning of vm-message-list.)
If START-POINT is closer to the head of vm-message-list than
vm-numbering-redo-start-point or is equal to t, then
vm-numbering-redo-start-point is set to match it." nil nil)

(autoload (quote vm-set-numbering-redo-end-point) "vm-folder" "Set vm-numbering-redo-end-point to END-POINT if appropriate.
Also mark the current buffer as needing a display update.

END-POINT should be a cons in vm-message-list or just t.
 (t means number all the way to the end of vm-message-list.)
If END-POINT is closer to the end of vm-message-list or is equal
to t, then vm-numbering-redo-start-point is set to match it.
The number-of slot is used to determine proximity to the end of
vm-message-list, so this slot must be valid in END-POINT's message
and the message in the cons pointed to by vm-numbering-redo-end-point." nil nil)

(autoload (quote vm-do-needed-renumbering) "vm-folder" "Number messages in vm-message-list as specified by
vm-numbering-redo-start-point and vm-numbering-redo-end-point.

vm-numbering-redo-start-point = t means start at the head
of vm-message-list.
vm-numbering-redo-end-point = t means number all the way to the
end of vm-message-list.

Otherwise the variables' values should be conses in vm-message-list
or nil." nil nil)

(autoload (quote vm-set-summary-redo-start-point) "vm-folder" "Set vm-summary-redo-start-point to START-POINT if appropriate.
Also mark the current buffer as needing a display update.

START-POINT should be a cons in vm-message-list or just t.
 (t means start from the beginning of vm-message-list.)
If START-POINT is closer to the head of vm-message-list than
vm-summary-redo-start-point or is equal to t, then
vm-summary-redo-start-point is set to match it." nil nil)

(autoload (quote vm-mark-for-summary-update) "vm-folder" "Mark message M for a summary update.
Also mark M's buffer as needing a display update. Any virtual
messages of M and their buffers are similarly marked for update.
If M is a virtual message and virtual mirroring is in effect for
M (i.e. attribute-of eq attributes-of M's real message), M's real
message and its buffer are scheduled for an update.

Optional arg DONT-KILL-CACHE non-nil means don't invalidate the
summary-of slot for any messages marked for update.  This is
meant to be used by functions that update message information
that is not cached in the summary-of slot, e.g. message numbers
and thread indentation." nil nil)

(autoload (quote vm-force-mode-line-update) "vm-folder" "Force a mode line update in all frames." nil nil)

(autoload (quote vm-do-needed-mode-line-update) "vm-folder" "Do a modeline update for the current folder buffer.
This means setting up all the various vm-ml attribute variables
in the folder buffer and copying necessary variables to the
folder buffer's summary and presentation buffers, and then
forcing Emacs to update all modelines.

If a virtual folder being updated has no messages, then
erase-buffer is called on its buffer.

If any type of folder is empty, erase-buffer is called
on its presentation buffer, if any." nil nil)

(autoload (quote vm-update-summary-and-mode-line) "vm-folder" "Update summary and mode line for all VM folder and summary buffers.
Really this updates all the visible status indicators.

Message lists are renumbered.
Summary entries are wiped and regenerated.
Mode lines are updated.
Toolbars are updated." nil nil)

(autoload (quote vm-reverse-link-messages) "vm-folder" "Set reverse links for all messages in vm-message-list." nil nil)

(autoload (quote vm-match-ordered-header) "vm-folder" "Try to match a header in ALIST and return the matching cell.
This is used by header ordering code.

ALIST looks like this ((\"From\") (\"To\")).  This function returns
the alist element whose car matches the header starting at point.
The header ordering code uses the cdr of the element
returned to hold headers to be output later." nil nil)

(autoload (quote vm-match-header) "vm-folder" "Match a header and save some state information about the matched header.
Optional first arg HEADER-NAME means match the header only
if it matches HEADER-NAME.  HEADER-NAME should be a string
containing a header name.  The string should end with a colon if just
that name should be matched.  A string that does not end in a colon
will match all headers that begin with that string.

State information is stored in vm-matched-header-vector bound to a vector
of this form.

 [ header-start header-end
   header-name-start header-name-end
   header-contents-start header-contents-end ]

Elements are integers.
There are functions to access and use this info." nil nil)

(autoload (quote vm-matched-header) "vm-folder" "Returns the header last matched by vm-match-header.
Trailing newline is included." nil nil)

(autoload (quote vm-matched-header-name) "vm-folder" "Returns the name of the header last matched by vm-match-header." nil nil)

(autoload (quote vm-matched-header-contents) "vm-folder" "Returns the contents of the header last matched by vm-match-header.
Trailing newline is not included." nil nil)

(autoload (quote vm-matched-header-start) "vm-folder" "Returns the start position of the header last matched by vm-match-header." nil nil)

(autoload (quote vm-matched-header-end) "vm-folder" "Returns the end position of the header last matched by vm-match-header." nil nil)

(autoload (quote vm-matched-header-name-start) "vm-folder" "Returns the start position of the name of the header last matched
by vm-match-header." nil nil)

(autoload (quote vm-matched-header-name-end) "vm-folder" "Returns the end position of the name of the header last matched
by vm-match-header." nil nil)

(autoload (quote vm-matched-header-contents-start) "vm-folder" "Returns the start position of the contents of the header last matched
by vm-match-header." nil nil)

(autoload (quote vm-matched-header-contents-end) "vm-folder" "Returns the end position of the contents of the header last matched
by vm-match-header." nil nil)

(autoload (quote vm-get-folder-type) "vm-folder" "Return a symbol indicating the folder type of the current buffer.
This function works by examining the beginning of a folder.
If optional arg FILE is present the type of FILE is returned instead.
If optional second and third arg START and END are provided,
vm-get-folder-type will examine the text between those buffer
positions.  START and END default to 1 and (buffer-size) + 1.

Returns
  nil      if folder has no type (empty)
  unknown  if the type is not known to VM
  mmdf     for MMDF folders
  babyl    for BABYL folders
  From_    for UNIX From_ folders

If vm-trust-From_-with-Content-Length is non-nil,
From_-with-Content-Length is returned if the first message in the
folder has a Content-Length header and the folder otherwise looks
like a From_ folder." nil nil)

(autoload (quote vm-convert-folder-type) "vm-folder" "Convert buffer from OLD-TYPE to NEW-TYPE.
OLD-TYPE and NEW-TYPE should be symbols returned from vm-get-folder-type.
This should be called on non-live buffers like crash boxes.
This will confuse VM if called on a folder buffer in vm-mode." nil nil)

(autoload (quote vm-convert-folder-header) "vm-folder" "Convert the folder header form OLD-TYPE to NEW-TYPE.
The folder header is the text at the beginning of a folder that
is a legal part of the folder but is not part of the first
message.  This is for dealing with BABYL files." nil nil)

(autoload (quote vm-skip-past-folder-header) "vm-folder" "Move point past the folder header.
The folder header is the text at the beginning of a folder that
is a legal part of the folder but is not part of the first
message.  This is for dealing with BABYL files." nil nil)

(autoload (quote vm-convert-folder-type-headers) "vm-folder" "Convert headers in the message around point from OLD-TYPE to NEW-TYPE.
This means to add/delete Content-Length and any other
headers related to folder-type as needed for folder type
conversions.  This function expects point to be at the beginning
of the header section of a message, and it only deals with that
message." nil nil)

(autoload (quote vm-munge-message-separators) "vm-folder" "Munge message separators of FOLDER-TYPE found between START and END.
This function is used to eliminate message separators for a particular
folder type that happen to occur in a message.  \">\" is prepended to such
separators." nil nil)

(autoload (quote vm-compatible-folder-p) "vm-folder" "Return non-nil if FILE is a compatible folder with the current buffer.
The current folder must have vm-folder-type initialized.
FILE is compatible if
  - it is empty
  - the current folder is empty
  - the two folder types are equal" nil nil)

(autoload (quote vm-leading-message-separator) "vm-folder" "Returns a leading message separator for the current folder.
Defaults to returning a separator for the current folder type.

Optional first arg FOLDER-TYPE means return a separator for that
folder type instead.

Optional second arg MESSAGE should be a message struct.  This is used
generating BABYL separators, because they contain message attributes
and labels that must must be copied from the message.

Optional third arg FOR-OTHER-FOLDER non-nil means that this separator will
be used a `foreign' folder.  This means that the `deleted'
attributes should not be copied for BABYL folders." nil nil)

(autoload (quote vm-trailing-message-separator) "vm-folder" "Returns a leading message separator for the current folder.
Defaults to returning a separator for the current folder type.

Optional first arg FOLDER-TYPE means return a separator for that
folder type instead." nil nil)

(autoload (quote vm-folder-header) "vm-folder" "Returns a folder header for the current folder.
Defaults to returning a folder header for the current folder type.

Optional first arg FOLDER-TYPE means return a folder header for that
folder type instead.

Optional second arg LABEL-OBARRAY should be an obarray of labels
that have been used in this folder.  This is used for BABYL folders." nil nil)

(autoload (quote vm-find-leading-message-separator) "vm-folder" "Find the next leading message separator in a folder.
Returns non-nil if the separator is found, nil otherwise." nil nil)

(autoload (quote vm-find-trailing-message-separator) "vm-folder" "Find the next trailing message separator in a folder." nil nil)

(autoload (quote vm-skip-past-leading-message-separator) "vm-folder" "Move point past a leading message separator at point." nil nil)

(autoload (quote vm-skip-past-trailing-message-separator) "vm-folder" "Move point past a trailing message separator at point." nil nil)

(autoload (quote vm-build-message-list) "vm-folder" "Build a chain of message structures, stored them in vm-message-list.
Finds the start and end of each message and fills in the relevant
fields in the message structures.

Also finds the beginning of the header section and the end of the
text section and fills in these fields in the message structures.

vm-text-of and vm-vheaders-of field don't get filled until they
are needed.

If vm-message-list already contained messages, the end of the last
known message is found and then the parsing of new messages begins
there and the message are appended to vm-message-list.

vm-folder-type is initialized here." nil nil)

(autoload (quote vm-build-header-order-alist) "vm-folder" nil nil nil)

(autoload (quote vm-reorder-message-headers) "vm-folder" nil nil nil)

(autoload (quote vm-read-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-read-babyl-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-set-default-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-emit-totals-blurb) "vm-folder" nil nil nil)

(autoload (quote vm-convert-v4-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-labels) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-bookmark) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-visible-header-variables) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-message-order) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-summary) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-folder-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-babyl-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-babyl-attributes-string) "vm-folder" nil nil nil)

(autoload (quote vm-babyl-labels-string) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-virtual-attributes) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-labels) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-bookmark) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-summary) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-header-variables) "vm-folder" nil nil nil)

(autoload (quote vm-stuff-message-order) "vm-folder" nil nil nil)

(autoload (quote vm-remove-message-order) "vm-folder" nil nil nil)

(autoload (quote vm-change-all-new-to-unread) "vm-folder" nil nil nil)

(autoload (quote vm-unread-message) "vm-folder" "Set the `unread' attribute for the current message.  If the message is
already new or unread, then it is left unchanged.

Numeric prefix argument N means to unread the current message plus the
next N-1 messages.  A negative N means unread the current message and
the previous N-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages are affected, other messages are ignored." t nil)

(autoload (quote vm-quit-just-bury) "vm-folder" "Bury the current VM folder and summary buffers.
The folder is not altered and Emacs is still visiting it.  You
can switch back to it with switch-to-buffer or by using the
Buffer Menu." t nil)

(autoload (quote vm-quit-just-iconify) "vm-folder" "Iconify the frame and bury the current VM folder and summary buffers.
The folder is not altered and Emacs is still visiting it." t nil)

(autoload (quote vm-quit-no-change) "vm-folder" "Quit visiting the current folder without saving changes made to the folder." t nil)

(autoload (quote vm-quit) "vm-folder" "Quit visiting the current folder, saving changes.  Deleted messages are not expunged." t nil)

(autoload (quote vm-start-itimers-if-needed) "vm-folder" nil nil nil)

(autoload (quote vm-check-mail-itimer-function) "vm-folder" nil nil nil)

(autoload (quote vm-get-mail-itimer-function) "vm-folder" nil nil nil)

(autoload (quote vm-flush-itimer-function) "vm-folder" nil nil nil)

(autoload (quote vm-flush-cached-data) "vm-folder" nil nil nil)

(autoload (quote vm-write-file-hook) "vm-folder" nil nil nil)

(autoload (quote vm-save-buffer) "vm-folder" nil t nil)

(autoload (quote vm-write-file) "vm-folder" nil t nil)

(autoload (quote vm-save-folder) "vm-folder" "Save current folder to disk.
Deleted messages are not expunged.
Prefix arg is handled the same as for the command save-buffer.

When applied to a virtual folder, this command runs itself on
each of the underlying real folders associated with the virtual
folder." t nil)

(autoload (quote vm-save-and-expunge-folder) "vm-folder" "Expunge folder, then save it to disk.
Prefix arg is handled the same as for the command save-buffer.
Expunge won't be done if folder is read-only.

When applied to a virtual folder, this command works as if you had
run vm-expunge-folder followed by vm-save-folder." t nil)

(autoload (quote vm-handle-file-recovery-or-reversion) "vm-folder" nil nil nil)

(autoload (quote vm-handle-file-recovery) "vm-folder" nil nil nil)

(autoload (quote vm-handle-file-reversion) "vm-folder" nil nil nil)

(autoload (quote vm-after-revert-buffer-hook) "vm-folder" nil nil nil)

(autoload (quote vm-help) "vm-folder" "Display help for various VM activities." t nil)

(autoload (quote vm-spool-move-mail) "vm-folder" nil nil nil)

(autoload (quote vm-gobble-crash-box) "vm-folder" nil nil nil)

(autoload (quote vm-compute-spool-files) "vm-folder" nil nil nil)

(autoload (quote vm-spool-check-mail) "vm-folder" nil nil nil)

(autoload (quote vm-check-for-spooled-mail) "vm-folder" nil nil nil)

(autoload (quote vm-get-spooled-mail) "vm-folder" nil nil nil)

(autoload (quote vm-safe-popdrop-string) "vm-folder" nil nil nil)

(autoload (quote vm-get-new-mail) "vm-folder" "Move any new mail that has arrived in any of the spool files for the
current folder into the folder.  New mail is appended to the disk
and buffer copies of the folder.

Prefix arg means to gather mail from a user specified folder, instead of
the usual spool files.  The file name will be read from the minibuffer.
Unlike when getting mail from a spool file, the source file is left
undisturbed after its messages have been copied.

When applied to a virtual folder, this command runs itself on
each of the underlying real folders associated with this virtual folder.
A prefix argument has no effect; mail is always gathered from the
spool files." t nil)

(autoload (quote vm-assimilate-new-messages) "vm-folder" nil nil nil)

(autoload (quote vm-select-marked-or-prefixed-messages) "vm-folder" nil nil nil)

(autoload (quote vm-display-startup-message) "vm-folder" nil nil nil)

(autoload (quote vm-load-init-file) "vm-folder" nil t nil)

(autoload (quote vm-session-initialization) "vm-folder" nil nil nil)

(autoload (quote vm-toggle-read-only) "vm-folder" nil t nil)

(autoload (quote vm-mode-internal) "vm-folder" nil nil nil)

(autoload (quote vm-link-to-virtual-buffers) "vm-folder" nil nil nil)

(autoload (quote vm-change-folder-type) "vm-folder" "Change folder type to TYPE.
TYPE may be one of the following symbol values:

    From_
    From_-with-Content-Length
    mmdf
    babyl

Interactively TYPE will be read from the minibuffer." t nil)

(autoload (quote vm-garbage-collect-folder) "vm-folder" nil nil nil)

(autoload (quote vm-garbage-collect-message) "vm-folder" nil nil nil)

(autoload (quote vm-show-copying-restrictions) "vm-license" nil t nil)

(autoload (quote vm-show-no-warranty) "vm-license" "Display \"NO WARRANTY\" section of the GNU General Public License." t nil)

(autoload (quote vm-clear-all-marks) "vm-mark" "Removes all message marks in the current folder." t nil)

(autoload (quote vm-mark-all-messages) "vm-mark" "Mark all messages in the current folder." t nil)

(autoload (quote vm-mark-message) "vm-mark" "Mark the current message.
Numeric prefix argument N means mark the current message and the next
N-1 messages.  A negative N means mark the current message and the
previous N-1 messages." t nil)

(autoload (quote vm-unmark-message) "vm-mark" "Remove the mark from the current message.
Numeric prefix argument N means unmark the current message and the next
N-1 messages.  A negative N means unmark the current message and the
previous N-1 messages." t nil)

(autoload (quote vm-mark-summary-region) "vm-mark" "Mark all messages with summary lines contained in the region." t nil)

(autoload (quote vm-unmark-summary-region) "vm-mark" "Remove marks from messages with summary lines contained in the region." t nil)

(autoload (quote vm-mark-or-unmark-summary-region) "vm-mark" nil nil nil)

(autoload (quote vm-mark-or-unmark-messages-with-selector) "vm-mark" nil nil nil)

(autoload (quote vm-mark-matching-messages) "vm-mark" "Mark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information." t nil)

(autoload (quote vm-unmark-matching-messages) "vm-mark" "Unmark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information." t nil)

(autoload (quote vm-mark-thread-subtree) "vm-mark" "Mark all messages in the thread tree rooted at the current message." t nil)

(autoload (quote vm-unmark-thread-subtree) "vm-mark" "Unmark all messages in the thread tree rooted at the current message." t nil)

(autoload (quote vm-mark-or-unmark-thread-subtree) "vm-mark" nil nil nil)

(autoload (quote vm-mark-messages-same-subject) "vm-mark" "Mark all messages with the same subject as the current message." t nil)

(autoload (quote vm-unmark-messages-same-subject) "vm-mark" "Unmark all messages with the same subject as the current message." t nil)

(autoload (quote vm-mark-or-unmark-messages-same-subject) "vm-mark" nil nil nil)

(autoload (quote vm-mark-messages-same-author) "vm-mark" "Mark all messages with the same author as the current message." t nil)

(autoload (quote vm-unmark-messages-same-author) "vm-mark" "Unmark all messages with the same author as the current message." t nil)

(autoload (quote vm-mark-or-unmark-messages-same-author) "vm-mark" nil nil nil)

(autoload (quote vm-next-command-uses-marks) "vm-mark" "Does nothing except insure that the next VM command will operate only
on the marked messages in the current folder.  This only works for
commands bound to key, menu or button press events.  M-x vm-command will
not work." t nil)

(autoload (quote vm-marked-messages) "vm-mark" nil nil nil)

(autoload (quote vm-mark-help) "vm-mark" nil t nil)

(autoload (quote vm-menu-fsfemacs-menus-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-xemacs-menus-p) "vm-menu" nil nil nil)

(autoload (quote vm-fsfemacs-19-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-run-command) "vm-menu" "Run COMMAND almost interactively, with ARGS.
call-interactive can't be used unfortunately, but this-command is
set to the command name so that window configuration will be done." nil nil)

(autoload (quote vm-menu-can-revert-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-can-recover-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-can-save-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-can-get-new-mail-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-can-undo-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-can-decode-mime-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-yank-original) "vm-menu" nil t nil)

(autoload (quote vm-menu-can-send-mail-p) "vm-menu" nil nil nil)

(autoload (quote vm-menu-create-subject-virtual-folder) "vm-menu" nil t nil)

(autoload (quote vm-menu-create-author-virtual-folder) "vm-menu" nil t nil)

(autoload (quote vm-menu-xemacs-global-menubar) "vm-menu" nil nil nil)

(autoload (quote vm-menu-fsfemacs-global-menubar) "vm-menu" nil nil nil)

(autoload (quote vm-menu-initialize-vm-mode-menu-map) "vm-menu" nil nil nil)

(autoload (quote vm-menu-make-xemacs-menubar) "vm-menu" nil nil nil)

(autoload (quote vm-menu-popup-mode-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-popup-context-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-goto-event) "vm-menu" nil nil nil)

(autoload (quote vm-menu-popup-url-browser-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-popup-mime-dispose-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-popup-content-disposition-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-popup-fsfemacs-menu) "vm-menu" nil t nil)

(autoload (quote vm-menu-mode-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-set-menubar-dirty-flag) "vm-menu" nil nil nil)

(autoload (quote vm-menu-toggle-menubar) "vm-menu" nil t nil)

(autoload (quote vm-menu-install-menubar) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-menubar-item) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-vm-mode-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-mail-mode-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-menus) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-known-virtual-folders-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-install-visited-folders-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-hm-delete-folder) "vm-menu" "Query deletes a folder." t nil)

(autoload (quote vm-menu-hm-rename-folder) "vm-menu" "Rename a folder." t nil)

(autoload (quote vm-menu-hm-create-dir) "vm-menu" "Create a subdir in PARENT-DIR." t nil)

(autoload (quote vm-menu-hm-make-folder-menu) "vm-menu" "Makes a menu with the mail folders of the directory `vm-folder-directory'." t nil)

(autoload (quote vm-menu-hm-install-menu) "vm-menu" nil nil nil)

(autoload (quote vm-menu-hm-tree-ls-in-temp-buffer) "vm-menu" "List the directory DIR in the TEMP-BUFFER." nil nil)

(autoload (quote vm-menu-hm-tree-make-file-list-1) "vm-menu" nil nil nil)

(autoload (quote vm-menu-hm-tree-menu-file-truename) "vm-menu" nil nil nil)

(autoload (quote vm-menu-hm-tree-make-file-list) "vm-menu" "Makes a list with the files and subdirectories of DIR.
The list looks like: ((dirname1 file1 file2) 
                      file3
                      (dirname2 (dirname3 file4 file5) file6))" nil nil)

(autoload (quote vm-menu-hm-tree-hide-file-p) "vm-menu" "t, if one of the regexps in RE-HIDDEN-FILE-LIST matches the FILENAME." nil nil)

(autoload (quote vm-menu-hm-tree-make-menu) "vm-menu" "Returns a menu list.
Each item of the menu list has the form 
 [\"subdir\" (FUNCTION \"dir\") SELECTABLE].
Hidden directories (with a leading point) are suppressed, 
if NO-HIDDEN-DIRS are non nil. Also all files which are
matching a regexp in RE-HIDDEN-FILE-LIST are suppressed.
If INCLUDE-CURRENT-DIR non nil, then an additional command
for the current directory (.) is inserted." nil nil)

(autoload (quote vm-location-data-of) "vm-message" nil nil t)

(autoload (quote vm-start-of) "vm-message" nil nil t)

(autoload (quote vm-headers-of) "vm-message" nil nil t)

(autoload (quote vm-vheaders-of) "vm-message" nil nil nil)

(autoload (quote vm-text-of) "vm-message" nil nil nil)

(autoload (quote vm-text-end-of) "vm-message" nil nil t)

(autoload (quote vm-end-of) "vm-message" nil nil t)

(autoload (quote vm-softdata-of) "vm-message" nil nil t)

(autoload (quote vm-number-of) "vm-message" nil nil t)

(autoload (quote vm-padded-number-of) "vm-message" nil nil t)

(autoload (quote vm-mark-of) "vm-message" nil nil t)

(autoload (quote vm-su-start-of) "vm-message" nil nil t)

(autoload (quote vm-su-end-of) "vm-message" nil nil t)

(autoload (quote vm-real-message-sym-of) "vm-message" nil nil t)

(autoload (quote vm-real-message-of) "vm-message" nil nil t)

(autoload (quote vm-reverse-link-of) "vm-message" nil nil t)

(autoload (quote vm-message-type-of) "vm-message" nil nil t)

(autoload (quote vm-message-id-number-of) "vm-message" nil nil t)

(autoload (quote vm-buffer-of) "vm-message" nil nil t)

(autoload (quote vm-thread-indentation-of) "vm-message" nil nil t)

(autoload (quote vm-thread-list-of) "vm-message" nil nil t)

(autoload (quote vm-babyl-frob-flag-of) "vm-message" nil nil t)

(autoload (quote vm-saved-virtual-attributes-of) "vm-message" nil nil t)

(autoload (quote vm-saved-virtual-mirror-data-of) "vm-message" nil nil t)

(autoload (quote vm-virtual-summary-of) "vm-message" nil nil t)

(autoload (quote vm-mime-layout-of) "vm-message" nil nil t)

(autoload (quote vm-mime-encoded-header-flag-of) "vm-message" nil nil t)

(autoload (quote vm-attributes-of) "vm-message" nil nil t)

(autoload (quote vm-new-flag) "vm-message" nil nil t)

(autoload (quote vm-unread-flag) "vm-message" nil nil t)

(autoload (quote vm-deleted-flag) "vm-message" nil nil t)

(autoload (quote vm-filed-flag) "vm-message" nil nil t)

(autoload (quote vm-replied-flag) "vm-message" nil nil t)

(autoload (quote vm-written-flag) "vm-message" nil nil t)

(autoload (quote vm-forwarded-flag) "vm-message" nil nil t)

(autoload (quote vm-edited-flag) "vm-message" nil nil t)

(autoload (quote vm-redistributed-flag) "vm-message" nil nil t)

(autoload (quote vm-cache-of) "vm-message" nil nil t)

(autoload (quote vm-byte-count-of) "vm-message" nil nil t)

(autoload (quote vm-weekday-of) "vm-message" nil nil t)

(autoload (quote vm-monthday-of) "vm-message" nil nil t)

(autoload (quote vm-month-of) "vm-message" nil nil t)

(autoload (quote vm-year-of) "vm-message" nil nil t)

(autoload (quote vm-hour-of) "vm-message" nil nil t)

(autoload (quote vm-zone-of) "vm-message" nil nil t)

(autoload (quote vm-full-name-of) "vm-message" nil nil t)

(autoload (quote vm-from-of) "vm-message" nil nil t)

(autoload (quote vm-message-id-of) "vm-message" nil nil t)

(autoload (quote vm-line-count-of) "vm-message" nil nil t)

(autoload (quote vm-subject-of) "vm-message" nil nil t)

(autoload (quote vm-vheaders-regexp-of) "vm-message" nil nil t)

(autoload (quote vm-to-of) "vm-message" nil nil t)

(autoload (quote vm-to-names-of) "vm-message" nil nil t)

(autoload (quote vm-month-number-of) "vm-message" nil nil t)

(autoload (quote vm-sortable-datestring-of) "vm-message" nil nil t)

(autoload (quote vm-sortable-subject-of) "vm-message" nil nil t)

(autoload (quote vm-summary-of) "vm-message" nil nil t)

(autoload (quote vm-parent-of) "vm-message" nil nil t)

(autoload (quote vm-mirror-data-of) "vm-message" nil nil t)

(autoload (quote vm-edit-buffer-of) "vm-message" nil nil t)

(autoload (quote vm-virtual-messages-of) "vm-message" nil nil t)

(autoload (quote vm-modflag-of) "vm-message" nil nil t)

(autoload (quote vm-labels-of) "vm-message" nil nil t)

(autoload (quote vm-label-string-of) "vm-message" nil nil t)

(autoload (quote vm-set-location-data-of) "vm-message" nil nil t)

(autoload (quote vm-set-start-of) "vm-message" nil nil t)

(autoload (quote vm-set-headers-of) "vm-message" nil nil t)

(autoload (quote vm-set-vheaders-of) "vm-message" nil nil t)

(autoload (quote vm-set-text-of) "vm-message" nil nil t)

(autoload (quote vm-set-text-end-of) "vm-message" nil nil t)

(autoload (quote vm-set-end-of) "vm-message" nil nil t)

(autoload (quote vm-set-softdata-of) "vm-message" nil nil t)

(autoload (quote vm-set-number-of) "vm-message" nil nil t)

(autoload (quote vm-set-padded-number-of) "vm-message" nil nil t)

(autoload (quote vm-set-mark-of) "vm-message" nil nil t)

(autoload (quote vm-set-su-start-of) "vm-message" nil nil t)

(autoload (quote vm-set-su-end-of) "vm-message" nil nil t)

(autoload (quote vm-set-real-message-sym-of) "vm-message" nil nil t)

(autoload (quote vm-set-reverse-link-of) "vm-message" nil nil t)

(autoload (quote vm-set-reverse-link-sym-of) "vm-message" nil nil t)

(autoload (quote vm-set-message-type-of) "vm-message" nil nil t)

(autoload (quote vm-set-message-id-number-of) "vm-message" nil nil t)

(autoload (quote vm-set-buffer-of) "vm-message" nil nil t)

(autoload (quote vm-set-thread-indentation-of) "vm-message" nil nil t)

(autoload (quote vm-set-thread-list-of) "vm-message" nil nil t)

(autoload (quote vm-set-babyl-frob-flag-of) "vm-message" nil nil t)

(autoload (quote vm-set-saved-virtual-attributes-of) "vm-message" nil nil t)

(autoload (quote vm-set-saved-virtual-mirror-data-of) "vm-message" nil nil t)

(autoload (quote vm-set-virtual-summary-of) "vm-message" nil nil t)

(autoload (quote vm-set-mime-layout-of) "vm-message" nil nil t)

(autoload (quote vm-set-mime-encoded-header-flag-of) "vm-message" nil nil t)

(autoload (quote vm-set-attributes-of) "vm-message" nil nil t)

(autoload (quote vm-set-edited-flag-of) "vm-message" nil nil nil)

(autoload (quote vm-set-cache-of) "vm-message" nil nil t)

(autoload (quote vm-set-byte-count-of) "vm-message" nil nil t)

(autoload (quote vm-set-weekday-of) "vm-message" nil nil t)

(autoload (quote vm-set-monthday-of) "vm-message" nil nil t)

(autoload (quote vm-set-month-of) "vm-message" nil nil t)

(autoload (quote vm-set-year-of) "vm-message" nil nil t)

(autoload (quote vm-set-hour-of) "vm-message" nil nil t)

(autoload (quote vm-set-zone-of) "vm-message" nil nil t)

(autoload (quote vm-set-full-name-of) "vm-message" nil nil t)

(autoload (quote vm-set-from-of) "vm-message" nil nil t)

(autoload (quote vm-set-message-id-of) "vm-message" nil nil t)

(autoload (quote vm-set-line-count-of) "vm-message" nil nil t)

(autoload (quote vm-set-subject-of) "vm-message" nil nil t)

(autoload (quote vm-set-vheaders-regexp-of) "vm-message" nil nil t)

(autoload (quote vm-set-to-of) "vm-message" nil nil t)

(autoload (quote vm-set-to-names-of) "vm-message" nil nil t)

(autoload (quote vm-set-month-number-of) "vm-message" nil nil t)

(autoload (quote vm-set-sortable-datestring-of) "vm-message" nil nil t)

(autoload (quote vm-set-sortable-subject-of) "vm-message" nil nil t)

(autoload (quote vm-set-summary-of) "vm-message" nil nil t)

(autoload (quote vm-set-parent-of) "vm-message" nil nil t)

(autoload (quote vm-set-mirror-data-of) "vm-message" nil nil t)

(autoload (quote vm-set-edit-buffer-of) "vm-message" nil nil t)

(autoload (quote vm-set-virtual-messages-of) "vm-message" nil nil t)

(autoload (quote vm-set-virtual-messages-sym-of) "vm-message" nil nil t)

(autoload (quote vm-set-modflag-of) "vm-message" nil nil t)

(autoload (quote vm-set-labels-of) "vm-message" nil nil t)

(autoload (quote vm-set-label-string-of) "vm-message" nil nil t)

(autoload (quote vm-make-message) "vm-message" nil nil nil)

(autoload (quote vm-find-and-set-text-of) "vm-message" nil nil nil)

(autoload (quote vm-virtual-message-p) "vm-message" nil nil nil)

(autoload (quote vm-mime-error) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-type) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-qtype) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-encoding) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-id) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-description) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-disposition) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-qdisposition) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-header-start) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-body-start) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-body-end) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-parts) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout-cache) "vm-mime" nil nil nil)

(autoload (quote vm-set-mm-layout-type) "vm-mime" nil nil nil)

(autoload (quote vm-set-mm-layout-cache) "vm-mime" nil nil nil)

(autoload (quote vm-mm-layout) "vm-mime" nil nil nil)

(autoload (quote vm-mm-encoded-header) "vm-mime" nil nil nil)

(autoload (quote vm-mime-Q-decode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-Q-encode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-B-encode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-crlf-to-lf-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-lf-to-crlf-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-charset-decode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-transfer-decode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-base64-decode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-base64-encode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-qp-decode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-qp-encode-region) "vm-mime" nil nil nil)

(autoload (quote vm-decode-mime-message-headers) "vm-mime" nil nil nil)

(autoload (quote vm-decode-mime-encoded-words) "vm-mime" nil nil nil)

(autoload (quote vm-decode-mime-encoded-words-in-string) "vm-mime" nil nil nil)

(autoload (quote vm-reencode-mime-encoded-words) "vm-mime" nil nil nil)

(autoload (quote vm-reencode-mime-encoded-words-in-string) "vm-mime" nil nil nil)

(autoload (quote vm-mime-parse-content-header) "vm-mime" nil nil nil)

(autoload (quote vm-mime-get-header-contents) "vm-mime" nil nil nil)

(autoload (quote vm-mime-parse-entity) "vm-mime" nil nil nil)

(autoload (quote vm-mime-parse-entity-safe) "vm-mime" nil nil nil)

(autoload (quote vm-mime-get-xxx-parameter) "vm-mime" nil nil nil)

(autoload (quote vm-mime-get-parameter) "vm-mime" nil nil nil)

(autoload (quote vm-mime-get-disposition-parameter) "vm-mime" nil nil nil)

(autoload (quote vm-mime-insert-mime-body) "vm-mime" nil nil nil)

(autoload (quote vm-mime-insert-mime-headers) "vm-mime" nil nil nil)

(autoload (quote vm-make-presentation-copy) "vm-mime" nil nil nil)

(autoload (quote vm-determine-proper-charset) "vm-mime" nil nil nil)

(autoload (quote vm-determine-proper-content-transfer-encoding) "vm-mime" nil nil nil)

(autoload (quote vm-mime-types-match) "vm-mime" nil nil nil)

(autoload (quote vm-mime-can-display-internal) "vm-mime" nil nil nil)

(autoload (quote vm-mime-can-convert) "vm-mime" nil nil nil)

(autoload (quote vm-mime-convert-undisplayable-layout) "vm-mime" nil nil nil)

(autoload (quote vm-mime-should-display-button) "vm-mime" nil nil nil)

(autoload (quote vm-mime-should-display-internal) "vm-mime" nil nil nil)

(autoload (quote vm-mime-find-external-viewer) "vm-mime" nil nil nil)

(autoload (quote vm-mime-delete-button-maybe) "vm-mime" nil nil nil)

(autoload (quote vm-decode-mime-message) "vm-mime" "Decode the MIME objects in the current message.

The first time this command is run on a message, decoding is done.
The second time, buttons for all the objects are displayed instead.
The third time, the raw, undecoded data is displayed.

If decoding, the decoded objects might be displayed immediately, or
buttons might be displayed that you need to activate to view the
object.  See the documentation for the variables

    vm-auto-displayed-mime-content-types
    vm-mime-internal-content-types
    vm-mime-external-content-types-alist

to see how to control whether you see buttons or objects.

If the variable vm-mime-display-function is set, then its value
is called as a function with no arguments, and none of the
actions mentioned in the preceding paragraphs are done.  At the
time of the call, the current buffer will be the presentation
buffer for the folder and a copy of the current message will be
in the buffer.  The function is expected to make the message
`MIME presentable' to the user in whatever manner it sees fit." t nil)

(autoload (quote vm-decode-mime-layout) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-text) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-text/plain) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-text/enriched) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-external-generic) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-application/octet-stream) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-image) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-audio) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-video) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-message) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-multipart) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-multipart/mixed) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-multipart/alternative) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-multipart/parallel) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-multipart/digest) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-message/rfc822) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-message/partial) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-image-xxxx) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-image/gif) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-image/jpeg) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-image/png) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-image/tiff) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-internal-audio/basic) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-button-xxxx) "vm-mime" nil nil nil)

(autoload (quote vm-mime-run-display-function-at-point) "vm-mime" nil t nil)

(autoload (quote vm-mime-set-extent-glyph-for-layout) "vm-mime" nil nil nil)

(autoload (quote vm-mime-insert-button) "vm-mime" nil nil nil)

(autoload (quote vm-mime-rewrite-failed-button) "vm-mime" nil nil nil)

(autoload (quote vm-mime-send-body-to-file) "vm-mime" nil nil nil)

(autoload (quote vm-mime-pipe-body-to-command) "vm-mime" nil nil nil)

(autoload (quote vm-mime-pipe-body-to-queried-command) "vm-mime" nil nil nil)

(autoload (quote vm-mime-pipe-body-to-queried-command-discard-output) "vm-mime" nil nil nil)

(autoload (quote vm-mime-send-body-to-printer) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-body-as-text) "vm-mime" nil nil nil)

(autoload (quote vm-mime-display-body-using-external-viewer) "vm-mime" nil nil nil)

(autoload (quote vm-mime-scrub-description) "vm-mime" nil nil nil)

(autoload (quote vm-mime-layout-description) "vm-mime" nil nil nil)

(autoload (quote vm-mime-layout-contains-type) "vm-mime" nil nil nil)

(autoload (quote vm-mime-plain-message-p) "vm-mime" nil nil nil)

(autoload (quote vm-mime-text-type-p) "vm-mime" nil nil nil)

(autoload (quote vm-mime-charset-internally-displayable-p) "vm-mime" nil nil nil)

(autoload (quote vm-mime-find-message/partials) "vm-mime" nil nil nil)

(autoload (quote vm-message-at-point) "vm-mime" nil nil nil)

(autoload (quote vm-mime-make-multipart-boundary) "vm-mime" nil nil nil)

(autoload (quote vm-mime-attach-file) "vm-mime" "Attach a file to a VM composition buffer to be sent along with the message.
The file is not inserted into the buffer and MIME encoded until
you execute vm-mail-send or vm-mail-send-and-exit.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, FILE, is the name of the file to attach.  Second
argument, TYPE, is the MIME Content-Type of the file.  Optional
third argument CHARSET is the character set of the attached
document.  This argument is only used for text types, and it is
ignored for other types.  Optional fourth argument DESCRIPTION
should be a one line description of the file.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use vm-mime-attach-mime-file to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer." t nil)

(autoload (quote vm-mime-attach-mime-file) "vm-mime" "Attach a MIME encoded file to a VM composition buffer to be sent
along with the message.

The file is not inserted into the buffer until you execute
vm-mail-send or vm-mail-send-and-exit.  A visible tag indicating
the existence of the attachment is placed in the composition
buffer.  You can move the attachment around or remove it entirely
with normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

The sole argument, FILE, is the name of the file to attach.
When called interactively the FILE argument is read from the
minibuffer.

This command is for attaching files that have a MIME
header section at the top.  For files without MIME headers, you
should use vm-mime-attach-file to attach such a file.  VM
will interactively query you for the file type information." t nil)

(autoload (quote vm-mime-attach-object) "vm-mime" nil nil nil)

(autoload (quote vm-mime-attachment-disposition-at-point) "vm-mime" nil nil nil)

(autoload (quote vm-mime-set-attachment-disposition-at-point) "vm-mime" nil nil nil)

(autoload (quote vm-disallow-overlay-endpoint-insertion) "vm-mime" nil nil nil)

(autoload (quote vm-mime-fake-attachment-overlays) "vm-mime" nil nil nil)

(autoload (quote vm-mime-default-type-from-filename) "vm-mime" nil nil nil)

(autoload (quote vm-remove-mail-mode-header-separator) "vm-mime" nil nil nil)

(autoload (quote vm-add-mail-mode-header-separator) "vm-mime" nil nil nil)

(autoload (quote vm-mime-transfer-encode-region) "vm-mime" nil nil nil)

(autoload (quote vm-mime-transfer-encode-layout) "vm-mime" nil nil nil)

(autoload (quote vm-mime-encode-composition) "vm-mime" "MIME encode the current buffer.
Attachment tags added to the buffer with vm-mime-attach-file are expanded
and the approriate content-type and boundary markup information is added." t nil)

(autoload (quote vm-mime-fragment-composition) "vm-mime" nil nil nil)

(autoload (quote vm-mime-preview-composition) "vm-mime" "Show how the current composition buffer might be displayed
in a MIME-aware mail reader.  VM copies and encodes the current
mail composition buffer and displays it as a mail folder.
Type `q' to quit this temp folder and return to composing your
message." t nil)

(autoload (quote vm-mime-composite-type-p) "vm-mime" nil nil nil)

(autoload (quote vm-mime-map-atomic-layouts) "vm-mime" nil nil nil)

(autoload (quote vm-minibuffer-complete-word) "vm-minibuf" nil t nil)

(autoload (quote vm-minibuffer-complete-word-and-exit) "vm-minibuf" nil t nil)

(autoload (quote vm-minibuffer-completion-message) "vm-minibuf" "Briefly display STRING to the right of the current minibuffer input.
Optional second arg SECONDS specifies how long to keep the message visible;
the default is 2 seconds.

A keypress causes the immediate erasure of the STRING, and return of control
to the calling program." nil nil)

(autoload (quote vm-minibuffer-replace-word) "vm-minibuf" nil nil nil)

(autoload (quote vm-minibuffer-show-completions) "vm-minibuf" "Display LIST in a multi-column listing in the \" *Completions*\" buffer.
LIST should be a list of strings." nil nil)

(autoload (quote vm-show-list) "vm-minibuf" "Display LIST in a multi-column listing in the current buffer at point.
The current buffer must be displayed in some window at the time
this function is called.

LIST should be a list of strings.

Optional second argument FUNCTION will be called if the mouse is
clicked on one of the strings in the current buffer.  The string
clicked upon will be passed to FUNCTION as its sole argument.

Optional third argument KEYMAPS specifies a lists of keymaps
where the FUNCTION should be bound to the mouse clicks.  By
default the local keymap of the current buffer is used." nil nil)

(autoload (quote vm-minibuffer-completion-help) "vm-minibuf" nil t nil)

(autoload (quote vm-keyboard-read-string) "vm-minibuf" nil nil nil)

(autoload (quote vm-read-string) "vm-minibuf" nil nil nil)

(autoload (quote vm-read-number) "vm-minibuf" nil nil nil)

(autoload (quote vm-read-password) "vm-minibuf" "Read and return a password from the minibuffer, prompting with PROMPT.
Optional second argument CONFIRM non-nil means that the user will be asked
  to type the password a second time for confirmation and if there is a
  mismatch, the process is repeated.

Line editing keys are:
  C-h, DEL	rubout
  C-u, C-x      line kill
  C-q, C-v      literal next" nil nil)

(autoload (quote vm-keyboard-read-file-name) "vm-minibuf" "Like read-file-name, except HISTORY's value is unaltered." nil nil)

(autoload (quote vm-read-file-name) "vm-minibuf" "Like read-file-name, except a mouse interface is used if a mouse
click mouse triggered the current command." nil nil)

(autoload (quote vm-delete-non-matching-strings) "vm-misc" "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned." nil nil)

(autoload (quote vm-parse) "vm-misc" nil nil nil)

(autoload (quote vm-parse-addresses) "vm-misc" nil nil nil)

(autoload (quote vm-write-string) "vm-misc" nil nil nil)

(autoload (quote vm-marker) "vm-misc" nil nil t)

(autoload (quote vm-increment) "vm-misc" nil nil t)

(autoload (quote vm-decrement) "vm-misc" nil nil t)

(autoload (quote vm-select-folder-buffer) "vm-misc" nil nil t)

(autoload (quote vm-check-for-killed-summary) "vm-misc" nil nil nil)

(autoload (quote vm-check-for-killed-presentation) "vm-misc" nil nil nil)

(autoload (quote vm-check-for-killed-folder) "vm-misc" nil nil nil)

(autoload (quote vm-error-if-folder-read-only) "vm-misc" nil nil t)

(autoload (quote vm-error-if-virtual-folder) "vm-misc" nil nil t)

(autoload (quote vm-build-threads-if-unbuilt) "vm-misc" nil nil t)

(autoload (quote vm-abs) "vm-misc" nil nil nil)

(autoload (quote vm-save-restriction) "vm-misc" nil nil t)

(autoload (quote vm-save-buffer-excursion) "vm-misc" nil nil t)

(autoload (quote vm-last) "vm-misc" nil nil nil)

(autoload (quote vm-vector-to-list) "vm-misc" nil nil nil)

(autoload (quote vm-extend-vector) "vm-misc" nil nil nil)

(autoload (quote vm-obarray-to-string-list) "vm-misc" nil nil nil)

(autoload (quote vm-mapcar) "vm-misc" nil nil nil)

(autoload (quote vm-mapc) "vm-misc" nil nil nil)

(autoload (quote vm-delete) "vm-misc" nil nil nil)

(autoload (quote vm-delete-directory-file-names) "vm-misc" nil nil nil)

(autoload (quote vm-delete-backup-file-names) "vm-misc" nil nil nil)

(autoload (quote vm-delete-auto-save-file-names) "vm-misc" nil nil nil)

(autoload (quote vm-delete-duplicates) "vm-misc" "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurrence of a string in the list,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)" nil nil)

(autoload (quote vm-member-0) "vm-misc" nil nil nil)

(autoload (quote vm-delqual) "vm-misc" nil nil nil)

(autoload (quote vm-copy-local-variables) "vm-misc" nil nil nil)

(autoload (quote vm-error-if-folder-empty) "vm-misc" nil nil nil)

(autoload (quote vm-copy) "vm-misc" nil nil nil)

(autoload (quote vm-xemacs-p) "vm-misc" nil nil nil)

(autoload (quote vm-xemacs-mule-p) "vm-misc" nil nil nil)

(autoload (quote vm-fsfemacs-19-p) "vm-misc" nil nil nil)

(autoload (quote vm-multiple-frames-possible-p) "vm-misc" nil nil nil)

(autoload (quote vm-mouse-support-possible-p) "vm-misc" nil nil nil)

(autoload (quote vm-menu-support-possible-p) "vm-misc" nil nil nil)

(autoload (quote vm-toolbar-support-possible-p) "vm-misc" nil nil nil)

(autoload (quote vm-multiple-fonts-possible-p) "vm-misc" nil nil nil)

(autoload (quote vm-run-message-hook) "vm-misc" nil nil nil)

(autoload (quote vm-error-free-call) "vm-misc" nil nil nil)

(autoload (quote vm-trace) "vm-misc" nil nil nil)

(autoload (quote vm-timezone-make-date-sortable) "vm-misc" nil nil nil)

(autoload (quote vm-current-time-zone) "vm-misc" nil nil nil)

(autoload (quote vm-should-generate-summary) "vm-misc" nil nil nil)

(autoload (quote vm-find-composition-buffer) "vm-misc" nil nil nil)

(autoload (quote vm-get-file-buffer) "vm-misc" "Like get-file-buffer, but also checks buffers against FILE's truename" nil nil)

(autoload (quote vm-set-region-face) "vm-misc" nil nil nil)

(autoload (quote vm-default-buffer-substring-no-properties) "vm-misc" nil nil nil)

(autoload (quote vm-buffer-string-no-properties) "vm-misc" nil nil nil)

(autoload (quote vm-insert-region-from-buffer) "vm-misc" nil nil nil)

(autoload (quote vm-copy-extent) "vm-misc" nil nil nil)

(autoload (quote vm-make-tempfile-name) "vm-misc" nil nil nil)

(autoload (quote vm-insert-char) "vm-misc" nil nil nil)

(autoload (quote vm-xemacs-compatible-insert-char) "vm-misc" nil nil nil)

(autoload (quote vm-symbol-lists-intersect-p) "vm-misc" nil nil nil)

(autoload (quote vm-set-buffer-variable) "vm-misc" nil nil nil)

(autoload (quote vm-buffer-variable-value) "vm-misc" nil nil nil)

(autoload (quote vm-with-virtual-selector-variables) "vm-misc" nil nil t)

(autoload (quote vm-string-assoc) "vm-misc" nil nil nil)

(autoload (quote vm-string-member) "vm-misc" nil nil nil)

(autoload (quote vm-assert) "vm-misc" nil nil t)

(autoload (quote vm-mouse-fsfemacs-mouse-p) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-xemacs-mouse-p) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-set-mouse-track-highlight) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-button-2) "vm-mouse" nil t nil)

(autoload (quote vm-mouse-button-3) "vm-mouse" nil t nil)

(autoload (quote vm-mouse-3-help) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-get-mouse-track-string) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-popup-or-select) "vm-mouse" nil t nil)

(autoload (quote vm-mouse-send-url-at-event) "vm-mouse" nil t nil)

(autoload (quote vm-mouse-send-url-at-position) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-send-url) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-send-url-to-netscape) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-send-url-to-mosaic) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-install-mouse) "vm-mouse" nil nil nil)

(autoload (quote vm-run-background-command) "vm-mouse" nil nil nil)

(autoload (quote vm-run-command) "vm-mouse" nil nil nil)

(autoload (quote vm-run-command-on-region) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-read-file-name) "vm-mouse" "Like read-file-name, except uses a mouse driven interface.
HISTORY argument is ignored." nil nil)

(autoload (quote vm-mouse-read-file-name-event-handler) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-read-file-name-quit-handler) "vm-mouse" nil t nil)

(autoload (quote vm-mouse-read-string) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-read-string-event-handler) "vm-mouse" nil nil nil)

(autoload (quote vm-mouse-read-string-quit-handler) "vm-mouse" nil t nil)

(autoload (quote vm-record-and-change-message-pointer) "vm-motion" nil nil nil)

(autoload (quote vm-goto-message) "vm-motion" "Go to the message numbered N.
Interactively N is the prefix argument.  If no prefix arg is provided
N is prompted for in the minibuffer.

If vm-follow-summary-cursor is non-nil this command will go to
the message under the cursor in the summary buffer if the summary
window is selected.  This only happens if no prefix argument is
given." t nil)

(autoload (quote vm-goto-message-last-seen) "vm-motion" "Go to the message last previewed." t nil)

(autoload (quote vm-goto-parent-message) "vm-motion" "Go to the parent of the current message." t nil)

(autoload (quote vm-check-count) "vm-motion" nil nil nil)

(autoload (quote vm-move-message-pointer) "vm-motion" nil nil nil)

(autoload (quote vm-should-skip-message) "vm-motion" nil nil nil)

(autoload (quote vm-next-message) "vm-motion" "Go forward one message and preview it.
With prefix arg (optional first argument) COUNT, go forward COUNT
messages.  A negative COUNT means go backward.  If the absolute
value of COUNT is greater than 1, then the values of the variables
vm-skip-deleted-messages and vm-skip-read-messages are ignored.

When invoked on marked messages (via vm-next-command-uses-marks)
this command 'sees' marked messages as it moves." t nil)

(autoload (quote vm-previous-message) "vm-motion" "Go back one message and preview it.
With prefix arg COUNT, go backward COUNT messages.  A negative COUNT
means go forward.  If the absolute value of COUNT > 1 the values of the
variables vm-skip-deleted-messages and vm-skip-read-messages are
ignored." t nil)

(autoload (quote vm-next-message-no-skip) "vm-motion" "Like vm-next-message but will not skip deleted or read messages." t nil)

(autoload (quote vm-previous-message-no-skip) "vm-motion" "Like vm-previous-message but will not skip deleted or read messages." t nil)

(autoload (quote vm-next-unread-message) "vm-motion" "Move forward to the nearest new or unread message, if there is one." t nil)

(autoload (quote vm-previous-unread-message) "vm-motion" "Move backward to the nearest new or unread message, if there is one." t nil)

(autoload (quote vm-next-message-same-subject) "vm-motion" "Move forward to the nearest message with the same subject.
vm-subject-ignored-prefix and vm-subject-ignored-suffix will apply
to the subject comparisons." t nil)

(autoload (quote vm-previous-message-same-subject) "vm-motion" "Move backward to the nearest message with the same subject.
vm-subject-ignored-prefix and vm-subject-ignored-suffix will apply
to the subject comparisons." t nil)

(autoload (quote vm-find-first-unread-message) "vm-motion" nil nil nil)

(autoload (quote vm-thoughtfully-select-message) "vm-motion" nil nil nil)

(autoload (quote vm-follow-summary-cursor) "vm-motion" nil nil nil)

(autoload (quote vm-scroll-forward) "vm-page" "Scroll forward a screenful of text.
If the current message is being previewed, the message body is revealed.
If at the end of the current message, moves to the next message iff the
value of vm-auto-next-message is non-nil.
Prefix argument N means scroll forward N lines." t nil)

(autoload (quote vm-scroll-forward-internal) "vm-page" nil nil nil)

(autoload (quote vm-howl-if-eom) "vm-page" nil nil nil)

(autoload (quote vm-emit-eom-blurb) "vm-page" nil nil nil)

(autoload (quote vm-scroll-backward) "vm-page" "Scroll backward a screenful of text.
Prefix N scrolls backward N lines." t nil)

(autoload (quote vm-highlight-headers) "vm-page" nil nil nil)

(autoload (quote vm-energize-urls) "vm-page" nil nil nil)

(autoload (quote vm-energize-headers) "vm-page" nil nil nil)

(autoload (quote vm-display-xface) "vm-page" nil nil nil)

(autoload (quote vm-url-help) "vm-page" nil nil nil)

(autoload (quote vm-energize-urls-in-message-region) "vm-page" nil nil nil)

(autoload (quote vm-highlight-headers-maybe) "vm-page" nil nil nil)

(autoload (quote vm-energize-headers-and-xfaces) "vm-page" nil nil nil)

(autoload (quote vm-narrow-for-preview) "vm-page" nil nil nil)

(autoload (quote vm-preview-current-message) "vm-page" nil nil nil)

(autoload (quote vm-show-current-message) "vm-page" nil nil nil)

(autoload (quote vm-expose-hidden-headers) "vm-page" "Toggle exposing and hiding message headers that are normally not visible." t nil)

(autoload (quote vm-widen-page) "vm-page" nil nil nil)

(autoload (quote vm-narrow-to-page) "vm-page" nil nil nil)

(autoload (quote vm-beginning-of-message) "vm-page" "Moves to the beginning of the current message." t nil)

(autoload (quote vm-end-of-message) "vm-page" "Moves to the end of the current message, exposing and flagging it read
as necessary." t nil)

(autoload (quote vm-pop-move-mail) "vm-pop" nil nil nil)

(autoload (quote vm-pop-check-mail) "vm-pop" nil nil nil)

(autoload (quote vm-pop-make-session) "vm-pop" nil nil nil)

(autoload (quote vm-pop-end-session) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-timer) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-x-box) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-x-currmsg) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-x-maxmsg) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-x-got) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-x-need) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-y-box) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-y-currmsg) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-y-maxmsg) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-y-got) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stat-y-need) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-timer) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-x-box) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-x-currmsg) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-x-maxmsg) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-x-got) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-x-need) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-y-box) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-y-currmsg) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-y-maxmsg) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-y-got) "vm-pop" nil nil nil)

(autoload (quote vm-set-pop-stat-y-need) "vm-pop" nil nil nil)

(autoload (quote vm-pop-start-status-timer) "vm-pop" nil nil nil)

(autoload (quote vm-pop-stop-status-timer) "vm-pop" nil nil nil)

(autoload (quote vm-pop-report-retrieval-status) "vm-pop" nil nil nil)

(autoload (quote vm-pop-send-command) "vm-pop" nil nil nil)

(autoload (quote vm-pop-read-response) "vm-pop" nil nil nil)

(autoload (quote vm-pop-read-past-dot-sentinel-line) "vm-pop" nil nil nil)

(autoload (quote vm-pop-read-stat-response) "vm-pop" nil nil nil)

(autoload (quote vm-pop-read-list-response) "vm-pop" nil nil nil)

(autoload (quote vm-pop-ask-about-large-message) "vm-pop" nil nil nil)

(autoload (quote vm-pop-retrieve-to-crashbox) "vm-pop" nil nil nil)

(autoload (quote vm-pop-cleanup-region) "vm-pop" nil nil nil)

(autoload (quote vm-pop-md5) "vm-pop" nil nil nil)

(autoload (quote vm-do-reply) "vm-reply" nil nil nil)

(autoload (quote vm-strip-ignored-addresses) "vm-reply" nil nil nil)

(autoload (quote vm-ignored-reply-to) "vm-reply" nil nil nil)

(autoload (quote vm-mail-yank-default) "vm-reply" nil nil nil)

(autoload (quote vm-yank-message-other-folder) "vm-reply" "Like vm-yank-message except the message is yanked from a folder other
than the one that spawned the current Mail mode buffer.  The name of the
folder is read from the minibuffer.

Don't call this function from a program." t nil)

(autoload (quote vm-yank-message) "vm-reply" "Yank message number N into the current buffer at point.
When called interactively N is always read from the minibuffer.  When
called non-interactively the first argument is expected to be a
message struct.

This command is meant to be used in VM created Mail mode buffers; the
yanked message comes from the mail buffer containing the message you
are replying to, forwarding, or invoked VM's mail command from.

All message headers are yanked along with the text.  Point is
left before the inserted text, the mark after.  Any hook
functions bound to mail-citation-hook are run, after inserting
the text and setting point and mark.  For backward compatibility,
if mail-citation-hook is set to nil, `mail-yank-hooks' is run
instead.

If mail-citation-hook and mail-yank-hooks are both nil, this
default action is taken: the yanked headers are trimmed as
specified by vm-included-text-headers and
vm-included-text-discard-header-regexp, and the value of
vm-included-text-prefix is prepended to every yanked line." t nil)

(autoload (quote vm-mail-send-and-exit) "vm-reply" "Just like mail-send-and-exit except that VM flags the appropriate message(s)
as having been replied to, if appropriate." t nil)

(autoload (quote vm-keep-mail-buffer) "vm-reply" nil nil nil)

(autoload (quote vm-help-tale) "vm-reply" nil nil nil)

(autoload (quote vm-mail-send) "vm-reply" "Just like mail-send except that VM flags the appropriate message(s)
as replied to, forwarded, etc, if appropriate." t nil)

(autoload (quote vm-mail-mode-get-header-contents) "vm-reply" nil nil nil)

(autoload (quote vm-rename-current-mail-buffer) "vm-reply" nil nil nil)

(autoload (quote vm-mail-mark-replied) "vm-reply" nil nil nil)

(autoload (quote vm-mail-mark-forwarded) "vm-reply" nil nil nil)

(autoload (quote vm-mail-mark-redistributed) "vm-reply" nil nil nil)

(autoload (quote vm-reply) "vm-reply" "Reply to the sender of the current message.
Numeric prefix argument N means to reply to the current message plus the
next N-1 messages.  A negative N means reply to the current message and
the previous N-1 messages. 

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be replied to.

You will be placed into a standard Emacs Mail mode buffer to compose and
send your message.  See the documentation for the function `mail' for
more info.

Note that the normal binding of C-c C-y in the reply buffer is
automatically changed to vm-yank-message during a reply.  This
allows you to yank any message from the current folder into a
reply.

Normal VM commands may be accessed in the reply buffer by prefixing them
with C-c C-v." t nil)

(autoload (quote vm-reply-include-text) "vm-reply" "Reply to the sender (only) of the current message and include text
from the message.  See the documentation for function vm-reply for details." t nil)

(autoload (quote vm-followup) "vm-reply" "Reply to all recipients of the current message.
See the documentation for the function vm-reply for details." t nil)

(autoload (quote vm-followup-include-text) "vm-reply" "Reply to all recipients of the current message and include text from
the message.  See the documentation for the function vm-reply for details." t nil)

(autoload (quote vm-forward-message-all-headers) "vm-reply" "Like vm-forward-message but always forwards all the headers." t nil)

(autoload (quote vm-forward-message) "vm-reply" "Forward the current message to one or more recipients.
You will be placed in a Mail mode buffer as you would with a
reply, but you must fill in the To: header and perhaps the
Subject: header manually." t nil)

(autoload (quote vm-resend-bounced-message) "vm-reply" "Extract the original text from a bounced message and resend it.
You will be placed in a Mail mode buffer with the extracted message and
you can change the recipient address before resending the message." t nil)

(autoload (quote vm-resend-message) "vm-reply" "Resend the current message to someone else.
The current message will be copied to a Mail mode buffer and you
can edit the message and send it as usual.

NOTE: since you are doing a resend, a Resent-To header is
provided for you to fill in.  If you don't fill it in, when you
send the message it will go to the original recipients listed in
the To and Cc headers.  You may also create a Resent-Cc header." t nil)

(autoload (quote vm-send-digest) "vm-reply" "Send a digest of all messages in the current folder to recipients.
The type of the digest is specified by the variable vm-digest-send-type.
You will be placed in a Mail mode buffer as is usual with replies, but you
must fill in the To: and Subject: headers manually.

Prefix arg means to insert a list of preamble lines at the beginning of
the digest.  One line is generated for each message being digestified.
The variable vm-digest-preamble-format determines the format of the
preamble lines.

If invoked on marked messages (via vm-next-command-uses-marks),
only marked messages will be put into the digest." t nil)

(autoload (quote vm-send-rfc934-digest) "vm-reply" "Like vm-send-digest but always sends an RFC 934 digest." t nil)

(autoload (quote vm-send-rfc1153-digest) "vm-reply" "Like vm-send-digest but always sends an RFC 1153 digest." t nil)

(autoload (quote vm-send-mime-digest) "vm-reply" "Like vm-send-digest but always sends an MIME (multipart/digest) digest." t nil)

(autoload (quote vm-continue-composing-message) "vm-reply" "Find and select the most recently used mail composition buffer.
If the selected buffer is already a Mail mode buffer then it is
buried before beginning the search.  Non Mail mode buffers and
unmodified Mail buffers are skipped.  Prefix arg means unmodified
Mail mode buffers are not skipped.  If no suitable buffer is
found, the current buffer remains selected." t nil)

(autoload (quote vm-mail-to-mailto-url) "vm-reply" nil nil nil)

(autoload (quote vm-mail-internal) "vm-reply" nil nil nil)

(autoload (quote vm-reply-other-frame) "vm-reply" "Like vm-reply, but run in a newly created frame." t nil)

(autoload (quote vm-reply-include-text-other-frame) "vm-reply" "Like vm-reply-include-text, but run in a newly created frame." t nil)

(autoload (quote vm-followup-other-frame) "vm-reply" "Like vm-followup, but run in a newly created frame." t nil)

(autoload (quote vm-followup-include-text-other-frame) "vm-reply" "Like vm-followup-include-text, but run in a newly created frame." t nil)

(autoload (quote vm-forward-message-all-headers-other-frame) "vm-reply" "Like vm-forward-message-all-headers, but run in a newly created frame." t nil)

(autoload (quote vm-forward-message-other-frame) "vm-reply" "Like vm-forward-message, but run in a newly created frame." t nil)

(autoload (quote vm-resend-message-other-frame) "vm-reply" "Like vm-resend-message, but run in a newly created frame." t nil)

(autoload (quote vm-resend-bounced-message-other-frame) "vm-reply" "Like vm-resend-bounced-message, but run in a newly created frame." t nil)

(autoload (quote vm-send-digest-other-frame) "vm-reply" "Like vm-send-digest, but run in a newly created frame." t nil)

(autoload (quote vm-send-rfc934-digest-other-frame) "vm-reply" "Like vm-send-rfc934-digest, but run in a newly created frame." t nil)

(autoload (quote vm-send-rfc1153-digest-other-frame) "vm-reply" "Like vm-send-rfc1153-digest, but run in a newly created frame." t nil)

(autoload (quote vm-send-mime-digest-other-frame) "vm-reply" "Like vm-send-mime-digest, but run in a newly created frame." t nil)

(autoload (quote vm-match-data) "vm-save" nil nil nil)

(autoload (quote vm-auto-select-folder) "vm-save" nil nil nil)

(autoload (quote vm-auto-archive-messages) "vm-save" "Save all unfiled messages that auto-match a folder via
vm-auto-folder-alist to their appropriate folders.  Messages that
are flagged for deletion are not saved.

Prefix arg means to ask user for confirmation before saving each message.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are checked against vm-auto-folder-alist.

The saved messages are flagged as `filed'." t nil)

(autoload (quote vm-save-message) "vm-save" "Save the current message to a mail folder.
If the folder already exists, the message will be appended to it.

Prefix arg COUNT means save this message and the next COUNT-1
messages.  A negative COUNT means save this message and the
previous COUNT-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `filed'." t nil)

(autoload (quote vm-save-message-sans-headers) "vm-save" "Save the current message to a file, without its header section.
If the file already exists, the message will be appended to it.
Prefix arg COUNT means save the next COUNT messages.  A negative COUNT means
save the previous COUNT.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `written'.

This command should NOT be used to save message to mail folders; use
vm-save-message instead (normally bound to `s')." t nil)

(autoload (quote vm-pipe-message-to-command) "vm-save" "Run shell command with the some or all of the current message as input.
By default the entire message is used.
With one \\[universal-argument] the text portion of the message is used.
With two \\[universal-argument]'s the header portion of the message is used.
With three \\[universal-argument]'s the visible header portion of the message
  plus the text portion is used.

When invoked on marked messages (via vm-next-command-uses-marks),
each marked message is successively piped to the shell command,
one message per command invocation.

Output, if any, is displayed.  The message is not altered." t nil)

(autoload (quote vm-print-message) "vm-save" "Print the current message
Prefix arg N means print the current message and the next N - 1 messages.
Prefix arg -N means print the current message and the previous N - 1 messages.

The variables `vm-print-command' controls what command is run to
print the message, and `vm-print-command-switches' is a list of switches
to pass to the command.

When invoked on marked messages (via vm-next-command-uses-marks),
each marked message is printed, one message per vm-print-command invocation.

Output, if any, is displayed.  The message is not altered." t nil)

(autoload (quote vm-isearch-forward) "vm-search" "Incrementally search forward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally.  Specifying a prefix ARG interactively toggles the value of
vm-search-using-regexps for this search." t nil)

(autoload (quote vm-isearch-backward) "vm-search" "Incrementally search backward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally.  Specifying a prefix ARG interactively toggles the value of
vm-search-using-regexps for this search." t nil)

(autoload (quote vm-isearch) "vm-search" nil nil nil)

(autoload (quote vm-isearch-widen) "vm-search" nil nil nil)

(autoload (quote vm-isearch-narrow) "vm-search" nil nil nil)

(autoload (quote vm-isearch-update) "vm-search" nil nil nil)

(autoload (quote vm-move-message-forward) "vm-sort" "Move a message forward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT messages forward.
A negative COUNT causes movement to be backward instead of forward.
COUNT defaults to 1.  The current message remains selected after being
moved.

If vm-move-messages-physically is non-nil, the physical copy of
the message in the folder is moved.  A nil value means just
change the presentation order and leave the physical order of
the folder undisturbed." t nil)

(autoload (quote vm-move-message-backward) "vm-sort" "Move a message backward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT
messages backward.  A negative COUNT causes movement to be
forward instead of backward.  COUNT defaults to 1.  The current
message remains selected after being moved.

If vm-move-messages-physically is non-nil, the physical copy of
the message in the folder is moved.  A nil value means just
change the presentation order and leave the physical order of
the folder undisturbed." t nil)

(autoload (quote vm-move-message-forward-physically) "vm-sort" "Like vm-move-message-forward but always move the message physically." t nil)

(autoload (quote vm-move-message-backward-physically) "vm-sort" "Like vm-move-message-backward but always move the message physically." t nil)

(autoload (quote vm-physically-move-message) "vm-sort" nil nil nil)

(autoload (quote vm-so-sortable-datestring) "vm-sort" nil nil nil)

(autoload (quote vm-so-sortable-subject) "vm-sort" nil nil nil)

(autoload (quote vm-sort-messages) "vm-sort" "Sort message in a folder by the specified KEYS.
You may sort by more than one particular message key.  If
messages compare equal by the first key, the second key will be
compared and so on.  When called interactively the keys will be
read from the minibuffer.  Valid keys are

\"date\"		\"reversed-date\"
\"author\"		\"reversed-author\"
\"subject\"		\"reversed-subject\"
\"recipients\"		\"reversed-recipients\"
\"line-count\"		\"reversed-line-count\"
\"byte-count\"		\"reversed-byte-count\"
\"physical-order\"	\"reversed-physical-order\"

Optional second arg (prefix arg interactively) means the sort
should change the physical order of the messages in the folder.
Normally VM changes presentation order only, leaving the
folder in the order in which the messages arrived." t nil)

(autoload (quote vm-sort-compare-xxxxxx) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-thread) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-author) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-author-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-date) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-date-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-recipients) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-recipients-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-subject) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-subject-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-line-count) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-line-count-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-byte-count) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-byte-count-r) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-physical-order) "vm-sort" nil nil nil)

(autoload (quote vm-sort-compare-physical-order-r) "vm-sort" nil nil nil)

(autoload (quote vm) "vm-startup" "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox causes any contents of the system mailbox to
be moved and appended to the resulting buffer.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' expunges deleted messages and saves the buffered folder to
disk.

See the documentation for vm-mode for more information." t nil)

(autoload (quote vm-other-frame) "vm-startup" "Like vm, but run in a newly created frame." t nil)

(autoload (quote vm-other-window) "vm-startup" "Like vm, but run in a different window." t nil)

(autoload (quote vm-mode) "vm-startup" "Major mode for reading mail.

This is VM 6.19.

Commands:
   h - summarize folder contents
 C-t - toggle threads display

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
   ^ - go to parent of this message
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - expose message body or scroll forward a page
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message

   d - delete message, prefix arg deletes messages forward
 C-d - delete message, prefix arg deletes messages backward
   u - undelete
   k - flag for deletion all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text from the current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       messages into the current folder.

   G - sort messages by various keys

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder

   e - edit the current message
   j - discard cached information about the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, does not expunge
   A - save unfiled messages to their vm-auto-folder-alist specified folders
   # - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are not expunged, folder is
       saved to disk if it is modified.  new messages are changed
       to be flagged as just unread.
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so.  These commands
       apply and remove marks to messages.

       M M - mark the current message
       M U - unmark the current message
       M m - mark all messages
       M u - unmark all messages
       M C - mark messages matched by a virtual folder selector
       M c - unmark messages matched by a virtual folder selector
       M T - mark thread tree rooted at the current message
       M t - unmark thread tree rooted at the current message
       M S - mark messages with the same subject as the current message
       M s - unmark messages with the same subject as the current message
       M A - mark messages with the same author as the current message
       M a - unmark messages with the same author as the current message
       M R - mark messages within the point/mark region in the summary
       M r - unmark messages within the point/mark region in the summary

       M ? - partial help for mark commands

 W S - save the current window configuration to a name
 W D - delete a window configuration
 W W - apply a configuration
 W ? - help for the window configuration commands

 V V - visit a virtual folder (must be defined in vm-virtual-folder-alist)
 V C - create a virtual folder composed of a subset of the
       current folder's messages.
 V A - apply the selectors of a named virtual folder to the
       messages in the current folder and create a virtual folder
       containing the selected messages.
 V M - toggle whether this virtual folder's messages mirror the
       underlying real messages' attributes.
 V ? - help for virtual folder commands

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes and labels.  Expunges,
             message edits, and saves cannot be undone.  C-x u is
             also bound to this command.

   a - set message attributes

 l a - add labels to message
 l d - delete labels from message

   L - reload your VM init file, ~/.vm

   % - change a folder to another type

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Use M-x vm-submit-bug-report to submit a bug report.

Variables:
   vm-arrived-message-hook
   vm-arrived-messages-hook
   vm-auto-center-summary
   vm-auto-decode-mime-messages
   vm-auto-displayed-mime-content-types
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-get-new-mail
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-burst-digest-messages-inherit-labels
   vm-check-folder-types
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-convert-folder-types
   vm-crash-box
   vm-crash-box-suffix
   vm-default-folder-type
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-burst-type
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-digest-send-type
   vm-display-buffer-hook
   vm-display-using-mime
   vm-edit-message-hook
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forward-message-hook
   vm-forwarded-headers
   vm-forwarding-digest-type
   vm-forwarding-subject-format
   vm-frame-parameter-alist
   vm-frame-per-completion
   vm-frame-per-composition
   vm-frame-per-edit
   vm-frame-per-folder
   vm-frame-per-summary
   vm-highlighted-header-face
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-image-directory
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-discard-header-regexp
   vm-included-text-headers
   vm-included-text-prefix
   vm-invisible-header-regexp
   vm-jump-to-new-messages
   vm-jump-to-unread-messages
   vm-keep-crash-boxes
   vm-keep-sent-messages
   vm-mail-check-interval
   vm-mail-header-from
   vm-mail-mode-hook
   vm-make-crash-box-name
   vm-make-spool-file-name
   vm-mime-8bit-composition-charset
   vm-mime-8bit-text-transfer-encoding
   vm-mime-alternative-select-method
   vm-mime-attachment-auto-type-alist
   vm-mime-attachment-save-directory
   vm-mime-avoid-folding-content-type
   vm-mime-base64-decoder-program
   vm-mime-base64-decoder-switches
   vm-mime-base64-encoder-program
   vm-mime-base64-encoder-switches
   vm-mime-button-face
   vm-mime-charset-font-alist
   vm-mime-default-face-charsets
   vm-mime-digest-discard-header-regexp
   vm-mime-digest-headers
   vm-mime-display-function
   vm-mime-external-content-types-alist
   vm-mime-internal-content-types
   vm-mime-max-message-size
   vm-mode-hook
   vm-mosaic-program
   vm-move-after-deleting
   vm-move-after-killing
   vm-move-after-undeleting
   vm-move-messages-physically
   vm-mutable-frames
   vm-mutable-windows
   vm-netscape-program
   vm-pop-bytes-per-session
   vm-pop-max-message-size
   vm-pop-md5-program
   vm-pop-messages-per-session
   vm-popup-menu-on-mouse-3
   vm-preferences-file
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-quit-hook
   vm-recognize-pop-maildrops
   vm-reply-hook
   vm-reply-ignored-addresses
   vm-reply-ignored-reply-tos
   vm-reply-subject-prefix
   vm-resend-bounced-discard-header-regexp
   vm-resend-bounced-headers
   vm-resend-bounced-message-hook
   vm-resend-discard-header-regexp
   vm-resend-headers
   vm-resend-message-hook
   vm-retrieved-spooled-mail-hook
   vm-rfc1153-digest-discard-header-regexp
   vm-rfc1153-digest-headers
   vm-rfc934-digest-discard-header-regexp
   vm-rfc934-digest-headers
   vm-search-using-regexps
   vm-select-message-hook
   vm-select-new-message-hook
   vm-select-unread-message-hook
   vm-send-digest-hook
   vm-send-using-mime
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-file-suffixes
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-arrow
   vm-summary-format
   vm-summary-highlight-face
   vm-summary-mode-hook
   vm-summary-redo-hook
   vm-summary-show-threads
   vm-summary-thread-indent-level
   vm-tale-is-an-idiot
   vm-temp-file-directory
   vm-trust-From_-with-Content-Length
   vm-undisplay-buffer-hook
   vm-unforwarded-header-regexp
   vm-url-browser
   vm-url-search-limit
   vm-use-menus
   vm-use-toolbar
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-folder-hook
   vm-visit-when-saving
   vm-warp-mouse-to-new-frame
   vm-window-configuration-file
" t nil)

(autoload (quote vm-visit-folder) "vm-startup" "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder." t nil)

(autoload (quote vm-visit-folder-other-frame) "vm-startup" "Like vm-visit-folder, but run in a newly created frame." t nil)

(autoload (quote vm-visit-folder-other-window) "vm-startup" "Like vm-visit-folder, but run in a different window." t nil)

(autoload (quote vm-virtual-mode) "vm-startup" "Mode for reading multiple mail folders as one folder.

The commands available are the same commands that are found in
vm-mode, except that a few of them are not applicable to virtual
folders.

vm-virtual-mode is not a normal major mode.  If you run it, it
will not do anything.  The entry point to vm-virtual-mode is
vm-visit-virtual-folder." nil nil)

(autoload (quote vm-visit-virtual-folder) "vm-startup" nil t nil)

(autoload (quote vm-visit-virtual-folder-other-frame) "vm-startup" "Like vm-visit-virtual-folder, but run in a newly created frame." t nil)

(autoload (quote vm-visit-virtual-folder-other-window) "vm-startup" "Like vm-visit-virtual-folder, but run in a different window." t nil)

(autoload (quote vm-mail) "vm-startup" "Send a mail message from within VM, or from without." t nil)

(autoload (quote vm-mail-other-frame) "vm-startup" "Like vm-mail, but run in a newly created frame." t nil)

(autoload (quote vm-mail-other-window) "vm-startup" "Like vm-mail, but run in a different window." t nil)

(autoload (quote vm-submit-bug-report) "vm-startup" "Submit a bug report, with pertinent information to the VM bug list." t nil)

(autoload (quote vm-load-init-file) "vm-startup" nil t nil)

(autoload (quote vm-check-emacs-version) "vm-startup" nil nil nil)

(autoload (quote vm-set-debug-flags) "vm-startup" nil nil nil)

(autoload (quote vm-session-initialization) "vm-startup" nil nil nil)

(autoload (quote vm-summary-mode-internal) "vm-summary" nil nil nil)

(autoload (quote vm-summarize) "vm-summary" "Summarize the contents of the folder in a summary buffer. 
The format is as described by the variable vm-summary-format.  Generally
one line per message is most pleasing to the eye but this is not
mandatory." t nil)

(autoload (quote vm-summarize-other-frame) "vm-summary" "Like vm-summarize, but run in a newly created frame." t nil)

(autoload (quote vm-do-summary) "vm-summary" nil nil nil)

(autoload (quote vm-do-needed-summary-rebuild) "vm-summary" nil nil nil)

(autoload (quote vm-update-message-summary) "vm-summary" nil nil nil)

(autoload (quote vm-set-summary-pointer) "vm-summary" nil nil nil)

(autoload (quote vm-summary-highlight-region) "vm-summary" nil nil nil)

(autoload (quote vm-auto-center-summary) "vm-summary" nil nil nil)

(autoload (quote vm-sprintf) "vm-summary" nil nil nil)

(autoload (quote vm-tokenized-summary-insert) "vm-summary" nil nil nil)

(autoload (quote vm-compile-format) "vm-summary" nil nil nil)

(autoload (quote vm-get-header-contents) "vm-summary" nil nil nil)

(autoload (quote vm-left-justify-string) "vm-summary" nil nil nil)

(autoload (quote vm-right-justify-string) "vm-summary" nil nil nil)

(autoload (quote vm-truncate-string) "vm-summary" nil nil nil)

(autoload (quote vm-su-attribute-indicators) "vm-summary" nil nil nil)

(autoload (quote vm-su-attribute-indicators-long) "vm-summary" nil nil nil)

(autoload (quote vm-su-byte-count) "vm-summary" nil nil nil)

(autoload (quote vm-su-weekday) "vm-summary" nil nil nil)

(autoload (quote vm-su-monthday) "vm-summary" nil nil nil)

(autoload (quote vm-su-month) "vm-summary" nil nil nil)

(autoload (quote vm-su-month-number) "vm-summary" nil nil nil)

(autoload (quote vm-su-year) "vm-summary" nil nil nil)

(autoload (quote vm-su-hour-short) "vm-summary" nil nil nil)

(autoload (quote vm-su-hour) "vm-summary" nil nil nil)

(autoload (quote vm-su-zone) "vm-summary" nil nil nil)

(autoload (quote vm-su-mark) "vm-summary" nil nil nil)

(autoload (quote vm-grok-From_-date) "vm-summary" nil nil nil)

(autoload (quote vm-parse-date) "vm-summary" nil nil nil)

(autoload (quote vm-su-do-date) "vm-summary" nil nil nil)

(autoload (quote vm-su-do-month) "vm-summary" nil nil nil)

(autoload (quote vm-run-user-summary-function) "vm-summary" nil nil nil)

(autoload (quote vm-su-full-name) "vm-summary" nil nil nil)

(autoload (quote vm-su-interesting-full-name) "vm-summary" nil nil nil)

(autoload (quote vm-su-from) "vm-summary" nil nil nil)

(autoload (quote vm-su-interesting-from) "vm-summary" nil nil nil)

(autoload (quote vm-grok-From_-author) "vm-summary" nil nil nil)

(autoload (quote vm-su-do-author) "vm-summary" nil nil nil)

(autoload (quote vm-default-chop-full-name) "vm-summary" nil nil nil)

(autoload (quote vm-choose-chop-full-name-function) "vm-summary" nil nil nil)

(autoload (quote vm-su-do-recipients) "vm-summary" nil nil nil)

(autoload (quote vm-su-to) "vm-summary" nil nil nil)

(autoload (quote vm-su-to-names) "vm-summary" nil nil nil)

(autoload (quote vm-su-message-id) "vm-summary" nil nil nil)

(autoload (quote vm-su-line-count) "vm-summary" nil nil nil)

(autoload (quote vm-su-subject) "vm-summary" nil nil nil)

(autoload (quote vm-su-summary) "vm-summary" nil nil nil)

(autoload (quote vm-fix-my-summary!!!) "vm-summary" nil t nil)

(autoload (quote vm-su-thread-indent) "vm-summary" nil nil nil)

(autoload (quote vm-su-labels) "vm-summary" nil nil nil)

(autoload (quote vm-toggle-threads-display) "vm-thread" "Toggle the threads display on and off.
When the threads display is on, the folder will be sorted by
thread and thread indentation (via the %I summary format specifier)
will be visible." t nil)

(autoload (quote vm-build-threads) "vm-thread" nil nil nil)

(autoload (quote vm-thread-mark-for-summary-update) "vm-thread" nil nil nil)

(autoload (quote vm-thread-list) "vm-thread" nil nil nil)

(autoload (quote vm-unthread-message) "vm-thread" nil nil nil)

(autoload (quote vm-th-parent) "vm-thread" nil nil nil)

(autoload (quote vm-th-thread-indentation) "vm-thread" nil nil nil)

(autoload (quote vm-th-thread-list) "vm-thread" nil nil nil)

(autoload (quote vm-toolbar-helper-command) "vm-toolbar" nil t nil)

(autoload (quote vm-toolbar-any-messages-p) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-delete/undelete-message) "vm-toolbar" nil t nil)

(autoload (quote vm-toolbar-can-autofile-p) "vm-toolbar" nil t nil)

(autoload (quote vm-toolbar-autofile-message) "vm-toolbar" nil t nil)

(autoload (quote vm-toolbar-can-recover-p) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-can-decode-mime-p) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-can-quit-p) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-mail-waiting-p) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-update-toolbar) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-install-toolbar) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-make-toolbar-spec) "vm-toolbar" nil nil nil)

(autoload (quote vm-toolbar-initialize) "vm-toolbar" nil nil nil)

(autoload (quote vm-set-buffer-modified-p) "vm-undo" nil nil nil)

(autoload (quote vm-undo-boundary) "vm-undo" nil nil nil)

(autoload (quote vm-clear-expunge-invalidated-undos) "vm-undo" nil nil nil)

(autoload (quote vm-clear-virtual-quit-invalidated-undos) "vm-undo" nil nil nil)

(autoload (quote vm-clear-modification-flag-undos) "vm-undo" nil nil nil)

(autoload (quote vm-squeeze-consecutive-undo-boundaries) "vm-undo" nil nil nil)

(autoload (quote vm-undo-record) "vm-undo" nil nil nil)

(autoload (quote vm-undo-describe) "vm-undo" nil nil nil)

(autoload (quote vm-undo-set-message-pointer) "vm-undo" nil nil nil)

(autoload (quote vm-undo) "vm-undo" "Undo last change to message attributes in the current folder.
Consecutive invocations of this command cause sequentially earlier
changes to be undone.  After an intervening command between undos,
the undos themselves become undoable." t nil)

(autoload (quote vm-set-message-attributes) "vm-undo" "Set message attributes.
Use this command to change attributes like `deleted' or
`replied'.  Interactively you will be prompted for the attributes
to be changed, and only the attributes you enter will be altered.
You can use completion to expand the attribute names.  The names
should be entered as a space separated list.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have their attributes altered.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one." t nil)

(autoload (quote vm-add-message-labels) "vm-undo" "Attach some labels to a message.
These are arbitrary user-defined labels, not to be confused with
message attributes like `new' and `deleted'.  Interactively you
will be prompted for the labels to be added.  You can use
completion to expand the label names, with the completion list
being all the labels that have ever been used in this folder.
The names should be entered as a space separated list.  Label
names are compared case-insensitively.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have the labels added.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one." t nil)

(autoload (quote vm-delete-message-labels) "vm-undo" "Delete some labels from a message.
These are arbitrary user-defined labels, not to be confused with
message attributes like `new' and `deleted'.  Interactively you
will be prompted for the labels to be deleted.  You can use
completion to expand the label names, with the completion list
being all the labels that have ever been used in this folder.
The names should be entered as a space separated list.  Label
names are compared case-insensitively.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have the labels deleted.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one." t nil)

(autoload (quote vm-add-or-delete-message-labels) "vm-undo" nil nil nil)

(autoload (quote vm-set-xxxx-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-labels) "vm-undo" nil nil nil)

(autoload (quote vm-set-new-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-unread-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-deleted-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-filed-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-replied-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-written-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-forwarded-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-redistributed-flag) "vm-undo" nil nil nil)

(autoload (quote vm-set-new-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-unread-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-deleted-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-filed-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-replied-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-written-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-forwarded-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-redistributed-flag-of) "vm-undo" nil nil nil)

(autoload (quote vm-set-deleted-flag-in-vector) "vm-undo" nil nil nil)

(autoload (quote vm-set-new-flag-in-vector) "vm-undo" nil nil nil)

(autoload (quote vm-user-composition-folder-buffer) "vm-user" "Returns the folder buffer associated with the current buffer.
The current buffer must be a composition buffer created by VM for
a reply, resend or forward.

Nil is returned if the current buffer is not assocaited with any
VM folder.

Note that the buffer returned might be a virtual folder buffer,
which might have several underlying real folders associated with
it.  To get the list of real folder buffers associated with a
composition buffer, use vm-user-composition-real-folder-buffers
instead." nil nil)

(autoload (quote vm-user-composition-real-folder-buffers) "vm-user" "Returns a list of the real folder buffers associated with the current
buffer.  The current buffer must be a composition buffer created
by VM for a reply, resend or forward." nil nil)

(autoload (quote vm-spool-files) "vm-vars" nil nil nil)

(autoload (quote vm-version) "vm-version" "Returns the value of the variable vm-version." nil nil)

(autoload (quote vm-build-virtual-message-list) "vm-virtual" nil nil nil)

(autoload (quote vm-create-virtual-folder) "vm-virtual" "Create a new virtual folder from messages in the current folder.
The messages will be chosen by applying the selector you specify,
which is normally read from the minibuffer.

Prefix arg means the new virtual folder should be visited read only." t nil)

(autoload (quote vm-apply-virtual-folder) "vm-virtual" "Apply the selectors of a named virtual folder to the current folder
and create a virtual folder containing the selected messages.

Prefix arg means the new virtual folder should be visited read only." t nil)

(autoload (quote vm-toggle-virtual-mirror) "vm-virtual" nil t nil)

(autoload (quote vm-virtual-help) "vm-virtual" nil t nil)

(autoload (quote vm-vs-or) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-and) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-not) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-any) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-author) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-recipient) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-subject) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-sent-before) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-sent-after) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-header) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-label) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-text) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-more-chars-than) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-less-chars-than) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-more-lines-than) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-less-lines-than) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-new) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-unread) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-read) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-deleted) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-replied) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-forwarded) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-filed) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-written) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-marked) "vm-virtual" nil nil nil)

(autoload (quote vm-vs-edited) "vm-virtual" nil nil nil)

(autoload (quote vm-read-virtual-selector) "vm-virtual" nil nil nil)

(autoload (quote vm-virtual-quit) "vm-virtual" nil nil nil)

(autoload (quote vm-virtual-save-folder) "vm-virtual" nil nil nil)

(autoload (quote vm-virtual-get-new-mail) "vm-virtual" nil nil nil)

(autoload (quote vm-make-virtual-copy) "vm-virtual" nil nil nil)

(autoload (quote vm-display) "vm-window" nil nil nil)

(autoload (quote vm-display-buffer) "vm-window" nil nil nil)

(autoload (quote vm-undisplay-buffer) "vm-window" nil nil nil)

(autoload (quote vm-load-window-configurations) "vm-window" nil nil nil)

(autoload (quote vm-store-window-configurations) "vm-window" nil nil nil)

(autoload (quote vm-set-window-configuration) "vm-window" nil nil nil)

(autoload (quote vm-record-current-window-configuration) "vm-window" nil nil nil)

(autoload (quote vm-save-window-configuration) "vm-window" "Name and save the current window configuration.
With this command you associate the current window setup with an
action.  Each time you perform this action VM will duplicate this
window setup.

Nearly every VM command can have a window configuration
associated with it.  VM also allows some category configurations,
`startup', `reading-message', `composing-message', `editing-message',
`marking-message' and `searching-message' for the commands that
do these things.  There is also a `default' configuration that VM
will use if no other configuration is applicable.  Command
specific configurations are searched for first, then the category
configurations and then the default configuration.  The first
configuration found is the one that is applied.

The value of vm-mutable-windows must be non-nil for VM to use
window configurations.

If vm-mutable-frames is non-nil and Emacs is running under X
windows, then VM will use all existing frames.  Otherwise VM will
restrict its changes to the frame in which it was started." t nil)

(autoload (quote vm-buffer-to-label) "vm-window" nil nil nil)

(autoload (quote vm-delete-window-configuration) "vm-window" "Delete the configuration saved for a particular action.
This action will no longer have an associated window configuration.
The action will be read from the minibuffer." t nil)

(autoload (quote vm-apply-window-configuration) "vm-window" "Change the current window configuration to be one
associated with a particular action.  The action will be read
from the minibuffer." t nil)

(autoload (quote vm-window-help) "vm-window" nil t nil)

(autoload (quote vm-iconify-frame) "vm-window" "Iconify the current frame.
Run the hooks in vm-iconify-frame-hook before doing so." t nil)

(autoload (quote vm-window-loop) "vm-window" nil nil nil)

(autoload (quote vm-frame-loop) "vm-window" nil nil nil)

(autoload (quote vm-delete-windows-or-frames-on) "vm-window" nil nil nil)

(autoload (quote vm-replace-buffer-in-windows) "vm-window" nil nil nil)

(autoload (quote vm-bury-buffer) "vm-window" nil nil nil)

(autoload (quote vm-unbury-buffer) "vm-window" nil nil nil)

(autoload (quote vm-get-buffer-window) "vm-window" nil nil nil)

(autoload (quote vm-get-visible-buffer-window) "vm-window" nil nil nil)

(autoload (quote vm-set-hooks-for-frame-deletion) "vm-window" nil nil nil)

(autoload (quote vm-created-this-frame-p) "vm-window" nil nil nil)

(autoload (quote vm-delete-buffer-frame) "vm-window" nil nil nil)

(autoload (quote vm-register-frame) "vm-window" nil nil nil)

(autoload (quote vm-goto-new-frame) "vm-window" nil nil nil)

(autoload (quote vm-goto-new-summary-frame-maybe) "vm-window" nil nil nil)

(autoload (quote vm-goto-new-folder-frame-maybe) "vm-window" nil nil nil)

(autoload (quote vm-warp-mouse-to-frame-maybe) "vm-window" nil nil nil)

(autoload (quote vm-iconify-frame-xxx) "vm-window" nil nil nil)
