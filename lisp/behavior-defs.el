;;; behavior-defs.el --- definitions of specific behaviors

;; Copyright (C) 2000, 2001, 2002 Ben Wing.

;; Author: Ben Wing
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

;;; Synched up with: Not in FSF.

;;; Authorship:

;; Created July 2000 by Ben Wing.

;;; Commentary:

;; This file will be dumped with XEmacs.

;;; Code:

(require 'behavior)

(define-behavior 'scroll-in-place
"This package provides improved vertical scrolling commands for XEmacs.
These new commands offer the following features:

+ When a scrolling command is executed, XEmacs tries to keep point as
  close as possible to its original window position (window line and
  column).  This is what \"scroll in place\" means: point stays \"in place\"
  within the window.  (There are times when point must be moved from its
  original window position in order to execute the scroll; see below.)

  The variable `scroll-in-place', which is true by default, determines
  whether or not the standard XEmacs scrolling commands (`scroll-down',
  `scroll-up', `scroll-other-window-down', and `scroll-other-window') use
  the \"in place\" features listed here.  When `scroll-in-place' is `nil'
  the standard XEmacs scrolling commands essentially just call the
  original versions of themselves.  (Note that even when `scroll-in-place'
  is `nil' the new versions of `scroll-down' and `scroll-up' have slightly
  different behavior when a minibuffer window is the selected window.  See
  below.)

  It is possible to turn off (or turn on) \"in place\" scrolling for certain
  buffers by making buffer-local bindings of the variable `scroll-in-
  place' for those buffers.  The variable `scroll-in-place' is not usually
  buffer-local, but you can make it so if you desire.

+ Because the improved scrolling commands keep point at its original
  window position, these scrolling commands are \"reversible.\"  The
  `scroll-up' command undoes the effect of the immediately previous
  `scroll-down' command (if any) and vice versa.  In other words, if you
  scroll up and then immediately scroll back down, the window config-
  uration is restored to its exact original state.  This allows you to
  browse through a buffer more easily, as you can always get back to the
  original configuration.

  Note, however, that the improved scrolling commands are guaranteed to be
  reversible only if there are no intervening non-scrolling commands.
  Also, if you give a prefix argument to a scrolling command (in order to
  specify the number of lines to scroll by), previous scrolling commands
  may no longer be reversible.  More specifically, if the new prefix
  argument has a different magnitude than the previous scrolling distance,
  then any previous scrolling commands are not reversible.  The new prefix
  argument takes precedence.

  You might find it useful to think of the scrolling commands as forming
  \"chains.\"  A scrolling command either starts or continues a chain.  By
  issuing a non-scrolling command or by changing the number of lines to be
  scrolled, you break the chain.  (Note that simply changing the scrolling
  direction won't break the chain; changing the absolute number of lines
  to be scrolled is what breaks the chain.)  Scrolling commands are
  guaranteed to be reversible only within the current chain.  Hopefully
  that's clear enough.

+ When a scrolling command is given a prefix argument (which specifies the
  number of lines to scroll by), then that argument becomes the default
  scrolling distance for all immediately subsequent scrolling commands.
  This means that you can easily set the scrolling distance for a chain
  of scrolling commands.  Note that a new prefix argument or any non-
  scrolling command breaks the chain (as described above), and any further
  scrolling commands will use the usual defaults (or the prefix argument
  you specify at that time, of course).

  However, there are cases in which one doesn't want the current scrolling
  command to use the default scrolling distance that was set by the
  previous scrolling command.  For example, suppose that you had special
  commands that scrolled one line up and one line down.  When you invoke
  one of these commands, the \"in place\" scrolling routines set the default
  scrolling distance to be just one line.  Now suppose that you use one of
  your special commands and then immediately invoke `scroll-up' (`C-v'),
  expecting it to scroll by a near windowful of text.  You would be
  disappointed --- because the previous command set the default scrolling
  distance to be just one line, `scroll-up' just scrolls by one line.

  To solve this problem, \"scroll-in-place\" allows you to divide scrolling
  commands into separate \"groups.\"  Commands in a group can only form
  chains with (and therefore, inherit defaults from) commands in the same
  group.  (Note that no command can be in more than one group.)  If you
  invoke a scrolling command that is not in the same group as that of the
  immediately previous scrolling command, then the previous chain is
  broken and you start a new chain --- with a new set of defaults.

  So to solve the problem described above, you could put your one-line
  scrolling commands in their own group.  Once that is done, the standard
  scrolling commands will not form chains with your one-line scrolling
  commands, and therefore will not use the default scrolling distance set
  by those commands.  Problem solved!

  By default, all \"in place\" scrolling commands are in a single group.  If
  you want to partition some commands into separate groups, you must do
  that yourself *before* any \"in place\" commands are invoked.  For more
  information about grouping commands, see the documentation for the
  variables `scroll-command-groups' and `scroll-default-command-group'.

+ The improved scrolling commands will avoid displaying empty lines past
  the end of the buffer when possible.  In other words, just as you can't
  see \"dead space\" before the beginning of the buffer text, the new
  scrolling commands try to avoid displaying \"dead space\" past the end of
  the buffer text.  This behavior is somewhat configurable; see the
  documentation for the variable `scroll-allow-blank-lines-past-eob'.

  Dead space will be displayed if it is necessary in order to make a
  previous scrolling action reversible, however.

+ If the scrolling commands cannot keep point at its initial window
  position (because a buffer boundary is on screen and the window can't be
  scrolled as far as necessary to keep point at the right place), point is
  allowed to temporarily stray from its initial window position.  That is,
  point moves the correct number of window lines, even if it means that it
  has to stray from its desired window position.  This straying is undone
  when (and if) the scrolling action is reversed.

+ If a scrolling command tries to move point past a buffer boundary, point
  is instead moved to the boundary (the beginning or the end of the buffer
  as appropriate) and an appropriate message is displayed.  This motion is
  reversible, of course.

  However, if point was already at the buffer boundary when the scrolling
  command was invoked, the command signals an appropriate error instead.

+ When a minibuffer window is the selected window, the new versions of
  `scroll-up' and `scroll-down' either scroll the window in the variable
  `minibuffer-scroll-window' (which is usually the window of completions)
  or the `next-window' if there is no `minibuffer-scroll-window'.  This is
  usually much more useful than scrolling the minibuffer itself.  (Note
  that this feature is available even when the variable `scroll-in-place'
  is `nil'.)

+ When a scrolling command is scrolling a window other than the selected
  window, it will signal an appropriate buffer boundary error if the
  window cannot be scrolled (because the appropriate buffer boundary is
  already visible).  This means that an error is signalled even in cases
  that would be allowed (by \"straying\" point or by moving it to the buffer
  boundary) if the window were selected.

  (If an error were not signalled in these cases, then there would be many
  cases in which the last scroll in a particular direction would appear to
  do nothing because only the point position would change --- the
  displayed text would stay the same!  To avoid these cases the scrolling
  commands signal boundary errors \"prematurely\" when the window to be
  scrolled is not selected.)"
  :short-doc "Keep cursor on same line when scrolling"
  :require 'scroll-in-place
  :enable #'turn-on-scroll-in-place
  :disable #'turn-off-scroll-in-place)

(define-behavior 'mouse-avoidance
"For those who are annoyed by the mouse pointer obscuring text,
this mode moves the mouse pointer - either just a little out of
the way, or all the way to the corner of the frame. 

Customize `mouse-avoidance-mode' to one of the symbols `banish',
`exile', `jump', `animate', `cat-and-mouse', `proteus', or `none'.

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
definition of \"random distance\".)"
  :short-doc "Keep mouse away from cursor"
  :enable #'(lambda ()
	       (mouse-avoidance-mode 'animate))
  :disable #'(lambda ()
	       (mouse-avoidance-mode 'none)))

(define-behavior 'jka-compr
  "This package implements low-level support for reading, writing,
and loading compressed files.  It hooks into the low-level file
I/O functions (including write-region and insert-file-contents) so
that they automatically compress or uncompress a file if the file
appears to need it (based on the extension of the file name).
Packages like Rmail, VM, GNUS, and Info should be able to work
with compressed files without modification."
  :short-doc "Transparently handle compressed files"
  :enable #'jka-compr-install
  :disable #'jka-compr-uninstall)

(define-behavior 'efs
"EFS is a system for transparent file-transfer between remote VMS, CMS,
MTS, MVS, Twenex, Explorer (the last two are Lisp machines), TOPS-20,
DOS (running the Distinct, Novell, FTP software, NCSA, Microsoft in both
unix and DOS mode, Super TCP, and Hellsoft FTP servers), Windows NT
\(running the Microsoft or Hummingbird ftp servers), Unix descriptive
listings (dl), KA9Q, OS/2 hosts using FTP. This means that you can edit,
copy and otherwise manipulate files on any machine you have access to
from within Emacs as if it were a local file. EFS works by introducing
an extended filename syntax, and overloading functions such as
`insert-file-contents' so that accessing a remote file causes
appropriate commands to be sent to an FTP process.

The syntax to use is like this:

\(for anonymous:)     /ftp.xemacs.org:/pub/xemacs/
\(for non-anonymous:) /ben@gwyn.tux.org:/etc/mail/xemacs/aliases-xemacs

You can specify either a file or a directory (in the latter case,
Dired will be brought up).  All operations in XEmacs on such files
should work exactly as on any other files, modulo the additional
slowness."
  :short-doc "Transparent file access over FTP"
  :require 'efs-auto
  :enable #'ignore
  ;; can't :disable
  )


(define-behavior 'resize-minibuffer
  "When this behavior is enabled, the minibuffer is dynamically resized to
contain the entire region of text put in it as you type.

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
counterparts."
  :short-doc "Resize minibuffer automatically"
  :enable #'(lambda ()
	      (resize-minibuffer-mode 1))
  :disable #'(lambda ()
	       (resize-minibuffer-mode -1)))

(define-behavior 'func-menu
  "Suppose you have a file with a lot of functions in it. Well, this
package makes it easy to jump to any of those functions. The names of
the functions in the current buffer are automatically put into menubar
menu, you select one of the function-names and the point is moved to
that very function. The mark is pushed on the mark-ring, so you can
easily go back to where you were. Alternatively, you can use enter the
name of the desired function via the minibuffer which offers
completing read input. In addition, the name of the function before
point is optionally displayed in the modeline."
  :short-doc "Add a menu of defined functions"
  :require 'func-menu
  :enable #'(lambda ()
	      (add-hook 'find-file-hooks 'fume-add-menubar-entry)
	      (mapc #'(lambda (buffer)
			(with-current-buffer buffer
			  (setq fume-display-in-modeline-p t)
			  (fume-add-menubar-entry)))
		    (buffer-list)))
  :disable #'(lambda ()
	       (remove-hook 'find-file-hooks 'fume-add-menubar-entry)
	       (fset 'widen (symbol-function 'fume-widen))
	       (fset 'narrow-to-region (symbol-function 'narrow-to-region))
	       (mapc #'(lambda (buffer)
			 (with-current-buffer buffer
			   (fume-remove-menubar-entry)
			   (setq fume-display-in-modeline-p nil)
			   (fume-remove-post-command-hook
			    'fume-tickle-modeline)
			   (fume-remove-post-command-hook
			    'fume-maybe-install-modeline-feature)
			   (fume-remove-post-command-hook
			    'fume-rescan-buffer-trigger)))
		     (buffer-list))))

(define-behavior 'mwheel
  "This code enables the use of the infamous 'wheel' on the new
crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
events are sent as button4/button5 events, which are automatically
set up to do scrolling in the expected way.  The actual way that the
scrolling works can be controlled by `mwheel-scroll-amount' and
`mwheel-follow-mouse'."
  :short-doc "Mouse wheel support for X Windows"
  :enable 'mwheel-install)

(define-behavior 'recent-files
"Recent-files adds the menu \"Recent Files\" (or whatever name you
choose, see \"Customization:\" below) to Emacs's menubar. Its
entries are the files (and directories) that have recently been
opened by Emacs. You can open one of these files again by
selecting its entry in the \"Recent Files\" menu. The list of file
entries in this menu is preserved from one Emacs session to
another. You can prevent Emacs from saving this list by selecting
\"Don't save recent-files list on exit\" from the menu. If you have
disabled saving, you can re-enable it by selecting \"Save
recent-files list on exit\".

The menu has permanent and non-permanent entries. Permanent
entries are marked with an asterisk in front of the filename. The
non-permanent entries are hidden in a submenu.

Each time you open a file in Emacs, it is added as a non-permanent
entry to the menu. The value of `recent-files-number-of-entries'
determines how many non-permanent entries are held in the
menu. When the number of non-permanent entries reaches this value,
the least recently added non-permanent entry is removed from the
menu when another non-permanent entry is added. It is not removed
from the list, though; it may reappear when entries are deleted
from the list. The number of entries saved to disk is the value of
the variable `recent-files-number-of-saved-entries'.

Permanent entries are not removed from the menu. You can make a
file entry permanent by selecting \"Make <buffer> permanent\" (where
<buffer> is the name of the current buffer) when the current
buffer holds this file. \"Make <buffer> non-permanent\" makes the
file entry of the current buffer non-permanent.

The command \"Kill buffer <buffer> and delete entry\" is handy when
you have accidently opened a file but want to keep neither the
buffer nor the entry.

You can erase the list of non-permanent entries by selecting
\"Erase non-permanent entries\" from the menu.

Customization:

There are lots of variables to control the behaviour of
recent-files. You do not have to change any of them if you like it
as it comes out of the box. However, you may want to look at these
options to make it behave different.

`recent-files-number-of-entries'
   Controls how many non-permanent entries are shown in the
   recent-files list.  The default is 15. 

`recent-files-number-of-saved-entries'
   Controls how many non-permanent entries are saved to disk when
   Emacs exits or recent-files-save-the-list is called. The
   default is 50.

`recent-files-save-file'
   The name of the file where the recent-files list is saved
   between Emacs session. You probably don't need to change this.
   The default is \".recent-files.el\" in your home directory.

`recent-files-dont-include'
   A list of regular expressions for files that should not be
   included into the recent-files list. This list is empty by
   default. For instance, a list to exclude all .newsrc
   files, all auto-save-files, and all files in the /tmp
   directory (but not the /tmp directory itself) would look
   like this:
        (setq recent-files-dont-include
              '(\"/\\.newsrc\" \"~$\" \"^/tmp/.\"))
   The default is empty.

`recent-files-use-full-names'
   If the value of this variable is non-nil, the full pathnames of
   the files are shown in the recent-files menu. Otherwise only
   the filename part (or the last name component if it is a
   directory) is shown in the menu. The default it t, i.e. show
   full names.

`recent-files-filename-replacements'
   This is a list of pairs of regular expressions and replacement
   strings. If a filename matches one of the regular expressions,
   the matching part is replaced by the replacement string for
   display in the recent-files menu.
   Example: My home directory is \"/users/mmc/nickel/\". I want to
   replace it with \"~/\". I also want to replace the directory
   \"/imports/teleservices/mmc/avc2/\", where I work a lot, with
   \".../avc2/\". The list then looks like
       (setq recent-files-filename-replacements
             '((\"/users/mmc/nickel/\" . \"~/\")
               (\"/imports/teleservices/mmc/avc2/\" . \".../avc2/\")))
   Only the first match is replaced. So, if you have several
   entries in this list that may match a filename simultaneously,
   put the one you want to match (usually the most special) in
   front of the others. The default is to replace the home
   directory with \"~\".

`recent-files-sort-function'
   Contains a function symbol to sort the display of filenames in
   the recent-files menu. Supplied are two functions,
   'recent-files-dont-sort and 'recent-files-sort-alphabetically.
   The first, which is the default, preserves the order of \"most
   recent on top\". 

`recent-files-permanent-submenu'
   If this variable is non-nil, the permanent entries are put into
   a separate submenu of the recent-files menu. The default is
   nil.

`recent-files-non-permanent-submenu'
   If this variable is non-nil, the non-permanent entries are put
   into a separate submenu of the recent-files menu. The default
   is nil. (You can set both `recent-files-permanent-submenu' and
   `recent-files-non-permanent-submenu' to t to have both lists in
   separate submenus.)

`recent-files-commands-submenu'
   If this variable is non-nil, the commands if recent-files are
   placed in a submenu of the recent-files menu. The default is
   nil.

`recent-files-commands-submenu-title'
   If the commands are placed in a submenu, this string is used as
   the title of the submenu. The default is \"Commands...\".

`recent-files-actions-on-top'
   If this variable is non-nil, the \"action\" menu entries (\"Make
   <buffer> permanent\" etc.) are put on top of the menu. Otherwise
   they appear below the file entries or submenus. The default is
   nil.

`recent-files-permanent-first'
   If this variable is t, the permanent entries are put first in
   the recent-files menu, i.e. above the non-permanent entries. If
   the value is nil, non-permanent entries appear first. If the
   value is neither t nor nil, the entries are sorted according to
   recent-files-sort-function. The default is 'sort.

`recent-files-find-file-command'
   This variable contains to commandto execute when a file entry
   is selected from the menu. Usually this will be `find-file',
   which is the default.

KNOWN BUG:
  - recent-files overwrites the recent-files-save-file
    unconditionally when Emacs exits. If you have two Emacs
    processes running, the one exiting later will overwrite the
    file without merging in the new entries from the other Emacs
    process. This can be avoided by disabling the save on exit from
    the menu."
  :short-doc "`Recent Files' menu"
  :enable 'recent-files-initialize)

(define-behavior 'filladapt
  "These functions enhance the default behavior of Emacs' Auto Fill
mode and the commands `fill-paragraph', `lisp-fill-paragraph',
`fill-region-as-paragraph' and `fill-region'.

The chief improvement is that the beginning of a line to be
filled is examined and, based on information gathered, an
appropriate value for fill-prefix is constructed.  Also the
boundaries of the current paragraph are located.  This occurs
only if the fill prefix is not already non-nil.

The net result of this is that blurbs of text that are offset
from left margin by asterisks, dashes, and/or spaces, numbered
examples, included text from USENET news articles, etc. are
generally filled correctly with no fuss."
  :short-doc "Adaptive (smart) filling"
  :require 'filladapt
  :enable  #'(lambda ()
	       (setq-default filladapt-mode t)
	       (mapc #'(lambda (buffer)
			 (with-current-buffer buffer
			   (unless filladapt-mode
			     (filladapt-mode 1))))
		     (buffer-list)))
  :disable #'(lambda ()
	       (setq-default filladapt-mode nil)
	       (mapc #'(lambda (buffer)
			 (with-current-buffer buffer
			   (when filladapt-mode
			     (filladapt-mode -1))))
		     (buffer-list))))
