;;; behavior-defs.el --- definitions of specific behaviors

;; Copyright (C) 2000, 2001 Ben Wing.

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

(define-behavior 'mouse-avoidance
  "Mouse avoidance mode"
  :title "Mouse Avoidance"
  :enable #'(lambda ()
	       (mouse-avoidance-mode 'animate))
  :disable #'(lambda ()
	       (mouse-avoidance-mode 'none)))

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
  :title "Resize Minibuffer Automatically"
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
  :title "Function Menu"
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
  :title "Mouse Wheel Support"
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
  :title "Recent Files Menu"
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
  :title "Adaptive Filling"
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
