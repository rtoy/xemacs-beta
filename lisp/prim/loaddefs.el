;;; loaddefs.el --- define standard autoloads of other files

;; Copyright (C) 1985, 1986, 1987, 1992-1995 Free Software Foundation, Inc.

;; Maintainer: XEmacs
;; Keywords: internal

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; Special formatting conventions are used in this file!
;;;
;;; a backslash-newline is used at the beginning of a documentation string
;;; when that string should be stored in the file lib-src/DOCnnn, not in core.
;;;
;;; Such strings read into Lisp as numbers (during the pure-loading phase).
;;;
;;; But you must obey certain rules to make sure the string is understood
;;; and goes into lib-src/DOCnnn properly.  Otherwise, the string will not go
;;; anywhere!
;;;
;;; The doc string must appear in the standard place in a call to
;;; defun, autoload, defvar or defconst.  No Lisp macros are recognized.
;;; The open-paren starting the definition must appear in column 0.
;;;
;;; In defvar and defconst, there is an additional rule:
;;; The double-quote that starts the string must be on the same
;;; line as the defvar or defconst.
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; **********************************************************************
;;; You should never need to write autoloads by hand and put them here.
;;;
;;; It is no longer necessary.  Instead use autoload.el to maintain them
;;; for you.  Just insert ";;;###autoload" before defuns or defmacros you
;;; want to be autoloaded, or other forms you want copied into loaddefs.el
;;; (defvars, key definitions, etc.).  For example, 
;;;	;;;###autoload
;;;	(defun foobar () ....)
;;;	;;;###autoload (define-key global-map "f" 'foobar)
;;;	;;;###autoload
;;;	(defvar foobar-var nil "\
;;;	This is foobar-var's doc-string.")
;;;
;;; Then do M-x update-file-autoloads on the file to update loaddefs.el.
;;;
;;; You can also use M-x update-directory-autoloads to update the autoloads
;;; in loaddefs.el for all .el files in the lisp/ directory, or M-x
;;; update-autoloads-here to update the autoloads for each file that
;;; already has an autoload section in this file.
;;; **********************************************************************

;;; Code:

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (mapcar 'purecopy
	      (if (eq system-type 'vax-vms)
		  '(".obj" ".elc" ".exe" ".bin" ".lbin" ".sbin"
		    ".dvi" ".toc" ".log" ".aux"
		    ".lof" ".brn" ".rnt" ".mem" ".lni" ".lis"
		    ".olb" ".tlb" ".mlb" ".hlb" ".glo" ".idx" ".lot" ".fmt")
		'(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
		  ".dvi" ".toc" ".log" ".aux" ".a" ".ln"
		  ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".fmt"
		  ".diff" ".oi"))))

(setq debug-ignored-errors
      '(beginning-of-line
	beginning-of-buffer
	end-of-line
        end-of-buffer
	end-of-file buffer-read-only
	"^Previous command was not a yank\\'"
	"^Minibuffer window is not active\\'"
	"^End of history; no next item\\'"
	"^Beginning of history; no preceding item\\'"
	"^No recursive edit is in progress\\'"
	"^Changes to be undone are outside visible portion of buffer\\'"
	"^No undo information in this buffer\\'"
	"^No further undo information\\'"
	"^Save not confirmed\\'"
	"^Recover-file cancelled\\.\\'"

	;; comint
	"^Not at command line\\'"
	"^Empty input ring\\'"
	"^No history\\'"
	"^Not found\\'";; To common?
	"^Current buffer has no process\\'"

	;; dabbrev
	"^No dynamic expansion for \".*\" found\\.\\'"
	"^No further dynamic expansions for \".*\" found\\.\\'"
	"^No further dynamic expansions for `.*' found\\'"

	;; Completion
	"^To complete, the point must be after a symbol at least [0-9]* character long\\.\\'"
	"^The string \".*\" is too short to be saved as a completion\\.\\'"

	;; Compile
	"^No more errors\\( yet\\|\\)\\'"

	;; Gnus
	"^NNTP: Connection closed\\.\\'"

	;; info
	"^Node has no Previous\\'"
	"^No \".*\" in index\\'"

	;; imenu
	"^No items suitable for an index found in this buffer\\.\\'"
	"^The mode \".*\" does not take full advantage of imenu\\.el yet\\.\\'"

	;; ispell
	"^No word found to check!\\'"

	;; man

	;; etags
	"^No tags table in use!  Use .* to select one\\.\\'"
	"^There is no default tag\\'"
	"^No previous tag locations\\'"
	"^File .* is not a valid tags table\\'"
	"^No \\(more \\|\\)tags \\(matching\\|containing\\) "
	"^Rerun etags: `.*' not found in "
	"^All files processed\\.\\'"
	"^No .* or .* in progress.\\'"
	"^File .* not in current tags tables\\'"
	"No tags table loaded."
	"^Nothing to complete\\'"

	;; BBDB
	"^no previous record\\'"
	"^no next record\\'"))

(make-variable-buffer-local 'indent-tabs-mode)


;;; This code also was not generated by autoload.el, because VM goes out
;;; of its way to be perverse.

(autoload 'vm "vm"
   "\
View Mail: an alternate mail reader for emacs.
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

See the documentation for vm-mode for more information."
 t)

(autoload 'vm-mode "vm" 
  "\
View Mail: an alternate mail reader for emacs.

Commands:
   h - summarize folder contents
   j - discard cached information about the current message

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - scroll forward a page (if at end of message, then display next message)
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message

   d - delete message, prefix arg deletes messages forward (flag as deleted)
 C-d - delete message, prefix arg deletes messages backward (flag as deleted)
   u - undelete
   k - flag for deletion all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text for current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       message into the current folder.

   G - sort messages by various keys

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder
   V - visit a virtual folder

   e - edit the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, expunging deleted messages
   A - save unfiled messages to their vm-auto-folder-alist specified folders
   # - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are expunged, folder saved to disk
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so

       M M - mark the current message
       M U - unmark the current message
       M m - mark all messages
       M u - unmark all messages
       M ? - help for the mark commands

 W S - save the current window configuration to a name
 W D - delete a window configuration
 W W - apply a configuration
 W ? - help for the window configuration commands

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes.  Expunges and saves
             cannot be undone.  C-x u is also bound to this
             command.

   L - reload your VM init file, ~/.vm

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Variables:
   vm-auto-center-summary
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-get-new-mail
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-check-folder-types
   vm-convert-folder-types
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-crash-box
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-burst-type
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-digest-send-type
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forwarded-headers
   vm-forwarding-digest-type
   vm-forwarding-subject-format
   vm-gargle-uucp
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-prefix
   vm-inhibit-startup-message
   vm-invisible-header-regexp
   vm-jump-to-new-messages
   vm-jump-to-unread-messages
   vm-keep-sent-messages
   vm-mail-header-from
   vm-mail-mode-hook
   vm-mail-window-percentage
   vm-mode-hook
   vm-move-after-deleting
   vm-move-after-undeleting
   vm-mutable-windows
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-recognize-pop-maildrops
   vm-reply-ignored-addresses
   vm-reply-subject-prefix
   vm-resend-bounced-headers
   vm-resend-bounced-discard-header-regexp
   vm-resend-headers
   vm-resend-discard-header-regexp
   vm-retain-message-order
   vm-rfc1153-digest-discard-header-regexp
   vm-rfc1153-digest-headers
   vm-rfc934-digest-discard-header-regexp
   vm-rfc934-digest-headers
   vm-search-using-regexps
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-format
   vm-unforwarded-header-regexp
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-when-saving
   vm-window-configuration-file
"
 t)

(autoload 'vm-visit-folder "vm" 
  "\
Visit a mail file with View Mail, an alternate mail reader for emacs.
See the description of the `vm' and `vm-mode' functions.

VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  t)

(autoload 'vm-mail "vm"
  "\
Send a mail message from within View Mail, or from without."
  t)


;;; Load in generated autoloads (made by autoload.el).
;; (condition-case nil
    ;; (load "auto-autoloads")
  ;; (file-error nil))
(let ((dir load-path))
  (while dir
    (condition-case nil
	(load (concat (car dir) "/auto-autoloads"))
      (file-error nil))
    (pop dir)))

;;; Local Variables:
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; loaddefs.el ends here
