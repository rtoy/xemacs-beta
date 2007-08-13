;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'mh-e-autoloads))
    (progn

;;;### (autoloads (mh-letter-mode mh-smail-other-window mh-smail-batch mh-smail) "mh-comp" "mh-e/mh-comp.el")

(autoload 'mh-smail "mh-comp" "\
Compose and send mail with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail." t nil)

(autoload 'mh-smail-batch "mh-comp" "\
Set up a mail composition draft with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.  This function does not prompt the user
for any header fields, and thus is suitable for use by programs
that want to create a mail buffer.
Users should use `\\[mh-smail]' to compose mail." nil nil)

(autoload 'mh-smail-other-window "mh-comp" "\
Compose and send mail in other window with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail." t nil)

(autoload 'mh-letter-mode "mh-comp" "\
Mode for composing letters in mh-e.\\<mh-letter-mode-map>
When you have finished composing, type \\[mh-send-letter] to send the message
using the MH mail handling system.
See the documentation for \\[mh-edit-mhn] for information on composing MIME
messages.

\\{mh-letter-mode-map}

Variables controlling this mode (defaults in parentheses):

 mh-delete-yanked-msg-window (nil)
    If non-nil, \\[mh-yank-cur-msg] will delete any windows displaying
    the yanked message.

 mh-yank-from-start-of-msg (t)
    If non-nil, \\[mh-yank-cur-msg] will include the entire message.
    If `body', just yank the body (no header).
    If nil, only the portion of the message following the point will be yanked.
    If there is a region, this variable is ignored.

 mh-ins-buf-prefix (\"> \")
    String to insert before each non-blank line of a message as it is
    inserted in a draft letter.

 mh-signature-file-name (\"~/.signature\")
    File to be inserted into message by \\[mh-insert-signature].

Upon invoking mh-letter-mode, text-mode-hook and mh-letter-mode-hook are
invoked with no args, if those values are non-nil." t nil)

;;;***

;;;### (autoloads (mh-version mh-rmail) "mh-e" "mh-e/mh-e.el")

(autoload 'mh-rmail "mh-e" "\
Inc(orporate) new mail with MH, or, with arg, scan an MH mail folder.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system." t nil)

(autoload 'mh-version "mh-e" "\
Display version information about mh-e and the MH mail handling system." t nil)

;;;***

;;;### (autoloads nil "mh-mime" "mh-e/mh-mime.el")

(defvar mh-mime-content-types '(("text/plain") ("text/richtext") ("multipart/mixed") ("multipart/alternative") ("multipart/digest") ("multipart/parallel") ("message/rfc822") ("message/partial") ("message/external-body") ("application/octet-stream") ("application/postscript") ("image/jpeg") ("image/gif") ("audio/basic") ("video/mpeg")) "\
Legal MIME content types.  See documentation for \\[mh-edit-mhn].")

;;;***

;;;### (autoloads nil "mh-utils" "mh-e/mh-utils.el")

(put 'mh-progs 'risky-local-variable t)

(put 'mh-lib 'risky-local-variable t)

;;;***

(provide 'mh-e-autoloads)
))
