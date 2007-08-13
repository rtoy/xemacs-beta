;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'gnus-autoloads))
    (progn

;;;### (autoloads (gnus-earcon-display) "earcon" "gnus/earcon.el")

(autoload 'gnus-earcon-display "earcon" "\
Play sounds in message buffers." t nil)

;;;***

;;;### (autoloads (gnus-audio-play) "gnus-audio" "gnus/gnus-audio.el")

(autoload 'gnus-audio-play "gnus-audio" "\
Play a sound through the speaker." t nil)

;;;***

;;;### (autoloads (gnus-cache-generate-nov-databases gnus-cache-generate-active gnus-jog-cache) "gnus-cache" "gnus/gnus-cache.el")

(autoload 'gnus-jog-cache "gnus-cache" "\
Go through all groups and put the articles into the cache.

Usage:
$ emacs -batch -l ~/.emacs -l gnus -f gnus-jog-cache" t nil)

(autoload 'gnus-cache-generate-active "gnus-cache" "\
Generate the cache active file." t nil)

(autoload 'gnus-cache-generate-nov-databases "gnus-cache" "\
Generate NOV files recursively starting in DIR." t nil)

;;;***

;;;### (autoloads (gnus-fetch-group) "gnus-group" "gnus/gnus-group.el")

(autoload 'gnus-fetch-group "gnus-group" "\
Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not." t nil)

;;;***

;;;### (autoloads (gnus-batch-score) "gnus-kill" "gnus/gnus-kill.el")

(defalias 'gnus-batch-kill 'gnus-batch-score)

(autoload 'gnus-batch-score "gnus-kill" "\
Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\".  If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"." t nil)

;;;***

;;;### (autoloads (gnus-change-server) "gnus-move" "gnus/gnus-move.el")

(autoload 'gnus-change-server "gnus-move" "\
Move from FROM-SERVER to TO-SERVER.
Update the .newsrc.eld file to reflect the change of nntp server." t nil)

;;;***

;;;### (autoloads (gnus-batch-brew-soup) "gnus-soup" "gnus/gnus-soup.el")

(autoload 'gnus-batch-brew-soup "gnus-soup" "\
Brew a SOUP packet from groups mention on the command line.
Will use the remaining command line arguments as regular expressions
for matching on group names.

For instance, if you want to brew on all the nnml groups, as well as
groups with \"emacs\" in the name, you could say something like:

$ emacs -batch -f gnus-batch-brew-soup ^nnml \".*emacs.*\"" t nil)

;;;***

;;;### (autoloads (gnus-update-format) "gnus-spec" "gnus/gnus-spec.el")

(autoload 'gnus-update-format "gnus-spec" "\
Update the format specification near point." t nil)

;;;***

;;;### (autoloads (gnus-declare-backend gnus-unload) "gnus-start" "gnus/gnus-start.el")

(autoload 'gnus-unload "gnus-start" "\
Unload all Gnus features." t nil)

(autoload 'gnus-declare-backend "gnus-start" "\
Declare backend NAME with ABILITIES as a Gnus backend." nil nil)

;;;***

;;;### (autoloads (gnus-add-configuration) "gnus-win" "gnus/gnus-win.el")

(autoload 'gnus-add-configuration "gnus-win" "\
Add the window configuration CONF to `gnus-buffer-configuration'." nil nil)

;;;***

;;;### (autoloads (gnus gnus-other-frame gnus-slave gnus-no-server gnus-slave-no-server) "gnus" "gnus/gnus.el")

(autoload 'gnus-slave-no-server "gnus" "\
Read network news as a slave, without connecting to local server" t nil)

(autoload 'gnus-no-server "gnus" "\
Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server." t nil)

(autoload 'gnus-slave "gnus" "\
Read news as a slave." t nil)

(autoload 'gnus-other-frame "gnus" "\
Pop up a frame to read news." t nil)

(autoload 'gnus "gnus" "\
Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use." t nil)

;;;***

;;;### (autoloads (unbold-region bold-region message-news-other-frame message-news-other-window message-mail-other-frame message-mail-other-window message-bounce message-resend message-forward message-recover message-supersede message-cancel-news message-followup message-wide-reply message-reply message-news message-mail message-mode) "message" "gnus/message.el")

(defcustom message-from-style 'default "*Specifies how \"From\" headers look.\n\nIf `nil', they contain just the return address like:\n	king@grassland.com\nIf `parens', they look like:\n	king@grassland.com (Elvis Parsley)\nIf `angles', they look like:\n	Elvis Parsley <king@grassland.com>\n\nOtherwise, most addresses look like `angles', but they look like\n`parens' if `angles' would need quoting and `parens' would not." :type '(choice (const :tag "simple" nil) (const parens) (const angles) (const default)) :group 'message-headers)

(defcustom message-signature-separator "^-- *$" "Regexp matching the signature separator." :type 'regexp :group 'message-various)

(defcustom message-user-organization-file "/usr/lib/news/organization" "*Local news organization file." :type 'file :group 'message-headers)

(defcustom message-send-mail-function 'message-send-mail-with-sendmail "Function to call to send the current buffer as mail.\nThe headers should be delimited by a line whose contents match the\nvariable `mail-header-separator'.\n\nLegal values include `message-send-mail-with-sendmail' (the default),\n`message-send-mail-with-mh' and `message-send-mail-with-qmail'." :type '(radio (function-item message-send-mail-with-sendmail) (function-item message-send-mail-with-mh) (function-item message-send-mail-with-qmail) (function :tag "Other")) :group 'message-sending :group 'message-mail)

(defcustom message-citation-line-function 'message-insert-citation-line "*Function called to insert the \"Whomever writes:\" line." :type 'function :group 'message-insertion)

(defcustom message-yank-prefix "> " "*Prefix inserted on the lines of yanked messages.\nnil means use indentation." :type 'string :group 'message-insertion)

(defcustom message-cite-function (if (and (boundp 'mail-citation-hook) mail-citation-hook) mail-citation-hook 'message-cite-original) "*Function for citing an original message." :type '(radio (function-item message-cite-original) (function-item sc-cite-original) (function :tag "Other")) :group 'message-insertion)

(defcustom message-indent-citation-function 'message-indent-citation "*Function for modifying a citation just inserted in the mail buffer.\nThis can also be a list of functions.  Each function can find the\ncitation between (point) and (mark t).  And each function should leave\npoint and mark around the citation text as modified." :type 'function :group 'message-insertion)

(defcustom message-signature t "*String to be inserted at the end of the message buffer.\nIf t, the `message-signature-file' file will be inserted instead.\nIf a function, the result from the function will be used instead.\nIf a form, the result from the form will be used instead." :type 'sexp :group 'message-insertion)

(defcustom message-signature-file "~/.signature" "*File containing the text inserted at end of message buffer." :type 'file :group 'message-insertion)

(autoload 'message-mode "message" "\
Major mode for editing mail and news to be sent.
Like Text Mode but with these additional commands:
C-c C-s  message-send (send the message)    C-c C-c  message-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To	C-c C-f C-s  move to Subject
	 C-c C-f C-c  move to Cc	C-c C-f C-b  move to Bcc
	 C-c C-f C-w  move to Fcc	C-c C-f C-r  move to Reply-To
	 C-c C-f C-u  move to Summary	C-c C-f C-n  move to Newsgroups
	 C-c C-f C-k  move to Keywords	C-c C-f C-d  move to Distribution
	 C-c C-f C-f  move to Followup-To
C-c C-t  message-insert-to (add a To header to a news followup)
C-c C-n  message-insert-newsgroups (add a Newsgroup header to a news reply)
C-c C-b  message-goto-body (move to beginning of message text).
C-c C-i  message-goto-signature (move to the beginning of the signature).
C-c C-w  message-insert-signature (insert `message-signature-file' file).
C-c C-y  message-yank-original (insert current message, if any).
C-c C-q  message-fill-yanked-message (fill what was yanked).
C-c C-e  message-elide-region (elide the text between point and mark).
C-c C-r  message-caesar-buffer-body (rot13 the message body)." t nil)

(autoload 'message-mail "message" "\
Start editing a mail message to be sent." t nil)

(autoload 'message-news "message" "\
Start editing a news article to be sent." t nil)

(autoload 'message-reply "message" "\
Start editing a reply to the article in the current buffer." t nil)

(autoload 'message-wide-reply "message" "\
Make a \"wide\" reply to the message in the current buffer." t nil)

(autoload 'message-followup "message" "\
Follow up to the message in the current buffer.
If TO-NEWSGROUPS, use that as the new Newsgroups line." t nil)

(autoload 'message-cancel-news "message" "\
Cancel an article you posted." t nil)

(autoload 'message-supersede "message" "\
Start composing a message to supersede the current message.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID." t nil)

(autoload 'message-recover "message" "\
Reread contents of current buffer from its last auto-save file." t nil)

(autoload 'message-forward "message" "\
Forward the current message via mail.
Optional NEWS will use news to forward instead of mail." t nil)

(autoload 'message-resend "message" "\
Resend the current article to ADDRESS." t nil)

(autoload 'message-bounce "message" "\
Re-mail the current message.
This only makes sense if the current message is a bounce message than
contains some mail you have written which has been bounced back to
you." t nil)

(autoload 'message-mail-other-window "message" "\
Like `message-mail' command, but display mail buffer in another window." t nil)

(autoload 'message-mail-other-frame "message" "\
Like `message-mail' command, but display mail buffer in another frame." t nil)

(autoload 'message-news-other-window "message" "\
Start editing a news article to be sent." t nil)

(autoload 'message-news-other-frame "message" "\
Start editing a news article to be sent." t nil)

(autoload 'bold-region "message" "\
Bold all nonblank characters in the region.
Works by overstriking characters.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload 'unbold-region "message" "\
Remove all boldness (overstruck characters) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

;;;***

;;;### (autoloads nil "messcompat" "gnus/messcompat.el")

(defvar message-signature-file mail-signature-file "\
*File containing the text inserted at end of message. buffer.")

;;;***

;;;### (autoloads (nndoc-add-type) "nndoc" "gnus/nndoc.el")

(autoload 'nndoc-add-type "nndoc" "\
Add document DEFINITION to the list of nndoc document definitions.
If POSITION is nil or `last', the definition will be added
as the last checked definition, if t or `first', add as the
first definition, and if any other symbol, add after that
symbol in the alist." nil nil)

;;;***

;;;### (autoloads (nnfolder-generate-active-file) "nnfolder" "gnus/nnfolder.el")

(autoload 'nnfolder-generate-active-file "nnfolder" "\
Look for mbox folders in the nnfolder directory and make them into groups." t nil)

;;;***

;;;### (autoloads (nnkiboze-generate-groups) "nnkiboze" "gnus/nnkiboze.el")

(autoload 'nnkiboze-generate-groups "nnkiboze" "\
Usage: emacs -batch -l nnkiboze -f nnkiboze-generate-groups
Finds out what articles are to be part of the nnkiboze groups." t nil)

;;;***

;;;### (autoloads (nnml-generate-nov-databases) "nnml" "gnus/nnml.el")

(autoload 'nnml-generate-nov-databases "nnml" "\
Generate NOV databases in all nnml directories." t nil)

;;;***

;;;### (autoloads (nnsoup-revert-variables nnsoup-set-variables nnsoup-pack-replies) "nnsoup" "gnus/nnsoup.el")

(autoload 'nnsoup-pack-replies "nnsoup" "\
Make an outbound package of SOUP replies." t nil)

(autoload 'nnsoup-set-variables "nnsoup" "\
Use the SOUP methods for posting news and mailing mail." t nil)

(autoload 'nnsoup-revert-variables "nnsoup" "\
Revert posting and mailing methods to the standard Emacs methods." t nil)

;;;***

;;;### (autoloads (gnus-score-mode) "score-mode" "gnus/score-mode.el")

(autoload 'gnus-score-mode "score-mode" "\
Mode for editing Gnus score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}" t nil)

;;;***

;;;### (autoloads (gnus-smiley-display smiley-buffer smiley-region) "smiley" "gnus/smiley.el")

(autoload 'smiley-region "smiley" "\
Smilify the region between point and mark." t nil)

(autoload 'smiley-buffer "smiley" nil t nil)

(autoload 'gnus-smiley-display "smiley" "\
Display \"smileys\" as small graphical icons." t nil)

;;;***

(provide 'gnus-autoloads)
))
