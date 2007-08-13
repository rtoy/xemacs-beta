;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'tm-autoloads))
    (progn

;;;### (autoloads (mime/editor-mode) "tm-edit" "tm/tm-edit.el")

(autoload 'mime/editor-mode "tm-edit" "\
MIME minor mode for editing the tagged MIME message.

In this mode, basically, the message is composed in the tagged MIME
format. The message tag looks like:

	--[[text/plain; charset=ISO-2022-JP][7bit]]

The tag specifies the MIME content type, subtype, optional parameters
and transfer encoding of the message following the tag. Messages
without any tag are treated as `text/plain' by default. Charset and
transfer encoding are automatically defined unless explicitly
specified. Binary messages such as audio and image are usually hidden.
The messages in the tagged MIME format are automatically translated
into a MIME compliant message when exiting this mode.

Available charsets depend on Emacs version being used. The following
lists the available charsets of each emacs.

EMACS 18:	US-ASCII is only available.
NEmacs:		US-ASCII and ISO-2022-JP are available.
EMACS 19:	US-ASCII and ISO-8859-1 (or other charset) are available.
XEmacs 19:	US-ASCII and ISO-8859-1 (or other charset) are available.
Mule:		US-ASCII, ISO-8859-* (except for ISO-8859-5), KOI8-R,
		ISO-2022-JP, ISO-2022-JP-2, ISO-2022-KR, BIG5 and
		ISO-2022-INT-1 are available.

ISO-2022-JP-2 and ISO-2022-INT-1 charsets used in mule is expected to
be used to represent multilingual text in intermixed manner. Any
languages that has no registered charset are represented as either
ISO-2022-JP-2 or ISO-2022-INT-1 in mule.

If you want to use non-ISO-8859-1 charset in EMACS 19 or XEmacs 19,
please set variable `default-mime-charset'. This variable must be
symbol of which name is a MIME charset.

If you want to add more charsets in mule, please set variable
`charsets-mime-charset-alist'. This variable must be alist of which
key is list of leading-char/charset and value is symbol of MIME
charset. (leading-char is a term of MULE 1.* and 2.*. charset is a
term of XEmacs/mule, mule merged EMACS and MULE 3.*) If name of
coding-system is different as MIME charset, please set variable
`mime-charset-coding-system-alist'. This variable must be alist of
which key is MIME charset and value is coding-system.

Following commands are available in addition to major mode commands:

[make single part]
\\[mime-editor/insert-text]	insert a text message.
\\[mime-editor/insert-file]	insert a (binary) file.
\\[mime-editor/insert-external]	insert a reference to external body.
\\[mime-editor/insert-voice]	insert a voice message.
\\[mime-editor/insert-message]	insert a mail or news message.
\\[mime-editor/insert-mail]	insert a mail message.
\\[mime-editor/insert-signature]	insert a signature file at end.
\\[mime-editor/insert-key]	insert PGP public key.
\\[mime-editor/insert-tag]	insert a new MIME tag.

[make enclosure (maybe multipart)]
\\[mime-editor/enclose-alternative-region]	enclose as multipart/alternative.
\\[mime-editor/enclose-parallel-region]	enclose as multipart/parallel.
\\[mime-editor/enclose-mixed-region]	enclose as multipart/mixed.
\\[mime-editor/enclose-digest-region]	enclose as multipart/digest.
\\[mime-editor/enclose-signed-region]	enclose as PGP signed.
\\[mime-editor/enclose-encrypted-region]	enclose as PGP encrypted.
\\[mime-editor/enclose-quote-region]	enclose as verbose mode (to avoid to expand tags)

[other commands]
\\[mime-editor/set-transfer-level-7bit]	set transfer-level as 7.
\\[mime-editor/set-transfer-level-8bit]	set transfer-level as 8.
\\[mime-editor/set-split]	set message splitting mode.
\\[mime-editor/set-sign]	set PGP-sign mode.
\\[mime-editor/set-encrypt]	set PGP-encryption mode.
\\[mime-editor/preview-message]	preview editing MIME message.
\\[mime-editor/exit]	exit and translate into a MIME compliant message.
\\[mime-editor/help]	show this help.
\\[mime-editor/maybe-translate]	exit and translate if in MIME mode, then split.

Additional commands are available in some major modes:
C-c C-c		exit, translate and run the original command.
C-c C-s		exit, translate and run the original command.

The following is a message example written in the tagged MIME format.
TABs at the beginning of the line are not a part of the message:

	This is a conventional plain text.  It should be translated
	into text/plain.
	--[[text/plain]]
	This is also a plain text.  But, it is explicitly specified as
	is.
	--[[text/plain; charset=ISO-8859-1]]
	This is also a plain text.  But charset is specified as
	iso-8859-1.

	¡Hola!  Buenos días.  ¿Cómo está usted?
	--[[text/enriched]]
	This is a <bold>enriched text</bold>.
	--[[image/gif][base64]]...image encoded in base64 here...
	--[[audio/basic][base64]]...audio encoded in base64 here...

User customizable variables (not documented all of them):
 mime-prefix
    Specifies a key prefix for MIME minor mode commands.

 mime-ignore-preceding-spaces
    Preceding white spaces in a message body are ignored if non-nil.

 mime-ignore-trailing-spaces
    Trailing white spaces in a message body are ignored if non-nil.

 mime-auto-hide-body
    Hide a non-textual body message encoded in base64 after insertion
    if non-nil.

 mime-editor/transfer-level
    A number of network transfer level.  It should be bigger than 7.
    If you are in 8bit-through environment, please set 8.

 mime-editor/voice-recorder
    Specifies a function to record a voice message and encode it.
    The function `mime-editor/voice-recorder-for-sun' is for Sun
    SparcStations.

 mime/editor-mode-hook
    Turning on MIME mode calls the value of mime/editor-mode-hook, if
    it is non-nil.

 mime-editor/translate-hook
    The value of mime-editor/translate-hook is called just before translating
    the tagged MIME format into a MIME compliant message if it is
    non-nil.  If the hook call the function mime-editor/insert-signature,
    the signature file will be inserted automatically.

 mime-editor/exit-hook
    Turning off MIME mode calls the value of mime-editor/exit-hook, if it is
    non-nil." t nil)

(defalias 'edit-mime 'mime/editor-mode)

;;;***

(provide 'tm-autoloads)
))
