;;; DO NOT MODIFY THIS FILE
(if (featurep 'mule-autoloads) (error "Already loaded"))

;;;### (autoloads (isearch-fep-quail isearch-fep-canna isearch-fep-egg isearch-fep-string) "isearch-mule" "mule/isearch-mule.el")

(defvar search-string-char-prompt "*Enter string... ")

(autoload 'isearch-fep-string "isearch-mule" "\
Read string from minibuffer for incremental search." t nil)

(autoload 'isearch-fep-egg "isearch-mule" "\
Read string for incremental search by using egg." t nil)

(autoload 'isearch-fep-canna "isearch-mule" "\
Read string for incremental search by using canna." t nil)

(autoload 'isearch-fep-quail "isearch-mule" "\
Read string for incremental search by using quail." t nil)

;;;***

;;;### (autoloads (ccl-execute-with-args define-ccl-program declare-ccl-program ccl-dump ccl-compile ccl-program-p) "mule-ccl" "mule/mule-ccl.el")

(autoload 'ccl-program-p "mule-ccl" "\
T if OBJECT is a valid CCL compiled code." nil nil)

(autoload 'ccl-compile "mule-ccl" "\
Return a compiled code of CCL-PROGRAM as a vector of integer." nil nil)

(autoload 'ccl-dump "mule-ccl" "\
Disassemble compiled CCL-CODE." nil nil)

(autoload 'declare-ccl-program "mule-ccl" "\
Declare NAME as a name of CCL program.

To compile a CCL program which calls another CCL program not yet
defined, it must be declared as a CCL program in advance." nil 'macro)

(autoload 'define-ccl-program "mule-ccl" "\
Set NAME the compiled code of CCL-PROGRAM.
CCL-PROGRAM is `eval'ed before being handed to the CCL compiler `ccl-compile'.
The compiled code is a vector of integers." nil 'macro)

(autoload 'ccl-execute-with-args "mule-ccl" "\
Execute CCL-PROGRAM with registers initialized by the remaining args.
The return value is a vector of resulting CCL registeres." nil nil)

;;;***

;;;### (autoloads (dump-coding-systems dump-charsets mule-diag list-fontset describe-fontset list-font describe-font list-coding-system list-coding-system-briefly list-charsets) "mule-debug" "mule/mule-debug.el")

(autoload 'list-charsets "mule-debug" "\
Display a list of existing character sets." t nil)

(autoload 'list-coding-system-briefly "mule-debug" "\
Display coding-systems currently used with a brief format in mini-buffer." t nil)

(autoload 'list-coding-system "mule-debug" "\
Describe coding-systems currently used with a detailed format.
If optional arg ALL is non-nil, all coding-systems are listed in
machine readable simple format." t nil)

(autoload 'describe-font "mule-debug" "\
Display information about fonts which partially match FONTNAME." t nil)

(autoload 'list-font "mule-debug" "\
Display a list of fonts." t nil)

(autoload 'describe-fontset "mule-debug" "\
Display information about FONTSET." t nil)

(autoload 'list-fontset "mule-debug" "\
Display a list of fontsets." t nil)

(autoload 'mule-diag "mule-debug" "\
Show diagnosis of the current running Mule." t nil)

(autoload 'dump-charsets "mule-debug" nil nil nil)

(autoload 'dump-coding-systems "mule-debug" nil nil nil)

;;;***

;;;### (autoloads (dump-codings dump-charsets mule-diag list-input-methods list-fontsets describe-fontset describe-font list-coding-systems describe-current-coding-system describe-current-coding-system-briefly describe-coding-system list-character-sets) "mule-diag" "mule/mule-diag.el")

(autoload 'list-character-sets "mule-diag" "\
Display a list of all character sets.

The ID column contains a charset identification number for internal use.
The B column contains a number of bytes occupied in a buffer.
The W column contains a number of columns occupied in a screen.

With prefix arg, the output format gets more cryptic
but contains full information about each character sets." t nil)

(autoload 'describe-coding-system "mule-diag" "\
Display information of CODING-SYSTEM." t nil)

(autoload 'describe-current-coding-system-briefly "mule-diag" "\
Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
at the place of `..':
  buffer-file-coding-system (of the current buffer)
  eol-type of buffer-file-coding-system (of the current buffer)
  (keyboard-coding-system)
  eol-type of (keyboard-coding-system)
  (terminal-coding-system)
  eol-type of (terminal-coding-system)
  process-coding-system for read (of the current buffer, if any)
  eol-type of process-coding-system for read (of the current buffer, if any)
  process-coding-system for write (of the current buffer, if any)
  eol-type of process-coding-system for write (of the current buffer, if any)
  default-buffer-file-coding-system
  eol-type of default-buffer-file-coding-system
  default-process-coding-system for read
  eol-type of default-process-coding-system for read
  default-process-coding-system for write
  eol-type of default-process-coding-system" t nil)

(autoload 'describe-current-coding-system "mule-diag" "\
Display coding systems currently used in a detailed format." t nil)

(autoload 'list-coding-systems "mule-diag" "\
Display a list of all coding systems.
It prints mnemonic letter, name, and description of each coding systems.

With prefix arg, the output format gets more cryptic,
but contains full information about each coding systems." t nil)

(autoload 'describe-font "mule-diag" "\
Display information about fonts which partially match FONTNAME." t nil)

(autoload 'describe-fontset "mule-diag" "\
Display information of FONTSET.

It prints name, size, and style of FONTSET, and lists up fonts
contained in FONTSET.

The column WDxHT contains width and height (pixels) of each fontset
\(i.e. those of ASCII font in the fontset).  The letter `-' in this
column means that the corresponding fontset is not yet used in any
frame.

The O column of each font contains one of the following letters.
 o -- font already opened
 - -- font not yet opened
 x -- font can't be opened
 ? -- no font specified

The Charset column of each font contains a name of character set
displayed by the font." t nil)

(autoload 'list-fontsets "mule-diag" "\
Display a list of all fontsets.

It prints name, size, and style of each fontset.
With prefix arg, it also lists up fonts contained in each fontset.
See the function `describe-fontset' for the format of the list." t nil)

(autoload 'list-input-methods "mule-diag" "\
Print information of all input methods." t nil)

(autoload 'mule-diag "mule-diag" "\
Display diagnosis of the multilingual environment (MULE).

It prints various information related to the current multilingual
environment, including lists of input methods, coding systems,
character sets, and fontsets (if Emacs running under some window
system)." t nil)

(autoload 'dump-charsets "mule-diag" "\
Dump information of all charsets into the file \"CHARSETS\".
The file is saved in the directory `data-directory'." nil nil)

(autoload 'dump-codings "mule-diag" "\
Dump information of all coding systems into the file \"CODINGS\".
The file is saved in the directory `data-directory'." nil nil)

;;;***

;;;### (autoloads (set-keyboard-coding-system) "mule-keyboard" "mule/mule-keyboard.el")

(defvar keyboard-allow-latin-input nil "\
If non-nil, \"ESC , Fe\" and \"ESC - Fe\" are used for inputting
Latin characters.")

(autoload 'set-keyboard-coding-system "mule-keyboard" "\
Set variable keyboard-coding-system to CODESYS and modify keymap for it." t nil)

;;;***

;;;### (autoloads (define-word-regexp regexp-word-compile regexp-compile) "mule-trex" "mule/mule-trex.el")

(autoload 'regexp-compile "mule-trex" nil nil nil)

(autoload 'regexp-word-compile "mule-trex" nil nil nil)

(autoload 'define-word-regexp "mule-trex" nil nil 'macro)

;;;***

;;;### (autoloads (decompose-composite-char compose-chars decompose-region compose-region set-coding-system-alist lookup-nested-alist set-nested-alist nested-alist-p) "mule-util" "mule/mule-util.el")

(autoload 'nested-alist-p "mule-util" "\
Return t if OBJ is a nesetd alist.

Nested alist is a list of the form (ENTRY . BRANCHES), where ENTRY is
any Lisp object, and BRANCHES is a list of cons cells of the form
\(KEY-ELEMENT . NESTED-ALIST).

You can use a nested alist to store any Lisp object (ENTRY) for a key
sequence KEYSEQ, where KEYSEQ is a sequence of KEY-ELEMENT.  KEYSEQ
can be a string, a vector, or a list." nil nil)

(autoload 'set-nested-alist "mule-util" "\
Set ENTRY for KEYSEQ in a nested alist ALIST.
Optional 4th arg LEN non-nil means the firlst LEN elements in KEYSEQ
 is considered.
Optional argument BRANCHES if non-nil is branches for a keyseq
longer than KEYSEQ.
See the documentation of `nested-alist-p' for more detail." nil nil)

(autoload 'lookup-nested-alist "mule-util" "\
Look up key sequence KEYSEQ in nested alist ALIST.  Return the definition.
Optional 1st argument LEN specifies the length of KEYSEQ.
Optional 2nd argument START specifies index of the starting key.
The returned value is normally a nested alist of which
car part is the entry for KEYSEQ.
If ALIST is not deep enough for KEYSEQ, return number which is
 how many key elements at the front of KEYSEQ it takes
 to reach a leaf in ALIST.
Optional 3rd argument NIL-FOR-TOO-LONG non-nil means return nil
 even if ALIST is not deep enough." nil nil)

(autoload 'set-coding-system-alist "mule-util" "\
Update `coding-system-alist' according to the arguments.
TARGET-TYPE specifies a type of the target: `file', `process', or `network'.
  TARGET-TYPE tells which slots of coding-system-alist should be affected.
  If `file', it affects slots for insert-file-contents and write-region.
  If `process', it affects slots for call-process, call-process-region, and
    start-process.
  If `network', it affects a slot for open-network-process.
REGEXP is a regular expression matching a target of I/O operation.
CODING-SYSTEM is a coding system to perform code conversion
  on the I/O operation, or a cons of coding systems for decoding and
  encoding respectively, or a function symbol which returns the cons.
Optional arg OPERATION if non-nil specifies directly one of slots above.
  The valid value is: insert-file-contents, write-region,
  call-process, call-process-region, start-process, or open-network-stream.
If OPERATION is specified, TARGET-TYPE is ignored.
See the documentation of `coding-system-alist' for more detail." nil nil)

(autoload 'compose-region "mule-util" "\
Compose characters in the current region into one composite character.
From a Lisp program, pass two arguments, START to END.
The composite character replaces the composed characters.
BUFFER defaults to the current buffer if omitted." t nil)

(autoload 'decompose-region "mule-util" "\
Decompose any composite characters in the current region.
From a Lisp program, pass two arguments, START to END.
This converts each composite character into one or more characters,
the individual characters out of which the composite character was formed.
Non-composite characters are left as-is.  BUFFER defaults to the current
buffer if omitted." t nil)

(defconst reference-point-alist '((tl . 0) (tc . 1) (tr . 2) (ml . 3) (mc . 4) (mr . 5) (bl . 6) (bc . 7) (br . 8) (top-left . 0) (top-center . 1) (top-right . 2) (mid-left . 3) (mid-center . 4) (mid-right . 5) (bottom-left . 6) (bottom-center . 7) (bottom-right . 8) (0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5) (6 . 6) (7 . 7) (8 . 8)) "\
Alist of reference point symbols vs reference point codes.
Meanings of reference point codes are as follows:

    0----1----2 <-- ascent	0:tl or top-left
    |         |			1:tc or top-center
    |         |			2:tr or top-right
    |         |			3:ml or mid-left
    |    4 <--+---- center	4:mc or mid-center
    |         |			5:mr or mid-right
--- 3         5 <-- baseline	6:bl or bottom-left
    |         |			7:bc or bottom-center
    6----7----8 <-- descent	8:br or bottom-right

Reference point symbols are to be used to specify composition rule of
the form (GLOBAL-REF-POINT . NEW-REF-POINT), where GLOBAL-REF-POINT
is a reference point in the overall glyphs already composed, and
NEW-REF-POINT is a reference point in the new glyph to be added.

For instance, if GLOBAL-REF-POINT is 8 and NEW-REF-POINT is 1, the
overall glyph is updated as follows:

    +-------+--+ <--- new ascent
    |       |  |
    | global|  |
    | glyph |  |
--- |       |  | <--- baseline (doesn't change)
    +----+--+--+
    |    | new |
    |    |glyph|
    +----+-----+ <--- new descent
")

(autoload 'compose-chars "mule-util" "\
Return one char string composed from the arguments.
Each argument is a character (including a composite chararacter)
or a composition rule.
A composition rule has the form (GLOBAL-REF-POINT . NEW-REF-POINT).
See the documentation of `reference-point-alist' for more detail." nil nil)

(autoload 'decompose-composite-char "mule-util" "\
Convert composite character CHAR to a string containing components of CHAR.
Optional 1st arg TYPE specifies the type of sequence returned.
It should be `string' (default), `list', or `vector'.
Optional 2nd arg WITH-COMPOSITION-RULE non-nil means the returned
sequence contains embedded composition rules if any.  In this case, the
order of elements in the sequence is the same as arguments for
`compose-chars' to create CHAR.
If TYPE is omitted or is `string', composition rules are omitted
even if WITH-COMPOSITION-RULE is t." nil nil)

;;;***

(provide 'mule-autoloads)
