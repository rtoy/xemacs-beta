;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'mule-autoloads))
    (progn

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

;;;### (autoloads (define-ccl-program ccl-dump ccl-compile ccl-program-p) "mule-ccl" "mule/mule-ccl.el")

(autoload 'ccl-program-p "mule-ccl" "\
T if OBJECT is a valid CCL compiled code." nil nil)

(autoload 'ccl-compile "mule-ccl" "\
Compile a CCL source program and return the compiled equivalent.
The return value will be a vector of integers." nil nil)

(autoload 'ccl-dump "mule-ccl" "\
Disassemble compiled CCL-CODE." nil nil)

(autoload 'define-ccl-program "mule-ccl" "\
Does (defconst NAME (ccl-compile (eval CCL-PROGRAM)) DOC).
Byte-compiler expand this macro while compiling." nil 'macro)

;;;***

;;;### (autoloads (dump-coding-systems dump-charsets mule-diag list-fontset describe-fontset list-font describe-font list-coding-system list-coding-system-briefly describe-coding-system list-charsets) "mule-debug" "mule/mule-debug.el")

(autoload 'list-charsets "mule-debug" "\
Display a list of existing character sets." t nil)

(autoload 'describe-coding-system "mule-debug" "\
Display documentation of the coding-system CS." t nil)

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

;;;### (autoloads (decompose-composite-char compose-chars decompose-region compose-region set-coding-system-alist lookup-nested-alist set-nested-alist nested-alist-p truncate-string-to-width store-substring string-to-vector string-to-list string-to-sequence) "mule-util" "mule/mule-util.el")

(autoload 'string-to-sequence "mule-util" "\
Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'.
Multibyte characters are concerned." nil nil)

(autoload 'string-to-list "mule-util" "\
Return a list of characters in STRING." nil nil)

(autoload 'string-to-vector "mule-util" "\
Return a vector of characters in STRING." nil nil)

(autoload 'store-substring "mule-util" "\
Embed OBJ (string or character) at index IDX of STRING." nil nil)

(autoload 'truncate-string-to-width "mule-util" "\
Truncate string STR to fit in WIDTH columns.
Optional 1st arg START-COLUMN if non-nil specifies the starting column.
Optional 2nd arg PADDING if non-nil is a padding character to be padded at
the head and tail of the resulting string to fit in WIDTH if necessary.
If PADDING is nil, the resulting string may be narrower than WIDTH." nil nil)

(defalias 'truncate-string 'truncate-string-to-width)

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

;;;### (autoloads (vn-decompose-viqr-buffer vn-decompose-viqr vn-compose-viqr-buffer vn-compose-viqr) "vietnamese" "mule/vietnamese.el")

(autoload 'vn-compose-viqr "vietnamese" "\
Convert 'VIQR' mnemonics of the current region to
pre-composed Vietnamese characaters." t nil)

(autoload 'vn-compose-viqr-buffer "vietnamese" nil t nil)

(autoload 'vn-decompose-viqr "vietnamese" "\
Convert pre-composed Vietnamese characaters of the current region to
'VIQR' mnemonics." t nil)

(autoload 'vn-decompose-viqr-buffer "vietnamese" nil t nil)

;;;***

(provide 'mule-autoloads)
))
