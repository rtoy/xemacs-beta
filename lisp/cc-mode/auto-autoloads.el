;;; DO NOT MODIFY THIS FILE
(if (featurep 'cc-mode-autoloads) (error "Already loaded"))

;;;### (autoloads nil "cc-langs" "cc-mode/cc-langs.el")

(defvar c-mode-syntax-table nil "\
Syntax table used in c-mode buffers.")

(defvar c++-mode-syntax-table nil "\
Syntax table used in c++-mode buffers.")

(defvar objc-mode-syntax-table nil "\
Syntax table used in objc-mode buffers.")

(defvar java-mode-syntax-table nil "\
Syntax table used in java-mode buffers.")

(defvar idl-mode-syntax-table nil "\
Syntax table used in idl-mode buffers.")

;;;***

;;;### (autoloads (idl-mode java-mode objc-mode c++-mode c-mode c-initialize-cc-mode) "cc-mode" "cc-mode/cc-mode.el")

(autoload 'c-initialize-cc-mode "cc-mode" nil nil nil)

(autoload 'c-mode "cc-mode" "\
Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}" t nil)

(autoload 'c++-mode "cc-mode" "\
Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}" t nil)

(autoload 'objc-mode "cc-mode" "\
Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `objc-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the hook `c-mode-common-hook'
is run first.

Key bindings:
\\{objc-mode-map}" t nil)

(autoload 'java-mode "cc-mode" "\
Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `java-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"java\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{java-mode-map}" t nil)

(autoload 'idl-mode "cc-mode" "\
Major mode for editing CORBA's IDL code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `idl-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{idl-mode-map}" t nil)

;;;***

;;;### (autoloads (c-set-offset c-add-style c-set-style) "cc-styles" "cc-mode/cc-styles.el")

(autoload 'c-set-style "cc-styles" "\
Set CC Mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles.

The variable `c-indentation-style' always contains the buffer's current
style name." t nil)

(autoload 'c-add-style "cc-styles" "\
Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil." t nil)

(autoload 'c-set-offset "cc-styles" "\
Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`c-offsets-alist' if it doesn't already appear there." t nil)

;;;***

(provide 'cc-mode-autoloads)
