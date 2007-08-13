;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'bytecomp-autoloads))
    (progn

;;;### (autoloads (batch-byte-recompile-directory batch-byte-recompile-directory-norecurse batch-byte-compile display-call-tree byte-compile-sexp byte-compile compile-defun byte-compile-file byte-recompile-file byte-recompile-directory byte-force-recompile) "bytecomp" "bytecomp/bytecomp.el")

(autoload 'byte-force-recompile "bytecomp" "\
Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also." t nil)

(autoload 'byte-recompile-directory "bytecomp" "\
Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also unless argument
NORECURSION is non-nil.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it.  Prefix argument 0 means
don't ask and compile the file anyway.

A nonzero prefix argument also means ask about each subdirectory.

If the fourth argument FORCE is non-nil,
recompile every `.el' file that already has a `.elc' file." t nil)

(autoload 'byte-recompile-file "bytecomp" "\
Recompile a file of Lisp code named FILENAME if it needs recompilation.
This is if the `.elc' file exists but is older than the `.el' file.

If the `.elc' file does not exist, normally the `.el' file is *not*
compiled.  But a prefix argument (optional second arg) means ask user
whether to compile it.  Prefix argument 0 don't ask and recompile anyway." t nil)

(autoload 'byte-compile-file "bytecomp" "\
Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending `c' to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), load the file after compiling." t nil)

(autoload 'compile-defun "bytecomp" "\
Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form." t nil)

(autoload 'byte-compile "bytecomp" "\
If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function." nil nil)

(autoload 'byte-compile-sexp "bytecomp" "\
Compile and return SEXP." nil nil)

(autoload 'display-call-tree "bytecomp" "\
Display a call graph of a specified file.
This lists which functions have been called, what functions called
them, and what functions they call.  The list includes all functions
whose definitions have been compiled in this Emacs session, as well as
all functions called by those functions.

The call graph does not include macros, inline functions, or
primitives that the byte-code interpreter knows about directly (eq,
cons, etc.).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled), and which cannot be
invoked interactively." t nil)

(autoload 'batch-byte-compile "bytecomp" "\
Run `byte-compile-file' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\"" nil nil)

(autoload 'batch-byte-recompile-directory-norecurse "bytecomp" "\
Same as `batch-byte-recompile-directory' but without recursion." nil nil)

(autoload 'batch-byte-recompile-directory "bytecomp" "\
Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `xemacs -batch -f batch-byte-recompile-directory .'." nil nil)

;;;***

;;;### (autoloads (disassemble) "disass" "bytecomp/disass.el")

(autoload 'disassemble "disass" "\
Print disassembled code for OBJECT in (optional) BUFFER.
OBJECT can be a symbol defined as a function, or a function itself
\(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol." t nil)

;;;***

(provide 'bytecomp-autoloads)
))
