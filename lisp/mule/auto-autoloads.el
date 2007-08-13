;;; DO NOT MODIFY THIS FILE
(if (featurep 'mule-autoloads) (error "Already loaded"))

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

(provide 'mule-autoloads)
