;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'edebug-autoloads))
    (progn

;;;### (autoloads (edebug-eval-top-level-form def-edebug-spec) "edebug" "edebug/edebug.el")

(autoload 'def-edebug-spec "edebug" "\
Set the edebug-form-spec property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated. The SPEC can be 0, t, a symbol
\(naming a function), or a list." nil 'macro)

(defalias 'edebug-defun 'edebug-eval-top-level-form)

(autoload 'edebug-eval-top-level-form "edebug" "\
Evaluate a top level form, such as a defun or defmacro.
This is like `eval-defun', but the code is always instrumented for Edebug.
Print its name in the minibuffer and leave point where it is,
or if an error occurs, leave point after it with mark at the original point." t nil)

;;;***

(provide 'edebug-autoloads)
))
