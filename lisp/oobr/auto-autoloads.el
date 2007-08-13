;;; DO NOT MODIFY THIS FILE
(if (not (featurep '-autoloads))
    (progn

;;;### (autoloads (br-env-load br-env-browse) "br-env" "oobr/br-env.el")

(autoload 'br-env-browse "br-env" "\
Invoke the OO-Browser on an existing or to be created Environment ENV-FILE." t nil)

(autoload 'br-env-load "br-env" "\
Load browser Environment or spec from optional ENV-FILE or 'br-env-file'.
Non-nil PROMPT means prompt user before building tables.
Non-nil NO-BUILD means skip build of Environment entirely.
Return t if load is successful, else nil." t nil)

;;;***

;;;### (autoloads (oo-browser) "br-start" "oobr/br-start.el")

(fset 'oobr 'oo-browser)

(autoload 'oo-browser "br-start" "\
Prompt for an Environment and language over which to run the OO-Browser.
Optional prefix argument SAME-ENV-FLAG means browse the current Environment,
if any, without prompting.  Otherwise, if called interactively, give the user
a choice whether to re-browse the last Environment or to browse a new one." t nil)

;;;***

;;;### (autoloads (br-to-from-viewer br-add-class-file) "br" "oobr/br.el")

(autoload 'br-add-class-file "br" "\
Add a file of classes to the current Environment.
Interactively or when optional CLASS-PATH is nil, CLASS-PATH defaults to the
current buffer file pathname.  If optional LIB-TABLE-P is non-nil, add to
Library Environment, otherwise add to System Environment.  If optional
SAVE-FILE is t, the Environment is then stored to the filename given by
`br-env-file'.  If SAVE-FILE is non-nil and not t, its string value is used
as the file to which to save the Environment." t nil)

(autoload 'br-to-from-viewer "br" "\
Move point to viewer window or back to last recorded listing window." t nil)

;;;***

;;;### (autoloads (c++-browse) "c++-browse" "oobr/c++-browse.el")

(autoload 'c++-browse "c++-browse" "\
Invoke the C++ OO-Browser.
This allows browsing through C++ library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (clos-browse) "clos-brows" "oobr/clos-brows.el")

(autoload 'clos-browse "clos-brows" "\
Invoke the CLOS OO-Browser.
This allows browsing through CLOS library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (eif-browse) "eif-browse" "oobr/eif-browse.el")

(autoload 'eif-browse "eif-browse" "\
Invoke the Eiffel OO-Browser.
This allows browsing through Eiffel library and system class hierarchies.
With an optional prefix arg ENV-FILE equal to t, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (info-browse) "info-brows" "oobr/info-brows.el")

(autoload 'info-browse "info-brows" "\
Invoke the Info OO-Browser.
This allows browsing through Info library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (java-browse) "java-brows" "oobr/java-brows.el")

(autoload 'java-browse "java-brows" "\
Invoke the Java OO-Browser.
This allows browsing through Java library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (objc-browse) "objc-brows" "oobr/objc-brows.el")

(autoload 'objc-browse "objc-brows" "\
Invoke the Objective-C OO-Browser.
This allows browsing through Objective-C library and system class
hierarchies.  With an optional non-nil prefix argument ENV-FILE, prompt for
Environment file to use.  Alternatively, a string value of ENV-FILE is used
as the Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (python-browse) "python-browse" "oobr/python-browse.el")

(autoload 'python-browse "python-browse" "\
Invoke the Python OO-Browser.
This allows browsing through Python library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (smt-browse) "smt-browse" "oobr/smt-browse.el")

(autoload 'smt-browse "smt-browse" "\
Invoke the Smalltalk OO-Browser.
This allows browsing through Smalltalk library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

(provide '-autoloads)
))
