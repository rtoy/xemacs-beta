;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-defun.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  efs-defun allows for OS-dependent coding of functions
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Thu Oct 22 17:58:14 1992
;; Modified:     Sun Nov 27 12:18:35 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; efs-defun allows object-oriented emacs lisp definitions.
;;; In efs, this feature is used to support multiple host types.
;;; 
;;; The first arg after the function name is a key which determines
;;; which version of the function is being defined. Normally, when the function
;;; is called this key is given as the first argument to the function.
;;;
;;; For example:
;;; 
;;; (efs-defun foobar vms (x y)
;;;   (message "hello vms world")
;;;   (+ x y))
;;;   => foobar
;;;
;;; (foobar 'vms 1 2)
;;;   => 3

;;; The key nil plays a special role: 
;;;
;;; First, it defines a default action. If there is no function
;;; definition associated with a given OS-key, then the function
;;; definition associated with nil is used.  If further there is no
;;; function definition associated with nil, then an error is
;;; signaled.  
;;;
;;; Second, the documentation string for the function is the one given
;;; with the nil definition. You can supply doc-strings with other
;;; definitions of the function, but they are not accessible with
;;; 'describe-function. In fact, when the function is either loaded or
;;; byte-compiled, they are just thrown away.

;;; There is another way to define the default action of an efs-function.
;;; This is with the use flag. If you give as the key (&use foobar),
;;; then when the function is called the variable foobar will be used to
;;; determine which OS version of the function to use. As well as
;;; allowing you to define the doc string, if the use flag is used,
;;; then you can specify an interactive specification with the function.
;;; Although a function is only interactive, if the default definition
;;; has an interactive spec, it is still necessary to give interactive
;;; specs for the other definitions of the function as well. It is possible
;;; for these interactive specs to differ.
;;; 
;;; For example:
;;; 
;;; (efs-defun fizzle (&use foobar)
;;;   "Fizzle's doc string."
;;;   (interactive)
;;;   (message "fizz wizz"))
;;; 
;;; (efs-defun fizzle vms
;;;   (interactive)
;;;   (message "VMS is fizzled."))
;;; 
;;; (setq foobar 'unix)
;;; => unix
;;; 
;;; (fizzle)
;;; => "fizz wizz"
;;; 
;;; (setq foobar 'vms)
;;; => vms
;;; 
;;; (fizzle)
;;; => "VMS is fizzled."
;;; 
;;; M-x f i z z l e <return>
;;; => "VMS is fizzled."
;;;
;;; Actually, when you use the &use spec, whatever follows it is simply
;;; evaluated at call time.

;;; Note that when the function is defined, the key is implicitly
;;; quoted, whereas when the function is called, the key is
;;; evaluated.  If this seems strange, think about how efs-defuns
;;; are used in practice.

;;; There are no restrictions on the order in which the different OS-type
;;; definitions are done.

;;; There are no restrictions on the keys that can be used, nor on the
;;; symbols that can be used as arguments to an efs-defun.  We go
;;; to some lengths to avoid potential conflicts. In particular, when
;;; the OS-keys are looked up in the symbol's property list, we
;;; actually look for a symbol with the same name in the special
;;; obarray, efs-key-obarray. This avoids possible conflicts with
;;; other entries in the property list, that are usually accessed with
;;; symbols in the standard obarray.

;;; The V19 byte-compiler will byte-compile efs-defun's.
;;; The standard emacs V18 compiler will not, however they will still
;;; work, just not at byte-compiled speed.

;;; efs-autoload works much like the standard autoload, except it
;;; defines the efs function cell for a given host type as an autoload.
;;; The from-kbd arg only makes sense if the default action of the autoload
;;; has been defined with a &use.

;;; To do:
;;;
;;; 1. Set an edebug-form-hook for efs-defun

;;; Known Bugs:
;;;
;;; 1. efs-autoload will correctly NOT overload an existing function
;;;    definition with an autoload definition. However, it will also
;;;    not overload a previous autoload with a new one. It should. An
;;;    overload can be forced for the KEY def of function FUN by doing
;;;    (put 'FUN (intern "KEY" efs-key-obarray) nil) first.
;;;

;;; Provisions and requirements

(provide 'efs-defun)
(require 'backquote)

;;; Variables

(defconst efs-defun-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

(defconst efs-key-obarray (make-vector 7 0))

;; Unfortunately, we need to track this in bytecomp.el.
;; It's not much to keep track of, although.
(defconst efs-defun-bytecomp-buffer "*Compile-Log*")

(defvar efs-key nil
  "Inside an efs function, this is set to the key that was used to
call the function. You can test this inside the default definition, to
determine which key was actually used.")
(defvar efs-args nil
  "Inside an efs function, this is set to a list of the calling args
of the function.")

;;; Utility Functions

;;; These functions are called when the macros efs-defun and efs-autoload
;;; are expanded. Their purpose is to help in producing the expanded code.

(defun efs-defun-arg-count (list)
  ;; Takes a list of arguments, and returns a list of three
  ;; integers giving the number of normal args, the number
  ;; of &optional args, and the number of &rest args (this should
  ;; only be 0 or 1, but we don't check this).
  (let ((o-leng (length (memq '&optional list)))
	(r-leng (length (memq '&rest list)))
	(leng (length list)))
    (list (- leng (max o-leng r-leng))
	  (max 0 (- o-leng r-leng 1))
	  (max 0 (1- r-leng)))))

;; For each efs-function the property efs-function-arg-structure
;; is either a list of three integers to indicate the number of normal,
;; optional, and rest args, or it can be the symbol 'autoload to indicate
;; that all definitions of the function are autoloads, and we have no
;; idea of its arg structure.

(defun efs-defun-arg-check (fun key list)
  ;; Checks that the LIST of args is consistent for the KEY def
  ;; of function FUN.
  (let ((prop (get fun 'efs-function-arg-structure))
	count)
    (if (eq list 'autoload)
	(or prop (put fun 'efs-function-arg-structure 'autoload))
      (setq count (efs-defun-arg-count list))
      (if (and prop (not (eq prop 'autoload)) (not (equal prop count)))
	  (let ((warning
		 (format
		  "args. for the %s def. of %s don't agree with previous defs."
		  key fun)))
	    (message (concat "Warning: " warning))
	    ;; We are compiling, I suppose...
	    (if (get-buffer efs-defun-bytecomp-buffer)
		(save-excursion
		  (set-buffer efs-defun-bytecomp-buffer)
		  (goto-char (point-max))
		  (insert "efs warning:\n  " warning "\n")))))
      (put fun 'efs-function-arg-structure count))))

(defun efs-def-generic (fun use doc-string interactive-p)
  ;; Generates a generic function def using USE.
  ;; If use is nil, the first arg of the function
  ;; is the key.
  (let ((def-args '(&rest efs-args))
	result)
    (or use
	(setq def-args (cons 'efs-key def-args)))
    (setq result
	  (` (or (get (quote (, fun))
		      (, (if use
			     (list 'intern
				   (list 'symbol-name use)
				   'efs-key-obarray)
			   '(intern
			     (symbol-name efs-key)
			     efs-key-obarray))))
		 (get (quote (, fun))
		      (intern "nil" efs-key-obarray)))))
    ;; Make the gen fun interactive, if nec.
    (setq result
	  (if interactive-p
	      (` ((interactive)
		  (if (interactive-p)
		      (let ((prefix-arg current-prefix-arg))
			(call-interactively
			 (, result)))
		    (, (cons 'apply (list result 'efs-args))))))
	    (list (cons 'apply (list result 'efs-args)))))
    (if doc-string (setq result (cons doc-string result)))
    (cons 'defun (cons fun (cons def-args result)))))

(defun efs-def-autoload (fun key file from-kbd)
  ;; Returns the autoload lambda for FUN and FILE.
  ;; I really should have some notion of efs-autoload
  ;; objects, and not just plain lambda's.
  (let ((result
	 (if from-kbd
	     (`
	      (lambda (&rest args)
		(interactive)
		(let ((qkey (intern (symbol-name (quote (, key)))
				    efs-key-obarray))
		      (tmp1 (intern "tmp1" efs-key-obarray))
		      (tmp2 (intern "tmp2" efs-key-obarray)))
		  ;; Need to store the a-f-function, to see if it has been
		  ;; re-defined by the load. This is avoid to an infinite loop.
		  (set tmp1 (get (quote (, fun)) qkey))
		  ;; Need to store the prefix arg in case it's interactive.
		  ;; These values are stored in variables interned in the
		  ;; efs-key-obarray, because who knows what loading a
		  ;; file might do.
		  (set tmp2 current-prefix-arg)
		  (load (, file))
		  ;; check for re-def
		  (if (equal (symbol-value tmp1)
			     (get (quote (, fun)) qkey))
		      (error "%s definition of %s is not defined by loading %s"
			     qkey (quote (, fun)) (, file)))
		  ;; call function
		  (if (interactive-p)
		      (let ((prefix-arg (symbol-value tmp2)))
			(call-interactively
			 (get (quote (, fun)) qkey)))
		    (apply (get (quote (, fun)) qkey) args)))))
	   (` (lambda (&rest args)
		(let ((qkey (intern (symbol-name (quote (, key)))
				    efs-key-obarray))
		      (tmp1 (intern "tmp1" efs-key-obarray)))
		  ;; Need to store the a-f-function, to see if it has been
		  ;; re-defined by the load. This is avoid to an infinite loop.
		  (set tmp1 (get (quote (, fun)) qkey))
		  (load (, file))
		  ;; check for re-def
		  (if (equal (symbol-value tmp1)
			     (get (quote (, fun)) qkey))
		      (error "%s definition of %s is not defined by loading %s"
			     qkey (quote (, fun)) (, file)))
		  ;; call function
		  (apply (get (quote (, fun)) qkey) args)))))))
    (list 'put (list 'quote fun)
	  (list 'intern
		(list 'symbol-name (list 'quote key))
		'efs-key-obarray)
	  (list 'function result))))
 
;;; User level macros -- efs-defun and efs-autoload.

(defmacro efs-defun (funame key args &rest body)
  (let* ((use (and (eq (car-safe key) '&use)
		   (nth 1 key)))
	 (key (and (null use) key))
	 result doc-string interactive-p)
    ;; check args
    (efs-defun-arg-check funame key args)
    ;; extract doc-string
    (if (stringp (car body))
	(setq doc-string  (car body)
	      body (cdr body)))
    ;; If the default fun is interactive, and it's a use construct,
    ;; then we allow the gen fun to be interactive.
    (if use
	(setq interactive-p (eq (car-safe (car-safe body)) 'interactive)))
    (setq result
	  (` ((put (quote (, funame))
		   (intern (symbol-name (quote (, key)))
			   efs-key-obarray)
		   (function
		    (, (cons 'lambda
			     (cons args body)))))
	      (quote (, funame)))))
    ;; if the key is null, make a generic def
    (if (null key)
	(setq result
	      (cons (efs-def-generic
		     funame use doc-string interactive-p)
		    result)))
    ;; return
    (cons 'progn result)))

;;; For lisp-mode

(put 'efs-defun 'lisp-indent-hook 'defun)

;; efs-autoload
;; Allows efs function cells to be defined as autoloads.
;; If efs-autoload inserted autoload objects in the property list,
;; and the funcall mechanism in efs-defun checked for such
;; auto-load objects, we could reduce the size of the code
;; resulting from expanding efs-autoload. However, the expansion
;; of efs-defun would be larger. What is the best thing to do?

(defmacro efs-autoload (fun key file &optional docstring from-kbd)
  (let* ((use (and (eq (car-safe key) '&use)
		   (nth 1 key)))
	 (key (and (null use) key)))
    (efs-defun-arg-check (eval fun) key 'autoload)
    ;; has the function been previously defined?
    (`
     (if (null (get (, fun)
		    (intern (symbol-name (quote (, key)))
			    efs-key-obarray)))
	 (,
	  (if (null key)
	      (list 'progn
		    ;; need to eval fun, since autoload wants an explicit
		    ;; quote built into the fun arg.
		    (efs-def-generic
		     (eval fun) use docstring from-kbd )
		    (efs-def-autoload (eval fun) key file from-kbd)
		    (list 'quote
			  (list
			   'efs-autoload
			   key file docstring from-kbd)))
	    (list 'progn
		  (efs-def-autoload (eval fun) key file from-kbd)
		  (list 'quote
			(list 
			 'efs-autoload
			 key file docstring from-kbd)))))))))

(defun efs-fset (sym key fun)
  ;; Like fset but sets KEY's definition of SYM.
  (put sym (intern (symbol-name key) efs-key-obarray) fun))

(defun efs-fboundp (key fun)
  ;; Like fboundp, but checks for KEY's def.
  (null (null (get fun (intern (symbol-name key) efs-key-obarray)))))

;; If we are going to use autoload objects, the following two functions
;; will be useful.
;;
;; (defun efs-defun-do-autoload (fun file key interactive-p args)
;;   ;; Loads FILE and runs the KEY def of FUN.
;;   (let (fun file key interactive-p args)
;;     (load file))
;;   (let ((new-def (get fun key)))
;;     (if (eq (car-safe new-def) 'autoload)
;; 	(error "%s definition of %s is not defined by loading %s"
;; 	       key fun file)
;;       (if interactive-p
;; 	  (let ((prefix-arg current-predix-arg))
;; 	    (call-interactively fun))
;; 	(apply new-def args)))))
;; 
;; (defun efs-defun-autoload (fun key file doc-string from-kbd)
;;   ;; Sets the KEY def of FUN to an autoload object.
;;   (let* ((key (intern (symbol-name key) efs-key-obarray))
;; 	 (def (get fun key)))
;;     (if (or (null def)
;; 	    (eq (car-safe def) 'autoload))
;; 	(put fun key (list 'autoload file doc-string from-kbd)))))

;;; end of efs-defun.el
