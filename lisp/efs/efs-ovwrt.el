;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-ovwrt.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.2 $
;; RCS:
;; Description:  Utilities for overwriting functions with new definitions.
;; Author:       Andy Norman <ange@hplb.hpl.hp.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Although used by efs, these utilities could be of general use to other
;;; packages too. Keeping them separate from the main efs program
;;; makes it easier for other programs to require them.

(provide 'efs-ovwrt)
(eval-when-compile
  (condition-case nil
      (require 'advice)
    (error)))

(defconst efs-ovwrt-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.2 $" 11 -2)))

(defvar efs-overwrite-fmt
  "Note: This function has been modified to work with %s.")

;; Make the byte compiler happy.
(defvar file-name-handler-alist)
(defvar inhibit-file-name-handlers)
(defvar inhibit-file-name-operation)

(defun efs-safe-documentation (fun)
  "A documentation function that isn't quite as fragile."
  (condition-case ()
      (documentation fun)
    (error nil)))

(defun efs-overwrite-fn (package fun &optional newfun)
  "Overwrites a function with a new definition from PACKAGE.
PACKAGE should be a string. The the function to be overwritten is FUN.
The new definition is obtained from the optional NEWFUN. If ommitted,
NEWFUN is taken to be PACKAGE-FUN. The original definition is stored in
PACKAGE-real-FUN. The original documentation is placed on the new
definition suitably augmented."
  (let* ((name (symbol-name fun))
	 (saved (intern (concat package "-real-" name)))
	 (new (or newfun (intern (concat package "-" name))))
	 (nfun (symbol-function new))
	 (exec-directory (if (or (equal (nth 3 command-line-args) "dump")
				 (equal (nth 4 command-line-args) "dump"))
			     "../etc/"
			   exec-directory)))			 
    
    (while (symbolp nfun)
      (setq nfun (symbol-function nfun)))
    
    ;; Interpose the new function between the function symbol and the
    ;; original definition of the function symbol AT TIME OF FIRST LOAD.
    ;; We must only redefine the symbol-function of FUN the very first
    ;; time, to avoid blowing away stuff that overloads FUN after this.
    
    ;; We direct the function symbol to the new function symbol
    ;; rather than function definition to allow reloading of this file or
    ;; redefining of the individual function (e.g., during debugging)
    ;; later after some other code has been loaded on top of our stuff.
    
    (or (fboundp saved)
	(let ((advised-p (and (featurep 'advice)
			      (ad-is-advised fun))))
	  (if advised-p (ad-deactivate fun))
	  (fset saved (symbol-function fun))
	  (fset fun new)
	  (if advised-p (ad-activate fun))))
    
    ;; Rewrite the doc string on the new function.  This should
    ;; be done every time the file is loaded (or a function is redefined),
    ;; because the underlying overloaded function may have changed its doc
    ;; string.
    
    (let* ((doc-str (efs-safe-documentation saved))
	   (ndoc-str (concat doc-str (and doc-str "\n")
			     (format efs-overwrite-fmt package))))
      
      (cond ((listp nfun)
	     ;; Probe to test whether function is in preloaded read-only
	     ;; memory, and if so make writable copy:
	     (condition-case nil
		 (setcar nfun (car nfun))
	       (error
		(setq nfun (copy-sequence nfun)) ; shallow copy only
		(fset new nfun)))
	     (let ((ndoc-cdr (nthcdr 2 nfun)))
	       (if (stringp (car ndoc-cdr))
		   ;; Replace the existing docstring.
		   (setcar ndoc-cdr ndoc-str)
		 ;; There is no docstring.  Insert the overwrite msg.
		 (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
		 (setcar ndoc-cdr (format efs-overwrite-fmt package)))))
	    (t
	     ;; it's an emacs19 compiled-code object
	     ;;
	     ;; XEmacs: can't use append on a compiled function
	     ;; as the latter is no longer a vector.  Use the
	     ;; accessor functions instead.
	     (let ((new-code (nconc
			      (list (compiled-function-arglist nfun)
				    (compiled-function-instructions nfun)
				    (compiled-function-constants nfun)
				    (compiled-function-stack-depth nfun)
				    ndoc-str)
			      (if (compiled-function-interactive nfun)
				  (list (compiled-function-interactive nfun))
				nil))))
	       (fset new (apply 'make-byte-code new-code))))))))


;;; end of efs-ovwrt.el
