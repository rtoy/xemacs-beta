;;!emacs
;;
;; FILE:         python-browse.el
;; SUMMARY:      Python source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools, python
;;
;; AUTHOR:       Harri Pasanen, based on Smalltalk and C++ browsers 
;;               by Bob Weiner
;; ORG:          Tekla Oy
;;
;; ORIG-DATE:    5-Apr-96
;; LAST-MOD:     12-Apr-96
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'Python-browse' to invoke the Python OO-Browser.  Prefix arg
;;    prompts for name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-python-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun python-browse (&optional env-file no-ui)
  "Invoke the Python OO-Browser.
This allows browsing through Python library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix python-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix python-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal python-env-file env-file)
		       (and (null env-file)
			    (or python-lib-search-dirs python-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (python-browse-setup) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p python-env-file)
	    (br-env-create python-env-file python-lang-prefix))
	(or env-file (setq env-file python-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(python-browse-setup)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  python-env-file load-succeeded
		  python-sys-search-dirs br-sys-search-dirs
		  python-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (br-init)
	   (or no-ui (br-browse)))
	  (no-ui nil)
	  (t (message "(python-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(fset 'python-class-list-filter 'identity)


;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun python-browse-setup ()
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  ;; Use this until an info function is implemented for the language.
  ;; (fmakunbound 'br-insert-class-info)
  (fset 'br-store-class-info 'python-store-class-info)
  (fset 'br-lang-mode
	(cond ((featurep 'python-mode) 'python-mode)
	      ((load "python-mode" 'missing-ok 'nomessage)
	       (provide 'python-mode))
	      (t 'fundamental-mode)))
  (br-setup-constants)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'python-add-default-classes)
    (setq br-after-build-sys-hook '(python-add-default-classes))))


(provide 'python-browse)
