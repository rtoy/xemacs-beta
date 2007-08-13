;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cpat.el --

;;; This file is part of ILISP.
;;; Version: 5.7
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995 Marco Antoniotti and Rick Busdiecker
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@lehman.com' to be included in the
;;; ILISP mailing list. 'ilisp@lehman.com' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;;
;;;
;;; Compatability between GNU emacs 18, 19, and Lucid emacs 19.
;;;
;;;
(defconst ilisp-emacs-version-id
  (cond ((string-match "Lucid" emacs-version)
	 (if (string-match "^19.[0-7][^0-9]" emacs-version)
	     'lucid-19
	   'lucid-19-new))
	((string-match "^19" emacs-version)
	 'gnu-19)
	(t 'gnu-18))
  "What version of emacs we are running.")


;; Hook stuff--this should really be a part of emacs-lisp anyway
      
(defun ilisp-member (elt list)
  (let ((result nil))
    (while list
      (cond ((equal elt (car list))
	     (setq result list
		   list nil))
	    (t
	     (setq list (cdr list)))))
    result))
	

(defun ilisp-add-hook (hook function)
  "Arguments are HOOK and FUNCTION. Add FUNCTION to HOOK's list.
FUNCTION is not added if it's already on the list."
  (set hook
       (if (boundp hook)
	   (let ((value (symbol-value hook)))
	     (if (and value (or (not (consp value)) (eq (car value) 'lambda)))
		 (setq value (cons value nil)))
	     (if (not (ilisp-member function value))
		 (setq value (append value (list function))))
	     value)
	 (list function))))

(if (not (fboundp 'add-hook))
    (fset 'add-hook 'ilisp-add-hook))


;;; 'ilisp-where-is' has been moved (and corrected) to ilisp-key.el.

;;;
;;; COMINT 
;;;
;;; GNU, Lucid and 18 use different versions of comint with
;;; incompatible interface variables and functions.  Hooray.
;;;

;; Some very old COMINT versions are missing these.
(if (not (boundp 'comint-input-chunk-size))
    (setq comint-input-chunk-size 512))
(if (not (boundp 'comint-ptyp))
    (setq comint-ptyp t))


(defun ilisp-get-input-ring ()
  "Use instead of get-input-ring coming-input-ring or input-ring."
  (cond ((eq ilisp-emacs-version-id 'lucid-19)
	 (get-input-ring))
	((or (eq ilisp-emacs-version-id 'gnu-19)
	     (eq ilisp-emacs-version-id 'lucid-19-new))
	 comint-input-ring)
	(t input-ring)))

(defun ilisp-ring-insert (ring input)
  (if (eq ilisp-emacs-version-id 'lucid-19)
      (ring-insert-new ring input)
      (ring-insert ring input)))

(defun ilisp-temp-buffer-show-function ()
  (if (eq ilisp-emacs-version-id 'gnu-18) 
      temp-buffer-show-hook
    temp-buffer-show-function))

(defun ilisp-input-ring-index ()
  (if (or (eq ilisp-emacs-version-id 'gnu-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      comint-input-ring-index
    input-ring-index))

(defun set-ilisp-input-ring-index (n)
  (if (or (eq ilisp-emacs-version-id 'gnu-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      (setq comint-input-ring-index n)
    (setq input-ring-index n)))

(defun ilisp-input-ring-size ()
  (if (or (eq ilisp-emacs-version-id 'gnu-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      comint-input-ring-size
    input-ring-size))

(defun set-ilisp-input-ring-size (n)
  (if (or (eq ilisp-emacs-version-id 'gnu-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      (setq comint-input-ring-size n)
    (setq input-ring-size n)))
