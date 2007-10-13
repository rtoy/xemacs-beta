;;; coding.el --- Coding-system functions for XEmacs.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2000, 2001, 2002 Ben Wing.

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; split off of mule.el.

;;; Code:

(globally-declare-fboundp
 '(coding-system-lock-shift
   coding-system-seven coding-system-charset charset-dimension))

(defalias 'check-coding-system 'get-coding-system)

(defun modify-coding-system-alist (target-type regexp coding-system)
  "Modify one of look up tables for finding a coding system on I/O operation.
There are three of such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.

TARGET-TYPE specifies which of them to modify.
If it is `file', it affects `file-coding-system-alist' (which see).
If it is `process', it affects `process-coding-system-alist' (which see).
If it is `network', it affects `network-coding-system-alist' (which see).

REGEXP is a regular expression matching a target of I/O operation.
The target is a file name if TARGET-TYPE is `file', a program name if
TARGET-TYPE is `process', or a network service name or a port number
to connect to if TARGET-TYPE is `network'.

CODING-SYSTEM is a coding system to perform code conversion on the I/O
operation, or a cons cell (DECODING . ENCODING) specifying the coding systems
for decoding and encoding respectively,
or a function symbol which, when called, returns such a cons cell."
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (or (stringp regexp)
      (and (eq target-type 'network) (integerp regexp))
      (error "Invalid regular expression: %s" regexp))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (cond ((eq target-type 'file)
	 (let ((slot (assoc regexp file-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq file-coding-system-alist
		   (cons (cons regexp coding-system)
			 file-coding-system-alist)))))
	((eq target-type 'process)
	 (let ((slot (assoc regexp process-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq process-coding-system-alist
		   (cons (cons regexp coding-system)
			 process-coding-system-alist)))))
	(t
	 (let ((slot (assoc regexp network-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq network-coding-system-alist
		   (cons (cons regexp coding-system)
			 network-coding-system-alist)))))))

(defsubst keyboard-coding-system ()
  "Return coding-system of what is sent from terminal keyboard."
  keyboard-coding-system)

(defun set-keyboard-coding-system (coding-system)
  "Set the coding system used for TTY keyboard input. Currently broken."
  (interactive "zkeyboard-coding-system: ")
  (get-coding-system coding-system) ; correctness check
  (setq keyboard-coding-system coding-system)
  (if (eq (device-type) 'tty)
      (declare-fboundp (set-console-tty-input-coding-system
			(device-console) keyboard-coding-system)))
  (redraw-modeline t))

(defsubst terminal-coding-system ()
  "Return coding-system of your terminal."
  terminal-coding-system)

(defun set-terminal-coding-system (coding-system)
  "Set the coding system used for TTY display output. Currently broken."
  (interactive "zterminal-coding-system: ")
  (get-coding-system coding-system) ; correctness check
  (setq terminal-coding-system coding-system)
  ; #### should this affect all current tty consoles ?
  (if (eq (device-type) 'tty)
      (declare-fboundp (set-console-tty-output-coding-system
			(device-console) terminal-coding-system)))
  (redraw-modeline t))

(defun what-coding-system (start end &optional arg)
  "Show the encoding of text in the region.
This function is meant to be called interactively;
from a Lisp program, use `detect-coding-region' instead."
  (interactive "r\nP")
  (princ (detect-coding-region start end)))

(defun decode-coding-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
Does not modify STR.  Returns the decoded string on successful conversion."
  (with-string-as-buffer-contents
   str (decode-coding-region (point-min) (point-max) coding-system)))

(defun encode-coding-string (str coding-system)
  "Encode the string STR using CODING-SYSTEM.
Does not modify STR.  Returns the encoded string on successful conversion."
  (with-string-as-buffer-contents
   str (encode-coding-region (point-min) (point-max) coding-system)))


;;;; Coding system accessors

(defun coding-system-mnemonic (coding-system)
  "Return the 'mnemonic property of CODING-SYSTEM."
  (coding-system-property coding-system 'mnemonic))

(defun coding-system-documentation (coding-system)
  "Return the 'documentation property of CODING-SYSTEM."
  (coding-system-property coding-system 'documentation))

(define-obsolete-function-alias 'coding-system-doc-string
  'coding-system-description)

(defun coding-system-eol-type (coding-system)
  "Return the 'eol-type property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-type))

(defun coding-system-eol-lf (coding-system)
  "Return the 'eol-lf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-lf))

(defun coding-system-eol-crlf (coding-system)
  "Return the 'eol-crlf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-crlf))

(defun coding-system-eol-cr (coding-system)
  "Return the 'eol-cr property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-cr))

(defun coding-system-post-read-conversion (coding-system)
  "Return the 'post-read-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'post-read-conversion))

(defun coding-system-pre-write-conversion (coding-system)
  "Return the 'pre-write-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'pre-write-conversion))

;;; #### bleagh!!!!!!!

(defun coding-system-get (coding-system prop)
  "Extract a value from CODING-SYSTEM's property list for property PROP."
  (or (plist-get
       (get (coding-system-name coding-system) 'coding-system-property)
       prop)
      (condition-case nil
	  (coding-system-property coding-system prop)
	(error nil))))

(defun coding-system-put (coding-system prop value)
  "Change value in CODING-SYSTEM's property list PROP to VALUE."
  (put (coding-system-name coding-system)
       'coding-system-property
       (plist-put (get (coding-system-name coding-system)
		       'coding-system-property)
		  prop value)))

(defun coding-system-category (coding-system)
  "Return the coding category of CODING-SYSTEM."
  (or (coding-system-get coding-system 'category)
      (case (coding-system-type coding-system)
	(no-conversion 'no-conversion)
	(shift-jis 'shift-jis)
	(unicode (case (coding-system-property coding-system 'unicode-type)
		   (utf-8 (let ((bom (coding-system-property coding-system
							     'need-bom)))
			    (cond (bom 'utf-8-bom)
				  ((not bom) 'utf-8))))
		   (ucs-4 'ucs-4)
		   (utf-16 (let ((bom (coding-system-property coding-system
							      'need-bom))
				 (le (coding-system-property coding-system
							     'little-endian)))
			     (cond ((and bom le) 'utf-16-little-endian-bom)
				   ((and bom (not le) 'utf-16-bom))
				   ((and (not bom) le) 'utf-16-little-endian)
				   ((and (not bom) (not le) 'utf-16)))))))
	(big5 'big5)
	(iso2022 (cond ((coding-system-lock-shift coding-system)
			'iso-lock-shift)
		       ((coding-system-seven coding-system)
			'iso-7)
		       (t
			(let ((dim 0)
			      ccs
			      (i 0))
			  (while (< i 4)
			    (setq ccs (declare-fboundp
				       (coding-system-iso2022-charset
					coding-system i)))
			    (if (and ccs
				     (> (charset-dimension ccs) dim))
				(setq dim (charset-dimension ccs))
			      )
			    (setq i (1+ i)))
			  (cond ((= dim 1) 'iso-8-1)
				((= dim 2) 'iso-8-2)
				(t 'iso-8-designate))))))
	)))


;;; Make certain variables equivalent to coding-system aliases
(defun dontusethis-set-value-file-name-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'file-name (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'file-name-coding-system
 'set-value
 'dontusethis-set-value-file-name-coding-system-handler)

(defun dontusethis-set-value-terminal-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'terminal (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'terminal-coding-system
 'set-value
 'dontusethis-set-value-terminal-coding-system-handler)

(defun dontusethis-set-value-keyboard-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'keyboard (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'keyboard-coding-system
 'set-value
 'dontusethis-set-value-keyboard-coding-system-handler)

(when (not (featurep 'mule))
  (define-coding-system-alias 'escape-quoted 'binary)
  ;; these are so that gnus and friends work when not mule
  (define-coding-system-alias 'iso-8859-1 'no-conversion)
  ;; We're misrepresenting ourselves to the gnus code by saying we support
  ;; both.
  ; (define-coding-system-alias 'iso-8859-2 'no-conversion)
  (define-coding-system-alias 'ctext 'binary))

(make-compatible-variable 'enable-multibyte-characters "Unimplemented")

;;; coding.el ends here
