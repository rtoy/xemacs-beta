;; edebug-cl-read.el  - Edebug reader macros for use with cl-read.

;; Copyright (C) 1993 Daniel LaLiberte
;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Keywords: lisp, tools, maint

;; LCD Archive Entry:
;; edebug-cl-read.el|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |Edebug reader macros for cl-read.el
;; |$Date: 1996/12/18 03:33:27 $|$Revision: 1.1.1.1 $|~/modes/edebug-cl-read.el|

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Commentary:

;; If you use cl-read.el and want to use edebug with any code
;; in a file written with CL read syntax, then you need to use this
;; package.

;; To Do:
;; Handle shared structures, but this is not normally used in executable code.

;; Read-time evaluation shouldn't be used in a form argument since
;; there is no way to instrument the result of the evaluation, and
;; no way to tell Edebug not to try.  

;; Need to mangle all local variable names that might be visible to
;; eval, e.g. stream, char.  Alternatively, packages could hide them.

(require 'cl)
;; For byte compiling cl-read is needed.
;; But edebug-cl-read should not even be loaded unless cl-read already is.
(require 'cl-read)

(provide 'edebug-cl-read)
;; Do the above provide before the following require to avoid load loop.
(require 'edebug)

(defvar reader::stack)

;; The following modifications of reader functions
;; could be done via advice.  But we need to switch between
;; edebug versions and originals frequently.  Also advice.el 
;; doesn't support advising anonymous functions.

(defun edebug-reader::read-sexp-func (point func)
  ;; dummy def
  )

(defvar edebug-read-dotted-list)

(defun edebug-read-sexp-func (point func)
  "Edebug offset storing is happening."
  (edebug-storing-offsets point
    (let (edebug-read-dotted-list)
      (edebug-reader::read-sexp-func point func))))

(defun edebug-end-list-handler (stream char)
  ;; If the dotted form is a list, signal to offset routines.
  (setq edebug-read-dotted-list (listp (car reader::stack)))
  (edebug-reader::end-list-handler stream char))


;;=========================================================================
;; Redefine the edebug reader to check whether CL syntax is active.
;; This might be a little cleaner using advice.

(defvar edebug-reading-with-cl-read nil)

(or (fboundp 'edebug-original-read-storing-offsets)
    (defalias 'edebug-original-read-storing-offsets
      (symbol-function 'edebug-read-storing-offsets)))

(defun edebug-read-storing-offsets (stream)
  ;; Read a sexp from STREAM.
  ;; STREAM is limited to the current buffer.
  ;; Create a parallel offset structure as described in doc for edebug-offsets.
  ;; This version, from edebug-cl-read, uses cl-read.
  (if (not cl-read-active)
      ;; Use the reader for standard Emacs Lisp.
      (edebug-original-read-storing-offsets stream)
    
    ;; Use cl-read with edebug hooks.
    (if edebug-reading-with-cl-read nil
      ;; Only do this if it's not already been done, else it loops.
      (fset 'edebug-reader::read-sexp-func
	    (symbol-function 'reader::read-sexp-func))
      (fset 'reader::read-sexp-func 'edebug-read-sexp-func)
      (fset 'edebug-reader::end-list-handler (get-macro-character ?\)))
      (set-macro-character ?\) 'edebug-end-list-handler)))
    (unwind-protect
	(let ((edebug-reading-with-cl-read t))
	  (reader::read stream))
      (if edebug-reading-with-cl-read nil
	(set-macro-character 
	 ?\) (symbol-function 'edebug-reader::end-list-handler))
	(fset 'reader::read-sexp-func
	      (symbol-function 'edebug-reader::read-sexp-func)))))

