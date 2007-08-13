;;; tl-str.el --- Emacs Lisp Library module about string

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: tl-str.el,v 1.3 1997/01/30 02:22:47 steve Exp $
;; Keywords: string

;; This file is part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'emu)
(require 'tl-list)


;;; @ converter
;;;

(defun expand-char-ranges (str)
  (let ((i 0)
	(len (length str))
	chr pchr nchr
	(dest ""))
    (while (< i len)
      (setq chr (elt str i))
      (cond ((and pchr (eq chr ?-))
	     (setq pchr (1+ pchr))
	     (setq i (1+ i))
	     (setq nchr (elt str i))
	     (while (<= pchr nchr)
	       (setq dest (concat dest (char-to-string pchr)))
	       (setq pchr (1+ pchr))
	       )
	     )
	    (t
	     (setq dest (concat dest (char-to-string chr)))
	     ))
      (setq pchr chr)
      (setq i (1+ i))
      )
    dest))


;;; @ space
;;;

(defun eliminate-top-spaces (str)
  "Eliminate top sequence of space or tab and return it. [tl-str.el]"
  (if (string-match "^[ \t]+" str)
      (substring str (match-end 0))
    str))

(defun eliminate-last-spaces (str)
  "Eliminate last sequence of space or tab and return it. [tl-str.el]"
  (if (string-match "[ \t]+$" str)
      (substring str 0 (match-beginning 0))
    str))

(defun replace-space-with-underline (str)
  (mapconcat (function
	      (lambda (arg)
		(char-to-string
		 (if (eq arg ?\ )
		     ?_
		   arg)))) str "")
  )


;;; @ version
;;;

(defun version-to-list (str)
  (if (string-match "[0-9]+" str)
      (let ((dest
	     (list
	      (string-to-number
	       (substring str (match-beginning 0)(match-end 0))
	       ))))
	(setq str (substring str (match-end 0)))
	(while (string-match "^\\.[0-9]+" str)
	  (setq dest
		(cons
		 (string-to-number
		  (substring str (1+ (match-beginning 0))(match-end 0)))
		 dest))
	  (setq str (substring str (match-end 0)))
	  )
	(nreverse dest)
	)))

(defun version< (v1 v2)
  (or (listp v1)
      (setq v1 (version-to-list v1))
      )
  (or (listp v2)
      (setq v2 (version-to-list v2))
      )
  (catch 'tag
    (while (and v1 v2)
      (cond ((< (car v1)(car v2))
	     (throw 'tag v2)
	     )
	    ((> (car v1)(car v2))
	     (throw 'tag nil)
	     ))
      (setq v1 (cdr v1)
	    v2 (cdr v2))
      )
    v2))

(defun version<= (v1 v2)
  (or (listp v1)
      (setq v1 (version-to-list v1))
      )
  (or (listp v2)
      (setq v2 (version-to-list v2))
      )
  (catch 'tag
    (while (and v1 v2)
      (cond ((< (car v1)(car v2))
	     (throw 'tag v2)
	     )
	    ((> (car v1)(car v2))
	     (throw 'tag nil)
	     ))
      (setq v1 (cdr v1)
	    v2 (cdr v2))
      )
    (or v2 (and (null v1)(null v2)))
    ))

(defun version> (v1 v2)
  (or (listp v1)
      (setq v1 (version-to-list v1))
      )
  (or (listp v2)
      (setq v2 (version-to-list v2))
      )
  (catch 'tag
    (while (and v1 v2)
      (cond ((> (car v1)(car v2))
	     (throw 'tag v1)
	     )
	    ((< (car v1)(car v2))
	     (throw 'tag nil)
	     ))
      (setq v1 (cdr v1)
	    v2 (cdr v2))
      )
    v1))

(defun version>= (v1 v2)
  (or (listp v1)
      (setq v1 (version-to-list v1))
      )
  (or (listp v2)
      (setq v2 (version-to-list v2))
      )
  (catch 'tag
    (while (and v1 v2)
      (cond ((> (car v1)(car v2))
	     (throw 'tag v1)
	     )
	    ((< (car v1)(car v2))
	     (throw 'tag nil)
	     ))
      (setq v1 (cdr v1)
	    v2 (cdr v2))
      )
    (or v1 (and (null v1)(null v2)))
    ))


;;; @ RCS version
;;;

(defun get-version-string (id)
  "Return a version-string from RCS ID. [tl-str.el]"
  (and (string-match ",v \\([0-9][0-9.][0-9.]+\\)" id)
       (substring id (match-beginning 1)(match-end 1))
       ))


;;; @ file name
;;;

(defun file-name-non-extension (filename)
  (if (string-match "\\.[^.]+$" filename)
      (substring filename 0 (match-beginning 0))
    filename))

(autoload 'replace-as-filename "filename"
  "Return safety filename from STRING.")


;;; @ symbol
;;;

(defun symbol-concat (&rest args)
  "Return a symbol whose name is concatenation of arguments ARGS
which are string or symbol. [tl-str.el]"
  (intern (apply (function concat)
		 (mapcar (function
			  (lambda (s)
			    (cond ((symbolp s) (symbol-name s))
				  ((stringp s) s)
				  )
			    ))
			 args)))
  )


;;; @ matching
;;;

(defun top-string-match (pat str)
  "Return a list (MATCHED REST) if string PAT is top substring of
string STR. [tl-str.el]"
  (if (string-match
       (concat "^" (regexp-quote pat))
       str)
      (list pat (substring str (match-end 0)))
    ))

(defun middle-string-match (pat str)
  "Return a list (PREVIOUS MATCHED REST) if string PAT is found in
string STR. [tl-str.el]"
  (if (equal pat str)
      (list nil pat nil)
    (if (string-match (regexp-quote pat) str)
	(let ((b (match-beginning 0))
	      (e (match-end 0)) )
	  (list (if (not (= b 0))
		    (substring str 0 b)
		  )
		pat
		(if (> (length str) e)
		    (substring str e)
		  )
		)))))

(defun re-top-string-match (pat str)
  "Return a list (MATCHED REST) if regexp PAT is matched as top
substring of string STR. [tl-str.el]"
  (if (string-match (concat "^" pat) str)
      (let ((e (match-end 0)))
	(list (substring str 0 e)(substring str e))
	)))


;;; @ compare
;;;

(defun string-compare-from-top (str1 str2)
  (let* ((len1 (length str1))
	 (len2 (length str2))
	 (len (min len1 len2))
	 (p 0)
	 c1 c2)
    (while (and (< p len)
		(progn
		  (setq c1 (sref str1 p)
			c2 (sref str2 p))
		  (eq c1 c2)
		  ))
      (setq p (+ p (char-length c1)))
      )
    (and (> p 0)
	 (let ((matched (substring str1 0 p))
	       (r1 (and (< p len1)(substring str1 p)))
	       (r2 (and (< p len2)(substring str2 p)))
	       )
	   (if (eq r1 r2)
	       matched
	     (list 'seq matched (list 'or r1 r2))
	     )))))


;;; @ regexp
;;;

(defun regexp-* (regexp)
  (concat regexp "*"))

(defun regexp-or (&rest args)
  (concat "\\(" (mapconcat (function identity) args "\\|") "\\)"))


;;; @ end
;;;

(provide 'tl-str)

;;; tl-str.el ends here
