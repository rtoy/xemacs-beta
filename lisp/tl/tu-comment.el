;;; tu-comment.el --- a comment out utility for Lisp programs.

;; Copyright (C) 1995,1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/10/27
;; Version: $Id: tu-comment.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
;; Keywords: comment, Lisp

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - How to install.
;;   1. bytecompile this file and copy it to the apropriate directory.
;;   2. put the following lines to your ~/.emacs:
;;		(autoload 'comment-sexp "tu-comment" nil t)
;;		(global-set-key "\C-c\C-q" 'comment-sexp)
;; - How to use.
;;      type `C-c C-q' at the beginning of S-expression you want to
;;      comment out.

;;; Code:

(defvar comment-sexp-first-line-method-alist
  '((emacs-lisp-mode       . comment-sexp-middle-line-method-for-lisp)
    (lisp-interaction-mode . comment-sexp-middle-line-method-for-lisp)
    (lisp-mode             . comment-sexp-middle-line-method-for-lisp)
    (scheme-mode           . comment-sexp-middle-line-method-for-lisp)
    (c-mode                . comment-sexp-first-line-method-for-c)
    (c++-mode              . comment-sexp-middle-line-method-for-c++)
    ))

(defvar comment-sexp-middle-line-method-alist
  '((emacs-lisp-mode       . comment-sexp-middle-line-method-for-lisp)
    (lisp-interaction-mode . comment-sexp-middle-line-method-for-lisp)
    (lisp-mode             . comment-sexp-middle-line-method-for-lisp)
    (scheme-mode           . comment-sexp-middle-line-method-for-lisp)
    (c-mode                . comment-sexp-middle-line-method-for-c)
    (c++-mode              . comment-sexp-middle-line-method-for-c++)
    ))

(defvar comment-sexp-last-line-method-alist
  '((emacs-lisp-mode       . comment-sexp-last-line-method-for-dummy)
    (lisp-interaction-mode . comment-sexp-last-line-method-for-dummy)
    (lisp-mode             . comment-sexp-last-line-method-for-dummy)
    (scheme-mode           . comment-sexp-last-line-method-for-dummy)
    (c-mode                . comment-sexp-last-line-method-for-c)
    (c++-mode              . comment-sexp-last-line-method-for-dummy)
    ))

(defun comment-sexp-middle-line-method-for-lisp ()
  (insert ";; ")
  )

(defun comment-sexp-middle-line-method-for-c++ ()
  (insert "// ")
  )

(defun comment-sexp-first-line-method-for-c ()
  (insert "/* ")
  )

(defun comment-sexp-middle-line-method-for-c ()
  (insert " * ")
  )

(defun comment-sexp-last-line-method-for-c (c)
  (insert "\n")
  (while (< 0 c)
    (insert " ")
    (setq c (1- c))
    )
  (insert " */")
  )

(defun comment-sexp-last-line-method-for-dummy (c))

(defun comment-sexp ()
  (interactive)
  (let ((c (current-column))
        (b (save-excursion
             (beginning-of-line)
             (point)))
        (e (save-excursion
             (forward-sexp)
             (point)
             ))
        )
    (save-excursion
      (save-restriction
        (narrow-to-region b e)
        (untabify b e)
        
        (beginning-of-line)
        (move-to-column c)
        (funcall
         (cdr (assq major-mode comment-sexp-first-line-method-alist)))
        (forward-line)
        
        (while (< (point) (point-max))
          (beginning-of-line)
          (move-to-column c)
          (funcall
           (cdr (assq major-mode comment-sexp-middle-line-method-alist)))
          (forward-line)
          )
        
        (funcall
         (cdr (assq major-mode comment-sexp-last-line-method-alist)) c)
        ))))

;;; tu-comment.el ends here
