;;; obarray.el --- Lisp-level implementation of traditional emacs obarrays.

;; Copyright (C) 2015-2016 Free Software Foundation

;; Author: Aidan Kehoe

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(symbol-macrolet ((symbol-next-table #s(hash-table :test eq :weakness key)))
  (defalias 'xemacs-intern-in-vector
      #'(lambda (string vector)
          "Return the symbol in VECTOR named STRING.

VECTOR is treated as a traditional emacs obarray. This means a Lisp vector
implementing a hash table, with each vector element corresponding to the start
of a bucket and the elements within the bucket stored as a linked list."
          (let* ((bucket (mod (equal-hash string) (length vector)))
                 (elt (aref vector bucket)) next)
            (if (eql elt 0)
                (aset vector bucket (make-symbol string))
              (while (and (not (equal string (symbol-name elt)))
                          (not (eql (setq next
                                          (gethash elt symbol-next-table 0))
                                    0)))
                (setq elt next))
              (if (not (eql next 0))
                  elt
                (puthash elt (make-symbol string) symbol-next-table))))))
  (defalias 'xemacs-intern-soft-in-vector
      #'(lambda (string vector &optional default)
          "Return the symbol named STRING in VECTOR.

Return the object DEFAULT if no such symbol exits in VECTOR, a traditional
emacs obarray.  See the documentation for `xemacs-intern-in-vector' for an
explanation of what a traditional emacs obarray is."
          (let* ((bucket (mod (equal-hash string) (length vector)))
                 (elt (aref vector bucket)) next)
            (if (eql elt 0)
                default
              (while (and (not (equal string (symbol-name elt)))
                          (not (eql (setq next (gethash elt symbol-next-table
                                                        0))
                                     0)))
                (setq elt next))
              (if (not (eql next 0))
                  elt
                default)))))
  (defalias 'xemacs-unintern-in-vector
      #'(lambda (name vector)
          "Delete the symbol having name NAME from VECTOR.

VECTOR is a traditional emacs obarray.
Return t if a symbol was found or deleted, nil otherwise.
If NAME is a string, delete any symbol with that name.
If NAME is a symbol, delete exactly that symbol if it belongs to
VECTOR, otherwise do nothing.

See the documentation for `xemacs-intern-in-vector' for an explanation of what
a traditional emacs obarray is."
          (let* ((string (if (symbolp name) (symbol-name name) name))
                 (bucket (mod (equal-hash string) (length vector)))
                 (elt (aref vector bucket)) next)
            (if (eql elt 0)
                nil
              (if (symbolp name)
                  (if (eq name elt)
                      (if (eql (setq next (gethash elt symbol-next-table 0))
                               0)
                          (progn
                            (aset vector bucket 0)
                            t)
                        (aset vector bucket next)
                        (remhash elt symbol-next-table)
                        t)
                    (while (and (not (eql (setq next
                                                (gethash elt symbol-next-table 
                                                         0))
                                          0))
                                (not (eq next name)))
                      (setq elt next))
                    (when (not (eq elt next))
                      (setf (gethash elt symbol-next-table)
                            (gethash next symbol-next-table))
                      (remhash next symbol-next-table)
                      t))
                (if (equal name (symbol-name elt))
                    (if (setq next (gethash elt symbol-next-table))
                        (progn
                          (aset vector bucket next)
                          (remhash elt symbol-next-table)
                          t)
                      (aset vector bucket 0)
                      t)
                  (while (and (setq next (gethash elt
                                                  symbol-next-table))
                              (not (equal (symbol-name next) name)))
                    (setq elt next))
                  (when next
                    (setf (gethash elt symbol-next-table)
                          (gethash next symbol-next-table))
                    (remhash next symbol-next-table)
                    t)))))))
  (defalias 'xemacs-mapatoms-in-vector
      #'(lambda (#1=#:function #2=#:vector)
          "Map FUNCTION across the symbols in VECTOR.

VECTOR is treated as a traditional emacs obarray. FUNCTION must
accept one argument.

See the documentation for `xemacs-intern-in-vector' for an
explanation of what a traditional emacs obarray is."
          (loop for #3=#:elt across #2#
                do (while (not (eql #3# 0))
                     (funcall #1# #3#)
                     (setq #3# (gethash #3# symbol-next-table 0))))))
  (defalias 'xemacs-next-iteration-in-vector
      (function*
       (lambda (vector &optional (last nil given))
        ;; Used in map_completion_trad_obarray in minibuf.c
        "Return the next symbol in VECTOR, a traditional emacs obarray.
LAST is the value given by the last call to this function. If LAST is not
supplied, return the first symbol in VECTOR.
Return the fixnum 0 if there are no remaining symbols in VECTOR.

See the documentation for `xemacs-intern-in-vector' for an
explanation of what a traditional emacs obarray is."
        (let ((next (if given (gethash last symbol-next-table 0) 0))
              bucket length)
          (if (symbolp next)
              next
            (if given
                (progn
                  (setq bucket (1+ (mod (equal-hash (symbol-name last))
					(setq length (length vector)))))
                  (while (and (< bucket length) (eql 0 (aref vector bucket)))
                    (incf bucket))
                  (if (< bucket length) (aref vector bucket) 0))
              (setq bucket 0
                    length (length vector))
              (while (and (< bucket length) (eql 0 (aref vector bucket)))
                (incf bucket))
              (if (< bucket length) (aref vector bucket) 0))))))))

;; The following are from GNU's ill-conceived obarray.el:

(defconst obarray-default-size 59
  "The value 59 is an arbitrary prime number that gives a good hash.")

(defun obarray-make (&optional size)
  "Return a new obarray of size SIZE or `obarray-default-size'."
  (let ((size (or size obarray-default-size)))
    (if (< 0 size)
        (make-vector size 0)
      (signal 'wrong-type-argument '(size 0)))))
(make-compatible 'obarray-make 'make-hash-table)

(defun obarrayp (object)
  "Return t if OBJECT is an obarray."
  (and (vectorp object)
       (< 0 (length object))))
(make-compatible 'obarrayp
                 "Use a real hash table and `hash-table-p'")

;; Don’t use obarray as a variable name to avoid shadowing.
(defun obarray-get (ob name)
  "Return symbol named NAME if it is contained in obarray OB.
Return nil otherwise."
  (xemacs-intern-soft-in-vector name ob))
(make-compatible 'obarray-get 'gethash)

(defun obarray-put (ob name)
  "Return symbol named NAME from obarray OB.
Creates and adds the symbol if doesn't exist."
  (xemacs-intern-in-vector name ob))
(make-compatible 'obarray-put 'puthash)

(defun obarray-remove (ob name)
  "Remove symbol named NAME if it is contained in obarray OB.
Return t on success, nil otherwise."
  (xemacs-unintern-in-vector name ob))
(make-compatible 'obarray-remove 'remhash)

(defun obarray-map (fn ob)
  "Call function FN on every symbol in obarray OB and return nil."
  (xemacs-mapatoms-in-vector fn ob))
(make-compatible 'obarray-map 'maphash)

(provide 'obarray)

;;; obarray.el ends here
