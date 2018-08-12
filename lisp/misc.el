;;; misc.el --- miscellaneous functions for XEmacs

;; Copyright (C) 1989, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with XEmacs.

;; 06/11/1997 - Use char-(after|before) instead of
;;  (following|preceding)-char. -slb

;;; Code:

(defun copy-from-above-command (&optional arg)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column))
	n
	(string ""))
    (save-excursion
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward "\ \t\n")
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (eq (char-before (point)) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\ ))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))


;;; Weak boxes, formerly in data.c, but never used enough to merit a C
;;; implementation. There is minimal performance and GC impact to this Lisp
;;; implementation (GC performance actually improves, as in the vast majority
;;; of the time, when there are no weak boxes, prune_weak_boxes() isn't
;;; called.) In debugging terms, however, there is no error on attempting to
;;; print readably, as the C implementation gave, and the print method isn't
;;; as pretty as it was with the C version.

;; Quiet the compiler about calls to this:
(autoload 'weak-box-ref-1 "misc")

(defun weak-box-p (object)
  "Return non-nil if OBJECT is a weak box."
  (and (vectorp object) (eql (length object) 3)
       (eq 'cl-weak-box (aref object 0))))

(defun make-weak-box (contents)
  "Return a new weak box from value CONTENTS.
The weak box is a reference to CONTENTS which may be extracted with
`weak-box-ref'.  However, the weak box does not contribute to the
reachability of CONTENTS.  When CONTENTS is garbage-collected,
`weak-box-ref' will return NIL."
  (symbol-macrolet ((all-weak-boxes #:all-weak-boxes))
    (defvar all-weak-boxes)
    (caar (set-weak-list-list
           (load-time-value
            (progn
              (setq all-weak-boxes (make-weak-list 'assoc))
              (defalias 'weak-box-ref-1
                  #'(lambda (weak-box)
                      (cdr (assq weak-box (weak-list-list all-weak-boxes)))))
              all-weak-boxes))
           ;; The #'eq-hash call isn't necessary, but it does mean that weak
           ;; boxes that refer to distinct objects print distinctly. The
           ;; #'type-of call is intended to give us a bit more context as to
           ;; what's going on, without the need to fire up GDB to convert the
           ;; eq-hash to an address.
           (acons (vector 'cl-weak-box (type-of contents) (eq-hash contents))
                  contents (weak-list-list all-weak-boxes))))))

(defun weak-box-ref (weak-box)
  "Return the contents of weak box WEAK-BOX.
If the contents have been GCed, return NIL."
  (check-argument-type 'weak-box-p weak-box)
  (weak-box-ref-1 weak-box))

;;; misc.el ends here
