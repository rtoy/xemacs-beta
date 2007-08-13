;;; bench.el --- a crude benchmark for emacsen
;; Copyright (C) 1987,88,89,90,93,94,95,96 Free Software Foundation, Inc.

;; Author: Shane Holder <holder@rsn.hp.com>
;; Adapted-By: Steve Baur <steve@altair.xemacs.org>

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; To run
;; Extract the shar file in /tmp, or modify bench-large-lisp-file to
;; point to the gnus-bench.el file.
;; At the shell prompt emacs -q --no-site-file <= don't load users .emacs or
;;    site-file
;; M-x byte-compile-file "/tmp/bench.el"
;; M-x load-file "/tmp/bench.elc"
;; In the scratch buffer (bench 1)

;;; Code:

;; Use elp to profile benchmarks
(require 'elp)
(eval-when-compile (require 'cl))	; Emacs doesn't have when and cdar

(defconst bench-version 1.0)

(defconst bench-large-lisp-file "/usr/local/lib/gnus-bench.el"
  "Large lisp file to use in benchmarks.
Grab `ftp://ftp.xemacs.org/pub/beta/contrib/gnus-bench.el.gz' for a good
version.  Don't install this file with Emacs/XEmacs.")

(defconst bench-sort-buffer "*Sort*"
  "File to be used in the sort benchmark")

(defconst bench-sort-number-words 10000
  "Number of words to use in sort benchmark")

(defconst bench-pre-bench-hook nil
  "Hook for individual bench mark initialization.")

(defconst bench-post-bench-hook nil
  "Hook for individual bench mark statistic collection.")

(defconst bench-mark-function-alist 
  '(
    (bench-mark-1 . "Tower of Hanoi")
    (bench-mark-2 . "Font Lock")
    (bench-mark-3 . "Large File scrolling")
    (bench-mark-4 . "Frame Creation")
    (bench-mark-5 . "Generate Words")
    (bench-mark-6 . "Sort Buffer")
    (bench-mark-7 . "Large File bytecompilation")
    (bench-mark-8 . "Loop Computation")
    (bench-mark-9 . "Make a Few Large Size List")
    (bench-mark-10 . "Garbage Collection Large Size List")
    (bench-mark-11 . "Make Several Small Size List")
    (bench-mark-12 . "Garbage Collection Small Size List")
))

(defconst bench-enabled-profiling nil
  "If non-nil and the underlying emacs supports it, do function profiling.")

(defconst bench-mark-profile-buffer "*Profile*"
  "Buffer used for collection of profiling data.")

(setq gc-cons-threshold 40000000)

(defconst bench-number-of-large-lists 10
  "Number of lists to use in large list creation/garbage collections")

(defconst bench-number-of-small-lists 1000000
  "Number of lists to use in small list creation/garbage collections")

(defconst bench-large-list-size 1000000
  "Size of list to use in small list creation/garbage collection")

(defconst bench-small-list-size 10
  "Size of list to use in small list creation/garbage collection")

;-----------------------------------------------------------------------------
(defun bench-mark-1 ()
  "How long to complete the tower of hanoi."
  (hanoi 4))

;-----------------------------------------------------------------------------
(defun bench-mark-2 ()
  "How long to fonitfy a large file."
  (find-file bench-large-lisp-file)
  (font-lock-fontify-buffer))

;-----------------------------------------------------------------------------
(defun bench-mark-3 ()
  "How long does it take to scroll down through a large file."
  (let ((buffer-read-only t))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (next-line 1)
      (sit-for 0))))

;-----------------------------------------------------------------------------
(defun bench-mark-4 ()
  "How quickly can emacs create a new frame."
  (make-frame))


;-----------------------------------------------------------------------------
(defun bench-mark-5 ()
  "How long does it take to generate lots of random words."
  (set-buffer (get-buffer-create bench-sort-buffer))
  (let ((tmp-words bench-sort-number-words))
    (while (not (= tmp-words 0))
      (let ((word-len (random 10)))
	(while (not (= word-len 0))
	  (insert (+ ?a (random 25)))
	  (setq word-len (- word-len 1))))
      (insert "\n")
      (setq tmp-words (- tmp-words 1)))))

;-----------------------------------------------------------------------------

(defun bench-mark-6 ()
  "How long does it take to sort the random words from bench-mark-5."
  (set-buffer (get-buffer-create bench-sort-buffer))
  (sort-lines nil (point-min) (point-max))
)

;-----------------------------------------------------------------------------
(defun bench-mark-7 ()
  "How long does it take to byte-compile a large lisp file"
  (byte-compile-file bench-large-lisp-file)
)

;-----------------------------------------------------------------------------
(defun bench-mark-8 ()
  "How long does it take to run through a loop."
  (let ((count 250000))
    (let ((i 0) (gcount 0))
      (while (< i count)
	(increment)
	(setq i (1+ i)))
      (message "gcount = %d" gcount))))

(defun increment ()
  "Increment a variable for bench-mark-8."
  (setq gcount (1+ gcount)))

;-----------------------------------------------------------------------------
(defun bench-mark-9 ()
  (let ((tmp-foo bench-number-of-large-lists))
    (while (> tmp-foo 0)
      (make-list bench-large-list-size '1)
      (setq tmp-foo (- tmp-foo 1)))
      )
)

;-----------------------------------------------------------------------------
(defun bench-mark-10 ()
  (garbage-collect)
)

;-----------------------------------------------------------------------------
(defun bench-mark-11 ()
  (let ((tmp-foo bench-number-of-small-lists))
    (while (> tmp-foo 0)
      (make-list bench-small-list-size '1)
      (setq tmp-foo (- tmp-foo 1))
      ))
)

;-----------------------------------------------------------------------------
(defun bench-mark-12 ()
  (garbage-collect)
)

;=============================================================================
(defun bench-init ()
  "Initialize profiling for bench marking package."
  (if (fboundp 'start-profiling)
      (let ((buf (get-buffer-create bench-mark-profile-buffer)))
	(erase-buffer buf)
	(when (profiling-active-p)
	  (stop-profiling)
	  (clear-profiling-info)))
    (message "Profiling not available in this Emacs.")
    (sit-for 2)))

(defun bench-profile-start (test-name)
  "Turn on profiling for test `test-name'."
  (when (and bench-enabled-profiling
	     (fboundp 'start-profiling))
    (when (profiling-active-p)
      (stop-profiling))
    (let ((buf (get-buffer-create bench-mark-profile-buffer)))
      (save-excursion
	(set-buffer buf)
	(insert "Test `" test-name "'\n")
	(start-profiling)))))

(defun bench-profile-stop (test-name)
  "Turn off profiling for test `test-name'."
  (when (and bench-enabled-profiling
	     (fboundp 'stop-profiling))
    (stop-profiling)
    (let ((buf (get-buffer-create bench-mark-profile-buffer)))
      (save-excursion
	(set-buffer buf)
	(insert (with-output-to-string
		 (pretty-print-profiling-info)) "\n")))
    (clear-profiling-info)))

(add-hook 'bench-pre-bench-hook 'bench-profile-start)
(add-hook 'bench-post-bench-hook 'bench-profile-stop)

(defun bench (arg)
  "Run a series of benchmarks."
  (interactive "p")
  (elp-instrument-package "bench-mark") ;Only instrument functions
                                        ;beginning with bench-mark
  (bench-init)
  (if (fboundp 'byte-optimize)		;Turn off byte-compile optimization in XEmacs
      (setq byte-optimize nil))
  (let ((benches bench-mark-function-alist))
    (while benches
      (let ((test-name (cdar benches)))
	(run-hook-with-args 'bench-pre-bench-hook test-name)
	(let ((count arg))
	  (while (> count 0)
	    (message "Running %s - %s." (symbol-name (caar benches)) test-name)
	    (funcall (caar benches))
	    (setq count (1- count))))
	(setq benches (cdr benches))
	(run-hook-with-args 'bench-post-bench-hook test-name))
      ))
  (elp-results)
  (goto-char (point-min))
  (next-line 2)
; I can't figure out a good way to sort the lines numerically.
; If someone comes up with a good way, let me know.
  (sort-lines nil (point) (point-max))
  (goto-char (point-min))
  (let ((benches bench-mark-function-alist))
    (while benches
      (goto-char (point-min))
      (let ((test-name (cdar benches))
	    (test-func (caar benches)))
	(search-forward (symbol-name test-func))
	(end-of-line)
	(insert "   <= " test-name))
	(setq benches (cdr benches))
      ))
)

;;; bench.el ends here
