;;; reproduce-bugs.el --- reproduce bugs in XEmacs

;; Copyright (C) 1997  Free Software Foundation, Inc.
;; Copyright (C) 1997  Sun Microsystems, Inc.

;; Keywords: bugs, crash, burn, die, croak, munge

;; This file is part of XEmacs.

;; This file is free software; you can redistribute it and/or modify it
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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Reproduce XEmacs crashes, so that they can get fixed.
;; A table of bugs is created.  You can list, describe, or reproduce bugs.

;; Non-crash bugs should not be in this file; they should be placed in
;; an appropriate file in the tests/automated suite.
;; You may need to use a debug version of XEmacs to reproduce some of these.

;; For XEmacs maintainers and other masochists.
;; It's a bad idea to rely on code in this file continuing to work in
;; the same way. :-)

;; #### This file should be cleaned up and renamed reproduce-crashes.el.
;; #### Bugs < 11 need to be tested and versions where they pass recorded.
;; #### Fixed bugs should become regression tests, maybe?
;; #### Non-crashes should be copied (not moved) to tests/automatic.
;; #### Do the autoloads make any sense?
;; #### `list-bugs' should optionally sort on status.
;; #### Bugs that depend on features (eg, Mule) should check for them and
;;      document them.

;;; Code:

;; UI entry points

(defun reproduce-bug (number)
  "Reproduce XEmacs bugs, so that they can get fixed.
Especially, make XEmacs crash.
See reproduce-bugs.el for bug descriptions and bug numbers.
A debug version of XEmacs may be needed to reproduce some bugs."
  (interactive "nBug Number: ")
  (funcall (nth 0 (gethash number bug-hashtable))))

(defun describe-bug (number &optional show-code)
  "Describe the bug with index NUMBER in a popup window.
If optional argument SHOW-CODE is non-nil, also display the reproduction code."
  (interactive "nBug number: \ncShow code? [y/N] ")
  (setq show-code (cond ((not (interactive-p)) show-code)
			((member show-code '(?y ?Y)) t)
			(t nil)))
  (with-displaying-temp-buffer (format "Bug %d" number)
    (let ((bug (gethash number bug-hashtable)))
      (princ (format "Bug #%d is %s.\n%s\n\n%s"
		     number
		     (nth 1 bug)
		     (nth 2 bug)
		     (if show-code (pp-to-string (nth 0 bug)) ""))))))

(defun list-bugs ()
  "List bugs most recent first, each with brief description in a popup window.
Assumes a maximum of 999 bugs and a minimum of 80 column width window."
  (interactive)
  (with-displaying-temp-buffer "*Bug list*"
    (princ " #    status                      description\n")
    (let (buglist)
      (maphash (lambda (number bug)
		 (push (format "%3d %-9s %s"
			       number
			       (nth 1 bug)
			       (let ((description (nth 2 bug)))
				 (save-match-data
				   (string-match "\\(.*\\)\\(\n\\|$\\)"
						 description)
				   (match-string 1 description))))
		       buglist))
	       bug-hashtable)
      (setq buglist (sort buglist (lambda (b1 b2) (string< b2 b1))))
      (while buglist
	(let ((bug (pop buglist)))
	  (princ (if (< (length bug) 79) bug (substring bug 0 78)))
	  (terpri))))))

;; Database and utilities (internal)

(defvar bug-hashtable (make-hashtable 10)
  "Table of bugs, keyed by bug index number.
The value is a list (LAMBDA STATUS DOCSTRING), where LAMBDA is a lambda
expression reproducing the bug, and STATUS and DOCSTRING describe the bug.
For details, see `defbug'.")

(put 'defbug 'lisp-indent-function 'defun)
(defmacro defbug (bug-number status docstring &rest body)
  "Record a bug with key BUG-NUMBER and value (LAMBDA STATUS DOCSTRING).
LAMBDA is a lambda expression which when called executes BODY.
BUG-NUMBER is the bug's index number, a positive integer.
STATUS is the current status of the bug, one of
  fixed         The bug has been diagnosed and fixed.
  diagnosed     The bug has been localized but not fixed.
  current       The bug has been reported and reproduced but cause is unknown.
  legacy        The bug is undocumented but presumed fixed.
DOCSTRING should be a string describing the bug, including any relevant
descriptive information and references to archived mailing list traffic or
a BTS issue.
BODY is a sequence of expressions to execute to reproduce the bug."
  (let ((body (if (stringp docstring) body (cons docstring body)))
	(docstring (if (stringp docstring) docstring "[docstring omitted]")))
    `(puthash ,bug-number
              '((lambda () ,@body) ,status ,docstring)
              bug-hashtable)))

(defconst bug-buffer
  (save-excursion
    (set-buffer (get-buffer-create "*Bug*"))
    (erase-buffer)
    (current-buffer)))


;;; ------------------------------------------------------------------
;;;; Bugs follow:

(defbug 11 fixed
  "Crash in search due to backward movement.
Need Mule build with error checking in 21.5.28.
Fatal error: assertion failed,
file /Users/steve/Software/XEmacs/alioth/xemacs/src/search.c, line 1487,
(this_pos) > ((Bytebpos) 1) && this_pos <= ((buf)->text->z + 0)
Reported: <475B104F.2070807@barco.com>
          <87hcixwkh4.fsf@uwakimon.sk.tsukuba.ac.jp>
Fixed:    <87hcixwkh4.fsf@uwakimon.sk.tsukuba.ac.jp>"
  (switch-to-buffer (get-buffer-create "*crash me*"))
  ;; doozy is the keystroke equivalent of the keyboard macro
  ;; "IAI" C-b C-b C-s C-x
  (let ((doozy [;;(control ?x) ?b ?j ?u ?n ?k return
		?I ?A ?I
		   (control ?b) (control ?b)
		   (control ?s) (control ?w)]))
    (execute-kbd-macro doozy)))


(defbug 10 current
  "Crash on trace-function
Fatal error: assertion failed, file src/eval.c, line 1405, abort()"
  (trace-function 'record-buffer bug-buffer)
  (pop-to-buffer bug-buffer))


(defbug 9 current
  "Crashes with stack overflow
Should give error via barf-if-buffer-read-only
Fatal error: assertion failed, file src/eval.c, line 1874, abort()
This bug has been fixed. -sb"
  (switch-to-buffer bug-buffer)
  ;; The following line should contain a number of eight-bit characters
  (insert "²èÌÌËè¤Î°ÜÆ°¤Ï¤Ç¤­¤ë¤è¤¦¤Ë¤Ê¤ê¤Þ¤·¤¿¡£º£ÅÙ¤Ï¡¢²èÌÌ¤ÎÃæ¤Ç¡¢ÆÃÄê¤Î¾ì")
  (setq buffer-read-only t)
  (ignore-errors
    (encode-coding-region (point-min) (point-max) 'euc-japan))
  (garbage-collect))


(defbug 8 current
  "Crashes in debug version only
Fatal error: assertion failed, file src/objects.h, line 149,
RECORD_TYPEP (_obj, lrecord_font_instance) || MARKED_RECORD_P (_obj)"
  (let (glyph ext)
    (make-face 'adobe-symbol-face)
    (set-face-font
     'adobe-symbol-face
     "-adobe-symbol-medium-r-normal--*-140-*-*-p-*-adobe-fontspecific")
    (setq glyph (make-glyph (list (vector 'string
					  :data (char-to-string ?\xD3)))))
    (set-glyph-face glyph 'adobe-symbol-face)
    (setq ext (make-extent 14 18))
    (set-extent-property ext 'begin-glyph glyph)))


(defbug 7 current
  "(maybe?) crash koi8
ACCL: Invalid command (c)
With debugging on, crashes as follows:
Fatal error: assertion failed, file src/lisp.h, line 1227, INTP (obj)"
  ;;(load "cyrillic")
  ;;(load "cyrillic-hooks")
  (princ (decode-coding-string "\xe1" 'koi8)))


(defbug 6 current
  "regexp crash
This doesn't crash for me. -sb"
  (string-match "\\(\\s-\\|$\\)" "å"))


(defbug 5 legacy
  "`subst-char-in-region' moves point."
  (interactive)
  (with-temp-buffer
    (insert "abc")
    (forward-char -1)
    (subst-char-in-region 1 4 ?b ?\344)
    (if (not (= (point) 3))
	(message "Bug!  point should equal 3 but is %d" (point)))))


(defbug 4 legacy
  "Infinite recursion crash - Segmentation Fault"
  (switch-to-buffer bug-buffer)
  (insert "abcdefg")
  (setq e (make-extent 1 4))
  (set-extent-property e 'face 'bold)
  (set-extent-property e 'duplicable t)
  (set-extent-property e 'replicating t)
  (insert (buffer-string))
  (delete-region 8 9))


(defbug 3 current
  "Completely Uninterruptible hang in re-search-backward (Was: java-mode)"
  (switch-to-buffer bug-buffer)
  (insert "{
public static void main(String[] args) throws java.io.IOException
    {
    }
}
")
  (goto-char (point-min))
  (search-forward "{" nil nil 2)
  (backward-char)
  (re-search-backward
   "^\\s(\\|\\(^[ \t]*\\(\\(\\(public\\|protected\\|static\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*\\)\\s("))


(defbug 2 legacy
  "crash popup frames
FIXED
#### This bug is not understood, and may be incomplete.  See source."
  (lambda ()
    (let ((f (selected-frame)))
      (make-frame `(popup ,(selected-frame)))
      (make-frame)
      (sit-for 0)
      (delete-frame f)
      ;; #### Check whether this is needed.
      ;; (save-buffers-kill-emacs5)
      )))


(defbug 1 legacy
  "crash on delete-frame-hook
FIXED!
#### This bug is not understood, and seems to be incomplete.  See source."
  (lambda ()
    ;; #### Should this be add-hook instead of setq?
    (setq delete-frame-hook
	  (lambda (frame)
	    (select-frame frame)
	    (kill-buffer (window-buffer (frame-selected-window frame)))
	    ;; #### Do we need to delete a frame here or something?
	    ))))

;;; reproduce-bugs.el ends here
