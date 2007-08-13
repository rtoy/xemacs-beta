;;; savehist.el --- Save minibuffer history

;; Copyright (c) 1997 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@srce.hr>
;; Keywords: minibuffer
;; Version: 0.2

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: not in FSF

;;; Commentary:

;; Many editors (e.g. Vim) have the feature of saving minibuffer
;; history to an external file after exit.  This package provides the
;; same feature in Emacs.  When Emacs is about the exit,
;; `savehist-save' will dump the contents of various minibuffer
;; histories (as determined by `savehist-history-variables') to a save
;; file (`~/.emacs-history' by default).  Although the package was
;; designed for saving the minibuffer histories, any variables can be
;; saved that way.

;; To use savehist, put the following to `~/.emacs':
;;
;; (require 'savehist)
;; (savehist-load)

;; Be sure to have `savehist.el' in a directory that is in your
;; load-path, and byte-compile it.

;; This code should work on XEmacs 19.14 and later, as well as GNU
;; Emacs 19.34 and later.


;;; Code:

(require 'custom)

;; User variables

(defgroup savehist nil
  "Save minibuffer history."
  :group 'minibuffer)


(defcustom savehist-history-variables
  '(
    ;; Catch-all minibuffer history
    minibuffer-history
    ;; File-oriented commands
    file-name-history
    ;; Regexp-related reads
    regexp-history
    ;; Searches in minibuffer (via `M-r' and such)
    minibuffer-history-search-history
    ;; Query replace
    query-replace-history
    ;; eval-expression (`M-:')
    read-expression-history
    ;; shell-command (`M-!')
    shell-command-history
    ;; Viper stuff
    vip-ex-history vip-search-history
    vip-replace1-history vip-replace2-history
    vip-shell-history vip-search-history

    ;; XEmacs-specific:
    ;; Buffer-related commands
    buffer-history
    ;; Reads of variables and functions
    variable-history function-history
    ;; Extended commands
    read-command-history

    ;; GNU Emacs-specific:
    ;; Extended commands
    extended-command-history

    ;; This is not a list, but it's cool to have it anyway, since it's
    ;; minibuffer history too.
    compile-command)
  "*List of symbols to be saved.
Every symbol should refer to a variable.  The variable will be saved only
if it is bound is bound, and has a non-nil value.  Thus it is safe to
specify a superset of the variables a user is expected to want to save.

Default value contains minibuffer history variables used by XEmacs, GNU
Emacs and Viper (uh-oh).  `compile-command' was added for good measure."
  :type '(repeat (symbol :tag "Variable"))
  :group 'minibuffer)

(defcustom savehist-file "~/.emacs-history"
  "*File name to save minibuffer history to.
The minibuffer history is a series of Lisp expressions, which should be
loaded using `savehist-load' from your .emacs.  See `savehist-load' for
more details."
  :type 'file
  :group 'savehist)

(defcustom savehist-length 100
  "*Maximum length of a minibuffer list.
If set to nil, the length is unlimited."
  :type '(choice integer
		 (const :tag "Unlimited" nil))
  :group 'savehist)


;; Functions

;;;###autoload
(defun savehist-load (&optional prefix)
  "Load the histories saved to `savehist-file'.
Unless PREFIX is non-nil, the function will also add the save function to
`kill-emacs-hook'.

This function should be normally used from your Emacs init file.  Since it
removes your current minibuffer histories (if any), it is unwise to call it
at any other time."
  (interactive "P")
  (unless prefix
    (add-hook 'kill-emacs-hook 'savehist-save))
  (when (file-exists-p savehist-file)
    (load savehist-file)))

;;;###autoload
(defun savehist-save ()
  "Save the histories from `savehist-history-variables' to `savehist-file'.
A variable will be saved if it is bound and non-nil."
  (interactive)
  (save-excursion
    ;; Is it wise to junk `find-file-hooks' just like that?  How else
    ;; should I avoid font-lock et al.?
    (let ((find-file-hooks nil)
	  (buffer-exists-p (get-file-buffer savehist-file)))
      (set-buffer (find-file-noselect savehist-file))
      (unwind-protect
	  (progn
	    (erase-buffer)
	    (insert
	     ";; -*- emacs-lisp -*-\n"
	     ";; Minibuffer history file.\n\n"
	     ";; This file is automatically generated by `savehist-save'"
	     " or when\n"
	     ";; exiting Emacs.\n"
	     ";; Do not edit.  Unless you really want to, that is.\n\n")
	    (dolist (sym savehist-history-variables)
	      (when (and (boundp sym)
			 (symbol-value sym))
		(prin1
		 `(setq ,sym (quote ,(savehist-delimit (symbol-value sym)
						       savehist-length)))
		 (current-buffer))
		(insert ?\n)))
	    (save-buffer))
	(or buffer-exists-p
	    (kill-buffer (current-buffer)))))))

;; If ARG is a arg with less than N elements, return it, else return
;; its subsequence of N elements.  If N is nil, always return ARG.  If
;; ARG is not a list, just return it.
(defun savehist-delimit (arg n)
  (if (and n
	   (listp arg)
	   (> (length arg) n))
      (subseq arg 0 n)
    arg))

(provide 'savehist)

;;; savehist.el ends here