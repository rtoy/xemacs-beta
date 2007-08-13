;;; xemacs-build-report.el --- Automatically formatted build reports for XEmacs

;; Copyright (C) 1997 Adrian Aichner

;; Author: Adrian Aichner, Teradyne GmbH Munich <aichner@ecf.teradyne.com>
;; Date: Sun., Apr. 20, 1997.
;; Version: 1.25
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
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

;;; Synched up with: Not synched.

;;; Commentary:

;; The Idea:
;; Let XEmacs report interesting aspects of how it was built.

;; The Concept:
;; User creates an XEmacs Build Report by just calling
;; M-x xemacs-create-build-report
;; which will initialize a mail buffer with relevant information
;; derived from the XEmacs build process. Point is left at the
;; beginning of the report for user to input some personal notes and
;; send the report.

;; The Status:
;; This is the first `Proof of Concept'.

;; The Author:
;; Adrian Aichner, Teradyne GmbH Munich, Sun., Apr. 20, 1997.

;;; Code:

(require 'config)
(provide 'xemacs-build-report)

;; Building the version from the RCS Revision keyword was lifted from
;; lisp/packages/auto-save.el.
;; This was later taken out due to recommandation by developers on
;; xemacs-beta@xemacs.org. Release versions are to be checked out
;; using `co -u -kv ...'.
(defconst xemacs-build-report-version
  "Revision: 1.25"
  "Version number of xemacs-build-report.")

(defgroup xemacs-build-report nil
  "Package automating the process of sending XEmacs Build Reports.")

(defcustom xemacs-build-report-destination
  "xemacs-beta@xemacs.org"
  "The mail address XEmacs Build Reports should go to."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-keep-regexp
  "make\\[\\|warn\\|pure.*\\(space\\|size\\)\\|hides\\|shadowings"
  "Regexp of make process output lines to keep in the report."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-delete-regexp
  "confl.*with.*auto-inlining"
  "Regexp of make process output lines to delete from the report."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-make-output-file
  (concat (gethash 'blddir (config-value-hash-table)) "/mk.err")
  "Filename where stdout and stderr of XEmacs make process have been stored.
mk.err will not be created automatically. You'll have to run make with
output redirection. I use an alias
alias mk 'make \!* >>&\! \!$.err &'
for that, so that I get mk.err went I run mk."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-installation-file
  (concat (gethash 'blddir (config-value-hash-table)) "/Installation")
  "Installation file produced by XEmacs configure process."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-subject
  (concat "Build %s: " emacs-version " on " system-configuration)
  "XEmacs Build Report Subject Line. %s-sequences will be substituted
with user input through `xemacs-create-build-report' according to
`xemacs-build-report-prompts' using `format'."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-prompts
  '(("Status?: "  "Success" "Failure"))
  "XEmacs Build Report Prompt(s). This is a list of prompt-string
lists used by `xemacs-create-build-report' in conjunction with
`xemacs-build-report-subject'. Each list consists of a prompt string
followed by any number of strings which can be chosen via the history
mechanism."
  :group 'xemacs-build-report)

(defcustom xemacs-build-report-file-encoding
  "7bit"
  "XEmacs Build Report File Encoding to be used when MIME support is
available."
:group 'xemacs-build-report)

;; Symbol Name mappings from TM to SEMI serving as
;; Compatibility Bandaid
;; If 'mime-editor/version-name is bound, we must be using TM(-edit).
(if (boundp 'mime-editor/version-name)
    (progn
      ;; No (defvaralias ...) so far. T
      ;; Thanks to "Didier Verna" verna@inf.enst.fr for reporting my
      ;; incorrect defvaraliasing of `mime-editor/insert-tag'.
      (defalias
	'mime-edit-insert-tag
	'mime-editor/insert-tag)
      (defalias
	'mime-edit-insert-binary-file
	'mime-editor/insert-binary-file)))

(defun xemacs-create-build-report (&rest args)
  "Initializes a fresh mail-mode buffer with the contents of XEmacs 
Installation file and excerpts from XEmacs make output and errors and
leaves point at the beginning of the mail text. See also
`xemacs-build-report-destination',
`xemacs-build-report-keep-regexp',
`xemacs-build-report-delete-regexp',
`xemacs-build-report-make-output-file' and
`xemacs-build-report-installation-file'."
  (interactive
   (let (prompt
	 hist
	 arg
	 (prompts xemacs-build-report-prompts))
     (progn
       (while prompts
	 (setq prompt (caar prompts))
	 (setq hist (cdar prompts))
	 (setq prompts (cdr prompts))
	 (setq arg (cons (read-string prompt "" 'hist) arg)))
       arg)))
  (save-excursion
    (mail
     t
     xemacs-build-report-destination
     (apply 'format xemacs-build-report-subject args))
    (let ((report-begin (mail-text)))
      (if (file-exists-p xemacs-build-report-make-output-file)
	  (progn
 	    (if (featurep 'mime-setup)
		(progn
		  (setq xemacs-build-report-keep-regexp
			(concat "^--\\[\\[\\|\\]\\]$\\|"
				xemacs-build-report-keep-regexp))
		  (mime-edit-insert-tag
		   "application"
		   "octet-stream" 
		   (concat
		    "\nContent-Disposition: attachment;"
		    " filename=\""
		    (file-name-nondirectory
		     xemacs-build-report-make-output-file)
		    "\""))
		  (mime-edit-insert-binary-file
		   xemacs-build-report-make-output-file
		   xemacs-build-report-file-encoding))
	      (insert-file-contents xemacs-build-report-make-output-file))
	    (goto-char report-begin)
	    (delete-non-matching-lines
	     xemacs-build-report-keep-regexp)
	    (goto-char report-begin)
	    (delete-matching-lines xemacs-build-report-delete-regexp)
	    (goto-char report-begin)
	    (insert "> Contents of " 
		    xemacs-build-report-make-output-file
		    "\n> keeping lines matching\n> \""
		    xemacs-build-report-keep-regexp
		    "\"\n> and then deleting lines matching\n> \""
		    xemacs-build-report-delete-regexp
		    "\"\n\n"))
	(insert "> " xemacs-build-report-make-output-file
		" does not exist!\n\n"))
      (goto-char report-begin)
      (insert "\n> XEmacs Build Report as generated\n> by "
	      "xemacs-build-report-version "
	      xemacs-build-report-version
	      " follows:\n\n")
      (if (file-exists-p xemacs-build-report-installation-file)
	  (progn
	    (insert "> Contents of "
		    xemacs-build-report-installation-file
		    ":\n\n")
	    (if (featurep 'mime-setup)
		(progn
		  (mime-edit-insert-tag
		   "application"
		   "octet-stream" 
		   (concat
		    "\nContent-Disposition: attachment;"
		    " filename=\""
		    (file-name-nondirectory
		     xemacs-build-report-installation-file)
		    "\""))
		  (mime-edit-insert-binary-file
		   xemacs-build-report-installation-file
		   xemacs-build-report-file-encoding))
	      (insert-file-contents xemacs-build-report-installation-file)))
	(insert "> " xemacs-build-report-installation-file
		" does not exist!\n\n"))
      (goto-char report-begin))))

;;; xemacs-build-report.el ends here
