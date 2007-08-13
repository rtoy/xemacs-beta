;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:           dired-grep.el
;; RCS:
;; Dired Version:  $Revision: 7.9 $
;; Description:    Support for running grep on marked files in a dired buffer.
;; Author:         Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:        Tue Jul 13 22:59:37 1993 by sandy on ibm550
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1993 Sandy Rutherford

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to sandy@ibm550.sissa.it) or
;;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;;; MA 02139, USA.

;;; The user-level command in this file is dired-grep-file. The command
;;; grep is defined in compile.el. This file does not change that command.

;;; Requirements and provisions

(provide 'dired-grep)
(or (fboundp 'file-local-copy) (require 'emacs-19))
(or (fboundp 'generate-new-buffer) (require 'emacs-19))
(require 'dired)

;;; Variables

(defvar dired-grep-program "grep"
  "Name of program to use to grep files.
When used with the \"-n\" flag, program must precede each match with \"###:\",
where \"###\" is the line number of the match.
If there are grep programs which don't do this, we'll try to think of
some way to accomodate them.")

(defvar dired-grep-switches nil
  "*Switches to pass to the grep program.
This may be either a string or a list of strings.  It is not necessary to 
include \"-n\" as that switch is always used.")

(defvar dired-grep-zcat-program "zcat"
  "Name of program to cat compressed files.")

(defvar dired-grep-compressed-file ".\\.\\(gz\\|[zZ]\\)$"
  "Regexp to match names of compressed files.")

(defvar dired-grep-pop-up-buffer t
  "*If non-nil, the grep output is displayed in the other window upon
completion of the grep process.")

(defvar dired-grep-results-buffer "*Dired Grep*"
  "Name of buffer where grep results are logged.")

(defvar dired-grep-mode-hook nil
  "Hook run after going into grep-mode")

(defvar grep-history nil
  "History of previous grep patterns used.")

(defvar dired-grep-parse-flags-cache nil)
(defvar dired-grep-parse-flags-cache-result nil)

(defvar dired-grep-mode-map nil
  "Keymap for dired-grep-mode buffers.")

(if dired-grep-mode-map
    ()
  (setq dired-grep-mode-map (make-keymap))
  (suppress-keymap dired-grep-mode-map)
  (define-key dired-grep-mode-map "[" 'backward-page)
  (define-key dired-grep-mode-map "]" 'forward-page)
  (define-key dired-grep-mode-map ">" 'dired-grep-next-hit)
  (define-key dired-grep-mode-map "<" 'dired-grep-previous-hit)
  (define-key dired-grep-mode-map "n" 'dired-grep-advertized-next-hit)
  (define-key dired-grep-mode-map "p" 'dired-grep-advertized-previous-hit)
  (define-key dired-grep-mode-map "k" 'dired-grep-delete-line)
  (define-key dired-grep-mode-map "d" 'dired-grep-delete-page)
  (define-key dired-grep-mode-map "^" 'dired-grep-delete-preceding-pages)
  (define-key dired-grep-mode-map "f" 'dired-grep-find-file)
  (define-key dired-grep-mode-map "e" 'dired-grep-find-file)
  (define-key dired-grep-mode-map "m" 'dired-grep-delete-misses)
  (define-key dired-grep-mode-map "o" 'dired-grep-find-file-other-window)
  (define-key dired-grep-mode-map "v" 'dired-grep-view-file)
  (define-key dired-grep-mode-map "w" 'dired-grep-delete-grep-for)
  (define-key dired-grep-mode-map "\C-_" 'dired-grep-undo)
  (define-key dired-grep-mode-map "\C-xu" 'dired-grep-undo))

;;; Entry functions from dired.el

(defun dired-grep (pattern flags)
  ;; grep the file on the current line for PATTERN, using grep flags FLAGS.
  ;; Return nil on success. Offending filename otherwise.
  (let* ((file (dired-get-filename))
	 (result (dired-grep-file pattern file flags)))
    (and result
	 (progn
	   (dired-log (buffer-name (current-buffer)) (concat result "\n"))
	   file))))

(defun dired-do-grep (pattern &optional flags arg)
  "Grep marked files for a pattern. With a \C-u prefix prompts for grep flags."
  (interactive
   (let* ((switches (if (consp current-prefix-arg)
			(read-string "Switches for grep: ")
		      dired-grep-switches))
	  (prompt (format "grep %sfor pattern"
			  (if (stringp switches)
			      (if (string-equal switches "")
				  switches
				(concat switches " "))
			    (if switches
				(concat (mapconcat 'identity switches " ") " ")
			      ""))))
	  (pattern (dired-read-with-history (concat prompt ": ")
					    nil 'grep-history)))
     (list pattern switches
	   (and (not (consp current-prefix-arg)) current-prefix-arg))))
  (dired-map-over-marks-check
   (function
    (lambda ()
      (dired-grep pattern flags)))
   arg 'grep (concat "grep " flags (if flags " \"" "\"") pattern "\"") t))

;;; Utility functions

(defun dired-grep-get-results-buffer ()
  ;; Return the buffer object of the dired-grep-results-buffer, creating and
  ;; initializing it if necessary.
  (let ((buffer (get-buffer dired-grep-results-buffer)))
    (or buffer
	 (save-excursion
	   (set-buffer (setq buffer (get-buffer-create dired-grep-results-buffer)))
	   (dired-grep-mode)
	   buffer))))

;; Only define if undefined, in case efs has got to it already.
(or (fboundp 'dired-grep-delete-local-temp-file)
    (defun dired-grep-delete-local-temp-file (file)
      (condition-case nil (delete-file file) (error nil))))

;;; Commands in the dired-grep-results-buffer buffer.

(defun dired-grep-mode ()
  "\\<dired-grep-mode-map>Mode for perusing grep output generated from dired.
The output is divided into pages, one page per grepped file.

Summary of commands:

Move to next grep hit                     \\[dired-grep-advertized-next-hit], \\[dired-grep-next-hit]
Move to previous grep hit                 \\[dired-grep-advertized-previous-hit], \\[dired-grep-previous-hit]
Move to output for next file              \\[forward-page]
Move to output for previous file          \\[backward-page]

Delete the current grep line              \\[dired-grep-delete-line]
Delete all output for current file        \\[dired-grep-delete-page]
Delete all preceding pages                \\[dired-grep-delete-preceding-pages]
Delete all pages for files with no hits   \\[dired-grep-delete-misses]
Delete all pages which grep for the 
  same pattern as the current page        \\[dired-grep-delete-grep-for]

Find current grep hit in file             \\[dired-grep-find-file]
Find current grep hit in other window     \\[dired-grep-find-file-other-window]
View current grep hit                     \\[dired-grep-view-file]

Undo changes to the grep buffer           \\[dired-grep-undo]

Keybindings:
\\{dired-grep-mode-map}"
  (kill-all-local-variables)
  (use-local-map dired-grep-mode-map)
  (setq major-mode 'dired-grep-mode
	mode-name "Dired-Grep"
	buffer-read-only t)
  (set (make-local-variable 'page-delimiter) "\n\n")
  (run-hooks 'dired-grep-mode-hook))

(defun dired-grep-current-file-and-line ()
  ;; Returns a list \(FILENAME . LINE\) corresponding to the filename
  ;; and line number associated with the position of the point in a
  ;; grep buffer. Returns nil if there is none.
  (save-excursion
    (let (file line)
      (and
       (progn
	 (beginning-of-line)
	 (looking-at "[0-9]+:"))
       (progn
	 (setq line (string-to-int (buffer-substring (point)
						     (1- (match-end 0)))))
	 (if (search-backward "\n\n" nil 'move) (forward-char 2))
	 (looking-at "Hits for "))
       (progn
	 (forward-line 1)
	 (looking-at "   "))
       (progn
	 (setq file (buffer-substring (match-end 0)
				      (progn (end-of-line) (1- (point)))))
	 (cons file line))))))

(defun dired-grep-find-file ()
  (interactive)
  (let ((file (dired-grep-current-file-and-line)))
    (if file
	(progn
	  (find-file (car file))
	  (goto-line (cdr file))
	  (recenter '(4)))
      (error "No file specified by this line."))))

(defun dired-grep-find-file-other-window ()
  (interactive)
  (let ((file (dired-grep-current-file-and-line)))
    (if file
	(progn
	  (find-file-other-window (car file))
	  (goto-line (cdr  file))
	  (recenter '(4)))
      (error "No file specified by this line."))))

(defun dired-grep-view-file ()
  (interactive)
  (let ((file (dired-grep-current-file-and-line)))
    (if file
	(let* ((fun (function
		     (lambda () (goto-line (cdr file)) (recenter '(4)))))
	       (view-hook
		(if (boundp 'view-hook)
		    (if (and (listp view-hook)
			     (not (eq (car view-hook) 'lambda)))
			(cons fun view-hook)
		      (list fun view-hook))
		  fun)))
	  (view-file (car file)))
      (error "No file specified by this line."))))

(defun dired-grep-next-hit (arg)
  "Moves to the next, or next ARGth, grep hit."
  (interactive "p")
  (forward-line 1)
  (if (re-search-forward "^[0-9]" nil 'move arg)
      (goto-char (match-beginning 0))
    (error "No further grep hits")))

(defun dired-grep-previous-hit (arg)
  "Moves to the previous, or previous ARGth, grep hit."
  (interactive "p")
  (beginning-of-line)
  (or (re-search-backward "^[0-9]" nil 'move arg)
      (error "No further grep hits")))

;; These are only so we can get a decent looking help buffer.
(fset 'dired-grep-advertized-next-hit 'dired-grep-next-hit)
(fset 'dired-grep-advertized-previous-hit 'dired-grep-previous-hit)

(defun dired-grep-delete-page (arg)
  "Deletes the current and ARG - 1 following grep output pages.
If ARG is negative, deletes preceding pages."
  (interactive "p")
  (let ((done 0)
	(buffer-read-only nil)
	(backward (< arg 0))
	start)
    (if backward (setq arg (- arg)))
    (while (and (< done arg) (not (if backward (bobp) (eobp))))
      (or (looking-at "^\n")
	  (if (search-backward "\n\n" nil 'move) (forward-char 1)))
      (setq start (point))
      (if (search-forward "\n\n" nil 'move) (forward-char -1))
      (delete-region start (point))
      (and (bobp) (not (eobp)) (delete-char 1))
      (if backward (skip-chars-backward "\n"))
      (setq done (1+ done)))))

(defun dired-grep-delete-preceding-pages ()
  "Deletes the current, and all preceding pages from the grep buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (if (looking-at "^\n")
	(forward-char 1)
      (search-forward "\n\n" nil 'move))
    (delete-region (point-min) (point))))

(defun dired-grep-delete-line (arg)
  "Deletes the current line and ARG following lines from the grep buffer.
Only operates on lines which correspond to file lines for grep hits."
  (interactive "p")
  (let ((opoint (point))
	(buffer-read-only nil)
	(backward (< arg 0))
	(done 0))
    (beginning-of-line)
    (if backward (setq arg (- arg)))
    (if (looking-at "[0-9]+:")
	(while (< done arg)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (if backward (forward-line -1))
	  (if (looking-at "[0-9]+:")
	      (setq done (1+ done))
	    (setq done arg)))
      ;; Do nothing.
      (goto-char opoint))))

(defun dired-grep-delete-grep-for ()
  "Deletes all pages which grep some file for the pattern of the current page."
  (interactive)
  (save-excursion
    ;; In case we happen to be right at the beginning of a page.
    (or (eobp) (eolp) (forward-char 1))
    (forward-page -1) ; gets to the beginning of the page.
    (let* ((eol (save-excursion (end-of-line) (point)))
	   (line (and (search-forward " grep " eol t)
		      (buffer-substring (point) eol))))
      (if line
	  (progn
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let* ((eol (save-excursion (end-of-line) (point)))
		     (this-line (and (search-forward " grep " eol t)
				     (buffer-substring (point) eol))))
		(if (equal line this-line)
		    (progn
		      (dired-grep-delete-page 1)
		      (skip-chars-forward "\n"))
		  (or (eobp) (forward-page 1))))))))))

(defun dired-grep-delete-misses ()
  "Delete all pages for which there were no grep hits.
Deletes pages for which grep failed because of an error too."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "Grep failed \\|No hits ")
	  (progn
	    (dired-grep-delete-page 1)
	    (skip-chars-forward "\n"))
	(forward-page 1)))))

(defun dired-grep-undo ()
  "Undoes deletions in a grep buffer."
  (interactive)
  (let (buffer-read-only)
    (undo)))

;;; Commands for grepping files.

(defun dired-grep-parse-flags (string)
  ;; Breaks a string of switches into a list.
  (if (equal dired-grep-parse-flags-cache string)
      dired-grep-parse-flags-cache-result
    (let ((length (length string))
	  (pointer 0)
	  (start 0)
	  (result nil))
      (while (and (< pointer length) (= (aref string pointer) ?\ ))
	(setq pointer (1+ pointer)))
    (while (< pointer length)
      (setq start pointer)
      (while (and (< pointer length) (/= (aref string pointer) ?\ ))
	(setq pointer (1+ pointer)))
      (setq result (cons (substring string start pointer) result))
      (while (and (< pointer length) (= (aref string pointer) ?\ ))
	(setq pointer (1+ pointer))))
    (setq dired-grep-parse-flags-cache string
	  dired-grep-parse-flags-cache-result (nreverse result)))))
  
(defun dired-grep-file (pattern file &optional flags)
  "Grep for PATTERN in FILE.
Optional FLAGS are flags to pass to the grep program.
When used interactively, will prompt for FLAGS if a prefix argument is used."
  (interactive 
   (let* ((switches (if (consp current-prefix-arg)
			(read-string "Switches for grep: ")
		      dired-grep-switches))
	  (prompt (format "grep %sfor pattern"
			  (if (stringp switches)
			      (if (string-match switches "^ *$")
				  ""
				(concat switches " "))
			    (if switches
				(concat (mapconcat 'identity switches " ") " ")
			      ""))))
	  (pattern (dired-read-with-history (concat prompt ": ")
					    nil 'grep-history))
	  (file (read-file-name (concat prompt " \"" pattern "\" in file :"))))
     (list pattern file switches)))
  (setq file (expand-file-name file))
  (if (listp flags)
      (setq flags (mapconcat 'identity flags " "))
    (if (string-match "^ +$" flags)
	(setq flags "")))
  (let ((file-buff (get-file-buffer file)))
    (if (and file-buff (buffer-modified-p file-buff))
	(if (y-or-n-p (format "Save buffer %s? " (buffer-name file-buff)))
	    (save-excursion
	      (set-buffer file-buff)
	      (save-buffer)))))
  (let ((buffer (dired-grep-get-results-buffer))
	(compressed (string-match dired-grep-compressed-file file))
	failed temp-file jka-compr-compression-info-list)
    (setq temp-file
	  (condition-case err
	      (file-local-copy file)
	    (error (progn (setq failed (format "%s" err)) nil))))
    (or failed
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-max))
	  (let ((buffer-read-only nil)
		pos-1 pos-2)
	    (or (bobp) (insert "\n"))
	    (setq pos-1 (point))
	    (insert "Hits for grep ")
	    (or (string-equal flags "") (insert flags " "))
	    (insert "\"" pattern "\" in\n   " file ":\n")
	    (setq pos-2 (point))
	    (condition-case err
		(apply
		 'call-process
		 (if compressed "sh" dired-grep-program)
		 (or temp-file file)
		 buffer t
		 (if compressed
		     (list "-c" (concat dired-grep-zcat-program
					" |" dired-grep-program
					" " flags " -n '" pattern "'"))
		   (append (dired-grep-parse-flags flags)
			   (list "-n" pattern))))
	      (error (setq failed (format "%s" err))))
	    (if failed
		(progn
		  (if (= pos-2 (point-max))
		      (progn
			(goto-char (1- pos-2))
			(delete-char -1)
			(insert ".")))
		  (goto-char pos-1)
		  (delete-char 4)
		  (insert "Grep failed")
		  failed)
	      (if (= pos-2 (point-max))
		  (progn
		    (goto-char pos-1)
		    (delete-char 1)
		    (insert "No h")
		    (forward-line 1)
		    (end-of-line)
		    (delete-char -1)
		    (insert "."))
		(goto-char pos-2)
		(or (looking-at "[0-9]+:")
		    (setq failed (buffer-substring pos-2
						   (progn (end-of-line)
							  (point))))))))))
    (let ((curr-wind (selected-window)))
      (unwind-protect
	  (progn
	    (pop-to-buffer buffer)
	    (goto-char (point-max)))
	(select-window curr-wind)))
    (if temp-file
	(dired-grep-delete-local-temp-file temp-file))
    failed))

;;; Run the load hook

(run-hooks 'dired-grep-load-hook)

;;; end of dired-grep.el
