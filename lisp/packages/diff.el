;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         diff.el
;; Version:      $Revision: 1.4 $
;; Author:       This file is based on diff.el by
;;               sunpitt!wpmstr!fbresz@Sun.COM 1/27/89.
;;               It has been completely rewritten in July 1994 by
;;               Sandy Rutherford <sandy@ibm550.sissa.it>
;; RCS:          
;; Description:  diff-mode for handling output from unix diff utility.
;; Modified:     Wed Jul 17 10:26:57 1996 (Andy Norman) ange@hplb.hpl.hp.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1994 Sandy Rutherford

;;; This file is based on diff.el by sunpitt!wpmstr!fbresz@Sun.COM 1/27/89.
;;; It has been completely rewritten in July 1994 by
;;; Sandy Rutherford <sandy@ibm550.sissa.it>

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

(provide 'diff)

;;; User Variables

;; should be in to loaddefs.el now.
;;;###autoload
(defvar diff-switches "-c"
  "*A list of switches (strings) to pass to the diff program.")

(defvar diff-do-narrow nil
  "*If non-nil diff buffers are initialized narrowed to each difference.")

(defvar diff-load-hooks nil
  "Hooks to run after loading diff.el")

;;; Internal variables

(defconst diff-emacs-19-p
  (let ((ver (string-to-int (substring emacs-version 0 2))))
    (>= ver 19)))

(or diff-emacs-19-p (require 'emacs-19))

(defvar diff-old-file nil)
;; A list whose car is the name of the old file, and whose cdr indicates
;; whether we should delete the buffer on quit.
(defvar diff-new-file nil)
;; Same as diff-old-file, except for the new file.
(defvar diff-total-differences "0")
;; Total number of difference hunks as a string.
(defvar diff-current-difference "0")
;; Current difference hunk as a string.
(defvar diff-current-hunk 0)
;; Current difference hunk as an integer.
(defvar diff-total-hunks 0)
;; Total number of difference hunks as an integer.
(defvar diff-hunk-vector (vector 0))
;; Vector storing the starting positions of the difference hunks.
(defvar diff-old-file-pattern nil)
(defvar diff-new-file-pattern nil)
(defvar diff-hunk-pattern nil)
;; Regexps to use when parsing file lines in difference hunks.


(defvar diff-search-pattern-alist
  (list
    (list ?e "^[0-9]\\(,[0-9]+\\)?[acd]$" "^\\([0-9]+\\)" nil)
    (list ?c "^\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\n"
	  "^\\*+ \\([0-9]+\\)" "^-+ \\([0-9]+\\)")
    (list ?u "^@@ " "^@@ -\\([0-9]+\\)" "^@@ -[0-9,]+ \\+\\([0-9]+\\)")
    (list nil "^[0-9]+" "^\\([0-9]+\\)" "^[0-9,]+[acd]\\([0-9]+\\)")))

(defvar diff-keymap-grokked nil)

(defvar diff-temp-template "/tmp/diff")

;; Initialize the keymap if it isn't already

(defvar diff-mode-map nil
  "Keymap used in diff mode.")

(if diff-mode-map
    nil
  (setq diff-mode-map (make-keymap))
  (suppress-keymap diff-mode-map)
  (define-key diff-mode-map "?" 'describe-mode)
  (define-key diff-mode-map "." 'diff-display-file)
  (define-key diff-mode-map "N" 'diff-narrow)
  (define-key diff-mode-map "W" 'widen)
  (define-key diff-mode-map "f" 'diff-find-file)
  (define-key diff-mode-map "h" 'diff-show-header)
  (define-key diff-mode-map "j" 'diff-show-difference)
  (define-key diff-mode-map "n" 'diff-next-difference)
  (define-key diff-mode-map "o" 'diff-find-file-other-window)
  (define-key diff-mode-map "p" 'diff-previous-difference)
  (define-key diff-mode-map "q" 'diff-quit)
  (define-key diff-mode-map "w" 'diff-find-file-other-frame)
  (define-key diff-mode-map "\C-c\C-c" 'diff-find-file-other-window)
  (define-key diff-mode-map " " 'diff-advertised-scroll-up)
  (define-key diff-mode-map "\177" 'diff-advertised-scroll-down)
  (define-key diff-mode-map "\C-n" 'diff-next-line)
  (define-key diff-mode-map "\C-p" 'diff-previous-line)
  (define-key diff-mode-map "\M->" 'diff-end-of-buffer)
  (define-key diff-mode-map "\M-<" 'diff-beginning-of-buffer))

;;; Internal functions

(defun diff-grok-keys (to-command from-command)
  ;; Assigns to TO-COMMAND the keys for the global binding of FROM-COMMAND.
  ;; Does not clobber anything in the local keymap.
  (let ((keys (where-is-internal from-command)))
    (while keys
      (condition-case nil
	  (if (eq (global-key-binding (car keys)) (key-binding (car keys)))
	      (local-set-key (car keys) to-command))
	(error nil))
      (setq keys (cdr keys)))))

(defun diff-grok-keymap ()
  (if diff-keymap-grokked
      ()
    (if (and term-setup-hook (boundp 'command-line-args-left))
	(progn
	  (if diff-emacs-19-p
	      (run-hooks term-setup-hook)
	    (funcall term-setup-hook))
	  (setq term-setup-hook nil)))
    (setq diff-keymap-grokked t)
    (diff-grok-keys 'diff-next-line 'next-line)
    (diff-grok-keys 'diff-previous-line 'previous-line)
    (diff-grok-keys 'diff-forward-char 'forward-char)
    (diff-grok-keys 'diff-backward-char 'backward-char)
    (diff-grok-keys 'diff-scroll-up 'scroll-up)
    (diff-grok-keys 'diff-scroll-down 'scroll-down)
    (diff-grok-keys 'diff-beginning-of-buffer 'beginning-of-buffer)
    (diff-grok-keys 'diff-end-of-buffer 'end-of-buffer)))

(defun diff-buffer-narrowed-p ()
  ;; Returns t if the current buffer is narrowed
  (save-restriction
    (let ((min (point-min))
	  (max (point-max)))
      (widen)
      (not (and (= min (point-min)) (= max (point-max)))))))

(defun diff-current-hunk ()
  ;; Returns the current diff hunk.
  (let ((pt (point))
	(start 0)
	(end (1+ diff-total-hunks))
	m)
    (while (> (- end start) 1)
      (setq m (/ (+ start end) 2))
      (if (>= pt (aref diff-hunk-vector m))
	  (setq start m)
	(setq end m)))
    (if (>= pt (aref diff-hunk-vector end))
	(setq m end)
      (setq m start))
    ;; Don't treat end of buffer as start of next hunk
    (if (eobp) (1- m) m)))

(defun diff-hunk-min (n)
  ;; Returns the start of the current diff hunk.
  (aref diff-hunk-vector n))

(defun diff-hunk-max (n)
  ;; Returns the end of the current diff hunk.
  (aref diff-hunk-vector (1+ n)))

(defun diff-parse-hunks ()
  ;; Parses a buffer of diff output.
  (save-excursion
    (save-restriction
      (message "Parsing differences...")
      (widen)
      (goto-char (point-min))
      (let ((hunks (list 1)))
	(while (re-search-forward diff-hunk-pattern nil t)
	  (setq hunks (cons (match-beginning 0) hunks)))
	(setq diff-total-hunks (1- (length hunks))
	      diff-hunk-vector (apply 'vector
				      (nreverse (cons (point-max) hunks)))))))
  (message "Parsing differences...done"))

(defun diff-update-modeline ()
  ;; Updates the mode line to show current diff hunk.
  (if (or (< (point) (diff-hunk-min diff-current-hunk))
	  (>= (point) (diff-hunk-max diff-current-hunk)))
      (progn
	(setq diff-current-hunk (diff-current-hunk)
	      diff-current-difference (int-to-string diff-current-hunk))
	(set-buffer-modified-p (buffer-modified-p)))))

(defun diff-read-args (oldprompt newprompt switchprompt
				 &optional file-for-backup)
  ;; Grab the args for diff.  OLDPROMPT and NEWPROMPT are the prompts
  ;; for the old & new filenames, SWITCHPROMPT for the list of
  ;; switches.  If FILE_FOR_BACKUP is provided (it must be a string if
  ;; so), then it will be used to try & work out a file & backup to
  ;; diff, & in this case the prompting order is backwards.  %s in a
  ;; prompt has a guess substituted into it.  This is nasty.
  (let (oldf newf)
    (if file-for-backup
	(setq newf file-for-backup
	      newf (if (and newf (file-exists-p newf))
		       (read-file-name
			(format newprompt (file-name-nondirectory newf))
			nil newf t)
		     (read-file-name (format newprompt "") nil nil t))
	      oldf (file-newest-backup newf)
	      oldf (if (and oldf (file-exists-p oldf))
		       (read-file-name
			(format oldprompt (file-name-nondirectory oldf))
			nil oldf t)
		     (read-file-name (format oldprompt "")
				     (file-name-directory newf) nil t)))
      ;; Else we aren't trying to be bright...
      (setq oldf (read-file-name (format oldprompt "") nil nil t)
	    newf (read-file-name
		  (format newprompt (file-name-nondirectory oldf))
		  nil (file-name-directory oldf) t)))
	(list oldf newf (diff-read-switches switchprompt))))

(defun diff-read-switches (switchprompt)
  ;; Read and return a list of switches
  (if current-prefix-arg
      (let ((default (if (listp diff-switches)
			 (mapconcat 'identity diff-switches " ")
		       diff-switches)))
	(diff-fix-switches
	 (read-string (format switchprompt default) default)))))

(defun diff-fix-switches (switch-spec)
  ;; Parse a string into a list of switches or leave it be if it's
  ;; not a string
  (if (stringp switch-spec)
      (let (result (start 0))
	(while (string-match "\\(\\S-+\\)" switch-spec start)
	  (setq result (cons (substring switch-spec (match-beginning 1)
					(match-end 1))
			     result)
		start (match-end 0)))
	(nreverse result))
    switch-spec))

(defun diff-get-file-buffer (file)
  ;; Returns \(BUFFER . DEL-P\), where DEL-P is t if diff is expected
  ;; to delete the buffer, and nil otherwise.
  (let* ((buff (get-file-buffer file))
	 (del-p (null buff)))
    (if (and buff (buffer-modified-p buff))
	(progn
	  (message
	   "Buffer %s is modified.  Diffing against buffer contents."
	   (buffer-name buff))
	  (sit-for 1)))
    ;; Call find-file-noselect even if we already have the buffer,
    ;; as it will run verify-buffer-file-modtime.
    (cons (find-file-noselect file) del-p)))

(defun diff-cleanup-buffers ()
  ;; Cleans up diff buffers by deleting buffers that we don't expect
  ;; the user to care about.
  (let ((files (list diff-old-file diff-new-file)))
    (while files
      (let ((ent (car files))
	    buff)
	(and (cdr ent)
	     (setq buff (get-file-buffer (car ent)))
	     (not (buffer-modified-p buff))
	     (kill-buffer buff)))
      (setq files (cdr files)))
    (if (get-buffer "*Diff Header*")
	(kill-buffer "*Diff Header*"))))

(defun diff-latest-backup-file (file)
  "Return the latest existing backup of FILE, or nil."
  ;; First try simple backup, then the highest numbered of the
  ;; numbered backups.
  ;; Ignore the value of version-control because we look for existing
  ;; backups, which maybe were made earlier or by another user with
  ;; a different value of version-control.
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'diff-latest-backup-file)))
    (if handler
	(funcall handler 'diff-latest-backup-file file)
      (or
       (let ((bak (make-backup-file-name file)))
	 (if (file-exists-p bak) bak))
       (let* ((dir (file-name-directory file))
	      (base-versions (concat (file-name-nondirectory file) ".~"))
	      (bv-length (length base-versions)))
	 (concat dir
		 (car (sort
		       (file-name-all-completions base-versions dir)
		       ;; bv-length is a fluid var for backup-extract-version:
		       (function
			(lambda (fn1 fn2)
			  (> (backup-extract-version fn1)
			     (backup-extract-version fn2))))))))))))

(defun diff-file-line (&optional old-file-p)
  "Return line number of current hunk in `diff-new-file'.
With optional argument OLD-FILE-P, use `diff-old-file' instead."
  (save-excursion
    (let ((min (diff-hunk-min diff-current-hunk))
	  (max (diff-hunk-max diff-current-hunk))
	  (regexp (if old-file-p diff-old-file-pattern diff-new-file-pattern)))
      (goto-char min)
      (or (and regexp (re-search-forward regexp max t))
	  (error "Unable to locate a file line for %s file."
		 (if old-file-p "old" "new")))
      (string-to-int (buffer-substring (match-beginning 1) (match-end 1))))))

(defun diff-run-diff (switches old old-temp new new-temp)
  ;; Actually run the diff process with SWITCHES on OLD and NEW.
  ;; OLD-TEMP and NEW-TEMP are names of temp files that can be used
  ;; to dump the data out to.
  (insert "diff " (mapconcat 'identity switches " ") " " old
	  " " new "\n")
  (apply 'call-process "diff" nil t nil
	 (append switches (list old-temp new-temp))))


(defun diff-fix-file-names (old old-temp new new-temp pattern)
  ;; Replaces any temp file names with the real names of files.
  (save-excursion
    (save-restriction
      (let ((files (list old new))
	    (temps (list old-temp new-temp))
	    buffer-read-only case-fold-search)
	(goto-char (point-min))
	(if (re-search-forward pattern nil t)
	    (narrow-to-region (point-min) (match-beginning 0)))
	(while files
	  (let ((regexp (concat "[ \t\n]\\("
				(regexp-quote (car temps))
				"\\)[ \t\n]")))
	    (goto-char (point-min))
	    (forward-line 1)
	    (while (re-search-forward regexp nil t)
	      (goto-char (match-beginning 1))
	      (delete-region (point) (match-end 1))
	      (insert (car files))))
	  (setq files (cdr files)
		temps (cdr temps)))))))

;;;; User commands	 

(defun diff-mode ()
  "Diff Mode is used by \\[diff] for perusing the output from the diff program.
All normal editing commands are turned off.  Instead, these are available:
\\<diff-mode-map>
\\[diff-advertised-scroll-up]	Scroll to next screen of this difference.
\\[diff-advertised-scroll-down]	Scroll to previous screen of this difference.
\\[diff-next-difference]	Move to Next Difference.
\\[diff-previous-difference]	Move to Previous Difference.
\\[diff-show-difference]	Jump to difference specified by numeric position.
\\[diff-find-file]	Find current diff in file
\\[diff-find-file-other-window]	Find current diff in file in other window
\\[diff-display-file]	Display file in other window
\\[diff-narrow]	Narrow diff buffer to current difference
\\[widen]	Widen diff buffer
\\[diff-show-header]	Show diff header describing file name etc.
\\[diff-quit]	Quit diff
"
  (interactive)
  (use-local-map diff-mode-map)
  (diff-grok-keymap)
  (setq buffer-read-only t
	major-mode 'diff-mode
	mode-name "Diff"
	mode-line-modified "--- "
	mode-line-process
	'(" " diff-current-difference "/" diff-total-differences))
  (diff-parse-hunks)
  (setq diff-total-differences (int-to-string diff-total-hunks)))

;;; Motion commands

(defun diff-next-difference (n)
  "In diff-mode go the the beginning of the next difference hunk."
  (interactive "p")
  (if (zerop n)
      (goto-char (diff-hunk-min diff-current-hunk))
    (let ((narrow (diff-buffer-narrowed-p))
	  (max (point-max))
	  (min (point-min)))
      (unwind-protect
	  (progn
	    (widen)
	    (setq diff-current-hunk (+ n diff-current-hunk))
	    (cond ((> diff-current-hunk diff-total-hunks)
		   (setq diff-current-hunk diff-total-hunks)
		   (message "No following difference hunks."))
		  ((< diff-current-hunk 0)
		   (setq diff-current-hunk 0)
		   (message "No preceding difference hunks.")))
	    (setq diff-current-difference (int-to-string diff-current-hunk)
		  min (goto-char (diff-hunk-min diff-current-hunk))
		  max (diff-hunk-max diff-current-hunk)))
	(if narrow (narrow-to-region min max))))
    (set-buffer-modified-p (buffer-modified-p))))

(defun diff-previous-difference (n)
  "In diff-mode go the the beginning of the previous difference hunk."
  (interactive "p")
  (diff-next-difference (- n)))

(defun diff-next-line (n)
  "In diff-mode go to the next line."
  (interactive "p")
  (condition-case nil
      (next-line n)
    (error (if (> n 0) (message "End of difference hunk"))))
  (diff-update-modeline))

(defun diff-previous-line (n)
  "In diff-mode go to the previous line."
  (interactive "p")
  (diff-next-line (- n)))

(defun diff-forward-char (n)
  "In diff-mode move the point forward."
  (interactive "p")
  (forward-char n)
  (diff-update-modeline))

(defun diff-backward-char (n)
  "In diff-mode move the point backward."
  (interactive "p")
  (backward-char n)
  (diff-update-modeline))

(defun diff-scroll-up (n)
  "In diff-mode scroll the buffer up."
  (interactive "P")
  (scroll-up n)
  (diff-update-modeline))

(fset 'diff-advertised-scroll-up 'diff-scroll-up)

(defun diff-scroll-down (n)
  "In diff-mode scroll the buffer down."
  (interactive "P")
  (scroll-down n)
  (diff-update-modeline))

(fset 'diff-advertised-scroll-down 'diff-scroll-down)

(defun diff-beginning-of-buffer (n)
  "In diff-mode go to the beginning of the buffer."
  (interactive "P")
  (beginning-of-buffer n)
  (diff-update-modeline))

(defun diff-end-of-buffer (n)
  "In diff-mode go to the end of the buffer."
  (interactive "P")
  (end-of-buffer n)
  (diff-update-modeline))

;;; The main command.

;;;###autoload
(defun diff (old new &optional switches)
  "Find and display the differences between OLD and NEW files.
Interactively you are prompted with the current buffer's file name for NEW
and what appears to be its backup for OLD."
  ;; Support for diffing directories is rather limited.  It needs work.
  (interactive (diff-read-args "Diff original file (%s) "
			       "Diff new file (%s) "
			       "Switches for diff (%s) "
			       (buffer-file-name)))
  (setq switches (diff-fix-switches (or switches diff-switches))
	old (expand-file-name old)
	new (expand-file-name new))
  (let ((curr-buff (current-buffer))
	doing-dirs old-temp new-temp old-buffer new-buffer flag)
    (let ((fdp-old (file-directory-p old))
	  (fdp-new (file-directory-p new)))
      (cond
       ((null (or fdp-new fdp-old)))
       ((null fdp-new)
	(setq old (expand-file-name (file-name-nondirectory new) old)))
       ((null fdp-old)
	(setq new (expand-file-name (file-name-nondirectory old) new)))
       (t (setq doing-dirs t))))
;;    (message "diff %s %s %s..."
;;	     (mapconcat (function identity) switches " ") new old)
    (message "diff %s %s %s..."
	     (mapconcat (function identity) switches " ") old new)
    (if doing-dirs
	(setq diff-old-file nil
	      diff-new-file nil)
      (setq old-temp (make-temp-name (concat diff-temp-template "1"))
	    new-temp (make-temp-name (concat diff-temp-template "2"))
	    old-buffer (diff-get-file-buffer old)
	    new-buffer (diff-get-file-buffer new)
	    diff-old-file (cons old (cdr old-buffer))
	    diff-new-file (cons new (cdr new-buffer))))
    (let (case-fold-search)
      (mapcar (function
	       (lambda (x)
		 (if (string-match "[ecu]" x)
		     (setq flag (aref x (match-beginning 0))))))
	      switches))
    (unwind-protect
	(let ((patterns (assq flag diff-search-pattern-alist)))
	  (set-buffer (get-buffer-create "*Diff Output*"))
	  (setq default-directory (file-name-directory new)
		diff-old-file-pattern (nth 2 patterns)
		diff-new-file-pattern (nth 3 patterns)
		diff-hunk-pattern (nth 1 patterns))
	  (let (buffer-read-only)
	    (if (fboundp 'buffer-disable-undo)
		(buffer-disable-undo (current-buffer))
	      ;; old style (Emacs 18.55 and earlier)
	      (buffer-disable-undo (current-buffer)))
	    (widen)
	    (erase-buffer)
	    (if doing-dirs
		(progn
		  (diff-run-diff switches old old new new)
		  (setq diff-hunk-pattern (concat diff-hunk-pattern
						  "\\|^Only in ")))
	      (save-excursion
		(set-buffer (car old-buffer))
		(write-region (point-min) (point-max) old-temp nil 'quiet)
		(set-buffer (car new-buffer))
		(write-region (point-min) (point-max) new-temp nil 'quiet))
	      (diff-run-diff switches old old-temp new new-temp))
	    ;; Need to replace file names
	    (if (and (not doing-dirs) (memq flag '(?c ?u)))
		(diff-fix-file-names old old-temp new new-temp
				     diff-hunk-pattern))
	    (diff-mode)
	    (goto-char (point-min))
	    (setq diff-current-difference "0"
		  diff-current-hunk 0)
	    (if (zerop diff-total-hunks)
		(progn
		  (diff-cleanup-buffers)
		  (message "No differences"))
	      (if diff-do-narrow (narrow-to-region (point) (diff-hunk-max 0)))
	      (display-buffer (current-buffer))
	      (message "%s difference hunk%s" diff-total-differences
		       (if (= diff-total-hunks 1) "" "s")))))
      (condition-case nil
	  (delete-file old-temp)
	(error nil))
      (condition-case nil
	  (delete-file new-temp)
	(error nil))
      (set-buffer curr-buff))))

;;;###autoload
(defun diff-backup (file &optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'."
  (interactive (list (read-file-name "Diff (file with backup): ")
		     (and current-prefix-arg
			  (diff-read-switches "Diff switches: "))))
  (let (bak ori)
    (if (backup-file-name-p file)
	(setq bak file
	      ori (file-name-sans-versions file))
      (setq bak (or (diff-latest-backup-file file)
		    (error "No backup found for %s" file))
	    ori file))
    (diff bak ori switches)))

(defun diff-show-difference (n)
  "Show difference number N (prefix argument)."
  (interactive "p")
  (let ((narrowedp (diff-buffer-narrowed-p))
	(min (diff-hunk-min diff-current-hunk))
	(max (diff-hunk-max diff-current-hunk)))
    (unwind-protect
	(progn
	  (widen)
	  (cond
	   ((< n 0)
	    (message "No negative hunks.")
	    (setq n 0))
	   ((> n diff-total-hunks)
	    (message "No hunk %d." n)
	    (setq n diff-total-hunks)))
	  (setq diff-current-hunk n
		diff-current-difference (int-to-string diff-current-hunk)
		min (diff-hunk-min n)
		max (diff-hunk-max n))
	  (goto-char min))
      (if narrowedp (narrow-to-region min max))
      (set-buffer-modified-p (buffer-modified-p)))))

(defun diff-show-header ()
  "Show `diff-header'."
  (interactive)
  (with-output-to-temp-buffer "*Diff Header*"
    (princ (save-restriction
	     (widen)
	     (buffer-substring (diff-hunk-min 0) (diff-hunk-max 0))))))


(defun diff-find-file (old-file-p)
  "Visit diffed file, at the point corresponding to the current hunk.
Default is to visit the new file; prefix means visit old file instead."
  (interactive "P")
  (let ((line (diff-file-line old-file-p)))
    (find-file 
     (if old-file-p
	 (car diff-old-file)
       (car diff-new-file)))
    (goto-line line)
    (recenter 0)))

(defun diff-find-file-other-window (old-file-p)
  "Visit the diffed file in other window, with the point at the current hunk.
Default is to visit the new file; prefix means visit the old file instead."
  (interactive "P")
  (let ((line (diff-file-line old-file-p)))
    (find-file-other-window
     (if old-file-p
	 (car diff-old-file)
       (car diff-new-file)))
    (goto-line line)
    (recenter 0)))

(defun diff-find-file-other-frame (old-file-p)
  "Visit the diffed file in other frame, with point at the current hunk.
Default is to visit the new file; prefix means visit the old file instead."
  (interactive "P")
  (let ((line (diff-file-line old-file-p)))
    (find-file-other-frame
     (if old-file-p
	 (car diff-old-file)
       (car diff-new-file)))
    (goto-line line)
    (recenter 0)))

(defun diff-display-file (old-file-p)
  "Display the diffed file in other window, with point at the current hunk.
Default is to visit the new file; prefix means visit the old file instead."
  (interactive "P")
  (let ((line (diff-file-line old-file-p))
	(wind (display-buffer (find-file-noselect (if old-file-p
						      (car diff-old-file)
						    (car diff-new-file)))))
	(curr-wind (selected-window)))
    (unwind-protect
	(progn
	  (select-window wind)
	  (goto-line line)
	  (recenter 0))
      (select-window curr-wind))))

(defun diff-quit ()
  "Quit diff by killing the diff buffer."
  (interactive)
  (kill-buffer "*Diff Output*")
  (diff-cleanup-buffers))

(defun diff-narrow ()
  "Narrow diff buffer to current difference hunk."
  (interactive)
  (narrow-to-region (diff-hunk-min diff-current-hunk)
		    (diff-hunk-max diff-current-hunk)))

;;; Run any load hooks
(run-hooks 'diff-load-hook)

;;; end of diff.el
