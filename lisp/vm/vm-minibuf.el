;;; Minibuffer read functions for VM
;;; Copyright (C) 1993, 1994 Kyle E. Jones
;;;
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-minibuf)

(defun vm-minibuffer-complete-word (&optional exiting)
  (interactive)
  (let ((opoint (point))
	trimmed-c-list c-list beg end diff word word-prefix-regexp completion)
    ;; find the beginning and end of the word we're trying to complete
    (if (or (eobp) (memq (following-char) '(?\t ?\n ?\ )))
	(progn
	  (skip-chars-backward " \t\n")   
	  (and (not (eobp)) (forward-char))
	  (setq end (point)))
      (skip-chars-forward "^ \t\n")
      (setq end (point)))
    (skip-chars-backward "^ \t\n")
    (setq beg (point))
    (goto-char opoint)
    ;; copy the word into a string
    (setq word (buffer-substring beg end))
    ;; trim the completion list down to just likely candidates
    ;; then convert it to an alist.
    (setq word-prefix-regexp (concat "^" (regexp-quote word))
	  trimmed-c-list (vm-delete-non-matching-strings
			  word-prefix-regexp
			  vm-minibuffer-completion-table)
	  trimmed-c-list (mapcar 'list trimmed-c-list)
	  c-list (mapcar 'list vm-minibuffer-completion-table))
    ;; Try the word against the completion list.
    (and trimmed-c-list
	 (setq completion (try-completion word trimmed-c-list)))
    ;; If completion is nil, figure out what prefix of the word would prefix
    ;; something in the completion list... but only if the user is interested.
    (if (and (null completion) vm-completion-auto-correct c-list)
	(let ((i -1))
	  (while (null (setq completion
			     (try-completion (substring word 0 i) c-list)))
	    (vm-decrement i))
	  (setq completion (substring word 0 i))))
    ;; If completion is t, we had a perfect match already.
    (if (eq completion t)
	(cond ((and (cdr trimmed-c-list)
		    (not (eq last-command 'vm-minibuffer-complete-word)))
	       (and (not exiting)
		    (vm-minibuffer-completion-message
		     "[Complete, but not unique]")))
	      (vm-completion-auto-space
	       (goto-char end)
	       (insert " "))
	      (t
	       (and (not exiting)
		    (vm-minibuffer-completion-message "[Sole completion]"))))
      ;; Compute the difference in length between the completion and the
      ;; word.  A negative difference means no match and the magnitude
      ;; indicates the number of chars that need to be shaved off the end
      ;; before a match will occur.  A positive difference means a match
      ;; occurred and the magnitude specifies the number of new chars that
      ;; can be appended to the word as a completion.
      ;;
      ;; `completion' can be nil here, but the code works anyway because
      ;; (length nil) still equals 0!
      (setq diff (- (length completion) (length word)))
      (cond
       ;; We have some completion chars.  Insert them.
       ((> diff 0)
	(goto-char end)
	(insert (substring completion (- diff)))
	(if (and vm-completion-auto-space
		 (null (cdr trimmed-c-list)))
	    (insert " ")))
       ;; The word prefixed more than one string, but we can't complete
       ;; any further.  Either give help or say "Ambiguous".
       ((zerop diff)
	(and (not exiting)
	     (if (null completion-auto-help)
		 (vm-minibuffer-completion-message "[Ambiguous]")
	       (vm-minibuffer-show-completions (sort
						(mapcar 'car trimmed-c-list)
						'string-lessp)))))
       ;; The word didn't prefix anything... if vm-completion-auto-correct is
       ;; non-nil strip the offending characters and try again.
       (vm-completion-auto-correct
	(goto-char end)
	(delete-char diff)
	(vm-minibuffer-complete-word exiting))
       ;; if we're not auto-correcting and we're doing
       ;; multi-word, just let the user insert a space.
       (vm-completion-auto-space
	(insert " "))
       ;; completion utterly failed, tell the user so.
       (t
	(and (not exiting)
	     (vm-minibuffer-completion-message "[No match]")))))))

(defun vm-minibuffer-complete-word-and-exit ()
  (interactive)
  (vm-minibuffer-complete-word t)
  (exit-minibuffer))

(defun vm-minibuffer-completion-message (string &optional seconds)
  "Briefly display STRING to the right of the current minibuffer input.
Optional second arg SECONDS specifies how long to keep the message visible;
the default is 2 seconds.

A keypress causes the immediate erasure of the STRING, and return of control
to the calling program."
  (let (omax (inhibit-quit t))
    (save-excursion
      (goto-char (point-max))
      (setq omax (point))
      (insert " " string))
    (sit-for (or seconds 2))
    (delete-region omax (point-max))))

(defun vm-minibuffer-replace-word (word)
  (goto-char (point-max))
  (skip-chars-backward "^ \t\n")
  (delete-region (point) (point-max))
  (insert word))

(defun vm-minibuffer-show-completions (list)
  "Display LIST in a multi-column listing in the \" *Completions*\" buffer.
LIST should be a list of strings."
  (save-excursion
    (set-buffer (get-buffer-create " *Completions*"))
    (setq buffer-read-only nil)
    (use-local-map (make-sparse-keymap))
    ;; ignore vm-mutable-* here.  the user shouldn't mind
    ;; because when they exit the minibuffer the windows will be
    ;; set right again.
    (display-buffer (current-buffer))
    (erase-buffer)
    (insert "Possible completions are:\n")
    (setq buffer-read-only t)
    (vm-show-list list 'vm-minibuffer-replace-word
		  (list (current-local-map) minibuffer-local-map))
    (goto-char (point-min))))

(defun vm-show-list (list &optional function keymaps)
  "Display LIST in a multi-column listing in the current buffer at point.
The current buffer must be displayed in some window at the time
this function is called.

LIST should be a list of strings.

Optional second argument FUNCTION will be called if the mouse is
clicked on one of the strings in the current buffer.  The string
clicked upon will be passed to FUNCTION as its sole argument.

Optional third argument KEYMAPS specifies a lists of keymaps
where the FUNCTION should be bound to the mouse clicks.  By
default the local keymap of the current buffer is used."
  (or keymaps (setq keymaps (and (current-local-map)
				 (list (current-local-map)))))
  (save-excursion
    (let ((buffer-read-only nil)
	  tab-stops longest rows columns list-length q i w start command
	  keymap)
      (cond ((and function keymaps (vm-mouse-support-possible-p))
	     (setq command
		   (list 'lambda '(e) '(interactive "e")
			 (list 'let
			       '((string (vm-mouse-get-mouse-track-string e)))
			       (list 'and 'string (list function 'string)))))
	     (while keymaps
	       (setq keymap (car keymaps))
	       (cond ((vm-mouse-xemacs-mouse-p)
		      (define-key keymap 'button1 'ignore)
		      (define-key keymap 'button2 'ignore)
		      (define-key keymap 'button1up command)
		      (define-key keymap 'button2up command))
		     ((vm-mouse-fsfemacs-mouse-p)
		      (define-key keymap [down-mouse-1] 'ignore)
		      (define-key keymap [drag-mouse-1] 'ignore)
		      (define-key keymap [mouse-1] command)
		      (define-key keymap [drag-mouse-2] 'ignore)
		      (define-key keymap [down-mouse-2] 'ignore)
		      (define-key keymap [mouse-2] command)))
	       (setq keymaps (cdr keymaps)))))
      (setq w (vm-get-buffer-window (current-buffer)))
      (setq q list
	    list-length 0
	    longest 0)
      (while q
	(setq longest (max longest (length (car q)))
	      list-length (1+ list-length)
	      q (cdr q)))
      ;; provide for separation between columns
      (setq longest (+ 3 longest))
      (setq columns (max 1 (/ (- (window-width w) 2) longest))
	    rows (/ list-length columns)
	    rows
	    (+ (if (zerop (% list-length columns)) 0 1)
	       rows))
      (setq i columns
	    tab-stops nil)
      (while (not (zerop i))
	(setq tab-stops (cons (* longest i) tab-stops)
	      i (1- i)))
      (setq q list
	    i 0)
      (while q
	(setq start (point))
	(insert (car q))
	(and function (vm-mouse-set-mouse-track-highlight start (point)))
	(setq i (1+ i)
	      q (cdr q))
	(if (zerop (% i columns))
	    (insert "\n")
	  (let ((tab-stop-list tab-stops))
	    (tab-to-tab-stop)))))))

(defun vm-minibuffer-completion-help ()
  (interactive)
  (let ((opoint (point))
	c-list beg end word word-prefix-regexp)
    ;; find the beginning and end of the word we're trying to complete
    (if (or (eobp) (memq (following-char) '(?\t ?\n ?\ )))
	(progn
	  (skip-chars-backward " \t\n")   
	  (and (not (eobp)) (forward-char))
	  (setq end (point)))
      (skip-chars-forward "^ \t\n")
      (setq end (point)))
    (skip-chars-backward "^ \t\n")
    (setq beg (point))
    (goto-char opoint)
    ;; copy the word into a string
    (setq word (buffer-substring beg end))
    ;; trim the completion list down to just likely candidates
    ;; then convert it to an alist.
    (setq word-prefix-regexp (concat "^" (regexp-quote word))
	  c-list (vm-delete-non-matching-strings
		  word-prefix-regexp
		  vm-minibuffer-completion-table)
	  c-list (sort c-list (function string-lessp)))
    (if c-list
	(vm-minibuffer-show-completions c-list)
      (vm-minibuffer-completion-message " [No match]"))))

(defun vm-keyboard-read-string (prompt completion-list &optional multi-word)
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map))
	(vm-completion-auto-space multi-word)
	(vm-minibuffer-completion-table completion-list))
    (define-key minibuffer-local-map "\t" 'vm-minibuffer-complete-word)
    (define-key minibuffer-local-map " " 'vm-minibuffer-complete-word)
    (define-key minibuffer-local-map "?" 'vm-minibuffer-completion-help)
    (if (not multi-word)
	(define-key minibuffer-local-map "\r"
	  'vm-minibuffer-complete-word-and-exit))
    ;; evade the XEmacs dialog box, yeccch.
    (let ((use-dialog-box nil))
      (read-string prompt))))

(defvar last-nonmenu-event)

(defun vm-read-string (prompt completion-list &optional multi-word)
  ;; handle alist
  (if (consp (car completion-list))
      (setq completion-list (nreverse (mapcar 'car completion-list))))
  (if (and completion-list (vm-mouse-support-possible-p))
      (cond ((and (vm-mouse-xemacs-mouse-p)
		  (or (button-press-event-p last-command-event)
		      (button-release-event-p last-command-event)
		      (menu-event-p last-command-event)))
	     (vm-mouse-read-string prompt completion-list multi-word))
	    ((and (vm-mouse-fsfemacs-mouse-p)
		  (listp last-nonmenu-event))
	     (vm-mouse-read-string prompt completion-list multi-word))
	    (t
	     (vm-keyboard-read-string prompt completion-list multi-word)))
    (vm-keyboard-read-string prompt completion-list multi-word)))

(defun vm-read-number (prompt)
  (let (result)
    (while
	(null
	 (string-match "^[ \t]*-?[0-9]+" (setq result (read-string prompt)))))
    (string-to-int result)))

(defun vm-read-password (prompt &optional confirm)
  "Read and return a password from the minibuffer, prompting with PROMPT.
Optional second argument CONFIRM non-nil means that the user will be asked
  to type the password a second time for confirmation and if there is a
  mismatch, the process is repeated.

Line editing keys are:
  C-h, DEL	rubout
  C-u, C-x      line kill
  C-q, C-v      literal next"
  (catch 'return-value
    (save-excursion
      (let ((cursor-in-echo-area t)
	    (echo-keystrokes 0)
	    (input-buffer nil)
	    (help-form nil)
	    (xxx "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
	    (string nil)
	    char done form)
	(unwind-protect
	    (save-excursion
	      (setq input-buffer (get-buffer-create " *password*"))
	      (set-buffer input-buffer)
	      (while t
		(erase-buffer)
		(message "%s%s" prompt
				    (vm-truncate-string xxx (buffer-size)))
		(while (not (memq (setq char (read-char)) '(?\C-m ?\C-j)))
		  (if (setq form
			    (cdr
			     (assq char
				   '((?\C-h . (delete-char -1))
				     (?\C-? . (delete-char -1))
				     (?\C-u . (delete-region 1 (point)))
				     (?\C-x . (delete-region 1 (point)))
				     (?\C-q . (quoted-insert 1))
				     (?\C-v . (quoted-insert 1))))))
		      (condition-case error-data
			  (eval form)
			(error t))
		    (insert char))
		  (message "%s%s" prompt
				      (vm-truncate-string xxx (buffer-size))))
		(cond ((and confirm string)
		       (cond ((not (string= string (buffer-string)))
			      (message
			       (concat prompt
				       (vm-truncate-string xxx (buffer-size))
				       " [Mismatch... try again.]"))
			      (ding)
			      (sit-for 2)
			      (setq string nil))
			     (t (throw 'return-value string))))
		      (confirm
		       (setq string (buffer-string))
		       (message
			(concat prompt
				(vm-truncate-string xxx (buffer-size))
				" [Retype to confirm...]"))
		       (sit-for 2))
		      (t
		       (message "")
		       (throw 'return-value (buffer-string))))))
	  (and input-buffer (kill-buffer input-buffer)))))))

(defun vm-keyboard-read-file-name (prompt &optional dir default
					  must-match initial history)
  "Like read-file-name, except HISTORY's value is unaltered."
  (let ((oldvalue (symbol-value history))
	;; evade the XEmacs dialog box, yeccch.
	(use-dialog-box nil))
    (unwind-protect
	(condition-case nil
	    (read-file-name prompt dir default must-match initial history)
	  (wrong-number-of-arguments
	   (if history
	       (let ((file-name-history (symbol-value history))
		     file)
		 (setq file
		       (read-file-name prompt dir default must-match initial))
		 file )
	     (read-file-name prompt dir default must-match initial))))
      (and history (set history oldvalue)))))

(defun vm-read-file-name (prompt &optional dir default
				 must-match initial history)
  "Like read-file-name, except a mouse interface is used if a mouse
click mouse triggered the current command."
  (if (vm-mouse-support-possible-p)
      (cond ((and (vm-mouse-xemacs-mouse-p)
		  (or (button-press-event-p last-command-event)
		      (button-release-event-p last-command-event)
		      (menu-event-p last-command-event)))
	     (vm-mouse-read-file-name prompt dir default
				      must-match initial history))
	    ((and (vm-mouse-fsfemacs-mouse-p)
		  (listp last-nonmenu-event))
	     (vm-mouse-read-file-name prompt dir default
				      must-match initial history))
	    (t
	     (vm-keyboard-read-file-name prompt dir default
					 must-match initial history)))
    (vm-keyboard-read-file-name prompt dir default
				must-match initial history)))


