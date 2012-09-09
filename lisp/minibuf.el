;;; minibuf.el --- Minibuffer functions for XEmacs

;; Copyright (C) 1992, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems.
;; Copyright (C) 1995, 1996, 2000, 2002 Ben Wing.

;; Author: Richard Mlynarik
;; Created: 2-Oct-92
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: all the minibuffer history stuff is synched with
;;; 19.30.  Not sure about the rest.

;;; Commentary:

;; This file is dumped with XEmacs.

;; Written by Richard Mlynarik 2-Oct-92

;; 06/11/1997 -  Use char-(after|before) instead of
;;  (following|preceding)-char. -slb

;;; Code:

(require 'cl)

(defgroup minibuffer nil
  "Controlling the behavior of the minibuffer."
  :group 'environment)


(defcustom insert-default-directory t
 "*Non-nil means when reading a filename start with default dir in minibuffer."
 :type 'boolean
 :group 'minibuffer)

(defcustom minibuffer-history-uniquify t
  "*Non-nil means when adding an item to a minibuffer history, remove
previous occurrences of the same item from the history list first,
rather than just consing the new element onto the front of the list."
  :type 'boolean
  :group 'minibuffer)

(defvar minibuffer-completion-table nil
  "List, hash table, function or obarray used for minibuffer completion.

This becomes the COLLECTION argument to `try-completion', `all-completions'
and `test-completion'; see the documentation of those functions for how
values are interpreted.")

(defvar minibuffer-completion-predicate nil
  "Within call to `completing-read', this holds the PREDICATE argument.")

(defvar minibuffer-completion-confirm nil
  "Non-nil => demand confirmation of completion before exiting minibuffer.")

(defcustom minibuffer-confirm-incomplete nil
  "If true, then in contexts where completing-read allows answers which
are not valid completions, an extra RET must be typed to confirm the
response.  This is helpful for catching typos, etc."
  :type 'boolean
  :group 'minibuffer)

(defcustom completion-auto-help t
  "*Non-nil means automatically provide help for invalid completion input."
  :type 'boolean
  :group 'minibuffer)

(defcustom enable-recursive-minibuffers nil
  "*Non-nil means to allow minibuffer commands while in the minibuffer.
More precisely, this variable makes a difference when the minibuffer window
is the selected window.  If you are in some other window, minibuffer commands
are allowed even if a minibuffer is active."
  :type 'boolean
  :group 'minibuffer)

(defcustom minibuffer-max-depth 1
  ;; See comment in #'minibuffer-max-depth-exceeded
  "*Global maximum number of minibuffers allowed;
compare to enable-recursive-minibuffers, which is only consulted when the
minibuffer is reinvoked while it is the selected window."
  :type '(choice integer
		 (const :tag "Indefinite" nil))
  :group 'minibuffer)

;; Moved to C.  The minibuffer prompt must be setup before this is run
;; and that can only be done from the C side.
;(defvar minibuffer-setup-hook nil
;  "Normal hook run just after entry to minibuffer.")

;; see comment at list-mode-hook.
(put 'minibuffer-setup-hook 'permanent-local t)

(defvar minibuffer-exit-hook nil
  "Normal hook run just after exit from minibuffer.")
(put 'minibuffer-exit-hook 'permanent-local t)

(defvar minibuffer-help-form nil
  "Value that `help-form' takes on inside the minibuffer.")

(defvar minibuffer-default nil
  "Default value for minibuffer input.")
  
(defvar minibuffer-local-map
  (let ((map (make-sparse-keymap 'minibuffer-local-map)))
    map)
  "Default keymap to use when reading from the minibuffer.")

(defvar minibuffer-local-completion-map
  (let ((map (make-sparse-keymap 'minibuffer-local-completion-map)))
    (set-keymap-parents map (list minibuffer-local-map))
    map)
  "Local keymap for minibuffer input with completion.")

(defvar minibuffer-local-must-match-map
  (let ((map (make-sparse-keymap 'minibuffer-must-match-map)))
    (set-keymap-parents map (list minibuffer-local-completion-map))
    map)
  "Local keymap for minibuffer input with completion, for exact match.")

;; (define-key minibuffer-local-map "\C-g" 'abort-recursive-edit)
(define-key minibuffer-local-map "\C-g" 'minibuffer-keyboard-quit) ;; moved here from pending-del.el
(define-key minibuffer-local-map "\r" 'exit-minibuffer)
(define-key minibuffer-local-map "\n" 'exit-minibuffer)

;; Historical crock.  Unused by anything but user code, if even that
;(defvar minibuffer-local-ns-map
;  (let ((map (make-sparse-keymap 'minibuffer-local-ns-map)))
;    (set-keymap-parents map (list minibuffer-local-map))
;    map)
;  "Local keymap for the minibuffer when spaces are not allowed.")
;(define-key minibuffer-local-ns-map [space] 'exit-minibuffer)
;(define-key minibuffer-local-ns-map [tab] 'exit-minibuffer)
;(define-key minibuffer-local-ns-map [?\?] 'self-insert-and-exit)

(define-key minibuffer-local-completion-map "\t" 'minibuffer-complete)
(define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-completion-map "?" 'minibuffer-completion-help)
(define-key minibuffer-local-must-match-map "\r" 'minibuffer-complete-and-exit)
(define-key minibuffer-local-must-match-map "\n" 'minibuffer-complete-and-exit)

(define-key minibuffer-local-map "\M-n" 'next-history-element)
(define-key minibuffer-local-map "\M-p" 'previous-history-element)
(define-key minibuffer-local-map '[next]  "\M-n")
(define-key minibuffer-local-map '[prior] "\M-p")
(define-key minibuffer-local-map "\M-r" 'previous-matching-history-element)
(define-key minibuffer-local-map "\M-s" 'next-matching-history-element)
(define-key minibuffer-local-must-match-map [next]
  'next-complete-history-element)
(define-key minibuffer-local-must-match-map [prior]
  'previous-complete-history-element)

;; This is an experiment--make up and down arrows do history.
(define-key minibuffer-local-map [up] 'previous-history-element)
(define-key minibuffer-local-map [down] 'next-history-element)
(define-key minibuffer-local-completion-map [up] 'previous-history-element)
(define-key minibuffer-local-completion-map [down] 'next-history-element)
(define-key minibuffer-local-must-match-map [up] 'previous-history-element)
(define-key minibuffer-local-must-match-map [down] 'next-history-element)

(defvar read-expression-map (let ((map (make-sparse-keymap
					'read-expression-map)))
                              (set-keymap-parents map
						  (list minibuffer-local-map))
                              (define-key map "\M-\t" 'lisp-complete-symbol)
                              map)
  "Minibuffer keymap used for reading Lisp expressions.")

(defvar read-shell-command-map
  (let ((map (make-sparse-keymap 'read-shell-command-map)))
    (set-keymap-parents map (list minibuffer-local-map))
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-completions)
    map)
  "Minibuffer keymap used by `shell-command' and related commands.")

(defcustom use-dialog-box t
  "*Variable controlling usage of the dialog box.
If nil, the dialog box will never be used, even in response to mouse events."
  :type 'boolean
  :group 'minibuffer)

(defcustom minibuffer-electric-file-name-behavior t
  "*If non-nil, slash and tilde in certain places cause immediate deletion.
These are the same places where this behavior would occur later on anyway,
in `substitute-in-file-name'."
  :type 'boolean
  :group 'minibuffer)

;; originally by Stig@hackvan.com
(defun minibuffer-electric-separator ()
  (interactive)
  (let ((c last-command-char))
    (and minibuffer-electric-file-name-behavior
	 (eq c directory-sep-char)
	 (eq c (char-before (point)))
	 (not (save-excursion
	      (goto-char (point-min))
	      (and (looking-at "/.+:~?[^/]*/.+")
		   (re-search-forward "^/.+:~?[^/]*" nil t)
		   (progn
		     (delete-region (point) (point-max))
		     t))))
	 (not (save-excursion
		(goto-char (point-min))
		(and (looking-at ".+://[^/]*/.+")
		     (re-search-forward "^.+:/" nil t)
		     (progn
		       (delete-region (point) (point-max))
		       t))))
	 ;; permit `//hostname/path/to/file'
	 (not (eq (point) (1+ (point-min))))
	 ;; permit `http://url/goes/here'
	 (or (not (eq ?: (char-after (- (point) 2))))
	     (eq ?/ (char-after (point-min))))
       (delete-region (point-min) (point)))
    (insert c)))

(defun minibuffer-electric-tilde ()
  (interactive)
  (and minibuffer-electric-file-name-behavior
       (eq directory-sep-char (char-before (point)))
       ;; permit URL's with //, for e.g. http://hostname/~user
       (not (save-excursion (search-backward "//" nil t)))
       (delete-region (point-min) (point)))
  (insert ?~))


(defvar read-file-name-map
  (let ((map (make-sparse-keymap 'read-file-name-map)))
    (set-keymap-parents map (list minibuffer-local-completion-map))
    (define-key map (vector directory-sep-char) 'minibuffer-electric-separator)
    (define-key map "~" 'minibuffer-electric-tilde)
    map
    ))

(defvar read-file-name-must-match-map
  (let ((map (make-sparse-keymap 'read-file-name-map)))
    (set-keymap-parents map (list minibuffer-local-must-match-map))
    (define-key map (vector directory-sep-char) 'minibuffer-electric-separator)
    (define-key map "~" 'minibuffer-electric-tilde)
    map
    ))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
If `zmacs-regions' is true, and the zmacs region is active in this buffer,
then this key deactivates the region without beeping."
  (interactive)
  (if (region-active-p)
      ;; pseudo-zmacs compatibility: don't beep if this ^G is simply
      ;; deactivating the region.  If it is inactive, beep.
      nil
    (abort-recursive-edit)))

;;;; Guts of minibuffer invocation

;;#### The only things remaining in C are
;; "Vminibuf_prompt" and the display junk
;;  "minibuf_prompt_width" and "minibuf_prompt_pix_width"
;; Also "active_frame", though I suspect I could already
;;   hack that in Lisp if I could make any sense of the
;;   complete mess of frame/frame code in XEmacs.
;; Vminibuf_prompt could easily be made Lisp-bindable.
;;  I suspect that minibuf_prompt*_width are actually recomputed
;;  by redisplay as needed -- or could be arranged to be so --
;;  and that there could be need for read-minibuffer-internal to
;;  save and restore them.
;;#### The only other thing which read-from-minibuffer-internal does
;;  which we can't presently do in Lisp is move the frame cursor
;;  to the start of the minibuffer line as it returns.  This is
;;  a rather nice touch and should be preserved -- probably by
;;  providing some Lisp-level mechanism (extension to cursor-in-echo-area ?)
;;  to effect it.


;; Like reset_buffer in FSF's buffer.c
;;  (Except that kill-all-local-variables doesn't nuke 'permanent-local
;;   variables -- we preserve them, reset_buffer doesn't.)
(defun reset-buffer (buffer)
  (with-current-buffer buffer
    ;(if (fboundp 'unlock-buffer) (unlock-buffer))
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    ;; don't let read only text yanked into the minibuffer
    ;; permanently wedge it.
    (make-local-variable 'inhibit-read-only)
    (setq inhibit-read-only t)
    (erase-buffer)
    ;(setq default-directory nil)
    (setq buffer-file-name nil)
    (setq buffer-file-truename nil)
    (set-buffer-modified-p nil)
    (setq buffer-backed-up nil)
    (setq buffer-auto-save-file-name nil)
    (set-buffer-dedicated-frame buffer nil)
    (set-marker (mark-marker t buffer) nil)
    buffer))

(defvar minibuffer-history-variable 'minibuffer-history
  "History list symbol to add minibuffer values to.
Each minibuffer output is added with
  (set minibuffer-history-variable
       (cons STRING (symbol-value minibuffer-history-variable)))")
(defvar minibuffer-history-position)

;; Added by hniksic:
(defvar initial-minibuffer-history-position)
(defvar current-minibuffer-contents)
(defvar current-minibuffer-point)

(defcustom minibuffer-history-minimum-string-length nil
  "*If this variable is non-nil, a string will not be added to the
minibuffer history if its length is less than that value."
  :type '(choice (const :tag "Any" nil)
		 integer)
  :group 'minibuffer)

(define-error 'input-error "Keyboard input error" 'io-error)

((macro
  . (lambda (read-from-minibuffer-definition)
      (nsublis
       ;; `M-x doctor' makes (the interned) history a local variable, use an
       ;; uninterned symbol here so we don't interact with it.
       '((history . #:history))
       read-from-minibuffer-definition)))
 (defun read-from-minibuffer (prompt &optional initial-contents keymap
			      readp history abbrev-table default)
   "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  If HISTORY is `t', no history will be recorded.
  Positions are counted starting from 1 at the beginning of the list.
Sixth arg ABBREV-TABLE, if non-nil, becomes the value of `local-abbrev-table'
  in the minibuffer.
Seventh arg DEFAULT, if non-nil, will be returned when user enters
  an empty string.

See also the variable `completion-highlight-first-word-only' for
  control over completion display."
   (if (and (not enable-recursive-minibuffers)
	    (> (minibuffer-depth) 0)
	    (eq (selected-window) (minibuffer-window)))
       (error "Command attempted to use minibuffer while in minibuffer"))

   (if (and minibuffer-max-depth
	    (> minibuffer-max-depth 0)
	    (>= (minibuffer-depth) minibuffer-max-depth))
       (minibuffer-max-depth-exceeded))

   ;; catch this error before the poor user has typed something...
   (if history
       (if (symbolp history)
	   (or (boundp history)
	       (error "History list %S is unbound" history))
	 (or (boundp (car history))
	     (error "History list %S is unbound" (car history)))))

   (if (noninteractive)
       (progn
	 ;; XEmacs in -batch mode calls minibuffer: print the prompt.
	 (message "%s" (gettext prompt))
	 ;;#### force-output

	 ;;#### Should this even be falling though to the code below?
	 ;;#### How does this stuff work now, anyway?
	 ))
   (let* ((dir default-directory)
	  (owindow (selected-window))
	  (oframe (selected-frame))
	  (window (minibuffer-window))
	  (buffer (get-buffer-create (format " *Minibuf-%d*"
					     (minibuffer-depth))))
	  (frame (window-frame window))
	  (mconfig (if (eq frame (selected-frame))
		       nil (current-window-configuration frame)))
	  (oconfig (current-window-configuration))
	  (minibuffer-default default))
     (unwind-protect
         (progn
           (set-buffer (reset-buffer buffer))
           (setq default-directory dir)
           (make-local-variable 'print-escape-newlines)
           (setq print-escape-newlines t)
	   (make-local-variable 'current-minibuffer-contents)
	   (make-local-variable 'current-minibuffer-point)
	   (make-local-variable 'initial-minibuffer-history-position)
	   (setq current-minibuffer-contents ""
		 current-minibuffer-point 1)
	   (if (not minibuffer-smart-completion-tracking-behavior)
	       nil
	     (make-local-variable 'mode-motion-hook)
	     (or mode-motion-hook
		 ;;####disgusting
		 (setq mode-motion-hook 'minibuffer-smart-mouse-tracker))
	     (make-local-variable 'mouse-track-click-hook)
	     (add-hook 'mouse-track-click-hook
		       'minibuffer-smart-maybe-select-highlighted-completion))
           (set-window-buffer window buffer)
           (select-window window)
           (set-window-hscroll window 0)
           (buffer-enable-undo buffer)
           (message nil)
           (if initial-contents
               (if (consp initial-contents)
                   (progn
                     (insert (car initial-contents))
                     (goto-char (1+ (cdr initial-contents)))
		     (setq current-minibuffer-contents (car initial-contents)
			   current-minibuffer-point (cdr initial-contents)))
		 (insert initial-contents)
		 (setq current-minibuffer-contents initial-contents
		       current-minibuffer-point (point))))
           (use-local-map (help-keymap-with-help-key
			   (or keymap minibuffer-local-map)
			   minibuffer-help-form))
           (let ((mouse-grabbed-buffer
		  (and minibuffer-smart-completion-tracking-behavior
		       (current-buffer)))
                 (current-prefix-arg current-prefix-arg)
;;                 (help-form minibuffer-help-form)
                 (minibuffer-history-variable (cond ((not history)
                                                     'minibuffer-history)
                                                    ((consp history)
                                                     (car history))
                                                    (t
                                                     history)))
                 (minibuffer-history-position (cond ((consp history)
                                                     (cdr history))
                                                    (t
                                                     0)))
                 (minibuffer-scroll-window owindow))
	     (setq initial-minibuffer-history-position
		   minibuffer-history-position)
	     (if abbrev-table
		 (setq local-abbrev-table abbrev-table
		       abbrev-mode t))
	     ;; This is now run from read-minibuffer-internal
					;(if minibuffer-setup-hook
					;    (run-hooks 'minibuffer-setup-hook))
					;(message nil)
             (if (eq 't
                     (catch 'exit
                       (if (> (recursion-depth) (minibuffer-depth))
                           (let ((standard-output t)
                                 (standard-input t))
                             (read-minibuffer-internal prompt))
			 (read-minibuffer-internal prompt))))
                 ;; Translate an "abort" (throw 'exit 't)
                 ;;  into a real quit
                 (signal 'quit '())
               ;; return value
               (let* ((val (progn (set-buffer buffer)
                                  (if minibuffer-exit-hook
                                      (run-hooks 'minibuffer-exit-hook))
                                  (if (and (eq (char-after (point-min)) nil)
					   default)
				      default
				    (buffer-string))))
		      (histval (if (and default (string= val ""))
				   default
				 val))
                      (err nil))
                 (if readp
                     (condition-case e
                         (let ((v (read-from-string val)))
                           (if (< (cdr v) (length val))
                               (save-match-data
                                 (or (string-match "[ \t\n]*\\'" val (cdr v))
                                     (error "Trailing garbage following expression"))))
                           (setq v (car v))
                           ;; total total kludge
                           (if (stringp v) (setq v (list 'quote v)))
                           (setq val v))
                       (end-of-file
			(setq err
			      '(input-error "End of input before end of expression")))
		       (error (setq err e))))
                 ;; Add the value to the appropriate history list unless
                 ;; it's already the most recent element, or it's only
                 ;; two characters long.
                 (if (and (symbolp minibuffer-history-variable)
                          (boundp minibuffer-history-variable))
		     (let ((list (symbol-value minibuffer-history-variable)))
		       (or (eq list t)
			   (null val)
			   (and list (equal histval (car list)))
			   (and (stringp val)
				minibuffer-history-minimum-string-length
				(< (length val)
				   minibuffer-history-minimum-string-length))
			   (set minibuffer-history-variable
				(if minibuffer-history-uniquify
				    (cons histval (remove histval list))
				  (cons histval list))))))
                 (if err (signal (car err) (cdr err)))
                 val))))
       ;; stupid display code requires this for some reason
       (set-buffer buffer)
       (buffer-disable-undo buffer)
       (setq buffer-read-only nil)
       (erase-buffer)

       ;; restore frame configurations
       (if (and mconfig (frame-live-p oframe)
		(eq frame (selected-frame)))
	   ;; if we changed frames (due to surrogate minibuffer),
	   ;; and we're still on the new frame, go back to the old one.
	   (select-frame oframe))
       (if mconfig (set-window-configuration mconfig))
       (set-window-configuration oconfig)))))

(defun minibuffer-max-depth-exceeded ()
  ;;
  ;; This signals an error if an Nth minibuffer is invoked while N-1 are
  ;; already active, whether the minibuffer window is selected or not.
  ;; Since, under X, it's easy to jump out of the minibuffer (by doing M-x,
  ;; getting distracted, and clicking elsewhere) many many novice users have
  ;; had the problem of having multiple minibuffers build up, even to the
  ;; point of exceeding max-lisp-eval-depth.  Since the variable
  ;; enable-recursive-minibuffers historically/crockishly is only consulted
  ;; when the minibuffer is currently active (like typing M-x M-x) it doesn't
  ;; help in this situation.
  ;;
  ;; This routine also offers to edit .emacs for you to get rid of this
  ;; complaint, like `disabled' commands do, since it's likely that non-novice
  ;; users will be annoyed by this change, so we give them an easy way to get
  ;; rid of it forever.
  ;;
  (beep t 'minibuffer-limit-exceeded)
  (message
   "Minibuffer already active: abort it with `^]', enable new one with `n': ")
  (let ((char (let ((cursor-in-echo-area t)) ; #### doesn't always work??
		(read-char))))
    (cond
     ((eq char ?n)
      (cond
       ((y-or-n-p "Enable recursive minibuffers for other sessions too? ")
	;; This is completely disgusting, but it's basically what novice.el
	;; does.  This kind of thing should be generalized.
	(setq minibuffer-max-depth nil)
	(save-excursion
	  (set-buffer
	   (find-file-noselect
	    (substitute-in-file-name custom-file)))
	  (goto-char (point-min))
	  (if (re-search-forward
	       "^(setq minibuffer-max-depth \\([0-9]+\\|'?nil\\|'?()\\))\n"
	       nil t)
	      (delete-region (match-beginning 0 ) (match-end 0))
	    ;; Must have been disabled by default.
	    (goto-char (point-max)))
	  (insert"\n(setq minibuffer-max-depth nil)\n")
	  (save-buffer))
	(message "Multiple minibuffers enabled")
	(sit-for 1))))
     ((eq char ?)
      (abort-recursive-edit))
     (t
      (error "Minibuffer already active")))))


;;;; Guts of minibuffer completion


;; Used by minibuffer-do-completion
(defvar last-exact-completion nil)

(defun temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (message nil)
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      ;;  If the user types a ^G while we're in sit-for, then quit-flag
      ;;  gets set. In this case, we want that ^G to be interpreted
      ;;  as a normal character, and act just like typeahead.
      (if (and quit-flag (not unread-command-event))
          (setq unread-command-event (character-to-event (quit-char))
                quit-flag nil)))))

;; 0 'none                 no possible completion
;; 1 'unique               was already an exact and unique completion
;; 3 'exact                was already an exact (but nonunique) completion
;; NOT USED 'completed-exact-unique completed to an exact and completion
;; 4 'completed-exact      completed to an exact (but nonunique) completion
;; 5 'completed            some completion happened
;; 6 'uncompleted          no completion happened
(defun minibuffer-do-completion-1 (buffer-string completion)
  (cond ((not completion)
         'none)
        ((eq completion t)
         ;; exact and unique match
         'unique)
        (t
         ;; It did find a match.  Do we match some possibility exactly now?
         (let ((completedp (not (string-equal completion buffer-string))))
           (if completedp
               (progn
                 ;; Some completion happened
                 (erase-buffer)
                 (insert completion)
                 (setq buffer-string completion)))
           (if (test-completion buffer-string minibuffer-completion-table
                                minibuffer-completion-predicate)
               ;; An exact completion was possible
               (if completedp
;; Since no callers need to know the difference, don't bother
;;  with this (potentially expensive) discrimination.
;;                 (if (eq (try-completion completion
;;                                         minibuffer-completion-table
;;                                         minibuffer-completion-predicate)
;;                         't)
;;                     'completed-exact-unique
                       'completed-exact
;;                     )
                   'exact)
               ;; Not an exact match
               (if completedp
                   'completed
                   'uncompleted))))))


(defun minibuffer-do-completion (buffer-string)
  (let* ((completion (try-completion buffer-string
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (status (minibuffer-do-completion-1 buffer-string completion))
         (last last-exact-completion))
    (setq last-exact-completion nil)
    (cond ((eq status 'none)
           ;; No completions
           (ding nil 'no-completion)
           (temp-minibuffer-message " [No match]"))
          ((eq status 'unique)
           )
          (t
           ;; It did find a match.  Do we match some possibility exactly now?
           (if (not (string-equal completion buffer-string))
               (progn
                 ;; Some completion happened
                 (erase-buffer)
                 (insert completion)
                 (setq buffer-string completion)))
           (cond ((eq status 'exact)
                  ;; If the last exact completion and this one were
                  ;;  the same, it means we've already given a
                  ;;  "Complete but not unique" message and that the
                  ;;  user's hit TAB again, so now we give help.
                  (setq last-exact-completion completion)
                  (if (equal buffer-string last)
                      (minibuffer-completion-help)))
                 ((eq status 'uncompleted)
                  (if completion-auto-help
                      (minibuffer-completion-help)
                      (temp-minibuffer-message " [Next char not unique]")))
                 (t
                  nil))))
    status))


;;;; completing-read

(defun completing-read (prompt collection &optional predicate require-match
                        initial-contents history default)
  "Read a string in the minibuffer, with completion.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION is a set of objects that are the possible completions.
PREDICATE limits completion to a subset of COLLECTION.
See `try-completion' and `all-completions' for details of COLLECTION,
  PREDICATE, and completion in general.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
  the input is (or completes to) an element of COLLECTION or is null.
  If it is also not t, Return does not exit if it does non-null completion.
If INITIAL-CONTENTS is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.

HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  If HISTORY is `t', no history will be recorded.
  Positions are counted starting from 1 at the beginning of the list.
DEFAULT, if non-nil, will be returned when the user enters an empty
  string.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil."
  (let ((minibuffer-completion-table collection)
        (minibuffer-completion-predicate predicate)
        (minibuffer-completion-confirm (if (eq require-match 't) nil t))
        (last-exact-completion nil)
	ret)
    (setq ret (read-from-minibuffer prompt
				    initial-contents
				    (if (not require-match)
					minibuffer-local-completion-map
				      minibuffer-local-must-match-map)
				    nil
				    history
				    nil
				    default))
    (if (and (string= ret "")
	     default)
	default
      ret)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   Minibuffer completion commands                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun minibuffer-complete ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive)
  ;; If the previous command was not this, then mark the completion
  ;;  buffer obsolete.
  (or (eq last-command this-command)
      (setq minibuffer-scroll-window nil))
  (let ((window minibuffer-scroll-window))
    (if (and window (windowp window) (window-buffer window)
             (buffer-name (window-buffer window)))
	;; If there's a fresh completion window with a live buffer
	;;  and this command is repeated, scroll that window.
	(let ((obuf (current-buffer)))
          (unwind-protect
	      (progn
		(set-buffer (window-buffer window))
		(if (pos-visible-in-window-p (point-max) window)
		    ;; If end is in view, scroll up to the beginning.
		    (set-window-start window (point-min))
		  ;; Else scroll down one frame.
		  (scroll-other-window)))
	    (set-buffer obuf))
          nil)
      (let ((status (minibuffer-do-completion (buffer-string))))
	(if (eq status 'none)
	    nil
	  (progn
	    (cond ((eq status 'unique)
		   (temp-minibuffer-message
		    " [Sole completion]"))
		  ((eq status 'exact)
		   (temp-minibuffer-message
		    " [Complete, but not unique]")))
	    t))))))


(defun minibuffer-complete-and-exit ()
  "Complete the minibuffer contents, and maybe exit.
Exit if the name is valid with no completion needed.
If name was completed to a valid match,
a repetition of this command will exit."
  (interactive)
  (if (= (point-min) (point-max))
      ;; Crockishly allow user to specify null string
      (throw 'exit nil))
  (let ((buffer-string (buffer-string)))
    ;; Short-cut -- don't call minibuffer-do-completion if we already
    ;;  have an (possibly nonunique) exact completion.
    (if (test-completion buffer-string minibuffer-completion-table
                                minibuffer-completion-predicate)
        (throw 'exit nil))
    (let ((status (minibuffer-do-completion buffer-string)))
      (if (or (eq status 'unique)
              (eq status 'exact)
              (if (or (eq status 'completed-exact)
                      (eq status 'completed-exact-unique))
                  (if minibuffer-completion-confirm
                      (progn (temp-minibuffer-message " [Confirm]")
                             nil)
                      t)))
          (throw 'exit nil)))))


(defun self-insert-and-exit ()
  "Terminate minibuffer input."
  (interactive)
  (self-insert-command 1)
  (throw 'exit nil))

(defun exit-minibuffer ()
  "Terminate this minibuffer argument.
If minibuffer-confirm-incomplete is true, and we are in a completing-read
of some kind, and the contents of the minibuffer is not an existing
completion, requires an additional RET before the minibuffer will be exited
\(assuming that RET was the character that invoked this command:
the character in question must be typed again)."
  (interactive)
  (if (not minibuffer-confirm-incomplete)
      (throw 'exit nil))
  (let ((buffer-string (buffer-string)))
    (if (test-completion buffer-string minibuffer-completion-table
                                minibuffer-completion-predicate)
        (throw 'exit nil))
    (let ((completion (if (not minibuffer-completion-table)
                          t
                          (try-completion buffer-string
                                          minibuffer-completion-table
                                          minibuffer-completion-predicate))))
      (if (or (eq completion 't)
              ;; Crockishly allow user to specify null string
              (string-equal buffer-string ""))
          (throw 'exit nil))
      (if completion ;; rewritten for I18N3 snarfing
	  (temp-minibuffer-message " [incomplete; confirm]")
	(temp-minibuffer-message " [no completions; confirm]"))
      (let ((event (let ((inhibit-quit t))
		     (prog1
			 (next-command-event)
		       (setq quit-flag nil)))))
        (cond ((equal event last-command-event)
               (throw 'exit nil))
              ((equal (quit-char) (event-to-character event))
               ;; Minibuffer abort.
               (throw 'exit t)))
        (dispatch-event event)))))

;;;; minibuffer-complete-word


;;;#### I think I have done this correctly; it certainly is simpler
;;;#### than what the C code seemed to be trying to do.
(defun minibuffer-complete-word ()
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t."
  (interactive)
  (let* ((buffer-string (buffer-string))
         (completion (try-completion buffer-string
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (status (minibuffer-do-completion-1 buffer-string completion)))
    (cond ((eq status 'none)
           (ding nil 'no-completion)
           (temp-minibuffer-message " [No match]")
           nil)
          ((eq status 'unique)
           ;; New message, only in this new Lisp code
           (temp-minibuffer-message " [Sole completion]")
           t)
          (t
           (cond ((or (eq status 'uncompleted)
                      (eq status 'exact))
                  (let ((foo #'(lambda (s)
				 (condition-case nil
				     (if (try-completion
					  (concat buffer-string s)
					  minibuffer-completion-table
					  minibuffer-completion-predicate)
					 (progn
					   (goto-char (point-max))
					   (insert s)
					   t)
                                       nil)
                                   (error nil))))
                        (char last-command-char))
                    ;; Try to complete by adding a word-delimiter
                    (or (and (characterp char) (> char 0)
                             (funcall foo (char-to-string char)))
                        (and (not (eq char ?\ ))
                             (funcall foo " "))
                        (and (not (eq char ?\-))
                             (funcall foo "-"))
                        (progn
                          (if completion-auto-help
                              (minibuffer-completion-help)
                              ;; New message, only in this new Lisp code
			    ;; rewritten for I18N3 snarfing
			    (if (eq status 'exact)
				(temp-minibuffer-message
				 " [Complete, but not unique]")
			      (temp-minibuffer-message " [Ambiguous]")))
                          nil))))
                 (t
                  (erase-buffer)
                  (insert completion)
                  ;; First word-break in stuff found by completion
                  (goto-char (point-min))
                  (let ((len (length buffer-string))
                        n)
                    (if (and (< len (length completion))
                             (catch 'match
                               (setq n 0)
                               (while (< n len)
                                 (if (char-equal
                                       (upcase (aref buffer-string n))
                                       (upcase (aref completion n)))
                                     (setq n (1+ n))
                                     (throw 'match nil)))
                               t)
                             (progn
                               (goto-char (point-min))
                               (forward-char len)
                               (re-search-forward "\\W" nil t)))
                        (delete-region (point) (point-max))
                        (goto-char (point-max))))
                  t))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      "Smart minibuffer" hackery                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ("Kludgy minibuffer hackery" is perhaps a better name)

;; This works by setting `mouse-grabbed-buffer' to the minibuffer,
;; defining button2 in the minibuffer keymap to
;; `minibuffer-smart-select-highlighted-completion', and setting the
;; mode-motion-hook of the minibuffer to `minibuffer-mouse-tracker'.
;; By setting `mouse-grabbed-buffer', the minibuffer's keymap and
;; mode-motion-hook apply (for mouse motion and presses) no matter
;; what buffer the mouse is over.  Then, `minibuffer-mouse-tracker'
;; examines the text under the mouse looking for something that looks
;; like a completion, and causes it to be highlighted, and
;; `minibuffer-smart-select-highlighted-completion' looks for a
;; flagged completion under the mouse and inserts it.  This has the
;; following advantages:
;;
;; -- filenames and such in any buffer can be inserted by clicking,
;;    not just completions
;;
;; but the following disadvantages:
;;
;; -- unless you're aware of the "filename in any buffer" feature,
;;    the fact that strings in arbitrary buffers get highlighted appears
;;    as a bug
;; -- mouse motion can cause ange-ftp actions -- bad bad bad.
;;
;; There's some hackery in minibuffer-mouse-tracker to try to avoid the
;; ange-ftp stuff, but it doesn't work.
;;

(defcustom minibuffer-smart-completion-tracking-behavior nil
  "*If non-nil, look for completions under mouse in all buffers.
This allows you to click on something that looks like a completion
and have it selected, regardless of what buffer it is in.

This is not enabled by default because

-- The \"mysterious\" highlighting in normal buffers is confusing to
   people not expecting it, and looks like a bug
-- If ange-ftp is enabled, this tracking sometimes causes ange-ftp
   action as a result of mouse motion, which is *bad bad bad*.
   Hopefully this bug will be fixed at some point."
  :type 'boolean
  :group 'minibuffer)

(defun minibuffer-smart-mouse-tracker (event)
  ;; Used as the mode-motion-hook of the minibuffer window, which is the
  ;; value of `mouse-grabbed-buffer' while the minibuffer is active.  If
  ;; the word under the mouse is a valid minibuffer completion, then it
  ;; is highlighted.
  ;;
  ;; We do some special voodoo when we're reading a pathname, because
  ;; the way filename completion works is funny.  Possibly there's some
  ;; more general way this could be dealt with...
  ;;
  ;; We do some further voodoo when reading a pathname that is an
  ;; ange-ftp or efs path, because causing FTP activity as a result of
  ;; mouse motion is a really bad time.
  ;;
  (and minibuffer-smart-completion-tracking-behavior
       (event-point event)
       ;; avoid conflict with display-completion-list extents
       (not (extent-at (event-point event)
		       (event-buffer event)
		       'list-mode-item))
       (let ((filename-kludge-p (eq minibuffer-completion-table
				    'read-file-name-internal)))
	 (mode-motion-highlight-internal
	  event
	  #'(lambda () (default-mouse-track-beginning-of-word
			 (if filename-kludge-p 'nonwhite t)))
	  #'(lambda ()
	      (let ((p (point))
		    (string ""))
		(default-mouse-track-end-of-word
		  (if filename-kludge-p 'nonwhite t))
		(if (and (/= p (point)) minibuffer-completion-table)
		    (setq string (buffer-substring p (point))))
		(if (string-match "\\`[ \t\n]*\\'" string)
		    (goto-char p)
		  (if filename-kludge-p
		      (setq string (minibuffer-smart-select-kludge-filename
				    string)))
		  ;; try-completion bogusly returns a string even when
		  ;; that string is complete if that string is also a
		  ;; prefix for other completions.  This means that we
		  ;; can't just do the obvious thing, (eq t
		  ;; (try-completion ...)).
                  ;; 
                  ;; Could be reasonable to use #'test-completion
                  ;; instead. Aidan Kehoe, Mo 14 Mai 2012 08:17:10 IST
		  (let (comp)
		    (if (and filename-kludge-p
			     ;; #### evil evil evil evil
			     (or (and (fboundp 'ange-ftp-ftp-path)
				      (declare-fboundp
				       (ange-ftp-ftp-path string)))
				 (and (fboundp 'efs-ftp-path)
				      (declare-fboundp
				       (efs-ftp-path string)))))
			(setq comp t)
		      (setq comp
			    (try-completion string
					    minibuffer-completion-table
					    minibuffer-completion-predicate)))
		    (or (eq comp t)
			(and (equal comp string)
			     (or (null minibuffer-completion-predicate)
				 (stringp
				  minibuffer-completion-predicate) ; ???
				 (funcall minibuffer-completion-predicate
					  (if (vectorp
					       minibuffer-completion-table)
					      (intern-soft
					       string
					       minibuffer-completion-table)
					    string))))
			(goto-char p))))))))))

(defun minibuffer-smart-select-kludge-filename (string)
  (save-excursion
    (set-buffer mouse-grabbed-buffer) ; the minibuf
    (let ((kludge-string (concat (buffer-string) string)))
      (if (or (and (fboundp 'ange-ftp-ftp-path)
		   (declare-fboundp (ange-ftp-ftp-path kludge-string)))
	       (and (fboundp 'efs-ftp-path)
		    (declare-fboundp (efs-ftp-path kludge-string))))
	  ;; #### evil evil evil, but more so.
	  string
	(append-expand-filename (buffer-string) string)))))

(defun minibuffer-smart-select-highlighted-completion (event)
  "Select the highlighted text under the mouse as a minibuffer response.
When the minibuffer is being used to prompt the user for a completion,
any valid completions which are visible on the frame will highlight
when the mouse moves over them.  Clicking \\<minibuffer-local-map>\
\\[minibuffer-smart-select-highlighted-completion] will select the
highlighted completion under the mouse.

If the mouse is clicked while not over a highlighted completion,
then the global binding of \\[minibuffer-smart-select-highlighted-completion] \
will be executed instead.  In this\nway you can get at the normal global \
behavior of \\[minibuffer-smart-select-highlighted-completion] as well as
the special minibuffer behavior."
  (interactive "e")
  (if minibuffer-smart-completion-tracking-behavior
      (minibuffer-smart-select-highlighted-completion-1 event t)
    (let ((command (lookup-key global-map
			       (vector current-mouse-event))))
      (if command (call-interactively command)))))

(defun minibuffer-smart-select-highlighted-completion-1 (event global-p)
  (let* ((filename-kludge-p (eq minibuffer-completion-table
				'read-file-name-internal))
	 completion
	 command-p
	 (evpoint (event-point event))
	 (evextent (and evpoint (extent-at evpoint (event-buffer event)
					   'list-mode-item))))
    (if evextent
	;; avoid conflict with display-completion-list extents.
	;; if we find one, do that behavior instead.
	(list-mode-item-selected-1 evextent event)
      (save-excursion
	(let* ((buffer (window-buffer (event-window event)))
	       (p (event-point event))
	       (extent (and p (extent-at p buffer 'mouse-face))))
	  (set-buffer buffer)
	  (if (not (and (extent-live-p extent)
			(eq (extent-object extent) (current-buffer))
			(not (extent-detached-p extent))))
	      (setq command-p t)
	    ;; ...else user has selected a highlighted completion.
	    (setq completion
		  (buffer-substring (extent-start-position extent)
				    (extent-end-position extent)))
	    (if filename-kludge-p
		(setq completion (minibuffer-smart-select-kludge-filename
				  completion)))
	    ;; remove the extent so that it's not hanging around in
	    ;; *Completions*
	    (detach-extent extent)
	    (set-buffer mouse-grabbed-buffer)
	    (erase-buffer)
	    (insert completion))))
      ;; we need to execute the command or do the throw outside of the
      ;; save-excursion.
      (cond ((and command-p global-p)
	     (let ((command (lookup-key global-map
					(vector current-mouse-event))))
	       (if command
		   (call-interactively command)
		 (if minibuffer-completion-table
		     (error
		      "Highlighted words are valid completions.  You may select one.")
		   (error "no completions")))))
	    ((not command-p)
	     ;; things get confused if the minibuffer is terminated while
	     ;; not selected.
	     (select-window (minibuffer-window))
	     (if (and filename-kludge-p (file-directory-p completion))
		 ;; if the user clicked middle on a directory name, display the
		 ;; files in that directory.
		 (progn
		   (goto-char (point-max))
		   (minibuffer-completion-help))
	       ;; otherwise, terminate input
	       (throw 'exit nil)))))))

(defun minibuffer-smart-maybe-select-highlighted-completion
  (event &optional click-count)
  "Like `minibuffer-smart-select-highlighted-completion' but does nothing if
there is no completion (as opposed to executing the global binding).  Useful
as the value of `mouse-track-click-hook'."
  (interactive "e")
  (minibuffer-smart-select-highlighted-completion-1 event nil))

(define-key minibuffer-local-map 'button2
  'minibuffer-smart-select-highlighted-completion)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Minibuffer History                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar minibuffer-history '()
  "Default minibuffer history list.
This is used for all minibuffer input except when an alternate history
list is specified.")

;; Some other history lists:
;;
(defvar minibuffer-history-search-history '())
(defvar function-history '())
(defvar variable-history '())
(defvar buffer-history '())
(defvar shell-command-history '())
(defvar file-name-history '())

(defvar read-expression-history nil)

(defvar minibuffer-history-sexp-flag nil ;weird FSF Emacs kludge
  "Non-nil when doing history operations on `command-history'.
More generally, indicates that the history list being acted on
contains expressions rather than strings.")

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil)
	 (minibuffer-max-depth (and minibuffer-max-depth
				    (1+ minibuffer-max-depth))))
     (if (eq 't (symbol-value minibuffer-history-variable))
	 (error "History is not being recorded in this context"))
     (list (read-from-minibuffer "Previous element matching (regexp): "
				 (car minibuffer-history-search-history)
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
	   (prefix-numeric-value current-prefix-arg))))
  (let ((history (symbol-value minibuffer-history-variable))
	prevpos
	(pos minibuffer-history-position))
    (if (eq history t)
	(error "History is not being recorded in this context"))
    (while (/= n 0)
      (setq prevpos pos)
      (setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
      (if (= pos prevpos)
	  (if (= pos 1) ;; rewritten for I18N3 snarfing
	      (error "No later matching history item")
	    (error "No earlier matching history item")))
      (if (string-match regexp
			(if minibuffer-history-sexp-flag
			    (let ((print-level nil))
			      (prin1-to-string (nth (1- pos) history)))
                            (nth (1- pos) history)))
	  (setq n (+ n (if (< n 0) 1 -1)))))
    (setq minibuffer-history-position pos)
    (setq current-minibuffer-contents (buffer-string)
	  current-minibuffer-point (point))
    (erase-buffer)
    (let ((elt (nth (1- pos) history)))
      (insert (if minibuffer-history-sexp-flag
		  (let ((print-level nil))
		    (prin1-to-string elt))
                  elt)))
      (goto-char (point-min)))
  (if (or (eq (car (car command-history)) 'previous-matching-history-element)
	  (eq (car (car command-history)) 'next-matching-history-element))
      (setq command-history (cdr command-history))))

(defun next-matching-history-element (regexp n)
  "Find the next history element that matches REGEXP.
\(The next history element refers to a more recent action.)
With prefix argument N, search for Nth next match.
If N is negative, find the previous or Nth previous match."
  (interactive
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil)
	 (minibuffer-max-depth (and minibuffer-max-depth
				    (1+ minibuffer-max-depth))))
     (if (eq t (symbol-value minibuffer-history-variable))
	 (error "History is not being recorded in this context"))
     (list (read-from-minibuffer "Next element matching (regexp): "
				 (car minibuffer-history-search-history)
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
	   (prefix-numeric-value current-prefix-arg))))
  (previous-matching-history-element regexp (- n)))

(defun next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (if (eq 't (symbol-value minibuffer-history-variable))
      (error "History is not being recorded in this context"))
  (unless (zerop n)
    (when (eq minibuffer-history-position
	      initial-minibuffer-history-position)
      (setq current-minibuffer-contents (buffer-string)
	    current-minibuffer-point (point)))
    (let ((narg (- minibuffer-history-position n))
	  (minimum (if minibuffer-default -1 0)))
      ;; a weird special case here; when in repeat-complex-command, we're
      ;; trying to edit the top command, and minibuffer-history-position
      ;; points to 1, the next-to-top command.  in this case, the top
      ;; command in the history is suppressed in favor of the one being
      ;; edited, and there is no more command below it, except maybe the
      ;; default.
      (if (and (zerop narg) (eq minibuffer-history-position
				initial-minibuffer-history-position))
	  (setq minimum (1+ minimum)))
      (cond ((< narg minimum)
	     (error (if minibuffer-default
			"No following item in %s"
		      "No following item in %s; no default available")
		    minibuffer-history-variable))
	    ((> narg (length (symbol-value minibuffer-history-variable)))
	     (error "No preceding item in %s" minibuffer-history-variable)))
      (erase-buffer)
      (setq minibuffer-history-position narg)
      (if (eq narg initial-minibuffer-history-position)
	  (progn
	    (insert current-minibuffer-contents)
	    (goto-char current-minibuffer-point))
	(let ((elt (if (> narg 0)
		       (nth (1- minibuffer-history-position)
			    (symbol-value minibuffer-history-variable))
		     minibuffer-default)))
	  (insert
	   (if (not (stringp elt))
	       (let ((print-level nil))
		 (condition-case nil
		     (let ((print-readably t)
			   (print-escape-newlines t))
		       (prin1-to-string elt))
		   (error (prin1-to-string elt))))
	     elt)))
	;; FSF has point-min here.
	(goto-char (point-max))))))

(defun previous-history-element (n)
  "Insert the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element (- n)))

(defun next-complete-history-element (n)
  "Get next element of history which is a completion of minibuffer contents."
  (interactive "p")
  (let ((point-at-start (point)))
    (next-matching-history-element
     (concat "^" (regexp-quote (buffer-substring (point-min) (point)))) n)
    ;; next-matching-history-element always puts us at (point-min).
    ;; Move to the position we were at before changing the buffer contents.
    ;; This is still sensical, because the text before point has not changed.
    (goto-char point-at-start)))

(defun previous-complete-history-element (n)
  "Get previous element of history which is a completion of minibuffer contents."
  (interactive "p")
  (next-complete-history-element (- n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                reading various things from a minibuffer            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-expression (prompt &optional initial-contents history default-value)
  "Return a Lisp object read using the minibuffer, prompting with PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
 in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history command, and as the value to return if the user enters the
 empty string."
  (let ((minibuffer-history-sexp-flag t)
	;; Semi-kludge to get around M-x C-x o M-ESC trying to do completion.
	(minibuffer-completion-table nil))
    (read-from-minibuffer prompt
			  initial-contents
			  read-expression-map
			  t
			  (or history 'read-expression-history)
			  lisp-mode-abbrev-table
			  default-value)))

(defun read-string (prompt &optional initial-contents history default-value)
  "Return a string from the minibuffer, prompting with string PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
 in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history command, and as the value to return if the user enters the
 empty string."
  (let ((minibuffer-completion-table nil))
    (read-from-minibuffer prompt
			  initial-contents
			  minibuffer-local-map
			  nil history nil default-value)))

(defun eval-minibuffer (prompt &optional initial-contents history default-value)
  "Return value of Lisp expression read using the minibuffer.
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history command, and as the value to return if the user enters the
 empty string."
  (eval (read-expression prompt initial-contents history default-value)))

;; The name `command-history' is already taken
(defvar read-command-history '())

(defun read-command (prompt &optional default-value)
  "Read the name of a command and return as a symbol.
Prompts with PROMPT.  By default, return DEFAULT-VALUE."
  (intern (completing-read prompt obarray 'commandp t nil
			   ;; 'command-history is not right here: that's a
			   ;; list of evalable forms, not a history list.
			   'read-command-history
			   default-value)))

(defun read-function (prompt &optional default-value)
  "Read the name of a function and return as a symbol.
Prompts with PROMPT.  By default, return DEFAULT-VALUE."
  (intern (completing-read prompt obarray 'fboundp t nil
			   'function-history default-value)))

(defun read-variable (prompt &optional default-value)
  "Read the name of a user variable and return it as a symbol.
Prompts with PROMPT.  By default, return DEFAULT-VALUE.
A user variable is one whose documentation starts with a `*' character."
  (intern (completing-read prompt obarray 'user-variable-p t nil
			   'variable-history
			   (if (symbolp default-value)
			       (symbol-name default-value)
			     default-value))))

(defun read-buffer (prompt &optional default require-match exclude)
  "Read the name of a buffer and return as a string.
Prompts with PROMPT.  Optional second arg DEFAULT is value to return if user
enters an empty line.  If optional third arg REQUIRE-MATCH is non-nil,
only existing buffer names are allowed.  Optional fourth argument EXCLUDE is
a buffer or a list of buffers to exclude from the completion list."
  (when (bufferp exclude)
    (setq exclude (list exclude)))
  (let ((prompt (if default
                    (format "%s(default %s) "
                            (gettext prompt) (if (bufferp default)
						 (buffer-name default)
					       default))
		    prompt))
	(alist (mapcar #'(lambda (b) (cons (buffer-name b) b))
                       (set-difference (buffer-list) exclude)))
	result)
    (while (progn
             (setq result (completing-read prompt alist nil require-match
					   nil 'buffer-history 
					   (if (bufferp default)
					       (buffer-name default)
					     default)))
             (cond ((not (equal result ""))
                    nil)
                   ((not require-match)
                    (setq result default)
                    nil)
                   ((not default)
                    nil)
                   ((not (get-buffer default))
                    t)
                   (t
                    (setq result default)
                    nil))))
    (if (bufferp result)
        (buffer-name result)
      result)))

(defun read-number (prompt &optional integers-only default-value)
  "Read a number from the minibuffer, prompting with PROMPT.
If optional second argument INTEGERS-ONLY is non-nil, accept
 only integer input.
If DEFAULT-VALUE is non-nil, return that if user enters an empty
 line."
  (let ((pred (if integers-only 'integerp 'numberp))
	num)
    (while (not (funcall pred num))
      (setq num (condition-case ()
		    (let ((minibuffer-completion-table nil))
		      (read-from-minibuffer
		       prompt (if num (prin1-to-string num)) nil t
		       nil nil default-value))
		  (input-error nil)
		  (invalid-read-syntax nil)
		  (end-of-file nil)))
      (or (funcall pred num) (beep)))
    num))

(defun read-shell-command (prompt &optional initial-input history default-value)
  "Just like read-string, but uses read-shell-command-map:
\\{read-shell-command-map}"
  (let ((minibuffer-completion-table nil))
    (read-from-minibuffer prompt initial-input read-shell-command-map
			  nil (or history 'shell-command-history)
			  nil default-value)))


;;; This read-file-name stuff probably belongs in files.el

;; Quote "$" as "$$" to get it past substitute-in-file-name
(defun un-substitute-in-file-name (string)
  (let ((regexp "\\$")
        (olen (length string))
        new
        n o ch)
    (if (not (string-match regexp string))
	string
      (setq n 1)
      (while (string-match regexp string (match-end 0))
	(setq n (1+ n)))
      (setq new (make-string (+ olen n) ?$))
      (setq n 0 o 0)
      (while (< o olen)
	(setq ch (aref string o))
	(aset new n ch)
	(setq o (1+ o) n (1+ n))
	(if (eq ch ?$)
	    ;; already aset by make-string initial-value
	    (setq n (1+ n))))
      new)))


;; Wrapper for `directory-files' for use in generating completion lists.
;; Generates output in the same format as `file-name-all-completions'.
;;
;; The EFS replacement for `directory-files' doesn't support the FILES-ONLY
;; option, so it has to be faked.  The listing cache will hopefully
;; improve the performance of this operation.
(defun minibuf-directory-files (dir &optional match-regexp files-only)
  (let ((want-file (or (eq files-only nil) (eq files-only t)))
        (want-dirs (or (eq files-only nil) (not (eq files-only t)))))
    (mapcan
     #'(lambda (f)
         (and (not (equal "." f))
              (if (file-directory-p (expand-file-name f dir))
                  (and want-dirs (list (file-name-as-directory f)))
                (and want-file (list f)))))
     (directory-files dir nil match-regexp))))


(defun read-file-name-2 (history prompt dir default
				 must-match initial-contents
				 completer)
  (if (not dir)
      (setq dir default-directory))
  (setq dir (abbreviate-file-name dir t))
  (let* ((insert (cond ((and (not insert-default-directory)
			     (not initial-contents))
                        "")
                       (initial-contents
                        (cons (un-substitute-in-file-name
			       (concat dir initial-contents))
                              (length dir)))
                       (t
                        (un-substitute-in-file-name dir))))
         (val 
                ;;  Hateful, broken, case-sensitive un*x
;;;                 (completing-read prompt
;;;                                  completer
;;;                                  dir
;;;                                  must-match
;;;                                  insert
;;;                                  history)
	  ;; #### - this is essentially the guts of completing read.
	  ;; There should be an elegant way to pass a pair of keymaps to
	  ;; completing read, but this will do for now.  All sins are
	  ;; relative.  --Stig
	  (let ((minibuffer-completion-table completer)
		(minibuffer-completion-predicate dir)
		(minibuffer-completion-confirm (if (eq must-match 't)
						   nil t))
		(last-exact-completion nil))
	    (read-from-minibuffer prompt
				  insert
				  (if (not must-match)
				      read-file-name-map
				    read-file-name-must-match-map)
				  nil
				  history
				  nil
				  default))))
;;;     ;; Kludge!  Put "/foo/bar" on history rather than "/default//foo/bar"
;;;     (let ((hist (cond ((not history) 'minibuffer-history)
;;;                       ((consp history) (car history))
;;;                       (t history))))
;;;       (if (and val
;;;                hist
;;;                (not (eq hist 't))
;;;                (boundp hist)
;;;                (equal (car-safe (symbol-value hist)) val))
;;;           (let ((e (condition-case nil
;;;                        (expand-file-name val)
;;;                      (error nil))))
;;;             (if (and e (not (equal e val)))
;;;                 (set hist (cons e (cdr (symbol-value hist))))))))

    (cond ((not val)
           (error "No file name specified"))
          ((and default
                (equal val (if (consp insert) (car insert) insert)))
           default)
          (t
           (substitute-in-file-name val)))))

;; #### this function should use minibuffer-completion-table
;; or something.  But that is sloooooow.
;; #### all this shit needs better documentation!!!!!!!!
(defun read-file-name-activate-callback (event extent dir-p)
  ;; used as the activate-callback of the filename list items
  ;; in the completion buffer, in place of default-choose-completion.
  ;; if a regular file was selected, we call default-choose-completion
  ;; (which just inserts the string in the minibuffer and calls
  ;; exit-minibuffer).  If a directory was selected, we display
  ;; the contents of the directory.
  (let* ((file (extent-string extent))
	 (completion-buf (extent-object extent))
	 (minibuf (symbol-value-in-buffer 'completion-reference-buffer
					  completion-buf))
	 (in-dir (file-name-directory (buffer-substring nil nil minibuf)))
	 (full (expand-file-name file in-dir)))
    (if (not (file-directory-p full))
	(default-choose-completion event extent minibuf)
      (erase-buffer minibuf)
      (insert-string (file-name-as-directory
		      (abbreviate-file-name full t)) minibuf)
      (reset-buffer completion-buf)
      (let ((standard-output completion-buf))
	(display-completion-list
         (minibuf-directory-files full nil (if dir-p 'directory))
	 :user-data dir-p
	 :reference-buffer minibuf
	 :activate-callback 'read-file-name-activate-callback)
	(goto-char (point-min) completion-buf)))))

(defun read-file-name-1 (type history prompt dir default
			      must-match initial-contents
			      completer)
  (if (should-use-dialog-box-p)
      (condition-case nil
	  (let ((file
		 (apply #'make-dialog-box
			type `(:title ,(capitalize-string-as-title
					;; Kludge: Delete ": " off the end.
					(replace-in-string prompt ": $" ""))
				      ,@(and dir (list :initial-directory
						       dir))
				      :file-must-exist ,must-match
				      ,@(and initial-contents
					     (list :initial-filename
						   initial-contents))))))
	    ;; hack -- until we implement reading a directory properly,
	    ;; allow a file as indicating the directory it's in
	    (if (and (eq completer 'read-directory-name-internal)
		     (not (file-directory-p file)))
		(file-name-directory file)
	      file))
	(unimplemented
	 ;; this calls read-file-name-2
	 (mouse-read-file-name-1 history prompt dir default must-match
				 initial-contents completer)
	 ))
    (add-one-shot-hook
     'minibuffer-setup-hook
     (lambda ()
       (and (file-system-ignore-case-p (or dir default-directory))
	    (set (make-local-variable 'completion-ignore-case) t))
       (set
	(make-local-variable
	 'completion-display-completion-list-function)
	#'(lambda (completions)
	    (display-completion-list
	     completions
	     :user-data (not (eq completer 'read-file-name-internal))
	     :activate-callback
	     'read-file-name-activate-callback)))))
    (read-file-name-2 history prompt dir default must-match
		      initial-contents completer)))

(defun read-file-name (prompt
                       &optional dir default must-match initial-contents
		       history)
  "Read file name, prompting with PROMPT and completing in directory DIR.
This will prompt with a dialog box if appropriate, according to
 `should-use-dialog-box-p'.
Value is not expanded---you must call `expand-file-name' yourself.
Value is subject to interpretation by `substitute-in-file-name' however.
Default name to DEFAULT if user enters a null string.
 (If DEFAULT is omitted, the visited file name is used,
  except that if INITIAL-CONTENTS is specified, that combined with DIR is
  used.)
Fourth arg MUST-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-CONTENTS specifies text to start with.  If this is not
 specified, and `insert-default-directory' is non-nil, DIR or the current
 directory will be used.
Sixth arg HISTORY specifies the history list to use.  Default is
 `file-name-history'.
DIR defaults to current buffer's directory default."
  (read-file-name-1 
   'file (or history 'file-name-history)
   prompt dir (or default
		  (and initial-contents
		       (abbreviate-file-name (expand-file-name
					      initial-contents dir) t))
		  (and buffer-file-truename
		       (abbreviate-file-name buffer-file-name t)))
   must-match initial-contents
   ;; A separate function (not an anonymous lambda-expression)
   ;; and passed as a symbol because of disgusting kludges in various
   ;; places which do stuff like (let ((filename-kludge-p (eq minibuffer-completion-table 'read-file-name-internal))) ...)
   'read-file-name-internal))

(defun read-directory-name (prompt
                            &optional dir default must-match initial-contents
			    history)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
This will prompt with a dialog box if appropriate, according to
 `should-use-dialog-box-p'.
Value is not expanded---you must call `expand-file-name' yourself.
Value is subject to interpreted by substitute-in-file-name however.
Default name to DEFAULT if user enters a null string.
 (If DEFAULT is omitted, the current buffer's default directory is used.)
Fourth arg MUST-MATCH non-nil means require existing directory's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-CONTENTS specifies text to start with.
Sixth arg HISTORY specifies the history list to use.  Default is
 `file-name-history'.
DIR defaults to current buffer's directory default."
  (read-file-name-1
   'directory (or history 'file-name-history)
   prompt dir (or default default-directory) must-match initial-contents
   'read-directory-name-internal))


;; Environment-variable and ~username completion hack
(defun read-file-name-internal-1 (string dir action completer)
  (if (not (string-match
	    "\\([^$]\\|\\`\\)\\(\\$\\$\\)*\\$\\([A-Za-z0-9_]*\\|{[^}]*\\)\\'"
	    string))
      ;; Not doing environment-variable completion hack
      (let* ((orig (if (equal string "") nil string))
	     (completion-ignore-case (file-system-ignore-case-p
				      (or dir default-directory)))
             (sstring (if orig (substitute-in-file-name string) string))
             (specdir (if orig (file-name-directory sstring) nil))
             (name    (if orig (file-name-nondirectory sstring) string))
             (direct  (if specdir (expand-file-name specdir dir) dir)))
        ;; ~username completion
        (if (and (fboundp 'user-name-completion-1)
                 (string-match "^[~]" name))
            (let ((user (substring name 1)))
              (cond ((eq action 'lambda)
                     (file-directory-p name))
                    ((eq action 't)
                     ;; all completions
                     (mapcar #'(lambda (p) (concat "~" p))
                             (declare-fboundp
			      (user-name-all-completions user))))
                    (t;; 'nil
                     ;; complete
                     (let* ((val+uniq (declare-fboundp
				       (user-name-completion-1 user)))
                            (val  (car val+uniq))
                            (uniq (cdr val+uniq)))
                       (cond ((stringp val)
                              (if uniq
                                  (file-name-as-directory (concat "~" val))
                                (concat "~" val)))
                             ((eq val t)
                              (file-name-as-directory name))
                             (t nil))))))
          (funcall completer
                   action
                   orig
                   sstring
                   specdir
                   direct
                   name)))
      ;; An odd number of trailing $'s
      (let* ((start (match-beginning 3))
	     (completion-ignore-case (file-system-ignore-case-p
				      (or dir default-directory)))
             (env (substring string
                             (cond ((eql start (length string))
                                    ;; "...$"
                                    start)
                                   ((eql (aref string start) ?{)
                                    ;; "...${..."
                                    (1+ start))
                                   (t
                                    start))))
             (head (substring string 0 (1- start)))
             (alist #'(lambda ()
                        (mapcar #'(lambda (x)
                                    (cons (substring x 0 (string-match "=" x))
                                          nil))
                                process-environment))))

	(cond ((eq action 'lambda)
               nil)
              ((eq action 't)
               ;; all completions
               (mapcar #'(lambda (p)
			   (if (and (> (length p) 0)
				    ;;#### Unix-specific
				    ;;####  -- need absolute-pathname-p
				    (/= (aref p 0) ?/))
			       (concat "$" p)
                             (concat head "$" p)))
                       (all-completions env (funcall alist))))
              (t ;; nil
               ;; complete
               (let* ((e (funcall alist))
                      (val (try-completion env e)))
                 (cond ((stringp val)
                        (if (string-match "[^A-Za-z0-9_]" val)
                            (concat head
                                    "${" val
                                    ;; completed uniquely?
                                    (if (eq (try-completion val e) 't)
                                        "}" ""))
                            (concat head "$" val)))
                       ((eql val 't)
                        (concat head
                                (un-substitute-in-file-name (getenv env))))
                       (t nil))))))))


(defun read-file-name-internal (string dir action)
  (read-file-name-internal-1
   string dir action
   #'(lambda (action orig string specdir dir name)
      (cond ((eq action 'lambda)
             (if (not orig)
                 nil
               (let ((sstring (condition-case nil
                                  (expand-file-name string)
                                (error nil))))
                 (if (not sstring)
                     ;; Some pathname syntax error in string
                     nil
                     (file-exists-p sstring)))))
            ((eq action 't)
             ;; all completions
             (mapcar #'un-substitute-in-file-name
                     (if (string= name "")
                         (delete "./" (file-name-all-completions "" dir))
                       (file-name-all-completions name dir))))
            (t;; nil
             ;; complete
             (let* ((d (or dir default-directory))
		    (val (file-name-completion name d)))
               (if (and (eq val 't)
                        (not (null completion-ignored-extensions)))
                   ;;#### (file-name-completion "foo") returns 't
                   ;;   when both "foo" and "foo~" exist and the latter
                   ;;   is "pruned" by completion-ignored-extensions.
                   ;; I think this is a bug in file-name-completion.
                   (setq val (let ((completion-ignored-extensions '()))
                               (file-name-completion name d))))
               (if (stringp val)
                   (un-substitute-in-file-name (if specdir
                                                   (concat specdir val)
                                                   val))
                   (let ((tem (un-substitute-in-file-name string)))
                     (if (not (equal tem orig))
                         ;; substitute-in-file-name did something
                         tem
                         val)))))))))

(defun read-directory-name-internal (string dir action)
  (read-file-name-internal-1
   string dir action
   #'(lambda (action orig string specdir dir name)
      (let* ((dirs #'(lambda (fn)
		       (let ((l (if (equal name "")
				    (minibuf-directory-files
				     dir
				     ""
				     'directories)
				  (minibuf-directory-files
				   dir
				   (concat "\\`" (regexp-quote name))
				   'directories))))
			 (mapcar fn
				 ;; Wretched unix
				 (delete "." l))))))
        (cond ((eq action 'lambda)
               ;; complete?
               (if (not orig)
                   nil
		 (file-directory-p string)))
              ((eq action 't)
               ;; all completions
               (funcall dirs #'(lambda (n)
				 (un-substitute-in-file-name
				  (file-name-as-directory n)))))
              (t
               ;; complete
               (let ((val (try-completion
                           name
                           (funcall dirs
                                    #'(lambda (n)
					(list (file-name-as-directory
					       n)))))))
                 (if (stringp val)
                     (un-substitute-in-file-name (if specdir
                                                     (concat specdir val)
						   val))
		   (let ((tem (un-substitute-in-file-name string)))
		     (if (not (equal tem orig))
			 ;; substitute-in-file-name did something
			 tem
		       val))))))))))

(defun append-expand-filename (file-string string)
  "Append STRING to FILE-STRING differently depending on whether STRING
is a username (~string), an environment variable ($string),
or a filename (/string).  The resultant string is returned with the
environment variable or username expanded and resolved to indicate
whether it is a file(/result) or a directory (/result/)."
  (let ((file
	 (cond ((string-match "\\([~$]\\)\\([^~$/]*\\)$" file-string)
		(cond ((string= (substring file-string
					   (match-beginning 1)
					   (match-end 1)) "~")
		       (concat (substring file-string 0 (match-end 1))
			       string))
		      (t (substitute-in-file-name
			  (concat (substring file-string 0 (match-end 1))
				  string)))))
	       (t (concat (file-name-directory
			   (substitute-in-file-name file-string)) string))))
	result)

    (cond ((stringp (setq result (and (file-exists-p (expand-file-name file))
				      (read-file-name-internal
				       (condition-case nil
					   (expand-file-name file)
					 (error file))
				       "" nil))))
	   result)
	  (t file))))

(defun mouse-rfn-setup-vars (prompt)
  ;; a specifier would be nice.
  (set (make-local-variable 'frame-title-format)
       (capitalize-string-as-title
	;; Kludge: Delete ": " off the end.
	(replace-in-string prompt ": $" "")))
  ;; ensure that killing the frame works right,
  ;; instead of leaving us in the minibuffer.
  (add-local-hook 'delete-frame-hook
		  #'(lambda (frame)
		      (abort-recursive-edit))))

(defun mouse-file-display-completion-list (window dir minibuf user-data)
  (let ((standard-output (window-buffer window)))
    (condition-case nil
	(display-completion-list
	 (minibuf-directory-files dir nil t)
	 :window-width (window-width window)
	 :window-height (window-text-area-height window)
	 :completion-string ""
	 :activate-callback
	 'mouse-read-file-name-activate-callback
	 :user-data user-data
	 :reference-buffer minibuf
	 :help-string "")
      (t nil))
    ))

(defun mouse-directory-display-completion-list (window dir minibuf user-data)
  (let ((standard-output (window-buffer window)))
    (condition-case nil
	(display-completion-list
	 (minibuf-directory-files dir nil 1)
	 :window-width (window-width window)
	 :window-height (window-text-area-height window)
	 :completion-string ""
	 :activate-callback
	 'mouse-read-file-name-activate-callback
	 :user-data user-data
	 :reference-buffer minibuf
	 :help-string "")
      (t nil))
    ))

(defun mouse-read-file-name-activate-callback (event extent user-data)
  (let* ((file (extent-string extent))
	 (minibuf (symbol-value-in-buffer 'completion-reference-buffer
					  (extent-object extent)))
	 (ministring (buffer-substring nil nil minibuf))
	 (in-dir (file-name-directory ministring))
	 (full (expand-file-name file in-dir))
	 (filebuf (nth 0 user-data))
	 (dirbuf (nth 1 user-data))
	 (filewin (nth 2 user-data))
	 (dirwin (nth 3 user-data)))
    (if (file-regular-p full)
	(default-choose-completion event extent minibuf)
      (erase-buffer minibuf)
      (insert-string (file-name-as-directory
		      (abbreviate-file-name full t)) minibuf)
      (reset-buffer filebuf)
      (if (not dirbuf)
	  (mouse-directory-display-completion-list filewin full minibuf
						   user-data)
	(mouse-file-display-completion-list filewin full minibuf user-data)
	(reset-buffer dirbuf)
	(mouse-directory-display-completion-list dirwin full minibuf
						 user-data)))))

;; our cheesy but god-awful time consuming file dialog box implementation.
;; this will be replaced with use of the native file dialog box (when
;; available).
(defun mouse-read-file-name-1 (history prompt dir default
				       must-match initial-contents
				       completer)
  ;; file-p is t if we're reading files, nil if directories.
  (let* ((file-p (eq 'read-file-name-internal completer))
	 (filebuf (get-buffer-create "*Completions*"))
	 (dirbuf (and file-p (generate-new-buffer " *mouse-read-file*")))
	 (butbuf (generate-new-buffer " *mouse-read-file-buttons*"))
	 (frame (make-dialog-frame))
	 filewin dirwin
	 user-data
	 (window-min-height 1)) ; allow button window to be height 2
    (unwind-protect
	(progn
	  (reset-buffer filebuf)

	  ;; set up the frame.
	  (focus-frame frame)
	  (split-window nil (- (window-height) 2))
	  (if file-p
	      (progn
		(split-window-horizontally 16)
		(setq filewin (frame-rightmost-window frame)
		      dirwin (frame-leftmost-window frame))
		(set-window-buffer filewin filebuf)
		(set-window-buffer dirwin dirbuf))
	    (setq filewin (frame-highest-window frame))
	    (set-window-buffer filewin filebuf))
	  (setq user-data (list filebuf dirbuf filewin dirwin))
	  (set-window-buffer (frame-lowest-window frame) butbuf)

	  ;; set up completion buffers.
	  (let ((rfcshookfun
		 ;; kludge!
		 ;; #### I really need to flesh out the object
		 ;; hierarchy better to avoid these kludges.
		 ;; (?? I wrote this comment above some time ago,
		 ;; and I don't understand what I'm referring to
		 ;; any more. --ben
		 (lambda ()
		   (mouse-rfn-setup-vars prompt)
		   (when-boundp 'scrollbar-width
		     (set-specifier scrollbar-width 0 (current-buffer)))
		   (setq truncate-lines t))))
	    
	    (set-buffer filebuf)
	    (add-local-hook 'completion-setup-hook rfcshookfun)
	    (when file-p
	      (set-buffer dirbuf)
	      (add-local-hook 'completion-setup-hook rfcshookfun)))

	  ;; set up minibuffer.
	  (add-one-shot-hook
	   'minibuffer-setup-hook
	   (lambda ()
	     (if (not file-p)
		 (mouse-directory-display-completion-list
		  filewin dir (current-buffer) user-data)
	       (mouse-file-display-completion-list
		filewin dir (current-buffer) user-data)
	       (mouse-directory-display-completion-list
		dirwin dir (current-buffer) user-data))
	     (set
	      (make-local-variable
	       'completion-display-completion-list-function)
	      (lambda (completions)
		(display-completion-list
		 completions
		 :help-string ""
		 :window-width (window-width filewin)
		 :window-height (window-text-area-height filewin)
		 :completion-string ""
		 :activate-callback
		 'mouse-read-file-name-activate-callback
		 :user-data user-data)))
	     (mouse-rfn-setup-vars prompt)
	     (save-selected-window
	       ;; kludge to ensure the frame title is correct.
	       ;; the minibuffer leaves the frame title the way
	       ;; it was before (i.e. of the selected window before
	       ;; the dialog box was opened), so to get it correct
	       ;; we have to be tricky.
	       (select-window filewin)
	       (redisplay-frame nil t)
	       ;; #### another kludge.  sometimes the focus ends up
	       ;; back in the main window, not the dialog box.  it
	       ;; occurs randomly and it's not possible to reliably
	       ;; reproduce.  We try to fix it by draining non-user
	       ;; events and then setting the focus back on the frame.
	       (sit-for 0 t)
	       (focus-frame frame))))

	  ;; set up button buffer.
	  (set-buffer butbuf)
	  (mouse-rfn-setup-vars prompt)
	  (when dir
	    (setq default-directory dir))
	  (when (featurep 'scrollbar)
	    (set-specifier scrollbar-width 0 butbuf))
	  (insert "                 ")
	  (insert-gui-button (make-gui-button "OK"
					      (lambda (foo)
						(exit-minibuffer))))
	  (insert "                 ")
	  (insert-gui-button (make-gui-button "Cancel"
					      (lambda (foo)
						(abort-recursive-edit))))

	  ;; now start reading filename.
	  (read-file-name-2 history prompt dir default
			    must-match initial-contents
			    completer))

      ;; always clean up.
      ;; get rid of our hook that calls abort-recursive-edit -- not a good
      ;; idea here.
      (kill-local-variable 'delete-frame-hook)
      (delete-frame frame)
      (kill-buffer filebuf)
      (kill-buffer butbuf)
      (and dirbuf (kill-buffer dirbuf)))))

(defun read-face (prompt &optional must-match)
  "Read the name of a face from the minibuffer and return it as a symbol."
  (intern (completing-read prompt obarray 'find-face must-match)))

(defun read-color-completion-table ()
  (mapcar #'list (color-list)))

(defun read-color (prompt &optional must-match initial-contents)
  "Read the name of a color from the minibuffer.
On X devices, this uses `x-library-search-path' to find rgb.txt in order
 to build a completion table.
On TTY devices, this uses `tty-color-list'.
On mswindows devices, this uses `mswindows-color-list'."
  (let ((table (color-list)))
    (completing-read prompt table nil (and table must-match)
		     initial-contents)))


(defun read-coding-system (prompt &optional default-coding-system)
  "Read a coding-system (or nil) from the minibuffer.
Prompting with string PROMPT.
If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
DEFAULT-CODING-SYSTEM can be a string, symbol, or coding-system object."
  (intern (completing-read prompt obarray 'find-coding-system t nil nil 
			   (cond ((symbolp default-coding-system)
				  (symbol-name default-coding-system))
				 ((coding-system-p default-coding-system)
				  (symbol-name (coding-system-name default-coding-system)))
				 (t
				  default-coding-system)))))

(defun read-non-nil-coding-system (prompt)
  "Read a non-nil coding-system from the minibuffer.
Prompt with string PROMPT."
  (let ((retval (intern "")))
    (while (eql 0 (length (symbol-name retval)))
      (setq retval (intern (completing-read prompt obarray
					    'find-coding-system
					    t))))
    retval))



(defcustom force-dialog-box-use nil
  "*If non-nil, always use a dialog box for asking questions, if possible.
You should *bind* this, not set it.  This is useful if you're doing
something mousy but which wasn't actually invoked using the mouse."
  :type 'boolean
  :group 'minibuffer)

;; We include this here rather than dialog.el so it is defined
;; even when dialog boxes are not present.
(defun should-use-dialog-box-p ()
  "If non-nil, questions should be asked with a dialog box instead of the
minibuffer.  This looks at `last-command-event' to see if it was a mouse
event, and checks whether dialog-support exists and the current device
supports dialog boxes.

The dialog box is totally disabled if the variable `use-dialog-box'
is set to nil."
  (and (featurep 'dialog)
       (device-on-window-system-p)
       use-dialog-box
       (or force-dialog-box-use
	   (button-press-event-p last-command-event)
	   (button-release-event-p last-command-event)
	   (misc-user-event-p last-command-event))))

(defun get-user-response (position question answers)
  "Ask a question and get a response from the user, in minibuffer or dialog box.
POSITION specifies which frame to use.
This is normally an event or a window or frame.
If POSITION is t or nil, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

QUESTION is the question to ask (it should end with a question mark followed
by a space).

ANSWERS are the possible answers.  It is a list; each item looks like

  (KEY BUTTON-TEXT RESPONSE)

where KEY is the key to be pressed in the minibuffer, BUTTON-TEXT is the
text to be displayed in a dialog box button (you should put %_ in it to
indicate the accelerator), and RESPONSE is a value (typically a symbol)
to be returned if the user selects this response.  KEY should be either a
single character or a string; which one you use needs to be consistent for
all responses and determines whether the user responds by hitting a single
key or typing in a string and hitting ENTER.

An item may also be just a string--that makes a nonselectable item in the
dialog box and is ignored in the minibuffer.

An item may also be nil -- that means to put all preceding items
on the left of the dialog box and all following items on the right; ignored
in the minibuffer."
  (if (should-use-dialog-box-p)
      (get-dialog-box-response
       position
       (cons question
	     (mapcar #'(lambda (x)
			 (cond
			  ((null x) nil)
			  ((stringp x) x)
			  (t (cons (second x) (third x)))))
		     answers)))
    (save-excursion
      (let* ((answers (remove-if-not #'consp answers))
	     (possible
	      (gettext
	       (labels ((car-to-string-if (x)
                          (setq x (car x))
                          (if (stringp x)  x (char-to-string x))))
		 (concat (mapconcat #'car-to-string-if
			   (butlast answers) ", ") " or "
			   (car-to-string-if (car (last answers)))))))
	     (question (gettext question))
	     (p (format "%s(%s) " question possible)))
	(block nil
	  (if (stringp (caar answers))
	      ;; based on yes-or-no-p.
	      (while t
		(let* ((ans (downcase (read-string p nil t))) ;no history
		       (res (member* ans answers :test #'equal :key #'car)))
		  (if res (return (third (car res)))
		    (ding nil 'yes-or-no-p)
		    (discard-input)
		    (message "Please answer %s." possible)
		    (sleep-for 2))))
	    ;; based on y-or-n-p.
	    (save-excursion
	      (let* ((pre "") event)
		(while t
		  (if (let ((cursor-in-echo-area t)
			    (inhibit-quit t))
			(message "%s%s(%s) " pre question possible)
			(setq event (next-command-event event))
			(condition-case nil
			    (prog1
				(or quit-flag (eq 'keyboard-quit
						  (key-binding event)))
			      (setq quit-flag nil))
			  (wrong-type-argument t)))
		      (progn
			(message "%s%s(%s) %s" pre question possible
				 (single-key-description event))
			(setq quit-flag nil)
			(signal 'quit '())))
		  (let* ((keys (events-to-keys (vector event)))
			 (def (lookup-key query-replace-map keys)))
		    (cond
; 		     ((eq def 'skip)
; 		      (message "%s%sNo" question possible)
; 		      (return nil))
; 		     ((eq def 'act)
; 		      (message "%s%sYes" question possible)
; 		      (return t))
		     ((eq def 'recenter)
		      (recenter))
		     ((or (eq def 'quit) (eq def 'exit-prefix))
		      (signal 'quit '()))
		     ((button-release-event-p event) ; ignore them
		      nil)
		     (t
		      (let ((res (member* (event-to-character event) answers
					  :key #'car)))
			(if res (return (third (car res)))
			  (message "%s%s(%s) %s" pre question possible
				   (single-key-description event))
			  (ding nil 'y-or-n-p)
			  (discard-input)
			  (if (eql (length pre) 0)
			      (setq pre (format "Please answer %s.  "
						;; 17 parens!  a record in
						;; our lisp code.
						possible)))))))))))))))))

;;; minibuf.el ends here
