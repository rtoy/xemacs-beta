;;!emacs
;;
;; FILE:         hactypes.el
;; SUMMARY:      Default action types for Hyperbole.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1997  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; ORIG-DATE:    23-Sep-91 at 20:34:36
;; LAST-MOD:     20-Feb-97 at 11:16:36 by Bob Weiner

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(hbut hpath hargs hact hmail))

;;; ************************************************************************
;;; Standard Hyperbole action types
;;; ************************************************************************

(defact annot-bib (key)
  "Follows internal ref KEY within an annotated bibliography, delimiters=[]."
  (interactive "sReference key (no []): ")
  (let ((opoint (point))
	(key-regexp (concat "^[*]*[ \t]*\\\[" (ebut:key-to-label key) "\\\]")))
    (goto-char (point-min))
    (if (re-search-forward key-regexp nil t)
	(progn (hpath:display-buffer (current-buffer))
	       (beginning-of-line))
      (goto-char opoint)
      (beep))))

(defact completion ()
  "Inserts completion at point into the minibuffer or a buffer.
Unless point is at the end of the buffer or if completion has already been
inserted, the completions window is deleted."
  (interactive)
  (if (eobp)
      (progn (bury-buffer nil)
	     (delete-window))
    (hargs:completion)))

(defact eval-elisp (lisp-expr)
  "Evaluates a Lisp expression LISP-EXPR."
  (interactive "xLisp to eval: ")
  (eval lisp-expr))

(defact exec-kbd-macro (kbd-macro &optional repeat-count)
  "Executes KBD-MACRO REPEAT-COUNT times.
KBD-MACRO may be a string of editor command characters, a function symbol or
nil to use the last defined keyboard macro.
Optional REPEAT-COUNT nil means execute once, zero means repeat until
error."
  (interactive
   (let (macro repeat)
     (setq macro (intern-soft
		  (hargs:read-match
		   "Unquoted macro name or nil for last one defined: "
		   obarray (function
			    (lambda (sym)
			      (and (fboundp sym)
				   (stringp (hypb:indirect-function sym)))))
		   nil "nil" 'symbol)))
     (cond ((fboundp macro))
	   ((null last-kbd-macro)
	    (hypb:error
	      "(exec-kbd-macro): Define a keyboard macro first."))
	   (t (fset 'zzk last-kbd-macro)
	      (setq macro 'zzk)))
     (save-excursion
       (let ((standard-output (get-buffer-create "*macro-def*")))
	 (unwind-protect
	     (progn (set-buffer standard-output)
		    (setq buffer-read-only nil)
		    (erase-buffer)
		    (insert-kbd-macro macro)
		    (goto-char (point-min))
		    (setq macro (car (cdr (cdr (read (current-buffer)))))))
	   (kill-buffer standard-output))))
     (fmakunbound 'zzk)
     (setq repeat (hargs:read "Repeat count: "
			     (function
			      (lambda (repeat)
				(or (null repeat)
				    (and (integerp repeat) (>= repeat 0)))))
			     1))
     (list macro repeat)))
  (if (interactive-p)
      nil
    (or (and kbd-macro (or (stringp kbd-macro)
		       (and (symbolp kbd-macro) (fboundp kbd-macro))))
	(hypb:error "(exec-kbd-macro): Bad macro: %s" kbd-macro))
    (or (null repeat-count) (and (integerp repeat-count) (<= 0 repeat-count))
	(hypb:error "(exec-kbd-macro): Bad repeat count: %s" repeat-count)))
  (execute-kbd-macro kbd-macro repeat-count))

;;; Support next two actypes on systems which use the `comint' shell package
;;; rather than Emacs V18 shell.el.
;;;
(if (or hyperb:lemacs-p hyperb:emacs19-p)
    (require 'comint))
(and (fboundp 'comint-send-input) (not (fboundp 'shell-send-input))
     (fset 'shell-send-input 'comint-send-input))
(and (fboundp 'comint-kill-output) (not (fboundp 'kill-output-from-shell))
     (fset 'kill-output-from-shell 'comint-kill-output))
(and (fboundp 'comint-show-output) (not (fboundp 'show-output-from-shell))
     (fset 'show-output-from-shell 'comint-show-output))

(defact exec-shell-cmd (shell-cmd &optional internal-cmd kill-prev)
  "Executes a SHELL-CMD string asynchronously.
Optional non-nil second argument INTERNAL-CMD means do not display the shell
command line executed.  Optional non-nil third argument KILL-PREV means
kill the last output to the shell buffer before executing SHELL-CMD."
  (interactive
   (let ((default  (car defaults))
	 (default1 (nth 1 defaults))
	 (default2 (nth 2 defaults)))
   (list (hargs:read "Shell cmd: "
		     (function
		      (lambda (cmd) (not (string-equal cmd ""))))
		    default "Enter a shell command." 'string)
	 (y-or-n-p (format "Omit cmd from output (default = %s): "
			   default1))
	 (y-or-n-p (format "Kill prior cmd's output (default = %s): "
			   default2)))))
  (let ((buf-name "*Hypb Shell*")
	(owind (selected-window)))
    (unwind-protect
	(progn
	  (if (not (hpath:ange-ftp-p default-directory))
	      (setq shell-cmd
		    (concat "cd " default-directory "; " shell-cmd)))
	  (if (not (get-buffer buf-name))
	      (save-excursion
		(hpath:display-buffer (current-buffer))
		(if (eq (minibuffer-window) (selected-window))
		    (other-window 1))
		(shell) (rename-buffer buf-name)
		(setq last-input-start (point-marker)
		      last-input-end (point-marker))
		(if (fboundp 'comint-kill-output)
		    (setq comint-last-input-start last-input-start
			  comint-last-input-end last-input-end)
		  )))
	  (hpath:display-buffer buf-name)
	  (goto-char (point-max))
	  (and kill-prev last-input-end
	       (not (equal last-input-start last-input-end))
	       (kill-output-from-shell))
	  (insert shell-cmd)
	  (shell-send-input)
	  (show-output-from-shell)
	  (or internal-cmd (scroll-down 1)))
      (select-window owind))))

(defact exec-window-cmd (shell-cmd)
  "Asynchronously executes an external window-based SHELL-CMD string."
  (interactive
   (let ((default  (car defaults)))
     (list (hargs:read "Shell cmd: "
		       (function
			(lambda (cmd) (not (string-equal cmd ""))))
		       default "Enter a shell command." 'string))))
  (let ((buf-name "*Hypb Shell*")
	(cmd (if (hpath:ange-ftp-p default-directory)
		 (concat "(" shell-cmd ") &")
	       (concat "(cd " default-directory "; " shell-cmd ") &")))
	(msg (format "Executing: %s" shell-cmd))
	(shell-buf))
    (message msg)
    (save-excursion
      (save-window-excursion
	(if (not (get-buffer buf-name))
	    (progn (save-excursion
		     (save-window-excursion
		       (shell)
		       (setq shell-buf (current-buffer))))
		   (message msg)
		   ;; Wait for shell to startup before sending it input.
		   (sit-for 1)
		   (set-buffer shell-buf)
		   (rename-buffer buf-name)
		   (setq last-input-start (point-marker)
			 last-input-end (point-marker))
		   (if (fboundp 'comint-kill-output)
		       (setq comint-last-input-start last-input-start
			     comint-last-input-end last-input-end)
		     )))
	(or (equal (buffer-name (current-buffer)) buf-name)
	    (set-buffer buf-name))
	(goto-char (point-max))
	(insert cmd)
	(shell-send-input)))
    (message msg)))

(defact function-in-buffer (name pos)
  "Displays the definition of function NAME found at POS in the current buffer."
  (save-excursion
      (goto-char pos)
      (if (looking-at (regexp-quote name))
	  nil
	(let ((fume-scanning-message nil))
	  (fume-rescan-buffer)
	  (setq pos (cdr-safe (assoc name fume-funclist))))))
  (if pos
      (progn (hpath:display-buffer (current-buffer))
	     (goto-char pos)
	     ;; Move to beginning of the line for compatibility with find-tag.
	     (beginning-of-line))))

(defact hyp-config (&optional out-buf)
  "Inserts Hyperbole configuration info at end of current buffer or optional OUT-BUF."
  (hypb:configuration out-buf))

(defact hyp-request (&optional out-buf)
  "Inserts Hyperbole mail list request help into current buffer or optional OUT-BUF."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    (goto-char (point-max))
    (delete-blank-lines) (delete-blank-lines)
    (insert "Use one of the following formats in the *body* of your message:\n
subscribe <mail-list-name> [<your-email-address>]
  or
unsubscribe <mail-list-name> [<your-email-address>]

where possible <mail-list-names> are:
  hyperbole          - discussion of Hyperbole
  hyperbole-announce - Hyperbole announcements only

For example:  subscribe hyperbole joe@nowhere.gov\n")))

(defact hyp-source (buf-str-or-file)
  "Displays a buffer or file from a line beginning with `hbut:source-prefix'."
  (interactive
   (list (prin1-to-string (get-buffer-create
			   (read-buffer "Buffer to link to: ")))))
  (if (stringp buf-str-or-file)
      (cond ((string-match "\\`#<buffer \"?\\([^ \n\"]+\\)\"?>" buf-str-or-file)
	     (hpath:display-buffer
	      (substring buf-str-or-file (match-beginning 1) (match-end 1))))
	    (t (hpath:find buf-str-or-file)))
    (hypb:error "(hyp-source): Non-string argument: %s" buf-str-or-file)))

(defact link-to-buffer-tmp (buffer)
  "Displays a BUFFER.
Link is generally only good for current Emacs session.
Use `link-to-file' instead for a permanent link."
  (interactive "bBuffer to link to: ")
  (if (or (stringp buffer) (bufferp buffer))
      (hpath:display-buffer buffer)
    (hypb:error "(link-to-buffer-tmp): Not a current buffer: %s" buffer)))

(defact link-to-directory (directory)
  "Displays a DIRECTORY in Dired mode."
  (interactive "DDirectory to link to: ")
  (hpath:find directory))

(defact link-to-ebut (key-file key)
  "Performs action given by another button, specified by KEY-FILE and KEY."
  (interactive
   (let (but-file but-lbl)
     (while (cond ((setq but-file
			 (read-file-name
			  "File of button to link to: " nil nil t))
		   (if (string-equal but-file "")
		       (progn (beep) t)))
		  ((not (file-readable-p but-file))
		   (message "(link-to-ebut): You cannot read `%s'."
			    but-file)
		   (beep) (sit-for 3))))
     (list but-file
	   (progn
	     (find-file-noselect but-file)
	     (while (string-equal "" (setq but-lbl
				      (hargs:read-match
				       "Button to link to: "
				       (ebut:alist but-file)
				       nil nil nil 'ebut)))
	       (beep))
	     (ebut:label-to-key but-lbl)))))
  (or (interactive-p)
      (setq key-file (hpath:validate (hpath:substitute-value key-file))))
  (let ((but (ebut:get key (find-file-noselect key-file))))
    (if but (hbut:act but)
      (hypb:error "(link-to-ebut): No button `%s' in `%s'." (ebut:key-to-label key)
	     key-file))))

(defact link-to-elisp-doc (func-symbol)
  "Displays documentation for FUNC-SYMBOL."
  (interactive "aFunction to display doc for: ")
  (cond ((not (symbolp func-symbol))
	 (hypb:error "(link-to-elisp-doc): `%s' not a symbol."
		func-symbol))
	((not (fboundp func-symbol))
	 (hypb:error "(link-to-elisp-doc): `%s' not defined as a function."
		func-symbol))
	((not (documentation func-symbol))
	 (hypb:error "(link-to-elisp-doc): `%s' has no documentation."
		func-symbol))
	(t (let ((temp-buffer-show-function 'switch-to-buffer))
	     (hpath:display-buffer (current-buffer))
	     (describe-function func-symbol)))))

(defact link-to-file (path &optional point)
  "Displays file given by PATH scrolled to optional POINT.
With POINT, buffer is displayed with POINT at window top."
  (interactive
   (let ((prev-reading-p hargs:reading-p))
     (unwind-protect
	 (let* ((default (car defaults))
		(hargs:reading-p 'file)
		(path (read-file-name "Path to link to: " default default))
		(path-buf (get-file-buffer path)))
	   (if path-buf
	       (save-excursion
		 (set-buffer path-buf)
		 (setq hargs:reading-p 'character)
		 (if (y-or-n-p
		      (format "y = Display at present position (line %d); n = no position: "
			      (count-lines 1 (point))))
		     (list path (point))
		   (list path)))
	     (list path)))
       (setq hargs:reading-p prev-reading-p))))
  (and (hpath:find path)
       (integerp point)
       (progn (goto-char (min (point-max) point))
	      (recenter 0))))

(defact link-to-file-line (path line-num)
  "Displays a file given by PATH scrolled to LINE-NUM."
  (interactive "fPath to link to: \nnDisplay at line number: ")
  (if (setq path (smart-tags-file-path path))
       (hpath:find-line path line-num)))

(defact link-to-Info-node (node)
  "Displays an Info NODE.
NODE must be a string of the form `(file)nodename'."
  (interactive "+IInfo (file)nodename to link to: ")
  (require 'info)
  (if (and (stringp node) (string-match "^(\\([^\)]+\\))\\(.*\\)" node))
      (let ((nodename (substring node (match-beginning 2) (match-end 2)))
	    (file (hpath:absolute-to
		   (substring node (match-beginning 1) (match-end 1))
		   (if (boundp 'Info-directory-list)
		       Info-directory-list
		     Info-directory))))
	(if (and file (setq file (hpath:substitute-value file)))
	    (let ((wind (get-buffer-window "*info*")))
	      (if wind (select-window wind)
		(hpath:display-buffer (other-buffer)))
	      (info) (Info-goto-node (concat "(" file ")" nodename)))
	  (hypb:error "(link-to-Info-node): Bad node spec: `%s'" node)))))

(defact link-to-kcell (file cell-ref)
  "Displays FILE with kcell given by CELL-REF at window top.
See documentation for `kcell:ref-to-id' for valid cell-ref formats.

If FILE is nil, the current buffer is used.
If CELL-REF is nil, the first cell in the view is shown."
  (interactive "fKotl file to link to: \n+KKcell to link to: ")
  (require 'kfile)
  (cond ((and (stringp cell-ref) (> (length cell-ref) 0)
	      (= ?| (aref cell-ref 0)))
	 ;; Activate view spec in current window.
	 (kotl-mode:goto-cell cell-ref))
	((if file
	     (hpath:find file)
	   (hpath:display-buffer (current-buffer)))
	 (if cell-ref
	     (kotl-mode:goto-cell cell-ref)
	   (kotl-mode:beginning-of-buffer))
	 (recenter 0))))

(defact link-to-mail (mail-msg-id &optional mail-file)
  "Displays mail msg with MAIL-MSG-ID from optional MAIL-FILE.
See documentation for the variable `hmail:init-function' for information on
how to specify a mail reader to use."
  (interactive "+MMail Msg: ")
  (if (not (fboundp 'rmail:msg-to-p))
      (hypb:error "(link-to-mail): Invoke mail reader before trying to follow a mail link.")
    (if (and (listp mail-msg-id) (null mail-file))
	(setq mail-file (car (cdr mail-msg-id))
	      mail-msg-id (car mail-msg-id)))
    (let ((wconfig (current-window-configuration)))
      (hpath:display-buffer (current-buffer))
      ;; Initialize user-specified mail reader if need be.
      (if (and (symbolp hmail:init-function)
	       (fboundp hmail:init-function)
	       (listp (symbol-function hmail:init-function))
	       (eq 'autoload (car (symbol-function hmail:init-function))))
	  (funcall hmail:init-function))
      (if (rmail:msg-to-p mail-msg-id mail-file)
	  nil
	;; Couldn't find message, restore old window config, report error
	(set-window-configuration wconfig)
	(hypb:error "(link-to-mail): No msg `%s' in file \"%s\"."
		    mail-msg-id mail-file)))))

(defact link-to-regexp-match (regexp n source &optional buffer-p)
  "Finds REGEXP's Nth occurrence in SOURCE and displays location at window top.
SOURCE is a pathname unless optional BUFFER-P is non-nil, then SOURCE must be
a buffer name or buffer.
Returns t if found, signals an error if not."
  (interactive "sRegexp to match: \nnOccurrence number: \nfFile to search: ")
  (let ((orig-src source))
    (if buffer-p
	(if (stringp source)
	    (setq source (get-buffer source)))
      ;; Source is a pathname.
      (if (not (stringp source))
	  (hypb:error
	   "(link-to-regexp-match): Source parameter is not a filename: `%s'"
	   orig-src)
	(setq source (find-file-noselect (hpath:substitute-value source)))))
    (if (not (bufferp source))
	(hypb:error
	 "(link-to-regexp-match): Invalid source parameter: `%s'" orig-src)
      (hpath:display-buffer source)
      (widen)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t n)
	  (progn (beginning-of-line) (recenter 0) t)
	(hypb:error
	 "(link-to-regexp-match): Pattern not found: `%s'" regexp)))))

(defact link-to-rfc (rfc-num)
  "Retrieves and displays an Internet rfc given by RFC-NUM.
RFC-NUM may be a string or an integer.  Requires ange-ftp or efs for
remote retrievals."
  (interactive "nRFC number to retrieve: ")
  (if (or (stringp rfc-num) (integerp rfc-num))
      (hpath:find (hpath:rfc rfc-num))))

(defact link-to-string-match (string n source &optional buffer-p)
  "Finds STRING's Nth occurrence in SOURCE and displays location at window top.
SOURCE is a pathname unless optional BUFFER-P is non-nil, then SOURCE must be
a buffer name or buffer.
Returns t if found, nil if not."
  (interactive "sString to match: \nnOccurrence number: \nfFile to search: ")
  (funcall (actype:action 'link-to-regexp-match)
	   (regexp-quote string) n source buffer-p))

(defact man-show (topic)
  "Displays man page on TOPIC, which may be of the form <command>(<section>).
If using the Superman manual entry package, see the documentation for
`sm-notify' to control where the man page is displayed."
  (interactive "sManual topic: ")
  (let ((display-buffer-function
	 (function (lambda (buffer &rest unused) (hpath:display-buffer buffer)))))
    (manual-entry topic)))

(defact rfc-toc (&optional buf-name opoint)
  "Computes and displays summary of an Internet rfc in BUF-NAME.
Assumes point has already been moved to start of region to summarize.
Optional OPOINT is point to return to in BUF-NAME after displaying summary."
  (interactive)
  (if buf-name
      (cond ((get-buffer buf-name)
	     (switch-to-buffer buf-name))
	    ((let ((buf (get-file-buffer buf-name)))
	       (if buf
		   (progn (switch-to-buffer (setq buf-name buf))
			  t))))
	    (t (if opoint (goto-char opoint))
	       (hypb:error "(rfc-toc): Invalid buffer name: %s" buf-name))))
  (let ((sect-regexp "^[ \t]*[1-9][0-9]*\\.[0-9.]*[ \t]+[^ \t\n]")
	(temp-buffer-show-function 'switch-to-buffer))
    (hpath:display-buffer (current-buffer))
    (occur sect-regexp)
    (set-buffer "*Occur*")
    (rename-buffer (format "*%s toc*" buf-name))
    (re-search-forward "^[ ]*[0-9]+:" nil t)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (insert "Contents of " (buffer-name occur-buffer) ":\n")
    (set-buffer-modified-p nil)
    (set-buffer buf-name)
    (if opoint (goto-char opoint))))

(defact text-toc (section)
  "Jumps to the text file SECTION referenced by a table of contents entry at point."
  (interactive "sGo to section named: ")
  (if (stringp section)
      (progn
	(actypes::link-to-regexp-match
	      (concat "^\\*+[ \t]*" (regexp-quote section))
	      1 (current-buffer) t)
	(while (and (= (forward-line -1) 0)
		    (looking-at "[ \t]*[-=][-=]")))
	(forward-line 1)
	(recenter 0))))

(provide 'hactypes)
