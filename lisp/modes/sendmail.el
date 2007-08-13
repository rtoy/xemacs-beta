;;; sendmail.el --- mail sending commands for Emacs.

;; Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; This mode provides mail-sending facilities from within Emacs.  It is
;; documented in the Emacs user's manual.

;;; Code:

;;;###autoload
(defvar mail-from-style 'angles "\
*Specifies how \"From:\" fields look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>")

;;;###autoload
(defvar mail-self-blind nil "\
Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

;;;###autoload
(defvar mail-interactive nil "\
Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

;;;###autoload
(defvar mail-dir nil "*Default directory for saving messages.")

;;; XEmacs change: moved rmail-ignored-headers here from rmail.el so that
;;; the value of mail-yank-ignored-headers can default from it.  Both of
;;; these end up in loaddefs.el, but "sendmail" comes before "rmail", so...
;;;
;;;###autoload
(defvar rmail-ignored-headers 
  (purecopy
   (concat
    "^\\("
    (mapconcat
     'identity
     '(;; RFC822
       "Sender:" "References:" "Return-Path:" "Received:"
       "[^: \t\n]*Message-ID:" "Errors-To:"
       ;; RFC977 (NNTP)
       "Path:" "Expires:" "Xref:" "Lines:" "Approved:" "Distribution:"
       ;; SYSV mail:
       "Content-Length:"
       ;; MIME:
       "Mime-Version:" "Content-Type:" "Content-Transfer-Encoding:"
       ;; X400
       "X400-Received:" "X400-Originator:" "X400-Mts-Identifier:"
       "X400-Content-Type:" "Content-Identifier:"
       ;; RMAIL and /usr/ucb/mail:
       "Status:" "Summary-Line:"
       ;; Supercite:
       "X-Attribution:"
       ;; Other random junk occasionally seen:
       "Via:" "Sent-Via:" "Mail-From:" "Origin:" "Comments:" "Originator:"
       "NF-ID:" "NF-From:" "Posting-Version:" "Posted:" "Posted-Date:"
       "Date-Received:" "Relay-Version:" "Article-I\\.D\\.:" "NNTP-Version:"
       "NNTP-Posting-Host:" "X-Mailer:" "X-Newsreader:" "News-Software:"
       "X-Received:" "X-References:" "X-Envelope-To:"
       "X-VMS-" "Remailed-" "X-Plantation:" "X-Windows:" "X-Pgp-"
       )
     "\\|")
    "\\)"))
  "*Gubbish header fields one would rather not see.")


;;;###autoload
(defvar mail-yank-ignored-headers
  (purecopy
   (concat rmail-ignored-headers "\\|"
	   "^\\("
	   (mapconcat 'identity
		      '(;; RFC822
			"Resent-To:" "Resent-By:" "Resent-CC:"
			"To:" "Subject:" "In-Reply-To:"
			)
		      "\\|")
	   "\\)"))
  "Delete these headers from old message when it's inserted in a reply.")
;; minimalist FSF version
;(defvar mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:" "\
;Delete these headers from old message when it's inserted in a reply.")

;; Useful to set in site-init.el
;;;###autoload
(defvar send-mail-function 'sendmail-send-it "\
Function to call to send the current buffer as mail.
The headers should be delimited by a line whose contents
match the variable `mail-header-separator'.")

;;;###autoload
(defvar mail-header-separator (purecopy "--text follows this line--") "\
*Line used to separate headers from text in messages being composed.")

;;;###autoload
(defvar mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
This can be an inbox file or an Rmail file.")

;;;###autoload
(defvar mail-default-reply-to nil
  "*Address to insert as default Reply-to field of outgoing messages.
If nil, it will be initialized from the REPLYTO environment variable
when you first send mail.")

;;;###autoload
(defvar mail-alias-file nil
  "*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.")

;(defvar mail-personal-alias-file "~/.mailrc"
;  "*If non-nil, the name of the user's personal mail alias file.
;This file typically should be in same format as the `.mailrc' file used by
;the `Mail' or `mailx' program.
;This file need not actually exist.")
(defvaralias 'mail-personal-alias-file 'mail-abbrev-mailrc-file)

(defvar mail-setup-hook nil
  "Normal hook, run each time a new outgoing mail message is initialized.
The function `mail-setup' runs this hook.")

; These are removed.  See `mail-abbrevs.el'.

;(defvar mail-aliases t
;  "Alist of mail address aliases,
;or t meaning should be initialized from your mail aliases file.
;\(The file's name is normally `~/.mailrc', but your MAILRC environment
;variable can override that name.)
;The alias definitions in the file have this form:
;    alias ALIAS MEANING")
;
;(defvar mail-alias-modtime nil
;  "The modification time of your mail alias file when it was last examined.")

;;;###autoload
(defvar mail-yank-prefix "> "		; XEmacs change
  "*Prefix insert on lines of yanked message being replied to.
nil means use indentation.")

(defvar mail-indentation-spaces 3
  "*Number of spaces to insert at the beginning of each cited line.
Used by `mail-yank-original' via `mail-indent-citation'.")

(defvar mail-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and mail agents should no longer use it.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), a default action is taken
instead of no action.")

(defvar mail-abbrevs-loaded nil)
(defvar mail-mode-map nil)

; Removed autoloads of `build-mail-aliases' and `expand-mail-aliases'.
; See `mail-abbrevs.el'.

(autoload 'mail-aliases-setup "mail-abbrevs")

;;;###autoload
(defvar mail-signature nil
  "*Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `mail-signature-file'.")

(defvar mail-signature-file "~/.signature"
  "*File containing the text inserted at end of mail buffer.")

(defvar mail-reply-buffer nil)
(defvar mail-send-actions nil
  "A list of actions to be performed upon successful sending of a message.")

(defvar mail-default-headers nil
  "*A string containing header lines, to be inserted in outgoing messages.
It is inserted before you edit the message,
so you can edit or delete these lines.")

(defvar mail-bury-selects-summary t
  "*If non-nil, try to show RMAIL summary buffer after returning from mail.
The functions \\[mail-send-on-exit] or \\[mail-dont-send] select
the RMAIL summary buffer before returning, if it exists and this variable
is non-nil.")

;; Note: could use /usr/ucb/mail instead of sendmail;
;; options -t, and -v if not interactive.
(defvar mail-mailer-swallows-blank-line
  (if (and (string-match "sparc-sun-sunos\\(\\'\\|[^5]\\)" system-configuration)
	   (file-readable-p "/etc/sendmail.cf")
	   (let ((buffer (get-buffer-create " *temp*")))
	     (unwind-protect
		 (save-excursion
		   (set-buffer buffer)
		   (insert-file-contents "/etc/sendmail.cf")
		   (goto-char (point-min))
		   (let ((case-fold-search nil))
		     (re-search-forward "^OR\\>" nil t)))
	       (kill-buffer buffer))))
      ;; According to RFC822, "The field-name must be composed of printable
      ;; ASCII characters (i.e. characters that have decimal values between
      ;; 33 and 126, except colon)", i.e. any chars except ctl chars,
      ;; space, or colon.
      '(looking-at "[ \t]\\|[][!\"#$%&'()*+,-./0-9;<=>?@A-Z\\\\^_`a-z{|}~]+:"))
  "Set this non-nil if the system's mailer runs the header and body together.
\(This problem exists on Sunos 4 when sendmail is run in remote mode.)
The value should be an expression to test whether the problem will
actually occur.")

(defvar mail-use-multiple-buffers-p t
  "Non-nil means `mail' will create a new buffer if one already exists.")

(defvar mail-mode-syntax-table nil
  "Syntax table used while in mail mode.")

(if (not mail-mode-syntax-table)
    (progn
     (setq mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
     (modify-syntax-entry ?% ". " mail-mode-syntax-table)))

(defvar mail-font-lock-keywords
  (let* ((cite-prefix "A-Za-z") (cite-suffix (concat cite-prefix "0-9_.@-")))
    (list '("^To:" . font-lock-function-name-face)
	  '("^B?CC:\\|^Reply-To:" . font-lock-keyword-face)
	  '("^\\(Subject:\\)[ \t]*\\(.+\\)?"
	    (1 font-lock-comment-face) (2 font-lock-type-face nil t))
	  (list (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
		1 'font-lock-comment-face)
	  (cons (concat "^[ \t]*"
			"\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
			"[>|}].*")
		'font-lock-reference-face)
	  '("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\):.*"
	    . font-lock-string-face)))
  "Additional expressions to highlight in Mail mode.")
(put 'mail-mode 'font-lock-defaults '(mail-font-lock-keywords t))

(defvar mail-send-hook nil
  "Normal hook run before sending mail, in Mail mode.")

; Removed.  See above and `mail-abbrevs.el'.
;(defun sendmail-synch-aliases ()
;  (let ((modtime (nth 5 (file-attributes mail-personal-alias-file))))
;    (or (equal mail-alias-modtime modtime)
;	(setq mail-alias-modtime modtime
;	      mail-aliases t))))

(defun mail-setup (to subject in-reply-to cc replybuffer actions)
  (or mail-default-reply-to
      (setq mail-default-reply-to (getenv "REPLYTO")))
;Removed.  See `mail-abbrevs.el'.
;  (sendmail-synch-aliases)
;  (if (eq mail-aliases t)
;      (progn
;	(setq mail-aliases nil)
;	(if (file-exists-p mail-personal-alias-file)
;	    (build-mail-aliases))))
  (setq mail-send-actions actions)
  (mail-aliases-setup)
  (setq mail-reply-buffer replybuffer)
  (goto-char (point-min))
  (insert "To: ")
  (save-excursion
    (if to
        ;; Here removed code to extract names from within <...>
        ;; on the assumption that mail-strip-quoted-names
        ;; has been called and has done so.
	(let ((fill-prefix "\t")
              (address-start (point)))
	  (insert to "\n")
          (fill-region-as-paragraph address-start (point-max)))
      (newline))
    (if cc
	(let ((fill-prefix "\t")
	      (address-start (progn (insert "CC: ") (point))))
	  (insert cc "\n")
	  (fill-region-as-paragraph address-start (point-max))))
    (if in-reply-to
        (let ((fill-prefix "\t")
	      (fill-column 78)
	      (address-start (point)))
	  (insert "In-reply-to: " in-reply-to "\n")
	  (fill-region-as-paragraph address-start (point-max))))
    (insert "Subject: " (or subject "") "\n")
    (if mail-default-headers
	(insert mail-default-headers))
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "BCC: " (user-login-name) "\n"))
    (if mail-archive-file-name
	(insert "FCC: " mail-archive-file-name "\n"))
    (insert mail-header-separator "\n")

    ;; Insert the signature.  But remember the beginning of the message.
    (if to (setq to (point)))
    (cond ((eq mail-signature t)
	   (if (file-exists-p mail-signature-file)
	       (progn
		 (insert "\n\n-- \n")
		 (insert-file-contents mail-signature-file))))
	  (mail-signature
	   (insert mail-signature)))
    (goto-char (point-max))
    (or (bolp) (newline)))
  (if to (goto-char to))
  (or to subject in-reply-to
      (set-buffer-modified-p nil))
  (run-hooks 'mail-setup-hook))

;;;###autoload
(defun mail-mode ()
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
	 C-c C-f C-f  move to FCC:	C-c C-f C-r  move to Reply-To:
C-c C-t  mail-text (move to beginning of message text).
C-c C-w  mail-signature (insert `mail-signature-file' file).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked).
C-c C-v  mail-sent-via (add a sent-via field for each To or CC)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (make-local-variable 'mail-send-actions)
  (set-syntax-table mail-mode-syntax-table)
  (use-local-map mail-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'mail-mode)
  (setq mode-name "Mail")
  (setq buffer-offer-save t)
  (turn-on-auto-fill)	; XEmacs - maybe filladapt should be default, too.
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat (regexp-quote mail-header-separator)
				"$\\|[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat (regexp-quote mail-header-separator)
				   "$\\|[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  ;; Set menu
  (setq mode-popup-menu mail-popup-menu)
  (if (featurep 'menubar)
      (progn
	;; make a local copy of the menubar, so our modes don't
	;; change the global menubar
	(set-buffer-menubar current-menubar)
	(add-submenu nil mail-menubar-menu)))

  (run-hooks 'text-mode-hook 'mail-mode-hook))


;;; Set up keymap.

(if mail-mode-map
    nil
  (setq mail-mode-map (make-sparse-keymap))
  (set-keymap-parents mail-mode-map (list text-mode-map))
  (set-keymap-name mail-mode-map 'mail-mode-map)
  (define-key mail-mode-map "\C-c?" 'describe-mode)
  (define-key mail-mode-map "\C-c\C-f\C-t" 'mail-to)
  (define-key mail-mode-map "\C-c\C-f\C-b" 'mail-bcc)
  (define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)
  (define-key mail-mode-map "\C-c\C-f\C-c" 'mail-cc)
  (define-key mail-mode-map "\C-c\C-f\C-s" 'mail-subject)
  (define-key mail-mode-map "\C-c\C-f\C-r" 'mail-reply-to)
  (define-key mail-mode-map "\C-c\C-t" 'mail-text)
  (define-key mail-mode-map "\C-c\C-y" 'mail-yank-original)
  (define-key mail-mode-map "\C-c\C-q" 'mail-fill-yanked-message)
  (define-key mail-mode-map "\C-c\C-w" 'mail-signature)
  ;;CRAP!!(define-key mail-mode-map "\C-c\C-v" 'mail-sent-via)CRAP!
  (define-key mail-mode-map "\C-c\C-c" 'mail-send-and-exit)
  (define-key mail-mode-map "\C-c\C-s" 'mail-send))

;;; mail-mode popup menu

(defvar mail-menubar-menu
  (purecopy
   '("Mail"
     "Sending Mail:"
     "----"
     ["Send and Exit"		mail-send-and-exit		t]
     ["Send Mail"		mail-send			t]
     "----"
     "Go to Field:"
     "----"
     ["To:"			mail-to				t]
     ["Subject:"		mail-subject			t]
     ["CC:"			mail-cc				t]
     ["BCC:"			mail-bcc			t]
     ["Reply-To:"		mail-reply-to			t]
     ;;    ["Sent Via:"		mail-sent-via			t]
     ["Text"			mail-text			t]
     "----"
     "Miscellaneous Commands:"
     "----"
     ["Yank Original"		mail-yank-original
      (not (null mail-reply-buffer))]
     ["Fill Yanked Message"	mail-fill-yanked-message
      (save-excursion
	(goto-char (point-min))
	(and (search-forward (concat "\n" mail-header-separator
				     "\n") nil t)
	     (not (looking-at "[ \t\n]*\\'"))))]
     ["Insert Signature"	mail-signature
      (and (stringp mail-signature-file)
	   (file-exists-p mail-signature-file))]
     ["Insert File..."		insert-file			t]
     ["Insert Buffer..."	insert-buffer			t]
     "----"
     ["Cancel"			mail-dont-send			t]
     ))
   "Menubar menu for `mail-mode'.")

(defvar mail-popup-menu
  (purecopy
   (cons "Sendmail Commands"
	 (cdr mail-menubar-menu)))
  "Menubar menu for `mail-mode'.")


(defun mail-send-and-exit (arg)
  "Send message like `mail-send', then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-send)
  (mail-bury arg))

(defun mail-dont-send (arg)
  "Don't send the message you have been editing.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-bury arg))

(defun mail-bury (arg)
  "Bury this mail buffer."
  (let ((newbuf (other-buffer (current-buffer))))
    (bury-buffer (current-buffer))
    (if (and (fboundp 'frame-parameters)
	     (cdr (assq 'dedicated (frame-parameters)))
	     (not (null (delq (selected-frame) (visible-frame-list)))))
	(delete-frame (selected-frame))
      (let (rmail-flag summary-buffer)
	(and (not arg)
	     (not (one-window-p))
	     (save-excursion
	       (set-buffer (window-buffer (next-window (selected-window) 'not)))
	       (setq rmail-flag (eq major-mode 'rmail-mode))
	       (setq summary-buffer
		     (and mail-bury-selects-summary
			  (boundp 'rmail-summary-buffer)
			  rmail-summary-buffer
			  (buffer-name rmail-summary-buffer)
			  (not (get-buffer-window rmail-summary-buffer))
			  rmail-summary-buffer))))
	(if rmail-flag
	    ;; If the Rmail buffer has a summary, show that.
	    (if summary-buffer (switch-to-buffer summary-buffer)
	      (delete-window))
	  (switch-to-buffer newbuf))))))

(defun mail-send ()
  "Send the message in the current buffer.
If `mail-interactive' is non-nil, wait for success indication or error
messages, and inform user.  Otherwise any failure is reported in a message
back to the user from the mailer."
  (interactive)
  (if (if buffer-file-name
        (y-or-n-p "Send buffer contents as mail message? ")
      (or (buffer-modified-p)
          (y-or-n-p "Message already sent; resend? ")))
      (progn
	(expand-abbrev)			; for mail-abbrevs
	(run-hooks 'mail-send-hook)
	(message "Sending...")
	(funcall send-mail-function)
	;; Now perform actions on successful sending.
	(while mail-send-actions
	  (condition-case nil
	      (apply (car (car mail-send-actions))
		     (cdr (car mail-send-actions)))
	    (error))
	  (setq mail-send-actions (cdr mail-send-actions)))
	(message "Sending...done")

        ;; If buffer has no file, mark it as unmodified and delete autosave.
        (cond ((or (not buffer-file-name)
                   (not (buffer-modified-p)))
               (set-buffer-modified-p nil)
               (delete-auto-save-file-if-necessary t))
              ((or noninteractive
                   (y-or-n-p (format "Save file %s? " buffer-file-name)))
               (save-buffer))))))

(defun sendmail-send-it ()
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	resend-to-addresses
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
;Removed.  See `mail-abbrevs.el'.
;	  (sendmail-synch-aliases)
;	  (if mail-aliases
;	      (expand-mail-aliases (point-min) delimline))
;	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (end-of-line)
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;;; Apparently this causes a duplicate Sender.
;;;	    ;; If the From is different than current user, insert Sender.
;;;	    (goto-char (point-min))
;;;	    (and (re-search-forward "^From:"  delimline t)
;;;		 (progn
;;;		   (require 'mail-utils)
;;;		   (not (string-equal
;;;			 (mail-strip-quoted-names
;;;			  (save-restriction
;;;			    (narrow-to-region (point-min) delimline)
;;;			    (mail-fetch-field "From")))
;;;			 (user-login-name))))
;;;		 (progn
;;;		   (forward-line 1)
;;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match ""))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login user-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward 
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (let ((default-directory "/"))
	    (apply 'call-process-region
		   (append (list (point-min) (point-max)
				 (if (boundp 'sendmail-program)
				     sendmail-program
				   "/usr/lib/sendmail")
				 nil errbuf nil "-oi")
			   ;; Always specify who from,
			   ;; since some systems have broken sendmails.
			   (list "-f" (user-login-name))
;;;			 ;; Don't say "from root" if running under su.
;;;			   (and (equal (user-real-login-name) "root")
;;;				(list "-f" (user-login-name)))
			   (and mail-alias-file
				(list (concat "-oA" mail-alias-file)))
			   ;; These mean "report errors by mail"
			   ;; and "deliver in background".
			   (if (null mail-interactive) '("-oem" "-odb"))
			   ;; Get the addresses from the message
			   ;; unless this is a resend.
			   ;; We must not do that for a resend
			   ;; because we would find the original addresses.
			   ;; For a resend, include the specific addresses.
			   (or resend-to-addresses
			       '("-t")))))
	  (if mail-interactive
	      (save-excursion
		(set-buffer errbuf)
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

;;; FCC hackery, by jwz.  This version works on BABYL and VM buffers.
;;; To accomplish the latter, VM is loaded when this file is compiled.
;;; Don't worry, it's only loaded at compile-time.

(defun mail-do-fcc (header-end)
  (let (fcc-list
	(send-mail-buffer (current-buffer))
	(tembuf (generate-new-buffer " rmail output"))
	(case-fold-search t)
	beg end)
    (or (markerp header-end) (error "header-end must be a marker"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^FCC:[ \t]*" header-end t)
	(setq fcc-list (cons (buffer-substring (point)
					       (progn
						 (end-of-line)
						 (skip-chars-backward " \t")
						 (point)))
			     fcc-list))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point))))
      (set-buffer tembuf)
      (erase-buffer)
      ;; insert just the headers to avoid moving the gap more than
      ;; necessary (the message body could be arbitrarily huge.)
      (insert-buffer-substring send-mail-buffer 1 header-end)

      ;; if there's no From: or Date: field, cons some.
      (goto-char (point-min))
      (or (re-search-forward "^From[ \t]*:" header-end t)
	  (insert "From: " (user-login-name) " (" (user-full-name) ")\n"))
      (goto-char (point-min))
      (or (re-search-forward "^Date[ \t]*:" header-end t)
	  (mail-do-fcc-insert-date-header))

      ;; insert a magic From_ line.
      (goto-char (point-min))
      (insert "\nFrom " (user-login-name) " " (current-time-string) "\n")
      (goto-char (point-max))
      (insert-buffer-substring send-mail-buffer header-end)
      (goto-char (point-max))
      (insert ?\n)
      (goto-char (1- header-end))

      ;; ``Quote'' "^From " as ">From "
      ;;  (note that this isn't really quoting, as there is no requirement
      ;;   that "^[>]+From " be quoted in the same transparent way.)
      (let ((case-fold-search nil))
	(while (search-forward "\nFrom " nil t)
	  (forward-char -5)
	  (insert ?>)))

      (setq beg (point-min)
	    end (point-max))
      (while fcc-list
	(let ((target-buffer (get-file-buffer (car fcc-list))))
	  (if target-buffer
	      ;; File is present in a buffer => append to that buffer.
	      (save-excursion
		(set-buffer target-buffer)
		(cond ((eq major-mode 'rmail-mode)
		       (mail-do-fcc-rmail-internal tembuf))
		      ((eq major-mode 'vm-mode)
		       (mail-do-fcc-vm-internal tembuf))
		      (t
		       ;; Append to an ordinary buffer as a Unix mail message.
		       (goto-char (point-max))
		       (insert-buffer-substring tembuf beg end))))
	    ;; Else append to the file directly.
	    ;; (It's OK if it is an RMAIL or VM file -- the message will be
	    ;; parsed when the file is read in.)
	    (write-region
	     (1+ (point-min)) (point-max) (car fcc-list) t)))
	(setq fcc-list (cdr fcc-list))))
    (kill-buffer tembuf)))

(defvar mail-do-fcc-cached-timezone nil)

(defun mail-do-fcc-insert-date-header ()
  ;; Convert the ctime() format that `current-time-string' returns into
  ;; an RFC-822-legal date.  
  (let ((s (current-time-string)))
    (string-match "\\`\\([A-Z][a-z][a-z]\\) +\\([A-Z][a-z][a-z]\\) +\\([0-9][0-9]?\\) *\\([0-9][0-9]?:[0-9][0-9]:[0-9][0-9]\\) *[0-9]?[0-9]?\\([0-9][0-9]\\)"
		  s)
    (insert "Date: "
	    (substring s (match-beginning 1) (match-end 1)) ", "
	    (substring s (match-beginning 3) (match-end 3)) " "
	    (substring s (match-beginning 2) (match-end 2)) " "
	    (substring s (match-beginning 5) (match-end 5)) " "
	    (substring s (match-beginning 4) (match-end 4)) " ")

    (if mail-do-fcc-cached-timezone
	(insert mail-do-fcc-cached-timezone "\n")
      ;;
      ;; First, try to use the current-time-zone function, which may not be
      ;; defined, and even if it is defined, may error or return nil.
      ;;
      (or (condition-case ()
	      (let ((zoneinfo (current-time-zone)))
		(setq mail-do-fcc-cached-timezone
		      (if (stringp (nth 1 zoneinfo))
			  (nth 1 zoneinfo)
			(or (if (nth 1 zoneinfo) (nth 3 zoneinfo))
			    (nth 2 zoneinfo))))
		(if mail-do-fcc-cached-timezone
		    (insert mail-do-fcc-cached-timezone "\n"))
		mail-do-fcc-cached-timezone)
	    (error nil))
	  ;;
	  ;; Otherwise, run date(1) and parse its output.  Yuck!
	  ;;
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (call-process "date" nil t nil)
	    (end-of-line)
	    (insert "\n")
	    (forward-word -1)		; skip back over year
	    (delete-region (1- (point)) (1- (point-max))) ; nuke year to end
	    (forward-word -1)		; skip back over zone
	    (delete-region (point-min) (point)) ; nuke beginning to zone
	    (setq mail-do-fcc-cached-timezone
		  (buffer-substring (point-min) (1- (point-max)))))))))

(defun mail-do-fcc-rmail-internal (buffer)
  (or (eq major-mode 'rmail-mode) (error "this only works in rmail-mode"))
  (let ((b (point-min))
	(e (point-max))
	(buffer-read-only nil))
    (unwind-protect
	(progn
	  (widen)
	  (goto-char (point-max))
	  ;; This forces RMAIL's message counters to be recomputed when the
	  ;; next RMAIL operation is done on the buffer.
	  ;; See rmail-maybe-set-message-counters.
	  (setq rmail-total-messages nil)
	  (insert "\^L\n0, unseen,,\n*** EOOH ***")
	  (insert-buffer-substring buffer)
	  (insert "\n\C-_"))
      (narrow-to-region b e)
      (rmail-maybe-set-message-counters))))

;;; Load VM into the compilation environment but not the load environment.
(eval-when-compile
 (or (and (boundp 'loading-vm-kludge) loading-vm-kludge)
     ;; nastiness to avoid circular provide/require dependency nonsense
     (fboundp 'vm-spool-files)
     (let ((loading-vm-kludge t))
       (require 'vm))))

(defun mail-do-fcc-vm-internal (buffer)
  (or (eq major-mode 'vm-mode) (error "this only works in vm-mode"))
  (let ((buffer-read-only nil)
	(foreign-folder-p (not (eq vm-folder-type 'From_))))

    (if foreign-folder-p
	;; `buffer' has already been prepared with a "From " line which
	;; has a sensible user-id and date in it, but if we're FCCing to
	;; a VM folder that isn't in From_ format, we must discard that
	;; and let VM do whatever voodoo it needs to do.  (Actually we
	;; could do this all the time, but then all FCCed messages would
	;; have "From VM ..." envelopes, which is less attractive.)
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (skip-chars-forward "\n")
	  (forward-line)
	  (delete-region (point-min) (point))))

    ;; Largely copied from #'vm-save-message in vm-save.el
    (vm-save-restriction
     (widen)
     (goto-char (point-max))
     (if foreign-folder-p
	 (vm-write-string (current-buffer)
			  (vm-leading-message-separator vm-folder-type)))
     (insert-buffer-substring buffer)
     (if foreign-folder-p
	 (vm-write-string (current-buffer)
			  (vm-trailing-message-separator vm-folder-type)))

     (vm-increment vm-messages-not-on-disk)
     (vm-set-buffer-modified-p t)
     (vm-clear-modification-flag-undos)
     (vm-check-for-killed-summary)
     (vm-assimilate-new-messages)
     (vm-update-summary-and-mode-line))))

;;(defun mail-sent-via ()
;;  "Make a Sent-via header line from each To or CC header line."
;;  (interactive)
;;  (save-excursion
;;  (goto-char (point-min))
;;  ;; find the header-separator
;;  (search-forward (concat "\n" mail-header-separator "\n"))
;;  (forward-line -1)
;;  ;; put a marker at the end of the header
;;  (let ((end (point-marker))
;;        (case-fold-search t)
;;        to-line)
;;    (goto-char (point-min))
;;    ;; search for the To: lines and make Sent-via: lines from them
;;    ;; search for the next To: line
;;    (while (re-search-forward "^\\(to\\|cc\\):" end t)
;;      ;; Grab this line plus all its continuations, sans the `to:'.
;;      (let ((to-line
;;             (buffer-substring (point)
;;                               (progn
;;                                 (if (re-search-forward "^[^ \t\n]" end t)
;;                                     (backward-char 1)
;;                                   (goto-char end))
;;				   (point)))))
;;	  ;; Insert a copy, with altered header field name.
;;	  (insert-before-markers "Sent-via:" to-line))))))

(defun mail-to ()
  "Move point to end of To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "To"))

(defun mail-subject ()
  "Move point to end of Subject-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Subject"))

(defun mail-cc ()
  "Move point to end of CC-field.  Create a CC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nCC: "))))

(defun mail-bcc ()
  "Move point to end of BCC-field.  Create a BCC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "bcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nBCC: "))))

(defun mail-fcc (folder)
  "Add a new FCC field, with file name completion."
  (interactive "FFolder carbon copy: ")
  (expand-abbrev)
  (or (mail-position-on-field "fcc" t)	;Put new field after exiting FCC.
      (mail-position-on-field "to"))
  (insert "\nFCC: " folder))

(defun mail-reply-to ()      
  "Move point to end of Reply-To-field.  Create a Reply-To field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "reply-to" t)
      (progn (mail-position-on-field "to")
	     (insert "\nReply-To: "))))

(defun mail-position-on-field (field &optional soft)
  (let (end
	(case-fold-search t))
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (setq end (match-beginning 0))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote field) ":") end t)
	(progn
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line)
	  (skip-chars-backward "\n")
	  t)
      (or soft
	  (progn (goto-char end)
		 ;; #### FSF has the next two clauses reversed.
		 ;; which is correct?
		 (skip-chars-backward "\n")
		 (insert "\n" field ": ")))
      nil)))

(defun mail-text ()
  "Move point to beginning of message text."
  (interactive)
  (expand-abbrev)
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n")))

(defun mail-signature (&optional atpoint)
  "Sign letter with contents of the file `mail-signature-file'.
Prefix arg means put contents at point."
  (interactive "P")
  (save-excursion
    (or atpoint
	(goto-char (point-max)))
    (skip-chars-backward " \t\n")
    (end-of-line)
    (or atpoint
        (delete-region (point) (point-max)))
    (insert "\n\n-- \n")
    (insert-file-contents (expand-file-name mail-signature-file))))

(defun mail-fill-yanked-message (&optional justifyp)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (fill-individual-paragraphs (point)
				(point-max)
				justifyp
				t)))

(defun mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    (mail-yank-clear-headers start (mark t))
    (if (null mail-yank-prefix)
	(indent-rigidly start (mark t) mail-indentation-spaces)
      (save-excursion
	(goto-char start)
	(while (< (point) (mark t))
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point))
	    (reader-buf mail-reply-buffer)
	    (reader-window (get-buffer-window mail-reply-buffer
					      (selected-frame))))
	;; If the original message is in another window in the same frame,
	;; delete that window to save screen space.
	;; t means don't alter other frames.
	(if reader-window
	    (delete-windows-on reader-buf t))
	(insert-buffer reader-buf)
	(if (consp arg)
	    nil
	  (goto-char start)
	  (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
					   mail-indentation-spaces)))
	    (cond (mail-citation-hook
                   (run-hooks 'mail-citation-hook))
                  (mail-yank-hooks
                   (run-hooks 'mail-yank-hooks))
                  (t
                   (mail-indent-citation)))))
	(exchange-point-and-mark t)
	(if (not (eolp)) (insert ?\n)))))

(defun mail-yank-clear-headers (start end)
  (if mail-yank-ignored-headers
      (save-excursion
	(goto-char start)
	(if (search-forward "\n\n" end t)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char start)
	      (while (let ((case-fold-search t))
		       (re-search-forward mail-yank-ignored-headers nil t))
		(beginning-of-line)
		(delete-region (point)
			       (progn (re-search-forward "\n[^ \t]")
				      (forward-char -1)
				      (point)))))))))

;; Put these last, to reduce chance of lossage from quitting in middle of loading the file.

;;;###autoload
(defun mail (&optional noerase to subject in-reply-to cc replybuffer actions)
  "Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

Optionally, the signature file `mail-signature-file' can be inserted at the
end; see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

The variable `mail-signature' controls whether the signature file
`mail-signature-file' is inserted immediately.

If `mail-signature' is nil, use \\[mail-signature] to insert the
signature in `mail-signature-file'.

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

The normal hook `mail-setup-hook' is run after the message is
initialized.  It can add more default fields to the message.

When calling from a program, the first argument if non-nil says
not to erase the existing contents of the `*mail*' buffer.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'."
  (interactive "P")
  (if mail-use-multiple-buffers-p

      ;; RMS doesn't like this behavior but it seems more logical to me. --ben
      (let ((index 1)
	    buffer)
	;; If requested, look for a mail buffer that is modified and go to it.
	(if noerase
	    (progn
	      (while (and (setq buffer
				(get-buffer (if (= 1 index) "*mail*"
					      (format "*mail*<%d>" index))))
			  (not (buffer-modified-p buffer)))
		(setq index (1+ index)))
	      (if buffer (switch-to-buffer buffer)
		;; If none exists, start a new message.
		;; This will never re-use an existing unmodified mail buffer
		;; (since index is not 1 anymore).  Perhaps it should.
		(setq noerase nil))))
	;; Unless we found a modified message and are happy, start a
	;; new message.
	(if (not noerase)
	    (progn
	      ;; Look for existing unmodified mail buffer.
	      (while (and (setq buffer
				(get-buffer (if (= 1 index) "*mail*"
					      (format "*mail*<%d>" index))))
			  (buffer-modified-p buffer))
		(setq index (1+ index)))
	      ;; If none, make a new one.
	      (or buffer
		  (setq buffer (generate-new-buffer "*mail*")))
	      ;; Go there and initialize it.
	      (switch-to-buffer buffer)
	      (erase-buffer)
	      ;; put mail auto-save files in home dir instead of
	      ;; scattering them around the file system.
	      (setq default-directory (or mail-dir (expand-file-name "~/")))
	      (auto-save-mode auto-save-default)
	      (mail-mode)
	      (mail-setup to subject in-reply-to cc replybuffer actions)
	      (if (and buffer-auto-save-file-name
		       (file-exists-p buffer-auto-save-file-name))
		  (message "Auto save file for draft message exists; consider M-x mail-recover"))
	      t)))

    ;; Alternate behavior that RMS likes.
    (pop-to-buffer "*mail*")
    (auto-save-mode auto-save-default)
    (mail-mode)
    ;; Disconnect the buffer from its visited file
    ;; (in case the user has actually visited a file *mail*).
;  (set-visited-file-name nil)
    (let (initialized)
      (and (not noerase)
	   (or (not (buffer-modified-p))
	       (y-or-n-p "Unsent message being composed; erase it? "))
	   (progn (erase-buffer)
		  (mail-setup to subject in-reply-to cc replybuffer actions)
		  (setq initialized t)))
      (if (and buffer-auto-save-file-name
	       (file-exists-p buffer-auto-save-file-name))
	  (message "Auto save file for draft message exists; consider M-x mail-recover"))
      initialized)))

(defun mail-recover ()
  "Reread contents of current buffer from its last auto-save file."
  (interactive)
  (let ((file-name (let ((default-directory (expand-file-name "~/")))
		     ;; put mail auto-save files in home dir instead of
		     ;; scattering them around the file system.
		     (make-auto-save-file-name))))
    (cond ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-disable-undo standard-output)
		   (let ((default-directory "/"))
		     (call-process
		      "ls" nil standard-output nil "-l" file-name))))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil)))
	  (t (error "mail-recover cancelled")))))

;;;###autoload
(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;;###autoload
(defun mail-other-frame (&optional noerase to subject in-reply-to cc
				   replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another frame."
  (interactive "P")
  (let ((pop-up-frames t)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(same-window-buffer-names nil)
	(same-window-regexps nil))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer sendactions))

;;; Do not execute these when sendmail.el is loaded,
;;; only in loaddefs.el.
;;; Do not autoload, this package is obsolete. -sb
;;;###autoload (define-key ctl-x-map "m" 'mail)
;;;###autoload (define-key ctl-x-4-map "m" 'mail-other-window)
;;;###autoload (define-key ctl-x-5-map "m" 'mail-other-frame)

;;;###autoload (add-hook 'same-window-buffer-names "*mail*")

;;; Do not add anything but external entries on this page.

(provide 'sendmail)

;;; sendmail.el ends here
