;;; code-cmds.el --- Commands for manipulating coding systems.

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2000,2006 Free Software Foundation
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2002 Ben Wing.


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

;;
;; This code defines the keybindings and utility commands for the
;; user to manipulate coding systems.
;; This code used to be in mule-cmds.el which now contains only the
;; additional bindings/commands that are available for full Mule.


;;; Code:

;;; Coding related key bindings and menus.

(defvar coding-keymap (make-sparse-keymap "Coding+Mule")
  "Keymap for coding-system-specific and (when available) Mule commands.")

;; Keep "C-x C-m ..." for coding/mule specific commands.
(define-key ctl-x-map "\C-m" coding-keymap)

(define-key coding-keymap "f" 'set-buffer-file-coding-system)
(define-key coding-keymap "F" 'set-default-buffer-file-coding-system) ; XEmacs
(define-key coding-keymap "t" 'set-terminal-coding-system)
(define-key coding-keymap "p" 'set-buffer-process-coding-system)
(define-key coding-keymap "c" 'universal-coding-system-argument)
;;(define-key coding-keymap "c" 'list-coding-system-briefly) ; XEmacs
(when-fboundp 'describe-coding-system
  (define-key coding-keymap "C" 'describe-coding-system))	 ; XEmacs
(when-fboundp 'set-selection-coding-system
  (define-key coding-keymap "x" 'set-selection-coding-system)
  (define-key coding-keymap "X" 'set-next-selection-coding-system))

;; XEmacs change: make code readable, and sanity-check EOL-TYPE.
(defun coding-system-change-eol-conversion (coding-system eol-type)
  "Return a version of CODING-SYSTEM that provides EOL-TYPE eol conversion.
EOL-TYPE should be `lf', `crlf', `cr' or nil.  nil means the returned coding
system automatically detects the end-of-line convention while decoding.
EOL-TYPE may also be one of the symbols `unix', `dos' or `mac', meaning
`lf', `crlf', and `cr' respectively."
  (setq eol-type (cond ((or (eq eol-type 'unix) (eq eol-type 'lf)) 'eol-lf)
		       ((or (eq eol-type 'dos) (eq eol-type 'crlf)) 'eol-crlf)
		       ((or (eq eol-type 'mac) (eq eol-type 'cr)) 'eol-cr)
		       ((null eol-type) nil)
		       (t (error 'invalid-constant eol-type))))
  (coding-system-name
   (let ((orig-eol-type (cdr (assq (coding-system-eol-type coding-system)
				   '((lf . eol-lf)
				     (cr . eol-cr)
				     (crlf . eol-crlf)
				     ;; #### also returns nil if not a key
				     (nil . nil)))))
	 (base (coding-system-base coding-system)))
     (cond ((eq eol-type orig-eol-type) coding-system)
	   ((null orig-eol-type)
	    (coding-system-property coding-system eol-type))
	   ((null eol-type) base)
	   ((null (coding-system-eol-type base))
	    (coding-system-property base eol-type))
	   (t (warn "couldn't change EOL conversion of %s from %s to %s."
		    coding-system orig-eol-type eol-type)
	      ;; return nil for compatibility with old code
	      nil)))))

;; (defun coding-system-change-text-conversion (coding-system coding)
;;   "Return a coding system which differs from CODING-SYSTEM in text conversion.
;; The returned coding system converts text by CODING
;; but end-of-line as the same way as CODING-SYSTEM.
;; If CODING is nil, the returned coding system detects
;; how text is formatted automatically while decoding."
;;   (if (not coding)
;;       (coding-system-base coding-system)
;;     (let ((eol-type (coding-system-eol-type coding-system)))
;;       (coding-system-change-eol-conversion
;;        coding
;;        (if (numberp eol-type) (aref [unix dos mac] eol-type))))))

(defun universal-coding-system-argument ()
  "Execute an I/O command using the specified coding system."
  (interactive)
  (let* ((default (and buffer-file-coding-system
		       (not (eq (coding-system-type buffer-file-coding-system)
				t))
		       (coding-system-name buffer-file-coding-system)))
	 (coding-system
	  (read-coding-system
	   (if default
	       (format "Coding system for following command (default, %s): "
		       default)
	     "Coding system for following command: ")
	   default))
	 (keyseq (read-key-sequence
		  (format "Command to execute with %s:" coding-system)))
	 (cmd (key-binding keyseq)))
    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system))
      (message "")
      (call-interactively cmd))))

(defun set-default-output-coding-systems (coding-system)
  "Set default value for coding system output to CODING-SYSTEM.
This sets the coding system of newly created buffers (the default value of
`buffer-file-coding-system') and the default coding system for output to a
subprocess (the CDR of `default-process-coding-system').

Other defaults are not changed; see `prefer-coding-system' for why."
  (check-coding-system coding-system)
  (set-default-buffer-file-coding-system coding-system)
  (setq default-process-coding-system
	(cons (car default-process-coding-system) coding-system)))

(defun prefer-coding-system (coding-system)
  "Add CODING-SYSTEM at the front of the priority list for automatic detection.
This also sets the coding system of newly created buffers (the default
value of `buffer-file-coding-system') and the default coding system for
output to a subprocess (the CDR of `default-process-coding-system').

If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
systems set by this function will use that type of EOL conversion.

This does not change the default coding system for converting file names
because that is dependent on the current locale; it's changed when
`set-language-environment' is called.  It does not change
`terminal-coding-system' or `keyboard-coding-system'; they should get set
when the terminal is opened (and are typically an inherent property of the
terminal), and don't change afterward.  It does not change the default
coding system for reading files or input from a subprocess; they should
remain as `undecided' so that automatic detection is done."
  (interactive "zPrefer coding system: ")
  (if (not (and coding-system (find-coding-system coding-system)))
      (error "Invalid coding system `%s'" coding-system))
  (let ((coding-category (coding-system-category coding-system))
	(base (coding-system-base coding-system))
	(eol-type (coding-system-eol-type coding-system)))
    (if (not coding-category)
	;; CODING-SYSTEM is no-conversion or undecided.
	(error "Can't prefer the coding system `%s'" coding-system))
    (set-coding-category-system coding-category (or base coding-system))
    ;; (update-coding-systems-internal)
    (or (eq coding-category (car (coding-category-list)))
	;; We must change the order.
	(set-coding-priority-list (list coding-category)))
    (if (and base (interactive-p))
	(message "Highest priority is set to %s (base of %s)"
		 base coding-system))
    ;; If they asked for specific EOL conversion, honor that.
    (if (memq eol-type '(lf crlf cr unix dos mac))
	(setq coding-system
	      (coding-system-change-eol-conversion base eol-type))
      (setq coding-system base))
    (set-default-output-coding-systems coding-system)))

;; (defun find-coding-systems-region-subset-p (list1 list2)
;;   "Return non-nil if all elements in LIST1 are included in LIST2.
;; Comparison done with EQ."
;;   (catch 'tag
;;     (while list1
;;       (or (memq (car list1) list2)
;;           (throw 'tag nil))
;;       (setq list1 (cdr list1)))
;;     t))

;; (defun find-coding-systems-region (from to)
;;   "Return a list of proper coding systems to encode a text between FROM and TO.
;; All coding systems in the list can safely encode any multibyte characters
;; in the text.
;; 
;; If the text contains no multibyte characters, return a list of a single
;; element `undecided'."
;;   (find-coding-systems-for-charsets (find-charset-region from to)))

;; (defun find-coding-systems-string (string)
;;   "Return a list of proper coding systems to encode STRING.
;; All coding systems in the list can safely encode any multibyte characters
;; in STRING.
;; 
;; If STRING contains no multibyte characters, return a list of a single
;; element `undecided'."
;;   (find-coding-systems-for-charsets (find-charset-string string)))

;; (defun find-coding-systems-for-charsets (charsets)
;;   "Return a list of proper coding systems to encode characters of CHARSETS.
;; CHARSETS is a list of character sets."
;;   (if (or (null charsets)
;;           (and (= (length charsets) 1)
;;                (eq 'ascii (car charsets))))
;;       '(undecided)
;;     (setq charsets (delq 'composition charsets))
;;     (let ((l (coding-system-list 'base-only))
;;           (charset-preferred-codings
;;            (mapcar (function
;;                     (lambda (x)
;;                       (if (eq x 'unknown)
;;                           'raw-text
;;                         (get-charset-property x 'preferred-coding-system))))
;;                    charsets))
;;           (priorities (mapcar (function (lambda (x) (symbol-value x)))
;;                               coding-category-list))
;;           codings coding safe)
;;       (if (memq 'unknown charsets)
;;           ;; The region contains invalid multibyte characters.
;;           (setq l '(raw-text)))
;;       (while l
;;         (setq coding (car l) l (cdr l))
;;         (if (and (setq safe (coding-system-get coding 'safe-charsets))
;;                  (or (eq safe t)
;;                      (find-coding-systems-region-subset-p charsets safe)))
;;             ;; We put the higher priority to coding systems included
;;             ;; in CHARSET-PREFERRED-CODINGS, and within them, put the
;;             ;; higher priority to coding systems which support smaller
;;             ;; number of charsets.
;;             (let ((priority
;;                    (+ (if (coding-system-get coding 'mime-charset) 4096 0)
;;                       (lsh (length (memq coding priorities)) 7)
;;                       (if (memq coding charset-preferred-codings) 64 0)
;;                       (if (> (coding-system-type coding) 0) 32 0)
;;                       (if (consp safe) (- 32 (length safe)) 0))))
;;               (setq codings (cons (cons priority coding) codings)))))
;;       (mapcar 'cdr
;;               (sort codings (function (lambda (x y) (> (car x) (car y))))))
;;       )))

;; (defun find-multibyte-characters (from to &optional maxcount excludes)
;;   "Find multibyte characters in the region specified by FROM and TO.
;; If FROM is a string, find multibyte characters in the string.
;; The return value is an alist of the following format:
;;   ((CHARSET COUNT CHAR ...) ...)
;; where
;;   CHARSET is a character set,
;;   COUNT is a number of characters,
;;   CHARs are found characters of the character set.
;; Optional 3rd arg MAXCOUNT limits how many CHARs are put in the above list.
;; Optional 4th arg EXCLUDE is a list of character sets to be ignored.
;; 
;; For invalid characters, CHARs are actually strings."
;;   (let ((chars nil)
;;         charset char)
;;     (if (stringp from)
;;         (let ((idx 0))
;;           (while (setq idx (string-match "[^\000-\177]" from idx))
;;             (setq char (aref from idx)
;;                   charset (char-charset char))
;;             (if (eq charset 'unknown)
;;                 (setq char (match-string 0)))
;;             (if (or (eq charset 'unknown)
;;                     (not (or (eq excludes t) (memq charset excludes))))
;;                 (let ((slot (assq charset chars)))
;;                   (if slot
;;                       (if (not (memq char (nthcdr 2 slot)))
;;                           (let ((count (nth 1 slot)))
;;                             (setcar (cdr slot) (1+ count))
;;                             (if (or (not maxcount) (< count maxcount))
;;                                 (nconc slot (list char)))))
;;                     (setq chars (cons (list charset 1 char) chars)))))
;;             (setq idx (1+ idx))))
;;       (save-excursion
;;         (goto-char from)
;;         (while (re-search-forward "[^\000-\177]" to t)
;;           (setq char (preceding-char)
;;                 charset (char-charset char))
;;           (if (eq charset 'unknown)
;;               (setq char (match-string 0)))
;;           (if (or (eq charset 'unknown)
;;                   (not (or (eq excludes t) (memq charset excludes))))
;;               (let ((slot (assq charset chars)))
;;                 (if slot
;;                     (if (not (member char (nthcdr 2 slot)))
;;                         (let ((count (nth 1 slot)))
;;                           (setcar (cdr slot) (1+ count))
;;                           (if (or (not maxcount) (< count maxcount))
;;                               (nconc slot (list char)))))
;;                   (setq chars (cons (list charset 1 char) chars))))))))
;;     (nreverse chars)))

;; (defvar last-coding-system-specified nil
;;   "Most recent coding system explicitly specified by the user when asked.
;; This variable is set whenever Emacs asks the user which coding system
;; to use in order to write a file.  If you set it to nil explicitly,
;; then call `write-region', then afterward this variable will be non-nil
;; only if the user was explicitly asked and specified a coding system.")

;; (defun select-safe-coding-system (from to &optional default-coding-system)
;;   "Ask a user to select a safe coding system from candidates.
;; The candidates of coding systems which can safely encode a text
;; between FROM and TO are shown in a popup window.
;; 
;; Optional arg DEFAULT-CODING-SYSTEM specifies a coding system to be
;; checked at first.  If omitted, buffer-file-coding-system of the
;; current buffer is used.
;; 
;; If the text can be encoded safely by DEFAULT-CODING-SYSTEM, it is
;; returned without any user interaction.
;; 
;; Kludgy feature: if FROM is a string, the string is the target text,
;; and TO is ignored."
;;   (or default-coding-system
;;       (setq default-coding-system buffer-file-coding-system))
;;   (let* ((charsets (if (stringp from) (find-charset-string from)
;;                      (find-charset-region from to)))
;;          (safe-coding-systems (find-coding-systems-for-charsets charsets)))
;;     (if (or (not enable-multibyte-characters)
;;             (eq (car safe-coding-systems) 'undecided)
;;             (eq default-coding-system 'no-conversion)
;;             (and default-coding-system
;;                  (memq (coding-system-base default-coding-system)
;;                        safe-coding-systems)))
;;         default-coding-system
;; 
;;       ;; At first, change each coding system to the corresponding
;;       ;; mime-charset name if it is also a coding system.
;;       (let ((l safe-coding-systems)
;;             mime-charset)
;;         (while l
;;           (setq mime-charset (coding-system-get (car l) 'mime-charset))
;;           (if (and mime-charset (coding-system-p mime-charset))
;;               (setcar l mime-charset))
;;           (setq l (cdr l))))
;; 
;;       (let ((non-safe-chars (find-multibyte-characters
;;                              from to 3
;;                              (and default-coding-system
;;                                   (coding-system-get default-coding-system
;;                                                      'safe-charsets))))
;;             show-position overlays)
;;         (save-excursion
;;           ;; Highlight characters that default-coding-system can't encode.
;;           (when (integerp from)
;;             (goto-char from)
;;             (let ((found nil))
;;               (while (and (not found)
;;                           (re-search-forward "[^\000-\177]" to t))
;;                 (setq found (assq (char-charset (preceding-char))
;;                                   non-safe-chars))))
;;             (forward-line -1)
;;             (setq show-position (point))
;;             (save-excursion
;;               (while (and (< (length overlays) 256)
;;                           (re-search-forward "[^\000-\177]" to t))
;;                 (let* ((char (preceding-char))
;;                        (charset (char-charset char)))
;;                   (when (assq charset non-safe-chars)
;;                     (setq overlays (cons (make-overlay (1- (point)) (point))
;;                                          overlays))
;;                     (overlay-put (car overlays) 'face 'highlight))))))
;; 
;;           ;; At last, ask a user to select a proper coding system.  
;;           (unwind-protect
;;               (save-window-excursion
;;                 (when show-position
;;                   ;; At first, be sure to show the current buffer.
;;                   (set-window-buffer (selected-window) (current-buffer))
;;                   (set-window-start (selected-window) show-position))
;;                 ;; Then, show a helpful message.
;;                 (with-output-to-temp-buffer "*Warning*"
;;                   (save-excursion
;;                     (set-buffer standard-output)
;;                     (insert "The target text contains the following non ASCII character(s):\n")
;;                     (let ((len (length non-safe-chars))
;;                           (shown 0))
;;                       (while (and non-safe-chars (< shown 3))
;;                         (when (> (length (car non-safe-chars)) 2)
;;                           (setq shown (1+ shown))
;;                           (insert (format "%25s: " (car (car non-safe-chars))))
;;                           (let ((l (nthcdr 2 (car non-safe-chars))))
;;                             (while l
;;                               (if (or (stringp (car l)) (char-valid-p (car l)))
;;                                   (insert (car l)))
;;                               (setq l (cdr l))))
;;                           (if (> (nth 1 (car non-safe-chars)) 3)
;;                               (insert "..."))
;;                           (insert "\n"))
;;                         (setq non-safe-chars (cdr non-safe-chars)))
;;                       (if (< shown len)
;;                           (insert (format "%27s\n" "..."))))
;;                     (insert (format "\
;; These can't be encoded safely by the coding system %s.
;; 
;; Please select one from the following safe coding systems:\n"
;;                                     default-coding-system))
;;                     (let ((pos (point))
;;                           (fill-prefix "  "))
;;                       (mapcar (function (lambda (x) (princ "  ") (princ x)))
;;                               safe-coding-systems)
;;                       (fill-region-as-paragraph pos (point)))))
;; 
;;                 ;; Read a coding system.
;;                 (let* ((safe-names (mapcar (lambda (x) (list (symbol-name x)))
;;                                            safe-coding-systems))
;;                        (name (completing-read
;;                               (format "Select coding system (default %s): "
;;                                       (car safe-coding-systems))
;;                               safe-names nil t nil nil
;;                               (car (car safe-names)))))
;;                   (setq last-coding-system-specified (intern name))
;;                   (if (integerp (coding-system-eol-type default-coding-system))
;;                       (setq last-coding-system-specified
;;                             (coding-system-change-eol-conversion
;;                              last-coding-system-specified
;;                              (coding-system-eol-type default-coding-system))))
;;                   last-coding-system-specified))
;;             (kill-buffer "*Warning*")
;;             (while overlays
;;               (delete-overlay (car overlays))
;;               (setq overlays (cdr overlays)))))))))

;; (setq select-safe-coding-system-function 'select-safe-coding-system)

;; (defun select-message-coding-system ()
;;   "Return a coding system to encode the outgoing message of the current buffer.
;; It at first tries the first coding system found in these variables
;; in this order:
;;   (1) local value of `buffer-file-coding-system'
;;   (2) value of `sendmail-coding-system'
;;   (3) value of `default-buffer-file-coding-system'
;;   (4) value of `default-sendmail-coding-system'
;; If the found coding system can't encode the current buffer,
;; or none of them are bound to a coding system,
;; it asks the user to select a proper coding system."
;;   (let ((coding (or (and (local-variable-p 'buffer-file-coding-system)
;;                          buffer-file-coding-system)
;;                     sendmail-coding-system
;;                     default-buffer-file-coding-system
;;                     default-sendmail-coding-system)))
;;     (if (eq coding 'no-conversion)
;;         ;; We should never use no-conversion for outgoing mails.
;;         (setq coding nil))
;;     (if (fboundp select-safe-coding-system-function)
;;         (funcall select-safe-coding-system-function
;;                  (point-min) (point-max) coding)
;;       coding)))

(provide 'code-cmds)

;;; code-cmds.el ends here
