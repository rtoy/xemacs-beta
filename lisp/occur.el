;;; occur.el --- Show all lines in the current buffer containing a match for REGEXP.

;; Copyright (C) 1985, 1986, 1987, 1992, 1994, 1996, 1997, 2000, 2001,
;;   2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: internal

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Synched up with: FSF 22.0.50.1 (CVS)

(require 'next-error)
(defun query-replace-descr (string)
  (mapconcat 'isearch-text-char-description string ""))

(defvar occur-mode-map ()
  "Keymap for `occur-mode'.")
(if occur-mode-map
    ()
  (setq occur-mode-map (make-sparse-keymap))
  (set-keymap-name occur-mode-map 'occur-mode-map) ; XEmacs
  (define-key occur-mode-map 'button2 'occur-mode-mouse-goto) ; XEmacs
  (define-key occur-mode-map "\C-c\C-c" 'occur-mode-goto-occurrence)
  (define-key occur-mode-map "\C-m" 'occur-mode-goto-occurrence)
  (define-key occur-mode-map "o" 'occur-mode-goto-occurrence-other-window)
  (define-key occur-mode-map "\C-o" 'occur-mode-display-occurrence)
  (define-key occur-mode-map "\M-n" 'occur-next)
  (define-key occur-mode-map "\M-p" 'occur-prev)
  (define-key occur-mode-map "r" 'occur-rename-buffer)
  (define-key occur-mode-map "c" 'clone-buffer)
  (define-key occur-mode-map "g" 'revert-buffer)
  (define-key occur-mode-map "q" 'quit-window)
  (define-key occur-mode-map "z" 'kill-this-buffer)
  (define-key occur-mode-map "\C-c\C-f" 'next-error-follow-minor-mode))

(defvar occur-revert-arguments nil
  "Arguments to pass to `occur-1' to revert an Occur mode buffer.
See `occur-revert-function'.")

(defcustom occur-mode-hook nil ; XEmacs
  "Hook run when entering Occur mode."
  :type 'hook
  :group 'matching)

(defcustom occur-hook nil
  "Hook run by Occur when there are any matches."
  :type 'hook
  :group 'matching)

(put 'occur-mode 'mode-class 'special)
;;;###autoload
(defun occur-mode ()
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name (gettext "Occur")) ; XEmacs
  (set (make-local-variable 'revert-buffer-function) 'occur-revert-function)
  (make-local-variable 'occur-revert-arguments)
  (add-hook 'change-major-mode-hook 'turn-off-font-lock t t)
  (setq next-error-function 'occur-next-error)
  (require 'mode-motion) ; XEmacs
  (setq mode-motion-hook 'mode-motion-highlight-line) ; XEmacs
  (run-mode-hooks 'occur-mode-hook))

(defun occur-revert-function (ignore1 ignore2)
  "Handle `revert-buffer' for Occur mode buffers."
  (apply 'occur-1 (append occur-revert-arguments (list (buffer-name)))))

;; FSF Version of next function:
; (defun occur-mode-mouse-goto (event)
;   "In Occur mode, go to the occurrence whose line you click on."
;   (interactive "e")
;   (let (pos)
;     (save-excursion
;       (set-buffer (window-buffer (posn-window (event-end event))))
;       (save-excursion
; 	(goto-char (posn-point (event-end event)))
; 	(setq pos (occur-mode-find-occurrence))))
;     (pop-to-buffer (marker-buffer pos))
;     (goto-char pos)))

(defun occur-mode-mouse-goto (event)
  "Go to the occurrence highlighted by mouse.
This function should be bound to a mouse key in the `*Occur*' buffer."
  (interactive "e")
  (let ((window-save (selected-window))
	(frame-save (selected-frame)))
    ;; preserve the window/frame setup
    (unwind-protect
	(progn
	  (mouse-set-point event)
	  (occur-mode-goto-occurrence))
      (select-frame frame-save)
      (select-window window-save))))

(defun occur-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'occur-target)))
    (unless pos
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this occurrence was killed"))
    pos))

(defun occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence))
	window
	;; Bind these to ensure `display-buffer' puts it in another window.
	same-window-buffer-names
	same-window-regexps)
    (setq window (display-buffer (marker-buffer pos)))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos))))

(defun occur-find-match (n search message)
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (setq r (funcall search (point) 'occur-match))
      (and r
           (get-text-property r 'occur-match)
           (setq r (funcall search r 'occur-match)))
      (if r
          (goto-char r)
        (error message))
      (setq n (1- n)))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'next-single-property-change "No more matches"))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'previous-single-property-change "No earlier matches"))

(defun occur-next-error (&optional argp reset)
  "Move to the Nth (default 1) next match in an Occur mode buffer.
Compatibility function for \\[next-error-framework-next-error] invocations."
  (interactive "p")
  ;; we need to run occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'occur-mode))))

    (goto-char (cond (reset (point-min))
		     ((< argp 0) (line-beginning-position))
		     ((line-end-position))))
    (occur-find-match
     (abs argp)
     (if (> 0 argp)
	 #'previous-single-property-change
       #'next-single-property-change)
     "No more matches")
    ;; In case the *Occur* buffer is visible in a nonselected window.
    (set-window-point (get-buffer-window (current-buffer)) (point))
    (occur-mode-goto-occurrence)))

(defface match
  '((((class color) (background light))
     (:background "Tan"))
    (((class color) (background dark))
     (:background "RoyalBlue3"))
    (((class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "gray")))
  "Face used to highlight matches permanently."
  :group 'matching
  :version "22.1")

(defcustom list-matching-lines-default-context-lines 0
  "*Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

;;;###autoload
(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'match
  "*Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "*Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type 'face
  :group 'matching)

(defun occur-accumulate-lines (count &optional keep-props)
  (save-excursion
    (let ((forwardp (> count 0))
	  result beg end)
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(bobp))))
	(setq count (+ count (if forwardp -1 1)))
	(setq beg (line-beginning-position)
	      end (line-end-position))
	(if (and keep-props (if-boundp 'jit-lock-mode jit-lock-mode)
		 (text-property-not-all beg end 'fontified t))
	    (if-fboundp 'jit-lock-fontify-now
		(jit-lock-fontify-now beg end)))
	(push
	 (funcall (if keep-props
		      #'buffer-substring
		    #'buffer-substring-no-properties)
		  beg end)
	 result)
	(forward-line (if forwardp 1 -1)))
      (nreverse result))))

(defun occur-read-primary-args ()
  (list (let* ((default (or (symbol-near-point)
			    (and regexp-history
				 (car regexp-history))))
	       (minibuffer-history-minimum-string-length 0)
	       (input
		 (if default
		     ;; XEmacs: rewritten for I18N3 snarfing
		     (read-from-minibuffer
		      (format "List lines matching regexp (default `%s'): "
			      default) nil nil nil 'regexp-history nil
                              default)
		   (read-from-minibuffer
		    "List lines matching regexp: "
		    nil nil nil
		    'regexp-history))))
	  (if (equal input "")
	      default
	    input))
	(when current-prefix-arg
	  (prefix-numeric-value current-prefix-arg))))

;;;###autoload
(defun occur-rename-buffer (&optional unique-p interactive-p)
  "Rename the current *Occur* buffer to *Occur: original-buffer-name*.
Here `original-buffer-name' is the buffer name were Occur was originally run.
When given the prefix argument, or called non-interactively, the renaming
will not clobber the existing buffer(s) of that name, but use
`generate-new-buffer-name' instead.  You can add this to `occur-hook'
if you always want a separate *Occur* buffer for each buffer where you
invoke `occur'."
  (interactive "P\np")
  (with-current-buffer
      (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
    (rename-buffer (concat "*Occur: "
                           (mapconcat #'buffer-name
                                      (car (cddr occur-revert-arguments)) "/")
                           "*")
                   (or unique-p (not interactive-p)))))

;;;###autoload
(defun occur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.
This function can not handle matches that span more than one line.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
  (interactive (occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

;;;###autoload
(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
;	   (ido-ignore-item-temp-list bufs)
	   )
      (while (not (string-equal
		   (setq buf (read-buffer
			      (if (and-boundp 'read-buffer-function
				       '(eq read-buffer-function 'ido-read-buffer))
				  "Next buffer to search (C-j to end): "
				"Next buffer to search (RET to end): ")
			      nil t))
		   ""))
	(add-to-list 'bufs buf)
;	(setq ido-ignore-item-temp-list bufs)
	)
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

;;;###autoload
(defun multi-occur-by-filename-regexp (bufregexp regexp &optional nlines)
  "Show all lines matching REGEXP in buffers named by BUFREGEXP.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-from-minibuffer
	     "List lines in buffers whose filename matches regexp: "
	     nil
	     nil
	     nil
	     'regexp-history)))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nlines
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (and (buffer-file-name buf)
					(string-match bufregexp
						      (buffer-file-name buf)))
			       buf))
			   (buffer-list))))))

(defun occur-1 (regexp nlines bufs &optional buf-name)
  (unless buf-name
    (setq buf-name "*Occur*"))
  (let (occur-buf
	(active-bufs (delq nil (mapcar #'(lambda (buf)
					   (when (buffer-live-p buf) buf))
				       bufs))))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name (mapcar 'buffer-name active-bufs))
      (with-current-buffer (get-buffer buf-name)
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-current-buffer occur-buf
      (occur-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(let ((count (occur-engine
		      regexp active-bufs occur-buf
		      (or nlines list-matching-lines-default-context-lines)
		      (and case-fold-search
			   (no-upper-case-p regexp t))
		      list-matching-lines-buffer-name-face
		      nil list-matching-lines-face t)))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d buffer%s%s; %s match%s for `%s'"
		     bufcount (if (= bufcount 1) "" "s")
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (if (= count 1) "" "es")
		     regexp))
	  (setq occur-revert-arguments (list regexp nlines bufs))
          (if (= count 0)
              (kill-buffer occur-buf)
            (display-buffer occur-buf)
            (setq next-error-last-buffer occur-buf)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (run-hooks 'occur-hook)))))))

(defun occur-engine-add-prefix (lines)
  (mapcar
   #'(lambda (line)
       (concat "       :" line "\n"))
   lines))

(defun occur-engine (regexp buffers out-buf nlines case-fold-search
			    title-face prefix-face match-face keep-props)
  (with-current-buffer out-buf
    (let ((globalcount 0)
	  ;; Don't generate undo entries for creation of the initial contents.
	  (buffer-undo-list t)
	  (coding nil))
      ;; Map over all the buffers
      (dolist (buf buffers)
	(when (buffer-live-p buf)
	  (let ((matches 0)	;; count of matched lines
		(lines 1)	;; line count
		(matchbeg 0)
		(origpt nil)
		(begpt nil)
		(endpt nil)
		(marker nil)
		(curstring "")
		(headerpt (with-current-buffer out-buf (point))))
	    (save-excursion
	      (set-buffer buf)
	      (or coding
		  ;; Set CODING only if the current buffer locally
		  ;; binds buffer-file-coding-system.
		  (not (local-variable-p 'buffer-file-coding-system (current-buffer)))
		  (setq coding buffer-file-coding-system))
	      (save-excursion
		(goto-char (point-min)) ;; begin searching in the buffer
		(while (not (eobp))
		  (setq origpt (point))
		  (when (setq endpt (re-search-forward regexp nil t))
		    (setq matches (1+ matches)) ;; increment match count
		    (setq matchbeg (match-beginning 0))
		    (setq lines (+ lines (1- (count-lines origpt endpt))))
		    (save-excursion
		      (goto-char matchbeg)
		      (setq begpt (line-beginning-position)
			    endpt (line-end-position)))
		    (setq marker (make-marker))
		    (set-marker marker matchbeg)
		    (if (and keep-props
			     (if-boundp 'jit-lock-mode jit-lock-mode)
			     (text-property-not-all begpt endpt 'fontified t))
			(if-fboundp #'jit-lock-fontify-now
			    (jit-lock-fontify-now begpt endpt)))
		    (setq curstring (buffer-substring begpt endpt))
		    ;; Depropertize the string, and maybe
		    ;; highlight the matches
		    (let ((len (length curstring))
			  (start 0))
		      (unless keep-props
			(set-text-properties 0 len nil curstring))
		      (while (and (< start len)
				  (string-match regexp curstring start))
			(add-text-properties
			 (match-beginning 0) (match-end 0)
			 (append
			  `(occur-match t)
			  (when match-face
			    ;; Use `face' rather than `font-lock-face' here
			    ;; so as to override faces copied from the buffer.
			    `(face ,match-face)))
			 curstring)
			(setq start (match-end 0))))
		    ;; Generate the string to insert for this match
		    (let* ((out-line
			    (concat
			     ;; Using 7 digits aligns tabs properly.
			     (apply #'propertize (format "%7d:" lines)
				    (append
				     (when prefix-face
				       `(font-lock-face prefix-face))
				     '(occur-prefix t)))
			     ;; We don't put `mouse-face' on the newline,
			     ;; because that loses.  And don't put it
			     ;; on context lines to reduce flicker.
			     (propertize curstring 'mouse-face 'highlight)
			     "\n"))
			   (data
			    (if (= nlines 0)
				;; The simple display style
				out-line
			      ;; The complex multi-line display
			      ;; style.  Generate a list of lines,
			      ;; concatenate them all together.
			      (apply #'concat
				     (nconc
				      (occur-engine-add-prefix (nreverse (cdr (occur-accumulate-lines (- (1+ (abs nlines))) keep-props))))
				      (list out-line)
				      (if (> nlines 0)
					  (occur-engine-add-prefix
					   (cdr (occur-accumulate-lines (1+ nlines) keep-props)))))))))
		      ;; Actually insert the match display data
		      (with-current-buffer out-buf
			(let ((beg (point))
			      (end (progn (insert data) (point))))
			  (unless (= nlines 0)
			    (insert "-------\n"))
			  (add-text-properties
			   beg end
			   `(occur-target ,marker help-echo "mouse-2: go to this occurrence")))))
		    (goto-char endpt))
		  (if endpt
		      (progn
			(setq lines (1+ lines))
			;; On to the next match...
			(forward-line 1))
		    (goto-char (point-max))))))
	    (when (not (zerop matches)) ;; is the count zero?
	      (setq globalcount (+ globalcount matches))
	      (with-current-buffer out-buf
		(goto-char headerpt)
		(let ((beg (point))
		      end)
		  (insert (format "%d match%s for \"%s\" in buffer: %s\n"
				  matches (if (= matches 1) "" "es")
				  regexp (buffer-name buf)))
		  (setq end (point))
		  (add-text-properties beg end
				       (append
					(when title-face
					  `(font-lock-face ,title-face))
					`(occur-title ,buf))))
		(goto-char (point-min)))))))
      (if coding
	  ;; CODING is buffer-file-coding-system of the first buffer
	  ;; that locally binds it.  Let's use it also for the output
	  ;; buffer.
	  (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      globalcount)))

(provide 'occur)
