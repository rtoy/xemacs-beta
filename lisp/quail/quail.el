;; Quail -- Simple inputting method
;; Copyright (C) 1992 Free Software Foundation, Inc.
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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; 92.2.12  created for Mule Ver.0.9.0 by K.Handa <handa@etl.go.jp>
;;; long and gory change log deleted.

;; Although, EGG is the major inputing method supported by Mule,
;; it's, for the moment, not convenient for inputing Chinese letters.
;; So, I wrote this program as an alternative to EGG to be used
;; until revision of EGG.
;; I modified all translation tables of cxterm/dict/tit of X.V11R5
;; to be used with this system, those are named as quail-*.el.
;; Please load only necessary tables.

;; Quail serves as a front end processor for inputing
;; multilingual text from normal ASCII keyboard.  By defining a
;; translation table which maps ASCII string to multilingual
;; string, you can input any text from ASCII keyboard.

(require 'mule)

;;;###autoload
(defconst quail-version "2.2")

(defvar quail-region-face 'underline)
(defvar quail-selection-face 'highlight)

(defvar quail-self-insert-after-hook nil
  "Function(s) to call after inserting quail characters.")
(defvar quail-last-char nil
  "A character typed last time in Quail mode.")

(defvar quail-completion-buf nil)
(defvar quail-guidance-buf nil)
(defmacro quail-buffer-alive-p (buf)
  (list 'and (list 'bufferp buf) (list 'buffer-name buf)))

;; Buffer local variables
(defvar quail-mode nil
  "Non-nil if using Quail minor mode.")
(make-variable-buffer-local 'quail-mode)
(defvar quail-sub-mode nil
  "Non-nil if in sub-mode of Quail minor mode.")
(make-variable-buffer-local 'quail-sub-mode)
(defvar quail-keep-state nil)
(make-variable-buffer-local 'quail-keep-state)
(defvar quail-mode-string nil)
(make-variable-buffer-local 'quail-mode-string)
(defvar quail-overlay nil
  "Overlay which covers quail zone.")
(make-variable-buffer-local 'quail-overlay)
(defvar quail-current-key nil
  "Within Quail mode, a key string typed so far.")
(make-variable-buffer-local 'quail-current-key)
(defvar quail-current-str nil
  "Within Quail mode, a string currently translated from quail-current-key.")
(make-variable-buffer-local 'quail-current-str)
(defvar quail-previous-extra-mode-list nil)
(make-variable-buffer-local 'quail-previous-extra-mode-list)

;; Quail package 

;;;###autoload
(defvar quail-package-alist nil
  "Assoc list of quail-packages.  Each element is a list of:
  NAME, PROMPT, MAP, GUIDANCE, DOC, SUB-MODE-MAP, NOLEARN, DETERMINISTIC,
  TRANSLATE, LAYOUT, OBSOLETE, DECODE-MAP, and EXTRA-MODE-LIST.
See the document of `quail-define-package' for more detail.")

(defmacro quail-package-name () '(car quail-current-package))
(defmacro quail-prompt () '(nth 1 quail-current-package))
(defmacro quail-map () '(nth 2 quail-current-package))
(defmacro quail-guidance () '(nth 3 quail-current-package))
(defmacro quail-document () '(nth 4 quail-current-package))
(defmacro quail-sub-mode-map () '(nth 5 quail-current-package))
(defmacro quail-nolearn () '(nth 6 quail-current-package))
(defmacro quail-deterministic () '(nth 7 quail-current-package))
(defmacro quail-translate () '(nth 8 quail-current-package))
(defmacro quail-layout () '(nth 9 quail-current-package))
(defmacro quail-decode-map () '(nth 11 quail-current-package))
(defmacro quail-extra-mode-list () '(nth 12 quail-current-package))

(defalias 'quail-showkey 'quail-guidance)
(defalias 'quail-dynamic-list 'quail-guidance)
;;;###autoload(make-obsolete 'quail-showkey 'quail-guidance)
;;;###autoload(make-obsolete 'quail-dynamic-list 'quail-guidance)

;;;###autoload
(defvar quail-current-package nil "Current quail-pacakge.")
;;;###autoload
(make-variable-buffer-local 'quail-current-package)

(defvar quail-last-package nil "Last quail-pacakge.")

(defmacro quail-package (name)
  "Return quail-package named NAME."
  (list 'assoc name 'quail-package-alist))

(defun quail-add-package (package)
  (let ((pac (quail-package (car package))))
    (if pac
	(setcdr pac (cdr package))
      (setq quail-package-alist (cons package quail-package-alist)))))

;;;###autoload
(defun quail-use-package (name)
  "Set current quail package to NAME."
  (setq quail-current-package (quail-package name))
  (if (null quail-current-package)
      (error "No such quail-pacakge: %s" name))
  (if (null (quail-map))
      (load-library (quail-prompt)))
  (if (null (string= name (car quail-current-package)))
      (setq quail-current-package (quail-package name)))
  (setq-default quail-current-package quail-current-package))

(defconst quail-mode-map
  (let ((map (make-keymap)))
    (substitute-key-definition 'self-insert-command
			       'quail-self-insert-command
			       map global-map)
    map))

(or (assq 'quail-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(quail-mode " Quail") minor-mode-alist)))

(or (assq 'quail-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'quail-mode quail-mode-map) minor-mode-map-alist)))

(defconst quail-sub-mode-map
  (let ((map (copy-keymap quail-mode-map))
	(i ?0))
    (while (<= i ?9)
      (define-key map (char-to-string i) 'quail-self-insert-or-index)
      (setq i (1+ i)))
    (define-key map "\en" 'quail-next-candidate)
    (define-key map "\ep" 'quail-prev-candidate)
    (define-key map "\eN" 'quail-next-candidate-block)
    (define-key map "\eP" 'quail-prev-candidate-block)
    (define-key map "\ei" 'quail-completion)
    (define-key map "\e " 'quail-select-current)
    (define-key map "\eg" 'quail-cancel-current)
    (define-key map "\177" 'quail-backward-delete-char)
    map))

(defconst quail-sub-mode-rich-bindings
  '((">" . quail-next-candidate)
    ("<" . quail-prev-candidate)
    ("." . quail-next-candidate-block)
    ("," . quail-prev-candidate-block)
    (" " . quail-select-current)
    ))

(defun quail-define-package (name prompt &optional guidance doc key-bindings
				  nolearn deterministic translate layout
				  obsolete decode-map extra-mode-list)
  "Define NAME as a quail-package with initial quail map.
The second argument PROMPT is a string to be displayed as a name of
 minor mode when NAME is selected as current package of quail-mode.
Optional arguments are GUIDANCE, DOC, KEY-BINDINGS, NOLEARN, DETERMINISTIC,
TRANSLATE, LAYOUT, OBSOLETE, DECODE-MAP, EXTRA-MODE-LIST.
 GUIDANCE is an alist of key and correspoing string to be shown in echo area,
	t means shows list of candidates,
	nil means shows typed key.
 DOC is a document shown by quail-help command.
 KEY-BINDINGS is an additional key-bindings for control in quail-sub-mode,
	the value should be a list of cons of KEY and COMMAND.
 NOLEARN non-nil means not remembering a candidate seleceted last time.
 DETERMINISTIC non-nil means deterministic translation
	and NOLEARN is automatically set to t.
 TRANSLATE non-nil means input characters are mapped to	vt100 layout
	with quail-keyboard-translate-table.
 LAYOUT non-nil means translated keyboard layout to be displayed by quail-help.
 OBSOLETE is an obsolete argument, anything specified is ignored.
 DECODE-MAP non-nil means create decoding map.
 EXTRA-MODE-LIST is a list of symbols of minor-modes to be set.
The current quail-package is set to this package and following quail-defrule
 defines translation rules in it."
  (if deterministic (setq nolearn t))	;92.10.26 by T.Saneto
  (quail-add-package
   (list name prompt (make-sparse-keymap) guidance (or doc "") key-bindings
	 nolearn deterministic translate layout nil
	 (if decode-map '(0) nil)
	 extra-mode-list))
  (quail-use-package name)
  nil)

(defconst quail-keyboard-standard-table
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    nil))

(defvar quail-keyboard-translate-table
	quail-keyboard-standard-table
"List of QUAIL-TRANSLATE-TABLE (first element) and QUAIL-ASSOC-TABLE
(second element).
 QUAIL-TRANSLATE-TABLE is a string which represents the 'normalised'
layout of a particular keyboard.
 QUAIL-ASSOC-TABLE is an a-list which describes 'unordinary' key
locations.  A key location is given by a vector of the form
[x-position y-position shift-or-not].  For example, the '1' (one)
key's location is [0 0 0], and 'W' (capital letter W) is [1 1 1].  The
third element being 0 means unshifted, 1 means shifted.  If
QUAIL-ASSOC-TABLE is NIL, the first argument given to quail-defrule
will not be translated.")

(defconst quail-keyboard-translate-table-sun-type3
  '("\
1234567890-=\\\
qwertyuiop[]\
asdfghjkl;'`\
zxcvbnm,./\
!@#$%^&*()_+|\
QWERTYUIOP{}\
ASDFGHJKL:\"~\
ZXCVBNM<>?\
 "
    ((?` [13 0 0]) (?~ [13 0 1]))))

(defconst quail-keyboard-translate-table-sun-type4
  '("\
1234567890-=\\\
qwertyuiop[]\
asdfghjkl;'`\
zxcvbnm,./\
!@#$%^&*()_+|\
QWERTYUIOP{}\
ASDFGHJKL:\"~\
ZXCVBNM<>?\
 "
    ((?\\ [11 -1 0]) (?| [11 -1 1]))))

(defconst quail-keyboard-translate-table-sony-nwp-411a
  '("\
1234567890-=\\\
qwertyuiop[]\
asdfghjkl;'`\
zxcvbnm,./\
!@#$%^&*()_+|\
QWERTYUIOP{}\
ASDFGHJKL:\"~\
ZXCVBNM<>?\
 "
    nil))

(defconst quail-keyboard-translate-table-jis
  '("\
1234567890-^\\\
qwertyuiop@[\
asdfghjkl;:]\
zxcvbnm,./\
!\"#$%&'()_=`|\
QWERTYUIOP~{\
ASDFGHJKL+*}\
ZXCVBNM<>?\
 "
    ((?_ [10 3 1]))))

(defconst quail-keyboard-translate-table-fujitsu-sigma-230
  '("\
1234567890-^\\\
qwertyuiop@[\
asdfghjkl;:]\
zxcvbnm,./\
!\"#$%&'()_=~|\
QWERTYUIOP`{\
ASDFGHJKL+*}\
ZXCVBNM<>?\
 "
    ((?_ [10 3 1]))))

(defconst quail-keyboard-translate-table-ibm-at
  '("\
1234567890-=\\\
qwertyuiop[]\
asdfghjkl;'`\
zxcvbnm,./\
!@#$%^&*()_+|\
QWERTYUIOP{}\
ASDFGHJKL:\"~\
ZXCVBNM<>?\
 "
    ((?` [-1 0 0]) (?~ [-1 0 1]))))

(defconst quail-keyboard-translate-table-ibm-rt/pc
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    ((?` [-1 0 0]) (?~ [-1 0 1]) (?\\ [12 1 0]) (?| [12 1 1]))))

(defconst quail-keyboard-translate-table-decstation
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    ((?` [-1 3 0]) (?~ [-1 3 1]))))

(defconst quail-keyboard-translate-table-dynabook
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    ((?` [7 4 0]) (?~ [7 4 1]) (?\\ [1 4 0]) (?| [1 4 1]))))

(defconst quail-keyboard-translate-table-mac-mo110
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    ((?` [-1 0 0]) (?~ [-1 0 1]) (?\\ [8 4 0]) (?| [8 4 1]))))

(defconst quail-keyboard-translate-table-mac-mo116
  '("\
1234567890-=`\
qwertyuiop[]\
asdfghjkl;'\\\
zxcvbnm,./\
!@#$%^&*()_+~\
QWERTYUIOP{}\
ASDFGHJKL:\"|\
ZXCVBNM<>?\
 "
    ((?` [1 4 0]) (?~ [1 4 1]) (?\\ [7 4 0]) (?| [7 4 1]))))

(defun quail-defrule (key candidate &optional name)
  "Define KEY (string) to produce CANDIDATE in the current quail-map.
CANDIDATE is a string, a list of strings, a quail-map, a command, or a symbol.
 If the string contains only one character, the character code (integer) is
 also acceptable.
 The command should be a lisp function suitable for interactive
 calling (and called with no argument).
 The symbol's function definition should be a quail-map.
Optional arg PACKAGE indicates the package name to be used."
  (let* ((quail-current-package
	  (if name (quail-package name) quail-current-package))
	 (map (quail-map)))
    (if (not (keymapp map))
	(error "QUAIL: Invalid quail-map: %s" map)
      (if (or (keymapp candidate)	; another quail-map
	      (symbolp candidate))	; command or symbol
	  (define-key map key candidate)
	(if (integerp candidate)
	    (setq candidate (char-to-string candidate)))
	(if (and (setq map (lookup-key map key))
		 (keymapp map))
	    (if (vectorp (car (cdr map)))
		(define-key map "\0" candidate)
	      (setcdr map (cons (vector candidate) (cdr map))))
	  (define-key (quail-map) key (list 'keymap (vector candidate)))))
      (if (null (vectorp candidate))
	  (let ((decode-map (quail-decode-map))
		tbl)
	    (if decode-map
		(if (setq tbl (assoc candidate decode-map))
		    (setcdr tbl key)
		  (setcar (nthcdr 11 quail-current-package)
			  (cons (cons candidate key) decode-map))))))
      ))
  nil)

(defalias 'qd 'quail-defrule)
(defmacro qdv (key str) (list 'quail-defrule key (list 'vector str)))

(defun quail-message (msg)
  (or (eq (current-buffer) (window-buffer (minibuffer-window)))
      (message "%s" msg)))

(defun quail-select-package (name)
  "Select quail-package."
  (interactive
   (let* ((completion-ignore-case t)
	  (default (if quail-last-package
		       (car quail-last-package)))
	  (package-name (completing-read
			 (format "Quail Package (%s): " default)
			 quail-package-alist nil t nil)))
     (if (> (length package-name) 0) (list package-name) (list default))))
  (if (quail-package name)
      (progn
	(setq quail-last-package quail-current-package)
	(quail-use-package name)
	(if quail-mode
	    (progn (quail-exit-mode) (quail-mode))))))

;;;###autoload
(defun quail-mode (&optional arg)
  "Toggle Quail minor mode.
With arg, turn Quail mode on if and only if arg is positive.
The command key you can use in Quail mode depends on a quail package.
Try \\[describe-bindings] in quail-mode.
The description about the current quail package is shown by \\[quail-help]."
  (interactive "P")
  (setq quail-mode (if (null arg)
		       (null quail-mode)
		     (> (prefix-numeric-value arg) 0)))
  (if quail-mode
      (if quail-package-alist
	  (quail-enter-mode)
	(setq quail-mode nil)
	(error "QUAIL: No quail-package, one should be loaded in advance."))
    (quail-exit-mode))
  (force-mode-line-update 'all))

(defun quail-setup-guidance-buf ()
  (or (and (bufferp quail-guidance-buf) (buffer-name quail-guidance-buf))
      (setq quail-guidance-buf
	    (get-buffer-create " *Quail guide for minibuffer*")))
  (let ((prompt (quail-prompt))
	(curbuf (current-buffer)))
    (set-buffer quail-guidance-buf)
    (if (null (assq 'quail-mode mode-line-format))
	(setq mode-line-format
	      (cons '(quail-mode (mc-flag ("[" quail-mode-string "]")))
		    mode-line-format)))
    (setq quail-mode t
	  quail-mode-string prompt)
    (erase-buffer)
    (or (overlayp quail-overlay)
	(setq quail-overlay (make-overlay 1 1)))
    (set-buffer curbuf))
  (cond ((get-buffer-window quail-guidance-buf)
	 ;; `buf' is already shown in some window.
	 )
	((null (eq (current-buffer) (window-buffer (minibuffer-window))))
	 ;; We are in normal buffer, let's use minibuffer.
	 (set-window-buffer (minibuffer-window) quail-guidance-buf))
	(t
	 ;; Since we are in minibuffer, we can't use it.
	 ;; Let's find the bottom window.
	 (let ((window-min-height 1)
	       (win (frame-lowest-window (selected-frame)))
	       height)
	   (setq height (window-height win))
	   (if (>= height 4)
	       (setq win (split-window win (- height 2))))
	   (set-window-buffer win quail-guidance-buf)))))

(defun quail-delete-guidance-buf ()
  (if (and (bufferp quail-guidance-buf) (buffer-name quail-guidance-buf))
      (if (eq (current-buffer) (window-buffer (minibuffer-window)))
	  (let ((win (get-buffer-window quail-guidance-buf)))
	    (if win (delete-window win)))
	(set-window-buffer (minibuffer-window)
			   (format " *Minibuf-%d*" (minibuffer-depth))))))

(defun quail-enter-mode ()
  (or (eq (car (car minor-mode-map-alist)) 'quail-mode)
      (setq minor-mode-map-alist
	    (cons (cons 'quail-mode quail-mode-map) minor-mode-map-alist)))
  (quail-use-package (or (car quail-current-package)
			 (car (car quail-package-alist))))
  (setq quail-mode-string (quail-prompt))
  (if (null (assq 'quail-mode mode-line-format))
      (setq mode-line-format
	    (cons '(quail-mode (mc-flag ("[" quail-mode-string "]")))
		  mode-line-format)))
  (if (null (overlayp quail-overlay))
      (progn
	(setq quail-overlay (make-overlay (point) (point)))
	(overlay-put quail-overlay 'face quail-region-face)))
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'quail-reset-state nil t)
  (setq quail-keep-state nil)
  (if (quail-sub-mode-map)
      (progn
	(if (null (keymapp (quail-sub-mode-map)))
	    (let ((bindings (quail-sub-mode-map))
		  (map (copy-keymap quail-sub-mode-map)))
	      (while bindings
		(define-key map (car (car bindings)) (cdr (car bindings)))
		(setq bindings (cdr bindings)))
	      (setcar (nthcdr 5 quail-current-package) map)))))
  (let ((modes (quail-extra-mode-list))
	(i 0))
    (setq quail-previous-extra-mode-list (make-list (length modes) nil))
    (while modes
      (setcar (nthcdr i quail-previous-extra-mode-list)
	      (symbol-value (car modes)))
      (if (null (symbol-value (car modes)))
	  (funcall (car modes) 1))
      (setq i (1+ i) modes (cdr modes))))
  (quail-init-state)
  (quail-message (substitute-command-keys "\\[quail-help] for help."))
  (run-hooks 'quail-mode-hook))

(defun quail-exit-mode ()
  (interactive)
  (kill-local-variable 'post-command-hook)
  (delete-overlay quail-overlay)
  (quail-delete-guidance-buf)
  (let ((modes (quail-extra-mode-list))
	(i 0))
    (while modes
      (if (null (nth i quail-previous-extra-mode-list))
	  (funcall (car modes) -1))
      (setq i (1+ i) modes (cdr modes))))
  (setq quail-mode nil))

(defun quail-enter-sub-mode ()
  (setcdr (assoc 'quail-mode minor-mode-map-alist)
	  (or (quail-sub-mode-map) quail-sub-mode-map))
  (setq quail-sub-mode t))

(defun quail-exit-sub-mode ()
  (setcdr (assoc 'quail-mode minor-mode-map-alist) quail-mode-map)
  (setq quail-sub-mode nil))

(defun quail-reset-state ()
  (if quail-mode
      (if quail-keep-state
	  (setq quail-keep-state nil)
	(quail-init-state))))

(defun quail-init-state ()
  (if (overlayp quail-overlay)
      (move-overlay quail-overlay (point) (point))
    (setq quail-overlay (make-overlay (point) (point))))
  (setq quail-current-key nil
	quail-current-str nil)
  (if quail-sub-mode (quail-exit-sub-mode))
  (quail-setup-guidance-buf))

(defun quail-check-state ()
  (if (and (overlay-buffer quail-overlay)
	   (= (point) (overlay-end quail-overlay)))
      quail-current-key
    (quail-init-state)
    nil))

(defun quail-delete-region ()
  (delete-region (overlay-start quail-overlay) (overlay-end quail-overlay)))

(defun quail-insert (str)
  (quail-delete-region)
  (if visual-mode
      (if (stringp str)
	  (let ((l (string-to-char-list str))) ;93.4.30 by Takahashi N.
	    (while l
	      (visual-insert-1-char (car l))
	      (setq l (cdr l))))
	(visual-insert-1-char str))
    (insert str))
  (if (and auto-fill-function (> (current-column) fill-column))
      (run-hooks 'auto-fill-function))
  (let ((len (if (integerp str) (char-bytes str) (length str))))
    (move-overlay quail-overlay (- (point) len) (point)))
  (quail-show-guidance))

(defun quail-get-candidates (def)
  (setq def (car (cdr def)))
  (if (null (vectorp def))
      nil
    (let ((candidates (aref def 0)))
      (if (vectorp candidates)
	  (progn
	    (setq candidates
		  (cons 0 (string-to-char-list (aref candidates 0))))
	    (aset def 0 candidates))
	(if (and (listp candidates)
		 (stringp (car candidates)))
	    (progn
	      (setq candidates (cons 0 candidates))
	      (aset def 0 candidates))))
      candidates)))

(defun quail-get-candidate (def &optional nolearn)
  (let ((candidates (quail-get-candidates def)))
    (if candidates
	(if (not (listp candidates))	;93.1.17 by K.Handa
	    (if (integerp candidates)
		candidates
	      (if (and (symbolp candidates) (fboundp candidates))
		  (if (keymapp (symbol-function candidates))
		      (symbol-function candidates)
		    (funcall candidates))
		candidates))
	  (if nolearn
	      (rplaca candidates 0))
	  (nth (car candidates) (cdr candidates)))))) ;93.1.17 by K.Handa

(defun quail-show-guidance ()
  ;; At first, show dynamic list or current keyin string in echo area.
  (quail-setup-guidance-buf)
  (let ((guidance (quail-guidance)))
    (if (eq guidance t)
	(quail-list-dynamically)
      (save-excursion
	(let ((key quail-current-key))
	  (save-excursion
	    (set-buffer quail-guidance-buf)
	    (if (null guidance)
		(insert key)
	      (let ((i 0) (len (length key)) ch show)
		(while (< i len)
		  (setq ch (aref key i))
		  (setq show (cdr (assoc ch guidance)))
		  (insert (if show show (char-to-string ch)))
		  (setq i (1+ i))))))))))
  ;; Then highlight the current candidate string in *Completion* buffer if any.
  (if (and (quail-buffer-alive-p quail-completion-buf)
	   (get-buffer-window quail-completion-buf))
      (let ((buf (current-buffer))
	    (str (if (stringp quail-current-str)
		     quail-current-str
		   (if (numberp quail-current-str)
		       (char-to-string quail-current-str))))
	    (key quail-current-key))
	(select-window (get-buffer-window quail-completion-buf))
	(goto-char (point-min))
	(if (search-forward (concat " " key ":") nil t)
	    (if (and str (search-forward (concat "." str) nil t))
		(move-overlay quail-overlay (1+ (match-beginning 0)) (point))
	      (move-overlay quail-overlay (match-beginning 0) (point)))
	  (move-overlay quail-overlay 1 1))
	(select-window (get-buffer-window buf))
	)))

(defun quail-translate-char (ch)
  (let* ((str (car quail-keyboard-translate-table))
	 (len (length str))
	 (i 0))
    (while (and (< i len) (/= ch (aref str i)))
      (setq i (1+ i)))
    (if (= i len)
        (error "'%c' not found in quail-keyboard-translate-table" ch))
    (aref (car quail-keyboard-standard-table) i)))

(defun quail-select-current ()
  "Select the current candidate."
  (interactive "*")
  (quail-self-insert-command t))

(defun quail-self-insert-or-index ()
  "Select a character from the current 10 candidates by digit."
  (interactive "*")
  (quail-self-insert-command (- last-command-char ?0)))

(defun quail-lookup-key (key)
  (let ((def (lookup-key (quail-map) key)))
    (if (and (symbolp def) (fboundp def))
	(setq def (symbol-function def)))
    def))

(defun quail-self-insert-command (&optional arg)
  (interactive "*")
  (setq quail-keep-state t)
  (quail-check-state)
  (setq quail-last-char last-command-char)
  (let* ((ch (if (quail-translate)
		 (quail-translate-char quail-last-char)
	       quail-last-char))
	 (chstr (char-to-string ch))
	 (key (if quail-current-key
		  (format "%s%c" quail-current-key ch)
		chstr))
	 (def (quail-lookup-key key)))
    (cond ((keymapp def)
	   (setq quail-current-key key)
	   (setq quail-current-str (quail-get-candidate def (quail-nolearn)))
	   (quail-insert (or quail-current-str quail-current-key))
	   (if (and quail-current-str
		    (null (nthcdr 2 def))
		    (setq def (aref (nth 1 def) 0))
		    (or (null (listp def)) (= (length def) 2))
		    (null (and (>= (length quail-current-key) 3)
			       (lookup-key (quail-map) chstr))))
	       (quail-init-state)))
	  ((commandp def)
	   (setq quail-current-key key)
	   (if (keymapp (symbol-function def))
	       (progn
		 (setq quail-current-str nil)
		 (quail-insert quail-current-key))
	     (call-interactively def)))
	  ((and (numberp arg) quail-current-str (null (quail-deterministic)))
	   (quail-indexed-candidate arg))
	  ((eq arg t)
	   (quail-init-state))
	  (quail-current-key
	   (let ((len (length key)) str)
	     (if (and (>= len 4)
		      (keymapp (lookup-key (quail-map) (substring key -2)))
		      (setq def (quail-lookup-key (substring key 0 -2)))
		      (keymapp def)
		      (setq str (quail-get-candidate def (quail-nolearn))))
		 (progn
		   (quail-delete-region)
		   (insert str)
		   (quail-init-state)
		   (setq unread-command-events
			 (cons (aref key (- len 2))
			       (cons ch unread-command-events))))
	       (quail-init-state)
	       (quail-self-insert-command))))
	  (t
	   (quail-init-state)
	   (self-insert-command 1)))
    (run-hooks 'quail-self-insert-after-hook)
    )
  (if (and quail-current-key (null quail-sub-mode))
      (quail-enter-sub-mode))
  )

(defun quail-next-candidate ()
  "Select next candidate."
  (interactive)
  (setq quail-keep-state t)
  (if (and (quail-check-state) quail-current-str)
      (quail-select-candidate 1 t)
    (beep)))

(defun quail-prev-candidate ()
  "Select previous candidate."
  (interactive)
  (setq quail-keep-state t)
  (if (and (quail-check-state) quail-current-str)
      (quail-select-candidate -1 t)
    (beep)))

(defun quail-indexed-candidate (idx)
  (setq idx (if (= idx 0) 9 (1- idx)))
  (quail-select-candidate idx nil t)
  (quail-init-state))

(defun quail-next-candidate-block ()
  "Select candidate in next 10 alternatives."
  (interactive)
  (setq quail-keep-state t)
  (if (and (quail-check-state) quail-current-str)
      (quail-select-candidate 10 t t)
    (beep)))

(defun quail-prev-candidate-block ()
  "Select candidate in previous 10 alternatives."
  (interactive)
  (setq quail-keep-state t)
  (if (and (quail-check-state) quail-current-str)
      (quail-select-candidate -10 t t)
    (beep)))

(defun quail-select-candidate (idx &optional relative block)
  (let* ((def (quail-lookup-key quail-current-key))
	 (candidates (quail-get-candidates def)))
    (if (listp candidates)
	(let ((maxidx (- (length candidates) 2))
	      (current-idx (car candidates)))
	  (if relative
	      (setq idx (+ current-idx idx))
	    (if block
		(setq idx (+ (* (/ current-idx 10) 10) idx))))
	  (if block
	      (if (> idx maxidx)
		  (if (/= (/ maxidx 10) (/ current-idx 10))
		      (setq idx maxidx)
		    (beep)
		    (setq idx current-idx))
		(if (< idx 0) (progn (beep) (setq idx (+ idx 10)))))
	    (if (> idx maxidx) (setq idx 0)
	      (if (< idx 0) (setq idx maxidx))))
	  (rplaca candidates idx)
	  (setq quail-current-str (nth (car candidates) (cdr candidates)))
	  (quail-insert quail-current-str)))))

(defun quail-cancel-current ()
  "Cancel the current key sequence."
  (interactive)
  (quail-delete-region)
  (quail-init-state))

(defun quail-backward-delete-char (arg)
  "Delete characters backward in quail-mode."
  (interactive "*p")
  (if (= (length quail-current-key) 1)
      (progn
	(quail-delete-region)
	(quail-init-state))
    (setq quail-keep-state t)
    (setq quail-current-key (substring quail-current-key 0 -1))
    (let ((def (quail-lookup-key quail-current-key)))
      (setq quail-last-char
	    (aref quail-current-key (1- (length quail-current-key))))
      (setq quail-current-str
	    (quail-get-candidate def (quail-nolearn))) ;93.1.17 by K.Handa
      (quail-insert (or quail-current-str quail-current-key)))))

(defvar quail-work-str (make-string 128 0))

(defun quail-list-dynamically ()
  "Show list of candidates dynamically."
  (let* ((def (quail-lookup-key quail-current-key))
	 (candidates (quail-get-candidates def))
	 (key quail-current-key))
    (save-excursion
      (set-buffer quail-guidance-buf)
      (erase-buffer)
      (insert key)
      (if (or (nthcdr 2 def) (listp (car (cdr def))))
	  (let ((i ? ) (l (cdr def)))
	    (while (< i 127) (aset quail-work-str i 0) (setq i (1+ i)))
	    (while l
	      (if (listp (car l))
		  (aset quail-work-str (car (car l)) 1))
	      (setq l (cdr l)))
	    (insert "[")
	    (setq i ? )
	    (while (< i 127)
	      (if (= (aref quail-work-str i) 1)
		  (insert i))
	      (setq i (1+ i)))
	    (insert "]")))
      (if (consp candidates)
	  (let ((idx (car candidates))
		(maxidx (1+ (/ (1- (length (cdr candidates))) 10)))
		(num 0)
		p p1 p2 str)
	    (indent-to 10)
	    (insert (format "(%d/%d)" (1+ (/ idx 10)) maxidx))
	    (setq candidates (nthcdr (* (/ idx 10) 10) (cdr candidates)))
	    (while (and candidates (< num 10))
	      (setq num (1+ num))
	      (insert (format " %d." (if (< num 10) num 0)))
	      (setq p (point))
	      (insert (car candidates))
	      (if (= num (1+ (% idx 10)))
		  (setq p1 p p2 (point)))
	      (setq candidates (cdr candidates)))
	    (put-text-property p1 p2 'face quail-selection-face))))
    ))

(defun quail-display-buffer (buf)
  (if (get-buffer-window buf)
      nil
    (let ((curwin (selected-window))
	  (height 0)
	  largest)
      (walk-windows '(lambda (win)
		       (if (and (null (eq curwin win ))
				(> (window-height win) height))
			   (setq height (window-height win)
				 largest win))))
      (set-window-buffer (if (> height (/ (window-height curwin) 2))
			     largest
			   (split-window curwin))
			 buf)
      )))

(defun quail-completion ()
  "Show list of candidates."
  (interactive)
  (let ((def (quail-lookup-key quail-current-key))
	(key quail-current-key))
    (if (not (keymapp def))
	(quail-message "No macth.")
      (setq quail-keep-state t)
      (save-excursion
	(if (not (quail-buffer-alive-p quail-completion-buf))
	    (setq quail-completion-buf
		  (get-buffer-create "*Completions*")))
	(set-buffer quail-completion-buf)
	(erase-buffer)
	(setq quail-overlay (make-overlay 1 1))
	(overlay-put quail-overlay 'face quail-selection-face)
	(insert "Current candidates:\n")
	(quail-completion-list key def 1)
	(quail-display-buffer (current-buffer)))
      (quail-show-guidance))))

(defun quail-completion-list (key def indent)
  (let ((candidates (quail-get-candidates def)))
    (indent-to indent)
    (insert key ":")
    (if candidates
	(quail-candidate-with-indent
	 (if (consp candidates) (cdr candidates) candidates)
	 key)
      (insert " none\n"))
    (setq indent (+ indent 2))
    (setq def (cdr def))
    (while def
      (if (listp (vectorp (car def)))
	  (let ((map (cdr (car def))))
	    (if (symbolp map) (setq map (symbol-function map)))
	    (quail-completion-list (format "%s%c" key (car (car def)))
				   map indent)))
      (setq def (cdr def)))))

(defun quail-candidate-with-indent (candidates key)
  (if (consp candidates)
      (let ((clm (current-column))
	    (i 0)
	    num)
	(while candidates
	  (if (= (% i 10) 0) (insert (format "(%d)" (1+ (/ i 10)))))
	  (insert " " (if (= (% i 10) 9) "0" (+ ?1 (% i 10))) ".")
	  (insert (if (stringp (car candidates))
		      (car candidates)
		    (char-to-string (car candidates))))
	  (setq i (1+ i))
	  (setq candidates (cdr candidates))
	  (if (and candidates (= (% i 10) 0))
	      (progn
		(insert ?\n)
		(indent-to clm)))))
    (if (and (symbolp candidates) (fboundp candidates))
	(insert " (1) 0."
		(let ((quail-current-key key)) (funcall candidates)))
      (insert " (1) 0." candidates)))
  (insert ?\n))

(defun quail-help ()
  "Show brief description of the current quail-pacakge."
  (interactive)
  (setq quail-keep-state t)
  (let ((package quail-current-package)
	(buf (get-buffer "*Quail-Help*"))
	(first t))
    (save-excursion
      (set-buffer (or buf (get-buffer-create "*Quail-Help*")))
      (if (and buf (eq package quail-current-package))
	  (setq first nil)
	(setq buf (current-buffer))
	(setq quail-current-package package)
	(erase-buffer)
	(insert (quail-document))
	(if (quail-layout) (quail-show-layout))
	(insert "\n--- Key bindinds ---\n")
	(let ((map (or (quail-sub-mode-map) quail-sub-mode-map))
	      (i 0))
	  (while (< i 256)
	    (quail-describe-binding map i)
	    (setq i (1+ i)))
	  (setq map (lookup-key map (vector meta-prefix-char)))
	  (setq i 0)
	  (while (< i 256)
	    (quail-describe-binding map i 'meta)
	    (setq i (1+ i)))
	  (goto-char (point-min)))))
    (let ((win (get-buffer-window buf)))
      (if win
	  (save-excursion
	    (set-buffer buf)
	    (if (null first)
		(if (> (point-max) (window-end win))
		    (set-window-start win (window-end win))
		  (if (< (point-min) (window-start win))
		      (set-window-start win 1)))))
	(quail-display-buffer buf))
      (let (up down)
	(save-excursion
	  (sit-for 0)
	  (set-buffer buf)
	  (if (> (point-max) (window-end win))
	      (setq up t)
	    (if (< (point-min) (window-start win))
		(setq down t))))
	(if up
	    (quail-message
	     (substitute-command-keys
	      "\\[quail-help] to scroll up *Quail-Help* buffer."))
	  (if down
	      (quail-message
	       (substitute-command-keys
		"\\[quail-help] to show top of *Quail-Help* buffer."))))))
    ))

(defun quail-show-layout ()
  (let* ((xoff 10)
	 (yoff 3)
	 (space 4)
	 (p (point))
	 (i 0)
	 (str (car quail-keyboard-translate-table))
	 (len (length str))
	 (alist (car (cdr quail-keyboard-translate-table)))
	 pos x y ch ch1 kmp)
    (insert "
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
")
    (save-excursion
      (while (< i len)
	(goto-char p)
	(setq ch (aref str i))
	(if (setq pos (car (cdr (assoc ch alist))))
	    (progn (forward-line (+ yoff (aref pos 1)))
		   (forward-char (+ xoff (* space (aref pos 0))
				    (aref pos 1) (aref pos 2))))
	  (cond
	   ((< i 13) (setq x i y 0)) ; unshifted, 1st row
	   ((< i 25) (setq x (- i 13) y 1)) ; unshifted, 2nd row
	   ((< i 37) (setq x (- i 25) y 2)) ; unshifted, 3rd row
	   ((< i 47) (setq x (- i 37) y 3)) ; unshifted, 4th row
	   ((< i 60) (setq x (- i 47) y 0)) ; shifted, 1st row
	   ((< i 72) (setq x (- i 60) y 1)) ; shifted, 2nd row
	   ((< i 84) (setq x (- i 72) y 2)) ; shifted, 3rd row
	   ((< i 94) (setq x (- i 84) y 3)) ; shifted, 4th row
	   (t (setq x (- i 90) y 4))) ; space, bottom row
	  (forward-line (+ yoff y))
	  (forward-char (+ xoff (* space x) y (if (< i 47) 0 1))))
	(delete-char 1)
	(if (quail-translate)
	    (setq ch (quail-translate-char ch)))
	(setq ch1
	  (and (setq kmp (quail-lookup-key (char-to-string ch)))
	       (quail-get-candidate kmp (quail-nolearn))))
	(insert (if ch1 ch1 ch))
	(setq i (1+ i))))))

(defun quail-describe-binding (map i &optional meta)
  (let ((cmd (lookup-key map (vector i))))
    (if (and (symbolp cmd) (fboundp cmd)
	     (not (memq cmd '(quail-self-insert-command
			      quail-self-insert-or-index))))
	(progn
	  (if meta (insert "ESC "))
	  (insert (single-key-description i) ":")
	  (indent-to-column 8)
	  (insert (documentation cmd) "\n")))))

;;;###autoload
(global-set-key "\C-]" 'quail-mode)

(define-key mule-keymap "M" 'quail-select-package)

;; For byte-compiler
(put 'quail-defrule 'byte-hunk-handler 'eval)
(put 'qdv 'byte-hunk-handler 'eval)
(put 'qd 'byte-hunk-handler 'eval)
(put 'quail-define-package 'byte-hunk-handler 'eval)

(defun quail-setup-current-package ()
  ;; Do nothing while loading non-compiled file.
  )

(defun quail-setup-current-package-handler (ignore)
  (list 'quail-add-package (list 'quote quail-current-package)))

(put 'quail-setup-current-package 'byte-hunk-handler
     'quail-setup-current-package-handler)

;;
(provide 'quail)
