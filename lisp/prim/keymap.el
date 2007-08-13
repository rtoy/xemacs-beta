;; Keymap functions.
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

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

;;; Synched up with: FSF 19.28.
;;; Note: FSF does not have a file keymap.el.  This stuff is
;;; in keymap.c.

;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun undefined ()
  (interactive)
  (ding))

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (substitute-key-definition 'self-insert-command 'undefined map global-map)
  (or nodigits
      (let ((string (make-string 1 ?0)))
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(while (<= (aref string 0) ?9)
	  (define-key map string 'digit-argument)
	  (aset string 0 (1+ (aref string 0)))))))

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Prefix keymaps are checked recursively.  If optional fourth argument OLDMAP
is specified, we redefine in KEYMAP as NEWDEF those chars which are defined
as OLDDEF in OLDMAP, unless that keybinding is already present in keymap.
If optional fifth argument PREFIX is defined, then only those occurrences of
OLDDEF found in keymaps accessible through the keymap bound to PREFIX in
KEYMAP are redefined.  See also `accessible-keymaps'."
  (let ((maps (accessible-keymaps (or oldmap keymap) prefix))
	(shadowing (not (null oldmap)))
	prefix map)
    (while maps
      (setq prefix (car (car maps))
	    map (cdr (car maps))
	    maps (cdr maps))
      ;; Substitute in this keymap
      (map-keymap #'(lambda (key binding)
		      (if (eq binding olddef)
			  ;; The new bindings always go in KEYMAP even if we
			  ;; found them in OLDMAP or one of it's children.
			  ;; If KEYMAP will be shadowing OLDMAP, then do not
			  ;; redefine the key if there is another binding
			  ;; in KEYMAP that will shadow OLDDEF.
			  (or (and shadowing
				   (lookup-key keymap key))
			      ;; define-key will give an error if a prefix
			      ;; of the key is already defined.  Otherwise
			      ;; it will define the key in the map. 
			      ;; #### - Perhaps this should be protected?
			      (define-key
				keymap
				(vconcat prefix (list key))
				newdef))))
		  map)
      )))

;; from Bill Dubuque <wgd@martigny.ai.mit.edu>
(defun insert-key-binding (key)         ; modeled after describe-key
  (interactive "kInsert command bound to key: ")
  (let (defn)
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key))
             (or (misc-user-event-p (aref key 0))
                 (eq (car-safe (aref key 0)) 'menu-selection)))
        (let ((event (aref key 0)))
          (setq defn (if (eventp event)
                         (list (event-function event) (event-object event))
                       (cdr event)))
          (if (eq (car defn) 'eval)
              (setq defn (` (lambda ()
                              (interactive)
                              (, (car (cdr defn)))))))
          (if (eq (car-safe defn) 'call-interactively)
              (setq defn (car (cdr defn))))
          (if (and (consp defn) (null (cdr defn)))
              (setq defn (car defn))))
      (setq defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (error "%s is undefined" (key-description key))
      (if (or (stringp defn) (vectorp defn))
          (setq defn (key-binding defn))) ;; a keyboard macro
      (insert (format "%s" defn)))))

;; from Bill Dubuque <wgd@martigny.ai.mit.edu>
(defun read-command-or-command-sexp (prompt)
  "Read a command symbol or command sexp.
A command sexp is wrapped in an interactive lambda if needed.
Prompts with PROMPT."
  ;; Todo: it would be better if we could reject symbols that are not
  ;; commandp (as does 'read-command') but that is not easy to do
  ;; because we must supply arg4 = require-match = nil for sexp case.
  (let ((result (car (read-from-string
                      (completing-read prompt obarray 'commandp)))))
    (if (and (consp result)
             (not (eq (car result) 'lambda)))
        (` (lambda ()
             (interactive)
             (, result)))
      result)))

(defun local-key-binding (keys)
  "Return the binding for command KEYS in current local keymap only.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (let ((map (current-local-map)))
    (if map
        (lookup-key map keys)
        nil)))

(defun global-key-binding (keys)
  "Return the binding for command KEYS in current global keymap only.
KEYS is a string or vector of events, a sequence of keystrokes.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (lookup-key (current-global-map) keys))

;; from Bill Dubuque <wgd@martigny.ai.mit.edu>
(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
Note that if KEY has a local binding in the current buffer
that local binding will continue to shadow any global binding."
  ;;(interactive "KSet key globally: \nCSet key %s to command: ")
  (interactive (list (setq key (read-key-sequence "Set key globally: "))
                     ;; Command sexps are allowed here so that this arg
                     ;; may be supplied interactively via insert-key-binding.
                     (read-command-or-command-sexp
                       (format "Set key %s to command: "
                               (key-description key)))))
  (define-key (current-global-map) key command)
  nil)

;; from Bill Dubuque <wgd@martigny.ai.mit.edu>
(defun local-set-key (key command)
  "Give KEY a local binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding goes in the current buffer's local map,
which is shared with other buffers in the same major mode."
  ;;(interactive "KSet key locally: \nCSet key %s locally to command: ")
  (interactive (list (setq key (read-key-sequence "Set key locally: "))
                     ;; Command sexps are allowed here so that this arg
                     ;; may be supplied interactively via insert-key-binding.
                     (read-command-or-command-sexp
                       (format "Set key %s locally to command: "
                               (key-description key)))))
  (if (null (current-local-map))
      (use-local-map (make-sparse-keymap)))
  (define-key (current-local-map) key command)
  nil)

(defun global-unset-key (key)
  "Remove global binding of KEY.
KEY is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
KEY is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (define-key (current-local-map) key nil)))


;; Yet more RMS brain-death.
(defun minor-mode-key-binding (key &optional accept-default)
  "Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is the
the symbol which names the minor mode binding KEY, and BINDING is
KEY's definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn't include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this."
  (let ((tail minor-mode-map-alist)
        a s v)
    (while tail
      (setq a (car tail)
            tail (cdr tail))
      (and (consp a)
           (symbolp (setq s (car a)))
           (boundp s)
           (symbol-value s)
           ;; indirect-function deals with autoloadable keymaps
           (setq v (indirect-function (cdr a)))
           (setq v (lookup-key v key accept-default))
           ;; Terminate loop, with v set to non-nil value
           (setq tail nil)))
    v))
    

(defun current-minor-mode-maps ()
  "Return a list of keymaps for the minor modes of the current buffer."
  (let ((l '())
        (tail minor-mode-map-alist)
        a s v)
    (while tail
      (setq a (car tail)
            tail (cdr tail))
      (and (consp a)
           (symbolp (setq s (car a)))
           (boundp s)
           (symbol-value s)
           ;; indirect-function deals with autoloadable keymaps
           (setq v (indirect-function (cdr a)))
           (setq l (cons v l))))
    (nreverse l)))


;;#### What a crock
(defun define-prefix-command (name &optional mapvar)
  "Define COMMAND as a prefix command.
A new sparse keymap is stored as COMMAND's function definition.
If second optional argument MAPVAR is not specified,
 COMMAND's value (as well as its function definition) is set to the keymap.
If a second optional argument MAPVAR is given and is not `t',
  the map is stored as its value.
Regardless of MAPVAR, COMMAND's function-value is always set to the keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map name)
    (fset name map)
    (cond ((not mapvar)
           (set name map))
          ((eq mapvar 't)
           )
          (t
           (set mapvar map)))
    name))


;;; Converting vectors of events to a read-equivalent form.
;;; This is used both by call-interactively (for the command history)
;;; and by macros.el (for saving keyboard macros to a file).

(defun events-to-keys (events &optional no-mice)
 "Given a vector of event objects, returns a vector of key descriptors,
or a string (if they all fit in the ASCII range).
Optional arg NO-MICE means that button events are not allowed."
 (if (and events (symbolp events)) (setq events (vector events)))
 (cond ((stringp events)
        events)
       ((not (vectorp events))
        (signal 'wrong-type-argument (list 'vectorp events)))
       ((let* ((length (length events))
               (string (make-string length 0))
               c ce
               (i 0))
          (while (< i length)
            (setq ce (aref events i))
            (or (eventp ce) (setq ce (character-to-event ce)))
            ;; Normalize `c' to `?c' and `(control k)' to `?\C-k'
            ;; By passing t for the `allow-meta' arg we could get kbd macros
            ;; with meta in them to translate to the string form instead of
            ;; the list/symbol form; but I expect that would cause confusion,
            ;; so let's use the list/symbol form whenever there's 
            ;; any ambiguity.
            (setq c (event-to-character ce))
            (if (and c
                     character-set-property
                     (key-press-event-p ce))
                (cond ((symbolp (event-key ce))
                       (if (get (event-key ce) character-set-property)
                           ;; Don't use a string for `backspace' and `tab' to
                           ;;  avoid that unpleasant little ambiguity.
                           (setq c nil)))
                      ((and (= (event-modifier-bits ce) 1) ;control
                            (integerp (event-key ce)))
                       (let* ((te (character-to-event c)))
                         (if (and (symbolp (event-key te))
                                  (get (event-key te) character-set-property))
                             ;; Don't "normalize" (control i) to tab
                             ;;  to avoid the ambiguity in the other direction
                             (setq c nil))
                         (deallocate-event te)))))
            (if c
                (aset string i c)
                (setq i length string nil))
            (setq i (1+ i)))
          string))
       (t
        (let* ((length (length events))
               (new (copy-sequence events))
               event mods key
               (i 0))
          (while (< i length)
            (setq event (aref events i))
            (cond ((key-press-event-p event)
                   (setq mods (event-modifiers event)
                         key (event-key event))
                   (if (numberp key)
                       (setq key (intern (make-string 1 key))))
                   (aset new i (if mods
                                   (nconc mods (cons key nil))
                                   key)))
                  ((misc-user-event-p event)
                   (aset new i (list 'menu-selection
                                     (event-function event)
                                     (event-object event))))
                  ((or (button-press-event-p event)
                       (button-release-event-p event))
                   (if no-mice
                       (error 
                         "Mouse events can't be saved in keyboard macros."))
                   (setq mods (event-modifiers event)
                         key (intern (concat "button"
                                             (event-button event)
                                             (if (button-release-event-p event)
                                                 "up" ""))))
                   (aset new i (if mods
                                   (nconc mods (cons key nil))
                                   key)))
                  ((or (and event (symbolp event))
                       (and (consp event) (symbolp (car event))))
                   (aset new i event))
                  (t
                   (signal 'wrong-type-argument (list 'eventp event))))
            (setq i (1+ i)))
          new))))

;FSFmacs ####
;;; Support keyboard commands to turn on various modifiers.
;
;;; These functions -- which are not commands -- each add one modifier
;;; to the following event.
;
;(defun event-apply-alt-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'alt 22 "A-")))
;(defun event-apply-super-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'super 23 "s-")))
;(defun event-apply-hyper-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'hyper 24 "H-")))
;(defun event-apply-shift-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'shift 25 "S-")))
;(defun event-apply-control-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'control 26 "C-")))
;(defun event-apply-meta-modifier (ignore-prompt)
;  (vector (event-apply-modifier (read-event) 'meta 27 "M-")))
;
;(defun event-apply-modifier (event symbol lshiftby prefix)
;  "Apply a modifier flag to event EVENT.
;SYMBOL is the name of this modifier, as a symbol.
;LSHIFTBY is the numeric value of this modifier, in keyboard events.
;PREFIX is the string that represents this modifier in an event type symbol."
;  (if (numberp event)
;      (cond ((eq symbol 'control)
;	      (if (and (<= (downcase event) ?z)
;		       (>= (downcase event) ?a))
;		  (- (downcase event) ?a -1)
;		(if (and (<= (downcase event) ?Z)
;			 (>= (downcase event) ?A))
;		    (- (downcase event) ?A -1)
;		  (logior (lsh 1 lshiftby) event))))
;	     ((eq symbol 'shift)
;	      (if (and (<= (downcase event) ?z)
;		       (>= (downcase event) ?a))
;		  (upcase event)
;		(logior (lsh 1 lshiftby) event)))
;	     (t
;	      (logior (lsh 1 lshiftby) event)))
;    (if (memq symbol (event-modifiers event))
;	 event
;      (let ((event-type (if (symbolp event) event (car event))))
;	 (setq event-type (intern (concat prefix (symbol-name event-type))))
;	 (if (symbolp event)
;	     event-type
;	   (cons event-type (cdr event)))))))
;
;(define-key function-key-map [?\C-x ?@ ?h] 'event-apply-hyper-modifier)
;(define-key function-key-map [?\C-x ?@ ?s] 'event-apply-super-modifier)
;(define-key function-key-map [?\C-x ?@ ?m] 'event-apply-meta-modifier)
;(define-key function-key-map [?\C-x ?@ ?a] 'event-apply-alt-modifier)
;(define-key function-key-map [?\C-x ?@ ?S] 'event-apply-shift-modifier)
;(define-key function-key-map [?\C-x ?@ ?c] 'event-apply-control-modifier)
