;;; delbackspace.el --- rebind backspace and delete to be correct

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing.
;;
;; Author: Tibor Polgar??
;; Maintainer: Ben Wing <wing@666.com>
;; Keywords: terminals
;;

;;; Synched up with: Not in FSF.
;;; #### should port FSF pc-bindings.el and nuke this.

;; this is a hack that will have to do until function-key-map is implemented
(defconst delbackspace-backspace
  (if (not (eq 'tty (device-type (selected-device))))
      'backspace
    "\C-h"))

(global-unset-key delbackspace-backspace)
(local-unset-key delbackspace-backspace)
(global-unset-key [(control delete)])
(global-unset-key [(meta delete)])
(global-unset-key [(meta shift delete)])
(global-unset-key [(control backspace)])
(global-unset-key [(meta backspace)])
(global-unset-key [(meta shift backspace)])

(if (eq 'tty (device-type (selected-device)))
    (progn
      (setq help-char '(meta h))
      (define-key global-map "\M-h" 'help-command)))

(define-key key-translation-map 'delete 'delbackspace-compute-delete-value)
(define-key global-map 'intercepted_delete 'delete-char)

(defun delbackspace-compute-delete-value (prompt)
  (let ((orig-binding (key-binding 'delete)))
    (if (memq orig-binding '(delete-backward-char
			     backward-delete-char
			     backward-delete-char-untabify
			     c-electric-delete
			     c++-electric-delete
			     cperl-electric-backspace))
	[intercepted_delete]
      [delete])))

(define-key key-translation-map delbackspace-backspace [delete])

;; (ctrl) backspace to beginning of line
(global-set-key [(control backspace)] 'backward-kill-line)

;; (meta) backspace word before cursor
(define-key key-translation-map [(meta backspace)]
  [intercepted_meta_backspace])
(define-key global-map 'intercepted_meta_backspace 'backward-kill-word)

;; (alt) backspace sentence before cursor; backspace sexp before cursor
;; in language modes
(define-key key-translation-map [(alt backspace)]
  [intercepted_alt_backspace])
(define-key global-map 'intercepted_alt_backspace 'backward-kill-sentence)
(mapcar #'(lambda (map)
	    (define-key map [(alt backspace)] 'backward-kill-sexp))
	(list
	 c-mode-map c++-mode-map objc-mode-map java-mode-map
	 emacs-lisp-mode-map lisp-mode-map minibuffer-local-map))

;; (meta shift) backspace paragraph before cursor
(global-set-key [(meta shift backspace)] 'backward-kill-paragraph)

;; (alt) delete rest of line  (erase EOF)
(global-set-key [(control delete)] 'kill-line)

;; (meta) delete word before cursor
(define-key key-translation-map [(meta delete)]
  [intercepted_meta_delete])
(define-key global-map 'intercepted_meta_delete 'kill-word)

;; (alt) delete sentence forward; delete sexp forward
;; in language modes
(define-key key-translation-map [(alt delete)]
  [intercepted_alt_delete])
(define-key global-map 'intercepted_alt_delete 'kill-sentence)
(mapcar #'(lambda (map)
	    (define-key map [(alt delete)] 'kill-sexp))
	(list
	 c-mode-map c++-mode-map objc-mode-map java-mode-map
	 emacs-lisp-mode-map lisp-mode-map minibuffer-local-map))

;; (meta shift) delete next paragraph
(global-set-key [(meta shift delete)] 'kill-paragraph)
