;;; mule-keyboard.el --- Direct input of multilingual chars from keyboard.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; 92.3.5   created for Mule Ver.0.9.0 by K.Handa <handa@etl.go.jp>

;;;###autoload
(defvar keyboard-allow-latin-input nil
  "If non-nil, \"ESC , Fe\" and \"ESC - Fe\" are used for inputting
Latin characters.")

;; common global variables of internal use
(defvar _keyboard-first-byte_ nil
  "Character buffer for the first byte of two-byte character.")
(defvar _keyboard-SS2_ nil
  "Flag to indicate Single Shift SS2.")
(defvar _keyboard-SS3_ nil
  "Flag to indicate Single Shift SS3.")
(defvar _keyboard-saved-local-map_ nil
  "Saved local keymap.")
(defvar _saved-local-map-single-shift_ nil
  "Saved single shift local map.")

(defvar _current-g0_ 0)
(defvar _current-g1_ nil)
(defvar _current-g2_ nil)
(defvar _current-g3_ nil)

(defconst local-map-iso nil
  "Local keymap used while inputing ISO2022 code directly.")
(defconst local-map-shift-jis nil
  "Local keymap used while inputing Shift-JIS code directly.")
(defconst local-map-big5 nil
  "Local keymap used while inputing Big5 code directly.")

(defconst esc-dol-map nil "Keys to designate 94n or 96n charset.")
(defconst esc-openpar-map nil "Keys to designate 94 charset to GL.")
(defconst esc-closepar-map nil "Keys to designate 94 charset to GR.")
(defconst esc-comma-map nil "Keys to designate 96 charset to GL.")
(defconst esc-minus-map nil "Keys to designate 96 charset to GR.")
(defconst esc-dol-openpar-map nil "Keys to designate 94n charset to GL.")
(defconst esc-dol-closepar-map nil "Keys to designate 94n charset to GR.")
(defconst esc-dol-comma-map nil "Keys to designate 96n charset to GL.")
(defconst esc-dol-minus-map nil "Keys to designate 96n charset to GR.")

;;;###autoload
(defun set-keyboard-coding-system (codesys)
  "Set variable keyboard-coding-system to CODESYS and modify keymap for it."
  (interactive "zKeyboard-coding-system: ")
  (let ((type (coding-system-type codesys)))
    (cond ((eq type 'shift-jis)
	   (set-keyboard-coding-system-shift-jis))
	  ((eq type 'iso2022)
	   (set-keyboard-coding-system-iso2022 codesys))
	  ((eq type 'big5)
	   (set-keyboard-coding-system-big5))
	  (t
	   (error "Direct input of code %s is not supported." codesys)))))

(defsubst keyboard-define-key (map key command)
  (define-key map (char-to-string key) command t))

(defun keyboard-set-input-mode (value)
  (let ((mode (current-input-mode)))
    ;; current-input-mode returns (INTERRUPT FLOW META QUIT-CHAR)
    ;; set META to value.
    (setcar (nthcdr 2 mode) value)
    (apply (function set-input-mode) mode)))


(defun keyboard-select-keymap (&rest maps)
  (or (nth (get-code-type keyboard-coding-system) maps)
      (error "invalid keyboard-coding-system")))

(defun keyboard-self-insert-do-insert (char)
  (self-insert-internal char)
  (check-auto-fill))

;; ### I think this is the right function to put this on... must check further
;; wire us into pending-delete
(put 'keyboard-self-insert-do-insert 'pending-delete t)

(defun keyboard-use-local-map-do-insert (map)
  (use-local-map map))

(defun keyboard-current-local-map-do-insert ()
  (current-local-map))

(defun keyboard-local-map-do-insert ()
  (keyboard-select-keymap nil local-map-shift-jis local-map-iso local-map-big5))


(defconst keyboard-self-insert-function 
  (function keyboard-self-insert-do-insert))

(defconst keyboard-use-local-map-function 
  (function keyboard-use-local-map-do-insert))

(defconst keyboard-current-local-map-function 
  (function keyboard-current-local-map-do-insert))

(defconst keyboard-local-map-function 
  (function keyboard-local-map-do-insert))

(defun keyboard-self-insert (char)
  (funcall keyboard-self-insert-function char))

(defun keyboard-current-local-map ()
  (funcall keyboard-current-local-map-function))

(defun keyboard-use-local-map (map)
  (funcall keyboard-use-local-map-function map))

(defun keyboard-local-map ()
  (funcall keyboard-local-map-function))


(defun keyboard-reset-state ()
  (setq _keyboard-first-byte_ nil
	_keyboard-SS2_ nil
	_keyboard-SS3_ nil))

(defun keyboard-define-global-map-iso (map)
  (let ((i 160))
    (while (< i 256)
      (keyboard-define-key map i 'self-insert-iso)
      (setq i (1+ i))))
  (define-key map "\216" 'keyboard-SS2 t)
  (define-key map "\217" 'keyboard-SS3 t)
  (define-key map "\e(" 'esc-openpar-prefix)
  (define-key map "\e)" 'esc-closepar-prefix)
  (if keyboard-allow-latin-input
      (progn
	(define-key map "\e," 'esc-comma-prefix)
	(define-key map "\e-" 'esc-minus-prefix)))
  (define-key map "\e$" 'esc-dol-prefix))

(defun keyboard-define-local-map-iso (map)
  (let ((i 33))
    (while (< i 127)
      (keyboard-define-key map i 'self-insert-iso)
      (setq i (1+ i)))))

(defun set-keyboard-coding-system-iso2022 (code)
  (setq _current-g0_ (coding-system-charset code 0))
  (setq _current-g1_ (coding-system-charset code 1))
  (setq _current-g2_ (coding-system-charset code 2))
  (setq _current-g3_ (coding-system-charset code 3))
  (if (null _current-g1_)
      (keyboard-set-input-mode t)	; enable Meta-key
    (keyboard-set-input-mode 0))	; enable 8bit input as chars.
  (let (i)
    (setq i 160)
    (while (< i 256)
      (keyboard-define-key global-map i 'self-insert-iso)
      (setq i (1+ i))))
  (if local-map-iso nil
    (setq local-map-iso (make-keymap))
    (let (i map)
      (setq i 33)
      (while (< i 127)
	(keyboard-define-key local-map-iso i 'self-insert-iso)
	(setq i (1+ i)))
      (setq map (current-global-map))
      (setq i 161)
      (while (< i 255)
	(keyboard-define-key map i 'self-insert-iso)
	(setq i (1+ i))))
    (define-key local-map-iso "\C-g" 'mule-keyboard-quit))
  (if esc-dol-map nil
    (setq esc-dol-map (make-keymap)
	  esc-openpar-map (make-keymap)
	  esc-closepar-map (make-keymap)
	  esc-comma-map (make-keymap)
	  esc-minus-map (make-keymap)
	  esc-dol-openpar-map (make-keymap)
	  esc-dol-closepar-map (make-keymap)
	  esc-dol-comma-map (make-keymap)
	  esc-dol-minus-map (make-keymap))
    (fset 'esc-dol-prefix esc-dol-map)
    (fset 'esc-openpar-prefix esc-openpar-map)
    (fset 'esc-closepar-prefix esc-closepar-map)
    (fset 'esc-comma-prefix esc-comma-map)
    (fset 'esc-minus-prefix esc-minus-map)
    (fset 'esc-dol-openpar-prefix esc-dol-openpar-map)
    (fset 'esc-dol-closepar-prefix esc-dol-closepar-map)
    (fset 'esc-dol-comma-prefix esc-dol-comma-map)
    (fset 'esc-dol-minus-prefix esc-dol-minus-map)
    (define-key esc-dol-map "(" 'esc-dol-openpar-prefix)
    (define-key esc-dol-map ")" 'esc-dol-closepar-prefix)
    (define-key esc-dol-map "," 'esc-dol-comma-prefix)
    (define-key esc-dol-map "-" 'esc-dol-minus-prefix)
    (let (i)
      (setq i ?0)
      (while (< i ?`)
	(keyboard-define-key esc-openpar-map i 'keyboard-designate-94-GL)
	(keyboard-define-key esc-closepar-map i 'keyboard-designate-94-GR)
	(keyboard-define-key esc-comma-map i 'keyboard-designate-96-GL)
	(keyboard-define-key esc-minus-map i 'keyboard-designate-96-GR)
	(keyboard-define-key esc-dol-map i 'keyboard-designate-94n-GL)
	(keyboard-define-key esc-dol-openpar-map i 'keyboard-designate-94n-GL)
	(keyboard-define-key esc-dol-closepar-map i 'keyboard-designate-94n-GR)
	(keyboard-define-key esc-dol-comma-map i 'keyboard-designate-96n-GL)
	(keyboard-define-key esc-dol-minus-map i 'keyboard-designate-96n-GR)
	(setq i (1+ i)))))
  (define-key global-map "\216" 'keyboard-SS2 t)
  (define-key global-map "\217" 'keyboard-SS3 t)
  (define-key esc-map "(" 'esc-openpar-prefix)
  (define-key esc-map ")" 'esc-closepar-prefix)
  (if keyboard-allow-latin-input
      (progn
	(define-key esc-map "," 'esc-comma-prefix)
	(define-key esc-map "-" 'esc-minus-prefix)))
  (define-key esc-map "$" 'esc-dol-prefix)
  (keyboard-reset-state)
  (setq keyboard-coding-system code)
  )

(defun mule-keyboard-quit ()
  (interactive)
  (keyboard-reset-state)
  (if _keyboard-saved-local-map_
      (keyboard-use-local-map _keyboard-saved-local-map_))
  (keyboard-quit))

(defun keyboard-change-local-map-for-iso ()
  (if (eq (keyboard-current-local-map) (keyboard-local-map))
      nil
    (setq _keyboard-saved-local-map_ (keyboard-current-local-map))
    (keyboard-use-local-map (keyboard-local-map))))

(defun keyboard-designate-94-GL ()
  (interactive)
  (if (and (coding-system-use-japanese-jisx0201-roman keyboard-coding-system)
	   (eq 'japanese-jisx0201-roman
	       (charset-from-attributes 1 94 last-command-char)))
      (setq _current-g0_ 'ascii)
    (setq _current-g0_ (charset-from-attributes 1 94 last-command-char)))
  (if (eq _current-g0_ 'ascii)
      (keyboard-use-local-map _keyboard-saved-local-map_)
    (keyboard-change-local-map-for-iso)))

(defun keyboard-designate-94-GR ()
  (interactive)
  (setq _current-g1_ (charset-from-attributes 1 94 last-command-char)))

(defun keyboard-designate-96-GL ()
  (interactive)
  (setq _current-g0_ (charset-from-attributes 1 96 last-command-char))
  (keyboard-change-local-map-for-iso))

(defun keyboard-designate-96-GR ()
  (interactive)
  (setq _current-g1_ (charset-from-attributes 1 96 last-command-char)))

(defun keyboard-designate-94n-GL ()
  (interactive)
  (if (and (coding-system-use-japanese-jisx0208-1978 keyboard-coding-system)
	   (eq 'japanese-jisx0208-1978
	       (charset-from-attributes 2 94 last-command-char)))
      (setq _current-g0_ 'japanese-jisx0208)
    (setq _current-g0_ (charset-from-attributes 2 94 last-command-char)))
  (keyboard-change-local-map-for-iso))

(defun keyboard-designate-94n-GR ()
  (interactive)
  (setq _current-g1_ (charset-from-attributes 2 94 last-command-char)))

(defun keyboard-designate-96n-GL ()
  (interactive)
  (setq _current-g0_ (charset-from-attributes 2 96 last-command-char))
  (keyboard-change-local-map-for-iso))

(defun keyboard-designate-96n-GR ()
  (interactive)
  (setq _current-g1_ (charset-from-attributes 2 96 last-command-char)))

(defun keyboard-SS2 ()
  (interactive)
  (setq _keyboard-SS2_ t)
  (setq _saved-local-map-single-shift_ (keyboard-current-local-map))
  (keyboard-change-local-map-for-iso))

(defun keyboard-SS3 ()
  (interactive)
  (setq _keyboard-SS3_ t)
  (setq _saved-local-map-single-shift_ (keyboard-current-local-map))
  (keyboard-change-local-map-for-iso))

(defun self-insert-iso ()
  (interactive)
  (let ((charset (cond (_keyboard-SS2_ _current-g2_)
		       (_keyboard-SS3_ _current-g3_)
		       ((< last-command-char 128) _current-g0_)
		       (t _current-g1_))))
    (if (not charset) (mule-keyboard-quit))
    (if (= (charset-dimension charset) 1)
	(progn
	  (keyboard-self-insert (make-char charset last-command-char))
	  (if (or _keyboard-SS2_ _keyboard-SS3_)
	      (keyboard-use-local-map _saved-local-map-single-shift_))
	  (keyboard-reset-state))
      (if _keyboard-first-byte_
	  (progn
	    (keyboard-self-insert (make-char charset _keyboard-first-byte_
					     last-command-char))
	    (if (or _keyboard-SS2_ _keyboard-SS3_)
		(keyboard-use-local-map _saved-local-map-single-shift_))
	    (keyboard-reset-state))
	(setq _keyboard-first-byte_ last-command-char)))))


(defun keyboard-define-global-map-shift-jis (map)
  (let ((i 128))
    (while (< i 160)
      (keyboard-define-key map i 'self-insert-shift-jis-japanese)
      (setq i (1+ i)))
    (while (< i 224)
      (keyboard-define-key map i 'self-insert-shift-jis-kana)
      (setq i (1+ i)))
    (while (< i 256)
      (keyboard-define-key map i 'self-insert-shift-jis-japanese)
      (setq i (1+ i)))))

(defun keyboard-define-local-map-shift-jis (map)
  (let ((i 64))
    (while (< i 256)
      (keyboard-define-key map i 'self-insert-shift-jis-japanese2)
      (setq i (1+ i)))))

(defun set-keyboard-coding-system-shift-jis ()
  (keyboard-set-input-mode 0)		; enable 8bit input as chars
  (keyboard-define-global-map-shift-jis global-map)
  (if local-map-shift-jis 
      nil
    (setq local-map-shift-jis (make-keymap))
    (keyboard-define-local-map-shift-jis local-map-shift-jis)
    (define-key local-map-shift-jis "\C-g" 'mule-keyboard-quit))
  (setq _keyboard-first-byte_ nil)
  (setq keyboard-coding-system 'shift-jis))

(defun self-insert-shift-jis-japanese ()
  (interactive)
  (setq _keyboard-first-byte_ last-command-char)
  (setq _keyboard-saved-local-map_ (keyboard-current-local-map))
  (keyboard-use-local-map (keyboard-local-map)))

(defun self-insert-shift-jis-japanese2 ()
  (interactive)
  (if _keyboard-first-byte_
      (let ((char
	     (decode-shift-jis-char _keyboard-first-byte_ last-command-char)))
	(keyboard-self-insert char)
	(setq _keyboard-first-byte_ nil)))
  (keyboard-use-local-map _keyboard-saved-local-map_))

(defun self-insert-shift-jis-kana ()
  (interactive)
  (keyboard-self-insert (make-char 'japanese-jisx0201-kana last-command-char)))


(defun keyboard-define-global-map-big5 (map)
  (let ((i ?\xA1))
    (while (< i ?\xFE)
      (keyboard-define-key map i 'self-insert-big5-1)
      (setq i (1+ i)))))

(defun keyboard-define-local-map-big5 (map)
  (let ((i ?\x40))
    (while (< i ?\x7F)
      (keyboard-define-key map i 'self-insert-big5-2)
      (setq i (1+ i)))
    (setq i ?\xA1)
    (while (< i ?\xFF)
      (keyboard-define-key map i 'self-insert-big5-2)
      (setq i (1+ i)))
    ))

(defun set-keyboard-coding-system-big5 ()
  (require 'chinese)
  (keyboard-set-input-mode 0)		; enable 8bit input as chars
  (keyboard-define-global-map-big5 global-map)
  (if local-map-big5
      nil
    (setq local-map-big5 (make-keymap))
    (keyboard-define-local-map-big5 local-map-big5)
    (define-key local-map-big5 "\C-g" 'mule-keyboard-quit))
  (setq _keyboard-first-byte_ 0)
  (setq keyboard-coding-system 'big5))

(defun self-insert-big5-1 ()
  (interactive)
  (setq _keyboard-first-byte_ last-command-char)
  (setq _keyboard-saved-local-map_ (keyboard-current-local-map))
  (keyboard-use-local-map (keyboard-local-map)))

(defun self-insert-big5-2 ()
  (interactive)
  (if _keyboard-first-byte_
      (progn
	(keyboard-self-insert
	 (decode-big5-char _keyboard-first-byte_ last-command-char
			   'character))
	(setq _keyboard-first-byte_ nil)))
  (keyboard-use-local-map _keyboard-saved-local-map_))


(defun check-auto-fill ()
  (if (and auto-fill-function (> (current-column) fill-column))
      (funcall auto-fill-function)))
