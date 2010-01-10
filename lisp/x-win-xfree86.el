;;; x-win-xfree86.el --- runtime initialization for XFree86 servers
;; Copyright (C) 1995 Sun Microsystems, Inc.
;; Copyright (C) 1995 Ben Wing.

;; Author: Ben Wing
;; Author: Martin Buchholz (rewritten to use function-key-map)
;; Keywords: terminals

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

;;; Commentary:

;; This file is loaded by x-win.el at run-time when we are sure that XEmacs
;; is running on the display of something running XFree86 (Linux,
;; NetBSD, FreeBSD, and perhaps other Intel Unixen).

;;; #### bleck!!! Use key-translation-map!

;;; #### Counter-bleck!! We shouldn't override a user binding for F13.
;;; So we use function-key-map for now.
;;; When we've implemented a fallback-style equivalent of
;;; keyboard-translate-table, we'll use that instead. (martin)

;; For no obvious reason, shift-F1 is called F13, although Meta-F1 and
;; Control-F1 have normal names.

;;; Code:

(globally-declare-fboundp
 '(x-keysym-on-keyboard-p x-keysym-on-keyboard-sans-modifiers-p))

;;;###autoload
(defun x-win-init-xfree86 (device)

  ;; We know this keyboard is an XFree86 keyboard. As such, we can predict
  ;; what key scan codes will correspond to the keys on US keyboard layout,
  ;; and we can use that information to fall back to the US layout when
  ;; looking up commands that would otherwise fail. (Cf. the hard-coding of
  ;; this information in /usr/X11R6/lib/X11/xkb/keycodes/xfree86 )
  ;;
  ;; These settings for x-us-keymap-first-keycode and
  ;; x-us-keymap-description were determined with 
  ;; 
  ;; setxkbmap us
  ;; xmodmap -pke > keyboard-description.txt
  ;;
  ;; "8" is the key code of the first line, x-us-keymap-description is
  ;; taken from the column describing the bindings. 

  (setq x-us-keymap-first-keycode 8
	x-us-keymap-description
	[nil nil [?1 ?!] [?2 ?@] [?3 ?\#] [?4 ?$] [?5 ?%] [?6 ?^] [?7 ?&]
	     [?8 ?*] [?9 ?\(] [?0 ?\)] [?- ?_] [?= ?+] nil ?\t [?q ?Q] 
	     [?w ?W] [?e ?E] [?r ?R] [?t ?T] [?y ?Y] [?u ?U] [?i ?I] [?o ?O]
	     [?p ?P] [?\[ ?{] [?\] ?}] nil nil [?a ?A] [?s ?S] [?d ?D]
	     [?f ?F] [?g ?G] [?h ?H] [?j ?J] [?k ?K] [?l ?L] [?\; ?:]
	     [?\' ?\"] [?\` ?~] nil [?\\ ?|] [?z ?Z] [?x ?X] [?c ?C]
	     [?v ?V] [?b ?B] [?n ?N] [?m ?M] [?\, ?<] [?\. ?>] [?/ ?\?]
	     nil ?* nil ?\  nil nil nil nil nil nil nil nil nil nil nil
	     nil nil ?7 ?8 ?9 ?- ?4 ?5 ?6 ?+ ?1 ?2 ?3 ?0 ?\. nil nil 
	     [?< ?>] nil nil nil nil nil nil nil nil nil nil nil nil 
	     nil nil nil nil nil ?/ nil nil nil nil nil nil nil nil 
	     nil nil nil nil nil ?=])

  (loop for (key sane-key) in
    '((f13 f1)
      (f14 f2)
      (f15 f3)
      (f16 f4)
      (f17 f5)
      (f18 f6)
      (f19 f7)
      (f20 f8)
      (f21 f9)
      (f22 f10)
      (f23 f11)
      (f24 f12))
    ;; Get the correct value for function-key-map
    with function-key-map = (symbol-value-in-console 'function-key-map
                                                     (device-console device)
                                                     function-key-map)

    do
    (when (and (x-keysym-on-keyboard-p key device)
	       (not (x-keysym-on-keyboard-sans-modifiers-p key device)))
      ;; define also the control, meta, and meta-control versions.
      (loop for mods in '(() (control) (meta) (meta control)) do
	(define-key function-key-map `[(,@mods ,key)] `[(shift ,@mods ,sane-key)])
	))))

;;; x-win-xfree86.el ends here
