;;; x-win-sun.el --- runtime initialization for Sun X servers and keyboards
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Authors: jwz@netscape.com, wing@666.com, mrb@eng.sun.com
;; Keywords: terminals

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is loaded by x-win.el at run-time when we are sure that XEmacs
;; is running on the display of a Sun.

;; The Sun X server (both the MIT and OpenWindows varieties) have extremely
;; stupid names for their keypad and function keys.  For example, the key
;; labeled 3 / PgDn, with R15 written on the front, is actually called F35.

;; There are 3 methods of dealing with the Sun key brokenness:
;;
;; - Use xmodmap to give all keys more sensible names for all apps:
;;   I use this shell script:
;;
;;   for i in 0 1 2 3 4 5 6 7 8 9 Add Subtract Multiply Divide Decimal ; do
;;     echo "keysym KP-$i = KP-$i"
;;   done | xmodmap
;;
;;   Clearly, as a good X11 citizen, we can't do this.
;;
;; - Use keyboard-translate-table to remap the keybindings at a low level.
;;   This approach is taken in the function `sun-x11-keyboard-translate'.
;;   This is like running xmodmap within XEmacs only.
;;   This is not the default, however, so that legacy keybindings in users'
;;   .emacs files like (global-set-key [(f35)] 'foo) continue to work
;;
;; - Use keyboard macros to provide indirection for keybindings.
;;   If we do (global-set-key [(f35)] [(kp-3)]), then the user's keybindings
;;   work whether he uses `f35' or `kp-3'.
;;   This is also compatible with FSF Emacs and other X11 apps.
;;   Although this has the disadvantage that these remappings
;;   only work with the global key map, we use this as the default.
;;
;; - The Right Way to do this remains to be written...

;; OK, here's another try at doing things the right way.

;; We use function-key-map, which honors explicit key-bindings for the
;; stupid Sun names, but also allows indirection if no explicit
;; key-binding exists.

;;; Code:

(defun x-remap-keysyms-using-function-key-map (from-key to-key)
  (dolist (prefix '(() (shift) (control) (meta) (alt)
		    (shift control) (shift alt) (shift meta)
		    (control alt) (control meta) (alt meta)
		    (shift control alt) (shift control meta)
		    (shift alt meta) (control alt meta)
		    (shift control alt meta)))
    (define-key function-key-map
      (append prefix (list from-key))
      (vector (append prefix (list to-key))))))

 ;; help is ok
 ;; num_lock is ok
 ;; up is ok
 ;; left is ok
 ;; right is ok
 ;; kp-add is ok
 ;; down is ok
 ;; insert is ok
 ;; delete is ok
 ;; kp-enter is ok
;; Sun Function keys
(loop for (x-name from-key to-key) in
  `(("F21" f21 pause)
    ("F22" f22 print)
    ("F23" f23 scroll_lock)

    ;; X11 R6 mappings
    ("SunProps" SunProps props)
    ("SunFront" SunFront front)
    ("SunOpen"  SunOpen  open)
    ("SunFind"  SunFind  find)
    ("Cancel"   cancel   stop)
    ("Undo"     Undo     undo)
    ("SunCopy"  SunCopy  copy)
    ("SunPaste" SunPaste paste)
    ("SunCut"   SunCut   cut)

    ("F13" f13 props)
    ("F14" f14 undo)
    ("F15" f15 front)
    ("F16" f16 copy)
    ("F17" f17 open)
    ("F18" f18 paste)
    ("F19" f19 find)
    ("F20" f20 cut)

    ("F25" f25 kp-divide)
    ("F26" f26 kp-multiply)
    ("F31" f31 kp-5)

    ;; Map f33 and r13 to end or kp-end
    ,@(cond
       ((not (x-keysym-on-keyboard-sans-modifiers-p "End"))
	'(("F33" f33 end)
	  ("R13" r13 end)))
       ((not (x-keysym-on-keyboard-sans-modifiers-p "KP_End"))
	'(("F33" f33 kp-end)
	  ("R13" r13 kp-end))))

    ,@(if (x-keysym-on-keyboard-sans-modifiers-p "F36")
	  '(("F36" f36 stop)
	    ("F37" f37 again)))

    ;; Type 4 keyboards have a real kp-subtract  and a f24 labelled `='
    ;; Type 5 keyboards have no key labelled `=' and a f24 labelled `-'
    ,@(when (x-keysym-on-keyboard-sans-modifiers-p "F24")
	`(("F24" f24 ,(if (x-keysym-on-keyboard-sans-modifiers-p "KP_Subtract")
			  'kp-equal
			'kp-subtract))))

    ;; Map f27 to home or kp-home, as appropriate
    ,@(cond ((not (x-keysym-on-keyboard-sans-modifiers-p "Home"))
	     '(("F27" f27 home)))
	    ((not (x-keysym-on-keyboard-sans-modifiers-p "KP_Home"))
	     '(("F27" f27 kp-home))))

    ;; Map f29 to prior or kp-prior, as appropriate
    ,@(cond ((not (x-keysym-on-keyboard-sans-modifiers-p "Prior"))
	     '(("F29" f29 prior)))
	    ((not (x-keysym-on-keyboard-sans-modifiers-p "KP_Prior"))
	     '(("F29" f29 kp-prior))))

    ;; Map f35 to next or kp-next, as appropriate
    ,@(cond ((not (x-keysym-on-keyboard-sans-modifiers-p "Next"))
	     '(("F35" f35 next)))
	    ((not (x-keysym-on-keyboard-sans-modifiers-p "KP_Next"))
	     '(("F35" f35 kp-next))))

    ,@(cond ((x-keysym-on-keyboard-sans-modifiers-p "apRead") ; SunOS 4.1.1
	     '(("apRead" apRead f11) ("apEdit" apEdit f12)))
	    ((x-keysym-on-keyboard-sans-modifiers-p "SunF36") ; SunOS 5
	     '(("SunF36" SunF36 f11)
	       ("SunF37" SunF37 f12)
	       ("F11"    f11    stop)
	       ("F12"    f12    again))))
    )
  do (when (x-keysym-on-keyboard-sans-modifiers-p x-name)
       (x-remap-keysyms-using-function-key-map from-key to-key)))

(unintern 'x-remap-keysyms-using-function-key-map)

  ;; for each element in the left column of the above table, alias it
  ;; to the thing in the right column.  Then do the same for many, but
  ;; not all, modifier combinations.
  ;;
  ;; (Well, we omit hyper and super. #### Handle this some other way!)
;  (while mapping
;    (let ((mods '(() (shift) (control) (meta) (alt))))
;      (while mods
;	(let ((k1 (vector (append (car mods) (list (car (car mapping))))))
;	      (k2 (vector (append (car mods) (list (cdr (car mapping)))))))
;	  (define-key global-map k1 k2))
;	(setq mods (cdr mods))))
;    (setq mapping (cdr mapping))))

;;; I've extended keyboard-translate-table to work over keysyms.
;;; [FSF Emacs has something called `system-key-alist' that is
;;; supposed to accomplish approximately the same thing.  Unfortunately,
;;; it's brain-dead in the typically FSF way, and associates *numbers*
;;; (who knows where the hell they come from?) with symbols.] --ben

;;; And I've made it into a function which is NOT called by default --mrb

(defun sun-x11-keyboard-translate ()
  "Remap Sun's X11 keyboard.
Keys with names like `f35' are remapped, at a low level,
to more mnemonic ones,like `kp-3'."
  (interactive)

  (keyboard-translate
   'f11		'stop			; the type4 keyboard Sun/MIT name
   'f36		'stop			; the type5 keyboard Sun name
   'cancel	'stop			; R6 binding
   'f12		'again			; the type4 keyboard Sun/MIT name
   'f37		'again			; the type5 keyboard Sun name
   'f13		'props			;
   'SunProps	'props			; R6 binding
   'f14		'undo			;
   'f15		'front			;
   'SunFront	'front			; R6 binding
   'f16		'copy			;
   'SunCopy	'copy			; R6 binding
   'f17		'open			;
   'SunOpen	'open			; R6 binding
   'f18		'paste			;
   'SunPaste	'paste			; R6 binding
   'f19		'find			;
   'f20		'cut			;
   'SunCut	'cut			; R6 binding
   ;; help is ok
   'f21 'pause
   'f22 'prsc
   'f23 'scroll
   ;; num_lock is ok
   ;;'f24 'kp-equal)			; type4 only!
   'f25 'kp-divide			;
   'f26 'kp-multiply			;
   'f24 'kp-subtract			; type5 only!
   'f27 'kp-7				;
   ;; up is ok
   'f29 'kp-9
   ;; left is ok
   'f31 'kp-5
   ;; right is ok
   ;; kp-add is ok
   'f33 'kp-1				; the Sun name
   'r13 'end				; the MIT name
   ;; down is ok
   'f35 'kp-3
   ;; insert is ok
   ;; delete is ok
   ;; kp-enter is ok
   'SunF36 'f11				; Type 5 keyboards
   'SunF37 'f12				; Used to be Stop & Again
   ))


;;; OpenWindows-like "find" processing.
;;; As far as I know, the `find' key is a Sunism, so we do that binding
;;; here.  This is the only Sun-specific keybinding.  (The functions
;;; themselves are in x-win.el in case someone wants to use them when
;;; not running on a Sun display.)

(define-key global-map 'find		'ow-find)
(define-key global-map '(shift find)	'ow-find-backward)

;;; x-win-sun.el ends here
