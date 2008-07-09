;;; x-compose.el --- Compose-key processing in XEmacs

;; Copyright (C) 1992, 1993, 1997, 2005 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Maintainer: XEmacs Development Team
;; Rewritten by Martin Buchholz far too many times.
;;
;; Changed: 11 Jun 1997 by Heiko Muenkel <muenkel@tnt.uni-hannover.de>
;;	The degree sign couldn't be inserted with the old version.
;; Keywords: i18n

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; created by jwz, 14-jun-92.
;;; changed by Jan Vroonhof, July 1997: Use function-key-map instead
;;;                                     of global map.
;;;                                     Preliminary support for
;;;                                     XFree86 deadkeys

;; This file implements DEC-, OpenWindows-, and HP-compatible "Compose"
;; processing for XEmacs.

;; If you are running a version of X which already does compose processing,
;; then you don't need this file.  But the MIT R4 and R5 distributions don't
;; do compose processing, so you may want to fake it by using this code.

;; The basic idea is that there are several ways to generate keysyms which
;; do not have keys devoted to them on your keyboard.

;; The first method is by using "dead" keys.  A dead key is a key which,
;; when typed, does not insert a character.  Instead it modifies the
;; following character typed.  So if you typed "dead-tilde" followed by "A",
;; then "A-tilde" would be inserted.  Of course, this requires you to modify
;; your keyboard to include a "dead-tilde" key on it somewhere.

;; The second method is by using a "Compose" key.  With a Compose key, you
;; would type "Compose" then "tilde" then "A" to insert "A-tilde".

;; There are a small number of dead keys: acute, grave, cedilla, diaeresis,
;; circumflex, tilde, and ring.  There are a larger number of accented and
;; other characters accessible via the Compose key, so both are useful.

;; To use this code, you will need to have a Compose key on your keyboard.
;; The default configuration of most X keyboards doesn't contain one.  You
;; can, for example, turn the right "Meta" key into a "Compose" key with
;; this command:

;;    xmodmap -e "remove mod1 = Meta_R" -e "keysym Meta_R = Multi_key"

;; Multi-key is the name that X (and emacs) know the "Compose" key by.
;; The "remove..." command is necessary because the "Compose" key must not
;; have any modifier bits associated with it.  This exact command may not
;; work, depending on what system and keyboard you are using.  If it
;; doesn't, you'll have to read the man page for xmodmap.  You might want
;; to get the "xkeycaps" program from
;; <URL:http://www.jwz.org/xkeycaps/>,
;; which is a graphical front end to xmodmap
;; that hides xmodmap's arcane syntax from you.

;; If for some reason you don't want to have a dedicated compose key on your
;; keyboard, you can use some other key as the prefix.  For example, to make
;; "Meta-Shift-C" act as a compose key (so that "M-C , c" would insert the
;; character "ccedilla") you could do

;;    (global-set-key "\M-C" compose-map)

;; I believe the bindings encoded in this file are the same as those used
;; by OpenWindows versions 2 and 3, and DEC VT320 terminals.  Please let me
;; know if you think otherwise.

;; Much thanks to Justin Bur <justin@crim.ca> for helping me understand how
;; this stuff is supposed to work.

;; You also might want to consider getting Justin's patch for the MIT Xlib
;; that implements compose processing in the library.  This will enable
;; compose processing in applications other than emacs as well.  You can
;; get it from export.lcs.mit.edu in contrib/compose.tar.Z.

;; This code has one feature that a more "builtin" Compose mechanism could
;; not have: at any point you can type C-h to get a list of the possible
;; completions of what you have typed so far.

;; Giacomo Boffi's problem of
;; 20050324103919.8D22E4901@boffi95.stru.polimi.it is caused by Xlib doing
;; the compose processing. To turn that off, I'm not certain what's
;; possible, beyond making C the current locale.

;;; Code:

(macrolet
    ((define-compose-map (keymap-symbol)
       `(progn
	  (defconst ,keymap-symbol (make-sparse-keymap ',keymap-symbol))
	  ;; Required to tell XEmacs the keymaps were actually autoloaded.
	  ;; #### Make this unnecessary!
	  (fset ',keymap-symbol ,keymap-symbol))))

  (define-compose-map compose-map)
  (define-compose-map compose-acute-map)
  (define-compose-map compose-grave-map)
  (define-compose-map compose-cedilla-map)
  (define-compose-map compose-diaeresis-map)
  (define-compose-map compose-circumflex-map)
  (define-compose-map compose-tilde-map)
  (define-compose-map compose-ring-map))

(define-key compose-map 'acute	    compose-acute-map)
(define-key compose-map 'grave	    compose-grave-map)
(define-key compose-map 'cedilla    compose-cedilla-map)
(define-key compose-map 'diaeresis  compose-diaeresis-map)
(define-key compose-map 'circumflex compose-circumflex-map)
(define-key compose-map 'tilde      compose-tilde-map)
(define-key compose-map 'degree	    compose-ring-map)

;;(define-key function-key-map [multi-key] compose-map)

;; The following is necessary, because one can't rebind [degree]
;; and use it to insert the degree sign!
;;(defun compose-insert-degree ()
;;  "Inserts a degree sign."
;;  (interactive)
;;  (insert ?\260))

(define-key compose-map [acute]		compose-acute-map)
(define-key compose-map [?']		compose-acute-map)
(define-key compose-map [grave]		compose-grave-map)
(define-key compose-map [?`]		compose-grave-map)
(define-key compose-map [cedilla]	compose-cedilla-map)
(define-key compose-map [?,]		compose-cedilla-map)
(define-key compose-map [diaeresis]	compose-diaeresis-map)
(define-key compose-map [?\"]		compose-diaeresis-map)
(define-key compose-map [circumflex]	compose-circumflex-map)
(define-key compose-map [?^]		compose-circumflex-map)
(define-key compose-map [tilde]		compose-tilde-map)
(define-key compose-map [~]		compose-tilde-map)
(define-key compose-map [degree]	compose-ring-map)
(define-key compose-map [?*]		compose-ring-map)


;;; The contents of the "dead key" maps.  These are shared by the
;;; compose-map.

;;; These used to all have nice readable X11-oriented keysym names as the
;;; macro definition in the third argument, but I moved the interpretation
;;; of those mappings (that is, Aacute to \301, &c.) to runtime in the X11
;;; code on first sight of the symbols--which is the more general solution,
;;; what with Unicode keysyms, publishing, technical and so on, there's no
;;; need to have them hanging around as symbols all the time--so they're no
;;; longer available to Lisp before X11 sees them, something this relied on.

;;; The transformation was done like so;

;;;   (while (re-search-forward "\\[\\([a-zA-Z]+\\)\\])$" nil t)
;;;     (replace-match (format "(?\\%o)" 
;;;		   (get (intern (match-string 1)) 'character-of-keysym))
;;;		 t t nil 1))
 
;;; with a lot of repeated calling of setxkbmap to esoteric keymaps--so
;;; x_reset_key_mapping gets called for all the keys on the keyboard--yacute
;;; getting picked up from the Czech keymap, idiaeresis from the Dutch one,
;;; and many more (al, ca, cz, de, dvorak, ee, es, fi, fr, hu,
;;; ie(UnicodeExpert), it, nl, pt, ro, tr, us, vn, if it interests you.)

;;; The parentheses inside the vector are because otherwise the macro gets
;;; interpreted as a meta character, the Latin-1 codes being in exactly that
;;; range. Perhaps that bears documenting somewhere. Also, why is help
;;; turned off for these (x-compose) sequences by default?

;;; (Aidan Kehoe, 2005-05-18)

(define-key compose-acute-map [space]	"'")
(define-key compose-acute-map [?']	[(?\264)])
(define-key compose-acute-map [?A]	[(?\301)])
(define-key compose-acute-map [E]	[(?\311)])
(define-key compose-acute-map [I]	[(?\315)])
(define-key compose-acute-map [O]	[(?\323)])
(define-key compose-acute-map [U]	[(?\332)])
(define-key compose-acute-map [Y]	[(?\335)])
(define-key compose-acute-map [a]	[(?\341)])
(define-key compose-acute-map [e]	[(?\351)])
(define-key compose-acute-map [i]	[(?\355)])
(define-key compose-acute-map [o]	[(?\363)])
(define-key compose-acute-map [u]	[(?\372)])
(define-key compose-acute-map [y]	[(?\375)])

(define-key compose-grave-map [space]	"`")
(define-key compose-grave-map [?`]	[(?\140)])
(define-key compose-grave-map [A]	[(?\300)])
(define-key compose-grave-map [E]	[(?\310)])
(define-key compose-grave-map [I]	[(?\314)])
(define-key compose-grave-map [O]	[(?\322)])
(define-key compose-grave-map [U]	[(?\331)])
(define-key compose-grave-map [a]	[(?\340)])
(define-key compose-grave-map [e]	[(?\350)])
(define-key compose-grave-map [i]	[(?\354)])
(define-key compose-grave-map [o]	[(?\362)])
(define-key compose-grave-map [u]	[(?\371)])

(define-key compose-cedilla-map [space]	",")
(define-key compose-cedilla-map [?,]	[(?\270)])
(define-key compose-cedilla-map [C]	[(?\307)])
(define-key compose-cedilla-map [c]	[(?\347)])

(define-key compose-diaeresis-map [space] [(?\250)])
(define-key compose-diaeresis-map [?\"]	[(?\250)])
(define-key compose-diaeresis-map [A]	[(?\304)])
(define-key compose-diaeresis-map [E]	[(?\313)])
(define-key compose-diaeresis-map [I]	[(?\317)])
(define-key compose-diaeresis-map [O]	[(?\326)])
(define-key compose-diaeresis-map [U]	[(?\334)])
(define-key compose-diaeresis-map [a]	[(?\344)])
(define-key compose-diaeresis-map [e]	[(?\353)])
(define-key compose-diaeresis-map [i]	[(?\357)])
(define-key compose-diaeresis-map [o]	[(?\366)])
;; Not strictly a diaeresis, but close enough for government work. 
(define-key compose-diaeresis-map [s]	[(?\337)])
(define-key compose-diaeresis-map [u]	[(?\374)])
(define-key compose-diaeresis-map [y]	[(?\377)])

(define-key compose-circumflex-map [space] "^")
(define-key compose-circumflex-map [?/]	"|")
(define-key compose-circumflex-map [?!]	[(?\246)])
(define-key compose-circumflex-map [?-]	[(?\257)])
(define-key compose-circumflex-map [?_]	[(?\257)])
(define-key compose-circumflex-map [?0]	[(?\260)])
(define-key compose-circumflex-map [?1]	[(?\271)])
(define-key compose-circumflex-map [?2]	[(?\262)])
(define-key compose-circumflex-map [?3]	[(?\263)])
(define-key compose-circumflex-map [?.]	[(?\267)])
(define-key compose-circumflex-map [A]	[(?\302)])
(define-key compose-circumflex-map [E]	[(?\312)])
(define-key compose-circumflex-map [I]	[(?\316)])
(define-key compose-circumflex-map [O]	[(?\324)])
(define-key compose-circumflex-map [U]	[(?\333)])
(define-key compose-circumflex-map [a]	[(?\342)])
(define-key compose-circumflex-map [e]	[(?\352)])
(define-key compose-circumflex-map [i]	[(?\356)])
(define-key compose-circumflex-map [o]	[(?\364)])
(define-key compose-circumflex-map [u]	[(?\373)])

(define-key compose-tilde-map [space]	"~")
(define-key compose-tilde-map [A]	[(?\303)])
(define-key compose-tilde-map [N]	[(?\321)])
(define-key compose-tilde-map [O]	[(?\325)])
(define-key compose-tilde-map [a]	[(?\343)])
(define-key compose-tilde-map [n]	[(?\361)])
(define-key compose-tilde-map [o]	[(?\365)])

(define-key compose-ring-map [space]	[(?\260)])
(define-key compose-ring-map [A]	[(?\305)])
(define-key compose-ring-map [a]	[(?\345)])


;;; The rest of the compose-map.  These are the composed characters
;;; that are not accessible via "dead" keys.

(define-key compose-map " '"	"'")
(define-key compose-map " ^"	"^")
(define-key compose-map " `"	"`")
(define-key compose-map " ~"	"~")
(define-key compose-map "  "	[(?\240)])
(define-key compose-map " \""	[(?\250)])
(define-key compose-map " :"	[(?\250)])
(define-key compose-map " *"	[(?\260)])

(define-key compose-map "!!"	[(?\241)])
(define-key compose-map "!^"	[(?\246)])
(define-key compose-map "!S"	[(?\247)])
(define-key compose-map "!s"	[(?\247)])
(define-key compose-map "!P"	[(?\266)])
(define-key compose-map "!p"	[(?\266)])

(define-key compose-map "(("	"[")
(define-key compose-map "(-"	"{")

(define-key compose-map "))"	"]")
(define-key compose-map ")-"	"}")

(define-key compose-map "++"	"#")
(define-key compose-map "+-"	[(?\261)])

(define-key compose-map "-("	"{")
(define-key compose-map "-)"	"}")
(define-key compose-map "--"	"-")
(define-key compose-map "-L"	[(?\243)])
(define-key compose-map "-l"	[(?\243)])
(define-key compose-map "-Y"	[(?\245)])
(define-key compose-map "-y"	[(?\245)])
(define-key compose-map "-,"	[(?\254)])
(define-key compose-map "-|"	[(?\254)])
(define-key compose-map "-^"	[(?\257)])
(define-key compose-map "-+"	[(?\261)])
(define-key compose-map "-:"	[(?\367)])
(define-key compose-map "-D"	[(?\320)])
(define-key compose-map "-d"	[(?\360)])
(define-key compose-map "-a"    [(?\252)])

(define-key compose-map ".^"	[(?\267)])

(define-key compose-map "//"	"\\")
(define-key compose-map "/<"	"\\")
(define-key compose-map "/^"	"|")
(define-key compose-map "/C"	[(?\242)])
(define-key compose-map "/c"	[(?\242)])
(define-key compose-map "/U"	[(?\265)])
(define-key compose-map "/u"	[(?\265)])
(define-key compose-map "/O"	[(?\330)])
(define-key compose-map "/o"	[(?\370)])

(define-key compose-map "0X"	[(?\244)])
(define-key compose-map "0x"	[(?\244)])
(define-key compose-map "0S"	[(?\247)])
(define-key compose-map "0s"	[(?\247)])
(define-key compose-map "0C"	[(?\251)])
(define-key compose-map "0c"	[(?\251)])
(define-key compose-map "0R"	[(?\256)])
(define-key compose-map "0r"	[(?\256)])
(define-key compose-map "0^"	[(?\260)])

(define-key compose-map "1^"	[(?\271)])
(define-key compose-map "14"	[(?\274)])
(define-key compose-map "12"	[(?\275)])

(define-key compose-map "2^"	[(?\262)])

(define-key compose-map "3^"	[(?\263)])
(define-key compose-map "34"	[(?\276)])

(define-key compose-map ":-"	[(?\367)])

(define-key compose-map "</"	"\\")
(define-key compose-map "<<"	[(?\253)])

(define-key compose-map "=L"	[(?\243)])
(define-key compose-map "=l"	[(?\243)])
(define-key compose-map "=Y"	[(?\245)])
(define-key compose-map "=y"	[(?\245)])

(define-key compose-map ">>"	[(?\273)])

(define-key compose-map "??"	[(?\277)])

(define-key compose-map "AA"	"@")
(define-key compose-map "Aa"	"@")
(define-key compose-map "A_"	[(?\252)])
(define-key compose-map "A`"	[(?\300)])
(define-key compose-map "A'"	[(?\301)])
(define-key compose-map "A^"	[(?\302)])
(define-key compose-map "A~"	[(?\303)])
(define-key compose-map "A\""	[(?\304)])
(define-key compose-map "A*"	[(?\305)])
(define-key compose-map "AE"	[(?\306)])

(define-key compose-map "C/"	[(?\242)])
(define-key compose-map "C|"	[(?\242)])
(define-key compose-map "C0"	[(?\251)])
(define-key compose-map "CO"	[(?\251)])
(define-key compose-map "Co"	[(?\251)])
(define-key compose-map "C,"	[(?\307)])

(define-key compose-map "D-"	[(?\320)])

(define-key compose-map "E`"	[(?\310)])
(define-key compose-map "E'"	[(?\311)])
(define-key compose-map "E^"	[(?\312)])
(define-key compose-map "E\""	[(?\313)])

(define-key compose-map "I`"	[(?\314)])
(define-key compose-map "I'"	[(?\315)])
(define-key compose-map "I^"	[(?\316)])
(define-key compose-map "I\""	[(?\317)])

(define-key compose-map "L-"	[(?\243)])
(define-key compose-map "L="	[(?\243)])

(define-key compose-map "N~"	[(?\321)])

(define-key compose-map "OX"	[(?\244)])
(define-key compose-map "Ox"	[(?\244)])
(define-key compose-map "OS"	[(?\247)])
(define-key compose-map "Os"	[(?\247)])
(define-key compose-map "OC"	[(?\251)])
(define-key compose-map "Oc"	[(?\251)])
(define-key compose-map "OR"	[(?\256)])
(define-key compose-map "Or"	[(?\256)])
(define-key compose-map "O_"	[(?\272)])
(define-key compose-map "O`"	[(?\322)])
(define-key compose-map "O'"	[(?\323)])
(define-key compose-map "O^"	[(?\324)])
(define-key compose-map "O~"	[(?\325)])
(define-key compose-map "O\""	[(?\326)])
(define-key compose-map "O/"	[(?\330)])

(define-key compose-map "P!"	[(?\266)])

(define-key compose-map "R0"	[(?\256)])
(define-key compose-map "RO"	[(?\256)])
(define-key compose-map "Ro"	[(?\256)])

(define-key compose-map "S!"	[(?\247)])
(define-key compose-map "S0"	[(?\247)])
(define-key compose-map "SO"	[(?\247)])
(define-key compose-map "So"	[(?\247)])
(define-key compose-map "SS"	[(?\337)])

(define-key compose-map "TH"	[(?\336)])

(define-key compose-map "U`"	[(?\331)])
(define-key compose-map "U'"	[(?\332)])
(define-key compose-map "U^"	[(?\333)])
(define-key compose-map "U\""	[(?\334)])

(define-key compose-map "X0"	[(?\244)])
(define-key compose-map "XO"	[(?\244)])
(define-key compose-map "Xo"	[(?\244)])

(define-key compose-map "Y-"	[(?\245)])
(define-key compose-map "Y="	[(?\245)])
(define-key compose-map "Y'"	[(?\335)])

(define-key compose-map "_A"	[(?\252)])
(define-key compose-map "_a"	[(?\252)])
(define-key compose-map "_^"	[(?\257)])
(define-key compose-map "_O"	[(?\272)])
(define-key compose-map "_o"	[(?\272)])

(define-key compose-map "aA"	"@")
(define-key compose-map "aa"	"@")
(define-key compose-map "a_"	[(?\252)])
(define-key compose-map "a-"    [(?\252)])
(define-key compose-map "a`"	[(?\340)])
(define-key compose-map "a'"	[(?\341)])
(define-key compose-map "a^"	[(?\342)])
(define-key compose-map "a~"	[(?\343)])
(define-key compose-map "a\""	[(?\344)])
(define-key compose-map "a*"	[(?\345)])
(define-key compose-map "ae"	[(?\346)])

(define-key compose-map "c/"	[(?\242)])
(define-key compose-map "c|"	[(?\242)])
(define-key compose-map "c0"	[(?\251)])
(define-key compose-map "cO"	[(?\251)])
(define-key compose-map "co"	[(?\251)])
(define-key compose-map "c,"	[(?\347)])

(define-key compose-map "d-"	[(?\360)])

(define-key compose-map "e`"	[(?\350)])
(define-key compose-map "e'"	[(?\351)])
(define-key compose-map "e^"	[(?\352)])
(define-key compose-map "e\""	[(?\353)])

(define-key compose-map "i`"	[(?\354)])
(define-key compose-map "i'"	[(?\355)])
(define-key compose-map "i^"	[(?\356)])
(define-key compose-map "i\""	[(?\357)])
(define-key compose-map "i:"	[(?\357)])

(define-key compose-map "l-"	[(?\243)])
(define-key compose-map "l="	[(?\243)])

(define-key compose-map "n~"	[(?\361)])

(define-key compose-map "oX"	[(?\244)])
(define-key compose-map "ox"	[(?\244)])
(define-key compose-map "oC"	[(?\251)])
(define-key compose-map "oc"	[(?\251)])
(define-key compose-map "oR"	[(?\256)])
(define-key compose-map "or"	[(?\256)])
(define-key compose-map "oS"	[(?\247)])
(define-key compose-map "os"	[(?\247)])
(define-key compose-map "o_"	[(?\272)])
(define-key compose-map "o`"	[(?\362)])
(define-key compose-map "o'"	[(?\363)])
(define-key compose-map "o^"	[(?\364)])
(define-key compose-map "o~"	[(?\365)])
(define-key compose-map "o\""	[(?\366)])
(define-key compose-map "o/"	[(?\370)])

(define-key compose-map "p!"	[(?\266)])

(define-key compose-map "r0"	[(?\256)])
(define-key compose-map "rO"	[(?\256)])
(define-key compose-map "ro"	[(?\256)])

(define-key compose-map "s!"	[(?\247)])
(define-key compose-map "s0"	[(?\247)])
(define-key compose-map "sO"	[(?\247)])
(define-key compose-map "so"	[(?\247)])
(define-key compose-map "ss"	[(?\337)])

(define-key compose-map "th"	[(?\376)])

(define-key compose-map "u`"	[(?\371)])
(define-key compose-map "u'"	[(?\372)])
(define-key compose-map "u^"	[(?\373)])
(define-key compose-map "u\""	[(?\374)])
(define-key compose-map "u/"	[(?\265)])

(define-key compose-map "x0"	[(?\244)])
(define-key compose-map "xO"	[(?\244)])
(define-key compose-map "xo"	[(?\244)])
(define-key compose-map "xx"	[(?\327)])

(define-key compose-map "y-"	[(?\245)])
(define-key compose-map "y="	[(?\245)])
(define-key compose-map "y'"	[(?\375)])
(define-key compose-map "y\""	[(?\377)])

(define-key compose-map "|C"	[(?\242)])
(define-key compose-map "|c"	[(?\242)])
(define-key compose-map "||"	[(?\246)])


;; [[ Suppose we type these three physical keys: [Multi_key " a]
;; Xlib can deliver these keys as the following sequences of keysyms:
;;
;; - [Multi_key " a] (no surprise here)
;; - [adiaeresis] (OK, Xlib is doing compose processing for us)
;; - [Multi_key " adiaeresis] (Huh?)
;;
;; It is the last possibility that is arguably a bug.  Xlib can't
;; decide whether it's really doing compose processing or not (or
;; actually, different parts of Xlib disagree).
;;
;; So we'll just convert [Multi_key " adiaeresis] to [adiaeresis] ]] 

(eval-when-compile 
  (when nil ;; Commenting out.

    ;; This _used_ to work with our X11-oriented keysyms above. With them
    ;; gone, it won't. The X11 bug it works around should be long dead. (Ha! 
    ;; Wasn't it cockroaches that would have ruled the planet after World
    ;; War III?)

    (defun xlib-input-method-bug-workaround (keymap)
      (map-keymap
       (lambda (key value)
	 (cond
	  ((keymapp value)
	   (xlib-input-method-bug-workaround value))
	  ((and (sequencep value)
		(eq 1 (length value))
		(null (lookup-key keymap value)))
	   (define-key keymap value value))))
       keymap))
    (xlib-input-method-bug-workaround compose-map)
    (unintern 'xlib-input-method-bug-workaround)))

;; While we're at it, a similar mechanism will make colon equivalent
;; to doublequote for diaeresis processing.  Some Xlibs do this.
(defun alias-colon-to-doublequote (keymap)
  (map-keymap
   (lambda (key value)
     (when (keymapp value)
       (alias-colon-to-doublequote value))
     (when (eq key '\")
       (define-key keymap ":" value)))
   keymap))
(alias-colon-to-doublequote compose-map)
(unintern 'alias-colon-to-doublequote)

;;; Electric dead keys: making a' mean a-acute.


(defun electric-diacritic (&optional count)
  "Modify the previous character with an accent.
For example, if `:' is bound to this command, then typing `a:'
will first insert `a' and then turn it into `\344' (adiaeresis).
The keys to which this command may be bound (and the accents
which it understands) are:

   '  (acute)       \301\311\315\323\332\335 \341\351\355\363\372\375
   `  (grave)       \300\310\314\322\331 \340\350\354\362\371
   :  (diaeresis)   \304\313\317\326\334 \344\353\357\366\374\377
   ^  (circumflex)  \302\312\316\324\333 \342\352\356\364\373
   ,  (cedilla)     \307\347
   .  (ring)        \305\345"
  (interactive "p")
  (or count (setq count 1))

  (if (not (eq last-command 'self-insert-command))
      ;; Only do the magic if the two chars were typed in succession.
      (self-insert-command count)

    ;; This is so that ``a : C-x u'' will transform `adiaeresis' back into `a:'
    (self-insert-command count)
    (undo-boundary)
    (delete-char (- count))

    (let* ((c last-command-char)
	   (map (cond ((eq c ?') compose-acute-map)
		      ((eq c ?`) compose-grave-map)
		      ((eq c ?,) compose-cedilla-map)
		      ((eq c ?:) compose-diaeresis-map)
		      ((eq c ?^) compose-circumflex-map)
		      ((eq c ?~) compose-tilde-map)
		      ((eq c ?.) compose-ring-map)
		      (t (error "unknown diacritic: %s (%c)" c c))))
	   (base-char (preceding-char))
	   (mod-char (and (>= (downcase base-char) ?a) ; only do alphabetics?
			  (<= (downcase base-char) ?z)
			  (lookup-key map (make-string 1 base-char)))))
      (if (and (vectorp mod-char) (= (length mod-char) 1))
	  (setq mod-char (aref mod-char 0)))
      (if (and mod-char (symbolp mod-char))
	  (setq mod-char (or (get mod-char 'character-of-keysym) mod-char)))
      (if (and mod-char (> count 0))
	  (delete-char -1)
	(setq mod-char c))
      (while (> count 0)
	(insert mod-char)
	(setq count (1- count))))))

;; should "::" mean "¨" and ": " mean ":"?
;; should we also do
;;    (?~
;;     (?A "\303")
;;     (?C "\307")
;;     (?D "\320")
;;     (?N "\321")
;;     (?O "\325")
;;     (?a "\343")
;;     (?c "\347")
;;     (?d "\360")
;;     (?n "\361")
;;     (?o "\365")
;;     (?> "\273")
;;     (?< "\253")
;;     (?  "~")) ; no special code
;;    (?\/
;;     (?A "\305") ;; A-with-ring (Norwegian and Danish)
;;     (?E "\306") ;; AE-ligature (Norwegian and Danish)
;;     (?O "\330")
;;     (?a "\345") ;; a-with-ring (Norwegian and Danish)
;;     (?e "\346") ;; ae-ligature (Norwegian and Danish)
;;     (?o "\370")
;;     (?  "/")) ; no special code


;;; Providing help in the middle of a compose sequence.  (Way cool.)

(eval-when-compile
  (defsubst next-composable-event ()
    (let (event)
      (while (progn
	       (setq event (next-command-event))
	       (not (or (key-press-event-p event)
			(button-press-event-p event))))
	(dispatch-event event))
      event)))

(defun compose-help (ignore-prompt)
  (let* ((keys (apply 'vector (nbutlast (append (this-command-keys) nil))))
	 (map (or (lookup-key function-key-map keys)
		  (error "can't find map?  %s %s" keys (this-command-keys))))
	 binding)
    (save-excursion
      (with-output-to-temp-buffer "*Help*"
	(set-buffer "*Help*")
	(erase-buffer)
	(message "Working...")
	(setq ctl-arrow 'compose) ; non-t-non-nil
	(insert "You are typing a compose sequence.  So far you have typed: ")
	(insert (key-description keys))
	(insert "\nCompletions from here are:\n\n")
	(map-keymap 'compose-help-mapper map t)
	(message "? ")))
    (while (keymapp map)
      (setq binding (lookup-key map (vector (next-composable-event))))
      (if (null binding)
	  (message "No such key in keymap. Try again.")
	(setq map binding)))
    binding))

(put 'compose-help 'isearch-command t)	; so that it doesn't terminate isearch

(defun compose-help-mapper (key binding)
  (if (and (symbolp key)
	   (get key 'character-of-keysym))
      (setq key (get key 'character-of-keysym)))
  (if (eq binding 'compose-help) ; suppress that...
      nil
    (if (keymapp binding)
	(let ((p (point)))
	  (map-keymap 'compose-help-mapper binding t)
	  (goto-char p)
	  (while (not (eobp))
	    (if (characterp key)
		(insert (make-string 1 key))
	      (insert (single-key-description key)))
	    (insert " ")
	    (forward-line 1)))
      (if (characterp key)
	  (insert (make-string 1 key))
	(insert (single-key-description key)))
      (indent-to 16)
      (let ((code (and (vectorp binding)
		       (= 1 (length binding))
		       (get (aref binding 0) 'character-of-keysym))))
	(if code
	    (insert (make-string 1 code))
	  (if (stringp binding)
	      (insert binding)
	    (insert (prin1-to-string binding)))))
      (when (and (vectorp binding) (= 1 (length binding)))
	(indent-to 32)
	(insert (symbol-name (aref binding 0)))))
    (insert "\n")))

;; define it at top-level in the compose map...
;;(define-key compose-map [(control h)] 'compose-help)
;;(define-key compose-map [help]        'compose-help)
;; and then define it in each sub-map of the compose map.
(map-keymap
 (lambda (key binding)
   (when (keymapp binding)
;;     (define-key binding [(control h)] 'compose-help)
;;     (define-key binding [help]        'compose-help)
     ))
 compose-map nil)

;; Make redisplay display the accented letters
(if (memq (default-value 'ctl-arrow) '(t nil))
    (setq-default ctl-arrow 'iso-8859/1))


(provide 'x-compose)

;;; x-compose.el ends here
