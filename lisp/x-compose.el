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

;; ----------------------------------------------------------------------
;;
;; Notes from Aidan Kehoe, Thu Feb 12 16:21:18 GMT 2009 (these conflict to
;; some extent with the above):

;; Giacomo Boffi's problem of
;; 20050324103919.8D22E4901@boffi95.stru.polimi.it is caused by Xlib doing
;; the compose processing. To turn that off, you need to recompile without
;; XIM support, or start up XEmacs in a locale that the system supports but
;; X11 does not (for me, ru_RU.CP866 works for this). This will be
;; preferable anyway for some people, because the XIM support drops
;; sequences we would prefer to see. E.g. in the following situation, with
;; an XIM build:

;;    $ LC_CTYPE=de_DE.ISO8859-1 ./xemacs -vanilla &
      
;;    Input: dead-acute a 
;;    Seen by XEmacs: aacute (thanks to XIM)
;;    Action: U+00E1 is inserted in the buffer 
      
;;    Input: dead-abovedot o
;;    Seen by XEmacs: dead-abovedot o (XIM does not intervene, since no
;;                    characters in this locale are generated with
;;                    dead-abovedot)
;;    Action: U+022F is inserted in the buffer (thanks to this file)
      
;;    Input: dead-acute r
;;    Seen by XEmacs: nothing (thanks to XIM, it considers U+0155 unavailable)
;;    Action: nothing

;; Without XIM, all the above inputs would work fine, independent of your
;; locale.

;; Also, XIM does not intervene at all with the second or subsequent X11
;; devices created, and this file is needed for compose processing
;; there. This may be a bug in our use of XIM, or it may a bug in XIM
;; itself.

;;; Code:

(macrolet
    ((define-compose-map (&rest keymap-symbols)
       (loop
         for keymap-symbol in keymap-symbols
         with result = nil
         do
	  ;; Required to tell XEmacs the keymaps were actually autoloaded.
	  ;; #### Make this unnecessary!
         (push `(fset ',keymap-symbol ,keymap-symbol) result)
         (push `(defconst ,keymap-symbol (make-sparse-keymap ',keymap-symbol))
               result)
         finally return (cons 'progn result))))
  (define-compose-map compose-map compose-acute-map compose-grave-map
    compose-cedilla-map compose-diaeresis-map compose-circumflex-map
    compose-tilde-map compose-ring-map compose-caron-map compose-macron-map
    compose-breve-map compose-dot-map compose-doubleacute-map
    compose-ogonek-map compose-hook-map compose-horn-map))

(define-key compose-map 'acute	    compose-acute-map)
(define-key compose-map 'grave	    compose-grave-map)
(define-key compose-map 'cedilla    compose-cedilla-map)
(define-key compose-map 'diaeresis  compose-diaeresis-map)
(define-key compose-map 'circumflex compose-circumflex-map)
(define-key compose-map 'tilde      compose-tilde-map)
(define-key compose-map 'degree	    compose-ring-map)
(define-key compose-map 'caron	    compose-caron-map)
(define-key compose-map 'macron	    compose-macron-map)
(define-key compose-map 'doubleacute compose-doubleacute-map)
(define-key compose-map 'ogonek     compose-ogonek-map)
(define-key compose-map 'breve      compose-breve-map)
(define-key compose-map 'abovedot   compose-dot-map)

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

(loop
  for (keysym character-code map)
  in '((caron #x02C7 compose-caron-map)
       (macron #x00AF compose-macron-map)
       (doubleacute #x02DD compose-doubleacute-map)
       (ogonek #x02db compose-ogonek-map)
       (breve #x0306 compose-breve-map)
       (abovedot #x0307 compose-dot-map)
       (U031b #x031b compose-horn-map))
  do
  (define-key compose-map (vector keysym) map)
  (when (setq character-code (decode-char 'ucs character-code))
    (define-key compose-map (vector character-code) map))) 


;;; The contents of the "dead key" maps.  These are shared by the
;;; compose-map.

;;; Against the spirit of Unicode, which says that the precomposed
;;; characters are just there for round-trip compatibility with other
;;; encodings and don't reflect that they're necessarily used much, these
;;; are just the precomposed Latin characters in UnicodeData.txt; we don't
;;; support any decomposed characters here. (Not least because in general we
;;; don't have worthwhile support for precomposed characters.)

(assert
 (or (featurep 'mule) (null (decode-char 'ucs #x100)))
 nil
 "This code assumes no non-Mule characters have a UCS value \
greater than #xFF, and needs to be rewritten if that is not true.")

(macrolet
    ((decide-on-bindings (&rest details)
       "Look through DETAILS, working out which bindings work on non-Mule.

This returns a long `if' statement that should be executed when this file is
loaded; it assumes that #xFF is the inclusive upper bound for the Unicode
value of characters under non-Mule. "
       (loop for (map key binding) in details
         with if-mule = nil
         with without-mule = nil
         do
         (push `(define-key ,map ,key
                 (vector (list (decode-char 'ucs ,binding)))) if-mule)
         (when (<= binding #xFF)
           (push `(define-key ,map ,key
                   (vector (list (decode-char 'ucs ,binding))))
                 without-mule))
         finally return `(if (featurep 'mule)
                              (progn ,@if-mule)
                          ,@without-mule))))
  (decide-on-bindings
   (compose-acute-map [space] #x0027) ;; APOSTROPHE
   (compose-acute-map [?\'] #x00B4) ;; ACUTE ACCENT
   (compose-acute-map [?A] #x00C1) ;; CAPITAL A WITH ACUTE
   (compose-acute-map [?C] #x0106) ;; CAPITAL C WITH ACUTE
   (compose-acute-map [?E] #x00C9) ;; CAPITAL E WITH ACUTE
   (compose-acute-map [?G] #x01F4) ;; CAPITAL G WITH ACUTE
   (compose-acute-map [?I] #x00CD) ;; CAPITAL I WITH ACUTE
   (compose-acute-map [?K] #x1E30) ;; CAPITAL K WITH ACUTE
   (compose-acute-map [?L] #x0139) ;; CAPITAL L WITH ACUTE
   (compose-acute-map [?M] #x1E3E) ;; CAPITAL M WITH ACUTE
   (compose-acute-map [?N] #x0143) ;; CAPITAL N WITH ACUTE
   (compose-acute-map [?O] #x00D3) ;; CAPITAL O WITH ACUTE
   (compose-acute-map [?P] #x1E54) ;; CAPITAL P WITH ACUTE
   (compose-acute-map [?R] #x0154) ;; CAPITAL R WITH ACUTE
   (compose-acute-map [?S] #x015A) ;; CAPITAL S WITH ACUTE
   (compose-acute-map [?U] #x00DA) ;; CAPITAL U WITH ACUTE
   (compose-acute-map [?W] #x1E82) ;; CAPITAL W WITH ACUTE
   (compose-acute-map [?Y] #x00DD) ;; CAPITAL Y WITH ACUTE
   (compose-acute-map [?Z] #x0179) ;; CAPITAL Z WITH ACUTE
   (compose-acute-map [?a] #x00E1) ;; SMALL A WITH ACUTE
   (compose-acute-map [?c] #x0107) ;; SMALL C WITH ACUTE
   (compose-acute-map [?e] #x00E9) ;; SMALL E WITH ACUTE
   (compose-acute-map [?g] #x01F5) ;; SMALL G WITH ACUTE
   (compose-acute-map [?i] #x00ED) ;; SMALL I WITH ACUTE
   (compose-acute-map [?k] #x1E31) ;; SMALL K WITH ACUTE
   (compose-acute-map [?l] #x013A) ;; SMALL L WITH ACUTE
   (compose-acute-map [?m] #x1E3F) ;; SMALL M WITH ACUTE
   (compose-acute-map [?n] #x0144) ;; SMALL N WITH ACUTE
   (compose-acute-map [?o] #x00F3) ;; SMALL O WITH ACUTE
   (compose-acute-map [?p] #x1E55) ;; SMALL P WITH ACUTE
   (compose-acute-map [?r] #x0155) ;; SMALL R WITH ACUTE
   (compose-acute-map [?s] #x015B) ;; SMALL S WITH ACUTE
   (compose-acute-map [?u] #x00FA) ;; SMALL U WITH ACUTE
   (compose-acute-map [?w] #x1E83) ;; SMALL W WITH ACUTE
   (compose-acute-map [?y] #x00FD) ;; SMALL Y WITH ACUTE
   (compose-acute-map [?z] #x017A) ;; SMALL Z WITH ACUTE
   (compose-grave-map [space] #x0060) ;; GRAVE ACCENT
   (compose-grave-map [?\`] #x0060) ;; GRAVE ACCENT
   (compose-grave-map [?A] #x00C0) ;; CAPITAL A WITH GRAVE
   (compose-grave-map [?E] #x00C8) ;; CAPITAL E WITH GRAVE
   (compose-grave-map [?I] #x00CC) ;; CAPITAL I WITH GRAVE
   (compose-grave-map [?N] #x01F8) ;; CAPITAL N WITH GRAVE
   (compose-grave-map [?O] #x00D2) ;; CAPITAL O WITH GRAVE
   (compose-grave-map [?U] #x00D9) ;; CAPITAL U WITH GRAVE
   (compose-grave-map [?W] #x1E80) ;; CAPITAL W WITH GRAVE
   (compose-grave-map [?Y] #x1EF2) ;; CAPITAL Y WITH GRAVE
   (compose-grave-map [?a] #x00E0) ;; SMALL A WITH GRAVE
   (compose-grave-map [?e] #x00E8) ;; SMALL E WITH GRAVE
   (compose-grave-map [?i] #x00EC) ;; SMALL I WITH GRAVE
   (compose-grave-map [?n] #x01F9) ;; SMALL N WITH GRAVE
   (compose-grave-map [?o] #x00F2) ;; SMALL O WITH GRAVE
   (compose-grave-map [?u] #x00F9) ;; SMALL U WITH GRAVE
   (compose-grave-map [?w] #x1E81) ;; SMALL W WITH GRAVE
   (compose-grave-map [?y] #x1EF3) ;; SMALL Y WITH GRAVE
   (compose-cedilla-map [space] #x002C) ;; COMMA
   (compose-cedilla-map [?\,] #x00B8) ;; CEDILLA
   (compose-cedilla-map [C] #x00C7) ;; CAPITAL C WITH CEDILLA
   (compose-cedilla-map [D] #x1E10) ;; CAPITAL D WITH CEDILLA
   (compose-cedilla-map [E] #x0228) ;; CAPITAL E WITH CEDILLA
   (compose-cedilla-map [G] #x0122) ;; CAPITAL G WITH CEDILLA
   (compose-cedilla-map [H] #x1E28) ;; CAPITAL H WITH CEDILLA
   (compose-cedilla-map [K] #x0136) ;; CAPITAL K WITH CEDILLA
   (compose-cedilla-map [L] #x013B) ;; CAPITAL L WITH CEDILLA
   (compose-cedilla-map [N] #x0145) ;; CAPITAL N WITH CEDILLA
   (compose-cedilla-map [R] #x0156) ;; CAPITAL R WITH CEDILLA
   (compose-cedilla-map [S] #x015E) ;; CAPITAL S WITH CEDILLA
   (compose-cedilla-map [T] #x0162) ;; CAPITAL T WITH CEDILLA
   (compose-cedilla-map [c] #x00E7) ;; SMALL C WITH CEDILLA
   (compose-cedilla-map [d] #x1E11) ;; SMALL D WITH CEDILLA
   (compose-cedilla-map [e] #x0229) ;; SMALL E WITH CEDILLA
   (compose-cedilla-map [g] #x0123) ;; SMALL G WITH CEDILLA
   (compose-cedilla-map [h] #x1E29) ;; SMALL H WITH CEDILLA
   (compose-cedilla-map [k] #x0137) ;; SMALL K WITH CEDILLA
   (compose-cedilla-map [l] #x013C) ;; SMALL L WITH CEDILLA
   (compose-cedilla-map [n] #x0146) ;; SMALL N WITH CEDILLA
   (compose-cedilla-map [r] #x0157) ;; SMALL R WITH CEDILLA
   (compose-cedilla-map [s] #x015F) ;; SMALL S WITH CEDILLA
   (compose-cedilla-map [t] #x0163) ;; SMALL T WITH CEDILLA
   (compose-diaeresis-map [space] #x00A8) ;; DIAERESIS
   (compose-diaeresis-map [?\"] #x00A8) ;; DIAERESIS
   (compose-diaeresis-map [?s] #x00DF) ;; SMALL SHARP S
   (compose-diaeresis-map [?A] #x00C4) ;; CAPITAL A WITH DIAERESIS
   (compose-diaeresis-map [?E] #x00CB) ;; CAPITAL E WITH DIAERESIS
   (compose-diaeresis-map [?H] #x1E26) ;; CAPITAL H WITH DIAERESIS
   (compose-diaeresis-map [?I] #x00CF) ;; CAPITAL I WITH DIAERESIS
   (compose-diaeresis-map [?O] #x00D6) ;; CAPITAL O WITH DIAERESIS
   (compose-diaeresis-map [?U] #x00DC) ;; CAPITAL U WITH DIAERESIS
   (compose-diaeresis-map [?W] #x1E84) ;; CAPITAL W WITH DIAERESIS
   (compose-diaeresis-map [?X] #x1E8C) ;; CAPITAL X WITH DIAERESIS
   (compose-diaeresis-map [?Y] #x0178) ;; CAPITAL Y WITH DIAERESIS
   (compose-diaeresis-map [?a] #x00E4) ;; SMALL A WITH DIAERESIS
   (compose-diaeresis-map [?e] #x00EB) ;; SMALL E WITH DIAERESIS
   (compose-diaeresis-map [?h] #x1E27) ;; SMALL H WITH DIAERESIS
   (compose-diaeresis-map [?i] #x00EF) ;; SMALL I WITH DIAERESIS
   (compose-diaeresis-map [?o] #x00F6) ;; SMALL O WITH DIAERESIS
   (compose-diaeresis-map [?t] #x1E97) ;; SMALL T WITH DIAERESIS
   (compose-diaeresis-map [?u] #x00FC) ;; SMALL U WITH DIAERESIS
   (compose-diaeresis-map [?w] #x1E85) ;; SMALL W WITH DIAERESIS
   (compose-diaeresis-map [?x] #x1E8D) ;; SMALL X WITH DIAERESIS
   (compose-diaeresis-map [?y] #x00FF) ;; SMALL Y WITH DIAERESIS
   (compose-circumflex-map [space] #x005e) ;; CIRCUMFLEX ACCENT
   (compose-circumflex-map [?A] #x00C2) ;; CAPITAL A WITH CIRCUMFLEX
   (compose-circumflex-map [?C] #x0108) ;; CAPITAL C WITH CIRCUMFLEX
   (compose-circumflex-map [?E] #x00CA) ;; CAPITAL E WITH CIRCUMFLEX
   (compose-circumflex-map [?G] #x011C) ;; CAPITAL G WITH CIRCUMFLEX
   (compose-circumflex-map [?H] #x0124) ;; CAPITAL H WITH CIRCUMFLEX
   (compose-circumflex-map [?I] #x00CE) ;; CAPITAL I WITH CIRCUMFLEX
   (compose-circumflex-map [?J] #x0134) ;; CAPITAL J WITH CIRCUMFLEX
   (compose-circumflex-map [?O] #x00D4) ;; CAPITAL O WITH CIRCUMFLEX
   (compose-circumflex-map [?S] #x015C) ;; CAPITAL S WITH CIRCUMFLEX
   (compose-circumflex-map [?U] #x00DB) ;; CAPITAL U WITH CIRCUMFLEX
   (compose-circumflex-map [?W] #x0174) ;; CAPITAL W WITH CIRCUMFLEX
   (compose-circumflex-map [?Y] #x0176) ;; CAPITAL Y WITH CIRCUMFLEX
   (compose-circumflex-map [?Z] #x1E90) ;; CAPITAL Z WITH CIRCUMFLEX
   (compose-circumflex-map [?a] #x00e2) ;; SMALL A WITH CIRCUMFLEX
   (compose-circumflex-map [?c] #x0109) ;; SMALL C WITH CIRCUMFLEX
   (compose-circumflex-map [?e] #x00ea) ;; SMALL E WITH CIRCUMFLEX
   (compose-circumflex-map [?g] #x011d) ;; SMALL G WITH CIRCUMFLEX
   (compose-circumflex-map [?h] #x0125) ;; SMALL H WITH CIRCUMFLEX
   (compose-circumflex-map [?i] #x00ee) ;; SMALL I WITH CIRCUMFLEX
   (compose-circumflex-map [?j] #x0135) ;; SMALL J WITH CIRCUMFLEX
   (compose-circumflex-map [?o] #x00f4) ;; SMALL O WITH CIRCUMFLEX
   (compose-circumflex-map [?s] #x015d) ;; SMALL S WITH CIRCUMFLEX
   (compose-circumflex-map [?u] #x00fb) ;; SMALL U WITH CIRCUMFLEX
   (compose-circumflex-map [?w] #x0175) ;; SMALL W WITH CIRCUMFLEX
   (compose-circumflex-map [?y] #x0177) ;; SMALL Y WITH CIRCUMFLEX
   (compose-circumflex-map [?z] #x1e91) ;; SMALL Z WITH CIRCUMFLEX
   (compose-tilde-map [space] #x007E) ;; TILDE
   (compose-tilde-map [?A] #x00C3) ;; CAPITAL A WITH TILDE
   (compose-tilde-map [?E] #x1EBC) ;; CAPITAL E WITH TILDE
   (compose-tilde-map [?I] #x0128) ;; CAPITAL I WITH TILDE
   (compose-tilde-map [?N] #x00D1) ;; CAPITAL N WITH TILDE
   (compose-tilde-map [?O] #x00D5) ;; CAPITAL O WITH TILDE
   (compose-tilde-map [?U] #x0168) ;; CAPITAL U WITH TILDE
   (compose-tilde-map [?V] #x1E7C) ;; CAPITAL V WITH TILDE
   (compose-tilde-map [?Y] #x1EF8) ;; CAPITAL Y WITH TILDE
   (compose-tilde-map [?a] #x00E3) ;; SMALL A WITH TILDE
   (compose-tilde-map [?e] #x1EBD) ;; SMALL E WITH TILDE
   (compose-tilde-map [?i] #x0129) ;; SMALL I WITH TILDE
   (compose-tilde-map [?n] #x00F1) ;; SMALL N WITH TILDE
   (compose-tilde-map [?o] #x00F5) ;; SMALL O WITH TILDE
   (compose-tilde-map [?u] #x0169) ;; SMALL U WITH TILDE
   (compose-tilde-map [?v] #x1E7D) ;; SMALL V WITH TILDE
   (compose-tilde-map [?y] #x1EF9) ;; SMALL Y WITH TILDE
   (compose-ring-map [space] #x00B0) ;; DEGREE SIGN
   (compose-ring-map [?A] #x00C5) ;; CAPITAL A WITH RING ABOVE
   (compose-ring-map [?U] #x016E) ;; CAPITAL U WITH RING ABOVE
   (compose-ring-map [?a] #x00E5) ;; SMALL A WITH RING ABOVE
   (compose-ring-map [?u] #x016F) ;; SMALL U WITH RING ABOVE
   (compose-ring-map [?w] #x1E98) ;; SMALL W WITH RING ABOVE
   (compose-ring-map [?y] #x1E99) ;; SMALL Y WITH RING ABOVE
   (compose-caron-map [space] #x02C7) ;; CARON
   (compose-caron-map [?A] #x01CD) ;; CAPITAL A WITH CARON
   (compose-caron-map [?C] #x010C) ;; CAPITAL C WITH CARON
   (compose-caron-map [?D] #x010E) ;; CAPITAL D WITH CARON
   (compose-caron-map [U01F1] #x01C4) ;; CAPITAL DZ WITH CARON
   (compose-caron-map [?E] #x011A) ;; CAPITAL E WITH CARON
   (compose-caron-map [U01B7] #x01EE) ;; CAPITAL EZH WITH CARON
   (compose-caron-map [?G] #x01E6) ;; CAPITAL G WITH CARON
   (compose-caron-map [?H] #x021E) ;; CAPITAL H WITH CARON
   (compose-caron-map [?I] #x01CF) ;; CAPITAL I WITH CARON
   (compose-caron-map [?K] #x01E8) ;; CAPITAL K WITH CARON
   (compose-caron-map [?L] #x013D) ;; CAPITAL L WITH CARON
   (compose-caron-map [?N] #x0147) ;; CAPITAL N WITH CARON
   (compose-caron-map [?O] #x01D1) ;; CAPITAL O WITH CARON
   (compose-caron-map [?R] #x0158) ;; CAPITAL R WITH CARON
   (compose-caron-map [?S] #x0160) ;; CAPITAL S WITH CARON
   (compose-caron-map [?T] #x0164) ;; CAPITAL T WITH CARON
   (compose-caron-map [?U] #x01D3) ;; CAPITAL U WITH CARON
   (compose-caron-map [?Z] #x017D) ;; CAPITAL Z WITH CARON
   (compose-caron-map [?a] #x01CE) ;; SMALL A WITH CARON
   (compose-caron-map [?c] #x010D) ;; SMALL C WITH CARON
   (compose-caron-map [?d] #x010F) ;; SMALL D WITH CARON
   (compose-caron-map [U01F3] #x01C6) ;; SMALL DZ WITH CARON
   (compose-caron-map [?e] #x011B) ;; SMALL E WITH CARON
   (compose-caron-map [U0292] #x01EF) ;; SMALL EZH WITH CARON
   (compose-caron-map [?g] #x01E7) ;; SMALL G WITH CARON
   (compose-caron-map [?h] #x021F) ;; SMALL H WITH CARON
   (compose-caron-map [?i] #x01D0) ;; SMALL I WITH CARON
   (compose-caron-map [?j] #x01F0) ;; SMALL J WITH CARON
   (compose-caron-map [?k] #x01E9) ;; SMALL K WITH CARON
   (compose-caron-map [?l] #x013E) ;; SMALL L WITH CARON
   (compose-caron-map [?n] #x0148) ;; SMALL N WITH CARON
   (compose-caron-map [?o] #x01D2) ;; SMALL O WITH CARON
   (compose-caron-map [?r] #x0159) ;; SMALL R WITH CARON
   (compose-caron-map [?s] #x0161) ;; SMALL S WITH CARON
   (compose-caron-map [?t] #x0165) ;; SMALL T WITH CARON
   (compose-caron-map [?u] #x01D4) ;; SMALL U WITH CARON
   (compose-caron-map [?z] #x017E) ;; SMALL Z WITH CARON
   (compose-macron-map [space] #x00AF) ;; MACRON
   (compose-macron-map [?A] #x0100) ;; CAPITAL A WITH MACRON  
   (compose-macron-map [AE] #x01E2) ;; CAPITAL AE WITH MACRON 
   (compose-macron-map [?E] #x0112) ;; CAPITAL E WITH MACRON  
   (compose-macron-map [?G] #x1E20) ;; CAPITAL G WITH MACRON  
   (compose-macron-map [?I] #x012A) ;; CAPITAL I WITH MACRON  
   (compose-macron-map [?O] #x014C) ;; CAPITAL O WITH MACRON  
   (compose-macron-map [?U] #x016A) ;; CAPITAL U WITH MACRON  
   (compose-macron-map [?Y] #x0232) ;; CAPITAL Y WITH MACRON  
   (compose-macron-map [?a] #x0101) ;; SMALL A WITH MACRON    
   (compose-macron-map [ae] #x01E3) ;; SMALL AE WITH MACRON   
   (compose-macron-map [?e] #x0113) ;; SMALL E WITH MACRON    
   (compose-macron-map [?g] #x1E21) ;; SMALL G WITH MACRON    
   (compose-macron-map [?i] #x012B) ;; SMALL I WITH MACRON    
   (compose-macron-map [?o] #x014D) ;; SMALL O WITH MACRON    
   (compose-macron-map [?u] #x016B) ;; SMALL U WITH MACRON    
   (compose-macron-map [?y] #x0233) ;; SMALL Y WITH MACRON    
   (compose-doubleacute-map [space] #x02DD) ;; DOUBLE ACUTE ACCENT
   (compose-doubleacute-map [?O] #x0150) ;; CAPITAL O WITH DOUBLE ACUTE
   (compose-doubleacute-map [?U] #x0170) ;; CAPITAL U WITH DOUBLE ACUTE
   (compose-doubleacute-map [?o] #x0151) ;; SMALL O WITH DOUBLE ACUTE
   (compose-doubleacute-map [?u] #x0171) ;; SMALL U WITH DOUBLE ACUTE
   (compose-ogonek-map [space] #x02DB) ;; OGONEK
   (compose-ogonek-map [?A] #x0104) ;; CAPITAL A WITH OGONEK
   (compose-ogonek-map [?E] #x0118) ;; CAPITAL E WITH OGONEK
   (compose-ogonek-map [?I] #x012E) ;; CAPITAL I WITH OGONEK
   (compose-ogonek-map [?O] #x01EA) ;; CAPITAL O WITH OGONEK
   (compose-ogonek-map [?U] #x0172) ;; CAPITAL U WITH OGONEK
   (compose-ogonek-map [?a] #x0105) ;; SMALL A WITH OGONEK
   (compose-ogonek-map [?e] #x0119) ;; SMALL E WITH OGONEK
   (compose-ogonek-map [?i] #x012F) ;; SMALL I WITH OGONEK
   (compose-ogonek-map [?o] #x01EB) ;; SMALL O WITH OGONEK
   (compose-ogonek-map [?u] #x0173) ;; SMALL U WITH OGONEK
   (compose-breve-map [space] #x02D8) ;; BREVE
   (compose-breve-map [?A] #x0102) ;; CAPITAL A WITH BREVE
   (compose-breve-map [?E] #x0114) ;; CAPITAL E WITH BREVE
   (compose-breve-map [?G] #x011E) ;; CAPITAL G WITH BREVE
   (compose-breve-map [?I] #x012C) ;; CAPITAL I WITH BREVE
   (compose-breve-map [?O] #x014E) ;; CAPITAL O WITH BREVE
   (compose-breve-map [?U] #x016C) ;; CAPITAL U WITH BREVE
   (compose-breve-map [?a] #x0103) ;; SMALL A WITH BREVE
   (compose-breve-map [?e] #x0115) ;; SMALL E WITH BREVE
   (compose-breve-map [?g] #x011F) ;; SMALL G WITH BREVE
   (compose-breve-map [?i] #x012D) ;; SMALL I WITH BREVE
   (compose-breve-map [?o] #x014F) ;; SMALL O WITH BREVE
   (compose-breve-map [?u] #x016D) ;; SMALL U WITH BREVE
   (compose-dot-map [space] #x02D9) ;; DOT ABOVE
   (compose-dot-map [?A] #x0226) ;; CAPITAL A WITH DOT ABOVE
   (compose-dot-map [?B] #x1E02) ;; CAPITAL B WITH DOT ABOVE
   (compose-dot-map [?C] #x010A) ;; CAPITAL C WITH DOT ABOVE
   (compose-dot-map [?D] #x1E0A) ;; CAPITAL D WITH DOT ABOVE
   (compose-dot-map [?E] #x0116) ;; CAPITAL E WITH DOT ABOVE
   (compose-dot-map [?F] #x1E1E) ;; CAPITAL F WITH DOT ABOVE
   (compose-dot-map [?G] #x0120) ;; CAPITAL G WITH DOT ABOVE
   (compose-dot-map [?H] #x1E22) ;; CAPITAL H WITH DOT ABOVE
   (compose-dot-map [?I] #x0130) ;; CAPITAL I WITH DOT ABOVE
   (compose-dot-map [?M] #x1E40) ;; CAPITAL M WITH DOT ABOVE
   (compose-dot-map [?N] #x1E44) ;; CAPITAL N WITH DOT ABOVE
   (compose-dot-map [?O] #x022E) ;; CAPITAL O WITH DOT ABOVE
   (compose-dot-map [?P] #x1E56) ;; CAPITAL P WITH DOT ABOVE
   (compose-dot-map [?R] #x1E58) ;; CAPITAL R WITH DOT ABOVE
   (compose-dot-map [?S] #x1E60) ;; CAPITAL S WITH DOT ABOVE
   (compose-dot-map [?T] #x1E6A) ;; CAPITAL T WITH DOT ABOVE
   (compose-dot-map [?W] #x1E86) ;; CAPITAL W WITH DOT ABOVE
   (compose-dot-map [?X] #x1E8A) ;; CAPITAL X WITH DOT ABOVE
   (compose-dot-map [?Y] #x1E8E) ;; CAPITAL Y WITH DOT ABOVE
   (compose-dot-map [?Z] #x017B) ;; CAPITAL Z WITH DOT ABOVE
   (compose-dot-map [?a] #x0227) ;; SMALL A WITH DOT ABOVE
   (compose-dot-map [?b] #x1E03) ;; SMALL B WITH DOT ABOVE
   (compose-dot-map [?c] #x010B) ;; SMALL C WITH DOT ABOVE
   (compose-dot-map [?d] #x1E0B) ;; SMALL D WITH DOT ABOVE
   (compose-dot-map [?e] #x0117) ;; SMALL E WITH DOT ABOVE
   (compose-dot-map [?f] #x1E1F) ;; SMALL F WITH DOT ABOVE
   (compose-dot-map [?g] #x0121) ;; SMALL G WITH DOT ABOVE
   (compose-dot-map [?h] #x1E23) ;; SMALL H WITH DOT ABOVE
   (compose-dot-map [U017F] #x1E9B) ;; SMALL LONG S WITH DOT ABOVE
   (compose-dot-map [?m] #x1E41) ;; SMALL M WITH DOT ABOVE
   (compose-dot-map [?n] #x1E45) ;; SMALL N WITH DOT ABOVE
   (compose-dot-map [?o] #x022F) ;; SMALL O WITH DOT ABOVE
   (compose-dot-map [?p] #x1E57) ;; SMALL P WITH DOT ABOVE
   (compose-dot-map [?r] #x1E59) ;; SMALL R WITH DOT ABOVE
   (compose-dot-map [?s] #x1E61) ;; SMALL S WITH DOT ABOVE
   (compose-dot-map [?t] #x1E6B) ;; SMALL T WITH DOT ABOVE
   (compose-dot-map [?w] #x1E87) ;; SMALL W WITH DOT ABOVE
   (compose-dot-map [?x] #x1E8B) ;; SMALL X WITH DOT ABOVE
   (compose-dot-map [?y] #x1E8F) ;; SMALL Y WITH DOT ABOVE
   (compose-dot-map [?z] #x017C) ;; SMALL Z WITH DOT ABOVE
   (compose-dot-map [?i] #x0131) ;; SMALL DOTLESS I
   (compose-dot-map [?j] #x0237) ;; SMALL DOTLESS J
   ;; There is nothing obvious we can bind space to on compose-hook-map,
   ;; these are IPA characters that are in Unicode theory not
   ;; precomposed.
   (compose-hook-map [?B] #x0181) ;; CAPITAL B WITH HOOK
   (compose-hook-map [?C] #x0187) ;; CAPITAL C WITH HOOK
   (compose-hook-map [?D] #x018A) ;; CAPITAL D WITH HOOK
   (compose-hook-map [?F] #x0191) ;; CAPITAL F WITH HOOK
   (compose-hook-map [?G] #x0193) ;; CAPITAL G WITH HOOK
   (compose-hook-map [?K] #x0198) ;; CAPITAL K WITH HOOK
   (compose-hook-map [?P] #x01A4) ;; CAPITAL P WITH HOOK
   (compose-hook-map [?T] #x01AC) ;; CAPITAL T WITH HOOK
   (compose-hook-map [?V] #x01B2) ;; CAPITAL V WITH HOOK
   (compose-hook-map [?Y] #x01B3) ;; CAPITAL Y WITH HOOK
   (compose-hook-map [?Z] #x0224) ;; CAPITAL Z WITH HOOK
   (compose-hook-map [U0262] #x029B) ;; SMALL CAPITAL G WITH HOOK
   (compose-hook-map [?b] #x0253) ;; SMALL B WITH HOOK
   (compose-hook-map [?c] #x0188) ;; SMALL C WITH HOOK
   (compose-hook-map [?d] #x0257) ;; SMALL D WITH HOOK
   (compose-hook-map [?f] #x0192) ;; SMALL F WITH HOOK
   (compose-hook-map [?g] #x0260) ;; SMALL G WITH HOOK
   (compose-hook-map [?h] #x0266) ;; SMALL H WITH HOOK
   (compose-hook-map [U0266] #x0267) ;; SMALL HENG WITH HOOK
   (compose-hook-map [?k] #x0199) ;; SMALL K WITH HOOK
   (compose-hook-map [?m] #x0271) ;; SMALL M WITH HOOK
   (compose-hook-map [?p] #x01A5) ;; SMALL P WITH HOOK
   (compose-hook-map [?q] #x02A0) ;; SMALL Q WITH HOOK
   (compose-hook-map [U025C] #x025D) ;; SMALL REVERSED OPEN E WITH HOOK
   (compose-hook-map [?s] #x0282) ;; SMALL S WITH HOOK
   (compose-hook-map [U0259] #x025A) ;; SMALL SCHWA WITH HOOK
   (compose-hook-map [?t] #x01AD) ;; SMALL T WITH HOOK
   (compose-hook-map [U0279] #x027B) ;; SMALL TURNED R WITH HOOK
   (compose-hook-map [?v] #x028B) ;; SMALL V WITH HOOK
   (compose-hook-map [?y] #x01B4) ;; SMALL Y WITH HOOK
   (compose-hook-map [?z] #x0225) ;; SMALL Z WITH HOOK
   (compose-horn-map [space] #x031b)
   (compose-horn-map [?O] #x01A0) ;; CAPITAL O WITH HORN
   (compose-horn-map [?U] #x01AF) ;; CAPITAL U WITH HORN
   (compose-horn-map [?o] #x01A1) ;; SMALL O WITH HORN
   (compose-horn-map [?u] #x01B0))) ;; SMALL U WITH HORN


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


;; Make colon equivalent to doublequote for diaeresis processing.  Some
;; Xlibs do this.
(flet ((alias-colon-to-doublequote (keymap)
         (map-keymap
          #'(lambda (key value)
              (when (keymapp value)
                (alias-colon-to-doublequote value))
              (when (eq key '\")
                (define-key keymap ":" value)))
          keymap)))
  (alias-colon-to-doublequote compose-map))

;;; Electric dead keys: making a' mean a-acute.


(defun electric-diacritic (&optional count)
  "Modify the previous character with an accent.
For example, if `:' is bound to this command, then typing `a:'
will first insert `a' and then turn it into `\344' (adiaeresis).
The minimum list of keys to which this command may be bound (and the accents
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
      (when (and (vectorp mod-char) (= (length mod-char) 1))
        (setq mod-char (aref mod-char 0))
        (if (and (consp mod-char) (= (length mod-char) 1)
                 (characterp (car mod-char)))
            (setq mod-char (car mod-char))))
      (if (and mod-char (symbolp mod-char))
	  (setq mod-char (or (get-character-of-keysym mod-char) mod-char)))
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


(provide 'x-compose)

;;; x-compose.el ends here
