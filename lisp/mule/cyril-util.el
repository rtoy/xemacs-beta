;;; cyril-util.el ---  utilities for Cyrillic scripts -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997  Free Software Foundation, Inc.
;; Copyright (C) 2002, 2005 Ben Wing.

;; Keywords: mule, multilingual, Cyrillic

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

;;; Synched up with: Emacs 21.1 (language/cyril-util.el).

;;; Commentary:

;;; Code:


;; Display 

;; Written by Valery Alexeev <valery@math.uga.edu>.

(defvar cyrillic-language-alist
      (list '("Belorussian") '("Bulgarian") '("Macedonian") 
	    '("Russian") '("Serbian") '("Ukrainian"))
      "*List of known cyrillic languages")

(defvar standard-display-table)

;;;###autoload
(defun standard-display-cyrillic-translit (&optional cyrillic-language)
  "Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument is a string which specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are listed in 'cyrillic-language-alist'.
If the argument is t, we use the default cyrillic transliteration.
If the argument is nil, we return the display table to its standard state."
  (interactive
   (list
    (let* ((completion-ignore-case t))
      (completing-read
       "Cyrillic language (default nil): "
       cyrillic-language-alist nil t nil nil nil))))

  (or standard-display-table
      (setq standard-display-table (make-display-table)))

  (if (equal cyrillic-language "")
      (setq cyrillic-language nil))

  (if (null cyrillic-language)
      (setq standard-display-table (make-display-table))
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd0)  [?a]) ;?,LP(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd1)  [?b]) ;?,LQ(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd2)  [?v]) ;?,LR(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd3)  [?g]) ;?,LS(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd4)  [?d]) ;?,LT(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd5)  [?e]) ;?,LU(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf1)  [?y?o]) ;?,Lq(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd6)  [?z?h]) ;?,LV(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd7)  [?z]) ;?,LW(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd8)  [?i]) ;?,LX(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd9)  [?j]) ;?,LY(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xda)  [?k]) ;?,LZ(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xdb)  [?l]) ;?,L[(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xdc)  [?m]) ;?,L\(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xdd)  [?n]) ;?,L](B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xde)  [?o]) ;?,L^(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xdf)  [?p]) ;?,L_(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe0)  [?r]) ;?,L`(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe1)  [?s]) ;?,La(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe2)  [?t]) ;?,Lb(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe3)  [?u]) ;?,Lc(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe4)  [?f]) ;?,Ld(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe5)  [?k?h]) ;?,Le(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe6)  [?t?s]) ;?,Lf(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe7)  [?c?h]) ;?,Lg(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe8)  [?s?h]) ;?,Lh(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe9)  [?s?c?h]) ;?,Li(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xea)  [?~]) ;?,Lj(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xeb)  [?y]) ;?,Lk(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xec)  [?']) ;?,Ll(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xed)  [?e?']) ;?,Lm(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xee)  [?y?u]) ;?,Ln(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xef)  [?y?a]) ;?,Lo(B
    
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb0)  [?A]) ;?,L0(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb1)  [?B]) ;?,L1(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb2)  [?V]) ;?,L2(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb3)  [?G]) ;?,L3(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb4)  [?D]) ;?,L4(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb5)  [?E]) ;?,L5(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa1)  [?Y?o]) ;?,L!(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb6)  [?Z?h]) ;?,L6(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb7)  [?Z]) ;?,L7(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb8)  [?I]) ;?,L8(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb9)  [?J]) ;?,L9(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xba)  [?K]) ;?,L:(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xbb)  [?L]) ;?,L;(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xbc)  [?M]) ;?,L<(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xbd)  [?N]) ;?,L=(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xbe)  [?O]) ;?,L>(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xbf)  [?P]) ;?,L?(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc0)  [?R]) ;?,L@(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc1)  [?S]) ;?,LA(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc2)  [?T]) ;?,LB(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc3)  [?U]) ;?,LC(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc4)  [?F]) ;?,LD(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc5)  [?K?h]) ;?,LE(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc6)  [?T?s]) ;?,LF(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc7)  [?C?h]) ;?,LG(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc8)  [?S?h]) ;?,LH(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc9)  [?S?c?h]) ;?,LI(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xca)  [?~]) ;?,LJ(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xcb)  [?Y]) ;?,LK(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xcc)  [?']) ;?,LL(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xcd)  [?E?']) ;?,LM(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xce)  [?Y?u]) ;?,LN(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xcf)  [?Y?a]) ;?,LO(B
    
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf4)  [?i?e]) ;?,Lt(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf7)  [?i]) ;?,Lw(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xfe)  [?u]) ;?,L~(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf2)  [?d?j]) ;?,Lr(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xfb)  [?c?h?j]) ;?,L{(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf3)  [?g?j]) ;?,Ls(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf5)  [?s]) ;?,Lu(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xfc)  [?k]) ;?,L|(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf6)  [?i]) ;?,Lv(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf8)  [?j]) ;?,Lx(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xf9)  [?l?j]) ;?,Ly(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xfa)  [?n?j]) ;?,Lz(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xff)  [?d?z]) ;?,L(B
    
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa4)  [?Y?e]) ;?,L$(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa7)  [?Y?i]) ;?,L'(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xae)  [?U]) ;?,L.(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa2)  [?D?j]) ;?,L"(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xab)  [?C?h?j]) ;?,L+(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa3)  [?G?j]) ;?,L#(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa5)  [?S]) ;?,L%(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xac)  [?K]) ;?,L,(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa6)  [?I]) ;?,L&(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa8)  [?J]) ;?,L((B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xa9)  [?L?j]) ;?,L)(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xaa)  [?N?j]) ;?,L*(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xaf)  [?D?j]) ;?,L/(B
    
    (when (equal cyrillic-language "Bulgarian")
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xe9) [?s?h?t]) ;?,Li(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xc9) [?S?h?t]) ;?,LI(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xee) [?i?u]) ;?,Ln(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xce) [?I?u]) ;?,LN(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xef) [?i?a]) ;?,Lo(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xcf) [?I?a])) ;?,LO(B
    
    (when (equal cyrillic-language "Ukrainian") ; based on the official 
					; transliteration table
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd8) [?y]) ;?,LX(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb8) [?Y]) ;?,L8(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xd9) [?i]) ;?,LY(B
      (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xb9) [?Y]) ;?,L9(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xee) [?i?u]) ;?,Ln(B
    (aset standard-display-table (make-char 'cyrillic-iso8859-5 #xef) [?i?a])))) ;?,Lo(B


;;
(provide 'cyril-util)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyril-util.el ends here
