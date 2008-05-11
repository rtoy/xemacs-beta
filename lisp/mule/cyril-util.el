;;; cyril-util.el ---  utilities for Cyrillic scripts -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997  Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

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

;;; #### Remove this, use the one in packages instead, but with the below
;;; standard-display-cyrillic-translit. This file is unfortunately shadowed
;;; if you have the Mule packages installed!

;;; Code:


;; Display 

;; Written by Valery Alexeev <valery@math.uga.edu>.

(defvar cyrillic-language-alist
      '(("Belorussian") ("Bulgarian") ("Macedonian") ("Russian") ("Serbian")
        ("Ukrainian"))
      "*List of known Cyrillic languages")

;;;###autoload
(defun standard-display-cyrillic-translit (&optional cyrillic-language
					   disable)
  "Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument is a string which specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are listed in 'cyrillic-language-alist'.

Specifying a prefix arg, by preceding
\\[standard-display-cyrillic-translit] with \\[universal-argument]
turns off Cyrillic display.  Noninteractively, the DISABLE argument
does the same thing.  "
  (interactive
   (list
    (let* ((completion-ignore-case t)
	   (default-language (if (assoc-ignore-case
				  current-language-environment
				  cyrillic-language-alist)
				 current-language-environment
			       "Russian")))
      (or current-prefix-arg
	  (completing-read
	   (format "Cyrillic language (default %s): " default-language)
	   cyrillic-language-alist nil t nil nil default-language)))))
  (frob-display-table
   (lambda (display-table)
     (if (or disable current-prefix-arg)
         (if (char-table-p display-table)
             (remove-char-table 'cyrillic-iso8859-5 display-table))
       (put-display-table ?,LP(B "a"   display-table)
       (put-display-table ?,LQ(B "b"   display-table)
       (put-display-table ?,LR(B "v"   display-table)
       (put-display-table ?,LS(B "g"   display-table)
       (put-display-table ?,LT(B "d"   display-table)
       (put-display-table ?,LU(B "e"   display-table)
       (put-display-table ?,Lq(B "yo"  display-table)
       (put-display-table ?,LV(B "zh"  display-table)
       (put-display-table ?,LW(B "z"   display-table)
       (put-display-table ?,LX(B "i"   display-table)
       (put-display-table ?,LY(B "j"   display-table)
       (put-display-table ?,LZ(B "k"   display-table)
       (put-display-table ?,L[(B "l"   display-table)
       (put-display-table ?,L\(B "m"   display-table)
       (put-display-table ?,L](B "n"   display-table)
       (put-display-table ?,L^(B "o"   display-table)
       (put-display-table ?,L_(B "p"   display-table)
       (put-display-table ?,L`(B "r"   display-table)
       (put-display-table ?,La(B "s"   display-table)
       (put-display-table ?,Lb(B "t"   display-table)
       (put-display-table ?,Lc(B "u"   display-table)
       (put-display-table ?,Ld(B "f"   display-table)
       (put-display-table ?,Le(B "kh"  display-table)
       (put-display-table ?,Lf(B "ts"  display-table)
       (put-display-table ?,Lg(B "ch"  display-table)
       (put-display-table ?,Lh(B "sh"  display-table)
       (put-display-table ?,Li(B "sch" display-table)
       (put-display-table ?,Lj(B "~"   display-table)
       (put-display-table ?,Lk(B "y"   display-table)
       (put-display-table ?,Ll(B "'"   display-table)
       (put-display-table ?,Lm(B "e'"  display-table)
       (put-display-table ?,Ln(B "yu"  display-table)
       (put-display-table ?,Lo(B "ya"  display-table)
       (put-display-table ?,L0(B "A"   display-table)
       (put-display-table ?,L1(B "B"   display-table)
       (put-display-table ?,L2(B "V"   display-table)
       (put-display-table ?,L3(B "G"   display-table)
       (put-display-table ?,L4(B "D"   display-table)
       (put-display-table ?,L5(B "E"   display-table)
       (put-display-table ?,L!(B "Yo"  display-table)
       (put-display-table ?,L6(B "Zh"  display-table)
       (put-display-table ?,L7(B "Z"   display-table)
       (put-display-table ?,L8(B "I"   display-table)
       (put-display-table ?,L9(B "J"   display-table)
       (put-display-table ?,L:(B "K"   display-table)
       (put-display-table ?,L;(B "L"   display-table)
       (put-display-table ?,L<(B "M"   display-table)
       (put-display-table ?,L=(B "N"   display-table)
       (put-display-table ?,L>(B "O"   display-table)
       (put-display-table ?,L?(B "P"   display-table)
       (put-display-table ?,L@(B "R"   display-table)
       (put-display-table ?,LA(B "S"   display-table)
       (put-display-table ?,LB(B "T"   display-table)
       (put-display-table ?,LC(B "U"   display-table)
       (put-display-table ?,LD(B "F"   display-table)
       (put-display-table ?,LE(B "Kh"  display-table)
       (put-display-table ?,LF(B "Ts"  display-table)
       (put-display-table ?,LG(B "Ch"  display-table)
       (put-display-table ?,LH(B "Sh"  display-table)
       (put-display-table ?,LI(B "Sch" display-table)
       (put-display-table ?,LJ(B "~"   display-table)
       (put-display-table ?,LK(B "Y"   display-table)
       (put-display-table ?,LL(B "'"   display-table)
       (put-display-table ?,LM(B "E'"  display-table)
       (put-display-table ?,LN(B "Yu"  display-table)
       (put-display-table ?,LO(B "Ya"  display-table)
       (put-display-table ?,Lt(B "ie"  display-table)
       (put-display-table ?,Lw(B "i"   display-table)
       (put-display-table ?,L~(B "u"   display-table)
       (put-display-table ?,Lr(B "dj"  display-table)
       (put-display-table ?,L{(B "chj" display-table)
       (put-display-table ?,Ls(B "gj"  display-table)
       (put-display-table ?,Lu(B "s"   display-table)
       (put-display-table ?,L|(B "k"   display-table)
       (put-display-table ?,Lv(B "i"   display-table)
       (put-display-table ?,Lx(B "j"   display-table)
       (put-display-table ?,Ly(B "lj"  display-table)
       (put-display-table ?,Lz(B "nj"  display-table)
       (put-display-table ?,L(B "dz"  display-table)
       (put-display-table ?,L$(B "Ye"  display-table)
       (put-display-table ?,L'(B "Yi"  display-table)
       (put-display-table ?,L.(B "U"   display-table)
       (put-display-table ?,L"(B "Dj"  display-table)
       (put-display-table ?,L+(B "Chj" display-table)
       (put-display-table ?,L#(B "Gj"  display-table)
       (put-display-table ?,L%(B "S"   display-table)
       (put-display-table ?,L,(B "K"   display-table)
       (put-display-table ?,L&(B "I"   display-table)
       (put-display-table ?,L((B "J"   display-table)
       (put-display-table ?,L)(B "Lj"  display-table)
       (put-display-table ?,L*(B "Nj"  display-table)
       (put-display-table ?,L/(B "Dj"  display-table)
    
       (when (equal cyrillic-language "Bulgarian")
         (put-display-table ?,Li(B "sht"  display-table)
         (put-display-table ?,LI(B "Sht"  display-table)
         (put-display-table ?,Ln(B "iu"   display-table)
         (put-display-table ?,LN(B "Iu"   display-table)
         (put-display-table ?,Lo(B "ia"   display-table)
         (put-display-table ?,LO(B "Ia"   display-table))

       (when (equal cyrillic-language "Ukrainian") ; based on the official 
                                        ; transliteration table
         (put-display-table ?,LX(B "y"    display-table)
         (put-display-table ?,L8(B "Y"    display-table)
         (put-display-table ?,LY(B "i"    display-table)
         (put-display-table ?,L9(B "Y"    display-table)
         (put-display-table ?,Ln(B "iu"   display-table)
         (put-display-table ?,Lo(B "ia"   display-table)))) nil))
;;
(provide 'cyril-util)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyril-util.el ends here
