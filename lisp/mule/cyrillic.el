;; Cyrillic specific utilityes for Mule
;; Copyright (C) 1995 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of GNU Emacs).
;; This file contains Cyrillic characters (ISO8859-5).

;; Mule is free software distributed in the form of patches to GNU Emacs.
;; You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; Mule is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; 95.6.21   modified for Mule Ver.2.2.2 by TAKAHASHI N. <ntakahas@etl.go.jp> 
;;;	Added Alternativnyj utilities.

;;; KOI8 staff

(define-ccl-program ccl-read-koi8
  '(((read r0)
     (loop
	 (write-read-repeat
	  r0
          [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
	   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
	   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
	   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
	   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
	   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
	   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
	   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
	   128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	   144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
	    32  32  32 ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32
	    32  32  32 ?,L!(B   32  32  32  32  32  32  32  32  32  32  32  32
	   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B 
	   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B 
	   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B 
	   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]))))
  "CCL program to read KOI8.")

(define-ccl-program ccl-write-koi8
  '(((read r0)
     (loop
	 (if (r0 != 140)		; lc-crl == 140
	     (write-read-repeat r0)
	   ((read r0)
	    (r0 -= 160)
	    (write-read-repeat
	     r0
	     [ 32 179  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	      225 226 247 231 228 229 246 250 233 234 235 236 237 238 239 240
	      242 243 244 245 230 232 227 254 251 253 255 249 248 252 224 241
	      193 194 215 199 196 197 214 218 201 202 203 204 205 206 207 208
	      210 211 212 213 198 200 195 222 219 221 223 217 216 220 192 209
	       32 163  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
	    )))))
  "CCL program to write KOI8.")

(make-coding-system
 'koi8 'ccl
 "Coding-system used for KOI8."
 `(decode ,ccl-read-koi8
   encode ,ccl-write-koi8
   mnemonic "KOI8"))

(define-ccl-program ccl-cyrillic-to-koi8
  '(((r1 -= 160)
     (r1 = r1
      [ 32 179  32  32  32  32  32  32  32  32  32  32  32  32  32  32
       225 226 247 231 228 229 246 250 233 234 235 236 237 238 239 240
       242 243 244 245 230 232 227 254 251 253 255 249 248 252 224 241
       193 194 215 199 196 197 214 218 201 202 203 204 205 206 207 208
       210 211 212 213 198 200 195 222 219 221 223 217 216 220 192 209
        32 163  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
     ))
  "CCL program to convert chars of 'cyrillic to KOI font.")

(defun set-cyrillic-environment-koi8 ()
  "Make KOI8 the default character set for cyrillic."
  (set-coding-category-system 'iso-8-designate 'koi8)

  (set-coding-priority-list '(iso-8-designate iso-8-1))

  (set-default-file-coding-system 'koi8)
  (set-terminal-coding-system 'koi8)

  (setq-default quail-current-package (assoc "yawerty" quail-package-alist))

  (set-charset-ccl-program 'cyrillic ccl-cyrillic-to-koi8)
  )


;;; Alternativnyj staff

(define-ccl-program ccl-read-alternativnyj
  '(((read r0)
     (loop
	 (write-read-repeat
	  r0
          [  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
	    16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
	    32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
	    48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
	    64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
	    80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
	    96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
	   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
	    ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
	    ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
	    ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
	    32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	    32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	    32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	    ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
	    ?,L!(B  ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32  32 ?,Lp(B]))))
  "CCL program to read Alternativnyj.")

(define-ccl-program ccl-write-alternativnyj
  '(((read r0)
     (loop
	 (if (r0 != 140)		; lc-crl == 140
	     (write-read-repeat r0)
	   ((read r0)
	    (r0 -= 160)
	    (write-read-repeat
	     r0
	     [ 32 240  32  32  32  32  32  32  32  32  32  32  32  32  32  32
	      128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	      144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
	      160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
	      224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
	      255 241  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
	    )))))
  "CCL program to write Alternativnyj.")
	     
(make-coding-system
 'alternativnyj 'ccl
 "Coding-system used for Alternativnyj."
 `(decode ,ccl-read-alternativnyj
   encode ,ccl-write-alternativnyj
   mnemonic "Alt.ivnyj"))

(define-ccl-program ccl-cyrillic-to-alternativnyj
  '(((r1 -= 160)
     (r1 = r1
      [ 32 240  32  32  32  32  32  32  32  32  32  32  32  32  32  32
       128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
       144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
       160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
       224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
       255 241  32  32  32  32  32  32  32  32  32  32  32  32  32  32])
     ))
  "CCL program to convert chars of 'cyrillic to Alternativnyj font.")

(defun set-cyrillic-environment-alternativnyj ()
  "Make Alternativnyj the default character set for cyrillic."
  (set-coding-category-system 'iso-8-designate 'alternativnyj-dos)

  (set-coding-priority-list '(iso-8-designate iso-8-1))

  (set-default-file-coding-system 'alternativnyj-dos)
  (set-terminal-coding-system 'alternativnyj-dos)

  (setq-default quail-current-package (assoc "yawerty" quail-package-alist))

  (set-charset-ccl-program 'cyrillic ccl-cyrillic-to-alternativnyj)
  )

(provide 'cyrillic)
