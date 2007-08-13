;;; vietnamese-hooks-2.el --- pre-loaded support for Vietnamese, part 2.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Synched up with: Mule 2.3.

;; See comment in vietnamese-hooks-1.el for why we split up the Vietnamese
;; stuff into two files.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIETNAMESE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ccl-program ccl-read-viscii
  '(((read r0)
      (loop
	  (write-read-repeat
	   r0
	   [ 0 1 ?-2Æ 3 4 ?Ç ?ç 7 8 9 10 11 12 13 14 15-A
	     16 17 18 19 ?-2Ö 21 22 23 24 ?Û 26 27 28 29 ?Ü 31-A
	     32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
	     48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
	     64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
	     80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
	     96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
	     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
	     ?-2Õ ?¡ ?¢ ?£ ?¤ ?¥ ?¦ ?§ ?¨ ?© ?ª ?« ?¬ ?­ ?® ?¯-A
	     ?-2° ?± ?² ?µ ?þ ?¾ ?¶ ?· ?¸ ?ö ?÷ ?ï ?ü ?û ?ø ?Ï-A
	     ?-2õ ?-1¡ ?¢ ?£ ?¤ ?¥ ?¦ ?§ ?¨ ?© ?ª ?« ?¬ ?­ ?® ?¯-A
	     ?-1° ?± ?² ?-2Þ ?½ ?-1µ ?¶ ?· ?¸ ?-2ñ ?Ñ ?× ?Ø ?-1½ ?¾ ?-2ß-A
	     ?-2à ?á ?â ?ã ?ä ?å ?-1Æ ?Ç ?-2è ?é ?ê ?ë ?ì ?í ?î ?-1Ï-A
	     ?-2ð ?-1Ñ ?-2ò ?ó ?ô ?-1Õ ?Ö ?× ?Ø ?-2ù ?ú ?-1Û ?Ü ?-2ý ?-1Þ ?ß-A
	     ?-1à ?á ?â ?ã ?ä ?å ?æ ?ç ?è ?é ?ê ?ë ?ì ?í ?î ?ï-A
	     ?-1ð ?ñ ?ò ?ó ?ô ?õ ?ö ?÷ ?ø ?ù ?ú ?û ?ü ?ý ?þ ?-2æ ]))))-A
  "CCL program to read VISCII 1.1")

(define-ccl-program ccl-write-viscii
  '(((read r0)
      (loop
       (if (r0 < 128)
	   (write-read-repeat r0)
	 (if (r0 != 154)
	     (write-read-repeat r0)
	   ((read-if (r0 == 163)
	     ((read r0)
	      (r0 -= 160)
	      (write-read-repeat
	       r0
	     [  0 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
	      176 177 178   0   0 181 182 183 184   0   0   0   0 189 190   0
	        0   0   0   0   0   0 198 199   0   0   0   0   0   0   0 207
		0 209   0   0   0 213 214 215 216   0   0 219 220   0 222 223
	      224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
	      240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 0
	      ]))
	     (if (r0 == 164)
		 ((read r0)
		  (r0 -= 160)
		  (write-read-repeat
		   r0
	       [  0 129 130 131 132 165 134 135 136 137 138 139 140 141 142 143
	        144 145 146   0   0 147 150 151 152   0   0   0   0 180 149   0
		  0   0   0   0   0   0   2   5   0   0   0   0   0   0   0 159
		  0 186   0   0   0 128  20 187 188   0   0  25  30   0 179 191
	        192 193 194 195 196 197 255   6 200 201 202 203 204 205 206 155
	        208 185 210 211 212 160 153 154 158 217 218 157 156 221 148   0
		]))
	       (write-read-repeat r0)))))))))
  "CCL program to write VISCII 1.1")

(make-coding-system
 'viscii 'ccl
 "Coding-system used for VISCII 1.1."
 `(mnemonic "VISCII"
   decode ,ccl-read-viscii
   encode ,ccl-write-viscii))

(make-coding-system
 'viqr 'no-conversion
 "Coding-system used for VIQR."
 '(mnemonic "VIQR"
   eol-type lf
   post-read-conversion vn-compose-viqr
   pre-write-conversion vn-decompose-viqr))

(define-ccl-program ccl-read-vscii
  '(((read r0)
      (loop
	  (write-read-repeat r0
       [0 ?-2ú ?ø 3 ?× ?Ø ?æ 7 8 9 10 11 12 13 14 15-A
	16 ?-2Ñ ?ß ?Ï ?Ö ?Û ?ý ?Ü 24 25 26 27 28 29 30 31-A
	32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
	48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
	64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
	80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
	96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
	112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
	?-2à ?ä ?ã ?á ?Õ ?£ ?§ ?è ?ë ?¨ ?é ?© ?® ?ì ?ï ?î-A
	?-2í ?¸ ?ò ?ö ?õ ?ó ?÷ ?µ ?¶ ?· ?Þ ?¾ ?þ ?ù ?ü ?û-A
	160 ?-2å ?â ?ê ?ô ?½ ?ß ?ð ?-1å ?â ?ê ?ô ?¾ ?ù ?ð ?-2¢-A
	192 193 194 195 196 ?-1à ?ä ?ã ?á ?Õ ?-2Æ ?-1¢ ?Æ ?Ç ?¡ ?-2Ç-A
	?-2¡ ?¥ ?¦ ?ç ?¥ ?« ?-1£ ?¥ ?¦ ?ç ?¤ ?§ ?è ?-2¬ ?-1ë ?¨-A
	?-1é ?© ?« ?¬ ?­ ?ª ?® ?ì ?ï ?-2­ ?ª ?° ?-1î ?í ?¸ ?ò-A
	?-2± ?-1ö ?õ ?ó ?÷ ?° ?± ?² ?¯ ?µ ?¶ ?· ?Þ ?¾ ?þ ?ù-A
	?-2² ?-1ü ?û ?ú ?ø ?× ?Ø ?æ ?Ñ ?ñ ?Ï ?Ö ?Û ?ý ?Ü ?-2¯]))))-A
  "CCL program to read VSCII-1.")

(define-ccl-program ccl-write-vscii
  '(((read r0)
      (loop
	  (if (r0 < 128)
	      (write-read-repeat r0)
	    (if (r0 != 154)
		(write-read-repeat r0)
	      (read-if (r0 == 163)
	       ((read r0)
		(r0 -= 160)
		(write-read-repeat
		 r0
	     [  0 190 187 198 202 199 200 203 207 209 213 210 211 212 214 232
	      229 230 231   0   0 233 234 235 222   0   0   0   0   0 237   0
	        0   0   0   0   0   0 188 189   0   0   0   0   0   0   0 250
	        0 248   0   0   0 185 251 245 246   0   0 252 254   0 236   0
	      181 184 169 183 182 168 247 201 204 208 170 206 215 221 220 216
	      174 249 223 227 171 226 225 228 244 239 243 242 241 253 238   0
	      ]))
	       (if (r0 == 164)
		((read r0)
		 (r0 -= 160)
		 (write-read-repeat
		  r0
	       [  0 192 175 133   0 196 194 134 137 139 218 197 205 217 140 255
	        219 224 240   0   0 151 152 153 145   0   0   0   0 165 155   0
		  0   0   0   0   0   0 186 191   0   0   0   0   0   0   0  19
		  0  17   0   0   0 132  20   4   5   0   0  21  23   0 154 166
	        128 131 162 130 129 161   6 195 135 138 163 136 141 144 143 142
	        167   0 146 149 164 148 147 150   2 157   1 159 158  22 156   0
		]))
		(write-read-repeat r0))))))))
  "CCL program to write VSCII-1.")

(make-coding-system
 'vscii 'ccl
 "Coding-system used for VSCII 1.1."
 `(mnemonic "VSCII"
   decode ,ccl-read-vscii
   encode ,ccl-write-vscii))

(define-ccl-program ccl-vietnamese-lower-to-viscii
  '(((r1 = r1
       [  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
          0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
	176 177 178   0   0 181 182 183 184   0   0   0   0 189 190   0
	  0   0   0   0   0   0 198 199   0   0   0   0   0   0   0 207
	  0 209   0   0   0 213 214 215 216   0   0 219 220   0 222 223
	224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
	240 241 242 243 244 245 246 247 248 249 250 251 252 253 254   0
	])))
  "CCL program to convert chars of 'vietnamese-lower to VISCII 1.1 font")

(define-ccl-program ccl-vietnamese-upper-to-viscii
  '(((r1 = r1
       [  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
          0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
	144 145 146   0   0 147 150 151 152   0   0   0   0 180 149   0
	  0   0   0   0   0   0   2   5   0   0   0   0   0   0   0 159
	  0 186   0   0   0 128  20 187 188   0   0  25  30   0 179 191
	192 193 194 195 196 197 255   6 200 201 202 203 204 205 206 155
	208 185 210 211 212 160 153 154 158 217 218 157 156 221 148   0
	])))
  "CCL program to convert chars of 'vietnamese-upper to VISCII 1.1 font")

(define-ccl-program ccl-vietnamese-lower-to-vscii
  '(((r1 = r1
       [  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0 190 187 198 202 199 200 203 207 209 213 210 211 212 214 232
	229 230 231   0   0 233 234 235 222   0   0   0   0   0 237   0
	  0   0   0   0   0   0 188 189   0   0   0   0   0   0   0 250
	  0 248   0   0   0 185 251 245 246   0   0 252 254   0 236   0
	181 184 169 183 182 168 247 201 204 208 170 206 215 221 220 216
	174 249 223 227 171 226 225 228 244 239 243 242 241 253 238   0
	])))
  "CCL program to convert chars of 'vietnamese-lower to VSCII-1 font.")

(define-ccl-program ccl-vietnamese-upper-to-vscii
  '(((r1 = r1
       [  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
	  0 192 175 133   0 196 194 134 137 139 218 197 205 217 140 255
	219 224 240   0   0 151 152 153 145   0   0   0   0 165 155   0
	  0   0   0   0   0   0 186 191   0   0   0   0   0   0   0  19
	  0  17   0   0   0 132  20   4   5   0   0  21  23   0 154 166
	128 131 162 130 129 161   6 195 135 138 163 136 141 144 143 142
	167   0 146 149 164 148 147 150   2 157   1 159 158  22 156   0
	])))
  "CCL program to convert chars of 'vietnamese-upper to VSCII-1 font.")

;; For VISCII users
(set-charset-ccl-program 'vietnamese-lower ccl-vietnamese-lower-to-viscii)
(set-charset-ccl-program 'vietnamese-upper ccl-vietnamese-upper-to-viscii)
;; For VSCII users
;; (set-charset-ccl-program 'vietnamese-lower ccl-vietnamese-lower-to-vscii)
;; (set-charset-ccl-program 'vietnamese-upper ccl-vietnamese-upper-to-vscii)

(add-hook 'quail-package-alist '("viqr" "quail-viet"))

(define-language-environment 'vietnamese
  "Vietnamese"
  (lambda ()
    ;; For VISCII users
    (set-coding-category-system 'no-conversion 'viscii)
    ;; For VSCII users
    ;; (setq coding-category-system 'binary 'vscii)
    (set-coding-priority-list '(no-conversion))
    (set-default-buffer-file-coding-system 'viscii)
    (setq-default quail-current-package (assoc "viqr" quail-package-alist))))
