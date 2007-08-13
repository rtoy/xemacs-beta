
;;;### (autoloads (encode-hz-buffer encode-hz-region decode-hz-buffer decode-hz-region) "china-util" "language/china-util.el")

(autoload 'decode-hz-region "china-util" "\
Decode HZ/ZW encoded text in the current region.
Return the length of resulting text." t nil)

(autoload 'decode-hz-buffer "china-util" "\
Decode HZ/ZW encoded text in the current buffer." t nil)

(autoload 'encode-hz-region "china-util" "\
Encode the text in the current region to HZ.
Return the length of resulting text." t nil)

(autoload 'encode-hz-buffer "china-util" "\
Encode the text in the current buffer to HZ." t nil)

;;;***

;;;### (autoloads (read-hiragana-string japanese-zenkaku-region japanese-hankaku-region japanese-hiragana-region japanese-katakana-region japanese-zenkaku japanese-hankaku japanese-hiragana japanese-katakana) "japan-util" "language/japan-util.el")

(autoload 'japanese-katakana "japan-util" "\
Convert argument to Katakana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument HANKAKU t means to convert to `hankaku' Katakana
 (`japanese-jisx0201-kana'), in which case return value
 may be a string even if OBJ is a character if two Katakanas are
 necessary to represent OBJ." nil nil)

(autoload 'japanese-hiragana "japan-util" "\
Convert argument to Hiragana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy." nil nil)

(autoload 'japanese-hankaku "japan-util" "\
Convert argument to `hankaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument ASCII-ONLY non-nil means to return only ASCII character." nil nil)

(autoload 'japanese-zenkaku "japan-util" "\
Convert argument to `zenkaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy." nil nil)

(autoload 'japanese-katakana-region "japan-util" "\
Convert Japanese `hiragana' chars in the region to `katakana' chars.
Optional argument HANKAKU t means to convert to `hankaku katakana' character
of which charset is `japanese-jisx0201-kana'." t nil)

(autoload 'japanese-hiragana-region "japan-util" "\
Convert Japanese `katakana' chars in the region to `hiragana'  chars." t nil)

(autoload 'japanese-hankaku-region "japan-util" "\
Convert Japanese `zenkaku' chars in the region to `hankaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument ASCII-ONLY non-nil means to convert only to ASCII char." t nil)

(autoload 'japanese-zenkaku-region "japan-util" "\
Convert hankaku' chars in the region to Japanese `zenkaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'." t nil)

(autoload 'read-hiragana-string "japan-util" "\
Read a Hiragana string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading." nil nil)

;;;***

;;;### (autoloads (thai-compose-buffer thai-compose-region) "thai-util" "language/thai-util.el")

(autoload 'thai-compose-region "thai-util" "\
Compose Thai characters in the region." t nil)

(autoload 'thai-compose-buffer "thai-util" "\
Compose Thai characters in the current buffer." t nil)

;;;***
