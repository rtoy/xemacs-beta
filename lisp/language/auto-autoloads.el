;;; DO NOT MODIFY THIS FILE
(if (featurep 'language-autoloads) (error "Already loaded"))

;;;### (autoloads (encode-hz-buffer encode-hz-region decode-hz-buffer decode-hz-region setup-chinese-big5-environment setup-chinese-gb-environment) "china-util" "language/china-util.el")

(autoload 'setup-chinese-gb-environment "china-util" "\
Setup multilingual environment (MULE) for Chinese GB2312 users." t nil)

(autoload 'setup-chinese-big5-environment "china-util" "\
Setup multilingual environment (MULE) for Chinese Big5 users." t nil)

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

;;;### (autoloads (standard-display-cyrillic-translit setup-cyrillic-alternativnyj-environment setup-cyrillic-koi8-environment setup-cyrillic-iso-environment) "cyril-util" "language/cyril-util.el")

(autoload 'setup-cyrillic-iso-environment "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic ISO-8859-5 users." t nil)

(autoload 'setup-cyrillic-koi8-environment "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic KOI8 users." t nil)

(autoload 'setup-cyrillic-alternativnyj-environment "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic ALTERNATIVNYJ users." t nil)

(autoload 'standard-display-cyrillic-translit "cyril-util" "\
Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument is a string which specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are listed in 'cyrillic-language-alist'.
If the argument is t, we use the default cyrillic transliteration.
If the argument is nil, we return the display table to its standard state." t nil)

;;;***

;;;### (autoloads (read-hiragana-string japanese-zenkaku-region japanese-hankaku-region japanese-hiragana-region japanese-katakana-region japanese-zenkaku japanese-hankaku japanese-hiragana japanese-katakana setup-japanese-environment) "japan-util" "language/japan-util.el")

(autoload 'setup-japanese-environment "japan-util" "\
Setup multilingual environment (MULE) for Japanese." t nil)

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

(provide 'language-autoloads)
