;;; mule-win32-init.el --- initialization code for MS Windows/Cygwin under MULE
;;; Copyright (C) 2001, 2002 Ben Wing.

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

(make-coding-system
 'mswindows-multibyte 'mswindows-multibyte
 "MS Windows Multibyte (current code page)"
 '(mnemonic "MSW-MB"
   documentation
   "MS Windows multibyte -- current code page.

This implements the encoding specified by the current code page --
i.e. the ANSI code page corresponding to the current locale, as
returned by

  (mswindows-locale-code-page (mswindows-current-locale))
"
   locale current
   code-page ansi))

;; we temporarily aliased this to raw-text in intl-win32.c.
(define-coding-system-alias 'mswindows-multibyte-system-default nil)
(make-coding-system
 'mswindows-multibyte-system-default 'mswindows-multibyte
 "MS Windows Multibyte (system default code page)"
 '(mnemonic "MSW-MB-SysDef"
   documentation
   "MS Windows multibyte -- system default code page.

This implements the encoding specified by the system default code page
-- i.e. the ANSI code page corresponding to the system default locale,
as returned by

  (mswindows-locale-code-page (mswindows-system-default-locale))
"
   locale system-default
   code-page ansi))

(make-coding-system
 'mswindows-multibyte-user-default 'mswindows-multibyte
 "MS Windows Multibyte (user default code page)"
 '(mnemonic "MSW-MB-UserDef"
   documentation
   "MS Windows multibyte -- user default code page.

This implements the encoding specified by the user default code page
-- i.e. the ANSI code page corresponding to the user default locale,
as returned by

  (mswindows-locale-code-page (mswindows-user-default-locale))
"
   locale user-default
   code-page ansi))

(make-coding-system
 'mswindows-multibyte-oem 'mswindows-multibyte
 "MS Windows Multibyte (current OEM code page)"
 '(mnemonic "MSW-MB-OEM"
   documentation
   "MS Windows multibyte -- current OEM code page.

This implements the encoding specified by the current OEM code page
-- i.e. the OEM code page corresponding to the current locale,
as returned by

  (mswindows-locale-oem-code-page (mswindows-current-locale))
"
   locale current
   code-page oem))

(make-coding-system
 'mswindows-multibyte-oem-system-default 'mswindows-multibyte
 "MS Windows Multibyte (system default OEM code page)"
 '(mnemonic "MSW-MB-OEM-SysDef"
   documentation
   "MS Windows multibyte -- system default OEM code page.

This implements the encoding specified by the system default OEM code page
-- i.e. the OEM code page corresponding to the system default locale,
as returned by

  (mswindows-locale-oem-code-page (mswindows-system-default-locale))
"
   locale system-default
   code-page oem))

(make-coding-system
 'mswindows-multibyte-oem-user-default 'mswindows-multibyte
 "MS Windows Multibyte (user default OEM code page)"
 '(mnemonic "MSW-MB-OEM-UserDef"
   documentation
   "MS Windows multibyte -- user default OEM code page.

This implements the encoding specified by the user default OEM code page
-- i.e. the OEM code page corresponding to the user default locale,
as returned by

  (mswindows-locale-oem-code-page (mswindows-user-default-locale))
"
   locale user-default
   code-page oem))

(let ((cplist
       '(("EBCDIC"      037 "EBCDIC")
	 ("OEM"         437 "MS-DOS United States")
	 ("EBCDIC"      500 "EBCDIC \"500V1\"")
	 ("OEM"         708 "Arabic (ASMO 708)")
	 ("OEM"         709 "Arabic (ASMO 449+, BCON V4)")
	 ("OEM"         710 "Arabic (Transparent Arabic)")
	 ("OEM"         720 "Arabic (Transparent ASMO)")
	 ("OEM"         737 "Greek (formerly 437G)")
	 ("OEM"         775 "Baltic")
	 ("OEM"         850 "MS-DOS Multilingual (Latin I)")
	 ("OEM"         852 "MS-DOS Slavic (Latin II)")
	 ("OEM"         855 "IBM Cyrillic (primarily Russian)")
	 ("OEM"         857 "IBM Turkish")
	 ("OEM"         860 "MS-DOS Portuguese")
	 ("OEM"         861 "MS-DOS Icelandic")
	 ("OEM"         862 "Hebrew")
	 ("OEM"         863 "MS-DOS Canadian-French")
	 ("OEM"         864 "Arabic")
	 ("OEM"         865 "MS-DOS Nordic")
	 ("OEM"         866 "MS-DOS Russian")
	 ("OEM"         869 "IBM Modern Greek")
	 ("Ansi/OEM"    874 "Thai")
	 ("EBCDIC"      875 "EBCDIC")
	 ("Ansi/OEM"    932 "Japanese")
	 ("Ansi/OEM"    936 "Chinese (PRC, Singapore)")
	 ("Ansi/OEM"    949 "Korean")
	 ("Ansi/OEM"    950 "Chinese (Taiwan; Hong Kong SAR, PRC)")
	 ("EBCDIC"      1026 "EBCDIC")
	 ("ANSI"        1200 "Unicode (BMP of ISO 10646)")
	 ("ANSI"        1250 "Windows 3.1 Eastern European")
	 ("ANSI"        1251 "Windows 3.1 Cyrillic")
	 ("ANSI"        1252 "Windows 3.1 US (ANSI)")
	 ("ANSI"        1253 "Windows 3.1 Greek")
	 ("ANSI"        1254 "Windows 3.1 Turkish")
	 ("ANSI"        1255 "Hebrew")
	 ("ANSI"        1256 "Arabic")
	 ("ANSI"        1257 "Baltic")
	 ("ANSI"        1258 "VietNam")
	 ("Ansi/OEM"    1361 "Korean (Johab)")
	 ("Mac"         10000 "Macintosh Roman")
	 ("Mac"         10001 "Macintosh Japanese")
	 ("Mac"         10006 "Macintosh Greek I")
	 ("Mac"         10007 "Macintosh Cyrillic")
	 ("Mac"         10029 "Macintosh Latin 2")
	 ("Mac"         10079 "Macintosh Icelandic")
	 ("Mac"         10081 "Macintosh Turkish"))))
  (dolist (cpprops cplist)
    (let ((ansioem (first cpprops))
	  (cp (second cpprops))
	  (name (third cpprops)))
      (make-coding-system
       (intern (format "windows-%s" cp))
       'mswindows-multibyte
       (format "MS Windows code page %s (%s, %s)" cp ansioem name)
       `(mnemonic
	 ,(format "MSW-%s" cp)
	 code-page ,cp
	 documentation
	 ,(format
	  "MS Windows Multibyte -- code page %s (%s, %s).

This implements the encoding specified by code page %s.
For more information on code pages, see `mswindows-charset-code-page'."
	  cp ansioem name cp))))))
