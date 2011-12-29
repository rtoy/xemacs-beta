;;; mule-win32-init.el --- initialization code for MS Windows/Cygwin under MULE
;;; Copyright (C) 2001, 2002, 2010 Ben Wing.

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

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

