;;; msw-mouse.el --- Mouse support for MS Windows.

;; Copyright (C) 1998 Kirill M. Katsnelson

;; Maintainer: XEmacs Development Team
;; Keywords: mouse, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when Windows support is compiled in).

;;; Code:

(set-glyph-image text-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "Ibeam"])
(set-glyph-image nontext-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "Normal"])
(set-glyph-image selection-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "Normal"])
(set-glyph-image modeline-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "SizeNS"])
(set-glyph-image divider-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "SizeWE"])
(set-glyph-image busy-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "Wait"])
(set-glyph-image gc-pointer-glyph
		 [mswindows-resource :resource-type cursor :resource-id "Wait"])

;;; msw-mouse.el ends here
