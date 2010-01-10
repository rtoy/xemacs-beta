;;; canna-leim.el --- Canna-related code for LEIM -*- coding: iso-2022-7bit; -*-
;; Copyright (C) 1997  Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Shamelessly ripped off from
;;
;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997
;; Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;; Author: Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Version: canna-leim.el,v 1.2 1997/10/27 10:08:49 steve Exp
;; Keywords: japanese, input method, LEIM
;; Last Modified: 1997/10/27 10:08:49

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; TODO
;;
;;  Add pointers to Canna documentation in LEIM format

(globally-declare-boundp 'canna:*japanese-mode*)
(globally-declare-fboundp '(canna canna-toggle-japanese-mode))

;; use (locate-file "canna_api" module-load-path module-extensions)
;; to locate the module without loading it.
;; then we could autoload canna-activate and dump and/or autoload:
;; (and (locate-file "canna_api" module-load-path module-extensions)
;;      (locate-library "canna")
;;      (register-input-method 'japanese-canna "Japanese" 'canna-activate nil
;;       "Canna - a kana to kanji conversion program"))

(defun canna-activate (&optional name)
  ;; XEmacs 21.5.10 and later have 3-argument require.
  (unless (require 'CANNA "canna_api" 'no-error)
    (error 'file-error
	   "No Canna API support!? See M-x describe-installation & C-h v module-load-path."))
  (unless (require 'canna nil 'no-error)
    (error 'file-error
	   "Canna LISP support not found.  Is the mule-base package missing?"))
  (setq inactivate-current-input-method-function 'canna-inactivate)
  (unless (featurep 'leim-canna-initialized)
    (canna)
    (provide 'leim-canna-initialized))
  (canna-toggle-japanese-mode))

(defun canna-inactivate ()
  (cond (canna:*japanese-mode* (canna-toggle-japanese-mode))))

(register-input-method
 'japanese-canna "Japanese"
 'canna-activate nil
 "Canna - a kana to kanji conversion program" )

(provide 'canna-leim)

;;; canna-leim.el ends here
