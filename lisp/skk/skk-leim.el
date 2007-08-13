;;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997
;; Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;; Author: Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;; Version: $Id: skk-leim.el,v 1.1 1997/12/02 08:48:38 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:38 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; (require 'skk-foreword)
;; (require 'skk-vars)

(defun skk-activate (&optional name)
  (require 'skk-foreword)
  (require 'skk-vars)
  (setq inactivate-current-input-method-function 'skk-inactivate)
  (skk-mode 1) )

(defun skk-auto-fill-activate (&optional name)
  (setq inactivate-current-input-method-function 'skk-inactivate)
  (skk-mode 1) )

(defun skk-inactivate ()
  (skk-mode -1) )

(register-input-method
 'japanese-skk "Japanese"
 'skk-activate nil
 "Simple Kana to Kanji conversion program" )

(register-input-method
 'japanese-skk-auto-fill "Japanese"
 'skk-auto-fill-activate nil
 "Simple Kana to Kanji conversion program with auto-fill" )

(provide 'skk-leim)
;;; skk-leim.el ends here
