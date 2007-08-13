;;; egg-leim.el --- Egg-related code for LEIM
;; Copyright (C) 1997 Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Shamelessly ripped off from
;;
;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997
;; Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;; Author: Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Version: egg-leim.el,v 1.1 1997/10/27 09:59:23 steve Exp steve
;; Keywords: japanese, input method, LEIM
;; Last Modified: 1997/10/27 09:59:23

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
;;  Add pointers to Egg documentation in LEIM format

(defun egg-activate (&optional name)
  (if (featurep 'wnn)
      (require 'egg)
    (error "Wnn is not built into this XEmacs"))
  (setq inactivate-current-input-method-function 'egg-inactivate)
  (toggle-egg-mode))

(defun egg-inactivate ()
  (cond (egg:*mode-on* (toggle-egg-mode))))

(register-input-method
 'japanese-egg-wnn "Japanese"
 'egg-activate nil
 "EGG - an interface to the Wnn kana to kanji conversion program" )

(provide 'egg-leim)

;;; egg-leim.el ends here
