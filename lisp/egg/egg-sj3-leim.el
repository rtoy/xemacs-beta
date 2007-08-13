;;; egg-sj3-leim.el --- Egg/SJ3-related code for LEIM

;; Copyright (C) 1997 Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Version: 1.1
;; Keywords: japanese, input method, LEIM

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; cloned from egg-leim.el
;; which was cloned from:

;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997
;; Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;

;; TODO
;;
;;  Add pointers to Egg documentation in LEIM format

;;; Code:

(defun egg-sj3-activate (&optional name)
  (setq inactivate-current-input-method-function 'egg-sj3-inactivate)
  (setq egg-default-startup-file "eggrc-sj3")
  (require 'egg-sj3)
  (let ((func (get 'japanese 'set-egg-environ)))
    (when func
      (funcall func)))
  (egg-mode)
  (toggle-egg-mode))

(defun egg-sj3-inactivate ()
  (cond (egg:*mode-on* (toggle-egg-mode))))

(register-input-method
 'japanese-egg-sj3 "Japanese"
 'egg-sj3-activate nil
 "EGG - an interface to the SJ3 kana to kanji conversion program" )

(provide 'egg-sj3-leim)

;;; egg-sj3-leim.el ends here
