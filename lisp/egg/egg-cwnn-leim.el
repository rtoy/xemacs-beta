;;; egg-cwnn-leim.el --- Egg/CWnn-related code for LEIM

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

;; EGG specific setup
(define-egg-environment 'chinese-pinyin
  "Chinese pinyin settings for egg."
  (lambda ()
    (when (not (featurep 'egg-cnpinyin))
      (load "its/its-pinyin")
      (setq its:*standard-modes*
	    (append
	     (list (its:get-mode-map "PinYin"))
	     its:*standard-modes*))
      (provide 'egg-cnpinyin))
    (setq wnn-server-type 'cserver)
    (setq-default its:*current-map* (its:get-mode-map "PinYin"))))

(define-egg-environment 'chinese-zhuyin
  "Chinese zhuyin settings for egg."
  (lambda ()
    (when (not (featurep 'egg-cnzhuyin))
      (load "its/its-zhuyin")
      (setq its:*standard-modes*
	    (append
	     (list (its:get-mode-map "zhuyin"))
	     its:*standard-modes*))
      (provide 'egg-cnzhuyin))
    (setq wnn-server-type 'cserver)
    (setq-default its:*current-map* (its:get-mode-map "zhuyin"))))


(defun egg-pinyin-activate (&optional name)
  (if (featurep 'wnn)
      (require 'egg)
    (error "Wnn is not built into this XEmacs"))
  (setq inactivate-current-input-method-function 'egg-pinyin-inactivate)
  (setq egg-default-startup-file "eggrc-wnn")
  (require 'egg-wnn)
  (let ((func (get 'chinese-pinyin 'set-egg-environ)))
    (when func
      (funcall func)))
  (egg-mode)
  (toggle-egg-mode))

(defun egg-pinyin-inactivate ()
  (cond (egg:*mode-on* (toggle-egg-mode))))

(defun egg-zhuyin-activate (&optional name)
  (if (featurep 'wnn)
      (require 'egg)
    (error "Wnn is not built into this XEmacs"))
  (setq inactivate-current-input-method-function 'egg-zhuyin-inactivate)
  (setq egg-default-startup-file "eggrc-wnn")
  (require 'egg-wnn)
  (let ((func (get 'chinese-zhuyin 'set-egg-environ)))
    (when func
      (funcall func)))
  (egg-mode)
  (toggle-egg-mode))

(defun egg-zhuyin-inactivate ()
  (cond (egg:*mode-on* (toggle-egg-mode))))

(register-input-method
 'chinese-egg-pinyin "Chinese"
 'egg-zhuyin-activate nil
 "EGG - an interface to the CWnn Chinese conversion program" )

(register-input-method
 'chinese-egg-zhuyin "Chinese"
 'egg-zhuyin-activate nil
 "EGG - an interface to the CWnn Chinese conversion program" )

(provide 'egg-cwnn-leim)

;;; egg-cwnn-leim.el ends here
