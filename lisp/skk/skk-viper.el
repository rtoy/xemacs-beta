;;; skk-viper.el --- SKK related code for Viper
;; Copyright (C) 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>, Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;; Maintainer: Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-viper.el,v 1.1 1997/12/02 08:48:40 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:40 $

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

;;; Commentary:

;; Following people contributed modifications to skk.el (Alphabetical order):
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;      Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>

;;; Change log:

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)
(require 'advice)
(require 'viper)

(setq skk-use-viper t)

;;;###skk-autoload
(defvar skk-viper-normalize-map-function nil
  "Viper が minor-mode-map-alist を調整するための関数。" )

(defvar skk-viper-or-vip 
  (if (fboundp 'viper-normalize-minor-mode-map-alist)
      'viper
    'vip ))

(let ((other-buffer
       (if skk-xemacs
           (local-variable-p 'minor-mode-map-alist nil t)
         (local-variable-p 'minor-mode-map-alist) )))
  (require 'viper)
  ;; for current buffer and buffers to be created in the future.
  ;; substantially the same job as vip-harness-minor-mode does.
  (setq skk-viper-normalize-map-function
	(if (eq skk-viper-or-vip 'viper)
	    'viper-normalize-minor-mode-map-alist
	  'vip-normalize-minor-mode-map-alist ))
  (funcall skk-viper-normalize-map-function)
  (setq-default minor-mode-map-alist minor-mode-map-alist)
  (if (not other-buffer)
      nil
    ;; for buffers which are already created and have the minor-mode-map-alist
    ;; localized by Viper.
    (save-current-buffer
      (let ((buf (buffer-list)))
        (while buf
          (set-buffer (car buf))
          (if (null (assq 'skk-j-mode minor-mode-map-alist))
              (progn
                (setq minor-mode-map-alist
                      (nconc
                       (list
                        (cons 'skk-abbrev-mode skk-abbrev-mode-map)
                        (cons 'skk-ascii-mode skk-ascii-mode-map)
                        (cons 'skk-j-mode skk-j-mode-map)
                        (cons 'skk-zenkaku-mode skk-zenkaku-mode-map) )
                       minor-mode-map-alist ))
                (funcall skk-viper-normalize-map-function) ))
          (setq buf (cdr buf)) )))))

(setq sentence-end (concat "[。？！]\\|" sentence-end))

(defsubst skk-looking-at-jisx0208 (char)
  (eq 'japanese-jisx0208 (car (find-charset-string (char-to-string char)))) )

(defmacro skk-viper-advice-select (viper vip arg body)
  (` (if (eq skk-viper-or-vip 'viper)
	 (defadvice (, viper) (, arg) (,@ body))
       (defadvice (, vip) (, arg) (,@ body)) )))
       
(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word-kernel
 (around skk-ad activate)
 ((if skk-mode 
      (let ((enable-multibyte-characters t))
        (forward-word val) )
    ad-do-it )))
 
(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word-kernel
 (around skk-ad activate)
 ((if skk-mode
      (let ((enable-multibyte-characters t))
        (backward-word val) )
    ad-do-it )))

(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-del-backward-char-in-insert
 (around skk-ad activate)
 ((cond ((and skk-henkan-on (>= skk-henkan-start-point (point)))
         (setq skk-henkan-count 0)
         (skk-kakutei) )
        (skk-henkan-active
         (if (and (not skk-delete-implies-kakutei)
                  (= skk-henkan-end-point (point)) )
             (skk-previous-candidate)
           (if skk-use-face (skk-henkan-face-off))
           (if overwrite-mode
               (progn
                 (backward-char 1)
                 (delete-char 1) )
             (delete-backward-char 1)
	     (if (>= skk-henkan-end-point (point)) (skk-kakutei)) )))
        ((and skk-henkan-on overwrite-mode)
         (backward-char 1)
         (delete-char 1) )
        (t ad-do-it) )))
 
(skk-viper-advice-select
 viper-intercept-ESC-key vip-intercept-ESC-key
 (before skk-add activate)
 ("▽モード、▼モードだったら確定する。"
  (and skk-mode skk-henkan-on (skk-kakutei)) ))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("スペースの両側の文字セットが JISX0208 だったらスペースを取り除く。"
  (save-match-data
    (and (skk-save-point
	  (skip-chars-backward " ")
	  (string-match "\\c|" (char-to-string (preceding-char))) )
         (skk-save-point
	  (skip-chars-forward " ")
	  (string-match "\\c|" (char-to-string (following-char))) )
         (delete-char 1) ))))

;;(defadvice vip-insert (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-Insert (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-open-line (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-Open-line (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-append (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-Append (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice vip-overwrite (after skk-ad activate)
;;  "skk-mode だったらかなモードにする。"
;;  (if skk-mode (skk-j-mode-on)) )

;;;; こりゃダメっすね。
;;;;(defadvice vip-replace-char (after skk-ad activate)
;;;;  "skk-mode だったらかなモードにする。"
;;;;  (if skk-mode (skk-j-mode-on)) )

(provide 'skk-viper)
;;; skk-viper.el ends here
