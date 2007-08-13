;; SKK tutorial for SKK version 9.4 and later versions
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-tut.el,v 1.1 1997/12/02 08:48:39 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:39 $

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

;; Following people contributed modifications to skk-tut.el
;; (Alphabetical order):
;;      Haru'yasu Ueda <hal@sics.se>
;;      Hideki Sakurada <sakurada@kusm.kyoto-u.ac.jp>
;;      Hitoshi SUZUKI <h-suzuki@ael.fujitsu.co.jp>
;;      IIDA Yosiaki <iida@sayla.secom-sis.co.jp>
;;      Koji Uchida <uchida@cfd.tytlabs.co.jp>
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;      Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;      Toyonobu Yoshida <toyono-y@is.aist-nara.ac.jp>
;;      Wataru Matsui <matsui@gf.hm.rd.sanyo.co.jp>
;;      沈志勇 <jshen@cas.org>

;;; Change log:
;; version 4.10 released 1997.2.4
;; version 3.9 released 1996.2.7
;; version 3.8 released 1995.5.13
;; version 3.7 released 1993.5.20
;; version 3.6 released 1992.9.19
;; version 3.5 released 1992.5.31
;; version 3.4 released 1992.4.12
;; version 3.3 released 1991.4.20
;; version 3.2 released 1990.4.15
;; version 2.2 released 1989.4.15

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)
(require 'advice)

;#SJT# This should be adjusted to XEmacs convention.
;      And patches to Murata-san should be SKK convention.
(defvar skk-tut-file-alist
  '(("Japanese" . (expand-file-name "skk/SKK.tut" data-directory))
    ("English" . (expand-file-name "skk/SKK.tut.E" data-directory)))
  "*
Alist of `(LANGUAGE . TUTORIAL-FILE)' pairs."
)

(defvar skk-tut-file "/usr/local/share/skk/SKK.tut"
  "*SKK チュートリアルのファイル名。
The English version is SKK.tut.E." )

(defvar skktut-japanese-tut
  (string= (file-name-nondirectory skk-tut-file) "SKK.tut")
  "Non-nil であれば、チュートリアルが日本語であることを示す。" )

(defvar skktut-use-face t
  "*Non-nil であれば、チュートリアルで face を利用した表示を行なう。" )

(defvar skktut-section-face
  (and skktut-use-face
       (cond ((and (eq skk-background-mode 'mono) (skk-terminal-face-p))
              'bold-italic )
             ((eq skk-background-mode 'light)
              (skk-make-face 'yellow/dodgerblue) )
             (t (skk-make-face 'yellow/slateblue)) ))
  "*チュートリアル中のセクションの表示部分の face。" )

(defvar skktut-do-it-face
  (and skktut-use-face
       (cond ((and (eq skk-background-mode 'mono) (skk-terminal-face-p))
              'bold )
             ((eq skk-background-mode 'light)
              (skk-make-face 'DarkGoldenrod) )
             (t (skk-make-face 'LightGoldenrod)) ))
  "*チュートリアル中の指示項目の表示部分の face。" )

(defvar skktut-question-face
  (and skktut-use-face
       (cond ((and (eq skk-background-mode 'mono) (skk-terminal-face-p))
              'underline )
             ((eq skk-background-mode 'light)
              (skk-make-face 'Blue) )
             (t (skk-make-face 'LightSkyBlue)) ))
  "*チュートリアル中の問題の表示部分の face。" )

(defvar skktut-key-bind-face
  (and skktut-use-face
       (cond ((and (eq skk-background-mode 'mono) (skk-terminal-face-p))
              'bold )
             ((eq skk-background-mode 'light)
              (skk-make-face 'Firebrick) )
             (t (skk-make-face 'OrangeRed)) ))
  "*チュートリアル中のキーバインドの表示部分の face。" )

(defvar skktut-hint-face
  (and skktut-use-face
       (cond ((and (eq skk-background-mode 'mono) (skk-terminal-face-p))
              'italic )
             ((eq skk-background-mode 'light)
              (skk-make-face 'CadetBlue) )
             (t (skk-make-face 'Aquamarine)) ))
  "*チュートリアル中のヒントの表示部分の face。
現在のところ、SKK.tut.E でしか使用されていない。" )

(defconst skktut-problem-numbers 37 "SKK チュートリアルの問題数。")

(defconst skktut-tut-jisyo "~/skk-tut-jisyo"
  "SKK チュートリアル用のダミー辞書。" )

(defconst skktut-init-variables-alist
  '((skk-init-file . "")
    (skk-special-midashi-char-list . (?> ?< ??))
    (skk-mode-hook . nil)
    (skk-auto-fill-mode-hook . nil)
    (skk-load-hook . nil)
    (skk-search-prog-list . ((skk-search-jisyo-file skktut-tut-jisyo 0 t)))
    (skk-jisyo . "~/skk-tut-jisyo")
    (skk-keep-record . nil)
    (skk-kakutei-key . "\C-j")
    (skk-use-vip . nil)
    (skk-use-viper . nil)
    (skk-henkan-okuri-strictly . nil)
    (skk-henkan-strict-okuri-precedence . nil)
    (skk-auto-okuri-process . nil)
    (skk-process-okuri-early . nil)
    (skk-egg-like-newline . nil)
    (skk-kakutei-early . t)
    (skk-delete-implies-kakutei . t)
    (skk-allow-spaces-newlines-and-tabs . t)
    (skk-convert-okurigana-into-katakana . nil)
    (skk-delete-okuri-when-quit . nil)
    (skk-henkan-show-candidates-keys . (?a ?s ?d ?f ?j ?k ?l))
    (skk-ascii-mode-string . " SKK")
    (skk-hirakana-mode-string . " かな")
    (skk-katakana-mode-string . " カナ")
    (skk-zenkaku-mode-string . " 全英")
    (skk-abbrev-mode-string . " aあ")
    (skk-echo . t)
    (skk-use-numeric-conversion . t)
    ;;(skk-char-type-vector . nil)
    ;;(skk-standard-rom-kana-rule-list . nil)
    (skk-rom-kana-rule-list . nil)
    (skk-postfix-rule-alist . (("oh" "オ" . "お")))
    (skk-previous-candidate-char . nil)
    ;;(skk-input-vector . nil)
    ;;(skk-zenkaku-vector . nil)
    ;;(skk-use-face . t)
    ;;(skk-henkan-face)
    ;;(skk-use-color-cursor . t)
    ;;(skk-default-cursor-color . "Black")
    ;;(skk-hirakana-cursor-color . t)
    ;;(skk-katakana-cursor-color . t)
    (skk-zenkaku-cursor-color . "gold") 
    (skk-ascii-cursor-color . "ivory4")
    (skk-abbrev-cursor-color . "royalblue")
    (skk-report-set-cursor-error . t)
    (skk-auto-insert-paren . nil)
    (skk-japanese-message-and-error . nil)
    (skk-ascii-mode-map . nil)
    (skk-j-mode-map . nil)
    (skk-zenkaku-mode-map . nil)
    (skk-abbrev-mode-map . nil)
    (skk-jisyo-save-count . nil)
    (skk-byte-compile-init-file . nil)
    (skk-count-private-jisyo-candidates-exactly . nil)
    (skk-compare-jisyo-size-when-saving . nil)
    (skk-auto-start-henkan . nil)
    (skk-insert-new-word-function . nil)
    
    (skk-date-ad . 1)
    (skk-number-style . 1)
    (skk-gadget-load-hook . nil)
    
    (skk-input-by-code-menu-keys1 . (?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y))
    (skk-input-by-code-menu-keys2 . (?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u))
    (skk-kcode-load-hook . nil)
    
    ;;(skk-num-type-list . nil)
    (skk-numeric-conversion-float-num . nil)
    (skk-uniq-numerals . t)
    (skk-num-load-hook . nil)
    
    (skk-dabbrev-like-completion . nil)
    (skk-comp-load-hook . nil))
  "skk.el のユーザー変数のリスト。" )

(defvar skktut-right-answer nil "問題の正解の文字列。")
(defvar skktut-problem-count 0 "SKK チュートリアルの現在の問題番号。")
(defvar skktut-tutorial-end nil "SKK チュートリアルの終了を示すフラグ。")
(defvar skktut-tutorial-map nil "SKK チュートリアルのためのキーマップ。")

(defvar skktut-original-buffer nil
  "skk-tutorial を呼んだときのバッファ名。" )

(defvar skktut-skk-on nil
  "Non-nil であれば、skk-tutorial を起動したときに SKK が既に起動されていたことを示す。" )

;; -- macros
(defmacro skktut-message (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH 
  ;; をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (append (list 'message (list 'if 'skktut-japanese-tut japanese english))
          arg ))
      
(defmacro skktut-error (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH 
  ;; をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (append (list 'error (list 'if 'skktut-japanese-tut japanese english))
          arg ))

(defmacro skktut-yes-or-no-p (japanese english)
  (list 'yes-or-no-p (list 'if 'skktut-japanese-tut japanese english)) )

;;;###autoload
(defun skk-tutorial (&optional query-language)
  "SKK チュートリアルを起動する。"
  (interactive "P")
  (if query-language
      (let ((lang
	     (completing-read "Language: " skk-tut-file-alist)))
	(setq skk-tut-file (cdr (assoc lang skk-tut-file-alist)))
	(message "SKK tutorial language set to %s until you exit Emacs."
                 lang)))
  (let ((inhibit-quit t))
    (if (not (< 9.4 (string-to-number (skk-version))))
        (error "skk.el version 9.4 or later is required")
      (skktut-pre-setup-tutorial)
      (skktut-setup-jisyo-buffer)
      (skktut-setup-working-buffer)
      (skktut-setup-problem-buffer)
      (skktut-setup-answer-buffer) )))

(defun skktut-save-buffers-kill-emacs (&optional query)
  (interactive "P")
  (if (skktut-yes-or-no-p "Tutorial も Emacs も終了します。よろしいですね？ "
                          "Quit tutorial and kill emacs? " )
      (progn (skktut-quit-tutorial 'now)
             (save-buffers-kill-emacs query) )))

(defun skktut-tutorial-again ()
  (interactive)
  (if (skktut-yes-or-no-p "最初から Tutorial をやり直します。よろしいですね？ "
                          "Quit tutorial and start from question 1 again? " )
      (progn (skktut-quit-tutorial 'now)
             (skk-tutorial) )))

(defun skktut-mode ()
  (interactive)
  (if (eq skktut-problem-count 1)
      (skktut-error "このキーはまだ使えません"
                    "Cannot use this key yet" )
    (if skk-mode
        (skk-j-mode-on)
      (add-hook 'before-make-frame-hook 'skktut-before-move-to-other-frame)
      (skk-j-mode-on)
      (define-key minibuffer-local-map "\C-j" 'skk-mode)
      ;;(define-key minibuffer-local-map "\C-m" 'skk-newline)
      )))

(defun skktut-kakutei (&optional word)
  (interactive)
  (if (eq skktut-problem-count 1)
      (skktut-error "このキーはまだ使えません"
                    "Cannot use this key yet" )
    (skk-kakutei word) ))

(defun skktut-error-command ()
  (interactive)
  (switch-to-buffer-other-window "*答*") )

(defun skktut-quit-tutorial (&optional now)
  (interactive)
  (if (or now (skktut-yes-or-no-p "本当にチュートリアルをやめますか? "
                                  "Quit tutorial? " ))
      (let ((inhibit-quit t))
        (delete-other-windows)
        ;; 再度チュートリアルを使えるように、内部変数を初期化しておく。
        (setq skktut-japanese-tut nil
              skktut-problem-count 0
              skktut-right-answer nil
              skktut-tutorial-end nil
              skktut-tutorial-map nil )
        (remove-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
        (remove-hook 'before-make-frame-hook
                     'skktut-before-move-to-other-frame )
        (ad-remove-advice 'other-frame 'before 'skktut-ad)
        (ad-remove-advice 'select-frame 'before 'skktut-ad)
        (ad-activate 'other-frame)
        (ad-activate 'select-frame)
        (if (featurep 'mule)
            (if (fboundp 'skktut-save-set-henkan-point)
                (skktut-change-func-def 'skk-set-henkan-point
                                        'skktut-save-set-henkan-point ))
          (if (fboundp 'skktut-nemacs-set-henkan-point)
              (skktut-change-func-def 'skk-set-henkan-point
                                      'skktut-nemacs-set-henkan-point )))
        (if (fboundp 'skktut-save-abbrev-mode)
            (skktut-change-func-def 'skk-abbrev-mode
                                    'skktut-save-abbrev-mode ))
        (fmakunbound 'skktut-save-set-henkan-point)
        (fmakunbound 'skktut-save-abbrev-mode)
        (fmakunbound 'skktut-nemacs-set-henkan-point)
        ;; skk-jisyo ;; for debugging
        (let ((buff (get-file-buffer skktut-tut-jisyo)))
          (if buff
              (progn
                (set-buffer buff)
                (set-buffer-modified-p nil)
                (kill-buffer buff))))
        (kill-buffer " *skk-tutorial*")
        (kill-buffer "*答*")
        (kill-buffer "*問*")
        ;;(skk-kill-local-variables)
        (switch-to-buffer skktut-original-buffer)
        ;; SKK を起動せずにいきなり 
        ;; skk-tutorial を実行したときに skk-jisyo バッファが作られないので 
        ;; skk-setup-jisyo-buffer でエラーとなり、Emacs の終了ができなく
        ;; なるので SKK モードを一度起こしておく。
        (skk-mode 1)
        ;; チュートリアル起動直前に開いていたバッファで、skk-mode を起動して
        ;; いたら、その状態にして、チュートリアルを終了する。
        ;; skk-jisyo  ;; for debugging
        (or skktut-skk-on
            (skk-mode -1) ))))

(defun skktut-answer-window ()
  (interactive)
  (let (p)
    (save-match-data
      (goto-char (point-max))
      (search-backward "\n>>")
      (forward-char 1)
      (setq skktut-right-answer
            (skk-buffer-substring (+ 3 (point))
                                  (skk-save-point (end-of-line) (point)) ))
      (switch-to-buffer-other-window "*答*")
      (insert ">> \n\n")
      (setq p (point))
      (if skktut-japanese-tut
          (insert "* 答ができたら『C-x n』; 途中でやめるには『C-x q』; "
                  "スキップするには『C-x s』 *" )
        (insert "* For next question `C-x n'; to quit `C-x q'; "
                "to skip this question `C-x s' *" ))
      (if skktut-use-face
          (put-text-property p (point) 'face skktut-key-bind-face) )
      (put-text-property p (point) 'read-only t)
      (goto-char (+ (point-min) 3)) )))

(defun skktut-next-window ()
  (interactive)
  (save-match-data
    (let (user-ans)
      (skk-save-point
        (goto-char (point-min))
        (end-of-line)
        (skip-chars-backward " \t")
        (setq user-ans (skk-buffer-substring (+ 3 (point-min)) (point))) )
      (if (not (string= skktut-right-answer user-ans))
          (progn
            (skktut-message "答が違います。もう一度やってみて下さい"
                            "Wrong.  Try again")
            (ding) )
        (skktut-erase-buffer)
        (message "")
        (other-window 1)
        (setq skktut-problem-count (1+ skktut-problem-count))
        (skktut-get-page skktut-problem-count)
        (if (>= skktut-problem-count (1+ skktut-problem-numbers))
            (skktut-quit-tutorial t)
          (skktut-answer-window) )))))

(defun skktut-skip-problem (arg)
  (interactive "p")
  (skktut-erase-buffer)
  (setq skktut-problem-count (+ skktut-problem-count arg))
  (if (< skktut-problem-count 1) (setq skktut-problem-count 1))
  (if (> skktut-problem-count skktut-problem-numbers)
      (setq skktut-problem-count skktut-problem-numbers))
  (if (and (>= skktut-problem-count 3) (not skk-j-mode))
      (skktut-mode) )
  (other-window 1)
  (skktut-get-page skktut-problem-count)
  (if skktut-tutorial-end (skktut-quit-tutorial 'now) (skktut-answer-window)) )

(defun skktut-set-henkan-point-tmp ()
  (interactive)
  (if skk-j-mode
      (skktut-error "かな/カナモードでは、英大文字はまだ使えません"
                    "Cannot use upper case character in kana/katakana mode" )
    (insert (if skk-zenkaku-mode
                (concat (char-to-string 163)
                        (char-to-string (+ last-command-char 128)))
              last-command-char))))

(defun skktut-abbrev-mode-tmp ()
  (interactive)
  (if skk-j-mode
      (skktut-error "このキーはまだ使えません"
                    "Cannot use this key yet" )
    (insert last-command-char)))

(defun skktut-get-page (page)
  (save-match-data
    (with-current-buffer " *skk-tutorial*"
      (let (pos)
        (goto-char (point-min))
        (search-forward "--\n" nil t page)
        (if (looking-at ";")
            (progn (forward-char 3)
                   (setq pos (point))
                   (end-of-line)
                   (save-excursion
                     (eval-region pos (point) nil) )
                   (forward-char 1) ))
        (if (not skktut-tutorial-end)
            (progn
              (setq pos (point))
              (search-forward "\n>>")
              (end-of-line)
              (copy-to-buffer "*問*" pos (point)) ))))
    (if (>= page 12)
        (skktut-enable) )
    (setq mode-line-buffer-identification
          (concat "ＳＫＫチュートリアル: ［問 "
                  (int-to-string page)
                  "］ （残り "
                  (int-to-string (- skktut-problem-numbers page))
                  "問）"))
    (set-buffer-modified-p nil)
    (sit-for 0) ))

(defun skktut-disable ()
  (if (not (fboundp 'skktut-save-set-henkan-point))
      (progn
        (skktut-change-func-def 'skktut-save-set-henkan-point
                                'skk-set-henkan-point )
        (skktut-change-func-def 'skk-set-henkan-point
                                'skktut-set-henkan-point-tmp )))
  (if (not (fboundp 'skktut-save-abbrev-mode))
      (progn
        (skktut-change-func-def 'skktut-save-abbrev-mode 'skk-abbrev-mode)
        (skktut-change-func-def 'skk-abbrev-mode 'skktut-abbrev-mode-tmp) )))

(defun skktut-enable ()
  (if (fboundp 'skktut-save-abbrev-mode)
      (progn (skktut-change-func-def 'skk-abbrev-mode 'skktut-save-abbrev-mode)
             (fmakunbound 'skktut-save-abbrev-mode) ))
  (if (fboundp 'skktut-save-set-henkan-point)
      (progn (skktut-change-func-def 'skk-set-henkan-point
                                     'skktut-save-set-henkan-point )
             (fmakunbound 'skktut-save-set-henkan-point) )))

(defun skktut-pre-setup-tutorial ()
  (setq skktut-original-buffer (current-buffer)
        skktut-skk-on skk-mode
        skktut-problem-count 1 ))
  
(defadvice other-frame (before skktut-ad activate)
  (skktut-before-move-to-other-frame) )
  
(defadvice select-frame (before skktut-ad activate)
  (skktut-before-move-to-other-frame) )
  
(add-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)

(defun skktut-setup-jisyo-buffer ()
  ;; setup skktut-tut-jisyo buffer.
  (set-buffer (get-buffer-create " *skk-tut-jisyo*"))
  (setq case-fold-search nil
        buffer-file-name (expand-file-name skktut-tut-jisyo) )
  (buffer-disable-undo (current-buffer))
  (insert (concat ";; okuri-ari entries.\n"
                  "ほっs /欲/\n"
                  "つかt /使/\n"
                  "たっs /達/\n"
                  "しt /知/\n"
                  "うごk /動/\n"
                  ";; okuri-nasi entries.\n"
                  "Greek /Α/Β/Γ/Δ/Ε/Ζ/Η/Θ/Ι/Κ/Λ/Μ/Ν/Ξ/Ο/Π/"
                  "Ρ/Σ/Τ/Υ/Φ/Χ/Ψ/Ω/\n"
                  "Russia /А/Б/В/Г/Д/Е/Ё/Ж/З/И/Й/К/Л/М/Н/О/"
                  "П/Р/С/Т/У/Ф/Х/Ц/Ч/Ш/Щ/Ъ/Ы/Ь/Э/Ю/Я/\n"
                  "greek /α/β/γ/δ/ε/ζ/η/θ/ι/κ/λ/μ/ν/ξ/ο/π/"
                  "ρ/σ/τ/υ/φ/χ/ψ/ω/\n"
                  "russia /а/б/в/г/д/е/ё/ж/з/и/й/к/л/м/н/о/"
                  "п/р/с/т/у/ф/х/ц/ч/ш/щ/ъ/ы/ь/э/ю/я/\n"
                  "いちおく /一億/\n"
                  "おおさか /大阪/\n"
                  "かな /仮名/\n"
                  "かんじ /漢字/幹事/監事/\n"
                  "がくしゅう /学習/\n"
                  "き /基/記/気/木/帰/\n"
                  "きごう /記号/、/。/，/．/・/：/；/？/！/゛/゜/´/｀/¨/"
                  "＾/￣/＿/ヽ/ヾ/ゝ/ゞ/〃/仝/々/〆/〇/ー/―/‐/／/＼/〜/"
                  "‖/｜/…/‥/‘/’/“/”/（/）/〔/〕/［/］/｛/｝/〈/〉/"
                  "《/》/「/】/『/』/【/】/＋/−/±/×/÷/＝/≠/＜/＞/≦/≧/"
                  "∞/∴/♂/♀/°/′/″/℃/￥/＄/¢/£/％/＃/＆/＊/＠/§/☆/"
                  "★/○/●/◎/◇/◆/□/■/△/▲/▽/▼/※/〒/→/←/↑/↓/"
                  "〓/\n"
                  "きょうと /京都/\n"
                  "こうべ /神戸/\n"
                  "ご /五/互/伍/午/呉/吾/娯/後/御/悟/梧/檎/瑚/碁/語/誤/護/"
                  "醐/\n"
                  "さい /細/最/再/\n"
                  "さいしょ /最初/\n"
                  "さいとう /斎藤/\n"
                  "さとう /佐藤/\n"
                  "しゅうりょう /終了/\n"
                  "じしょ /辞書/地所/\n"
                  "じんこう /人口/\n"
                  "せんたく /選択/洗濯/\n"
                  "そう /走/\n"
                  "だい /大/第/代/\n"
                  "てき /的/敵/滴/適/摘/\n"
                  "とう /東/\n"
                  "とうほく /東北/\n"
                  "とうろく /登録/\n"
                  "とうろく /登録/\n"
                  "どう /動/\n"
                  "にゅうりょく /入力/\n"
                  "ひこうき /飛行機/\n"
                  "へんかん /変換/\n"
                  "ほく /北/\n"
                  "みょうじ /名字/\n"
                  "ようい /容易/用意/\n" ))
  (skk-setup-jisyo-buffer)
  (skktut-localize-and-init-variables) )

(defun skktut-setup-working-buffer ()
  (save-match-data
    (let (sexp)
      (set-buffer (get-buffer-create " *skk-tutorial*"))
      ;; " *skk-tut-jisyo*" バッファの skk.el の変数をバッファローカル化し、
      ;; 初期化する。
      (skktut-localize-and-init-variables)
      (erase-buffer)
      (insert-file-contents skk-tut-file)
      (goto-char (point-min))
      ;; チュートリアルが日本語か英語かをチェック。
      (setq skktut-japanese-tut (looking-at ";; SKK Japanese"))
      (while (re-search-forward "^>> \\((.+)\\)$" nil t nil)
        (setq sexp (skk-buffer-substring (match-beginning 1) (match-end 1)))
        (delete-region (match-beginning 1) (match-end 1))
        (insert (eval (car (read-from-string sexp)))) )
      (goto-char (point-min))
      (if skktut-use-face
          (skktut-colored) ))))

(defun skktut-setup-problem-buffer ()
  (switch-to-buffer (get-buffer-create "*問*"))
  (erase-buffer)
  (setq skktut-tutorial-map (make-keymap))
  (if (featurep 'xemacs)
      (map-keymap
       #'(lambda (key ignored)
	   (define-key skktut-tutorial-map key 'skktut-error-command))
       skktut-tutorial-map)
    (fillarray (nth 1 skktut-tutorial-map) 'skktut-error-command))
  (use-local-map skktut-tutorial-map)
  (skktut-get-page skktut-problem-count)
  (delete-other-windows)
  (split-window-vertically nil)
  (other-window 1)
  (enlarge-window (- (window-height (selected-window)) 20)) )

(defun skktut-setup-answer-buffer ()
  (switch-to-buffer (get-buffer-create "*答*"))
  ;; "*答*" バッファの skk.el の変数をバッファローカル化し、初期化する。
  (skktut-localize-and-init-variables)
  (local-set-key "\C-j" 'skktut-kakutei)
  (local-set-key "\C-x\C-c" 'skktut-save-buffers-kill-emacs)
  (local-set-key "\C-x\C-j" 'skktut-mode)
  (local-set-key "\C-xj" 'skktut-error-command)
  (local-set-key "\C-xn" 'skktut-next-window)
  (local-set-key "\C-xq" 'skktut-quit-tutorial)
  (local-set-key "\C-xs" 'skktut-skip-problem)
  (local-set-key "\C-xt" 'skktut-tutorial-again)
  (skktut-disable)
  (auto-fill-mode -1)
  (switch-to-buffer-other-window "*問*")
  (goto-char (point-max))
  (beginning-of-line)
  (skktut-answer-window)
  (message "") )

(defun skktut-localize-and-init-variables ()
  ;; ユーザーが skk.el の変数をカスタマイズしている可能性があるので、カレント
  ;; バッファの skk.el の変数をバッファローカル化し、初期化する。
  (mapcar
   (function
    (lambda (alist)
      (let ((v (car alist)))
	(make-local-variable v)
	(set v (cdr alist)))))
   skktut-init-variables-alist)
  (if (string= (buffer-name) "*答*")
      (load-library "skk"))
  (make-local-variable 'skk-mode-invoked)
  (setq skk-mode-invoked 'invoked))

(defun skktut-erase-buffer ()
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil) )
  (erase-buffer) )

(defun skktut-before-move-to-other-frame ()
  (if (skktut-yes-or-no-p "Tutorial を終了します。よろしいですね？ "
                          "Quit tutorial?" )
      (skktut-quit-tutorial 'now)
    (skktut-error "Tutorial を終了せずに他のフレームに移ることはできません。"
                  "Quit tutorial or you cannot move to other frame" )))

(defun skktut-colored ()
  (while (re-search-forward "▼\\([^】 ぁ-んァ-ン]+\\)" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       'highlight ))
  (goto-char (point-min))
  (while (re-search-forward "^==.+==$" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skktut-section-face ))
  (goto-char (point-min))
  (while (re-search-forward "^!!.+" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skktut-do-it-face ))
  (goto-char (point-min))
  (while (re-search-forward "^>> \\(.+\\)$" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face skktut-question-face ))
  (if skktut-japanese-tut
      nil
    (goto-char (point-min))
    (while (re-search-forward "Hint: .*$" nil t nil)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face skktut-hint-face ))))

(defun skktut-change-func-def (old new &optional save)
  ;; 関数 OLD の定義を NEW で置き変える。
  ;; オプショナル引数の SAVE を指定すると、OLD の定義を SAVE に保存する。
  (if save (defalias save (symbol-function old)))
  (defalias old (symbol-function new)) )

;; The following function is tricky, since they are executed by "eval-region".

(defun skktut-today ()
  (save-match-data
    (let (str p)
      (widen)
      (search-forward "\n>> ")
      (if (re-search-forward "「.*」" (skk-save-point (end-of-line) (point)) t)
          (delete-region (match-beginning 0) (match-end 0)) )
      (setq p (point)
            str (concat "「きょうは、" (skk-date) "です。」") )
      (insert str)
      (narrow-to-region (point-min) (point))
      (if skktut-use-face
          (put-text-property p (point) 'face skktut-question-face) ))))

(defun skktut-end-tutorial ()
  (message "")
  (switch-to-buffer "*問*")
  (delete-other-windows)
  (erase-buffer)
  (goto-char (point-min))
  (if skktut-japanese-tut
      (insert
       (concat "SKK チュートリアルはこれで終りです。\n\n"
               "SKK に関する質問、コメント、bug report 等は\n\n"
               "\tskk@kuis.kyoto-u.ac.jp\n\n"
               "迄お送り下さい。なお、このアドレスは SKK メイリングリストの"
               "アドレスです。\n"
               "回答は通常このアドレスに対してなされるので、メンバーでない"
               "方はその旨を明\n"
               "記してメールをお送りください。 SKK メイリングリストへ参加希"
               "望の場合は\n\n"
               "\tskk-join@kuis.kyoto-u.ac.jp\n\n"
               "へメールをお送りください\n\n"
               "!! 最後に <return> キーを押してください。" ))
    (insert
     (concat "Now we end the SKK tutorial.\n\n"
             "Please send comments, questions and bug reports on SKK to:\n\n"
             "\tskk@kuis.kyoto-u.ac.jp\n\n"
             "This is the address of the SKK mailing list, and normally the "
             "responces\n"
             "will be sent only to the ML members.  So, if you are not a ML "
             "member,\n"
             "please say so in your mail.  If you are interested in joining "
             "the SKK ML,\n"
             "send a mail to:\n\n"
             "\tskk-join@kuis.kyoto-u.ac.jp\n\n"
             "!! Hit <return> key when you are ready." )))
  (if skktut-use-face
      (save-match-data
        (goto-char (point-min))
        (re-search-forward "^!!.+" nil t nil)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face skktut-do-it-face )))
  (while (not (= ?\C-m (read-char)))
    (skktut-message "<return> キーを押してください" "Hit <return> key")
    (ding) )
  (setq skktut-tutorial-end t) )

(provide 'skk-tut)
;;; skk-tut.el ends here
