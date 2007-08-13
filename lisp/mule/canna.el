;;; canna.el --- Interface to the Canna input method.

;; Copyright (C) 1994 Akira Kon, NEC Corporation.
;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: Akira Kon <kon@d1.bs2.mt.nec.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Revision: 1.6 $
;; Keywords: Canna, Japanese, input method, mule, multilingual

;; This file is not a part of Emacs yet.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Egg offered some influences to the implementation of Canna on
;; Nemacs/Mule, and this file contains a few part of Egg which is
;; written by S.Tomura, Electrotechnical Lab.  (tomura@etl.go.jp)

;; This program is rewritten for Emacs/mule and XEmacs/mule by MORIOKA
;; Tomohiko.

;;; Code:

;; -*-mode: emacs-lisp-*-

;; by 守岡 知彦 <morioka@jaist.ac.jp> 1996/11/11
(or (boundp 'CANNA)
    (let ((handle (dynamic-link (expand-file-name "canna.so" exec-directory))))
      (dynamic-call "emacs_canna_init" handle))
    )

(defvar self-insert-after-hook nil)
;; (defalias 'self-insert-internal 'self-insert-command)
;; end

(defconst canna-rcs-version
  "$Id: canna.el,v 1.6 1997/04/27 19:30:30 steve Exp $")

(defun canna-version ()
  "Display version of canna.el in mini-buffer."
  (interactive)
  (message (concat
	    (substring canna-rcs-version
		       5
		       (if (string-match "[0-9] [a-z]" canna-rcs-version)
			   (1+ (match-beginning 0))
			 ))
	    " ...")))

(require 'emu)

(if running-xemacs
    (defun canna-self-insert-string (string)
      (let ((len (length string))
	    (i 0)
	    ;; 挿入の途中で blink が起きるとうっとおしいので、
	    ;; 一時的に blink を抑止する。
	    (blink-matching-paren nil))
	(while (< i len)
	  (self-insert-internal (aref canna-kakutei-string i))
	  (setq i (1+ i))
	  )))
  (defalias 'canna-self-insert-string 'insert)
  )


;;; かんなの変数

(defvar canna-save-undo-text-predicate nil)
(defvar canna-undo-hook nil)

(defvar canna-do-keybind-for-functionkeys t)
(defvar canna-use-functional-numbers nil)
(defvar canna-use-space-key-as-henkan-region t)

(defvar canna-server nil)
(defvar canna-file   nil)

(defvar canna-underline nil)
(defvar canna-with-fences (not canna-underline))

(defvar canna-initialize-minibuffer-state-when-exit nil
  "*Non-nil のときは, ミニバッファを抜ける時日本語状態を初期化する.")

(defvar canna-inhibit-hankakukana nil
  "*Non-nil の時、字種変換で半角かなに変換しない")

;;;
;;; モードラインの修整
;;;

(defvar canna:*kanji-mode-string* "[ あ ]")
(defvar canna:*alpha-mode-string* "かんな")
(defvar canna:*saved-mode-string* "[ あ ]")

(defvar mode-line-canna-mode canna:*alpha-mode-string*)
(defvar mode-line-canna-mode-in-minibuffer canna:*alpha-mode-string*)

(defvar display-minibuffer-mode-in-minibuffer nil) ; same name as TAKANA
; たかなでは t がデフォルトだけど、nil をデフォルトにしておこうかな。

(make-variable-buffer-local 'mode-line-canna-mode)

; select-window-hook は mule から入ったんだと思うけど、
; これが無いと preprompt があってもどうしようもないのでないときは
; display-minibuffer-mode-in-minibuffer を nil にする。

(if (not (boundp 'select-window-hook))
    (setq display-minibuffer-mode-in-minibuffer nil))

(defun canna:select-window-hook (old new)
  (if (and (eq old (minibuffer-window))
           (not (eq new (minibuffer-window))))
      (save-excursion
        (set-buffer (window-buffer (minibuffer-window)))
	;; minibufferのデフォルトはアルファベットモード
	(setq mode-line-canna-mode-in-minibuffer canna:*alpha-mode-string*
              canna:*japanese-mode-in-minibuffer* nil	
	      minibuffer-preprompt nil)))
  (if (eq new (minibuffer-window))
      (setq minibuffer-window-selected t)
    (setq minibuffer-window-selected nil)))

; egg:select-window-hook でも十分なので、egg:select-window-hook が
; 設定されていない場合のみ定義する。

; 良く考えてみると display-minibuffer-mode-in-minibuffer が t の時は
; やはり上記の canna:select-window-hook が必要だなあ。どうしよう。

(if (and (boundp 'select-window-hook)
	 (not (eq select-window-hook 'egg:select-window-hook)))
    (setq select-window-hook 'canna:select-window-hook))

(defun mode-line-canna-mode-update (str)
  (if (eq (current-buffer) (window-buffer (minibuffer-window)))
      (if (and display-minibuffer-mode-in-minibuffer
	       (boundp 'minibuffer-preprompt))
	  (setq minibuffer-preprompt str)
	;else
	(setq mode-line-canna-mode-in-minibuffer str))
    (setq mode-line-canna-mode str) )
  (set-buffer-modified-p (buffer-modified-p)) )

;; memq を強調するなら、以下だが、
;(defun canna:memq-recursive (a l)
;  (or (eq a l)
;      (and (consp l)
;	   (or (canna:memq-recursive a (car l))
;	       (canna:memq-recursive a (cdr l)) ))))
;; 次の定義を使おう...
(defun canna:memq-recursive (a l)
  (if (atom l) (eq a l)
    (or (canna:memq-recursive a (car l))
	(canna:memq-recursive a (cdr l)) )))

(defun canna:create-mode-line ()
  "Add string of Canna status into mode-line."
  (cond (running-xemacs
	 (or (canna:memq-recursive 'mode-line-canna-mode
				   default-modeline-format)
	     (setq-default default-modeline-format
			   (append '("" mode-line-canna-mode)
				  default-modeline-format))
	     )
	 (mapcar (function
		  (lambda (buffer)
		    (save-excursion
		      (set-buffer buffer)
		      (or (canna:memq-recursive 'mode-line-canna-mode
						modeline-format)
			  (setq modeline-format
				(append '("" mode-line-canna-mode)
				       modeline-format))
			  )
		      )))
		 (buffer-list))
	 )
	(t
	 (or (canna:memq-recursive 'mode-line-canna-mode mode-line-format)
	     (setq-default
	      mode-line-format
	      (append (list (list 'minibuffer-window-selected
				  (list 'display-minibuffer-mode-in-minibuffer
					"-" "m") "-")
			    (list 'minibuffer-window-selected
				  (list 'display-minibuffer-mode-in-minibuffer
					'mode-line-canna-mode
					'mode-line-canna-mode-in-minibuffer)
				  'mode-line-canna-mode))
		      mode-line-format))
	     )))
  (mode-line-canna-mode-update mode-line-canna-mode))

(defun canna:mode-line-display ()
  (mode-line-canna-mode-update mode-line-canna-mode))

;;;
;;; Canna local variables
;;;

(defvar canna:*japanese-mode* nil "T if canna mode is ``japanese''.")
(make-variable-buffer-local 'canna:*japanese-mode*)
(set-default 'canna:*japanese-mode* nil)

(defvar canna:*japanese-mode-in-minibuffer* nil
  "T if canna mode is ``japanese'' in minibuffer.")

(defvar canna:*exit-japanese-mode* nil)
(defvar canna:*fence-mode* nil)
;(make-variable-buffer-local 'canna:*fence-mode*)
;(setq-default canna:*fence-mode* nil)

;;;
;;; global variables
;;;

(defvar canna-sys:*global-map* (copy-keymap global-map))
(defvar canna:*region-start* (make-marker))
(defvar canna:*region-end*   (make-marker))
(defvar canna:*spos-undo-text* (make-marker))
(defvar canna:*epos-undo-text* (make-marker))
(defvar canna:*undo-text-yomi* nil)
(defvar canna:*local-map-backup*  nil)
(defvar canna:*last-kouho* 0)
(defvar canna:*initialized* nil)
(defvar canna:*previous-window* nil)
(defvar canna:*minibuffer-local-map-backup* nil)
(defvar canna:*cursor-was-in-minibuffer* nil)
(defvar canna:*menu-buffer* " *menu*")
(defvar canna:*saved-minibuffer*)
(defvar canna:*saved-redirection* nil)
(defvar canna:*use-region-as-henkan-region* nil)
(make-variable-buffer-local 'canna:*use-region-as-henkan-region*)
(setq-default canna:*use-region-as-henkan-region* nil)

;;;
;;; 色の設定
;;;
(defvar canna-use-color nil
  "*Non-nil でカラーディスプレイで色を付ける.
t の時はデフォルトの色を使用する。
色を指定したい時は, \"読みの色\", \"変換対象の色\", \"選択対象の色\" の
リストを設定する")
(defvar canna:color-p nil "色が使えるか")
(defvar canna:attr-mode nil "現在のディスプレイモード")
(defvar canna:attr-yomi nil "読みの色属性")
(defvar canna:attr-taishou nil "変換対象部分の色属性")
(defvar canna:attr-select nil
  "ミニバッファ分離時のメニューの選択対象番号の色属性")
(defvar canna:attribute-alist		;colored by tagu@ae.keio.ac.jp
  '((yomi (normal . "red") 
	  (reverse . "moccasin"))
    (taishou (normal . "blue/lavender") 
	     (reverse . "yellow/cadet blue"))
    (select (normal . "DarkOliveGreen1/cadet blue")
	    (reverse . "light sea green/burlywood1")))
  "かんな変換時の配色のalist")

(make-variable-buffer-local (defvar canna:*yomi-overlay* nil))
(make-variable-buffer-local (defvar canna:*henkan-overlay* nil))
(make-variable-buffer-local (defvar canna:*select-overlay* nil))

;;;
;;; キーマップテーブル
;;;

;; フェンスモードでのローカルマップ
(defvar canna-mode-map (make-sparse-keymap))

(let ((ch 0))
  (while (<= ch 127)
    (define-key canna-mode-map (make-string 1 ch) 'canna-functional-insert-command)
    (setq ch (1+ ch))))

(cond (running-xemacs
       (define-key canna-mode-map [up]		    "\C-p")
       (define-key canna-mode-map [(shift up)]      "\C-p")
       (define-key canna-mode-map [(control up)]    "\C-p")
       (define-key canna-mode-map [down]            "\C-n")
       (define-key canna-mode-map [(shift down)]    "\C-n")
       (define-key canna-mode-map [(control down)]  "\C-n")
       (define-key canna-mode-map [right]           "\C-f")
       (define-key canna-mode-map [(shift right)]   "\C-f")
       (define-key canna-mode-map [(control right)] "\C-f")
       (define-key canna-mode-map [left]            "\C-b")
       (define-key canna-mode-map [(shift left)]    "\C-b")
       (define-key canna-mode-map [(control left)]  "\C-b")
       (define-key canna-mode-map [kanji]           " ")
       (define-key canna-mode-map [(control space)] [(control @)])
       )
      (t
       (define-key canna-mode-map [up]      [?\C-p])
       (define-key canna-mode-map [S-up]    [?\C-p])
       (define-key canna-mode-map [C-up]    [?\C-p])
       (define-key canna-mode-map [down]    [?\C-n])
       (define-key canna-mode-map [S-down]  [?\C-n])
       (define-key canna-mode-map [C-down]  [?\C-n])
       (define-key canna-mode-map [right]   [?\C-f])
       (define-key canna-mode-map [S-right] [?\C-f])
       (define-key canna-mode-map [C-right] [?\C-f])
       (define-key canna-mode-map [left]    [?\C-b])
       (define-key canna-mode-map [S-left]  [?\C-b])
       (define-key canna-mode-map [C-left]  [?\C-b])
       (define-key canna-mode-map [kanji]   [? ])
       (define-key canna-mode-map [?\C- ]   [?\C-@])
       ))

;; ミニバッファに何かを表示している時のローカルマップ
(defvar canna-minibuffer-mode-map (make-sparse-keymap))

(let ((ch 0))
  (while (<= ch 127)
    (define-key canna-minibuffer-mode-map (make-string 1 ch) 'canna-minibuffer-insert-command)
    (setq ch (1+ ch))))

(cond (running-xemacs
       (define-key canna-minibuffer-mode-map [up]              "\C-p")
       (define-key canna-minibuffer-mode-map [(shift up)]      "\C-p")
       (define-key canna-minibuffer-mode-map [(control up)]    "\C-p")
       (define-key canna-minibuffer-mode-map [down]            "\C-n")
       (define-key canna-minibuffer-mode-map [(shift down)]    "\C-n")
       (define-key canna-minibuffer-mode-map [(control down)]  "\C-n")
       (define-key canna-minibuffer-mode-map [right]           "\C-f")
       (define-key canna-minibuffer-mode-map [(shift right)]   "\C-f")
       (define-key canna-minibuffer-mode-map [(control right)] "\C-f")
       (define-key canna-minibuffer-mode-map [left]            "\C-b")
       (define-key canna-minibuffer-mode-map [(shift left)]    "\C-b")
       (define-key canna-minibuffer-mode-map [(control left)]  "\C-b")
       (define-key canna-minibuffer-mode-map [kanji]           " ")
       (define-key canna-minibuffer-mode-map [(control space)] [(control @)])
       )
      (t
       (define-key canna-minibuffer-mode-map [up]      [?\C-p])
       (define-key canna-minibuffer-mode-map [S-up]    [?\C-p])
       (define-key canna-minibuffer-mode-map [C-up]    [?\C-p])
       (define-key canna-minibuffer-mode-map [down]    [?\C-n])
       (define-key canna-minibuffer-mode-map [S-down]  [?\C-n])
       (define-key canna-minibuffer-mode-map [C-down]  [?\C-n])
       (define-key canna-minibuffer-mode-map [right]   [?\C-f])
       (define-key canna-minibuffer-mode-map [S-right] [?\C-f])
       (define-key canna-minibuffer-mode-map [C-right] [?\C-f])
       (define-key canna-minibuffer-mode-map [left]    [?\C-b])
       (define-key canna-minibuffer-mode-map [S-left]  [?\C-b])
       (define-key canna-minibuffer-mode-map [C-left]  [?\C-b])
       (define-key canna-minibuffer-mode-map [kanji]   [? ])
       (define-key canna-minibuffer-mode-map [?\C- ]   [?\C-@])
       ))

;;;
;;; グローバル関数の書き替え
;;;


;; Keyboard quit

;(if (not (fboundp 'canna-sys:keyboard-quit))
;    (fset 'canna-sys:keyboard-quit (symbol-function 'keyboard-quit)) )

;(defun canna:keyboard-quit ()
;  "See documents for canna-sys:keyboard-quit"
;  (interactive)
;  (if canna:*japanese-mode*
;      (progn
;;	(setq canna:*japanese-mode* nil)
;	(setq canna:*fence-mode* nil)
;	(if (boundp 'disable-undo)
;	    (setq disable-undo canna:*fence-mode*))
;	(canna:mode-line-display) ))
;  (canna-sys:keyboard-quit) )

;; Abort recursive edit

;(if (not (fboundp 'canna-sys:abort-recursive-edit))
;    (fset 'canna-sys:abort-recursive-edit 
;	  (symbol-function 'abort-recursive-edit)) )

;(defun canna:abort-recursive-edit ()
;  "see documents for canna-sys:abort-recursive-edit"
;  (interactive)
;  (if canna:*japanese-mode*
;      (progn
;	(setq canna:*japanese-mode* nil)
;	(setq canna:*fence-mode* nil)
;	(if (boundp 'disable-undo)
;	    (setq disable-undo canna:*fence-mode*))
;	(canna:mode-line-display) ))
;  (canna-sys:abort-recursive-edit) )

;; Exit-minibuffer

(defun canna:exit-minibuffer ()
  "Exit minibuffer turning off canna Japanese mode.
See also document for canna:saved-exit-minibuffer."
  (interactive)
  (if canna-initialize-minibuffer-state-when-exit
      (setq canna:*japanese-mode-in-minibuffer* nil
	    mode-line-canna-mode-in-minibuffer canna:*alpha-mode-string*))
  )

(add-hook 'minibuffer-exit-hook 'canna:exit-minibuffer)

;; kill-emacs

(add-hook 'kill-emacs-hook 'canna:finalize)

;;;
;;; function for mini-buffer
;;;

(defun adjust-minibuffer-mode ()
  (if (eq (current-buffer) (window-buffer (minibuffer-window)))
      (progn
	(setq canna:*japanese-mode* canna:*japanese-mode-in-minibuffer*)
	t)
    nil))

;;;
;;; keyboard input for japanese language
;;;

(defun canna-functional-insert-command (arg)
  "Use input character as a key of complex translation input such as\n\
kana-to-kanji translation."
  (interactive "*p")
  (let ((ch))
    (if (char-or-char-int-p arg)
	(setq ch last-command-char)
      (setq ch (event-to-character last-command-event)))
    (canna:functional-insert-command2 ch arg) ))

(defun canna:functional-insert-command2 (ch arg)
  "This function actualy isert a converted Japanese string."
  ;; この関数は与えられた文字を日本語入力のためのキー入力として取り扱
  ;; い、日本語入力の中間結果を含めた処理をEmacsのバッファに反映させる
  ;; 関数である。
  (canna:display-candidates (canna-key-proc ch)) )

(defun canna:delete-last-preedit ()
  (if (not (zerop canna:*last-kouho*))
      (progn
	(if canna-underline
        ; まず、属性を消す。
	    (progn
	      (canna:henkan-attr-off canna:*region-start* canna:*region-end*)
	      (canna:yomi-attr-off canna:*region-start* canna:*region-end*)))
	(delete-region canna:*region-start* canna:*region-end*)
	(setq canna:*last-kouho* 0) )))

(defun canna:insert-fixed (strs)
  (cond ((> strs 0)
	 (cond ((and canna-kakutei-yomi
		     (or (null canna-save-undo-text-predicate)
			 (funcall canna-save-undo-text-predicate
				  (cons canna-kakutei-yomi
					canna-kakutei-romaji) )))
		(setq canna:*undo-text-yomi*
		      (cons canna-kakutei-yomi canna-kakutei-romaji))
		(set-marker canna:*spos-undo-text* (point))
;;
;; update kbnes
		(canna-self-insert-string canna-kakutei-string)
		;; 未確定の文字がなく、確定文字列の最後が閉じ括弧の
		;; 類だったときは blink させる。
		(if (and canna-empty-info
			 (eq (char-syntax (char-before (point))) ?\)) )
		    (blink-matching-open))

;		(if overwrite-mode
;		    (let ((num strs)
;			  (kanji-compare 128))
;		      (catch 'delete-loop 
;			(while (> num 0)
;			  (if (eolp)
;			      (throw 'delete-loop nil))
;			  (if (>= (following-char) kanji-compare)
;			      (setq num (1- num)))
;			  (delete-char 1)
;			  (setq num (1- num))))))
;; end kbnes
;		(insert canna-kakutei-string)
		(if self-insert-after-hook
                    (funcall self-insert-after-hook
                             canna:*region-start* canna:*region-end*))
		(canna:do-auto-fill)
		(set-marker canna:*epos-undo-text* (point)) )
	       (t
;;
;; update kbnes
		(canna-self-insert-string canna-kakutei-string)
		;; 未確定の文字がなく、確定文字列の最後が閉じ括弧の
		;; 類だったときは blink させる。
		(if (and canna-empty-info
			 (eq (char-syntax (char-before (point))) ?\)) )
		    (blink-matching-open))

;		(if overwrite-mode
;		    (let ((num strs)
;			  (kanji-compare 128))
;		      (catch 'delete-loop 
;			(while (> num 0)
;			  (if (eolp) 
;			      (throw 'delete-loop nil))
;			  (if (>= (following-char) kanji-compare)
;			      (setq num (1- num)))
;			  (delete-char 1)
;			  (setq num (1- num))))))
;; end kbnes
;		(insert canna-kakutei-string)
		(if self-insert-after-hook
                    (funcall self-insert-after-hook
                             canna:*region-start* canna:*region-end*))
		(canna:do-auto-fill) ))
	 ) ))

(defun canna:insert-preedit ()
  (cond ((> canna-henkan-length 0)
	 (set-marker canna:*region-start* (point))
	 (if canna-with-fences
	     (progn
	       (insert "||")
	       (set-marker canna:*region-end* (point))
	       (backward-char 1)
	       ))
	 (insert canna-henkan-string)
	 (if (not canna-with-fences)
	     (set-marker canna:*region-end* (point)) )
	 (if canna-underline
	     (canna:yomi-attr-on canna:*region-start* canna:*region-end*))
	 (setq canna:*last-kouho* canna-henkan-length)
	 ))
  
  ;; 候補領域では強調したい文字列が存在するものと考えら
  ;; れる。強調したい文字はEmacsではカーソルポジションにて表示
  ;; することとする。強調したい文字がないのであれば、カーソル
  ;; は一番後の部分(入力が行われるポイント)に置いておく。
  
  ;; カーソルを移動する。
  (if (not canna-underline)
      (backward-char 
       (- canna:*last-kouho*
	  ;; カーソル位置は、反転表示部分が存在しないのであれば、
	  ;; 候補文字列の最後の部分とし、反転表示部分が存在するの
	  ;; であれば、その部分の始めとする。
	  (cond ((zerop canna-henkan-revlen)
		 canna:*last-kouho*)
		(t canna-henkan-revpos) )) )
    (if (and (> canna-henkan-revlen 0)
	     (> canna-henkan-length 0))
					; 候補の長さが0でなく、
					; 反転表示の長さが0でなければ、
					; その部分を変転表示する。
	(let ((start (+ canna:*region-start*
			(if canna-with-fences 1 0)
			canna-henkan-revpos) ))
	  (if canna-underline
	      (canna:henkan-attr-on start 
				    (+ start canna-henkan-revlen)))))
    ) )

(defun canna:display-candidates (strs)
  (cond ((stringp strs) ; エラーが起こった場合
	 (beep)
	 (message strs) )
	(canna-henkan-string
	 ;; もし候補表示が前の結果から変わっていなくないときは......

	 ;; 取り合えず最初は前に書いておいた中間結果を消す。
	 (canna:delete-last-preedit)

	 ;; 確定した文字列があればそれを挿入する。
	 (canna:insert-fixed strs)

	 ;; 次は候補についての作業である。

	 ;; 候補を挿入する。候補は縦棒二本にて挟まれる。
	 (canna:insert-preedit)
	 ))

  ;; モードを表す文字列が存在すればそれをモードとして取り扱う。
  (if (stringp canna-mode-string)
      (mode-line-canna-mode-update canna-mode-string))

  ;; 候補表示がなければフェンスモードから抜ける。
  (cond (canna-empty-info (canna:quit-canna-mode)))

  ;; ミニバッファに書くことが存在するのであれば、それをミニバッファ
  ;; に表示する。
  (cond (canna-ichiran-string
	 (canna:minibuffer-input canna-ichiran-string
				 canna-ichiran-length
				 canna-ichiran-revpos
				 canna-ichiran-revlen
				 strs) )
	(canna:*cursor-was-in-minibuffer*
;	 (select-frame (window-frame (minibuffer-window)))
	 (select-window (minibuffer-window))
	 (set-window-buffer (minibuffer-window)
			    (get-buffer-create canna:*menu-buffer*))
	 (use-local-map canna-minibuffer-mode-map) ))
  )

(defun canna:minibuffer-input (str len revpos revlen nfixed)
  "Displaying misc informations for kana-to-kanji input."

  ;; 作業をミニバッファに移すのに際して、現在のウィンドウの情報を保存
  ;; しておく。
  (setq canna:*previous-window* (selected-window))
;  (select-frame (window-frame (minibuffer-window)))

;; 自分に来る前がミニバッファかどうかを変数にでもいれておいた方がいいなあ。

  (if (not canna:*cursor-was-in-minibuffer*)
      (progn
	;; ミニバッファをクリアする。
;	(if (eq canna:*previous-window* (selected-window))
;	    (progn
;	      (canna:henkan-attr-off (point-min) (point-max))
;	      (canna:delete-last-preedit) ))

        ;; ミニバッファウィンドウに候補一覧用のバッファを割り当てる。
	(setq canna:*saved-minibuffer* (window-buffer (minibuffer-window)))
;	(set-window-buffer (minibuffer-window)
;			   (get-buffer-create canna:*menu-buffer*))
	;; modified by 守岡 知彦 <morioka@jaist.ac.jp>, 1996/6/7
	(unless running-xemacs
	  ;; とりあえず XEmacs では動かさないことにしておこう (^_^;
	  (setq canna:*saved-redirection* (frame-focus (selected-frame)))
	  (redirect-frame-focus (selected-frame) 
				(window-frame (minibuffer-window)))
	  )
	;; ミニバッファのキーマップを保存しておく。
	(setq canna:*minibuffer-local-map-backup* (current-local-map))
	))
  (select-window (minibuffer-window))
  (set-window-buffer (minibuffer-window)
		     (get-buffer-create canna:*menu-buffer*))

  (use-local-map canna-minibuffer-mode-map)

;  (canna:yomi-attr-off (point-min) (point-max) )
;  (canna:henkan-attr-off (point-min) (point-max) )
  (canna:select-attr-off (point-min) (point-max) )
  (setq canna:*cursor-was-in-minibuffer* t)
  (delete-region (point-min) (point-max))
  (if (not (eq canna:*previous-window* (selected-window)))
      (setq minibuffer-window-selected nil))

  (insert str)

  ;; ミニバッファで反転表示するべき文字のところにカーソルを移動する。
  (cond ((> revlen 0)
	 (backward-char (- len revpos)) ))
  ;;(message "%s" (selected-frame)) (sit-for 3)
  (raise-frame (window-frame (minibuffer-window)))
;  (select-frame (window-frame (minibuffer-window)))
  (and canna:color-p (not (eobp)) 
       (canna:select-attr-on (point) 
			     (save-excursion (forward-char 1) (point))))
  
  ;; ミニバッファに表示するべき文字列がヌル文字列なのであれば、前のウィ
  ;; ンドウに戻る。
  (if (or (zerop len) canna-empty-info)
      (progn
	(setq canna:*cursor-was-in-minibuffer* nil)
	(use-local-map canna:*minibuffer-local-map-backup*)

	;; ミニバッファウィンドウのバッファを元に戻す。
	(set-window-buffer (minibuffer-window) canna:*saved-minibuffer*)
;	(setq canna:*saved-minibuffer* nil)
	;; modified by 守岡 知彦 <morioka@jaist.ac.jp>, 1996/6/7
	(unless running-xemacs
	  ;; とりあえず XEmacs では動かさないようにしておこう (^_^;
	  (redirect-frame-focus (window-frame canna:*previous-window*)
				canna:*saved-redirection*)
	  )
	; ミニバッファで入力していたのなら以下もする。
;	(if (eq canna:*previous-window* (selected-window))
;	    (progn
;	      (canna:insert-fixed nfixed)
;	      (canna:insert-preedit) ))

	(if (and canna-empty-info (> len 0))
	    (progn
;	      (delete-region (point-min) (point-max))
	      (message str) ))
	(select-window canna:*previous-window*) ))
  )

(defun canna-minibuffer-insert-command (arg)
  "Use input character as a key of complex translation input such as\n\
kana-to-kanji translation, even if you are in the minibuffer."
  (interactive "p")
  (use-local-map canna:*minibuffer-local-map-backup*)
  (set-window-buffer (minibuffer-window) canna:*saved-minibuffer*)
  (select-window canna:*previous-window*)
  (let ((ch))
    (if (char-or-char-int-p arg)
	(setq ch last-command-char)
      (setq ch (event-to-character last-command-event)))
    (canna:functional-insert-command2 ch arg) ))

;;;
;;; かんなモードの主役は、次の canna-self-insert-command である。この
;;; コマンドは全てのグラフィックキーにバインドされる。
;;;
;;; この関数では、現在のモードが日本語入力モードかどうかをチェックして、
;;; 日本語入力モードでないのであれば、システムの self-insert-command 
;;; を呼ぶ。日本語入力モードであれば、フェンスモードに入り、
;;; canna-functional-insert-command を呼ぶ。
;;;

(if (not (boundp 'MULE)) ; for Nemacs
    (defun cancel-undo-boundary ()))

(defun canna-self-insert-command (arg)
  "Self insert pressed key and use it to assemble Romaji character."
  (interactive "*p")
  (adjust-minibuffer-mode)
  (if (and canna:*japanese-mode*
	   ;; フェンスモードだったらもう一度フェンスモードに入ったりし
	   ;; ない。
	   (not canna:*fence-mode*) )
      (canna:enter-canna-mode-and-functional-insert)
    (progn
      ;; 以下の部分は egg.el の 3.09 の egg-self-insert-command の部分から
      ;; コピーし、手を入れています。93.11.5 kon
      ;; treat continuous 20 self insert as a single undo chunk.
      ;; `20' is a magic number copied from keyboard.c
;      (if (or				;92.12.20 by T.Enami
;	   (not (eq last-command 'canna-self-insert-command))
;	   (>= canna:*self-insert-non-undo-count* 20))
;	  (setq canna:*self-insert-non-undo-count* 1)
;	(cancel-undo-boundary)
;	(setq canna:*self-insert-non-undo-count*
;	      (1+ canna:*self-insert-non-undo-count*)))
      (if (and (eq last-command 'canna-self-insert-command)
	       (> last-command-char ? ))
	  (cancel-undo-boundary))
      (self-insert-command arg)
;      (if canna-insert-after-hook
;	  (run-hooks 'canna-insert-after-hook))
      (if self-insert-after-hook
	  (if (<= 1 arg)
	      (funcall self-insert-after-hook
		       (- (point) arg) (point)))
	(if (= last-command-char ? ) (canna:do-auto-fill))))))

;; wire us into pending-delete
(put 'canna-self-insert-command 'pending-delete t)

(defun canna-toggle-japanese-mode ()
  "Toggle canna japanese mode."
  (interactive)
  (let ((in-minibuffer (adjust-minibuffer-mode)))
    (cond (canna:*japanese-mode*
	   (setq canna:*japanese-mode* nil) 
	   (canna-abandon-undo-info)
	   (setq canna:*use-region-as-henkan-region* nil)
	   (setq canna:*saved-mode-string* mode-line-canna-mode)
	   (mode-line-canna-mode-update canna:*alpha-mode-string*) )
	  (t
	   (setq canna:*japanese-mode* t)
	   (if (fboundp 'canna-query-mode)
	       (let ((new-mode (canna-query-mode)))
		 (if (string-equal new-mode "")
		     (setq canna:*kanji-mode-string* canna:*saved-mode-string*)
		   (setq canna:*kanji-mode-string* new-mode)
		   )) )
	   (mode-line-canna-mode-update canna:*kanji-mode-string*) ) )
    (if in-minibuffer
	(setq canna:*japanese-mode-in-minibuffer* canna:*japanese-mode*)) ))

(defun canna:initialize ()
  (let ((init-val nil))
    (cond (canna:*initialized*) ; initialize されていたら何もしない
	  (t
	   (setq canna:*initialized* t)
	   (setq init-val (canna-initialize 
			   (if canna-underline 0 1)
			   canna-server canna-file))
	   (cond ((car (cdr (cdr init-val)))
		  (canna:output-warnings (car (cdr (cdr init-val)))) ))
	   (cond ((car (cdr init-val))
		  (error (car (cdr init-val))) ))
	   ) )

    (if (fboundp 'canna-query-mode)
	(progn
	  (canna-change-mode canna-mode-alpha-mode)
	  (setq canna:*alpha-mode-string* (canna-query-mode)) ))

    (canna-do-function canna-func-japanese-mode)

    (if (fboundp 'canna-query-mode)
	(setq canna:*kanji-mode-string* (canna-query-mode)))

    init-val))

(defun canna:finalize ()
  (cond ((null canna:*initialized*)) ; initialize されていなかったら何もしない
	(t
	 (setq canna:*initialized* nil)
	 (let ((init-val (canna-finalize)))
	   (cond ((car (cdr (cdr init-val)))
		  (canna:output-warnings (car (cdr (cdr init-val)))) ))
	   (cond ((car (cdr init-val))
		  (error (car (cdr init-val))) ))
	   )
	 (message "『かんな』の辞書をセーブします。")
	 )))

(defun canna:enter-canna-mode ()
  (if (not canna:*initialized*)
      (progn 
	(message "『かんな』の初期化を行っています....")
	(canna:initialize)
	(message "『かんな』の初期化を行っています....done")
	))
  (canna-set-width (- (window-width (minibuffer-window))
		      (minibuffer-prompt-width)
		      (if (and display-minibuffer-mode-in-minibuffer
			       (eq (selected-window) (minibuffer-window)))
			  (string-width
			   (let ((new-mode (canna-query-mode)))
			     (if (string-equal new-mode "")
				 canna:*saved-mode-string*
			       new-mode)))
			0)))
  (setq canna:*local-map-backup*  (current-local-map))
  (setq canna:*fence-mode* t)
  ;; XEmacs change:
  (buffer-disable-undo (current-buffer))
  ;; (if (boundp 'disable-undo)
  ;;     (setq disable-undo canna:*fence-mode*))
  (use-local-map canna-mode-map))

(defun canna:enter-canna-mode-and-functional-insert ()
  (canna:enter-canna-mode)
  (setq canna:*use-region-as-henkan-region* nil)
  (setq unread-command-events (list last-command-event)))

(defun canna:quit-canna-mode ()
  (cond (canna:*fence-mode*
	 (use-local-map canna:*local-map-backup*)
	 (setq canna:*fence-mode* nil)
	 (if canna:*exit-japanese-mode*
	     (progn
	       (setq canna:*exit-japanese-mode* nil)
	       (setq canna-mode-string canna:*alpha-mode-string*)
	       (if canna:*japanese-mode*
		   (canna-toggle-japanese-mode)
		 (mode-line-canna-mode-update canna:*alpha-mode-string*) )))
	 ;; XEmacs change:
	 (buffer-enable-undo (current-buffer))
         ;; (if (boundp 'disable-undo)
         ;;     (setq disable-undo canna:*fence-mode*))
	 ))
  (set-marker canna:*region-start* nil)
  (set-marker canna:*region-end* nil)
  )

(defun canna-touroku ()
  "Register a word into a kana-to-kanji dictionary."
  (interactive)
;  (if canna:*japanese-mode*
  (if (not canna:*fence-mode*)
      (progn
	(setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	(canna:enter-canna-mode)
	(canna:display-candidates (canna-touroku-string "")) )
    (beep)
  ))

(defun canna-without-newline (start end)
  (and (not (eq start end))
       (or 
	(and (<= end (point))
	     (save-excursion
	       (beginning-of-line)
	       (<= (point) start) ))
	(and (<= (point) start)
	     (save-excursion 
	       (end-of-line) 
	       (<= end (point)) ))
	)))

(defun canna-touroku-region (start end)
  "Register a word which is indicated by region into a kana-to-kanji\n\
dictionary."
  (interactive "r")
  (if (canna-without-newline start end)
;      (if canna:*japanese-mode*
      (if (not canna:*fence-mode*)
	  (progn
	    (setq canna:*use-region-as-henkan-region* nil)
	    (setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	    (canna:enter-canna-mode)
	    (canna:display-candidates
	     (canna-touroku-string (buffer-substring start end))) ))
    (message "リージョンが不正です。ヌルリージョンか、改行が含まれています。")
    ))

(defun canna-extend-mode ()
  "To enter an extend-mode of Canna."
  (interactive "*")
;  (if (and (not (eq (window-frame (minibuffer-window)) (selected-frame)))
;	   (not canna:*fence-mode*))
	   ;; ミニバッファを分離している時は一時的にフェンスモードに入る
           ;; そうしないとメニューを選べない
           ;; (focusがミニバッファに行かないから)
  (if (not canna:*fence-mode*)
      (progn
	(setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	(canna:enter-canna-mode)
	(canna:display-candidates
	 (canna-do-function canna-func-extend-mode) ))
    (beep)))

(defun canna-kigou-mode ()
  "Enter symbol choosing mode."
  (interactive "*")
;  (if canna:*japanese-mode*
  (if (not canna:*fence-mode*)
      (progn
	(setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	(canna:enter-canna-mode)
	(canna:display-candidates (canna-change-mode canna-mode-kigo-mode)) )
    (beep)
    ))

(defun canna-hex-mode ()
  "Enter hex code entering mode."
  (interactive "*")
;  (if canna:*japanese-mode*
  (if (not canna:*fence-mode*)
      (progn
	(setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	(canna:enter-canna-mode)
	(canna:display-candidates (canna-change-mode canna-mode-hex-mode)) )
    (beep)
    ))

(defun canna-bushu-mode ()
  "Enter special mode to convert by BUSHU name."
  (interactive "*")
;  (if canna:*japanese-mode*
  (if (not canna:*fence-mode*)
      (progn
	(setq canna:*exit-japanese-mode* (not canna:*japanese-mode*))
	(canna:enter-canna-mode)
	(canna:display-candidates (canna-change-mode canna-mode-bushu-mode)) )
    (beep)
    ))

(defun canna-reset ()
  (interactive)
  (message "『かんな』の辞書をセーブします。");
  (canna:finalize)
  (message "『かんな』の再初期化を行っています....")
  (canna:initialize)
  (message "『かんな』の再初期化を行っています....done")
  )
  

(defun canna ()
  (interactive)
  (message "『かんな』を初期化しています....")
  (let (init-val)
    (cond ((and (fboundp 'canna-initialize) (fboundp 'canna-change-mode) )
	   
	   ;; canna が使える時は次の処理をする。
	   
	   ;; 配色設定 (by yuuji@ae.keio.ac.jp)
	   (setq canna:color-p (and canna-use-color 
				    window-system 
				    (x-display-color-p)))
	   ;;カラーの時underlineモードと同じ状態で初期化する必要がある
	   (setq canna-underline (or canna:color-p canna-underline))
	   (cond 
	    (canna:color-p
	     (setq canna:attr-mode
		   (cond
		    ((or (and (boundp 'hilit-background-mode)
			      (eq hilit-background-mode 'dark))
			 (string-match
			  "on\\|t"
			  (or (if running-xemacs
				  (x-get-resource "ReverseVideo"
						  "reverseVideo" 'string)
				(x-get-resource "ReverseVideo" "reverseVideo"))
			      "")))
		     'reverse)	;反転しているなら 'reverse
		    (t 'normal)))
	     (setq canna:attr-yomi
		   (if (listp canna-use-color)
		       (car canna-use-color)
		     (cdr (assq canna:attr-mode 
				(assq 'yomi canna:attribute-alist)))))
	     (setq canna:attr-taishou
		   (if (listp canna-use-color)
		       (car (cdr canna-use-color))
		     (setq canna:attr-taishou
			   (cdr (assq 
				 canna:attr-mode
				 (assq 'taishou canna:attribute-alist))))))
	     (setq canna:attr-select
		   (if (listp canna-use-color)
		       (car (cdr (cdr canna-use-color)))
		     (setq canna:attr-select
			   (cdr (assq canna:attr-mode
				      (assq 'select canna:attribute-alist))))))
	     ;;色づけ用faceの作成
	     (mapcar
	      (function
	       (lambda (face)
		 (let* ((color (symbol-value
				(intern (concat "canna:" (symbol-name face)))))
			backp)
		   (make-face face)
		   (if (stringp color)
		       (progn
			 (setq backp (string-match "/" color))
			 (set-face-foreground
			  face (substring color 0 backp))
			 (if backp 
			     (set-face-background
			      face (substring color (1+ backp)))))
		     (copy-face color face)))))
	      '(attr-yomi attr-taishou attr-select))
	     ))
	   ;;配色設定終了
	   
	   ;; 『かんな』システムの初期化
	   
	   (setq init-val (canna:initialize))
	   
	   ;; キーのバインディング
	   
	   (let ((ch 32))
	     (while (< ch 127)
	       (define-key global-map (make-string 1 ch) 'canna-self-insert-command)
	       (setq ch (1+ ch)) ))

	   (cond ((let ((keys (car init-val)) (ok nil))
		    (while keys
		      (cond ((< (car keys) 128)
			     (global-set-key
			      (make-string 1 (car keys))
			      'canna-toggle-japanese-mode)
			     (setq ok t) ))
		      (setq keys (cdr keys))
		      ) ok))
		 (t ; デフォルトの設定
		  (global-set-key "\C-o" 'canna-toggle-japanese-mode) ))

	   (if (not (keymapp (global-key-binding "\e[")))
	       (global-unset-key "\e[") )
	   (global-set-key "\e[210z" 'canna-toggle-japanese-mode) ; XFER
	   (define-key global-map [kanji] 'canna-toggle-japanese-mode)
	   (if canna-do-keybind-for-functionkeys
	       (progn
		 (global-set-key "\e[28~" 'canna-extend-mode) ; HELP on EWS4800
		 (global-set-key "\e[2~"  'canna-kigou-mode)  ; INS  on EWS4800
		 (global-set-key "\e[11~" 'canna-kigou-mode)
		 (global-set-key "\e[12~" 'canna-hex-mode)
		 (global-set-key "\e[13~" 'canna-bushu-mode)
		 (define-key global-map [help] 'canna-extend-mode)
		 (define-key global-map [insert] 'canna-kigou-mode)
		 (define-key global-map [f1] 'canna-kigou-mode)
		 (define-key global-map [f2] 'canna-hex-mode)
		 (define-key global-map [f3] 'canna-bushu-mode)
		 ))

	   (if canna-use-space-key-as-henkan-region
	       (progn
		 (global-set-key "\C-@" 'canna-set-mark-command)
		 ;; X Window は C-@ と C-SPC を区別するので、これが必要。
		 (global-set-key [?\C-\ ] 'canna-set-mark-command)
		 (global-set-key " " 'canna-henkan-region-or-self-insert) ))

	 ;; モード行の作成

	   (canna:create-mode-line)
	   (mode-line-canna-mode-update canna:*alpha-mode-string*)

	 ;; システム関数の書き替え

;	   (fset 'abort-recursive-edit 
;		 (symbol-function 'canna:abort-recursive-edit))
;	   (fset 'keyboard-quit 
;		 (symbol-function 'canna:keyboard-quit))

	   )

	  ((fboundp 'canna-initialize)
	   (beep)
	   (with-output-to-temp-buffer "*canna-warning*"
	     (princ "この Mule では new-canna が使えません")
	     (terpri)
	     (print-help-return-message)) )

	  (t ; 『かんな』システムが使えなかった時の処理
	   (beep)
	   (with-output-to-temp-buffer "*canna-warning*"
	     (princ "この Mule では canna が使えません")
	     (terpri)
	     (print-help-return-message))
	   ))
    (message "『かんな』を初期化しています....done")
    ) )

;;;
;;; auto fill controll (from egg)
;;;

(defun canna:do-auto-fill ()
  (if (and auto-fill-function (not buffer-read-only)
	   (> (current-column) fill-column))
      (let ((ocolumn (current-column)))
	(funcall auto-fill-function)
	(while (and (< fill-column (current-column))
		    (< (current-column) ocolumn))
  	  (setq ocolumn (current-column))
	  (funcall auto-fill-function)))))

(defun canna:output-warnings (mesg)
  (with-output-to-temp-buffer "*canna-warning*"
    (while mesg
      (princ (car mesg))
      (terpri)
      (setq mesg (cdr mesg)) )
    (print-help-return-message)))

(defun canna-undo (&optional arg)
  (interactive "*p")
  (if (and canna:*undo-text-yomi*
	   (eq (current-buffer) (marker-buffer canna:*spos-undo-text*))
;	   (canna-without-newline canna:*spos-undo-text*
;				  canna:*epos-undo-text*)
	   )
      (progn
	(message "読みに戻します！")
;	(switch-to-buffer (marker-buffer canna:*spos-undo-text*))
	(goto-char canna:*spos-undo-text*)
	(delete-region canna:*spos-undo-text*
		       canna:*epos-undo-text*)

	(if (null canna:*japanese-mode*)
	    (progn
	      (setq canna:*exit-japanese-mode* t) ))
;	      (canna-toggle-japanese-mode) ))
	(if (not canna:*fence-mode*)
	    ;; フェンスモードだったらもう一度フェンスモードに入ったりし
	    ;; ない。
	    (canna:enter-canna-mode) )
	(canna:display-candidates 
	 (let ((texts (canna-store-yomi (car canna:*undo-text-yomi*)
					(cdr canna:*undo-text-yomi*) )) )
	   (cond (canna-undo-hook
		  (funcall canna-undo-hook))
		 (t texts) )))
	(canna-abandon-undo-info)
	)
    (canna-abandon-undo-info)
    (undo arg) ))

(defun canna-abandon-undo-info ()
  (interactive)
  (setq canna:*undo-text-yomi* nil)
  (set-marker canna:*spos-undo-text* nil)
  (set-marker canna:*epos-undo-text* nil) )

(defun canna-henkan-region (start end)
  "Convert a text which is indicated by region into a kanji text."
  (interactive "*r")
  (if (null canna:*japanese-mode*)
      (progn
	(setq canna:*exit-japanese-mode* t) ))
;	(canna-toggle-japanese-mode) ))
  (let ((res nil))
    (setq res (canna-store-yomi (buffer-substring start end)))
    (delete-region start end)
    (canna:enter-canna-mode)
    (if (fboundp 'canna-do-function)
	(setq res (canna-do-function canna-func-henkan)))
    (canna:display-candidates res) ))

;;;
;;; マークコマンド，canna-henkan-region-or-self-insert で使うかも
;;;

(defun canna-set-mark-command (arg)
  "Besides setting mark, set mark as a HENKAN region if it is in\n\
the japanese mode."
  (interactive "P")
  (set-mark-command arg)
  (if canna:*japanese-mode*
      (progn
	(setq canna:*use-region-as-henkan-region* t)
	(message "Mark set(変換領域開始)") )))

(defun canna-henkan-region-or-self-insert (arg)
  "Do kana-to-kanji convert region if HENKAN region is defined,\n\
self insert otherwise."
  (interactive "*p")
  (if (and canna:*use-region-as-henkan-region*
;	   (< (mark) (point))
;	   (not (save-excursion (beginning-of-line) (< (mark) (point)))) )
	   (canna-without-newline (region-beginning) (region-end)))
      (progn
	(setq canna:*use-region-as-henkan-region* nil)
	(canna-henkan-region (region-beginning) (region-end)))
    (canna-self-insert-command arg) ))

;;
;; for C-mode
;;

(defun canna-electric-c-terminator (arg)
  (interactive "P")
  (if canna:*japanese-mode*
      (canna-self-insert-command arg)
    (electric-c-terminator arg) ))

(defun canna-electric-c-semi (arg)
  (interactive "P")
  (if canna:*japanese-mode*
      (canna-self-insert-command arg)
    (electric-c-semi arg) ))

(defun canna-electric-c-brace (arg)
  (interactive "P")
  (if canna:*japanese-mode*
      (canna-self-insert-command arg)
    (electric-c-brace arg) ))

(defun canna-c-mode-hook ()
  (define-key c-mode-map "{" 'canna-electric-c-brace)
  (define-key c-mode-map "}" 'canna-electric-c-brace)
  (define-key c-mode-map ";" 'canna-electric-c-semi)
  (define-key c-mode-map ":" 'canna-electric-c-terminator) )

(defun canna-set-fence-mode-format (fence sep underline)
  (setq canna-with-fences fence)
  (canna-set-bunsetsu-kugiri sep)
  (setq canna-underline underline)
)

;; リージョンにあるローマ字を『かんな』に食わす。
;; 結果として、『かんな』の読みモードになる。
;; リージョンに存在している空白文字と制御文字は捨てられる。

(defun canna-rk-region (start end)
  "Convert region into kana."
  (interactive "*r")
  (let ((str nil) (len 0) (i 0) (res 0))
    (setq str (buffer-substring start end))
    (setq len (length str))
    (while (< i len)
      (let ((ch (elt str i)))
	(if (> ch ? )
	    (setq res (canna-do-function canna-func-functional-insert ch)) ))
      (setq i (1+ i)) )
    res))

(defun canna-rk-trans-region (start end)
  "Insert alpha-numeric string as it is sent from keyboard."
  (interactive "*r")
  (let ((res))
    (setq res (canna-rk-region start end))
    (delete-region start end)
    (if (null canna:*japanese-mode*)
	(progn
	  (setq canna:*exit-japanese-mode* t) ))
    (setq res (canna-do-function canna-func-henkan))
    (canna:enter-canna-mode)
    (canna:display-candidates res) ))

;; カーソルの左にある arg ワードのローマ字を『かんな』に食わす。

(defun canna-rk-trans (arg)
  (interactive "*p")
  (let ((po (point)))
    (skip-chars-backward "-a-zA-Z.,?!~")
    (if (not (eq (point) po))
	(canna-rk-trans-region (point) po) )))

(defun canna-henkan-kakutei-and-self-insert (arg)
  (interactive "*p")
  (if canna:*japanese-mode*
      (canna-functional-insert-command arg)
    (progn
      (setq unread-command-events (list last-command-event))
      (canna-kakutei-to-basic-stat)) ))

(defun canna-kakutei-to-basic-stat ()
  (let ((res 0)
	(kakutei canna-henkan-string))
    (while (not canna-empty-info)
;      (setq res (canna-key-proc ?\C-m)))
      (setq res (canna-do-function canna-func-kakutei)))
    (setq canna-kakutei-string kakutei)
    (canna:display-candidates (length canna-kakutei-string))
    (if (not canna:*japanese-mode*)
	(mode-line-canna-mode-update canna:*alpha-mode-string*))
    ))

(defun canna-minibuffer-henkan-kakutei-and-self-insert (arg)
  (interactive "p")
  (set-window-buffer (minibuffer-window) canna:*saved-minibuffer*)
  (select-window canna:*previous-window*)
  (if canna:*japanese-mode*
      (canna:functional-insert-command2 last-command-event arg)
    (progn
      (setq unread-command-events (list last-command-event))
      (canna-kakutei-to-basic-stat)) ))

(defun canna-setup-for-being-boiled ()
  (let ((ch (1+ ? )))
    (while (< ch 127)
      (define-key canna-mode-map (make-string 1 ch) 'canna-henkan-kakutei-and-self-insert)
      (define-key canna-minibuffer-mode-map (make-string 1 ch) 'canna-minibuffer-henkan-kakutei-and-self-insert)
      (setq ch (1+ ch)))))

(defvar rK-trans-key "\C-j" "for `boil' only")
(make-variable-buffer-local 'rK-trans-key)

(defun canna-boil ()
  "`canna-boil' cooks `canna' as if `boil' does for `egg'."
  (interactive)
  (canna-setup-for-being-boiled)
  (local-set-key rK-trans-key 'canna-rk-trans)
  (message "boiled"))

;;
;; 色づけのための関数
;;
(defun canna:yomi-attr-on (start end)
  (if (overlayp canna:*yomi-overlay*)
      (move-overlay canna:*yomi-overlay* start end)
    (overlay-put (setq canna:*yomi-overlay* (make-overlay start end nil nil t))
		 'face 
		 (if canna:color-p 'attr-yomi 'underline))
    )
  )

(defun canna:yomi-attr-off (start end);
  (and (overlayp canna:*yomi-overlay*) 
       (delete-overlay canna:*yomi-overlay*)
       )
  )

(defun canna:henkan-attr-on (start end)
  (if (overlayp canna:*henkan-overlay*)
      (move-overlay canna:*henkan-overlay* start end)
    (overlay-put (setq canna:*henkan-overlay*
		       (make-overlay start end nil nil t))
		 'face 
		 (if canna:color-p 'attr-taishou 'region))
	)
  )

(defun canna:henkan-attr-off (start end)
  (and (overlayp canna:*henkan-overlay*)
       (delete-overlay canna:*henkan-overlay*)
       )
  )

(defun canna:select-attr-on (start end)
  (if (overlayp canna:*select-overlay*)
      (move-overlay canna:*select-overlay* start end)
    (overlay-put (setq canna:*select-overlay*
		       (make-overlay start end nil nil t))
		 'face 
		 'attr-select))
  )

(defun canna:select-attr-off (start end)
  (and (overlayp canna:*select-overlay*)
       (delete-overlay canna:*select-overlay*)
       )
  )


(provide 'canna)

;;; canna.el ends here
