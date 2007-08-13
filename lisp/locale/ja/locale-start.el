;;; @(#)locale-start.el.euc	1.7 97/03/06 13:43:21
;;; locale/ja/locale-start.el --- startup.el customized for ja locale.
;;;			This file, being an .el file, 
;;;			must be in ISO 2022 encoding after installation.
;; Copyright (C) 1985-1986, 1990, 1992-1995 Free Software Foundation, Inc.
;; Copyright (c) 1993-1997 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Maintainer: XEmacs
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun startup-splash-frame-body ()
  `("\n" ,(emacs-version) "\n"
    (face bold-italic "\
Copyright (C) 1985-1996 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1997 Sun Microsystems, Inc. All Rights Reserved.
Copyright (C) 1994-1996 Board of Trustees, University of Illinois
Copyright (C) 1995-1996 Ben Wing\n\n")
    
    ,@(if (featurep 'sparcworks)
          `( "\
サンは、WorkShop/Emacs 統合化パッケージのみをサポートします。
他の XEmacs パッケージはすべて、「現状のまま」で供給されます。
詳細 (英文) は" (key describe-no-warranty) "とタイプして、
1991 年 6 月版 GPL バージョン 2 をご覧ください。
本 XEmacs は基本メニューに限り日本語化されています。組み込まれた
メール (VM、MH) やニュースを読むためのパッケージ (Gnus) 等は、
日本語を完璧には処理できませんので、ご使用になる場合は十分ご注意
ください。\n")

        '("XEmacs には *まったく何の* 保障もありません。詳細は、"
          (key describe-no-warranty) "をタイプしてください。\n"))

    "XEmacs の複製は許可されています。許可の条件を見るには、\n"
    (key describe-copying) " とタイプしてください。\n"
    "最新版の取得方法については、" (key describe-distribution)
    " とタイプしてください。\n\n"
    
    "ヘルプ情報を見るには、" (key help-command) " とタイプ、または" 
    (face bold "ヘルプ") "メニューを使用\n"
    "してください\n"
    (key advertised-undo) " により変更を取り消せませす。(C- は、Control キーです。)\n"
    "XEmacs を終了するには、" (key save-buffers-kill-emacs) " とタイプしてください。\n"
    (key help-with-tutorial) " で XEmacs の使用方法の実習を開始できます。\n"
    (key info) " により、Info モードに入り、オンラインのドキュメントを\n"
    "読むことができます。\n"
    (face (bold red) ( "\
頻繁に尋ねられる質問とその答えは、XEmacs 問答集にあります。\n"
    "ヘルプメニューを使用するか、"(key xemacs-local-faq) " とタイプしてください。"))))


(defun command-line-do-help (arg)
  "Print the XEmacs usage message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (concat "\n" (emacs-version) "\n\n"))
    (princ
     (if (featurep 'x)
	 "XEmacs は、X ツールキットの標準コマンドオプションをすべて認識します。\nそれに加えて、"
       "XEmacs は、 "))
    (princ " 以下のオプションを認識し、出現順に処理します:

  -t デバイス名         端末の代わりに指定の TTY デバイスを使用して入出力を
			行なう。-nw が自動的に仮定される。
  -nw                   ウィンドウシステムを使用せず、現 TTY を使用。(注: TTY 
                        モードでの日本語表示は、本バージョンの XEmacs ではでき
                        ません。)
  -batch                非対話的使用。メッセージは、標準エラー (stderr) ヘ。
  -debug-init           初期化ファイルでのエラー発生時にはデバッガを起動。
  -unmapped             初期フレームのマップをしない。
  -no-site-file         サイト特化初期化ファイル (site-start.el) を読み込まない。
  -no-init-file         ユーザー特化初期化ファイル (~/.emacs) を読み込まない。
  -q                    -no-init-file と同義。
  -user ユーザー名      自分の初期化ファイルの代わりに指定ユーザーのファイルを使用。
  -u ユーザー名         -user と同義。\n")
;; 英語版では、以下のようなコードにより、ドキュメントの一部が表示されていたが、
;; これでは、英語のドキュメントしか表示できないので、このコードはコメントアウトし、
;; 単純に説明を印字するコードと置き換える。
;;   (let ((l command-switch-alis\nt)
;;	  (insert (lambda (&rest x)
;;		    (princ "  ")
;;		    (let ((len 2))
;;		      (while x
;;			(princ (car x))
;;			(incf len (length (car x)))
;;			(setq x (cdr x)))
;;		      (when (>= len 24)
;;			(terpri) (setq len 0))
;;		      (while (< len 24)
;;			(princ " ")
;;			(incf len))))))
;;      (while l
;;        (let ((name (car (car l)))
;;              (fn (cdr (car l)))
;;	      doc arg cons)
;;	  (cond
;;	   ((and (symbolp fn) (get fn 'undocumented)) nil)
;;	   (t
;;	    (setq doc (documentation fn))
;;	    (if (member doc '(nil "")) (setq doc "(undocumented)"))
;;	    (cond ((string-match "\n\\(<.*>\\)\n?\\'" doc)
;;		   ;; Doc of the form "The frobber switch\n<arg1> <arg2>"
;;		   (setq arg (substring doc (match-beginning 1) (match-end 1))
;;			 doc (substring doc 0 (match-beginning 0))))
;;		  ((string-match "\n+\\'" doc)
;;		   (setq doc (substring doc 0 (match-beginning 0)))))
;;	    (if (and (setq cons (rassq fn command-switch-alist))
;;		     (not (eq cons (car l))))
;;		(setq doc (format "Same as %s." (car cons))))
;;
;;	    (if arg
;;		(funcall insert name " " arg)
;;	      (funcall insert name))
;;	    (princ doc)
;;	    (terpri))))
;;        (setq l (cdr l))))
;; 置き換え部分開始
  (princ "\
  -help                 XEmacs 使用法を表示して終了。
  -flags                -help と同義。
  -h                    -help と同義。
  -?                    -help と同義。
  -version              バージョン情報を表示して終了。
  -V                    -version と同義。
  -funcall 関数名       指定の lisp 関数を引数なしで起動。
  -f 関数名             -funcall と同義。
  -eval フォーム        lisp のフォームを評価。引用 (quote) は注意深く行なってください。
  -load ファイル名      指定の lisp コードを XEmacs に読み込む。
  -l ファイル名         -load と同義。
  -insert ファイル名    現バッファにファイルを挿入。
  -i ファイル名         -insert と同義。
  -kill                 XEmacs を終了。
  -tooltalk             ToolTalk サーバーに接続。\n")
;; 置き換え部分終了
  (princ "\
  +N ファイル名         指定ファイルを N 行目から表示。

他のすべて形式の引数はファイル名と解釈され、編集のためにバッファに読み込ま
れます。

XEmacs には、オンラインの自習書とマニュアル  (両方とも英語版のみ) が付属し
ています。自習書を開始するには、XEmacs 開始後に、^Ht (Control-h t) をタイプ
してください。マニュアルを読むには、^Hi、さらに詳しいヘルプ情報は、^H^H^H 
(Control-h を 3 回) をタイプしてください。\n")
    (kill-emacs 0)))
