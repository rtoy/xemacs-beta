;;; skk-foreword.el --- 前書き
;; Copyright (C) 1997 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Murata Shuuichirou  <mrt@mickey.ai.kyutech.ac.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-foreword.el,v 1.1 1997/12/02 08:48:37 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:37 $

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

;; 前に定義しておかなければなりません。このファイルは、このよ
;; うに、変数の宣言以前に定義しておかなければならないものをま
;; とめたものです。ユーザー変数の定義の前に、ごちゃごちゃとユ
;; ーザーに興味がないものが並んでいたのでは、ユーザーフレンド
;; リーではないと考えるからです。
;; 
;; Following people contributed modifications to skk-foreword.el (Alphabetical order):
;;       Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;       Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>

;;; Change log:

;;; Code:
(require 'easymenu)

;; necessary macro and functions to be declared before user variable declarations.

;; From viper-util.el.  Welcome!
(defmacro skk-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	       (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
     )))

;; From emu.el of tm package.   Welcome!  Its original is defun-maybe.
(defmacro skk-defunsoft (name &rest everything-else)
  (or (fboundp name)
      (` (or (fboundp (quote (, name)))
             (defun (, name) (,@ everything-else)) ))))

(put 'skk-deflocalvar 'lisp-indent-function 'defun)
(put 'skk-defunsoft 'lisp-indent-function 'defun)

;;(defun skk-get (symbol property-name &optional default)
;;  ;; SYMBOL の属性リストに PROPERTY-NAME という属性名があればその属性値を返す。
;;  ;; なければ DEFAULT を返す (DEFAULT が指定されていなければ NIL)。属性名の比
;;  ;; 較は、eq で行なう。
;;  (if default
;;      (let ((pl (memq property-name (symbol-plist symbol))))
;;        (if pl (nth 1 pl) default) )
;;    (get symbol property-name) ))

(defun skk-terminal-face-p ()
  (and (not window-system)
       (fboundp 'frame-face-alist) ;; 変数名みたいな関数だな...。
       (fboundp 'selected-frame) ))

;;; skk-defunsofts  Define nothing if it is already there.

;; eval-after-load is not defined in XEmacs but after-load-alist is usable.
;; See subr.el in XEmacs.
(skk-defunsoft eval-after-load (file form)
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
        (nconc elt (list form))))
  form )

(skk-defunsoft set-cursor-color (color-name)
  (set-frame-property (selected-frame) 'cursor-color
                      (if (color-instance-p color-name)
                          color-name
                        (make-color-instance color-name))))

(skk-defunsoft rassoc (key alist)
  (cond ((null alist) nil)
        ((and (consp (car alist))
              (equal key (cdr (car alist))) (car alist)))
        (t (rassoc key (cdr alist))) ))

(skk-defunsoft add-to-list (list-var element)
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var))) ))

;; mule-3 では、cancel-undo-boundary がない。    
;; from mule-util.el
(skk-defunsoft cancel-undo-boundary ()
  ;; buffer-undo-list の car の nil を消し、undo コマンドが直近のバッファ
  ;; の変更で止まらないようにする。buffer-undo-list における nil は、変
  ;; 更群と変更群の境界を示すデリミタの働きをしている。
  (if (and (consp buffer-undo-list)
           ;; car が nil だったらそれを消す。
           (null (car buffer-undo-list)) )
      (setq buffer-undo-list (cdr buffer-undo-list)) ))

(skk-defunsoft match-string (n str)
  (substring str (match-beginning n) (match-end n)) )

;;;; version specific matter.
(eval-and-compile
  (defconst skk-xemacs (and (featurep 'mule)
			    (string-match "XEmacs" emacs-version) )
    "Non-nil であれば、XEmacs で SKK を使用していることを示す。" )
  
  (defconst skk-mule3 (and (featurep 'mule) (boundp 'mule-version)
                           (string< "3.0" mule-version))
    "Non-nil であれば、Mule 3 で SKK を使用していることを示す。" )
  
  (defconst skk-mule (featurep 'mule)
    "Non-nil であれば、Mule で SKK を使用していることを示す。" )

  (defconst skk-20 (or skk-mule3 skk-xemacs)
    "Non-nil であれば、Emacs の ver. 20 で SKK を使用していることを示す。" )

  (cond ((or (and (boundp 'epoch::version) epoch::version)
             (string< (substring emacs-version 0 2) "18") )
         (message "THIS SKK requires Emacs 19")
         (sit-for 2) )
        ;; for XEmacs
        (skk-xemacs
         (defalias 'skk-buffer-substring 'buffer-substring-no-properties)
         (defalias 'skk-character-to-event 'character-to-event)
         (defalias 'skk-event-to-character 'event-to-character)
         (defalias 'skk-int-char 'int-char)
         (defalias 'skk-read-event 'next-command-event)
         (defsubst skk-unread-event (event)
           ;; Unread single EVENT.
           (setq unread-command-events
 		 (nconc unread-command-events (list event))) )
         (defalias 'skk-make-overlay 'make-extent)
         (defalias 'skk-move-overlay 'set-extent-endpoints)
         (defalias 'skk-overlay-put 'set-extent-property)
         (defalias 'skk-overlayp 'extentp)
         (defalias 'skk-delete-overlay 'detach-extent)
	 (defalias 'skk-charsetp 'find-charset)
	 (defalias 'skk-char-to-string 'char-to-string)
	 (defun skk-make-char (charset n1 n2)
	   (make-char charset
		      (logand (lognot 128) n1)
		      (logand (lognot 128) n2) ))
         ;; skk-kana-input-event-type
         ;; (event でなく文字を引数にとるようにしたので,
         ;;  skk-kana-input-char-type の名前を使いました)
         (defun skk-kana-input-char-type (char)
           ;; "Return type of CHAR for `skk-kana-input'."
           ;; CHAR is character or nil
           (cond ((and char
                       (<= 0 char) (< char (length skk-char-type-vector)))
                  ;; this is normal ascii char
                  (aref skk-char-type-vector char))
                 ;; if you want to perform delete by event other than ascii
                 ;; keystroke event, following clause should be modified to
                 ;; return type 5 when apropriciate.
                 (t nil) ))

         (defmacro with-output-to-temp-buffer (bufname &rest body)
           (let ((obuf (make-symbol "obuf"))
                 (buf (make-symbol "buf")) )
             `(let ((,obuf (current-buffer))
                    (,buf (get-buffer-create ,bufname))
                    standard-output  )
                (set-buffer ,buf)
                (erase-buffer)
                (setq standard-output ,buf)
                ,@body
                (pop-to-buffer ,buf) )))

	 (defmacro with-current-buffer (buffer &rest body)
	   "Execute the forms in BODY with BUFFER as the current buffer.
             The value returned is the value of the last form in BODY.
             See also `with-temp-buffer'."
	   (` (save-current-buffer
	      (set-buffer (, buffer))
	      (,@ body))))
	 
	 (defmacro with-temp-file (file &rest forms)
	   "Create a new buffer, evaluate FORMS there, and write the buffer to FILE.
             The value of the last form in FORMS is returned, like `progn'.
             See also `with-temp-buffer'."
	   (let ((temp-file (make-symbol "temp-file"))
		 (temp-buffer (make-symbol "temp-buffer")))
	     (` (let (((, temp-file) (, file))
		    ((, temp-buffer)
		     (get-buffer-create (generate-new-buffer-name " *temp file*"))))
		(unwind-protect
		    (prog1
			(with-current-buffer (, temp-buffer)
			  (,@ forms))
		      (with-current-buffer (, temp-buffer)
			(widen)
			(write-region (point-min) (point-max) (, temp-file) nil 0)))
		  (and (buffer-name (, temp-buffer))
		       (kill-buffer (, temp-buffer))))))))
	 (defmacro combine-after-change-calls (&rest body)
	   (` (unwind-protect
		(let ((combine-after-change-calls t))
		  . (, body))
	      (combine-after-change-execute))))
	 (defmacro combine-after-change-execute (&rest body)
	   body )
         )
        ;; for Emacs 19
        (t
         (defmacro skk-character-to-event (char) char);; 何もしない
         (defmacro skk-int-char (char) char);; 何もしない
         (defsubst skk-unread-event (event)
           ;; Unread single EVENT.
           (setq unread-command-events
 		 (nconc unread-command-events (list event))))
         (defalias 'skk-read-event 'read-event)
         (defalias 'skk-make-overlay 'make-overlay)
         (defalias 'skk-move-overlay 'move-overlay)
         (defalias 'skk-overlay-put 'overlay-put)
         (defalias 'skk-overlayp 'overlayp)
         (defalias 'skk-delete-overlay 'delete-overlay)
         (defun skk-event-to-character (event)
           ;; Return character of keystroke EVENT.
           (cond ((symbolp event)
                  ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
                  (let ((mask (get event 'event-symbol-element-mask)))
                    (if mask
                        (let ((base (get (car mask) 'ascii-character)))
                          (if base
                              (logior base (car (cdr mask))))))))
                 ((integerp event)
                  event)
                 (t nil) ))
         (defun skk-kana-input-char-type (event)
           ;; "Return type of EVENT for `skk-kana-input'."
           (cond ((and (integerp event)
                       (<= 0 event) (< event (length skk-char-type-vector)))
                  ;; this is normal ascii keystroke event
                  (aref skk-char-type-vector event))
                 ;; if you want to perform delete by event other than ascii
                 ;; keystroke event, following clause should be modified to
                 ;; return type 5 when apropriciate.
                 (t nil) ))

	 ;; overwrite built-in combine-after-change-execute
	 (defmacro combine-after-change-execute (&rest body)
	   body )
	 (defmacro combine-after-change-calls (&rest body)
	   (` (unwind-protect
		  (let ((combine-after-change-calls t))
		    . (, body))
		(combine-after-change-execute))))
	 (if skk-mule3
	     nil

           (defmacro with-output-to-temp-buffer (bufname &rest body)
             (let ((obuf (make-symbol "obuf"))
                   (buf (make-symbol "buf")) )
               `(let ((,obuf (current-buffer))
                      (,buf (get-buffer-create ,bufname))
                      standard-output  )
                  (set-buffer ,buf)
                  (erase-buffer)
                  (setq standard-output ,buf)
                  ,@body
                  (pop-to-buffer ,buf) )))

	   (defmacro save-current-buffer (&rest body)
	     (let ((orig-buffer (make-symbol "orig-buffer")))
	       (` (let (((, orig-buffer) (current-buffer)))
		    (unwind-protect
			(progn (,@ body))
		      (set-buffer (, orig-buffer)) )))))
	   
	   (defmacro with-current-buffer (buffer &rest body)
	     "Execute the forms in BODY with BUFFER as the current buffer.
	      The value returned is the value of the last form in BODY.
	      See also `with-temp-buffer'."
	      (` (save-current-buffer
		   (set-buffer (, buffer))
		   (,@ body))))
	   
	   (defmacro with-temp-file (file &rest forms)
	     "Create a new buffer, evaluate FORMS there, and write the buffer to FILE.
	      The value of the last form in FORMS is returned, like `progn'.
	      See also `with-temp-buffer'."
	      (let ((temp-file (make-symbol "temp-file"))
		    (temp-buffer (make-symbol "temp-buffer")))
		(` (let (((, temp-file) (, file))
			 ((, temp-buffer)
			  (get-buffer-create (generate-new-buffer-name " *temp file*"))))
		     (unwind-protect
			 (prog1
			     (with-current-buffer (, temp-buffer)
			       (,@ forms))
			   (with-current-buffer (, temp-buffer)
			     (widen)
			     (write-region (point-min) (point-max) (, temp-file) nil 0)))
		       (and (buffer-name (, temp-buffer))
			    (kill-buffer (, temp-buffer))))))))
	   
	   (defmacro with-temp-buffer (&rest forms)
	     "Create a temporary buffer, and evaluate FORMS there like `progn'.
	      See also `with-temp-file' and `with-output-to-string'."
	      (let ((temp-buffer (make-symbol "temp-buffer")))
		(` (let (((, temp-buffer)
			  (get-buffer-create (generate-new-buffer-name " *temp*"))))
		     (unwind-protect
			 (with-current-buffer (, temp-buffer)
			   (,@ forms))
		       (and (buffer-name (, temp-buffer))
			    (kill-buffer (, temp-buffer))))))))
	   
	   (defmacro with-output-to-string (&rest body)
	     "Execute BODY, return the text it sent to `standard-output', as a string."
	      (` (let ((standard-output
			(get-buffer-create (generate-new-buffer-name " *string-output*"))))
		   (let ((standard-output standard-output))
		     (,@ body))
		   (with-current-buffer standard-output
		     (prog1
			 (buffer-string)
		       (kill-buffer nil)))))))
	 (cond
	  ((string< "20" emacs-version)
	   ;; For emacs 20
	   (defalias 'skk-charsetp 'charsetp)
	   (defalias 'skk-make-char 'make-char)
	   (defalias 'skk-buffer-substring 'buffer-substring-no-properties)
	   (defun skk-char-to-string (char)
	     (condition-case nil
		 (char-to-string char)
	       (error
		nil ))))
	  ((string< "19.29" emacs-version)
	   ;; For emacs 19.29, 19.30...
	   (defalias 'skk-charsetp 'character-set)
	   (defalias 'skk-make-char 'make-character)
	   (defalias 'skk-buffer-substring 'buffer-substring-no-properties)
	   (defalias 'skk-char-to-string 'char-to-string) )
	  (t
	   ;; For emacs 19...19.28
	   (defalias 'skk-charsetp 'character-set)
	   (defalias 'skk-make-char 'make-character)
	   (defalias 'skk-buffer-substring 'buffer-substring)
	   (defalias 'skk-char-to-string 'char-to-string) )
	  ))))

(defconst skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (cond
   (skk-xemacs
    (if (< (apply '+ (color-rgb-components
                      (face-property 'default 'background)))
           (/ (apply '+ (color-rgb-components
                         (make-color-specifier "white"))) 3))
        'dark
      'light)
    )
   ((and window-system (x-display-color-p))
    (let ((bg-resource (x-get-resource ".backgroundMode"
                                       "BackgroundMode"))
          params )
      (if bg-resource
          (intern (downcase bg-resource))
        (setq params (frame-parameters))
        ;; Mule for Win32 を Windows 95 で動かしているときは、system-type は？
        ;; -> windows-nt でした -- Mikio (会社の Win 95 に Mule for Win32 を
        ;; 入れた)。
        (cond ((and (eq system-type 'windows-nt)

                    (fboundp 'win32-color-values) )
               (< (apply '+ (win32-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (win32-color-values "white")) 3) )
               'dark )
              ((and (memq system-type '(ms-dos windows-nt))
                    (fboundp 'x-color-values) )
               (if (string-match "light"
                                 (cdr (assq 'background-color params)) )
                   'light
                 'dark ))
              ((< (apply '+ (x-color-values
                             (cdr (assq 'background-color params)) ))
                  (/ (apply '+ (x-color-values "white")) 3) )
               'dark )
              (t 'light) ))))
   (t 'mono )))

(eval-after-load "hilit19"
  '(mapcar (function
            (lambda (pattern)
              (hilit-add-pattern
               (car pattern) (cdr pattern)
               (cond ((eq skk-background-mode 'mono)
                      'bold )
                     ((eq skk-background-mode 'light)
                      'RoyalBlue )
                     (t 'cyan) )
               'emacs-lisp-mode )))
           '(("^\\s *(skk-deflocalvar\\s +\\S +" . "")
             ("^\\s *(skk-defunsoft\\s +\\S +" . "") )))

(defun skk-define-menu-bar-map (map)
  ;; SKK メニューのトップに出現するコマンドのメニューへの定義を行なう。
  (easy-menu-define
   skk-menu map
   "Menu used in SKK mode."
   '("SKK"
     ("Convert Region and Echo"
      ("Gyakubiki"
       ["to Hirakana" skk-gyakubiki-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hirakana, All Candidates"
        ;; あれれ、lambda 関数は定義できないのか？？？  動かないぞ...。
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-message
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ("Hurigana"
       ["to Hirakana" skk-hurigana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hirakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-message start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-message
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-katakana-message
                     start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      )
     ("Convert Region and Replace"
      ["Ascii" skk-ascii-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Gyakubiki"
       ["to Hirakana" skk-gyakubiki-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hirakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-gyakubiki-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-gyakubiki-katakana-region
                     start end 'all-candidates ) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Hiragana" skk-hiragana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ("Hurigana"
       ["to Hirakana" skk-hurigana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Hirakana, All Candidates"
        (function (lambda (start end) (interactive "r")
                    (skk-hurigana-region start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana" skk-hurigana-katakana-region
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       ["to Katakana, All Candidates" (function
                                       (lambda (start end) (interactive "r")
                                         (skk-hurigana-katakana-region
                                          start end 'all-candidates) ))
        (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
       )
      ["Katakana" skk-katakana-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Romaji" skk-romaji-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["Zenkaku" skk-zenkaku-region
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      )
     ["Count Jisyo Candidates" skk-count-jisyo-candidates
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Save Jisyo" skk-save-jisyo
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ["Undo Kakutei" skk-undo-kakutei
      (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
     ("User Options"
      ["skk-allow-spaces-newlines-and-tabs" skk-menu-allow-spaces-newlines-and-tabs
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-auto-start-henkan" skk-menu-auto-henkan
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-auto-insert-paren" skk-menu-auto-insert-paren
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-auto-okuri-process"
       (function (lambda ()
                   (interactive)
                   (skk-menu-auto-okuri-process)
                   (skk-adjust-search-prog-list-for-auto-okuri) ))
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0))]
      ["skk-compare-jisyo-size-when-saving" skk-menu-compare-jisyo-size-when-saving
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-convert-okurigana-into-katakana" skk-menu-convert-okurigana-into-katakana
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-count-private-jisyo-candidates-exactly"
       skk-menu-count-private-jisyo-entries-exactly
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["skk-dabbrev-like-completion" skk-menu-dabbrev-like-completion
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-date-ad" skk-menu-date-ad
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["skk-delete-implies-kakutei" skk-menu-delete-implies-kakutei
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-delete-okuri-when-quit" skk-menu-delete-okuri-when-quit
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0) ) ]
      ["skk-echo" skk-menu-echo
       (or (not (boundp 'skktut-problem-count)) (eq skktut-problem-count 0)) ]
      ["skk-egg-like-newline" skk-menu-egg-like-newline
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-henkan-okuri-strictly" skk-menu-henkan-okuri-strictly
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-henkan-strict-okuri-precedence" skk-menu-henkan-strict-okuri-precedence
       (or (not (boundp 'skktut-problem-count))
	   (eq skktut-problem-count 0)) ]
      ["skk-japanese-message-and-error" skk-menu-japanese-message-and-error
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-kakutei-early" skk-menu-kakutei-early
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-numeric-conversion-float-num" skk-menu-numeric-conversion-float-num
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-process-okuri-early" skk-menu-process-okuri-early
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-report-server-response" skk-menu-report-server-response
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-romaji-*-by-hepburn" skk-menu-romaji-*-by-hepburn
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-uniq-numerals" skk-menu-uniq-numerals
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-use-color-cursor" skk-menu-use-color-cursor
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-use-kakasi" skk-menu-use-kakasi
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-use-numeric-conversion" skk-menu-use-numeric-conversion
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0)) ]
      ["skk-use-face" skk-menu-use-overlay
       (or (not (boundp 'skktut-problem-count))
           (eq skktut-problem-count 0))] )
     ["Version" skk-version
      (or (not (boundp 'skktut-problem-count))
          (eq skktut-problem-count 0)) ]
     )))

(defun skk-update-autoloads (dir)
  (interactive "DUpdate skk autoloads from directory: ")
  (require 'autoload)
  (let ((generated-autoload-file "skk-vars.el"))
    (update-autoloads-from-directory dir)))

(provide 'skk-foreword)
;;; skk-forwords.el ends here
