;;; arabic.el --- minor mode for editing Arabic.
;; Copyright (C) 1994 Free Software Foundation, Inc.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Mule 2.3.

;;; 94.6.13  created for Mule Ver.1.1 by Takahashi N. <ntakahas@etl.go.jp>

(require 'visual-mode)

(defvar arabic-mode-indicator " [2](3=a:GJ[0](B"
  "String displayed in mode-line.
\" Arabic\" for Arabic keyboard input, \" [2](3=a:GJ[0](B\".")

(make-variable-buffer-local 'arabic-mode-indicator)

;;;###autoload
(defvar arabic-mode nil
  "Non-nil if in arabic-mode.")

(make-variable-buffer-local 'arabic-mode)

(if (not (assq 'arabic-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(arabic-mode arabic-mode-indicator) minor-mode-alist)))

(define-key global-map [(meta \\)] 'arabic-mode)

(defvar arabic-input-arabic-char t
  "Non-nil if key input is Arabic.  Nil if key input is ASCII.")

(make-variable-buffer-local 'arabic-input-arabic-char)

(defvar arabic-input-keymap 'arabic-keymap-0
  "Specify which input table is used for Arabic input.  Should be on of:
arabic-keymap-0 (default),
arabic-keymap-1 (Farsi standard), or
arabic-keymap-2 (Microsoft Arabic).")

(defvar arabic-translate-table
  (cond
   ((eq arabic-input-keymap 'arabic-keymap-0)
    [?[2](3![0](B  ?[2](3"[0](B  ?[2](3-[0](B  nil nil nil nil ?'  ?[2](3#[0](B  ?[2](3$[0](B  nil nil ?[2](3%[0](B  nil ?[2](3&[0](B  ?[2](49[0](B
     ?(2!(B  ?(2"(B  ?(2#(B  ?(2$(B  ?(2%(B  ?(2&(B  ?(2'(B  ?(2((B  ?(2)(B  ?(2*(B  ?[2](3'[0](B  ?[2](3([0](B  ?[2](3*[0](B  nil ?[2](3+[0](B  ?[2](3)[0](B
     nil ?[2](4][0](B nil ?[2](4g[0](B ?[2](4A[0](B nil nil ?[2](4O[0](B ?[2](4-[0](B nil nil ?[2](41[0](B nil nil nil nil
     nil nil nil ?[2](4=[0](B ?[2](4E[0](B nil nil nil ?[2](3h[0](B  nil ?[2](4I[0](B nil nil nil nil nil
     ?[2](4M[0](B ?[2](38[0](B  ?[2](4#[0](B ?[2](4'[0](B ?[2](3B[0](B  nil ?[2](4Q[0](B ?[2](4k[0](B ?[2](3Z[0](B  nil ?[2](4)[0](B ?[2](4U[0](B ?[2](4Y[0](B ?[2](3T[0](B  ?[2](4[[0](B ?[2](3<[0](B
     ?[2](4e[0](B ?[2](4S[0](B ?[2](3F[0](B  ?[2](45[0](B ?[2](4%[0](B nil nil ?[2](3^[0](B  ?[2](3D[0](B  ?[2](4_[0](B ?[2](3H[0](B  nil ?|  nil nil])
   ((eq arabic-input-keymap 'arabic-keymap-1)
    [?[2](3![0](B  ?[2](3"[0](B  nil nil nil nil nil nil ?[2](3#[0](B  ?[2](3$[0](B  nil nil ?[2](3%[0](B  nil ?[2](3&[0](B  nil
     ?(2!(B  ?(2"(B  ?(2#(B  ?(2$(B  ?(2%(B  ?(2&(B  ?(2'(B  ?(2((B  ?(2)(B  ?(2*(B  ?[2](3'[0](B  ?[2](4U[0](B ?[2](3*[0](B  nil ?[2](3+[0](B  ?[2](3)[0](B
     nil nil ?[2](3h[0](B  nil nil nil ?[2](4e[0](B ?[2](3.[0](B  nil nil nil nil nil nil nil nil
     nil nil nil nil nil nil ?[2](30[0](B  nil nil nil nil ?[2](4)[0](B nil ?[2](4g[0](B nil nil
     nli ?[2](49[0](B ?[2](3H[0](B  ?[2](3D[0](B  ?[2](4_[0](B ?[2](4S[0](B ?[2](4#[0](B ?[2](38[0](B  ?[2](4%[0](B nil ?[2](4Y[0](B ?[2](4[[0](B ?[2](3T[0](B  ?[2](3^[0](B  ?[2](3F[0](B  ?[2](41[0](B
     ?[2](4-[0](B ?[2](4A[0](B ?[2](4Q[0](B ?[2](45[0](B ?[2](4O[0](B ?[2](3Z[0](B  ?[2](3B[0](B  ?[2](4=[0](B ?[2](4E[0](B ?[2](4M[0](B ?[2](4I[0](B nil nli nil nil ])
   (t
    [?[2](3![0](B  ?[2](3"[0](B  ?\" ?#  ?$  ?%  ?&  ?'  ?[2](3#[0](B  ?[2](3$[0](B  ?*  ?+  ?[2](3^[0](B  ?-  ?[2](3H[0](B  ?[2](4I[0](B
     ?(2!(B  ?(2"(B  ?(2#(B  ?(2$(B  ?(2%(B  ?(2&(B  ?(2'(B  ?(2((B  ?(2)(B  ?(2*(B  ?[2](3'[0](B  ?[2](4U[0](B ?,  ?=  ?.  ?[2](3)[0](B
     ?@  nil ?[2](3b[0](B  ?{  ?[  nil ?]  ?[2](3c[0](B  ?[2](30[0](B  nil nil ?[2](3%[0](B  ?/  ?`  ?[2](3.[0](B  nil
     ?[2](3([0](B  nil nil nil ?[2](3d[0](B  ?'  ?}  nil nil ?[2](34[0](B  ?~  ?[2](4)[0](B ?\\ ?[2](3B[0](B  ?^  ?_
     ?[2](3D[0](B  ?[2](49[0](B ?[2](3e[0](B  ?[2](32[0](B  ?[2](4_[0](B ?[2](4'[0](B ?[2](4#[0](B ?[2](4Y[0](B ?[2](38[0](B  ?[2](3Z[0](B  ?[2](4%[0](B ?[2](4[[0](B ?[2](3T[0](B  ?[2](3<[0](B  ?[2](4][0](B ?[2](41[0](B
     ?[2](4-[0](B ?[2](4A[0](B ?[2](4S[0](B ?[2](45[0](B ?[2](4Q[0](B ?[2](4M[0](B ?[2](3F[0](B  ?[2](4=[0](B ?[2](3-[0](B  ?[2](4O[0](B ?[2](4![0](B ?<  ?|  ?>  nil ])))

(defvar arabic-mode-map
  (let ((map (make-keymap)))
    (substitute-key-definition 'self-insert-command
			       'arabic-self-insert-command
			       map global-map)

    (define-key map [(control c) (control c)] 'arabic-mode)
    (define-key map [(control d)]       'arabic-delete-char)
    (define-key map [(control k)]       'arabic-kill-line)
    (define-key map [(control m)]       'arabic-newline)
    (define-key map [(control o)]       'arabic-open-line)
    (define-key map [(control w)]       'arabic-kill-region)
    (define-key map [(control y)]       'arabic-yank)
    (define-key map [delete]       'arabic-backward-delete-char)
    (define-key map [(meta d)]       'arabic-delete-word)
    (define-key map [(meta y)]       'arabic-yank-pop)
    (define-key map [(meta z)]       'arabic-help)
    (define-key map [(meta \\)]      'arabic-toggle-input-char)
    (define-key map [(meta delete)]    'arabic-backward-kill-word)

    (define-key map [(control n)]       'visual-next-line)
    (define-key map [(control p)]       'visual-previous-line)
    (define-key map [(meta <)]       'visual-beginning-of-buffer)
    (define-key map [(meta >)]       'visual-end-of-buffer)
    (define-key map [up]          'visual-previous-line)
    (define-key map [down]        'visual-next-line)
    (define-key map [home]        'visual-beginning-of-buffer)
    (define-key map [end]         'visual-end-of-buffer)
    (define-key map [left]        'visual-move-to-left-char)
    (define-key map [right]       'visual-move-to-right-char)
    (define-key map [(meta left)]      'visual-move-to-left-word)
    (define-key map [(meta right)]     'visual-move-to-right-word)

    (if visual-use-lr-commands
	(progn
	  (define-key map [(control a)] 'visual-left-end-of-line)
	  (define-key map [(control b)] 'visual-move-to-left-char)
	  (define-key map [(control e)] 'visual-right-end-of-line)
	  (define-key map [(control f)] 'visual-move-to-right-char)
	  (define-key map [(meta b)] 'visual-move-to-left-word)
	  (define-key map [(meta f)] 'visual-move-to-right-word))
      (define-key map [(control a)]       'visual-beginning-of-line)
      (define-key map [(control b)]       'visual-backward-char)
      (define-key map [(control e)]       'visual-end-of-line)
      (define-key map [(control f)]       'visual-forward-char)
      (define-key map [(meta b)]       'visual-backward-word)
      (define-key map [(meta f)]       'visual-forward-word))
  
     (cond
      ((eq arabic-input-keymap 'arabic-keymap-0)
       (define-key map [?~] 'arabic-insert-madda)
       (define-key map [?'] 'arabic-insert-hamza)
       (define-key map [?a] 'arabic-insert-alif)
       (define-key map [?_] 'arabic-make-connection)
       (define-key map [?|] 'arabic-cut-connection))
      ((eq arabic-input-keymap 'arabic-keymap-1)
       (define-key map [?~] 'arabic-insert-madda)
       (define-key map [?'] 'arabic-insert-hamza)
       (define-key map [?a] 'arabic-insert-alif)
       (define-key map [?_] 'arabic-make-connection)
       (define-key map [?|] 'arabic-cut-connection)
       (define-key map [(alt \;)] 'arabic-insert-gaaf)
       (define-key map [(alt v)]  'arabic-insert-isolated-hamza))
      (t
       (define-key map [(alt z)] 'arabic-insert-madda)
       (define-key map [(alt x)] 'arabic-insert-hamza)
       (define-key map [(alt h)] 'arabic-insert-alif)
       (define-key map [(alt _)] 'arabic-make-connection)
       (define-key map [(alt |)] 'arabic-cut-connection)))
 
     map)
   "minor-mode-keymap for arabic-mode.")
 
 (if (not (assq 'arabic-mode minor-mode-map-alist))
     (setq minor-mode-map-alist
 	  (cons (cons 'arabic-mode arabic-mode-map) minor-mode-map-alist)))
 
 (defvar arabic-help-string
   (cond
    ((eq arabic-input-keymap 'arabic-keymap-0)
     "\
                     Keymap in Arabic-mode
 
 +----------------------------------------------------------------+
 |! [2](3"[0](B |@   |#   |$   |%   |^   |&   |*   |( [2](3#[0](B |) [2](3$[0](B |_   |+   |~   |
 |1 (2"(B |2 (2#(B |3 (2$(B |4 (2%(B |5 (2&(B |6 (2'(B |7 (2((B |8 (2)(B |9 (2*(B |0 (2!(B |-   |=   |` [2](4M[0](B|
 +----------------------------------------------------------------+
   |Q   |W   |E   |R   |T [2](4E[0](B|Y   |U   |I   |O   |P   |
   |q [2](4S[0](B|w [2](3^[0](B |e   |r [2](3F[0](B |t [2](4%[0](B|y [2](4_[0](B|u   |i   |o [2](3<[0](B |p [2](4e[0](B|
   +--------------------------------------------------------+
     |A [2](4][0](B|S [2](4=[0](B|D [2](4A[0](B|F   |G [2](4O[0](B|H [2](4-[0](B|J   |K [2](41[0](B|L   |: [2](3'[0](B |\" [2](3-[0](B |
     |a [2](38[0](B |s [2](45[0](B|d [2](3B[0](B |f [2](4Q[0](B|g [2](4k[0](B|h [2](3Z[0](B |j [2](4)[0](B|k [2](4U[0](B|l [2](4Y[0](B|; [2](3([0](B |'   |
     +------------------------------------------------------+
       |Z [2](4I[0](B|X [2](3h[0](B |C [2](4g[0](B|V   |B   |N   |M   |< [2](3*[0](B |> [2](3+[0](B |? [2](3)[0](B |
       |z [2](3H[0](B |x [2](3D[0](B |c [2](4'[0](B|v   |b [2](4#[0](B|n [2](4[[0](B|m [2](3T[0](B |, [2](3%[0](B |. [2](3&[0](B |/ [2](49[0](B|
       +-------------------------------------------------+")
 
    ((eq arabic-input-keymap 'arabic-keymap-1)
     "\
                     Keymap in Arabic-mode                   +--------------+
 							    | ALT     SHIFT|
 +-------------------------------------------------+	    |ASCII   ARABIC|
|  [2](3"[0](B |    |    |    |    |    |    |    |  [2](3#[0](B |  [2](3$[0](B |	    +--------------+
|1 (2"(B |2 (2#(B |3 (2$(B |4 (2%(B |5 (2&(B |6 (2'(B |7 (2((B |8 (2)(B |9 (2*(B |0 (2!(B |
+-------------------------------------------------------------+
  |    |    |    |    |    |    |    |    |    |    |    |    |
  |q [2](4A[0](B|w [2](4=[0](B|e [2](4S[0](B|r [2](4Q[0](B|t [2](4O[0](B|y [2](4M[0](B|u [2](3Z[0](B |i   |o [2](41[0](B|p [2](4-[0](B|[ [2](4)[0](B|] [2](4g[0](B|
  +-----------------------------------------------------------+
    |    |    |    |  [2](4e[0](B|  [2](3.[0](B |    |    |    |    |[2](4k[0](B [2](3'[0](B|
    |a [2](49[0](B|s [2](45[0](B|d [2](4_[0](B|f [2](4#[0](B|g [2](38[0](B |h [2](4%[0](B|j [2](4Y[0](B|k [2](4[[0](B|l [2](3T[0](B |; [2](4U[0](B|
    +---------------------------------------------------+
      |    |    |    |[2](3-[0](B [2](30[0](B |  [2](3h[0](B |    |    |  [2](3*[0](B |  [2](3+[0](B |  [2](3)[0](B |
      |z [2](4I[0](B|x [2](4E[0](B|c [2](3D[0](B |v [2](3B[0](B |b [2](3H[0](B |n [2](3F[0](B |m [2](3^[0](B |, [2](3%[0](B |. [2](3&[0](B |/   |
      +-------------------------------------------------+")

   (t
    "\
                    +-----------------+
                    |S-ASCII  S-Arabic|
                    | ASCII    Arabic |                +----+
                    +-----------------+                ||  ||
                                                       |\\  \\|
+-----------------------------------------------------------+
|!  [2](3"[0](B|@  @|#  #|$  $|%  %|^  ^|&  &|*  *|(  [2](3#[0](B|)  [2](3$[0](B|_  _|+  +|
|1  (2"(B|2  (2#(B|3  (2$(B|4  (2%(B|5  (2&(B|6  (2'(B|7  (2((B|8  (2)(B|9  (2*(B|0  (2!(B|-  -|=  =|
+-------------------------------------------------------------+
  |Q   |W   |E   |R   |T  [2](3d[0](B|Y  [2](34[0](B|U  '|I   |O   |P  [2](3([0](B|{  <|}  >|
  |  [2](4A[0](B|  [2](4=[0](B|  [2](4'[0](B|  [2](4S[0](B|  [2](4Q[0](B|  [2](4O[0](B|  [2](4M[0](B|   [2](3Z[0](B|  [2](41[0](B|  [2](4-[0](B|[ [2](4)[0](B|]  [2](3B[0](B|
  +-------------------------------------------------------------+
    |A   |S   |D  [|F  ]|G  [2](3c[0](B|H  [2](30[0](B|J   |K  [2](3%[0](B|L  /|:  [2](3'[0](B|\"  \" |~[2](3,[0](B  |
    |  [2](49[0](B|  [2](45[0](B|  [2](4_[0](B|  [2](4#[0](B|  [2](4Y[0](B|   [2](38[0](B|  [2](4%[0](B|  [2](4[[0](B|   [2](3T[0](B|\; [2](4U[0](B|' [2](4E[0](B|`  [2](3D[0](B|
    +-----------------------------------------------------------+
      |Z  ~|X   |C  {|V  }|B  [2](3b[0](B|N  [2](3.[0](B|M  `|<  ,|>  [2](3&[0](B|?  [2](3)[0](B|
      |  [2](4![0](B|   [2](3-[0](B|   [2](32[0](B|   [2](3F[0](B|   [2](3e[0](B|  [2](4][0](B|   [2](3<[0](B|,  [2](3^[0](B|.  [2](3H[0](B|/ [2](4I[0](B|
      +-------------------------------------------------+"))

  "Document shown by arabic-help (M-z).")

;;;###autoload
(defun arabic-mode (&optional arg)
  "Toggle arabic-mode.  With ARG, turn arabic-mode on iff ARG is positive."
  (interactive "P")
  (if (null arg)
    (if arabic-mode (exit-arabic-mode) (enter-arabic-mode))
   (if (> (prefix-numeric-value arg) 0)
       (enter-arabic-mode)
     (exit-arabic-mode))))

(defun enter-arabic-mode nil
  "Enter arabic-mode."
  (interactive)
  (if (not arabic-mode)
      (progn
	(setq arabic-mode t
	      arabic-input-arabic-char t
	      arabic-mode-indicator " [2](3=a:GJ[0](B")
	(redraw-modeline t)
	(message "M-z to display arabic keymap.")
	(run-hooks 'arabic-mode-hooks))))

(defun exit-arabic-mode nil
  "Exit arabic-mode."
  (interactive)
  (if arabic-mode
      (progn
	(setq arabic-mode nil)
	(redraw-modeline t))))

(defconst *arabic-adding-connection-to-right*
  '((?[2](3.[0](B  . ?[2](3/[0](B )                         (?[2](3/[0](B  . ?[2](3/[0](B )
    (?[2](30[0](B  . ?[2](31[0](B )                         (?[2](31[0](B  . ?[2](31[0](B )
    (?[2](32[0](B  . ?[2](33[0](B )                         (?[2](33[0](B  . ?[2](33[0](B )
    (?[2](34[0](B  . ?[2](35[0](B )                         (?[2](35[0](B  . ?[2](35[0](B )
    (?[2](4![0](B . ?[2](4"[0](B) (?[2](36[0](B  . ?[2](37[0](B ) (?[2](37[0](B  . ?[2](37[0](B ) (?[2](4"[0](B . ?[2](4"[0](B)
    (?[2](36[0](B  . ?[2](37[0](B )                         (?[2](37[0](B  . ?[2](37[0](B )
    (?[2](38[0](B  . ?[2](39[0](B )                         (?[2](39[0](B  . ?[2](39[0](B )
    (?[2](4#[0](B . ?[2](4$[0](B) (?[2](3:[0](B  . ?[2](3;[0](B ) (?[2](3;[0](B  . ?[2](3;[0](B ) (?[2](4$[0](B . ?[2](4$[0](B)
    (?[2](3<[0](B  . ?[2](3=[0](B )                         (?[2](3=[0](B  . ?[2](3=[0](B )
    (?[2](4%[0](B . ?[2](4&[0](B) (?[2](3>[0](B  . ?[2](3?[0](B ) (?[2](3?[0](B  . ?[2](3?[0](B ) (?[2](4&[0](B . ?[2](4&[0](B)
    (?[2](4'[0](B . ?[2](4([0](B) (?[2](3@[0](B  . ?[2](3A[0](B ) (?[2](3A[0](B  . ?[2](3A[0](B ) (?[2](4([0](B . ?[2](4([0](B)
    (?[2](4)[0](B . ?[2](4,[0](B) (?[2](4*[0](B . ?[2](4+[0](B) (?[2](4+[0](B . ?[2](4+[0](B) (?[2](4,[0](B . ?[2](4,[0](B)
    (?[2](4-[0](B . ?[2](40[0](B) (?[2](4.[0](B . ?[2](4/[0](B) (?[2](4/[0](B . ?[2](4/[0](B) (?[2](40[0](B . ?[2](40[0](B)
    (?[2](41[0](B . ?[2](44[0](B) (?[2](42[0](B . ?[2](43[0](B) (?[2](43[0](B . ?[2](43[0](B) (?[2](44[0](B . ?[2](44[0](B)
    (?[2](3B[0](B  . ?[2](3C[0](B )                         (?[2](3C[0](B  . ?[2](3C[0](B )
    (?[2](3D[0](B  . ?[2](3E[0](B )                         (?[2](3E[0](B  . ?[2](3E[0](B )
    (?[2](3F[0](B  . ?[2](3G[0](B )                         (?[2](3G[0](B  . ?[2](3G[0](B )
    (?[2](3H[0](B  . ?[2](3I[0](B )                         (?[2](3I[0](B  . ?[2](3I[0](B )
    (?[2](45[0](B . ?[2](48[0](B) (?[2](46[0](B . ?[2](47[0](B) (?[2](47[0](B . ?[2](47[0](B) (?[2](48[0](B . ?[2](48[0](B)
    (?[2](49[0](B . ?[2](4<[0](B) (?[2](4:[0](B . ?[2](4;[0](B) (?[2](4;[0](B . ?[2](4;[0](B) (?[2](4<[0](B . ?[2](4<[0](B)
    (?[2](4=[0](B . ?[2](4@[0](B) (?[2](4>[0](B . ?[2](4?[0](B) (?[2](4?[0](B . ?[2](4?[0](B) (?[2](4@[0](B . ?[2](4@[0](B)
    (?[2](4A[0](B . ?[2](4D[0](B) (?[2](4B[0](B . ?[2](4C[0](B) (?[2](4C[0](B . ?[2](4C[0](B) (?[2](4D[0](B . ?[2](4D[0](B)
    (?[2](4E[0](B . ?[2](4H[0](B) (?[2](4F[0](B . ?[2](4G[0](B) (?[2](4G[0](B . ?[2](4G[0](B) (?[2](4H[0](B . ?[2](4H[0](B)
    (?[2](4I[0](B . ?[2](4L[0](B) (?[2](4J[0](B . ?[2](4K[0](B) (?[2](4K[0](B . ?[2](4K[0](B) (?[2](4L[0](B . ?[2](4L[0](B)
    (?[2](4M[0](B . ?[2](4N[0](B) (?[2](3J[0](B  . ?[2](3K[0](B ) (?[2](3K[0](B  . ?[2](3K[0](B ) (?[2](4N[0](B . ?[2](4N[0](B)
    (?[2](4O[0](B . ?[2](4P[0](B) (?[2](3L[0](B  . ?[2](3M[0](B ) (?[2](3M[0](B  . ?[2](3M[0](B ) (?[2](4P[0](B . ?[2](4P[0](B)
    (?[2](4Q[0](B . ?[2](4R[0](B) (?[2](3N[0](B  . ?[2](3O[0](B ) (?[2](3O[0](B  . ?[2](3O[0](B ) (?[2](4R[0](B . ?[2](4R[0](B)
    (?[2](4S[0](B . ?[2](4T[0](B) (?[2](3P[0](B  . ?[2](3Q[0](B ) (?[2](3Q[0](B  . ?[2](3Q[0](B ) (?[2](4T[0](B . ?[2](4T[0](B)
    (?[2](4U[0](B . ?[2](4X[0](B) (?[2](4V[0](B . ?[2](4W[0](B) (?[2](4W[0](B . ?[2](4W[0](B) (?[2](4X[0](B . ?[2](4X[0](B)
    (?[2](4Y[0](B . ?[2](4Z[0](B) (?[2](3R[0](B  . ?[2](3S[0](B ) (?[2](3S[0](B  . ?[2](3S[0](B ) (?[2](4Z[0](B . ?[2](4Z[0](B)
    (?[2](3T[0](B  . ?[2](3W[0](B ) (?[2](3U[0](B  . ?[2](3V[0](B ) (?[2](3V[0](B  . ?[2](3V[0](B ) (?[2](3W[0](B  . ?[2](3W[0](B )
    (?[2](4[[0](B . ?[2](4\[0](B) (?[2](3X[0](B  . ?[2](3Y[0](B ) (?[2](3Y[0](B  . ?[2](3Y[0](B ) (?[2](4\[0](B . ?[2](4\[0](B)
    (?[2](3Z[0](B  . ?[2](3][0](B ) (?[2](3[[0](B  . ?[2](3\[0](B ) (?[2](3\[0](B  . ?[2](3\[0](B ) (?[2](3][0](B  . ?[2](3][0](B )
    (?[2](3^[0](B  . ?[2](3_[0](B )                         (?[2](3_[0](B  . ?[2](3_[0](B )
    (?[2](4][0](B . ?[2](4^[0](B)                         (?[2](4^[0](B . ?[2](4^[0](B)
    (?[2](4_[0](B . ?[2](4`[0](B) (?[2](3`[0](B  . ?[2](3a[0](B ) (?[2](3a[0](B  . ?[2](3a[0](B ) (?[2](4`[0](B . ?[2](4`[0](B)
    (?[2](3b[0](B  . ?[2](4a[0](B)                         (?[2](4a[0](B . ?[2](4a[0](B)
    (?[2](3c[0](B  . ?[2](4b[0](B)                         (?[2](4b[0](B . ?[2](4b[0](B)
    (?[2](3d[0](B  . ?[2](4c[0](B)                         (?[2](4c[0](B . ?[2](4c[0](B)
    (?[2](3e[0](B  . ?[2](4d[0](B)                         (?[2](4d[0](B . ?[2](4d[0](B)
    (?[2](4e[0](B . ?[2](4f[0](B) (?[2](3f[0](B  . ?[2](3g[0](B ) (?[2](3g[0](B  . ?[2](3g[0](B ) (?[2](4f[0](B . ?[2](4f[0](B)
    (?[2](4g[0](B . ?[2](4j[0](B) (?[2](4h[0](B . ?[2](4i[0](B) (?[2](4i[0](B . ?[2](4i[0](B) (?[2](4j[0](B . ?[2](4j[0](B)
    (?[2](3h[0](B  . ?[2](3i[0](B )                         (?[2](3i[0](B  . ?[2](3i[0](B )
    (?[2](4k[0](B . ?[2](4n[0](B) (?[2](4l[0](B . ?[2](4m[0](B) (?[2](4m[0](B . ?[2](4m[0](B) (?[2](4n[0](B . ?[2](4n[0](B)))

(defconst *arabic-adding-connection-to-left*
  '((?[2](4![0](B . ?[2](36[0](B ) (?[2](36[0](B  . ?[2](36[0](B ) (?[2](37[0](B  . ?[2](37[0](B ) (?[2](4"[0](B . ?[2](37[0](B)
    (?[2](4#[0](B . ?[2](3:[0](B ) (?[2](3:[0](B  . ?[2](3:[0](B ) (?[2](3;[0](B  . ?[2](3;[0](B ) (?[2](4$[0](B . ?[2](3;[0](B )
    (?[2](4%[0](B . ?[2](3>[0](B ) (?[2](3>[0](B  . ?[2](3>[0](B ) (?[2](3?[0](B  . ?[2](3?[0](B ) (?[2](4&[0](B . ?[2](3?[0](B )
    (?[2](4'[0](B . ?[2](3@[0](B ) (?[2](3@[0](B  . ?[2](3@[0](B ) (?[2](3A[0](B  . ?[2](3A[0](B ) (?[2](4([0](B . ?[2](3A[0](B )
    (?[2](4)[0](B . ?[2](4*[0](B) (?[2](4*[0](B . ?[2](4*[0](B) (?[2](4+[0](B . ?[2](4+[0](B) (?[2](4,[0](B . ?[2](4+[0](B)
    (?[2](4-[0](B . ?[2](4.[0](B) (?[2](4.[0](B . ?[2](4.[0](B) (?[2](4/[0](B . ?[2](4/[0](B) (?[2](40[0](B . ?[2](4/[0](B)
    (?[2](41[0](B . ?[2](42[0](B) (?[2](42[0](B . ?[2](42[0](B) (?[2](43[0](B . ?[2](43[0](B) (?[2](44[0](B . ?[2](43[0](B)
    (?[2](45[0](B . ?[2](46[0](B) (?[2](46[0](B . ?[2](46[0](B) (?[2](47[0](B . ?[2](47[0](B) (?[2](48[0](B . ?[2](47[0](B)
    (?[2](49[0](B . ?[2](4:[0](B) (?[2](4:[0](B . ?[2](4:[0](B) (?[2](4;[0](B . ?[2](4;[0](B) (?[2](4<[0](B . ?[2](4;[0](B)
    (?[2](4=[0](B . ?[2](4>[0](B) (?[2](4>[0](B . ?[2](4>[0](B) (?[2](4?[0](B . ?[2](4?[0](B) (?[2](4@[0](B . ?[2](4?[0](B)
    (?[2](4A[0](B . ?[2](4B[0](B) (?[2](4B[0](B . ?[2](4B[0](B) (?[2](4C[0](B . ?[2](4C[0](B) (?[2](4D[0](B . ?[2](4C[0](B)
    (?[2](4E[0](B . ?[2](4F[0](B) (?[2](4F[0](B . ?[2](4F[0](B) (?[2](4G[0](B . ?[2](4G[0](B) (?[2](4H[0](B . ?[2](4G[0](B)
    (?[2](4I[0](B . ?[2](4J[0](B) (?[2](4J[0](B . ?[2](4J[0](B) (?[2](4K[0](B . ?[2](4K[0](B) (?[2](4L[0](B . ?[2](4K[0](B)
    (?[2](4M[0](B . ?[2](3J[0](B ) (?[2](3J[0](B  . ?[2](3J[0](B ) (?[2](3K[0](B  . ?[2](3K[0](B ) (?[2](4N[0](B . ?[2](3K[0](B )
    (?[2](4O[0](B . ?[2](3L[0](B ) (?[2](3L[0](B  . ?[2](3L[0](B ) (?[2](3M[0](B  . ?[2](3M[0](B ) (?[2](4P[0](B . ?[2](3M[0](B )
    (?[2](4Q[0](B . ?[2](3N[0](B ) (?[2](3N[0](B  . ?[2](3N[0](B ) (?[2](3O[0](B  . ?[2](3O[0](B ) (?[2](4R[0](B . ?[2](3O[0](B )
    (?[2](4S[0](B . ?[2](3P[0](B ) (?[2](3P[0](B  . ?[2](3P[0](B ) (?[2](3Q[0](B  . ?[2](3Q[0](B ) (?[2](4T[0](B . ?[2](3Q[0](B )
    (?[2](4U[0](B . ?[2](4V[0](B) (?[2](4V[0](B . ?[2](4V[0](B) (?[2](4W[0](B . ?[2](4W[0](B) (?[2](4X[0](B . ?[2](4W[0](B)
    (?[2](4Y[0](B . ?[2](3R[0](B ) (?[2](3R[0](B  . ?[2](3R[0](B ) (?[2](3S[0](B  . ?[2](3S[0](B ) (?[2](4Z[0](B . ?[2](3S[0](B )
    (?[2](3T[0](B  . ?[2](3U[0](B ) (?[2](3U[0](B  . ?[2](3U[0](B ) (?[2](3V[0](B  . ?[2](3V[0](B ) (?[2](3W[0](B  . ?[2](3V[0](B )
    (?[2](4[[0](B . ?[2](3X[0](B ) (?[2](3X[0](B  . ?[2](3X[0](B ) (?[2](3Y[0](B  . ?[2](3Y[0](B ) (?[2](4\[0](B . ?[2](3Y[0](B )
    (?[2](3Z[0](B  . ?[2](3[[0](B ) (?[2](3[[0](B  . ?[2](3[[0](B ) (?[2](3\[0](B  . ?[2](3\[0](B ) (?[2](3][0](B  . ?[2](3\[0](B )
    (?[2](4_[0](B . ?[2](3`[0](B ) (?[2](3`[0](B  . ?[2](3`[0](B ) (?[2](3a[0](B  . ?[2](3a[0](B ) (?[2](4`[0](B . ?[2](3a[0](B )
    (?[2](4e[0](B . ?[2](3f[0](B ) (?[2](3f[0](B  . ?[2](3f[0](B ) (?[2](3g[0](B  . ?[2](3g[0](B ) (?[2](4f[0](B . ?[2](3g[0](B)
    (?[2](4g[0](B . ?[2](4h[0](B) (?[2](4h[0](B . ?[2](4h[0](B) (?[2](4i[0](B . ?[2](4i[0](B) (?[2](4j[0](B . ?[2](4i[0](B)
    (?[2](4k[0](B . ?[2](4l[0](B) (?[2](4l[0](B . ?[2](4l[0](B) (?[2](4m[0](B . ?[2](4m[0](B) (?[2](4n[0](B . ?[2](4m[0](B)))

(defconst *arabic-removing-connection-from-right*
  '((?[2](3/[0](B  . ?[2](3.[0](B )
    (?[2](31[0](B  . ?[2](30[0](B )
    (?[2](33[0](B  . ?[2](32[0](B )
    (?[2](35[0](B  . ?[2](34[0](B )
    (?[2](4"[0](B . ?[2](4![0](B) (?[2](37[0](B  . ?[2](36[0](B )
    (?[2](39[0](B  . ?[2](38[0](B )
    (?[2](4$[0](B . ?[2](4#[0](B) (?[2](3;[0](B  . ?[2](3:[0](B )
    (?[2](3=[0](B  . ?[2](3<[0](B )
    (?[2](4&[0](B . ?[2](4%[0](B) (?[2](3?[0](B  . ?[2](3>[0](B )
    (?[2](4([0](B . ?[2](4'[0](B) (?[2](3A[0](B  . ?[2](3@[0](B )
    (?[2](4,[0](B . ?[2](4)[0](B) (?[2](4+[0](B . ?[2](4*[0](B)
    (?[2](40[0](B . ?[2](4-[0](B) (?[2](4/[0](B . ?[2](4.[0](B)
    (?[2](44[0](B . ?[2](41[0](B) (?[2](43[0](B . ?[2](42[0](B)
    (?[2](3C[0](B  . ?[2](3B[0](B )
    (?[2](3E[0](B  . ?[2](3D[0](B )
    (?[2](3G[0](B  . ?[2](3F[0](B )
    (?[2](3I[0](B  . ?[2](3H[0](B )
    (?[2](48[0](B . ?[2](45[0](B) (?[2](47[0](B . ?[2](46[0](B)
    (?[2](4<[0](B . ?[2](49[0](B) (?[2](4;[0](B . ?[2](4:[0](B)
    (?[2](4@[0](B . ?[2](4=[0](B) (?[2](4?[0](B . ?[2](4>[0](B)
    (?[2](4D[0](B . ?[2](4A[0](B) (?[2](4C[0](B . ?[2](4B[0](B)
    (?[2](4H[0](B . ?[2](4E[0](B) (?[2](4G[0](B . ?[2](4F[0](B)
    (?[2](4L[0](B . ?[2](4I[0](B) (?[2](4K[0](B . ?[2](4J[0](B)
    (?[2](4N[0](B . ?[2](4M[0](B) (?[2](3K[0](B  . ?[2](3J[0](B )
    (?[2](4P[0](B . ?[2](4O[0](B) (?[2](3M[0](B  . ?[2](3L[0](B )
    (?[2](4R[0](B . ?[2](4Q[0](B) (?[2](3O[0](B  . ?[2](3N[0](B )
    (?[2](4T[0](B . ?[2](4S[0](B) (?[2](3Q[0](B  . ?[2](3P[0](B )
    (?[2](4X[0](B . ?[2](4U[0](B) (?[2](4W[0](B . ?[2](4V[0](B)
    (?[2](4Z[0](B . ?[2](4Y[0](B) (?[2](3S[0](B  . ?[2](3R[0](B )
    (?[2](3W[0](B  . ?[2](3T[0](B ) (?[2](3V[0](B  . ?[2](3U[0](B )
    (?[2](4\[0](B . ?[2](4[[0](B) (?[2](3Y[0](B  . ?[2](3X[0](B )
    (?[2](3][0](B  . ?[2](3Z[0](B ) (?[2](3\[0](B  . ?[2](3[[0](B )
    (?[2](3_[0](B  . ?[2](3^[0](B )
    (?[2](4^[0](B . ?[2](4][0](B)
    (?[2](4`[0](B . ?[2](4_[0](B) (?[2](3a[0](B  . ?[2](3`[0](B )
    (?[2](4a[0](B . ?[2](3b[0](B )
    (?[2](4b[0](B . ?[2](3c[0](B )
    (?[2](4c[0](B . ?[2](3d[0](B )
    (?[2](4d[0](B . ?[2](3e[0](B )
    (?[2](4f[0](B . ?[2](4e[0](B) (?[2](3g[0](B  . ?[2](3f[0](B )
    (?[2](4j[0](B . ?[2](4g[0](B) (?[2](4i[0](B . ?[2](4h[0](B)
    (?[2](3i[0](B  . ?[2](3h[0](B)
    (?[2](4n[0](B . ?[2](4k[0](B) (?[2](4m[0](B . ?[2](4l[0](B)))

(defconst *arabic-removing-connection-from-left*
  '((?[2](36[0](B  . ?[2](4![0](B) (?[2](37[0](B  . ?[2](4"[0](B)
    (?[2](3:[0](B  . ?[2](4#[0](B) (?[2](3;[0](B  . ?[2](4$[0](B)
    (?[2](3>[0](B  . ?[2](4%[0](B) (?[2](3?[0](B  . ?[2](4&[0](B)
    (?[2](3@[0](B  . ?[2](4'[0](B) (?[2](3A[0](B  . ?[2](4([0](B)
    (?[2](4*[0](B . ?[2](4)[0](B) (?[2](4+[0](B . ?[2](4,[0](B)
    (?[2](4.[0](B . ?[2](4-[0](B) (?[2](4/[0](B . ?[2](40[0](B)
    (?[2](42[0](B . ?[2](41[0](B) (?[2](43[0](B . ?[2](44[0](B)
    (?[2](46[0](B . ?[2](45[0](B) (?[2](47[0](B . ?[2](48[0](B)
    (?[2](4:[0](B . ?[2](49[0](B) (?[2](4;[0](B . ?[2](4<[0](B)
    (?[2](4>[0](B . ?[2](4=[0](B) (?[2](4?[0](B . ?[2](4@[0](B)
    (?[2](4D[0](B . ?[2](4A[0](B) (?[2](4C[0](B . ?[2](4A[0](B)
    (?[2](4F[0](B . ?[2](4E[0](B) (?[2](4G[0](B . ?[2](4H[0](B)
    (?[2](4J[0](B . ?[2](4I[0](B) (?[2](4K[0](B . ?[2](4L[0](B)
    (?[2](3J[0](B  . ?[2](4M[0](B) (?[2](3K[0](B  . ?[2](4N[0](B)
    (?[2](3L[0](B  . ?[2](4O[0](B) (?[2](3M[0](B  . ?[2](4P[0](B)
    (?[2](3N[0](B  . ?[2](4Q[0](B) (?[2](3O[0](B  . ?[2](4R[0](B)
    (?[2](3P[0](B  . ?[2](4S[0](B) (?[2](3Q[0](B  . ?[2](4T[0](B)
    (?[2](4V[0](B . ?[2](4U[0](B) (?[2](4W[0](B . ?[2](4X[0](B)
    (?[2](3R[0](B  . ?[2](4Y[0](B) (?[2](3S[0](B  . ?[2](4Z[0](B)
    (?[2](3U[0](B  . ?[2](3T[0](B ) (?[2](3V[0](B  . ?[2](3W[0](B )
    (?[2](3X[0](B  . ?[2](4[[0](B) (?[2](3Y[0](B  . ?[2](4\[0](B)
    (?[2](3[[0](B  . ?[2](3Z[0](B ) (?[2](3\[0](B  . ?[2](3][0](B )
    (?[2](3`[0](B  . ?[2](4_[0](B) (?[2](3a[0](B  . ?[2](4`[0](B)
    (?[2](4h[0](B . ?[2](4g[0](B) (?[2](4i[0](B . ?[2](4j[0](B)
    (?[2](4l[0](B . ?[2](4k[0](B) (?[2](4m[0](B . ?[2](4n[0](B)))

(defun arabic-make-connection nil
  "If possible, tie the two characters around the cursor."
  (interactive)
  (let ((lch (assoc (visual-char-left) *arabic-adding-connection-to-right*))
	(rch (assoc (visual-char-right) *arabic-adding-connection-to-left*)))
    (if (not (and lch rch))
	(arabic-cut-connection)
      (visual-replace-left-1-char (cdr lch))
      (visual-replace-right-1-char (cdr rch)))))

(defun arabic-cut-connection nil
  "Remove the connection between the two characters around the cursor, if any."
  (interactive)
  (let
    ((lch (assoc (visual-char-left) *arabic-removing-connection-from-right*))
     (rch (assoc (visual-char-right) *arabic-removing-connection-from-left*)))
    (if lch
	(visual-replace-left-1-char (cdr lch)))
    (if rch
	(visual-replace-right-1-char (cdr rch)))))

(defun arabic-insert-char (ch arg)
  "Insert ARG (2nd arg; > 0) number of CHs (1st arg; character) around
visual point.
If CH is l2r, inserted on the left.  Otherwise, on the right."
  (while (> arg 0)
    (arabic-insert-1-char ch)
    (setq arg (1- arg))))

(defun arabic-insert-1-char (ch)
  "Insert CH (1st arg; character) around visual point.
If CH is l2r, inserted on the left.  Otherwise, on the right."
  (if (= (visual-char-direction ch) 0)
      ; if visual-char-direction = 0, always disjoint.
      (progn
	(arabic-cut-connection)
	(visual-insert-left-1-char ch))
    (visual-insert-left-1-char ch)
    (arabic-make-connection)
    (visual-move-to-left-1-char)
    (arabic-make-connection)))

(defun arabic-self-insert-command (arg)
  "Self-insert-command for arabic-mode."
  (interactive "*p")
  (let ((ch last-command-char))
    (if arabic-input-arabic-char
	(setq ch (aref arabic-translate-table (- ch 32))))
    (if (null ch)
	(beep)
      (while (> arg 0)
	(arabic-keyboard-insert-1-char ch)
	(setq arg (1- arg))))))

(defun arabic-keyboard-insert-1-char (ch)
  "Insert CH (1st arg; Arabic character) at visual cursor position.
if last-command is arabic-cut-connection, CH will not connected to the
right adjacent character (but connected to the left, if possible)."
  (let ((rch (visual-char-right)))
    (cond
     ((= (visual-char-direction ch) 0)
      (arabic-cut-connection)
      (visual-insert-left-1-char ch))
     ((eq last-command 'arabic-cut-connection)
      (visual-insert-right-1-char ch)
      (arabic-make-connection))
     (t
      (arabic-insert-1-char ch)))))

(defun arabic-insert-gaaf (arg)
  "Insert gaaf as if it were typed from keyboard."
  (interactive "*p")
  (while (> arg 0)
    (arabic-keyboard-insert-1-char ?[2](4k[0](B)
    (setq arg (1- arg))))

(defun arabic-insert-isolated-hamza (arg)
  "Insert an isolated hamza as if it were typed from keyboard."
  (interactive "*p")
  (while (> arg 0)
    (arabic-keyboard-insert-1-char ?[2](3-[0](B)
    (setq arg (1- arg))))

(defun arabic-insert-madda nil
  "Put madda on the previous alif."
  (interactive)
  (let ((rch (visual-char-right)))
    (cond
     ((eq rch ?[2](38[0](B ) (visual-replace-right-1-char ?[2](3.[0](B ))
     ((eq rch ?[2](39[0](B ) (visual-replace-right-1-char ?[2](3/[0](B ))
     ((eq rch ?[2](3e[0](B ) (visual-replace-right-1-char ?[2](3b[0](B ))
     ((eq rch ?[2](4d[0](B) (visual-replace-right-1-char ?[2](4a[0](B))
     (t (beep)))))

(defun arabic-insert-alif (arg)
  "Insert ARG number of alif's.
If the previous character is a laam, replace it with an alif+laam ligature."
  (interactive "*p")
  (let (rch)
    (while (> arg 0)
      (setq rch (visual-char-right))
      (cond
       ((eq last-command 'arabic-cut-connection)
	(visual-insert-right-1-char ?[2](38[0](B))
       ((or (eq rch ?[2](4Y[0](B) (eq rch ?[2](3R[0](B ))
	(visual-replace-right-1-char ?[2](3e[0](B ))
       ((or (eq rch ?[2](3S[0](B ) (eq rch ?[2](4Z[0](B))
	(visual-replace-right-1-char ?[2](4d[0](B))
       (t
	(visual-insert-left-1-char ?[2](38[0](B )
	(arabic-make-connection)
	(visual-move-to-left-1-char)))
      (setq arg (1- arg)))
    (arabic-cut-connection)))

(defun arabic-insert-hamza (arg)
  "Insert ARG number of hamza's.
Put it on/under previous characters, if possible."
  (interactive "*p")
  (let (rch)
    (while (> arg 0)
      (setq rch (visual-char-right))
      (cond
       ((eq last-command 'arabic-cut-connection)
	(visual-insert-right-1-char ?[2](3-[0](B))
       ((eq rch ?[2](38[0](B ) (visual-replace-right-1-char ?[2](30[0](B ))
       ((eq rch ?[2](39[0](B ) (visual-replace-right-1-char ?[2](31[0](B ))
       ((eq rch ?[2](30[0](B ) (visual-replace-right-1-char ?[2](34[0](B ))
       ((eq rch ?[2](31[0](B ) (visual-replace-right-1-char ?[2](35[0](B ))
       ((eq rch ?[2](3^[0](B ) (visual-replace-right-1-char ?[2](32[0](B ))
       ((eq rch ?[2](3_[0](B ) (visual-replace-right-1-char ?[2](33[0](B ))
       ((eq rch ?[2](4_[0](B) (visual-replace-right-1-char ?[2](4![0](B))
       ((eq rch ?[2](3`[0](B ) (visual-replace-right-1-char ?[2](36[0](B ))
       ((eq rch ?[2](3a[0](B ) (visual-replace-right-1-char ?[2](37[0](B ))
       ((eq rch ?[2](4`[0](B) (visual-replace-right-1-char ?[2](4"[0](B))
       ((eq rch ?[2](4][0](B) (visual-replace-right-1-char ?[2](4![0](B))
       ((eq rch ?[2](4^[0](B) (visual-replace-right-1-char ?[2](4"[0](B))
       ((eq rch ?[2](3e[0](B ) (visual-replace-right-1-char ?[2](3c[0](B ))
       ((eq rch ?[2](4d[0](B) (visual-replace-right-1-char ?[2](4b[0](B))
       ((eq rch ?[2](3c[0](B ) (visual-replace-right-1-char ?[2](3d[0](B ))
       ((eq rch ?[2](4b[0](B) (visual-replace-right-1-char ?[2](4c[0](B))
       (t (arabic-cut-connection)
	  (visual-insert-right-1-char ?[2](3-[0](B)))
      (setq arg (1- arg)))))

(defun arabic-toggle-input-char nil
  "Toggle Arabic key input and ASCII key input."
  (interactive)
  (if arabic-input-arabic-char
      (setq arabic-input-arabic-char nil
	    arabic-mode-indicator " Arabic")
    (setq arabic-input-arabic-char t
	  arabic-mode-indicator " [2](3=a:GJ[0](B"))
  (redraw-modeline t))

(defun arabic-newline (arg)
  "Newline for arabic-mode."
  (interactive "*p")
  (arabic-insert-char ?\n arg))

(defun arabic-open-line (arg)
  "Openline for arabic-mode."
  (interactive "*p")
  (arabic-insert-char ?\n arg)
  (visual-backward-char arg))

(defun arabic-delete-char (arg)
  "Delete ARG (1st arg; integer) chars visually after visual point.
After that, Arabic ligature is performed."
  (interactive "*p")
  (visual-delete-char arg)
  (arabic-make-connection))

(defun arabic-backward-delete-char (arg)
  "Delete ARG (1st arg; integer) chars visually before visual point.
After that, Arabic ligature is performed."
  (interactive "*p")
  (visual-backward-delete-char arg)
  (arabic-make-connection))

(defun arabic-kill-region (beg end)
  "Kill-region command for arabic-mode."
  (interactive "r")
  (if (or (and buffer-read-only (not inhibit-read-only))
	  (text-property-not-all beg end 'read-only nil))
      (visual-kill-region beg end)
    (visual-kill-region beg end)
    (arabic-make-connection)))

(defun arabic-kill-word (arg)
  "Kill-word command for arabic-mode."
  (interactive "*p")
  (visual-kill-word arg)
  (arabic-make-connection))

(defun arabic-backward-kill-word (arg)
  "Backword-ill-word command for arabic-mode."
  (interactive "*p")
  (visual-backward-kill-word arg)
  (arabic-make-connection))

(defun arabic-kill-line (&optional arg)
  "Kill-line command for arabic-mode."
  (interactive "*P")
  (visual-kill-line arg)
  (arabic-make-connection))

(defun arabic-yank (&optional arg)
  "yank command for arabic-mode."
  (interactive "*P")
  (visual-yank arg)
  (let ((p1 (point)) (p2 (mark t)))
    (arabic-make-connection)
    (goto-char p2)
    (arabic-make-connection)
    (goto-char p1)
    (set-marker (mark-marker) p2 (current-buffer))
    nil))

(defun arabic-yank-pop (arg)
  "yank-pop command for arabic-mode."
  (interactive "*p")
  (visual-yank-pop arg)
  (let ((p1 (point)) (p2 (mark t)))
    (arabic-make-connection)
    (goto-char p2)
    (arabic-make-connection)
    (goto-char p1)
    (set-marker (mark-marker) p2 (current-buffer))
    nil))

(defun arabic-help nil
  "Display keymap in Arabic-mode."
  (interactive)
  (let ((arabic-help-buffer (get-buffer-create "*Help*")))
    (set-buffer arabic-help-buffer)
    (erase-buffer)
    (insert arabic-help-string)
    (goto-char (point-min))
    (display-buffer (current-buffer))))


;; arabic LR commands

(defun arabic-delete-left-char (arg)
  "Kill N (1st arg; integer) characters on the left of visual point."
  (interactive "*p")
  (if display-direction
      (arabic-delete-char arg)
    (arabic-backward-delete-char arg)))

(defun arabic-delete-right-char (arg)
  "Kill N (1st arg; integer) characters on the right of visual point."
  (interactive "*p")
  (if display-direction
      (arabic-backward-delete-char arg)
    (arabic-delete-char arg)))

(defun arabic-kill-left-word (arg)
  "Kill N (1st arg; integer) words on the left of visual point."
  (interactive "*p")
  (if display-direction
      (arabic-kill-word arg)
    (arabic-backward-kill-word arg)))

(defun arabic-kill-right-word (arg)
  "Kill N (1st arg; integer) words on the right of visual point."
  (interactive "*p")
  (if display-direction
      (arabic-backward-kill-word arg)
    (arabic-kill-word arg)))

;;;
(provide 'arabic)
