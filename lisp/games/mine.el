;;; mine.el --- Mine game for GNU Emacs

;; Author:     Jacques Duthen <duthen@cegelec-red.fr>
;; Keywords:   games
;; Time-stamp: <97/01/20 14:37:36 duthen>
;; Version:    1.17

(defconst mine-version-number "1.17" "Emacs Mine version number.")
(defconst mine-version (format "XEmacs Mine v%sx by Jacques Duthen © 1997"
			       mine-version-number)
  "Full Emacs Mine version number.")

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The object of this classical game is to locate the hidden mines.
;; To do this, you hit the squares on the game board that do not
;; contain mines, and you mark the squares that do contain mines.

;; The number of hidden mines remaining in the mine field is indicated
;; inside the buffer.  Every time you mark a square as a mine, this
;; number decreases by one, even if you incorrectly mark a square.

;; To hit a square: Point to the square, and click the left button.
;; If the square is a mine, you lose.
;; If the square isn't a mine, a number appears, which represents
;; the number of mines in the surrounding eight squares.

;; To mark a square as a mine: Point to the square, and click
;; the right button.

;; To play Mine, compile it if you want, load it, and type `M-x mine'.

;; To get help and doc, see the functions `mine' and `mine-help'
;; (ie. type `?' in the *Mine* buffer or type `C-h f mine')

;; This module has been developed and tested with GNU Emacs 19.31.1,
;; but it should run with any GNU Emacs 19.* (at least with versions
;; superior to 19.31). 

;; This module has not been tested (yet) with XEmacs.  It may or may
;; not run (can anybody tell me?).  

;; Send any comment or bug report (do you expect to find any? ;-) to me:
;; duthen@cegelec-red.fr (Jacques Duthen)

;; Good luck.

;; 1.17 Thanks to Vladimir Alexiev <vladimir@cs.ualberta.ca>.
;; Fix bug: (void-function unless), add minimal support for xemacs.
;; (mine-xemacs-p): Added.
;; (event-point): New function.
;; (mine-mouse-hit, mine-mouse-mark): Use (interactive "@e") and `event-point'
;; (mine-init-mode-map): Support xemacs mouse binding.  
;; (mine-make-face): Support xemacs get-face.  
;; (mine-goto): Support `auto-show-make-point-visible' as well as
;; `hscroll-point-visible'. 

;; 1.16 Initial released version.

;;; Code:

(defvar mine-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))

;;; ================================================================
;;; User Variables:

;;; -1- size

;;; The mine field is a rectangle (mine-xmax x mine-ymax), which is
;;; duplicated to fill a bigger rectangle periodically tiled with the
;;; smaller one, the period being (mine-xmax x mine-ymax).  

(defvar mine-xmax 16 "*The logical width  of the mine field.")
(defvar mine-ymax 24 "*The logical height of the mine field.")

(defvar mine-mines-%  16
  "*Percentage (between 0 and 100) of mines in the mine field.")

(defvar mine-torus (not mine-xemacs-p)
  "*Non-nil to play the game on a periodic board (a torus).
This is the default unless using graphics (XEmacs)")

(defvar mine-nb-tiles-x 2
  "*Number of duplications in the x direction, when `mine-torus' is non-nil.
Indicate the number of times the original mine field is duplicated 
in the x direction.
It's better looking when it's an integer.  
nil means fill exactly the whole window.
0 means fill the whole window with the biggest integer that fits.
a negative number means use exactly the opposite number.  If it's
too big, the rows are truncated by emacs.  Automatic horizontal
scrolling will occur if you move to an invisible point.
a positive float means limit to the window width if needed.
a positive integer means limit to the window width if needed,
with the biggest possible integer value anyway.
")

(defvar mine-nb-tiles-y 2
  "*Number of duplications in the y direction, when `mine-torus' is non-nil.
Indicate the number of times the original mine field is duplicated 
in the y direction.
It's better looking when it's an integer.  
nil means fill exactly the whole window.
0 means fill the whole window with the biggest integer that fits.
a negative number means use exactly the opposite number.  If it's
too big, the rows will be simply scrolled up or down by emacs.
a positive float means limit to the window height if needed.
a positive integer means limit to the window height if needed,
with the biggest possible integer value anyway.
")

;;; -2- square characters

;;; All these characters may be changed but the first three ones
;;; `unmarked' `marked' `zero' must differ from each other.

(defvar mine-char-unmarked  ?-
  "*Character for a square not yet marked nor hit.")
(defvar mine-char-marked    ?@
  "*Character for a square marked as containing a mine.")
(defvar mine-char-zero      ?\ 
  "*Character for a square hit with no adjacent mine.")

(defvar mine-char-pad       ?\ 
  "*Character to pad in the x direction or nil (not yet implemented).")
(defvar mine-char-not-found ?o
  "*Character for a square marked but with no mine.")
(defvar mine-char-bogus     ?x
  "*Character for a square not marked but with a mine.")

;;; -3- colors

(defvar mine-colorp (if window-system 't 'nil)
  "*Non-nil means with colors.  Nil means in black and white.")

(defvar mine-colors nil
  "*Set this variable to override the colors defined by 
`mine-default-colors' (use the same format).")

(defconst mine-default-colors
  '((mine-face-unmarked  . "LightBlue")
    (mine-face-marked    . "Red")
    (0 . nil)
    (1 . "Cyan")
    (2 . "Green")
    (3 . "Yellow")
    (4 . "Orange")
    (5 . "OrangeRed")
    (6 . "Red")
    (7 . "Red")
    (8 . "Red")
    (mine-face-pad       . nil)
    (mine-face-not-found . "Red")
    (mine-face-bogus     . "Red")
    )
  "A-list of default colors for Mine faces.  Don't change its value.
You can override these settings with `mine-colors' using the same format.")

;;; -4- redisplay

(defvar mine-level 2
  "*Redisplay speed.  0 is the slowest redisplay, 5 is the fastest one.
0 means redisplay when every single square changes.  
1 means redisplay when one square and its periodic images change.  
2 means redisplay every `mine-count1-max' change.  
3 means redisplay every `mine-count1-max'*`mine-count2-max' change.  
-1 or nil means redisplay only when all the changes are done.
")

(defvar mine-count1-max 16
  "*See `mine-level'.  
Redisplay when the number of empty squares which have changed
is greater than `mine-count1-max'.
8 means redisplay each time 8 squares have been changed.
-1 means redisplay only when all the changes are done.")

(defvar mine-count2-max  4
  "*See `mine-level'.  
Redisplay when the number of empty squares which have changed
is greater than `mine-count1-max'.
8 means redisplay each time 8 squares have been changed.
-1 means redisplay only when all the changes are done.")

(defvar mine-hscroll-step 4
  "*Local value for `hscroll-step'")

(defvar mine-mode-hook nil
  "*Hook called by `mine-mode-hook'.")

;;; ================================================================
;;; Internal variables:

;; XEmacs stuffs
(defvar mine-glyph-directory (concat data-directory "mine")
  "Directory where mine glyphs are kept.")
(defun mine-make-glyph (file)
  (when mine-xemacs-p
    (make-glyph (list (cons 'x
			    (expand-file-name file mine-glyph-directory))))))
(defvar mine-default-glyphs
  `((mine-face-unmarked . ,(mine-make-glyph "empty_16_up.gif"))
    (mine-face-marked . ,(mine-make-glyph "flagged_16_up.gif"))
    (0 . ,(mine-make-glyph "empty_16_flat.gif"))
    (1 . ,(mine-make-glyph "1_16_flat.gif"))
    (2 . ,(mine-make-glyph "2_16_flat.gif"))
    (3 . ,(mine-make-glyph "3_16_flat.gif"))
    (4 . ,(mine-make-glyph "4_16_flat.gif"))
    (5 . ,(mine-make-glyph "5_16_flat.gif"))
    (6 . ,(mine-make-glyph "6_16_flat.gif"))
    (7 . ,(mine-make-glyph "7_16_flat.gif"))
    (8 . ,(mine-make-glyph "8_16_flat.gif"))
    (mine-face-pad . ,(mine-make-glyph "empty_16_down.gif"))
    (mine-face-not-found . ,(mine-make-glyph "bomb_16_flat.gif"))
    (mine-face-bogus . ,(mine-make-glyph "question_16_up.gif"))
    )
  "A-list of default graphics for various mine characters.  Unless you
have an entire replacement set of graphics I wouldn't suggest changing it.")

(defvar mine-user-variables
  '("Size"
    mine-xmax mine-ymax mine-mines-%
    mine-torus mine-nb-tiles-x mine-nb-tiles-y
    "Square characters"
    mine-char-unmarked mine-char-marked mine-char-zero
    mine-char-pad mine-char-not-found mine-char-bogus
    "Colors"
    mine-colorp mine-colors
    "Redisplay"
    mine-level mine-count1-max mine-count2-max
    "Scrolling"
    mine-hscroll-step
    "Hook"
    mine-mode-hook))

(defvar mine-user-commands
  '("Help"
    mine mine-help mine-help-bindings mine-help-variables
    "Mouse control"
    mine-mouse-hit mine-mouse-mark
    "Move"
    mine-left mine-right mine-up mine-down
    mine-bol mine-eol mine-top mine-bottom
    "Hit and mark"
    mine-hit-curpoint mine-mark-curpoint
    "Quit"
    mine-quit))

;; pad x factor == (if mine-char-pad 2 1)
(defvar mine-padx*)

(defvar mine-width)
(defvar mine-height)

;; (x y) of current point
(defvar mine-x) ;; 1 <= mine-x <= mine-width
(defvar mine-y) ;; 1 <= mine-y <= mine-height

;; limits of the playable part of the board
(defvar mine-point-min)
(defvar mine-point-max)

(defvar mine-point-remaining-mines)
(defvar mine-point-mines-hit)

(defvar mine-mode-map nil)

(defvar mine-real-mines)

(defvar mine-nb-remaining-mines)
(defvar mine-nb-remaining-marks)
(defvar mine-nb-mines-hit)

(defvar mine-faces)

;;; This variable is more special rather than global.
(defvar mine-adjacent-points)

(defvar mine-count1)
(defvar mine-count2)

;;; ================================================================
;;; Macros (stolen from "cl.el" (soon in "subr.el" (thanks to rms)))

(eval-when-compile
(or (fboundp 'when)
(defmacro when (cond &rest body)
  "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))))

;;; ================================================================
;;; User commands

;;;###autoload
(defun mine (num)
  "Play Mine.  Optional prefix argument is the number of mines.

To play Mine, type `\\[mine]' or `\\[universal-argument] NUM \\[mine]'.  

An optional prefix argument specifies the number of mines to be hidden
in the field.  If no prefix argument is given, a percentage
`mine-mines-%' of the field will contain mines.

What is Mine?\\<mine-mode-map>

Mine is a classical game of hide and seek played on a rectangular grid
containing `mine-xmax' by `mine-ymax' squares (the mine field).

Your opponent (Emacs, in this case) has hidden several mines within
this field.  The object of the game is to find every hidden mine.

When you're sure a square does NOT contain a mine, you can hit it:
move the mouse over the square and press `\\[mine-mouse-hit]' or 
move the cursor with the usual keys and press `\\[mine-hit-curpoint]'.

If the square is a mine, you lose.
If the square isn't a mine, a number appears which represents
the number of mines in the surrounding eight squares.  

When you think a square DOES contain a mine, you can mark it:
move the mouse over the square and press `\\[mine-mouse-mark]' or
move the cursor with the usual keys and press `\\[mine-mark-curpoint]'.

The number of hidden mines remaining in the mine field is indicated
inside the buffer.  Every time you mark a square as a mine, this
number decreases by one, even if you incorrectly mark a square.

If `mine-torus' is non-nil (the default), the Mine game is played over
a periodic field (like a torus).  Each mine is hidden periodically
over the mine board `mine-nb-tiles-x' times in the x direction and
`mine-nb-tiles-y' times in the y direction.

If `mine-colorp' is non-nil (the default, if the system allows it),
the game is displayed with colors.  The colors can be chosen with the
variable `mine-colors'.

If the redisplay is not fast enough, increase `mine-level'.  If you
want to see a smoother (slower) redisplay, decrease `mine-level',
`mine-count1-max' and `mine-count2-max'.

You can get help on `mine-mode' and its key bindings by pressing `\\[mine-help]'
while in the *Mine* buffer.
"
  (interactive "P")
  (switch-to-buffer "*Mine*")
  (mine-mode)
  (setq buffer-read-only 't)
  (buffer-disable-undo (current-buffer))
  (setq mine-nb-remaining-mines
	(or num (round (/ (* mine-xmax mine-ymax mine-mines-%) 100)))
        mine-nb-remaining-marks mine-nb-remaining-mines)
  (if (> mine-nb-remaining-mines (* mine-xmax mine-ymax))
    (error "Too many mines: %d" mine-nb-remaining-mines))
  (mine-init-faces)
  (setq mine-real-mines (mine-init-mines mine-nb-remaining-mines))
  (setq mine-nb-mines-hit 0)
  (mine-init-board)
  (mine-reset-counters)
  (mine-update-remaining-mines)
  (setq hscroll-step mine-hscroll-step)
  ;; initial position
  (setq mine-x 1)
  (setq mine-y 1)
  (mine-goto mine-x mine-y)
)

;; Mine mode is suitable only for specially formatted data.
(put 'mine-mode 'mode-class 'special)

(defun mine-mode ()
  "Major mode for playing Mine.  To learn how to play Mine, see `mine'.

If you have a mouse, you can do:\\<mine-mode-map>

`\\[mine-mouse-hit]' -- hit point
`\\[mine-mouse-mark]' -- mark or unmark a mine at point

If you don't have a mouse, you can move the cursor over the mine
field with the usual mnemonic keys and:

`\\[mine-hit-curpoint]' -- hit point
`\\[mine-mark-curpoint]' -- mark or unmark a mine at point

`\\[mine-quit]' -- give up and see the hidden mines

You can get help with:

`\\[mine-help-variables]' -- get help on Mine variables
`\\[mine-help-bindings]' -- get help on Mine bindings

\\{mine-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'hscroll-step)
  (use-local-map mine-mode-map)
  (setq truncate-lines 't)
  (setq major-mode 'mine-mode)
  (setq mode-name "Mine")
  (run-hooks 'mine-mode-hook)
)

;;;###autoload
(defun mine-version ()
  "Return string describing the current version of Mine.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message (mine-version))
    mine-version))

;;;###autoload
(defun mine-help ()
  "*Get help on `mine-mode'."
  (interactive)
  (save-excursion
   (switch-to-buffer "*Mine*")
   (mine-mode)
   (describe-mode)))

(defun mine-help-variables ()
  "*Get help on Mine variables."
  (interactive)
  (save-excursion
   (switch-to-buffer "*Mine*")
   (mine-mode)
   (apropos-symbols mine-user-variables 't)))

(defun mine-help-bindings ()
  "*Get help on Mine bindings."
  (interactive)
  (save-excursion
   (switch-to-buffer "*Mine*")
   (mine-mode)
   (apropos-symbols mine-user-commands 't)))

(defun mine-print-settings ()
  "*Print the current Mine settings (value of all the user variables)."
  (interactive)
  (with-output-to-temp-buffer "*scratch*"
    (mine-print-variables mine-user-variables)))

;;; ================================================================
;;; Click events - nop hit mark

;;; [jack] The elisp manual says:
;;; If you want to take action as soon as a button is pressed,
;;; you need to handle "button-down" events.
;;; The global map (cf. `mouse.el') has, by default, the binding:
;;;	(define-key global-map [down-mouse-1] 'mouse-drag-region)
;;; It seems that this function "eats" the final event [mouse-1].
;;; So, we need a local binding for [down-mouse-1] which shadows
;;; the global one and prevents `mouse-drag-region' from being called.
;;; Hence, in `mine-init-mode-map' I use the following binding:
;;;	(define-key mine-mode-map [down-mouse-1] 'mine-mouse-nop)
;;; I found a better binding in "apropos.el"
;;;	(define-key mine-mode-map [down-mouse-1] nil)
;;; but, as it does not work, let's go back to nop...

(or (fboundp 'event-point)
    (defun event-point (event)
      (posn-point (event-end event))))

(defun mine-mouse-nop (event)
  "Nop"
  (interactive "e"))

(defun mine-mouse-hit (event)
  "Move point to the position clicked on with the mouse and hit this point."
  (interactive "@e")
  (if (mine-goto-point (event-closest-point event))
      (mine-hit-curpoint)
    (mine-message 'mine-msg-click-precisely)))

(defun mine-mouse-mark (event)
  "Move point to the position clicked on with the mouse and mark or unmark
this point."
  (interactive "@e")
  (if (mine-goto-point (event-point event))
      (mine-mark-curpoint)
    (mine-message 'mine-msg-click-precisely)))

;;; ================================================================
;;; Key events - hit mark quit

(defun mine-hit-curpoint ()
  "Hit point"
  (interactive)
  (mine-reset-counters)
  (let ((c (following-char)))
    (save-excursion
      (cond
	((eq c mine-char-marked)
         (mine-message 'mine-msg-unmark-before-hit))
	((not (eq c mine-char-unmarked))
         (mine-message 'mine-msg-point-already-hit))
	((mine-mine-at-point-p (point) 'slowp)
	 (setq mine-nb-mines-hit (1+ mine-nb-mines-hit))
	 (mine-update-mines-hit)
         (mine-message 'mine-msg-lose)
	 (mine-quit))
	(t ;; the real job...
	 (let* ((x.y (mine-top-left (mine-point-to-x.y (point))))
		(pxy (cons (point) x.y))
		(mine-adjacent-points (list pxy))) ; special variable
	   (while mine-adjacent-points
	     (setq pxy (car mine-adjacent-points)
		   mine-adjacent-points (cdr mine-adjacent-points))
	     (mine-deep-hit pxy))))))))

(defun mine-mark-curpoint ()
  "Mark or unmark current position"
  (interactive)
  (mine-reset-counters)
  (let ((c (following-char)))
    (save-excursion
      (cond
	((eq c mine-char-unmarked)
	 (mine-mark-board (point))
	 (setq mine-nb-remaining-marks
	       (1- mine-nb-remaining-marks))
	 (if (mine-mine-at-point-p (point) 'slowp)
	     (setq mine-nb-remaining-mines
		   (1- mine-nb-remaining-mines))))
	((eq c mine-char-marked)
	 (mine-unmark-board (point))
	 (setq mine-nb-remaining-marks
	       (1+ mine-nb-remaining-marks))
	 (if (mine-mine-at-point-p (point) 'slowp)
	     (setq mine-nb-remaining-mines
		   (1+ mine-nb-remaining-mines))))
	(t
         (mine-message 'mine-msg-cannot-mark)))
      (mine-update-remaining-mines))))

(defun mine-quit ()
  "*Display hidden and bogus mines."
  (interactive)
  (when (y-or-n-p "Do you want to see the remaining and bogus mines? ")
    (mine-show-bogus-mines)))

(defun mine-show-bogus-mines ()
  (mine-reset-counters)
  (let ((nrb 0) (nbb 0)
        (x.y (cons nil nil))
        (y 1) x
        point c)
    (while (<= y mine-ymax)
      (setq x 1)
      (setcdr x.y y)
      (while (<= x mine-xmax)
        (setq point (mine-xy-to-point x y)
              c (char-after point))
        (cond
         ((eq c mine-char-unmarked)
          (setcar x.y x)
          (when (mine-mine-at-xy-p x.y)
            (setq nrb (1+ nrb))
            (mine-update-board point mine-char-not-found 'mine-face-not-found)))
         ((eq c mine-char-marked)
          (setcar x.y x)
          (when (not (mine-mine-at-xy-p x.y))
            (setq nbb (1+ nbb))
            (mine-update-board point mine-char-bogus 'mine-face-bogus))))
        (setq x (1+ x)))
      (setq y (1+ y)))
    (mine-update-bogus-mines nrb nbb)))

;;; ================================================================
;;; Key events - moves

(defun mine-left ()
  "Move left"
  (interactive)
  (setq mine-x (1- mine-x))
  (when (<= mine-x 0)
    (while (<= mine-x mine-width)
      (setq mine-x (+ mine-x mine-xmax)))
    (setq mine-x (- mine-x mine-xmax)))
  (mine-goto mine-x mine-y))

(defun mine-right ()
  "Move right"
  (interactive)
  (setq mine-x (1+ mine-x))
  (when (> mine-x mine-width)
    (while (>= mine-x 0)
      (setq mine-x (- mine-x mine-xmax)))
    (setq mine-x (+ mine-x mine-xmax)))
  (mine-goto mine-x mine-y))

(defun mine-up ()
  "Move up"
  (interactive)
  (setq mine-y (1- mine-y))
  (when (<= mine-y 0)
    (while (<= mine-y mine-height)
      (setq mine-y (+ mine-y mine-ymax)))
    (setq mine-y (- mine-y mine-ymax)))
  (mine-goto mine-x mine-y))

(defun mine-down ()
  "Move down"
  (interactive)
  (setq mine-y (1+ mine-y))
  (when (> mine-y mine-height)
    (while (>= mine-y 0)
      (setq mine-y (- mine-y mine-ymax)))
    (setq mine-y (+ mine-y mine-ymax)))
  (mine-goto mine-x mine-y))


(defun mine-bol ()
  "Move to the beginning of the row"
  (interactive)
  (setq mine-x 1)
  (mine-goto mine-x mine-y))

(defun mine-eol ()
  "Move to the end of the row"
  (interactive)
  (setq mine-x mine-width)
  (mine-goto mine-x mine-y))

(defun mine-top ()
  "Move to the top of the column"
  (interactive)
  (setq mine-y 1)
  (mine-goto mine-x mine-y))

(defun mine-bottom ()
  "Move to the bottom of the column"
  (interactive)
  (setq mine-y mine-height)
  (mine-goto mine-x mine-y))

;;; ================================================================
;;; Internal model functions

(defun mine-init-mines (num-mines)
  (random t)
  (let ((mines (list)) (n num-mines) x y x.y)
    (while (> n 0)
      (setq n (1- n)
            x (1+ (random mine-xmax))
            y (1+ (random mine-ymax))
            x.y (cons x y))
      (while (mine-member x.y mines 'nil)
	;; replace by the point to the right (or next row if eol)
        (if (< x mine-xmax)
	    (setcar x.y (setq x (1+ x)))
          (setcar x.y (setq x 1))
          (setcdr x.y (setq y (if (< y mine-ymax) (1+ y) 1)))))
      (setq mines (cons x.y mines)))
    mines))

(defun mine-mine-at-point-p (point slowp)
  (mine-member (mine-top-left (mine-point-to-x.y point))
               mine-real-mines slowp))

(defun mine-mine-at-xy-p (x.y)
  (mine-member x.y mine-real-mines 'nil))

;;; Returns non-nil if ELT is an element of LIST. 
;;; Constant time execution if slowp is non-nil.
(defun mine-member (x.y list slowp)
  (let ((found 'nil))
    (while (and list (or slowp (not found)))
      (if (equal x.y (car list))
        (setq found 't))
      (setq list (cdr list)))
    found))

;;; ================================================================
;;; Internal model & interface functions

(defun mine-pxy (x y)
  (cons (mine-xy-to-point x y) (cons x y)))

;; pxy == (point . (x . y))
;; with 1 <= {xy} <= mine-{xy}max
(defun mine-deep-hit (pxy)
  (interactive)
  (let (point x.y c)
    (setq point (car pxy)
	  x.y   (cdr pxy)
          c     (char-after point))
    (cond
     ((eq c mine-char-marked)) ;; free but marked (user bug)
     ((not (eq c mine-char-unmarked))) ;; already done
     ((mine-mine-at-xy-p x.y)
      (error "Internal error: mine-deep-hit mine at %s" point))
     (t ;; the real job...
      (let* ((adjacent-points (mine-adjacent-points point x.y))
             (nb-adjacent-mines (mine-nb-adjacent-mines adjacent-points)))
        (mine-display-nb-adjacent-mines point nb-adjacent-mines)
        (when (zerop nb-adjacent-mines)
	  ;; Stack overflow: "Lisp nesting exceeds max-lisp-eval-depth"
          ;;(mapc 'mine-deep-hit adjacent-points)
	  (setq mine-adjacent-points
		(nconc adjacent-points mine-adjacent-points))))))))

;; return == ((point . (x . y))*)
;; with 1 <= {xy} <= mine-{xy}max
(defun mine-adjacent-points (point x.y)
  (mine-random-permut
   (if mine-torus
     (mine-adjacent-points-on-torus point x.y)
     (mine-adjacent-points-no-torus point x.y))))

(defun mine-random-permut (l)
  (let ((ll (nthcdr (random (length l)) l)))
    (nconc ll l)
    (prog1 (cdr ll) (setcdr ll ()))))

(defun mine-adjacent-points-no-torus (point x.y)
  (let ((x (car x.y)) (y (cdr x.y)) (points (list)) xx yy)
    ;; left column
    (when (not (= x 1))
      (setq xx (1- x))
      (when (not (= y 1))
        (setq yy (1- y))
        (setq points (cons (mine-pxy xx yy) points)))
      (setq points (cons (mine-pxy xx y) points))
      (when (not (= y mine-ymax))
        (setq yy (1+ y))
        (setq points (cons (mine-pxy xx yy) points))))
    ;; middle column
    (setq xx x)
    (when (not (= y 1))
      (setq yy (1- y))
      (setq points (cons (mine-pxy xx yy) points)))
    (when (not (= y mine-ymax))
      (setq yy (1+ y))
      (setq points (cons (mine-pxy xx yy) points)))
    ;; right column
    (when (not (= x mine-xmax))
      (setq xx (1+ x))
      (when (not (= y 1))
        (setq yy (1- y))
        (setq points (cons (mine-pxy xx yy) points)))
      (setq points (cons (mine-pxy xx y) points))
      (when (not (= y mine-ymax))
        (setq yy (1+ y))
        (setq points (cons (mine-pxy xx yy) points))))
    (nreverse points)))

(defun mine-adjacent-points-on-torus (point x.y)
  (let ((x (car x.y)) (y (cdr x.y)) (points (list)) xx yy)
    ;; left column
    (setq xx (if (= x 1) mine-xmax (1- x)))
    (setq yy (if (= y 1) mine-ymax (1- y)))
    (setq points (cons (mine-pxy xx yy) points))
    (setq points (cons (mine-pxy xx y) points))
    (setq yy (if (= y mine-ymax) 1 (1+ y)))
    (setq points (cons (mine-pxy xx yy) points))
    ;; middle column
    (setq xx x)
    (setq yy (if (= y 1) mine-ymax (1- y)))
    (setq points (cons (mine-pxy xx yy) points))
    (setq yy (if (= y mine-ymax) 1 (1+ y)))
    (setq points (cons (mine-pxy xx yy) points))
    ;; right column
    (setq xx (if (= x mine-xmax) 1 (1+ x)))
    (setq yy (if (= y 1) mine-ymax (1- y)))
    (setq points (cons (mine-pxy xx yy) points))
    (setq points (cons (mine-pxy xx y) points))
    (setq yy (if (= y mine-ymax) 1 (1+ y)))
    (setq points (cons (mine-pxy xx yy) points))
    (nreverse points)))

;; l == ((p . (x . y))*)
(defun mine-nb-adjacent-mines (l)
  (let ((nb 0) pxy x.y)
    (while l
      (setq pxy (car l)  l (cdr l)  x.y (cdr pxy))
      (if (mine-mine-at-xy-p x.y)
          (setq nb (1+ nb))))
    nb))

;;; ================================================================
;;; Mode map

(defun mine-init-mode-map ()
  (let ((map (make-keymap)) (gm global-map))
    ;; All normally self-inserting keys (except digits) are undefined
    (suppress-keymap map 'nil)
    ;; Help
    (define-key map "?"      'mine-help)
    (define-key map "h"      'mine-help)
    (define-key map "b"      'mine-help-bindings)
    (define-key map "v"      'mine-help-variables)
    (cond
      (mine-xemacs-p
       ;; Mouse control
       (define-key map [mouse-1] 'mine-mouse-hit)
       (define-key map [mouse-3] 'mine-mouse-mark)
       ;; Mouse control to prevent problems
       (define-key map [mouse-2] 'mine-mouse-nop))
      (t
       ;; Mouse control
       (define-key map [mouse-1] 'mine-mouse-hit)
       (define-key map [mouse-3] 'mine-mouse-mark)
       ;; Mouse control to prevent problems
       (define-key map [mouse-2] 'mine-mouse-nop)
       (define-key map [down-mouse-1] 'mine-mouse-nop)
       (define-key map [down-mouse-2] 'mine-mouse-nop)
       (define-key map [down-mouse-3] 'mine-mouse-nop)
       (define-key map [drag-mouse-1] 'mine-mouse-nop)
       (define-key map [drag-mouse-2] 'mine-mouse-nop)
       (define-key map [drag-mouse-3] 'mine-mouse-nop)
       (define-key map [mouse-2] 'mine-mouse-nop)))
    ;; Move
    (substitute-key-definition 'backward-char 'mine-left  map gm)
    (substitute-key-definition 'forward-char  'mine-right map gm)
    (substitute-key-definition 'previous-line 'mine-up    map gm)
    (substitute-key-definition 'next-line     'mine-down  map gm)

    (substitute-key-definition 'beginning-of-line 'mine-bol map gm)
    (substitute-key-definition 'backward-word     'mine-bol map gm)
    (substitute-key-definition 'backward-sexp     'mine-bol map gm)
    (substitute-key-definition 'end-of-line       'mine-eol map gm)
    (substitute-key-definition 'forward-word      'mine-eol map gm)
    (substitute-key-definition 'forward-sexp      'mine-eol map gm)
    (define-key map "\M-p"   'mine-top)
    (define-key map "\M-n"   'mine-bottom)
    ;; Hit and mark
    (define-key map " "      'mine-hit-curpoint)
    (define-key map "\C-m"   'mine-mark-curpoint)
    (define-key map [kp-enter] 'mine-mark-curpoint)
    (define-key map "m"      'mine-mark-curpoint)
    (define-key map "q"      'mine-quit)

    (setq mine-mode-map map)))

;;; ================================================================
;;; Faces

(defun mine-init-faces ()
  (setq mine-faces (list))
  (when mine-colorp
    (let ((l (append mine-colors mine-default-colors))
          key.col key col name)
      (while l
        (setq key.col (car l)
              l (cdr l)
              key (car key.col)
              col (cdr key.col))
        (when (null (assoc key mine-faces))
          (setq name
            (cond
	      ((null key) nil)
	      ((symbolp key) (mine-make-face key col))
	      ((not (integerp key))
	       (error "Key should be a symbol or a number: '%s'" key))
	      ((or (< key 0) (> key 8))
	       (error "Key should be a number between 0 and 8: '%s'" key))
	      (t
	       (setq name (intern (concat "mine-face-" key)))
	       (mine-make-face name col))))
          (setq mine-faces (cons (cons key name) mine-faces))))
      (setq mine-faces (nreverse mine-faces)))))

(defun mine-make-face (name col)
  (or (if (fboundp 'internal-find-face)
	  (internal-find-face name)
	(find-face name))
      (let ((face (make-face name)))
	(unless (or (not mine-xemacs-p) col)
	  (setq col (cdr (face-background 'default 'global))))
	(set-face-background face col)
	face))
  name)

(defun mine-get-face (key)
  (cdr (assoc key mine-faces)))

(defun mine-get-glyph (key)
  (if mine-xemacs-p
      (cdr (assoc key mine-default-glyphs))
    nil))
;;; ================================================================
;;; Init board

(defun mine-init-board ()
  (setq mine-padx* (if mine-char-pad 2 1))
  (if (not mine-torus)
      (setq  mine-width  mine-xmax
             mine-height mine-ymax)
    (let (window-xmax window-nb-tiles-x window-xmax-int
          window-ymax window-nb-tiles-y window-ymax-int)
      (setq window-xmax       (/ (window-width) mine-padx*)
            window-nb-tiles-x (/ window-xmax mine-xmax)
            window-xmax-int   (* window-nb-tiles-x window-xmax))
      (setq mine-width
         (max mine-xmax ; at least mine-xmax
            (cond
             ((null mine-nb-tiles-x) window-xmax)
             ((not (numberp mine-nb-tiles-x))
              (error "mine-nb-tiles-x should be nil or a number: %s"
                     mine-nb-tiles-x))
             ((zerop mine-nb-tiles-x) window-xmax-int)
             ((< mine-nb-tiles-x 0)
              (floor (* mine-xmax (- mine-nb-tiles-x))))
             ((floatp mine-nb-tiles-x)
              (min window-xmax (floor (* mine-xmax mine-nb-tiles-x))))
             (t (min window-xmax-int (* mine-xmax mine-nb-tiles-x))))))
      (setq window-ymax (- (window-height) 5)
            window-nb-tiles-y (/ window-ymax mine-ymax)
            window-ymax-int (* window-nb-tiles-y window-ymax))
      (setq mine-height
         (max mine-ymax
            (cond
             ((null mine-nb-tiles-y) window-ymax)
             ((not (numberp mine-nb-tiles-y))
              (error "mine-nb-tiles-y should be nil or a number: %s"
                     mine-nb-tiles-y))
             ((zerop mine-nb-tiles-y) window-ymax-int)
             ((< mine-nb-tiles-y 0)
              (floor (* mine-ymax (- mine-nb-tiles-y))))
             ((floatp mine-nb-tiles-y)
              (min window-ymax (floor (* mine-ymax mine-nb-tiles-y))))
             (t (min window-ymax-int (* mine-ymax mine-nb-tiles-y))))))))
  (let ((buffer-read-only 'nil)
	(face-unmarked (mine-get-face 'mine-face-unmarked))
	(glyph-unmarked (mine-get-glyph 'mine-face-unmarked))
	(face-pad      (mine-get-face 'mine-face-pad))
	(glyph-pad (mine-get-glyph 'mine-face-pad))
        row col)
    (erase-buffer)
    (mine-insert-copyright)
    (mine-insert-remaining-mines)
    (mine-insert-mines-hit)
    (setq mine-point-min (point))
    (setq row mine-height)
    (while (>= (setq row (1- row)) 0)
      (setq col (1- mine-width))
      (insert mine-char-unmarked)
      (when (and (not glyph-unmarked) face-unmarked)
	(put-text-property (1- (point)) (point) 'face face-unmarked))
      (when glyph-unmarked
	(let ((e))
	  (setq e (make-extent (1- (point)) (point)))
	  (set-extent-property e 'invisible t)
	  (set-extent-property e 'end-open t)
	  (set-extent-property e 'start-open nil)
	  (set-extent-end-glyph e glyph-unmarked)))
      (while (>= (setq col (1- col)) 0)
        (when mine-char-pad
          (insert mine-char-pad)
          (when face-pad
            (put-text-property (1- (point)) (point) 'face face-pad)))
        (insert mine-char-unmarked)
	(when (and (not glyph-unmarked) face-unmarked)
	  (put-text-property (1- (point)) (point) 'face face-unmarked))
	(when glyph-unmarked
	  (let ((e))
	    (setq e (make-extent (1- (point)) (point)))
	    (set-extent-property e 'invisible t)
	    (set-extent-property e 'end-open t)
	    (set-extent-property e 'start-open nil)
	    (set-extent-end-glyph e glyph-unmarked))))

      (insert ?\n))
    (setq mine-point-max (1- (point)))
    (mine-update-remaining-mines)
    (mine-update-mines-hit)
    (set-buffer-modified-p 'nil)))

;;; ================================================================
;;; Internal moves

(defun mine-goto-point (point)
  (let ((x.y (mine-point-to-x.y point)))
    (setq mine-x (car x.y) mine-y (cdr x.y))
    (mine-goto mine-x mine-y)
    (= point (point))))

(defun mine-goto (x y)
  (goto-char (mine-xy-to-point x y))
  (cond ((fboundp 'hscroll-point-visible)
	 (hscroll-point-visible))
	((fboundp 'auto-show-make-point-visible)
	 (auto-show-make-point-visible))))

;;; ================================================================
;;; Conversions

(defun mine-xy-to-point (x y)
  ;; p = pmin + 2*w*(y-1) + 2*(x-1)
  (+ mine-point-min
     (* mine-padx* mine-width (1- y))
     (* mine-padx* (1- x))))

;;; Returns the topleft equivalent of point,
;;; on the periodic board, ie. converts point to model coordinates.
(defun mine-top-left (x.y)
  (setcar x.y (1+ (mod (1- (car x.y)) mine-xmax)))
  (setcdr x.y (1+ (mod (1- (cdr x.y)) mine-ymax)))
  x.y)

(defun mine-point-to-x.y (point)
  (let (x y (p0 (- point mine-point-min)))
    (cond
     ((<= p0 0)
      (setq x 1  y 1))
     ((>= point mine-point-max)
      (setq x mine-width  y mine-height))
     (t
      ;; p = pmin + 2*w*(y-1) + 2*(x-1)
      ;; y = (p - pmin)/2w + 1
      ;; x = (p - pmin - 2*w*(y-1)) / 2  +  1
      (setq y (1+ (/ p0 mine-width mine-padx*))
            x (1+ (/ (- p0 (* mine-padx* mine-width (1- y))) mine-padx*)))))
    (cons x y)))

;;; ================================================================
;;; Screen display

(defun mine-mark-board (point)
  (mine-update-board point mine-char-marked 'mine-face-marked))

(defun mine-unmark-board (point)
  (mine-update-board point mine-char-unmarked 'mine-face-unmarked))

(defun mine-display-nb-adjacent-mines (point nb)
  (mine-update-board point
     (if (zerop nb) mine-char-zero (+ ?0 nb))
     nb))

;; todo: enumerer tous les points periodiques
(defun mine-update-board (point c key)
  (let ((buffer-read-only 'nil)
	(face (mine-get-face key))
	(glyph (mine-get-glyph key))
	(x.y (mine-top-left (mine-point-to-x.y point)))
	x y)
    (setq x (car x.y))
    (while (<= x mine-width)
      (setq y (cdr x.y))
      (while (<= y mine-height)
	(mine-update-point (mine-xy-to-point x y) c face glyph)
	(setq y (+ y mine-ymax)))
      (setq x (+ x mine-xmax)))
    (mine-reach-level 1) ; redisplay point and its periodic images
    (set-buffer-modified-p 'nil)))

(defun mine-update-point (point c face &optional glyph)
  (goto-char point)
  (if glyph
      (progn
	(insert c)
	(delete-char 1))
    (delete-char 1)
    (insert c))
  (when (and (not glyph) face)
    (put-text-property point (point) 'face face))
  (when glyph
    ;; (set-extent-end-glyph (extent-at (point)) nil)
    (set-extent-end-glyph (extent-at (point) nil 'end-glyph nil 'at) glyph))
  (mine-reach-level 0)) ; redisplay point

(defun mine-reach-level (level)
  (cond
    ((null mine-level)) ; no update at all
    ((< mine-level 0))  ; no update at all
    ((zerop mine-level) ; unconditional update
     (sit-for 0))
    ((zerop level))     ; wait for level 1
    ((= level 1)
     (cond
       ((= mine-level level)
	(sit-for 0))
       ((= mine-count1 mine-count1-max)
	(setq mine-count1 0)
	(mine-reach-level (1+ level)))
       (t (setq mine-count1 (1+ mine-count1)))))
    ((= level 2)
     (setq mine-count1 0)
     (cond
       ((= mine-level level)
	(sit-for 0))
       ((= mine-count2 mine-count2-max)
	(setq mine-count2 0)
	(mine-reach-level (1+ level)))
       (t (setq mine-count2 (1+ mine-count2)))))
    ((= level 3)
     (setq mine-count1 0)
     (setq mine-count2 0)
     (cond
       ((= mine-level level)
	(sit-for 0))))))

(defun mine-reset-counters ()
  (setq mine-count1 0
        mine-count2 0))

;;; ================================================================
;;; Messages - init board

(defun mine-insert-copyright ()
  (insert mine-version "\n\n"))

(defun mine-insert-remaining-mines ()
  (insert (format "%16s" "Remaining mines") ":")
  (setq mine-point-remaining-mines (point))
  (insert "   \n"))

(defun mine-insert-mines-hit ()
  (insert (format "%16s" "mines hit") ":")
  (setq mine-point-mines-hit (point))
  (insert "   \n\n"))

;;; ================================================================
;;; Messages - update board

(defun mine-update-remaining-mines ()
  (let ((buffer-read-only 'nil))
    (save-excursion
      (goto-char mine-point-remaining-mines)
      (delete-char 3)
      (insert (format "%3d" mine-nb-remaining-marks)))
    (set-buffer-modified-p 'nil))
  (sit-for 0)
  (message "mines remaining to find...%d" mine-nb-remaining-marks)
  (when (and (zerop mine-nb-remaining-mines)
	     (zerop mine-nb-remaining-marks))
    (mine-message 'mine-msg-win)))

(defun mine-update-mines-hit ()
  (let ((buffer-read-only 'nil))
    (save-excursion
      (goto-char mine-point-mines-hit)
      (delete-char 3)
      (insert (format "%3d" mine-nb-mines-hit)))
    (set-buffer-modified-p 'nil)))

(defun mine-update-bogus-mines (nrb nbb)
  (let ((buffer-read-only 'nil)
	(msg (format "There were %d remaining mines and %d bogus mines"
		     nrb nbb)))
    (save-excursion
      (goto-char (point-max))
      (insert "\n" msg))
    (set-buffer-modified-p 'nil)
    (message msg)))

;;; ================================================================
;;; Messages - write minibuffer

(defun mine-message (msg)
  (ding)
  (cond
   ((eq msg 'mine-msg-click-precisely)
    (message "Please, click more precisely"))
   ((eq msg 'mine-msg-unmark-before-hit)
    (message "You must unmark point before hitting it."))
   ((eq msg 'mine-msg-point-already-hit)
    (message "Point has already been hit."))
   ((eq msg 'mine-msg-cannot-mark)
    (message "Can't (un)mark point..."))
   ((eq msg 'mine-msg-lose)
    (message "Sorry... There's a mine here...")
    (sit-for 1)
    (message "Sorry... There's a mine here... You lost!"))
   ((eq msg 'mine-msg-win)
    (message "Congratulations...")
    (sit-for 1)
    (message "Congratulations... You won!"))
   (t
    (message (format "%s" msg)))))

(mine-init-mode-map)

;;; ================================================================

(defun mine-print-variables (l)
  (let (var)
    (princ "(setq ")
    (while l
      (setq var (car l) l (cdr l))
      (cond
	((stringp var) (princ (format ";; %s\n      " var)))
	((not (symbolp var)) (error "Not a symbol: %s" var))
	((not (boundp var))  (error "Unboundp symbol: %s" var))
	(t (princ (format "%-20s'%s" var (symbol-value var)))
	   (when l (princ "\n      ")))))
    (princ "))\n")))

;;; ================================================================

;;(autoload 'apropos-print  "apropos")
;;(autoload 'apropos-do-all "apropos")

(if (not (boundp 'apropos-accumulator))
    (load "apropos"))

(if (boundp 'apropos-item)
;; (Daniel.Pfeiffer's) old official version of apropos
(defun apropos-symbols (l &optional do-all)
  (let ((ll (list)))
    (while l
      (when (not (stringp (car l)))
	(setq ll (cons (car l) ll)))
      (setq l (cdr l)))
    (setq apropos-accumulator (nreverse ll)))
  (or do-all (setq do-all apropos-do-all))
  (apropos-print
   t
   (lambda (p)
     (let (doc symbol)
       (while p
         (setcar p
                 (list ; (s f v p)
                  (setq symbol (car p))
                  (if (commandp symbol)
                    (if (setq doc (documentation symbol t))
                      (substring doc 0 (string-match "\n" doc))
                      "(not documented)"))
                  (and do-all
                       (user-variable-p symbol)
                       (if (setq doc (documentation-property
                                      symbol 'variable-documentation t))
                         (substring doc 0 (string-match "\n" doc))))))
         (setq p (cdr p)))))
   t)))

(provide 'mine)

;;; mine.el ends here
