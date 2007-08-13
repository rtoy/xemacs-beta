;;; xmine.el --- Mine game for XEmacs

;; Author:     Jens Lautenbacher <jens@lemming0.lem.uni-karlsruhe.de>
;; Keywords:   games
;; Version:    1.2

(defconst xmine-version-number "1.2" "XEmacs Mine version number.")
(defconst xmine-version (format "XEmacs Mine v%s by Jens Lautenbacher © 1997"
			       xmine-version-number)
  "Full XEmacs Mine version number.")

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary: This is a complete reimplementation of the classical
;; mine searching game known from various OS/GUIs under names like
;; xmine, minesweeper etc.

;; The idea to implement this in elisp is from
;; Jacques Duthen <duthen@cegelec-red.fr>,
;; the author of the original mine game for GNU Emacs. This version
;; has to the best of my knowledge no code in common with his version,
;; but cudos go to him for first starting this...
;;
;; I mainly wrote this as an example how graphics handling in XEmacs
;; is possible. I think I did it the right way, using an extension to
;; the annotation mechanism and via extensive use of `slots' (realized
;; as properties of extents) to hold the data in the object itself.
;; (Of course this is not true. The keyboard handling is controlled from
;; the "outside" of the objects. But at one time during development
;; before hacking the keyboard controls the code really _was_ nice...
;; now it's a bad messing with slots and controls from the outside)
;;
;; Code:
;;
;;; First of all we'll define the needed varibles.

(defgroup xmine nil
  "The well known mine searching game."
  :group 'games)

(defcustom xmine-width 25
  "The width of the mine field"
  :group 'xmine
  :type 'integer)

(defcustom xmine-height 20
  "The height of the mine field"
  :group 'xmine
  :type 'integer)

(defcustom xmine-glyph-dir (concat data-directory "mine/")
  "The directory where the mine glyphs reside"
  :group 'xmine
  :type 'directory)

(defface xmine-hidden-face
  '((t
     (:background "blue")))
  "The face used for hidden tiles on ttys"
  :group 'xmine)

(defface xmine-flagged-face
  '((t
     (:background "red")))
  "The face used for flagged tiles on ttys"
  :group 'xmine)

(defface xmine-number-face
  '((t
     (:background "green")))
  "The face used for unhidden, numbered tiles on ttys"
  :group 'xmine)


(defvar xmine-pad-glyph
  (make-glyph
   (if (and (eq window-system 'x) (featurep 'xpm))
       (concat xmine-glyph-dir "pad.xpm")
     "      ")))

(defvar xmine-title-glyph
  (make-glyph
   (if (and (eq window-system 'x) (featurep 'xpm))
       (concat xmine-glyph-dir "splash.xpm")
     "------------------ XEmacs XMine ------------------")))

(defvar xmine-glyph-production-list
  '(("xmine-new-up"            "new_up.gif"               "new"  nil               )
    ("xmine-new-down"          "new_down.gif"             "NEW"  nil               )
    ("xmine-quit-up"           "quit_up.gif"              "quit" nil               )
    ("xmine-quit-down"         "quit_down.gif"   	  "QUIT" nil               )
    ("xmine-up-glyph"          "empty_16_up.gif"          "@ "   xmine-hidden-face )
    ("xmine-up-sel-glyph"      "empty_16_up_sel.gif"      "@<"   xmine-hidden-face )
    ("xmine-down-glyph"        "empty_16_down.gif"        "? "   nil               ) 
    ("xmine-flagged-glyph"     "flagged_16_up.gif"        "! "   xmine-flagged-face)
    ("xmine-flagged-sel-glyph" "flagged_16_up_sel.gif"    "!<"   xmine-flagged-face)
    ("xmine-mine-glyph"        "bomb_16_flat.gif"         "* "   nil               )
    ("xmine-mine-sel-glyph"    "bomb_16_flat.gif"         "*<"   nil               )
    ("xmine-trapped-glyph"     "bomb_trapped_16_flat.gif" "X "   nil               )
    ("xmine-0-glyph"           "empty_16_flat.gif"        ". "   nil               )
    ("xmine-0-sel-glyph"       "empty_16_flat_sel.gif"    ".<"   nil               )
    ("xmine-1-glyph"           "1_16_flat.gif"            "1 "   xmine-number-face )
    ("xmine-1-sel-glyph"       "1_16_flat_sel.gif"        "1<"   xmine-number-face )
    ("xmine-2-glyph"           "2_16_flat.gif"            "2 "   xmine-number-face )
    ("xmine-2-sel-glyph"       "2_16_flat_sel.gif"        "2<"   xmine-number-face )
    ("xmine-3-glyph"           "3_16_flat.gif"            "3 "   xmine-number-face )
    ("xmine-3-sel-glyph"       "3_16_flat_sel.gif"        "3<"   xmine-number-face )
    ("xmine-4-glyph"           "4_16_flat.gif"            "4 "   xmine-number-face )
    ("xmine-4-sel-glyph"       "4_16_flat_sel.gif"        "4<"   xmine-number-face )
    ("xmine-5-glyph"           "5_16_flat.gif"            "5 "   xmine-number-face )
    ("xmine-5-sel-glyph"       "5_16_flat_sel.gif"        "5<"   xmine-number-face )
    ("xmine-6-glyph"           "6_16_flat.gif"            "6 "   xmine-number-face )
    ("xmine-6-sel-glyph"       "6_16_flat_sel.gif"        "6<"   xmine-number-face )
    ("xmine-7-glyph"           "7_16_flat.gif"            "7 "   xmine-number-face )
    ("xmine-7-sel-glyph"       "7_16_flat_sel.gif"        "7<"   xmine-number-face )
    ("xmine-8-glyph"           "8_16_flat.gif"            "8 "   xmine-number-face )
    ("xmine-8-sel-glyph"       "8_16_flat_sel.gif"        "8<"   xmine-number-face )))

(defun xmine-generate-glyphs ()
  (let ((list xmine-glyph-production-list)
	elem var gif text face)
    (while (setq elem (pop list))
      (setq var  (car    elem)
	    gif  (cadr   elem)
	    text (caddr  elem)
	    face (cadddr elem))
      (set (intern var)
	   (make-glyph (if (eq window-system 'x)
			   (concat xmine-glyph-dir gif)
			 text)))
      (if face
	  (set-glyph-face (eval (intern-soft var)) face)))))

(xmine-generate-glyphs)

(defvar xmine-key-sel-button nil)

(defun xmine-up-glyph (ext)
  (if (equal ext xmine-key-sel-button)
      (progn
	(set-extent-property ext 'xmine-non-selected-glyph xmine-up-glyph)
	xmine-up-sel-glyph)
    xmine-up-glyph))

(defun xmine-flagged-glyph (ext)
  (if (equal ext xmine-key-sel-button)
      (progn
	(set-extent-property ext 'xmine-non-selected-glyph xmine-flagged-glyph)
	xmine-flagged-sel-glyph)
    xmine-flagged-glyph))

(defcustom xmine-%-of-mines 12
  "The percentage of tiles that should be mines."
  :group 'xmine
  :type 'integer)

(defcustom xmine-balloon-list (list "What are you waiting for?"
				    "Push me!"
				    "Come on. Don't sleep."
				    "Are you sure?"
				    "Are you sleeping?"
				    "Yes! Do it!"
				    "I'm getting bored."
				    "You will NEVER beat me.")
  "(Random) texts for the balloon-help property of the tiles"
  :group 'xmine
  :type '(repeat (string)))

(defcustom xmine-background "white"
  "The background color of XMine's buffer.
Many colors will not blend nicely with the logo. Shades of light grey are
preferred if you don't want to use white."
  :group 'xmine
  :type 'color)

(defvar xmine-keymap nil)

(if xmine-keymap ()
  (setq xmine-keymap (make-sparse-keymap))
  (suppress-keymap xmine-keymap)
  (define-key xmine-keymap [up] 'xmine-key-up)
  (define-key xmine-keymap [down] 'xmine-key-down)
  (define-key xmine-keymap [right] 'xmine-key-right)
  (define-key xmine-keymap [left] 'xmine-key-left)
  (define-key xmine-keymap "e" 'xmine-key-up)
  (define-key xmine-keymap "c" 'xmine-key-down)
  (define-key xmine-keymap "f" 'xmine-key-right)
  (define-key xmine-keymap "s" 'xmine-key-left)
  (define-key xmine-keymap "w" 'xmine-key-up-left)
  (define-key xmine-keymap "x" 'xmine-key-down-left)
  (define-key xmine-keymap "r" 'xmine-key-up-right)
  (define-key xmine-keymap "v" 'xmine-key-down-right)
  (define-key xmine-keymap [return] 'xmine-key-action3)
  (define-key xmine-keymap "d" 'xmine-key-action3)
  (define-key xmine-keymap [(shift space)] 'xmine-key-action2)
  (define-key xmine-keymap "a" 'xmine-key-action2)
  (define-key xmine-keymap [space] 'xmine-key-action1)
  (define-key xmine-keymap [Q] 'xmine-key-quit)
  (define-key xmine-keymap [N] 'xmine-key-new))

(defvar xmine-number-of-flagged 0)

(defvar xmine-number-of-opened 0)
  
(defvar xmine-number-of-mines 0)

(defvar xmine-field nil)

(defvar xmine-buffer nil)

(defvar xmine-quit-ann nil)

(defvar xmine-new-ann nil)

(defvar xmine-count-ann nil)

(defvar xmine-count-glyph (make-glyph "Mines: 00"))

(defvar xmine-mode-hook nil
  "*Hook called by `xmine-mode-hook'.")

;; the next function is more or less stolen from annotation.el and
;; modified to fit in our scheme were all three buttons should trigger
;; actions

(defun xmine-activate-function-button (event)
  (interactive "e")
  (let* ((extent (event-glyph-extent event))
	 (button (event-button event))
	 (action (intern (concat "action" (number-to-string button))))
	 (mouse-down t)
	 (up-glyph nil))
    ;; make the glyph look pressed
    (cond ((annotation-down-glyph extent)
	   (setq up-glyph (annotation-glyph extent))
	   (set-annotation-glyph extent (annotation-down-glyph extent))))
    (while mouse-down
      (setq event (next-event event))
      (if (button-release-event-p event)
	  (setq mouse-down nil)))
    ;; make the glyph look released
    (cond ((annotation-down-glyph extent)
	   (set-annotation-glyph extent up-glyph)))
    (if (eq extent (event-glyph-extent event))
	(if (extent-property extent action)
	    (funcall (extent-property extent action) extent)))))

;;; Here we define the button object's constructor function

(defun xmine-button-create (x y type)
  (let ((ext (make-annotation
	      xmine-up-glyph nil 'text nil nil xmine-down-glyph nil)))
    (set-extent-property ext 'action1 'xmine-action1)
    (set-extent-property ext 'action2 'xmine-beep)
    (set-extent-property ext 'action3 'xmine-action3)
    (set-extent-property ext 'xmine-glyph (xmine-type-to-glyph type))
    (set-extent-property ext 'xmine-sel-glyph (xmine-type-to-sel-glyph type)) 
    (set-extent-property ext 'xmine-type type)
    (set-extent-property ext 'xmine-x x)
    (set-extent-property ext 'xmine-y y)
    (set-extent-property ext 'xmine-flagged nil)
    (set-extent-property ext 'xmine-hidden t)
    (set-extent-property ext 'end-open t)
    (set-extent-property ext 'balloon-help (xmine-balloon-text))
    (aset xmine-field (+ (* (1- y) xmine-width) (1- x)) ext)))

;;; ...and this is the second global function to change a
;;; button object. It is only needed during creation of the board.

(defun xmine-button-change-type (ext type)
  (set-extent-property ext 'xmine-glyph (xmine-type-to-glyph type))
  (set-extent-property ext 'xmine-sel-glyph (xmine-type-to-sel-glyph type)) 
  (set-extent-property ext 'xmine-type type))

;;; some needed predicates.

(defun xmine-flat-button-p (ext)
  (and ext
       (not (extent-property ext 'xmine-hidden))
       (equal "0" (extent-property ext 'xmine-type))))

(defun xmine-mine-button-p (ext)
  (and ext
       (equal "mine" (extent-property ext 'xmine-type))))

;;; the next three functions are helper functions used inside a button
;;; object.

(defun xmine-balloon-text ()
  (nth (random (length xmine-balloon-list)) xmine-balloon-list))

(defun xmine-beep (&rest forget)
  (beep))

(defun xmine-type-to-glyph (type)
  (eval (intern-soft (concat "xmine-" type "-glyph"))))

(defun xmine-type-to-sel-glyph (type)
  (eval (intern-soft (concat "xmine-" type "-sel-glyph"))))

;;; the next three functions are the main functions that are used
;;; inside the button objects and which are bound to the 'action1,
;;; 'action2 and 'action3 slots respectively

(defun xmine-action1 (ext &optional no-repaint force)
  "This unhides a hidden button"
  (if (or force
	  (not (extent-property ext 'xmine-flagged)))
      (progn
	(if (and (not force)
		 (extent-property ext 'xmine-hidden))
	    (setq xmine-number-of-opened (1+ xmine-number-of-opened)))
	(set-extent-property ext 'xmine-hidden nil)
	(set-annotation-glyph ext (if (equal ext xmine-key-sel-button)
				      (progn
					(set-extent-property
					 ext 'xmine-non-selected-glyph
					 (extent-property ext 'xmine-glyph))
					(extent-property ext 'xmine-sel-glyph))
				    (extent-property ext 'xmine-glyph)))
	(set-extent-property ext 'action3 nil)
	(set-extent-property ext 'action1 nil)
	(set-extent-property ext 'balloon-help nil)
	(set-extent-property ext 'action2 'xmine-action2)
	(if (not no-repaint)
	    (progn
	      (xmine-field-repaint ext)
	      (if (xmine-game-solved-p) (xmine-end-game)))))))

(defun xmine-action2 (ext)
  "This unhides all hidden neighbours of a button.
It is meant as convenience function you can use if you're sure that
you've marked all mines around the button correctly (or you're sure
there isn't one)"
  (let ((list (xmine-get-neighbours ext))
	next)
    (while (setq next (pop list))
      (if (not (xmine-flat-button-p next)) (xmine-action1 next)))))

(defun xmine-action3 (ext)
  "This toggles the flagged status of a button.
You flag a button if you know - or think - that there's a mine under it"
(if (extent-property ext 'xmine-flagged)
      (progn
	(set-annotation-glyph ext (xmine-up-glyph ext))
	(set-extent-property ext 'action1 'xmine-action1)
	(set-extent-property ext 'xmine-flagged nil)
	(setq xmine-number-of-flagged (1- xmine-number-of-flagged))
	(set-annotation-glyph xmine-count-ann
			      (make-glyph
			       (format "Mines: %2d"
				       (- xmine-number-of-mines
					    xmine-number-of-flagged)))))
    (if (= xmine-number-of-flagged xmine-number-of-mines)
	(progn 
	  (beep)
	  (message
	   "Impossible. You seem to have marked too many tiles as mines?"))
      (set-annotation-glyph ext (xmine-flagged-glyph ext))
      (set-extent-property ext 'action1 nil)
      (set-extent-property ext 'xmine-flagged t)
      (setq xmine-number-of-flagged (1+ xmine-number-of-flagged))
      (if (xmine-game-solved-p) (xmine-end-game)
	(set-annotation-glyph xmine-count-ann
			      (make-glyph
			       (format "Mines: %2d"
				       (- xmine-number-of-mines
					  xmine-number-of-flagged))))))))


;;; what to do after a button is unhidden: We (maybe) have to repaint
;;; parts of the board. This is done here recursively.

(defun xmine-field-repaint (ext)
  (let* ((flatp  (xmine-flat-button-p ext))
	 (minep  (xmine-mine-button-p ext))
	 (neighbours (xmine-get-neighbours ext))
	 (max-lisp-eval-depth (* 8 xmine-width xmine-height))
	 next-ext ext-list)
    (cond (flatp
	   (while (setq next-ext (pop neighbours))
	     (if (extent-property next-ext 'xmine-hidden)
		 (progn
		   (xmine-action1 next-ext 'no-repaint)
		   (and (equal "0" (extent-property next-ext 'xmine-type))
			(push next-ext ext-list)))))
	   (while ext-list
	     (setq next-ext (pop ext-list))
	     (xmine-field-repaint next-ext)))
	  (minep
	   (set-extent-property ext 'xmine-glyph xmine-trapped-glyph)
	   (set-extent-property ext 'xmine-sel-glyph xmine-trapped-glyph)
	   (xmine-show-all)
	   (xmine-end-game-trapped)))))


(defun xmine-get-neighbours (ext)
  "This gives back a list of all neighbours of a button, correctly
  handling buttons at the side or corner of course"
(let* ((x (extent-property ext 'xmine-x))
	 (y (extent-property ext 'xmine-y))
	 next-coord next list
	 (neighbours  (list (list (1- x) (1+ y))
			    (list     x  (1+ y))
			    (list (1+ x) (1+ y))
			    (list (1- x) (1- y))
			    (list     x  (1- y))
			    (list (1+ x) (1- y))
			    (list (1+ x)     y)
			    (list (1- x)     y))))
    (while (setq next-coord (pop neighbours))
      (if (setq next (xmine-field-button-at (car next-coord)
					    (cadr next-coord)))
	  (push next list)))
    list))
    

;;; the next four functions are used to know if we're at the end of
;;; the game (either successfully or exploded) and do the approbate
;;; action

(defun xmine-game-solved-p ()
  "You have solved the game successfully if the number of flagged
mines plus the number of unhidden buttons equals width*height of the field"
  (equal (+ xmine-number-of-flagged xmine-number-of-opened)
	 (* xmine-width xmine-height)))

(defun xmine-end-game ()
  (beep)
  (set-annotation-glyph xmine-count-ann
			(make-glyph " Solved. ")))

(defun xmine-end-game-trapped ()
  (beep)
  (set-annotation-glyph xmine-count-ann
			(make-glyph "++ RIP ++")))

(defun xmine-show-all ()
  (let ((list (append xmine-field nil))
	next)
    (while (setq next (pop list))
      (xmine-action1 next 'no-repaint 'force))))


(defun xmine-field-button-at (x y)
  "This function gives back the button at a given coordinate pair (x y)
It is only used during creation of the board and when getting the
neighbours of a button (and for keyboard handling...), as we don't
want to use coordinates in the main loop, only the button object
itself should be referenced. Of course the use of this function could
be avoided in xmine-get-neighbours by storing the neighbour buttons
directly in the button, but this seems to be a bit oversized for this
little game."
  (if (or (> x xmine-width)  (< x 1)
	  (> y xmine-height) (< y 1)) nil
    (aref xmine-field (+ (* (1- y) xmine-width) (1- x)))))

(defun xmine-mode ()
"A mode for playing the well known mine searching game.

   `\\<annotation-local-map-default>\\[xmine-activate-function-button1]' or `\\<xmine-keymap>\\[xmine-key-action1]' unhides a tile,
   `\\<annotation-local-map-default>\\[xmine-activate-function-button2]' or `\\<xmine-keymap>\\[xmine-key-action2]' unhides all neighbours of a tile,
   `\\<annotation-local-map-default>\\[xmine-activate-function-button3]' or `\\<xmine-keymap>\\[xmine-key-action3]' (un)flagges a tile to hold a mine.

   `\\[xmine-key-new]' starts a new game.
   `\\[xmine-key-quit]' ends a game.

All keybindings (with alternatives) currently in effect:
   \\{xmine-keymap}

The rules are quite easy: You start by unhiding (random) tiles. An unhidden
tile showing a number tells you something about the number of mines in it's
neighborhood, where the neighborhood are all 8 tiles (or less if it's
at a border) around the tile.

E.g. a \"1\" shows you that there is only one mine in the neighborhood of
this tile. Empty tiles have no mines around them, and empty tiles in
the neighborhood of another empty tile are all automatically unhidden
if you unhide one of them. You need to find a strategy to use the
information you have from the numbers to \"flag\" the tiles with mines
under them and unhide all other tiles. If you correctly made this
without accidently unhiding a mine, you've won.

If you are sure you have correctly flagged all mines around a unhidden tile,
you can use Button-2 or \\[xmine-key-action2] on it to unhide all it's
neighbors. But beware: If you made a mistake by flagging the wrong mines,
you'll blow up! 

Have Fun."
  (interactive)
  (xmine-field-create))

(fset 'xmine 'xmine-mode)

(defun xmine-field-create ()
  "We create the playing board here."
  (let ((width 1)
	(height 1)
	(pop-up-windows nil)
	total)
    (xmine-buffer-init)
    (pop-to-buffer xmine-buffer)
    (setq total (* xmine-height xmine-width))
    (setq xmine-field (make-vector total nil))
    (xmine-init-mines
     (setq xmine-number-of-mines
	   (min 99 (round (* (/ (float xmine-%-of-mines) 100) total)))))
    (insert "\n ")
    (set-extent-end-glyph (make-extent (point) (point)) xmine-title-glyph)
    (insert "\n\n")
    (while (<= height xmine-height)
      (insert " ")
      (while (<= width xmine-width)
	(if (xmine-field-button-at width height)
	    (xmine-button-create width height "mine")
	  (xmine-button-create width height "0"))
	(setq width (+ width 1)))
      (insert " \n")
      (setq width 1)
      (setq height (+ height 1)))
    (insert "\n  ")
    (set-extent-begin-glyph (make-extent (point) (point)) xmine-pad-glyph)
    (setq xmine-new-ann
	  (make-annotation xmine-new-up nil
			   'text nil nil xmine-new-down nil))
    (set-extent-property xmine-new-ann 'action1 '(lambda (&rest egal)
						   (xmine-field-create)))
    (set-extent-property xmine-new-ann 'action2 nil)
    (set-extent-property xmine-new-ann 'action3 nil)
    (set-extent-property xmine-new-ann 'end-open t)
    (set-extent-begin-glyph (make-extent (point) (point)) xmine-pad-glyph)
    (setq xmine-count-ann
	  (make-annotation xmine-count-glyph nil
			   'text nil nil nil nil))
    (set-extent-begin-glyph (make-extent (point) (point)) xmine-pad-glyph)
    (setq xmine-quit-ann
	  (make-annotation xmine-quit-up nil
			   'text nil nil xmine-quit-down nil))
    (set-extent-property xmine-quit-ann 'action1
			 '(lambda (&rest egal)
			    (kill-buffer (current-buffer))))
    (set-extent-property xmine-quit-ann 'action2 nil)
    (set-extent-property xmine-quit-ann 'action3 nil)
    (set-extent-property xmine-quit-ann 'end-open t)
    (xmine-attach-numbers)
    (setq xmine-number-of-flagged 0)
    (setq xmine-number-of-opened 0)
    (set-annotation-glyph xmine-count-ann
			  (make-glyph
			   (format "Mines: %2d" xmine-number-of-mines)))
    (goto-char (point-min))
    (setq buffer-read-only 't)
    (if (eq window-system 'x)
	(set-specifier (face-background 'default)
		       xmine-background xmine-buffer))
    (set-specifier (face-background 'text-cursor)
		   xmine-background xmine-buffer)
    (setq xmine-key-sel-button nil)
    (xmine-select-button (xmine-field-button-at (/ xmine-width 2)
						(/ xmine-height 2)))))


(defun xmine-init-mines (num)
  "A subroutine for xmine-field create.
We randomly set a part of the nil-filled board vector with t to
indicate the places where mines should reside."
  (let (x y elem)
    (random t)
    (while (> num 0)
      (setq x (1+ (random xmine-width)))
      (setq y (1+ (random xmine-height)))
      (setq elem (xmine-field-button-at x y))
      (if (not elem)
	  (progn
	    (aset xmine-field (+ (* (1- y) xmine-width) (1- x)) t)
	    (setq num (1- num)))))))
	
(defun xmine-attach-numbers ()
  "A subroutine for xmine-field-create.
The board is populated by now with empty buttons and mines. Here we
change the correct empty buttons to \"numbered\" buttons"
  (let 
      ((buttons (append xmine-field nil))
       ext)
    (while (setq ext (pop buttons))
      (let ((num 0)
	    (minep (xmine-mine-button-p ext))
	    (neighbours (xmine-get-neighbours ext))
	    next)
	(if (not minep)
	    (progn
	      (while (setq next (pop neighbours))
		(if (xmine-mine-button-p next) (setq num (1+ num))))
	      (if (> num 0)
		  (xmine-button-change-type ext (number-to-string num)))))))))

	    
(defun xmine-buffer-init ()
  "A subroutine for xmine-create-field.
We set up the XMine buffer, set up the keymap and so on."
  (if xmine-buffer (kill-buffer xmine-buffer))
  (setq xmine-buffer (get-buffer-create "XEmacs Mine"))
  (save-excursion
    (set-buffer xmine-buffer)
    (kill-all-local-variables)
    (make-local-variable 'annotation-local-map-default)
    (setq truncate-lines 't)
    (setq major-mode 'xmine-mode)
    (setq mode-name "XMine")
    (put 'xmine-mode 'mode-class 'special)
    (use-local-map xmine-keymap)
    (buffer-disable-undo (current-buffer))
    (setq annotation-local-map-default
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-name map 'annotation-local-map)
	    (define-key map 'button1 'xmine-activate-function-button)
	    (define-key map 'button2 'xmine-activate-function-button)
	    (define-key map 'button3 'xmine-activate-function-button)
	    map))
    (run-hooks 'xmine-mode-hook)))

;;; The keyboard navigation.

(defun xmine-select-button (ext)
  (let ((flagged (extent-property ext 'xmine-flagged))
	(hidden  (extent-property ext 'xmine-hidden))
	sel-glyph)
    (setq sel-glyph (if hidden
		       (if flagged xmine-flagged-sel-glyph
			 xmine-up-sel-glyph)
		      (extent-property ext 'xmine-sel-glyph)))
    (if xmine-key-sel-button
	(set-annotation-glyph xmine-key-sel-button
			      (extent-property xmine-key-sel-button
					       'xmine-non-selected-glyph)))
    (set-extent-property ext 'xmine-non-selected-glyph
			 (annotation-glyph ext))
    (set-annotation-glyph ext sel-glyph)
    (setq xmine-key-sel-button ext)))

(defun xmine-key-action1 ()
  (interactive)
  (let ((action (extent-property xmine-key-sel-button 'action1)))
    (if action
	(funcall action xmine-key-sel-button))))

(defun xmine-key-action2 ()
  (interactive)
  (let ((action (extent-property xmine-key-sel-button 'action2)))
    (if action
	(funcall action xmine-key-sel-button))))

(defun xmine-key-action3 ()
  (interactive)
  (let ((action (extent-property xmine-key-sel-button 'action3)))
    (if action
	(funcall action xmine-key-sel-button))))

(defun xmine-key-quit ()
  (interactive)
  (kill-buffer (current-buffer)))
  
(defun xmine-key-new ()
  (interactive)
  (xmine-field-create))

(defun xmine-key-down-right ()
  (interactive)
  (xmine-key-down)
  (xmine-key-right))

(defun xmine-key-down-left ()
  (interactive)
  (xmine-key-down)
  (xmine-key-left))

(defun xmine-key-up-right ()
  (interactive)
  (xmine-key-up)
  (xmine-key-right))

(defun xmine-key-up-left ()
  (interactive)
  (xmine-key-up)
  (xmine-key-left))

(defun xmine-key-down ()
  (interactive)
  (let* ((x (extent-property xmine-key-sel-button 'xmine-x))
	 (y (extent-property xmine-key-sel-button 'xmine-y))
	 (ext (xmine-field-button-at x (1+ y))))
    (if ext (xmine-select-button ext)
      (xmine-select-button (xmine-field-button-at x 1)))))

(defun xmine-key-up ()
  (interactive)
  (let* ((x (extent-property xmine-key-sel-button 'xmine-x))
	 (y (extent-property xmine-key-sel-button 'xmine-y))
	 (ext (xmine-field-button-at x (1- y))))
    (if ext (xmine-select-button ext)
      (xmine-select-button (xmine-field-button-at x xmine-height)))))

(defun xmine-key-right ()
  (interactive)
  (let* ((x (extent-property xmine-key-sel-button 'xmine-x))
	 (y (extent-property xmine-key-sel-button 'xmine-y))
	 (ext (xmine-field-button-at (1+ x) y)))
    (if ext (xmine-select-button ext)
      (xmine-select-button (xmine-field-button-at 1 y)))))

(defun xmine-key-left ()
  (interactive)
  (let* ((x (extent-property xmine-key-sel-button 'xmine-x))
	 (y (extent-property xmine-key-sel-button 'xmine-y))
	 (ext (xmine-field-button-at (1- x) y)))
    (if ext (xmine-select-button ext)
      (xmine-select-button (xmine-field-button-at xmine-width y)))))

(provide 'xmine)

