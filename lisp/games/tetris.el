;;; tetris.el -- Implementation of Tetris for Emacs.

;; Copyright (C) 1997 Glynn Clements <glynn@sensei.co.uk>

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 1.8
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; Modified: 1997-08-17, added tetris-move-bottom
;; Modified: 1997-08-22, changed setting of display table for compatibility
;;	with XEmacs 19.15
;; Modified: 1997-08-23, changed setting of display table for TTY compatibility
;; Modified: 1997-08-24, various changes for FSF Emacs compatibility
;; Modified: 1997-08-25
;;	modified existing docstrings, added new docstrings
;;	L now rotates the same way as T and mirror-L
;;	now adds tetris-end-game to buffer-local value of kill-buffer-hook
;; Modified: 1997-08-26, miscellaneous bugfixes
;; Modified: 1997-08-27
;;	added color support for non-glyph mode
;;	added tetris-mode-hook
;;	added tetris-update-speed-function
;; Modified: 1997-09-09, changed layout to work in a 22 line window
;; Modified: 1997-09-12
;;	fixed tetris-shift-down to deal with multiple rows correctly
;; URL: ftp://sensei.co.uk/misc/tetris.el.gz
;; Tested with XEmacs 20.3-beta and Emacs 19.34
;; Reported to work with XEmacs 19.15 and 20.2

(eval-when-compile
  (require 'cl))

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-use-glyphs t
  "Non-nil means use glyphs when available")

(defvar tetris-use-color t
  "Non-nil means use color when available")

(defvar tetris-draw-border-with-glyphs t
  "Non-nil means draw a border even when using glyphs")

(defvar tetris-font "-*-courier-medium-r-*-*-*-140-100-75-*-*-iso8859-*"
  "Name of the font used for tetris in X mode")

(defvar tetris-default-tick-period 0.3
  "The default time taken for a shape to drop one row")

(defvar tetris-update-speed-function
  'tetris-default-update-speed-function
  "Function run whenever the Tetris score changes
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped
ROWS is the number of rows which have been completed

If the return value is a number, it is used as the timer period")

(defvar tetris-mode-hook nil
  "Hook run upon starting Tetris")

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tetris-buffer-name "*Tetris*"
  "Name used for Tetris buffer")

(defconst tetris-space-char [?\040]
  "Character vector used for a space")

(defconst tetris-block-char [?\040]
  "Character vector for a full square in text mode")

(defconst tetris-emacs-block-char [?O]
  "Character vector for a full square in text mode under Emacs")

(defconst tetris-border-char [?\+]
  "Character vector for a border square in text mode")

(defconst tetris-buffer-width 30
  "Width of used portion of buffer")

(defconst tetris-buffer-height 22
  "Height of used portion of buffer")

(defconst tetris-width 10
  "Width of playing area")

(defconst tetris-height 20
  "Height of playing area")

(defconst tetris-top-left-x 3
  "X position of top left of playing area")

(defconst tetris-top-left-y 1
  "Y position of top left of playing area")

(defconst tetris-next-x (+ (* 2 tetris-top-left-x) tetris-width)
  "X position of next shape")

(defconst tetris-next-y tetris-top-left-y
  "Y position of next shape")

(defconst tetris-score-x tetris-next-x
  "X position of score")

(defconst tetris-score-y (+ tetris-next-y 6)
  "Y position of score")

(defconst tetris-blank 0)

(defconst tetris-space ?\.)

(defconst tetris-border ?\*)

(defconst tetris-shapes
  [[[[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[2 2 2 0] [0 2 0 0] [2 0 0 0] [2 2 0 0]]
    [[0 0 2 0] [0 2 0 0] [2 2 2 0] [2 0 0 0]]
    [[0 0 0 0] [2 2 0 0] [0 0 0 0] [2 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[3 3 3 0] [3 3 0 0] [0 0 3 0] [3 0 0 0]]
    [[3 0 0 0] [0 3 0 0] [3 3 3 0] [3 0 0 0]]
    [[0 0 0 0] [0 3 0 0] [0 0 0 0] [3 3 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[4 4 0 0] [0 4 0 0] [4 4 0 0] [0 4 0 0]]
    [[0 4 4 0] [4 4 0 0] [0 4 4 0] [4 4 0 0]]
    [[0 0 0 0] [4 0 0 0] [0 0 0 0] [4 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 5 5 0] [5 0 0 0] [0 5 5 0] [5 0 0 0]]
    [[5 5 0 0] [5 5 0 0] [5 5 0 0] [5 5 0 0]]
    [[0 0 0 0] [0 5 0 0] [0 0 0 0] [0 5 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 6 0 0] [6 0 0 0] [6 6 6 0] [0 6 0 0]]
    [[6 6 6 0] [6 6 0 0] [0 6 0 0] [6 6 0 0]]
    [[0 0 0 0] [6 0 0 0] [0 0 0 0] [0 6 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[7 7 7 7] [7 0 0 0] [7 7 7 7] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]]])

(defconst tetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-text-colors
  ["black" "blue" "white" "yellow"
   "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode
Element 0 is the background color")

(defconst tetris-colors
  [[0 0 0] [0 0 1] [0.7 0 1] [1 1 0]
   [1 0 1] [0 1 1] [0 1 0] [1 0 0]
   [0.5 0.5 0.5]]
  "Vector of colors of the various shapes
Element 0 is the background color
Element 8 is the border color")

(defconst tetris-xpm "\
/* XPM */
static char *noname[] = {
/* width height ncolors chars_per_pixel */
\"16 16 3 1\",
/* colors */
\"+ s col1\",
\". s col2\",
\"- s col3\",
/* pixels */
\"---------------+\",
\"--------------++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"--............++\",
\"-+++++++++++++++\",
\"++++++++++++++++\"
};
"
  "XPM format image used for each square")

(defun tetris-default-update-speed-function (shapes rows)
  (/ 20.0 (+ 50.0 rows)))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-faces (make-vector 256 nil))

(defvar tetris-buffer-start 1)

(defvar tetris-display-mode nil)

(defvar tetris-shape 0)
(defvar tetris-rot 0)
(defvar tetris-next-shape 0)
(defvar tetris-n-shapes 0)
(defvar tetris-n-rows 0)
(defvar tetris-pos-x 0)
(defvar tetris-pos-y 0)

(defvar tetris-timer nil)

(defvar tetris-display-table nil)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "n"		'tetris-start-game)
(define-key tetris-mode-map "q"		'tetris-end-game)

(define-key tetris-mode-map " "		'tetris-move-bottom)
(define-key tetris-mode-map [left]	'tetris-move-left)
(define-key tetris-mode-map [right]	'tetris-move-right)
(define-key tetris-mode-map [up]	'tetris-rotate-prev)
(define-key tetris-mode-map [down]	'tetris-rotate-next)

(defvar tetris-null-map
  (make-sparse-keymap 'tetris-null-map))

(define-key tetris-null-map "n"		'tetris-start-game)

;; ;;;;;;;;;;;;;;;; timer functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-start-timer (period)
  (setq tetris-timer
	(if (featurep 'itimer)
	    (start-itimer
	     "Tetris"
	     'tetris-update-game period period
	     nil t (current-buffer))
	  (run-with-timer
	   period period
	   'tetris-update-game (current-buffer)))))

(defun tetris-set-timer (delay)
  (if tetris-timer
      (if (featurep 'itimer)
	  (set-itimer-restart tetris-timer delay)
	(timer-set-time tetris-timer
			(list (aref tetris-timer 1)
			      (aref tetris-timer 2)
			      (aref tetris-timer 3))
			delay))))

(defun tetris-kill-timer ()
  (if tetris-timer
      (if (featurep 'itimer)
          (delete-itimer tetris-timer)
        (timer-set-time tetris-timer '(0 0 0) nil)))
  (setq tetris-timer nil))

;; ;;;;;;;;;;;;; display functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-color (col shade)
  (let* ((vec (aref tetris-colors col))
	 (v (floor (* shade 255)))
	 (r (* v (aref vec 0)))
	 (g (* v (aref vec 1)))
	 (b (* v (aref vec 2))))
    (format "#%02x%02x%02x" r g b)))

(defun tetris-set-font (face)
  (if tetris-font
      (condition-case nil
	  (set-face-font face tetris-font)
	('error nil))))

(defun tetris-setup-face (face color)
  (set-face-foreground face color)
  (set-face-background face color)
  (tetris-set-font face)
  (condition-case nil
      (set-face-background-pixmap face [nothing])	;; XEmacs
    ('error nil))
  (condition-case nil
      (set-face-background-pixmap face nil)		;; Emacs
    ('error nil)))

(defun tetris-make-mono-tty-face ()
  (let ((face (make-face 'tetris-mono-tty-face)))
    (condition-case nil
	(set-face-property face 'reverse t)
      ('error nil))
    face))

(defun tetris-make-color-tty-face (c)
  (let* ((name (intern (format "tetris-color-tty-face-%d" c)))
	 (face (make-face name)))
    (tetris-setup-face face (aref tetris-text-colors c))
    face))

(defun tetris-make-x-border-face ()
  (let ((face (make-face 'tetris-x-border-face)))
    (tetris-set-font face)
    face))

(defun tetris-make-mono-x-face ()
  (let ((face (make-face 'tetris-mono-x-face))
	(color (face-foreground 'default)))
    (if (null color)
	(setq color
	      (cdr-safe (assq 'foreground-color (frame-parameters)))))
    (tetris-setup-face face color)
    face))

(defun tetris-make-color-x-face (c)
  (let* ((name (intern (format "tetris-color-x-face-%d" c)))
	 (face (make-face name)))
    (tetris-setup-face face (tetris-color c 1.0))
    face))

(defun tetris-make-mono-tty-faces ()
  (let ((face (tetris-make-mono-tty-face)))
    (loop for c from 0 to 255 do
	  (aset tetris-faces c
		(cond
		 ((or (= c 0) (> c 7))
		  'default)
		 (t
		  face))))))

(defun tetris-make-color-tty-faces ()
  (loop for c from 0 to 255 do
	(aset tetris-faces c
	      (cond
	       ((> c 7)
		  'default)
	       (t
		(tetris-make-color-tty-face c))))))

(defun tetris-make-mono-x-faces ()
  (let ((face (tetris-make-mono-x-face))
	(face2 (tetris-make-x-border-face)))
    (loop for c from 0 to 255 do
	  (aset tetris-faces c
		(cond
		 ((or (= c 0) (= c tetris-border))
		  face2)
		 ((> c 7)
		  'default)
		 (t
		  face))))))

(defun tetris-make-color-x-faces ()
  (loop for c from 0 to 255 do
	(aset tetris-faces c
	      (cond
	       ((= c tetris-border)
		(tetris-make-x-border-face))
	       ((> c 7)
		'default)
	       (t
		(tetris-make-color-x-face c))))))

(defun tetris-make-glyph (index)
  (make-glyph
   (vector
    'xpm
    :data tetris-xpm
    :color-symbols (list
		    (cons "col1" (tetris-color index 0.6))
		    (cons "col2" (tetris-color index 0.8))
		    (cons "col3" (tetris-color index 1.0))))))

(defun tetris-make-display-table ()
  (setq tetris-display-table (make-display-table))
  (aset tetris-display-table tetris-space tetris-space-char)
  (case tetris-display-mode
    ('glyph
     (aset tetris-display-table tetris-border (tetris-make-glyph 8))
     (aset tetris-display-table tetris-blank (tetris-make-glyph 0)))
    (otherwise
     (aset tetris-display-table tetris-border tetris-border-char)
     (aset tetris-display-table tetris-blank tetris-space-char)))
  (loop for i from 1 to 7 do
	(aset tetris-display-table
	      (+ tetris-blank i)
	      (case tetris-display-mode
		('glyph
		 (tetris-make-glyph i))
		('emacs-tty
		 tetris-emacs-block-char)
		(otherwise
		 tetris-block-char)))))

(defun tetris-color-display-p ()
  (if (fboundp 'device-class)
      (eq (device-class (selected-device)) 'color)
    (eq (cdr-safe (assq 'display-type (frame-parameters))) 'color)))

(defun tetris-display-type ()
  (cond ((and tetris-use-glyphs (eq window-system 'x) (featurep 'xpm))
	 'glyph)
	((and tetris-use-color (eq window-system 'x) (tetris-color-display-p))
	 'color-x)
	((eq window-system 'x)
	 'mono-x)
	((and tetris-use-color (tetris-color-display-p))
	 'color-tty)
	(t
	 (if (fboundp 'set-face-property)
	     'mono-tty
	   'emacs-tty))))

(defun tetris-initialize-display ()
  (setq tetris-display-mode (tetris-display-type))
  (tetris-make-display-table)
  (case tetris-display-mode
    ('mono-tty
     (tetris-make-mono-tty-faces))
    ('color-tty
     (tetris-make-color-tty-faces))
    ('mono-x
     (tetris-make-mono-x-faces))
    ('color-x
     (tetris-make-color-x-faces))))

(defun tetris-set-display-table ()
  (if (fboundp 'specifierp)
      (add-spec-to-specifier current-display-table
			     tetris-display-table
			     (current-buffer)
			     nil 'remove-locale)
    (setq buffer-display-table tetris-display-table)))

(defun tetris-hide-cursor ()
  (if (fboundp 'specifierp)
      (set-specifier text-cursor-visible-p nil (current-buffer))))

(defun tetris-draw-border-p ()
  (or (not (eq tetris-display-mode 'glyph))
      tetris-draw-border-with-glyphs))

(defun tetris-set-color (c)
  (unless (eq tetris-display-mode 'glyph)
    (put-text-property
     (1- (point)) (point) 'face (aref tetris-faces c))))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-get-tick-period ()
  (if (boundp 'tetris-update-speed-function)
      (let ((period (apply tetris-update-speed-function
			   tetris-n-shapes
			   tetris-n-rows nil)))
	(and (numberp period) period))))

(defun tetris-cell-offset (x y)
  (+ tetris-buffer-start
     (* (1+ tetris-buffer-width) y)
     x))

(defun tetris-get-cell (x y)
  (char-after (tetris-cell-offset x y)))

(defun tetris-set-cell (x y c)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (tetris-cell-offset x y))
      (delete-char 1)
      (insert-char c 1)
      (tetris-set-color c))))

(defun tetris-get-shape-cell (x y)
  (aref
   (aref
    (aref
     (aref tetris-shapes tetris-shape)
     y)
    tetris-rot)
   x))

(defun tetris-shape-width ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(% tetris-rot 2)))

(defun tetris-shape-height ()
  (aref (aref tetris-shape-dimensions tetris-shape)
	(- 1 (% tetris-rot 2))))

(defun tetris-draw-score ()
  (let ((strings (vector
		  (format "Shapes: %05d" tetris-n-shapes)
		  (format "Rows:   %05d" tetris-n-rows))))
    (loop for y from 0 to 1 do
	  (let* ((string (aref strings y))
		 (len (length string)))
	    (loop for x from 0 to (1- len) do
		  (tetris-set-cell
		   (+ tetris-score-x x)
		   (+ tetris-score-y y)
		   (aref string x)))))))

(defun tetris-update-score ()
  (tetris-draw-score)
  (let ((period (tetris-get-tick-period)))
    (if period (tetris-set-timer period))))

(defun tetris-new-shape ()
  (setq tetris-shape tetris-next-shape)
  (setq tetris-rot 0)
  (setq tetris-next-shape (random 7))
  (setq tetris-pos-x (random (- tetris-width (tetris-shape-width))))
  (setq tetris-pos-y 0)
  (setq tetris-n-shapes (1+ tetris-n-shapes))
  (tetris-draw-next-shape)
  (tetris-update-score))

(defun tetris-draw-next-shape ()
  (loop for y from 0 to 3 do
	(loop for x from 0 to 3 do
	      (tetris-set-cell
	       (+ tetris-next-x x)
	       (+ tetris-next-y y)
	       (let ((tetris-shape tetris-next-shape)
		     (tetris-rot 0))
		 (tetris-get-shape-cell x y))))))

(defun tetris-draw-shape ()
  (loop for y from 0 to (1- (tetris-shape-height)) do
	(loop for x from 0 to (1- (tetris-shape-width)) do
	      (let ((c (tetris-get-shape-cell x y)))
		(if (/= c tetris-blank)
		    (tetris-set-cell
		     (+ tetris-top-left-x tetris-pos-x x)
		     (+ tetris-top-left-y tetris-pos-y y)
		     c))))))

(defun tetris-erase-shape ()
  (loop for y from 0 to (1- (tetris-shape-height)) do
	(loop for x from 0 to (1- (tetris-shape-width)) do
	      (let ((c (tetris-get-shape-cell x y)))
		(if (/= c tetris-blank)
		    (tetris-set-cell
		     (+ tetris-top-left-x tetris-pos-x x)
		     (+ tetris-top-left-y tetris-pos-y y)
		     tetris-blank))))))

(defun tetris-test-shape ()
  (let ((hit nil))
    (loop for y from 0 to (1- (tetris-shape-height)) do
	  (loop for x from 0 to (1- (tetris-shape-width)) do
		(unless hit
		  (setq hit
			(let ((c (tetris-get-shape-cell x y))
			      (xx (+ tetris-pos-x x))
			      (yy (+ tetris-pos-y y)))
			  (and (/= c tetris-blank)
			       (or (>= xx tetris-width)
				   (>= yy tetris-height)
				   (/= (tetris-get-cell
					(+ tetris-top-left-x xx)
					(+ tetris-top-left-y yy))
				       tetris-blank))))))))
    hit))

(defun tetris-full-row (y)
  (let ((full t))
    (loop for x from 0 to (1- tetris-width) do
	  (if (= (tetris-get-cell
		  (+ tetris-top-left-x x)
		  (+ tetris-top-left-y y))
		 tetris-blank)
	      (setq full nil)))
    full))

(defun tetris-shift-row (y)
  (loop for x from 0 to (1- tetris-width) do
	(let ((c (tetris-get-cell
		 (+ tetris-top-left-x x)
		 (+ tetris-top-left-y y -1))))
	  (tetris-set-cell
	   (+ tetris-top-left-x x)
	   (+ tetris-top-left-y y)
	   c))))

(defun tetris-shift-down ()
  (loop for y0 from 0 to (1- tetris-height) do
	(if (tetris-full-row y0)
	    (progn
	      (setq tetris-n-rows (1+ tetris-n-rows))
	      (tetris-update-score)
	      (loop for y from y0 downto 1 do
		    (tetris-shift-row y))))))

(defun tetris-init-buffer ()
  (let ((line (concat
	       (make-string tetris-buffer-width tetris-space)
	       "\n"))
	(buffer-read-only nil))
    (erase-buffer)
    (setq tetris-buffer-start (point))
    (dotimes (i tetris-buffer-height)
      (insert-string line))
    (goto-char (point-min))
    (if (tetris-draw-border-p)
      (loop for y from -1 to tetris-height do
	    (loop for x from -1 to tetris-width do
		  (tetris-set-cell
		   (+ tetris-top-left-x x)
		   (+ tetris-top-left-y y)
		   tetris-border))))
    (loop for y from 0 to (1- tetris-height) do
	  (loop for x from 0 to (1- tetris-width) do
		(tetris-set-cell
		 (+ tetris-top-left-x x)
		 (+ tetris-top-left-y y)
		 tetris-blank)))
    (if (tetris-draw-border-p)
      (loop for y from -1 to 4 do
	    (loop for x from -1 to 4 do
		  (tetris-set-cell
		   (+ tetris-next-x x)
		   (+ tetris-next-y y)
		   tetris-border))))))

(defun tetris-reset-game ()
  (tetris-kill-timer)
  (tetris-init-buffer)
  (setq tetris-next-shape (random 7))
  (setq tetris-shape		0
	tetris-rot		0
	tetris-n-shapes		0
	tetris-n-rows		0
	tetris-pos-x		0
	tetris-pos-y		0)
  (tetris-new-shape)
  (tetris-draw-shape))

(defun tetris-shape-done ()
  (tetris-shift-down)
  (tetris-new-shape)
  (if (tetris-test-shape)
      (progn
	(tetris-end-game))
    (tetris-draw-shape)))

(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (eq (current-buffer) tetris-buffer)
      (let (hit)
	(tetris-erase-shape)
	(setq tetris-pos-y (1+ tetris-pos-y))
	(setq hit (tetris-test-shape))
	(if hit
	    (setq tetris-pos-y (1- tetris-pos-y)))
	(tetris-draw-shape)
	(if hit
	    (tetris-shape-done)))))

(defun tetris-move-bottom ()
  "Drops the shape to the bottom of the playing area"
  (interactive)
  (let ((hit nil))
    (tetris-erase-shape)
    (while (not hit)
      (setq tetris-pos-y (1+ tetris-pos-y))
      (setq hit (tetris-test-shape)))
    (setq tetris-pos-y (1- tetris-pos-y))
    (tetris-draw-shape)
    (tetris-shape-done)))

(defun tetris-move-left ()
  "Moves the shape one square to the left"
  (interactive)
  (unless (= tetris-pos-x 0)
    (tetris-erase-shape)
    (setq tetris-pos-x (1- tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1+ tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-move-right ()
  "Moves the shape one square to the right"
  (interactive)
  (unless (= (+ tetris-pos-x (tetris-shape-width))
	     tetris-width)
    (tetris-erase-shape)
    (setq tetris-pos-x (1+ tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1- tetris-pos-x)))
    (tetris-draw-shape)))

(defun tetris-rotate-prev ()
  "Rotates the shape clockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 1 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 3 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-rotate-next ()
  "Rotates the shape anticlockwise"
  (interactive)
  (tetris-erase-shape)
  (setq tetris-rot (% (+ 3 tetris-rot) 4))
  (if (tetris-test-shape)
      (setq tetris-rot (% (+ 1 tetris-rot) 4)))
  (tetris-draw-shape))

(defun tetris-end-game ()
  "Terminates the current game"
  (interactive)
  (tetris-kill-timer)
  (use-local-map tetris-null-map))

(defun tetris-start-game ()
  "Starts a new game of Tetris"
  (interactive)
  (tetris-reset-game)
  (use-local-map tetris-mode-map)
  (let ((period (or (tetris-get-tick-period)
		    tetris-default-tick-period)))
    (tetris-start-timer period)))

(put 'tetris-mode 'mode-class 'special)

(defun tetris-mode ()
  "A mode for playing Tetris.

tetris-mode keybindings:
   \\{tetris-mode-map}
"
  (kill-all-local-variables)

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'tetris-end-game nil t)

  (make-local-variable 'tetris-display-mode)
  (make-local-variable 'tetris-display-table)
  (make-local-variable 'tetris-faces)
  (make-local-variable 'tetris-timer)
  (make-local-variable 'tetris-buffer-start)
  (make-local-variable 'tetris-shape)
  (make-local-variable 'tetris-rot)
  (make-local-variable 'tetris-next-shape)
  (make-local-variable 'tetris-n-shapes)
  (make-local-variable 'tetris-n-rows)
  (make-local-variable 'tetris-pos-x)
  (make-local-variable 'tetris-pos-y)

  (use-local-map tetris-null-map)

  (setq buffer-read-only t)
  (setq truncate-lines 't)
  (setq major-mode 'tetris-mode)
  (setq mode-name "Tetris")

  (buffer-disable-undo (current-buffer))

  (tetris-initialize-display)
  (tetris-set-display-table)
  (tetris-hide-cursor)

  (run-hooks 'tetris-mode-hook))

;;;###autoload
(defun tetris ()
  "Tetris

Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (switch-to-buffer tetris-buffer-name)
  (tetris-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(provide 'tetris)

;;; tetris.el ends here

