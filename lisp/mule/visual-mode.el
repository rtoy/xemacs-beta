;; visual.el -- cursor motion, insertion, deletion, etc. in visual order
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

;;; 94.5.15  created for Mule Ver.1.1 by Takahashi N. <ntakahas@etl.go.jp>

;;;###autoload
(defvar visual-mode nil "non-nil if in visual-mode.")

(make-variable-buffer-local 'visual-mode)

(defvar visual-use-lr-commands nil
  "If non-nil, use visual-left-* and visual-right-* commands instead of
visual-forward-* and visual-backward-* commands.")

(defvar visual-mode-map
  (let ((map (make-keymap)))
    (substitute-key-definition 'self-insert-command
			       'visual-self-insert-command
			       map global-map)
    ; visual basic commands
    (define-key map [(control d)]       'visual-delete-char)
    (define-key map [(control k)]       'visual-kill-line)
    (define-key map [(control m)]       'visual-newline)
    (define-key map [(control o)]       'visual-open-line)
    (define-key map [(control p)]       'visual-previous-line)
    (define-key map [(control w)]       'visual-kill-region)
    (define-key map [(control y)]       'visual-yank)
    (define-key map [delete]       'visual-backward-delete-char)
    (define-key map [(meta <)]       'visual-beginning-of-buffer)
    (define-key map [(meta >)]       'visual-end-of-buffer)
    (define-key map [(meta d)]       'visual-kill-word)
    (define-key map [(meta w)]       'visual-kill-ring-save)
    (define-key map [(meta y)]       'visual-yank-pop)
    (define-key map [(meta delete)]    'visual-backward-kill-word)
    (define-key map [up]          'visual-previous-line)
    (define-key map [down]        'visual-next-line)
    (define-key map [home]        'visual-beginning-of-buffer)
    (define-key map [end]         'visual-end-of-buffer)
    (define-key map [left]        'visual-move-to-left-char)
    (define-key map [right]       'visual-move-to-right-char)
    (define-key map [(meta left)]      'visual-move-to-left-word)
    (define-key map [(meta right)]     'visual-move-to-right-word)
    (define-key map [(control c) (control c)] 'exit-visual-mode)
    (define-key map [(control c) <]    'l2r-mode)
    (define-key map [(control c) >]    'r2l-mode)
    ; LR commands
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
    map)
  "minor-mode-keymap for visual-mode.")

(if (not (assq 'visual-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'visual-mode visual-mode-map)
		minor-mode-map-alist)))

(defvar visual-mode-indicator nil
  "string displayed in mode line. \" l2r\" or \" r2l\".")
(make-variable-buffer-local 'visual-mode-indicator)

(if (not (assq 'visual-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(visual-mode visual-mode-indicator)
		minor-mode-alist)))

(setq auto-mode-alist
      (append '(("\\.l2r$" . l2r-mode) ("\\.r2l$" . r2l-mode))
	      auto-mode-alist))

(defvar visual-mode-hooks nil)

;;;###autoload
(defun visual-mode (&optional arg)
  "Toggle visual-mode.  With ARG, turn visual-mode on iff ARG is positive."
  (interactive "P")
  (if (null arg)
    (if visual-mode (exit-visual-mode) (enter-visual-mode))
   (if (> (prefix-numeric-value arg) 0)
       (enter-visual-mode)
     (exit-visual-mode))))

(defun enter-visual-mode nil
  "Enter visual-mode.  Cursor moves in visual order."
  (interactive)
  (if (not visual-mode)
      (progn
	(setq visual-mode t
	      visual-mode-indicator (if display-direction " r2l" " l2r"))
	(redraw-display)
	(run-hooks 'visual-mode-hooks))))

(defun exit-visual-mode nil
  "Exit visual-mode.  Cursor moves in logical order."
  (interactive)
  (if visual-mode
      (progn
	(setq visual-mode nil)
	(redraw-modeline t))))

(defun l2r-mode nil
  "Set display-direction left to right."
  (interactive)
  (if (not visual-mode)
      (enter-visual-mode))
  (setq display-direction nil)
  (setq visual-mode-indicator " l2r")
  (redraw-display))

(defun r2l-mode nil
  "Set display-direction right to left."
  (interactive)
  (if (not visual-mode)
      (enter-visual-mode))
  (setq display-direction t)
  (setq visual-mode-indicator " r2l")
  (redraw-display))


;; cursor motion

(defun visual-forward-char (arg)
  "Move the cursor visually forward by ARG (integer) characters.
if ARG is negative, move backward."
  (interactive "p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-backward-1-char)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-forward-1-char)
      (setq arg (1- arg)))))

(defun visual-forward-1-char nil
  "Move the cursor visually forward by 1 character."
  (let ((r-dir (if display-direction 0 1))
	(a-dir (visual-char-direction-after-point))
	(aa-dir (visual-char-direction-after-after-point))
	(b-dir (visual-char-direction-before-point)))

    ;  symbols used in the following comments
    ; ^  : point in here
    ; ~  : point will be there
    ; d  : character whose direction is the same as display-direction
    ; r  : character whose direction is opposite to display-direction
    ; !d : r or nil
    ; !r : d or nil
    ; r* : 0 or more r's
    ; d* : 0 or more d's

    (cond
     ((null a-dir)
      ; ... nil
      ;    ^
      (error "end of buffer"))

     ((eq a-dir r-dir)
      (if (eq b-dir r-dir)

	  ; ... r r ...
	  ;    ~ ^
	  (backward-char 1)

	; ... !r r r* ...
	;       ^    ~
	(skip-direction-forward r-dir)))

     ((eq aa-dir r-dir)
      ; ... d r* r ...
      ;    ^    ~
      (forward-char 1)
      (skip-direction-forward r-dir)
      (backward-char 1))

     (t
      ; ... d !r ...
      ;    ^ ~
      (forward-char 1)))))

(defun visual-backward-char (arg)
  "Move the cursor visually backward by ARG (integer) characters.
if ARG is negative, move forward."
  (interactive "p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-forward-1-char)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-backward-1-char)
      (setq arg (1- arg)))))

(defun visual-backward-1-char nil
  "Move the cursor visually backward by 1 character."
  (let ((r-dir (if display-direction 0 1))
	(a-dir (visual-char-direction-after-point))
	(aa-dir (visual-char-direction-after-after-point))
	(b-dir (visual-char-direction-before-point)))

    ;  symbols used in the following comments
    ; ^  : point in here
    ; ~  : point will be there
    ; d  : character whose direction is the same as display-direction
    ; r  : character whose direction is opposite to display-direction
    ; !d : r or nil
    ; !r : d or nil
    ; r* : 0 or more r's
    ; d* : 0 or more d's

    (cond
     ((eq a-dir r-dir)
      (if (eq aa-dir r-dir)
	  ; ... r r ...
	  ;    ^ ~
	  (forward-char 1)

	; ... !r r* !r ...
	;    ~     ^
	(skip-direction-backward r-dir)
	(if (visual-char-direction-before-point)
	    (backward-char 1)
	  (skip-direction-forward r-dir)
	  (backward-char 1)
	  (error "beginning of buffer"))))

     ((null b-dir)
      ; nil !r ...
      ;    ^
      (error "beginning of buffer"))

     ((eq b-dir r-dir)
      ; ... r* r !r
      ;    ~    ^
      (skip-direction-backward r-dir))

     (t
      ; ... d !r ...
      ;    ~ ^
      (backward-char 1)))))

(defun visual-char-direction (ch)
  "Return the direction of CH (character).
Newline's direction will be same as display-direction."
  (cond
   ((null ch) nil)
   ((= ch ?\n) (if display-direction 1 0))
   (t (char-direction ch))))

(defun visual-char-direction-after-point nil
  "Return the direction of after-point-character.
0: left-to-right, 1: right-to-left"
  (visual-char-direction (char-after (point))))

(defun visual-char-direction-after-after-point nil
  "Return the direction of after-after-point-character.
0: left-to-right, 1: right-to-left"
  (if (= (point) (point-max))
      nil
    (save-excursion
      (forward-char 1)
      (visual-char-direction (char-after (point))))))

(defun visual-char-direction-before-point nil
  "Return the direction of before-point-character.
0: left-to-right, 1: right-to-left"
  (visual-char-direction (char-before (point))))

(defun skip-direction-forward (dir)
  "Move point forward as long as DIR-direction characters continue."
  (while (eq (visual-char-direction-after-point) dir)
    (forward-char 1)))

(defun skip-direction-backward (dir)
  "Move point backward as long as DIR-direction characters continue."
  (while (eq (visual-char-direction-before-point) dir)
    (backward-char 1)))

(defvar *visual-punctuations*
  '(?  ?. ?, ?: ?; ?? ?! ?- ?_ ?' ?\" ?/ ?( ?) ?[ ?] ?{ ?} ?\n ?\t ; ASCII
    ?  ?. ?, ?: ?; ?? ?! ?- ?_ ?' ?" ?( ?) ?[ ?]		   ; Hebrew
    ?›2](3!›0](B ?›2](3&›0](B ?›2](3%›0](B ?›2](3)›0](B ?›2](3"›0](B ?›2](3'›0](B ?›2](3(›0](B ?›2](3#›0](B ?›2](3$›0](B ?›2](3*›0](B ?›2](3+›0](B ))				   ; Arabic

(defun visual-forward-word (arg)
  "Move the cursor visually forward by ARG (integer) words.
If ARG is negative, move the cursor backward."
  (interactive "p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-backward-1-word)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-forward-1-word)
      (setq arg (1- arg)))))

(defun visual-backward-word (arg)
  "Move the cursor visually backward by ARG (integer) words.
If ARG is negative, move the cursor forward."
  (interactive "p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-forward-1-word)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-backward-1-word)
      (setq arg (1- arg)))))

(defun visual-forward-1-word nil
  "Move the cursor visually forward by one word."
  (while (memq (visual-char-after) *visual-punctuations*)
    (visual-forward-1-char))
  (while (not (memq (visual-char-after) *visual-punctuations*))
    (visual-forward-1-char)))

(defun visual-backward-1-word nil
  "Move the cursor visually backward by one word."
  (while (memq (visual-char-before) *visual-punctuations*)
    (visual-backward-1-char))
  (while (not (memq (visual-char-before) *visual-punctuations*))
    (visual-backward-1-char)))

(defun visual-char-before nil
  "Return the character visually before the cursor.
If such position is out of range, returns nil."
  ; almost same as visual-backward-1-char
  (save-excursion			
    (let ((r-dir (if display-direction 0 1))
	  (a-dir (visual-char-direction-after-point))
	  (aa-dir (visual-char-direction-after-after-point))
	  (b-dir (visual-char-direction-before-point)))
      (cond
       ((eq a-dir r-dir)
	(if (eq aa-dir r-dir)
	    (progn
	      (forward-char 1)
	      (char-after (point)))
	  (skip-direction-backward r-dir)
	  (if (visual-char-direction-before-point)
	      (progn
		(backward-char 1)
		(char-after (point)))
	    nil)))
       ((null b-dir)
	nil)
       ((eq b-dir r-dir)
	(skip-direction-backward r-dir)
	(char-after (point)))
       (t
	(backward-char 1)
	(char-after (point)))))))

(defun visual-char-after nil
  "Return the character under the cursor.
If such position is out of range, returns nil."
  (char-after (point)))

(defun visual-beginning-of-line (&optional arg)
  "Move the cursor to the visual beginning of line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (beginning-of-line arg)
  (let ((a-dir (visual-char-direction-after-point))
	(d-dir (if display-direction 1 0)))
    (if (and a-dir (/= a-dir d-dir))
	(progn (skip-direction-forward a-dir)
	       (backward-char 1)))))

(fset 'visual-end-of-line 'end-of-line)

(defun visual-beginning-of-buffer nil
  "Move the cursor to the visual beginning of current buffer."
  (interactive)
  (beginning-of-buffer)
  (visual-beginning-of-line))

(fset 'visual-end-of-buffer 'end-of-buffer)

(defvar visual-temporary-goal-column 0
  "temporary-goal-column command for visual-mode.")

(defun visual-next-line (arg)
  "next-line command for visual-mode."
  (interactive "p")
  (if (and (not (eq last-command 'visual-next-line))
	   (not (eq last-command 'visual-previous-line)))
      (setq visual-temporary-goal-column (visual-current-column)))
  (next-line arg)
  (visual-goto-column visual-temporary-goal-column))

(defun visual-previous-line (arg)
  "previous-line command for visual-mode."
  (interactive "p")
  (if (and (not (eq last-command 'visual-next-line))
	   (not (eq last-command 'visual-previous-line)))
      (setq visual-temporary-goal-column (visual-current-column)))
  (previous-line arg)
  (visual-goto-column visual-temporary-goal-column))

(defun visual-current-column nil
  "Return the current column counted in visual order."
  (let ((c 0) (p (point)))
    (visual-beginning-of-line)
    (while (/= (point) p)
      (setq c (+ c (char-width (visual-char-after))))
      (visual-forward-1-char))
    c))

(defun visual-goto-column (col)
  "Move the cursor to visual column N (integer) in the current line.
If it is impossible to go to column N, the cursor is put on the nearest column
M (M < N).  Returns N - M."
  (if (< col 0)
      (error "argument must be positive."))
  (let ((c 0))
    (visual-beginning-of-line)
    (while (and (< c col) (not (eolp)))
      (setq c (+ c (char-width (visual-char-after))))
      (visual-forward-1-char))
    (if (> c col)
	(progn
	  (visual-backward-1-char)
	  (setq c (- c (char-width (visual-char-after))))))
    (- col c)))


;; insertion

(defun visual-insert-char (ch arg)
  "Insert character CH visually before the cursor.
With ARG (integer) insert that many characters."
  (if (< arg 0)
      (error "arg must be >= 0."))
  (while (> arg 0)
    (visual-insert-1-char ch)
    (setq arg (1- arg))))

(defun visual-insert-1-char (ch)
  "Insert character CH visually before the cursor.
The cursor moves visually forward."
  (let ((c-dir (visual-char-direction ch))
	(r-dir (if display-direction 0 1))
	(a-dir (visual-char-direction-after-point))
	(tmp))

    ;  symbols used in the following comments
    ; d  : character whose direction is the same as display-direction
    ; r  : character whose direction is opposite to display-direction
    ; !d : r or nil
    ; !r : d or nil
    ; ^d : point is here and the character to be inserted is d
    ; ^r : point is here and the character to be inserted is d

    (if (eq c-dir r-dir)
	(if (eq a-dir r-dir)
	    
	    ; ... r ...
	    ;    ^r
	    (progn
	      (forward-char 1)
	      (insert ch)
	      (backward-char 2))
	  
	  ; ... !r ...
	  ;    ^r
	  (skip-direction-backward c-dir)
	  (insert ch)
	  (skip-direction-forward c-dir))
      
      (if (or (eq a-dir nil)
	      (eq a-dir c-dir))
	  
	  ; ... !r ...
	  ;    ^d
	  (insert ch)
	
	; ... r ...
	;    ^d
	(forward-char 1)
	(setq tmp (delete-direction-backward r-dir))
	(skip-direction-forward r-dir)
	(insert ch tmp)
	(backward-char 1)))))

(defun delete-direction-forward (dir)
  "From current point, delete DIR-direction characters forward.
Returns the deleted string."
  (let ((p (point)))
    (skip-direction-forward dir)
    (prog1
      (buffer-substring (point) p)
      (delete-region (point) p))))

(defun delete-direction-backward (dir)
  "From current point, delete DIR-direction characters backward.
Return the deleted string."
  (let ((p (point)))
    (skip-direction-backward dir)
    (prog1
      (buffer-substring (point) p)
      (delete-region (point) p))))

(defun visual-self-insert-command (arg)
  "Insert this character (32 <= CH < 127).
With ARG (integer), insert that many characters.
If display-direction is non-nil, the cursor stays at the same position."
  (interactive "*p")
  (visual-insert-char last-command-char arg)
  (if display-direction
      (visual-backward-char arg)))

;; wire us into pending-delete
(put 'visual-self-insert-command 'pending-delete t)

(defun visual-newline (arg)
  "newline command for visual-mode.
With ARG (integer), insert that many newlines."
  (interactive "*p")
  (visual-insert-char ?\n arg))

(defun visual-open-line (arg)
  "open-line command for visual-mode.
With arg (integer), insert that many newlines."
  (interactive "*p")
  (visual-insert-char ?\n arg)
  (visual-backward-char arg))


;; deletion

(defun visual-delete-char (arg)
  "Delete ARG (integer) characters visually forward.
If ARG is negative, delete backward."
  (interactive "*p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-backward-delete-1-char)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-delete-1-char)
      (setq arg (1- arg)))))

(defun visual-backward-delete-char (arg)
  "Delete ARG (integer) characters visually backward.
If arg is negative, delete forward."
  (interactive "*p")
  (if (< arg 0)
      (while (< arg 0)
	(visual-delete-1-char)
	(setq arg (1+ arg)))
    (while (> arg 0)
      (visual-backward-delete-1-char)
      (setq arg (1- arg)))))

(fset 'visual-delete-backward-char 'visual-backward-delete-char)

(defun visual-backward-delete-1-char nil
  "Delete a character visually before the cursor.
Ther cursor moves visually backward."
  (let ((d-dir (if display-direction 1 0))
	(r-dir (if display-direction 0 1))
	(a-dir (visual-char-direction-after-point))
	(aa-dir (visual-char-direction-after-after-point))
	(b-dir (visual-char-direction-before-point))
	(tmp))

    ;  symbols used in the following comments
    ; ^  : point in here
    ; d  : character whose direction is the same as display-direction
    ; r  : character whose direction is opposite to display-direction
    ; !d : r or nil
    ; !r : d or nil
    ; r* : 0 or more r's
    ; d* : 0 or more d's

    (if (eq a-dir r-dir)
	(cond
	 ((eq aa-dir r-dir)
	  ; ... r r ...
	  ;    ^
	  (forward-char 1)
	  (delete-char 1)
	  (backward-char 1))

	 ((save-excursion
	    (skip-direction-backward r-dir)
	    (backward-char 1)
	    (and (eq (visual-char-direction-after-point) d-dir)
		 (eq (visual-char-direction-before-point) r-dir)))
	  ; ... r d r* r !r ...
	  ;           ^
	  (forward-char 1)
	  (setq tmp (delete-direction-backward r-dir))
	  (delete-backward-char 1)
	  (skip-direction-backward r-dir)
	  (insert tmp)
	  (backward-char 1))

	 (t
	  ; .....!r d r* r !r ...
	  ;             ^
	  (skip-direction-backward r-dir)
	  (delete-backward-char 1)
	  (skip-direction-forward r-dir)
	  (backward-char 1)))

      (cond
       ((null b-dir)
	; nil !r ...
	;    ^
	(error "beginning of buffer"))

       ((eq b-dir r-dir)
	; ... r !r ...
	;      ^
	(skip-direction-backward r-dir)
	(delete-char 1)
	(skip-direction-forward r-dir))

       (t
	; ... !r !r ...
	;       ^
	(delete-backward-char 1))))))

(fset 'visual-delete-backward-1-char 'visual-backward-delete-1-char)

(defun visual-delete-1-char nil
  "Delete a character under the cursor.
Visually, the cursor stays at the same position."
  (let ((d-dir (if display-direction 1 0))
	(r-dir (if display-direction 0 1))
	(a-dir (visual-char-direction-after-point))
	(aa-dir (visual-char-direction-after-after-point))
	(b-dir (visual-char-direction-before-point))
	(tmp))

    ;  symbols used in the following comments
    ; ^  : point in here
    ; d  : character whose direction is the same as display-direction
    ; r  : character whose direction is opposite to display-direction
    ; !d : r or nil
    ; !r : d or nil
    ; r* : 0 or more r's
    ; d* : 0 or more d's

    (cond
     ((null a-dir)
      ; ... nil
      ;    ^
      (error "end of buffer"))

     ((eq a-dir r-dir)
      (if (eq b-dir r-dir)

	  ; ... r r ...
	  ;      ^
	  (progn (delete-char 1)
		 (backward-char 1))

	; ... !r r ...
	;       ^
	(delete-char 1)
	(skip-direction-forward r-dir)))

     ((not (eq aa-dir r-dir))
      ; ... d !r ...
      ;    ^
      (delete-char 1))

     ((eq b-dir r-dir)
      ; ... r d r ...
      ;      ^
      (delete-char 1)
      (setq tmp (delete-direction-forward r-dir))
      (skip-direction-backward r-dir)
      (insert tmp)
      (backward-char 1))

     (t
      ; ...!r d r ...
      ;      ^
      (delete-char 1)
      (skip-direction-forward r-dir)
      (backward-char 1)))))

(defun visual-delete-region (beg end)
  "delete-region command for visual-mode."
  (interactive "*r")
  (let ((begl) (begc) (endl) (endc) (l))

    ; swap beg & end if necessary
    (goto-char beg)
    (setq begl (current-line)
	  begc (visual-current-column))
    (goto-char end)
    (setq endl (current-line)
	  endc (visual-current-column))
    (if (or (> begl endl)
	    (and (= begl endl)
		 (> begc endc)))
	(progn
	  (setq beg (prog1 end (setq end beg))
		begl (prog1 endl (setq endl begl))
		begc (prog1 endc (setq endc begc)))
	  (goto-char end)))

    ; insert a newline visually at END
    (visual-insert-1-char ?\n)
    (visual-backward-1-char)
    (setq l (current-line))

    ; insert a newline visually at BEG
    (goto-line begl)
    (visual-goto-column begc)
    (visual-insert-1-char ?\n)
    (beginning-of-line)

    (delete-region
     (point)
     (progn
       (goto-line (1+ l))
       (end-of-line)
       (point)))
    (backward-char 1)
    (visual-delete-char 2)))

(defun current-line nil
  "Return the current line number (in the buffer) of point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))


;; kill

(defun visual-kill-region (beg end)
  "kill-region command for visual-mode."
  (interactive "r")
  (let ((begl) (begc) (endl) (endc) (l))

    ; swap beg & end if necessary
    (goto-char beg)
    (setq begl (current-line)
	  begc (visual-current-column))
    (goto-char end)
    (setq endl (current-line)
	  endc (visual-current-column))
    (if (or (> begl endl)
	    (and (= begl endl) (> begc endc)))
	(progn
	  (setq beg (prog1 end (setq end beg))
		begl (prog1 endl (setq endl begl))
		begc (prog1 endc (setq endc begc)))
	  (goto-char end)))

  (if (or (and buffer-read-only (not inhibit-read-only))
	  (text-property-not-all beg end 'read-only nil))
      (progn
	(visual-copy-region-as-kill beg end)
	(if kill-read-only-ok
	    (message "Read only text copied to kill ring")
	  (barf-if-buffer-read-only)))

    ; insert a newline visually at END
    (visual-insert-1-char ?\n)
    (visual-backward-1-char)
    (setq l (current-line))

    ; insert a newline visually at BEG
    (goto-line begl)
    (visual-goto-column begc)
    (visual-insert-1-char ?\n)
    (beginning-of-line)

    (kill-region
     (point)
     (progn
       (goto-line (1+ l))
       (end-of-line)
       (point)))
    (backward-char 1)
    (visual-delete-char 2)))

  (setq this-command 'kill-region))
  
(defun visual-kill-word (arg)
  "Kill ARG (integer) words visually forward.
If ARG is negative, kill backward."
  (interactive "*p")
  (visual-kill-region
   (point)
   (progn
     (visual-forward-word arg)
     (point))))

(defun visual-backward-kill-word (arg)
  "Kill ARG (integer) words visually backward.
If ARG is negative, kill forward."
  (interactive "*p")
  (visual-kill-region
   (point)
   (progn
     (visual-backward-word arg)
     (point))))

(defun visual-kill-line (&optional arg)
  "kill-line command for visual-mode."
  (interactive "*P")
  (visual-kill-region
   (point)
   (progn
     (if arg
	 (progn
	   (forward-line (prefix-numeric-value arg))
	   (visual-beginning-of-line))
       (if (eobp)
	   (signal 'end-of-buffer nil))
       (if (not (eolp))
	   (visual-end-of-line)
	 (forward-line 1)
	 (visual-beginning-of-line)))
     (point))))

(defun visual-copy-region-as-kill (beg end)
  "copy-region-as-kill command for visual-mode."
  (interactive "r")
  (let ((buffer-read-only nil)
	(auto-save-mode 0)
	(p (point)))
    (visual-kill-region beg end)
    (visual-yank 1)
    (if (/= (point) p)
	(exchange-point-and-mark)))
  nil)

(defun visual-kill-ring-save (beg end)
  "kill-ring-save command for visual-mode."
  (interactive "r")
  (visual-copy-region-as-kill beg end)
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    (inhibit-quit t))
	(if (pos-visible-in-window-p other-end (selected-window))
	    (progn
	      (set-marker (mark-marker) (point) (current-buffer))
	      (goto-char other-end)
	      (sit-for 1)
	      (set-marker (mark-marker) other-end (current-buffer))
	      (goto-char opoint)
	      (and quit-flag mark-active
		   (deactivate-mark)))
	  (let* ((killed-text (current-kill 0))
		 (message-len (min (length killed-text) 40)))
	    (if (= (point) beg)
		(message "Saved text until \"%s\""
			(substring killed-text (- message-len)))
	      (message "Saved text from \"%s\""
		      (substring killed-text 0 message-len))))))))
      

;; yank

(defun visual-yank (&optional arg)
  "yank command for visual-mode."
  (interactive "*P")
  (setq this-command t)

  (let ((l1 (current-line)) (c1 (visual-current-column)) l2 c2)

    ;; Insert a newline both before and after current point.
    (visual-insert-char ?\n 2)
    (visual-backward-1-char)

    ;; Reinsert killed string between the two newlines.
    (insert (current-kill (cond
			   ((listp arg) 0)
			   ((eq arg '-) -1)
			   (t (1- arg)))))

    ;; Delete the latter newline visually.
    (visual-delete-1-char)
    (setq l2 (current-line)
	  c2 (visual-current-column))

    ;; Delete the former newline visually.
    (goto-line l1)
    (end-of-line)
    (visual-delete-1-char)
    (push-mark (point))

    ;; Go back to the end of yanked string.
    (if (= (- l2 l1) 1)
	(visual-goto-column (+ c1 c2))
      (goto-line (1- l2))
      (visual-goto-column c2))

    ;; Exchange point and mark if necessary.
    (if (consp arg)
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))))

  (setq this-command 'yank)
  nil)

(defun visual-yank-pop (arg)
  "yank-pop command for visual-mode."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let (l1 c1 l2 c2 before)

    (save-excursion
      (setq l2 (current-line)
	    c2 (visual-current-column))
      (goto-char (mark t))
      (setq l1 (current-line)
	    c1 (visual-current-column))
      (if (or (> l1 l2)
	      (and (= l1 l2) (> c1 c2)))
	  (setq before t)))

    (visual-delete-region (point) (mark t))
    (setq l1 (current-line)
	  c1 (visual-current-column))

    ;; Insert a newline both before and after current point.
    (visual-insert-char ?\n 2)
    (visual-backward-1-char)

    ;; Reinsert killed string between the two newlines.
    (insert (current-kill arg))

    ;; Delete the latter newline visually.
    (visual-delete-1-char)
    (setq l2 (current-line)
	  c2 (visual-current-column))

    ;; Delete the former newline visually.
    (goto-line l1)
    (end-of-line)
    (visual-delete-1-char)
    (set-marker (mark-marker) (point) (current-buffer))

    ;; Go back to the end of yanked string.
    (if (= (- l2 l1) 1)
	(visual-goto-column (+ c1 c2))
      (goto-line (1- l2))
      (visual-goto-column c2))

    ;; Exchange point and mark if necessary.
    (if before
	(goto-char (prog1 (mark t)
		     (set-marker (mark-marker) (point) (current-buffer))))))

  nil)


;; misc

(defun visual-reverse-direction-word nil
  "Reverse the char order of the word before point."
  (interactive "*")
  (goto-char
    (prog1
      (point)
      (reverse-region
       (point)
       (progn (skip-direction-backward (visual-char-direction-before-point))
	      (point))))))

(defun visual-reverse-region (begin end)
  "Reverse the order of chars between BEGIN and END."
  (interactive "*r")
  (apply 'insert
	 (nreverse
	  (string-to-char-list
	   (prog1 (buffer-substring begin end) (delete-region begin end))))))


;; LR commands

(defun visual-char-left nil
  "Return the character on the left of visual point."
  (if display-direction
      (visual-char-after)
    (visual-char-before)))

(defun visual-char-right nil
  "Return the character on the right of visual point."
  (if display-direction
      (visual-char-before)
    (visual-char-after)))

(defun visual-move-to-left-char (arg)
  "Move the cursor visually left by ARG (integer) characters.
If ARG is negative, move the cursor right."
  (interactive "p")
  (if display-direction
      (visual-forward-char arg)
    (visual-backward-char arg)))

(defun visual-move-to-left-1-char nil
  "Move the cursor visually left by 1 character."
  (interactive "p")
  (if display-direction
      (visual-forward-1-char)
    (visual-backward-1-char)))

(defun visual-move-to-right-char (arg)
  "Move the cursor visually right by ARG (integer) characters.
If ARG is negative, move the cursor left."
  (interactive "p")
  (if display-direction
      (visual-backward-char arg)
    (visual-forward-char arg)))

(defun visual-move-to-right-1-char nil
  "Move the cursor visually right by 1 character."
  (interactive "p")
  (if display-direction
      (visual-backward-1-char)
    (visual-forward-1-char)))

(defun visual-move-to-left-word (arg)
  "Move the cursor visually left by ARG (integer) words.
If ARG is negative, move the cursor right."
  (interactive "p")
  (if display-direction
      (visual-forward-word arg)
    (visual-backward-word arg)))

(defun visual-move-to-right-word (arg)
  "Move the cursor visually right by ARG (integer) words.
If ARG is negative, move the cursor left."
  (interactive "p")
  (if display-direction
      (visual-backward-word arg)
    (visual-forward-word arg)))

(defun visual-left-end-of-line (arg)
  "Move the line cursor to the left-end of line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if display-direction
      (visual-end-of-line arg)
    (visual-beginning-of-line arg)))

(defun visual-right-end-of-line (arg)
  "Move the line cursor to the right-end of line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if display-direction
      (visual-beginning-of-line arg)
    (visual-end-of-line arg)))

(defun visual-insert-char-left (ch arg)
  "Insert CH (character) on the left of visual point as many as
ARG (integer)."
  (if (< arg 0)
      (error "ARG must be >= 0."))
  (visual-insert-char ch arg)
  (and display-direction
       (visual-backward-char arg)))

(defun visual-insert-left-1-char (ch)
  "Insert CH (character) on the left of visual point."
  (visual-insert-1-char ch)
  (and display-direction
       (visual-backward-1-char)))

(defun visual-insert-char-right (ch arg)
  "Insert CH (character) on the right of visual point as many as
ARG (integer)."
  (if (< arg 0)
      (error "ARG must be >= 0."))
  (visual-insert-char ch arg)
  (or display-direction
      (visual-backward-char arg)))

(defun visual-insert-right-1-char (ch)
  "Insert CH (character) on the right of visual point."
  (visual-insert-1-char ch)
  (or display-direction
      (visual-backward-1-char)))

(defun visual-delete-left-char (arg)
  "Delete ARG (integer) characters on the left of visual point.
If ARG is negative, on the right."
  (interactive "*p")
  (if display-direction
      (visual-delete-char arg)
    (visual-backward-delete-char arg)))

(defun visual-delete-left-1-char nil
  "Delete 1 character on the left of visual point."
  (interactive "*p")
  (if display-direction
      (visual-delete-1-char)
    (visual-backward-delete-1-char)))

(defun visual-delete-right-char (arg)
  "Delete ARG (integer) characters on the right of visual point.
If ARG is negative, on the left."
  (interactive "*p")
  (if display-direction
      (visual-backward-delete-char arg)
    (visual-delete-char arg)))

(defun visual-delete-right-1-char nil
  "Delete 1 character on the right of visual point."
  (interactive "*p")
  (if display-direction
      (visual-backward-delete-1-char)
    (visual-delete-1-char)))

(defmacro visual-replace-left-1-char (ch)
  (list
   'progn
   '(visual-delete-left-1-char)
   (list 'visual-insert-left-1-char ch)))

(defmacro visual-replace-right-1-char (ch)
  (list
   'progn
   '(visual-delete-right-1-char)
   (list 'visual-insert-right-1-char ch)))

(defun visual-kill-left-word (arg)
  "Kill ARG (integer) words on the left of visual pointer.
If ARG is negative, kill on the right."
  (interactive "*p")
  (if display-direction
      (visual-kill-word arg)
    (visual-backward-kill-word arg)))

(defun visual-kill-right-word (arg)
  "Kill ARG (integer) words on the right of visual point.
If ARG is negative, kill on the left."
  (interactive "*p")
  (if display-direction
      (visual-backward-kill-word arg)
    (visual-kill-word arg)))

;;;
(provide 'visual-mode)
