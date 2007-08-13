;;; strokes.el	Sat May 24 14:18:08 1997

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@mit.edu>
;; Maintainer: David Bakhash <cadet@mit.edu>
;; Version: 2.3-beta
;; Created: 12 April 1997
;; Keywords: lisp, mouse, extensions

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This package is written for for XEmacs v19.14 and up.
;; This is the strokes package.  It is intended to allow the user to
;; control XEmacs by means of mouse strokes.  Once strokes is loaded, you
;; can always get help be invoking `strokes-help':

;; > M-x strokes-help

;; and you can learn how to use the package.  A mouse stroke, for now,
;; can be defined as holding the middle button, for instance, and then
;; moving the mouse in whatever pattern you wish, which you have set
;; XEmacs to understand as mapping to a given command.  For example, you
;; may wish the have a mouse stroke that looks like a capital `C' which
;; means `copy-region-as-kill'.  Treat strokes just like you do key
;; bindings.  For example, XEmacs sets key bindings globally with the
;; `global-set-key' command.  Likewise, you can do

;; > M-x global-set-stroke

;; to interactively program in a stroke.  It would be wise to set the
;; first one to this very command, so that from then on, you invoke
;; `global-set-stroke' with a stroke.  likewise, there may eventually
;; be a `local-set-stroke' command, also analogous to `local-set-key'.

;; You can always unset the last stroke definition with the command

;; > M-x strokes-unset-last-stroke

;; and the last stroke that was added to `strokes-global-map' will be
;; removed.

;; Other analogies between strokes and key bindings are as follows:

;;    1) To describe a stroke binding, you can type

;;       > M-x describe-stroke

;;       analogous to `describe-key'.  It's also wise to have a stroke,
;;       like an `h', for help, or a `?', mapped to `describe-stroke'.

;;    2) stroke bindings are set internally through the lisp function
;;       `define-stroke', similar to the `define-key' function.  some
;;       examples for a 3x3 stroke grid would be

;;       (define-stroke c-mode-stroke-map
;;                      '((0 . 0) (1 . 1) (2 . 2))
;;                      'kill-region)
;;       (define-stroke strokes-global-map
;;                      '((0 . 0) (0 . 1) (0 . 2) (1 . 2) (2 . 2))
;;                      'list-buffers)

;;       however, if you would probably just have the user enter in the
;;       stroke interactively and then set the stroke to whatever he/she
;;       entered. The lisp function to interactively read a stroke is
;;       `strokes-read-stroke'.  This is especially helpful when you're
;;       on a fast computer that can handle a 9x9 stroke grid.

;;       NOTE: only global stroke bindings are currently implemented,
;;       however mode- and buffer-local stroke bindings may eventually
;;       be implemented in a future version.

;; The important variables to be aware of for this package are listed
;; below.  They can all be altered through the customizing package via

;; > M-x customize

;; and customizing the group named `strokes'.  You can also read
;; documentation on the variables there.

;; `strokes-minimum-match-score' (determines the threshold of error that
;; makes a stroke acceptable or unacceptable.  If your strokes arn't
;; matching, then you should raise this variable.

;; `strokes-grid-resolution' (determines the grid dimensions that you use
;; when defining/reading strokes.  The finer the grid your computer can
;; handle, the more you can do, but even a 3x3 grid is pretty cool.)
;; The default value (7) should be fine for most decent computers.
;; NOTE: This variable should not be set to a number less than 3.

;; `strokes-display-strokes-buffer' will allow you to hide the strokes
;; buffer when doing simple strokes.  This is a speedup for slow
;; computers as well as people who don't want to see their strokes.

;; If you find that your mouse is accelerating too fast, you can
;; execute the UNIX X command to slow it down.  A good possibility is

;; % xset m 5/4 8

;; which seems, heuristically, to work okay, without much disruption.

;; Whenever you load in the strokes package, you will be able to save
;; what you've done upon exiting XEmacs.  You can also do

;; > M-x save-strokes

;; and it will save your strokes in ~/.strokes, or you may wish to change
;; this by setting the variable `strokes-file'.

;; Note that internally, all of the routines that are part of this
;; package are able to deal with complex strokes, as they are a superset
;; of simple strokes.  However, the default of this package will map
;; mouse button2 to the command `strokes-do-stroke', and NOT
;; `strokes-do-complex-stroke'.  If you wish to use complex strokes, you
;; will have to override this key mapping.  Complex strokes are terminated
;; with mouse button3.  The strokes package will not interfere with
;; `mouse-yank', but you may want to examine how this is done (see the
;; variable `strokes-click-command')

;; To get strokes to work as part of your your setup, then you'll have
;; put the strokes package in your load-path (preferably byte-compiled)
;; and then add the following to your .xemacs-options file (or wherever
;; you put XEmacs-specific startup preferences):

;;(and (fboundp 'device-on-window-system-p)
;;     (device-on-window-system-p)
;;     (require 'strokes))

;; Once loaded, you can start stroking.  You can also toggle between
;; strokes mode by simple typing

;; > M-x strokes-mode

;; I am now in the process of porting this package to emacs.  I also hope
;; that, with the help of others, this package will be useful in entering
;; in pictographic-like language text using the mouse (i.e. Korean).
;; Japanese and Chinese are a bit trickier, but I'm sure that with help
;; it can be done.  The next version will allow the user to enter strokes
;; which "remove the pencil from the paper" so to speak, so one character
;; can have multiple strokes.

;; Great thanks to Rob Ristroph for his generosity in letting me use his
;; PC to develop this, Jason Johnson for his help in algorithms, Euna
;; Kim for her help in Korean, and massive thanks to the helpful guys
;; on the help instance on athena (zeno, jered, amu, gsstark, ghudson, etc)
;; Special thanks to Steve Baur and Hrvoje Niksic for all their help.

;; Tasks: (what I'm getting ready for future version)...
;; 2) use 'strokes-read-complex-stroke for korean, etc.
;; 4) buffer-local 'strokes-local-map, and mode-stroke-maps would be nice
;; 5) 'list-strokes (kinda important).  What do people want?
;;    How about an optional docstring for each stroke so that a person
;;    can examine the strokes-file and actually make sense of it?
;;    (e.g. "This stroke is a pentagram")
;; 6) add some hooks, like `strokes-read-stroke-hook'
;; 7) See what people think of the factory settings.  Should I change
;;    them?  They're all pretty arbitrary in a way.  I guess they
;;    should be minimal, but computers are getting lots faster, and
;;    if I choose the defaults too conservatively, then strokes will
;;    surely dissapoint some people on decent machines (until they
;;    figure out M-x customize).  I need feedback.
;; Other: I always have the most beta version of strokes, so if you
;;        want it just let me know.

;;; Change Log:

;; 1.3: provided user variable `strokes-use-strokes-buffer' to let users
;;      hide the strokes and strokes buffer when entering simple strokes.
;; 1.3: cleaned up most leaks.
;; 1.3: with Jari Aalto's help, cleaned up overall program.
;; 1.3: added `strokes-help' for help on strokes
;; 1.3: fixed 'strokes-load-hook bug
;; 1.3: email address change: now <cadet@mit.edu>
;; 1.3: added `strokes-report-bug' based on efs/dired's `dired-report-bug'
;; 1.3: added more dialog-box queries for mouse-event stuff.
;; 1.4: allowed strokes to invoke kbd macros as well (thanks gsstark!)
;; 2.0: fixed up ordering of certain functions.
;; 2.0: fixed bug applying to strokes in dedicated and minibuffer windows.
;; 2.0: punted the C-h way of invoking strokes help routines.
;; 2.0: fixed `strokes-define-stroke' so it would error check against
;;      defining strokes that were too short (really clicks)
;; 2.0: added `strokes-toggle-strokes-buffer' interactive function
;; 2.0: added `customize' support, thanks to patch from Hrvoje (thanks)
;; 2.1: strokes no longer forces `mouse-yank-at-point' to t on mouse-yank
;;      (i.e. `mouse-yank-at-point' is up to you again)
;; 2.1: toggling strokes-mode off and then back on no longer deletes
;;      the strokes that you programmed in but didn't save before
;;      toggling off strokes-mode.
;; 2.1: advised may functions for modes like VM and w3 so that they too
;;      can use strokes, while still mantaining old button2 functionality.
;; 2.1: with steve's help, got the autoload for `strokes-mode' and
;;      fixed up the package so loading it does not enable strokes
;;      until user calls `strokes-mode'.
;; 2.2: made sure that abbrev-mode was off in the ` *strokes*' buffer
;; 2.2: added more dired advice for mouse permissions commands
;; 2.2: added some checks to see if saving strokes is really necessary so
;;      the user doesn't get promped aimlessly.
;; 2.2: change the `strokes-lift' symbol to a keyword of value `:strokes-lift'
;;      for legibility.  IF YOUR OLD STROKES DON'T WORK, THIS IS PROBABLY WHY.
;; 2.2: I might have to change this back to `'strokes-lift' because the keyword
;;      fails in emacs, though I don't know why.
;; 2.2: `strokes-describe-stroke' is nicer during recursive edits
;; 2.2: provided `strokes-fill-stroke' to fill in empty spaces of strokes
;;      as an important step towards platform (speed) independence.
;;      Because of this, I moved the global setting of `strokes-last-stroke'
;;      from `strokes-eliminate-consecutive-redundancies' to
;;      `strokes-fill-stroke' since the latter comes later in processing
;;      a user stroke.
;; 2.2: Finally changed the defaults, so now `strokes-grid-resolution' is 9
;;      and `strokes-minimum-match-score' is 1000 by default.  This will surely
;;      mess some people up, but if so, just set it back w/ M-x customize.
;; 2.2: Fixed up the mechanism for updating the `strokes-window-configuration'.
;;      Now it only uses one function (`strokes-update-window-configuration')
;;      which does it all, and much more efficiently (thanks RMS!).
;; 2.2  Fixed up the appearance of the *strokes* buffer so that there are no
;;      ugly line truncations, and I got rid of the bug which would draw the stroke
;;      on the wrong line.  I still wish that `event-closest-point' was smarter.
;;      In fact, `event-closest-point' does *not* do what its name suggests.
;; 2.3  Added more to `strokes-update-window-configuration' so it goes to hell less often
;; 2.3  `strokes-mode' no longer will undefine keys unless it's sure that the user had
;;      had them mapped to a strokes command.
;; 2.3  added more magic autoload statements so strokes work more smoothly.
;;      similarly, I made strokes-mode turn itself on when the user defines a stroke
;;      (thanks Hrvoje).
;; 2.3  Added "Strokes" to the modeline when strokes is on, and allow toggling strokes
;;      with mouse button2.
;; 2.3  Added `list-strokes', which is a really nice function which graphically lists
;;      all the strokes that the user has defined and their corresponding commands.
;;      `list-strokes' will appropriately colorize the pixmaps to display some time info.

;;; Code:

;;; Requirements and provisions...

(autoload 'reporter-submit-bug-report "reporter")
(autoload 'mail-position-on-field "sendmail")

;;; Constants...

(defconst strokes-version "2.3-beta")

(defconst strokes-bug-address "cadet@mit.edu")

(defconst strokes-lift :strokes-lift
  "Symbol representing a stroke lift event for complex strokes.
Complex strokes are those which contain two or more simple strokes.
This will be useful for when XEmacs understands Chinese.")

;;; user variables...

(defgroup strokes nil
  "Control Emacs through mouse strokes"
  :group 'mouse)

(defcustom strokes-modeline-string " Strokes"
  "*Modeline identification when strokes are on \(default is \" Strokes\"\)."
  :type 'string
  :group 'strokes)

(defcustom strokes-character ?o
  "*Character used when drawing strokes in the strokes buffer.
\(The default is lower-case `o', which works okay\)."
  :type 'character
  :group 'strokes)

(defcustom strokes-minimum-match-score 1000
  "*Minimum score for a stroke to be considered a possible match.
Requiring a perfect match would set this variable to 0.
The default value is 1000, but it's mostly dependent on how precisely
you manage to replicate your user-defined strokes.  It also depends on
the value of `strokes-grid-resolution', since a higher grid resolution
will correspond to more sample points, and thus more distance
measurements.  Usually, this is not a problem since you first set
`strokes-grid-resolution' based on what your computer seems to be able
to handle (though the defaults are usually more than sufficent), and
then you can set `strokes-minimum-match-score' to something that works
for you.  The only purpose of this variable is to insure that if you
do a bogus stroke that really doesn't match any of the predefined
ones, then strokes should NOT pick the one that came closest."
  :type 'integer
  :group 'strokes)

(defcustom strokes-grid-resolution 9
  "*Integer defining dimensions of the stroke grid.
The grid is a square grid, where STROKES-GRID-RESOLUTION defaults to
`9', making a 9x9 grid whose coordinates go from (0 . 0) on the top
left to ((STROKES-GRID-RESOLUTION - 1) . (STROKES-GRID-RESOLUTION - 1))
on the bottom right.  The greater the resolution, the more intricate
your strokes can be.
NOTE: This variable should be odd and MUST NOT be less than 3.
WARNING: Changing the value of this variable will gravely affect the
         strokes you have already programmed in.  You should try to
         figure out what it should be based on your needs and on how
         quick the particular platform(s) you're operating on, and
         only then start programming in your custom strokes."
  :type 'integer
  :group 'strokes)

(defcustom strokes-file "~/.strokes"
  "*File containing saved strokes for stroke-mode (default is ~/.strokes)."
  :type 'file
  :group 'strokes)

(defcustom strokes-buffer-name " *strokes*"
  "The buffer that the strokes take place in (default is ` *strokes*')."
  :type 'string
  :group 'strokes)

(defcustom strokes-use-strokes-buffer t
  "*If non-nil, the strokes buffer is used and strokes are displayed.
If nil, strokes will be read the same, however the user will not be
able to see the strokes.  This be helpful for people who don't like
the delay in switching to the strokes buffer."
  :type 'boolean
  :group 'strokes)

(defcustom strokes-click-command 'mouse-yank
  "*Command to execute when stroke is actually a `click' event.
This is set to `mouse-yank' by default."
  :type 'function
  :group 'strokes)

;;; internal variables...

;;;###autoload
(defvar strokes-mode nil
  "Non-nil when `strokes' is globally enabled")

(defvar strokes-window-configuration nil
  "The special window configuration used when entering strokes.
This is set properly in the function `strokes-update-window-configuration'.")

(defvar strokes-last-stroke nil
  "Last stroke entered by the user.
Its value gets set every time the function
`strokes-fill-stroke' gets called,
since that is the best time to set the variable")

(defvar strokes-global-map '()
  "Association list of strokes and their definitions.
Each entry is (STROKE . COMMAND) where STROKE is itself a list of
coordinates (X . Y) where X and Y are lists of positions on the
normalized stroke grid, with the top left at (0 . 0).  COMMAND is the
corresponding interactive function")

(defvar strokes-load-hook nil
  "Function or functions to be called when `strokes' is loaded.")

;;; Macros...

(defsubst strokes-click-p (stroke)
  "Non-nil if STROKE is really click."
  (< (length stroke) 3))

;;; old, but worked pretty good (just in case)...
;;(defmacro strokes-define-stroke (stroke-map stroke def)
;;  "Add STROKE to STROKE-MAP alist with given command DEF"
;;  (list 'if (list '< (list 'length stroke) 3)
;;	(list 'error
;;	      "That's a click, not a stroke.  See `strokes-click-command'")
;;	(list 'setq stroke-map (list 'cons (list 'cons stroke def)
;;				     (list 'remassoc stroke stroke-map)))))

(defmacro strokes-define-stroke (stroke-map stroke def)
  "Add STROKE to STROKE-MAP alist with given command DEF"
  `(if (strokes-click-p ,stroke)
       (error "That's a click, not a stroke; see `strokes-click-command'")
     (setq ,stroke-map (cons (cons ,stroke ,def)
			     (remassoc ,stroke ,stroke-map)))))

(defalias 'define-stroke 'strokes-define-stroke)

(defsubst strokes-square (x)
  "Returns the square of the number X"
  (* x x))

(defsubst strokes-distance-squared (p1 p2)
  "Gets the distance (squared) between to points P1 and P2.
Each point is a cons cells (X . Y)"
  (let ((x1 (car p1))
	(y1 (cdr p1))
	(x2 (car p2))
	(y2 (cdr p2)))
    (+ (strokes-square (- x2 x1))
       (strokes-square (- y2 y1)))))

;;; Advice for various functions...

;; I'd originally wanted to write a macro that would just take in the
;; generic functions which use mouse button2 in various modes.  Most of
;; them are identical in form: they take an event as the single argument
;; and then do their thing.  I tried writing a macro that looked
;; something like this, but failed.  Advice just ain't that easy.  The
;; one that bugged me the most was `Manual-follow-xref', because that had
;; &rest arguments, and I didn't know how to work around it in defadvice.
;; However, I was able to fix up most of the important modes (i.e. the
;; ones I use all the time).  One `bug' in the program that I just can't
;; seem to figure out is why I can only advise other button2 functions
;; successfully when the variable `strokes-use-strokes-buffer' is nil.  I
;; did all the save-excursion/save-window-excursion stuff SPECIFICALLY so
;; that using the strokes buffer or not would absolutely not affect any
;; other part of the program.  If someone can figure out how to make the
;; following advices work w/ regardless of that variable
;; `strokes-use-strokes-buffer', then that would be a great victory.  If
;; someone out there would be kind enough to make the commented code
;; below work, I'd be grateful.  By the way, I put the `protect' keywords
;; there to insure that if a stroke went bad, then
;; `strokes-click-command' would be set back.  If this isn't necessary,
;; then feel free to let me know.

;; For what follows, I really wanted something that would work like this:

;;(strokes-fix-button2 'vm-mouse-button-2)

;; Or even better, I could have simply done something like:

;;(mapcar 'strokes-fix-button2
;; 	  '(vm-mouse-button-2
;;          rmail-summary-mouse-goto-msg
;;	    <rest of them>))

;;; With help from Hans (author of advice.el)...
(defmacro strokes-fix-button2-command (command)
  "Fix COMMAND so that it can also work with strokes.
COMMAND must take one event argument.
Example of how one might fix up a command that's bound to button2
and which is an interactive funcion of one event argument:

(strokes-fix-button2-command 'vm-mouse-button-2)"
  (let ((command (eval command)))
    `(progn
       (defadvice ,command (around strokes-fix-button2 compile preactivate)
         ,(format "Fix %s to work with strokes." command)
         (if strokes-use-strokes-buffer
             ;; then strokes is no good and we'll have to use the original
             ad-do-it
           ;; otherwise, we can make strokes work too...
	     (let ((strokes-click-command
                  ',(intern (format "ad-Orig-%s" command))))
             (strokes-do-stroke (ad-get-arg 0))))))))

(strokes-fix-button2-command 'vm-mouse-button-2)
(strokes-fix-button2-command 'rmail-summary-mouse-goto-msg)
(strokes-fix-button2-command 'Buffer-menu-mouse-select)
(strokes-fix-button2-command 'w3-widget-button-click)
(strokes-fix-button2-command 'widget-image-button-press)
(strokes-fix-button2-command 'Info-follow-clicked-node)
(strokes-fix-button2-command 'compile-mouse-goto-error)
(strokes-fix-button2-command 'gdbsrc-select-or-yank)
(strokes-fix-button2-command 'hypropos-mouse-get-doc)
(strokes-fix-button2-command 'gnus-mouse-pick-group)
(strokes-fix-button2-command 'gnus-mouse-pick-article)
(strokes-fix-button2-command 'gnus-article-push-button)
(strokes-fix-button2-command 'dired-mouse-find-file)
(strokes-fix-button2-command 'url-dired-find-file-mouse)
(strokes-fix-button2-command 'dired-u-r-mouse-toggle)
(strokes-fix-button2-command 'dired-u-w-mouse-toggle)
(strokes-fix-button2-command 'dired-u-x-mouse-toggle)
(strokes-fix-button2-command 'dired-g-r-mouse-toggle)
(strokes-fix-button2-command 'dired-g-w-mouse-toggle)
(strokes-fix-button2-command 'dired-g-x-mouse-toggle)
(strokes-fix-button2-command 'dired-o-r-mouse-toggle)
(strokes-fix-button2-command 'dired-o-w-mouse-toggle)
(strokes-fix-button2-command 'isearch-yank-x-selection)
(strokes-fix-button2-command 'occur-mode-mouse-goto)

;;; I can fix the customize widget button click, but then
;;; people will get confused when they try to customize
;;; strokes with the mouse and customize tells them that
;;; `strokes-click-command' is mapped to `ad-Orig-widget-button-click'
;;(strokes-fix-button2-command 'widget-button-click)

;;; without the advice, each advised function would look like...
;;(defadvice vm-mouse-button-2 (around vm-strokes activate protect)
;;  "Allow strokes to work in VM."
;;  (if strokes-use-strokes-buffer
;;      ;; then strokes is no good and we'll have to use the original
;;      ad-do-it
;;    ;; otherwise, we can make strokes work too...
;;    (let ((strokes-click-command 'ad-Orig-vm-mouse-button-2))
;;      (strokes-do-stroke (ad-get-arg 0)))))

;;; Functions...

(defun strokes-lift-p (object)
  "Return non-nil if object is a stroke-lift"
  (eq object strokes-lift))

(defun strokes-unset-last-stroke ()
  "Undo the last stroke definition."
  (interactive)
  (let ((command (cdar strokes-global-map)))
    (if (y-or-n-p-maybe-dialog-box
	 (format "really delete last stroke definition, defined to `%s'? "
		 command))
	(progn
	  (setq strokes-global-map (cdr strokes-global-map))
	  (message "That stroke has been deleted"))
      (message "Nothing done"))))

;;;###autoload
(defun strokes-global-set-stroke (stroke command)
  "Interactively give STROKE the global binding as COMMAND.
Operated just like `global-set-key', except for strokes.
COMMAND is a symbol naming an interactively-callable function.  STROKE
is a list of sampled positions on the stroke grid as described in the
documentation for the `strokes-define-stroke' function."
  (interactive
   (list
    (and (or strokes-mode (strokes-mode t))
	 (strokes-read-complex-stroke
	  "Define a new stroke.  Draw with button1 (or 2).  End with button3..."))
    (read-command "command to map stroke to: ")))
  (strokes-define-stroke strokes-global-map stroke command))

;;;###autoload
(defalias 'global-set-stroke 'strokes-global-set-stroke)

;;(defun global-unset-stroke (stroke); FINISH THIS DEFUN!
;;  "delete all strokes matching STROKE from `strokes-global-map',
;; letting the user input
;; the stroke with the mouse"
;;  (interactive
;;   (list
;;    (strokes-read-stroke "Enter the stroke you want to delete...")))
;;  (strokes-define-stroke 'strokes-global-map stroke command))

(defun strokes-get-grid-position (stroke-extent position &optional grid-resolution)
  "Map POSITION to a new grid position based on its STROKE-EXTENT and GRID-RESOLUTION.
STROKE-EXTENT as a list \(\(xmin . ymin\) \(xmax . ymax\)\).
If POSITION is a `strokes-lift', then it is itself returned.
Optional GRID-RESOLUTION may be used in place of STROKES-GRID-RESOLUTION.
The grid is a square whose dimesion is [0,GRID-RESOLUTION)."
  (cond ((consp position)		; actual pixel location
	 (let ((grid-resolution (or grid-resolution strokes-grid-resolution))
	       (x (car position))
	       (y (cdr position))
	       (xmin (caar stroke-extent))
	       (ymin (cdar stroke-extent))
	       ;; the `1+' is there to insure that the
	       ;; formula evaluates correctly at the boundaries
	       (xmax (1+ (caadr stroke-extent)))
	       (ymax (1+ (cdadr stroke-extent))))
	   (cons (floor (* grid-resolution
			   (/ (float (- x xmin))
			      (- xmax xmin))))
		 (floor (* grid-resolution
			   (/ (float (- y ymin))
			      (- ymax ymin)))))))
	((strokes-lift-p position)	; stroke lift
	 strokes-lift)))

;;(defun strokes-get-grid-position (stroke-extent pix-pos)
;;  "Return the stroke-grid position for PIX-POS given the total STROKE-EXTENT.
;;STROKE-EXTENT as a list \(\(xmin . ymin\) \(xmax . ymax\)\) and a particular
;;pixel position or `strokes-lift', find the corresponding grid position
;;\(based on `strokes-grid-resolution'\) for the PIX-POS."
;;  (cond ((consp pix-pos)		; actual pixel location
;;	 (let ((x (car pix-pos))
;;	       (y (cdr pix-pos))
;;	       (xmin (caar stroke-extent))
;;	       (ymin (cdar stroke-extent))
;;	       ;; the `1+' is there to insure that the
;;	       ;; formula evaluates correctly at the boundaries
;;	       (xmax (1+ (caadr stroke-extent)))
;;	       (ymax (1+ (cdadr stroke-extent))))
;;	   (cons (floor (* strokes-grid-resolution
;;			   (/ (float (- x xmin))
;;			      (- xmax xmin))))
;;		 (floor (* strokes-grid-resolution
;;			   (/ (float (- y ymin))
;;			      (- ymax ymin)))))))
;;	((strokes-lift-p pix-pos)	; stroke lift
;;	 strokes-lift)))

(defun strokes-get-stroke-extent (pixel-positions)
  "From a list of absolute PIXEL-POSITIONS, returns absolute spatial extent.
The return value is a list ((xmin . ymin) (xmax . ymax))."
  (if pixel-positions
      (let ((xmin (caar pixel-positions))
	    (xmax (caar pixel-positions))
	    (ymin (cdar pixel-positions))
	    (ymax (cdar pixel-positions))
	    (rest (cdr pixel-positions)))
	(while rest
	  (if (consp (car rest))
	      (let ((x (caar rest))
		    (y (cdar rest)))
		(if (< x xmin)
		    (setq xmin x))
		(if (> x xmax)
		    (setq xmax x))
		(if (< y ymin)
		    (setq ymin y))
		(if (> y ymax)
		    (setq ymax y))))
	  (setq rest (cdr rest)))
	(let ((delta-x (- xmax xmin))
	      (delta-y (- ymax ymin)))
	  (if (> delta-x delta-y)
	      (setq ymin (- ymin
			    (/ (- delta-x delta-y)
			       2))
		    ymax (+ ymax
			    (/ (- delta-x delta-y)
			       2)))
	    (setq xmin (- xmin
			  (/ (- delta-y delta-x)
			     2))
		  xmax (+ xmax
			  (/ (- delta-y delta-x)
			     2))))
	  (list (cons xmin ymin)
		(cons xmax ymax))))
    nil))

(defun strokes-eliminate-consecutive-redundancies (entries)
  "Returns a list with no consecutive redundant entries."
  ;; defun a grande vitesse grace a Dave G.
  (loop for element on entries
        if (not (equal (car element) (cadr element))) 
        collect (car element)))
;;  (loop for element on entries
;;        nconc (if (not (equal (car el) (cadr el))) 
;;                  (list (car el)))))
;; yet another (orig) way of doing it...
;;  (if entries
;;      (let* ((current (car entries))
;;	     (rest (cdr entries))
;;	     (non-redundant-list (list current))
;;	     (next nil))
;;	(while rest
;;	  (setq next (car rest))
;;	  (if (equal current next)
;;	      (setq rest (cdr rest))
;;	    (setq non-redundant-list (cons next non-redundant-list)
;;		  current next
;;		  rest (cdr rest))))
;;	(nreverse non-redundant-list))
;;    nil))

(defun strokes-renormalize-to-grid (positions &optional grid-resolution)
  "Map POSITIONS to a new grid whose dimensions are based on GRID-RESOLUTION.
POSITIONS is a list of positions and stroke-lifts.
Optional GRID-RESOLUTION may be used in place of STROKES-GRID-RESOLUTION.
The grid is a square whose dimesion is [0,GRID-RESOLUTION)."
  (let ((stroke-extent (strokes-get-stroke-extent positions)))
    (mapcar (function
	     (lambda (pos)
	       (strokes-get-grid-position stroke-extent pos grid-resolution)))
	    positions)))

;;(defun strokes-normalize-pixels-to-grid (pixel-positions)
;;  "Map PIXEL-POSITIONS to the stroke grid.
;;PIXEL-POSITIONS is a list of pixel-positions and stroke-lifts.  The
;;normalized stroke grid is defined by the variable STROKES-GRID-RESOLUTION"
;;  (let ((stroke-extent (strokes-get-stroke-extent pixel-positions)))
;;    (mapcar (function
;;	     (lambda (pix-pos)
;;	       (strokes-get-grid-position stroke-extent pix-pos)))
;;	    pixel-positions)))

(defun strokes-fill-stroke (unfilled-stroke &optional force)
  "Fill in missing grid locations in the list of UNFILLED-STROKE.
If FORCE is non-nil, then fill the stroke even if it's `stroke-click'.
NOTE: This is where the global variable `strokes-last-stroke' is set."
  (setq strokes-last-stroke		; this is global
	(if (and (strokes-click-p unfilled-stroke)
		 (not force))
	    unfilled-stroke
	  (loop for grid-locs on unfilled-stroke
		nconc (let* ((current (car grid-locs))
			     (current-is-a-point-p (consp current))
			     (next (cadr grid-locs))
			     (next-is-a-point-p (consp next))
			     (both-are-points-p (and current-is-a-point-p
						     next-is-a-point-p))
			     (x1 (and current-is-a-point-p
				      (car current)))
			     (y1 (and current-is-a-point-p
				      (cdr current)))
			     (x2 (and next-is-a-point-p
				      (car next)))
			     (y2 (and next-is-a-point-p
				      (cdr next)))
			     (delta-x (and both-are-points-p
					   (- x2 x1)))
			     (delta-y (and both-are-points-p
					   (- y2 y1)))
			     (slope (and both-are-points-p
					 (if (zerop delta-x)
					     nil ; undefined vertical slope
					   (/ (float delta-y)
					      delta-x)))))
			(cond ((not both-are-points-p)
			       (list current))
			      ((null slope) ; undefinded vertical slope
			       (if (>= delta-y 0)
				   (loop for y from y1 below y2
					 collect (cons x1 y))
				 (loop for y from y1 above y2
				       collect (cons x1 y))))
			      ((zerop slope) ; (= y1 y2)
			       (if (>= delta-x 0)
				   (loop for x from x1 below x2
					 collect (cons x y1))
				 (loop for x from x1 above x2
				       collect (cons x y1))))
			      ((>= (abs delta-x) (abs delta-y))
			       (if (> delta-x 0)
				   (loop for x from x1 below x2
					 collect (cons x
						       (+ y1
							  (round (* slope
								    (- x x1))))))
				 (loop for x from x1 above x2
				       collect (cons x
						     (+ y1
							(round (* slope
								  (- x x1))))))))
			      (t	; (< (abs delta-x) (abs delta-y))
			       (if (> delta-y 0)
				   (loop for y from y1 below y2
					 collect (cons (+ x1
							  (round (/ (- y y1)
								    slope)))
						       y))
				 (loop for y from y1 above y2
				       collect (cons (+ x1
							(round (/ (- y y1)
								  slope)))
						     y))))))))))

(defun strokes-rate-stroke (stroke1 stroke2)
  "Rates STROKE1 with STROKE2 and returns a score based on a distance metric.
Note: the rating is an error rating, and therefore, a return of 0
represents a perfect match.  Also note that the order of stroke
arguments is order-independent for the algorithm used here."
  (if (and stroke1 stroke2)
      (let ((rest1 (cdr stroke1))
	    (rest2 (cdr stroke2))
	    (err (strokes-distance-squared (car stroke1)
					   (car stroke2))))
	(while (and rest1 rest2)
	  (while (and (consp (car rest1))
		      (consp (car rest2)))
	    (setq err (+ err
			 (strokes-distance-squared (car rest1)
						   (car rest2)))
		  stroke1 rest1
		  stroke2 rest2
		  rest1 (cdr stroke1)
		  rest2 (cdr stroke2)))
	  (cond ((and (strokes-lift-p (car rest1))
		      (strokes-lift-p (car rest2)))
		 (setq rest1 (cdr rest1)
		       rest2 (cdr rest2)))
		((strokes-lift-p (car rest2))
		 (while (consp (car rest1))
		   (setq err (+ err
				(strokes-distance-squared (car rest1)
							  (car stroke2)))
			 rest1 (cdr rest1))))
		((strokes-lift-p (car rest1))
		 (while (consp (car rest2))
		   (setq err (+ err
				(strokes-distance-squared (car stroke1)
							  (car rest2)))
			 rest2 (cdr rest2))))))
	(if (null rest2)
	    (while (consp (car rest1))
	      (setq err (+ err
			   (strokes-distance-squared (car rest1)
						     (car stroke2)))
		    rest1 (cdr rest1))))
	(if (null rest1)
	    (while (consp (car rest2))
	      (setq err (+ err
			   (strokes-distance-squared (car stroke1)
						     (car rest2)))
		    rest2 (cdr rest2))))
	(if (or (strokes-lift-p (car rest1))
		(strokes-lift-p (car rest2)))
	    (setq err nil)
	  err))
    nil))

(defun strokes-match-stroke (stroke stroke-map)
  "Finds the best matching command of STROKE in STROKE-MAP.
Returns the corresponding match as (COMMAND . SCORE)."
  (if (and stroke stroke-map)
      (let ((score (strokes-rate-stroke stroke (caar stroke-map)))
	    (command (cdar stroke-map))
	    (map (cdr stroke-map)))
	(while map
	  (let ((newscore (strokes-rate-stroke stroke (caar map))))
	    (if (or (and newscore score (< newscore score))
		    (and newscore (null score)))
		(setq score newscore
		      command (cdar map)))
	    (setq map (cdr map))))
	(if score
	    (cons command score)
	  nil))
    nil))

;;;###autoload
(defun strokes-read-stroke (&optional prompt event)
  "Read a simple stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
This function will display the stroke interactively as it is being
entered in the strokes buffer if the variable
`strokes-use-strokes-buffer' is non-nil.
Optional EVENT is currently not used, but hopefully will be soon."
  (save-excursion
    (let ((pix-locs nil)
	  (grid-locs nil)
	  (event (or event (make-event))))
      (if strokes-use-strokes-buffer
	  ;; switch to the strokes buffer and
	  ;; display the stroke as it's being read
	  (save-window-excursion
	    (set-window-configuration strokes-window-configuration)
	    (if prompt
		(progn
		  (setq event (next-event event prompt))
		  (while (not (button-press-event-p event))
		    (dispatch-event event)
		    (setq event (next-event event)))))
	    (unwind-protect
		(progn
		  (setq event (next-event event))
		  (while (not (button-release-event-p event))
		    (if (mouse-event-p event)
			(let ((point (event-closest-point event)))
			  (when point
			    (goto-char point)
			    (subst-char-in-region point (1+ point) ?\  strokes-character))
			  (setq pix-locs (cons (cons (event-x-pixel event)
						     (event-y-pixel event))
					       pix-locs))))
		    (setq event (next-event event))))
	      ;; protected
	      ;; clean up strokes buffer and then bury it.
	      (subst-char-in-region (point-min) (point-max) strokes-character ?\ )
	      (goto-char (point-min))
	      (bury-buffer)))
	;; Otherwise, don't use strokes buffer and read stroke silently
	(if prompt
	    (progn
	      (setq event (next-event event prompt))
	      (while (not (button-press-event-p event))
		(dispatch-event event)
		(setq event (next-event event)))))
	(setq event (next-event))
	(while (not (button-release-event-p event))
	  (if (mouse-event-p event)
	      (setq pix-locs (cons (cons (event-x-pixel event)
					 (event-y-pixel event))
				   pix-locs)))
	  (setq event (next-event event))))
      (setq grid-locs (strokes-renormalize-to-grid (nreverse pix-locs)))
      (strokes-fill-stroke (strokes-eliminate-consecutive-redundancies grid-locs)))))

;;;###autoload
(defun strokes-read-complex-stroke (&optional prompt event)
  "Read a complex stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
Note that a complex stroke allows the user to pen-up and pen-down.  This
is implemented by allowing the user to paint with button1 or button2 and
then complete the stroke with button3.
Optional EVENT is currently not used, but hopefully will be soon."
  (save-excursion
    (save-window-excursion
      (set-window-configuration strokes-window-configuration)
      (let ((pix-locs nil)
	    (grid-locs nil)
	    (event (or event (next-event nil prompt))))
	(if prompt
	    (while (not (button-press-event-p event))
	      (dispatch-event event)
	      (setq event (next-event event))))
	(unwind-protect
	    (progn
	      (while (not (and (button-press-event-p event)
			       (eq (event-button event) 3)))
		(while (not (button-release-event-p event))
		  (if (mouse-event-p event)
		      (let ((point (event-closest-point event)))
			(when point
			  (goto-char point)
			  (subst-char-in-region point (1+ point) ?\ strokes-character))
			(setq pix-locs (cons (cons (event-x-pixel event)
						   (event-y-pixel event))
					     pix-locs))))
		  (setq event (next-event event prompt)))
		(setq pix-locs (cons strokes-lift pix-locs))
		(while (not (button-press-event-p event))
		  (dispatch-event event)
		  (setq event (next-event event prompt))))
	      (setq pix-locs (nreverse (cdr pix-locs))
		    grid-locs (strokes-renormalize-to-grid pix-locs))
	      (strokes-fill-stroke
	       (strokes-eliminate-consecutive-redundancies grid-locs)))
	  ;; protected
	  (subst-char-in-region (point-min) (point-max) strokes-character ?\ )
	  (goto-char (point-min))
	  (bury-buffer))))))

(defun strokes-execute-stroke (stroke)
  "Given STROKE, execute the command which corresponds to it.
The command will be executed provided one exists for that stroke,
based on the variable `strokes-minimum-match-score'.
If no stroke matches, nothing is done and return value is nil."
  (let* ((match (strokes-match-stroke stroke strokes-global-map))
	 (command (car match))
	 (score (cdr match)))
    (cond ((strokes-click-p stroke)
	   ;; This is the case of a `click' type event
	   (command-execute strokes-click-command))
	  ((and match (<= score strokes-minimum-match-score))
	   (message "%s" command)
	   (command-execute command))
	  ((null strokes-global-map)
	   (if (file-exists-p strokes-file)
	       (and (y-or-n-p-maybe-dialog-box
		     (format "No strokes loaded.  Load `%s'? "
			     strokes-file))
		    (strokes-load-user-strokes))
	     (error "No strokes defined; use `global-set-stroke'")))
	  (t
	   (error
	    "No stroke matches; see variable `strokes-minimum-match-score'")
	   nil))))

;;;###autoload
(defun strokes-do-stroke (event)
  "Read a simple stroke from the user and then exectute its comand.
This must be bound to a mouse event."
  (interactive "e")
  (or strokes-mode (strokes-mode t))
  (strokes-execute-stroke (strokes-read-stroke nil event)))

;;;###autoload
(defun strokes-do-complex-stroke (event)
  "Read a complex stroke from the user and then exectute its command.
This must be bound to a mouse event."
  (interactive "e")
  (or strokes-mode (strokes-mode t))
  (strokes-execute-stroke (strokes-read-complex-stroke nil event)))

;;;###autoload
(defun strokes-describe-stroke (stroke)
  "Displays the command which STROKE maps to, reading STROKE interactively."
  (interactive
   (list
    (strokes-read-complex-stroke
     "Enter stroke to describe; end with button3...")))
  (let* ((match (strokes-match-stroke stroke strokes-global-map))
	 (command (or (and (strokes-click-p stroke)
			   strokes-click-command)
		      (car match)))
	 (score (cdr match)))
    (if (or (and match
		 (<= score strokes-minimum-match-score))
	    (and (strokes-click-p stroke)
		 strokes-click-command))
	(message "That stroke maps to `%s'" command)
      (message "That stroke is undefined"))
    (sleep-for 1)))			; helpful for recursive edits

;;;###autoload
(defalias 'describe-stroke 'strokes-describe-stroke)

;;;###autoload
(defun strokes-help ()
  "Get instructional help on using the the `strokes' package."
  (interactive)
  (with-displaying-help-buffer
   (function
    (lambda ()
      (save-excursion
	(let ((helpdoc
	       "This is help for the strokes package.

If you find something wrong with it, or feel that it can be improved
in some way, then please feel free to email me:

David Bakhash <cadet@mit.edu>

or just do

M-x strokes-report-bug

------------------------------------------------------------

The strokes package allows you to define strokes (that you make with
the mouse or other pointer device) that XEmacs can interpret as
corresponding to commands, and then executes the commands.  It does
character recognition, so you don't have to worry about getting it
right every time.

Strokes are easy to program and fun to use.  To start strokes going,
you'll want to put the following line in your .emacs file:

(and (fboundp 'device-on-window-system-p)
     (device-on-window-system-p)
     (require 'strokes))

This will load strokes when and only when you start XEmacs on a window
system (i.e. that has a pointer (mouse) device, etc.).

To toggle strokes-mode, you just do

> M-x strokes-mode

When you're ready to start defining strokes, just use the command

> M-x global-set-stroke

You will see a ` *strokes*' buffer which is waiting for you to enter in
your stroke.  When you enter in the stroke, you draw with button1 or
button2, and then end with button3.  Next, you enter in the command
which will be executed when that stroke is invoked.  Simple as that.
For now, try to define a stroke to copy a region.  This is a popular
edit command, so type

> M-x global-set-stroke

Then, in the ` *strokes*' buffer, draw the letter `C' (for `copy')
and then, when it asks you to enter the command to map that to, type

> copy-region-as-kill

That's about as hard as it gets.
Remember: paint with button1 or button2 and then end with button3.

If ever you want to know what a certain strokes maps to, then do

> M-x describe-stroke

and you can enter in any arbitrary stroke.  Remember: The strokes
package lets you program in simple and complex (multi-lift) strokes.
The only difference is how you *invoke* the two.  You will most likely
use simple strokes, as complex strokes were developed for
Chinese/Japanese/Korean.  So the middle mouse button (button2) will
invoke the command `strokes-do-stroke' in buffers where button2 doesn't
already have a meaning other than its original, which is `mouse-yank'.
But don't worry: `mouse-yank' will still work with strokes (see the
variable `strokes-click-command').

If ever you define a stroke which you don't like, then you can unset
it with the command

> M-x strokes-unset-last-stroke

You can always get an idea of what your current strokes look like with
the command

> M-x list-strokes

Your strokes will be displayed in from most recent down, and the
beginning of each simple stroke will be marked by a color dot.  Since
you may have several simple strokes in a complex stroke, the dot
colors are arranged in the rainbow color sequence, `ROYGBIV'.

Your strokes are stored as you enter them.  They get saved in a file
called ~/.strokes, along with other strokes configuration variables.
You can change this location by setting the variable `strokes-file'.
You will be prompted to save them when you exit XEmacs, or you can save
them with

> M-x save-strokes

Your strokes get loaded automatically when you enable `strokes-mode'.
You can also load in your user-defined strokes with

> M-x load-user-strokes

A few more important things:

o The command `strokes-do-stroke' is also invoked with C-button2, so that you
  can still enter a stroke in modes which use button2 for other things,
  such as cross-referencing.

o Complex strokes (i.e. `strokes-do-complex-stroke'), by default, use
  Sh-button2.

o Strokes are a bit computer-dependent in that they depend somewhat on
  the speed of the computer you're working on.  This means that you
  may have to tweak some variables.  You can read about them in the
  commentary of `strokes.el'.  Better to just use apropos and read their
  docstrings.  All variables/functions start with `strokes'.  The one
  variable which many people wanted to see was
  `strokes-use-strokes-buffer' which allows the user to use strokes
  silently--without displaying the strokes.  All variables can be set
  by customizing the group named `strokes' via the customization package:

  > M-x customize

o A problem with strokes happens when you resize windows.  If you
  enlarge your XEmacs window a lot and realize that your strokes
  buffer is not big enough, you may need to fix it with

  > M-x strokes-update-window-configuration."))
	 (princ helpdoc standard-output)))))))

(defun strokes-report-bug ()
  "Submit a bug report for strokes."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (or (boundp 'reporter-version)
	(setq reporter-version
	      "Your version of reporter is obsolete.  Please upgrade."))
    (reporter-submit-bug-report
     strokes-bug-address "Strokes"
     (cons
      'strokes-version
      (nconc
       (mapcar
	'intern
	(sort
	 (let (completion-ignore-case)
	   (all-completions "strokes-" obarray 'user-variable-p))
	 'string-lessp))
       (list 'reporter-version)))
     (function
      (lambda ()
	(save-excursion
	  (mail-position-on-field "subject")
	  (beginning-of-line)
	  (skip-chars-forward "^:\n")
	  (if (looking-at ": Strokes;")
	      (progn
		(goto-char (match-end 0))
		(delete-char -1)
		(insert " " strokes-version " bug:")))))))))

(defsubst strokes-fill-current-buffer-with-whitespace ()
  "Erase the contents of the current buffer and fill it with whitespace"
  (erase-buffer)
  (loop for i from 1 to (frame-height) do
	(progn
	  (insert-char ?\  (1- (frame-width)))
	  (newline)))
  (goto-char (point-min)))

(defun strokes-update-window-configuration ()
  "Insure that `strokes-window-configuration' is up-to-date."
  (interactive)
  (let ((current-window (selected-window)))
    (cond ((or (window-minibuffer-p current-window)
	       (window-dedicated-p current-window))
	   ;; don't try to update strokes window configuration
	   ;; if window is dedicated or a minibuffer
	   nil)
	  ((or (interactive-p)
	       (not (buffer-live-p (get-buffer strokes-buffer-name)))
	       (null strokes-window-configuration))
	   ;; create `strokes-window-configuration' from scratch...
	   (save-excursion
	     (save-window-excursion
	       (get-buffer-create strokes-buffer-name)
	       (set-window-buffer current-window strokes-buffer-name)
	       (delete-other-windows)
	       (fundamental-mode)
	       (auto-save-mode 0)
	       (if (featurep 'font-lock)
		   (font-lock-mode 0))
	       (abbrev-mode 0)
	       (buffer-disable-undo (current-buffer))
	       (setq truncate-lines nil)
	       (strokes-fill-current-buffer-with-whitespace)
	       (setq strokes-window-configuration (current-window-configuration))
	       (bury-buffer))))
	  (t				; `strokes buffer' still exists...
	   ;; update the strokes-window-configuration for this specific frame...
	   (save-excursion
	     (save-window-excursion
	       (set-window-buffer current-window strokes-buffer-name)
	       (delete-other-windows)
	       (strokes-fill-current-buffer-with-whitespace)
	       (setq strokes-window-configuration (current-window-configuration))
	       (bury-buffer)))))))

;;;###autoload
(defun strokes-load-user-strokes ()
  "Load user-defined strokes from file named by `strokes-file'."
  (interactive)
  (cond ((and (file-exists-p strokes-file)
	      (file-readable-p strokes-file))
	 (load-file strokes-file))
	((interactive-p)
	 (error "Trouble loading user-defined strokes; nothing done"))
	(t
	 (message "No user-defined strokes, sorry"))))

;;;###autoload
(defalias 'load-user-strokes 'strokes-load-user-strokes)

(defun strokes-prompt-user-save-strokes ()
  "Save user-defined strokes to file named by `strokes-file'."
  (interactive)
  (save-excursion
    (let ((current strokes-global-map))
      (unwind-protect
	  (progn
	    (setq strokes-global-map nil)
	    (strokes-load-user-strokes)
	    (if (and (not (equal current strokes-global-map))
		     (or (interactive-p)
			 (yes-or-no-p-maybe-dialog-box "save your strokes? ")))
		(progn
		  (require 'pp)		; pretty-print variables
		  (message "Saving strokes in %s..." strokes-file)
		  (get-buffer-create "*saved-strokes*")
		  (set-buffer "*saved-strokes*")
		  (erase-buffer)
		  (emacs-lisp-mode)
		  (goto-char (point-min))
		  (insert-string
		   ";;   -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*-\n")
		  (insert-string (format ";;; saved strokes for %s, as of %s\n\n"
					 (user-full-name)
					 (format-time-string "%B %e, %Y" nil)))
		  (message "Saving strokes in %s..." strokes-file)
		  (insert-string (format "(setq strokes-global-map '%s)"
					 (pp current)))
		  (message "Saving strokes in %s..." strokes-file)
		  (indent-region (point-min) (point-max) nil)
		  (write-region (point-min)
				(point-max)
				strokes-file))
	      (message "(no changes need to be saved)")))
	;; protected
	(if (get-buffer "*saved-strokes*")
	    (kill-buffer (get-buffer "*saved-strokes*")))
	(setq strokes-global-map current)))))

(defalias 'save-strokes 'strokes-prompt-user-save-strokes)

(defun strokes-toggle-strokes-buffer (&optional arg)
  "Toggle the use of the strokes buffer.
In other words, toggle the variabe `strokes-use-strokes-buffer'.
With ARG, use strokes buffer if and only if ARG is positive or true.
Returns value of `strokes-use-strokes-buffer'."
  (interactive "P")
  (setq strokes-use-strokes-buffer
	(if arg (> (prefix-numeric-value arg) 0)
	  (not strokes-use-strokes-buffer))))

(defun strokes-xpm-for-stroke (stroke &optional bufname)
  "Create an xpm pixmap for the given stroke in buffer `*strokes-xpm*'.
Optional BUFNAME to name something else.
The pixmap will contain time information via rainbow dot colors
where each individual strokes begins."
  (save-excursion
    (let ((buf (get-buffer-create (or bufname "*strokes-xpm*")))
	  (stroke (strokes-eliminate-consecutive-redundancies
		   (strokes-fill-stroke
		    (strokes-renormalize-to-grid stroke 31))))
	  (lift-flag t)
	  (rainbow-chars (list ?R ?O ?Y ?G ?B ?P)) ; ROYGBIV w/o indigo
	  (header (format "/* XPM */
static char * stroke_xpm[] = {
/* width height ncolors cpp [x_hot y_hot] */
\"33 33 9 1 26 23\",
/* colors */
\" 	c #FFFFFFFFFFFF\",
\"*	s iconColor1	m black	c black\",
\"R	c #FFFF00000000\",
\"O	c #FFFF80000000\",
\"Y	c #FFFFFFFF0000\",
\"G	c #0000FFFF0000\",
\"B	c #00000000FFFF\",
\"P	c #FFFF0000FFFF\",
\".	c #45458B8B0000\",
/* pixels */")))
      (set-buffer buf)
      (erase-buffer)
      (insert header)
      (loop repeat 33 do
	    (newline)
	    (insert-char ?\")
	    (insert-char ?\  33)
	    (insert "\",")
	    finally (insert "}\n"))
      (loop for point in stroke
	    for x = (car-safe point)
	    for y = (cdr-safe point) do
	    (cond ((consp point)
		   ;; draw a point, and possibly a starting-point
		   (if lift-flag
		       ;; mark starting point with the appropriate color
		       (let ((char (or (car rainbow-chars) ?\.)))
			 (loop for i from 0 to 2 do
			       (loop for j from 0 to 2 do
				     (goto-line (+ 16 i y))	
				     (forward-char (+ 1 j x))
				     (delete-char 1)
				     (insert-char char)))
			 (setq rainbow-chars (cdr rainbow-chars)
			       lift-flag nil))
		     ;; Otherwise, just plot the point...
		     (goto-line (+ 17 y))	
		     (forward-char (+ 2 x))	
		     (subst-char-in-region (point) (1+ (point)) ?\  ?\*)))
		  ((strokes-lift-p point)
		   ;; a lift--tell the loop to X out the next point...
		   (setq lift-flag t)))))))

;;;###autoload
(defun strokes-list-strokes (&optional stroke-map)
  "Pop up a buffer containing a listing of all strokes defined in STROKE-MAP.
If STROKE-MAP is not given, `strokes-global-map' will be used instead."
  (interactive)
  (push-window-configuration)
  (set-buffer (get-buffer-create "*Strokes List*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert
   "Command                                     Stroke\n"
   "-------                                     ------\n\n")
  (loop for def in (or stroke-map strokes-global-map) do
	(let ((stroke (car def))
	      (command (cdr def)))
	  (strokes-xpm-for-stroke stroke " *strokes-xpm*")
	  (insert-char ?\ 60)
	  (beginning-of-line)
	  (insert (symbol-name command))
	  (beginning-of-line)
	  (forward-char 45)
	  (make-annotation (make-glyph
			    (list
			     (vector 'xpm
				     :data (buffer-substring
					    (point-min " *strokes-xpm*")
					    (point-max " *strokes-xpm*")
					    " *strokes-xpm*"))
			     [string :data "[Image]"]))
			   (point) 'text)
	  (newline 2)))
  (view-buffer "*Strokes List*" t)
  (goto-char (point-min))
;;  (define-key
;;    (current-local-map (get-buffer "*Strokes List*"))
;;    [(q)]
;;    'pop-window-configuration))
  )

;;;###autoload
(defalias 'list-strokes 'strokes-list-strokes)

;;;###autoload
(defun strokes-mode (&optional arg)
  "Toggle strokes being enabled.
With ARG, turn strokes on if and only if ARG is positive or true.
Note that `strokes-mode' is a global mode.  Think of it as a minor
mode in all buffers when activated.
By default, strokes are invoked with mouse button-2.  You can define
new strokes with

> M-x global-set-stroke"
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not strokes-mode))))
    (cond ((not (device-on-window-system-p))
	   (error "Can't use strokes without windows"))
	  (on-p				; turn on strokes
	   (and (file-exists-p strokes-file)
		(null strokes-global-map)
		(strokes-load-user-strokes))
	   (add-hook 'kill-emacs-hook
		     'strokes-prompt-user-save-strokes)
	   (add-hook 'select-frame-hook
		     'strokes-update-window-configuration)
	   (strokes-update-window-configuration)
	   (define-key global-map [(button2)] 'strokes-do-stroke)
	   (define-key global-map [(control button2)] 'strokes-do-stroke)
	   (define-key global-map [(shift button2)]
	     'strokes-do-complex-stroke)
	   (ad-activate-regexp "^strokes-") ; advise button2 commands
	   (setq strokes-mode t))
	  (t				; turn off strokes
	   (if (get-buffer strokes-buffer-name)
	       (kill-buffer (get-buffer strokes-buffer-name)))
	   (remove-hook 'select-frame-hook
			'strokes-update-window-configuration)
	   (if (string-match "^strokes-" (symbol-name (key-binding [(button2)])))
	       (define-key global-map [(button2)] strokes-click-command))
	   (if (string-match "^strokes-" (symbol-name (key-binding [(control button2)])))
	       (global-unset-key [(control button2)]))
	   (if (string-match "^strokes-" (symbol-name (key-binding [(shift button2)])))	
	       (global-unset-key [(shift button2)]))
	   (ad-deactivate-regexp "^strokes-") ; unadvise strokes-button2 commands
	   (setq strokes-mode nil))))
  (redraw-modeline))

(add-minor-mode 'strokes-mode strokes-modeline-string nil nil 'strokes-mode)

(provide 'strokes)
(run-hooks 'strokes-load-hook)

;;; strokes.el ends here
