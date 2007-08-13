;;; gnus-xmas.el --- Gnus functions for XEmacs
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

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

;;; Code:

(require 'text-props)
(eval-when-compile (require 'cl))
(defvar menu-bar-mode (featurep 'menubar))
(require 'messagexmas)

(defvar gnus-xmas-glyph-directory nil
  "*Directory where Gnus logos and icons are located.
If this variable is nil, Gnus will try to locate the directory
automatically.")

(defvar gnus-xmas-logo-color-alist
  '((flame "#cc3300" "#ff2200") 
    (pine "#c0cc93" "#f8ffb8") 
    (moss "#a1cc93" "#d2ffb8")
    (irish "#04cc90" "#05ff97")
    (sky "#049acc" "#05deff")
    (tin "#6886cc" "#82b6ff")
    (velvet "#7c68cc" "#8c82ff")
    (grape "#b264cc" "#cf7df")
    (labia "#cc64c2" "#fd7dff")
    (berry "#cc6485" "#ff7db5")
    (neutral "#b4b4b4" "#878787")
    (september "#bf9900" "#ffcc00"))
  "Color alist used for the Gnus logo.")

(defvar gnus-xmas-logo-color-style 'september
  "Color styles used for the Gnus logo.")

(defvar gnus-xmas-logo-colors
  (cdr (assq gnus-xmas-logo-color-style gnus-xmas-logo-color-alist))
  "Colors used for the Gnus logo.")

(defvar gnus-article-x-face-command
  (if (featurep 'xface)
      'gnus-xmas-article-display-xface
    "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | xv -quit -")
  "String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command.")

;;; Internal variables.

;; Don't warn about these undefined variables.

(defvar gnus-group-mode-hook)
(defvar gnus-summary-mode-hook)
(defvar gnus-article-mode-hook)

;;defined in gnus.el
(defvar gnus-active-hashtb)
(defvar gnus-article-buffer)
(defvar gnus-auto-center-summary)
(defvar gnus-buffer-list)
(defvar gnus-current-headers)
(defvar gnus-level-killed)
(defvar gnus-level-zombie)
(defvar gnus-newsgroup-bookmarks)
(defvar gnus-newsgroup-dependencies)
(defvar gnus-newsgroup-selected-overlay)
(defvar gnus-newsrc-hashtb)
(defvar gnus-read-mark)
(defvar gnus-refer-article-method)
(defvar gnus-reffed-article-number)
(defvar gnus-unread-mark)
(defvar gnus-version)
(defvar gnus-view-pseudos)
(defvar gnus-view-pseudos-separately)
(defvar gnus-visual)
(defvar gnus-zombie-list)
;;defined in gnus-msg.el
(defvar gnus-article-copy)
(defvar gnus-check-before-posting)
;;defined in gnus-vis.el
(defvar gnus-article-button-face)
(defvar gnus-article-mouse-face)
(defvar gnus-summary-selected-face)
(defvar gnus-group-reading-menu)
(defvar gnus-group-group-menu)
(defvar gnus-group-misc-menu)
(defvar gnus-summary-article-menu)
(defvar gnus-summary-thread-menu)
(defvar gnus-summary-misc-menu)
(defvar gnus-summary-post-menu)
(defvar gnus-summary-kill-menu)
(defvar gnus-article-article-menu)
(defvar gnus-article-treatment-menu)
(defvar gnus-mouse-2)
(defvar standard-display-table)
(defvar gnus-tree-minimize-window)

(defun gnus-xmas-set-text-properties (start end props &optional buffer)
  "You should NEVER use this function.  It is ideologically blasphemous.
It is provided only to ease porting of broken FSF Emacs programs."
  (if (stringp buffer) 
      nil
    (map-extents (lambda (extent ignored)
		   (remove-text-properties 
		    start end
		    (list (extent-property extent 'text-prop) nil)
		    buffer))
		 buffer start end nil nil 'text-prop)
    (gnus-add-text-properties start end props buffer)))

(defun gnus-xmas-highlight-selected-summary ()
  ;; Highlight selected article in summary buffer
  (when gnus-summary-selected-face
    (if gnus-newsgroup-selected-overlay
	(delete-extent gnus-newsgroup-selected-overlay))
    (setq gnus-newsgroup-selected-overlay 
	  (make-extent (gnus-point-at-bol) (gnus-point-at-eol)))
    (set-extent-face gnus-newsgroup-selected-overlay
		     gnus-summary-selected-face)))

(defun gnus-xmas-summary-recenter ()
  "\"Center\" point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed."
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.  Suggested by popovich@park.cs.columbia.edu.
  (when gnus-auto-center-summary
    (let* ((height (if (fboundp 'window-displayed-height)
		       (window-displayed-height)
		     (- (window-height) 2)))
	   (top (cond ((< height 4) 0)
		      ((< height 7) 1)
		      (t 2)))
	   (bottom (save-excursion (goto-char (point-max))
				   (forward-line (- height))
				   (point)))
	   (window (get-buffer-window (current-buffer))))
      (when (get-buffer-window gnus-article-buffer)
	;; Only do recentering when the article buffer is displayed,
	;; Set the window start to either `bottom', which is the biggest
	;; possible valid number, or the second line from the top,
	;; whichever is the least.
	(set-window-start
	 window (min bottom (save-excursion 
			      (forward-line (- top)) (point)))))
      ;; Do horizontal recentering while we're at it.
      (when (and (get-buffer-window (current-buffer) t)
		 (not (eq gnus-auto-center-summary 'vertical)))
	(let ((selected (selected-window)))
	  (select-window (get-buffer-window (current-buffer) t))
	  (gnus-summary-position-point)
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-xmas-add-hook (hook function &optional append local)
  (add-hook hook function))

(defun gnus-xmas-add-text-properties (start end props &optional object)
  (add-text-properties start end props object)
  (put-text-property start end 'start-closed nil object))

(defun gnus-xmas-put-text-property (start end prop value &optional object)
  (put-text-property start end prop value object)
  (put-text-property start end 'start-closed nil object))

(defun gnus-xmas-extent-start-open (point)
  (map-extents (lambda (extent arg)
		 (set-extent-property extent 'start-open t))
	       nil point (min (1+ (point)) (point-max))))
		  
(defun gnus-xmas-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (let* ((pos (event-closest-point event))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (if fun (funcall fun data))))

(defun gnus-xmas-move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end))

;; Fixed by Christopher Davis <ckd@loiosh.kei.com>.
(defun gnus-xmas-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to) 
			 'face gnus-article-button-face))
  (gnus-add-text-properties 
   from to
   (nconc
    (and gnus-article-mouse-face
	 (list 'mouse-face gnus-article-mouse-face))
    (list 'gnus-callback fun)
    (and data (list 'gnus-data data))
    (list 'highlight t))))

(defun gnus-xmas-window-top-edge (&optional window)
  (nth 1 (window-pixel-edges window)))

(defun gnus-xmas-tree-minimize ()
  (when (and gnus-tree-minimize-window
	     (not (one-window-p)))
    (let* ((window-min-height 2)
	   (height (1+ (count-lines (point-min) (point-max))))
	   (min (max (1- window-min-height) height))
	   (tot (if (numberp gnus-tree-minimize-window)
		    (min gnus-tree-minimize-window min)
		  min))
	   (win (get-buffer-window (current-buffer)))
	   (wh (and win (1- (window-height win)))))
      (when (and win
		 (not (eq tot wh)))
	(let ((selected (selected-window)))
	  (select-window win)
	  (enlarge-window (- tot wh))
	  (select-window selected))))))

;; Select the lowest window on the frame.
(defun gnus-xmas-appt-select-lowest-window ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-pixel-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr 
                                               (window-pixel-edges 
						this-window)))))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))

        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))

(defmacro gnus-xmas-menu-add (type &rest menus)
  `(gnus-xmas-menu-add-1 ',type ',menus))
(put 'gnus-xmas-menu-add 'lisp-indent-function 1)
(put 'gnus-xmas-menu-add 'lisp-indent-hook 1)

(defun gnus-xmas-menu-add-1 (type menus)
  (when (and menu-bar-mode
	     (gnus-visual-p (intern (format "%s-menu" type)) 'menu))
    (while menus
      (easy-menu-add (symbol-value (pop menus))))))

(defun gnus-xmas-group-menu-add ()
  (gnus-xmas-menu-add group
    gnus-group-reading-menu gnus-group-group-menu gnus-group-misc-menu))

(defun gnus-xmas-summary-menu-add ()
  (gnus-xmas-menu-add summary
    gnus-summary-misc-menu gnus-summary-kill-menu
    gnus-summary-article-menu gnus-summary-thread-menu
    gnus-summary-post-menu ))

(defun gnus-xmas-article-menu-add ()
  (gnus-xmas-menu-add article
    gnus-article-article-menu gnus-article-treatment-menu))

(defun gnus-xmas-score-menu-add ()
  (gnus-xmas-menu-add score
    gnus-score-menu))

(defun gnus-xmas-pick-menu-add ()
  (gnus-xmas-menu-add pick
    gnus-pick-menu))

(defun gnus-xmas-binary-menu-add ()
  (gnus-xmas-menu-add binary
    gnus-binary-menu))

(defun gnus-xmas-tree-menu-add ()
  (gnus-xmas-menu-add tree
    gnus-tree-menu))

(defun gnus-xmas-server-menu-add ()
  (gnus-xmas-menu-add menu
    gnus-server-server-menu gnus-server-connections-menu))

(defun gnus-xmas-browse-menu-add ()
  (gnus-xmas-menu-add browse
    gnus-browse-menu))

(defun gnus-xmas-grouplens-menu-add ()
  (gnus-xmas-menu-add grouplens
    gnus-grouplens-menu))

(defun gnus-xmas-read-event-char ()
  "Get the next event."
  (let ((event (next-event)))
    ;; We junk all non-key events.  Is this naughty?
    (while (not (key-press-event-p event))
      (setq event (next-event)))
    (cons (and (key-press-event-p event) 
	      ; (numberp (event-key event))
	       (event-to-character event)) 
	  event)))

(defun gnus-xmas-group-remove-excess-properties ()
  (let ((end (point))
	(beg (progn (forward-line -1) (point))))
    (remove-text-properties (1+ beg) end '(gnus-group nil))
    (remove-text-properties 
     beg end 
     '(gnus-topic nil gnus-topic-level nil gnus-topic-visible nil))
    (goto-char end)
    (map-extents 
     (lambda (e ma)
       (set-extent-property e 'start-closed t))
     (current-buffer) beg end)))
		  
(defun gnus-xmas-topic-remove-excess-properties ()
  (let ((end (point))
	(beg (progn (forward-line -1) (point))))
    (remove-text-properties beg end '(gnus-group nil gnus-unread nil))
    (remove-text-properties (1+ beg) end '(gnus-topic nil))
    (goto-char end)))

(defun gnus-xmas-seconds-since-epoch (date)
  "Return a floating point number that says how many seconds have lapsed between Jan 1 12:00:00 1970 and DATE."
  (let* ((tdate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date date)))
	 (ttime (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (edate (mapcar (lambda (ti) (and ti (string-to-int ti)))
			(timezone-parse-date "Jan 1 12:00:00 1970")))
	 (tday (- (timezone-absolute-from-gregorian 
		   (nth 1 tdate) (nth 2 tdate) (nth 0 tdate))
		  (timezone-absolute-from-gregorian 
		   (nth 1 edate) (nth 2 edate) (nth 0 edate)))))
    (+ (nth 2 ttime)
       (* (nth 1 ttime) 60)
       (* (float (nth 0 ttime)) 60 60)
       (* (float tday) 60 60 24))))

(defun gnus-xmas-define ()
  (setq gnus-mouse-2 [button2])

  (or (memq 'underline (face-list))
      (and (fboundp 'make-face)
	   (funcall (intern "make-face") 'underline)))
  ;; Must avoid calling set-face-underline-p directly, because it
  ;; is a defsubst in emacs19, and will make the .elc files non
  ;; portable!
  (or (face-differs-from-default-p 'underline)
      (funcall (intern "set-face-underline-p") 'underline t))

  (fset 'gnus-make-overlay 'make-extent)
  (fset 'gnus-overlay-put 'set-extent-property)
  (fset 'gnus-move-overlay 'gnus-xmas-move-overlay)
  (fset 'gnus-overlay-end 'extent-end-position)
  (fset 'gnus-extent-detached-p 'extent-detached-p)
  (fset 'gnus-add-text-properties 'gnus-xmas-add-text-properties)
  (fset 'gnus-put-text-property 'gnus-xmas-put-text-property)
      
  (require 'text-props)
  (if (< emacs-minor-version 14)
      (fset 'gnus-set-text-properties 'gnus-xmas-set-text-properties))

  (or (boundp 'standard-display-table) (setq standard-display-table nil))

  (defvar gnus-mouse-face-prop 'highlight)

  (unless (fboundp 'encode-time)
    (defun encode-time (sec minute hour day month year &optional zone)
      (let ((seconds
	     (gnus-xmas-seconds-since-epoch
	      (timezone-make-arpa-date 
	       year month day (timezone-make-time-string hour minute sec)
	       zone))))
	(list (floor (/ seconds (expt 2 16)))
	      (round (mod seconds (expt 2 16)))))))
      
  (defun gnus-byte-code (func)
    "Return a form that can be `eval'ed based on FUNC."
    (let ((fval (symbol-function func)))
      (if (compiled-function-p fval)
	  (list 'funcall fval)
	(cons 'progn (cdr (cdr fval))))))
      
  ;; Fix by "jeff (j.d.) sparkes" <jsparkes@bnr.ca>.
  (defvar gnus-display-type (device-class)
    "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'. If Emacs
guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your
`~/.Xdefaults'. See also `gnus-background-mode'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")


  (fset 'gnus-x-color-values 
	(if (fboundp 'x-color-values)
	    'x-color-values
	  (lambda (color)
	    (color-instance-rgb-components
	     (make-color-instance color)))))
    
  (defvar gnus-background-mode 
    (let* ((bg-resource 
	    (condition-case ()
		(x-get-resource ".backgroundMode" "BackgroundMode" 'string)
	      (error nil)))
	   (params (frame-parameters))
	   (color (condition-case ()
		      (or (assq 'background-color params)
			  (color-instance-name
			   (specifier-instance
			    (face-background 'default))))
		    (error nil))))
      (cond (bg-resource (intern (downcase bg-resource)))
	    ((and color
		  (< (apply '+ (gnus-x-color-values color))
		     (/ (apply '+ (gnus-x-color-values "white")) 3)))
	     'dark)
	    (t 'light)))
    "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `gnus-display-type'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")
  )



(defun gnus-xmas-redefine ()
  "Redefine lots of Gnus functions for XEmacs."
  (fset 'gnus-summary-make-display-table 'ignore)
  (fset 'gnus-visual-turn-off-edit-menu 'identity)
  (fset 'gnus-summary-recenter 'gnus-xmas-summary-recenter)
  (fset 'gnus-extent-start-open 'gnus-xmas-extent-start-open)
  (fset 'gnus-article-push-button 'gnus-xmas-article-push-button)
  (fset 'gnus-article-add-button 'gnus-xmas-article-add-button)
  (fset 'gnus-window-top-edge 'gnus-xmas-window-top-edge)
  (fset 'gnus-read-event-char 'gnus-xmas-read-event-char)
  (fset 'gnus-group-startup-message 'gnus-xmas-group-startup-message)
  (fset 'gnus-tree-minimize 'gnus-xmas-tree-minimize)
  (fset 'gnus-appt-select-lowest-window 
	'gnus-xmas-appt-select-lowest-window)
  (fset 'gnus-mail-strip-quoted-names 'gnus-xmas-mail-strip-quoted-names)
  (fset 'gnus-make-local-hook 'make-local-variable)
  (fset 'gnus-add-hook 'gnus-xmas-add-hook)
  (fset 'gnus-character-to-event 'character-to-event)
  (fset 'gnus-article-show-hidden-text 'gnus-xmas-article-show-hidden-text)
  (fset 'gnus-mode-line-buffer-identification
	'gnus-xmas-mode-line-buffer-identification)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-group-menu-add)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-summary-menu-add)
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add)
  (add-hook 'gnus-score-mode-hook 'gnus-xmas-score-menu-add)

  (add-hook 'gnus-pick-mode-hook 'gnus-xmas-pick-menu-add)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-tree-menu-add)
  (add-hook 'gnus-binary-mode-hook 'gnus-xmas-binary-menu-add)
  (add-hook 'gnus-grouplens-mode-hook 'gnus-xmas-grouplens-menu-add)
  (add-hook 'gnus-server-mode-hook 'gnus-xmas-server-menu-add)
  (add-hook 'gnus-browse-mode-hook 'gnus-xmas-browse-menu-add)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-setup-group-toolbar)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-setup-summary-toolbar)

  (when (and (<= emacs-major-version 19)
	     (<= emacs-minor-version 13))
    (setq gnus-article-x-face-too-ugly (if (eq (device-type) 'tty) "."))
    (fset 'gnus-highlight-selected-summary
	  'gnus-xmas-highlight-selected-summary)
    (fset 'gnus-group-remove-excess-properties
	  'gnus-xmas-group-remove-excess-properties)
    (fset 'gnus-topic-remove-excess-properties
	  'gnus-xmas-topic-remove-excess-properties)
    (fset 'gnus-mode-line-buffer-identification 'identity)
    (unless (boundp 'shell-command-switch)
      (setq shell-command-switch "-c"))
    ))


;;; XEmacs logo and toolbar.

(defun gnus-xmas-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (setq gnus-xmas-glyph-directory (message-xmas-find-glyph-directory "gnus"))
  (erase-buffer)
  (let ((logo (and gnus-xmas-glyph-directory
		   (concat 
		    (file-name-as-directory gnus-xmas-glyph-directory)
		    "gnus."
		    (if (featurep 'xpm) "xpm" "xbm"))))
	(xpm-color-symbols 
	 (and (featurep 'xpm)
	      (append `(("thing" ,(car gnus-xmas-logo-colors))
			("shadow" ,(cadr gnus-xmas-logo-colors)))
		      xpm-color-symbols))))
    (if (and (featurep 'xpm)
	     (not (equal (device-type) 'tty))
	     logo
	     (file-exists-p logo))
	(progn
	  (setq logo (make-glyph logo))
	  (insert " ")
	  (set-extent-begin-glyph (make-extent (point) (point)) logo)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert (make-string (/ (max (- (window-width) (or x 35)) 0) 2)
				 ? ))
	    (forward-line 1))
	  (goto-char (point-min))
	  (let* ((pheight (+ 20 (count-lines (point-min) (point-max))))
		 (wheight (window-height))
		 (rest (- wheight pheight)))
	    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n))))

      (insert
       (format "              %s
          _    ___ _             _      
          _ ___ __ ___  __    _ ___     
          __   _     ___    __  ___     
              _           ___     _     
             _  _ __             _      
             ___   __            _      
                   __           _       
                    _      _   _        
                   _      _    _        
                      _  _    _         
                  __  ___               
                 _   _ _     _          
                _   _                   
              _    _                    
             _    _                     
            _                         
          __                             

" 
	       ""))
      ;; And then hack it.
      (gnus-indent-rigidly (point-min) (point-max) 
			   (/ (max (- (window-width) (or x 46)) 0) 2))
      (goto-char (point-min))
      (forward-line 1)
      (let* ((pheight (count-lines (point-min) (point-max)))
	     (wheight (window-height))
	     (rest (- wheight pheight)))
	(insert (make-string (max 0 (* 2 (/ rest 3))) ?\n))))
    ;; Fontify some.
    (goto-char (point-min))
    (and (search-forward "Praxis" nil t)
	 (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
    (goto-char (point-min))
    (let* ((mode-string (gnus-group-set-mode-line)))
      (setq modeline-buffer-identification 
	    (list (concat gnus-version ": *Group*")))
      (set-buffer-modified-p t))))


;;; The toolbar.

(defvar gnus-use-toolbar (if (featurep 'toolbar)
			     'default-toolbar
			   nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five legal values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar'.")

(defvar gnus-group-toolbar 
  '(
    [gnus-group-get-new-news gnus-group-get-new-news t "Get new news"]
    [gnus-group-get-new-news-this-group 
     gnus-group-get-new-news-this-group t "Get new news in this group"]
    [gnus-group-catchup-current 
     gnus-group-catchup-current t "Catchup group"]
    [gnus-group-describe-group 
     gnus-group-describe-group t "Describe group"]
    [gnus-group-kill-group gnus-group-kill-group t "Kill group"]
    [gnus-group-exit gnus-group-exit t "Exit Gnus"]
    )
  "The group buffer toolbar.")

(defvar gnus-summary-toolbar 
  '(
    [gnus-summary-prev-unread 
     gnus-summary-prev-unread-article t "Prev unread article"]
    [gnus-summary-next-unread 
     gnus-summary-next-unread-article t "Next unread article"]
    [gnus-summary-post-news 
     gnus-summary-post-news t "Post an article"]
    [gnus-summary-followup-with-original
     gnus-summary-followup-with-original t 
     "Post a followup and yank the original"]
    [gnus-summary-followup 
     gnus-summary-followup t "Post a followup"]
    [gnus-summary-reply-with-original
     gnus-summary-reply-with-original t "Mail a reply and yank the original"]
    [gnus-summary-reply 
     gnus-summary-reply t "Mail a reply"]
    [gnus-summary-caesar-message
     gnus-summary-caesar-message t "Rot 13"]
    [gnus-uu-decode-uu
     gnus-uu-decode-uu t "Decode uuencoded articles"]
    [gnus-summary-save-article-file
     gnus-summary-save-article-file t "Save article in file"]
    [gnus-summary-save-article
     gnus-summary-save-article t "Save article"]
    [gnus-uu-post-news 
     gnus-uu-post-news t "Post an uuencoded article"]
    [gnus-summary-cancel-article
     gnus-summary-cancel-article t "Cancel article"]
    [gnus-summary-catchup-and-exit
     gnus-summary-catchup-and-exit t "Catchup and exit"]
    [gnus-summary-exit gnus-summary-exit t "Exit this summary"]
    )
  "The summary buffer toolbar.")

(defvar gnus-summary-mail-toolbar
  '(
    [gnus-summary-prev-unread 
     gnus-summary-prev-unread-article t "Prev unread article"]
    [gnus-summary-next-unread 
     gnus-summary-next-unread-article t "Next unread article"]
    [gnus-summary-mail-reply gnus-summary-reply t "Reply"]
    [gnus-summary-mail-get gnus-mail-get t "Message get"]
    [gnus-summary-mail-originate gnus-summary-post-news t "Originate"]
    [gnus-summary-mail-save gnus-summary-save-article t "Save"]
    [gnus-summary-mail-copy gnus-summary-copy-article t "Copy message"]
;    [gnus-summary-mail-delete gnus-summary-delete-article t "Delete message"]
    [gnus-summary-mail-forward gnus-summary-mail-forward t "Forward message"]
;    [gnus-summary-mail-spell gnus-mail-spell t "Spell"]
;    [gnus-summary-mail-help gnus-mail-help  t "Message help"]
    [gnus-summary-caesar-message
     gnus-summary-caesar-message t "Rot 13"]
    [gnus-uu-decode-uu
     gnus-uu-decode-uu t "Decode uuencoded articles"]
    [gnus-summary-save-article-file
     gnus-summary-save-article-file t "Save article in file"]
    [gnus-summary-save-article
     gnus-summary-save-article t "Save article"]
    [gnus-summary-catchup-and-exit
     gnus-summary-catchup-and-exit t "Catchup and exit"]
    [gnus-summary-exit gnus-summary-exit t "Exit this summary"]
    )
  "The summary buffer mail toolbar.")

(defun gnus-xmas-setup-group-toolbar ()
  (and gnus-use-toolbar
       (message-xmas-setup-toolbar gnus-group-toolbar nil "gnus")
       (set-specifier (symbol-value gnus-use-toolbar)
		      (cons (current-buffer) gnus-group-toolbar))))

(defun gnus-xmas-setup-summary-toolbar ()
  (let ((bar (if (gnus-news-group-p gnus-newsgroup-name)
		 gnus-summary-toolbar gnus-summary-mail-toolbar)))
    (and gnus-use-toolbar
	 (message-xmas-setup-toolbar bar nil "gnus")
	 (set-specifier (symbol-value gnus-use-toolbar)
			(cons (current-buffer) bar)))))

(defun gnus-xmas-mail-strip-quoted-names (address)
  "Protect mail-strip-quoted-names from NIL input.
XEmacs compatibility workaround."
  (if (null address)
      nil
    (mail-strip-quoted-names address)))

(defun gnus-xmas-call-region (command &rest args)
  (apply
   'call-process-region (point-min) (point-max) command t '(t nil) nil
   args))

(unless (find-face 'gnus-x-face)
  (copy-face 'default 'gnus-x-face)
  (set-face-foreground 'gnus-x-face "black")
  (set-face-background 'gnus-x-face "white"))

(defun gnus-xmas-article-display-xface (beg end)
  "Display any XFace headers in the current article."
  (save-excursion
    (let (xface-glyph)
      (if (featurep 'xface)
	  (setq xface-glyph
		(make-glyph (vector 'xface :data 
				    (concat "X-Face: "
					    (buffer-substring beg end)))))
	(let ((cur (current-buffer)))
	  (save-excursion
	    (gnus-set-work-buffer)
	    (insert (format "%s" (buffer-substring beg end cur)))
	    (gnus-xmas-call-region "uncompface")
	    (goto-char (point-min))
	    (insert "/* Width=48, Height=48 */\n")
	    (gnus-xmas-call-region "icontopbm")
	    (gnus-xmas-call-region "ppmtoxpm")
	    (setq xface-glyph
		  (make-glyph
		   (vector 'xpm :data (buffer-string )))))))
      (set-glyph-face xface-glyph 'gnus-x-face)
      (goto-char (point-min))
      (re-search-forward "^From:" nil t)
      (set-extent-begin-glyph 
       (make-extent (point) (1+ (point))) xface-glyph))))

(defun gnus-xmas-article-show-hidden-text (type &optional hide)
  "Show all hidden text of type TYPE.
If HIDE, hide the text instead."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (beg (point-min)))
      (while (gnus-goto-char (text-property-any
			      beg (point-max) 'gnus-type type))
	(setq beg (point))
	(forward-char)
	(if hide
	    (gnus-hide-text beg (point) gnus-hidden-properties)
	  (gnus-unhide-text beg (point)))
	(setq beg (point)))
      (save-window-excursion
	(select-window (get-buffer-window (current-buffer)))
	(recenter))
      t)))

(defun gnus-xmas-mode-line-buffer-identification (line)
  (let ((line (car line))
	chop)
    (if (not (stringp line))
	(list line)
      (unless (setq chop (string-match ":" line))
	(setq chop (/ (length line) 2)))
      (list (cons modeline-buffer-id-left-extent (substring line 0 chop))
	    (cons modeline-buffer-id-right-extent (substring line chop))))))

(provide 'gnus-xmas)

;;; gnus-xmas.el ends here
