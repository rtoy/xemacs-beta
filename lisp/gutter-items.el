;;; gutter-items.el --- Gutter content for XEmacs.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: frames, extensions, internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Gutter-specific buffers tab code

(defgroup buffers-tab nil
  "Customization of `Buffers' tab."
  :group 'gutter)

(defvar gutter-buffers-tab nil
  "A tab widget in the gutter for displaying buffers.
Do not set this. Use `set-glyph-image' to change the properties of the tab.")

(defcustom gutter-buffers-tab-visible-p
  (gutter-element-visible-p default-gutter-visible-p 'buffers-tab)
  "Whether the buffers tab is globally visible. 
This option should be set through the options menu."
  :group 'buffers-tab
  :type 'boolean
  :set #'(lambda (var val)
	   (set-gutter-element-visible-p default-gutter-visible-p 
					 'buffers-tab val)
	   (setq gutter-buffers-tab-visible-p val)))

(defcustom gutter-buffers-tab-enabled t
  "*Whether to enable support for buffers tab in the gutter.
This is different to `gutter-buffers-tab-visible-p' which still runs hooks
even when the gutter is invisible."
  :group 'buffers-tab
  :type 'boolean)

(defvar gutter-buffers-tab-orientation 'top
  "Where the buffers tab currently is. Do not set this.")

(defcustom buffers-tab-max-size 6
  "*Maximum number of entries which may appear on the \"Buffers\" tab.
If this is 10, then only the ten most-recently-selected buffers will be
shown.  If this is nil, then all buffers will be shown.  Setting this to
a large number or nil will slow down tab responsiveness."
  :type '(choice (const :tag "Show all" nil)
		 (integer 6))
  :group 'buffers-tab)

(defcustom buffers-tab-switch-to-buffer-function 'buffers-tab-switch-to-buffer
  "*The function to call to select a buffer from the buffers tab.
`switch-to-buffer' is a good choice, as is `pop-to-buffer'."
  :type '(radio (function-item switch-to-buffer)
		(function-item pop-to-buffer)
		(function :tag "Other"))
  :group 'buffers-tab)

(defcustom buffers-tab-omit-function 'buffers-tab-omit-some-buffers
  "*If non-nil, a predicate matching buffers to omit from the buffers tab.
This is passed a buffer and should return non-nil if the buffer should be
omitted.  The default value `buffers-tab-omit-some-buffers' omits
buffers with names matched by any regexp in `buffers-tab-omit-list'."
  :type '(choice (const :tag "None" nil)
  		 function)
  :group 'buffers-tab)

(defcustom buffers-tab-omit-list '("\\` ")
  "*A list of regexps matching names of buffers to omit from the buffers tab.
This is used by `buffers-tab-omit-some-buffers' \(the default value of
`buffers-tab-omit-function')."
  :type '(checklist
	  :greedy t
	  :format "%{Omit List%}: \n%v"
	  (const
	   :tag "Invisible buffers (those whose names start with a space) "
	   "\\` ")
	  (const
	   :tag "Help buffers "
	   "\\`\\*Help")
	  (const
	   :tag "Customize buffers "
	   "\\`\\*Customize")
	  (const
	   :tag "`special' buffers (those whose names start with *) "
	   "\\`\\*")
	  (const
	   :tag "`special' buffers other than *scratch*"
	   "\\`\\*\\([^s]\\|s[^c]\\|sc[^r]\\|scr[^a]\\|scra[^t]\\|scrat[^c]\\|scratc[^h]\\|scratch[^*]\\|scratch\\*.+\\)"))
  :group 'buffers-tab)

(defvar buffers-tab-selection-function 'select-buffers-tab-buffers-by-mode
  "*If non-nil, a predicate selecting buffers eligible for the buffers tab.

This is passed a buffer to test and a comparison buffer, returning non-nil if
the first buffer should be selected.  The default value
`select-buffers-tab-buffers-by-mode' groups buffers by major mode and
by `buffers-tab-grouping-regexp'.")

(make-obsolete-variable buffers-tab-selection-function
			"Set `buffers-tab-filter-functions' instead.")

(defcustom buffers-tab-filter-functions (list 'select-buffers-tab-buffers-by-mode)
  "*A list of predicates selecting buffers eligible for the buffers tab.

If nil, all buffers are kept, up to `buffers-tab-max-size', in usual order.
Otherwise, each function in the list must take arguments (BUF1 BUF2).
BUF1 is the candidate, and BUF2 is a comparison buffer \(usually the current
buffer).  The function returns non-nil to request that BUF1 be added to the
buffers tab.  BUF1 will be omitted if any of the functions returns nil.

Defaults to `select-buffers-tab-buffers-by-mode', which adds BUF1 if BUF1 and
BUF2 have the same major mode, or both match `buffers-tab-grouping-regexp'."

  :type '(repeat function)
  :group 'buffers-tab)

(defcustom buffers-tab-sort-function nil
  "*If non-nil, a function specifying precedence of buffers in the buffers tab.

This is passed the buffer list and returns the list in the order desired for
the tab widget.  The default value `nil' leaves the list in `buffer-list'
order (usual most-recently-selected-first)."

  :type '(choice (const :tag "None" nil)
		 function)
  :group 'buffers-tab)

(make-face 'buffers-tab "Face for displaying the buffers tab.")
(set-face-parent 'buffers-tab 'modeline)

(defcustom buffers-tab-face 'buffers-tab
  "*Face to use for displaying the buffers tab."
  :type 'face
  :group 'buffers-tab)

(defcustom buffers-tab-grouping-regexp 
  '("^\\(gnus-\\|message-mode\\|mime/viewer-mode\\)"
    "^\\(emacs-lisp-\\|lisp-\\)")
  "*If non-nil, a list of regular expressions for buffer grouping.

If two buffers' modes or mode names both are matched by any regular expression
in the list, they are considered equivalent when selecting buffers by major
mode."
  :type '(choice (const :tag "None" nil)
		 sexp)
  :group 'buffers-tab)

(defcustom buffers-tab-format-buffer-line-function 'format-buffers-tab-line
  "*A function returning a string representing the buffer passed.

The default value `format-buffers-tab-line' returns the name of the buffer,
truncated to `buffers-tab-max-buffer-line-length'.  An alternative is
`slow-format-buffers-menu-line' which returns more info about the buffer."
  :type 'function
  :group 'buffers-tab)

(defvar buffers-tab-default-buffer-line-length
  (make-specifier-and-init 'generic '((global ((default) . 25))) t)
  "*Maximum length of text which may appear in a \"Buffers\" tab.
This is a specifier, use set-specifier to modify it.")

(defcustom buffers-tab-max-buffer-line-length 
  (specifier-instance buffers-tab-default-buffer-line-length)
  "*Maximum length of text which may appear in a \"Buffers\" tab.
Buffer names over this length will be truncated with elipses.
If this is 0, then the full buffer name will be shown."
  :type '(choice (const :tag "Show all" 0)
		 (integer 25))
  :group 'buffers-tab
  :set #'(lambda (var val)
	   (set-specifier buffers-tab-default-buffer-line-length val)
	   (setq buffers-tab-max-buffer-line-length val)))

(defun buffers-tab-omit-some-buffers (buf)
  "For use as a value of `buffers-tab-omit-function'.
Omit buffers based on the value of `buffers-tab-omit-list', which
see."
  (let ((regexp (mapconcat 'concat buffers-tab-omit-list "\\|")))
    (not (null (string-match regexp (buffer-name buf))))))

(defun buffers-tab-switch-to-buffer (buffer)
  "For use as a value for `buffers-tab-switch-to-buffer-function'."
  (unless (eq (window-buffer) buffer)
    ;; this used to add the norecord flag to both calls below.
    ;; this is bogus because it is a pervasive assumption in XEmacs
    ;; that the current buffer is at the front of the buffers list.
    ;; for example, select an item and then do M-C-l
    ;; (switch-to-other-buffer).  Things get way confused.
    (let ((window (get-buffer-window buffer)))
      (if window
          (select-window window)
        (switch-to-buffer buffer)))))

(defun select-buffers-tab-buffers-by-mode (buffer-to-select buf1)
  "For use as an element of `buffers-tab-filter-functions'.
This selects buffers by major mode `buffers-tab-grouping-regexp'."
  (let ((mode1 (symbol-name (symbol-value-in-buffer 'major-mode buf1)))
	(mode2 (symbol-name (symbol-value-in-buffer 'major-mode 
						    buffer-to-select)))
	(modenm1 (symbol-value-in-buffer 'mode-name buf1))
	(modenm2 (symbol-value-in-buffer 'mode-name buffer-to-select))
        position)
    (cond ((or (eq mode1 mode2)
	       (eq modenm1 modenm2)
	       (and (> (or (setq position (position ?- mode1)) -1) 0)
                    (eql (1+ position) (mismatch mode1 mode2)))
	       (and buffers-tab-grouping-regexp
		    (find-if #'(lambda (x)
				 (or
				  (and (string-match x mode1)
				       (string-match x mode2))
				  (and (string-match x modenm1)
				       (string-match x modenm2))))
			     buffers-tab-grouping-regexp)))
	   t)
	  (t nil))))

(defun format-buffers-tab-line (buffer)
  "Return BUFFER's name, truncated to `buffers-tab-default-buffer-line-length'.
For use as a value of `buffers-tab-format-buffer-line-function'."

  (let* ((len (specifier-instance buffers-tab-default-buffer-line-length))
         (buffer-name (buffer-name buffer))
         (length (length buffer-name)))
    (if (> length len 0) 
	(if (and (eql ?< (aref buffer-name (- length 3)))
                 (eql ?> (aref buffer-name (- length 1))))
            ; (string-match ".*<.>$" (buffer-name buffer))
	    (concat (subseq buffer-name 0 (- len 6)) "..."
                    (subseq buffer-name (- length 3)))
	  (concat (subseq buffer-name 0 (- len 3)) "..."))
      buffer-name)))

;;; #### SJT would like this function to have a sort function list. I
;;; don't see how this could work given that sorting is not
;;; cumulative --andyp.
(defun buffers-tab-items (&optional in-deletion frame force-selection)
  "Return a list of tab instantiators based on the current buffers list.

This is the tab filter for the top-level buffers \"Buffers\" tab.
It dynamically creates a list of tab instantiators to use as the contents of
the tab.  The contents and order of the list is controlled by
`buffers-tab-filter-functions' which by default groups buffers according to
major mode and removes invisible buffers.  At most `buffers-tab-max-size'
tabs will be added to the control.  The label for each tab is produced by
`format-buffers-menu-line'.

Optional IN-DELETION is an internal flag indicating a buffer is being deleted.
Optional FRAME is the frame whose buffer list is used, defaulting to the
selected frame.
Optional FORCE-SELECTION makes the currently selected window first in list."
  (save-match-data
    ;; NB it is too late if we run the omit function as part of the
    ;; filter functions because we need to know which buffer is the
    ;; context buffer before they get run.
    (let* ((buffers (delete-if 
		     buffers-tab-omit-function (buffer-list frame)))
	   (first-buf (car buffers))
	   tail)
      ;; maybe force the selected window
      (when (and force-selection
		 (not in-deletion)
		 (not (eq first-buf (window-buffer (selected-window frame)))))
	(setq buffers (cons (window-buffer (selected-window frame))
			    (delete* first-buf buffers))))
      ;; if we're in deletion ignore the current buffer
      (when in-deletion 
	(setq buffers (delete* (current-buffer) buffers))
	(setq first-buf (car buffers)))
      ;; filter buffers
      (when buffers-tab-filter-functions
	(setq buffers
              (mapcan
               #'(lambda (buffer)
                   (and (every #'(lambda (function)
                                   (funcall function buffer first-buf))
                               buffers-tab-filter-functions)
                        (list buffer)))
               buffers)))
      ;; maybe shorten list of buffers
      (let ((n (1- 
                ;; Error on non-number, non-nil buffers-tab-max-size
                (or buffers-tab-max-size most-positive-fixnum))))
        (and (> n 0)
             (setf tail (nthcdr n buffers)) ;; Length greater than (1+ n)?
             (setf (cdr tail) nil)))
      ;; sort buffers in group (default is most-recently-selected)
      (when buffers-tab-sort-function
	(setq buffers (funcall buffers-tab-sort-function buffers)))
      (labels
          ((build-buffers-tab-internal (buffers)
             "Convert BUFFERS to a list of structures used by the tab widget."
             (let ((selected t))
               (mapcar
                #'(lambda (buffer)
                    (prog1
                        `[,(funcall buffers-tab-format-buffer-line-function
                                    buffer)
                          (,buffers-tab-switch-to-buffer-function ,buffer)
                          :selected ,selected]
                      (when selected (setq selected nil))))
                buffers))))
        (declare (inline build-buffers-tab-internal))
	(build-buffers-tab-internal buffers)))))

(defun add-tab-to-gutter ()
  "Put a tab control in the gutter area to select buffers."
  (setq gutter-buffers-tab-orientation (default-gutter-position))
  (let* ((gutter-string (copy-sequence "\n"))
	 (gutter-buffers-tab-extent (make-extent 0 1 gutter-string)))
    (set-extent-begin-glyph gutter-buffers-tab-extent
			    (setq gutter-buffers-tab 
				  (make-glyph)))
    ;; Nuke all existing tabs
    (remove-gutter-element top-gutter 'buffers-tab)
    (remove-gutter-element bottom-gutter 'buffers-tab)
    (remove-gutter-element left-gutter 'buffers-tab)
    (remove-gutter-element right-gutter 'buffers-tab)
    ;; Put tabs into all devices that will be able to display them
    (dolist (x (console-type-list))
      (when (valid-image-instantiator-format-p 'tab-control x)
	(case gutter-buffers-tab-orientation
	  (top
	   ;; This looks better than a 3d border
	   (set-specifier top-gutter-border-width 0 'global x)
	   (set-gutter-element top-gutter 'buffers-tab 
			       gutter-string 'global x))
	  (bottom
	   (set-specifier bottom-gutter-border-width 0 'global x)
	   (set-gutter-element bottom-gutter 'buffers-tab
			       gutter-string 'global x))
	  (left
	   (set-specifier left-gutter-border-width 0 'global x)
	   (set-gutter-element left-gutter 'buffers-tab
			       gutter-string 'global x))
	  (right
	   (set-specifier right-gutter-border-width 0 'global x)
	   (set-gutter-element right-gutter 'buffers-tab gutter-string 'global
			       x)))))))

(defun update-tab-in-gutter (frame &optional force-selection)
  "Update the tab control in the gutter area.
Optional FORCE-SELECTION makes the currently selected window first in list."    ;; dedicated frames don't get tabs
  (unless (or (window-dedicated-p (frame-selected-window frame))
	      (frame-property frame 'popup))
    (when (specifier-instance default-gutter-visible-p frame)
      (unless (and gutter-buffers-tab
		   (eq (default-gutter-position)
		       gutter-buffers-tab-orientation))
	(add-tab-to-gutter))
      (when (valid-image-instantiator-format-p 'tab-control frame)
	(let ((items (buffers-tab-items nil frame force-selection)))
	  (when items
	    (set-glyph-image
	     gutter-buffers-tab
	     (vector 'tab-control :descriptor "Buffers" :face buffers-tab-face
		     :orientation gutter-buffers-tab-orientation
		     (if (or (eq gutter-buffers-tab-orientation 'top)
			     (eq gutter-buffers-tab-orientation 'bottom))
			 :pixel-width :pixel-height)
		     (if (or (eq gutter-buffers-tab-orientation 'top)
			     (eq gutter-buffers-tab-orientation 'bottom))
			 '(gutter-pixel-width) '(gutter-pixel-height)) 
		     :items items)
	     frame)
	    ;; set-glyph-image will not make the gutter dirty
	    (set-gutter-dirty-p gutter-buffers-tab-orientation)))))))

;; A myriad of different update hooks all doing slightly different things
(add-one-shot-hook 
 'after-init-hook
 #'(lambda ()
     ;; don't add the hooks if the user really doesn't want them
     (when gutter-buffers-tab-enabled
       (add-hook 'create-frame-hook 
		 #'(lambda (frame)
		     (when gutter-buffers-tab (update-tab-in-gutter frame t))))
       (add-hook 'buffer-list-changed-hook 'update-tab-in-gutter)
       (add-hook 'default-gutter-position-changed-hook
		 #'(lambda ()
		     (when gutter-buffers-tab
		       (mapc #'update-tab-in-gutter (frame-list)))))
       (add-hook 'gutter-element-visibility-changed-hook
		 #'(lambda (prop visible-p)
		     (when (and (eq prop 'buffers-tab) visible-p)
		       (mapc #'update-tab-in-gutter (frame-list)))))
       (update-tab-in-gutter (selected-frame) t))))

;; Commands to move between those buffers grouped together in the
;; gutter. Previously in files.el, but moved here since
;; buffers-tab-omit-function and buffers-tab-filter-functions are not
;; available if that file is not dumped.

;; Comments on their implementation:
;;
;; -- Repeatedly consing-up the buffer list is wasteful, but this function
;; won't be called that often (I'd be surprised if it's called at all for
;; most installations, it's a Ben Wing addition from the tail end of the
;; 90s, not in GNU Emacs and not really advertised.).
;; 
;; -- You could argue that that buffer-list-filtering code could be factored
;; out from #'buffers-tab-items, and we could just operate on that. This would
;; be a little easier to maintain.

(defun switch-to-next-buffer-in-group (&optional n)
  "Switch to the next-most-recent buffer in the current tab group.
This essentially rotates the buffer list forward.
N (interactively, the prefix arg) specifies how many times to rotate
forward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  (interactive "p")
  (dotimes (n (or n 1))
    (let ((curbuf (car (buffer-list))))
      (loop
	do (bury-buffer (car (buffer-list)))
	while (or (funcall buffers-tab-omit-function (car (buffer-list)))
                  (notevery #'(lambda (function)
                                (funcall function curbuf (car (buffer-list))))
                            buffers-tab-filter-functions)))))
  (switch-to-buffer (car (buffer-list))))

(defun switch-to-previous-buffer-in-group (&optional n)
  "Switch to the previously most-recent buffer in the current tab group.
This essentially rotates the buffer list backward.
N (interactively, the prefix arg) specifies how many times to rotate
backward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  (interactive "p")
  (dotimes (n (or n 1))
    (let ((curbuf (car (buffer-list))))
      (loop
	do (switch-to-buffer (car (last (buffer-list))))
	while (or (funcall buffers-tab-omit-function (car (buffer-list)))
                  (notevery #'(lambda (function)
                                (funcall function curbuf (car (buffer-list))))
                            buffers-tab-filter-functions))))))

;;
;; progress display
;; ripped off from message display
;;
(defcustom progress-feedback-use-echo-area nil
  "*Whether progress gauge display should display in the echo area.
If NIL then progress gauges will be displayed with whatever native widgets
are available on the current console. If non-NIL then progress display will be
textual and displayed in the echo area."
  :type 'boolean
  :group 'gutter)

(defvar progress-glyph-height 24
  "Height of the progress gauge glyph.")

(defvar progress-feedback-popup-period 0.5
  "The time that the progress gauge should remain up after completion")

(defcustom progress-feedback-style 'large
  "*Control the appearance of the progress gauge.
If 'large, the default, then the progress-feedback text is displayed
above the gauge itself. If 'small then the gauge and text are arranged
side-by-side."
  :group 'gutter
  :type '(choice (const :tag "large" large)
		 (const :tag "small" small)))

;; private variables
(defvar progress-text-instantiator [string :data ""])
(defvar progress-layout-glyph (make-glyph))
(defvar progress-layout-instantiator nil)

(defvar progress-gauge-instantiator
  [progress-gauge
   :value 0
   :pixel-height (eval progress-glyph-height)
   :pixel-width 250
   :descriptor "Progress"])

(defun set-progress-feedback-instantiator (&optional locale)
  (cond
   ((eq progress-feedback-style 'small)
    (setq progress-glyph-height 16)
    (setq progress-layout-instantiator
	  `[layout
	    :orientation horizontal
	    :margin-width 4
	    :items (,progress-gauge-instantiator
		    [button
		     :pixel-height (eval progress-glyph-height)
		     ;; 'quit is special and acts "asynchronously".
		     :descriptor "Stop" :callback 'quit]
		    ,progress-text-instantiator)])
    (set-glyph-image progress-layout-glyph progress-layout-instantiator
		     locale))
   (t 
    (setq progress-glyph-height 24)
    (setq progress-layout-instantiator
	  `[layout 
	    :orientation vertical :margin-width 4
	    :horizontally-justify left :vertically-justify center
	    :items (,progress-text-instantiator
		    [layout 
		     :orientation horizontal
		     :items (,progress-gauge-instantiator
			     [button 
			      :pixel-height (eval progress-glyph-height)
			      :descriptor " Stop "
			      ;; 'quit is special and acts "asynchronously".
			      :callback 'quit])])])
    (set-glyph-image progress-layout-glyph progress-layout-instantiator
		     locale))))

(defvar progress-abort-glyph (make-glyph))

(defun set-progress-abort-instantiator (&optional locale)
  (set-glyph-image progress-abort-glyph
		   `[layout :orientation vertical
			    :horizontally-justify left :vertically-justify center
			    :items (,progress-text-instantiator
				    [layout
				     :margin-width 4
				     :pixel-height progress-glyph-height
				     :orientation horizontal])]
		   locale))

(defvar progress-stack nil
  "An alist of label/string pairs representing active progress gauges.
The first element in the list is currently displayed in the gutter area.
Do not modify this directly--use the `progress-feedback' or
`display-progress-feedback'/`clear-progress-feedback' functions.")

(defun progress-feedback-displayed-p (&optional return-string frame)
  "Return a non-nil value if a progress gauge is presently displayed in the
gutter area.  If optional argument RETURN-STRING is non-nil,
return a string containing the message, otherwise just return t."
  (let ((buffer (get-buffer-create " *Gutter Area*")))
    (and (< (point-min buffer) (point-max buffer))
	 (if return-string
	     (buffer-substring nil nil buffer)
	   t))))

;;; Returns the string which remains in the echo area, or nil if none.
;;; If label is nil, the whole message stack is cleared.
(defun clear-progress-feedback (&optional label frame no-restore)
  "Remove any progress gauge with LABEL from the progress gauge-stack,
erasing it from the gutter area if it's currently displayed there.
If a message remains at the head of the progress-stack and NO-RESTORE
is nil, it will be displayed.  The string which remains in the gutter
area will be returned, or nil if the progress-stack is now empty.
If LABEL is nil, the entire progress-stack is cleared.

Unless you need the return value or you need to specify a label,
you should just use (progress nil)."
  (if (or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	  progress-feedback-use-echo-area)
      (clear-message label frame nil no-restore)
    (or frame (setq frame (selected-frame)))
    (remove-progress-feedback label frame)
    (let ((inhibit-read-only t))
      (erase-buffer (get-buffer-create " *Gutter Area*")))
    (if no-restore
	nil			; just preparing to put another msg up
      (if progress-stack
	  (let ((oldmsg (cdr (car progress-stack))))
	    (raw-append-progress-feedback oldmsg nil frame)
	    oldmsg)
	;; nothing to display so get rid of the gauge
	(set-specifier bottom-gutter-border-width 0 frame)
	(set-gutter-element-visible-p bottom-gutter-visible-p 
				      'progress nil frame)))))

(defun progress-feedback-clear-when-idle (&optional label)
  (add-one-shot-hook 'pre-idle-hook
		     `(lambda ()
			(clear-progress-feedback ',label))))

(defun remove-progress-feedback (&optional label frame)
  ;; If label is nil, we want to remove all matching progress gauges.
  (while (and progress-stack
	      (or (null label)	; null label means clear whole stack
		  (eq label (car (car progress-stack)))))
    (setq progress-stack (cdr progress-stack)))
  (let ((s  progress-stack))
    (while (cdr s)
      (let ((msg (car (cdr s))))
	(if (eq label (car msg))
	    (progn
	      (setcdr s (cdr (cdr s))))
	  (setq s (cdr s)))))))

(defun progress-feedback-dispatch-non-command-events ()
  ;; don't allow errors to hose things
  (condition-case nil 
      ;; (sit-for 0) causes more redisplay than we want.
      (dispatch-non-command-events)
    (t nil)))

(defun append-progress-feedback (label message &optional value frame)
  (or frame (setq frame (selected-frame)))
  ;; Add a new entry to the progress-stack, or modify an existing one
  (let* ((top (car progress-stack))
	 (tmsg (cdr top)))
    (if (eq label (car top))
	(progn
	  (setcdr top message)
	  (if (equal tmsg message)
	      (progn 
		(set-instantiator-property progress-gauge-instantiator :value value)
		(set-progress-feedback-instantiator (frame-selected-window frame)))
	    (raw-append-progress-feedback message value frame))
	  (redisplay-gutter-area))
      (push (cons label message) progress-stack)
      (raw-append-progress-feedback message value frame))
    (progress-feedback-dispatch-non-command-events)
    ;; either get command events or sit waiting for them
    (when (eq value 100)
;      (sit-for progress-feedback-popup-period nil)
      (clear-progress-feedback label))))

(defun abort-progress-feedback (label message &optional frame)
  (if (or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	  progress-feedback-use-echo-area)
      (display-message label (concat message "aborted.") frame)
    (or frame (setq frame (selected-frame)))
    ;; Add a new entry to the progress-stack, or modify an existing one
    (let* ((top (car progress-stack))
	   (inhibit-read-only t))
      (if (eq label (car top))
	  (setcdr top message)
	(push (cons label message) progress-stack))
      (unless (equal message "")
	(insert-string message (get-buffer-create " *Gutter Area*"))
	(let* ((gutter-string (copy-sequence "\n"))
	       (ext (make-extent 0 1 gutter-string)))
	  ;; do some funky display here.
	  (set-extent-begin-glyph ext progress-abort-glyph)
	  ;; fixup the gutter specifiers
	  (set-gutter-element bottom-gutter 'progress gutter-string frame)
	  (set-specifier bottom-gutter-border-width 2 frame)
	  (set-instantiator-property progress-text-instantiator :data message)
	  (set-progress-abort-instantiator (frame-selected-window frame))
	  (set-specifier bottom-gutter-height 'autodetect frame)
	  (set-gutter-element-visible-p bottom-gutter-visible-p 
					'progress t frame)
	  ;; we have to do this so redisplay is up-to-date and so
	  ;; redisplay-gutter-area performs optimally.
	  (redisplay-gutter-area)
	  (sit-for progress-feedback-popup-period nil)
	  (clear-progress-feedback label frame)
	  (set-extent-begin-glyph ext progress-layout-glyph)
	  (set-gutter-element bottom-gutter 'progress gutter-string frame)
	  )))))

(defun raw-append-progress-feedback (message &optional value frame)
  (unless (equal message "")
    (let* ((inhibit-read-only t)
	  (val (or value 0))
	  (gutter-string (copy-sequence "\n"))
	  (ext (make-extent 0 1 gutter-string)))
      (insert-string message (get-buffer-create " *Gutter Area*"))
      ;; do some funky display here.
      (set-extent-begin-glyph ext progress-layout-glyph)
      ;; fixup the gutter specifiers
      (set-gutter-element bottom-gutter 'progress gutter-string frame)
      (set-specifier bottom-gutter-border-width 2 frame)
      (set-instantiator-property progress-gauge-instantiator :value val)
      (set-progress-feedback-instantiator (frame-selected-window frame))

      (set-instantiator-property progress-text-instantiator :data message)
      (set-progress-feedback-instantiator (frame-selected-window frame))
      (if (and (eq (specifier-instance bottom-gutter-height frame)
		   'autodetect)
	       (gutter-element-visible-p bottom-gutter-visible-p
					 'progress frame))
	  ;; if the gauge is already visible then just draw the gutter
	  ;; checking for user events
	  (progn
	    (redisplay-gutter-area)
	    (progress-feedback-dispatch-non-command-events))
	;; otherwise make the gutter visible and redraw the frame
	(set-specifier bottom-gutter-height 'autodetect frame)
	(set-gutter-element-visible-p bottom-gutter-visible-p
				      'progress t frame)
	;; we have to do this so redisplay is up-to-date and so
	;; redisplay-gutter-area performs optimally. This may also
	;; make sure the frame geometry looks ok.
	(progress-feedback-dispatch-non-command-events)
	(redisplay-frame frame)
	))))

(defun display-progress-feedback (label message &optional value frame)
  "Display a progress gauge and message in the bottom gutter area.
 First argument LABEL is an identifier for this message.  MESSAGE is
the string to display.  Use `clear-progress-feedback' to remove a labelled
message."
  (cond ((eq value 'abort)
	 (abort-progress-feedback label message frame))
	((or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	     progress-feedback-use-echo-area)
	 (display-message label 
	   (concat message (if (eq value 100) "done."
			     (make-string (/ value 5) ?.)))
	   frame))
	(t
	 (append-progress-feedback label message value frame))))

(defun current-progress-feedback (&optional frame)
  "Return the current progress gauge in the gutter area, or nil.
The FRAME argument is currently unused."
  (cdr (car progress-stack)))

;;; may eventually be frame-dependent
(defun current-progress-feedback-label (&optional frame)
  (car (car progress-stack)))

(defun progress-feedback (fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing progress gauge."
  (save-excursion
    (if (and (null fmt) (null args))
	(prog1 nil
	  (clear-progress-feedback nil))
      (let ((str (apply 'format fmt args)))
	(display-progress-feedback 'progress str value)
	str))))

(defun progress-feedback-with-label (label fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
LABEL is an identifier for this progress gauge.
FMT is a format string to be passed to `format' along with ARGS.
Optional VALUE is the current degree of progress, an integer 0-100.
The remaining ARGS are passed with FMT `(apply #'format FMT ARGS)'."
  ;; #### sometimes the buffer gets changed temporarily. I don't know
  ;; why this is, so protect against it.
  (save-excursion
    (if (and (null fmt) (null args))
	(prog1 nil
	  (clear-progress-feedback label nil))
      (let ((str (apply 'format fmt args)))
	(display-progress-feedback label str value)
	str))))

(provide 'gutter-items)

;;; gutter-items.el ends here.
