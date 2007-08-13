;;; time.el --- display time and load in mode line of Emacs.

;; Copyright (C) 1985, 86, 87, 93, 94, 1996 Free Software Foundation, Inc.

;; Maintainer: FSF for the original version. 
;;             XEmacs add-ons and rewrite (C) by Jens Lautenbacher
;;                            mail <jens@lemming0.lem.uni-karlsruhe.de>
;;                            for comments/fixes about the enhancements.

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Version: 1.17  (I choose the version number starting at 1.1
;;;                to indicate that 1.0 was the old version
;;;                before I hacked away on it -jtl)

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; Facilities to display current time/date and a new-mail indicator
;; in the Emacs mode line.  The single entry point is `display-time'.

;; See also reportmail.el.
;; This uses the XEmacs timeout-event mechanism, via a version
;; of Kyle Jones' itimer package.

;;; jtl: This is in a wide part reworked for XEmacs so it won't use
;;;      the old mechanism for specifying what is to be displayed.
;;;      The starting variable to look at is `display-time-form-list'

;;; It's more advanced features include heavy use of `balloon-help' a
;;; package again written by Kyle Jones. You need to load this
;;; explicitely on your own because I don't think a package should make
;;; decisions which have a global effect (if you want to use it, a
;;; (require 'balloon-help) in your .emacs should work. But look at the
;;; documentation in balloon-help.el itself).

;;; Thanks to Mike Scheidler for the idea to make the time led's fore- and
;;; background color customizable

;;; Code:

(require 'itimer)
;;; Not sure for now...
;;;(require 'balloon-help)

(defconst display-time-version-number "1.15" "Version number of time.el")
(defconst display-time-version (format "Time.el version %s for XEmacs"
				       display-time-version-number)
  "The full version string for time.el")

;;; Doesn't work by now....
;;;(defvar display-time-keymap nil)
;;;
;;;(if display-time-keymap ()
;;;  (setq display-time-keymap (make-sparse-keymap)) 
;;;  (suppress-keymap display-time-keymap)
;;;  (define-key display-time-keymap 'button1 'balloon-help))

(defgroup display-time nil
  "Facilities to display the current time/date/load and a new-mail indicator
in the XEmacs mode line or echo area."
  :group 'applications)

(defgroup display-time-balloon nil
  "Fancy add-ons to display-time for using the `balloon-help' feature.
balloon-help must be loaded before these settings take effect."
  :group 'display-time)


(defcustom display-time-mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
Non-nil and not a string means don't check for mail.  nil means use
default, which is system-dependent, and is the same as used by Rmail."
  :group 'display-time)

;;;###autoload
(defcustom display-time-day-and-date nil
  "*Non-nil means \\[display-time] should display day,date and time.
This affects the spec 'date in the variable display-time-form-list."
  :group 'display-time
  :type 'boolean)

(defcustom display-time-interval 20
  "*Seconds between updates of time in the mode line."
  :group 'display-time
  :type 'integer)

(defcustom display-time-24hr-format nil
  "*Non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23.
Nil means 1 <= hh <= 12, and an AM/PM suffix is used.
This affects the spec 'time in the variable display-time-form-list."
  :group 'display-time
  :type 'boolean)

(defcustom display-time-echo-area nil
  "*If non-nil, display-time will use the echo area instead of the mode line."
  :group 'display-time
  :type 'boolean)

(defvar display-time-string nil)

(defcustom display-time-hook nil
  "*List of functions to be called when the time is updated on the mode line."
  :group 'display-time
  :type 'hook)

(defvar display-time-server-down-time nil
   "Time when mail file's file system was recorded to be down.
If that file system seems to be up, the value is nil.")

(defcustom display-time-ignore-read-mail t
  "*Non-nil means display the mail icon on any non-empty mailbox."
  :group 'display-time
  :type 'boolean)

;;;###autoload
(defun display-time ()
  "Display current time, load level, and mail flag in mode line of each buffer.
Updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
After each update, `display-time-hook' is run with `run-hooks'.
If `display-time-echo-area' is non-nil, the time is displayed in the
echo area instead of in the mode-line."
  (interactive)
  (or display-time-insinuated
      (display-time-insinuate))
  ;; if the "display-time" itimer already exists, nuke it first.
  (let ((old (get-itimer "display-time")))
    (if old (delete-itimer old)))

  (if (memq 'display-time-string global-mode-string)
      (setq global-mode-string
	    (remove 'display-time-string global-mode-string)))
  ;; If we're not displaying the time in the echo area
  ;; and the global mode string does not have a non-nil value
  ;; then initialize the global mode string's value.
  (or display-time-echo-area
      global-mode-string
      (setq global-mode-string '("")))
  ;; If we're not displaying the time in the echo area
  ;; then we add our variable to the list.  This will make the time
  ;; appear on the modeline.
  (or display-time-echo-area
      (setq global-mode-string
		(append global-mode-string '(display-time-string))))
  ;; Display the time initially...
  (display-time-function)
  ;; ... and start an itimer to do it automatically thereafter.
  ;;
  ;; If we wanted to be really clever about this, we could have the itimer
  ;; not be automatically restarted, but have it re-add itself each time.
  ;; Then we could look at (current-time) and arrange for the itimer to
  ;; wake up exactly at the minute boundary.  But that's just a little
  ;; more work than it's worth...
  (start-itimer "display-time" 'display-time-function
		display-time-interval display-time-interval))

(defun display-time-stop ()
  (interactive)
  (delete-itimer "display-time")
  (setq display-time-string nil))

(defcustom display-time-show-icons-maybe t
  "Use icons for time, load and mail status if possible
and not specified different explicitely"
  :group 'display-time
  :type 'boolean)  

(defvar display-time-icons-dir (locate-data-directory "time"))

(defcustom display-time-mail-sign-string " Mail" 
  "The string used as mail indicator in the echo area 
(and in the modeline if display-time-show-icons-maybe is nil)
if display-time-echo-area is t"
:group 'display-time
:type 'string)

(defcustom display-time-no-mail-sign-string ""
  "The string used as no-mail indicator in the echo area
(and in the modeline if display-time-show-icons-maybe is nil)
if display-time-echo-area is t"
:group 'display-time
:type 'string)

(defcustom display-time-display-pad  "grey35"
  "How the load indicator's trapezoidal \"pad\" is to be displayed.
This can be 'transparent or a string describing the color it should have"
  :group 'display-time
  :type '(choice :tag "Value"
		 (const transparent)
		 (string :tag "Color")))

(defcustom display-time-display-time-foreground  "firebrick"
  "How the time LEDs foreground is to be displayed.
This can be 'modeline (foreground color of the Modeline)
or a string describing the color it should have"
  :group 'display-time
  :type '(choice :tag "Value"
		 (const modline)
		 (string :tag "Color")))

(defcustom display-time-display-time-background  'transparent
  "How the time LEDs background is to be displayed.
This can be 'transparent or a string describing the color it should have"
  :group 'display-time
  :type '(choice :tag "Value"
		 (const transparent)
		 (string :tag "Color")))

(defcustom display-time-mail-balloon 'display-time-mail-balloon
  "What to use to generate the balloon frame of the \"mail\" glyph
if balloon-help is loaded. This can be the function
display-time-mail-balloon, nil or a string."
  :group 'display-time-balloon 
  :type '(choice (const display-time-mail-balloon)
		 (const nil)
		 (string)))

(defcustom display-time-no-mail-balloon "No mail is good mail."
  "The string used in the balloon frame of the \"no mail\" glyph
if balloon-help is loaded. This can also be nil"
  :group 'display-time-balloon
  :type '(choice (const nil)
		 (string)))

(defcustom display-time-mail-balloon-show-gnus-group nil
  "Show the mail group gnus would put this message in.
This is only useful if you use gnus to read your mail and have set the variable
nnmail-split-methods to split your incoming mail into different groups.
Look at the documentation for gnus. If you don't know what we're talking about,
don't care and leave this set to nil"
  :group 'display-time-balloon
  :type 'boolean)

(defface display-time-mail-balloon-enhance-face '((t (:background  "orange")))
  "Face used for entries in the mail balloon which match the regexp
display-time-mail-balloon-enhance"
  :group 'display-time-balloon)

(defface display-time-time-balloon-face '((t (:foreground  "red")))
  "Face used in the time balloon to display the full date and load.
It is also used in the mail balloon for the \"You have mail:\" heading."
  :group 'display-time-balloon) 

(defface display-time-mail-balloon-gnus-group-face '((t (:foreground "blue")))
  "Face used for the gnus group entry in the mail balloon
if display-time-mail-balloon-show-gnus-group is t (see the documentation there
before you set it to t)"
  :group 'display-time-balloon)

(defcustom display-time-mail-balloon-max-displayed 10
  "The maximum number of messaged which are displayed in the mail balloon.
You need to have balloon-help loaded to use this."
  :group 'display-time-balloon
  :type 'number)

(defcustom display-time-mail-balloon-from-width 20
  "The width of the `From:' part of the mail balloon.
You need to have balloon-help loaded to use this"
  :group 'display-time-balloon
  :type 'number)

(defcustom display-time-mail-balloon-subject-width 25
  "The width of the `Subject:' part of the mail balloon.
You need to have balloon-help loaded to use this"
  :group 'display-time-balloon
  :type 'number)

(defcustom display-time-mail-balloon-gnus-split-width 10
  "The width of the `Gnus Mail Group' part of the mail balloon.
This denotes the mail group gnus would decide to put this message in.
For getting this information, it consults the relevant variables from gnus
(nnmail-split-methods).
You need to have balloon-help loaded to use this"
  :group 'display-time-balloon
  :type 'number)

(defcustom display-time-mail-balloon-enhance nil
  "A list of regular expressions describing which messages should be highlighted
in the mail balloon. The regexp will be matched against the complete header block
of an email. You need to load balloon-help to use this"
  :group 'display-time-balloon
  :type '(repeat (string :tag "Regexp")))

(defcustom display-time-mail-balloon-suppress nil
  "A list of regular expressions describing which messages should be completely suppressed
in the mail balloon. The regexp will be matched against the complete header block
of an email. It will only take effect if the message is not matched already
by display-time-mail-balloon-enhance.
You need to load balloon-help to use this"
  :group 'display-time-balloon
  :type '(repeat (string :tag "Regexp")))

(defcustom display-time-mail-balloon-enhance-gnus-group nil
  "A list of regular expressions describing which messages should be highlighted
in the mail balloon. The regexp will be matched against the group gnus would stuff
this message into. It will only take effect if the message is not matched already
by display-time-mail-balloon-suppress.

This requires display-time-mail-balloon-show-gnus-group to be t
and balloon-help to be loaded"
  :group 'display-time-balloon
  :type '(repeat (string :tag "Regexp")))

(defcustom display-time-mail-balloon-suppress-gnus-group nil
  "A list of regular expressions describing which messages should be completely suppressed
in the mail balloon. The regexp will be matched against the group gnus would stuff
this message into. It will only take effect if the message is not matched already
by display-time-mail-balloon-enhance or display-time-mail-balloon-enhance-gnus-group.

This requires display-time-mail-balloon-show-gnus-group to be t
and balloon-help to be loaded"
  :group 'display-time-balloon
  :type '(repeat (string :tag "Regexp")))

(defvar display-time-spool-file-modification nil)

(defvar display-time-mail-header nil)

(defvar display-time-temp-buffer " *Display-time-temp-buffer*")

(defvar display-time-display-pad-old nil)

(defvar display-time-display-time-fg-old nil)

(defvar display-time-display-time-bg-old nil)

(defcustom display-time-load-list
  (list 0.2 0.5 0.8 1.1 1.8 2.6)
  "*A list giving six thresholds for the load
which correspond to the six different icons to be displayed
as a load indicator"
  :group 'display-time
  :type '(list (number :tag "Threshold 1")
	       (number :tag "Threshold 2")
	       (number :tag "Threshold 3")
	       (number :tag "Threshold 4")
	       (number :tag "Threshold 5")
	       (number :tag "Threshold 6")))

(defcustom display-time-compatible nil 
  "*This variable may be set to t to get the old behaviour of display-time.
It should be considered obsolete and only be used if you really want the
old behaviour (eq. you made extensive customizations yourself).
This means no display of a spiffy mail icon or use of the
display-time-form-list instead of the old display-time-string-form."
  :group 'display-time
  :type 'boolean)

(defun display-time-string-to-char-list (str)
  (mapcar (function identity) str))

(defun display-time-generate-load-glyphs (&optional force)
  (let* ((pad-color (if (symbolp display-time-display-pad)
			(list "pad-color" '(face-background 'modeline))
		      (list "pad-color" display-time-display-pad)))
	 (xpm-color-symbols (append (list pad-color) xpm-color-symbols)))
    (if (and (featurep 'xpm)
	     (or force (not (equal display-time-display-pad
				   display-time-display-pad-old))))
	(progn
	  (setq display-time-load-0.0-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-0.0.xpm"))))
	  (setq display-time-load-0.5-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-0.5.xpm"))))
	  (setq display-time-load-1.0-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-1.0.xpm"))))
	  (setq display-time-load-1.5-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-1.5.xpm"))))
	  (setq display-time-load-2.0-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-2.0.xpm"))))
	  (setq display-time-load-2.5-glyph
		(cons (make-extent nil nil)
		      (make-glyph
		       (concat display-time-icons-dir "l-2.5.xpm"))))
	  (setq display-time-load-3.0-glyph
	  (cons (make-extent nil nil)
		(make-glyph
		 (concat display-time-icons-dir "l-3.0.xpm"))))
	  (setq display-time-display-pad-old display-time-display-pad)
	  ))))


(defun display-time-generate-time-glyphs (&optional force)
  (let* ((ledbg (if (symbolp display-time-display-time-background)
		    (list "ledbg" '(face-background 'modeline))
		  (list "ledbg" display-time-display-time-background)))
	 (ledfg (if (symbolp display-time-display-time-foreground)
		    (list "ledfg" '(face-foreground 'modeline))
		  (list "ledfg" display-time-display-time-foreground)))
	 (xpm-color-symbols (append (list ledbg)
				    (list ledfg) xpm-color-symbols)))
    (if (and (featurep 'xpm)
	     (or force (not (equal display-time-display-time-background
				   display-time-display-time-bg-old))
		 (not (equal display-time-display-time-foreground
			     display-time-display-time-fg-old))))
	(progn
	  (setq display-time-1-glyph 
		(cons (make-extent nil nil) 
		      (make-glyph (concat display-time-icons-dir "1.xpm"))))
	  (setq display-time-2-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "2.xpm"))))
	  (setq display-time-3-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "3.xpm"))))
	  (setq display-time-4-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "4.xpm"))))
	  (setq display-time-5-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "5.xpm"))))
	  (setq display-time-6-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "6.xpm"))))
	  (setq display-time-7-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "7.xpm"))))
	  (setq display-time-8-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "8.xpm"))))
	  (setq display-time-9-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "9.xpm"))))
	  (setq display-time-0-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "0.xpm"))))
	  (setq display-time-:-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "dp.xpm"))))
	  (setq display-time-am-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "am.xpm"))))
	  (setq display-time-pm-glyph
		(cons (make-extent nil nil)
		      (make-glyph (concat display-time-icons-dir "pm.xpm"))))
	  (setq display-time-display-time-fg-old
		display-time-display-time-foreground
		display-time-display-time-bg-old
		display-time-display-time-background)
	  ))))

(defun display-time-init-glyphs ()
  "This is a hack to have all glyphs be displayed one time at startup.
It helps avoiding problems with the background color of the glyphs if a
balloon-help frame is open and a not yet displayed glyph is going to be
displayed."
  (let ((i 0)
	(list '("am" "pm" ":"))
	elem mlist)
    (while (< i 10)
      (push (eval (intern-soft (concat "display-time-"
				       (number-to-string i)
				       "-glyph"))) mlist)
      (setq i (1+ i)))
    (setq i 0.0)
    (while (<= i 3.0)
      (push (eval (intern-soft (concat "display-time-load-"
				       (number-to-string i)
				       "-glyph"))) mlist)
      (setq i (+ i 0.5)))
    (while (setq elem (pop list))
      (push (eval (intern-soft (concat "display-time-"
				       elem "-glyph"))) mlist))
    (let ((global-mode-string mlist))
      (redisplay-frame))
    ))

(defvar display-time-insinuated nil)

;; This used to be at top-level!
(defun display-time-insinuate ()
  (when (featurep 'xpm)
    (defvar display-time-mail-sign
      (cons (make-extent nil nil)
	    (make-glyph  (concat display-time-icons-dir "letter.xpm"))))
    (set-extent-property (car display-time-mail-sign) 'balloon-help
			 'display-time-mail-balloon)
;;;	 (set-extent-keymap (car display-time-mail-sign)
;;;			    display-time-keymap)
    (defvar display-time-no-mail-sign
      (cons (make-extent nil nil)
	    (make-glyph  (concat display-time-icons-dir "no-letter.xpm"))))
    (set-extent-property (car display-time-no-mail-sign) 'balloon-help
			 display-time-no-mail-balloon)
;;;	 (set-extent-keymap (car display-time-no-mail-sign)
;;;			    display-time-keymap)
    (defvar display-time-1-glyph  nil)
    (defvar display-time-2-glyph  nil)
    (defvar display-time-3-glyph  nil)
    (defvar display-time-4-glyph  nil)
    (defvar display-time-5-glyph  nil)
    (defvar display-time-6-glyph  nil)
    (defvar display-time-7-glyph  nil)
    (defvar display-time-8-glyph  nil)
    (defvar display-time-9-glyph  nil)
    (defvar display-time-0-glyph  nil)
    (defvar display-time-:-glyph  nil)
    (defvar display-time-am-glyph nil)
    (defvar display-time-pm-glyph nil)
    (defvar display-time-load-0.0-glyph nil)
    (defvar display-time-load-0.5-glyph nil)
    (defvar display-time-load-1.0-glyph nil)
    (defvar display-time-load-1.5-glyph nil)
    (defvar display-time-load-2.0-glyph nil)
    (defvar display-time-load-2.5-glyph nil)
    (defvar display-time-load-3.0-glyph nil)
    (display-time-generate-time-glyphs 'force)
    (display-time-generate-load-glyphs 'force)  
    (display-time-init-glyphs)
    (sit-for 0))
  (setq display-time-insinuated t))


(defun display-time-can-do-graphical-display (&optional textual)
  (and display-time-show-icons-maybe
       (not textual)
       (eq (console-type) 'x)
       (featurep 'xpm)
       (not display-time-echo-area)))
       
       
(defun display-time-convert-num (time-string &optional textual)
  (let ((list (display-time-string-to-char-list time-string))
	elem tmp balloon-help balloon-ext)
    (if (not (display-time-can-do-graphical-display textual)) time-string 
      (display-time-generate-time-glyphs)
      (setq balloon-help
	    (format "%s, %s %s %s %s" dayname day monthname year
		    (concat "   Average load:"
			    (if (not (equal load ""))
				load
			      " 0"))))
      (setq balloon-ext (make-extent 0 (length balloon-help) balloon-help))
      (set-extent-property balloon-ext 'face 'display-time-time-balloon-face)
      (set-extent-property balloon-ext 'duplicable 't)
      (while (setq elem (pop list))
	(setq elem
	      (eval (intern-soft (concat "display-time-"
					 (char-to-string elem)
					 "-glyph"))))
	(set-extent-property (car elem) 'balloon-help balloon-help)
;;;	(set-extent-keymap (car elem) display-time-keymap)
	(push elem tmp))
      (reverse tmp))))

(defun display-time-convert-load (load-string &optional textual)
  (let ((load-number (string-to-number load-string))
	(alist (list (cons 0.0 0.0)
		    (cons 0.5 (car display-time-load-list))
		    (cons 1.0 (cadr display-time-load-list))
		    (cons 1.5 (caddr display-time-load-list))
		    (cons 2.0 (cadddr display-time-load-list))
		    (cons 2.5 (cadr (cdddr display-time-load-list)))
		    (cons 3.0 (caddr (cdddr display-time-load-list)))
		    (cons 100000 100000)))
	elem load-elem)
    (if (not (display-time-can-do-graphical-display textual))
	load-string
      (display-time-generate-load-glyphs)
      (while (>= load-number (cdr (setq elem (pop alist))))
	(setq load-elem elem))
      (eval (intern-soft (concat "display-time-load-"
				 (number-to-string (car load-elem))
				 "-glyph"))))))

(defun display-time-convert-am-pm (ampm-string &optional textual)
  (if (not (display-time-can-do-graphical-display textual))
      ampm-string
    (cond ((equal ampm-string "am") display-time-am-glyph)
	  ((equal ampm-string "pm") display-time-pm-glyph))))

(defun display-time-mail-balloon (&rest ciao)
  (let* ((mail-spool-file (or display-time-mail-file
			      (getenv "MAIL")
			      (concat rmail-spool-directory
				      (user-login-name))))
	 (show-split (and display-time-mail-balloon-show-gnus-group
			  (or (featurep 'nnmail) (require 'nnmail))))
	 (display-time-mail-balloon-gnus-split-width
	  (if (not show-split) 0
	    (+ 3 display-time-mail-balloon-gnus-split-width))) ; -><space>... = +3
	 (mod (nth 5 (file-attributes mail-spool-file)))
	 header	header-ext)
    (setq header "You have mail:")
    (setq header-ext
	  (make-extent 0 (length header) header))
    (set-extent-property header-ext 'face 'display-time-time-balloon-face)
    (set-extent-property header-ext 'duplicable t)
    (setq header (concat header "\n"
			 (make-string (+ display-time-mail-balloon-from-width
					 display-time-mail-balloon-subject-width
					 display-time-mail-balloon-gnus-split-width
					 3) (string-to-char "-"))))
    (if (not (equal
	      mod display-time-spool-file-modification))
	(progn
	  (setq display-time-spool-file-modification mod)
	  (setq display-time-mail-header
		(display-time-scan-mail-file mail-spool-file show-split))))
    (setq header (concat header display-time-mail-header))
    ))


(defun display-time-scan-mail-file (file show-split)
  (let ((mail-headers "")
	(nntp-server-buffer (get-buffer-create " *Display-Time-Split-Buffer*"))
	(suppress-count 0)
	(not-displayed 0)
	(i 0)
	(suppress-list display-time-mail-balloon-suppress)
	(enhance-list display-time-mail-balloon-enhance)
	(gnus-suppress-list display-time-mail-balloon-suppress-gnus-group)
	(gnus-enhance-list display-time-mail-balloon-enhance-gnus-group)
	mail-headers-list start end from subject gnus-group tmp
	suppress enhance line line-ext
	gnus-suppress-reg gnus-enhance-reg suppress-reg enhance-reg)
    
    (erase-buffer (get-buffer-create display-time-temp-buffer))
    (message "Scanning spool file...")
    (while (setq tmp (pop enhance-list))
      (setq enhance-reg
	    (if (car enhance-list) (concat enhance-reg tmp "\\|")
	      (concat enhance-reg tmp))))
    (while (setq tmp (pop suppress-list))
      (setq suppress-reg
	    (if (car suppress-list) (concat suppress-reg tmp "\\|")
	      (concat suppress-reg tmp))))
    (while (setq tmp (pop gnus-enhance-list))
      (setq gnus-enhance-reg
	    (if (car gnus-enhance-list) (concat gnus-enhance-reg tmp "\\|")
	      (concat gnus-enhance-reg tmp))))
    (while (setq tmp (pop gnus-suppress-list))
      (setq gnus-suppress-reg
	    (if (car gnus-suppress-list) (concat gnus-suppress-reg tmp "\\|")
	      (concat gnus-suppress-reg tmp))))
    (save-excursion
      (set-buffer display-time-temp-buffer)
      (setq case-fold-search nil)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (setq start (re-search-forward "^From " nil t))
	(save-excursion
	  (setq end (re-search-forward "^$" nil t))
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (setq enhance
		(save-excursion
		  (if display-time-mail-balloon-enhance
		      (re-search-forward enhance-reg nil t))))
	  (if show-split
	      (save-excursion
		(goto-char (point-min))
		(nnmail-article-group '(lambda (name) (setq gnus-group name)))))
	    
	  (if enhance () ; this takes prejudice over everything else
	    (setq suppress ; maybe set suppress only if not already enhanced
		  (save-excursion
		    (if display-time-mail-balloon-suppress
			(re-search-forward suppress-reg nil t))))
	    (if suppress ()
	      (or (setq enhance      ;;maybe we enhance because of the gnus group name
			(save-excursion
			  (if (and show-split gnus-group
				   display-time-mail-balloon-enhance-gnus-group)
			      (string-match gnus-enhance-reg gnus-group))))
		  (setq suppress  ;; if we didn't enhance then maybe we have to
			          ;; suppress it?
			(save-excursion
			  (if (and show-split gnus-group
				   display-time-mail-balloon-suppress-gnus-group)
			      (string-match gnus-suppress-reg gnus-group)))))))
	  
	  (setq from
		(save-excursion
		  (re-search-forward "^From: \\(.*\\)" nil t)
		  (mail-extract-address-components (match-string 1))))
	  (setq subject
		(save-excursion
		  (re-search-forward "^Subject: \\(.*\\)" nil t)
		  (match-string 1)))
	  (if suppress (setq suppress-count (1+ suppress-count))
	    (if (car from) (setq from (car from))
	      (setq from (cadr from)))
	    (if (> (length from) display-time-mail-balloon-from-width)
		(setq from (substring from 0
				      display-time-mail-balloon-from-width)))
	    (if (> (length subject) display-time-mail-balloon-subject-width)
		(setq subject (substring subject 0
					 display-time-mail-balloon-subject-width)))
	    (if (and show-split gnus-group
		     (> (length gnus-group)
			(- display-time-mail-balloon-gnus-split-width 3)))
		(setq gnus-group (substring gnus-group 0
					    (- display-time-mail-balloon-gnus-split-width 3))))
		
	    (setq line (format (concat
				"\n%-"(number-to-string
				       display-time-mail-balloon-from-width)
				"s [%-"(number-to-string
					display-time-mail-balloon-subject-width)
				"s]")
			       from subject))
	    (if (and show-split gnus-group)
		(setq line (concat line
				   (format
				    (concat
				     "-> %" (number-to-string
					     (- display-time-mail-balloon-gnus-split-width 3))
				     "s") gnus-group))))
	    (if enhance
		(progn
		  (setq line-ext (make-extent 1 (length line) line))
		  (set-extent-property line-ext 'face
				       'display-time-mail-balloon-enhance-face)
		  (set-extent-property line-ext 'duplicable t)
		  (set-extent-property line-ext 'end-open t)))
	    (if (and show-split gnus-group)
		(progn
		  (setq line-ext (make-extent (- (length line)
						 display-time-mail-balloon-gnus-split-width)
					      (length line) line))
		  (set-extent-property line-ext 'face
				       'display-time-mail-balloon-gnus-group-face)
		  (set-extent-property line-ext 'duplicable t)
		  (set-extent-property line-ext 'end-open t)))
	    (push line mail-headers-list))
	  (goto-char (point-max))
	  (setq suppress nil
		gnus-group nil
		enhance nil)
	  (widen)
	  )))
    (kill-buffer display-time-temp-buffer)
    (if (> (length mail-headers-list) display-time-mail-balloon-max-displayed)
	(setq not-displayed (- (length mail-headers-list)
			       display-time-mail-balloon-max-displayed)))
    (while (< i display-time-mail-balloon-max-displayed)
      (setq mail-headers (concat mail-headers (pop mail-headers-list))) 
      (setq i (1+ i)))
    (if (and (equal mail-headers "") (> suppress-count 0))
	     (setq mail-headers "\nOnly junk mail..."))
    (concat mail-headers "\n"
	    (make-string (+ display-time-mail-balloon-from-width
			    display-time-mail-balloon-subject-width
			    display-time-mail-balloon-gnus-split-width
			    3) (string-to-char "-"))
	    "\n"
	     (if (> not-displayed 0)
		 (concat "More:       " (number-to-string not-displayed)"\n"))
	     (if (> suppress-count 0)
		 (concat "Suppressed: " (number-to-string suppress-count)))
	     )))


(defun display-time-mail-sign (&optional textual)
  "*A function giving back the object indicating 'mail' which
is the value of display-time-mail-sign when running under X,
display-time-echo-area is nil and display-time-show-icons-maybe is t.
It is the value of display-time-mail-sign-string otherwise or when
the optional parameter TEXTUAL is non-nil." 
  (if (not (display-time-can-do-graphical-display textual))
      display-time-mail-sign-string
    (list " " display-time-mail-sign " ")))

(defun display-time-no-mail-sign (&optional textual)
  "*A function giving back the object indicating 'no mail' which
is the value of display-time-no-mail-sign when running under X,
display-time-echo-area is nil and display-time-show-icons-maybe is t.
It is the value of display-time-no-mail-sign-string otherwise or when
the optional parameter TEXTUAL is non-nil." 
  (if (not (display-time-can-do-graphical-display textual))
      display-time-no-mail-sign-string
    (list " " display-time-no-mail-sign " ")))

(defcustom display-time-form-list
  (list 'date 'time 'load 'mail)
  "*This list describes the format of the strings/glyphs
which are to be displayed by display-time.
The old variable display-time-string-forms is only used if
display-time-compatible is non-nil. It is a list consisting of
strings or any of the following symbols:

There are three complex specs whose behaviour is changed via
the setting of various variables 

date:          This prints out the date in a manner compatible to
               the default value of the obsolete variable
               display-time-string-forms. It respects the variable
               display-time-day-and-date. If this is t it will print
               out the current date in the form DAYNAME MONTH DAY
               otherwise it will print nothing.
	      
time:          This prints out the time in a manner compatible to 
               the default value of the obsolete variable
               display-time-string-forms. It respects the variable
               display-time-24hr-format. If this is t it will print
               out the current hours in 24-hour format, if nil the
               hours will be printed in 12-hour format and the
               minutes will be followed by 'AM' or 'PM'.
	      
time-text:     The same as above, but will not use a glyph
	      
The other specs are simpler, as their meaning is not changed via
variables.

24-hours:      This prints the hours in 24-hours format
	      
24-hours-text: The same as above, but will not use a glyph
	      
12-hours:      This prints the hours in 12-hours format
	      
12-hours-text: The same as above, but will not use a glyph
	      
am-pm:         This prints am or pm.

Timezone:      This prints out the local timezone
	      
am-pm-text:    The same as above, but will not use a glyph
	      
minutes:       This prints the minutes.
	      
minutes-text:  The same as above, but will not use a glyph
	      
day:           This prints out the current day as a number. 
	      
dayname:       This prints out today's name.
	      
month:         This prints out the current month as a number
	      
monthname:     This prints out the current month's name

year:          This prints out the current year.
	      
load:          This prints out the system's load.
	      
load-text:     The same as above, but will not use a glyph
	      
mail:          This displays a mail indicator. Under X this will 
               normally be a small icon which changes depending if 
               there is new mail or not.
	      
mail-text:     The same as above, but will not use a glyph"
  :group 'display-time
  :type '(repeat (choice :tag "Symbol/String"
			 (const :tag "Date" date)
			 (const :tag "Time" time)
			 (const :tag "Time (text)" time-text)
			 (const :tag "24 hour format" 24-hours)
			 (const :tag "24 hour format (text)" 24-hours-text)
			 (const :tag "12 hour format" 12-hours)
			 (const :tag "12 hour format (text)" 12-hours-text)
			 (const :tag "AM/PM indicator" am-pm)
			 (const :tag "AM/PM indicator (text)" am-pm-text)
			 (const :tag "Timezone" timezone)
			 (const :tag "Minutes" minutes)
			 (const :tag "Minutes (text)" minutes-text)
			 (const :tag "Day" day)
			 (const :tag "Dayname" dayname)
			 (const :tag "Month" month)
			 (const :tag "Monthname" monthname)
			 (const :tag "Year" year)
			 (const :tag "Load" load)
			 (const :tag "Load (text)" load-text)
			 (const :tag "Mail sign" mail)
			 (const :tag "Mail sign (text)" mail-text)
			 (string :tag "String"))))

(defun display-time-evaluate-list ()
  "Evalute the variable display-time-form-list"
  (let ((list display-time-form-list) elem tmp result)
    (while (setq elem (pop list))
      (cond ((stringp elem) (push elem tmp))
	    ((eq elem 'date)
	     (push (if display-time-day-and-date
		       (format "%s %s %s " dayname monthname day) "") tmp))
	    ((eq elem 'time)
	     (progn
	       (push (display-time-convert-num
		      (format "%s:%s"
			      (if display-time-24hr-format 24-hours 12-hours)
			      minutes)) tmp) 
	       (if (not display-time-24hr-format)
		   (push (display-time-convert-am-pm am-pm) tmp))))
	    ((eq elem 'time-text)
	     (push (display-time-convert-num
		    (format "%s:%s"
			   (if display-time-24hr-format 24-hours 12-hours)
			   minutes) t) tmp)
	     (if (not display-time-24hr-format)
		 (push (display-time-convert-am-pm am-pm t) tmp)))
	    ((eq elem 'day) (push day tmp))
	    ((eq elem 'dayname) (push dayname tmp))
	    ((eq elem 'month) (push month tmp))
	    ((eq elem 'monthname) (push monthname tmp))
	    ((eq elem '24-hours)
	     (push (display-time-convert-num 24-hours) tmp))
	    ((eq elem 'year)
	     (push year tmp))
	    ((eq elem '24-hours-text)
	     (push (display-time-convert-num 24-hours t) tmp))
	    ((eq elem '12-hours)
	     (push (display-time-convert-num 12-hours) tmp))
	    ((eq elem '12-hours-text)
	     (push (display-time-convert-num 12-hours t) tmp))
	    ((eq elem 'minutes)
	     (push (display-time-convert-num minutes) tmp))
	    ((eq elem 'seconds)
	     (push (display-time-convert-num seconds) tmp))
	    ((eq elem 'minutes-text)
	     (push (display-time-convert-num minutes t) tmp))
	    ((eq elem 'am-pm)
	     (push (display-time-convert-am-pm am-pm) tmp))
	    ((eq elem 'am-pm-text)
	     (push (display-time-convert-am-pm am-pm t) tmp))
	    ((eq elem 'timezone)
	     (push time-zone tmp))
	    ((eq elem 'load)
	     (push (display-time-convert-load load) tmp))
	    ((eq elem 'load-text)
	     (push (display-time-convert-load load t) tmp))
	    ((eq elem 'mail)
	     (push (if mail (display-time-mail-sign)
		     (display-time-no-mail-sign)) tmp))
	    ((eq elem 'mail-text)
	     (push (if mail (display-time-mail-sign t)
		     (display-time-no-mail-sign t)) tmp))
	    ))
    ;; We know that we have a list containing only of strings if
    ;; display-time-echo-area is t. So we construct this string from
    ;; the list. Else we just reverse the list and give it as result.
    (if (not display-time-echo-area) (setq result (reverse tmp))
      (while (setq elem (pop tmp))
	(setq result (concat elem result))))
    result))
    
	    
(defvar display-time-string-forms
  '((if display-time-day-and-date
        (format "%s %s %s " dayname monthname day)
      "")
    (format "%s:%s%s"
            (if display-time-24hr-format 24-hours 12-hours)
            minutes
            (if display-time-24hr-format "" am-pm))
    load
    (if mail " Mail" ""))
    "*It will only be used if display-time-compatible is t.
A list of expressions governing display of the time in the mode line.
This expression is a list of expressions that can involve the keywords
`load', `day', `month', and `year', `12-hours', `24-hours', `minutes',
`seconds', all numbers in string form, and `monthname', `dayname', `am-pm',
and `time-zone' all alphabetic strings and `mail' a true/nil string value.

For example, the form

  '((substring year -2) \"/\" month \"/\" day
    \" \" 24-hours \":\" minutes \":\" seconds
    (if time-zone \" (\") time-zone (if time-zone \")\"))

would give mode line times like `94/12/30 21:07:48 (UTC)'.")

(make-obsolete-variable 'display-time-string-forms
			"You should use the new facilities for `display-time'.
Look at display-time-form-list.")   

(defun display-time-function ()
  (let* ((now (current-time))
	 (nowhigh (* (- (nth 0 now) (* (/ (nth 0 now) 10) 10)) 65536))
	 (time (current-time-string now))
         (load (condition-case ()
                   (if (zerop (car (load-average))) ""
                     (let ((str (format " %03d" (car (load-average)))))
                       (concat (substring str 0 -2) "." (substring str -2))))
                 (error "")))
         (mail-spool-file (or display-time-mail-file
                              (getenv "MAIL")
                              (concat rmail-spool-directory
                                      (user-login-name))))
	 (mail (and (stringp mail-spool-file)
		    (or (null display-time-server-down-time)
			;; If have been down for 20 min, try again.
			(> (- (+ (nth 1 now) nowhigh)
			      display-time-server-down-time)
			   1200))
		    (let ((start-time (current-time)))
		      (prog1
			  (display-time-file-nonempty-p mail-spool-file)
			(setq now (current-time)
			      nowhigh (* (- (nth 0 now) (* (/ (nth 0 now) 10) 10)) 65536))
			(if (> (- (+ (nth 1 now) nowhigh)
				  (+ (nth 1 start-time)
				     (* (- (nth 0 start-time) (* (/ (nth 0 start-time) 10) 10)) 65536)))
			       20)
			    ;; Record that mail file is not accessible.
			    (setq display-time-server-down-time 
				  (+ (nth 1 now) nowhigh))
			  ;; Record that mail file is accessible.
			  (setq display-time-server-down-time nil))))))
         (24-hours (substring time 11 13))
         (hour (string-to-int 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (>= hour 12) "pm" "am"))
         (minutes (substring time 14 16))
         (seconds (substring time 17 19))
         (time-zone (car (cdr (current-time-zone now))))
         (day (substring time 8 10))
         (year (substring time 20 24))
         (monthname (substring time 4 7))
         (month
          (cdr
           (assoc
            monthname
            '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4")
              ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8")
              ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
         (dayname (substring time 0 3)))
    (setq display-time-string
	  (if display-time-compatible
	      (mapconcat 'eval display-time-string-forms "")
	    (display-time-evaluate-list)))
    ;; This is inside the let binding, but we are not going to document
    ;; what variables are available.
    (run-hooks 'display-time-hook))
  (if display-time-echo-area
      (or (> (minibuffer-depth) 0)
	  ;; don't stomp echo-area-buffer if reading from minibuffer now.
	  (save-excursion
	    (save-window-excursion
	      (select-window (minibuffer-window))
	      (erase-buffer)
	      (indent-to (- (frame-width) (length display-time-string) 1))
	      (insert display-time-string)
	      (message (buffer-string)))))
    (force-mode-line-update)
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)))

(defun display-time-file-nonempty-p (file)
  (let ((attributes (file-attributes (file-chase-links file))))
    (and attributes
	 (< 0 (nth 7 attributes))
	 (or display-time-ignore-read-mail
	     (> (car (nth 5 attributes)) (car (nth 4 attributes)))
	     (and (= (car (nth 5 attributes)) (car (nth 4 attributes)))
		  (> (cadr (nth 5 attributes)) (cadr (nth 4 attributes))))))))

(provide 'time)

;;; time.el ends here
