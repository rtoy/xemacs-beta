;;; time.el --- display time and load in mode line of Emacs.

;; Copyright (C) 1985, 86, 87, 93, 94, 1996 Free Software Foundation, Inc.

;; Maintainer: FSF,     XEmacs add-ons (C) by Jens T. Lautenbacher
;;                      mail <jens@lemming0.lem.uni-karlsruhe.de>
;;                      for comments/fixes about the enhancements.

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; Facilities to display current time/date and a new-mail indicator
;; in the Emacs mode line.  The single entry point is `display-time'.

;; See also reportmail.el.
;; This uses the XEmacs timeout-event mechanism, via a version
;; of Kyle Jones' itimer package.

;;; JTL: This is in a wide part reworked for XEmacs so it won't use
;;;      the old mechanism for specifying what is to be displayed.
;;;      The starting variable to look at is `display-time-form-list'

;;; Code:

(require 'itimer)

(defvar display-time-compatible nil
  "*This variable may be set to nil to get the old behaviour of display-time.
This means no display of a spiffy mail icon or use of the display-time-form-list
instead of the old display-time-string-form.")

(defvar display-time-mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
Non-nil and not a string means don't check for mail.  nil means use
default, which is system-dependent, and is the same as used by Rmail.")

;;;###autoload
(defvar display-time-day-and-date nil "\
*Non-nil means \\[display-time] should display day and date as well as time.")

(defvar display-time-interval 20
  "*Seconds between updates of time in the mode line.")

(defvar display-time-24hr-format nil
  "*Non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23.
Nil means 1 <= hh <= 12, and an AM/PM suffix is used.")

(defvar display-time-echo-area nil
  "*If non-nil, display-time will use the echo area instead of the mode line.")

(defvar display-time-string nil)

(defvar display-time-hook nil
  "*List of functions to be called when the time is updated on the mode line.")

(defvar display-time-server-down-time nil
   "Time when mail file's file system was recorded to be down.
If that file system seems to be up, the value is nil.")

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

(defvar display-time-show-icons-maybe t
  "Use icons to indicate the mail status if possible")

(defvar display-time-icons-dir (concat data-directory "time/"))

(defvar display-time-mail-sign-string " Mail"
  "The string used as mail indicator in the echo area
(and in the modeline if display-time-show-icons-maybe is nil)
if display-time-echo-area is t")

(defvar display-time-no-mail-sign-string ""
  "The string used as no-mail indicator in the echo area
(and in the modeline if display-time-show-icons-maybe is nil)
if display-time-echo-area is t")
 
(defvar display-time-mail-sign
  (progn
    (let* ((file (concat display-time-icons-dir "letter.xpm"))
	   (glyph (if (featurep 'xpm) (make-glyph file)
		    display-time-mail-sign-string))
	   (ext (make-extent nil nil)))
      (cons ext glyph)))
  "A variable holding a cons cell (ext . glyph)
which gives an indicator for new mail in the modeline") 

(defvar display-time-no-mail-sign
    (progn
    (let* ((file (concat display-time-icons-dir "no-letter.xpm"))
	   (glyph (if (featurep 'xpm) (make-glyph file)
		   display-time-no-mail-sign-string))
	   (ext (make-extent nil nil)))
      (cons ext glyph)))
    "A variable holding a cons cell (ext . glyph) which gives
an indicator for `no mail' in the modeline") 

(defun display-time-string-to-char-list (str)
  (mapcar (function identity) str))


(if (featurep 'xpm)
    (progn
      (setq display-time-1-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "1.xpm"))))
      (setq display-time-2-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "2.xpm"))))
      (setq display-time-3-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "3.xpm"))))
      (setq display-time-4-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "4.xpm"))))
      (setq display-time-5-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "5.xpm"))))
      (setq display-time-6-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "6.xpm"))))
      (setq display-time-7-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "7.xpm"))))
      (setq display-time-8-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "8.xpm"))))
      (setq display-time-9-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "9.xpm"))))
      (setq display-time-0-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "0.xpm"))))
      (setq display-time-:-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "dp.xpm"))))
      (setq display-time-load-0.0-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-0.0.xpm"))))
      (setq display-time-load-0.5-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-0.5.xpm"))))
      (setq display-time-load-1.0-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-1.0.xpm"))))
      (setq display-time-load-1.5-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-1.5.xpm"))))
      (setq display-time-load-2.0-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-2.0.xpm"))))
      (setq display-time-load-2.5-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-2.5.xpm"))))
      (setq display-time-load-3.0-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "l-3.0.xpm"))))
      (setq display-time-am-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "am.xpm"))))
      (setq display-time-pm-glyph
	    (cons (make-extent nil nil)
		  (make-glyph	(concat display-time-icons-dir "pm.xpm"))))
      )) 


(defun display-time-convert-num-to-pics (string)
  (let ((list (display-time-string-to-char-list string))
	elem result tmp)
    (if (not (and display-time-show-icons-maybe
		  (eq (console-type) 'x)
		  (not display-time-echo-area))) string
      (while (setq elem (pop list))
	(push (eval (intern-soft (concat "display-time-"
					 (char-to-string elem)
					 "-glyph"))) tmp))
      (setq result (reverse tmp))))) 

(defvar display-time-load-list
  (list 0.2 0.5 0.8 1.1 1.8 2.6)
  "*A list giving six thresholds for the load which correspond
to the six different icons to be displayed as a load indicator")

(defun display-time-convert-load-to-glyph (n)
  (let ((load-number (string-to-number n))
	(alist (list (cons 0.0 0.0)
		    (cons 0.5 (car display-time-load-list))
		    (cons 1.0 (cadr display-time-load-list))
		    (cons 1.5 (caddr display-time-load-list))
		    (cons 2.0 (cadddr display-time-load-list))
		    (cons 2.5 (cadr (cdddr display-time-load-list)))
		    (cons 3.0 (caddr (cdddr display-time-load-list)))
		    (cons 100000 100000)))
	result elem)
    (if (not (and display-time-show-icons-maybe
		  (eq (console-type) 'x)
		  (not display-time-echo-area))) n
      (while (>= load-number (cdr (setq elem (pop alist))))
	(setq result (eval (intern-soft (concat
					 "display-time-load-"
					 (number-to-string (car elem))
					 "-glyph")))))
      result)))

(defun display-time-convert-am-pm (n)
  (if (not (and display-time-show-icons-maybe
		(eq (console-type) 'x)
		(not display-time-echo-area))) n
    (cond ((equal n "am") display-time-am-glyph)
	  ((equal n "pm") display-time-pm-glyph))))


(defun display-time-mail-sign ()
  "*A function giving back the object indicating 'mail' which
is the value of display-time-mail-sign when running under X,
display-time-echo-area is nil and display-time-show-icons-maybe is t.
It is the value of display-time-mail-sign-string otherwise." 
  (if (or (not (eq (console-type) 'x))
	  display-time-echo-area
	  (not display-time-show-icons-maybe))
      display-time-mail-sign-string
    display-time-mail-sign))

(defun display-time-no-mail-sign ()
  "*A function giving back the object indicating 'no mail' which
is the value of display-time-no-mail-sign when running under X,
display-time-echo-area is nil and display-time-show-icons-maybe is t.
It is the value of display-time-no-mail-sign-string otherwise." 
  (if (or (not (eq (console-type) 'x))
	  display-time-echo-area
	  (not display-time-show-icons-maybe))
      display-time-no-mail-sign-string
    display-time-no-mail-sign))

(defvar display-time-form-list
  (list 'date-compatible 'time-compatible 'load 'mail)
  "*This list describes the format of the strings/glyphs which are to be
displayed by display-time. The old variable display-time-string-forms is
only used if display-time-compatible is non-nil. It is a list consisting of
strings or any of the following symbols:

date-compatible:    This prints out the date in a manner compatible to
                    the default value of the obsolete variable 
                    display-time-string-forms. It respects the variable
                    display-time-day-and-date. If this is t it will print
                    out the current date in the form DAYNAME MONTH DAY
                    otherwise it will print nothing.

time-compatible:    This prints out the time in a manner compatible to 
                    the default value of the obsolete variable
                    display-time-string-forms. It respects the variable
                    display-time-24hr-format. If this is t it will print
                    out the current hours in 24-hour format, if nil the
                    hours will be printed in 12-hour format and the
                    minutes will be followed by 'AM' or 'PM'.

24-hours:           This prints the hours in 24-hours format

12-hours:           This prints the hours in 12-hours format

am-pm:              This prints Am or Pm.

dp:                 This prints a \":\", maybe as an icon

minutes:            This prints the minutes.

day:                This prints out the current day as a number. 

dayname:            This prints out today's name.

month:              This prints out the current month as a number

monthname:          This prints out the current month's name

load:               This prints out the system's load.

mail:               This displays a mail indicator. Under X this will 
                    normally be a small icon which changes depending if 
                    there is new mail or not.")

(defun display-time-evaluate-list ()
  "Evalute the variable display-time-form-list"
  (let ((list display-time-form-list) elem tmp result)
    (while (setq elem (pop list))
      (cond ((stringp elem) (push elem tmp))
	    ((eq elem 'date-compatible)
	     (push (if display-time-day-and-date
		       (format "%s %s %s " dayname monthname day) "") tmp))
	    ((eq elem 'time-compatible)
	     (progn
	       (push (display-time-convert-num-to-pics
		      (format "%s:%s"
			      (if display-time-24hr-format 24-hours 12-hours)
			      minutes)) tmp)
	       (if (not display-time-24hr-format)
		   (push (display-time-convert-am-pm am-pm) tmp))))
	    ((eq elem 'day) (push day tmp))
	    ((eq elem 'dayname) (push dayname tmp))
	    ((eq elem 'month) (push month tmp))
	    ((eq elem 'monthname) (push monthname tmp))
	    ((eq elem '24-hours) (push (display-time-convert-num-to-pics 24-hours)
				       tmp))
	    ((eq elem '12-hours) (push (display-time-convert-num-to-pics 12-hours)
				       tmp))
	    ((eq elem 'minutes)  (push (display-time-convert-num-to-pics minutes)
				       tmp))
	    ((eq elem 'am-pm) (push am-pm tmp))
	    ((eq elem 'dp) (push (display-time-convert-num-to-pics ":") tmp))
	    ((eq elem 'load)
	     (push (display-time-convert-load-to-glyph load) tmp))
	    ((eq elem 'mail) (push (if mail (display-time-mail-sign)
				     (display-time-no-mail-sign))
				   tmp))))
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
    "*THIS IS OBSOLETE! It will only be used if display-time-compatible is t.
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

(defun display-time-function ()
  (let* ((now (current-time))
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
			(> (- (nth 1 (current-time))
			      display-time-server-down-time)
			   1200))
		    (let ((start-time (current-time)))
		      (prog1
			  (display-time-file-nonempty-p mail-spool-file)
			(if (> (- (nth 1 (current-time)) (nth 1 start-time))
			       20)
			    ;; Record that mail file is not accessible.
			    (setq display-time-server-down-time 
				  (nth 1 (current-time)))
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
	      (indent-to (- (screen-width) (length display-time-string) 1))
	      (insert display-time-string)
	      (message (buffer-string)))))
    (force-mode-line-update)
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)))

(defun display-time-file-nonempty-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes (file-chase-links file))))))

(provide 'time)

;;; time.el ends here
