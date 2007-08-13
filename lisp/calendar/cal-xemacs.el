;;; cal-xemacs.el --- calendar functions for menu bar and popup menu support
;;; Original file is cal-menu.el.

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;;	Lara Rios <lrios@coewl.cen.uiuc.edu>
;; Ported to XEmacs by Chuck Thompson <cthomp@cs.uiuc.edu>
;; Keywords: calendar
;; Human-Keywords: calendar, popup menus, menu bar

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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This collection of functions implements menu bar and popup menu support for
;; calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

;;(define-key calendar-mode-map 'button2 'calendar-mouse-2-date-menu)
;;(define-key calendar-mode-map 'button2up 'ignore)

(defconst calendar-popup-menu-3
  '("Calendar"
    ["Scroll forward" scroll-calendar-left-three-months t]
    ["Scroll backward" scroll-calendar-right-three-months t]
    ["Mark diary entries" mark-diary-entries t]
    ["List holidays" list-calendar-holidays t]
    ["Mark holidays" mark-calendar-holidays t]
    ["Unmark" calendar-unmark t]
    ["Lunar phases" calendar-phases-of-moon t]
    ["Show diary" show-all-diary-entries t]
    ["Exit calendar" exit-calendar t]
    ))

(defun calendar-popup-menu-3 (e)
  (interactive "@e")
  (popup-menu calendar-popup-menu-3))
(define-key calendar-mode-map 'button3 'calendar-popup-menu-3)

(defvar calendar-foobar nil)

(defun calendar-popup-menu-2 (e)
  (interactive "@e")
  (setq calendar-foobar (calendar-event-to-date e t))
  (let ((menu (list (format "Menu - %s" (calendar-date-string calendar-foobar) t t)
	       "-----"
	       ["Holidays" calendar-mouse-holidays t]
	       ["Mark date" calendar-mouse-set-mark t]
	       ["Sunrise/sunset" calendar-mouse-sunrise/sunset t]
	       ["Other calendars" calendar-mouse-print-dates (calendar-event-to-date e)]
	       ["Diary entries" calendar-mouse-view-diary-entries t]
	       ["Insert diary entry" calendar-mouse-insert-diary-entry t]
	       ["Other Diary file entries"
		calendar-mouse-view-other-diary-entries
		(calendar-cursor-to-date)]
	       )))
    (popup-menu menu)))
(define-key calendar-mode-map 'button2 'calendar-popup-menu-2)

(defconst calendar-scroll-menu
  '("Scroll"
    ["Forward 1 Month" scroll-calendar-left t]
    ["Forward 3 Months" scroll-calendar-left-three-months t]
    ["Forward 1 Year" (scroll-calendar-left-three-months 4) t]
    ["Backward 1 Month" scroll-calendar-right t]
    ["Backward 3 Months" scroll-calendar-right-three-months t]
    ["Backward 1 Year" (scroll-calendar-right-three-months 4) t]))

(defconst calendar-goto-menu
  '("Goto"
    ["Today" calendar-current-month t]
    ["Beginning of week" calendar-beginning-of-week (calendar-cursor-to-date)]
    ["End of week" calendar-end-of-week (calendar-cursor-to-date)]
    ["Beginning of month" calendar-beginning-of-month (calendar-cursor-to-date)]
    ["End of month" calendar-end-of-month (calendar-cursor-to-date)]
    ["Beginning of year" calendar-beginning-of-year (calendar-cursor-to-date)]
    ["End of year" calendar-end-of-year (calendar-cursor-to-date)]
    ["Other date" calendar-goto-date t]
    ["ISO date" calendar-goto-iso-date t]
    ["Astronomical date" calendar-goto-astro-day-number t]
    ["Hebrew date" calendar-goto-hebrew-date t]
    ["Islamic date" calendar-goto-islamic-date t]
    ["Julian date" calendar-goto-julian-date t]
    ("Mayan date"
     ["Next Tzolkin" calendar-next-tzolkin-date t]
     ["Previous Tzolkin" calendar-previous-tzolkin-date t]
     ["Next Haab" calendar-next-haab-date t]
     ["Previous Haab" calendar-previous-haab-date t]
     ["Next Round" calendar-next-calendar-round-date t]
     ["Previous Round" calendar-previous-calendar-round-date t])
    ["French date" calendar-goto-french-date t]))

(defconst calendar-holidays-menu
  '("Holidays"
    ["One day" calendar-cursor-holidays (calendar-cursor-to-date)]
    ["3 months" list-calendar-holidays t]
    ["Mark" mark-calendar-holidays t]
    ["Unmark" calendar-unmark t]))

(defconst calendar-diary-menu
  '("Diary"
    ["Other file" view-other-diary-entries (calendar-cursor-to-date)]
    ["Cursor date" view-diary-entries (calendar-cursor-to-date)]
    ["Mark all" mark-diary-entries t]
    ["Show all" show-all-diary-entries t]
    ["Insert daily"insert-diary-entry t]
    ["Insert weekly" insert-weekly-diary-entry (calendar-cursor-to-date)]
    ["Insert monthly" insert-monthly-diary-entry (calendar-cursor-to-date)]
    ["Insert yearly" insert-yearly-diary-entry (calendar-cursor-to-date)]
    ["Insert anniversary" insert-anniversary-diary-entry (calendar-cursor-to-date)]
    ["Insert block" insert-block-diary-entry (calendar-cursor-to-date)]
    ["Insert cyclic" insert-cyclic-diary-entry (calendar-cursor-to-date)]
    ["Insert Islamic" calendar-mouse-insert-islamic-diary-entry (calendar-cursor-to-date)]
    ["Insert Hebrew" calendar-mouse-insert-hebrew-diary-entry (calendar-cursor-to-date)]))

(defun calendar-add-menus ()
  (set-buffer-menubar (copy-sequence current-menubar))
  (if (assoc "Calendar" current-menubar)
      nil
    (add-submenu nil '("Calendar"))
    (if (not (assoc "Scroll" current-menubar))
	(add-submenu '("Calendar") calendar-scroll-menu))
    (if (not (assoc "Goto" current-menubar))
	(add-submenu '("Calendar") calendar-goto-menu))
    (if (not (assoc "Holidays" current-menubar))
	(add-submenu '("Calendar") calendar-holidays-menu))
    (if (not (assoc "Diary" current-menubar))
	(add-submenu '("Calendar") calendar-diary-menu))
    (if (not (assoc "Moon" current-menubar))
	(add-menu-button '("Calendar") ["Moon" calendar-phases-of-moon t]))))

(defun calendar-event-to-date (event &optional error)
  "Date of last event.
If event is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (save-excursion
    (goto-char (event-point event))
    (calendar-cursor-to-date error)))

(defun calendar-mouse-insert-hebrew-diary-entry (event)
  "Pop up menu to insert a Hebrew-date diary entry."
  (interactive "e")
  (let ((menu (list (format "Hebrew insert menu - %s"
			    (calendar-hebrew-date-string
			     (calendar-cursor-to-date)))
		    "-----"
		    ["One time" insert-hebrew-diary-entry t]
		    ["Monthly" insert-monthly-hebrew-diary-entry t]
		    ["Yearly" insert-yearly-hebrew-diary-entry t])))
    (popup-menu menu)))

(defun calendar-mouse-insert-islamic-diary-entry (event)
  "Pop up menu to insert an Islamic-date diary entry."
  (interactive "e")
  (let ((menu (list (format "Islamic insert menu - %s"
			    (calendar-islamic-date-string
			     (calendar-cursor-to-date)))
		    "-----"
		    ["One time" insert-islamic-diary-entry t]
		    ["Monthly" insert-monthly-islamic-diary-entry t]
		    ["Yearly" insert-yearly-islamic-diary-entry t])))
    (popup-menu menu)))

(defun calendar-mouse-sunrise/sunset ()
  "Show sunrise/sunset times for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (setq calendar-foobar nil)
    (calendar-sunrise-sunset)))

(defun calendar-mouse-holidays ()
  "Show holidays for mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (setq calendar-foobar nil)
    (calendar-cursor-holidays)))

(defun calendar-mouse-view-diary-entries ()
  "View diary entries on mouse-selected date."
  (interactive)
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (setq calendar-foobar nil)
    (view-diary-entries 1)))

(defun calendar-mouse-view-other-diary-entries (event)
  "View diary entries from alternative file on mouse-selected date."
  (interactive "e")
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (call-interactively 'view-other-diary-entries)))

(defun calendar-mouse-insert-diary-entry (event)
  "Insert diary entry for mouse-selected date."
  (interactive "e")
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (insert-diary-entry nil)))

(defun calendar-mouse-set-mark ()
  "Mark the date under the cursor."
  (interactive)
  (save-excursion
    (calendar-goto-date calendar-foobar)
    (setq calendar-foobar nil)
    (calendar-set-mark nil)))

(defun calendar-mouse-print-dates ()
  "Pop up menu of equivalent dates to mouse selected date."
  (interactive)
  (let* ((menu (list (format "Date Menu - %s (Gregorian)"
			     (calendar-date-string calendar-foobar))
		     "-----"
		     (calendar-day-of-year-string calendar-foobar)
		     (format "ISO date: %s" (calendar-iso-date-string calendar-foobar))
		     (format "Julian date: %s"
			     (calendar-julian-date-string calendar-foobar))
		     (format "Astronomical (Julian) date (before noon): %s"
			     (calendar-astro-date-string calendar-foobar))
		     (format "Hebrew date (before sunset): %s"
			     (calendar-hebrew-date-string calendar-foobar))
		     (let ((i (calendar-islamic-date-string calendar-foobar)))
		       (if (not (string-equal i ""))
			   (format "Islamic date (before sunset): %s" i)))
		     (let ((f (calendar-french-date-string calendar-foobar)))
		       (if (not (string-equal f ""))
			   (format "French Revolutionary date: %s" f)))
		     (format "Mayan date: %s" (calendar-mayan-date-string calendar-foobar)))))
    (popup-menu menu))
  (setq calendar-foobar nil))

(run-hooks 'cal-xemacs-load-hook)

(provide 'cal-xemacs)

;;; cal-menu.el ends here
