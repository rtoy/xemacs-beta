;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-cp-p.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  Support for preserving file modtimes with copies. i.e. cp -p
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Fri Feb 18 03:28:22 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 12:17:33 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-cp-p)
(require 'efs)

;;;; Internal Variables

(defconst efs-cp-p-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

(defvar efs-local-timezone nil)
;; cache.

;;; Utility functions

(efs-define-fun efs-gmt-time ()
  ;; Get the time as the number of seconds elapsed since midnight,
  ;; Jan 1, 1970, GMT.  Emacs 18 doesn't have `current-time' function.
  (let ((time (current-time)))
    (list (car time) (nth 1 time))))

(defun efs-local-time ()
  (let ((str (current-time-string)))
    (efs-seconds-elapsed
     (string-to-int (substring str -4))
     (cdr (assoc (substring str 4 7) efs-month-alist))
     (string-to-int (substring str 8 10))
     (string-to-int (substring str 11 13))
     (string-to-int (substring str 14 16))
     0))) ; don't care about seconds
   
(defun efs-local-timezone ()
  ;; Returns the local timezone as an integer. Right two digits the minutes,
  ;; others the hours.
  (or efs-local-timezone
      (setq efs-local-timezone
	    (let* ((local (efs-local-time))
		   (gmt (efs-gmt-time))
		   (sign 1)
		   (diff (efs-time-minus local gmt))
		   hours minutes)
	      ;; 2^16 is 36 hours.
	      (if (zerop (car diff))
		  (setq diff (nth 1 diff))
		(error "Weird timezone!"))
	      (setq diff (/ (- (nth 1 local) (nth 1 gmt)) 60))
	      (setq hours (/ diff 60))
	      (setq minutes (% diff 60))
	      (if (< diff 0)
		  (setq sign -1
			hours (- hours)
			minutes (- minutes)))
	      ;; Round minutes
	      (setq minutes (* 10 (/ (+ minutes 5) 10)))
	      (if (= minutes 60)
		  (setq hours (1+ hours)
			minutes 0))
	      (* sign (+ (* hours 100) minutes))))))
	    
(defun efs-last-day-of-month (month year)
  ;; The last day in MONTH during YEAR.
  ;; Taken from calendar.el. Thanks.
  (if (and
       (or
	(and (=  (% year   4) 0)
	     (/= (% year 100) 0))  ; leap-year-p
	(= (% year 400) 0))
       (= month 2))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defun efs-make-date-local (year month day hour minutes seconds)
  ;; Takes a GMT date (list of integers), and returns the local time.
  (let* ((lzone (efs-local-timezone))
	 (lminutes (% lzone 100))
	 (lhour (/ lzone 100)))
    (setq minutes (+ minutes lminutes))
    (cond ((> minutes 60)
	   (setq minutes (- minutes 60)
		 hour (1+ hour)))
	  ((< minutes 0)
	   (setq minutes (+ minutes 60)
		 hour (1- hour))))
    (setq hour (+ lhour hour))
    (if (or (< hour 0) (> hour 23))
	(progn
	  (cond ((< hour 0)
		 (setq hour (+ hour 24)
		       day (1- day)))
		((> hour 23)
		 (setq hour (- hour 24)
		       day (1+ day))))
	  (if (or (zerop day) (> day
				 (efs-last-day-of-month month year)))
	      (cond ((zerop day)
		     (setq month (1- month))
		     (if (zerop month)
			 (setq year (1- year)
			       month 12))
		     (setq day (efs-last-day-of-month month year)))
		    ((> day (efs-last-day-of-month month year))
		     (setq month (1+ month)
			   day 1)
		     (if (= month 13)
			 (setq year (1+ year)
			       month 1)))))))
    (list year month day hour minutes seconds)))

;;;; Entry function

(defun efs-set-mdtm-of (filename newname &optional cont)
  ;; NEWNAME must be local
  ;; Always works NOWAIT = 0
  (let* ((parsed (efs-ftp-path filename))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (file (nth 2 parsed)))
    (if (efs-get-host-property host 'mdtm-failed)
	(and cont (efs-call-cont cont 'failed "" "") nil)
      (efs-send-cmd
       host user
       (list 'quote 'mdtm file)
       nil nil
       (efs-cont (result line cont-lines) (host newname cont)
	 (if (or result
		 (not (string-match efs-mdtm-msgs line)))
	     (efs-set-host-property host 'mdtm-failed t)
	   (let ((time (apply 'efs-make-date-local
			      (mapcar 'string-to-int
				      (list
				       (substring line 4 8)
				       (substring line 8 10)
				       (substring line 10 12)
				       (substring line 12 14)
				       (substring line 14 16)
				       (substring line 16 18))))))
	     (if time
		 (call-process "touch" nil 0 nil "-t"
			       (format "%04d%02d%02d%02d%02d.%02d"
				       (car time) (nth 1 time)
				       (nth 2 time) (nth 3 time)
				       (nth 4 time) (nth 5 time))
			       newname))))
	 (if cont (efs-call-cont cont result line cont-lines)))
       0))))

;;; end of efs-cp-p.el
