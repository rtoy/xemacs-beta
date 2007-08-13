;;; $Id: hm--date.el,v 1.1.1.2 1996/12/18 03:46:46 steve Exp $
;;;
;;; Copyright (C) 1993, 1996  Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	Defines the function hm--date, which returns the date in the
;;;	format "day-month-year" like "30-Jun-1993".
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load path directories.
;;;	The files which uses this function must only have
;;;	following line:
;;;		(require 'hm--date)
;;;


(provide 'hm--date)



(defun hm--date ()
  "Returns the current date in the format \"day-month-year\"."
  (let* ((time-string (current-time-string))
	(day (substring time-string 8 10))
	(month (substring time-string 4 7))
	(year (substring time-string 20 24)))
    (concat day "-" month "-" year)))
