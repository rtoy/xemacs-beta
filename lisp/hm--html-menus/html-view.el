;;; html-view.el --- routines for communicating with a NCSA Mosaic process
;;;
;;; Some routines for communicating with a NCSA Mosaic process.
;;; 
;;; Copyright (C) 1993 Ron Tapia tapia@hydra.unm.edu
;;; Copyright (C) 1994, 1995 Heiko Münkel muenkel@tnt.uni-hannover.de
;;;
;;; VERSION: 1.10
;;; LAST MODIFIED: 20/07/95
;;; Keywords: comm unix wp help
;;;
;;; Adapted to the lemacs: 19.07.1993 Heiko Muenkel 
;;;			   (muenkel@tnt.uni-hannover.de)
;;; Changed: 19.07.1993 by Heiko Muenkel
;;; Changed: 28.12.1993 by Heiko Muenkel
;;;	Changed (signal-process id 30)
;;;	to	(signal-process id html-sigusr1-signal-value)
;;;	Addapted the file for the new Mosaic-2.1
;;;	Thanks to Neal Becker, who has reported this problem.
;;;	The file now requires the package hm--html-menus.
;;;	But you can also delete the line (require 'hm--html) and
;;;	add a line like (setq html-sigusr1-signal-value 30)
;;; Changed: 10.01.1994 by Heiko Muenkel
;;;	Fixed a bug.
;;; Changed: 16.12.1994 by Heiko Münkel
;;;     Addapted the file for Mosaic-2.4.
;;; Changed: 03.02.1995 by Heiko Münkel
;;;	The "view-buffer" is now different from the original buffer.
;;;	So the name of the original buffer isn't change anymore. 
;;; Changed: 02.04.1995 by Heiko Münkel
;;;	Integrated the changes from the XEmacs distribution.
;;; Changed: 20.07.1995 by Heiko Münkel
;;;	Fixed a bug in html-view-goto-url.
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Commentary: 
;;; To use, just set the value of html-view-mosaic-command to whatever you
;;; use to run NCSA Mosaic. You may have to set html-view-tmp-file.
;;; Type M-x html-view-start-mosaic <ret>. 
;;; Afterwards, view files/buffers with html-view-view-file/
;;; html-view-view-buffer. There's also a command, of dubious utility,
;;; for jumping to particular documents: html-view-goto-url
;;;
;;; If you have any questions or comments mail to tapia@hydra.unm.edu.


(require 'hm--html)

(defvar html-view-mosaic-process nil "The NCSA Mosaic Process")

(defvar html-view-mosaic-command "mosaic"  
  "The command that runs Mosaic on your system")

(defvar html-view-mosaic-tmp-file-prefix "/tmp/Mosaic."
  "Prefix for the temp files, which are used by Mosaic.
For old versions this must be \"/tmp/xmosaic.\".
For new versions it is \"/tmp/Mosaic.\".")

(defvar html-view-tmp-file (concat "/tmp/mosaic.html-" 
				   (user-login-name)
				   (emacs-pid))
  "File where buffers are saved for viewing by Mosaic")

(defvar html-view-display nil "The display that Mosaic is using.")

(defvar html-view-wait-counter 100000
  "*Counter for a wait loop.
The wait loop is beween the start of the Mosaic and the command 
`set-process-sentinel'. If Mosaic don't start, then you must set
this value higher. You can try to set it to a lower number otherwise.")

;;;###autoload
(defun html-view-start-mosaic ()
  "Start Mosaic."
  (interactive)
  (or (stringp html-view-display)
      (call-interactively 'html-view-get-display))
  (or (and (processp html-view-mosaic-process)
	   (eq (process-status html-view-mosaic-process) 'run))
      (progn (setq html-view-mosaic-process 
		   (start-process "mosaic" "mosaic" 
				  html-view-mosaic-command 
				  "-display" html-view-display))
	     (let ((i html-view-wait-counter))
	       (while (> i 0)
		 (setq i (1- i))))
	     (set-process-sentinel html-view-mosaic-process
				   'html-view-mosaic-process-sentinel))))
 
;;;###autoload
(defun html-view-view-file (filename)
  "View an html file with Mosaic."
  (interactive "fFile to view: ")
  (or (and (processp html-view-mosaic-process)
	   (eq (process-status html-view-mosaic-process) 'run))
      (html-view-start-mosaic))
  (if (and (processp html-view-mosaic-process)
	   (eq (process-status html-view-mosaic-process) 'run))
      (progn
	(let ((buffer (process-buffer html-view-mosaic-process))
	      (id (process-id html-view-mosaic-process))
	      (file nil))
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (setq file (format "%s%s" html-view-mosaic-tmp-file-prefix id))
	    (set-visited-file-name file)
	    ;;	  (set-visited-file-name (concat "/tmp/Mosaic."
	    ;;					 (number-to-string id)))
	    (insert-before-markers "goto\n")
	    (insert-before-markers (concat
				    "file://"
				    (expand-file-name filename)))
	    (save-buffer)
	    (signal-process id html-sigusr1-signal-value))))
    (message "Can't start mosaic process.")))
	    
;;;###autoload
(defun html-view-view-buffer (&optional buffer-to-view)
  "View html buffer with Mosaic.
If BUFFER-TO-VIEW is nil, then the current buffer is used."
  (interactive)
  (or (bufferp buffer-to-view)
      (setq buffer-to-view (current-buffer)))
  (save-excursion
    (find-file html-view-tmp-file)
    (insert-buffer buffer-to-view)
    (write-file html-view-tmp-file)
    (html-view-view-file html-view-tmp-file)))
 
;;;###autoload
(defun html-view-goto-url (url)
  "Goto an URL in Mosaic."
  (interactive "sURL: ")
  (or (processp html-view-mosaic-process)
  (html-view-start-mosaic))
  (if (processp html-view-mosaic-process)
  (progn
    (let ((buffer (process-buffer html-view-mosaic-process))
	  (id (process-id html-view-mosaic-process))
	  (file nil))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
;;	(setq file (format "%s%s" "/tmp/xmosaic." id))
	(setq file (format "%s%s" html-view-mosaic-tmp-file-prefix id))
	(set-visited-file-name file)
	;;	  (set-visited-file-name (concat "/tmp/Mosaic."
	;;					 (number-to-string id)))
	(insert-before-markers "goto\n")
	(insert-before-markers url)
	(save-buffer)
	(signal-process id html-sigusr1-signal-value))))
  (message "Can't start mosaic process.")))
 
;;;###autoload
(defun html-view-get-display (display)
  "Get the display for Mosaic."
  (interactive "sDisplay: ")
  (setq html-view-display display))
 
 
(defun html-view-mosaic-process-sentinel (proc, event)
  (cond ((or (string-match "exited abnormally with code" event)
	     (string-match "finished" event))
	 (message event)
	 (setq html-view-mosaic-process nil))
	(t (message event))))
	
	 
(provide 'html-view)
