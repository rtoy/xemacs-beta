;;; mule-process.el --- Process functions for XEmacs/Mule.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Commentary:

;;; split off of mule.el.

;;; Code:


(defun set-current-process-coding-system (input output)
  (interactive
   "zCoding-system for process input: \nzCoding-system for process output: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "no process")
      (set-process-coding-system proc input output)))
  (redraw-modeline t))

(defun code-convert-process-arguments (arguments coding-systems)
  "Convert the code of ARGUMENTS passed to the process using
input coding-system of CODINGS-SYSTEMS.  If you never wants to convert
code of arguments, define this function just to return ARGUMENTS."
  (mapcar (function (lambda (arg)
		      (or (code-convert-string arg 'internal
					       (cdr coding-systems))
			  arg)))
	  arguments))

(defvar call-process-hook nil
  "A hook function to decide coding-systems for calling programs.
Before calling programs, call-process and call-process-region call
 this function with arguments PROGRAM, BUFFER, START, END and ARGS,
 where START and END are nil when called from call-process.
The return value of this function should be a cons of coding-systems
 for input and output of the program.  The input coding-system
 is also used for converting ARGS.
 If the value is not cons object, further calling is suppressed.")

(defun call-process (program &optional infile buffer display &rest args)
  "Call PROGRAM synchronously in separate process.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.
If BUFFER is 0, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you
quit again.
The coding-system used for converting ARGS and receiving the output
 of PROGRAM default to car and cdr of default-process-coding-system,
 but can be changed by `call-process-hook'.
See also `call-process-hook' and `call-process-internal'."
  (let ((coding-systems
	 (if call-process-hook
	     (apply call-process-hook program buffer nil nil args)
	   default-process-coding-system)))
    (if (consp coding-systems)
	(apply 'call-process-internal
	       program infile buffer display (car coding-systems)
	       (code-convert-process-arguments args coding-systems)))))

(defun call-process-region (start end program
				  &optional delete buffer display &rest args)
  "Send text from START to END to a process running PROGRAM.
Delete the text if DELETE is non-nil.
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.
Returns nil if BUFFER is 0; otherwise waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGKILL.
The coding-system used for receiving from the PROGRAM defaults to
 car of default-process-coding-system.
The coding-system used for sending the region to the PROGRAM and converting
 ARGS default to cdr of default-process-coding-system.
But these can be changed by `call-process-hook'.
See also `call-process-hook' and `call-process'."
  (let ((temp (if (eq system-type 'ms-dos)
		  (let* ((tem (or (getenv "TMP") (getenv "TEMP") "/"))
			 (temm (aref tem (1- (length tem)))))
		    (make-temp-name
		     (concat tem
			     (if (or (eq temm ?/) (eq temm ?\\)) "" "/")
			     "em")))
		(make-temp-name "/tmp/emacs")))
	(coding-systems (if call-process-hook
			    (apply call-process-hook
				   program buffer start end args)
			  default-process-coding-system))
	status)
    (if (consp coding-systems)
	(unwind-protect
	    (let ((call-process-hook nil)
		  (default-process-coding-system coding-systems)
		  (output-coding-system (cdr coding-systems)))
	      (write-region start end temp nil 'nomessage)
	      (if delete (delete-region start end))
	      (setq status
		    (apply 'call-process program temp buffer display args)))
	  (delete-file temp)))
    status))

(defvar start-process-hook nil
  "A hook function to decide coding-systems of process input and output.
Before starting process, start-process calls it with arguments
 NAME, BUFFER, PROGRAM, and ARGS [same as those given to start-process].
The return value of this function should be a cons of coding-systems
 used while sending and receiving to/from the started process.
 If the value is not cons object, further calling is supressed.")

(if (not (eq system-type 'ms-dos))

(defun start-process (name buf program &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER PROGRAM &rest PROGRAM-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
Third arg is program file name.  It is searched for as in the shell.
Remaining arguments are strings to give program as arguments.
The coding-system used for sending and receiving to/from the process are
 the value of default-process-coding-system, but can be changed by
 `start-process-hook'.
See also `start-process-hook' and `start-process-internal;."
  (let ((coding-systems
	 (if start-process-hook
	     (apply start-process-hook name buf program args)
	   default-process-coding-system)))
    (if (consp coding-systems)
	(let ((process
	       (apply 'start-process-internal name buf program
		      (code-convert-process-arguments args coding-systems))))
	  (set-process-input-coding-system (car coding-systems))
	  (set-process-output-coding-system (cdr coding-systems))
	  process))))

(defvar open-network-stream-hook nil
  "A hook function to decide coding-systems of input and output for service.
Before starting service, open-network-stream calls this function with arguments
 NAME, BUFFER, PROGRAM, and ARGS [same as those given to open-network-stream].
The return value of this function should be a cons of coding-systems
 used while sending and receiving to/from the network service.
 If the value is not cons object, further calling is supressed.")

(defun open-network-stream (name buf host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to.
The coding system used for sending and receiving to/from the SERVICE are
 the value of default-process-coding-system, but can be changed by
 open-network-stream-hook.
See also `open-network-stream-hook' and `open-network-stream-internal'."
  (let ((coding-systems
	 (if open-network-stream-hook
	     (funcall open-network-stream-hook name buf host service)
	   default-process-coding-system)))
    (if (consp coding-systems)
	(let ((process
	       (open-network-stream-internal
		name buf host service)))
	  (set-process-input-coding-system
	   (car coding-systems))
	  (set-process-output-coding-system
	   (cdr coding-systems))
	  process))))

)

;;;
;;; For process-coding-system
;;;

(defun default-start-process-hook (name buf program &rest args)
  (apply 'find-process-coding-system buf program nil args))

(defun default-open-network-stream-hook (name buf host service)
  (find-process-coding-system buf service t host))

(defun default-call-process-hook (program buffer start end &rest args)
  (apply 'find-process-coding-system buffer program nil args))

(defun find-process-coding-system (buffer program
					  &optional servicep &rest args)
 "Arguments are BUFFER, PROGRAM, SERVICEP, and ARGS.
BUFFER is output buffer (or its name) of a process or nil.
If SERVICEP is nil, PROGRAM is a path name of a program to be executed
 by start-process and ARGS is a list of the arguments.
If SERVICEP is non-nil, PROGRAM is a name of a service
 for open-network-stream and ARGS is a list of a host.
The return value is a cons of coding-systems
 for input and output for the process.
Please redefine this function as you wish."
 (if (eq buffer t) (setq buffer (buffer-name))
   (if (bufferp buffer) (setq buffer (buffer-name buffer))
     (if (not (stringp buffer)) (setq buffer ""))))

  (let ((place (if servicep
		   (find-service-coding-system program (car args))
		 (find-program-coding-system buffer program))))
    (if place
	(cond( (consp (cdr place)) (cdr place))
	     ( (null (cdr place)) '(nil nil))
	     ( t (condition-case ()
		     (apply (cdr place) buffer program servicep args)
		   (error default-process-coding-system))))
      default-process-coding-system)))
	  
(setq start-process-hook 'default-start-process-hook
      open-network-stream-hook 'default-open-network-stream-hook
      call-process-hook 'default-call-process-hook)

;;;
;;;  program --> coding-system translation
;;;

(defun strict-string-match (regexp string &optional start)
  (and (eq 0 (string-match regexp string (or start 0)))
       (eq (match-end 0) (length string))))

(defvar program-coding-system-alist nil)

(defun define-program-coding-system (buffer program code)
  (let* ((key (cons buffer program))
	 (place (assoc key program-coding-system-alist)))
    (if (coding-system-p code)
	(setq code (cons code code)))
    (if place
	(setcdr place code)
      (setq place (cons key code))
      (setq program-coding-system-alist
	    (cons place program-coding-system-alist)))
    place))

(defun find-program-coding-system (buffer program)
  (let ((alist program-coding-system-alist) (place nil))
    (while (and alist (null place))
      (if (program-coding-system-match buffer program (car (car alist)))
	  (setq place (car alist)))
      (setq alist (cdr alist)))
    place))

(defun program-coding-system-match (buffer program patpair)
  (let ((bpat (car patpair)) (ppat (cdr patpair)))
    (if (and (symbolp ppat) (boundp ppat)
	     (stringp (symbol-value ppat)))
	(setq ppat (symbol-value ppat)))
    (and (or (null bpat)
	     (and (stringp bpat) (string-match bpat buffer)))
	 (or (null ppat)
	     (and (stringp ppat)
		  (or
		   (strict-string-match ppat program)
		   (strict-string-match ppat (file-name-nondirectory program))
		   ))))))
  		      
(define-program-coding-system
  nil "rsh" 'find-process-coding-system-rsh)

(defun find-process-coding-system-rsh (buffer rsh &optional servicep host
					      &rest args)
  (if (equal (car args) "-l")
      (setq args (cdr (cdr args))))
  (if (equal (car args) "-n")
      (setq args (cdr args)))
  (apply 'find-process-coding-system buffer (car args) nil (cdr args)))


;;;
;;; 
;;; 
(define-program-coding-system
  nil (concat exec-directory "env") 'find-process-coding-system-env)

;;;(defun find-mc-process-code-env (buffer env &optional servicep &rest args)
;;;  (while (string-match "[-=]" (car args))
;;;    (setq args (cdr args)))
;;;  (find-mc-process-code buffer (car args) nil (cdr args)))

;;;
;;; coded by nakagawa@titisa.is.titech.ac.jp 1989
;;; modified by tomura@etl.go.jp 
;;;
;;; env command syntax:   See etc/env.c
;;; env [ - ]
;;; ;;; GNU env only
;;;     { variable=value 
;;;      | -u     variable
;;;      | -unset variable
;;;      | -s     variable value 
;;;      | -set   variable value }*
;;;     [ - | -- ] 
;;; ;;; end of GNU env only
;;;      <program> <args>
;;;

(defun find-process-coding-system-env (buffer env &optional servicep
					      &rest args)
  (if (string= (car args) "-") (setq args (cdr args)))
  (while (or (string-match "=" (car args))
	     (string= "-s"     (car args))
	     (string= "-set"   (car args))
	     (string= "-u"     (car args))
	     (string= "-unset" (car args)))
    (cond((or (string= "-s" (car args))
	      (string= "-set" (car args)))
	  (setq args (cdr(cdr(cdr args)))))
	 ((or (string= "-u" (car args))
	      (string= "-unset" (car args)))
	  (setq args (cdr(cdr args))))
	 (t 
	  (setq args (cdr args)))))
  (if (or (string= (car args) "-")
	  (string= (car args) "--"))
      (setq args (cdr args)))
  (apply 'find-process-coding-system buffer (car args) nil (cdr args)))

;;;
;;; service --> mc code translation
;;;

(defvar service-coding-system-alist nil)

(defun define-service-coding-system (service host code)
  (let* ((key (cons service host))
	 (place (assoc key service-coding-system-alist)))
    (if (coding-system-p code)
	(setq code (cons code code)))
    (if place
	(setcdr place code)
      (setq place (cons key code)
	    service-coding-system-alist (cons place service-coding-system-alist)))
    place))
	
(defun find-service-coding-system (service host)
  (let ((alist service-coding-system-alist) (place nil))
    (while (and alist (null place))
      (if (service-coding-system-match service host (car (car alist)))
	  (setq place (car alist)))
      (setq alist (cdr alist)))
    place))

(defun service-coding-system-match (service host patpair)
  (let ((spat (car patpair)) (hpat (cdr patpair)))
    (and (or (null spat)
	     (eq service spat)
	     (and (stringp spat) (stringp service)
		  (strict-string-match spat service)))
	 (or (null hpat)
	     (strict-string-match hpat host)))))

(defvar default-process-coding-system (cons 'autodetect-unix nil)
  "Cons of default values used to receive from and send to process.")

(defun set-default-process-coding-system (input output)
  "Set default values of input and output coding-system for process to
INPUT and OUTPUT, which should be symbols referring to coding systems."
  (interactive
   "zDefault coding-system for process input: \nzDefault coding-system for process output: ")
  (setq default-process-coding-system (cons input output))
  (redraw-modeline t))

;; For RMAIL and NEWS
;; Notice!  In Korea for mail, use 'iso-2022-kr instead of 'junet.
(define-program-coding-system nil ".*mail.*" 'junet)
(define-program-coding-system nil ".*inews.*" 'junet)
;; For GNUS
(define-service-coding-system "nntp" nil 'junet-unix)
;; For MH
(define-program-coding-system nil ".*scan.*" 'junet)
(define-program-coding-system nil ".*inc.*" 'junet)
(define-program-coding-system nil ".*mhl.*" 'junet)
;; For MIME
(define-program-coding-system nil ".*anno.*" 'junet)
(define-program-coding-system nil ".*rcvstore.*" 'junet)
(setq mh-before-send-letter-hook
      '(lambda () (set-file-coding-system 'junet)))
;; For VM
(add-hook 'vm-mode-hooks
	  '(lambda ()
	     (set-file-coding-system 'junet)))
;; For Wnn and cWnn
(define-service-coding-system "wnn" nil 'no-conversion)

;; For shells -- commented out
;;(define-program-coding-system nil ".*sh.*" '(nil . nil))

;; For gnus user only
;(setq gnus-your-domain "your.domain.address"
;      gnus-your-organization "Your site name"
;      gnus-use-generic-from t)

;; For rnews user only
(setq news-inews-program "/usr/lib/news/inews")
