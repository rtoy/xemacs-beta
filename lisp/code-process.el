;;; code-process.el --- Process coding functions for XEmacs.

;; Copyright (C) 1985-1987, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 2000, 2002 Ben Wing
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: Ben Wing
;;         MORIOKA Tomohiko
;; Maintainer: XEmacs Development Team
;; Keywords: mule, multilingual, coding system, process

;; This file is part of XEmacs.

;; This file is very similar to code-process.el

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

;;; Code:

(defvar process-coding-system-alist nil
  "Alist to decide a coding system to use for a process I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a program name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the program and encoding what sent to the program.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, it is called with two arguments, a symbol
indicating the operation being performed (one of `start-process',
`call-process', `open-network-stream', or `open-multicast-group') and the
program name.  The function must return a coding system or a cons of
coding systems which are used as above.")

(defun call-process (program &optional infile buffer displayp &rest args)
  "Call PROGRAM synchronously in separate process.

The program's input comes from file INFILE (nil means `/dev/null').
XEmacs feature: INFILE can also be a list of (BUFFER [START [END]]), i.e.
a list of one to three elements, consisting of a buffer and optionally
a start position or start and end position.  In this case, input comes
from the buffer, starting from START (defaults to the beginning of the
buffer) and ending at END (defaults to the end of the buffer).

Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
If BUFFER is a string, then find or create a buffer with that name,
then insert the output in that buffer, before point.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), a file name string, or (XEmacs feature)
a buffer object.  If STDERR-FILE is a buffer object (but not the name of
a buffer, since that would be interpreted as a file), the standard error
output will be inserted into the buffer before point.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If BUFFER is 0, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate and returns a numeric exit status
or a signal description string.  If you quit, the process is first killed
with SIGINT, then with SIGKILL if you quit again before the process exits.

If INFILE is a file, we transfer its exact contents to the process without
any encoding/decoding. (#### This policy might change.)

Otherwise, the read/write coding systems used for process I/O on the
process are determined as follows:

1. `coding-system-for-read', `coding-system-for-write', if non-nil.
      (Intended as a temporary overriding mechanism for use by Lisp
      code.)
2. The matching value for the process name from `process-coding-system-alist',
      if any, and if non-nil.  The value may be either a single coding
      system, used for both read and write; or a cons of read/write; or a
      function, called to get one of the other two values.
3. For writing: If a buffer was given in INFILE, the value of
      `buffer-file-coding-system' in that buffer.
   For reading: if a buffer was given in BUFFER, the value of
      `buffer-file-coding-system-for-read' in that buffer.
4. The value of `default-process-coding-system', which should be a cons
      of read/write coding systems, if the values are non-nil.
5. The coding system `undecided' for read, and `raw-text' for write.

Note that the processes of determining the read and write coding systems
proceed essentially independently one from the other, as in `start-process'."
  (let (cs-r cs-w)
    (let (ret)
      (catch 'found
	(let ((alist process-coding-system-alist)
	      (case-fold-search nil))
	  (while alist
	    (if (string-match (car (car alist)) program)
		(throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'call-process program)))
      (cond ((consp ret)
	     (setq cs-r (car ret)
		   cs-w (cdr ret)))
	    ((and ret (find-coding-system ret))
	     (setq cs-r ret
		   cs-w ret))))
    (let ((coding-system-for-read
	   (or coding-system-for-read cs-r
	       (let ((thebuf (if (consp buffer) (car buffer) buffer)))
		 (and (or (bufferp thebuf) (stringp thebuf))
		      (get-buffer thebuf)
		      (symbol-value-in-buffer
		       'buffer-file-coding-system-for-read (get-buffer thebuf))))
	       (car default-process-coding-system)
	       'undecided))
	  (coding-system-for-write
	   (or coding-system-for-write cs-w
	       (and (consp infile)
		    (symbol-value-in-buffer
		     'buffer-file-coding-system
		     (get-buffer (car infile))))
	       (cdr default-process-coding-system)
	       'raw-text)))
      (apply 'call-process-internal program infile buffer displayp args))))

(defun call-process-region (start end program
				  &optional deletep buffer displayp
				  &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.
Delete the text if fourth arg DELETEP is non-nil.

Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
If BUFFER is a string, then find or create a buffer with that name,
then insert the output in that buffer, before point.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), a file name string, or (XEmacs feature)
a buffer object.  If STDERR-FILE is a buffer object (but not the name of
a buffer, since that would be interpreted as a file), the standard error
output will be inserted into the buffer before point.

Sixth arg DISPLAYP non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.

If BUFFER is 0, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate and returns a numeric exit status
or a signal description string.  If you quit, the process is first killed
with SIGINT, then with SIGKILL if you quit again before the process exits.

The read/write coding systems used for process I/O on the process are
the same as for `call-process'."
  ;; We used to delete the text before calling call-process; that was when
  ;; a temporary file was used to pass the text to call-process.  Now that
  ;; we don't do that, we delete the text afterward; if it's being inserted
  ;; in the same buffer, make sure we track the insertion, and don't get
  ;; any of it in the deleted region.  We keep marker s before the
  ;; insertion and e afterward.  Finally we delete the regions before
  ;; and after the insertion.
  (let ((s (and deletep (copy-marker (point))))
	(e (and deletep (copy-marker (point) t))))
    (let ((retval
	   (apply #'call-process program (list (current-buffer) start end)
		  buffer displayp args)))
      ;; If start and end were the same originally, e will be beyond s now
      (when (and deletep (> e s))
	;; APA: Is it always correct to honor narrowing, which affects
	;; (point-min) and (point-max)?
	;; Delete region before insertion.
	(delete-region (point-min) s)
	;; Delete region after insertion.
	(delete-region e (point-max)))
      retval)))

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
BUFFER can also have the form (REAL-BUFFER STDERR-BUFFER); in that case,
 REAL-BUFFER says what to do with standard output, as above,
 while STDERR-BUFFER says what to do with standard error in the child.
 STDERR-BUFFER may be nil (discard standard error output, unless a stderr
 filter is set).  Note that if you do not use this form at process creation,
 stdout and stderr will be mixed in the output buffer, and this cannot be
 changed, even by setting a stderr filter.
Third arg is program file name.  It is searched for as in the shell.
Remaining arguments are strings to give program as arguments.

The read/write coding systems used for process I/O on the process are
determined as follows:

1. `coding-system-for-read', `coding-system-for-write', if non-nil.
      (Intended as a temporary overriding mechanism for use by Lisp
      code.)
2. The matching value for the process name from `process-coding-system-alist',
      if any, and if non-nil.  The value may be either a single coding
      system, used for both read and write; or a cons of read/write; or a
      function, called to get one of the other two values.
3. The value of `default-process-coding-system', which should be a cons
      of read/write coding systems, if the values are non-nil.
4. The coding system `undecided' for read, and `raw-text' for write.

Note that the processes of determining the read and write coding systems
proceed essentially independently one from the other.  For example, a value
determined from `process-coding-system-alist' might specify a read coding
system but not a write coding system, in which the read coding system is as
specified and the write coding system comes from proceeding to step 3 (and
looking in `default-process-coding-system').

You can change the coding systems later on using
`set-process-coding-system', `set-process-input-coding-system', or
`set-process-output-coding-system'.

See also `set-process-filter' and `set-process-stderr-filter'."
  (let (cs-r cs-w)
    (let (ret)
      (catch 'found
	(let ((alist process-coding-system-alist)
	      (case-fold-search nil))
	  (while alist
	    (if (string-match (car (car alist)) program)
		(throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'start-process program)))
      (cond ((consp ret)
	     (setq cs-r (car ret)
		   cs-w (cdr ret)))
	    ((and ret (find-coding-system ret))
	     (setq cs-r ret
		   cs-w ret))))
    (let ((coding-system-for-read
	   (or coding-system-for-read cs-r
	       (car default-process-coding-system) 'undecided))
	  (coding-system-for-write
	   (or coding-system-for-write cs-w
	       (cdr default-process-coding-system) 'raw-text)))
      (apply 'start-process-internal name buffer program program-args)
      )))

(defvar network-coding-system-alist nil
  "Alist to decide a coding system to use for a network I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a network service name
or is a port number to connect to,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the network stream and encoding what sent to the network stream.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.")

(defun open-network-stream (name buffer host service &optional protocol)
  "Open a TCP connection for a service to a host.
Return a process object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to.
Fifth argument PROTOCOL is a network protocol.  Currently 'tcp
 (Transmission Control Protocol) and 'udp (User Datagram Protocol) are
 supported.  When omitted, 'tcp is assumed.

Output via `process-send-string' and input via buffer or filter (see
`set-process-filter') are stream-oriented.  That means UDP datagrams are
not guaranteed to be sent and received in discrete packets. (But small
datagrams around 500 bytes that are not truncated by `process-send-string'
are usually fine.)  Note further that UDP protocol does not guard against 
lost packets.

The read/write coding systems used for process I/O on the process are
determined as follows:

1. `coding-system-for-read', `coding-system-for-write', if non-nil.
      (Intended as a temporary overriding mechanism for use by Lisp
      code.)
2. The matching value for the service from `network-coding-system-alist',
      if any, and if non-nil.  The value may be either a single coding
      system, used for both read and write; or a cons of read/write; or a
      function, called to get one of the other two values.
3. The value of `default-network-coding-system', which should be a cons
      of read/write coding systems, if the values are non-nil.
4. The coding system `undecided' for read, and `raw-text' for write.

Note that the processes of determining the read and write coding systems
proceed essentially independently one from the other, as in `start-process'.

You can change the coding systems later on using
`set-process-coding-system', `set-process-input-coding-system', or
`set-process-output-coding-system'."
  (let (cs-r cs-w)
    (let (ret)
      (catch 'found
	(let ((alist network-coding-system-alist)
	      (case-fold-search nil)
	      pattern)
	  (while alist
	    (setq pattern (car (car alist)))
	    (and
	     (cond ((numberp pattern)
		    (and (numberp service)
			 (eq pattern service)))
		   ((stringp pattern)
		    (or (and (stringp service)
			     (string-match pattern service))
			(and (numberp service)
			     (string-match pattern
					   (number-to-string service))))))
	     (throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'open-network-stream service)))
      (cond ((consp ret)
	     (setq cs-r (car ret)
		   cs-w (cdr ret)))
	    ((and ret (find-coding-system ret))
	     (setq cs-r ret
		   cs-w ret))))
    (let ((coding-system-for-read
	   (or coding-system-for-read cs-r
	       (car default-network-coding-system)
	       'undecided))
	  (coding-system-for-write
	   (or coding-system-for-write cs-w
	       (cdr default-network-coding-system)
	       'raw-text)))
      (open-network-stream-internal name buffer host service protocol))))

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems for the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is the coding system to be used to encode output to the process.

For a list of possible values of CODING-SYSTEM, use \\[coding-system-list]."
  (interactive
   "zCoding-system for process input: \nzCoding-system for process output: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "no process")
      (get-coding-system decoding)
      (get-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

;;; code-process.el ends here
