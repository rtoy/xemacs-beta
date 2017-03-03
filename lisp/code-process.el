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

  ;; We can't delete the region before feeding it to `call-process', so we
  ;; take care not to delete the insertion when we delete the region.  START
  ;; and END may not be markers; copy them.  (point) will end up after the
  ;; insertion.  A copy of (point) tracks the beginning of the insertion.

  (let ((s (and deletep (copy-marker start)))         ;  Only YOU can
	(e (and deletep (copy-marker end t)))         ;     prevent
	(p (and deletep (copy-marker (point))))       ; excess consing!
	(retval
	 (apply #'call-process program (list (current-buffer) start end)
		buffer displayp args)))
    (when deletep
      (if (<= s p e)
	  ;; region was split by insertion
	  ;; the order checks are gilt lilies
	  (progn (when (< (point) e) (delete-region (point) e))
		 (when (< s p) (delete-region s p)))
	;; insertion was outside of region
	(delete-region s e)))
    retval))

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

(defun network-stream-get-response (stream start end-of-command)
  (when end-of-command
    (with-current-buffer (process-buffer stream)
      (save-excursion
	(goto-char start)
	(while (and (memq (process-status stream) '(open run))
		    (not (re-search-forward end-of-command nil t)))
	  (accept-process-output stream 0 50)
	  (goto-char start))
	;; Return the data we got back, or nil if the process died.
	(unless (= start (point))
	  (buffer-substring start (point)))))))

(defun network-stream-command (stream command eoc)
  (when command
    (let ((start (point-max (process-buffer stream))))
      (process-send-string stream command)
      (network-stream-get-response stream start eoc))))

(defun network-stream-open-plain (name buffer host service parameters)
  (let ((start (point buffer))
	(stream
	 (open-network-stream-internal name buffer host service
				       (plist-get parameters :protocol))))
    (list stream
	  (network-stream-get-response stream start
				       (plist-get parameters :end-of-command))
	  nil
	  'plain)))

(defun network-stream-open-tls (name buffer host service parameters)
  (with-current-buffer buffer
    (let* ((start (point-max))
	   (stream
	    (open-network-stream-internal name buffer host service
					  (plist-get parameters :protocol) t)))
      (if (null stream)
	  (list nil nil nil 'plain)
	(let ((eoc (plist-get parameters :end-of-command))
	      (capability-command (plist-get parameters :capability-command)))
	  (list stream
		(network-stream-get-response stream start eoc)
		(network-stream-command stream capability-command eoc)
		'tls))))))

(defun network-stream-certificate (host service parameters)
  (let ((spec (plist-get :client-certificate parameters)))
    (cond
     ((listp spec)
      ;; Either nil or a list with a key/certificate pair.
      spec)
     ((eq spec t)
      (when (fboundp 'auth-source-search)
	(let* ((auth-info
		(car (auth-source-search :max 1
					 :host host
					 :port service)))
	       (key (plist-get auth-info :key))
	       (cert (plist-get auth-info :cert)))
	  (and key cert
	       (list key cert))))))))

(defun network-stream-open-starttls (name buffer host service parameters)
  (let* ((start (point buffer))
	 (require-tls    (eq (plist-get parameters :type) 'starttls))
	 (starttls-function  (plist-get parameters :starttls-function))
	 (success-string     (plist-get parameters :success))
	 (capability-command (plist-get parameters :capability-command))
	 (eoc                (plist-get parameters :end-of-command))
	 (eo-capa        (or (plist-get parameters :end-of-capability) eoc))
	 (protocol           (plist-get parameters :protocol))
	 ;; Return (STREAM GREETING CAPABILITIES RESULTING-TYPE)
	 (stream (open-network-stream-internal name buffer host service
					       protocol))
	 (greeting (and (not (plist-get parameters :nogreeting))
			(network-stream-get-response stream start eoc)))
	 (capabilities (network-stream-command stream capability-command
					       eo-capa))
	 (resulting-type 'plain)
	 starttls-available starttls-command error)

    ;; First check whether the server supports STARTTLS at all.
    (when (and capabilities success-string starttls-function)
      (setq starttls-command
	    (funcall starttls-function capabilities)))
    ;; If we have built-in STARTTLS support, try to upgrade the
    ;; connection.
    (when (and starttls-command
	       (setq starttls-available t)
	       (not (eq (plist-get parameters :type) 'plain)))
      (when (let ((response
		   (network-stream-command stream starttls-command eoc)))
	      (and response (string-match success-string response)))
	;; The server said it was OK to begin STARTTLS negotiations.
	(let ((cert (network-stream-certificate host service parameters)))
	  (condition-case nil
	      (tls-negotiate stream host (and cert (list cert)))
	    ;; If we get a tls-specific error (for instance if the
	    ;; certificate the server gives us is completely syntactically
	    ;; invalid), then close the connection and possibly (further
	    ;; down) try to create a non-encrypted connection.
	    (gnutls-error (delete-process stream))))
	(if (memq (process-status stream) '(open run))
	    (setq resulting-type 'tls)
	  ;; We didn't successfully negotiate STARTTLS; if TLS
	  ;; isn't demanded, reopen an unencrypted connection.
	  (unless require-tls
	    (setq stream
		  (make-network-process :name name :buffer buffer
					:host host :service service))
	    (network-stream-get-response stream start eoc)))
	;; Re-get the capabilities, which may have now changed.
	(setq capabilities
	      (network-stream-command stream capability-command eo-capa))))

    ;; If TLS is mandatory, close the connection if it's unencrypted.
    (when (and require-tls
	       ;; ... but Emacs wasn't able to -- either no built-in
	       ;; support, or no gnutls-cli installed.
	       (eq resulting-type 'plain))
      (setq error
	    (if (or (null starttls-command)
		    starttls-available)
		"Server does not support TLS"
	      ;; See `starttls-available-p'.  If this predicate
	      ;; changes to allow running under Windows, the error
	      ;; message below should be amended.
	      (if (memq system-type '(windows-nt ms-dos))
		  (concat "Emacs does not support TLS")
		(concat "Emacs does not support TLS, and no external `"
			(if starttls-use-gnutls
			    starttls-gnutls-program
			  starttls-program)
			"' program was found"))))
      (delete-process stream)
      (setq stream nil))
    ;; Return value:
    (list stream greeting capabilities resulting-type error)))

;; Requires that format-spec.el from gnus be loaded
(defun network-stream-open-shell (name buffer host service parameters)
  (require 'format-spec)
  (let* ((capability-command (plist-get parameters :capability-command))
	 (eo-capa            (plist-get parameters :end-of-capability))
	 (eoc                (plist-get parameters :end-of-command))
	 (start (point buffer))
	 (stream (let ((process-connection-type nil))
		   (start-process name buffer shell-file-name
				  shell-command-switch
				  (format-spec
				   (plist-get parameters :shell-command)
				   (format-spec-make
				    ?s host
				    ?p service))))))
    (list stream
	  (network-stream-get-response stream start eoc)
	  (network-stream-command stream capability-command (or eo-capa eoc))
	  'plain)))

(defun open-network-stream (name buffer host service &rest parameters)
  "Open a TCP connection for a service to a host.
Normally, return a process object to represent the connection.  If the
:return-list parameter is non-NIL, instead return a list; see below.
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

The remaining PARAMETERS should be a sequence of keywords and values:
- :protocol is a network protocol.  Currently 'tcp (Transmission Control
   Protocol) and 'udp (User Datagram Protocol) are supported.  When
   omitted, 'tcp is assumed.
- :type specifies the connection type; it is one of the following:
  nil or `network': begin with an ordinary network connection, and if
              the parameters :success and :capability-command are also
              supplied, try to upgrade to an encrypted connection via
              STARTTLS.  If that fails (e.g., HOST does not support TLS),
              retain an unencrypted connection.
  `plain': an ordinary, unencrypted network connection.
  `starttls': begin with an ordinary network connection and try to
              upgrade via STARTTLS.  If that fails, drop the connection
              and return a killed process object.
  `tls': a TLS connection.
  `ssl': a synonym for `tls'.
  `shell': a shell connection.
- :return-list specifies this function's return value.
  If omitted or nil, return a process object as usual.  Otherwise, return
  (PROC . PROPS), where PROC is a process object and PROPS is a plist of
  connection properties, with these keywords:
  :greeting: the greeting returned by HOST (a string), or nil.
  :capabilities: a string representing HOST's capabilities, or nil if none
              could be found.
  :type: the resulting connection type, `plain' (unencrypted) or `tls'
              (encrypted).
- :end-of-command specifies a regexp matching the end of a command.
- :end-of-capability specifies a regexp matching the end of the response
  to the command specified for :capability-command.  It defaults to the
  regexp specified for :end-of-command.
- :success specifies a regexp matching a message indicating a successful
  STARTTLS negotiation.  For example, the default should be \"^3\" for an
  NNTP connection.
- :capability-command specifies a command used to query HOST for its
  capabilities.  For example, this should be \"1 CAPABILITY\\r\\n\" for
  IMAP.
- :starttls-function specifies a function for handling STARTTLS.  This
  function should take one parameter, the response to the capability
  command, and should return the command to switch on STARTTLS if the
  server supports it, or nil otherwise.
- :always-query-capabilities, if non-nil, indicates that the server should
  be queried for capabilities even if constructing a `plain' network
  connection.
- :client-certificate is either a list (certificate-key-filename
  certificate-filename), or `t', meaning that `auth-source' will be
  queried for the key and certificate.  This parameter is used only when
  constructing a TLS or STARTTLS connection.
- :use-starttls-if-possible, if non-nil, indicates that STARTTLS should
  be used even if TLS support is not compiled in to XEmacs.
- :nogreeting, if non-nil, indicates that we should not wait for a
  greeting from the server.
- :nowait, if non-nil, indicates that an asynchronous connection should be
  made, if possible.  NOTE: this is currently unimplemented.

For backwards compatibility, if exactly five arguments are given, the fifth
must be one of nil, 'tcp, or 'udp.  Both nil and 'tcp select TCP (Transmission
Control Protocol) and 'udp selects UDP (User Datagram Protocol).

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
  (when (and (car parameters) (not (cdr parameters)))
    (setq parameters (list :protocol (car parameters))))
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
	       'raw-text))
	  (type (plist-get parameters :type))
	  (return-list (plist-get parameters :return-list))
	  (capability-command (plist-get parameters :capability-command)))
      (if (and (not return-list)
	       (or (eq type 'plain)
		   (and (or (null type) (eq type 'network))
			(not (and (plist-get parameters :success)
				  capability-command)))))
	  ;; The simplest case: a plain connection
	  (open-network-stream-internal name buffer host service
					(plist-get parameters :protocol))
	(let ((work-buffer (or buffer
			       (generate-new-buffer " *stream buffer*")))
	      (fun (cond ((and (eq type 'plain)
			       (not (plist-get parameters
					       :always-query-capabilities)))
			  #'network-stream-open-plain)
			 ((memq type '(nil network starttls plain))
			  #'network-stream-open-starttls)
			 ((memq type '(tls ssl)) #'network-stream-open-tls)
			 ((eq type 'shell) 'network-stream-open-shell)
			 (t (error "Invalid connection type" type))))
	      result)
	  (unwind-protect
	      (setq result
		    (funcall fun name work-buffer host service parameters))
	    (unless buffer
	      (and (processp (car result))
		   (set-process-buffer (car result) nil))
	      (kill-buffer work-buffer)))
	  (if return-list
	      (list (car result)
		    :greeting     (nth 1 result)
		    :capabilities (nth 2 result)
		    :type         (nth 3 result)
		    :error        (nth 4 result))
	    (car result)))))))

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
