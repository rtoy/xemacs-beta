;;; ssl.el,v --- ssl functions for emacsen without them builtin
;; Author: wmperry
;; Created: 1997/03/09 23:02:56
;; Version: 1.8
;; Keywords: comm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995, 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ssl-program-name "ssl"
  "*The program to run in a subprocess to open an SSL connection.")

(defvar ssl-program-arguments '(host port)
  "*Arguments that should be passed to the program `ssl-program-name'.
This should be used if your SSL program needs command line switches to
specify any behaviour (certificate file locations, etc).
The special symbols 'host and 'port may be used in the list of arguments
and will be replaced with the hostname and service/port that will be connected
to.")

(defun open-ssl-stream (name buffer host service)
  "Open a SSL connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
specifying a port number to connect to."
  (if (integerp service) (setq service (int-to-string service)))
  (let* ((process-connection-type t)
	 (port service)
	 (proc (eval
		(`
		 (start-process name buffer ssl-program-name
				(,@ ssl-program-arguments))))))
    (process-kill-without-query proc)
    proc))

(provide 'ssl)
