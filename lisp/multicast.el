;;; multicast.el --- lisp frontend for multicast connections in XEmacs

;; Copyright (C) 1997-2000 Didier Verna.
;; Copyright (C) 2002 Ben Wing.

;; Author:          Didier Verna <didier@xemacs.org>
;; Maintainer:      Didier Verna <didier@xemacs.org>
;; Created:         Thu Dec  4 16:37:39 1997
;; Last Revision:   Mon Jan 19 19:10:50 1998
;; Current Version: 0.4
;; Keywords:        dumped comm processes

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; This file just contains a lisp frontend to the internal function
;; open-multicast-group-internal written in C and belonging to process.c
;; Well, nothing much to say about it ... read the doc string.


;;; Change Log:

;; Rev. of Mon Jan 19 19:04:44 1998 : packaging cleanup
;; Rev. of Thu Dec 11 13:54:26 1997 : updated the docstring
;; Rev. of Mon Dec  8 15:28:47 1997 : Improved the doc string
;; Rev. of Thu Dec  4 16:38:09 1997 : Initial Version.


;;; Code:

(defun open-multicast-group (name buffer address)
  "Open a multicast connection on the specified address.
Returns a process object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
NAME is a name for the process. It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at the end of that buffer, unless you specify an output
 stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated with any
 buffer.
ADDRESS specifies a standard multicast address \"dest/port/ttl\":
 dest is an internet address between 224.0.0.0 and 239.255.255.255
 port is a communication port like in traditional unicast
 ttl is the time-to-live (15 for site, 63 for region and 127 for world).

WARNING: it is *strongly* recommended to avoid using groups beginning with
         224 or 239. Such groups are considered 'admin' groups, and may
         behave in a surprising way ...

The read/write coding systems used for process I/O on the process are
determined as follows:

1. `coding-system-for-read', `coding-system-for-write', if non-nil.
      (Intended as a temporary overriding mechanism for use by Lisp
      code.)
2. The matching value for the port from `network-coding-system-alist',
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
  (let (dest port ttl)
    ;; We check only the general form of the multicast address.
    ;; The rest will be handled by the internal function.
    (string-match "^\\([0-9\\.]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" address)
    (and (not (and (= (match-beginning 0) 0)
		   (= (match-end 0) (length address))))
	 (error "malformed multicast address: %s" address))
    (and (not (setq dest (match-string 1 address)))
	 (error "invalid destination specification."))
    (and (= 0 (setq port (string-to-int (match-string 2 address))))
	 (error "invalid port specification."))
    (and (= 0 (setq ttl (string-to-int (match-string 3 address))))
	 (error "invalid ttl specification."))
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
		    (and (numberp port)
			 (eq pattern port)))
		   ((stringp pattern)
		    (or (and (stringp port)
			     (string-match pattern port))
			(and (numberp port)
			     (string-match pattern
					   (number-to-string port))))))
	     (throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'open-multicast-group port)))
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
      (declare-fboundp (open-multicast-group-internal name buffer dest port
						      ttl))))))

;;; multicast.el ends here
