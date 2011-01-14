;;; Example of Sending Messages

;; Copyright (C) 1995 Sun Microsystems, Inc

;; Author: Vladimir Ivanovic <vladimir@Eng.Sun.COM>

;; This file is part of XEmacs.

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

(defun tooltalk-random-query-handler (msg pat)
  (let ((state (get-tooltalk-message-attribute msg 'state)))
    (cond
      ((eq state 'TT_HANDLED)
       (message (get-tooltalk-message-attribute msg arg_val 0)))
      ((memq state '(TT_FAILED TT_REJECTED))
       (message "Random query turns up nothing")))))

(setq random-query-message
  '(   class TT_REQUEST
       scope TT_SESSION
     address TT_PROCEDURE
	  op "random-query"
        args ((TT_INOUT "?" "string"))
    callback tooltalk-random-query-handler))

(let ((m (make-tooltalk-message random-query-message)))
      (send-tooltalk-message m))

;;; Example of Receiving Messaegs

(defun tooltalk-display-string-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (describe-tooltalk-message msg)
  (message (get-tooltalk-message-attribute msg 'arg_val 0)))

(setq display-string-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "emacs-eval"
	args ((TT_IN "filename" "string"))
    callback tooltalk-display-string-handler))

(let ((p (make-tooltalk-pattern display-string-pattern)))
  (register-tooltalk-pattern p))
 
