;;; webster-www.el --- Look up a word in WWW Merriam-Webster dictionary

;; Copyright (c) 1997 by Tomasz J. Cholewo <t.cholewo@ieee.org>

;; Created: 1997/03/10 
;; Version: 1.0
;; Keywords: comm, hypermedia

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: not in FSF.

;;; Code:

(defvar webster-url "http://www.m-w.com/cgi-bin/mweb?book=Dictionary&va="
  "URL to reference for Webster's dictionary.")

;;;###autoload
(defun webster-www (arg)
"Look up a word in the Webster's dictionary at http://www.m-w.com using WWW."
  (interactive (list
		(let ((prompt (concat "Look up word in webster ("
				      (current-word) "): ")))
		     (read-string prompt))))
  (require 'url)
  (require 'w3-forms)
  (if (equal "" arg) (setq arg (current-word)))
  (funcall browse-url-browser-function
	   (concat
	     webster-url
	    (w3-form-encode-xwfu arg))))

(provide 'webster-www)

;;; webster-www.el ends here
