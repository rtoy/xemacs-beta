;;; url-ns.el --- Various netscape-ish functions for proxy definitions
;; Author: wmperry
;; Created: 1997/07/14 05:11:46
;; Version: 1.4
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997 Free Software Foundation, Inc.
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

(require 'url-gw)

(defun isPlainHostName (host)
  (not (string-match "\\." host)))

(defun dnsDomainIs (host dom)
  (string-match (concat (regexp-quote dom) "$") host))

(defun dnsResolve (host)
  (url-gateway-nslookup-host host))

(defun isResolvable (host)
  (if (string-match "^[0-9.]+$" host)
      t
    (not (string= host (url-gateway-nslookup-host host)))))

(defun isInNet (ip net mask)
  (let ((netc (split-string ip "\\."))
	(ipc  (split-string net "\\."))
	(maskc (split-string mask "\\.")))
    (if (or (/= (length netc) (length ipc))
	    (/= (length ipc) (length maskc)))
	nil
      (setq netc (mapcar 'string-to-int netc)
	    ipc (mapcar 'string-to-int ipc)
	    maskc (mapcar 'string-to-int maskc))
      (and
       (= (logand (nth 0 netc) (nth 0 maskc))
	  (logand (nth 0 ipc)  (nth 0 maskc)))
       (= (logand (nth 1 netc) (nth 1 maskc))
	  (logand (nth 1 ipc)  (nth 1 maskc)))
       (= (logand (nth 2 netc) (nth 2 maskc))
	  (logand (nth 2 ipc)  (nth 2 maskc)))
       (= (logand (nth 3 netc) (nth 3 maskc))
	  (logand (nth 3 ipc)  (nth 3 maskc)))))))

(provide 'url-ns)
