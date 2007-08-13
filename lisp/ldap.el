;;; ldap.el --- LDAP support for Emacs

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Created: Jan 1998
;; Version: $Revision: 1.3 $
;; Keywords: help comm

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to 
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;    This file provides mid-level and user-level functions to access directory
;;    servers using the LDAP protocol (RFC 1777). 

;;; Installation:
;;    LDAP support must have been built into XEmacs.


;;; Code:

(eval-when '(load eval)
  (require 'ldap))

(defvar ldap-default-host nil
  "*Default LDAP server.")

(defvar ldap-host-parameters-alist nil
  "*An alist describing per host options to use for LDAP transactions
The list has the form ((HOST OPTION OPTION ...) (HOST OPTION OPTION ...))
HOST is the name of an LDAP server. OPTIONs are cons cells describing
parameters for the server.  Valid options are:
 (binddn . BINDDN)
 (passwd . PASSWD)
 (auth . AUTH)
 (base . BASE)
 (scope . SCOPE)
 (deref . DEREF)
 (timelimit . TL)
 (sizelimit . SL))
BINDDN is the distinguished name of the user to bind as (in RFC 1779 syntax).
PASSWD is the password to use for simple authentication.
AUTH is the authentication method to use, possible values are:
`simple', `krbv41' and `krbv42'.
BASE is the base for the search as described in RFC 1779.
SCOPE is one of the three symbols `subtree', `base' or `onelevel'.
DEREF is one of the symbols `never', `always', `search' or `find'.
TL is the timeout limit for the connection in seconds.
SL is the maximum number of matches to return." )


(defun ldap-search (filter &optional host attributes attrsonly)
  "Perform an LDAP search.
FILTER is the search filter in RFC1558 syntax
HOST is the LDAP host on which to perform the search
ATTRIBUTES is the specific attributes to retrieve, nil means 
retrieve all
ATTRSONLY if non nil retrieves the attributes only without 
the associated values.
Additional search parameters can be specified through 
`ldap-host-parameters-alist' which see."
  (interactive "sFilter:")
  (let (host-alist res ldap)
    (if (null host)
	(setq host ldap-default-host))
    (if (null host)
	(error "No LDAP host specified"))
    (setq host-alist
	  (assoc host ldap-host-parameters-alist))
    (message "Opening LDAP connection to %s..." host)
    (setq ldap (ldap-open host (alist-to-plist host-alist)))
    (message "Searching with LDAP on %s..." host)
    (setq res (ldap-search-internal ldap filter 
				    (cdr (assq 'base host-alist))
				    (cdr (assq 'scope host-alist))
				    attributes attrsonly))
    (ldap-close ldap)
    res))

		

(provide 'ldap)

;;; ldap.el ends here
