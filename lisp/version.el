;; Record version number of Emacs.
;; Copyright (C) 1985, 1991-1994 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: FSF 19.30.

;;; Code:

;; The following line is modified automatically
;; by loading inc-version.el, each time a new Emacs is dumped.
;; (defconst emacs-version "19.16" "\
;; Version numbers of this version of Emacs.")

;; (setq emacs-version (purecopy (concat emacs-version " XEmacs Lucid (beta90)")))

;(defconst emacs-major-version
;  (progn (or (string-match "^[0-9]+" emacs-version)
;	     (error "emacs-version unparsable"))
;         (string-to-int (match-string 0 emacs-version)))
;  "Major version number of this version of Emacs, as an integer.
;Warning, this variable did not exist in Emacs versions earlier than:
;  FSF Emacs:   19.23
;  XEmacs:      19.10")

;(defconst emacs-minor-version
;  (progn (or (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
;	     (error "emacs-version unparsable"))
;         (string-to-int (match-string 1 emacs-version)))
;  "Minor version number of this version of Emacs, as an integer.
;Warning, this variable did not exist in Emacs versions earlier than:
;  FSF Emacs:   19.23
;  XEmacs:      19.10")

(defconst emacs-build-time (current-time-string) "\
Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defconst xemacs-betaname nil
  "Non-nil when this is a test (beta) version of XEmacs.
Warning, this variable did not exist in XEmacs versions prior to 20.3")

(defconst xemacs-codename "Bronx"
  "Symbolic name of XEmacs build.
Warning, this variable did not exist in XEmacs versions prior to 19.16
and 20.3")

(defconst emacs-version
  (purecopy
   (format "%d.%d \"%s\"%s%s"
	   emacs-major-version
	   emacs-minor-version
	   xemacs-codename
	   " XEmacs Lucid"
	   (if xemacs-betaname
	       (concat " " xemacs-betaname)
	     "")))
  "Version numbers of this version of XEmacs.")


(defun emacs-version  (&optional here) "\
Return string describing the version of Emacs that is running.
If optional argument HERE is non-nil, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "P")
  (let ((version-string 
         (format "XEmacs %s [Lucid] (%s) of %s %s on %s"
		 (substring emacs-version 0
			    (string-match " XEmacs" emacs-version))
		 system-configuration
                 (substring emacs-build-time 0
                            (string-match " *[0-9]*:" emacs-build-time))
                 (substring emacs-build-time 
                            (string-match "[0-9]*$" emacs-build-time))
                 emacs-build-system)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;; from emacs-vers.el
(defun emacs-version>= (major &optional minor)
  "Return true if the Emacs version is >= to the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (>= emacs-major-version major)
    (or (> emacs-major-version major)
	(and (=  emacs-major-version major)
	     (>= emacs-minor-version minor))
	)
    ))

;;; We hope that this alias is easier for people to find.
(define-function 'version 'emacs-version)

;; Put the emacs version number into the `pure[]' array in a form that
;; `what(1)' can extract from the executable or a core file.  We don't
;; actually need this to be pointed to from lisp; pure objects can't
;; be GCed.
(or (memq system-type '(vax-vms windows-nt ms-dos))
    (purecopy (concat "\n@" "(#)" (emacs-version)
		      "\n@" "(#)" "Configuration: "
		      system-configuration "\n")))

;;Local variables:
;;version-control: never
;;End:

;;; version.el ends here
