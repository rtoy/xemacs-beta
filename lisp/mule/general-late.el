;;; general-late.el --- General Mule code that needs to be run late when
;;                      dumping.
;; Copyright (C) 2006 Free Software Foundation

;; Author: Aidan Kehoe

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; The variable is declared in mule-cmds.el; it's initialised here, to give
;; the language-specific code a chance to create its coding systems.

(setq posix-charset-to-coding-system-hash
      (eval-when-compile
	(let ((res (make-hash-table :test 'equal)))
	  ;; We want both normal and internal coding systems in order
	  ;; to pick up coding system aliases.
	  (dolist (coding-system (coding-system-list 'both-normal-and-internal) res)
	    (setq coding-system
		  (symbol-name coding-system))
	    (unless (or (string-match #r"\(-unix\|-mac\|-dos\)$" coding-system)
			(string-match #r"^\(internal\|mswindows\)" coding-system))
	      (puthash 
	       (replace-in-string (downcase coding-system) "[^a-z0-9]" "")
	       (coding-system-name (intern coding-system)) res)))))

      ;; In a thoughtless act of cultural imperialism, move English, German
      ;; and Japanese to the front of language-info-alist to make start-up a
      ;; fraction faster for those languages.
      language-info-alist
      (cons (assoc "Japanese" language-info-alist)
	    (remassoc "Japanese" language-info-alist))
      language-info-alist 
      (cons (assoc "German" language-info-alist)
	    (remassoc "German" language-info-alist))
      language-info-alist
      (cons (assoc "English" language-info-alist)
	    (remassoc "English" language-info-alist)))

;; At this point in the dump, all the charsets have been loaded. Now, load
;; their Unicode mappings.
(if load-unicode-tables-at-dump-time
    (let ((data-directory (expand-file-name "etc" source-directory)))
      (load-unicode-tables)))

;;; general-late.el ends here