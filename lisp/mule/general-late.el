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
      (loop
        ;; We want both normal and internal coding systems in order
        ;; to pick up coding system aliases.
        for coding-system in (coding-system-list 'every)
        with res = (make-hash-table :test #'equal)
        do
        (setq coding-system (symbol-name coding-system))
        (unless (or (string-match #r"\(-unix\|-mac\|-dos\)$" coding-system)
                    (string-match #r"^\(internal\|mswindows\)" coding-system))
          (puthash 
           (replace-in-string (downcase coding-system) "[^a-z0-9]" "")
           (coding-system-name (intern coding-system)) res))
        finally return res)

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
	    (remassoc "English" language-info-alist))

      ;; Make Installation-string actually reflect the environment at
      ;; byte-compile time. (We can't necessarily decode it when version.el
      ;; is loaded, since not all the coding systems are available then.)
      Installation-string (if-boundp 'Installation-file-coding-system
			      (decode-coding-string
			       Installation-string
			       Installation-file-coding-system)
			    Installation-string)

      ;; Convince the byte compiler that, really, this file can't be encoded
      ;; as binary. Ugh.
      system-type (symbol-value (intern "\u0073ystem-type"))

      unicode-query-coding-skip-chars-arg
      (eval-when-compile 
        (when-fboundp #'map-charset-chars 
          (loop
            for charset in (charset-list)
            with skip-chars-string = ""
            do
            (block no-ucs-mapping
              (map-charset-chars
               #'(lambda (begin end)
                   (loop
                     while (/= end begin)
                     do
                     (when (= -1 (char-to-unicode begin))
                       (setq this-charset-works nil)
                       (return-from no-ucs-mapping))
                     (setq begin (int-to-char (1+ begin)))))
               charset)
              (setq skip-chars-string
                    (concat skip-chars-string
                            (charset-skip-chars-string charset))))
            finally return skip-chars-string))))

;; At this point in the dump, all the charsets have been loaded. Now, load
;; their Unicode mappings.
(if load-unicode-tables-at-dump-time
    (let ((data-directory (expand-file-name "etc" source-directory)))
      (load-unicode-tables)))

;;; general-late.el ends here
