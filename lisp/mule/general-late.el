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

      ;; This used to be here to convince the byte-compiler to encode the
      ;; output file using escape-quoted. This is no longer necessary, but
      ;; keeping it here avoids doing the eval-when-compile clause below
      ;; twice, which is a significant improvement.
      system-type (symbol-value (intern "\u0073ystem-type")))

;; When this file is being compiled, all the charsets have been loaded, so
;; we can construct the query-skip-chars-arg string correctly. 
(set-unicode-query-skip-chars-args
 (eval-when-compile 
   (when-fboundp 'map-charset-chars 
     (loop
       for charset in (charset-list)
       with skip-chars-string = ""
       do
       (block no-ucs-mapping
         (map-charset-chars
          #'(lambda (begin end)
              (loop
                while (and begin (>= end begin))
                do
                (when (= -1 (char-to-unicode begin))
                  (return-from no-ucs-mapping))
                (setq begin (int-to-char (1+ begin)))))
          charset)
         (setq skip-chars-string
               (concat skip-chars-string
                       (charset-skip-chars-string charset))))
       finally return skip-chars-string)))
 unicode-invalid-sequence-regexp-range
 (eval-when-compile
   (concat (loop
             for i from #x80 to #xFF
             collect (aref (decode-coding-string (int-char i)
                                                 'utf-8) 0)))))

;; At this point in the dump, all the charsets have been loaded.
;; Now, set the precedence list. @@#### There should be a better way.
(initialize-default-unicode-precedence-list)

;;; general-late.el ends here
