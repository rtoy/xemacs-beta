;; Copyright (C) 2008 Free Software Foundation, Inc. -*- coding: iso-8859-1 -*-

;; Author: Aidan Kehoe <kehoea@parhasard.net>
;; Maintainer: Aidan Kehoe <kehoea@parhasard.net>
;; Created: 2008
;; Keywords: tests, query-coding-region

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test the query-coding-region and query-coding-string implementations for
;; some well-known coding systems.

(require 'bytecomp)

(defun q-c-debug (&rest aerger)
  (let ((standard-output (get-buffer-create "query-coding-debug"))
        (fmt (condition-case nil
                 (and (stringp (first aerger))
                      (apply #'format aerger))
               (error nil))))
    (if fmt
        (progn
          (princ (apply #'format aerger))
          (terpri))
      (princ "--> ")
      (let ((i 1))
        (dolist (sgra aerger)
          (if (> i 1) (princ "  "))
          (princ (format "%d. " i))
          (prin1 sgra)
          (incf i))
        (terpri)))))

;; Comment this out if debugging:
(defalias 'q-c-debug #'ignore)

(when (featurep 'mule)
  (let ((ascii-chars-string (apply #'string
                                   (loop for i from #x0 to #x7f
                                     collect (int-to-char i))))
        (latin-1-chars-string (apply #'string 
                                     (loop for i from #x0 to #xff
                                       collect (int-to-char i))))
        unix-coding-system text-conversion-error-signalled)
    (with-temp-buffer
      (insert ascii-chars-string)
      ;; First, check all the coding systems that are ASCII-transparent for
      ;; ASCII-transparency in the check.
      (dolist (coding-system
               (delete-duplicates
                (mapcar #'(lambda (coding-system)
                            (unless (coding-system-alias-p coding-system)
                              ;; We're only interested in the version with
                              ;; Unix line endings right now.
                              (setq unix-coding-system 
                                    (subsidiary-coding-system
                                     (coding-system-base coding-system) 'lf))
                              (when (and 
                                     ;; ASCII-transparent
                                     (equal ascii-chars-string
                                           (encode-coding-string
                                            ascii-chars-string
                                            unix-coding-system))
                                     (not 
                                      (memq (coding-system-type
                                             unix-coding-system)
                                            '(undecided chain))))
                                unix-coding-system)))
                        (coding-system-list nil))
                :test #'eq))
        (q-c-debug "looking at coding system %S" (coding-system-name
                                                  coding-system))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) (point-max) coding-system)
          (q-c-debug "checking type, coding-system, q-c-s, q-c-t %S"
                     (list (coding-system-type coding-system)
                           coding-system query-coding-succeeded
                           query-coding-table))
          (unless (and (eq t query-coding-succeeded)
                       (null query-coding-table))
            (q-c-debug "(eq t query-coding-succeeded) %S, (\
null query-coding-table) %S" (eq t query-coding-succeeded)
                             (null query-coding-table)))
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        (q-c-debug "testing the ASCII strings for %S" coding-system)
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-string ascii-chars-string coding-system)
          (unless (and (eq t query-coding-succeeded)
                       (null query-coding-table))
            (q-c-debug "(eq t query-coding-succeeded) %S, (\
null query-coding-table) %S" (eq t query-coding-succeeded)
                             (null query-coding-table)))
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table))))
      (q-c-debug "past the loop through the coding systems")
      (delete-region (point-min) (point-max))
      ;; Check for success from the two Latin-1 coding systems 
      (insert latin-1-chars-string)
      (q-c-debug "point is now %S" (point))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (q-c-debug "point is now %S" (point))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (q-c-debug "point is now %S" (point))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-latin-1-with-esc-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (q-c-debug "point is now %S" (point))
      ;; Make it fail, check that it fails correctly
      (insert (decode-char 'ucs #x20AC)) ;; EURO SIGN
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
        (unless (and (null query-coding-succeeded)
                     (equal query-coding-table
                            #s(range-table type start-closed-end-open data
                                           ((257 258) t))))
          (q-c-debug "dealing with %S" 'iso-8859-1-unix)
          (q-c-debug "query-coding-succeeded not null, query-coding-table \
%S" query-coding-table))
        (Assert (null query-coding-succeeded))
        (Assert (equal query-coding-table
                       #s(range-table type start-closed-end-open data
                                      ((257 258) t)))))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max)
                               'iso-latin-1-with-esc-unix)
        ;; Stupidly, this succeeds. The behaviour is compatible with
        ;; GNU, though, and we encourage people not to use
        ;; iso-latin-1-with-esc-unix anyway:

        (unless (and query-coding-succeeded
                     (null query-coding-table))
          (q-c-debug "dealing with %S" 'iso-latin-1-with-esc-unix)
          (q-c-debug "query-coding-succeeded %S, query-coding-table \
%S" query-coding-succeeded query-coding-table))
        (Assert query-coding-succeeded)
        (Assert (null query-coding-table)))
      ;; Check that it errors correctly. 
      (setq text-conversion-error-signalled nil)
      (condition-case nil
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix nil t)
        (text-conversion-error
         (setq text-conversion-error-signalled t)))
      (Assert text-conversion-error-signalled)
      (setq text-conversion-error-signalled nil)
      (condition-case nil
          (query-coding-region (point-min) (point-max)
                               'iso-latin-1-with-esc-unix nil t)
        (text-conversion-error
         (setq text-conversion-error-signalled t)))
      (Assert (null text-conversion-error-signalled))
      (delete-region (point-min) (point-max))
      (insert latin-1-chars-string)
      (decode-coding-region (point-min) (point-max) 'windows-1252-unix)
      (goto-char (point-max)) ;; #'decode-coding-region just messed up point.
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (insert ?\x80)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (unless (and (null query-coding-succeeded)
                     (equal query-coding-table
                            #s(range-table type start-closed-end-open data
                                           ((257 258) t))))
          (q-c-debug "dealing with %S" 'windows-1252-unix)
          (q-c-debug "query-coding-succeeded not null, query-coding-table \
%S" query-coding-table))
        (Assert (null query-coding-succeeded))
        (Assert (equal query-coding-table
                       #s(range-table type start-closed-end-open data
                                      ((257 258) t)))))
      ;; Try a similar approach with koi8-o, the koi8 variant with
      ;; support for Old Church Slavonic.
      (delete-region (point-min) (point-max))
      (insert latin-1-chars-string)
      (decode-coding-region (point-min) (point-max) 'koi8-o-unix)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'koi8-o-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'escape-quoted)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (unless (and (null query-coding-succeeded)
                     (equal query-coding-table
                            #s(range-table type start-closed-end-open
                                           data ((129 131) t (132 133) t
                                                 (139 140) t (141 146) t
                                                 (155 156) t (157 161) t
                                                 (162 170) t (173 176) t
                                                 (178 187) t (189 192) t
                                                 (193 257) t))))
          (q-c-debug "query-coding-succeeded not null, query-coding-table \
%S" query-coding-table))
        (Assert (null query-coding-succeeded))
        (Assert (equal query-coding-table
                       #s(range-table type start-closed-end-open
                                      data ((129 131) t (132 133) t (139 140) t
                                            (141 146) t (155 156) t (157 161) t
                                            (162 170) t (173 176) t (178 187) t
                                            (189 192) t (193 257) t)))))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'koi8-r-unix)
        (Assert (null query-coding-succeeded))
        (Assert (equal query-coding-table
                       #s(range-table type start-closed-end-open
                                      data ((129 154) t (155 161) t (162 164) t
                                            (165 177) t (178 180) t
                                            (181 192) t)))))
      ;; Check that the Unicode coding systems handle characters
      ;; without Unicode mappings.
      (delete-region (point-min) (point-max))
      (insert latin-1-chars-string)
      (decode-coding-region (point-min) (point-max) 'greek-iso-8bit-with-esc)
      (dolist (coding-system
               '(utf-16-mac ucs-4-mac utf-16-little-endian-bom-dos ucs-4-dos
                 utf-16-little-endian-mac utf-16-bom-unix
                 utf-16-little-endian ucs-4 utf-16-dos
                 ucs-4-little-endian-dos utf-16-bom-mac utf-16-bom
                 utf-16-unix utf-32-unix utf-32-little-endian
                 utf-32-dos utf-32 utf-32-little-endian-dos utf-8-bom
                 utf-16-bom-dos ucs-4-unix
                 utf-16-little-endian-bom-unix utf-8-bom-mac
                 utf-32-little-endian-unix utf-16
                 utf-16-little-endian-dos utf-16-little-endian-bom-mac
                 utf-8-bom-dos ucs-4-little-endian-mac utf-8-bom-unix
                 utf-32-little-endian-mac utf-8-dos utf-8-unix
                 utf-32-mac utf-8-mac utf-16-little-endian-unix
                 ucs-4-little-endian ucs-4-little-endian-unix utf-8
                 utf-16-little-endian-bom))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) (point-max) coding-system)
          (Assert (null query-coding-succeeded))
          (Assert (equal query-coding-table
                         #s(range-table type start-closed-end-open data
                                        ((173 174) t (209 210) t
                                         (254 255) t)))))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) 173 coding-system)
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region 174 209 coding-system)
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region 210 254 coding-system)
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        ;; Check that it errors correctly. 
        (setq text-conversion-error-signalled nil)
        (condition-case nil
            (query-coding-region (point-min) (point-max) coding-system nil t)
          (text-conversion-error
           (setq text-conversion-error-signalled t)))
        (Assert text-conversion-error-signalled)
        (setq text-conversion-error-signalled nil)
        (condition-case nil
            (query-coding-region (point-min) 173 coding-system nil t)
          (text-conversion-error
           (setq text-conversion-error-signalled t)))
        (Assert (null text-conversion-error-signalled))))))
