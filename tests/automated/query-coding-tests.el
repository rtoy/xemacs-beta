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
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-string ascii-chars-string coding-system)
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table))))
      (delete-region (point-min) (point-max))
      ;; Check for success from the two Latin-1 coding systems 
      (insert latin-1-chars-string)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-latin-1-with-esc-unix)
        (Assert (eq t query-coding-succeeded))
        (Assert (null query-coding-table)))
      ;; Make it fail, check that it fails correctly
      (insert (decode-char 'ucs #x20AC)) ;; EURO SIGN
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
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
        (Assert (null text-conversion-error-signalled)))

      ;; Now to test #'encode-coding-char. Most of the functionality was
      ;; tested in the query-coding-region tests above, so we don't go into
      ;; as much detail here.
      (Assert (null (encode-coding-char
                     (decode-char 'ucs #x20ac) 'iso-8859-1)))
      (Assert (equal "\x80" (encode-coding-char 
                             (decode-char 'ucs #x20ac) 'windows-1252)))
      (delete-region (point-min) (point-max))

      ;; And #'unencodable-char-position. 
      (insert latin-1-chars-string)
      (insert (decode-char 'ucs #x20ac))
      (Assert (= 257 (unencodable-char-position (point-min) (point-max)
                                                'iso-8859-1)))
      (Assert (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 1)))
      ;; Compatiblity, sigh: 
      (Assert (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 0)))
      (dotimes (i 6) (insert (decode-char 'ucs #x20ac)))
      ;; Check if it stops at one:
      (Assert (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 1)))
      ;; Check if it stops at four:
      (Assert (equal '(260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 4)))
      ;; Check whether it stops at seven: 
      (Assert (equal '(263 262 261 260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 7)))
      ;; Check that it still stops at seven:
      (Assert (equal '(263 262 261 260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 2000)))
      ;; Now, #'check-coding-systems-region. 
      ;; UTF-8 should certainly be able to encode these characters:
      (Assert (eq t (check-coding-systems-region (point-min) (point-max)
                                                 '(utf-8))))
      (Assert (equal '((iso-8859-1 257 258 259 260 261 262 263)
                       (windows-1252 129 131 132 133 134 135 136 137 138 139
                                     140 141 143 146 147 148 149 150 151 152
                                     153 154 155 156 157 159 160))
                       (sort
                        (check-coding-systems-region (point-min) (point-max)
                                                     '(utf-8 iso-8859-1
                                                       windows-1252))
                        ;; (The sort is to make the algorithm irrelevant.)
                        #'(lambda (left right)
                            (string< (car left) (car right))))))
      ;; Ensure that the indices are all decreased by one when passed a
      ;; string:
      (Assert (equal '((iso-8859-1 256 257 258 259 260 261 262)
                       (windows-1252 128 130 131 132 133 134 135 136 137 138
                                     139 140 142 145 146 147 148 149 150 151
                                     152 153 154 155 156 158 159))
                     (sort
                      (check-coding-systems-region (buffer-string) nil
                                                   '(utf-8 iso-8859-1
                                                     windows-1252))
                      #'(lambda (left right)
                          (string< (car left) (car right)))))))))

