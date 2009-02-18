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
      ;; ASCII-transparency in query-coding-region.
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
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) (point-max) coding-system)
          (Assert (eq t query-coding-succeeded)
                  (format "checking query-coding-region ASCII-transparency, %s"
                          coding-system))
          (Assert (null query-coding-table)
                  (format "checking query-coding-region ASCII-transparency, %s"
                          coding-system)))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-string ascii-chars-string coding-system)
          (Assert (eq t query-coding-succeeded)
                  (format "checking query-coding-string ASCII-transparency, %s"
                          coding-system))
          (Assert (null query-coding-table)
                  (format "checking query-coding-string ASCII-transparency, %s"
                          coding-system))))
      (delete-region (point-min) (point-max))
      ;; Check for success from the two Latin-1 coding systems 
      (insert latin-1-chars-string)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded)
                "checking query-coding-region iso-8859-1-transparency")
        (Assert (null query-coding-table)
                "checking query-coding-region iso-8859-1-transparency"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-8859-1-unix)
        (Assert (eq t query-coding-succeeded)
                "checking query-coding-string iso-8859-1-transparency")
        (Assert (null query-coding-table)
                "checking query-coding-string iso-8859-1-transparency"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-string (buffer-string) 'iso-latin-1-with-esc-unix)
        (Assert
         (eq t query-coding-succeeded)
         "checking query-coding-region iso-latin-1-with-esc-transparency")
        (Assert
         (null query-coding-table)
         "checking query-coding-region iso-latin-1-with-esc-transparency"))
      ;; Make it fail, check that it fails correctly
      (insert (decode-char 'ucs #x20AC)) ;; EURO SIGN
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix)
        (Assert
         (null query-coding-succeeded)
         "checking that query-coding-region fails, U+20AC, iso-8859-1")
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open data
                               ((257 258) unencodable)))
         "checking query-coding-region fails correctly, U+20AC, iso-8859-1"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max)
                               'iso-latin-1-with-esc-unix)
        ;; Stupidly, this succeeds. The behaviour is compatible with
        ;; GNU, though, and we encourage people not to use
        ;; iso-latin-1-with-esc-unix anyway:
        (Assert
	 query-coding-succeeded
         "checking that query-coding-region succeeds, U+20AC, \
iso-latin-with-esc-unix-1")
        (Assert
	 (null query-coding-table)
         "checking that query-coding-region succeeds, U+20AC, \
iso-latin-with-esc-unix-1"))
      ;; Check that it errors correctly. 
      (setq text-conversion-error-signalled nil)
      (condition-case nil
          (query-coding-region (point-min) (point-max) 'iso-8859-1-unix
			       (current-buffer) nil t)
        (text-conversion-error
         (setq text-conversion-error-signalled t)))
      (Assert
       text-conversion-error-signalled
       "checking query-coding-region signals text-conversion-error correctly")
      (setq text-conversion-error-signalled nil)
      (condition-case nil
          (query-coding-region (point-min) (point-max)
                               'iso-latin-1-with-esc-unix nil nil t)
        (text-conversion-error
         (setq text-conversion-error-signalled t)))
      (Assert
       (null text-conversion-error-signalled)
       "checking query-coding-region doesn't signal text-conversion-error")
      (delete-region (point-min) (point-max))
      (insert latin-1-chars-string)
      (decode-coding-region (point-min) (point-max) 'windows-1252-unix)
      (goto-char (point-max)) ;; #'decode-coding-region just messed up point.
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (Assert
         (null query-coding-succeeded)
         "check query-coding-region fails, windows-1252, invalid-sequences")
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open
                               data ((130 131) invalid-sequence
                                     (142 143) invalid-sequence
                                     (144 146) invalid-sequence
                                     (158 159) invalid-sequence)))
         "check query-coding-region fails, windows-1252, invalid-sequences"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix
			       (current-buffer) t)
        (Assert
         (eq t query-coding-succeeded)
         "checking that query-coding-region succeeds, U+20AC, windows-1252")
        (Assert
         (null query-coding-table)
         "checking that query-coding-region succeeds, U+20AC, windows-1252"))
      (insert ?\x80)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix
			       (current-buffer) t)
        (Assert
         (null query-coding-succeeded)
         "checking that query-coding-region fails, U+0080, windows-1252")
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open data
                               ((257 258) unencodable)))
         "checking that query-coding-region fails, U+0080, windows-1252"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (Assert
         (null query-coding-succeeded)
         "check query-coding-region fails, U+0080, invalid-sequence, cp1252")
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open
                               data ((130 131) invalid-sequence
                                     (142 143) invalid-sequence
                                     (144 146) invalid-sequence
                                     (158 159) invalid-sequence
                                     (257 258) unencodable)))
         "check query-coding-region fails, U+0080, invalid-sequence, cp1252"))
      ;; Try a similar approach with koi8-o, the koi8 variant with
      ;; support for Old Church Slavonic.
      (delete-region (point-min) (point-max))
      (insert latin-1-chars-string)
      (decode-coding-region (point-min) (point-max) 'koi8-o-unix)
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'koi8-o-unix)
        (Assert
         (eq t query-coding-succeeded)
         "checking that query-coding-region succeeds, koi8-o-unix")
        (Assert
         (null query-coding-table)
         "checking that query-coding-region succeeds, koi8-o-unix"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'escape-quoted)
        (Assert (eq t query-coding-succeeded)
         "checking that query-coding-region succeeds, escape-quoted")
        (Assert (null query-coding-table)
         "checking that query-coding-region succeeds, escape-quoted"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'windows-1252-unix)
        (Assert
         (null query-coding-succeeded)
         "checking that query-coding-region fails, windows-1252 and Cyrillic") 
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open
                               data ((129 131) unencodable
                                     (132 133) unencodable
                                     (139 140) unencodable
                                     (141 146) unencodable
                                     (155 156) unencodable
                                     (157 161) unencodable
                                     (162 170) unencodable
                                     (173 176) unencodable
                                     (178 187) unencodable
                                     (189 192) unencodable
                                     (193 257) unencodable)))
         "checking that query-coding-region fails, windows-1252 and Cyrillic"))
      (multiple-value-bind (query-coding-succeeded query-coding-table)
          (query-coding-region (point-min) (point-max) 'koi8-r-unix)
        (Assert
         (null query-coding-succeeded)
         "checking that query-coding-region fails, koi8-r and OCS characters")
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open
                               data ((129 154) unencodable
                                     (155 161) unencodable
                                     (162 164) unencodable
                                     (165 177) unencodable
                                     (178 180) unencodable
                                     (181 192) unencodable)))
         "checking that query-coding-region fails, koi8-r and OCS characters")) 
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
          (Assert (null query-coding-succeeded)
                  "checking unicode coding systems fail with unmapped chars")
          (Assert (equal query-coding-table
                         #s(range-table type start-closed-end-open data
                                        ((173 174) unencodable
                                         (209 210) unencodable
                                         (254 255) unencodable)))
                  "checking unicode coding systems fail with unmapped chars"))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) 173 coding-system)
          (Assert (eq t query-coding-succeeded)
                  "checking unicode coding systems succeed sans unmapped chars")
          (Assert
           (null query-coding-table)
           "checking unicode coding systems succeed sans unmapped chars"))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region 174 209 coding-system)
          (Assert
           (eq t query-coding-succeeded)
           "checking unicode coding systems succeed sans unmapped chars, again")
          (Assert
           (null query-coding-table)
           "checking unicode coding systems succeed sans unmapped chars again"))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region 210 254 coding-system)
          (Assert (eq t query-coding-succeeded))
          (Assert (null query-coding-table)))
        ;; Check that it errors correctly. 
        (setq text-conversion-error-signalled nil)
        (condition-case nil
            (query-coding-region (point-min) (point-max) coding-system
				 (current-buffer) nil t)
          (text-conversion-error
           (setq text-conversion-error-signalled t)))
        (Assert text-conversion-error-signalled
                "checking that unicode coding systems error correctly")
        (setq text-conversion-error-signalled nil)
        (condition-case nil
            (query-coding-region (point-min) 173 coding-system
				 (current-buffer)
				 nil t)
          (text-conversion-error
           (setq text-conversion-error-signalled t)))
        (Assert
         (null text-conversion-error-signalled)
         "checking that unicode coding systems do not error when unnecessary"))

      (delete-region (point-min) (point-max))
      (insert (decode-coding-string "\xff\xff\xff\xff"
                                    'greek-iso-8bit-with-esc))
      (insert (decode-coding-string "\xff\xff\xff\xff" 'utf-8))
      (insert (decode-coding-string "\xff\xff\xff\xff"
                                    'greek-iso-8bit-with-esc))
      (dolist (coding-system '(utf-8 utf-16 utf-16-little-endian
                               utf-32 utf-32-little-endian))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) (point-max) coding-system)
          (Assert (null query-coding-succeeded)
                  (format 
                   "checking %s fails with unmapped chars and invalid seqs"
                   coding-system))
          (Assert (equal query-coding-table
                         #s(range-table type start-closed-end-open
                                        data ((1 5) unencodable
                                              (5 9) invalid-sequence
                                              (9 13) unencodable)))
                  (format 
                   "checking %s fails with unmapped chars and invalid seqs"
                   coding-system)))
        (multiple-value-bind (query-coding-succeeded query-coding-table)
            (query-coding-region (point-min) (point-max) coding-system
                                 (current-buffer) t)
        (Assert (null query-coding-succeeded)
                (format 
                 "checking %s fails with unmapped chars sans invalid seqs"
                 coding-system))
        (Assert
         (equal query-coding-table
                #s(range-table type start-closed-end-open
                               data ((1 5) unencodable
                                     (9 13) unencodable)))
         (format
          "checking %s fails correctly, unmapped chars sans invalid seqs"
          coding-system))))
      ;; Now to test #'encode-coding-char. Most of the functionality was
      ;; tested in the query-coding-region tests above, so we don't go into
      ;; as much detail here.
      (Assert
       (null (encode-coding-char
              (decode-char 'ucs #x20ac) 'iso-8859-1))
       "check #'encode-coding-char doesn't think iso-8859-1 handles U+20AC")
      (Assert
       (equal "\x80" (encode-coding-char 
                      (decode-char 'ucs #x20ac) 'windows-1252))
       "check #'encode-coding-char doesn't think windows-1252 handles U+0080")
      (delete-region (point-min) (point-max))

      ;; And #'unencodable-char-position. 
      (insert latin-1-chars-string)
      (insert (decode-char 'ucs #x20ac))
      (Assert
       (= 257 (unencodable-char-position (point-min) (point-max)
                                         'iso-8859-1))
       "check #'unencodable-char-position doesn't think latin-1 encodes U+20AC")
      (Assert
       (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                'iso-8859-1 1))
       "check #'unencodable-char-position doesn't think latin-1 encodes U+20AC")
      ;; Compatiblity, sigh: 
      (Assert
       (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                'iso-8859-1 0))
       "check #'unencodable-char-position has some borked GNU semantics")
      (dotimes (i 6) (insert (decode-char 'ucs #x20ac)))
      ;; Check if it stops at one:
      (Assert (equal '(257) (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 1))
              "check #'unencodable-char-position stops at 1 when asked to")
      ;; Check if it stops at four:
      (Assert (equal '(260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 4))
              "check #'unencodable-char-position stops at 4 when asked to")
      ;; Check whether it stops at seven: 
      (Assert (equal '(263 262 261 260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 7))
              "check #'unencodable-char-position stops at 7 when asked to")
      ;; Check that it still stops at seven:
      (Assert (equal '(263 262 261 260 259 258 257)
                     (unencodable-char-position (point-min) (point-max)
                                                       'iso-8859-1 2000))
              "check #'unencodable-char-position stops at 7 if 2000 asked for")
      ;; Now, #'check-coding-systems-region. 
      ;; UTF-8 should certainly be able to encode these characters:
      (Assert (null (check-coding-systems-region (point-min) (point-max)
                                                 '(utf-8)))
              "check #'check-coding-systems-region gives nil if encoding works")
      (Assert
       (equal '((iso-8859-1 257 258 259 260 261 262 263)
                (windows-1252 129 130 131 132 133 134 135 136
                              137 138 139 140 141 142 143 144
                              145 146 147 148 149 150 151 152
                              153 154 155 156 157 158 159 160))
              (sort
               (check-coding-systems-region (point-min) (point-max)
                                            '(utf-8 iso-8859-1
                                              windows-1252))
               ;; (The sort is to make the algorithm irrelevant.)
               #'(lambda (left right)
                   (string< (car left) (car right)))))
       "check #'check-coding-systems-region behaves well given a list")
      ;; Ensure that the indices are all decreased by one when passed a
      ;; string:
      (Assert
       (equal '((iso-8859-1 256 257 258 259 260 261 262)
                (windows-1252 128 129 130 131 132 133 134 135
                              136 137 138 139 140 141 142 143
                              144 145 146 147 148 149 150 151
                              152 153 154 155 156 157 158 159))
              (sort
               (check-coding-systems-region (buffer-string) nil
                                            '(utf-8 iso-8859-1
                                              windows-1252))
               #'(lambda (left right)
                   (string< (car left) (car right)))))
       "check #'check-coding-systems-region behaves given a string and list"))))



