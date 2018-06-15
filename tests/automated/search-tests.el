;;; -*- coding: iso-8859-1 -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.
;; Copyright (C) 2010 Ben Wing.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Created: 2000
;; Keywords: tests

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test of non-regexp searching.

;; Split out of case-tests.el.

;; NOTE NOTE NOTE: See also:
;;
;; (1) regexp-tests.el, for regexp searching.
;; (2) case-tests.el, for some case-related searches.

;; NOTE NOTE NOTE: There is some domain overlap among case-tests.el,
;; regexp-tests.el and search-tests.el.  See case-tests.el.

(with-temp-buffer
  (insert "Test Buffer")
  (let ((case-fold-search t))
    (goto-char (point-min))
    (Assert (eq (search-forward "test buffer" nil t) 12))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test buffer" nil t) 12))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test Buffer" nil t) 12))

    (setq case-fold-search nil)
    (goto-char (point-min))
    (Assert (not (search-forward "test buffer" nil t)))
    (goto-char (point-min))
    (Assert (not (search-forward "Test buffer" nil t)))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test Buffer" nil t) 12))))

(with-temp-buffer
  (insert "abcdefghijklmn�opqrstu�vwxyz")
  ;; case insensitive
  (Assert (not (search-forward "�" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "�" nil t)))
  (Assert (eq 24 (search-forward "�" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "�" nil t)))
  (Assert (eq 24 (search-forward "�" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "�" nil t)))
  (Assert (eq 15 (search-backward "�" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "�" nil t)))
  (Assert (eq 15 (search-backward "�" nil t)))
  ;; case sensitive
  (setq case-fold-search nil)
  (goto-char (point-min))
  (Assert (not (search-forward "�" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "�" nil t)))
  (Assert (not (search-forward "�" nil t)))
  (goto-char (point-min))
  (Assert (eq 24 (search-forward "�" nil t)))
  (goto-char 16)
  (Assert (eq 24 (search-forward "�" nil t)))
  (goto-char (point-max))
  (Assert (eq 15 (search-backward "�" nil t)))
  (goto-char 15)
  (Assert (not (search-backward "�" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "�" nil t)))
  (Assert (not (search-backward "�" nil t))))

(with-temp-buffer
  (insert "aaaa����������bbbb")
  (goto-char (point-min))
  (Assert (eq 15 (search-forward "��" nil t 5)))
  (goto-char (point-min))
  (Assert (not (search-forward "��" nil t 6)))
  (goto-char (point-max))
  (Assert (eq 5 (search-backward "��" nil t 5)))
  (goto-char (point-max))
  (Assert (not (search-backward "��" nil t 6))))

(when (featurep 'mule)
  (let* ((hiragana-a (make-char 'japanese-jisx0208 36 34))
	 (a-diaeresis ?�)
	 (case-table (copy-case-table (standard-case-table)))
	 (str-hiragana-a (char-to-string hiragana-a))
	 (str-a-diaeresis (char-to-string a-diaeresis))
	 (string (concat str-hiragana-a str-a-diaeresis)))
    (put-case-table-pair hiragana-a a-diaeresis case-table)
    (with-temp-buffer
      (set-case-table case-table)
      (insert hiragana-a "abcdefg" a-diaeresis)
      ;; forward
      (goto-char (point-min))
      (Assert (not (search-forward "�" nil t)))
      (goto-char (point-min))
      (Assert (eq 2 (search-forward str-hiragana-a nil t)))
      (goto-char (point-min))
      (Assert (eq 2 (search-forward str-a-diaeresis nil t)))
      (goto-char (1+ (point-min)))
      (Assert (eq (point-max)
		  (search-forward str-hiragana-a nil t)))
      (goto-char (1+ (point-min)))
      (Assert (eq (point-max)
		  (search-forward str-a-diaeresis nil t)))
      ;; backward
      (goto-char (point-max))
      (Assert (not (search-backward "�" nil t)))
      (goto-char (point-max))
      (Assert (eq (1- (point-max)) (search-backward str-hiragana-a nil t)))
      (goto-char (point-max))
      (Assert (eq (1- (point-max)) (search-backward str-a-diaeresis nil t)))
      (goto-char (1- (point-max)))
      (Assert (eq 1 (search-backward str-hiragana-a nil t)))
      (goto-char (1- (point-max)))
      (Assert (eq 1 (search-backward str-a-diaeresis nil t)))
      (replace-match "a")
      (Assert (looking-at (format "abcdefg%c" a-diaeresis))))
    (with-temp-buffer
      (set-case-table case-table)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (goto-char (point-min))
      (Assert (eq 11 (search-forward string nil t 5)))
      (goto-char (point-min))
      (Assert (not (search-forward string nil t 6)))
      (goto-char (point-max))
      (Assert (eq 1 (search-backward string nil t 5)))
      (goto-char (point-max))
      (Assert (not (search-backward string nil t 6))))))

;; Bug reported in http://mid.xemacs.org/y9lk5lu5orq.fsf@deinprogramm.de from
;; Michael Sperber. Fixed 2008-01-29.
(with-string-as-buffer-contents "\n\nDer beruhmte deutsche Flei\xdf\n\n"
  (goto-char (point-min))
  (Assert (search-forward "Flei\xdf")))

(with-temp-buffer
  (let ((target "M\xe9zard")
        (debug-searches 1))
    (Assert (not (search-forward target nil t)))
    (insert target)
    (goto-char (point-min))
    ;; #### search-algorithm-used is simple-search after the following,
    ;; which shouldn't be necessary; it should be possible to use
    ;; Boyer-Moore. 
    ;;
    ;; But searches for ASCII strings in buffers with nothing above ?\xFF
    ;; use Boyer Moore with the current implementation, which is the
    ;; important thing for the Gnus use case.
    (Assert (= (1+ (length target)) (search-forward target nil t)))))

(Skip-Test-Unless
 (boundp 'debug-searches) ; normal when we have DEBUG_XEMACS
 "not a DEBUG_XEMACS build"
 "checks that the algorithm chosen by #'search-forward is relatively sane"
 (let ((debug-searches 1)
       newcase)
   (with-temp-buffer
     (insert "\n\nDer beruehmte deutsche Fleiss\n\n")
     (goto-char (point-min))
     (Assert (search-forward "Fleiss"))
     (delete-region (point-min) (point-max))
     (insert "\n\nDer ber\xfchmte \u00BBdeutsche\u00AB Flei\xdf\n\n")
     (goto-char (point-min))
     (Assert (search-forward "\u00BBdeutsche\u00AB"))
     (Assert (eq 'boyer-moore search-algorithm-used)
	     "Boyer-Moore not used with a Latin-1 buffer")
     (delete-region (point-min) (point-max))
     (when (featurep 'mule)
       (insert "\n\nDer ber\xfchmte deutsche Fleiss\n\n")
       (goto-char (point-min))
       (Assert 
        (search-forward (format "Fle%css"
                                (make-char 'latin-iso8859-9 #xfd))))
       (Assert
	(eq 'boyer-moore search-algorithm-used)
	"Boyer-Moore not used with a Latin-1 buffer, Latin-9 search string")
       (insert (make-char 'latin-iso8859-9 #xfd))
       (goto-char (point-min))
       (Assert (search-forward "Fleiss"))
       (Assert (eq 'simple-search search-algorithm-used))
       (goto-char (point-min))
       (Assert (search-forward (format "Fle%css"
                                       (make-char 'latin-iso8859-9 #xfd))))
       (Assert (eq 'simple-search search-algorithm-used)
	       "simple search not used with a Latin 9 dotless i, case equivalents with differing byte lengths")
       (setq newcase (copy-case-table (standard-case-table)))
       (put-case-table-pair (make-char 'ethiopic #x23 #x23)
			    (make-char 'ethiopic #x23 #x25)
			    newcase)
       (with-case-table
	 ;; Check that when a multidimensional character has case and two
	 ;; repeating octets, searches involving it in the search pattern
	 ;; use simple-search; otherwise boyer_moore() gets confused in the
	 ;; construction of the stride table.
	 newcase
	 (delete-region (point-min) (point-max))
	 (insert ?0)
	 (insert (make-char 'ethiopic #x23 #x23))
	 (insert ?1)
	 (goto-char (point-min))
	 (Assert (eql (search-forward
		      (string (make-char 'ethiopic #x23 #x25))
		      nil t)
		     3))
	 (Assert (eq 'simple-search search-algorithm-used)
		 "simple search not used with multidimensional character with case and repeated octets")
	 (goto-char (point-min))
	 (Assert (eql (search-forward
		      (string (make-char 'ethiopic #x23 #x27))
		      nil t)
		     nil))
	 (Assert (eq 'boyer-moore search-algorithm-used)
		 "Boyer-Moore not used with multidimensional character with no repeated octets and no case"))))))

;; XEmacs bug of long standing.

(with-temp-buffer
  (insert "foo\201bar")
  (goto-char (point-min))
  (Assert (eq (search-forward "\201" nil t) 5)))

;; end of search-tests.el
