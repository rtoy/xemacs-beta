;;; descr-text.el --- describe text mode

;; Copyright (C) 1994, 1995, 1996, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Maintainer: FSF
;; Keywords: faces, i18n, Unicode, multilingual

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Describe-Text Mode.

;;; Code:

(eval-when-compile (require 'wid-edit))

(require 'hyper-apropos)

;;; Describe-Text Utilities.

(defun describe-text-widget (widget)
  "Insert text to describe WIDGET in the current buffer."
  ;; XEmacs change; use the widget function.
  (widget-create 'push-button
                 :notify `(lambda (&rest ignore)
                            (widget-browse ',widget))
                 :help-echo
                 "mouse-2, RET: browse this widget"
                 (symbol-name (if (symbolp widget)
                                  widget
                                (car widget))))
  (widget-insert " ")
  (widget-create 'info-link
                 :tag "Widget help"
                 :help-echo
                 "Read widget documentation"
                 "(widget)Top"))

(defun describe-text-sexp (sexp)
  "Insert a short description of SEXP in the current buffer."
  ;; XEmacs change; use the widget functions. 
  (let ((pp (condition-case signal
                ;; XEmacs change; pp is in packages, use cl-prettyprint
                ;; instead.
		(with-output-to-string (cl-prettyprint sexp))
	      (error (prin1-to-string signal)))))
    (when (string-match "\n\\'" pp)
      (setq pp (substring pp 0 (1- (length pp)))))
    (if (cond ((string-match "\n" pp)
	       nil)
	      ((> (length pp) (- (window-width) (current-column)))
	       nil)
	      (t t))
	(widget-insert pp)
      (widget-create 'push-button
                     :notify `(lambda (&rest ignore)
                                (with-output-to-temp-buffer
                                    "*Pp Eval Output*"
                                  (princ ',pp)))
                     :help-echo
                     "mouse-2, RET: pretty print value in another buffer"
                     "[Show]"))))

(defun describe-property-list (properties)
  "Insert a description of PROPERTIES in the current buffer.
PROPERTIES should be a list of overlay or text properties.
The `category', `face' and `font-lock-face' properties are made
into help buttons that call `describe-text-category' or
`describe-face' when pushed."
  ;; Sort the properties by the size of their value.
  (dolist (elt (sort (let (ret)
		       (while properties
			 (push (list (pop properties) (pop properties)) ret))
		       ret)
		     (lambda (a b) (string< (prin1-to-string (nth 0 a) t)
					    (prin1-to-string (nth 0 b) t)))))
    (let ((key (nth 0 elt))
	  (value (nth 1 elt)))
      ;; XEmacs change; use #'widget-insert, #'widget-create
      (widget-insert (propertize (format "  %-20s " key)
                                 'face 'hyper-apropos-heading))
      (cond ((eq key 'category)
             (widget-create
              'push-button
	      :notify `(lambda (&rest ignore)
			 (describe-text-category ',value))
	      :help-echo "mouse-2, RET: describe this category"
              (symbol-name value)))
            ((memq key '(face font-lock-face mouse-face))
             (widget-create
              'push-button
              :notify (lexical-let
                          ((value-name (symbol-name value)))
                        (lambda (&rest ignore)
                          (hyper-describe-face (intern value-name))))
	      :help-echo "mouse-2, RET: describe this face"
              (format "%S" value)))
            ((widgetp value)
	     (describe-text-widget value))
	    (t
	     (describe-text-sexp value))))
    (insert "\n")))

;;; Describe-Text Commands.

(defun describe-text-category (category)
  "Describe a text property category."
  (interactive "SCategory: ")
  ; (help-setup-xref (list #'describe-text-category category) (interactive-p))
  (save-excursion
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (insert "Category " (format "%S" category) ":\n\n")
      (describe-property-list (symbol-plist category))
      (goto-char (point-min)))))

;;;###autoload
(defun describe-text-properties (pos &optional output-buffer)
  "Describe widgets, buttons, overlays and text properties at POS.
Interactively, describe them for the character after point.
If optional second argument OUTPUT-BUFFER is non-nil,
insert the output into that buffer, and don't initialize or clear it
otherwise."
  (interactive "d")
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (if output-buffer
      (describe-text-properties-1 pos output-buffer)
    (if (not (or (text-properties-at pos) ; (overlays-at pos)))
                 ;; XEmacs change. 
                 (extents-at pos)))
	(message "This is plain text.")
      (let ((buffer (current-buffer))
	    (target-buffer "*Help*"))
	(when (eq buffer (get-buffer target-buffer))
	  (setq target-buffer "*Help*<2>"))
	(save-excursion
	  (with-output-to-temp-buffer target-buffer
	    (set-buffer standard-output)
	    (setq output-buffer (current-buffer))
	    (insert "Text content at position " (format "%d" pos) ":\n\n")
	    (with-current-buffer buffer
	      (describe-text-properties-1 pos output-buffer))
	    (goto-char (point-min))))))))

(defun describe-text-properties-1 (pos output-buffer)
  (let* ((properties (text-properties-at pos))
         ;; XEmacs change; extents, not overlays. 
	 (extents (extents-at pos))
	 (wid-field (get-char-property pos 'field))
	 (wid-button (get-char-property pos 'button))
	 (wid-doc (get-char-property pos 'widget-doc))
	 ;; If button.el is not loaded, we have no buttons in the text.
         ;; XEmacs change; use the #'and-fboundp, #'declare-fboundp macros.
	 (button (and-fboundp 'button-at (button-at pos)))
	 (button-type (and button 
                           (declare-fboundp (button-type button))))
	 (button-label (and button 
                            (declare-fboundp (button-label button))))
	 (widget (or wid-field wid-button wid-doc)))
    (with-current-buffer output-buffer
      ;; Widgets
      (when (widgetp widget)
	(newline)
	(insert (cond (wid-field "This is an editable text area")
		      (wid-button "This is an active area")
		      (wid-doc "This is documentation text")))
	(insert " of a ")
	(describe-text-widget widget)
	(insert ".\n\n"))
      ;; Buttons
      (when (and button (not (widgetp wid-button)))
	(newline)
	(insert "Here is a `" (format "%S" button-type)
		"' button labeled `" button-label "'.\n\n"))
      ;; Overlays
      (when extents
	(newline)
	(if (eq (length extents) 1)
	    (insert "There is an extent here:\n")
	  (insert "There are " (format "%d" (length extents))
			 " overlays here:\n"))
	(dolist (extent extents)
	  (insert " From " (format "%d" (extent-start-position extent))
                  " to " (format "%d" (extent-end-position extent)) "\n")
	  (describe-property-list (extent-properties extent)))
	(insert "\n"))
      ;; Text properties
      (when properties
	(newline)
	(insert "There are text properties here:\n")
	(describe-property-list properties)))))

(defcustom describe-char-unicodedata-file
  ;; XEmacs change; initialise this by default, using Perl. 
  (let ((have-perl
         (member-if 
          #'(lambda (path) 
              (file-exists-p (format "%s%cperl" path directory-sep-char))) 
          exec-path)) 
        installprivlib res)
    (when have-perl 
      (setq installprivlib  
            (with-string-as-buffer-contents ""
              (shell-command "perl -V:installprivlib" t) 
              ;; 1+ because buffer offsets start at one. 
              (delete-region 1 (1+ (length "installprivlib='"))) 
              ;; Delete the final newline, semicolon and quotation mark. 
              (delete-region (- (point-max) 3) (point-max)))) 
      (cond 
       ((file-exists-p 
         (setq res
               (format "%s%cunicore%cUnicodeData.txt" 
                       installprivlib directory-sep-char directory-sep-char)))) 
       ((file-exists-p 
         (setq res
               (format "%s%cunicode%cUnicodeData.txt" 
                       installprivlib directory-sep-char directory-sep-char)))))
      res))
  "Location of Unicode data file.
This is the UnicodeData.txt file from the Unicode Consortium, used for
diagnostics.  If it is non-nil `describe-char' will print data
looked up from it.  This facility is mostly of use to people doing
multilingual development.

This is a fairly large file, typically installed with Perl.
At the time of writing it is at the URL
`http://www.unicode.org/Public/UNIDATA/UnicodeData.txt'.

It is possible to build a DBM or Berkeley index cache for this file, so that
it is not necessary to parse the whole file at run time.  See
`unidata-initialize-unicodedata-database'.

See also `describe-char-unihan-file' for the complementary file describing
East Asian Han characters and their associated information."

  :group 'mule
  :type '(choice (const :tag "None" nil)
		 file))

;; XEmacs additions, from here until `describe-char-unicode-data'
(defcustom describe-char-use-cache t
  "Whether `describe-char' should use a DBM or Berkeley DB cache.
This speeds up navigation of `describe-char-unicodedata-file', and makes
navigation of `describe-char-unihan-file' reasonable."
  :group 'mule
  :type '(choice (const :tag "None" nil)
		 file))

(defcustom describe-char-unihan-file nil
  "Location of Unihan file.
This the Unihan.txt file from the Unicode Consortium, used for diagnostics.
If it is non-nil `describe-char' can print data looked up from it.  This
facility is of use to people doing multilingual development, to those
learning Chinese or Japanese, and to a lesser extent to those learning
Korean or Vietnamese.

This is large file, typically not installed with the operating system.  At
the time of writing it is at the URL
`http://www.unicode.org/Public/UNIDATA/UniHan.txt'.

In contrast with `describe-char-unicodedata-file', `describe-char' will not
load this entire file and parse it if it is available.  It requires a
pre-initialized cache; see `unidata-initialize-unihan-database'.  "
  :group 'mule
  :type '(choice (const :tag "None" nil)
		 file))
 
;; XEmacs addition
(defvar unidata-database-format
  (or (and (featurep 'dbm) 'dbm)
      (and (featurep 'berkeley-db) 'berkeley-db))
  "The DB format to use for the `describe-char' cache, or nil if no cache.")

(defvar describe-char-unihan-field-descriptions
  #s(hash-table test equal data 
                ("kAccountingNumeric"
                     "Value as an an accounting numeral"
                 "kBigFive"
                     "Big Five mapping (excluding ETEN, etc. extensions)"
                 "kCCCII"
                     "Hex CCCII code, for libraries in the Republic of China"
                 "kCNS1986"
                     "Hex CNS 11643-1986 mapping, for the Republic of China"
                 "kCNS1992"
                     "Hex CNS 11643-1986 mapping, for the Republic of China"
                 "kCangjie"
                     "Cangjie input code for the character"
                 "kCantonese"
                     "Cantonese pronunciation, using jyutping"
                 "kCheungBauer"
                     "Radical-stroke index, cangjie input code, \
and Cantonese readings"
                 "kCheungBauerIndex"
                     "Index of information about this character \
in Cheung & Bauer, 2002"
                 "kCihaiT"
                     "Lookup information for this character in the \
Cihai dictionary ISBN 962-231-005-2."
                 "kCompatibilityVariant"
                     "Compatibility decomposition for this character"
                 "kCowles"
                     "Lookup information for this character in the \
Cowles dictionary ISBN 962-231-005-2."
                 "kDaeJaweon"
                     "Lookup information for this character in the \
Dae Jaweon (Korean) dictionary, 1988"
                 "kDefinition"
                     "Definition for this character in modern written Chinese"
                 "kEACC"
                     "The EACC (= CCCII, as used by the \
US library of congress) code for this character"
                 "kFenn"
                     "Frequency information for this character from \
Fenn's Chinese-English dictionary, 1979"
                 "kFennIndex"
                     "Lookup information for this character in \
Fenn's Chinese-English dictionary, 1979"
                 "kFourCornerCode"
                     "Four-corner lookup code for this character"
                 "kFrequency"
                     "Frequency information from traditional \
Chinese USENET postings"
                 "kGB0" "GB 2312-80 mapping, ku/ten"
                 "kGB1" "GB 12345-90 mapping, ku/ten"
                 "kGB3" "GB 7589-87 mapping, ku/ten"
                 "kGB5" "GB 7590-87 mapping, ku/ten"
                 "kGB7" "GB 8565-89 mapping, ku/ten"
                 ;; Identical to the previous information?!
                 "kGB8" "GB 8565-89 mapping, ku/ten" 
                 "kGSR"
                     "Lookup information for this character in \
Karlgern's Grammata Serica Recensa"
                 "kGradeLevel"
                     "The first grade in the HK school system \
where knowledge of this character is expected"
                 "kHDZRadBreak" "Whether Hanyu Da Zidian has a radical break \
beginning with this character"
                 "kHKGlyph" "Lookup information for this character in the HK \
glyph reference, ISBN 962-949-040-4"
                 "kHKSCS" "Mapping to the HK Supplementary Character Set for \
Big Five."
                 "kHanYu" "Character lookup information for Hanyu Da Zidian, \
`Great Chinese Character Dictionary'"
                 "kHangul" "Korean pronunciation"
                 "kHanyuPinlu" "Pronunciation and frequency info, from Xiandai\
 Hanyu Pinlu Cidian"
                 "kIBMJapan" "IBM Japanese mapping"
                 "kIICore" "Is this character in the core East Asian \
ideograph set from the IRG?"
                 "kIRGDaeJaweon" "Lookup information for this character \
in the Dae Jaweon (Korean) dictionary"
                 "kIRGDaiKanwaZiten" "Lookup information for this character \
in the Morohashi (Japanese) dictionary"
                 "kIRGHanyuDaZidian" "Lookup information for this character \
in the Hanyu Da Zidian (Chinese) dictionary"
                 "kIRGKangXi" "Lookup information for this character \
in the KangXi dictionary"
                 "kIRG_GSource" "PRC character source information"
                 "kIRG_HSource" "Hong Kong character source information"
                 "kIRG_JSource" "Japanese character source information"
                 "kIRG_KPSource" "Korean character source information"
                 "kIRG_KSource" "Republic of Korean character source\
 information"
                 "kIRG_TSource" "Republic of China character source \
information"
                 "kIRG_USource" "Unicode (standards body) source information"
                 "kIRG_VSource" "Vietnamese character source information"
                 "kJIS0213" "JIS X 0213-2000 mapping in min,ku,ten form"
                 "kJapaneseKun" "Native Japanese pronunciation"
                 "kJapaneseOn" "Sino-Japanese pronunciation"
                 "kJis0" "JIS X 0208-1990 mapping in ku/ten form"
                 "kJis1" "JIS X 0212-1990 mapping in ku/ten form"
                 "kKPS0" "KPS 9566-97 mapping in hexadecimal"
                 "kKPS1" "KPS 10721-2000 mapping in hexadecimal"
                 "kKSC0" "KS X 1001:1992 (KS C 5601-1989) mapping \
in ku/ten form"
                 "kKSC1" "KS X 1002:1991 (KS C 5657-1991) mapping \
in ku/ten form"
                 "kKangXi" "Lookup information for this character \
in the KangXi (Chinese) dictionary"
                 "kKarlgren" "Lookup information for this character \
in Karlgren's dictionary, 1974"
                 "kKorean" "Pronunciation in Korean"
                 "kLau" "Lookup information for this character \
in Lau's Cantonese-English dictionary"
                 "kMainlandTelegraph" "PRC telegraph code"
                 "kMandarin" "Mandarin pronunciation in Pinyin"
                 "kMatthews" "Lookup information for Robert Mathews' \
Chinese-English dictionary"
                 "kMeyerWempe" "Lookup information for Bernard Meyer and \
Theodore Wempe's dictionary"
                 ;; Identical to kIRGDaiKanwaZiten?!?
                 "kMorohashi" "Lookup information for this character \
in the Morohashi (Japanese) dictionary"
                 "kNelson" "Lookup information for this character in \
Nelson's Japanese-English dictionary"
                 "kOtherNumeric" "Esoteric numeric value"
                 "kPhonetic" "Phonetic index data"
                 "kPrimaryNumeric" "Standard numeric value"
                 "kPseudoGB1" "Fake GB 12345-90, for the purposes of \
Unicode inclusion"
                 "kRSAdobe_Japan1_6" "Adobe-Japan1-6 information for \
the character"
                 "kRSJapanese" "Radical/stroke count for Japanese"
                 "kRSKanWa" "Morohashi radical/stroke count"
                 "kRSKangXi" "KangXi radical/stroke count"
                 "kRSKorean" "Korean radical/stroke count"
                 "kRSUnicode" "Unicode radical/stroke count"
                 "kSBGY" "Lookup information for this character in the Song \
Ben Guang Yun Chinese dictionary"
                 "kSemanticVariant" "Semantic variant character"
                 "kSimplifiedVariant" "Simplified variant character"
                 "kSpecializedSemanticVariant" "Specialized semantic variant"
                 "kTaiwanTelegraph" "Taiwanese telegraph code"
                 "kTang" "Tang dynasty pronunciation"
                 "kTotalStrokes" "Total number of strokes"
                 "kTraditionalVariant" "Traditional variant character"
                 "kVietnamese" "Vietnamese pronunciation"
                 "kXerox" "Xerox code"
                 "kZVariant" "Z-variant code(s)"))
  "A map from symbolic Unihan field names to English-language descriptions.")

(defun unidata-generate-database-file-name (unidata-file-name size
                                            database-format)
  "Return a filename suitable for storing the cache for UNIDATA-FILE-NAME."
  (expand-file-name
   (format "~%c.xemacs%c%s-%s" directory-sep-char directory-sep-char
           (md5 (format "%s-%d" unidata-file-name size))
           database-format)))

(defun unidata-initialize-unicodedata-database (unidata-file-name)
  "Init the berkeley or gdbm lookup table for UNIDATA-FILE-NAME.

The table is a (non-SQL) database with information on the file offset of
each Unicode code point described in UNIDATA-FILE-NAME.  In the normal
course of events UNIDATA-FILE-NAME is the value of
`unidata-default-file-name', which see.  "
  (check-argument-type #'file-readable-p unidata-file-name)
  (unless unidata-database-format
    (error 'unimplemented "No (non-SQL) DB support available"))
  (let* ((database-format unidata-database-format)
         (size (eighth (file-attributes unidata-file-name)))
         (database-file-name
          (unidata-generate-database-file-name unidata-file-name
                                               size database-format))
         (database-handle (open-database database-file-name database-format 
                                         nil "rw+" #o644 'no-conversion-unix))
         (coding-system-for-read 'no-conversion-unix)
         (buffer-size 32768)
         (offset-start 0)
         (offset-end buffer-size)
         (range-information (make-range-table 'start-closed-end-closed))
         (range-staging (make-hash-table :test 'equal))
         (message "Initializing UnicodeData database cache: ")
         (loop-count 1)
         range-startinfo)
    (with-temp-buffer
      (progress-feedback-with-label 'describe-char-unicodedata-file
                                    "%s" 0 message)
      (while (progn
               (delete-region (point-min) (point-max))
               (insert-file-contents unidata-file-name nil
                                     offset-start offset-end)
               ;; If we've reached the end of the data, pass nil back to
               ;; the while loop test.
               (not (= (point-min) (point-max))))

        (when (= buffer-size (- (point-max) (point-min)))
          ;; If we're in the body of the file, and there's a trailing
          ;; incomplete end-line, delete it, and adjust offset-end
          ;; appropriately.
          (goto-char (point-max))
          (search-backward "\n")
          (forward-char)
          (delete-region (point) (point-max))
          (setq offset-end (+ offset-start (- (point) (point-min)))))

        (progress-feedback-with-label 'describe-char-unicodedata-file
                                      "%s" (truncate 
                                            (* (/ offset-start size) 100))
                                      (concat message
                                              (make-string
                                               (mod loop-count 39) ?.)))
        (incf loop-count)
        (goto-char (point-min))
        (while (re-search-forward 
                #r"^\([0-9A-F]\{4,6\}\);\([^;]*\);.*$" nil t)
          (cond
           ((and (> (- (match-end 2) (match-beginning 2)) 7)
                 (equal (substring (match-string 2) -7)
                        " First>"))
            ;; Start of a range. Save the start info in range-staging.
            (puthash (substring (match-string 2) 0 -7)
                     (list (string-to-int (match-string 1) 16)
                           (+ offset-start (1- (match-beginning 0))))
                     range-staging))
           ((and (> (- (match-end 2) (match-beginning 2)) 7)
                 (equal (substring (match-string 2) -6)
                        " Last>"))
            ;; End of a range. Combine with the start info, save it to the
            ;; range-information range table. 
            (setq range-startinfo
                  (gethash (substring (match-string 2) 0 -6) range-staging))
            (assert range-startinfo nil
                    "Unexpected order for range information.")
            (put-range-table 
             (first range-startinfo)
             (string-to-int (match-string 1) 16)
             (list (second range-startinfo) 
                   (+ offset-start (1- (match-end 0))))
             range-information)
            (remhash (substring (match-string 2) 0 -6) range-staging))
           (t
            ;; Normal character. Save the associated information in the
            ;; database directly.
            (put-database (match-string 1)
                          (format "(%d %d)"
                                  (+ offset-start (1- (match-beginning 0)))
                                  (+ offset-start (1- (match-end 0))))
                          database-handle))))
        (goto-char (point-min))
        (setq offset-start offset-end
              offset-end (+ buffer-size offset-end))))
    ;; Save the range information as such in the database. 
    (put-database "range-information"
                  (let ((print-readably t))
                    (prin1-to-string range-information))
                  database-handle) 
    (close-database database-handle)
    (progress-feedback-with-label 'describe-char-unicodedata-file
                                  "%s" 100 message)
    database-file-name))

(defun unidata-initialize-unihan-database (unihan-file-name)
  "Init the berkeley or gdbm lookup table for UNIHAN-FILE-NAME.

The table is a (non-SQL) database with information on the file offset of
each Unicode code point described in Unicode.org's Han character repository.
Unihan.txt (see `describe-char-unihan-file', the usual argument to this
function) is very large, and manipulating it directly can be tedious and
slow, so creating this cache makes it reasonable to display Unihan info in
the output of \\[universal-argument] \\[what-cursor-position] .  "
  (check-argument-type #'file-readable-p unihan-file-name)
  (unless unidata-database-format
    (error 'unimplemented "No (non-SQL) DB support available"))
  (let* ((database-format unidata-database-format)
         (size (eighth (file-attributes unihan-file-name)))
         (database-file-name
          (unidata-generate-database-file-name unihan-file-name
                                               size database-format))
         (database-handle (open-database database-file-name database-format 
                                         nil "rw+" #o644 'no-conversion-unix))
         (coding-system-for-read 'no-conversion-unix)
         (buffer-size 65536)
         (offset-start 0)
         (offset-end buffer-size)
         (message "Initializing Unihan database cache: ")
         (loop-count 1)
         trailing-unicode leading-unicode character-start character-end)
    (with-temp-buffer
      (progress-feedback-with-label 'describe-char-unihan-file
                                    "%s" 0 message)
      (while (progn
               (delete-region (point-min) (point-max))
               (insert-file-contents unihan-file-name nil
                                     offset-start offset-end)
               ;; If we've reached the end of the data, return nil to the
               ;; while.
               (not (= (point-min) (point-max))))

        (incf loop-count)
        (progress-feedback-with-label 'describe-char-unihan-file
                                      "%s" (truncate
                                            (* (/ offset-start size) 100))
                                      (concat message
                                              (make-string
                                               (mod loop-count 44) ?.)))
        (block 'dealing-with-chars
          (when (= buffer-size (- (point-max) (point-min)))
            ;; If we're in the body of the file, we need to delete the
            ;; character info for the last character, and set offset-end
            ;; appropriately. Otherwise, we may not be able to pick where
            ;; the actual description of a character ends and
            ;; begins. 
            ;;
            ;; This breaks if any single Unihan character description is
            ;; greater than the buffer size in length.
            (goto-char (point-max))
            (beginning-of-line)

            (when (< (- (point-max) (point)) (eval-when-compile
                                               (length "U+ABCDEF\t")))
              ;; If the character ID of the last line may have been cut off,
              ;; we need to delete all of that line here.
              (delete-region (point) (point-max))
              (forward-line -1))

            (when (looking-at "^\\(U\\+[0-9A-F]\\{4,6\\}\\)\t")
              (setq trailing-unicode (match-string 1)
                    trailing-unicode
                    (format "^%s\t" (regexp-quote trailing-unicode)))

              (end-of-line)

              ;; Go back until we hit a line that doesn't start with this
              ;; character info.
              (while (re-search-backward trailing-unicode nil t))

              ;; The re-search-backward failed, so point is still at the end
              ;; of the last match. Move to its beginning.
              (beginning-of-line)
              (delete-region (point) (point-max))
              (setq offset-end (+ offset-start (- (point) (point-min))))))
          (goto-char (point-min))
          (while t
            (when (= (point) (point-max))
              ;; We're at the end of this part of the file.
              (return-from 'dealing-with-chars))

            (unless (re-search-forward "^\\(U\\+[0-9A-F]\\{4,6\\}\\)\t"
                                     nil t)
              ;; We're probably in the comments at the start of the file. No
              ;; need to look for character info.
              (return-from 'dealing-with-chars))

            ;; Store where the character started. 
            (beginning-of-line)
            (setq character-start (point))

            (setq leading-unicode 
                  (format "^%s\t" (regexp-quote (match-string 1))))

            ;; Loop until we get past this entry.
            (while (re-search-forward leading-unicode nil t))

            ;; Now, store the information.
            (setq leading-unicode
                  (string-to-number (substring leading-unicode 3) 16)
                  leading-unicode (format "%04X" leading-unicode)
                  character-end (prog2 (end-of-line) (point)))
            (put-database leading-unicode
                          (format "(%d %d)"
                                  (+ offset-start (1- character-start))
                                  (+ offset-start (1- character-end)))
                          database-handle)
            (forward-line)))
        (setq offset-start offset-end
              offset-end (+ buffer-size offset-end))))
    (close-database database-handle)
    (progress-feedback-with-label 'describe-char-unihan-file
                                  "%s" 100
                                  message)
    database-file-name))
;; End XEmacs additions.

(defun describe-char-unicode-data (char)
  "Return a list of Unicode data for unicode CHAR.
Each element is a list of a property description and the property value.
The list is null if CHAR isn't found in `describe-char-unicodedata-file'."
  (when describe-char-unicodedata-file
    (unless (file-exists-p describe-char-unicodedata-file)
      (error 'file-error
             (format "`unicodedata-file' %s not found"
                     describe-char-unicodedata-file)))
    ;; XEmacs change; accept a character argument, use the cache if
    ;; appropriate.
    (when (characterp char)
      (setq char (encode-char char 'ucs)))
    (with-temp-buffer
      (if describe-char-use-cache
          ;; Use the database info.
          (let ((database-handle (open-database
                                  (unidata-generate-database-file-name
                                   describe-char-unicodedata-file
                                   (eighth (file-attributes
                                            describe-char-unicodedata-file))
                                   unidata-database-format)
                                  unidata-database-format
                                  nil "r"
                                  #o644 'no-conversion-unix))
                (coding-system-for-read 'no-conversion-unix)
                key lookup)
            (unless database-handle
              (error 'io-error 
                     (format "Could not open %s as a %s database"
                             (unidata-generate-database-file-name
                              describe-char-unicodedata-file
                              (eighth (file-attributes
                                       describe-char-unicodedata-file))
                              unidata-database-format)
                             unidata-database-format)))
            (setq key (format "%04X" char)
                  lookup (get-database key database-handle))
            (if lookup
                ;; Okay, we have information on that character in particular.
                (progn (setq lookup (read lookup))
                       (insert-file-contents describe-char-unicodedata-file nil
                                             (first lookup) (second lookup)))
              ;; No information on that character in particular. Do we have
              ;; range information? If so, load and check for our desired
              ;; character.
              (setq lookup (get-database "range-information" database-handle)
                    lookup (if lookup (read lookup))
                    lookup (if lookup (get-range-table char lookup)))
              (when lookup 
                (insert-file-contents describe-char-unicodedata-file nil
                                      (first lookup) (second lookup))))
            (close-database database-handle))

        ;; Otherwise, insert the whole file (the FSF approach).
        (set-buffer (get-buffer-create " *Unicode Data*"))
        (when (zerop (buffer-size))
          ;; Don't use -literally in case of DOS line endings.
          (insert-file-contents describe-char-unicodedata-file)))

      (goto-char (point-min))
      (let ((hex (format "%04X" char))
            found first last unihan-match unihan-info unihan-database-handle
            (coding-system-for-read 'no-conversion-unix))
        (if (re-search-forward (concat "^" hex) nil t)
            (setq found t)
	  ;; It's not listed explicitly.  Look for ranges, e.g. CJK
	  ;; ideographs, and check whether it's in one of them.
	  (while (and (re-search-forward "^\\([^;]+\\);[^;]+First>;" nil t)
		      (>= char (setq first
				     (string-to-number (match-string 1) 16)))
		      (progn
			(forward-line 1)
			(looking-at "^\\([^;]+\\);[^;]+Last>;")
			(> char
			   (setq last
				 (string-to-number (match-string 1) 16))))))
	  (if (and first (>= char first)
		   last (<= char last))
	      (setq found t)))
	(if found
	    (let ((fields (mapcar (lambda (elt)
				    (if (> (length elt) 0)
					elt))
				  (cdr (split-string
					(buffer-substring
					 (line-beginning-position)
					 (line-end-position))
					";")))))
	      ;; The length depends on whether the last field was empty.
	      (unless (or (= 13 (length fields))
			  (= 14 (length fields)))
		(error 'invalid-argument
                       (format "Invalid contents in %s"
                               describe-char-unicodedata-file)))
	      ;; The field names and values lists are slightly
	      ;; modified from Mule-UCS unidata.el.
	      (apply #'list
	       (list "Name" (let ((name (nth 0 fields)))
			      ;; Check for <..., First>, <..., Last>
			      (if (string-match "\\`\\(<[^,]+\\)," name)
				  (concat (match-string 1 name) ">")
				name)))
	       (list "Category"
		     (cdr (assoc
			   (nth 1 fields)
			   '(("Lu" . "uppercase letter")
			     ("Ll" . "lowercase letter")
			     ("Lt" . "titlecase letter")
			     ("Mn" . "non-spacing mark")
			     ("Mc" . "spacing-combining mark")
			     ("Me" . "enclosing mark")
			     ("Nd" . "decimal digit")
			     ("Nl" . "letter number")
			     ("No" . "other number")
			     ("Zs" . "space separator")
			     ("Zl" . "line separator")
			     ("Zp" . "paragraph separator")
			     ("Cc" . "other control")
			     ("Cf" . "other format")
			     ("Cs" . "surrogate")
			     ("Co" . "private use")
			     ("Cn" . "not assigned")
			     ("Lm" . "modifier letter")
			     ("Lo" . "other letter")
			     ("Pc" . "connector punctuation")
			     ("Pd" . "dash punctuation")
			     ("Ps" . "open punctuation")
			     ("Pe" . "close punctuation")
			     ("Pi" . "initial-quotation punctuation")
			     ("Pf" . "final-quotation punctuation")
			     ("Po" . "other punctuation")
			     ("Sm" . "math symbol")
			     ("Sc" . "currency symbol")
			     ("Sk" . "modifier symbol")
			     ("So" . "other symbol")))))
	       (list "Combining class"
		     (cdr (assoc
			   (string-to-number (nth 2 fields))
			   '((0 . "Spacing")
			     (1 . "Overlays and interior")
			     (7 . "Nuktas")
			     (8 . "Hiragana/Katakana voicing marks")
			     (9 . "Viramas")
			     (10 . "Start of fixed position classes")
			     (199 . "End of fixed position classes")
			     (200 . "Below left attached")
			     (202 . "Below attached")
			     (204 . "Below right attached")
			     (208 . "Left attached (reordrant around \
single base character)")
			     (210 . "Right attached")
			     (212 . "Above left attached")
			     (214 . "Above attached")
			     (216 . "Above right attached")
			     (218 . "Below left")
			     (220 . "Below")
			     (222 . "Below right")
			     (224 . "Left (reordrant around single base \
character)")
			     (226 . "Right")
			     (228 . "Above left")
			     (230 . "Above")
			     (232 . "Above right")
			     (233 . "Double below")
			     (234 . "Double above")
			     (240 . "Below (iota subscript)")))))
	       (list "Bidi category"
		     (cdr (assoc
			   (nth 3 fields)
			   '(("L" . "Left-to-Right")
			     ("LRE" . "Left-to-Right Embedding")
			     ("LRO" . "Left-to-Right Override")
			     ("R" . "Right-to-Left")
			     ("AL" . "Right-to-Left Arabic")
			     ("RLE" . "Right-to-Left Embedding")
			     ("RLO" . "Right-to-Left Override")
			     ("PDF" . "Pop Directional Format")
			     ("EN" . "European Number")
			     ("ES" . "European Number Separator")
			     ("ET" . "European Number Terminator")
			     ("AN" . "Arabic Number")
			     ("CS" . "Common Number Separator")
			     ("NSM" . "Non-Spacing Mark")
			     ("BN" . "Boundary Neutral")
			     ("B" . "Paragraph Separator")
			     ("S" . "Segment Separator")
			     ("WS" . "Whitespace")
			     ("ON" . "Other Neutrals")))))
	       (list
		"Decomposition"
		(if (nth 4 fields)
		    (let* ((parts (split-string (nth 4 fields)))
			   (info (car parts)))
		      (if (string-match "\\`<\\(.+\\)>\\'" info)
			  (setq info (match-string 1 info))
			(setq info nil))
		      (if info (setq parts (cdr parts)))
		      ;; Maybe printing ? for unrepresentable unicodes
		      ;; here and below should be changed?
		      (setq parts (mapconcat
				   (lambda (arg)
				     (string (or (decode-char
						  'ucs
						  (string-to-number arg 16))
						 ??)))
				   parts " "))
		      (concat info parts))))
	       (list "Decimal digit value"
		     (nth 5 fields))
	       (list "Digit value"
		     (nth 6 fields))
	       (list "Numeric value"
		     (nth 7 fields))
	       (list "Mirrored"
		     (if (equal "Y" (nth 8 fields))
			 "yes"))
	       (list "Old name" (nth 9 fields))
	       (list "ISO 10646 comment" (nth 10 fields))
	       (list "Uppercase" (and (nth 11 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 11 fields) 16))
						  ??))))
	       (list "Lowercase" (and (nth 12 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 12 fields) 16))
						  ??))))
	       (list "Titlecase" (and (nth 13 fields)
				      (string (or (decode-char
						   'ucs
						   (string-to-number
						    (nth 13 fields) 16))
						  ??))))

               ;; XEmacs addition.
               ;; If we're aware the character is a Han character, provide
               ;; the Unihan information, or tell the user that it's not
               ;; available.
               (if (and (> (length (nth 0 fields)) 13)
                        (equal "<CJK Ideograph"
                               (substring (nth 0 fields) 0 14)))
                   (if (and describe-char-unihan-file
                            (setq unihan-database-handle
                                  (open-database
                                   (unidata-generate-database-file-name
                                    describe-char-unihan-file
                                    (eighth (file-attributes
                                             describe-char-unihan-file))
                                    unidata-database-format)
                                   unidata-database-format
                                   nil "r" #o644 'no-conversion-unix))
                            (setq unihan-match
                                  (get-database (format "%04X" char)
                                                unihan-database-handle)
                                  unihan-match
                                  (and unihan-match (read unihan-match))))
                       (with-temp-buffer
                         (insert-file-contents describe-char-unihan-file
                                               nil (first unihan-match)
                                               (second unihan-match))
                         (goto-char (point-min))
                         (while (re-search-forward
                                 "^U\\+[0-9A-F]+\t\\(k[^\t]+\\)\t\\(.*\\)$"
                                 nil t)
                           (push
                            (list
                             (or (gethash
                                  (match-string 1)
                                  describe-char-unihan-field-descriptions)
                                 (match-string 1))
                             (decode-coding-string (match-string 2) 'utf-8))
                            unihan-info))
                         (close-database unihan-database-handle)
                         unihan-info)
                     ;; It's a Han character, but Unihan.txt is not
                     ;; available. Tell the user.
                     (list
                      '("Unihan"
                        "No Unihan information available; is \
`describe-char-unihan-file' set, and its cache initialized?")))))))))))

;; Return information about how CHAR is displayed at the buffer
;; position POS.  If the selected frame is on a graphic display,
;; return a cons (FONTNAME . GLYPH-CODE).  Otherwise, return a string
;; describing the terminal codes for the character.
(defun describe-char-display (pos char)
  (let* ((frame (selected-frame))
         (charset (char-charset char))
         (ccl (or (and (charset-property charset 'encode-as-utf-8)
                       ccl-encode-to-ucs-2)
                  (charset-property charset 'ccl-program)))
         (ccl-vector (make-vector 8 0)))
    (if (display-graphic-p (selected-frame))
        (list
         (font-instance-name
          (face-font-instance (or (get-char-property pos 'face)
                                  'default)
                              (selected-window)
                              charset))
         (cond 
          ((and ccl (eq 'x (frame-type frame)))
           (setq char (split-char char))
           (aset ccl-vector 0 (charset-id charset))
           (aset ccl-vector 1 (second char))
           (if (= 2 (charset-dimension charset))
               (aset ccl-vector 2 (third char)))
           (ccl-execute ccl ccl-vector)
           (if (= 2 (charset-dimension charset))
               (logior (lsh (aref ccl-vector 1) 8)
                       (aref ccl-vector 2))
             (aref ccl-vector 1)))
          ;; #### We don't handle the X case where redisplay falls back to an
          ;; ISO 10646-1 font at runtime.
          ((eq 'x (frame-type frame))
           (if (= 2 (charset-dimension charset))
               (prog2
                   (setq char (split-char char)) 
                   (logior (lsh (second char) 8)
                           (third char)))
             (second (split-char char))))
          ;; Otherwise we assume we're using Unicode.
          (t
           (encode-char char 'ucs))))
      (let* ((coding (console-tty-output-coding-system (device-console)))
             (encoded (encode-coding-string char coding)))
        (if encoded
            (format "%s, coding system %s"
                    (encoded-string-description encoded coding)
                    (coding-system-name coding)))))))


;;;###autoload
(defun describe-char (pos)
  "Describe the character after POS (interactively, the character after point).
The information includes character code, charset and code points in it,
syntax, category, how the character is encoded in a file,
character composition information (if relevant),
as well as widgets, buttons, overlays, and text properties."
  (interactive "d")
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let* ((char (char-after pos))
	 (charset (char-charset char))
	 (composition (find-composition pos nil nil t))
	 (component-chars nil)
	 (display-table
          (specifier-instance current-display-table (selected-window)))
	 (disp-table-entry (and display-table
                                (get-display-table char display-table)))
	 (extents (mapcar #'(lambda (o) (extent-properties o))
			   (extents-at pos)))
	 (char-description (single-key-description char))
	 (text-props-desc
	  (let ((tmp-buf (generate-new-buffer " *text-props*")))
	    (unwind-protect
		(progn
		  (describe-text-properties pos tmp-buf)
		  (with-current-buffer tmp-buf (buffer-string)))
	      (kill-buffer tmp-buf))))
	 item-list max-width unicode unicode-formatted unicode-error
	 unicodedata (max-unicode-description-width (- (window-width) 50)))


    (setq unicode-error
          ;; XEmacs change, check does the character represent a Unicode
          ;; error sequence.
          (get-char-table char unicode-error-default-translation-table)
          unicode (and (not unicode-error) (encode-char char 'ucs))
          unicode-formatted (if unicode-error
                                (format
                                 "Invalid Unicode sequence, ?\x%02x on disk"
                                 unicode-error)
                              (if (and unicode (natnump unicode))
                                  (format (if (> unicode #xFFFF)
                                              "U+%06X" "U+%04X")
                                          unicode)
                                ""))
          item-list
	  `(("character"
	     ,(format "%s (%s, %d, #o%o, #x%x)"
		      (apply 'propertize char-description
			     (text-properties-at pos))
                      unicode-formatted
                      char 
                      char 
		      char))
 	    ("charset"
 	     ,(lexical-let 
                   ((charset-name (symbol-name charset)))
                `(progn
                  (widget-create 'push-button
                                 :notify ,(lambda (&rest ignored-arg)
                                            (with-displaying-help-buffer
                                             (lambda nil
                                               (charset-description
                                                (intern charset-name)))
                                             charset-name))
                                 ,charset-name)
                  (widget-insert (format " (%s)" (charset-description
                                                 ',charset))))))
	    ("code point"
	     ,(let ((split (split-char char)))
		`(widget-create 'push-button
;                  :notify
;                  ,(lambda (&rest ignored-arg)
;                     (with-selected-wind
; insert-gui-button
;                  (make-gui-button
                  ,(if (= (charset-dimension charset) 1)
                       (format "#x%02X" (nth 1 split))
                     (format "#x%02X #x%02X" (nth 1 split)
                             (nth 2 split))))))
	    ("syntax"
	     ,(let ((syntax
                     (syntax-string-to-code (string (syntax-after pos)))))
		(with-temp-buffer
		  (describe-syntax-code syntax (current-buffer))
                  ;; Remove the newline. 
                  (delete-backward-char)
		  (buffer-string))))
            ;; XEmacs; #### add category support. 
; 	    ("category"
; 	     ,@(let ((category-set (char-category-set char)))
; 		 (if (not category-set)
; 		     '("-- none --")
; 		   (mapcar #'(lambda (x) (format "%c:%s"
; 						 x (category-docstring x)))
; 			   (category-set-mnemonics category-set)))))
; 	    ,@(let ((props (get-char-table char char-code-property-table))
; 		    ps)
; 		(when props
; 		  (while props
; 		    (push (format "%s:" (pop props)) ps)
; 		    (push (format "%s;" (pop props)) ps))
; 		  (list (cons "Properties" (nreverse ps)))))
	    ("to input"
	     ,@(let ((key-list (and-fboundp #'quail-find-key
                                 current-input-method
                                 (quail-find-key char))))
		 (if (consp key-list)
		     (list "type"
			   (mapconcat #'(lambda (x) (concat "\"" x "\""))
				      key-list " or ")
			   "with"
			   `(insert-text-button
			     ,current-input-method
			     'type 'help-input-method
			     'help-args '(,current-input-method))))))
;	    ("buffer code"
;	     ,(encoded-string-description
;	       (string-as-unibyte (char-to-string char) nil))
	    ("file code"
	     ,@(let* ((coding buffer-file-coding-system)
                      ;; ### XEmacs; use encode-coding-char once
                      ;; merged. 
		      (encoded (encode-coding-string char coding)))
		 (if encoded
		     (list (encoded-string-description encoded coding)
			   (format "(encoded by coding system %S)"
                                   (coding-system-name coding)))
		   (list "not encodable by coding system"
			 (coding-system-name coding)))))
	    ("display"
	     ,(cond
	       (disp-table-entry
                ;; XEmacs change; just use the print syntax of the display
                ;; table entry. Might be possible to improve this, but
                ;; nothing occurs to me right now.
		(format "by display table entry [%S] " disp-table-entry))
	       (composition
		(let ((from (car composition))
		      (to (nth 1 composition))
		      (next (1+ pos))
		      (components (nth 2 composition))
		      ch)
		  (setcar composition
			  (and (< from pos) (buffer-substring from pos)))
		  (setcar (cdr composition)
			  (and (< next to) (buffer-substring next to)))
		  (dotimes (i (length components))
		    (if (integerp (setq ch (aref components i)))
			(push (cons ch (describe-char-display pos ch))
			      component-chars)))
		  (setq component-chars (nreverse component-chars))
		  (format "composed to form \"%s\" (see below)"
			  (buffer-substring from to))))
	       (t
		(let ((display (describe-char-display pos char)))
		  (if (display-graphic-p (selected-frame))
		      (if display
			  (concat
			   "by this font (glyph code)\n"
			   (format "     %s (#x%02X)"
				   (first display) (second display)))
			"no font available")
		    (if display
			(format "terminal code %s" display)
		      "not encodable for terminal"))))))
	    ,@(let ((face
		     (if (not (or disp-table-entry composition))
			 (cond
                          ;; XEmacs #### Implement this. 
;			  ((and show-trailing-whitespace
;				(save-excursion (goto-char pos)
;						(looking-at "[ \t]+$")))
;			   'trailing-whitespace)
;			  ((and nobreak-char-display unicode (eq unicode '#xa0))
;			   'nobreak-space)
;			  ((and nobreak-char-display unicode (eq unicode '#xad))
;			   'escape-glyph)
			  ((and (< char 32) (not (memq char '(9 10))))
			   'escape-glyph)))))
		(if face (list (list "hardcoded face"
				     `(insert-gui-button
                                       (make-gui-button
                                        ,(symbol-name face)))))))
	    ,@(progn 
		(setq unicodedata (and unicode
				       (describe-char-unicode-data unicode)))
		(if unicodedata
		    (cons (list "Unicode data" " ") unicodedata)))))
    (setq max-width (apply #'max (mapcar #'(lambda (x)
					     (if (cadr x) (length (car x)) 0))
					 item-list)))
    (when (and unicodedata (> max-width max-unicode-description-width))
      (setq max-width max-unicode-description-width)
      (with-temp-buffer
	(let ((fill-column max-unicode-description-width)
	      (indent-tabs-mode nil))
	  (dolist (unidata-line unicodedata)
	    (when (cadr unidata-line)
	      (setf (car unidata-line)
		    (progn (insert (car unidata-line))
			   (goto-char (point-min))
			   (fill-paragraph 'right)
			   (delete-region (1- (point-max))
					  (point-max))
			   (buffer-string)))
	      (delete-region (point-min) (point-max)))))))
    ; (help-setup-xref nil (interactive-p))
    (with-displaying-help-buffer
     (lambda ()
       (with-current-buffer standard-output
         ; (set-buffer-multibyte multibyte-p)
         (let ((formatter (format "%%%ds:" max-width)))
           (dolist (elt item-list)
             (when (cadr elt)
               (insert (format formatter (car elt)))
               (dolist (clm (cdr elt))
                 (if (consp clm)
                     (progn (insert " ") (eval clm))
                   (when (>= (+ (current-column)
                                (or (string-match "\n" clm)
                                    (string-width clm))
                                1)
                             (window-width))
                     (insert "\n")
                     (indent-to (1+ max-width)))
                   (insert " " clm)))
               (insert "\n"))))

         (when extents
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "character:[ \t\n]+")
             (let* ((end (+ (point) (length char-description))))
               (mapc #'(lambda (props)
                         (let ((o (make-extent (point) end)))
                           (while props
                             (set-extent-property o (car props) (nth 1 props))
                             (setq props (cddr props)))))
                     extents))))

         ;; XEmacs change; don't give GUI- or TTY-specific detail about the
         ;; display table entry, the #'specifier-instance call above dealt
         ;; with that.
         ; (when disp-table-entry ...)

         ;; XEmacs; this doesn't work now. 
         (when composition
           (insert "\nComposed")
           (if (car composition)
               (if (cadr composition)
                   (insert " with the surrounding characters \""
                           (car composition) "\" and \""
                           (cadr composition) "\"")
                 (insert " with the preceding character(s) \""
                         (car composition) "\""))
             (if (cadr composition)
                 (insert " with the following character(s) \""
                         (cadr composition) "\"")))
           (insert " by the rule:\n\t("
                   (mapconcat (lambda (x)
                                (format (if (consp x) "%S" "?%c") x))
                              (nth 2 composition)
                              " ")
                   ")")
           (insert  "\nThe component character(s) are displayed by ")
           ;; XEmacs #### Once composition is in place, this should be 
           ;; a (font-instance-name (face-font-instance [...])) call. 
           (if (display-graphic-p (selected-frame))
               (progn
                 (insert "these fonts (glyph codes):")
                 (dolist (elt component-chars)
                   (insert "\n " (car elt) ?:
                           (propertize " " 'display '(space :align-to 5))
                           (if (cdr elt)
                               (format "%s (#x%02X)" (cadr elt) (cddr elt))
                             "-- no font --"))))
             (insert "these terminal codes:")
             (dolist (elt component-chars)
               (insert "\n  " (car elt) ":"
                       (propertize " " 'display '(space :align-to 5))
                       (or (cdr elt) "-- not encodable --"))))
           (insert "\nSee the variable `reference-point-alist' for "
                   "the meaning of the rule.\n"))

         (if text-props-desc (insert text-props-desc))
;        (setq help-xref-stack-item (list 'help-insert-string (buffer-string)))
         (toggle-read-only 1)
         (print-help-return-message)))
       (format "Describe %c <%d>" (char-after pos) pos))))

(defalias 'describe-char-after 'describe-char)
(make-obsolete 'describe-char-after 'describe-char "22.1")

(provide 'descr-text)

;; arch-tag: fc55a498-f3e9-4312-b5bd-98cc02480af1
;;; descr-text.el ends here
