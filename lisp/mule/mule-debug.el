;;; mule-diag.el --- debugging functions for Mule.

;; Copyright (C) 1992 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; 93.7.28  created for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>

;;; General utility function

(defun mule-debug-princ-list (&rest args)
  (while (cdr args)
    (if (car args)
	(progn (princ (car args)) (princ " ")))
    (setq args (cdr args)))
  (princ (car args))
  (princ "\n"))

;;; character sets

;;;###autoload
(defun list-charsets ()
  "Display a list of existing character sets."
  (interactive)
  (with-output-to-temp-buffer "*Charset List*"
    (princ "## LIST OF CHARACTER SETS\n")
    (princ
     "NAME                 REGISTRY        BYTES CHARS FINAL GRAPHIC DIR\n")
    (princ
     "--------------------------------------------------------------------")
    (dolist (charset (charset-list))
      (setq charset (get-charset charset))
      (princ (format
	      "%20s %15s %5d %5d %5d %7d %s\n"
	      (charset-name charset)
	      (charset-registry  charset)
	      (charset-dimension charset)
	      (charset-chars     charset)
	      (charset-final     charset)
	      (charset-graphic   charset)
	      (charset-direction charset)))
      (princ "        ")
      (princ "%s\n" (charset-doc-string charset)))))

;    (princ "## CCL PROGRAM TO CONVERT INTERNAL TO EXTERNAL CODE\n")
;    (princ "NAME                 CCL-PROGRAMS\n")
;    (mapcar
;     (lambda (name)
;       (let ((ccl (charset-ccl-program name)))
;	 (if ccl
;	     (let ((i 0) (len (length ccl)))
;	       (princ (format "%20s " name))
;	       (while (< i len)
;		 (princ (format " %x" (aref ccl i)))
;		 (setq i (1+ i)))
;	       (princ "\n")))))
;     (charset-list))
;    ))

(defun describe-designation (cs register)
  (let ((charset
	 (coding-system-property
	  cs (intern (format "charset-g%d" register))))
	(force
	 (coding-system-property
	  cs (intern (format "force-g%d-on-output" register)))))
    (princ
     (format
      "  G%d: %s%s\n"
      register
      (cond ((null charset) "never used")
	    ((eq t charset) "none")
	    (t (charset-name charset)))
      (if force " (explicit designation required)" "")))))
	
;;;###autoload
(defun describe-coding-system (cs)
  "Display documentation of the coding-system CS."
  (interactive "zCoding-system: ")
  (setq cs (get-coding-system cs))
  (with-output-to-temp-buffer "*Help*"
    (princ (format "Coding-system %s [%s]:\n"
		   (coding-system-name cs)
		   (coding-system-mnemonic cs)))
    (princ (format "  %s\n" (coding-system-doc-string cs)))
    (let ((type (coding-system-type cs)))
      (princ "Type: ") (princ type) (terpri)
      (case type
	('iso2022
	 (princ "\nInitial designations:\n")
	 (dolist (register '(0 1 2 3))
	   (describe-designation cs register))
	 (princ "\nOther properties: \n")
	 (dolist (prop '(short no-ascii-eol no-ascii-cntl seven lock-shift no-iso6429))
	   (princ (format "  %s: " (symbol-name prop)))
	   (princ (coding-system-property cs prop))
	   (terpri)))
	 ;;(princ "  short: ") (princ (coding-system-short))
	 ;;(princ (if (aref flags 4) "ShortForm" "LongForm"))
	 ;;(if (aref flags 5) (princ ", ASCII@EOL"))
	 ;;(if (aref flags 6) (princ ", ASCII@CNTL"))
	 ;;(princ (if (coding-system-seven cs) ", 7bit" ", 8bit"))
	 ;;(if (aref flags 8) (princ ", UseLockingShift"))
	 ;;(if (aref flags 9) (princ ", UseRoman"))
	 ;;(if (aref flags 10) (princ ", UseOldJIS"))
	 ;;(if (aref flags 11) (princ ", No ISO6429"))
	 ;;(terpri))
	
	('big5
	 ;;(princ (if flags "Big-ETen\n" "Big-HKU\n")))
	 ))
      (princ (format "\nEOL-Type: %s\n"
		     (case (coding-system-eol-type cs)
		       ('nil   "null (= LF)")
		       ('lf   "LF")
		       ('crlf "CRLF")
		       ('cr   "CR")
		       (t     "invalid"))))
      )))

;;;###autoload
(defun list-coding-system-briefly ()
  "Display coding-systems currently used with a brief format in mini-buffer."
  (interactive)
  (let ((cs (and (fboundp 'process-coding-system) (process-coding-system)))
	eol-type)
    (message
     "current: [FKDPp=%c%c%c%c%c%c%c%c] default: [FPp=%c%c%c%c%c%c]"
     (coding-system-mnemonic buffer-file-coding-system)
     (coding-system-eol-mnemonic buffer-file-coding-system)
     (coding-system-mnemonic keyboard-coding-system)
     (coding-system-mnemonic terminal-coding-system)
     (coding-system-mnemonic (car cs))
     (coding-system-eol-mnemonic (car cs))
     (coding-system-mnemonic (cdr cs))
     (coding-system-eol-mnemonic (cdr cs))
     (coding-system-mnemonic (default-value 'buffer-file-coding-system))
     (coding-system-eol-mnemonic (default-value 'buffer-file-coding-system))
     (coding-system-mnemonic (car default-process-coding-system))
     (coding-system-eol-mnemonic (car default-process-coding-system))
     (coding-system-mnemonic (cdr default-process-coding-system))
     (coding-system-eol-mnemonic (cdr default-process-coding-system))
     )))

(defun princ-coding-system (code)
  (princ ": ")
  (princ code)
  (princ " [")
  (princ (char-to-string (coding-system-mnemonic code)))
  (princ (char-to-string (coding-system-eol-mnemonic code)))
  (princ "]\n"))

(defun todigit (flags idx &optional default-value)
  (if (aref flags idx)
      (if (numberp (aref flags idx)) (aref flags idx) 1)
    (or default-value 0)))

(defun print-coding-system-description (code)
  (let ((type (get-code-type code))
	(eol (or (get-code-eol code) 1))
	(flags (get-code-flags code))
	line)
    (setq type
	  (cond ((null type) 0)
		((eq type t) 2)
		((eq type 0) 1)
		((eq type 1) 3)
		((eq type 2) 4)
		((eq type 3) 5)
		((eq type 4) 6)
		(t nil)))
    (if (or (null type)
	    (get code 'post-read-conversion)
	    (get (get-base-code code) 'post-read-conversion)
	    (get code 'pre-write-conversion)
	    (get (get-base-code code) 'pre-write-conversion)
	    (eq code '*noconv*))
	nil
      (princ
       (format "%s:%d:%c:"
	       code type (coding-system-mnemonic code)))
      (princ (format "%d" (if (numberp eol) eol 0)))
      (cond ((= type 4)
	     (princ
	      (format
	       ":%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"
	       (todigit flags 0 -1)
	       (todigit flags 1 -1)
	       (todigit flags 2 -1)
	       (todigit flags 3 -1)
	       (todigit flags 4)
	       (todigit flags 5)
	       (todigit flags 6)
	       (todigit flags 7)
	       (todigit flags 8)
	       (todigit flags 9)
	       (todigit flags 10)
	       (todigit flags 11))))
	    ((= type 5)
	     (princ ":0"))
	    ((= type 6)
	     (if (and (vectorp (car flags)) (vectorp (cdr flags)))
		 (let (i len)
		   (princ ":")
		   (setq i 0 len (length (car flags)))
		   (while (< i len)
		     (princ (format " %x" (aref (car flags) i)))
		     (setq i (1+ i)))
		   (princ ",")
		   (setq i 0 len (length (cdr flags)))
		   (while (< i len)
		     (princ (format " %x" (aref (cdr flags) i)))
		     (setq i (1+ i))))))
	    (t (princ ":0")))
      (princ ":")
      (princ (get-code-document code))
      (princ "\n"))
    ))

;;;###autoload
(defun list-coding-system (&optional all)
  "Describe coding-systems currently used with a detailed format.
If optional arg ALL is non-nil, all coding-systems are listed in
machine readable simple format."
  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (if (null all)
	(let ((cs (and (fboundp 'process-coding-system)
		       (process-coding-system))))
	  (princ "Current:\n  buffer-file-coding-system")
	  (princ-coding-system buffer-file-coding-system)
	  (princ "  keyboard-coding-system")
	  (princ-coding-system keyboard-coding-system)
	  (princ "  terminal-coding-system")
	  (princ-coding-system terminal-coding-system)
	  (when cs
	    (princ "  process-coding-system (input)")
	    (princ-coding-system (car cs))
	    (princ "  process-coding-system (output)")
	    (princ-coding-system (cdr cs)))
	  (princ "Default:\n  buffer-file-coding-system")
	  (princ-coding-system (default-value 'buffer-file-coding-system))
	  (princ "  process-coding-system (input)")
	  (princ-coding-system (car default-process-coding-system))
	  (princ "  process-coding-system (output)")
	  (princ-coding-system (cdr default-process-coding-system))
	  (princ "Others:\n  buffer-file-coding-system-for-read")
	  (princ-coding-system buffer-file-coding-system-for-read)
	  (princ "Coding categories by priority:\n")
	  (princ (coding-priority-list)))
      (princ "########################\n")
      (princ "## LIST OF CODING SYSTEM\n")
      (princ "## NAME(str):TYPE(int):MNEMONIC(char):EOL(int):FLAGS:DOC(str)\n")
      (princ "##  TYPE = 0(no conversion),1(auto conversion),\n")
      (princ "##         2(Mule internal),3(SJIS),4(ISO2022),5(BIG5),6(CCL)\n")
      (princ "##  EOL = 0(AUTO), 1(LF), 2(CRLF), 3(CR)\n")
      (princ "##  FLAGS =\n")
      (princ "##    if TYPE = 4 then\n")
      (princ "##        G0,G1,G2,G3,SHORT,ASCII-EOL,ASCII-CNTL,SEVEN,\n")
      (princ "##        LOCK-SHIFT,USE-ROMAN,USE-OLDJIS\n")
      (princ "##    else if TYPE = 6 then\n")
      (princ "##        CCL_PROGRAM_FOR_READ,CCL_PROGRAM_FOR_WRITE\n")
      (princ "##    else\n")
      (princ "##        0\n")
      (princ "##\n")
      (let ((codings nil))
	(mapatoms
	 (function
	  (lambda (arg)
	    (if (eq arg '*noconv*)
		nil
	      (if (and (or (vectorp (get arg 'coding-system))
			   (vectorp (get arg 'eol-type)))
		       (null (get arg 'pre-write-conversion))
		       (null (get arg 'post-read-conversion)))
		  (setq codings (cons arg codings)))))))
	(while codings
	  (print-coding-system-description (car codings))
	  (setq codings (cdr codings))))
      (princ "############################\n")
      (princ "## LIST OF CODING CATEGORIES (ordered by priority)\n")
      (princ "## CATEGORY(str):CODING-SYSTEM(str)\n")
      (princ "##\n")
      (princ (coding-priority-list))
      )))

;;; FONT
(defun describe-font-internal (fontinfo &optional verbose)
  (let ((cs (character-set (aref fontinfo 3))))
    (mule-debug-princ-list (format "Font #%02d for" (aref fontinfo 0))
		(nth 6 cs) (nth 7 cs) "--"
		(cond ((= (aref fontinfo 4) 0) "NOT YET OPENED")
		      ((= (aref fontinfo 4) 1) "OPENED")
		      (t "NOT FOUND")))
    (mule-debug-princ-list "  request:" (aref fontinfo 1))
    (if (= (aref fontinfo 4) 1)
	(mule-debug-princ-list "   opened:" (aref fontinfo 2)))
    (if (and verbose (= (aref fontinfo 4) 1))
	(progn
	  (mule-debug-princ-list "     size:" (format "%d" (aref fontinfo 5)))
	  (mule-debug-princ-list " encoding:" (if (= (aref fontinfo 6) 0) "low" "high"))
	  (mule-debug-princ-list "  yoffset:" (format "%d" (aref fontinfo 7)))
	  (mule-debug-princ-list "  rel-cmp:" (format "%d" (aref fontinfo 8)))))
    ))

;;;###autoload
(defun describe-font (fontname)
  "Display information about fonts which partially match FONTNAME."
  (interactive "sFontname: ")
  (setq fontname (regexp-quote fontname))
  (with-output-to-temp-buffer "*Help*"
    (let ((fontlist (font-list)) fontinfo)
      (while fontlist
	(setq fontinfo (car fontlist))
	(if (or (string-match fontname (aref fontinfo 1))
		(and (aref fontinfo 2)
		     (string-match fontname (aref fontinfo 2))))
	    (describe-font-internal fontinfo 'verbose))
	(setq fontlist (cdr fontlist))))))

;;;###autoload
(defun list-font ()
  "Display a list of fonts."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let ((fontlist (font-list)))
      (while fontlist
	(describe-font-internal (car fontlist))
	(setq fontlist (cdr fontlist))))))

;;; FONTSET
(defun describe-fontset-internal (fontset-info)
  (mule-debug-princ-list "### Fontset-name:" (car fontset-info) "###")
  (let ((i 0) font)
    (while (< i 128)
      (if (>= (setq font (aref (cdr fontset-info) i)) 0)
	  (describe-font-internal (get-font-info font)))
      (setq i (1+ i)))))

;;;###autoload
(defun describe-fontset (fontset)
  "Display information about FONTSET."
  (interactive
   (let ((fontset-list (mapcar '(lambda (x) (list x)) (fontset-list))))
     (list (completing-read "Fontset: " fontset-list nil 'match))))
  (let ((fontset-info (get-fontset-info fontset)))
    (if fontset-info
	(with-output-to-temp-buffer "*Help*"
	  (describe-fontset-internal fontset-info))
      (error "No such fontset: %s" fontset))))

;;;###autoload
(defun list-fontset ()
  "Display a list of fontsets."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (let ((fontsetlist (fontset-list 'all)))
      (while fontsetlist
	(describe-fontset-internal (car fontsetlist))
	(setq fontsetlist (cdr fontsetlist))))))

;;; DIAGNOSIS

(defun insert-list (args)
  (while (cdr args)
    (insert (or (car args) "nil") " ")
    (setq args (cdr args)))
  (if args (insert (or (car args) "nil")))
  (insert "\n"))

(defun insert-section (sec title)
  (insert "########################################\n"
	  "# Section " (format "%d" sec) ".  " title "\n"
	  "########################################\n\n"))

;;;###autoload
(defun mule-diag ()
  "Show diagnosis of the current running Mule."
  (interactive)
  (let ((buf (get-buffer-create "*Diagnosis*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert "\t##############################\n"
	      "\t### DIAGNOSIS OF YOUR MULE ###\n"
	      "\t##############################\n\n"
	      "CONTENTS: Section 0.  General information\n"
	      "          Section 1.  Display\n"
	      "          Section 2.  Input methods\n"
	      "          Section 3.  Coding-systems\n"
	      "          Section 4.  Character sets\n")
      (if window-system
	  (insert "          Section 5.  Fontset list\n"))
      (insert "\n")

      (insert-section 0 "General information")
      (insert "Mule's version: " mule-version " of " mule-version-date "\n")
      (if window-system
	  (insert "Window-system: "
		  (symbol-name window-system)
		  (format "%s" window-system-version))
	(insert "Terminal: " (getenv "TERM")))
      (insert "\n\n")

      (insert-section 1 "Display")
      (if (eq window-system 'x)
	  (let* ((alist (nth 1 (assq (selected-frame)
				     (current-frame-configuration))))
		 (fontset (cdr (assq 'font alist))))
	    (insert-list (cons "Defined fontsets:" (fontset-list)))
	    (insert "Current frame's fontset: " fontset "\n"
		    "See Section 5 for more detail.\n\n"))
	(insert "Coding system for output to terminal: "
		(symbol-name terminal-coding-system)
		"\n\n"))
      (insert-section 2 "Input methods")
      (if (featurep 'egg)
	  (let (temp)
	    (insert "EGG (Version " egg-version ")\n")
	    (insert "  jserver host list: ")
	    (insert-list (if (boundp 'jserver-list) jserver-list
			   (if (setq temp (getenv "JSERVER"))
			       (list temp))))
	    (insert "  cserver host list: ")
	    (insert-list (if (boundp 'cserver-list) cserver-list
			   (if (setq temp (getenv "CSERVER"))
			       (list temp))))
	    (insert "  loaded ITS mode:\n\t")
	    (insert-list (mapcar 'car its:*mode-alist*))
	    (insert "  current server:" (symbol-name wnn-server-type) "\n"
		    "  current ITS mode:"
		    (let ((mode its:*mode-alist*))
		      (while (not (eq (cdr (car mode)) its:*current-map*))
			(setq mode (cdr mode)))
		      (car (car mode))))
	    (insert "\n")))
      (insert "QUAIL (Version " quail-version ")\n")
      (insert "  Quail packages: (not-yet-loaded) [current]\n\t")
      (let ((l quail-package-alist)
	    (current (or (car quail-current-package) "")))
	(while l
	  (cond ((string= current (car (car l)))
		 (insert "[" (car (car l)) "]"))
		((nth 2 (car l))
		 (insert (car (car l))))
		(t
		 (insert "(" (car (car l)) ")")))
	  (if (setq l (cdr l)) (insert " ") (insert "\n"))))
      (if (featurep 'canna)
	  (insert "CANNA (Version " canna-rcs-version ")\n"
		  "  server:" (or canna-server "Not specified") "\n"))
      (if (featurep 'sj3-egg)
	  (insert "SJ3 (Version" sj3-egg-version ")\n"
		  "  server:" (get-sj3-host-name) "\n"))
      (insert "\n")

      (insert-section 3 "Coding systems")
      (save-excursion (list-coding-systems))
      (insert-buffer "*Help*")
      (goto-char (point-max))
      (insert "\n")

      (insert-section 4 "Character sets")
      (save-excursion (list-charsets))
      (insert-buffer "*Help*")
      (goto-char (point-max))
      (insert "\n")

      (if window-system
	  (progn
	    (insert-section 5 "Fontset list")
	    (save-excursion (list-fontset))
	    (insert-buffer "*Help*")))

      (set-buffer-modified-p nil)
      )
    (let ((win (display-buffer buf)))
      (set-window-point win 1)
      (set-window-start win 1))
    ))

;;; DUMP DATA FILE

;;;###autoload
(defun dump-charsets ()
  (list-charsets)
  (set-buffer (get-buffer "*Help*"))
  (let (make-backup-files)
    (write-region (point-min) (point-max) "charsets.lst"))
  (kill-emacs))

;;;###autoload
(defun dump-coding-systems ()
  (list-coding-systems 'all)
  (set-buffer (get-buffer "*Help*"))
  (let (make-backup-files)
    (write-region (point-min) (point-max) "coding-systems.lst"))
  (kill-emacs))

