;;; disp-table.el --- functions for dealing with char tables.

;; Copyright (C) 1987, 1994, 1997, 2007 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Author: Howard Gayle
;; Maintainer: XEmacs Development Team
;; Keywords: i18n, internal

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; Rewritten for XEmacs July 1995, Ben Wing.


;;; Code:

;;;###autoload
(defun make-display-table ()
  "Return a new, empty display table.

This returns a generic character table; previously it returned a vector, but
that was not helpful when dealing with internationalized characters above
#xFF.  See `make-char-table' for details of character tables in general.  To
write code that works with both vectors and character tables, add something
like the following to the beginning of your file, and use
`put-display-table' to set what a given character is displayed as, and
`get-display-table' to examine what that character is currently displayed
as:

\(defun-when-void put-display-table (range value display-table)
  \"Set the value for char RANGE to VALUE in DISPLAY-TABLE.  \"
  (ecase (type-of display-table)
    (vector
     (aset display-table range value))
    (char-table
     (put-char-table range value display-table))))

\(defun-when-void get-display-table (character display-table)
  \"Find value for CHARACTER in DISPLAY-TABLE.  \"
  (ecase (type-of display-table)
    (vector
     (aref display-table character))
    (char-table
     (get-char-table character display-table))))

In this implementation, `put-display-table' and `get-display-table' are
aliases of `put-char-table' and `get-char-table' respectively, and are
always available.   "
  (make-char-table 'generic))

;;;###autoload
(defalias 'put-display-table #'put-char-table)

;;;###autoload
(defalias 'get-display-table #'get-char-table)

(defun describe-display-table (dt)
  "Describe the display table DT in a help buffer."
  (with-displaying-help-buffer
   (lambda ()
     (map-char-table
      (lambda (range value)
        (cond
         ((eq range t)
          (princ "\nAll characters: \n")
          (princ (format "  %S" value)))
         ((eq 'charset (and (symbolp range) (type-of (find-charset range))))
          (princ (format "\n\nCharset %S: \n" (charset-name range)))
          (princ (format "  %S" value)))
         ((vectorp range)
          (princ (format "\n\nCharset %S, row %d \n"
                         (charset-name (aref value 0))
                         (aref value 1)))
          (princ (format "  %S\n\n" value)))
         ((characterp range)
          (princ (format "\nCharacter U+%04X, %S: "
                         range (if (fboundp 'split-char)
                                   (split-char range)
                                 (list 'ascii (char-to-int range)))))
          (princ (format "  %S" value))))
        nil) dt)
     (princ 
      "\n\nFor some of the various other glyphs that GNU Emacs uses the display
table for, see the XEmacs specifiers `truncation-glyph' ,
`continuation-glyph', `control-arrow-glyph', `octal-escape-glyph' and the
others described in the docstring of `make-glyph'. \n\n"))))


;;;###autoload
(defun describe-current-display-table (&optional domain)
  "Describe the display table in use in the selected window and buffer."
  (interactive)
  (or domain (setq domain (selected-window)))
  (let ((disptab (specifier-instance current-display-table domain)))
    (if disptab
	(describe-display-table disptab)
      (message "No display table"))))

;; #### we need a generic frob-specifier function.
;; #### this also needs to be redone like frob-face-property.

;; Let me say one more time how much dynamic scoping sucks.

;;;###autoload
(defun frob-display-table (fdt-function fdt-locale &optional tag-set)
  (or fdt-locale (setq fdt-locale 'global))
  (or (specifier-spec-list current-display-table fdt-locale tag-set)
      (add-spec-to-specifier current-display-table (make-display-table)
			     fdt-locale tag-set))
  (add-spec-list-to-specifier
   current-display-table
   (list (cons fdt-locale
	       (mapcar
		(lambda (fdt-x)
                  (funcall fdt-function (cdr fdt-x))
                  fdt-x)
		(cdar (specifier-spec-list current-display-table
					   fdt-locale tag-set)))))))

(defun standard-display-8bit-1 (dt l h)
  (while (<= l h)
    (remove-char-table (int-to-char l) dt)
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-8bit (l h &optional locale)
  "Display characters in the range L to H literally [sic].

GNU Emacs includes this function.  There, `literally' has no good meaning.
Under XEmacs, this function makes characters with numeric values in the
range L to H display as themselves; that is, as ASCII, latin-iso8859-1,
latin-iso8859-2 or whatever.  See `standard-display-default' for the inverse
function.  "
  (frob-display-table
   (lambda (x)
     (standard-display-8bit-1 x l h))
   locale))

(defun standard-display-default-1 (dt l h)
  "Misnamed function under XEmacs. See `standard-display-default'."
  (while (<= l h)
    (put-char-table (int-to-char l) (format "\\%o" l) dt)
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-default (l h &optional locale)
  "Display characters in the range L to H using octal escape notation.

In the XEmacs context this function is misnamed.  Under GNU Emacs,
characters in the range #xA0 to #xFF display as octal escapes unless
`standard-display-european' has been called; this function neutralizes the
effects of `standard-display-european'.  Under XEmacs, those characters
normally do not display as octal escapes (this ignores hackery like
specifying the X11 font character set on non-Mule builds) and this function
sets them to display as octal escapes.  "
  (frob-display-table
   (lambda (x)
     (standard-display-default-1 x l h))
   locale))

;;;###autoload
(defun standard-display-ascii (c s &optional locale)
  "Display character C using printable string S."
  (frob-display-table
   (lambda (x)
     (put-char-table c s x))
   locale))

;;;###autoload
(defun standard-display-g1 (c sc &optional locale)
  "Display character C as character SC in the g1 character set.
This function assumes that your terminal uses the SO/SI characters;
it is meaningless for an X frame."
  (frob-display-table
   (lambda (x)
     (put-char-table c (concat "\016" (char-to-string sc) "\017") x))
   locale '(tty)))

;;;###autoload
(defun standard-display-graphic (c gc &optional locale)
  "Display character C as character GC in graphics character set.
This function assumes VT100-compatible escapes; it is meaningless for an
X frame."
  (frob-display-table
   (lambda (x)
     (put-char-table c (concat "\e(0" (char-to-string gc) "\e(B") x))
   locale '(tty)))

;;;###autoload
(defun standard-display-underline (c uc &optional locale)
  "Display character C as character UC plus underlining."
  (frob-display-table
   (lambda (x)
     (let (glyph)
       (setq glyph (make-glyph (vector 'string :data (char-to-string uc))))
       (set-glyph-face glyph 'underline)
       (put-char-table c glyph x)))
   locale))

;;;###autoload
(defun standard-display-european (arg &optional locale)
  "Toggle display of European characters encoded with ISO 8859-1.
When enabled (the default), characters in the range of 160 to 255 display
as accented characters. With negative prefix argument, display characters in
that range as octal escapes.  

If you want to work in a Western European language under XEmacs, it
shouldn't be necessary to call this function--things should just work.  But
it's in a sufficient number of init files that we're not in a hurry to
remove it.  "
  (interactive "P")
  (if (<= (prefix-numeric-value arg) 0)
      (frob-display-table
       (lambda (x)
         (standard-display-default-1 x 160 255))
       locale)
    (frob-display-table
     (lambda (x)
       (standard-display-8bit-1 x 160 255))
       locale)))

(provide 'disp-table)

;;; disp-table.el ends here
