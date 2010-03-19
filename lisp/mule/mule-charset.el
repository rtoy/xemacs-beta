;;; mule-charset.el --- Charset functions for Mule.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1992, 2001 Free Software Foundation, Inc.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.
;; Copyright (C) 2002, 2005, 2010 Ben Wing.

;; Author: Mostly Ben Wing
;; Keywords: i18n, mule, internal

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

;;; Synched up with: Not synched.  API at source level synched with FSF 20.3.9.

;;; Commentary:

;; These functions are not compatible at the bytecode level with Emacs/Mule,
;; and they never will be.  -sb [1999-05-26]

;; Most of this file was written by Ben Wing, including all of the charset
;; tag code and the charset creation code, and some of the
;; charset-properties code.  Translation table stuff from FSF (?) with
;; mods from ?.  Various extra authors in the charset-properties code.
;; Author of charsets-in-string: ?

;;; Code:

;;;; Classifying text according to charsets

(defun charsets-in-string (string)
  "Return a list of the charsets in STRING."
  (let (res)
    (with-string-as-buffer-contents string
      ;; charsets-in-region now in C.
      (setq res (charsets-in-region (point-min) (point-max))))
    res))

(defalias 'find-charset-string 'charsets-in-string)

(defalias 'find-charset-region 'charsets-in-region)


;;;; Charset properties and accessors

(defun charset-or-charset-name-p (obj)
  "Return true if OBJ is a charset object or a symbol naming a charset."
  (or (charsetp obj) (and (symbolp obj) (not (null (find-charset obj))))))

(defun charset-iso-graphic-plane (charset)
  "Return the `graphic' property of CHARSET.
See `make-charset'."
  (charset-property charset 'graphic))

(defun charset-iso-final-char (charset)
  "Return the final byte of the ISO 2022 escape sequence designating CHARSET."
  (charset-property charset 'final))

(defun charset-iso-2022-p (charset)
  "Return whether CHARSET is normally encodable using an ISO-2022 coding system.
\"Normally encodable\" means that CHARSET can be designated using a standard
ISO 2022 escape sequence.  Almost any charset can be encoded in ISO-2022 by
using the extension allowing UTF-8 to be embedded in an ISO-2022 encoding,
but that does not count as \"normally encodable\"."
  (not (null (charset-property charset 'final))))

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (charset-property charset 'chars))

(defun charset-offset (charset)
  "Return the minimum index per dimension of CHARSET."
  (charset-property charset 'offset))

(defun charset-width (charset)
  "Return the number of display columns per character of CHARSET.
This only applies to TTY mode (under X, the actual display width can
be automatically determined)."
  (charset-property charset 'columns))

;; #### FSFmacs returns 0
(defun charset-direction (charset)
  "Return the display direction (0 for `l2r' or 1 for `r2l') of CHARSET.
Only left-to-right is currently implemented."
  (if (eq (charset-property charset 'direction) 'l2r)
      0
    1))

;; Not in GNU Emacs/Mule
(defun charset-registry (charset)
  "Obsolete; use charset-registries instead. "
  (lwarn 'xintl 'warning
    "charset-registry is obsolete--use charset-registries instead. ")
  (when (charset-property charset 'registries)
    (elt (charset-property charset 'registries) 0)))

(make-obsolete 'charset-registry 'charset-registries)

(defun charset-registries (charset)
  "Return the registries of CHARSET."
  (charset-property charset 'registries))

(defun set-charset-registry (charset registry)
  "Obsolete; use set-charset-registries instead. "
  (check-argument-type 'stringp registry)
  (check-argument-type 'charsetp (find-charset charset))
  (unless (equal registry (regexp-quote registry))
    (lwarn 'xintl 'warning
      "Regexps no longer allowed for charset-registry. Treating %s%s"
      registry " as a string."))
  (set-charset-registries
   charset
   (apply 'vector registry (append (charset-registries charset) nil))))

(make-obsolete 'set-charset-registry 'set-charset-registries)

(defun charset-ccl-program (charset)
  "Return the CCL program of CHARSET.
See `make-charset'."
  (charset-property charset 'ccl-program))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(defun charset-skip-chars-string (charset)
  "Given CHARSET, return a string suitable for for `skip-chars-forward'.
Passing the string to `skip-chars-forward' will cause it to skip all
characters in CHARSET."
  (setq charset (get-charset charset))
  (let* ((dim (charset-dimension charset))
	 (chars (charset-chars charset))
	 (offset (charset-offset charset))
	 (lowchar (if (= dim 1)
		      (make-char charset offset)
		    (make-char charset
			       (first offset)
			       (second offset))))
	 (highchar (if (= dim 1)
		       (make-char charset (+ offset chars -1))
		     (make-char charset
				(+ (first offset) (first chars) -1)
				(+ (second offset) (second chars) -1)))))
    (unless (and lowchar highchar)
      (signal-error 'invalid-argument
		    `("Charset not encodable in a buffer" ,charset)))
    (format "%c-%c" lowchar highchar)))

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
(defsetf charset-ccl-program set-charset-ccl-program)
(defsetf charset-ccl-program set-charset-ccl-program)
(defsetf charset-registries  set-charset-registries)

;;; FSF compatibility functions
(defun charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil."
  (when (null pos)
    (setq pos (point)))
  (check-argument-type 'integerp pos)
  (unless (or (< pos (point-min))
	      (> pos (point-max)))
    (char-charset (char-after pos))))

;; Yuck!
;; We're not going to support these.
;(defun charset-info (charset) [incredibly broken function with random vectors]
;(defun define-charset (...) [incredibly broken function with random vectors]

;;; Charset property

(defalias 'get-charset-property 'get)
(defalias 'put-charset-property 'put)
(defalias 'charset-plist 'object-plist)
(defalias 'set-charset-plist 'setplist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          translation tables                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (translation-table (:constructor internal-make-translation-table))
  forward
  reverse)

(defun make-translation-table (&rest args)
  "Make a translation table from arguments.
A translation table is a char table intended for character translation
in CCL programs.

Each argument is a list of elemnts of the form (FROM . TO), where FROM
is a character to be translated to TO.

FROM can be a generic character (see `make-char').  In this case, TO is
a generic character containing the same number of characters, or a
ordinary character.  If FROM and TO are both generic characters, all
characters belonging to FROM are translated to characters belonging to TO
without changing their position code(s).

The arguments and forms in each argument are processed in the given
order, and if a previous form already translates TO to some other
character, say TO-ALT, FROM is also translated to TO-ALT."
  (let ((table (internal-make-translation-table
		:forward (make-char-table 'generic)))
	revlist)
    (while args
      (let ((elts (car args)))
	(while elts
	  (let* ((from (car (car elts)))
		 (from-i 0)		; degree of freedom of FROM
		 (from-rev (nreverse (split-char from)))
		 (to (cdr (car elts)))
		 (to-i 0)		; degree of freedom of TO
		 (to-rev (nreverse (split-char to))))
	    ;; Check numbers of heading 0s in FROM-REV and TO-REV.
	    (while (eq (car from-rev) 0)
	      (setq from-i (1+ from-i) from-rev (cdr from-rev)))
	    (while (eq (car to-rev) 0)
	      (setq to-i (1+ to-i) to-rev (cdr to-rev)))
	    (if (and (/= from-i to-i) (/= to-i 0))
		(error "Invalid character pair (%d . %d)" from to))
	    ;; If we have already translated TO to TO-ALT, FROM should
	    ;; also be translated to TO-ALT.  But, this is only if TO
	    ;; is a generic character or TO-ALT is not a generic
	    ;; character.
	    (let ((to-alt (get-char-table to table)))
	      (if (and to-alt
		       (or (> to-i 0) (not (find-charset to-alt))))
		  (setq to to-alt)))
	    (if (> from-i 0)
		(set-char-table-default table from to)
	      (put-char-table from to table))
	    ;; If we have already translated some chars to FROM, they
	    ;; should also be translated to TO.
	    (let ((l (assq from revlist)))
	      (if l
		  (let ((ch (car l)))
		    (setcar l to)
		    (setq l (cdr l))
		    (while l
		      (put-char-table ch to table)
		      (setq l (cdr l)) ))))
	    ;; Now update REVLIST.
	    (let ((l (assq to revlist)))
	      (if l
		  (setcdr l (cons from (cdr l)))
		(setq revlist (cons (list to from) revlist)))))
	  (setq elts (cdr elts))))
      (setq args (cdr args)))
    ;; Return TABLE just created.
    table))

;; Do we really need this?
; (defun make-translation-table-from-vector (vec)
;   "Make translation table from decoding vector VEC.
; VEC is an array of 256 elements to map unibyte codes to multibyte characters.
; See also the variable `nonascii-translation-table'."
;   (let ((table (make-char-table 'translation-table))
;	(rev-table (make-char-table 'translation-table))
;	(i 0)
;	ch)
;     (while (< i 256)
;       (setq ch (aref vec i))
;       (aset table i ch)
;       (if (>= ch 256)
;	  (aset rev-table ch i))
;       (setq i (1+ i)))
;     (set-char-table-extra-slot table 0 rev-table)
;     table))

(defvar named-translation-table-hash-table (make-hash-table))

(defun define-translation-table (symbol &rest args)
  "Define SYMBOL as the name of translation table made by ARGS.
This sets up information so that the table can be used for
translations in a CCL program.

If the first element of ARGS is a translation table, just define SYMBOL to
name it.  (Note that this function does not bind SYMBOL.)

Any other ARGS should be suitable as arguments of the function
`make-translation-table' (which see).

Look up a named translation table using `find-translation-table' or
`get-translation-table'."
  (let ((table (if (translation-table-p (car args))
		   (car args)
		 (apply 'make-translation-table args))))
    (puthash symbol table named-translation-table-hash-table)))

(defun find-translation-table (table-or-name)
  "Retrieve the translation table of the given name.
If TABLE-OR-NAME is a translation table object, it is simply returned.
Otherwise, TABLE-OR-NAME should be a symbol.  If there is no such
translation table, nil is returned.  Otherwise the associated translation
table object is returned."
  (if (translation-table-p table-or-name)
      table-or-name
    (check-argument-type 'symbolp table-or-name)
    (gethash table-or-name named-translation-table-hash-table)))

(defun get-translation-table (table-or-name)
  "Retrieve the translation table of the given name.
Same as `find-translation-table' except an error is signalled if there is
no such translation table instead of returning nil."
  (or (find-translation-table table-or-name)
      (error 'invalid-argument "No such translation table" table-or-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                           ;
;                                                                           ;
;                                charsets                                   ;
;                                                                           ;
;                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;       Charset tags: General functions       ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar charset-tag-to-properties-mapping (make-hash-table)
  "Mapping from full-form charset tag to property list of tag's properties.")
(puthash 'default/default '(parent nil) charset-tag-to-properties-mapping)

(defvar charset-tag-to-charset-mapping (make-hash-table)
  "Mapping from full-form charset tag to list of charsets matching the tag.")

(defvar charset-tag-short-to-full-mapping (make-hash-table)
  "Mapping from short-form charset tag to list of full-form tags.")
(puthash 'default '(default/default) charset-tag-short-to-full-mapping)

(defvar charset-tag-function-list nil
  "Alist of (NAME . FUNCTION) for defined charset-tag functions.")

(defun charset-tag-p (obj)
  "Return t if OBJ is a charset tag."
  ;; Check for both full-form and short-form tags.
  (not (null (or (gethash obj charset-tag-to-properties-mapping)
		 (gethash obj charset-tag-short-to-full-mapping)))))

(defun charset-or-charset-tag-p (obj)
  "Return t if OBJ is a charset, charset name, or charset tag."
  (cond ((charset-or-charset-name-p obj) t)
	((charset-tag-p obj) t)
	((and (listp obj) (every #'charset-or-charset-tag-p obj)) t)
	(t nil)))

(defsubst charset-tag-full-p (tag)
  "Return true if charset tag TAG is in full form."
  (not (null (string-match "/" (symbol-name tag)))))

(defun charset-tag-name (tag)
  "Return the NAME portion of TAG."
  (if (not (charset-tag-full-p tag)) tag
    (let* ((name (symbol-name tag))
	   (val (string-match "\\(.*\\)/\\(.*\\)" name)))
      (assert val)
      (intern (match-string 1 name)))))

(defun charset-tag-to-full-tags (tag)
  "Return list of full tags matching TAG."
  (if (charset-tag-full-p tag) (list tag)
    (gethash tag charset-tag-short-to-full-mapping)))

(defun charset-tag-property (tag property)
  "Return a property of a charset tag."
  (getf (charset-tag-properties tag) property))

(defun charset-tag-properties (tag)
  "Return the list of properties of a charset tag."
  (gethash tag charset-tag-to-properties-mapping))

(defun* define-charset-tag (tag &key list parent function doc-string)
  "Define a charset tag.
PARENT specifies the parent(s) of the tag.  PARENT can be a symbol or list
of symbols.  If PARENT is omitted, `default' is used.
TAG is a symbol.  TAG can either in the full form of `NAME/CATEGORY' or
the short form of `NAME', in which case the category will be taken
to be the first parent.  For example,

\(define-charset-tag 'japanese 'language)

is the same as

\(define-charset-tag 'japanese/language 'language)

If the tags in PARENT are not in full-form, the category `default' will
be assumed.

DOC-STRING is optional documentation for the tag.

FUNCTION is a one-argument predicate that is given a charset and should
return true or false.  If specified, this tag is automatically true for
all charsets matching the function.

LIST is a list of charsets or other tags.  If given, TAG is a list tag,
and when specified in a precedence list, the value of LIST will be
substituted, in order.  This is useful for ensuring that certain charsets
end up in front of or behind certain other ones.

Charset tags can be used in place of charsets in charset precedence lists,
which are used for converting Unicode characters and codepoints into
charset codepoints.  See `make-char' for more information on charsets and
charset codepoints, and `unicode-to-char' for more information on
charset precedence lists.

In a charset precedence list, charset tags match all charsets with the tag
specified in the charset's `tags' property (see `make-charset').  If a
short tag name is given, all tags with the same name match.  For example,
if charset tags `japanese/language' and `japanese/writing-system' both
exist, then the charset tag `japanese' in a charset precedence list matches
both of these tags."
  ;; Coerce arguments to normal form
  (unless parent (setq parent 'default))
  (unless (listp parent) (setq parent (list parent)))
  (setq parent (loop for par in parent
		 collect (if (charset-tag-full-p par) par
			   (intern (format "%s/default" par)))))
  (unless (charset-tag-full-p tag)
    (setq tag (intern (format "%s/%s" tag (charset-tag-name (first parent))))))

  ;; Check for redefinition of existing tag
  (when (gethash tag charset-tag-to-properties-mapping)
    (signal-error 'invalid-change `("Can't redefine existing tag" ,tag)))

  ;; Add to hash tables
  (puthash tag `(parent ,parent doc-string ,doc-string function ,function
		 list ,list)
	   charset-tag-to-properties-mapping)
  (let ((tag-name (charset-tag-name tag)))
    (push tag (gethash tag-name charset-tag-short-to-full-mapping)))

  ;; If a function is given, add to the list of known charset-tag functions,
  ;; and iterate over existing charsets, making note of charsets for which
  ;; the function is true.
  (when function
    (push `(,tag . ,function) charset-tag-function-list)
    (loop for cs in (charset-list) do
      (when (funcall function cs)
	(register-charset-tags-1 cs tag))))

  ;; Validate the value of `list'.
  (when list
    (loop for elt in list do
      (check-argument-type 'charset-or-charset-tag-p elt)))
  )

(defun charset-tag-to-charset-list (tag)
  "Return list of all charsets matching TAG or any of its ancestors.
If TAG has a :list property, fetch the value of that property and recursively
  process each element of the list.
If TAG is in full form, return a list of all charsets directly matching TAG,
  followed by the result of recursively processing TAG's parents.
If TAG is in short form (see `define-charset-tag'), return the result of
  recursively processing all tags with the same short form.
If TAG is a charset, just return a list of that charset.
If TAG is a list, return a list of charsets matching all tags given in the
  list.
This is called from the C code."
  (cond ((or (charsetp tag) (and (symbolp tag) (find-charset tag)))
	 (list (find-charset tag)))
	((listp tag)
	 ;; We could use `intersection', but that doesn't produce a "stable"
	 ;; intersection -- it iterates over the second list instead of the
	 ;; first one, and returns the values in backwards order.
	 (reduce #'(lambda (list1 list2)
		     (loop for l in list1
		       if (memq l list2)
		       collect l))
		 (mapcar #'charset-tag-to-charset-list tag)))
	(t
	 (check-argument-type 'charset-tag-p tag)
	 (remove-duplicates
	  ;; First map to list of full tags ...
	  (let ((full-tags (charset-tag-to-full-tags tag)))
	    (cond ((null full-tags) nil)
		  ((= 1 (length full-tags))
		   ;; If there's only one, then:
		   (let* ((full-tag (first full-tags))
			  (props (charset-tag-properties full-tag))
			  (list (getf props 'list)))
		     ;; 1. If a list tag, recurse over the elements of the
		     ;;    list.
		     (if list
			 (loop for l in list
			   append (charset-tag-to-charset-list l))
		       ;; 2. Else, recursively loop over all parents ...
		       (let* ((ancestor-charsets
			       (loop for par in (getf props 'parent)
				 append (charset-tag-to-charset-list par))))
			 ;; ... then add charsets for the tag itself.
			 ;; Technically the order of these is undefined,
			 ;; but we may as well make it agree with the
			 ;; order the charsets are added, so it is halfway
			 ;; reasonable.
			 (append (reverse
				  (gethash full-tag
					   charset-tag-to-charset-mapping))
				 ancestor-charsets)))))
		  (t
		   ;; Otherwise, loop over all full tags.
		   (loop for full-tag in full-tags
		     append (charset-tag-to-charset-list full-tag)))))
	  :from-end t))))

(defun register-charset-tags-1 (charset full-tag)
  ;; Actually make a note of this CHARSET/FULL-TAG combination.
  (pushnew (get-charset charset)
	   (gethash full-tag charset-tag-to-charset-mapping)))

(defun register-charset-tags (charset tags)
  "Register CHARSET as having `tags' property TAGS.
This is intended to be called from `make-charset'."
  (setq charset (get-charset charset))
  ;; Iterate over all tags ...
  (loop for tag in tags do
    ;; Iterate over all full tags matching each tag ...
    (loop for full-tag in (charset-tag-to-full-tags tag) do
      ;; Add to list of charsets for the full tag.
      (register-charset-tags-1 charset full-tag)))
  ;; Also process function tags.
  (loop for (tag . function) in charset-tag-function-list do
    (when (funcall function charset)
      (register-charset-tags-1 charset tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;       Define the charset tags       ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun do-define-charset-tags (category stuff)
  ;; STUFF should be a list of (CLASS PARENTS DOC), where PARENTS or DOC
  ;; may be omitted.  For each element, define a charset tag CLASS with
  ;; parents PARENTS (a single element or a list), and doc-string DOC.
  ;; If PARENTS is omitted, CATEGORY is the parent.  Both CLASS and PARENTS
  ;; should specify short-form tags, which will have CATEGORY added to make
  ;; them full-form.
  (loop for (class parents doc) in stuff
    for parents = (or parents category)
    for parents = (if (listp parents) parents (list parents))
    do
    (define-charset-tag
      (intern (format "%s/%s" class category))
      :parent (loop for par in parents
		collect (if (eq par category) category
			  (intern (format "%s/%s" par category))))
      :doc-string doc)))

;; define classes of scripts

(define-charset-tag 'script)

(define-charset-tag 'charset
  :doc-string "Used for functional tags describing properties of a charset.")

(loop for (name fun) in
  '((one-column (= 1 (charset-property charset 'columns)))
    (two-column (= 2 (charset-property charset 'columns)))
    (one-dimension (= 1 (charset-property charset 'dimension)))
    (two-dimension (= 2 (charset-property charset 'dimension)))
    (iso2022 (not (null (charset-property charset 'final))))
    (left-to-right (eq 'l2r (charset-property charset 'direction)))
    (right-to-left (eq 'r2l (charset-property charset 'direction)))
    (encodable (charset-encodable-p charset)))
  do
  (define-charset-tag name :parent 'charset
    :function `(lambda (charset)
		 (setq charset (get-charset charset))
		 ,fun)))

;; define scripts that are part of writing systems

(do-define-charset-tags 'script
			'((western)
			  (eastern)
			  (east-asian eastern)
			  (south-asian eastern)
			  (middle-eastern eastern)
			  (alphabetic)
			  (abjad)
			  (abugida)
			  (syllabic)
			  (semi-syllabic)
			  (logographic)
			  (control-chars)))

;; define classes of writing systems

(define-charset-tag 'writing-system)

(do-define-charset-tags 'writing-system
			'((simple)
			  (complex)))

;; define the simple scripts that also serve as writing systems

(loop for (script-and-writing-system script-parents doc) in
  '((latin (western alphabetic) "Latin letters, aka Roman letters")
    (cyrillic (western alphabetic))
    (greek (western alphabetic))
    (arabic (middle-eastern abjad))
    (hebrew (middle-eastern abjad))
    (thai (south-asian abugida))
    (lao (south-asian abugida))
    (tibetan (south-asian abugida))
    (ethiopic (middle-eastern abugida))
    (devanagari (south-asian abugida)))
  do
  (do-define-charset-tags 'script
			  (list (list script-and-writing-system
				      script-parents doc)))
  (do-define-charset-tags 'writing-system
			  (list (list script-and-writing-system nil doc))))

;; define scripts that are part of writing systems

(do-define-charset-tags
 'script
 '((kanji (east-asian logographic))
   (kana (east-asian syllabic))
   (hirigana (kana))
   (katakana (kana))
   ;; Wikipedia claims that Hangul is actually a "featural alphabet" rather
   ;; than a syllabary, but that's getting very obscure.
   (hangul (east-asian syllabic))
   (bopomofo (east-asian semi-syllabic))))

;; define complex writing systems

(do-define-charset-tags
 'writing-system
 '((japanese complex)
   (chinese complex)
   (traditional-chinese chinese)
   (simplified-chinese chinese)
   (korean complex)))

;; define families

(define-charset-tag 'family)

(do-define-charset-tags
 'family
 '((national-standard)
   (iso8859)
   (windows)
   (windows-ansi windows)
   (windows-oem windows)
   (macintosh)
   (ebcdic)
   (jis national-standard)
   (gb national-standard)
   (cns national-standard)
   (kns national-standard)
   (tis national-standard)
   (is national-standard)
   (koi8)
   ))

;; define languages

(define-charset-tag 'language)

(do-define-charset-tags
 'language
 '(
   ;; GEOGRAPHIC REGIONS
   (european)
   (asian)
   (west-asian asian)
   (caucasus west-asian)
   (middle-eastern asian)
   (central-asian asian)
   (east-asian asian)
   (south-asian asian)
   (southeast-asian asian)
   (african)
   (north-african african)
   (subsaharan-african african)

   ;; LANGUAGE FAMILIES
   (indo-european)
   (romance (indo-european european))
   (germanic (indo-european european))
   (balto-slavic (indo-european european))
   (slavic balto-slavic)
   (celtic (indo-european european))
   (baltic balto-slavic)
   (greek (indo-european european))
   (albanian (indo-european european))
   (indo-iranian indo-european)
   (iranian indo-european)
   (indic (indo-european south-asian))

   (afroasiatic)
   (semitic afroasiatic)
   (berber afroasiatic)
   (chadic afroasiatic)
   (cushitic afroasiatic)

   (uralic)
   (finno-ugric (uralic european))
   (finno-permic finno-ugric)
   (ugric finno-ugric)

   (altaic)
   (mongolic (altaic east-asian))
   (tungusic (altaic east-asian))
   (turkic altaic)

   (niger-congo subsaharan-african)
   (bantu niger-congo)

   (sino-tibetan)
   (tibeto-burman sino-tibetan)

   (dravidian south-asian)
   (austro-asiatic)
   (austronesian)
   (eskimo-aleut)
   (nilo-saharan)
   (south-caucasian central-asian)

   ;; ROMANCE LANGUAGES

   (french romance)
   (spanish romance)
   (portuguese romance)
   (italian romance)
   (romanian romance)

   ;; GERMANIC LANGUAGES

   (west-germanic germanic)
   (english west-germanic)
   (german west-germanic)
   (dutch west-germanic)

   (north-germanic germanic)
   (swedish north-germanic)
   (danish north-germanic)
   (norwegian north-germanic)
   (icelandic north-germanic)

   ;; SLAVIC LANGUAGES

   (east-slavic slavic)
   (russian east-slavic)
   (ukrainian east-slavic)
   (belarusian east-slavic)

   (west-slavic slavic)
   (polish west-slavic)
   (czech west-slavic)
   (slovak west-slavic)
   (sorbian west-slavic)

   (south-slavic slavic)
   (bulgarian south-slavic)
   (serbian south-slavic)
   (croatian south-slavic)
   (macedonian south-slavic)
   (slovenian south-slavic)

   ;; OTHER EUROPEAN LANGUAGES

   (irish celtic)
   (welsh celtic)
   (breton celtic)

   (latvian baltic)
   (lithuanian baltic)

   (finnish (finno-permic european))
   (estonian (finno-permic european))
   (hungarian (ugric european))

   ;; MIDDLE-EASTERN LANGUAGES

   (hebrew (semitic middle-eastern))
   (arabic (semitic middle-eastern))
   (amharic (semitic subsaharan-african))

   ;; WEST ASIAN, CENTRAL ASIAN LANGUAGES

   (persian (iranian west-asian))
   (pashto (iranian central-asian))
   (kurdish (iranian west-asian))
   (tajik (iranian central-asian))

   (turkish (turkic west-asian))
   (kazakh (turkic central-asian))
   (uzbek (turkic central-asian))

   ;; SOUTH-ASIAN LANGUAGES

   (hindi indic)
   (urdu indic)
   (marathi indic)
   (gujarati indic)
   (bengali indic)
   (punjabi indic)
   (oriya indic)
   (sindhi indic)
   (sinhala indic)
   ;; etc.

   (tamil dravidian)
   (telugu dravidian)
   (malayalam dravidian)
   (kannada dravidian)

   ;; SOUTHEAST ASIAN, EAST ASIAN LANGUAGES

   (thai southeast-asian)
   (lao southeast-asian)
   (vietnamese southeast-asian)

   (chinese (sino-tibetan east-asian))
   ;; #### or should tibetan be central-asian?
   (tibetan (sino-tibetan east-asian))
   (burmese (sino-tibetan southeast-asian))
   (japanese east-asian)
   (korean east-asian)
   (mongolian mongolic)

   ))

;; misc

(unless (featurep 'unicode-internal)
  (define-charset-tag 'jit
    :doc-string
"Just-in-time (JIT) charsets used for holding undefined Unicode characters."))
(define-charset-tag 'internal
  :doc-string
"Charsets used for internal purposes.  Generally should be left alone.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;       GNU EMACS CHARSETS        ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Synched up with: FSF 21.1.

;; All FSF charset definitions are in mule-conf.el.  I copied the relevant
;; part of that file below, then converted all charset definitions using
;; the macro below, then globally replaced 'direction 0' with 'direction
;; l2r' and 'direction 1' with 'direction r2l', then commented everything
;; out.  Copy the definitions as necessary to individual files.

;; Kbd macro to convert from FSF-style define-charset to our make-charset.

; (setq last-kbd-macro (read-kbd-macro
; "<right> M-d make <M-right> M-d <home> <down> TAB '[dimension DEL SPC <M-right> RET TAB chars SPC <M-right> RET TAB columns SPC <M-right> RET TAB direction SPC <M-right> RET TAB final SPC <M-right> RET TAB graphic SPC <M-right> RET TAB short- name SPC <M-right> RET TAB long- name SPC <M-right> RET TAB <S-M-right> <f2> DEL TAB <end> ] <M-left> <end> SPC <f4> 3*<M-left> <left> <M-right> RET <down>"))

;; Kbd macro to take one registry entry from the list of registry entries,
;; find the appropriate make-charset call, and add the appropriate registry
;; property.

; (setq last-kbd-macro (read-kbd-macro
; "3*<right> <S-M-right> C-x x 1 <right> <S-M-right> C-x x 2 <home> C-x r m foo RET <M-down> M-x sear TAB for TAB RET C-x g 1 RET C-s dimen RET <end> RET TAB 3*<backspace> registry SPC C-x g 2 C-x r b RET <down>"))

;; List from FSF international/fontset.el of registries for charsets.

;; latin-iso8859-1 "ISO8859-1"
;; latin-iso8859-2 "ISO8859-2"
;; latin-iso8859-3 "ISO8859-3"
;; latin-iso8859-4 "ISO8859-4"
;; thai-tis620 "TIS620"
;; greek-iso8859-7 "ISO8859-7"
;; arabic-iso8859-6 "ISO8859-6"
;; hebrew-iso8859-8 "ISO8859-8"
;; katakana-jisx0201 "JISX0201"
;; latin-jisx0201 "JISX0201"
;; cyrillic-iso8859-5 "ISO8859-5"
;; latin-iso8859-9 "ISO8859-9"
;; japanese-jisx0208-1978 "JISX0208.1978"
;; chinese-gb2312 "GB2312.1980"
;; japanese-jisx0208 "JISX0208.1990"
;; korean-ksc5601 "KSC5601.1989"
;; japanese-jisx0212 "JISX0212"
;; chinese-cns11643-1 "CNS11643.1992-1"
;; chinese-cns11643-2 "CNS11643.1992-2"
;; chinese-cns11643-3 "CNS11643.1992-3"
;; chinese-cns11643-4 "CNS11643.1992-4"
;; chinese-cns11643-5 "CNS11643.1992-5"
;; chinese-cns11643-6 "CNS11643.1992-6"
;; chinese-cns11643-7 "CNS11643.1992-7"
;; chinese-big5-1 "Big5"
;; chinese-big5-2 "Big5"
;; chinese-sisheng "sisheng_cwnn"
;; vietnamese-viscii-lower "VISCII1.1"
;; vietnamese-viscii-upper "VISCII1.1"
;; arabic-digit "MuleArabic-0"
;; arabic-1-column "MuleArabic-1"
;; arabic-2-column "MuleArabic-2"
;; ipa "MuleIPA"
;; ethiopic "Ethiopic-Unicode"
;; indian-is13194 "IS13194-Devanagari"
;; indian-2-column "MuleIndian-2"
;; indian-1-column "MuleIndian-1"
;; lao "MuleLao-1"
;; tibetan "MuleTibetan-2"
;; tibetan-1-column "MuleTibetan-1"
;; latin-iso8859-14 "ISO8859-14"
;; latin-iso8859-15 "ISO8859-15"
;; mule-unicode-0100-24ff "ISO10646-1"
;; mule-unicode-2500-33ff "ISO10646-1"
;; mule-unicode-e000-ffff "ISO10646-1"
;; japanese-jisx0213-1 "JISX0213.2000-1"
;; japanese-jisx0213-2 "JISX0213.2000-2"

;;; In international/mule-conf.el in GNU Emacs.

; ;; ISO-2022 allows a use of character sets not registered in ISO with
; ;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
; ;; reserves `0' through `9' to support several private character sets.
; ;; The remaining final characters `:' through `?' are for users.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; DEFINITION OF INTERNAL CHARSETS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE NOTE NOTE NOTE NOTE:
;;
;; We must define these charsets FIRST if we want them to end up with
;; "official" rather than "private" ID's under old-Mule.  This matters in
;; that charsets with official ID's are stored more efficiently (with one
;; less byte) than charsets with private ID's.

;; Old comment, may not be completely accurate: [[ We also put many
;; charsets here, rather than in the individual files devoted to particular
;; languages (as we did before), because we need to load the Unicode tables
;; for them *before* loading any files containing characters from these
;; character sets. (If/when these files are converted to UTF-8, the problem
;; will conceivably go away, at least for Unicode-internal -- but then the
;; opposite problem would exist for old-Mule, if this is still being
;; preserved.) ]]

;; Here is one option for a more sensible keyword-based interface onto
;; charset creation:

;(defun* make-charset* (name short-name &key long-name doc-string dimension
;		       offset chars direction registries columns graphic
;		       final ccl-program unicode-map tags)
;  "Make a charset.  This is an alternative interface to `make-charset'.
;This interface uses keyword properties instead of a list of properties,
;and takes a mandatory short-name parameter rather than a doc string.
;NAME is a symbol, the charset's name.  SHORT-NAME is a string describing
; the charset briefly, and will be used as the `short-name' property.
;The keys :long-name, :doc-string, :dimension, :registries, :columns,
; :graphic, :final, :ccl-program, and :unicode-map will be used to
; set the associated charset properties.  If unspecified, :long-name defaults
; to `short-name', and :doc-string defaults to :long-name."
;  (setq long-name (or long-name short-name))
;  (setq doc-string (or doc-string long-name))
;  (make-charset name doc-string
;		`(short-name ,short-name
;		  long-name ,long-name
;		  ,@(and dimension `(dimension ,dimension))
;		  ,@(and offset `(offset ,offset))
;		  ,@(and chars `(chars ,chars))
;		  ,@(and direction `(direction ,direction))
;		  ,@(and registries `(registries ,registries))
;		  ,@(and columns `(columns ,columns))
;		  ,@(and graphic `(graphic ,graphic))
;		  ,@(and final `(final ,final))
;		  ,@(and ccl-program `(ccl-program ,ccl-program))
;		  ,@(and unicode-map `(unicode-map ,unicode-map))
;		  ,@(and unicode-map `(tags ,tags))
;		  )))

(defun* make-128-byte-charset (name short-name &key long-name
			       doc-string unicode-map tags)
  "Make a one-dimension size-128 charset.
NAME is a symbol, the charset's name.
SHORT-NAME is a string describing the charset briefly, and will be used as
the `short-name' property.

The keys :long-name, :doc-string, :unicode-map and :tags will be used to
set the associated charset properties.  If unspecified, :long-name defaults
to `short-name', and :doc-string defaults to :long-name."
  (setq long-name (or long-name short-name))
  (setq doc-string (or doc-string long-name))
  (make-charset name doc-string
		`(dimension 1
		  offset 128
		  chars 128
		  ,@(and unicode-map `(unicode-map ,unicode-map))
		  short-name ,short-name
		  long-name ,long-name
		  ,@(and tags `(tags ,tags))
		  )))

;;;;;;;;;;;;;;;;;;;;; ASCII, Control-1, Composite, etc. ;;;;;;;;;;;;;;;;;;;;

; #### No equivalent of the following charset from FSF

; ;; ASCII with right-to-left direction.
; (make-charset 'ascii-right-to-left
;	      "ASCII (left half of ISO 8859-1) with right-to-left direction"
;	      '(dimension
;		1
;		registries ["ISO8859-1"]
;		chars 94
;		columns 1
;		direction r2l
;		final ?B
;		graphic 0
;		short-name "rev ASCII"
;		long-name "ASCII with right-to-left direction"
;		))
(set-charset-tags 'ascii '(latin))
(set-charset-tags 'control-1 '(control-chars))
(set-charset-tags 'composite '(internal))
(unless (featurep 'unicode-internal)
  (set-charset-tags 'jit-ucs-charset-0 '(jit internal)))

;; Windows Glyph List 4
(init-windows-glyph-list-4) ;; in unicode.el
(fmakunbound 'init-windows-glyph-list-4) ;; we don't want it dumped

;;;;;;;;;;;;;;;;;;;;; ISO 8859 ;;;;;;;;;;;;;;;;;;;;

(defun* make-iso8859-charset (symbol str8859 short-name alphabet-name
			      iso-ir-name final tags &key doc-string direction)
  (let ((doc-string
	 (or doc-string
	     (format "Right-Hand Part of %s (ISO/IEC %s): %s"
		     alphabet-name str8859 iso-ir-name))))
    (make-charset
     symbol doc-string
     `(dimension 1
       offset 160
       chars 96
       short-name ,short-name
       long-name ,(format "RHP of %s (ISO %s): %s"
			  short-name str8859 iso-ir-name)
       unicode-map
       (,(format "unicode/unicode-consortium/ISO8859/%s.TXT" str8859) #xA0)
       registries ,(vector (format "ISO%s" str8859))
       final ,final
       tags ,(cons 'iso8859 tags)
       direction ,(or direction 'l2r)))))

;; This is defined internally because it's (probably) needed early on,
;; and because it needs to have a very specific charset ID.
;(make-iso8859-charset 'latin-iso8859-1 "8859-1" "Latin-1" "Latin Alphabet 1"
;		      "ISO-IR-100" ?A '(latin))
(set-charset-tags 'latin-iso8859-1 '(latin))
(make-iso8859-charset 'latin-iso8859-2 "8859-2" "Latin-2" "Latin Alphabet 2"
		      "ISO-IR-101" ?B '(latin))
(make-iso8859-charset 'latin-iso8859-3 "8859-3" "Latin-3" "Latin Alphabet 3"
		      "ISO-IR-109" ?C '(latin))
(make-iso8859-charset 'latin-iso8859-4 "8859-4" "Latin-4" "Latin Alphabet 4"
		      "ISO-IR-110" ?D '(latin))
(make-iso8859-charset 'latin-iso8859-9 "8859-9" "Latin-5" "Latin Alphabet 5"
		      "ISO-IR-148" ?M '(latin))
(make-iso8859-charset 'latin-iso8859-15 "8859-15" "Latin-9 (Euro Sign)"
		      "Latin Alphabet 9" "ISO-IR-203" ?b '(latin)
		      :doc-string
		      "European Supplementary Latin Set (\"Latin 9\") (Euro Sign) (ISO/IEC 8859-15): ISO-IR-203
FIELD OF UTILIZATION: \"Communication and processing of text in European
languages. The set provides for the languages enumerated in ISO/IEC
8859-1. In addition, it contains the EURO SIGN and provides support for the
French, and Finnish languages in addition.\"")

(make-iso8859-charset 'greek-iso8859-7 "8859-7" "Greek" "Latin/Greek Alphabet"
		      "ISO-IR-126" ?F '(greek))
(make-iso8859-charset 'cyrillic-iso8859-5 "8859-5" "Cyrillic"
		      "Latin/Cyrillic Alphabet" "ISO-IR-144" ?L '(cyrillic))
(make-iso8859-charset 'hebrew-iso8859-8 "8859-8" "Hebrew"
		      "Latin/Hebrew Alphabet" "ISO-IR-138" ?H '(hebrew)
		      :direction 'r2l)
(make-iso8859-charset 'arabic-iso8859-6 "8859-6" "Arabic"
		      "Latin/Arabic Alphabet" "ISO-IR-127" ?G '(arabic)
		      :direction 'r2l)

;;;;;;;;;;;;;;;;;;;;; Japanese ;;;;;;;;;;;;;;;;;;;;

(make-charset
 'katakana-jisx0201
 "Katakana Part of JISX0201.1976"
 '(dimension 1
   chars 94
   final ?I
   graphic 1
   short-name "Japanese (JISX0201 Kana)"
   long-name "Japanese Katakana (JISX0201.1976)"
   registries ["jisx0201.1976-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0201.TXT"
		#xA0)
   tags (jis katakana japanese)
   ))

(make-charset
 'latin-jisx0201
 "Roman Part of JISX0201.1976"
 '(dimension 1
   chars 94
   final ?J
   graphic 0
   short-name "Japanese (JISX0201 Roman)"
   long-name "Japanese Roman (JISX0201.1976)"
   registries ["jisx0201.1976-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0201.TXT"
		#x21 #x7F)
   tags (jis latin/script japanese)
   ))


(make-charset
 'japanese-jisx0208-1978
 "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
 '(dimension 2
   chars 94
   final ?@
   graphic 0
   short-name "Japanese (JISX0208.1978)"
   long-name "Japanese (JISX0208.1978): ISO-IR-42"
   registries ["jisx0208.1978-0" "jisc6226.1978-0"]
   ;; @@#### FIXME This is not correct!!!!!!!!!!!!
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0208.TXT" nil nil nil ignore-first-column)
   tags (jis kanji japanese)
   ))

(make-charset
 'japanese-jisx0208
 "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
 '(dimension 2
   chars 94
   final ?B
   graphic 0
   short-name "Japanese (JISX0208)"
   long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
   registries ["jisx0208.1983-0" "jisx0208.1990-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0208.TXT" nil nil nil ignore-first-column)
   tags (jis kanji japanese)
   ))

(make-charset
 'japanese-jisx0212
 "JISX0212 Japanese supplement: ISO-IR-159"
 '(dimension 2
   chars 94
   final ?D
   graphic 0
   short-name "Japanese (JISX0212)"
   long-name "JISX0212 (Japanese): ISO-IR-159"
   registries ["jisx0212.1990-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0212.TXT")
   tags (jis kanji japanese)
   ))

(when (featurep 'unicode-internal)
  ;; We can support Shift-JIS directly.
  (make-charset
   'japanese-shift-jis
   ;; You could imagine trying to declare this to be an "algorithmic" charset
   ;; with indices shifted in a programmatic way from JIS X 0208:1997.
   ;; However, besides being a lot of hassle for little gain (how many other
   ;; such algorithmic charsets would there be?), I'm pretty sure that
   ;; Shift-JIS adds at least a couple of characters not present in
   ;; JIS X 0208:1997.
   "Shift-JIS Japanese encoding of JIS X 0208:1997"
   '(dimension 2
     ;; See comments in mule-coding.c.
     ;; First byte is in the range [80-9F], [E0-EF]; second byte is in the
     ;; range [40-7E], [80-FC]
     chars (112 189)
     offset (128 64)
     short-name "Japanese (Shift-JIS)"
     long-name "Japanese (Shift-JIS)"
     ;; @@#### FIXME This is the X registry; is it right?
     registries ["sjis"]
     unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/SHIFTJIS.TXT"
		  #x8000)
     tags (jis kanji japanese)
     ))
  )

;;;;;;;;;;;;;;;;;;;;; Chinese ;;;;;;;;;;;;;;;;;;;;

(make-charset
 'chinese-gb2312
 "GB2312 Chinese simplified: ISO-IR-58"
 '(dimension 2
   chars 94
   final ?A
   graphic 0
   short-name "Chinese simplified (GB2312)"
   long-name "Chinese simplified (GB2312): ISO-IR-58"
   registries ["gb2312.1980-0" "gb2312.80&gb8565.88-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/GB2312.TXT")
   tags (gb kanji simplified-chinese chinese/language)
   ))

(make-charset
 'chinese-cns11643-1
 "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
 '(dimension 2
   chars 94
   final ?G
   graphic 0
   short-name "Chinese traditional (CNS11643-1)"
   long-name "Chinese traditional (CNS11643-1): ISO-IR-171"
   registries ["CNS11643.1992-1"]
   ;; Currently, CNS11643.TXT is based on CNS 11643-1986 (with planes 1, 2,
   ;; and 14, and no Unicode codepoint assignments outside of the BMP),
   ;; where the files in mule-ucs/chinese-cns11643-*.txt are based on
   ;; CNS 11643-1992 (with planes 1-7 and codepoint assignments outside of
   ;; the BMP).
   ;;unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/CNS11643.TXT"
   ;;             #x10000 #x1FFFF #x-10000)
   unicode-map ("unicode/mule-ucs/chinese-cns11643-1.txt")
   tags (cns kanji traditional-chinese chinese/language)
   ))

(make-charset
 'chinese-cns11643-2
 "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
 '(dimension 2
   chars 94
   final ?H
   graphic 0
   short-name "Chinese traditional (CNS11643-2)"
   long-name "Chinese traditional (CNS11643-2): ISO-IR-172"
   registries ["CNS11643.1992-2"]
   ;; See above.
   ;;unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/CNS11643.TXT"
   ;;             #x20000 #x2FFFF #x-20000)
   unicode-map ("unicode/mule-ucs/chinese-cns11643-2.txt")
   tags (cns kanji traditional-chinese chinese/language)
   ))

(if (featurep 'unicode-internal)
    ;; We can support Big5 directly.
    (make-charset
     'chinese-big5
     "Big5 (Chinese traditional)"
     '(dimension 2
       ;; Big5 claims to be a 94x157 charset, but with gaps in the middle.
       ;; In particular, the rows are (theoretically) indexed from A1 - FE
       ;; and the columns from 40 - 7E and A1 - FE.  In fact, there are gaps
       ;; in the rows as well (rows C7 and C8 are missing, as well as rows
       ;; FA - FE), but that appears to be due to accident -- i.e. they just
       ;; ran out of chars and/or wanted to make room for expansion.  Note
       ;; also that the gap at C7 and C8 is due to the Level-1/Level-2
       ;; division of Big5 (see below).  The 94 rows are those between
       ;; A1 and FE, inclusive.  The 157 columns count the sum of the columns
       ;; in each disjoint set.  For us, we need to use the size of the range
       ;; [40, FE], which is 191.
       chars (94 191)
       offset (161 64)
       short-name "Chinese traditional (Big5)"
       long-name "Chinese traditional (Big5)"
       registries ["big5.eten-0"]
       unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/BIG5.TXT")
       tags (kanji traditional-chinese chinese/language)
       ))
  ;; Old Mule situation; we can only handle up to 96x96 charsets.
  ;; So we split it into two charsets.  According to Ken Lunde's CJKV
  ;; book, Big5 itself is split into "Big Five Level 1" (rows A1-C6)
  ;; and "Big Five Level 2" (rows C9-F9), with the latter containing
  ;; less used characters.  We split the same way then coerce the
  ;; result into a 94x94 block.
  (make-charset
   'chinese-big5-1
   "Frequently used part (A141-C67F) of Big5 (Chinese traditional)"
   '(dimension 2
     chars 94
     final ?0
     graphic 0
     short-name "Chinese traditional (Big5), L1"
     long-name "Chinese traditional (Big5) (Level-1) A141-C67F"
     registries ["big5.eten-0"]
     ;; no unicode map, see chinese-big5-2
     tags (kanji traditional-chinese chinese/language)
     ))
  (make-charset
   'chinese-big5-2
   "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
   '(dimension 2
     chars 94
     final ?1
     graphic 0
     short-name "Chinese traditional (Big5), L2"
     long-name "Chinese traditional (Big5) (Level-2) C940-FEFE"
     registries ["big5.eten-0"]
     ;; HACK HACK HACK!  The `big5' special flag tells the internal code
     ;; in Fload_unicode_mapping_table() to take codepoints out of the
     ;; Big5 table, convert them to a codepoint in a "fake" chinese-big5-1
     ;; or chinese-big5-2, and store appropriately.  Hence, it actually
     ;; ignores the name of the charset on which the property is set and
     ;; always stores in the "right" place.  Correspondingly, we must set
     ;; the property on big5-2, not 1, so that both charsets will be
     ;; created by the time we initialize the map.
     unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/BIG5.TXT"
		  nil nil nil big5)
     tags (kanji traditional-chinese chinese/language)
     ))
  )

;;;;;;;;;;;;;;;;;;;;; Korean ;;;;;;;;;;;;;;;;;;;;

(make-charset
 'korean-ksc5601
 "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
 '(dimension 2
   chars 94
   final ?C
   graphic 0
   short-name "Korean (KSC5601)"
   long-name "Korean (KSC5601): ISO-IR-149"
					;registries ["KSC5601.1989"]
   registries ["ksc5601.1987-0"]
   ;; Note that KSC5601.TXT as currently distributed is NOT what
   ;; it claims to be!  See comments in KSX1001.TXT.
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/KSX1001.TXT" )
   tags (ksc kanji hangul korean)
   ))

;;;;;;;;;;;;;;;;;;;;; Thai ;;;;;;;;;;;;;;;;;;;;

(make-charset
 'thai-tis620
 "Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166"
 '(dimension 1
   chars 96
   final ?T
   graphic 1
   short-name "Thai (TIS620)"
   long-name "RHP of Thai (TIS620): ISO-IR-166"
   registries ["tis620.2529-1"]
   unicode-map ("unicode/mule-ucs/thai-tis620.txt" nil nil #x80)
   tags (tis thai)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; DEFINITION OF OTHER CHARSETS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; APPROPRIATE FILE: vietnamese.el
;; CAN'T BE DEFINED THERE BECAUSE: The sample text in the file has
;; Vietnamese chars.  In old-Mule, they will end up in a JIT charset unless
;; the charset is already defined.

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(make-charset
 'vietnamese-viscii-lower "VISCII1.1 lower-case"
 '(dimension 1
   registries ["VISCII1.1"]
   chars 96
   final ?1
   graphic 1
   unicode-map ("unicode/mule-ucs/vietnamese-viscii-lower.txt"
		nil nil #x80)
   short-name "VISCII lower"
   long-name "VISCII lower-case"
   tags (latin vietnamese)
   ))

(make-charset
 'vietnamese-viscii-upper "VISCII1.1 upper-case"
 '(dimension 1
   registries ["VISCII1.1"]
   chars 96
   final ?2
   graphic 1
   unicode-map ("unicode/mule-ucs/vietnamese-viscii-upper.txt"
		nil nil #x80)
   short-name "VISCII upper"
   long-name "VISCII upper-case"
   tags (latin vietnamese)
   ))

; ;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
; ;; not assigned.  They are automatically converted to each Indian
; ;; script which IS-13194 supports.

(make-charset
 'indian-is13194
 "Generic Indian charset for data exchange with IS 13194"
 '(dimension 1
   registries ["IS13194-Devanagari"]
   chars 94
   columns 2
   final ?5
   graphic 1
   unicode-map ("unicode/mule-ucs/indian-is13194.txt"
		nil nil #x80)
   short-name "IS 13194"
   long-name "Indian IS 13194"
   tags (devanagari indic)
   ))

;; Actual Glyph for 1-column width.
(make-charset
 'indian-1-column
 "Indian charset for 2-column width glyphs"
 '(dimension 2
   registries ["MuleIndian-1"]
   chars 94
   columns 1
   final ?6
   graphic 0
   short-name "Indian 1-col"
   long-name "Indian 1 Column"
   tags (devanagari indic)
   ))

;; Actual Glyph for 2-column width.
(make-charset
 'indian-2-column
 "Indian charset for 2-column width glyphs"
 '(dimension    2
   registries ["MuleIndian-2"]
   chars 94
   columns 2
   final ?5
   graphic 0
   short-name "Indian 2-col"
   long-name "Indian 2 Column"
   tags (devanagari indic)
   ))

;; Lao script.
;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
(make-charset
 'lao "Lao characters (ISO10646 0E80..0EDF)"
 '(dimension 1
   registries ["MuleLao-1"]
   chars 94
   final ?1
   graphic 0
   unicode-map ("unicode/other/lao.txt")
   short-name "Lao"
   long-name "Lao"
   tags (lao)
   ))

;; APPROPRIATE FILE: ethiopic.el
;; CAN'T BE DEFINED THERE BECAUSE: The charset is used inside of that file.

;; Ethiopic characters (Amharic and Tigrinya).
(make-charset
 'ethiopic "Ethiopic characters"
 '(dimension 2
   registries ["Ethiopic-Unicode"]
   chars 94
   final ?3
   graphic 0
   unicode-map ("unicode/mule-ucs/ethiopic.txt")
   short-name "Ethiopic"
   long-name "Ethiopic characters"
   tags (ethiopic)
   ))

(make-charset
 'tibetan-1-column "Tibetan 1 column glyph"
 '(dimension 2
   registries ["MuleTibetan-1"]
   chars 94
   columns 1
   final ?8
   graphic 0
   short-name "Tibetan 1-col"
   long-name "Tibetan 1 column"
   tags (tibetan)
   ))

;; Tibetan script.
(make-charset
 'tibetan "Tibetan characters"
 '(dimension 2
   registries ["MuleTibetan-2"]
   chars 94
   columns 2
   final ?7
   graphic 0
   unicode-map ("unicode/mule-ucs/tibetan.txt")
   short-name "Tibetan 2-col"
   long-name "Tibetan 2 column"
   tags (tibetan)
   ))

;; GNU Emacs has the charsets:

;;     mule-unicode-2500-33ff
;;     mule-unicode-e000-ffff
;;     mule-unicode-0100-24ff

;; built-in.  This is hack--and an incomplete hack at that--against the
;; spirit and the letter of standard ISO 2022 character sets.  Instead of
;; this, we have the jit-ucs-charset-N Mule character sets, created in
;; unicode.c on encountering a Unicode code point that we don't recognise,
;; and saved in ISO 2022 coding systems using the UTF-8 escape described in
;; ISO-IR 196.

;;; mule-charset.el ends here
