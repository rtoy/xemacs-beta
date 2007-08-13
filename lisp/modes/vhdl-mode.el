;;; vhdl-mode.el --- major mode for editing VHDL code

;; Copyright (C) 1994 - 1997 Rodney J. Whitby
;; Copyright (C) 1992, 1993, 1994 Barry A. Warsaw
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author:	  Rodney J. Whitby <rwhitby@geocities.com>
;; Maintainer:	  Rodney J. Whitby <rwhitby@geocities.com>
;; Created:	  June 1994, adapted from cc-mode.el 4.29 by Barry A. Warsaw.
;; Version:	  $Revision: 1.7 $
;; Last Modified: $Date: 1997/09/17 05:19:27 $
;; Keywords:	  languages VHDL
;; Archive:	  http://www.geocities.com/SiliconValley/Park/8287/

;; NOTE: Read the commentary below for the right way to submit bug reports!

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides indentation support for VHDL code.

;; Details on VHDL-MODE are now contained in an accompanying texinfo
;; manual (vhdl-mode.texi).

;; To submit bug reports, hit "C-c C-b", and please try to include a
;; code sample so I can reproduce your problem.  If you have other
;; questions contact me at the address listed at the top of this file.

;; YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS. They are the result of
;; the multi-Emacsen support. FSF Emacs 19 and XEmacs 19 (formerly
;; Lucid) do things differently and there's no way to shut the
;; byte-compiler up at the necessary granularity.  Let me say this
;; again: YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS (you'd be
;; surprised at how many people don't follow this advice :-).

;; To use VHDL-MODE, add the following to your .emacs file.  This
;; assumes you will use .vhd extensions for your VHDL source:
;;
;; (autoload 'vhdl-mode   "vhdl-mode" "VHDL Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.vhd$"  . vhdl-mode)   ; to edit VHDL code
;;            ) auto-mode-alist))
;;
;; If you would like to join the `vhdl-mode-announce' announcements
;; list or the `vhdl-mode-victims' beta testers list, send add/drop
;; requests to the address listed at the top of this file.
;;
;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, and code contributions,
;; and encouragement vhdl-mode.el would be a far inferior package.

;; LCD Archive Entry:
;; vhdl-mode.el|Rodney J. Whitby|rwhitby@geocities.com
;; |Major mode for editing VHDL code
;; |$Date: 1997/09/17 05:19:27 $|$Revision: 1.7 $
;; |http://www.geocities.com/SiliconValley/Park/8287/


;;; Code:

;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup vhdl nil
  "Major mode for editing VHDL code."
  :group 'languages)


(defcustom vhdl-inhibit-startup-warnings-p nil
  "*If non-nil, inhibits start up compatibility warnings."
  :type 'boolean
  :group 'vhdl)

(defcustom vhdl-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `vhdl-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored."
  :type 'boolean
  :group 'vhdl)

(defcustom vhdl-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented."
  :type 'boolean
  :group 'vhdl)

(defcustom vhdl-basic-offset 2
  "*Amount of basic offset used by + and - symbols in `vhdl-offsets-alist'."
  :type 'integer
  :group 'vhdl)

(defconst vhdl-offsets-alist-default
  '((string                . -1000)
    (block-open            . 0)
    (block-close           . 0)
    (statement             . 0)
    (statement-cont        . vhdl-lineup-statement-cont)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (case-alternative      . +)
    (comment               . vhdl-lineup-comment)
    (arglist-intro         . vhdl-lineup-arglist-intro)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . vhdl-lineup-arglist)
    (arglist-close         . vhdl-lineup-arglist)
    (entity                . 0)
    (configuration         . 0)
    (package               . 0)
    (architecture          . 0)
    (package-body          . 0)
    )
  "Default settings for offsets of syntactic elements.
Do not change this constant!  See the variable `vhdl-offsets-alist' for
more information.")

(defcustom vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default)
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, vhdl-mode first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `vhdl-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is call the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, vhdl-mode
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `vhdl-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `vhdl-offsets-alist',
an error is generated if `vhdl-strict-syntax-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', or `--'.  These latter
designate positive or negative multiples of `vhdl-basic-offset',
respectively: *1, *-1, *2, and *-2. If OFFSET is a function, it is
called with a single argument containing the cons of the syntactic
element symbol and the relative indent point.  The function should
return an integer offset.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 block-open             -- statement block open
 block-close            -- statement block close
 statement              -- a VHDL statement
 statement-cont         -- a continuation of a VHDL statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case alternative block
 case-alternative       -- a case statement alternative clause
 comment                -- a line containing only a comment
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 entity                 -- inside an entity declaration
 configuration          -- inside a configuration declaration
 package                -- inside a package declaration
 architecture           -- inside an architecture body
 package-body           -- inside a package body
"
  :type 'sexp
  :group 'vhdl)

(defcustom vhdl-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil,
hitting TAB indents the current line if point is at the left margin or
in the line's indentation, otherwise it insert a real tab character.
If other than nil or t, then tab is inserted only within literals
-- defined as comments and strings -- and inside preprocessor
directives, but line is always reindented.

Note that indentation of lines containing only comments is also
controlled by the `vhdl-comment-only-line-offset' variable."
  :type '(radio (const :tag "Always indent" t)
		(const :tag "Indent if point in indentation" nil)
		(sexp :format "%t\n"
		      :tag "Insert if point within literals" other))
  :group 'vhdl)

(defcustom vhdl-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)"
  :type '(choice integer
		 (cons :value (0 . 0)
		       (integer :tag "Non-anchored offset")
		       (integer :tag "Anchored offset")))
  :group 'vhdl)

(defcustom vhdl-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode."
  :type 'hook
  :group 'vhdl)

(defvar vhdl-style-alist
  '(("IEEE"
     (vhdl-basic-offset . 4)
     (vhdl-offsets-alist . ())
     )
    )
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any vhdl-mode variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `vhdl-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `vhdl-offsets-alist'.  These are passed directly to
`vhdl-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.")

;; dynamically append the default value of most variables
(or (assoc "Default" vhdl-style-alist)
    (let* ((varlist '(vhdl-inhibit-startup-warnings-p
		      vhdl-strict-syntax-p
		      vhdl-echo-syntactic-information-p
		      vhdl-basic-offset
		      vhdl-offsets-alist
		      vhdl-tab-always-indent
		      vhdl-comment-only-line-offset))
	   (default (cons "Default"
			  (mapcar
			   (function
			    (lambda (var)
			      (cons var (symbol-value var))
			      ))
			   varlist))))
      (setq vhdl-style-alist (cons default vhdl-style-alist))))

(defvar vhdl-mode-hook nil
  "*Hook called by `vhdl-mode'.")

(defvar vhdl-mode-menu
  '(["Comment Out Region"     comment-region (mark)]
    ;; ["Indent Expression"      vhdl-indent-exp
    ;; (memq (following-char) '(?\( ?\[ ?\{))]
    ["Indent Line"            vhdl-indent-command t]
    ["Backward Statement"     vhdl-beginning-of-statement t]
    ;; ["Forward Statement"      vhdl-end-of-statement t]
    )
  "XEmacs 19 (formerly Lucid) menu for VHDL mode.")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT


;; Emacs variant handling, and standard mode variables and functions:

(defconst vhdl-emacs-features
  (let ((major (and (boundp 'emacs-major-version)
		    emacs-major-version))
	(minor (and (boundp 'emacs-minor-version)
		    emacs-minor-version))
	flavor)
    ;; figure out version numbers if not already discovered
    (and (or (not major) (not minor))
	 (string-match "\\([0-9]+\\).\\([0-9]+\\)" emacs-version)
	 (setq major (string-to-int (substring emacs-version
					       (match-beginning 1)
					       (match-end 1)))
	       minor (string-to-int (substring emacs-version
					       (match-beginning 2)
					       (match-end 2)))))
    (if (not (and major minor))
	(error "Cannot figure out the major and minor version numbers."))
    ;; calculate the major version
    (cond
     ((= major 18)  (setq major 'v18))	;Emacs 18
     ((= major 4)   (setq major 'v18))	;Epoch 4
     ((= major 19)  (setq major 'v19	;Emacs 19
			  flavor (cond
				  ((string-match "Win-Emacs" emacs-version)
				   'Win-Emacs)
				  ((or (string-match "Lucid" emacs-version)
				       (string-match "XEmacs" emacs-version))
				   'XEmacs)
				  (t
				   'FSF))))
     ((>= major 20) (setq major 'v20	;Emacs 20 or later
			  flavor (if (string-match "XEmacs" emacs-version)
				     'XEmacs
				   'FSF)))
     ;; I don't know
     (t (error "Cannot recognize major version number: %s" major)))
    ;; lets do some minimal sanity checking.
    (if (and (or
	      ;; Emacs 18 is brain dead
	      (eq major 'v18)
	      ;; Lemacs before 19.6 had bugs
	      (and (eq major 'v19) (eq flavor 'XEmacs) (< minor 6))
	      ;; FSF 19 before 19.21 had bugs
	      (and (eq major 'v19) (eq flavor 'FSF) (< minor 21)))
	     (not vhdl-inhibit-startup-warnings-p))
	(with-output-to-temp-buffer "*vhdl-mode warnings*"
	  (print (format
"The version of Emacs that you are running, %s,
has known bugs in its syntax.c parsing routines which will affect the
performance of vhdl-mode. You should strongly consider upgrading to the
latest available version.  vhdl-mode may continue to work, after a
fashion, but strange indentation errors could be encountered."
		     emacs-version))))
    (list major flavor))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by vhdl-mode.  Here's the current
supported list, along with the values for this variable:

 Emacs 18/Epoch 4:           (v18)
 XEmacs (formerly Lucid) 19: (v19 XEmacs)
 Win-Emacs 1.35:             (V19 Win-Emacs)
 FSF Emacs 19:               (v19 FSF).")

(defvar vhdl-mode-abbrev-table nil
  "Abbrev table in use in vhdl-mode buffers.")
(define-abbrev-table 'vhdl-mode-abbrev-table ())

(defvar vhdl-mode-map ()
  "Keymap used in vhdl-mode buffers.")
(if vhdl-mode-map
    ()
  ;; TBD: should we even worry about naming this keymap. My vote: no,
  ;; because FSF and XEmacs (formerly Lucid) do it differently.
  (setq vhdl-mode-map (make-sparse-keymap))
  ;; put standard keybindings into MAP
  (define-key vhdl-mode-map "\M-a"	'vhdl-beginning-of-statement)
  ;;(define-key vhdl-mode-map "\M-e"	'vhdl-end-of-statement)
  (define-key vhdl-mode-map "\M-\C-f"   'vhdl-forward-sexp)
  (define-key vhdl-mode-map "\M-\C-b"   'vhdl-backward-sexp)
  (define-key vhdl-mode-map "\M-\C-u"	'vhdl-backward-up-list)
  ;;(define-key vhdl-mode-map "\M-\C-d"	'vhdl-down-list)
  (define-key vhdl-mode-map "\M-\C-a"	'vhdl-beginning-of-defun)
  (define-key vhdl-mode-map "\M-\C-e"	'vhdl-end-of-defun)
  (define-key vhdl-mode-map "\M-\C-h"	'vhdl-mark-defun)
  (define-key vhdl-mode-map "\M-\C-q"	'vhdl-indent-sexp)
  (define-key vhdl-mode-map "\t"        'vhdl-indent-command)
  (define-key vhdl-mode-map "\177"      'backward-delete-char-untabify)
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key vhdl-mode-map "\C-c\C-b"  'vhdl-submit-bug-report)
  (define-key vhdl-mode-map "\C-c\C-c"  'comment-region)
  (define-key vhdl-mode-map "\C-c\C-o"  'vhdl-set-offset)
  (define-key vhdl-mode-map "\C-c\C-r"  'vhdl-regress-line)
  (define-key vhdl-mode-map "\C-c\C-s"  'vhdl-show-syntactic-information)
  (define-key vhdl-mode-map "\C-c\C-v"  'vhdl-version)
  ;; in XEmacs (formerly Lucid) 19, we want the menu to popup when
  ;; the 3rd button is hit.  In 19.10 and beyond this is done
  ;; automatically if we put the menu on mode-popup-menu variable,
  ;; see c-common-init. RMS decided that this feature should not be
  ;; included for FSF's Emacs.
  (if (and (boundp 'current-menubar)
	   (not (boundp 'mode-popup-menu)))
      (define-key vhdl-mode-map 'button3 'vhdl-popup-menu))
  )

(defvar vhdl-mode-syntax-table nil
  "Syntax table used in vhdl-mode buffers.")
(if vhdl-mode-syntax-table
    ()
  (setq vhdl-mode-syntax-table (make-syntax-table))
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?\" "\""    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\$ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\% "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\& "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\' "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\( "()"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\) ")("    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\* "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\. "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\/ "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\: "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\; "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\< "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\= "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\> "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\] ")["    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\| "."     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\} "){"    vhdl-mode-syntax-table)
  ;; add comment syntax
  (modify-syntax-entry ?\- ". 12"  vhdl-mode-syntax-table)
  (modify-syntax-entry ?\n ">"     vhdl-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"    vhdl-mode-syntax-table))

(defvar vhdl-syntactic-context nil
  "Buffer local variable containing syntactic analysis list.")
(make-variable-buffer-local 'vhdl-syntactic-context)

;; Support for outline modes

(defconst vhdl-outline-regexp
  (concat "\\(entity\\)\\|\\(package\\)\\|"
	  "\\( *procedure\\)\\|\\( *function\\)\\|"
	  "\\( *component\\)\\|\\(architecture\\)\\|"
	  "\\(package body\\)\\|\\( *[A-Za-z][A-Za-z0-9_]* : block\\)\\|"
	  "\\( *[A-Za-z][A-Za-z0-9_]* : process\\)\\|\\(configuration\\)"))

(defun vhdl-outline-level ()		; was copied from c-outline-level
  (save-excursion
    (skip-chars-forward "\t ")
    (current-column)))

;; Support for font-lock

(defconst vhdl-font-lock-keywords-1
  (purecopy
   (list
    ;; Highlight names of common constructs
    (list
     (concat
      "^[ \t]*\\(entity\\|architecture\\|configuration\\|function\\|"
      "procedure\\|component\\|package[ \t]+body\\|package\\|"
      "end[ \t]+\\(block\\|process\\|case\\|generate\\|loop\\)\\)[ \t]+"
      "\\(\\(\\w\\|\\s_\\)+\\)")
     3 'font-lock-function-name-face)
    
    ;; Highlight labels of common constructs
    (list
     (concat
      "^[ \t]*\\(\\(\\w\\|\\s_\\)+\\)[ \t]*:[ \t\n]*\\(block\\|process\\|"
      "if\\|for\\|case\\|exit\\|loop\\|next\\|null\\|with\\|"
      "\\(\\w\\|\\s_\\)+[ \t\n]+port[ \t]+map\\)\\>[^_]")
     1 'font-lock-function-name-face)
    
    ;; Highlight OF labels
    (list
     (concat
      "^[ \t]*\\(configuration\\|architecture\\|attribute\\)[ \t]+"
      "\\(\\(\\w\\|\\s_\\)+\\)[ \t]+of[ \t]+\\(\\(\\w\\|\\s_\\)+\\)")
     4 'font-lock-function-name-face)
    
    ;; Fontify library useage clauses.
    (list
     (concat
      "[^\\s_]\\<\\(library\\|use\\)[ \t\n]+\\(entity[ \t\n]+\\)?"
      "\\(\\(\\w\\|\\s_\\|[\.()]\\)+\\)")
     3 'font-lock-function-name-face)
    ))
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does fairly subdued highlighting of function names.")

(defconst vhdl-font-lock-keywords-2
  (purecopy
   (append
    vhdl-font-lock-keywords-1
    (list
     (list
      (concat
       "[^\\s_]\\<\\("
       (mapconcat
	'identity
	'(
	  ;; the following is a list of all reserved words known in VHDL'93
	  "abs" "access" "after" "alias" "all" "and" "assert"
	  "architecture" "array" "attribute"
	  "begin" "block" "body" "buffer" "bus"
	  "case" "component" "configuration" "constant"
	  "disconnect" "downto"
	  "else" "elsif" "end" "entity" "exit"
	  "file" "for" "function"
	  "generate" "generic" "group" "guarded"
	  "if" "impure" "in" "inertial" "inout" "is"
	  "label" "library" "linkage" "literal" "loop" 
	  "map" "mod" 
	  "nand" "new" "next" "nor" "not" "null"
	  "of" "on" "open" "or" "others" "out"
	  "package" "port" "postponed" "procedure" "process" "pure"
	  "range" "record" "register" "reject" "rem" "report" "return"
	  "rol" "ror"
	  "select" "severity" "signal" "shared" "sla" "sll" "sra" "srl"
	  "subtype"
	  "then" "to" "transport" "type" 
	  "unaffected" "units" "until" "use"
	  "variable" "wait" "when" "while" "with" 
	  "xnor" "xor"
	  "note" "warning" "error" "failure"
	  ;; the following list contains predefined attributes
	  "base" "left" "right" "high" "low" "pos" "val" "succ"
	  "pred" "leftof" "rightof" "range" "reverse_range"
	  "length" "delayed" "stable" "quiet" "transaction"
	  "event" "active" "last_event" "last_active" "last_value"
	  "driving" "driving_value" "ascending" "value" "image"
	  "simple_name" "instance_name" "path_name"
	  "foreign"
	  ;; the following list contains standardized types
	  "boolean" "bit" "bit_vector" "character" "severity_level" "integer"
	  "real" "time" "natural" "positive" "string" "text" "line"
	  "unsigned" "signed"
	  "std_logic" "std_logic_vector"
	  "std_ulogic" "std_ulogic_vector"
	  )
	"\\|")
       "\\)\\>[^\\s_]")
      1 'font-lock-keyword-face)
     )))
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does a lot more highlighting.")

;; The keywords in the preceding lists assume case-insensitivity.
(put 'vhdl-mode 'font-lock-keywords-case-fold-search t)

(defvar vhdl-font-lock-keywords vhdl-font-lock-keywords-1
  "Additional expressions to highlight in VHDL mode.")

;; This should eventually be subsumed into the respective functions in
;; the source for "font-lock.el".
(if (featurep 'advice)
    (progn
      (defadvice font-lock-use-default-minimal-decoration
	(before vhdl-mode activate)
	"Do it for VHDL mode too."
	(setq vhdl-font-lock-keywords vhdl-font-lock-keywords-1))
      
      (defadvice font-lock-use-default-maximal-decoration
	(before vhdl-mode activate)
	"Do it for VHDL mode too."
	(setq vhdl-font-lock-keywords vhdl-font-lock-keywords-2))
      ))


;; Main entry point for VHDL mode:

;;;###autoload
(defun vhdl-mode ()
  "Major mode for editing VHDL code.
vhdl-mode $Revision: 1.7 $
To submit a problem report, enter `\\[vhdl-submit-bug-report]' from a
vhdl-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducable test case and send the message.

Note that the details of configuring vhdl-mode will soon be moved to the
accompanying texinfo manual.  Until then, please read the README file
that came with the vhdl-mode distribution.

The hook variable `vhdl-mode-hook' is run with no args, if that value is
bound and has a non-nil value.

Key bindings:
\\{vhdl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table vhdl-mode-syntax-table)
  (setq major-mode 'vhdl-mode
	mode-name "VHDL"
	local-abbrev-table vhdl-mode-abbrev-table)
  (use-local-map vhdl-mode-map)
  ;; set local variable values
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'vhdl-indent-line)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "--+ *")
  (set (make-local-variable 'outline-regexp) vhdl-outline-regexp)
  (set (make-local-variable 'outline-level) 'vhdl-outline-level)

  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (if (boundp 'comment-indent-function)
      (progn
	   (make-local-variable 'comment-indent-function)
	   (setq comment-indent-function 'vhdl-comment-indent))
    (make-local-variable 'comment-indent-hook)
    (setq comment-indent-hook 'vhdl-comment-indent))
  ;; put VHDL menu into menubar and on popup menu for XEmacs (formerly
  ;; Lucid) 19. I think this happens automatically for FSF Emacs 19.
  (if (and (boundp 'current-menubar)
	   current-menubar
	   (not (assoc mode-name current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil mode-name vhdl-mode-menu)))
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
	    (cons (concat mode-name " Mode Commands") vhdl-mode-menu)))
  (run-hooks 'vhdl-mode-hook))

;; menus for XEmacs (formerly Lucid)

(defun vhdl-popup-menu (e)
  "Pops up the VHDL menu."
  (interactive "@e")
  (popup-menu (cons (concat mode-name " Mode Commands") vhdl-mode-menu))
  (vhdl-keep-region-active))

;; active regions

(defun vhdl-keep-region-active ()
  ;; do whatever is necessary to keep the region active in XEmacs
  ;; (formerly Lucid). ignore byte-compiler warnings you might see
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;; constant regular expressions for looking at various constructs

(defconst vhdl-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a VHDL symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")

(defconst vhdl-case-alternative-key "when[( \t\n][^;=>]+=>"
  "Regexp describing a case statement alternative key.")

(defconst vhdl-case-header-key "case[( \t\n][^;=>]+[) \t\n]is"
  "Regexp describing a case statement header key.")

(defconst vhdl-label-key
  (concat vhdl-symbol-key "\\s-*:")
  "Regexp describing a VHDL label.")


;; Macro definitions:

(defmacro vhdl-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; eoi  -- last whitespace on line
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; 
  ;; This function does not modify point or mark.
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  (` (let ((here (point)))
       (,@ (cond
	    ((eq position 'bol)  '((beginning-of-line)))
	    ((eq position 'eol)  '((end-of-line)))
	    ((eq position 'bod)  '((save-match-data
				     (vhdl-beginning-of-defun))))
	    ((eq position 'boi)  '((back-to-indentation)))
	    ((eq position 'eoi)  '((end-of-line)(skip-chars-backward " \t")))
	    ((eq position 'bonl) '((forward-line 1)))
	    ((eq position 'bopl) '((forward-line -1)))
	    ((eq position 'iopl)
	     '((forward-line -1)
	       (back-to-indentation)))
	    ((eq position 'ionl)
	     '((forward-line 1)
	       (back-to-indentation)))
	    (t (error "unknown buffer position requested: %s" position))
	    ))
       (prog1
	   (point)
	 (goto-char here))
       ;; workaround for an Emacs18 bug -- blech! Well, at least it
       ;; doesn't hurt for v19
       (,@ nil)
       )))

(defmacro vhdl-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defmacro vhdl-add-syntax (symbol &optional relpos)
  ;; a simple macro to append the syntax in symbol to the syntax list.
  ;; try to increase performance by using this macro
  (` (setq vhdl-syntactic-context
	   (cons (cons (, symbol) (, relpos)) vhdl-syntactic-context))))

(defmacro vhdl-has-syntax (symbol)
  ;; a simple macro to return check the syntax list.
  ;; try to increase performance by using this macro
  (` (assoc (, symbol) vhdl-syntactic-context)))


;; Syntactic element offset manipulation:

(defun vhdl-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; legal value only
  (let ((oldoff (format "%s" (cdr-safe (assq langelem vhdl-offsets-alist))))
	(errmsg "Offset must be int, func, var, or one of +, -, ++, --: ")
	(prompt "Offset: ")
	offset input interned)
    (while (not offset)
      (setq input (read-string prompt oldoff)
	    offset (cond ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

(defun vhdl-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `vhdl-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`vhdl-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)))
		     vhdl-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (vhdl-get-syntactic-context))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      (if (or (memq 'v19 vhdl-emacs-features)
			      (memq 'v20 vhdl-emacs-features))
			  (cons ic 0)
			ic))
		    )))
	  (offset (vhdl-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (integerp offset)
      (fboundp offset)
      (boundp offset)
      (error "Offset must be int, func, var, or one of +, -, ++, --: %s"
	     offset))
  (let ((entry (assq symbol vhdl-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq vhdl-offsets-alist (cons (cons symbol offset) vhdl-offsets-alist))
	(error "%s is not a valid syntactic symbol." symbol))))
  (vhdl-keep-region-active))

(defun vhdl-set-style (style &optional local)
  "Set vhdl-mode variables to use one of several different indentation styles.
STYLE is a string representing the desired style and optional LOCAL is
a flag which, if non-nil, means to make the style variables being
changed buffer local, instead of the default, which is to set the
global variables.  Interactively, the flag comes from the prefix
argument.  The styles are chosen from the `vhdl-style-alist' variable."
  (interactive (list (completing-read "Use which VHDL indentation style? "
                                      vhdl-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style vhdl-style-alist))))
    (or vars
	(error "Invalid VHDL indentation style `%s'" style))
    ;; set all the variables
    (mapcar
     (function
      (lambda (varentry)
	(let ((var (car varentry))
	      (val (cdr varentry)))
	  (and local
	       (make-local-variable var))
	  ;; special case for vhdl-offsets-alist
	  (if (not (eq var 'vhdl-offsets-alist))
	      (set var val)
	    ;; reset vhdl-offsets-alist to the default value first
	    (setq vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default))
	    ;; now set the langelems that are different
	    (mapcar
	     (function
	      (lambda (langentry)
		(let ((langelem (car langentry))
		      (offset (cdr langentry)))
		  (vhdl-set-offset langelem offset)
		  )))
	     val))
	  )))
     vars))
  (vhdl-keep-region-active))

(defun vhdl-add-style (style descrip &optional set-p)
  "Adds a style to `vhdl-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ((VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `vhdl-style-alist' for the semantics of VARIABLE and
VALUE.  This function also sets the current style to STYLE using
`vhdl-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " vhdl-style-alist))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style vhdl-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq vhdl-style-alist (cons (cons style descrip) vhdl-style-alist))))
  (and set-p (vhdl-set-style style)))

(defun vhdl-get-offset (langelem)
  ;; Get offset from LANGELEM which is a cons cell of the form:
  ;; (SYMBOL . RELPOS).  The symbol is matched against
  ;; vhdl-offsets-alist and the offset found there is either returned,
  ;; or added to the indentation at RELPOS.  If RELPOS is nil, then
  ;; the offset is simply returned.
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vhdl-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if vhdl-strict-syntax-p
	  (error "don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)  (setq offset vhdl-basic-offset))
     ((eq offset '-)  (setq offset (- vhdl-basic-offset)))
     ((eq offset '++) (setq offset (* 2 vhdl-basic-offset)))
     ((eq offset '--) (setq offset (* 2 (- vhdl-basic-offset))))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (vhdl-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))


;; Syntactic support functions:

;; Returns `comment' if in a comment, `string' if in a string literal,
;; or nil if not in a literal at all.  Optional LIM is used as the
;; backward limit of the search.  If omitted, or nil, (point-min) is
;; used.

(defun vhdl-in-literal (&optional lim)
  ;; Determine if point is in a VHDL literal.
  (save-excursion
    (let* ((lim (or lim (point-min)))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))
    ))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-il (&optional lim)
  ;; Determine if point is in a VHDL literal
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (vhdl-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "--\\|[\"']"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a VHDL style comment
	       ((string= "--" match)
		(if (<= here (progn (end-of-line) (point))) 'comment))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       (t nil)))
	) ; end-while
      state)))

(and (memq 'Win-Emacs vhdl-emacs-features)
     (fset 'vhdl-in-literal 'vhdl-win-il))

;; Skipping of "syntactic whitespace".  Syntactic whitespace is
;; defined as lexical whitespace or comments.  Search no farther back
;; or forward than optional LIM.  If LIM is omitted, (point-min) is
;; used for backward skipping, (point-max) is used for forward
;; skipping.

(defun vhdl-forward-syntactic-ws (&optional lim)
  ;; Forward skip of syntactic whitespace.
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-fsws (&optional lim)
  ;; Forward skip syntactic whitespace for Win-Emacs.
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((looking-at "--") (end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (memq 'Win-Emacs vhdl-emacs-features)
     (fset 'vhdl-forward-syntactic-ws 'vhdl-win-fsws))

(defun vhdl-backward-syntactic-ws (&optional lim)
  ;; Backward skip over syntactic whitespace.
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      )))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-bsws (&optional lim)
  ;; Backward skip syntactic whitespace for Win-Emacs.
  (let ((lim (or lim (vhdl-point 'bod)))
	stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((eq (vhdl-in-literal lim) 'comment)
	(skip-chars-backward "^-" lim)
	(skip-chars-backward "-" lim)
	(while (not (or (and (= (following-char) ?-)
			     (= (char-after (1+ (point))) ?-))
			(<= (point) lim)))
	  (skip-chars-backward "^-" lim)
	  (skip-chars-backward "-" lim)))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (memq 'Win-Emacs vhdl-emacs-features)
    (fset 'vhdl-backward-syntactic-ws 'vhdl-win-bsws))

;; Functions to help finding the correct indentation column:

(defun vhdl-first-word (point)
  "If the keyword at POINT is at boi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (eq (point) (vhdl-point 'boi))
	 (current-column))))

(defun vhdl-last-word (point)
  "If the keyword at POINT is at eoi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (save-excursion (or (eq (progn (forward-sexp) (point))
				 (vhdl-point 'eoi))
			     (looking-at "\\s-*\\(--\\)?")))
	 (current-column))))


;; Core syntactic evaluation functions:

(defconst vhdl-libunit-re
  "\\b\\(architecture\\|configuration\\|entity\\|package\\)\\b[^_]")

(defun vhdl-libunit-p ()
  (and
   (save-excursion
     (forward-sexp)
     (skip-chars-forward " \t\n")
     (not (looking-at "is\\b[^_]")))
   (save-excursion
     (backward-sexp)
     (not (looking-at "use\\b[^_]")))))

(defconst vhdl-defun-re
  "\\b\\(architecture\\|block\\|configuration\\|entity\\|package\\|process\\|procedure\\|function\\)\\b[^_]")

(defun vhdl-defun-p ()
  (save-excursion
    (if (looking-at "block\\|process")
	;; "block", "process":
	(save-excursion
	  (backward-sexp)
	  (not (looking-at "end\\s-+\\w")))
      ;; "architecture", "configuration", "entity",
      ;; "package", "procedure", "function":
      t)))
  
(defun vhdl-corresponding-defun ()
  "If the word at the current position corresponds to a \"defun\"
keyword, then return a string that can be used to find the
corresponding \"begin\" keyword, else return nil."
  (save-excursion
    (and (looking-at vhdl-defun-re)
	 (vhdl-defun-p)
	 (if (looking-at "block\\|process")
	     ;; "block", "process":
	     (buffer-substring (match-beginning 0) (match-end 0))
	   ;; "architecture", "configuration", "entity", "package",
	   ;; "procedure", "function":
	   "is"))))

(defconst vhdl-begin-fwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|units\\|record\\|for\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"begin\" keywords.")

(defconst vhdl-begin-bwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|units\\|record\\|for\\)\\b"
  "A regular expression for searching backward that matches all known
\"begin\" keywords.")

(defun vhdl-begin-p (&optional lim)
  "Return t if we are looking at a real \"begin\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-begin-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"begin\"
keyword."
  (cond
   ;; "[architecture|case|configuration|entity|package|
   ;;   procedure|function] ... is":
   ((and (looking-at "i")
	 (save-excursion
	   ;; Skip backward over first sexp (needed to skip over a
	   ;; procedure interface list, and is harmless in other
	   ;; situations).  Note that we need "return" in the
	   ;; following search list so that we don't run into
	   ;; semicolons in the function interface list.
	   (backward-sexp)
	   (let (foundp)
	     (while (and (not foundp)
			 (re-search-backward
			  ";\\|\\b\\(architecture\\|case\\|configuration\\|entity\\|package\\|procedure\\|return\\|is\\|begin\\|process\\|block\\)\\b[^_]"
			  lim 'move))
	       (if (or (= (preceding-char) ?_)
		       (vhdl-in-literal lim))
		   (backward-char)
		 (setq foundp t))))
	   (and (/= (following-char) ?\;)
		(not (looking-at "is\\|begin\\|process\\|block")))))
    t)
   ;; "begin", "then":
   ((looking-at "be\\|t")
    t)
   ;; "else":
   ((and (looking-at "e")
	 ;; make sure that the "else" isn't inside a
	 ;; conditional signal assignment.
	 (save-excursion
	   (re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	   (or (eq (following-char) ?\;)
	       (eq (point) lim))))
    t)
   ;; "block", "component", "generate", "loop", "process",
   ;; "units", "record":
   ((and (looking-at "bl\\|[cglpur]")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "for" (inside configuration declaration):
   ((and (looking-at "f")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 (vhdl-has-syntax 'configuration))
    t)
   ))
  
(defun vhdl-corresponding-mid (&optional lim)
  (cond
   ((looking-at "is\\|block\\|process")
    "begin")
   ((looking-at "then")
    "<else>")
   (t
    "end")))

(defun vhdl-corresponding-end (&optional lim)
  "If the word at the current position corresponds to a \"begin\"
keyword, then return a vector containing enough information to find
the corresponding \"end\" keyword, else return nil.  The keyword to
search forward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain a \"begin\" keyword."
  (save-excursion
    (and (looking-at vhdl-begin-fwd-re)
	 (/= (preceding-char) ?_)
	 (not (vhdl-in-literal lim))
	 (vhdl-begin-p lim)
	 (cond
	  ;; "is", "generate", "loop":
	  ((looking-at "[igl]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "begin", "else", "for":
	  ((looking-at "be\\|[ef]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "component", "units", "record":
	  ((looking-at "[cur]")
	   ;; The first end found will close the block
	   (vector "end" nil))
	  ;; "block", "process":
	  ((looking-at "bl\\|p")
	   (vector "end"
		   (or (vhdl-first-word (point))
		       (save-excursion
			 (vhdl-beginning-of-statement-1 lim)
			 (vhdl-backward-skip-label lim)
			 (vhdl-first-word (point))))))
	  ;; "then":
	  ((looking-at "t")
	   (vector "elsif\\|else\\|end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ))))

(defconst vhdl-end-fwd-re "\\b\\(end\\|else\\|elsif\\)\\b\\([^_]\\|\\'\\)")

(defconst vhdl-end-bwd-re "\\b\\(end\\|else\\|elsif\\)\\b")

(defun vhdl-end-p (&optional lim)
  "Return t if we are looking at a real \"end\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-end-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain an \"end\"
keyword."
  (or (not (looking-at "else"))
      ;; make sure that the "else" isn't inside a conditional signal
      ;; assignment.
      (save-excursion
	(re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	(or (eq (following-char) ?\;)
	    (eq (point) lim)))))

(defun vhdl-corresponding-begin (&optional lim)
  "If the word at the current position corresponds to an \"end\"
keyword, then return a vector containing enough information to find
the corresponding \"begin\" keyword, else return nil.  The keyword to
search backward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.  The supplementary
keyword to search forward for is aref 2 or nil if this is not
required.  If aref 3 is t, then the \"begin\" keyword may be found in
the middle of a statement.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain an \"end\" keyword."
  (save-excursion
    (let (pos)
      (if (and (looking-at vhdl-end-fwd-re)
	       (not (vhdl-in-literal lim))
	       (vhdl-end-p lim))
	  (if (looking-at "el")
	      ;; "else", "elsif":
	      (vector "if\\|elsif" (vhdl-first-word (point)) "then" nil)
	    ;; "end ...":
	    (setq pos (point))
	    (forward-sexp)
	    (skip-chars-forward " \t\n")
	    (cond
	     ;; "end if":
	     ((looking-at "if\\b[^_]")
	      (vector "else\\|elsif\\|if"
		      (vhdl-first-word pos)
		      "else\\|then" nil))
	     ;; "end component":
	     ((looking-at "component\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil nil))
	     ;; "end units", "end record":
	     ((looking-at "\\(units\\|record\\)\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil t))
	     ;; "end block", "end process":
	     ((looking-at "\\(block\\|process\\)\\b[^_]")
	      (vector "begin" (vhdl-first-word pos) nil nil))
	     ;; "end case":
	     ((looking-at "case\\b[^_]")
	      (vector "case" (vhdl-first-word pos) "is" nil))
	     ;; "end generate":
	     ((looking-at "generate\\b[^_]")
	      (vector "generate\\|for\\|if"
		      (vhdl-first-word pos)
		      "generate" nil))
	     ;; "end loop":
	     ((looking-at "loop\\b[^_]")
	      (vector "loop\\|while\\|for"
		      (vhdl-first-word pos)
		      "loop" nil))
	     ;; "end for" (inside configuration declaration):
	     ((looking-at "for\\b[^_]")
	      (vector "for" (vhdl-first-word pos) nil nil))
	     ;; "end [id]":
	     (t
	      (vector "begin\\|architecture\\|configuration\\|entity\\|package\\|procedure\\|function"
		      (vhdl-first-word pos)
		      ;; return an alist of (statement . keyword) mappings
		      '(
			;; "begin ... end [id]":
			("begin"          . nil)
			;; "architecture ... is ... begin ... end [id]":
			("architecture"   . "is")
			;; "configuration ... is ... end [id]":
			("configuration"  . "is")
			;; "entity ... is ... end [id]":
			("entity"         . "is")
			;; "package ... is ... end [id]":
			("package"        . "is")
			;; "procedure ... is ... begin ... end [id]":
			("procedure"      . "is")
			;; "function ... is ... begin ... end [id]":
			("function"       . "is")
			)
		      nil))
	     ))) ; "end ..."
      )))

(defconst vhdl-leader-re
  "\\b\\(block\\|component\\|process\\|for\\)\\b[^_]")

(defun vhdl-end-of-leader ()
  (save-excursion
    (cond ((looking-at "block\\|process")
	   (if (save-excursion
		 (forward-sexp)
		 (skip-chars-forward " \t\n")
		 (= (following-char) ?\())
	       (forward-sexp 2)
	     (forward-sexp))
	   (point))
	  ((looking-at "component")
	   (forward-sexp 2)
	   (point))
	  ((looking-at "for")
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (while (looking-at "[,:(]")
	     (forward-sexp)
	     (skip-chars-forward " \t\n"))
	   (point))
	  (t nil)
	  )))

(defconst vhdl-trailer-re
  "\\b\\(is\\|then\\|generate\\|loop\\)\\b[^_]")

(defconst vhdl-statement-fwd-re
  "\\b\\(if\\|for\\|while\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"statement\" keywords.")

(defconst vhdl-statement-bwd-re
  "\\b\\(if\\|for\\|while\\)\\b"
  "A regular expression for searching backward that matches all known
\"statement\" keywords.")

(defun vhdl-statement-p (&optional lim)
  "Return t if we are looking at a real \"statement\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-statement-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"statement\"
keyword."
  (cond
   ;; "for" ... "generate":
   ((and (looking-at "f")
	 ;; Make sure it's the start of a parameter specification.
	 (save-excursion
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (looking-at "in\\b[^_]"))
	 ;; Make sure it's not an "end for".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "if" ... "then", "if" ... "generate", "if" ... "loop":
   ((and (looking-at "i")
	 ;; Make sure it's not an "end if".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "while" ... "loop":
   ((looking-at "w")
    t)
   ))
  

;; Core syntactic movement functions:

(defconst vhdl-b-t-b-re
  (concat vhdl-begin-bwd-re "\\|" vhdl-end-bwd-re))

(defun vhdl-backward-to-block (&optional lim)
  "Move backward to the previous \"begin\" or \"end\" keyword."
  (let (foundp)
    (while (and (not foundp)
		(re-search-backward vhdl-b-t-b-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; "begin" keyword:
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p lim))
	  (setq foundp 'begin))
	 ;; "end" keyword:
	 ((and (looking-at vhdl-end-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-end-p lim))
	  (setq foundp 'end))
	 ))
      )
    foundp
    ))

(defun vhdl-forward-sexp (&optional count lim)
  "Move forward across one balanced expression (sexp).
With COUNT, do it that many times."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	end-vec target)
    (save-excursion
      (while (> count 0)
	;; skip whitespace
	(skip-chars-forward " \t\n")
	;; Check for an unbalanced "end" keyword
	(if (and (looking-at vhdl-end-fwd-re)
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim))
		 (vhdl-end-p lim)
		 (not (looking-at "else")))
	    (error
	     "Containing expression ends prematurely in vhdl-forward-sexp"))
	;; If the current keyword is a "begin" keyword, then find the
	;; corresponding "end" keyword.
	(if (setq end-vec (vhdl-corresponding-end lim))
	    (let (
		  ;; end-re is the statement keyword to search for
		  (end-re
		   (concat "\\b\\(" (aref end-vec 0) "\\)\\b\\([^_]\\|\\'\\)"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref end-vec 1))
		  (eol (vhdl-point 'eol))
		  foundp literal placeholder)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-forward end-re nil t)
			  (setq placeholder (match-end 1))
			  (goto-char (match-beginning 0)))
		;; If we are in a literal, or not in the right target
		;; column and not on the same line as the begin, then
		;; try again.
		(if (or (and column
			     (/= (current-indentation) column)
			     (> (point) eol))
			(= (preceding-char) ?_)
			(setq literal (vhdl-in-literal lim)))
		    (if (eq literal 'comment)
			(end-of-line)
		      (forward-char))
		  ;; An "else" keyword corresponds to both the opening brace
		  ;; of the following sexp and the closing brace of the
		  ;; previous sexp.
		  (if (not (looking-at "else"))
		      (goto-char placeholder))
		  (setq foundp t))
		)
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-forward-sexp"))
	      )
	  ;; If the current keyword is not a "begin" keyword, then just
	  ;; perform the normal forward-sexp.
	  (forward-sexp)
	  )
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-sexp (&optional count lim)
  "Move backward across one balanced expression (sexp).
With COUNT, do it that many times.  LIM bounds any required backward
searches."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	begin-vec target)
    (save-excursion
      (while (> count 0)
	;; Perform the normal backward-sexp, unless we are looking at
	;; "else" - an "else" keyword corresponds to both the opening brace
	;; of the following sexp and the closing brace of the previous sexp.
	(if (and (looking-at "else\\b\\([^_]\\|\\'\\)")
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim)))
	    nil
	  (backward-sexp)
	  (if (and (looking-at vhdl-begin-fwd-re)
		   (/= (preceding-char) ?_)
		   (not (vhdl-in-literal lim))
		   (vhdl-begin-p lim))
	      (error "Containing expression ends prematurely in vhdl-backward-sexp")))
	;; If the current keyword is an "end" keyword, then find the
	;; corresponding "begin" keyword.
	(if (and (setq begin-vec (vhdl-corresponding-begin lim))
		 (/= (preceding-char) ?_))
	    (let (
		  ;; begin-re is the statement keyword to search for
		  (begin-re
		   (concat "\\b\\(" (aref begin-vec 0) "\\)\\b[^_]"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref begin-vec 1))
		  ;; internal-p controls where the statement keyword can
		  ;; be found.
		  (internal-p (aref begin-vec 3))
		  (last-backward (point)) last-forward
		  foundp literal keyword)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-backward begin-re lim t)
			  (setq keyword
				(buffer-substring (match-beginning 1)
						  (match-end 1))))
		;; If we are in a literal or in the wrong column,
		;; then try again.
		(if (or (and column
			     (and (/= (current-indentation) column)
				  ;; possibly accept current-column as
				  ;; well as current-indentation.
				  (or (not internal-p)
				      (/= (current-column) column))))
			(= (preceding-char) ?_)
			(vhdl-in-literal lim))
		    (backward-char)
		  ;; If there is a supplementary keyword, then
		  ;; search forward for it. 
		  (if (and (setq begin-re (aref begin-vec 2))
			   (or (not (listp begin-re))
			       ;; If begin-re is an alist, then find the
			       ;; element corresponding to the actual
			       ;; keyword that we found.
			       (progn
				 (setq begin-re
				       (assoc keyword begin-re))
				 (and begin-re
				      (setq begin-re (cdr begin-re))))))
		      (and
		       (setq begin-re
			     (concat "\\b\\(" begin-re "\\)\\b[^_]"))
		       (save-excursion
			 (setq last-forward (point))
			 ;; Look for the supplementary keyword
			 ;; (bounded by the backward search start
			 ;; point).
			 (while (and (not foundp)
				     (re-search-forward begin-re
							last-backward t)
				     (goto-char (match-beginning 1)))
			   ;; If we are in a literal, then try again.
			   (if (or (= (preceding-char) ?_)
				   (setq literal
					 (vhdl-in-literal last-forward)))
			       (if (eq literal 'comment)
				   (goto-char
				    (min (vhdl-point 'eol) last-backward))
				 (forward-char))
			     ;; We have found the supplementary keyword.
			     ;; Save the position of the keyword in foundp.
			     (setq foundp (point)))
			   )
			 foundp)
		       ;; If the supplementary keyword was found, then
		       ;; move point to the supplementary keyword.
		       (goto-char foundp))
		    ;; If there was no supplementary keyword, then
		    ;; point is already at the statement keyword.
		    (setq foundp t)))
		) ; end of the search for the statement keyword
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-backward-sexp"))
	      ))
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-up-list (&optional count limit)
  "Move backward out of one level of blocks.
With argument, do this that many times."
  (interactive "p")
  (let ((count (or count 1))
	target)
    (save-excursion
      (while (> count 0)
	(if (looking-at vhdl-defun-re)
	    (error "Unbalanced blocks"))
	(vhdl-backward-to-block limit)
	(setq count (1- count)))
      (setq target (point)))
    (goto-char target)))

(defun vhdl-end-of-defun (&optional count)
  "Move forward to the end of a VHDL defun."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-beginning-of-defun)
    (if (not (looking-at "block\\|process"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)))
    
(defun vhdl-mark-defun ()
  "Put mark at end of this \"defun\", point at beginning."
  (interactive)
  (let ((case-fold-search t))
    (push-mark)
    (vhdl-beginning-of-defun)
    (push-mark)
    (if (not (looking-at "block\\|process"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)
    (exchange-point-and-mark)))

(defun vhdl-beginning-of-libunit ()
  "Move backward to the beginning of a VHDL library unit.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer." 
  ;; Note that if point is between the "libunit" keyword and the
  ;; corresponding "begin" keyword, then that libunit will not be
  ;; recognised, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognised.  The
  ;; returned point is at the first character of the "libunit" keyword.
  (let ((last-forward (point))
	(last-backward
	 ;; Just in case we are actually sitting on the "begin"
	 ;; keyword, allow for the keyword and an extra character,
	 ;; as this will be used when looking forward for the
	 ;; "begin" keyword.
	 (save-excursion (forward-word 1) (1+ (point))))
	foundp literal placeholder)
    ;; Find the "libunit" keyword.
    (while (and (not foundp)
		(re-search-backward vhdl-libunit-re nil 'move))
      ;; If we are in a literal, or not at a real libunit, then try again.
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal (point-min))
	      (not (vhdl-libunit-p)))
	  (backward-char)
	;; Find the corresponding "begin" keyword.
	(setq last-forward (point))
	(while (and (not foundp)
		    (re-search-forward "\\bis\\b[^_]" last-backward t)
		    (setq placeholder (match-beginning 0)))
	  (if (or (= (preceding-char) ?_)
		  (setq literal (vhdl-in-literal last-forward)))
	      ;; It wasn't a real keyword, so keep searching.
	      (if (eq literal 'comment)
		  (goto-char
		   (min (vhdl-point 'eol) last-backward))
		(forward-char))
	    ;; We have found the begin keyword, loop will exit.
	    (setq foundp placeholder)))
	;; Go back to the libunit keyword
	(goto-char last-forward)))
    foundp))
    
(defun vhdl-beginning-of-defun (&optional count)
  "Move backward to the beginning of a VHDL defun.
With argument, do it that many times.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer." 
  ;; Note that if point is between the "defun" keyword and the
  ;; corresponding "begin" keyword, then that defun will not be
  ;; recognised, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognised.  The
  ;; returned point is at the first character of the "defun" keyword.
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(last-forward (point))
	foundp)
    (while (> count 0)
      (setq foundp nil)
      (goto-char last-forward)
      (let ((last-backward
	     ;; Just in case we are actually sitting on the "begin"
	     ;; keyword, allow for the keyword and an extra character,
	     ;; as this will be used when looking forward for the
	     ;; "begin" keyword.
	     (save-excursion (forward-word 1) (1+ (point))))
	    begin-string literal)
	(while (and (not foundp)
		    (re-search-backward vhdl-defun-re nil 'move))
	  ;; If we are in a literal, then try again.
	  (if (or (= (preceding-char) ?_)
		  (vhdl-in-literal (point-min)))
	      (backward-char)
	    (if (setq begin-string (vhdl-corresponding-defun))
		;; This is a real defun keyword.
		;; Find the corresponding "begin" keyword.
		;; Look for the begin keyword.
		(progn
		  ;; Save the search start point.
		  (setq last-forward (point))
		  (while (and (not foundp)
			      (search-forward begin-string last-backward t))
		    (if (or (= (preceding-char) ?_)
			    (save-match-data
			      (setq literal (vhdl-in-literal last-forward))))
			;; It wasn't a real keyword, so keep searching.
			(if (eq literal 'comment)
			    (goto-char
			     (min (vhdl-point 'eol) last-backward))
			  (forward-char))
		      ;; We have found the begin keyword, loop will exit.
		      (setq foundp (match-beginning 0)))
		    )
		  ;; Go back to the defun keyword
		  (goto-char last-forward)) ; end search for begin keyword
	      ))
	  ) ; end of the search for the defun keyword
	)
      (setq count (1- count))
      )
    (vhdl-keep-region-active)
    foundp))
    
(defun vhdl-beginning-of-statement (&optional count lim)
  "Go to the beginning of the innermost VHDL statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 2 optional args: the
prefix arg, and a buffer position limit which is the farthest back to
search."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(lim (or lim (point-min)))
	(here (point))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and (interactive-p)
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))))
	(forward-sentence (- count))
      (while (> count 0)
	(vhdl-beginning-of-statement-1 lim)
	(setq count (1- count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (vhdl-keep-region-active))

(defconst vhdl-b-o-s-re
  (concat ";\\|\(\\|\)\\|\\bwhen\\b[^_]\\|"
	  vhdl-begin-bwd-re "\\|" vhdl-statement-bwd-re))

(defun vhdl-beginning-of-statement-1 (&optional lim)
  ;; move to the start of the current statement, or the previous
  ;; statement if already at the beginning of one.
  (let ((lim (or lim (point-min)))
	(here (point))
	(pos (point))
	donep)
    ;; go backwards one balanced expression, but be careful of
    ;; unbalanced paren being reached
    (if (not (vhdl-safe (progn (backward-sexp) t)))
	(progn
	  (backward-up-list 1)
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t)))
    (while (and (not donep)
		(not (bobp))
		;; look backwards for a statement boundary
		(re-search-backward vhdl-b-o-s-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; If we are looking at an open paren, then stop after it
	 ((eq (following-char) ?\()
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t))
	 ;; If we are looking at a close paren, then skip it
	 ((eq (following-char) ?\))
	  (forward-char)
	  (setq pos (point))
	  (backward-sexp)
	  (if (< (point) lim)
	      (progn (goto-char pos)
		     (vhdl-forward-syntactic-ws here)
		     (setq donep t))))
	 ;; If we are looking at a semicolon, then stop
	 ((eq (following-char) ?\;)
	  (progn
	    (forward-char)
	    (vhdl-forward-syntactic-ws here)
	    (setq donep t)))
	 ;; If we are looking at a "begin", then stop
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p nil))
	  ;; If it's a leader "begin", then find the
	  ;; right place
	  (if (looking-at vhdl-leader-re)
	      (save-excursion
		;; set a default stop point at the begin
		(setq pos (point))
		;; is the start point inside the leader area ?
		(goto-char (vhdl-end-of-leader))
		(vhdl-forward-syntactic-ws here)
		(if (< (point) here)
		    ;; start point was not inside leader area
		    ;; set stop point at word after leader
		    (setq pos (point))))
	    (forward-word 1)
	    (vhdl-forward-syntactic-ws here)
	    (setq pos (point)))
	  (goto-char pos)
	  (setq donep t))
	 ;; If we are looking at a "statement", then stop
	 ((and (looking-at vhdl-statement-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-statement-p nil))
	  (setq donep t))
	 ;; If we are looking at a case alternative key, then stop
	 ((looking-at vhdl-case-alternative-key)
	  (save-excursion
	    ;; set a default stop point at the when
	    (setq pos (point))
	    ;; is the start point inside the case alternative key ?
	    (goto-char (match-end 0))
	    (vhdl-forward-syntactic-ws here)
	    (if (< (point) here)
		;; start point was not inside the case alternative key
		;; set stop point at word after case alternative keyleader
		(setq pos (point))))
	  (goto-char pos)
	  (setq donep t))
	 ;; Bogus find, continue
	 (t
	  (backward-char)))))
    ))


;; Defuns for calculating the current syntactic state:

(defun vhdl-get-library-unit (bod placeholder)
  ;; If there is an enclosing library unit at bod, with it's \"begin\"
  ;; keyword at placeholder, then return the library unit type.
  (let ((here (vhdl-point 'bol)))
    (if (save-excursion
	  (goto-char placeholder)
	  (vhdl-safe (vhdl-forward-sexp 1 bod))
	  (<= here (point)))
	(save-excursion
	  (goto-char bod)
	  (cond
	   ((looking-at "e") 'entity)
	   ((looking-at "a") 'architecture)
	   ((looking-at "c") 'configuration)
	   ((looking-at "p")
	    (save-excursion
	      (goto-char bod)
	      (forward-sexp)
	      (vhdl-forward-syntactic-ws here)
	      (if (looking-at "body\\b[^_]")
		  'package-body 'package))))))
    ))

(defun vhdl-get-block-state (&optional lim)
  ;; Finds and records all the closest opens.
  ;; lim is the furthest back we need to search (it should be the
  ;; previous libunit keyword).
  (let ((here (point))
	(lim (or lim (point-min)))
	keyword sexp-start sexp-mid sexp-end
	preceding-sexp containing-sexp
	containing-begin containing-mid containing-paren)
    (save-excursion
      ;; Find the containing-paren, and use that as the limit
      (if (setq containing-paren
		(save-restriction
		  (narrow-to-region lim (point))
		  (vhdl-safe (scan-lists (point) -1 1))))
	  (setq lim containing-paren))
      ;; Look backwards for "begin" and "end" keywords.
      (while (and (> (point) lim)
		  (not containing-sexp))
	(setq keyword (vhdl-backward-to-block lim))
	(cond
	 ((eq keyword 'begin)
	  ;; Found a "begin" keyword
	  (setq sexp-start (point))
	  (setq sexp-mid (vhdl-corresponding-mid lim))
	  (setq sexp-end (vhdl-safe
			  (save-excursion
			    (vhdl-forward-sexp 1 lim) (point))))
	  (if (and sexp-end (<= sexp-end here))
	      ;; we want to record this sexp, but we only want to
	      ;; record the last-most of any of them before here
	      (or preceding-sexp
		  (setq preceding-sexp sexp-start))
	    ;; we're contained in this sexp so put sexp-start on
	    ;; front of list
	    (setq containing-sexp sexp-start)
	    (setq containing-mid sexp-mid)
	    (setq containing-begin t)))
	 ((eq keyword 'end)
	  ;; Found an "end" keyword
	  (forward-sexp)
	  (setq sexp-end (point))
	  (setq sexp-mid nil)
	  (setq sexp-start
		(or (vhdl-safe (vhdl-backward-sexp 1 lim) (point))
		    (progn (backward-sexp) (point))))
	  ;; we want to record this sexp, but we only want to
	  ;; record the last-most of any of them before here
	  (or preceding-sexp
	      (setq preceding-sexp sexp-start)))
	 )))
    ;; Check if the containing-paren should be the containing-sexp
    (if (and containing-paren
	     (or (null containing-sexp)
		 (< containing-sexp containing-paren)))
	(setq containing-sexp containing-paren
	      preceding-sexp nil
	      containing-begin nil
	      containing-mid nil))
    (vector containing-sexp preceding-sexp containing-begin containing-mid)
    ))
	      

(defconst vhdl-s-c-a-re
  (concat vhdl-case-alternative-key "\\|" vhdl-case-header-key))

(defun vhdl-skip-case-alternative (&optional lim)
  ;; skip forward over case/when bodies, with optional maximal
  ;; limit. if no next case alternative is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp)
    (while (and (< (point) lim)
		(not donep))
      (if (and (re-search-forward vhdl-s-c-a-re lim 'move)
	       (save-match-data
		 (not (vhdl-in-literal)))
	       (/= (match-beginning 0) here))
	  (progn
	    (goto-char (match-beginning 0))
	    (cond
	     ((and (looking-at "case")
		   (re-search-forward "\\bis[^_]" lim t))
	      (backward-sexp)
	      (vhdl-forward-sexp))
	     (t
	      (setq donep t
		    foundp t))))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun vhdl-backward-skip-label (&optional lim)
  ;; skip backward over a label, with optional maximal
  ;; limit. if label is found, nil is returned and point
  ;; is not moved
  (let ((lim (or lim (point-min)))
	placeholder)
    (if (save-excursion
	  (vhdl-backward-syntactic-ws lim)
	  (and (eq (preceding-char) ?:)
	       (progn
		 (backward-sexp)
		 (setq placeholder (point))
		 (looking-at vhdl-label-key))))
	(goto-char placeholder))
    ))

(defun vhdl-get-syntactic-context ()
  ;; guess the syntactic description of the current line of VHDL code.
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search t)
	     vec literal containing-sexp preceding-sexp
	     containing-begin containing-mid containing-leader
	     char-before-ip char-after-ip begin-after-ip end-after-ip
	     placeholder lim library-unit
	    )

	;; Reset the syntactic context
	(setq vhdl-syntactic-context nil)

	(save-excursion
	  ;; Move to the start of the previous library unit, and
	  ;; record the position of the "begin" keyword.
	  (setq placeholder (vhdl-beginning-of-libunit))
	  ;; The position of the "libunit" keyword gives us a gross
	  ;; limit point.
	  (setq lim (point))
	  )

	;; If there is a previous library unit, and we are enclosed by
	;; it, then set the syntax accordingly.
	(and placeholder
	     (setq library-unit (vhdl-get-library-unit lim placeholder))
	     (vhdl-add-syntax library-unit lim))
	    
	;; Find the surrounding state.
	(if (setq vec (vhdl-get-block-state lim))
	    (progn
	      (setq containing-sexp (aref vec 0))
	      (setq preceding-sexp (aref vec 1))
	      (setq containing-begin (aref vec 2))
	      (setq containing-mid (aref vec 3))
	      ))

	;; set the limit on the farthest back we need to search
	(setq lim (if containing-sexp
		      (save-excursion
			(goto-char containing-sexp)
			;; set containing-leader if required
			(if (looking-at vhdl-leader-re)
			    (setq containing-leader (vhdl-end-of-leader)))
			(vhdl-point 'bol))
		    (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq literal (vhdl-in-literal lim))
	(setq char-after-ip (following-char))
	(setq begin-after-ip (and
			      (not literal)
			      (looking-at vhdl-begin-fwd-re)
			      (vhdl-begin-p)))
	(setq end-after-ip (and
			    (not literal)
			    (looking-at vhdl-end-fwd-re)
			    (vhdl-end-p)))
	(vhdl-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string or comment.
	 ((memq literal '(string comment))
	  (vhdl-add-syntax literal (vhdl-point 'bopl)))
	 ;; CASE 2: Line is at top level.
	 ((null containing-sexp)
	  ;; Find the point to which indentation will be relative
	  (save-excursion
	    (if (null preceding-sexp)
		;; CASE 2X.1
		;; no preceding-sexp -> use the preceding statement
		(vhdl-beginning-of-statement-1 lim)
	      ;; CASE 2X.2
	      ;; if there is a preceding-sexp then indent relative to it
	      (goto-char preceding-sexp)
	      ;; if not at boi, then the block-opening keyword is
	      ;; probably following a label, so we need a different
	      ;; relpos
	      (if (/= (point) (vhdl-point 'boi))
		  ;; CASE 2X.3
		  (vhdl-beginning-of-statement-1 lim)))
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2X.4
		 (vhdl-forward-syntactic-ws indent-point))
	    (setq placeholder (point)))
	  (cond
	   ;; CASE 2A : we are looking at a block-open
	   (begin-after-ip
	    (vhdl-add-syntax 'block-open placeholder))
	   ;; CASE 2B: we are looking at a block-close
	   (end-after-ip
	    (vhdl-add-syntax 'block-close placeholder))
	   ;; CASE 2C: we are looking at a top-level statement
	   ((progn
	      (vhdl-backward-syntactic-ws lim)
	      (or (bobp)
		  (= (preceding-char) ?\;)))
	    (vhdl-add-syntax 'statement placeholder))
	   ;; CASE 2D: we are looking at a top-level statement-cont
	   (t
	    (vhdl-beginning-of-statement-1 lim)
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2D.1
		 (vhdl-forward-syntactic-ws indent-point))
	    (vhdl-add-syntax 'statement-cont (point)))
	   )) ; end CASE 2
	 ;; CASE 3: line is inside parentheses.  Most likely we are
	 ;; either in a subprogram argument (interface) list, or a
	 ;; continued expression containing parentheses.
	 ((null containing-begin)
	  (vhdl-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 3A: we are looking at the arglist closing paren
	   ((eq char-after-ip ?\))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-close (vhdl-point 'boi)))
	   ;; CASE 3B: we are looking at the first argument in an empty
	   ;; argument list.
	   ((eq char-before-ip ?\()
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-intro (vhdl-point 'boi)))
	   ;; CASE 3C: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; expression paren groupings.
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp))
		   (not (looking-at "--")))
		 (save-excursion
		   (vhdl-beginning-of-statement-1 containing-sexp)
		   (skip-chars-backward " \t(")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-cont-nonempty (vhdl-point 'boi)))
	   ;; CASE 3D: we are looking at just a normal arglist
	   ;; continuation line
	   (t (vhdl-beginning-of-statement-1 containing-sexp)
	      (vhdl-forward-syntactic-ws indent-point)
	      (vhdl-add-syntax 'arglist-cont (vhdl-point 'boi)))
	   ))
	 ;; CASE 4: A block mid open
	 ((and begin-after-ip
	       (looking-at containing-mid))
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 4.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-open (point)))
	 ;; CASE 5: block close brace
	 (end-after-ip
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 5.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-close (point)))
	 ;; CASE 6: A continued statement
	 ((and (/= char-before-ip ?\;)
	       ;; check it's not a trailer begin keyword, or a begin
	       ;; keyword immediately following a label.
	       (not (and begin-after-ip
			 (or (looking-at vhdl-trailer-re)
			     (save-excursion
			       (vhdl-backward-skip-label containing-sexp)))))
	       ;; check it's not a statement keyword
	       (not (and (looking-at vhdl-statement-fwd-re)
			 (vhdl-statement-p)))
	       ;; see if the b-o-s is before the indent point
	       (> indent-point
		  (save-excursion
		    (vhdl-beginning-of-statement-1 containing-sexp)
		    ;; If we ended up after a leader, then this will
		    ;; move us forward to the start of the first
		    ;; statement.  Note that a containing sexp here is
		    ;; always a keyword, not a paren, so this will
		    ;; have no effect if we hit the containing-sexp.
		    (vhdl-forward-syntactic-ws indent-point)
		    (setq placeholder (point))))
	       ;; check it's not a block-intro
	       (/= placeholder containing-sexp)
	       ;; check it's not a case block-intro
	       (save-excursion
		 (goto-char placeholder)
		 (or (not (looking-at vhdl-case-alternative-key))
		     (> (match-end 0) indent-point))))
	  (vhdl-add-syntax 'statement-cont placeholder)
	  (if begin-after-ip
	      (vhdl-add-syntax 'block-open)))
	 ;; Statement. But what kind?
	 ;; CASE 7: A case alternative key
	 ((looking-at vhdl-case-alternative-key)
	  ;; for a case alternative key, we set relpos to the first
	  ;; non-whitespace char on the line containing the "case"
	  ;; keyword.
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-add-syntax 'case-alternative (vhdl-point 'boi)))
	 ;; CASE 8: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (if containing-leader
	      (goto-char containing-leader)
	    (goto-char containing-sexp)
	    ;; Note that a containing sexp here is always a keyword,
	    ;; not a paren, so skip over the keyword.
	    (forward-sexp))
	  ;; move to the start of the first statement
	  (vhdl-forward-syntactic-ws indent-point)
	  (setq placeholder (point))
	  ;; we want to ignore case alternatives keys when skipping forward
	  (let (incase-p)
	    (while (looking-at vhdl-case-alternative-key)
	      (setq incase-p (point))
	      ;; we also want to skip over the body of the
	      ;; case/when statement if that doesn't put us at
	      ;; after the indent-point
	      (while (vhdl-skip-case-alternative indent-point))
	      ;; set up the match end
	      (looking-at vhdl-case-alternative-key)
	      (goto-char (match-end 0))
	      ;; move to the start of the first case alternative statement
	      (vhdl-forward-syntactic-ws indent-point)
	      (setq placeholder (point)))
	    (cond
	     ;; CASE 8A: we saw a case/when statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case alternative key
	     ((and incase-p
		   (= (point) indent-point))
	      ;; relpos is the "when" keyword
	      (vhdl-add-syntax 'statement-case-intro incase-p))
	     ;; CASE 8B: any old statement
	     ((< (point) indent-point)
	      ;; relpos is the first statement of the block
	      (vhdl-add-syntax 'statement placeholder)
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     ;; CASE 8C: first statement in a block
	     (t
	      (goto-char containing-sexp)
	      ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	      (if (looking-at vhdl-trailer-re)
		  (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	      (vhdl-backward-skip-label (vhdl-point 'boi))
	      (vhdl-add-syntax 'statement-block-intro (point))
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     )))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(if (looking-at "--")
	    (vhdl-add-syntax 'comment))
	;; return the syntax
	vhdl-syntactic-context))))


;; Standard indentation line-ups:

(defun vhdl-lineup-arglist (langelem)
  ;; lineup the current arglist line with the arglist appearing just
  ;; after the containing paren which starts the arglist.
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is vhdl-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (vhdl-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (cs-curcol (save-excursion
			(goto-char (cdr langelem))
			(current-column))))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (backward-sexp)
		 (forward-char)
		 (vhdl-forward-syntactic-ws)
		 (- (current-column) cs-curcol))
	(goto-char containing-sexp)
	(or (eolp)
	    (let ((eol (vhdl-point 'eol))
		  (here (progn
			  (forward-char)
			  (skip-chars-forward " \t")
			  (point))))
	      (vhdl-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) cs-curcol)
	))))

(defun vhdl-lineup-arglist-intro (langelem)
  ;; lineup an arglist-intro line to just after the open paren
  (save-excursion
    (let ((cs-curcol (save-excursion
		       (goto-char (cdr langelem))
		       (current-column)))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (vhdl-point 'eol))
		       (current-column))))
      (- ce-curcol cs-curcol -1))))

(defun vhdl-lineup-comment (langelem)
  ;; support old behavior for comment indentation. we look at
  ;; vhdl-comment-only-line-offset to decide how to indent comment
  ;; only-lines
  (save-excursion
    (back-to-indentation)
    ;; at or to the right of comment-column
    (if (>= (current-column) comment-column)
	(vhdl-comment-indent)
      ;; otherwise, indent as specified by vhdl-comment-only-line-offset
      (if (not (bolp))
	  (or (car-safe vhdl-comment-only-line-offset)
	      vhdl-comment-only-line-offset)
	(or (cdr-safe vhdl-comment-only-line-offset)
	    (car-safe vhdl-comment-only-line-offset)
	    -1000			;jam it against the left side
	    )))))

(defun vhdl-lineup-statement-cont (langelem)
  ;; line up statement-cont after the assignment operator
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (assignp (save-excursion
		     (goto-char (vhdl-point 'boi))
		     (and (re-search-forward "\\(<\\|:\\)="
					     (vhdl-point 'eol) t)
			  (- (point) (vhdl-point 'boi)))))
	   (curcol (progn
		     (goto-char relpos)
		     (current-column)))
	   foundp)
      (while (and (not foundp)
		  (< (point) (vhdl-point 'eol)))
	(re-search-forward "\\(<\\|:\\)=\\|(" (vhdl-point 'eol) 'move)
	(if (vhdl-in-literal (cdr langelem))
	    (forward-char)
	  (if (= (preceding-char) ?\()
	      ;; skip over any parenthesized expressions
	      (goto-char (min (vhdl-point 'eol)
			      (scan-lists (point) 1 1)))
	    ;; found an assignment operator (not at eol)
	    (setq foundp (not (looking-at "\\s-*$"))))))
      (if (not foundp)
	  ;; there's no assignment operator on the line
	  vhdl-basic-offset
	;; calculate indentation column after assign and ws, unless
	;; our line contains an assignment operator
	(if (not assignp)
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq assignp 0)))
	(- (current-column) assignp curcol))
      )))


;; Indentation commands:

;; This is used by indent-for-comment to decide how much to indent a
;; comment in VHDL code based on its context.
(defun vhdl-comment-indent ()
  (if (looking-at (concat "^--"))
      0				;Existing comment at bol stays there.
    (let ((opoint (point))
	  placeholder)
      (save-excursion
	(beginning-of-line)
	(cond
	 ;; CASE 1: use comment-column if previous line is a
	 ;; comment-only line indented to the left of comment-column
	 ((save-excursion
	    (beginning-of-line)
	    (and (not (bobp))
		 (forward-line -1))
	    (skip-chars-forward " \t")
	    (prog1
		(looking-at "--")
	      (setq placeholder (point))))
	  (goto-char placeholder)
	  (if (< (current-column) comment-column)
	      comment-column
	    (current-column)))
	 ;; CASE 2: If comment-column is 0, and nothing but space
	 ;; before the comment, align it at 0 rather than 1.
	 ((progn
	    (goto-char opoint)
	    (skip-chars-backward " \t")
	    (and (= comment-column 0) (bolp)))
	  0)
	 ;; CASE 3: indent at comment column except leave at least one
	 ;; space.
	 (t (max (1+ (current-column))
		 comment-column))
	 )))))

(defun vhdl-indent-line ()
  ;; indent the current line as VHDL code. Returns the amount of
  ;; indentation change
  (let* ((syntax (vhdl-get-syntactic-context))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'vhdl-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (and vhdl-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" syntax indent))
    (if (zerop shift-amt)
	nil
      (delete-region (vhdl-point 'bol) (vhdl-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (vhdl-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )
    (run-hooks 'vhdl-special-indent-hook)
    shift-amt))

(defun vhdl-indent-command (&optional whole-exp)
  "Indent current line as VHDL code, or in some cases insert a tab character.

If `vhdl-tab-always-indent' is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.  If other than nil
or t, then tab is inserted only within literals (comments and strings)
and inside preprocessor directives, but line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as VHDL
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (vhdl-indent-line))
	    beg end)
	(save-excursion
	  (if (eq vhdl-tab-always-indent t)
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end (- shift-amt))))
    ;; No arg supplied, use vhdl-tab-always-indent to determine
    ;; behavior
    (cond
     ;; CASE 1: indent when at column zero or in lines indentation,
     ;; otherwise insert a tab
     ((not vhdl-tab-always-indent)
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	  (insert-tab)
	(vhdl-indent-line)))
     ;; CASE 2: just indent the line
     ((eq vhdl-tab-always-indent t)
      (vhdl-indent-line))
     ;; CASE 3: if in a literal, insert a tab, but always indent the
     ;; line
     (t
      (if (vhdl-in-literal (vhdl-point 'bod))
	  (insert-tab))
      (vhdl-indent-line)
      ))))

(defun vhdl-indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered.  (interactive)"
  (interactive)
  (save-excursion
    (let ((beg (point))
	  (end (progn
		 (vhdl-forward-sexp nil endpos)
		 (point))))
      (indent-region beg end nil))))

(defun vhdl-show-syntactic-information ()
  "Show syntactic information for current line."
  (interactive)
  (message "syntactic analysis: %s" (vhdl-get-syntactic-context))
  (vhdl-keep-region-active))


;; Verification and regression functions:

(defun vhdl-regress-line (&optional arg)
  "Check syntactic information for current line."
  (interactive "P")
  (let ((expected (save-excursion
		    (end-of-line)
		    (if (search-backward " -- ((" (vhdl-point 'bol) t)
			(progn
			  (forward-char 4)
			  (read (current-buffer))))))
	(actual (vhdl-get-syntactic-context))
	(expurgated))
    ;; remove the library unit symbols
    (mapcar
     (function
      (lambda (elt)
	(if (memq (car elt) '(entity configuration package
				     package-body architecture))
	    nil
	  (setq expurgated (append expurgated (list elt))))))
     actual)
    (if (and (not arg) expected (listp expected))
	(if (not (equal expected expurgated))
	    (error "Should be: %s, is: %s" expected expurgated))
      (save-excursion
	(beginning-of-line)
	(if (not (looking-at "^\\s-*\\(--.*\\)?$"))
	    (progn
	      (end-of-line)
	      (if (search-backward " -- ((" (vhdl-point 'bol) t)
		  (kill-line))
	      (insert " -- ")
	      (insert (format "%s" expurgated)))))))
  (vhdl-keep-region-active))

(defun test-vhdl-get-block-state ()
  (interactive)
  (let ((case-fold-search t)
	here vec (delay 0.5))
    (setq here (point))
    (message "%s" (prin1-to-string (setq vec (vhdl-get-block-state))))
    (and (aref vec 0)
	 (goto-char (aref vec 0))
	 (sit-for delay))
    (and (aref vec 1)
	 (goto-char (aref vec 1))
	 (sit-for delay))
    (goto-char here)
    ))

;; Support for Barry Warsaw's elp (emacs lisp profiler) package:

(eval-when-compile
  (require 'elp))

(setq elp-all-instrumented-list nil)
(setq elp-function-list
      '(
	vhdl-indent-command
	vhdl-indent-line
	vhdl-comment-indent
	vhdl-lineup-statement-cont
	vhdl-lineup-comment
	vhdl-lineup-arglist-intro
	vhdl-lineup-arglist
	vhdl-get-syntactic-context
	vhdl-skip-case-alternative
	vhdl-get-block-state
	vhdl-get-library-unit
	vhdl-beginning-of-statement
	vhdl-beginning-of-statement-1
	vhdl-beginning-of-defun
	vhdl-beginning-of-libunit
	vhdl-backward-sexp
	vhdl-forward-sexp
	vhdl-backward-to-block
	vhdl-statement-p
	vhdl-end-of-leader
	vhdl-corresponding-begin
	vhdl-end-p
	vhdl-corresponding-end
	vhdl-corresponding-mid
	vhdl-begin-p
	vhdl-corresponding-defun
	vhdl-defun-p
	vhdl-libunit-p
	vhdl-last-word
	vhdl-first-word
	vhdl-backward-syntactic-ws
	vhdl-forward-syntactic-ws
	vhdl-in-literal
	vhdl-keep-region-active
	))

;; (elp-instrument-list elp-function-list)

(defun vhdl-trace-all-functions ()
  (interactive)
  (let ((list elp-function-list))
    (while list
      (trace-function-background (car list))
      (setq list (cdr list)))))


;; Defuns for submitting bug reports:

(defconst vhdl-version "$Revision: 1.7 $"
  "vhdl-mode version number.")
(defconst vhdl-mode-help-address "rwhitby@geocities.com"
  "Address accepting submission of bug reports.")

(defun vhdl-version ()
  "Echo the current version of vhdl-mode in the minibuffer."
  (interactive)
  (message "Using vhdl-mode %s" vhdl-version)
  (vhdl-keep-region-active))

;; get reporter-submit-bug-report when byte-compiling
(and (fboundp 'eval-when-compile)
     (eval-when-compile
      (require 'reporter)))

(defun vhdl-submit-bug-report ()
  "Submit via mail a bug report on vhdl-mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on vhdl-mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    vhdl-mode-help-address
    (concat "vhdl-mode " vhdl-version)
    (list
     ;; report only the vars that affect indentation
     'vhdl-basic-offset
     'vhdl-offsets-alist
     'vhdl-comment-only-line-offset
     'vhdl-tab-always-indent
     'tab-width
     )
    (function
     (lambda ()
       (insert
	(if vhdl-special-indent-hook
	    (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		    "vhdl-special-indent-hook is set to '"
		    (format "%s" vhdl-special-indent-hook)
		    ".\nPerhaps this is your problem?\n"
		    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	  "\n")
	(format "vhdl-emacs-features: %s\n" vhdl-emacs-features)
	)))
    nil
    "Dear Rod,"
    )))

(provide 'vhdl-mode)
;;; vhdl-mode.el ends here
