;;; code-init.el --- Handle coding system default values

;; Copyright (C) 2001, 2002, 2003 Ben Wing.

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

;;; Commentary:

;; Placed in a separate file so it can be loaded after the various
;; coding systems have been created, because we'll be using them at
;; load time.

;;; Code:

(defcustom eol-detection-enabled-p (or (featurep 'mule)
				       (memq system-type '(windows-nt
							   cygwin32))
				       (featurep 'unix-default-eol-detection))
"True if XEmacs automatically detects the EOL type when reading files.
Normally, this is always the case on Windows or when international (Mule)
support is compiled into this XEmacs.  Otherwise, it is currently off by
default, but this may change.  Don't set this; nothing will happen.  Instead,
use the Options menu or `set-eol-detection'."
  :group 'encoding
  :type 'boolean
  ;; upon initialization, we don't want the whole business of
  ;; set-eol-detection to be called.  We will init everything appropriately
  ;; later in the same file, when reset-language-environment is called.
  :initialize #'(lambda (var val)
		  (setq eol-detection-enabled-p val))
  :set #'(lambda (var val)
	   (set-eol-detection val)
	   (setq eol-detection-enabled-p val)))

(defun set-eol-detection (flag)
  "Enable (if FLAG is non-nil) or disable automatic EOL detection of files.
EOL detection is enabled by default on Windows or when international (Mule)
support is compiled into this XEmacs.  Otherwise, it is currently off by
default, but this may change.  NOTE: You *REALLY* should not turn off EOL
detection on Windows!  Your files will have lots of annoying ^M's in them
if you do this."
  (dolist (x '(buffer-file-coding-system-for-read
	       keyboard
	       default-process-coding-system-read
	       no-conversion-coding-system-mapping))
    (set-coding-system-variable
     x (coding-system-change-eol-conversion (get-coding-system-variable x)
					    (if flag nil 'lf)))))

(defun coding-system-current-system-configuration ()
  (cond ((memq system-type '(windows-nt cygwin32))
	 (if (featurep 'mule) 'windows-mule 'windows-no-mule))
	((featurep 'mule) 'unix-mule)
	(eol-detection-enabled-p 'unix-no-mule-eol-detection)
	(t 'unix-no-mule-no-eol-detection)))

;; NOTE NOTE NOTE: These values may get overridden when the language
;; environment is initialized (set-language-environment-coding-systems).
(defvar coding-system-variable-default-value-table
  '((buffer-file-coding-system-for-read
     binary raw-text undecided raw-text undecided)
    (default-buffer-file-coding-system
      binary binary iso-2022-8 raw-text-dos mswindows-multibyte-dos)
    (native
     binary binary binary raw-text-dos mswindows-multibyte-system-default-dos)
    (keyboard
     binary raw-text undecided-unix raw-text undecided-unix)
    ;; the `terminal' coding system is used for output to stderr.  such
    ;; streams do automatic lf->crlf encoding in the C library, so we need
    ;; to not do the same translations ourselves.
    (terminal
     binary binary binary binary mswindows-multibyte-unix)
    (default-process-coding-system-read
      binary raw-text undecided raw-text undecided)
    (default-process-coding-system-write
      binary binary binary raw-text mswindows-multibyte-system-default)
    (no-conversion-coding-system-mapping
     binary raw-text raw-text raw-text mswindows-multibyte)
    ))

(defvar coding-system-default-configuration-list
  '(unix-no-mule-no-eol-detection
    unix-no-mule-eol-detection
    unix-mule
    windows-no-mule
    windows-mule))

(defvar coding-system-default-variable-list
  '(buffer-file-coding-system-for-read
    default-buffer-file-coding-system
    native
    keyboard
    terminal
    default-process-coding-system-read
    default-process-coding-system-write))

(defun get-coding-system-variable (var)
  "Return the value of a basic coding system variable.
This is intended as a uniform interface onto the coding system settings that
control how encoding detection and conversion works.  See
`coding-system-variable-default-value' for a list of the possible values of
VAR."
  (case var
    (buffer-file-coding-system-for-read buffer-file-coding-system-for-read)
    (default-buffer-file-coding-system
      (default-value 'buffer-file-coding-system))
    (native (coding-system-aliasee 'native))
    (keyboard (coding-system-aliasee 'keyboard))
    (terminal (coding-system-aliasee 'terminal))
    (default-process-coding-system-read (car default-process-coding-system))
    (default-process-coding-system-write (cdr default-process-coding-system))
    (t (error 'invalid-constant "Invalid coding system variable" var))))

(defun set-coding-system-variable (var value)
  "Set a basic coding system variable to VALUE.
This is intended as a uniform interface onto the coding system settings that
control how encoding detection and conversion works.  See
`coding-system-variable-default-value' for a list of the possible values of
VAR."
  (case var
    (buffer-file-coding-system-for-read
     (set-buffer-file-coding-system-for-read value))
    (default-buffer-file-coding-system
      (set-default-buffer-file-coding-system value))
    (native (define-coding-system-alias 'native value))
    (keyboard (set-keyboard-coding-system value))
    (terminal (set-terminal-coding-system value))
    (default-process-coding-system-read
      (setq default-process-coding-system
	    (cons value (cdr default-process-coding-system))))
    (default-process-coding-system-write
      (setq default-process-coding-system
	    (cons (car default-process-coding-system) value)))
    (t (error 'invalid-constant "Invalid coding system variable" var))))

(defun coding-system-variable-default-value (var &optional config)
  "Return the appropriate default value for a coding system variable.

VAR specifies the variable, and CONFIG the configuration, defaulting
to the current system configuration (as returned by
`coding-system-current-system-configuration').

The table of default values looks like this: (see below for abbreviations)


               Unix    Unix+EOL  Unix+Mule       MSW           MSW+Mule
-----------------------------------------------------------------------------
bfcs-for-read  binary  raw-text  undecided       raw-text      undecided
default bfcs   binary  binary    iso-2022-8      raw-text-dos  MSW-MB-dos
native         binary  binary    binary          raw-text-dos  MSW-MB-SD-dos
keyboard       binary  raw-text  undecided-unix  raw-text      undecided-unix
terminal       binary  binary    binary          binary        MSW-MB-unix
process-read   binary  raw-text  undecided       raw-text      undecided
process-write  binary  binary    binary          raw-text      MSW-MB-SD
no-conv-cs     binary  raw-text  raw-text        raw-text      MSW-MB


VAR can be one of: (abbreviations in parens)

`buffer-file-coding-system-for-read' (bfcs-for-read)

  Lisp variable of the same name; the default coding system used when
  reading in a file, in the absence of more specific settings. (See
  `insert-file-contents' for a description of exactly how a file's
  coding system is determined when it's read in.)

`default-buffer-file-coding-system' (default bfcs)

  Default value of `buffer-file-coding-system', the buffer-local
  variable specifying a file's coding system to be used when it is
  written out.  Set using `set-default-buffer-file-coding-system' (or
  the primitive `setq-default').  When a file is read in,
  `buffer-file-coding-system' for that file is set from the coding
  system used to read the file in; the default value applies to newly
  created files.

`native' (native)

  The coding system named `native'.  Changed using
  `define-coding-system-alias'.  Used internally when passing
  text to or from system API's, unless the particular
  API specifies another coding system.

`keyboard' (keyboard)

 #### fill in

`terminal' (terminal)

 #### fill in

`default-process-coding-system-read' (process-read)

 #### fill in

`default-process-coding-system-write' (process-write)

 #### fill in

`no-conversion-coding-system-mapping' (no-conv-cs)

  Coding system used when category `no-conversion' is detected.


CONFIG is one of: (abbreviations in parens)

`unix-no-mule-no-eol-detection' (Unix)

Unix, no Mule support, no automatic EOL detection. (Controlled by
`eol-detection-enabled-p', which is set by the command-line flag
-enable-eol-detection or the configure flag --with-default-eol-detection.)

`unix-no-mule-eol-detection' (Unix+EOL)

Unix, no Mule support, automatic EOL detection.

`unix-mule' (Unix+Mule)

Unix, Mule support.

`windows-no-mule' (MSW)

MS Windows or Cygwin, no Mule support.

`windows-mule'. (MSW+Mule)

MS Windows or Cygwin, Mule support.


The following coding system abbreviations are also used in the table:

MSW-MB = mswindows-multibyte
MSW-MB = mswindows-multibyte-system-default
"
  (setq config (or config (coding-system-current-system-configuration)))
  (let ((defs (cdr (assq var coding-system-variable-default-value-table))))
    (or defs (error 'invalid-constant "Invalid coding system variable" var))
    (let ((pos (position config coding-system-default-configuration-list)))
      (or pos (error 'invalid-constant "Invalid coding system configuration"
		     config))
      (nth pos defs))))

(defun reset-coding-system-defaults (&optional config)
  "Reset all basic coding system variables are set to their default values.
See `coding-system-variable-default-value'."
  (setq config (or config (coding-system-current-system-configuration)))
  (mapcar #'(lambda (var)
	      (set-coding-system-variable
	       var (coding-system-variable-default-value var config)))
	  coding-system-default-variable-list))

(defun reset-coding-categories-to-default ()
"Reset all coding categories (used for automatic detection) to their defaults.

The order of priorities of coding categories and the coding system
bound to each category are as follows:

	coding category		  coding system
	--------------------------------------------------
        utf-16-little-endian-bom  utf-16-little-endian
	utf-16-bom		  utf-16-bom
	utf-8-bom		  utf-8-bom
	iso-7			  iso-2022-7bit
	no-conversion		  raw-text
	utf-8			  utf-8
	iso-8-1			  iso-8859-1
	iso-8-2			  ctext (iso-8859-1 alias)
	iso-8-designate		  ctext (iso-8859-1 alias)
	iso-lock-shift		  iso-2022-lock
	shift-jis		  shift-jis
	big5			  big5
        utf-16-little-endian      utf-16-little-endian
	utf-16			  utf-16
        ucs-4			  ucs-4
"
  ;; #### What a mess!  This needs to be overhauled.

  ;; The old table (from FSF synch?) was not what we use (cf mule-coding.el),
  ;; and as documented iso-8-designate is inconsistent with iso-2022-8bit-ss2.
  ;; The order of priorities of coding categories and the coding system
  ;; bound to each category are as follows:
  ;;
  ;;	coding category		coding system
  ;;	--------------------------------------------------
  ;;	iso-8-2			iso-8859-1
  ;;	iso-8-1			iso-8859-1
  ;;	iso-7			iso-2022-7bit
  ;;	iso-lock-shift		iso-2022-lock
  ;;	iso-8-designate		iso-2022-8bit-ss2
  ;;	no-conversion		raw-text
  ;;	shift-jis		shift_jis
  ;;	big5			big5
  ;;	ucs-4			----
  ;;	utf-8			----
  (when (featurep 'mule)
    (set-coding-category-system 'iso-7	'iso-2022-7)
    (set-coding-category-system 'iso-8-1 'iso-8859-1)
    (set-coding-category-system 'iso-8-2 'ctext)
    (set-coding-category-system 'iso-lock-shift	'iso-2022-lock)
    (set-coding-category-system 'iso-8-designate 'ctext)
    (if (find-coding-system 'shift-jis)
	(set-coding-category-system 'shift-jis 'shift-jis))
    (if (find-coding-system 'big5)
	(set-coding-category-system 'big5 'big5))
    )
  (set-coding-category-system
   'no-conversion
   (coding-system-variable-default-value 'no-conversion-coding-system-mapping))
  (set-coding-category-system 'ucs-4 'ucs-4)
  (set-coding-category-system 'utf-8 'utf-8)
  (set-coding-category-system 'utf-8-bom 'utf-8-bom)
  (set-coding-category-system 'utf-16-little-endian 'utf-16-little-endian)
  (set-coding-category-system 'utf-16 'utf-16)
  (set-coding-category-system 'utf-16-little-endian-bom
			      'utf-16-little-endian-bom)
  (set-coding-category-system 'utf-16-bom 'utf-16-bom)
  (set-coding-priority-list
   (if (featurep 'mule)
       '(utf-16-little-endian-bom
	 utf-16-bom
	 utf-8-bom
	 iso-7
	 no-conversion
	 utf-8
	 iso-8-1
	 iso-8-2
	 iso-8-designate
	 iso-lock-shift
	 shift-jis
	 big5
	 utf-16-little-endian
	 utf-16
	 ucs-4)
     '(utf-16-little-endian-bom
       utf-16-bom
       utf-8-bom
       no-conversion
       utf-8
       utf-16-little-endian
       utf-16
       ucs-4))))

(defun reset-language-environment ()
  "Reset coding system environment of XEmacs to the default status.
All basic coding system variables are set to their default values, as
are the coding categories used for automatic detection and their
priority.

BE VERY CERTAIN YOU WANT TO DO THIS BEFORE DOING IT!

For more information, see `reset-coding-system-defaults' and
`reset-coding-categories-to-default'."
  (reset-coding-system-defaults)
  (reset-coding-categories-to-default))

;; Initialize everything so that the remaining Lisp files can contain
;; extended characters.  (They will be in ISO-7 format)

;; !!####!! The Lisp files should all be in UTF-8!!!  That way, all
;; special characters appear as high bits and there's no problem with
;; the Lisp parser trying to read a Mule file and getting all screwed
;; up.  The only other thing then would be characters; we just need to
;; modify the Lisp parser to read the stuff directly after a ? as
;; UTF-8 and return a 30-bit value directly, and modify the character
;; routines a bit to allow such a beast to exist.  MAKE IT A POINT TO
;; IMPLEMENT THIS AS ONE OF MY FUTURE PROJECTS. --ben

(reset-language-environment)

;;; code-init.el ends here
