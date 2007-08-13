;;; w3-vars.el,v --- All variable definitions for emacs-w3
;; Author: wmperry
;; Created: 1996/08/29 04:09:40
;; Version: 1.18
;; Keywords: comm, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable definitions for w3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst w3-version-number
  (let ((x "p3.0.12"))
    (if (string-match "State:[ \t\n]+.\\([^ \t\n]+\\)" x)
	(setq x (substring x (match-beginning 1) (match-end 1)))
      (setq x (substring x 1)))
    (mapconcat
     (function (lambda (x) (if (= x ?-) "." (char-to-string x)))) x ""))
  "Version # of w3-mode.")

(defconst w3-version-date (let ((x "1996/08/29 04:09:40"))
			    (if (string-match "Date: \\([^ \t\n]+\\)" x)
				(substring x (match-beginning 1) (match-end 1))
			      x))
  "Date this version of w3-mode was released.")

(defconst w3-version
  (format "WWW %s %s" w3-version-number w3-version-date)
  "More descriptive version of w3-version-number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-annotation-mode 'html-mode
  "*A symbol specifying the major mode to enter when doing annotations.")

(defvar w3-annotation-position 'bottom
  "*A symbol specifying where personal annotations should appear in a buffer.
Can be one of the symbols 'top or 'bottom.  If the symbol is eq to 'top, then
the annotations will appear at the top of the buffer.  If 'bottom, will appear
at the end of the buffer.")

(defvar w3-auto-image-alt t
  "*Whether emacs-w3 should create an alt attribute for an image that
is missing it.
If nil, emacs-w3 will not automatically create an ALT attribute.
If t, the alt attribute will be [IMAGE(nameofimage)]
If a string, it should be a string suitable for running through format,
   with only one %s, which will be replaced with just the filename of the
   graphic that is not loaded.")

(defvar w3-configuration-directory "~/.w3/"
  "*Where emacs-w3 can find its configuration files")

(defvar w3-debug-html nil "*Whether to gripe about bad HTML or not.")

(defvar w3-debug-buffer "*HTML Debug*"
  "*Name of buffer to store debugging information in.")

(defvar w3-default-configuration-file nil
  "*Where per-user customizations of w3 are kept.")

(defvar w3-default-action 'w3-prepare-buffer
  "*A lisp symbol specifying what action to take for files with
extensions that are not mapped to a MIME type in `mm-mime-extensions'.
This is useful in case you ever run across files with weird extensions
\(.foo, .README, .READMEFIRST, etc).  This should not be required
anymore.

Possible values: any lisp symbol.  Should be a function that takes no
arguments.  The return value does not matter, it is ignored.  Some examples
are:

Action			Value
----------------------------------------------
Parse as HTML		'w3-prepare-buffer
View as text		'indented-text-mode")

(defvar w3-default-homepage nil
  "*The url to open at startup.  It can be any valid URL.  This will
default to the environment variable WWW_HOME if you do not set it in
your .emacs file. If WWW_HOME is undefined, then it will default to
the hypertext documentation for W3 at Indiana University.")

(defvar w3-default-stylesheet nil
  "*The filename of the users default stylesheet.")

(defvar w3-do-blinking nil
  "*Whether emacs-w3 should display blinking text.")

(defvar w3-do-incremental-display nil
  "*Whether to do incremental display of pages or not.")

(defvar w3-documents-menu-file nil
  "*Where the Mosaic documents-menu file is located.  This is a file
that has extra menus for the 'Navigate' menu.  This should be in the same
format as the Mosaic extra documents.menu.")

(defvar w3-dump-to-disk nil
  "*If non-nil, all W3 pages loaded will be dumped to disk.")

(defvar w3-echo-link 'url
  "Whether to display the URL of a link when tabbing through links.
Possible values are:

  url    == show the url of the target in the minibuffer
  text   == show the text of the link in the minibuffer
  nil    == show nothing")
  
(defvar w3-horizontal-rule-char ?-
  "*The character to use to create a horizontal rule.
Must be the character's code, not a string.  This character is
replicated across the screen to create a division.")

(defvar w3-hotlist-file nil
  "*Hotlist filename.
This should be the name of a file that is stored in either
NCSA's Mosaic/X or Netscape/X format.  It is used to keep a listing
of commonly accessed URL's without having to go through 20 levels of
menus to get to them.")

(defvar w3-html2latex-args "-s -"
  "*Args to pass `w3-html2latex-prog'.  This should send the LaTeX source
to standard output.")

(defvar w3-html2latex-prog "html2latex"
  "*Program to convert html to latex.")

(defvar w3-icon-directory "http://cs.indiana.edu/elisp/w3/icons/"
  "*Where to find standard icons.  Must end in a /!")

(defvar w3-icon-format 'xbm
  "*What file extension icons end in.  This is a symbol, string, or nil.
If nil, then no file extension is used.")

(defvar w3-indent-level 4
  "*Default # of spaces to indent instead of using TABs.  This is
necessary to preserve tabs in PRE segments yet still get smaller
indentation for lists, etc.")

(defvar w3-keep-old-buffers t
  "*Whether to keep old buffers around when following links.")

(defvar w3-latex-docstyle "{article}"
  "*The documentstyle to use when printing/mailing converted HTML
files in LaTeX.  Good defaults are:
{article}, [psfig,twocolumn]{article}, etc.")

(defvar w3-link-info-display-function nil
  "*A function to call to get extra information about a link and
include it in a buffer.  Will be placed after the link and any other
delimiters.")

(defvar w3-mail-command 'mail
  "*This function will be called whenever w3 needs to send mail.  It should
enter a mail-mode-like buffer in the current window.
`w3-mail-other-window-command' will be used if w3-mutable-windows is t.
The commands `mail-to' and `mail-subject' should still work in this
buffer, and it should use mail-header-separator if possible.")

(defvar w3-mail-other-window-command 'mail-other-window
  "*This function will be called whenever w3 needs to send mail in
another window.  It should enter a mail-mode-like buffer in a
different window.  The commands `mail-to' and `mail-subject' should still
work in this buffer, and it should use mail-header-separator if
possible.")

(defvar w3-max-inlined-image-size nil
  "*The maximum byte size of a file to transfer as an inlined image.
If an image is being retrieved and exceeds this size, then it will be
cancelled.  This works best on HTTP/1.0 servers that send a
Content-length header, otherwise the image is retrieved up until the
max number of bytes is retrieved, then killed.")

(defvar w3-max-menu-length 35
  "*The maximum length of a pulldown menu before it will be split into
smaller chunks, with the first part as a submenu, followed by the rest
of the menu.")

(defvar w3-max-menu-width 40 "*The maximum width of a pulldown menu choice.")

(defvar w3-modeline-format
  '("  " ("W3"
	(w3-netscape-emulation-minor-mode
	 " (NS)")
	(w3-lynx-emulation-minor-mode
	 " (Lynx)")
	": "
	(40 (-40 "%b"))
	" "
	(w3-current-isindex "[Searchable]  ")
	"%p" "  " global-mode-string))
  "*The modeline format string when in w3 mode")

(defvar w3-mule-attribute 'underline
  "*How to highlight items in Mule (Multi-Linugual Emacs).")

(defvar w3-mutable-windows nil
  "*Controls how new WWW documents are displayed.  If this is set to
non-nil and pop-up-windows is non-nil, then new buffers will be shown
in another window.  If either is nil, then it will replace the document
in the current window.")

(defvar w3-netscape-configuration-file nil
  "*A Netscape-for-X style configuration file.  This file will only be read if
and only if `w3-use-netscape-configuration-file' is non-nil.")

(defvar w3-netscape-compatible-comments t
  "*Whether to honor netscape-style <! > comments.
Ye gods I wish I could turn this off by default.")

(defvar w3-notify 'semibully
  "*Selects the behavior when w3 page is ready.
This variable may have one of the following values:

newframe   -- put the w3 page in its own frame
bully      -- make the w3 page the current buffer and only window
semibully  -- make the w3 page the current buffer in the same window
aggressive -- make the w3 page the current buffer in the other window
friendly   -- display  w3page in other window but don't make current
polite     -- don't display w3 page, but prints message when ready (beeps)
quiet      -- like `polite', but don't beep
meek       -- make no indication that page is ready

Any other value of `w3-notify' is equivalent to `meek'.")

(defvar w3-personal-annotation-directory nil
  "*Directory where w3 looks for personal annotations.
This is a directory that should hold the personal annotations stored in
a Mosaic-compatible format.")

(defvar w3-ppmtoxbm-command "ppmtopgm | pgmtopbm | pbmtoxbm"
  "*The command used to convert from the portable-pixmap graphics format
to an x bitmap.  This will only ever be used if XEmacs doesn't have support
for XPM.")

(defvar w3-ppmtoxpm-command "ppmtoxpm"
  "*The command used to convert from the portable-pixmap graphics format
to XPM.  The XPM _MUST_ be in version 3 format.")

(defvar w3-print-command "lpr -h -d"
  "*Print command for dvi files.
This is usually lpr -h -d to send it to a postscript printer, but you can set
it up so that it is any command that takes a dvi file as its last argument.")

(defvar w3-reuse-buffers 'reuse
  "What to do when following a link will re-fetch a document that has
already been fetched into a W3 buffer.  Possible values are: nil,
'yes, and 'no.  Nil means ask the user if we should reuse the buffer.
 A value of 'yes means assume the user wants us to reuse the buffer.
A value of 'no means assume the user wants us to re-fetch the document.

This will also accept:
'no 'never 'reload	==> always reload
'yes 'reuse 'always	==> always reuse
'ask nil		==> always ask")

(defvar w3-right-border 2
  "*Amount of space to leave on right margin of WWW buffers.
This amount is subtracted from (window-width) for each new WWW buffer
and used as the new fill-column.")

(defvar w3-maximum-line-length nil
  "*Maximum length of a line.  If nil, then lines can extend all the way to
the window margin.  If a number, the smaller of this and
(- (window-width) w3-right-border) is used.")

(defvar w3-right-justify-address t
  "*Whether to make address fields right justified, like Arena.")

(defvar w3-show-headers nil
  "*This is a list of regexps that match HTTP/1.0 headers to show at
the end of a buffer.  All the headers being matched against will be
in lowercase.  All matching headers will be inserted at the end of the
buffer in a <UL> list.")

(defvar w3-show-status t
  "*Whether to show a running total of bytes transferred.  Can cause a
large hit if using a remote X display over a slow link, or a terminal
with a slow modem.")

(defvar w3-starting-documents
  '(("Internet Starting Points"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/StartingPoints/NetworkStartingPoints.html")
    ("Internet Resources Meta-index"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/MetaIndex.html")
    ("NCSA's What's New"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/whats-new.html"))
  "*An assoc list of titles and URLs for quick access.  These are just
defaults so that new users have somewhere to go.")

(defvar w3-temporary-directory "/tmp" "*Where temporary files go.")

(defvar w3-track-last-buffer nil
  "*Whether to track the last w3 buffer to automatically switch to with
 M-x w3.")

(defvar w3-track-mouse t
  "*Whether to track the mouse and message the url under the mouse.")

(defvar w3-use-forms-index t
  "*Non-nil means translate <ISINDEX> tags into a hypertext form.
A single text entry box will be drawn where the ISINDEX tag appears.
If t, the isindex handling will be the same as Mosaic for X.")

(defvar w3-use-html2latex nil
  "*This controls how HTML is converted into LaTeX for printing or mailing.
If nil, the w3-convert-html-to-latex function is used instead of the
html2latex in a subprocess.  The lisp function gives slightly better
formatting in my biased opinion.")

(defvar w3-use-netscape-configuration-file nil
  "*Whether to use a netscape configuration file to determine things like
home pages, link colors, etc.  If non-nil, then `w3-netscape-configuration-file'
is read in at startup.")

(defvar w3-honor-stylesheets t
  "*Whether to let a document specify a CSS stylesheet.")

(defvar w3-user-colors-take-precedence nil
  "*Whether to let a document define certain colors about itself.
Like foreground and background colors and pixmaps, color of links and
visited links, etc.")

(defvar w3-gc-cons-threshold-multiplier 1
  "Amount to temporarily multiply gc-cons-threshold by when parsing HTML.
Setting this to a number greater than 1 will result in less frequent
garbage collections when parsing an HTML document, which may often speed
up handling of a large document with many elements.  The disadvantage is
that it allows Emacs's total memory usage to grow larger, which may result
in later garbage collections taking more time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-load-hook nil "*Hooks to be run after loading w3.")
(defvar w3-mode-hook nil "*Hooks to be run after entering w3-mode.")
(defvar w3-file-prepare-hook nil
  "*Hooks to be run before preparing a buffer.")
(defvar w3-file-done-hook nil "*Hooks to be run after preparing a buffer.")
(defvar w3-source-file-hook nil
  "*Hooks to be run after getting document source.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Figure out what flavor of emacs we are running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "*In XEmacs?.")

(defvar w3-running-FSF19 (and (string-match "^19" emacs-version)
			      (not w3-running-xemacs))
  "*In FSF v19 emacs?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link delimiting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-delimit-emphasis 'guess
  "*Whether to use characters at the start and end of each bold/italic
region.  Obsolete variable (almost) - all this should be specified by the
default stylesheet.")

(defvar w3-link-start-delimiter '("[[" . "{{")
  "*Put this at front of link if w3-delimit-links is t.")

(defvar w3-link-end-delimiter '("]]" . "}}")
  "*Put this at end of link if w3-delimit-links is t.")

(defvar w3-delimit-links 'guess
  "*Put brackets around links?  If this variable is eq to 'linkname, then
it will put the link # in brackets after the link text.  If it is nil, then
it will not put anything.  If it is non-nil and not eq to 'linkname, then
it will put [[ & ]] around the entire text of the link.  Is initially set
to be t iff in normal emacs.  Nil if in XEmacs or lucid emacs, since links
should be in different colors/fonts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics parsing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-graphics-always-show-entities t
  "*Set to t to always show graphic entities, regardless of the value of
w3-delay-image-loads.  Useful if you keep the entities locally and aren't
worried about the transfer time on something that small.")

(defvar w3-graphics-list nil
  "*List of graphics already read in.")

(defvar w3-delay-image-loads nil
  "*Delay loading images for w3 or not?")

(defvar w3-delayed-images nil
  "*A buffer-local variable holding positions and urls of images within
the buffer.")

(defvar w3-image-mappings
  '(
    ("image/x-xbitmap"        . xbm)
    ("image/xbitmap"          . xbm)
    ("image/xbm"              . xbm)
    ("image/jpeg"             . jpeg)
    ("image/gif"              . gif)
    ("image/png"              . png)
    ("image/x-fax"            . g3fax)
    ("image/x-raster"         . rast)
    ("image/windowdump"       . xwd)
    ("image/x-icon"           . icon)
    ("image/portable-graymap" . pgm)
    ("image/portable-pixmap"  . ppm)
    ("image/x-pixmap"         . xpm)
    ("image/x-xpixmap"        . xpm)
    ("image/pict"             . pict)
    ("image/x-rgb"            . sgi)
    ("image/x-sgi"            . sgi)
    ("image/x-macpaint"       . macpt)
    ("image/x-targa"          . tga)
    ("image/tiff"             . tiff)
    ) "*How to map MIME types to image types for the `image' package.")

(defvar w3-color-use-reducing 'guess
  "*Whether to use ppmquant/ppmdither to do color reducing for inlined images.
If you are using a 24bit display, you should set this to nil.")

(defvar w3-color-max-red 4
  "*Max # of red cells to allocate for inlined images.")

(defvar w3-color-max-green 4
  "*Max # of green cells to allocate for inlined images.")

(defvar w3-color-max-blue 4
  "*Max # of blue cells to allocate for inlined images.")

(defvar w3-color-filter 'ppmdither
  "*How to do color reducing on inlined images.
This should be a symbol, either ppmdither or ppmquant.
This variable only has any meaning if w3-color-use-reducing is non-nil.
Possible values are:

ppmquant    :== Use the ppmquant program to reduce colors.  The product
                of w3-color-max-[red|green|blue] is used as the maximum
                number of colors.
ppmdither   :== Use the ppmdither program to reduce colors.

any string :== Use this string as the filter.  No interpretation of it
               is done at all.  Example is:
               ppmquant -fs -map ~/pixmaps/colormap.ppm")

(defvar w3-ppmdither-is-buggy t
  "*The ppmdither which comes with pbmplus/netpbm releases through 
1mar1994 at least ignores the 'maxval' in its input.  This can cause
trouble viewing black-and-white gifs.  If this variable is set, a
(harmless) 'pnmdepth 255' step is inserted to work around this bug.
You can test your ppmdither by doing 
  ppmmake white 100 100 | pnmdepth 1 | ppmdither | pnmdepth 255 | ppmhist
If the output has a single line like this:
  255 255 255     255     10000
then it's safe to set this variable to nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to look up styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-style-tags-assoc
  '(
    (b       . ("*" . "*"))
    (address . ("*" . "*"))
    (byline  . ("_" . "_"))
    (cite    . ("_" . "_"))
    (cmd     . ("*" . "*"))
    (dfn     . ("*" . "*"))
    (em      . ("~" . "~"))
    (i       . ("~" . "~"))
    (q       . ("\"" . "\""))
    (removed . ("" . ""))
    (s       . ("" . ""))
    (strong  . ("*" . "*"))
    (sub     . ("" . ""))
    (sup     . ("" . ""))
    (u       . ("_" . "_"))
    )
  "*An assoc list of emphasis tags and their corresponding
begin and end characters.")

(defvar w3-header-chars-assoc
  '(
    (h1 . (?* ?* w3-upcase-region))
    (h2 . (?* ?* w3-upcase-region))
    (h3 . (?- ?- w3-upcase-region))
    (h4 . (nil ?= nil))
    (h5 . (nil ?= nil))
    (h6 . (nil ?: nil)))
  "*An assoc list of header tags and a list of formatting instructions.
This list consists of 3 items - the first item is no longer used.  The
second item is the character to insert after the header.  A <BR> is
inserted before and after this string. And the third is a function to
call on the region between the start and end of the header.  This will
be called with 2 arguments, the buffer positions of the start and end
of the headers.")

;; Store the database of HTML general entities.
(defvar w3-html-entities 
  '(
    (excl        .  33)
    (quot        .  34)
    (num         .  35)
    (dollar      .  36)
    (percent     .  37)
    (amp         .  38)
    (rsquo       .  39)
    (apos        .  39)
    (lpar        .  40)
    (rpar        .  41)
    (times       .  42)
    (ast         .  42)
    (plus        .  43)
    (comma       .  44)
    (period      .  46)
    (colon       .  58)
    (semi        .  59)
    (lt          .  60)
    (equals      .  61)
    (gt          .  62)
    (quest       .  63)
    (commat      .  64)
    (lsqb        .  91)
    (rsqb        .  93)
    (uarr        .  94)
    (lowbar      .  95)
    (lsquo       .  96)
    (lcub        . 123)
    (verbar      . 124)
    (rcub        . 125)
    (tilde       . 126)
    (nbsp        . 160)
    (iexcl       . 161)
    (cent        . 162)
    (pound       . 163)
    (curren      . 164)
    (yen         . 165)
    (brvbar      . 166)
    (sect        . 167)
    (uml         . 168)
    (copy        . 169)
    (ordf        . 170)
    (laquo       . 171)
    (not         . 172)
    (shy         . 173)
    (reg         . 174)
    (macr        . 175)
    (deg         . 176)
    (plusmn      . 177)
    (sup2        . 178)
    (sup3        . 179)
    (acute       . 180)
    (micro       . 181)
    (para        . 182)
    (middot      . 183)
    (cedil       . 184)
    (sup1        . 185)
    (ordm        . 186)
    (raquo       . 187)
    (frac14      . 188)
    (frac12      . 189)
    (frac34      . 190)
    (iquest      . 191)
    (Agrave      . 192)
    (Aacute      . 193)
    (Acirc       . 194)
    (Atilde      . 195)
    (Auml        . 196)
    (Aring       . 197)
    (AElig       . 198)
    (Ccedil      . 199)
    (Egrave      . 200)
    (Eacute      . 201)
    (Ecirc       . 202)
    (Euml        . 203)
    (Igrave      . 204)
    (Iacute      . 205)
    (Icirc       . 206)
    (Iuml        . 207)
    (ETH         . 208)
    (Ntilde      . 209)
    (Ograve      . 210)
    (Oacute      . 211)
    (Ocirc       . 212)
    (Otilde      . 213)
    (Ouml        . 214)
    (times       . 215)
    (Oslash      . 216)
    (Ugrave      . 217)
    (Uacute      . 218)
    (Ucirc       . 219)
    (Uuml        . 220)
    (Yacute      . 221)
    (THORN       . 222)
    (szlig       . 223)
    (agrave      . 224)
    (aacute      . 225)
    (acirc       . 226)
    (atilde      . 227)
    (auml        . 228)
    (aring       . 229)
    (aelig       . 230)
    (ccedil      . 231)
    (egrave      . 232)
    (eacute      . 233)
    (ecirc       . 234)
    (euml        . 235)
    (igrave      . 236)
    (iacute      . 237)
    (icirc       . 238)
    (iuml        . 239)
    (eth         . 240)
    (ntilde      . 241)
    (ograve      . 242)
    (oacute      . 243)
    (ocirc       . 244)
    (otilde      . 245)
    (ouml        . 246)
    (divide      . 247)
    (oslash      . 248)
    (ugrave      . 249)
    (uacute      . 250)
    (ucirc       . 251)
    (uuml        . 252)
    (yacute      . 253)
    (thorn       . 254)
    (yuml        . 255)

    ;; Special handling of these
    (frac56      . "5/6")
    (frac16      . "1/6")
    (frac45      . "4/5")
    (frac35      . "3/5")
    (frac25      . "2/5")
    (frac15      . "1/5")
    (frac23      . "2/3")
    (frac13      . "1/3")
    (frac78      . "7/8")
    (frac58      . "5/8")
    (frac38      . "3/8")
    (frac18      . "1/8")
    
    ;; The following 5 entities are not mentioned in the HTML 2.0
    ;; standard, nor in any other HTML proposed standard of which I
    ;; am aware.  I am not even sure they are ISO entity names.  ***
    ;; Hence, some arrangement should be made to give a bad HTML
    ;; message when they are seen.
    (ndash       .  45)
    (mdash       .  45)
    (emsp        .  32)
    (ensp        .  32)
    (sim         . 126)
    (le          . "<=")
    (agr         . "alpha")
    (rdquo       . "''")
    (ldquo       . "``")
    (trade       . "(TM)")
    ;; To be done
    ;; (shy      . ????) ; soft hyphen
    )
  "*An assoc list of entity names and how to actually display them.")

(defvar w3-graphic-entities
  '(
    (archive             "archive"                )
    (audio               "audio"                  )
    (binary.document     "binary.document"        )
    (binhex.document     "binhex.document"        )
    (calculator          "calculator"             )
    (caution             "caution"                )
    (cd.i                "cd.i"                   )
    (cd.rom              "cd.rom"                 )
    (clock               "clock"                  )
    (compressed.document "compressed.document"    )
    (disk.drive          "disk.drive"             )
    (diskette            "diskette"               )
    (document            "document"               )
    (fax                 "fax"                    )
    (filing.cabinet      "filing.cabinet"         )
    (film                "film"                   )
    (fixed.disk          "fixed.disk"             )
    (folder              "folder"                 )
    (form                "form"                   )
    (ftp                 "ftp"                    )
    (glossary            "glossary"               )
    (gopher              "gopher"                 )
    (home                "home"                   )
    (html                "html"                   )
    (image               "image"                  )
    (index               "index"                  )
    (keyboard            "keyboard"               )
    (mail                "mail"                   )
    (mail.in             "mail.in"                )
    (mail.out            "mail.out"               )
    (map                 "map"                    )
    (mouse               "mouse"                  )
    (new                 "new"                    )
    (next                "next"                   )
    (notebook            "notebook"               )
    (parent              "parent"                 )
    (play.fast.forward   "play.fast.forward"      )
    (play.fast.reverse   "play.fast.reverse"      )
    (play.pause          "play.pause"             )
    (play.start          "play.start"             )
    (play.stop           "play.stop"              )
    (previous            "previous"               )
    (prince              "prince" "the artist formerly known as prince")
    (princesymbol        "prince" "the artist formerly known as prince")
    (printer             "printer"                )
    (sadsmiley           "sadsmiley"          ":(")
    (smiley              "smiley"             ":)")
    (stop                "stop"                   )
    (summary             "summary"                )
    (telephone           "telephone"              )
    (telnet              "telnet"                 )
    (text.document       "text.document"          )
    (tn3270              "tn3270"                 )
    (toc                 "toc"                    )
    (trash               "trash"                  )
    (unknown.document    "unknown.document"       )
    (uuencoded.document  "uuencoded.document"     )
    (work                "work"                   )
    (www                 "www"                    )
    )
  "List of graphical entity names and the tail end of a URL for them.
If there is a 3rd item in the list, it is the alternative text to use
for the image.")

(defvar w3-list-chars-assoc 
  '(
    (ul . ("o" "*" "+" ">"))
    (ol . ("." ")" "]" ":"))
    (dl . ("o" "*" "+" ">")))
  "An assoc list of characters to put at the front of list items.  It is
keyed on the type of list, followed by a list of items.  Each item should
be placed in the nth position of the list, where n is the nesting level it
should be used for.  n starts at 1.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-navigate-menu nil)
(defvar w3-popup-menu
  '("Emacs-W3 Commands"
    ["Back" w3-backward-in-history t]
    ["Forward" w3-forward-in-history t]
    "---"
    ["Add annotation" w3-annotation-add t]
    )
  "The shorter popup menu.")

(defvar w3-documentation-root "http://www.cs.indiana.edu/elisp/w3/docs/"
  "*Where the w3 documentation lives.  This MUST end in a slash.")

(defvar w3-graphlink-menu
  '(("Open this Image (%s)"     . w3-fetch)
    ("Save this Image As..."    . w3-download-url)
    ("Copy this Image Location" . w3-save-url)
    ("Information on this Image". w3-popup-image-info))
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
( (label . function)), function is called with one argument, the URL of
the link.  Each label can have exactly one `%s' that will be replaced by
the URL of the link.")

(defvar w3-hyperlink-menu
  '(("Open this Link (%s)"        . w3-fetch)
    ("Add Bookmark for this Link" . w3-hotlist-add-document)
    ("New Window with this Link"  . w3-fetch-other-frame)
    ("Save Link As..."            . w3-download-url)
    ("Copy this Link Location to Clipboard" . w3-save-url)
    ("Information on this Link"   . w3-popup-info))
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
( (label . function)), function is called with one argument, the URL of
the link.  Each label can have exactly one `%s' that will be replaced by
the URL of the link.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables internal to W3, you should not change any of these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-form-radio-elements nil "Internal variable - do not touch!")
(defvar w3-form-elements nil "Internal variable - do not touch!")

(defvar w3-invisible-href-list nil
  "A list of 'invisible' graphic links in the current buffer.")

(defconst w3-state-locator-variable
  '(
    :align
    :background
    :center
    :depth
    :figalt
    :figdata
    :fillcol
    :form
    :formnum
    :header-start
    :href
    :link-args
    :image
    :lists
    :map
    :name
    :needspace
    :next-break
    :nofill
    :nowrap
    :optarg
    :options
    :pre-start
    :select
    :secret
    :table
    :text-mangler
    :title
    :link-title
    :w3-graphic
    :zone
    :label-text
    :seen-this-url
    )
  "A list of all the various state kept in the drawing engine.
This is used by the `w3-get-state' and `w3-put-state' macros.")

(defvar w3-state-vector
  (make-vector (1+ (length w3-state-locator-variable)) nil)
  "Various state shit kept by emacs-w3.")

(defvar w3-user-stylesheet nil
  "The global stylesheet for this user.")

(defvar w3-current-stylesheet nil
  "The stylesheet for this document.")

(defvar w3-base-alist nil
  "An assoc list of named BASE tags in the current document.")

(defvar w3-blinking-buffs nil
  "A list of buffers with blinking text in them.
This is used to optimize when we change a face so the entire display
doesn't flash every second, whether we've run into a buffer that is
displaying blinking text or not.")

(defvar w3-last-fill-pos nil
  "An internal variable for the new display engine that specifies the
last character position that was correctly filled.")

(defvar w3-last-tag nil
  "An internal variable for the new display engine that specifies the
last tag processed.")

(defvar w3-table-info nil
  "An internal variable for the new display engine for keeping table data
during the pre-pass parsing.")

(defvar w3-current-formatter nil
  "Current formatter function.")

(defvar w3-draw-buffer nil
  "Where we are currently drawing into.  This _must_ be a buffer object
when it is referenced.")

(defvar w3-active-faces nil "The list of active faces.")
(defvar w3-active-voices nil "The list of active voices.")

(defvar w3-netscape-variable-mappings
  '(("PRINT_COLOR"	. ps-print-color-p)
    ("DITHER_IMAGES"	. w3-color-use-reducing)
    ("SOCKS_HOST"	. url-socks-host)
    ("ORGANIZATION"	. url-user-organization)
    ("EMAIL_ADDRESS"	. url-personal-mail-address)
    ("REAL_NAME"	. url-user-real-name)
    ("NEWSGROUP_DESCRIPTIONS" . url-show-newsgroup-descriptions)
    ("NNTPSERVER"	. url-news-server)
    ("AUTOLOAD_IMAGES"	. w3-delay-image-loads)
    ("HOME_DOCUMENT"	. w3-default-homepage)
    ("UNDERLINE_LINKS"	. w3-underline-links)
    ("TMPDIR"		. url-temporary-directory))
  "A mapping from netscape configuration file options to w3 variables.")
     
(defvar w3-acceptable-protocols-alist
  '(("Gopher"                           . "gopher")
    ("TN3270 (IBM Mainframe emulation)" . "tn3270")
    ("Interactive Telnet Session"       . "telnet")
    ("Local file or file over ftp"      . "file")
    ("File on an http server"           . "http")
    ("Usenet newsgroup/article"         . "news")
    ("Mail session"                     . "mailto"))
  "An assoc list of descriptive labels and the corresponding URL stub.")

(defvar w3-annotation-marker "<ncsa-annotation-format-1>")
(defvar w3-annotation-minor-mode nil "Whether we are in the minor mode.")
(defconst w3-bug-address "wmperry@cs.indiana.edu"
  "Address of current maintainer, where to send bug reports.")
(defvar w3-continuation '(url-uncompress url-clean-text)
  "List of functions to call to process a document completely.")
(defvar w3-current-annotation nil "URL of document we are annotating...")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one.")
(defvar w3-current-links nil "An assoc list of <LINK> tags for this doc.")
(defvar w3-current-source nil "Source of current document.")
(defvar w3-current-parse nil "Parsed version of current document.")
(defconst w3-default-continuation '(url-uncompress url-clean-text) 
  "Default action to start with - cleans text and uncompresses if necessary.")
(defvar w3-editing-annotation nil "Are we editing an annotation or not?")
(defvar w3-find-this-link nil "Link to go to within a document.")
(defvar w3-hidden-forms nil "List of hidden form areas and their info.")
(defvar w3-hotlist nil "Default hotlist.")
(defvar w3-icon-path-cache nil "Cache of where we found icons for entities.")
(defvar w3-last-buffer nil "The last W3 buffer visited.")
(defvar w3-personal-annotations nil "Assoc list of personal annotations.")
(defvar w3-print-next nil "Should we latex & print the next doc?")
(defvar w3-roman-characters "ivxLCDMVX" "Roman numerals.")
(defvar w3-setup-done nil "Have we been through setup code yet?")
(defvar w3-source nil "Should we source the next document or not?")

(defvar w3-strict-width nil
  "*This variable will control how wide emacs thinks the current window is.
This is useful when working in batch mode, and (window-width) returns the
wrong value.  If the value is nil, it will use the value (window-width)
returns.")

(defvar w3-submit-button nil
  "A widget object specifying what button was pressed to submit a form.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-local variables to keep around when going into w3-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-e19-hotlist-menu nil
  "A menu for hotlists.")

(defvar w3-e19-links-menu nil
  "A buffer-local menu for links.")

(defvar w3-id-positions nil "Internal use only.")
(defvar w3-imagemaps nil "Internal use only.")

(defvar w3-persistent-variables
  '(
    ;; So we can show the URL in the list-buffers listing
    list-buffers-directory
    ;; So widgets don't get lost
    widget-field-new
    w3-form-radio-elements
    w3-form-elements
    url-current-callback-func
    url-current-content-length
    url-current-file
    url-current-mime-encoding
    url-current-mime-headers
    url-current-mime-type
    url-current-mime-viewer
    url-current-port
    url-current-referer
    url-current-server
    url-current-type
    url-current-user
    w3-e19-links-menu
    w3-current-parse
    w3-current-annotation
    w3-current-isindex
    w3-current-last-buffer
    w3-current-links
    w3-current-source
    w3-delayed-images
    w3-hidden-forms
    w3-invisible-href-list
    w3-state-vector
    w3-current-stylesheet
    w3-form-labels
    w3-id-positions
    w3-imagemaps
    )
  "A list of variables that should be preserved when entering w3-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes.")

(modify-syntax-entry ?' "\"" w3-parse-args-syntax-table)
(modify-syntax-entry ?` "\"" w3-parse-args-syntax-table)
(modify-syntax-entry ?< "(>" w3-parse-args-syntax-table)
(modify-syntax-entry ?> ")<" w3-parse-args-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emulation stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-netscape-emulation-minor-mode nil
  "Whether we are in the netscape emulation minor mode.")
(defvar w3-netscape-emulation-minor-mode-map (make-sparse-keymap)
  "Keymap for netscape emulation.")
(defvar w3-lynx-emulation-minor-mode nil
  "Whether we are in the lynx emulation minor mode.")
(defvar w3-lynx-emulation-minor-mode-map (make-sparse-keymap)
  "Keymap for lynx emulation.")
(defvar w3-last-search-item nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-form-labels nil "")
(mapcar (function
	 (lambda (var)
	   (if (boundp var)
	       (make-variable-buffer-local var)))) w3-persistent-variables)
(make-variable-buffer-local 'w3-state-vector)
(make-variable-buffer-local 'w3-current-stylesheet)
(make-variable-buffer-local 'w3-base-alist)
(make-variable-buffer-local 'w3-annotation-minor-mode)
(make-variable-buffer-local 'w3-last-tag)
(make-variable-buffer-local 'w3-last-fill-pos)
(make-variable-buffer-local 'w3-table-info)
(make-variable-buffer-local 'w3-draw-buffer)
(make-variable-buffer-local 'w3-current-formatter)
(make-variable-buffer-local 'w3-active-faces)
(make-variable-buffer-local 'w3-default-style)
(make-variable-buffer-local 'w3-netscape-emulation-minor-mode)
(make-variable-buffer-local 'w3-lynx-emulation-minor-mode)
(make-variable-buffer-local 'w3-last-search-item)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Keymap definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mode-map (make-keymap) "Keymap to use in w3-mode.")
(defvar w3-annotation-minor-mode-map (make-keymap) "Keymap for annotation.")

(suppress-keymap w3-mode-map)

(define-key w3-mode-map "h" (make-sparse-keymap))
(define-key w3-mode-map "H" (make-sparse-keymap))
(define-key w3-mode-map "a" (make-sparse-keymap))

(define-key w3-mode-map "ha"       'w3-hotlist-apropos)
(define-key w3-mode-map "hd"       'w3-hotlist-delete)
(define-key w3-mode-map "hi"       'w3-hotlist-add-document)
(define-key w3-mode-map "hv"       'w3-show-hotlist)
(define-key w3-mode-map "hr"       'w3-hotlist-rename-entry)
(define-key w3-mode-map "hu"       'w3-use-hotlist)
(define-key w3-mode-map "hA"       'w3-hotlist-append)
(define-key w3-mode-map "hI"       'w3-hotlist-add-document-at-point)
(define-key w3-mode-map "hR"       'w3-hotlist-refresh)

(define-key w3-mode-map "ai"       'w3-annotation-add)
(define-key w3-mode-map "ad"       'w3-delete-personal-annotation)
(define-key w3-mode-map "ae"       'w3-annotation-edit)

(define-key w3-mode-map "HF"       'w3-forward-in-history)
(define-key w3-mode-map "HB"       'w3-backward-in-history)
(define-key w3-mode-map "Hv"       'w3-show-history-list)

(define-key w3-mode-map " "	   'w3-scroll-up)
(define-key w3-mode-map "<"        'beginning-of-buffer)
(define-key w3-mode-map ">"        'end-of-buffer)
(define-key w3-mode-map "?"        'w3-help)
(define-key w3-mode-map "B"        'w3-backward-in-history)
(define-key w3-mode-map "F"        'w3-forward-in-history)
(define-key w3-mode-map "G"        'w3-show-graphics)
(define-key w3-mode-map "I"        'w3-popup-info)
(define-key w3-mode-map "K"        'w3-save-this-url)
(define-key w3-mode-map "P"        'w3-print-url-under-point)
(define-key w3-mode-map "Q"        'w3-leave-buffer)
(define-key w3-mode-map "R"        'w3-refresh-buffer)
(define-key w3-mode-map "S"        'w3-source-document-at-point)
(define-key w3-mode-map "U"        'w3-use-links)
(define-key w3-mode-map "V"        'w3-view-this-url)
(define-key w3-mode-map "\C-?"     'scroll-down)
(define-key w3-mode-map "\C-c\C-b" 'w3-show-history-list)
(define-key w3-mode-map "\C-c\C-v" 'w3-version)
(define-key w3-mode-map "\C-o"     'w3-fetch)
(define-key w3-mode-map "\M-M"     'w3-mail-document-under-point)
(define-key w3-mode-map "\M-\C-i"  'w3-insert-this-url)
(define-key w3-mode-map "\M-m"	   'w3-mail-current-document)
(define-key w3-mode-map "\M-s"	   'w3-search)
(define-key w3-mode-map "\M-\r"    'w3-follow-inlined-image)
(define-key w3-mode-map "\r"       'w3-widget-button-press)
(define-key w3-mode-map "b"	   'widget-backward)
(define-key w3-mode-map "c"        'w3-mail-document-author)
(define-key w3-mode-map "f"	   'widget-forward)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "i"        'w3-document-information)
(define-key w3-mode-map "k"        'w3-save-url)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'widget-forward)
(define-key w3-mode-map "o"	   'w3-open-local)
(define-key w3-mode-map "p"        'w3-print-this-url)
(define-key w3-mode-map "q"	   'w3-quit)
(define-key w3-mode-map "r"        'w3-reload-document)
(define-key w3-mode-map "s"        'w3-source-document)
(define-key w3-mode-map "u"        'w3-leave-buffer)
(define-key w3-mode-map "v"	   'url-view-url)
(define-key w3-mode-map "w"        'w3-submit-bug)

;; Emulate some netscape stuff by default
(define-key w3-mode-map [(control alt t)] 'url-list-processes)
(define-key w3-mode-map [(control meta t)] 'url-list-processes)

;; Widget navigation
(define-key w3-mode-map "\t"       'widget-forward)
(define-key w3-mode-map [(shift tab)] 'widget-backward)
  
(define-key w3-annotation-minor-mode-map "\C-c\C-c"
  'w3-personal-annotation-finish)

;;; This is so we can use a consistent method of checking for mule support
;;; Emacs-based mule uses (boundp 'MULE), but XEmacs-based mule uses
;;; (featurep 'mule) - I choose to use the latter.

(if (boundp 'MULE)
    (provide 'mule))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyword definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-keyword)
(provide 'w3-vars)
