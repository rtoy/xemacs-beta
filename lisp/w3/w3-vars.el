;;; w3-vars.el,v --- All variable definitions for emacs-w3
;; Author: wmperry
;; Created: 1997/02/14 17:57:21
;; Version: 1.89
;; Keywords: comm, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable definitions for w3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst w3-version-number
  (let ((x "p3.0.59"))
    (if (string-match "State:[ \t\n]+.\\([^ \t\n]+\\)" x)
	(setq x (substring x (match-beginning 1) (match-end 1)))
      (setq x (substring x 1)))
    (mapconcat
     (function (lambda (x) (if (= x ?-) "." (char-to-string x)))) x ""))
  "Version # of w3-mode.")

(defconst w3-version-date (let ((x "1997/02/14 17:57:21"))
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

(defvar w3-default-homepage nil
  "*The url to open at startup.  It can be any valid URL.
This will default to the environment variable WWW_HOME if you do not
set it in your .emacs file. If WWW_HOME is undefined, then it will
default to  the hypertext documentation for W3 at Indiana University.")

(defvar w3-default-stylesheet nil
  "*The filename of the users default stylesheet.")

(defvar w3-do-incremental-display nil
  "*Whether to do incremental display of pages or not.")

(defvar w3-dump-to-disk nil
  "*If non-nil, all W3 pages loaded will be dumped to disk.")

(defvar w3-echo-link '(title url text name)
  "*Whether to display the URL of a link when tabbing through links.
Value is a list of one or more of the following symbols:

  url    == url of the target
  text   == text of the link
  title  == title attribute of the link
  name   == name or id attribute of the link

If none of the information is available, nothing will be shown for the link
in menus, etc.")
  
(defvar w3-horizontal-rule-char ?-
  "*The character to use to create a horizontal rule.
Must be the character's code, not a string.  This character is
replicated across the screen to create a division.")

(defvar w3-fetch-with-default t
  "*Whether `w3-fetch' should determine a good starting URL as a default.")

(defvar w3-hotlist-file nil
  "*Hotlist filename.
This should be the name of a file that is stored in either
NCSA's Mosaic/X or Netscape/X format.  It is used to keep a listing
of commonly accessed URL's without having to go through 20 levels of
menus to get to them.")

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

(defvar w3-mail-command 'mail
  "*This function will be called whenever w3 needs to send mail.  It should
enter a mail-mode-like buffer in the current window.
The commands `mail-to' and `mail-subject' should still work in this
buffer, and it should use mail-header-separator if possible.")

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

(defvar w3-popup-menu-on-mouse-3 t
  "*Non-nil value means W3 should provide context-sensitive menus on mouse-3.
A nil value means W3 should not change the binding of mouse-3.")

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

(defvar w3-right-margin 2
  "*Amount of space to leave on right margin of WWW buffers.
This amount is subtracted from (window-width) for each new WWW buffer
and used as the new fill-column.")

(defvar w3-maximum-line-length nil
  "*Maximum length of a line.  If nil, then lines can extend all the way to
the window margin.")

(defvar w3-temporary-directory "/tmp" "*Where temporary files go.")

(defvar w3-track-last-buffer nil
  "*Whether to track the last w3 buffer to automatically switch to with
 M-x w3.")

(defvar w3-track-mouse t
  "*Whether to track the mouse and message the url under the mouse.")

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
;;; Graphics parsing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-popup-menu
  '("Emacs-W3 Commands"
    ["Back" w3-history-backward (car (w3-history-find-url-internal (url-view-url t)))]
    ["Forward" w3-history-forward (cdr (w3-history-find-url-internal (url-view-url t)))]
    )
  "The shorter popup menu.")

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

(defvar w3-documentation-root "http://www.cs.indiana.edu/elisp/w3/docs/"
  "*Where the w3 documentation lives.  This MUST end in a slash.")

(defvar w3-defined-link-types
  ;; This is the HTML3.0 list (downcased) plus "made".
  '("previous" "next" "up" "down" "home" "toc" "index" "glossary"
    "copyright" "bookmark" "help" "made")
  "A list of the (lower-case) names which have special significance
as the values of REL or REV attributes of <link> elements.  They will
be presented on the toolbar or the links menu, for instance.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables internal to W3, you should not change any of these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-form-radio-elements nil "Internal variable - do not touch!")
(defvar w3-form-elements nil "Internal variable - do not touch!")

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

(defvar w3-active-faces nil "The list of active faces.")
(defvar w3-active-voices nil "The list of active voices.")

(defvar w3-netscape-variable-mappings
  '(("PRINT_COLOR"	. ps-print-color-p)
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

(defconst w3-bug-address "wmperry@cs.indiana.edu"
  "Address of current maintainer, where to send bug reports.")
(defvar w3-continuation '(url-uncompress url-clean-text)
  "List of functions to call to process a document completely.")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one.")
(defvar w3-current-links nil "An assoc list of <link> tags for this doc.")
(defvar w3-current-metainfo nil "An assoc list of <meta> tags for this doc.")
(defvar w3-current-source nil "Source of current document.")
(defvar w3-current-parse nil "Parsed version of current document.")
(defconst w3-default-continuation '(url-uncompress url-clean-text) 
  "Default action to start with - cleans text and uncompresses if necessary.")
(defvar w3-find-this-link nil "Link to go to within a document.")
(defvar w3-hidden-forms nil "List of hidden form areas and their info.")
(defvar w3-hotlist nil "Default hotlist.")
(defvar w3-icon-path-cache nil "Cache of where we found icons for entities.")
(defvar w3-last-buffer nil "The last W3 buffer visited.")
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
    w3-current-parse
    w3-current-isindex
    w3-current-last-buffer
    w3-current-links
    w3-current-metainfo
    w3-current-source
    w3-delayed-images
    w3-hidden-forms
    w3-current-stylesheet
    w3-form-labels
    w3-id-positions
    w3-imagemaps
    )
  "A list of variables that should be preserved when entering w3-mode.")

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

(make-variable-buffer-local 'w3-base-alist)
(make-variable-buffer-local 'w3-last-tag)
(make-variable-buffer-local 'w3-last-fill-pos)
(make-variable-buffer-local 'w3-active-faces)
(make-variable-buffer-local 'w3-netscape-emulation-minor-mode)
(make-variable-buffer-local 'w3-lynx-emulation-minor-mode)
(make-variable-buffer-local 'w3-last-search-item)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Keymap definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mode-map (make-keymap) "Keymap to use in w3-mode.")
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

(define-key w3-mode-map "HF"       'w3-history-forward)
(define-key w3-mode-map "HB"       'w3-history-backward)
(define-key w3-mode-map "Hv"       'w3-show-history-list)

(define-key w3-mode-map " "	   'w3-scroll-up)
(define-key w3-mode-map "<"        'beginning-of-buffer)
(define-key w3-mode-map ">"        'end-of-buffer)
(define-key w3-mode-map "?"        'w3-help)
(define-key w3-mode-map "B"        'w3-history-backward)
(define-key w3-mode-map "F"        'w3-history-forward)
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
(define-key w3-mode-map "\M-m"	   'w3-mail-current-document)
(define-key w3-mode-map "\M-s"	   'w3-search)
(define-key w3-mode-map "\M-\r"    'w3-follow-inlined-image)
(define-key w3-mode-map "\r"       'w3-widget-button-press)
(define-key w3-mode-map "\n"       'w3-widget-button-press)
(define-key w3-mode-map "b"	   'w3-widget-backward)
(define-key w3-mode-map "c"        'w3-mail-document-author)
(define-key w3-mode-map "f"	   'w3-widget-forward)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "i"        'w3-document-information)
(define-key w3-mode-map "k"        'w3-save-url)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'w3-widget-forward)
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
(define-key w3-mode-map [tab]         'w3-widget-forward)
(define-key w3-mode-map "\M-\t"       'w3-widget-backward)
(define-key w3-mode-map [backtab]     'w3-widget-backward)
(define-key w3-mode-map [(shift tab)] 'w3-widget-backward)
(define-key w3-mode-map [(meta tab)]  'w3-widget-backward)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyword definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-keyword)
(provide 'w3-vars)
