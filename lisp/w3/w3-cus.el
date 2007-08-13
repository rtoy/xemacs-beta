;;; w3-cus.el --- Customization support for Emacs-W3
;; Author: wmperry
;; Created: 1997/03/14 06:51:36
;; Version: 1.3
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
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

;;; File related variables
(defcustom w3-configuration-directory "~/.w3/"
  "*Directory where Emacs-w3 can find its configuration files"
  :group 'w3-files
  :type 'directory)  

(defcustom w3-default-configuration-file nil
  "*Where per-user customizations of w3 are kept."
  :group 'w3-files
  :type 'file)

(defcustom w3-default-homepage nil
  "*The url to open at startup.  It can be any valid URL.
This will default to the environment variable WWW_HOME if you do not
set it in your .emacs file. If WWW_HOME is undefined, then it will
default to  the hypertext documentation for W3 at Indiana University."
  :group 'w3-files
  :type 'string)

(defcustom w3-default-stylesheet nil
  "*The filename of the users default stylesheet."
  :group 'w3-files
  :type 'file)

(defcustom w3-hotlist-file nil
  "*Hotlist filename.
This should be the name of a file that is stored in either
NCSA's Mosaic/X or Netscape/X format.  It is used to keep a listing
of commonly accessed URL's without having to go through 20 levels of
menus to get to them."
  :group 'w3-files
  :type 'file)

(defcustom w3-documentation-root "http://www.cs.indiana.edu/elisp/w3/docs/"
  "*Where the w3 documentation lives.  This MUST end in a slash."
  :group 'w3-files
  :type 'string)

(defcustom w3-temporary-directory (or (getenv "TMPDIR") "/tmp")
  "*Where temporary files go."
  :group 'w3-files
  :type 'directory)

;;; Display related variables
(defcustom w3-display-frames nil
  "*Fetch frames - not optimal."
  :group 'w3-display
  :type 'boolean)

(defcustom w3-do-incremental-display nil
  "*Whether to do incremental display of pages or not."
  :group 'w3-display
  :type 'boolean)

(defcustom w3-echo-link '(title url text name)
  "*Whether to display the URL of a link when tabbing through links.
Value is a list of one or more of the following symbols:

  url    == url of the target
  text   == text of the link
  title  == title attribute of the link
  name   == name or id attribute of the link

If none of the information is available, nothing will be shown for the link
in menus, etc."
  :group 'w3-display
  :type '(set (const :tag "URL" :value url)
	      (const :tag "Link Text" :value text)
	      (const :tag "Title of the link as defined in the HTML tag"
		     :value title)
	      (const :tag "Name of the link as defined in the HTML tag"
		     :value name)))

(defcustom w3-horizontal-rule-char ?-
  "*The character to use to create a horizontal rule.
Must be the character's code, not a string.  This character is
replicated across the screen to create a division."
  :group 'w3-display
  :type 'character)

(defcustom w3-defined-link-types
  ;; This is the HTML3.0 list (downcased) plus "made".
  '("previous" "next" "up" "down" "home" "toc" "index" "glossary"
    "copyright" "bookmark" "help" "made")
  "A list of the (lower-case) names which have special significance
as the values of REL or REV attributes of <link> elements.  They will
be presented on the toolbar or the links menu, for instance."
  :group 'w3-display
  :type '(repeat string))

;;; Parsing related variables
(defcustom w3-debug-html nil
  "*Whether to gripe about bad HTML or not."
  :group 'w3-parsing
  :type '(choice (const :tag "HTML Errors" :value t)
		 (const :tag "Errors and stylistic issues" :value style)
		 (const :tag "None" :value nil)))

(defcustom w3-debug-buffer "*HTML Debug*"
  "*Name of buffer to store debugging information in."
  :group 'w3-parsing
  :type 'string)

;;; Image related variables
(defcustom w3-auto-image-alt t
  "*Whether emacs-w3 should create an alt attribute for an image that
is missing it.
If nil, emacs-w3 will not automatically create an ALT attribute.
If t, the alt attribute will be [IMAGE(nameofimage)]
If a string, it should be a string suitable for running through format,
   with only one %s, which will be replaced with just the filename of the
   graphic that is not loaded."
  :group 'w3-images
  :type '(choice (const :tag "None" :value nil)
		 (const :tag "Default" :value t)
		 string))

(defcustom w3-icon-directory "http://cs.indiana.edu/elisp/w3/icons/"
  "*Where to find standard icons.  Must end in a /!"
  :group 'w3-images
  :type 'string)

(defcustom w3-icon-format 'gif
  "*Image format the default icons are expected to be in.
This is a symbol, string or nil, specifing what file extension to use.
If nil, then no file extension is used."
  :group 'w3-images
  :type '(choice (const :tag "GIF Image" :value gif)
		 (const :tag "XPM Image" :value xpm)
		 (const :tag "XBM Image" :value xbm)
		 (const :tag "Let the server decide" :value nil)
		 (string :tag "Other")))

(defcustom w3-delay-image-loads nil
  "*Whether to delay image loading, or automatically retrieve them."
  :group 'w3-images
  :type 'boolean)

(defcustom w3-image-mappings
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
    )
  "*How to map MIME types to image types for the `image' package.
Each entry is a cons cell of MIME types and image-type symbols."
  :group 'w3-images
  :type '(repeat cons))

;;; Printing variables
(defcustom w3-latex-docstyle "{article}"
  "*The documentstyle to use when printing or mailing files as LaTeX.
Good defaults are: {article}, [psfig,twocolumn]{article}, etc."
  :group 'w3-printing
  :type 'string)

(defcustom w3-latex-print-links nil
  "*If non-nil, prints the URLs of hypertext links as endnotes at the end of
the document.  If `footnote', prints the URL's as footnotes on a page."
  :group 'w3-printing
  :type '(choice (const :tag "As endnotes" :value t)
		 (const :tag "As footnotes" :value footnote)
		 (const :tag "Do not print" :value nil)))

(defcustom w3-latex-use-latex2e nil
  "*If non-nil, configures LaTeX parser to use LaTeX2e syntax.  A `nil' 
value indicates that LaTeX 2.0.9 compatibility will be used instead."
  :group 'w3-printing
  :type 'boolean)

(defcustom w3-latex-packages nil
  "*List of LaTeX packages to include when converting HTML to LaTeX.
Currently this is only used if `w3-latex-use-latex2e' is non-nil."
  :group 'w3-printing
  :type '(repeat string))

(defcustom w3-latex-use-maketitle nil
  "*Non-nil makes the LaTeX parser use real LaTeX title pages."
  :group 'w3-printing
  :type 'boolean)

;;; Menus
(defcustom w3-max-menu-length 35
  "*The maximum length of a pulldown menu before it will be split into
smaller chunks, with the first part as a submenu, followed by the rest
of the menu."
  :group 'w3-menus
  :type 'integer)

(defcustom w3-max-menu-width 40
  "*The maximum width of a pulldown menu choice."
  :group 'w3-menus
  :type 'integer)

;;; Advanced stuff
(defcustom w3-modeline-format
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
  "*The modeline format string when in w3 mode"
  :group 'w3-advanced
  :type 'list)

(defcustom w3-netscape-compatible-comments t
  "*Whether to honor netscape-style <! > comments.
Ye gods I wish I could turn this off by default."
  :group 'w3-parsing
  :type 'boolean)

(defcustom w3-notify 'semibully
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

Any other value of `w3-notify' is equivalent to `meek'."
  :group 'w3-display
  :type '(choice (const :tag "Display in a new frame"
			:value newframe)
		 (const :tag "Display in the current window, select buffer, and kill other windows"
			:value bully)
		 (const :tag "Display in the current window, select buffer"
			:value semibully)
		 (const :tag "Display in another window, select buffer"
			:value aggressive)
		 (const :tag "Display in another window, but do not select it"
			:value friendly)
		 (const :tag "Do not display page, but show a message and beep"
			:value polite)
		 (const :tag "Do not display page, but show a message (no beep)"
			:value quiet)
		 (const :tag "Do not indicate that the page has been retrieved"
			:value meek)))

(defcustom w3-popup-menu-on-mouse-3 t
  "*Non-nil value means W3 should provide context-sensitive menus on mouse-3.
A nil value means W3 should not change the binding of mouse-3."
  :group 'w3-display
  :type 'boolean)

(defcustom w3-print-command "lpr -h -d"
  "*Print command for dvi files.
This is usually 'lpr -h -d' to send it to a postscript printer, but you can set
it up so that it is any command that takes a dvi file as its last argument."
  :group 'w3-printing
  :type 'string)

(defcustom w3-reuse-buffers 'yes
  "What to do when following a link will re-fetch a document that has
already been fetched into a W3 buffer.  Possible values are: nil,
'yes, and 'no.  Nil means ask the user if we should reuse the buffer.
 A value of 'yes means assume the user wants us to reuse the buffer.
A value of 'no means assume the user wants us to re-fetch the document.

This will also accept:
'no	==> always reload
'yes	==> always reuse
'ask	==> always ask"
  :group 'w3-display
  :type '(choice (const :tag "Always reload" :value no)
		 (const :tag "Always reuse" :value yes)
		 (const :tag "Always ask" :value ask)))

(defcustom w3-right-margin 2
  "*Default right margin for Emacs-W3 buffers.
This amount is subtracted from (window-width) for each new WWW buffer
and used as the new fill-column."
  :group 'w3-display
  :type 'integer)

(defcustom w3-maximum-line-length nil
  "*Maximum length of a line.
If nil, then lines can extend all the way to the window margin."
  :group 'w3-display
  :type 'integer)

(defcustom w3-track-mouse t
  "*Whether to track the mouse and message the url under the mouse."
  :group 'w3-display
  :type 'boolean)

(defcustom w3-honor-stylesheets t
  "*Whether to let a document specify a CSS stylesheet."
  :group 'w3-display
  :type 'boolean)

(defcustom w3-user-colors-take-precedence nil
  "*Whether to let a document define certain colors about itself.
Like foreground and background colors and pixmaps, color of links and
visited links, etc."
  :group 'w3-display
  :type 'boolean)

;;; Hook Variables
(defcustom w3-load-hook nil
  "*Hooks to be run after loading w3."
  :group 'w3-hooks
  :type 'hook)

(defcustom w3-mode-hook nil
  "*Hooks to be run after entering w3-mode."
  :group 'w3-hooks
  :type 'hook)

(defcustom w3-source-file-hook nil
  "*Hooks to be run after getting document source."
  :group 'w3-hooks
  :type 'hook)

(provide 'w3-cus)
