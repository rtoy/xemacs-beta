;;; face-tests.el --- test text display (faces, fonts)   -*- coding: utf-8 -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2013
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

;; Test text display (faces, fonts)

;; Test fontconfig

(let* ((test-name-parts
	'("Bitstream Vera Sans Mono-16"
	  "familylang=en"
	  "style=Roman"
	  "stylelang=en"
	  "fullname=Bitstream Vera Sans Mono"
	  "fullnamelang=en"
	  "slant=0"
	  "weight=80"
	  "width=100"
	  "pixelsize=21.3174"
	  "spacing=100"
	  "foundry=bitstream"
	  "antialias=True"
	  "hintstyle=3"
	  "hinting=True"
	  "verticallayout=False"
	  "autohint=False"
	  "globaladvance=True"
	  "file=/usr/X11/lib/X11/fonts/TTF/VeraMono.ttf"
	  "index=0"
	  "outline=True"
	  "scalable=True"
	  "dpi=95.9282"
	  "rgba=0"
	  "scale=1"
	  "minspace=False"
	  "charset=  |>^1!|>^1!P0oWQ |>^1!|>^1!|>^1!!!!%#gfN8.!!B7%ggR6OF3y?4!!K?&   !!!)$      9;*f! !!!.%     !!!)$!!!!# !!#0GM>RAd#y#fx   !!!W5  !!#3H !!!!&      !!#6I<UKaX!!!?+!!!%#!!!!X    !!#AL      !!!1& !!+u{!!!!)       "
	  "lang=aa|ay|bi|br|ch|co|da|de|en|es|et|eu|fi|fj|fo|fr|fur|fy|gd|gl|gv|ho|ia|id|ie|io|is|it|lb|mg|nb|nds|nl|nn|no|nr|nso|oc|om|pt|rm|sma|smj|so|sq|ss|st|sv|sw|tl|tn|tr|ts|uz|vo|vot|wa|xh|yap|zu|an|crh|fil|ht|jv|kj|ku-tr|kwm|li|ms|ng|pap-an|pap-aw|rn|rw|sc|sg|sn|su|za"
	  "fontversion=131072"
	  "fontformat=TrueType"
	  "embolden=False"
	  "embeddedbitmap=True"
	  "decorative=False"
	  "lcdfilter=1"
	  "namelang=en"
	  "prgname=xemacs"
	  "hash=sha256\\:da4281dc7db17a3dfce64a62ced92875c5895340055ec8ba24a3914eb97b349d"
	  "postscriptname=BitstreamVeraSansMono-Roman"))
	(test-name-degenerate "")
	(test-name-trivial (nth 0 test-name-parts))
	(test-name-short
	 (concat (nth 0 test-name-parts) ":" (nth 26 test-name-parts)))
	(test-name-long	(mapconcat #'identity
				   (append (subseq test-name-parts 0 26)
					   (subseq test-name-parts 27))
				   ":"))
	(test-name-full (mapconcat #'identity test-name-parts ":"))
	)
  (labels ((try (fontname)
	     (fc-name-unparse (fc-name-parse fontname)))
	   (try-harder (fontname)
	     (fc-name-unparse (fc-name-parse-harder fontname))))
    (Assert (string= test-name-degenerate (try test-name-degenerate)))
    (Assert (string= test-name-degenerate (try-harder test-name-degenerate)))
    (Assert (string= test-name-trivial (try test-name-trivial)))
    (Assert (string= test-name-trivial (try-harder test-name-trivial)))
    ;; Note when the `try' form fails, the `try-harder' form returns a
    ;; shorter name.
    (Check-Error 'invalid-argument
		 (string= test-name-short (try test-name-short)))
    (Assert (string= test-name-trivial (try-harder test-name-short)))
    (Assert (string= test-name-long (try test-name-long)))
    (Assert (string= test-name-long (try-harder test-name-long)))
    ;; Note when the `try' form fails, the `try-harder' form returns a
    ;; shorter name.
    (Check-Error 'invalid-argument
		 (string= test-name-full (try test-name-full)))
    (Assert (string= test-name-long (try-harder test-name-full)))
    ) ; labels
  ) ; let

;;; end face-tests.el
