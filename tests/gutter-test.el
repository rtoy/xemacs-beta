;;; Copyright (C) 1998 Andy Piper

;;; This file is part of XEmacs.

;;; XEmacs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your
;;; option) any later version.

;;; XEmacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; see the file COPYING.  If not, write to the Free
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.

(setq str "Hello\nAgain")
(setq str-ext (make-extent 0 5 str))
(set-extent-begin-glyph 
 str-ext
 (make-glyph [xpm :file "../etc/xemacs-icon.xpm"]))
(set-extent-property str-ext 'mouse-face 'highlight)

(setq str2 "Hello\n")
(setq str2-ext (make-extent 0 1 str2))
(set-extent-begin-glyph
 str2-ext
 (make-glyph 
  [button :width 5 :height 1
	  :face modeline-mousable
	  :callback (set-specifier bottom-gutter-visible-p '(str2))
	  :descriptor "ok" :selected t]))

(set-specifier bottom-gutter-height 'autodetect)
(set-specifier bottom-gutter-border-width 2)

(set-gutter-element 
 bottom-gutter 'str
 (make-glyph 
  [layout :orientation vertical :margin-width 4
	  :vertically-justify center :horizontally-justify left
	  :items ([string :data "Fontifying glyphs.c..."]
		  [layout :orientation horizontal
			  :items 
			  ([progress-gauge :value 0 :pixel-height 24
					   :pixel-width 250 :descriptor 
					   "Progress"]
			   [button :pixel-height 24
				   :descriptor " Stop " 
				   :callback (quote quit)])])]))

(set-gutter-element-visible-p bottom-gutter-visible-p 'str t)
(set-gutter-element left-gutter 'str2 str2)
(set-gutter-element-visible-p left-gutter-visible-p 'str2 t)

