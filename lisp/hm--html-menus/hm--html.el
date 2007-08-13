;;; $Id: hm--html.el,v 1.8 1997/06/06 00:57:04 steve Exp $
;;;
;;; Copyright (C) 1993 - 1997  Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	Defines functions for the file hm--html-menu.el.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories.
;;;

(defun hm--html-set-marker-at-position (&optional position)
  "Creates a new marker and set the marker at the POSITION.
If POSITION is nil, then the marker is set at the current point.
The return value is the marker."
  (let ((marker (make-marker)))
    (if position
	(set-marker marker position)
      (set-marker marker (point)))))

;;; Functions for adding html commands which consists of a start and a
;;; end tag and some text between them. (Basicfunctions)

(defun hm--html-add-tags (function-insert-start-tag 
			  start-tag
			  &optional
			  function-insert-end-tag
			  end-tag
			  function-insert-middle-start-tag
			  middle-start-tag
			  function-insert-middle-end-tag
			  middle-end-tag)
  "Adds the start and the end html tag at point.
The first parameter specifies the funtion which insert the start tag
and the third parameter specifies the function which insert the end tag.
The second parameter is the string for the start tag and the fourth parameter
is the string for the end tag. The third and fourth parameters are optional.
The fifth parameter is optional. If it exists, it specifies a function which
inserts the sixth parameter (the middle-start-tag) between the start and the
end tag."
  (eval (list function-insert-start-tag start-tag))
  (if function-insert-middle-start-tag
      (eval (list function-insert-middle-start-tag middle-start-tag)))
  (let ((position (hm--html-set-marker-at-position (point))))
    (if function-insert-middle-end-tag
      (eval (list function-insert-middle-end-tag middle-end-tag)))
    (if function-insert-end-tag
	(eval (list function-insert-end-tag end-tag)))
    (goto-char position)))


(defun hm--html-add-tags-to-region (function-insert-start-tag 
				    start-tag
				    function-insert-end-tag
				    end-tag
				    &optional
				    function-insert-middle-tag
				    middle-tag)
  "Adds the start and the end html tag to the active region.
The first parameter specifies the funtion which insert the start tag
and the third parameter specifies the function which insert the end tag.
The second parameter is the string for the start tag and the fourth parameter
is the string for the end tag.
The fifth parameter is optional. If it exists, it specifies a function which
inserts the sixth parameter (the middle-tag) between the start and the end
tag."
  (save-window-excursion
    (let ((start (hm--html-set-marker-at-position (region-beginning)))
	  (end (region-end)))
      (goto-char end)
      (eval (list function-insert-end-tag end-tag))
      (goto-char start)
      (eval (list function-insert-start-tag start-tag))
      (if function-insert-middle-tag
	  (eval (list function-insert-middle-tag middle-tag)))
      )))


(defun hm--html-insert-start-tag (tag)
  "Inserts the HTML start tag 'tag' without a Newline.
The parameter must be a string (i.e. \"<B>\")"
  (let ((start (point)))
    (insert tag)
    (hm--html-indent-region start (point))))


(defun hm--html-insert-end-tag (tag)
  "Inserts the HTML end tag 'tag' without a Newline.
The parameter must be a string (i.e. \"</B>\")"
  (let ((start (point)))
    (insert tag)
    (hm--html-indent-region start (point))))


(defun hm--html-insert-start-tag-with-newline (tag)
  "Inserts the HTML start tag 'tag' with a Newline.
The parameter must be a string (i.e. \"<PRE>\")"
  (let ((start (point)))
    (insert tag)
    (hm--html-indent-region start (point))
    )
  (insert "\n"))


(defun hm--html-insert-end-tag-with-newline (tag)
  "Inserts the HTML end tag 'tag' with a Newline.
The parameter must be a string (i.e. \"</PRE>\")"
  (insert "\n")
  (let ((start (point)))
    (insert tag)
    (hm--html-indent-region start (point))))



;;; Functions which add simple tags of the form <tag>

(defun hm--html-add-list-or-menu-item-separator ()
  "Adds a list or menu item.  Assume we're at the end of the last item."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-end-tag-with-newline "<LI> "))

(defun hm--html-add-list-or-menu-item ()
  "Adds the tags for a menu item at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-end-tag-with-newline "<LI> "
		     'hm--html-insert-end-tag " </LI>"))

(defun hm--html-add-list-or-menu-item-to-region ()
  "Adds the tags for a menu item to the region in the current buffer."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag "<LI> "
			       'hm--html-insert-end-tag " </LI>"))

(defun hm--html-add-basefont (size)
  "Adds the HTML tag for a basefont."
  (interactive (list (hm--html-read-font-size t)))
  (hm--html-add-tags 'hm--html-insert-start-tag 
		     (concat "<BASEFONT SIZE=" size ">")))

(defun hm--html-add-line-break ()
  "Adds the HTML tag for a line break."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag "<BR>"))


(defun hm--html-add-horizontal-rule ()
  "Adds the HTML tag for a horizontal rule (line)."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag "<HR>"))


(defun hm--html-add-paragraph ()
  "Adds the HTML tags for a paragraph at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<P>"
		     'hm--html-insert-end-tag-with-newline
		     "</P>"))


(defun hm--html-add-paragraph-to-region ()
  "Adds the HTML tags for a paragraph to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<P>"
			       'hm--html-insert-end-tag-with-newline
			       "</P>"))


(defun hm--html-add-paragraph-separator ()
  "Adds the tag for a paragraph seperator."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag "<P>"))
  
(defun hm--html-add-doctype ()
  "Adds the tag with the doctype."
  (interactive)
  (goto-char (point-min))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<!DOCTYPE HTML PUBLIC \""
			     hm--html-html-doctype-version
			     "\">"))
  (newline))

(defun hm--html-search-place-for-element-in-head (end-point)
  "Searches the point for inserting an element between the head tags."
  (let ((point (point)))
    (if (and end-point (< (point) end-point))
	(point)
      (goto-char (point-min))
      (if (re-search-forward
	   (concat ;"\\(<title\\)\\|\\(<head\\)\\|\\(<html\\)\\|"
	    "\\(<title\\)\\|"
	    "\\(<isindex\\)\\|\\(<base\\)\\|\\(<link\\)\\|"
	    "\\(<meta\\)")
	   end-point
	   t)
	  (beginning-of-line)
	end-point))))

(defun hm--html-add-isindex (prompt)
  "Inserts the isindex tag. PROMPT is the value of the prompt attribute."
  (interactive "sPrompt: ")
  (save-excursion
    (let ((point (point))
	  (case-fold-search t)
	  (head-end-point))
      (goto-char (point-min))
      (setq head-end-point (when (re-search-forward
				  "\\(</head\\)\\|\\(<body\\)\\|\\(</html\\)")
			     (beginning-of-line)
			     (point)))
      (goto-char (point-min))
      (cond ((re-search-forward "<isindex[^>]*>" head-end-point t)
	     (delete-region (match-beginning 0) (match-end 0)))
	    (t (goto-char point)
	       (hm--html-search-place-for-element-in-head head-end-point)))
      (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
			 (concat "<ISINDEX "
				 (if (and prompt
					  (not (string= prompt "")))
				     (concat " PROMPT=\"" prompt "\">")
				   ">"))))))
				   
(defun hm--html-add-base (href)
  "Inserts the base tag. HREF is the value of the href attribute."
  (interactive (list (hm--html-read-url "URL of this document: "
					nil
					nil
					t
					nil)))
  (save-excursion
    (let ((point (point))
	  (case-fold-search t)
	  (head-end-point))
      (goto-char (point-min))
      (setq head-end-point (when (re-search-forward
				  "\\(</head\\)\\|\\(<body\\)\\|\\(</html\\)")
			     (beginning-of-line)
			     (point)))
      (goto-char (point-min))
      (cond ((re-search-forward "<base[^>]*>" head-end-point t)
	     (delete-region (match-beginning 0) (match-end 0)))
	    (t (goto-char point)
	       (hm--html-search-place-for-element-in-head head-end-point)))
      (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
			 (concat "<BASE "
				 (if (and href
					  (not (string= href "")))
				     (concat " HREF=\"" href "\">")
				   ">"))))))

(defun hm--html-add-meta (name content &optional name-instead-of-http-equiv)
  "Inserts the meta tag."
  (interactive (list (completing-read "Name: " hm--html-meta-name-alist)
		     (read-string "Content: ")))
  (save-excursion
    (let ((point (point))
	  (case-fold-search t)
	  (head-end-point))
      (goto-char (point-min))
      (setq head-end-point (when (re-search-forward
				  "\\(</head\\)\\|\\(<body\\)\\|\\(</html\\)")
			     (beginning-of-line)
			     (point)))
      (goto-char point)
      (hm--html-search-place-for-element-in-head head-end-point)
      (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
			 (concat "<META "
				 (if name-instead-of-http-equiv
				     "NAME=\""
				   "HTTP-EQUIV=\"")
				 name
				 "\" CONTENT=\""
				 content
				 "\">")))))


;;; Functions which include something in HTML- documents

(defvar hm--html-url-history-list nil 
  "History list for the function 'hm--html-read-url'")


(defun hm--html-read-url-predicate (table-element-list usagesymbol)
  "Predicatefunction for hm--html-read-url."
  (hm--html-read-url-predicate-1 (cdr table-element-list) usagesymbol))


(defun hm--html-read-url-predicate-1 (table-element-list usagesymbol)
  "Internal function of hm--html-read-url-predicate."
  (cond ((not table-element-list) nil)
	((eq (car table-element-list) usagesymbol))
	(t (hm--html-read-url-predicate-1 (cdr table-element-list) 
					  usagesymbol))))


(defun hm--html-read-url (prompt &optional 
				 table 
				 predicate 
				 require-match 
				 initial-contents)
  "Function prompts for a URL string.
TABLE is an alist whose elements' cars are URL's.
PREDICATE limits completion to a subset of TABLE.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
the input is (or completes to) an element of TABLE.
INITIAL-CONTENTS is a string to insert in the minibuffer before reading.
If INITIAL-CONTENTS is nil, the car of the 'hm--html-url-history-list'
is used instead."
  (if table
      (completing-read prompt 
		       table 
		       predicate 
		       require-match 
		       initial-contents
		       hm--html-url-history-list)
    (read-string prompt
		 (if initial-contents
		     initial-contents
		   (car hm--html-url-history-list))
		 hm--html-url-history-list)))


(defun hm--html-read-altenate (url)
  "Function reads the value for the \"ALT\"- attribute in IMG tags.
URL will be used as the default URL for the external viewer."
  (let ((alttype
	 (string-to-int
	  (completing-read 
	   "0: No ALT atribute, 1: ALT=\"\", 2: ALT=Text: "
	   '(("0") ("1") ("2"))
	   nil
	   t
	   "2"))))
    (cond ((= alttype 0) nil)
	  ((= alttype 1) "")
	  ((= alttype 2) (read-string
			  "Text for the ALT attribute: "
			  (substring (file-name-nondirectory url)
				     0
				     (string-match
				      "\\."
				      (file-name-nondirectory url)))))
	  )))

(defun hm--html-read-alignment (prompt)
  "Read the value for the align attribute."
  (upcase (completing-read prompt
			   '(("left") ("right") ("top") ("bottom") ("middle"))
			   nil
			   t
			   "left")))

(defvar hm--html-shape-history nil
  "History variable for reading the shape of an image map.")

(defun hm--html-read-shape ()
  "Reads the shap for an area element."
  (upcase(completing-read "The shape of the area: "
			  '(("rect") ("circle") ("poly"))
			  nil
			  t
			  (or (car hm--html-shape-history) "rect")
			  'hm--html-shape-history)))

(defun hm--html-read-rect-coords ()
  "Reads rectangle coordinates for the area element."
  (concat (read-string "Left x position of the rectangle: ") ", "
	  (read-string "Top y position of the rectangle: ") ", "
	  (read-string "Right x position of the rectangle: ") ", "
	  (read-string "Bottom y position of the rectangle: ")))

(defun hm--html-read-circle-coords ()
  "Reads circle coordinates for the area element."
  (concat (read-string "x position of the center of the circle: ") ", "
	  (read-string "y position of the center of the circle: ") ", "
	  (read-string "Radius: ")))

(defun hm--html-read-one-poly-coordinate (&optional empty-string-prompt)
  "Reads one poly coordinate pair."
  (let* ((x (read-string (concat "x coordinate"
				  (or empty-string-prompt "")
				  ": ")))
	 (y (unless (string= "" x)
	      (read-string "y coordinate: "))))
    (if (string= "" x)
	""
      (concat x ", " y))))

(defun hm--html-read-more-poly-coordinates ()
  "Reads poly coordinates until an empty string is given."
  (let ((coord (hm--html-read-one-poly-coordinate
		" (Empty string for no further coords!)")))
    (cond ((string= "" coord) "")
	  (t (concat ", " coord (hm--html-read-more-poly-coordinates))))))

(defun hm--html-read-poly-coords ()
  "Reads poly coordinates for the area element."
  (concat (hm--html-read-one-poly-coordinate) ", "
	  (hm--html-read-one-poly-coordinate) ", "
	  (hm--html-read-one-poly-coordinate)
	  (hm--html-read-more-poly-coordinates)))

(defun hm--html-add-area (href alt shape coords)
  "Adds the tags for an area at the current point."
  (interactive (let* ((href (hm--html-read-url "Url for the image area: "))
		      (alt (hm--html-read-altenate href))
		      (shape (hm--html-read-shape))
		      (coords (cond ((string= shape "RECT")
				     (hm--html-read-rect-coords))
				    ((string= shape "CIRCLE")
				     (hm--html-read-circle-coords))
				    ((string= shape "POLY")
				     (hm--html-read-poly-coords))
				    (t (error "No function to read \""
					      shape
					      "\" coordinates!")))))
		 (list href alt shape coords)))
  (hm--html-add-tags 'hm--html-insert-end-tag-with-newline
		     (concat "<AREA"
			     " HREF=\"" href "\""
			     (if alt
				 (concat "\nALT=\"" alt "\"")
			       "")
			     "\nSHAPE=" shape
			     "\nCOORDS=\"" coords "\""
			     ">")))


(when (adapt-emacs19p)
      (defvar :ask ':ask))

(defvar hm--html-use-image-as-map ':ask
  "Internal variable of `hm--html-add-image'.
nil => insert the image element without an usemap attribute.
t => insert the image element with an usemap attribute.
:ask => ask, if the image element should have an usemap attribute.")

(defun hm--html-add-image (href alt alignment mapname)
  "Add an image."
  (interactive (let* ((href (hm--html-read-url "Image URL: "))
		      (alt (hm--html-read-altenate href))
		      (alignment (hm--html-read-alignment
				  "Alignment of the image: "))
		      (use-as-map (if (eq hm--html-use-image-as-map ':ask)
				      (y-or-n-p
				       "Use the image as a map with links? ")
				    hm--html-use-image-as-map))
		      (mapname (and use-as-map (hm--html-read-mapname))))
		 (list href alt alignment mapname)))
  (hm--html-add-tags 
   'hm--html-insert-start-tag
   (concat "<IMG ALIGN=" alignment
	   "\nHREF=\"" href "\""
	   (if alt
	       (concat "\nALT=\"" alt "\"")
	     "")
	   (if mapname
	       (concat "\nUSEMAP=\"#" mapname "\"")
	     "")
	   ">")))
	   

(defun hm--html-add-image-bottom (href alt)
  "Add an image, bottom aligned."
  (interactive (let ((url (hm--html-read-url "Image URL: ")))
		 (list url (hm--html-read-altenate url))))
  (hm--html-add-tags 
   'hm--html-insert-start-tag 
   (concat "<IMG ALIGN=BOTTOM SRC=\""
	   href
	   (when alt
	     (concat "\" ALT=\"" alt))
	   "\">")))


(defun hm--html-add-image-middle (href alt)
  "Add an image, middle aligned."
  (interactive (let ((url (hm--html-read-url "Image URL: ")))
		 (list url (hm--html-read-altenate url))))
  (hm--html-add-tags 
   'hm--html-insert-start-tag 
   (concat "<IMG ALIGN=MIDDLE SRC=\""
	   href
	   (when alt
	     (concat "\" ALT=\"" alt))
	   "\">")))


(defun hm--html-add-image-top (href alt)
  "Add an image, top aligned."
  (interactive (let ((url (hm--html-read-url "Image URL: ")))
		 (list url (hm--html-read-altenate url))))
    (hm--html-add-tags 
     'hm--html-insert-start-tag 
     (concat "<IMG ALIGN=TOP SRC=\""
	     href
	     (when alt
	       (concat "\" ALT=\"" alt))
	     "\">")))


(defun hm--html-add-applet (name code width height)
  "Add an applet."
  (interactive (let ((name (read-string "Applet Name: " "applet"))
		     (code (read-file-name "Applet Class File: "))
		     (width (read-number "Width (i.e.: 100): " t))
		     (height (read-number "Height (i.e.: 100): " t)))
		 (list name code width height)))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<APPLET "
			     (if (string= name "")
				 ""
			       (concat "NAME=\"" name "\"\n"))
			     "CODE=\""
			     code
			     "\"\n"
			     "WIDTH=\""
			     width
			     "\"\n"
			     "HEIGHT=\""
			     height
			     "\">")
		     'hm--html-insert-start-tag-with-newline
		     "</APPLET>"))

(defun hm--html-add-applet-parameter (name value)
  "Adds the tag for an applet parameter at the current point.
This tag must be added between <APPLET> and </APPLET>."
  (interactive "sParameter Name: \nsParameter Value: ")
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<PARAM "
			     "NAME=\""
			     name
			     "\" VALUE=\""
			     value
			     "\">")))
		     


;;; Functions, which adds tags of the form <starttag> ... </endtag>

(defun hm--html-add-big ()
  "Adds the HTML tags for Big at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<BIG>"
		     'hm--html-insert-end-tag
		     "</BIG>"))


(defun hm--html-add-big-to-region ()
  "Adds the HTML tags for Big to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<BIG>"
			       'hm--html-insert-end-tag
			       "</BIG>"))


(defun hm--html-add-small ()
  "Adds the HTML tags for Small at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<SMALL>"
		     'hm--html-insert-end-tag
		     "</SMALL>"))


(defun hm--html-add-small-to-region ()
  "Adds the HTML tags for Small to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<SMALL>"
			       'hm--html-insert-end-tag
			       "</SMALL>"))


(defun hm--html-add-bold ()
  "Adds the HTML tags for Bold at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<B>"
		     'hm--html-insert-end-tag
		     "</B>"))


(defun hm--html-add-bold-to-region ()
  "Adds the HTML tags for Bold to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<B>"
			       'hm--html-insert-end-tag
			       "</B>"))


(defun hm--html-add-italic ()
  "Adds the HTML tags for Italic at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<I>"
		     'hm--html-insert-end-tag
		     "</I>"))


(defun hm--html-add-italic-to-region ()
  "Adds the HTML tags for Italic to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<I>"
			       'hm--html-insert-end-tag
			       "</I>"))


(defun hm--html-add-underline ()
  "Adds the HTML tags for Underline at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<U>"
		     'hm--html-insert-end-tag
		     "</U>"))


(defun hm--html-add-underline-to-region ()
  "Adds the HTML tags for Underline to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<U>"
			       'hm--html-insert-end-tag
			       "</U>"))


(defun hm--html-add-definition ()
  "Adds the HTML tags for Definition at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<DFN>"
		     'hm--html-insert-end-tag
		     "</DFN>"))


(defun hm--html-add-definition-to-region ()
  "Adds the HTML tags for Definition to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<DFN>"
			       'hm--html-insert-end-tag
			       "</DFN>"))


(defun hm--html-add-code ()
  "Adds the HTML tags for Code at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<CODE>"
		     'hm--html-insert-end-tag
		     "</CODE>"))


(defun hm--html-add-code-to-region ()
  "Adds the HTML tags for Code to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<CODE>"
			       'hm--html-insert-end-tag
			       "</CODE>"))


(defun hm--html-add-citation ()
  "Adds the HTML tags for Citation."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<CITE>"
		     'hm--html-insert-end-tag
		     "</CITE>"))

(defun hm--html-add-citation-to-region ()
  "Adds the HTML tags for Citation to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<CITE>"
			       'hm--html-insert-end-tag
			       "</CITE>"))


(defun hm--html-add-emphasized ()
  "Adds the HTML tags for Emphasized."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<EM>"
		     'hm--html-insert-end-tag
		     "</EM>"))


(defun hm--html-add-emphasized-to-region ()
  "Adds the HTML tags for Emphasized to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<EM>"
			       'hm--html-insert-end-tag
			       "</EM>"))


(defun hm--html-add-fixed ()
  "Adds the HTML tags for Fixed."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<TT>"
		     'hm--html-insert-end-tag
		     "</TT>"))


(defun hm--html-add-fixed-to-region ()
  "Adds the HTML tags for Fixed to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<TT>"
			       'hm--html-insert-end-tag
			       "</TT>"))


(defun hm--html-add-keyboard ()
  "Adds the HTML tags for Keyboard."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<KBD>"
		     'hm--html-insert-end-tag
		     "</KBD>"))


(defun hm--html-add-keyboard-to-region ()
  "Adds the HTML tags for Keyboard to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<KBD>"
			       'hm--html-insert-end-tag
			       "</KBD>"))


(defun hm--html-add-sample ()
  "Adds the HTML tags for Sample."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<SAMP>"
		     'hm--html-insert-end-tag
		     "</SAMP>"))

(defun hm--html-add-sample-to-region ()
  "Adds the HTML tags for Sample to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<SAMP>"
			       'hm--html-insert-end-tag
			       "</SAMP>"))


(defun hm--html-add-strong ()
  "Adds the HTML tags for Strong."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<STRONG>"
		     'hm--html-insert-end-tag
		     "</STRONG>"))


(defun hm--html-add-strong-to-region ()
  "Adds the HTML tags for Strong to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<STRONG>"
			       'hm--html-insert-end-tag
			       "</STRONG>"))


(defun hm--html-add-variable ()
  "Adds the HTML tags for Variable."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<VAR>"
		     'hm--html-insert-end-tag
		     "</VAR>"))

(defun hm--html-add-variable-to-region ()
  "Adds the HTML tags for Variable to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<VAR>"
			       'hm--html-insert-end-tag
			       "</VAR>"))


(defun hm--html-add-comment ()
  "Adds the HTML tags for Comment at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<!-- "
		     'hm--html-insert-end-tag
		     " -->"))


(defun hm--html-add-comment-to-region ()
  "Adds the HTML tags for Comment to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<!-- "
			       'hm--html-insert-end-tag
			       " -->"))


(defun hm--html-add-document-division (alignment)
  "Adds the HTML tags for document division at the current point."
  (interactive (list (hm--html-read-alignment "Alignment of the division: ")))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<DIV ALIGN=\"" alignment "\">")
		     'hm--html-insert-end-tag-with-newline
		     "</DIV>"))


(defun hm--html-add-document-division-to-region (alignment)
  "Adds the HTML tags for document division to the region."
  (interactive (list (hm--html-read-alignment "Alignment of the division: ")))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       (concat "<DIV ALIGN=\"" alignment "\">")
			       'hm--html-insert-end-tag-with-newline
			       "</DIV>"))


(defun hm--html-add-preformatted ()
  "Adds the HTML tags for preformatted text at the current point."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<PRE>"
		     'hm--html-insert-end-tag-with-newline
		     "</PRE>"))

(define-obsolete-function-alias 'hm--html-add-preformated
  'hm--html-add-preformatted)

(defun hm--html-add-preformatted-to-region ()
  "Adds the HTML tags for preformatted text to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<PRE>"
			       'hm--html-insert-end-tag-with-newline
			       "</PRE>"))

(define-obsolete-function-alias 'hm--html-add-preformated-to-region
  'hm--html-add-preformatted-to-region)

(defun hm--html-add-blockquote ()
  "Adds the HTML tags for blockquote."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<BLOCKQUOTE>"
		     'hm--html-insert-end-tag-with-newline
		     "</BLOCKQUOTE>"))


(defun hm--html-add-blockquote-to-region ()
  "Adds the HTML tags for blockquote to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<BLOCKQUOTE>"
			       'hm--html-insert-end-tag-with-newline
			       "</BLOCKQUOTE>"))

(defun hm--html-add-script ()
  "Adds the HTML tags for script."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<SCRIPT>"
		     'hm--html-insert-end-tag-with-newline
		     "</SCRIPT>"))


(defun hm--html-add-script-to-region ()
  "Adds the HTML tags for script to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<SCRIPT>"
			       'hm--html-insert-end-tag-with-newline
			       "</SCRIPT>"))

(defun hm--html-add-style ()
  "Adds the HTML tags for style."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<STYLE>"
		     'hm--html-insert-end-tag-with-newline
		     "</STYLE>"))


(defun hm--html-add-style-to-region ()
  "Adds the HTML tags for style to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<STYLE>"
			       'hm--html-insert-end-tag-with-newline
			       "</STYLE>"))

(defun hm--html-add-strikethru ()
  "Adds the HTML tags for Strikethru at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<STRIKE>"
		     'hm--html-insert-end-tag
		     "</STRIKE>"))


(defun hm--html-add-strikethru-to-region ()
  "Adds the HTML tags for Strikethru to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<STRIKE>"
			       'hm--html-insert-end-tag
			       "</STRIKE>"))


(defun hm--html-add-superscript ()
  "Adds the HTML tags for Superscript at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<SUP>"
		     'hm--html-insert-end-tag
		     "</SUP>"))


(defun hm--html-add-superscript-to-region ()
  "Adds the HTML tags for Superscript to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<SUP>"
			       'hm--html-insert-end-tag
			       "</SUP>"))


(defun hm--html-add-subscript ()
  "Adds the HTML tags for Subscript at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<SUB>"
		     'hm--html-insert-end-tag
		     "</SUB>"))


(defun hm--html-add-subscript-to-region ()
  "Adds the HTML tags for Subscript to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<SUB>"
			       'hm--html-insert-end-tag
			       "</SUB>"))


(defun hm--html-add-option ()
  "Adds the HTML tags for Option at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<OPT>"
		     'hm--html-insert-end-tag
		     "</OPT>"))


(defun hm--html-add-option-to-region ()
  "Adds the HTML tags for Option to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<OPT>"
			       'hm--html-insert-end-tag
			       "</OPT>"))


(defun hm--html-read-font-size (&optional only-absolute-size)
  "Reads the size for the FONT element.
It returns nil, if the size should not be changed."
  (let ((size
	 (if only-absolute-size
	     (completing-read "The absolute font size (1 .. 7): "
			      '(("7") ("6") ("5") ("4") ("3") ("2") ("1"))
			      nil
			      t
			      "4")
	   (completing-read "The relative (+/-) or absolute font size: "
			    '(("-7") ("-6") ("-5") ("-4") ("-3") ("-2") ("-1")
			      ("+7") ("+6") ("+5") ("+4") ("+3") ("+2") ("+1")
			      ("7") ("6") ("5") ("4") ("3") ("2") ("1")
			      ("use-basefont"))
			    nil
			    t
			    "use-basefont-size"))))
    (if (string= size "use-basefont-size")
	nil
      size)))
	
(defun hm--html-read-font-color ()
  "Reads the size for the FONT element.
It returns nil, if the color should not be changed."
  (let ((color
	 (completing-read "The font color: "
			  '(("Black") ("Silver") ("Gray") ("White") ("Maroon")
			    ("Green") ("Lime") ("Olive") ("Yellow") ("Navy")
			    ("Red") ("Purple") ("Fuchsia") ("Blue") ("Teal")
			    ("Aqua") ("dont-set-color"))
			  nil
			  nil
			  "dont-set-color")))
    (if (string= color "dont-set-color")
	nil
      color)))
	

(defun hm--html-add-font (size color)
  "Adds the HTML tags for Font at the point in the current buffer."
  (interactive (list (hm--html-read-font-size)
		     (hm--html-read-font-color)))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<FONT"
			     (if size 
				 (concat " SIZE=" size)
			       "")
			     (if color
				 (concat " COLOR=" color)
			       "")
			     ">")
		     'hm--html-insert-end-tag-with-newline
		     "</FONT>"))


(defun hm--html-add-font-to-region (size color)
  "Adds the HTML tags for Font to the region."
  (interactive (list (hm--html-read-font-size)
		     (hm--html-read-font-color)))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       (concat "<FONT"
				       (if size 
					   (concat " SIZE=" size)
					 "")
				       (if color
					   (concat " COLOR=" color)
					 "")
				       ">")
			       'hm--html-insert-end-tag-with-newline
			       "</FONT>"))


;;; Lists


(defun hm--html-add-center ()
  "Adds the HTML tags for center at the current point."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<CENTER>"
		     'hm--html-insert-end-tag-with-newline
		     "</CENTER>"))

(defun hm--html-add-center-to-region ()
  "Adds the HTML tags for center to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<CENTER>"
			       'hm--html-insert-end-tag-with-newline
			       "</CENTER>"))


(defvar hm--html-mapname-history nil
  "The history variable for the function `hm--html-read-mapname'.")

(defun hm--html-read-mapname ()
  "Reads the name of an image map."
  (let ((name (read-string "The name of the image map: "
			   (or (car hm--html-mapname-history)
			       "map")
			   'hm--html-mapname-history)))
    name))

(defun hm--html-add-image-map ()
  "Adds an image and a map element."
  (interactive)
  (let* ((href (hm--html-read-url "Image URL: "))
	 (alt (hm--html-read-altenate href))
	 (alignment (hm--html-read-alignment
		     "Alignment of the image: "))
	 (mapname (hm--html-read-mapname)))
    (hm--html-add-image href alt alignment mapname)
    (newline)
    (hm--html-add-map mapname)
    (call-interactively 'hm--html-add-area)))

(defun hm--html-add-map (name)
  "Adds the HTML tags for map at the current point."
  (interactive (list (hm--html-read-mapname)))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<MAP NAME=\"" name "\">")
		     'hm--html-insert-end-tag
		     "</MAP>")
  (end-of-line 0))

(defun hm--html-add-map-to-region (name)
  "Adds the HTML tags for map to the region."
  (interactive (list (hm--html-read-mapname)))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       (concat "<MAP NAME=\"" name "\">")
			       'hm--html-insert-end-tag-with-newline
			       "</MAP>"))


(defun hm--html-add-numberlist ()
  "Adds the HTML tags for a numbered list at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<OL>"
		     'hm--html-insert-end-tag-with-newline
		     "</OL>"
		     'hm--html-insert-start-tag
		     "<LI> "
		     'hm--html-insert-end-tag
		     " </LI>"))
  
(defun hm--html-add-numberlist-to-region ()
  "Adds the HTML tags for a numbered list to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<OL>"
			       'hm--html-insert-end-tag-with-newline
			       "</OL>"))


(defun hm--html-add-directory-list ()
  "Adds the HTML tags for a directory list at the point in the current buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<DIR>"
		     'hm--html-insert-end-tag-with-newline
		     "</DIR>"
		     'hm--html-insert-start-tag
		     "<LI> "
		     'hm--html-insert-end-tag
		     " </LI>"))
  
(defun hm--html-add-directorylist-to-region ()
  "Adds the HTML tags for a directory list to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<DIR>"
			       'hm--html-insert-end-tag-with-newline
			       "</DIR>"))


(defun hm--html-add-list ()
  "Adds the HTML tags for a (unnumbered) list to the region."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
			       "<UL>"
			       'hm--html-insert-end-tag-with-newline
			       "</UL>"
			       'hm--html-insert-start-tag
			       "<LI> "
			       'hm--html-insert-end-tag
			       " </LI>"))


(defun hm--html-add-list-to-region ()
  "Adds the HTML tags for a (unnumbered) list to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<UL>"
			       'hm--html-insert-end-tag-with-newline
			       "</UL>"))


(defun hm--html-add-menu ()
  "Adds the HTML tags for a menu."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<MENU>"
		     'hm--html-insert-end-tag-with-newline
		     "</MENU>"
		     'hm--html-insert-start-tag
		     "<LI> "
		     'hm--html-insert-end-tag
		     " </LI>"))


(defun hm--html-add-menu-to-region ()
  "Adds the HTML tags for a menu to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<MENU>"
			       'hm--html-insert-end-tag-with-newline
			       "</MENU>"))


(defun hm--html-add-description-title-and-entry ()
  "Adds a definition title and entry.
Assumes we're at the end of a previous entry."
  (interactive)
  (hm--html-add-description-title)
  (let ((position (point))
	(case-fold-search t))
    (search-forward "</dt>")
    (hm--html-add-description-entry)
    (goto-char position)))


(defun hm--html-add-description-list ()
  "Adds the HTML tags for a description list.
It also inserts a tag for the description title."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     "<DL>"
		     'hm--html-insert-end-tag-with-newline
		     "</DL>"
		     'hm--html-insert-start-tag
		     "<DT> "
		     'hm--html-insert-end-tag
		     " </DT>"))
  

(defun hm--html-add-description-list-to-region ()
  "Adds the HTML tags for a description list to a region.
It also inserts a tag for the description title."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<DL>"
			       'hm--html-insert-end-tag-with-newline
			       "</DL>"))
  

(defun hm--html-add-description-title ()
  "Adds the HTML tags for a description title at current point in the buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-end-tag-with-newline
		     "<DT> "
		     'hm--html-insert-end-tag
		     " </DT>"))


(defun hm--html-add-description-title-to-region ()
  "Adds the HTML tags for a description title to the region in the buffer."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<DT> "
			       'hm--html-insert-end-tag
			       " </DT>"))


(defun hm--html-add-description-entry ()
  "Adds the HTML tags for a description entry at current point in the buffer."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-end-tag-with-newline
		     "<DD> "
		     'hm--html-insert-end-tag
		     " </DD>"))


(defun hm--html-add-description-entry-to-region ()
  "Adds the HTML tags for a description entry to the region in the buffer."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<DD> "
			       'hm--html-insert-end-tag
			       " </DD>"))


(defun hm--html-add-address ()
  "Adds the HTML tags for an address."
  (interactive)
  (hm--html-add-tags 'hm--html-insert-start-tag
		     "<ADDRESS>"
		     'hm--html-insert-end-tag
		     "</ADDRESS>"))

(defun hm--html-add-address-to-region ()
  "Adds the HTML tags for an address to the region"
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       "<ADDRESS>"
			       'hm--html-insert-end-tag
			       "</ADDRESS>"))


(defvar hm--html-signature-reference-name "Signature"
  "The signature reference name.")


(defun hm--html-make-signature-link-string (signature-file-name)
  "Returns a string which is a link to a signature file."
  (concat
   "<A NAME=\""
   hm--html-signature-reference-name
   "\"\nHREF=\""
   signature-file-name
   "\">"))


(defun hm--html-delete-old-signature ()
  "Searches for the old signature and deletes it, if the user want it"
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "<address>[ \t\n]*"
				     "<a[ \t\n]+name=[ \t\n]*\"?"
				     hm--html-signature-reference-name
				     "\"?[ \t\n]+href=[ \t\n]*\"")
			     nil
			     t)
	  (let ((signature-start (match-beginning 0))
		(signature-end (progn
				 (re-search-forward "</address>[ \t]*[\n]?"
						    nil
						    t) 
				 (point))))
	    (when (yes-or-no-p "Delete the old signature (yes or no) ?")
	      (delete-region signature-start signature-end)
	      (hm--html-indent-line)))))))


(defun hm--html-set-point-for-signature ()
  "Searches and sets the point for inserting the signature.
It searches from the end to the beginning of the file. At first it
tries to use the point before the </body> tag then the point before
the </html> tag and the the end of the file."
  (goto-char (point-max))
  (let ((case-fold-search t))
    (cond ((search-backward "</body>" nil t)
	   (end-of-line 0)
	   (if (> (current-column) 0)
	       (newline 1)))
	  ((search-backward "</html>" nil t)
	   (end-of-line 0)
	   (if (> (current-column) 0)
	       (newline 2)))
	  ((> (current-column) 0)
	   (newline 2))
	  (t))))


(defun hm--html-add-signature ()
  "Adds the owner's signature at the end of the buffer."
  (interactive)
  (if hm--html-signature-file
      (progn
	(if (not hm--html-username)
	    (setq hm--html-username (user-full-name)))
	(save-excursion
	  (hm--html-delete-old-signature)
	  (hm--html-set-point-for-signature)
	  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
			     "<ADDRESS>"
			     'hm--html-insert-end-tag
			     "</A>\n</ADDRESS>"
			     'hm--html-insert-start-tag
			     (hm--html-make-signature-link-string
			      hm--html-signature-file)
			     )
	  (insert hm--html-username)))
    (error "ERROR: Define your hm--html-signature-file first !")))


(defun hm--html-add-header (size &optional header)
  "Adds the HTML tags for a header at the point in the current buffer."
  (interactive "nSize (1 .. 6; 1 biggest): ")
  (if (or (< size 1) (> size 6))
      (message "The size must be a number from 1 to 6 !")
    (hm--html-add-tags 'hm--html-insert-start-tag
		       (format "<H%d>" size)
		       'hm--html-insert-start-tag-with-newline
		       (format "</H%d>" size))
    (if header
	(insert header))))


(defun hm--html-add-header-to-region (size)
  "Adds the HTML tags for a header to the region.
The parameter 'size' specifies the size of the header."
  (interactive "nSize (1 .. 6; 1 biggest): ")
  (if (or (< size 1) (> size 6))
      (message "The size must be a number from 1 to 6 !")
    (hm--html-add-tags-to-region 'hm--html-insert-start-tag
				 (format "<H%d>" size)
				 'hm--html-insert-end-tag
				 (format "</H%d>" size))))


(defun hm--html-set-point-for-title ()
  "Searches and sets the point for inserting the HTML element title.
The functions start at the beginning of the file and searches first
for the HTML tag <ISINDEX>. If such a tag exists, the point is set to the
position after the tag. If not, the function next searches for the
tag <HEAD> and sets the point after the tag, if it exists, or searches for
the tag <HTML>. If this tag exists, the point is set to the position after
this tag or the beginning of the file otherwise."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (cond ((search-forward-regexp "<isindex[^>]*>" nil t) (newline))
	  ((search-forward-regexp "<head[^>]*>" nil t) (newline))
	  ((search-forward-regexp "<html[^>]*>" nil t) (newline))
	  (t))))


(defun hm--html-add-title (title)
  "Adds the HTML tags for a title at the beginning of the buffer."
  (interactive "sTitle: ")
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (search-forward "<title>" nil t)
	  (let ((point-after-start-tag (point)))
	    (if (not (search-forward "</title>" nil t))
 		nil
	      (goto-char (- (point) 8))
	      (delete-backward-char (- (point) point-after-start-tag))
	      (let ((start (point)))
		(if hm--html-automatic-create-title-date
		    (insert title " (" (hm--date) ")")
		  (insert title))
		(goto-char start))))
	;; Noch kein <TITLE> im Buffer vorhanden
	(hm--html-set-point-for-title)
	(hm--html-add-tags 'hm--html-insert-start-tag
			   "<TITLE>"
			   'hm--html-insert-end-tag
			   "</TITLE>"
			   'insert
			   (if hm--html-automatic-create-title-date
			       (concat title " (" (hm--date) ")")
			     title))
	(forward-char 8)
	(newline 1)
	))))


(defun hm--html-add-title-to-region ()
  "Adds the HTML tags for a title to the region."
  (interactive)
  (let ((title (buffer-substring (region-beginning) (region-end)))
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "<title>" nil t)
	  (let ((point-after-start-tag (point)))
	    (if (not (search-forward "</title>" nil t))
		nil
	      (goto-char (- (point) 8))
	      (delete-backward-char (- (point) point-after-start-tag))
	      (if hm--html-automatic-create-title-date
		  (insert title " (" (hm--date) ")")
		(insert title))))
	;; Noch kein <TITLE> im Buffer vorhanden
	(hm--html-set-point-for-title)
	(hm--html-add-tags 'hm--html-insert-start-tag
			   "<TITLE>"
			   'hm--html-insert-end-tag
			   "</TITLE>"
			   'insert
			   (if hm--html-automatic-create-title-date
			       (concat title " (" (hm--date) ")")
			     title))
	(forward-char 8)
	;(newline 1)
	))))


(defun hm--html-add-html ()
  "Adds the HTML tags <HTML> and </HTML> in the buffer.
The tag <HTML> will be inserted at the beginning (after the
<!DOCTYPE ...>, if it is already there.) and </HTML> at the
end of the file." 
  (interactive)
  (let ((new-cursor-position nil)
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "<html>" nil t)
	  (error "There is an old tag <HTML> in the current buffer !")
	(re-search-forward "<!DOCTYPE[^>]*>[ \t\n]*" nil t)
	(hm--html-add-tags 'hm--html-insert-start-tag-with-newline "<HTML>")
;	(newline 1)
	)
      (setq new-cursor-position (point))
      (goto-char (point-max))
      (if (search-backward "</html>" nil t)
	  (error "There is an old tag </HTML> in the current buffer !")
	(newline 1)
	(hm--html-add-tags 'hm--html-insert-end-tag "</HTML>")))
    (goto-char new-cursor-position)))


(defun hm--html-add-head ()
  "Adds the HTML tags <HEAD> and </HEAD> in the buffer.
The tags will be inserted after <HTML> or at the beginning
of the file after <DOCTYPE...> (if it is already there).
The function also looks for the tags <BODY> and </TITLE>." 
  (interactive)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (re-search-forward "<!DOCTYPE[^>]*>[ \t\n]*" nil t)
    (if (search-forward "<html>" nil t)
	(if (search-forward "<head>" nil t)
	    (error "There is an old tag <HEAD> in the current buffer !")
	  (if (search-forward "</head>" nil t)
	      (error "There is an old tag </HEAD> in the current buffer !")  
	    (newline 1))))
    (let ((start-tag-position (point)))
      (if (search-forward "<body>" nil t)
	  (progn
	    (forward-line 0)
	    (forward-char -1)
	    (if (= (point) (point-min))
		(progn
		  (newline)
		  (forward-line -1)))
	    (hm--html-add-tags 'hm--html-insert-end-tag-with-newline  
			       "</HEAD>")
	    (goto-char start-tag-position)
	    (hm--html-add-tags 'hm--html-insert-start-tag-with-newline 
			       "<HEAD>")
	    )
	(if (search-forward "</title>" nil t)
	    (progn
	      (newline 1)
	      (hm--html-add-tags 'hm--html-insert-end-tag-with-newline  
				 "</HEAD>")
	      (goto-char start-tag-position)
	      (hm--html-add-tags 'hm--html-insert-start-tag-with-newline 
				 "<HEAD>"))
	  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline 
			     "<HEAD>"
			     'hm--html-insert-end-tag-with-newline 
			     "</HEAD>"))))))


(defun hm--html-add-head-to-region ()
  "Adds the HTML tags <HEAD> and </HEAD> to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<HEAD>"
			       'hm--html-insert-end-tag-with-newline
			       "</HEAD>"))  


(defun hm--html-add-body ()
  "Adds the HTML tags <BODY> and </BODY> in the buffer.
The tags will be inserted before </HTML> or at the end of the file." 
  (interactive)
  (let ((case-fold-search t))
    (goto-char (point-max))
    (if (search-backward "</html>" nil t)
	(progn
	  (if (search-backward "</body>" nil t)
	      (error "There is an old tag </BODY> in the current buffer !")
	    (if (search-backward "<body>" nil t)
		(error "There is an old tag <BODY> in the current buffer !")))
	  (forward-char -1)))
    (let ((end-tag-position (set-marker (make-marker) (point))))
      (if (search-backward "</head>" nil t)
	  (progn
	    (forward-char 7)
	    (newline 1)
	    (hm--html-add-tags 'hm--html-insert-start-tag-with-newline 
			       "<BODY>")
	    (let ((cursor-position (point)))
	      (goto-char end-tag-position)
	      (hm--html-add-tags 'hm--html-insert-end-tag-with-newline 
				 "</BODY>")
	      (goto-char cursor-position)
	      ))
	(if (not (= (current-column) 0))
	    (newline))
	(hm--html-add-tags 'hm--html-insert-start-tag-with-newline "<BODY>"
			   'hm--html-insert-end-tag-with-newline "</BODY>")))))


(defun hm--html-add-body-to-region ()
  "Adds the HTML tags <BODY> and </BODY> to the region."
  (interactive)
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       "<BODY>"
			       'hm--html-insert-end-tag-with-newline
			       "</BODY>"))  


(defun hm--html-add-title-and-header (title)
  "Adds the HTML tags for a title and a header in the current buffer."
  (interactive "sTitle and Header String: ")
  (let ((case-fold-search t))
    (hm--html-add-title title)
    (save-excursion
      (goto-char (point-min))
      (search-forward "</title>" nil t)
      (if (search-forward "</head>" nil t)
	  (progn
	    (search-forward "<body>" nil t)
	    (newline 1))
	(if (search-forward "<body>" nil t)
	    (newline 1)
	  (if (string= (what-line) "Line 1")
	      (progn
		(end-of-line)
		(newline 1)))))
      (hm--html-add-header 1 title))))


(defun hm--html-add-title-and-header-to-region ()
  "Adds the HTML tags for a title and a header to the region."
  (interactive)
  (let ((title (buffer-substring (region-beginning) (region-end))))
    (hm--html-add-header-to-region 1)
    (hm--html-add-title title)))


(defun hm--html-add-full-html-frame (title)
  "Adds a full HTML frame to the current buffer.
The frame consists of the elements html, head, body, title,
header and the signature. The parameter TITLE specifies the
title and the header of the document."
  (interactive "sTitle and Header String: ")
  (let ((case-fold-search t))
    (hm--html-add-doctype)
    (hm--html-add-html)
    (hm--html-add-head)
    (hm--html-add-body)
    (hm--html-add-title-and-header title)
    (when hm--html-signature-file
      (hm--html-add-signature))
    (goto-char (point-min))
    (search-forward "</h1>" nil t)
    (forward-line 1)
    (when hm--html-automatic-created-comment
      (hm--html-insert-created-comment))
    (when hm--html-automatic-create-modified-line
      (hm--html-insert-modified-line))))


(defun hm--html-add-full-html-frame-with-region ()
  "Adds a full HTML frame to the current buffer with the use of a region.
The frame consists of the elements html, head, body, title,
header and the signature. The function uses the region as
the string for the title and the header of the document."
  (interactive)
  (hm--html-add-title-and-header-to-region)
  (hm--html-add-doctype)
  (hm--html-add-html)
  (hm--html-add-head)
  (hm--html-add-body)
  (hm--html-add-signature)
  (when hm--html-automatic-created-comment
    (hm--html-insert-created-comment))
  (when hm--html-automatic-create-modified-line
    (hm--html-insert-modified-line)))


(defun hm--html-add-link-target-to-region (name)
  "Adds the HTML tags for a link target to the region."
  (interactive "sName: ")
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       (concat "<A NAME=\"" name "\">")
			       'hm--html-insert-end-tag
			       "</A>"))

(defun hm--html-add-link-target (name)
  "Adds the HTML tags for a link target at point in the current buffer."
  (interactive "sName: ")
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<A NAME=\"" name "\">")
		     'hm--html-insert-end-tag
		     "</A>"))


;;; Functions which add links

(defun hm--html-mark-example (parameter-list)
  "Marks the example of the parameterlist in the current buffer.
It returns the example extent."
  (let ((case-fold-search t))
    (if (hm--html-get-example-from-parameter-list parameter-list)
	(progn
	  (search-forward (hm--html-get-example-from-parameter-list 
			   parameter-list))
	  (let ((extent (make-extent (match-beginning 0)
				     (match-end 0))))
	    (set-extent-face extent 'hm--html-help-face)
	    extent)))))


(defun hm--html-unmark-example (extent)
  "Unmarks the example for the current question."
  (if extent
      (delete-extent extent)))


(defun hm--html-write-alist-in-buffer (alist)
  "The function writes the contents of the ALIST in the currentbuffer."
  (cond ((car alist)
	 (insert (int-to-string (car (car alist))) ":\t" (cdr (car alist)))
	 (newline)
	 (hm--html-write-alist-in-buffer (cdr alist)))))


(defun hm--html-select-directory (alist default)
  "The function selects one of the directories of the ALIST,
or the DEFAULT or the 'default-directory' by number. See also the
documentation of the function hm--html-read-filename."
  (if (or (string= default "") (not default))
      (setq default default-directory))
  (if alist
      (save-window-excursion
	(let ((buffername (generate-new-buffer "*html-directories*")))
	  (set-buffer buffername)
	  (insert "Select one of the following directories by number !")
	  (newline)
	  (insert "===================================================")
	  (newline)
	  (insert "0:\t" default)
	  (newline)
	  (hm--html-write-alist-in-buffer alist)
	  (goto-char (point-min))
	  (pop-to-buffer buffername))
	(let ((dirnumber (read-number 
			  "Select directory prefix by number: "
			  t)))
	(kill-buffer "*html-directories*")
	(expand-file-name (or (cdr (assoc dirnumber alist)) default))))
    (expand-file-name default))
  )


(defun hm--html-delete-wrong-path-prefix-1 (filename prefix-list)
  "The function deletes wrong path prefixes."
  (cond (prefix-list (if (string-match (car prefix-list) filename)
			 (substring filename (match-end 0))
		       (hm--html-delete-wrong-path-prefix-1 filename
							    (cdr prefix-list)
							    )))
	(t filename)))


(defun hm--html-delete-wrong-path-prefix (filename)
  "The function deletes wrong path prefixes.
The path prefixes are specified by the variable 
`hm--html-delete-wrong-path-prefix'."
  (if (not hm--html-delete-wrong-path-prefix)
      filename
    (if (listp hm--html-delete-wrong-path-prefix)
	(hm--html-delete-wrong-path-prefix-1 filename 
					     hm--html-delete-wrong-path-prefix)
      (hm--html-delete-wrong-path-prefix-1 filename 
					   (list
					    hm--html-delete-wrong-path-prefix))
      )))


(defun hm--html-read-filename (parameter-list)
  "The function reads a filename with its directory path, 
if PARAMETER-LIST is not nil. If the PARAMETER-LIST is nil, only an empty
string will be returned.
The PARAMETER-LIST consists of the following elements:
	PROMPT, ALIST, DEFAULT, REQUIRE-MATCH, EXAMPLE.
If the ALIST is nil and DEFAULT is nil, then the function only reads
a filename (without path). These precede the following.
If the ALIST isn't nil, the function lists the contents of the ALIST
in a buffer and reads a number from the minbuffer, which selects one
of the directories (lines) of the buffer. Therefore the ALIST must look
like the following alist:
	((1 . \"/appl/gnu/\") (2 . \"/\"))
If only ALIST is nil, or if you type a number which is not in the ALIST,
the DEFAULT directory is selected. If the DEFAULT is nil or \"\" the 
'default-directory' is selected.
After that the function reads the name of the file from the minibuffer.
Therefore the PROMPT is printed in the minibuffer and the selected directory
is taken as the start of the path of the file.
If REQUIRE-MATCH is t, the filename with path must match an existing file."
  (if parameter-list
      (let ((marked-object (hm--html-mark-example parameter-list))
	    (prompt (hm--html-get-prompt-from-parameter-list parameter-list))
	    (alist (hm--html-get-alist-from-parameter-list parameter-list))
	    (default (hm--html-get-default-from-parameter-list parameter-list))
	    (require-match (hm--html-get-require-match-from-parameter-list
			    parameter-list))
	    (filename nil))
	(if (or alist default)
	    (let ((directory (hm--html-select-directory alist default)))
	      (setq filename (read-file-name prompt
					     directory
					     directory
					     require-match
					     nil)))
	  (setq filename (read-file-name prompt
					 ""
					 ""
					 require-match
					 nil)))
	(hm--html-unmark-example marked-object)
	(hm--html-delete-wrong-path-prefix filename))
    ""))


(defun hm--html-completing-read (parameter-list)
  "Reads a string with completing-read, if alist is non nil.
The PARAMETER-LIST consists of the following elements:
	PROMPT, ALIST, DEFAULT, REQUIRE-MATCH, EXAMPLE.
If ALIST is nil, it returns the DEFAULT, or if the DEFAULT is
also nil it returns an empty string."
  (let ((marked-object (hm--html-mark-example parameter-list))
	(string 
	 (if (hm--html-get-alist-from-parameter-list parameter-list)
	     (completing-read 
	      (hm--html-get-prompt-from-parameter-list parameter-list)
	      (hm--html-get-alist-from-parameter-list parameter-list)
	      nil
	      (hm--html-get-require-match-from-parameter-list
	       parameter-list)
	      (hm--html-get-default-from-parameter-list 
	       parameter-list))
	   (if (hm--html-get-default-from-parameter-list parameter-list)
	       (hm--html-get-default-from-parameter-list parameter-list)
	     ""))))
    (hm--html-unmark-example marked-object)
    string))


(defvar hm--html-faces-exist nil)


(defun hm--html-generate-help-buffer-faces ()
  "Generates faces for the add-link-help-buffer."
  (if (not (facep 'hm--html-help-face))
      (progn
	(setq hm--html-faces-exist t)
	(make-face 'hm--html-help-face)
	(if hm--html-help-foreground
	    (set-face-foreground 'hm--html-help-face hm--html-help-foreground))
	(if hm--html-help-background
	    (set-face-background 'hm--html-help-face hm--html-help-background))
	(set-face-font 'hm--html-help-face hm--html-help-font)
	)))


(defun hm--html-get-prompt-from-parameter-list (parameter-list)
  "Returns the prompt from the PARAMETER-LIST."
  (car parameter-list))


(defun hm--html-get-alist-from-parameter-list (parameter-list)
  "Returns the alist from the PARAMETER-LIST."
  (car (cdr parameter-list)))


(defun hm--html-get-default-from-parameter-list (parameter-list)
  "Returns the default from the PARAMETER-LIST."
  (car (cdr (cdr parameter-list))))


(defun hm--html-get-require-match-from-parameter-list (parameter-list)
  "Returns the require-match from the PARAMETER-LIST."
  (car (cdr (cdr (cdr parameter-list)))))


(defun hm--html-get-example-from-parameter-list (parameter-list)
  "Returns the example from the PARAMETER-LIST."
  (car (cdr (cdr (cdr (cdr parameter-list))))))


(defun hm--html-get-anchor-seperator-from-parameter-list (parameter-list)
  "Returns the anchor-seperator from the PARAMETER-LIST."
  (car (cdr (cdr (cdr (cdr (cdr parameter-list)))))))


(defun hm--html-generate-add-link-help-buffer (scheme-parameter-list
					       host-name:port-parameter-list
					       servername:port-parameter-list
					       path+file-parameter-list
					       anchor-parameter-list)
  "Generates and displays a help buffer with an example for adding a link."
  (let ((buffername (generate-new-buffer "*Link-Example*")))
    (pop-to-buffer buffername)
    (shrink-window (- (window-height) 5))
    (insert "Example:")
    (newline 2)
    (if (hm--html-get-example-from-parameter-list scheme-parameter-list)
	(progn
	  (insert (hm--html-get-example-from-parameter-list
		   scheme-parameter-list))
	  (if (hm--html-get-example-from-parameter-list 
	       scheme-parameter-list)
	      (progn
		(insert ":")
		(if (hm--html-get-example-from-parameter-list 
		     host-name:port-parameter-list)
		    (insert "//"))))))
    (if (hm--html-get-example-from-parameter-list 
	 host-name:port-parameter-list)
	(progn
	  (insert (hm--html-get-example-from-parameter-list
		   host-name:port-parameter-list))
	  (if (and (hm--html-get-example-from-parameter-list
		    servername:port-parameter-list)
		   (not (string= "/"
				 (substring
				  (hm--html-get-example-from-parameter-list
				   servername:port-parameter-list)
				  0
				  1))))
	      (insert "/"))))
    (if (hm--html-get-example-from-parameter-list 
	 servername:port-parameter-list)
	(progn
	  (insert (hm--html-get-example-from-parameter-list
		   servername:port-parameter-list))
	  (if (hm--html-get-example-from-parameter-list
	       path+file-parameter-list)
	      (insert "/"))))
    (if (hm--html-get-example-from-parameter-list path+file-parameter-list)
	(progn
	  (insert (hm--html-get-example-from-parameter-list
		   path+file-parameter-list))))
    (if (hm--html-get-example-from-parameter-list anchor-parameter-list)
	(progn
	  (insert (hm--html-get-anchor-seperator-from-parameter-list
		   anchor-parameter-list))
	  (insert (hm--html-get-example-from-parameter-list
		   anchor-parameter-list))))
    (goto-char (point-min))
    buffername
    ))


(defun hm--html-add-link (function-add-tags
			  scheme-parameter-list
			  host-name:port-parameter-list
			  servername:port-parameter-list
			  path+file-parameter-list
			  anchor-parameter-list)
  "The function adds a link in the current buffer.
The parameter FUNCTION-ADD-TAGS determines the function which adds the tag
in the buffer (for example: 'hm--html-add-tags or 
'hm--html-add-tags-to-region).
The parameters SCHEME-PARAMETER-LIST, HOST-NAME:PORT-PARAMETER-LIST,
SERVERNAME:PORT-PARAMETER-LIST, PATH+FILE-PARAMETER-LIST and
ANCHOR-PARAMETER-LIST are lists with a prompt string, an alist, a default
value and an example string. The ANCHOR-PARAMETER-LIST has as an additional
element an anchor seperator string. All these elements are used to read and
construct the link."
;  (let ((point nil))
  (save-window-excursion
    (let ((html-buffer (current-buffer))
	  (html-help-buffer (hm--html-generate-add-link-help-buffer
			     scheme-parameter-list
			     host-name:port-parameter-list
			     servername:port-parameter-list
			     path+file-parameter-list
			     anchor-parameter-list))
	  (scheme (hm--html-completing-read scheme-parameter-list))
	  (hostname:port (hm--html-completing-read 
			  host-name:port-parameter-list))
	  (servername:port (hm--html-completing-read 
			    servername:port-parameter-list))
	  (path+file (hm--html-read-filename path+file-parameter-list))
	  (anchor (hm--html-completing-read anchor-parameter-list))
;	  (hrefname (setq html-link-counter (1+ html-link-counter)))
	  (anchor-seperator 
	   (hm--html-get-anchor-seperator-from-parameter-list
	    anchor-parameter-list)))
      (if (not (string= scheme ""))
	  (if (string= hostname:port "")
	      (setq scheme (concat scheme ":"))
	    (setq scheme (concat scheme "://"))))
      (if (and (not (string= hostname:port ""))
	       (not (string= servername:port ""))
	       (not (string= (substring servername:port 0 1) "/")))
	  (setq servername:port (concat "/" servername:port)))
      (if (and (not (string= path+file ""))
	       (not (string= "/" (substring path+file 0 1))))
	  (setq path+file (concat "/" path+file)))
      (if (not (string= anchor ""))
	  (setq anchor (concat anchor-seperator anchor)))
      (kill-buffer  html-help-buffer)
      (pop-to-buffer html-buffer)
      (eval (list function-add-tags 
		  ''hm--html-insert-start-tag
		  (concat "<A"
;		            "<A Name="
;			    hrefname
			  " HREF=\""
			  scheme
			  hostname:port
			  servername:port
			  path+file
			  anchor
			  "\">")
		  ''hm--html-insert-end-tag
		  "</A>")))
;    (setq point (point))))
; (goto-char (point)))
    ))

(defun hm--html-add-info-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link on a GNU Info file."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "http" 
		      t
		      "http")
		     (list				; hostname:port
		      "Gateway and Port: "
		      hm--html-info-hostname:port-alist
		      hm--html-info-hostname:port-default
		      nil
		      "www.tnt.uni-hannover.de:8005")
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; path/file
		      "Path/File: "
		      hm--html-info-path-alist
		      nil
		      nil
		      "/appl/lemacs/Global/info/dir")
		     (list				; anchor
		      "Node: "
		      '((""))
		      nil
		      nil
		      "emacs"
		      ",")))


(defun hm--html-add-info-link ()
  "Adds the HTML tags for a link on a GNU Info file."
  (interactive)
  (hm--html-add-info-link-1 'hm--html-add-tags))


(defun hm--html-add-info-link-to-region ()
  "Adds the HTML tags for a link on a GNU Info file to the region."
  (interactive)
  (hm--html-add-info-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-wais-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a WAIS server."
  (hm--html-add-link function-add-tags
		     (list				   ; scheme 
		      ""
		      nil
		      "http" 
		      t
		      "http")
		     (list				   ; hostname:port
		      "Gateway and Port: "
		      hm--html-wais-hostname:port-alist
		      hm--html-wais-hostname:port-default
		      nil
		      "www.tnt.uni-hannover.de:8001")
		     (list				   ; servername:port
		      "Wais Servername and Port: "
		      hm--html-wais-servername:port-alist
		      hm--html-wais-servername:port-default
		      nil
		      "quake.think.com:210")
		     (list				   ; path/file
		      "Database: "
		      hm--html-wais-path-alist
		      nil
		      nil
		      "database")
		     (list				   ; anchor
		      "Searchstring: "
		      '((""))
		      nil
		      nil
		      "searchstring"
		      "?")))


(defun hm--html-add-wais-link ()
  "Adds the HTML tags for a link to a WAIS server."
  (interactive)
  (hm--html-add-wais-link-1 'hm--html-add-tags))


(defun hm--html-add-wais-link-to-region ()
  "Adds the HTML tags for a link to a WAIS server to the region."
  (interactive)
  (hm--html-add-wais-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-direct-wais-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a direct link to a WAIS server.
This function uses the new direct WAIS support instead of a WAIS gateway."
  (hm--html-add-link function-add-tags
		     (list				   ; scheme 
		      ""
		      nil
		      "wais" 
		      t
		      "wais")
		     (list				; hostname:port
		      "Wais Servername and Port: "
		      hm--html-wais-servername:port-alist
		      hm--html-wais-servername:port-default
		      nil
		      "quake.think.com:210")
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				   ; path/file
		      "Database: "
		      hm--html-wais-path-alist
		      nil
		      nil
		      "database")
		     (list				   ; anchor
		      "Searchstring: "
		      '((""))
		      nil
		      nil
		      "searchstring"
		      "?")))


(defun hm--html-add-direct-wais-link ()
  "Adds the HTML tags for a direct link to a WAIS server.
This function uses the new direct WAIS support instead of a WAIS gateway."
  (interactive)
  (hm--html-add-direct-wais-link-1 'hm--html-add-tags))


(defun hm--html-add-direct-wais-link-to-region ()
  "Adds the HTML tags for a direct link to a WAIS server to the region.
This function uses the new direct WAIS support instead of a WAIS gateway."
  (interactive)
  (hm--html-add-direct-wais-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-html-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to an HTML page."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "http" 
		      t
		      "http")
		     (list				; hostname:port
		      "Servername and Port: "
		      hm--html-html-hostname:port-alist
		      hm--html-html-hostname:port-default
		      nil
		      "www.tnt.uni-hannover.de:80")
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; path/file
		      "Path/File: "
		      hm--html-html-path-alist
		      nil
		      nil
		      "/data/info/www/tnt/overview.html")
		     (list				; anchor
		      "Anchor: "
		      '((""))
		      nil
		      nil
		      "1"
		      "#")))
  

(defun hm--html-add-html-link ()
  "Adds the HTML tags for a link to an HTML file."
  (interactive)
  (hm--html-add-html-link-1 'hm--html-add-tags))


(defun hm--html-add-html-link-to-region ()
  "Adds the HTML tags for a link to an HTML file to the region."
  (interactive)
  (hm--html-add-html-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-file-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a filegateway link."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "file" 
		      t
		      "file")
		     (list				; hostname:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; path/file
		      "Path/File: "
		      hm--html-file-path-alist
		      nil
		      nil
		      "/data/info/www/tnt/overview.html")
		     (list				; anchor
		      "Anchor: "
		      '((""))
		      nil
		      nil
		      "1"
		      "#")))
  

(defun hm--html-add-file-link ()
  "Adds the HTML tags for a for a filegateway link."
  (interactive)
  (hm--html-add-file-link-1 'hm--html-add-tags))


(defun hm--html-add-file-link-to-region ()
  "Adds the HTML tags for a for a filegateway link to the region."
  (interactive)
  (hm--html-add-file-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-ftp-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to an FTP server."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "ftp" 
		      t
		      "ftp")
		     (list				; hostname:port
		      "FTP Servername: "
		      hm--html-ftp-hostname:port-alist
		      hm--html-ftp-hostname:port-default
		      nil
		      "ftp.rrzn.uni-hannover.de")
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; path/file
		      "Path/File: "
		      hm--html-ftp-path-alist
		      nil
		      nil
		      "/pub/gnu/gcc-2.4.5.tar.gz")
		     (list				; anchor
		      ""
		      nil
		      ""
		      t
		      nil
		      nil)))
  

(defun hm--html-add-ftp-link ()
  "Adds the HTML tags for a link to an FTP server."
  (interactive)
  (hm--html-add-ftp-link-1 'hm--html-add-tags))


(defun hm--html-add-ftp-link-to-region ()
  "Adds the HTML tags for a link to an FTP server to the region."
  (interactive)
  (hm--html-add-ftp-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-gopher-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a gopher server."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "gopher" 
		      t
		      "gopher")
		     (list				; hostname:port
		      "Gopher Servername: "
		      hm--html-gopher-hostname:port-alist
		      hm--html-gopher-hostname:port-default
		      nil
		      "newsserver.rrzn.uni-hannover.de:70")
		     (list				; servername:port
		      "Documenttype: "
		      hm--html-gopher-doctype-alist
		      hm--html-gopher-doctype-default
		      nil
		      "/1")
		     nil				; path/file
		     (list				; anchor
		      "Entrypoint: "
		      hm--html-gopher-anchor-alist
		      nil
		      nil
		      "Subject%20Tree"
		      "/")))
  

(defun hm--html-add-gopher-link ()
  "Adds the HTML tags for a link to a gopher server."
  (interactive)
  (hm--html-add-gopher-link-1 'hm--html-add-tags))


(defun hm--html-add-gopher-link-to-region ()
  "Adds the HTML tags for a link to a gopher server to the region."
  (interactive)
  (hm--html-add-gopher-link-1 'hm--html-add-tags-to-region))


(defun hm--html-make-proggate-alist (proggate-allowed-file)
  "Makes a proggate-alist from the PROGGATE-ALLOWED-FILE."
  (if (and (stringp proggate-allowed-file)
	   (file-exists-p proggate-allowed-file))
      (save-window-excursion
	(let ((alist nil)
	      (buffername (find-file-noselect proggate-allowed-file))
	      (case-fold-search t))
	  (set-buffer buffername)
	  (toggle-read-only)
	  (goto-char (point-min))
	  (while (search-forward-regexp "[^ \t\n]+" nil t)
	    (setq alist (append (list (list (buffer-substring 
					     (match-beginning 0)
					     (match-end 0))))
					    alist)))
	  (kill-buffer buffername)
	  alist))
    (error "ERROR: Can't find the 'hm--html-progate-allowed-file !")))


(defun hm--html-add-proggate-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a program.
The program is called via the program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (let ((progname-alist (hm--html-make-proggate-alist
			 hm--html-proggate-allowed-file)))
    (hm--html-add-link function-add-tags
		       (list		; scheme 
			""
			nil
			"http" 
			t
			"http")
		       (list		; hostname:port
			"Servername and Port: "
			hm--html-proggate-hostname:port-alist
			hm--html-proggate-hostname:port-default
			nil
			"www.tnt.uni-hannover.de:8007")
		       (list		; program
			"Programname: "
			progname-alist
			nil
			nil
			"/usr/ucb/man")
		       nil		; path/file
		       (list		; Program Parameter
			"Programparameter: "
			'((""))
			nil
			nil
			"8+lpd"
			"+"))))
  

(defun hm--html-add-proggate-link ()
  "Adds the HTML tags for a link to a program.
The program is called via the program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (interactive)
  (hm--html-add-proggate-link-1 'hm--html-add-tags))


(defun hm--html-add-proggate-link-to-region ()
  "Adds the HTML tags for a link to a program to the region.
The program is called via the program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (interactive)
  (hm--html-add-proggate-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-local-proggate-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a program.
The program is called via the local program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (hm--html-add-link function-add-tags
		     (list		; scheme 
		      ""
		      nil
		      "" 
		      t
		      nil)
		     (list		; hostname:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list		; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list		; path/file
		      "Path/file: "
		      hm--html-local-proggate-path-alist
		      nil
		      nil
		      "/data/info/programs/lemacs.evlm")
		     (list		; anchor
		      ""
		      nil
		      ""
		      t
		      nil)))
  

(defun hm--html-add-local-proggate-link ()
  "Adds the HTML tags for a link to a program.
The program is called via the local program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (interactive)
  (hm--html-add-local-proggate-link-1 'hm--html-add-tags))


(defun hm--html-add-local-proggate-link-to-region ()
  "Adds the HTML tags for a link to a program to the region.
The program is called via the local program gateway.
Email to muenkel@tnt.uni-hannover.de for information over
this gateway."
  (interactive)
  (hm--html-add-local-proggate-link-1 'hm--html-add-tags-to-region))


(defvar hm--html-newsgroup-alist nil
  "Alist with newsgroups for the newsgateway.")


(defvar gnus-newsrc-assoc nil)


(defun hm--html-make-newsgroup-alist ()
  "Makes a hm--html-make-newsgroup-alist from a .newsrc.el file.
The function looks at the environment variable NNTPSERVER.
If this variable exists, it tries to open the file with the Name
~/$NNTPSERVER.el. If this file exists, the alist of the file is
returned as the newsgroup-alist. If the file doesn't exist, it
tries to use the file ~/$NNTPSERVER to make the alist. The function
returns '((\"\"))"
  (if hm--html-newsgroup-alist
      hm--html-newsgroup-alist
    (if gnus-newsrc-assoc
	(setq hm--html-newsgroup-alist gnus-newsrc-assoc)
      (if (not (getenv "NNTPSERVER"))
	  '((""))
	(let ((newsrc-file (expand-file-name (concat "~/.newsrc-"
						     (getenv "NNTPSERVER")))))
	  (if (file-exists-p (concat newsrc-file ".el"))
	      (progn
		(load-file (concat newsrc-file ".el"))
		(setq hm--html-newsgroup-alist gnus-newsrc-assoc))
	    (if (not (file-exists-p newsrc-file))
		'((""))
	      (save-window-excursion
		(let ((alist nil)
		      (buffername (find-file-noselect newsrc-file))
		      (case-fold-search t))
		  (set-buffer buffername)
		  (toggle-read-only)
		  (goto-char (point-min))		  
		  (while (search-forward-regexp "[^:!]+" nil t)
		    (setq alist (append (list (list (buffer-substring
						     (match-beginning 0)
						     (match-end 0))))
					alist))
		    (search-forward-regexp "\n"))
		  (kill-buffer buffername)
		  (setq hm--html-newsgroup-alist alist))))))))))
    


(defun hm--html-add-news-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a news group."
  (let ((newsgroup-alist (hm--html-make-newsgroup-alist)))
    (hm--html-add-link function-add-tags
		       (list		; scheme 
			""
			nil
			"news" 
			t
			"news")
		       (list		; hostname:port
			""
			nil
			""
			t
			nil)
		       (list		; servername:port
			"NEWS Group: "
			newsgroup-alist
			nil
			nil
			"comp.emacs.xemacs")
		       nil		; path/file
		       (list		; anchor
			""
			nil
			""
			t
			nil
			nil))))
  

(defun hm--html-add-news-link ()
  "Adds the HTML tags for a link to a news group."
  (interactive)
  (hm--html-add-news-link-1 'hm--html-add-tags))


(defun hm--html-add-news-link-to-region ()
  "Adds the HTML tags for a link to a news group to the region."
  (interactive)
  (hm--html-add-news-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-mail-box-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a link to a mail box."
  (hm--html-add-link function-add-tags
		     (list				; scheme 
		      ""
		      nil
		      "http" 
		      t
		      "http")
		     (list				; hostname:port
		      "Hostname and Port: "
		      hm--html-mail-hostname:port-alist
		      hm--html-mail-hostname:port-default
		      nil
		      "www.tnt.uni-hannover.de:8003")
		     (list				; servername:port
		      ""
		      nil
		      ""
		      t
		      nil)
		     (list				; path/file
		      "Path/File: "
		      hm--html-mail-path-alist
		      nil
		      nil
		      "/data/info/mail/mailbox")
		     (list				; anchor
		      ""
		      nil
		      ""
		      t
		      nil
		      nil)))
  

(defun hm--html-add-mail-box-link ()
  "Adds the HTML tags for a link to a mail box."
  (interactive)
  (hm--html-add-mail-box-link-1 'hm--html-add-tags))


(defun hm--html-add-mail-box-link-to-region ()
  "Adds the HTML tags for a link to a mail box to the region."
  (interactive)
  (hm--html-add-mail-box-link-1 'hm--html-add-tags-to-region))


(defun hm--html-add-mailto-link-1 (function-add-tags)
  "Internal function. Adds the HTML tags for a mailto link."
  (let ((mailto-alist (if (and (boundp 'user-mail-address)
			       user-mail-address)
			  (cons (list user-mail-address)
				hm--html-mailto-alist)
			hm--html-mailto-alist)))
    (hm--html-add-link function-add-tags
		       (list				; scheme 
			""
			nil
			"mailto" 
			t
			"mailto")
		       (list				; hostname:port
			""
			nil
			""
			t
			nil)
		       (list				; servername:port
			"Mailaddress: "
			mailto-alist
			nil
			nil
			"muenkel@tnt.uni-hannover.de")
		       nil		                ; path/file
		       (list				; anchor
			""
			nil
			""
			t
			nil
			nil))))

(defun hm--html-add-mailto-link ()
  "Adds the HTML tags for a mailto link."
  (interactive)
  (hm--html-add-mailto-link-1 'hm--html-add-tags))


(defun hm--html-add-mailto-link-to-region ()
  "Adds the HTML tags for a mailto link to the region."
  (interactive)
  (hm--html-add-mailto-link-1 'hm--html-add-tags-to-region))

(defun hm--html-add-relative-link (relative-file-path)
  "Adds the HTML tags for a relative link at the current point."
  (interactive (list (file-relative-name
		      (read-file-name "Relative Filename: "
				      nil
				      nil
				      nil
				      "")
		      default-directory)
		     ))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<A HREF=\""
			     relative-file-path
			     "\">")
		     'hm--html-insert-end-tag
		     "</A>"))

(defun hm--html-add-relative-link-to-region (relative-file-path)
  "Adds the HTML tags for a relative link to the region."
  (interactive (list (file-relative-name
		      (read-file-name "Relative Filename: "
				      nil
				      nil
				      nil
				      ""))))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       (concat "<A HREF=\""
				       relative-file-path
				       "\">")
			       'hm--html-insert-end-tag
			       "</A>"))

(defun hm--html-add-normal-link (link-object)
  "Adds the HTML tags for a normal general link.
Single argument LINK-OBJECT is value of HREF in the new anchor.
Mark is set after anchor."
  (interactive "sNode Link to: ")
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<A HREF=\""
			     link-object
			     "\">")
		     'hm--html-insert-end-tag
		     "</A>"))

(defun hm--html-add-normal-link-to-region (link-object)
  "Adds the HTML tags for a normal general link to region.
Single argument LINK-OBJECT is value of HREF in the new anchor.
Mark is set after anchor."
  (interactive "sNode Link to: ")
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       (concat "<A HREF=\""
				       link-object
				       "\">")
			       'hm--html-insert-end-tag
			       "</A>"))


(defun hm--html-add-normal-node-link ()
  "Adds the HTML tags for a normal node link (<LINK...>) at the point."
  (interactive)
  (hm--html-insert-start-tag (concat "<LINK HREF=\""
				     (read-string "Node Link to: ")
				     "\">")
			     ))

;;; Functions to update the date and the changelog entries


(defun hm--html-maybe-new-date-and-changed-comment ()
  "Hook function which updates the date in the title line, if
'hm--html-automatic-update-title-date' is t and which inserts a 
\"changed comment\" line, if 'hm--html-automatic-changed-comment' is t."
  (when hm--html-automatic-update-title-date 
    (hm--html-new-date))
  (when hm--html-automatic-changed-comment 
    (hm--html-insert-changed-comment t))
  (when hm--html-automatic-update-modified-line
    (hm--html-insert-modified-line)))
      

(defun hm--html-new-date ()
  "The function sets the date in the title line up."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (end-of-head (if (search-forward "</head>" nil t)
			   (point)
			 (if (search-forward "<body>" nil t)
			     (point)
			   (point-max)))))
      (goto-char (point-min))
      (if (re-search-forward 
	   (concat
	    "\\((\\)"
	    "\\([ \t]*[0-3]?[0-9]-[A-Z][a-z][a-z]-[0-9][0-9][0-9][0-9]"
	    "[ \t]*\\)"
	    "\\()[ \t\n]*</title>\\)") 
	   end-of-head
	   t)
	  (progn
	    (delete-region (match-beginning 2) (match-end 2))
	    (goto-char (match-beginning 2))
	    (insert (hm--date)))))))


(defun hm--html-insert-created-comment (&optional noerror)
  "The function inserts a \"created comment\".
The comment looks like <!-- Created by: Heiko Münkel, 10-Dec-1993 -->.
The comment will be inserted after the title line.
An error message is printed, if there is no title line and if
noerror is nil."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (end-of-head (if (search-forward "</head>" nil t)
			   (point)
			 (if (search-forward "<body>" nil t)
			     (point)
			   (point-max))))
	  (comment-infix (or hm--html-comment-infix
			     (concat (or hm--html-username (user-full-name))
				     ", "))))
      (goto-char (point-min))
      (if (not (search-forward "</title>" end-of-head t))
	  (if (not noerror)
	      (error "ERROR: Please insert a title in the document !"))
;	(let ((end-of-title-position (point)))
	(if (search-forward (concat "<!-- "
				    hm--html-created-comment-prefix)
			    end-of-head
			    t)
	    (if (yes-or-no-p 
		 (concat "Replace the old comment \"<!-- "
			 hm--html-created-comment-prefix
			 "\" "))
		(progn
		  (goto-char (match-beginning 0))
		  (kill-line)
		  (hm--html-add-comment)
		  (insert hm--html-created-comment-prefix
			  comment-infix
;			  (or hm--html-username (user-full-name))
;			  ", "
			  (hm--date))))
	  (newline)
	  (hm--html-add-comment)
	  (insert hm--html-created-comment-prefix
		  comment-infix
;	  (insert "Created by: " 
;		  (or hm--html-username (user-full-name))
;		  ", "
		  (hm--date)
		  ))))))


(defun hm--html-insert-changed-comment-1 (newline)
  "Internal function of 'hm--html-insert-changed-comment'.
Inserts a newline if NEWLINE is t, before the comment is inserted."
  (if newline
      (progn
	(newline)))
  (hm--html-add-comment)
  (insert hm--html-changed-comment-prefix
	  (or hm--html-comment-infix
	      (concat (or hm--html-username (user-full-name)) ", "))
	  (hm--date)))

(defun hm--html-insert-changed-comment (&optional noerror)
  "The function inserts a \"changed comment\".
The comment looks by default like 
	<!-- Changed by: Heiko Münkel, 10-Dec-1993 -->.
The comment will be inserted after the last \"changed comment\" line, or,
if there isn't such a line, after the \"created comment\" line, or,
after the title line. If there is no title and NOERROR is nil, an error 
message is generated. The line is not inserted after the end of the head 
or the beginning of the body.
If the last \"changed line\" is from the same author, it is only replaced
by the new one. 

Look at the variables `hm--html-changed-comment-prefix' and
`hm--html-comment-infix', if you'd like to change the
inserted comments. You should not use different values for this
variables in the same HTML file.

Attention: Don't write anything else in such a line!"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (end-of-head (if (search-forward "</head>" nil t)
			   (point)
			 (if (search-forward "<body>" nil t)
			     (point)
			   (point-max))))
	  (comment-infix  (or hm--html-comment-infix
			      hm--html-username
			      (user-full-name))))
;	  (username (or hm--html-username (user-full-name))))
      (goto-char end-of-head)
;      (if (search-backward "<!-- Changed by: " nil t)
      (if (search-backward (concat "<!-- "
				   hm--html-changed-comment-prefix)
			   nil
			   t)
;	  (if (string-match username
	  (if (string-match comment-infix
			    (buffer-substring (point)
					      (progn
						(end-of-line)
						(point))))
	      ;; exchange the comment line
	      (progn
		(beginning-of-line)
		(delete-region (point) (progn
					      (end-of-line)
					      (point)))
		(hm--html-insert-changed-comment-1 nil))
	    ;; new comment line
	    (end-of-line)
	    (hm--html-insert-changed-comment-1 t))
;	(if (search-backward "<!-- Created by: " nil t)
	(if (search-backward (concat "<!-- "
				     hm--html-created-comment-prefix)
			     nil
			     t)
	    (progn
	      (end-of-line)
	      (hm--html-insert-changed-comment-1 t))
	  (if (search-backward "</title>" nil t)
	      (progn
		(goto-char (match-end 0))
		(if (not (looking-at "\n"))
		    (progn
		      (newline)
		      (forward-char -1)))
		(hm--html-insert-changed-comment-1 t))
	    (if (not noerror)
		(error 
		 "ERROR: Insert at first a title in the document !"))))))))

(defun hm--html-insert-modified-line ()
  "Inserts a modified line.
By default the line looks like:
    	<EM>Modified: 23-May-1997</EM>

Look at the variables `hm--html-automatic-create-modified-line',
`hm--html-automatic-update-modified-line', `hm--html-modified-prefix',
`hm--html-modified-start-tag', `hm--html-modified-end-tag' and
`hm--html-modified-insert-before' to change the behaviour of this
feature."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward-regexp (concat hm--html-modified-start-tag
					"[ \t\n]*"
					hm--html-modified-prefix)
				nil
				t)
	(progn ; old modified line exists
	  (goto-char (match-end 0))
	  (if (search-forward-regexp (concat "\\([ \t\n]*\\)"
					     "\\([^ \t\n<]+\\)"
					     "\\([ \t\n]*" 
					     hm--html-modified-end-tag
					     "\\)"))
	      (progn
		(delete-region (match-beginning 2) (match-end 2))
		(goto-char (match-beginning 2))
		(insert (hm--date)))
;	      (replace-match (concat "\\1" (hm--date)))
	    (error "Destroyed \"Modified line\" found!")))
      (search-backward hm--html-modified-insert-before nil t)
      (search-backward "</html>" nil t)
      (search-backward "</body>" nil t)
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (unless (= (util-return-end-of-line) (point))
	(end-of-line)
	(newline))
      (newline)
      (indent-according-to-mode)
      (insert hm--html-modified-start-tag
	      hm--html-modified-prefix
	      (hm--date)
	      hm--html-modified-end-tag))))
				    

;;; Functions to insert templates

(defvar hm--html-template-file-history nil
  "Historyvariable for the template files in the `hm--html-mode'.")

(defun hm--html-insert-template (filename)
  "Inserts a templatefile.
It uses `tmpl-insert-template-file' to insert
the templates. The variables `tmpl-template-dir-list',
`tmpl-automatic-expand' and `tmpl-history-variable-name' are
overwritten by `hm--html-template-dir',
`hm--html-automatic-expand-templates' and `hm--html-template-file-history'."
  (interactive (list nil))
  (let ((tmpl-template-dir-list (if (listp hm--html-template-dir)
				    hm--html-template-dir
				  (list hm--html-template-dir)))
	(tmpl-automatic-expand hm--html-automatic-expand-templates)
	(tmpl-history-variable-name 'hm--html-template-file-history))
    (if filename
	(tmpl-insert-template-file filename)
      (call-interactively 'tmpl-insert-template-file))
    ))

(defun hm--html-insert-template-from-fixed-dirs (filename)
  "Inserts a templatefile.
It uses `tmpl-insert-template-file-from-fixed-dirs' to insert
the templates. The variables `tmpl-template-dir-list',
`tmpl-automatic-expand', `tmpl-filter-regexp' and
`tmpl-history-variable-name' are overwritten by
`hm--html-template-dir', `hm--html-automatic-expand-templates',
`hm--html-template-filter-regexp' and `hm--html-template-file-history'."
  (interactive (list nil))
  (let ((tmpl-template-dir-list (if (listp hm--html-template-dir)
				    hm--html-template-dir
				  (list hm--html-template-dir)))
	(tmpl-automatic-expand hm--html-automatic-expand-templates)
	(tmpl-filter-regexp hm--html-template-filter-regexp)
	(tmpl-history-variable-name 'hm--html-template-file-history))
    (if filename
	(tmpl-insert-template-file-from-fixed-dirs filename)
      (call-interactively 'tmpl-insert-template-file-from-fixed-dirs))
    ))


;;; Functions for font lock mode

(if (adapt-emacs19p)
    (progn
      (make-face 'font-lock-comment-face)
      (make-face 'font-lock-doc-string-face)
      (make-face 'font-lock-string-face)
      (or (face-differs-from-default-p 'font-lock-doc-string-face)
	  (copy-face 'font-lock-comment-face 'font-lock-doc-string-face))
      (or (face-differs-from-default-p 'font-lock-comment-face)
	  (copy-face 'italic 'font-lock-comment-face))
      (or (face-differs-from-default-p 'font-lock-string-face)
	  (progn
	    (copy-face 'font-lock-doc-string-face 'font-lock-string-face)
	    (set-face-underline-p 'font-lock-string-face t)))
      (setq font-lock-comment-face 'font-lock-comment-face)
      (setq font-lock-string-face 'font-lock-string-face)))


;;; Functions to insert forms

(defun hm--html-form-read-method ()
  "Reads the method for a form."
  (completing-read "Method of the form: "
		   '(("POST") ("GET"))
		   nil
		   t
		   "POST"))


(defun hm--html-form-read-action (method)
  "Reads the URL for the action attribute of a form.
It returns nil if no action attribute is wanted.
METHOD is the method of the form."
  (if (y-or-n-p "Current document URL as action attribute ? ")
      nil
    (hm--html-read-url "Query server URL: "
		       hm--html-url-alist
		       (function
			(lambda (table-element-list)
			  (hm--html-read-url-predicate table-element-list
						       (car
							(read-from-string
							 method)))))
		       nil
		       nil)))


(defun hm--html-add-form (&optional method)
  "Adds the HTML tags for a form.
The function asks only for a method, if METHOD is nil, otherwise
the METHOD must have one of the values \"GET\" or \"POST\"."
  (interactive)
  (let* ((method (or method (hm--html-form-read-method)))
	 (action (hm--html-form-read-action method)))
    (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		       (concat "<FORM METHOD=\""
			       method
			       "\""
			       (if action
				   (concat " ACTION=\""
					   action
					   "\"")
				 "")
			       ">")
		       'hm--html-insert-end-tag-with-newline
		       "</FORM>")))


(defun hm--html-add-form-to-region (&optional method)
  "Adds the HTML tags for a form to a region.
The function asks only for a method, if METHOD is nil, otherwise
the METHOD must have one of the values \"GET\" or \"POST\"."
  (interactive)
  (let* ((method (or method (hm--html-form-read-method)))
	 (action (hm--html-form-read-action method)))
    (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
				 (concat "<FORM METHOD=\""
					 method
					 "\""
					 (if action
					     (concat " ACTION=\""
						     action
						     "\"")
					   "")
					 ">")
				 'hm--html-insert-end-tag-with-newline
				 "</FORM>")))


(defun hm--html-form-read-name (&optional last-name)
  "Reads the name for an input tag."
  (read-string "Symbolic name: " last-name))


(defun hm--html-form-read-value (prompt &optional initial-contents)
  "Reads the value for an input tag."
  (read-string prompt initial-contents))


(defun hm--html-form-read-checked ()
  "Reads whether a button is checked by default or not."
  (y-or-n-p "Should the button be checked by default ? "))


(defun hm--html-form-read-size ()
  "Reads the size of text entry fields of input tags."
  (if (y-or-n-p "Defaultsize of the Inputfield ? ")
      nil
    (format "%d,%d"
	    (read-number "Width of the input field: " t)
	    (read-number "Height of the input field: " t))))


(defun hm--html-form-read-maxlength ()
  "Reads the maxlength of text entry fields of input tags."
  (let ((maxlength (read-number "Maximum number of chars (0 = unlimited): "
				t)))
    (if (<= maxlength 0)
	nil
      (int-to-string maxlength))))


(defun hm--html-form-read-src (prompt &optional initial-contents)
  "Reads the src for an input tag."
  (read-string prompt initial-contents))


(defun hm--html-form-add-input (type 
				name 
				value 
				checked 
				size 
				maxlength
				&optional src)
  "Adds the HTML tags for an input tag to the buffer."
  (hm--html-insert-start-tag (concat "<INPUT TYPE=\""
				     type
				     "\""
				     (if (and name (not (string= name "")))
					 (concat " NAME=\"" name "\""))
				     (if (and value (not (string= value "")))
					 (concat " VALUE=\"" value "\""))
				     (if checked " CHECKED")
				     (if (and size (not (string= size "")))
					 (concat " SIZE=" size))
				     (if (and maxlength
					      (not (string= maxlength "")))
					 (concat " MAXLENGTH=" 
						 maxlength 
						 ))
				     (if (and src
					      (not (string= src "")))
					 (concat " SRC=\""
						 src
						 "\""))
				     ">")))


(defun hm--html-form-add-input-text (name value size maxlength)
  "Adds the HTML tags for a text input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "text" name value nil size maxlength))


(defun hm--html-form-add-input-password (name value size maxlength)
  "Adds the HTML tags for a password input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "password" name value nil size maxlength))


(defun hm--html-form-add-input-integer (name value size maxlength)
  "Adds the HTML tags for a integer input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "int" name value nil size maxlength))


(defun hm--html-form-add-input-float (name value size maxlength)
  "Adds the HTML tags for a float input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "float" name value nil size maxlength))


(defun hm--html-form-add-input-date (name value size maxlength)
  "Adds the HTML tags for a date input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "date" name value nil size maxlength))


(defun hm--html-form-add-input-url (name value size maxlength)
  "Adds the HTML tags for a url input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "url" name value nil size maxlength))


(defun hm--html-form-add-input-scribble (name value size maxlength)
  "Adds the HTML tags for a scribble input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Defaultvalue: ") 
		     (hm--html-form-read-size)
		     (hm--html-form-read-maxlength)))
  (hm--html-form-add-input "scribble" name value nil size maxlength))


(defun hm--html-form-add-input-checkbox (name value checked)
  "Adds the HTML tags for a checkbox button."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-form-read-value "Checkbox value: ")
		     (hm--html-form-read-checked)))
  (hm--html-form-add-input "checkbox" name value checked nil nil))


(defvar hm--html-last-radio-button-name nil
  "Name of the last radio button.")


(defun hm--html-form-add-input-radio (name value checked)
  "Adds the HTML tags for a radio button."
  (interactive (list (hm--html-form-read-name hm--html-last-radio-button-name)
		     (hm--html-form-read-value "Radiobutton value: ")
		     (hm--html-form-read-checked)))
  (setq hm--html-last-radio-button-name name)
  (hm--html-form-add-input "radio" name value checked nil nil))


(defun hm--html-form-add-input-submit (value)
  "Adds the HTML tags for a submit input field."
  (interactive (list (hm--html-form-read-value 
		      "Label of the submit button: "
		      "Submit")))
  (hm--html-form-add-input "submit" nil value nil nil nil))


(defun hm--html-form-add-input-image (name src)
  "Adds the HTML tags for an image input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-read-url "Image URL: "
					hm--html-url-alist
					(function
					 (lambda (table-element-list)
					   (hm--html-read-url-predicate 
					    table-element-list
					    'IMAGE)))
					nil
					nil)))
  (hm--html-form-add-input "IMAGE"
			   name
			   nil
			   nil
			   nil
			   nil
			   src))


(defun hm--html-form-add-input-audio (name src)
  "Adds the HTML tags for an audio input field."
  (interactive (list (hm--html-form-read-name)
		     (hm--html-read-url "Audio URL: "
					hm--html-url-alist
					(function
					 (lambda (table-element-list)
					   (hm--html-read-url-predicate 
					    table-element-list
					    'AUDIO)))
					nil
					nil)))
  (hm--html-form-add-input "AUDIO"
			   name
			   nil
			   nil
			   nil
			   nil
			   src))


(defun hm--html-form-add-input-reset (value)
  "Adds the HTML tags for a reset input field."
  (interactive (list (hm--html-form-read-value 
		      "Label of the reset button: "
		      "Reset")))
  (hm--html-form-add-input "reset" nil value nil nil nil))


(defun hm--html-form-add-input-isindex (size)
  "Adds the HTML tags for an isindex input field.
Size is the value of the input field wide."
  (interactive "nWidth of the input field (i.e: 20): ")
  (hm--html-insert-start-tag (concat "<INPUT NAME=\"isindex\""
				     (if (= size 20)
					 ">"
				       (format
					" SIZE=%d>"
					size)))))


(defun hm--html-form-add-select-option-menu (name)
  "Adds the HTML tags for a select option menu to the buffer."
  (interactive (list (hm--html-form-read-name)))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<SELECT NAME=\"" name "\">")
		     'hm--html-insert-end-tag-with-newline
		     "</SELECT>"
		     'hm--html-insert-start-tag
		     "<OPTION> "))
  
			     
(defun hm--html-form-add-select-scrolled-list (name listsize multiple)
  "Adds the HTML tags for a select scrolled list to the buffer."
  (interactive (list (hm--html-form-read-name)
		     (read-number "No of visible items (>1): " t)
		     (y-or-n-p "Multiple selections allowed ? ")))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<SELECT NAME=\"" 
			     name 
			     "\" SIZE="
			     (int-to-string listsize)
			     (if multiple
				 " MULTIPLE")
			     ">")
		     'hm--html-insert-end-tag-with-newline
		     "</SELECT>"
		     'hm--html-insert-start-tag
		     "<OPTION> "))


(defun hm--html-form-add-select-option (selected-by-default)
  "Adds the tags for an option in a select form menu."
  (interactive (list (y-or-n-p "Select this option by default ? ")))
  (hm--html-insert-end-tag-with-newline (concat "<OPTION"
						  (if selected-by-default
						      " SELECTED")
						  "> ")))


(defun hm--html-form-add-textarea (name rows columns)
  "Adds the tags for a textarea tag."
  (interactive (list (hm--html-form-read-name)
		     (read-number "Number of Rows of the Textarea: " t)
		     (read-number "Number of Columns of the Textarea: " t)))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<TEXTAREA NAME=\""
			     name
			     "\" ROWS="
			     (int-to-string rows)
			     " COLS="
			     (int-to-string columns)
			     ">")
		     'hm--html-insert-end-tag
		     "</TEXTAREA>"))


;;; Functions to insert tables

(defun hm--html-add-table (border compact)
  "Add the HTML tags for a table frame. 
If BORDER is t, then the table should be drawn with a border.
If COMPACT is t, then the table should be drawn in a smaller size."
  (interactive (list (y-or-n-p "Use a table with a border? ")
		     (y-or-n-p "Use a small table? ")))
  (hm--html-add-tags 'hm--html-insert-start-tag-with-newline
		     (concat "<TABLE"
			     (if border " border" "")
			     (if compact " compact" "")
			     ">")
		     'hm--html-insert-start-tag-with-newline
		     "</TABLE>")
  (backward-char))


(defun hm--html-add-table-to-region (border compact)
  "Add the HTML tags for a table frame. 
If BORDER is t, then the table should be drawn with a border.
If COMPACT is t, then the table should be drawn in a smaller size."
  (interactive (list (y-or-n-p "Use a table with a border? ")
		     (y-or-n-p "Use a small table? ")))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag-with-newline
			       (concat "<TABLE"
				       (if border " border" "")
				       (if compact " compact" "")
				       ">")
			       'hm--html-insert-start-tag-with-newline
			       "</TABLE>"))


(defun hm--html-add-table-title (top)
  "Adds the HTML tag for a table title at the current point.
If TOP is t, then the title will positioned at the top instead of the
bottom of the table."
  (interactive (list (y-or-n-p "Put the title at the table top? ")))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "\n<CAPTION"
			     (if top " align=top" " align=bottom")
			     "> ")
		     'hm--html-insert-end-tag
		     " </CAPTION>"))


(defun hm--html-add-table-title-to-region (top)
  "Adds the HTML tag for a table title to the region.
If TOP is t, then the title will positioned at the top instead of the
bottom of the table."
  (interactive (list (y-or-n-p "Put the title at the table top? ")))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       (concat "<CAPTION"
				       (if top " align=top" " align=bottom")
				       "> ")
			       'hm--html-insert-end-tag
			       " </CAPTION>"))


(defvar hm--html-table-alignment-alist '(("default")
					 ("left")
					 ("right")
					 ("center"))
  "Alist with table alignments.")

(defun hm--html-table-read-cell-entries-and-alignments (cell-no 
							no-of-cells
							&optional 
							alignment-list)
  "Reads the alignments and the entries for NO-OF-CELLS cells.
The return is a list with strings of the form: \"align=left> entry\".
CELL-NO is the current cell no.
If (car ALIGNMENT-LIST) is non-nil, then it is used as alignment."
  (if (> cell-no no-of-cells)
      nil
    (let ((alignment 
	   (or (car alignment-list)
	       (completing-read (format "Alignment of the %d. cell: " 
					cell-no)
				hm--html-table-alignment-alist
				nil
				t
				"default")))
	  (entry (read-string (format "Entry of the %d. cell: " cell-no))))
      (if (string= "default" alignment)
	  (setq alignment "")
	(setq alignment (concat " align=" alignment)))
      (cons (concat alignment "> " entry)
	    (hm--html-table-read-cell-entries-and-alignments (1+ cell-no)
							     no-of-cells
							     (cdr 
							      alignment-list))
	    ))))

(defun hm--html-add-table-header (no-of-cells)
  "Adds the HTML tags for a complete simple table header line.
It asks for the number of cells and the allignment of the cells.
The number of cells can also be given as prefix argument."
  (interactive "NNo of cells in a row: ")
  (if (< no-of-cells 1)
      (error "ERROR: There must be at least one cell in a row!"))
  (hm--html-add-tags
   'hm--html-insert-end-tag-with-newline
   (concat "<TR>"
	   (mapconcat '(lambda (entry)
			 (concat "<TH" entry))
		      (hm--html-table-read-cell-entries-and-alignments
		       1 
		       no-of-cells)
		      " </TH>")
	   " </TH></TR>")))


(defun hm--html-add-first-table-row (no-of-cells)
  "Adds the HTML tags for a table row.
It asks for the number of cells and the allignment of the cells.
The number of cells can also be given as prefix argument."
  (interactive "NNo of cells in a row: ")
  (if (< no-of-cells 1)
      (error "ERROR: There must be at least one cell in a row!"))
  (hm--html-add-tags 
   'hm--html-insert-end-tag-with-newline
   (concat "<TR><TD"
	   (car (hm--html-table-read-cell-entries-and-alignments 1 1))
	   " </TD>"
	   (if (<= no-of-cells 1)
	       "</TR>"
	     (concat
	      (mapconcat '(lambda (entry)
			    (concat "<TD" entry))
			  (hm--html-table-read-cell-entries-and-alignments 
			   2 no-of-cells)
			 " </TD>")
	      " </TD></TR>")))))


(defun hm--html-table-get-previous-alignments ()
  "Returns a list with the alignments of the previous table row.
The row must be a data row and not a header row!
An example for the return list: '(\"left\" \"default\" \"center\" \"right\")"
  (save-excursion
    (let* ((point-of-view (point))
	   (case-fold-search t)
	   (end-of-last-row (search-backward "</tr>" (point-min) t))
	   (begin-of-last-row (progn (search-backward "<tr" (point-min) t)
				     (re-search-forward "<t[dh]"
							point-of-view t)
				     (match-beginning 0)))
	   (alignment-list nil))
      (goto-char begin-of-last-row)
      (if (not (re-search-forward "<t[dh]" end-of-last-row t))
	  (error "Error: No previous data row found!")
	(goto-char end-of-last-row)
	(while (> (point) begin-of-last-row)
	  (let ((cell-start 
		 (search-backward-regexp "\\(<td[^>]*>\\)\\|\\(<th[^>]*>\\)"
					 begin-of-last-row
					 t)))
	    (if (not cell-start)
		(goto-char begin-of-last-row)
	      (setq alignment-list
		    (cons
		     (if (search-forward-regexp "\\(align=\\)\\([^ \t\n>]*\\)"
						(match-end 0)
						t)
			 (buffer-substring (match-beginning 2)
					   (match-end 2))
		       "default")
		     alignment-list))
	      (goto-char cell-start))))
	alignment-list))))


(defun hm--html-add-additional-table-row ()
  "Adds the HTML tags for a table row.
It tries to detect the number of cells and their alignments
from existing rows of the table."
  (interactive)
  (let* ((old-alignment-list (hm--html-table-get-previous-alignments))
	 (no-of-cells (length old-alignment-list)))
    (hm--html-add-tags
     'hm--html-insert-end-tag-with-newline
     (concat "<TR><TD" (car (hm--html-table-read-cell-entries-and-alignments 
			     1 
			     1
			     old-alignment-list))
	     " </TD>"
	     (if (<= no-of-cells 1)
		 "</TR>"
	       (concat
		(mapconcat '(lambda (entry)
			      (concat "<TD" entry))
			   (hm--html-table-read-cell-entries-and-alignments 
			    2 
			    no-of-cells
			    (cdr old-alignment-list))
			   " </TD>")
		" </TD></TR>"))))))


(defun hm--html-add-row-entry (alignment)
  "Adds the HTML tag for a table row entry at the current point."
  (interactive (list (completing-read "Alignment of the cell: " 
				      hm--html-table-alignment-alist
				      nil
				      t
				      "default")))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<TD"
			     (if (string= "default" alignment)
				 "> "
			       (concat " align=" alignment "> ")))))


(defun hm--html-add-header-entry (alignment)
  "Adds the HTML tag for a table header entry at the current point."
  (interactive (list (completing-read "Alignment of the cell: " 
				      hm--html-table-alignment-alist
				      nil
				      t
				      "default")))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<TH"
			     (if (string= "default" alignment)
				 "> "
			       (concat " align=" alignment "> ")))))


(defun hm--html-add-row-frame (alignment)
  "Adds the HTML tags for a table row start and end at the current point."
  (interactive (list (completing-read "Alignment of the start cell: " 
				      hm--html-table-alignment-alist
				      nil
				      t
				      "default")))
  (hm--html-add-tags 'hm--html-insert-start-tag
		     (concat "<TD"
			     (if (string= "default" alignment)
				 "> "
			       (concat " align=" alignment "> ")))
		     'hm--html-insert-end-tag
		     "<TR>"))


(defun hm--html-add-row-frame-to-region (alignment)
  "Adds the HTML tags for a table row start and end to the current region."
  (interactive (list (completing-read "Alignment of the start cell: " 
				      hm--html-table-alignment-alist
				      nil
				      t
				      "default")))
  (hm--html-add-tags-to-region 'hm--html-insert-start-tag
			       (concat "<TD"
				       (if (string= "default" alignment)
					   "> "
					 (concat " align=" alignment "> ")))
			       'hm--html-insert-end-tag
			       " <TR>"))


(defun hm--html-table-add-colspan-attribute (columns)
  "Adds a colspawn attribute to a table cell.
A prefix arg is used as no of COLUMNS."
  (interactive "NNo of columns, spaned by this cell: ")
  (let ((case-fold-search t))
    (save-excursion
      (if (and (search-backward "<" nil t)
	       (search-forward-regexp "<[ \t\n]*\\(th\\)\\|\\(td\\)" nil t))
	  (if (search-forward-regexp "\\([ \t\n]+colspan=\\)\\([^ \t\n>]*\\)"
				     nil
				     t)
	      (progn
		(delete-region (match-beginning 2) (match-end 2))
		(insert (format "\"%d\"" columns)))
	    (insert (format " colspan=\"%d\"" columns)))
	(error "ERROR: Point not in a table cell!")))))
	     

(defun hm--html-table-add-rowspan-attribute (rows)
  "Adds a rowspan attribute to a table cell.
A prefix arg is used as no of ROWS."
  (interactive "NNo of rows, spaned by this cell: ")
  (let ((case-fold-search t))
    (save-excursion
      (if (and (search-backward "<" nil t)
	       (search-forward-regexp "<[ \t\n]*\\(th\\)\\|\\(td\\)" nil t))
	  (if (search-forward-regexp "\\([ \t\n]+rowspan=\\)\\([^ \t\n>]*\\)"
				     nil
				     t)
	      (progn
		(delete-region (match-beginning 2) (match-end 2))
		(insert (format "\"%d\"" rows)))
	    (insert (format " rowspan=\"%d\"" rows)))
	(error "ERROR: Point not in a table cell!")))))


;;; ISO-Characters for Emacs HTML-mode (Berthold Crysmann)
;(setq buffer-invisibility-spec '(hm--html-iso-entity-invisible-flag))

;(defvar hm--html-iso-entity-invisible-flag t
;  "Controls the visibility of the iso entities.")

;(defvar hm--html-iso-glyph-invisible-flag nil
;  "Controls the visibility of the iso character glyphs.")

;(defvar hm--html-glyph-cache nil
;  "Internal variable. An assoc list with the already created glyphs.")

;(defun hm--html-create-glyph (string)
;  "Creates a glyph from the string or returns an existing one.
;The glyph is stored in `hm--html-glyph-cache'."
;  (if nil ;(assoc string hm--html-glyph-cache)
;      (cdr (assoc string hm--html-glyph-cache))
;    (let ((glyph (make-glyph string)))
;      (setq hm--html-glyph-cache (cons (cons string glyph)
;				       hm--html-glyph-cache))
;      glyph)))

;(defun hm--html-attach-glyph-to-region (start
;					end
;					string
;					region-invisible-flag
;					glyph-invisible-flag)
;  "Make the region invisible and attach a glyph STRING.
;The invisible flags could be used, to toggle the visibility."
;  (mapcar 'delete-annotation (annotations-at end)) ; delete old anotations
;  ;; delete old extents
;  (let ((extent (make-extent start end))
;	(annotation nil))
;    (set-extent-property extent 'invisible region-invisible-flag)
;    (set-extent-property extent 'end-open t)
;    (set-extent-property extent 'start-open t)
;    (set-extent-property extent 'intangible t)
;    (setq annotation (make-annotation "Hallo Du da" ;(hm--html-create-glyph string) 
;				      end
;				      'text))
;    (goto-char end)))


;(defun hm--html-insert-iso-char-as-entity-and-glyph (char entity)
;  "Inserts an iso char as html ENTITY and displays a glyph.
;The glyph is created from the string CHAR."
;  (let ((start (point)))
;    (insert entity)
;    (hm--html-attach-glyph-to-region start 
;				     (point) 
;				     char
;				     'hm--html-iso-entity-invisible-flag
;				     'hm--html-iso-glyph-invisible-flag)))

;(defun hm--html_ue ()
;  (interactive)
;  (hm--html-insert-iso-char-as-entity-and-glyph "ü" "&uuml;"))


;(defun hm--html-insert-iso-char-as-entity-and-glyph (char entity)
;  (let ((start (point))
;	(end nil)
;	(extent nil))
;    (insert entity)
;    (setq end (point))
;    (setq extent (make-extent start end))
;    (set-extent-begin-glyph extent char)
;    (set-extent-property extent 'invisible t)))

;(defun hm--html_ue ()
;  (interactive)
;  (hm--html-insert-iso-char-as-entity-and-glyph ?ü "&uuml;"))

(defun hm--html_ue ()
  "Insert the character 'ue'."
  (interactive)
  (insert "&uuml;"))

(defun hm--html_oe ()
  "Insert the character 'oe'."
  (interactive)
  (insert "&ouml;"))

(defun hm--html_ae ()
  "Insert the character 'ae'."
  (interactive)
  (insert "&auml;"))

(defun hm--html_aa ()
  "Insert the character 'aa'."
  (interactive)
  (insert "&aring;"))

(defun hm--html_Ue ()
  "Insert the character 'Ue'."
  (interactive)
  (insert "&Uuml;"))

(defun hm--html_Oe ()
  "Insert the character 'Oe'."
  (interactive)
  (insert "&Ouml;"))

(defun hm--html_Ae ()
  "Insert the character 'Ae'."
  (interactive)
  (insert "&Auml;"))

(defun hm--html_Aa ()
  "Insert the character 'Aa'."
  (interactive)
  (insert "&Aring;"))

(defun hm--html_sz ()
  "Insert the character 'sz'."
  (interactive)
  (insert "&szlig;"))

(defun hm--html_aacute ()
  "Insert the character 'aacute'."
  (interactive)
  (insert "&aacute;"))

(defun hm--html_eacute ()
  "Insert the character 'eacute'."
  (interactive)
  (insert "&eacute;"))

(defun hm--html_iacute ()
  "Insert the character 'iacute'."
  (interactive)
  (insert "&iacute;"))

(defun hm--html_oacute ()
  "Insert the character 'oacute'."
  (interactive)
  (insert "&oacute;"))

(defun hm--html_uacute ()
  "Insert the character 'uacute'."
  (interactive)
  (insert "&uacute;"))

(defun hm--html_Aacute ()
  "Insert the character 'Aacute'."
  (interactive)
  (insert "&aacute;"))

(defun hm--html_Eacute ()
  "Insert the character 'Eacute'."
  (interactive)
  (insert "&eacute;"))

(defun hm--html_Iacute ()
  "Insert the character 'Iacute'."
  (interactive)
  (insert "&iacute;"))

(defun hm--html_Oacute ()
  "Insert the character 'Oacute'."
  (interactive)
  (insert "&oacute;"))

(defun hm--html_Uacute ()
  "Insert the character 'Uacute'."
  (interactive)
  (insert "&uacute;"))

(defun hm--html_agrave ()
  "Insert the character 'agrave'."
  (interactive)
  (insert "&agrave;"))

(defun hm--html_egrave ()
  "Insert the character 'egrave'."
  (interactive)
  (insert "&egrave;"))

(defun hm--html_igrave ()
  "Insert the character 'igrave'."
  (interactive)
  (insert "&igrave;"))

(defun hm--html_ograve ()
  "Insert the character 'ograve'."
  (interactive)
  (insert "&ograve;"))

(defun hm--html_ugrave ()
  "Insert the character 'ugrave'."
  (interactive)
  (insert "&ugrave;"))

(defun hm--html_Agrave ()
  "Insert the character 'Agrave'."
  (interactive)
  (insert "&Agrave;"))

(defun hm--html_Egrave ()
  "Insert the character 'Egrave'."
  (interactive)
  (insert "&Egrave;"))

(defun hm--html_Igrave ()
  "Insert the character 'Igrave'."
  (interactive)
  (insert "&Igrave;"))

(defun hm--html_Ograve ()
  "Insert the character 'Ograve'."
  (interactive)
  (insert "&Ograve;"))

(defun hm--html_Ugrave ()
  "Insert the character 'Ugrave'."
  (interactive)
  (insert "&Ugrave;"))

(defun hm--html_ccedilla ()
  "Insert the character 'ccedilla'."
  (interactive)
  (insert "&ccedilla;"))

(defun hm--html_Ccedilla ()
  "Insert the character 'Ccedilla'."
  (interactive)
  (insert "&Ccedilla;"))

(defun hm--html_atilde ()
  "Insert the character 'atilde'."
  (interactive)
  (insert "&atilde;"))

(defun hm--html_otilde ()
  "Insert the character 'otilde'."
  (interactive)
  (insert "&otilde;"))

(defun hm--html_ntilde ()
  "Insert the character 'ntilde'."
  (interactive)
  (insert "&ntilde;"))

(defun hm--html_Atilde ()
  "Insert the character 'Atilde'."
  (interactive)
  (insert "&Atilde;"))

(defun hm--html_Otilde ()
  "Insert the character 'Otilde'."
  (interactive)
  (insert "&Otilde;"))

(defun hm--html_Ntilde ()
  "Insert the character 'Ntilde'."
  (interactive)
  (insert "&Ntilde;"))

(defun hm--html_acircumflex ()
  "Insert the character 'acircumflex'."
  (interactive)
  (insert "&acirc;"))

(defun hm--html_ecircumflex ()
  "Insert the character 'ecircumflex'."
  (interactive)
  (insert "&ecirc;"))

(defun hm--html_icircumflex ()
  "Insert the character 'icircumflex'."
  (interactive)
  (insert "&icirc;"))

(defun hm--html_ocircumflex ()
  "Insert the character 'ocircumflex'."
  (interactive)
  (insert "&ocirc;"))

(defun hm--html_ucircumflex ()
  "Insert the character 'ucircumflex'."
  (interactive)
  (insert "&ucirc;"))

(defun hm--html_Acircumflex ()
  "Insert the character 'Acircumflex'."
  (interactive)
  (insert "&Acirc;"))

(defun hm--html_Ecircumflex ()
  "Insert the character 'Ecircumflex'."
  (interactive)
  (insert "&Ecirc;"))

(defun hm--html_Icircumflex ()
  "Insert the character 'Icircumflex'."
  (interactive)
  (insert "&Icirc;"))

(defun hm--html_Ocircumflex ()
  "Insert the character 'Ocircumflex'."
  (interactive)
  (insert "&Ocirc;"))

(defun hm--html_Ucircumflex ()
  "Insert the character 'Ucircumflex'."
  (interactive)
  (insert "&Ucirc;"))

(defun hm--html_ediaeresis ()
  "Insert the character 'ediaeresis'."
  (interactive)
  (insert "&euml;"))

(defun hm--html_idiaeresis ()
  "Insert the character 'idiaeresis'."
  (interactive)
  (insert "&iuml;"))

(defun hm--html_Ediaeresis ()
  "Insert the character 'Ediaeresis'."
  (interactive)
  (insert "&Euml;"))

(defun hm--html_Idiaeresis ()
  "Insert the character 'Idiaeresis'."
  (interactive)
  (insert "&Iuml;"))

(defun hm--html_thorn ()
  "Insert the character 'thorn'."
  (interactive)
  (insert "&thorn;"))

(defun hm--html_Thorn ()
  "Insert the character 'Thorn'."
  (interactive)
  (insert "&THORN;"))

(defun hm--html_eth ()
  "Insert the character 'eth'."
  (interactive)
  (insert "&eth;"))

(defun hm--html_Eth ()
  "Insert the character 'Eth'."
  (interactive)
  (insert "&ETH;"))


;;;
;
; smart functions

(defvar hm--just-insert-less-than nil
  "Internal variable.")

(defun hm--html-less-than ()
  "Inserts the entity '&gt;'."
  (interactive)
  (insert "&lt;"))

(defun hm--html-smart-less-than ()
  "Insert a '<' or the entity '&lt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'hm--html-smart-less-than)
	   hm--just-insert-less-than)
      (progn
	(delete-char -1)
	(hm--html-less-than)
	(setq hm--just-insert-less-than nil))
    (insert ?<)
    (setq hm--just-insert-less-than t)))

(defvar hm--just-insert-greater-than nil
  "Internal variable.")

(defun hm--html-greater-than ()
  "Inserts the entity '&gt;'."
  (interactive)
  (insert "&gt;"))

(defun hm--html-smart-greater-than ()
  "Insert a '>' or the entity '&gt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'hm--html-smart-greater-than)
	   hm--just-insert-greater-than)
      (progn
	(delete-char -1)
	(hm--html-greater-than)
	(setq hm--just-insert-greater-than nil))
    (insert ?>)
    ;; Next line added by Bob Weiner, Altrasoft, 11/21/96.
    #+infodock (if id-html-auto-indent (indent-according-to-mode))
    (setq hm--just-insert-greater-than t)))


(defvar hm--just-insert-ampersand nil
  "Internal variable.")

(defun hm--html-ampersand ()
  "Inserts the entity '&amp;'."
  (interactive)
  (insert "&amp;"))

(defun hm--html-smart-ampersand ()
  "Insert a '&' or the entity '&amp;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'hm--html-smart-ampersand)
	   hm--just-insert-ampersand)
      (progn
	(delete-char -1)
	(hm--html-ampersand)
	(setq hm--just-insert-ampersand nil))
    (insert ?&)
    (setq hm--just-insert-ampersand t)))


;;;
;   sending the contents of a html buffer to netscape
;   (Thanks to Adrian Aichner for providing this function)

(defun hm--html-send-buffer-to-netscape (buffer 
					 &optional new-netscape new-window)
  "View html buffer with Netscape.
This should be changed in the fututure, so that it doesn't need vm."
  (interactive)
  (require 'vm)
  (if new-netscape
      (vm-run-background-command vm-netscape-program buffer-file-name)
    (or (equal 0 
	       (vm-run-command vm-netscape-program 
			       "-remote" 
			       (concat "openURL(file://localhost" 
				       buffer-file-name
				       (if new-window ", new-window" "")
				       ")")))
	(hm--html-send-buffer-to-netscape buffer t new-window))))



;;;
;   some other usefull functions
;  

(defun hm--html-remove-numeric-names ()
  "Remove the number in numbered links in the current buffer.
Eg: the string \"Name=3\". The function asks the user every time whether 
the number should be removed."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp "name=\"?[0-9]+\"?+[ \t]*" "")))

;;This should be extended in the future to use also other viewers.
(defun hm--html-view-www-package-docu ()
  "View the WWW documentation of the package."
  (interactive)
  (w3-fetch (concat "http://www.tnt.uni-hannover.de"
		    "/~muenkel/software/own/hm--html-menus/overview.html")))


;;;
;   Bug reporting
;

(defun hm--html-submit-bug-report ()
  "Submit via mail a bug report on hm--html-menus."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     hm--html-menus-package-maintainer
     (concat hm--html-menus-package-name 
	     " " 
	     hm--html-menus-package-version)
     (list 'emacs-version
	   'major-mode
	   'hm--html-automatic-changed-comment
	   'hm--html-automatic-created-comment
	   'hm--html-automatic-create-modified-line
	   'hm--html-automatic-expand-templates
	   'hm--html-automatic-update-modified-line
	   'hm--html-automatic-update-title-date
	   'hm--html-changed-comment-prefix
	   'hm--html-comment-infix
	   'hm--html-created-comment-prefix
	   'hm--html-expert
	   'hm--html-favorite-http-server-host-name
	   'hm--html-file-path-alist
	   'hm--html-ftp-hostname:port-alist
	   'hm--html-ftp-hostname:port-default
	   'hm--html-ftp-path-alist
	   'hm--html-gopher-anchor-alist
	   'hm--html-gopher-doctype-alist
	   'hm--html-gopher-doctype-default
	   'hm--html-gopher-hostname:port-alist
	   'hm--html-gopher-hostname:port-default
	   'hm--html-html-hostname:port-alist
	   'hm--html-html-hostname:port-default
	   'hm--html-html-path-alist
	   'hm--html-info-hostname:port-alist
	   'hm--html-info-hostname:port-default
	   'hm--html-info-path-alist
	   'hm--html-local-proggate-path-alist
	   'hm--html-mail-hostname:port-alist
	   'hm--html-mail-hostname:port-default
	   'hm--html-mail-path-alist
	   'hm--html-marc
	   'hm--html-menu-load-hook
	   'hm--html-modified-end-tag
	   'hm--html-modified-insert-before
	   'hm--html-modified-prefix
	   'hm--html-modified-start-tag
	   'hm--html-proggate-allowed-file
	   'hm--html-proggate-hostname:port-alist
	   'hm--html-proggate-hostname:port-default
	   'hm--html-server-side-include-command-alist
	   'hm--html-server-side-include-command-with-parameter-alist
	   'hm--html-signature-file
	   'hm--html-template-dir
	   'hm--html-url-alist
	   'hm--html-user-config-file
	   'hm--html-site-config-file
	   'hm--html-username
	   'hm--html-wais-hostname:port-alist
	   'hm--html-wais-hostname:port-default
	   'hm--html-wais-path-alist
	   'hm--html-wais-servername:port-alist
	   'hm--html-wais-servername:port-default
	   'html-document-previewer
	   'hm--html-region-mode
	   'html-sigusr1-signal-value
	   )
     nil
     nil
     "Decribe your Bug: "
     )))


;;;
;   hook adding functions
;

(if (adapt-xemacsp)
    (progn

      (add-hook 'zmacs-activate-region-hook
		'hm--html-switch-region-modes-on)

      (add-hook 'zmacs-deactivate-region-hook
		'hm--html-switch-region-modes-off)

      )

  (transient-mark-mode t)

  (add-hook 'activate-mark-hook 
	    'hm--html-switch-region-modes-on)
  
  (add-hook 'deactivate-mark-hook
	    'hm--html-switch-region-modes-off)

  )


;;;
;   Environment loading
;

(defun hm--html-load-config-files ()
  "Load the html configuration files.
First, the system config file (detemined by the environment variable
HTML_CONFIG_FILE; normaly hm--html-configuration.el(c)) is loaded.
At second a site config file is loaded, if the environment variable
HTML_SITE_CONFIG_FILE or the lisp variable `hm--html-site-config-file'
is set to such a file.
At least the user config file (determined by the environment variable
HTML_USER_CONFIG_FILE; normaly the file ~/.hm--html-configuration.el(c)).
If no HTML_CONFIG_FILE exists, then the file hm--html-configuration.el(c)
is searched in one of the lisp load path directories.
If no HTML_USER_CONFIG_FILE exists, then the variable 
`hm--html-user-config-file' is checked. If this variable is nil or the file
also doesn't exist, then the file ~/.hm--html-configuration.el(c) is used."
  (interactive)
  ;; at first the system config file
  (if (and (stringp (getenv "HTML_CONFIG_FILE"))
	   (file-exists-p
	    (expand-file-name
	     (getenv "HTML_CONFIG_FILE"))))
      (load-library (expand-file-name (getenv "HTML_CONFIG_FILE")))
    (load-library "hm--html-configuration"))

  ;; at second the site config file
  (if (and (stringp (getenv "HTML_SITE_CONFIG_FILE"))
	      (file-exists-p
	       (expand-file-name
		(getenv "HTML_SITE_CONFIG_FILE"))))
      (load-file (expand-file-name (getenv "HTML_SITE_CONFIG_FILE")))
    (when (and (boundp 'hm--html-site-config-file)
	       (stringp hm--html-site-config-file)
	       (file-exists-p (expand-file-name hm--html-site-config-file)))
      (load-file (expand-file-name hm--html-site-config-file))))
  
  ;; and now the user config file 
  (cond ((and (stringp (getenv "HTML_USER_CONFIG_FILE"))
	      (file-exists-p
	       (expand-file-name
		(getenv "HTML_USER_CONFIG_FILE"))))
	 (load-file (expand-file-name (getenv "HTML_USER_CONFIG_FILE"))))
	((and (boundp 'hm--html-user-config-file)
	      (stringp hm--html-user-config-file)
	      (file-exists-p (expand-file-name hm--html-user-config-file)))
	 (load-file (expand-file-name hm--html-user-config-file)))
	((file-exists-p (expand-file-name "~/.hm--html-configuration.elc"))
	 (load-file (expand-file-name "~/.hm--html-configuration.elc")))
	((file-exists-p (expand-file-name "~/.hm--html-configuration.el"))
	 (load-file (expand-file-name "~/.hm--html-configuration.el")))
	(t
	 (message (concat "WARNING: No HTML User Config File ! "
			  "Look at hm--html-load-config-files !")))
	)
  )
			  

;;; quotify href

(defvar hm--html-quotify-href-regexp
  "<[aA][ \t\n]+\\([nN][aA][mM][eE]=[a-zA-Z0-9]+[ \t\n]+\\)?[hH][rR][eE][fF]="
  "Regular expression used for searching hrefs.")

(defun hm--html-quotify-hrefs ()
  "Insert quotes around all HREF and NAME attribute value literals.

This remedies the problem with old HTML files that can't be processed
by SGML parsers. That is, changes <A HREF=foo> to <A HREF=\"foo\">.

Look also at the variable `hm--html-quotify-href-regexp'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while 
        (re-search-forward hm--html-quotify-href-regexp
			   (point-max)
			   t)
      (cond
       ((null (looking-at "\""))
        (insert "\"")
        (re-search-forward "[ \t\n>]" (point-max) t)
        (forward-char -1)
        (insert "\""))))))



(provide 'hm--html)
