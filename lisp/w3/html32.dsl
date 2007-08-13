<!doctype style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">

;; ######################################################################
;;
;; DSSSL style sheet for HTML 3.2 print output
;;
;; 1996.11.17
;;
;; Base version, August 1996: Jon Bosak, Sun Microsystems, based on work
;;    by Anders Berglund, EBT, with critical assistance from James Clark
;; TOC section and recto/verso page treatments based on models by James
;;    Clark, October 1996
;;
;; ######################################################################

;; Features in HTML 3.2 that are not implemented in the style sheet:
;;
;;    automatic table column widths
;;    % on width attribute for TABLE
;;    attributes on TH and TD: align, valign, rowspan, colspan
;;    attributes on TABLE: width, align, border, cellspacing, cellpadding
;;    start attribute on OL
;;    value attribute on LI
;;    noshade attribute on HR
;;
;;    See also "Non-Printing Elements" below
;;
;; Features in the style sheet that are not in HTML 3.2:
;;
;;    page headers that display the HEAD TITLE content
;;    page footers that display the page number
;;    autonumbering of heads and table captions
;;    support for named units (pt, pi, cm, mm) in size attributes
;;    automatic TOC generation

;; ============================== UNITS ================================

(define-unit pi (/ 1in 6))
(define-unit pt (/ 1in 72))
(define-unit px (/ 1in 96))

;; see below for definition of "em"


;; ============================ PARAMETERS ==============================

;; ........................... Basic "look" .............................

;; Visual acuity levels are "normal", "presbyopic", and 
;;   "large-type"; set the line following to choose the level

(define %visual-acuity% "normal")
;; (define %visual-acuity% "presbyopic")
;; (define %visual-acuity% "large-type")

(define %bf-size%
  (case %visual-acuity%
	(("normal") 11pt)
	(("presbyopic") 12pt)
	(("large-type") 24pt)))
(define %mf-size% (- %bf-size% 1pt))
(define %hf-size% %bf-size%)

(define-unit em %bf-size%)

(define %autonum-level% 6)       ;; zero disables autonumbering
(define %flushtext-headlevel%    ;; heads above this hang out on the left
  (if (equal? %visual-acuity% "large-type") 6 4))
(define %body-start-indent%      ;; sets the white space on the left
  (if (equal? %visual-acuity% "large-type") 0pi 4pi))
(define %toc?% #t)               ;; enables TOC after H1

;; ........................ Basic page geometry .........................

(define %page-width% 8.5in)
(define %page-height% 11in)

(define %left-right-margin% 6pi)
(define %top-margin%
  (if (equal? %visual-acuity% "large-type") 7.5pi 6pi))
(define %bottom-margin%
  (if (equal? %visual-acuity% "large-type") 7.5pi 6pi))
(define %header-margin%
  (if (equal? %visual-acuity% "large-type") 4.5pi 3pi))
(define %footer-margin% 3.5pi)

(define %text-width% (- %page-width% (* %left-right-margin% 2)))
(define %body-width% (- %text-width% %body-start-indent%))

;; .......................... Spacing factors ...........................

(define %para-sep% (/ %bf-size% 2.0))
(define %block-sep% (* %para-sep% 2.0))

(define %line-spacing-factor% 1.2)
(define %bf-line-spacing% (* %bf-size% %line-spacing-factor%))
(define %mf-line-spacing% (* %mf-size% %line-spacing-factor%))
(define %hf-line-spacing% (* %hf-size% %line-spacing-factor%))

(define %head-before-factor% 1.0)
(define %head-after-factor% 0.6)
(define %hsize-bump-factor% 1.2)

(define %ss-size-factor% 0.6)
(define %ss-shift-factor% 0.4)
(define %smaller-size-factor% 0.9)
(define %bullet-size-factor% 0.8)

;; ......................... Fonts and bullets ..........................

;; these font selections are for Windows 95

(define %title-font-family% "Arial")
(define %body-font-family% "Times New Roman")
(define %mono-font-family% "Courier New")
(define %dingbat-font-family% "Wingdings")

;; these "bullet strings" are a hack that is completely dependent on
;;    the Wingdings font family selected above; consider this a
;;    placeholder for suitable ISO 10646 characters

(define %disk-bullet% "l")
(define %circle-bullet% "¡")
(define %square-bullet% "o")

(define %bullet-size% (* %bf-size% %bullet-size-factor%))


;; ========================== COMMON FUNCTIONS ==========================

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; per ISO/IEC 10179
(define (node-list-reduce nl proc init)
  (if (node-list-empty? nl)
      init
      (node-list-reduce (node-list-rest nl)
                        proc
                        (proc init (node-list-first nl)))))

;; per ISO/IEC 10179
(define (node-list-length nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (+ result 1))
                    0))

(define if-front-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-front-page"))

(define if-first-page
  (external-procedure "UNREGISTERED::James Clark//Procedure::if-first-page"))

(define upperalpha
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define loweralpha
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define (char-downcase ch)
  (case ch
	((#\A) #\a) ((#\B) #\b) ((#\C) #\c) ((#\D) #\d) ((#\E) #\e)
	((#\F) #\f) ((#\G) #\g) ((#\H) #\h) ((#\I) #\i) ((#\J) #\j)
	((#\K) #\k) ((#\L) #\l) ((#\M) #\m) ((#\N) #\n) ((#\O) #\o)
	((#\P) #\p) ((#\Q) #\q) ((#\R) #\r) ((#\S) #\s) ((#\T) #\t)
	((#\U) #\u) ((#\V) #\v) ((#\W) #\w) ((#\X) #\x) ((#\Y) #\y)
	((#\Z) #\z) (else ch)))

(define (LOCASE slist)
  (if (null? slist)
      '()
      (cons (char-downcase (car slist)) (LOCASE (cdr slist)))))

(define (STR2LIST s)
  (let ((len (string-length s)))
    (let loop ((i 0) (ln len))
	 (if (= i len)
	     '()
	     (cons (string-ref s i) (loop (+ i 1) ln))))))

(define (STRING-DOWNCASE s)
  (apply string (LOCASE (STR2LIST s))))

(define (UNAME-START-INDEX u last)
  (let ((c (string-ref u last)))
    (if (or (member c upperalpha) (member c loweralpha))
	(if (= last 0)
	    0
	    (UNAME-START-INDEX u (- last 1)))
        (+ last 1))))

(define (PARSEDUNIT u) ;; this doesn't deal with "%" yet
 (if (string? u)
  (let ((strlen (string-length u)))
    (if (> strlen 2)
	(let ((u-s-i (UNAME-START-INDEX u (- strlen 1))))
	  (if (= u-s-i 0) ;; there's no number here
	      1pi         ;; so return something that might work
	      (if (= u-s-i strlen)           ;; there's no unit name here
		  (* (string->number u) 1px) ;; so default to pixels (3.2)
		  (let* ((unum (string->number
			       (substring u 0 u-s-i)))
			 (uname (STRING-DOWNCASE
				 (substring u u-s-i strlen))))
		    (case uname
			  (("mm") (* unum 1mm))
			  (("cm") (* unum 1cm))
			  (("in") (* unum 1in))
			  (("pi") (* unum 1pi))
			  (("pc") (* unum 1pi))
			  (("pt") (* unum 1pt))
			  (("px") (* unum 1px))
			  (("barleycorn") (* unum 2pi)) ;; extensible!
			  (else
			   (cond 
			    ((number? unum)
			     (* unum 1px))
			    ((number? (string->number u))
			     (* (string->number u) 1px))
				 (else u))))))))
        (if (number? (string->number u))
	    (* (string->number u) 1px)
	    1pi)))
    1pi))

(define (INLIST?)
  (or
    (have-ancestor? "OL")
    (have-ancestor? "UL")
    (have-ancestor? "DIR")
    (have-ancestor? "MENU")
    (have-ancestor? "DL")))

(define (INHEAD?)
  (or
    (have-ancestor? "H1")
    (have-ancestor? "H2")
    (have-ancestor? "H3")
    (have-ancestor? "H4")
    (have-ancestor? "H5")
    (have-ancestor? "H6")))

(define (HSIZE n)
  (* %bf-size%
    (expt %hsize-bump-factor% n)))

(define (OLSTEP)
  (case (modulo (length (hierarchical-number-recursive "OL")) 4)
	((1) 1.2em)
	((2) 1.2em)
	((3) 1.6em)
	((0) 1.4em)))

(define (ULSTEP) 1em)

(define (PQUAD)
  (case (attribute-string "align")
	(("LEFT") 'start)
	(("CENTER") 'center)
	(("RIGHT") 'end)
	(else (inherited-quadding))))

(define (HQUAD)
  (cond
    ((string? (attribute-string "align")) (PQUAD))
    ((have-ancestor? "CENTER") 'center)
    ((have-ancestor? "DIV") (inherited-quadding))
    (else 'start)))

(define (BULLSTR sty)
  (case sty
	(("circle") %circle-bullet%)
	(("square") %square-bullet%)
	(else %disk-bullet%)))


;; ======================= NON-PRINTING ELEMENTS ========================

;; Note that HEAD includes TITLE, ISINDEX, BASE, META, STYLE,
;;   SCRIPT, and LINK as possible children

(element HEAD (empty-sosofo))
(element FORM (empty-sosofo))
(element APPLET (empty-sosofo))
(element PARAM (empty-sosofo))
(element TEXTFLOW (empty-sosofo))
(element MAP (empty-sosofo))
(element AREA (empty-sosofo))


;; ========================== TABLE OF CONTENTS =========================

;; Container elements in which to look for headings
(define %clist% '("BODY" "DIV" "CENTER" "BLOCKQUOTE" "FORM"))

(mode toc
  (element h1 (empty-sosofo))
  (element h2 ($toc-entry$ 2))
  (element h3 ($toc-entry$ 3))
  (element h4 ($toc-entry$ 4))
  (element h5 ($toc-entry$ 5))
  (element h6 ($toc-entry$ 6))
  (default (apply process-matching-children
		  (append %hlist% %clist%)))
)

(define %toc-indent% 1em)

(define ($toc-entry$ level)
  (make paragraph
	use: para-style
	start-indent: (+ %body-start-indent%
			 (* %toc-indent% (+ 1 level)))
	first-line-start-indent: (* -3 %toc-indent%)
	quadding: 'start
	(literal (NUMLABEL level))
	(make link
	      destination: (current-node-address)
	      (with-mode #f (process-children-trim)))
	(make leader (literal "."))
	(current-node-page-number-sosofo)))

(define (MAKEBODYRULE)
  (make rule
	orientation: 'horizontal
	space-before: (* 2 %block-sep%)
	space-after: (* 2 %block-sep%)
	line-thickness: 1pt
	length: %body-width%
	start-indent: %body-start-indent%
	display-alignment: 'start))

(define (MAKETOC)
  (if %toc?%
      (sosofo-append
       (MAKEBODYRULE)
       (make paragraph
	     font-family-name: %title-font-family%
	     font-weight: 'bold
	     font-posture: 'upright
	     font-size: (HSIZE 2)
	     line-spacing: (* (HSIZE 2) %line-spacing-factor%)
	     space-before: (* (HSIZE 2) %head-before-factor%)
	     space-after: (* (HSIZE 2) %head-after-factor%)
	     start-indent: %body-start-indent%
	     quadding: 'start
	     keep-with-next?: #t
	     (literal "Table of Contents"))
       (with-mode toc
		  (process-node-list (ancestor "BODY")))
       (MAKEBODYRULE))
      (empty-sosofo)))

;; ============================ TOP LEVEL ===============================

(define page-style
  (style
       page-width: %page-width%
       page-height: %page-height%
       left-margin: %left-right-margin%
       right-margin: %left-right-margin%
       top-margin: %top-margin%
       bottom-margin: %bottom-margin%
       header-margin: %header-margin%
       footer-margin: %footer-margin%
       font-family-name: %body-font-family%
       font-size: %bf-size%
       line-spacing: %bf-line-spacing%))

(element HTML
 (let ((page-footer
	(make sequence
	      font-size: %hf-size%
	      line-spacing: %hf-line-spacing%
	      font-posture: 'italic
	      (literal "Page ")
	      (page-number-sosofo)))
       (page-header
	(make sequence
	      font-size: %hf-size%
	      line-spacing: %hf-line-spacing%
	      font-posture: 'italic
	      (process-first-descendant "TITLE"))))
   (make simple-page-sequence
	 use: page-style
	 left-header: (if-first-page 
		       (empty-sosofo)
		       (if-front-page (empty-sosofo) page-header))
	 right-header: (if-first-page
			(empty-sosofo)
			(if-front-page page-header (empty-sosofo)))
	 left-footer: (if-first-page
		       (empty-sosofo)
		       (if-front-page (empty-sosofo) page-footer))
	 right-footer: (if-first-page
			(empty-sosofo)
			(if-front-page page-footer (empty-sosofo)))
	 input-whitespace-treatment: 'collapse
	 quadding: 'justify
	 (process-children-trim))))

(element BODY (process-children-trim))

;; ========================== BLOCK ELEMENTS ============================

;; ............................ Generic DIV .............................

(element DIV
 (let ((align (attribute-string "align")))
  (make display-group
	quadding:
	  (case align
		(("LEFT") 'start)
		(("CENTER") 'center)
		(("RIGHT") 'end)
		(else 'justify))
	(process-children-trim))))

(element CENTER
 (make display-group
       quadding: 'center
       (process-children-trim)))


;; .............................. Headings ..............................

(define %hlist% '("H1" "H2" "H3" "H4" "H5" "H6"))

(define (NUMLABEL hlvl)
  (let ((enl (element-number-list
	      (reverse (list-tail (reverse %hlist%) (- 6 hlvl))))))
    (let loop ((idx 1))
	 (if (or (= idx %autonum-level%) (= idx hlvl))
	     (if (= idx 2) ". " " ")
	     (let ((thisnum (list-ref enl idx)))
	       (string-append
		 (if (> idx 1) "." "")
		 (format-number thisnum "1")
		 (loop (+ idx 1))))))))

(define ($heading$ headlevel)
  (let ((headsize (if (= headlevel 6) 0 (- 5 headlevel))))
    (make paragraph
	  font-family-name: %title-font-family%
	  font-weight: (if (< headlevel 6) 'bold 'medium)
	  font-posture: (if (< headlevel 6) 'upright 'italic)
	  font-size: (HSIZE headsize)
	  line-spacing: (* (HSIZE headsize) %line-spacing-factor%)
	  space-before: (* (HSIZE headsize) %head-before-factor%)
	  space-after: (if (and %toc?% (= headlevel 1))
				4em ;; space if H1 before TOC
				(* (HSIZE headsize) %head-after-factor%))
	  start-indent:
	    (if (< headlevel %flushtext-headlevel%)
	        0pt
		%body-start-indent%)
	  quadding: (HQUAD)
	  keep-with-next?: #t
	  break-before: (if (and 
			     %toc?%
			     (= headlevel 2)
			     (= (child-number) 1))
			    'page #f) ;; if TOC on, break before first H2
	  (literal
	   (if (and (<= headlevel %autonum-level%) (> headlevel 1))
	       (NUMLABEL headlevel)
	       (string-append "")))
	  (process-children-trim))))

(element H1 
  (sosofo-append
    ($heading$ 1)
    (MAKETOC)))

(element H2 ($heading$ 2))
(element H3 ($heading$ 3))
(element H4 ($heading$ 4))
(element H5 ($heading$ 5))
(element H6 ($heading$ 6))


;; ............................ Paragraphs ..............................

(define para-style
  (style
   font-size: %bf-size%
   font-weight: 'medium
   font-posture: 'upright
   font-family-name: %body-font-family%
   line-spacing: %bf-line-spacing%))

(element P
 (make paragraph
       use: para-style
       space-before: %para-sep%
       start-indent: %body-start-indent%
       quadding: (PQUAD)
       (process-children-trim)))

(element ADDRESS
  (make paragraph
	use: para-style
	font-posture: 'italic
	space-before: %para-sep%
	start-indent: %body-start-indent%
	(process-children-trim)))

(element BLOCKQUOTE
  (make paragraph
	font-size: (- %bf-size% 1pt)
	line-spacing: (- %bf-line-spacing% 1pt)
	space-before: %para-sep%
	start-indent: (+ %body-start-indent% 1em)
	end-indent: 1em
	(process-children-trim)))

(define ($monopara$)
  (make paragraph
	use: para-style
	space-before: %para-sep%
	start-indent: %body-start-indent%
        lines: 'asis
	font-family-name: %mono-font-family%
	font-size: %mf-size%
	input-whitespace-treatment: 'preserve
	quadding: 'start
        (process-children-trim)))

(element PRE ($monopara$))
(element XMP ($monopara$))
(element LISTING ($monopara$))
(element PLAINTEXT ($monopara$))

(element BR
  (make display-group
    (empty-sosofo)))


;; ................... Lists: UL, OL, DIR, MENU, DL .....................

(define ($list-container$)
 (make display-group
       space-before: (if (INLIST?) %para-sep% %block-sep%)
       space-after:  (if (INLIST?) %para-sep% %block-sep%)
       start-indent: (if (INLIST?) 
			 (inherited-start-indent)
		         %body-start-indent%)))

(define ($li-para$)
  (make paragraph
	use: para-style
	start-indent: (+ (inherited-start-indent) (OLSTEP))
	first-line-start-indent: (- (OLSTEP))
	(process-children-trim)))

(element UL ($list-container$))

(element (UL LI)
  (let ((isnested (> (length (hierarchical-number-recursive "UL")) 1)))
    (make paragraph
	  use: para-style
	  space-before:
	   (if (attribute-string "compact" (ancestor "UL")) 0pt %para-sep%)
	  start-indent: (+ (inherited-start-indent) (ULSTEP))
	  first-line-start-indent: (- (ULSTEP))
	  (make line-field
		font-family-name: %dingbat-font-family%
		font-size: (if isnested
			       (* %bullet-size% %bullet-size-factor%)
			       %bullet-size%)
		field-width: (ULSTEP)
		(literal
		  (let
		      ((litype
			(attribute-string "type"))
		       (ultype
			(attribute-string "type" (ancestor "UL"))))
		    (cond
		      ((string? litype) (BULLSTR (STRING-DOWNCASE litype)))
		      ((string? ultype) (BULLSTR (STRING-DOWNCASE ultype)))
		      (else %disk-bullet%)))))
	  (process-children-trim))))

(element (UL LI P) ($li-para$))

(element OL ($list-container$))

(element (OL LI)
 (make paragraph
       use: para-style
       space-before:
         (if (attribute-string "compact" (ancestor "OL")) 0pt %para-sep%)
       start-indent: (+ (inherited-start-indent) (OLSTEP))
       first-line-start-indent: (- (OLSTEP))
       (make line-field
	     field-width: (OLSTEP)
	     (literal
 	       (case (modulo
		       (length (hierarchical-number-recursive "OL")) 4)
		     ((1) (string-append 
			   (format-number (child-number) "1") "."))
		     ((2) (string-append
			   (format-number (child-number) "a") "."))
		     ((3) (string-append
			   "(" (format-number (child-number) "i") ")"))
		     ((0) (string-append
			   "(" (format-number (child-number) "a") ")")))))
       (process-children-trim)))

(element (OL LI P) ($li-para$))

;; Note that DIR cannot properly have block children.  Here DIR is
;;   interpreted as an unmarked list without extra vertical
;;   spacing.

(element DIR ($list-container$))

(element (DIR LI)
 (make paragraph
       use: para-style
       start-indent: (+ (inherited-start-indent) (* 2.0 (ULSTEP)))
       first-line-start-indent: (- (ULSTEP))
       (process-children-trim)))

;; Note that MENU cannot properly have block children.  Here MENU is
;;   interpreted as a small-bulleted list with no extra vertical
;;   spacing.

(element MENU ($list-container$))

(element (MENU LI)
 (make paragraph
       use: para-style
       start-indent: (+ (inherited-start-indent) (ULSTEP))
       first-line-start-indent: (- (ULSTEP))
       (make line-field
	     font-family-name: %dingbat-font-family%
	     font-size: %bullet-size%
	     field-width: (ULSTEP)
	     (literal %disk-bullet%))
       (process-children-trim)))

;; This treatment of DLs doesn't apply a "compact" attribute set at one
;;    level to any nested DLs.  To change this behavior so that nested
;;    DLs inherit the "compact" attribute from an ancestor DL, substitute
;;    "inherited-attribute-string" for "attribute-string" in the
;;    construction rules for DT and DD.


(element DL
  (make display-group
	space-before: (if (INLIST?) %para-sep% %block-sep%)
	space-after:  (if (INLIST?) %para-sep% %block-sep%)
	start-indent: (if (INLIST?)
			  (+ (inherited-start-indent) 2em)
			  (+ %body-start-indent% 2em))
	(make paragraph)))

(element DT
  (let ((compact (attribute-string "compact" (ancestor "DL"))))
    (if compact
	(make line-field
	      field-width: 3em
	      (process-children-trim))
        (make paragraph
	      use: para-style
	      space-before: %para-sep%
	      first-line-start-indent: -1em
	      (process-children-trim)))))

(element DD
  (let ((compact (attribute-string "compact" (ancestor "DL"))))
    (if compact
	(sosofo-append
	  (process-children-trim)
	  (make paragraph-break))
        (make paragraph
	      use: para-style
	      start-indent: (+ (inherited-start-indent) 2em)
	      (process-children-trim)))))


;; ========================== INLINE ELEMENTS ===========================

(define ($bold-seq$)
  (make sequence
    font-weight: 'bold
    (process-children-trim)))

(element B ($bold-seq$))
(element EM ($bold-seq$))
(element STRONG ($bold-seq$))

;; ------------

(define ($italic-seq$)
  (make sequence
    font-posture: 'italic
    (process-children-trim)))

(element I ($italic-seq$))
(element CITE ($italic-seq$))
(element VAR ($italic-seq$))

;; ------------

(define ($bold-italic-seq$)
  (make sequence
    font-weight: 'bold
    font-posture: 'italic
    (process-children-trim)))

(element DFN ($bold-italic-seq$))
(element A 
  (if (INHEAD?)
      (process-children-trim)
      ($bold-italic-seq$)))

;; ------------

(define ($mono-seq$)
  (make sequence
	font-family-name: %mono-font-family%
	font-size: %mf-size%
	(process-children-trim)))

(element TT ($mono-seq$))
(element CODE ($mono-seq$))
(element KBD ($mono-seq$))
(element SAMP ($mono-seq$))

;; ------------

(define ($score-seq$ stype)
  (make score
	type: stype
	(process-children-trim)))

(element STRIKE ($score-seq$ 'through))
(element U      ($score-seq$ 'after))

;; ------------

(define ($ss-seq$ plus-or-minus)
  (make sequence
	font-size:
	  (* (inherited-font-size) %ss-size-factor%)
	position-point-shift:
	  (plus-or-minus (* (inherited-font-size) %ss-shift-factor%))
	(process-children-trim)))

(element SUP ($ss-seq$ +))
(element SUB ($ss-seq$ -))

;; ------------

(define ($bs-seq$ div-or-mult)
  (make sequence
	font-size:
	  (div-or-mult (inherited-font-size) %smaller-size-factor%)
	line-spacing:
	  (div-or-mult (inherited-line-spacing) %smaller-size-factor%)))

(element BIG ($bs-seq$ /))
(element SMALL ($bs-seq$ *))

;; ------------

(element FONT
 (let ((fsize (attribute-string "SIZE")))
  (make sequence
    	font-size:
	  (if fsize (PARSEDUNIT fsize) (inherited-font-size)))))


;; ============================== RULES =================================

(element HR
 (let ((align (attribute-string "ALIGN"))
       (noshade (attribute-string "NOSHADE"))
       (size (attribute-string "SIZE"))
       (width (attribute-string "WIDTH")))
  (make rule
	orientation: 'horizontal
	space-before: %block-sep%
	space-after: %block-sep%
	line-thickness: (if size (PARSEDUNIT size) 1pt)
	length: (if width (PARSEDUNIT width) %body-width%)
	display-alignment:
	  (case align
		(("LEFT") 'start)
		(("CENTER") 'center)
		(("RIGHT") 'end)
		(else 'end)))))


;; ============================= GRAPHICS ===============================

;; Note that DSSSL does not currently support text flowed around an
;;   object, so the action of the ALIGN attribute is merely to shift the
;;   image to the left or right.  An extension to add runarounds to DSSSL
;;   has been proposed and should be incorporated here when it becomes
;;   final.

(element IMG
  (make external-graphic
	entity-system-id: (attribute-string "src")
	display?: #t
	space-before: 1em
	space-after: 1em
	display-alignment:
	  (case (attribute-string "align")
		(("LEFT") 'start)
		(("RIGHT") 'end)
		(else 'center))))

;; ============================== TABLES ================================

(element TABLE
;; number-of-columns is for future use
  (let ((number-of-columns
	 (node-list-reduce (node-list-rest (children (current-node)))
			   (lambda (cols nd)
			     (max cols
				  (node-list-length (children nd))))
			   0)))
  (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	start-indent: %body-start-indent%
;; for debugging:
;;	(make paragraph
;;	      (literal
;;	       (string-append
;;		"Number of columns: "
;;		(number->string number-of-columns))))
	(with-mode table-caption-mode (process-first-descendant "CAPTION"))
	(make table
	      (process-children)))))

(mode table-caption-mode
  (element CAPTION
	   (make paragraph
		 use: para-style
		 font-weight: 'bold
		 space-before: %block-sep%
		 space-after: %para-sep%
		 start-indent: (inherited-start-indent)
		 (literal
		  (string-append
		   "Table "
		   (format-number
		    (element-number) "1") ". "))
		 (process-children-trim))))

(element CAPTION (empty-sosofo)) ; don't show caption inside the table

(element TR
  (make table-row
	(process-children-trim)))

(element TH
  (make table-cell
	n-rows-spanned: (string->number (attribute-string "COLSPAN"))
	(make paragraph
	      font-weight: 'bold
	      space-before: 0.25em
	      space-after: 0.25em
	      start-indent: 0.25em
	      end-indent: 0.25em
	      quadding: 'start
	      (process-children-trim))))

(element TD
  (make table-cell
	n-rows-spanned: (string->number (attribute-string "COLSPAN"))
	(make paragraph
	      space-before: 0.25em
	      space-after: 0.25em
	      start-indent: 0.25em
	      end-indent: 0.25em
	      quadding: 'start
	      (process-children-trim))))
