;; gnats administration code
;; display the pr's in a buffer, 
;; dired-style commands to edit & view them.

;; this version is known to work in XEmacs.

;; author: Roger Hayes, roger.hayes@sun.com

;; copyright: You are welcome to use this software as you see fit.
;; Neither the author nor his employer make any representation about
;; the suitability of this software for any purpose whatsoever.

(defconst gnats-admin-copyright 
"Copyright (c) 1996 Roger Hayes.

Permission to use, copy, modify and distribute this software and
documentation for any purpose and without fee is hereby granted in
perpetuity, provided that this COPYRIGHT AND LICENSE NOTICE appears in
its entirety in all copies of the software and supporting
documentation.

The names of the author or Sun Microsystems, Inc. shall not be used in
advertising or publicity pertaining to distribution of the software
and documentation without specific, written prior permission.

ANY USE OF THE SOFTWARE AND DOCUMENTATION SHALL BE GOVERNED BY
CALIFORNIA LAW.  THE AUTHOR AND SUN MICROSYSTEMS, INC. MAKE NO
REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE SOFTWARE OR
DOCUMENTATION FOR ANY PURPOSE.  THEY ARE PROVIDED *AS IS* WITHOUT
EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  THE AUTHOR AND SUN
MICROSYSTEMS, INC. SEVERALLY AND INDIVIDUALLY DISCLAIM ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE AND DOCUMENTATION, INCLUDING THE
WARRANTIES OF MERCHANTABILITY, DESIGN, FITNESS FOR A PARTICULAR
PURPOSE AND NON-INFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL
THE AUTHOR OR SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL,
INDIRECT, INCIDENTAL OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA, OR PROFITS, WHETHER IN
ACTION ARISING OUT OF CONTRACT, NEGLIGENCE, PRODUCT LIABILITY, OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE OR DOCUMENTATION."

"Copyright and disclaimer notice")

;; magic words are highlighted using font-lock

;; data structures: a pr is represented as an alist
;; the list of known pr's is represented as a vector
;; pr references are often done by number
;; (pr N) takes either a number, and returns the pr assoc list,
;; or the pr assoc list, returning the same.
;; regions are tagged with the number of the pr; the indirection lets
;; updates to pr's happen without disturbing the region.

(defvar pr-list nil "Vector of parsed problem reports.")

(require 'cl)

;;; (require 'match-string)
;;; inline match-string
(if (not (fboundp 'match-string))
    (defun match-string (n &optional target)
      "Return the text of the NTH match in optional TARGET."
      (let* ((m-data (match-data))
	     (idx  	(* 2 n))
	     (m-beg (elt m-data idx))
	     (m-end (elt m-data (1+ idx))))
	(cond 
	 ((markerp m-beg)
	  (buffer-substring m-beg m-end))
	 ((integerp m-beg)
	  (substring target m-beg m-end))
	 (t
	  (error "Bad argument N to match-string"))))))

;;; (require 'edit-expr)
;; edit-expr -- pop up a buffer to edit an expression
(if (not (fboundp 'edit-expr))
    (defun edit-expr (e &optional explain)
      "Pop up a buffer to edit the EXPRESSION; return the edited value.
Buffer gets optional EXPLANATION."
      (with-output-to-temp-buffer "*expr-buffer*"
	(let ((buffer standard-output)
	      (val nil)
	      emark)
	  
	  (save-excursion
	    (pop-to-buffer (buffer-name buffer))
	    (emacs-lisp-mode)
	    (delete-region (point-min) (point-max))
	    (insert (or explain ";; Edit this value"))
	    (insert "\n")
	    (setq emark (point-marker))
	    (prin1 e) 
	    (goto-char emark)
	    (message "recursive edit -- M-C-c when done")
	    (recursive-edit)
	    (goto-char emark)
	    (setq val (read buffer)))
	  val))))
    
;; (require 'regret)    ;; creates regression tests in my environment [rh]

(provide 'gnats-admin)

;; add stuff for font-lock; harmless if you don't use font-lock.
;; beware -- font-lock uses the first match; hence longer words must
;; precede shorter words, if they both match.
(defconst gnats-admin::font-lock-keywords
  '(;; severity
    ("non-critical" . non-critical)
    ("critical" . critical)
    ("serious" .  serious)

    ;; priority
    ("high" . high)
    ("medium" . medium)
    ("low" . low)

    ;; state
    ("open" . open)
    ("analyzed" . analyzed)
    ("suspended" . suspended)
    ("feedback" . feedback)
    ("closed\\*" . closed*)
    ("closed\\?" . closed?)
    ("closed" . closed)

    ;; class
    ("sw-bug" . sw-bug) 
    ("doc-bug" . doc-bug)
    ("support" . support)
    ("change-request" . change-request) 
    ("mistaken" . mistaken) 
    ("duplicate" . duplicate)))

(defvar gnats-admin::popup-menu
  '("Gnats-Admin"
    ["Edit"	'gnats-admin:pr-edit t]
    ["View"	'gnats-admin:pr-view t]
    )
  "Local popup menu.")

(defvar gnats-admin-query-hook nil
  "Hooklist for post-query processing in gnats-admin mode.")
(defvar gnats-admin:query-list nil
  "Results of one query -- may be altered by query hook to change results of query.")
(defvar gnats-admin-refresh-hook nil
  "Hooklist for post-refresh processing in gnats-admin mode.
Run after a refresh -- gnats-admin::dirty-list may contain list of re-read pr numbers")

(defvar gnats-admin::selector nil
  "Function of one argument that governs if a PR should be diplayed.")

(add-hook 'gnats-admin-refresh-hook
	  (function (lambda ()
		      (font-lock-fontify-buffer))))

;;;
;; first, how do we get & parse the pr's?
(defun gnats-admin::run-query ()
  "Run a default query, setting pr-list to the result."
  (setq pr-list (apply 'vector (gnats-admin::query)))
  (length pr-list))

(defun trim (s)
  "trim the leading & trailing blanks from a string"
  (if (string-match "^\\s-*\\(\\S-.*\\S-\\|\\S-\\)\\s-*$" s)
      (substring s (match-beginning 1) (match-end 1))
    "")
  )

;; parse one pr -- moves point.
(defun gnats-admin::parse-pr ()
  "parse one pr, moving point"
  (let ((pr nil))
    (let ((flv (make-vector 13 nil))
	  (lim 0)
	  (bol 0)
	  (index 0))
      (end-of-line)
      (setq lim (point))
      (beginning-of-line)
      (setq bol (point))
      (while (and (< (point) lim)
		  (re-search-forward "\\([^|]*\\)|" lim t))
	(aset flv index (trim (match-string 1)))
	(setq index (1+ index)))
      (if (not (= index 13))
	  (error "Bad PR inquiry: %s" (buffer-substring bol lim)))
      (setq pr (gnats-admin::vec->pr flv)))
    (if (not (bolp))
	(forward-line 1))
    pr))

(defun gnats-admin::query (&rest args)
  (let ((prl (list))
	(buf (get-buffer-create "**gnats-query*")))
    (save-excursion
      (set-buffer buf)
      (delete-region (point-min) (point-max))
      (message "Running query")
      (setq args
	    (mapcar (function (lambda (x) (format "%s" x))) args))
      (apply 'call-process "query-pr" nil t nil "--sql" args)
      (message "Query completed")
      ;; now parse the output
      (goto-char (point-min))
      (while (not (eobp))
	(setq prl (cons (gnats-admin::parse-pr) prl)))
      )
    (message "Result parsed")
    ;; lots of stuff to apply the proper hook
    (setq gnats-admin:query-list prl)
    (run-hooks 'gnats-admin-query-hook)
    (setq prl gnats-admin:query-list)
    (setq gnats-admin:query-list nil)
    (nreverse prl)))

;; 
;; 
(defun gnats-admin::vec->pr (v)
  "massage a 13-element vector into the internal pr representation.
fields are as described in query-pr documentation."
  (if (not (and (vectorp v)
		(= (length v) 13)))
      (error "Not a valid PR intermediate form"))

  ;;; 0 - pr number
  (aset v 0 (read (aref v 0)))
  ;;; 1 - category
  (aset v 1 (read (aref v 1)))
  ;;; 2 - synopsis
  ; leave as string
  ;;; 3 - confidential
  (aset v 3 (if (equal "no" (aref v 3)) nil (aref v 3)))
  ;;; 4 - severity
  (let ((num (read (aref v 4))))
    (aset v 4 (aref 
	       [null critical serious non-critical] 
	       num)))
  ;;; 5 - priority
  (let ((num (read (aref v 5))))
    (aset v 5 (aref 
	       [null high medium low]
	       num)))
  ;;; 6 - responsible
  ; leave as string
  ;;; 7 - state
  (let ((num (read (aref v 7))))
    (aset v 7 (aref
	       [null open analyzed suspended feedback closed]
	       num)))
  ;;; 8 - class
  (let ((num (read (aref v 8))))
    (aset v 8 (aref
	       [null sw-bug doc-bug support change-request mistaken duplicate]
	       num)))
  ;;; 9 - submitter-id
  ; leave as string
  ;;; 10 - arrival-date
  ; leave as string
  ;;; 11 - originator
  ; leave as string
  ;;; 12 - release
  ; leave as string

  ;; the fields of v have been transformed; now map them into an alist
  (do
      ((vx 0 (1+ vx))			; v index
       (an				; field names (in order!)
	(gnats-admin::pr-field-names)
	(cdr an))
       (al nil)				; assoc list
       )
      ((null an) (nreverse al))		; <- here's where the result comes from
    (setq al (cons (cons (car an) (aref v vx)) al))
    ))

(defun gnats-admin::pr-get (pr field)
  "Get, from PR, value of slot named FIELD (a symbol)."
  (let ((p (assq field (gnats-admin:pr pr))))
    (if p
	(cdr p)
      nil)))

(defun nset-assq (alist key val)
  "destructively set key'v association in the alist to val.  returns the original
list, unless it was null"
  (let ((p (assq key alist)))
    (if p
	(progn 
	  (setcdr p val)
	  alist)
      (nconc alist (list (cons key val))))))

(defun gnats-admin::pr-set! (pr field val)
  "Set, in PR, slot named FIELD to VAL.  Slot name is a symbol."
  (nset-assq pr field val))

;; fast version of field name->index mapper
;; also tests if a symbol is a field name present in sql report.
(defun gnats-admin::pr-field-index (feild)
  (case feild
    (Number 0)
    (Category 1)
    (Synopsis 2)
    (Confidential  3)
    (Severity  4)
    (Priority 5)
    (Responsible 6)
    (State 7)
    (Class 8)
    (Submitter-Id 9)
    (Arrival-Date 10)
    (Originator 11) 
    (Release 12)))

;; next is for completing-read
;; order must be the same as indices
;; content is (name index type width)
;; width is field width, not counting space
(defconst gnats-admin::pr-field-alist
  '(("Number" 0 integer 3)
    ("Category" 1 symbol 15)
    ("Synopsis" 2 string 80)
    ("Confidential"  3 boolean 1)
    ("Severity"  4 symbol 12)
    ("Priority" 5 symbol 6)
    ("Responsible" 6 string 7)
    ("State" 7 symbol 8)
    ("Class" 8 symbol 14)
    ("Submitter-Id" 9 string 7)
    ("Arrival-Date" 10 string 14)
    ("Originator" 11 string 32) 
    ("Release" 12 string 48))
  "Alist that maps field-name->(name index type width)")

(defun gnats-admin::pr-field-names ()
  "List of symbols that are field keys in pr.  Must be in order."
  (mapcar (function (lambda (pr) (intern (car pr))))
	  gnats-admin::pr-field-alist))

;; format control template for PR display
(defconst gnats-admin::pr-long-format
  '((4 Category Class) (35 Priority Severity) (60 Responsible) (70 State ) nl
    4 "Synopsis:" Synopsis 
    ))
(defconst gnats-admin::pr-short-format 
  '((4 Category Class) (35 Priority Severity) (60 Responsible) (70 State )))

(defvar gnats-admin::pr-format gnats-admin::pr-short-format
  "Format list for printing a pr.")

;; hook that sets extent etc for Lucid emacs
(defun gnats-admin::pr-display-hook (b e pr buf)
  "Set extent around pr."
  (let ((ext (make-extent b e buf)))
    (set-extent-layout ext 'outside-margin)
    (set-extent-property ext 'pr pr)
    (set-extent-property ext 'start-open t)
    (set-extent-property ext 'end-open t)
    (set-extent-property ext 'highlight t)))

;; gnats uses one face for each element of the enumerated fields,
;; to give maximum flexibility in display.

;;; symbol->(face foreground background) alist
(defvar gnats-admin::face-color
  '((critical "firebrick" nil)
    (serious "goldenrod" nil)
    (non-critical "blue3" nil)

    (high "firebrick" nil) 
    (medium "goldenrod" nil) 
    (low "blue3" nil)

    (open "firebrick" nil) 
    (analyzed "goldenrod" nil) 
    (suspended "turquoise" nil) 
    (feedback "blue3" nil) 
    (closed "ForestGreen" nil)
    (closed* "HotPink" nil)
    (closed? "blue3" nil)

    (sw-bug nil nil) 
    (doc-bug nil nil) 
    (support nil nil) 
    (change-request nil nil) 
    (mistaken nil nil) 
    (duplicate nil nil))
  "Alist of font properties")

(defun gnats-admin::field-display (pr fld buf)
  "Display value field on specified stream."
  (let
      ((fv (gnats-admin::pr-get pr fld)))
    (princ fv buf)))

(defun gnats-admin::pr-print-func (f pr buf)
  "Printer for pr.  Depends on free variable pr-did-indent."
  (cond
   ((eq 'nl f)
    (princ "\n " buf)
    (setq pr-did-indent t))
   ((listp f)
    (do
	((fmt f (cdr fmt)))
	((null fmt))
      (gnats-admin::pr-print-func (car fmt) pr buf)))
   ((numberp f)
    (indent-to-column f 1)
    (setq pr-did-indent t))
   ((symbolp f)
    (if (not pr-did-indent)
	(princ " " buf))
    (gnats-admin::field-display pr f buf)
    (setq pr-did-indent nil))
   ((stringp f)
    (if (not pr-did-indent)
	(princ " " buf))
    (princ f buf)
    (setq pr-did-indent nil))
   )
  t)
  
(defun gnats-admin::display-pr (pr &optional buf)
  ;; always print the number first
  (if (not buf)
      (setq buf (current-buffer)))
  (let 
      ((b (point))
       (buffer-read-only nil)
       (pr-did-indent nil))
    (princ (gnats-admin::pr-number pr) buf)
    ;; now print according to the pr-format list
    (do
	((fmt gnats-admin::pr-format (cdr fmt)))
	((null fmt))
      (gnats-admin::pr-print-func (car fmt) pr buf))
    (gnats-admin::pr-display-hook 
     b 
     (point) 
     (gnats-admin::pr-number pr)
     buf)
    (newline)))

(defun gnats-admin::pr-buffer-extent (pr)
  "Find the extent for this PR."
  (extent-at (1+ (gnats-admin::pr-buffer-begin pr))
	     (gnats-admin::pr-buffer pr)
	     'pr))
  
(defun gnats-admin::pr-reread (pr)
  "Rerun query for one PR in the pr-list."
  (let
      ((num (gnats-admin::pr-number pr)))
    (let
	((repl (gnats-admin::query num)))
      (if (and (listp repl)
	       (= (length repl) 1)
	       (= (gnats-admin::pr-get (car repl) 'Number) num))
	  (gnats-admin::pr-replace! num (car repl))
	(error "Query failed for pr %s" num)))
    ))

(defun gnats-admin::selection (loprs)
  "Return the elements of LOPRS (list of PR's) which satify PREDicate."
  (let ((pred gnats-admin::selector))
    (if (not pred)
	loprs
      ;; else use the common-lisp loop appropriate to the type of loprs
      (cond
       ((arrayp loprs)
	(loop
	 for pr across loprs
	 if (apply pred pr '())
	   collect pr
	 ))
       ((listp loprs)
	(loop
	 for pr in loprs
	 if (apply pred pr '())
	   collect pr
	 ))
       (t
	(error "Bad type for PR collection")))
      )))

(defun gnats-admin::selected? (pr)
  "Test to see if one pr meets the selection criterion."
  (or (null gnats-admin::selector)
      (apply gnats-admin::selector pr '())))

(defun gnats-admin::pr-replace! (oldpr newpr)
  "Replace the old pr with the new one"
  (if (not (consp newpr))
      (error "Replacement pr must be a full PR value"))
  (let ((pr-num (gnats-admin::pr-number newpr)))
    (if (not (= (gnats-admin::pr-number oldpr)
		pr-num))
	(error "Cannot replace PR with one of different number"))
    (if (< (length pr-list) pr-num)
	(setq pr-list
	      (vconcat pr-list (make-vector (- pr-num (length pr-list)) nil))))
    (aset pr-list (1- pr-num) newpr)))

(defvar gnats-admin::dirty-list nil
  "List of PRs which may be out of date and need refreshing.")

(defun gnats-admin:reset () 
  "Reset the cached data for gnats-admin."
  (setq pr-list nil)
  (setq gnats-admin::dirty-list nil)	; it's everything now
  )

(defun gnats-admin:regret ()
  "Create or edit the regression test for the current problem report."
  (interactive)
  (let* ((pr (gnats-admin::pr-at (point)))
	 (num (gnats-admin::pr-number pr)))
    (pr-regret num)))

(defun gnats-admin:refresh (&optional force)
  (interactive "P")
  (if force
      (gnats-admin:reset))
  (if (not pr-list)
      (progn
	(gnats-admin::run-query))
    (progn
      (mapc (function (lambda (p) (gnats-admin::pr-reread p)))
	    gnats-admin::dirty-list)))
  (setq gnats-admin::dirty-list nil)
  (set-buffer (gnats-admin::buffer))
  (let ((standard-output (current-buffer))
	(buffer-read-only nil)
	(this-pr-num (gnats-admin::pr-num-at (point))))
    ; is this overkill; could we save our extents unless force?
    (if nil
	(map-extents (function (lambda (ext data) (delete-extent ext) nil))
		     (current-buffer)))
    (delete-region (point-min) (point-max))
    (beginning-of-buffer)
    (message "Redisplay")
    (mapc (function (lambda (pr) 
		      (if (gnats-admin::selected? pr)
			  (gnats-admin::display-pr pr))))
	  pr-list)
    (message nil)
    ;; catch search errors in case the current pr no longer exists--
    ;; if so, go to end of buffer
    (condition-case err
	(if (numberp this-pr-num)
	    (gnats-admin:goto-pr this-pr-num))
      (search-failed
       (goto-char (point-max)))))
  (run-hooks 'gnats-admin-refresh-hook)
  t)

(defun gnats-admin::pr-number (pr)
  "Get the number of this pr -- which can be either a pr datum or a number."
  (or (and (numberp pr) pr)
      (gnats-admin::pr-get pr 'Number)))

;; this must not depend on pr-at, because that depends on this.
(defun gnats-admin::pr-num-at (pos)
  "Find the pr number for the pr at POS"
  (let*
      ((ext (extent-at pos nil 'pr))
       (pr-prop (and ext (extent-property ext 'pr))))
    (if pr-prop
	(gnats-admin::pr-number pr-prop)
      (save-excursion
	(goto-char pos)
	(if (not (looking-at "^\\s-*[0-9]"))
	    (backward-paragraph))
	(if (looking-at "\\s-*[0-9]+[^0-9]")
	    (read (match-string 0))
	  nil)))))

(defun gnats-admin::pr-by-number (num)
  "Find the pr numbered N."
  ;; first try the easy way
  (let
      ((pr (elt pr-list (1- num))))
    (if (and pr (= num (gnats-admin::pr-number pr)))
	pr
      ;; easy way didnt work; scan the list
      (loop
       for prx across pr-list
       if (= num (gnats-admin::pr-number prx))
       return prx))))

;; use face alist to set face colors
(defun gnats-admin::setup-faces ()
  "Set up the faces for gnats admin mode."
  (mapc
   (function (lambda (l) (make-face (car l))))
   gnats-admin::face-color)
  (if (memq (device-class) '(color grayscale))
      (mapc
       (function (lambda (l) 
		   (if (cadr l)
		       (set-face-foreground (car l) (cadr l)))
		   (if (caddr l)
		       (set-face-background (car l) (caddr l)))))
       gnats-admin::face-color))
  (setq font-lock-keywords gnats-admin::font-lock-keywords)
  ;; this is too slow -- instead, do explicit fontification after modify
  ; (turn-on-font-lock)
  )

(defvar gnats-admin::pr-mark-glyph nil
  "Glyph used to mark the current PR in display.")

(defun gnats-admin::buffer ()
  "Find or create gnats admin buffer."
  (or (get-buffer "*gnats*")
      (let ((buf (get-buffer-create "*gnats*")))
	(set-buffer buf)
	(make-local-variable 'paragraph-start)
	(make-local-variable 'paragraph-separate)
	(setq paragraph-start "^\\(\\S-\\|[ \t\n]*$\\)")
	(setq paragraph-separate "^[ \t\n]*$")
	(setq buffer-read-only t)
	(setq buffer-undo-list t)		; disable undo info
	(setq gnats-admin::pr-mark-glyph (make-pixmap "target"))
	(gnats-admin::setup-faces)
	buf)
      ))
    
(defun gnats-admin:pr (pr-or-num)
  "If PR-OR-NUM is a pr, return it; if it's a number, 
return the pr with that number."
  (cond
   ((numberp pr-or-num)
    (gnats-admin::pr-by-number pr-or-num))
   ((consp pr-or-num)
    pr-or-num)
   (t
    (error "Not a valid PR: %s" pr-or-num))
   ))
    
(defun gnats-admin::pr-at (pos)
  "PR at POSITION"
  (or
   (let ((ext (extent-at pos nil 'pr)))
     (if ext (extent-property ext 'pr)))
   (gnats-admin::pr-num-at pos)))

;; next should, ideally, run a 1-pr query then splice that into
;; pr-list to update the current pr.
;; however, there's a race condition with gnats; so put the edited
;; pr on the dirty list to be inquired later.
(defun gnats-admin:pr-edit ()
  (interactive)
  (let*
      ((pr (gnats-admin::pr-at (point)))
       (num (gnats-admin::pr-number pr))
       (num-str (format "%d" num)))
    (pr-edit num-str)
    (setq gnats-admin::dirty-list (cons pr gnats-admin::dirty-list))))

(defun gnats-admin:pr-view ()
  (interactive)
  (pr-view (format "%d" (gnats-admin::pr-num-at (point)))))
(defun gnats-admin:pr-synopsis ()
  (interactive)
  (let*
      ((pr (gnats-admin::pr-at (point)))
       (syn (gnats-admin::pr-get pr 'Synopsis)))
    (message "Synopsis: %s" syn)))

(defun gnats-admin:pr-originator ()
  (interactive)
  (let*
      ((pr (gnats-admin::pr-at (point)))
       (syn (gnats-admin::pr-get pr 'Originator)))
    (message "Originator: %s" syn)))

(defun gnats-admin:pr-field (fld)
  "Show any pr field FLD of current pr."
  (interactive
   (list (completing-read "Field: " gnats-admin::pr-field-alist
			   nil t)))
  (let*
      ((pr (gnats-admin::pr-at (point)))
       (val (gnats-admin::pr-get pr (intern fld))))
    (message "%s: %s" fld val)))

(defvar gnats-admin::current-pr nil "Current pr")
(defun gnats-admin::highlight (pr)
  "Hilight the current PR -- may unhilight the previous."
  (condition-case err
      (progn
	(if gnats-admin::current-pr
	    (highlight-extent
	     (gnats-admin::pr-buffer-extent gnats-admin::current-pr)
	     nil))
	(highlight-extent (gnats-admin::pr-buffer-extent pr) t)
	(setq gnats-admin::current-pr pr))
    (error (setq gnats-admin::current-pr nil))))

(defun gnats-admin::highlight-point ()
  "Highlight the pr at point"
  ;; make point visible
  (or
   (pos-visible-in-window-p)
   (recenter '(t)))
  (gnats-admin::highlight (gnats-admin::pr-at (point))))

(defun gnats-admin:next ()
  "Next pr"
  (interactive)
  (forward-paragraph)
  (gnats-admin::highlight-point))
(defun gnats-admin:prev ()
  "Prev pr."
  (interactive)
  (backward-paragraph)
  (gnats-admin::highlight-point))
(defun gnats-admin:this ()
  "Activate pr at point."
  (interactive)
  (end-of-line)
  (backward-paragraph)
  (gnats-admin::highlight-point))

(defun gnats-admin:mouse-set (ev)
  (interactive "e")
  (mouse-set-point ev)
  (gnats-admin::highlight-point))

(defun gnats-admin:mouse-synopsis (ev)
  (interactive "e")
  (gnats-admin:mouse-set ev)
  (gnats-admin:pr-synopsis))

(defun gnats-admin:mouse-menu (ev)
  (interactive "e")
  (gnats-admin:mouse-set ev)
  (popup-mode-menu))

(defun gnats-admin:refresh-this-pr ()
  "Reread and refresh the display of the current PR."
  (interactive)
  (let* ((pr (gnats-admin::pr-at (point)))
	 (buffer-read-only nil)
	 (b (gnats-admin::pr-buffer-begin pr))
	 (e (gnats-admin::pr-buffer-end pr)))
    (goto-char b)
    (save-excursion
      (gnats-admin::pr-buffer-delete pr)
      (gnats-admin::pr-reread pr)
      (gnats-admin::display-pr pr (current-buffer))
      ;; had to dive pretty deep into font-lock to get this one...
      (let ((font-lock-mode t))
	(font-lock-after-change-function b e 1)))
    (gnats-admin::highlight-point)))

(defun gnats-admin:goto-pr (n)
  "Make pr number N the current pr."
  (interactive "nPR: ")
  (goto-char (gnats-admin::pr-n-pos n))
  (gnats-admin::highlight-point))

(defun gnats-admin::pr-n-pos (n)
  "Find the buffer position of pr numbered N in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((re (format "^%s\\s-" n)))
      (re-search-forward re)
      (point)
      )))

(defun gnats-admin::pr-buffer-delete (pr)
  "Delete the display of the PR."
  (let* ((b (gnats-admin::pr-buffer-begin pr))
	 (e (gnats-admin::pr-buffer-end pr))
	 (mbuf (gnats-admin::pr-buffer pr))
	 (pr-ext (gnats-admin::pr-buffer-extent pr)))
    (set-buffer mbuf)
    (let ((buffer-read-only nil))
      (delete-region b e)
      (if (extentp pr-ext)
	  (delete-extent pr-ext)))
    ))

(defun gnats-admin:quit ()
  "Quit out of gnats admin mode."
  (interactive)
  (if (get-buffer "**gnats-query*")
      (kill-buffer "**gnats-query*"))
  (kill-buffer nil))
  
(defvar gnats-admin-mode-map nil "Key map for gnats admin mode.")

(defun gnats-admin::setup-keymap ()
  (if (not (keymapp gnats-admin-mode-map))
      (progn
	(setq gnats-admin-mode-map (make-keymap))
	(suppress-keymap gnats-admin-mode-map)
	(define-key gnats-admin-mode-map "e" 'gnats-admin:pr-edit)
	(define-key gnats-admin-mode-map "v" 'gnats-admin:pr-view)
	(define-key gnats-admin-mode-map "s" 'gnats-admin:pr-synopsis)
	(define-key gnats-admin-mode-map "o" 'gnats-admin:pr-originator)
	(define-key gnats-admin-mode-map "f" 'gnats-admin:pr-field)
	(define-key gnats-admin-mode-map "\C-l" 'gnats-admin:refresh)
	(define-key gnats-admin-mode-map "n" 'gnats-admin:next)
	(define-key gnats-admin-mode-map "p" 'gnats-admin:prev)
	(define-key gnats-admin-mode-map " " 'gnats-admin:this)
	(define-key gnats-admin-mode-map "q" 'gnats-admin:quit)
	(define-key gnats-admin-mode-map "g" 'gnats-admin:goto-pr)
	(define-key gnats-admin-mode-map "r" 'gnats-admin:refresh-this-pr)
	(define-key gnats-admin-mode-map "S" 'gnats-admin:edit-selection)
	(define-key gnats-admin-mode-map "R" 'gnats-admin:regret)
	(define-key gnats-admin-mode-map 'button1 'gnats-admin:mouse-set)
	(define-key gnats-admin-mode-map 'button2 'gnats-admin:mouse-synopsis)
	(define-key gnats-admin-mode-map 'button3 'gnats-admin:mouse-menu)
	))
  )

(defun gnats-admin-mode ()
  "Major mode for looking at a summary of gnats reports.
Stomps to gnats admin buffer!
Commands: \\{gnats-admin-mode-map}."
  (interactive)
  (switch-to-buffer (gnats-admin::buffer))
  (setq major-mode 'gnats-admin-mode)
  (setq mode-name "Gnats Admin")
  (gnats-admin::setup-keymap)
  (use-local-map gnats-admin-mode-map)
  (setq mode-popup-menu gnats-admin::popup-menu)
  (gnats-admin:refresh)
  )

(put 'gnats-admin-mode 'mode-class 'special)

(defvar gnats-admin::pr-mark-glyph nil "marker for current PR.")
           
(defun gnats-admin::pr-buffer (pr)
  "The buffer in which this pr is displayed."
  (gnats-admin::buffer))

(defun gnats-admin::pr-buffer-begin (pr)
  "Return the position of beginning of this PR."
  (let 
      ((pr-num (gnats-admin::pr-number pr)))
    (save-excursion 
      (set-buffer (gnats-admin::pr-buffer pr))
      ;; first try locally, then thru whole buffer
      (or
       (and (progn (backward-paragraph 1)
		   (re-search-forward (format "^%d " pr-num) nil t))
	    (progn 
	      (beginning-of-line 1)
	      (point)))
       (and (progn (goto-char (point-min))
		   (re-search-forward (format "^%d " pr-num) nil t))
	    (progn 
	      (beginning-of-line 1)
	      (point)))))))

(defun gnats-admin::pr-buffer-end (pr)
  "Return the position of the end of the PR."
  (save-excursion
    (set-buffer (gnats-admin::pr-buffer pr))
    (goto-char (gnats-admin::pr-buffer-begin pr))
    (forward-paragraph 1)
    (point)))

(defun gnats-admin::unclosed (pr)
  "A selector that chooses unclosed PR's."
  (not (eq 'closed (gnats-admin::pr-get pr 'State))))

(defvar gnats-admin:selexpr nil
  "Selection expression -- see eval-selexpr.")

(defun gnats-admin::eval-selexpr (pr)
  "Evaluate the selection expression, in an environment with
pr bound to the pr, and the field names bound to their value."
  (let
      ((Number (gnats-admin::pr-get pr 'Number))
       (Category (gnats-admin::pr-get pr 'Category))
       (Synopsis (gnats-admin::pr-get pr 'Synopsis))
       (Confidential (gnats-admin::pr-get pr 'Confidential))
       (Severity (gnats-admin::pr-get pr 'Severity))
       (Priority (gnats-admin::pr-get pr 'Priority))
       (Responsible (gnats-admin::pr-get pr 'Responsible))
       (State (gnats-admin::pr-get pr 'State))
       (Class (gnats-admin::pr-get pr 'Class))
       (Submitter-Id (gnats-admin::pr-get pr 'Submitter-Id))
       (Arrival-Date (gnats-admin::pr-get pr 'Arrival-Date))
       (Originator (gnats-admin::pr-get pr 'Originator))
       (Release (gnats-admin::pr-get pr 'Release)))
    (or (not gnats-admin:selexpr)
	(eval gnats-admin:selexpr))))

(defun gnats-admin:edit-selection ()
  "Edit the selection criteria."
  (interactive)
  (setq gnats-admin::selector 'gnats-admin::eval-selexpr)
  (setq gnats-admin:selexpr 
	(edit-expr gnats-admin:selexpr
";; Selection expression.  This is evaluated with Number, Category, Synopsis,
;; Confidential, Severity, Priority, Responsible, State, Class, Submitter-Id,
;; Arrival-Date, Originator, and Release set from the PR; if it's a non-null
;; expression that evaluates true, then that record is displayed.  Free-form
;; fields are strings, others are symbols or other atoms.
"
))
  (gnats-admin:refresh))
