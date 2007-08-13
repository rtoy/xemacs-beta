;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-sex.el
;; Dired Version: #Revision: 7.9 $
;; RCS:
;; Description:   Marking files according to sexpressions.  Sorry.
;; Created:       Wed Sep 14 01:30:43 1994 by sandy on ibm550
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-sex)
(require 'dired)

(defvar dired-sexpr-history-symbol nil
  "History of sexpr used to mark files in dired.")

;;; Marking files according to sexpr's

(defmacro dired-parse-ls ()
  ;; Sets vars
  ;;                inode s mode nlink uid gid size time name sym
  ;; (probably let-bound in caller) according to current file line.
  ;; Returns t for succes, nil if this is no file line.
  ;; Upon success, all variables are set, either to nil or the
  ;; appropriate value, so they need not be initialized.
  ;; Moves point within the current line to the end of the file name.
  '(let ((bol (progn (beginning-of-line) (point)))
	 (eol (save-excursion (skip-chars-forward "^\n\r") (point))))
     (if (re-search-forward dired-re-month-and-time eol t)
	 (let ((mode-len 10)		; length of mode string
	       (tstart (progn (goto-char (match-beginning 0))
			      (skip-chars-forward " ")
			      (point)))
	       (fstart (match-end 0))
	       pos)
	   (goto-char (1+ bol))
	   (skip-chars-forward " \t")
	   ;; This subdir had better have been created with the current
	   ;; setting of actual switches. Otherwise, we can't parse.
	   (cond
	    ((and (or (memq ?k dired-internal-switches)
		      (memq ?s dired-internal-switches))
		  (memq ?i dired-internal-switches))
	     (setq pos (point))
	     (skip-chars-forward "0-9")
	     (if (setq inode (and (/= pos (point)) (string-to-int
						    (buffer-substring
						     pos (point)))))
		 (progn
		   (skip-chars-forward " ")
		   (setq pos (point))
		   (skip-chars-forward "0-9")
		   (setq s (and (/= pos (point)) (string-to-int
						  (buffer-substring
						   pos (point))))))
	       (setq s nil)))
	    ((or (memq ?s dired-internal-switches)
		 (memq ?k dired-internal-switches))
	     (setq pos (point))
	     (skip-chars-forward "0-9")
	     (setq s (and (/= pos (point)) (string-to-int
					       (buffer-substring
						pos (point))))
		   inode nil))
	    ((memq ?i dired-internal-switches)
	     (setq pos (point))
	     (skip-chars-forward "0-9")
	     (setq inode (and (/= pos (point)) (string-to-int
						(buffer-substring
						 pos (point))))
		   s nil))
	    (t
	     (setq s nil
		   inode nil)))
	   (skip-chars-forward " 0-9") ; in case of junk
	   (setq mode (buffer-substring (point) (+ mode-len (point))))
	   (forward-char mode-len)
	   (setq nlink (read (current-buffer)))
	   (or (integerp nlink) (setq nlink nil)) 
	   (setq uid (buffer-substring (point) (progn
						 (skip-chars-forward "^ ")
						 (point))))
	   (goto-char tstart)
	   (skip-chars-backward " ")
	   (setq pos (point))
	   (skip-chars-backward "0-9")
	   (if (= pos (point))
	       (setq size nil)
	     (setq size (string-to-int (buffer-substring (point) pos))))
	   (skip-chars-backward " ")
	   ;; if no gid is displayed, gid will be set to uid
	   ;; but user will then not reference it anyway in PREDICATE.
	   (setq gid (buffer-substring (point) (progn
						 (skip-chars-backward "^ ")
						 (point)))
		 time (buffer-substring tstart
					(progn
					  (goto-char fstart)
					  (skip-chars-backward " ")
					  (point)))
		 name (buffer-substring
		       fstart
		       (or (dired-move-to-end-of-filename t)
			   (point)))
		 sym  (and (looking-at "[/*@#=|]? -> ")
			   (buffer-substring (match-end 0)
					     eol)))
	   t)))) ; return t if parsing was a success


(defun dired-mark-sexp (predicate &optional unflag-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unflag those files instead.

PREDICATE is a lisp expression that can refer to the following symbols:

    inode  [integer] the inode of the file (only for ls -i output)
    s      [integer] the size of the file for ls -s output
	             (ususally in blocks or, with -k, in KByte)
    mode   [string]  file permission bits, e.g. \"-rw-r--r--\"
    nlink  [integer] number of links to file
    uid    [string]  owner
    gid    [string]  group  (If the gid is not displayed by ls,
	             this will still be set (to the same as uid))
    size   [integer] file size in bytes
    time   [string]  the time that ls displays, e.g. \"Feb 12 14:17\"
    name   [string]  the name of the file
    sym    [string]  if file is a symbolic link, the linked-to name, else nil.

For example, use

        (equal 0 size)

to mark all zero length files."
  ;; Using sym="" instead of nil avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; No! Want to be able look for symlinks pointing to the empty string.
  ;; Can happen. Also, then I can do an (if sym ...) structure. --sandy
  ;; Give `equal' instead of `=' in the example, as this works on
  ;; integers and strings.
  (interactive
   (list
    (read
     (dired-read-with-history "Mark if (lisp expr): " nil
			      'dired-sexpr-history))
    current-prefix-arg))
  (message "%s" predicate)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char))
	inode s mode nlink uid gid size time name sym)
    (dired-mark-if (save-excursion
		     (and (dired-parse-ls)
			  (eval predicate)))
		   (format "'%s file" predicate)))
  (dired-update-mode-line-modified t))

;;; end of dired-sex.el
