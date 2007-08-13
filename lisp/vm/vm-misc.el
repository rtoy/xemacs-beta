;;; Miscellaneous functions for VM
;;; Copyright (C) 1989-1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-misc)

(defun vm-delete-non-matching-strings (regexp list &optional destructively)
  "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned."
  (or destructively (setq list (copy-sequence list)))
  (let ((curr list) (prev nil))
    (while curr
      (if (string-match regexp (car curr))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setcdr prev (cdr curr))
	  (setq curr (cdr curr)))))
    list ))

(defun vm-parse (string regexp &optional matchn)
  (or matchn (setq matchn 1))
  (let (list)
    (store-match-data nil)
    (while (string-match regexp string (match-end 0))
      (setq list (cons (substring string (match-beginning matchn)
				  (match-end matchn)) list)))
    (nreverse list)))

(defun vm-parse-addresses (string)
  (if (null string)
      ()
    (let (work-buffer)
      (save-excursion
       (unwind-protect
	   (let (list start s char)
	     (setq work-buffer (generate-new-buffer "*vm-work*"))
	     (set-buffer work-buffer)
	     (insert string)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward "^\"\\,(")
	       (setq char (following-char))
	       (cond ((= char ?\\)
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((= char ?,)
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      (skip-chars-forward ",\t\f\n\r ")
		      (setq start (point)))
		     ((= char ?\")
		      (re-search-forward "[^\\]\"" nil 0))
		     ((= char ?\()
		      (let ((parens 1))
			(forward-char 1)
			(while (and (not (eobp)) (not (zerop parens)))
			  (re-search-forward "[()]" nil 0)
			  (cond ((or (eobp)
				     (= (char-after (- (point) 2)) ?\\)))
				((= (preceding-char) ?\()
				 (setq parens (1+ parens)))
				(t
				 (setq parens (1- parens)))))))))
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list)) ; jwz: fixed order
	(and work-buffer (kill-buffer work-buffer)))))))

(defun vm-parse-structured-header (string &optional sepchar keep-quotes)
  (if (null string)
      ()
    (let ((work-buffer nil))
      (save-excursion
       (unwind-protect
	   (let ((list nil)
		 (nonspecials "^\"\\( \t\n\r\f")
		 start s char sp+sepchar)
	     (if sepchar
		 (setq nonspecials (concat nonspecials (list sepchar))
		       sp+sepchar (concat "\t\f\n\r " (list sepchar))))
	     (setq work-buffer (generate-new-buffer "*vm-work*"))
	     (buffer-disable-undo work-buffer)
	     (set-buffer work-buffer)
	     (insert string)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward nonspecials)
	       (setq char (following-char))
	       (cond ((looking-at "[ \t\n\r\f]")
		      (delete-char 1))
		     ((= char ?\\)
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((and sepchar (= char sepchar))
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      (skip-chars-forward sp+sepchar)
		      (setq start (point)))
		     ((looking-at " \t\n\r\f")
		      (skip-chars-forward " \t\n\r\f"))
		     ((= char ?\")
		      (let ((done nil))
			(if keep-quotes
			    (forward-char 1)
			  (delete-char 1))
			(while (not done)
			  (if (null (re-search-forward "[\\\"]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (delete-char -1)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  (t (if (not keep-quotes)
					 (delete-char -1))
				     (setq done t)))))))
		     ((= char ?\()
		      (let ((done nil)
			    (pos (point))
			    (parens 1))
			(forward-char 1)
			(while (not done)
			  (if (null (re-search-forward "[\\()]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  ((char-equal char ?\()
				   (setq parens (1+ parens)))
				  (t
				   (setq parens (1- parens)
					 done (zerop parens))))))
			(delete-region pos (point))))))
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list))
	(and work-buffer (kill-buffer work-buffer)))))))

(defun vm-write-string (where string)
  (if (bufferp where)
      (vm-save-buffer-excursion
	(set-buffer where)
	(goto-char (point-max))
	(insert string))
    (let ((temp-buffer nil))
      (unwind-protect
	  (save-excursion
	    (setq temp-buffer (generate-new-buffer "*vm-work*"))
	    (set-buffer temp-buffer)
	    (insert string)
	    ;; correct for VM's uses of this function---
	    ;; writing out message separators
	    (setq buffer-file-type nil)
	    ;; Tell XEmacs/MULE to pick the correct newline conversion.
	    (and vm-xemacs-mule-p
		 (set-file-coding-system 'no-conversion nil))
	    (write-region (point-min) (point-max) where t 'quiet))
	(and temp-buffer (kill-buffer temp-buffer))))))

(defmacro vm-marker (pos &optional buffer)
  (list 'set-marker '(make-marker) pos buffer))

(defmacro vm-increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro vm-decrement (variable)
  (list 'setq variable (list '1- variable)))

(defmacro vm-select-folder-buffer ()
  '(and vm-mail-buffer
	(or (buffer-name vm-mail-buffer)
	    (error "Folder buffer has been killed."))
	(set-buffer vm-mail-buffer)))

(defun vm-check-for-killed-summary ()
  (and (bufferp vm-summary-buffer) (null (buffer-name vm-summary-buffer))
       (let ((mp vm-message-list))
	 (setq vm-summary-buffer nil)
	 (while mp
	   (vm-set-su-start-of (car mp) nil)
	   (vm-set-su-end-of (car mp) nil)
	   (setq mp (cdr mp))))))

(defun vm-check-for-killed-presentation ()
  (and (bufferp vm-presentation-buffer-handle)
       (null (buffer-name vm-presentation-buffer-handle))
       (progn
	 (setq vm-presentation-buffer-handle nil
	       vm-presentation-buffer nil))))

(defun vm-check-for-killed-folder ()
  (and (bufferp vm-mail-buffer) (null (buffer-name vm-mail-buffer))
       (setq vm-mail-buffer nil)))

(defmacro vm-error-if-folder-read-only ()
  '(while vm-folder-read-only
     (signal 'folder-read-only (list (current-buffer)))))

(put 'folder-read-only 'error-conditions '(folder-read-only error))
(put 'folder-read-only 'error-message "Folder is read-only")

(defmacro vm-error-if-virtual-folder ()
  '(and (eq major-mode 'vm-virtual-mode)
	(error "%s cannot be applied to virtual folders." this-command)))

(defmacro vm-build-threads-if-unbuilt ()
  '(if (null vm-thread-obarray)
       (vm-build-threads nil)))

(defun vm-abs (n) (if (< n 0) (- n) n))

;; save-restriction flubs restoring the clipping region if you
;; (widen) and modify text outside the old region.
;; This should do it right.
(defmacro vm-save-restriction (&rest forms)
  (let ((vm-sr-clip (make-symbol "vm-sr-clip"))
	(vm-sr-min (make-symbol "vm-sr-min"))
	(vm-sr-max (make-symbol "vm-sr-max")))
    (list 'let (list (list vm-sr-clip '(> (buffer-size)
					  (- (point-max) (point-min))))
		     ;; this shouldn't be necessary but the
		     ;; byte-compiler turns these into interned symbols
		     ;; which utterly defeats the purpose of the
		     ;; make-symbol calls above.  Soooo, until the compiler
		     ;; is fixed, these must be made into (let ...)
		     ;; temporaries so that nested calls to this macros
		     ;; won't misbehave.
		     vm-sr-min vm-sr-max)
	  (list 'and vm-sr-clip
		(list 'setq vm-sr-min '(set-marker (make-marker) (point-min)))
		(list 'setq vm-sr-max '(set-marker (make-marker) (point-max))))
	  (list 'unwind-protect (cons 'progn forms)
		'(widen)
		(list 'and vm-sr-clip
		      (list 'progn
			    (list 'narrow-to-region vm-sr-min vm-sr-max)
			    (list 'set-marker vm-sr-min nil)
			    (list 'set-marker vm-sr-max nil)))))))

(defmacro vm-save-buffer-excursion (&rest forms)
  (list 'let '((vm-sbe-buffer (current-buffer)))
	(list 'unwind-protect
	      (cons 'progn forms)
	      '(and (not (eq vm-sbe-buffer (current-buffer)))
		    (buffer-name vm-sbe-buffer)
		    (set-buffer vm-sbe-buffer)))))

(defun vm-last (list) (while (cdr-safe list) (setq list (cdr list))) list)

(defun vm-vector-to-list (vector)
  (let ((i (1- (length vector)))
	list)
    (while (>= i 0)
      (setq list (cons (aref vector i) list))
      (vm-decrement i))
    list ))

(defun vm-extend-vector (vector length &optional fill)
  (let ((vlength (length vector)))
    (if (< vlength length)
	(apply 'vector (nconc (vm-vector-to-list vector)
			      (make-list (- length vlength) fill)))
      vector )))

(defun vm-obarray-to-string-list (blobarray)
  (let ((list nil))
    (mapatoms (function (lambda (s) (setq list (cons (symbol-name s) list))))
	      blobarray)
    list ))

(defun vm-mapcar (function &rest lists)
  (let (arglist result)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (setq result (cons (apply function arglist) result))
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun vm-mapc (function &rest lists)
  (let (arglist)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (apply function arglist)
      (setq lists (mapcar 'cdr lists)))))

(defun vm-delete (predicate list &optional reverse)
  (let ((p list) (reverse (if reverse 'not 'identity)) prev)
    (while p
      (if (funcall reverse (funcall predicate (car p)))
	  (if (null prev)
	      (setq list (cdr list) p list)
	    (setcdr prev (cdr p))
	    (setq p (cdr p)))
	(setq prev p p (cdr p))))
    list ))

(defun vm-delete-directory-file-names (list)
  (vm-delete 'file-directory-p list))

(defun vm-delete-backup-file-names (list)
  (vm-delete 'backup-file-name-p list))

(defun vm-delete-auto-save-file-names (list)
  (vm-delete 'auto-save-file-name-p list))

(defun vm-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurrence of a string in the list,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
  (let ((hashtable vm-delete-duplicates-obarray)
	(new-list nil)
	sym-string sym)
    (fillarray hashtable 0)
    (while list
      (setq sym-string
	    (if hack-addresses
		(nth 1 (funcall vm-chop-full-name-function (car list)))
	      (car list))
	    sym-string (or sym-string "-unparseable-garbage-")
	    sym (intern sym-string hashtable))
      (if (boundp sym)
	  (and all (setcar (symbol-value sym) nil))
	(setq new-list (cons (car list) new-list))
	(set sym new-list))
      (setq list (cdr list)))
    (delq nil (nreverse new-list))))

(defun vm-member-0 (thing list)
  (catch 'done
    (while list
      (and (equal (car list) thing)
	   (throw 'done list))
      (setq list (cdr list)))
    nil ))

(fset 'vm-member (symbol-function (if (fboundp 'member) 'member 'vm-member-0)))

(defun vm-delqual (ob list)
  (let ((prev nil)
	(curr list))
    (while curr
      (if (not (equal ob (car curr)))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setq curr (cdr curr))
	  (setcdr prev curr))))
    list ))

(defun vm-copy-local-variables (buffer &rest variables)
  (let ((values (mapcar 'symbol-value variables)))
    (save-excursion
      (set-buffer buffer)
      (vm-mapc 'set variables values))))

(put 'folder-empty 'error-conditions '(folder-empty error))
(put 'folder-empty 'error-message "Folder is empty")
(put 'unrecognized-folder-type 'error-conditions
     '(unrecognized-folder-type error))
(put 'unrecognized-folder-type 'error-message "Unrecognized folder type")

(defun vm-error-if-folder-empty ()
  (while (null vm-message-list)
    (if vm-folder-type
	(signal 'unrecognized-folder-type nil)
      (signal 'folder-empty nil))))

(defun vm-copy (object)
  (cond ((consp object)
	 (let (return-value cons)
	   (setq return-value (cons (vm-copy (car object)) nil)
		 cons return-value
		 object (cdr object))
	   (while (consp object)
	     (setcdr cons (cons (vm-copy (car object)) nil))
	     (setq cons (cdr cons)
		   object (cdr object)))
	   (setcdr cons object)
	   return-value ))
	((vectorp object) (apply 'vector (mapcar 'vm-copy object)))
	((stringp object) (copy-sequence object))
	((markerp object) (copy-marker object))
	(t object)))

;; make-frame might be defined and still not work.  This would
;; be true since the user could be running on a tty and using
;; XEmacs 19.12, or using FSF Emacs 19.28 (or prior FSF Emacs versions).
;;
;; make-frame works on ttys in FSF Emacs 19.29, but other than
;; looking at the version number I don't know a sane way to
;; test for it without just running make-frame.  I'll just
;; let it not work for now... someone will complain eventually
;; and I'll think of something.

(defun vm-multiple-frames-possible-p () 
  (cond (vm-xemacs-p 
         (eq (device-type) 'x)) 
        (vm-fsfemacs-19-p 
         (not (eq window-system nil))))) 
 
(defun vm-mouse-support-possible-p () 
  (vm-multiple-frames-possible-p)) 
 
(defun vm-menu-support-possible-p ()
  (cond (vm-xemacs-p
	 (featurep 'menubar))
	(vm-fsfemacs-19-p
	 (fboundp 'menu-bar-mode))
	(t nil)))
 
(defun vm-toolbar-support-possible-p ()
  (and vm-xemacs-p (featurep 'toolbar)))

(defun vm-multiple-fonts-possible-p ()
  (cond (vm-xemacs-p
	 (eq (device-type) 'x))
	(vm-fsfemacs-19-p
	 (eq window-system 'x))))

(defun vm-run-message-hook (message &optional hook-variable)
  (save-excursion
    (set-buffer (vm-buffer-of message))
    (vm-save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(run-hooks hook-variable)))))

(defun vm-error-free-call (function &rest args)
  (condition-case nil
      (apply function args)
    (error nil)))

(put 'beginning-of-folder 'error-conditions '(beginning-of-folder error))
(put 'beginning-of-folder 'error-message "Beginning of folder")
(put 'end-of-folder 'error-conditions '(end-of-folder error))
(put 'end-of-folder 'error-message "End of folder")

(defun vm-trace (&rest args)
  (save-excursion
    (set-buffer (get-buffer-create "*vm-trace*"))
    (apply 'insert args)))

(defun vm-timezone-make-date-sortable (string)
  (or (cdr (assq string vm-sortable-date-alist))
      (let ((vect (vm-parse-date string))
	    (date (vm-parse (current-time-string) " *\\([^ ]+\\)")))
	;; if specified date is incomplete fill in the holes
	;; with useful information, defaulting to the current
	;; date and timezone for everything except hh:mm:ss which
	;; defaults to midnight.
	(if (equal (aref vect 1) "")
	    (aset vect 1 (nth 2 date)))
	(if (equal (aref vect 2) "")
	    (aset vect 2 (nth 1 date)))
	(if (equal (aref vect 3) "")
	    (aset vect 3 (nth 4 date)))
	(if (equal (aref vect 4) "")
	    (aset vect 4 "00:00:00"))
	(if (equal (aref vect 5) "")
	    (aset vect 5 (vm-current-time-zone)))
	;; save this work so we won't have to do it again
	(setq vm-sortable-date-alist
	      (cons (cons string
			  (condition-case nil
			      (timezone-make-date-sortable
			       (format "%s %s %s %s %s"
				       (aref vect 1)
				       (aref vect 2)
				       (aref vect 3)
				       (aref vect 4)
				       (aref vect 5)))
			    (error "1970010100:00:00")))
		    vm-sortable-date-alist))
	;; return result
	(cdr (car vm-sortable-date-alist)))))

(defun vm-current-time-zone ()
  (or (condition-case nil
	  (let* ((zone (car (current-time-zone)))
		 (absmin (/ (vm-abs zone) 60)))
	    (format "%c%02d%02d" (if (< zone 0) ?- ?+)
		    (/ absmin 60) (% absmin 60)))
	(error nil))
      (let ((temp-buffer nil))
	(condition-case nil
	    (unwind-protect
		(save-excursion
		  (setq temp-buffer (generate-new-buffer "*vm-work*"))
		  (set-buffer temp-buffer)
		  (call-process "date" nil temp-buffer nil)
		  (nth 4 (vm-parse (vm-buffer-string-no-properties)
				   " *\\([^ ]+\\)")))
	      (and temp-buffer (kill-buffer temp-buffer)))
	  (error nil)))
      ""))

(defun vm-should-generate-summary ()
  (cond ((eq vm-startup-with-summary t) t)
	((integerp vm-startup-with-summary)
	 (let ((n vm-startup-with-summary))
	   (cond ((< n 0) (null (nth (vm-abs n) vm-message-list)))
		 (t (nth (1- n) vm-message-list)))))
	(vm-startup-with-summary t)
	(t nil)))

(defun vm-find-composition-buffer (&optional not-picky)
  (let ((b-list (buffer-list)) choice alternate)
    (save-excursion
     (while b-list
       (set-buffer (car b-list))
       (if (eq major-mode 'mail-mode)
	   (if (buffer-modified-p)
	       (setq choice (current-buffer)
		     b-list nil)
	     (and not-picky (null alternate)
		  (setq alternate (current-buffer)))
	     (setq b-list (cdr b-list)))
	 (setq b-list (cdr b-list))))
    (or choice alternate))))

(defun vm-get-file-buffer (file)
  "Like get-file-buffer, but also checks buffers against FILE's truename"
  (or (get-file-buffer file)
      (and (fboundp 'file-truename)
	   (get-file-buffer (file-truename file)))))

(defun vm-set-region-face (start end face)
  (let ((e (vm-make-extent start end)))
    (vm-set-extent-property e 'face face)))

(defun vm-default-buffer-substring-no-properties (beg end &optional buffer)
  (let ((s (if buffer
	       (save-excursion
		 (set-buffer buffer)
		 (buffer-substring beg end))
	     (buffer-substring beg end))))
    (set-text-properties 0 (length s) nil s)
    (copy-sequence s)))

(fset 'vm-buffer-substring-no-properties
  (cond ((fboundp 'buffer-substring-no-properties)
	 (function buffer-substring-no-properties))
	(vm-xemacs-p
	 (function buffer-substring))
	(t (function vm-default-buffer-substring-no-properties))))

(defun vm-buffer-string-no-properties ()
  (vm-buffer-substring-no-properties (point-min) (point-max)))

(defun vm-insert-region-from-buffer (buffer &optional start end)
  (let ((target-buffer (current-buffer)))
    (set-buffer buffer)
    (save-restriction
      (widen)
      (or start (setq start (point-min)))
      (or end (setq end (point-max)))
      (set-buffer target-buffer)
      (insert-buffer-substring buffer start end)
      (set-buffer buffer))
    (set-buffer target-buffer)))

(if (not (fboundp 'vm-extent-property))
    (if (fboundp 'overlay-get)
	(fset 'vm-extent-property 'overlay-get)
      (fset 'vm-extent-property 'extent-property)))

(if (not (fboundp 'vm-set-extent-property))
    (if (fboundp 'overlay-put)
	(fset 'vm-set-extent-property 'overlay-put)
      (fset 'vm-set-extent-property 'set-extent-property)))

(if (not (fboundp 'vm-set-extent-endpoints))
    (if (fboundp 'move-overlay)
	(fset 'vm-set-extent-endpoints 'move-overlay)
      (fset 'vm-set-extent-endpoints 'set-extent-endpoints)))

(if (not (fboundp 'vm-make-extent))
    (if (fboundp 'make-overlay)
	(fset 'vm-make-extent 'make-overlay)
      (fset 'vm-make-extent 'make-extent)))

(if (not (fboundp 'vm-extent-end-position))
    (if (fboundp 'overlay-end)
	(fset 'vm-extent-end-position 'overlay-end)
      (fset 'vm-extent-end-position 'extent-end-position)))

(if (not (fboundp 'vm-extent-start-position))
    (if (fboundp 'overlay-start)
	(fset 'vm-extent-start-position 'overlay-start)
      (fset 'vm-extent-start-position 'extent-start-position)))

(if (not (fboundp 'vm-detach-extent))
    (if (fboundp 'delete-overlay)
	(fset 'vm-detach-extent 'delete-overlay)
      (fset 'vm-detach-extent 'detach-extent)))

(if (not (fboundp 'vm-extent-properties))
    (if (fboundp 'overlay-properties)
	(fset 'vm-extent-properties 'overlay-properties)
      (fset 'vm-extent-properties 'extent-properties)))

(defun vm-copy-extent (e)
  (let ((props (vm-extent-properties e))
	(ee (vm-make-extent (vm-extent-start-position e)
			    (vm-extent-end-position e))))
    (while props
      (vm-set-extent-property ee (car props) (car (cdr props)))
      (setq props (cdr (cdr props))))))

(defun vm-make-tempfile-name ()
  (let ((done nil) (pid (emacs-pid)) filename)
    (while (not done)
      (setq filename (format "%s/vm%d.%d" vm-temp-file-directory pid
			     vm-tempfile-counter)
	    vm-tempfile-counter (1+ vm-tempfile-counter)
	    done (not (file-exists-p filename))))
    filename ))

(defun vm-insert-char (char &optional count ignored buffer)
  (condition-case nil
      (progn
	(insert-char char count ignored buffer)
	(fset 'vm-insert-char 'insert-char))
    (wrong-number-of-arguments
     (fset 'vm-insert-char 'vm-xemacs-compatible-insert-char)
     (vm-insert-char char count ignored buffer))))

(defun vm-xemacs-compatible-insert-char (char &optional count ignored buffer)
  (if (and buffer (eq buffer (current-buffer)))
      (insert-char char count)
    (save-excursion
      (set-buffer buffer)
      (insert-char char count))))

(defun vm-symbol-lists-intersect-p (list1 list2)
  (catch 'done
    (while list1
      (and (memq (car list1) list2)
	   (throw 'done t))
      (setq list1 (cdr list1)))
    nil ))

(defun vm-set-buffer-variable (buffer var value)
  (save-excursion
    (set-buffer buffer)
    (set var value)))

(defun vm-buffer-variable-value (buffer var)
  (save-excursion
    (set-buffer buffer)
    (symbol-value var)))

(defsubst vm-with-string-as-temp-buffer (string function)
  (let ((work-buffer nil))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *work*"))
	  (set-buffer work-buffer)
	  (insert string)
	  (funcall function)
	  (buffer-string))
      (and work-buffer (kill-buffer work-buffer)))))

(defmacro vm-with-virtual-selector-variables (&rest forms)
  (append '(let ((any 'vm-vs-any)
		 (and 'vm-vs-and)
		 (or 'vm-vs-or)
		 (not 'vm-vs-not)
		 (header 'vm-vs-header)
		 (label 'vm-vs-label)
		 (text 'vm-vs-text)
		 (recipient 'vm-vs-recipient)
		 (author 'vm-vs-author)
		 (subject 'vm-vs-subject)
		 (sent-before 'vm-vs-sent-before)
		 (sent-after 'vm-vs-sent-after)
		 (more-chars-than 'vm-vs-more-chars-than)
		 (less-chars-than 'vm-vs-less-chars-than)
		 (more-lines-than 'vm-vs-more-lines-than)
		 (less-lines-than 'vm-vs-less-lines-than)
		 (new 'vm-vs-new)
		 (unread 'vm-vs-unread)
		 (read 'vm-vs-read)
		 (deleted 'vm-vs-deleted)
		 (replied 'vm-vs-replied)
		 (forwarded 'vm-vs-forwarded)
		 (filed 'vm-vs-filed)
		 (written 'vm-vs-written)
		 (edited 'vm-vs-edited)
		 (marked 'vm-vs-marked)))
	  forms))

(defun vm-string-assoc (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car (car list))))
	       (= (match-end 0) (length (car (car list)))))
	  (setq found t)
	(setq list (cdr list))))
    (car list)))

(defun vm-string-member (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car list)))
	       (= (match-end 0) (length (car list))))
	  (setq found t)
	(setq list (cdr list))))
    list))

(defmacro vm-assert (expression)
  (list 'or expression
	(list 'progn
	      (list 'setq 'debug-on-error t)
	      (list 'error "assertion failed: %S"
		    (list 'quote expression)))))
