;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-xemacs.el
;; Dired Version: $Revision: 1.2 $
;; RCS:
;; Description:   dired functions for XEmacs
;; Author:        Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-xemacs)
(require 'dired)
(require 'dired-faces)

(require 'backquote)


;;; Variables not meant for user editing

;; kludge
(defun dired-demarkify-regexp (re)
  (if (string-equal (substring re 0 (length dired-re-maybe-mark))
		    dired-re-maybe-mark)
      (concat "^" (substring re
			     (length dired-re-maybe-mark)
			     (length re)))
    re))

(defvar dired-re-raw-dir (dired-demarkify-regexp dired-re-dir))
(defvar dired-re-raw-sym (dired-demarkify-regexp dired-re-sym))
(defvar dired-re-raw-exe (dired-demarkify-regexp dired-re-exe))

(defvar dired-re-raw-boring (dired-omit-regexp)
  "Regexp to match backup, autosave and otherwise boring files.")

(defvar dired-re-raw-socket (concat "^" dired-re-inode-size "s"))

(defvar dired-re-raw-setuid
  (concat "^" dired-re-inode-size
	  "-[-r][-w][Ss][-r][-w][sx][-r][-w][xst]")
  "setuid plain file (even if not executable)")

(defvar dired-re-raw-setgid 
  (concat "^" dired-re-inode-size
	  "-[-r][-w][-x][-r][-w][sS][-r][-w][xst]")
  "setgid plain file (even if not executable)")

(defvar dired-re-pre-permissions "^.? ?[0-9 	]*[-d]"
  "Regexp matching the preamble to file permissions part of a dired line.
This shouldn't match socket or symbolic link lines (which aren't editable).")

(defvar dired-re-permissions "[-r][-w][-Ssx][-r][-w][-Ssx][-r][-w][-xst]"
  "Regexp matching the file permissions part of a dired line.")

;;; Setup

(setq dired-modeline-tracking-cmds '(mouse-track))


;;; Menus

(defvar dired-subdir-menu nil "The Subdir menu for dired")
(defvar dired-mark-menu nil "The Mark menu for dired")
(defvar dired-do-menu nil "The Do menu for dired")
(defvar dired-regexp-menu nil "The Regexp menu for dired")
(defvar dired-look-menu nil "The Look menu for dired")
(defvar dired-sort-menu nil "The Sort menu for dired")
(defvar dired-help-menu nil "The Help menu for dired")

(defvar dired-menubar-menus
  '(("Subdir" . dired-subdir-menu)
    ("Mark" . dired-mark-menu)
    ("Do" . dired-do-menu)
    ("Regexp" . dired-regexp-menu)
    ("Look" . dired-look-menu)
    ("Sort" . dired-sort-menu))
  "All the dired menus.")

(defvar dired-visit-popup-menu nil "The Visit popup for dired")
(defvar dired-do-popup-menu nil "The Do popup for dired")

(defun dired-setup-menus ()
  (setq
   dired-visit-popup-menu
   '(["Find File" dired-find-file t]
     ["Find in Other Window" dired-find-file-other-window t]
     ["Find in Other Frame" dired-find-file-other-frame t]
     ["View File" dired-view-file t]
     ["Display in Other Window" dired-find-file-other-window t]))

  (setq
   dired-do-popup-menu
   '(["Copy to..." dired-do-copy t]
     ["Rename to..." dired-do-rename t]
     ["Compress/Uncompress" dired-do-compress t]
     ["Uuencode/Uudecode" dired-do-uucode t]
     ["Change Mode..." dired-do-chmod t]
     ["Change Owner..." dired-do-chown t]
     ["Change Group..." dired-do-chgrp t]
     ["Load" dired-do-load t]
     ["Byte-compile" dired-do-byte-compile t]
     ["Hardlink to..." dired-do-hardlink t]
     ["Symlink to..." dired-do-symlink t]
     ["Shell Command..." dired-do-shell-command t]
     ["Background Shell Command..." dired-do-background-shell-command t]
     ["Delete" dired-do-delete t]))

  (setq
   dired-subdir-menu
   (list 
    ["Next Subdir" dired-next-subdir t]
    ["Prev Subdir" dired-prev-subdir t]
    ["Next Dirline" dired-next-dirline t]
    ["Prev Dirline" dired-prev-dirline t]
    ["Up Dir" dired-up-directory t]
    ["Down Dir" dired-down-directory t]
    ["Insert This Subdir" dired-maybe-insert-subdir t]
    ["Create Directory..." dired-create-directory t]
    ["Kill This Subdir" dired-kill-subdir t]
    "-- Commands on All Files in Subdir --"
    ["Redisplay Subdir" dired-redisplay-subdir t]
    ["Mark Files" dired-mark-subdir-files t]
    ["Flag Files for Deletion" dired-flag-subdir-files t]
    ["Compress Uncompressed Files" dired-compress-subdir-files t]
    (vector "Uncompress Compressed Files"
	    '(let ((current-prefix-arg t))
	       (dired-compress-subdir-files))
	    ':keys (dired-key-description 'dired-compress-subdir-files
					  'universal-argument))))

  (setq
   dired-mark-menu
   (list
    ["Next Marked" dired-next-marked-file t]
    ["Previous Marked" dired-prev-marked-file t]
    ["Change Marks..." dired-change-marks t]
    ["Unmark All" dired-unmark-all-files t]
    (vector "Toggle marks..."
	    '(let ((current-prefix-arg t))
	       (call-interactively 'dired-change-marks))
	    ':keys (dired-key-description 'dired-change-marks
					  'universal-argument))
    ["Mark Symlinks" dired-mark-symlinks t]
    ["Mark Directories" dired-mark-directories t]
    ["Mark Old Backups" dired-clean-directory t]
    ["Mark Executables" dired-mark-executables t]
    ["Flag Backup Files" dired-flag-backup-files t]
    ["Flag Auto-save Files" dired-flag-auto-save-files t]
    ["Set new marker char" dired-set-marker-char t]
    ["Restore marker char" dired-restore-marker-char t]
    ["Marker stack left" dired-marker-stack-left t]
    ["Marker stack right" dired-marker-stack-right t]
    "---"
    ["Mark Files from Other Dired" dired-mark-files-from-other-dired-buffer t]
    ["Mark Files from Compile Buffer..." dired-mark-files-compilation-buffer t]))

   (setq
   dired-do-menu
   '(["Copy to..." dired-do-copy t]
     ["Rename to..." dired-do-rename t]
     ["Expunge File Flagged for Deletion" dired-expunge-deletions t]
     ["Compress/Uncompress" dired-do-compress t]
     ["Uuencode/Uudecode" dired-do-uucode t]
     ["Print..." dired-do-print t]
     ["Change Mode..." dired-do-interactive-chmod t]
     ["Change Owner..." dired-do-chown t]
     ["Change Group..." dired-do-chgrp t]
     ["Byte-compile" dired-do-byte-compile t]
     ["Hardlink to..." dired-do-hardlink t]
     ["Symlink to..." dired-do-symlink t]
     ["Shell Command..." dired-do-shell-command t]
     ["Background Shell Command..." dired-do-background-shell-command t]
     ["Delete Marked Files" dired-do-delete t]
     ["Visit file menu >" dired-visit-popup-menu-internal t]
     ["Operate on file menu >" dired-do-popup-menu-internal t]))

  (setq
   dired-regexp-menu
   (list
    ["Mark..." dired-mark-files-regexp t]
    ["Mark Files with Extension..." dired-mark-extension t]
    ["Flag..." dired-flag-files-regexp t]
    ["Flag Files with Extension..." dired-flag-extension t]
    ["Downcase" dired-downcase t]
    ["Upcase" dired-upcase t]
    ["Copy..." dired-do-copy-regexp t]
    ["Rename..." dired-do-rename-regexp t]
    ["Hardlink..." dired-do-hardlink-regexp t]
    ["Symlink..." dired-do-symlink-regexp t]
    ["Relative Symlink..." dired-do-relsymlink-regexp t]
    "---"
    ["Add Omit Regex..." dired-add-omit-regexp t]
    (vector "Remove Omit Regex..."
	    '(let ((current-prefix-arg 1))
	       (call-interactively 'dired-add-omit-regexp))
	    ':keys (dired-key-description 'dired-add-omit-regexp 1))
    (vector "Add Omit Extension..."
	    '(let ((current-prefix-arg '(4)))
	       (call-interactively 'dired-add-omit-regexp))
	    ':keys (dired-key-description 'dired-add-omit-regexp 'universal-argument))
    (vector "Remove Omit Extension..."
	    '(let ((current-prefix-arg '(16)))
	       (call-interactively 'dired-add-omit-regexp))
	    ':keys (dired-key-description 'dired-add-omit-regexp
					  'universal-argument 'universal-argument))
    (vector "Show Omit Regex"
	    '(let ((current-prefix-arg 0))
	       (call-interactively 'dired-add-omit-regexp))
	    ':keys (dired-key-description 'dired-add-omit-regexp 0))))

  (setq
   dired-look-menu
   '(["Grep for..." dired-do-grep t]
     ["Tags Search for..." dired-do-tags-search t]
     ["Tags Query Replace..." dired-do-tags-query-replace t]
     "---"
     ["Diff File..." dired-diff t]
     ["Diff with Backup" dired-backup-diff t]
     ["Merge Files..." dired-emerge t]
     ["Merge Files Having Common Ancestor..." dired-emerge-with-ancestor t]
     ["Ediff Files..." dired-ediff t]
     ["Patch File" dired-epatch t]))

  (setq
   dired-sort-menu
   (list
    ["Toggle Current Subdir by Name/Date" dired-sort-toggle-or-edit t]
    (vector "Show Current Switches"
	    '(dired-sort-toggle-or-edit 0)
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit 0))
    (vector "Edit Switches for Current Subdir..."
	    '(dired-sort-toggle-or-edit 1)
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit 1))
    (vector "Edit Default Switches for Inserted Subdirs..."
	    '(dired-sort-toggle-or-edit 2) 
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit 2))
    (vector "Sort Entire Buffer by Date"
	    '(dired-sort-toggle-or-edit 'date)
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit
					  'universal-argument))
    (vector "Sort Entire Buffer by Name"
	    '(dired-sort-toggle-or-edit 'name)
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit
					  'universal-argument))
    (vector "Edit Switches for Entire Buffer..."
	    '(dired-sort-toggle-or-edit '(16))
	    ':keys (dired-key-description 'dired-sort-toggle-or-edit
					  'universal-argument))
    "---"
    ["Hide All Subdirs" dired-hide-all t]
    ["Hide Subdir" dired-hide-subdir t]
    ["Toggle Omit" dired-omit-toggle t]
    ["Kill Marked Lines" dired-do-kill-file-lines t]
    (vector "Redisplay Killed Lines"
	    '(dired-do-kill-file-lines 0)
	    ':keys (dired-key-description 'dired-do-kill-file-lines "0"))))
  (setq
   dired-help-menu
   (list
    ["Dired Summary Help" dired-summary t]
    ["Describe Dired" dired-describe-mode t]
    (vector "Dired Info Manual"
	    '(dired-describe-mode t)
	    ':keys (dired-key-description 'dired-describe-mode
					  'universal-argument))
    ["Dired Command Apropos" dired-apropos t]
    (vector "Dired Variable Apropos"
	    '(let ((current-prefix-arg t))
	       (call-interactively 'dired-apropos))
	    ':keys (dired-key-description 'dired-apropos 'universal-argument))
    ["Report Dired Bug" dired-report-bug t])))

(defun dired-install-menubar ()
  "Installs the Dired menu at the menubar."
  (if (null dired-help-menu)
      (dired-setup-menus))
  (if current-menubar
      (progn
	(let ((buffer-menubar (copy-sequence current-menubar)))
	  (delete (assoc "Edit" buffer-menubar) buffer-menubar)
	  (set-buffer-menubar buffer-menubar)
	  (mapcar
	   (function
	    (lambda (pair)
	      (let ((name (car pair))
		    (menu (symbol-value (cdr pair))))
		(add-submenu nil (cons name menu)))))
	   dired-menubar-menus))
	(add-menu-button '("Help") (list "---"))
	(add-submenu '("Help") (cons "Dired" dired-help-menu)))))

(add-hook 'dired-mode-hook 'dired-install-menubar)

;;; Mouse functions

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (event-point event))
	  (dired-find-file))
      (error
       (concat "dired-subdir-alist seems to be mangled.  "
	       (substitute-command-keys
		"\\<dired-mode-map>Try dired-revert (\\[dired-revert])."))))))

(defun dired-mouse-mark (event)
  "In dired, mark the file name that you click on.
If the file name is already marked, this unmarks it."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (event-point event))
	  (beginning-of-line)
	  (if (looking-at dired-re-mark)
	      (dired-unmark 1)
	    (dired-mark 1)))
      (error
       (concat "dired-subdir-alist seems to be mangled.  "
	       (substitute-command-keys
		"\\<dired-mode-map>Try dired-revert (\\[dired-revert])."))))))

(defun dired-mouse-flag (event)
  "In dired, flag for deletion the file name that you click on.
If the file name is already flag, this unflags it."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (event-point event))
	  (beginning-of-line)
	  (if (char-equal (following-char) dired-del-marker)
	      (dired-unflag 1)
	    (dired-flag-file-deletion 1)))
      (error
       (concat "dired-subdir-alist seems to be mangled.  "
	       (substitute-command-keys
		"\\<dired-mode-map>Try dired-revert (\\[dired-revert])."))))))

(defun dired-mouse-get-target (event)
  "In dired, put a copy of the selected directory in the active minibuffer."
  (interactive "e")
  (let ((obuff (current-buffer))
	mb)
    (set-buffer (window-buffer (event-window event)))
    (if (and dired-subdir-alist (setq mb (dired-get-active-minibuffer-window)))
	(let (dir)
	  (goto-char (event-point event))
	  (setq dir (dired-current-directory))
	  (select-window mb)
	  (set-buffer (window-buffer mb))
	  (erase-buffer)
	  (insert dir))
      (set-buffer obuff)
      (if mb
	  (error "No directory specified")
	(error "No active minibuffer")))))

(defun dired-visit-popup-menu (event)
  "Popup a menu to visit the moused file."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (save-excursion
      (goto-char (event-point event))
      (dired-visit-popup-menu-internal event))))

(defun dired-visit-popup-menu-internal (event)
  (interactive "e")
  (let ((fn (dired-get-filename 'no-dir)))
    (popup-menu
     (cons (concat "Visit " fn " with") dired-visit-popup-menu))
    ;; this looks like a kludge to me ...
    (while (popup-up-p)
      (dispatch-event (next-event)))))

(defun dired-do-popup-menu (event)
  "Pop up a menu to do an operation on the moused file."
  (interactive "e")
  (let ((obuff (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (window-buffer (event-window event)))
	  (dired-save-excursion
	    (goto-char (event-point event))
	    (dired-do-popup-menu-internal event)))
      (set-buffer obuff))))

(defun dired-do-popup-menu-internal (event)
  (interactive "e")
  (let ((fn (dired-get-filename 'no-dir))
	(current-prefix-arg 1))
    (popup-menu
     (cons (concat "Do operation on " fn) dired-do-popup-menu))
    (while (popup-up-p)
      (dispatch-event (next-event)))))

(defvar dired-filename-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'dired-filename-local-map)
    (define-key map 'button2 'dired-mouse-find-file)
    (define-key map 'button3 'dired-visit-popup-menu)
    (define-key map '(control button2) 'dired-do-popup-menu)
    (define-key map '(shift button1) 'dired-mouse-mark)
    (define-key map '(control shift button1) 'dired-mouse-flag)
    map)
  "Keymap used to activate actions on files in dired.")

;; Make this defined everywhere in the dired buffer.
(define-key dired-mode-map '(meta button3) 'dired-mouse-get-target)

;;; Extent managment

(defun dired-set-text-properties (start end &optional face)
  (let ((filename-extent (make-extent start end)))
    (set-extent-face filename-extent (or face 'default))
    (set-extent-property filename-extent 'dired-file-name t)
    (set-extent-property filename-extent 'start-open t)
    (set-extent-property filename-extent 'end-open t)
    (set-extent-property filename-extent 'keymap dired-filename-local-map)
    (set-extent-property filename-extent 'highlight t)
    (set-extent-property
     filename-extent 'help-echo
     (concat
      "button2 finds, button3 visits, "
      "C-button2 file ops, [C-]shift-button1 marks/flags."))
    filename-extent))

(defun dired-insert-set-properties (beg end)
  ;; Sets the extents for the file names and their properties
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let ((eol (save-excursion (end-of-line) (point)))
          (bol (point))
          start)
      (while (< (point) end)
        (setq eol (save-excursion (end-of-line) (point))) 

	(if dired-do-interactive-permissions
	    (dired-make-permissions-interactive (point)))

        (if (dired-manual-move-to-filename nil bol eol)
            (progn
              (setq start (point))
              (dired-manual-move-to-end-of-filename nil bol eol)
	      (dired-set-text-properties
	       start
	       (point)
	       (save-excursion
		 (beginning-of-line)
		 (cond
		  ((null dired-do-highlighting) nil)
		  ((looking-at dired-re-raw-dir) 'dired-face-directory)
		  ((looking-at dired-re-raw-sym) 'dired-face-symlink)
		  ((or (looking-at dired-re-raw-setuid)
		       (looking-at dired-re-raw-setgid)) 'dired-face-setuid)
		  ((looking-at dired-re-raw-exe) 'dired-face-executable)
		  ((looking-at dired-re-raw-socket) 'dired-face-socket)
		  ((save-excursion
		     (goto-char start)
		     (re-search-forward dired-re-raw-boring eol t))
		   'dired-face-boring))))))

        (setq bol (1+ eol))
        (goto-char bol)))))

(defun dired-remove-text-properties (start end)
  ;; Removes text properties.  Called in popup buffers.
  (map-extents
   (function
    (lambda (extent maparg)
      (if (extent-property extent 'dired-file-name)
	  (delete-extent extent))
      nil))
   nil start end))

(defun dired-highlight-filename-mark (extent)
  (let ((mark
	 (save-excursion
	   (skip-chars-backward "^\n\r")
	   (following-char)))
	(face (extent-face extent)))
    (if (char-equal mark ?\ )
	(if (consp face)
	    (set-extent-face extent (cadr face)))
      (let ((new-face
	     (cond
	      ((char-equal dired-default-marker mark)
	       'dired-face-marked)
	      ((char-equal dired-del-marker mark)
	       'dired-face-flagged)
	      (t 'default))))
	(set-extent-face
	 extent
	 (if (consp face)
	     (list new-face (cadr face))
	   (list new-face face)))))))

(defun dired-move-to-filename (&optional raise-error bol eol)
  (or bol (setq bol (save-excursion
		      (skip-chars-backward "^\n\r")
		      (point))))
  (or eol (setq eol (save-excursion
		      (skip-chars-forward "^\n\r")
		      (point))))
  (goto-char bol)
  (let ((extent
	 (map-extents
	  (function
	   (lambda (extent maparg)
	     (if (extent-property extent 'dired-file-name)
		 extent
	       nil)))
	  nil bol eol)))
    (if extent
	(progn
	  (if dired-do-highlighting
	      (dired-highlight-filename-mark extent))
	  (goto-char (extent-start-position extent)))
      (if raise-error
	  (error "No file on this line")
	nil))))


(defun dired-move-to-end-of-filename (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (and
   (null no-error)
   selective-display
   (or bol (setq bol (save-excursion (skip-chars-backward "^\r\n") (point))))
   (eq (char-after (1- bol)) ?\r)
   (cond
    ((dired-subdir-hidden-p (dired-current-directory))
     (error
      (substitute-command-keys
       "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
    ((error
      (substitute-command-keys
       "File line is omitted. Type \\[dired-omit-toggle] to un-omit.")))))
  (let ((filename-extent  (map-extents
 			    (function
 			     (lambda (e p) (and (extent-property e p) e)))
 			    (current-buffer) bol eol 'dired-file-name)))
    (if filename-extent
	(goto-char (extent-end-position filename-extent))
      (and (null no-error) (error "No file on this line")))))

;;; Interactive chmod
;;; (based on ideas from Russell Ritchie's dired-chmod.el)

(defun dired-do-interactive-chmod (new-attribute)
  (let* ((file (dired-get-filename))
	 (operation (concat "chmod " new-attribute " " file))
	 (failure (apply (function dired-check-process)
			 operation
			 "chmod" new-attribute (list file))))
    (dired-do-redisplay) 
    (if failure
	(dired-log-summary (buffer-name (current-buffer))
			   (format "%s: error" operation) nil))))

(defun dired-chmod-popup-menu (event menu)
  (save-excursion
    (set-buffer (window-buffer (event-window event)))
    (save-excursion
      (goto-char (event-point event))
      (popup-menu menu)
      ;; this looks like a kludge to me ...
      (while (popup-up-p)
	(dispatch-event (next-event))))))

;; This is probably overdoing it.
;; Someone give me lexical scoping here ...

(defun dired-setup-chmod-keymap (domain id keys &optional toggle-keys)
  (let* ((names
	  (mapcar
	   (function
	    (lambda (key)
	      (let ((name (intern (concat "dired-"
					  (list domain ?-  key)))))
		(eval
		 `(defun ,name ()
		    (interactive)
		    (dired-do-interactive-chmod ,(concat (list domain ?+ key)))))
		name)))
	   keys))
	 (prefix (concat "dired-" (list domain) "-" (list id)))
	 (remove-name (intern (concat prefix "-remove")))
	 (toggle-name (intern (concat prefix "-toggle")))
	 (mouse-toggle-name (intern (concat prefix "-mouse-toggle")))
	 (mouse-menu-name (intern (concat prefix "-menu"))))

    (eval
     `(defun ,remove-name ()
	(interactive)
	(cond ,@(mapcar (function
			 (lambda (key)
			   `((looking-at ,(regexp-quote (char-to-string key)))
			     (dired-do-interactive-chmod
			      ,(concat (list domain ?- key))))))
			keys))))

    (eval
     `(defun ,toggle-name ()
	(interactive)
	(cond ((looking-at "-") (dired-do-interactive-chmod
				 ,(concat (list domain ?+ (car keys)))))
	      ,@(let ((l (or toggle-keys keys))
		      (c '()))
		  (while l
		    (setq c
			  (cons
			   `((looking-at (regexp-quote (char-to-string ,(car l))))
			     (dired-do-interactive-chmod
			      ,(if (null (cdr l))
				   (concat (list domain ?- (car l)))
				 (concat (list domain ?+ (cadr l))))))
			   c))
		    (setq l (cdr l)))
		  (reverse c))
	      (t (dired-do-interactive-chmod
		  ,(concat (list domain ?+ (car keys))))))))

    (eval
     `(defun ,mouse-toggle-name (event)
	(interactive "e")
	(save-excursion
	  (set-buffer (window-buffer (event-window event)))
	  (save-excursion
	    (goto-char (event-point event))
	    (,toggle-name)))))

    (let ((menu '())
	  (loop-keys keys)
	  (loop-names names))
      (while loop-keys
	(setq menu
	      (cons (vector (concat (list ?+ (car loop-keys)))
			    (car loop-names)
			    t)
		    menu))
	(setq loop-keys (cdr loop-keys)
	      loop-names (cdr loop-names)))
      (setq menu (append menu (list (vector "Toggle" toggle-name t)
				    (vector "Clear" remove-name t))))
      (setq menu (cons (char-to-string domain) menu))

      (eval
       `(defun ,mouse-menu-name (event)
	  (interactive "e")
	  (dired-chmod-popup-menu event ',menu))))

    (let ((keymap (make-sparse-keymap)))
      (let ((loop-keys (cons ?. (cons ?- keys)))
	    (loop-names (cons toggle-name (cons remove-name names))))
	(while loop-keys
	  (define-key keymap (car loop-keys) (car loop-names))
	  (setq loop-keys (cdr loop-keys)
		loop-names (cdr loop-names))))

      (define-key keymap 'button2 mouse-toggle-name)
      (define-key keymap 'button3 mouse-menu-name)
      keymap)))
    
(defvar dired-u-r-keymap nil "internal keymap for dired")
(defvar dired-u-w-keymap nil "internal keymap for dired")
(defvar dired-u-x-keymap nil "internal keymap for dired")
(defvar dired-g-r-keymap nil "internal keymap for dired")
(defvar dired-g-w-keymap nil "internal keymap for dired")
(defvar dired-g-x-keymap nil "internal keymap for dired")
(defvar dired-o-r-keymap nil "internal keymap for dired")
(defvar dired-o-w-keymap nil "internal keymap for dired")
(defvar dired-o-x-keymap nil "internal keymap for dired")


(defun dired-setup-chmod-keymaps ()
  (setq
   dired-u-r-keymap (dired-setup-chmod-keymap ?u ?r '(?r))
   dired-u-w-keymap (dired-setup-chmod-keymap ?u ?w '(?w))
   dired-u-x-keymap (dired-setup-chmod-keymap ?u ?x '(?x ?s ?S) '(?x))
   dired-g-r-keymap (dired-setup-chmod-keymap ?g ?r '(?r))
   dired-g-w-keymap (dired-setup-chmod-keymap ?g ?w '(?w))
   dired-g-x-keymap (dired-setup-chmod-keymap ?g ?x '(?x ?s ?S) '(?x))
   dired-o-r-keymap (dired-setup-chmod-keymap ?o ?r '(?r))
   dired-o-w-keymap (dired-setup-chmod-keymap ?o ?w '(?w))
   dired-o-x-keymap (dired-setup-chmod-keymap ?o ?x '(?x ?s ?t) '(?x))))

(defun dired-make-permissions-interactive (beg)
  (save-excursion
    (goto-char beg)
    (buffer-substring (point) (save-excursion (end-of-line) (point)))
    (if (and (re-search-forward dired-re-pre-permissions
				(save-excursion (end-of-line) (point))
				t)
	     (looking-at dired-re-permissions))
	(let ((p (point)))
	  (dired-activate-permissions (make-extent p (+ 1 p)) dired-u-r-keymap)
	  (dired-activate-permissions (make-extent (+ 1 p) (+ 2 p)) dired-u-w-keymap)
	  (dired-activate-permissions (make-extent (+ 2 p) (+ 3 p)) dired-u-x-keymap)
	  (dired-activate-permissions (make-extent (+ 3 p) (+ 4 p)) dired-g-r-keymap)
	  (dired-activate-permissions (make-extent (+ 4 p) (+ 5 p)) dired-g-w-keymap)
	  (dired-activate-permissions (make-extent (+ 5 p) (+ 6 p)) dired-g-x-keymap)
	  (dired-activate-permissions (make-extent (+ 6 p) (+ 7 p)) dired-o-r-keymap)
	  (dired-activate-permissions (make-extent (+ 7 p) (+ 8 p)) dired-o-w-keymap)
	  (dired-activate-permissions (make-extent (+ 8 p) (+ 9 p)) dired-o-x-keymap)))))

(defun dired-activate-permissions (extent keymap)
  (set-extent-face extent 'dired-face-permissions)
  (set-extent-property extent 'keymap keymap)
  (set-extent-property extent 'highlight t)
  (set-extent-property
   extent 'help-echo
   "button2 toggles, button3 changes otherwise."))

(dired-setup-chmod-keymaps)
	  
;;; end of dired-xemacs.el
