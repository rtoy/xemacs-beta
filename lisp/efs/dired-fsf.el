;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-fsf.el
;; Dired Version: $Revision: 1.1 $
;; RCS:
;; Description:   dired functions for V19 of the original GNU Emacs from FSF
;; Created:       Sat Jan 29 01:38:49 1994 by sandy on ibm550
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Requirements and provisions
(provide 'dired-fsf)
(require 'dired)

;;;; Variables to set.

(setq dired-modeline-tracking-cmds '(mouse-set-point))

;;;; Support for text properties

(defun dired-insert-set-properties (beg end)
  ;; Sets the text properties for the file names.
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let ((eol (save-excursion (end-of-line) (point)))
	  (bol (point)))
      (while (< (point) end)
	(setq eol (save-excursion (end-of-line) (point)))
	(if (dired-manual-move-to-filename nil bol eol)
	    (dired-set-text-properties
	     (point) (dired-manual-move-to-end-of-filename nil bol eol)))
	(goto-char (setq bol (1+ eol)))))))

(defun dired-remove-text-properties (start end &optional object)
  ;; Removes text properties.  Called in popup buffers.
  (remove-text-properties start end '(mouse-face dired-file-name) object))

(defun dired-set-text-properties (start end)
  ;; Sets dired's text properties
  (put-text-property start end 'mouse-face 'highlight)
  (put-text-property start end 'dired-file-name t))

(defun dired-move-to-filename (&optional raise-error bol eol)
  (or bol (setq bol (save-excursion
		      (skip-chars-backward "^\n\r")
		      (point))))
  (or eol (setq eol (save-excursion
		      (skip-chars-forward "^\n\r")
		      (point))))
  (goto-char bol)
  (let ((spot (next-single-property-change bol 'dired-file-name nil eol)))
    (if (=  spot eol)
	(if raise-error
	    (error "No file on this line")
	  nil)
      (goto-char spot))))

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
  (if (get-text-property (point) 'dired-file-name nil)
      (goto-char (next-single-property-change (point) 'dired-file-name
					      nil eol))
    (and (null no-error) (error "No file on this line"))))

;; Text properties do not work properly in pre-19.26.

(if (or (not (boundp 'emacs-major-version))
	(= emacs-major-version 19))
    (progn
      (if (not (boundp 'emacs-minor-version))
	  ;; Argument structure of where-is-internal went through some
	  ;; changes.
	  (defun dired-key-description (cmd &rest prefixes)
	    ;; Return a key description string for a menu.
	    ;; If prefixes are given, they should be either strings,
	    ;; integers, or 'universal-argument.
	    (let ((key (where-is-internal cmd dired-mode-map nil t)))
	      (if key
		  (key-description
		   (apply 'vconcat
			  (append
			   (mapcar
			    (function
			     (lambda (x)
			       (if (eq x 'universal-argument)
				   (where-is-internal 'universal-argument
						      dired-mode-map nil t)
				 x)))
			    prefixes)
			   (list key))))
		""))))
      (if (or (not (boundp 'emacs-minor-version))
	      (< emacs-minor-version 26))
	  (progn
	    (fset 'dired-insert-set-properties 'ignore)
	    (fset 'dired-remove-text-properties 'ignore)
	    (fset 'dired-set-text-properties 'ignore)
	    (fset 'dired-move-to-filename 'dired-manual-move-to-filename)
	    (fset 'dired-move-to-end-of-filename
		  'dired-manual-move-to-end-of-filename)))))

;;;; Keymaps

;;; Caching Menus

(defun dired-menu-item (menu-item cmd width &rest prefixes)
  ;; Return a key description string for a menu.  If prefixes are given,
  ;; they should be either characters, or 'universal-argument.
  (let ((desc (apply 'dired-key-description cmd prefixes)))
    (if (string-equal desc "")
	menu-item
      (concat menu-item
	      (make-string
	       (max (- width (length menu-item) (length desc) 2) 1) 32)
	      "(" desc ")"))))

(defun dired-cache-key (keymap event cmd &rest prefixes)
  ;; Caches a keybinding for cms in a menu keymap.
  ;; This is able to handle prefix keys.
  (let ((desc (apply 'dired-key-description cmd prefixes)))
    (or (string-equal desc "")
	(progn
	  (let ((elt (assq event keymap)))
	    (if elt
		(let ((tail (cdr elt)))
		  (setcdr tail
			  (cons
			   (cons
			    nil (concat "  (" desc ")"))
			   (cdr tail))))))))))

;; Don't cache keys in old emacs versions.  Is 23 the right cut-off point?
(if (or (not (boundp 'emacs-minor-version))
	(< emacs-minor-version 23))
    (fset 'dired-cache-key 'ignore))

(defvar dired-visit-popup-menu nil)
;; Menus of commands in the Visit popup menu.
(defvar dired-do-popup-menu nil)
;; Menu of commands in the dired Do popup menu.

;; Menus for the menu bar.  
(defvar dired-subdir-menu
  (cons "Subdir" (make-sparse-keymap "Subdir")))
(defvar dired-mark-menu
  (cons "Mark" (make-sparse-keymap "Mark")))
(defvar dired-do-menu
  (cons "Do" (make-sparse-keymap "Do")))
(defvar dired-regex-menu
  (cons "Regexp" (make-sparse-keymap "Regex")))
(defvar dired-look-menu
  (cons "Look" (make-sparse-keymap "Look")))
(defvar dired-sort-menu
  (cons "Sort" (make-sparse-keymap "Sort")))
(defvar dired-help-menu nil)

(defun dired-setup-menus ()

  ;;  popup menu

  (setq dired-visit-popup-menu
	(list
	 (cons (dired-menu-item "Find File" 'dired-find-file 35)
	       'dired-advertised-find-file)
	 (cons (dired-menu-item "Find in Other Window"
				'dired-find-file-other-window 35)
	       'dired-find-file-other-window)
	 (cons (dired-menu-item "Find in Other Frame"
				'dired-find-file-other-frame 35)
	       'dired-find-file-other-frame)
	 (cons (dired-menu-item "View File" 'dired-view-file 35)
	       'dired-view-file)
	 (cons (dired-menu-item "Display in Other Window"
				'dired-find-file-other-window 35
				'universal-argument)
	       'dired-display-file)))

  ;; Operate popup menu

  (setq dired-do-popup-menu
	(list
	 (cons (dired-menu-item "Copy to..." 'dired-do-copy 35 1)
	       'dired-do-copy)
	 (cons (dired-menu-item "Rename to..." 'dired-do-rename 35 1)
	       'dired-do-rename)
	 (cons (dired-menu-item "Compress/Uncompress" 'dired-do-compress
				35 1) 'dired-do-compress)
	 (cons (dired-menu-item "Uuencode/Uudecode" 'dired-do-uucode
				35 1) 'dired-do-uucode)
	 (cons (dired-menu-item "Change Mode..." 'dired-do-chmod 35 1)
	       'dired-do-chmod)
	 (cons (dired-menu-item "Change Owner..." 'dired-do-chown 35 1)
	       'dired-do-chown)
	 (cons (dired-menu-item "Change Group..." 'dired-do-chgrp 35 1)
	       'dired-do-chgrp)
	 (cons (dired-menu-item "Load" 'dired-do-load 35 1)
	       'dired-do-load)
	 (cons (dired-menu-item "Byte-compile" 'dired-do-byte-compile 35 1)
	       'dired-do-byte-compile)
	 (cons (dired-menu-item "Hardlink to..." 'dired-do-hardlink 35 1)
	       'dired-do-hardlink)
	 (cons (dired-menu-item "Symlink to..." 'dired-do-symlink 35 1)
	       'dired-do-symlink)
	 (cons (dired-menu-item "Relative Symlink to..."
				'dired-do-relsymlink 35 1)
	       'dired-do-relsymlink)
	 (cons (dired-menu-item "Shell Command..."
				'dired-do-shell-command 35 1)
	       'dired-do-shell-command)
	 (cons (dired-menu-item "Background Shell Command..."
				'dired-do-background-shell-command 35 1)
	       'dired-do-background-shell-command)
	 (cons (dired-menu-item "Delete" 'dired-do-delete 35 1)
	       'dired-do-delete)))

  ;; Subdir Menu-bar Menu
  
  (define-key dired-mode-map [menu-bar subdir] dired-subdir-menu)
  (define-key dired-mode-map [menu-bar subdir uncompress-subdir-files]
    (cons "Uncompress Compressed Files"
	  (function
	   (lambda () (interactive) (dired-compress-subdir-files t)))))
  (dired-cache-key dired-subdir-menu 'uncompress-subdir-files
		   'dired-compress-subdir-files 'universal-argument)
  (define-key dired-mode-map [menu-bar subdir compress-subdir-files]
    '("Compress Uncompressed Files" . dired-compress-subdir-files))
  (define-key dired-mode-map [menu-bar subdir flag]
    '("Flag Files for Deletion" . dired-flag-subdir-files))
  (define-key dired-mode-map [menu-bar subdir mark]
    '("Mark Files" . dired-mark-subdir-files))
  (define-key dired-mode-map [menu-bar subdir redisplay]
    '("Redisplay Subdir" . dired-redisplay-subdir))
  (define-key dired-mode-map [menu-bar subdir subdir-separator]
    '("-- Commands on All Files in Subdir --"))
  (define-key dired-mode-map [menu-bar subdir kill-subdir]
    '("Kill This Subdir" . dired-kill-subdir))
  (define-key dired-mode-map [menu-bar subdir create-directory]
    '("Create Directory..." . dired-create-directory))
  (define-key dired-mode-map [menu-bar subdir insert]
    '("Insert This Subdir" . dired-maybe-insert-subdir))
  (define-key dired-mode-map [menu-bar subdir down-dir]
    '("Down Dir" . dired-down-directory))
  (define-key dired-mode-map [menu-bar subdir up-dir]
    '("Up Dir" . dired-up-directory))
  (define-key dired-mode-map [menu-bar subdir prev-dirline]
    '("Prev Dirline" . dired-prev-dirline))
  (define-key dired-mode-map [menu-bar subdir next-dirline]
    '("Next Dirline" . dired-next-dirline))
  (define-key dired-mode-map [menu-bar subdir prev-subdir]
    '("Prev Subdir" . dired-prev-subdir))
  (define-key dired-mode-map [menu-bar subdir next-subdir]
    '("Next Subdir" . dired-next-subdir))
  
  ;; Mark Menu-bar Menu

  (define-key dired-mode-map [menu-bar mark] dired-mark-menu)
  (define-key dired-mode-map [menu-bar mark mark-from-compilation-buffer]
    '("Mark Files from Compile Buffer..." . dired-mark-files-compilation-buffer))
  (define-key dired-mode-map [menu-bar mark mark-from-other-buffer]
    '("Mark Files from Other Dired" .
      dired-mark-files-from-other-dired-buffer))
  (define-key dired-mode-map [menu-bar mark mark-separator]
    '("--"))
  (define-key dired-mode-map [menu-bar mark marker-char-right]
    '("Marker stack right" . dired-marker-stack-right))
  (define-key dired-mode-map [menu-bar mark marker-char-left]
    '("Marker stack left" . dired-marker-stack-left))
  (define-key dired-mode-map [menu-bar mark restore-marker]
    '("Restore marker char" . dired-restore-marker-char))
  (define-key dired-mode-map [menu-bar mark add-marker]
    '("Set new marker char..." . dired-set-marker-char))
  (define-key dired-mode-map [menu-bar mark auto-save-files]
    '("Flag Auto-save Files" . dired-flag-auto-save-files))
  (define-key dired-mode-map [menu-bar mark backup-files]
    '("Flag Backup Files" . dired-flag-backup-files))
  (define-key dired-mode-map [menu-bar mark executables]
  '("Mark Executables" . dired-mark-executables))
  (define-key dired-mode-map [menu-bar mark directory]
    '("Mark Old Backups" . dired-clean-directory))
  (define-key dired-mode-map [menu-bar mark directories]
    '("Mark Directories" . dired-mark-directories))
  (define-key dired-mode-map [menu-bar mark symlinks]
    '("Mark Symlinks" . dired-mark-symlinks))
  (define-key dired-mode-map [menu-bar mark toggle]
    (cons "Toggle Marks..."
	  (function (lambda () (interactive)
		      (let ((current-prefix-arg t))
			(call-interactively 'dired-change-marks))))))
  (dired-cache-key dired-mark-menu 'toggle 'dired-change-marks
		   'universal-argument)
  (define-key dired-mode-map [menu-bar mark unmark-all]
    '("Unmark All" . dired-unmark-all-files))
  (define-key dired-mode-map [menu-bar mark marks]
    '("Change Marks..." . dired-change-marks))
  (define-key dired-mode-map [menu-bar mark prev]
    '("Previous Marked" . dired-prev-marked-file))
  (define-key dired-mode-map [menu-bar mark next]
    '("Next Marked" . dired-next-marked-file))
 
  ;; Do Menu-bar Menu
  
  (define-key dired-mode-map [menu-bar do]
    dired-do-menu)
  (define-key dired-mode-map [menu-bar do do-popup]
    (cons "Operate on file menu >"
	  'dired-do-popup-menu-internal))
  (dired-cache-key dired-do-menu 'do-popup
		   'dired-do-popup-menu)
  (define-key dired-mode-map [menu-bar do visit-popup]
    (cons "Visit file menu >"
	  'dired-visit-popup-menu-internal))
  (dired-cache-key dired-do-menu 'visit-popup
		   'dired-visit-popup-menu)
  (define-key dired-mode-map [menu-bar do delete]
    '("Delete Marked Files" . dired-do-delete))
  (define-key dired-mode-map [menu-bar do background-command]
    '("Background Shell Command..." . dired-do-background-shell-command))
  (define-key dired-mode-map [menu-bar do command]
    '("Shell Command..." . dired-do-shell-command))
  (define-key dired-mode-map [menu-bar do symlink]
    '("Symlink to..." . dired-do-symlink))
  (define-key dired-mode-map [menu-bar do hardlink]
    '("Hardlink to..." . dired-do-hardlink))
  (define-key dired-mode-map [menu-bar do compile]
    '("Byte-compile" . dired-do-byte-compile))
  (define-key dired-mode-map [menu-bar do load]
    '("Load" . dired-do-load))
  (define-key dired-mode-map [menu-bar do chgrp]
    '("Change Group..." . dired-do-chgrp))
  (define-key dired-mode-map [menu-bar do chown]
    '("Change Owner..." . dired-do-chown))
  (define-key dired-mode-map [menu-bar do chmod]
  '("Change Mode..." . dired-do-chmod))
  (define-key dired-mode-map [menu-bar do print]
    '("Print..." . dired-do-print))
  (define-key dired-mode-map [menu-bar do uucode]
    '("Uuencode/Uudecode" . dired-do-uucode))
  (define-key dired-mode-map [menu-bar do compress]
    '("Compress/Uncompress" . dired-do-compress))
  (define-key dired-mode-map [menu-bar do expunge]
    '("Expunge File Flagged for Deletion" . dired-expunge-deletions))
  (define-key dired-mode-map [menu-bar do rename]
    '("Rename to..." . dired-do-rename))
  (define-key dired-mode-map [menu-bar do copy]
    '("Copy to..." . dired-do-copy))

;; Regex Menu-bar Menu
  
  (define-key dired-mode-map [menu-bar regex] dired-regex-menu)
  (define-key dired-mode-map [menu-bar regex show-omit-regexp]
    (cons "Show Omit Regex"
	  (function
	   (lambda ()
	     (interactive)
	     (let ((current-prefix-arg 0))
	       (call-interactively 'dired-add-omit-regexp))))))
  (dired-cache-key dired-regex-menu 'show-omit-regexp
		   'dired-add-omit-regexp 0)
  (define-key dired-mode-map [menu-bar regex remove-omit-extension]
    (cons "Remove Omit Extension..."
	  (function
	   (lambda ()
	     (interactive)
	     (let ((current-prefix-arg '(16)))
	       (call-interactively 'dired-add-omit-regexp))))))
  (dired-cache-key dired-regex-menu 'remove-omit-extension
		   'dired-add-omit-regexp 'universal-argument
		   'universal-argument)
  (define-key dired-mode-map [menu-bar regex add-omit-extension]
    (cons "Add Omit Extension..."
	  (function
	   (lambda ()
	     (interactive)
	     (let ((current-prefix-arg '(4)))
	       (call-interactively 'dired-add-omit-regexp))))))
  (dired-cache-key dired-regex-menu 'add-omit-extension
		   'dired-add-omit-regexp 'universal-argument)
  (define-key dired-mode-map [menu-bar regex remove-omit-regexp]
    (cons "Remove Omit Regex..."
	  (function
	   (lambda ()
	     (interactive)
	     (let ((current-prefix-arg 1))
	       (call-interactively 'dired-add-omit-regexp))))))
  (dired-cache-key dired-regex-menu 'remove-omit-regexp
		   'dired-add-omit-regexp 1)
  (define-key dired-mode-map [menu-bar regex add-omit-regexp]
    '("Add Omit Regex..." . dired-add-omit-regexp))
  (define-key dired-mode-map [menu-bar regex separator]
    '("--"))
  (define-key dired-mode-map [menu-bar regex relsymlink]
    '("Relative Symlink..." . dired-do-relsymlink-regexp))
  (define-key dired-mode-map [menu-bar regex symlink]
    '("Symlink..." . dired-do-symlink-regexp))
  (define-key dired-mode-map [menu-bar regex hardlink]
    '("Hardlink..." . dired-do-hardlink-regexp))
  (define-key dired-mode-map [menu-bar regex rename]
    '("Rename..." . dired-do-rename-regexp))
  (define-key dired-mode-map [menu-bar regex copy]
    '("Copy..." . dired-do-copy-regexp))
  (define-key dired-mode-map [menu-bar regex upcase]
    '("Upcase" . dired-upcase))
  (define-key dired-mode-map [menu-bar regex downcase]
    '("Downcase" . dired-downcase))
  (define-key dired-mode-map [menu-bar regex dired-flag-extension]
    '("Flag Files with Extension..." . dired-flag-extension))
  (define-key dired-mode-map [menu-bar regex flag]
    '("Flag..." . dired-flag-files-regexp))
  (define-key dired-mode-map [menu-bar regex mark-extension]
    '("Mark Files with Extension..." . dired-mark-extension))
  (define-key dired-mode-map [menu-bar regex mark]
    '("Mark..." . dired-mark-files-regexp))

  ;; Look Menu-bar Menu
  
  (define-key dired-mode-map [menu-bar look] dired-look-menu)
  (define-key dired-mode-map [menu-bar look patch]
    '("Patch File" . dired-epatch))
  (define-key dired-mode-map [menu-bar look ediff]
    '("Ediff Files..." . dired-ediff))
  (define-key dired-mode-map [menu-bar look emerge-with-ancestor]
    '("Merge Files Having Common Ancestor..." . dired-emerge-with-ancestor))
  (define-key dired-mode-map [menu-bar look emerge]
    '("Merge Files..." . dired-emerge))
  (define-key dired-mode-map [menu-bar look backup-diff]
    '("Diff with Backup" . dired-backup-diff))
  (define-key dired-mode-map [menu-bar look diff]
    '("Diff File..." . dired-diff))
  ;; Put in a separator line.
  (define-key dired-mode-map [menu-bar look look-separator]
    '("--"))
  (define-key dired-mode-map [menu-bar look tags-query-replace]
    '("Tags Query Replace..." . dired-do-tags-query-replace))
  (define-key dired-mode-map [menu-bar look tags-search]
    '("Tags Search for..." . dired-do-tags-search))
  (define-key dired-mode-map [menu-bar look grep]
    '("Grep for..."  . dired-do-grep))
  
  ;; Sort Menu-bar Menu
  
  (define-key dired-mode-map [menu-bar sort] dired-sort-menu)
  (define-key dired-mode-map [menu-bar sort redisplay-killed]
    (cons "Redisplay Killed Lines"
	  (function (lambda () (interactive) (dired-do-kill-file-lines 0)))))
  (dired-cache-key dired-sort-menu 'redisplay-killed
		   'dired-do-kill-file-lines 0)
  (define-key dired-mode-map [menu-bar sort kill]
    '("Kill Marked Lines" . dired-do-kill-file-lines))
  (define-key dired-mode-map [menu-bar sort toggle-omit]
    '("Toggle Omit" . dired-omit-toggle))
  (define-key dired-mode-map [menu-bar sort hide-subdir]
    '("Hide Subdir" . dired-hide-subdir))
  (define-key dired-mode-map [menu-bar sort hide-all]
    '("Hide All Subdirs" . dired-hide-all))
  (define-key dired-mode-map [menu-bar sort sort-separator]
    '("--"))
  (define-key dired-mode-map [menu-bar sort entire-edit]
    (cons "Edit Switches for Entire Buffer..."
	  (function (lambda () (interactive)
		      (dired-sort-toggle-or-edit '(16))))))
  (dired-cache-key dired-sort-menu 'entire-edit
		   'dired-sort-toggle-or-edit 'universal-argument
		   'universal-argument)
  (define-key dired-mode-map [menu-bar sort entire-name]
    (cons "Sort Entire Buffer by Name"
	  (function (lambda () (interactive)
		      (dired-sort-toggle-or-edit 'name)))))
  (dired-cache-key dired-sort-menu 'entire-name 'dired-sort-toggle-or-edit
		   'universal-argument)
  (define-key dired-mode-map [menu-bar sort entire-date]
    (cons "Sort Entire Buffer by Date"
	  (function (lambda () (interactive)
		      (dired-sort-toggle-or-edit 'date)))))
  (dired-cache-key dired-sort-menu 'entire-date 'dired-sort-toggle-or-edit
		   'universal-argument)
  (define-key dired-mode-map [menu-bar sort new-edit]
    (cons "Edit Default Switches for Inserted Subdirs..."
	  (function (lambda () (interactive) (dired-sort-toggle-or-edit 2)))))
  (dired-cache-key dired-sort-menu 'new-edit 'dired-sort-toggle-or-edit 2)
  (define-key dired-mode-map [menu-bar sort edit]
    (cons "Edit Switches for Current Subdir..."
	  (function (lambda () (interactive) (dired-sort-toggle-or-edit 1)))))
  (dired-cache-key dired-sort-menu 'edit 'dired-sort-toggle-or-edit 1)
  (define-key dired-mode-map [menu-bar sort show]
    (cons "Show Current Switches"
	  (function (lambda () (interactive) (dired-sort-toggle-or-edit 0)))))
  (dired-cache-key dired-sort-menu 'show 'dired-sort-toggle-or-edit 0)
  (define-key dired-mode-map [menu-bar sort toggle]
    '("Toggle Current Subdir by Name/Date" . dired-sort-toggle-or-edit))

  ;; Help Menu-bar Menu
  
  (or dired-help-menu
      (setq dired-help-menu
	    (if (and (boundp 'menu-bar-help-menu) (keymapp menu-bar-help-menu))
		(cons "Help" (cons 'keymap (cdr menu-bar-help-menu)))
	      (cons "Help" (make-sparse-keymap "Help")))))
  (define-key dired-mode-map [menu-bar dired-help] dired-help-menu)
  (define-key dired-mode-map [menu-bar dired-help help-separator]
    '("--"))
  (define-key dired-mode-map [menu-bar dired-help dired-bug]
    '("Report Dired Bug" . dired-report-bug))
  (define-key dired-mode-map [menu-bar dired-help dired-var-apropos]
    (cons "Dired Variable Apropos"
	  (function (lambda ()
		      (interactive)
		      (let ((current-prefix-arg t))
			(call-interactively 'dired-apropos))))))
  (dired-cache-key dired-help-menu 'dired-var-apropos
		   'dired-apropos 'universal-argument)
  (define-key dired-mode-map [menu-bar dired-help dired-apropos]
    '("Dired Command Apropos" . dired-apropos))
  (define-key dired-mode-map [menu-bar dired-help dired-info]
    (cons "Dired Info Manual"
	  (function (lambda ()
		      (interactive)
		      (dired-describe-mode t)))))
  (dired-cache-key dired-help-menu 'dired-info 'dired-describe-mode
		   'universal-argument)
  (define-key dired-mode-map [menu-bar dired-help dired-describe-mode]
    '("Describe Dired" . dired-describe-mode))
  (define-key dired-mode-map [menu-bar dired-help dired-summary]
    '("Dired Summary Help" . dired-summary)))

(add-hook 'dired-setup-keys-hook 'dired-setup-menus)

;;; Mouse functions

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (posn-point (event-end event)))
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
    (set-buffer (window-buffer (posn-window (event-end event))))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (posn-point (event-end event)))
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
    (set-buffer (window-buffer (posn-window (event-end event))))
    (if dired-subdir-alist
	(save-excursion
	  (goto-char (posn-point (event-end event)))
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
    (set-buffer (window-buffer (posn-window (event-end event))))
    (if (and dired-subdir-alist (setq mb (dired-get-active-minibuffer-window)))
	(let (dir)
	  (goto-char (posn-point (event-end event)))
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
    (set-buffer (window-buffer (posn-window (event-end event))))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (dired-visit-popup-menu-internal event))))

(defun dired-visit-popup-menu-internal (event)
  (interactive "e")
  (let ((fn (dired-get-filename 'no-dir))
	fun)
    (dired-remove-text-properties 0 (length fn) fn)
    (setq fun (x-popup-menu
	      event
	      (list "Visit popup menu"
		    (cons
		     (concat "Visit " fn " with")
		     dired-visit-popup-menu))))
    (if fun (funcall fun))))

(defun dired-do-popup-menu (event)
  ;; Pop up a menu do an operation on the moused file.
  (interactive "e")
  (let ((obuff (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (window-buffer (posn-window (event-end event))))
	  (dired-save-excursion
	    (goto-char (posn-point (event-end event)))
	    (dired-do-popup-menu-internal event)))
      (set-buffer obuff))))

(defun dired-do-popup-menu-internal (event)
  (interactive "e")
  (let ((fn (dired-get-filename 'no-dir))
	fun)
    (dired-remove-text-properties 0 (length fn) fn)
    (setq fun (x-popup-menu
	       event
	       (list "Do popup menu"
		     (cons
		      (concat "Do operation on " fn)
		      dired-do-popup-menu))))
    (dired-save-excursion
      (if fun (let ((current-prefix-arg 1))
		(call-interactively fun))))))

;;; Key maps

;; Get rid of the Edit menu bar item to save space.
(define-key dired-mode-map [menu-bar edit] 'undefined)
;; We have our own help item
(define-key dired-mode-map [menu-bar help] 'undefined)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
(define-key dired-mode-map [S-mouse-1] 'dired-mouse-mark)
(define-key dired-mode-map [C-S-mouse-1] 'dired-mouse-flag)
(define-key dired-mode-map [down-mouse-3] 'dired-visit-popup-menu)
;; This can be useful in dired, so move to double click.
(define-key dired-mode-map [double-mouse-3] 'mouse-save-then-kill)
(define-key dired-mode-map [C-down-mouse-2] 'dired-do-popup-menu)
(define-key dired-mode-map [M-mouse-2] 'dired-mouse-get-target)

(or (memq 'dired-help menu-bar-final-items)
    (setq menu-bar-final-items (cons 'dired-help menu-bar-final-items)))

;;; end of dired-fsf.el
