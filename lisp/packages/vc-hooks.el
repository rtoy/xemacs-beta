;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: ttn@netcom.com
;; Version: 5.3 + CVS hacks by ceder@lysator.liu.se made in Jan-Feb 1994.
;;
;; XEmacs fixes, CVS fixes, and general improvements
;; by Jonathan Stigelman <Stig@hackvan.com>

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.28.

;;; Commentary:

;; See the commentary of vc.el.

;;; Code:

(defvar vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS)
    ("%s%s@@" . CC)
    vc-find-cvs-master)
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defvar vc-make-backup-files nil
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups.")

(defvar vc-display-status t
  "*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed.")

;; Tell Emacs about this new kind of minor mode
;(if (not (assoc 'vc-mode minor-mode-alist))
;    (setq minor-mode-alist (cons '(vc-mode vc-mode)
;				 minor-mode-alist)))
;; NO!  Do it right.
(add-minor-mode 'vc-mode 'vc-mode)

(defvar vc-mode nil)			; used for modeline flag
(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we don't
;; want to recompute it even on every find.

(defmacro vc-error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defvar vc-file-prop-obarray (make-vector 17 0)
  "Obarray for per-file properties.")

(defun vc-file-setprop (file property value)
  ;; set per-file property
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  ;; get per-file property
  (get (intern file vc-file-prop-obarray) property))

;;; actual version-control code starts here

(defun vc-registered (file)
  (let (handler)
    (if (boundp 'file-name-handler-alist)
	(setq handler (find-file-name-handler file 'vc-registered)))
    (if handler
	(funcall handler 'vc-registered file)
      ;; Search for a master corresponding to the given file
      (let ((dirname (or (file-name-directory file) ""))
	    (basename (file-name-nondirectory file)))
	(catch 'found
	  (mapcar
	   (function (lambda (s)
		       (if (atom s)
			   (funcall s dirname basename)
			 (let ((trial (format (car s) dirname basename)))
			   (if (and (file-exists-p trial)
				    ;; Make sure the file we found with name
				    ;; TRIAL is not the source file itself.
				    ;; That can happen with RCS-style names
				    ;; if the file name is truncated
				    ;; (e.g. to 14 chars).  See if either
				    ;; directory or attributes differ.
				    (or (not (string= dirname
						      (file-name-directory trial)))
					(not (equal
					      (file-attributes file)
					      (file-attributes trial)))))
			       (throw 'found (cons trial (cdr s))))))))
	   vc-master-templates)
	  nil)))))

(defun vc-find-cvs-master (dirname basename)
  ;; Check if DIRNAME/BASENAME is handled by CVS.
  ;; If it is, do a (throw 'found (cons MASTER 'CVS)).
  ;; Note: If the file is ``cvs add''ed but not yet ``cvs commit''ed 
  ;; the MASTER will not actually exist yet.  The other parts of VC
  ;; checks for this condition.  This function returns something random if 
  ;; DIRNAME/BASENAME is not handled by CVS.
  (and (string= "" dirname) (setq dirname default-directory))
  (if (and (file-directory-p (concat dirname "CVS/"))
	   (file-readable-p (concat dirname "CVS/Entries")))
      (let ((fname (concat dirname basename))
	    sbuf rev)
	(unwind-protect
	    (save-excursion
	      (set-buffer (generate-new-buffer " vc-scratch"))
	      (setq sbuf (current-buffer))
	      (insert-file-contents (concat dirname "CVS/Entries"))
	      (cond
	       ((re-search-forward
		 (concat "^/" (regexp-quote basename) "/\\([0-9.]*\\)/.*/\\(T\\([^/\n]+\\)\\)?$")
		 nil t)
		;; We found it.  Store version number, and branch tag
		(setq rev (buffer-substring (match-beginning 1)
					    (match-end 1)))
		(vc-file-setprop fname 'vc-your-latest-version rev)
		;; XEmacs - we put something useful in the modeline
		(vc-file-setprop fname 'sticky-tag
				 (cond ((string= "0" rev) "newfile")
				       ((match-beginning 3)
					(buffer-substring (match-beginning 3)
							  (match-end 3)))
				       (t "main")))
		(erase-buffer)
		(insert-file-contents (concat dirname "CVS/Repository"))
		(let ((master
		       (concat (file-name-as-directory 
				(buffer-substring (point-min)
						  (1- (point-max))))
			       basename
			       ",v")))
		  (throw 'found (cons master 'CVS))))))
	  (kill-buffer sbuf)))))

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered."
  (or (vc-file-getprop file 'vc-name)
      (let ((name-and-type (vc-registered file)))
	(if name-and-type
	    (progn
	      (vc-file-setprop file 'vc-backend (cdr name-and-type))
	      (vc-file-setprop file 'vc-name (car name-and-type)))))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered."
  (and file
       (or (vc-file-getprop file 'vc-backend)
           (let ((name-and-type (vc-registered file)))
	     (if name-and-type
		 (progn
		   (vc-file-setprop file 'vc-name (car name-and-type))
		   (vc-file-setprop file 'vc-backend (cdr name-and-type))))))))

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with a form of version control
that locks files by making them read-only (i.e.: not CVS), then check the
file in or out.  Otherwise, just change the read-only flag of the buffer.

If you provide a prefix argument, we pass it on to `vc-next-action'."
  (interactive "P")
  (let ((vc-type (vc-backend-deduce (buffer-file-name))))
    (cond ((and vc-type
		buffer-read-only
		(file-writable-p buffer-file-name)
		(/= 0 (user-uid)))
	   ;; XEmacs - The buffer isn't read-only because it's locked, so
	   ;; keep vc out of this...
	   (toggle-read-only))
	  ((and vc-type (not (eq 'CVS  vc-type)))
	   (vc-next-action verbose))
	  (t
	   (toggle-read-only)))
    ))

(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-file-owner (file)
  ;; XEmacs - vc-locking-user is just WAY too slow.
  (let* ((fa (file-attributes file)))
    (cond ((eq ?w (aref (nth 8 fa) 2))	; -rw-r--r--
	   ;; #### - if it's writable, we trust unix...dumb move?
	   (user-login-name (nth 2 fa)))
	  (t
	   ;; big slowness here...
	   (require 'vc)
	   (vc-locking-user file)
	   ))))

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name."
  (interactive (list buffer-file-name nil))
  (if file
      (let ((vc-type (vc-backend-deduce file)))
	(setq vc-mode
	      (if vc-type
		  (concat " " (or label (symbol-name vc-type))
			  (if vc-display-status
			      (vc-status file vc-type)))))
	;; Even root shouldn't modify a registered file without
	;; locking it first.
	(and vc-type
	     (not (string= (user-login-name) (vc-file-owner file)))
	     (setq buffer-read-only t))
	(and (null vc-type)
	     (file-symlink-p file)
	     (let ((link-type (vc-backend-deduce (file-symlink-p file))))
	       (if link-type
		   (message
		    "Warning: symbolic link to %s-controlled source file"
		    link-type))))
	(redraw-modeline)
	;;(set-buffer-modified-p (buffer-modified-p))  ;;use this if Emacs 18
	vc-type)))

(defun vc-status (file vc-type)
  ;; Return string for placement in modeline by `vc-mode-line'.
  ;; If FILE is not registered, return nil.
  ;; If FILE is registered but not locked, return " REV" if there is a head
  ;; revision and " @@" otherwise.
  ;; If FILE is locked then return all locks in a string of the
  ;; form " LOCKER1:REV1 LOCKER2:REV2 ...", where "LOCKERi:" is empty if you
  ;; are the locker, and otherwise is the name of the locker followed by ":".

  ;; Algorithm: 

  ;; Check for master file corresponding to FILE being visited.
  ;; 
  ;; RCS: Insert the first few characters of the master file into a
  ;; work buffer.  Search work buffer for "locks...;" phrase; if not
  ;; found, then keep inserting more characters until the phrase is
  ;; found.  Extract the locks, and remove control characters
  ;; separating them, like newlines; the string " user1:revision1
  ;; user2:revision2 ..." is returned.
  ;;
  ;; SCCS: Check if the p-file exists.  If it does, read it and
  ;; extract the locks, giving them the right format.  Else use prs to
  ;; find the revision number.
  ;;
  ;; CVS: vc-find-cvs-master has already stored the current revision
  ;; number and sticky-tag for the file.  XEmacs displays the sticky-tag.
  
  ;; Limitations:

  ;; The output doesn't show which version you are actually looking at.
  ;; The modeline can get quite cluttered when there are multiple locks.
  ;; The head revision is probably not what you want if you've used `rcs -b'.

  (let ((master (vc-name file))
	found
	status)

    ;; If master file exists, then parse its contents, otherwise we
    ;; return the nil value of this if form.
    (if (and master vc-type)
        (save-excursion

          ;; Create work buffer.
          (set-buffer (get-buffer-create " *vc-status*"))
          (setq buffer-read-only nil
                default-directory (file-name-directory master))
          (erase-buffer)

	  ;; Set the `status' var to the return value.
	  (cond

	   ;; RCS code.
	   ((eq vc-type 'RCS)
	    ;; Check if we have enough of the header.
	    ;; If not, then keep including more.
            (while
		(not (or found
			 (let ((s (buffer-size)))
			   (goto-char (1+ s))
			   (zerop (car (cdr (insert-file-contents
					     master nil s (+ s 8192))))))))
	      (beginning-of-line)
	      (setq found (re-search-forward "^locks\\([^;]*\\);" nil t)))

	    (if found
		;; Clean control characters and self-locks from text.
		(let* ((lock-pattern
			(concat "[ \b\t\n\v\f\r]+\\("
				(regexp-quote (user-login-name))
                                ":\\)?"))
		       (locks
			(save-restriction
			  (narrow-to-region (match-beginning 1) (match-end 1))
			  (goto-char (point-min))
			  (while (re-search-forward lock-pattern nil t)
			    (replace-match (if (eobp) "" ":") t t))
			  (buffer-string))))
		  (setq status
			(if (not (string-equal locks ""))
			    locks
			  (goto-char (point-min))
			  (if (looking-at "head[ \b\t\n\v\f\r]+\\([.0-9]+\\)")
			      (concat "-"
				      (buffer-substring (match-beginning 1)
							(match-end 1)))
			    " @@"))))))

	   ;; SCCS code.
           ((eq vc-type 'SCCS)
	    ;; Build the name of the p-file and put it in the work buffer.
	    (insert master)
	    (search-backward "/s.")
	    (delete-char 2)
	    (insert "/p")
	    (if (not (file-exists-p (buffer-string)))
		;; No lock.
		(let ((exec-path (if (boundp 'vc-path) (append exec-path vc-path)
				   exec-path)))
		  (erase-buffer)
                  (insert "-")
		  (if (zerop (call-process "prs" nil t nil "-d:I:" master))
		      (setq status (buffer-substring 1 (1- (point-max))))))
	      ;; Locks exist.
	      (insert-file-contents (buffer-string) nil nil nil t)
	      (while (looking-at "[^ ]+ \\([^ ]+\\) \\([^ ]+\\).*\n")
		(replace-match " \\2:\\1"))
	      (setq status (buffer-string))
	      (aset status 0 ?:)))
	   ;; CVS code.
	   ((eq vc-type 'CVS)
	    ;; sticky-tag is initialized by vc-backend-deduce
	    (setq status (concat ":" (vc-file-getprop file 'sticky-tag) "-"
				 (vc-file-getprop file 'vc-your-latest-version)
				 ))
	    )
	   ;; ClearCase code.
	   ((eq vc-type 'CC)
	    (require 'vc)
	    (setq status (concat ":" (vc-latest-version file)))
	    ))

	  ;; Clean work buffer.
	  (erase-buffer)
          (set-buffer-modified-p nil)
	  status))))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (if buffer-file-name
      (vc-file-setprop buffer-file-name 'vc-backend nil))
  (if (and (vc-mode-line buffer-file-name) (not vc-make-backup-files))
      (progn
	;; Use this variable, not make-backup-files,
	;; because this is for things that depend on the file name.
	(set (make-local-variable 'backup-inhibited) t))))

(add-hook 'find-file-hooks 'vc-find-file-hook)

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (save-excursion
	(require 'vc)
	(not (vc-error-occurred (vc-checkout buffer-file-name))))))

(add-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as we'll often
;;; want to call them from random buffers.

; XEmacs - this is preloaded.  let's not be obtuse!
(defconst vc-prefix-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'vc-prefix-map) 
    (define-key map "a" 'vc-update-change-log)
    (define-key map "c" 'vc-cancel-version)
    (define-key map "d" 'vc-directory)
    (define-key map "h" 'vc-insert-headers)
    (define-key map "i" 'vc-register)
    (define-key map "l" 'vc-print-log)
    (define-key map "r" 'vc-retrieve-snapshot)
    (define-key map "s" 'vc-create-snapshot)
    (define-key map "u" 'vc-revert-buffer)
    (define-key map "v" 'vc-next-action)
    (define-key map "=" 'vc-diff)
    (define-key map "?" 'vc-file-status) ; XEmacs - this doesn't fit elsewhere
    (define-key map "~" 'vc-version-other-window)
    (global-set-key "\C-xv" map)
    map
    ))

;; FSF menus...
;; (if (not (boundp 'vc-menu-map))
;;     ;; Don't do the menu bindings if menu-bar.el wasn't loaded to defvar
;;     ;; vc-menu-map.
;;     ()
;;   ;;(define-key vc-menu-map [show-files]
;;   ;;  '("Show Files under VC" . (vc-directory t)))
;;   (define-key vc-menu-map [vc-directory] '("Show Locked Files" . vc-directory))
;;   (define-key vc-menu-map [separator1] '("----"))
;;   (define-key vc-menu-map [vc-rename-file] '("Rename File" . vc-rename-file))
;;   (define-key vc-menu-map [vc-version-other-window]
;;     '("Show Other Version" . vc-version-other-window))
;;   (define-key vc-menu-map [vc-diff] '("Compare with Last Version" . vc-diff))
;;   (define-key vc-menu-map [vc-update-change-log]
;;     '("Update ChangeLog" . vc-update-change-log))
;;   (define-key vc-menu-map [vc-print-log] '("Show History" . vc-print-log))
;;   (define-key vc-menu-map [separator2] '("----"))
;;   (define-key vc-menu-map [undo] '("Undo Last Check-In" . vc-cancel-version))
;;   (define-key vc-menu-map [vc-revert-buffer]
;;     '("Revert to Last Version" . vc-revert-buffer))
;;   (define-key vc-menu-map [vc-insert-header]
;;     '("Insert Header" . vc-insert-headers))
;;   (define-key vc-menu-map [vc-menu-check-in] '("Check In" . vc-next-action))
;;   (define-key vc-menu-map [vc-check-out] '("Check Out" . vc-toggle-read-only))
;;   (define-key vc-menu-map [vc-register] '("Register" . vc-register))
;;   (put 'vc-rename-file 'menu-enable 'vc-mode)
;;   (put 'vc-version-other-window 'menu-enable 'vc-mode)
;;   (put 'vc-diff 'menu-enable 'vc-mode)
;;   (put 'vc-update-change-log 'menu-enable
;;        '(eq (vc-backend-deduce (buffer-file-name)) 'RCS))
;;   (put 'vc-print-log 'menu-enable 'vc-mode)
;;   (put 'vc-cancel-version 'menu-enable 'vc-mode)
;;   (put 'vc-revert-buffer 'menu-enable 'vc-mode)
;;   (put 'vc-insert-headers 'menu-enable 'vc-mode)
;;   (put 'vc-next-action 'menu-enable '(and vc-mode (not buffer-read-only)))
;;   (put 'vc-toggle-read-only 'menu-enable '(and vc-mode buffer-read-only))
;;   (put 'vc-register 'menu-enable '(not vc-mode))
;;   )

;; #### - sync with fsf menus
(defconst vc-menu
  '("VC"
    :filter vc-menu-filter
    ["" 		           vc-next-action		buffer-file-name nil]
    ;; ^^^ this gets changed to checkin, checkout, register, or steal
    ["Show status of"              vc-file-status               nil nil]
    ;;["Show Locked Files"	   vc-directory t] ;; needs new dired
    "----"
    ["Revert to Last Revision"	   vc-revert-buffer    		vc-mode nil]
    ["Cancel Last Checkin"	   vc-cancel-version		vc-mode]
    ["Rename File"		   vc-rename-this-file		vc-mode nil]
    "----"
    ["Diff Against Last Version"   vc-diff			vc-mode]
    ["Diff Between Revisions..."   vc-version-diff		vc-mode]
    ;;["Ediff Between Revisions..."   ediff-revision		vc-mode]
    ["Visit Other Version..."	   vc-version-other-window	vc-mode]
    ["Show Edit History"	   vc-print-log			vc-mode]
    "----"
    ;; The two commented out List functions simply don't work at the
    ;; moment.
    ;;["List Locked Files"	   (vc-directory '(16))		t]
    ["List Locked Files Any User"  vc-directory			t]
    ;;["List Registered Files"	   (vc-directory '(4))		t]
    "----"
    ["Create Snapshot"	    	   vc-create-snapshot 		t]
    ["Retrieve Snapshot"	   vc-retrieve-snapshot		t]
    "----"
    ["CVS Update Directory"   	   cvs-update                   t] ; pcl-cvs
    ;;["Show File Status"	   vc-cvs-file-status		vc-mode]
    )
  "Menubar entry for using the revision control system.")

(defun vc-menu-filter (menu-items)
  (let* ((result menu-items)		; modify in-place
	 (case-fold-search t)
	 (type (vc-backend-deduce buffer-file-name))
	 (file (if buffer-file-name
		   (file-name-nondirectory buffer-file-name)
		 (buffer-name)))
	 op owner item status)
    (setq op (cond ((null type)
		    "Register File")
		   ((eq type 'CVS)
		    (setq status
			  (vc-file-getprop buffer-file-name 'cvs-status))
		    (if status
			(cdr (assoc status
				    '(("Locally Modified" . "Commit")
				      ("Needs Merge" . "Merge with repository")
				      ("Up-to-date" . "Do nothing to")
				      ("Needs Checkout" . "Update"))))
		      ;; #### - we're not gonna call cvs status just to
		      ;; post a lousy menu...that's insane!
		      "Next action on" 
		      ))
		   ;; these are all for RCS and SCCS
		   ((not (setq owner (vc-file-owner file)))
		    ;; #### - ugh!  this is broken.
		    ;; vc-file-owner is not a suitable
		    ;; substitute for vc-locking-user.
		    "Check out File")
		   ((not (string-equal owner (user-login-name)))
		    "Steal File Lock")
		   (t "Check in File")))
    (while (setq item (pop menu-items))
      (and (vectorp item)
	   (cond ((eq 'vc-next-action (aref item 1))
		  (aset item 0 op)
		  (aset item 3 file))
		 ((eq 'vc-file-status (aref item 1))
		  (aset item 2 (eq 'CVS type))
		  (aset item 3 file))
		 ((> (length item) 3)
		  (aset item 3 file)))))
    result))

(add-hook 'before-init-hook
	  #'(lambda () (and (featurep 'menubar)
	                    current-menubar
			    (car (find-menu-item current-menubar '("Tools")))
			    (add-submenu '("Tools") vc-menu "Compare")
			    (add-menu-button '("Tools") "---" "Compare"))
	      ))

;; #### called by files.el.  Define it like this until we're merged.
(defun vc-after-save ())

(provide 'vc-hooks)

;;; vc-hooks.el ends here
