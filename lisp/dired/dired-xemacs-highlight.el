;;; Copyright (C) 1993 Cengiz Alaettinoglu
;;; Cengiz Alaettinoglu <ca@cs.umd.edu>

;;; Copyright (C) 1991 Tim Wilson and Sebastian Kremer
;;; Tim.Wilson@cl.cam.ac.uk
;;; Sebastian Kremer <sk@thp.uni-koeln.de>
;;; Modified to work with XEmacs

;; Keywords: dired extensions, faces

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not synched with FSF.


; How to install
; (add-hook 'dired-load-hook '(lambda () (require 'dired-xemacs-highlight)) t)

(require 'dired)
(require 'dired-extra "dired-x")
(provide 'dired-xemacs-highlight)

(or (find-face 'dired-face-marked)
    (and
     (make-face 'dired-face-marked)
     (or (face-differs-from-default-p 'dired-face-marked)
	 (if (eq (device-class) 'color)
	     (progn
	       (set-face-foreground 'dired-face-marked (face-foreground 'default))
	       (set-face-background 'dired-face-marked "PaleVioletRed"))
	   (set-face-underline-p 'dired-face-marked t)))))

(or (find-face 'dired-face-deleted)
    (and
     (make-face 'dired-face-deleted)
     (or (face-differs-from-default-p 'dired-face-deleted)
	 (if (eq (device-class) 'color)
	     (progn
	       (set-face-foreground 'dired-face-deleted
				    (face-foreground 'default))
	       (set-face-background 'dired-face-deleted "LightSlateGray"))
	   (set-face-underline-p 'dired-face-deleted t)))))

(or (find-face 'dired-face-directory)
    (and
     (make-face 'dired-face-directory)
     (or (face-differs-from-default-p 'dired-face-directory)
	 (if (eq (device-class) 'color)
	     (progn 
	       (set-face-foreground 'dired-face-directory
				    (face-foreground 'default))
	       (make-face-bold 'dired-face-directory))
	   (make-face-bold-italic 'dired-face-directory)))))

(or (find-face 'dired-face-executable)
    (and
     (make-face 'dired-face-executable)
     (or (face-differs-from-default-p 'dired-face-executable)
	 (if (eq (device-class) 'color)
	     (set-face-foreground 'dired-face-executable "SeaGreen")
	   (make-face-bold 'dired-face-executable)))))

(or (find-face 'dired-face-setuid)
    (and
     (make-face 'dired-face-setuid)
     (or (face-differs-from-default-p 'dired-face-setuid)
	 (if (eq (device-class) 'color)
	     (set-face-foreground 'dired-face-setuid "Red")
	   (make-face-bold 'dired-face-setuid)))))

(or (find-face 'dired-face-socket)
    (and
     (make-face 'dired-face-socket)
     (or (face-differs-from-default-p 'dired-face-socket)
	 (if (eq (device-class) 'color)
	     (set-face-foreground 'dired-face-socket "Gold")
	   (make-face-italic 'dired-face-socket)))))

(or (find-face 'dired-face-symlink)
    (and
     (make-face 'dired-face-symlink)
     (or (face-differs-from-default-p 'dired-face-symlink)
	 (if (eq (device-class) 'color)
	     (progn 
	       (set-face-foreground 'dired-face-symlink "MediumBlue")
	       (make-face-bold 'dired-face-symlink))
	   (make-face-italic 'dired-face-symlink)))))

(or (find-face 'dired-face-boring)
    (and
     (make-face 'dired-face-boring)
     (or (face-differs-from-default-p 'dired-face-boring)
	 (if (eq (device-class) 'color)
	     (set-face-foreground 'dired-face-boring "Grey")
	   (set-face-background-pixmap
	    'dired-face-boring 
	    [32 2 "\125\125\125\125\252\252\252\252"])))))

(defvar dired-do-permission-highlighting-too nil
  "Set if we think we should use dired-chmod style permission highlighting.
This is determined at first-pass time, to avoid filtering the buffer twice.")

(defvar dired-x11-re-boring (if (fboundp 'dired-omit-regexp)
				(dired-omit-regexp)
			      "^#\\|~$")
  "Regexp to match backup, autosave and otherwise boring files.
Those files are displayed in a boring color such as grey (see
variable `dired-x11-boring-color').")

(defvar dired-re-socket
  (concat dired-re-maybe-mark dired-re-inode-size "s"))

(defvar dired-re-setuid 
  (concat dired-re-maybe-mark dired-re-inode-size
	  "-[-r][-w][Ss][-r][-w][sx][-r][-w][xst]")
  "setuid plain file (even if not executable)")

(defvar dired-re-setgid 
  (concat dired-re-maybe-mark dired-re-inode-size
	  "-[-r][-w][-x][-r][-w][Ss][-r][-w][xst]")
  "setgid plain file (even if not executable)")

(defun dired-xemacs-highlight-one (face)
  (and (dired-move-to-filename t)
       (set-extent-face (make-extent (dired-move-to-filename) 
				     (dired-move-to-end-of-filename)) 
			face)))

(defun dired-xemacs-highlight ()
  (message "Highlighting... directory")
  ;; Let's try to do this in one pass...
  (setq dired-do-permission-highlighting-too
	(or dired-do-permission-highlighting-too (featurep 'dired-chmod)))
  (if (and dired-do-permission-highlighting-too
	   (member 'dired-permissions-highlight dired-after-readin-hook))
      (remove-hook 'dired-after-readin-hook 'dired-permissions-highlight))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (and (not (eolp))
	   (progn
	     (beginning-of-line)
	     (cond
	      ((re-search-forward
		dired-x11-re-boring
		(save-excursion
		  (end-of-line)
		  (point))
		t)
	       (dired-xemacs-highlight-one 'dired-face-boring))
	      ((looking-at dired-re-dir)
	       (dired-xemacs-highlight-one 'dired-face-directory))
	      ((looking-at dired-re-sym)
	       (dired-xemacs-highlight-one 'dired-face-symlink))
	      ((or (looking-at dired-re-setuid)
		   (looking-at dired-re-setgid))
	       (dired-xemacs-highlight-one 'dired-face-setuid))
	      ((looking-at dired-re-exe)
	       (dired-xemacs-highlight-one 'dired-face-executable))
	      ((looking-at dired-re-socket)
	       (dired-xemacs-highlight-one 'dired-face-socket)))
	     (if dired-do-permission-highlighting-too
		 (dired-make-permissions-interactive))))
      (forward-line 1))
    (message "Highlighting...done")
    ))

;FSF's version?
;(defconst dired-font-lock-keywords
;  (list (cons "^\\*.*$" 'dired-face-marked)
;	(cons "^\\D.*$" 'dired-face-deleted)))

(defconst dired-font-lock-keywords (purecopy
  (let ((bn (concat "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|"
		    "Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +[0-9]+ +[0-9:]+")))
    (list
     '("^  [/~].*:$" . bold-italic)				   ; Header
     (list (concat "^\\(\\([^ ].*\\)" bn "\\) \\(.*\\)$") 1 'bold) ; Marked
     (list (concat "^. +d.*" bn " \\(.*\\)$") 2 'bold)		   ; Subdirs
     (list (concat "^. +l.*" bn " \\(.*\\)$") 2 'italic)	   ; Links
     (cons (concat "^. +-..[xsS]......\\|"	; Regular files with executable
		   "^. +-.....[xsS]...\\|"	; or setuid/setgid bits set
		   "^. +-........[xsS]")
	   'bold)
     ;; Possibly we should highlight more types of files differently:
     ;; backups; autosaves; core files?  Those with ignored-extensions?
     )))
  "Expressions to highlight in Dired buffers.")

(put 'dired-mode 'font-lock-keywords 'dired-font-lock-keywords)

(add-hook 'dired-after-readin-hook 'dired-xemacs-highlight)
