;;; edit-toolbar.el --- Interactive toolbar editing mode for XEmacs

;; Copyright (C) 1996 Peter D. Pezaris

;; Author: Peter D. Pezaris <pez@dwwc.com>
;; Keywords: tools

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; To use edit-toolbar.el, simply type M-x edit-toolbar RET

;; For help on the various commands you can type ? in a edit-toolbar
;; buffer.  To save a modified toolbar type C-x C-s in an edit-toolbar
;; buffer.  If you want to use a saved toolbar in your future XEmacs
;; sessions, add the following line of code to your .emacs file:

;;     (load "~/.xemacs/.toolbar")

;; Acknowledgements:

;; Many thanks to Stig <stig@hackvan.com> and Ben Wing <wing@666.com>
;; for writing edit-faces.el, on which much of this code is based.

;; To do:

;; o It would be nice if edit-toolbar could edit *any* toolbar, not just
;;   the default one.
;; o The function edit-toolbar-quit should do something other than just
;;   bury the buffer.
;; o Dynamically add new items to edit-toolbar-button-alist as new buttons
;;   are added.

;;; Code:

(defvar edit-toolbar-version "1.01"
  "Version of Edit Toolbar.")

(defvar edit-toolbar-default-toolbar (specifier-instance default-toolbar)
  "Default toolbar used when reverting.")

(defvar edit-toolbar-file-name (concat "~"
				       (if (boundp 'emacs-user-extension-dir)
					   emacs-user-extension-dir
					 "/")
				       ".toolbar")
  "File name to save toolbars to.  Defaults to \"~/.xemacs/.toolbar\"")

(defvar edit-toolbar-menu
  '("Edit Toolbar"
    ["Move This Item Up" edit-toolbar-up t]
    ["Move This Item Down" edit-toolbar-down t]
    ["Set Function" edit-toolbar-set-function t]
    ["Set Help String" edit-toolbar-set-help t]
    ["Remove This Item" edit-toolbar-kill t]
    "----"
    ["Add Button..." edit-toolbar-add-button t]
    ("Add Separator"
     ["2D (narrow)      " edit-toolbar-add-separator-2D-narrow t]
     ["3D (narrow)" edit-toolbar-add-separator-3D-narrow t]
     ["2D (wide)" edit-toolbar-add-separator-2D-wide t]
     ["3D (wide)" edit-toolbar-add-separator-3D-wide t]
     )
    "----"
    ["Restore Default Toolbar      " edit-toolbar-restore t]
    ["Save This Toolbar" edit-toolbar-save t]
    "----"
    ["Help" describe-mode t]
    "----"
    ["Quit" edit-toolbar-quit t]
    )
  )

(defvar edit-toolbar-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'edit-toolbar-quit)
    (define-key map "n" 'edit-toolbar-next)
    (define-key map "p" 'edit-toolbar-previous)
    (define-key map " " 'edit-toolbar-next)
    (define-key map "?" 'describe-mode)
    (define-key map "f" 'edit-toolbar-set-function)
    (define-key map "h" 'edit-toolbar-set-help)
    (define-key map "a" 'edit-toolbar-add-button)
    (define-key map "2" 'edit-toolbar-add-separator-2D-narrow)
    (define-key map "@" 'edit-toolbar-add-separator-2D-wide)
    (define-key map "3" 'edit-toolbar-add-separator-3D-narrow)
    (define-key map "#" 'edit-toolbar-add-separator-3D-wide)
    (define-key map "c" 'edit-toolbar-copy)
    (define-key map "d" 'edit-toolbar-down)
    (define-key map "u" 'edit-toolbar-up)
    (define-key map "k" 'edit-toolbar-kill)
    (define-key map "s" 'edit-toolbar-save)
    (define-key map "\C-x\C-s" 'edit-toolbar-save)
    (define-key map "r" 'edit-toolbar-restore)
    (define-key map 'return 'edit-toolbar-next)
    (define-key map 'delete 'edit-toolbar-previous)
    map
    ))

;;;###autoload
(defun edit-toolbar ()
  "Alter toolbar characteristics by editing a buffer representing the current toolbar.
Pops up a buffer containing a list of the current toobar."
  (interactive)
  (pop-to-buffer (get-buffer-create "*Edit Toolbar*"))
  (edit-toolbar-list)
  (set-buffer-modified-p nil)
  (edit-toolbar-mode)
  (set-face-foreground 'default "black" (current-buffer))
  (set-face-background 'default "grey75" (current-buffer))
  (set-face-foreground 'list-mode-item-selected "yellow" (current-buffer))
  (set-face-background 'list-mode-item-selected "black" (current-buffer)))

(define-derived-mode edit-toolbar-mode list-mode "Edit-Toolbar"
  "Major mode for 'edit-toolbar' buffers.

Editing commands:

\\{edit-toolbar-map}"
  (setq mode-popup-menu edit-toolbar-menu)
  (if current-menubar
      (progn
	(set (make-local-variable 'current-menubar)
	     (copy-sequence current-menubar))
	(add-submenu nil edit-toolbar-menu)))
  (use-local-map edit-toolbar-map)
  (setq buffer-read-only nil)
  (message "Edit Toolbar Version %s.  Type \"?\" for help." edit-toolbar-version))

(defun edit-toolbar-list ()
  (erase-buffer)
  (edit-toolbar-insert-item 'header)
  (let ((ilist (specifier-instance default-toolbar)))
    (while (setq item (car ilist))
      (edit-toolbar-insert-item item)
      (setq ilist (cdr ilist))))
  (goto-char (point-min)))

(defun edit-toolbar-quit ()
  "Quit an Edit Toolbar session.  This simply buries the buffer."
  (interactive)
  ;;FIXME
  (bury-buffer))

(defun edit-toolbar-next ()
  "Move to the next line in the Edit Toolbar buffer."
  (interactive)
  (next-line 1))

(defun edit-toolbar-previous ()
  "Move to the previous line in the Edit Toolbar buffer."
  (interactive)
  (next-line -1))

(defun edit-toolbar-set-function (func)
  "Set the function for the selected toolbar button."
  (interactive "aNew Function: ")
  (let ((toolbar (specifier-instance default-toolbar))
	(index (- (count-lines (point-min) (point)) 2)))
    (setf (aref (nth index toolbar) 1) func)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

(defun edit-toolbar-set-help (help)
  "Set the help string for the selected toolbar button."
  (interactive "sNew Help String: ")
  (let ((toolbar (specifier-instance default-toolbar))
	(index (- (count-lines (point-min) (point)) 2)))
    (setf (aref (nth index toolbar) 3) help)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

(defun edit-toolbar-copy ()
  "Make a copy of the selected toolbar button."
  (interactive)
  (let* ((toolbar (specifier-instance default-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (item (nth index toolbar)))
    (setcdr (nthcdr index toolbar)
	    (cons item (nthcdr (1+ index) toolbar)))
    (edit-toolbar-list)
    (forward-line (+ index 3))))

(defun edit-toolbar-down ()
  "Move the current toolbar button down (right) one position."
  (interactive)
  (let* ((toolbar (specifier-instance default-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (item (nth index toolbar)))
    (if (eq (1+ index) (length toolbar))
	(error "Already at the bottom of the toolbar."))
    (if (eq index 0)
	(setq toolbar (cdr toolbar))
      (setcdr (nthcdr (1- index) toolbar)
	      (nthcdr (1+ index) toolbar)))
    (setcdr (nthcdr index toolbar)
	    (cons item (nthcdr (1+ index) toolbar)))
    (set-specifier default-toolbar toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 3))))

(defun edit-toolbar-up ()
  "Move the current toolbar button up (left) one position."
  (interactive)
  (let* ((toolbar (specifier-instance default-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (item (nth index toolbar)))
    (if (eq index 0)
	(error "Already at the top of the toolbar."))
    (setcdr (nthcdr (1- index) toolbar)
	    (nthcdr (1+ index) toolbar))
    (if (eq index 1)
	(setq toolbar (cons item toolbar))
      (setcdr (nthcdr (- index 2) toolbar)
	      (cons item (nthcdr (- index 1) toolbar))))
    (set-specifier default-toolbar toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 1))))

(defun edit-toolbar-kill ()
  "Remove the current toolbar button."
  (interactive)
  (let* ((toolbar (specifier-instance default-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (item (nth index toolbar)))
    (if (eq index 0)
	(setq toolbar (cdr toolbar))
      (setcdr (nthcdr (1- index) toolbar)
	      (nthcdr (1+ index) toolbar)))
    (set-specifier default-toolbar toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

(defun edit-toolbar-insert-item (item)
  (let ((line-format "%-30s %s\n")
	icon function help)
    (if (eq item 'header)
	(progn
	  (setq function "Function"
		help "Help String")
	  (insert-face "Icon\t" 'bold)
	  (insert-face (format line-format function help) 'bold))
      (cond ((or (eq (aref item 0) :style)
		 (eq (aref item 0) :size))
	     (setq icon nil
		   function "----------------------------------------"
		   help ""))
	    (t
	     (setq icon (if (listp (aref item 0))
			    (car (aref item 0))
			  (car (symbol-value (aref item 0))))
		   function (aref item 1)
		   help (aref item 3))))
      (let ((st (point))
	    (fn #'(lambda (str callback data)
		    (let ((st1 (point)))
		      (insert str)
		      (add-list-mode-item st1 (point) nil callback data)))))
	(insert "\t")
	(funcall fn (format line-format function help) nil item)
	(set-extent-begin-glyph (make-extent st (point)) icon)))))

(defun edit-toolbar-create-button-alist ()
  (let ((button-alist nil)
	(buttons (specifier-instance default-toolbar)))
    (while buttons
      (setq button-alist
	    (cons (cons (symbol-name (aref (car buttons) 1)) (car buttons))
		  button-alist))
      (setq buttons (cdr buttons)))
    button-alist))

(defvar edit-toolbar-button-alist (edit-toolbar-create-button-alist))

(defun edit-toolbar-add-item (item)
  "Add a toolbar item ITEM at the current location."
  (let* ((toolbar (specifier-instance default-toolbar))
	 (index (- (count-lines (point-min) (point)) 2)))
    (if (eq index 0)
	(setq toolbar (cons item toolbar))
      (setcdr (nthcdr (- index 1) toolbar)
	      (cons item (nthcdr index toolbar))))
    (set-specifier default-toolbar toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

;(defun edit-toolbar-check-for-save ()
;  (if (not (buffer-modified-p))
;      ()
;    (if (yes-or-no-p-maybe-dialog-box "

(defun edit-toolbar-restore ()
  "Restore the default toolbar."
  (interactive)
;  (edit-toolbar-check-for-save)
  (set-specifier default-toolbar edit-toolbar-default-toolbar)
  (edit-toolbar-list)
  (set-buffer-modified-p nil))
  
(defun edit-toolbar-add-separator-2D-narrow ()
  "Add a narrow 2D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 2D]))

(defun edit-toolbar-add-separator-3D-narrow ()
  "Add a narrow 3D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 3D]))

(defun edit-toolbar-add-separator-2D-wide ()
  "Add a wide 2D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 2D :size 30]))

(defun edit-toolbar-add-separator-3D-wide ()
  "Add a wide 3D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 3D :size 30]))

(defun edit-toolbar-add-button ()
  "Add a new toolbar item at the current position.
Completion is available to the known toolbar buttons."
  (interactive)
  (let ((button (completing-read
		 "New Toolbar Button (RET to create a new button): "
		 edit-toolbar-button-alist nil t)))
    (if (string-equal button "")
	(let ((prompts '("UP glyph for button: "
			 "DOWN glyph (RET for no glyph): "
			 "DISABLED glyph (RET for no glyph): "
			 "UP CAPTIONED glyph (RET for no glyph): "
			 "DOWN CAPTIONED glyph (RET for no glyph): "
			 "DISABLED CAPTIONED glyph (RET for no glyph): "))
	      (glyphs nil)
	      (count 0))
	  (let ((glyph-file (read-file-name (car prompts) nil "")))
	    (if (string-equal glyph-file "")
		(error "You must specify at least the UP glyph.")
	      (setq glyphs (list (make-glyph glyph-file)))
	      (setq prompts (cdr prompts))))
	  (while prompts
	    (let ((glyph-file (read-file-name (car prompts) nil "")))
	      (if (not (string-equal glyph-file ""))
		  (setq glyphs
			(append glyphs (list (make-glyph glyph-file))))))
	    (setq prompts (cdr prompts)))
	  (let ((func (read-string "Function to call: "))
		(help (read-string "Help String: ")))
	    (setq new-button (vector glyphs (intern func) t help))))
      (let ((match (assoc button edit-toolbar-button-alist)))
	(if match
	    (setq new-button (cdr match))
	  (error "Can't find button %s" button))))
    (edit-toolbar-add-item new-button)))

(defun edit-toolbar-prompt-for-initialization ()
  (popup-dialog-box
   '("Edit Toolbar has created the file ~/.xemacs/.toolbar

In order for your changes to take effect the next time
you start XEmacs, you need to add the following line
to the end of your .emacs file:

    (load \"~/.xemacs/.toolbar\")

Alternatively, I can do this for you now."
     ["Yes, please\nadd the line\nof code for me." edit-toolbar-add-initialization t]
     nil
     ["No thanks,\nI'll take care\nof it myself." ignore t])))

(defun edit-toolbar-add-initialization ()
  "Add a line to the end of the user's init file for edit-toolbar use."
  (interactive)
  (set-buffer (find-file-noselect user-init-file))
  (goto-char (point-max))
  (insert "
(if (and (featurep 'toolbar)
	 (fboundp 'console-on-window-system-p)
	 (console-on-window-system-p)
	 (file-exists-p \"" edit-toolbar-file-name "\"))
    (load-file (expand-file-name \"" edit-toolbar-file-name "\")))
")
  (save-buffer))

(defun edit-toolbar-save ()
  "Save the current toolbar in the file specified by edit-toolbar-file-name."
  (interactive)
  (save-excursion
    (let* ((exists (file-exists-p edit-toolbar-file-name))
	   (buf (find-file-noselect edit-toolbar-file-name))
	   (standard-output buf))
      (set-buffer buf)
      (erase-buffer)
      (insert "(set-specifier default-toolbar '")
      (prin1 (specifier-instance default-toolbar))
      (insert ")")
      (save-buffer)
      (kill-buffer (current-buffer))
      (or exists (edit-toolbar-prompt-for-initialization))))
  (set-buffer-modified-p nil))

(provide 'edit-toolbar)

;;; edit-toolbar.el ends here
