;;; Mouse related functions and commands
;;; Copyright (C) 1995 Kyle E. Jones
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

(provide 'vm-mouse)

(defun vm-mouse-fsfemacs-mouse-p ()
  (and (vm-fsfemacs-19-p)
       (fboundp 'set-mouse-position)))

(defun vm-mouse-xemacs-mouse-p ()
  (and (vm-xemacs-p)
       (fboundp 'set-mouse-position)))

(defun vm-mouse-set-mouse-track-highlight (start end)
  (cond ((fboundp 'make-overlay)
	 (let ((o (make-overlay start end)))
	   (overlay-put o 'mouse-face 'highlight)))
	((fboundp 'make-extent)
	 (let ((o (make-extent start end)))
	   (set-extent-property o 'highlight t)))))

(defun vm-mouse-button-2 (event)
  (interactive "e")
  ;; go to where the event occurred
  (cond ((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event))))
	((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))))
  ;; now dispatch depending on where we are
  (cond ((eq major-mode 'vm-summary-mode)
	 (mouse-set-point event)
	 (beginning-of-line)
	 (if (let ((vm-follow-summary-cursor t))
	       (vm-follow-summary-cursor))
	     (progn
	       (vm-select-folder-buffer)
	       (vm-preview-current-message))
	   (setq this-command 'vm-scroll-forward)
	   (call-interactively 'vm-scroll-forward)))
	((memq major-mode '(vm-mode vm-virtual-mode))
	 (cond ((and (vm-mouse-fsfemacs-mouse-p) vm-url-browser)
		(vm-mouse-popup-or-select event))))))

(defun vm-mouse-button-3 (event)
  (interactive "e")
  (if vm-use-menus
      (progn
	;; go to where the event occurred
	(cond ((vm-mouse-xemacs-mouse-p)
	       (set-buffer (window-buffer (event-window event)))
	       (and (event-point event) (goto-char (event-point event))))
	      ((vm-mouse-fsfemacs-mouse-p)
	       (set-buffer (window-buffer (posn-window (event-start event))))
	       (goto-char (posn-point (event-start event)))))
	;; now dispatch depending on where we are
	(cond ((eq major-mode 'vm-summary-mode)
	       (vm-menu-popup-mode-menu event))
	      ((eq major-mode 'vm-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'vm-virtual-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'mail-mode)
	       (vm-menu-popup-mode-menu event))))))

(defun vm-mouse-3-help (object)
  "Use mouse button 3 to see a menu of options.")

(defun vm-mouse-get-mouse-track-string (event)
  (save-excursion
    ;; go to where the event occurred
    (cond ((vm-mouse-xemacs-mouse-p)
	   (set-buffer (window-buffer (event-window event)))
	   (and (event-point event) (goto-char (event-point event))))
	  ((vm-mouse-fsfemacs-mouse-p)
	   (set-buffer (window-buffer (posn-window (event-start event))))
	   (goto-char (posn-point (event-start event)))))
    (cond ((fboundp 'overlays-at)
	   (let ((o-list (overlays-at (point)))
		 (string nil))
	     (while o-list
	       (if (overlay-get (car o-list) 'mouse-face)
		   (setq string (vm-buffer-substring-no-properties
				 (overlay-start (car o-list))
				 (overlay-end (car o-list)))
			 o-list nil)
		 (setq o-list (cdr o-list))))
	     string ))
	  ((fboundp 'extent-at)
	   (let ((e (extent-at (point) nil 'highlight)))
	     (if e
		 (buffer-substring (extent-start-position e)
				   (extent-end-position e))
	       nil)))
	  (t nil))))

(defun vm-mouse-popup-or-select (event)
  (interactive "e")
  (cond ((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))
	 (let (o-list o menu (found nil))
	   (setq o-list (overlays-at (point)))
	   (while (and o-list (not found))
	     (cond ((overlay-get (car o-list) 'vm-url)
		    (setq found t)
		    (vm-mouse-send-url-at-event event)))
	     (setq o-list (cdr o-list)))
	   (and (not found) (vm-menu-popup-context-menu event))))
	;; The XEmacs code is not actually used now, since all
	;; selectable objects are handled by an extent keymap
	;; binding that points to a more specific function.  But
	;; this might come in handy later if I want selectable
	;; objects that don't have an extent attached.
	((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event)))
	 (if (extent-at (point) (current-buffer) 'vm-url)
	     (vm-mouse-send-url-at-event event)
	   (vm-menu-popup-context-menu event)))))

(defun vm-mouse-send-url-at-event (event)
  (interactive "e")
  (cond ((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event)))
	 (vm-mouse-send-url-at-position (event-point event)))
	((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))
	 (vm-mouse-send-url-at-position (posn-point (event-start event))))))

(defun vm-mouse-send-url-at-position (pos &optional browser)
  (cond ((vm-mouse-xemacs-mouse-p)
	 (let ((e (extent-at pos (current-buffer) 'vm-url))
	       url)
	   (if (null e)
	       nil
	     (setq url (buffer-substring (extent-start-position e)
					 (extent-end-position e)))
	     (vm-mouse-send-url url browser))))
	((vm-mouse-fsfemacs-mouse-p)
	 (let (o-list url o)
	   (setq o-list (overlays-at pos))
	   (while (and o-list (null (overlay-get (car o-list) 'vm-url)))
	     (setq o-list (cdr o-list)))
	   (if (null o-list)
	       nil
	     (setq o (car o-list))
	     (setq url (vm-buffer-substring-no-properties
			(overlay-start o)
			(overlay-end o)))
	     (vm-mouse-send-url url browser))))))

(defun vm-mouse-send-url (url &optional browser)
  (let ((browser (or browser vm-url-browser)))
    (cond ((symbolp browser)
	   (funcall browser url))
	  ((stringp browser)
	   (vm-unsaved-message "Sending URL to %s..." browser)
	   (vm-run-background-command browser url)
	   (vm-unsaved-message "Sending URL to %s... done" browser)))))

(defun vm-mouse-send-url-to-netscape (url &optional new-netscape new-window)
  (vm-unsaved-message "Sending URL to Netscape...")
  (if new-netscape
      (vm-run-background-command vm-netscape-program url)
    (or (equal 0 (vm-run-command vm-netscape-program "-remote" 
				 (concat "openURL(" url
					 (if new-window ", new-window" "")
					 ")")))
	(vm-mouse-send-url-to-netscape url t new-window)))
  (vm-unsaved-message "Sending URL to Netscape... done"))

(defun vm-mouse-send-url-to-mosaic (url &optional new-mosaic new-window)
  (vm-unsaved-message "Sending URL to Mosaic...")
  (if (null new-mosaic)
      (let ((pid-file "~/.mosaicpid")
	    (work-buffer " *mosaic work*")
	    pid)
	(cond ((file-exists-p pid-file)
	       (set-buffer (get-buffer-create work-buffer))
	       (erase-buffer)
	       (insert-file-contents pid-file)
	       (setq pid (int-to-string (string-to-int (buffer-string))))
	       (erase-buffer)
	       (insert (if new-window "newwin" "goto") ?\n)
	       (insert url ?\n)
	       (write-region (point-min) (point-max)
			     (concat "/tmp/Mosaic." pid)
			     nil 0)
	       (set-buffer-modified-p nil)
	       (kill-buffer work-buffer)))
	(cond ((or (null pid)
		   (not (equal 0 (vm-run-command "kill" "-USR1" pid))))
	       (setq new-mosaic t)))))
  (if new-mosaic
      (vm-run-background-command vm-mosaic-program url))
  (vm-unsaved-message "Sending URL to Mosaic... done"))


(defun vm-mouse-install-mouse ()
  (cond ((vm-mouse-xemacs-mouse-p)
	 (if (null (lookup-key vm-mode-map 'button2))
	     (define-key vm-mode-map 'button2 'vm-mouse-button-2)))
	((vm-mouse-fsfemacs-mouse-p)
	 (if (null (lookup-key vm-mode-map [mouse-2]))
	     (define-key vm-mode-map [mouse-2] 'vm-mouse-button-2))
	 (if (null (lookup-key vm-mode-map [down-mouse-3]))
	     (progn
	       (define-key vm-mode-map [mouse-3] 'ignore)
	       (define-key vm-mode-map [down-mouse-3] 'vm-mouse-button-3))))))

(defun vm-run-background-command (command &rest arg-list)
  (apply (function call-process) command nil 0 nil arg-list))

(defun vm-run-command (command &rest arg-list)
  (apply (function call-process) command nil nil nil arg-list))

;; stupid yammering compiler
(defvar vm-mouse-read-file-name-prompt)
(defvar vm-mouse-read-file-name-dir)
(defvar vm-mouse-read-file-name-default)
(defvar vm-mouse-read-file-name-must-match)
(defvar vm-mouse-read-file-name-initial)
(defvar vm-mouse-read-file-name-history)
(defvar vm-mouse-read-file-name-return-value)

(defun vm-mouse-read-file-name (prompt &optional dir default
				       must-match initial history)
  "Like read-file-name, except uses a mouse driven interface.
HISTORY argument is ignored."
  (save-excursion
    (or dir (setq dir default-directory))
    (set-buffer (generate-new-buffer " *Files*"))
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t
	  default-directory dir)
    (make-local-variable 'vm-mouse-read-file-name-prompt)
    (make-local-variable 'vm-mouse-read-file-name-dir)
    (make-local-variable 'vm-mouse-read-file-name-default)
    (make-local-variable 'vm-mouse-read-file-name-must-match)
    (make-local-variable 'vm-mouse-read-file-name-initial)
    (make-local-variable 'vm-mouse-read-file-name-history)
    (make-local-variable 'vm-mouse-read-file-name-return-value)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-dir dir)
    (setq vm-mouse-read-file-name-default default)
    (setq vm-mouse-read-file-name-must-match must-match)
    (setq vm-mouse-read-file-name-initial initial)
    (setq vm-mouse-read-file-name-history history)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-return-value nil)
    (save-excursion
      (vm-goto-new-frame 'completion))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-file-name-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-file-name-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-file-name-return-value)
	 (prog1
	     vm-mouse-read-file-name-return-value
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-file-name-event-handler (&optional string)
  (let ((key-doc "Click here for keyboard interface.")
	start list)
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (save-excursion
		       (let ((vm-mutable-frames t))
			 (vm-delete-windows-or-frames-on (current-buffer))))
		     (setq vm-mouse-read-file-name-return-value
			   (vm-keyboard-read-file-name
			    vm-mouse-read-file-name-prompt
			    vm-mouse-read-file-name-dir
			    vm-mouse-read-file-name-default
			    vm-mouse-read-file-name-must-match
			    vm-mouse-read-file-name-initial
			    vm-mouse-read-file-name-history))
		     (vm-mouse-read-file-name-quit-handler t))
		 (quit (vm-mouse-read-file-name-quit-handler))))
	      ((file-directory-p string)
	       (setq default-directory (expand-file-name string)))
	      (t (setq vm-mouse-read-file-name-return-value
		       (expand-file-name string))
		 (vm-mouse-read-file-name-quit-handler t))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-file-name-prompt)
    (vm-set-region-face start (point) 'bold)
    (cond ((and (not string) vm-mouse-read-file-name-default)
	   (setq start (point))
	   (insert vm-mouse-read-file-name-default)
	   (vm-mouse-set-mouse-track-highlight start (point)))
	  ((not string) nil)
	  (t (insert default-directory)))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n ?\n)
    (setq list (directory-files default-directory))
    (vm-show-list list 'vm-mouse-read-file-name-event-handler)
    (setq buffer-read-only t)))

(defun vm-mouse-read-file-name-quit-handler (&optional normal-exit)
  (interactive)
  (let ((vm-mutable-frames t))
    (vm-delete-windows-or-frames-on (current-buffer))
    (if normal-exit
	(throw 'exit nil)
      (throw 'exit t))))

(defvar vm-mouse-read-string-prompt)
(defvar vm-mouse-read-string-completion-list)
(defvar vm-mouse-read-string-multi-word)
(defvar vm-mouse-read-string-return-value)

(defun vm-mouse-read-string (prompt completion-list &optional multi-word)
  (save-excursion
    (set-buffer (generate-new-buffer " *Choices*"))
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t)
    (make-local-variable 'vm-mouse-read-string-prompt)
    (make-local-variable 'vm-mouse-read-string-completion-list)
    (make-local-variable 'vm-mouse-read-string-multi-word)
    (make-local-variable 'vm-mouse-read-string-return-value)
    (setq vm-mouse-read-string-prompt prompt)
    (setq vm-mouse-read-string-completion-list completion-list)
    (setq vm-mouse-read-string-multi-word multi-word)
    (setq vm-mouse-read-string-return-value nil)
    (save-excursion
      (vm-goto-new-frame 'completion))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-string-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-string-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-string-return-value)
	 (prog1
	     (if (listp vm-mouse-read-string-return-value)
		 (mapconcat 'identity vm-mouse-read-string-return-value " ")
	       vm-mouse-read-string-return-value)
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-string-event-handler (&optional string)
  (let ((key-doc  "Click here for keyboard interface.")
	(bs-doc   "      .... to go back one word.")
	(done-doc "      .... to when you're done.")
	start list)
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (save-excursion
		       (let ((vm-mutable-frames t))
			 (vm-delete-windows-or-frames-on (current-buffer))))
		     (setq vm-mouse-read-string-return-value
			   (vm-keyboard-read-string
			    vm-mouse-read-string-prompt
			    vm-mouse-read-string-completion-list
			    vm-mouse-read-string-multi-word))
		     (vm-mouse-read-string-quit-handler t))
		 (quit (vm-mouse-read-string-quit-handler))))
	      ((equal string bs-doc)
	       (setq vm-mouse-read-string-return-value
		     (nreverse
		      (cdr
		       (nreverse vm-mouse-read-string-return-value)))))
	      ((equal string done-doc)
	       (vm-mouse-read-string-quit-handler t))
	      (t (setq vm-mouse-read-string-return-value
		       (nconc vm-mouse-read-string-return-value
			      (list string)))
		 (if (null vm-mouse-read-string-multi-word)
		     (vm-mouse-read-string-quit-handler t)))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-string-prompt)
    (vm-set-region-face start (point) 'bold)
    (insert (mapconcat 'identity vm-mouse-read-string-return-value " "))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n)
    (if vm-mouse-read-string-multi-word
	(progn
	  (setq start (point))
	  (insert bs-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)
	  (setq start (point))
	  (insert done-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)))
    (insert ?\n)
    (vm-show-list vm-mouse-read-string-completion-list
		  'vm-mouse-read-string-event-handler)
    (setq buffer-read-only t)))

(defun vm-mouse-read-string-quit-handler (&optional normal-exit)
  (interactive)
  (let ((vm-mutable-frames t))
    (vm-delete-windows-or-frames-on (current-buffer))
    (if normal-exit
	(throw 'exit nil)
      (throw 'exit t))))