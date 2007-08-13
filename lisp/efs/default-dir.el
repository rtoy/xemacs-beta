;;  -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         default-dir.el
;; RCS:
;; Version:      $Revision: 1.4 $
;; Description:  Defines the function default-directory, for fancy handling
;;               of the initial contents in the minibuffer when reading
;;               file names.
;; Authors:      Sebastian Kremer <sk@thp.uni-koeln.de>
;;               Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Sun Jul 18 11:38:06 1993 by sandy on ibm550
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'default-dir)
(require 'efs-ovwrt)

(defconst default-dir-emacs-variant
  (cond ((string-match "XEmacs" emacs-version) 'xemacs)
	((>= (string-to-int (substring emacs-version 0 2)) 19) 'fsf-19)
	(t 'fsf-18)))

;;;###autoload
(defvar default-directory-function nil
  "A function to call to compute the default-directory for the current buffer.
If this is nil, the function default-directory will return the value of the
variable default-directory.
Buffer local.")
(make-variable-buffer-local 'default-directory-function)

;; As a bonus we give shell-command history if possible.
(defvar shell-command-history nil
  "History list of previous shell commands.")

(defun default-directory ()
  " Returns the default-directory for the current buffer.
Will use the variable default-directory-function if it non-nil."
  (if default-directory-function
      (funcall default-directory-function)
    (if (eq default-dir-emacs-variant 'xemacs)
	(abbreviate-file-name default-directory t)
      (abbreviate-file-name default-directory))))

;;; Overloads

(cond
 ((or (featurep 'mule)
      (boundp 'MULE))
      
  (defun default-dir-find-file (file &optional coding-system)
    "Documented as original"
    (interactive   
     (list
      (expand-file-name
       (read-file-name "Find file: " (default-directory)))
      (and current-prefix-arg
	   (read-coding-system "Coding-system: "))))
    (default-dir-real-find-file file coding-system))

  (defun default-dir-find-file-other-window (file &optional coding-system)
    "Documented as original"
    (interactive
     (list
      (expand-file-name
       (read-file-name "Find file in other window: " (default-directory)))
      (and current-prefix-arg
	   (read-coding-system "Coding-system: "))))
    (default-dir-real-find-file-other-window file coding-system))

  (defun default-dir-find-file-read-only (file &optional coding-system)
    "Documented as original"
    (interactive
     (list
      (expand-file-name
       (read-file-name "Find file read-only: " (default-directory) nil t))
      (and current-prefix-arg
	   (read-coding-system "Coding-system: "))))
    (default-dir-real-find-file-read-only file coding-system))

  (if (fboundp 'find-file-read-only-other-window)
      (progn
	(defun default-dir-find-file-read-only-other-window
	  (file &optional coding-system)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name
	      "Find file read-only in other window: "
	      (default-directory) nil t))
	    (and current-prefix-arg
		 (read-coding-system "Coding-system: "))))
	  (default-dir-real-find-file-read-only-other-window file
	    coding-system))))

  (if (fboundp 'find-file-other-frame)
      (progn
	(defun default-dir-find-file-other-frame
	  (file &optional coding-system)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name "Find file in other frame: "
			     (default-directory)))
	    (and current-prefix-arg
		 (read-coding-system "Coding-system: "))))
	  (default-dir-real-find-file-other-frame file
	    coding-system))))

  (if (fboundp 'find-file-read-only-other-frame)
      (progn
	(defun default-dir-find-file-read-only-other-frame
	  (file &optional coding-system)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name "Find file read-only in other frame: "
			     (default-directory) nil t))
	    (and current-prefix-arg
		 (read-coding-system "Coding-system: "))))
	  (default-dir-real-find-file-read-only-other-frame file
	    coding-system)))))

 (default-dir-find-file-takes-coding-system
   ;; This lossage is due to the fact that XEmacs 20.x without mule
   ;; still accepts an optional argument for find-file related
   ;; functions.  Things like advice.el insist on passing nil for
   ;; optional arguments, and the interaction screws things up.
   ;; Therefore these functions accept an optional dummy coding-system
   ;; argument.

   (defun default-dir-find-file (file &optional coding-system)
     "Documented as original"
     (interactive
      (list
       (expand-file-name
	(read-file-name "Find file: " (default-directory)))))
     (default-dir-real-find-file file coding-system))
  
   (defun default-dir-find-file-other-window (file &optional coding-system)
     "Documented as original"
     (interactive
      (list
       (expand-file-name
	(read-file-name "Find file in other window: " (default-directory)))))
     (default-dir-real-find-file-other-window file coding-system))

   (defun default-dir-find-file-read-only (file &optional coding-system)
     "Documented as original"
     (interactive
      (list
       (expand-file-name
	(read-file-name "Find file read-only: " (default-directory) nil t))))
     (default-dir-real-find-file-read-only file coding-system))
  
   (if (fboundp 'find-file-read-only-other-window)
       (progn
	 (defun default-dir-find-file-read-only-other-window
	   (file  &optional coding-system)
	   "Documented as original"
	   (interactive
	    (list
	     (expand-file-name
	      (read-file-name
	       "Find file read-only in other window: "
	       (default-directory) nil t))))
	   (default-dir-real-find-file-read-only-other-window file))))

   (if (fboundp 'find-file-other-frame)
       (progn
	 (defun default-dir-find-file-other-frame
	   (file  &optional coding-system)
	   "Documented as original"
	   (interactive
	    (list
	     (expand-file-name
	      (read-file-name "Find file in other frame: "
			      (default-directory)))))
	   (default-dir-real-find-file-other-frame file))))

   (if (fboundp 'find-file-read-only-other-frame)
       (progn
	 (defun default-dir-find-file-read-only-other-frame
	   (file &optional coding-system)
	   "Documented as original"
	   (interactive
	    (list
	     (expand-file-name
	      (read-file-name "Find file read-only in other frame: "
			      (default-directory) nil t))))
	   (default-dir-real-find-file-read-only-other-frame file)))))

 (t
  (defun default-dir-find-file (file)
    "Documented as original"
    (interactive
     (list
      (expand-file-name
       (read-file-name "Find file: " (default-directory)))))
    (default-dir-real-find-file file))
  
  (defun default-dir-find-file-other-window (file)
    "Documented as original"
    (interactive
     (list
      (expand-file-name
       (read-file-name "Find file in other window: " (default-directory)))))
    (default-dir-real-find-file-other-window file))

  (defun default-dir-find-file-read-only (file)
    "Documented as original"
    (interactive
     (list
      (expand-file-name
       (read-file-name "Find file read-only: " (default-directory) nil t))))
    (default-dir-real-find-file-read-only file))
  
  (if (fboundp 'find-file-read-only-other-window)
      (progn
	(defun default-dir-find-file-read-only-other-window (file)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name
	      "Find file read-only in other window: "
	      (default-directory) nil t))))
	  (default-dir-real-find-file-read-only-other-window file))))

  (if (fboundp 'find-file-other-frame)
      (progn
	(defun default-dir-find-file-other-frame (file)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name "Find file in other frame: "
			     (default-directory)))))
	  (default-dir-real-find-file-other-frame file))))

  (if (fboundp 'find-file-read-only-other-frame)
      (progn
	(defun default-dir-find-file-read-only-other-frame (file)
	  "Documented as original"
	  (interactive
	   (list
	    (expand-file-name
	     (read-file-name "Find file read-only in other frame: "
			     (default-directory) nil t))))
	  (default-dir-real-find-file-read-only-other-frame file))))))



(efs-overwrite-fn "default-dir" 'find-file 'default-dir-find-file)
(efs-overwrite-fn "default-dir" 'find-file-other-window
		  'default-dir-find-file-other-window)
(if (fboundp 'find-file-other-frame)
    (efs-overwrite-fn "default-dir" 'find-file-other-frame
		      'default-dir-find-file-other-frame))
(efs-overwrite-fn "default-dir" 'find-file-read-only
		  'default-dir-find-file-read-only)
(if (fboundp 'find-file-read-only-other-window)
    (efs-overwrite-fn "default-dir" 'find-file-read-only-other-window
		      'default-dir-find-file-read-only-other-window))
(if (fboundp 'find-file-read-only-other-frame)
    (efs-overwrite-fn "default-dir" 'find-file-read-only-other-frame
		      'default-dir-find-file-read-only-other-frame))


(defun default-dir-load-file (file)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "Load file: " (default-directory) nil t))))
  (default-dir-real-load-file file))

(efs-overwrite-fn "default-dir" 'load-file 'default-dir-load-file)

(condition-case nil
    (require 'view-less)
  (error (require 'view)))

(defun default-dir-view-file (file)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "View file: " (default-directory) nil t))))
  (default-dir-real-view-file file))

(efs-overwrite-fn "default-dir" 'view-file 'default-dir-view-file)

(if (fboundp 'view-file-other-window)
    (progn
      (defun default-dir-view-file-other-window (file)
	"Documented as original"
	(interactive
	 (list
	  (expand-file-name
	   (read-file-name "View file in other window: "
			   (default-directory) nil t))))
	(default-dir-real-view-file-other-window file))
      (efs-overwrite-fn "default-dir" 'view-file-other-window
			'default-dir-view-file-other-window)))

(if (fboundp 'view-file-other-frame)
    (progn
      (defun default-dir-view-file-other-frame (file)
	"Documented as original"
	(interactive
	 (list
	  (expand-file-name
	   (read-file-name "View file in other frame: "
			   (default-directory) nil t))))
	(default-dir-real-view-file-other-frame file))
      (efs-overwrite-fn "default-dir" 'view-file-other-frame
			'default-dir-view-file-other-frame)))


(defun default-dir-shell-command (command &optional insert)
  "Documented as original"
  (interactive
   (list
    (let ((prompt (format "Shell command in %s: " (default-directory))))
      (cond
       ((memq  default-dir-emacs-variant '(fsf-19 xemacs))
	(read-from-minibuffer prompt nil nil nil
			      'shell-command-history))
       ((featurep 'gmhist)
	(let ((minibuffer-history-symbol 'shell-command-history))
	  (read-string prompt)))
       (t (read-string prompt))))
    current-prefix-arg))
  (let ((default-directory (expand-file-name (default-directory))))
    (default-dir-real-shell-command command insert)))

(efs-overwrite-fn "default-dir" 'shell-command 'default-dir-shell-command)

(defun default-dir-cd (dir)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "Change default directory: " (default-directory)))))
  (default-dir-real-cd dir))

(efs-overwrite-fn "default-dir" 'cd 'default-dir-cd)

(defun default-dir-set-visited-file-name (filename)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "Set visited file name: " (default-directory)))))
  (default-dir-real-set-visited-file-name filename))

(efs-overwrite-fn "default-dir" 'set-visited-file-name
		  'default-dir-set-visited-file-name)

(defun default-dir-insert-file (filename &rest args)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "Insert file: " (default-directory) nil t))))
  (apply 'default-dir-real-insert-file filename args))

(efs-overwrite-fn "default-dir" 'insert-file 'default-dir-insert-file)

(defun default-dir-append-to-file (start end filename &rest args)
  "Documented as original"
  (interactive
   (progn
     (or (mark) (error "The mark is not set now"))
     (list
      (min (mark) (point))
      (max (mark) (point))
      (expand-file-name
       (read-file-name "Append to file: " (default-directory))))))
  (apply 'default-dir-real-append-to-file start end filename args))

(efs-overwrite-fn "default-dir" 'append-to-file 'default-dir-append-to-file)

(defun default-dir-delete-file (file)
  "Documented as original"
  (interactive
   (list
    (expand-file-name
     (read-file-name "Delete file: " (default-directory) nil t))))
  (default-dir-real-delete-file file))

(efs-overwrite-fn "default-dir" 'delete-file 'default-dir-delete-file)

;;; end of default-dir.el
