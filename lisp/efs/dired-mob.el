;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:           dired-mob.el
;; RCS:
;; Dired Version:  #Revision: 7.9 $
;; Description:    Commands for marking files from another buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-mob)
(require 'dired)
(autoload 'compilation-buffer-p "compile")
(autoload 'compile-reinitialize-errors "compile")

;; For the byte-compiler
(defvar compilation-error-list)

;;; Utilities

(defun dired-mark-these-files (file-list from)
  ;; Mark the files in FILE-LIST.  Relative filenames are taken to be
  ;; in the current dired directory.
  ;; FROM is a string (used for logging) describing where FILE-LIST
  ;; came from.
  ;; Logs files that were not found and displays a success or failure
  ;; message.
  (message "Marking files %s..." from)
  (let ((total (length file-list))
	(cur-dir (dired-current-directory))
	file failures)
    (while file-list
      (setq file (expand-file-name (car file-list) cur-dir)
	    file-list (cdr file-list))
      ;;(message "Marking file `%s'" file)
      (save-excursion
	(if (dired-goto-file file)
	    (dired-mark 1) ; supplying a prefix keeps it from checking
			   ; for a subdir.
	  (setq failures (cons (dired-make-relative file) failures))
	  (dired-log (buffer-name (current-buffer))
		     "Cannot mark this file (not found): %s\n" file))))
    (dired-update-mode-line-modified t)
    (if failures
	(dired-log-summary
	 (buffer-name (current-buffer))
	 (format "Failed to mark %d of %d files %s %s"
		 (length failures) total from failures) failures)
      (message "Marked %d file%s %s." total (dired-plural-s total) from))))

;;; User commands

(defun dired-mark-files-from-other-dired-buffer (buf)
  "Mark files that are marked in the other Dired buffer.
I.e, mark those files in this Dired buffer that have the same
non-directory part as the marked files in the Dired buffer in the other 
window."
  (interactive (list (window-buffer (next-window))))
  (if (eq (get-buffer buf) (current-buffer))
      (error "Other dired buffer is the same"))
  (or (stringp buf) (setq buf (buffer-name buf)))
  (let ((other-files (save-excursion
		       (set-buffer buf)
		       (or (eq major-mode 'dired-mode)
			   (error "%s is not a dired buffer" buf))
		       (dired-get-marked-files 'no-dir))))
    (dired-mark-these-files other-files (concat "from buffer " buf))))

(defun dired-mark-files-compilation-buffer (&optional buf)
  "Mark the files mentioned in the `*compilation*' buffer.
With a prefix, you may specify the other buffer."
  (interactive
   (list
    (let ((buff  (let ((owin (selected-window))
		      found)
		  (unwind-protect
		      (progn
			(other-window 1)
			(while (null (or found (eq (selected-window) owin)))
			  (if (compilation-buffer-p
			       (window-buffer (selected-window)))
			      (setq found (current-buffer)))
			  (other-window 1)))
		    (select-window owin))
		  found)))
      (if (or current-prefix-arg (null buff))
	  (let ((minibuffer-history
		 (delq nil
		      (mapcar
		       (function
			(lambda (b)
			  (and (compilation-buffer-p b) (buffer-name b))))
		       (buffer-list)))))
	    (read-buffer "Use buffer: "
			 (or buff (car minibuffer-history))))
	buff))))
  (let ((dired-dir (directory-file-name default-directory))
	files)
    (save-window-excursion
      (set-buffer buf)
      (compile-reinitialize-errors nil (point-max))
      (let ((alist compilation-error-list)
	    f d elt)
	(while alist
	  (setq elt (car alist)
		alist (cdr alist))
	  (and (consp (setq elt (car (cdr elt))))
	       (stringp (setq d (car elt)))
	       (stringp (setq f (cdr elt)))
	       (progn
		 (setq d (expand-file-name d))
		 (dired-in-this-tree d dired-dir))
	       (progn
		 (setq f (expand-file-name f d))
		 (not (member f files)))
	       (setq files (cons f files))))))
    (dired-mark-these-files
     files
     (concat "From compilation buffer "
	     (if (stringp buf) buf (buffer-name buf))))))

;;; end of dired-mob.el
