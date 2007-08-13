;;;; dired-rcs.el - RCS support for Tree Dired

(defconst dired-rcs-version (substring "!Revision: 1.6 !" 11 -2)
  "I don't speak RCS-ese")
  
;; Originally written by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Rewritten by Heiko Muenkel <muenkel@tnt.uni-hannover.de>
  
;; Copyright (C) 1991 by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Copyright (C) 1994 by Heiko Muenkel <muenkel@tnt.uni-hannover.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; INSTALLATION ======================================================
;; 
;; This will not work with classic (18.xx) Dired, you'll need Tree Dired,
;; available via anonymous ftp from
;; 
;;     ftp.thp.Uni-Koeln.DE[134.95.64.1]:/pub/gnu/emacs/diredall.tar.Z
;;
;; Put this file into your load-path and the following in your ~/.emacs:
;; 
;;   (autoload 'dired-rcs-mark-rcs-locked-files "dired-rcs")
;;   (autoload 'dired-rcs-mark-rcs-files "dired-rcs")
;;
;; Put this inside your dired-load-hook:
;; 
;;   (define-key dired-mode-map "," 'dired-rcs-mark-rcs-files)
;;   (define-key dired-mode-map "\M-," 'dired-rcs-mark-rcs-locked-files)
;;

(require 'dired)

;;;###autoload
(defun dired-rcs-mark-rcs-locked-files (&optional unflag-p)
  "Mark all files that are under RCS control and RCS-locked.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  (interactive "P")
  (dired-rcs-mark-rcs-files unflag-p t))

;;;###autoload
(defun dired-rcs-mark-rcs-files (&optional unflag-p locked)
  "Mark all files that are under RCS control.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  ;; Returns list of failures, or nil on success.
  ;; Optional arg LOCKED means just mark RCS-locked files.
  (interactive "P")
  (message "%s %sRCS controlled files..."
	   (if unflag-p "Unmarking" "Marking")
	   (if locked "locked " ""))
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char))
	rcs-files wf failures count total)
    ;; Loop over subdirs to set `rcs-files'
    (mapcar
     (function
      (lambda (dir)
	(or (equal (file-name-nondirectory (directory-file-name dir))
		   "RCS")
	    ;; skip inserted RCS subdirs
	    (setq rcs-files
		  (append (if locked
			      ;; these two functions from sk's rcs.el
			      (rcs-locked-files dir)
			    (rcs-files dir))
			  rcs-files)))))
     (mapcar (function car) dired-subdir-alist))
    (setq total (length rcs-files))
    (while rcs-files
      (setq wf (rcs-working-file (car rcs-files))
	    rcs-files (cdr rcs-files))
      (save-excursion (if (dired-goto-file wf)
			  (dired-mark-file 1)
			(dired-log "RCS working file not found: %s\n" wf)
			(setq failures (cons (dired-make-relative wf)
					     failures)))))
    (if (null failures)
	(message "%d %sRCS file%s %smarked."
		 total
		 (if locked "locked " "")
		 (dired-plural-s total)
		 (if unflag-p "un" ""))
      (setq count (length failures))
      (dired-log-summary "RCS working file not found %s" failures)
      (message "%d %sRCS file%s: %d %smarked - %d not found %s."
	       total
	       (if locked "locked " "")
	       (dired-plural-s total) (- total count)
	       (if unflag-p "un" "") count failures))
    failures))

(defun rcs-files (directory)
  "Return list of RCS data files for all RCS controlled files in DIRECTORY."
  (setq directory (file-name-as-directory directory))
  (let ((rcs-dir (file-name-as-directory (expand-file-name "RCS" directory)))
	(rcs-files (directory-files directory t ",v$")))
    (if (file-directory-p rcs-dir)
	(setq rcs-files
	      (append (directory-files rcs-dir t ",v$")
		      rcs-files)))
    rcs-files))

(defvar rcs-output-buffer "*RCS-output*"
  "If non-nil, buffer name used by function `rcs-get-output-buffer' (q.v.).
If nil, a new buffer is used each time.")

(defun rcs-get-output-buffer (file)
  ;; Get a buffer for RCS output for FILE, make it writable and clean
  ;; it up.  Return the buffer.
  ;; The buffer used is named according to variable
  ;; `rcs-output-buffer'.  If the caller wants to be reentrant, it
  ;; should let-bind this to nil: a new buffer will be chosen. 
  (let* ((default-major-mode 'fundamental-mode);; no frills!
	 (buf (get-buffer-create (or rcs-output-buffer "*RCS-output*"))))
    (if rcs-output-buffer
	nil
      (setq buf (generate-new-buffer "*RCS-output*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil
	    default-directory (file-name-directory (expand-file-name file)))
      (erase-buffer))
    buf))

(defun rcs-locked-files (directory)
  "Return list of RCS data file names of all RCS-locked files in DIRECTORY."
  (let ((output-buffer (rcs-get-output-buffer directory))
	(rcs-files (rcs-files directory))
	result)
    (and rcs-files
	 (save-excursion
	   (set-buffer output-buffer)
	   (apply (function call-process) "rlog" nil t nil "-L" "-R" rcs-files)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (setq result (cons (buffer-substring (point)
						  (progn (forward-line 1)
							 (1- (point))))
				result)))
	   result))))

(defun rcs-working-file (filename)
  "Convert an RCS file name to a working file name.
That is, convert `...foo,v' and `...RCS/foo,v' to `...foo'.
If FILENAME doesn't end in `,v' it is returned unchanged."
  (if (not (string-match ",v$" filename))
      filename
    (setq filename (substring filename 0 -2))
    (let ((dir (file-name-directory filename)))
      (if (null dir)
	  filename
	(let ((dir-file (directory-file-name dir)))
	  (if (equal "RCS" (file-name-nondirectory dir-file))
	      ;; Working file for ./RCS/foo,v is ./foo.
	      ;; Don't use expand-file-name as this converts "" -> pwd
	      ;; and thus forces a relative FILENAME to be relative to
	      ;; the current value of default-directory, which may not
	      ;; what the caller wants.  Besides, we want to change
	      ;; FILENAME only as much as necessary.
	      (concat (file-name-directory dir-file)
		      (file-name-nondirectory filename))
	    filename))))))

(defun dired-do-vc-register (&optional arg)
  "Register the marked (or next ARG) files under version control."
  (interactive "P")
  (dired-mark-map-check (function dired-vc-register) arg 'register t))

(defun dired-vc-register ()
  (let ((file (dired-get-filename)) failure)
    (condition-case err
	(save-window-excursion
	  (find-file file)
	  (vc-register))
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log "Register error for %s:\n%s\n" file failure)
      (dired-make-relative file))))
    
(provide 'dired-rcs)
