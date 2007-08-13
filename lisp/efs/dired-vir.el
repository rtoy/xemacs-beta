;;  -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-vir.el
;; Dired Version: #Revision: 7.9 $
;; RCS:
;; Description:   Virtual dired mode for browsing ls -lR listings.
;; Author:        Sebastian Kremer <sk@thp.uni-koeln.de>
;; Created:       7-Mar-1991 16:00
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-vir)
(require 'dired)

(defun dired-virtual (dirname &optional switches)
  "Put this buffer into Virtual Dired mode.

In Virtual Dired mode, all commands that do not actually consult the
filesystem will work.

This is useful if you want to peruse and move around in an ls -lR
output file, for example one you got from an ftp server.  With
efs, you can even dired a directory containing an ls-lR file,
visit that file and turn on virtual dired mode.  But don't try to save
this file, as dired-virtual indents the listing and thus changes the
buffer.

If you have save a Dired buffer in a file you can use \\[dired-virtual] to
resume it in a later session.

Type \\<dired-mode-map>\\[revert-buffer] in the
Virtual Dired buffer and answer `y' to convert the virtual to a real
dired buffer again.  You don't have to do this, though: you can relist
single subdirs using \\[dired-do-redisplay].
"

  ;; DIRNAME is the top level directory of the buffer.  It will become
  ;; its `default-directory'.  If nil, the old value of
  ;; default-directory is used.

  ;; Optional SWITCHES are the ls switches to use.

  ;; Shell wildcards will be used if there already is a `wildcard'
  ;; line in the buffer (thus it is a saved Dired buffer), but there
  ;; is no other way to get wildcards.  Insert a `wildcard' line by
  ;; hand if you want them.

  (interactive
   (list (read-string "Virtual Dired directory: " (dired-virtual-guess-dir))))
  (goto-char (point-min))
  (or (looking-at "  ")
      ;; if not already indented, do it now:
      (indent-region (point-min) (point-max) 2))
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (file-name-as-directory dirname)))
  (setq default-directory dirname)	; contains no wildcards
  (let ((wildcard (save-excursion
		    (goto-char (point-min))
		    (forward-line 1)
		    (and (looking-at "^  wildcard ")
			 (buffer-substring (match-end 0)
					   (progn (end-of-line) (point)))))))
  (if wildcard
	(setq dirname (expand-file-name wildcard default-directory))))
  ;; If raw ls listing (not a saved old dired buffer), give it a
  ;; decent subdir headerline:
  (goto-char (point-min))
  (or (looking-at dired-subdir-regexp)
      (dired-insert-headerline default-directory))
  (dired-mode dirname (or switches dired-listing-switches))
  (setq mode-name "Virtual Dired"
	revert-buffer-function 'dired-virtual-revert)
  (set (make-local-variable 'dired-subdir-alist) nil)
  (dired-build-subdir-alist)
  (goto-char (point-min))
  (dired-initial-position dirname))

(defun dired-virtual-guess-dir ()

  ;; Guess and return appropriate working directory of this buffer,
  ;; assumed to be in Dired or ls -lR format.
  ;; The guess is based upon buffer contents.
  ;; If nothing could be guessed, returns nil.

  (let ((regexp "^\\(  \\)?\\([^ \n\r]*\\)\\(:\\)[\n\r]")
	(subexpr 2))
    (goto-char (point-min))
    (cond ((looking-at regexp)
	   ;; If a saved dired buffer, look to which dir and
	   ;; perhaps wildcard it belongs:
	   (let ((dir (buffer-substring (match-beginning subexpr)
					(match-end subexpr))))
	     (file-name-as-directory dir)))
	  ;; Else no match for headerline found.  It's a raw ls listing.
	  ;; In raw ls listings the directory does not have a headerline
	  ;; try parent of first subdir, if any
	  ((re-search-forward regexp nil t)
	   (file-name-directory
	    (directory-file-name
	     (file-name-as-directory
	      (buffer-substring (match-beginning subexpr)
				(match-end subexpr))))))
	  (t				; if all else fails
	   nil))))


(defun dired-virtual-revert (&optional arg noconfirm)
  (if (not
       (y-or-n-p "Cannot revert a Virtual Dired buffer - switch to Real Dired mode? "))
      (error "Cannot revert a Virtual Dired buffer.")
    (setq mode-name "Dired"
	  revert-buffer-function 'dired-revert)
    (revert-buffer)))

;; A zero-arg version of dired-virtual.
;; You need my modified version of set-auto-mode for the
;; `buffer-contents-mode-alist'.
;; Or you use infer-mode.el and infer-mode-alist, same syntax.
(defun dired-virtual-mode ()
  "Put current buffer into virtual dired mode (see `dired-virtual').
Useful on `buffer-contents-mode-alist' (which see) with the regexp

    \"^  \\(/[^ /]+\\)/?+:$\"

to put saved dired buffers automatically into virtual dired mode.

Also useful for `auto-mode-alist' (which see) like this:

  \(setq auto-mode-alist (cons '(\"[^/]\\.dired$\" . dired-virtual-mode)
			      auto-mode-alist)\)
"
  (interactive)
  (dired-virtual (dired-virtual-guess-dir)))

;;; end of dired-vir.el
