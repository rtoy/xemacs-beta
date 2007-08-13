;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-coke.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  Coke Machine support for efs
;; Author:       Sandy Rutherford <sandy@imb550.sissa.it>
;; Created:      Fri Oct 14 23:55:04 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 12:16:47 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-coke)
(require 'efs)

(defconst efs-coke-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; Coke Machine support
;;;; ------------------------------------------------------------
;;;
;;;  Works for the MIT vending machine FTP server.
;;;  Hopefully, a vending machine RFC is on its way, so we won't
;;;  need to support a wide variation of vending machine protocols.

(efs-defun efs-send-pwd coke (host user &optional xpwd)
  ;; Directories on vending machines?
  "/")

(efs-defun efs-fix-path coke (path &optional reverse)
  (if (= ?/ (aref path 0))
      (if reverse path (substring path 1))
    (if reverse (concat "/" path) path)))

(efs-defun efs-fix-dir-path coke (dir-path)
  ;; Make a beverage path for a dir listing.
  (if (or (string-equal dir-path "/") (string-equal dir-path "/."))
      "*"
    dir-path))

(efs-defun efs-parse-listing coke
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be in coke machine
  ;; ftp dir format.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a remote full path
  ;; PATH = directory as an efs full path
  ;; SWITCHES are never used here, but they
  ;; must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  (let ((tbl (efs-make-hashtable)))
    (goto-char (point-min))
    (efs-save-match-data
      (while (re-search-forward "^\\(SOLD OUT \\)?\\[[0-9]+\\] +\\([^:\n]+\\)"
				nil t)
	(efs-put-hash-entry (buffer-substring (match-beginning 2)
					      (match-end 2))
			    (list nil) tbl)
	(forward-line 1)))
    ;; Don't need to bother with ..
    (efs-put-hash-entry "." '(t) tbl)
    tbl))

(efs-defun efs-allow-child-lookup coke (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.

  ;; Coke machine file system is flat.  Hopefully not the coke.
  (and (string-equal "/" dir) (string-equal "." file)))

(defun efs-coke-insert-beverage-contents (buffer file line)
  ;; Inserts the contents of a beverage (determined by the FTP server
  ;; response LINE) into BUFFER, and then drinks it.
  ;; FILE is the name of the file.
  (efs-save-buffer-excursion
    (set-buffer buffer)
    (if (zerop (buffer-size))
	(progn
	  (insert "\n\n\n\n      " (substring line 4) "\n")
	  (set-buffer-modified-p nil)
	  (set-process-sentinel
	   (start-process "efs-coke-gulp-buffer" (current-buffer) "sleep" "3")
	   (function
	    (lambda (proc str)
	      (efs-save-buffer-excursion
		(let ((buff (process-buffer proc)))
		  (and buff (get-buffer buff)
		       (progn
			 (set-buffer buff)
			 (erase-buffer)
			 (insert "\n\n\n\n          GULP!!!\n")
			 (sit-for 1)
			 (set-buffer-modified-p nil)
			 (kill-buffer (current-buffer)))))))))
	  (if (featurep 'dired)
	      (dired-fun-in-all-buffers
	       (file-name-directory file) 'dired-revert)))
      (message "You haven't finished your last drink in buffer %s!"
	       (current-buffer))
      (ding)
      (sit-for 1))))

;;; Dired support

(efs-defun efs-dired-manual-move-to-filename coke
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the COKE version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (if bol
      (goto-char bol)
    (skip-chars-backward "^\n\r")
    (setq bol (point)))
  (if (looking-at "\\(. \\)?\\(SOLD OUT \\)?\\[[0-9]+\\] +\\([^:\n]+\\): ")
      (goto-char (match-beginning 3))
    (and raise-error (error "No file on this line"))))

(efs-defun efs-dired-manual-move-to-end-of-filename coke
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the COKE version.
  (let ((opoint (point)))
    (and selective-display
	 (null no-error)
	 (eq (char-after
	      (1- (or bol (save-excursion
			    (skip-chars-backward "^\r\n")
			    (point)))))
	     ?\r)
	 ;; File is hidden or omitted.
	 (cond
	  ((dired-subdir-hidden-p (dired-current-directory))
	   (error
	    (substitute-command-keys
	     "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
	  ((error
	    (substitute-command-keys
	     "File line is omitted. Type \\[dired-omit-toggle] to un-omit."
	     )))))
    (if (search-forward ": " eol t)
	(goto-char (- (match-end 0) 2))
      (if no-error
	  nil
	(error "No file on this line"))
      (point))))

(efs-defun efs-dired-insert-headerline coke (dir)
  (let* ((parsed (efs-ftp-path dir))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (accounting
	  (efs-send-cmd
	   host user '(quote pwd)
	   (format "Getting accounting data for %s@%s user host" user host))))
    (insert "  " user "@" host "\n    "
	    (if (car accounting)
		"Account status unavailable"
	      (substring (nth 1 accounting) 4)))
    (delete-region (point) (progn (skip-chars-backward ":.,;") (point)))
    (insert ":\n \n")))

;;; end of efs-coke.el
