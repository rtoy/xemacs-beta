;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dired.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Makes efs-dired.el work with MULE.
;; Author:       Ishikawa Ichiro <ichiro@ccsr.u-tokyo.ac.jp>
;; Created:      Sat Aug 20 05:25:55 1994
;; Modified:     Sun Nov 27 12:19:17 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst efs-dired-mule-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;; Keep the byte-compiler happy
(defvar efs-version-host-types)
(defvar efs-dired-host-type)

(defun efs-dired-find-file (&optional coding-system)
  "Documented as original"
  (interactive "ZCoding-system: ")
  (let ((file (dired-get-filename)))
    (if (memq efs-dired-host-type efs-version-host-types)
	(setq file (efs-internal-file-name-sans-versions
		    efs-dired-host-type file t)))
  (find-file file coding-system)))

(defun efs-dired-find-file-other-window (&optional display coding-system)
  "Documented as original"
  (interactive "P\nZCoding-system: ")
  (if display
      (dired-display-file coding-system)
    (let ((file (dired-get-filename)))
      (if (memq efs-dired-host-type efs-version-host-types)
	  (setq file (efs-internal-file-name-sans-versions
		      efs-dired-host-type file t)))
      (find-file-other-window file coding-system))))


(defun efs-dired-display-file (&optional coding-system)
  "Documented as original"
  (interactive "ZCoding-system: ")
  (let ((file (dired-get-filename)))
    (if (memq efs-dired-host-type efs-version-host-types)
	(setq file (efs-internal-file-name-sans-versions
		    efs-dired-host-type file t)))
    (display-buffer (find-file-noselect file coding-system))))

;;; end of efs-dired-mule.el
