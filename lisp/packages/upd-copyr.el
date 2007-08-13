;;; upd-copyr.el --- update the copyright notice in a GNU Emacs Lisp file

;;; Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
;;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; hacked on by Jamie Zawinski.
;; hacked upon by Jonathan Stigelman <Stig@hackvan.com>
;; Keywords: maint

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
;;; 02139, USA.

;;; Synched up with: Not synched with FSF.
;;; Apparently mly synched this file with the version of upd-copyr.el
;;; supplied with FSF 19.22 or 19.23.  Since then, FSF renamed the
;;; file to copyright.el and basically rewrote it, and Stig and Jamie
;;; basically rewrote it, so there's not much in common any more.

;;; Code:

;; #### - this will break if you dump it into emacs
(defconst copyright-year (substring (current-time-string) -4)
  "String representing the current year.")

;;;###autoload
(defvar copyright-do-not-disturb "Free Software Foundation, Inc."
  "*If non-nil, the existing copyright holder is checked against this regexp.
If it does not match, then a new copyright line is added with the copyright
holder set to the value of `copyright-whoami'.") 

;;;###autoload
(defvar copyright-whoami nil
  "*A string containing the name of the owner of new copyright notices.")

;;;###autoload
(defvar copyright-notice-file nil
  "*If non-nil, replace copying notices with this file.")

(defvar copyright-files-to-ignore-regex "loaddefs.el$"
  "*Regular expression for files that should be ignored")

(defvar current-gpl-version "2"
  "String representing the current version of the GPL.")

(defvar copyright-inhibit-update nil
  "If nil, ask the user whether or not to update the copyright notice.
If the user has said no, we set this to t locally.")

(defvar copyright-search-limit 2048
  "Portion of file to search for copyright notices")

;;;###autoload
(defun update-copyright (&optional replace ask-upd ask-year)
  "Update the copyright notice at the beginning of the buffer
to indicate the current year.  If optional arg REPLACE is given
\(interactively, with prefix arg\) replace the years in the notice
rather than adding the current year after them.
If `copyright-notice-file' is set, the copying permissions following the
copyright are replaced as well.

If optional third argument ASK is non-nil, the user is prompted for whether
or not to update the copyright.  If optional fourth argument ASK-YEAR is
non-nil, the user is prompted for whether or not to replace the year rather
than adding to it."
  (interactive "*P")
  (or (and ask-upd copyright-inhibit-update)
      (and buffer-file-truename
	   (string-match copyright-files-to-ignore-regex buffer-file-truename))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (narrow-to-region (point-min)
			    (min copyright-search-limit (point-max)))
	  ;; Handle abbreviated year lists like "1800, 01, 02, 03"
	  ;; or "1900, '01, '02, '03".
	  (let ((case-fold-search t)
		p-string holder add-new
		mine current
		cw-current cw-mine last-cw
		(cw-position '(lambda ()
				(goto-char (point-min))
				(cond (cw-mine (goto-char cw-mine))
				      ((or (and last-cw (goto-char last-cw))
					   (re-search-forward
					    "copyright[^0-9\n]*\\([-, \t]*\\([0-9]+\\)\\)+"
					    nil t))
				       (and add-new (beginning-of-line 2)))
				      (t (goto-char (point-min)))))))
	    ;; scan for all copyrights
	    (while (re-search-forward
		    (concat "^\\(.*\\)copyright.*\\(" (substring copyright-year 0 2)
			    "\\)?" "\\([0-9][0-9]\\(, \t\\)+\\)*'?"
			    "\\(\\(" (substring copyright-year 2) "\\)\\|[0-9][0-9]\\)\\s *\\(\\S .*\\)$")
		    nil t)
	      (buffer-substring (match-beginning 0) (match-end 0))
	      (setq p-string (buffer-substring (match-beginning 1)
					       (match-end 1))
		    last-cw   (match-end 5)
		    holder    (buffer-substring (match-beginning 7)
						(match-end 7))
		    current    (match-beginning 6)
		    mine	   (string-match copyright-do-not-disturb holder)
		    cw-current (if mine
				   current
				 (or cw-current current))
		    cw-mine (or cw-mine (and mine last-cw))
		    ))
	    ;; ok, now decide if a new copyright is needed...
	    (setq add-new (not cw-mine))
	    (or ask-upd add-new
		(message "Copyright notice already includes %s." copyright-year))
	    (goto-char (point-min))
	    (cond ((and cw-current cw-mine)
		   (or ask-upd (message "The copyright is up to date"))
		   (copyright-check-notice))
		  ((and (or add-new (not cw-current))
			;; #### - doesn't bother to ask about non-GPL sources
			(or (not ask-upd)
			    (prog1
				(search-forward "is free software" nil t)
			      (goto-char (point-min))))
			;; adding a new copyright or one exists already...
			(or add-new last-cw)
			;; adding a new copyright or the user wants to update...
			(or (not ask-upd)
			    (save-window-excursion
			      (pop-to-buffer (current-buffer))
			      ;; Show user the copyright.
			      (funcall cw-position)
			      (sit-for 0)
			      (or (y-or-n-p "Update copyright? ")
				  (progn
				    (set (make-local-variable
					  'copyright-inhibit-update) t)
				    nil)))))
		   ;; The "XEmacs change" below effectively disabled this
		   ;; already, so I'm gonna comment it out entirely...  --Stig
		   ;; (setq replace
		   ;;       (or replace
		   ;;           (and ask-year
		   ;;                (save-window-excursion
		   ;;                  (pop-to-buffer (current-buffer))
		   ;;                  (save-excursion
		   ;;                    ;; Show the user the copyright.
		   ;;                    (goto-char (point-min))
		   ;;                    ;;XEmacs change
		   ;;                    ;; (sit-for 0)
		   ;;                    ;; (y-or-n-p "Replace copyright year? ")
		   ;;                    nil
		   ;;                    )))))
		   (cond (add-new
			  ;; the cursor should already be at the beginning of a
			  ;; line here...
			  (funcall cw-position)
			  (setq holder (or copyright-whoami
					   (read-string "New copyright holder: ")))
			  (if p-string (insert p-string) (indent-for-comment))
			  (insert "Copyright (C) ")
			  (save-excursion
			    (insert " " holder "\n"))
			  )
			 (replace
			  ;; #### - check this...
			  (beginning-of-line)
			  (re-search-forward "copyright\\([^0-9]*\\([-, \t]*\\([0-9]+\\)\\)+\\)"
					     (save-excursion (end-of-line)
							     (point)))
			  (delete-region (match-beginning 1) (match-end 1)))
			 (t (insert ", ")
			    ;; This did the wrong thing:  "1990-1992" -> "1990, 1992"
			    ;; Perhaps "1990, 1991, 1992" would be an appropriate 
			    ;; substitution, but "1990-1992" is satisfactory.  --Stig
			    ;;
			    ;; XEmacs addition
			    ;; (save-excursion
			    ;;   (goto-char (match-beginning 1))
			    ;;   (if (looking-at "[0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")
			    ;;       (progn (forward-char 4)
			    ;; 	     (delete-char 1)
			    ;; 	     (insert ", "))))
			    ))
		   (insert copyright-year)
		   ;; XEmacs addition
		   ;; #### - this assumes lisp and shouldn't
		   (if (save-excursion
			 (end-of-line)
			 (>= (current-column) fill-column))
		       (if (= (char-syntax ?\;) ?<)
			   (insert "\n;;;")
			 (insert "\n  ")))
		   (message "Copyright updated to %s%s."
			    (if replace "" "include ") copyright-year)
		   (copyright-check-notice)
		   ;; show the newly-munged copyright.
		   (message "The copyright has been updated")
		   (sit-for 1))
		  ((not ask-upd)
		   (error "This buffer does not contain a copyright notice!"))
		  ))))))

(defun copyright-check-notice ()
  (if copyright-notice-file
      (let (beg)
	(goto-char (point-min))
	;; Find the beginning of the copyright.
	(if (search-forward "copyright" nil t)
	    (progn
	      ;; Look for a blank line or a line with only comment chars.
	      (if (re-search-forward "^\\(\\s \\s<\\|\\s>\\)*$" nil t)
		  (forward-line 1)
		(with-output-to-temp-buffer "*Help*"
		  (princ (substitute-command-keys "\
I don't know where the copying notice begins.
Put point there and hit \\[exit-recursive-edit]."))
		  (recursive-edit)))
	      (setq beg (point))
	      (or (search-forward "02139, USA." nil t)
		  (with-output-to-temp-buffer "*Help*"
		    (princ (substitute-command-keys "\
I don't know where the copying notice ends.
Put point there and hit \\[exit-recursive-edit]."))
		    (recursive-edit)))
	      (delete-region beg (point))))
	(insert-file copyright-notice-file))
    (if (re-search-forward
	 "; either version \\(.+\\), or (at your option)"
	 nil t)
	(progn
	  (goto-char (match-beginning 1))
	  (delete-region (point) (match-end 1))
	  (insert current-gpl-version)))))

;;;###autoload
(defun ask-to-update-copyright ()
  "If the current buffer contains a copyright notice that is out of date,
ask the user if it should be updated with `update-copyright' (which see).
Put this on write-file-hooks."
  (update-copyright nil t t)
  ;; Be sure return nil; if a write-file-hook return non-nil,
  ;; the file is presumed to be already written.
  nil)

(provide 'upd-copyr)

;;; upd-copyr.el ends here
