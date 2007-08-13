;;;
;;; $Id: tm-tar.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $
;;;
;;; tm-tar.el
;;;
;;; Internal viewer for
;;;    - application/x-tar
;;;    - application/x-gzip, type="tar"
;;;    - aplication/octet-stream, type="tar"
;;;    - aplication/octet-stream, type="tar+gzip"
;;;
;;; by Hiroshi Ueno <zodiac@ibm.net>
;;;	modified by Tomohiko Morioka <morioka@jaist.ac.jp>
;;;

;;; @ required modules
;;;

(require 'emu)
(require 'tm-view)

;;; @ constants
;;;

(defconst tm-tar/list-buffer "*tm-tar/List*")
(defconst tm-tar/view-buffer "*tm-tar/View*")
(defconst tm-tar/file-search-regexp "[0-9]+\:[0-9\:]+[ ]+[0-9]+[ ]+")
(defconst tm-tar/popup-menu-title "Action Menu")

;;; @ variables
;;;

(defvar tm-tar/tar-program  "gtar")
(defvar tm-tar/tar-decompress-arg '("-z"))
(defvar tm-tar/gzip-program "gzip")
(defvar tm-tar/mmencode-program "mmencode")
(defvar tm-tar/uudecode-program "uudecode")

(defvar tm-tar/popup-menu-items
  '(("View File"         . tm-tar/view-file)
    ("Key Help"          . tm-tar/helpful-message)
    ("Quit tm-tar Mode"  . exit-recursive-edit)
    ))

(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       (defvar tm-tar/popup-menu
	 (cons tm-tar/popup-menu-title
		(mapcar (function
			(lambda (item)
			  (vector (car item)(cdr item) t)
			  ))
			 tm-tar/popup-menu-items)))

       (defun tm-tar/mouse-button-2 (event)
	   (popup-menu tm-tar/popup-menu)
	   )
       )
      ((>= emacs-major-version 19)
       (defun tm-tar/mouse-button-2 (event)
	 (let ((menu
		(cons tm-tar/popup-menu-title
			(list (cons "Menu Items" tm-tar/popup-menu-items))
			)))
	   (let ((func (x-popup-menu event menu)))
		 (if func
		     (funcall func)
		   ))
	     ))
       ))

(defvar tm-tar/tar-mode-map nil)
(if tm-tar/tar-mode-map
      nil
    (setq tm-tar/tar-mode-map (make-keymap))
    (suppress-keymap tm-tar/tar-mode-map)
    (define-key tm-tar/tar-mode-map "\C-c"    'exit-recursive-edit)
    (define-key tm-tar/tar-mode-map "q"       'exit-recursive-edit)
    (define-key tm-tar/tar-mode-map "n"       'tm-tar/next-line)
    (define-key tm-tar/tar-mode-map " "       'tm-tar/next-line)
    (define-key tm-tar/tar-mode-map "\C-m"    'tm-tar/next-line)
    (define-key tm-tar/tar-mode-map "p"       'tm-tar/previous-line)
    (define-key tm-tar/tar-mode-map "\177"    'tm-tar/previous-line)
    (define-key tm-tar/tar-mode-map "\C-\M-m" 'tm-tar/previous-line)
    (define-key tm-tar/tar-mode-map "v"       'tm-tar/view-file)
    (define-key tm-tar/tar-mode-map "\C-h"    'Helper-help)
    (define-key tm-tar/tar-mode-map "?"       'tm-tar/helpful-message)
    (if mouse-button-2
	(define-key tm-tar/tar-mode-map
				  mouse-button-2 'tm:button-dispatcher)
	)
  )

;;; @@ tm-tar mode functions
;;;

(defun tm-tar/tar-mode (&optional prev-buf)
  "Major mode for listing the contents of a tar archive file."
    (unwind-protect
	(let ((buffer-read-only t)
	      (mode-name "tm-tar")
	      (mode-line-buffer-identification '("%17b"))
	      )
	    (goto-char (point-min))
	    (tm-tar/move-to-filename)
	    (catch 'tm-tar/tar-mode (tm-tar/command-loop))
	 )
	(if prev-buf
	    (switch-to-buffer prev-buf)
	 )
     ))

(defun tm-tar/command-loop ()
    (let ((old-local-map (current-local-map))
	  )
	(unwind-protect
	    (progn
		(use-local-map tm-tar/tar-mode-map)
		(tm-tar/helpful-message)
		(recursive-edit)
	     )
	    (save-excursion
		(use-local-map old-local-map)
	     ))
     ))

(defun tm-tar/next-line ()
    (interactive)
    (next-line 1)
    (tm-tar/move-to-filename)
  )

(defun tm-tar/previous-line ()
    (interactive)
    (previous-line 1)
    (tm-tar/move-to-filename)
  )

(defun tm-tar/view-file ()
    (interactive)
    (let ((name (tm-tar/get-filename))
	  )
      (save-excursion
	  (switch-to-buffer tm-tar/view-buffer)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (message "Reading a file from an archive. Please wait...")
	  (apply 'call-process tm-tar/tar-program
			 nil t nil (append tm-tar/view-args (list name)))
	  (goto-char (point-min))
       )
	(view-buffer tm-tar/view-buffer)
     ))

(defun tm-tar/get-filename ()
    (let (eol)
	(save-excursion
	    (end-of-line)
	    (setq eol (point))
	    (beginning-of-line)
	    (save-excursion
		(if (re-search-forward "^d" eol t)
			 (error "Cannot view a directory"))
	     )
	    (if (re-search-forward tm-tar/file-search-regexp eol t)
		     (progn (let ((beg (point))
				  )
				(skip-chars-forward "^ \n")
				(buffer-substring beg (point))
				))
		     (error "No file on this line")
	     ))
     ))

(defun tm-tar/move-to-filename ()
    (let ((eol (progn (end-of-line) (point)))
	  )
	(beginning-of-line)
	(re-search-forward tm-tar/file-search-regexp eol t)
     ))

(defun tm-tar/set-properties ()
    (if mouse-button-2
	(let ((beg (point-min))
	      (end (point-max))
	      )
	    (goto-char beg)
	    (save-excursion
		(while (re-search-forward tm-tar/file-search-regexp end t)
		    (tm:add-button (point)
					   (progn
						(end-of-line)
						(point))
					   'tm-tar/view-file)
		 ))
	 )))

(defun tm-tar/helpful-message ()
    (interactive)
    (message "Type %s, %s, %s, %s, %s, %s."
	(substitute-command-keys "\\[Helper-help] for help")
	(substitute-command-keys "\\[tm-tar/helpful-message] for keys")
	(substitute-command-keys "\\[tm-tar/next-line] to next")
	(substitute-command-keys "\\[tm-tar/previous-line] to prev")
	(substitute-command-keys "\\[tm-tar/view-file] to view")
	(substitute-command-keys "\\[exit-recursive-edit] to quit")
     ))

(defun tm-tar/y-or-n-p (prompt)
    (prog1
	(y-or-n-p prompt)
	(message "")
     ))

;;; @@ tar message decoder
;;

(defun mime/decode-message/tar (beg end cal)
    (if (tm-tar/y-or-n-p "Do you want to enter tm-tar mode? ")
	(let ((coding (cdr (assoc 'encoding cal)))
	      (cur-buf (current-buffer))
	      (tm-tar/tar-file-name (expand-file-name (concat (make-temp-name
			(expand-file-name "tm" mime/tmp-dir)) ".tar")))
	      (tm-tar/tmp-file-name (expand-file-name (make-temp-name
			(expand-file-name "tm" mime/tmp-dir))))
	      new-buf
	      )
	    (find-file tm-tar/tmp-file-name)
	    (setq new-buf (current-buffer))
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (save-excursion
		 (set-buffer cur-buf)
		(goto-char beg)
		(re-search-forward "^$")
		(append-to-buffer new-buf (+ (match-end 0) 1) end)
	     )
	    (if (member coding mime-viewer/uuencode-encoding-name-list)
		(progn
		    (goto-char (point-min))
		    (if (re-search-forward "^begin [0-9]+ " nil t)
			(progn
			    (kill-line)
			    (insert tm-tar/tar-file-name)
			 )
			(progn
			    (set-buffer-modified-p nil)
			    (kill-buffer new-buf)
			    (error "uuencode file signature was not found")
			 ))))
	    (save-buffer)
	    (kill-buffer new-buf)
	    (message "Listing the contents of an archive.  Please wait...")
	    (cond ((string-equal coding "base64")
		   (call-process tm-tar/mmencode-program nil nil nil "-u"
				"-o" tm-tar/tar-file-name tm-tar/tmp-file-name)
		   )
		  ((string-equal coding "quoted-printable")
		   (call-process tm-tar/mmencode-program nil nil nil "-u" "-q"
				"-o" tm-tar/tar-file-name tm-tar/tmp-file-name)
		   )
		  ((member coding mime-viewer/uuencode-encoding-name-list)
		   (call-process tm-tar/uudecode-program nil nil nil
				tm-tar/tmp-file-name)
		   )
		  (t
		   (copy-file tm-tar/tmp-file-name tm-tar/tar-file-name t)
		   ))
	    (delete-file tm-tar/tmp-file-name)
	    (setq tm-tar/list-args (list "-tvf" tm-tar/tar-file-name))
	    (setq tm-tar/view-args (list "-xOf" tm-tar/tar-file-name))
	    (if (eq 0 (call-process tm-tar/gzip-program
			    nil nil nil "-t" tm-tar/tar-file-name))
		(progn
		    (setq tm-tar/list-args
			  (append tm-tar/tar-decompress-arg tm-tar/list-args))
		    (setq tm-tar/view-args
			  (append tm-tar/tar-decompress-arg tm-tar/view-args))
		 ))
	    (switch-to-buffer tm-tar/view-buffer)
	    (switch-to-buffer tm-tar/list-buffer)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (apply 'call-process tm-tar/tar-program
		   nil t nil tm-tar/list-args)
	    (if mouse-button-2
		 (progn
		    (make-local-variable 'tm:mother-button-dispatcher)
		    (setq tm:mother-button-dispatcher 'tm-tar/mouse-button-2)
		 ))
	    (tm-tar/set-properties)
	    (tm-tar/tar-mode mime::article/preview-buffer)
	    (kill-buffer tm-tar/view-buffer)
	    (kill-buffer tm-tar/list-buffer)
	    (delete-file tm-tar/tar-file-name)
	 )
     ))

;;; @@ program/buffer coding system
;;;

(cond ((boundp 'MULE)
       (define-program-coding-system tm-tar/view-buffer nil *autoconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code tm-tar/view-buffer nil 1)
       ))

;;; @@ message types to use tm-tar
;;;

(set-atype 'mime/content-decoding-condition
	   '((type . "application/octet-stream")
	     (method . mime/decode-message/tar)
	     (mode . "play") ("type" . "tar")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/octet-stream")
	     (method . mime/decode-message/tar)
	     (mode . "play") ("type" . "tar+gzip")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/x-gzip")
	     (method . mime/decode-message/tar)
	     (mode . "play") ("type" . "tar")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/x-tar")
	     (method . mime/decode-message/tar)
	     (mode . "play")
	     ))

;;; @ end
;;;

(provide 'tm-tar)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
