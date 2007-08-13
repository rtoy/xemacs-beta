;;; tm-play.el --- decoder for tm-view.el

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/9/26 (separated from tm-view.el)
;; Version: $Id: tm-play.el,v 1.1.1.2 1996/12/21 20:50:43 steve Exp $
;; Keywords: mail, news, MIME, multimedia

;; This file is part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tm-view)

  
;;; @ content decoder
;;;

(defvar mime-preview/after-decoded-position nil)

(defun mime-preview/decode-content ()
  (interactive)
  (let ((pc (mime-preview/point-pcinfo (point))))
    (if pc
	(let ((the-buf (current-buffer)))
	  (setq mime-preview/after-decoded-position (point))
	  (set-buffer (mime::preview-content-info/buffer pc))
	  (mime-article/decode-content
	   (mime::preview-content-info/content-info pc))
	  (if (eq (current-buffer)
		  (mime::preview-content-info/buffer pc))
	      (progn
		(set-buffer the-buf)
		(goto-char mime-preview/after-decoded-position)
		))
	  ))))

(defun mime-article/decode-content (cinfo)
  (let ((beg (mime::content-info/point-min cinfo))
	(end (mime::content-info/point-max cinfo))
	(ctype (or (mime::content-info/type cinfo) "text/plain"))
	(params (mime::content-info/parameters cinfo))
	(encoding (mime::content-info/encoding cinfo))
	)
    ;; Check for VM
    (if (< beg (point-min))
	(setq beg (point-min))
      )
    (if (< (point-max) end)
	(setq end (point-max))
      )
    (let (method cal ret)
      (setq cal (list* (cons 'type ctype)
		       (cons 'encoding encoding)
		       (cons 'major-mode major-mode)
		       params))
      (if mime-viewer/decoding-mode
	  (setq cal (cons
		     (cons 'mode mime-viewer/decoding-mode)
		     cal))
	)
      (setq ret (mime/get-content-decoding-alist cal))
      (setq method (cdr (assq 'method ret)))
      (cond ((and (symbolp method)
		  (fboundp method))
	     (funcall method beg end ret)
	     )
	    ((and (listp method)(stringp (car method)))
	     (mime-article/start-external-method-region beg end ret)
	     )
	    (t
	     (mime-article/show-output-buffer
	      "No method are specified for %s\n" ctype)
	     ))
      )
    ))

(defun field-unifier-for-mode (a b)
  (let ((va (cdr a)))
    (if (if (consp va)
	    (member (cdr b) va)
	  (equal va (cdr b))
	  )
	(list nil b nil)
      )))

(defun mime/get-content-decoding-alist (al)
  (get-unified-alist mime/content-decoding-condition al)
  )


;;; @ external decoder
;;;

(defun mime-article/start-external-method-region (beg end cal)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((method (cdr (assoc 'method cal)))
	    (name (mime-article/get-filename cal))
	    )
	(if method
	    (let ((file (make-temp-name
			 (expand-file-name "TM" mime/tmp-dir)))
		  b args)
	      (if (nth 1 method)
		  (setq b beg)
		(setq b
		      (if (re-search-forward "^$" nil t)
			  (1+ (match-end 0))
			(point-min)
			))
		)
	      (goto-char b)
	      (write-region b end file)
	      (message "External method is starting...")
	      (setq cal (put-alist
			 'name (replace-as-filename name) cal))
	      (setq cal (put-alist 'file file cal))
	      (setq args (nconc
			  (list (car method)
				mime/output-buffer-name (car method)
				)
			  (mime-article/make-method-args cal
							 (cdr (cdr method)))
			  ))
	      (apply (function start-process) args)
	      (mime-article/show-output-buffer)
	      ))
	))))

(defun mime-article/make-method-args (cal format)
  (mapcar (function
	   (lambda (arg)
	     (if (stringp arg)
		 arg
	       (let* ((item (eval arg))
		      (ret (cdr (assoc item cal)))
		      )
		 (if ret
		     ret
		   (if (eq item 'encoding)
		       "7bit"
		     ""))
		 ))
	     ))
	  format))

(defun mime-article/show-output-buffer (&rest forms)
  (get-buffer-create mime/output-buffer-name)
  (let ((the-win (selected-window))
	(win (get-buffer-window mime/output-buffer-name))
	)
    (or win
	(if (and mime/output-buffer-window-is-shared-with-bbdb
		 (boundp 'bbdb-buffer-name)
		 (setq win (get-buffer-window bbdb-buffer-name))
		 )
	    (set-window-buffer win mime/output-buffer-name)
	  (select-window (get-buffer-window mime::article/preview-buffer))
	  (setq win (split-window-vertically (/ (* (window-height) 3) 4)))
	  (set-window-buffer win mime/output-buffer-name)
	  ))
    (select-window win)
    (goto-char (point-max))
    (if forms
	(insert (apply (function format) forms))
      )
    (select-window the-win)
    ))


;;; @ file name
;;;

(defvar mime-viewer/file-name-char-regexp "[A-Za-z0-9+_-]")

(defvar mime-viewer/file-name-regexp-1
  (concat mime-viewer/file-name-char-regexp "+\\."
	  mime-viewer/file-name-char-regexp "+"))

(defvar mime-viewer/file-name-regexp-2
  (concat (regexp-* mime-viewer/file-name-char-regexp)
	  "\\(\\." mime-viewer/file-name-char-regexp "+\\)*"))

(defun mime-article/get-original-filename (param &optional encoding)
  (or (mime-article/get-uu-filename param encoding)
      (let (ret)
	(or (if (or (and (setq ret (mime/Content-Disposition))
			 (setq ret (assoc "filename" (cdr ret)))
			 )
		    (setq ret (assoc "name" param))
		    (setq ret (assoc "x-name" param))
		    )
		(std11-strip-quoted-string (cdr ret))
	      )
	    (if (setq ret
		      (std11-find-field-body '("Content-Description"
					       "Subject")))
		(if (or (string-match mime-viewer/file-name-regexp-1 ret)
			(string-match mime-viewer/file-name-regexp-2 ret))
		    (substring ret (match-beginning 0)(match-end 0))
		  ))
	    ))
      ))

(defun mime-article/get-filename (param)
  (replace-as-filename (mime-article/get-original-filename param))
  )


;;; @ mail/news message
;;;

(defun mime-viewer/quitting-method-for-mime/show-message-mode ()
  (let ((mother mime::preview/mother-buffer)
	(win-conf mime::preview/original-window-configuration)
	)
    (kill-buffer
     (mime::preview-content-info/buffer (car mime::preview/content-list)))
    (mime-viewer/kill-buffer)
    (set-window-configuration win-conf)
    (pop-to-buffer mother)
    ;;(goto-char (point-min))
    ;;(mime-viewer/up-content)
    ))

(defun mime-article/view-message/rfc822 (beg end cal)
  (let* ((cnum (mime-article/point-content-number beg))
	 (cur-buf (current-buffer))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (mother mime::article/preview-buffer)
	 (code-converter
	  (or (cdr (assq major-mode mime-viewer/code-converter-alist))
	      'mime-viewer/default-code-convert-region))
	 str)
    (setq str (buffer-substring beg end))
    (switch-to-buffer new-name)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\n" nil t)
	(delete-region (point-min) (match-end 0))
      )
    (setq major-mode 'mime/show-message-mode)
    (setq mime::article/code-converter code-converter)
    (mime/viewer-mode mother)
    ))


;;; @ message/partial
;;;

(defvar mime-article/coding-system-alist
  (and (boundp 'MULE)
       '((mh-show-mode . *noconv*)
	 (t            . *ctext*)
	 )))		 

(defvar mime-article/kanji-code-alist
  (and (boundp 'NEMACS)
       '((mh-show-mode . nil)
	 (t            . 2)
	 ))) 

(defun mime-article/decode-message/partial (beg end cal)
  (goto-char beg)
  (let* ((root-dir (expand-file-name
		    (concat "m-prts-" (user-login-name)) mime/tmp-dir))
	 (id (cdr (assoc "id" cal)))
	 (number (cdr (assoc "number" cal)))
	 (total (cdr (assoc "total" cal)))
	 (the-buf (current-buffer))
	 file
	 (mother mime::article/preview-buffer)
	 (win-conf (save-excursion
		     (set-buffer mother)
		     mime::preview/original-window-configuration))
	 )
    (if (not (file-exists-p root-dir))
	(make-directory root-dir)
      )
    (setq id (replace-as-filename id))
    (setq root-dir (concat root-dir "/" id))
    (if (not (file-exists-p root-dir))
	(make-directory root-dir)
      )
    (setq file (concat root-dir "/FULL"))
    (if (not (file-exists-p file))
	(progn
	  (re-search-forward "^$")
	  (goto-char (1+ (match-end 0)))
	  (setq file (concat root-dir "/" number))
	  (let ((file-coding-system
		 (cdr
		  (or (assq major-mode mime-article/coding-system-alist)
		      (assq t mime-article/coding-system-alist)
		      )))
		(kanji-fileio-code
		 (cdr
		  (or (assq major-mode mime-article/kanji-code-alist)
		      (assq t mime-article/kanji-code-alist)
		      )))
		)
	    (write-region (point) (point-max) file)
	    )
	  (if (get-buffer mime/temp-buffer-name)
	      (kill-buffer mime/temp-buffer-name)
	    )
	  (switch-to-buffer mime/temp-buffer-name)
	  (let ((i 1)
		(max (string-to-int total))
		(file-coding-system-for-read (if (boundp 'MULE)
						 *noconv*))
		kanji-fileio-code)
	    (catch 'tag
	      (while (<= i max)
		(setq file (concat root-dir "/" (int-to-string i)))
		(if (not (file-exists-p file))
		    (progn
		      (switch-to-buffer the-buf)
		      (throw 'tag nil)
		      ))
		(insert-file-contents file)
		(goto-char (point-max))
		(setq i (1+ i))
		)
	      ;;(delete-other-windows)
	      (let ((buf (current-buffer)))
		(write-file (concat root-dir "/FULL"))
		(set-window-configuration win-conf)
		(let ((win (get-buffer-window mother)))
		  (if win
		      (select-window win)
		    ))
		(set-window-buffer (selected-window) buf)
		;;(set-window-buffer buf)
		(setq major-mode 'mime/show-message-mode)
		)
	      (mime/viewer-mode mother)
	      (pop-to-buffer (current-buffer))
	      ))
	  )
      (progn
	;;(delete-other-windows)
	(set-window-configuration win-conf)
	(select-window (or (get-buffer-window mother)
			   (get-buffer-window
			    (save-excursion
			      (set-buffer mother)
			      mime::preview/article-buffer))
			   (get-largest-window)
			   ))
	(as-binary-input-file
	 (set-buffer (get-buffer-create "FULL"))
	 (insert-file-contents file)
	 )
	(setq major-mode 'mime/show-message-mode)
	(mime/viewer-mode mother)
	;;(pop-to-buffer (current-buffer))
	))
    ))


;;; @ rot13-47
;;;

(defun mime-article/decode-caesar (beg end cal)
  (let* ((cnum (mime-article/point-content-number beg))
	 (cur-buf (current-buffer))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (mother mime::article/preview-buffer)
	 (charset (cdr (assoc "charset" cal)))
	 (encoding (cdr (assq 'encoding cal)))
	 (mode major-mode)
	 str)
    (setq str (buffer-substring beg end))
    (switch-to-buffer new-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\n" nil t)
	(delete-region (point-min) (match-end 0))
      )
    (let ((m (cdr (or (assq mode mime-viewer/code-converter-alist)
		      (assq t mime-viewer/code-converter-alist)))))
      (and (functionp m)
	   (funcall m charset encoding)
	   ))
    (save-excursion
      (set-mark (point-min))
      (goto-char (point-max))
      (tm:caesar-region)
      )
    (view-mode)
    ))


;;; @ end
;;;

(provide 'tm-play)

;;; tm-play.el ends here
