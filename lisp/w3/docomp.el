(setq load-path (append (list (expand-file-name "./")
			      (or (getenv "WIDGETDIR")
				  (expand-file-name "../widget"))
			      )
			load-path))

(setq max-specpdl-size (* 10 max-specpdl-size)
      max-lisp-eval-depth (* 10 max-lisp-eval-depth))

(defun hack-dot-emacs ()
  (interactive)
  (let* ((args command-line-args-left)
	 (fname (expand-file-name (nth 0 args)))
	 (lispdir (nth 1 args)))
    (setq command-line-args-left (cdr (cdr (cdr command-line-args-left))))
    (set-buffer (get-buffer-create " *x*"))
    (erase-buffer)
    (if (file-exists-p fname)
	(insert-file-contents fname))
    (goto-char (point-min))
    (if (search-forward ";;; Emacs-w3 configuration options" nil t)
	(message "No changes made.")
      (goto-char (point-max))
      (insert "\n;;; Emacs-w3 configuration options\n")
      (insert "(setq load-path (cons (expand-file-name \""
	      lispdir "\") load-path))\n")
      (insert "(autoload 'w3-preview-this-buffer \"w3\" \"WWW Previewer\" t)\n")
      (insert "(autoload 'w3-follow-url-at-point \"w3\" \"Find document at pt\" t)\n")
      (insert "(autoload 'w3 \"w3\" \"WWW Browser\" t)\n")
      (insert "(autoload 'w3-open-local \"w3\" \"Open local file for WWW browsing\" t)\n")
      (insert "(autoload 'w3-fetch \"w3\" \"Open remote file for WWW browsing\" t)\n")
      (insert "(autoload 'w3-use-hotlist \"w3\" \"Use shortcuts to view WWW docs\" t)\n")
      (insert "(autoload 'w3-show-hotlist \"w3\" \"Use shortcuts to view WWW docs\" t)\n")
      (insert "(autoload 'w3-follow-link \"w3\" \"Follow a hypertext link.\" t)\n")
      (insert "(autoload 'w3-batch-fetch \"w3\" \"Batch retrieval of URLs\" t)\n")
      (insert "(autoload 'url-get-url-at-point \"url\" \"Find the url under the cursor\" nil)\n")
      (insert "(autoload 'url-file-attributes  \"url\" \"File attributes of a URL\" nil)\n")
      (insert "(autoload 'url-popup-info \"url\" \"Get info on a URL\" t)\n")
      (insert "(autoload 'url-retrieve   \"url\" \"Retrieve a URL\" nil)\n")
      (insert "(autoload 'url-buffer-visiting \"url\" \"Find buffer visiting a URL.\" nil)\n")
      (insert "(autoload 'gopher-dispatch-object \"gopher\" \"Fetch gopher dir\" t)\n")
      (insert ";;; End of Emacs-w3 configuration options\n")
      (write-file fname))))

(defun w3-declare-variables (&rest args)
  (while args
    (eval (list 'defvar (car args) nil ""))
    (setq args (cdr args))))

;; For Emacs 19
(w3-declare-variables 'track-mouse 'menu-bar-help-menu 'menu-bar-mode)

;; For XEmacs/Lucid
(w3-declare-variables 'current-menubar 'default-menubar 'extent
		      'mode-motion-hook 'mode-popup-menu 'sound-alist
		      'menubar-visible-p
		      'inhibit-help-echo 'default-toolbar
		      'bottom-toolbar-height 'top-toolbar-height
		      'toolbar-buttons-captioned-p
		      'right-toolbar-width 'left-toolbar-width
		      'top-toolbar 'bottom-toolbar 'right-toolbar
		      'left-toolbar 'device-fonts-cache
		      'has-modeline-p 'baud-rate)

;; For MULE
(w3-declare-variables '*noconv* '*autoconv* '*euc-japan* '*internal*
		      'w3-mime-list-for-code-conversion 'lc-ltn1
		      'mule-version 'enable-multibyte-characters
		      'charset-latin-iso8859-1
		      'file-coding-system-for-read 'file-coding-system)

;; For NNTP
(w3-declare-variables 'nntp-server-buffer 'nntp-server-process 'nntp/connection
		      'gnus-nntp-server 'nntp-server-name 'nntp-version
		      'gnus-default-nntp-server)

;; For xpm-button
(w3-declare-variables 'x-library-search-path)

;; For emacspeak
(w3-declare-variables 'dtk-voice-table 'dtk-punctuation-mode)

;; For a few internal things
(w3-declare-variables 'tag 'w3-working-buffer 'proxy-info 'args
		      'w3-image-widgets-waiting 'w3-form-info
		      'w3-last-parse-tree 'command-line-args-left
		      'standard-display-table 'w3-html-bookmarks
		      'browse-url-browser-function 'widget-keymap)

;; GNUS
(w3-declare-variables 'gnus-group-buffer 'gnus-version)		      

(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))

(require 'w3-vars)
(require 'url)
(require 'mm)
(and w3-running-FSF19
     (< emacs-minor-version 29)
     (require 'font))

(require 'w3-sysdp)
(provide 'ange-ftp)
