(setq load-path (cons (expand-file-name "./") load-path))

(defun url-declare-variables (&rest args)
  (while args
    (eval (list 'defvar (car args) nil ""))
    (setq args (cdr args))))

;; Various internals
(url-declare-variables 'proxy-info 'mm-mime-data
		       'mm-content-transfer-encodings)

;; For Emacs 19
(url-declare-variables 'track-mouse 'menu-bar-help-menu)

;; For MULE
(url-declare-variables '*noconv* '*autoconv* '*euc-japan* '*internal*
		      'file-coding-system-for-read 'file-coding-system)

;; For Mailcrypt
(url-declare-variables 'mc-pgp-path 'mc-pgp-key-begin-line 'mc-ripem-pubkeyfile
		      'mc-default-scheme 'mc-flag)

;; For NNTP
(url-declare-variables 'nntp-server-buffer 'nntp-server-process
		       'nntp/connection 'gnus-nntp-server
		       'nntp-server-name 'nntp-version
		       'gnus-default-nntp-server)

;; For ps-print
(url-declare-variables 'ps-bold-faces 'ps-italic-faces 'ps-print-version)

;; For xpm-button
(url-declare-variables 'x-library-search-path)

(url-declare-variables 'command-line-args-left 'standard-display-table)

(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))

(require 'url-vars)
