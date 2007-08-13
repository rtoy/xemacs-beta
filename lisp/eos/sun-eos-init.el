;;; sun-eos-init.el --- Initializes the XEmacs/SPARCworks interface

;; Copyright (C) 1996  Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks initialize

;;; Commentary:

;; Initialize EOS
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

;; This stuff needs to be done at startup time
(defun eos::start ()
  "Initialization needed at start-up time.  Should be done by automatic
loading of eos"
  (if (not (and (string-match "XEmacs" emacs-version)
		(emacs-version>= 19 12)))
      (error "Eos version %s only runs on XEmacs 19.12 and later"
	     eos::version))
  (if (not noninteractive)
      (progn
	(eos::common-startup)
	(eos::editor-startup)
	(eos::debugger-startup)
	(eos::debugger-extra-startup)
	(eos::browser-startup)
	(eos::menubar-startup))))

;(add-hook 'before-init-hook 'eos::start t) ; append to the end of hook list

(provide 'eos-init)

;;; sun-eos-init.el ends here
