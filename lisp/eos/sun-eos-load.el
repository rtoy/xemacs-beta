;;; sun-eos-load.el --- Loads the XEmacs/SPARCworks interface code

;; Copyright (C) 1995  Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks Debugger dbx

;;; Commentary:

;; Load EOS code
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

(load "sun-eos-init" nil t)
(load "sun-eos-common" nil t)
(load "sun-eos-editor" nil t)
(load "sun-eos-browser" nil t)
(load "sun-eos-debugger" nil t)
(load "sun-eos-debugger-extra" nil t)
(load "sun-eos-menubar" nil t)
;; don't load toolbar (load "sun-eos-toolbar" nil t)

(provide 'eos-load)

;;; sun-eos-load.el ends here
