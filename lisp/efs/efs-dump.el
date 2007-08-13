;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dump.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:
;; Description:  Install a bare-bones EFS hook into file-name-handler-alist
;;               for dumping
;; Author:       Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(setq file-name-handler-alist
      (cons
       (cons efs-path-root-regexp 'efs-file-handler-function)
       file-name-handler-alist))
