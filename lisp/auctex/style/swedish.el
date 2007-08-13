;;; swedish.el - Setup AUC TeX for editing Swedish text.

;; $Id: swedish.el,v 1.1 1997/02/20 02:15:21 steve Exp $

;;; Commentary:
;;
;; Apparently the Swedes use ''this style'' quotations.

(TeX-add-style-hook "swedish"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (setq TeX-open-quote "''")
   (run-hooks 'TeX-language-sv-hook))))
