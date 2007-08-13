;;; swedish.el - Setup AUC TeX for editing Swedish text.

;; $Id: swedish.el,v 1.1 1997/08/30 02:45:17 steve Exp $

;;; Commentary:
;;
;; Apparently the Swedes use ''this style'' quotations.

(TeX-add-style-hook "swedish"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (setq TeX-open-quote "''")
   (run-hooks 'TeX-language-sv-hook))))
