;;; report.el - Special code for report style.

;; $Id: report.el,v 1.1 1997/02/20 02:15:21 steve Exp $

;;; Code:

(TeX-add-style-hook "report"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; report.el ends here
