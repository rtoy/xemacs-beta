;;; report.el - Special code for report style.

;; $Id: report.el,v 1.1 1997/04/05 17:56:47 steve Exp $

;;; Code:

(TeX-add-style-hook "report"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; report.el ends here
