;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.1 1997/08/30 02:45:16 steve Exp $

;;; Code:

(TeX-add-style-hook "j-report"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-report.el ends here
