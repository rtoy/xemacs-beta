;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.1 1997/04/05 17:56:46 steve Exp $

;;; Code:

(TeX-add-style-hook "j-report"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-report.el ends here
