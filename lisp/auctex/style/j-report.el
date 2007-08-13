;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.1 1997/02/20 02:15:42 steve Exp $

;;; Code:

(TeX-add-style-hook "j-report"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-report.el ends here
