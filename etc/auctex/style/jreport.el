;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.1 1997/08/30 02:45:16 steve Exp $

;;; Code:

(TeX-add-style-hook "jreport"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))


;;; jreport.el ends here
