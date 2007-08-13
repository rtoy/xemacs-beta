;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.1 1997/02/20 02:15:20 steve Exp $

;;; Code:

(TeX-add-style-hook "jreport"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))


;;; jreport.el ends here
