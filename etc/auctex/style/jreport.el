;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.1 1997/04/05 17:56:47 steve Exp $

;;; Code:

(TeX-add-style-hook "jreport"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))


;;; jreport.el ends here
