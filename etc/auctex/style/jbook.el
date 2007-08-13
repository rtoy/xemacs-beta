;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.1 1997/08/30 02:45:16 steve Exp $

;;; Code:

(TeX-add-style-hook "jbook"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; jbook.el ends here
