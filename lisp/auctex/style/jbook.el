;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.1 1997/02/20 02:15:20 steve Exp $

;;; Code:

(TeX-add-style-hook "jbook"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; jbook.el ends here
