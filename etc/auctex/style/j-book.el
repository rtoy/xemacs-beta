;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.1 1997/08/30 02:45:16 steve Exp $

;;; Code:

(TeX-add-style-hook "j-book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-book.el ends here
