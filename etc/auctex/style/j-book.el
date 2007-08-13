;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.1 1997/04/05 17:56:46 steve Exp $

;;; Code:

(TeX-add-style-hook "j-book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-book.el ends here
