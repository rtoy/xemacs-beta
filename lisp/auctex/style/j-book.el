;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.1 1997/02/20 02:15:42 steve Exp $

;;; Code:

(TeX-add-style-hook "j-book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-book.el ends here
