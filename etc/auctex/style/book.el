;;; book.el - Special code for book style.

;; $Id: book.el,v 1.1 1997/08/30 02:45:15 steve Exp $

;;; Code:

(TeX-add-style-hook "book"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; book.el ends here
