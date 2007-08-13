;;; book.el - Special code for book style.

;; $Id: book.el,v 1.1 1997/04/05 17:56:45 steve Exp $

;;; Code:

(TeX-add-style-hook "book"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; book.el ends here
