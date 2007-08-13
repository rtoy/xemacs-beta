;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.1 1997/04/05 17:56:46 steve Exp $

;;; Code:

(TeX-add-style-hook "j-article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; j-article.el ends here
