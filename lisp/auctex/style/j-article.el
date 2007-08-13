;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.1 1997/02/20 02:15:20 steve Exp $

;;; Code:

(TeX-add-style-hook "j-article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; j-article.el ends here
