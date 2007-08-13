;;; article.el - Special code for article style.

;; $Id: article.el,v 1.1 1997/02/20 02:15:41 steve Exp $

;;; Code:

(TeX-add-style-hook "article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; article.el ends here
