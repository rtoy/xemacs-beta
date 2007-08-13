;;; article.el - Special code for article style.

;; $Id: article.el,v 1.1 1997/04/05 17:56:45 steve Exp $

;;; Code:

(TeX-add-style-hook "article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; article.el ends here
