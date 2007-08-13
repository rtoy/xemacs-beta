;;; danish.el - Setup AUC TeX for editing Danish text.

;; $Id: danish.el,v 1.1 1997/05/27 22:13:46 steve Exp $

;;; Code:

(TeX-add-style-hook "danish"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; danish.el ends here
