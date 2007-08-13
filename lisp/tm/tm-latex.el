;;;
;;; tm-latex: tm-view internal decoder for LaTeX
;;;
;;; by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp> (1994/11/11)
;;;
;;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;
;;; $Id: tm-latex.el,v 1.3 1996/12/29 00:15:14 steve Exp $
;;;

(require 'tm-view)

(defun mime/decode-text/latex (beg end cal)
  (let* ((cur-buf (current-buffer))
	 new-buf
	 (name (or (cdr (assoc "name" cal))
		   (cdr (assoc "x-name" cal))
		   (concat (make-temp-name "tm") ".tex"))))
    (switch-to-buffer mime::article/preview-buffer)
    (funcall mime/find-file-function (expand-file-name name mime/tmp-dir))
    (if (or (<= (buffer-size) 0)
	    (y-or-n-p "Replace the existing buffer?"))
	(progn
	  (erase-buffer)
	  (setq new-buf (current-buffer))
	  (save-excursion
	    (set-buffer cur-buf)
	    (goto-char beg)
	    (re-search-forward "^$")
	    (append-to-buffer new-buf (+ (match-end 0) 1) end)
	    )))
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "text/x-latex")
	     (method . mime/decode-text/latex)
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/x-latex")
	     (method . mime/decode-text/latex)
	     ))

;(set-atype 'mime/content-decoding-condition
;	   '((type . "application/octet-stream")
;	     ("type" . "latex")
;	     (method . mime/decode-text/latex)
;	     ))

(provide 'tm-latex)
