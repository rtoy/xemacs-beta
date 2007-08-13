;;;
;;; tm-ftp: anonymous ftp processor for tm-view
;;;
;;; by MASUTANI Yasuhiro <masutani@me.es.osaka-u.ac.jp> (1994/11/ 5)
;;;    
;;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>  (1994/11/ 8)
;;;         and OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp> (1994/11/11)
;;;
;;; $Id: tm-ftp.el,v 1.2 1996/12/28 21:03:14 steve Exp $
;;;

(require 'tm-view)
(require 'ange-ftp)

(defvar mime/dired-function
  (if mime/use-multi-frame
      (function dired-other-frame)
    (function dired)
    ))

(defun mime/decode-message/external-ftp (beg end cal)
  (let ((access-type (cdr (assoc "access-type" cal)))
	(site (cdr (assoc "site" cal)))
	(directory (cdr (assoc "directory" cal)))
	(name (cdr (assoc "name" cal)))
	(mode (cdr (assoc "mode" cal)))
	(pathname))
    (setq pathname
	  (concat "/anonymous@" site ":" directory))
    (message (concat "Accessing " pathname "/" name "..."))
    (switch-to-buffer mime::article/preview-buffer)
    (funcall mime/dired-function pathname)
    (goto-char (point-min))
    (search-forward name)
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "message/external-body")
	     ("access-type" . "anon-ftp")
	     (method . mime/decode-message/external-ftp)
	     ))

(provide 'tm-ftp)
