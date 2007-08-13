;;;
;;; $Id: sc-setup.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
;;;

(require 'emu)


;;; @ for Super Cite
;;;

(if (< emacs-major-version 19)
    (autoload 'sc-cite-original "sc" nil t)
  (autoload 'sc-cite-original	"supercite" "supercite 3.1" t)
  (autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)
  )

(setq sc-citation-leader "")

(cond ((boundp 'MULE)
       ;; for MULE
       (setq sc-cite-regexp "\\s *\\([a-zA-Z0-9]\\|\\cj\\)*>+\\s *")
       )
      ((boundp 'NEMACS)
       ;; for Nemacs
       (setq sc-cite-regexp
	     "\\s *\\([a-zA-Z0-9]\\|\\cc\\|\\cC\\|\\ch\\|\\cH\\|\\ck\\|\\cK\\)*>+\\s *")
       ))

(if (< emacs-major-version 19)
    (progn
      (defun my-sc-overload-hook ()
	(require 'sc-oloads)
	(sc-overload-functions)
	)

      ;; @@ for all but mh-e
      ;;
      (setq mail-yank-hooks (function sc-cite-original))

      ;; @@ for RMAIL, PCMAIL, GNUS
      ;;
      (add-hook 'mail-setup-hook (function my-sc-overload-hook))

      ;; @@ for Gnus
      ;;
      (add-hook 'news-reply-mode-hook (function my-sc-overload-hook))
      (add-hook 'gnews-ready-hook (function my-sc-overload-hook))
      
      ;; @@ for mh-e
      ;;
      (add-hook 'mh-letter-mode-hook (function my-sc-overload-hook))
      (setq mh-yank-hooks 'sc-cite-original)  ; for MH-E only
      )
  (add-hook 'mail-citation-hook 'sc-cite-original)
  (setq news-reply-header-hook nil)
  )


;;; @ for sc-register
;;;
;; (setq sc-load-hook
;;       '(lambda ()
;;	  (require 'sc-register)
;;	  (setq sc-rewrite-header-list
;;		(append sc-rewrite-header-list
;;			(list (list 'sc-header-in-Japanese))
;;			))
;;	  (setq sc-preferred-header-style
;;		(- (length sc-rewrite-header-list) 1))
;;	  ))
(setq sc-preferred-attribution 'registeredname)


;;; @ end
;;;

(provide 'sc-setup)
