;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-xy.el
;; Dired Version: $Revision: 7.9 $
;; RCS:
;; Description:   Commands for reading mail from dired.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-xy)
(require 'dired)

;;; Special request: Will an mh-e user please write some mh support in here?

(defun dired-read-mail (&optional arg)
  "Reads the current file as a mail folder.
Uses the setting of `dired-mail-reader' to determine which reader to use.
Possibilities are VM or RMAIL. With a prefix arg, visits the folder read-only\;
this only works with VM."
  (interactive "P")
  (cond
   ((eq dired-mail-reader 'vm)
    (dired-vm arg))
   ((eq dired-mail-reader 'rmail)
    (dired-rmail)) ; doesn't take read-only arg.
   (t (error "Never heard of the mail reader %s" dired-mail-reader))))

;; Read-only folders only work in VM 5, not in VM 4.
(defun dired-vm (&optional read-only)
  "Run VM on this file.
With prefix arg, visit folder read-only (this requires at least VM 5).
See also variable `dired-vm-read-only-folders'."
  (interactive "P")
  (let ((dir (dired-current-directory))
	(fil (dired-get-filename)))
    ;; take care to supply 2nd arg only if requested - may still run VM 4!
    (require 'vm) ; vm-visit-folder may not be an autoload
    (setq this-command 'vm-visit-folder) ; for vm window config
    (if read-only
	(vm-visit-folder fil t)
      (vm-visit-folder fil))
    ;; so that pressing `v' inside VM does prompt within current directory:
    (set (make-local-variable 'vm-folder-directory) dir)))

(defun dired-rmail ()
  "Run RMAIL on this file."
  (interactive)
  (rmail (dired-get-filename)))

;; end of dired-xy.el

