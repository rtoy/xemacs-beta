;;
;; make-vars.el -- make skk-vars.el
;;

(require 'autoload)

(defvar skk-foreword-file "skk-foreword.el")
(defvar skk-variable-file "skk-vars.el")
(defvar skk-autoload-cookie ";;;###skk-autoload")

(defvar skk-files
  '("skk.el"
    "skk-auto.el"
    "skk-comp.el"
    "skk-gadget.el"
    "skk-isearch.el"
    "skk-kakasi.el"
    "skk-kcode.el"
    "skk-menu.el"
    "skk-num.el"
    "skk-server.el"
    "skk-tree.el"
    "skk-vip.el"
    "skk-viper.el" ))

(defun make-skk-vars ()
  (interactive)
  (let* ((generated-autoload-file skk-variable-file)
	 (buf (find-file-noselect generated-autoload-file))
	 (generate-autoload-cookie skk-autoload-cookie)
	 (coding-system-for-write 'junet)
	 (file-coding-system '*junet*) )
    (set-buffer buf)
    (delete-region (point-min) (point-max))
    (insert-string (format "(require '%s)\n"
			   (file-name-sans-extension skk-foreword-file) ))
    (mapcar
     (function
      (lambda (file)
	(generate-file-autoloads file)))
     skk-files)
    (goto-char (point-max))
    (insert-string (format "(provide '%s)\n"
			   (file-name-sans-extension skk-variable-file) ))
    (save-buffer)))
