;;; No byte-compiler warnings
;(eval-when-compile
;  (require 'w3))

;;; Keep these obsolete variables for backward compatibility
(defconst era-version "0.95" "\
Version numbers of this version of Era.")

;; We're (not really anymore) calling this version Sun Era.
(defconst sun-era t)

(defun era-version ()
  "Return (obsolete) string describing the version of Era that is running.
Era is now known as XEmacs.  Use (emacs-version) instead."
  (interactive)
  (if (interactive-p)
      (message "%s" (era-version))
    (format "%sEra %s of %s %s on %s (%s)"
	    (if sun-era "Sun " "")
	    era-version
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time
                       (string-match "[0-9]*$" emacs-build-time))
	    emacs-build-system system-type)))

;(defun sunpro-maybe-connect-to-tooltalk ()
;  (if (and (not (noninteractive))
;	   (fboundp 'command-line-do-tooltalk))
;      (command-line-do-tooltalk nil)))

;; sunpro-maybe-connect-to-tooltalk must appear in the hook list
;; before any clients that register patterns, like eos-load.el.
;; Currently eos-load.el places its functions at the end of the list

;(add-hook 'before-init-hook 'sunpro-maybe-connect-to-tooltalk)

(set-glyph-image text-pointer-glyph    "xterm")
(set-glyph-image nontext-pointer-glyph "xterm")

;; W3 doesn't know about using pageview, so let's fix that.
;; There doesn't seem to be any such function `w3-parse-mailcap' - mrb

;(defun sunpro-fix-postscript-viewer ()
;  (if (not (noninteractive))
;    (condition-case nil
;      (w3-parse-mailcap
;        (expand-file-name "sparcworks/sunpro-mailcap" data-directory))
;      (error nil))))

;(add-hook 'w3-load-hooks 'sunpro-fix-postscript-viewer)

;; turn on pending delete without messing up its autoloads
;(defun sunpro-pending-delete-on ()
;  (if (not (noninteractive))
;      (pending-delete-on nil)))

;(add-hook 'before-init-hook 'sunpro-pending-delete-on)

;;; Decide whether to use workshop.el or eos at runtime, based on
;;; which Sun DevPro products are installed.

(defun sunpro-update-paths-for-workshop ()
  "Update exec-path and load-path to find supporting workshop files.
Returns nil if the required files cannot be found."
  (and
   (cond
    ((locate-file "workshop.el" load-path))
    
    ((file-exists-p (concat sunpro-dir "lib/workshop.el"))
     (setq load-path (append load-path (list (concat sunpro-dir "lib/"))))))
   
   (cond
    ((locate-file "workshop" exec-path))
    
    ((file-exists-p (concat sunpro-dir "bin/workshop"))
     (setq exec-path (append exec-path (list (concat sunpro-dir "bin/"))))))))

(defun sunpro-startup ()
  (when (not (noninteractive))

    (let ((sunpro-dir-p
           #'(lambda (dir)
               (and dir
                    (file-exists-p (concat dir "bin/workshop"))
                    (file-exists-p (concat dir "lib/workshop.el"))))))
    
      (defconst sunpro-dir
        (cond
         ((let ((path exec-path) dir (found nil))
            (while (and path (not found))
              (setq dir (car path))
              (setq path (cdr path))
              (setq dir (concat dir (if (string-match "/$" dir) "../" "/../")))
              (setq found (funcall sunpro-dir-p dir)))
            (if found
                (expand-file-name dir))))
       
         ((let ((dir exec-directory) (i 0) (found nil))
            (while (and (< i 8) (not found))
              (setq i (1+ i))
              (setq dir (concat dir "../"))
              (setq found (funcall sunpro-dir-p dir))
              )
            (if found
                (expand-file-name dir))))

         ("/opt/SUNWspro/"))                  ; Default install location

        "Directory where Sunsoft Developer Products are installed."))

    ;; Connect to tooltalk
    (and (featurep 'tooltalk)
         (fboundp 'command-line-do-tooltalk)
         (command-line-do-tooltalk nil))
    
    ;; Sun's pending-del default is like textedit's
    (require 'pending-del)
    (pending-delete-on nil)
    
    ;; Bar cursor 2 pixels wide
    (setq bar-cursor 2)

    ;; Nice CDE compliant icon -- now the default...
    ;(if (featurep 'xpm)
    ;    (set-glyph-image
    ;     frame-icon-glyph
    ;     (format "%s%s" data-directory "xemacs-icon3.xpm")
    ;     'global 'x))
    
    (cond
     ;; Use Sunsoft WorkShop if available
     ((sunpro-update-paths-for-workshop)
      (require 'workshop))

     ;; Else, use eos package with sparcworks if available
     ((or
       (locate-file "sparcworks" exec-path)
       (prog1
           (file-exists-p (concat sunpro-dir "bin/sparcworks"))
         (setq exec-path (append exec-path (list (concat sunpro-dir "bin/"))))))
      
      (and (fboundp 'eos::start)) (eos::start))
     
     (t ; Neither? Complain...
      (display-warning
       'sunpro
       "XEmacs was compiled with support for Sun Developer Products,
but neither `workshop' nor `sparcworks' were found on the PATH.")))
    ))

(add-hook 'before-init-hook 'sunpro-startup)

(provide 'sunpro)
