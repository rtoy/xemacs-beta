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
  "Runs at startup if support for Sun Workshop is compiled in.  Don't run this."
  
  ;; Sun distribution censors yow, among other things...
  (unless (locate-file "yow.el" load-path)
    (fmakunbound 'yow)
    (delete-menu-item '("Apps" "Games" "Quote from Zippy"))
    (delete-menu-item '("Apps" "Games" "Psychoanalyze Zippy!")))
  
  (when (not (noninteractive))

    (flet
        ((sunpro-dir-p (dir)
                       (and dir
                            (file-exists-p (concat dir "bin/workshop"))
                            (file-exists-p (concat dir "lib/workshop.el")))))
      (defconst sunpro-dir
        (cond
         ;; Look on the PATH
         ((let ((path exec-path) dir (found nil))
            (while (and path (not found))
              (setq dir (or (car path) "."))
              (setq path (cdr path))
              (setq dir (concat dir (if (string-match "/$" dir) "../" "/../")))
              (setq found (sunpro-dir-p dir)))
            (if found
                (expand-file-name dir))))

         ;; Check for standard Sun DevPro CD Install layout
         ((if (string-match "contrib/[^/]+/[^/]+/[^/]+/[^/]+/$" exec-directory)
              (let ((dir (substring exec-directory 0 (match-beginning 0))))
                (if (sunpro-dir-p dir)
                    (expand-file-name dir)))))
         
         ;; Default install location
         ("/opt/SUNWspro/"))

        "Directory where Sun Developer Products are installed."))

    ;; Sunpro ships the mule version as a 2-file addition to the
    ;; non-mule distribution - the binary and the doc file.
    ;;
    ;; This is a quick hack, I know...
    ;; There ought to be a better way to do this.
    ;; Perhaps a --xemacs-flavor=mule flag?
    (if (featurep 'mule)
        (let ((mule-doc-file-name (concat internal-doc-file-name "-mule")))
          (if (file-exists-p (concat doc-directory mule-doc-file-name))
              (setq internal-doc-file-name mule-doc-file-name))))

    ;; Connect to tooltalk, but only on an X server.
    (when (and (featurep 'tooltalk)
	       (fboundp 'command-line-do-tooltalk)
	       (eq 'x (device-type)))
      (command-line-do-tooltalk nil))
    
    ;; Sun's pending-del default is like textedit's
    (require 'pending-del)
    (turn-on-pending-delete)
    
    ;; Bar cursor 2 pixels wide
    (setq bar-cursor 2)

    ;; Nice CDE compliant icon -- now the default...
    ;;(if (featurep 'xpm)
    ;;    (set-glyph-image
    ;;     frame-icon-glyph
    ;;     (format "%s%s" data-directory "xemacs-icon3.xpm")
    ;;     'global 'x))
    
    (cond
     ;; Use Sun WorkShop if available
     ((sunpro-update-paths-for-workshop)
      ;; Unfortunately, changes to the default toolbar in 20.3 b21
      ;; have broken workshop-frob-toolbar in workshop.el.  Since new
      ;; XEmacsen have to work with older WorkShops, this must be
      ;; fixed both in workshop.el (distributed on the Sun WorkShop CD)
      ;; and worked-around here.
      (set-specifier default-toolbar
		     (append (specifier-instance default-toolbar)
			     `([,(toolbar-make-button-list nil)
				workshop-bugfix nil nil])))
      (require 'workshop)
      (set-specifier default-toolbar
		     (delete-if (lambda (b) (eq (aref b 1) 'workshop-bugfix))
				(specifier-instance default-toolbar))))

     ;; Else, use eos package with sparcworks if available
     ((or
       (locate-file "sparcworks" exec-path)
       (prog1
           (file-exists-p (concat sunpro-dir "bin/sparcworks"))
         (setq exec-path (append exec-path (list (concat sunpro-dir "bin/"))))))
      
      (load "sun-eos-init")
      (load "sun-eos-common")
      (load "sun-eos-editor")
      (load "sun-eos-browser")
      (load "sun-eos-debugger")
      (load "sun-eos-debugger-extra")
      (load "sun-eos-menubar")
      (eos::start))
     
     (t ; Neither? Complain...
      (display-warning
       'sunpro
       "XEmacs was compiled with support for Sun Developer Products,
but neither `workshop' nor `sparcworks' were found on the PATH.")))
    ))

(add-hook 'before-init-hook 'sunpro-startup)

(provide 'sunpro)
