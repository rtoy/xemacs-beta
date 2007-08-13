;;; w3-prefs.el --- Preferences panels for Emacs-W3
;; Author: wmperry
;; Created: 1997/04/24 15:41:27
;; Version: 1.24
;; Keywords: hypermedia, preferences

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preferences panels for Emacs-W3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)
(require 'w3-keyword)
(require 'w3-toolbar)
(eval-and-compile
  (require 'w3-widget))

(defvar w3-preferences-panel-begin-marker nil)
(defvar w3-preferences-panel-end-marker nil)
(defvar w3-preferences-panels '(
				(appearance    . "Appearance")
				(images        . "Images")
				(cookies       . "HTTP Cookies")
				(hooks         . "Various Hooks")
				(compatibility . "Compatibility")
				(proxy         . "Proxy")
				(privacy       . "Privacy")))

(defun w3-preferences-generic-variable-callback (widget &rest ignore)
  (condition-case ()
      (set (widget-get widget 'variable) (widget-value widget))
    (error (message "Invalid or incomplete data..."))))

(defun w3-preferences-restore-variables (vars)
  (let ((temp nil))
    (while vars
      (setq temp (intern (format "w3-preferences-temp-%s" (car vars))))
      (set (car vars) (symbol-value temp))
      (if (fboundp 'custom-set-variables)
	  (eval (` (custom-set-variables '((, (car vars)) (quote (, (symbol-value temp))) t)))))
      (setq vars (cdr vars)))))
					 
(defun w3-preferences-create-temp-variables (vars)
  (let ((temp nil))
    (while vars
      (setq temp (intern (format "w3-preferences-temp-%s" (car vars))))
      (set (make-local-variable temp) (symbol-value (car vars)))
      (setq vars (cdr vars)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance of the frame / pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-preferences-init-appearance-panel ()
  (let ((vars '(w3-toolbar-orientation
		w3-use-menus
		w3-honor-stylesheets
		w3-default-stylesheet
		w3-default-homepage
		w3-toolbar-type))
	(temp nil))
    (set (make-local-variable 'w3-preferences-temp-use-home-page)
	 (and w3-default-homepage t))
    (w3-preferences-create-temp-variables vars)))

(defun w3-preferences-create-appearance-panel ()
  ;; First the toolbars
  (widget-insert "\nToolbars\n--------\n")
  (widget-insert "\tShow Toolbars as:\t")
  (widget-put
   (widget-create 'radio
		  :value (symbol-value 'w3-preferences-temp-w3-toolbar-type)
		  :notify 'w3-preferences-generic-variable-callback
		  :format "%v"
		  (list 'item :format "%t\t" :tag "Pictures" :value 'pictures)
		  (list 'item :format "%t\t" :tag "Text"     :value 'text)
		  (list 'item :format "%t" :tag "Both" :value 'both))
   'variable 'w3-preferences-temp-w3-toolbar-type)
  (widget-insert "\n\tToolbars appear on ")
  (widget-put
   (widget-create 'choice
		  :value (symbol-value 'w3-preferences-temp-w3-toolbar-orientation)
		  :notify 'w3-preferences-generic-variable-callback
		  :format "%v"
		  :tag "Toolbar Position"
		  (list 'choice-item :format "%[%t%]" :tag "XEmacs Default" :value 'default)
		  (list 'choice-item :format "%[%t%]" :tag "Top" :value 'top)
		  (list 'choice-item :format "%[%t%]" :tag "Bottom" :value 'bottom)
		  (list 'choice-item :format "%[%t%]" :tag "Right" :value 'right)
		  (list 'choice-item :format "%[%t%]" :tag "Left" :value 'left)
		  (list 'choice-item :format "%[%t%]" :tag "No Toolbar" :value 'none))
   'variable 'w3-preferences-temp-w3-toolbar-orientation)
  (widget-insert " side of window.\n")

  ;; Home page
  (widget-insert "\nStartup\n--------\n\tBrowser starts with:\t")
  (widget-put
   (widget-create
    'radio
    :format "%v"
    :value (symbol-value 'w3-preferences-temp-use-home-page)
    :notify 'w3-preferences-generic-variable-callback
    (list 'item :format "%t\t" :tag "Blank Page" :value nil)
    (list 'item :format "%t" :tag "Home Page Location" :value t))
   'variable 'w3-preferences-temp-use-home-page)
  (widget-insert "\n\t\tURL: ")
  (widget-put
   (widget-create
    'editable-field
    :value (or (symbol-value 'w3-preferences-temp-w3-default-homepage) "None")
    :notify 'w3-preferences-generic-variable-callback)
   'variable 'w3-preferences-temp-w3-default-homepage)

  ;; Stylesheet
  (widget-insert "\nStyle\n--------\n\tDefault stylesheet:\t")
  (widget-put
   (widget-create
    'file
    :value (or (symbol-value 'w3-preferences-temp-w3-default-stylesheet) "")
    :must-match t
    :notify 'w3-preferences-generic-variable-callback)
   'variable 'w3-preferences-temp-w3-default-stylesheet)
  (widget-setup)
  )

(defun w3-preferences-save-appearance-panel ()
  (let ((vars '(w3-toolbar-orientation
		w3-use-menus
		w3-honor-stylesheets
		w3-default-stylesheet
		w3-toolbar-type))
	(temp nil))
  (if (symbol-value 'w3-preferences-temp-use-home-page)
      (setq vars (cons 'w3-default-homepage vars))
    (setq w3-default-homepage nil))
  (w3-preferences-restore-variables vars)
  (w3-toolbar-make-buttons)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The images panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-preferences-init-images-panel ()
  (let ((vars '(w3-delay-image-loads
		w3-image-mappings)))
    (w3-preferences-create-temp-variables vars)))

(defun w3-preferences-create-images-panel ()
  (widget-insert "\n")
  (widget-put
   (widget-create
    'checkbox
    :notify 'w3-preferences-generic-variable-callback
    :value (symbol-value 'w3-preferences-temp-w3-delay-image-loads))
   'variable 'w3-preferences-temp-w3-delay-image-loads)
  (widget-insert " Delay Image Loads\n"
  ))

(defun w3-preferences-save-images-panel ()
  (let ((vars '(w3-delay-image-loads
		w3-image-mappings)))
    (w3-preferences-restore-variables vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The cookies panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-preferences-init-cookies-panel ()
  (let ((cookies url-cookie-storage)
	(secure-cookies url-cookie-secure-storage))
    )
  )

(defun w3-preferences-create-cookies-panel ()
  (widget-insert "\n\t\tSorry, not yet implemented.\n\n"))

(defun w3-preferences-save-cookies-panel ()
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The hooks panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-preferences-hooks-variables
  '(w3-load-hook
    w3-mode-hook
    w3-preferences-cancel-hook
    w3-preferences-default-hook
    w3-preferences-ok-hook
    w3-preferences-setup-hook
    w3-source-file-hook))
		
(defun w3-preferences-init-hooks-panel ()
  (w3-preferences-create-temp-variables w3-preferences-hooks-variables))

(defun w3-preferences-create-hooks-panel ()
  (let ((todo w3-preferences-hooks-variables)
	(cur nil)
	(pt nil)
	(doc nil))
    (widget-insert "\n")
    (while todo
      (setq cur (car todo)
	    todo (cdr todo)
	    doc (documentation-property cur 'variable-documentation))
      (if (string-match "^\\*" doc)
	  (setq doc (substring doc 1 nil)))
      (setq pt (point))
      (widget-insert "\n" (symbol-name cur) " - " doc)
      (fill-region-as-paragraph pt (point))
      (setq cur (intern (format "w3-preferences-temp-%s" cur)))
      (widget-put
       (widget-create
	'sexp
	:notify 'w3-preferences-generic-variable-callback
	:value (or (symbol-value cur) "nil"))
       'variable cur))
    (widget-setup)))

(defun w3-preferences-save-hooks-panel ()
  (w3-preferences-restore-variables w3-preferences-hooks-variables))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The compatibility panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-preferences-compatibility-variables
  '(
    (w3-netscape-compatible-comments
     . "Allow Netscape compatible comments")
    (w3-user-colors-take-precedence
     . "Ignore netscape document color control")
    (url-honor-refresh-requests
     . "Allow Netscape `Client Pull'"))
  "A list of variables that the preferences compability pane knows about.")

(defun w3-preferences-init-compatibility-panel ()
  (let ((compat w3-preferences-compatibility-variables)
	(cur nil)
	(var nil))
    (w3-preferences-create-temp-variables
     (mapcar 'car w3-preferences-compatibility-variables))))

(defun w3-preferences-create-compatibility-panel ()
  (let ((compat w3-preferences-compatibility-variables)
	(cur nil)
	(var nil))
    (widget-insert "\n")
    (while compat
      (setq cur (car compat)
	    compat (cdr compat)
	    var (intern (format "w3-preferences-temp-%s" (car cur))))
      (widget-put
       (widget-create 'checkbox
		      :notify 'w3-preferences-generic-variable-callback
		      :value (symbol-value var))
       'variable var)
      (widget-insert " " (cdr cur) "\n\n"))
    (widget-setup)))

(defun w3-preferences-save-compatibility-panel ()
  (w3-preferences-restore-variables
   (mapcar 'car w3-preferences-compatibility-variables)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The proxy configuration panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-preferences-init-proxy-panel ()
  (let ((proxies '("FTP" "Gopher" "HTTP" "Security" "WAIS" "SHTTP" "News"))
	(proxy nil)
	(host-var nil)
	(port-var nil)
	(host nil)
	(port nil)
	(proxy-entry nil))
    (widget-insert "\n")
    (while proxies
      (setq proxy (car proxies)
	    proxies (cdr proxies)
	    host-var (intern (format "w3-%s-proxy-host" (downcase proxy)))
	    port-var (intern (format "w3-%s-proxy-port" (downcase proxy)))
	    proxy-entry (cdr-safe (assoc (downcase proxy) url-proxy-services)))
      (if (and proxy-entry (string-match "\\(.*\\):\\([0-9]+\\)" proxy-entry))
	  (setq host (match-string 1 proxy-entry)
		port (match-string 2 proxy-entry))
	(setq host proxy-entry
	      port nil))
      (set (make-local-variable host-var) (or host ""))
      (set (make-local-variable port-var) (or port ""))))
  (set (make-local-variable 'w3-preferences-temp-no-proxy)
       (cdr-safe (assoc "no_proxy" url-proxy-services))))

(defun w3-preferences-create-proxy-panel ()
  (let ((proxies '("FTP" "Gopher" "HTTP" "Security" "WAIS" "SHTTP" "News"))
	(proxy nil)
	(host-var nil)
	(port-var nil)
	(urlobj nil))
    (widget-insert "\n")
    (while proxies
      (setq proxy (car proxies)
	    proxies (cdr proxies)
	    host-var (intern (format "w3-%s-proxy-host" (downcase proxy)))
	    port-var (intern (format "w3-%s-proxy-port" (downcase proxy))))
      (widget-insert (format "%10s Proxy: " proxy))
      (widget-put
       (widget-create 'editable-field
		      :size 20
		      :value-face 'underline
		      :notify 'w3-preferences-generic-variable-callback
		      :value (format "%-20s" (symbol-value host-var)))
       'variable host-var)
      (widget-insert "  Port: ")
      (widget-put
       (widget-create 'editable-field
		      :size 5
		      :value-face 'underline
		      :notify 'w3-preferences-generic-variable-callback
		      :value (format "%5s" (symbol-value port-var)))
       'variable port-var)
      (widget-insert "\n\n"))
    (widget-insert "        No proxy: ")
    (widget-put
     (widget-create 'editable-field
		    :size 40
		    :value-face 'underline
		    :notify 'w3-preferences-generic-variable-callback
		    :value (or (symbol-value 'w3-preferences-temp-no-proxy) ""))
     'variable 'w3-preferences-temp-no-proxy)
    (widget-setup)))

(defun w3-preferences-save-proxy-panel ()
  (let ((proxies '("FTP" "Gopher" "HTTP" "Security" "WAIS" "SHTTP" "News"))
	(proxy nil)
	(host-var nil)
	(port-var nil)
	(urlobj nil)
	(host nil)
	(port nil)
	(new-proxy-services nil))
    (if (/= 0 (length (symbol-value 'w3-preferences-temp-no-proxy)))
	(setq new-proxy-services (cons
				  (cons
				   "no_proxy"
				   (symbol-value 'w3-preferences-temp-no-proxy))
				  new-proxy-services)))
    (while proxies
      (setq proxy (car proxies)
	    proxies (cdr proxies)
	    host-var (intern (format "w3-%s-proxy-host" (downcase proxy)))
	    port-var (intern (format "w3-%s-proxy-port" (downcase proxy)))
	    urlobj (url-generic-parse-url
		    (cdr-safe
		     (assoc (downcase proxy) url-proxy-services)))
	    host (symbol-value host-var)
	    port (symbol-value port-var))
      (if (and host (/= 0 (length host)))
	  (setq new-proxy-services (cons (cons (downcase proxy)
					       (format "%s:%s" host
						       (or port "80")))
					 new-proxy-services))))
    (setq url-proxy-services new-proxy-services)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Privacy panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst w3-preferences-privacy-bits-sort (bits) 
  (sort bits (function (lambda (a b)
			 (memq b (memq a '(email os lastloc agent cookie)))))))

(defvar url-valid-privacy-levels
  '((paranoid . (email os lastloc agent cookie))
    (high     . (email lastloc))
    (low      . (lastloc))
    (none     . nil)))

(defvar w3-preferences-privacy-bit-widgets nil)
(defvar w3-preferences-privacy-level-widget nil)
(defvar w3-preferences-temp-url-privacy-level nil)
;; darnit i just noticed the checklist widget, this should probably be
;; reimplemented with that instead of checkboxes, but i've almost finished.
(defun w3-preferences-privacy-bit-callback (widget &rest ignore)
  (let ((privacy-bits (if (listp w3-preferences-temp-url-privacy-level)
  			  w3-preferences-temp-url-privacy-level
  			(copy-list (cdr-safe (assq w3-preferences-temp-url-privacy-level url-valid-privacy-levels)))))
  	(bit (widget-get widget 'bit))
  	(val (widget-value widget)))
    (if val
  	(setq privacy-bits (delq bit privacy-bits))
      (setq privacy-bits (w3-preferences-privacy-bits-sort (cons bit (delq bit privacy-bits)))))
    (setq w3-preferences-temp-url-privacy-level
  	  (or (car (rassoc privacy-bits url-valid-privacy-levels))
  	      privacy-bits))
    (widget-value-set w3-preferences-privacy-level-widget 
  		      (if (listp w3-preferences-temp-url-privacy-level)
  			  'custom
  			w3-preferences-temp-url-privacy-level))
    ))


(defun w3-preferences-privacy-level-callback (widget &rest ignore)
  (let* ((val (widget-value widget))
  	 (privacy-bits (cdr-safe (assq val url-valid-privacy-levels))))
    (if (eq val 'custom) nil
      (setq w3-preferences-temp-url-privacy-level val)
      (mapcar (function (lambda (bit)
  			  (widget-value-set (cdr bit)
  					    (not (memq (car bit)
  						       privacy-bits)))))
  	      w3-preferences-privacy-bit-widgets))
    ))

(defun w3-preferences-init-privacy-panel ()
  (w3-preferences-create-temp-variables '(url-privacy-level
					  url-cookie-confirmation))
  (setq w3-preferences-privacy-bit-widgets nil)
  (setq w3-preferences-privacy-level-widget nil))

(defsubst w3-preferences-create-privacy-bit-widget (bit bit-text current-bits)
  (let ((bit-widget (widget-create 
		     'checkbox
		     :value (not (memq bit current-bits))
		     :notify 'w3-preferences-privacy-bit-callback
		     )))
    (widget-put bit-widget 'bit bit)
    (setq w3-preferences-privacy-bit-widgets (cons (cons bit bit-widget)
						   w3-preferences-privacy-bit-widgets))
    (widget-insert " " bit-text "\n")))


(defun w3-preferences-create-privacy-panel ()
  (let ((privacy-bits (if (listp url-privacy-level)
			  url-privacy-level
			(cdr-safe (assq url-privacy-level url-valid-privacy-levels)))))
    (widget-insert "\n")
    (widget-insert "General Privacy Level: ")
    ;;; XXX something is weird with case folding in the following widget if you
    ;;; type an option in lower case it accepts it but doesn't do anything
    (setq w3-preferences-privacy-level-widget
	  (widget-create 
	   'choice
	   :value (if (listp w3-preferences-temp-url-privacy-level)
		      'custom
		    w3-preferences-temp-url-privacy-level)
	    :notify 'w3-preferences-privacy-level-callback
	   :format "%v"
	   :tag "Privacy Level"
	   (list 'choice-item :format "%[%t%]" :tag "Paranoid" :value 'paranoid)
	   (list 'choice-item :format "%[%t%]" :tag "High"     :value 'high)
	   (list 'choice-item :format "%[%t%]" :tag "Low"      :value 'low)
	   (list 'choice-item :format "%[%t%]" :tag "None"     :value 'none)
	   (list 'choice-item :format "%[%t%]" :tag "Custom"   :value 'custom)))
    (widget-put w3-preferences-privacy-level-widget 'variable 'w3-preferences-temp-url-privacy-level)
    
    (widget-insert "\n(controls the options below)\n\nSend the following information with each request:\n")
    (setq w3-preferences-privacy-bit-widgets nil)
    (w3-preferences-create-privacy-bit-widget 'email   "E-mail address" privacy-bits)
    (w3-preferences-create-privacy-bit-widget 'lastloc "Last location visited" privacy-bits)
    (w3-preferences-create-privacy-bit-widget 'os      "Operating system information" privacy-bits)
    (w3-preferences-create-privacy-bit-widget 'agent   "User agent information" privacy-bits)
    (w3-preferences-create-privacy-bit-widget 'cookie  "Accept cookies" privacy-bits)
    (widget-insert "    ")
    (widget-put
     (widget-create 
      'checkbox
      :value (symbol-value 'w3-preferences-temp-url-cookie-confirmation)
      :notify 'w3-preferences-generic-variable-callback)
     'variable 'w3-preferences-temp-url-cookie-confirmation)
    (widget-insert " Ask before accepting cookies\n"))
  (widget-setup))
  
(defun w3-preferences-save-privacy-panel ()
  (w3-preferences-restore-variables '(url-privacy-level
				      url-cookie-confirmation))
  (url-setup-privacy-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-preferences-create-panel (panel)
  (let ((func (intern (format "w3-preferences-create-%s-panel" panel)))
	(inhibit-read-only t))
    (goto-char w3-preferences-panel-begin-marker)
    (delete-region w3-preferences-panel-begin-marker
		   w3-preferences-panel-end-marker)
    (set-marker-insertion-type w3-preferences-panel-end-marker t)
    (if (fboundp func)
	(funcall func)
      (insert (format "You should be seeing %s right now.\n" panel))))
  (set-marker-insertion-type w3-preferences-panel-end-marker nil)
  (set-marker w3-preferences-panel-end-marker (point))
  (goto-char w3-preferences-panel-begin-marker)
  (condition-case ()
      (widget-forward 1)
    (error nil)))

(defun w3-preferences-notify (widget widget-ignore &optional event)
  (let* ((glyph (and event w3-running-xemacs (event-glyph event)))
	 (x     (and glyph (widget-glyphp glyph) (event-glyph-x-pixel event)))
	 (y     (and glyph (widget-glyphp glyph) (event-glyph-y-pixel event)))
	 (map   (widget-get widget 'usemap))
	 (value (widget-value widget)))
    (if (and map x y)
	(setq value (w3-point-in-map (vector x y) map)))
    (if value
	(w3-preferences-create-panel value))))

(defun w3-preferences-save-options ()
  (w3-menu-save-options))

(defun w3-preferences-ok-callback (widget &rest ignore)
  (let ((panels w3-preferences-panels)
	(buffer (current-buffer))
	(func nil))
    (run-hooks 'w3-preferences-ok-hook)
    (while panels
      (setq func (intern
		  (format "w3-preferences-save-%s-panel" (caar panels)))
	    panels (cdr panels))
      (if (fboundp func)
	  (funcall func)))
    (if (fboundp 'custom-save-variables)
	(custom-save-variables))
    (w3-preferences-save-options)
    (message "Options saved")
    (sit-for 1)
    (kill-buffer (current-buffer))))

(defun w3-preferences-reset-all-panels ()
  (let ((panels w3-preferences-panels)
	(func nil))
    (while panels
      (setq func (intern (format "w3-preferences-init-%s-panel"
				 (caar panels)))
	    panels (cdr panels))
      (if (and func (fboundp func))
	  (funcall func)))))

(defun w3-preferences-cancel-callback (widget &rest ignore)
  (if (not (funcall url-confirmation-func "Cancel and lose all changes? "))
      (error "Not cancelled!"))
  (w3-preferences-reset-all-panels)
  (kill-buffer (current-buffer))
  (run-hooks 'w3-preferences-cancel-hook))

(defun w3-preferences-reset-callback (widget &rest ignore)
  (w3-preferences-reset-all-panels)
  (run-hooks 'w3-preferences-default-hook)
  (w3-preferences-create-panel (caar w3-preferences-panels)))

(defvar w3-preferences-setup-hook nil
  "*Hooks to be run before setting up the preferences buffer.")

(defvar w3-preferences-cancel-hook nil
  "*Hooks to be run when cancelling the preferences (Cancel was chosen).")

(defvar w3-preferences-default-hook nil
  "*Hooks to be run when resetting preference defaults (Defaults was chosen).")

(defvar w3-preferences-ok-hook nil
  "*Hooks to be run before saving the preferences (OK was chosen).")

(defun w3-preferences-init-all-panels ()
  (let ((todo w3-preferences-panels)
	(func nil))
    (while todo
      (setq func (intern (format "w3-preferences-init-%s-panel" (caar todo)))
	    todo (cdr todo))
      (and (fboundp func) (funcall func)))))

;;###autoload
(defun w3-preferences-edit ()
  (interactive)
  (let* ((prefs-buffer (get-buffer-create "W3 Preferences"))
	 (widget nil)
	 (inhibit-read-only t)
	 (window-conf (current-window-configuration)))
    (delete-other-windows)
    (set-buffer prefs-buffer)
    (set (make-local-variable 'widget-push-button-gui) nil)
    (w3-preferences-init-all-panels)
    (set-window-buffer (selected-window) prefs-buffer)
    (make-local-variable 'widget-field-face)
    (setq w3-preferences-panel-begin-marker (make-marker)
	  w3-preferences-panel-end-marker (make-marker))
    (set-marker-insertion-type w3-preferences-panel-begin-marker nil)
    (set-marker-insertion-type w3-preferences-panel-end-marker t)
    (use-local-map widget-keymap)
    (erase-buffer)
    (run-hooks 'w3-preferences-setup-hook)
    (setq widget (apply 'widget-create 'menu-choice
			:tag "Panel"
			:notify 'w3-preferences-notify
			:value 'appearance
			(mapcar
			 (function
			  (lambda (x)
			    (list 'choice-item
				  :format "%[%t%]"
				  :tag (cdr x)
				  :value (car x))))
			 w3-preferences-panels)))
    (goto-char (point-max))
    (insert "\n\n")
    (set-marker w3-preferences-panel-begin-marker (point))
    (set-marker w3-preferences-panel-end-marker (point))
    (w3-preferences-create-panel (caar w3-preferences-panels))
    (goto-char (point-max))
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify 'w3-preferences-ok-callback
		   :value "Ok")
    (widget-insert "  ")
    (widget-create 'push-button
		   :notify 'w3-preferences-cancel-callback
		   :value "Cancel")
    (widget-insert "  ")
    (widget-create 'push-button
		   :notify 'w3-preferences-reset-callback
		   :value "Reset")
    (center-region (point-min) w3-preferences-panel-begin-marker)
    (center-region w3-preferences-panel-end-marker (point-max))))

(provide 'w3-prefs)
