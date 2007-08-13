;;; All the autoloads for emacs-w3

;; About pages
(autoload 'w3-about "w3-about")

;; Hotlist handling
(autoload 'w3-read-html-bookmarks "w3-hot" nil t)
(autoload 'w3-hotlist-apropos "w3-hot" nil t)
(autoload 'w3-hotlist-refresh "w3-hot" nil t)
(autoload 'w3-hotlist-delete "w3-hot" nil t)
(autoload 'w3-hotlist-rename-entry "w3-hot" nil t)
(autoload 'w3-hotlist-append "w3-hot" nil t)
(autoload 'w3-parse-hotlist "w3-hot")
(autoload 'w3-use-hotlist "w3-hot" nil t)
(autoload 'w3-hotlist-add-document-at-point "w3-hot" nil t)
(autoload 'w3-hotlist-add-document "w3-hot" nil t)

;; Printing
(autoload 'w3-print-this-url "w3-print" nil t)
(autoload 'w3-print-url-under-point "w3-print" nil t)
(autoload 'w3-parse-tree-to-latex "w3-latex")
(autoload 'w3-show-dvi "w3-latex" nil t)

;; Stylesheet stuff
(autoload 'w3-handle-style "w3-style")
(autoload 'w3-display-stylesheet "w3-style" nil t)

;; Setup stuff
(autoload 'url-do-setup "url")
(autoload 'w3-do-setup "w3")

;; Forms stuff
(autoload 'w3-form-resurrect-widgets "w3-forms")
(autoload 'w3-form-add-element "w3-forms")
(autoload 'w3-do-text-entry "w3-forms")
(autoload 'w3-do-form-entry "w3-forms")
(autoload 'w3-next-widget "w3-forms")

;; Widget stuff
(autoload 'widget-setup "wid-edit")
(autoload 'widget-create "wid-edit")
(autoload 'widget-get "wid-edit")
(autoload 'widget-put "wid-edit")
(autoload 'widget-forward "wid-edit")
(autoload 'widget-backward "wid-edit")
(autoload 'widget-at "wid-edit")

;; URL stuff
(autoload 'url-gateway-nslookup-host "url-gw")

;; Preferences
(autoload 'w3-preferences-edit "w3-prefs" nil t)

(defvar widget-field-new nil)

(provide 'w3-auto)

