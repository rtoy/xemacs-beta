;;; All the autoloads for emacs-w3

;; About pages
(autoload 'w3-about "w3-about")

;; Annotation handling
(autoload 'w3-parse-personal-annotations "w3-annotat")
(autoload 'w3-is-personal-annotation "w3-annotat")
(autoload 'w3-delete-personal-annotation "w3-annotat")
(autoload 'w3-personal-annotation-add "w3-annotat")
(autoload 'w3-annotation-minor-mode "w3-annotat")
(autoload 'w3-annotation-add "w3-annotat")

;; Hotlist handling
(autoload 'w3-read-html-bookmarks "w3-hot")
(autoload 'w3-hotlist-apropos "w3-hot")
(autoload 'w3-hotlist-refresh "w3-hot")
(autoload 'w3-hotlist-delete "w3-hot")
(autoload 'w3-hotlist-rename-entry "w3-hot")
(autoload 'w3-hotlist-append "w3-hot")
(autoload 'w3-parse-hotlist "w3-hot")
(autoload 'w3-use-hotlist "w3-hot")
(autoload 'w3-hotlist-add-document-at-point "w3-hot")
(autoload 'w3-hotlist-add-document "w3-hot")

;; Printing
(autoload 'w3-print-with-ps-print "w3-print")
(autoload 'w3-print-this-url "w3-print")
(autoload 'w3-print-url-under-point "w3-print")
(autoload 'w3-parse-tree-to-latex "w3-latex")
(autoload 'w3-show-dvi "w3-latex")

;; Stylesheet stuff
(autoload 'w3-handle-style "w3-style")
(autoload 'w3-style-parse-css "w3-style")
(autoload 'w3-generate-stylesheet-faces "w3-style")

;; Setup stuff
(autoload 'url-do-setup "url")
(autoload 'w3-do-setup "w3")

;; Forms stuff
(autoload 'w3-form-add-element "w3-forms")
(autoload 'w3-do-text-entry "w3-forms")
(autoload 'w3-do-form-entry "w3-forms")
(autoload 'widget-at "w3-forms")
(autoload 'w3-next-widget "w3-forms")

;; Widget stuff
(autoload 'widget-setup "widget-edit")
(autoload 'widget-create "widget-edit")
(autoload 'widget-get "widget-edit")
(autoload 'widget-put "widget-edit")
(autoload 'widget-forward "widget-edit")
(autoload 'widget-backward "widget-edit")

;; Preferences
(autoload 'w3-preferences-edit "w3-prefs")

(defvar widget-field-new nil)

(provide 'w3-auto)

