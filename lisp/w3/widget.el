;;; widget.el --- a library of user interface components.
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; Version: 0.4

;;; Commentary:
;;
;; The documentation for the unbundled version of this library is
;; available in `custom.texi'.
;;
;; This file only contain the code needed to define new widget types.
;; Everything else is autoloaded from `widget-edit.el'.

;;; Code:

(eval-when-compile (require 'cl))

(let ((keywords 
       '(:create :convert-widget :format :value-create :tag :doc :from :to
		 :args :value :value-from :value-to :action :value-set
		 :value-delete :match :parent :delete :menu-tag-get
		 :value-get :choice :void :menu-tag :on :off :on-type 
		 :off-type :notify :entry-format :button :children
		 :buttons :insert-before :delete-at :format-handler
		 :widget :value-pos :value-to-internal :indent
		 :help-echo
		 :value-to-external :validate :error :directory :must-match
		 :initial :type-error :value-inline :inline :match-inline
		 :greedy :button-face :value-face :keymap :size)))
  (while keywords
    (or (boundp (car keywords))
	(set (car keywords) (car keywords)))
    (setq keywords (cdr keywords))))

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply 'widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc))

(autoload 'widget-create "widget-edit")
(autoload 'widget-insert "widget-edit")

(defun define-widget-group (name class doc &rest args)
  "Define a new widget group named NAME.

CLASS should be nil, it is reserved for future use.

MATCH should be a function taking a widget group and a list of match
types as an argument, and returning the remaining part of the list if
the widget group matches the beginning of the list, or throwing
`no-match' if not.

CREATE should be a function taking a widget group and a list of values
as arguments, and returning a cons whose car is a list of widgets
representing the matches values and whose cdr is the remaining
unmatched values."
  (put name 'widget-group (cons class args)))

;;; The End.

(provide 'widget)

;; widget.el ends here
