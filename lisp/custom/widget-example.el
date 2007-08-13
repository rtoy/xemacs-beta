;;; widget-example.el -- example of using the widget library

;; Copyright (C) 1996 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; Version: 1.9956
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

(require 'widget)

(require 'wid-edit)
(eval-when-compile (require 'cl))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
		 :size 12
		 :format "Name: %v "
		 "My Name")
  (widget-create 'menu-choice
		 :tag "Choose"
		 :value "This"
		 :help-echo "Choose me, please!"
		 :notify (lambda (widget &rest ignore)
			   (message "%s is a good choice!" 
				    (widget-value widget)))
		 '(item :tag "This option" :value "This")
		 '(choice-item "That option")
		 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-insert "Address: ")
  (widget-create 'editable-field
		 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
		 :notify (lambda (&rest ignore)
			   (widget-value-set widget-example-repeat 
					     '("En" "To" "Tre"))
			   (widget-setup))
		 "other work")
  (widget-insert " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
	(widget-create 'editable-list
		       :entry-format "%i %d %v"
		       :notify (lambda (widget &rest ignore)
				 (let ((old (widget-get widget
							':example-length))
				       (new (length (widget-value widget))))
				   (unless (eq old new)
				     (widget-put widget ':example-length new)
				     (message "You can count to %d." new))))
		       :value '("One" "Eh, two?" "Five!")
		       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
		 :notify (lambda (&rest ignore) (message "Tickle"))
		 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
		 :value "One"
		 :notify (lambda (widget &rest ignore)
			   (message "You selected %s"
				    (widget-value widget)))
		 '(item "One") '(item "Anthor One.") '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore) 
			   (if (= (length (widget-value widget-example-repeat))
				  3)
			       (message "Congratulation!")
			     (error "Three was the count!")))
		 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (widget-example))
		 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))
