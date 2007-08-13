;; a simple test of the drop event
;;

(defun some-drop (event)
  (interactive "@e")
  (setq dnd-data (event-dnd-data event))
  (message "At %d,%d; button %d, data %s" (event-x event) (event-y event) (event-button event) (event-dnd-data event)))

(define-key global-map [drop1] 'some-drop)
