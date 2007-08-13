;; a simple test of the drag functions
;;
;; still to do: drag only after about 10 pix of movement

(defun text-drag (event)
  (interactive "@e")
  (offix-start-drag event "That's a test"))

(defun file-drag (event)
  (interactive "@e")
  (offix-start-drag event "/tmp/printcap" 2))

(defun url-drag (event)
  (interactive "@e")
  (offix-start-drag event "http://www.xemacs.org/" 8))

(defun files-drag (event)
  (interactive "@e")
  (offix-start-drag event '("/tmp/dragtest" "/tmp/droptest" "/tmp/printcap") 3))

(define-key global-map [button1] 'text-drag)
(define-key global-map [button2] 'files-drag)
(define-key global-map [button3] 'url-drag)
