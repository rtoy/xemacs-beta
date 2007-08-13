;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-mule.el
;; Dired Version: #Revision: 7.9 $
;; RCS:
;; Description:   MULE support for dired.
;; Created:       Sun Jul 17 14:45:12 1994 by sandy on ibm550
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Acknowledgements:
;;
;;  Ishikawa Ichiro for sending MULE patches and information.

(require 'dired)

(defun dired-find-file (&optional coding-system)
  "In dired, visit the file or directory named on this line."
  (interactive "ZCoding-system: ")
  (find-file (dired-get-filename) coding-system))

(defun dired-find-file-other-window (&optional display coding-system)
  "In dired, visit this file or directory in another window.
With a prefix, the file is displayed, but the window is not selected."
  (interactive "P\nZCoding-system: ")
  (if display
      (dired-display-file coding-system)
    (find-file-other-window (dired-get-filename) coding-system)))

(defun dired-display-file (&optional coding-system)
  "In dired, displays this file or directory in the other window."
  (interactive "ZCoding-system: ")
  (display-buffer
   (find-file-noselect (dired-get-filename) coding-system)))

;;; end of dired-mule.el
