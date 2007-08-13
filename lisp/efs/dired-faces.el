;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-faces.el
;; Dired Version: $Revision: 1.2 $
;; RCS:
;; Description:   rudimentary face customization support for dired
;; Author:        Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-faces)

(require 'custom)

;;; Variables

(defgroup dired nil
  "Directory editing."
  :group 'emacs)

(defcustom dired-do-highlighting t
  "Set if we should use highlighting according to filetype."
  :type 'boolean
  :group 'dired)

(defcustom dired-do-interactive-permissions t
  "Set if we should allow interactive chmod."
  :type 'boolean
  :group 'dired)

(defface dired-face-marked '((((class color))
			       (:background "PaleVioletRed"))
			     (t (:underline t)))
  "Face used for marked files."
  :group 'dired)

(defface dired-face-flagged '((((class color))
			       (:background "LightSlateGray"))
			      (t (:underline t)))
  "Face used for flagged files."
  :group 'dired)

(defface dired-face-directory '((t (:bold t)))
  "Face used for directories."
  :group 'dired)

(defface dired-face-executable '((((class color))
				  (:foreground "SeaGreen"))
				 (t (:bold t)))
  "Face used for executables."
  :group 'dired)

(defface dired-face-setuid '((((class color))
			      (:foreground "Red"))
			     (t (:bold t)))
  "Face used for setuid executables."
  :group 'dired)

(defface dired-face-boring '((((class color))
			      (:foreground "Grey"))
			     (((class grayscale))
			      (:foreground "Grey")))
  "Face used for unimportant files."
  :group 'dired)

(defface dired-face-permissions '((t (:background "grey75")
				     (:foreground "black")))
  "Face used for interactive permissions."
  :group 'dired)

(defface dired-face-socket '((((class color))
			      (:foreground "magenta"))
			     (t (:bold nil)))
  "Face used to indicate sockets."
  :group 'dired)

(defface dired-face-symlink  '((((class color))
			      (:foreground "cyan"))
			     (t (:bold t)))
  "Face used to indicate symbolic links."
  :group 'dired)

;;; end of dired-faces.el
