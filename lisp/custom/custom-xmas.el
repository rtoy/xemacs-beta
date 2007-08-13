;;; custom-xmas.el -- XEmacs specific custom support.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.44
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

(unless (featurep 'custom)
  (error "Load `custom.el' instead"))

;; Emacs function missing in XEmacs 19.14.
(unless (fboundp 'x-color-values)
  (defun x-color-values  (color &optional frame)
    "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--(RED GREEN BLUE).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is (65280 65280 65280) or (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
    (color-instance-rgb-components (make-color-instance color))))

;; Overwrite Emacs definition.
(defun custom-facep (face) 
  "Face symbol or object."
  (or (facep face)
      (find-face face)))

;; Support for special XEmacs font attributes.
(autoload 'font-create-object "font" nil)

(unless (fboundp 'face-font-name)
  (defun face-font-name (face &rest args)
    (apply 'face-font face args)))

(defun custom-set-face-font-size (face size &rest args)
  "Set the font of FACE to SIZE"
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (set-font-size fontobj size)
    (apply 'set-face-font face fontobj args)))

(defun custom-set-face-font-family (face family &rest args)
  "Set the font of FACE to FAMILY"
  (let* ((font (apply 'face-font-name face args))
	 (fontobj (font-create-object font)))
    (set-font-family fontobj family)
    (apply 'set-face-font face fontobj args)))

(nconc custom-face-attributes
       '((:family (editable-field :format "Family: %v") 
		  custom-set-face-font-family)
	 (:size (editable-field :format "Size: %v")
		custom-set-face-font-size)))

;; Overwrite Emacs definition.
(defun custom-menu-reset ()
  "Reset customize menu."
  (remove-hook 'custom-define-hook 'custom-menu-reset)
  (when (fboundp 'add-submenu)
    ;; XEmacs with menus.
    (add-submenu '("Help") custom-help-menu)))

(defun get-face-documentation (face)
  "Get the documentation string for FACE."
  (face-property face 'doc-string))

(defun set-face-documentation (face string)
  "Set the documentation string for FACE to STRING."
  (set-face-property face 'doc-string string))

;; custom-xmas.el ends here
