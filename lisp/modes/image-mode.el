;;; image-mode.el --- Major mode for navigate images

;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/6/27
;; Version: image-mode.el,v 20.3.1.2 1997/07/01 17:29:44 morioka Exp
;; Keywords: image, graphics

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defvar buffer-image-format nil)
(make-variable-buffer-local 'buffer-image-format)

(defsubst image-decode (start end type)
  "Decode the image between START and END which is encoded in TYPE."
  (save-excursion
    (let ((image (make-image-instance
		  (vector type :data (buffer-string)) nil nil 'no-error)))
      (delete-region start end)
      (if image
	  (let ((glyph (make-glyph image)))
	    (set-extent-begin-glyph (make-extent start start) glyph)
	    (setq buffer-read-only t)
	    )
	(insert (format "%s is not supported!\n" type))
	(let ((overriding-local-map image-mode-map))
	  (insert
	   (substitute-command-keys
	    "
Please type `\\[image-toggle-decoding]' if you would like to display
raw data.
Please type `\\[image-enter-hexl-mode]' if you would like to edit hex
data.
Please type `\\[image-start-external-viewer]' if you would like to
display contents of this buffer by external viewer.\n")))
	(call-interactively 'fill-paragraph)
	)
      start)))

(defvar image-mode-map (make-keymap))
(suppress-keymap image-mode-map)
(define-key image-mode-map "v" 'image-start-external-viewer)
(define-key image-mode-map "t" 'image-toggle-decoding)
(define-key image-mode-map "h" 'image-enter-hexl-mode)
(define-key image-mode-map "q" 'image-mode-quit)

(defvar image-external-viewer
  (cond ((exec-installed-p "display")	 "display")	; ImageMagic
	((exec-installed-p "xv")	 "xv")		; xv
	)
  "*External viewer for image-mode.")

(defun image-start-external-viewer ()
  "Start external image viewer for current-buffer.
It uses `image-external-viewer' as external image viewer."
  (interactive)
  (start-process "external image viewer" nil
		 image-external-viewer buffer-file-name)
  )

(defun image-toggle-decoding ()
  "Toggle image display mode in current buffer."
  (interactive)
  (if buffer-file-format
      (progn
	(setq buffer-read-only nil)
	(erase-buffer)
	(map-extents (function
		      (lambda (extent maparg)
			(delete-extent extent)
			)) nil (point-min)(point-min))
	(setq buffer-file-format nil)
	(insert-file-contents-literally buffer-file-name)
	(set-buffer-modified-p nil)
	)
    (format-decode-buffer buffer-image-format)
    ))

(defun image-exit-hexl-mode-function ()
  (format-decode-buffer)
  (remove-hook 'hexl-mode-exit-hook 'image-exit-hexl-mode-function)
  )

(defun image-enter-hexl-mode ()
  "Enter to hexl-mode."
  (interactive)
  (when buffer-file-format
    (setq buffer-read-only nil)
    (erase-buffer)
    (map-extents (function
		  (lambda (extent maparg)
		    (delete-extent extent)
		    )) nil (point-min)(point-min))
    (setq buffer-file-format nil)
    (insert-file-contents-literally buffer-file-name)
    (set-buffer-modified-p nil)
    (add-hook 'hexl-mode-exit-hook 'image-exit-hexl-mode-function)
    )
  (hexl-mode)
  )

(defun image-mode-quit ()
  "Exit image-mode."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun image-maybe-restore ()
  "Restore buffer from file if it is decoded as `buffer-file-format'."
  (when (and buffer-file-format
	     buffer-file-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (map-extents (function
		  (lambda (extent maparg)
		    (delete-extent extent)
		    )) nil (point-min)(point-min))
    (setq buffer-file-format nil)
    (insert-file-contents-literally buffer-file-name)
    (set-buffer-modified-p nil)
    ))

(add-hook 'change-major-mode-hook 'image-maybe-restore)


;;;###autoload
(defun image-mode (&optional arg)
  "\\{image-mode-map}"
  (interactive)
  (setq major-mode 'image-mode)
  (setq mode-name "Image")
  (use-local-map image-mode-map)
  )

;;;###autoload
(defun image-decode-jpeg (start end)
  "Decode JPEG image between START and END."
  (setq buffer-image-format 'image/jpeg)
  (image-decode start end 'jpeg)
  )

;;;###autoload
(defun image-decode-gif (start end)
  "Decode GIF image between START and END."
  (setq buffer-image-format 'image/gif)
  (image-decode start end 'gif)
  )

;;;###autoload
(defun image-decode-png (start end)
  "Decode PNG image between START and END."
  (setq buffer-image-format 'image/png)
  (image-decode start end 'png)
  )

;;;###autoload
(defun image-decode-xpm (start end)
  "Decode XPM image between START and END."
  (setq buffer-image-format 'image/x-xpm)
  (image-decode start end 'xpm)
  )

(provide 'image-mode)

;;; image-mode.el ends here
