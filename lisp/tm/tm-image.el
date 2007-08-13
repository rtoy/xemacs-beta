;;; tm-image.el --- tm-view filter to display images in XEmacs or MULE buffers

;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;; Copyright (C) 1996 Dan Rich

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Dan Rich <drich@morpheus.corp.sgi.com>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/12/15
;; Version: $Id: tm-image.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $

;; Keywords: mail, news, MIME, multimedia, image, picture, X-Face

;; This file is part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;	If you use this program with MULE, please install
;;	etl8x16-bitmap.bdf font included in tl package.

;;; Code:

(require 'tm-view)

(cond (running-xemacs
       (require 'annotations)
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/jpeg"
		  (if (featurep 'jpeg) ; Use built-in suport if available
		      (function mime-preview/filter-for-inline-image)
		    (function mime-preview/filter-for-image)
		    ))
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/gif"
		  (if (featurep 'gif) ; Use built-in suport if available
		      (function mime-preview/filter-for-inline-image)
		    (function mime-preview/filter-for-image)
		    ))
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/x-xpixmap"
		  (if (featurep 'xpm) ; Use built-in suport if available
		      (function mime-preview/filter-for-inline-image)
		    (function mime-preview/filter-for-image)
		    ))
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/tiff" (function mime-preview/filter-for-image))
       (set-alist 'mime-viewer/content-filter-alist
		  "image/x-tiff" (function mime-preview/filter-for-image))
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/x-pic" (function mime-preview/filter-for-image))
       
       (set-alist 'mime-viewer/content-filter-alist
		  "image/x-mag" (function mime-preview/filter-for-image))
       
       (defvar tm-image/inline-image-types
	 (if (featurep 'gif)
	     (nconc
	      '("image/jpeg" "image/gif" "image/tiff"
		"image/x-tiff" "image/x-pic" "image/x-mag"
		"image/x-xbm" "image/x-xpixmap")
	      (if (featurep 'gif)
		  '("application/postscript")
		)
	      )))
       
       (defun bitmap-insert-xbm-file (file)
	 (let (gl)
	   (while (progn
		    (setq gl (make-glyph file))
		    (eq (image-instance-type (glyph-image-instance gl))
			'text)
		    ))
	   (make-annotation gl (point) 'text)
	   ))
       
       (defvar mime-viewer/image-converter-alist
	 '(("image/jpeg"      . jpeg)
	   ("image/gif"       . gif)
	   ("image/x-png"     . png)
	   ("image/x-xpixmap" . xpm)
	   ))
       
       (defvar mime-preview/x-face-function
	 (function mime-preview/x-face-function-use-highlight-headers))
       
       (autoload 'highlight-headers "highlight-headers")
       
       (defun mime-preview/x-face-function-use-highlight-headers ()
	 (highlight-headers (point-min) (re-search-forward "^$" nil t) t)
	 )
       )
      ((featurep 'mule)
       ;; for MULE 2.* or mule merged EMACS
       (require 'x-face-mule)
       
       (defvar tm-image/inline-image-types '("image/x-mag" "image/x-xbm"))
       
       (defvar mime-preview/x-face-function
	 (function x-face-decode-message-header))
       ))

(defvar mime-viewer/shell-command "/bin/sh")
(defvar mime-viewer/shell-arguments '("-c"))

(defvar mime-viewer/ps-to-gif-command "pstogif")

(defvar mime-viewer/graphic-converter-alist
  '(("image/jpeg"   . "djpeg -color 256 < %s | ppmtoxpm > %s")
    ("image/gif"    . "giftopnm < %s | ppmtoxpm > %s")
    ("image/tiff"   . "tifftopnm < %s | ppmquant 256 | ppmtoxpm > %s")
    ("image/x-tiff" . "tifftopnm < %s | ppmquant 256 | ppmtoxpm > %s")
    ("image/x-pic"  . "pictoppm < %s | ppmquant 256 | ppmtoxpm > %s")
    ("image/x-mag"  . "magtoppm < %s | ppmtoxpm > %s")
    ))


;;; @ X-Face
;;;

(defvar mime-viewer/x-face-to-xbm-command
  (concat mime-viewer/x-face-to-pbm-command " | pbmtoxbm"))

(if mime-preview/x-face-function
    (add-hook 'mime-viewer/content-header-filter-hook
	      mime-preview/x-face-function)
  )


;;; @ content filter for images
;;;
;;    (for XEmacs 19.12 or later)

(defun mime-preview/filter-for-image (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (filter (assoc-value ctype mime-viewer/graphic-converter-alist))
	 )
    (if filter
	(let* ((beg (point-min)) (end (point-max))
	       (orig-file
		(make-temp-name (expand-file-name "tm" mime/tmp-dir)))
	       (xbm-file (concat orig-file ".xbm"))
	       gl annot)
	  ;;(remove-text-properties beg end '(face nil))
	  (mime-decode-region beg end encoding)
	  (write-region (point-min)(point-max) orig-file)
	  (delete-region (point-min)(point-max))
	  (message "Now translating, please wait...")
	  (apply (function call-process)
		 mime-viewer/shell-command nil nil nil
		 (append mime-viewer/shell-arguments
			 (list (format filter orig-file xbm-file)))
		 )
	  (setq gl (make-glyph xbm-file))
	  (setq annot (make-annotation gl (point) 'text))
	  (unwind-protect
	      (delete-file orig-file)
	    (condition-case nil
		(delete-file xbm-file)
	      (error nil)))
	  (goto-char (point-max))
	  (insert "\n")
	  (message "Translation done.")
	  )
      (message (format "%s is not supported." ctype))
      )))


;;; @ content filter for xbm
;;;

(defun mime-preview/filter-for-image/xbm (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (assoc "charset" params))
	 (beg (point-min)) (end (point-max))
	 (xbm-file (make-temp-name (expand-file-name "tm" mime/tmp-dir)))
	 )
    (remove-text-properties beg end '(face nil))
    (mime-decode-region beg end encoding)
    (write-region (point-min)(point-max) xbm-file)
    (delete-region (point-min)(point-max))
    (bitmap-insert-xbm-file xbm-file)
    (delete-file xbm-file)
    ))

(set-alist 'mime-viewer/content-filter-alist
	   "image/xbm" (function mime-preview/filter-for-image/xbm))

(set-alist 'mime-viewer/content-filter-alist
	   "image/x-xbm" (function mime-preview/filter-for-image/xbm))


;;; @ content filter for support in-line image types
;;;
;;    (for XEmacs 19.14 or later)

(defun mime-preview/filter-for-inline-image (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (assoc "charset" params))
	 (beg (point-min)) (end (point-max))
	 )
    (remove-text-properties beg end '(face nil))
    (mime-decode-region beg end encoding)
    (let ((data (buffer-string))
	  (minor (assoc-value ctype mime-viewer/image-converter-alist))
	  gl)
      (delete-region (point-min)(point-max))
      (while (progn
	       (setq gl (make-glyph (vector minor :data data)))
	       (eq (image-instance-type (glyph-image-instance gl))
		   'text)
	       ))
      (make-annotation gl (point) 'text)
      )
    (insert "\n")
    ))


;;; @ content filter for Postscript
;;;
;;    (for XEmacs 19.14 or later)

(defun mime-preview/filter-for-application/postscript (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (beg (point-min)) (end (point-max))
	 (file-base (make-temp-name (expand-file-name "tm" mime/tmp-dir)))
	 (ps-file (concat file-base ".ps"))
	 (gif-file (concat file-base ".gif"))
	 )
    (remove-text-properties beg end '(face nil))
    (mime-decode-region beg end encoding)
    (write-region (point-min)(point-max) ps-file) 
    (delete-region (point-min)(point-max))
    (call-process mime-viewer/ps-to-gif-command nil nil nil ps-file)
    (let (gl)
      (while (progn
	       (setq gl (make-glyph (vector 'gif :file gif-file)))
	       (eq (image-instance-type (glyph-image-instance gl))
		   'text)
	       ))
      (make-annotation gl (point) 'text)
      )
    (delete-file ps-file)
    (delete-file gif-file)
    ))

(set-alist 'mime-viewer/content-filter-alist
	   "application/postscript"
	   (function mime-preview/filter-for-application/postscript))


;;; @ setting
;;;

(mapcar
 (lambda (ctype)
   (or (member ctype mime-viewer/default-showing-Content-Type-list)
       (setq mime-viewer/default-showing-Content-Type-list
	     (cons ctype
		   mime-viewer/default-showing-Content-Type-list))
       ))
 tm-image/inline-image-types)


;;; @ end
;;;

(provide 'tm-image)

;;; tm-image.el ends here
