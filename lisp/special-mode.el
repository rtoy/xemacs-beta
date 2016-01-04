;;; special-mode.el --- Special major mode to view specially formatted data

;; Copyright (C) 2011 Didier Verna <didier@xemacs.org>

;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Keywords:      dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is dumped with XEmacs.


;;; Code:

;; This code is imported from GNU Emacs 23.3.1 -- dvl

(defvar special-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'quit-window)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "?" 'describe-mode)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map "g" 'revert-buffer)
    map))

(put 'special-mode 'mode-class 'special)
(define-derived-mode special-mode nil "Special"
  "Parent major mode from which special major modes should inherit."
  (setq buffer-read-only t))


;;; special-mode.el ends here
