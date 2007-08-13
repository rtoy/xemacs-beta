;; Vietnamese language specific setup for Mule
;; Copyright (C) 1992 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of GNU Emacs).
;; This file contains European characters.

;; Mule is free software distributed in the form of patches to GNU Emacs.
;; You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; Mule is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; 93.5.25  created for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
;;; 93.7.22  modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>

(defconst viqr-regexp
  "[aeiouyAEIOUY]\\([(^+]?['`?~.]\\|[(^+]\\)\\|[Dd][Dd]")

;;;###autoload
(defun vn-compose-viqr (from to)
  "Convert 'VIQR' mnemonics of the current region to
pre-composed Vietnamese characaters."
  (interactive "r")
  (let (quail-current-package map key def)
    (quail-use-package "viqr")
    (setq map (quail-map))
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward viqr-regexp   0 t)
	(setq key (buffer-substring (match-beginning 0) (match-end 0)))
	(setq def (lookup-key map key))
	(if (numberp def)
	    (if (> def 2)
		(setq key (substring key 0 (1- def))
		      def (lookup-key map key))))
	(if (keymapp def)
	    (progn
	      (goto-char (match-beginning 0))
	      (delete-region (point) (+ (point) (length key)))
	      (insert (quail-get-candidate def t))))))))

;;;###autoload
(defun vn-compose-viqr-buffer ()
  (interactive)
  (vn-compose-viqr (point-min) (point-max)))

;;;###autoload
(defun vn-decompose-viqr (from to)
  "Convert pre-composed Vietnamese characaters of the current region to
'VIQR' mnemonics."
  (interactive "r")
  (let (quail-current-package decode-map key def)
    (quail-use-package "viqr")
    (setq decode-map (quail-decode-map))
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\cv"   0 t)
	(setq def (preceding-char))
	(if (setq key (assq def decode-map))
	    (progn
	      (delete-char -1)
	      (insert (cdr key))))))))

;;;###autoload
(defun vn-decompose-viqr-buffer ()
  (interactive)
  (vn-decompose-viqr (point-min) (point-max)))

;;;
(provide 'viet)
(provide 'vietnamese)
