;;; delbs.el --- a small lisp package to allow you to swap around DEL/BS keys

;; Copyright (C) 1997 Gary Foster

;; Author: Gary Foster <Gary.Foster@corp.sun.com>
;; Keywords: lisp, terminals

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

;;; Synched up with: not in FSF.

;;; Commentary:

;;   This package should, *theoretically*, serve to quieten the DEL/BS rwars
;;     a little.  By using this package, you can have DEL and BS both do the
;;     same function (normally delete one char to the left) or you can have
;;     then bound separately (DEL --> delete-char, BS --> delete-backward-char)
;;     with all appropriate Meta bindings in each mode.
;;
;; Author: Gary Foster <Gary.Foster@corp.sun.com>
;; Credits due to: Per Abrahamsen <abraham@dina.kvl.dk>

;;; Code:

(defun delbs-enable-delete-forward ()
  "Set up the delete key to delete forward, and backspace to delete backward."
  (interactive)
  (define-key key-translation-map [backspace] "\C-?")
  (define-key key-translation-map [delete] "\C-d")
  (define-key key-translation-map [(meta backspace)] "\M-\C-?")
  (define-key key-translation-map [(meta delete)] "\M-d"))
  
(defun delbs-disable-delete-forward ()
  "Return the DEL/BS key mappings to the XEmacs default"
  (interactive)
  (define-key key-translation-map [backspace] [backspace])
  (define-key key-translation-map [delete] [delete])
  (define-key key-translation-map [(meta backspace)] [(meta backspace)])
  (define-key key-translation-map [(meta delete)] [(meta delete)]))

(provide 'delbs)

;;; delbs.el ends here
