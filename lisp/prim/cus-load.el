;;; cus-load.el --- Batch load all available cus-load files

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@altair.xemacs.org>
;; Keywords: internal, help, faces

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; custom loads are no longer in a single file.  So, collect the whole set!

;;; Code:

(let ((dir load-path))
  (while dir
    (condition-case nil
	(load (concat (car dir) "/cus-load"))
      (file-error nil))
    (pop dir)))

(provide 'cus-load)

;;; cus-load.el ends here
