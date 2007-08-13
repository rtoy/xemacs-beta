;;; advise-eval-region.el --- Wrap advice around eval-region
;; Copyright (C) 1996 Miranova Systems, Inc.

;; Original-Author: Unknown
;; Adapted-By: Steven L Baur <steve@miranova.com>
;; Keywords: extensions lisp

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file splits out advice to eval-region formerly done in cl-read.el.
;; Due to the way cl-read.el reads itself in twice during bytecompilation,
;; and the fact that functions shouldn't be advised twice, I split this out
;; into its own file.

;;; Code:

(require 'advice)

;; Advise the redefined eval-region
(defadvice eval-region (around cl-read activate)
  "Use the reader::read instead of the original read if cl-read-active."
  (with-elisp-eval-region (not cl-read-active)
    ad-do-it))

(provide 'advise-eval-region)

;;; advise-eval-region.el ends here
