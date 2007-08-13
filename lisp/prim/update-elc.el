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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF.

;; Byte compile the .EL files necessary to dump out xemacs.
;; Use this file like this:
;;
;; temacs -batch -l ../lisp/prim/update-elc.el $lisp
;;
;; where $lisp comes from the Makefile.  .elc files listed in $lisp will
;; cause the corresponding .el file to be compiled.  .el files listed in
;; $lisp will be ignored.
;;
;; (the idea here is that you can bootstrap if your .ELC files
;; are missing or badly out-of-date)

(setq update-elc-files-to-compile
      (delq nil
	    (mapcar (function
		     (lambda (x)
		       (if (string-match "\.elc$" x)
			   (let ((src (substring x 0 -1)))
			     (if (file-newer-than-file-p src x)
				 (progn
				   (and (file-exists-p x)
					(null (file-writable-p x))
					(set-file-modes x (logior (file-modes x) 128)))
				   src))))))
		    ;; -batch gets filtered out.
		    (nthcdr 3 command-line-args))))

(if update-elc-files-to-compile
    (progn
      (setq command-line-args
	    (cons (car command-line-args)
		  (append '("-l" "loadup-el.el" "run-temacs"
			    "-batch" "-q" "-no-site-file" "-f"
			    "batch-byte-compile")
			  update-elc-files-to-compile)))
      (load "loadup-el.el")))

(kill-emacs)
