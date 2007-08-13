;;; mule-files.el --- File I/O functions for XEmacs/Mule.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Derived from mule.el in the original Mule but heavily modified
;;; by Ben Wing.

;;; Code:

;;;; #### also think more about `binary' vs. `no-conversion'

(setq-default file-coding-system 'iso-2022-8)
(put 'file-coding-system 'permanent-local t)

(defvar overriding-file-coding-system nil
  "Overriding coding system used when reading a file.
You should *bind* this, not set it.  If this is non-nil, it specifies
the coding system that will be used when a file is read in, and
overrides `file-coding-system-for-read', `file-coding-system-alist',
etc.  Use those variables instead of this one for permanent changes
to the environment.")

(defvar file-coding-system-for-read 'autodetect
  "Coding system used when reading a file.
This provides coarse-grained control; for finer-grained control,
use `file-coding-system-alist'.  From a Lisp program, if you wish
to unilaterally specify the coding system used for one
particular operation, you should bind the variable
`overriding-file-coding-system' rather than setting this variable,
which is intended to be used for global environment specification.")

(defvar file-coding-system-alist
  ;;  '(; ("\\.el$" . euc-japan)
  '(("\\.el$" . iso-2022-8)
    ("\\.info$" . iso-2022-8)
    ("\\.\\(gz\\|Z\\)$" . binary)
    ("/spool/mail/.*$" . convert-mbox-coding-system))
  "Alist specifying the coding system used for particular files.
Each element of the alist is a cons of a regexp, specifying the files
to be affected, and a coding system.  This overrides the more
general specification in `file-coding-system-for-read', but is
overridden by `overriding-file-coding-system'.

Instead of a coding system you may specify a function, and it will be
called after the file has been read in to decode the file.  It is
called with four arguments: FILENAME, VISIT, START, and END, the same
as the first four arguments to `insert-file-contents'.")

(defun set-file-coding-system (coding-system &optional force)
  "Set the current `file-coding-system' to CODING-SYSTEM.
If optional argument FORCE (interactively, the prefix argument) is not
given, attempt to match the EOL type of the new coding system to
the current value of `file-coding-system'."
  (interactive "zFile coding system: \nP")
  (get-coding-system coding-system) ;; correctness check
  (if (not force)
      (setq coding-system
	    (subsidiary-coding-system coding-system (coding-system-eol-type
				       		     file-coding-system))))
  (setq file-coding-system coding-system)
  (redraw-modeline t))

(defun set-file-coding-system-for-read (coding-system)
  "Set the coding system used when reading in a file.
This is equivalent to setting the variable `file-coding-system-for-read'.
You can also use `file-coding-system-alist' to specify the coding system
for particular files."
  (interactive "zFile coding system for read: ")
  (get-coding-system coding-system) ;; correctness check
  (setq file-coding-system-for-read coding-system))

(defun set-default-file-coding-system (coding-system)
  "Set the default value of `file-coding-system' to CODING-SYSTEM.
The default value is used both for buffers without associated files
and for files with no apparent coding system (i.e. primarily ASCII).
See `file-coding-system' for more information."
  (interactive "zDefault file coding system: ")
  (setq-default file-coding-system coding-system)
  (redraw-modeline t))

(defun find-file-coding-system-from-filename (filename)
  "Look up a file in `file-coding-system-alist'.
The return value will be nil (no applicable entry), a coding system object
\(the entry specified a coding system), or something else (the entry
specified a function to be called)."
  (let ((alist file-coding-system-alist)
	(found nil)
	(codesys nil))
    (let ((case-fold-search (eq system-type 'vax-vms)))
      (setq filename (file-name-sans-versions filename))
      (while (and (not found) alist)
	(if (string-match (car (car alist)) filename)
	    (setq codesys (cdr (car alist))
		  found t))
	(setq alist (cdr alist))))
    (if codesys
 	(cond ((find-coding-system codesys))
	      (t codesys)))))

(defun convert-mbox-coding-system (filename visit start end)
  "Decoding function for Unix mailboxes.
Does separate detection and decoding on each message, since each
message might be in a different encoding."
  (let ((buffer-read-only nil))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((start (point))
	      end)
	  (forward-char 1)
	  (if (re-search-forward "^From" nil 'move)
	      (beginning-of-line))
	  (setq end (point))
	  (decode-coding-region start end 'autodetect))))))

(defun find-coding-system-magic-cookie ()
  "Look for the coding-system magic cookie in the current buffer.\n"
"The coding-system magic cookie is the exact string\n"
"\";;;###coding system: \" followed by a valid coding system symbol,\n"
"somewhere within the first 3000 characters of the file.  If found,\n"
"the coding system symbol is returned; otherwise nil is returned.\n"
"Note that it is extremely unlikely that such a string would occur\n"
"coincidentally as the result of encoding some characters in a non-ASCII\n"
"charset, and that the spaces make it even less likely since the space\n"
"character is not a valid octet in any ISO 2022 encoding of most non-ASCII\n"
"charsets."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (if (search-forward ";;;###coding system: " (+ (point-min) 3000) t)
	  (let ((start (point))
		(end (progn
		       (skip-chars-forward "^ \t\n\r")
		       (point))))
	    (if (> end start)
		(let ((codesys (intern (buffer-substring start end))))
		  (if (find-coding-system codesys) codesys))))))))

(defun load (file &optional noerror nomessage nosuffix)
  "Execute a file of Lisp code named FILE.
First tries FILE with .elc appended, then tries with .el,
 then tries FILE unmodified.  Searches directories in load-path.
If optional second arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional third arg NOMESSAGE is non-nil.
If optional fourth arg NOSUFFIX is non-nil, don't try adding
 suffixes .elc or .el to the specified name FILE.
Return t if file exists."
  (let* ((filename (substitute-in-file-name file))
	 (handler (find-file-name-handler filename 'load))
	 (path nil))
    (if handler
	(funcall handler 'load filename noerror nomessage nosuffix)
      (if (or (<= (length filename) 0)
	      (null (setq path
			  (locate-file filename load-path
				       (and (not nosuffix) ".elc:.el:")))))
	  (and (null noerror)
	       (signal 'file-error (list "Cannot open load file" filename)))
	(let (__codesys__)
	  (save-excursion
	    (set-buffer (get-buffer-create " *load*"))
	    (erase-buffer)
	    (let ((file-coding-system-for-read 'no-conversion))
	      (insert-file-contents path nil 1 3001))
	    (setq __codesys__ (find-coding-system-magic-cookie)))
	  ;; use string= instead of string-match to keep match-data.
	  (if (string= ".elc" (downcase (substring path -4)))
	      ;; if reading a byte-compiled file and we didn't find
	      ;; a coding-system magic cookie, then use `binary'.
	      ;; We need to guarantee that we never do autodetection
	      ;; on byte-compiled files because confusion here would
	      ;; be a very bad thing.  Pre-existing byte-compiled
	      ;; files are always in the `no-conversion' system.
	      ;; Also, byte-compiled files always use `lf' to terminate
	      ;; a line; don't risk confusion here either.
	      (if (not __codesys__)
		  (setq __codesys__ 'binary))
	    ;; otherwise use `file-coding-system-for-read', as normal
	    ;; #### need to do some looking up in file-coding-system-alist!
	    (if (not __codesys__)
		(setq __codesys__ file-coding-system-for-read)))
	  ;; now use the internal load to actually load the file.
	  (load-internal file noerror nomessage nosuffix __codesys__))))))

(defvar insert-file-contents-access-hook nil
  "A hook to make a file accessible before reading it.
`insert-file-contents' calls this hook before doing anything else.
Called with two arguments: FILENAME and VISIT, the same as the
corresponding arguments in the call to `insert-file-contents'.")

(defvar insert-file-contents-pre-hook nil
  "A special hook to decide the coding system used for reading in a file.

Before reading a file, `insert-file-contents' calls the functions
on this hook with arguments FILENAME and VISIT, the same as the
corresponding arguments in the call to `insert-file-contents'.  In
these functions, you may refer to the global variable
`file-coding-system-for-read'.

The return value of the functions should be either

-- nil
-- A coding system or a symbol denoting it, indicating the coding system
   to be used for reading the file
-- A list of two elements (absolute pathname and length of data inserted),
   which is used as the return value to `insert-file-contents'.  In this
   case, `insert-file-contents' assumes that the function has inserted
   the file for itself and suppresses further reading.

If any function returns non-nil, the remaining functions are not called.")

(defvar insert-file-contents-error-hook nil
  "A hook to set `file-coding-system' when a read error has occurred.

When a file error (e.g. nonexistent file) occurs while read a file,
`insert-file-contents' calls the functions on this hook with three
arguments: FILENAME and VISIT (the same as the corresponding arguments
in the call to `insert-file-contents') and a cons (SIGNALED-CONDITIONS
. SIGNAL-DATA).

After calling this hook, the error is signalled for real and
propagates to the caller of `insert-file-contents'.")

(defvar insert-file-contents-post-hook nil
  "A hook to set `file-coding-system' for the current buffer.

After successful reading, `insert-file-contents' calls the functions
on this hook with four arguments: FILENAME and VISIT (the same as the
corresponding arguments in the call to `insert-file-contents'),
CODING-SYSTEM (the actual coding system used to decode the file), and
a cons of absolute pathname and length of data inserted (the same
thing as will be returned from `insert-file-contents').")

(defun insert-file-contents (filename &optional visit beg end replace)
  "Insert contents of file FILENAME after point.
Returns list of absolute file name and length of data inserted.
If second argument VISIT is non-nil, the buffer's visited filename
and last save file modtime are set, and it is marked unmodified.
If visiting and the file does not exist, visiting is completed
before the error is signaled.

The optional third and fourth arguments BEG and END
specify what portion of the file to insert.
If VISIT is non-nil, BEG and END must be nil.
If optional fifth argument REPLACE is non-nil,
it means replace the current buffer contents (in the accessible portion)
with the file contents.  This is better than simply deleting and inserting
the whole thing because (1) it preserves some marker positions
and (2) it puts less data in the undo list.

NOTE: When Mule support is enabled, the REPLACE argument is
currently ignored.

The coding system used for decoding the file is determined as follows:

1. `overriding-file-coding-system', if non-nil.
2. The result of `insert-file-contents-pre-hook', if non-nil.
3. The matching value for this filename from `file-coding-system-alist',
   if any.
4. `file-coding-system-for-read', if non-nil.
5. The coding system 'no-conversion.

If a local value for `file-coding-system' in the current buffer does
not exist, it is set to the coding system which was actually used for
reading.

See also `insert-file-contents-access-hook', `insert-file-contents-pre-hook',
`insert-file-contents-error-hook', and `insert-file-contents-post-hook'."
  (let (return-val coding-system used-codesys conversion-func)
    ;; OK, first load the file.
    (condition-case err
	(progn
	  (run-hook-with-args 'insert-file-contents-access-hook
			      filename visit)
	  ;; determine the coding system to use, as described above.
	  (setq coding-system
		(or
		 ;; #1.
		 overriding-file-coding-system
		 ;; #2.
		 (run-special-hook-with-args 'insert-file-contents-pre-hook
					     filename visit)
		 ;; #3.
		 (let ((retval (find-file-coding-system-from-filename
				filename)))
		   (if (or (null retval) (coding-system-p retval))
		       retval
		     (setq conversion-func retval)
		     'no-conversion))
		 ;; #4.
		 file-coding-system-for-read
		 ;; #5.
		 'no-conversion))
	  (if (consp coding-system)
	      (setq return-val coding-system)
	    (if (null (find-coding-system coding-system))
		(progn
		  (message "Invalid coding-system (%s), using 'autodetect"
			   coding-system)
		  (setq coding-system 'autodetect)))
	    (setq return-val
		  (insert-file-contents-internal filename visit beg end
						 replace coding-system
						 ;; store here!
						 'used-codesys))
	    ))
      (file-error
       (run-hook-with-args 'insert-file-contents-error-hook
			   filename visit err)
       (signal (car err) (cdr err))))
    (setq coding-system used-codesys)
    ;; call any `post-read-conversion' for the coding system that
    ;; was used ...
    (let ((func
	   (coding-system-property coding-system 'post-read-conversion))
	  (endmark (make-marker)))
      (set-marker endmark (+ (point) (nth 1 return-val)))
      (if func
	  (unwind-protect
	      (save-excursion
		(let (buffer-read-only)
		  (funcall func (point) (marker-position endmark))))
	    (if visit
		(progn
		  (set-buffer-auto-saved)
		  (set-buffer-modified-p nil)))))
      (setcar (cdr return-val) (- (marker-position endmark) (point)))
      ;; also call any post-conversion function called for by
      ;; `file-coding-system-alist'
      (if conversion-func
	  (unwind-protect
	      (save-excursion
		(let (buffer-read-only)
		  (funcall conversion-func (point) (marker-position endmark))))
	    (if visit
		(progn
		  (set-buffer-auto-saved)
		  (set-buffer-modified-p nil)))))
      (setcar (cdr return-val) (- (marker-position endmark) (point))))
    ;; now finally set the buffer's `file-coding-system'.
    (if (run-special-hook-with-args 'insert-file-contents-post-hook
				    filename visit return-val)
	nil
      (if (local-variable-p 'file-coding-system (current-buffer))
	  ;; if file-coding-system is already local, just
	  ;; set its eol type to what was found, if it wasn't
	  ;; set already.
	  (set-file-coding-system
	   (subsidiary-coding-system file-coding-system
				     (coding-system-eol-type coding-system)))
	;; otherwise actually set file-coding-system.
	(set-file-coding-system coding-system)))
    return-val))

(defvar write-region-pre-hook nil
  "A special hook to decide the coding system used for writing out a file.

Before writing a file, `write-region' calls the functions on this hook
with arguments START, END, FILENAME, APPEND, VISIT, and CODING-SYTEM,
the same as the corresponding arguments in the call to
`write-region'.

The return value of the functions should be either

-- nil
-- A coding system or a symbol denoting it, indicating the coding system
   to be used for reading the file
-- A list of two elements (absolute pathname and length of data written),
   which is used as the return value to `write-region'.  In this
   case, `write-region' assumes that the function has written
   the file for itself and suppresses further writing.

If any function returns non-nil, the remaining functions are not called.")

(defvar write-region-post-hook nil
  "A hook called by `write-region' after a file has been written out.

The functions on this hook are called with arguments START, END,
FILENAME, APPEND, VISIT, and CODING-SYSTEM, the same as the
corresponding arguments in the call to `write-region'.")

(defun write-region (start end filename &optional append visit lockname coding-system)
  "Write current region into specified file.
When called from a program, takes three arguments:
START, END and FILENAME.  START and END are buffer positions.
Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).
Optional fifth argument VISIT if t means
  set last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not print the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
Kludgy feature: if START is a string, then that string is written
to the file, instead of any buffer contents, and END is ignored.
Optional seventh argument CODING-SYSTEM specifies the coding system
  used to encode the text when it is written out, and defaults to
  the value of `file-coding-system' in the current buffer.
  Interactively, with a prefix arg, you will be prompted for the
  coding system.
See also `write-region-pre-hook' and `write-region-post-hook'."
  (interactive "r\nFWrite region to file: \ni\ni\ni\nZCoding-system: ")
  (setq coding-system
	(or (run-special-hook-with-args
	     'write-region-pre-hook start end filename append visit lockname)
	    coding-system
	    file-coding-system))
  (if (consp coding-system)
      coding-system
    (let ((func
	   (coding-system-property coding-system 'pre-write-conversion)))
      (if func
	  (let ((curbuf (current-buffer))
		(tempbuf (generate-new-buffer " *temp-write-buffer*"))
		(modif (buffer-modified-p)))
	    (unwind-protect
		(save-excursion
		  (set-buffer tempbuf)
		  (erase-buffer)
		  (insert-buffer-substring curbuf start end)
		  (funcall func (point-min) (point-max))
		  (write-region-internal (point-min) (point-max) filename
					 append
					 (if (eq visit t) nil visit)
					 lockname
					 coding-system))
	      ;; leaving a buffer associated with file will cause problems
	      ;; when next visiting.
	      (kill-buffer tempbuf)
	      (if (or visit (null modif))
		  (progn
		    (set-buffer-auto-saved)
		    (set-buffer-modified-p nil)
		    (if (buffer-file-name) (set-visited-file-modtime))))))
	(write-region-internal start end filename append visit lockname
			       coding-system)))
    (run-hook-with-args 'write-region-post-hook
			start end filename append visit lockname
			coding-system)))

;;; mule-files.el ends here
