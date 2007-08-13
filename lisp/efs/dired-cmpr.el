;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-cmpr.el
;; Dired Version: #Revision: 7.9 $
;; RCS:
;; Description:   Commands for compressing marked files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-cmpr)
(require 'dired)

;;; Entry points.

(defun dired-do-compress (&optional arg files)
  "Compress or uncompress marked (or next ARG) files.
With a zero prefix, prompts for a new value of `dired-compression-method'."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg))
	 files)
     (if (zerop arg)
	 (let ((new (completing-read
		     (format "Set compression method (currently %s): "
			     dired-compression-method)
		     (mapcar
		      (function
		       (lambda (x)
			 (cons (symbol-name (car x)) nil)))
		      dired-compression-method-alist)
		     nil t)))
	   (or (string-equal new "")
	       (setq dired-compression-method (intern new))))
       (setq files (dired-get-marked-files nil current-prefix-arg))
       (or (memq 'compress dired-no-confirm)
	   (let* ((dir (dired-current-directory))
		  (rfiles (mapcar (function
				   (lambda (fn)
				     (dired-make-relative fn dir t)))
				    files))
		  (prompt "")
		  (comp 0)
		  (uncomp nil)
		  (total (length files))
		  elt)
	     (mapcar (function
		      (lambda (fn)
			(if (listp (setq elt
					 (dired-make-compressed-filename fn)))
			    (let* ((method (car (nth 3 elt)))
				   (count (assoc method uncomp)))
			      (if count
				  (setcdr count (1+ (cdr count)))
				(setq uncomp (cons (cons method 1) uncomp))))
			  (setq comp (1+ comp)))))
		     files)
	     (if (/= comp 0)
		 (setq prompt
		       (format "%s %d"
			       (car
				(nth 2
				     (assq dired-compression-method
					   dired-compression-method-alist)))
			       comp)))
	     (if uncomp
		 (let ((case-fold-search t)
		       method)
		   (or (string-equal prompt "")
		       (setq prompt (concat prompt "; ")))
		   (setq uncomp
			 (sort
			  (mapcar
			   (function
			    (lambda (elt)
			      (setq method (car elt))
			      (if (string-equal method "gzip")
				  (setq method "gunzip")
				(or (string-match "^un" method)
				    (setq method (concat "un" method))))
			      (setcar elt method)
			      elt))
			   uncomp)
			  (function
			   (lambda (x y)
			     (string< (car x) (car y))))))
		   (setq prompt
			 (concat prompt
				 (mapconcat
				  (function
				   (lambda (elt)
				     (format "%s %d" (car elt) (cdr elt))))
				  uncomp ", ")))))
	     (cond
	      ((= (length rfiles) 1)
	       (setq prompt (format "%s %s? "
				    ;; Don't need the number 1
				    (substring prompt 0 -2)
				    (car rfiles))))
	      ((or (> (length uncomp) 1) (and (/= 0 comp) uncomp))
	       (setq prompt (format "%s? Total: %d file%s " prompt total
				    (dired-plural-s total))))
	      ((setq prompt (format "%s file%s? " prompt
				    (dired-plural-s total)))))
	     (or (dired-mark-pop-up nil 'compress rfiles 'y-or-n-p prompt)
		 (setq arg 0)))))
     (list arg files)))
       
  (if (not (zerop arg))
      (dired-create-files
       'dired-compress-file
       "Compress or Uncompress"
       files
       (function
	(lambda (fn)
	  (let ((cfn (dired-make-compressed-filename fn)))
	    (if (stringp cfn)
		cfn
	      (substring fn 0 (- (length (nth 1 cfn))))))))
       dired-keep-marker-compress nil t)))

(defun dired-compress-subdir-files (&optional uncompress)
  "Compress all uncompressed files in the current subdirectory.
With a prefix argument uncompresses all compressed files."
  (interactive "P")
  (let ((dir (dired-current-directory))
	files methods uncomp elt)
    (save-excursion
      (save-restriction
	(narrow-to-region (dired-subdir-min) (dired-subdir-max))
	(dired-map-dired-file-lines
	 (function
	  (lambda (f)
	    (if uncompress
		(and (listp (setq uncomp (dired-make-compressed-filename f)))
		     (let ((program (car (nth 3 uncomp))))
		       (setq files (cons f files))
		       (if (setq elt (assoc program methods))
			   (setcdr elt (1+ (cdr elt)))
			 (setq methods (cons (cons program 1) methods)))))
	      (and (stringp (dired-make-compressed-filename f))
		   (setq files (cons f files)))))))))
    (if files
	(let ((total (length files))
	      (rfiles (mapcar
		       (function
			(lambda (fn)
			  (dired-make-relative fn dir t)))
		       files))
	      prompt)
	  (if uncompress
	      (progn
		(setq prompt (mapconcat
			      (function
			       (lambda (x)
				 (format "%s %d"
					 (if (string-equal (car x) "gzip")
					     "gunzip"
					   (if (string-match "^un" (car x))
					       (car x)
					     (concat "un" (car x))))
					 (cdr x))))
			      methods ", "))
		(cond
		 ((= total 1)
		  (setq prompt
			(concat (substring prompt 0 -1) (car rfiles) "? ")))
		 ((= (length methods) 1)
		  (setq prompt
			(format "%s file%s? " prompt (dired-plural-s total))))
		 (t
		  (setq prompt (format "%s? Total: %d file%s " prompt total
				       (dired-plural-s total))))))
	    (setq prompt
		  (if (= total 1)
		      (format "%s %s? " dired-compression-method (car rfiles))
		    (format "%s %d file%s? "
			    dired-compression-method total
			    (dired-plural-s total)))))
	  (if (dired-mark-pop-up nil 'compress rfiles 'y-or-n-p prompt)
	      (dired-create-files
	       'dired-compress-file
	       "Compress or Uncompress"
	       files
	       (function
		(lambda (fn)
		  (let ((cfn (dired-make-compressed-filename fn)))
		    (if (stringp cfn)
			cfn
		      (substring fn 0 (- (length (nth 1 cfn))))))))
	       dired-keep-marker-compress nil t)))
      (message "No files need %scompressing in %s."
	       (if uncompress "un" "")
	       (dired-abbreviate-file-name dir)))))

(defun dired-compress-file (file ok-flag)
  ;; Compress or uncompress FILE.
  ;; If ok-flag is non-nil, it is OK to overwrite an existing
  ;; file. How well this actually works may depend on the compression
  ;; program.
  ;; Return the name of the compressed or uncompressed file.
  (let ((handler (find-file-name-handler file 'dired-compress-file)))
    (if handler
	(funcall handler 'dired-compress-file file ok-flag)
      (let ((compressed-fn (dired-make-compressed-filename file))
	    (err-buff (get-buffer-create " *dired-check-process output*")))
	(save-excursion
	  (set-buffer err-buff)
	  (erase-buffer)
	  (cond ((file-symlink-p file)
		 (signal 'file-error (list "Error compressing file"
					   file "a symbolic link")))
		((listp compressed-fn)
		 (message "Uncompressing %s..." file)
		 (let* ((data (nth 3 compressed-fn))
			(ret
			 (apply 'call-process
				(car data) file t nil
				(append (cdr data)
					(and ok-flag
					     (list (nth 4 compressed-fn)))
					(list file)))))
		   (if (or (and (integerp ret) (/= ret 0))
			   (not (bobp)))
		       (signal 'file-error
			       (nconc
				(list "Error uncompressing file"
				      file)
				(and (not (bobp))
				     (list
				      (progn
					(goto-char (point-min))
					(buffer-substring
					 (point) (progn (end-of-line)
							(point))))))))))
		 (message "Uncompressing %s...done" file)
		 (dired-remove-file file)
		 (let ((to (substring file 0
				      (- (length (nth 1 compressed-fn))))))
		   ;; rename any buffers
		   (and (get-file-buffer file)
			(save-excursion
			  (set-buffer (get-file-buffer file))
			  (let ((modflag (buffer-modified-p)))
			    ;; kills write-file-hooks
			    (set-visited-file-name to)	
			    (set-buffer-modified-p modflag))))
		   to))
		((stringp compressed-fn)
		 (message "Compressing %s..." file)
		 (let* ((data (assq dired-compression-method
				    dired-compression-method-alist))
			(compr-args (nth 2 data))
			(ret
			 (apply 'call-process
				(car compr-args) file t nil
				(append (cdr compr-args)
					(and ok-flag
					     (list (nth 4 data)))
					(list file)))))
		   (if (or (and (integerp ret) (/= ret 0))
			   (not (bobp)))
		       (signal 'file-error
			       (nconc
				(list "Error compressing file"
				      file)
				(and (not (bobp))
				     (list
				      (progn
					(goto-char (point-min))
					(buffer-substring
					 (point) (progn (end-of-line)
							(point))))))))))
		 (message "Compressing %s...done" file)
		 (dired-remove-file file)
		 ;; rename any buffers
		 (and (get-file-buffer file)
		      (save-excursion
			(set-buffer (get-file-buffer file))
			(let ((modflag (buffer-modified-p)))
			  ;; kills write-file-hooks
			  (set-visited-file-name compressed-fn)	
			  (set-buffer-modified-p modflag))))
		 compressed-fn)
		(t (error "Strange error in dired-compress-file."))))))))

(defun dired-make-compressed-filename (name &optional method)
  ;; If NAME is in the syntax of a compressed file (according to
  ;; dired-compression-method-alist), return the data (a list) from this
  ;; alist on how to uncompress it. Otherwise, return a string, the
  ;; compressed form of this file name. This is computed using the optional
  ;; argument METHOD (a symbol). If METHOD is nil, the ambient value of
  ;; dired-compression-method is used.
  (let ((handler (find-file-name-handler
		  name 'dired-make-compressed-filename)))
    (if handler
	(funcall handler 'dired-make-compressed-filename name method)
      (let ((alist dired-compression-method-alist)
	    (len (length name))
	    ext ext-len result)
	(while alist
	  (if (and (> len
		      (setq ext-len (length (setq ext (nth 1 (car alist))))))
		   (string-equal ext (substring name (- ext-len))))
	      (setq result (car alist)
		    alist nil)
	    (setq alist (cdr alist))))
	(or result
	    (concat name
		    (nth 1 (or (assq (or method dired-compression-method)
				     dired-compression-method-alist)
			       (error "Unknown compression method: %s"
				      (or method dired-compression-method))))))
	))))

;;; end of dired-cmpr.el
