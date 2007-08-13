;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:          dired-shell.el
;; Dired Version: $Revision: 1.1 $
;; RCS:
;; Description:   Commands for running shell commands on marked files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Requirements and provisions
(provide 'dired-shell)
(require 'dired)
(autoload 'comint-mode "comint")

;;; Variables

(defvar dired-postscript-print-command
  (concat
   (if (boundp 'lpr-command)
       lpr-command
     (if (memq system-type
	       '(usg-unix-v hpux silicon-graphics-unix))
	 "lp"
       "lpr"))
   (if (and (boundp 'lpr-switches) lpr-switches)
       (concat " "
	       (mapconcat 'identity lpr-switches " ")
	       " ")
     " "))
  "Command to print a postscript file.")

(defvar dired-text-print-command (concat dired-postscript-print-command " -p")
  "Command to print a text file.")

(defvar dired-print-program-alist
  (list
   (cons "\\.gif$" (concat "giftoppm * | ppmtopgm | pnmtops | "
			   dired-postscript-print-command))
   (cons "\\.\\(fts\\|FTS\\)$" (concat "fitstopgm * | pnmtops | "
				       dired-postscript-print-command))
   ;; People with colour printers won't want the g-flag in djpeg
   (cons "\\.\\(JPG\\|jpg\\)$" (concat "djpeg -Pg * | pnmtops | "
				       dired-postscript-print-command))
   (cons "\\.ps\\.\\(gz\\|Z\\)$" (concat "zcat * | "
					 dired-postscript-print-command))
   (cons "\\.ps$" dired-postscript-print-command)
   (cons "\\.\\(gz\\|Z\\)$" (concat "zcat * | "
				    dired-postscript-print-command))
   (cons "\\.dvi$" "dvips")
   (cons ".*" dired-text-print-command))
  "Alist of regexps and print commands.
This is used by `dired-do-print' to determine the default print command for
printing the marked files.")

(defvar dired-auto-shell-command-alist nil
  "*Alist of regexps and command lists to guess shell commands.
Each element of this list should be a list of regular expression, and a list
of guesses for shell commands to be used if the file name matches the regular
expression. The list of guesses is evalled. This alist is appended to the front
of dired-default-auto-shell-command-alist before prompting for each shell
command.")

(defvar dired-default-auto-shell-command-alist
  (list

   ;; Archiving
   '("\\.tar$"
     (if dired-gnutar-program
	 (concat dired-gnutar-program " xvf")
       "tar xvf")
     (if dired-gnutar-program
	 (concat dired-gnutar-program " tvf")
       "tar tvf"))
   ;; regexps for compressed archives must come before the .Z rule to
   ;; be recognized:
   '("\\.tar\\.\\([zZ]\\|gz\\)\\|\\.tgz$" ; .tgz is for DOS
     (if dired-gnutar-program
	 (concat dired-gnutar-program " zxvf")
       "zcat * | tar xvf -")
     (if dired-gnutar-program
	 (concat dired-gnutar-program " ztvf")
       "zcat * | tar tvf -"))
   '("\\.shar.[zZ]$" (if dired-unshar-program
			 (concat "zcat * | " dired-unshar-program)
		       "zcat * | sh"))
   '("\\.zoo$" "zoo x//")
   '("\\.zip$" "unzip" "unzip -v")
   '("\\.lzh$" "lharc x")
   '("\\.arc$" "arc x")
   '("\\.shar$" (if dired-unshar-program dired-unshar-program "sh"))

   ;; Encoding/compressing
   '("\\.uu$" "uudecode")
   '("\\.hqx$" "mcvert")

   ;; Executing (in the generalized sense)
   '("\\.sh$" "sh")			; execute shell scripts
   '("^[Mm]akefile$" "make -f *")
   '("\\.diff$" "patch -t <")

   ;; Displaying (assumes X)
   '("\\.xbm$" "bitmap")		; view X11 bitmaps
   '("\\.gp$" "gnuplot")
   '("\\.gif$" "xv")			; view gif pictures
   '("\\.fig$" "xfig")			; edit fig pictures
   '("\\.ps$" "ghostview")

   ;; Typesetting.  For printing documents, see dired-print-program-alist.
   '("\\.tex$" "latex" "tex")
   '("\\.texi\\(nfo\\)?$" "makeinfo" "texi2dvi")
   (if (eq window-system 'x)
       (if dired-use-file-transformers
	   '("\\.dvi$"  "xdvi" "dvips -o *b.ps *")
	 '("\\.dvi$" "xdvi" "dvips"))
     (if dired-use-file-transformers
	 '("\\.dvi$" "dvips -o *b.ps *")
       '("\\.dvi$" "dvips")))

   ;; The last word.  Things that cannot be grokked with a regexp.
   '("." (if (> (length files) 1)
	     "tar cvf "
	   (and (= (length files) 1) (file-directory-p
				      (expand-file-name
				       (car files)
				       (dired-current-directory)))
		(concat "tar cvf " (file-name-nondirectory
				    (directory-file-name (car files)))
			".tar"))))
   )
  "Default for variable `dired-auto-shell-command-alist' (which see).
Set this to nil to turn off shell command guessing.")

;; Might use {,} for bash or csh:
(defvar dired-shell-prefix ""
  "Prepended to marked files in dired shell commands.")
(defvar dired-shell-postfix ""
  "Appended to marked files in dired shell commands.")
(defvar dired-shell-separator " "
  "Separates marked files in dired shell commands.")

(defvar dired-file-wildcard ?*
  "Wildcard character used by dired shell commands.
Indicates where file names should be inserted.")

(defvar dired-shell-command-separators '(?\  ?| ?> ?< ?& ?;)
  "Defines the start of a string specifying a word in a shell command.")

(defvar dired-trans-map
  (list
   (cons ?f 'identity)
   (cons ?n 'file-name-nondirectory)
   (cons ?d 'file-name-directory)
   (cons ?b 'dired-file-name-base)
   (cons ?e 'dired-file-name-extension)
   (cons ?v 'dired-file-name-sans-rcs-extension)
   (cons ?z 'dired-file-name-sans-compress-extension))
  "Alist that associates keys with file transformer functions
Each transformer function should be a funcion of one argument, the file name.
The keys are characters.")

(defvar dired-shell-failure-marker ?!
  "*A marker to mark files on which shell commands fail.
If nil, such files are not marked.")

;;; Internal variables

;; Make sure this gets defined.
(defvar shell-command-history nil
  "History list of previous shell commands.")

(defvar dired-print-history nil
  "History of commands used to print files.")

(defvar dired-shell-input-start) ; only defined in shell output buffers

;;; Utility functions and Macros

(defun dired-shell-quote (filename)
  ;; Quote a file name for inferior shell (see variable shell-file-name).
  ;; Quote everything except POSIX filename characters.
  ;; This should be safe enough even for really wierd shells.
  (let ((result "") (start 0) end)
    (while (string-match "[^---0-9a-zA-Z_./]" filename start)
      (setq end (match-beginning 0)
	    result (concat result (substring filename start end)
			   "\\" (substring filename end (1+ end)))
	    start (1+ end)))
    (concat result (substring filename start))))

(defun dired-uniquefy-list (list)
  ;; Returns list, after removing 2nd and higher occurrences
  ;; of all elements. Tests elements with equal. Retains the relative
  ;; order of the elements.
  ;; For small lists, this way is probably faster than sorting.
  (let (result)
    (while list
      (or (member (car list) result)
	  (setq result (nconc result (list (car list)))))
      (setq list (cdr list)))
    result))

(defun dired-read-shell-command (prompt arg files)
  ;; Read a dired shell command prompting with PROMPT (using read-string).
  ;; ARG is the prefix arg and may be used to indicate in the prompt which
  ;;  files are affected.
  (dired-mark-pop-up
   nil 'shell files
   (function
    (lambda (prompt files)
      (let* ((default (car shell-command-history))
	     (guesses (dired-guess-default files))
	     (len (length guesses))
	     cmd)
	(or (zerop len)
	    (setq prompt (format "%s{%d guess%s} "
				 prompt len (if (= len 1) "" "es"))))
	(if default (setq prompt (concat prompt "[" default "] ")))
	(put 'guesses 'no-default t) ; for gmhist, in case.
	(setq guesses (nconc guesses (copy-sequence shell-command-history))
	      cmd (dired-read-with-history prompt nil 'guesses))
	(if (string-match "^[ \t\n]*$" cmd)
	    (if default
		(setq cmd default)
	      (error "No shell command given.")))
	(setq shell-command-history
	      (dired-uniquefy-list
	       (cons cmd shell-command-history)))
	cmd)))
   (format prompt (dired-mark-prompt arg files)) files))

(defmacro dired-trans-subst (transformers filename dir)
;; Applies each transformer supplied in the string TRANSFORMERS in sequence
;; to FILE and returns the concatenation of the results. Also unquotes \\'s.
;; Returns a string if no file transformations were done, otherwise a list
;; consisting of a single string.
  (` (let* ((transformers (, transformers))
	    (filename (, filename))
	    (len (length transformers))
	    (pos 0)
	    (last 0)
	    (transformed nil)
	    (quoted nil)
	    char result trans)
       (while (< pos len)
	 (setq char (aref transformers pos))
	 (cond
	  (quoted (setq pos (1+ pos)
			quoted nil))
	  ((= ?\\ char)
	   (setq quoted t
		 result (concat result (substring transformers last pos))
		 pos (1+ pos)
		 last pos))
	  ((and (null quoted) (= char dired-file-wildcard))
	   (setq pos (1+ pos)
		 trans (and (< pos len)
			    dired-use-file-transformers
			    (assq (aref transformers pos)
				  dired-trans-map))
		 transformed t)
	   (if trans
	       (setq result (concat result
				    (substring transformers last (1- pos))
				    (funcall (cdr trans) filename))
		     pos (1+ pos)
		     last pos)
	     (setq result (concat result (substring transformers last (1- pos))
				  (dired-make-relative filename (, dir) t))
		   last pos)))
	  ((setq pos (1+ pos)))))
       (if result
	   (progn
	     (setq result (dired-shell-quote
			   (concat result (substring transformers last))))
	     (if transformed (list result) result))
	 transformers))))

(defun dired-trans-filenames (transformers files dir)
  ;; Applies a transformer string to a list of filenames,
  ;; concatenating them into a string. The result will be prefixed
  ;; by dired-shell-prefix, the filenames separated by dired-shell-separator,
  ;; and postfixed by dired-shell-postfix.
  ;; Returns a list if filename subst. was done. A string otherwise.
  (let ((list files)
	(res nil)
	trans)
    (while list
      (setq trans (dired-trans-subst transformers (car list) dir))
      (if (listp trans)
	  (setq res (nconc res trans)
		list (cdr list))
	(setq res trans
	      list nil)))
    (if (listp res)
	(list
	 (if (> (length files) 1)
	     (concat dired-shell-prefix
		     (mapconcat 'identity res dired-shell-separator)
		     dired-shell-postfix)
	   (car res)))
      res)))

(defun dired-trans-command (command files dir)
  ;; Do all of the trans substitutions in COMMAND for the list
  ;; of files FILES. FILES must be a list of *absolute* pathnames.
  ;; DIR is an absolute directory wrto which filenames may be relativized.
  (let ((len (length command))
	(start 0)
	(pos 0)
	(last 0)
	result char transed transform)
    (while (< pos len)
      ;; read over word separators.
      (while (and (< pos len) (memq (aref command pos)
				 dired-shell-command-separators))
	(setq pos (1+ pos)))
      (setq start pos)
      ;; read a word
      (while (and (< pos len) (not (memq (setq char (aref command pos))
					 dired-shell-command-separators)))
	(setq pos (1+ pos))
	;; look out for quoted separators
	(and (= ?\\ char) (< pos len) (or (memq (setq char (aref command pos))
						dired-shell-command-separators)
					  (= ?\\ char))
	     (setq pos (1+ pos))))
      (setq transform (if (= start pos)
			  ""
			(dired-trans-filenames (substring command start pos)
					       files dir))
	    ;; remember if we did any transforming
	    transed (or transed (listp transform))
	    result (concat result
			   (substring command last start)
			   (if (listp transform)
			       (car transform)
			     transform))
	    last pos))
    (if transed
	;; just return result
	result
      ;; add the filenames at the end.
      (let ((fns (if (> (length files) 1)
		     (concat dired-shell-prefix
			     (mapconcat
			      (function
			       (lambda (fn)
				 (dired-shell-quote
				  (dired-make-relative fn dir t))))
			      files dired-shell-separator)
			     dired-shell-postfix)
		   (dired-shell-quote
		    (dired-make-relative (car files) dir t)))))
	(concat result " " fns)))))

(defun dired-shell-stuff-it (command file-list dir on-each)
  ;; Make up a shell command line from COMMAND and FILE-LIST.
  ;; If ON-EACH is t, COMMAND should be applied to each file, else
  ;; simply concat all files and apply COMMAND to this.
  ;; If ON-EACH is 'dir, the command is run in the directory of each file
  ;; In this case FILE-LIST must be a list of full paths.
  ;; FILE-LIST's elements will be quoted for the shell.
  (cond
   ((eq on-each 'dir)
    (let ((subshell-dir nil)
	  (list file-list)
	  (result nil))
      (while list
	(let ((cmd (dired-trans-command command (list (car list))
					(file-name-directory (car list))))
	      (fdir (dired-shell-quote (file-name-directory (car list)))))
	  (setq result
		(apply 'concat
		       result
		       (if subshell-dir
			   (if (string-equal dir subshell-dir)
			       (list "\; " cmd)
			     (if (string-equal dir fdir)
				 (progn
				   (setq subshell-dir nil)
				   (list "\)\; " cmd))
			       (setq subshell-dir fdir)
			       (list "\)\; \(cd "
				     fdir
				     "\; "
				     cmd)))
			 (if (string-equal fdir dir)
			     (list (and result "\; ")
				   cmd)
			   (setq subshell-dir fdir)
			   (list (and result "\; ")
				 "\(cd "
				 fdir
				 "\; "
				 cmd)))))
	  (setq list (cdr list))))
      (concat result (and subshell-dir ")"))))
   (on-each
    (mapconcat (function
		(lambda (fn)
		  (dired-trans-command command (list fn) dir)))
	       file-list "; "))
   
   (t (dired-trans-command command file-list dir))))

(defun dired-guess-default (files)
  ;; Guess a list of possible shell commands for FILES.
  (and dired-default-auto-shell-command-alist
       files
       (let ((alist (append dired-auto-shell-command-alist
			    dired-default-auto-shell-command-alist))
	     guesses)
	 (while alist
	   (let* ((elt (car alist))
		  (regexp (car elt)))
	     (setq guesses
		   (nconc guesses
			  (catch 'missed
			    (mapcar (function
				     (lambda (file)
				       (or (string-match regexp file)
					   (throw 'missed nil))))
				    files)
			    (delq nil (mapcar 'eval (cdr elt)))))))
	   (setq alist (cdr alist)))
	 (dired-uniquefy-list guesses))))

(defun dired-shell-unhandle-file-name (filename)
  "Turn a file name into a form that can be sent to a shell process.
This is particularly usefull if we are sending file names to a remote shell."
  (let ((handler (find-file-name-handler filename 'dired-shell-unhandle-file-name)))
    (if handler
	(funcall handler 'dired-shell-unhandle-file-name filename)
      filename)))

;;; Actually running the shell command

(defun dired-run-shell-command-closeout (buffer &optional message)
  ;; Report on the number of lines produced by a shell command.
  (if (get-buffer buffer)
      (save-excursion
	(set-buffer buffer)
	(if (zerop (buffer-size))
	    (progn
	      (if message
		  (message "Shell command completed with no output. %s"
			     message)
		(message "Shell command completed with no output."))
	      (kill-buffer buffer))
	  (set-window-start (display-buffer buffer) 1)
	  (if message
	      (message "Shell command completed. %s" message)
	    (message "Shell command completed."))))))

(defun dired-rsc-filter (proc string)
  ;; Do save-excursion by hand so that we can leave point
  ;; numerically unchanged despite an insertion immediately
  ;; after it.
  (let* ((obuf (current-buffer))
	 (buffer (process-buffer proc))
	 opoint
	 (window (get-buffer-window buffer))
	 (pos (window-start window)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (setq opoint (point))
	  (goto-char (point-max))
	  (insert-before-markers string))
      ;; insert-before-markers moved this marker: set it back.
      (set-window-start window pos)
      ;; Finish our save-excursion.
      (goto-char opoint)
      (set-buffer obuf))))

(defun dired-rsc-sentinel (process signal)
  ;; Sentinel function used by dired-run-shell-command
  (if (memq (process-status process) '(exit signal))
      (let ((buffer (get-buffer (process-buffer process))))
	(if buffer
	    (save-excursion
	      (set-buffer buffer)
	      (if (zerop (buffer-size))
		  (message
		   "Dired & shell command completed with no output.")
		(let ((lines (count-lines dired-shell-input-start
					  (point-max))))
		  (message
		   "Dired & shell command completed with %d line%s of output."
                   lines (dired-plural-s lines))))
	      (setq mode-line-process nil)))
	(delete-process process))))

(defun dired-shell-call-process (command dir &optional in-background)
  ;; Call a shell command as a process in the current buffer.
  ;; The process should try to run in DIR.  DIR is also
  ;; used to lookup a file-name-handler.
  ;; Must return the process object if IN-BACKGROUND is non-nil,
  ;; otherwise the process exit status.
  (let ((handler (find-file-name-handler dir 'dired-shell-call-process)))
    (if handler
	(funcall handler 'dired-shell-call-process command dir in-background)
      (let ((process-connection-type ; don't waste pty's
	     (null (null in-background))))
	(setq default-directory dir)
	(if in-background
	    (progn
	      (setq mode-line-process '(": %s"))
	      (start-process "Shell" (current-buffer)
			     shell-file-name "-c" command))
	  (call-process shell-file-name nil t nil "-c" command))))))

(defun dired-run-shell-command (command dir in-background &optional append)
  ;; COMMAND is shell command
  ;; DIR is directory in which to do the shell command.
  ;; If IN-BACKGROUND is non-nil, the shell command is run in the background.
  ;;   If it is a string, this is written as header into the output buffer
  ;;   before the command is run.
  ;; If APPEND is non-nil, the results are appended to the contents
  ;;   of *shell-command* buffer, without erasing its previous contents.
  (save-excursion
    (if in-background
	(let* ((buffer (get-buffer-create
		       "*Background Shell Command Output*"))
	       (n 2)
	       proc)
	  ;; No reason why we can't run two+ background commands.
	  (while (get-buffer-process buffer)
	    (setq buffer (get-buffer-create
			  (concat "*Background Shell Command Output*<"
				  (int-to-string n) ">"))
		  n (1+ n)))
	  (set-buffer buffer)
	  (or (eq major-mode 'comint-mode)
	      (progn
		(comint-mode)
		(set (make-local-variable 'comint-prompt-regexp)
		     "^[^\n]*\\? *")))
	  (display-buffer buffer)
	  (barf-if-buffer-read-only)
	  ;; If will kill a process, query first.

	  (set (make-local-variable 'dired-shell-input-start) (point-min))
	  (if append
	      (progn
		(goto-char (point-max))
		(or (= (preceding-char) ?\n) (bobp) (insert "\n")))
	    (erase-buffer)
	    (if (stringp in-background)
		(progn
		  (insert in-background)
		  (set (make-local-variable 'dired-shell-input-start)
		       (point)))))
	  (setq proc (dired-shell-call-process command dir t))
	  (set-marker (process-mark proc) (point))
	  (set-process-sentinel proc 'dired-rsc-sentinel)
	  (set-process-filter proc 'dired-rsc-filter)
	  nil) ; return
      (let ((buffer (get-buffer-create "*Shell Command Output*")))
	(set-buffer buffer)
	(barf-if-buffer-read-only)
	(set (make-local-variable 'dired-shell-input-start) (point-min))
	(if append
	    (progn
	      (goto-char (point-max))
	      (or (= (preceding-char) ?\n) (bobp) (insert "\n")))
	  (erase-buffer))
	(dired-shell-call-process command dir)))))

;;; User commands

(defun dired-do-shell-command (command arg files &optional in-background)
  ;; ARG = (16) means operate on each file, in its own directory.
  ;; ARG = (4) means operate on each file, but in the current
  ;;       default-directory.
  "Run a shell command COMMAND on the marked files.
If no files are marked or a non-zero numeric prefix arg is given,
the next ARG files are used.  Use prefix 1 to indicate the current file.

Normally the shell command is executed in the current dired subdirectory.
This is the directory in the dired buffer which currently contains the point.
One shell command is run for all of the files.
e.g. cmd file1 file2 file3 ... 
If the total length of of the command exceeds 10000 characters, the files will
be bunched to forms commands shorter than this length, and successive commands
will be sent.

With a prefix of \\[universal-argument], a separate command for each file will
be executed.

With a prefix of \\[universal-argument] \\[universal-argument], a separate command will be sent for each file,
and the command will be executed in the directory of that file.  The explicit
command will be of the form 

                      cd dir; cmd file

When prompting for the shell command, dired will always indicate the directory
in which the command will be executed.

The following documentation depends on the settings of `dired-file-wildcard',
`dired-shell-command-separators', `dired-trans-map', `dired-shell-prefix',
`dired-shell-separator', and `dired-shell-postfix'. See the documentation for
these variables. Below, I will assume default settings for these variables.

If the shell command contains a *, then the list of files is substituted for *.
The filenames will be written as relative to the directory in which the shell
command is executing. If there is no *, and the command does not end in &, 
then the files are appended to the end of the command. If the command ends in
a &, then the files are inserted before the &.

If `dired-use-file-transformers' is non-nil, then certain 2-character
sequences represent parts of the file name.
The default transformers are:
*f = full file name
*n = file name without directory
*d = file name's directory 
     This will end in a \"/\" in unix.
*e = file names extension
     By default this the part of the file name without directory, which
     proceeds the first \".\". If \".\" is the first character of the name,
     then this \".\" is ignored. The definition of extension can
     be customized with `dired-filename-re-ext'.
*b = file base name
     This is the part of the file name without directory that precedes
     the extension.
*v = file name with out version control extension (i.e. \",v\")
*z = file name without compression extension
     (i.e. \".Z\", \".z\", or \".gz\")

Shell commands are divided into words separated by spaces. Then for each
word the file name transformers are applied to the list of files, the result
concatenated together and substituted for the word in the shell command.

For example
   cmd -a *f -b *d*b.fizzle applied to /foo/bar and /la/di/da results in
   cmd -a /foo/bar /la/di/da -b /foo/bar.fizzle /la/di/da.fizzle

The \"on-each\" prefixes \\[universal-argument] and 0, also apply while
using file transformers. As well, when using file-transformers * still
represents the file name relative to the current directory. Not that this
differs from *f, which always represents the full pathname.

A \"\\\" can always be used to quote any character having special meaning.
For example, if the current directory is /la, then *n applied
to /la/di/da returns la, whereas *\\n returns di/dan. Similarly,
\"*d\\ *n\" returns \"/la/di da\".

The prefix character for file name transformers is always the same as
`dired-file-wildcard'."

  (interactive
   (let ((on-each (or (equal '(4) current-prefix-arg)
		      (equal '(16) current-prefix-arg)))
	 (files (dired-get-marked-files
		 nil (and (not (consp current-prefix-arg))
			  current-prefix-arg)))
	 (dir (and (not (equal current-prefix-arg '(16)))
		   (dired-current-directory))))
     (list
      (dired-read-shell-command
       (concat (if dir
		   (format "! in %s" (dired-abbreviate-file-name dir))
		 "cd <dir>; ! ")
	       "on "
	       (if on-each "each ")
	       "%s: ")
       (and (not on-each) current-prefix-arg)
       (if dir
	   (mapcar (function
		    (lambda (fn)
		      (dired-make-relative fn dir t)))
		   files)
	 files))
      current-prefix-arg files nil)))

  ;; Check for background commands
  (if (string-match "[ \t]*&[ \t]*$" command)
      (setq command (substring command 0 (match-beginning 0))
	    in-background t))

  ;; Look out for remote file names.
  
  (let* ((on-each (or (equal arg '(4)) (and (equal arg '(16)) 'dir)))
	 (ufiles (mapcar 'dired-shell-unhandle-file-name files))
	 (dir (dired-current-directory))
	 (udir (dired-shell-unhandle-file-name dir)))

    (save-excursion ; in case `shell-command' changes buffer
      (cond

       ((null ufiles)
	;; Just run as a command on no files.
	(if in-background
	    (dired-run-shell-command command dir t)
	  (dired-run-shell-command command dir nil)
	  (dired-run-shell-command-closeout "*Shell Command Output*")))
	
       (in-background
	;; Can't use dired-bunch-files for background shell commands.
	;; as we will create a bunch of process running simultaneously.
	;; A better solution needs to be found.
	(dired-run-shell-command
	 (dired-shell-stuff-it command ufiles udir on-each)
	 dir (if (equal arg '(16))
		 (concat "cd <dir>; \"" command "\"\n\n")
	       (concat "\"" command "\" in " dir "\n\n"))))
       (on-each
	(let ((buff (get-buffer "*Shell Command Output*"))
	      failures this-command this-dir ufile return message)
	  (if buff
	      (save-excursion
		(set-buffer buff)
		(erase-buffer)))
	  (while ufiles
	    (setq ufile (car ufiles))
	    (if (eq on-each 'dir)
		(setq this-dir (dired-shell-quote (file-name-directory (directory-file-name ufile)))
		      this-command (concat "cd " this-dir "; " command))
	      (setq this-command command)
	      (or this-dir (setq this-dir udir)))
	    (setq return
		  (dired-run-shell-command
		   (dired-shell-stuff-it this-command (list ufile) this-dir nil)
		   this-dir nil t))
	    (if (and (integerp return) (/= return 0))
		(save-excursion
		  (let ((file (nth (- (length files) (length (member ufile ufiles))) files)))
		    (if (and dired-shell-failure-marker
			     (dired-goto-file file))
			(let ((dired-marker-char dired-shell-failure-marker))
			  (dired-mark 1)))
		    (setq failures (cons file failures)))))
	    (setq ufiles (cdr ufiles)))
	  (if failures
	      (let ((num (length failures)))
		(setq message
		      (if dired-shell-failure-marker
			  (format
			   "Marked %d failure%s with %c."
			   num (dired-plural-s num)
			   dired-shell-failure-marker)
			"Failed on %d file%s." num
			(dired-plural-s num)))
		(dired-log
		 (current-buffer)
		 "Shell command %s failed (non-zero exit status) for:\n  %s"
		 command failures)
		(dired-log (current-buffer) t)))
	  (dired-run-shell-command-closeout "*Shell Command Output*" message)))
	
       (t
	(dired-bunch-files
	 (- 10000 (length command))
	 (function (lambda (&rest ufiles)
		     (dired-run-shell-command
		      (dired-shell-stuff-it command ufiles udir nil)
		      dir nil)
		     nil)) ; for the sake of nconc in dired-bunch-files
	 nil ufiles)
	(dired-run-shell-command-closeout "*Shell Command Output*"))))
    ;; Update any directories
    (or in-background
	(let ((dired-no-confirm '(revert-subdirs)))
	  (dired-verify-modtimes)))))

(defun dired-do-background-shell-command (command arg files)
  "Like \\[dired-do-shell-command], but starts command in background.
Note that you can type input to the command in its buffer.
This requires background.el from the comint package to work."
  ;; With the version in emacs-19.el, you can alternatively just
  ;; append an `&' to any shell command to make it run in the
  ;; background, but you can't type input to it.
  (interactive
   (let ((on-each (or (equal '(4) current-prefix-arg)
		      (equal '(16) current-prefix-arg)))
	 (files (dired-get-marked-files
		 nil (and (not (consp current-prefix-arg))
			  current-prefix-arg)))
	 (dir (and (not (equal current-prefix-arg '(16)))
		   (dired-current-directory))))
     (list
      (dired-read-shell-command
       (concat "& "
	       (if dir
		   (format "in %s " (dired-abbreviate-file-name dir))
		 "cd <dir>; ")
	       "on "
	       (if on-each "each ")
	       "%s: ")
       (and (not on-each) current-prefix-arg)
       (if dir
	   (mapcar (function
		    (lambda (fn)
		      (dired-make-relative fn dir t)))
		   files)
	 files))
      current-prefix-arg files)))
  (dired-do-shell-command command arg files t))

;;; Printing files

(defun dired-do-print (&optional arg command files)
  "Print the marked (or next ARG) files.
Uses the shell command coming from variable `dired-print-program-alist'."
  (interactive
   (progn
     (if dired-print-history
	 (setq dired-print-history (dired-uniquefy-list dired-print-history))
       (setq dired-print-history (mapcar 'cdr dired-print-program-alist)))
     (let* ((files (dired-get-marked-files nil current-prefix-arg))
	    (rel-files (mapcar (function
				(lambda (fn)
				  (dired-make-relative
				   fn
				   (dired-current-directory) t)))
			       files))
	    (alist dired-print-program-alist)
	    (first (car files))
	    (dired-print-history (copy-sequence dired-print-history))
	    elt initial command)
       ;; For gmhist
       (put 'dired-print-history 'no-default t)
       (if first
	   (while (and alist (not initial))
	     (if (string-match (car (car alist)) first)
		 (setq initial (cdr (car alist)))
	       (setq alist (cdr alist)))))
       (if (and initial (setq elt (member initial dired-print-history)))
	   (setq dired-print-history (nconc
				      (delq (car elt) dired-print-history)
				      (list initial))))
       (setq command
	     (dired-mark-read-string
	      "Print %s with: "
	      initial 'print current-prefix-arg rel-files
	      'dired-print-history))
       (list current-prefix-arg command files))))
  (or files
      (setq files (dired-get-marked-files nil arg)))
  (while files
    (dired-print-file command (car files))
    (setq files (cdr files))))

(defun dired-print-file (command file)
  ;; Using COMMAND, print FILE.
  (let ((handler (find-file-name-handler file 'dired-print-file)))
    (if handler
	(funcall handler 'dired-print-file command file)
      (let ((rel-file (dired-make-relative file (dired-current-directory) t)))
	(message "Spooling %s..." rel-file)
	(shell-command (dired-trans-command command (list file) ""))
	(message "Spooling %s...done" rel-file)))))

;;; end of dired-shell.el
