;; Convert tit format dictionary (of cxterm) to quail-package.
;; Usage (within mule):
;;	M-x tit-to-quail<CR>tit-file-name<CR>
;; Usage (from shell):
;;	% mule -batch -l quail/tit -f batch-tit-to-quail [dirname|filename] ...
;;   When you run tit-to-quail within Mule, you are shown lines for setting
;;   key bindings.  You can modify them as you wish.  After that, save the
;;   buffer into somewhere under your `load-path'.
;;   You may also modify the second arg PROMPT of quail-define-pacakge
;;   to shorter string.
;; As for Big5 file, Big5-HKU is not supported, you must
;; at first convert Big5-HKU to Big5-ETen before you run `tit-to-quail'.

(defun tit-value-head ()
  (skip-chars-forward "^:")
  (forward-char 1)
  (skip-chars-forward " \t"))

(defun tit-set-keys ()
  (let (limit str)
    (save-excursion
      (end-of-line)
      (setq limit (point)))
    (if (re-search-forward "[^ \t\n]+" limit t)
	(progn
	  (setq str
		(concat "\""
			(buffer-substring (match-beginning 0) (match-end 0))
			"\""))
	  (car (read-from-string str))))))

(defun tit-insert (arg)
  (let ((pos (point)))
    (insert arg)
    (if (or (string-match "\"" arg) (string-match "\\\\" arg))
	(save-excursion
	  (while (re-search-backward "\"\\|\\\\" pos t)
	    (insert "\\")
	    (forward-char -1))))))

(defun tit-buffer-substring (key)
  (let ((i 0) ch)
    (while (and (/= (setq ch (following-char)) ? )
		(/= ch ?\t)
		(/= ch ?\n))
      (aset key i ch)
      (setq i (1+ i))
      (forward-char 1))
    (aset key i 0)))

(defun tit-looking-at (key)
  (let ((pos (point)) (i 0) ch)
    (while (eq (char-after (+ pos i)) (aref key i))
      (setq i (1+ i)))
    (and (integerp (setq ch (char-after (+ pos i))))
	 (or (= ch ? ) (= ch ?\t))
	 (= (aref key i) 0))))

(defun tit-message (&rest args)
  (if (null noninteractive)
      (apply 'message args)))

;; make quail-package name (e.g. ZOZY.tit -> .../quail/zozy.el)
(defun tit-dest-file (file &optional dir)
  (expand-file-name
   (concat (if dir "" "quail/")
	   (downcase (file-name-nondirectory (substring file 0 -4)))
	   ".el")
   (or dir (car load-path))))

(defun tit-to-quail (tit-file &optional dest-dir)
  (interactive "Ftit file: ")
  (let ((buf (get-buffer-create "*tit-work*"))
	pos
	;; tit keywords and default values
	(encode '*euc-china*)
	(multichoice t)
	prompt
	comment
	validinputkey
	(selectkey '["1 " "2" "3" "4" "5" "6" "7" "8" "9" "0"
		     nil nil nil nil nil nil])
	(selectkey-idx 0)
	(backspace "\177")
	(deleteall "\C-u")
	(moveright ".>")
	(moveleft ",<")
	keyprompt
	phrase)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (if (null (string-match "\\.tit$" tit-file))
	  (setq tit-file (concat tit-file ".tit")))
      (let ((file-coding-system-for-read '*noconv*))
	(insert-file-contents tit-file))

      (set-visited-file-name (tit-dest-file tit-file dest-dir))
      (set-file-coding-system '*junet*unix)

      ;; convert GB or BIG5 to Mule's internal code
      (save-excursion
	(if (re-search-forward "^ENCODE:" nil t)
	    (progn
	      (skip-chars-forward " \t")
	      (if (looking-at "GB")
		  (setq encode '*euc-china*)
		(setq encode '*big5*)))))
      (tit-message "Converting %s to Mule's internal code..." encode)
      (code-convert 1 (point-max) encode '*internal*)

      (goto-char 1)
      ;; setting headers
      (insert "(require 'quail)\n")

      (tit-message "Extracting header information...")
      (while (null (looking-at "BEGINDICTIONARY\\|BEGINPHRASE"))
	(insert ";; ")
	(let ((ch (following-char)))
	  (cond ((= ch ?C)		; COMMENT
		 (forward-word 1)
		 (setq pos (point))
		 (end-of-line)
		 (setq comment (cons (buffer-substring pos (point)) comment)))
		((= ch ?M)		; MULTICHOICE, MOVERIGHT, MOVELEFT
		 (cond ((looking-at "MULTI")
			(tit-value-head)
			(setq multichoice (looking-at "YES")))
		       ((looking-at "MOVERIGHT")
			(tit-value-head)
			(setq moveright (tit-set-keys)))
		       ((looking-at "MOVELEFT")
			(tit-value-head)
			(setq moveleft (tit-set-keys)))))
		((= ch ?P)		; PROMPT
		 (tit-value-head)
		 (setq pos (point))
		 (end-of-line)
		 (setq comment (cons (buffer-substring pos (point)) comment))
		 (setq pos (point))
		 (skip-chars-backward "^ \t")
		 (setq prompt (buffer-substring (point) pos)))
		((= ch ?S)		; SELECTKEY
		 (tit-value-head)
		 (let (key)
		   (while (and (< selectkey-idx 16) (setq key (tit-set-keys)))
		     (aset selectkey selectkey-idx key)
		     (setq selectkey-idx (1+ selectkey-idx)))))
		((= ch ?B)		; BACKSPACE
		 (tit-value-head)
		 (setq backspace (tit-set-keys)))
		((= ch ?K)		; KEYPROMPT
		 (forward-word 1)
		 (forward-char 1)
		 (setq pos (point))
		 (let (key str)
		   (search-forward ")" nil t)
		   (setq key (buffer-substring pos (1- (point))))
		   (skip-chars-forward ": \t")
		   (setq pos (point))
		   (forward-char 1)
		   (setq str (buffer-substring pos (point)))
		   (setq keyprompt (cons (cons key str) keyprompt))))
		))
	(forward-line 1))

      (setq phrase (looking-at "BEGINPHRASE"))
      ;; comment out the line BEGINDICTIONARY/BEGINPHRASE
      (insert ";; ")
      (forward-line 1)

      (tit-message "Defining quail-package...")
      (insert "(quail-define-package \""
	      (substring (file-name-nondirectory buffer-file-name) 0 -3)
	      "\" \"")
      (if (string-match "[:$A!K$(0!(!J(B]\\(.*\\)[:$A!K$(0!(!K(B]" prompt)
	  (tit-insert (substring prompt (match-beginning 1) (match-end 1)))
	(tit-insert prompt))
      (insert "\"\n")
      (if multichoice
	  (insert " t\n")
	(if keyprompt
	    (let (key)
	      (insert " '(")
	      (while keyprompt
		(setq key (car keyprompt))
		(insert "(?")
		(tit-insert (car key))
		(insert " . \"")
		(tit-insert (cdr key))
		(insert "\")\n   ")
		(setq keyprompt (cdr keyprompt)))
	      (setq pos (point))
	      (forward-char -4)
	      (delete-region (point) pos)
	      (insert ")\n"))
	  (insert " nil\n")))
      (if comment
	  (progn
	    (insert " \"")
	    (setq comment (nreverse comment))
	    (while comment
	      (tit-insert (car comment))
	      (insert "\n")
	      (setq comment (cdr comment)))
	    (forward-char -1)
	    (insert "\"")
	    (forward-char 1))
	(insert " \"\"\n"))
      (let (i len key)
	(insert " '(\n")
	(setq i 0 len (length moveright))
	(while (< i len)
	  (setq key (aref moveright i))
	  (if (and (>= key ? ) (< key 127))
	      (progn
		(insert "  (\"")
		(tit-insert (char-to-string (aref moveright i)))
		(insert "\" . quail-next-candidate-block)\n")))
	  (setq i (1+ i)))
	(setq i 0 len (length moveleft))
	(while (< i len)
	  (setq key (aref moveleft i))
	  (if (and (>= key ? ) (< key 127))
	      (progn
		(insert "  (\"")
		(tit-insert (char-to-string (aref moveleft i)))
		(insert "\" . quail-prev-candidate-block)\n")))
	  (setq i (1+ i)))
	(if (string-match " " (aref selectkey 0))
	    (insert "  (\" \" . quail-select-current)\n"))
	(insert "  )\n"))
      (insert " nil" (if multichoice " nil" " t") ")\n\n")

      (tit-message "Formatting translation rules...")
      (let ((mc-flag nil)
	    (key (make-string 30 0)))
	(while (null (eobp))
	  (if (or (= (following-char) ?#) (= (following-char) ?\n))
	      (progn
		(insert ";; ")
		(forward-line 1))
	    (insert (if phrase "(qd \"" "(qdv \""))
	    (setq pos (point))
	    (tit-buffer-substring key)
	    (save-excursion
	      (while (re-search-backward "\"\\|\\\\[^0-9]" pos t)
		(insert "\\")
		(forward-char -1)))
	    (insert "\"")
	    (skip-chars-forward " \t")
	    (if (eolp)
		(progn
		  (beginning-of-line)
		  (setq pos (point))
		  (forward-line 1)
		  (delete-region pos (point)))
	      (insert (if phrase "'(" "\""))
	      (setq pos (point))
	      (forward-line 1)
	      (while (tit-looking-at key)
		(let (p)
		  (skip-chars-backward " \t\n")
		  (if phrase (insert " "))
		  (setq p (point))
		  (skip-chars-forward " \t\n")
		  (skip-chars-forward "^ \t")
		  (skip-chars-forward " \t")
		  (delete-region p (point)))
		(forward-line 1))
	      (goto-char pos)
	      (if phrase
		  (while (null (eolp))
		    (insert "\"")
		    (skip-chars-forward "^ \t\n")
		    (insert "\"")
		    (skip-chars-forward " \t"))
		(let ((mc-flag t)) (forward-char 1))
		(if (= (following-char) ?\n)
		    (progn
		      (beginning-of-line)
		      (forward-char 3)
		      (delete-char 1))))
	      (end-of-line)
	      (insert (if phrase "))" "\")"))
	      (forward-line 1)))))
      (insert "\n(quail-setup-current-package)\n")
      )

    (if noninteractive
	(save-excursion
	  (set-buffer buf)
	  (write-file buffer-file-name))
      ;; show user the line for keymap definition.
      (switch-to-buffer buf)
      (goto-char 1)
      (search-forward "defconst" nil t)
      (beginning-of-line)
      (recenter 1)
      (tit-message
       "Modify key bindings or prompt as you wish and save this file.")
      )
    ))

(defun batch-tit-to-quail ()
  "Run `tit-to-quail' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
For example, invoke \"emacs -batch -f batch-tit-to-quail $emacs/ ~/*.el\""
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-tit-to-quail' is to be used only with -batch"))
  (let ((error nil))
    (while command-line-args-left
      (if (file-directory-p (expand-file-name (car command-line-args-left)))
	  (let ((files (directory-files (car command-line-args-left)))
		file)
	    (while files
	      (setq file (expand-file-name (car files)
					   (car command-line-args-left)))
	      (if (and (string-match "\\.tit$" file)
		       (file-newer-than-file-p
			file
			(tit-dest-file file default-directory)))
		  (progn
		    (message "Converting %s to quail-package..." file)
		    (tit-to-quail file default-directory)))
	      (setq files (cdr files))))
	(tit-to-quail (car command-line-args-left) default-directory))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs 0)))
