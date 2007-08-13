;;; mule-ccl.el --- Code Conversion Language functions.

;; Copyright (C) 1992 Free Software Foundation, Inc.

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

;;; 93.5.26  created for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>

;;;; #### This stuff doesn't work yet.

(defconst ccl-operator-table
  '[if branch loop break repeat write-repeat write-read-repeat
    read read-if read-branch write end])

(let (op (i 0) (len (length ccl-operator-table)))
  (while (< i len)
    (setq op (aref ccl-operator-table i))
    (put op 'ccl-compile-function (intern (format "ccl-compile-%s" op)))
    (setq i (1+ i))))

(defconst ccl-machine-code-table
  '[set-cs set-cl set-r set-a
    jump jump-cond write-jump write-read-jump write-c-jump
    write-c-read-jump write-s-jump write-s-read-jump write-a-read-jump
    branch
    read1 read2 read-branch write1 write2 write-c write-s write-a
    end
    set-self-cs set-self-cl set-self-r set-expr-cl set-expr-r
    jump-cond-c jump-cond-r read-jump-cond-c read-jump-cond-r
    ])

(let (code (i 0) (len (length ccl-machine-code-table)))
  (while (< i len)
    (setq code (aref ccl-machine-code-table i))
    (put code 'ccl-code i)
    (put code 'ccl-dump-function (intern (format "ccl-dump-%s" code)))
    (setq i (1+ i))))

(defconst ccl-register-table '[r0 r1 r2 r3 r4 r5 r6 r7])

(let (reg (i 0) (len (length ccl-register-table)))
  (while (< i len)
    (setq reg (aref ccl-register-table i))
    (put reg 'ccl-register-number i)
    (setq i (1+ i))))

(defconst ccl-arith-table
  '[+ - * / % & | ^ << >> <8 >8 // nil nil nil < > == <= >= !=])

(let (arith (i 0) (len (length ccl-arith-table)))
  (while (< i len)
    (setq arith (aref ccl-arith-table i))
    (if arith (put arith 'ccl-arith-code i))
    (setq i (1+ i))))

(defconst ccl-self-arith-table
  '[+= -= *= /= %= &= |= ^= <<= >>= <8= >8= //=])

(let (arith (i 0) (len (length ccl-self-arith-table)))
  (while (< i len)
    (setq arith (aref ccl-self-arith-table i))
    (put arith 'ccl-self-arith-code i)
    (setq i (1+ i))))

;; this holds the compiled CCL program as it is being compiled.
(defvar ccl-program-vector nil)

;; this holds the index into ccl-program-vector where the next
;; instruction is to be stored.
(defvar ccl-current-ic 0)

;; add a constant to the compiled CCL program, either at IC (if specified)
;; or at the current instruction counter (and bumping that value)
(defun ccl-embed-const (const &optional ic)
  (if ic
      (aset ccl-program-vector ic const)
    (aset ccl-program-vector ccl-current-ic const)
    (setq ccl-current-ic (1+ ccl-current-ic))))

(defun ccl-embed-code (op reg const &optional ic)
  (let ((machine-code (logior (get op 'ccl-code)
			      (if (symbolp reg)
				  (ash (get reg 'ccl-register-number) 5)
				0)
			      (ash const 8))))
    (if ic
	(aset ccl-program-vector ic machine-code)
      (aset ccl-program-vector ccl-current-ic machine-code)
      (setq ccl-current-ic (1+ ccl-current-ic)))))

;; advance the instruction counter by INC without doing anything else
(defun ccl-embed-nop (&optional inc)
  (setq ccl-current-ic (+ ccl-current-ic (or inc 1))))

;;;###autoload
(defun ccl-program-p (obj)
  "T if OBJECT is a valid CCL compiled code."
  (and (vectorp obj)
       (let ((i 0) (len (length obj)) (flag t))
	 (if (> len 1)
	     (progn
	       (while (and flag (< i len))
		 (setq flag (integerp (aref obj i)))
		 (setq i (1+ i)))
	       flag)))))

(defvar ccl-loop-head nil)
(defvar ccl-breaks nil)

;;;###autoload
(defun ccl-compile (ccl-program)
  "Compile a CCL source program and return the compiled equivalent.
The return value will be a vector of integers."
  (if (or (null (consp ccl-program))
	  (null (listp (car ccl-program))))
      (error "CCL: Invalid source program: %s" ccl-program))
  (if (null (vectorp ccl-program-vector))
      (setq ccl-program-vector (make-vector 8192 0))
    ;; perhaps not necessary but guarantees some sort of determinism
    (fillarray ccl-program-vector 0))
  (setq ccl-loop-head nil ccl-breaks nil)
  (setq ccl-current-ic 0)
  ;; leave space for offset to EOL program
  (ccl-embed-nop)
  (ccl-compile-1 (car ccl-program))
  ;; store offset to EOL program in first word of compiled prog
  (ccl-embed-const ccl-current-ic 0)
  (if (car (cdr ccl-program))
      (ccl-compile-1 (car (cdr ccl-program))))
  (ccl-embed-code 'end 0 0)
  (let ((vec (make-vector ccl-current-ic 0))
	(i 0))
    (while (< i ccl-current-ic)
      (aset vec i (aref ccl-program-vector i))
      (setq i (1+ i)))
    vec))

(defun ccl-check-constant (arg cmd)
  (if (>= arg 0)
      arg
    (error "CCL: Negative constant %s not allowed: %s" arg cmd)))

(defun ccl-check-register (arg cmd)
  (if (get arg 'ccl-register-number)
      arg
    (error "CCL: Invalid register %s: %s" arg cmd)))

(defun ccl-check-reg-const (arg cmd)
  (if (integer-or-char-p arg)
      (ccl-check-constant arg cmd)
    (ccl-check-register arg cmd)))

(defun ccl-check-compile-function (arg cmd)
  (or (get arg 'ccl-compile-function)
      (error "CCL: Invalid command: %s" cmd)))

;; compile a block of CCL code (see CCL_BLOCK above).
(defun ccl-compile-1 (cmd-list)
  (let (cmd)
    ;; a CCL_BLOCK is either STATEMENT or (STATEMENT [STATEMENT ...])
    ;; convert the former into the latter.
    (if (or (not (listp cmd-list))
	    (and cmd-list (symbolp (car cmd-list))))
	(setq cmd-list (list cmd-list)))
    (while cmd-list
      (setq cmd (car cmd-list))
      ;; an int-or-char is equivalent to (r0 = int-or-char)
      ;; a string is equivalent to (write string)
      ;; convert the above two into their equivalent forms.
      ;; everything else is a list.
      (cond ((integer-or-char-p cmd)
	     (ccl-compile-set (list 'r0 '= cmd)))
	    ((stringp cmd)
	     (ccl-compile-write-string (list 'write cmd)))
	    ((listp cmd)
	     (if (eq (nth 1 cmd) '=)
		 (ccl-compile-set cmd)
	       (if (and (symbolp (nth 1 cmd))
			(get (nth 1 cmd) 'ccl-self-arith-code))
		   (ccl-compile-self-set cmd)
		 (funcall (ccl-check-compile-function (car cmd) cmd) cmd))))
	    (t
	     (error "CCL: Invalid command: %s" cmd)))
      (setq cmd-list (cdr cmd-list)))))

(defun ccl-compile-set (cmd)
  (let ((rrr (ccl-check-register (car cmd) cmd))
	(right (nth 2 cmd)))
    (cond ((listp right)
	   ;; cmd == (RRR = (XXX OP YYY))
	   (ccl-compile-expression rrr right))
	  ((integer-or-char-p right)
	   (ccl-check-constant right cmd)
	   (if (< right 524288)		; (< right 2^19)
	       (ccl-embed-code 'set-cs rrr right)
	     (ccl-embed-code 'set-cl rrr 0)
	     (ccl-embed-const right)))
	  (t
	   (ccl-check-register right cmd)
	   (let ((ary (nth 3 cmd)))
	     (if (vectorp ary)
		 (let ((i 0) (len (length ary)))
		   (ccl-embed-code 'set-a rrr (get right 'ccl-register-number))
		   (ccl-embed-const len)
		   (while (< i len)
		     (ccl-check-constant (aref ary i) cmd)
		     (ccl-embed-const (aref ary i))
		     (setq i (1+ i))))
	       (ccl-embed-code 'set-r rrr right)))))))

(defun ccl-compile-self-set (cmd)
  (let ((rrr (ccl-check-register (car cmd) cmd))
	(right (nth 2 cmd)))
    (if (listp right)
	;; cmd == (RRR SELF-OP= (XXX OP YYY))
	(progn
	  (ccl-compile-expression 'r7 right)
	  (setq right 'r7)))
    (ccl-compile-expression
     rrr
     (list rrr (intern (substring (symbol-name (nth 1 cmd)) 0 -1)) right))))

(defun ccl-compile-expression (rrr expr)
  (let ((left (car expr))
	(right (nth 2 expr)))
    (if (listp left)
	(progn
	  (ccl-compile-expression 'r7 left)
	  (setq left 'r7)))
    (if (eq rrr left)
	(if (integer-or-char-p right)
	    (if (< right 32768)
		(ccl-embed-code 'set-self-cs rrr right)
	      (ccl-embed-code 'set-self-cl rrr 0)
	      (ccl-embed-const right))
	  (ccl-check-register right expr)
	  (ccl-embed-code 'set-self-r rrr (get right 'ccl-register-number)))
      (if (integer-or-char-p right)
	  (progn
	    (ccl-embed-code 'set-expr-cl rrr (get left 'ccl-register-number))
	    (ccl-embed-const right))
	(ccl-check-register right expr)
	(ccl-embed-code 'set-expr-r rrr (get left 'ccl-register-number))
	(ccl-embed-const (get right 'ccl-register-number))))
    (ccl-embed-const (get (nth 1 expr) 'ccl-arith-code))))

(defun ccl-compile-write-string (cmd)
  (if (/= (length cmd) 2)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let* ((str (nth 1 cmd))
	 (len (length str))
	 (i 0))
    (ccl-embed-code 'write-s 0 0)
    (ccl-embed-const len)
    (while (< i len)
      (ccl-embed-const (aref str i))
      (setq i (1+ i)))))

(defun ccl-compile-if (cmd)
  (if (and (/= (length cmd) 3) (/= (length cmd) 4))
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let ((condition (nth 1 cmd))
	(true-cmds (nth 2 cmd))
	(false-cmds (nth 3 cmd))
	ic0 ic1 ic2)
    (if (listp condition)
	;; cmd == (if (XXX OP YYY) ...)
	(if (listp (car condition))
	    ;; cmd == (if ((xxx op yyy) OP YYY) ...)
	    (progn
	      (ccl-compile-expression 'r7 (car condition))
	      (setq condition (cons 'r7 (cdr condition)))
	      (setq cmd (cons (car cmd)
			      (cons condition
				    (cdr (cdr cmd))))))))
    (setq ic0 ccl-current-ic)
    (ccl-embed-nop (if (listp condition) 3 1))
    (ccl-compile-1 true-cmds)
    (if (null false-cmds)
	(setq ic1 ccl-current-ic)
      (setq ic2 ccl-current-ic)
      (ccl-embed-const 0)
      (setq ic1 ccl-current-ic)
      (ccl-compile-1 false-cmds)
      (ccl-embed-code 'jump 0 ccl-current-ic ic2))
    (if (symbolp condition)
	(ccl-embed-code 'jump-cond condition ic1 ic0)
      (let ((arg (nth 2 condition)))
	(if (integer-or-char-p arg)
	    (progn
	      (ccl-embed-code 'jump-cond-c (car condition) ic1 ic0)
	      (ccl-embed-const arg (1+ ic0)))
	  (ccl-check-register arg cmd)
	  (ccl-embed-code 'jump-cond-r (car condition) ic1 ic0)
	  (ccl-embed-const (get arg 'ccl-register-number) (1+ ic0)))
	(ccl-embed-const (get (nth 1 condition) 'ccl-arith-code) (+ ic0 2))))))

(defun ccl-compile-branch (cmd)
  (if (< (length cmd) 3)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (if (listp (nth 1 cmd))
      (progn
	(ccl-compile-expression 'r7 (nth 1 cmd))
	(setq cmd (cons (car cmd)
			(cons 'r7 (cdr (cdr cmd)))))))
  (ccl-compile-branch-1 cmd))

(defun ccl-compile-read-branch (cmd)
  (ccl-compile-branch-1 cmd))

(defun ccl-compile-branch-1 (cmd)
  (if (< (length cmd) 3)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let ((rrr (ccl-check-register (car (cdr cmd)) cmd))
	(branches (cdr (cdr cmd)))
	i ic0 ic1 ic2
	branch-tails)
    (ccl-embed-code (car cmd) rrr (- (length cmd) 2))
    (setq ic0 ccl-current-ic)
    (ccl-embed-nop (1- (length cmd)))
    (setq i 0)
    (while branches
      (ccl-embed-const ccl-current-ic (+ ic0 i))
      (ccl-compile-1 (car branches))
      (setq branch-tails (cons ccl-current-ic branch-tails))
      (ccl-embed-nop)
      (setq i (1+ i))
      (setq branches (cdr branches)))
    ;; We don't need `jump' from the last branch.
    (setq branch-tails (cdr branch-tails))
    (setq ccl-current-ic (1- ccl-current-ic))
    (while branch-tails
      (ccl-embed-code 'jump 0 ccl-current-ic (car branch-tails))
      (setq branch-tails (cdr branch-tails)))
    ;; This is the case `rrr' is out of range.
    (ccl-embed-const ccl-current-ic (+ ic0 i))
    ))

(defun ccl-compile-loop (cmd)
  (if (< (length cmd) 2)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let ((ccl-loop-head ccl-current-ic)
	(ccl-breaks nil))
    (setq cmd (cdr cmd))
    (while cmd
      (ccl-compile-1 (car cmd))
      (setq cmd (cdr cmd)))
    (while ccl-breaks
      (ccl-embed-code 'jump 0 ccl-current-ic (car ccl-breaks))
      (setq ccl-breaks (cdr ccl-breaks)))))

(defun ccl-compile-break (cmd)
  (if (/= (length cmd) 1)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (if (null ccl-loop-head)
      (error "CCL: No outer loop: %s" cmd))
  (setq ccl-breaks (cons ccl-current-ic ccl-breaks))
  (ccl-embed-nop))

(defun ccl-compile-repeat (cmd)
  (if (/= (length cmd) 1)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (if (null ccl-loop-head)
      (error "CCL: No outer loop: %s" cmd))
  (ccl-embed-code 'jump 0 ccl-loop-head))

(defun ccl-compile-write-repeat (cmd)
  (if (/= (length cmd) 2)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (if (null ccl-loop-head)
      (error "CCL: No outer loop: %s" cmd))
  (let ((arg (nth 1 cmd)))
    (cond ((integer-or-char-p arg)
	   (ccl-embed-code 'write-c-jump 0 ccl-loop-head)
	   (ccl-embed-const arg))
	  ((stringp arg)
	   (ccl-embed-code 'write-s-jump 0 ccl-loop-head)
	   (let ((i 0) (len (length arg)))
	     (ccl-embed-const (length arg))
	     (while (< i len)
	       (ccl-embed-const (aref arg i))
	       (setq i (1+ i)))))
	  (t
	   (ccl-check-register arg cmd)
	   (ccl-embed-code 'write-jump arg ccl-loop-head)))))

(defun ccl-compile-write-read-repeat (cmd)
  (if (or (< (length cmd) 2) (> (length cmd) 3))
      (error "CCL: Invalid number of arguments: %s" cmd))
  (if (null ccl-loop-head)
      (error "CCL: No outer loop: %s" cmd))
  (let ((rrr (ccl-check-register (nth 1 cmd) cmd))
	(arg (nth 2 cmd)))
    (cond ((null arg)
	   (ccl-embed-code 'write-read-jump rrr ccl-loop-head))
	  ((integer-or-char-p arg)
	   (ccl-embed-code 'write-c-read-jump rrr ccl-loop-head)
	   (ccl-embed-const arg))
	  ((or (stringp arg) (vectorp arg))
	   (ccl-embed-code (if (stringp arg)
			       'write-s-read-jump
			     'write-a-read-jump)
			   rrr ccl-loop-head)
	   (let ((i 0) (len (length arg)))
	     (ccl-embed-const (length arg))
	     (while (< i len)
	       (ccl-embed-const (aref arg i))
	       (setq i (1+ i)))))
	  (t (error "CCL: Invalide argument %s: %s" arg cmd)))))
			    
(defun ccl-compile-read (cmd)
  (let ((rrr (ccl-check-register (nth 1 cmd) cmd)))
    (cond ((= (length cmd) 2)
	   (ccl-embed-code 'read1 rrr 0))
	  ((= (length cmd) 3)
	   (ccl-embed-code 'read2 rrr (get (nth 2 cmd) 'ccl-register-number)))
	  (t (error "CCL: Invalid number of arguments: %s" cmd)))))

(defun ccl-compile-read-if (cmd)
  (if (and (/= (length cmd) 3) (/= (length cmd) 4))
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let* ((expr (nth 1 cmd))
	 (rrr (ccl-check-register (car expr) cmd))
	 (true-cmds (nth 2 cmd))
	 (false-cmds (nth 3 cmd))
	 ic0 ic1 ic2)
    (setq ic0 ccl-current-ic)
    (ccl-embed-nop 3)
    (ccl-compile-1 true-cmds)
    (if (null false-cmds)
	(setq ic1 ccl-current-ic)
      (setq ic2 ccl-current-ic)
      (ccl-embed-const 0)
      (setq ic1 ccl-current-ic)
      (ccl-compile-1 false-cmds)
      (ccl-embed-code 'jump 0 ccl-current-ic ic2))
    (let ((arg (nth 2 expr)))
      (ccl-embed-code (if (integer-or-char-p arg) 'read-jump-cond-c
			'read-jump-cond-r)
		      rrr ic1 ic0)
      (ccl-embed-const (if (integer-or-char-p arg) arg
			 (get arg 'ccl-register-number))
		       (1+ ic0))
      (ccl-embed-const (get (nth 1 expr) 'ccl-arith-code) (+ ic0 2)))))

(defun ccl-compile-write (cmd)
  (if (and (/= (length cmd) 2) (/= (length cmd) 3))
      (error "CCL: Invalid number of arguments: %s" cmd))
  (let ((rrr (nth 1 cmd)))
    (cond ((integer-or-char-p rrr)
	   (ccl-embed-code 'write-c 0 0)
	   (ccl-embed-const rrr))
	  ((stringp rrr)
	   (ccl-compile-write-string (list 'write rrr)))
	  (t
	   (ccl-check-register rrr cmd)
	   (let ((arg (nth 2 cmd)))
	     (if arg
		 (cond ((symbolp arg)
			(ccl-check-register arg cmd)
			(ccl-embed-code 'write2 rrr
					(get arg 'ccl-register-number)))
		       ((vectorp arg)
			(let ((i 0) (len (length arg)))
			  (ccl-embed-code 'write-a rrr 0)
			  (ccl-embed-const len)
			  (while (< i len)
			    (ccl-embed-const (aref arg i))
			    (setq i (1+ i)))))
		       (t (error "CCL: Invalid argument %s: %s" arg cmd)))
	       (ccl-embed-code 'write1 rrr 0)))))))

(defun ccl-compile-end (cmd)
  (if (/= (length cmd) 1)
      (error "CCL: Invalid number of arguments: %s" cmd))
  (ccl-embed-code 'end 0 0))

;;; CCL dump staffs
(defvar ccl-program-vector-dump nil)

;;;###autoload
(defun ccl-dump (ccl-code)
  "Disassemble compiled CCL-CODE."
  (save-excursion
    (set-buffer (get-buffer-create "*CCL-Dump*"))
    (erase-buffer)
    (setq ccl-program-vector-dump ccl-code)
    (let ((len (length ccl-code)))
      (insert "Main:\n")
      (setq ccl-current-ic 1)
      (if (> (aref ccl-code 0) 0)
	  (progn
	    (while (< ccl-current-ic (aref ccl-code 0))
	      (ccl-dump-1))
	    (insert "At EOF:\n")))
      (while (< ccl-current-ic len)
	(ccl-dump-1))
      ))
  (display-buffer (get-buffer "*CCL-Dump*")))

(defun ccl-get-next-code ()
  (prog1
      (aref ccl-program-vector-dump ccl-current-ic)
    (setq ccl-current-ic (1+ ccl-current-ic))))

(defun ccl-dump-1 ()
  (let* ((opcode (ccl-get-next-code))
	 (code (logand opcode 31))
	 (cmd (aref ccl-machine-code-table code))
	 (rrr (logand (ash opcode -5) 7))
	 (cc (ash opcode -8)))
    (insert (format "%4d: " (1- ccl-current-ic)))
    (funcall (get cmd 'ccl-dump-function) rrr cc))) 

(defun ccl-dump-set-cs (rrr cc)
  (insert (format "r%d = %s\n" rrr cc)))

(defun ccl-dump-set-cl (rrr cc)
  (setq cc (ccl-get-next-code))
  (insert (format "r%d = %s\n" rrr cc)))

(defun ccl-dump-set-r (rrr cc)
  (insert (format "r%d = r%d\n" rrr cc)))

(defun ccl-dump-set-a (rrr cc)
  (let ((range (ccl-get-next-code)) (i 0))
    (insert (format "r%d = array[r%d] of length %d\n\t"
		    rrr cc range))
    (let ((i 0))
      (while (< i range)
	(insert (format "%d " (ccl-get-next-code)))
	(setq i (1+ i))))
    (insert "\n")))

(defun ccl-dump-jump (rrr cc)
  (insert (format "jump to %d\n" cc)))

(defun ccl-dump-jump-cond (rrr cc)
  (insert (format "if !(r%d), jump to %d\n" rrr cc)))

(defun ccl-dump-write-jump (rrr cc)
  (insert (format "write r%d, jump to %d\n" rrr cc)))

(defun ccl-dump-write-read-jump (rrr cc)
  (insert (format "write r%d, read r%d, jump to %d\n" rrr rrr cc)))

(defun ccl-dump-write-c-jump (rrr cc)
  (let ((const (ccl-get-next-code)))
    (insert (format "write %s, jump to %d\n" const cc))))

(defun ccl-dump-write-c-read-jump (rrr cc)
  (let ((const (ccl-get-next-code)))
    (insert (format "write %s, read r%d, jump to %d\n" const rrr cc))))

(defun ccl-dump-write-s-jump (rrr cc)
  (let ((len (ccl-get-next-code)) (i 0))
    (insert "write \"")
    (while (< i len)
      (insert (format "%c" (ccl-get-next-code)))
      (setq i (1+ i)))
    (insert (format "\", jump to %d\n" cc))))

(defun ccl-dump-write-s-read-jump (rrr cc)
  (let ((len (ccl-get-next-code)) (i 0))
    (insert "write \"")
    (while (< i len)
      (insert (format "%c" (ccl-get-next-code)))
      (setq i (1+ i)))
    (insert (format "\", read r%d, jump to %d\n" rrr cc))))

(defun ccl-dump-write-a-read-jump (rrr cc)
  (let ((len (ccl-get-next-code)) (i 0))
    (insert (format "write array[r%d] of length %d, read r%d, jump to %d\n\t"
		    rrr len rrr cc))
    (while (< i len)
      (insert (format "%d " (ccl-get-next-code)))
      (setq i (1+ i)))
    (insert "\n")))

(defun ccl-dump-branch (rrr cc)
  (let ((i 0))
    (insert (format "jump to array[r%d] of length %d)\n\t" rrr cc))
    (while (<= i cc)
      (insert (format "%d " (ccl-get-next-code)))
      (setq i (1+ i)))
    (insert "\n")))

(defun ccl-dump-read1 (rrr cc)
  (insert (format "read r%d\n" rrr)))

(defun ccl-dump-read2 (rrr cc)
  (insert (format "read r%d and r%d\n" rrr cc)))

(defun ccl-dump-read-branch (rrr cc)
  (insert (format "read r%d, " rrr))
  (ccl-dump-branch rrr cc))

(defun ccl-dump-write1 (rrr cc)
  (insert (format "write r%d\n" rrr)))

(defun ccl-dump-write2 (rrr cc)
  (insert (format "write r%d and r%d\n" rrr cc)))

(defun ccl-dump-write-c (rrr cc)
  (insert (format "write %s\n" (ccl-get-next-code))))

(defun ccl-dump-write-s (rrr cc)
  (let ((len (ccl-get-next-code)) (i 0))
    (insert "write \"")
    (while (< i len)
      (insert (format "%c" (ccl-get-next-code)))
      (setq i (1+ i)))
    (insert "\"\n")))

(defun ccl-dump-write-a (rrr cc)
  (let ((len (ccl-get-next-code)) (i 0))
    (insert (format "write array[r%d] of length %d\n\t" rrr len))
    (while (< i 0)
      (insert "%d " (ccl-get-next-code))
      (setq i (1+ i)))
    (insert "\n")))

(defun ccl-dump-end (rrr cc)
  (insert "end\n"))

(defun ccl-dump-set-self-cs (rrr cc)
  (let ((arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "r%d %s= %s\n" rrr arith cc))))

(defun ccl-dump-set-self-cl (rrr cc)
  (setq cc (ccl-get-next-code))
  (let ((arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "r%d %s= %s\n" rrr arith cc))))

(defun ccl-dump-set-self-r (rrr cc)
  (let ((arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "r%d %s= r%d\n" rrr arith cc))))

(defun ccl-dump-set-expr-cl (rrr cc)
  (let ((const (ccl-get-next-code))
	(arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "r%d = r%d %s %s\n" rrr cc arith const))))

(defun ccl-dump-set-expr-r (rrr cc)
  (let ((reg (ccl-get-next-code))
	(arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "r%d = r%d %s r%d\n" rrr cc arith reg))))

(defun ccl-dump-jump-cond-c (rrr cc)
  (let ((const (ccl-get-next-code))
	(arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "if !(r%d %s %s), jump to %d\n" rrr arith const cc))))

(defun ccl-dump-jump-cond-r (rrr cc)
  (let ((reg (ccl-get-next-code))
	(arith (aref ccl-arith-table (ccl-get-next-code))))
    (insert (format "if !(r%d %s r%d), jump to %d\n" rrr arith reg cc))))

(defun ccl-dump-read-jump-cond-c (rrr cc)
  (insert (format "read r%d, " rrr))
  (ccl-dump-jump-cond-c rrr cc))

(defun ccl-dump-read-jump-cond-r (rrr cc)
  (insert (format "read r%d, " rrr))
  (ccl-dump-jump-cond-r rrr cc))

;; CCL emulation staffs 

;; Not yet implemented.

;; For byte-compiler

;;;###autoload
(defmacro define-ccl-program (name ccl-program &optional doc)
  "Does (defconst NAME (ccl-compile (eval CCL-PROGRAM)) DOC).
Byte-compiler expand this macro while compiling."
  (` (defconst (, name) (, (ccl-compile (eval ccl-program))) (, doc))))

(put 'define-ccl-program 'byte-hunk-handler 'macroexpand)

(provide 'ccl)
