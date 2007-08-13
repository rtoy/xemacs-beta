;; Some tests for edebug.

;;=======================
;; Reading tests.

(testing (one two) three)

(progn '(testing))

(a . (b . c))

(a . "test")

(a . (b . nil))

(a . [one two three])

;;===========================
;; Backquote test

(defun test ()
 (macroexpand '(` ((, (a)) . (, test))))
)
(test)

(progn (` ((, (point)) . (, (point)))))
(` (, (point)))

(defun test ()
(message "%d" 999999)

(defun test1 ()

  (progn
    (defun test ()
      (message "%d" 99999))
    (test)
    )

  )
(test1)
(test)

(eval (edebug-` (append [(, (point)) (, (point))] nil)))
(eval (edebug-` (append (, (point)) (, (point)) nil)))

(eval (progn (edebug-` (edebug-` (, '(, (point)))))))

(eval (edebug-` (let (((, 'a) 'b))
		  (message "%s" a))))

(defun test ()

(let ((r '(union x y)))
   (` (` (foo (, '(, r))))))
)

(defun test ()
 (let ((a '(one two))) a))

(def-edebug-spec test-func (sexp &rest def-form))

(setq edebug-unwrap-results t)
(setq edebug-unwrap-results nil)

(defmacro test-func (func &rest args)
  (edebug-` ((, func) (,@ args))))

(test-func message (concat "hi%s" "there") (+ 1 2))

(defmacro test-progn (&rest body)
  (edebug-` (progn (,@ body))))

(def-edebug-spec test-progn (&rest def-form))

(test-progn
 (message "testing"))


;;=================
;; Testing read syntax.

(format "testing %s %s %s" 1 2 (+ 1 2))

(defun test-syntax ()
  (setq mode-line-stuff'("draft(%b) ^C^S(end) ^C^Q(uit) ^C^K(ill)"))
;;  (re-search-forward "[.?!][])""']*$" nil t)
;;  (let (test)
    )
)

(test-syntax)

(let ())
;;====================
;; Testing function

(defun foo (x)
  (mapconcat (function identity) x ", "))

(defun foo (x)
  (mapconcat 'identity x ", "))

(defun foo (x)
  (mapconcat (function (lambda (x) x)) x ", "))

(require 'cl)

(defun foo (x)
  (mapconcat (function* (lambda (x &optional (y (1+ x)) &key xyz) x)) x ", "))

(defun foo (x)
  (mapconcat '(lambda (x) x) x ", "))

(foo '(1 2 3))

(apply 'identity one two)

(defun test1 (arg)
  arg)

(def-edebug-spec test1
  (form))
(setq x 5)
(test1 (+ x 2))

  (("test1" test1)))

(def-edebug-spec test1
  (&define sexp form))

(test (test1 xyz (message "jfdjfd")))

;;====================
;; Anonymous function test
(defun hej (arg)
  "docstring"
  (interactive (list 2))
  ((lambda (luttr &rest params)
     (apply luttr luttr params))
   (function (lambda (self n)
	       (edebug-trace "n: %s" n)
	       (if (= n 5) (edebug nil "n is 5"))
	       (edebug-tracing "cond"
		(cond
		 ((= 0 n) 1)
		 (t (* n (funcall self self (1- n))))))))
   11))

(defun hej-test ()
  (interactive)
  (message 
   "testing")
  (hej edebug-execution-mode)
  )
(hej-test)

(defun lambda-test ()
  ((lambda (arg) arg) 'xyz))
(lambda-test)

(defun test ()
  "doc string
 (with left paren on start of line)"

  1)


(progn
  (save-window-excursion
    (split-window)
    (split-window)
    (setq w (next-window)))
  (edebug-window-live-p w))


;;====================
;; Test edebugging top-level-forms

(def-edebug-spec test nil)
(let ((arg (list 'a 'b 'c)))
  (defun test (arg)
    arg)
  (test arg))


(fset 'emacs-setq (symbol-function 'setq))

(defmacro my-setq (&rest args)
  (while args
    (set (car args) (eval (car (cdr args))))
    (setq args (cdr (cdr args)))))

(defmacro test-macro (&rest args)
  (cons 'list args))
(def-edebug-spec test-macro 0)

(defun test ()
  (test-macro (message "testing")))
(test)

(defun test ()
  (message "someting")
  (function (lambda ()
	      (message "something else")))
  )

(funcall (test))

;;====================
;; Test for and inc
(def-edebug-spec for
  (symbolp ["from" def-form ["to" def-form] ["do" &rest def-form]]))

 ;; (symbolp ['from form ['to form] ['do &rest form]])

(inc x)
(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(defmacro for (var from init to final do &rest body)
  (let ((tempvar (make-symbol "max")))
    (edebug-` (let (((, var) (, init))
		    ((, tempvar) (, final)))
		(while (<= (, var) (, tempvar))
		  (,@ body)
		  (inc (, var)))))))

(defun test-for (one two)
  (for i from one to two do
       (message "%s" i))
  )

(let ((n 5))
  (for i from n to (* n (+ n 1)) do
    (message "%s" i)))

(test-for 3 10)

;;====================
;; Test condition-case
(def-edebug-spec condition-case
  (symbolp
   form
   &rest (symbolp &optional form)))

(setq edebug-on-signal '(error))

(defun test-condition-case ()
  (condition-case err
      (signal 'error '(oh))
    (error (message "error: %s" err))
    ))
(test-condition-case)

(require 'cl)

;;=============
;; lexical let

(defun test-lexical ()
  (funcall (lexical-let ((xyz 123))
	     (function (lambda (arg) (+ arg xyz))))
	   456))
(test-lexical)

;;====================
;; case test.
(defun test-case (one)
  (case one
	((one) (message "(one)"))
	("one" (message "one"))
	('one (message "'one"))
	))

(test-case 'one)

;;====================
;; Test of do from cl.el

(defun list-reverse (list)
  (do ((x list (cdr x))
       (y nil (cons (car x) y)))
      ((endp x) y)
    (message "x: %s  y: %s" x y)
    ))


(list-reverse '(testing one two three))

(defmacro test-backquote (arg list)
  (edebug-` 
   (progn
     (message "%s %s" (, arg) (, list))
     (mapcar (function (lambda (arg1) 
			 (message "%s %s" arg1 (, arg)))) (, list)))))

(def-edebug-spec test-backquote (def-form def-form))
(test-backquote (symbol-name 'something) (list 1 2 3))


(defmacro dired-map-over-marks (body arg &optional show-progress)
  (edebug-` (prog1
	 (let (buffer-read-only case-fold-search found results)
	   (if (, arg)
	       (if (integerp (, arg))
		   (progn;; no save-excursion, want to move point.
		     (dired-repeat-over-lines
		      (, arg)
		      (function (lambda ()
				  (if (, show-progress) (sit-for 0))
				  (setq results (cons (, body) results)))))
		     (if (< (, arg) 0)
			 (nreverse results)
		       results))
		 ;; non-nil, non-integer ARG means use current file:
		 (list (, body)))
	     (let ((regexp (dired-marker-regexp)) next-position)
	       (save-excursion
		 (goto-char (point-min))
		 ;; remember position of next marked file before BODY
		 ;; can insert lines before the just found file,
		 ;; confusing us by finding the same marked file again
		 ;; and again and...
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker))
		       found (not (null next-position)))
		 (while next-position
		   (goto-char next-position)
		   (if (, show-progress) (sit-for 0))
		   (setq results (cons (, body) results))
		   ;; move after last match
		   (goto-char next-position)
		   (forward-line 1)
		   (set-marker next-position nil)
		   (setq next-position (and (re-search-forward regexp nil t)
					    (point-marker)))))
	       (if found
		   results
		 (list (, body))))))
       ;; save-excursion loses, again
       (dired-move-to-filename))))


(def-edebug-spec dired-map-over-marks (&rest def-form))

(dired-map-over-marks
 (message "here") (+ 1 2) t)

;;====================
;; circular structure test

(edebug-install-custom-print)
(edebug-uninstall-custom-print)

(setq a '(1 2))
(progn
  (edebug-install-custom-print)
  (setq a '(1 2))
  (setcar a a))

(defun test ()
  (with-custom-print
     (format "%s" (setcar a a)))))
(test)
(setcdr a a)
(let ((b a)) b)

(with-custom-print
 (let ((print-circle t)
       (circ-list (list 'a 'b (vector 1 2 3 4) 'd 'e 'f)))
   (setcar (nthcdr 3 circ-list) circ-list)
   (aset (nth 2 circ-list) 2 circ-list)
   (prin1-to-string circ-list)))

;;====================
;; interactive-p test
(defun test-interactive ()
  (interactive)
  (interactive-p))

(test-interactive)
(call-interactively 'test-interactive)


;;====================
;; test several things:
;; - nested defun.
;; - display scrolling.


(defmacro testmacro ()
  '(interactive-p))

(call-interactively 'testing1)
(testing1 9)

(defun testing1 (arg)
  (interactive (list 3))
  (message "%s" (interactive-p)) (sit-for 2)
  (edebug-trace "interactive: %s" (testmacro))
  (defun testing1-1 ()
    (testing1 2))
;;  (custom-message "%s" arg "extra")
  (current-buffer)
  (selected-window)
  (while (< 0 (setq arg (1- arg)))
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg ; middle
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg   ; jump
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
  arg
))
(edebug-trace-display "*testing*" "one")
(edebug-tracer "one\n")

(testing1 a)
(call-interactively 'testing1)
(testing1 2)

(testing1-1)


(defmacro testmacro ()
  (interactive)
  '(one))

(defun testing2 ()
  (let* ((buf (get-buffer-create "testing"))
	 (win (get-buffer-window buf)))
    (testing1 1) 
    (window-point win)
    (window-point win)

;;    (read-stream-char buf)
    ))

(testing2)


(defun testing3 ()
  (save-excursion
    (set-buffer (get-buffer-create "*testing*"))
    (current-buffer)
    (point)
    (forward-char 1)
    ))
(testing3)


;;====================
;; anonymous function test
(defun testanon (arg)
  (mapcar '(lambda (x) x) arg)
  (mapcar (function (lambda (x) x)) arg)
  (mapcar (function testing3 ) arg)
  )

(testanon '(1 2 3))

;;====================
;; upward funarg test

(defmacro lambda (&rest args)
  "Return the quoted lambda expression."
  (cons 'function (list (cons 'lambda args))))

(lambda (testing) one two)

(defun testanon2 ()
  "return an anoymous function."
  (function (lambda (x) x))
  )
;; Emacs 19 has a lambda macro
(defun testanon2 ()
  "return an anoymous function."
  (lambda (x) x))
(testanon2)

(setq func
      (testanon2))
(funcall func 2)

(defun foo ()
  (mapcar #'(lambda (x)
	      (message "%S" x))
	  (append '(0) '(a b c d e f))))
(foo)

;;====================
;; downward funarg test

(defun xxx (func)
  (funcall func))

(defun yyy ()
  (xxx (function (lambda () (message "hello")))))

(yyy)

;; eval this:
(def-edebug-spec test nil)
(defun test (func list)
  (dolist (el list)
    (funcall func el)))

;; edebug this:
(defun testcall (l)
  (test (function (lambda (x) (print x)))  ;; set breakpoints in anon.
	l))

;; test call: 
(testcall '(a b c))

;; flet test.

(defun alep-write-history (&rest args)
  (message "alep-write-history( %s )\n"
	   args)
  ;; write out header
  '(write-region (format ";;Saved on %s\n" (current-time-string))
		nil buffer-file-name nil 'shut-up)
  ;; dump all not deleted actions
  (flet ((write-solution (sol)
	  t)
	 (write-action (action)
	  (if (a-h-action-deleted action)
	      ;; nothing to be done
	      t
	    (write-region
	     (format "(alep-new-history-action %S %S %S)\n"
		     (a-h-action-name action)
		     (alep-tnowv-string (a-h-action-in-tnowv
					 action))
		     (a-h-action-timestamp action))
	     nil buffer-file-name t 'shut-up)
	    (mapc 'write-solution
		  (a-h-action-solutions action)))))
    (mapc 'write-action
	  history-list))
  t)
(setq history-list '(1 2 3))
(alep-write-history)

;;=========================

  (edebug-trace "my stuff")

(defun fac (n)
  (if (= n 0) (edebug))
;#6           1      0 =5 
  (if (< 0 n)
;#5         = 
      (* n (fac (1- n)))
;#    5               0  
    1))
;#   0 

(fac 5)


;;====================
;; Timing test - how bad is edebug?

(defun looptest (n)
  (let ((i 0))
    (while (< i n) (setq i (1+ i)))))

(looptest 10000)

;;====================
;; eval-depth testing.

(defun test-depth (i)
  (test-depth (1+ i)))

;; Without edebug i reaches 193, failing on eval depth
;; With edebug, i reaches about 57.  Better safe than sorry.
(setq max-lisp-eval-depth 200)
(test-depth 0)

;;====================
;; specpdl-size testing.
(defun test-depth2 (i max)
  (let ((test max-specpdl-size)
	(max-lisp-eval-depth (+ 2 max-lisp-eval-depth))
	)
    (test-depth2 (1+ i) max-specpdl-size)))

(let ((max-lisp-eval-depth 300)
      (max-specpdl-size 3))
  (test-depth2 0 max-specpdl-size))

;;====================
;; Buffer testing.

(defun zprint-region-1 (start end switches)
  (let ((name (concat (buffer-name) ""))
        (width tab-width))
    (save-excursion
      (message "Spooling...")
      (let ((oldbuf (current-buffer)))
        (set-buffer (get-buffer-create " *spool temp*"))
        (widen)
        (erase-buffer)
        (insert-buffer-substring oldbuf start end)
        (setq tab-width width)
        (if (/= tab-width 8)
            (untabify (point-min) (point-max)))
        (setq start (point-min) end (point-max)))
      (apply 'call-process-region
             (nconc (list start end zpr-command nil nil nil
                          "-h" name switches)))
      (message "Spooling...done")
      )
    )
  )



(defun quick-hanoi (nrings)
  (with-output-to-temp-buffer "*hanio*"
    (set-buffer "*hanio*")
    (princ (format "Solution to %s ring hanoi problem\n\n" nrings))
    (hanoi0 nrings 'pole-1 'pole-2 'pole-3)))

(defun hanoi0 (n from to work)
;;  (edebug-set-window-configuration (edebug-current-window-configuration))
  (if (> n 0)
      (progn
;;	(save-excursion
;;	  (set-buffer "*hanio*")
;;	  (message "Point=%s window-point=%s" (point)
;;		   (window-point (get-buffer-window "*hanio*")))
;;	  (set-window-point (get-buffer-window "*hanio*") (point))
;;	  )
	
	(hanoi0 (1- n) from work to)
	(princ (format "ring %s from %s to %s\n" n from to))
	(hanoi0 (1- n) work to from))))

(quick-hanoi 5)


;;====================
;; Error test

(defun error-generating-function ()
  (message "try again?") (sit-for 1)
  (prog1
      (signal 'bogus '("some error" xyz abc))
      (error "debug-on-error: %s edebug-entered: %s edebug-recursion-depth: %s"
	     debug-on-error edebug-entered edebug-recursion-depth)))

;; --><-- point will be left between the two arrows
(setq debug-on-error nil)
(setq edebug-on-signal '(bogus))

(testing-function)
(defun testing-function ()
  (interactive)
  (message "YYY")
  (error-generating-function)
  (message "ZZZ"))


(let ((debug-on-error t))
  xyzzyz)

;;====================
;; Quitting with unwind-protect

(defun unwind-test ()
  (prog1
      (unwind-protect
	  (unwind-protect
	      (message "testing")
	    (message "unwinding1"))
	(message "unwinding2")
	(sit-for 1)
	)
    ))
(unwind-test)

(defmacro save-buffer-points (&rest body)
  (` (let ((buffer-points
	    (mapcar (function (lambda (buf)
				(set-buffer buf)
				(cons buf (point))))
		    (buffer-list))))
       (unwind-protect
	   (progn
	     (,@ body))
	 (mapcar (function (lambda (buf-point)
			     (if (buffer-name (car buf-point))
				 (progn
				   (set-buffer (car buf-point))
				   (goto-char (cdr buf-point))))))
		 buffer-points)))))

(defun testing4 ()
  (with-output-to-temp-buffer "*testing*"
    (princ "Line 1\n")
    (save-buffer-points
      (recursive-edit)
      )
    (princ "Line 2\n")
    ))

(testing4)
test!


;;====================
;; edebug-form-specs for Guido Bosch's flavors

(def-edebug-spec defmethod defun) ; same as defun
(def-edebug-spec defwhopper defun) ; same as defun

;;======================
;; Check syntax errors.

(defun test-too-many-arguments ()
  (mapcar 'test one two))

(mapcar 'not-enough)

(defun test-not-enough-arguments ()
  (mapcar 'test))

(defun test-bad-function ()
  (function))

(defun test-bad-function ()
  (function
   (bad () )))

(defun test-bad-lambda-arguments ()
  (function (lambda "bad" )))

(defun test-bad-defun-arguments "bad"
  (function (lambda "bad" )))

(defun test-bad-defun-arguments (arg "bad")  ;; wrong error
  (function (lambda "bad" )))

(defun test-bad-defun-arguments (&optional)
  (function (lambda "bad" )))

(defun test-bad-let-in-lambda ()
  (function (lambda ()
	      (let ((something one bad))))))  ;; wrong error

(defun test-bad-interactive ()
  (interactive one bad))

(defun test-bad-defvar ()
  (defvar test-defvar nil [bad]))

(defun test-bad-let1 ()
  (let bad))

(defun test-bad-let2 ()
  (let ((something one bad))))

(defun test-good-let ()
  (let ((a b))))

(defun test-bad-let3 ()
  (let (((bad)))))

(defun test-bad-let4 ()
  (let ("bad")))

(let ((good (list 'one))) good)

(defun test-bad-setq ()
  (setq "bad" ))

(setq good ok 
      "bad")

(defun test-bad-cond ()
  (cond "bad"))

(cond ())

(defun test-bad-cond ()
  (cond () [] "bad"))

(defun test-bad-condition-case1 ()
  (condition-case "bad"))

(defun test-bad-condition-case2 ()
  (condition-case err
      nil
    "bad"))

(defun test-bad-condition-case3 ()
  (condition-case err
      (error "messages")
;;    ()
    ((error quit) (message "%s" err))))


(def-edebug-spec do
  ((&rest &or symbolp
	       (fence symbolp &optional form form))
   (form body) body))

(defun bad-do (list)

(do (     x
	   (x list (cdr x))
     (y nil (cons (car x) y))
     (x list (cdr x) bad)
     "bad"
     )
      ((endp x) y)
    ))

(defun ok ()
  test
  )

(defun "bad" () )
(defun)

;;=========================

;; Test printing.

(defun test-window-buffer-change (arg)
  "testing"
  (interactive arg)
  (save-window-excursion
    (set-window-buffer (selected-window) (get-buffer "*scratch*"))
    (get-buffer-window (current-buffer))))
(test-window-buffer-change 'test)


(defun test-window-buffer-change ()
  (selected-window))

(test-window-buffer-change 1)

arg


(def-edebug-spec edebug-forms
  (&rest edebug-form))

(def-edebug-spec edebug-form
  (&or (edebug-function-symbolp edebug-forms)
       (anonymous-function edebug-forms)
       (edebug-macro-symbolp 
       sexp)))


(defun test-mapatoms () )

(mapatoms (function (lambda (arg) 
		      (princ 
		       arg)
		      )))


(test-mapatoms)

;; Test embedded &rest
(def-edebug-spec symbol-list
  ([&rest "a" symbolp] form))

(defun test ()
  (symbol-list a b a (+ c d)))
(test)

(def-edebug-spec group-alternates-test
  (&or ["foo" "bar"] "baz"))

(group-alternates-test foo bar)
(group-alternates-test baz )

;;---------------------

(defun test ()
  (dolist (f (list 1 2))
	  (message f)))

(defun test ()
  (dolist (el (list 'a 'b 'c))
    (print el)))


;; (of-type (type (more type)))

(def-edebug-spec test-nil
  (&or symbolp "nil"))
(test-nil () )

(defun test ()
  ((lambda (arg) arg) two)
)


;; Dot notation testing

(def-edebug-spec test-dot
  (symbolp . [&or symbolp (stringp)]))
(test-dot xyz . jk)
(test-dot xyz "jk")

(def-edebug-spec test-dot
  (&or symbolp (test-dot1)))

(def-edebug-spec test-dot1 
  (test-dot2 . test-dot2))

(def-edebug-spec test-dot2
  (symbolp))

(def-edebug-spec test-dot2
  ([&or test-dot1 nil]))

(def-edebug-spec test-dot1
  (symbolp))

  (&or symbolp (test-dot)))


(defun test ()
  (test-dot (a . b)))

(def-edebug-spec edebug-specs
  (symbolp . symbolp))

(def-edebug-spec edebug-specs1
  (&or symbolp))

(def-edebug-spec edebug-spec
  (&or
   symbolp))


(def-edebug-spec test-not
  (symbolp . [&not symbolp form]))
(test-not "string")

;;--------------------------
;; Loop macro testing

(defun test ()
  (loop-var (((var1 (var2 var4) . (var3 var5)) . var1))
	    ))

(loop-var (var1 var2 . var3))
(loop-var (var1 ["bad"] . "bad"))

            '	    (var2 var3 . var4))

(loop for ((a . b) (c . d))
      of-type ((float . float) (integer. integer))
      )

(defun test ()
  (loop if some-test
	       collect a-form into var
	else minimize x ;; of-type some-type
	     and append x
	end))

(defun test ()
  (loop for x from 1 to 9
	and y = nil then x
	collect (list x y)))

(defun test ()
  (loop for i from 10 downto 1 by 3
	do (print i)))


(defun test ()
  (loop for item = 1 then (+ item 10)
	repeat 5
	collect item))

(defun test ()
  (loop for z upfrom 2
	thereis
	(loop for n upfrom 3 below (+ z 2) ;; + was log
	      thereis
	      (loop for x below z
		    thereis
		    (loop for y below z
			  thereis (= (+ (* x n) ;; * was expt
					(* y n))
				     (* z n)))))))

(defun test ()
  (loop for name in '(fred sue alice joe june)
	as age in '(22 26 19 20 10)
	append (list name age) into name-and-age-list
	count name into name-count
	sum age into total-age
	finally
	(return (values (round* total-age name-count)
			name-and-age-list))))

(defun test ()
  (loop for x from 0 to 3
	do (print x)
	if (zerop (mod x 2))
	do (princ " a")
	and if (zerop (floor* x 2))
	do (princ " b")
	end
	and do (princ " c")))


(defun test ()
  (loop initially do (message x)
	do (dispatch-event event)))

(defun test ()
  (loop initially do (popup-menu menu)   ;; do is an error here.
	with event = (allocate-event)
	do (dispatch-event event)))

(defun popup-menu-synchronously (menu)
  (loop initially (popup-menu menu) 
	with event = (allocate-event)
	until (button-release-event-p (next-event event))
	do (dispatch-event event)
	finally do (deallocate-event event)))

(defun test ()
   (loop with list = '(1 2 3 4)
         for item in list
         sum item into summation
         collect (list item)))

;;----------

(defun test-catch (n)
  (if (> n 0)
      (let* ((test
	      (catch 'test
		(test-catch (1- n)))))
	(if test
	    (do-throw)))
    (do-throw)))

(defun do-throw ()
  (funcall 'throw 'test 'here))

(test-catch 3)


;;------------

(defun* foo (a &optional b &key c d (e 17)))

(def-edebug-spec test-vector
  ((vector form)))

(defun test ()

  (test-vector [one]))

[testing one two three]
(testing one two three)

(def-edebug-spec test
  (&optional &or ["something" keywordp] symbolp))

(test something :somekey)

;;----------



(defun find-faq (filename)
  "Hmtar en faq."
  (interactive 

   (list 
    (all-faq-a-valid-ftp
     (intern-soft
      (let ((minibuffer-help-form
	     (function
	      (let* ((partial (buffer-string))
		     (soft (intern-soft partial all-faq-known-files)))
		(if soft
		    (set soft (append (cdr (symbol-value soft)) 
				      (list (car (symbol-value soft))))))
		(if (and soft (all-faq-a-valid-ftp soft))
		    (mapconcat 
		     (function
		      (lambda (apair)
			(car apair)))
		     (symbol-value soft)
		     "\n"))))))
	(completing-read "What faq? "
			 all-faq-known-files
			 (function all-faq-a-valid-ftp)
			 t ""))
      all-faq-known-files)))
)
  (find-file filename))


;;===============

;; Keyword testing

(def-edebug-spec test
  (&key (bad "one") (good "thing")))
(defun test-key ()
  (test :bad one)
  (test1 :bad one))

(def-edebug-spec test
  (("one")))

  (&rest ["one" "two"]))

(test (one))

(progn (message "one" ) )
(testet  xxx)
(progn (message "one" ) )

(let ((a (+ 1 1)))
  (1+ a))

(mapcar 'test (list 1 2 3))
(defun test (testing) testing)

;;==================
;; Test defstruct.

(defun test ()
  (defstruct 
    (test (:constructor construct (args)))
    a
    (b (+ a c))
    c))

;;================
;; advice

(defun foo (x)
  "Add 1 to x."
  (1+ x))

(require 'advice)

(defadvice foo (before add2 first activate)
  "  Add 2 to x"
  (setq x (1+ x)))

(foo 3)
