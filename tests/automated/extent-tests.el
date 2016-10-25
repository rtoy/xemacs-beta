;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Created: 1999
;; Keywords: tests

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test extents operations.
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))


;;-----------------------------------------------------
;; Creating and attaching.
;;-----------------------------------------------------

(with-temp-buffer
  (let ((extent (make-extent nil nil))
	(string "somecoolstring"))

    ;; Detached extent.
    (Assert (extent-detached-p extent))

    ;; Put it in a buffer.
    (set-extent-endpoints extent 1 1 (current-buffer))
    (Assert (eq (extent-object extent) (current-buffer)))

    ;; And then into another buffer.
    (with-temp-buffer
      (set-extent-endpoints extent 1 1 (current-buffer))
      (Assert (eq (extent-object extent) (current-buffer))))

    ;; Now that the buffer doesn't exist, extent should be detached
    ;; again.
    (Assert (extent-detached-p extent))

    ;; This line crashes XEmacs 21.2.46 and prior.
    (set-extent-endpoints extent 1 (length string) string)
    (Assert (eq (extent-object extent) string))
    )

  (let ((extent (make-extent 1 1)))
    ;; By default, extent should be closed-open
    (Assert (eq (get extent 'start-closed) t))
    (Assert (eq (get extent 'start-open) nil))
    (Assert (eq (get extent 'end-open) t))
    (Assert (eq (get extent 'end-closed) nil))

    ;; Make it closed-closed.
    (set-extent-property extent 'end-closed t)

    (Assert (eq (get extent 'start-closed) t))
    (Assert (eq (get extent 'start-open) nil))
    (Assert (eq (get extent 'end-open) nil))
    (Assert (eq (get extent 'end-closed) t))

    ;; open-closed
    (set-extent-property extent 'start-open t)

    (Assert (eq (get extent 'start-closed) nil))
    (Assert (eq (get extent 'start-open) t))
    (Assert (eq (get extent 'end-open) nil))
    (Assert (eq (get extent 'end-closed) t))

    ;; open-open
    (set-extent-property extent 'end-open t)

    (Assert (eq (get extent 'start-closed) nil))
    (Assert (eq (get extent 'start-open) t))
    (Assert (eq (get extent 'end-open) t))
    (Assert (eq (get extent 'end-closed) nil)))

  )

;;-----------------------------------------------------
;; Insertion behavior.
;;-----------------------------------------------------

(defun et-range (extent)
  "List (START-POSITION END-POSITION) of EXTENT."
  (list (extent-start-position extent)
	(extent-end-position extent)))

(defun et-insert-at (string position)
  "Insert STRING at POSITION in the current buffer."
  (save-excursion
    (goto-char position)
    (insert string)))

;; Test insertion at the beginning, middle, and end of the extent.

;; closed-open

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    ;; current state: "###[eee)###"
    ;;                 123 456 789
    (Assert (equal (et-range e) '(4 7)))

    (et-insert-at "xxx" 4)

    ;; current state: "###[xxxeee)###"
    ;;                 123 456789 012
    (Assert (equal (et-range e) '(4 10)))

    (et-insert-at "yyy" 7)

    ;; current state: "###[xxxyyyeee)###"
    ;;                 123 456789012 345
    (Assert (equal (et-range e) '(4 13)))

    (et-insert-at "zzz" 13)

    ;; current state: "###[xxxyyyeee)zzz###"
    ;;                 123 456789012 345678
    (Assert (equal (et-range e) '(4 13)))
    ))

;; closed-closed

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'end-closed t)

    ;; current state: "###[eee]###"
    ;;                 123 456 789
    (Assert (equal (et-range e) '(4 7)))

    (et-insert-at "xxx" 4)

    ;; current state: "###[xxxeee]###"
    ;;                 123 456789 012
    (Assert (equal (et-range e) '(4 10)))

    (et-insert-at "yyy" 7)

    ;; current state: "###[xxxyyyeee]###"
    ;;                 123 456789012 345
    (Assert (equal (et-range e) '(4 13)))

    (et-insert-at "zzz" 13)

    ;; current state: "###[xxxyyyeeezzz]###"
    ;;                 123 456789012345 678
    (Assert (equal (et-range e) '(4 16)))
    ))

;; open-closed

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'start-open t)
    (put e 'end-closed t)

    ;; current state: "###(eee]###"
    ;;                 123 456 789
    (Assert (equal (et-range e) '(4 7)))

    (et-insert-at "xxx" 4)

    ;; current state: "###xxx(eee]###"
    ;;                 123456 789 012
    (Assert (equal (et-range e) '(7 10)))

    (et-insert-at "yyy" 8)

    ;; current state: "###xxx(eyyyee]###"
    ;;                 123456 789012 345
    (Assert (equal (et-range e) '(7 13)))

    (et-insert-at "zzz" 13)

    ;; current state: "###xxx(eyyyeezzz]###"
    ;;                 123456 789012345 678
    (Assert (equal (et-range e) '(7 16)))
    ))

;; open-open

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'start-open t)

    ;; current state: "###(eee)###"
    ;;                 123 456 789
    (Assert (equal (et-range e) '(4 7)))

    (et-insert-at "xxx" 4)

    ;; current state: "###xxx(eee)###"
    ;;                 123456 789 012
    (Assert (equal (et-range e) '(7 10)))

    (et-insert-at "yyy" 8)

    ;; current state: "###xxx(eyyyee)###"
    ;;                 123456 789012 345
    (Assert (equal (et-range e) '(7 13)))

    (et-insert-at "zzz" 13)

    ;; current state: "###xxx(eyyyee)zzz###"
    ;;                 123456 789012 345678
    (Assert (equal (et-range e) '(7 13)))
    ))


;;-----------------------------------------------------
;; Deletion behavior.
;;-----------------------------------------------------

(dolist (props '((start-closed t end-open t)
		 (start-closed t end-open nil)
		 (start-closed nil end-open nil)
		 (start-closed nil end-open t)))
  ;; Deletion needs to behave the same regardless of the open-ness of
  ;; the boundaries.

  (with-temp-buffer
    (insert "xxxxxxxxxx")
    (let ((e (make-extent 3 9)))
      (set-extent-properties e props)

      ;; current state: xx[xxxxxx]xx
      ;;                12 345678 90
      (Assert (equal (et-range e) '(3 9)))

      (delete-region 1 2)

      ;; current state: x[xxxxxx]xx
      ;;                1 234567 89
      (Assert (equal (et-range e) '(2 8)))

      (delete-region 2 4)

      ;; current state: x[xxxx]xx
      ;;                1 2345 67
      (Assert (equal (et-range e) '(2 6)))

      (delete-region 1 3)

      ;; current state: [xxx]xx
      ;;                 123 45
      (Assert (equal (et-range e) '(1 4)))

      (delete-region 3 5)

      ;; current state: [xx]x
      ;;                 12 3
      (Assert (equal (et-range e) '(1 3)))

      )))

;;; #### Should have a test for read-only-ness and insertion and
;;; deletion!

;;-----------------------------------------------------
;; `detachable' property
;;-----------------------------------------------------

(dolist (props '((start-closed t end-open t)
		 (start-closed t end-open nil)
		 (start-closed nil end-open nil)
		 (start-closed nil end-open t)))
  ;; `detachable' shouldn't relate to region properties, hence the
  ;; loop.
  (with-temp-buffer
    (insert "###eee###")
    (let ((e (make-extent 4 7)))
      (set-extent-properties e props)
      (Assert (get e 'detachable))

      (Assert (not (extent-detached-p e)))

      (delete-region 4 5)
      ;; ###ee### (not detached yet)
      (Assert (not (extent-detached-p e)))

      (delete-region 4 6)
      ;; ###### (should be detached now)
      (Assert (extent-detached-p e))))

  (with-temp-buffer
    (insert "###eee###")
    (let ((e (make-extent 4 7)))
      (set-extent-properties e props)
      (put e 'detachable nil)
      (Assert (not (get e 'detachable)))
    
      (Assert (not (extent-detached-p e)))

      (delete-region 4 5)
      ;; ###ee###
      (Assert (not (extent-detached-p e)))

      (delete-region 4 6)
      ;; ###[]###
      (Assert (not (extent-detached-p e)))
      (Assert (equal (et-range e) '(4 4)))
      ))
  )


;;-----------------------------------------------------
;; Zero-length extents.
;;-----------------------------------------------------

;; closed-open (should stay put)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (et-insert-at "foo" 4)
    (Assert (equal (et-range e) '(4 4)))))

;; open-closed (should move)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'start-open t)
    (put e 'end-closed t)
    (et-insert-at "foo" 4)
    (Assert (equal (et-range e) '(7 7)))))

;; closed-closed (should extend)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'end-closed t)
    (et-insert-at "foo" 4)
    (Assert (equal (et-range e) '(4 7)))))

;; open-open (illegal; forced to behave like closed-open)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'start-open t)
    (et-insert-at "foo" 4)
    (Assert (equal (et-range e) '(4 4)))))

;;-----------------------------------------------------
;; Extents and the minibuffer.
;;-----------------------------------------------------

(let* ((string (copy-sequence "Der Hoelle Rache kocht in meinem Herzen"))
       (e (make-extent (search "Rache" string) (search "kocht" string)
                       string))
       (ee (make-extent (search "meinem" string) (search "Herzen" string)
                       string))
       (property-name '#:secret-token)
       event list)
  (setf (extent-property e 'duplicable) t
        (extent-property e property-name) t
        (extent-property ee 'duplicable) nil) ;; Actually the default.
  (block enough
    (enqueue-eval-event #'(lambda (ignore) (return-from enough)) nil)
    ;; Silence prompt on TTY. Maybe we shouldn't be doing this.
    (flet ((send-string-to-terminal (&rest ignore)))
      (while (setq event (next-event event string))
        (dispatch-event event))))
  (setq list (extent-list (get-buffer " *Echo Area*")))
  (Assert list "checking extent info was preserved in #'next-event")
  (Assert (eql 1 (length list)) "checking only one extent was preserved")
  (Assert (eql t (get (car list) property-name))
          "checking it was our duplicable extent that was preserved"))

;;-----------------------------------------------------
;; Extents and string output streams.
;;-----------------------------------------------------

(let* ((stream (make-string-output-stream :element-type 'character))
       (string (copy-sequence "Tod und Verzweiflung flammet um mich her!"))
       (e (make-extent (search "Verzweiflung" string) (search "flammet" string)
                       string))
       (ee (make-extent (search " um " string) (search "her!" string)
                       string))
       (property-name '#:secret-token)
       event list output)
  (Assert (eq 'stream (type-of stream))
          "checking string output stream created OK")
  (Assert (equal "" (get-output-stream-string stream))
          "checking newly created string output stream empty")
  (setf (extent-property e 'duplicable) t
        (extent-property e property-name) t
        (extent-property ee 'count) most-positive-fixnum
        (extent-property ee 'duplicable) nil) ;; Actually the default.
  (write-sequence string stream) ;; Includes both ee and e, since the
                                 ;; stream is treated like a string,
                                 ;; not a buffer.
  (write-sequence " " stream)
  ;; Now write a string that only includes part of the text of e.
  (write-sequence string stream :end (search "flung" string))
  (terpri stream)
  (setf output (get-output-stream-string stream))
  (Assert
   (equal "Tod und Verzweiflung flammet um mich her! Tod und Verzwei\n"
          output)
   "checking #'get-output-stream-string functioned as expected")
  (setf list (extent-list output))
  (Assert (eql 3 (length list)))
  (Assert (eq (get (car list) property-name) t)
          "checking property preserved, duplicable extent")
  (Assert (eql (get (cadr list) 'count) most-positive-fixnum)
          "checking property preserved, non-duplicable extent")
  (Assert (eq (get (caddr list) property-name) t)
          "checking property preserved, second duplicable extent")

  (Assert (eq string (write-sequence string stream))
          "checking return value of #'write-sequence")
  (Assert (eq nil (clear-output stream))
          "checking return value of #'clear-output")
  (Assert (equal "" (get-output-stream-string stream))
          "checking #'clear-output succeeded"))

(Check-Error unimplemented (make-string-output-stream
                            :element-type '(unsigned-byte 8)))
(Check-Error wrong-number-of-arguments
             (make-string-output-stream :element-type))

(Assert (eq 'stream (type-of (make-string-output-stream)))
        "checking passing no arguments to #'make-string-output-stream fine")

(Check-Error wrong-type-argument (clear-output most-positive-fixnum))
(Assert (eq nil (clear-output (current-buffer)))
        "checking #'clear-output doesn't choke on a buffer argument")

(macrolet
 ((Assert-format-into-extents (&rest pad-chars)
    (cons
     'progn
     (loop for pad-char in pad-chars
       collect
       `(let* ((format (concatenate 'string
				    "hello there "
				    "%!" (string (or ,pad-char ?\x20))
				    "400s everyone"))
	       ;; This extent will stretch, because it's at the beginning
	       ;; and its start is closed (the default).
	       (extent (make-extent (search "%" format)
				    (search " everyone" format) format))
	       ;; This extent won't, it will move later with insertion of a
	       ;; longer string.
	       (ee (make-extent (1- (extent-end-position extent))
				(extent-end-position extent) format))
	       (string
		(copy-sequence "So bist du meine Tochter nimmer mehr!"))
	       ;; These two extents should just pass through.
	       (E (make-extent (search "meine" string)
			       (search "nimmer" string) string))
	       (EE (make-extent (search " mehr" string) (search "!" string)
				string))
	       (property-name '#:secret-token)
               last marker)
	 (setf (extent-property E 'duplicable) t
	       (extent-property E property-name) t
	       (extent-property E 'face) 'green
	       (extent-property EE 'count) most-positive-fixnum
	       (extent-property EE 'duplicable) nil ;; Actually the default.
	       (extent-property ee 'start-open) t
	       (extent-property ee 'face) 'isearch
	       (extent-property ee 'duplicable) 't
	       (extent-property ee 'shorter) t
	       (extent-property extent 'longer) t
	       (extent-property extent 'face) 'blue
	       (extent-property extent 'duplicable) 't)
	 (with-temp-buffer
	   (Assert (eq (format-into (current-buffer) format string)
		       (current-buffer))
		   "checking return value of #'format-into")
	   (Assert (eql (point) (1+ (+ (length format) 400
				       (- (length "%! 400s")))))
		   "checking #'format-into did the expected with a buffer arg")
	   (Assert (eql (point) (point-max))
		   "checking no random unpredicted garbage inserted")
	   (Assert (null (extent-list (current-buffer) nil nil nil
				      'count))
		   "checking non-duplcable extent not passed through")
	   (Assert (eql (extent-length
			 (car (extent-list (current-buffer) nil nil nil
					   'shorter)))
			1)
		   "checking shorter extent has expected length")
	   (Assert (eql (extent-length
			 (car (extent-list (current-buffer) nil nil nil
					   'longer)))
			400)
		   "checking longer extent has expected length")
	   (Assert (eql (extent-length 
			 (car (extent-list (current-buffer) nil nil nil
					   property-name)))
			(extent-length E))
		   "checking first pass-through extent has expected length")
	   (Assert (equal (buffer-substring
			   (extent-start-position 
			    (car (extent-list (current-buffer) nil nil nil
					      property-name)))
			   (extent-end-position 
			    (car (extent-list (current-buffer) nil nil nil
					      property-name))))
			  "meine Tochter ")
		   "checking first pass-through extent reflects text OK")
	   (Assert (not (cdr (extent-list (current-buffer) nil nil nil
					  'shorter)))
		   "checking no duplicates passed through")
	   (Assert (not (cdr (extent-list (current-buffer) nil nil nil
					  'longer)))
		   "checking no duplicates passed through")
	   (Assert (not (cdr (extent-list (current-buffer) nil nil nil
					  property-name)))
		   "checking no duplicates passed through")
           (setf last (point) marker (point-marker))
	   (Assert (eq (format-into marker format string) marker)
		   "checking return value of #'format-into, marker")
	   (Assert (= marker (+ (length format) 400
                                (- (length "%! 400s")) last))
		   "checking #'format-into did the expected, marker arg")
	   (Assert (= marker (point-max))
		   "checking no random unpredicted garbage inserted, marker")
	   (Assert (null (extent-list (current-buffer)
                                      last marker nil 'count))
		   "checking non-duplcable extent not passed through, marker")
	   (Assert (eql (extent-length
			 (car (extent-list (current-buffer) last marker nil
					   'shorter)))
			1)
		   "checking shorter extent has expected length, marker")
	   (Assert (eql (extent-length
			 (car (extent-list (current-buffer) last marker nil
					   'longer)))
			400)
		   "checking longer extent has expected length, marker")
	   (Assert (eql (extent-length 
			 (car (extent-list (current-buffer) last marker nil
					   property-name)))
			(extent-length E))
		   "checking length first pass-through extent, marker")
	   (Assert (equal (buffer-substring
			   (extent-start-position 
			    (car (extent-list (current-buffer) last marker
                                              nil property-name)))
			   (extent-end-position 
			    (car (extent-list (current-buffer) last marker
                                              nil property-name))))
			  "meine Tochter ")
		   "checking first pass-through reflects text OK, marker")
	   (Assert (not (cdr (extent-list (current-buffer) last marker
                                          nil 'shorter)))
		   "checking no duplicates passed through, marker")
	   (Assert (not (cdr (extent-list (current-buffer) last marker
                                          nil 'longer)))
		   "checking no duplicates passed through, marker")
	   (Assert (not (cdr (extent-list (current-buffer) last marker
                                          nil property-name)))
		   "checking no duplicates passed through, marker")))))))
  (Assert-format-into-extents
    ?\x20 ?a ?z ?A ?Z ?\xa0 (decode-char 'ucs #x20ac)
    (decode-char 'ucs #x2012) (decode-char 'ucs #x2020)))

;; Check that substitute-command-keys preserves extents.

(let*
    ((string
      (copy-sequence "So bist du meine Tochter nimmer mehr!"))
     (ostring string)
     ;; These two extents should just pass through.
     (E (make-extent (search "meine" string)
                     (search "nimmer" string) string))
     (EE (make-extent (search " mehr" string) (search "!" string)
                      string))
     (property-name '#:secret-token) substituted pE pEE)
  (setf (extent-property E 'duplicable) t
        (extent-property E property-name) t
        (extent-property E 'face) 'green
        (extent-property EE 'count) most-positive-fixnum
        (extent-property EE 'duplicable) nil) ;; Actually the default.
  (Assert (eq string (substitute-command-keys string))
          "check substitute-command-keys gives identical string, no changes")
  (setf string (concat string " \\[find-file] "))
  (Assert (equal (format "%s %s " ostring
                         (key-description (where-is-internal 'find-file
                                                             nil t)))
                 (setq substituted (substitute-command-keys string))))
  (Assert (setf pE (car (extent-list substituted nil nil nil property-name)))
          "checking duplicable extent returned")
  (Assert (eql (extent-start-position pE) (extent-start-position E))
          "checking duplicable extent start position preserved")
  (Assert (eql (extent-end-position pE) (extent-end-position E))
          "checking duplicable extent end position preserved")
  ;; This is a string operation returning a string, it should preserve
  ;; non-duplicable extents, as do #'concat, #'format.
  (Assert (setf pEE (car (extent-list substituted nil nil nil 'count)))
          "checking non-duplicable extent returned")
  (when pEE
    (Assert (eql (extent-start-position pEE) (extent-start-position EE))
            "checking non-duplicable extent start position preserved")
    (Assert (eql (extent-end-position pEE) (extent-end-position EE))
            "checking non-duplicable extent end position preserved"))
  (Assert (equal (concat ostring "\\=")
                 (substitute-command-keys (concat ostring "\\="))))
  
  (Assert (equal (concat ostring "\\{find-file}")
                 (substitute-command-keys (concat ostring
                                                  "\\=\\{find-file}")))))

;;; end of extent-tests.el
