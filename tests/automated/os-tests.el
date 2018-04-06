;;; os-tests.el --- test support for OS interaction

;; Copyright (C) 2004 Free Software Foundation

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Maintainer: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2004 October 28
;; Keywords: tests, process support

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

;; Test OS support.  Processes, environment variables, etc.
;; See test-harness.el for instructions on how to run these tests.

;; call-process-region bug reported by Katsumi Yamaoka on 2004-10-26
;; in <b9yvfcyuscf.fsf@jpl.org>, who suggested the basic test scheme
;; in <b9yoeipvwn0.fsf@jpl.org>.

;; tac works by lines, unfortunately
(macrolet
    ((handle-call-process-cases (program &rest cases)
       (cons
	'progn
	(loop for (pos . result) in cases
	      nconc `((erase-buffer)
		      (insert "a\nb\nc\nd\n")
		      (goto-char ,pos)
		      (Assert (eql (call-process-region 3 7 ,program t t) 0)
		       ,(concat "failed calling " program))
		      (goto-char (point-min))
		      (Assert (equal (buffer-string) ,result)
		       ,(format "test call-process-region, %s, pos %d, "
				program pos)))))))
  (with-temp-buffer 
    (Skip-Test-Unless
     (condition-case nil (call-process "tac") (process-error nil))
     "tac executable not found"
     "Tests of call-process-region with region deleted after inserting
tac process output."
     (handle-call-process-cases "tac"
				(1 . "c\nb\na\nd\n")
				(3 . "a\nc\nb\nd\n")
				(5 . "a\nc\nb\nd\n")
				(7 . "a\nc\nb\nd\n")
				(9 . "a\nd\nc\nb\n")))
    ;; if you're in that much of a hurry you can blow cat off
    ;; if you've done tac, but I'm not going to bother
    (Skip-Test-Unless
     (condition-case nil (call-process "cat") (process-error nil))
     "cat executable not found"
     "Tests of call-process-region with region deleted after inserting
cat process output."
     (handle-call-process-cases "cat"
				(1 . "b\nc\na\nd\n")
				(3 . "a\nb\nc\nd\n")
				(5 . "a\nb\nc\nd\n")
				(7 . "a\nb\nc\nd\n")
				(9 . "a\nd\nb\nc\n")))))

(loop
  with envvar-not-existing = (symbol-name (gensym "whatever"))
  with envvar-existing = (symbol-name (gensym "whatever"))
  with envvar-existing-val = (make-string #x10000 ?\xe1)
  with examples = 
  (list (list (format "%chome%cwhatever%c%chi-there%c$%s"
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      envvar-existing)
              (format "%chi-there%c%s"
                      directory-sep-char
                      directory-sep-char
                      envvar-existing-val))
        (if (memq system-type '(windows-nt cygwin32))
            '("//network-path/c$" "//network-path/c$")
          '("/network-path/c$" "/network-path/c$"))
        (list (format "/home/whoever/$%s" envvar-not-existing)
              (format "/home/whoever/$%s" envvar-not-existing))
        (list (format "/home/whoever/$%s" envvar-existing)
              (format "/home/whoever/%s" envvar-existing-val))
        (list (format "/home/whoever/${%s}" envvar-existing)
              (format "/home/whoever/%s" envvar-existing-val))
        (list (format "/home/whoever/${%s}" envvar-not-existing)
              (format "/home/whoever/${%s}" envvar-not-existing)))
  initially (progn (setenv envvar-not-existing nil t)
                   (setenv envvar-existing envvar-existing-val))
  for (pre post)
  in examples
  do (Assert (string= post (substitute-in-file-name pre)))
  ;; Be polite and don't overrun ARG_MAX for any processes called down
  ;; the line.
  finally (setenv envvar-existing nil t))

;; Check some restrictions introduced to the ZONE argument to #'encode-time.

(Check-Error args-out-of-range (encode-time 24 4 20 11 5 2017 -86401))
(Assert (equal (encode-time 24 4 20 11 5 2017 -86400)
               '(22806 5448))) ;; "05/12/17 09:04:25 PM"
(Assert (equal (encode-time 24 4 20 11 5 2017 86400)
               '(22803 29256))) ;; "05/10/17 09:04:24 PM"
(Check-Error args-out-of-range (encode-time 24 4 20 11 5 2017 86401))

;;; end of os-tests.el
