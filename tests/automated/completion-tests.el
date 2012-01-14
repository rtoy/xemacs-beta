;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Aidan Kehoe <kehoea@parhasard.net>
;; Maintainers: Aidan Kehoe <kehoea@parhasard.net>
;; Created: 2012
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

;; This file tests pseudo-alist, obarray and hash-table arguments to
;; #'try-completion, #'all-completions and #'test-completion. It doesn't
;; test function arguments as COLLECTION.

(require 'cl)

(or (featurep 'xemacs)
    (defmacro Assert (assertion &optional failing-case)
      ;; This file can actually execute on GNU, though it exposes some bugs
      ;; as of So 1 Jan 2012 14:41:32 GMT, described in
      ;; http://mid.gmane.org/20224.27302.821804.284656@parhasard.net .
      `(condition-case err
           (assert ,assertion nil
                   ,@(if (memq (car-safe assertion)
                               '(eq eql equal equalp = string= < <= > >=))
                         (list
                          (concat (if failing-case
                                      (concat failing-case ", ")
                                    "")
                                  "%S should be `"
                                  (symbol-name (car assertion))
                                  "' to %S but isn't")
                          (cadr assertion)
                          (caddr assertion))
                       (list failing-case)))
           (error
            (message "error executing %S, %S, %S" ',assertion ,failing-case
                     err)))))

(let* ((strings '("del-alist" "delay-mode-hooks" "delete" "delete*"
		  "delete-and-extract-region" "delete-annotation"
		  "delete-auto-save-file-if-necessary" "delete-backward-char"
		  "delete-blank-lines" "delete-char"
		  "delete-completion-window" "delete-console"
		  "delete-debug-class-to-check" "delete-device"
		  "delete-directory" "delete-duplicates" "delete-dups"
		  "delete-extent" "delete-extract-rectangle" "delete-field"
		  "delete-file" "delete-forward-p" "delete-frame"
		  "delete-horizontal-space" "delete-if" "delete-if-not"
		  "delete-indentation" "delete-itimer" "delete-matching-lines"
		  "delete-menu-item" "delete-non-matching-lines"
		  "delete-other-frames" "delete-other-windows"
		  "delete-overlay" "delete-primary-selection" "delete-process"
		  "delete-rectangle" "delete-region" "delete-selection-mode"
		  "delete-text-in-column" "delete-to-left-margin"
		  "delete-window" "delete-windows-on" "delq" "remote-compile"
		  "remote-path-file-handler-function" "remove" "remove*"
		  "remove-alist" "remove-char-table" "remove-database"
		  "remove-directory" "remove-duplicates"
		  "remove-face-property" "remove-from-invisibility-spec"
		  "remove-glyph-property" "remove-gutter-element"
		  "remove-hook" "remove-if" "remove-if-not"
		  "remove-local-hook" "remove-message"
		  "remove-progress-feedback" "remove-range-table"
		  "remove-specifier"
		  "remove-specifier-specs-matching-tag-set-cdrs"
		  "remove-text-properties" "sublis"
		  "submenu-generate-accelerator-spec" "subr-arity"
		  "subr-interactive" "subr-max-args" "subr-min-args"
		  "subr-name" "subregexp-context-p" "subrp" "subseq" "subsetp"
		  "subsidiary-coding-system" "subst" "subst-char-in-region"
		  "subst-char-in-string" "subst-if" "subst-if-not"
		  "substitute" "substitute-command-keys" "substitute-env-vars"
		  "substitute-if" "substitute-if-not"
		  "substitute-in-file-name" "substitute-key-definition"
		  "substring" "substring-no-properties" "subtract-time"
		  "subwindow-height" "subwindow-image-instance-p"
		  "subwindow-width" "subwindow-xid" "subwindowp"))
       (list (let ((count -1))
	       (mapcar #'(lambda (string)
			   (incf count)
			   (case (% count 3)
			     (0 string)
			     (1 (cons (make-symbol string) nil))
			     (2 (cons string (make-symbol string))))) strings)))
       (vector (loop
		 for string in strings
		 with vector = (make-vector 511 0)
		 with count = -1
		 with symbol = nil
		 do
		 (setq symbol (intern string vector)
		       count (1+ count))
		 (case (% count 3)
		   (0 (set symbol nil))
		   (1 (fset symbol (symbol-function 'ignore)))
		   (2 (setf (symbol-plist symbol) 'hello)))
		 finally return vector))
       (init-hash-table
        #'(lambda ()
            (loop
              for string in strings
              with hash-table = (make-hash-table :test #'equal)
              with count = -1
              do
              (incf count)
              (case (% count 3)
                (0 (setf (gethash (make-symbol string) hash-table)
                         'hello))
                (1 (setf (gethash string hash-table) 'everyone))
                (2 (setf (gethash string hash-table) nil)))
              finally return hash-table)))
       (hash-table (funcall init-hash-table))
       ;; The following three could be circular lists, but that's not
       ;; portable to GNU.
       (list-list (make-list (length strings) list))
       (vector-list (make-list (length strings) vector))
       (hash-table-list (make-list (length strings) hash-table))
       scratch-hash-table cleared)
  (macrolet
      ((Assert-with-collections (assertion failing-case)
         `(progn
           (Assert ,(subst 'list 'collection assertion :test #'eq)
                   ,(replace-regexp-in-string "collection" "list" failing-case))
           (Assert ,(subst 'vector 'collection assertion :test #'eq)
                   ,(replace-regexp-in-string "collection" "vector"
                                              failing-case))
           (Assert ,(subst 'hash-table 'collection assertion :test #'eq)
                   ,(replace-regexp-in-string "collection" "hash-table"
                                              failing-case)))))
    ;; #'try-completion.
    (Assert (every #'try-completion strings list-list)
            "check #'try-completion gives no false negatives, list")
    (Assert (every #'try-completion strings vector-list)
            "check #'try-completion gives no false negatives, vector")
    (Assert (every #'try-completion strings hash-table-list)
            "check #'try-completion gives no false negatives, hash-table")
    (Assert-with-collections
     (null (try-completion "iX/ZXLwiOU+a " collection))
     "check #'try-completion with no match, collection")
    (Assert-with-collections
     (eq t (try-completion "delq" collection))
     "check #'try-completion with an exact match, collection")
    (Assert-with-collections
     (equal "delq"
	    (let ((completion-ignore-case t))
	      (try-completion "DElq" collection)))
     "check #'try-completion with a case-insensitive match, collection")
    (Assert-with-collections
     (equal "del" (try-completion "de" collection))
     "check #'try-completion where it needs to complete, collection")
    (Assert (equal "del" (try-completion "de" list #'consp))
	    "check #'try-completion, list, it needs to complete, predicate")
    (Assert
     (equal "del" (try-completion "de" vector #'fboundp))
     "check #'try-completion, vector, it needs to complete, predicate")
    (Assert
     (equal "del" (try-completion "de" hash-table #'(lambda (key value)
						      (eq 'everyone value))))
     "check #'try-completion, hash-table, it needs to complete, predicate")
    (Assert
     ;; The actual result here is undefined, the important thing is we don't
     ;; segfault.
     (prog1
         t
       (try-completion "de"
                       (setq cleared nil
                             scratch-hash-table (funcall init-hash-table))
                       #'(lambda (key value)
                           (if cleared
                               (eq 'everyone value)
                             (clrhash scratch-hash-table)
                             (garbage-collect)
                             (setq cleared t)))))
     "check #'try-completion doesn't crash when hash table modified")

    ;; #'all-completions
    (Assert (every #'all-completions strings list-list)
            "check #'all-completions gives no false negatives, list")
    (Assert (every #'all-completions strings vector-list)
            "check #'all-completions gives no false negatives, vector")
    (Assert (every #'all-completions strings hash-table-list)
            "check #'all-completions gives no false negatives, hash-table")
    (Assert-with-collections
     (null (all-completions "iX/ZXLwiOU+a " collection))
     "check #'all-completion with no match, collection")
    (Assert-with-collections
     (equal '("delq") (all-completions "delq" collection))
     "check #'all-completions with an exact match, collection")
    (Assert-with-collections
     (equal '("delq") (let ((completion-ignore-case t))
			(all-completions "dElQ" collection)))
     "check #'all-completions with a case-insensitive match, collection")
    (Assert
     (equal
      '("delay-mode-hooks" "delete-and-extract-region"
        "delete-backward-char" "delete-completion-window" "delete-device"
        "delete-dups" "delete-field" "delete-frame" "delete-if-not"
        "delete-matching-lines" "delete-other-frames"
        "delete-primary-selection" "delete-region" "delete-to-left-margin"
        "delq")
      (sort (all-completions "de" vector #'fboundp) #'string-lessp))
     "check #'all-completions where it need to complete, vector")
    (Assert
     (eql (length (all-completions "de" hash-table #'(lambda (key value)
                                                       (eq 'everyone value))))
          15)
     "check #'all-completions gives enough results with predicate, hash")
    (Assert
     (equal (sort
             (all-completions
              "de" list #'(lambda (object) (and (consp object)
                                                (null (cdr object)))))
             #'string-lessp)
            (sort
             (all-completions
              "de" hash-table #'(lambda (key value)
                                  (eq 'everyone value)))
             #'string-lessp))
     "check #'all-completion with complex predicates behaves well")
    (Assert-with-collections
     (equal (sort* (all-completions "" collection) #'string-lessp) strings)
     "check #'all-completions, empty string, with collection")
    (Assert
     ;; The actual result here is undefined, the important thing is we don't
     ;; segfault.
     (prog1
         t
       (all-completions "de"
                        (setq cleared nil
                              scratch-hash-table (funcall init-hash-table))
                        #'(lambda (key value)
                            (if cleared
                                (eq 'everyone value)
                              (clrhash scratch-hash-table)
                              (garbage-collect)
                              (setq cleared t)))))
     "check #'all-completions doesn't crash when hash table modified")
    ;; #'test-completion
    (Assert (every #'test-completion strings list-list)
            "check #'test-completion gives no false negatives, list")
    (Assert (every #'test-completion strings vector-list)
            "check #'test-completion gives no false negatives, vector")
    (Assert (every #'test-completion strings hash-table-list)
            "check #'test-completion gives no false negatives, hash-table")
    (Assert-with-collections
     (null (test-completion "iX/ZXLwiOU+a " collection))
     "check #'test-completion with no match, collection")
    (Assert-with-collections
     (eq t (test-completion "delq" collection))
     "check #'test-completion with an exact match, collection")
    (Assert-with-collections
     (null (let (completion-ignore-case) (test-completion "DElq" collection)))
     "check #'test-completion fails correctly if case-sensitive, collection")
    (Assert-with-collections
     (eq t (let ((completion-ignore-case t))
             (test-completion "DElq" collection)))
     "check #'test-completion with a case-insensitive match, collection")
    (Assert-with-collections
     (null (test-completion "de" collection))
     "check #'test-completion gives nil if no exact match, collection")
    (Assert (null (test-completion "de" list #'consp))
	    "check #'test-completion, list, no exact match, predicate")
    (Assert (eq t (test-completion "delete-matching-lines" list #'consp))
	    "check #'test-completion, list, exact match, predicate")
    (Assert (null (test-completion "de" vector #'fboundp))
	    "check #'test-completion, vector, no exact match, predicate")
    (Assert (eq t (test-completion "delete-to-left-margin" vector #'fboundp))
	    "check #'test-completion, vector, exact match, predicate")
    (Assert
     (null (test-completion "de" hash-table #'(lambda (key value)
                                                (eq 'everyone value))))
     "check #'test-completion, hash-table, it needs to complete, predicate")
    (Assert
     (eq t (test-completion "delete-frame" hash-table
                            #'(lambda (key value) (eq 'everyone value))))
     "check #'test-completion, hash-table, exact match, predicate")
    (Assert
     ;; The actual result here is undefined, the important thing is we don't
     ;; segfault.
     (prog1
         t
       (test-completion "delete-frame"
                        (setq cleared nil
                              scratch-hash-table (funcall init-hash-table))
                        #'(lambda (key value)
                            (if cleared
                                (eq 'everyone value)
                              (clrhash scratch-hash-table)
                              (garbage-collect)
                              (setq cleared t)))))
     "check #'all-completions doesn't crash when hash table modified")))

