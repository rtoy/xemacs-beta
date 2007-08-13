;;; ph.el --- Client for the CCSO directory system (aka PH/QI)

;; Copyright (C) 1997 Oscar Figueiredo

;; Author: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Created: May 1997
;; Version: $Revision: 1.2 $
;; Keywords: help

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; LCD Archive Entry (not registered yet):
;; ph|Oscar Figueiredo|Oscar.Figueiredo@di.epfl.ch|
;; Client for the CCSO directory system (aka PH/QI)|
;; 27-May-1997|Version $Revision: 1.2 $|ftp://(Not Available)

;;    This package provides functions to query CCSO nameservers through an
;;    interactive form or replace inline query strings in buffers with
;;    appropriately formatted query results (especially used to expand email
;;    addresses in message buffers). It also interfaces with the BBDB package
;;    to let you register entries of the CCSO directory into your own database.
;;    The CCSO white pages system was developped at UIUC and is in use in more
;;    than 300 sites in the world. The distribution can be found at
;;    ftp://uiarchive.cso.uiuc.edu/pub/packages/ph 
;;    Traditionally the server is called QI while the client is called PH.

;;; Installation:
;;    This package uses the custom and widget libraries. If they are not already 
;;    installed on your system get them from http://www.dina.kvl.dk/~abraham/custom/
;;    Then uncomment and add the following to your .emacs file:
;;      (require 'ph)
;;      (eval-after-load "message"
;;                       (define-key message-mode-map [(control ?c) (tab)] 'ph-expand-inline))
;;      (eval-after-load "mail"
;;                       (define-key mail-mode-map [(control ?c) (tab)] 'ph-expand-inline))

;;    This package should run under XEmacs 19.15 or 20 as well as under Emacs 19.34 and above

;;; Usage:
;;    * Provided you did the installation as proposed in the above section, 
;;      inline expansion will be available when you compose an email
;;      message. Type the name of somebody recorded in your PH/QI server and hit
;;      C-c TAB, this will overwrite the name with the corresponding email 
;;      address
;;    * M-x ph-customize to customize inline expansion and other features to
;;      your needs.
;;    * Look for the Ph submenu in the tools menu for more.

;;; Code:

(eval-when-compile
  (require 'wid-edit))
(require 'custom)
(if (not (fboundp 'make-overlay))
    (require 'overlay))
(if (locate-library "timer")
    (require 'timer))
(autoload 'custom-menu-create "cus-edit")

;;{{{      Package customization variables

(defgroup ph nil 
  "CCSO (PH/QI) directory system client"
  :group 'mail
  :group 'comm)

(defcustom ph-server nil
  "*The name or IP address of the CCSO (PH/QI) server.
A port number may be specified by appending a colon and a
number to the name of the server."
  :type  '(string :tag "Server")
  :group 'ph)

(defcustom ph-strict-return-matches t
  "*If non-nil, entries that do not contain all the requested return fields are ignored."
  :type  'boolean
  :group 'ph)

(defcustom ph-default-return-fields nil
  "*A list of the default fields to extract from CCSO entries.
If it contains `all' then all available fields are returned.
nil means return the default fields as configured in the server."
  :type  '(repeat (symbol :tag "Field name"))
  :group 'ph)

(defcustom ph-multiple-match-handling-method 'select
  "*What to do when multiple entries match a query for an inline expansion.
Possible values are: 
`first' (equivalent to nil) which means consider the first match.
`select' pop-up a selection buffer
`all' use all matches
`abort' the operation is aborted, an error is signaled"
  :type  '(choice :menu-tag "Method"
		  (const :menu-tag "First"  first)
		  (const :menu-tag "Select" select)
		  (const :menu-tag "All"    all)
		  (const :menu-tag "Abort"  abort)
		  (const :menu-tag "None" nil))
  :group 'ph)

(defcustom ph-duplicate-fields-handling-method 'list
  "*A method to handle entries containing duplicate fields.
This is either an alist (FIELD . METHOD) or a symbol METHOD.
The alist form of the variable associates a method to an individual field,
the second form specifies a method applicable to all fields.
Available methods are:
`list' or nil lets the value of the field be a list of values
`first' keeps the first value and discards the others,
`concat' concatenates the values into a single multiline string,
`duplicate' duplicates the entire entry into as many instances as 
different values."
  :type '(choice (const :menu-tag "List" list)
		 (const :menu-tag "First" first)
		 (const :menu-tag "Concat" concat)
		 (const :menu-tag "Duplicate" duplicate)
		 (repeat :menu-tag "Per Field Specification"
			 :tag "Per Field Specification"
			 (cons :tag "Field/Method"
			       :value (nil . list)
			       (symbol :tag "Field name")
			       (choice :tag "Method"
				       :menu-tag "Method"
				       (const :menu-tag "List" list)
				       (const :menu-tag "First" first)
				       (const :menu-tag "Concat" concat)
				       (const :menu-tag "Duplicate" duplicate)))))
  :group 'ph
)

(defcustom ph-inline-query-format-list nil
  "*Format of an inline expansion query.
If the inline query string consists of several words, this list specifies 
how these individual words are associated to CCSO database field names.
If nil all the words will be mapped onto the default CCSO database key."
  :type  '(repeat (symbol :tag "Field name"))
  :group 'ph)

(defcustom ph-expanding-overwrites-query t
  "*If non nil, expanding a query overwrites the query string"
  :type  'boolean
  :group 'ph)

(defcustom ph-inline-expansion-format '("%s" email)
  "*A list specifying the format of the expansion of inline queries.
This variable controls what ph-expand-inline actually inserts in the buffer.
First element is a string passed to format. Remaining elements are symbols
indicating CCSO database field names, corresponding field values are passed
as additional arguments to format."
  :type  '(list (string :tag "Format String")
		(repeat :inline t
			:tag "Field names"
			(symbol :tag "")))
  :group 'ph)

(defcustom ph-form-fields '(name firstname email phone)
  "*A list of fields presented in the query form."
  :tag   "Default Fields in Query Forms"
  :type  '(repeat (symbol :tag "Field name"))
  :group 'ph)

(defcustom ph-fieldname-formstring-alist '((url . "URL")
					    (unix_gid . "Unix GID")
					    (unix_uid . "Unix UID")
					    (unit_code . "Unit Code")
					    (department_code . "Department Code")
					    (high_school . "High School")
					    (home_phone . "Home Phone")
					    (office_phone . "Office Phone")
					    (callsign . "HAM Call Sign")
					    (office_address . "Office Address")
					    (office_location . "Office Location")
					    (id . "ID")
					    (email . "E-Mail")
					    (firstname . "First Name"))
  "*A mapping of CCSO database field names onto prompt strings used in query/response forms.
Prompt strings for fields that are not in this are derived by capitalizing
the field name."
  :tag   "Mapping of Field Names onto Prompt Strings"
  :type  '(repeat (cons :tag "Field"
			(symbol :tag "Name")
		        (string :tag "Prompt string")))
  :group 'ph)

(defcustom ph-bbdb-mapping-alist '((name . (firstname name))
				    (email . net))
  "*A mapping of CCSO database field names onto BBDB field names"
  :tag "CCSO to BBDB Field Name Mapping"
  :type '(repeat (cons :tag "Field Name"
		       (symbol :tag "CCSO")
		       (sexp :tag "BBDB")))
  :group 'ph)

(defcustom ph-options-file "~/.emacs"
  "*A file where the servers hotlist is stored. 
It should be loaded automatically at startup so ~/.emacs is a reasonable
choice."
  :type '(file :Tag "File Name:"))

;;}}}


;;{{{      Internal cooking

(defconst ph-xemacs-p (string-match "XEmacs" emacs-version))
(defconst ph-fsfemacs-p (not ph-xemacs-p))
(defconst ph-xemacs-mule-p (and ph-xemacs-p
				 (featurep 'mule)))
(defconst ph-fsfemacs-mule-p (and ph-fsfemacs-p
				   (featurep 'mule)))

(defvar ph-server-hotlist nil)

(defconst ph-default-server-port 105
  "Default TCP port for CCSO directory services")

(defvar ph-form-widget-list nil)
(defvar ph-process-buffer nil)
(defvar ph-read-point)

(defun ph-display-records (records &optional raw-field-names)
  "Display the record list RECORDS in a formatted buffer. 
If RAW-FIELD-NAMES is non nil, no translation to form strings or 
capitalization is done on field names."
  (let ((buffer (get-buffer-create "*PH Query Results*"))
	inhibit-read-only
	precords
	(width 0)
	beg field-beg
	field-name)
    (switch-to-buffer buffer)    
    (setq buffer-read-only t)
    (setq inhibit-read-only t)
    (erase-buffer)
    (kill-all-local-variables)
    (insert "PH Query Result\n")
    (insert "===============\n\n\n")
    (if (null records)
	(insert "No match found.\n"
		(if ph-strict-return-matches
		    "Try setting ph-strict-return-matches to nil or change ph-default-return-fields."
		  ""))
      ;; Replace field names with prompt strings, compute prompt max width
      (setq precords
	    (mapcar 
	     (function
	      (lambda (record)
		(mapcar 
		 (function
		  (lambda (field)
		    (setq field-name (if raw-field-names
					 (symbol-name (car field))
				       (or (and (assq (car field) ph-fieldname-formstring-alist)
						(cdr (assq (car field) ph-fieldname-formstring-alist)))
					   (capitalize (symbol-name (car field))))))
		    (if (> (length field-name) width)
			(setq width (length field-name)))
		    (cons field-name (cdr field))))
		 record)))
	     records))
      (mapcar (function
	       (lambda (record)
		 (setq beg (point))
		 ;; Actually insert the field/value pairs
		 (mapcar (function
			  (lambda (field)
			    (setq field-beg (point))
			    (insert (format (concat "%" width "s: ") (car field)))
			    (put-text-property field-beg (point) 'face 'bold)
			    (mapcar (function 
				     (lambda (val)
				       (indent-to (+ 2 width))
				       (insert val "\n")))
				    (if (stringp (cdr field))
					(split-string (cdr field) "\n")
				      (cdr field)))))
			 record)
		 ;; Store the record internal format in some convenient place
		 (overlay-put (make-overlay beg (point))
			      'ph-record
			      (car records))
		 (setq records (cdr records))
		 (insert "\n")))
	      precords))
    (insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (ph-query-form))
		   "New query")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-this-buffer))
		   "Quit")
    (use-local-map widget-keymap)
    (widget-setup)      
    )
)

(defun ph-process-form ()
  "Process the form in current buffer and display the results"
  (let (query-alist
	value)
    (if (not (and (boundp 'ph-form-widget-list)
		  ph-form-widget-list))
	(error "Not in a PH query form buffer")
      (mapcar (function 
	       (lambda (wid-field)
		 (setq value (widget-value (cdr wid-field)))
		 (if (not (string= value ""))
		     (setq query-alist (cons (cons (car wid-field) value)
					     query-alist)))))
	      ph-form-widget-list)
      (kill-buffer (current-buffer))
      (ph-display-records (ph-query-internal query-alist))
    )))
			   
					 
(defun ph-query-internal (query &optional return-fields)
  "Query the PH/QI server with QUERY.
QUERY can be a string NAME or a list made of strings NAME 
and/or cons cells (KEY . VALUE) where KEYs should be valid 
CCSO database keys. NAME is equivalent to (DEFAULT . NAME) where 
DEFAULT is the default key of the database) 
RETURN-FIELDS is a list of database fields to return defaulting to 
ph-default-return-fields."
  (let (request)
    (if (null return-fields)
	(setq return-fields ph-default-return-fields))
    (setq request 
	  (concat "query "
		  (if (stringp query)
		      query
		    (mapconcat (function (lambda (elt)
					   (if (stringp elt) elt)
					   (format "%s=%s" (car elt) (cdr elt))))
			       query
			       " "))
		  (if return-fields
		      (concat " return " (mapconcat 'symbol-name return-fields " ")))))
    (and (> (length request) 6)
	 (ph-do-request request)
	 (ph-parse-query-result return-fields))))

(defun ph-parse-query-result (&optional fields)
  "Return a list of alists of key/values from the record in ph-process-buffer. 
Fields not in FIELDS are discarded."
  (let (record records
	line-regexp
	current-key key value
	ignore)
    (save-excursion
      (message "Parsing results...")
      (set-buffer ph-process-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(-[0-9]+\\):\\([0-9]+\\):" nil t)
	(catch 'ignore
	  (setq line-regexp (concat "^\\(-[0-9]+\\):" (match-string 2) ":[ \t]*\\([-a-zA-Z_]*\\)?:[ \t]*\\(.*\\)$"))
	  (beginning-of-line)
	  (setq record nil
		ignore nil
		current-key nil)
	  (while (re-search-forward line-regexp nil t)
	    (catch 'skip-line
	      (if (string= "-508" (match-string 1))
		  ;; A field is missing in this entry. Skip it or skip the
		  ;; whole record (see ph-strict-return-matches)
		  (if (not ph-strict-return-matches)
		      (throw 'skip-line t)
		    (while (re-search-forward line-regexp nil t))
		    (setq ignore t)
		    (throw 'ignore t)))
	      (setq key   (and (not (string= (match-string 2) ""))
			       (intern (match-string 2)))
		    value (match-string 3))
	      (if (and current-key
		       (eq key current-key)) 
		  (setq key nil)
		(setq current-key key))
	      (if (or (null fields)
		      (memq 'all fields)
		      (memq current-key fields))
		  (if key
		      (setq record (cons (cons key value) record))
		    (setcdr (car record) (cons value (if (listp (cdar record))
							 (cdar record)
						       (cons (cdar record) nil)))))))))
	(and (not ignore)
	     (or (null fields)
		 (memq 'all fields)
		 (setq record (nreverse record)))
	     (setq record (if (not (eq 'list ph-duplicate-fields-handling-method))
			       (ph-filter-duplicate-fields record)
			     (list record)))
	     (setq records (append record records))))
      )
    (message "Done")
    records)
  )

(defun ph-filter-duplicate-fields (record)
  "Filter RECORD according to ph-duplicate-fields-handling-method."
  (let ((rec record)
	unique
	duplicates
	result)

    ;; Search for multiple records
    (while (and rec
		(not (listp (cdar rec))))
      (setq rec (cdr rec)))

    (if (null (cdar rec))
	(list record)			; No duplicate fields in this record
      (mapcar (function 
	       (lambda (field)
		 (if (listp (cdr field))
		     (setq duplicates (cons field duplicates))
		   (setq unique (cons field unique)))))
	      record)
      (setq result (list unique))
      (mapcar (function
	       (lambda (field)
		 (let ((method (if (consp ph-duplicate-fields-handling-method)
				   (cdr (assq (car field) ph-duplicate-fields-handling-method))
				 ph-duplicate-fields-handling-method)))
		 (cond
		  ((or (null method) (eq 'list method))
		   (setq result 
			 (ph-add-field-to-records field result)))
		  ((eq 'first method)
		   (setq result 
			 (ph-add-field-to-records (cons (car field) (cadr field)) result)))
		  ((eq 'concat method)
		   (setq result 
			 (ph-add-field-to-records (cons (car field)
							 (mapconcat 
							  'identity
							  (cdr field)
							  "\n")) result)))
		  ((eq 'duplicate method)
		   (setq result
			 (ph-distribute-field-on-records field result)))))))
	      duplicates)
      result)))
			    
(defun ph-add-field-to-records (field records)
  "Add FIELD to each individual record in RECORDS and return the resulting list."
  (mapcar (function
	   (lambda (r)
	     (cons field r)))
	  records))

(defun ph-distribute-field-on-records (field records)
  "Duplicate each individual record in RECORDS according to value of FIELD.
Each copy is added a new field containing one of the values of FIELD."
  (let (result
	(values (cdr field)))
    ;; Uniquify values first
    (while values
      (setcdr values (delete (car values) (cdr values)))
      (setq values (cdr values)))
    (mapcar (function
	     (lambda (value)
	       (let ((result-list (copy-sequence records)))
		 (setq result-list (ph-add-field-to-records (cons (car field) value)
							      result-list))
		 (setq result (append result-list result))
	       )))
	    (cdr field))
    result)
)

(defun ph-do-request (request)
  "Send REQUEST to the server. Wait for response and return the buffer containing it."
  (let (process
	buffer)
    (unwind-protect
	(progn
	  (message "Contacting server...")
	  (setq process (ph-open-session))
	  (if process
	      (save-excursion 
		(set-buffer (setq buffer (process-buffer process)))
		(ph-send-command process request)
		(message "Request sent, waiting for reply...")
		(ph-read-response process))))
      (if process
	  (ph-close-session process)))
    buffer))
	      
(defun ph-open-session (&optional server)
  "Open a connection to the given CCSO SERVER.
SERVER is either a string naming the server or a list (NAME PORT)."
  (let (process
	host
	port)
    (catch 'done
      (if (null server)
	  (setq server (or ph-server
			   (call-interactively 'ph-set-server))))
      (string-match "\\(.*\\)\\(:\\(.*\\)\\)?" server)
      (setq host (match-string 1 server))
      (setq port (or (match-string 3 server)
		     ph-default-server-port))
      (setq ph-process-buffer (get-buffer-create (format " *PH-%s*" host)))
      (save-excursion
	(set-buffer ph-process-buffer)
	(erase-buffer)
	(setq ph-read-point (point))
	(and ph-xemacs-mule-p
	     (set-buffer-file-coding-system 'binary t)))
      (setq process (open-network-stream "ph" ph-process-buffer host port))
      (if (null process)
	  (throw 'done nil))
      (process-kill-without-query process)
      process)))


(defun ph-close-session (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (ph-send-command process "quit")
    (ph-read-response process)
    (if (fboundp 'add-async-timeout)
	(add-async-timeout 10 'delete-process process)
      (run-at-time 2 nil 'delete-process process))))

(defun ph-send-command (process command)
  (goto-char (point-max))
  (process-send-string process command)
  (process-send-string process "\r\n")
)

(defun ph-read-response (process &optional return-response)
  "Read a response from the PH/QI query process PROCESS.
Returns nil if response starts with an error code. If the
response is successful the return code or the reponse itself is returned
depending on RETURN-RESPONSE"
  (let ((case-fold-search nil)
	return-code
	match-end)
    (goto-char ph-read-point)
    ;; CCSO protocol : response complete if status >= 200
    (while (not (re-search-forward "^\\(^[2-5].*\\):.*\n" nil t))
      (accept-process-output process)
      (goto-char ph-read-point))
    (setq match-end (point))
    (goto-char ph-read-point)
    (if (and (setq return-code (match-string 1))
	     (setq return-code (string-to-number return-code))
	     (>= (abs return-code) 300))
	(progn (setq ph-read-point match-end) nil)
      (setq ph-read-point match-end)
      (if return-response
	  (buffer-substring (point) match-end)
	return-code))))

;;; FSF Emacs does not provide that one
(if (not (fboundp 'split-string))
    (defun split-string (string pattern)
      "Return a list of substrings of STRING which are separated by PATTERN."
      (let (parts (start 0))
	(while (string-match pattern string start)
	  (setq parts (cons (substring string start (match-beginning 0)) parts)
		start (match-end 0)))
	(nreverse (cons (substring string start) parts))
	)))

;;}}}				 

;;{{{      High-level interfaces (interactive functions)

(defun ph-customize ()
  "Customize the PH package."
  (interactive)
  (customize 'ph))

(defun ph-set-server (server)
  "Set the server to SERVER."
  (interactive "sNew PH/QI Server: ")
  (setq ph-server server)
  (message "Selected PH/QI server is now %s" server))

(defun ph-get-email (name)
  "Get the email field of NAME from the PH/QI directory server."
  (interactive "sName: ")
  (let ((email (cdaar (ph-query-internal name '(email)))))
    (if (interactive-p)
	(if email
	    (message "%s" email)
	  (message "No record matching %s" name)))
    email))

(defun ph-get-phone (name)
  "Get the phone field of NAME from the PH/QI directory server."
  (interactive "sName: ")
  (let ((phone (cdaar (ph-query-internal name '(phone)))))
    (if (interactive-p)
	(if phone
	    (message "%s" phone)
	  (message "No record matching %s" name)))
    phone))

(defun ph-get-field-list ()
  "Return a list of valid field names for current server.
When called interactively the list is formatted in a dedicated buffer
otherwise a list of symbols is returned."
  (interactive)
  (ph-do-request "fields")
  (if (interactive-p)
      (let ((ph-duplicate-fields-handling-method 'list))
	(ph-display-records (ph-parse-query-result) t))
    (mapcar 'caar 
	    (ph-parse-query-result)))
)

(defun ph-expand-inline (&optional replace)
  "Query the server and expand the query string before point.
The query string consists of the buffer substring from the point back to
the preceding comma, colon or beginning of line. If it consists of more than
one word the variable ph-inline-query-format-list controls how these are mapped
onto CCSO database field names.
After querying the server for the given string, the expansion specified by 
ph-inline-expansion-format is inserted in the buffer at point. If REPLACE is t 
then this expansion replaces the name in the buffer.
If ph-expanding-overwrites-query is t then the meaning of REPLACE is inverted."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(if (re-search-backward "[:,][ \t]*" 
					(save-excursion
					  (beginning-of-line)
					  (point))
					'move)
		    (goto-char (match-end 0)))
		(point)))
	 (words (buffer-substring beg end))
	 query
	 query-alist
	 (query-format ph-inline-query-format-list)
	 response
	 response-strings
	 key val cell)
    
    ;; Prepare the query
    (if (or (not query-format)
	    (not (string-match "[ \t]+" words)))
	(setq query words)
      (setq words (split-string words "[ \t]+"))
      (while (and words query-format)
	(setq query-alist (cons (cons (car query-format) (car words)) query-alist))
	(setq words (cdr words)
	      query-format (cdr query-format)))
      (if words
	  (setcdr (car query-alist)
		  (concat (cdar query-alist) " "
			  (mapconcat 'identity words " "))))
      ;; Uniquify query-alist
      (setq query-alist (nreverse query-alist))
      (while query-alist
	(setq key (caar query-alist)
	      val (cdar query-alist)
	      cell (assq key query))
	(if cell
	    (setcdr cell (concat val " " (cdr cell)))
	  (setq query (cons (car query-alist) query))))
      (setq query-alist (cdr query-alist)))

    (setq response (ph-query-internal query (cdr ph-inline-expansion-format)))

    (if (null response)
	(error "No match found")

      ;; Process response through ph-inline-expansion-format
      (while response
	(setq response-strings
	      (cons (apply 'format 
			   (car ph-inline-expansion-format)
			   (mapcar (function 
				    (lambda (field)
				      (or (cdr (assq field (car response))) 
					  "")))
				   (cdr ph-inline-expansion-format)))
		    response-strings))
	(setq response (cdr response)))

      (if (or
	   (and replace (not ph-expanding-overwrites-query))
	   (and (not replace) ph-expanding-overwrites-query))
	  (delete-region beg end))
      (cond 
       ((or (= (length response-strings) 1)
	    (null ph-multiple-match-handling-method)
	    (eq ph-multiple-match-handling-method 'first))
	(insert (car response-strings)))
       ((eq ph-multiple-match-handling-method 'select)
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list response-strings)))
       ((eq ph-multiple-match-handling-method 'all)
	(insert (mapconcat 'identity response-strings ", ")))
       ((eq ph-multiple-match-handling-method 'abort)
	(error "There is more than one match for the query"))
       ))
    )
)

(defun ph-query-form (&optional get-fields-from-server)
  "*Display a form to query the CCSO PH/QI nameserver.
If given a non-nil argument the function first queries the  server 
for the existing fields and displays a corresponding form."
  (interactive "P")
  (let ((fields (or (and get-fields-from-server
			 (ph-get-field-list))
		    ph-form-fields))
	(buffer (get-buffer-create "*PH/QI Query Form*"))
	field-name
	widget
	(width 0)
	inhibit-read-only
	pt)
    (switch-to-buffer buffer)
    (setq inhibit-read-only t)
    (erase-buffer)
    (kill-all-local-variables)
    (make-local-variable 'ph-form-widget-list)
    (widget-insert "PH/QI Query Form\n")
    (widget-insert "================\n\n")
    (widget-insert "Current server is: " (or ph-server
					     (call-interactively 'ph-set-server)) "\n")
    ;; Loop over prompt strings to find the biggest one
    (setq fields 
	  (mapcar (function
		   (lambda (field)
		     (setq field-name (or (and (assq field ph-fieldname-formstring-alist)
					       (cdr (assq field ph-fieldname-formstring-alist)))
					  (capitalize (symbol-name field))))
		     (if (> (length field-name) width)
			 (setq width (length field-name)))
		     (cons field field-name)))
		  fields))
    ;; Insert the first widget out of the mapcar to leave the cursor 
    ;; in the first field 
    (widget-insert "\n\n" (format (concat "%" width "s: ") (cdr (car fields))))
    (setq pt (point))
    (setq widget (widget-create 'editable-field :size 15))
    (setq ph-form-widget-list (cons (cons (car (car fields)) widget)
				     ph-form-widget-list))
    (setq fields (cdr fields))
    (mapcar (function
	     (lambda (field)
	       (widget-insert "\n\n" (format (concat "%" width "s: ") (cdr field)))
	       (setq widget (widget-create 'editable-field
					   :size 15))
	       (setq ph-form-widget-list (cons (cons (car field) widget)
						ph-form-widget-list))))
	    fields)
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (ph-process-form))
		   "Query Server")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (ph-query-form))
		   "Reset Form")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-this-buffer))
		   "Quit")
    (goto-char (1+ pt)) ; 1+ for some extent boundary reason
    (use-local-map widget-keymap)
    (widget-setup))
  )

(defun ph-bookmark-server (server)
  "Add SERVER to the servers' hotlist."
  (interactive "sServer: ")
  (if (member server ph-server-hotlist)
      (error "%s is already in the hotlist" server)
    (setq ph-server-hotlist (cons server ph-server-hotlist))
    (ph-install-menu)
    (ph-save-hotlist)))

(defun ph-bookmark-current-server ()
  "Add current server to the servers' hotlist."
  (interactive)
  (ph-bookmark-server ph-server))

(defun ph-save-hotlist ()
  "Save the servers hotlist to ph-options-file"
  (save-excursion
    (set-buffer (find-file-noselect ph-options-file))
    ;; delete the previous setq
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (if (and (listp sexp)
		   (eq (car sexp)  'setq)
		   (eq (cadr sexp) 'ph-server-hotlist))
	      (progn
		(delete-region (save-excursion
				 (backward-sexp)
				 (point))
			       (point))
		(throw 'found t))))))
    (let ((standard-output (current-buffer)))
      (if (not (bolp))
	  (princ "\n"))
      (princ "(setq ph-server-hotlist '")
      (prin1 ph-server-hotlist)
      (princ ")\n"))
    (save-buffer))
)

;;}}}

;;{{{      Menu interface

(require 'easymenu)

(defconst ph-tail-menu 
  `(["---" nil nil]
    ["Query Form" ph-query-form t]
    ["Expand Inline" ph-expand-inline t]
    ["---" nil nil]
    ["Get Email" ph-get-email t]
    ["Get Phone" ph-get-phone t]
    ["List Valid Field Names" ph-get-field-list t]
    ["---" nil nil]    
    ,(cons "Customize" (cdr (custom-menu-create 'ph)))))

(defconst ph-server-menu 
  '(["---" ph-bookmark-server t]
    ["Bookmark Current Server" ph-bookmark-current-server t]
    ["New Server" ph-set-server t]))


(defun ph-menu ()
  (let (command)
    (append '("Ph")
	    (list
	     (append '("Server")
		     (mapcar (function 
			      (lambda (server)
				(setq command (intern (concat "ph-set-server-" server)))
				(if (not (fboundp command))
				    (fset command `(lambda ()
						     (interactive)
						     (setq ph-server ,server)
						     (message "Selected PH/QI server is now %s" ,server))))
				(vector server command t)))
			     ph-server-hotlist)
		     ph-server-menu))
	    ph-tail-menu)))

(defun ph-install-menu ()
  (cond 
   (ph-xemacs-p
    (add-submenu '("Tools") (ph-menu)))
   (ph-fsfemacs-p
    (define-key 
      global-map
      [menu-bar tools ph] 
      (cons "Ph"
	    (easy-menu-create-keymaps "Ph" (cdr (ph-menu))))))
   ))

(ph-install-menu)
  
      
;;}}}

(provide 'ph)

;;; ph.el ends here
