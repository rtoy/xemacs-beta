;;!emacs
;;
;; FILE:         hsite.el
;; SUMMARY:      Site-specific setup for Hyperbole
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, local
;;
;; AUTHOR:       Bob Weiner
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:    15-Apr-91 at 00:48:49
;; LAST-MOD:     17-Feb-97 at 18:34:17 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1997, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   See the "README" file for installation instructions.
;;
;;   "hsite.el" may be byte-compiled if you like but normally it is not.
;;
;;   Be sure to have users load any personal mail/news initializations
;;   before they load this file if any of Hyperbole's mail or news
;;   support features are enabled either herein or within their personal
;;   Hyperbole initializations.  Otherwise, the mail/news support may
;;   not be configured properly.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Read the comments and modify as desired.
;;; ************************************************************************

(message "Initializing Hyperbole, please wait...")

;;; ************************************************************************
;;; TIMEZONE SETTING
;;; ************************************************************************

;; The following section applies only to MS-DOS and MS-Windows OSs.
;; For such OSs, you must configure this section or you will receive
;; an error when starting Hyperbole.  Users of other OSs may simply
;; ignore this section.


;; Microcruft OSs don't provide an automatically set timezone environment
;; variable.  Nor do they include a UNIX-style date program.  So follow
;; the commented instructions in the code below here.

;; If you happened to have installed a UNIX-style date program (when you type
;; `date' at a shell, it simply spits out the date and time and then quits),
;; you may comment out the logic.
;;
(if (and hyperb:microcruft-os-p
	 (not (or (getenv "TZ") (getenv "TIMEZONE"))))
    (progn
      ;; Comment out the following `error' line...
      (error "(hsite.el): Configure the TIMEZONE SETTING section in this file.")
      ;; ... and uncomment the following line, substituting an appropriate
      ;;     timezone from the list in the variable, `htz:world-timezones'
      ;;     in the file, "htz.el".
      ;;   (setenv "TZ" "your-3char-timezone")
      ))

;;; ************************************************************************
;;; SMART SETTINGS
;;; ************************************************************************

(defvar hkey-always-display-menu nil
  "*Non-nil means always display the Smart Menu window when the Action or Assist Key is pressed and the Smart Menu system has been loaded.
If a Smart Menu is already displayed, perform another Action or Assist Key
function.")

(defvar smart-scroll-proportional t
  "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful.")

;;; ************************************************************************
;;; HYPERBOLE DIRECTORY SETTING
;;; ************************************************************************

(require 'hyperbole)

;;; ************************************************************************
;;; INTERNET SETTINGS
;;; ************************************************************************

;; String to be used in the call: (hpath:rfc rfc-num) to create a remote
;; path to the RFC document for `rfc-num'.  Uncomment and alter this setting
;; if another site is closer for you.
;; (setq hpath:rfc "/anonymous@ds.internic.net:rfc/rfc%s.txt")

;; When a user creates an explicit button, Hyperbole tries to store her
;; Internet e-mail address with the button by using the formula, email-id =
;; <user-id>@<domainname>.  Not every system has its domainname set
;; up properly, however.  If you do a {M-x load-file hypb.elc RET} and then
;; hit {C-x C-e} after the closing paren of the following function,
;; (hypb:domain-name), you will see whether or not yours is configured
;; properly.  If it is not, uncomment the following line and set it to the
;; proper value.

;; (setenv "DOMAINNAME" "yourdomain.com")

;;; ************************************************************************
;;; XEMACS, GNU EMACS 19, AND EPOCH CONFIGURATION
;;; ************************************************************************

;; No-op unless set by one of the conditionals below.
(defun hui:but-flash ())

(if (and hyperb:emacs19-p window-system)
    (progn
      (require 'hui-em19-b)
      ;; Highlight explicit buttons whenever a file is read in.
      (var:append 'find-file-hooks '(hproperty:but-create))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;;
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-em19-b.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;;
      ;; Non-nil means visually emphasize that button under mouse cursor is
      ;; selectable.
      (setq hproperty:but-emphasize-p nil)
      ;;
      ;; If you find that the Hyperbole button flash time is too slow
      ;; or too fast, adjust it here.
      (setq hproperty:but-flash-time 1000)
      ))

(if (and hyperb:lemacs-p (not noninteractive))
    (progn
      (require 'hui-xe-but)
      ;;
      ;; If running XEmacs 19.8 or below, don't highlight explicit buttons
      ;; whenever a file is read in since this can cause a sporadic crash
      ;; when find-files are done.
      (if hyperb:kotl-p (var:append 'find-file-hooks '(hproperty:but-create)))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;;
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-xe-but.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;;
      ;; Non-nil means visually emphasize that button under mouse cursor is
      ;; selectable.
      (setq hproperty:but-emphasize-p nil)
      ;;
      ;; If you find that the Hyperbole button flash time is too slow
      ;; or too fast, adjust it here.
      (setq hproperty:but-flash-time 1000)
      ))

(if (and hyperb:epoch-p (string= hyperb:epoch-p "V4"))
    (progn
      (require 'hui-epV4-b)
      ;; Highlight explicit buttons whenever a file is read in.
      (var:append 'find-file-hooks '(hproperty:but-create))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-epV4-b.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;; If you use Epoch and find that the Hyperbole button flash time is
      ;; too slow or too fast, adjust it here.
      (defvar hproperty:but-flash-time 1000
	"Machine specific value for empty loop counter, Epoch but flash delay.")
      ))

;;; ************************************************************************
;;; EXTERNAL SYSTEM ENCAPSULATIONS
;;; ************************************************************************

;;; Support for encapsulations of any of these external systems may be
;;; enabled here.  You should be familiar with the external system before
;;; you try to use the Hyperbole support for it.
;;; Possible system encapsulations to include within the innermost set of
;;; parentheses are:
;;;   hsys-wais hsys-hbase
;;; See files with the same name, e.g. "hsys-wais.el" for details on each
;;; system.
;;;
;;; Note: hsys-w3 is automatically loaded by default by Hyperbole.
(setq hibtypes:begin-load-hook
      (list (function (lambda () (mapcar 'require '())))))

;;; ************************************************************************
;;; ONLINE LIBRARY CONFIGURATION
;;; ************************************************************************

;;; Support for online library document id references is loaded here but
;;; requires some additional configuration before use.  See the DESCRIPTION
;;; section in "hib-doc-id.el" for complete installation and use information.
;;;
(setq hibtypes:end-load-hook
      (list (function (lambda () (mapcar 'require '(hib-doc-id))))))

;;; ************************************************************************
;;; FILE VIEWER COMMAND SETTINGS
;;; ************************************************************************

(defvar hpath:display-alist
  (let ((info-suffix "\\.info\\(-[0-9]+\\)?\\(\\.gz\\|\\.Z\\|-z\\)?$"))
    (list
     ;; Run the OO-Browser on OOBR or OOBR-FTR Environment files.
     '("OOBR\\(-FTR\\)?$" . br-env-browse)
     ;; Display the top node from Info online manuals.
     (cons
      (concat (` (, info-suffix)) "\\|/info/[^.]+$\\|/info-local/[^.]+$")
      (` (lambda (file)
	   (if (and (string-match (, info-suffix) file)
		    (match-beginning 1))
	       ;; Removed numbered trailer to get basic filename.
	       (setq file (concat (substring file 0 (match-beginning 1))
				  (substring file (match-end 1)))))
	   (require 'info)
	   (condition-case ()
	       (Info-find-node file "Top")
	     (error (if (and file (file-exists-p file))
			(progn
			  (if (get-buffer "*info*")
			      (kill-buffer "*info*"))
			  (Info-find-node file "*" nil t))
		      (error "Invalid file")))))))
     ))
  "*Alist of (FILENAME-REGEXP . EDIT-FUNCTION) elements for calling special
functions to display particular file types within Emacs.  See also
`hpath:file-alist' for external display program settings.")

(defvar hpath:display-buffer-alist
  (list
   (list 'this-window   'switch-to-buffer)
   (list 'other-window  (function (lambda (b)
				    (if (br-in-browser)
					(progn (br-to-view-window)
					       (switch-to-buffer b))
				      (switch-to-buffer-other-window b)))))
   (list 'one-window    (function (lambda (b)
				    (if (br-in-browser) (br-quit))
				    (delete-other-windows)
				    (switch-to-buffer b))))
   (list 'new-frame     (function (lambda (b)
				    (select-frame (make-frame))
				    (switch-to-buffer b))))
   (list 'other-frame   'hpath:display-buffer-other-frame)
   (list 'other-frame-one-window
	 (function (lambda (b)
		     (hpath:display-buffer-other-frame b)
		     (delete-other-windows)))))
  "*Alist of (DISPLAY-WHERE-SYMBOL  DISPLAY-BUFFER-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays linked to buffers.
The default value of DISPLAY-WHERE-SYMBOL is given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in the current window, deleting other windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

(defvar hpath:display-where 'other-window
  "Symbol specifying the default method to use to display Hyperbole link referents.
See documentation of `hpath:display-where-alist' for valid values.")

(defvar hpath:display-where-alist
  (list
   (list 'this-window 'find-file)
   (list 'other-window (function (lambda (f)
				   (if (br-in-browser)
				       (progn (br-to-view-window)
					      (find-file f))
				     (find-file-other-window f)))))
   (list 'one-window  (function (lambda (f)
				  (if (br-in-browser) (br-quit))
				  (delete-other-windows) (find-file f))))
   (list 'new-frame   'find-file-new-frame)
   (list 'other-frame 'hpath:find-other-frame)
   (list 'other-frame-one-window
	 (function (lambda (f) (hpath:find-other-frame f) (delete-other-windows)))))
  "*Alist of (DISPLAY-WHERE-SYMBOL DISPLAY-FILE-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays linked to files.
The default value of DISPLAY-WHERE-SYMBOL is given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in the current window, deleting other windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

;;; `hyperb:window-system' variable from "hversion.el" must be defined
;;; prior to this variable definition.
;;;
(defvar hpath:find-alist
  (let ((nextstep-suffixes '(("\\.\\(adaptor\\|app\\|bshlf\\|clr\\|concur\\|create\\|diagram\\|dp\\|e?ps\\|frame\\|gif\\|locus\\|Mesa\\|nib\\|project\\|rtf\\|sense\\|tiff\\|tree\\)$" . "open")))
	(x-suffixes '(("\\.e?ps$" . "ghostview")
		      ("\\.ps\\.g?[zZ]$" . "zcat %s | ghostview -")
		      ("\\.\\(gif\\|tiff?\\|xbm\\|pm\\|pbm\\|jpe?g\\)"  . "xv")
		      ("\\.xwd$" . "xwud -noclick -in")
		      ("\\.ra?s$" . "snapshot -l")
		      ("\\.xpm$" . "sxpm")
		      ("\\.\\(fm\\|frame\\|mif\\)$" .
		       "frame.pl -vn -preader -c -f%s") ;; was "msgfm_driver"
		      ("\\.\\(doc\\|boo\\)$" . "ileaf")
		      )))
    (if (memq window-system '(dps ns))
	nextstep-suffixes
      (cdr (assoc hyperb:window-system
		  (list (cons "emacs19" x-suffixes)  ; GNU Emacs V19 under X
			(cons "lemacs"  x-suffixes)  ; XEmacs under X
			(cons "xterm"   x-suffixes)  ; GNU Emacs V18 under X
			(cons "epoch"   x-suffixes)  ; UofI Epoch under X
			'("sun"     . nil)       ; SunView
			(cons "next" nextstep-suffixes)
			'("apollo"  . nil)       ; Display Manager
			)))))
  "*Alist of (FILENAME-REGEXP . EDIT-PROGRAM) elements for using window system
dependent external programs to edit/display particular file types.  See also
`hpath:display-alist' for internal, window-system independent display
settings.")

;;; ************************************************************************
;;; LINK PATH VARIABLE SUBSTITUTION SETTINGS
;;; ************************************************************************

;;; The following variable permits sharing of links over wide areas, where
;;; links may contain variable references whose values may differ between
;;; link creator and link activator.
;;;
;;; When a link is created, if its path contains a match for any of the
;;; variable values in hpath:variables, then the variable's symbol is
;;; substituted for the literal value.  Hyperbole then replaces the variable
;;; with a matching value when the link is resolved.
;;;
(defvar hpath:variables
  '(hyperb:dir Info-directory Info-directory-list sm-directory load-path exec-path)
  "*List of Emacs Lisp variable symbols to substitute within matching link paths.
Each variable value, if bound, must be either a pathname or a list of pathnames.")

;;; ************************************************************************
;;; HYPERBOLE INITIALIZATION
;;; ************************************************************************

;;; This call loads the whole Hyperbole system.
;;; You may want to look at this file just to see what it does.
;;;
(require 'hinit)
;;;
;;; This call initializes the Hyperbole system for use.
;;;
(hyperb:init)

;;; ************************************************************************
;;; HYPERBOLE LOCAL VARIABLE SUPPORT
;;; ************************************************************************

;;; Uncomment this if you really need to be able to use Hyperbole variables
;;; (and others with colons in their names) within file local variable lists.
;;; See the source file for more details.
;;;
;;  (require 'hlvar)

;;; ************************************************************************
;;; SITE-SPECIFIC ADDITIONS - Add your Hyperbole configuration additions here.
;;; ************************************************************************

;;; ************************************************************************
;;; END OF HYPERBOLE CONFIGURATION
;;; ************************************************************************

(provide 'hsite)

(message "Hyperbole is ready for action.")
