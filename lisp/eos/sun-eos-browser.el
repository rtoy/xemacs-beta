;;; sun-eos-browser.el --- Implements the XEmacs/SPARCworks SourceBrowser interface

;; Copyright (C) 1995  Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks SBrowser Source Browser

;;; Commentary:
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

(require 'eos-common "sun-eos-common")

;; ================
;; Browser Protocol
;; ================
;;
;; three notifications
;;
;; SPRO_SBENG_START
;; SPRO_SBENG_CURRENT_ELEMENT CONTEXT_UID filename lineno center==0
;; SPRO_SBENG_QUIT

(defvar eos::currentMatch-inst "/* XPM */
static char * file[] = {
\"14 11 5 1\",
\" 	s background c #FFFFFFFFFFFF\",
\".	c #000000000000\",
\"X	c #0000FFFF0000\",
\"o	c #000077770000\",
\"O	c #000044440000\",
\"              \",
\"   oo         \",
\"   oXOo       \",
\"   oXXXOo     \",
\"   oXXXXXOo   \",
\"   oXXXXXXXo. \",
\"   oXXXXXOo   \",
\"   oXXXOo     \",
\"   oXOo       \",
\"   oo         \",
\"              \"};")

(defvar eos::currentMatch-inst-alt "/* XPM */
static char * file[] = {
\"14 11 5 1\",
\" 	s background c #FFFFFFFFFFFF\",
\".	c #000000000000\",
\"X	c #0000FFFF0000\",
\"o	c #000077770000\",
\"O	c #000044440000\",
\"              \",
\"   oo         \",
\"   oXOo       \",
\"   oXXXOo     \",
\"   oXXXXXOo   \",
\"   oXXXXXXXo. \",
\"   oXXXXXOo   \",
\"   oXXXOo     \",
\"   oXOo       \",
\"   oo      .. \",
\"           .. \"};")

(defvar sbrowser-pattern-list nil)


(defun eos::browser-startup ()
  ;; Actions to do at startup for eos-browser.el
  (make-face 'sbrowse-arrow-face)

  (set-face-foreground 'sbrowse-arrow-face
		       eos::sbrowse-arrow-color)
  (set-face-background 'sbrowse-arrow-face
		       (face-background (get-face 'default)))

  (setq sbrowser-pattern-list		; list of browser TT patterns
	(eos::create-sbrowser-patterns))

  ;; now register glyphs and faces...

  (eos::annotation-set-inst 'sbrowser 'x eos::currentMatch-inst [nothing])
  (eos::annotation-set-inst 'sbrowser 'tty "|>" [nothing])
  (eos::annotation-set-face 'sbrowser 'x
			    (get-face 'sbrowse-arrow-face)
			    (get-face 'sbrowse-arrow-face))
  (eos::annotation-set-face 'sbrowser 'tty
			    (get-face 'highlight)
			    (get-face 'highlight))
)

(defvar eos::current-match nil)

(defun eos::spro_sbeng_current_element (msg pat)
  ;; SPRO_SBENG_CURRENT_ELEMENT CONTEXT_UID filename lineno center==0
  (let* ((filename
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 2)))
	 )
    (setq eos::current-match
	  (eos::make-annotation-visible eos::current-match
					filename
					lineno
					'sbrowser))
    (return-tooltalk-message msg)
    ))

(defun eos::spro_sbeng_start (msg pat)
    (eos::make-annotation-invisible eos::current-match)
    (return-tooltalk-message msg)
    )

(defun eos::spro_sbeng_quit (msg pat)
    (eos::make-annotation-invisible eos::current-match)
    (return-tooltalk-message msg)
    )

(defun eos::create-sbrowser-patterns ()
  ;; returns list of patterns
  (list
   (make-an-observer "SPRO_SBENG_CURRENT_ELEMENT"
		     'eos::spro_sbeng_current_element)
   (make-an-observer "SPRO_SBENG_START"
		     'eos::spro_sbeng_start)
   (make-an-observer "SPRO_SBENG_QUIT"
		     'eos::spro_sbeng_quit)
   ))

(defun eos::register-sbrowser-patterns ()
  ;; register all sbrowser patterns
  (mapcar 'register-tooltalk-pattern sbrowser-pattern-list))

(defun eos::unregister-sbrowser-patterns ()
  ;; unregister all sbrowser patterns
  (mapcar 'unregister-tooltalk-pattern sbrowser-pattern-list))

;; Actions to start a sourcebrowser in the background.

(defvar eos::sbrowser-process nil
  "sbrowser process for the background.  Only one per XEmacs")

(defun eos::start-sbrowser ()
  ;; Start an "sbrowser -editor" in the background. Will ask for confirmation if
  ;; XEmacs somehow believes there is already one running
  (interactive)
  (if (or (not (processp eos::sbrowser-process))
	  (not (eq (process-status eos::sbrowser-process) 'run))
	  (yes-or-no-p
	   "Warning! XEmacs believes there already is a sbrowser -editor, proceed?"))
      (progn
	(setq eos::sbrowser-process
	      (start-process "*eos sbrowser*" nil "sbrowser" "-editor"))
	(message "Starting SBrowser subprocess")
	(eos::select-sbrowser-frame (selected-frame))
	)))

(provide 'eos-browser) 

;;; sun-eos-browser.el ends here
