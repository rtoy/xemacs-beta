;;; sunpro-sparcworks.el --- support SPARCworks manager ToolTalk messages

;; Copyright (C)  Sun Microsystems, Inc.

;; Author:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
;; Maintainer:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
;; Created:	20 Mar 95

;; Keywords:	SPARCworks, ToolTalk, messages

;;; Commentary:

;; Called from the SPARCworks Manager with the command:
;;
;;    xemacs -q -l sunpro-sparcworks $SUNPRO_SWM_TT_ARGS $SUNPRO_SWM_GUI_ARGS
;;

;;; To Do:

;;; Code:

(require 'cl)				; Common Lisp compatibility
(require 'cl-19)			; Version for XEmacs 19

(defvar sunpro-sparcworks-ops
  '("quit"
    "hide"
    "expose"
    "PEI_CLOSE"
    "PEI_OPEN"
    "PEI_NOP")
  "The ToolTalk operations that are handled.")

(defvar sunpro-sparcworks-callbacks
  '(sp-sw-quit-handler
    sp-sw-hide-handler
    sp-sw-expose-handler
    sp-sw-close-handler
    sp-sw--open-handler
    sp-sw-nop-handler)
  "The ToolTalk operations that are handled.")

(defvar sunpro-sparcworks-invocation-count nil
  "The number of XEmacsen invoked via the SPARCworks Manager.")

(defun sp-sw-quit-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (save-buffers-kill-emacs))

(defun sp-sw-hide-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (mapcar #'make-frame-invisible (frame-list)))

(defun sp-sw-expose-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (mapcar #'make-frame-visible (frame-list)))

(defun sp-sw-close-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (mapcar #'iconify-frame (frame-list)))

(defun sp-sw-open-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (mapcar #'deiconify-frame (frame-list)))

(defun sp-sw-nop-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  '())

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "quit"
    callback sp-sw-quit-handler)))

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "hide"
    callback sp-sw-hide-handler)))

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "expose"
    callback sp-sw-expose-handler)))

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "PEI_CLOSE"
    callback sp-sw-close-handler)))

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "PEI_OPEN"
    callback sp-sw-open-handler)))

(register-tooltalk-pattern
 (make-tooltalk-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "PEI_NOP"
    callback sp-sw-nop-handler)))

(defun sunpro-sparcworks-handle-command-line (arg)
  "Handle the SPARCworks Manager-specific command line arguments."
  (setq *sunpro-sparcworks-invocation-count* arg)
  ;;Fix up the command-line in case there are more arguments
  (setq command-line-args-left
	(cdr command-line-args-left)))


;;; Initialize
(setq command-switch-alist
      (purecopy
       (append '(("-swtm" . sunpro-sparcworks-handle-command-line))
	       command-switch-alist)))


;;; sunpro-sparcworks.el ends here

