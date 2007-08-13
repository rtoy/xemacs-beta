;;; sun-eos-debugger.el --- Implements the XEmacs/SPARCworks interface

;; Copyright (C) 1995 Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks Debugger dbx

;;; Commentary:

;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

(require 'eos-common "sun-eos-common")

;;; =================
;;; debugger protocol
;;; =================

(defvar eos::current-hollow-arrow nil)
(defvar eos::current-solid-arrow nil)
(defvar eos::current-dbx-proc-id nil
  "TT id for the current dbx")
(defvar eos::current-debugger-clique-id nil
  "Clique_ID for the current debugger/dbx")

;; currentpc.color

(defvar eos::currentpc-inst   "/* XPM */
static char * file[] = {
\"16 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #0000FFFF0000\",
\"o	c #000077770000\",
\"O	c #000044440000\",
\"         .      \",
\"         ..     \",
\"         .X.    \",
\" .........XX.   \",
\" .XXXXXXXXXoX.  \",
\" .Xooooooooooo. \",
\" .oOOOOOOOOoO.  \",
\" .........OO.   \",
\"         .O.    \",
\"         ..     \",
\"         .      \"};")

(defvar eos::currentpc-inst-alt
   "/* XPM */
static char * file[] = {
\"16 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #0000FFFF0000\",
\"o	c #000077770000\",
\"O	c #000044440000\",
\"         .      \",
\"         ..     \",
\"         .X.    \",
\" .........XX.   \",
\" .XXXXXXXXXoX.  \",
\" .Xooooooooooo. \",
\" .oOOOOOOOOoO.  \",
\" .........OO.   \",
\"         .O.    \",
\"         ..   ..\",
\"         .    ..\"};")

(defvar eos::visitedpc-inst
   "/* XPM */
static char * file[] ={
\"16 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #AFAFAFAFAFAF\",
\"o	c #7E7E7E7EA9A9\",
\"O	c #666633339999\",
\"         .      \",
\"         ..     \",
\"         .X.    \",
\" .........XX.   \",
\" .XXXXXXXXXoX.  \",
\" .XooooooooooO. \",
\" .XOOOOOOOOoO.  \",
\" .........OO.   \",
\"         .O.    \",
\"         ..     \",
\"         .      \"};")

(defvar eos::visitedpc-inst-alt
   "/* XPM */
static char * file[] ={
\"16 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #AFAFAFAFAFAF\",
\"o	c #7E7E7E7EA9A9\",
\"O	c #666633339999\",
\"         .      \",
\"         ..     \",
\"         .X.    \",
\" .........XX.   \",
\" .XXXXXXXXXoX.  \",
\" .XooooooooooO. \",
\" .XOOOOOOOOoO.  \",
\" .........OO.   \",
\"         .O.    \",
\"         ..   ..\",
\"         .    ..\"};")

(defvar eos::breakpoint-inst
   "/* XPM */
static char * file[] ={
\"11 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #FFFF66666666\",
\"o	c #FFFF00000000\",
\"O	c #777700000000\",
\"   .....   \",
\"  .XXXXX.  \",
\" .XXoooXX. \",
\".XXoooooXO.\",
\".XoooooooO.\",
\".XoooooooO.\",
\".XoooooooO.\",
\".XXoooooOO.\",
\" .XXoooOO. \",
\"  .OOOOO.  \",
\"   .....   \"};")

(defvar eos::breakpoint-inst-alt
   "/* XPM */
static char * file[] ={
\"11 11 5 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c #FFFF66666666\",
\"o	c #FFFF00000000\",
\"O	c #777700000000\",
\"   .....   \",
\"  .XXXXX.  \",
\" .XXoooXX. \",
\".XXoooooXO.\",
\".XoooooooO.\",
\".XoooooooO.\",
\".XoooooooO.\",
\".XXoooooOO.\",
\" .XXoooOO. \",
\"  .OOOOO...\",
\"   ..... ..\"};")

;; The TT protocol does not provide enough information to
;; use the eos::disabledBreakpoint glyph.

(defvar eos::disabledBreakpoint-inst
   "/* XPM */
static char * file[] ={
\"11 11 4 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c Grey\",
\"O	c Grey80\",
\"   .....   \",
\"  .XXXXX.  \",
\" .XXXXXXX. \",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXOO.\",
\" .XXXXXOO. \",
\"  .OOOOO.  \",
\"   .....   \"};")

(defvar eos::disabledBreakpoint-inst-alt
   "/* XPM */
static char * file[] ={
\"11 11 4 1\",
\" 	s background c #BDBDBDBDBDBD\",
\".	c #000000000000\",
\"X	c Grey\",
\"O	c Grey80\",
\"   .....   \",
\"  .XXXXX.  \",
\" .XXXXXXX. \",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXXO.\",
\".XXXXXXXOO.\",
\" .XXXXXOO. \",
\"  .OOOOO...\",
\"   ..... ..\"};")

(defvar eos::dbx-pattern-list nil)

(defun eos::debugger-startup ()
  ;; Actions to do at startup for eos-debugger.el
  (make-face 'stop-face)
  (make-face 'solid-arrow-face)
  (make-face 'hollow-arrow-face)
  
  (set-face-foreground 'stop-face eos::stop-color)
  (set-face-background 'stop-face 
		       (face-background (get-face 'default)))
  (set-face-foreground 'solid-arrow-face eos::solid-arrow-color)
  (set-face-background 'solid-arrow-face 
		       (face-background (get-face 'default)))
  (set-face-foreground 'hollow-arrow-face eos::hollow-arrow-color)
  (set-face-background 'hollow-arrow-face 
		       (face-background (get-face 'default)))

  (setq eos::dbx-pattern-list		; list of dbx TT patterns
	(eos::create-debugger-patterns))

;; should there be only one stop-face, with different properties depending
;; on the frame/device?

  (eos::annotation-set-inst 'debugger-stop 'x eos::breakpoint-inst [nothing])
  (eos::annotation-set-inst 'debugger-stop 'tty "[S]" [nothing])
  (eos::annotation-set-face 'debugger-stop 'x
			    (get-face 'stop-face) (get-face 'stop-face))
  (eos::annotation-set-face 'debugger-stop 'tty
			    (get-face 'highlight) (get-face 'highlight))

  (eos::annotation-set-inst 'debugger-hollow-arrow 'x eos::visitedpc-inst [nothing])
  (eos::annotation-set-inst 'debugger-hollow-arrow 'tty "[]>" [nothing])
  (eos::annotation-set-face 'debugger-hollow-arrow 'x
			    (get-face 'hollow-arrow-face)
			    (get-face 'hollow-arrow-face))
  (eos::annotation-set-face 'debugger-hollow-arrow 'tty
			    (get-face 'highlight) (get-face 'highlight))

  (eos::annotation-set-inst 'debugger-solid-arrow 'x eos::currentpc-inst [nothing])
  (eos::annotation-set-inst 'debugger-solid-arrow 'tty "=>" [nothing])
  (eos::annotation-set-face 'debugger-solid-arrow 'x
			    (get-face 'solid-arrow-face)
			    (get-face 'solid-arrow-face))
  (eos::annotation-set-face 'debugger-solid-arrow 'tty
			    (get-face 'highlight) (get-face 'highlight))
)

;; Not yet ready for prime time.

(defvar eos::fill-stack-buffer nil
  "when t don't try any stack tracing")

(defvar eos::stack-buffer "*Eos Stack*"
  "name of buffer where to log Stack")

(defun eos::empty-stack ()
  ;; No valid stack data - e.g. resume/run program -
  (if eos::fill-stack-buffer
      (progn
	(set-buffer (get-buffer-create eos::stack-buffer))
	(toggle-read-only -1)
	(delete-region (point-min) (point-max))
	(toggle-read-only 1)
	)))

(defun eos::load-stack ()
  ;; Should send a TT message requesting for the stack information;
  ;; with the real work done in a callback
  (if eos::fill-stack-buffer
      (eos::stack-test 1)))

(defun eos::visit-stack (stackpos)
  (if eos::fill-stack-buffer
      (progn
	(eos::empty-stack)
	(eos::stack-test 1)
	)))

(defun eos::create-stack-patterns ()
  ;; returns a list of patterns
  (list
   (make-an-observer "SPRO_SPIDER_FRAMES" 'eos::spro_spider_frames)
   ))

(defun eos::spro_spider_frames (msg pat)
  ;; We have received a SPRO_SPIDER_FRAMES notice
  (let ((count (get-tooltalk-message-attribute msg 'args_count))
	(i 1))
    (set-buffer (get-buffer-create eos::stack-buffer))
    (toggle-read-only -1)
    (while (< i count)
      ;; optional leading comment
      (if (equal (get-tooltalk-message-attribute msg 'arg_type i)
		 "Stack_Info1")
	  (progn
	    (insert (get-tooltalk-message-attribute msg 'arg_val i))
	    (setq i (1+ i))))
      ;; current frame?
      (insert (if (equal (get-tooltalk-message-attribute msg 'arg_ival i)
			 "0") "  " "> "))
      (setq i (1+ i))
      (insert (format "[%s] %s%s %s:%s"
		      ;; frameno
		      (get-tooltalk-message-attribute msg 'arg_ival i)
		      ;; funcname
		      (get-tooltalk-message-attribute msg 'arg_val (+ i 1))
		      ;; funcargs
		      (get-tooltalk-message-attribute msg 'arg_val (+ i 2))
      		      ;; source
		      (get-tooltalk-message-attribute msg 'arg_val (+ i 3))
		      ;; line
		      (get-tooltalk-message-attribute msg 'arg_val (+ i 4))))
      (setq i (+ i 5))
      (if (equal (get-tooltalk-message-attribute msg 'arg_type i)
		 "Stack_Info2")
	  (progn
	    (insert (get-tooltalk-message-attribute msg 'arg_val i))
	    (setq i (1+ i))))
      (insert "\n"))
    (toggle-read-only 1)
;;    (return-tooltalk-message msg)
    ))

(defun eos::spider-stack-callback (msg pat)
  ;; Callback after processing a spider_stack request
  (destroy-tooltalk-message msg)
  )

(defmacro eos::stack-tt-args (spider-id clique-id hidden verbose quick starting-index count)
  (` (list
      'class TT_REQUEST
      'address TT_HANDLER
      'scope TT_SESSION
      'handler (, spider-id)
      'op "SPRO_SPIDER_STACK"
      'callback 'eos::spider-stack-callback
      'args (list
	     (list 'TT_IN (, clique-id) "Context_ID")
	     (list 'TT_IN (, hidden) "Boolean")
	     (list 'TT_IN (, verbose) "Boolean")
	     (list 'TT_IN (, quick) "Boolean")
	     (list 'TT_IN (, starting-index) "int")
	     (list 'TT_IN (, count) "int"))
      )))

(defun eos::stack-test (starting-index)
  (let ((msg (make-tooltalk-message
	      (eos::stack-tt-args eos::current-dbx-proc-id
				  eos::current-debugger-clique-id
				  0	; hidden
				  1	; verbose
				  0	; quick
				  starting-index
				  4	; count
				  ))))
    (send-tooltalk-message msg)
;;    (destroy-tooltalk-message msg)
    ))

;; (setq eos::fill-stack-buffer t)
;; (setq eos::fill-stack-buffer nil)
;; (setq eos::stack-pattern-list (eos::create-stack-patterns))
;; (mapcar 'register-tooltalk-pattern eos::stack-pattern-list)
;; (mapcar 'unregister-tooltalk-pattern eos::stack-pattern-list)
;; (eos::stack-test 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;

(defun eos::spro_te_eventset (msg pat)
  ;; thread_id trap_id string string filename lineno string string
  (let* ((trap-id
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (filename
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 5))))
    (eos::add-annotation 'debugger-stop filename lineno trap-id)
;;    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_eventdel (msg pat)
  ;; trap_id string string filename lineno string string
  (let* ((trap-id
	  (get-tooltalk-message-attribute msg 'arg_val 0))
	 (filename
	  (get-tooltalk-message-attribute msg 'arg_val 3))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 4))))
    (eos::delete-annotation 'debugger-stop filename lineno trap-id)
;;    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_stopped (msg pat)
  ;; thread_id filename procname lineno filename procname lineno
  (let* ((filename-hollow
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (procname-hollow
	  (get-tooltalk-message-attribute msg 'arg_val 2))
	 (lineno-hollow
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (filename-solid
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno-solid
	  (read (get-tooltalk-message-attribute msg 'arg_ival 6)))
	 )
    (setq eos::current-solid-arrow
	  (eos::make-annotation-visible eos::current-solid-arrow
					filename-solid
					lineno-solid
					'debugger-solid-arrow))
    (if (or (not (equal filename-solid filename-hollow))
	    (not (equal lineno-solid lineno-hollow)))
	(setq eos::current-hollow-arrow
	      (eos::make-annotation-visible eos::current-hollow-arrow
				 filename-hollow
				 lineno-hollow
				 'debugger-hollow-arrow)))
;;    (return-tooltalk-message msg)
    (eos::load-stack)
    ))

;; Tracking current id's
;;

(defun eos::update-dbx-proc-id (msg)
  (setq eos::current-dbx-proc-id
	(get-tooltalk-message-attribute msg 'sender))
  ;; the following is needed to make toolbar entries be active or not
  ;; I think it is not needed in 19.13
  (eos::select-debugger-frame eos::debugger-frame)
  )

(defun eos::update-current-debugger-clique-id (msg)
  (setq eos::current-debugger-clique-id
	(get-tooltalk-message-attribute msg 'arg_val 0))
  )

;;
;; Updating arrows
;;


(defun eos::update-pids (msg)
  (eos::update-dbx-proc-id msg)
  (eos::update-current-debugger-clique-id msg))

(defun eos::internal-clear-annotations (stack arrows stops &optional clique)
  (if stack
      (eos::empty-stack))
  (if arrows
      (progn
	(eos::make-annotation-invisible eos::current-hollow-arrow)
	(eos::make-annotation-invisible eos::current-solid-arrow)))
  (if clique
      (progn
	(setq eos::current-debugger-clique-id nil)
	;; not needed in 19.13?
	(eos::select-debugger-frame eos::debugger-frame)))
  (if stops
      (eos::remove-all-from-annotation-list 'debugger-stop)))


(defun eos::clear-arrows (msg pat)
  (eos::internal-clear-annotations t t nil)
;;  (return-tooltalk-message msg)
  )

(defun eos::update-clear-stops (msg pat)
  (eos::update-pids msg)
  (eos::internal-clear-annotations t nil t)
;;  (return-tooltalk-message msg)
  )

(defun eos::update-clear-arrows-stops (msg pat)
  (eos::update-pids msg)
  (eos::internal-clear-annotations t t t)
;;  (return-tooltalk-message msg)
  )

(defun eos::clear-arrows-stops (msg pat)
  (let ((this-proc-id
	 (get-tooltalk-message-attribute msg 'sender)))
    (if (equal eos::current-dbx-proc-id this-proc-id)
	(progn
	  (eos::internal-clear-annotations t t t)
	  ;;  (return-tooltalk-message msg)
	  ))))

;;

;;

(defun eos::spro_detach (msg pat)
  ;; a detach notification has been received. this means dbx/debugger
  ;; is exiting
  (eos::internal-clear-annotations t t t t)
  (eos::dismiss-print-frame))

(defun eos::spro_te_location (msg pat)
  ;; thread_id filename procname lineno filename procname lineno
  (let* ((filename-hollow
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno-hollow
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (filename-solid
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno-solid
	  (read (get-tooltalk-message-attribute msg 'arg_ival 6)))
	 )
    (setq eos::current-solid-arrow
	  (eos::make-annotation-visible eos::current-solid-arrow
			     filename-solid
			     lineno-solid
			     'debugger-solid-arrow))
    (if (or (not (equal filename-solid filename-hollow))
	    (not (equal lineno-solid lineno-hollow)))
	(setq eos::current-hollow-arrow
	      (eos::make-annotation-visible eos::current-hollow-arrow
				 filename-hollow
				 lineno-hollow
				 'debugger-hollow-arrow)))
;;    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_visit (msg pat)
  ;; thread_id filename procname lineno stackpos
  (let* ((filename
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (procname
	  (get-tooltalk-message-attribute msg 'arg_val 2))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (stackpos
	  (read (get-tooltalk-message-attribute msg 'arg_ival 4)))
	 )
    (eos::make-annotation-invisible eos::current-hollow-arrow)
    (if (equal stackpos 1)
	(progn
	  (eos::make-annotation-invisible eos::current-solid-arrow)
	  (setq eos::current-solid-arrow
		(eos::make-annotation-visible eos::current-solid-arrow
					      filename
					      lineno
					      'debugger-solid-arrow))
	  )
      (setq eos::current-hollow-arrow
	    (eos::make-annotation-visible eos::current-hollow-arrow
					  filename
					  lineno
					  'debugger-hollow-arrow))
      )
;;    (return-tooltalk-message msg)
    (eos::visit-stack stackpos)
    ))

;; generate a list of patterns
;; so it can be registered and unregistered.


(defun eos::create-debugger-patterns ()
  ;; returns a list of patterns
  (list
   (make-an-observer "SPRO_TE_STOPPED" 'eos::spro_te_stopped)
   (make-an-observer "SPRO_SE_STARTED" 'eos::clear-arrows)
   (make-an-observer "SPRO_TE_STEPPED" 'eos::clear-arrows)
   (make-an-observer "SPRO_TE_CONTINUED" 'eos::clear-arrows)
   (make-an-observer "SPRO_SE_DROPPED" 'eos::clear-arrows-stops)
   (make-an-observer "SPRO_SE_DEBUGGED" 'eos::update-clear-stops)
   (make-an-observer "SPRO_SE_REVIVED" 'eos::update-clear-arrows-stops)
   (make-an-observer "SPRO_SE_ATTACHED" 'eos::update-clear-arrows-stops)
   (make-an-observer "SPRO_SE_GONE" 'eos::clear-arrows)
   (make-an-observer "SPRO_TE_LOCATION" 'eos::spro_te_location)
   (make-an-observer "SPRO_TE_VISIT" 'eos::spro_te_visit)
   (make-an-observer "SPRO_TE_EVENTSET" 'eos::spro_te_eventset)
   (make-an-observer "SPRO_TE_EVENTDEL" 'eos::spro_te_eventdel)
   (make-an-observer "SPRO_DETACH" 'eos::spro_detach)
   ))

(defun eos::register-debugger-patterns ()
  ;; register all dbx patterns
  (mapcar 'register-tooltalk-pattern eos::dbx-pattern-list)
  (eos::register-debugger-extra-patterns))

(defun eos::unregister-debugger-patterns ()
  ;; unregister all dbx patterns
  (mapcar 'unregister-tooltalk-pattern eos::dbx-pattern-list)
  (eos::unregister-debugger-extra-patterns))

(provide 'eos-debugger)

;;; sun-eos-debugger.el ends here
