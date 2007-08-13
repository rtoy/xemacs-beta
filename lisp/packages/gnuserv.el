;;; gnuserv.el --- Lisp interface code between Emacs and gnuserv
;; Copyright (C) 1989-1997 Free Software Foundation, Inc.

;; Version: 3.10
;; Author: Andy Norman (ange@hplb.hpl.hp.com), originally based on server.el
;;         Hrvoje Niksic <hniksic@srce.hr>, rewritten from scratch in May 1997
;; Maintainer: Jan Vroonhof <vroonhof@math.ethz.ch>,
;;             Hrvoje Niksic <hniksic@srce.hr>
;; Keywords: environment, processes, terminals

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:
 
;; Gnuserv is run when Emacs needs to operate as a server for other
;; processes.  Specifically, any number of files can be attached for
;; editing to a running XEmacs process using the `gnuclient' program.

;; Use `M-x gnuserv-start' to start the server and `gnuclient files'
;; to load them to XEmacs.  When you are done with a buffer, press
;; `C-x #' (`M-x gnuserv-edit').  You can put (gnuserv-start) to your
;; .emacs, and enable `gnuclient' as your Unix "editor".  When all the
;; buffers for a client have been edited and exited with
;; `gnuserv-edit', the client "editor" will return to the program that
;; invoked it.

;; Your editing commands and Emacs' display output go to and from the
;; terminal or X display in the usual way.  If you are running under
;; X, a new X frame will be open for each gnuclient.  If you are on a
;; TTY, this TTY will be attached as a new device to the running
;; XEmacs, and will be removed once you are done with the buffer.

;; To evaluate a Lisp form in a running Emacs, use the `-eval'
;; argument of gnuclient.  To simplify this, we provide the `gnudoit'
;; shell script.  For example `gnudoit "(+ 2 3)"' will print `5',
;; whereas `gnudoit "(gnus)"' will fire up your favorite newsreader.
;; Like gnuclient, `gnudoit' requires the server to be started prior
;; to using it.

;; For more information you can refer to man pages of gnuclient,
;; gnudoit and gnuserv, distributed with XEmacs.

;; gnuserv.el was originally written by Andy Norman as an improvement
;; ver William Sommerfeld's server.el.  Since then, a number of people
;; have worked on it, including Bob Weiner, Darell Kindred, Arup
;; Mukherjee, Ben Wing and Jan Vroonhof.  It was completely rewritten
;; (labeled as version 3) by Hrvoje Niksic in May 1997.

;; Jan Vroonhof
;;     Customized.
;;
;; Hrvoje Niksic <hniksic@srce.hr> May/1997
;;     Completely rewritten.  Now uses `defstruct' and other CL stuff
;;     to define clients cleanly.  Many thanks to Dave Gillespie!
;;
;; Mike Scheidler <c23mts@eng.delcoelect.com> July, 1997
;;     Added 'Done' button to the menubar.
;;
;; Hrvoje Niksic <hniksic@srce.hr> Sep/1997
;;     More pervasive changes.


;;; Code:

(defgroup gnuserv nil
  "The gnuserv suite of programs to talk to Emacs from outside."
  :group 'environment
  :group 'processes
  :group 'terminals)


;; Provide the old variables as aliases, to avoid breaking .emacs
;; files.  However, they are obsolete and should be converted to the
;; new forms.  This ugly crock must be before the variable
;; declaration, or the scheme fails.  I'd prefer if we could junk this
;; sh*t, but I guess the users will appreciate compatibility.  Uh...

(define-obsolete-variable-alias 'server-frame 'gnuserv-frame)
(define-obsolete-variable-alias 'server-done-function
  'gnuserv-done-function)
(define-obsolete-variable-alias 'server-done-temp-file-function
  'gnuserv-done-temp-file-function)
(define-obsolete-variable-alias 'server-find-file-function
  'gnuserv-find-file-function)
(define-obsolete-variable-alias 'server-program
  'gnuserv-program)
(define-obsolete-variable-alias 'server-visit-hook
  'gnuserv-visit-hook)
(define-obsolete-variable-alias 'server-done-hook
  'gnuserv-done-hook)
(define-obsolete-variable-alias 'server-kill-quietly
  'gnuserv-kill-quietly)

;;;###autoload
(defcustom gnuserv-frame 'new
  "*Determines what frame will be used to display all edited files.
Legal values are:
 `new'     -- a new frame will be created for each file edited;
 `current' -- the currently selected frame will be used;
 `main'    -- \"main\" Emacs frame will be used;
 `visible' -- a visible frame will be used, or a new one created;
 `special' -- a special Gnuserv frame will be created, and used for
              all gnuserv-edited files;
 frame     -- that particular frame will be used.

If gnuclient is called using the `-nw' method (from a TTY device), the
 behaviour will be as if gnuserv-frame were `new'.
This variable is read by `gnuserv-frame-default-function'.  If you
 change `gnuserv-frame-function' to anything else, this variable will
 have no effect."
  :tag "Gnuserv Frame"
  ;; Compatibility
  :type '(radio (const :tag "Create new frame each time" new)
		(const :tag "Use currently selected frame" current)
		(const :tag "Use main Emacs frame" main)
		(const :tag "Use visible frame, otherwise create new" visible)
		(const :tag "Create special Gnuserv frame and use it" special))
  :group 'gnuserv)

(defcustom gnuserv-frame-properties nil
  "*Properties of the frame in which gnuclient buffers are displayed.
Note that this is in effect only for frames created by
`gnuserv-frame-default-function'."
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value")))
  :group 'gnuserv)

(defcustom gnuserv-frame-function 'gnuserv-frame-default-function
  "*Function to return the appropriate frame for use by gnuclient.
The function will be called with two arguments: the first one as
 described by `gnuserv-frame', and the second one as the device to
 create the frame on.
The function must return a valid frame object."
  :type 'function
  :group 'gnuserv)

(defcustom gnuserv-done-function 'kill-buffer 
  "*Function used to remove a buffer after editing.
It is called with one BUFFER argument.  Functions such as `kill-buffer' and
`bury-buffer' are good values. See also `gnuserv-done-temp-file-function'."
  :type '(radio (function-item kill-buffer)
		(function-item bury-buffer)
		(function :tag "Other"))
  :group 'gnuserv)

(defcustom gnuserv-find-file-function 'find-file
  "*Function to visit a file with.
It takes one argument, a file name to visit."
  :type 'function
  :group 'gnuserv)

(defcustom gnuserv-view-file-function 'view-file
  "*Function to view a file with.
It takes one argument, a file name to view."
  :type '(radio (function-item view-file)
		(function-item find-file-read-only)
		(function :tag "Other"))
  :group 'gnuserv)

(defcustom gnuserv-program "gnuserv"
  "*Program to use as the editing server."
  :type 'string
  :group 'gnuserv)

(defcustom gnuserv-visit-hook nil
  "*Hook run after visiting a file."
  :type 'hook
  :group 'gnuserv)

(defcustom gnuserv-done-hook nil
  "*Hook run when done editing a buffer for the Emacs server.
The hook functions are called after the file has been visited, with the
current buffer set to the visiting buffer."
  :type 'hook
  :group 'gnuserv)

(defcustom gnuserv-init-hook nil
  "*Hook run after the server is started."
  :type 'hook
  :group 'gnuserv)

(defcustom gnuserv-shutdown-hook nil
  "*Hook run before the server exits."
  :type 'hook
  :group 'gnuserv)

(defcustom gnuserv-kill-quietly nil
  "*Non-nil means to kill buffers with clients attached without requiring confirmation."
  :type 'boolean
  :group 'gnuserv)


;;; Internal variables:

(defstruct gnuclient
  "An object that encompasses several buffers in one.
Normally, a client connecting to Emacs will be assigned an id, and
will request editing of several files.

ID       - Client id (integer).
BUFFERS  - List of buffers that \"belong\" to the client.
           NOTE: one buffer can belong to several clients.
DEVICE   - The device this client is on.  If the device was also created.
           by a client, it will be placed to `gnuserv-devices' list.
FRAME    - Frame created by the client, or nil if the client didn't
           create a frame.

All the slots default to nil."
  (id nil)
  (buffers nil)
  (device nil)
  (frame nil))

(defvar gnuserv-process nil
  "The current gnuserv process.")

(defvar gnuserv-string ""
  "The last input string from the server.")

(defvar gnuserv-current-client nil
  "The client we are currently talking to.")

(defvar gnuserv-clients nil
  "List of current gnuserv clients.
Each element is a gnuclient structure that identifies a client.")

(defvar gnuserv-devices nil
  "List of devices created by clients.")

;; We want the client-infested buffers to have some modeline
;; identification, so we'll make a "minor mode".  We don't use
;; `add-minor-mode', as we don't want it to be togglable.
(defvar gnuserv-minor-mode nil)

(make-variable-buffer-local 'gnuserv-mode)
(pushnew '(gnuserv-minor-mode " Server") minor-mode-alist :test 'equal)

(defvar gnuserv-special-frame nil
  "Frame created specially for Server.")


;; Creating gnuserv frame.

(defun gnuserv-frame-default-function (arg device)
  "Default function to create Gnuserv frames.
See the documentation of `gnuserv-frame' for instructions how to
customize it."
  ;; If we are on TTY, act as if `new' was given.
  (if (not (device-on-window-system-p))
      (gnuserv-frame-default-function 'new device)
    (cond
      ((or (eq arg 'new)
	   ;; nil for back-compat
	   (eq arg nil))
       (make-frame gnuserv-frame-properties device))
      ((or (eq arg 'current)
	   ;; t for back-compat
	   (eq arg t))
       (selected-frame))
      ((eq arg 'main)
       (car (frame-list)))
      ((eq arg 'visible)
       (cond ((car (filtered-frame-list 'frame-totally-visible-p device)))
	     ((car (filtered-frame-list (lambda (frame)
					  ;; eq t as in not 'hidden
					  (eq (frame-visible-p frame) t))
					device)))
	     (t (make-frame gnuserv-frame-properties device))))
      ((eq arg 'special)
       (unless (frame-live-p gnuserv-special-frame)
	 (setq gnuserv-special-frame
	       (make-frame gnuserv-frame-properties device))))
      ((frame-live-p arg)
       arg)
      (t
       (error "Invalid argument %s" arg)))))


;;; Communication functions

;; We used to restart the server here, but it's too risky -- if
;; something goes awry, it's too easy to wind up in a loop.
(defun gnuserv-sentinel (proc msg)
  (let ((msgstring (concat "Gnuserv process %s; restart with `%s'"))
	(keystring (substitute-command-keys "\\[gnuserv-start]")))
  (case (process-status proc)
    (exit
     (message msgstring "exited" keystring)
     (gnuserv-prepare-shutdown))
    (signal
     (message msgstring "killed" keystring)
     (gnuserv-prepare-shutdown))
    (closed
     (message msgstring "closed" keystring))
     (gnuserv-prepare-shutdown))))

;; This function reads client requests from our current server.  Every
;; client is identified by a unique ID within the server
;; (incidentally, the same ID is the file descriptor the server uses
;; to communicate to client).
;;
;; The request string can arrive in several chunks.  As the request
;; ends with \C-d, we check for that character at the end of string.
;; If not found, keep reading, and concatenating to former strings.
;; So, if at first read we receive "5 (gn", that text will be stored
;; to gnuserv-string.  If we then receive "us)\C-d", the two will be
;; concatenated, `current-client' will be set to 5, and `(gnus)' form
;; will be evaluated.
;;
;; Server will send the following:
;;
;; "ID <text>\C-d"  (no quotes)
;;
;;  ID    - file descriptor of the given client;
;; <text> - the actual contents of the request.
(defun gnuserv-process-filter (proc string)
  "Process gnuserv client requests to execute Emacs commands."
  (setq gnuserv-string (concat gnuserv-string string))
  ;; C-d means end of request.
  (when (string-match "\C-d\\'" gnuserv-string)
    (cond ((string-match "^[0-9]+" gnuserv-string) ; client request id
	   (let ((header (read-from-string gnuserv-string)))
	     ;; Set the client we are talking to.
	     (setq gnuserv-current-client (car header))
	     ;; Evaluate the expression
	     (condition-case oops
		 (eval (car (read-from-string gnuserv-string (cdr header))))
	       ;; In case of an error, write the description to the
	       ;; client, and then signal it.
	       (error (setq gnuserv-string "")
		      (gnuserv-write-to-client gnuserv-current-client oops)
		      (setq gnuserv-current-client nil)
		      (signal (car oops) (cdr oops)))
	       (quit (setq gnuserv-string "")
		     (gnuserv-write-to-client gnuserv-current-client oops)
		     (setq gnuserv-current-client nil)
		     (signal 'quit nil)))
	     (setq gnuserv-string "")))
	  (t
	   (error "%s: invalid response from gnuserv" gnuserv-string)
	   (setq gnuserv-string "")))))

;; This function is somewhat of a misnomer.  Actually, we write to the
;; server (using `process-send-string' to gnuserv-process), which
;; interprets what we say and forwards it to the client.  The
;; incantation server understands is (from gnuserv.c):
;;
;; "FD/LEN:<text>\n"  (no quotes)
;;    FD     - file descriptor of the given client (which we obtained from
;;             the server earlier);
;;    LEN    - length of the stuff we are about to send;
;;    <text> - the actual contents of the request.
(defun gnuserv-write-to-client (client-id form)
  "Write the given form to the given client via the gnuserv process."
  (when (eq (process-status gnuserv-process) 'run)
    (let* ((result (format "%s" form))
	   (s      (format "%s/%d:%s\n" client-id
			   (length result) result)))
      (process-send-string gnuserv-process s))))

;; The following two functions are helper functions, used by
;; gnuclient.

(defun gnuserv-eval (form)
  "Evaluate form and return result to client."
  (gnuserv-write-to-client gnuserv-current-client (eval form))
  (setq gnuserv-current-client nil))

(defun gnuserv-eval-quickly (form)
  "Let client know that we've received the request, and then eval the form.
This order is important as not to keep the client waiting."
  (gnuserv-write-to-client gnuserv-current-client nil)
  (setq gnuserv-current-client nil)
  (eval form))


;; "Execute" a client connection, called by gnuclient.  This is the
;; backbone of gnuserv.el.
(defun gnuserv-edit-files (type list &rest flags)
  "For each (line-number . file) pair in LIST, edit the file at line-number.
The visited buffers are memorized, so that when \\[gnuserv-edit] is invoked
in such a buffer, or when it is killed, or the client's device deleted, the
client will be invoked that the edit is finished.

TYPE should either be a (tty TTY TERM PID) list, or (x DISPLAY) list.
If a flag is `quick', just edit the files in Emacs.
If a flag is `view', view the files read-only."
  (let (quick view)
    (mapc (lambda (flag)
	    (case flag
	      (quick (setq quick t))
	      (view  (setq view t))
	      (t     (error "Invalid flag %s" flag))))
	  flags)
    (let* ((old-device-num (length (device-list)))
	   (old-frame-num  (length (frame-list)))
	   (device (case (car type)
			    (tty (apply 'make-tty-device (cdr type)))
			    (x   (make-x-device (cadr type)))
			    (t   (error "Invalid device type"))))
	   (frame (funcall gnuserv-frame-function gnuserv-frame device))
	   (client (make-gnuclient :id gnuserv-current-client
				   :device device
				   :frame (if (= (length (frame-list))
						 old-frame-num)
					      nil frame))))
      (setq gnuserv-current-client nil)
      ;; If the device was created by this client, push it to the list.
      (and (/= old-device-num (length (device-list)))
	   (push device gnuserv-devices))
      (and (frame-iconified-p frame)
	   (deiconify-frame frame))
      ;; Visit all the listed files.
      (while list
	(let ((line (caar list)) (path (cdar list)))
	  (select-frame frame)
	  (raise-frame frame)
	  ;; Visit the file.
	  (funcall (if view
		       gnuserv-view-file-function
		     gnuserv-find-file-function)
		   path)
	  (goto-line line)
	  ;; Don't memorize the quick and view buffers.
	  (unless (or quick view)
	    (pushnew (current-buffer) (gnuclient-buffers client))
	    (setq gnuserv-minor-mode t)
	    ;; Add the "Done" button to the menubar, only in this buffer.
	    (when (boundp 'current-menubar)
	      (set-buffer-menubar current-menubar)
	      (add-menu-button nil ["Done" gnuserv-edit t])))
	  (run-hooks 'gnuserv-visit-hook)
	  (pop list)))
      (cond
       ((and (or quick view)
	     (device-on-window-system-p device))
	;; Exit if on X device, and quick or view.  NOTE: if the
	;; client is to finish now, it must absolutely /not/ be
	;; included to the list of clients.  This way the client-ids
	;; should be unique.
	(gnuserv-write-to-client (gnuclient-id client) nil))
       (t
	;; Else, the client gets a vote.
	(push client gnuserv-clients)
	;; Explain buffer exit options.  If client-frame is non-nil,
	;; the user can exit via `delete-frame'.  OTOH, if FLAGS are
	;; nil and there are some buffers, the user can exit via
	;; `gnuserv-edit'.
	(if (and (not (or quick view))
		 (gnuclient-buffers client))
	    (message "%s"
		     (substitute-command-keys
		      "Type `\\[gnuserv-edit]' to finish editing"))
	  (and (gnuclient-frame client)
	       (message "%s"
			(substitute-command-keys
			 "Type `\\[delete-frame]' to finish editing")))))))))


;;; Functions that hook into Emacs in various way to enable operation

;; Defined later.
(add-hook 'kill-emacs-hook 'gnuserv-kill-all-clients t)

;; A helper function; used by others.  Try avoiding it whenever
;; possible, because it is slow, and conses a list.  Use
;; `gnuserv-buffer-p' when appropriate, for instance.
(defun gnuserv-buffer-clients (buffer)
  "Returns a list of clients to which BUFFER belongs."
  (let (res)
    (dolist (client gnuserv-clients res)
      (when (memq buffer (gnuclient-buffers client))
	(push client res)))))

;; Like `gnuserv-buffer-clients', but returns a boolean; doesn't
;; collect a list.
(defun gnuserv-buffer-p (buffer)
  (member* buffer gnuserv-clients
	   :test 'memq
	   :key 'gnuclient-buffers))

;; This function makes sure that a killed buffer is deleted off the
;; list for the particular client.
;;
;; This hooks into `kill-buffer-hook'.  It is *not* a replacement for
;; `kill-buffer' (thanks God).
(defun gnuserv-kill-buffer-function ()
  "Remove the buffer from the buffer lists of all the clients it belongs to.
Any client that remains \"empty\" after the removal is informed that the
editing has ended."
  (let* ((buf (current-buffer)))
    (dolist (client (gnuserv-buffer-clients buf))
      (callf2 delq buf (gnuclient-buffers client))
      ;; If no more buffers, kill the client.
      (when (null (gnuclient-buffers client))
	(gnuserv-kill-client client)))))

(add-hook 'kill-buffer-hook 'gnuserv-kill-buffer-function)

;; Ask for confirmation before killing a buffer that belongs to a
;; living client.
(defun gnuserv-kill-buffer-query-function ()
  (or gnuserv-kill-quietly
      (not (gnuserv-buffer-p (current-buffer)))
      (yes-or-no-p
       (format "Buffer %s belongs to gnuserv client(s); kill anyway? "
	       (current-buffer)))))

(add-hook 'kill-buffer-query-functions
	  'gnuserv-kill-buffer-query-function)

(defun gnuserv-kill-emacs-query-function ()
  (or gnuserv-kill-quietly
      (not (some 'gnuclient-buffers gnuserv-clients))
      (yes-or-no-p "Gnuserv buffers still have clients; exit anyway? ")))

(add-hook 'kill-emacs-query-functions
	  'gnuserv-kill-emacs-query-function)

;; If the device of a client is to be deleted, the client should die
;; as well.  This is why we hook into `delete-device-hook'.
(defun gnuserv-check-device (device)
  (when (memq device gnuserv-devices)
    (dolist (client gnuserv-clients)
      (when (eq device (gnuclient-device client))
	;; we must make sure that the server kill doesn't result in
	;; killing the device, because it would cause a device-dead
	;; error when `delete-device' tries to do the job later.
	(gnuserv-kill-client client t))))
  (callf2 delq device gnuserv-devices))

(add-hook 'delete-device-hook 'gnuserv-check-device)

(defun gnuserv-kill-client (client &optional leave-frame)
  "Kill the gnuclient CLIENT.
This will do away with all the associated buffers.  If LEAVE-FRAME,
the function will not remove the frames associated with the client."
  ;; Order is important: first delete client from gnuserv-clients, to
  ;; prevent gnuserv-buffer-done-1 calling us recursively.
  (callf2 delq client gnuserv-clients)
  ;; Process the buffers.
  (mapc 'gnuserv-buffer-done-1 (gnuclient-buffers client))
  (unless leave-frame
    (let ((device (gnuclient-device client)))
      ;; kill frame created by this client (if any), unless
      ;; specifically requested otherwise.
      ;;
      ;; note: last frame on a device will not be deleted here.
    (when (and (gnuclient-frame client)
	       (frame-live-p (gnuclient-frame client))
	       (second (device-frame-list device)))
      (delete-frame (gnuclient-frame client)))
    ;; If the device is live, created by a client, and no longer used
    ;; by any client, delete it.
    (when (and (device-live-p device)
	       (memq device gnuserv-devices)
	       (second (device-list))
	       (not (member* device gnuserv-clients
			     :key 'gnuclient-device)))
      ;; `gnuserv-check-device' will remove it from `gnuserv-devices'.
      (delete-device device))))
  ;; Notify the client.
  (gnuserv-write-to-client (gnuclient-id client) nil))

;; Do away with the buffer.
(defun gnuserv-buffer-done-1 (buffer)
  (dolist (client (gnuserv-buffer-clients buffer))
    (callf2 delq buffer (gnuclient-buffers client))
    (when (null (gnuclient-buffers client))
      (gnuserv-kill-client client)))
  ;; Get rid of the buffer.
  (save-excursion
    (set-buffer buffer)
    (run-hooks 'gnuserv-done-hook)
    (setq gnuserv-minor-mode nil)
    ;; Delete the menu button.
    (if (boundp 'current-menubar)
      (delete-menu-item '("Done")))
    (funcall gnuserv-done-function buffer)))


;;; Higher-level functions

;; Choose a `next' server buffer, according to several criteria, and
;; return it.  If none are found, return nil.
(defun gnuserv-next-buffer ()
  (let* ((frame (selected-frame))
	 (device (selected-device))
	 client)
    (cond
     ;; If we have a client belonging to this frame, return
     ;; the first buffer from it.
     ((setq client
	    (car (member* frame gnuserv-clients :key 'gnuclient-frame)))
      (car (gnuclient-buffers client)))
     ;; Else, look for a device.
     ((and
       (memq (selected-device) gnuserv-devices)
       (setq client
	     (car (member* device gnuserv-clients :key 'gnuclient-device))))
      (car (gnuclient-buffers client)))
     ;; Else, try to find any client with at least one buffer, and
     ;; return its first buffer.
     ((setq client
	    (car (member-if-not #'null gnuserv-clients
				:key 'gnuclient-buffers)))
      (car (gnuclient-buffers client)))
     ;; Oh, give up.
     (t nil))))

(defun gnuserv-buffer-done (buffer)
  "Mark BUFFER as \"done\" for its client(s).
Does the save/backup queries first, and calls `gnuserv-done-function'."
  ;; Check whether this is the real thing.
  (unless (gnuserv-buffer-p buffer)
    (error "%s does not belong to a gnuserv client" buffer))
  ;; Backup/ask query.
  (if (and (buffer-modified-p)
	   (y-or-n-p (concat "Save file " buffer-file-name "? ")))
      (save-buffer buffer))
  (gnuserv-buffer-done-1 buffer))

;; Called by `gnuserv-start-1' to clean everything.  Hooked into
;; `kill-emacs-hook', too.
(defun gnuserv-kill-all-clients ()
  "Kill all the gnuserv clients.  Ruthlessly."
  (mapc 'gnuserv-kill-client gnuserv-clients))

;; This serves to run the hook and reset
;; `allow-deletion-of-last-visible-frame'.
(defun gnuserv-prepare-shutdown ()
  (setq allow-deletion-of-last-visible-frame nil)
  (run-hooks 'gnuserv-shutdown-hook))

;; This is a user-callable function, too.
(defun gnuserv-shutdown ()
  "Shutdown the gnuserv server, if one is currently running.
All the clients will be disposed of via the normal methods."
  (interactive)
  (gnuserv-kill-all-clients)
  (when gnuserv-process
    (set-process-sentinel gnuserv-process nil)
    (gnuserv-prepare-shutdown)
    (condition-case ()
	(delete-process gnuserv-process)
      (error nil))
    (setq gnuserv-process nil)))

;; Actually start the process.  Kills all the clients before-hand.
(defun gnuserv-start-1 (&optional leave-dead)
  ;; Shutdown the existing server, if any.
  (gnuserv-shutdown)
  ;; If we already had a server, clear out associated status.
  (unless leave-dead
    (setq gnuserv-string ""
	  gnuserv-current-client nil)
    (let ((process-connection-type t))
      (setq gnuserv-process
	    (start-process "gnuserv" nil gnuserv-program)))
    (set-process-sentinel gnuserv-process 'gnuserv-sentinel)
    (set-process-filter gnuserv-process 'gnuserv-process-filter)
    (process-kill-without-query gnuserv-process)
    (setq allow-deletion-of-last-visible-frame t)
    (run-hooks 'gnuserv-init-hook)))


;;; User-callable functions:

;;;###autoload
(defun gnuserv-running-p ()
  "Return non-nil if a gnuserv process is running from this XEmacs session."
  (not (not gnuserv-process)))

;;;###autoload
(defun gnuserv-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
 This starts a gnuserv communications subprocess through which
 client \"editors\" (gnuclient and gnudoit) can send editing commands to 
 this Emacs job.  See the gnuserv(1) manual page for more details.
Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  (and gnuserv-process
       (not leave-dead)
       (message "Restarting gnuserv"))
  (gnuserv-start-1 leave-dead))

(defun gnuserv-edit (&optional count)
  "Mark the current gnuserv buffer as \"done\", and switch to next one.
Run with a numeric prefix argument, repeat the operation that number
 of times.  If given a universal prefix argument, close all the buffers
 of this buffer's clients.
The `gnuserv-done-function' (`kill-buffer' by default) is called to
 dispose of the buffer after marking it as done.
When all of a client's buffers are marked as \"done\", the client is
 notified."
  (interactive "P")
  (when (null count)
    (setq count 1))
  (cond ((numberp count)
	 (let (next)
	   (while (natnump (decf count))
	     (gnuserv-buffer-done (current-buffer))
	     (setq next (gnuserv-next-buffer))
	     (when next
	       (switch-to-buffer next)))))
	(count
	   (let* ((buf (current-buffer))
		  (clients (gnuserv-buffer-clients buf)))
	     (unless clients
	       (error "%s does not belong to a gnuserv client" buf))
	     (mapc 'gnuserv-kill-client (gnuserv-buffer-clients buf))))))

(global-set-key "\C-x#" 'gnuserv-edit)

(provide 'gnuserv)

;;; gnuserv.el ends here
