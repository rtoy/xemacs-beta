;;; sun-eos-menu.el --- Implements the XEmacs/SPARCworks menubar

;; Copyright (C) 1995  Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks menubar

;;; Commentary:
;; This file contains functions that populate a SPARCworks menu
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

(require 'eos-common "sun-eos-common")

(defun eos::toggle-sbrowser-selected-frame ()
  ;; Toggle whether this frame is selected for SBrowser
  (interactive)
  (if (equal eos::sbrowser-frame (selected-frame))
      (eos::select-sbrowser-frame nil)
    (eos::select-sbrowser-frame (selected-frame)))
  )

(defun eos::toggle-debugger-selected-frame ()
  ;; Toggle whether this frame is selected for Debugger
  (interactive)
  (if (equal eos::debugger-frame (selected-frame))
      (eos::select-debugger-frame nil)
    (eos::select-debugger-frame (selected-frame)))
  )

(defvar eos::long-menu
  '(
    ["Read and Execute a Dbx Command" eos::dbx-cmd (not (eq eos::key-mode 'none))]
    ["Run" eos::run (not (eq eos::key-mode 'none))]
    ["Fix" eos::fix (not (eq eos::key-mode 'none))]
    "-----"
    ["Print" eos::print (not (eq eos::key-mode 'none))]
    ["Print *" eos::print* (not (eq eos::key-mode 'none))]
    ["Dismiss Print" eos::dismiss-print-frame (not (eq eos::key-mode 'none))]
    "-----"
    ["Continue" eos::cont (not (eq eos::key-mode 'none))]
    ["Stop" eos::stop-at (not (eq eos::key-mode 'none))]
    ["Clear" eos::clear-at (not (eq eos::key-mode 'none))]
    ["Next" eos::next (not (eq eos::key-mode 'none))]
    ["Step" eos::step (not (eq eos::key-mode 'none))]
    ["Step Up" eos::step-up (not (eq eos::key-mode 'none))]
    ["Continue To" eos::cont-to (not (eq eos::key-mode 'none))]
    "-----"
    ["Stack Up" eos::up (not (eq eos::key-mode 'none))]
    ["Stack Down" eos::down (not (eq eos::key-mode 'none))]
    "-----"
    ("Start Tool and Enable Frame"
     ["Debugger" eos::start-debugger t]
     ["Dbx" eos::start-dbx t]
     ["SBrowser" eos::start-sbrowser t]
    )
    "-----"
    ["Enable Frame for SBrowser"
     eos::toggle-sbrowser-selected-frame
     :style toggle
     :selected (equal eos::sbrowser-frame
		      (selected-frame))]
    ["Enable Frame for Debugger and Dbx"
     eos::toggle-debugger-selected-frame
     :style toggle
     :selected (equal eos::debugger-frame
		      (selected-frame))]
    "-----"
    ["News..." eos::sw-news t]
    )
  )

(defvar eos::short-menu
  '(
    ("Start Tool and Enable Frame"
     ["Debugger" eos::start-debugger t]
     ["Dbx" eos::start-dbx t]
     ["SBrowser" eos::start-sbrowser t]
    )
    "-----"
    ["Enable Frame for SBrowser"
     eos::toggle-sbrowser-selected-frame
     :style toggle
     :selected (equal eos::sbrowser-frame
		      (selected-frame))]
    ["Enable Frame for Debugger and Dbx"
     eos::toggle-debugger-selected-frame
     :style toggle
     :selected (equal eos::debugger-frame
		      (selected-frame))]
    "-----"
    ["News..." eos::sw-news t]
    )
  )

(defun eos::menubar-startup ()
  ;; Actions to do at startup for eos-menubar.el
  (if (and (eq (device-type (selected-device)) 'x)
	   (or (not (local-variable-p 'current-menubar (current-buffer)))
	       (yes-or-no-p
		"SPARCworks menu will be local (menubar is buffer-local); proceed?")))
      (progn
	(add-menu-button '("Help") ["SPARCworks..." eos::sw-intro t])
	(add-submenu nil
		     (append '("SPARCworks") (copy-tree eos::short-menu))
		     "VC"
		     )
	)))

;;
;; Insertion of text with a font
;;

(defun eos::insert-italics (a-string)
  (eos::insert-with-font a-string 'italic))

(defun eos::insert-bold (a-string)
  (eos::insert-with-font a-string 'bold))

(defun eos::insert-with-font (a-string a-font)
  (interactive "")
  (let (a b ext)
    (setq a (point))
    (insert a-string)
    (setq b (point))
    (setq ext (make-extent a b))
    (set-extent-face ext (find-face a-font))
    ))

;;
;; Generic insert code
;;

(defun eos::insert (s)
  (let ((len (length s))
	(pos 0)
	(newpos 0)
	(state 'normal))
    (while (< pos len)
      (setq newpos (string-match "#[bnir]" s pos))
      (if (and newpos (> newpos pos))
	  (progn
	    (cond ((equal (aref s (+ newpos 1)) ?b) ; bold
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'bold))
		     (error "found bold when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?r) ; red
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'red))
		     (error "found red when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?i) ; italics
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'italics))
		     (error "found italics when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?n) ; normal
		   (cond ((equal state 'italics)
			  (eos::insert-italics (substring s pos newpos))
			  (setq state 'normal))
			 ((equal state 'bold)
			  (eos::insert-bold (substring s pos newpos))
			  (setq state 'normal))
			 ((equal state 'normal)
			  (error "found normal when in normal"))))
		  (t
		   (error "internal error"))
		  )
	    (setq pos (+ newpos 2))
	    )
	(if (equal state 'normal)
	    (progn
	      (insert (substring s pos))
	      (setq pos len))
	  (error "eos::insert with unclosed special font"))
	))
    ))

;;
;; Introduction File
;;

(defun eos::sw-intro ()
  "Generate an intro buffer."
  (interactive)
  (let ((buffer1 (get-buffer-create " *SPARCworks Intro*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bSPARCworks Editor Integration#n
	    Eos is copyright (c) 1995 by Sun Microsystems.

#bIntroduction (for Eos 1.5.x)#n

#iSPARCworks#n is a set of integrated programming tools from SunSoft that
support the program development cycle. #iXEmacs#n is a version of the Emacs
editor that includes interfaces to the selection service and to the
#iToolTalk#n service.  The #iEos#n package uses these two interfaces to provide
a simple yet useful editor integration with three SPARCworks tools:
the #iSourceBrowser#n, the #iDebugger#n and #iDbx#n.  Eos requires XEmacs 19.12
or above, and SW3.0.1 or above.

When used with Eos, the Debugger and SourceBrowser do not include a
source pane for displaying of sources and instead use an XEmacs frame.
Then the user can interact with the XEmacs frame in a way very similar
to how the source panes of the SW tools would be used.  The user can also
start Dbx and request that sources be shown in XEmacs.

#bSimple Startup#n

In most cases, the user will start an interaction with Eos as follows:

 (1) Start XEmacs,

 (2) Load \"eos.el\" to add a SPARCworks submenu to the menubar (this
step might not be needed if Eos is preloaded to your XEmacs binary), and

 (3) On some XEmacs frame use the SPARCworks submenu and start the
desired tool and simultaneously enable that frame to display sources.

The toolbar for the enabled frame will change after (3) to show that
this frame will behave as the source display for the SW tool and to
indicate that some actions on the tool can be performed from this frame.

The actions available depend on the SW tool.  The interaction model for
the Debugger and the SourceBrowser can be described as #iselect on the
XEmacs frame and then click on the button on the SW tool#n. As an example,
a browser query can be performed by selecting some text and then clicking
on the query button on the SBrowser tool; the source for the first match
will appear in the XEmacs frame, together with a glyph showing the match.

The Debugger and Dbx can also be driven from XEmacs.  Most frequently
this will be done using the ToolBar.  Entries in the toolbar of a frame
enabled for debugging are deactivated when there is not enough information
to invoke their associated commands (due to technical reasons, it is
necessary for XEmacs to have had a frame enabled for Debugger/Dbx when
a debug or attach command was issued to Debugger/Dbx to make most toolbar
commands active).  As an example, to set a breakpoint at some line, select
a position in that line and then click on the toolbar icon with the stop
with the arrow inside.

#bDetails#n

#iManual Startup#n

In the scenario described above, the user simultaneously starts a tool
and enables a frame for that tool. The two actions can also be done
independently. The tools (Source Browser, Debugger, and Dbx) have to
be started with the \"-editor\" option and the XEmacs frame can be
enabled manually using the SPARCworks submenu.  The most common use
of this feature is to disable and re-enable a frame, be it to recover
the default toolbar, or to avoid conflicts with other active tools
(see the paragraph below on multiple active tools).

#iFrame Enabling#n

At any given time there can be at most one frame enabled to display
Source Browser sources, and at most one frame enabled to display
Debugger and Dbx sources.  The same XEmacs frame can be used for both
types of sources.  The toolbar of an enabled frame always starts with
an informational icon.  This icon is a large-font #ii#n with either a
smaller-font #iB#n, if the frame has browsing enabled, and/or a smaller-font
#iD#n, if the frame has debugging enabled.

If no frames are enabled for a given tool, the editor integration for
that tool is disabled. This means that XEmacs deregisters the TT
patterns relevant to this tool, and XEmacs does not receive any
messages from that tool.

#iMultiple Active Tools#n

In order to provide a simpler user model, Eos has no provisions to
#igracefully#n support more than one simultaneous active tool of a
given class per TT session. A Debugger and a SourceBrowser, or a Dbx
and a SourceBrowser, can coexist gracefully, but a Debugger and a Dbx
cannot, and neither can two SourceBrowsers, two Debuggers, or two
dbxs.  This simplification is consistent with the needs of most users.

The implementation of Eos notifies the user if she attempts to start two
conflicting tools, but it does not enforce the restriction.  In some
cases two conflicting tools can be used profitably by a careful user,
but in others the result is likely to be chaos.  An example of the first
is using two SourceBrowsers, and one of the later is attempting to send
debugging commands from XEmacs to two debuggers.

If a user really needs to have multiple active tools, she can do this
in a safe way by creating several TT sessions (e.g. using #ittsession
-c /bin/csh#n, see the man page for ttsession), and placing the tools
with their own XEmacses in separate TT sessions.

#iA Visual Data Inspector in XEmacs#n

Users that choose to drive the debugger from XEmacs also have
available a #ivery simple#n but fast visual data inspector.  The results
of #iprint#n and #iprint*#n commands are formatted into an XEmacs buffer
(#i\"*Eos Print Output*\"#n) and presented into a separate frame.
This frame is mapped and unmapped so that, except for the first time,
it appears quickly.

#iBuffers for Debugger/Dbx Interaction#n

When starting dbx as a subprocess, a buffer will be created to interact
with dbx.  The name of this buffer is of the form #i\"*Eos dbx*\"#n.

If a dbx engine is receiving requests from both Debugger and XEmacs
(e.g. it was started via #idebugger -editor#n), the responses to
commands sent by XEmacs will be shown in the echo area and will be
recorded in a read-only buffer (#i\"*Eos Debugger Log*\"#n), but responses
to Debugger commands will not appear.  Conversely, responses to Debugger
commands will appear in the Debugger transcript pane but not in XEmacs's
log buffer.  This is a limitation of the underlying TT protocols.

#bTTY Support#n

Although tty support is not an official part of Eos, it is possible
with some extra effort and specialized knowledge from the user.

#iStarting a ToolTalk Session#n

Eos requires a ToolTalk communication.  This may require starting a TT
session by:

 (0) Start a ToolTalk session, and a shell so that all processes
started from this shell will use the new TT session.  Do this by
executing \"ttsession -c /bin/csh\" - or whatever shell you use

At this point, you can start your XEmacs on that shell, as shown in
step (1) above.  Note that, since there is no TTY toolbar in 19.12
(nor 19.13), an alternative mechanism must be used to enable the
(tty) frame.

A typical use for tty is to interact with dbx. The command
#ieos::start-dbx#n will select the tty frame for debugging and will start
a dbx buffer.  From this point on, dbx will use this tty frame to show
its sources.  The introduction and news messages can be generated
using the commands #ieos::sw-intro#n and #ieos::sw-news#n.  You can interact
with the dbx subprocess by typing to its associated input buffer or
using some key bindings.

#iKey Bindings#n

A tty user can interact with Eos by invoking directly the Eos
commands, evaluating elisp expressions, or through some key-bindings.
The expert user may provide her own key bindings.  Eos also provides two
set of global bindings, which are activated by evaluating the
expressions (eos::set-key-mode 'prefix) or (eos::set-key-mode
'function).

#bKnown Bugs#n

Due to a bug in the internal subprocess machinery of XEmacs 19.12, the
default prompt of dbx subprocesses will show the full path to the binary.
The prompt can be overridden using the ksh variable PS1\; one way to do
this is by adding the following line to your ~/.dbxrc:

	PS1='(dbx) '

#bFeedback#n

You are encouraged to send us feedback via the Comments button in
the About Box of either SPARCworks tool, or directly to
eos-comments@cs.uiuc.edu.

#bEnjoy.#n")
   (setq buffer-read-only t)
   (goto-char (point-min))
   (view-mode nil 'kill-buffer)		;; assume the new view-less
   ))

;;
;; Cheat Sheets for keyboard mappings
;;
;; This depends on the mapping being used!
;;

(defun eos::sw-cheat-sheet ()
  "Generate buffer that has a description of the key maps that can be
printed, cut and then taped somewhere (like on the keyboard or on your
monitor).  This is particularly useful for the function keys"
  (interactive)
  (let ((buffer1 (get-buffer-create " *Cheat Sheets*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bCheat Sheets for Eos#n

This buffer has a description of the key maps that can be printed, cut
and then taped somewhere (like on the keyboard or on your monitor).
This is particularly useful for the function keys since their numbers
don't any particular mnemonic value.


#bWhen using function keys#n #i[Options->SPARCworks->Use Function Keys]#n

----------------------------------------

F6      F7        F8             F9

Do      Print     Cont    ----   Next
Run     Print*    Stop   <Ctrl>  Step
Fix     Dismiss   Clear  <Shft>  Step Up


----------------------------------------

#bWhen using prefix map#n #i[Options->SPARCworks->Use C-c d Prefix Map]#n

----------------------------------------
Basic prefix: C-c d


	Do	 %
	Run	 r
	Fix	 f

	Print	 p
	Print*	 C-p

	Cont	 c
	Stop	 b (for breakpoint)
	Clear	 C-b

	Next	 n
	Step	 s
	Step up  C-s

	Up       u
	Down     d
----------------------------------------

")
   (setq buffer-read-only t)
   (goto-char (point-min))
   (view-mode nil 'kill-buffer)		;; assume the new view-less
   ))

;;
;; News files
;;

(defun eos::sw-news ()
  "Generate a News buffer."
  (interactive)
  (let ((buffer1 (get-buffer-create " *Eos News*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bEos News#n

See the #iHelp#n top-level menu for additional information on the
SPARCworks lightweight editor integration (Eos).  The current version
of Eos is available as the contents of the variable eos::version.

#bversion 1.5.2#n

	Support for 19.12 and 19.13.  Works on TTYs. Uses real ToolBar.
	Toolbars for debugger & content inspector are frame-local.
	Better icons and glyphs.  Support for (load-library \"eos\").
	Ease-of-use: startup for tools.
	Icon files are now defined \"in-line\" to simplify administration.

	Removed the following to simplify use:
	 - Textual toolbar (from 1.4).
	 - Option submenu to add keymaps for debugger use.
	 - Popup menu.
	 - Any pretenses to support SW3.0; use SW3.0.1 instead.

#bversion 1.4.1#n

	Added eos::add-button interface.

#bversion 1.4#n

	Added toolbar like in dbxtool.  Toolbar uses echo-help to show
	meaning of buttons, (setq inhibit-help-echo t) if you don't
	want it.

	Selection now remains after \"print\"-like commands.  Now it
	is possible to have the	*debugger* buffer in the frame selected
	for displaying debugged	sources.

	Added a command to relayout debugger buffers so they show in
	a layout similar to that of dbxtool.

#bversion 1.3#n

	Provided popup-menu bindings for those debugger actions
	that operate on the contents of the selection or its position;
	selectable via options.

	The *debugger* buffer now support M-p and M-n.

#bversion 1.2#n

	Better support for interactions via *debugger* buffer and directly
	using a prefix map and function keys.

	Converted to use new toggle and radio menus, reorganizing
	SPARCworks menu to factor out help and options into submenus,
	which are now available under the Options and Help top-level menus.

#bversion 1.1#n

	Some internal cleanup.

	Eos now provides basic machinery to drive the debugger
	engine directly using ToolTalk messages.  This feature is
	not yet very well polished. You can try using it at your own risk,
	or await for release 1.2 (soon to come) that will provide a better
	interface and improved functionality, as well as documentation
	for the interface.

#bversion 1.0#n

	First widely available release.  Supports simple #iselect and click#n model.

#bPossible Future Enhancements#n

* Add a \"peek-in-source\" mechanism to show the values of
  expressions in the sources.

* The comint package should be generalized to allow for TT-based
  interpreters and it should be used in Eos.

* Key & popup bindings should probably be a minor mode (currently
  it conflicts with cc-mode).

* Should support locking a print frame to force new print frames.  Also,
  should allow for following fields in print frames.


#bFeedback#n

	Send feedback to #ieos-comments@cs.uiuc.edu#n")
   (setq buffer-read-only t)
   (goto-char (point-min))
   (view-mode nil 'kill-buffer)		;; assume the new view-less
   ))

(provide 'eos-menubar)

;;; sun-eos-debugger.el ends here
