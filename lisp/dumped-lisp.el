(defvar packages-hardcoded-lisp
  '(
    ;; Nothing at this time
    )
  "Lisp packages that are always dumped with XEmacs.
This includes every package that is loaded directly by a package listed
in dumped-lisp.el and is not itself listed.")

(provide 'use-unidata-case-tables)

;; WARNING WARNING WARNING: None of the files below, until where it says
;; "All files after this can have extended characters in them", can have
;; extended (non-ASCII characters) of any sort in them!  Unfortunately, you
;; will not get any error at load-time; however, you may get a later very
;; cryptic error "Invalid opcode"!  This is caused by the byte-code data
;; being encoded as escape-quoted, when we can't handle that yet.
;;
;; #### We should resurrect the check for the coding-system magic cookie in
;; fileio.c and put in an abort if we are not able to handle it yet.

(setq preloaded-file-list
      (list
       ;; do not defcustom any variables in these files

       "backquote" 		; needed for defsubst etc.
       "bytecomp-runtime"	; define defsubst
       "subr" 			; load the most basic Lisp functions
       "cl"
       "cl-extra"	; also loads cl-macs if we're running interpreted.
       "cl-seq"
       "post-gc"
       "version"
       "custom"		; Before the world so everything can be customized
       "cus-start"	; for customization of builtin variables
       "find-paths"
       "packages"
       "setup-paths"
       "replace"
       "widget"
       "cmdloop"
       "keymap"
       "syntax"
       "device"
       "console"
       "obsolete"
       "specifier"
       "frame"			; needed by faces
       ;; #### this should be (featurep 'xft)
       (when (featurep 'xft-fonts) "fontconfig") ; needed by x-faces
       (when (featurep 'x) "x-faces") ; needed by faces
       (when (featurep 'gtk) "gtk-faces")
       (when (valid-console-type-p 'mswindows) "msw-faces")
       "faces"			; must be loaded before any make-face call
       ;;(pureload "facemenu") #### not yet ported
       "glyphs"
       "fontcolor"
       "extents"
       "events"
       "hash-table"
       "text-props"
       "process" ;; This is bad. network-streams may not be defined.
       (when (featurep 'multicast) "multicast") ; #+network-streams implicitly true
       "map-ynp"
       "undo-stack"
       "window"		; simple needs `save-window-excursion'
       "window-xemacs"
       "resize-minibuffer"	; simple needs `resize-minibuffer-mode'
       "simple"
       "newcomment"
       "keydefs"		; Before loaddefs so that keymap vars exist.
       "abbrev"
       "derived"
       "minibuf"
       "list-mode"
       "modeline"		; needs simple.el to be loaded first
;; If SparcWorks support is included some additional packages are
;; dumped which would normally have autoloads.  To avoid
;; duplicate doc string warnings, SparcWorks uses a separate
;; autoloads file with the dumped packages removed.
;; After fixing, eos/loaddefs-eos and loaddefs appear identical?!!
;; So just make loaddefs-eos go away...
;;(pureload (if (featurep 'sparcworks) "eos/loaddefs-eos" "loaddefs"))
       "cus-file"
       "startup"		; For initialization of
				;  `emacs-user-extension-dir'
       "misc"
       ;; (pureload "profile")
       "loadhist"		; Must be dumped before loaddefs is loaded
				; Used by help. 
       ;; (pureload "hyper-apropos")  Soon...
       "files"
       "lib-complete"
       "format"
       "indent"
       "isearch-mode"
       "buffer"
       "buff-menu"
       "paths"
       "lisp"
       "page"
       "register"
       "iso8859-1"		; This must be before any modes
				; (sets standard syntax table.)
       "easy-mmode"		; Added for 21.5. Used by help.
       "help"
       "easymenu"		; Added for 20.3.
       "lisp-mode"
       "text-mode"
       "fill"
       "auto-save"		; Added for 20.4
       "movemail"               ; Added for 21.2
       (when (eq system-type 'windows-nt) "win32-native")
       (when (featurep 'lisp-float-type) "float-sup")
       "itimer"			; for vars auto-save-timeout and
				; auto-gc-threshold
       "itimer-autosave"
       "printer"
       "behavior"
       "behavior-defs"
       "diagnose"
       
	;;;;;;;;;;;;;;;;;; GUI support
       (when (featurep 'window-system)
	 '("gui"
	   "mouse"
	   "mode-motion"
	   ))
       (when (featurep 'toolbar) "toolbar")
       (when (featurep 'scrollbar) "scrollbar")
       (when (featurep 'menubar) "menubar")
       (when (featurep 'dialog) "dialog")
       (when (featurep 'gutter) "gutter")
       (when (featurep 'dragdrop-api) "dragdrop")
       "select"
       
	;;;;;;;;;;;;;;;;;; Content for GUI's
       ;; There used to be window-system inserted in the when-feature,
       ;; but IMHO your configure script should turn off the menubar,
       ;; toolbar, etc. features when there is no window system.  We
       ;; should just be able to assume that, if (featurep 'menubar),
       ;; the menubar should work and if items are added, they can be
       ;; seen clearly and usefully.
       (when (featurep 'menubar) "menubar-items")
       (when (featurep 'gutter) "gutter-items")
       (when (featurep 'toolbar) "toolbar-items")
       (when (featurep 'dialog) "dialog-items")

	;;;;;;;;;;;;;;;;;; Coding-system support
       "coding"
       "code-files"
       ;; Handle process with encoding/decoding coding-system.
       "code-process"
       ;; Provide basic commands to set coding systems to user
       "code-cmds"
       ;; Initialize Unicode and load the translation tables for
       ;; the built-in charsets.
       "unicode"
	;;;;;;;;;;;;;;;;;; MULE support
       (when (featurep 'mule)
	 '("mule/mule-charset" ;; needs to load after unicode in old-Mule
	   "mule/mule-cmds" ; to sync with Emacs 20.1
	   "mule/mule-coding"
	   "mule/mule-composite-stub"
	   "mule/mule-composite"
	   "mule/windows" ; for creating Windows charsets/coding systems
	   ))
       ;; may initialize coding systems
       (when (featurep '(and mule x)) "mule/mule-x-init")
       (when (featurep '(and mule tty)) "mule/mule-tty-init")
       (when (and (featurep 'mule) (memq system-type '(windows-nt cygwin32)))
	 "mule/mule-win32-init")
       "code-init" ; set up defaults

;;; ***************************************************************************
;;;           All files after this can have extended characters in them.
;;; ***************************************************************************

       (when (featurep 'mule)
	 '("mule/mule-category"
	   "mule/kinsoku"
	   ))

;; after this goes the specific lisp routines for a particular input system
;; 97.2.5 JHod Shouldn't these go into a site-load file to allow site
;; or user switching of input systems???
;(if (featurep 'wnn)
;    (progn
;      (pureload "egg")
;      (pureload "egg-wnn")
;      (setq egg-default-startup-file "eggrc-wnn")))

;; (if (and (boundp 'CANNA) CANNA)
;;     (pureload "canna")
;;   )

;; Now load files to set up all the different languages/environments that
;; Mule knows about.  Formerly we had to worry about files shadowed by
;; those of the same name in leim/quail.el, but no longer, since we now
;; compile with -no-packages.

       (when (featurep 'mule)
	 '("mule/general-early"
	   "mule/arabic"
	   "mule/chinese"
	   "mule/cyrillic"
	   "mule/english"
	   "mule/ethiopic"
	   "mule/greek"
	   "mule/hebrew"
	   "mule/indian"
	   "mule/devanagari" ; must be loaded after indian.el
	   "mule/japanese"
	   "mule/korean"
	   "mule/lao" ; sucks. 
	   "mule/latin"
	   "mule/misc-lang"
	   "mule/thai"
	   "mule/tibetan"
	   "mule/vietnamese"
	   ))
	    
	;; Specialized language support
       (when (featurep 'mule) "mule/canna-leim")
	;; needs access to the charsets created by the above
	;; language-specific files.
       (when (and (featurep 'mule) (valid-console-type-p 'mswindows))
	 "mule/mule-msw-init-late")

       ;; in old-Mule, must be loaded after all charsets created
       (when (and (featurep 'mule) (featurep 'use-unidata-case-tables))
	 "mule/uni-case-conv")
       (when (featurep 'mule) "mule/general-late")

;;; mule-load.el ends here

;; preload the X code.
       (when (featurep '(and x scrollbar)) "x-scrollbar")
       (when (featurep 'x)
	 '("x-mouse"
	   "x-select"
	   "x-misc"
	   "x-init"
	   "x-win-xfree86"
	   "x-win-sun"))
       ;; preload the GTK code
       (when (featurep 'gtk)
	 '("gtk-ffi"
	   "gtk-widgets"
	   "gdk"
	   "gtk-init"
	   "gtk-select"
	   "gtk-mouse"
	   "gtk-glyphs"
	   "widgets-gtk"))
       (when (featurep '(and gtk dialog)) "dialog-gtk")
       (when (featurep 'glade) "glade")

;; preload the mswindows code.
       (when (valid-console-type-p 'mswindows)
	 '("msw-glyphs"
	   "msw-mouse"
	   "msw-init"
	   "msw-select"))
;; preload the TTY init code.
       (when (featurep 'tty) "tty-init")
;;; Formerly in tooltalk/tooltalk-load.el
	;; Moved to tooltalk package
        ;; (when (featurep 'tooltalk)
        ;;   '("tooltalk-macros" "tooltalk-util" "tooltalk-init"))
	;; "vc-hooks"		; Packaged.  Available in two versions.
	;; "ediff-hook"		; Packaged.
       "fontl-hooks"
       "auto-show"
       "paragraphs"             ; needs easy-mmode, coding
       (when (featurep 'ldap) "ldap")

;; (when (featurep 'energize) "energize/energize-load.el")
;;; formerly in sunpro/sunpro-load.el
;;	(when (featurep '(and mule sparcworks)) "mime-setup")

	;; Moved to Sun package
	;; (when (featurep 'sparcworks)
	;;   '("cc-mode" ; Requires cc-mode package
	;;     "sunpro-init"
	;;     "ring"
	;;     "comint" ; Requires comint package
	;;     "annotations"))

;;; formerly in eos/sun-eos-load.el
        ;; (when (featurep 'sparcworks)
        ;;   '("sun-eos-init"
        ;;     "sun-eos-common"
        ;;     "sun-eos-editor"
        ;;     "sun-eos-browser"
        ;;     "sun-eos-debugger"
        ;;     "sun-eos-debugger-extra"
        ;;     "sun-eos-menubar"))
       "loaddefs"		; <=== autoloads get loaded here
	))

(setq preloaded-file-list
      (mapcan #'(lambda (x) (if (listp x) x (list x))) preloaded-file-list))
