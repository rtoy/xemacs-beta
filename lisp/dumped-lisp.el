(defvar packages-hardcoded-lisp
  '(
    ;; Nothing at this time
    )
  "Lisp packages that are always dumped with XEmacs.
This includes every package that is loaded directly by a package listed
in dumped-lisp.el and is not itself listed.")

(setq preloaded-file-list
      (list
       "backquote" 		; needed for defsubst etc.
       "bytecomp-runtime"	; define defsubst
       "find-paths"
       "packages"		; Bootstrap run-time lisp environment
       "setup-paths"
       "subr" 			; load the most basic Lisp functions
       "post-gc"
       "replace" 		; match-string used in version.el.

       "version.el"             ; Ignore compiled-by-mistake version.elc

       "cl"
       "cl-extra"
       "cl-seq"
       "widget"
       "custom"		; Before the world so everything can be
			; customized
       "cus-start"	; for customization of builtin variables
       "cmdloop"
       "keymap"
       "syntax"
       "device"
       "console"
       "obsolete"
       "specifier"
       "frame"			; needed by faces
       (when (featurep 'x) "x-faces") ; needed by faces
       (when (featurep 'gtk) "gtk-faces")
       (when (valid-console-type-p 'mswindows) "msw-faces")
       "faces"			; must be loaded before any make-face call
       ;;(pureload "facemenu") #### not yet ported
       "glyphs"
       "objects"
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
       "simple"
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
       "help"
       ;; (pureload "hyper-apropos")  Soon...
       "files"
       "lib-complete"
       "format"
       "indent"
       "isearch-mode"
       "buffer"
       "buff-menu"
       "paths.el"		; don't get confused if paths compiled.
       "lisp"
       "page"
       "register"
       "iso8859-1"		; This must be before any modes
				; (sets standard syntax table.)
       "paragraphs"
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
       (when (featurep '(and (not infodock) menubar)) "menubar-items")
       (when (featurep '(and gutter)) "gutter-items")
       (when (featurep '(and (not infodock) toolbar)) "toolbar-items")
       (when (featurep '(and (not infodock) dialog)) "dialog-items")

	;;;;;;;;;;;;;;;;;; Coding-system support
       "coding"
       "code-files"
       ;; Handle process with encoding/decoding coding-system.
       "code-process"
       ;; Provide basic commands to set coding systems to user
       "code-cmds"
       "unicode"
	;;;;;;;;;;;;;;;;;; MULE support
       (when (featurep 'mule)
	 '("mule-charset"
	   "mule-cmds" ; to sync with Emacs 20.1
	   "mule-coding"
	   "mule-composite-stub"
	   "mule-composite"
	   ))
       ;; may initialize coding systems
       (when (featurep '(and mule x)) "mule-x-init")
       (when (featurep '(and mule tty)) "mule-tty-init")
       (when (and (featurep 'mule) (memq system-type '(windows-nt cygwin32)))
	 "mule-win32-init")
       "code-init" ; set up defaults
       ;; All files after this can have extended characters in them.
       (when (featurep 'mule)
	 '("mule-category"
	   "mule-ccl"
	   "kinsoku"
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
	 '("arabic"
	   "chinese"
	   "cyrillic"
	   "english"
	   "ethiopic"
	   "european"
	   "greek"
	   "hebrew"
	   "indian"
	   "devanagari" ; must be loaded after indian.el
	   "japanese"
	   "korean"
	   "lao"
	   "latin"
	   "misc-lang"
	   ;; "thai" #### merge thai and thai-xtis!!!
	   "thai-xtis"
	   "tibetan"
	   "vietnamese"
	   ))
	    
	;; Specialized language support
       (when (featurep '(and mule CANNA)) "canna-leim")
;; Egg/Its is now a package
;	(when (featurep '(and mule wnn))
;         '("egg-leim" "egg-kwnn-leim" "egg-cwnn-leim"))
;	(when (featurep 'mule) "egg-sj3-leim")
;; SKK is now a package
;	(when (featurep 'mule) "skk-leim")

;; Enable Mule capability for Gnus, mail, etc...
;; Moved to sunpro-load.el - the default only for Sun.
;;(pureload "mime-setup")

	;; needs access to the charsets created by the above
	;; language-specific files.
       (when (and (featurep 'mule) (valid-console-type-p 'mswindows))
	 "mule-msw-init-late")

;;; mule-load.el ends here

;; preload InfoDock stuff.  should almost certainly not be here if
;; id-menus is not here.  infodock needs to figure out a clever way to
;; advise this stuff or we need to export a clean way for infodock or
;; others to control this programmatically.
       (when (featurep '(and infodock (or x mswindows gtk) menubar))
	 "id-menus")
;; preload the X code.
       (when (featurep '(and x scrollbar)) "x-scrollbar")
       (when (featurep 'x)
	 '("x-iso8859-1"
	   "x-mouse"
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
	   "gtk-iso8859-1"
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
       "resize-minibuffer"
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
       "loadhist"		; Must be dumped before loaddefs is loaded
       "loaddefs"		; <=== autoloads get loaded here
	))

(setq preloaded-file-list
      (apply #'nconc
	     (mapcar #'(lambda (x)
			 (if (listp x) x (list x)))
		     preloaded-file-list)))
