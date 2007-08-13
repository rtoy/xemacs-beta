(setq preloaded-file-list
      '("backquote" 		; needed for defsubst etc.
	"bytecomp-runtime"	; define defsubst
	"packages"		; Bootstrap run-time lisp environment
	"subr" 			; load the most basic Lisp functions
	"replace" 		; match-string used in version.el.
	; Ignore compiled-by-mistake version.elc
	#-infodock "version.el"	; XEmacs
	#+infodock "id-version.el" ; InfoDock
	"cl"
	"cl-extra"
	"cl-seq"
	"widget"
	"custom"		; Before the world so everything can be
				; customized
	"cus-start"		; for customization of builtin variables
	"cmdloop"
	"keymap"
	"syntax"
	"device"
	"console"
	"obsolete"
	"specifier"
	"faces"			; must be loaded before any make-face call
;;(load-gc "facemenu") #### not yet ported
	"glyphs"
	"objects"
	"extents"
	"events"
	"text-props"
	"process"
	"frame"			; move up here cause some stuff needs it here
	"map-ynp"
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
;;(load-gc (if (featurep 'sparcworks) "eos/loaddefs-eos" "loaddefs"))
	"startup"		; For initialization of
				;  `emacs-user-extension-dir'
	"misc"
	;; (load-gc "profile")
	#-mule "help-nomule"
	"help"
	;; (load-gc "hyper-apropos")  Soon...
	#-mule "files-nomule"
	"files"
	#-infodock "lib-complete" ; InfoDock uses an older version
	"format"
	"indent"
	"isearch-mode"
	"buffer"
	"buff-menu"
	"undo-stack"
	"window"
	"window-xemacs"
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

	#+windows-nt "winnt"
	#+lisp-float-type "float-sup"
	"itimer"		; for vars auto-save-timeout and
				; auto-gc-threshold
	"itimer-autosave"
	#+toolbar "toolbar"
	#+scrollbar "scrollbar"
	#+menubar "menubar"
	#+dialog "dialog"
	#+mule "mule-charset"
	#+mule "mule-coding"
;; Handle I/O of files with extended characters.
	#+mule "mule-files"
;; Handle process with encoding/decoding non-ascii coding-system.
	#+mule "mule-process"
	#+mule "mule-help"
;; Load the remaining basic files.
	#+mule "mule-category"
	#+mule "mule-ccl"
	#+mule "mule-misc"
	#+mule "kinsoku"
	#+(and mule x) "mule-x-init"
	#+mule "mule-cmds" ; to sync with Emacs 20.1

;; after this goes the specific lisp routines for a particular input system
;; 97.2.5 JHod Shouldn't these go into a site-load file to allow site
;; or user switching of input systems???
;(if (featurep 'wnn)
;    (progn
;      (load-gc "egg")
;      (load-gc "egg-wnn")
;      (setq egg-default-startup-file "eggrc-wnn")))

;; (if (and (boundp 'CANNA) CANNA)
;;     (load-gc "canna")
;;   )

;; Now load files to set up all the different languages/environments
;; that Mule knows about.

	#+mule "arabic"
	#+mule "chinese"
	#+mule "cyrillic"
	#+mule "english"
;;	#+mule "ethiopic"
	#+mule "european"
	#+mule "greek"
	#+mule "hebrew"
	#+mule "japanese"
	#+mule "korean"
	#+mule "misc-lang"
;;	#+mule "thai"
	#+mule "viet-chars"
;;	#+mule "vietnamese"

	;; Specialized language support
	#+(and mule CANNA) "canna-leim"
;; Egg/Its is now a package with its own dumped-lisp.el file
;	#+(and mule wnn) "egg-leim"
;	#+(and mule wnn) "egg-kwnn-leim"
;	#+(and mule wnn) "egg-cwnn-leim"
;	#+mule "egg-sj3-leim"
;; SKK is now a package with its own dumped-lisp.el file
;	#+mule "skk-leim"

;; Set up the XEmacs environment for Mule.
;; Assumes the existence of various stuff above.
	#+mule "mule-init"

;; Enable Mule capability for Gnus, mail, etc...
;; Moved to sunpro-load.el - the default only for Sun.
;;(load-gc "mime-setup")
;;; mule-load.el ends here
	#+window-system "gui"
	#+window-system "mode-motion"
	#+window-system "mouse"
;; preload the X code, for faster startup.
	#+(and (not infodock) (or x mswindows) menubar) "x-menubar"
	#+(and infodock (or x mswindows) menubar) "id-menus"
	#+x "x-faces"
	#+x "x-iso8859-1"
	#+x "x-mouse"
	#+x "x-select"
	#+(and x scrollbar) "x-scrollbar"
	#+x "x-misc"
	#+x "x-init"
	#+(and (not infodock) x toolbar) "x-toolbar"
	#+x "x-win-xfree86"
	#+x "x-win-sun"
;; preload the mswindows code.
	#+mswindows "msw-faces"
	#+mswindows "msw-init"
	#+mswindows "msw-select"
;; preload the TTY init code.
	#+tty "tty-init"
;;; Formerly in tooltalk/tooltalk-load.el
	;; Moved to tooltalk package
	;; #+tooltalk "tooltalk-macros"
	;; #+tooltalk "tooltalk-util"
	;; #+tooltalk "tooltalk-init"
	;; "vc-hooks"		; Packaged.  Available in two versions.
	;; "ediff-hook"		; Packaged.
	"fontl-hooks"
	"auto-show"
;; #+energize "energize/energize-load.el"
;;; formerly in sunpro/sunpro-load.el
;;	#+(and mule sparcworks) "mime-setup"

	;; Moved to Sun package
	;; #+sparcworks "cc-mode" ; Requires cc-mode package
	;; #+sparcworks "sunpro-init"
	;; #+sparcworks "ring"
	;; #+sparcworks "comint" ; Requires comint package
	;; #+sparcworks "annotations"

;;; formerly in eos/sun-eos-load.el
;;	#+sparcworks "sun-eos-init"
;;	#+sparcworks "sun-eos-common"
;;	#+sparcworks "sun-eos-editor"
;;	#+sparcworks "sun-eos-browser"
;;	#+sparcworks "sun-eos-debugger"
;;	#+sparcworks "sun-eos-debugger-extra"
;;	#+sparcworks "sun-eos-menubar"
	"loadhist"		; Must be dumped before loaddefs is loaded
	"loaddefs"		; <=== autoloads get loaded here
))
