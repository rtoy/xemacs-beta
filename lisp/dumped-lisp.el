(setq preloaded-file-list
      (assemble-list
        "backquote" 		; needed for defsubst etc.
	"bytecomp-runtime"	; define defsubst
	"find-paths"
	"packages"		; Bootstrap run-time lisp environment
	"setup-paths"
	"dump-paths"
	"subr" 			; load the most basic Lisp functions
	"post-gc"
	"replace" 		; match-string used in version.el.
	; Ignore compiled-by-mistake version.elc
	"version.el"
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
	"frame"			; needed by faces
	(when-feature x "x-faces") ; needed by faces
 	(when-feature gtk "gtk-faces")
	(when-feature mswindows "msw-faces")
	"faces"			; must be loaded before any make-face call
;;(pureload "facemenu") #### not yet ported
	"glyphs"
	"objects"
	"extents"
	"events"
	"hash-table"
	"text-props"
	"process" ;; This is bad. network-streams may not be defined.
	(when-feature multicast "multicast") ; #+network-streams implicitly true
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
	"movemail"              ; Added for 21.2
	(when-feature windows-nt "win32-native")
	(when-feature lisp-float-type "float-sup")
	"itimer"		; for vars auto-save-timeout and
				; auto-gc-threshold
	"itimer-autosave"
	"printer"
	"behavior"
	"behavior-defs"
	"diagnose"

	;;;;;;;;;;;;;;;;;; GUI support
	(when-feature window-system "gui")
	(when-feature window-system "mouse")
	(when-feature window-system "mode-motion")
	(when-feature toolbar "toolbar")
	(when-feature scrollbar "scrollbar")
	(when-feature menubar "menubar")
	(when-feature dialog "dialog")
	(when-feature gutter "gutter")
	(when-feature dragdrop-api "dragdrop")
	"select"

	;;;;;;;;;;;;;;;;;; Content for GUI's
	;; There used to be window-system inserted in the when-feature,
	;; but IMHO your configure script should turn off the menubar,
	;; toolbar, etc. features when there is no window system.  We
	;; should just be able to assume that, if (featurep 'menubar),
	;; the menubar should work and if items are added, they can be
	;; seen clearly and usefully.
	(when-feature (and (not infodock) menubar) "menubar-items")
	(when-feature (and gutter) "gutter-items")
	(when-feature (and (not infodock) toolbar) "toolbar-items")
	(when-feature (and (not infodock) dialog) "dialog-items")

	;;;;;;;;;;;;;;;;;; Coding-system support
	"coding"
	"code-files"
	;; Handle process with encoding/decoding coding-system.
	"code-process"
	;; Provide basic commands to set coding systems to user
	"code-cmds"
	"unicode"
	;;;;;;;;;;;;;;;;;; MULE support
	(when-feature mule "mule-charset")
	(when-feature mule "mule-cmds") ; to sync with Emacs 20.1
	(when-feature mule "mule-coding")
	(when-feature mule "mule-composite-stub")
	(when-feature mule "mule-composite")
	;; may initialize coding systems
	(when-feature (and mule x) "mule-x-init")
	(when-feature (and mule tty) "mule-tty-init")
	(when-feature (and mule mswindows) "mule-msw-init")
	"code-init" ; set up defaults
	;; All files after this can have extended characters in them.
	(when-feature mule "mule-category")
	(when-feature mule "mule-ccl")
	(when-feature mule "kinsoku")

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

	(when-feature mule "arabic")
	(when-feature mule "chinese")
	(when-feature mule "cyrillic")
	(when-feature mule "english")
	(when-feature mule "ethiopic")
	(when-feature mule "european")
	(when-feature mule "greek")
	(when-feature mule "hebrew")
	(when-feature mule "indian")
	(when-feature mule "devanagari") ; must be loaded after indian.el
	(when-feature mule "japanese")
	(when-feature mule "korean")
	(when-feature mule "lao")
	(when-feature mule "latin")
	(when-feature mule "misc-lang")
	;; #### merge thai and thai-xtis!!!
	;(when-feature mule "thai")
	(when-feature mule "thai-xtis")
	(when-feature mule "tibetan")
	(when-feature mule "vietnamese")

	;; Specialized language support
	(when-feature (and mule CANNA) "canna-leim")
;; Egg/Its is now a package
;	(when-feature (and mule wnn) "egg-leim")
;	(when-feature (and mule wnn) "egg-kwnn-leim")
;	(when-feature (and mule wnn) "egg-cwnn-leim")
;	(when-feature mule "egg-sj3-leim")
;; SKK is now a package
;	(when-feature mule "skk-leim")

;; Enable Mule capability for Gnus, mail, etc...
;; Moved to sunpro-load.el - the default only for Sun.
;;(pureload "mime-setup")

	;; needs access to the charsets created by the above
	;; language-specific files.
	(when-feature (and mule mswindows) "mule-msw-init-late")

;;; mule-load.el ends here

;; preload InfoDock stuff.  should almost certainly not be here if
;; id-menus is not here.  infodock needs to figure out a clever way to
;; advise this stuff or we need to export a clean way for infodock or
;; others to control this programmatically.
	(when-feature (and infodock (or x mswindows gtk) menubar) "id-menus")
;; preload the X code.
	(when-feature x "x-iso8859-1")
	(when-feature x "x-mouse")
	(when-feature x "x-select")
	(when-feature (and x scrollbar) "x-scrollbar")
	(when-feature x "x-misc")
	(when-feature x "x-init")
	(when-feature x "x-win-xfree86")
	(when-feature x "x-win-sun")
;; preload the GTK code
 	(when-feature gtk "gtk-ffi")
 	(when-feature gtk "gtk-widgets")
 	(when-feature gtk "gdk")
 	(when-feature gtk "gtk-init")
 	(when-feature gtk "gtk-iso8859-1")
 	(when-feature (and gtk dialog) "dialog-gtk")
 	(when-feature gtk "gtk-select")
 	(when-feature gtk "gtk-mouse")
 	(when-feature gtk "gtk-glyphs")
 	(when-feature glade "glade")
	(when-feature gtk "widgets-gtk")

;; preload the mswindows code.
	(when-feature mswindows "msw-glyphs")
	(when-feature mswindows "msw-mouse")
	(when-feature mswindows "msw-init")
	(when-feature mswindows "msw-select")
;; preload the TTY init code.
	(when-feature tty "tty-init")
;;; Formerly in tooltalk/tooltalk-load.el
	;; Moved to tooltalk package
	;; (when-feature tooltalk "tooltalk-macros")
	;; (when-feature tooltalk "tooltalk-util")
	;; (when-feature tooltalk "tooltalk-init")
	;; "vc-hooks"		; Packaged.  Available in two versions.
	;; "ediff-hook"		; Packaged.
	"fontl-hooks"
	"auto-show"
	"resize-minibuffer"
	(when-feature ldap "ldap")

;; (when-feature energize "energize/energize-load.el")
;;; formerly in sunpro/sunpro-load.el
;;	(when-feature (and mule sparcworks) "mime-setup")

	;; Moved to Sun package
	;; (when-feature sparcworks "cc-mode") ; Requires cc-mode package
	;; (when-feature sparcworks "sunpro-init")
	;; (when-feature sparcworks "ring")
	;; (when-feature sparcworks "comint") ; Requires comint package
	;; (when-feature sparcworks "annotations")

;;; formerly in eos/sun-eos-load.el
;;	(when-feature sparcworks "sun-eos-init")
;;	(when-feature sparcworks "sun-eos-common")
;;	(when-feature sparcworks "sun-eos-editor")
;;	(when-feature sparcworks "sun-eos-browser")
;;	(when-feature sparcworks "sun-eos-debugger")
;;	(when-feature sparcworks "sun-eos-debugger-extra")
;;	(when-feature sparcworks "sun-eos-menubar")
	"loadhist"		; Must be dumped before loaddefs is loaded
	"loaddefs"		; <=== autoloads get loaded here
))
