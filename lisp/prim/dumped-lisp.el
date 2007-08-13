(setq preloaded-file-list
      '("backquote" 		; needed for defsubst etc.
	"bytecomp-runtime"	; define defsubst
	"packages"		; Bootstrap run-time lisp environment
	"subr" 			; load the most basic Lisp functions
	"replace" 		; match-string used in version.el.
	"version.el"		; Ignore compiled-by-mistake version.elc
	"cl"
	"cl-extra"
	"cl-seq"
	"cl/auto-autoloads"	; Prevents problems later
	;;"featurep"
	"widget"
	"custom"		; Before the world so everything can be
				; customized
	"cus-start"		; for customization of builtin variables
	"cmdloop"
;; (or (fboundp 'recursive-edit) (load-gc "cmdloop1"))
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
	;; "cc-mode"		; as FSF goes so go we ..
	;; "scroll-in-place"	; We're not ready for this :-(
	;; we no longer load buff-menu automatically.
	;; it will get autoloaded if needed.

	;; Before this will work again, the different system types will need
	;; to have features named for them.

;     (cond  ; Differences based on system-type
;      ((eq system-type 'vax-vms)
;       (load-gc "vmsproc")
;       (load-gc "vms-patch"))
;      ((eq system-type 'windows-nt)
;       ;; (load-gc "ls-lisp")
	#+windows-nt "winnt"
;      ((eq system-type 'ms-dos)
;       ;; (load-gc "ls-lisp")
;       (load-gc "dos-fns")
;       (load-gc "disp-table")))	; needed to setup ibm-pc char set,
				; see internal.el
	#+lisp-float-type "float-sup"
	"itimer"		; for vars auto-save-timeout and
				; auto-gc-threshold
	"itimer-autosave"
	#+toolbar "toolbar"
;       ;; else still define a few functions.
;       (defun toolbar-button-p    (obj) "No toolbar support." nil)
;       (defun toolbar-specifier-p (obj) "No toolbar support." nil))
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

	#+mule "language/arabic"
	#+mule "language/chinese"
	#+mule "language/cyrillic"
	#+mule "language/english"
;;	#+mule "language/ethiopic"
	#+mule "language/european"
	#+mule "language/greek"
	#+mule "language/hebrew"
	#+mule "language/japanese"
	#+mule "language/korean"
	#+mule "language/misc-lang"
;;	#+mule "language/thai"
	#+mule "language/viet-chars"
	#+mule "language/vietnamese"

	;; Specialized language support
	#+(and mule CANNA) "canna-leim"
	#+(and mule wnn) "egg-leim"

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
	#+(and x menubar) "x-menubar"
	#+x "x-faces"
	#+x "x-iso8859-1"
	#+x "x-mouse"
	#+x "x-select"
	#+(and x scrollbar) "x-scrollbar"
	#+x "x-misc"
	#+x "x-init"
	#+(and x toolbar) "x-toolbar"
;; preload the TTY init code.
	#+tty "tty-init"
;;; Formerly in tooltalk/tooltalk-load.el
	#+tooltalk "tooltalk/tooltalk-macros"
	#+tooltalk "tooltalk/tooltalk-util"
	#+tooltalk "tooltalk/tooltalk-init"
	;; "vc-hooks"		; Packaged.  Available in two versions.
	;; "ediff-hook"		; Packaged.
	"fontl-hooks"
	"auto-show"
;; #+energize "energize/energize-load.el"
;;; formerly in sunpro/sunpro-load.el
;;	#+(and mule sparcworks) "mime-setup"
	#+sparcworks "cc-mode"
	#+sparcworks "sunpro-init"
	#+sparcworks "ring"
	#+sparcworks "comint"
	#+sparcworks "annotations"
;;; formerly in eos/sun-eos-load.el
;;	#+sparcworks "sun-eos-init"
;;	#+sparcworks "sun-eos-common"
;;	#+sparcworks "sun-eos-editor"
;;	#+sparcworks "sun-eos-browser"
;;	#+sparcworks "sun-eos-debugger"
;;	#+sparcworks "sun-eos-debugger-extra"
;;	#+sparcworks "sun-eos-menubar"
	"loaddefs"		; <=== autoloads get loaded here
))
