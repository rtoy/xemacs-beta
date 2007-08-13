(require 'package-get)
(defgroup comm-packages nil
  "comm package group"
  :group 'packages)

(defcustom eudc-package nil 
  "Emacs Unified Directory Client (LDAP, PH)."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom footnote-package nil 
  "Footnoting in mail message editing modes."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom gnats-package nil 
  "XEmacs bug reports."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom gnus-package nil 
  "The Gnus Newsreader and Mailreader."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mailcrypt-package nil 
  "Support for messaging encryption with PGP."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mew-package nil 
  "Messaging in an Emacs World."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mh-e-package nil 
  "Front end support for MH."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom net-utils-package nil 
  "Miscellaneous Networking Utilities."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom rmail-package nil 
  "An obsolete Emacs mailer."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom supercite-package nil 
  "An Emacs citation tool for News & Mail messages."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom tm-package nil 
  "Emacs MIME support."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom vm-package nil 
  "An Emacs mailer."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom w3-package nil 
  "A Web browser."
  :group 'comm-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup games-packages nil
  "games package group"
  :group 'packages)

(defcustom cookie-package nil 
  "Spook and Yow (Zippy quotes)."
  :group 'games-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom games-package nil 
  "Tetris, Sokoban, and Snake."
  :group 'games-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mine-package nil 
  "Minehunt Game."
  :group 'games-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom misc-games-package nil 
  "Other amusements and diversions."
  :group 'games-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup libs-packages nil
  "libs package group"
  :group 'packages)

(defcustom Sun-package nil 
  "Support for Sparcworks."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom apel-package nil 
  "A Portable Emacs Library.  Used by XEmacs MIME support."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom dired-package nil 
  "Manage file systems."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom edebug-package nil 
  "An Emacs Lisp debugger."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom efs-package nil 
  "Treat files on remote systems the same as local files."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom fsf-compat-package nil 
  "FSF Emacs compatibility files."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mail-lib-package nil 
  "Fundamental lisp files for providing email support."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom sounds-au-package nil 
  "XEmacs Sun sound files."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom sounds-wav-package nil 
  "XEmacs Microsoft sound files."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom tooltalk-package nil 
  "Support for building with Tooltalk."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom xemacs-base-package nil 
  "Fundamental XEmacs support, you almost certainly need this."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom xemacs-devel-package nil 
  "Emacs Lisp developer support."
  :group 'libs-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup mule-packages nil
  "mule package group"
  :group 'packages)

(defcustom edict-package nil 
  "Lisp Interface to EDICT, Kanji Dictionary"
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom egg-its-package nil 
  "Wnn (4.2 and 6) support.  SJ3 support."
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom leim-package nil 
  "Quail.  All non-English and non-Japanese language support."
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom locale-package nil 
  "Localized menubars and localized splash screens."
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom mule-base-package nil 
  "Basic Mule support, required for building with Mule."
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom skk-package nil 
  "Japanese Language Input Method."
  :group 'mule-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup oa-packages nil
  "oa package group"
  :group 'packages)

(defcustom calc-package nil 
  "Emacs calculator"
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom calendar-package nil 
  "Calendar and diary support."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom edit-utils-package nil 
  "Miscellaneous editor extensions, you probably need this."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom forms-package nil 
  "Forms editing support (obsolete, use Widget instead)."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom frame-icon-package nil 
  "Set up mode-specific icons for each frame under XEmacs"
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom hm--html-menus-package nil 
  "HTML editing."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom ispell-package nil 
  "Spell-checking with GNU ispell."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom pc-package nil 
  "PC style interface emulation."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom psgml-package nil 
  "Validated HTML/SGML editing."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom sgml-package nil 
  "SGML/Linuxdoc-SGML editing."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom slider-package nil 
  "User interface tool."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom speedbar-package nil 
  "??? Document me."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom strokes-package nil 
  "Mouse enhancement utility."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom text-modes-package nil 
  "Miscellaneous support for editing text files."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom time-package nil 
  "Display time & date on the modeline."
  :group 'oa-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup os-packages nil
  "os package group"
  :group 'packages)

(defcustom eterm-package nil 
  "Terminal emulation."
  :group 'os-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom igrep-package nil 
  "Enhanced front-end for Grep."
  :group 'os-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom ilisp-package nil 
  "Front-end for Inferior Lisp."
  :group 'os-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom os-utils-package nil 
  "Miscellaneous O/S utilities."
  :group 'os-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom view-process-package nil 
  "A Unix process browsing tool."
  :group 'os-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup prog-packages nil
  "prog package group"
  :group 'packages)

(defcustom ada-package nil 
  "Ada language support."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom c-support-package nil 
  "Basic single-file add-ons for editing C code."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom cc-mode-package nil 
  "C, C++ and Java language support."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom debug-package nil 
  "GUD, gdb, dbx debugging support."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom ediff-package nil 
  "Interface over GNU patch."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom emerge-package nil 
  "Another interface over GNU patch."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom jde-package nil 
  "Java language and development support."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom pcl-cvs-package nil 
  "CVS frontend."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom prog-modes-package nil 
  "Support for various programming languages."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom scheme-package nil 
  "Front-end support for Inferior Scheme."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom sh-script-package nil 
  "Support for editing shell scripts."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom vc-cc-package nil 
  "Version Control for ClearCase (UnFree) systems."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom vc-package nil 
  "Version Control for Free systems."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom vhdl-package nil 
  "Support for VHDL."
  :group 'prog-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defgroup wp-packages nil
  "wp package group"
  :group 'packages)

(defcustom auctex-package nil 
  "Basic TeX/LaTeX support."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom crisp-package nil 
  "Crisp/Brief emulation."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom edt-package nil 
  "DEC EDIT/EDT emulation."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom reftex-package nil 
  "Emacs support for LaTeX cross-references, citations.."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom texinfo-package nil 
  "XEmacs TeXinfo support."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom textools-package nil 
  "Miscellaneous TeX support."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom tpu-package nil 
  "DEC EDIT/TPU support."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

(defcustom viper-package nil 
  "VI emulation support."
  :group 'wp-packages
  :initialize 'package-get-ever-installed-p
  :type 'boolean)

