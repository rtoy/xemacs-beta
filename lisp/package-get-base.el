(setq package-get-base
'((eudc
  (standards-version 1.0
   version "1.0"
   author-version "0.2"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "Oscar Figueiredo <Oscar.Figueiredo@epfl.ch>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Emacs Unified Directory Client."
   filename "eudc-1.0-pkg.tar.gz"
   md5sum "c9a7556ad99205b4dab655781f8b2895"
   size 25863
   provides (eudc eudc-ldap eudc-ph)
   requires (xemacs-base)
   type regular
))
(footnote
  (standards-version 1.0
   version "1.02"
   author-version "0.18"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "SL Baur <steve@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Footnoting in mail message editing modes."
   filename "footnote-1.02-pkg.tar.gz"
   md5sum "460a7309abcb52bc6fce3c80cfcdf1d9"
   size 18045
   provides (footnote)
   requires (mail-lib xemacs-base)
   type regular
))
(gnats
  (standards-version 1.0
   version "1.02"
   author-version "3.101"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "comm"
   dump nil
   description "XEmacs bug reports."
   filename "gnats-1.02-pkg.tar.gz"
   md5sum "0cd0ab1a2c7dfc61b043948bc98f43d9"
   size 126338
   provides (gnats gnats-admin send-pr)
   requires (mail-lib xemacs-base)
   type regular
))
(gnus
  (standards-version 1.0
   version "1.09"
   author-version "5.6.1"
   date "1998-03-07"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "comm"
   dump nil
   description "The Gnus Newsreader and Mailreader."
   filename "gnus-1.09-pkg.tar.gz"
   md5sum "8e5dd9a1d861f59a5961ba22d511fcbe"
   size 1648527
   provides (gnus message)
   requires (gnus w3 mh-e mailcrypt rmail mail-lib xemacs-base)
   type regular
))
(mailcrypt
  (standards-version 1.0
   version "1.03"
   author-version "3.4"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Support for messaging encryption with PGP."
   filename "mailcrypt-1.03-pkg.tar.gz"
   md5sum "78c26e92be2b302d462045527c5d46ed"
   size 66823
   provides (mailcrypt)
   requires (gnus vm mail-lib xemacs-base)
   type regular
))
(mh-e
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "Front end support for MH."
   filename "mh-e-1.04-pkg.tar.gz"
   md5sum "5ecad7b26355ed5adcf2b1ecee9a9c95"
   size 128977
   provides (mh-e)
   requires (mail-lib xemacs-base)
   type regular
))
(net-utils
  (standards-version 1.0
   version "1.02"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Miscellaneous Networking Utilities."
   filename "net-utils-1.02-pkg.tar.gz"
   md5sum "48d2bd98d8b26f2ad4c451982807d960"
   size 48559
   provides (ilisp-browse-cltl2 emacsbug feedmail metamail rcompile shadowfile webjump webster-www)
   requires (w3 efs mail-lib xemacs-base)
   type single
))
(ph
  (standards-version 1.0
   version "1.02"
   author-version "2.6"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "LDAP support."
   filename "ph-1.02-pkg.tar.gz"
   md5sum "753a88169b747db3a449f60255d38cf2"
   size 27548
   provides (ph)
   requires (xemacs-base)
   type regular
))
(rmail
  (standards-version 1.0
   version "1.02"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "An obsolete Emacs mailer."
   filename "rmail-1.02-pkg.tar.gz"
   md5sum "d385c1d48a41899ec5436669c422699f"
   size 83574
   provides (rmail rmailsum)
   requires (tm apel mail-lib xemacs-base)
   type regular
))
(supercite
  (standards-version 1.0
   version "1.05"
   author-version "3.55x"
   date "1998-02-11"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "An Emacs citation tool for News & Mail messages."
   filename "supercite-1.05-pkg.tar.gz"
   md5sum "34e772eff70739d2d25f282b71264428"
   size 90161
   provides (supercite)
   requires (mail-lib xemacs-base)
   type regular
))
(tm
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Emacs MIME support."
   filename "tm-1.04-pkg.tar.gz"
   md5sum "f52c4e83a3c2b9a33ffec67668cd2dff"
   size 252091
   provides (tm tm-edit tm-view mime-setup)
   requires (gnus mh-e rmail vm mailcrypt mail-lib apel xemacs-base)
   type regular
))
(vm
  (standards-version 1.0
   version "1.06"
   author-version "6.41"
   date "1998-02-17"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "comm"
   dump t
   description "An Emacs mailer."
   filename "vm-1.06-pkg.tar.gz"
   md5sum "7a9e4f7943fa74752bd677ac1d26fc6f"
   size 506117
   provides (vm)
   requires (mail-lib xemacs-base)
   type regular
))
(w3
  (standards-version 1.0
   version "1.03"
   author-version "4.0pre14"
   date "1998-01-20"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority high
   category "comm"
   dump nil
   description "A Web browser."
   filename "w3-1.03-pkg.tar.gz"
   md5sum "1870ae4bc79d5c38827fd6fdce0fa16d"
   size 584854
   provides (w3 url)
   requires (w3 mail-lib xemacs-base)
   type regular
))
(cookie
  (standards-version 1.0
   version "1.06"
   author-version "20.5b29"
   date "1998-02-01"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Spook and Yow (Zippy quotes)."
   filename "cookie-1.06-pkg.tar.gz"
   md5sum "528a9773c0e84394d3cc04fdc2f5304e"
   size 34107
   provides (cookie1 yow)
   requires (xemacs-base)
   type regular
))
(games
  (standards-version 1.0
   version "1.02"
   author-version "1.0"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Tetris, Sokoban, and Snake."
   filename "games-1.02-pkg.tar.gz"
   md5sum "d9c68af0f2e98e80b5dade3508d4d1bf"
   size 29267
   provides (gamegrid snake tetris sokoban)
   requires (xemacs-base)
   type regular
))
(mine
  (standards-version 1.0
   version "1.04"
   author-version "1.8"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Minehunt Game."
   filename "mine-1.04-pkg.tar.gz"
   md5sum "59b117b2d0b581f67f09db288d82263d"
   size 67519
   provides (xmine)
   requires (xemacs-base)
   type regular
))
(misc-games
  (standards-version 1.0
   version "1.05"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Other amusements and diversions."
   filename "misc-games-1.05-pkg.tar.gz"
   md5sum "cafe2c566a63f7b049ead69e27326cd1"
   size 163192
   provides (decipher gomoku hanoi life)
   requires (xemacs-base)
   type single
))
(Sun
  (standards-version 1.0
   version "1.05"
   author-version "20.5b30"
   date "1998-03-06"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution sun
   priority low
   category "libs"
   dump t
   description "Support for Sparcworks."
   filename "Sun-1.05-pkg.tar.gz"
   md5sum "d6c701e221c32cdbf2874eb68cc4def0"
   size 63782
   provides (sccs eos-browser eos-common eos-debugger eos-debugger eos-editor eos-init eos-load eos-menubar eos-toolbar sunpro)
   requires (cc-mode xemacs-base)
   type regular
))
(apel
  (standards-version 1.0
   version "1.03"
   author-version "3.3"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "A Portable Emacs Library.  Used by XEmacs MIME support."
   filename "apel-1.03-pkg.tar.gz"
   md5sum "8122f0fd87cc0fd5d2c15fbd9d9ee10d"
   size 34609
   provides (atype emu-20 emu-e19 emu-x20 emu-xemacs emu file-detect filename install mule-caesar path-util richtext std11-parse std11 tinyrich)
   requires (xemacs-base)
   type regular
))
(dired
  (standards-version 1.0
   version "1.0"
   author-version "7.9"
   date "1998-02-17"
   build-date "1998-02-27"
   maintainer "Mike Sperber <sperber@informatik.uni-tuebingen.de>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Manage file systems."
   filename "dired-1.0-pkg.tar.gz"
   md5sum "e81b83bc45d46ea06e82b8118ab0fbc7"
   size 187313
   provides (diff dired)
   requires (xemacs-base)
   type regular
))
(edebug
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "libs"
   dump nil
   description "An Emacs Lisp debugger."
   filename "edebug-1.03-pkg.tar.gz"
   md5sum "767100572c7dd771f59277a808515a3e"
   size 117962
   provides (edebug cl-read cust-print eval-reg cl-specs)
   requires (xemacs-base)
   type regular
))
(efs
  (standards-version 1.0
   version "1.07"
   author-version "1.16"
   date "1998-02-17"
   build-date "1998-02-27"
   maintainer "Mike Sperber <sperber@informatik.uni-tuebingen.de>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Treat files on remote systems the same as local files."
   filename "efs-1.07-pkg.tar.gz"
   md5sum "07a76f9a45d62d1087fc8a79049bb665"
   size 370308
   provides (efs)
   requires (xemacs-base vm dired)
   type regular
))
(mail-lib
  (standards-version 1.0
   version "1.07"
   author-version "20.5b29"
   date "1998-02-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Fundamental lisp files for providing email support."
   filename "mail-lib-1.07-pkg.tar.gz"
   md5sum "58f719dd3b9e4e6f20f9c38ca2776b79"
   size 118919
   provides (browse-url highlight-headers mail-abbrevs mail-extr mail-utils reporter rfc822 rmail-mini rmailout sendmail smtpmail)
   requires (xemacs-base)
   type single
))
(tooltalk
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "libs"
   dump t
   description "Support for building with Tooltalk."
   filename "tooltalk-1.04-pkg.tar.gz"
   md5sum "56c6ff17e4144bf7e8086ee62a9551c5"
   size 9235
   provides ()
   requires ()
   type regular
))
(xemacs-base
  (standards-version 1.0
   version "1.15"
   author-version "20.5b30"
   date "1998-03-04"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "Fundamental XEmacs support, you almost certainly need this."
   filename "xemacs-base-1.15-pkg.tar.gz"
   md5sum "0b8aaecdcd8ce5070ad4c66310e0f42d"
   size 384120
   provides (add-log advice annotations assoc case-table chistory comint-xemacs comint compile debug ebuff-menu echistory edmacro ehelp electric enriched env facemenu helper imenu iso-syntax macros novice outline overlay passwd pp regi ring shell skeleton sort thing time-stamp timezone xbm-button xpm-button)
   requires ()
   type regular
))
(xemacs-devel
  (standards-version 1.0
   version "1.09"
   author-version "20.5b30"
   date "1998-03-03"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Emacs Lisp developer support."
   filename "xemacs-devel-1.09-pkg.tar.gz"
   md5sum "d81dbc1af8301c2339720936a1e77768"
   size 75206
   provides (docref eldoc elp find-func ielm regexp-opt trace)
   requires (xemacs-base)
   type single
))
(egg-its
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump t
   description "Wnn (4.2 and 6) support.  SJ3 support."
   filename "egg-its-1.04-pkg.tar.gz"
   md5sum "5d531d513111576d4e0367abf7c2aa3c"
   size 260075
   provides ()
   requires (leim mule-base xemacs-base)
   type regular
))
(leim
  (standards-version 1.0
   version "1.05"
   author-version "20.5b29"
   date "1998-02-06"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump nil
   description "Quail.  All non-English and non-Japanese language support."
   filename "leim-1.05-pkg.tar.gz"
   md5sum "3c846cdbe34e946f0ca0fd371f149b84"
   size 1743902
   provides ()
   requires (mule-base xemacs-base)
   type regular
))
(locale
  (standards-version 1.0
   version "1.04"
   author-version "20.5b30"
   date "1998-03-01"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump nil
   description "Localized menubars and localized splash screens."
   filename "locale-1.04-pkg.tar.gz"
   md5sum "897687db6df1450d83576511da728db1"
   size 35573
   provides ()
   requires (mule-base)
   type regular
))
(mule-base
  (standards-version 1.0
   version "1.10"
   author-version "20.5b30"
   date "1998-03-07"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump t
   description "Basic Mule support, required for building with Mule."
   filename "mule-base-1.10-pkg.tar.gz"
   md5sum "9eeb07585a000efb9172f2c081eaaad3"
   size 487358
   provides (canna-leim canna char-table china-util cyril-util isearch-ext japan-util ccl can-n-egg mule-help)
   requires (xemacs-base)
   type regular
))
(skk
  (standards-version 1.0
   version "1.03"
   author-version "10.38"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump t
   description "Japanese Language Input Method."
   filename "skk-1.03-pkg.tar.gz"
   md5sum "f5a459223e78f146959a4c44795b7830"
   size 1466964
   provides (skk skk-tut)
   requires (viper mule-base xemacs-base)
   type regular
))
(calc
  (standards-version 1.0
   version "1.02"
   author-version "2.02f"
   date "1998-03-03"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Emacs calculator"
   filename "calc-1.02-pkg.tar.gz"
   md5sum "9e5f89ecd0f03e1abab4d78ca4dbe79e"
   size 1158346
   provides (calc)
   requires ()
   type regular
))
(calendar
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Calendar and diary support."
   filename "calendar-1.03-pkg.tar.gz"
   md5sum "c1688fe7c3228dd369a096d7c47cb51c"
   size 168688
   provides (appt cal-dst cal-french cal-mayan cal-x cal-xemacs calendar diary-ins diary-lib holidays lunar solar)
   requires (xemacs-base)
   type regular
))
(edit-utils
  (standards-version 1.0
   version "1.13"
   author-version "20.5b30"
   date "1998-03-02"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous editor extensions, you probably need this."
   filename "edit-utils-1.13-pkg.tar.gz"
   md5sum "89d088c6ac182fe5941d7ed3d239544f"
   size 557868
   provides (abbrevlist atomic-extents avoid backup-dir balloon-help big-menubar blink-cursor blink-paren bookmark compare-w completion dabbrev desktop detached-minibuf edit-toolbar fast-lock file-part floating-toolbar flow-ctrl foldout func-menu hippie-exp icomplete id-select info-look iswitchb lazy-lock lazy-shot live-icon man mic-paren paren popper mode-motion+ outl-mouse page-ext blink-paren paren permanent-buffers recent-files redo reportmail rsz-minibuf saveconfsavehist saveplace scroll-in-place tempo toolbar-utils tree-menu uniquify where-was-i-db)
   requires (xemacs-base)
   type single
))
(forms
  (standards-version 1.0
   version "1.05"
   author-version "2.10"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Forms editing support (obsolete, use Widget instead)."
   filename "forms-1.05-pkg.tar.gz"
   md5sum "fbf0d97c78f304cf4cb8212490c58a25"
   size 39853
   provides (forms forms-mode)
   requires ()
   type regular
))
(frame-icon
  (standards-version 1.0
   version "1.02"
   author-version "20.5b29"
   date "1998-02-26"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Set up mode-specific icons for each frame under XEmacs"
   filename "frame-icon-1.02-pkg.tar.gz"
   md5sum "5878c2f4fed7546265bf93b381f9aa83"
   size 33563
   provides (forms forms-mode)
   requires ()
   type regular
))
(hm--html-menus
  (standards-version 1.0
   version "1.05"
   author-version "5.9"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "HTML editing."
   filename "hm--html-menus-1.05-pkg.tar.gz"
   md5sum "544597bdddd4b2e138bf6f26a8ecb2b8"
   size 134645
   provides (adapt hm--date hm--html-configuration hm--html-drag-and-drop hm--html-indentation hm--html-keys hm--html-menu hm--html-mode hm--html-not-standard hm--html html-view tmpl-minor-mode)
   requires (xemacs-base)
   type regular
))
(ispell
  (standards-version 1.0
   version "1.06"
   author-version "3.0beta"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Spell-checking with GNU ispell."
   filename "ispell-1.06-pkg.tar.gz"
   md5sum "7b04d028581c0c7c3b9c7812dbaf70ca"
   size 65123
   provides (ispell)
   requires ()
   type regular
))
(pc
  (standards-version 1.0
   version "1.07"
   author-version "20.5b29"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "PC style interface emulation."
   filename "pc-1.07-pkg.tar.gz"
   md5sum "c5e92917cbe06e156f8ba08c36cae217"
   size 12155
   provides (delbs pc-select pending-del s-region)
   requires (xemacs-base)
   type regular
))
(psgml
  (standards-version 1.0
   version "1.05"
   author-version "1.01"
   date "1998-02-06"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Validated HTML/SGML editing."
   filename "psgml-1.05-pkg.tar.gz"
   md5sum "d9d9bcb7434b0bf39724e4f3d4119e22"
   size 402844
   provides (psgml sgml)
   requires (edit-utils)
   type regular
))
(sgml
  (standards-version 1.0
   version "1.01"
   author-version "20.5b29"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "SGML/Linuxdoc-SGML editing."
   filename "sgml-1.01-pkg.tar.gz"
   md5sum "a9377edae67887eec79458909e143f4a"
   size 26872
   provides (sgml linuxdoc-sgml)
   requires (xemacs-base)
   type regular
))
(slider
  (standards-version 1.0
   version "1.05"
   author-version "0.3"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority low
   category "oa"
   dump nil
   description "User interface tool."
   filename "slider-1.05-pkg.tar.gz"
   md5sum "10632105a03c6c186168b7e9746c6074"
   size 12123
   provides (slider color-selector)
   requires ()
   type regular
))
(speedbar
  (standards-version 1.0
   version "1.05"
   author-version "0.6.2"
   date "1998-02-07"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "??? Document me."
   filename "speedbar-1.05-pkg.tar.gz"
   md5sum "122c2b1676a2d9cd0cb5dbdb2c787514"
   size 95006
   provides (speedbar)
   requires (xemacs-base)
   type regular
))
(strokes
  (standards-version 1.0
   version "1.01"
   author-version "20.5b29"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Mouse enhancement utility."
   filename "strokes-1.01-pkg.tar.gz"
   md5sum "38979b99949b221fe85c0845a922aa9e"
   size 43723
   provides (strokes)
   requires (text-modes edit-utils mail-lib xemacs-base)
   type regular
))
(text-modes
  (standards-version 1.0
   version "1.05"
   author-version "20.5b29"
   date "1998-02-26"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous support for editing text files."
   filename "text-modes-1.05-pkg.tar.gz"
   md5sum "bf46fbc87327b7512932aa4374bcf1f9"
   size 106135
   provides (autoinsert crontab-edit filladapt image-mode iso-acc iso-ascii iso-cvt iso-insert iso-swed swedish tabify whitespace-mode winmgr-mode xpm-mode xrdb-mode)
   requires (xemacs-base)
   type regular
))
(time
  (standards-version 1.0
   version "1.03"
   author-version "1.17"
   date "1998-01-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Display time & date on the modeline."
   filename "time-1.03-pkg.tar.gz"
   md5sum "47dcbd05b3b7b6982f2f1a42475de338"
   size 20091
   provides (time)
   requires (xemacs-base)
   type regular
))
(eterm
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Terminal emulation."
   filename "eterm-1.03-pkg.tar.gz"
   md5sum "4314f3f85e2ac035c356d99df910231c"
   size 102100
   provides (eterm)
   requires (xemacs-base)
   type regular
))
(igrep
  (standards-version 1.0
   version "1.01"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Enhanced front-end for Grep."
   filename "igrep-1.01-pkg.tar.gz"
   md5sum "d1e1224ed2547cb28d17064e46dcad48"
   size 13961
   provides (igrep)
   requires (dired xemacs-base)
   type regular
))
(ilisp
  (standards-version 1.0
   version "1.03"
   author-version "5.8"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Front-end for Inferior Lisp."
   filename "ilisp-1.03-pkg.tar.gz"
   md5sum "62c1e14eaca07d6ed5c00cba517c3b26"
   size 223412
   provides (ilisp completer)
   requires (xemacs-base)
   type regular
))
(os-utils
  (standards-version 1.0
   version "1.06"
   author-version "20.5b29"
   date "1998-02-21"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Miscellaneous O/S utilities."
   filename "os-utils-1.06-pkg.tar.gz"
   md5sum "c2b8aec12884057993b11b19868cd1d8"
   size 224310
   provides (archive-mode background crypt crypt++ inf-lisp jka-compr lpr mchat ps-print tar-mode telnet terminal uncompress)
   requires (xemacs-base)
   type single
))
(view-process
  (standards-version 1.0
   version "1.03"
   author-version "2.4"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "A Unix process browsing tool."
   filename "view-process-1.03-pkg.tar.gz"
   md5sum "9e9567a768184a0aeea90bbdadf15f08"
   size 59864
   provides (view-process-mode)
   requires (xemacs-base)
   type regular
))
(ada
  (standards-version 1.0
   version "1.03"
   author-version "2.27"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Ada language support."
   filename "ada-1.03-pkg.tar.gz"
   md5sum "4626ea66734003692b32b86fa7fa4fe8"
   size 54305
   provides (ada-mode ada-stmt)
   requires ()
   type regular
))
(c-support
  (standards-version 1.0
   version "1.05"
   author-version "20.5b29"
   date "1998-02-26"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Basic single-file add-ons for editing C code."
   filename "c-support-1.05-pkg.tar.gz"
   md5sum "9f82872d426d4364543bc9e36b59074d"
   size 41785
   provides (c-comment-edit cmacexp hideif hideshow)
   requires (cc-mode xemacs-base)
   type regular
))
(debug
  (standards-version 1.0
   version "1.01"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "GUD, gdb, dbx debugging support."
   filename "debug-1.01-pkg.tar.gz"
   md5sum "dab66c6b6fc130a9bf4a6e6794014971"
   size 87152
   provides ()
   requires (xemacs-base)
   type regular
))
(ediff
  (standards-version 1.0
   version "1.05"
   author-version "2.70"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Interface over GNU patch."
   filename "ediff-1.05-pkg.tar.gz"
   md5sum "51b558d3af30e129d82ca5a62a2eefaa"
   size 244330
   provides (ediff)
   requires (pcl-cvs dired xemacs-base)
   type regular
))
(emerge
  (standards-version 1.0
   version "1.01"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Another interface over GNU patch."
   filename "emerge-1.01-pkg.tar.gz"
   md5sum "59f3d69e46bf05f1f1f8cd26e6caac43"
   size 60905
   provides (emerge)
   requires ()
   type regular
))
(pcl-cvs
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "CVS frontend."
   filename "pcl-cvs-1.03-pkg.tar.gz"
   md5sum "e077c161d2013314bb98d94fc50efa7f"
   size 134541
   provides (pcl-cvs dll elib-node generic-sc)
   requires (xemacs-base)
   type regular
))
(prog-modes
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-02-08"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Support for various programming languages."
   filename "prog-modes-1.04-pkg.tar.gz"
   md5sum "82d3259204008efa69d7eab20db7a80b"
   size 535354
   provides (autoconf-mode cperl-mode eiffel3 f90 fortran ksh-mode m4-mode makefile perl-mode postscript python-mode rexx-mode simula-mode tcl teco verilog-mod)
   requires (mail-lib xemacs-base)
   type regular
))
(scheme
  (standards-version 1.0
   version "1.02"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Front-end support for Inferior Scheme."
   filename "scheme-1.02-pkg.tar.gz"
   md5sum "b23c568e5939392f054119f9c6eee981"
   size 34801
   provides (scheme xscheme cmuscheme)
   requires (xemacs-base)
   type regular
))
(sh-script
  (standards-version 1.0
   version "1.04"
   author-version "2.0e"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Support for editing shell scripts."
   filename "sh-script-1.04-pkg.tar.gz"
   md5sum "d3840b34d0b92b57257b4170d8fa33a5"
   size 33617
   provides (sh-script executable)
   requires (xemacs-base)
   type regular
))
(vc-cc
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump t
   description "Version Control for ClearCase (UnFree) systems."
   filename "vc-cc-1.03-pkg.tar.gz"
   md5sum "b9d346ff0e29fdb75bf0309c5b25d869"
   size 96057
   provides (vc)
   requires (dired xemacs-base)
   type regular
))
(vc
  (standards-version 1.0
   version "1.05"
   author-version "20.5b30"
   date "1998-03-08"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump t
   description "Version Control for Free systems."
   filename "vc-1.05-pkg.tar.gz"
   md5sum "9448703c390f3b4e1c0a4b2a2daf7da9"
   size 74464
   provides (vc)
   requires (dired xemacs-base)
   type regular
))
(vhdl
  (standards-version 1.0
   version "1.03"
   author-version "2.74"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Support for VHDL."
   filename "vhdl-1.03-pkg.tar.gz"
   md5sum "0edc086261a9e0526704e99400d100e2"
   size 54064
   provides (vhdl-mode)
   requires ()
   type regular
))
(auctex
  (standards-version 1.0
   version "1.06"
   author-version "9.7p"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "wp"
   dump nil
   description "Basic TeX/LaTeX support."
   filename "auctex-1.06-pkg.tar.gz"
   md5sum "e6ed0053066ca0c49251d1f85dc31fc1"
   size 304695
   provides (auc-old bib-cite font-latex latex multi-prompt tex-buf tex-info tex-jp tex-site tex)
   requires (xemacs-base)
   type regular
))
(crisp
  (standards-version 1.0
   version "1.03"
   author-version "1.31"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "Crisp/Brief emulation."
   filename "crisp-1.03-pkg.tar.gz"
   md5sum "7d1e952213bd159dd11957daff005083"
   size 9054
   provides (crisp scroll-lock)
   requires ()
   type regular
))
(edt
  (standards-version 1.0
   version "1.03"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "wp"
   dump nil
   description "DEC EDIT/EDT emulation."
   filename "edt-1.03-pkg.tar.gz"
   md5sum "93715876897a4f0bfcf66eed73010c2c"
   size 46019
   provides (edt)
   requires (xemacs-base)
   type regular
))
(reftex
  (standards-version 1.0
   version "1.02"
   author-version "3.18.0.4"
   date "1998-02-25"
   build-date "1998-03-08"
   maintainer "Carsten Dominik <dominik@strw.LeidenUniv.nl>"
   distribution stable
   priority medium
   category "wp"
   dump nil
   description "Emacs support for LaTeX cross-references, citations.."
   filename "reftex-1.02-pkg.tar.gz"
   md5sum "15da65e996854bdc212bbfaff9db8417"
   size 140412
   provides (reftex)
   requires (xemacs-base)
   type regular
))
(texinfo
  (standards-version 1.0
   version "1.06"
   author-version "20.5b30"
   date "1998-03-03"
   build-date "1998-03-08"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "wp"
   dump nil
   description "XEmacs TeXinfo support."
   filename "texinfo-1.06-pkg.tar.gz"
   md5sum "60579a4364d996cb65b85e62f9c2bf47"
   size 125816
   provides (makeinfo tex-mode texinfmt texinfo texnfo-tex texnfo-upd)
   requires (xemacs-base)
   type regular
))
(textools
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-02-17"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stabl
   priority medium
   category "wp"
   dump nil
   description "Miscellaneous TeX support."
   filename "textools-1.04-pkg.tar.gz"
   md5sum "b18c912373db3b8ae4ad9c472c33128b"
   size 78953
   provides (bib-mode bibtex refer-to-bibtex)
   requires (xemacs-base)
   type single
))
(tpu
  (standards-version 1.0
   version "1.04"
   author-version "20.5b29"
   date "1998-01-24"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "wp"
   dump nil
   description "DEC EDIT/TPU support."
   filename "tpu-1.04-pkg.tar.gz"
   md5sum "08968507b0879b85c2d56e49bc69b379"
   size 57407
   provides (tpu)
   requires ()
   type regular
))
(viper
  (standards-version 1.0
   version "1.05"
   author-version "3.005"
   date "1998-02-25"
   build-date "1998-02-27"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "VI emulation support."
   filename "viper-1.05-pkg.tar.gz"
   md5sum "9fdea665cb1021fd8ce70ef17ba1a19d"
   size 261076
   provides (viper)
   requires (xemacs-base)
   type regular
))
))
