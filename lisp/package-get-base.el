(setq package-get-base
'((footnote
  (standards-version 1.0
   version "1.02"
   author-version "0.18"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "SL Baur <steve@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Footnoting in mail message editing modes."
   filename "footnote-1.02-pkg.tar.gz"
   md5sum "9c4e614eab727d58d1cb850b4334a533"
   size 18018
   provides (footnote)
   requires (mail-lib xemacs-base)
   type regular
))
(gnats
  (standards-version 1.0
   version "1.02"
   author-version "3.101"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "comm"
   dump nil
   description "XEmacs bug reports."
   filename "gnats-1.02-pkg.tar.gz"
   md5sum "89cd5144aa36dede739d845b65b3fd2a"
   size 126343
   provides (gnats gnats-admin send-pr)
   requires (mail-lib xemacs-base)
   type regular
))
(gnus
  (standards-version 1.0
   version "1.03"
   author-version "0.22q"
   date "1998-01-24"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "comm"
   dump nil
   description "The Gnus Newsreader and Mailreader."
   filename "gnus-1.03-pkg.tar.gz"
   md5sum "89614e38ec293f82996246140dae1a05"
   size 1664505
   provides (gnus message)
   requires (gnus w3 mh-e mailcrypt rmail mail-lib xemacs-base)
   type regular
))
(mailcrypt
  (standards-version 1.0
   version "1.03"
   author-version "3.4"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Support for messaging encryption with PGP."
   filename "mailcrypt-1.03-pkg.tar.gz"
   md5sum "39f5b5f57f557dc760ef49be5275e97f"
   size 66815
   provides (mailcrypt)
   requires (gnus vm mail-lib xemacs-base)
   type regular
))
(mh-e
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "Front end support for MH."
   filename "mh-e-1.04-pkg.tar.gz"
   md5sum "f412f2a82ac7005c788880119d1f75d5"
   size 128979
   provides (mh-e)
   requires (mail-lib xemacs-base)
   type regular
))
(net-utils
  (standards-version 1.0
   version "1.02"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Miscellaneous Networking Utilities."
   filename "net-utils-1.02-pkg.tar.gz"
   md5sum "c2391219560bd5edf83e25eacf773236"
   size 48538
   provides (ilisp-browse-cltl2 emacsbug feedmail metamail rcompile shadowfile webjump webster-www)
   requires (w3 efs mail-lib xemacs-base)
   type single
))
(ph
  (standards-version 1.0
   version "1.02"
   author-version "2.6"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "LDAP support."
   filename "ph-1.02-pkg.tar.gz"
   md5sum "db0d1c059d645624c2cf38a006a9d837"
   size 27560
   provides (ph)
   requires (xemacs-base)
   type regular
))
(rmail
  (standards-version 1.0
   version "1.02"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "An obsolete Emacs mailer."
   filename "rmail-1.02-pkg.tar.gz"
   md5sum "1d72ef61388abef1dc44633f756b09b4"
   size 83554
   provides (rmail rmailsum)
   requires (tm apel mail-lib xemacs-base)
   type regular
))
(supercite
  (standards-version 1.0
   version "1.03"
   author-version "3.55x"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "An Emacs citation tool for News & Mail messages."
   filename "supercite-1.03-pkg.tar.gz"
   md5sum "21ee6cbf484b3fac5acf61a2425a83dd"
   size 69394
   provides (supercite)
   requires (mail-lib xemacs-base)
   type regular
))
(tm
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Emacs MIME support."
   filename "tm-1.04-pkg.tar.gz"
   md5sum "1c56f8dcbd2cf367be4ed6ff1e31351e"
   size 251912
   provides (tm tm-edit tm-view mime-setup)
   requires (gnus mh-e rmail vm mailcrypt mail-lib apel xemacs-base)
   type regular
))
(w3
  (standards-version 1.0
   version "1.02"
   author-version "4.0pre13"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority high
   category "comm"
   dump nil
   description "A Web browser."
   filename "w3-1.02-pkg.tar.gz"
   md5sum "0c7cc3cb5606f72038b1c2c004335c1b"
   size 583513
   provides (w3 url)
   requires (w3 mail-lib xemacs-base)
   type regular
))
(cookie
  (standards-version 1.0
   version "1.05"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Spook and Yow (Zippy quotes)."
   filename "cookie-1.05-pkg.tar.gz"
   md5sum "00bca0b3ada1ec3f893b14fc212d2578"
   size 33860
   provides (cookie1 yow)
   requires (xemacs-base)
   type regular
))
(games
  (standards-version 1.0
   version "1.02"
   author-version "1.0"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Tetris, Sokoban, and Snake."
   filename "games-1.02-pkg.tar.gz"
   md5sum "c494d0e7ab65a27d01d6ecb9da678f38"
   size 29242
   provides (gamegrid snake tetris sokoban)
   requires (xemacs-base)
   type regular
))
(mine
  (standards-version 1.0
   version "1.04"
   author-version "1.8"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Minehunt Game."
   filename "mine-1.04-pkg.tar.gz"
   md5sum "2954130562ca8ce131000f98f79f0773"
   size 67463
   provides (xmine)
   requires (xemacs-base)
   type regular
))
(misc-games
  (standards-version 1.0
   version "1.05"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Other amusements and diversions."
   filename "misc-games-1.05-pkg.tar.gz"
   md5sum "0d745a9b1f6c0dba0e37d3647b45fd3c"
   size 163195
   provides (decipher gomoku hanoi life)
   requires (xemacs-base)
   type single
))
(apel
  (standards-version 1.0
   version "1.03"
   author-version "3.3"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "A Portable Emacs Library.  Used by XEmacs MIME support."
   filename "apel-1.03-pkg.tar.gz"
   md5sum "2a38de58306f6f8a7a285b00c2849f8d"
   size 34561
   provides (atype emu-20 emu-e19 emu-x20 emu-xemacs emu file-detect filename install mule-caesar path-util richtext std11-parse std11 tinyrich)
   requires (xemacs-base)
   type regular
))
(edebug
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "libs"
   dump nil
   description "An Emacs Lisp debugger."
   filename "edebug-1.03-pkg.tar.gz"
   md5sum "b9f12435b9782b95eb9b7def4d301e80"
   size 117966
   provides (edebug cl-read cust-print eval-reg cl-specs)
   requires (xemacs-base)
   type regular
))
(efs
  (standards-version 1.0
   version "1.05"
   author-version "1.15"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Treat files on remote systems the same as local files."
   filename "efs-1.05-pkg.tar.gz"
   md5sum "d74dd614e4b84522bacb58820123622d"
   size 544501
   provides (efs diff dired efs-auto)
   requires (vm xemacs-base)
   type regular
))
(tooltalk
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "libs"
   dump t
   description "Support for building with Tooltalk."
   filename "tooltalk-1.04-pkg.tar.gz"
   md5sum "b9c04e07001c9994e771ed8f992692b0"
   size 9226
   provides ()
   requires ()
   type regular
))
(xemacs-base
  (standards-version 1.0
   version "1.09"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "Fundamental XEmacs support, you almost certainly need this."
   filename "xemacs-base-1.09-pkg.tar.gz"
   md5sum "e2b8169fa6b444f26910131f172e443d"
   size 381040
   provides (add-log advice annotations assoc case-table chistory comint-xemacs comint compile debug ebuff-menu echistory edmacro ehelp electric enriched env facemenu helper imenu iso-syntax macros novice outline overlay passwd pp regi ring shell skeleton sort thing time-stamp timezone xbm-button xpm-button)
   requires ()
   type regular
))
(xemacs-devel
  (standards-version 1.0
   version "1.06"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Emacs Lisp developer support."
   filename "xemacs-devel-1.06-pkg.tar.gz"
   md5sum "d0be22936b57c896e6bbe4f6874ba949"
   size 72163
   provides (docref eldoc elp find-func ielm regexp-opt trace)
   requires (xemacs-base)
   type single
))
(egg-its
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump t
   description "Wnn (4.2 and 6) support.  SJ3 support."
   filename "egg-its-1.04-pkg.tar.gz"
   md5sum "4ffa0409545751d4d277da3f06fe90a2"
   size 260213
   provides ()
   requires (leim mule-base xemacs-base)
   type regular
))
(leim
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump nil
   description "Quail.  All non-English and non-Japanese language support."
   filename "leim-1.03-pkg.tar.gz"
   md5sum "17402ccca45ddee1bc7b873c4e692bce"
   size 1744352
   provides ()
   requires (mule-base xemacs-base)
   type regular
))
(locale
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump nil
   description "Localized menubars and localized splash screens."
   filename "locale-1.03-pkg.tar.gz"
   md5sum "2ef17851ae7feaf2c4dd54cd54736717"
   size 21336
   provides ()
   requires (mule-base)
   type regular
))
(skk
  (standards-version 1.0
   version "1.03"
   author-version "10.38"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump t
   description "Japanese Language Input Method."
   filename "skk-1.03-pkg.tar.gz"
   md5sum "8da4b97eb3b4107adae3bf870257cbb9"
   size 1466864
   provides (skk skk-tut)
   requires (viper mule-base xemacs-base)
   type regular
))
(calendar
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Calendar and diary support."
   filename "calendar-1.03-pkg.tar.gz"
   md5sum "dd1477302a9ed29c06a309c947d3fb2e"
   size 168691
   provides (appt cal-dst cal-french cal-mayan cal-x cal-xemacs calendar diary-ins diary-lib holidays lunar solar)
   requires (xemacs-base)
   type regular
))
(edit-utils
  (standards-version 1.0
   version "1.07"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous editor extensions, you probably need this."
   filename "edit-utils-1.07-pkg.tar.gz"
   md5sum "ebdb25ce0968d7f0d2793c0e7a093dc2"
   size 518923
   provides (abbrevlist atomic-extents avoid backup-dir balloon-help big-menubar blink-cursor blink-paren bookmark compare-w completion dabbrev desktop detached-minibuf edit-toolbar fast-lock file-part floating-toolbar flow-ctrl foldout func-menu hippie-exp icomplete id-select info-look iswitchb lazy-lock lazy-shot live-icon man mic-paren paren mode-motion+ outl-mouse page-ext blink-paren paren permanent-buffers recent-files redo reportmail rsz-minibuf saveconfsavehist saveplace scroll-in-place toolbar-utils tree-menu uniquify where-was-i-db)
   requires (xemacs-base)
   type single
))
(forms
  (standards-version 1.0
   version "1.05"
   author-version "2.10"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Forms editing support (obsolete, use Widget instead)."
   filename "forms-1.05-pkg.tar.gz"
   md5sum "b2cb31ec9a5f6b59f28b05c21759b23c"
   size 39848
   provides (forms forms-mode)
   requires ()
   type regular
))
(frame-icon
  (standards-version 1.0
   version "1.02"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Set up mode-specific icons for each frame under XEmacs"
   filename "frame-icon-1.02-pkg.tar.gz"
   md5sum "d18342bd040c737fe74e6b6588b42e96"
   size 33248
   provides (forms forms-mode)
   requires ()
   type regular
))
(hm--html-menus
  (standards-version 1.0
   version "1.04"
   author-version "5.9"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "HTML editing."
   filename "hm--html-menus-1.04-pkg.tar.gz"
   md5sum "50a9d8d0a1543092cf7dfb7f0dce7aeb"
   size 146988
   provides (adapt hm--date hm--html-configuration hm--html-drag-and-drop hm--html-indentation hm--html-keys hm--html-menu hm--html-mode hm--html-not-standard hm--html html-view tmpl-minor-mode)
   requires (xemacs-base)
   type regular
))
(ispell
  (standards-version 1.0
   version "1.04"
   author-version "2.37"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Spell-checking with GNU ispell."
   filename "ispell-1.04-pkg.tar.gz"
   md5sum "56d350f27230d5b408be412e16ad1d36"
   size 55746
   provides (ispell)
   requires ()
   type regular
))
(pc
  (standards-version 1.0
   version "1.05"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "PC style interface emulation."
   filename "pc-1.05-pkg.tar.gz"
   md5sum "935e9b26c800a3b82ece0a4b3eac7343"
   size 11281
   provides (delbs pc-select pending-del s-region)
   requires (xemacs-base)
   type regular
))
(psgml
  (standards-version 1.0
   version "1.01"
   author-version "1.01"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Validated HTML/SGML editing."
   filename "psgml-1.01-pkg.tar.gz"
   md5sum "487f119aea6e8a7b982ab7c6cd143e3b"
   size 379283
   provides (psgml sgml tempo)
   requires ()
   type regular
))
(sgml
  (standards-version 1.0
   version "1.01"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "SGML/Linuxdoc-SGML editing."
   filename "sgml-1.01-pkg.tar.gz"
   md5sum "ff9341e50d595a553a61770b25682e6d"
   size 26866
   provides (sgml linuxdoc-sgml)
   requires (xemacs-base)
   type regular
))
(slider
  (standards-version 1.0
   version "1.05"
   author-version "0.3"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority low
   category "oa"
   dump nil
   description "User interface tool."
   filename "slider-1.05-pkg.tar.gz"
   md5sum "2cea293cb5aefde724d16c41e0ee269d"
   size 12085
   provides (slider color-selector)
   requires ()
   type regular
))
(speedbar
  (standards-version 1.0
   version "1.03"
   author-version "0.5.4x"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "??? Document me."
   filename "speedbar-1.03-pkg.tar.gz"
   md5sum "06422307016c770a860292f6548c0f3c"
   size 62468
   provides (speedbar)
   requires (xemacs-base)
   type regular
))
(strokes
  (standards-version 1.0
   version "1.01"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Mouse enhancement utility."
   filename "strokes-1.01-pkg.tar.gz"
   md5sum "7c741c59686b65a74606c037ef1ea5b1"
   size 43714
   provides (strokes)
   requires (text-modes edit-utils mail-lib xemacs-base)
   type regular
))
(text-modes
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous support for editing text files."
   filename "text-modes-1.03-pkg.tar.gz"
   md5sum "8573bc737fc11fd11b634dfa7fa1b617"
   size 103577
   provides (autoinsert crontab-edit filladapt image-mode iso-acc iso-ascii iso-cvt iso-insert iso-swed swedish tabify whitespace-mode winmgr-mode xpm-mode xrdb-mode)
   requires (xemacs-base)
   type regular
))
(time
  (standards-version 1.0
   version "1.03"
   author-version "1.17"
   date "1998-01-25"
   build-date "1998-01-25"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Display time & date on the modeline."
   filename "time-1.03-pkg.tar.gz"
   md5sum "19d6473ad84e913baff82e551f607acc"
   size 20033
   provides (time)
   requires (xemacs-base)
   type regular
))
(eterm
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Terminal emulation."
   filename "eterm-1.03-pkg.tar.gz"
   md5sum "6b8ef9310dbd121abe6b6896fc5ac10a"
   size 102075
   provides (eterm)
   requires (xemacs-base)
   type regular
))
(igrep
  (standards-version 1.0
   version "1.01"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Enhanced front-end for Grep."
   filename "igrep-1.01-pkg.tar.gz"
   md5sum "35883e177b034c3d58d795852fa68969"
   size 13957
   provides (igrep)
   requires (efs xemacs-base)
   type regular
))
(ilisp
  (standards-version 1.0
   version "1.03"
   author-version "5.8"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Front-end for Inferior Lisp."
   filename "ilisp-1.03-pkg.tar.gz"
   md5sum "28a131647f17713438f489957944242f"
   size 223275
   provides (ilisp completer)
   requires (xemacs-base)
   type regular
))
(os-utils
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Miscellaneous O/S utilities."
   filename "os-utils-1.04-pkg.tar.gz"
   md5sum "230dd8c50f5287786e7d4a6bcf864ca6"
   size 217403
   provides (archive-mode background crypt crypt++ inf-lisp jka-compr lpr ps-print tar-mode telnet terminal uncompress)
   requires (xemacs-base)
   type single
))
(view-process
  (standards-version 1.0
   version "1.03"
   author-version "2.4"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "A Unix process browsing tool."
   filename "view-process-1.03-pkg.tar.gz"
   md5sum "6c33516337b77605d7c474a7c565996e"
   size 59872
   provides (view-process-mode)
   requires (xemacs-base)
   type regular
))
(ada
  (standards-version 1.0
   version "1.03"
   author-version "2.27"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Ada language support."
   filename "ada-1.03-pkg.tar.gz"
   md5sum "12cf923735f28acd68f6664f8676f7b2"
   size 54311
   provides (ada-mode ada-stmt)
   requires ()
   type regular
))
(c-support
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Basic single-file add-ons for editing C code."
   filename "c-support-1.04-pkg.tar.gz"
   md5sum "2afcb4f1e6d9b2dab60171505e81e885"
   size 41821
   provides (c-comment-edit cmacexp hideif hideshow)
   requires (cc-mode xemacs-base)
   type regular
))
(cc-mode
  (standards-version 1.0
   version "1.05"
   author-version "5.19"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "C, C++ and Java language support."
   filename "cc-mode-1.05-pkg.tar.gz"
   md5sum "22b97f82faa9240de79eeaf3b505a95b"
   size 128672
   provides (cc-mode)
   requires (xemacs-base)
   type regular
))
(debug
  (standards-version 1.0
   version "1.01"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "GUD, gdb, dbx debugging support."
   filename "debug-1.01-pkg.tar.gz"
   md5sum "22be059d89ff639e11ffbb2da7c792ce"
   size 87149
   provides ()
   requires (xemacs-base)
   type regular
))
(ediff
  (standards-version 1.0
   version "1.03"
   author-version "2.70"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Interface over GNU patch."
   filename "ediff-1.03-pkg.tar.gz"
   md5sum "53981c4c53547b48f12de32ae386725d"
   size 241873
   provides (ediff)
   requires (pcl-cvs efs xemacs-base)
   type regular
))
(emerge
  (standards-version 1.0
   version "1.01"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Another interface over GNU patch."
   filename "emerge-1.01-pkg.tar.gz"
   md5sum "01cb5e705bd1a5400d9a137b1ed6b7c2"
   size 60912
   provides (emerge)
   requires ()
   type regular
))
(pcl-cvs
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "CVS frontend."
   filename "pcl-cvs-1.03-pkg.tar.gz"
   md5sum "1747e0ae7bdfc69e1bcb498ad9c050fe"
   size 134547
   provides (pcl-cvs dll elib-node generic-sc)
   requires (xemacs-base)
   type regular
))
(prog-modes
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Support for various programming languages."
   filename "prog-modes-1.03-pkg.tar.gz"
   md5sum "338ebcd25b844c448a9c7c2d5bac966b"
   size 535330
   provides (autoconf-mode cperl-mode eiffel3 f90 fortran ksh-mode m4-mode makefile perl-mode postscript python-mode rexx-mode simula-mode tcl teco verilog-mod)
   requires (mail-lib xemacs-base)
   type regular
))
(scheme
  (standards-version 1.0
   version "1.02"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Front-end support for Inferior Scheme."
   filename "scheme-1.02-pkg.tar.gz"
   md5sum "070fe92d28b95b548bcb713c2072f701"
   size 34793
   provides (scheme xscheme cmuscheme)
   requires (xemacs-base)
   type regular
))
(sh-script
  (standards-version 1.0
   version "1.04"
   author-version "2.0e"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Support for editing shell scripts."
   filename "sh-script-1.04-pkg.tar.gz"
   md5sum "35daef7a3c07cae2ff9011d625140004"
   size 33618
   provides (sh-script executable)
   requires (xemacs-base)
   type regular
))
(vc-cc
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump t
   description "Version Control for ClearCase (UnFree) systems."
   filename "vc-cc-1.03-pkg.tar.gz"
   md5sum "4c2b00b98f6581f89aa58b77049d5406"
   size 96065
   provides (vc)
   requires (efs xemacs-base)
   type regular
))
(vc
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump t
   description "Version Control for Free systems."
   filename "vc-1.03-pkg.tar.gz"
   md5sum "eff043dcb522dfe7bff4a5d11acd4d5f"
   size 74214
   provides (vc)
   requires (efs xemacs-base)
   type regular
))
(auctex
  (standards-version 1.0
   version "1.04"
   author-version "9.7p"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "wp"
   dump nil
   description "Basic TeX/LaTeX support."
   filename "auctex-1.04-pkg.tar.gz"
   md5sum "48359901fa551ae9ff3f77b996aa8507"
   size 289682
   provides (auc-old bib-cite font-latex latex multi-prompt tex-buf tex-info tex-jp tex-site tex)
   requires (xemacs-base)
   type regular
))
(crisp
  (standards-version 1.0
   version "1.03"
   author-version "1.31"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "Crisp/Brief emulation."
   filename "crisp-1.03-pkg.tar.gz"
   md5sum "45ffef47a1dfa2e3de4efb9958a053dc"
   size 9051
   provides (crisp scroll-lock)
   requires ()
   type regular
))
(edt
  (standards-version 1.0
   version "1.03"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "wp"
   dump nil
   description "DEC EDIT/EDT emulation."
   filename "edt-1.03-pkg.tar.gz"
   md5sum "da250582becf065c32cfd2c1182d0449"
   size 46005
   provides (edt)
   requires (xemacs-base)
   type regular
))
(texinfo
  (standards-version 1.0
   version "1.05"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "wp"
   dump nil
   description "XEmacs TeXinfo support."
   filename "texinfo-1.05-pkg.tar.gz"
   md5sum "8b9573a97dfc7dc9ddc716c741b8c9d4"
   size 113889
   provides (makeinfo tex-mode texinfmt texinfo texnfo-tex texnfo-upd)
   requires (xemacs-base)
   type regular
))
(textools
  (version "1.0"
   filename "textools-1.0-pkg.tar.gz"
   md5sum "c75f9e6b009a6f9892cd0aa68f8df17b"
   size 187454
   provides (bib-mode bibtex refer-to-bibtex reftex)
   requires (xemacs-base)
   type single
))
(tpu
  (standards-version 1.0
   version "1.04"
   author-version "20.5b21"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "wp"
   dump nil
   description "DEC EDIT/TPU support."
   filename "tpu-1.04-pkg.tar.gz"
   md5sum "6c7b0e97485b7ddf374fe5ceca45d74d"
   size 57401
   provides (tpu)
   requires ()
   type regular
))
(viper
  (standards-version 1.0
   version "1.04"
   author-version "3.005"
   date "1998-01-24"
   build-date "1998-01-24"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "VI emulation support."
   filename "viper-1.04-pkg.tar.gz"
   md5sum "ea2bf0b5bd6600bb5e5400d2fd21205f"
   size 260680
   provides (viper)
   requires (xemacs-base)
   type regular
))
))
