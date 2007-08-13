(setq package-get-base
'((eudc
  (standards-version 1.0
   version "1.20"
   author-version "1.20"
   date "1998-08-07"
   build-date "1998-08-11"
   maintainer "Oscar Figueiredo <Oscar.Figueiredo@epfl.ch>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Emacs Unified Directory Client (LDAP, PH)."
   filename "eudc-1.20-pkg.tar.gz"
   md5sum "0f54415850524bd1e38b11b1281fd77e"
   size 41817
   provides (eudc eudc-ldap eudc-ph)
   requires (fsf-compat xemacs-base)
   type regular
))
(footnote
  (standards-version 1.0
   version "1.04"
   author-version "0.18x"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "SL Baur <steve@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Footnoting in mail message editing modes."
   filename "footnote-1.04-pkg.tar.gz"
   md5sum "62cf5df16cbc95f78c836e68de8a425a"
   size 18254
   provides (footnote)
   requires (mail-lib xemacs-base)
   type regular
))
(gnats
  (standards-version 1.0
   version "1.05"
   author-version "3.101"
   date "1998-08-01"
   build-date "1998-08-03"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "comm"
   dump nil
   description "XEmacs bug reports."
   filename "gnats-1.05-pkg.tar.gz"
   md5sum "09eadea6fa48bb9e067cce19fb8d4452"
   size 126465
   provides (gnats gnats-admin send-pr)
   requires (mail-lib xemacs-base)
   type regular
))
(gnus
  (standards-version 1.0
   version "1.24"
   author-version "5.6.33"
   date "1998-08-11"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "comm"
   dump nil
   description "The Gnus Newsreader and Mailreader."
   filename "gnus-1.24-pkg.tar.gz"
   md5sum "c88bf1bae2578ab133697cd0e8cb7f68"
   size 1707259
   provides (gnus message)
   requires (gnus w3 mh-e mailcrypt rmail mail-lib xemacs-base)
   type regular
))
(mailcrypt
  (standards-version 1.0
   version "1.04"
   author-version "3.4"
   date "1998-01-24"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Support for messaging encryption with PGP."
   filename "mailcrypt-1.04-pkg.tar.gz"
   md5sum "66601a110f1499d3c6f815f806e43a71"
   size 66937
   provides (mailcrypt)
   requires (gnus vm mail-lib xemacs-base)
   type regular
))
(mew
  (standards-version 1.0
   version "1.0"
   author-version "1.93b38"
   date "1998-06-21"
   build-date "1998-06-21"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "Messaging in an Emacs World."
   filename "mew-1.0-pkg.tar.gz"
   md5sum "be366b8dd9495ecb7b3b6a7a46563faa"
   size 441775
   provides (mew)
   requires (mew)
   type regular
))
(mh-e
  (standards-version 1.0
   version "1.06"
   author-version "21.0"
   date "1998-07-12"
   build-date "1998-07-18"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "Front end support for MH."
   filename "mh-e-1.06-pkg.tar.gz"
   md5sum "444f2ff3becd5b9c06e880079a8ea094"
   size 129262
   provides (mh-e)
   requires (mail-lib xemacs-base)
   type regular
))
(net-utils
  (standards-version 1.0
   version "1.08"
   author-version "21.0"
   date "1998-07-01"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Miscellaneous Networking Utilities."
   filename "net-utils-1.08-pkg.tar.gz"
   md5sum "2591eca88f5ea04272012e479ea8665c"
   size 107983
   provides (ilisp-browse-cltl2 emacsbug feedmail metamail net-utils rcompile shadowfile webjump webster-www)
   requires (w3 efs mail-lib xemacs-base)
   type single
))
(rmail
  (standards-version 1.0
   version "1.04"
   author-version "21.0"
   date "1998-06-28"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "comm"
   dump nil
   description "An obsolete Emacs mailer."
   filename "rmail-1.04-pkg.tar.gz"
   md5sum "5a4fc73565cb0e9ea62d6b0665ccb013"
   size 85711
   provides (rmail rmailsum)
   requires (tm apel mail-lib xemacs-base)
   type regular
))
(supercite
  (standards-version 1.0
   version "1.08"
   author-version "3.55x2"
   date "1998-08-9"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "An Emacs citation tool for News & Mail messages."
   filename "supercite-1.08-pkg.tar.gz"
   md5sum "de5053422b5765bf48220a29bccf774b"
   size 71088
   provides (supercite)
   requires (mail-lib xemacs-base)
   type regular
))
(tm
  (standards-version 1.0
   version "1.11"
   author-version "21.0"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "comm"
   dump nil
   description "Emacs MIME support."
   filename "tm-1.11-pkg.tar.gz"
   md5sum "883566a0ff2b1eb48e37a81390d889fe"
   size 253020
   provides (tm tm-edit tm-view mime-setup)
   requires (gnus mh-e rmail vm mailcrypt mail-lib apel xemacs-base)
   type regular
))
(vm
  (standards-version 1.0
   version "1.10"
   author-version "6.53"
   date "1998-06-26"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "comm"
   dump nil
   description "An Emacs mailer."
   filename "vm-1.10-pkg.tar.gz"
   md5sum "96c26ebc950f790c775c8ff5199160fd"
   size 514306
   provides (vm)
   requires (mail-lib xemacs-base)
   type regular
))
(w3
  (standards-version 1.0
   version "1.08"
   author-version "4.0pre23"
   date "1998-07-18"
   build-date "1998-07-18"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority high
   category "comm"
   dump nil
   description "A Web browser."
   filename "w3-1.08-pkg.tar.gz"
   md5sum "ebde805d2dd99decbaaca2d6b4d0a87f"
   size 586478
   provides (w3 url)
   requires (w3 mail-lib xemacs-base)
   type regular
))
(cookie
  (standards-version 1.0
   version "1.07"
   author-version "21.0b36"
   date "1998-04-07"
   build-date "1998-04-17"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Spook and Yow (Zippy quotes)."
   filename "cookie-1.07-pkg.tar.gz"
   md5sum "df97f80082395667a0e23eda8f68b8dd"
   size 34184
   provides (cookie1 yow)
   requires (xemacs-base)
   type regular
))
(games
  (standards-version 1.0
   version "1.05"
   author-version "1.04"
   date "1998-06-04"
   build-date "1998-07-09"
   maintainer "Glynn Clements <glynn@sensei.co.uk>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Tetris, Sokoban, and Snake."
   filename "games-1.05-pkg.tar.gz"
   md5sum "2b856bc25a05ad32400bdd947fec6231"
   size 32000
   provides (gamegrid snake tetris sokoban)
   requires (xemacs-base)
   type regular
))
(mine
  (standards-version 1.0
   version "1.07"
   author-version "1.9"
   date "1998-05-09"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Minehunt Game."
   filename "mine-1.07-pkg.tar.gz"
   md5sum "85b19bc650dd0934b1c9240c0426c438"
   size 66617
   provides (xmine)
   requires (xemacs-base)
   type regular
))
(misc-games
  (standards-version 1.0
   version "1.06"
   author-version "21.0b35"
   date "1998-03-22"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "games"
   dump nil
   description "Other amusements and diversions."
   filename "misc-games-1.06-pkg.tar.gz"
   md5sum "48d883e7e6092c227b476386ece41672"
   size 165586
   provides (decipher gomoku hanoi life morse rot13)
   requires (xemacs-base)
   type single
))
(Sun
  (standards-version 1.0
   version "1.07"
   author-version "21.0"
   date "1998-07-25"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution sun
   priority low
   category "libs"
   dump nil
   description "Support for Sparcworks."
   filename "Sun-1.07-pkg.tar.gz"
   md5sum "c3033491cd9ea137e6ba45e96250d05f"
   size 63753
   provides (sccs eos-browser eos-common eos-debugger eos-debugger eos-editor eos-init eos-load eos-menubar eos-toolbar sunpro)
   requires (cc-mode xemacs-base)
   type regular
))
(apel
  (standards-version 1.0
   version "1.05"
   author-version "3.3"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "A Portable Emacs Library.  Used by XEmacs MIME support."
   filename "apel-1.05-pkg.tar.gz"
   md5sum "4a375d58b0a95372aaae618514e4bb45"
   size 34660
   provides (atype emu-20 emu-e19 emu-x20 emu-xemacs emu file-detect filename install mule-caesar path-util richtext std11-parse std11 tinyrich)
   requires (fsf-compat xemacs-base)
   type regular
))
(dired
  (standards-version 1.0
   version "1.01"
   author-version "7.9"
   date "1998-05-05"
   build-date "1998-05-05"
   maintainer "Mike Sperber <sperber@informatik.uni-tuebingen.de>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Manage file systems."
   filename "dired-1.01-pkg.tar.gz"
   md5sum "d9748d8e8af8a63095aaaab9924987ef"
   size 187526
   provides (diff dired)
   requires (xemacs-base)
   type regular
))
(edebug
  (standards-version 1.0
   version "1.04"
   author-version "21.0b35"
   date "1998-03-12"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "libs"
   dump nil
   description "An Emacs Lisp debugger."
   filename "edebug-1.04-pkg.tar.gz"
   md5sum "d4a46e9bee361d60cb079731e5b152e9"
   size 118141
   provides (edebug cl-read cust-print eval-reg cl-specs)
   requires (xemacs-base)
   type regular
))
(efs
  (standards-version 1.0
   version "1.08"
   author-version "1.16x1"
   date "1998-03-21"
   build-date "1998-04-04"
   maintainer "Mike Sperber <sperber@informatik.uni-tuebingen.de>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Treat files on remote systems the same as local files."
   filename "efs-1.08-pkg.tar.gz"
   md5sum "1ec45851fe72d06d32a6f941877ae544"
   size 347544
   provides (efs)
   requires (xemacs-base vm dired)
   type regular
))
(fsf-compat
  (standards-version 1.0
   version "1.0"
   author-version "21.0b39"
   date "1998-03-25"
   build-date "1998-05-06"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "FSF Emacs compatibility files."
   filename "fsf-compat-1.0-pkg.tar.gz"
   md5sum "71351ff26a69b341015612d9b88dfc55"
   size 16083
   provides (overlay thingatpt timer)
   requires ()
   type single
))
(mail-lib
  (standards-version 1.0
   version "1.16"
   author-version "21.0"
   date "1998-06-08"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Fundamental lisp files for providing email support."
   filename "mail-lib-1.16-pkg.tar.gz"
   md5sum "8466339df937c3e7dc4176df85987cf3"
   size 120230
   provides (browse-url highlight-headers mail-abbrevs mail-extr mail-utils reporter rfc822 rmail-mini rmailout sendmail smtpmail)
   requires (xemacs-base)
   type regular
))
(sounds-au
  (standards-version 1.0
   version "1.02"
   author-version "21.0"
   date "1998-06-30"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "libs"
   dump nil
   description "XEmacs Sun sound files."
   filename "sounds-au-1.02-pkg.tar.gz"
   md5sum "061ab67267c7cdfe37472141130d19ff"
   size 125736
   provides ()
   requires ()
   type regular
))
(sounds-wav
  (standards-version 1.0
   version "1.02"
   author-version "21.0"
   date "1998-06-30"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "libs"
   dump nil
   description "XEmacs Microsoft sound files."
   filename "sounds-wav-1.02-pkg.tar.gz"
   md5sum "c970808088c408bfd780dc8466a848b3"
   size 148621
   provides ()
   requires ()
   type regular
))
(tooltalk
  (standards-version 1.0
   version "1.06"
   author-version "21.0"
   date "1998-07-25"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "libs"
   dump nil
   description "Support for building with Tooltalk."
   filename "tooltalk-1.06-pkg.tar.gz"
   md5sum "5be8c4fdc0d69a73e091d23bbf887328"
   size 9265
   provides ()
   requires ()
   type regular
))
(xemacs-base
  (standards-version 1.0
   version "1.23"
   author-version "21.0"
   date "1998-08-06"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "libs"
   dump nil
   description "Fundamental XEmacs support, you almost certainly need this."
   filename "xemacs-base-1.23-pkg.tar.gz"
   md5sum "ce47be3e72fb0b54a363f4d2722b8ec4"
   size 458552
   provides (add-log advice annotations assoc case-table chistory comint-xemacs comint compile debug ebuff-menu echistory edmacro ehelp electric enriched env facemenu ffap helper imenu iso-syntax macros novice outline overlay passwd pp regi ring shell skeleton sort thing time-stamp timezone xbm-button xpm-button)
   requires ()
   type regular
))
(xemacs-devel
  (standards-version 1.0
   version "1.14"
   author-version "21.0"
   date "1998-07-14"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "libs"
   dump nil
   description "Emacs Lisp developer support."
   filename "xemacs-devel-1.14-pkg.tar.gz"
   md5sum "978fdde5b7801c077a337f6d122bec1f"
   size 78634
   provides (docref eldoc elp find-func hide-copyleft ielm regexp-opt trace)
   requires (xemacs-base)
   type single
))
(edict
  (standards-version 1.0
   version "1.04"
   author-version "0.9.8"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "Stephen J. Turnbull <turnbull@sk.tsukuba.ac.jp>"
   distribution mule
   priority high
   category "mule"
   dump nil
   description "Lisp Interface to EDICT, Kanji Dictionary"
   filename "edict-1.04-pkg.tar.gz"
   md5sum "15cec90e8d8aac7cbf9f5bd86f824026"
   size 94846
   provides (dui-registry dui edict-edit edict-english edict-japanese edict-morphology edict-test edict ts-mode)
   requires (mule-base xemacs-base)
   type regular
))
(egg-its
  (standards-version 1.0
   version "1.09"
   author-version "21.0"
   date "1998-08-11"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump t
   description "Wnn (4.2 and 6) support.  SJ3 support."
   filename "egg-its-1.09-pkg.tar.gz"
   md5sum "5f44ea2d8e1dc96fce45e651206c69cd"
   size 260216
   provides (egg-cnpinyin egg-cnzhuyin egg-cwnn-leim egg-jisx0201 egg-jsymbol egg-kwnn-leim egg-leim egg-sj3-client egg-sj3-leim egg-sj3 egg-wnn egg)
   requires (leim mule-base fsf-compat xemacs-base)
   type regular
))
(leim
  (standards-version 1.0
   version "1.08"
   author-version "21.0"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump nil
   description "Quail.  All non-English and non-Japanese language support."
   filename "leim-1.08-pkg.tar.gz"
   md5sum "54aa493e13bb282923f18cfbbb344b37"
   size 1744040
   provides ()
   requires (mule-base fsf-compat xemacs-base)
   type regular
))
(locale
  (standards-version 1.0
   version "1.08"
   author-version "21.0"
   date "1998-07-24"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump nil
   description "Localized menubars and localized splash screens."
   filename "locale-1.08-pkg.tar.gz"
   md5sum "0a671e426762e5dedc182e1c489201fb"
   size 34001
   provides ()
   requires (mule-base)
   type regular
))
(mule-base
  (standards-version 1.0
   version "1.20"
   author-version "21.0"
   date "1998-07-13"
   build-date "1998-07-28"
   maintainer "SL Baur <steve@altair.xemacs.org>"
   distribution mule
   priority high
   category "mule"
   dump t
   description "Basic Mule support, required for building with Mule."
   filename "mule-base-1.20-pkg.tar.gz"
   md5sum "ed144fd26691f40e539deadc1675d771"
   size 489195
   provides (canna-leim canna char-table china-util cyril-util isearch-ext japan-util ccl can-n-egg mule-help)
   requires (fsf-compat xemacs-base)
   type regular
))
(skk
  (standards-version 1.0
   version "1.08"
   author-version "10.38"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "SL Baur <steve@altair.xemacs.org>"
   distribution mule
   priority medium
   category "mule"
   dump t
   description "Japanese Language Input Method."
   filename "skk-1.08-pkg.tar.gz"
   md5sum "18f1e3989d875c1ceec7ec8f2e7f8a96"
   size 1467704
   provides (skk skk-tut)
   requires (viper mule-base xemacs-base)
   type regular
))
(calc
  (standards-version 1.0
   version "1.07"
   author-version "2.02fX3"
   date "1998-07-25"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Emacs calculator"
   filename "calc-1.07-pkg.tar.gz"
   md5sum "8859c298c097d38cbeb9b11c51bdafe6"
   size 1165227
   provides (calc)
   requires ()
   type regular
))
(calendar
  (standards-version 1.0
   version "1.05"
   author-version "21.0"
   date "1998-07-20"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Calendar and diary support."
   filename "calendar-1.05-pkg.tar.gz"
   md5sum "bb8ea5f92394889f5c4577549b265ef6"
   size 238420
   provides (appt cal-dst cal-french cal-mayan cal-x cal-xemacs calendar diary-ins diary-lib holidays lunar solar)
   requires (xemacs-base)
   type regular
))
(edit-utils
  (standards-version 1.0
   version "1.27"
   author-version "21.0"
   date "1998-07-20"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous editor extensions, you probably need this."
   filename "edit-utils-1.27-pkg.tar.gz"
   md5sum "38a2397c5ded12717b256f923ab89104"
   size 604746
   provides (abbrevlist atomic-extents avoid backup-dir balloon-help big-menubar blink-cursor blink-paren bookmark compare-w completion dabbrev desktop detached-minibuf edit-toolbar fast-lock file-part floating-toolbar flow-ctrl foldout func-menu hippie-exp icomplete id-select info-look iswitchb lazy-lock lazy-shot live-icon man mic-paren paren popper mode-motion+ outl-mouse page-ext blink-paren paren permanent-buffers recent-files redo reportmail rsz-minibuf saveconfsavehist saveplace scroll-in-place tempo toolbar-utils tree-menu uniquify where-was-i-db)
   requires (xemacs-base)
   type single
))
(forms
  (standards-version 1.0
   version "1.06"
   author-version "2.10"
   date "1998-01-25"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Forms editing support (obsolete, use Widget instead)."
   filename "forms-1.06-pkg.tar.gz"
   md5sum "ebee64ebf564f934e15fed3503e3b15e"
   size 39948
   provides (forms forms-mode)
   requires ()
   type regular
))
(frame-icon
  (standards-version 1.0
   version "1.03"
   author-version "21.0"
   date "1998-07-14"
   build-date "1998-07-18"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "Set up mode-specific icons for each frame under XEmacs"
   filename "frame-icon-1.03-pkg.tar.gz"
   md5sum "82f0714e2ee9ace4b7dba62ea3607f3c"
   size 33149
   provides (forms forms-mode)
   requires ()
   type regular
))
(hm--html-menus
  (standards-version 1.0
   version "1.06"
   author-version "5.9"
   date "1998-01-25"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "HTML editing."
   filename "hm--html-menus-1.06-pkg.tar.gz"
   md5sum "2570d8211b63c2edcc114ec3560a075f"
   size 147168
   provides (adapt hm--date hm--html-configuration hm--html-drag-and-drop hm--html-indentation hm--html-keys hm--html-menu hm--html-mode hm--html-not-standard hm--html html-view tmpl-minor-mode)
   requires (xemacs-base)
   type regular
))
(ispell
  (standards-version 1.0
   version "1.08"
   author-version "3.0x1"
   date "1998-04-01"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "Spell-checking with GNU ispell."
   filename "ispell-1.08-pkg.tar.gz"
   md5sum "54cd76987a472eca72c24592a10756d6"
   size 64990
   provides (ispell)
   requires ()
   type regular
))
(pc
  (standards-version 1.0
   version "1.11"
   author-version "21.0"
   date "1998-07-25"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "PC style interface emulation."
   filename "pc-1.11-pkg.tar.gz"
   md5sum "aee63a0ceda69a5217e182a6ee35518a"
   size 16350
   provides (delbs fusion pc-select pending-del s-region)
   requires (xemacs-base)
   type regular
))
(psgml
  (standards-version 1.0
   version "1.08"
   author-version "1.01"
   date "1998-07-06"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Validated HTML/SGML editing."
   filename "psgml-1.08-pkg.tar.gz"
   md5sum "757842225e4d3e9841bf6de1d3fdbbc4"
   size 419252
   provides (psgml sgml)
   requires (edit-utils)
   type regular
))
(sgml
  (standards-version 1.0
   version "1.01"
   author-version "21.0b35"
   date "1998-01-25"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "oa"
   dump nil
   description "SGML/Linuxdoc-SGML editing."
   filename "sgml-1.01-pkg.tar.gz"
   md5sum "4e7039730eb4399c09b1a85d1758381c"
   size 26874
   provides (sgml linuxdoc-sgml)
   requires (xemacs-base)
   type regular
))
(slider
  (standards-version 1.0
   version "1.05"
   author-version "0.3"
   date "1998-01-25"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution experimental
   priority low
   category "oa"
   dump nil
   description "User interface tool."
   filename "slider-1.05-pkg.tar.gz"
   md5sum "67b376e5b886a78f5094eb13c61ff8ec"
   size 12116
   provides (slider color-selector)
   requires ()
   type regular
))
(speedbar
  (standards-version 1.0
   version "1.05"
   author-version "0.6.2"
   date "1998-02-07"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "oa"
   dump nil
   description "??? Document me."
   filename "speedbar-1.05-pkg.tar.gz"
   md5sum "8a988bada9d09dac0e934f0859f88613"
   size 95018
   provides (speedbar)
   requires (xemacs-base)
   type regular
))
(strokes
  (standards-version 1.0
   version "1.01"
   author-version "21.0b35"
   date "1998-01-25"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Mouse enhancement utility."
   filename "strokes-1.01-pkg.tar.gz"
   md5sum "a160a62e0570fc69f3c03b6ee1693fcd"
   size 43743
   provides (strokes)
   requires (text-modes edit-utils mail-lib xemacs-base)
   type regular
))
(text-modes
  (standards-version 1.0
   version "1.08"
   author-version "21.0"
   date "1998-07-03"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "oa"
   dump nil
   description "Miscellaneous support for editing text files."
   filename "text-modes-1.08-pkg.tar.gz"
   md5sum "7334a90ddbcedec459caecf8e0314bad"
   size 171811
   provides (autoinsert crontab-edit filladapt fold-isearch folding image-mode iso-acc iso-ascii iso-cvt iso-insert iso-swed swedish tabify whitespace-mode winmgr-mode xpm-mode xrdb-mode)
   requires (fsf-compat xemacs-base)
   type regular
))
(time
  (standards-version 1.0
   version "1.04"
   author-version "1.17"
   date "1998-04-24"
   build-date "1998-04-26"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "oa"
   dump nil
   description "Display time & date on the modeline."
   filename "time-1.04-pkg.tar.gz"
   md5sum "e25caf29cf9684887460d9cd124639d4"
   size 19905
   provides (time)
   requires (xemacs-base)
   type regular
))
(eterm
  (standards-version 1.0
   version "1.05"
   author-version "21.0"
   date "1998-06-28"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Terminal emulation."
   filename "eterm-1.05-pkg.tar.gz"
   md5sum "0c1660a9a8426077534caf84762e7ec1"
   size 144233
   provides (eterm)
   requires (xemacs-base)
   type regular
))
(igrep
  (standards-version 1.0
   version "1.02"
   author-version "2.83"
   date "1998-08-11"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Enhanced front-end for Grep."
   filename "igrep-1.02-pkg.tar.gz"
   md5sum "b0c0551e8a48170a98dd00b5a805a7f6"
   size 15005
   provides (igrep)
   requires (dired xemacs-base)
   type regular
))
(ilisp
  (standards-version 1.0
   version "1.04"
   author-version "5.8"
   date "1998-01-24"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "Front-end for Inferior Lisp."
   filename "ilisp-1.04-pkg.tar.gz"
   md5sum "1fa1b08bd6b7cc3c71f512ad412e1b24"
   size 223559
   provides (ilisp completer)
   requires (xemacs-base)
   type regular
))
(os-utils
  (standards-version 1.0
   version "1.09"
   author-version "21.0"
   date "1998-07-14"
   build-date "1998-07-18"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "os"
   dump nil
   description "Miscellaneous O/S utilities."
   filename "os-utils-1.09-pkg.tar.gz"
   md5sum "7ac91db01771a95da824e97696e4a01b"
   size 229975
   provides (archive-mode background crypt crypt++ inf-lisp jka-compr lpr mchat ps-print tar-mode telnet terminal uncompress)
   requires (xemacs-base)
   type single
))
(view-process
  (standards-version 1.0
   version "1.03"
   author-version "2.4"
   date "1998-01-24"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "os"
   dump nil
   description "A Unix process browsing tool."
   filename "view-process-1.03-pkg.tar.gz"
   md5sum "96bcf35e325034ee3c37563fecfe623d"
   size 59886
   provides (view-process-mode)
   requires (xemacs-base)
   type regular
))
(ada
  (standards-version 1.0
   version "1.03"
   author-version "2.27"
   date "1998-01-24"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Ada language support."
   filename "ada-1.03-pkg.tar.gz"
   md5sum "661f8c0ac17fe447f8cc0e54f753704d"
   size 54323
   provides (ada-mode ada-stmt)
   requires ()
   type regular
))
(c-support
  (standards-version 1.0
   version "1.07"
   author-version "21.0b35"
   date "1998-03-25"
   build-date "1998-04-04"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Basic single-file add-ons for editing C code."
   filename "c-support-1.07-pkg.tar.gz"
   md5sum "771e606d76e18922efb6559e101c7ecf"
   size 68651
   provides (c-comment-edit cmacexp ctypes hideif hideshow)
   requires (cc-mode xemacs-base)
   type regular
))
(cc-mode
  (standards-version 1.0
   version "1.11"
   author-version "5.22"
   date "1998-03-05"
   build-date "1998-06-14"
   maintainer "Barry Warsaw <cc-mode-help@python.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "C, C++ and Java language support."
   filename "cc-mode-1.11-pkg.tar.gz"
   md5sum "dadf89d5a4dfbee90d0168831a33150f"
   size 151138
   provides (cc-mode)
   requires (xemacs-base)
   type regular
))
(debug
  (standards-version 1.0
   version "1.04"
   author-version "21.0"
   date "1998-07-09"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "GUD, gdb, dbx debugging support."
   filename "debug-1.04-pkg.tar.gz"
   md5sum "f881ca1a0593d218ca6a0e19dd10d8a0"
   size 90350
   provides (dbx gdb-highlight gdb gdbsrc gud history)
   requires (xemacs-base)
   type regular
))
(ediff
  (standards-version 1.0
   version "1.08"
   author-version "2.70.1"
   date "1998-04-27"
   build-date "1998-05-15"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Interface over GNU patch."
   filename "ediff-1.08-pkg.tar.gz"
   md5sum "d73e47087119a6cb7d5b4f71fdba8b72"
   size 243042
   provides (ediff)
   requires (pcl-cvs dired xemacs-base)
   type regular
))
(emerge
  (standards-version 1.0
   version "1.02"
   author-version "21.0b36"
   date "1998-04-07"
   build-date "1998-04-17"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Another interface over GNU patch."
   filename "emerge-1.02-pkg.tar.gz"
   md5sum "6f7687196172109d6014346d5ead6d3a"
   size 60940
   provides (emerge)
   requires ()
   type regular
))
(jde
  (standards-version 1.0
   version "1.04"
   author-version "2.05"
   date "1998-07-09"
   build-date "1998-07-09"
   maintainer "Andy Piper <andyp@parallax.co.uk>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Java language and development support."
   filename "jde-1.04-pkg.tar.gz"
   md5sum "97b90e88928033f405005a9441b7e141"
   size 126784
   provides (jde)
   requires (cc-mode debug speedbar edit-utils mail-lib xemacs-base)
   type regular
))
(pcl-cvs
  (standards-version 1.0
   version "1.11"
   author-version "21.0"
   date "1998-06-18"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "CVS frontend."
   filename "pcl-cvs-1.11-pkg.tar.gz"
   md5sum "7592786d2734d87778915e50561c472d"
   size 141698
   provides (pcl-cvs dll elib-node generic-sc)
   requires (xemacs-base)
   type regular
))
(prog-modes
  (standards-version 1.0
   version "1.08"
   author-version "21.0"
   date "1998-07-20"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "prog"
   dump nil
   description "Support for various programming languages."
   filename "prog-modes-1.08-pkg.tar.gz"
   md5sum "46d3c9671760fe10b159967f4f6960ed"
   size 537430
   provides (autoconf-mode cperl-mode eiffel3 f90 fortran ksh-mode m4-mode makefile perl-mode postscript python-mode rexx-mode simula-mode tcl teco verilog-mod)
   requires (mail-lib xemacs-base)
   type regular
))
(scheme
  (standards-version 1.0
   version "1.03"
   author-version "21.0b36"
   date "1998-04-11"
   build-date "1998-04-17"
   maintainer "Karl M. Hegbloom <karlheg@bittersweet.inetarena.com>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Front-end support for Inferior Scheme."
   filename "scheme-1.03-pkg.tar.gz"
   md5sum "f22026713da1be70eba93f8d59700499"
   size 36833
   provides (scheme xscheme cmuscheme cmuscheme48)
   requires (xemacs-base)
   type regular
))
(sh-script
  (standards-version 1.0
   version "1.05"
   author-version "2.0e"
   date "1998-05-12"
   build-date "1998-05-15"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Support for editing shell scripts."
   filename "sh-script-1.05-pkg.tar.gz"
   md5sum "8462bd33b9edc71da72ebd134b8a77c6"
   size 33785
   provides (sh-script executable)
   requires (xemacs-base)
   type regular
))
(vc-cc
  (standards-version 1.0
   version "1.07"
   author-version "21.0"
   date "1998-07-24"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "prog"
   dump nil
   description "Version Control for ClearCase (UnFree) systems."
   filename "vc-cc-1.07-pkg.tar.gz"
   md5sum "a50aa99e76d620f1165526529eb5980d"
   size 96445
   provides (vc)
   requires (dired xemacs-base)
   type regular
))
(vc
  (standards-version 1.0
   version "1.11"
   author-version "21.0"
   date "1998-08-05"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Version Control for Free systems."
   filename "vc-1.11-pkg.tar.gz"
   md5sum "4153e30aa432bbb7831522dc4d2b9905"
   size 83755
   provides (vc)
   requires (dired xemacs-base)
   type regular
))
(vhdl
  (standards-version 1.0
   version "1.04"
   author-version "2.74"
   date "1998-01-24"
   build-date "1998-06-14"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "prog"
   dump nil
   description "Support for VHDL."
   filename "vhdl-1.04-pkg.tar.gz"
   md5sum "8de144972dd6f33bcdd43314e6e6564d"
   size 54169
   provides (vhdl-mode)
   requires ()
   type regular
))
(auctex
  (standards-version 1.0
   version "1.11"
   author-version "9.7p"
   date "1998-08-05"
   build-date "1998-08-11"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority medium
   category "wp"
   dump nil
   description "Basic TeX/LaTeX support."
   filename "auctex-1.11-pkg.tar.gz"
   md5sum "de3b63d3e1e38a3727f0c5b2108745bf"
   size 306729
   provides (auc-old bib-cite font-latex latex multi-prompt tex-buf tex-info tex-jp tex-site tex)
   requires (xemacs-base)
   type regular
))
(crisp
  (standards-version 1.0
   version "1.04"
   author-version "1.33"
   date "1998-01-24"
   build-date "1998-07-09"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "Crisp/Brief emulation."
   filename "crisp-1.04-pkg.tar.gz"
   md5sum "2a51917984d7556019b1b20ff85a9feb"
   size 10189
   provides (crisp scroll-lock)
   requires ()
   type regular
))
(edt
  (standards-version 1.0
   version "1.04"
   author-version "21.0b36"
   date "1998-04-07"
   build-date "1998-04-17"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution contrib
   priority low
   category "wp"
   dump nil
   description "DEC EDIT/EDT emulation."
   filename "edt-1.04-pkg.tar.gz"
   md5sum "fabfedc63988de7296eae068d8b78ae0"
   size 46095
   provides (edt)
   requires (xemacs-base)
   type regular
))
(reftex
  (standards-version 1.0
   version "1.05"
   author-version "3.34"
   date "1998-08-11"
   build-date "1998-08-11"
   maintainer "Carsten Dominik <dominik@strw.LeidenUniv.nl>"
   distribution stable
   priority medium
   category "wp"
   dump nil
   description "Emacs support for LaTeX cross-references, citations.."
   filename "reftex-1.05-pkg.tar.gz"
   md5sum "d16db345599d47be1c99f6fccfdaaf90"
   size 166434
   provides (reftex)
   requires (fsf-compat xemacs-base)
   type regular
))
(texinfo
  (standards-version 1.0
   version "1.11"
   author-version "21.0"
   date "1998-07-20"
   build-date "1998-07-28"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority high
   category "wp"
   dump nil
   description "XEmacs TeXinfo support."
   filename "texinfo-1.11-pkg.tar.gz"
   md5sum "44fc14e758771d362433a657281f3756"
   size 127833
   provides (makeinfo tex-mode texinfmt texinfo texnfo-tex texnfo-upd)
   requires (xemacs-base)
   type regular
))
(textools
  (standards-version 1.0
   version "1.05"
   author-version "21.0b38"
   date "1998-04-29"
   build-date "1998-05-01"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stabl
   priority medium
   category "wp"
   dump nil
   description "Miscellaneous TeX support."
   filename "textools-1.05-pkg.tar.gz"
   md5sum "4b0a417849ca270ed498c1e9c9aaa07b"
   size 79125
   provides (bib-mode bibtex refer-to-bibtex)
   requires (xemacs-base)
   type single
))
(tpu
  (standards-version 1.0
   version "1.05"
   author-version "4.2X"
   date "1998-07-23"
   build-date "1998-07-28"
   maintainer "Kevin Oberman <oberman@es.net>"
   distribution normal
   priority medium
   category "wp"
   dump nil
   description "DEC EDIT/TPU support."
   filename "tpu-1.05-pkg.tar.gz"
   md5sum "ea158daa3dd9c98cee4acaff78866f0c"
   size 57851
   provides (tpu)
   requires ()
   type regular
))
(viper
  (standards-version 1.0
   version "1.08"
   author-version "3.03"
   date "1998-02-25"
   build-date "1998-06-01"
   maintainer "XEmacs Development Team <xemacs-beta@xemacs.org>"
   distribution stable
   priority low
   category "wp"
   dump nil
   description "VI emulation support."
   filename "viper-1.08-pkg.tar.gz"
   md5sum "f36b7e49bda79a19d7beeeeb6092bedd"
   size 261090
   provides (viper)
   requires (xemacs-base)
   type regular
))
))
(provide 'package-get-base)
