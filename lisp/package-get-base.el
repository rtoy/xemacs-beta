(setq package-get-base
'((footnote
  (version "1.01"
   filename "footnote-1.01-pkg.tar.gz"
   md5sum "6cca2b03fe2ed76f664a398d436bf954"
   size 17986
   provides (footnote)
   requires (mail-lib xemacs-base)
   type regular
))
(gnats
  (version "1.01"
   filename "gnats-1.01-pkg.tar.gz"
   md5sum "6bfb95bc283102cbbe0a5fc7b3f7d727"
   size 126295
   provides (gnats gnats-admin send-pr)
   requires (mail-lib xemacs-base)
   type regular
))
(mailcrypt
  (version "1.02"
   filename "mailcrypt-1.02-pkg.tar.gz"
   md5sum "a4bdb22e770882d1bf465e46736d67e3"
   size 66763
   provides (mailcrypt)
   requires (gnus vm mail-lib xemacs-base)
   type regular
))
(mh-e
  (version "1.03"
   filename "mh-e-1.03-pkg.tar.gz"
   md5sum "3a82a8fd5c3d69b375b8e6191edef061"
   size 128925
   provides (mh-e)
   requires (mail-lib xemacs-base)
   type regular
))
(net-utils
  (version "1.01"
   filename "net-utils-1.01-pkg.tar.gz"
   md5sum "a5e186be46e80d01367ca1b2923655e6"
   size 48550
   provides (ilisp-browse-cltl2 emacsbug feedmail metamail rcompile shadowfile webjump webster-www)
   requires (w3 efs mail-lib xemacs-base)
   type regular
))
(ph
  (version "1.01"
   filename "ph-1.01-pkg.tar.gz"
   md5sum "f7e686b77eb427a505a10348a0e9ec59"
   size 27483
   provides (ph)
   requires (xemacs-base)
   type regular
))
(rmail
  (version "1.01"
   filename "rmail-1.01-pkg.tar.gz"
   md5sum "d8a0b4457820839c3383d59c2e0c3ed6"
   size 83541
   provides (rmail rmailsum)
   requires (tm apel mail-lib xemacs-base)
   type regular
))
(supercite
  (version "1.02"
   filename "supercite-1.02-pkg.tar.gz"
   md5sum "19327a45b407218b67c0dfeb143c7a18"
   size 69344
   provides (supercite)
   requires (mail-lib xemacs-base)
   type regular
))
(tm
  (version "1.02"
   filename "tm-1.02-pkg.tar.gz"
   md5sum "fa55affd4762141dc1a8090565438bbe"
   size 180988
   provides (tm tm-edit tm-view mime-setup)
   requires (gnus vm mailcrypt mail-lib xemacs-base)
   type regular
))
(vm
  (version "1.03"
   filename "vm-1.03-pkg.tar.gz"
   md5sum "c998cbe419f3d482050632ee7115240f"
   size 476245
   provides (vm)
   requires (mail-lib xemacs-base)
   type regular
))
(w3
  (version "1.01"
   filename "w3-1.01-pkg.tar.gz"
   md5sum "7f896771d25cef4e9e606536bdb13f2e"
   size 582585
   provides (w3 url)
   requires (w3 mail-lib xemacs-base)
   type regular
))
(cookie
  (version "1.03"
   filename "cookie-1.03-pkg.tar.gz"
   md5sum "28b2fee1f0acbc8f19af12a415743c3f"
   size 33784
   provides (cookie1 yow)
   requires (xemacs-base)
   type regular
))
(games
  (version "1.01"
   filename "games-1.01-pkg.tar.gz"
   md5sum "844d4f68950614f11c078649c231c017"
   size 29160
   provides (gamegrid snake tetris sokoban)
   requires (xemacs-base)
   type regular
))
(mine
  (version "1.03"
   filename "mine-1.03-pkg.tar.gz"
   md5sum "ca06e2e17ac4f5695e66e7004fc7b860"
   size 67172
   provides (xmine)
   requires (xemacs-base)
   type regular
))
(misc-games
  (version "1.04"
   filename "misc-games-1.04-pkg.tar.gz"
   md5sum "07ad46ffdf17ef846c40e81d15fcd832"
   size 163171
   provides (decipher gomoku hanoi life)
   requires (xemacs-base)
   type single
))
(Sun
  (version "1.03"
   filename "Sun-1.03-pkg.tar.gz"
   md5sum "1c121ffcac7a00a653c144d488b43860"
   size 63758
   provides (sccs eos-browser eos-common eos-debugger eos-debugger eos-editor eos-init eos-load eos-menubar eos-toolbar sunpro)
   requires (cc-mode xemacs-base)
   type regular
))
(apel
  (version "1.02"
   filename "apel-1.02-pkg.tar.gz"
   md5sum "57230c9a3ed91126ea97277c87b9e72b"
   size 34606
   provides (atype emu-20 emu-e19 emu-x20 emu-xemacs emu file-detect filename install mule-caesar path-util richtext std11-parse std11 tinyrich)
   requires (xemacs-base)
   type regular
))
(edebug
  (version "1.02"
   filename "edebug-1.02-pkg.tar.gz"
   md5sum "5c89f5421ecabb1f026a93b8a4a1db0b"
   size 117931
   provides (edebug cl-read cust-print eval-reg cl-specs)
   requires (xemacs-base)
   type regular
))
(efs
  (version "1.03"
   filename "efs-1.03-pkg.tar.gz"
   md5sum "7bc6786f8dee4d955d6365f0cbe2b610"
   size 540959
   provides (efs diff dired efs-auto)
   requires (xemacs-base)
   type regular
))
(mail-lib
  (version "1.04"
   filename "mail-lib-1.04-pkg.tar.gz"
   md5sum "59b9cd8aefaa035cb6de8ebdfc141b0a"
   size 118703
   provides (browse-url highlight-headers mail-abbrevs mail-extr mail-utils reporter rfc822 rmail-mini sendmail smtpmail)
   requires (xemacs-base)
   type single
))
(tooltalk
  (version "1.03"
   filename "tooltalk-1.03-pkg.tar.gz"
   md5sum "88845fc4ace46737ef34ff8acbe0be84"
   size 9223
   provides ()
   requires ()
   type regular
))
(xemacs-base
  (version "1.07"
   filename "xemacs-base-1.07-pkg.tar.gz"
   md5sum "b1b4c6a7005a2ee33fee0c992e256ceb"
   size 376530
   provides (add-log advice annotations assoc case-table chistory comint-xemacs comint compile edmacro ehelp electric enriched env facemenu helper imenu iso-syntax outline passwd pp regi ring shell skeleton sort thing time-stamp timezone xbm-button xpm-button)
   requires ()
   type regular
))
(xemacs-devel
  (version "1.05"
   filename "xemacs-devel-1.05-pkg.tar.gz"
   md5sum "e12d383c1c30467d8e683504eb00604c"
   size 71065
   provides (docref eldoc elp find-func ielm regexp-opt trace)
   requires (xemacs-base)
   type single
))
(egg-its
  (version "1.02"
   filename "egg-its-1.02-pkg.tar.gz"
   md5sum "e7fa42a1121fc260f459e4dabac061ac"
   size 257713
   provides ()
   requires (leim mule-base xemacs-base)
   type regular
))
(leim
  (version "1.02"
   filename "leim-1.02-pkg.tar.gz"
   md5sum "5697435f2a2d0849b4eebcc22596fdda"
   size 1744281
   provides ()
   requires (mule-base xemacs-base)
   type regular
))
(locale
  (version "1.02"
   filename "locale-1.02-pkg.tar.gz"
   md5sum "9fcdf34a9f3ef696fa143398fb92ce1f"
   size 20633
   provides ()
   requires (mule-base)
   type regular
))
(mule-base
  (version "1.03"
   filename "mule-base-1.03-pkg.tar.gz"
   md5sum "098abbbc88f1702a3a79ec9511475f38"
   size 499505
   provides (canna-leim canna char-table china-util cyril-util isearch-ext japan-util ccl can-n-egg mule-help)
   requires (xemacs-base)
   type regular
))
(skk
  (version "1.02"
   filename "skk-1.02-pkg.tar.gz"
   md5sum "9b836a14af426899f172a3d7ec5bc13a"
   size 1466604
   provides (skk skk-tut)
   requires (viper mule-base xemacs-base)
   type regular
))
(calendar
  (version "1.02"
   filename "calendar-1.02-pkg.tar.gz"
   md5sum "df5bb1a0d8c53dc94debc1b9f6b625d2"
   size 168684
   provides (appt cal-dst cal-french cal-mayan cal-x cal-xemacs calendar diary-ins diary-lib holidays lunar solar)
   requires (xemacs-base)
   type regular
))
(edit-utils
  (version "1.06"
   filename "edit-utils-1.06-pkg.tar.gz"
   md5sum "6b93a01ea3389125c1ea09d035c3eced"
   size 517449
   provides (abbrevlist atomic-extents avoid backup-dir balloon-help big-menubar blink-cursor blink-paren bookmark compare-w completion dabbrev desktop detached-minibuf edit-toolbar fast-lock file-part floating-toolbar flow-ctrl foldout func-menu hippie-exp icomplete id-select info-look iswitchb lazy-lock lazy-shot live-icon man mic-paren paren mode-motion+ outl-mouse page-ext blink-paren paren permanent-buffers recent-files redo reportmail rsz-minibuf saveconfsavehist saveplace scroll-in-place toolbar-utils tree-menu uniquify where-was-i-db)
   requires (xemacs-base)
   type single
))
(forms
  (version "1.04"
   filename "forms-1.04-pkg.tar.gz"
   md5sum "9ed253efbb559b54320cd7fda934bf87"
   size 39809
   provides (forms forms-mode)
   requires ()
   type regular
))
(hm--html-menus
  (version "1.02"
   filename "hm--html-menus-1.02-pkg.tar.gz"
   md5sum "79a9958a08c7e40f8316a9bf8637b3c5"
   size 144731
   provides (adapt hm--date hm--html-configuration hm--html-drag-and-drop hm--html-indentation hm--html-keys hm--html-menu hm--html-mode hm--html-not-standard hm--html html-view tmpl-minor-mode)
   requires ()
   type regular
))
(ispell
  (version "1.03"
   filename "ispell-1.03-pkg.tar.gz"
   md5sum "1119bf349568fb8004b317f1a58fed57"
   size 55723
   provides (ispell)
   requires ()
   type regular
))
(pc
  (version "1.04"
   filename "pc-1.04-pkg.tar.gz"
   md5sum "4933e0ac375397076df583a6e93ba304"
   size 11274
   provides (delbs pc-select pending-del s-region)
   requires (xemacs-base)
   type regular
))
(psgml
  (version "1.0"
   filename "psgml-1.0-pkg.tar.gz"
   md5sum "9d3f8d0909bc2c291955daa6829a9882"
   size 379154
   provides (psgml sgml tempo)
   requires ()
   type regular
))
(sgml
  (version "1.0"
   filename "sgml-1.0-pkg.tar.gz"
   md5sum "e8e744cff0466a77554dfdd477e40cfe"
   size 26814
   provides (sgml linuxdoc-sgml)
   requires (xemacs-base)
   type regular
))
(slider
  (version "1.04"
   filename "slider-1.04-pkg.tar.gz"
   md5sum "b0af411f9c5a2764152cd0e67b11ba8b"
   size 11855
   provides (slider color-selector)
   requires ()
   type regular
))
(speedbar
  (version "1.02"
   filename "speedbar-1.02-pkg.tar.gz"
   md5sum "c32984d7c232e7049d09f85989e2087c"
   size 62424
   provides (speedbar)
   requires (xemacs-base)
   type regular
))
(strokes
  (version "1.0"
   filename "strokes-1.0-pkg.tar.gz"
   md5sum "a15ce2a1dca07b18a64bf332c8946e79"
   size 43588
   provides (strokes)
   requires (text-modes edit-utils mail-lib xemacs-base)
   type regular
))
(text-modes
  (version "1.02"
   filename "text-modes-1.02-pkg.tar.gz"
   md5sum "cc787c8b9945415148ddcdb2590fea10"
   size 102015
   provides (autoinsert crontab-edit filladapt image-mode iso-acc iso-ascii iso-cvt iso-insert iso-swed swedish tabify whitespace-mode winmgr-mode xpm-mode xrdb-mode)
   requires (xemacs-base)
   type regular
))
(time
  (version "1.02"
   filename "time-1.02-pkg.tar.gz"
   md5sum "78751042a5fc579eda833e75fe91b347"
   size 19839
   provides (time)
   requires (xemacs-base)
   type regular
))
(eterm
  (version "1.02"
   filename "eterm-1.02-pkg.tar.gz"
   md5sum "9bc542d4e560e8a00fd555e76079164f"
   size 101730
   provides (eterm)
   requires (xemacs-base)
   type regular
))
(igrep
  (version "1.0"
   filename "igrep-1.0-pkg.tar.gz"
   md5sum "58c6155767fd01765000d829fabbc790"
   size 13899
   provides (igrep)
   requires (efs xemacs-base)
   type regular
))
(ilisp
  (version "1.02"
   filename "ilisp-1.02-pkg.tar.gz"
   md5sum "7fc28390ad9ff212de8da2c3add0dba0"
   size 213183
   provides (ilisp completer)
   requires (xemacs-base)
   type regular
))
(os-utils
  (version "1.03"
   filename "os-utils-1.03-pkg.tar.gz"
   md5sum "579cd4d025e0b20bbf7887250a0ea599"
   size 216645
   provides (archive-mode background crypt crypt++ inf-lisp jka-compr lpr ps-print tar-mode telnet terminal uncompress)
   requires (xemacs-base)
   type single
))
(view-process
  (version "1.02"
   filename "view-process-1.02-pkg.tar.gz"
   md5sum "e0ee0de2d7116477aee75b82b8d929a5"
   size 59824
   provides (view-process-mode)
   requires (xemacs-base)
   type regular
))
(ada
  (version "1.02"
   filename "ada-1.02-pkg.tar.gz"
   md5sum "0788c1e082dd1c01a7014b2d6314c4b5"
   size 54261
   provides (ada-mode ada-stmt)
   requires ()
   type regular
))
(c-support
  (version "1.02"
   filename "c-support-1.02-pkg.tar.gz"
   md5sum "9d6e3bb330ca128c13dea88f321c74c3"
   size 41817
   provides (c-comment-edit cmacexp hideif hideshow)
   requires (cc-mode xemacs-base)
   type regular
))
(cc-mode
  (version "1.03"
   filename "cc-mode-1.03-pkg.tar.gz"
   md5sum "c943e700b28d9c0fe9a870403b913959"
   size 126973
   provides (cc-mode)
   requires (xemacs-base)
   type regular
))
(debug
  (version "1.0"
   filename "debug-1.0-pkg.tar.gz"
   md5sum "b4f7e2d93c10d9b60dd94171d4fe6a7f"
   size 87115
   provides ()
   requires (xemacs-base)
   type regular
))
(ediff
  (version 1.01
   filename "ediff-1.01-pkg.tar.gz"
   md5sum "68431adebf0b9b76b2e347b5cb531378"
   size 241640
   provides (ediff)
   requires (xemacs-base efs pcl-cvs)
   type regular
))
(emerge
  (version "1.0"
   filename "emerge-1.0-pkg.tar.gz"
   md5sum "d5a58538014f89a0210530605eac1817"
   size 60851
   provides (emerge)
   requires ()
   type regular
))
(pcl-cvs
  (version "1.02"
   filename "pcl-cvs-1.02-pkg.tar.gz"
   md5sum "c0d535bee1bb2ed97f209d76a0dad15f"
   size 134504
   provides (pcl-cvs dll elib-node generic-sc)
   requires (xemacs-base)
   type regular
))
(prog-modes
  (version "1.02"
   filename "prog-modes-1.02-pkg.tar.gz"
   md5sum "6f49bfdc7c88ac2182d55c1d4f6c0015"
   size 535033
   provides (autoconf-mode cperl-mode eiffel3 f90 fortran ksh-mode m4-mode makefile perl-mode postscript python-mode rexx-mode simula-mode tcl teco verilog-mod)
   requires (mail-lib xemacs-base)
   type regular
))
(scheme
  (version "1.01"
   filename "scheme-1.01-pkg.tar.gz"
   md5sum "404316bd16588b6a8e743e261a14afb1"
   size 34759
   provides (scheme xscheme cmuscheme)
   requires (xemacs-base)
   type regular
))
(sh-script
  (version "1.03"
   filename "sh-script-1.03-pkg.tar.gz"
   md5sum "1f804e9aa1defc8d5f9d35ffb8f26a34"
   size 33568
   provides (sh-script executable)
   requires (xemacs-base)
   type regular
))
(vc-cc
  (version "1.02"
   filename "vc-cc-1.02-pkg.tar.gz"
   md5sum "d0c0c5a9cdae43971d845858de4aa6bc"
   size 95974
   provides (vc)
   requires (efs xemacs-base)
   type regular
))
(vc
  (version "1.02"
   filename "vc-1.02-pkg.tar.gz"
   md5sum "c2e1a4c4de6b1e9a43dd6b74028fedf5"
   size 74142
   provides (vc)
   requires (efs xemacs-base)
   type regular
))
(vhdl
  (version "1.02"
   filename "vhdl-1.02-pkg.tar.gz"
   md5sum "3c28c8ab7866519131ee0fe8e5b08f6e"
   size 54025
   provides (vhdl-mode)
   requires ()
   type regular
))
(auctex
  (version "1.02"
   filename "auctex-1.02-pkg.tar.gz"
   md5sum "31f3845c0cd3178cbd07478da9a0e70e"
   size 289385
   provides (auc-old bib-cite font-latex latex multi-prompt tex-buf tex-info tex-jp tex-site tex)
   requires (xemacs-base)
   type regular
))
(crisp
  (version "1.02"
   filename "crisp-1.02-pkg.tar.gz"
   md5sum "d1f9279563f71f947d758e5722619aef"
   size 9038
   provides (crisp scroll-lock)
   requires ()
   type regular
))
(edt
  (version "1.02"
   filename "edt-1.02-pkg.tar.gz"
   md5sum "eee41be05231820696aa27fcca84a38c"
   size 45981
   provides (edt)
   requires (xemacs-base)
   type regular
))
(texinfo
  (version "1.04"
   filename "texinfo-1.04-pkg.tar.gz"
   md5sum "6da524a224540b84c7be79cffcc8f966"
   size 113831
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
  (version "1.03"
   filename "tpu-1.03-pkg.tar.gz"
   md5sum "c9801b4a173570fce0598182408ce90c"
   size 57373
   provides (tpu)
   requires ()
   type regular
))
(viper
  (version "1.02"
   filename "viper-1.02-pkg.tar.gz"
   md5sum "99145b4a71030c0fc8c93e24ddc29dd3"
   size 260582
   provides (viper)
   requires (xemacs-base)
   type regular
))
))
