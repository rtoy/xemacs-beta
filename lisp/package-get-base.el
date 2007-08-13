(setq package-get-base
'((cookie
  (version "1.02"
   filename "cookie-1.02-pkg.tar.gz"
   md5sum "95e110ddf03202403e45950d2cb53b11"
   size 33774
   provides (cookie1 yow)
   requires (xemacs-base games-misc)
   type regular
))
(mine
  (version "1.02"
   filename "mine-1.02-pkg.tar.gz"
   md5sum "054f2eefb015b2878ba37f6473a47dec"
   size 67166
   provides (xmine)
   requires (xemacs-base)
   type regular
))
(misc-games
  (version "1.01"
   filename "misc-games-1.01-pkg.tar.gz"
   md5sum "a86cf625a2700a9b0e7f71139df011a7"
   size 172861
   provides (decipher gomoku hanoi life tetris)
   requires (xemacs-base)
   type single
))
(Sun
  (version "1.02"
   filename "Sun-1.02-pkg.tar.gz"
   md5sum "22d61bc22784e1509a3b74e38393b828"
   size 63707
   provides (sccs eos-browser eos-common eos-debugger eos-debugger eos-editor eos-init eos-load eos-menubar eos-toolbar sunpro)
   requires (cc-mode xemacs-base)
   type regular
))
(apel
  (version "1.01"
   filename "apel-1.01-pkg.tar.gz"
   md5sum "6de339aeae8a36be7e954a80bb18bc3c"
   size 34546
   provides (atype emu-20 emu-e19 emu-x20 emu-xemacs emu file-detect filename install mule-caesar path-util richtext std11-parse std11 tinyrich)
   requires (xemacs-base)
   type regular
))
(edebug
  (version "1.01"
   filename "edebug-1.01-pkg.tar.gz"
   md5sum "7ebf5f57dc47236f71d38a387d3cdccc"
   size 117881
   provides (edebug cl-read cust-print eval-reg cl-specs)
   requires (xemacs-base)
   type regular
))
(efs
  (version "1.01"
   filename "efs-1.01-pkg.tar.gz"
   md5sum "d2ad5d764d60345e4530dc679715ec26"
   size 506222
   provides (efs dired efs-auto)
   requires (xemacs-base)
   type regular
))
(mail-lib
  (version "1.02"
   filename "mail-lib-1.02-pkg.tar.gz"
   md5sum "2d02a1b69146925c13ae4e2d759b1fb2"
   size 118488
   provides (browse-url highlight-headers mail-abbrevs mail-extr mail-utils reporter rfc822 rmail-mini sendmail smtpmail)
   requires (xemacs-base)
   type single
))
(tooltalk
  (version "1.02"
   filename "tooltalk-1.02-pkg.tar.gz"
   md5sum "3fdf1c49c43dd852d2c8895e074c39ba"
   size 9174
   provides ()
   type regular
))
(xemacs-base
  (version "1.05"
   filename "xemacs-base-1.05-pkg.tar.gz"
   md5sum "cbfeb7910ba552a47a23cde220156179"
   size 394189
   provides (add-log advice annotations assoc case-table chistory comint-xemacs comint compile edmacro ehelp electric enriched env facemenu helper imenu iso-syntax outline passwd pp regi ring shell skeleton sort thing time-stamp timezone xbm-button xpm-button)
   type regular
))
(xemacs-devel
  (version "1.03"
   filename "xemacs-devel-1.03-pkg.tar.gz"
   md5sum "e806e74988a89ac66314a72b6f0801a8"
   size 64241
   provides (docref eldoc elp find-func ielm regexp-opt trace)
   requires (xemacs-base)
   type single
))
(egg-its
  (version "1.01"
   filename "egg-its-1.01-pkg.tar.gz"
   md5sum "ba5e6aca993a2795a1cc3bedfbc7dcc0"
   size 316515
   provides ()
   requires (xemacs-base mule-base)
   type regular
))
(leim
  (version "1.01"
   filename "leim-1.01-pkg.tar.gz"
   md5sum "5c467eb0476c9da9cc6f671da1884685"
   size 1743472
   provides ()
   requires (xemacs-base mule-base)
   type regular
))
(locale
  (version "1.01"
   filename "locale-1.01-pkg.tar.gz"
   md5sum "2f390e23929941a6aa6e3478a3f10ed7"
   size 20628
   provides ()
   requires (xemacs-base mule-base)
   type regular
))
(mule-base
  (version "1.02"
   filename "mule-base-1.02-pkg.tar.gz"
   md5sum "bf8754e52b309e130242bf5036de9a3e"
   size 499508
   provides (canna-leim canna char-table china-util cyril-util isearch-ext japan-util ccl can-n-egg mule-help)
   requires (xemacs-base)
   type regular
))
(skk
  (version "1.01"
   filename "skk-1.01-pkg.tar.gz"
   md5sum "9aa608eccfe7da7371ea7924c3523545"
   size 1464809
   provides (skk skk-tut)
   requires (xemacs-base mule-base)
   type regular
))
(calendar
  (version "1.01"
   filename "calendar-1.01-pkg.tar.gz"
   md5sum "bc0908ae5798f662b9cc5cb52325c21e"
   size 168660
   provides (appt cal-dst cal-french cal-mayan cal-x cal-xemacs calendar diary-ins diary-lib holidays lunar solar)
   requires (xemacs-base)
   type regular
))
(edit-utils
  (version "1.03"
   filename "edit-utils-1.03-pkg.tar.gz"
   md5sum "af2cbb68fce602132f6153bb3814b59a"
   size 498040
   provides (abbrevlist atomic-extents avoid backup-dir balloon-help big-menubar blink-cursor blink-paren bookmark compare-w completion dabbrev desktop detached-minibuf edit-toolbar fast-lock file-part floating-toolbar flow-ctrl foldout func-menu hippie-exp icomplete id-select info-look iswitchb lazy-lock lazy-shot live-icon man mic-paren paren mode-motion+ outl-mouse page-ext blink-paren paren recent-files redo reportmail rsz-minibuf saveconfsavehist saveplace scroll-in-place toolbar-utils tree-menu uniquify where-was-i-db)
   requires (xemacs-base)
   type single
))
(forms
  (version "1.03"
   filename "forms-1.03-pkg.tar.gz"
   md5sum "8f8a2d7118102b39e06e6a8766b3efe6"
   size 39802
   provides (forms forms-mode)
   type regular
))
(hm--html-menus
  (version "1.01"
   filename "hm--html-menus-1.01-pkg.tar.gz"
   md5sum "23e7e4a7e783e70c9d4d68ecf609884f"
   size 145889
   provides (adapt hm--date hm--html-configuration hm--html-drag-and-drop hm--html-indentation hm--html-keys hm--html-menu hm--html-mode hm--html-not-standard hm--html html-view tmpl-minor-mode)
   type regular
))
(ispell
  (version "1.02"
   filename "ispell-1.02-pkg.tar.gz"
   md5sum "2b9140b4b8757b64ccfc59755df5a7ab"
   size 55709
   provides (ispell)
   type regular
))
(pc
  (version "1.03"
   filename "pc-1.03-pkg.tar.gz"
   md5sum "97eef269303c73dbe5d19c0f2ce6ed4d"
   size 11247
   provides (delbs pc-select pending-del s-region)
   requires (xemacs-base)
   type regular
))
(slider
  (version "1.03"
   filename "slider-1.03-pkg.tar.gz"
   md5sum "b7d54a810b03f394cafcbc4a8e6e5b44"
   size 11845
   provides (slider color-selector)
   type regular
))
(speedbar
  (version "1.01"
   filename "speedbar-1.01-pkg.tar.gz"
   md5sum "3fe68d6e9ae69036d35189a3ddebcc2e"
   size 62397
   provides (speedbar)
   requires (xemacs-base)
   type regular
))
(text-modes
  (version "1.01"
   filename "text-modes-1.01-pkg.tar.gz"
   md5sum "6a4a4536bd9397f0af0b5d224f0176d3"
   size 84693
   provides (autoinsert crontab-edit filladapt image-mode iso-acc iso-ascii iso-cvt iso-insert iso-swed swedish tabify whitespace-mode winmgr-mode xpm-mode xrdb-mode)
   requires (xemacs-base)
   type regular
))
(time
  (version "1.01"
   filename "time-1.01-pkg.tar.gz"
   md5sum "d20af53a3a5f2731032e38783db2b9e9"
   size 19819
   provides (time)
   requires (xemacs-base)
   type regular
))
(ada
  (version "1.01"
   filename "ada-1.01-pkg.tar.gz"
   md5sum "3dfd652bb8e2aae934f34f1f015f4a57"
   size 55141
   provides (ada-mode ada-stmt)
   type regular
))
(c-support
  (version "1.01"
   filename "c-support-1.01-pkg.tar.gz"
   md5sum "eedf97a19d02454a0ac4021a213cd566"
   size 41788
   provides (c-comment-edit cmacexp hideif hideshow)
   requires (cc-mode xemacs-base)
   type regular
))
(cc-mode
  (version "1.02"
   filename "cc-mode-1.02-pkg.tar.gz"
   md5sum "a6c3aab6e2c0168d981a5dacbab429f3"
   size 128544
   provides (cc-mode)
   requires (xemacs-base)
   type regular
))
(ediff
  (version "1.01"
   filename "ediff-1.01-pkg.tar.gz"
   md5sum "68431adebf0b9b76b2e347b5cb531378"
   size 241640
   provides (ediff)
   requires (xemacs-base efs pcl-cvs)
   type regular
))
(pcl-cvs
  (version "1.01"
   filename "pcl-cvs-1.01-pkg.tar.gz"
   md5sum "4fd7beb2f7a61c6bb8be8ebea5bfa03b"
   size 134459
   provides (pcl-cvs dll elib-node generic-sc)
   requires (xemacs-base)
   type regular
))
(prog-modes
  (version "1.01"
   filename "prog-modes-1.01-pkg.tar.gz"
   md5sum "b0af0691a51202322a7d7d800a2d5543"
   size 519423
   provides (autoconf-mode cperl-mode eiffel3 f90 fortran ksh-mode m4-mode makefile perl-mode postscript python-mode rexx-mode simula-mode tcl teco verilog-mod)
   requires (xemacs-base mail-lib )
   type regular
))
(sh-script
  (version "1.02"
   filename "sh-script-1.02-pkg.tar.gz"
   md5sum "14493ca0455148f4d62b583d3c1562a9"
   size 33556
   provides (sh-script executable)
   requires (xemacs-base)
   type regular
))
(vc
  (version "1.01"
   filename "vc-cc-1.01-pkg.tar.gz"
   md5sum "2e549b7fbebc82e7166a6c225344e46c"
   size 95934
   provides (vc)
   requires (xemacs-base efs)
   type regular
))
(vc
  (version "1.01"
   filename "vc-1.01-pkg.tar.gz"
   md5sum "3b40953a1a171aed5f3f4e58963be229"
   size 74214
   provides (vc)
   requires (xemacs-base efs)
   type regular
))
(vhdl
  (version "1.01"
   filename "vhdl-1.01-pkg.tar.gz"
   md5sum "cefebce133fae1544bcd4a487493bde4"
   size 54036
   provides (vhdl-mode)
   type regular
))
(auctex
  (version "1.01"
   filename "auctex-1.01-pkg.tar.gz"
   md5sum "f720a7e3cedae6276079adc2329b335b"
   size 289282
   provides (auc-old bib-cite font-latex latex multi-prompt tex-buf tex-info tex-jp tex-site tex)
   requires (xemacs-base)
   type regular
))
(crisp
  (version "1.01"
   filename "crisp-1.01-pkg.tar.gz"
   md5sum "a984e6bc7446c14fc33d94d5ced95f57"
   size 9029
   provides (crisp scroll-lock)
   type regular
))
(edt
  (version "1.01"
   filename "edt-1.01-pkg.tar.gz"
   md5sum "7a9f9182ef96250906ec531ee8b81f46"
   size 45966
   provides (edt)
   requires (xemacs-base)
   type regular
))
(texinfo
  (version "1.03"
   filename "texinfo-1.03-pkg.tar.gz"
   md5sum "391537d64b8e03a4233d124c0a92a6c9"
   size 113799
   provides (makeinfo tex-mode texinfmt texinfo texnfo-tex texnfo-upd)
   requires (xemacs-base)
   type regular
))
(tpu
  (version "1.01"
   filename "tpu-1.01-pkg.tar.gz"
   md5sum "c76aa33c56bb7555c5976b42fdf7b391"
   size 64845
   provides (tpu)
   type regular
))
(viper
  (version "1.01"
   filename "viper-1.01-pkg.tar.gz"
   md5sum "c8b1c0c664baeefe4bfccac077d83ad3"
   size 252762
   provides (viper)
   type regular
))
))
