;;; sun-eos-toolbar.el --- Implements the EOS toolbar interface

;; Copyright (C) Sun Microsystems, Inc.

;; Maintainer:	Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>
;; Author:      Eduardo Pelegri-Llopart <eduardo.pelegri-llopart@Eng.Sun.COM>

;; Keywords:	SPARCworks EOS Era on SPARCworks toolbar

;;; Commentary:

;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;;; Code:

(defvar eos::toolbar-icon-directory
  (file-name-as-directory (locate-data-directory "eos")))

(defvar eos::toolbar-run-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"X	c #0000FFFF0000\",
\"+	c #000077770000\",
\"@	c #000044440000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                 .......    \",
\"                            \",
\"     .....                  \",
\"     .X+@.       .......    \",
\"     .X+@.                  \",
\"  ......@....               \",
\"   .XXX++++.     .......    \",
\"    .XX++@.                 \",
\"     .@+@.                  \",
\"      .@.        .......    \",
\"       .                    \",
\"                            \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-run.xbm" eos::toolbar-icon-directory)))
  "A Run icon pair.")

(defvar eos::toolbar-type-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\"X	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"     XX XX                  \",
\"     XX XX                  \",
\"   XXXX XXXX  XX XX         \",
\"  XX XX XX XX XX XX  XXX  X \",
\"  XX XX XX XX  XXX   X X X  \",
\"  XX XX XX XX   X    XXXX   \",
\"  XX XX XX XX  XXX     XXXX \",
\"  XX XX XX XX XX XX   X X X \",
\"   XXX   XXX  XX XX  X  XXX \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-type.xbm" eos::toolbar-icon-directory)))
  "A Type-at icon pair.")


(defvar eos::toolbar-stop-at-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #4B4B4B4B4B4B\",
\"X	c #FFFFFFFFFFFF\",
\"o	c #AFAFAFAFAFAF\",
\"O	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .XXXXXXXX.         \",
\"        .XoOOOOOOoX.        \",
\"       .XoOOOOOOOOoX.       \",
\"      .XoOOOOOOOOOOoX.      \",
\"     .XoOOOOOOOOOOOOoX.     \",
\"    .XoOOOOOOOOXOOOOOoX.    \",
\"    .XOOOOOOOOOXXOOOOOX.    \",
\"    .XOOOOXXXXXXXXOOOOX.    \",
\"    .XOOOOXXXXXXXXXOOOX.    \",
\"    .XOOOOXXXXXXXXOOOOX.    \",
\"    .XOOOOOOOOOXXOOOOOX.    \",
\"    .XOOOOOOOOOXOOOOOOX.    \",
\"    .XoOOOOOOOOOOOOOOoX.    \",
\"     .XoOOOOOOOOOOOOOX.     \",
\"      .XoOOOOOOOOOOoX.      \",
\"       .XoOOOOOOOOoX.       \",
\"        .XoOOOOOOoX.        \",
\"         .XXXXXXXX.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-stop.xbm" eos::toolbar-icon-directory)))
  "A Stop At icon pair.")

(defvar eos::toolbar-clear-at-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #4B4B4B4B4B4B\",
\"X	c #FFFFFFFFFFFF\",
\"o	c #AFAFAFAFAFAF\",
\"O	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .XXXXXXXX.         \",
\"        .XoOOOOOOoX.        \",
\"       .XoOOOOOOOOoX.       \",
\"      .XoOOOOOOOOOOoX.      \",
\"     .XoOOOOOOOOOOOOoX.     \",
\"    .XoOOOXXOOOOXXOOOoX.    \",
\"    .XOOOOOXXOOXXOOOOOX.    \",
\"    .XOOOOOOXXXXOOOOOOX.    \",
\"    .XOOOOOOOXXOOOOOOOX.    \",
\"    .XOOOOOOXXXXOOOOOOX.    \",
\"    .XOOOOOXXOOXXOOOOOX.    \",
\"    .XOOOOXXOOOOXXOOOOX.    \",
\"    .XoOOOXOOOOOOXOOOoX.    \",
\"     .XoOOOOOOOOOOOOoX.     \",
\"      .XoOOOOOOOOOOoX.      \",
\"       .XoOOOOOOOOoX.       \",
\"        .XoOOOOOOoX.        \",
\"         .XXXXXXXX.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-clear-at.xbm" eos::toolbar-icon-directory)))
  "A Clear At icon pair.")

(defvar eos::toolbar-stop-in-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #4B4B4B4B4B4B\",
\"X	c #FFFFFFFFFFFF\",
\"o	c #AFAFAFAFAFAF\",
\"O	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"          ........          \",
\"         .XXXXXXXX.         \",
\"        .XoOOOOOOoX.        \",
\"       .XoOOOOOOOOoX.       \",
\"      .XoOOOOOOOOOOoX.      \",
\"     .XoOOOOOOOOOOOOoX.     \",
\"    .XoOOOOOOOOOXOXOOoX.    \",
\"    .XOOOXXXXOOXOOOXOOX.    \",
\"    .XOOOXOOOOOXOOOXOOX.    \",
\"    .XOOOXOOOOOXOOOXOOX.    \",
\"    .XOOOXXXOOXOOOOOXOX.    \",
\"    .XOOOXOOOOOXOOOXOOX.    \",
\"    .XOOOXOOOOOXOOOXOOX.    \",
\"    .XoOOXOOOOOXOOOXOoX.    \",
\"     .XoOOOOOOOOXOXOoX.     \",
\"      .XoOOOOOOOOOOoX.      \",
\"       .XoOOOOOOOOoX.       \",
\"        .XoOOOOOOoX.        \",
\"         .XXXXXXXX.         \",
\"          ........          \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-stop-in.xbm" eos::toolbar-icon-directory)))
    "A Stop in icon pair.")

(defvar eos::toolbar-step-into-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"O	c #0000FFFF0000\",
\"+	c #000077770000\",
\"@	c #000044440000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....   .......       \",
\"     .OOOO.                 \",
\"    .O++++.                 \",
\"    .O+....   ........      \",
\"    .O+.                    \",
\"    .O+. .                  \",
\"    .O+. ..                 \",
\"    .O+. .O.                \",
\"    .O+...O@.    .......    \",
\"    .O++OOO+@.              \",
\"    .O+++++++@.             \",
\"     .++++++@.   .......    \",
\"      ....O@.               \",
\"         .O.                \",
\"         ..      .......    \",
\"         .                  \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-step-into.xbm" eos::toolbar-icon-directory)))
  "A Step Into icon pair.")

(defvar eos::toolbar-step-up-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"O	c #0000FFFF0000\",
\"+	c #000077770000\",
\"@	c #000044440000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         .                  \",
\"         ..      .......    \",
\"         .O.                \",
\"      ....O@.               \",
\"     .++++++@.   .......    \",
\"    .O+++++++@.             \",
\"    .O++OOO+@.              \",
\"    .O+...O@.    .......    \",
\"    .O+. .O.                \",
\"    .O+. ..                 \",
\"    .O+. .                  \",
\"    .O+.                    \",
\"    .O+....   ........      \",
\"    .O++++.                 \",
\"     .OOOO.                 \",
\"      .....   .......       \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-step-up.xbm" eos::toolbar-icon-directory)))
  "A Step up icon pair.")

(defvar eos::toolbar-step-over-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"X	c #0000FFFF0000\",
\"+	c #000077770000\",
\"@	c #000044440000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....                 \",
\"     .XXXX.      .......    \",
\"    .X++++.                 \",
\"    .X+....                 \",
\"    .X+.         .......    \",
\"    .X+. .                  \",
\"    .X+. ..                 \",
\"    .X+. .X.     .......    \",
\"    .X+...X@.               \",
\"    .X++XXX+@.              \",
\"    .X+++++++@.  .......    \",
\"     .++++++@.              \",
\"      ....X@.               \",
\"         .X.     .......    \",
\"         ..                 \",
\"         .                  \",
\"                 .......    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-step-over.xbm" eos::toolbar-icon-directory)))
  "A Step Over icon pair.")

(defvar eos::toolbar-evaluate-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         ....               \",
\"         .. ..  ......      \",
\"         .. ..  ......      \",
\"         .. ..              \",
\"         .. ..  ......      \",
\"         .. ..  ......      \",
\"         ....               \",
\"         ..                 \",
\"         ..                 \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-evaluate.xbm" eos::toolbar-icon-directory)))
  "A Evaluate icon pair.")

(defvar eos::toolbar-evaluate-star-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 2 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\"X	c #000000000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"     XX XX                  \",
\"      XXX                   \",
\"    XXXXXXX                 \",
\"      XXX   XXXX            \",
\"     XX XX  XX XX XXXXXX    \",
\"            XX XX XXXXXX    \",
\"            XX XX           \",
\"            XX XX XXXXXX    \",
\"            XX XX XXXXXX    \",
\"            XXXX            \",
\"            XX              \",
\"            XX              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-evaluate-star.xbm" eos::toolbar-icon-directory)))
  "A Evaluate Star icon pair.")

(defvar eos::toolbar-fix-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #7D7D7D7D7D7D\",
\"X	c #000000000000\",
\"o	c #FFFFFFFF0000\",
\"O	c #FFFF99990000\",
\"+	c #FFFFCCCC3333\",
\"@	c #CCCC9999FFFF\",
\"#	c #99996666CCCC\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         .XX.               \",
\"         XoOXX.             \",
\"        .Xo+OOXXX.          \",
\"        Xo++++OOOXXX        \",
\"       .Xo+++++++OOOX.      \",
\"       Xo++++++OOOXX.       \",
\"      .Xo++++OOXXX.         \",
\"      Xo++OOOXX.            \",
\"     .XoOOOXXXXXXXXXXXX     \",
\"     XoOXXX@@@@@@@@@@@X     \",
\"     XXX##############X     \",
\"      X@##############X     \",
\"      XXXXXXXXXXXXXXXXX     \",
\"      X@@@@X     X@@@@X     \",
\"      X@###X     X@###X     \",
\"      X@###X     X@###X     \",
\"      X@###X     X@###X     \",
\"      X@###X     X@###X     \",
\"      XXXXXX     XXXXXX     \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-fix.xbm" eos::toolbar-icon-directory)))
  "A Fix icon pair.")

(defvar eos::toolbar-run2-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"X	c #0000FFFF0000\",
\"o	c #000077770000\",
\"O	c #000044440000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                .           \",
\"                ..          \",
\"                .X.         \",
\"     ............XX.        \",
\"     .XXXXXXXXXXXXoX.       \",
\"     .XoooooooooooooX.      \",
\"     .Xooooooooooooooo.     \",
\"     .XoooooooooooooO.      \",
\"     .oOOOOOOOOOOOoO.       \",
\"     ............OO.        \",
\"                .O.         \",
\"                ..          \",
\"                .           \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-run2.xbm" eos::toolbar-icon-directory)))
  "A Run icon pair.")

(defvar eos::toolbar-cont-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 6 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"O	c #0000FFFF0000\",
\"+	c #000077770000\",
\"@	c #000044440000\",
\"o	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      .....   .......       \",
\"     .OOOO.                 \",
\"    .O++++.                 \",
\"    .O+....   ........      \",
\"    .O+.                    \",
\"    .O+. .                  \",
\"    .O+. ..                 \",
\"    .O+. .O.                \",
\"    .O+...O@.     ..        \",
\"    .O++OOO+@.   .oo.       \",
\"    .O+++++++@. .oooo.      \",
\"     .++++++@.  .oooo.      \",
\"      ....O@.    .oo.       \",
\"         .O.      ..        \",
\"         ..                 \",
\"         .                  \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-cont.xbm" eos::toolbar-icon-directory)))
  "A Cont icon pair.")	  
		      

(defvar eos::toolbar-up-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"X	c #CCCC9999FFFF\",
\"o	c #99996666CCCC\",
\"O	c #FFFFFFFF0000\",
\"+	c #FFFFCCCC3333\",
\"@	c #0000FFFF0000\",
\"#	c #000077770000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                  .         \",
\"                 ...        \",
\"      ........  .....       \",
\"      .XXXXXX. .......      \",
\"      .Xooooo.   ...        \",
\"      .Xooooo.   ...        \",
\"      .Xooooo.   ...        \",
\"      .Xooooo.   ...        \",
\"      .O+++++.   ...        \",
\"      .O+++++.   ...        \",
\"      .O+++++.              \",
\"      .O+++++.              \",
\"      .O+++++.              \",
\"      .@#####.              \",
\"      .@#####.              \",
\"      .@#####.              \",
\"      .@#####.              \",
\"      .@#####.              \",
\"      ........              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-up.xbm" eos::toolbar-icon-directory)))
  "A Up icon pair.")

(defvar eos::toolbar-down-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	s FgColor c #000000000000\",
\"X	c #CCCC9999FFFF\",
\"o	c #99996666CCCC\",
\"O	c #FFFFFFFF0000\",
\"+	c #FFFFCCCC3333\",
\"@	c #0000FFFF0000\",
\"#	c #000077770000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"      ........              \",
\"      .XXXXXX.              \",
\"      .Xooooo.              \",
\"      .Xooooo.              \",
\"      .Xooooo.              \",
\"      .Xooooo.              \",
\"      .O+++++.    ...       \",
\"      .O+++++.    ...       \",
\"      .O+++++.    ...       \",
\"      .O+++++.    ...       \",
\"      .O+++++.    ...       \",
\"      .@#####.    ...       \",
\"      .@#####.  .......     \",
\"      .@#####.   .....      \",
\"      .@#####.    ...       \",
\"      .@#####.     .        \",
\"      ........              \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-down.xbm" eos::toolbar-icon-directory)))
  "A Down icon pair.")

(defvar eos::toolbar-build-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 8 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\".	c #000000000000\",
\"X	c #CCCC9999FFFF\",
\"o	c #99996666CCCC\",
\"O	c #FFFFFFFF0000\",
\"+	c #FFFFCCCC3333\",
\"@	c #FFFF99990000\",
\"#	c #FFFF66666666\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                ......      \",
\"                .XXXX.      \",
\"                .Xooo.      \",
\"                .Xooo.      \",
\"                .Xooo.      \",
\"                .Xooo.      \",
\"           .    .Xooo.      \",
\"          .O.   .Xooo.      \",
\"         .O+@.  .Xooo.      \",
\"        .O+++@. .Xooo.      \",
\"       .O+++++@..Xooo.      \",
\"      .O+++++++@.Xooo.      \",
\"     .O+++..............    \",
\"    .O@@@@.            .    \",
\"    ....... ###########.    \",
\"          . ###########.    \",
\"          . ###########.    \",
\"          ..............    \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-build.xbm" eos::toolbar-icon-directory)))
  "A Build icon pair.")

(defvar eos::toolbar-dismiss-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * file[] = {
\"28 28 5 1\",
\" 	c #C8C8C8C8C8C8 s backgroundToolBarColor\",
\"X	c #4B4B4B4B4B4B\",
\". 	c #FFFFFFFFFFFF\",
\"o	c #AFAFAFAFAFAF\",
\"O	c #FFFF00000000\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"       X            X       \",
\"       XX          XX       \",
\"        XX        XX        \",
\"         XX      XX         \",
\"          XX    XX          \",
\"           XX  XX           \",
\"            XXXX            \",
\"             XX             \",
\"            XXXX            \",
\"           XX  XX           \",
\"          XX    XX          \",
\"         XX      XX         \",
\"        XX        XX        \",
\"       XX          XX       \",
\"       X            X       \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"                            \"};")
    (toolbar-make-button-list
     (expand-file-name "eos-dismiss.xbm" eos::toolbar-icon-directory)))
  "A Dismiss icon pair.")

(defvar eos::toolbar-intro-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * info[] = {
\"28 28 2 1\",
\"X	c Gray75 s backgroundToolBarColor\",
\"o	c #000077770000\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXoooooooXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXooooooXXXXXXXXXXXX\",
\"XXXXXXXXXoooooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXoooooooXXXXXXXXXXX\",
\"XXXXXXXXXoooooooooXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};")
    (toolbar-make-button-list
     (expand-file-name "eos-intro.xbm" eos::toolbar-icon-directory)))
  "An intro icon pair.")

(defvar eos::toolbar-introD-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * info[] = {
\"28 28 2 1\",
\"X	c Gray75 s backgroundToolBarColor\",
\"o	c #000077770000\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXoooooooXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXooooooXXXXXXXXXXXX\",
\"XXXXXXXXXoooooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXoooXXXXXXoooooXXXXXXXXXXXX\",
\"XXoXXoXXXXXoooooXXXXXXXXXXXX\",
\"XXoXXoXXXXoooooooXXXXXXXXXXX\",
\"XXoXXoXXXoooooooooXXXXXXXXXX\",
\"XXoXXoXXXXXXXXXXXXXXXXXXXXXX\",
\"XXoXXoXXXXXXXXXXXXXXXXXXXXXX\",
\"XXoooXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};")
    (toolbar-make-button-list
     (expand-file-name "eos-introD.xbm" eos::toolbar-icon-directory)))
  "An intro icon pair.")

(defvar eos::toolbar-introDB-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * info[] = {
\"28 28 2 1\",
\"X	c Gray75 s backgroundToolBarColor\",
\"o	c #000077770000\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXoooooooXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXooooooXXXXXXXXXXXX\",
\"XXXXXXXXXoooooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXoooXXXXXXoooooXXXXXXoooXXX\",
\"XXoXXoXXXXXoooooXXXXXXoXXoXX\",
\"XXoXXoXXXXoooooooXXXXXoXXoXX\",
\"XXoXXoXXXoooooooooXXXXoooXXX\",
\"XXoXXoXXXXXXXXXXXXXXXXoXXoXX\",
\"XXoXXoXXXXXXXXXXXXXXXXoXXoXX\",
\"XXoooXXXXXXXXXXXXXXXXXoooXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};")
    (toolbar-make-button-list
     (expand-file-name "eos-introDB.xbm" eos::toolbar-icon-directory)))
  "An intro icon pair.")

(defvar eos::toolbar-introB-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * info[] = {
\"28 28 2 1\",
\"X	c Gray75 s backgroundToolBarColor\",
\"o	c #000077770000\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXoooooooXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXoXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXooooooXXXXXXXXXXXX\",
\"XXXXXXXXXoooooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXXXXXXX\",
\"XXXXXXXXXXXoooooXXXXXXoooXXX\",
\"XXXXXXXXXXXoooooXXXXXXoXXoXX\",
\"XXXXXXXXXXoooooooXXXXXoXXoXX\",
\"XXXXXXXXXoooooooooXXXXoooXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXoXXoXX\",
\"XXXXXXXXXXXXXXXXXXXXXXoXXoXX\",
\"XXXXXXXXXXXXXXXXXXXXXXoooXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};")
    (toolbar-make-button-list
     (expand-file-name "eos-introB.xbm" eos::toolbar-icon-directory)))
  "An intro icon pair.")


(defvar eos::debugger-toolbar
      '(
	[eos::toolbar-introD-icon
	 eos::sw-intro
	 t
	 "Show Introduction to Eos"]
	[eos::toolbar-stop-at-icon
	 eos::stop-at
	 eos::current-debugger-clique-id
	 "stop at: Stop at selected position"]
	[eos::toolbar-stop-in-icon
	 eos::stop-in
	 eos::current-debugger-clique-id
	 "stop in: Stop in function whose name is selected"]
	[eos::toolbar-clear-at-icon
	 eos::clear-at
	 eos::current-debugger-clique-id
	 "clear at: Clear at selected position"]
	[eos::toolbar-run-icon
	 eos::run
	 eos::current-debugger-clique-id
	 "run: Run current program"]
	[eos::toolbar-evaluate-icon
	 eos::print
	 eos::current-debugger-clique-id
	 "print: Evaluate selected expression; shows in separate XEmacs frame"]
	[eos::toolbar-evaluate-star-icon
	 eos::print*
	 eos::current-debugger-clique-id
	 "print *: Evaluate selected expression as a pointer; shows in separate XEmacs frame"]
	[eos::toolbar-up-icon
	 eos::up
	 eos::current-debugger-clique-id
	 "up: move in stack towards \"cooler\" (less recently visited) frames"]
	[eos::toolbar-down-icon
	 eos::down
	 eos::current-debugger-clique-id
	 "down: move in stack towards \"warmer\" (more recently visited) frames)"]
	[eos::toolbar-cont-icon
	 eos::cont
	 eos::current-debugger-clique-id
	 "cont: Continue current program"]
	[eos::toolbar-step-over-icon
	 eos::next
	 eos::current-debugger-clique-id
	 "next: Step over subprogram calls"]
	[eos::toolbar-step-into-icon
	 eos::step
	 eos::current-debugger-clique-id
	 "step: Step into subprogram calls)"]
	[eos::toolbar-step-up-icon
	 eos::step-up
	 eos::current-debugger-clique-id
	 "step up: Step up from subprogram calls)"]
	[eos::toolbar-build-icon
	 eos::build
	 eos::current-debugger-clique-id
	 "make: Build target"]
	[eos::toolbar-fix-icon
	 eos::fix
	 eos::current-debugger-clique-id
	 "fix: Fix file"]
	[eos::toolbar-type-icon
	 eos::type
	 (or (and (eq eos::dbx-or-debugger 'debugger)
		  eos::current-debugger-clique-id)
	     (and (eq eos::dbx-or-debugger 'dbx)
		  (eos::dbx-process)
		  (eq (process-status (eos::dbx-process)) 'run)))
	 "Type a Dbx command"]
	))

(defvar eos::debugger-sbrowser-toolbar
      '(
	[eos::toolbar-introDB-icon
	 eos::sw-intro
	 t
	 "Show Introduction to Eos"]
	[eos::toolbar-stop-at-icon
	 eos::stop-at
	 eos::current-debugger-clique-id
	 "stop at: Stop at selected position"]
	[eos::toolbar-stop-in-icon
	 eos::stop-in
	 eos::current-debugger-clique-id
	 "stop in: Stop in function whose name is selected"]
	[eos::toolbar-clear-at-icon
	 eos::clear-at
	 eos::current-debugger-clique-id
	 "clear at: Clear at selected position"]
	[eos::toolbar-run-icon
	 eos::run
	 eos::current-debugger-clique-id
	 "run: Run current program"]
	[eos::toolbar-evaluate-icon
	 eos::print
	 eos::current-debugger-clique-id
	 "print: Evaluate selected expression; shows in separate XEmacs frame"]
	[eos::toolbar-evaluate-star-icon
	 eos::print*
	 eos::current-debugger-clique-id
	 "print *: Evaluate selected expression as a pointer; shows in separate XEmacs frame"]
	[eos::toolbar-up-icon
	 eos::up
	 eos::current-debugger-clique-id
	 "up: move in stack towards \"cooler\" (less recently visited) frames"]
	[eos::toolbar-down-icon
	 eos::down
	 eos::current-debugger-clique-id
	 "down: move in stack towards \"warmer\" (more recently visited) frames)"]
	[eos::toolbar-cont-icon
	 eos::cont
	 eos::current-debugger-clique-id
	 "cont: Continue current program"]
	[eos::toolbar-step-over-icon
	 eos::next
	 eos::current-debugger-clique-id
	 "next: Step over subprogram calls"]
	[eos::toolbar-step-into-icon
	 eos::step
	 eos::current-debugger-clique-id
	 "step: Step into subprogram calls)"]
	[eos::toolbar-step-up-icon
	 eos::step-up
	 eos::current-debugger-clique-id
	 "step up: Step up from subprogram calls)"]
	[eos::toolbar-build-icon
	 eos::build
	 eos::current-debugger-clique-id
	 "make: Build target"]
	[eos::toolbar-fix-icon
	 eos::fix
	 eos::current-debugger-clique-id
	 "fix: Fix file"]
	[eos::toolbar-type-icon
	 eos::type
	 (or (and (eq eos::dbx-or-debugger 'debugger)
		  eos::current-debugger-clique-id)
	     (and (eq eos::dbx-or-debugger 'dbx)
		  (eos::dbx-process)
		  (eq (process-status (eos::dbx-process)) 'run)))
	 "Type a Dbx command"]
	))

(defvar eos::sbrowser-toolbar
      '([eos::toolbar-introB-icon
	 eos::sw-intro
	 t
	 "Show Introduction to Eos"]
	))

(defvar eos::print-toolbar
      '(
	[eos::toolbar-intro-icon
	 eos::sw-intro
	 t
	 "Show Introduction to Eos"]
	[eos::toolbar-evaluate-icon
	 eos::print
	 eos::current-debugger-clique-id
	 "print: Evaluate selected expression; shows in separate XEmacs frame"]
	[eos::toolbar-evaluate-star-icon
	 eos::print*
	 eos::current-debugger-clique-id
	 "print *: Evaluate selected expression as a pointer; shows in separate XEmacs frame"]
	[eos::toolbar-cont-icon
	 eos::cont-and-dismiss
	 eos::current-debugger-clique-id
	 "cont & dismiss: Continue current program and dismiss this frame"]
	[eos::toolbar-step-over-icon
	 eos::next-and-dismiss
	 eos::current-debugger-clique-id
	 "next  & dismiss: Step over subprogram calls and dismiss this frame"]
	[eos::toolbar-step-into-icon
	 eos::step-and-dismiss
	 eos::current-debugger-clique-id
	 "step & dismiss: Step into subprogram calls and dismiss this frame)"]
	[eos::toolbar-dismiss-icon
	 eos::dismiss-print-frame
	 t
	 "dismiss (make invisible) this print frame"]
	))

(defun eos::toolbar-position ()
  (let ((pos (default-toolbar-position)))
    (cond ((eq pos 'top) top-toolbar)
	  ((eq pos 'bottom) bottom-toolbar)
	  ((eq pos 'left) left-toolbar)
	  ((eq pos 'right) right-toolbar)
	  (t top-toolbar))))

(provide 'eos-toolbar)

;;; sun-eos-toolbar.el ends here
