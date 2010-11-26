#!/bin/sh

# Copyright (C) 1998 Oliver Graf <ograf@fga.de>

# This file is part of XEmacs.

# XEmacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

# Synched up with: Not in FSF.

TEMPDIR=/tmp

cat README > $TEMPDIR/DropTest.txt

cat > $TEMPDIR/DropTest.html <<EOF
<HTML>
<HEAD>
<TITLE>DropTest Page</TITLE>
</HEAD>
<BODY>
<H1>DropTest</H1>
Just a Test!
</BODY>
</HTML>
EOF

cat > $TEMPDIR/DropTest.tex <<EOF
\documentclass{article}

\begin{document}
This is a DropTest!
\end{document}
EOF

cat > $TEMPDIR/DropTest.xpm <<EOF
/* XPM */
static char *test[] = {
/* width height num_colors chars_per_pixel */
"    76    50       19            1",
/* colors */
". c #ffffff",
"# c #000000",
"a c #737373",
"b c #aeb2c3",
"c c #9397a5",
"d c #dcdee5",
"e c #5d6069",
"f c #949494",
"g c #adadad",
"h c #212121",
"i c #bdbdbd",
"j c #dedede",
"k c #636363",
"l c #ff0000",
"m c #00ff00",
"n c #0000ff",
"o c #ffff00",
"p c #ff00ff",
"q c #424242",
/* pixels */
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbb#eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebbbbbbbbbbbbb",
"bbbbbbbbbbbbbb#jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjqqdbbbbbbbbbbbbb",
"bbbbbbbbbbbbbb#jjggggggggggggggggggggggggggggggggggggggggggjqqdbbbbbbbbbbbbb",
"bbbbbbbbbbbbbb#jjggggggggggggggggggggggggggggggggggj...........jiibbbbbbbbbb",
"bbbbbbbbbbbbbb#jjggjjjjjjjjjjjjjjjjjjjjjjjjjjjjj...iiiiiipp.ppii....bbbbbbbb",
"bbbbbbbbbbbbbb#jjggjjgggggggggggggqqjgggggggg...iiiiiiiip..pppqiiiii..cbbbbb",
"bbbbbbbbbbbbbb#jjggjjgggggggggggggqqjggggggg.iiiiooooooi.pp.ppqiiiii##cbbbbb",
"bbbbbbbbbbbbbb#jjggjjgggggggggggggqqjgggggg.iiiiooooolliiqqqqqiiiiii##cbbbbb",
"bbbbbbbbbbbbbb#qqggjjgggjjjjjjjjggqqjgggg..iiiiiooooolliiiiiiiiiiii#cccbbbbb",
"bbbbbbb..........qgjjgggjqqqqqqqggqqjggg.iiiiiiiioolliiif#####iiii#cccbbbbbb",
"bbbbbbbqqqqqqqqii.qjjgggggggggggggqqjggg.iiillliiiiiiiii#aaaiiii##ccbbbbbbbb",
"bbbbbbbbbcccccciiiqjjgggggggggggggqqjggg.iillll#iii..iii#jjjiiiiaajjjjbbbbbb",
"bbbbbbbbbbbbbbeaaaqjjgggggggggggggqqjggg.iillll#i..nnnniiiiiiiiiiiiiqqcbbbbb",
"bbbbbbbbbbbbbb#..iqjjqqqqqqqqqqqqqqqjqqq.iii###iinnnnnnnimmnmmnmiiiiqqcbbbbb",
"bbbbbbbbbbbbbbeaaaaqqiiiiiiiiiiiiiiiiiiikiiiiiiiinnnnnn#innmnnmiiiiq##cbbbbb",
"bbbbbbbbbbbbbb.iiiiqqffffffffffffffffffffkkhiiiiiiinnnn#immnmmqiiif#cccbbbbb",
"bbbbbbbbbbjjjjjjjjjjjjjjjjfffffffffffffffffhh##iiiiiiiiiinnnqqiihh#cccbbbbbb",
"bbbbbbbbbjiiiiiiiiiiiiiiiifffffffffffffffffff####ggiiiiiiiiiiiahccccbbbbbbbb",
"bbbbbbbjjijjjjfjjjjffjjjjfggqfffffffffffffffffff###############cccbbbbbbbbbb",
"bbbbbbbjjijgggkjjggkkjgggkggqkffffff................aaaaaaaaqqbcbbbbbbbbbbbb",
"bbbbbbbjjijgggkjjggkkjgggkggqkaaffff..#####..####...#fffffffqqdbbbbbbbbbbbbb",
"bbbbbbbjjijgggkjjggkkjgggkggqkaaffff.#aaaaa..##aaaa.#aafffffqqdbbbbbbbbbbbbb",
"bbbbbbbjjijgggkjjggkkjgggkggqkaaffff.#aaaff..##ffff.#aafffffqqdbbbbbbbbbbbbb",
"bbbbbbbjjiaaaagaaaaggaaaaiggqkaaffff##aafff..##ffff##aafffffqqdbbbbbbbbbbbbb",
"bbbbbbbjjiiiiiiiiiiiiiiiiiggqkaafffffaaafff..##fffffaaafffffqqdbbbbbbbbbbbbb",
"bbbbbbbjjijiijijjijiijiijiggqkaafffffffffff..##f...........dqqdbbbbbbbbbbbbb",
"bbbbbbbjjjjjjjjjjjjjjjjjjjggqhqqqqqqqqqqqqq..##q.###...##..#qqdbbbbbbbbbbbbb",
"bbbbbbbjjjjjjjjjjjjjjjjjjjggqebbddddddddddd..##d.##d...#d..#dddbbbbbbbbbbbbb",
"bbbbbbbjjijiijijjijiijiijiggqeccbbbbbbbbbbb..##c###c...#c###ccbbbbbbbbbbbbbb",
"bbbbbbbjjiiiiiiiiiiiiiiiiiggqeccbbbbbbbbbbb..##cbccc...#cbbcccbbbbbbbbbbbbbb",
"bbbbbbbjjgiggigiigiggiggigffqeccbbbbbbbbbbb..##cbbbb...#cbb.......bbbbbbbbbb",
"bbbbbbbjjgggggggggggggggggffqeccbbbbbbbbbbb..##cbbbb...#cbb.##.#..#bbbbbbbbb",
"bbbbbbbjjfgffgfggfgffgffgfaaqeccbbbbbbbbbbb..##cbbbb...#cbb###.####cbbbbbbbb",
"bbbbbbbjjfffffffffffffffffffqeccbbbbbbbb........#bbb...#cbbbbb.#ccccbbbbbbbb",
"bbbbbbbjjffffffffffffffffaffqeccbbbbbbbb#########cc.....#bbbbb.#ccbbbbbbbbbb",
"bbbbbbbjjafaafaffafaafaafaffqeccbbbbbbbbbcccccccccc######ccb....##bbbbbbbbbb",
"bbbbbbbjjaaaaaaaaaaaaaaaaaaaqeccbbbbbbbbbbbbbbbbbbbbcccccccbbb####cbbbbbbbbb",
"bbbbbbbbbbaaaaaaaaaaaaaaaahheeccbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbccccbbbbbbbbb",
"bbbbbbbbbbbqqqqqqqqqqqqqqqeeecccbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbeeeeeeeeeeeeeeeccbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbccccccccccccccccbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
};
EOF
