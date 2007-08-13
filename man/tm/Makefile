# Makefile for TM manual

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

NAMES = tm_en gnus-mime_en tm-mh-e_en
INFOS = $(NAMES:%=../../info/%.info)
DVIS  = $(NAMES:%=%.dvi)

TEXI2DVI = texi2dvi

# List of all the texinfo files in the manual:

srcs = tm-edit-en.texi   tm-ew-d=en.texi    tm-ew=en.texi      tm-util=en.texi \
       tm-view-a=en.texi tm-view-cb=en.texi tm-view-ch=en.texi tm-view-cs=en.texi \
       tm-view-m=en.texi tm-view-p=en.texi  tm-view=en.texi    tm-en.texi

all : info
info : $(INFOS)

../../info/tm_en.info : $(srcs)

../../info/%.info : %.texi
	makeinfo -o $@ $<

dvi: $(DVIS)
.texi.dvi :
	$(TEXI2DVI) $<

mostlyclean:
	rm -f *.toc *.aux *.oaux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys
clean: mostlyclean
	rm -f *.dvi *.ps make.out core
distclean:  clean
realclean:  clean
extraclean: clean
	-rm -f *~ \#*
