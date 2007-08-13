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

# The japanese texinfo files requires the very latest 20.0 xemacs
EMACS = xemacs
NAMES = tm tm-edit tm-view gnus-mime tm-mh-e
NAMES_EN = $(NAMES:%=%-en) tm-vm-en
NAMES_JA = $(NAMES:%=%-ja)
NAMES_ALL = $(NAMES_EN) $(NAMES_JA)
#NAMES_ALL = $(NAMES_EN)
INFOS = $(NAMES_ALL:%=../../info/%.info)
DVIS  = $(NAMES_EN:%=%.dvi)

TEXI2DVI = texi2dvi

# List of all the texinfo files in the manual:

all : info
info : $(INFOS)

../../info/%-en.info : %-en.texi
	makeinfo -o $@ $<

../../info/%-ja.info : %-ja.texi
	$(EMACS) -batch -q -no-site-file $< -eval '(cd "../../info")' \
	    -l texinfmt -f texinfo-format-buffer \
	    -eval '(setq file-coding-system (quote iso-2022-7))' \
	    -eval '(save-buffer 0)'

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
