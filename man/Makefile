#  Makefile for man subdirectory in XEmacs
#   Copyright (C) 1995 Board of Trustees, University of Illinois
#   Copyright (C) 1994, 1995 Sun Microsystems.

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

# Avoid trouble on systems where the "SHELL" variable might be
# inherited from the environment.
SHELL = /bin/sh

EMACS = ../src/xemacs
EMACSFLAGS = -batch -q -no-site-file 

# NOTE:  You *must* have makeinfo-1.68 or later to rebuild the
# info tree.
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi

.SUFFIXES:
.SUFFIXES: .info .texi .dvi

RECURSIVE_MAKE = $(MAKE) $(MFLAGS) MAKEINFO='$(MAKEINFO)' TEXI2DVI='$(TEXI2DVI)'

# Subdirectories to make recursively.
SUBDIR = xemacs lispref new-users-guide internals gnats skk

../info/%.info : %.texi
	-$(MAKEINFO) -o $@ $<

srcs = cl custom external-widget forms \
	info \
	standards term \
	termcap texinfo widget xemacs-faq

info = $(srcs:%=../info/%.info)
dvi  = $(srcs:%=%.dvi)

special = # ../info/w3.info ../info/vm.info ../info/texinfo.info

all  : info
info : $(info) $(special)
	-for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done

xemacs: FRC.xemacs
	-cd $@ && $(RECURSIVE_MAKE)
lispref: FRC.lispref
	-cd $@ && $(RECURSIVE_MAKE)
new-users-guide: FRC.new-users-guide
	-cd $@ && $(RECURSIVE_MAKE)
internals: FRC.internals
	-cd $@ && $(RECURSIVE_MAKE)
tm: FRC.tm
	-cd $@ && $(RECURSIVE_MAKE)
gnats: FRC.gnats
	-cd $@ && $(RECURSIVE_MAKE)
FRC.xemacs FRC.lispref FRC.new-users-guide FRC.internals FRC.tm FRC.gnats:

../info/w3.info : w3.texi
	-$(MAKEINFO) w3.texi -o ../info/w3.info

../info/texinfo.info : texinfo.texi
	-$(MAKEINFO) texinfo.texi -o ../info/texinfo.info


../info/vm.info : vm.texi
	-$(EMACS) $(EMACSFLAGS) -insert vm.texi -l texinfmt \
		-f texinfo-format-buffer -f save-buffer
	-mv vm.info* ../info

texinfo.dvi: texinfo.texi
	$(TEXI2DVI) $<
.texi.dvi:
	$(TEXI2DVI) $<

.PHONY: dvi
dvi : $(dvi)
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done

.PHONY: mostlyclean clean distclean realclean extraclean
mostlyclean:
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done
	rm -f *.toc *.aux *.log \
	      *.cp *.cps *.fn *.fns *.ky *.kys *.pg *.pgs *.vr *.vrs *.tp *.tps
clean: mostlyclean
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done
	rm -f *.o core *.dvi
distclean: clean
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done
realclean: distclean
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done
extraclean: distclean
	for d in $(SUBDIR) ; do (cd $${d} && $(RECURSIVE_MAKE) $@) ; done
	-rm -f *~ \#*
