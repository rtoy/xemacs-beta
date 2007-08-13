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

.SUFFIXES: .info .texi .dvi

# Subdirectories to make recursively.
SUBDIR = xemacs lispref new-users-guide internals tm

../info/%.info : %.texi
	makeinfo -o $@ $<

# hyperbole and oo-browser manuals broken - do not TeX properly
srcs = ange-ftp cc-mode cl custom dired ediff external-widget forms gnus \
       hyperbole ilisp info ispell mailcrypt message mh-e oo-browser \
       pcl-cvs psgml psgml-api rmail standards supercite term \
       termcap texinfo vhdl-mode viper vm w3 widget xemacs-faq

info = $(srcs:%=../info/%.info)
dvi  = $(srcs:%=%.dvi)

all  : info
info : $(info)
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done

xemacs: FORCE
	cd $@ && $(MAKE) $(MFLAGS)
lispref: FORCE
	cd $@ && $(MAKE) $(MFLAGS)
new-users-guide: FORCE
	cd $@ && $(MAKE) $(MFLAGS)
internals: FORCE
	cd $@ && $(MAKE) $(MFLAGS)
tm: FORCE
	cd $@ && $(MAKE) $(MFLAGS)
FORCE :

PERL = perl
TEXI2DVI = texi2dvi

gnus.dvi : gnus.texi
	$(PERL) -n -e 'print unless (/\@iflatex/ .. /\@end iflatex/)' gnus.texi > gnus.tmptexi
	$(TEXI2DVI) gnus.tmptexi
	rm -f gnus.tmptexi

texinfo.dvi: texinfo.texi
	$(TEXI2DVI) $<
.texi.dvi:
	$(TEXI2DVI) $<

dvi : $(dvi) FORCE
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done

mostlyclean:
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done
	rm -f *.toc *.aux *.log \
	      *.cp *.cps *.fn *.fns *.ky *.kys *.pg *.pgs *.vr *.vrs *.tp *.tps
clean: mostlyclean
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done
	rm -f *.o core *.dvi
distclean: clean
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done
realclean: distclean
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done
extraclean: distclean
	for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done
	-rm -f *~ \#*
