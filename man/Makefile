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

EMACS = xemacs
EMACSFLAGS = -batch -q -no-site-file 

# NOTE:  You *must* have a patched makeinfo-1.67 or later to rebuild the
# info tree.
MAKEINFO = makeinfo

.SUFFIXES: .info .texi .dvi

# Subdirectories to make recursively.
SUBDIR = xemacs lispref new-users-guide internals tm auctex gnats

../info/%.info : %.texi
	-$(MAKEINFO) -o $@ $<

srcs = cc-mode cl custom ediff efs external-widget forms gnus \
	hm--html-mode \
	hyperbole ilisp info ispell mailcrypt message mh-e oo-browser \
	pcl-cvs ph psgml psgml-api rmail standards supercite term \
	termcap texinfo vhdl-mode viper vm w3 widget xemacs-faq

info = $(srcs:%=../info/%.info)
dvi  = $(srcs:%=%.dvi)

special = # ../info/w3.info ../info/vm.info ../info/texinfo.info

all  : info
info : $(info) $(special)
	-for d in $(SUBDIR) ; do (cd $${d} && $(MAKE) $(MFLAGS) $@) ; done

xemacs: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
lispref: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
new-users-guide: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
internals: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
tm: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
auctex: FORCE
	-cd $@ && $(MAKE) $(MFLAGS)
FORCE :

../info/w3.info : w3.texi
	-$(MAKEINFO) w3.texi -o ../info/w3.info

../info/texinfo.info : texinfo.texi
	-$(MAKEINFO) texinfo.texi -o ../info/texinfo.info


../info/vm.info : vm.texi
	-$(EMACS) $(EMACSFLAGS) -insert vm.texi -l texinfmt \
		-f texinfo-format-buffer -f save-buffer
	-mv vm.info* ../info

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

.PHONY: mostlyclean clean distclean realclean extraclean
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
