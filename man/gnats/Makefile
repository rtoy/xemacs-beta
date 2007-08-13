# Makefile for the XEmacs Internals Manual.

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

VERSION=1.0
NAME=gnats

# Uncomment this line for permuted index.
# permuted_index = 1 

# List of all the texinfo files in the manual:

srcs = categ.texi fields.texi flowchart.txt gnats.texi p-admin.texi \
	p-inst.texi p-usage.texi s-usage.texi send-pr.texi states.text \
	version.texi

gnats_srcs = flowchart.txt gnats.texi p-admin.texi p-inst.texi \
	p-usage.texi version.texi

send_pr_srcs = categ.texi fields.texi s-usage.texi send-pr.texi states.texi \
	version.texi

all : info
info : ../../info/$(NAME).info ../../info/send-pr.info

../../info/$(NAME).info: ${gnats_srcs}
	${MAKEINFO} $(NAME).texi -o ../../info/${NAME}.info

../../info/send-pr.info: ${seend_pr_srcs}
	${MAKEINFO} send-pr.texi -o ../../info/send-pr.info

dvi : $(NAME).dvi send-pr.dvi

$(NAME).dvi: $(srcs) index.texi
	texi2dvi gnats.texi
	texi2dvi send-pr.texi

.PHONY: mostlyclean clean distclean realclean extraclean
mostlyclean:
	rm -f *.toc *.aux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys
clean: mostlyclean
	rm -f *.dvi *.ps make.out core index.texi
distclean:  clean
realclean:  clean
extraclean: clean
	-rm -f *~ \#*
