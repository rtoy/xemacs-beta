# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

EMACS=../../src/xemacs

srcdir = .
top_srcdir = ..

INFOOBJ = skk.info skk.info-1 skk.info-2 skk.info-3 skk.info-4 skk.info-5 skk.info-6

INSTALL = /usr/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644

prefix = /usr/local
infodir = ${prefix}/info

TEXI2DVI = texi2dvi

all: ../../info/skk.info
	@if test -f skk.info; then \
		mv skk.info* ../../info; \
	fi

../../info/skk.info: skk.texi gpl.texi
	$(EMACS) -batch -vanilla \
	-eval '(setq buffer-file-coding-system-for-read (quote euc-jp))' \
	skk.texi \
	-eval '(unless (featurep (quote mule)) (kill-emacs 0))' \
	-l texinfmt -f texinfo-format-buffer \
	-eval '(setq buffer-file-coding-system (quote iso-2022-7))' \
	-eval '(save-buffer 0)'

info: all

#all: skk.dvi
#	$(TEXI2DVI) skk.texi

install:
	$(top_srcdir)/mkinstalldirs $(infodir)
	@list="$(INFOOBJ)"; for p in $$list; do \
	  if test -f $$p; then \
	    echo " $(INSTALL_DATA) $$p $(infodir)/"; \
             $(INSTALL_DATA) $$p $(infodir)/; \
          else :; fi; \
        done

.PHONY: mostlyclean clean distclean realclean extraclean
mostlyclean:
	rm -f skk.ky skk.kys skk.log skk.pg skk.toc skk.fn skk.fns \
		skk.vr skk.vrs skk.tp skk.cp skk.cps skk.aux
clean: mostlyclean
	rm -f *.dvi *.ps make.out core
distclean: clean
realclean: clean
extraclean: clean
	-rm -f *~ \#*
