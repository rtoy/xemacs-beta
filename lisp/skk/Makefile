# Generated automatically from Makefile.in by configure.
# Makefile for compiling SKK 10.x
# $Id: Makefile,v 1.1 1997/12/02 08:48:36 steve Exp $
# 
SHELL = /bin/sh


srcdir = .
top_srcdir = ..

INSTALL = /usr/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644

prefix = /usr/local
datadir = ${prefix}/share

EMACS = /usr/src/XEmacs/xemacs-20.3-pre1/src/xemacs
ELISPDIR = $(datadir)/emacs/site-lisp

SOURCES = stack-m.el queue-m.el string.el \
	  skk-foreword.el skk.el skk-num.el skk-gadget.el skk-auto.el \
          skk-comp.el skk-vip.el skk-viper.el skk-kakasi.el skk-kcode.el \
          skk-server.el skk-tree.el skk-tut.el skk-isearch.el skk-menu.el \
          skk-attr.el skk-leim.el

COMPILED = stack-m.elc queue-m.elc string.elc \
           skk-foreword.elc skk.elc skk-num.elc skk-gadget.elc skk-auto.elc \
           skk-comp.elc skk-vip.elc skk-viper.elc skk-kakasi.elc skk-kcode.elc \
           skk-server.elc skk-tree.elc skk-tut.elc skk-isearch.elc skk-menu.elc \
           skk-attr.elc skk-leim.elc

all: skk-vars.elc auto-autoloads.elc $(COMPILED)

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -vanilla -f batch-byte-compile $<

skk-vars.el:
	rm -f skk-vars.el
	$(EMACS) -batch -q -no-site-file -l ./make-vars.el -f make-skk-vars

install: skk-vars.elc $(COMPILED)
	$(top_srcdir)/mkinstalldirs $(ELISPDIR)
	@list="$(SOURCES) skk-vars.el $(COMPILED) skk-vars.elc"; for p in $$list; do \
	  if test -f $$p; then \
	    echo " $(INSTALL_DATA) $$p $(ELISPDIR)/"; \
             $(INSTALL_DATA) $$p $(ELISPDIR)/; \
          else :; fi; \
        done

TAGS: $(SOURCES)
	etags $(SOURCES)

.PHONY: clean
clean:
	-rm -f *~ $(COMPILED) skk-vars.el skk-vars.elc
stack-m.elc: stack-m.el
queue-m.elc: queue-m.el
string.elc: string.el queue-m.el
skk.elc: string.el queue-m.el skk-foreword.el
skk-comp.elc: skk-comp.el skk-foreword.el skk.el stack-m.el
skk-auto.elc: skk-auto.el skk-foreword.el skk.el
skk-gadget.elc: skk-gadget.el skk-foreword.el skk.el
skk-kakasi.elc: skk-kakasi.el skk-foreword.el skk.el
skk-kcode.elc: skk-kcode.el skk-foreword.el skk.el
skk-num.elc: skk-num.el skk-foreword.el skk.el
skk-server.elc: skk-server.el skk-foreword.el skk.el
skk-tree.elc: skk-tree.el skk-foreword.el skk.el
skk-tut.elc: skk-tut.el skk-foreword.el skk.el
skk-vip.elc: skk-vip.el skk-foreword.el skk.el
skk-isearch.elc: skk-isearch.el skk-foreword.el skk.el
skk-menu.elc: skk-menu.el skk.el
skk-attr.elc: skk-attr.el skk.el
skk-vars.el: stack-m.el queue-m.el string.el \
             skk-foreword.el skk.el skk-num.el skk-gadget.el skk-auto.el \
             skk-comp.el skk-vip.el skk-viper.el skk-kakasi.el skk-kcode.el \
             skk-server.el skk-tree.el skk-tut.el skk-isearch.el skk-menu.el
