#
#	Makefile for compiling and installing Viper
#

# -------- USER MAY NEED TO CHANGE THESE ---------------------
# To compile under XEmacs, uncomment XEmacs-specific versions of the
# variables VERSION, EMACS, DATADIR and comment out the Emacs-specific
# versions.

VERSION = 19.34
EMACS = emacs-$(VERSION)
#VERSION = 19.14
#EMACS = xemacs-$(VERSION)
PREFIX = /usr/local
DATADIR = $(PREFIX)/share
#DATADIR = $(PREFIX)/lib
LISPDIR = $(DATADIR)/emacs/site-lisp
INFODIR = $(PREFIX)/info
ETCDIR = $(DATADIR)/emacs/$(VERSION)/etc

# --------- YOU PROBABLY DON'T WANT TO CHANGE THESE ----------------
TeX = tex
TEXINDEX = texindex
MAKEINFO = makeinfo
INSTALL = install

# --------- ONLY AUTHORIZED PERSONNEL BEYOND THIS POINT!!! ------------
VIPER = viper.el viper-util.el viper-mous.el viper-ex.el \
        viper-macs.el viper-keym.el
VIPERelc = viper-util.elc viper-mous.elc viper-ex.elc viper-macs.elc \
	   viper-keym.elc viper.elc

PRELOADS = -l viper-util.el -l viper-ex.el -l viper-mous.el \
	   -l viper-macs.el -l viper-keym.el -l viper.el

all: dvi info hello elc goodbye

elc: $(VIPERelc)

goodbye:
	@echo ""
	@echo ""
	@echo ""
	@echo "    The above compiler warnings were normal -- don't panic!"
	@echo ""
	@echo "    Please report bugs via the command  :submitReport"
	@echo ""
	@echo ""
	@echo ""

hello:
	@echo ""
	@echo ""
	@echo "Byte compiling using Emacs 19"
	@echo "Use  make EMACS=xemacs  to compile under XEmacs"
	@echo ""
	@echo ""
	@echo "    The following compiler warnings are normal -- don't panic!"
	@echo ""
	@echo ""
	@echo ""

viper.dvi: viper.texi viper-cmd.texi
	@echo ""
	@echo ""
	@echo "Making viper.dvi --- a Printable Version of the Viper Manual";
	@echo ""
	$(TeX) viper.texi
	@echo ""
	@echo "Indexing "
	$(TEXINDEX) viper.cp
	$(TEXINDEX) viper.fn
	$(TEXINDEX) viper.ky
	$(TEXINDEX) viper.pg
#	$(TEXINDEX) viper.tp
	$(TEXINDEX) viper.vr
	@echo ""
	@echo "Texing Again";
	$(TeX) viper.texi

viper.info: viper.texi viper-cmd.texi
	@echo ""
	@echo ""
	@echo "Making Info Files for the Viper Manual"
	@echo ""
	$(MAKEINFO) viper.texi

viperCard.dvi: viperCard.tex
	@echo ""
	@echo ""
	@echo "Making the Viper Reference Card"
	@echo ""
	$(TeX) viperCard.tex

viper-util.elc: viper-util.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile viper-util.el

viper-ex.elc: viper-ex.el viper-util.el
	@echo ""
	$(EMACS) -batch $(PRELOADS) -f batch-byte-compile viper-ex.el

viper-mous.elc: viper-mous.el viper-util.el
	@echo ""
	$(EMACS) -batch $(PRELOADS) -f batch-byte-compile viper-mous.el

viper-macs.elc: viper-macs.el viper-util.el
	@echo ""
	$(EMACS) -batch $(PRELOADS) -f batch-byte-compile viper-macs.el

viper-keym.elc: viper-keym.el viper-util.el
	@echo ""
	$(EMACS) -batch $(PRELOADS) -f batch-byte-compile viper-keym.el

viper.elc: viper.el viper-util.el
	@echo ""
	$(EMACS) -batch $(PRELOADS) -f batch-byte-compile viper.el


dvi: viper.dvi viperCard.dvi

info: viper.info

install: all
	$(INSTALL) -m444 $(VIPER) $(LISPDIR)
	$(INSTALL) -m644 viper*.elc $(LISPDIR)
	$(INSTALL) -m444 viper viper-? $(INFODIR)
	$(INSTALL) -m644 viper.dvi viperCard.dvi $(ETCDIR)
	@echo ""
	@echo "Please move viper.texi and viper-cmd.texi to"
	@echo "            emacs-distribution-directory/man/"
	@echo ""

clean:
	rm -f *.elc *~ core

distclean: clean

realclean: clean
	rm -f *.dvi viper.info*
	rm -f viper.aux viper.cp viper.cps viper.fn viper.fns viper.ky \
	      viper.kys viper.log viper.pg viper.pgs viper.toc viper.tp \
	      viper.tps viper.vr viper.vrs
