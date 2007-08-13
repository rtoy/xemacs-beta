#
#	Makefile for compiling and installing Viper
#

# -------- USER MAY NEED TO CHANGE THESE ---------------------
# To compile under XEmacs, uncomment XEmacs-specific versions of the
# variables VERSION, EMACS, DATADIR and comment out the Emacs-specific
# versions.

VERSION = 20.0
EMACS = emacs-$(VERSION)
#VERSION = 20.3
#EMACS = xemacs-$(VERSION)
PREFIX = /usr/local
DATADIR = $(PREFIX)/share
#DATADIR = $(PREFIX)/lib
LISPDIR = $(DATADIR)/emacs/site-lisp
INFODIR = $(PREFIX)/info
ETCDIR = $(DATADIR)/emacs/$(VERSION)/etc
COMPDIR =

# --------- YOU PROBABLY DON'T WANT TO CHANGE THESE ----------------
TeX = tex
TEXINDEX = texindex
MAKEINFO = makeinfo
INSTALL = install

# --------- ONLY AUTHORIZED PERSONNEL BEYOND THIS POINT!!! ------------
VIPER = viper.el viper-cmd.el viper-util.el viper-mous.el viper-ex.el \
        viper-macs.el viper-keym.el viper-init.el
VIPERelc = $(COMPDIR)viper-util.elc \
	   $(COMPDIR)viper-mous.elc \
	   $(COMPDIR)viper-ex.elc \
	   $(COMPDIR)viper-macs.elc \
	   $(COMPDIR)viper-keym.elc \
	   $(COMPDIR)viper.elc \
	   $(COMPDIR)viper-cmd.elc \
	   $(COMPDIR)viper-init.elc

COMPILE_ARGS = -batch -f batch-byte-compile

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
	@echo "Byte compiling using Emacs"
	@echo "Use  make EMACS=xemacs  to compile under XEmacs"
	@echo ""
	@echo ""
	@echo "    The following compiler warnings are normal -- don't panic!"
	@echo ""
	@echo ""
	@echo ""

viper.dvi: viper.texi
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

viper: viper.texi 
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

${COMPDIR}viper-init.elc: viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-init.el

$(COMPDIR)viper-util.elc: viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-util.el

$(COMPDIR)viper-ex.elc: viper-ex.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-ex.el

$(COMPDIR)viper-mous.elc: viper-mous.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-mous.el

$(COMPDIR)viper-macs.elc: viper-macs.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-macs.el

$(COMPDIR)viper-keym.elc: viper-keym.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-keym.el

$(COMPDIR)viper.elc: viper.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper.el

$(COMPDIR)viper-cmd.elc: viper-cmd.el viper.el viper-util.el viper-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) viper-cmd.el


dvi: viper.dvi viperCard.dvi

info: viper

install: all
	$(INSTALL) -m444 $(VIPER) $(LISPDIR)
	$(INSTALL) -m644 viper*.elc $(LISPDIR)
	$(INSTALL) -m444 viper viper-? $(INFODIR)
	$(INSTALL) -m644 viper.dvi viperCard.dvi $(ETCDIR)
	@echo ""
	@echo "Please move viper.texi to emacs-distribution-directory/man/"
	@echo ""

clean:
	rm -f *.elc *~ core

distclean: clean
 
realclean: clean
	rm -f *.dvi ../info/viper ../info/viper-?
	rm -f viper.aux viper.cp viper.cps viper.fn viper.fns viper.ky \
	      viper.kys viper.log viper.pg viper.pgs viper.toc viper.tp \
	      viper.tps viper.vr viper.vrs
