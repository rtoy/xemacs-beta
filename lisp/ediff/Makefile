#
#	Makefile for compiling and installing EDIFF
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
EDIFF    = ediff-init.el ediff-help.el ediff-diff.el ediff-merg.el \
           ediff-wind.el ediff-util.el ediff-mult.el ediff-vers.el \
           ediff-ptch.el ediff.el ediff-hook.el ediff-tbar.el
EDIFFelc = ediff-init.elc ediff-help.elc ediff-diff.elc ediff-merg.elc \
           ediff-wind.elc ediff-util.elc ediff-mult.elc ediff-vers.elc \
           ediff-ptch.elc ediff.elc ediff-hook.elc ediff-tbar.elc


all: hello elc goodbye dvi info

elc: $(EDIFFelc) 

goodbye:
	@echo ""
	@echo ""
	@echo ""
	@echo "    The above compiler warnings were normal -- don't panic!"
	@echo ""
	@echo "    Please report bugs via  M-x ediff-submit-report"
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

ediff-init.elc: ediff-init.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-init.el

ediff-help.elc: ediff-help.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-help.el

ediff-hook.elc: ediff-hook.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-hook.el

ediff-tbar.elc: ediff-tbar.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-tbar.el

ediff-diff.elc: ediff-init.el ediff-diff.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-diff.el

ediff-merg.elc: ediff-init.el ediff-merg.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-merg.el

ediff-mult.elc: ediff-init.el ediff-mult.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-mult.el

ediff-vers.elc: ediff-init.el ediff-vers.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-vers.el

ediff-ptch.elc: ediff-init.el ediff-ptch.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-ptch.el

ediff.elc: ediff-init.el ediff.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff.el

ediff-util.elc: ediff-init.el ediff-util.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-util.el

ediff-wind.elc: ediff-init.el ediff-wind.el
	@echo ""
	$(EMACS) -batch -f batch-byte-compile ediff-wind.el

ediff.dvi: ediff.texi
	@echo ""
	@echo ""
	@echo "Making ediff.dvi --- a Printable Version of the Ediff Manual";
	@echo ""
	$(TeX) ediff.texi
	@echo ""
	@echo "Indexing "
	$(TEXINDEX) ediff.cp
	$(TEXINDEX) ediff.fn
	$(TEXINDEX) ediff.pg
	$(TEXINDEX) ediff.vr
	@echo ""
	@echo "Texing Again";
	$(TeX) ediff.texi
	@echo ""

ediff.info:  ediff.texi
	@echo ""
	@echo ""
	@echo "Making Info Files for the Ediff Manual"
	@echo ""
	$(MAKEINFO) ediff.texi
	@echo "Info done"

dvi: ediff.dvi

info: ediff.info

install: all
	$(INSTALL) -m444 $(EDIFF) $(LISPDIR)
	$(INSTALL) -m644 ediff*.elc $(LISPDIR)
	$(INSTALL) -m644 ediff.dvi $(ETCDIR)
	$(INSTALL) -m444 ediff ediff-? $(INFODIR)
	@echo ""
	@echo "Please move ediff.texi to emacs-distribution-directory/man/"
	@echo ""

clean:
	rm -f ediff*.elc *~ core

distclean: clean
 
realclean: clean
	rm -f *.dvi ediff.info*
	rm -f ediff.aux ediff.cp ediff.cps ediff.fn ediff.fns ediff.ky \
	      ediff.kys ediff.log ediff.pg ediff.pgs ediff.toc ediff.tp \
	      ediff.tps ediff.vr ediff.vrs
