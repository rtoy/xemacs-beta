#
#	Makefile for compiling and installing EDIFF
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
EDIFF    = ediff-init.el ediff-help.el ediff-diff.el ediff-merg.el \
           ediff-wind.el ediff-util.el ediff-mult.el ediff-vers.el \
           ediff-ptch.el ediff.el ediff-hook.el ediff-tbar.el
EDIFFelc = $(COMPDIR)ediff-init.elc \
	   $(COMPDIR)ediff-help.elc \
	   $(COMPDIR)ediff-diff.elc \
	   $(COMPDIR)ediff-merg.elc \
           $(COMPDIR)ediff-wind.elc \
	   $(COMPDIR)ediff-util.elc \
	   $(COMPDIR)ediff-mult.elc \
	   $(COMPDIR)ediff-vers.elc \
           $(COMPDIR)ediff-ptch.elc \
	   $(COMPDIR)ediff.elc \
	   $(COMPDIR)ediff-hook.elc \
	   $(COMPDIR)ediff-tbar.elc

COMPILE_ARGS = -batch -f batch-byte-compile


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
	@echo "Byte compiling using Emacs"
	@echo "Use  make EMACS=xemacs  to compile under XEmacs"
	@echo ""
	@echo ""
	@echo "    The following compiler warnings are normal -- don't panic!"
	@echo ""
	@echo ""
	@echo ""

$(COMPDIR)ediff-init.elc: ediff-init.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-init.el

$(COMPDIR)ediff-help.elc: ediff-help.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-help.el

$(COMPDIR)ediff-hook.elc: ediff-hook.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-hook.el

$(COMPDIR)ediff-tbar.elc: ediff-tbar.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-tbar.el

$(COMPDIR)ediff-diff.elc: ediff-init.el ediff-diff.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-diff.el

$(COMPDIR)ediff-merg.elc: ediff-init.el ediff-merg.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-merg.el

$(COMPDIR)ediff-mult.elc: ediff-init.el ediff-mult.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-mult.el

$(COMPDIR)ediff-vers.elc: ediff-init.el ediff-vers.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-vers.el

$(COMPDIR)ediff-ptch.elc: ediff-init.el ediff-ptch.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-ptch.el

$(COMPDIR)ediff.elc: ediff-init.el ediff.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff.el

$(COMPDIR)ediff-util.elc: ediff-init.el ediff-util.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-util.el

$(COMPDIR)ediff-wind.elc: ediff-init.el ediff-wind.el
	@echo ""
	$(EMACS) $(COMPILE_ARGS) ediff-wind.el

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

ediff:  ediff.texi
	@echo ""
	@echo ""
	@echo "Making Info Files for the Ediff Manual"
	@echo ""
	$(MAKEINFO) ediff.texi
	@echo "Info done"

dvi: ediff.dvi

info: ediff

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
	rm -f *.dvi ../info/ediff ../info/ediff-?
	rm -f ediff.aux ediff.cp ediff.cps ediff.fn ediff.fns ediff.ky \
	      ediff.kys ediff.log ediff.pg ediff.pgs ediff.toc ediff.tp \
	      ediff.tps ediff.vr ediff.vrs
