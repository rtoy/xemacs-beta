# what emacs is called on your system
EMACS = emacs

# How to make a directory
# need a -p if you want to make the parents!
MKDIR = mkdir

# Various other stuff used
RM    = rm -f
CP    = cp

# where the Info file should go
INFODIR = ../info

# where the w3 lisp files should go
LISPDIR = $$HOME/lisp
URLDIR  = ../url

# Change this to be where your .emacs file is stored
DOTEMACS      = $$HOME/.emacs

# Where the Emacs-W3 configuration files should be installed
DOTDIR = $$HOME/.w3/

# Change this to be how to convert texinfo files into info files
# examples:
#	$(EMACS) -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile
SHELL=/bin/sh
DEPS = -l ./docomp.el -l ./w3-vars.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# files that contain variables and macros that everything else depends on
CORE = docomp.el

SOURCES = \
	w3.el w3-draw.el w3-e19.el w3-mule.el w3-parse.el w3-print.el	\
	w3-vars.el w3-xemac.el w3-style.el w3-about.el w3-hot.el	\
	w3-toolbar.el font.el						\
	w3-annotat.el w3-auto.el w3-forms.el images.el w3-imap.el	\
	w3-emulate.el w3-menu.el w3-keyword.el w3-mouse.el widget.el	\
	widget-edit.el w3-widget.el w3-speak.el w3-prefs.el

OBJECTS = $(SOURCES:.el=.elc)

DISTFILES     = Makefile ChangeLog $(SOURCES) w3.txi docomp.el	\
		clean-cache default.css w3-sysdp.el

.SUFFIXES: .elc .el .el,v

.el.elc:
	@echo Compiling $<
	@URLDIR=$(URLDIR) $(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $<

w3:	$(URLDIR) docomp.el $(OBJECTS)
	@echo Build of w3 complete...

$(URLDIR):
	@echo "Please install the URL package in $(URLDIR) first."
	@/bin/false

all:	w3.info w3 emacs

install: all
	@echo Installing in $(LISPDIR)
	@if [ ! -d $(LISPDIR) ] ; then $(MKDIR) $(LISPDIR) ; fi
	@(cd $(LISPDIR) ; $(RM) $(SOURCES) $(OBJECTS) w3-sysdp.el*)
	@$(CP) $(SOURCES) $(OBJECTS) THIS* $(LISPDIR)
	@if [ ! -d $(INFODIR) ] ; then $(MKDIR) $(INFODIR) ; fi
	@$(CP) w3.info* $(INFODIR)
	@$(CP) default.css $(DOTDIR)/stylesheet

emacs:	
	@echo Adding w3 setup to $(DOTEMACS)
	$(EMACS) -batch -q -l docomp.el -f hack-dot-emacs $(DOTEMACS) \
		 $(LISPDIR) $(URLDIR)

clean:
	$(RM) $(OBJECTS)

w3.info:	w3.txi
	@$(MAKEINFO) w3.txi

w3.dvi:		w3.txi
	tex w3.txi
	texindex w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr
	tex w3.txi
	$(RM) 	w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr 	\
		w3.cps w3.fns w3.kys w3.pgs w3.tps w3.vrs	\
		w3.log w3.toc w3.aux

