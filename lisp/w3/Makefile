# where the w3 lisp files should go
prefix = /usr/local
infodir = $(prefix)/info
datadir = $(prefix)/share
lispdir = $(datadir)/emacs/site-lisp
confdir = $(datadir)/emacs/w3

# what emacs is called on your system
EMACS = emacs

# How to install a file
INSTALL = install

# Various other stuff used
RM    = rm -f
CP    = cp

# Change this to be how to convert texinfo files into info files
# examples:
#	$(EMACS) -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile

DEPS = -l ./docomp.el -l ./w3-vars.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# files that contain variables and macros that everything else depends on
CORE = docomp.el

URLSOURCES = \
	url-nfs.el url-file.el url-cookie.el url-parse.el url-irc.el	\
	url-gopher.el url-http.el url-mail.el url-misc.el url-news.el	\
	url-vars.el url-auth.el mm.el md5.el url-gw.el ssl.el base64.el	\
	url.el socks.el url-cache.el

CUSTOMSOURCES = # widget.el widget-edit.el
CUSTOMOBJECTS = $(CUSTOMSOURCES:.el=.elc)
URLOBJECTS    = $(URLSOURCES:.el=.elc)

SOURCES = \
	$(CUSTOMSOURCES) $(URLSOURCES) mule-sysdp.el w3-widget.el	\
	w3-imap.el css.el dsssl.el font.el images.el w3-vars.el		\
	w3-cus.el w3-style.el w3-keyword.el w3-forms.el w3-emulate.el	\
	w3-auto.el w3-menu.el w3-mouse.el w3-toolbar.el	w3-prefs.el	\
	w3-speak.el w3-latex.el w3-parse.el w3-display.el w3-print.el	\
	w3-about.el w3-hot.el w3-e19.el w3-xemac.el w3.el w3-script.el	\
	w3-jscript.el w3-elisp.el

OBJECTS = $(SOURCES:.el=.elc)

# Warning!  Currently, the following file can _NOT_ be bytecompiled.
EXTRAS = w3-sysdp.el

.SUFFIXES: .elc .el .el,v

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $<

w3:	check-custom docomp.el $(OBJECTS)
	@echo Build of w3 complete...

xemacs-w3:	docomp.el $(OBJECTS)
	@echo Build of w3 complete...

xemacs-w3: docomp.el $(OBJECTS)
	@echo Build of w3 complete...

all:	w3.info w3

install: all
	@echo Installing in $(lispdir)
	@( if [ ! -d $(lispdir) ]; then mkdir -p $(lispdir); fi )
	@( if [ ! -d $(infodir) ]; then mkdir -p $(infodir); fi )
	@( if [ ! -d $(confdir) ]; then mkdir -p $(confdir);  fi )
	$(INSTALL) -m 644 $(SOURCES) $(OBJECTS) $(lispdir)
	$(INSTALL) -m 644 $(EXTRAS) $(lispdir)
	$(INSTALL) -m 644 w3.info* $(infodir)
	$(INSTALL) -m 644 default.css $(confdir)/stylesheet
	$(INSTALL) -m 644 html32.dsl $(confdir)/

clean:
	$(RM) $(OBJECTS)

check-custom:
	@./custom-check $(EMACS)

w3.info:	w3.txi
	@$(MAKEINFO) w3.txi

w3.dvi:		w3.txi
	tex w3.txi
	texindex w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr
	tex w3.txi
	$(RM) 	w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr 	\
		w3.cps w3.fns w3.kys w3.pgs w3.tps w3.vrs	\
		w3.log w3.toc w3.aux

w3-vars.elc: w3-cus.el w3-vars.el
w3-display.elc: w3-display.el css.el font.el w3-imap.el
css.elc: css.el font.el
w3.elc: css.el w3-vars.el w3.el
