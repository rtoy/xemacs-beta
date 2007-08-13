# Temp file to use to build .elc files
ELISP-TO-COMPILE = /tmp/elc-url-${USER}

# what emacs is called on your system
EMACS = xemacs

# where the Info file should go
INFODIR = .

# where the lisp files will be installed.
LISPDIR = $$HOME/lisp

# Change this to be where your .emacs file is stored
DOTEMACS      = $$HOME/.emacs

# Change this to be how to convert texinfo files into info files
# examples:
# MAKEINFO       = $(EMACS) -batch -q -f batch-texinfo-format
# MAKEINFO       = makeinfo
MAKEINFO         = makeinfo

# How to make a directory
# need a -p if you want to make the parents!
MKDIR = mkdir

# Various other stuff used
RM    = rm -f
CP    = cp
############## no user servicable parts beyond this point ###################
MAJOR=1
MINOR=0

# Have to preload a few things to get a nice clean compile
DEPS = -l ./url-vars.el -l ./docomp.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# What type of version this is - beta or normal
VTYPE = p
DIRNAME = url

.SUFFIXES: .elc .el .el,v

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $<

OBJECTS = \
	url-file.elc	\
	url-nfs.elc	\
	url-cookie.elc	\
	url-irc.elc	\
	url-parse.elc	\
	url-gopher.elc	\
	url-hash.elc	\
	url-http.elc	\
	url-mail.elc	\
	url-misc.elc	\
	url-news.elc	\
	url-pgp.elc	\
	url-vars.elc	\
	url-wais.elc	\
	urlauth.elc	\
	mm.elc		\
	md5.elc		\
	ssl.elc		\
	base64.elc	\
	url.elc

SOURCES = \
	docomp.el	\
	url-nfs.el	\
	url-sysdp.el	\
	url-file.el	\
	url-cookie.el	\
	url-parse.el	\
	url-irc.el	\
	url-gopher.el	\
	url-hash.el	\
	url-http.el	\
	url-mail.el	\
	url-misc.el	\
	url-news.el	\
	url-pgp.el	\
	url-vars.el	\
	url-wais.el	\
	urlauth.el	\
	mm.el		\
	md5.el		\
	ssl.el		\
	base64.el	\
	url.el

DISTFILES     = $(SOURCES) descrip.mms

url:	docomp.el $(OBJECTS)
	@echo Build of url complete...

clean:
	rm -f $(OBJECTS)

ChangeLog:
	/usr/local/bin/rcs2log -h cs.indiana.edu -i 0 >| ChangeLog

version: ChangeLog url.info
	@echo -n "Creating new version "
	@mkdir $(DIRNAME)
	@./incvers
	@co -q $(DISTFILES)
	@rcs -q -s$(VTYPE)$(MAJOR)-$(MINOR)-`cat .version` url-vars.el
	@rcs -q -s$(VTYPE)$(MAJOR)-$(MINOR)-`cat .version` url.texi
	@co -q -kv $(SOURCES) docomp.el url.texi
	@mv $(DISTFILES) $(DIRNAME)
	@cp Makefile $(DIRNAME)/Makefile
	@touch $(DIRNAME)/THIS-IS-VERSION-$(MAJOR).$(MINOR).`cat .version`
	@mv ChangeLog url.info* $(DIRNAME)
	@echo -n "$(MAJOR).$(MINOR)."
	@echo `cat .version`
	@tar czf url-$(MAJOR).$(MINOR).`cat .version`.tar.gz $(DIRNAME)
	@rcs -nv$(MAJOR)-$(MINOR)-`cat .version`: $(SOURCES)
	@rm -fr $(DIRNAME)

url.html: url.texi
	@texi2html -menu -split_node -verbose url.texi

url.info: url.texi
	@$(MAKEINFO) url.texi

url.dvi: url.texi
	@tex url.texi
	@texindex url.cp  url.fn  url.ky  url.pg  url.tp  url.vr
	@tex url.texi
	@rm -f 	url.cp  url.fn  url.ky  url.pg  url.tp  url.vr 	\
		url.cps url.fns url.kys url.pgs url.tps url.vrs	\
		url.log url.toc url.aux

install: url
	@echo Installing in $(LISPDIR)
	if [ ! -d $(LISPDIR) ] ; then $(MKDIR) $(LISPDIR) ; fi
	(cd $(LISPDIR) ; $(RM) $(SOURCES) $(OBJECTS) url-sysdp.el*)
	$(CP) $(SOURCES) $(OBJECTS) $(LISPDIR)
	if [ ! -d $(INFODIR) ] ; then $(MKDIR) $(INFODIR) ; fi
	$(CP) url.info* $(INFODIR)
