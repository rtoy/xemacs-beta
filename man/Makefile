SHELL = /bin/sh
CC=gcc
CFLAGS=-bi486-linuxaout -O4 -m486 -malign-functions=2 -malign-jumps=2 -malign-loops=2
YACC=bison -y
version=19.14
configuration=i486-unknown-linux2.0.0
prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib
srcdir=/m/xemacs-19.14/man

VPATH=/m/xemacs-19.14/man
LOADLIBES= -L/usr/i486-linuxaout -L/lib-aout -L/m/xpm-3.4h/lib   -lgcc -lc -lgcc /usr/lib/crtn.o 
CPPFLAGS=
ALL_CFLAGS = -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib  -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE  -Demacs -DHAVE_CONFIG_H    -DHAVE_STDARG_H -DHAVE_STRING_H -DHAVE_ALLOCA_H -Dxfree=free    -I. -I../src -I${srcdir} -I${srcdir}/../src ${CFLAGS}
CPP_CFLAGS = -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib  -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE  -Demacs -DHAVE_CONFIG_H    -DHAVE_STDARG_H -DHAVE_STRING_H -DHAVE_ALLOCA_H -Dxfree=free    -I. -I../src -I${srcdir} -I${srcdir}/../src ${CPPFLAGS} ${CFLAGS}
OBJS = makeinfo.o getopt.o getopt1.o alloca.o
SUBDIR = xemacs lispref new-users-guide internals
SUBDIR_MAKEFILES = xemacs/Makefile lispref/Makefile new-users-guide/Makefile internals/Makefile
.c.o:
	${CC} -c ${CPP_CFLAGS} $<
../info/%.info : %.texi
	-./makeinfo -o $@ $<
srcs = ange-ftp cc-mode cl dired ediff external-widget forms gnus        hyperbole ilisp info internals ispell mailcrypt message mh-e        oo-browser pcl-cvs psgml rmail standards supercite term termcap        texinfo viper vm w3 xemacs-faq
info = $(srcs:%=../info/%.info)
all : ${SUBDIR} $(info)
.RECURSIVE: ${SUBDIR}
xemacs: FRC.xemacs
lispref: FRC.lispref
new-users-guide: FRC.new-users-guide
internals: FRC.internals
${SUBDIR}: ${SUBDIR_MAKEFILES} makeinfo FRC
	cd $@ && $(MAKE) all $(MFLAGS) 		CC='${CC}' CFLAGS='${CFLAGS}' MAKE='${MAKE}'
makeinfo: ../src/config.h $(OBJS)
	$(CC) -o makeinfo $(OBJS)
texindex: texindex.o
	$(CC) -o texindex texindex.o
alloca.c :
	[ -h alloca.c ] || ln -s ../src/alloca.c .
getopt.h :
	[ -h getopt.h ] || ln -s ../lib-src/getopt.h .
getopt.c :
	[ -h getopt.c ] || ln -s ../lib-src/getopt.c .
getopt1.c :
	[ -h getopt1.c ] || ln -s ../lib-src/getopt1.c .
mostlyclean: clean
	(cd xemacs &&	$(MAKE) $(MFLAGS) mostlyclean)
	(cd lispref &&	$(MAKE) $(MFLAGS) mostlyclean)
	(cd new-users-guide && $(MAKE) $(MFLAGS) mostlyclean)
	(cd internals && $(MAKE) $(MFLAGS) mostlyclean)
clean:
	rm -f *.dvi
	rm -f getopt1.c getopt.c getopt.h alloca.c makeinfo texindex
	rm -f *.toc *.aux *.log *.cp *.fn *.ky *.pg *.vr *.o core
distclean: mostlyclean
	(cd xemacs &&	$(MAKE) $(MFLAGS) distclean)
	(cd lispref &&	$(MAKE) $(MFLAGS) distclean)
	(cd new-users-guide && $(MAKE) $(MFLAGS) distclean)
	(cd internals && $(MAKE) $(MFLAGS) distclean)
	rm -f Makefile Makefile.in
realclean: distclean
	(cd xemacs &&	$(MAKE) $(MFLAGS) realclean)
	(cd lispref &&	$(MAKE) $(MFLAGS) realclean)
	(cd new-users-guide && $(MAKE) $(MFLAGS) realclean)
	(cd internals && $(MAKE) $(MFLAGS) realclean)
extraclean: distclean
	(cd xemacs &&	$(MAKE) $(MFLAGS) extraclean)
	(cd lispref &&	$(MAKE) $(MFLAGS) extraclean)
	(cd new-users-guide && $(MAKE) $(MFLAGS) extraclean)
	(cd internals && $(MAKE) $(MFLAGS) extraclean)
	-rm -f *~ \#*
makeinfo.o : getopt.h
getopt.o : getopt.h
getopt1.o : getopt.h
texindex.o : getopt.h
FRC FRC.xemacs FRC.lispref FRC.new-users-guide FRC.internals:
