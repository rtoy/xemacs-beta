SHELL = /bin/sh
CC=gcc
CFLAGS=-bi486-linuxaout -O4 -m486 -malign-functions=2 -malign-jumps=2 -malign-loops=2
ALLOCA=
YACC=bison -y
LN_S=ln -s
version=19.14
configuration=i486-unknown-linux2.0.0
prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib
srcdir=/m/xemacs-19.14/lib-src
archlibdir=${libdir}/xemacs-${version}/${configuration}
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALLABLES = etags ctags emacsclient b2m gnuclient gnuattach gnudoit
INSTALLABLE_SCRIPTS = rcs-checkin
UTILITIES= make-path wakeup profile make-docfile digest-doc 	sorted-doc movemail cvtmail fakemail yow emacsserver hexl 	gnuserv
SCRIPTS= rcs2log vcdiff
EXECUTABLES= ${UTILITIES} ${INSTALLABLES} ${SCRIPTS} ${INSTALLABLE_SCRIPTS}
SOURCES = COPYING ChangeLog Makefile.in.in README aixcc.lex emacs.csh 	makedoc.com *.[chy] rcs2log vcdiff

VPATH=/m/xemacs-19.14/lib-src
LOADLIBES=       -L/usr/i486-linuxaout -L/lib-aout -L/m/xpm-3.4h/lib 
LOAD_X_LIBES= -L/usr/X11R6/lib   
C_SWITCH_X= -I/usr/X11R6/include    -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib 
ALL_CFLAGS = -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE    -Demacs -DHAVE_CONFIG_H    -DSTDC_HEADERS -I. -I../src -I${srcdir} -I${srcdir}/../src    ${LDFLAGS} ${CPPFLAGS} ${CFLAGS}
BASE_CFLAGS = -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE    -DHAVE_CONFIG_H -DSTDC_HEADERS     -I. -I../src -I${srcdir} -I${srcdir}/../src ${CPPFLAGS} ${CFLAGS}
LINK_CFLAGS = -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE    -Demacs -DHAVE_CONFIG_H    -I. -I../src -I${srcdir} -I${srcdir}/../src ${LDFLAGS} ${CFLAGS}
CPP_CFLAGS = -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE    -Demacs -DHAVE_CONFIG_H    -I. -I../src -I${srcdir} -I${srcdir}/../src ${CPPFLAGS} ${CFLAGS}
ALLOCA_CFLAGS = -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE    -Demacs -DHAVE_CONFIG_H    -I. -I../src -I${srcdir} -I${srcdir}/../src ${CPPFLAGS} ${CFLAGS}
.c.o:
	${CC} -c ${CPP_CFLAGS} $<
all: ${UTILITIES} ${INSTALLABLES} ${SCRIPTS}
rcs2log: force
	@if [ ! -r rcs2log ] ; then 				  rm -f rcs2log ; ${LN_S} ${srcdir}/rcs2log . ;		fi
vcdiff: force
	@if [ ! -r vcdiff ] ; then				  rm -f vcdiff ; ${LN_S} ${srcdir}/vcdiff . ;		fi
force:
maybe-blessmail:  
${archlibdir}: all
	@echo
	@echo "Installing utilities run internally by XEmacs."
	./make-path ${archlibdir}
	if [ `(cd ${archlibdir} && /bin/pwd)` != `/bin/pwd` ]; then 	  for file in ${UTILITIES}; do 	    (cd ..; $(INSTALL_PROGRAM) lib-src/$$file ${archlibdir}/$$file) ; 	  done ; 	fi
	if [ `(cd ${archlibdir} && /bin/pwd)` 	     != `(cd ${srcdir} && /bin/pwd)` ]; then 	  for file in ${SCRIPTS}; do 	    (cd ..; $(INSTALL_PROGRAM) ${srcdir}/$$file ${archlibdir}/$$file); 	  done ; 	fi
install: ${archlibdir}
	@echo
	@echo "Installing utilities for users to run."
	for file in ${INSTALLABLES} ; do 	  (cd ..; $(INSTALL_PROGRAM) lib-src/$${file} ${bindir}/$${file}) ; 	done
	for file in ${INSTALLABLE_SCRIPTS} ; do 	  (cd ..; $(INSTALL_PROGRAM) ${srcdir}/$${file} ${bindir}/$${file}) ; 	done
uninstall:
	(cd ${bindir}; 	 rm -f ${INSTALLABLES} ${INSTALLABLE_SCRIPTS})
	(cd ${archlibdir}; 	 rm -f ${UTILITIES} ${INSTALLABLES} ${SCRIPTS} ${INSTALLABLE_SCRIPTS})
mostlyclean:
	-rm -f core *.o
clean: mostlyclean
	-rm -f ${INSTALLABLES} ${UTILITIES}
distclean: clean
	-rm -f ../etc/DOC* *.tab.c *.tab.h aixcc.c TAGS
	-rm -f Makefile Makefile.in blessmail
realclean: distclean
	true
extraclean: realclean
	-rm -f *~ \#*
unlock:
	chmod u+w $(SOURCES)
relock:
	chmod u-w $(SOURCES)
check:
	@echo "We don't have any tests for XEmacs yet."
TAGS: etags
	etags *.[ch]
GETOPTOBJS = getopt.o getopt1.o $(ALLOCA)
GETOPTDEPS = $(GETOPTOBJS) ${srcdir}/getopt.h
getopt.o: ${srcdir}/getopt.c ${srcdir}/getopt.h
	${CC} -c ${CPP_CFLAGS} ${srcdir}/getopt.c
getopt1.o: ${srcdir}/getopt1.c ${srcdir}/getopt.h
	${CC} -c ${CPP_CFLAGS} ${srcdir}/getopt1.c
alloca.o: ${srcdir}/../src/alloca.c
	${CC} -c ${ALLOCA_CFLAGS} ${srcdir}/../src/alloca.c
REGEXOBJ = regex.o
REGEXDEPS = $(REGEXOBJ) ${srcdir}/../src/regex.h
regex.o: ${srcdir}/../src/regex.c ${srcdir}/../src/regex.h
	$(CC) -c ${BASE_CFLAGS} -DCONFIG_BROKETS -DINHIBIT_STRING_HEADER ${srcdir}/../src/regex.c
etags: ${srcdir}/etags.c $(GETOPTDEPS) $(REGEXDEPS) ../src/config.h
	$(CC) ${ALL_CFLAGS} -DVERSION="\"${version}\"" -DETAGS_REGEXPS ${srcdir}/etags.c $(GETOPTOBJS) $(REGEXOBJ) $(LOADLIBES) -o etags
ctags: ${srcdir}/etags.c $(GETOPTDEPS) etags
	$(CC) ${ALL_CFLAGS} -DCTAGS -DVERSION="\"${version}\"" ${srcdir}/etags.c $(GETOPTOBJS) $(LOADLIBES) -o ctags
wakeup: ${srcdir}/wakeup.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/wakeup.c $(LOADLIBES) -o wakeup
profile: ${srcdir}/profile.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/profile.c $(LOADLIBES) -o profile
make-docfile: ${srcdir}/make-docfile.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/make-docfile.c $(LOADLIBES) -o make-docfile
digest-doc: ${srcdir}/digest-doc.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/digest-doc.c $(LOADLIBES) -o digest-doc 
sorted-doc: ${srcdir}/sorted-doc.c ${ALLOCA}
	$(CC) ${ALL_CFLAGS} ${srcdir}/sorted-doc.c ${ALLOCA} $(LOADLIBES) -o sorted-doc
b2m: ${srcdir}/b2m.c ../src/config.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/b2m.c $(LOADLIBES) -o b2m 
movemail: ${srcdir}/movemail.c ../src/config.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/movemail.c $(LOADLIBES) -o movemail
cvtmail: ${srcdir}/cvtmail.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/cvtmail.c $(LOADLIBES) -o cvtmail
fakemail: ${srcdir}/fakemail.c ../src/config.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/fakemail.c $(LOADLIBES) -o fakemail
yow: ${srcdir}/yow.c ../src/paths.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/yow.c $(LOADLIBES) -o yow
emacsserver: ${srcdir}/emacsserver.c ../src/config.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/emacsserver.c $(LOADLIBES) -o emacsserver
emacsclient: ${srcdir}/emacsclient.c ../src/config.h
	$(CC) ${ALL_CFLAGS} ${srcdir}/emacsclient.c $(LOADLIBES) -o emacsclient
hexl: ${srcdir}/hexl.c
	$(CC) ${ALL_CFLAGS} ${srcdir}/hexl.c $(LOADLIBES) -o hexl
gnuslib.o: ${srcdir}/gnuslib.c ${srcdir}/gnuserv.h ../src/config.h
	$(CC) -c ${CPP_CFLAGS} ${C_SWITCH_X} ${srcdir}/gnuslib.c
gnuclient: ${srcdir}/gnuclient.c gnuslib.o ${srcdir}/gnuserv.h 
	$(CC) ${ALL_CFLAGS} -o gnuclient ${srcdir}/gnuclient.c gnuslib.o $(LOADLIBES) $(LOAD_X_LIBES) -lXau
gnuattach: ${srcdir}/gnuclient.c gnuslib.o ${srcdir}/gnuserv.h gnuclient
	$(CC) ${ALL_CFLAGS} -o gnuattach -DGNUATTACH ${srcdir}/gnuclient.c gnuslib.o $(LOADLIBES) $(LOAD_X_LIBES) -lXau
gnudoit: ${srcdir}/gnudoit.c gnuslib.o ${srcdir}/gnuserv.h 
	$(CC) ${ALL_CFLAGS} -o gnudoit ${srcdir}/gnudoit.c gnuslib.o $(LOADLIBES) $(LOAD_X_LIBES) -lXau
gnuserv: ${srcdir}/gnuserv.c gnuslib.o ${srcdir}/gnuserv.h
	$(CC) ${ALL_CFLAGS} ${C_SWITCH_X} -o gnuserv ${srcdir}/gnuserv.c gnuslib.o $(LOADLIBES) $(LOAD_X_LIBES) -lXau
make-path: ${srcdir}/make-path.c ../src/config.h
	$(CC) $(ALL_CFLAGS) ${srcdir}/make-path.c -o make-path
emacstool: ${srcdir}/emacstool.c
	$(CC) ${srcdir}/emacstool.c -o emacstool ${ALL_CFLAGS} 	  -lsuntool -lsunwindow -lpixrect $(LOADLIBES)
nemacstool: ${srcdir}/emacstool.c
	$(CC) -o nemacstool -DJLE ${ALL_CFLAGS} ${srcdir}/emacstool.c 	  -lsuntool -lmle -lsunwindow -lpixrect $(LOADLIBES)
xvetool: ${srcdir}/emacstool.c
	$(CC) -o xvetool -DXVIEW ${ALL_CFLAGS} ${srcdir}/emacstool.c 	  -lxview -lX -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib 	  $(LOADLIBES)
xveterm: ${srcdir}/emacstool.c
	$(CC) -o xveterm -DXVIEW -DTTERM ${ALL_CFLAGS} ${srcdir}/emacstool.c 	  -lxview -lolgx -lX  -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib 	  $(LOADLIBES)
aixcc: ${srcdir}/aixcc.c
	$(CC) $(ALL_CFLAGS) -o aixcc ${srcdir}/aixcc.c
aixcc.c: ${srcdir}/aixcc.lex
	lex ${srcdir}/aixcc.lex
	mv lex.yy.c aixcc.c
