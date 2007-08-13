SHELL = /bin/sh
CC=gcc
CFLAGS=-bi486-linuxaout -O4 -m486 -malign-functions=2 -malign-jumps=2 -malign-loops=2
ARCH=
srcdir=/m/xemacs-19.14/dynodump

VPATH=/m/xemacs-19.14/dynodump
INCLUDES = -I${srcdir} -I${srcdir}/$(ARCH)
ALL_CFLAGS = ${CFLAGS} -fpic  ${INCLUDES}
OBJS = _relocate.o dynodump.o syms.o uninit.o
.c.o:
	$(CC) -c $(ALL_CFLAGS) $<
all::
dynodump.so: ${srcdir}/_dynodump.h $(OBJS)
	$(CC) -o dynodump.so -G $(OBJS) -lelf -lmapmalloc
_relocate.o: ${srcdir}/$(ARCH)/_relocate.c
	$(CC) -c $(ALL_CFLAGS) ${srcdir}/$(ARCH)/_relocate.c
uninit.o: ${srcdir}/$(ARCH)/uninit.c
	$(CC) -c $(ALL_CFLAGS) ${srcdir}/$(ARCH)/uninit.c
mostlyclean:
	-rm -f *.o \#*
clean: mostlyclean
	-rm -f *.so *.so.1
distclean: clean
	-rm -f Makefile Makefile.in
realclean: distclean
	rm -f TAGS
versionclean:
	true
extraclean: realclean
	-rm -f *~ \#*
