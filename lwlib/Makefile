srcdir=/m/xemacs-19.14/lwlib
C_SWITCH_X_SITE=-I/usr/X11R6/include
C_SWITCH_SITE= -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib
SHELL=/bin/sh
CC=gcc
CFLAGS=-bi486-linuxaout -O4 -m486 -malign-functions=2 -malign-jumps=2 -malign-loops=2
CPP=gcc -E -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib -I/usr/X11R6/include
RANLIB=ranlib
RM = rm -f
AR = ar cq

VPATH=/m/xemacs-19.14/lwlib
EZ_OBJS =
ENERGIZEP =
EZ_SUBDIR =
SRCS = lwlib.c   lwlib-Xlw.c xlwmenu.c  xlwscrollbar.c   lwlib-Xaw.c  lwlib-utils.c        lwlib-config.c
OBJS = lwlib.o   lwlib-Xlw.o xlwmenu.o  xlwscrollbar.o   lwlib-Xaw.o  lwlib-utils.o        lwlib-config.o $(EZ_OBJS)
all:: $(EZ_SUBDIR) liblw.a
ALL_CFLAGS = -I.   -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib  -I/usr/X11R6/include  	         	     -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE  -DNEED_LUCID    -DNEED_ATHENA  	     -DMENUBARS_LUCID  -DSCROLLBARS_LUCID  -DDIALOGS_ATHENA  	     $(ENERGIZEP) ${CFLAGS} -DCONST_IS_LOSING 
.c.o:
	$(CC) -c $(CPPFLAGS) $(ALL_CFLAGS) $<
liblw.a: $(OBJS)
	$(RM) $@
	$(AR) $@ $(OBJS)
	-if [ -n $(RANLIB) ]; then 	  $(RANLIB) $@; 	fi
FRC.mostlyclean:
mostlyclean: FRC.mostlyclean
	rm -f liblw.a liblw_pure_*.a *.o
clean: mostlyclean
distclean: clean
	rm -f Makefile Makefile.in .pure
realclean: distclean
extraclean: distclean
	-rm -f *~ \#* m/?*~ s/?*~
lwlib-config.o:	lwlib.h ../src/config.h
lwlib-Xaw.o:	lwlib.h lwlib-internal.h lwlib-Xaw.h ../src/config.h
lwlib-Xlw.o:	lwlib.h lwlib-internal.h lwlib-Xlw.h xlwmenu.h ../src/config.h
lwlib-Xm.o:	lwlib.h lwlib-internal.h lwlib-utils.h lwlib-Xm.h
lwlib-Xm.o:	../src/config.h
lwlib-utils.o:	lwlib-utils.h ../src/config.h
lwlib.o:	lwlib.h lwlib-internal.h lwlib-utils.h ../src/config.h
lwlib.o:	lwlib-Xlw.h lwlib-Xm.h lwlib-Xaw.h ../src/config.h
xlwmenu.o:	lwlib.h xlwmenu.h xlwmenuP.h ../src/config.h
xlwscrollbar.o:	lwlib.h xlwscrollbar.h xlwscrollbarP.h ../src/config.h
