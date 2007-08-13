/* Synched up with: FSF 19.31. */

/* System description file for hpux version 10.  */

#include "hpux9-shr.h"

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX10
#define FORCE_ALLOCA_H
/* XEmacs addition */
#ifndef OBJECTS_SYSTEM
#define OBJECTS_SYSTEM strcat.o
#endif
/* AlainF 20-Jul-1996 -- fixes for 10.10, untested for 10.0x */
/* Fix kernel file name for 10.10 */
#undef KERNEL_FILE
#define KERNEL_FILE "/stand/vmunix"
/* The curses library seems to have a badly broken version of select(2)
   that makes "poll: interrupted system call" messages to appear and
   Emacs suprocesses to hang (e.g. TeX compilation w/ AUCTeX) */
#undef LIBS_TERMCAP
#define LIBS_TERMCAP -ltermcap
/* 10.10 has Xmu in /usr/contrib/X11R5/lib */
#ifndef HAVE_LIBXMU
#undef C_SWITCH_X_SYSTEM 
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2 -I/usr/contrib/include/X11R5
#undef LD_SWITCH_X_DEFAULT
#define LD_SWITCH_X_DEFAULT -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 -L/usr/contrib/lib/X11R5
#undef LIBXMU
#define LIBXMU -lXmu
#define HAVE_LIBXMU 1
#undef NO_EDITRES
#endif
