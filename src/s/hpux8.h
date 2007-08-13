/* Synched up with: FSF 19.31. */

/* system description file for hpux version 8.
   This contains changes that were suggested "for the hp700".
   They were not needed for the 800.
   Our conjecture that they are needed for hpux version 8,
   which is what runs on the 700.  */

/* XEmacs change -- suggested by hamish@bnr.ca */
#define DONT_DEFINE_NO_REMAP /* `static' hack not needed */

#include "hpux.h"

#define HPUX8

/* XEmacs change -- suggested by hamish@bnr.ca */
#undef HPUX_PRE_8_0

/* XEmacs change -- Ugly, nasty kludge to prevent X11R4 Xos.h from
   redefining struct timeval and struct timezone. */
#define __TIMEVAL__

#if 0 /* RMSmacs */
/* #### It appears that the FSF version of hpux8.h handles both
   R4 and R5, while the XEmacs one handles only R4.  Someone
   fix this up. */

/* dob@inel.gov says HPUX 8.07 needs this.  He was using X11R5, I think.  */
#define LIBX11_SYSTEM -lXext

#define LIB_X11_LIB -L/usr/lib/X11R5 -L/usr/lib/X11R4 -lX11
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/X11R4
#define LD_SWITCH_X_DEFAULT -L/usr/lib/X11R5 -L/usr/lib/X11R4
#endif

/* XEmacs change -- changed LIBX11_SYSTEM and C_SWITCH_X_SYSTEM */
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R4 -I/usr/include/Motif1.1
#define LD_SWITCH_X_SYSTEM -L/usr/lib/X11R4 -L/usr/lib/Motif1.1

/* Don't use shared libraries.  unexec doesn't handle them.
   Note GCC automatically passes -a archive to ld, and it has its own
   conflicting -a.  */
#ifdef __GNUC__
/* No need to specify roundabout way of linking temacs.  */
#define ORDINARY_LINK

#ifdef HPUX_USE_SHLIBS
#define LD_SWITCH_SYSTEM
#else
#define LD_SWITCH_SYSTEM -Xlinker -a -Xlinker archive
#endif

#else /* not __GNUC__ */

#if (defined(hp9000s700) || defined(__hp9000s700))
#ifdef HPUX_USE_SHLIBS
#define LD_SWITCH_SYSTEM -L/lib/pa1.1
#else
#define LD_SWITCH_SYSTEM -a archive -L/lib/pa1.1
#endif
#else /* not (defined(hp9000s700) || defined(__hp9000s700)) */
#ifdef HPUX_USE_SHLIBS
#define LD_SWITCH_SYSTEM
#else
#define LD_SWITCH_SYSTEM -a archive
#endif
#endif /* not (defined(hp9000s700) || defined(__hp9000s700)) */

#endif /* not __GNUC__ */

/* XEmacs change */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Aa -D_HPUX_SOURCE
#endif

/* Some hpux 8 machines seem to have TIOCGWINSZ,
   and none have sioctl.h, so might as well define this.  */
#define NO_SIOCTL_H

#if 0 /* autoconf should be detecting the presence or absence of 
	 random and srandom now.  */
/* If you use X11R4 you must define this.  If you use
   X11R5 you must comment this out */
/* #define HAVE_RANDOM */
#define random foo_random
#define srandom foo_srandom
#endif

#if 0  /* This seems to be spurious.  */
/* "X11R5" on hpux8 doesn't have this function, which is supposed to exist
   in X11R5.  Maybe things will work if we just don't call it.  */
#define NO_XRM_SET_DATABASE
#endif

/* Enable a special hack in XTread_socket.  */
/* XEmacs change:  we don't use this. */
#if 0
#define X_IO_BUG
#endif

/* XEmacs change */

#undef HAVE_UNION_WAIT
