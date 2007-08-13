/* Synched up with: FSF 19.31. */

/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

#if 0 /* FSFmacs */

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */
#endif /* 0 */

#undef LD_SWITCH_SYSTEM
#undef C_SWITCH_SYSTEM

#if 0 /* mrb */
#undef LD_SWITCH_SYSTEM
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX -R /usr/openwin/lib -L /usr/openwin/lib -R /usr/dt/lib -L /usr/dt/lib
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX` -R /usr/openwin/lib -L /usr/openwin/lib -R /usr/dt/lib -L /usr/dt/lib
#endif /* GCC */

/* Gregory Neil Shapiro <gshapiro@hhmi.org> reports the Motif header files
   are in this directory on Solaris 2.4.  */
#define C_SWITCH_X_SYSTEM -I/usr/dt/include
#endif

/* XEmacs addition: Raymond Toy says XEmacs completely misses SIGCHLD
   when compiled with GCC 2.7.0 (but not, apparently, with SunPro C?),
   X11R6, and Solaris 2.4.

   Someone else submitted a simple test program that duplicates this
   behavior, and says it has something to do with the fact that X11R6
   links with the threads library. */

#ifdef THIS_IS_X11R6
#define BROKEN_SIGCHLD
#endif
