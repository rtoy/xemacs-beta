/* Synched up with: FSF 19.31. */

#include "sol2.h"

/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO

#if 0 /* RMSmacs */
/* Unless this proves necessary, I'm going to leave it out. --ben
   Also see comment in sol2.h about LD_SWITCH_X_SITE_AUX. */
/* Override LD_SWITCH_SYSTEM: add  -L /usr/ccs/lib to the sol2.h value.  */

#undef LD_SWITCH_SYSTEM

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */
#endif /* 0 */

/* Motif is an optional package for Solaris 2.3.  The standard location is
   /usr/dt/lib, but it may have been installed in /opt/SUNWspro/lib when
   SPARCworks was installed. For Solaris 2.4 and beyond, the default location
   is /usr/dt/lib.  This "-R" definition allows an XEmacs built on Solaris 2.3
   to run on 2.3 and beyond at the cost of some extra, bound-to-fail searches
   in either /usr/dt/lib or /opt/SUNWspro/lib. 

   Even though /usr/lib is automatically appended to the -R search path, we
   include it at the begining of the search path to speed up the entire
   process.  "ldd xemacs" shows that there are 9 libraries found in /usr/lib,
   4 in /usr/openwin/lib and only 1 in /usr/dt/lib or /opt/SUNWspro/lib.
   -- vladimir@Eng.Sun.COM 
*/

#ifdef LD_SWITCH_SYSTEM
#undef LD_SWITCH_SYSTEM
#endif
#define LD_SWITCH_SYSTEM -R/usr/lib:/usr/openwin/lib:/usr/dt/lib:/opt/SUNWspro/lib

/* XEmacs: remove unnecessary signal stuff */
