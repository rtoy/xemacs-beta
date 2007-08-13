/* Synched up with: FSF 19.31. */

/* System description file for hpux version 9.  */

#include "hpux8.h"

#define HPUX9

/* If Emacs doesn't seem to work when built to use GNU malloc, you
   probably need to get the latest patches to the HP/UX compiler.
   See `etc/MACHINES' for more information.  */
#if 0
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC
#endif

/* cc1: warning: `-g' not supported by this configuration of GCC
   #### Still needs to be fixed in a more general way... */
#if 0 
#ifdef __GNUC__
#undef  C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
#endif

#ifndef __GNUC__
/* Make room for enough symbols, so dispnew.c does not fail.  */
/* XEmacs: cognot@ensg.u-nancy.fr: C_SWITCH_SYSTEM already defined in hpux8.h,
                           -D_BSD makes hp CC choke on process.c
#define C_SWITCH_SYSTEM -Wp,-H200000 -D_BSD
*/
#undef C_SWITCH_SYSTEM
# ifdef __hp9000s300
#  define C_SWITCH_SYSTEM -Aa -D_HPUX_SOURCE
# else
#  define C_SWITCH_SYSTEM -Ae -Wp,-H100000
# endif
/* XEmacs: commented out
#else
#define C_SWITCH_SYSTEM -D_BSD
*/
#endif

/* neal@ctd.comsat.com */
#define NO_TERMIO

/* According to ngorelic@speclab.cr.usgs.gov,
   references to the X11R4 directories in these variables
   (inherited from hpux8.h)
   cause the wrong libraries to be found,
   and the options to specify the X11R5 directories are unnecessary
   since the R5 files are found without them.  */
#undef LIB_X11_LIB
#undef C_SWITCH_X_SYSTEM
#undef LD_SWITCH_X_SYSTEM
/* However, HPUX 9 has Motif includes in a strange place.
   So search that place.  These definitions assume that X11R5 is being
   used -- if X11R4 is used, "s/hpux9-x11r4.h" gets loaded instead.  */
/* XEmacs change: Change LD_SWITCH_X_DEFAULT to LD_SWITCH_X_SYSTEM.
   #### Why do we need to make this change? */
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2
#define LD_SWITCH_X_SYSTEM -L/usr/lib/X11R5 -L/usr/lib/Motif1.2

#ifndef HAVE_LIBXMU
/* HP-UX doesn't supply Xmu.  */
#define LIBXMU

/* Unfortunately without libXmu we cannot support EditRes.  */
#define NO_EDITRES
#endif

/* zoo@armadillo.com says we don't need -lXext in HPUX 9.  */
#undef LIBX11_SYSTEM

/* XEmacs: apparently rint() is totally broken in HPUX 9. */
#undef HAVE_RINT

/* XEmacs addition */
#ifndef OBJECTS_SYSTEM
#define OBJECTS_SYSTEM strcat.o
#endif
