/* Synched up with: FSF 19.31. */

/* System description file for hpux version 10.  */

#include "hpux9.h"

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef random /* XEmacs addition: necessary? */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX10
#define FORCE_ALLOCA_H

/* XEmacs addition: New paths for hpux 10 */
#undef C_SWITCH_X_SYSTEM
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2 -I/opt/audio/include
#undef LD_SWITCH_X_SYSTEM
#define LD_SWITCH_X_SYSTEM -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 -L/opt/audio/lib
