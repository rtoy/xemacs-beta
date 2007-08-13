/* Synched up with: FSF 19.31. */

/* Handle Solaris 2.4.  */

#include "sol2-3.h"

/* XEmacs addition: Raymond Toy says XEmacs completely misses SIGCHLD
   when compiled with GCC 2.7.0 (but not, apparently, with SunPro C?),
   X11R6, and Solaris 2.4.

   Someone else submitted a simple test program that duplicates this
   behavior, and says it has something to do with the fact that X11R6
   links with the threads library. */

#ifdef THIS_IS_X11R6
#define BROKEN_SIGCHLD
#endif
