/* Synched up with: FSF 19.31. */

/* Handle Solaris 2.5.  */

#include "sol2-4.h"

#ifndef NOT_C_CODE
/* Solaris 2.5 is the first Solaris that has getpagesize(), srandom()
   and random(), but they forgot to add prototypes to the header
   files. */
int getpagesize (void);
long random (void);
void srandom (unsigned int seed);
#endif /* C_CODE */

/* 2.5 now has random back in libc but we don't want to use it. */
#undef HAVE_RANDOM

/* Apparently this is not necessary here, and it causes 10% CPU
   chewage. */
#undef BROKEN_SIGCHLD
