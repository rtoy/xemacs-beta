/* Synched up with: FSF 19.31. */

/* Handle Solaris 2.5.  */

#include "sol2-4.h"

#if 0 /* A recent patch in unexelf.c should eliminate the need for this.  */
/* Don't use the shared libraries for -lXt and -lXaw,
   to work around a linker bug in Solaris 2.5.
   (This also affects the other libraries used specifically for
   the X toolkit, which may not be necessary.)  */
#define LIBXT_STATIC

#ifdef __GNUC__
#define STATIC_OPTION -Xlinker -Bstatic
#define DYNAMIC_OPTION -Xlinker -Bdynamic
#else
#define STATIC_OPTION -Bstatic
#define DYNAMIC_OPTION -Bdynamic
#endif
 
#endif /* 0 */

#ifndef NOT_C_CODE
/* Solaris 2.5 is the first Solaris that has getpagesize(), srandom()
   and random(), but they forgot to add prototypes to the header
   files. */
int getpagesize (void);
long random (void);
int srandom (unsigned int seed);
#endif /* NOT_C_CODE */

/* 2.5 now has random back in libc but we don't want to use it. */
#undef HAVE_RANDOM

/* Apparently this is not necessary here, and it causes 10% CPU
   chewage. */
#undef BROKEN_SIGCHLD
