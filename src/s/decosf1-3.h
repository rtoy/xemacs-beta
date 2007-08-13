/* Synched up with: Not in FSF. */

#include "decosf1-2.h"
/* XEmacs change: Kim Nyberg <kny@tekla.fi> says this is needed. */
#ifdef emacs
#include <sys/stropts.h>
#endif

/* Supposedly gmalloc and rel_alloc will work now
   (grunwald@foobar.cs.colorado.edu) */
#undef SYSTEM_MALLOC
#undef NO_REMAP

#define _NO_MALLOC_WARNING_

#ifdef NOT_C_CODE
/* This to get rid of the -X that Makefile inserts
 * and force dynamic linking and optimization during link time.
 * The ifndef is needed to avoid screwups during configure
 */
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM

#undef LD_SWITCH_CALL_SHARED
#ifndef USE_GCC
#define LD_SWITCH_CALL_SHARED -call_shared
#endif /* !USE_GCC */

#undef LD_SWITCH_SHARED
#define LD_SWITCH_SHARED -shared
#endif /* NOT_C_CODE */
