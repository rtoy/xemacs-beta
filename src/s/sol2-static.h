/* Synched up with: Not in FSF. */

#ifndef DONT_INCLUDE_SOL2_H
#include "sol2.h"
#endif

/* Force static linking */
/* Here is how to find X Windows.  The -R option says where
   to find X windows at run time.  */
#undef LD_SWITCH_SYSTEM
#ifndef __GNUC__
# ifdef USE_LCC
#  define LD_SWITCH_SYSTEM -R /usr/openwin/lib -Bstatic
# else 
#  define LD_SWITCH_SYSTEM -R/usr/openwin/lib -Bstatic
#endif
#else /* GCC */
/* jwz: note that we need "-Xlinker -Bstatic" and not just "-static" */
#define LD_SWITCH_SYSTEM -Xlinker -R/usr/openwin/lib -Xlinker -Bstatic
#endif /* GCC */

/* static linking and Solaris don't mix real well */
#undef LIB_STANDARD
#ifndef __GNUC__
# ifdef I18N4
#  define LIB_STANDARD -lw -Bdynamic -lc -ldl -Bstatic
# else /* !I18N4 */
#  define LIB_STANDARD -lw -lc -Bdynamic -ldl -Bstatic
# endif /* !I18N4 */
#else
# define LIB_STANDARD -lw -lc -Xlinker -Bdynamic -ldl
#endif

#undef LIBS_SYSTEM
#ifdef NOT_C_CODE
# undef LIBS_SYSTEM
# define LIBS_SYSTEM -lsocket -lnsl -lintl -lkvm -lelf -lgen
#else
# define LIBS_SYSTEM -lsocket -lnsl -lkvm -lelf -lgen
#endif
