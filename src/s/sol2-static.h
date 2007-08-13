/* Synched up with: Not in FSF. */

#ifndef DONT_INCLUDE_SOL2_H
#include "sol2.h"
#endif

/* Force static linking */
#ifdef NOT_C_CODE

#undef  LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM "-Bstatic"

/* static linking and Solaris don't mix real well */
#undef LIB_STANDARD
#ifdef I18N4
# define LIB_STANDARD "-lw -Bdynamic -lc -ldl -Bstatic"
#else /* !I18N4 */
# define LIB_STANDARD "-lw -lc -Bdynamic -ldl -Bstatic"
#endif /* !I18N4 */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"

#endif /* NOT_C_CODE */
