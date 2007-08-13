/* Synched up with: Not in FSF. */

/* For building XEmacs under SunOS 4.1.* with dynamic libraries. */

#ifdef NOT_C_CODE
# ifdef USE_GCC
  /* of course gcc has to take different args than the rest of the universe */
#  define LD_SWITCH_SYSTEM -dynamic
# else
#  define LD_SWITCH_SYSTEM -Bdynamic
# endif
#endif

#include "sunos4-1-4.h"
