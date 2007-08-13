#define AIX4

#include "aix3-2-5.h"

/* AIX 4 does not have HFT any more.  */
#undef AIXHFT

/* Get bzero, strcasecmp, and friends, for warning suppression.
   These functions shouldn't actually be used, but don't try to fix it.  */
#ifndef NOT_C_CODE
#include <strings.h>
#endif
