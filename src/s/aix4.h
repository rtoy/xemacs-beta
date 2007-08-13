/* Synched up with: FSF 19.31. */

#define AIX4

#include "aix3-2-5.h"

/* AIX 4 does not have HFT any more.  */
#undef AIXHFT

/* realpath is broken */
#undef HAVE_REALPATH
