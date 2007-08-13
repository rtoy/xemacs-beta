/* Synched up with: Not in FSF. */

#include "decosf4-0.h"

#ifdef NOT_C_CODE
/* This is to get rid of the definition that selects dynamic linking. */
#undef LD_SWITCH_CALL_SHARED
#ifdef USE_GCC
#define LD_SWITCH_CALL_SHARED -static -Xlinker -non_shared
#else
#define LD_SWITCH_CALL_SHARED -non_shared
#endif /* USE_GCC */
#endif /* NOT_C_CODE */
