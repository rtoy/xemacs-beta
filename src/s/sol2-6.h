/* Synched up with: Not in FSF. */

/* Handle Solaris 2.6.  */

#include "sol2-5.h"

/* 2.6 has dldump, dynodump is not necessary */
#ifdef UNEXEC
#undef UNEXEC
#endif
#define UNEXEC "unexsol2-6.o"
