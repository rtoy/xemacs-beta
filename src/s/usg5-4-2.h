/* Synched up with: FSF 19.31. */

/* s/ file for System V release 4.2.  */

#include "usg5-4.h"

/* Motif needs -lgen.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"

/* Use libw.a along with X11R6 Xt.  */
#define NEED_LIBW

/* ryanr@ellingtn.ftc.nrcs.usda.gov (Richard Anthony Ryan) says -lXimp
   is needed in UNIX_SV ... 4.2 1.1.2.  */
#define LIB_MOTIF "-lXm -lXimp"

#define VFORK_RETURN_TYPE pid_t

/* XEmacs change (since getwd is auto-determined) */
#undef HAVE_GETWD  /* (appears to be buggy on SVR4.2) */

/* XEmacs change: communicate to m/intel386.h */
#define USG5_4_2
