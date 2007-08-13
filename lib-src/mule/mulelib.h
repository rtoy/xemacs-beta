#ifndef _MULELIB_H
#define _MULELIB_H

#include "config.h"
#include "paths.h"
#include "lisp.h"
#include "mule-charset.h"
#include "mule-coding.h"
#include "bdf.h"

extern char *mule_library_version;

extern int mule_error;
extern char mule_error_msg[256];

#define MULE_ERROR_UNKNOWN_CODE		1
#define MULE_ERROR_OVERFLOW		2
#define MULE_ERROR_NO_CHARSETS		3
#define MULE_ERROR_INVALID_CHARSETS	4
#define MULE_ERROR_NO_CODING		5
#define MULE_ERROR_INVALID_CODING	6

#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

#ifndef CHARSETS
#define CHARSETS "CHARSETS"
#endif

#ifndef CODINGS
#define CODINGS "CODINGS"
#endif

#define CODING_SYSTEM_COUNT 256
extern struct coding_type coding_system_table[CODING_SYSTEM_COUNT];
extern int n_base_coding_system;
extern int n_coding_system;

extern char *font_name[128];
extern int font_encoding[128];
extern CCL_PROGRAM *x_ccl_programs[128];

extern FILE *open_file();

#ifdef MSDOS
extern void init_environment();
extern char *rootrelativepath();
#endif

#endif /* _MULELIB_H */
