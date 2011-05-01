/* Copyright (C) 2000, 2003 Martin Buchholz

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Completely divergent from FSF. */
#define SOLARIS2 1
/* #define POSIX -- not used in XEmacs */

/* Fix understandable GCC lossage on Solaris 2.6 */
#if defined(__GNUC__) && OS_RELEASE >= 506 && OS_RELEASE < 510 && !defined(NOT_C_CODE)

/* GCC va_list munging is a little messed up */
#define __GNUC_VA_LIST
#define _VA_LIST_
#define _VA_LIST va_list
typedef void *__gnuc_va_list;
typedef __gnuc_va_list va_list;

/* Missing prototypes for functions added in Solaris 2.6 */
#include <sys/types.h>
struct msghdr;
struct sockaddr;
extern int     __xnet_bind    (int, const struct sockaddr *, size_t);
extern int     __xnet_listen  (int, int);
extern int     __xnet_connect (int, const struct sockaddr *, size_t);
extern ssize_t __xnet_recvmsg (int, struct msghdr *, int);
extern ssize_t __xnet_sendmsg (int, const struct msghdr *, int);
extern ssize_t __xnet_sendto  (int, const void *, size_t, int, const struct sockaddr *, size_t);
extern int     __xnet_socket  (int, int, int);
extern int     __xnet_socketpair (int, int, int, int *);
extern int     __xnet_getsockopt (int, int, int, void *, size_t *);
#endif /* GCC && >= Solaris 2.6 && C code */

#include "usg5-4-2.h"	/* XEmacs change from 5-4 to 5-4-2 */
#undef PC /* Defined in x86 /usr/include/sys/reg.h */

/* SIGIO seems to be working under Solaris and it makes ^G work better... */
#undef BROKEN_SIGIO

#ifdef NOT_C_CODE
/* XEmacs change -- some Motif packages need -lgen to get regex and regcmp */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen -ldl"

/* SYSTEM_MALLOC must be defined if dbx/RTC is going to be used.  dbx/RTC does
   not work with a static definition of malloc(). */
/* We want to be able to test out ralloc.c. */
/* #define SYSTEM_MALLOC */

#undef UNEXEC
#define UNEXEC "unexsol2-6.o"

#else /* C_CODE */

/* 2.5 now has `random' back in libc but we don't want to use it. */
#undef HAVE_RANDOM

#include <sys/utsname.h> /* warning: macro redefined: SYS_NMLN */

#endif /* C_CODE */
