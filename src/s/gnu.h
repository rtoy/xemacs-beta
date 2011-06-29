/* Definitions file for XEmacs running on the GNU Hurd.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

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

/* Synched up with: FSF 19.31. */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* Delete BSD4_2 -- unused in XEmacs */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

/* XEmacs deleted LIBS_DEBUG */

/* GNU needs its own crt0, and libc defines data_start.  */
#define ORDINARY_LINK
#define DATA_START ({ extern int data_start; (char *) &data_start; })

/* GNU now always uses the ELF format.  */
#define UNEXEC "unexelf.o"

/* Some losing code fails to include this and then assumes
   that because it is braindead that O_RDONLY==0.  */
#ifndef NOT_C_CODE
#include <fcntl.h>
#endif
