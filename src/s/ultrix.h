/* Definitions file for XEmacs running on Ultrix (bsd 4.3)
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "ultrix"

/* #### A very old comment in unix_open_network_stream() said this:

   Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
   when connect is interrupted.  So let's not let it get interrupted.

   Someone using Ultrix (anyone still out there?) should verify this.
*/

#define CONNECT_NEEDS_SLOWED_INTERRUPTS

/* We don't have a built-in strdup() function */
#define NEED_STRDUP
