/* Definitions file for XEmacs running on SCO System V release 4.2
   Copyright (C) 1999 Ron Record

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Synched up with: FSF 19.31. */

#include "usg5-4.h"

/* Motif needs -lgen.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"

#define VFORK_RETURN_TYPE pid_t

/* XEmacs change: communicate to m/intel386.h */
#define USG5_4_2
