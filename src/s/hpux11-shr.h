/* For building XEmacs under HPUX 11.0 with dynamic libraries.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St. - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Synched up with: FSF 19.31. */

/* XEmacs change */
#define HPUX_USE_SHLIBS

#include "hpux11.h"

/* We must turn off -g since it forces -static.  */
/* XEmacs deleted C_DEBUG_SWITCH */
