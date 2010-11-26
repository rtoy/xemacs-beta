/* Machine description file for generic Motorola 68k.
   Copyright (C) 1985, 1995 Free Software Foundation, Inc.

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef m68k
#define m68k
#endif

#ifdef linux

#define NO_REMAP
#define TEXT_START 0
#endif

#define ASSERT_VALID_POINTER(pnt) (assert ((((int) pnt) & 1) == 0))
