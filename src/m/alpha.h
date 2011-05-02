/* machine description file For the alpha chip.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.  */


#ifndef __ELF__
  /* Describe layout of the address space in an executing process.  */
# define TEXT_START    0x120000000
# define DATA_START    0x140000000
  /* The program to be used for unexec. */
# define UNEXEC "unexalpha.o"
#endif
