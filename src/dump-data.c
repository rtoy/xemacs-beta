/* Static array to put the dumped data in and its management
   Copyright (C) 2003 Olivier Galibert

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Mule-ized? Mwahahahahahaha */

/* Magic values by Larry McVoy to prevent every known compiler, including
   an especially perverse HP-UX one, from putting the array in BSS.
*/


#include <config.h>
#include "lisp.h"
#include "dump-data.h"

/* 4 bytes for the data size, 4096 for alignment */

static unsigned char dumped_data[MAX_SIZE+4096+4] = {
  255,
  6,
  1,
  2,
  3,
  4,
  255,
  3,
  9,
  62,
  255,
  10,
  4,
  61,
  255
};

size_t
dumped_data_size(void)
{
  return dumped_data[0] | (dumped_data[1] << 8) | (dumped_data[2] << 16) | (dumped_data[3] << 24);
}

size_t
dumped_data_max_size(void)
{
  return MAX_SIZE;
}

size_t
dumped_data_align_offset(void)
{
  EMACS_INT iptr = (EMACS_INT)dumped_data;
  EMACS_INT iptr2;
  iptr2 = (iptr+4+4095) & ~(EMACS_INT)4095;
  
  return iptr2-iptr;
}

unsigned char *
dumped_data_get(void)
{
  EMACS_INT iptr = (EMACS_INT)dumped_data;
  iptr = (iptr+4+4095) & ~(EMACS_INT)4095;
  return (unsigned char *)iptr;
}

