/* Include file for iterating over all buttons.
   Copyright (C) 1985, 1991-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2010 Ben Wing.

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

/* Synched up with: Not in FSF.  Split out of keymap.c. */

/* To use this, define FROB to do something with the button number.  No
   need to undefine, it happens automatically at the end of this file.  If
   you want button 0 included, define INCLUDE_BUTTON_ZERO (also undefined
   automatically). */

#ifdef INCLUDE_BUTTON_ZERO
FROB(0)
#endif
FROB(1)
FROB(2)
FROB(3)
FROB(4)
FROB(5)
FROB(6)
FROB(7)
FROB(8)
FROB(9)
FROB(10)
FROB(11)
FROB(12)
FROB(13)
FROB(14)
FROB(15)
FROB(16)
FROB(17)
FROB(18)
FROB(19)
FROB(20)
FROB(21)
FROB(22)
FROB(23)
FROB(24)
FROB(25)
FROB(26)

#undef FROB
#undef INCLUDE_BUTTON_ZERO
