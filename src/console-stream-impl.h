/* Define stream specific console, device, and frame object for XEmacs.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/* Written by Ben Wing. */

#ifndef INCLUDED_console_stream_impl_h_
#define INCLUDED_console_stream_impl_h_

#include "console-impl.h"
#include "console-stream.h"

DECLARE_CONSOLE_TYPE (stream);

struct stream_console
{
  FILE *in;
  FILE *out;
  FILE *err;
  int needs_newline;
};

#define CONSOLE_STREAM_DATA(con) CONSOLE_TYPE_DATA (con, stream)

#endif /* INCLUDED_console_stream_impl_h_ */
