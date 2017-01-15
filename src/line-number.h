/* Copyright (C) 1997, Free Software Foundation, Inc.

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

#ifndef INCLUDED_line_number_h_
#define INCLUDED_line_number_h_

/* Synched up with: Not in FSF. */

void narrow_line_number_cache (struct buffer *);
void insert_invalidate_line_number_cache (struct buffer *, Charbpos,
					  const Ibyte *, Bytecount);
void delete_invalidate_line_number_cache (struct buffer *, Charbpos, Charbpos);

EMACS_INT buffer_line_number (struct buffer *, Charbpos, int, Boolint);

#endif /* INCLUDED_line_number_h_ */
