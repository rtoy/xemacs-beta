/* Portable data dumper for XEmacs.
   Copyright (C) 1999-2000 Olivier Galibert

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

/* This file has been Mule-ized, Ben Wing, 10-7-04. */

#ifndef INCLUDED_dumper_h
#define INCLUDED_dumper_h

BEGIN_C_DECLS

void pdump_objects_unmark (void);
void pdump (void);
int pdump_load (const Wexttext *argv0);
void pdump_backtrace (void);
extern unsigned int dump_id;
extern Rawbyte *pdump_start, *pdump_end;

END_C_DECLS

#endif /* INCLUDED_dumper_h */
