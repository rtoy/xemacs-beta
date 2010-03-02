/* Copyright (c) 2010 Ben Wing.

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

#ifndef INCLUDED_unicode_h_
#define INCLUDED_unicode_h_

#ifdef MULE

/************************************************************************/
/*                          Precedence arrays                           */
/************************************************************************/

struct precedence_array
{
  struct LCRECORD_HEADER header;

  /* Encapsulated dynarr listing all the charsets, in order, used for
     converting a Unicode codepoint to a charset codepoint */
  Lisp_Object_dynarr *precdyn;
  /* Have we seen ASCII yet? */
  unsigned int seen_ascii :1;
  /* Does the precedence array have a charset that's higher-precedence than
     ASCII and contains different mappings for any Unicode codepoints in
     the ASCII range?  This flag and the preceding one are used to
     determine whether we can short-circuit ASCII conversion */
  unsigned int has_overriding_ascii :1;
};

DECLARE_LRECORD (precedence_array, struct precedence_array);
#define XPRECEDENCE_ARRAY(x) XRECORD (x, precedence_array, struct precedence_array)
#define wrap_precedence_array(p) wrap_record (p, precedence_array)
#define PRECEDENCE_ARRAYP(x) RECORDP (x, precedence_array)
#define CHECK_PRECEDENCE_ARRAY(x) CHECK_RECORD (x, precedence_array)
#define CONCHECK_PRECEDENCE_ARRAY(x) CONCHECK_RECORD (x, precedence_array)

#define XPRECEDENCE_ARRAY_DYNARR(x) (XPRECEDENCE_ARRAY (x)->precdyn)

#endif /* MULE */

#endif /* INCLUDED_unicode_h_ */
