/* Lisp font data structures for X and Xft.

Copyright (C) 2003 Eric Knauel and Matthias Neubauer
Copyright (C) 2005 Eric Knauel
Copyright (C) 2004, 2005 Free Software Foundation, Inc.

Authors:	Eric Knauel <knauel@informatik.uni-tuebingen.de>
		Matthias Neubauer <neubauer@informatik.uni-freiburg.de>
		Stephen J. Turnbull <stephen@xemacs.org>
Created:	27 Oct 2003
Updated:	05 Mar 2005 by Stephen J. Turnbull

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

/* Synched up with: Not in GNU Emacs. */

/* This module provides the Lisp interface to fonts in X11, including Xft,
   but (at least at first) not GTK+ or Qt.

   It should be renamed to fonts-x.h.

   Sealevel code should be in ../lwlib/lwlib-fonts.h or
   ../lwlib/lwlib-colors.h.
*/


#ifndef INCLUDED_xft_fonts_h_
#define INCLUDED_xft_fonts_h_

#include "../lwlib/lwlib-fonts.h"
#include "../lwlib/lwlib-colors.h"

extern Fixnum debug_xft;

/* Standard for fontconfig.  Use a macro to show we're not guessing. */
#define Qxft_font_name_encoding Qutf_8

#define XE_XLFD_MAKE_LISP_STRING(s) (make_string(s, strlen(s)))

struct fc_pattern
{
  struct LCRECORD_HEADER header;
  FcPattern *fcpatPtr;
};

typedef struct fc_pattern fc_pattern;

DECLARE_LRECORD(fc_pattern, struct fc_pattern);
#define XFCPATTERN(x) XRECORD (x, fc_pattern, struct fc_pattern)
#define wrap_fcpattern(p) wrap_record (p, fc_pattern)
#define FCPATTERNP(x) RECORDP (x, fc_pattern)
#define CHECK_FCPATTERN(x) CHECK_RECORD (x, fc_pattern)
#define CONCHECK_FCPATTERN(x) CONCHECK_RECORD (x, fc_pattern)
#define XFCPATTERN_PTR(x) (XFCPATTERN(x)->fcpatPtr)

#endif /* INCLUDED_xft_fonts_h_ */
