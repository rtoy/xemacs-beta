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


#ifndef INCLUDED_font_mgr_h_
#define INCLUDED_font_mgr_h_

#include "../lwlib/lwlib-fonts.h"
#include "../lwlib/lwlib-colors.h"

extern Fixnum debug_xft;

/* Standard for fontconfig.  Use a macro to show we're not guessing. */
#define Qfc_font_name_encoding Qutf_8

#define XE_XLFD_MAKE_LISP_STRING(s) (make_string(s, strlen(s)))

struct fc_pattern
{
  NORMAL_LISP_OBJECT_HEADER header;
  FcPattern *fcpatPtr;
};

typedef struct fc_pattern fc_pattern;

DECLARE_LISP_OBJECT(fc_pattern, struct fc_pattern);
#define XFC_PATTERN(x) XRECORD (x, fc_pattern, struct fc_pattern)
#define wrap_fc_pattern(p) wrap_record (p, fc_pattern)
#define FC_PATTERNP(x) RECORDP (x, fc_pattern)
#define CHECK_FC_PATTERN(x) CHECK_RECORD (x, fc_pattern)
#define CONCHECK_FC_PATTERN(x) CONCHECK_RECORD (x, fc_pattern)
#define XFC_PATTERN_PTR(x) (XFC_PATTERN(x)->fcpatPtr)

#define FONTCONFIG_EXPOSE_CONFIG
#ifdef FONTCONFIG_EXPOSE_CONFIG

struct fc_config
{
  NORMAL_LISP_OBJECT_HEADER header;
  FcConfig *fccfgPtr;
};

typedef struct fc_config fc_config;

DECLARE_LISP_OBJECT(fc_config, struct fc_config);
#define XFC_CONFIG(x) XRECORD (x, fc_config, struct fc_config)
#define wrap_fc_config(p) wrap_record (p, fc_config)
#define FC_CONFIGP(x) RECORDP (x, fc_config)
#define CHECK_FC_CONFIG(x) CHECK_RECORD (x, fc_config)
#define CONCHECK_FC_CONFIG(x) CONCHECK_RECORD (x, fc_config)
#define XFC_CONFIG_PTR(x) (XFC_CONFIG(x)->fccfgPtr)

#endif /* FONTCONFIG_EXPOSE_CONFIG */

#ifdef HAVE_XFT

#ifndef HAVE_FCCONFIGGETRESCANINTERVAL
/* Older fontconfig versions misspell this function name. */
#define FcConfigGetRescanInterval FcConfigGetRescanInverval 
#endif /* */

#ifndef HAVE_FCCONFIGSETRESCANINTERVAL
/* Older fontconfig versions misspell this function name. */
#define FcConfigSetRescanInterval FcConfigSetRescanInverval 
#endif /* */

/*
  The format of a fontname (as returned by fontconfig) is not well-documented,
  But the character repertoire is represented in an ASCII-compatible way.  See
  fccharset.c (FcCharSetUnparse).  So we can use UTF-8 for long names.

  Currently we have a hack where different versions of the unparsed name are
  used in different contexts fairly arbitrarily.  I don't think this is close
  to coherency; even without the charset and lang properties fontconfig names
  are too unwieldy to use.  We need to rethink the approach here.  I think
  probably Lisp_Font_Instance.name should contain the font name as specified
  to Lisp (almost surely much shorter than shortname, even, and most likely
  wildcarded), while Lisp_Font_Instance.truename should contain the longname.
  For now, I'm going to #ifdef the return values defaulting to short. -- sjt
*/

/*                DEBUGGING STUFF                */

/* print message to stderr: one internal-format string argument */
#define DEBUG_XFT0(level,s)		\
  if (debug_xft > level) stderr_out (s)

/* print message to stderr: one formatted argument */
#define DEBUG_XFT1(level,format,x1)		\
  if (debug_xft > level) stderr_out (format, x1)

/* print message to stderr: two formatted arguments */
#define DEBUG_XFT2(level,format,x1,x2)			\
  if (debug_xft > level) stderr_out (format, x1, x2)

/* print message to stderr: three formatted arguments */
#define DEBUG_XFT3(level,format,x1,x2,x3)			\
  if (debug_xft > level) stderr_out (format, x1, x2, x3)

/* print message to stderr: four formatted arguments */
#define DEBUG_XFT4(level,format,x1,x2,x3,x4)			\
  if (debug_xft > level) stderr_out (format, x1, x2, x3, x4)

/* print an Xft pattern to stderr
   LEVEL is the debug level (to compare to debug_xft)
   FORMAT is a newline-terminated printf format with one %s for the pattern
     and must be internal format (eg, pure ASCII)
   PATTERN is an FcPattern *. */
#define PRINT_XFT_PATTERN(level,format,pattern)			\
  do {								\
    DECLARE_EISTRING (eistrpxft_name);				\
    Extbyte *name = (Extbyte *) FcNameUnparse (pattern);	\
								\
    eicpy_ext(eistrpxft_name,					\
              name ? name : "FONT WITH NULL NAME",		\
              Qfc_font_name_encoding);				\
    DEBUG_XFT1 (level, format, eidata(eistrpxft_name));		\
    free (name);						\
  } while (0)

/* print a progress message
   LEVEL is the debug level (to compare to debug_xft)
   FONT is the Xft font name in Mule internal encoding (from an eistring).
   LANG is the language being checked for support (must be ASCII). */
#define CHECKING_LANG(level,font,lang)					\
  do {									\
    DEBUG_XFT2 (level, "checking if %s handles %s\n", font, lang);	\
  } while (0)

#else /* HAVE_XFT */

#endif /* HAVE_XFT */

#endif /* INCLUDED_font_mgr_h_ */
