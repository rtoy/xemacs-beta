/* mswindows-specific glyph objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996 Ben Wing
   Copyright (C) 1995 Sun Microsystems
   Copyright (C) 1998, 1999, 2000 Andy Piper.

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

/* taken from glyphs-*.c
   HINT HINT HINT Bill Perry: Please put code here and avoid massive
   duplication in *-gtk.c!!! */

#include <config.h>
#include "lisp.h"
#include "lstream.h"

#include "window.h"
#include "elhash.h"
#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"
#include "sysdep.h"
#include "sysfile.h"
#include "faces.h"
#include "imgproc.h"

Lisp_Object Q_resource_type, Q_resource_id;

void
shared_resource_validate (Lisp_Object instantiator)
{
  if ((NILP (find_keyword_in_vector (instantiator, Q_file))
       &&
       NILP (find_keyword_in_vector (instantiator, Q_resource_id)))
      ||
      NILP (find_keyword_in_vector (instantiator, Q_resource_type)))
    sferror ("Must supply :file, :resource-id and :resource-type",
	     instantiator);
}


Lisp_Object
shared_resource_normalize (Lisp_Object inst, Lisp_Object console_type,
			   Lisp_Object dest_mask, Lisp_Object tag)
{
  /* This function can call lisp */
  Lisp_Object file = Qnil;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object alist = Qnil;

  GCPRO2 (file, alist);

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data,
					     console_type);

  if (CONSP (file)) /* failure locating filename */
    signal_double_image_error ("Opening pixmap file",
			       "no such file or directory",
			       Fcar (file));

  if (NILP (file)) /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  {
    alist = remassq_no_quit (Q_file, alist);
    alist = Fcons (Fcons (Q_file, file), alist);
  }

  {
    Lisp_Object result = alist_to_tagged_vector (tag, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

void
syms_of_glyphs_shared (void)
{
  DEFKEYWORD (Q_resource_id);
  DEFKEYWORD (Q_resource_type);
}
