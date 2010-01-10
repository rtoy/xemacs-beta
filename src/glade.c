/* glade.c
**
** Description: Interface to `libglade' for XEmacs/GTK
**
** Created by: William M. Perry <wmperry@gnu.org>
**
** Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
** Copyright (c) 2000 Free Software Foundation
**
** This file is part of XEmacs.
**
** XEmacs is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 2, or (at your option) any
** later version.
**
** XEmacs is distributed in the hope that it will be useful, but WITHOUT
** ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
** for more details.
**
** You should have received a copy of the GNU General Public License
** along with XEmacs; see the file COPYING.  If not, write to
** the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
** Boston, MA 02111-1301, USA.  */
*/

#if defined(HAVE_GLADE_H) || defined(HAVE_GLADE_GLADE_H)

/* For COMPILED_FUNCTIONP */
#include "bytecode.h"

#ifdef HAVE_GLADE_GLADE_H
#include <glade/glade.h>
#endif

#ifdef HAVE_GLADE_H
#include <glade.h>
#endif

/* This is based on the code from rep-gtk 0.11 in libglade-support.c */

static void
connector (const gchar *handler_name, GtkObject *object,
	   const gchar *signal_name, const gchar *signal_data,
	   GtkObject *connect_object, gboolean after, gpointer user_data)
{
  Lisp_Object func;
  Lisp_Object lisp_data = Qnil;

  func = VOID_TO_LISP (user_data);

  if (NILP (func))
    {
      /* Look for a lisp function called HANDLER_NAME */
      func = intern (handler_name);
    }

  if (signal_data && signal_data[0])
    {
      lisp_data
        = IGNORE_MULTIPLE_VALUES (Feval (Fread (build_string (signal_data))));
    }

  /* obj, name, func, cb_data, object_signal, after_p */
  Fgtk_signal_connect (build_gtk_object (object),
		       intern (signal_name),
		       func,
		       lisp_data,
		       connect_object ? Qt : Qnil,
		       after ? Qt : Qnil);
}

/* This differs from lisp/subr.el (functionp) definition by allowing
** symbol names that may not necessarily be fboundp yet.
*/
static int __almost_functionp (Lisp_Object obj)
{
  return (SYMBOLP (obj) ||
	  SUBRP (obj) ||
	  COMPILED_FUNCTIONP (obj) ||
	  EQ (Fcar_safe (obj), Qlambda));
}

DEFUN ("glade-xml-signal-connect", Fglade_xml_signal_connect, 3, 3, 0, /*
Connect a glade handler.
*/
       (xml, handler_name, func))
{
  CHECK_GTK_OBJECT (xml);
  CHECK_STRING (handler_name);

  if (!__almost_functionp (func))
    {
      func = wrong_type_argument (intern ("functionp"), func);
    }

  glade_xml_signal_connect_full (GLADE_XML (XGTK_OBJECT (xml)->object),
				 (char*) XSTRING_DATA (handler_name),
				 connector, LISP_TO_VOID (func));
  return (Qt);
}

DEFUN ("glade-xml-signal-autoconnect", Fglade_xml_signal_autoconnect, 1, 1, 0, /*
Connect all glade handlers.
*/
       (xml))
{
  CHECK_GTK_OBJECT (xml);

  glade_xml_signal_autoconnect_full (GLADE_XML (XGTK_OBJECT (xml)->object),
				     connector, LISP_TO_VOID (Qnil));
  return (Qt);
}

DEFUN ("glade-xml-textdomain", Fglade_xml_textdomain, 1, 1, 0, /*
Return the textdomain of a GladeXML object.
*/
       (xml))
{
  gchar *the_domain = NULL;

  CHECK_GTK_OBJECT (xml);

  if (!GLADE_IS_XML (XGTK_OBJECT (xml)->object))
    {
      wtaerror ("Object is not a GladeXML type.", xml);
    }

#ifdef LIBGLADE_XML_TXTDOMAIN
  the_domain = GLADE_XML (XGTK_OBJECT (xml)->object)->txtdomain;
#else
  the_domain = GLADE_XML (XGTK_OBJECT (xml)->object)->textdomain;
#endif  
  return (build_string (the_domain));
}

void syms_of_glade (void)
{
  DEFSUBR (Fglade_xml_signal_connect);
  DEFSUBR (Fglade_xml_signal_autoconnect);
  DEFSUBR (Fglade_xml_textdomain);
}

void vars_of_glade (void)
{
  Fprovide (intern ("glade"));
}

#else /* !(HAVE_GLADE_H || HAVE_GLADE_GLADE_H) */
#define syms_of_glade()
#define vars_of_glade()
#endif
