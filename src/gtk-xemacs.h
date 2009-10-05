/* gtk-xemacs.h
**
** Description: A widget to encapsulate a XEmacs 'text widget'
**
** Created by: William M. Perry
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
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

#ifndef __GTK_XEMACS_H__
#define __GTK_XEMACS_H__

#include <config.h>
#include "frame.h"
#include <gdk/gdk.h>
#include <gtk/gtkfixed.h>

BEGIN_C_DECLS

#define GTK_XEMACS(obj)			GTK_CHECK_CAST (obj, gtk_xemacs_get_type (), GtkXEmacs)
#define GTK_XEMACS_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_xemacs_get_type (), GtkXEmacsClass)
#define GTK_IS_XEMACS(obj)		GTK_CHECK_TYPE (obj, gtk_xemacs_get_type ())
#define GTK_XEMACS_FRAME(obj)	GTK_XEMACS (obj)->f

	typedef struct _GtkXEmacs GtkXEmacs;
	typedef struct _GtkXEmacsClass GtkXEmacsClass;

	struct _GtkXEmacs
	{
		GtkFixed fixed;
		struct frame *f;
	};

	struct _GtkXEmacsClass
	{
		GtkFixedClass parent_class;
	};

	guint gtk_xemacs_get_type (void);
	GtkWidget *gtk_xemacs_new (struct frame *f);

END_C_DECLS

#endif /* __GTK_XEMACS_H__ */
