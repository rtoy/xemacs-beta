/* gtk-xemacs.h
**
** Description: A widget to encapsulate a XEmacs 'text widget'
**
** Created by: William M. Perry
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
**
** This file is part of XEmacs.
**
** XEmacs is free software: you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation, either version 3 of the License, or (at your
** option) any later version.
** 
** XEmacs is distributed in the hope that it will be useful, but WITHOUT
** ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
** for more details.
** 
** You should have received a copy of the GNU General Public License
** along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef __GTK_XEMACS_H__
#define __GTK_XEMACS_H__

BEGIN_C_DECLS

#define GTK_TYPE_XEMACS            (gtk_xemacs_get_type ())
#define GTK_XEMACS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_XEMACS, GtkXEmacs))
#define GTK_XEMACS_CLASS(klass)	   (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_XEMACS, GtkXEmacsClass))
#define GTK_IS_XEMACS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_XEMACS))
#define GTK_IS_XEMACS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_XEMACS)
#define GTK_XEMACS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_XEMACS, GtkXEmacsClass))
#define GTK_XEMACS_FRAME(obj)	   GTK_XEMACS (obj)->f

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

GType gtk_xemacs_get_type (void);
GtkWidget *gtk_xemacs_new (struct frame *f);

END_C_DECLS

#endif /* __GTK_XEMACS_H__ */
