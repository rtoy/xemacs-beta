/* ui-byhand.c --- hand-coded GTK functions

Copyright (C) 2000, 2001 William M. Perry

   I really wish this entire file could go away, but there is
   currently no way to do the following in the Foreign Function
   Interface:

   1) Deal with return values in the parameter list (ie: int *foo)

   So we have to code a few functions by hand.  Ick.

   William M. Perry 5/8/00

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

#include "gui.h"
#include "ui-gtk.h"

DEFUN ("gtk-box-query-child-packing", Fgtk_box_query_child_packing, 2, 2,0, /*
Returns information about how CHILD is packed into BOX.
Return value is a list of (EXPAND FILL PADDING PACK_TYPE).
*/
       (box, child))
{
  gboolean expand, fill;
  guint padding;
  GtkPackType pack_type;
  Lisp_Object result = Qnil;
  
  CHECK_GTK_OBJECT (box);
  CHECK_GTK_OBJECT (child);

  if (!GTK_IS_BOX (XGTK_OBJECT (box)->object))
    {
      wtaerror ("Object is not a GtkBox", box);
    }

  if (!GTK_IS_WIDGET (XGTK_OBJECT (child)->object))
    {
      wtaerror ("Child is not a GtkWidget", child);
    }

  gtk_box_query_child_packing (GTK_BOX (XGTK_OBJECT (box)->object),
			       GTK_WIDGET (XGTK_OBJECT (child)->object),
			       &expand, &fill, &padding, &pack_type);

  result = Fcons (make_fixnum (pack_type), result);
  result = Fcons (make_fixnum (padding), result);
  result = Fcons (fill ? Qt : Qnil, result);
  result = Fcons (expand ? Qt : Qnil, result);

  return (result);
}

/*void	   gtk_calendar_get_date	(GtkCalendar *calendar, 
					 guint	     *year,
					 guint	     *month,
					 guint	     *day);
*/
DEFUN ("gtk-calendar-get-date", Fgtk_calendar_get_date, 1, 1, 0, /*
Return a list of (YEAR MONTH DAY) from the CALENDAR object.
*/
       (calendar))
{
  guint year, month, day;

  CHECK_GTK_OBJECT (calendar);

  if (!GTK_IS_CALENDAR (XGTK_OBJECT (calendar)->object))
    {
      wtaerror ("Not a GtkCalendar object", calendar);
    }

  gtk_calendar_get_date (GTK_CALENDAR (XGTK_OBJECT (calendar)->object),
			 &year, &month, &day);

  return (list3 (make_fixnum (year), make_fixnum (month), make_fixnum (day)));
}

/* void gtk_color_selection_get_color(GtkColorSelection *colorsel, gdouble *color); */
DEFUN ("gtk-color-selection-get-color", Fgtk_color_selection_get_color, 1, 1, 0, /*
Return a list of (RED GREEN BLUE ALPHA) from the GtkColorSelection OBJECT.
*/
       (object))
{
  gdouble rgba[4];

  CHECK_GTK_OBJECT (object);

  if (!GTK_IS_COLOR_SELECTION (XGTK_OBJECT (object)->object))
    {
      wtaerror ("Object is not a GtkColorSelection", object);
    }

  gtk_color_selection_get_color (GTK_COLOR_SELECTION (XGTK_OBJECT (object)), rgba);

  return (list4 (make_float (rgba[0]),
		 make_float (rgba[1]),
		 make_float (rgba[2]),
		 make_float (rgba[3])));
}

/* (gtk-import-function nil "gtk_editable_insert_text" 'GtkEditable 'GtkString 'gint 'pointer-to-gint) */
DEFUN ("gtk-editable-insert-text", Fgtk_editable_insert_text, 3, 3, 0, /*
Insert text STRINT at POS in GtkEditable widget OBJ.
Returns the new position of the cursor in the widget.
*/
       (obj, string, pos))
{
  gint the_pos;

  CHECK_GTK_OBJECT (obj);
  CHECK_STRING (string);
  CHECK_FIXNUM (pos);

  the_pos = XFIXNUM (pos);

  if (!GTK_IS_EDITABLE (XGTK_OBJECT (obj)->object))
    {
      wtaerror ("Object is not a GtkEditable", obj);
    }

  gtk_editable_insert_text (GTK_EDITABLE (XGTK_OBJECT (obj)->object),
			    (char *) XSTRING_DATA (string),
			    XSTRING_LENGTH (string),
			    &the_pos);

  return (make_fixnum (the_pos));
}

DEFUN ("gtk-pixmap-get", Fgtk_pixmap_get, 1, 1, 0, /*
Return a cons cell of (PIXMAP . MASK) from GtkPixmap OBJECT.
*/
	 (object))
{
  GdkPixmap *pixmap, *mask;

  CHECK_GTK_OBJECT (object);

  if (!GTK_IS_PIXMAP (XGTK_OBJECT (object)->object))
    {
      wtaerror ("Object is not a GtkPixmap", object);
    }

  gtk_pixmap_get (GTK_PIXMAP (XGTK_OBJECT (object)->object), &pixmap, &mask);

  return (Fcons (pixmap ? build_gtk_object (G_OBJECT (pixmap)) : Qnil,
		 mask ? build_gtk_object (G_OBJECT (mask)) : Qnil));
}

DEFUN ("gtk-curve-get-vector", Fgtk_curve_get_vector, 2, 2, 0, /*
Returns a vector of LENGTH points representing the curve of CURVE.
*/
       (curve, length))
{
  gfloat *vector = NULL;
  Lisp_Object lisp_vector = Qnil;
  int i;

  CHECK_GTK_OBJECT (curve);
  CHECK_FIXNUM (length);

  if (!GTK_IS_CURVE (XGTK_OBJECT (curve)->object))
    {
      wtaerror ("Object is not a GtkCurve", curve);
    }

  vector = alloca_array (gfloat, XFIXNUM (length));

  gtk_curve_get_vector (GTK_CURVE (XGTK_OBJECT (curve)->object), XFIXNUM (length), vector);
  lisp_vector = make_vector (XFIXNUM (length), Qnil);

  for (i = 0; i < XFIXNUM (length); i++)
    {
      XVECTOR_DATA (lisp_vector)[i] = make_float (vector[i]);
    }

  return (lisp_vector);
}

DEFUN ("gtk-curve-set-vector", Fgtk_curve_set_vector, 2, 2, 0, /*
Set the vector of points on CURVE to VECTOR.
*/
       (curve, vector))
{
  gfloat *c_vector = NULL;
  int vec_length = 0;
  int i;

  CHECK_GTK_OBJECT (curve);
  CHECK_VECTOR (vector);

  vec_length = XVECTOR_LENGTH (vector);

  if (!GTK_IS_CURVE (XGTK_OBJECT (curve)->object))
    {
      wtaerror ("Object is not a GtkCurve", curve);
    }

  c_vector = alloca_array (gfloat, vec_length);

  for (i = 0; i < vec_length; i++)
    {
      CHECK_FLOAT (XVECTOR_DATA (vector)[i]);
      c_vector[i] = extract_float (XVECTOR_DATA (vector)[i]);
    }

  gtk_curve_set_vector (GTK_CURVE (XGTK_OBJECT (curve)->object), vec_length, c_vector);
  return (Qt);
}

DEFUN ("gtk-label-get", Fgtk_label_get, 1, 1, 0, /*
Return the text of LABEL.
*/
       (label))
{
  gchar *string;

  CHECK_GTK_OBJECT (label);

  if (!GTK_IS_LABEL (XGTK_OBJECT (label)->object))
    {
      wtaerror ("Object is not a GtkLabel", label);
    }

  gtk_label_get (GTK_LABEL (XGTK_OBJECT (label)->object), &string);

  return (build_cistring (string));
}

DEFUN ("gtk-notebook-query-tab-label-packing", Fgtk_notebook_query_tab_label_packing, 2, 2, 0, /*
Return a list of packing information (EXPAND FILL PACK_TYPE) for CHILD in NOTEBOOK.
*/
       (notebook, child))
{
  gboolean expand, fill;
  GtkPackType pack_type;

  CHECK_GTK_OBJECT (notebook);
  CHECK_GTK_OBJECT (child);

  if (!GTK_IS_NOTEBOOK (XGTK_OBJECT (notebook)->object))
    {
      wtaerror ("Object is not a GtkNotebook", notebook);
    }

  if (!GTK_IS_WIDGET (XGTK_OBJECT (child)->object))
    {
      wtaerror ("Object is not a GtkWidget", child);
    }

  gtk_notebook_query_tab_label_packing (GTK_NOTEBOOK (XGTK_OBJECT (notebook)->object),
					GTK_WIDGET (XGTK_OBJECT (child)->object),
					&expand, &fill, &pack_type);

  return (list3 (expand ? Qt : Qnil, fill ? Qt : Qnil, make_fixnum (pack_type)));
}

DEFUN ("gtk-widget-get-pointer", Fgtk_widget_get_pointer, 1, 1, 0, /*
Return the pointer position relative to WIDGET as a cons of (X . Y).
*/
       (widget))
{
  gint x,y;
  CHECK_GTK_OBJECT (widget);

  if (!GTK_IS_WIDGET (XGTK_OBJECT (widget)->object))
    {
      wtaerror ("Object is not a GtkWidget", widget);
    }

  gtk_widget_get_pointer (GTK_WIDGET (XGTK_OBJECT (widget)->object), &x, &y);

  return (Fcons (make_fixnum (x), make_fixnum (y)));
}

/* This is called whenever an item with a GUI_ID associated with it is
   destroyed.  This allows us to remove the references in gui-gtk.c
   that made sure callbacks and such were GCPRO-ed
*/
static void
__remove_gcpro_by_id (gpointer user_data, GObject * UNUSED (old_location))
{
  ungcpro_popup_callbacks ((GUI_ID) GPOINTER_TO_UINT (user_data));
}

static void
__generic_toolbar_callback (GtkWidget *UNUSED (item), gpointer user_data)
{
  Lisp_Object callback;
  Lisp_Object lisp_user_data;

  callback = GET_LISP_FROM_VOID (user_data);

  lisp_user_data = XCAR (callback);
  callback = XCDR (callback);

  signal_special_gtk_user_event (Qnil, callback, lisp_user_data);
}


DEFUN ("gtk-widget-size-request", Fgtk_widget_size_request, 1, 1, 0, /*
Sets WIDGET size request to WIDTH by HEIGHT.
*/
       (widget))
{
  GtkRequisition req;
  
  CHECK_GTK_OBJECT (widget);

  if (!GTK_IS_WIDGET (XGTK_OBJECT (widget)->object))
    wtaerror ("Object is not a GtkWidget", widget);

  gtk_widget_size_request ((GtkWidget *) (XGTK_OBJECT (widget)->object), &req);
  return cons3 (make_fixnum (req.width), make_fixnum (req.height), Qnil);
}

#ifdef JSPARKES

/* GtkCTree is an abomination in the eyes of the object system. */
static void
__emacs_gtk_ctree_recurse_internal (GtkCTree *ctree, GtkCTreeNode *node, gpointer user_data)
{
  Lisp_Object closure;

  closure = GET_LISP_FROM_VOID (user_data);

  call3 (XCAR (closure),
	 build_gtk_object (G_OBJECT (ctree)),
	 build_gtk_boxed (node, G_TYPE_CTREE_NODE),
	 XCDR (closure));
}

DEFUN ("gtk-ctree-recurse", Fgtk_ctree_recurse, 3, 6, 0, /*
Recursively apply FUNC to all nodes of CTREE at or below NODE.
FUNC is called with three arguments: CTREE, a GtkCTreeNode, and DATA.
The return value of FUNC is ignored.

If optional 5th argument CHILDFIRSTP is non-nil, then
the function is called for each node after it has been
called for that node's children.

Optional 6th argument DEPTH limits how deeply to recurse.

This function encompasses all the following Gtk functions:

void gtk_ctree_post_recursive                    (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_post_recursive_to_depth           (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  gint          depth,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_pre_recursive                     (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_pre_recursive_to_depth            (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  gint          depth,
						  GtkCTreeFunc  func,
						  gpointer      data);
*/
       (ctree, node, func, data, childfirstp, depth))
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object closure = Qnil;

  CHECK_GTK_OBJECT (ctree);

  if (!NILP (node))
    {
      CHECK_GTK_BOXED (node);
    }

  if (!NILP (depth))
    {
      CHECK_FIXNUM (depth);
    }

  closure = Fcons (func, data);

  GCPRO3 (ctree, node, closure);

  if (NILP (depth))
    {
      (NILP (childfirstp) ? gtk_ctree_post_recursive : gtk_ctree_pre_recursive)
	(GTK_CTREE (XGTK_OBJECT (ctree)->object),
	 NILP (node) ? NULL : (GtkCTreeNode *) XGTK_BOXED (node)->object,
	 __emacs_gtk_ctree_recurse_internal,
	 STORE_LISP_IN_VOID (closure));
    }
  else
    {
      (NILP (childfirstp) ? gtk_ctree_post_recursive_to_depth : gtk_ctree_pre_recursive_to_depth)
	(GTK_CTREE (XGTK_OBJECT (ctree)->object),
	 NILP (node) ? NULL : (GtkCTreeNode *) XGTK_BOXED (node)->object,
	 XFIXNUM (depth),
	 __emacs_gtk_ctree_recurse_internal,
	 STORE_LISP_IN_VOID (closure));
    }

  UNGCPRO;
  return (Qnil);
}

#endif
void syms_of_ui_byhand (void)
{
  DEFSUBR (Fgtk_box_query_child_packing);
  DEFSUBR (Fgtk_calendar_get_date);
  DEFSUBR (Fgtk_color_selection_get_color);
  DEFSUBR (Fgtk_editable_insert_text);
  DEFSUBR (Fgtk_pixmap_get);
  DEFSUBR (Fgtk_curve_get_vector);
  DEFSUBR (Fgtk_curve_set_vector);
  DEFSUBR (Fgtk_label_get);
  DEFSUBR (Fgtk_notebook_query_tab_label_packing);
  DEFSUBR (Fgtk_widget_get_pointer);
  DEFSUBR (Fgtk_widget_size_request);
#ifdef JSPARKES
  DEFSUBR (Fgtk_ctree_recurse);
#endif
}
