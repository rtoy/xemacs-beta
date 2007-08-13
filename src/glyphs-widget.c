/* Widget-specific glyph objects.
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

/* written by Andy Piper <andy@xemacs.org> */

#include <config.h>
#include "lisp.h"
#include "lstream.h"
#include "console.h"
#include "device.h"
#include "faces.h"
#include "glyphs.h"
#include "objects.h"
#include "bytecode.h"
#include "window.h"
#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"

DEFINE_IMAGE_INSTANTIATOR_FORMAT (button);
DEFINE_IMAGE_INSTANTIATOR_FORMAT (combo_box);
Lisp_Object Qcombo_box;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (edit_field);
Lisp_Object Qedit_field;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (scrollbar);
Lisp_Object Qscrollbar;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (widget);
DEFINE_IMAGE_INSTANTIATOR_FORMAT (label);
Lisp_Object Qlabel;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (progress_gauge);
Lisp_Object Qprogress_gauge;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (tree_view);
Lisp_Object Qtree_view;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (tab_control);
Lisp_Object Qtab_control;
DEFINE_IMAGE_INSTANTIATOR_FORMAT (layout);
Lisp_Object Qlayout;

Lisp_Object Q_descriptor, Q_height, Q_width, Q_properties, Q_items;
Lisp_Object Q_image, Q_text, Q_percent, Q_orientation, Q_justify, Q_border;
Lisp_Object Qetched_in, Qetched_out, Qbevel_in, Qbevel_out;

#ifdef DEBUG_WIDGETS
int debug_widget_instances;
#endif

/* TODO:
   - more complex controls.
   - tooltips for controls, especially buttons.
 */

/* In MS-Windows normal windows work in pixels, dialog boxes work in
   dialog box units. Why? sigh. We could reuse the metrics for dialogs
   if this were not the case. As it is we have to position things
   pixel wise. I'm not even sure that X has this problem at least for
   buttons in groups. */
static int
widget_possible_dest_types (void)
{
  return IMAGE_WIDGET_MASK;
}

static void
check_valid_glyph_or_instantiator (Lisp_Object data)
{
  Lisp_Object glyph = data;
  if (SYMBOLP (data))
    glyph = XSYMBOL (data)->value;

  if (IMAGE_INSTANCEP (glyph))
    CHECK_IMAGE_INSTANCE (glyph);
  else if (!CONSP (glyph) && !VECTORP (glyph))
    CHECK_BUFFER_GLYPH (glyph);
}

static void
check_valid_orientation (Lisp_Object data)
{
  if (!EQ (data, Qhorizontal)
      &&
      !EQ (data, Qvertical))
    signal_simple_error ("unknown orientation for layout", data);
}

static void
check_valid_tab_orientation (Lisp_Object data)
{
  if (!EQ (data, Qtop)
      &&
      !EQ (data, Qbottom)
      &&
      !EQ (data, Qleft)
      &&
      !EQ (data, Qright))
    signal_simple_error ("unknown orientation for tab control", data);
}

static void
check_valid_justification (Lisp_Object data)
{
  if (!EQ (data, Qleft) && !EQ (data, Qright) && !EQ (data, Qcenter))
    signal_simple_error ("unknown justification for layout", data);
}

static void
check_valid_border (Lisp_Object data)
{
  if (!EQ (data, Qt) && !EQ (data, Qetched_in) && !EQ (data, Qetched_out)
      && !EQ (data, Qbevel_in) && !EQ (data, Qbevel_out)
      && !GLYPHP (data) && !VECTORP (data))
    signal_simple_error ("unknown border style for layout", data);
}

static void
check_valid_anything (Lisp_Object data)
{
}

static void
check_valid_callback (Lisp_Object data)
{
    if (!SYMBOLP (data)
	&& !COMPILED_FUNCTIONP (data)
	&& !CONSP (data))
    {
	signal_simple_error (":callback must be a function or expression", data);
    }
}

static void
check_valid_symbol (Lisp_Object data)
{
    CHECK_SYMBOL (data);
}

static void
check_valid_string_or_vector (Lisp_Object data)
{
    if (!STRINGP (data) && !VECTORP (data))
	signal_simple_error (":descriptor must be a string or a vector", data);
}

void
check_valid_item_list_1 (Lisp_Object items)
{
  Lisp_Object rest;

  CHECK_LIST (items);
  EXTERNAL_LIST_LOOP (rest, items)
    {
      if (STRINGP (XCAR (rest)))
	CHECK_STRING (XCAR (rest));
      else if (VECTORP (XCAR (rest)))
	gui_parse_item_keywords (XCAR (rest));
      else if (LISTP (XCAR (rest)))
	check_valid_item_list_1 (XCAR (rest));
      else
	signal_simple_error ("Items must be vectors, lists or strings", items);
    }
}

static void
check_valid_item_list (Lisp_Object data)
{
  Lisp_Object items;

  Fcheck_valid_plist (data);
  items = Fplist_get (data, Q_items, Qnil);

  check_valid_item_list_1 (items);
}

static void
check_valid_glyph_or_instantiator_list (Lisp_Object data)
{
  Lisp_Object rest;

  CHECK_LIST (data);
  EXTERNAL_LIST_LOOP (rest, data)
    {
      check_valid_glyph_or_instantiator (XCAR (rest));
    }
}

static Lisp_Object
glyph_instantiator_to_glyph (Lisp_Object sym)
{
  /* This function calls lisp. */
  Lisp_Object glyph = sym;
  struct gcpro gcpro1;
	  
  GCPRO1 (glyph);
  /* if we have a symbol get at the actual data */
  if (SYMBOLP (glyph))
    glyph = XSYMBOL (glyph)->value;
	  
  if (CONSP (glyph))
    glyph = Feval (glyph);

  /* Be really helpful to the user. */
  if (VECTORP (glyph))
    {
      glyph = call1 (intern ("make-glyph"), glyph);
    }

  /* substitute the new glyph */
  RETURN_UNGCPRO (glyph);
}

static void 
substitute_keyword_value (Lisp_Object inst, Lisp_Object key, Lisp_Object val)
{
  int i;
  /* substitute the new glyph */
  for (i = 0; i < XVECTOR_LENGTH (inst); i++)
    {
      if (EQ (key, XVECTOR_DATA (inst)[i]))
	{
	  XVECTOR_DATA (inst)[i+1] = val;
	  break;
	}
    }
}

/* Wire widget property invocations to specific widgets. The problem
   we are solving here is that when instantiators get converted to
   instances they lose some type information (they just become
   subwindows or widgets for example). For widgets we need to preserve
   this type information so that we can do widget specific operations
   on the instances. This is encoded in the widget type
   field. widget_property gets invoked by decoding the primary type
   (Qwidget), <widget>_property then invokes based on the secondary
   type (Qedit_field for example). It is debatable whether we should
   wire things in this generalised way rather than treating widgets
   specially in image_instance_property. */
static Lisp_Object 
widget_property (Lisp_Object image_instance, Lisp_Object prop)
{
  struct Lisp_Image_Instance* ii = XIMAGE_INSTANCE (image_instance);
  struct image_instantiator_methods* meths;

  /* first see if its a general property ... */
  if (!NILP (Fplist_member (IMAGE_INSTANCE_WIDGET_PROPS (ii), prop)))
    return Fplist_get (IMAGE_INSTANCE_WIDGET_PROPS (ii), prop, Qnil);

  /* .. then try device specific methods ... */
  meths = decode_device_ii_format (IMAGE_INSTANCE_DEVICE (ii), 
				   IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				   ERROR_ME_NOT);
  if (meths && HAS_IIFORMAT_METH_P (meths, property))
    return IIFORMAT_METH (meths, property, (image_instance, prop));
  /* ... then format specific methods ... */
  meths = decode_device_ii_format (Qnil, IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				   ERROR_ME_NOT);
  if (meths && HAS_IIFORMAT_METH_P (meths, property))
    return IIFORMAT_METH (meths, property, (image_instance, prop));
  /* ... then fail */
  return Qunbound;
}

static Lisp_Object 
widget_set_property (Lisp_Object image_instance, Lisp_Object prop, Lisp_Object val)
{
  struct Lisp_Image_Instance* ii = XIMAGE_INSTANCE (image_instance);
  struct image_instantiator_methods* meths;
  Lisp_Object ret;

  /* PIck up any generic properties that we might need to keep hold
     of. */
  if (EQ (prop, Q_text))
    {
      IMAGE_INSTANCE_WIDGET_TEXT (ii) = val;
    }

  /* Now try device specific methods first ... */
  meths = decode_device_ii_format (IMAGE_INSTANCE_DEVICE (ii), 
				   IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				   ERROR_ME_NOT);
  if (meths && HAS_IIFORMAT_METH_P (meths, set_property)
      &&
      !UNBOUNDP (ret = 
		 IIFORMAT_METH (meths, set_property, (image_instance, prop, val))))
    {
      return ret;
    }
  /* ... then format specific methods ... */
  meths = decode_device_ii_format (Qnil, IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				   ERROR_ME_NOT);
  if (meths && HAS_IIFORMAT_METH_P (meths, set_property)
      &&
      !UNBOUNDP (ret = 
		 IIFORMAT_METH (meths, set_property, (image_instance, prop, val))))
    {
      return ret;
    }
  /* we didn't do any device specific properties, so shove the property in our plist */
  IMAGE_INSTANCE_WIDGET_PROPS (ii)
    = Fplist_put (IMAGE_INSTANCE_WIDGET_PROPS (ii), prop, val);
  return val;
}

/* Query for a widgets desired geometry. If no type specific method is
   provided then use the widget text to calculate sizes. */
static void 
widget_query_geometry (Lisp_Object image_instance, 
		       unsigned int* width, unsigned int* height,
		       enum image_instance_geometry disp, Lisp_Object domain)
{
  struct Lisp_Image_Instance* ii = XIMAGE_INSTANCE (image_instance);
  struct image_instantiator_methods* meths;

  /* First just set up what we already have. */
  if (width)	*width = IMAGE_INSTANCE_WIDTH (ii);
  if (height)	*height = IMAGE_INSTANCE_HEIGHT (ii);
  
  if (IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii)
      ||
      IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii))
    {
      /* .. then try device specific methods ... */
      meths = decode_device_ii_format (IMAGE_INSTANCE_DEVICE (ii), 
				       IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				       ERROR_ME_NOT);
      if (meths && HAS_IIFORMAT_METH_P (meths, query_geometry))
	IIFORMAT_METH (meths, query_geometry, (image_instance, 
					       width, height, disp,
					       domain));
      else
	{
	  /* ... then format specific methods ... */
	  meths = decode_device_ii_format (Qnil, IMAGE_INSTANCE_WIDGET_TYPE (ii), 
					   ERROR_ME_NOT);
	  if (meths && HAS_IIFORMAT_METH_P (meths, query_geometry))
	    IIFORMAT_METH (meths, query_geometry, (image_instance, 
						   width, height, disp,
						   domain));
	  else 
	    {
	      unsigned int w, h;
	      
	      /* Then if we are allowed to resize the widget, make the
		 size the same as the text dimensions. */
	      query_string_geometry (IMAGE_INSTANCE_WIDGET_TEXT (ii),
				     IMAGE_INSTANCE_WIDGET_FACE (ii),
				     &w, &h, 0, domain);
	      /* Adjust the size for borders. */
	      if (IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii))
		*width = w + 2 * WIDGET_BORDER_WIDTH;
	      if (IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii))
		*height = h +  2 * WIDGET_BORDER_HEIGHT;
	    }
	}
    }
}

static void 
widget_layout (Lisp_Object image_instance, 
	       unsigned int width, unsigned int height, Lisp_Object domain)
{
  struct Lisp_Image_Instance* ii = XIMAGE_INSTANCE (image_instance);
  struct image_instantiator_methods* meths;

  /* .. then try device specific methods ... */
  meths = decode_device_ii_format (IMAGE_INSTANCE_DEVICE (ii), 
				   IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				   ERROR_ME_NOT);
  if (meths && HAS_IIFORMAT_METH_P (meths, layout))
    IIFORMAT_METH (meths, layout, (image_instance, 
				   width, height, domain));
  else
    {
      /* ... then format specific methods ... */
      meths = decode_device_ii_format (Qnil, IMAGE_INSTANCE_WIDGET_TYPE (ii), 
				       ERROR_ME_NOT);
      if (meths && HAS_IIFORMAT_METH_P (meths, layout))
	IIFORMAT_METH (meths, layout, (image_instance, 
				       width, height, domain));
    }
}

static void
widget_validate (Lisp_Object instantiator)
{
  Lisp_Object desc = find_keyword_in_vector (instantiator, Q_descriptor);

  if (NILP (desc))
    signal_simple_error ("Must supply :descriptor", instantiator);

  if (VECTORP (desc))
    gui_parse_item_keywords (desc);

  if (!NILP (find_keyword_in_vector (instantiator, Q_width))
      && !NILP (find_keyword_in_vector (instantiator, Q_pixel_width)))
    signal_simple_error ("Must supply only one of :width and :pixel-width", instantiator);

  if (!NILP (find_keyword_in_vector (instantiator, Q_height))
	     && !NILP (find_keyword_in_vector (instantiator, Q_pixel_height)))
    signal_simple_error ("Must supply only one of :height and :pixel-height", instantiator);
}

static void
combo_box_validate (Lisp_Object instantiator)
{
  widget_validate (instantiator);
  if (NILP (find_keyword_in_vector (instantiator, Q_properties)))
    signal_simple_error ("Must supply item list", instantiator);
}

/* we need to convert things like glyphs to images, eval expressions
   etc.*/
static Lisp_Object
widget_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  /* This function can call lisp */
  Lisp_Object glyph = find_keyword_in_vector (inst, Q_image);

  /* we need to eval glyph if its an expression, we do this for the
     same reasons we normalize file to data. */
  if (!NILP (glyph))
    {
      substitute_keyword_value (inst, Q_image, glyph_instantiator_to_glyph (glyph));
    }

  return inst;
}

static void
initialize_widget_image_instance (struct Lisp_Image_Instance *ii, Lisp_Object type)
{
  /*  initialize_subwindow_image_instance (ii);*/
  IMAGE_INSTANCE_WIDGET_TYPE (ii) = type;
  IMAGE_INSTANCE_WIDGET_PROPS (ii) = Qnil;
  SET_IMAGE_INSTANCE_WIDGET_FACE (ii, Qnil);
  IMAGE_INSTANCE_WIDGET_ITEMS (ii) = allocate_gui_item ();
  IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii) = 1;
  IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii) = 1;
  IMAGE_INSTANCE_SUBWINDOW_ORIENT (ii) = 0;
  IMAGE_INSTANCE_SUBWINDOW_JUSTIFY (ii) = 0;
}

/* Instantiate a button widget. Unfortunately instantiated widgets are
   particular to a frame since they need to have a parent. It's not
   like images where you just select the image into the context you
   want to display it in and BitBlt it. So image instances can have a
   many-to-one relationship with things you see, whereas widgets can
   only be one-to-one (i.e. per frame) */
void
widget_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		    Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		    int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object face = find_keyword_in_vector (instantiator, Q_face);
  Lisp_Object height = find_keyword_in_vector (instantiator, Q_height);
  Lisp_Object width = find_keyword_in_vector (instantiator, Q_width);
  Lisp_Object pixwidth = find_keyword_in_vector (instantiator, Q_pixel_width);
  Lisp_Object pixheight = find_keyword_in_vector (instantiator, Q_pixel_height);
  Lisp_Object desc = find_keyword_in_vector (instantiator, Q_descriptor);
  Lisp_Object glyph = find_keyword_in_vector (instantiator, Q_image);
  Lisp_Object props = find_keyword_in_vector (instantiator, Q_properties);
  Lisp_Object orient = find_keyword_in_vector (instantiator, Q_orientation);
  int pw=0, ph=0, tw=0, th=0;
  
  /* this just does pixel type sizing */
  subwindow_instantiate (image_instance, instantiator, pointer_fg, pointer_bg,
			 dest_mask, domain);

  if (!(dest_mask & IMAGE_WIDGET_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_WIDGET_MASK);

  initialize_widget_image_instance (ii, XVECTOR_DATA (instantiator)[0]);

  /* retrieve the fg and bg colors */
  if (!NILP (face))
    SET_IMAGE_INSTANCE_WIDGET_FACE (ii, Fget_face (face));
  
  /* data items for some widgets */
  IMAGE_INSTANCE_WIDGET_PROPS (ii) = props;

  /* Pick up the orientation before we do our first layout. */
  if (EQ (orient, Qleft) || EQ (orient, Qright) || EQ (orient, Qvertical))
    IMAGE_INSTANCE_SUBWINDOW_ORIENT (ii) = 1;

  /* retrieve the gui item information. This is easy if we have been
     provided with a vector, more difficult if we have just been given
     keywords */
  if (STRINGP (desc) || NILP (desc))
    {
      /* big cheat - we rely on the fact that a gui item looks like an instantiator */
      IMAGE_INSTANCE_WIDGET_ITEMS (ii) = 
	gui_parse_item_keywords_no_errors (instantiator);
      IMAGE_INSTANCE_WIDGET_TEXT (ii) = desc;
    }
  else
    IMAGE_INSTANCE_WIDGET_ITEMS (ii) =
      gui_parse_item_keywords_no_errors (desc);

  /* parse more gui items out of the properties */
  if (!NILP (props))
    {
      Lisp_Object items = Fplist_get (props, Q_items, Qnil);
      if (!NILP (items))
	IMAGE_INSTANCE_WIDGET_ITEMS (ii) = 
	  Fcons (IMAGE_INSTANCE_WIDGET_ITEMS (ii), 
		 parse_gui_item_tree_children (items));
    }

  /* Normalize size information. We now only assign sizes if the user
     gives us some explicitly, or there are some constraints that we
     can't change later on. Otherwise we postpone sizing until query
     geometry gets called. */
  if (!NILP (pixwidth))		/* pixwidth takes precendent */
    {
      pw = XINT (pixwidth);
      IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii) = 0;
    }
  else if (!NILP (width))
    {
      tw = XINT (width);
      IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii) = 0;
    }

  if (!NILP (pixheight))
    {
      ph = XINT (pixheight);
      IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii) = 0;
    }
  else if (!NILP (height) && XINT (height) > 1)
    {
      th = XINT (height);
      IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii) = 0;
    }

  /* Taking the default face information when the user has specified
     size in characters is probably as good as any since the widget
     face is more likely to be proportional and thus give inadequate
     results. Using character sizes can only ever be approximate
     anyway. */
  if (tw || th)
    {
      int charwidth, charheight;
      default_face_font_info (domain, 0, 0, &charheight, &charwidth, 0);
      if (tw)
	pw = charwidth * tw;
      if (th)
	ph = charheight * th;
    }

  /* for a widget with an image pick up the dimensions from that */
  if (!NILP (glyph))
    {
      if (!pw)
	pw = glyph_width (glyph, domain) + 2 * WIDGET_BORDER_WIDTH;
      if (!ph)
	ph = glyph_height (glyph, domain) + 2 * WIDGET_BORDER_HEIGHT;
      IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP (ii) = 0;
      IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP (ii) = 0;
    }

  /* have to set the type this late in case there is no device
     instantiation for a widget */
  IMAGE_INSTANCE_TYPE (ii) = IMAGE_WIDGET;

  /* When we create the widgets the window system expects a valid
     size, so If we still don' t have sizes, call layout to pick them
     up. If query_geometry or layout relies on the widget being in
     existence then we are in catch 22. */
  image_instance_layout (image_instance, 
			 pw ? pw : IMAGE_UNSPECIFIED_GEOMETRY,
			 ph ? ph : IMAGE_UNSPECIFIED_GEOMETRY,
			 domain);
  /* Layout has already been done so we don't need to re-layout. */
  IMAGE_INSTANCE_DIRTYP (ii) = 0;

#ifdef DEBUG_WIDGETS
  debug_widget_instances++;
  stderr_out ("instantiated ");
  debug_print (instantiator);
  stderr_out ("%d widgets instantiated\n", debug_widget_instances);
#endif
}

/* tree-view geometry - get the height right */
static void
tree_view_query_geometry (Lisp_Object image_instance, 
			  unsigned int* width, unsigned int* height,
			  enum image_instance_geometry disp, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object items = IMAGE_INSTANCE_WIDGET_ITEMS (ii);

  
  if (*width)
    {
      /* #### what should this be. reconsider when X has tree views. */
      query_string_geometry (IMAGE_INSTANCE_WIDGET_TEXT (ii),
			     IMAGE_INSTANCE_WIDGET_FACE (ii),
			     width, 0, 0, domain);
    }
  if (*height)
    {
      int len, h;
      default_face_font_info (domain, 0, 0, &h, 0, 0);
      GET_LIST_LENGTH (items, len);
      *height = len * h;
    }
}

/* Get the geometry of a tab control. This is based on the number of
   items and text therin in the tab control. */
static void
tab_control_query_geometry (Lisp_Object image_instance, 
			    unsigned int* width, unsigned int* height,
			    enum image_instance_geometry disp, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object items = IMAGE_INSTANCE_WIDGET_ITEMS (ii);
  Lisp_Object rest;
  unsigned int tw = 0, th = 0;

  LIST_LOOP (rest, items)
    {
      unsigned int h, w;

      query_string_geometry (XGUI_ITEM (XCAR (rest))->name,
			     IMAGE_INSTANCE_WIDGET_FACE (ii),
			     &w, &h, 0, domain);
      tw += 2 * WIDGET_BORDER_WIDTH; /* some bias */
      tw += w;
      th = max (th, h + 2 * WIDGET_BORDER_HEIGHT);
    }

  /* Fixup returned values depending on orientation. */
  if (IMAGE_INSTANCE_SUBWINDOW_ORIENT (ii))
    {
      if (height)	*height = tw;
      if (width)	*width = th;
    }
  else
    {
      if (height)	*height = th;
      if (width)	*width = tw;
    }
}


/*****************************************************************************
 *                              widget layout                               *
 *****************************************************************************/
static int
layout_possible_dest_types (void)
{
  return IMAGE_LAYOUT_MASK;
}

/* we need to convert things like glyphs to images, eval expressions
   etc.*/
static Lisp_Object
layout_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  /* This function can call lisp */
  Lisp_Object items = find_keyword_in_vector (inst, Q_items);
  Lisp_Object border = find_keyword_in_vector (inst, Q_border);
  /* we need to eval glyph if its an expression, we do this for the
     same reasons we normalize file to data. */
  if (!NILP (items))
    {
      Lisp_Object rest;
      LIST_LOOP (rest, items)
	{
	  /* substitute the new glyph */
	  Fsetcar (rest, glyph_instantiator_to_glyph (XCAR (rest)));
	}
    }
  /* normalize the border spec. */
  if (VECTORP (border) || CONSP (border))
    {
      substitute_keyword_value (inst, Q_border, glyph_instantiator_to_glyph (border));
    }
  return inst;
}

/* Instantiate a layout widget. Sizing commentary: we have a number of
   problems that we would like to address. Some consider some of these
   more important than others. Currently size information is
   determined at instantiation time and is then fixed forever
   after. Generally this is not what we want. Users want size to be
   "big enough" to accommodate whatever they are trying to show and
   this is dependent on text length, lines, font metrics etc. Of
   course these attributes can change dynamically and so the size
   should changed dynamically also. Only in a few limited cases should
   the size be fixed and remain fixed. Of course this actually means
   that we don't really want to specifiy the size *at all* for most
   widgets - we want it to be discovered dynamically. Thus we can
   envisage the following scenarios:
   
   1. A button is sized to accommodate its text, the text changes and the
   button should change size also.  

   2. A button is given an explicit size. Its size should never change.

   3. Layout is put inside an area. The size of the area changes, the
   layout should change with it. 

   4. A button grows to accommodate additional text. The whitespace
   around it should be modified to cope with the new layout
   requirements. 

   5. A button grows. The area surrounding it should grow also if
   possible. 

   What metrics are important?
   1. Actual width and height.
   
   2. Whether the width and height are what the widget actually wants, or
   whether it can grow or shrink. 

   Text glyphs are particularly troublesome since their metrics depend
   on the context in which they are being viewed. For instance they
   can appear differently depending on the window face, frame face or
   glyph face. All other glyphs are essentially fixed in
   appearance. Perhaps the problem is that text glyphs are cached on a
   device basis like most other glyphs. Instead they should be cached
   per-window and then the instance would be fixed and we wouldn't
   have to mess around with font metrics and the rest. */
static void
layout_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		    Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		    int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object rest, device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object frame = FW_FRAME (domain);
  Lisp_Object items = find_keyword_in_vector (instantiator, Q_items);
  Lisp_Object width = find_keyword_in_vector (instantiator, Q_pixel_width);
  Lisp_Object height = find_keyword_in_vector (instantiator, Q_pixel_height);
  Lisp_Object orient = find_keyword_in_vector (instantiator, Q_orientation);
  Lisp_Object justify = find_keyword_in_vector (instantiator, Q_justify);
  Lisp_Object border = find_keyword_in_vector (instantiator, Q_border);
  Lisp_Object children = Qnil;
  int pw = 0, ph = 0, x, y, maxph = 0, maxpw = 0, nitems = 0,
    horiz_spacing, vert_spacing, ph_adjust = 0;

  if (NILP (frame))
    signal_simple_error ("No selected frame", device);
  
  if (!(dest_mask & IMAGE_LAYOUT_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_LAYOUT_MASK);

  if (NILP (orient))
    orient = Qvertical;

  if (EQ (border, Qt))
    border = Qetched_in;

  ii->data = 0;
  IMAGE_INSTANCE_TYPE (ii) = IMAGE_LAYOUT;
  IMAGE_INSTANCE_SUBWINDOW_ID (ii) = 0;
  IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (ii) = 0;
  IMAGE_INSTANCE_SUBWINDOW_FRAME (ii) = frame;
  IMAGE_INSTANCE_LAYOUT_BORDER (ii) = border;

  /* normalize size information */
  if (!NILP (width))
    pw = XINT (width);
  if (!NILP (height))
    ph = XINT (height);

  /* flip through the items to work out how much stuff we have to display */
  LIST_LOOP (rest, items)
    {
      Lisp_Object glyph = XCAR (rest);
      int gheight = glyph_height (glyph, domain);
      int gwidth = glyph_width (glyph, domain);
      nitems ++;
      if (EQ (orient, Qhorizontal))
	{
	  maxph = max (maxph, gheight);
	  maxpw += gwidth;
	}
      else if (EQ (orient, Qvertical))
	{
	  maxpw = max (maxpw, gwidth);
	  maxph += gheight;
	}
    }

  /* work out spacing between items and bounds of the layout */
  if (!pw)
    {
      /* No user provided width so we just do default spacing. */
      horiz_spacing = WIDGET_BORDER_WIDTH * 2;
      if (EQ (orient, Qhorizontal))
	pw = maxpw + (nitems + 1) * horiz_spacing;
      else 
	pw = maxpw + 2 * horiz_spacing;
    }
  else if (pw < maxpw)
    /* The user wants a smaller space than the largest item, so we
       just provide default spacing and will let the output routines
       clip.. */
    horiz_spacing = WIDGET_BORDER_WIDTH * 2;
  else if (EQ (orient, Qhorizontal))
    /* We have a larger area to display in so distribute the space
       evenly. */
    horiz_spacing = (pw - maxpw) / (nitems + 1);
  else
    horiz_spacing = (pw - maxpw) / 2;

  /* Do the border now so that we can adjust the layout. */
  if (GLYPHP (border))
    {
      /* We are going to be sneaky here and add the border text as
         just another child, the layout and output routines don't know
         this and will just display at the offsets we prescribe. */
      Lisp_Object bglyph = glyph_image_instance (border, domain, ERROR_ME, 1);

      children = Fcons (bglyph, children);
      XIMAGE_INSTANCE_XOFFSET (bglyph) = 10; /* Really, what should this be? */
      XIMAGE_INSTANCE_YOFFSET (bglyph) = 0;

      ph_adjust = (glyph_height (border, domain) / 2);
      IMAGE_INSTANCE_LAYOUT_BORDER (ii) = make_int (ph_adjust);
    }

  /* Work out vertical spacings. */
  if (!ph)
    {
      vert_spacing = WIDGET_BORDER_HEIGHT * 2;
      if (EQ (orient, Qvertical))
	ph = maxph + (nitems + 1) * vert_spacing + ph_adjust;
      else 
	ph = maxph + 2 * vert_spacing + ph_adjust;
    }
  else if (ph < maxph)
    vert_spacing = WIDGET_BORDER_HEIGHT * 2;
  else if (EQ (orient, Qvertical))
    vert_spacing = (ph - (maxph + ph_adjust)) / (nitems + 1);
  else
    vert_spacing = (ph - (maxph + ph_adjust)) / 2;

  y = vert_spacing + ph_adjust;
  x = horiz_spacing;

  /* Now flip through putting items where we want them, paying
     attention to justification. */
  LIST_LOOP (rest, items)
    {
      /* make sure the image is instantiated */
      Lisp_Object glyph = XCAR (rest);
      Lisp_Object gii = glyph_image_instance (glyph, domain, ERROR_ME, 1);
      int gwidth = glyph_width (glyph, domain);
      int gheight = glyph_height (glyph, domain);

      children = Fcons (gii, children);

      if (EQ (orient, Qhorizontal))
	{
	  if (EQ (justify, Qright))
	    y = ph - (gheight + vert_spacing);
	  else if (EQ (justify, Qcenter))
	    y = (ph - gheight) / 2;
	}
      else if (EQ (orient, Qvertical))
	{
	  if (EQ (justify, Qright))
	    x = pw - (gwidth + horiz_spacing);
	  else if (EQ (justify, Qcenter))
	    x = (pw - gwidth) / 2;
	}

      XIMAGE_INSTANCE_XOFFSET (gii) = x;
      XIMAGE_INSTANCE_YOFFSET (gii) = y;

      if (EQ (orient, Qhorizontal))
	{
	  x += (gwidth + horiz_spacing);
	}
      else if (EQ (orient, Qvertical))
	{
	  y += (gheight + vert_spacing);
	}
    }

  IMAGE_INSTANCE_LAYOUT_CHILDREN (ii) = children;
  assert (pw && ph);
  IMAGE_INSTANCE_SUBWINDOW_WIDTH (ii) = pw;
  IMAGE_INSTANCE_SUBWINDOW_HEIGHT (ii) = ph;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_glyphs_widget (void)
{
  defkeyword (&Q_descriptor, ":descriptor");
  defkeyword (&Q_height, ":height");
  defkeyword (&Q_width, ":width");
  defkeyword (&Q_properties, ":properties");
  defkeyword (&Q_items, ":items");
  defkeyword (&Q_image, ":image");
  defkeyword (&Q_percent, ":percent");
  defkeyword (&Q_text, ":text");
  defkeyword (&Q_orientation, ":orientation");
  defkeyword (&Q_justify, ":justify");
  defkeyword (&Q_border, ":border");

  defsymbol (&Qetched_in, "etched-in");
  defsymbol (&Qetched_out, "etched-out");
  defsymbol (&Qbevel_in, "bevel-in");
  defsymbol (&Qbevel_out, "bevel-out");
}

void
image_instantiator_format_create_glyphs_widget (void)
{
#define VALID_GUI_KEYWORDS(type) \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_active, check_valid_anything); \
  IIFORMAT_VALID_KEYWORD (type, Q_suffix, check_valid_anything);		\
  IIFORMAT_VALID_KEYWORD (type, Q_keys, check_valid_string);		\
  IIFORMAT_VALID_KEYWORD (type, Q_style, check_valid_symbol);		\
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_selected, check_valid_anything); \
  IIFORMAT_VALID_KEYWORD (type, Q_filter, check_valid_anything);		\
  IIFORMAT_VALID_KEYWORD (type, Q_config, check_valid_symbol);		\
  IIFORMAT_VALID_KEYWORD (type, Q_included, check_valid_anything);	\
  IIFORMAT_VALID_KEYWORD (type, Q_key_sequence, check_valid_string);	\
  IIFORMAT_VALID_KEYWORD (type, Q_accelerator, check_valid_string);		\
  IIFORMAT_VALID_KEYWORD (type, Q_label, check_valid_anything);		\
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_callback, check_valid_callback); \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_descriptor, check_valid_string_or_vector)

#define VALID_WIDGET_KEYWORDS(type) \
  IIFORMAT_VALID_KEYWORD (type, Q_width, check_valid_int);		\
  IIFORMAT_VALID_KEYWORD (type, Q_height, check_valid_int);		\
  IIFORMAT_VALID_KEYWORD (type, Q_pixel_width, check_valid_int);	\
  IIFORMAT_VALID_KEYWORD (type, Q_pixel_height, check_valid_int);	\
  IIFORMAT_VALID_KEYWORD (type, Q_face, check_valid_face)

  /* we only do this for properties */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT_NO_SYM (widget, "widget");
  IIFORMAT_HAS_METHOD (widget, property);
  IIFORMAT_HAS_METHOD (widget, set_property);
  IIFORMAT_HAS_METHOD (widget, query_geometry);
  IIFORMAT_HAS_METHOD (widget, layout);

  /* widget image-instantiator types - buttons */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (button, "button");
  IIFORMAT_HAS_SHARED_METHOD (button, validate, widget);
  IIFORMAT_HAS_SHARED_METHOD (button, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (button, instantiate, widget);
  IIFORMAT_HAS_SHARED_METHOD (button, normalize, widget);
  IIFORMAT_VALID_KEYWORD (button, 
			  Q_image, check_valid_glyph_or_instantiator);
  VALID_WIDGET_KEYWORDS (button);
  VALID_GUI_KEYWORDS (button);

  /* edit fields */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (edit_field, "edit-field");
  IIFORMAT_HAS_SHARED_METHOD (edit_field, validate, widget);
  IIFORMAT_HAS_SHARED_METHOD (edit_field, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (edit_field, instantiate, widget);
  VALID_WIDGET_KEYWORDS (edit_field);
  VALID_GUI_KEYWORDS (edit_field);

  /* combo box */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (combo_box, "combo-box");
  IIFORMAT_HAS_METHOD (combo_box, validate);
  IIFORMAT_HAS_SHARED_METHOD (combo_box, possible_dest_types, widget);
  VALID_GUI_KEYWORDS (combo_box);

  IIFORMAT_VALID_KEYWORD (combo_box, Q_width, check_valid_int);
  IIFORMAT_VALID_KEYWORD (combo_box, Q_height, check_valid_int);
  IIFORMAT_VALID_KEYWORD (combo_box, Q_pixel_width, check_valid_int);
  IIFORMAT_VALID_KEYWORD (combo_box, Q_face, check_valid_face);
  IIFORMAT_VALID_KEYWORD (combo_box, Q_properties, check_valid_item_list);

  /* scrollbar */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (scrollbar, "scrollbar");
  IIFORMAT_HAS_SHARED_METHOD (scrollbar, validate, widget);
  IIFORMAT_HAS_SHARED_METHOD (scrollbar, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (scrollbar, instantiate, widget);
  VALID_GUI_KEYWORDS (scrollbar);

  IIFORMAT_VALID_KEYWORD (scrollbar, Q_pixel_width, check_valid_int);
  IIFORMAT_VALID_KEYWORD (scrollbar, Q_pixel_height, check_valid_int);
  IIFORMAT_VALID_KEYWORD (scrollbar, Q_face, check_valid_face);

  /* progress guage */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (progress_gauge, "progress-gauge");
  IIFORMAT_HAS_SHARED_METHOD (progress_gauge, validate, widget);
  IIFORMAT_HAS_SHARED_METHOD (progress_gauge, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (progress_gauge, instantiate, widget);
  VALID_WIDGET_KEYWORDS (progress_gauge);
  VALID_GUI_KEYWORDS (progress_gauge);

  /* tree view */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (tree_view, "tree-view");
  IIFORMAT_HAS_SHARED_METHOD (tree_view, validate, combo_box);
  IIFORMAT_HAS_SHARED_METHOD (tree_view, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (tree_view, instantiate, widget);
  IIFORMAT_HAS_METHOD (tree_view, query_geometry);
  VALID_WIDGET_KEYWORDS (tree_view);
  VALID_GUI_KEYWORDS (tree_view);
  IIFORMAT_VALID_KEYWORD (tree_view, Q_properties, check_valid_item_list);

  /* tab control */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (tab_control, "tab-control");
  IIFORMAT_HAS_SHARED_METHOD (tab_control, validate, combo_box);
  IIFORMAT_HAS_SHARED_METHOD (tab_control, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (tab_control, instantiate, widget);
  IIFORMAT_HAS_METHOD (tab_control, query_geometry);
  VALID_WIDGET_KEYWORDS (tab_control);
  VALID_GUI_KEYWORDS (tab_control);
  IIFORMAT_VALID_KEYWORD (tab_control, Q_orientation, check_valid_tab_orientation);
  IIFORMAT_VALID_KEYWORD (tab_control, Q_properties, check_valid_item_list);

  /* labels */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (label, "label");
  IIFORMAT_HAS_SHARED_METHOD (label, possible_dest_types, widget);
  IIFORMAT_HAS_SHARED_METHOD (label, instantiate, widget);
  VALID_WIDGET_KEYWORDS (label);
  IIFORMAT_VALID_KEYWORD (label, Q_descriptor, check_valid_string);

  /* layout */
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (layout, "layout");
  IIFORMAT_HAS_METHOD (layout, possible_dest_types);
  IIFORMAT_HAS_METHOD (layout, instantiate);
  IIFORMAT_HAS_METHOD (layout, normalize);
  IIFORMAT_VALID_KEYWORD (layout, Q_pixel_width, check_valid_int);
  IIFORMAT_VALID_KEYWORD (layout, Q_pixel_height, check_valid_int);
  IIFORMAT_VALID_KEYWORD (layout, Q_orientation, check_valid_orientation);
  IIFORMAT_VALID_KEYWORD (layout, Q_justify, check_valid_justification);
  IIFORMAT_VALID_KEYWORD (layout, Q_border, check_valid_border);
  IIFORMAT_VALID_KEYWORD (layout, Q_items, 
			  check_valid_glyph_or_instantiator_list);
}

void
reinit_vars_of_glyphs_widget (void)
{
#ifdef DEBUG_WIDGETS
  debug_widget_instances = 0;
#endif
}

void
vars_of_glyphs_widget (void)
{
  reinit_vars_of_glyphs_widget ();
}
