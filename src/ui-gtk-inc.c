/* ui-gtk-inc.c
**
** Description: Include file for duplicated code in ui-gtk.c
**
** Created by: William M. Perry <wmperry@gnu.org>
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
   Copyright (c) 2003 Ben Wing.
**
*/

#undef GTK_LVALUE

#ifdef GTK_CONVERT_NORMAL
#define GTK_LVALUE(type) GTK_VALUE_##type (*arg)
#else
#define GTK_LVALUE(type) *(GTK_RETLOC_##type (*arg))
#endif

int
#ifdef GTK_CONVERT_NORMAL
lisp_to_gtk_type (Lisp_Object obj, GtkArg *arg)
#else
lisp_to_gtk_ret_type (Lisp_Object obj, GtkArg *arg)
#endif
{
  switch (GTK_FUNDAMENTAL_TYPE (arg->type))
    {
      /* flag types */
    case GTK_TYPE_NONE:
      return (0);
    case GTK_TYPE_CHAR:
    case GTK_TYPE_UCHAR:
      CHECK_CHAR_COERCE_INT (obj);
      GTK_LVALUE (CHAR) = ichar_to_unicode (XCHAR (obj));
      break;
    case GTK_TYPE_BOOL:
      GTK_LVALUE (BOOL) = NILP (obj) ? FALSE : TRUE;
      break;
    case GTK_TYPE_INT:
    case GTK_TYPE_UINT:
      if (NILP (obj) || EQ (Qt, obj))
	{
	  /* For we are a kind mistress and allow sending t/nil for
             1/0 to stupid GTK functions that say they take guint or
             gint in the header files, but actually treat it like a
             bool.  *sigh*
	  */
	  GTK_LVALUE (INT) = NILP (obj) ? 0 : 1;
	}
      else
	{
	  CHECK_INT (obj);
	  GTK_LVALUE (INT) = XINT (obj);
	}
      break;
    case GTK_TYPE_LONG:
    case GTK_TYPE_ULONG:
      ABORT ();
    case GTK_TYPE_FLOAT:
      CHECK_INT_OR_FLOAT (obj);
      GTK_LVALUE (FLOAT) = extract_float (obj);
      break;
    case GTK_TYPE_DOUBLE:
      CHECK_INT_OR_FLOAT (obj);
      GTK_LVALUE (DOUBLE) = extract_float (obj);
      break;
    case GTK_TYPE_STRING:
      if (NILP (obj))
	GTK_LVALUE (STRING) = NULL;
      else
	{
	  CHECK_STRING (obj);
#ifdef GTK_CONVERT_NORMAL
	  LISP_STRING_TO_EXTERNAL_MALLOC (obj, GTK_LVALUE (STRING),
					  Vgtk_text_encoding);
#else
	  /* #### BILL!! Is this correct?  It followed the old logic */
	  LISP_STRING_TO_EXTERNAL (obj, GTK_LVALUE (STRING),
				   Vgtk_text_encoding);
#endif

	}
      break;
    case GTK_TYPE_ENUM:
    case GTK_TYPE_FLAGS:
      /* Convert a lisp symbol to a GTK enum */
      GTK_LVALUE (ENUM) = lisp_to_flag (obj, arg->type);
      break;
    case GTK_TYPE_BOXED:
      if (NILP (obj))
	{
	  GTK_LVALUE (BOXED) = NULL;
	}
      else if (GTK_BOXEDP (obj))
	{
	  GTK_LVALUE (BOXED) = XGTK_BOXED (obj)->object;
	}
      else if (arg->type == GTK_TYPE_STYLE)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  GTK_LVALUE (BOXED) = face_to_style (obj);
	}
      else if (arg->type == GTK_TYPE_GDK_GC)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  GTK_LVALUE (BOXED) = face_to_gc (obj);
	}
      else if (arg->type == GTK_TYPE_GDK_WINDOW)
	{
	  if (GLYPHP (obj))
	    {
	      Lisp_Object window = Fselected_window (Qnil);
	      Lisp_Object instance =
		glyph_image_instance (obj, window, ERROR_ME_DEBUG_WARN, 1);
	      struct Lisp_Image_Instance *p = XIMAGE_INSTANCE (instance);

	      switch (XIMAGE_INSTANCE_TYPE (instance))
		{
		case IMAGE_TEXT:
		case IMAGE_POINTER:
		case IMAGE_SUBWINDOW:
		case IMAGE_NOTHING:
		  GTK_LVALUE (BOXED) = NULL;
		  break;

		case IMAGE_MONO_PIXMAP:
		case IMAGE_COLOR_PIXMAP:
		  GTK_LVALUE (BOXED) = IMAGE_INSTANCE_GTK_PIXMAP (p);
		  break;
		}
	    }
	  else if (GTK_OBJECTP (obj) &&
		   GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	    {
	      GTK_LVALUE (BOXED) = GTK_WIDGET (XGTK_OBJECT (obj))->window;
	    }
	  else
	    {
	      invalid_argument
		("Don't know how to convert object to GDK_WINDOW", obj);
	    }
	  break;
	}
      else if (arg->type == GTK_TYPE_GDK_COLOR)
	{
	  if (COLOR_SPECIFIERP (obj))
	    {
	      /* If it is a specifier, we just convert it to an
                 instance, and let the ifs below handle it.
	      */
	      obj = Fspecifier_instance (obj, Qnil, Qnil, Qnil);
	    }
	  
	  if (COLOR_INSTANCEP (obj))
	    {
	      /* Easiest one */
	      GTK_LVALUE (BOXED) =
		COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument
		("Please use a color specifier or instance, not a string",
		 obj);
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert to GdkColor", obj);
	    }
	}
      else if (arg->type == GTK_TYPE_GDK_FONT)
	{
	  if (SYMBOLP (obj))
	    {
	      /* If it is a symbol, we treat that as a face name */
	      obj = Ffind_face (obj);
	    }

	  if (FACEP (obj))
	    {
	      /* If it is a face, we just grab the font specifier, and
                 cascade down until we finally reach a FONT_INSTANCE
	      */
	      obj = Fget (obj, Qfont, Qnil);
	    }

	  if (FONT_SPECIFIERP (obj))
	    {
	      /* If it is a specifier, we just convert it to an
                 instance, and let the ifs below handle it
	      */
	      obj = Fspecifier_instance (obj, Qnil, Qnil, Qnil);
	    }

	  if (FONT_INSTANCEP (obj))
	    {
	      /* Easiest one */
	      GTK_LVALUE (BOXED) =
		FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument
		("Please use a font specifier or instance, not a string", obj);
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert to GdkColor", obj);
	    }
	}
      else
	{
	  /* Unknown type to convert to boxed */
	  stderr_out ("Don't know how to convert to boxed!\n");
	  GTK_LVALUE (BOXED) = NULL;
	}
      break;

    case GTK_TYPE_POINTER:
      if (NILP (obj))
	GTK_LVALUE (POINTER) = NULL;
      else
	GTK_LVALUE (POINTER) = LISP_TO_VOID (obj);
      break;

      /* structured types */
    case GTK_TYPE_SIGNAL:
    case GTK_TYPE_ARGS: /* This we can do as a list of values */
    case GTK_TYPE_C_CALLBACK:
    case GTK_TYPE_FOREIGN:
      stderr_out ("Do not know how to convert `%s' from lisp!\n",
		  GTK_TEXT_TO_C_STRING (gtk_type_name (arg->type)));
      return (-1);

#if 0
      /* #### BILL! */
      /* #### This is not used, and GTK_RETLOC_CALLBACK does not exist */
    case GTK_TYPE_CALLBACK:
      {
	GUI_ID id;

	id = new_gui_id ();
	obj = Fcons (Qnil, obj); /* Empty data */
	obj = Fcons (make_int (id), obj);

	gcpro_popup_callbacks (id, obj);

	GTK_LVALUE (CALLBACK).marshal = __internal_callback_marshal;
	GTK_LVALUE (CALLBACK).data = LISP_TO_VOID (obj);
	GTK_LVALUE (CALLBACK).notify = __internal_callback_destroy;
      }
      break;
#endif

      /* base type of the object system */
    case GTK_TYPE_OBJECT:
      if (NILP (obj))
	GTK_LVALUE (OBJECT) = NULL;
      else
	{
	  CHECK_GTK_OBJECT (obj);
	  if (XGTK_OBJECT (obj)->alive_p)
	    GTK_LVALUE (OBJECT) = XGTK_OBJECT (obj)->object;
	  else
	    invalid_argument
	      ("Attempting to pass dead object to GTK function", obj);
	}
      break;

    default:
      /* GTK_TYPE_ARRAY, GTK_TYPE_LISTOF not constants */
      if (GTK_FUNDAMENTAL_TYPE_EQ (arg->type, GTK_TYPE_ARRAY))
	{
	  if (NILP (obj))
	    GTK_LVALUE (POINTER) = NULL;
	  else
	    xemacs_list_to_array (obj, arg);
	}
      else if (GTK_FUNDAMENTAL_TYPE_EQ (arg->type, GTK_TYPE_LISTOF))
	{
	  if (NILP (obj))
	    GTK_LVALUE (POINTER) = NULL;
	  else
	    xemacs_list_to_gtklist (obj, arg);
	}
      else
	{
	  stderr_out ("Do not know how to convert `%s' from lisp!\n",
		      GTK_TEXT_TO_C_STRING (gtk_type_name (arg->type)));
	  ABORT ();
	}
      break;
    }

  return (0);
}
