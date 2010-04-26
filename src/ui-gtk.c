/* ui-gtk.c
**
** Description: Creating 'real' UIs from lisp.
**
** Created by: William M. Perry <wmperry@gnu.org>
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
** Copyright (C) 2010 Ben Wing.
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

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "elhash.h"
#include "events.h"
#include "faces.h"
#include "hash.h"
#include "sysdll.h"
#include "window.h"

#include "console-gtk-impl.h"
#include "glyphs-gtk.h"
#include "fontcolor-gtk.h"
#include "ui-gtk.h"

/* XEmacs specific GTK types */
#include "gtk-glue.c"

/* Is the fundamental type of 't' the xemacs defined fundamental type 'type'? */
#define IS_XEMACS_GTK_FUNDAMENTAL_TYPE(t,type) (((GtkType) GTK_FUNDAMENTAL_TYPE(t)) == (type))

Lisp_Object Qemacs_ffip;
Lisp_Object Qemacs_gtk_objectp;
Lisp_Object Qemacs_gtk_boxedp;
Lisp_Object Qvoid;
Lisp_Object Venumeration_info;

static GHashTable *dll_cache;

Lisp_Object gtk_type_to_lisp (GType *arg);
int lisp_to_gtk_type (Lisp_Object obj, GType *arg);
int lisp_to_gtk_ret_type (Lisp_Object obj, GType *arg);
#if 0
void describe_gtk_arg (GType *arg);
#endif
guint symbol_to_enum (Lisp_Object obj, GtkType t);
static guint lisp_to_flag (Lisp_Object obj, GType t);
static Lisp_Object flags_to_list (guint value, GType t);
static Lisp_Object enum_to_symbol (guint value, GType t);

#define NIL_OR_VOID_P(x) (NILP (x) || EQ (x, Qvoid))

static void
initialize_dll_cache (void)
{
  if (!dll_cache)
    {
      static char text[] = "---XEmacs Internal Handle---";

      dll_cache = g_hash_table_new (g_str_hash, g_str_equal);

      g_hash_table_insert (dll_cache, text, dll_open (Qnil));
    }
}

DEFUN ("dll-load", Fdll_load, 1, 1, 0, /*
Load a shared library DLL into XEmacs.  No initialization routines are required.
This is for loading dependency DLLs into XEmacs.
*/
       (dll))
{
  dll_handle h;

  CHECK_STRING (dll);

  initialize_dll_cache ();

  /* If the dll name has a directory component in it, then we should
     expand it. */
  if (!NILP (Fstring_match (build_ascstring ("/"), dll, Qnil, Qnil)))
    dll = Fexpand_file_name (dll, Qnil);

  /* Check if we have already opened it first */
  h = g_hash_table_lookup (dll_cache, XSTRING_DATA (dll));

  if (!h)
    {
      h = dll_open (dll);

      if (h)
	{
	  g_hash_table_insert (dll_cache, qxestrdup (XSTRING_DATA (dll)), h);
	}
      else
	{
	  signal_error (Qfile_error, "dll_open error", dll_error());
	}
    }
  return (h ? Qt : Qnil);
}


/* Gtk object importing */
EXFUN (Fgtk_import_type, 1);

static struct hash_table *internal_type_hash;

static int
type_already_imported_p (GType t)
{
  void *retval = NULL;

  /* These are cases that we don't need to import */
  switch (GTK_FUNDAMENTAL_TYPE (t))
    {
    case GTK_TYPE_CHAR:
    case GTK_TYPE_UCHAR:
    case GTK_TYPE_BOOL:
    case GTK_TYPE_INT:
    case GTK_TYPE_UINT:
    case GTK_TYPE_LONG:
    case GTK_TYPE_ULONG:
    case GTK_TYPE_FLOAT:
    case GTK_TYPE_DOUBLE:
    case GTK_TYPE_STRING:
    case GTK_TYPE_BOXED:
    case GTK_TYPE_POINTER:
      //case GTK_TYPE_SIGNAL:
      //case GTK_TYPE_ARGS:
      //case GTK_TYPE_CALLBACK:
      //case GTK_TYPE_C_CALLBACK:
      //case GTK_TYPE_FOREIGN:
	return (1);
    }

  if (!internal_type_hash)
    {
      internal_type_hash = make_hash_table (163);
      return (0);
    }

  if (gethash ((void *)t, internal_type_hash, (const void **)&retval))
    {
      return (1);
    }
  return (0);
}

static void
mark_type_as_imported (GType t)
{
  if (type_already_imported_p (t))
    return;

  puthash ((void *) t, (void *) 1, internal_type_hash);
}

static void import_gtk_type (GType t);

static void
import_gtk_object_internal (GType the_type)
{
  GType original_type = the_type;
  int first_time = 1;

  do
    {
      GtkArg *args;
      guint32 *flags;
      guint n_args;
      guint i;
#if 0
      GtkObjectClass *klass;
      GtkSignalQuery *query;
      guint32 *signals;
      guint n_signals;
#endif

      /* Register the type before we do anything else with it... */
      if (!first_time)
	{
	  if (!type_already_imported_p (the_type))
	    {
	      import_gtk_type (the_type);
	    }
	}
      else
	{
	  /* We need to mark the object type as imported here or we
	     run the risk of SERIOUS recursion when we do automatic
	     argument type importing.  mark_type_as_imported() is
	     smart enough to be a noop if we attempt to register
	     things twice.  */
	  first_time = 0;
	  mark_type_as_imported (the_type);
	}

      args = gtk_object_query_args(the_type,&flags,&n_args);

      /* First get the arguments the object can accept */
      for (i = 0; i < n_args; i++)
	{
	  if ((args[i].type != original_type) && !type_already_imported_p (args[i].type))
	    {
	      import_gtk_type (args[i].type);
	    }
	}

      g_free(args);
      g_free(flags);

#if 0
      /* Now lets publish the signals */
      klass = (GtkObjectClass *) gtk_type_class (the_type);
      signals = klass->signals;
      n_signals = klass->nsignals;

      for (i = 0; i < n_signals; i++)
	{
	  query = gtk_signal_query (signals[i]);
	  /* What do we want to do here? */
	  g_free (query);
	}
#endif

      the_type = gtk_type_parent(the_type);
    } while (the_type != GTK_TYPE_INVALID);
}

static void
import_gtk_enumeration_internal (GType the_type)
{
  GtkEnumValue *vals = gtk_type_enum_get_values (the_type);
  Lisp_Object assoc = Qnil;

  if (NILP (Venumeration_info))
    {
      Venumeration_info = call2 (intern ("make-hashtable"), make_int (100), Qequal);
    }
  
  while (vals && vals->value_name)
    {
      assoc = Fcons (Fcons (intern (vals->value_nick), make_int (vals->value)), assoc);
      assoc = Fcons (Fcons (intern (vals->value_name), make_int (vals->value)), assoc);
      vals++;
    }

  assoc = Fnreverse (assoc);

  Fputhash (make_int (the_type), assoc, Venumeration_info);
}

static void
import_gtk_type (GType t)
{
  if (type_already_imported_p (t))
    {
      return;
    }

  switch (GTK_FUNDAMENTAL_TYPE (t))
    {
    case GTK_TYPE_ENUM:
    case GTK_TYPE_FLAGS:
      import_gtk_enumeration_internal (t);
      break;
    case GTK_TYPE_OBJECT:
      import_gtk_object_internal (t);
      break;
    default:
      break;
    }

  mark_type_as_imported (t);
}


/* Foreign function calls */
static emacs_ffi_data *
allocate_ffi_data (void)
{
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (emacs_ffi);
  emacs_ffi_data *data = XFFI (obj);

  data->return_type = GTK_TYPE_NONE;
  data->n_args = 0;
  data->function_name = Qnil;
  data->function_ptr = 0;
  data->marshal = 0;

  return (data);
}

static const struct memory_description ffi_data_description [] = {
  { XD_LISP_OBJECT, offsetof (emacs_ffi_data, function_name) }, 
  { XD_END }
};

static Lisp_Object
mark_ffi_data (Lisp_Object obj)
{
  emacs_ffi_data *data = (emacs_ffi_data *) XFFI (obj);

  mark_object (data->function_name);
  return (Qnil);
}

static void
ffi_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
		    int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);

  write_fmt_string_lisp (printcharfun, "#<ffi %S", 1, XFFI (obj)->function_name);
  if (XFFI (obj)->n_args)
    write_fmt_string (printcharfun, " %d arguments", XFFI (obj)->n_args);
  write_fmt_string (printcharfun, " %p>", (void *)XFFI (obj)->function_ptr);
}

DEFINE_NODUMP_LISP_OBJECT ("ffi", emacs_ffi,
			   mark_ffi_data, ffi_object_printer,
			   0, 0, 0, 
			   ffi_data_description, emacs_ffi_data);

#if defined (__cplusplus)
#define MANY_ARGS ...
#else
#define MANY_ARGS
#endif

typedef void (*pfv)();
typedef GtkObject * (*__OBJECT_fn) (MANY_ARGS);
typedef gint (*__INT_fn) (MANY_ARGS);
typedef void (*__NONE_fn) (MANY_ARGS);
typedef gchar * (*__STRING_fn) (MANY_ARGS);
typedef gboolean (*__BOOL_fn) (MANY_ARGS);
typedef gfloat (*__FLOAT_fn) (MANY_ARGS);
typedef void * (*__POINTER_fn) (MANY_ARGS);
typedef GList * (*__LIST_fn) (MANY_ARGS);

/* An auto-generated file of marshalling functions. */
#include "emacs-marshals.c"
#undef MANY_ARGS

#define CONVERT_SINGLE_TYPE(var,nam,tp) case GTK_TYPE_##nam: GTK_VALUE_##nam (var) = * (tp *) v; break;
#define CONVERT_RETVAL(a,freep) 			\
  do {							\
    void *v = GTK_VALUE_POINTER(a);			\
    switch (GTK_FUNDAMENTAL_TYPE (a.type))		\
      {							\
	CONVERT_SINGLE_TYPE(a,CHAR,gchar);		\
	CONVERT_SINGLE_TYPE(a,UCHAR,guchar);		\
	CONVERT_SINGLE_TYPE(a,BOOL,gboolean);		\
	CONVERT_SINGLE_TYPE(a,INT,gint);		\
	CONVERT_SINGLE_TYPE(a,UINT,guint);		\
	CONVERT_SINGLE_TYPE(a,LONG,glong);		\
	CONVERT_SINGLE_TYPE(a,ULONG,gulong);		\
	CONVERT_SINGLE_TYPE(a,FLOAT,gfloat);		\
	CONVERT_SINGLE_TYPE(a,DOUBLE,gdouble);		\
	CONVERT_SINGLE_TYPE(a,STRING,gchar *);		\
	CONVERT_SINGLE_TYPE(a,ENUM,gint);		\
	CONVERT_SINGLE_TYPE(a,FLAGS,guint);		\
	CONVERT_SINGLE_TYPE(a,BOXED,void *);		\
	CONVERT_SINGLE_TYPE(a,POINTER,void *);		\
	CONVERT_SINGLE_TYPE(a,OBJECT,GtkObject *);	\
      default:						\
	GTK_VALUE_POINTER (a) = * (void **) v;		\
	break;						\
      }							\
    if (freep) xfree (v);			\
  } while (0)

static gpointer __allocate_object_storage (GType t)
{
  size_t s = 0;
  void *rval = NULL;

  switch (GTK_FUNDAMENTAL_TYPE (t))
    {
      /* flag types */
    case GTK_TYPE_CHAR:
      s = (sizeof (gchar));
      break;
    case GTK_TYPE_UCHAR:
      s = (sizeof (guchar));
      break;
    case GTK_TYPE_BOOL:
      s = (sizeof (gboolean));
      break;
    case GTK_TYPE_INT:
      s = (sizeof (gint));
      break;
    case GTK_TYPE_UINT:
      s = (sizeof (guint));
      break;
    case GTK_TYPE_LONG:
      s = (sizeof (glong));
      break;
    case GTK_TYPE_ULONG:
      s = (sizeof (gulong));
      break;
    case GTK_TYPE_FLOAT:
      s = (sizeof (gfloat));
      break;
    case GTK_TYPE_DOUBLE:
      s = (sizeof (gdouble));
      break;
    case GTK_TYPE_STRING:
      s = (sizeof (gchar *));
      break;
    case GTK_TYPE_ENUM:
    case GTK_TYPE_FLAGS:
      s = (sizeof (guint));
      break;
    case GTK_TYPE_BOXED:
    case GTK_TYPE_POINTER:
      s = (sizeof (void *));
      break;

      /* base type of the object system */
    case GTK_TYPE_OBJECT:
      s = (sizeof (GtkObject *));
      break;

    default:
      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(t, GTK_TYPE_LISTOF))
	{
	  s = (sizeof (void *));
	}
      rval = NULL;
      break;
    }

  if (s)
    {
      rval = xmalloc (s);
      memset (rval, '\0', s);
    }

  return (rval);
}

static Lisp_Object type_to_marshaller_type (GType t)
{
  switch (GTK_FUNDAMENTAL_TYPE (t))
    {
    case GTK_TYPE_NONE:
      return (build_ascstring ("NONE"));
      /* flag types */
    case GTK_TYPE_CHAR:
    case GTK_TYPE_UCHAR:
      return (build_ascstring ("CHAR"));
    case GTK_TYPE_BOOL:
      return (build_ascstring ("BOOL"));
    case GTK_TYPE_ENUM:
    case GTK_TYPE_FLAGS:
    case GTK_TYPE_INT:
    case GTK_TYPE_UINT:
      return (build_ascstring ("INT"));
    case GTK_TYPE_LONG:
    case GTK_TYPE_ULONG:
      return (build_ascstring ("LONG"));
    case GTK_TYPE_FLOAT:
    case GTK_TYPE_DOUBLE:
      return (build_ascstring ("FLOAT"));
    case GTK_TYPE_STRING:
      return (build_ascstring ("STRING"));
    case GTK_TYPE_BOXED:
    case GTK_TYPE_POINTER:
      return (build_ascstring ("POINTER"));
    case GTK_TYPE_OBJECT:
      return (build_ascstring ("OBJECT"));
    case GTK_TYPE_CALLBACK:
      return (build_ascstring ("CALLBACK"));
    default:
      /* I can't put this in the main switch statement because it is a
         new fundamental type that is not fixed at compile time.
         *sigh*
	 */
      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(t, GTK_TYPE_ARRAY))
	return (build_ascstring ("ARRAY"));

      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(t, GTK_TYPE_LISTOF))
	return (build_ascstring ("LIST"));
      return (Qnil);
    }
}

struct __dll_mapper_closure {
  void * (*func) (dll_handle, const CIbyte *);
  Ibyte *obj_name;
  void **storage;
};

static void __dll_mapper (gpointer UNUSED (key), gpointer value,
			  gpointer user_data)
{
  struct __dll_mapper_closure *closure = (struct __dll_mapper_closure *) user_data;

  if (*(closure->storage) == NULL)
    {
      /* Need to see if it is in this one */
      *(closure->storage) = closure->func ((dll_handle) value, (CIbyte*) closure->obj_name);
    }
}

DEFUN ("gtk-import-variable-internal", Fgtk_import_variable_internal, 2, 2, 0, /*
Import a variable into the XEmacs namespace.
*/
       (type, name))
{
  void *var = NULL;
  GtkArg arg;

  if (SYMBOLP (type)) type = Fsymbol_name (type);

  CHECK_STRING (type);
  CHECK_STRING (name);

  initialize_dll_cache ();
  xemacs_init_gtk_classes ();

  arg.type = gtk_type_from_name ((char *) XSTRING_DATA (type));

  if (arg.type == GTK_TYPE_INVALID)
    {
      sferror ("Unknown type", type);
    }

  /* Need to look thru the already-loaded dlls */
  {
    struct __dll_mapper_closure closure;

    closure.func = dll_variable;
    closure.obj_name = XSTRING_DATA (name);
    closure.storage = &var;

    g_hash_table_foreach (dll_cache, __dll_mapper, &closure);
  }

  if (!var)
    {
      gui_error ("Could not locate variable", name);
    }

  GTK_VALUE_POINTER(arg) = var;
  CONVERT_RETVAL (arg, 0);
  return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-import-function-internal", Fgtk_import_function_internal, 2, 3, 0, /*
Import a function into the XEmacs namespace.
*/
       (rettype, name, args))
{
  Lisp_Object rval = Qnil;
  Lisp_Object marshaller = Qnil;
  emacs_ffi_data *data = NULL;
  gint n_args = 0;
#if 0
  dll_handle h = NULL;
#endif
  ffi_marshalling_function marshaller_func = NULL;
  ffi_actual_function name_func = NULL;

  CHECK_SYMBOL (rettype);
  CHECK_STRING (name);
  CHECK_LIST (args);

  initialize_dll_cache ();
  xemacs_init_gtk_classes ();

  /* Need to look thru the already-loaded dlls */
  {
    struct __dll_mapper_closure closure;

    closure.func = dll_function;
    closure.obj_name = XSTRING_DATA (name);
    closure.storage = (void **) &name_func;

    g_hash_table_foreach (dll_cache, __dll_mapper, &closure);
  }

  if (!name_func)
    {
      gui_error ("Could not locate function", name);
    }

  data = allocate_ffi_data ();

  if (NILP (rettype))
    {
      rettype = Qvoid;
    }

  if (!NILP (args))
    {
      Lisp_Object value = args;
      Lisp_Object type = Qnil;

      EXTERNAL_LIST_LOOP_2 (elt, value)
	{
	  GType the_type;
	  Lisp_Object marshaller_type = Qnil;

	  CHECK_SYMBOL (elt);

	  type = Fsymbol_name (elt);

	  the_type = gtk_type_from_name ((char *) XSTRING_DATA (type));

	  if (the_type == GTK_TYPE_INVALID)
	    {
	      invalid_argument ("Unknown argument type", type);
	    }

	  /* All things must be reduced to their basest form... */
	  import_gtk_type (the_type);
	  data->args[n_args] = the_type; /* GTK_FUNDAMENTAL_TYPE (the_type); */

	  /* Now lets build up another chunk of our marshaller function name */
	  marshaller_type = type_to_marshaller_type (data->args[n_args]);

	  if (NILP (marshaller_type))
	    {
	      invalid_argument ("Do not know how to marshal", type);
	    }
	  marshaller = concat3 (marshaller, build_ascstring ("_"), marshaller_type);
	  n_args++;
	}
    }
  else
    {
      marshaller = concat3 (marshaller, build_ascstring ("_"), type_to_marshaller_type (GTK_TYPE_NONE));
    }

  rettype = Fsymbol_name (rettype);
  data->return_type = gtk_type_from_name ((char *) XSTRING_DATA (rettype));

  if (data->return_type == GTK_TYPE_INVALID)
    {
      invalid_argument ("Unknown return type", rettype);
    }

  import_gtk_type (data->return_type);

  marshaller = concat3 (type_to_marshaller_type (data->return_type), build_ascstring ("_"), marshaller);
  marshaller = concat2 (build_ascstring ("emacs_gtk_marshal_"), marshaller);

  marshaller_func = (ffi_marshalling_function) find_marshaller ((char *) XSTRING_DATA (marshaller));

  if (!marshaller_func)
    {
      gui_error ("Could not locate marshaller function", marshaller);
    }

  data->n_args = n_args;
  data->function_name = name;
  data->function_ptr = (dll_func) name_func;
  data->marshal = marshaller_func;

  rval = wrap_emacs_ffi (data);
  return (rval);
}

DEFUN ("gtk-call-function", Fgtk_call_function, 1, 2, 0, /*
Call an external function.
*/
       (func, args))
{
  GtkArg the_args[MAX_GTK_ARGS];
  gint n_args = 0;
  Lisp_Object retval = Qnil;

  CHECK_FFI (func);
  CHECK_LIST (args);

  n_args = XINT (Flength (args));

#ifdef XEMACS_IS_SMARTER_THAN_THE_PROGRAMMER
  /* #### I think this is too dangerous to enable by default.
  ** #### Genuine program bugs would probably be allowed to
  ** #### slip by, and not be very easy to find.
  ** #### Bill Perry July 9, 2000
  */
  if (n_args != XFFI(func)->n_args)
    {
      Lisp_Object for_append[3];

      /* Signal an error if they pass in too many arguments */
      if (n_args > XFFI(func)->n_args)
	{
	  return Fsignal (Qwrong_number_of_arguments,
			  list2 (func, make_int (n_args)));
	}

      /* If they did not provide enough arguments, be nice and assume
      ** they wanted `nil' in there.
      */
      for_append[0] = args;
      for_append[1] = Fmake_list (make_int (XFFI(func)->n_args - n_args), Qnil);

      args = Fappend (2, for_append);
    }
#else
  if (n_args != XFFI(func)->n_args)
    {
      /* Signal an error if they do not pass in the correct # of arguments */
      return Fsignal (Qwrong_number_of_arguments,
		      list2 (func, make_int (n_args)));
    }
#endif

  if (!NILP (args))
    {
      Lisp_Object value = args;
      
      CHECK_LIST (args);
      n_args = 0;

      /* First we convert all of the arguments from Lisp to GtkArgs */
      {
	EXTERNAL_LIST_LOOP_2 (elt, value)
	  {
	    the_args[n_args].type = XFFI (func)->args[n_args];

	    if (lisp_to_gtk_type (elt, &the_args[n_args]))
	      {
		/* There was some sort of an error */
		gui_error ("Error converting arguments", args);
	      }
	    n_args++;
	  }
      }
    }

  /* Now we need to tack on space for a return value, if they have
     asked for one */
  if (XFFI (func)->return_type != GTK_TYPE_NONE)
    {
      the_args[n_args].type = XFFI (func)->return_type;
      GTK_VALUE_POINTER (the_args[n_args]) = __allocate_object_storage (the_args[n_args].type);
      n_args++;
    }

  XFFI (func)->marshal ((ffi_actual_function) (XFFI (func)->function_ptr), the_args);

  if (XFFI (func)->return_type != GTK_TYPE_NONE)
    {
      CONVERT_RETVAL (the_args[n_args - 1], 1);
      retval = gtk_type_to_lisp (&the_args[n_args - 1]);
    }

  /* Need to free any array or list pointers */
  {
    int i;
    for (i = 0; i < n_args; i++)
      {
	if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(the_args[i].type, GTK_TYPE_ARRAY))
	  {
	    g_free (GTK_VALUE_POINTER (the_args[i]));
	  }
	else if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(the_args[i].type, GTK_TYPE_LISTOF))
	  {
	    /* g_list_free (GTK_VALUE_POINTER (the_args[i])); */
	  }
      }
  }

  return (retval);
}



/* GtkObject wrapping for Lisp */
static void
emacs_gtk_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			  int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);

  write_ascstring (printcharfun, "#<GtkObject (");
  if (XGTK_OBJECT (obj)->alive_p)
    write_cistring (printcharfun, gtk_type_name (GTK_OBJECT_TYPE (XGTK_OBJECT (obj)->object)));
  else
    write_ascstring (printcharfun, "dead");
  write_fmt_string (printcharfun, ") %p>", (void *) XGTK_OBJECT (obj)->object);
}

static Lisp_Object
emacs_gtk_object_getprop (Lisp_Object obj, Lisp_Object prop)
{
  Lisp_Object rval = Qnil;
  Lisp_Object prop_name = Qnil;
  GtkArgInfo *info = NULL;
  char *err;
  GtkArg args[2];

  CHECK_SYMBOL (prop);		/* Shouldn't need to ever do this, but I'm paranoid */

  prop_name = Fsymbol_name (prop);

  args[0].name = (char *) XSTRING_DATA (prop_name);

  err = gtk_object_arg_get_info (GTK_OBJECT_TYPE (XGTK_OBJECT (obj)->object),
				 args[0].name,
				 &info);

  if (err)
    {
      /* Not a magic symbol, fall back to just looking in our real plist */
      g_free (err);

      return (Fplist_get (XGTK_OBJECT (obj)->plist, prop, Qunbound));
    }

  if (!(info->arg_flags & GTK_ARG_READABLE))
    {
      invalid_operation ("Attempt to get write-only property", prop);
    }

  gtk_object_getv (XGTK_OBJECT (obj)->object, 1, args);

  if (args[0].type == GTK_TYPE_INVALID)
    {
      /* If we can't get the attribute, then let the code in Fget know
         so it can use the default value supplied by the caller */
      return (Qunbound);
    }

  rval = gtk_type_to_lisp (&args[0]);

  /* Free up any memory.  According to the documentation and Havoc's
     book, if the fundamental type of the returned value is
     GTK_TYPE_STRING, GTK_TYPE_BOXED, or GTK_TYPE_ARGS, you are
     responsible for freeing it. */
  switch (GTK_FUNDAMENTAL_TYPE (args[0].type))
    {
    case GTK_TYPE_STRING:
      g_free (GTK_VALUE_STRING (args[0]));
      break;
    case GTK_TYPE_BOXED:
      g_free (GTK_VALUE_BOXED (args[0]));
      break;
    case GTK_TYPE_ARGS:
      g_free (GTK_VALUE_ARGS (args[0]).args);
    default:
      break;
    }

  return (rval);
}

static int
emacs_gtk_object_putprop (Lisp_Object obj, Lisp_Object prop, Lisp_Object value)
{
  GtkArgInfo *info = NULL;
  Lisp_Object prop_name = Qnil;
  GtkArg args[2];
  char *err = NULL;

  prop_name = Fsymbol_name (prop);

  args[0].name = (char *) XSTRING_DATA (prop_name);

  err = gtk_object_arg_get_info (GTK_OBJECT_TYPE (XGTK_OBJECT (obj)->object),
				 args[0].name,
				 &info);

  if (err)
    {
      /* Not a magic symbol, fall back to just storing in our real plist */
      g_free (err);

      XGTK_OBJECT (obj)->plist = Fplist_put (XGTK_OBJECT (obj)->plist, prop, value);
      return (1);
    }

  args[0].type = info->type;

  if (lisp_to_gtk_type (value, &args[0]))
    {
      gui_error ("Error converting to GType", value);
    }

  if (!(info->arg_flags & GTK_ARG_WRITABLE))
    {
      invalid_operation ("Attempt to set read-only argument", prop);
    }

  gtk_object_setv (XGTK_OBJECT (obj)->object, 1, args);

  return (1);
}

static const struct memory_description gtk_object_data_description [] = {
  { XD_LISP_OBJECT, offsetof (emacs_gtk_object_data, plist) }, 
  { XD_END }
};

static Lisp_Object
mark_gtk_object_data (Lisp_Object obj)
{
  return (XGTK_OBJECT (obj)->plist);
}

static void
emacs_gtk_object_finalizer (Lisp_Object obj)
{
  emacs_gtk_object_data *data = XEMACS_GTK_OBJECT_DATA (obj);

  if (data->alive_p)
    gtk_object_unref (data->object);
}

DEFINE_NODUMP_LISP_OBJECT ("GtkObject", emacs_gtk_object,
			   mark_gtk_object_data,
			   emacs_gtk_object_printer,
			   emacs_gtk_object_finalizer,
			   0, /* equality */
			   0, /* hash */
			   gtk_object_data_description,
			   emacs_gtk_object_data);

static emacs_gtk_object_data *
allocate_emacs_gtk_object_data (void)
{
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (emacs_gtk_object);
  emacs_gtk_object_data *data = XGTK_OBJECT (obj);

  data->object = NULL;
  data->alive_p = FALSE;
  data->plist = Qnil;

  return (data);
}

/* We need to keep track of when the object is destroyed so that we
   can mark it as dead, otherwise even our print routine (which calls
   GTK_OBJECT_TYPE) will crap out and die.  This is also used in the
   lisp_to_gtk_type() routine to defend against passing dead objects
   to GTK routines. */
static void
__notice_object_destruction (GtkObject *UNUSED (obj), gpointer user_data)
{
  ungcpro_popup_callbacks ((GUI_ID) user_data);
}

Lisp_Object build_gtk_object (GtkObject *obj)
{
  Lisp_Object retval = Qnil;
  emacs_gtk_object_data *data = NULL;
  GUI_ID id = 0;

  id = (GUI_ID) gtk_object_get_data (obj, GTK_DATA_GUI_IDENTIFIER);

  if (id)
    {
      retval = get_gcpro_popup_callbacks (id);
    }

  if (NILP (retval))
    {
      data = allocate_emacs_gtk_object_data ();

      data->object = obj;
      data->alive_p = TRUE;
      retval = wrap_emacs_gtk_object (data);

      id = new_gui_id ();
      gtk_object_set_data (obj, GTK_DATA_GUI_IDENTIFIER, (gpointer) id);
      gcpro_popup_callbacks (id, retval);
      gtk_object_ref (obj);
      gtk_signal_connect (obj, "destroy", GTK_SIGNAL_FUNC (__notice_object_destruction), (gpointer)id);
    }

  return (retval);
}

static void
__internal_callback_destroy (gpointer data)
{
  Lisp_Object lisp_data;

  lisp_data = GET_LISP_FROM_VOID (data);

  ungcpro_popup_callbacks (XINT (XCAR (lisp_data)));
}

static void
__internal_callback_marshal (GtkObject *obj, gpointer data, guint n_args, GtkArg *args)
{
  Lisp_Object arg_list = Qnil;
  Lisp_Object callback_fn = Qnil;
  Lisp_Object callback_data = Qnil;
  Lisp_Object newargs[3];
  Lisp_Object rval = Qnil;
  struct gcpro gcpro1;
  int i;

  callback_fn = GET_LISP_FROM_VOID (data);

  /* Nuke the GUI_ID off the front */
  callback_fn = XCDR (callback_fn);

  callback_data = XCAR (callback_fn);
  callback_fn = XCDR (callback_fn);

  /* The callback data goes at the very end of the argument list */
  arg_list = Fcons (callback_data, Qnil);

  /* Build up the argument list, lisp style */
  for (i = n_args - 1; i >= 0; i--)
    {
      arg_list = Fcons (gtk_type_to_lisp (&args[i]), arg_list);
    }

  /* We always pass the widget as the first parameter at the very least */
  arg_list = Fcons (build_gtk_object (obj), arg_list);

  GCPRO1 ((arg_list));

  newargs[0] = callback_fn;
  newargs[1] = arg_list;

  rval = Fapply (2, newargs);
  signal_fake_event ();

  if (args[n_args].type != GTK_TYPE_NONE)
    lisp_to_gtk_ret_type (rval, &args[n_args]);

  UNGCPRO;
}

DEFUN ("gtk-signal-connect", Fgtk_signal_connect, 3, 6, 0, /*
*/
       (obj, name, func, cb_data, object_signal, after_p))
{
  int c_after;
  int c_object_signal;
  GUI_ID id = 0;

  CHECK_GTK_OBJECT (obj);

  if (SYMBOLP (name))
    name = Fsymbol_name (name);

  CHECK_STRING (name);

  if (NILP (object_signal))
    c_object_signal = 0;
  else
    c_object_signal = 1;

  if (NILP (after_p))
    c_after = 0;
  else
    c_after = 1;

  id = new_gui_id ();
  func = Fcons (cb_data, func);
  func = Fcons (make_int (id), func);

  gcpro_popup_callbacks (id, func);

  gtk_signal_connect_full (XGTK_OBJECT (obj)->object, (char *) XSTRING_DATA (name),
			   NULL, __internal_callback_marshal, STORE_LISP_IN_VOID (func),
			   __internal_callback_destroy, c_object_signal, c_after);
  return (Qt);
}


/* GTK_TYPE_BOXED wrapper for Emacs lisp */
static const struct memory_description emacs_gtk_boxed_description [] = {
  { XD_END }
};

static void
emacs_gtk_boxed_printer (Lisp_Object obj, Lisp_Object printcharfun,
			 int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);

  write_ascstring (printcharfun, "#<GtkBoxed (");
  write_cistring (printcharfun, gtk_type_name (XGTK_BOXED (obj)->object_type));
  write_fmt_string (printcharfun, ") %p>", (void *) XGTK_BOXED (obj)->object);
}

static int
emacs_gtk_boxed_equality (Lisp_Object o1, Lisp_Object o2, int UNUSED (depth))
{
  emacs_gtk_boxed_data *data1 = XGTK_BOXED(o1);
  emacs_gtk_boxed_data *data2 = XGTK_BOXED(o2);

  return ((data1->object == data2->object) &&
	  (data1->object_type == data2->object_type));
}

static Hashcode
emacs_gtk_boxed_hash (Lisp_Object obj, int UNUSED (depth),
                      Boolint UNUSED (equalp))
{
  emacs_gtk_boxed_data *data = XGTK_BOXED(obj);
  return (HASH2 ((Hashcode) data->object, data->object_type));
}

DEFINE_NODUMP_LISP_OBJECT ("GtkBoxed", emacs_gtk_boxed,
			   0, /* marker function */
			   emacs_gtk_boxed_printer,
			   0, /* nuker */
			   emacs_gtk_boxed_equality,
			   emacs_gtk_boxed_hash,
			   emacs_gtk_boxed_description,
			   emacs_gtk_boxed_data);
/* Currently defined GTK_TYPE_BOXED structures are:

   GtkAccelGroup -
   GtkSelectionData -
   GtkStyle -
   GtkCTreeNode - 
   GdkColormap -
   GdkVisual -
   GdkFont -
   GdkWindow -
   GdkDragContext -
   GdkEvent -
   GdkColor - 
*/
static emacs_gtk_boxed_data *
allocate_emacs_gtk_boxed_data (void)
{
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (emacs_gtk_boxed);
  emacs_gtk_boxed_data *data = XGTK_BOXED (obj);

  data->object = NULL;
  data->object_type = GTK_TYPE_INVALID;

  return (data);
}

Lisp_Object build_gtk_boxed (void *obj, GType t)
{
  Lisp_Object retval = Qnil;
  emacs_gtk_boxed_data *data = NULL;

  if (GTK_FUNDAMENTAL_TYPE (t) != GTK_TYPE_BOXED)
    ABORT();

  data = allocate_emacs_gtk_boxed_data ();
  data->object = obj;
  data->object_type = t;

  retval = wrap_emacs_gtk_boxed (data);

  return (retval);
}


/* The automatically generated structure access routines */
#include "emacs-widget-accessors.c"

/* The hand generated funky functions that we can't just import using the FFI */
#include "ui-byhand.c"

/* The glade support */
#include "glade.c"


/* Type manipulation */
DEFUN ("gtk-fundamental-type", Fgtk_fundamental_type, 1, 1, 0, /*
Load a shared library DLL into XEmacs.  No initialization routines are required.
This is for loading dependency DLLs into XEmacs.
*/
       (type))
{
  GType t;

  if (SYMBOLP (type))
    type = Fsymbol_name (type);

  CHECK_STRING (type);

  t = gtk_type_from_name ((char *) XSTRING_DATA (type));

  if (t == GTK_TYPE_INVALID)
    {
      invalid_argument ("Not a GTK type", type);
    }
  return (make_int (GTK_FUNDAMENTAL_TYPE (t)));
}

DEFUN ("gtk-object-type", Fgtk_object_type, 1, 1, 0, /*
Return the GType of OBJECT.
*/
       (object))
{
  CHECK_GTK_OBJECT (object);
  return (make_int (GTK_OBJECT_TYPE (XGTK_OBJECT (object)->object)));
}

DEFUN ("gtk-describe-type", Fgtk_describe_type, 1, 1, 0, /*
Returns a cons of two lists describing the Gtk object TYPE.
The car is a list of all the signals that it will emit.
The cdr is a list of all the magic properties it has.
*/
       (type))
{
  Lisp_Object rval, signals, props;
  GType t;

  props = signals = rval = Qnil;

  if (SYMBOLP (type))
    {
      type = Fsymbol_name (type);
    }

  if (STRINGP (type))
    {
      t = gtk_type_from_name ((gchar*) XSTRING_DATA (type));
      if (t == GTK_TYPE_INVALID)
	{
	  invalid_argument ("Not a GTK type", type);
	}
    }
  else
    {
      CHECK_INT (type);
      t = XINT (type);
    }

  if (GTK_FUNDAMENTAL_TYPE (t) != GTK_TYPE_OBJECT)
    {
      invalid_argument ("Not a GtkObject", type);
    }

  /* Need to do stupid shit like this to get the args
  ** registered... damn GTK and its lazy loading
  */
  {
    GtkArg args[3];
    GtkObject *obj = gtk_object_newv (t, 0, args);

    gtk_object_destroy(obj);
  }

  do
    {
      guint i;

      /* Do the magic arguments first */
      {
	GtkArg *args;
	guint32 *flags;
	guint n_args;

	args = gtk_object_query_args(t,&flags,&n_args);

	for (i = 0; i < n_args; i++)
	  {
	    props = Fcons (Fcons (intern (gtk_type_name(args[i].type)),
				  intern (args[i].name)), props);
	  }

	g_free (args);
	g_free (flags);
      }

      /* Now the signals */
      {
	GtkObjectClass *klass;
	GtkSignalQuery *query;
	guint32 *gtk_signals;
	guint n_signals;

	klass = (GtkObjectClass *) gtk_type_class (t);
	gtk_signals = klass->signals;
	n_signals = klass->nsignals;

	for (i = 0; i < n_signals; i++)
	  {
	    Lisp_Object params = Qnil;

	    query = gtk_signal_query (gtk_signals[i]);

	    if (query)
	      {
		if (query->nparams)
		  {
		    int j;

		    for (j = query->nparams - 1; j >= 0; j--)
		      {
			params = Fcons (intern (gtk_type_name (query->params[j])), params);
		      }
		  }

		signals = Fcons (Fcons (intern (gtk_type_name (query->return_val)),
					Fcons (intern (query->signal_name),
					       params)),
				 signals);
		
		g_free (query);
	      }
	  }
      }
      t = gtk_type_parent(t);
    } while (t != GTK_TYPE_INVALID);

  rval = Fcons (signals, props);

  return (rval);
}


void
ui_gtk_objects_create (void)
{
  OBJECT_HAS_METHOD (emacs_gtk_object, getprop);
  OBJECT_HAS_METHOD (emacs_gtk_object, putprop);
  /* #### No remprop or plist methods */
}

void
syms_of_ui_gtk (void)
{
  INIT_LISP_OBJECT (emacs_ffi);
  INIT_LISP_OBJECT (emacs_gtk_object);
  INIT_LISP_OBJECT (emacs_gtk_boxed);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qemacs_ffip);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qemacs_gtk_objectp);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qemacs_gtk_boxedp);
  DEFSYMBOL (Qvoid);
  DEFSUBR (Fdll_load);
  DEFSUBR (Fgtk_import_function_internal);
  DEFSUBR (Fgtk_import_variable_internal);
  DEFSUBR (Fgtk_signal_connect);
  DEFSUBR (Fgtk_call_function);
  DEFSUBR (Fgtk_fundamental_type);
  DEFSUBR (Fgtk_object_type);
  DEFSUBR (Fgtk_describe_type);
  syms_of_widget_accessors ();
  syms_of_ui_byhand ();
  syms_of_glade ();
}

void
vars_of_ui_gtk (void)
{
  Fprovide (intern ("gtk-ui"));
  DEFVAR_LISP ("gtk-enumeration-info", &Venumeration_info /*
A hashtable holding type information about GTK enumerations and flags.
Do NOT modify unless you really understand ui-gtk.c.
*/);

  Venumeration_info = Qnil;
  vars_of_glade ();
}


/* Various utility functions */
#if 0
void describe_gtk_arg (GtkArg *arg)
{
  GtkArg a = *arg;

  switch (GTK_FUNDAMENTAL_TYPE (a.type))
    {
      /* flag types */
    case GTK_TYPE_CHAR:
      stderr_out ("char: %c\n", GTK_VALUE_CHAR (a));
      break;
    case GTK_TYPE_UCHAR:
      stderr_out ("uchar: %c\n", GTK_VALUE_CHAR (a));
      break;
    case GTK_TYPE_BOOL:
      stderr_out ("uchar: %s\n", GTK_VALUE_BOOL (a) ? "true" : "false");
      break;
    case GTK_TYPE_INT:
      stderr_out ("int: %d\n", GTK_VALUE_INT (a));
      break;
    case GTK_TYPE_UINT:
      stderr_out ("uint: %du\n", GTK_VALUE_UINT (a));
      break;
    case GTK_TYPE_LONG:
      stderr_out ("long: %ld\n", GTK_VALUE_LONG (a));
      break;
    case GTK_TYPE_ULONG:
      stderr_out ("ulong: %lu\n", GTK_VALUE_ULONG (a));
      break;
    case GTK_TYPE_FLOAT:
      stderr_out ("float: %g\n", GTK_VALUE_FLOAT (a));
      break;
    case GTK_TYPE_DOUBLE:
      stderr_out ("double: %f\n", GTK_VALUE_DOUBLE (a));
      break;
    case GTK_TYPE_STRING:
      stderr_out ("string: %s\n", GTK_VALUE_STRING (a));
      break;
    case GTK_TYPE_ENUM:
    case GTK_TYPE_FLAGS:
      stderr_out ("%s: ", (a.type == GTK_TYPE_ENUM) ? "enum" : "flag");
      {
	GtkEnumValue *vals = gtk_type_enum_get_values (a.type);

	while (vals && vals->value_name && (vals->value != GTK_VALUE_ENUM(a))) vals++;

	stderr_out ("%s\n", vals ? vals->value_name : "!!! UNKNOWN ENUM VALUE !!!");
      }
      break;
    case GTK_TYPE_BOXED:
      stderr_out ("boxed: %p\n", GTK_VALUE_BOXED (a));
      break;
    case GTK_TYPE_POINTER:
      stderr_out ("pointer: %p\n", GTK_VALUE_BOXED (a));
      break;

      /* structured types */
    case GTK_TYPE_SIGNAL:
    case GTK_TYPE_ARGS: /* This we can do as a list of values */
      ABORT();
    case GTK_TYPE_CALLBACK:
      stderr_out ("callback fn: ...\n");
      break;
    case GTK_TYPE_C_CALLBACK:
    case GTK_TYPE_FOREIGN:
      ABORT();

      /* base type of the object system */
    case GTK_TYPE_OBJECT:
      if (GTK_VALUE_OBJECT (a))
	stderr_out ("object: %s\n", gtk_type_name (GTK_OBJECT_TYPE (GTK_VALUE_OBJECT (a))));
      else
	stderr_out ("object: NULL\n");
      break;

    default:
      ABORT();
    }
}
#endif

Lisp_Object gtk_type_to_lisp (GValue *arg)
{
  //  G_TYPE_IS_VALUE_TYPE
  switch (G_VALUE_TYPE (G_TYPE_FROM_CLASS (arg)))
    {
    case G_TYPE_NONE:
      return (Qnil);
    case G_TYPE_CHAR:
      return (make_char (g_value_get_char (*arg)));
    case G_TYPE_UCHAR:
      return (make_char (GTK_VALUE_UCHAR (*arg)));
    case G_TYPE_BOOLEAN:
      return (GTK_VALUE_BOOL (*arg) ? Qt : Qnil);
    case G_TYPE_INT:
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_UINT:
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_LONG:		/* I think these are wrong! */
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_ULONG:	/* I think these are wrong! */
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_INT64:	/* bignum? */
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_UINT64:	/* bignum? */
      return (make_int (GTK_VALUE_INT (*arg)));
    case G_TYPE_ENUM:
      return (enum_to_symbol (GTK_VALUE_ENUM (*arg), arg->type));
    case G_TYPE_FLAGS:
      return (flags_to_list (GTK_VALUE_FLAGS (*arg), arg->type));
    case G_TYPE_FLOAT:
      return (make_float (GTK_VALUE_FLOAT (*arg)));
    case G_TYPE_DOUBLE:
      return (make_float (GTK_VALUE_DOUBLE (*arg)));
    case G_TYPE_STRING:
      return (build_cistring (GTK_VALUE_STRING (*arg)));
    case G_TYPE_BOXED:
      if (arg->type == G_TYPE_GDK_EVENT)
	{
	  return (gdk_event_to_emacs_event((GdkEvent *) GTK_VALUE_BOXED (*arg)));
	}

      if (GTK_VALUE_BOXED (*arg))
	return (build_gtk_boxed (GTK_VALUE_BOXED (*arg), arg->type));
      else
	return (Qnil);
    case G_TYPE_POINTER:
      if (GTK_VALUE_POINTER (*arg))
	{
	  Lisp_Object rval;
	  
	  rval = GET_LISP_FROM_VOID (GTK_VALUE_POINTER (*arg));
	  return (rval);
	}
      else
	return (Qnil);
    case G_TYPE_OBJECT:
      if (GTK_VALUE_OBJECT (*arg))
	return (build_gtk_object (GTK_VALUE_OBJECT (*arg)));
      else
	return (Qnil);
      //case G_TYPE_PARAM:
      //case G_TYPE_GTYPE
      

    case G_TYPE_CALLBACK:
      {
	Lisp_Object rval;

	rval = GET_LISP_FROM_VOID (GTK_VALUE_CALLBACK (*arg).data);

	return (rval);
      }

    default:
      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(arg->type, G_TYPE_LISTOF))
	{
	  if (!GTK_VALUE_POINTER (*arg))
	    return (Qnil);
	  else
	    {
	      return (xemacs_gtklist_to_list (arg));
	    }
	}
      stderr_out ("Do not know how to convert `%s' to lisp!\n", gtk_type_name (arg->type));
      ABORT ();
    }
  /* This is chuck reminding GCC to... SHUT UP! */
  return (Qnil);
}

int lisp_to_gtk_type (Lisp_Object obj, GtkArg *arg)
{
  switch (GTK_FUNDAMENTAL_TYPE (arg->type))
    {
      /* flag types */
    case G_TYPE_NONE:
      return (0);
    case G_TYPE_CHAR:
      {
	Ichar c;

	CHECK_CHAR_COERCE_INT (obj);
	c = XCHAR (obj);
	GTK_VALUE_CHAR (*arg) = c;
      }
      break;
    case G_TYPE_UCHAR:
      {
	Ichar c;

	CHECK_CHAR_COERCE_INT (obj);
	c = XCHAR (obj);
	GTK_VALUE_CHAR (*arg) = c;
      }
      break;
    case G_TYPE_BOOLEAN:
      GTK_VALUE_BOOL (*arg) = NILP (obj) ? FALSE : TRUE;
      break;
    case G_TYPE_INT:
    case G_TYPE_UINT:
      if (NILP (obj) || EQ (Qt, obj))
	{
	  /* For we are a kind mistress and allow sending t/nil for
             1/0 to stupid GTK functions that say they take guint or
             gint in the header files, but actually treat it like a
             bool.  *sigh*
	  */
	  GTK_VALUE_INT(*arg) = NILP (obj) ? 0 : 1;
	}
      else
	{
	  CHECK_INT (obj);
	  GTK_VALUE_INT(*arg) = XINT (obj);
	}
      break;
    case G_TYPE_LONG:
    case G_TYPE_ULONG:
      ABORT();
    case G_TYPE_FLOAT:
      CHECK_INT_OR_FLOAT (obj);
      GTK_VALUE_FLOAT(*arg) = extract_float (obj);
      break;
    case G_TYPE_DOUBLE:
      CHECK_INT_OR_FLOAT (obj);
      GTK_VALUE_DOUBLE(*arg) = extract_float (obj);
      break;
    case G_TYPE_STRING:
      if (NILP (obj))
	GTK_VALUE_STRING (*arg) = NULL;
      else
	{
	  CHECK_STRING (obj);
	  GTK_VALUE_STRING (*arg) = (char *) XSTRING_DATA (obj);
	}
      break;
    case G_TYPE_ENUM:
    case G_TYPE_FLAGS:
      /* Convert a lisp symbol to a GTK enum */
      GTK_VALUE_ENUM(*arg) = lisp_to_flag (obj, arg->type);
      break;
    case G_TYPE_BOXED:
      if (NILP (obj))
	{
	  GTK_VALUE_BOXED(*arg) = NULL;
	}
      else if (GTK_BOXEDP (obj))
	{
	  GTK_VALUE_BOXED(*arg) = XGTK_BOXED (obj)->object;
	}
      else if (arg->type == G_TYPE_STYLE)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  GTK_VALUE_BOXED(*arg) = face_to_style (obj);
	}
      else if (arg->type == G_TYPE_GDK_GC)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  GTK_VALUE_BOXED(*arg) = face_to_gc (obj);
	}
      else if (arg->type == G_TYPE_GDK_WINDOW)
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
		  GTK_VALUE_BOXED(*arg) = NULL;
		  break;

		case IMAGE_MONO_PIXMAP:
		case IMAGE_COLOR_PIXMAP:
		  GTK_VALUE_BOXED(*arg) = IMAGE_INSTANCE_GTK_PIXMAP (p);
		  break;
		}
	    }
	  else if (GTK_OBJECTP (obj) && GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	    {
	      GTK_VALUE_BOXED(*arg) = GTK_WIDGET (XGTK_OBJECT (obj))->window;
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert object to GDK_WINDOW", obj);
	    }
	  break;
	}
      else if (arg->type == G_TYPE_GDK_COLOR)
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
	      GTK_VALUE_BOXED(*arg) = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument ("Please use a color specifier or instance, not a string", obj);
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert to GdkColor", obj);
	    }
	}
      else if (arg->type == G_TYPE_GDK_FONT)
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
	      GTK_VALUE_BOXED(*arg) = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument ("Please use a font specifier or instance, not a string", obj);
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
	  GTK_VALUE_BOXED(*arg) = NULL;
	}
      break;

    case G_TYPE_POINTER:
      if (NILP (obj))
	GTK_VALUE_POINTER(*arg) = NULL;
      else
	GTK_VALUE_POINTER(*arg) = STORE_LISP_IN_VOID (obj);
      break;

      /* structured types */
    case G_TYPE_SIGNAL:
    case G_TYPE_ARGS: /* This we can do as a list of values */
    case G_TYPE_C_CALLBACK:
    case G_TYPE_FOREIGN:
      stderr_out ("Do not know how to convert `%s' from lisp!\n", gtk_type_name (arg->type));
      return (-1);

#if 0
      /* #### BILL! */
      /* This is not used, and does not work with union type */
    case G_TYPE_CALLBACK:
      {
	GUI_ID id;

	id = new_gui_id ();
	obj = Fcons (Qnil, obj); /* Empty data */
	obj = Fcons (make_int (id), obj);

	gcpro_popup_callbacks (id, obj);

	GTK_VALUE_CALLBACK(*arg).marshal = __internal_callback_marshal;
	GTK_VALUE_CALLBACK(*arg).data = (gpointer) obj;
	GTK_VALUE_CALLBACK(*arg).notify = __internal_callback_destroy;
      }
      break;
#endif

      /* base type of the object system */
    case G_TYPE_OBJECT:
      if (NILP (obj))
	GTK_VALUE_OBJECT (*arg) = NULL;
      else
	{
	  CHECK_GTK_OBJECT (obj);
	  if (XGTK_OBJECT (obj)->alive_p)
	    GTK_VALUE_OBJECT (*arg) = XGTK_OBJECT (obj)->object;
	  else
	    invalid_argument ("Attempting to pass dead object to GTK function", obj);
	}
      break;

    default:
      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(arg->type, GTK_TYPE_ARRAY))
	{
	  if (NILP (obj))
	    GTK_VALUE_POINTER(*arg) = NULL;
	  else
	    {
	      xemacs_list_to_array (obj, arg);
	    }
	}
      else if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(arg->type, GTK_TYPE_LISTOF))
	{
	  if (NILP (obj))
	    GTK_VALUE_POINTER(*arg) = NULL;
	  else
	    {
	      xemacs_list_to_gtklist (obj, arg);
	    }
	}
      else
	{
	  stderr_out ("Do not know how to convert `%s' from lisp!\n", gtk_type_name (arg->type));
	  ABORT();
	}
      break;
    }

  return (0);
}

/* Convert lisp types to GTK return types.  This is identical to
   lisp_to_gtk_type() except that the macro used to set the value is
   different.

   ### There should be some way of combining these two functions.
*/
int lisp_to_gtk_ret_type (Lisp_Object obj, GtkArg *arg)
{
  switch (GTK_FUNDAMENTAL_TYPE (arg->type))
    {
      /* flag types */
    case G_TYPE_NONE:
      return (0);
    case G_TYPE_CHAR:
      {
	Ichar c;

	CHECK_CHAR_COERCE_INT (obj);
	c = XCHAR (obj);
	*(GTK_RETLOC_CHAR (*arg)) = c;
      }
      break;
    case G_TYPE_UCHAR:
      {
	Ichar c;

	CHECK_CHAR_COERCE_INT (obj);
	c = XCHAR (obj);
	*(GTK_RETLOC_CHAR (*arg)) = c;
      }
      break;
    case G_TYPE_BOOLEAN:
      *(GTK_RETLOC_BOOL (*arg)) = NILP (obj) ? FALSE : TRUE;
      break;
    case G_TYPE_INT:
    case G_TYPE_UINT:
      if (NILP (obj) || EQ (Qt, obj))
	{
	  /* For we are a kind mistress and allow sending t/nil for
             1/0 to stupid GTK functions that say they take guint or
             gint in the header files, but actually treat it like a
             bool.  *sigh*
	  */
	  *(GTK_RETLOC_INT(*arg)) = NILP (obj) ? 0 : 1;
	}
      else
	{
	  CHECK_INT (obj);
	  *(GTK_RETLOC_INT(*arg)) = XINT (obj);
	}
      break;
    case G_TYPE_LONG:
    case G_TYPE_ULONG:
      ABORT();
    case G_TYPE_FLOAT:
      CHECK_INT_OR_FLOAT (obj);
      *(GTK_RETLOC_FLOAT(*arg)) = extract_float (obj);
      break;
    case G_TYPE_DOUBLE:
      CHECK_INT_OR_FLOAT (obj);
      *(GTK_RETLOC_DOUBLE(*arg)) = extract_float (obj);
      break;
    case G_TYPE_STRING:
      if (NILP (obj))
	*(GTK_RETLOC_STRING (*arg)) = NULL;
      else
	{
	  CHECK_STRING (obj);
	  *(GTK_RETLOC_STRING (*arg)) = (char *) XSTRING_DATA (obj);
	}
      break;
    case G_TYPE_ENUM:
    case G_TYPE_FLAGS:
      /* Convert a lisp symbol to a GTK enum */
      *(GTK_RETLOC_ENUM(*arg)) = lisp_to_flag (obj, arg->type);
      break;
    case G_TYPE_BOXED:
      if (NILP (obj))
	{
	  *(GTK_RETLOC_BOXED(*arg)) = NULL;
	}
      else if (GTK_BOXEDP (obj))
	{
	  *(GTK_RETLOC_BOXED(*arg)) = XGTK_BOXED (obj)->object;
	}
      else if (arg->type == G_TYPE_STYLE)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  *(GTK_RETLOC_BOXED(*arg)) = face_to_style (obj);
	}
      else if (arg->type == G_TYPE_GDK_GC)
	{
	  obj = Ffind_face (obj);
	  CHECK_FACE (obj);
	  *(GTK_RETLOC_BOXED(*arg)) = face_to_gc (obj);
	}
      else if (arg->type == G_TYPE_GDK_WINDOW)
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
		  *(GTK_RETLOC_BOXED(*arg)) = NULL;
		  break;

		case IMAGE_MONO_PIXMAP:
		case IMAGE_COLOR_PIXMAP:
		  *(GTK_RETLOC_BOXED(*arg)) = IMAGE_INSTANCE_GTK_PIXMAP (p);
		  break;
		}
	    }
	  else if (GTK_OBJECTP (obj) && GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	    {
	      *(GTK_RETLOC_BOXED(*arg)) = GTK_WIDGET (XGTK_OBJECT (obj))->window;
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert object to GDK_WINDOW", obj);
	    }
	  break;
	}
      else if (arg->type == G_TYPE_GDK_COLOR)
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
	      *(GTK_RETLOC_BOXED(*arg)) = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument ("Please use a color specifier or instance, not a string", obj);
	    }
	  else
	    {
	      invalid_argument ("Don't know how to convert to GdkColor", obj);
	    }
	}
      else if (arg->type == G_TYPE_GDK_FONT)
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
	      *(GTK_RETLOC_BOXED(*arg)) = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (obj));
	    }
	  else if (STRINGP (obj))
	    {
	      invalid_argument ("Please use a font specifier or instance, not a string", obj);
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
	  *(GTK_RETLOC_BOXED(*arg)) = NULL;
	}
      break;

    case G_TYPE_POINTER:
      if (NILP (obj))
	*(GTK_RETLOC_POINTER(*arg)) = NULL;
      else
	*(GTK_RETLOC_POINTER(*arg)) = STORE_LISP_IN_VOID (obj);
      break;

      /* structured types */
    case G_TYPE_SIGNAL:
    case G_TYPE_ARGS: /* This we can do as a list of values */
    case G_TYPE_C_CALLBACK:
    case G_TYPE_FOREIGN:
      stderr_out ("Do not know how to convert `%s' from lisp!\n", gtk_type_name (arg->type));
      return (-1);

#if 0
      /* #### BILL! */
      /* This is not used, and does not work with union type */
    case G_TYPE_CALLBACK:
      {
	GUI_ID id;

	id = new_gui_id ();
	obj = Fcons (Qnil, obj); /* Empty data */
	obj = Fcons (make_int (id), obj);

	gcpro_popup_callbacks (id, obj);

	*(GTK_RETLOC_CALLBACK(*arg)).marshal = __internal_callback_marshal;
	*(GTK_RETLOC_CALLBACK(*arg)).data = (gpointer) obj;
	*(GTK_RETLOC_CALLBACK(*arg)).notify = __internal_callback_destroy;
      }
      break;
#endif

      /* base type of the object system */
    case G_TYPE_OBJECT:
      if (NILP (obj))
	*(GTK_RETLOC_OBJECT (*arg)) = NULL;
      else
	{
	  CHECK_GTK_OBJECT (obj);
	  if (XGTK_OBJECT (obj)->alive_p)
	    *(GTK_RETLOC_OBJECT (*arg)) = XGTK_OBJECT (obj)->object;
	  else
	    invalid_argument ("Attempting to pass dead object to GTK function", obj);
	}
      break;

    default:
      if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(arg->type, G_TYPE_ARRAY))
	{
	  if (NILP (obj))
	    *(GTK_RETLOC_POINTER(*arg)) = NULL;
	  else
	    {
	      xemacs_list_to_array (obj, arg);
	    }
	}
      else if (IS_XEMACS_GTK_FUNDAMENTAL_TYPE(arg->type, G_TYPE_LISTOF))
	{
	  if (NILP (obj))
	    *(GTK_RETLOC_POINTER(*arg)) = NULL;
	  else
	    {
	      xemacs_list_to_gtklist (obj, arg);
	    }
	}
      else
	{
	  stderr_out ("Do not know how to convert `%s' from lisp!\n", gtk_type_name (arg->type));
	  ABORT();
	}
      break;
    }

  return (0);
}

/* This is used in glyphs-gtk.c as well */
static Lisp_Object
get_enumeration (GType t)
{
  Lisp_Object alist;

  if (NILP (Venumeration_info))
    {
      Venumeration_info = call2 (intern ("make-hashtable"), make_int (100), Qequal);
    }

  alist = Fgethash (make_int (t), Venumeration_info, Qnil);  

  if (NILP (alist))
    {
      import_gtk_enumeration_internal (t);
      alist = Fgethash (make_int (t), Venumeration_info, Qnil);
    }
  return (alist);
}

guint
symbol_to_enum (Lisp_Object obj, GType t)
{
  Lisp_Object alist = get_enumeration (t);
  Lisp_Object value = Qnil;

  if (NILP (alist))
    {
      invalid_argument ("Unknown enumeration", build_cistring (gtk_type_name (t)));
    }

  value = Fassq (obj, alist);

  if (NILP (value))
    {
      invalid_argument ("Unknown value", obj);
    }

  CHECK_INT (XCDR (value));

  return (XINT (XCDR (value)));
}

static guint
lisp_to_flag (Lisp_Object obj, GType t)
{
  guint val = 0;

  if (NILP (obj))
    {
      /* Do nothing */
    }
  else if (SYMBOLP (obj))
    {
      val = symbol_to_enum (obj, t);
    }
  else if (LISTP (obj))
    {
      while (!NILP (obj))
	{
	  val |= symbol_to_enum (XCAR (obj), t);
	  obj = XCDR (obj);
	}
    }
  else
    {
      /* ABORT ()? */
    }
  return (val);
}

static Lisp_Object
flags_to_list (guint value, GType t)
{
  Lisp_Object rval = Qnil;
  Lisp_Object alist = get_enumeration (t);

  while (!NILP (alist))
    {
      if (value & XINT (XCDR (XCAR (alist))))
	{
	  rval = Fcons (XCAR (XCAR (alist)), rval);
	  value &= ~(XINT (XCDR (XCAR (alist))));
	}
      alist = XCDR (alist);
    }
  return (rval);
}

static Lisp_Object
enum_to_symbol (guint value, GType t)
{
  Lisp_Object alist = get_enumeration (t);
  Lisp_Object cell = Qnil;

  if (NILP (alist))
    {
      invalid_argument ("Unknown enumeration", build_cistring (gtk_type_name (t)));
    }

  cell = Frassq (make_int (value), alist);

  return (NILP (cell) ? Qnil : XCAR (cell));
}
