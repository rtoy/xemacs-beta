/* ui-gtk.h
**
** Description: 
**
** Created by: William M. Perry
** Copyright (c) 2000 Aventail Corporation
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

#ifndef __UI_GTK_H__
#define __UI_GTK_H__

/* Encapsulate a foreign function call */

#include "sysgtk.h"
#include "sysdll.h"

typedef void (*ffi_actual_function) (void);
typedef void (*ffi_marshalling_function) (ffi_actual_function, GtkArg *);

#define MAX_GTK_ARGS 100

typedef struct {
  NORMAL_LISP_OBJECT_HEADER header;
  GType return_type;
  GType arg_type[MAX_GTK_ARGS];
  gint n_args;
  Lisp_Object function_name;
  dll_func function_ptr;
  ffi_marshalling_function marshal;
} emacs_ffi_data;

DECLARE_LISP_OBJECT (emacs_ffi, emacs_ffi_data);

#define FFI_RETURN_TYPE(d) ((d)->return_type)
#define FFI_ARG_TYPE(d, n) ((d)->arg_type[(n)])
#define FFI_N_ARGS(d) ((d)->n_args)
#define FFI_FUNCTION_NAME(d) ((d)->function_name)
#define FFI_FUNCTION_PTR(d) ((d)->function_ptr)
#define FFI_MARSHAL(d) ((d)->marshal)
#define XFFI(x) XRECORD (x, emacs_ffi, emacs_ffi_data)
#define XFFI_RETURN_TYPE(x) (XFFI (x)->return_type)
#define XFFI_ARG_TYPE(x, n) (XFFI (x)->arg_type[(n)])
#define XFFI_N_ARGS(x) (XFFI (x)->n_args)
#define XFFI_FUNCTION_NAME(x) (XFFI (x)->function_name)
#define XFFI_FUNCTION_PTR(x) (XFFI(x)->function_ptr)
#define XFFI_MARSHAL(x) (XFFI(x)->marshal)
#define wrap_emacs_ffi(p) wrap_record (p, emacs_ffi)
#define FFIP(x) RECORDP (x, emacs_ffi)
#define CHECK_FFI(x) CHECK_RECORD (x, emacs_ffi)

/* Encapsulate a GtkObject in Lisp */
typedef struct {
  NORMAL_LISP_OBJECT_HEADER header;
  gboolean alive_p;
  GObject *object;
  Lisp_Object plist;
} emacs_gtk_object_data;

DECLARE_LISP_OBJECT (emacs_gtk_object, emacs_gtk_object_data);

#define XGTK_OBJECT(x) XRECORD (x, emacs_gtk_object, emacs_gtk_object_data)
#define wrap_emacs_gtk_object(p) wrap_record (p, emacs_gtk_object)
#define GTK_OBJECTP(x) RECORDP (x, emacs_gtk_object)
#define CHECK_GTK_OBJECT(x) CHECK_RECORD (x, emacs_gtk_object)

extern Lisp_Object build_gtk_object (GObject *obj);

/* Encapsulate a G_TYPE_BOXED in lisp */
typedef struct {
  NORMAL_LISP_OBJECT_HEADER header;
  GType object_type;
  void *object;
} emacs_gtk_boxed_data;

DECLARE_LISP_OBJECT (emacs_gtk_boxed, emacs_gtk_boxed_data);

#define XGTK_BOXED(x) XRECORD (x, emacs_gtk_boxed, emacs_gtk_boxed_data)
#define wrap_emacs_gtk_boxed(p) wrap_record (p, emacs_gtk_boxed)
#define GTK_BOXEDP(x) RECORDP (x, emacs_gtk_boxed)
#define CHECK_GTK_BOXED(x) CHECK_RECORD (x, emacs_gtk_boxed)

extern Lisp_Object build_gtk_boxed (void *obj, GType t);

#ifdef JSPARKES
/*
 * A convenience macro for boxed type implementations, which defines a
 * type_name_get_type() function registering the boxed type.
 * From poppler originally
 */
#define DEFINE_BOXED_TYPE(TypeName, type_name, copy_func, free_func) \
GType                                                                      \
type_name##_get_type (void)                                                \
{                                                                          \
  static volatile gsize g_type_id = 0;                                     \
  if (g_once_init_enter (g_type_id)) {                                     \
    GType g_define_type_id =                                               \
      g_boxed_type_register_static (g_intern_static_string (#TypeName),    \
				    (GBoxedCopyFunc) copy_func,            \
				    (GBoxedFreeFunc) free_func);           \
    g_once_init_leave (&g_type_id, g_define_type_id);                      \
  }                                                                        \
  return g_typeid;                                                         \
}

/*
 * Create boxed GObject types where copy and free simply adjust reference
 * counters.
 */
#define DEFINE_GOBJECT_BOXED_TYPE(TypeName, type_name) \
  DEFINE_BOXED_TYPE(TypeName, type_name, g_object_ref, g_object_unref)
#endif
#endif /* __UI_GTK_H__ */
