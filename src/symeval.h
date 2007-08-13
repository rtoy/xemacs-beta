/* Definitions of symbol-value forwarding for XEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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

/* Fsymbol_value checks whether XSYMBOL (sym)->value is one of these,
 *  and does weird magic stuff if so */

#ifndef _XEMACS_SYMEVAL_H_
#define _XEMACS_SYMEVAL_H_

struct symbol_value_magic
{
  struct lcrecord_header lcheader;
  enum
    {
      /* The following tags use the 'symbol_value_forward' structure
	 and are strictly for variables DEFVARed on the C level. */
      SYMVAL_FIXNUM_FORWARD,           /* Forward C "int" */
      SYMVAL_CONST_FIXNUM_FORWARD,     /* Same, but can't be set */
      SYMVAL_BOOLEAN_FORWARD,          /* Forward C boolean ("int") */
      SYMVAL_CONST_BOOLEAN_FORWARD,    /* Same, but can't be set */
      SYMVAL_OBJECT_FORWARD,           /* Forward C Lisp_Object */
      SYMVAL_CONST_OBJECT_FORWARD,     /* Same, but can't be set */
      SYMVAL_CONST_SPECIFIER_FORWARD,  /* Same, can't be set, but gives
					  a different message when attempting
					  to set that says "use set-specifier"
					  */
      SYMVAL_DEFAULT_BUFFER_FORWARD,   /* Forward Lisp_Object into
					  Vbuffer_defaults */
      SYMVAL_CURRENT_BUFFER_FORWARD,   /* Forward Lisp_Object into
					  current_buffer */
      SYMVAL_CONST_CURRENT_BUFFER_FORWARD,   /* Forward Lisp_Object into
						current_buffer, can't
						be set */
      SYMVAL_DEFAULT_CONSOLE_FORWARD,	/* Forward Lisp_Object into
					   Vconsole_defaults */
      SYMVAL_SELECTED_CONSOLE_FORWARD,   /* Forward Lisp_Object into
					     Vselected_console */
      SYMVAL_CONST_SELECTED_CONSOLE_FORWARD,   /* Forward Lisp_Object into
						   Vselected_console, can't
						   be set */
      SYMVAL_UNBOUND_MARKER,           /* Only Qunbound actually has this
					  tag */

      /* The following tags use the 'symbol_value_buffer_local' structure */
      SYMVAL_BUFFER_LOCAL,             /* make-variable-buffer-local */
      SYMVAL_SOME_BUFFER_LOCAL,        /* make-local-variable */

      /* The following tag uses the 'symbol_value_lisp_magic' structure */
      SYMVAL_LISP_MAGIC,	       /* Forward to lisp callbacks */

      /* The following tag uses the 'symbol_value_varalias' structure */
      SYMVAL_VARALIAS		       /* defvaralias */

#if 0
      /* NYI */
      SYMVAL_CONSTANT_SYMBOL,	       /* Self-evaluating symbol */
      /* NYI */
#endif
    } type;
};
#define SYMBOL_VALUE_MAGIC_P(x)				\
  (LRECORDP (x)						\
   && (XRECORD_LHEADER (x)->implementation->printer	\
       == print_symbol_value_magic))
#define XSYMBOL_VALUE_MAGIC_TYPE(v) \
	(((struct symbol_value_magic *) XPNTR (v))->type)
#define XSETSYMBOL_VALUE_MAGIC(s, p) XSETOBJ (s, Lisp_Record, p)
extern void print_symbol_value_magic (Lisp_Object, Lisp_Object, int);

/********** The various different symbol-value-magic types ***********/

/* 1. symbol-value-forward */

/* This type of symbol-value-magic is used for variables declared
   DEFVAR_LISP, DEFVAR_INT, DEFVAR_BOOL, DEFVAR_BUFFER_LOCAL,
   DEFVAR_BUFFER_DEFAULTS, DEFVAR_SPECIFIER, and for Qunbound.

   Note that some of these types of variables can be made buffer-local.
   Then, the symbol's value field contains a symbol-value-buffer-local,
   whose CURRENT-VALUE field then contains a symbol-value-forward.
 */
     
extern CONST_IF_NOT_DEBUG struct lrecord_implementation
  lrecord_symbol_value_forward[];
struct symbol_value_forward
{
  struct symbol_value_magic magic;
  /* void *forward; -- use magic.lcheader.next instead */
  /* Function controlling magic behavior of this forward variable.

     SYM is the symbol being operated on (read, set, etc.);

     VAL is either the value to set or the value to be returned.

     IN_OBJECT is the buffer or console that the value is read in
       or set in.  A value of Qnil means that the current buffer
       and possibly other buffers are being set. (This value will
       never be passed for built-in buffer-local or console-local
       variables such as `truncate-lines'.) (Currently, a value of
       Qnil is always passed for DEFVAR_INT, DEFVAR_LISP, and
       DEFVAR_BOOL variables; the code isn't smart enough to figure
       out what buffers besides the current buffer are being
       affected.  Because the magic function is called
       before the value is changed, it's not that easy
       to determine which buffers are getting changed.
       #### If this information is important, let me know
       and I will look into providing it.) (Remember also
       that the only console-local variables currently existing
       are built-in ones, because others can't be created.)

     FLAGS gives more information about the operation being
       performed.

     The return value indicates what the magic function actually
       did.

     Currently FLAGS and the return value are not used.  This
     function is only called when the value of a forward variable
     is about to be changed.  Note that this can occur explicitly
     through a call to `set', `setq', `set-default', or `setq-default',
     or implicitly by the current buffer being changed.

     */
     
  int (*magicfun) (Lisp_Object sym, Lisp_Object *val, Lisp_Object in_object,
		   int flags);
};
#define XSYMBOL_VALUE_FORWARD(v) \
	((CONST struct symbol_value_forward *) XPNTR(v))
#define symbol_value_forward_forward(m) ((void *)((m)->magic.lcheader.next))
#define symbol_value_forward_magicfun(m) ((m)->magicfun)

/* 2. symbol-value-buffer-local */

extern CONST_IF_NOT_DEBUG struct lrecord_implementation
  lrecord_symbol_value_buffer_local[];
struct symbol_value_buffer_local
{
  struct symbol_value_magic magic;
  /* Used in a symbol value cell when the symbol's value is per-buffer.

     The type of the symbol-value-magic will be either
     SYMVAL_BUFFER_LOCAL (i.e. `make-variable-buffer-local' was called)
     or SYMVAL_SOME_BUFFER_LOCAL (i.e. `make-local-variable' was called).
     The only difference between the two is that when setting the
     former kind of variable, an implicit `make-local-variable' is
     called.

     A buffer-local variable logically has

     -- a default value
     -- local values in some buffers

     The primary place where the local values are stored is in each
     buffer's local_var_alist slot.

     In the simplest implementation, all that this structure needs to
     keep track of is the default value; to retrieve the value in
     a buffer, look in that buffer's local_var_alist, and use the
     default value if there is no local value.  To implement
     `make-local-variable' in a buffer, look in the buffer's
     local_var_alist, and if no element exists for this symbol,
     add one, copying the value from the default value.  When setting
     the value in a buffer, look in the buffer's local_var_alist, and set
     the value in that list if an element exists for this symbol;
     otherwise, set the default. (Remember that SYMVAL_BUFFER_LOCAL
     variables implicitly call `make-local-variable' first, so when
     setting a value, there will always be an entry in the buffer's
     local_var_alist to set.)

     However, these operation is potentially slow.  To speed it up,
     we cache the value in one buffer in this structure.

     NOTE: This is *not* a write-through cache.  I.e. when setting
     the value in the buffer that is cached, we *only* change the
     cache and don't write the value through to either the buffer's
     local_var_alist or the default value.  Therefore, when retrieving
     a value in a buffer, you must *always* look in the cache to see if
     it refers to that buffer.

     The cache consists of

     -- a buffer, or nil if the cache has not been set up
     -- the value in that buffer
     -- the element (a cons) from the buffer's local_var_alist, or
        nil if there is no local value in the buffer

    These slots are called CURRENT-BUFFER, CURRENT-VALUE, and
    CURRENT-ALIST-ELEMENT, respectively.
   
    If we want to examine or set the value in BUFFER and CURRENT-BUFFER
    equals BUFFER, we just examine or set CURRENT-VALUE.  Otherwise,
    we store CURRENT-VALUE value into CURRENT-ALIST-ELEMENT (or maybe
    into DEFAULT-VALUE), then find the appropriate alist element for
    BUFFER and set up CURRENT-ALIST-ELEMENT.  Then we set CURRENT-VALUE
    out of that element (or maybe out of DEFAULT-VALUE), and store
    BUFFER into CURRENT-BUFFER.
    
    If we are setting the variable and the current buffer does not have
    an alist entry for this variable, an alist entry is created.
    
    Note that CURRENT-BUFFER's local_var_alist value for this variable
    might be out-of-date (the correct value is stored in CURRENT-VALUE).
    Similarly, if CURRENT-BUFFER sees the default value, then
    DEFAULT-VALUE might be out-of-date.
    
    Note that CURRENT-VALUE (but not DEFAULT-VALUE) can be a
    forwarding pointer.  Each time it is examined or set, 
    forwarding must be done.
   */
  Lisp_Object default_value;
  Lisp_Object current_value;
  Lisp_Object current_buffer;
  Lisp_Object current_alist_element;
};
#define XSYMBOL_VALUE_BUFFER_LOCAL(v) \
	((struct symbol_value_buffer_local *) XPNTR(v))
#define SYMBOL_VALUE_BUFFER_LOCAL_P(v)			\
  (SYMBOL_VALUE_MAGIC_P (v) &&				\
   (XSYMBOL_VALUE_MAGIC_TYPE (v) == SYMVAL_BUFFER_LOCAL \
    || XSYMBOL_VALUE_MAGIC_TYPE (v) == SYMVAL_SOME_BUFFER_LOCAL))

/* 3. symbol-value-lisp-magic */

extern CONST_IF_NOT_DEBUG struct lrecord_implementation
  lrecord_symbol_value_lisp_magic[];
enum lisp_magic_handler
{
  MAGIC_HANDLER_GET_VALUE,
  MAGIC_HANDLER_SET_VALUE,
  MAGIC_HANDLER_BOUND_PREDICATE,
  MAGIC_HANDLER_MAKE_UNBOUND,
  MAGIC_HANDLER_LOCAL_PREDICATE,
  MAGIC_HANDLER_MAKE_LOCAL,
  MAGIC_HANDLER_MAX
};

struct symbol_value_lisp_magic
{
  struct symbol_value_magic magic;
  Lisp_Object handler[MAGIC_HANDLER_MAX];
  Lisp_Object harg[MAGIC_HANDLER_MAX];
  Lisp_Object shadowed;
};
#define XSYMBOL_VALUE_LISP_MAGIC(v)			\
	((struct symbol_value_lisp_magic *) XPNTR (v))
#define SYMBOL_VALUE_LISP_MAGIC_P(v)			\
  (SYMBOL_VALUE_MAGIC_P (v) &&				\
   XSYMBOL_VALUE_MAGIC_TYPE (v) == SYMVAL_LISP_MAGIC)

/* 4. symbol-value-varalias */

extern CONST_IF_NOT_DEBUG struct lrecord_implementation
  lrecord_symbol_value_varalias[];
struct symbol_value_varalias
{
  struct symbol_value_magic magic;
  Lisp_Object aliasee;
  Lisp_Object shadowed;
};
#define XSYMBOL_VALUE_VARALIAS(v)			\
	((struct symbol_value_varalias *) XPNTR (v))
#define SYMBOL_VALUE_VARALIAS_P(v)			\
  (SYMBOL_VALUE_MAGIC_P (v) &&				\
   XSYMBOL_VALUE_MAGIC_TYPE (v) == SYMVAL_VARALIAS)
#define symbol_value_varalias_aliasee(m) ((m)->aliasee)
#define symbol_value_varalias_shadowed(m) ((m)->shadowed)

/* DEFSUBR (Fname);
 is how we define the symbol for function `Fname' at start-up time. */
#define DEFSUBR(Fname) defsubr (&S##Fname)
extern void defsubr (struct Lisp_Subr *);

extern void defsymbol (Lisp_Object *location, CONST char *name);

extern void defkeyword (Lisp_Object *location, CONST char *name);

extern void deferror (Lisp_Object *symbol, CONST char *name,
		      CONST char *message, Lisp_Object inherits_from);

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.  */

extern void defvar_mumble (CONST char *names,
                           CONST void *magic, int sizeof_magic);

#define DEFVAR_HEADER(lname, c_location, forward_type)			  \
  static CONST struct symbol_value_forward I_hate_C			  \
   = { { { { lrecord_symbol_value_forward }, (void *) (c_location), 69 }, \
         forward_type }, 0 };						  \
  defvar_mumble ((lname), &I_hate_C, sizeof (I_hate_C))

#define DEFVAR_MAGIC_HEADER(lname, c_location, forward_type, magicfun)	  \
  static CONST struct symbol_value_forward I_hate_C			  \
   = { { { { lrecord_symbol_value_forward }, (void *) (c_location), 69 }, \
         forward_type }, magicfun };					  \
  defvar_mumble ((lname), &I_hate_C, sizeof (I_hate_C))

/* These discard their DOC arg because it is snarfed by make-docfile
 *  and stored in an external file. */

#define DEFVAR_LISP(lname, c_location)				\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_OBJECT_FORWARD);	\
      staticpro (c_location);					\
 } while (0)
#define DEFVAR_CONST_LISP(lname, c_location)				\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_CONST_OBJECT_FORWARD);	\
      staticpro (c_location);						\
 } while (0)
#define DEFVAR_SPECIFIER(lname, c_location)				 \
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_CONST_SPECIFIER_FORWARD); \
      staticpro (c_location);						 \
 } while (0)
#define DEFVAR_INT(lname, c_location)				\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_FIXNUM_FORWARD);	\
 } while (0)
#define DEFVAR_CONST_INT(lname, c_location)			\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_CONST_FIXNUM_FORWARD);	\
 } while (0)
#define DEFVAR_BOOL(lname, c_location)				\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_BOOLEAN_FORWARD);\
 } while (0)
#define DEFVAR_CONST_BOOL(lname, c_location)			\
 do { DEFVAR_HEADER (lname, c_location, SYMVAL_CONST_BOOLEAN_FORWARD);	\
 } while (0)

#define DEFVAR_LISP_MAGIC(lname, c_location, magicfun)		\
 do { DEFVAR_MAGIC_HEADER (lname, c_location,			\
      SYMVAL_OBJECT_FORWARD, magicfun);				\
      staticpro (c_location);					\
 } while (0)
#define DEFVAR_INT_MAGIC(lname, c_location, magicfun)		\
 do { DEFVAR_MAGIC_HEADER (lname, c_location,			\
      SYMVAL_FIXNUM_FORWARD, magicfun);				\
 } while (0)
#define DEFVAR_BOOL_MAGIC(lname, c_location, magicfun)		\
 do { DEFVAR_MAGIC_HEADER (lname, c_location,			\
      SYMVAL_BOOLEAN_FORWARD, magicfun);			\
 } while (0)

#endif /* _XEMACS_SYMEVAL_H_ */
