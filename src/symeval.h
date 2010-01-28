/* Definitions of symbol-value forwarding for XEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

#ifndef INCLUDED_symeval_h_
#define INCLUDED_symeval_h_

BEGIN_C_DECLS

enum symbol_value_type
{
  /* The following tags use the 'symbol_value_forward' structure
     and are strictly for variables DEFVARed on the C level. */
  SYMVAL_FIXNUM_FORWARD,	/* Forward C "Fixnum", really "EMACS_INT" */
  SYMVAL_CONST_FIXNUM_FORWARD,	/* Same, but can't be set */
  SYMVAL_BOOLEAN_FORWARD,	/* Forward C boolean ("int") */
  SYMVAL_CONST_BOOLEAN_FORWARD,	/* Same, but can't be set */
  SYMVAL_OBJECT_FORWARD,	/* Forward C Lisp_Object */
  SYMVAL_CONST_OBJECT_FORWARD,	/* Same, but can't be set */
  SYMVAL_CONST_SPECIFIER_FORWARD, /* Same, can't be set, but gives a
                                     different message when attempting to
				     set that says "use set-specifier" */
  SYMVAL_DEFAULT_BUFFER_FORWARD, /* Forward Lisp_Object into Vbuffer_defaults */
  SYMVAL_CURRENT_BUFFER_FORWARD, /* Forward Lisp_Object into current_buffer */
  SYMVAL_CONST_CURRENT_BUFFER_FORWARD, /* Forward Lisp_Object into
					  current_buffer, can't be set */
  SYMVAL_DEFAULT_CONSOLE_FORWARD, /* Forward Lisp_Object into
				     Vconsole_defaults */
  SYMVAL_SELECTED_CONSOLE_FORWARD, /* Forward Lisp_Object into
				      Vselected_console */
  SYMVAL_CONST_SELECTED_CONSOLE_FORWARD, /* Forward Lisp_Object into
					    Vselected_console,
					    can't be set */
  SYMVAL_UNBOUND_MARKER,	/* Only Qunbound actually has this tag */

  /* The following tags use the 'symbol_value_buffer_local' structure */
  SYMVAL_BUFFER_LOCAL,		/* make-variable-buffer-local */
  SYMVAL_SOME_BUFFER_LOCAL,	/* make-local-variable */

  /* The following tag uses the 'symbol_value_lisp_magic' structure */
  SYMVAL_LISP_MAGIC,		/* Forward to lisp callbacks */

  /* The following tag uses the 'symbol_value_varalias' structure */
  SYMVAL_VARALIAS		/* defvaralias */

#if 0
  /* NYI */
  SYMVAL_CONSTANT_SYMBOL,	/* Self-evaluating symbol */
  /* NYI */
#endif
};

/* Underlying C type used to implement DEFVAR_INT */
typedef EMACS_INT Fixnum;

struct symbol_value_magic
{
  struct LCRECORD_HEADER header;
  void *value;
  enum symbol_value_type type;
};
#define SYMBOL_VALUE_MAGIC_P(x)						\
(LRECORDP (x) &&							\
 XRECORD_LHEADER (x)->type <= lrecord_type_max_symbol_value_magic)
#define XSYMBOL_VALUE_MAGIC_TYPE(v) \
	(((struct symbol_value_magic *) XPNTR (v))->type)
#define wrap_symbol_value_magic(p) wrap_pointer_1 (p)
void print_symbol_value_magic (Lisp_Object, Lisp_Object, int);

/********** The various different symbol-value-magic types ***********/

/* 1. symbol-value-forward */

/* This type of symbol-value-magic is used for variables declared
   DEFVAR_LISP, DEFVAR_INT, DEFVAR_BOOL, DEFVAR_BUFFER_LOCAL,
   DEFVAR_BUFFER_DEFAULTS, DEFVAR_SPECIFIER, and for Qunbound.

   Note that some of these types of variables can be made buffer-local.
   Then, the symbol's value field contains a symbol-value-buffer-local,
   whose CURRENT-VALUE field then contains a symbol-value-forward.
 */

struct symbol_value_forward
{
  struct symbol_value_magic magic;

  /* `magicfun' is a function controlling the magic behavior of this
      forward variable.

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

     FLAGS gives more information about the operation being performed.

     The return value indicates what the magic function actually did.

     Currently FLAGS and the return value are not used.  This
     function is only called when the value of a forward variable
     is about to be changed.  Note that this can occur explicitly
     through a call to `set', `setq', `set-default', or `setq-default',
     or implicitly by the current buffer being changed.  */
  int (*magicfun) (Lisp_Object sym, Lisp_Object *val, Lisp_Object in_object,
		   int flags);
};
DECLARE_LRECORD (symbol_value_forward, struct symbol_value_forward);
#define XSYMBOL_VALUE_FORWARD(x) \
	XRECORD (x, symbol_value_forward, struct symbol_value_forward)
#define symbol_value_forward_forward(m) ((void *)((m)->magic.value))
#define symbol_value_forward_magicfun(m) ((m)->magicfun)

/* 2. symbol-value-buffer-local */

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

     However, this operation is potentially slow.  To speed it up,
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
DECLARE_LRECORD (symbol_value_buffer_local, struct symbol_value_buffer_local);
#define XSYMBOL_VALUE_BUFFER_LOCAL(x) \
	XRECORD (x, symbol_value_buffer_local, struct symbol_value_buffer_local)
#define SYMBOL_VALUE_BUFFER_LOCAL_P(x) RECORDP (x, symbol_value_buffer_local)

/* 3. symbol-value-lisp-magic */

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
DECLARE_LRECORD (symbol_value_lisp_magic, struct symbol_value_lisp_magic);
#define XSYMBOL_VALUE_LISP_MAGIC(x) \
	XRECORD (x, symbol_value_lisp_magic, struct symbol_value_lisp_magic)
#define SYMBOL_VALUE_LISP_MAGIC_P(x) RECORDP (x, symbol_value_lisp_magic)

/* 4. symbol-value-varalias */

struct symbol_value_varalias
{
  struct symbol_value_magic magic;
  Lisp_Object aliasee;
  Lisp_Object shadowed;
};
DECLARE_LRECORD (symbol_value_varalias,	struct symbol_value_varalias);
#define XSYMBOL_VALUE_VARALIAS(x) \
	XRECORD (x, symbol_value_varalias, struct symbol_value_varalias)
#define SYMBOL_VALUE_VARALIAS_P(x) RECORDP (x, symbol_value_varalias)
#define symbol_value_varalias_aliasee(m) ((m)->aliasee)
#define symbol_value_varalias_shadowed(m) ((m)->shadowed)

/* To define a Lisp primitive function using a C function `Fname', do this:
   DEFUN ("name, Fname, ...); // at top level in foo.c
   DEFSUBR (Fname);           // in syms_of_foo();
*/
#ifdef NEW_GC
MODULE_API void defsubr (Lisp_Subr *);
#define DEFSUBR_MC_ALLOC(Fname)						\
  S##Fname= (struct Lisp_Subr *) mc_alloc (sizeof (struct Lisp_Subr));	\
  set_lheader_implementation (&S##Fname->lheader, &lrecord_subr);	\
									\
  S##Fname->min_args = MC_ALLOC_S##Fname.min_args;			\
  S##Fname->max_args = MC_ALLOC_S##Fname.max_args;			\
  S##Fname->prompt = MC_ALLOC_S##Fname.prompt;				\
  S##Fname->doc = MC_ALLOC_S##Fname.doc;				\
  S##Fname->name = MC_ALLOC_S##Fname.name;				\
  S##Fname->subr_fn = MC_ALLOC_S##Fname.subr_fn;			\
  MARK_LRECORD_AS_LISP_READONLY (S##Fname);


#define DEFSUBR(Fname)				\
do {						\
  DEFSUBR_MC_ALLOC (Fname);			\
  defsubr (S##Fname);				\
} while (0)

/* To define a Lisp primitive macro using a C function `Fname', do this:
   DEFUN ("name, Fname, ...); // at top level in foo.c
   DEFSUBR_MACRO (Fname);     // in syms_of_foo();
*/
MODULE_API void defsubr_macro (Lisp_Subr *);
#define DEFSUBR_MACRO(Fname)			\
do {						\
  DEFSUBR_MC_ALLOC (Fname);			\
  defsubr_macro (S##Fname);			\
} while (0)

#else /* not NEW_GC */
/* To define a Lisp primitive function using a C function `Fname', do this:
   DEFUN ("name, Fname, ...); // at top level in foo.c
   DEFSUBR (Fname);           // in syms_of_foo();
*/
MODULE_API void defsubr (Lisp_Subr *);
#define DEFSUBR(Fname) defsubr (&S##Fname)

/* To define a Lisp primitive macro using a C function `Fname', do this:
   DEFUN ("name, Fname, ...); // at top level in foo.c
   DEFSUBR_MACRO (Fname);     // in syms_of_foo();
*/
MODULE_API void defsubr_macro (Lisp_Subr *);
#define DEFSUBR_MACRO(Fname) defsubr_macro (&S##Fname)
#endif /* not NEW_GC */

MODULE_API void defsymbol_massage_name (Lisp_Object *location,
					const Ascbyte *name);
MODULE_API void defsymbol_massage_name_nodump (Lisp_Object *location,
					       const Ascbyte *name);
MODULE_API void defsymbol_massage_multiword_predicate (Lisp_Object *location,
						       const Ascbyte *name);
MODULE_API void
defsymbol_massage_multiword_predicate_nodump (Lisp_Object *location,
					      const Ascbyte *name);
MODULE_API void defsymbol (Lisp_Object *location, const Ascbyte *name);
MODULE_API void defsymbol_nodump (Lisp_Object *location, const Ascbyte *name);

/* Defining symbols:

   (1) A standard symbol is defined with DEFSYMBOL.  That means that
       the symbol's print name can be derived from the symbol's variable
       name by removing the initial Q and replacing underscores with hyphens.
   (2) A keyword symbol is defined with DEFKEYWORD.  That means that
       the symbol's print name can be derived from the symbol's variable
       name by removing the initial Q and replacing underscores with hyphens,
       except that the initial underscore, which comes directly after the Q,
       is replaced by a colon.
   (3) DEFSYMBOL_MULTIWORD_PREDICATE is used for the predicates that are
       associated with a particular type of Lisp Object.  Because of the
       limitations of C macros, they're always given a predicate symbol
       whose C name simply appends `p' to the type name, modulo hyphen/
       underscore conversion.  Properly, however, the Lisp name should have
       `-p' if there is more than one word in the type name.
       DEFSYMBOL_MULTIWORD_PREDICATE is for these weird symbols -- the
       C name as supplied to the macro should end with a `p' with no
       underscore before it, and the macro will insert a hyphen there in
       the Lisp name.
   (4) In case you have some weird symbol where the equivalence between
       the C and Lisp names is more complicated (e.g. the Lisp symbol has
       non-alphabetic, non-numeric characters in it), you can just call
       defsymbol() (the lowercase version) directly.
*/

#define DEFSYMBOL(name) defsymbol_massage_name (&name, #name)
#define DEFSYMBOL_NO_DUMP(name) defsymbol_massage_name_nodump (&name, #name)
#define DEFSYMBOL_MULTIWORD_PREDICATE(name) \
  defsymbol_massage_multiword_predicate (&name, #name)
#define DEFSYMBOL_MULTIWORD_PREDICATE_NO_DUMP(name) \
  defsymbol_massage_multiword_predicate_nodump (&name, #name)

MODULE_API void defkeyword (Lisp_Object *location, const Ascbyte *name);
MODULE_API void defkeyword_massage_name (Lisp_Object *location,
					 const Ascbyte *name);
#define DEFKEYWORD(name) defkeyword_massage_name (&name, #name)

MODULE_API void deferror (Lisp_Object *symbol, const Ascbyte *name,
			  const Ascbyte *message, Lisp_Object inherits_from);
MODULE_API void deferror_massage_name (Lisp_Object *symbol, const Ascbyte *name,
				       const Ascbyte *message,
				       Lisp_Object inherits_from);
MODULE_API void deferror_massage_name_and_message (Lisp_Object *symbol,
						   const Ascbyte *name,
						   Lisp_Object inherits_from);
#define DEFERROR(name, message, inherits_from) \
  deferror_massage_name (&name, #name, message, inherits_from)
/* In this case, the error message is the same as the name, modulo some
   prettifying */
#define DEFERROR_STANDARD(name, inherits_from) \
  deferror_massage_name_and_message (&name, #name, inherits_from)

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.  */

MODULE_API void defvar_magic (const Ascbyte *symbol_name,
			      const struct symbol_value_forward *magic);

#ifdef NEW_GC
#define DEFVAR_SYMVAL_FWD(lname, c_location, forward_type, magic_fun)	\
do									\
{									\
  struct symbol_value_forward *I_hate_C =				\
    alloc_lrecord_type (struct symbol_value_forward,			\
		        &lrecord_symbol_value_forward);			\
  /*  mcpro ((Lisp_Object) I_hate_C);*/					\
									\
  MARK_LRECORD_AS_LISP_READONLY (I_hate_C);				\
									\
  I_hate_C->magic.value = c_location;					\
  I_hate_C->magic.type = forward_type;					\
  I_hate_C->magicfun = magic_fun;					\
									\
  defvar_magic ((lname), I_hate_C);					\
} while (0)
#else /* not NEW_GC */
#define DEFVAR_SYMVAL_FWD(lname, c_location, forward_type, magicfun)	\
do									\
{									\
  static const struct symbol_value_forward I_hate_C =			\
  { /* struct symbol_value_forward */					\
    { /* struct symbol_value_magic */					\
      { /* struct old_lcrecord_header */				\
	{ /* struct lrecord_header */					\
	  lrecord_type_symbol_value_forward, /* lrecord_type_index */	\
	  1, /* mark bit */						\
	  1, /* c_readonly bit */					\
	  1, /* lisp_readonly bit */					\
          0  /* unused */                                               \
	},								\
	0, /* next */							\
	0, /* uid  */							\
	0  /* free */							\
      },								\
      c_location,							\
      forward_type							\
    },									\
    magicfun								\
  };									\
  defvar_magic ((lname), &I_hate_C);					\
} while (0)
#endif /* not NEW_GC */
#define DEFVAR_SYMVAL_FWD_INT(lname, c_location, forward_type, magicfun) \
do									 \
{									 \
  DEFVAR_SYMVAL_FWD (lname, c_location, forward_type, magicfun);	 \
  dump_add_opaque_int (c_location);					 \
} while (0)

#define DEFVAR_SYMVAL_FWD_FIXNUM(lname, c_location, forward_type, magicfun) \
do									    \
{									    \
  DEFVAR_SYMVAL_FWD (lname, c_location, forward_type, magicfun);	    \
  dump_add_opaque_fixnum (c_location);					    \
} while (0)

#define DEFVAR_SYMVAL_FWD_OBJECT(lname, c_location, forward_type, magicfun) \
do									    \
{									    \
  DEFVAR_SYMVAL_FWD (lname, c_location, forward_type, magicfun);	    \
  {									    \
    Lisp_Object *DSF_location = c_location; /* Type check */		    \
    staticpro (DSF_location);						    \
    if (EQ (*DSF_location, Qnull_pointer)) *DSF_location = Qnil;	    \
  }									    \
} while (0)

#define DEFVAR_LISP(lname, c_location) \
	DEFVAR_SYMVAL_FWD_OBJECT (lname, c_location, SYMVAL_OBJECT_FORWARD, 0)
#define DEFVAR_CONST_LISP(lname, c_location) \
	DEFVAR_SYMVAL_FWD_OBJECT (lname, c_location, SYMVAL_CONST_OBJECT_FORWARD, 0)
#define DEFVAR_SPECIFIER(lname, c_location) \
	DEFVAR_SYMVAL_FWD_OBJECT (lname, c_location, SYMVAL_CONST_SPECIFIER_FORWARD, 0)
#define DEFVAR_INT(lname, c_location) \
	DEFVAR_SYMVAL_FWD_FIXNUM (lname, c_location, SYMVAL_FIXNUM_FORWARD, 0)
#define DEFVAR_CONST_INT(lname, c_location) \
	DEFVAR_SYMVAL_FWD_FIXNUM (lname, c_location, SYMVAL_CONST_FIXNUM_FORWARD, 0)
#define DEFVAR_BOOL(lname, c_location) \
	DEFVAR_SYMVAL_FWD_INT (lname, c_location, SYMVAL_BOOLEAN_FORWARD, 0)
#define DEFVAR_CONST_BOOL(lname, c_location) \
	DEFVAR_SYMVAL_FWD_INT (lname, c_location, SYMVAL_CONST_BOOLEAN_FORWARD, 0)
#define DEFVAR_LISP_MAGIC(lname, c_location, magicfun) \
	DEFVAR_SYMVAL_FWD_OBJECT (lname, c_location, SYMVAL_OBJECT_FORWARD, magicfun)
#define DEFVAR_INT_MAGIC(lname, c_location, magicfun) \
	DEFVAR_SYMVAL_FWD_FIXNUM (lname, c_location, SYMVAL_FIXNUM_FORWARD, magicfun)
#define DEFVAR_BOOL_MAGIC(lname, c_location, magicfun) \
	DEFVAR_SYMVAL_FWD_INT (lname, c_location, SYMVAL_BOOLEAN_FORWARD, magicfun)

void flush_all_buffer_local_cache (void);

struct multiple_value {
  struct LCRECORD_HEADER header;
  Elemcount count;
  Elemcount allocated_count; 
  Elemcount first_desired;
  Lisp_Object contents[1];
};
typedef struct multiple_value multiple_value;

DECLARE_LRECORD (multiple_value, multiple_value);
#define MULTIPLE_VALUEP(x) RECORDP (x, multiple_value)

#define XMULTIPLE_VALUE(x) XRECORD (x, multiple_value, multiple_value)
#define wrap_multiple_value(p) wrap_record (p, multiple_value)

#define CHECK_MULTIPLE_VALUE(x) CHECK_RECORD (x, multiple_value)
#define CONCHECK_MULTIPLE_VALUE(x) CONCHECK_RECORD (x, multiple_value)

#define multiple_value_count(x) ((x)->count)
#define multiple_value_allocated_count(x) ((x)->allocated_count)
#define multiple_value_first_desired(x) ((x)->first_desired)
#define multiple_value_contents(x) ((x)->contents)

#define XMULTIPLE_VALUE_COUNT(x) multiple_value_count (XMULTIPLE_VALUE (x))
#define XMULTIPLE_VALUE_ALLOCATED_COUNT(x) \
  multiple_value_allocated_count (XMULTIPLE_VALUE (x))
#define XMULTIPLE_VALUE_FIRST_DESIRED(x) \
  multiple_value_first_desired (XMULTIPLE_VALUE(x))
#define XMULTIPLE_VALUE_CONTENTS(x) multiple_value_contents (XMULTIPLE_VALUE(x))

Lisp_Object multiple_value_call (int nargs, Lisp_Object *args);
Lisp_Object multiple_value_list_internal (int nargs, Lisp_Object *args);

/* It's slightly ugly to expose this here, but it does cut down the amount
   of work the bytecode interpreter has to do substantially. */
extern int multiple_value_current_limit;

/* Bind the multiple value limits that #'values and #'values-list pay
   attention to. Used by bytecode and interpreted code. */
int bind_multiple_value_limits (int first, int upper);

Lisp_Object multiple_value_aref (Lisp_Object, Elemcount);
void multiple_value_aset (Lisp_Object, Elemcount, Lisp_Object);

Lisp_Object values2 (Lisp_Object first, Lisp_Object second);

DECLARE_INLINE_HEADER (
Lisp_Object 
ignore_multiple_values (Lisp_Object obj)
)
{
  return MULTIPLE_VALUEP (obj) ? multiple_value_aref (obj, 0) : obj;
}

#ifdef ERROR_CHECK_MULTIPLE_VALUES

DECLARE_INLINE_HEADER (
Lisp_Object
ignore_multiple_values_1 (Lisp_Object obj)
)
{
  if (1 == multiple_value_current_limit)
    {
      assert (!MULTIPLE_VALUEP (obj));
      return obj;
    }

  return ignore_multiple_values (obj);
}

#define IGNORE_MULTIPLE_VALUES(X) ignore_multiple_values_1 (X)

#else 
#define IGNORE_MULTIPLE_VALUES(X) (multiple_value_current_limit == 1 ? (X) \
                                   : ignore_multiple_values (X))
#endif

END_C_DECLS

#endif /* INCLUDED_symeval_h_ */
