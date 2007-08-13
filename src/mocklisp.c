/* Mocklisp compatibility functions for XEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1992, 1993, 1995 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.30. */


/* Compatibility for mocklisp */

#include <config.h>

#ifdef MOCKLISP_SUPPORT /* whole file */

#include "lisp.h"
#include "buffer.h"

Lisp_Object Qmocklisp;
Lisp_Object Qmocklisp_arguments;
Lisp_Object Vmocklisp_arguments;

#if 0 /* Now in lisp code ("macrocode...") */
xxDEFUN ("ml-defun", Fml_defun, Sml_defun, 0, UNEVALLED, 0 /*
Define mocklisp functions
*/ )
 (args)
    Lisp_Object args;
{
 Lisp_Object elt;

  while (!NILP (args))
    {
      elt = Fcar (args);
      Ffset (Fcar (elt), Fcons (Qmocklisp, Fcdr (elt)));
      args = Fcdr (args);
    }
  return Qnil;
}
#endif /* 0 */


DEFUN ("ml-if", Fml_if, Sml_if, 0, UNEVALLED, 0 /*
Mocklisp version of `if'.
*/ )
  (args)
     Lisp_Object args;
{
  /* This function can GC */
  Lisp_Object val;
  struct gcpro gcpro1;

  GCPRO1 (args);
  while (!NILP (args))
    {
      val = Feval (Fcar (args));
      args = Fcdr (args);
      if (NILP (args)) break;
      if (XINT (val))
	{
	  val = Feval (Fcar (args));
	  break;
	}
      args = Fcdr (args);
    }
  UNGCPRO;
  return val;
}

#if 0 /* Now converted to regular "while" by hairier conversion code. */
xxDEFUN ("ml-while", Fml_while, Sml_while, 1, UNEVALLED, 0 /*
while  for mocklisp programs
*/ )
  (args)
     Lisp_Object args;
{
  Lisp_Object test, body, tem;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (test, body);

  test = Fcar (args);
  body = Fcdr (args);
  while (tem = Feval (test), XINT (tem))
    {
      QUIT;
      Fprogn (body);
   }

  UNGCPRO;
  return Qnil;
}
#endif /* 0 */


/* This is the main entry point to mocklisp execution.
 When eval sees a mocklisp function being called, it calls here
 with the unevaluated argument list */

Lisp_Object
ml_apply (Lisp_Object function, Lisp_Object args)
{
  /* This function can GC */
  int speccount = specpdl_depth ();
  Lisp_Object val;

  specbind (Qmocklisp_arguments, args);
  val = Fprogn (Fcdr (function));
  return unbind_to (speccount, val);
}

#if 0 /* now in lisp code */

xxDEFUN ("ml-nargs", Fml_nargs, Sml_nargs, 0, 0, 0 /*
Number of arguments to currently executing mocklisp function.
*/ )
  ()
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return make_int (0);
  return Flength (Vmocklisp_arguments);
}


/* now in lisp code */
xxDEFUN ("ml-arg", Fml_arg, Sml_arg, 1, 2, 0 /*
Argument number N to currently executing mocklisp function.
*/ )
  (n, prompt)
     Lisp_Object n, prompt;
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return call1 (Qread_from_minibuffer, prompt);
  CHECK_INT (n);
  XSETINT (n, XINT (n) - 1);	/* Mocklisp likes to be origin-1 */
  return Fcar (Fnthcdr (n, Vmocklisp_arguments));
}

/* now in lisp code */
xxDEFUN ("ml-interactive", Fml_interactive, Sml_interactive, 0, 0, 0 /*
True if currently executing mocklisp function was called interactively.
*/ )
  ()
{
  return (EQ (Vmocklisp_arguments, Qinteractive)) ? Qt : Qnil;
}

#endif /* 0 */


/* ???  Isn't this the same as `provide-prefix-arg' from mlsupport.el? */
DEFUN ("ml-provide-prefix-argument", Fml_provide_prefix_argument, Sml_provide_prefix_argument,
  2, UNEVALLED, 0 /*
Evaluate second argument, using first argument as prefix arg value.
*/ )
  (args)
     Lisp_Object args;
{
  /* This function can GC */
  struct gcpro gcpro1;
  GCPRO1 (args);
  Vcurrent_prefix_arg = Feval (Fcar (args));
  UNGCPRO;
  return Feval (Fcar (Fcdr (args)));
}

DEFUN ("ml-prefix-argument-loop", Fml_prefix_argument_loop,
       Sml_prefix_argument_loop,
       0, UNEVALLED, 0 /*

*/ )
  (args)
     Lisp_Object args;
{
  /* This function can GC */
  Lisp_Object tem;
  int i;
  struct gcpro gcpro1;

  /* Set `arg' in case we call a built-in function that looks at it.  Still are a few. */
  if (NILP (Vcurrent_prefix_arg))
    i = 1;
  else
    {
      tem = Vcurrent_prefix_arg;
      if (CONSP (tem))
	tem = Fcar (tem);
      if (EQ (tem, Qminus))
	i = -1;
      else i = XINT (tem);
    }

  GCPRO1 (args);
  while (i-- > 0)
    Fprogn (args);
  UNGCPRO;
  return Qnil;
}


#if 0
/* now in lisp code */
DEFUN ("ml-substr", Fml_substr, Sml_substr, 3, 3, 0 /*
Return a substring of STRING, starting at index FROM and of length LENGTH.
If either FROM or LENGTH is negative, the length of STRING is added to it.
*/ )
  (string, from, to)
     Lisp_Object string, from, to;
{
  CHECK_STRING (string);
  CHECK_INT (from);
  CHECK_INT (to);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + string_char_length (XSTRING (string)));
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + string_char_length (XSTRING (string)));
  XSETINT (to, XINT (to) + XINT (from));
  return Fsubstring (string, from, to);
}


/* now in lisp code */
DEFUN ("insert-string", Finsert_string, Sinsert_string, 0, MANY, 0 /*
Mocklisp-compatibility insert function.
Like the function `insert' except that any argument that is a number
is converted into a string by expressing it in decimal.
*/ )
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  int argnum;
  Lisp_Object tem;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (INTP (tem))
	tem = Fnumber_to_string (tem);
      if (STRINGP (tem))
	buffer_insert1 (current_buffer, tem);
      else
	{
	  tem = wrong_type_argument (Qstringp, tem);
	  goto retry;
	}
    }
  return Qnil;
}

#endif /* 0 */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_mocklisp (void)
{
  defsymbol (&Qmocklisp, "mocklisp");
  defsymbol (&Qmocklisp_arguments, "mocklisp-arguments");

/*defsubr (&Sml_defun);*/
  defsubr (&Sml_if);
/*defsubr (&Sml_while);*/
/*defsubr (&Sml_nargs);*/
/*defsubr (&Sml_arg);*/
/*defsubr (&Sml_interactive);*/
  defsubr (&Sml_provide_prefix_argument);
  defsubr (&Sml_prefix_argument_loop);
/*defsubr (&Sml_substr);*/
/*defsubr (&Sinsert_string);*/
}

void
vars_of_mocklisp (void)
{
  DEFVAR_LISP ("mocklisp-arguments", &Vmocklisp_arguments /*
While in a mocklisp function, the list of its unevaluated args.
*/ );
  Vmocklisp_arguments = Qt;
}

#endif /* MOCKLISP_SUPPORT */
