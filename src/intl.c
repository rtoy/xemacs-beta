/* Various functions for internationalizing XEmacs.
   Copyright (C) 1993, 1994, 1995 Board of Trustees, University of Illinois.
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

#include <config.h>
#include "lisp.h"

#if defined (HAVE_X_WINDOWS) && defined (HAVE_X11_XLOCALE_H)
#include <X11/Xlocale.h>
#else
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#endif

#ifdef HAVE_X_WINDOWS
int init_x_locale (Lisp_Object locale);
#endif

DEFUN ("current-locale", Fcurrent_locale, 0, 0, 0, /*
Return the current locale.
This is of the form LANG_COUNTRY.ENCODING, or LANG_COUNTRY, or LANG,
or .ENCODING.  Unfortunately, the meanings of these three values are
system-dependent, and there is no universal agreement.
*/
       ())
{
  Extbyte *loc;

  loc = setlocale (LC_CTYPE, NULL);
  if (!loc)
    return Qnil;
  return build_ext_string (loc, Qctext);
}

DEFUN ("set-current-locale", Fset_current_locale, 1, 1, 0, /*
Set the user's current locale.
Takes a string, the value passed to setlocale().
This is of the form LANG_COUNTRY.ENCODING, or LANG_COUNTRY, or LANG,
or .ENCODING.  Unfortunately, the meanings of these three values are
system-dependent, and there is no universal agreement.  This function
is meant to be called only from `set-language-environment', which
keeps tables to figure out the values to use for particular systems.

If the empty string is passed in, the locale is initialized from
environment variables.

Returns nil if the call failed (typically, an invalid locale was given).
Otherwise, returns the locale, or possibly a more-specified version.
*/
       (locale))
{
  Extbyte *loc;
  Lisp_Object str;

  CHECK_STRING (locale);
  /* RedHat 6.2 contains a locale called "Francais" with the C-cedilla
     encoded in ISO2022! */
  LISP_STRING_TO_EXTERNAL (locale, loc, Qctext);
  loc = setlocale (LC_ALL, loc);
  if (!loc)
    return Qnil;
  loc = xstrdup (loc);
  setlocale (LC_NUMERIC, "C");
#ifdef HAVE_X_WINDOWS
  if (!init_x_locale (locale))
    {
      /* Locale not supported under X.  Put it back. */
      setlocale (LC_ALL, loc);
      setlocale (LC_NUMERIC, "C");
      free (loc);
      return Qnil;
    }
#endif

  str = build_ext_string (loc, Qctext);
  xfree (loc, Extbyte *);
  return str;
}

#if 0

/* #### some old code that I really want to nuke, but I'm not completely
   sure what it did, so I'll leave it until we get around to implementing
   message-translation and decide whether the functionality that this
   is trying to support makes any sense. --ben */

Lisp_Object Qdefer_gettext;

DEFUN ("ignore-defer-gettext", Fignore_defer_gettext, 1, 1, 0, /*
If OBJECT is of the form (defer-gettext "string"), return the string.
The purpose of the defer-gettext symbol is to identify strings which
are translated when they are referenced instead of when they are defined.
*/
       (object))
{
  if (CONSP (object)
      && SYMBOLP (Fcar (object))
      && EQ (Fcar (object), Qdefer_gettext))
    return Fcar (Fcdr (object));
  else
    return object;
}

#endif /* 0 */

DEFUN ("gettext", Fgettext, 1, 1, 0, /*
Look up STRING in the default message domain and return its translation.
This function does nothing if I18N3 was not enabled when Emacs was compiled.
*/
       (string))
{
#ifdef I18N3
  /* #### What should happen here is:

     1) If the string has no `string-translatable' property or its value
        is nil, no translation takes place.  The `string-translatable' property
	only gets added when a constant string is read in from a .el or .elc
	file, to avoid excessive translation.  (The user can also explicitly
	add this property to a string.)
     2) If the string's `string-translatable' property is a string,
	that string should be returned.  `format' add this property.
	This allows translation to take place at the proper time but
	avoids excessive translation if the string is not destined for
	a translating stream.  (See print_internal().)
     3) If gettext() returns the same string, then Fgettext() should return
        the same object, minus the 'string-translatable' property. */

#endif
  return string;
}

#ifdef I18N3

/* #### add the function `force-gettext', perhaps in Lisp.  This
   ignores the `string-translatable' property and simply calls gettext()
   on the string.  Add the functions `set-string-translatable' and
   `set-stream-translating'. */

#endif



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
init_intl (void)
{
  /* This function cannot GC, because it explicitly prevents it. */
  if (initialized)
    {
      int count = begin_gc_forbidden ();
      Lisp_Object args[2];

      specbind (Qinhibit_quit, Qt);
      args[0] = Qreally_early_error_handler;
      args[1] = intern ("init-locale-at-early-startup");
      Fcall_with_condition_handler (2, args);

      /* Should be calling this here, but problems with
         `data-directory' and locating the files.  See comment in
         mule-cmds.el:`init-mule-at-startup'.

      args[1] = intern ("init-unicode-at-early-startup");
      Fcall_with_condition_handler (2, args);
       */
      unbind_to (count);
    }
}

void
syms_of_intl (void)
{
  DEFSUBR (Fgettext);
  DEFSUBR (Fset_current_locale);
  DEFSUBR (Fcurrent_locale);
}

void
vars_of_intl (void)
{
#ifdef I18N3
  Fprovide (intern ("i18n3"));
#endif

#ifdef MULE
  Fprovide (intern ("mule"));
#endif /* MULE */
}
