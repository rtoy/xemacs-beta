/* X-specific functions for internationalizing XEmacs.
   Copyright (C) 1996 Sun Microsystems.
   Copyright (C) 2000, 2001 Ben Wing.

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

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include <X11/Xlocale.h>        /* More portable than <locale.h> ? */

Lisp_Object Qxintl;

int init_x_locale (Lisp_Object locale);

int
init_x_locale (Lisp_Object USED_IF_MULE (locale))
{
#ifdef MULE
  /* dverna - Nov. 98: #### DON'T DO THIS !!! The default XtLanguageProc
     routine calls setlocale(LC_ALL, lang) which fucks up our lower-level
     locale management, and especially the value of LC_NUMERIC. Anyway, since
     at this point, we don't know yet whether we're gonna need an X11 frame,
     we should really do it manually and not use Xlib's dumb default routine */
  /*XtSetLanguageProc (NULL, (XtLanguageProc) NULL, NULL);*/

  if (!XSupportsLocale ())
    {
      warn_when_safe (Qxintl, Qwarning,
		      "System supports locale `%s' but X Windows does not",
		      XSTRING_DATA (locale));
      return 0;
    }

  if (XSetLocaleModifiers ("") == NULL)
    {
      warn_when_safe (Qxintl, Qwarning,
		      "XSetLocaleModifiers(\"\") failed.  Check the value\n"
		      "of the XMODIFIERS environment variable.");
      return 0;
    }
#endif /* MULE */

  return 1;
}

void
syms_of_intl_x (void)
{
  DEFSYMBOL (Qxintl);
}
