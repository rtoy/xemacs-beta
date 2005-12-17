/* Console functions for X windows.
   Copyright (C) 1996, 2002 Ben Wing.

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

/* This file Mule-ized by Ben Wing, 7-10-00. */

/* Authorship:

   Ben Wing: January 1996, for 19.14.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "elhash.h"
#include "process.h" /* canonicalize_host_name */
#include "redisplay.h" /* for display_arg */

#include "device-impl.h"
#include "console-x-impl.h"

DEFINE_CONSOLE_TYPE (x);

extern void x_has_keysym (KeySym, Lisp_Object, int);

static int
x_initially_selected_for_input (struct console *UNUSED (con))
{
  return 1;
}

/* Parse a DISPLAY specification like "host:10.0" or ":0" */
static void
split_up_display_spec (Lisp_Object display, int *hostname_length,
		       int *display_length, int *screen_length)
{
  Ibyte *beg = XSTRING_DATA (display);
  Ibyte *end = beg + XSTRING_LENGTH (display);
  Ibyte *p = end;

  while (p > beg)
    {
      DEC_IBYTEPTR (p);
      if (itext_ichar (p) == ':')
	{
	  *hostname_length = p - beg;

	  while (p < end - 1)
	    {
	      INC_IBYTEPTR (p);
	      if (itext_ichar (p) == '.')
		{
		  *display_length = p - beg - *hostname_length;
		  *screen_length = end - p;
		  return;
		}
	    }
	  /* No '.' found. */
	  *display_length = XSTRING_LENGTH (display) - *hostname_length;
	  *screen_length = 0;
	  return;
	}
    }

  /* No ':' found. */
  *hostname_length = XSTRING_LENGTH (display);
  *display_length = 0;
  *screen_length = 0;
}

/* Remember, in all of the following functions, we have to verify
   the integrity of our input, because the generic functions don't. */

static Lisp_Object
x_device_to_console_connection (Lisp_Object connection, Error_Behavior errb)
{
  /* Strip the trailing .# off of the connection, if it's there. */

  if (NILP (connection))
    return Qnil;
  else
    {
      int hostname_length, display_length, screen_length;

      if (!ERRB_EQ (errb, ERROR_ME))
	{
	  if (!STRINGP (connection))
	    return Qunbound;
	}
      else
	CHECK_STRING (connection);

      split_up_display_spec (connection, &hostname_length, &display_length,
			     &screen_length);
      connection = make_string (XSTRING_DATA (connection),
				hostname_length + display_length);
    }

  return connection;
}

static Lisp_Object
get_display_arg_connection (void)
{
  const Extbyte *disp_name;

  /* If the user didn't explicitly specify a display to use when
     they called make-x-device, then we first check to see if a
     display was specified on the command line with -display.  If
     so, we set disp_name to it.  Otherwise we use XDisplayName to
     see what DISPLAY is set to.  XtOpenDisplay knows how to do
     both of these things, but we need to know the name to use. */
  if (display_arg)
    {
      int elt;
      int argc;
      Extbyte **argv;
      Lisp_Object conn;

      make_argc_argv (Vx_initial_argv_list, &argc, &argv);

      disp_name = NULL;
      for (elt = 0; elt < argc; elt++)
	{
	  if (!strcmp (argv[elt], "-d") || !strcmp (argv[elt], "-display"))
	    {
	      if (elt + 1 == argc)
		{
		  suppress_early_error_handler_backtrace = 1;
		  invalid_argument ("-display specified with no arg", Qunbound);
		}
	      else
		{
		  disp_name = argv[elt + 1];
		  break;
		}
	    }
	}

      /* assert: display_arg is only set if we found the display
	 arg earlier so we can't fail to find it now. */
      assert (disp_name != NULL);
      conn = build_ext_string (disp_name, Qcommand_argument_encoding);
      free_argc_argv (argv);
      return conn;
    }
  else
    return build_ext_string (XDisplayName (0), Qx_display_name_encoding);
}

/* "semi-canonicalize" means convert to a nicer form for printing, but
   don't completely canonicalize (into some likely ugly form) */

static Lisp_Object
x_semi_canonicalize_console_connection (Lisp_Object connection,
					Error_Behavior errb)
{
  struct gcpro gcpro1;

  GCPRO1 (connection);

  if (NILP (connection))
    connection = get_display_arg_connection ();
  else
    {
      if (!ERRB_EQ (errb, ERROR_ME))
	{
	  if (!STRINGP (connection))
	    RETURN_UNGCPRO (Qunbound);
	}
      else
	CHECK_STRING (connection);
    }


  /* Be lenient, allow people to specify a device connection instead of
     a console connection -- e.g. "foo:0.0" instead of "foo:0".  This
     only happens in `find-console' and `get-console'. */
  connection = x_device_to_console_connection (connection, errb);

  /* Check for a couple of standard special cases */
  if (string_ichar (connection, 0) == ':')
    connection = concat2 (build_string ("localhost"), connection);
  else
    {
      /* connection =~ s/^unix:/localhost:/; */
      const Ibyte *p   = XSTRING_DATA (connection);
      const Ibyte *end = XSTRING_DATA (connection) + XSTRING_LENGTH (connection);
      int i;

      for (i = 0; i < (int) sizeof ("unix:") - 1; i++)
	{
	  if (p == end || itext_ichar (p) != "unix:"[i])
	    goto ok;
	  INC_IBYTEPTR (p);
	}

      connection = concat2 (build_string ("localhost:"),
			    make_string (p, end - p));
    }
 ok:

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
x_canonicalize_console_connection (Lisp_Object connection, Error_Behavior errb)
{
  Lisp_Object hostname = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (connection, hostname);

  connection = x_semi_canonicalize_console_connection (connection, errb);
  if (UNBOUNDP (connection))
    RETURN_UNGCPRO (Qunbound);

  {
    int hostname_length, display_length, screen_length;

    split_up_display_spec (connection, &hostname_length, &display_length,
			   &screen_length);
    hostname = Fsubstring (connection, Qzero, make_int (hostname_length));
    hostname = canonicalize_host_name (hostname);
    connection = concat2 (hostname,
			  make_string (XSTRING_DATA (connection)
				       + hostname_length, display_length));
  }

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
x_semi_canonicalize_device_connection (Lisp_Object connection,
				       Error_Behavior errb)
{
  int hostname_length, display_length, screen_length;
  struct gcpro gcpro1;

  GCPRO1 (connection);
  if (NILP (connection))
    connection = get_display_arg_connection ();
  else
    {
      if (!ERRB_EQ (errb, ERROR_ME))
	{
	  if (!STRINGP (connection))
	    RETURN_UNGCPRO (Qunbound);
	}
      else
	CHECK_STRING (connection);
    }

  split_up_display_spec (connection, &hostname_length, &display_length,
			 &screen_length);

  if (!screen_length)
    connection = concat2 (connection, build_string (".0"));
  RETURN_UNGCPRO (connection);
}

static Lisp_Object
x_canonicalize_device_connection (Lisp_Object connection, Error_Behavior errb)
{
  int hostname_length, display_length, screen_length;
  Lisp_Object screen_str = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (screen_str, connection);
  connection = x_semi_canonicalize_device_connection (connection, errb);
  if (UNBOUNDP (connection))
    RETURN_UNGCPRO (Qunbound);

  split_up_display_spec (connection, &hostname_length, &display_length,
			 &screen_length);

  screen_str = make_string (XSTRING_DATA (connection)
			    + hostname_length + display_length, screen_length);
  connection = x_canonicalize_console_connection (connection, errb);

  RETURN_UNGCPRO (concat2 (connection, screen_str));
}

/* Given a key, if it maps to a character and we weren't previously aware
   that it could be generated on console CON, and if it's unbound in the
   global map, bind it to self-insert-command. Return Qt if the binding was
   done; Qnil if not. */

static Lisp_Object
x_perhaps_init_unseen_key_defaults (struct console *con, Lisp_Object key)
{
  KeySym xkeysym;
  const Extbyte *keysym_ext;
  Lisp_Object key_name, previous_binding = Qnil;
  extern Lisp_Object Qcharacter_of_keysym, Vcurrent_global_map;

  /* Getting the device exactly right is not horrendously important; as long
     as it's an X11 device it should be okay, because the global keymap (and
     whether the key is bound) _is_ global, and any previously seen keysym
     will already be bound, or not, in it. However, there is a corner case
     where a symbol has been typed, and then explicitly unbound; if the next
     event using that symbol comes in on some other frame, it'll get bound
     again. This is not realistically an issue. */
  struct device *d = XDEVICE(con->selected_device);

  if (SYMBOLP (key))
    {
      key_name = symbol_name(XSYMBOL(key));
    }
  else
    {
      Ibyte buf[MAX_ICHAR_LEN + 1];
      CHECK_CHAR(key);

      buf[set_itext_ichar(buf, XCHAR(key))] = '\0';
      key_name = build_intstring (buf);

      /* We need to do the lookup and compare later, because we can't check
	 the Qcharacter_of_keysym property belonging to an actual character. */
      previous_binding = Flookup_key (Vcurrent_global_map, key, Qnil);
    }

  if (!NILP(Fgethash(key, DEVICE_X_KEYSYM_MAP_HASH_TABLE (d), Qnil)))
    {
      return Qnil;
    }

  LISP_STRING_TO_EXTERNAL (key_name, keysym_ext, Qctext);
  xkeysym = XStringToKeysym(keysym_ext);
  if (NoSymbol == xkeysym) 
    {
      /* Keysym is NoSymbol; this may mean the key event passed to us came
	 from an input method, which stored the actual character intended to
	 be inserted in the key name, and didn't trouble itself to set the
	 keycode to anything useful. Thus, if the key name is a single
	 character, and the keysym is NoSymbol, give it a default binding,
	 if that is possible. */
      Lisp_Object keychar;

      if (1 != string_char_length(key_name))
	{
	  /* Don't let them pass us more than one character. */
	  return Qnil;
	}
      keychar = make_char(itext_ichar(XSTRING_DATA(key_name)));
      if (NILP (Flookup_key (Vcurrent_global_map, keychar, Qnil))) 
        {
	  Fdefine_key (Vcurrent_global_map, keychar, Qself_insert_command); 
	  Fputhash (keychar, Qt, DEVICE_X_KEYSYM_MAP_HASH_TABLE (d));
	  return Qt; 
        }
      return Qnil;
    }

  x_has_keysym(xkeysym, DEVICE_X_KEYSYM_MAP_HASH_TABLE (d), 0);

  if (SYMBOLP(key))
    {
      return NILP(Fget (key, Qcharacter_of_keysym, Qnil)) ? Qnil : Qt;
    }
  else
    {
      return EQ(previous_binding, Flookup_key(Vcurrent_global_map, key, Qnil))
	? Qnil : Qt;
    }
}

void
console_type_create_x (void)
{
  INITIALIZE_CONSOLE_TYPE (x, "x", "console-x-p");

  CONSOLE_HAS_METHOD (x, semi_canonicalize_console_connection);
  CONSOLE_HAS_METHOD (x, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (x, semi_canonicalize_device_connection);
  CONSOLE_HAS_METHOD (x, canonicalize_device_connection);
  CONSOLE_HAS_METHOD (x, device_to_console_connection);
  CONSOLE_HAS_METHOD (x, initially_selected_for_input);
  CONSOLE_HAS_METHOD (x, perhaps_init_unseen_key_defaults);
}


void
reinit_console_type_create_x (void)
{
  REINITIALIZE_CONSOLE_TYPE (x);
}
