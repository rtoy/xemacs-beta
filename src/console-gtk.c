/* Console functions for X windows.
   Copyright (C) 1996 Ben Wing.

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

/* Authorship:

   Ben Wing: January 1996, for 19.14.
   William Perry: April 2000, for 21.1 (Gtk version)
 */

#include <config.h>
#include "lisp.h"

#include "process.h" /* canonicalize_host_name */
#include "redisplay.h" /* for display_arg */

#include "charset.h"
#include "elhash.h"

#include "console-gtk-impl.h"

DEFINE_CONSOLE_TYPE (gtk);

Lisp_Object Vgtk_seen_characters;

static int
gtk_initially_selected_for_input (struct console *UNUSED (con))
{
  return 1;
}

/* Remember, in all of the following functions, we have to verify
   the integrity of our input, because the generic functions don't. */

static Lisp_Object
gtk_device_to_console_connection (Lisp_Object connection,
				  Error_Behavior UNUSED (errb))
{
  /* Strip the trailing .# off of the connection, if it's there. */

  if (NILP (connection))
    return Qnil;
  else
    {
	connection = build_string ("gtk");
    }
  return connection;
}

static Lisp_Object
gtk_semi_canonicalize_console_connection (Lisp_Object connection,
					  Error_Behavior UNUSED (errb))
{
  struct gcpro gcpro1;

  GCPRO1 (connection);

  connection = build_string ("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_canonicalize_console_connection (Lisp_Object connection,
				     Error_Behavior UNUSED (errb))
{
  Lisp_Object hostname = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (connection, hostname);

  connection = build_string ("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_semi_canonicalize_device_connection (Lisp_Object connection,
				         Error_Behavior UNUSED (errb))
{
  struct gcpro gcpro1;

  GCPRO1 (connection);

  connection = build_string("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_canonicalize_device_connection (Lisp_Object connection,
				    Error_Behavior UNUSED (errb))
{
  struct gcpro gcpro1;

  GCPRO1 (connection);
  connection = build_string("gtk");

  RETURN_UNGCPRO (connection);
}

extern Lisp_Object gtk_keysym_to_character(guint keysym);

static Lisp_Object
gtk_perhaps_init_unseen_key_defaults (struct console *UNUSED(con),
				      Lisp_Object key)
{
  Lisp_Object char_to_associate = Qnil;
  extern Lisp_Object Vcurrent_global_map, Qcharacter_of_keysym;

  if (SYMBOLP(key))
    {
      gchar *symbol_name;
      DECLARE_EISTRING(ei_symname);
      
      eicpy_rawz(ei_symname, XSTRING_DATA(symbol_name(XSYMBOL(key))));

      /* No information on the coding system of the string key names in GDK,
	 to my knowledge. Defaulting to binary, */
      eito_external(ei_symname, Qbinary);
      symbol_name = eiextdata(ei_symname);

/* GTK 2.0 has an API we can use, and makes this available in gdkkeys.h

   This has yet to be compiled, because XEmacs' GTK support hasn't yet moved
   to 2.0. So if you're porting XEmacs to GTK 2.0, bear that in mind. */
      char_to_associate 
#ifdef __GDK_KEYS_H__ 
	= Funicode_to_char
	      (make_int(gdk_keyval_to_unicode
			(gdk_keyval_from_name(symbol_name))), Qnil);
#else /* GTK 1.whatever doesn't. Use the X11 map. */
        = gtk_keysym_to_character(gdk_keyval_from_name(symbol_name));
#endif
    }
  else
    {
      CHECK_CHAR(key);
      char_to_associate = key;
    }

  if (!(HASH_TABLEP(Vgtk_seen_characters)))
    {
      Vgtk_seen_characters = make_lisp_hash_table (128, HASH_TABLE_NON_WEAK,
						   HASH_TABLE_EQUAL);
    }

  /* Might give the user an opaque error if make_lisp_hash_table fails,
     but it shouldn't crash. */
  CHECK_HASH_TABLE(Vgtk_seen_characters);

  if (EQ(char_to_associate, Qnil) /* If there's no char to bind,  */
      || (XCHAR(char_to_associate) < 0x80) /* or it's ASCII */
      || !NILP(Fgethash(key, Vgtk_seen_characters, Qnil))) /* Or we've seen
							      it already, */
    {
      /* then don't bind the key. */
      return Qnil;
    }

  if (NILP (Flookup_key (Vcurrent_global_map, key, Qnil))) 
    { 
      Fputhash(key, Qt, Vgtk_seen_characters);
      Fdefine_key (Vcurrent_global_map, key, Qself_insert_command); 
      if (SYMBOLP(key))
	{
	  Fput (key, Qcharacter_of_keysym, char_to_associate);
	}
      return Qt; 
    }

  return Qnil;
}

void
console_type_create_gtk (void)
{
  INITIALIZE_CONSOLE_TYPE (gtk, "gtk", "console-gtk-p");

  CONSOLE_HAS_METHOD (gtk, semi_canonicalize_console_connection);
  CONSOLE_HAS_METHOD (gtk, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (gtk, semi_canonicalize_device_connection);
  CONSOLE_HAS_METHOD (gtk, canonicalize_device_connection);
  CONSOLE_HAS_METHOD (gtk, device_to_console_connection);
  CONSOLE_HAS_METHOD (gtk, initially_selected_for_input);
  CONSOLE_HAS_METHOD (gtk, perhaps_init_unseen_key_defaults);
  /* CONSOLE_HAS_METHOD (gtk, delete_console); */
}

void
reinit_console_type_create_gtk (void)
{
  REINITIALIZE_CONSOLE_TYPE (gtk);
}

void
vars_of_console_gtk (void)
{
  staticpro (&Vgtk_seen_characters);
  Vgtk_seen_characters = Qnil;
}
