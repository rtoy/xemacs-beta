/* Generic GUI code. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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
#include "gui.h"

Lisp_Object Q_active, Q_suffix, Q_keys, Q_style, Q_selected;
Lisp_Object Q_filter, Q_config, Q_included;
Lisp_Object Q_accelerator;
Lisp_Object Qtoggle, Qradio;

#ifdef HAVE_POPUPS

/* count of menus/dboxes currently up */
int popup_up_p;

DEFUN ("popup-up-p", Fpopup_up_p, 0, 0, 0, /*
Return t if a popup menu or dialog box is up, nil otherwise.
See `popup-menu' and `popup-dialog-box'.
*/
       ())
{
  return popup_up_p ? Qt : Qnil;
}

int
separator_string_p (CONST char *s)
{
  CONST char *p;
  char first;

  if (!s || s[0] == '\0')
    return 0;
  first = s[0];
  if (first != '-' && first != '=')
    return 0;
  for (p = s; *p == first; p++);

  if (*p == '!' || *p == ':' || *p == '\0')
    return 1;
  return 0;
}

/* 
 * Initialize the gui_item structure by setting all (GC-protected)
 * fields to their default values. The defaults are t for :active and
 * :included values, and nil for others.
 */
void
gui_item_init (struct gui_item *pgui_item)
{
  pgui_item->name = Qnil;
  pgui_item->callback = Qnil;
  pgui_item->suffix = Qnil;
  pgui_item->active = Qt;
  pgui_item->included = Qt;
  pgui_item->config = Qunbound;
  pgui_item->filter = Qnil;
  pgui_item->style = Qnil;
  pgui_item->selected = Qnil;
  pgui_item->keys = Qnil;
}

/*
 * Add a value VAL associated with keyword KEY into PGUI_ITEM
 * structure. If KEY is not a keyword, or is an unknown keyword, then
 * error is signaled.
 */
void 
gui_item_add_keyval_pair (struct gui_item *pgui_item,
			  Lisp_Object key, Lisp_Object val)
{
  if (!KEYWORDP (key))
    error ("Not a keyword %S in gui item %S", key, pgui_item->name);

  if (EQ (key, Q_suffix))
    pgui_item->suffix = val;
  else if (EQ (key, Q_active))
    pgui_item->active = val;
  else if (EQ (key, Q_included))
    pgui_item->included = val;
  else if (EQ (key, Q_config))
    pgui_item->config = val;
  else if (EQ (key, Q_filter))
    pgui_item->filter = val;
  else if (EQ (key, Q_style))
    pgui_item->style = val;
  else if (EQ (key, Q_selected))
    pgui_item->selected = val;
  else if (EQ (key, Q_keys))
    pgui_item->keys = val;
  else
    error ("Unknown keyword %S in gui item %S", key, pgui_item->name);
}

/*
 * ITEM is a lisp vector, describing a menu item or a button. The
 * function extracts the description of the item into the PGUI_ITEM
 * structure.
 */
void
gui_parse_item_keywords (Lisp_Object item, struct gui_item *pgui_item)
{
  int length, plist_p;
  Lisp_Object *contents;

  CHECK_VECTOR (item);
  length = XVECTOR_LENGTH (item);
  contents = XVECTOR_DATA (item);

  if (length < 3)
    signal_simple_error ("GUI item descriptors must be at least 3 elts long", item);

  /* length 3:		[ "name" callback active-p ]
     length 4:		[ "name" callback active-p suffix ]
     		   or	[ "name" callback keyword  value  ]
     length 5+:		[ "name" callback [ keyword value ]+ ]
  */
  plist_p = (length >= 5 || KEYWORDP (contents [2]));

  pgui_item->name = contents [0];
  pgui_item->callback = contents [1];

  if (!plist_p)
    /* the old way */
    {
      pgui_item->active = contents [2];
      if (length == 4)
	pgui_item->suffix = contents [3];
    }
  else
    /* the new way */
    {
      int i;
      if (length & 1)
	signal_simple_error (
		"GUI item descriptor has an odd number of keywords and values",
			     item);

      for (i = 2; i < length;)
	{
	  Lisp_Object key = contents [i++];
	  Lisp_Object val = contents [i++];
	  gui_item_add_keyval_pair (pgui_item, key, val);
	}
    }    
}

/*
 * Decide whether a GUI item is active by evaluating its :active form
 * if any
 */
int
gui_item_active_p (CONST struct gui_item *pgui_item)
{
  /* This function can call lisp */
  
  /* Shortcut to avoid evaluating Qt each time */
  return (EQ (pgui_item->active, Qt)
	  || !NILP (Feval (pgui_item->active)));
}

/*
 * Decide whether a GUI item is included by evaluating its :included
 * form if given, and testing its :config form against supplied CONFLIST
 * configuration variable
 */
int
gui_item_included_p (CONST struct gui_item *pgui_item, Lisp_Object conflist)
{
  /* This function can call lisp */

  /* Evaluate :included first. Shortcut to avoid evaluating Qt each time */
  if (!EQ (pgui_item->included, Qt)
      && NILP (Feval (pgui_item->included)))
    return 0;

  /* Do :config if conflist is given */
  if (!NILP (conflist) && !NILP (pgui_item->config)
      && NILP (Fmemq (pgui_item->config, conflist)))
    return 0;

  return 1;
}

static DOESNT_RETURN
signal_too_long_error (Lisp_Object name)
{
  error ("GUI item %s produces too long displayable string", name);
}

/*
 * Format "left flush" display portion of an item into BUF, guarded by 
 * maximum buffer size BUF_LEN. BUF_LEN does not count for terminating 
 * null character, so actual maximum size of buffer consumed is
 * BUF_LEN + 1 bytes. If buffer is not big enough, then error is
 * signaled.
 * Return value is the offset to the terminating null character into the
 * buffer.
 */
unsigned int
gui_item_display_flush_left  (CONST struct gui_item *pgui_item,
			      char* buf, unsigned int buf_len)
{
  unsigned int consumed;

  /* Copy item name first */
  CHECK_STRING (pgui_item->name);
  if (XSTRING_LENGTH (pgui_item->name) > buf_len)
    signal_too_long_error (pgui_item->name);
  strcpy (buf, XSTRING_DATA (pgui_item->name));
  buf += (consumed = XSTRING_LENGTH (pgui_item->name));
  buf_len -= consumed;

  /* Add space and suffix text, if there is a suffix */
  if (!NILP (pgui_item->suffix))
    {
      if (XSTRING_LENGTH (pgui_item->suffix) + 1 > buf_len)
	signal_too_long_error (pgui_item->name);
      *(buf++) = ' ';
      strcpy (buf, XSTRING_DATA (pgui_item->suffix));
      consumed += XSTRING_LENGTH (pgui_item->suffix) + 1;
    }

  return consumed;
}

/*
 * Format "right flush" display portion of an item into BUF, guarded by 
 * maximum buffer size BUF_LEN. BUF_LEN does not count for terminating 
 * null character, so actual maximum size of buffer consumed is
 * BUF_LEN + 1 bytes. If buffer is not big enough, then error is
 * signaled.
 * Return value is the offset to the terminating null character into the
 * buffer.
 */
unsigned int
gui_item_display_flush_right (CONST struct gui_item *pgui_item,
			      char* buf, unsigned int buf_len)
{
  *buf = 0;

  /* Have keys? */
  if (!menubar_show_keybindings)
    return 0;

  /* Try :keys first */
  if (!NILP (pgui_item->keys))
    {
      CHECK_STRING (pgui_item->keys);
      if (XSTRING_LENGTH (pgui_item->keys) > buf_len)
	signal_too_long_error (pgui_item->name);
      strcpy (buf, XSTRING_DATA (pgui_item->keys));
      return XSTRING_LENGTH (pgui_item->keys);
    }

  /* See if we can derive keys out of callback symbol */
  if (SYMBOLP (pgui_item->callback))
    {
      char buf2 [1024];
      unsigned int len;

      where_is_to_char (pgui_item->callback, buf2);
      len = strlen (buf2);
      if (len > buf_len)
	signal_too_long_error (pgui_item->name);
      strcpy (buf, buf2);
      return len;
    }

  /* No keys - no right flush display */
  return 0;
}

#endif /* HAVE_POPUPS */

void
syms_of_gui (void)
{
  defkeyword (&Q_active,   ":active");
  defkeyword (&Q_suffix,   ":suffix");
  defkeyword (&Q_keys,     ":keys");
  defkeyword (&Q_style,    ":style");
  defkeyword (&Q_selected, ":selected");
  defkeyword (&Q_filter,   ":filter");
  defkeyword (&Q_config,   ":config");
  defkeyword (&Q_included, ":included");
  defkeyword (&Q_accelerator, ":accelerator");

  defsymbol (&Qtoggle, "toggle");
  defsymbol (&Qradio, "radio");

#ifdef HAVE_POPUPS
  DEFSUBR (Fpopup_up_p);
#endif
}

void
vars_of_gui (void)
{
}
