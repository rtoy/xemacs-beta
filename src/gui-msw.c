/* mswindows GUI code. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1998 Andy Piper.
   Copyright (C) 2002 Ben Wing.

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

/* This file essentially Mule-ized (except perhaps some Unicode splitting).
   5-2000. */

#include <config.h>
#include "lisp.h"
#include "console-msw-impl.h"
#include "redisplay.h"
#include "gui.h"
#include "glyphs.h"
#include "frame-impl.h"
#include "elhash.h"
#include "events.h"
#include "buffer.h"

/*
 * Return value is Qt if we have dispatched the command,
 * or Qnil if id has not been mapped to a callback.
 * Window procedure may try other targets to route the
 * command if we return nil
 */
Lisp_Object
mswindows_handle_gui_wm_command (struct frame *f, HWND ctrl, LPARAM id)
{
  /* Try to map the command id through the proper hash table */
  Lisp_Object callback, callback_ex, image_instance, frame, event;

  frame = wrap_frame (f);

  image_instance = Fgethash (make_int_verify (id), 
			     FRAME_MSWINDOWS_WIDGET_HASH_TABLE1 (f), Qnil);
  /* It is possible for a widget action to cause it to get out of sync
     with its instantiator. Thus it is necessary to signal this
     possibility. */
  if (IMAGE_INSTANCEP (image_instance))
    XIMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (image_instance) = 1;
  callback = Fgethash (make_int (id), 
		       FRAME_MSWINDOWS_WIDGET_HASH_TABLE2 (f), Qnil);
  callback_ex = Fgethash (make_int (id), 
			  FRAME_MSWINDOWS_WIDGET_HASH_TABLE3 (f), Qnil);

  if (!NILP (callback_ex) && !UNBOUNDP (callback_ex))
    {
      event = Fmake_event (Qnil, Qnil);

      XSET_EVENT_TYPE (event, misc_user_event);
      XSET_EVENT_CHANNEL (event, frame);
      XSET_EVENT_TIMESTAMP (event, GetTickCount());
      XSET_EVENT_MISC_USER_FUNCTION (event, Qeval);
      XSET_EVENT_MISC_USER_OBJECT (event,
			     list4 (Qfuncall, callback_ex, image_instance, event));
    }
  else if (NILP (callback) || UNBOUNDP (callback))
    return Qnil;
  else
    {
      Lisp_Object fn, arg;

      event = Fmake_event (Qnil, Qnil);

      get_gui_callback (callback, &fn, &arg);
      XSET_EVENT_TYPE (event, misc_user_event);
      XSET_EVENT_CHANNEL (event, frame);
      XSET_EVENT_TIMESTAMP (event, GetTickCount());
      XSET_EVENT_MISC_USER_FUNCTION (event, fn);
      XSET_EVENT_MISC_USER_OBJECT (event, arg);
    }

  mswindows_enqueue_dispatch_event (event);
  /* The result of this evaluation could cause other instances to change so 
     enqueue an update callback to check this. */
  enqueue_magic_eval_event (update_widget_instances, frame);
  return Qt;
}

/*
 * Translate X accelerator syntax to win32 accelerator syntax.
 * accel = (Ichar*) to receive the accelerator character
 *         or NULL to suppress accelerators in the menu or dialog item.
 *
 * %% is replaced with %
 * if accel is NULL:
 *   %_ is removed.
 * if accel is non-NULL:
 *   %_ is replaced with &.
 *   The accelerator character is passed back in *accel.
 *   (If there is no accelerator, it will be added on the first character.)
 *
 */

Lisp_Object
mswindows_translate_menu_or_dialog_item (Lisp_Object item, Ichar *accel)
{
  Bytecount len = XSTRING_LENGTH (item);
  Ibyte *it = (Ibyte *) ALLOCA (2 * len + 42), *ptr = it;

  memcpy (ptr, XSTRING_DATA (item), len + 1);
  if (accel)
    *accel = '\0';

  /* Escape '&' as '&&' */
  
  while ((ptr = (Ibyte *) memchr (ptr, '&', len - (ptr - it))) != NULL)
    {
      memmove (ptr + 1, ptr, (len - (ptr - it)) + 1);
      len++;
      ptr += 2;
    }

  /* Replace XEmacs accelerator '%_' with Windows accelerator '&'
     and `%%' with `%'. */
  ptr = it;
  while ((ptr = (Ibyte *) memchr (ptr, '%', len - (ptr - it))) != NULL)
    {
      if (*(ptr + 1) == '_')
	{
	  if (accel)
	    {
	      *ptr = '&';
	      if (!*accel)
		*accel = DOWNCASE (0, itext_ichar (ptr + 2));
	      memmove (ptr + 1, ptr + 2, len - (ptr - it + 2) + 1);
	      len--;
	    }
	  else	/* Skip accelerator */
	    {
	      memmove (ptr, ptr + 2, len - (ptr - it + 2) + 1);
	      len -= 2;
	    }
	}
      else if (*(ptr + 1) == '%')
	{
	  memmove (ptr + 1, ptr + 2, len - (ptr - it + 2) + 1);
	  len--;
	  ptr++;
	}
      else	/* % on its own - shouldn't happen */
	ptr++;
    }

  if (accel && !*accel)
    {
      /* Force a default accelerator */
      ptr = it;
      memmove (ptr + 1, ptr, len + 1);
      *accel = DOWNCASE (0, itext_ichar (ptr + 1));
      *ptr = '&';

      len++;
    }

  return make_string (it, len);
}

void
syms_of_gui_mswindows (void)
{
}
