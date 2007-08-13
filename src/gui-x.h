/* General GUI code -- X-specific header file.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
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

#ifndef _XEMACS_XLWLIB_H_
#define _XEMACS_XLWLIB_H_

#include "lwlib.h"

widget_value *xmalloc_widget_value (void);

extern LWLIB_ID new_lwlib_id (void);

#ifdef HAVE_POPUPS

extern int popup_up_p;

/* Each frame has one of these, and they are also contained in
   Vpopup_callbacks.
   It doesn't really need to be an lrecord (it's not lisp-accessible)
   but it makes marking slightly more modular.
 */

struct popup_data
{
  struct lcrecord_header header;

  /* lwlib ID of the tree of widgets corresponding to this popup.
     We pass this to lw_map_widget_values() to retrieve all of our
     Lispy call-data values that need to be GCPRO'd. */
  LWLIB_ID id;

  /* For the frame popup data, this is the last buffer for which the
     menubar was displayed.  If the buffer has changed, we may have to
     update things. */
  Lisp_Object last_menubar_buffer;

  /* This flag tells us if the menubar contents are up-to-date with respect
     to the current menubar structure.  If we want to actually pull down a
     menu and this is false, then we need to update things. */
  char menubar_contents_up_to_date;
};

DECLARE_LRECORD (popup_data, struct popup_data);
#define XPOPUP_DATA(x) XRECORD (x, popup_data, struct popup_data)
#define XSETPOPUP_DATA(x, p) XSETRECORD (x, p, popup_data)
#define POPUP_DATAP(x) RECORDP (x, popup_data)
#define GC_POPUP_DATAP(x) GC_RECORDP (x, popup_data)
#define CHECK_POPUP_DATA(x) CHECK_RECORD (x, popup_data)

void gcpro_popup_callbacks (LWLIB_ID id);
void ungcpro_popup_callbacks (LWLIB_ID id);
int popup_handled_p (LWLIB_ID id);
void free_popup_widget_value_tree (widget_value *wv);
void popup_selection_callback (Widget widget, LWLIB_ID ignored_id,
			       XtPointer client_data);
int button_item_to_widget_value (Lisp_Object desc, widget_value *wv,
				 int allow_text_field_p, int no_keys_p);
Boolean separator_string_p (CONST char *s);
char *menu_separator_style (CONST char *s);
Lisp_Object widget_value_unwind (Lisp_Object closure);

#endif /* HAVE_POPUPS */

#endif /* _XEMACS_XLWLIB_H_ */