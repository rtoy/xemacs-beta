/* Implements elisp-programmable dialog boxes -- MS Windows interface.
   Copyright (C) 1998 Kirill M. Katsnelson <kkm@kis.ru>

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

/* Author:
   Initially written by kkm, May 1998
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-msw.h"
#include "frame.h"
#include "gui.h"
#include "opaque.h"

/* List containing all dialog data structures of currently popped up
   dialogs. Each item is a cons of frame object and a vector of
   callbacks for buttons in the dialog, in order */
static Lisp_Object Vdialog_data_list;

/* DLUs per character metrics */
#define X_DLU_PER_CHAR	     4
#define Y_DLU_PER_CHAR	     8

/*
  Button metrics
  --------------
  All buttons have height of 15 DLU. The minimum width for a button is 32 DLU, 
  but it can be expanded to accommodate its text, so the width is calculated as
  8 DLU per button plus 4 DLU per character.
  max (32, 6 * text_length). The factor of six is rather empirical, but it
  works better than 8 which comes from the definition of a DLU. Buttons are
  spaced with 6 DLU gap. Minimum distance from the button to the left or right 
  dialog edges is 6 DLU, and the distance between the dialog bottom edge and
  buttons is 7 DLU.
*/

#define X_MIN_BUTTON	    32
#define X_BUTTON_MARGIN	     8
#define Y_BUTTON	    15
#define X_BUTTON_SPACING     6
#define X_BUTTON_FROM_EDGE   6
#define Y_BUTTON_FROM_EDGE   7

/* 
   Text field metrics
   ------------------
   Text distance from left and right edges is the same as for buttons, and the
   top margin is 11 DLU. The static control has height of 2 DLU per control
   plus 8 DLU per each line of text. Distance between the bottom edge of the
   control and the button row is 15 DLU. Minimum width of the static control
   is 100 DLU, thus giving minimum dialog weight of 112 DLU. Maximum width is
   300 DLU, and, if the text is wider than that, the text is wrapped on the
   next line. Each character in the text is considered 4 DLU wide.
*/

#define X_MIN_TEXT	   100
#define X_AVE_TEXT	   200
#define X_MAX_TEXT	   300
#define X_TEXT_FROM_EDGE      X_BUTTON_FROM_EDGE
#define Y_TEXT_FROM_EDGE    11
#define Y_TEXT_MARGIN	     2
#define Y_TEXT_FROM_BUTTON  15

#define X_MIN_TEXT_CHAR	   (X_MIN_TEXT / X_DLU_PER_CHAR)
#define X_AVE_TEXT_CHAR	   (X_AVE_TEXT / X_DLU_PER_CHAR)
#define X_MAX_TEXT_CHAR	   (X_MAX_TEXT / X_DLU_PER_CHAR)

/*
  Layout algorithm
  ----------------
  First we calculate the minimum width of the button row, excluding "from
  edge" distances. Note that the static control text can be narrower than
  X_AVE_TEXT only if both text and button row are narrower than that (so,
  even if text *can* be wrapped into 2 rows narrower than ave width, it is not 
  done). Let WBR denote the width of the button row.

  Next, the width of the static field is determined.
  First, if all lines of text fit into max (WBR, X_MAX_TEXT), the width of the
  control is the same as the width of the longest line. 
  Second, if all lines of text are narrower than X_MIN_TEXT, then width of
  the control is set to X_MIN_TEXT.
  Otherwise, width is set to max(WBR, X_AVE_TEXT). In this case, line wrapping will
  happen.

  If width of the text control is larger than that of the button row, then the
  latter is centered across the dialog, by giving it extra edge
  margins. Otherwise, minimal margins are given to the button row.
*/

#define ID_ITEM_BIAS 32

/* Dialog procedure */
static BOOL CALLBACK 
dialog_proc (HWND hwnd, UINT msg, WPARAM w_param, LPARAM l_param)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      SetWindowLong (hwnd, DWL_USER, l_param);
      break;
      
    case WM_DESTROY:
      {
	Lisp_Object data;
	VOID_TO_LISP (data, GetWindowLong (hwnd, DWL_USER));
	Vdialog_data_list = delq_no_quit (data, Vdialog_data_list);
      }
      break;

    case WM_COMMAND:
      {
	Lisp_Object fn, arg, data;
	VOID_TO_LISP (data, GetWindowLong (hwnd, DWL_USER));
	
	assert (w_param >= ID_ITEM_BIAS 
		&& w_param < XVECTOR_LENGTH (XCDR (data)) + ID_ITEM_BIAS);
	
	get_gui_callback (XVECTOR_DATA (XCDR (data)) [w_param - ID_ITEM_BIAS],
			  &fn, &arg);
	mswindows_enqueue_misc_user_event (XCAR (data), fn, arg);

	DestroyWindow (hwnd);
      }
      break;

    default:
      return FALSE;
    }
  return TRUE;
}

/* Helper function which converts the supplied string STRING into Unicode and
   pushes it at the end of DYNARR */
static void
push_lisp_string_as_unicode (unsigned_char_dynarr* dynarr, Lisp_Object string)
{
  Extbyte *mbcs_string;
  Charcount length = XSTRING_CHAR_LENGTH (string);
  LPWSTR uni_string;

  TO_EXTERNAL_FORMAT (LISP_STRING, string,
		      C_STRING_ALLOCA, mbcs_string,
		      Qnative);
  uni_string = alloca_array (WCHAR, length + 1);
  length = MultiByteToWideChar (CP_ACP, 0, mbcs_string, -1,
				uni_string, sizeof(WCHAR) * (length + 1));
  Dynarr_add_many (dynarr, uni_string, sizeof(WCHAR) * length);
}

/* Helper function which converts the supplied string STRING into Unicode and
   pushes it at the end of DYNARR */
static void
push_bufbyte_string_as_unicode (unsigned_char_dynarr* dynarr, Bufbyte *string,
				Bytecount len)
{
  Extbyte *mbcs_string;
  Charcount length = bytecount_to_charcount (string, len);
  LPWSTR uni_string;

  TO_EXTERNAL_FORMAT (C_STRING, string,
		      C_STRING_ALLOCA, mbcs_string,
		      Qnative);
  uni_string = alloca_array (WCHAR, length + 1);
  length = MultiByteToWideChar (CP_ACP, 0, mbcs_string, -1,
				uni_string, sizeof(WCHAR) * (length + 1));
  Dynarr_add_many (dynarr, uni_string, sizeof(WCHAR) * length);
}

/* Given button TEXT, return button width in DLU */
static unsigned int
button_width (Lisp_Object text)
{
  unsigned int width = X_DLU_PER_CHAR * XSTRING_CHAR_LENGTH (text);
  return max (X_MIN_BUTTON, width);
}

/* Unwind protection routine frees a dynarr opaqued into arg */
static Lisp_Object
free_dynarr_opaque_ptr (Lisp_Object arg)
{
  Dynarr_free (get_opaque_ptr (arg));
  return arg;
}


#define ALIGN_TEMPLATE					\
{							\
  unsigned int slippage = Dynarr_length (template) & 3;	\
  if (slippage)						\
    Dynarr_add_many (template, &zeroes, slippage);	\
}

static void
mswindows_popup_dialog_box (struct frame* f, Lisp_Object desc)
{
  Lisp_Object_dynarr *dialog_items = Dynarr_new (Lisp_Object);
  unsigned_char_dynarr *template = Dynarr_new (unsigned_char);
  unsigned int button_row_width = 0;
  unsigned int text_width, text_height;

  int unbind_count = specpdl_depth ();
  record_unwind_protect (free_dynarr_opaque_ptr,
			 make_opaque_ptr (dialog_items));
  record_unwind_protect (free_dynarr_opaque_ptr,
			 make_opaque_ptr (template));

  /* A big NO NEED to GCPRO gui_items stored in the array: they are just
     pointers into DESC list, which is GC-protected by the caller */

  /* Parse each item in the dialog into gui_item structs, and stuff a dynarr
     of these. Calculate button row width in this loop too */
  {
    Lisp_Object item_cons;

    EXTERNAL_LIST_LOOP (item_cons, XCDR (desc))
      {
	if (!NILP (XCAR (item_cons)))
	  {
	    Lisp_Object gitem = gui_parse_item_keywords (XCAR (item_cons));
	    Dynarr_add (dialog_items, gitem);
	    button_row_width += button_width (XGUI_ITEM (gitem)->name) 
	      + X_BUTTON_MARGIN;
	  }
      }
    if (Dynarr_length (dialog_items) == 0)
      signal_simple_error ("Dialog descriptor provides no active items", desc);
    button_row_width -= X_BUTTON_MARGIN;
  }

  /* Determine the final width layout */
  {
    Bufbyte *p = XSTRING_DATA (XCAR (desc));
    Charcount string_max = 0, this_length = 0;
    while (1)
      {
	Emchar ch = charptr_emchar (p);
	INC_CHARPTR (p);
	
	if (ch == (Emchar)'\n' || ch == (Emchar)'\0')
	  {
	    string_max = max (this_length, string_max);
	    this_length = 0;
	  }
	else
	  ++this_length;

	if (ch == (Emchar)'\0')
	  break;
      }

    if (string_max * X_DLU_PER_CHAR > max (X_MAX_TEXT, button_row_width))
      text_width = X_AVE_TEXT;
    else if (string_max * X_DLU_PER_CHAR < X_MIN_TEXT)
      text_width = X_MIN_TEXT;
    else
      text_width = string_max * X_DLU_PER_CHAR;
    text_width = max (text_width, button_row_width);
  }
  
  /* Now calculate the height for the text control */
  {
    Bufbyte *p = XSTRING_DATA (XCAR (desc));
    Charcount break_at = text_width / X_DLU_PER_CHAR;
    Charcount char_pos = 0;
    int num_lines = 1;
    Emchar ch;
    
    while ((ch = charptr_emchar (p)) != (Emchar)'\0')
      {
	INC_CHARPTR (p);
	char_pos += ch != (Emchar)'\n';
	if (ch == (Emchar)'\n' || char_pos == break_at)
	  {
	    ++num_lines;
	    char_pos = 0;
	  }
      }
    text_height = Y_TEXT_MARGIN + Y_DLU_PER_CHAR * num_lines;
  }

  /* Ok, now we are ready to stuff the dialog template and lay out controls */
  {
    DLGTEMPLATE dlg_tem;
    DLGITEMTEMPLATE item_tem;
    int i;
    const unsigned int zeroes = 0;
    const unsigned int ones = 0xFFFFFFFF;
    const WORD static_class_id = 0x0082;
    const WORD button_class_id = 0x0080;

    /* Create and stuff in DLGTEMPLATE header */
    dlg_tem.style = (DS_CENTER | DS_MODALFRAME | DS_SETFONT
		     | WS_CAPTION | WS_POPUP | WS_VISIBLE);
    dlg_tem.dwExtendedStyle = 0;
    dlg_tem.cdit = Dynarr_length (dialog_items) + 1;
    dlg_tem.x = 0;
    dlg_tem.y = 0;
    dlg_tem.cx = text_width + 2 * X_TEXT_FROM_EDGE;
    dlg_tem.cy = (Y_TEXT_FROM_EDGE + text_height + Y_TEXT_FROM_BUTTON
		  + Y_BUTTON + Y_BUTTON_FROM_EDGE);
    Dynarr_add_many (template, &dlg_tem, sizeof (dlg_tem));

    /* We want no menu and standard class */
    Dynarr_add_many (template, &zeroes, 4);

    /* And the third is the dialog title. "XEmacs" as long as we do not supply
       one in descriptor. Note that the string must be in Unicode. */
    Dynarr_add_many (template, L"XEmacs", 14);

    /* We want standard dialog font */
    Dynarr_add_many (template, L"\x08MS Shell Dlg", 28);

    /* Next add text control. */
    item_tem.style = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
    item_tem.dwExtendedStyle = 0;
    item_tem.x = X_TEXT_FROM_EDGE;
    item_tem.y = Y_TEXT_FROM_EDGE;
    item_tem.cx = text_width;
    item_tem.cy = text_height;
    item_tem.id = 0xFFFF;

    ALIGN_TEMPLATE;
    Dynarr_add_many (template, &item_tem, sizeof (item_tem));

    /* Right after class id follows */
    Dynarr_add_many (template, &ones, 2);
    Dynarr_add_many (template, &static_class_id, sizeof (static_class_id));

    /* Next thing to add is control text, as Unicode string */
    push_lisp_string_as_unicode (template, XCAR (desc));

    /* Specify 0 length creation data */
    Dynarr_add_many (template, &zeroes, 2);

    /* Now it's the button time */
    item_tem.y = Y_TEXT_FROM_EDGE + text_height + Y_TEXT_FROM_BUTTON;
    item_tem.x = X_BUTTON_FROM_EDGE + (button_row_width < text_width
				       ? (text_width - button_row_width) / 2
				       : 0);
    item_tem.cy = Y_BUTTON;
    item_tem.dwExtendedStyle = 0;

    for (i = 0; i < Dynarr_length (dialog_items); ++i)
      {
	Lisp_Object* gui_item = Dynarr_atp (dialog_items, i);
	Lisp_Gui_Item *pgui_item = XGUI_ITEM (*gui_item);

	item_tem.style = (WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON
			  | (gui_item_active_p (*gui_item) ? 0 : WS_DISABLED));
	item_tem.cx = button_width (pgui_item->name);
	/* Item ids are indices into dialog_items plus offset, to avoid having
           items by reserved ids (IDOK, IDCANCEL) */
	item_tem.id = i + ID_ITEM_BIAS;

	ALIGN_TEMPLATE;
	Dynarr_add_many (template, &item_tem, sizeof (item_tem));

	/* Right after 0xFFFF and class id atom follows */
	Dynarr_add_many (template, &ones, 2);
	Dynarr_add_many (template, &button_class_id, sizeof (button_class_id));

	/* Next thing to add is control text, as Unicode string */
	{
	  Lisp_Object ctext = pgui_item->name;
	  Emchar accel_unused;
	  Bufbyte *trans = (Bufbyte *) alloca (2 * XSTRING_LENGTH (ctext) + 3);
	  Bytecount translen;

	  memcpy (trans, XSTRING_DATA (ctext), XSTRING_LENGTH (ctext) + 1);
	  translen =
	    mswindows_translate_menu_or_dialog_item (trans,
					       XSTRING_LENGTH (ctext),
					       2 * XSTRING_LENGTH (ctext) + 3,
					       &accel_unused,
					       ctext);
	  push_bufbyte_string_as_unicode (template, trans, translen);
	}

	/* Specify 0 length creation data. */
	Dynarr_add_many (template, &zeroes, 2);

	item_tem.x += item_tem.cx + X_BUTTON_SPACING;
      }
  }

  /* Now the Windows dialog structure is ready. We need to prepare a
     data structure for the new dialog, which will contain callbacks
     and the frame for these callbacks. This structure has to be
     GC-protected. The data structure itself is a cons of frame object
     and a vector of callbacks; for the protection reasons it is put
     into a statically protected list. */
  {
    Lisp_Object frame, vector, dialog_data;
    int i;
    
    XSETFRAME (frame, f);
    vector = make_vector (Dynarr_length (dialog_items), Qunbound);
    dialog_data = Fcons (frame, vector);
    for (i = 0; i < Dynarr_length (dialog_items); i++)
      XVECTOR_DATA (vector) [i] =
	XGUI_ITEM (*Dynarr_atp (dialog_items, i))->callback;

    /* Woof! Everything is ready. Pop pop pop in now! */
    if (!CreateDialogIndirectParam (NULL,
				    (LPDLGTEMPLATE) Dynarr_atp (template, 0),
				    FRAME_MSWINDOWS_HANDLE (f), dialog_proc,
				    (LPARAM) LISP_TO_VOID (dialog_data)))
      /* Something went wrong creating the dialog */
      signal_simple_error ("System error creating dialog", desc);

    Vdialog_data_list = Fcons (dialog_data, Vdialog_data_list);
  }

  /* Cease protection and free dynarrays */
  unbind_to (unbind_count, Qnil);
}

void
console_type_create_dialog_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, popup_dialog_box);
}

void
vars_of_dialog_mswindows (void)
{
  Vdialog_data_list = Qnil;
  staticpro (&Vdialog_data_list);
}
