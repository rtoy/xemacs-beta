/* Define mswindows-specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2001, 2002, 2003 Ben Wing.

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

/* Authorship:

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
 */

#ifndef INCLUDED_console_msw_h_
#define INCLUDED_console_msw_h_

#include "console.h"
#include "syswindows.h"

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

/* The name of the main window class */
#define XEMACS_CLASS "XEmacs"  /* WARNING: uses of this need XETEXT */

/* WARNING: uses of this need XETEXT */
#define XEMACS_CONTROL_CLASS "XEmacsControl"

/*
 * Printer settings, aka devmode
 */

typedef struct Lisp_Devmode Lisp_Devmode;


DECLARE_LRECORD (devmode, Lisp_Devmode);
#define XDEVMODE(x) XRECORD (x, devmode, Lisp_Devmode)
#define wrap_devmode(p) wrap_record (p, devmode)
#define DEVMODEP(x) RECORDP (x, devmode)
#define CHECK_DEVMODE(x) CHECK_RECORD (x, devmode)
#define CONCHECK_DEVMODE(x) CONCHECK_RECORD (x, devmode)

/*
 * Devices
 */

#define MSW_FONTSIZE (LF_FACESIZE * 4 + 12)

/* Printer functions in frame-msw.c */
void msprinter_start_page (struct frame *f);

/*
 * Frames
 */
typedef struct
{
  int left;
  int top;
  int width;
  int height;
} XEMACS_RECT_WH;

/*
 * Random globals
 */

/* win32 "Windows" procedure */
LRESULT WINAPI mswindows_wnd_proc (HWND hwnd, UINT msg, WPARAM wParam,
				   LPARAM lParam);
LRESULT WINAPI mswindows_control_wnd_proc (HWND hwnd,
					   UINT msg, WPARAM wParam,
					   LPARAM lParam);

void mswindows_size_frame_internal (struct frame *f, XEMACS_RECT_WH *dest);
HWND mswindows_get_selected_frame_hwnd (void);
void mswindows_enqueue_magic_event (HWND hwnd, UINT msg);
int mswindows_is_dialog_msg (MSG *msg);

/* win32 DDE management library */

/* WARNING: uses of these constants need XETEXT */
#define MSWINDOWS_DDE_ITEM_OPEN "Open"
#define MSWINDOWS_DDE_TOPIC_EVAL "Eval"
#define MSWINDOWS_DDE_ITEM_RESULT "Result"

extern DWORD mswindows_dde_mlid;
extern HSZ mswindows_dde_service;
extern HSZ mswindows_dde_topic_system;
extern HSZ mswindows_dde_topic_eval;
extern HSZ mswindows_dde_item_result;
extern HSZ mswindows_dde_item_open;
HDDEDATA CALLBACK mswindows_dde_callback (UINT uType, UINT uFmt, HCONV hconv,
					  HSZ hszTopic, HSZ hszItem,
					  HDDEDATA hdata,
					  DWORD dwData1, DWORD dwData2);

void mswindows_enqueue_dispatch_event (Lisp_Object event);
void mswindows_enqueue_misc_user_event (Lisp_Object channel,
					Lisp_Object function,
					Lisp_Object object);
Lisp_Object mswindows_cancel_dispatch_event (Lisp_Event *event);
Lisp_Object mswindows_pump_outstanding_events (void);
void mswindows_unmodalize_signal_maybe (void);

COLORREF mswindows_string_to_color (const Ibyte *name);

#ifdef HAVE_WIN32_PROCESSES
HANDLE get_nt_process_handle_only_first_time (Lisp_Process *p);
HANDLE get_nt_process_handle (Lisp_Process *p);
#endif

void mswindows_unwait_process (Lisp_Process *p);

extern Lisp_Object Vmswindows_frame_being_created;
extern Lisp_Object mswindows_frame_being_created;

void mswindows_get_workspace_coords (RECT *rc);

Lisp_Object mswindows_enumerate_fonts (HDC hdc);

#ifdef HAVE_MENUBARS
int mswindows_char_is_accelerator (struct frame *f, Ichar ch);
#endif

#ifdef HAVE_TOOLBARS
Lisp_Object mswindows_get_toolbar_button_text (struct frame *f,
					       int command_id);
Lisp_Object mswindows_handle_toolbar_wm_command (struct frame *f,
						 HWND ctrl, WORD id);
#endif
Lisp_Object mswindows_handle_gui_wm_command (struct frame *f,
					     HWND ctrl, LPARAM id);
Lisp_Object mswindows_translate_menu_or_dialog_item (Lisp_Object item,
						     Ichar *accel);
void mswindows_handle_destroyclipboard (void);

Lisp_Object mswindows_handle_print_dialog_box (struct frame *f,
					       Lisp_Object keys);
Lisp_Object mswindows_handle_page_setup_dialog_box (struct frame *f,
						    Lisp_Object keys);
int mswindows_get_default_margin (Lisp_Object prop);

void mswindows_register_popup_frame (Lisp_Object frame);
void mswindows_unregister_popup_frame (Lisp_Object frame);

void mswindows_destroy_selection (Lisp_Object selection);

int mswindows_window_is_xemacs (HWND hwnd);

Lisp_Object msprinter_default_printer (void);

Lisp_Object mswindows_find_frame (HWND hwnd);

/* Defined in console-msw.c */
EXFUN (Fmswindows_message_box, 3);
extern int mswindows_message_outputted;
void mswindows_hide_console (void);
int mswindows_output_console_string (const Ibyte *ptr, Bytecount len);
void write_string_to_mswindows_debugging_output (Ibyte *str, Bytecount len);


#ifdef MULE
Lisp_Object mswindows_get_code_page_charset (int code_page);
void mswindows_start_ime_composition (struct frame *f);
#endif /* MULE */

/* Defined in intl-win32.c */
EXFUN (Fmswindows_set_current_locale, 1);
EXFUN (Fmswindows_current_locale, 0);
EXFUN (Fmswindows_user_default_locale, 0);
EXFUN (Fmswindows_system_default_locale, 0);
EXFUN (Fmswindows_locale_code_page, 1);
EXFUN (Fmswindows_supported_locales, 0);
EXFUN (Fmswindows_charset_code_page, 1);
EXFUN (Fmswindows_charset_registry, 1);
EXFUN (Fmswindows_set_charset_code_page, 2);

struct mswindows_dialog_id;

DECLARE_LRECORD (mswindows_dialog_id, struct mswindows_dialog_id);
#define XMSWINDOWS_DIALOG_ID(x) XRECORD (x, mswindows_dialog_id, struct mswindows_dialog_id)
#define wrap_mswindows_dialog_id(p) wrap_record (p, mswindows_dialog_id)
#define MSWINDOWS_DIALOG_IDP(x) RECORDP (x, mswindows_dialog_id)
#define CHECK_MSWINDOWS_DIALOG_ID(x) CHECK_RECORD (x, mswindows_dialog_id)
#define CONCHECK_MSWINDOWS_DIALOG_ID(x) CONCHECK_RECORD (x, mswindows_dialog_id)

#endif /* INCLUDED_console_msw_h_ */
