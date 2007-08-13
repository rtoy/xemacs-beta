/* Device functions for mswindows.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

   Original authors: Jamie Zawinski and the FSF
   Rewritten by Ben Wing and Chuck Thompson.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
*/


#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "console-stream.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "sysdep.h"

/* win32 DDE management library globals */
DWORD mswindows_dde_mlid;
HSZ mswindows_dde_service;
HSZ mswindows_dde_topic_system;
HSZ mswindows_dde_item_open;


/* Control conversion of upper case file names to lower case.
   nil means no, t means yes. */
Lisp_Object Vmswindows_downcase_file_names;

/* Control whether stat() attempts to determine file type and link count
   exactly, at the expense of slower operation.  Since true hard links
   are supported on NTFS volumes, this is only relevant on NT.  */
Lisp_Object Vmswindows_get_true_file_attributes;

Lisp_Object Qinit_pre_mswindows_win, Qinit_post_mswindows_win;

static void
mswindows_init_device (struct device *d, Lisp_Object props)
{
  WNDCLASSEX wc;
  HWND desktop;
  HDC hdc;

  DEVICE_INFD (d) = DEVICE_OUTFD (d) = -1;
  init_baud_rate (d);
  init_one_device (d);

  d->device_data = xnew_and_zero (struct mswindows_device);

  desktop = GetDesktopWindow();
  hdc = GetDC(desktop);
  DEVICE_MSWINDOWS_LOGPIXELSX(d) =  GetDeviceCaps(hdc, LOGPIXELSX);
  DEVICE_MSWINDOWS_LOGPIXELSY(d) =  GetDeviceCaps(hdc, LOGPIXELSY);
  DEVICE_MSWINDOWS_PLANES(d) = GetDeviceCaps(hdc, PLANES);
  /* FIXME: Only valid if RC_PALETTE bit set in RASTERCAPS,
     what should we return for a non-palette-based device? */
  DEVICE_MSWINDOWS_CELLS(d) = GetDeviceCaps(hdc, SIZEPALETTE);
  DEVICE_MSWINDOWS_HORZRES(d) = GetDeviceCaps(hdc, HORZRES);
  DEVICE_MSWINDOWS_VERTRES(d) = GetDeviceCaps(hdc, VERTRES);
  DEVICE_MSWINDOWS_HORZSIZE(d) = GetDeviceCaps(hdc, HORZSIZE);
  DEVICE_MSWINDOWS_VERTSIZE(d) = GetDeviceCaps(hdc, VERTSIZE);
  DEVICE_MSWINDOWS_BITSPIXEL(d) = GetDeviceCaps(hdc, BITSPIXEL);
  ReleaseDC(desktop, hdc);

  DEVICE_CLASS(d) = Qcolor;

  /* Register the main window class */
  wc.cbSize = sizeof (WNDCLASSEX);
  wc.style = CS_OWNDC;	/* One DC per window */
  wc.lpfnWndProc = (WNDPROC) mswindows_wnd_proc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = MSWINDOWS_WINDOW_EXTRA_BYTES;
  wc.hInstance = NULL;	/* ? */
  wc.hIcon = LoadIcon (GetModuleHandle(NULL), XEMACS_CLASS);
  wc.hCursor = LoadCursor (NULL, IDC_ARROW);
  /* Background brush is only used during sizing, when XEmacs cannot
     take over */
  wc.hbrBackground = (HBRUSH)(COLOR_APPWORKSPACE + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = XEMACS_CLASS;
  wc.hIconSm = LoadImage (GetModuleHandle (NULL), XEMACS_CLASS,
			  IMAGE_ICON, 16, 16, 0);
  RegisterClassEx (&wc);
}

static void
mswindows_finish_init_device (struct device *d, Lisp_Object props)
{
  /* Initialise DDE management library and our related globals */
  mswindows_dde_mlid = 0;
  DdeInitialize (&mswindows_dde_mlid, mswindows_dde_callback,
		 APPCMD_FILTERINITS|CBF_FAIL_SELFCONNECTIONS|CBF_FAIL_ADVISES|
		 CBF_FAIL_POKES|CBF_FAIL_REQUESTS|CBF_SKIP_ALLNOTIFICATIONS, 0);
  
  mswindows_dde_service = DdeCreateStringHandle (mswindows_dde_mlid, XEMACS_CLASS, 0);
  mswindows_dde_topic_system = DdeCreateStringHandle (mswindows_dde_mlid, SZDDESYS_TOPIC, 0);
  mswindows_dde_item_open = DdeCreateStringHandle (mswindows_dde_mlid,
						   TEXT(MSWINDOWS_DDE_ITEM_OPEN), 0);
  DdeNameService (mswindows_dde_mlid, mswindows_dde_service, 0L, DNS_REGISTER);
}

static void
mswindows_delete_device (struct device *d)
{
  DdeNameService (mswindows_dde_mlid, 0L, 0L, DNS_REGISTER);
  DdeUninitialize (mswindows_dde_mlid);
}

static int
mswindows_device_pixel_width (struct device *d)
{
  return(DEVICE_MSWINDOWS_HORZRES(d));
}

static int
mswindows_device_pixel_height (struct device *d)
{
  return(DEVICE_MSWINDOWS_VERTRES(d));
}

static int
mswindows_device_mm_width (struct device *d)
{
  return(DEVICE_MSWINDOWS_HORZSIZE(d));
}

static int
mswindows_device_mm_height (struct device *d)
{
  return(DEVICE_MSWINDOWS_VERTSIZE(d));
}

static int
mswindows_device_bitplanes (struct device *d)
{
  return(DEVICE_MSWINDOWS_PLANES(d));
}

static int
mswindows_device_color_cells (struct device *d)
{
  return(DEVICE_MSWINDOWS_CELLS(d));
}

static unsigned int
mswindows_device_implementation_flags (void)
{
  return XDEVIMPF_PIXEL_GEOMETRY;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_mswindows (void)
{
  defsymbol (&Qinit_pre_mswindows_win, "init-pre-mswindows-win");
  defsymbol (&Qinit_post_mswindows_win, "init-post-mswindows-win");

  DEFVAR_LISP ("mswindows-downcase-file-names", &Vmswindows_downcase_file_names /*
Non-nil means convert all-upper case file names to lower case.
This applies when performing completions and file name expansion.*/ );
  Vmswindows_downcase_file_names = Qnil;

  DEFVAR_LISP ("mswindows-get-true-file-attributes", &Vmswindows_get_true_file_attributes /*
    "Non-nil means determine accurate link count in file-attributes.
This option slows down file-attributes noticeably, so is disabled by
default.  Note that it is only useful for files on NTFS volumes,
where hard links are supported.
*/ );
  Vmswindows_get_true_file_attributes = Qnil;
}

void
console_type_create_device_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, init_device);
  CONSOLE_HAS_METHOD (mswindows, finish_init_device);
/*  CONSOLE_HAS_METHOD (mswindows, mark_device); */
  CONSOLE_HAS_METHOD (mswindows, delete_device);
  CONSOLE_HAS_METHOD (mswindows, device_pixel_width);
  CONSOLE_HAS_METHOD (mswindows, device_pixel_height);
  CONSOLE_HAS_METHOD (mswindows, device_mm_width);
  CONSOLE_HAS_METHOD (mswindows, device_mm_height);
  CONSOLE_HAS_METHOD (mswindows, device_bitplanes);
  CONSOLE_HAS_METHOD (mswindows, device_color_cells);
  CONSOLE_HAS_METHOD (mswindows, device_implementation_flags);
}

void
vars_of_device_mswindows (void)
{
}
