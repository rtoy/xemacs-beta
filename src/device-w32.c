/* Device functions for win32.
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
   Rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
*/


#include <config.h>
#include "lisp.h"

#include "console-w32.h"
#include "console-stream.h"
#include "events.h"
#include "event-w32.h"
#include "faces.h"
#include "frame.h"

Lisp_Object Qinit_pre_w32_win, Qinit_post_w32_win;

DWORD w32_main_thread_id;
DWORD w32_win_thread_id;

static void
w32_init_device (struct device *d, Lisp_Object props)
{
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
  HWND desktop;
  HDC hdc;
  MSG msg;
  HANDLE handle;

  /* Ensure our message queue is created */
  PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

  w32_main_thread_id = GetCurrentThreadId ();
#if 0
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (), 
		   GetCurrentProcess (), &hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);
#endif
  handle = CreateThread (NULL, 0, 
			 (LPTHREAD_START_ROUTINE) w32_win_thread,
			 0, 0, &w32_win_thread_id);
  AttachThreadInput (w32_main_thread_id, w32_win_thread_id, TRUE);

  d->device_data = xnew_and_zero (struct w32_device);

  desktop = GetDesktopWindow();
  hdc = GetDC(desktop);
  DEVICE_W32_LOGPIXELSX(d) =  GetDeviceCaps(hdc, LOGPIXELSX);
  DEVICE_W32_LOGPIXELSY(d) =  GetDeviceCaps(hdc, LOGPIXELSY);
  DEVICE_W32_PLANES(d) = GetDeviceCaps(hdc, PLANES);
  /* FIXME: Only valid if RC_PALETTE bit set in RASTERCAPS,
     what should we return for a non-palette-based device? */
  DEVICE_W32_CELLS(d) = GetDeviceCaps(hdc, SIZEPALETTE);
  DEVICE_W32_HORZRES(d) = GetDeviceCaps(hdc, HORZRES);
  DEVICE_W32_VERTRES(d) = GetDeviceCaps(hdc, VERTRES);
  DEVICE_W32_HORZSIZE(d) = GetDeviceCaps(hdc, HORZSIZE);
  DEVICE_W32_VERTSIZE(d) = GetDeviceCaps(hdc, VERTSIZE);
  ReleaseDC(desktop, hdc);

  /* Wait for windows thread to be ready */
  GetMessage (&msg, NULL, WM_XEMACS_ACK, WM_XEMACS_ACK);
}

static int
w32_device_pixel_width (struct device *d)
{
  return(DEVICE_W32_HORZRES(d));
}

static int
w32_device_pixel_height (struct device *d)
{
  return(DEVICE_W32_VERTRES(d));
}

static int
w32_device_mm_width (struct device *d)
{
  return(DEVICE_W32_HORZSIZE(d));
}

static int
w32_device_mm_height (struct device *d)
{
  return(DEVICE_W32_VERTSIZE(d));
}

static int
w32_device_bitplanes (struct device *d)
{
  return(DEVICE_W32_PLANES(d));
}

static int
w32_device_color_cells (struct device *d)
{
  return(DEVICE_W32_CELLS(d));
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_w32 (void)
{
  defsymbol (&Qinit_pre_w32_win, "init-pre-w32-win");
  defsymbol (&Qinit_post_w32_win, "init-post-w32-win");
}

void
console_type_create_device_w32 (void)
{
  CONSOLE_HAS_METHOD (w32, init_device);
/*  CONSOLE_HAS_METHOD (w32, finish_init_device); */
/*  CONSOLE_HAS_METHOD (w32, mark_device); */
/*  CONSOLE_HAS_METHOD (w32, delete_device); */
  CONSOLE_HAS_METHOD (w32, device_pixel_width);
  CONSOLE_HAS_METHOD (w32, device_pixel_height);
  CONSOLE_HAS_METHOD (w32, device_mm_width);
  CONSOLE_HAS_METHOD (w32, device_mm_height);
  CONSOLE_HAS_METHOD (w32, device_bitplanes);
  CONSOLE_HAS_METHOD (w32, device_color_cells);
}

void
vars_of_device_w32 (void)
{
}
