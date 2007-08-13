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
   Rewritten for mswindows by Jonathan Harris, November 1997 for 20.4.
*/


#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "console-stream.h"
#include "events.h"
#include "event-msw.h"
#include "faces.h"
#include "frame.h"

Lisp_Object Qinit_pre_mswindows_win, Qinit_post_mswindows_win;

static void
mswindows_init_device (struct device *d, Lisp_Object props)
{
  WNDCLASS wc;
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
  ReleaseDC(desktop, hdc);

  DEVICE_CLASS(d) = Qcolor;

  /* Register the main window class */
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
  RegisterClass(&wc);		/* XXX FIXME: Should use RegisterClassEx */
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


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_mswindows (void)
{
  defsymbol (&Qinit_pre_mswindows_win, "init-pre-mswindows-win");
  defsymbol (&Qinit_post_mswindows_win, "init-post-mswindows-win");
}

void
console_type_create_device_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, init_device);
/*  CONSOLE_HAS_METHOD (mswindows, finish_init_device); */
/*  CONSOLE_HAS_METHOD (mswindows, mark_device); */
/*  CONSOLE_HAS_METHOD (mswindows, delete_device); */
  CONSOLE_HAS_METHOD (mswindows, device_pixel_width);
  CONSOLE_HAS_METHOD (mswindows, device_pixel_height);
  CONSOLE_HAS_METHOD (mswindows, device_mm_width);
  CONSOLE_HAS_METHOD (mswindows, device_mm_height);
  CONSOLE_HAS_METHOD (mswindows, device_bitplanes);
  CONSOLE_HAS_METHOD (mswindows, device_color_cells);
}

void
vars_of_device_mswindows (void)
{
}
