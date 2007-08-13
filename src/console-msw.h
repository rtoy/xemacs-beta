/* Define mswindowsindows-specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 20.4.
 */

#ifndef _XEMACS_CONSOLE_MSW_H_
#define _XEMACS_CONSOLE_MSW_H_

#include "console.h"

#include "windows.h"

DECLARE_CONSOLE_TYPE (mswindows);

struct mswindows_console
{
  int infd, outfd;
};


struct mswindows_device
{
  int logpixelsx, logpixelsy;
  int planes, cells;
  int horzres, vertres;		/* Size in pixels */
  int horzsize, vertsize;	/* Size in mm */
};

#define DEVICE_MSWINDOWS_DATA(d) DEVICE_TYPE_DATA (d, mswindows)
#define DEVICE_MSWINDOWS_LOGPIXELSX(d) 	(DEVICE_MSWINDOWS_DATA (d)->logpixelsx)
#define DEVICE_MSWINDOWS_LOGPIXELSY(d) 	(DEVICE_MSWINDOWS_DATA (d)->logpixelsy)
#define DEVICE_MSWINDOWS_PLANES(d) 	(DEVICE_MSWINDOWS_DATA (d)->planes)
#define DEVICE_MSWINDOWS_CELLS(d) 	(DEVICE_MSWINDOWS_DATA (d)->cells)
#define DEVICE_MSWINDOWS_HORZRES(d) 	(DEVICE_MSWINDOWS_DATA (d)->horzres)
#define DEVICE_MSWINDOWS_VERTRES(d) 	(DEVICE_MSWINDOWS_DATA (d)->vertres)
#define DEVICE_MSWINDOWS_HORZSIZE(d) 	(DEVICE_MSWINDOWS_DATA (d)->horzsize)
#define DEVICE_MSWINDOWS_VERTSIZE(d) 	(DEVICE_MSWINDOWS_DATA (d)->vertsize)


struct mswindows_frame
{
  /* win32 window handle */
  HWND hwnd;

  /* DC for this win32 window */
  HDC hdc;

  /* Time of last click event, for button 2 emul */
  DWORD last_click_time;

  /* Coordinates of last click event, screen-relative */
  POINTS last_click_point;

  /* Menu hashtable. See menubar-msw.c */
  Lisp_Object menu_hashtable;

  /* Menu checksum. See menubar-msw.c */
  unsigned int menu_checksum;

  /* Misc flags */
  int button2_need_lbutton : 1;
  int button2_need_rbutton : 1;
  int button2_is_down : 1;
  int ignore_next_lbutton_up : 1;
  int ignore_next_rbutton_up : 1;
  int sizing : 1;
};

#define FRAME_MSWINDOWS_DATA(f) FRAME_TYPE_DATA (f, mswindows)

#define FRAME_MSWINDOWS_HANDLE(f)	  (FRAME_MSWINDOWS_DATA (f)->hwnd)
#define FRAME_MSWINDOWS_DC(f)		  (FRAME_MSWINDOWS_DATA (f)->hdc)
#define FRAME_MSWINDOWS_MENU_HASHTABLE(f) (FRAME_MSWINDOWS_DATA (f)->menu_hashtable)
#define FRAME_MSWINDOWS_MENU_CHECKSUM(f)  (FRAME_MSWINDOWS_DATA (f)->menu_checksum)

/*
 * Redisplay functions
 */
void mswindows_redraw_exposed_area (struct frame *f, int x, int y, 
			      int width, int height);

#endif /* _XEMACS_CONSOLE_MSW_H_ */
