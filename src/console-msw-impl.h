/* Define mswindows-specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2001, 2002 Ben Wing.

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

#ifndef INCLUDED_console_msw_impl_h_
#define INCLUDED_console_msw_impl_h_

#include "console-impl.h"
#include "console-msw.h"

/*
 * Consoles
 */

DECLARE_CONSOLE_TYPE (mswindows);

struct mswindows_console
{
  int infd, outfd;
};

DECLARE_CONSOLE_TYPE (msprinter);

/*
 * Printer settings, aka devmode
 */

struct Lisp_Devmode
{
  struct LCRECORD_HEADER header;

  /* Pointer to the DEVMODE structure */
  DEVMODEW *devmode;

  /* Full printer name. It can be longer than devmode->dmDeviceName
     can accommodate, so need to keep it separately */
  Lisp_Object printer_name;

  /* Printer device this object is currently selected in, or Qnil
     if not selected */
  Lisp_Object device;

};

#define DEVMODE_SIZE(dm) ((dm)->dmSize + (dm)->dmDriverExtra)
#define XDEVMODE_SIZE(x) ((x)->devmode ? DEVMODE_SIZE((x)->devmode) : 0)

/*
 * Devices
 */

struct mswindows_device
{
  Lisp_Object fontlist;		/* List of (STRING . FIXED-P), device fonts */
  HDC hcdc;			/* Compatible DC */
  DWORD update_tick;		/* Used when device is modified through
				   Windows messages, see WM_DISPLAYCHANGE
				   in event-msw.c */
};

#define DEVICE_MSWINDOWS_DATA(d) DEVICE_TYPE_DATA (d, mswindows)
#define DEVICE_MSWINDOWS_FONTLIST(d)    (DEVICE_MSWINDOWS_DATA (d)->fontlist)
#define DEVICE_MSWINDOWS_HCDC(d)        (DEVICE_MSWINDOWS_DATA (d)->hcdc)
#define DEVICE_MSWINDOWS_UPDATE_TICK(d) (DEVICE_MSWINDOWS_DATA (d)->update_tick)

struct msprinter_device
{
  HDC hdc, hcdc;		/* Printer and the comp. DCs */
  HANDLE hprinter;
  Lisp_Object name;
  Lisp_Object devmode;
  Lisp_Object fontlist;
};

#define DEVICE_MSPRINTER_DATA(d) DEVICE_TYPE_DATA (d, msprinter)
#define DEVICE_MSPRINTER_HDC(d) 	(DEVICE_MSPRINTER_DATA (d)->hdc)
#define DEVICE_MSPRINTER_HCDC(d)	(DEVICE_MSPRINTER_DATA (d)->hcdc)
#define DEVICE_MSPRINTER_HPRINTER(d) 	(DEVICE_MSPRINTER_DATA (d)->hprinter)
#define DEVICE_MSPRINTER_FONTLIST(d) 	(DEVICE_MSPRINTER_DATA (d)->fontlist)
#define DEVICE_MSPRINTER_NAME(d) 	(DEVICE_MSPRINTER_DATA (d)->name)
#define DEVICE_MSPRINTER_DEVMODE(d) 	(DEVICE_MSPRINTER_DATA (d)->devmode)

#define CONSOLE_TYPESYM_MSPRINTER_P(typesym) EQ (typesym, Qmsprinter)
#define DEVICE_MSPRINTER_P(dev) CONSOLE_TYPESYM_MSPRINTER_P (DEVICE_TYPE (dev))
#define CHECK_MSPRINTER_DEVICE(z) CHECK_DEVICE_TYPE (z, msprinter)
#define CONCHECK_MSPRINTER_DEVICE(z) CONCHECK_DEVICE_TYPE (z, msprinter)

/* Common checks */

#define DEVICE_MSGDI_P(dev) (DEVICE_MSWINDOWS_P(dev) || DEVICE_MSPRINTER_P(dev))
#define CHECK_MSGDI_DEVICE(d)				\
  do {							\
    CHECK_DEVICE (d);					\
    if (!(DEVICEP (d) && DEVICE_MSGDI_P(XDEVICE(d))))	\
      dead_wrong_type_argument				\
	(list3 (Qor, Qmswindows, Qmsprinter), d);	\
  } while (0)
#define CONCHECK_MSGDI_DEVICE(d)			\
  do {							\
    CHECK_DEVICE (d);					\
    if (!(DEVICEP (d) && DEVICE_MSGDI_P(XDEVICE(d))))	\
      wrong_type_argument				\
	(list3 (Qor, Qmswindows, Qmsprinter), d);	\
  } while (0)

/*
 * Frames
 */

struct mswindows_frame
{
  /* win32 window handle */
  HWND hwnd;

  /* DC for this win32 window */
  HDC hdc;

  /* Used with DeferWindowPos */
  HDWP hdwp;

  /* Time of last click event, for button 2 emul */
  DWORD last_click_time;

  /* Mods of last click event */
  DWORD last_click_mods;

  /* Coordinates of last click event, screen-relative */
  POINTS last_click_point;
#ifdef HAVE_TOOLBARS
  /* Toolbar hash table. See toolbar-msw.c */
  Lisp_Object toolbar_hash_table;
  unsigned int toolbar_checksum[4];
#endif

  /* Menu hash table. See menubar-msw.c */
  Lisp_Object menu_hash_table;

  /* Menu checksum. See menubar-msw.c */
  unsigned int menu_checksum;

  /* Widget glyphs attached to this frame. See glyphs-msw.c */
  Lisp_Object widget_hash_table1, widget_hash_table2, widget_hash_table3;

  /* Frame title hash value. See frame-msw.c */
  unsigned int title_checksum;

  /* Real character width and height of the frame.
     FRAME_{HEIGHT,WIDTH} do not work for pixel geometry! */
  int charheight, charwidth;

#ifdef MULE
  int cursor_x;
  int cursor_y;
  face_index cursor_findex;
#endif

  /* Misc flags */
  int button2_need_lbutton : 1;
  int button2_need_rbutton : 1;
  int button2_is_down : 1;
  int ignore_next_lbutton_up : 1;
  int ignore_next_rbutton_up : 1;
  int sizing : 1;
  int paint_pending : 1; /* Whether a WM_PAINT magic event has been queued */
  int popup : 1; /* frame is a popup frame */

  /* Geometry, in characters, as specified by proplist during frame
     creation. Members are set to -1 for unspecified */
  XEMACS_RECT_WH *target_rect;
};

#define FRAME_MSWINDOWS_DATA(f) FRAME_TYPE_DATA (f, mswindows)

#define FRAME_MSWINDOWS_HANDLE(f)	   (FRAME_MSWINDOWS_DATA (f)->hwnd)
#define FRAME_MSWINDOWS_DC(f)		   (FRAME_MSWINDOWS_DATA (f)->hdc)
#define FRAME_MSWINDOWS_MENU_HASH_TABLE(f) (FRAME_MSWINDOWS_DATA (f)->menu_hash_table)
#define FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE(f) \
 (FRAME_MSWINDOWS_DATA (f)->toolbar_hash_table)
#define FRAME_MSWINDOWS_WIDGET_HASH_TABLE1(f) \
 (FRAME_MSWINDOWS_DATA (f)->widget_hash_table1)
#define FRAME_MSWINDOWS_WIDGET_HASH_TABLE2(f) \
 (FRAME_MSWINDOWS_DATA (f)->widget_hash_table2)
#define FRAME_MSWINDOWS_WIDGET_HASH_TABLE3(f) \
 (FRAME_MSWINDOWS_DATA (f)->widget_hash_table3)
#define FRAME_MSWINDOWS_TOOLBAR_CHECKSUM(f,pos) \
 (FRAME_MSWINDOWS_DATA (f)->toolbar_checksum[pos])
#define FRAME_MSWINDOWS_MENU_CHECKSUM(f)  (FRAME_MSWINDOWS_DATA (f)->menu_checksum)
#define FRAME_MSWINDOWS_TITLE_CHECKSUM(f) (FRAME_MSWINDOWS_DATA (f)->title_checksum)
#define FRAME_MSWINDOWS_CHARWIDTH(f)	  (FRAME_MSWINDOWS_DATA (f)->charwidth)
#define FRAME_MSWINDOWS_CHARHEIGHT(f)	  (FRAME_MSWINDOWS_DATA (f)->charheight)
#define FRAME_MSWINDOWS_TARGET_RECT(f)	  (FRAME_MSWINDOWS_DATA (f)->target_rect)

#define FRAME_MSWINDOWS_POPUP(f)	  (FRAME_MSWINDOWS_DATA (f)->popup)

#ifdef MULE
# define FRAME_MSWINDOWS_CURSOR_X(f) (FRAME_MSWINDOWS_DATA (f)->cursor_x)
# define FRAME_MSWINDOWS_CURSOR_Y(f) (FRAME_MSWINDOWS_DATA (f)->cursor_y)
# define FRAME_MSWINDOWS_CURSOR_FINDEX(f) (FRAME_MSWINDOWS_DATA (f)->cursor_findex)
#endif

/* Frame check and validation macros */
#define FRAME_MSWINDOWS_P(frm) CONSOLE_TYPESYM_MSWINDOWS_P (FRAME_TYPE (frm))
#define CHECK_MSWINDOWS_FRAME(z) CHECK_FRAME_TYPE (z, mswindows)
#define CONCHECK_MSWINDOWS_FRAME(z) CONCHECK_FRAME_TYPE (z, mswindows)

/* win32 window LONG indices */
#define XWL_FRAMEOBJ	0
#define XWL_COUNT	1	/* Number of LONGs that we use */
#define MSWINDOWS_WINDOW_EXTRA_BYTES	(XWL_COUNT * 4)

/*
 * Printer frame, aka printer job
 */

struct msprinter_frame
{
  int left_margin, top_margin,		/* All in twips */
    right_margin, bottom_margin;
  int charheight, charwidth;		/* As per proplist or -1 if not given */
  int pix_left, pix_top;		/* Calculated in init_frame_*, VP offset */
  int job_started : 1;
  int page_started : 1;
};

#define FRAME_MSPRINTER_DATA(f) FRAME_TYPE_DATA (f, msprinter)
#define FRAME_MSPRINTER_LEFT_MARGIN(f)	 (FRAME_MSPRINTER_DATA (f)->left_margin)
#define FRAME_MSPRINTER_RIGHT_MARGIN(f)	 (FRAME_MSPRINTER_DATA (f)->top_margin)
#define FRAME_MSPRINTER_TOP_MARGIN(f)	 (FRAME_MSPRINTER_DATA (f)->right_margin)
#define FRAME_MSPRINTER_BOTTOM_MARGIN(f) (FRAME_MSPRINTER_DATA (f)->bottom_margin)
#define FRAME_MSPRINTER_JOB_STARTED(f)	 (FRAME_MSPRINTER_DATA (f)->job_started)
#define FRAME_MSPRINTER_PAGE_STARTED(f)	 (FRAME_MSPRINTER_DATA (f)->page_started)
#define FRAME_MSPRINTER_CHARWIDTH(f)	 (FRAME_MSPRINTER_DATA (f)->charwidth)
#define FRAME_MSPRINTER_CHARHEIGHT(f)	 (FRAME_MSPRINTER_DATA (f)->charheight)
#define FRAME_MSPRINTER_PIXLEFT(f)	 (FRAME_MSPRINTER_DATA (f)->pix_left)
#define FRAME_MSPRINTER_PIXTOP(f)	 (FRAME_MSPRINTER_DATA (f)->pix_top)

/*
 * Events
 */

/* win32 messages / magic event types */
#define XM_BUMPQUEUE	(WM_USER + 101)
#define XM_MAPFRAME	(WM_USER + 102)
#define XM_UNMAPFRAME	(WM_USER + 103)

struct mswindows_dialog_id
{
  struct LCRECORD_HEADER header;

  Lisp_Object frame;
  Lisp_Object callbacks;
  HWND hwnd;
};

#endif /* INCLUDED_console_msw_impl_h_ */
