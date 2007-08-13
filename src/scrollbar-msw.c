/* scrollbar implementation -- mswindows interface.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Amdhal Corporation.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Darrell Kindred <dkindred+@cmu.edu>.

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

#include "console-msw.h"
#include "frame.h"
#include "window.h"
#include "scrollbar-msw.h"
#include "events.h"
#include "event-msw.h"

static void
mswindows_create_scrollbar_instance (struct frame *f, int vertical,
				     struct scrollbar_instance *sb)
{
  int orientation;
  
  sb->scrollbar_data = xnew_and_zero (struct mswindows_scrollbar_data);
  
  if (vertical)
    orientation = SBS_VERT;
  else
    orientation = SBS_HORZ;
  
  SCROLLBAR_MSW_HANDLE (sb) =
    CreateWindowEx(0, "SCROLLBAR", 0, orientation|WS_CHILD,
		 CW_USEDEFAULT, CW_USEDEFAULT,
		 CW_USEDEFAULT, CW_USEDEFAULT,
		 FRAME_MSWINDOWS_HANDLE (f),
		 NULL, NULL, NULL);
  SCROLLBAR_MSW_INFO (sb).fMask = SIF_ALL;
  GetScrollInfo(SCROLLBAR_MSW_HANDLE (sb), SB_CTL,
		&SCROLLBAR_MSW_INFO (sb));
  SetWindowLong (SCROLLBAR_MSW_HANDLE(sb), GWL_USERDATA, (LONG)sb);

#if 0
  {
	  HWND h = SCROLLBAR_MSW_HANDLE (sb);
	  int x = SetWindowLong (SCROLLBAR_MSW_HANDLE(sb), GWL_USERDATA, (LONG)sb);
	  int y = GetLastError();
	  struct scrollbar_instance *z = (struct scrollbar_instance *)GetWindowLong (SCROLLBAR_MSW_HANDLE(sb),
		  GWL_USERDATA);
	  *z = *z;
  }
#endif
}

static void
mswindows_free_scrollbar_instance (struct scrollbar_instance *sb)
{
  DestroyWindow (SCROLLBAR_MSW_HANDLE (sb));
  if (sb->scrollbar_data) 
    xfree (sb->scrollbar_data);
}

static void
mswindows_release_scrollbar_instance (struct scrollbar_instance *sb)
{
  ShowScrollBar (SCROLLBAR_MSW_HANDLE (sb), SB_CTL, 0);
  SCROLLBAR_MSW_SIZE (sb) = 0;
}

#define UPDATE_POS_FIELD(field)						   \
  if (new_##field >= 0 && SCROLLBAR_MSW_DATA (sb)->field != new_##field) { \
    SCROLLBAR_MSW_DATA (sb)->field = new_##field;			   \
    pos_changed = 1;							   \
  }

static void
mswindows_update_scrollbar_instance_values (struct window *w,
					    struct scrollbar_instance *sb,
					    int new_line_increment,
					    int new_page_increment,
					    int new_minimum, int new_maximum,
					    int new_slider_size,
					    int new_slider_position,
					    int new_scrollbar_width,
					    int new_scrollbar_height,
					    int new_scrollbar_x,
					    int new_scrollbar_y)
{
  struct frame *f;
  int pos_changed = 0;

  f = XFRAME (w->frame);

  /* These might be optimized, but since at least one will change at each
     call, it's probably not worth it. */
  SCROLLBAR_MSW_INFO (sb).nMin = new_minimum - 1;
  SCROLLBAR_MSW_INFO (sb).nMax = new_maximum - 1;
  SCROLLBAR_MSW_INFO (sb).nPage = new_slider_size;
  SCROLLBAR_MSW_INFO (sb).nPos = new_slider_position;
  SCROLLBAR_MSW_INFO (sb).fMask = SIF_ALL;
  
  SetScrollInfo(SCROLLBAR_MSW_HANDLE (sb), SB_CTL, &SCROLLBAR_MSW_INFO (sb),
		TRUE);

  UPDATE_POS_FIELD (scrollbar_x);
  UPDATE_POS_FIELD (scrollbar_y);
  UPDATE_POS_FIELD (scrollbar_width);
  UPDATE_POS_FIELD (scrollbar_height);

  if (pos_changed) 
    {
      MoveWindow(SCROLLBAR_MSW_HANDLE (sb),
		 new_scrollbar_x, new_scrollbar_y,
		 new_scrollbar_width, new_scrollbar_height,
		 TRUE);
    }
}

static void
mswindows_update_scrollbar_instance_status (struct window *w,
					    int active, int size,
					    struct scrollbar_instance *sb)
{
  struct frame *f = XFRAME (w->frame);
  
  if (SCROLLBAR_MSW_SIZE (sb) != size) {
    SCROLLBAR_MSW_SIZE (sb) = size;
    ShowScrollBar (SCROLLBAR_MSW_HANDLE (sb), SB_CTL,
		   SCROLLBAR_MSW_SIZE (sb));
    FRAMEMETH (f, set_frame_size, (f, FRAME_WIDTH (f), FRAME_HEIGHT (f)));
  }
}

void
mswindows_handle_scrollbar_event (HWND hwnd, int code, int pos)
{
  struct frame *f;
  Lisp_Object win;
  struct scrollbar_instance *sb;
  SCROLLINFO scrollinfo;

  sb = (struct scrollbar_instance *)GetWindowLong (hwnd, GWL_USERDATA);
  win = real_window (sb->mirror, 1);
  f = XFRAME (XWINDOW (win)->frame);

  switch (code)
    {
    case SB_LINEDOWN:
      enqueue_misc_user_event(win, Qscrollbar_line_down, win);
      break;
	  
    case SB_LINEUP:
      enqueue_misc_user_event(win, Qscrollbar_line_up, win);
      break;
	  
    case SB_PAGEDOWN:
      enqueue_misc_user_event(win, Qscrollbar_page_down, Fcons (win, Qnil));
      break;

    case SB_PAGEUP:
      enqueue_misc_user_event(win, Qscrollbar_page_up, Fcons (win, Qnil));
      break;
	  
    case SB_BOTTOM:
      enqueue_misc_user_event(win, Qscrollbar_to_bottom, win);
      break;

    case SB_TOP:
      enqueue_misc_user_event(win, Qscrollbar_to_top, win);
      break;

    case SB_THUMBTRACK:
    case SB_THUMBPOSITION:
      enqueue_misc_user_event (win, Qscrollbar_vertical_drag,
			       Fcons (win, make_int (pos)));
      break;
    }
}

#ifdef MEMORY_USAGE_STATS

static int
mswindows_compute_scrollbar_instance_usage (struct device *d,
				    struct scrollbar_instance *inst,
				    struct overhead_stats *ovstats)
{
  int total = 0;

  while (inst)
    {
      struct mswindows_scrollbar_data *data =
	(struct mswindows_scrollbar_data *) inst->scrollbar_data;

      total += malloced_storage_size (data, sizeof (*data), ovstats);
      inst = inst->next;
    }

  return total;
}

#endif /* MEMORY_USAGE_STATS */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_scrollbar_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, create_scrollbar_instance);
  CONSOLE_HAS_METHOD (mswindows, free_scrollbar_instance);
  CONSOLE_HAS_METHOD (mswindows, release_scrollbar_instance);
  CONSOLE_HAS_METHOD (mswindows, update_scrollbar_instance_values);
  CONSOLE_HAS_METHOD (mswindows, update_scrollbar_instance_status);
/*  CONSOLE_HAS_METHOD (mswindows, scrollbar_width_changed_in_frame); */
#ifdef MEMORY_USAGE_STATS
  CONSOLE_HAS_METHOD (mswindows, compute_scrollbar_instance_usage);
#endif
}

void
vars_of_scrollbar_mswindows(void)
{
  Fprovide (intern ("mswindows-scrollbars"));
}

