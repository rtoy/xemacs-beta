/* device functions for mswindows.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

/* This file Mule-ized 8-11-2000. */

/* Authorship:

   Original authors: Jamie Zawinski and the FSF
   Rewritten by Ben Wing and Chuck Thompson.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
   Print support added by Kirill Katsnelson, July 2000.
*/

#define NEED_MSWINDOWS_COMMCTRL
#define NEED_MSWINDOWS_OBJBASE /* for CoInitialize */

#include <config.h>
#include "lisp.h"

#include "device-impl.h"
#include "events.h"
#include "faces.h"
#include "frame.h"

#include "console-msw-impl.h"
#include "console-stream.h"
#include "objects-msw.h"

#include "sysdep.h"

/* win32 DDE management library globals */
#ifdef HAVE_DRAGNDROP
DWORD mswindows_dde_mlid;
int mswindows_dde_enable;
HSZ mswindows_dde_service;
HSZ mswindows_dde_topic_system;
HSZ mswindows_dde_topic_eval;
HSZ mswindows_dde_item_result;
HSZ mswindows_dde_item_open;
#endif

Lisp_Object Qinit_pre_mswindows_win, Qinit_post_mswindows_win;
Lisp_Object Qdevmodep;

static Lisp_Object Q_allow_selection;
static Lisp_Object Q_allow_pages;
static Lisp_Object Q_selected_page_button;
static Lisp_Object Qselected_page_button;

static const struct memory_description mswindows_device_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct mswindows_device, fontlist) },
  { XD_END }
};

extern const struct sized_memory_description mswindows_device_data_description;

const struct sized_memory_description mswindows_device_data_description = {
  sizeof (struct mswindows_device), mswindows_device_data_description_1
};

static Lisp_Object allocate_devmode (DEVMODEW *src_devmode, int do_copy,
				     Lisp_Object src_name, struct device *d);

/************************************************************************/
/*                               helpers                                */
/************************************************************************/

static Lisp_Object
build_syscolor_string (int idx)
{
  return (idx < 0 ? Qnil : mswindows_color_to_string (GetSysColor (idx)));
}

static Lisp_Object
build_syscolor_cons (int index1, int index2)
{
  Lisp_Object color1, color2;
  struct gcpro gcpro1;
  GCPRO1 (color1);
  color1 = build_syscolor_string (index1);
  color2 = build_syscolor_string (index2);
  RETURN_UNGCPRO (Fcons (color1, color2));
}

static Lisp_Object
build_sysmetrics_cons (int index1, int index2)
{
  return Fcons (index1 < 0 ? Qnil : make_int (GetSystemMetrics (index1)),
		index2 < 0 ? Qnil : make_int (GetSystemMetrics (index2)));
}

static Lisp_Object
build_devicecaps_cons (HDC hdc, int index1, int index2)
{
  return Fcons (index1 < 0 ? Qnil : make_int (GetDeviceCaps (hdc, index1)),
		index2 < 0 ? Qnil : make_int (GetDeviceCaps (hdc, index2)));
}


/************************************************************************/
/*                          display methods                             */
/************************************************************************/

static void
mswindows_init_device (struct device *d, Lisp_Object props)
{
  HDC hdc;
  WNDCLASSEXW wc;

  DEVICE_CLASS (d) = Qcolor;
  DEVICE_INFD (d) = DEVICE_OUTFD (d) = -1;
  init_baud_rate (d);
  init_one_device (d);

  d->device_data = xnew_and_zero (struct mswindows_device);
  hdc = CreateCompatibleDC (NULL);
  assert (hdc != NULL);
  DEVICE_MSWINDOWS_HCDC (d) = hdc;
  DEVICE_MSWINDOWS_FONTLIST (d) = mswindows_enumerate_fonts (hdc);
  DEVICE_MSWINDOWS_UPDATE_TICK (d) = GetTickCount ();

  /* Register the main window class */
  wc.cbSize = sizeof (wc);
  wc.style = CS_OWNDC;	/* One DC per window */
  wc.lpfnWndProc = (WNDPROC) mswindows_wnd_proc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = MSWINDOWS_WINDOW_EXTRA_BYTES;
  /* This must match whatever is passed to CreateWIndowEx, NULL is ok
     for this. */
  wc.hInstance = NULL;	
  wc.hIcon = qxeLoadIcon (qxeGetModuleHandle (NULL), XETEXT (XEMACS_CLASS));
  wc.hCursor = qxeLoadCursor (NULL, IDC_ARROW);
  /* Background brush is only used during sizing, when XEmacs cannot
     take over */
  wc.hbrBackground = (HBRUSH) (COLOR_APPWORKSPACE + 1);
  wc.lpszMenuName = NULL;

  wc.lpszClassName = (XELPTSTR) XETEXT (XEMACS_CLASS);
  wc.hIconSm = (HICON) qxeLoadImage (qxeGetModuleHandle (NULL),
				     XETEXT (XEMACS_CLASS),
				     IMAGE_ICON, 16, 16, 0);
  qxeRegisterClassEx (&wc);

#ifdef HAVE_WIDGETS
  xzero (wc);
  /* Register the main window class */
  wc.cbSize = sizeof (wc);
  wc.lpfnWndProc = (WNDPROC) mswindows_control_wnd_proc;
  wc.lpszClassName = (XELPTSTR) XETEXT (XEMACS_CONTROL_CLASS);
  wc.hInstance = NULL;
  qxeRegisterClassEx (&wc);
#endif

#if defined (HAVE_TOOLBARS) || defined (HAVE_WIDGETS)
  InitCommonControls ();
#endif
}

#ifdef HAVE_DRAGNDROP
static void
mswindows_init_dde (void)
{
  /* Initialize DDE management library and our related globals. We execute a
   * dde Open ("file") by simulating a drop, so this depends on dnd support. */

  mswindows_dde_mlid = 0;
  mswindows_dde_enable = 0;
  qxeDdeInitialize (&mswindows_dde_mlid, (PFNCALLBACK)mswindows_dde_callback,
		    APPCMD_FILTERINITS|CBF_FAIL_SELFCONNECTIONS|
		    CBF_FAIL_POKES|CBF_SKIP_ALLNOTIFICATIONS,
		    0);
  
  mswindows_dde_service =
    qxeDdeCreateStringHandle (mswindows_dde_mlid,
			      XETEXT (XEMACS_CLASS),
			      XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);
  /* The following strings we Unicode-ize ourselves:
     -- SZDDESYS_TOPIC is system-provided
     -- MSWINDOWS_DDE_TOPIC_EVAL is defined by us
     -- MSWINDOWS_DDE_ITEM_RESULT is defined by us
     -- MSWINDOWS_DDE_ITEM_OPEN is used in internal-format comparisons
  */
  mswindows_dde_topic_system =
    qxeDdeCreateStringHandle (mswindows_dde_mlid,
			      XETEXT (SZDDESYS_TOPIC),
			      XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);
  mswindows_dde_topic_eval =
    qxeDdeCreateStringHandle (mswindows_dde_mlid,
			      XETEXT (MSWINDOWS_DDE_TOPIC_EVAL),
			      XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);
  mswindows_dde_item_result =
    qxeDdeCreateStringHandle (mswindows_dde_mlid,
			      XETEXT (MSWINDOWS_DDE_ITEM_RESULT),
			      XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);
  mswindows_dde_item_open =
    qxeDdeCreateStringHandle (mswindows_dde_mlid,
			      XETEXT (MSWINDOWS_DDE_ITEM_OPEN),
			      XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);
  DdeNameService (mswindows_dde_mlid, mswindows_dde_service, 0L, DNS_REGISTER);
}
#endif /* HAVE_DRAGNDROP */

void 
init_mswindows_dde_very_early (void)
{
#if !defined (NO_CYGWIN_COM_SUPPORT)
  /* Needed by SHBrowseForFolder, so do it always */
  CoInitialize (NULL);
#endif

#ifdef HAVE_DRAGNDROP
  /* Initializing dde when the device is created is too late - the
     client will give up waiting.  Instead we initialize here and tell
     the client we're too busy until the rest of initialization has
     happened. */
  mswindows_init_dde ();
#endif
}

static void
mswindows_finish_init_device (struct device *d, Lisp_Object props)
{
#ifdef HAVE_DRAGNDROP
  /* Tell pending clients we are ready. */
  mswindows_dde_enable = 1;
#endif
}

static void
mswindows_delete_device (struct device *d)
{
#ifdef HAVE_DRAGNDROP
  DdeNameService (mswindows_dde_mlid, 0L, 0L, DNS_UNREGISTER);
  DdeFreeStringHandle (mswindows_dde_mlid, mswindows_dde_item_result);
  DdeFreeStringHandle (mswindows_dde_mlid, mswindows_dde_item_open);
  DdeFreeStringHandle (mswindows_dde_mlid, mswindows_dde_topic_system);
  DdeFreeStringHandle (mswindows_dde_mlid, mswindows_dde_topic_eval);
  DdeFreeStringHandle (mswindows_dde_mlid, mswindows_dde_service);
  DdeUninitialize (mswindows_dde_mlid);

# if !defined (NO_CYGWIN_COM_SUPPORT)
  CoUninitialize ();
# endif
#endif

  DeleteDC (DEVICE_MSWINDOWS_HCDC (d));
  xfree (d->device_data);
}

void
mswindows_get_workspace_coords (RECT *rc)
{
  qxeSystemParametersInfo (SPI_GETWORKAREA, 0, rc, 0);
}

static void
mswindows_mark_device (struct device *d)
{
  mark_object (DEVICE_MSWINDOWS_FONTLIST (d));
}

static Lisp_Object
mswindows_device_system_metrics (struct device *d,
				 enum device_metrics m)
{
  const HDC hdc = DEVICE_MSWINDOWS_HCDC(d);

  switch (m)
    {
    case DM_size_device:
      return Fcons (make_int (GetDeviceCaps (hdc, HORZRES)),
		    make_int (GetDeviceCaps (hdc, VERTRES)));
      break;
    case DM_device_dpi:
      return Fcons (make_int (GetDeviceCaps (hdc, LOGPIXELSX)),
		    make_int (GetDeviceCaps (hdc, LOGPIXELSY)));
      break;
    case DM_size_device_mm:
      return Fcons (make_int (GetDeviceCaps (hdc, HORZSIZE)),
		    make_int (GetDeviceCaps (hdc, VERTSIZE)));
      break;
    case DM_num_bit_planes:
      /* this is what X means by bitplanes therefore we ought to be
         consistent. num planes is always 1 under mswindows and
         therefore useless */
      return make_int (GetDeviceCaps (hdc, BITSPIXEL));
      break;
    case DM_num_color_cells:
      /* #### SIZEPALETTE only valid if RC_PALETTE bit set in RASTERCAPS,
         what should we return for a non-palette-based device? */
      return make_int (GetDeviceCaps (hdc, SIZEPALETTE));
      break;

      /*** Colors ***/
#define FROB(met, fore, back)				\
    case DM_##met:					\
      return build_syscolor_cons (fore, back);

      FROB (color_default, COLOR_WINDOWTEXT, COLOR_WINDOW);
      FROB (color_select, COLOR_HIGHLIGHTTEXT, COLOR_HIGHLIGHT);
      FROB (color_balloon, COLOR_INFOTEXT, COLOR_INFOBK);
      FROB (color_3d_face, COLOR_BTNTEXT, COLOR_BTNFACE);
      FROB (color_3d_light, COLOR_3DHILIGHT, COLOR_3DLIGHT);
      FROB (color_3d_dark, COLOR_3DDKSHADOW, COLOR_3DSHADOW);
      FROB (color_menu, COLOR_MENUTEXT, COLOR_MENU);
      FROB (color_menu_highlight, COLOR_HIGHLIGHTTEXT, COLOR_HIGHLIGHT);
      FROB (color_menu_button, COLOR_MENUTEXT, COLOR_MENU);
      FROB (color_menu_disabled, COLOR_GRAYTEXT, COLOR_MENU);
      FROB (color_toolbar, COLOR_BTNTEXT, COLOR_BTNFACE);
      FROB (color_scrollbar, COLOR_CAPTIONTEXT, COLOR_SCROLLBAR);
      FROB (color_desktop, -1, COLOR_DESKTOP);
      FROB (color_workspace, -1, COLOR_APPWORKSPACE);
#undef FROB

      /*** Sizes ***/
#define FROB(met, index1, index2)			\
    case DM_##met:					\
      return build_sysmetrics_cons (index1, index2);

      FROB (size_cursor, SM_CXCURSOR, SM_CYCURSOR);
      FROB (size_scrollbar, SM_CXVSCROLL, SM_CYHSCROLL);
      FROB (size_menu, -1, SM_CYMENU);
      FROB (size_icon, SM_CXICON, SM_CYICON);
      FROB (size_icon_small, SM_CXSMICON, SM_CYSMICON);
#undef FROB

    case DM_size_workspace:
      {
	RECT rc;
	mswindows_get_workspace_coords (&rc);
	return Fcons (make_int (rc.right - rc.left),
		      make_int (rc.bottom - rc.top));
      }

    case DM_offset_workspace:
      {
	RECT rc;
	mswindows_get_workspace_coords (&rc);
	return Fcons (make_int (rc.left), make_int (rc.top));
      }

      /*
	case DM_size_toolbar:
	case DM_size_toolbar_button:
	case DM_size_toolbar_border:
      */

      /*** Features ***/
#define FROB(met, index)			\
    case DM_##met:				\
      return make_int (GetSystemMetrics (index));

      FROB (mouse_buttons, SM_CMOUSEBUTTONS);
      FROB (swap_buttons, SM_SWAPBUTTON);
      FROB (show_sounds, SM_SHOWSOUNDS);
      FROB (slow_device, SM_SLOWMACHINE);
      FROB (security, SM_SECURE);
#undef FROB

    }

  /* Do not know such property */
  return Qunbound;
}


/************************************************************************/
/*                          printer helpers                             */
/************************************************************************/

static void
signal_open_printer_error (struct device *d)
{
  invalid_operation ("Failed to open printer", DEVICE_CONNECTION (d));
}


/* Helper function */
static int
msprinter_init_device_internal (struct device *d, Lisp_Object printer_name)
{
  Extbyte *printer_ext;
  HDC hdc;

  DEVICE_MSPRINTER_NAME (d) = printer_name;

  LISP_STRING_TO_TSTR (printer_name, printer_ext);

  if (!qxeOpenPrinter (printer_ext, &DEVICE_MSPRINTER_HPRINTER (d), NULL))
    {
      DEVICE_MSPRINTER_HPRINTER (d) = NULL;
      return 0;
    }

  DEVICE_MSPRINTER_HDC (d) = qxeCreateDC (XETEXT ("WINSPOOL"), printer_ext,
					  NULL, NULL);
  if (DEVICE_MSPRINTER_HDC (d) == NULL)
    return 0;

  hdc = CreateCompatibleDC (DEVICE_MSPRINTER_HDC (d));
  DEVICE_MSPRINTER_HCDC (d) = hdc;
  DEVICE_MSPRINTER_FONTLIST (d) = mswindows_enumerate_fonts (hdc);

  DEVICE_CLASS (d) = (GetDeviceCaps (DEVICE_MSPRINTER_HDC (d), BITSPIXEL)
		      * GetDeviceCaps (DEVICE_MSPRINTER_HDC (d), PLANES)
		      > 1) ? Qcolor : Qmono;
  return 1;
}

static void
msprinter_delete_device_internal (struct device *d)
{
  if (DEVICE_MSPRINTER_HPRINTER (d))
    ClosePrinter (DEVICE_MSPRINTER_HPRINTER (d));
  if (DEVICE_MSPRINTER_HDC (d))
    DeleteDC (DEVICE_MSPRINTER_HDC (d));
  if (DEVICE_MSPRINTER_HCDC (d))
    DeleteDC (DEVICE_MSPRINTER_HCDC (d));

  DEVICE_MSPRINTER_FONTLIST (d) = Qnil;
}

static int
msprinter_reinit_device (struct device *d, Lisp_Object devname)
{
  msprinter_delete_device_internal (d);
  return msprinter_init_device_internal (d, devname);
}

Lisp_Object
msprinter_default_printer (void)
{
  Extbyte name[666];
  Ibyte *nameint;

  if (qxeGetProfileString (XETEXT ("windows"), XETEXT ("device"), NULL, name,
			   sizeof (name) / XETCHAR_SIZE) <= 0)
    return Qnil;
  TSTR_TO_C_STRING (name, nameint);

  if (nameint[0] == '\0')
    return Qnil;

  /* this is destructive, but that's ok because the string is either in
     name[] or ALLOCA ()ed */
  qxestrtok (nameint, ",");

  return build_intstring (nameint);
}


/************************************************************************/
/*                          printer methods                             */
/************************************************************************/

static void
msprinter_init_device (struct device *d, Lisp_Object props)
{
  DEVMODEW *pdm;
  LONG dm_size;
  Extbyte *printer_name;

  d->device_data = xnew_and_zero (struct msprinter_device);

  DEVICE_INFD (d) = DEVICE_OUTFD (d) = -1;
  DEVICE_MSPRINTER_DEVMODE (d) = Qnil;
  DEVICE_MSPRINTER_NAME (d) = Qnil;

#if 0 /* @@#### deleted in new ikeyama ws */
  /* We do not use printer font list as we do with the display
     device.  Rather, we allow GDI to pick the closest match to the
     display font. */
  DEVICE_MSPRINTER_FONTLIST (d) = Qnil;
#endif /* 0 */

  CHECK_STRING (DEVICE_CONNECTION (d));

  if (!msprinter_init_device_internal (d, DEVICE_CONNECTION (d)))
    signal_open_printer_error (d);

  LISP_STRING_TO_TSTR (DEVICE_CONNECTION (d), printer_name);
  /* Determine DEVMODE size and store the default DEVMODE */
  dm_size = qxeDocumentProperties (NULL, DEVICE_MSPRINTER_HPRINTER (d),
				   printer_name, NULL, NULL, 0);
  if (dm_size <= 0)
    signal_open_printer_error (d);

  pdm = (DEVMODEW *) xmalloc (dm_size);
  if (qxeDocumentProperties (NULL, DEVICE_MSPRINTER_HPRINTER(d),
			     printer_name, pdm,
			     NULL, DM_OUT_BUFFER) < 0)
    signal_open_printer_error (d);

  assert (DEVMODE_SIZE (pdm) <= dm_size);

  DEVICE_MSPRINTER_DEVMODE (d) = 
    allocate_devmode (pdm, 0, DEVICE_CONNECTION (d), d);
}

static void
msprinter_delete_device (struct device *d)
{
  if (d->device_data)
    {
      msprinter_delete_device_internal (d);

      /* Disassociate the selected devmode with the device */
      if (!NILP (DEVICE_MSPRINTER_DEVMODE (d)))
	{
	  XDEVMODE (DEVICE_MSPRINTER_DEVMODE (d))->device = Qnil;
	  DEVICE_MSPRINTER_DEVMODE (d) = Qnil;
	}

      xfree (d->device_data);
    }
}

static Lisp_Object
msprinter_device_system_metrics (struct device *d,
				 enum device_metrics m)
{
  switch (m)
    {
      /* Device sizes - pixel and mm */
#define FROB(met, index1, index2)			\
    case DM_##met:					\
      return build_devicecaps_cons			\
         (DEVICE_MSPRINTER_HDC (d), index1, index2);

      FROB (size_device, PHYSICALWIDTH, PHYSICALHEIGHT);
      FROB (size_device_mm, HORZSIZE, VERTSIZE);
      FROB (size_workspace, HORZRES, VERTRES);
      FROB (offset_workspace, PHYSICALOFFSETX, PHYSICALOFFSETY);
      FROB (device_dpi, LOGPIXELSX, LOGPIXELSY);
#undef FROB

    case DM_num_bit_planes:
      /* this is what X means by bitplanes therefore we ought to be
         consistent. num planes is always 1 under mswindows and
         therefore useless */
      return make_int (GetDeviceCaps (DEVICE_MSPRINTER_HDC (d), BITSPIXEL));

    case DM_num_color_cells:	/* Printers are non-palette devices */
    case DM_slow_device:	/* Animation would be a really bad idea */
    case DM_security:		/* Not provided by windows */
      return Qzero;
    }

  /* Do not know such property */
  return Qunbound;
}

static void
msprinter_mark_device (struct device *d)
{
  mark_object (DEVICE_MSPRINTER_FONTLIST (d));
  mark_object (DEVICE_MSPRINTER_DEVMODE (d));
  mark_object (DEVICE_MSPRINTER_NAME (d));
}


/************************************************************************/
/*                      printer Lisp subroutines                        */
/************************************************************************/

static void
global_free_2_maybe (HGLOBAL hg1, HGLOBAL hg2)
{
  if (hg1 != NULL)
    GlobalFree (hg1);
  if (hg2 != NULL)
    GlobalFree (hg2);
}

static HGLOBAL
devmode_to_hglobal (Lisp_Devmode *ldm)
{
  HGLOBAL hg = GlobalAlloc (GHND, XDEVMODE_SIZE (ldm));
  memcpy (GlobalLock (hg), ldm->devmode, XDEVMODE_SIZE (ldm));
  GlobalUnlock (hg);
  return hg;
}

/* Returns 0 if the printer has been deleted due to a fatal I/O error,
   1 otherwise. */
static int
sync_printer_with_devmode (struct device* d, DEVMODEW* devmode_in,
			   DEVMODEW* devmode_out, Lisp_Object devname)
{
  /* Change connection if the device changed */
  if (!NILP (devname)
      && lisp_strcasecmp (devname, DEVICE_MSPRINTER_NAME (d)) != 0)
    {
      Lisp_Object new_connection = devname;

      DEVICE_CONNECTION (d) = Qnil;
      if (!NILP (Ffind_device (new_connection, Qmsprinter)))
	{
	  /* We are in trouble - second msprinter for the same device.
	     Nothing wrong on the Windows side, just forge a unique
	     connection name. Use the memory address of d as a unique
	     suffix. */
	  Ibyte new_connext[20];

	  qxesprintf (new_connext, ":%X", d->header.uid);
	  new_connection = concat2 (devname, build_intstring (new_connext));
	}
      DEVICE_CONNECTION (d) = new_connection;

      /* Reinitialize printer. The device can pop off in process */
      if (!msprinter_reinit_device (d, devname))
	{
	  /* Kaboom! */
	  delete_device_internal (d, 1, 0, 1);
	  return 0;
	}
    }
  {
    Extbyte *nameext;

    LISP_STRING_TO_TSTR (DEVICE_MSPRINTER_NAME (d), nameext);

    /* Apply the new devmode to the printer */
    qxeDocumentProperties (NULL, DEVICE_MSPRINTER_HPRINTER (d),
			   nameext, devmode_out, devmode_in,
			   DM_IN_BUFFER | DM_OUT_BUFFER);

    /* #### ResetDC fails sometimes, Bill only knows why.
       The solution below looks more like a workaround to me,
       although it might be fine. --kkm */
    if (qxeResetDC (DEVICE_MSPRINTER_HDC (d), devmode_out) == NULL)
      {
	DeleteDC (DEVICE_MSPRINTER_HDC (d));
	DEVICE_MSPRINTER_HDC (d) =
	  qxeCreateDC (XETEXT ("WINSPOOL"), nameext, NULL,
		       devmode_out);
      }
  }
 
  return 1;
}

static void
handle_devmode_changes (Lisp_Devmode *ldm, HGLOBAL hDevNames, HGLOBAL hDevMode)
{
  DEVNAMES *devnames = (DEVNAMES *) GlobalLock (hDevNames);
  Extbyte *new_name =
    devnames ?
    (Extbyte *) devnames + XETCHAR_SIZE * devnames->wDeviceOffset : NULL;
  DEVMODEW *devmode = (DEVMODEW *) GlobalLock (hDevMode);

  /* Size and name may have changed */
  ldm->devmode = (DEVMODEW *) xrealloc (ldm->devmode, DEVMODE_SIZE (devmode));
  if (new_name)
    ldm->printer_name = build_tstr_string (new_name);

  if (!NILP (ldm->device))
    {
      /* Apply the new devmode to the printer and get a compete one back */
      struct device *d = XDEVICE (ldm->device);
      if (!sync_printer_with_devmode (d, devmode, ldm->devmode,
				      new_name ? ldm->printer_name : Qnil))
	{
	  global_free_2_maybe (hDevNames, hDevMode);
	  signal_error
	    (Qio_error,
	     "Printer device initialization I/O error, device deleted",
	     ldm->device);
	}
    }
  else
    {
      /* Just copy the devmode structure */
      memcpy (ldm->devmode, devmode, DEVMODE_SIZE (devmode));
    }
}

static void
ensure_not_printing (struct device *d)
{
  if (!NILP (DEVICE_FRAME_LIST (d)))
  {
    Lisp_Object device = wrap_device (d);

    invalid_operation ("Cannot change settings while print job is active",
		       device);
  }
}

static Lisp_Devmode *
decode_devmode (Lisp_Object dev)
{
  if (DEVMODEP (dev))
    return XDEVMODE (dev);
  else
    {
      ensure_not_printing (XDEVICE (dev));
      return XDEVMODE (DEVICE_MSPRINTER_DEVMODE (XDEVICE (dev)));
    }
}

/*
 * DEV can be either a printer or devmode
 */
static Lisp_Object
print_dialog_worker (Lisp_Object dev, DWORD flags)
{
  Lisp_Devmode *ldm = decode_devmode (dev);
  PRINTDLGW pd;

  memset (&pd, 0, sizeof (pd));
  pd.lStructSize = sizeof (pd);
  pd.hwndOwner = mswindows_get_selected_frame_hwnd ();
  pd.hDevMode = devmode_to_hglobal (ldm);
  pd.Flags = flags | PD_USEDEVMODECOPIESANDCOLLATE;
  pd.nMinPage = 0;
  pd.nMaxPage = 0xFFFF;

  if (!qxePrintDlg (&pd))
    {
      global_free_2_maybe (pd.hDevNames, pd.hDevMode);
      return Qnil;
    }

  handle_devmode_changes (ldm, pd.hDevNames, pd.hDevMode);

  /* Finally, build the resulting plist */
  {
    Lisp_Object result = Qnil;
    struct gcpro gcpro1;
    GCPRO1 (result);

    /* Do consing in reverse order.
       Number of copies */
    result = Fcons (Qcopies, Fcons (make_int (pd.nCopies), result));

    /* Page range */
    if (pd.Flags & PD_PAGENUMS)
      {
	result = Fcons (Qto_page, Fcons (make_int (pd.nToPage), result));
	result = Fcons (Qfrom_page, Fcons (make_int (pd.nFromPage), result));
	result = Fcons (Qselected_page_button, Fcons (Qpages, result));
      }
    else if (pd.Flags & PD_SELECTION)
      result = Fcons (Qselected_page_button, Fcons (Qselection, result));
    else
      result = Fcons (Qselected_page_button, Fcons (Qall, result));

    /* Device name */
    result = Fcons (Qname, Fcons (ldm->printer_name, result));
    UNGCPRO;

    global_free_2_maybe (pd.hDevNames, pd.hDevMode);
    return result;
  }
}

Lisp_Object
mswindows_handle_print_dialog_box (struct frame *f, Lisp_Object keys)
{
  Lisp_Object device = Qunbound, settings = Qunbound;
  DWORD flags = PD_NOSELECTION;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_device))
	  {
	    device = wrap_device (decode_device (value));
	    CHECK_MSPRINTER_DEVICE (device);
	  }
	else if (EQ (key, Q_printer_settings))
	  {
	    CHECK_DEVMODE (value);
	    settings = value;
	  }
	else if (EQ (key, Q_allow_pages))
	  {
	    if (NILP (value))
	      flags |= PD_NOPAGENUMS;
	  }
	else if (EQ (key, Q_allow_selection))
	  {
	    if (!NILP (value))
	      flags &= ~PD_NOSELECTION;
	  }
	else if (EQ (key, Q_selected_page_button))
	  {
	    if (EQ (value, Qselection))
	      flags |= PD_SELECTION;
	    else if (EQ (value, Qpages))
	      flags |= PD_PAGENUMS;
	    else if (!EQ (value, Qall))
	      invalid_constant ("for :selected-page-button", value);
	  }
	else
	  invalid_constant ("Unrecognized print-dialog keyword", key);
      }
  }

  if ((UNBOUNDP (device) && UNBOUNDP (settings)) ||
      (!UNBOUNDP (device) && !UNBOUNDP (settings)))
    sferror ("Exactly one of :device and :printer-settings must be given",
		  keys);

  return print_dialog_worker (!UNBOUNDP (device) ? device : settings, flags);
}

int
mswindows_get_default_margin (Lisp_Object prop)
{
  if (EQ (prop, Qleft_margin)) return 1440;
  if (EQ (prop, Qright_margin)) return 1440;
  if (EQ (prop, Qtop_margin)) return 720;
  if (EQ (prop, Qbottom_margin)) return 720;
  abort ();
  return 0;
}

static int
plist_get_margin (Lisp_Object plist, Lisp_Object prop, int mm_p)
{
  Lisp_Object val =
    Fplist_get (plist, prop, make_int (mswindows_get_default_margin (prop)));
  if (!INTP (val))
    invalid_argument ("Margin value must be an integer", val);

  return MulDiv (XINT (val), mm_p ? 254 : 100, 144);
}

static Lisp_Object
plist_set_margin (Lisp_Object plist, Lisp_Object prop, int margin, int mm_p)
{
  Lisp_Object val = make_int (MulDiv (margin, 144, mm_p ? 254 : 100));
  return Fcons (prop, Fcons (val, plist));
}

Lisp_Object
mswindows_handle_page_setup_dialog_box (struct frame *f, Lisp_Object keys)
{
  Lisp_Object device = Qunbound, settings = Qunbound;
  Lisp_Object plist = Qnil;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_device))
	  {
	    device = wrap_device (decode_device (value));
	    CHECK_MSPRINTER_DEVICE (device);
	  }
	else if (EQ (key, Q_printer_settings))
	  {
	    CHECK_DEVMODE (value);
	    settings = value;
	  }
	else if (EQ (key, Q_properties))
	  {
	    CHECK_LIST (value);
	    plist = value;
	  }
	else
	  invalid_constant ("Unrecognized page-setup dialog keyword", key);
      }
  }

  if ((UNBOUNDP (device) && UNBOUNDP (settings)) ||
      (!UNBOUNDP (device) && !UNBOUNDP (settings)))
    sferror ("Exactly one of :device and :printer-settings must be given",
	     keys);

  if (UNBOUNDP (device))
    device = settings;

  {
    Lisp_Devmode *ldm = decode_devmode (device);
    PAGESETUPDLGW pd;
    Extbyte measure[2 * MAX_XETCHAR_SIZE];
    int data;

    qxeGetLocaleInfo (LOCALE_USER_DEFAULT, LOCALE_IMEASURE,
		      measure, sizeof (measure) / XETCHAR_SIZE);
    data = xetcscmp (measure, XETEXT ("0"));

    memset (&pd, 0, sizeof (pd));
    pd.lStructSize = sizeof (pd);
    pd.hwndOwner = mswindows_get_selected_frame_hwnd ();
    pd.Flags = PSD_MARGINS;
    pd.rtMargin.left   = plist_get_margin (plist, Qleft_margin, !data);
    pd.rtMargin.top    = plist_get_margin (plist, Qtop_margin, !data);
    pd.rtMargin.right  = plist_get_margin (plist, Qright_margin, !data);
    pd.rtMargin.bottom = plist_get_margin (plist, Qbottom_margin, !data);
    pd.hDevMode = devmode_to_hglobal (ldm);

    if (!qxePageSetupDlg (&pd))
      {
	global_free_2_maybe (pd.hDevNames, pd.hDevMode);
	return Qnil;
      }

    if (pd.hDevMode)
      handle_devmode_changes (ldm, pd.hDevNames, pd.hDevMode);

    /* Finally, build the resulting plist */
    {
      Lisp_Object result = Qnil;
      int mm_p = pd.Flags & PSD_INHUNDREDTHSOFMILLIMETERS;
      result = plist_set_margin (result, Qbottom_margin, pd.rtMargin.bottom,
				 mm_p);
      result = plist_set_margin (result, Qright_margin, pd.rtMargin.right,
				 mm_p);
      result = plist_set_margin (result, Qtop_margin, pd.rtMargin.top, mm_p);
      result = plist_set_margin (result, Qleft_margin, pd.rtMargin.left, mm_p);
      return result;
    }
  }
}

DEFUN ("msprinter-get-settings", Fmsprinter_get_settings, 1, 1, 0, /*
Return the settings object currently used by DEVICE.
The object returned is not a copy, but rather a pointer to the
original one. Use `msprinter-settings-copy' to create a copy of it.
*/
	(device))
{
  struct device *d = decode_device (device);
  device = wrap_device (d);
  CHECK_MSPRINTER_DEVICE (device);
  return DEVICE_MSPRINTER_DEVMODE (d);
}

DEFUN ("msprinter-select-settings", Fmsprinter_select_settings, 2, 2, 0, /*
Select SETTINGS object into a DEVICE.
The settings from the settings object are immediately applied to the
printer, possibly changing even the target printer itself, and all
future changes are applied synchronously to the printer device and the
selected printer object, until a different settings object is selected
into the same printer.

A settings object can be selected to no more than one printer at a time.

If the supplied settings object is not specialized, it is specialized
for the printer immediately upon selection. The object can be
despecialized after it is unselected by calling the function
`msprinter-settings-despecialize'.

Return value is the previously selected settings object.
*/
	(device, settings))
{
  Lisp_Devmode *ldm;
  struct device *d = decode_device (device);

  struct gcpro gcpro1;
  GCPRO1 (settings);

  device = wrap_device (d);
  CHECK_MSPRINTER_DEVICE (device);
  CHECK_DEVMODE (settings);
  ldm = XDEVMODE (settings);

  if (!NILP (ldm->device))
    invalid_operation ("The object is currently selected into a device",
		       settings);

  /* If the object being selected is de-specialized, then its
     size is perhaps not enough to receive the new devmode. We can ask
     for printer's devmode size here, because despecialized settings
     cannot force switching to a different printer, as they supply no
     printer name at all. */
  if (NILP (ldm->printer_name))
    {
      Extbyte *nameext;
      LONG dm_size;

      LISP_STRING_TO_TSTR (DEVICE_MSPRINTER_NAME (d), nameext);
      dm_size = qxeDocumentProperties (NULL, DEVICE_MSPRINTER_HPRINTER (d),
				       nameext, NULL, NULL, 0);
      if (dm_size <= 0)
	signal_error (Qio_error,
		      "Unable to specialize settings, printer error",
		      device);

      assert (XDEVMODE_SIZE (ldm) <= dm_size);
      ldm->devmode = (DEVMODEW *) xrealloc (ldm->devmode, dm_size);
    }

  /* If we bail out on signal here, no damage is done, except that
     the storage for the DEVMODE structure might be reallocated to
     hold a larger one - not a big deal */
  if (!sync_printer_with_devmode (d, ldm->devmode, ldm->devmode,
				  ldm->printer_name))
    signal_error (Qio_error,
		  "Printer device initialization I/O error, device deleted",
		  device);

  if (NILP (ldm->printer_name ))
    ldm->printer_name = DEVICE_MSPRINTER_NAME (d);

  {
    Lisp_Object old_mode = DEVICE_MSPRINTER_DEVMODE (d);
    ldm->device = device;
    XDEVMODE (old_mode)->device = Qnil;
    DEVICE_MSPRINTER_DEVMODE (d) = settings;
    UNGCPRO;
    return old_mode;
  }
}

DEFUN ("msprinter-apply-settings", Fmsprinter_apply_settings, 2, 2, 0, /*
Apply settings from a SETTINGS object to a 'msprinter DEVICE.
The settings from the settings object are immediately applied to the
printer, possibly changing even the target printer itself. The SETTING
object is not modified, unlike `msprinter-select-settings', and the
supplied object is not changed.  The changes are immediately recorded
into the settings object which is currently selected into the printer
device.

Return value is the currently selected settings object.
*/
	(device, settings))
{
  Lisp_Devmode *ldm_current, *ldm_new;
  struct device *d = decode_device (device);

  struct gcpro gcpro1;
  GCPRO1 (settings);

  device = wrap_device (d);
  CHECK_MSPRINTER_DEVICE (device);
  CHECK_DEVMODE (settings);
  ldm_new = XDEVMODE (settings);
  ldm_current = XDEVMODE (DEVICE_MSPRINTER_DEVMODE (d));

  /* If the supplied devmode is not specialized, then the current
     devmode size will always be sufficient, as the printer does
     not change.  If it is specialized, we must reallocate the current
     devmode storage to match with the supplied one, as it has the right
     size for the new printer, if it is going to change.  The correct
     way is to use the largest of the two though, to keep the old
     contents unchanged in case of preliminary exit.
  */
  if (!NILP (ldm_new->printer_name))
    ldm_current->devmode =
      (DEVMODEW*) xrealloc (ldm_current->devmode,
			   max (XDEVMODE_SIZE (ldm_new),
				XDEVMODE_SIZE (ldm_current)));

  if (!sync_printer_with_devmode (d, ldm_new->devmode,
				  ldm_current->devmode,
				  ldm_new->printer_name))
    signal_error
      (Qio_error,
       "Printer device initialization I/O error, device deleted", device);
  
  if (!NILP (ldm_new->printer_name))
    ldm_current->printer_name = ldm_new->printer_name;

  UNGCPRO;
  return DEVICE_MSPRINTER_DEVMODE (d);
}

/************************************************************************/
/*                                devmode                               */
/************************************************************************/

static const struct memory_description devmode_description[] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Devmode, printer_name) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Devmode, device) },
  { XD_END }
};

static Lisp_Object
mark_devmode (Lisp_Object obj)
{
  Lisp_Devmode *data = XDEVMODE (obj);
  mark_object (data->printer_name);
  return data->device;
}

static void
print_devmode (Lisp_Object obj, Lisp_Object printcharfun,
	       int escapeflag)
{
  Lisp_Devmode *dm = XDEVMODE (obj);
  if (print_readably)
    printing_unreadable_object ("#<msprinter-settings 0x%x>",
				dm->header.uid);
  write_c_string (printcharfun, "#<msprinter-settings");
  if (!NILP (dm->printer_name))
    write_fmt_string_lisp (printcharfun, " for %S", 1, dm->printer_name);
  if (!NILP (dm->device))
    write_fmt_string_lisp (printcharfun, " (currently on %s)", 1, dm->device);
  write_fmt_string (printcharfun, " 0x%x>", dm->header.uid);
}

static void
finalize_devmode (void *header, int for_disksave)
{
  Lisp_Devmode *dm = (Lisp_Devmode *) header;

  if (for_disksave)
    {
      Lisp_Object devmode = wrap_devmode (dm);

      invalid_operation
	("Cannot dump XEmacs containing an msprinter-settings object",
	 devmode);
    }

  assert (NILP (dm->device));
}

static int
equal_devmode (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Devmode *dm1 = XDEVMODE (obj1);
  Lisp_Devmode *dm2 = XDEVMODE (obj2);

  if ((dm1->devmode != NULL) != (dm1->devmode != NULL))
    return 0;
  if (dm1->devmode == NULL)
    return 1;
  if (memcmp (dm1->devmode, dm2->devmode, XDEVMODE_SIZE (dm1)) != 0)
    return 0;
  if (NILP (dm1->printer_name) || NILP (dm2->printer_name))
    return 1;
  return lisp_strcasecmp (dm1->printer_name, dm2->printer_name) == 0;
}

static Hashcode
hash_devmode (Lisp_Object obj, int depth)
{
  Lisp_Devmode *dm = XDEVMODE (obj);

  return HASH3 (XDEVMODE_SIZE (dm),
		dm->devmode ? memory_hash (dm->devmode, XDEVMODE_SIZE (dm))
		: 0,
		internal_hash (dm->printer_name, depth + 1));
}

DEFINE_LRECORD_IMPLEMENTATION ("msprinter-settings", devmode,
			       0, /*dumpable-flag*/
			       mark_devmode, print_devmode, finalize_devmode,
			       equal_devmode, hash_devmode, 
			       devmode_description,
			       Lisp_Devmode);

static Lisp_Object
allocate_devmode (DEVMODEW* src_devmode, int do_copy,
		  Lisp_Object src_name, struct device *d)
{
  Lisp_Devmode *dm;

  dm = alloc_lcrecord_type (Lisp_Devmode, &lrecord_devmode);

  if (d)
    dm->device = wrap_device (d);
  else
    dm->device = Qnil;

  dm->printer_name = src_name;

  if (src_devmode != NULL && do_copy)
    {
      dm->devmode = (DEVMODEW*) xmalloc (DEVMODE_SIZE (src_devmode));
      memcpy (dm->devmode, src_devmode, DEVMODE_SIZE (src_devmode));
    }
  else
    {
      dm->devmode = src_devmode;
    }

  return wrap_devmode (dm);
}

DEFUN ("msprinter-settings-copy", Fmsprinter_settings_copy, 1, 1, 0, /*
Create and returns an exact copy of a printer settings object.
*/
       (settings))
{
  Lisp_Devmode *dm;

  CHECK_DEVMODE (settings);
  dm = XDEVMODE (settings);

  return allocate_devmode (dm->devmode, 1, dm->printer_name, NULL);
}

DEFUN ("msprinter-settings-despecialize", Fmsprinter_settings_despecialize, 1, 1, 0, /*
Erase printer-specific settings from a printer settings object.
*/
       (settings))
{
  Lisp_Devmode *ldm;
  DEVMODEW *dm;

  CHECK_DEVMODE (settings);
  ldm = XDEVMODE (settings);

  if (!NILP (ldm->device))
    invalid_operation ("The object is currently selected into a device",
		       settings);

  dm = ldm->devmode;

  /* #### TODO. Either remove references to device specific bins,
     paper sizes etc, or signal an error of they are present. */

  dm->dmDriverExtra = 0;
  dm->dmDeviceName[0] = '\0';

  ldm->printer_name = Qnil;

  return Qnil;
}

DEFUN ("mswindows-get-default-printer", Fmswindows_get_default_printer, 0, 0, 0, /*
Return name of the default printer, as string, on nil if there is no default.
*/
       ())
{
  return msprinter_default_printer ();
}

static void
signal_enum_printer_error (void)
{
  invalid_operation ("Error enumerating printers", make_int (GetLastError ()));
}

DEFUN ("mswindows-printer-list", Fmswindows_printer_list, 0, 0, 0, /*
Return a list of string names of installed printers.
If there is a default printer, it is returned as the first element of
the list.  If there is no default printer, the first element of the
list will be nil.  The rest of elements are guaranteed to have string
values.  Return value is nil if there are no printers installed.
*/
       ())
{
  int have_nt, ok;
  BYTE *data_buf, dummy_byte;
  Bytecount enum_entry_size;
  DWORD enum_flags, enum_level, bytes_needed, num_printers;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object result = Qnil, def_printer = Qnil;

  /* Determine OS flavor, to use the fastest enumeration method available */
  have_nt = !mswindows_windows9x_p;
  enum_flags = PRINTER_ENUM_LOCAL | (have_nt ? PRINTER_ENUM_CONNECTIONS : 0);
  enum_level = have_nt ? 4 : 5;
  enum_entry_size = (have_nt ? sizeof (PRINTER_INFO_4) :
		     sizeof (PRINTER_INFO_5));

  /* Allocate memory for printer enum structure */
  ok = qxeEnumPrinters (enum_flags, NULL, enum_level, &dummy_byte, 1,
			&bytes_needed, &num_printers);
  if (ok)
    /* No printers, if just 1 byte is enough */
    return Qnil;

  if (GetLastError () != ERROR_INSUFFICIENT_BUFFER)
    signal_enum_printer_error ();

  data_buf = (BYTE *) ALLOCA (bytes_needed);
  ok = qxeEnumPrinters (enum_flags, NULL, enum_level, data_buf, bytes_needed,
			&bytes_needed, &num_printers);
  if (!ok)
    signal_enum_printer_error ();

  if (num_printers == 0)
    /* Strange but... */
    return Qnil;

  GCPRO2 (result, def_printer);

  while (num_printers--)
    {
      Extbyte *printer_name;
      if (have_nt)
	{
	  PRINTER_INFO_4 *info = (PRINTER_INFO_4 *) data_buf;
	  printer_name = (Extbyte *) info->pPrinterName;
	}
      else
	{
	  PRINTER_INFO_5 *info = (PRINTER_INFO_5 *) data_buf;
	  printer_name = (Extbyte *) info->pPrinterName;
	}
      data_buf += enum_entry_size;

      result = Fcons (build_tstr_string (printer_name), result);
    }

  def_printer = msprinter_default_printer ();
  result = Fdelete (def_printer, result);
  result = Fcons (def_printer, result);

  RETURN_UNGCPRO (result);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_mswindows (void)
{
  INIT_LRECORD_IMPLEMENTATION (devmode);

  DEFSUBR (Fmsprinter_get_settings);
  DEFSUBR (Fmsprinter_select_settings);
  DEFSUBR (Fmsprinter_apply_settings);
  DEFSUBR (Fmsprinter_settings_copy);
  DEFSUBR (Fmsprinter_settings_despecialize);
  DEFSUBR (Fmswindows_get_default_printer);
  DEFSUBR (Fmswindows_printer_list);

  DEFKEYWORD (Q_allow_selection);
  DEFKEYWORD (Q_allow_pages);
  DEFKEYWORD (Q_selected_page_button);
  DEFSYMBOL (Qselected_page_button);

  DEFSYMBOL (Qinit_pre_mswindows_win);
  DEFSYMBOL (Qinit_post_mswindows_win);
}

void
console_type_create_device_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, init_device);
  CONSOLE_HAS_METHOD (mswindows, finish_init_device);
  CONSOLE_HAS_METHOD (mswindows, mark_device);
  CONSOLE_HAS_METHOD (mswindows, delete_device);
  CONSOLE_HAS_METHOD (mswindows, device_system_metrics);
  CONSOLE_IMPLEMENTATION_FLAGS (mswindows, XDEVIMPF_PIXEL_GEOMETRY);

  CONSOLE_HAS_METHOD (msprinter, init_device);
  CONSOLE_HAS_METHOD (msprinter, mark_device);
  CONSOLE_HAS_METHOD (msprinter, delete_device);
  CONSOLE_HAS_METHOD (msprinter, device_system_metrics);
  CONSOLE_IMPLEMENTATION_FLAGS (msprinter, (XDEVIMPF_PIXEL_GEOMETRY
					    | XDEVIMPF_IS_A_PRINTER
					    | XDEVIMPF_NO_AUTO_REDISPLAY
					    | XDEVIMPF_DONT_PREEMPT_REDISPLAY
					    | XDEVIMPF_FRAMELESS_OK));
}


void
vars_of_device_mswindows (void)
{
}
