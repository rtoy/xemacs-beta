/* Console functions for mswindows.
   Copyright (C) 1996, 2000, 2001, 2002 Ben Wing.

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

   Ben Wing: January 1996, for 19.14.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0
 */

/* See win32.c for info about the different Windows files in XEmacs. */

#include <config.h>
#include "lisp.h"

#include "console-msw-impl.h"
#include "events.h"
#include "opaque.h"
#include "elhash.h"

DEFINE_CONSOLE_TYPE (mswindows);
DEFINE_CONSOLE_TYPE (msprinter);

Lisp_Object Qabortretryignore;
Lisp_Object Qapplmodal;
Lisp_Object Qdefault_desktop_only;
Lisp_Object Qdefbutton1;
Lisp_Object Qdefbutton2;
Lisp_Object Qdefbutton3;
Lisp_Object Qdefbutton4;
/* Lisp_Object Qhelp; */
Lisp_Object Qiconasterisk;
Lisp_Object Qiconexclamation;
Lisp_Object Qiconhand;
Lisp_Object Qiconinformation;
Lisp_Object Qiconquestion;
Lisp_Object Qiconstop;
/* Lisp_Object Qok; */
Lisp_Object Qokcancel;
Lisp_Object Qretrycancel;
/* Lisp_Object Qright; */
Lisp_Object Qrtlreading;
Lisp_Object Qservice_notification;
Lisp_Object Qsetforeground;
Lisp_Object Qsystemmodal;
Lisp_Object Qtaskmodal;
Lisp_Object Qtopmost;
Lisp_Object Qyesno;
Lisp_Object Qyesnocancel;
Lisp_Object Vmswindows_seen_characters;

/* Lisp_Object Qabort; */
/* Lisp_Object Qcancel; */
/* Lisp_Object Qignore; */
/* Lisp_Object Qno; */
/* Lisp_Object Qok; */
/* Lisp_Object Qretry; */
/* Lisp_Object Qyes; */


/************************************************************************/
/*                       mswindows console methods                      */
/************************************************************************/

static int
mswindows_initially_selected_for_input (struct console *UNUSED (con))
{
  return 1;
}

static HWND mswindows_console_hwnd = 0;

/* Based on Microsoft KB article Q124103 */
static HWND
GetConsoleHwnd (void)
{ 
  HWND hwndFound;
  Ascbyte newtitle[100];
  Extbyte *oldtitle;
  int numchars;

  /* fetch current window title */

  {
    int size = 64;
    do
      {
	size *= 2;
	oldtitle = alloca_extbytes (size * XETCHAR_SIZE);
	numchars = qxeGetConsoleTitle (oldtitle, size);
      }
    while (numchars >= size - 1);
  }

  /* format a "unique" new title */

  sprintf (newtitle, "%ld/%ld", GetTickCount (), GetCurrentProcessId ());

  /* change current window title; we may be called during armageddon
     so don't do any conversion */

  SetConsoleTitleA (newtitle);

  /* ensure window title has been updated */

  Sleep (40);

  /* look for NewWindowTitle */

  hwndFound = FindWindowA (NULL, newtitle);

  /* restore original window title */

  qxeSetConsoleTitle (oldtitle);

  return hwndFound;
} 

static HWND
mswindows_get_console_hwnd (void)
{
  if (!mswindows_console_hwnd)
    mswindows_console_hwnd = GetConsoleHwnd ();
  return mswindows_console_hwnd;
}

static int
mswindows_ensure_console_allocated (void)
{
  HWND fgwin = GetForegroundWindow ();
  /* stupid mswin api won't let you create the console window
     hidden!  creating it changes the focus!  fuck me! */
  if (AllocConsole ())
    {
      SetForegroundWindow (fgwin);
      return 1;
    }
  return 0;
}

static Lisp_Object
mswindows_canonicalize_console_connection (Lisp_Object connection,
					   Error_Behavior errb)
{
  /* Do not allow more than one mswindows device, by explicitly
     requiring that CONNECTION is nil, the only allowed connection in
     Windows. */
  if (!NILP (connection))
    {
      if (ERRB_EQ (errb, ERROR_ME))
	invalid_argument
	  ("Invalid (non-nil) connection for mswindows device/console",
	   connection);
      else
	return Qunbound;
    }

  return Qnil;
}

static Lisp_Object
mswindows_canonicalize_device_connection (Lisp_Object connection,
					  Error_Behavior errb)
{
  return mswindows_canonicalize_console_connection (connection, errb);
}

/* The actual console doesn't matter, because the global map is global. See
   console-x.c for a corner case, though. */

static Lisp_Object
mswindows_perhaps_init_unseen_key_defaults (struct console *UNUSED(con),
					    Lisp_Object key)
{
  Ichar val;
  extern Lisp_Object Vcurrent_global_map;

  if (SYMBOLP(key))
    {
      /* We've no idea what to default a symbol to on MS Windows, and most
	 of the keys I'm aware of that have
	 symbols--cf. event-msw.c--_shouldn't_ have associated chars. */
      return Qnil;
    }

  CHECK_CHAR(key);

  if (!(HASH_TABLEP(Vmswindows_seen_characters)))
    {
      /* All the keysym we deal with are character objects; therefore, we
	 can use eq as the test without worrying. */
      Vmswindows_seen_characters = make_lisp_hash_table (128,
							 HASH_TABLE_NON_WEAK,
							 Qeq);
    }
  /* Might give the user an opaque error if make_lisp_hash_table fails,
     but it shouldn't crash. */
  CHECK_HASH_TABLE(Vmswindows_seen_characters);

  val = XCHAR(key);

  /* Same logic as in x_has_keysym; I'm not convinced it's sane. */
  if (val < 0x80) 
    {
      return Qnil; 
    }

  if (!NILP(Fgethash(key, Vmswindows_seen_characters, Qnil)))
    {
      return Qnil;
    }

  if (NILP (Flookup_key (Vcurrent_global_map, key, Qnil))) 
    {
      Fputhash(key, Qt, Vmswindows_seen_characters);
      Fdefine_key (Vcurrent_global_map, key, Qself_insert_command); 
      return Qt; 
    }

  return Qnil;
}

void
mswindows_hide_console (void)
{
  ShowWindow (mswindows_get_console_hwnd (), SW_HIDE);
}

static void
mswindows_show_console (void)
{
  /* What I really want is for the console window to appear on top of other
     windows, but NOT get the focus.  This seems hard-to-impossible under
     Windows.  The following sequence seems to do the best possible, along
     with keeping the console window on top when xemacs --help is used. */
  HWND hwnd = mswindows_get_console_hwnd ();
  HWND hwndf = GetFocus ();
  if (!IsWindowVisible (hwnd))
    ShowWindow (hwnd, SW_SHOWNA);
  if (noninteractive)
    BringWindowToTop (hwnd);
  else
    SetWindowPos (hwnd, hwndf, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE
                  | SWP_NOACTIVATE);
}

static int mswindows_console_buffered = 0;
HANDLE mswindows_console_buffer;

static void
mswindows_ensure_console_buffered (void)
{
  if (!mswindows_console_buffered)
    {
      COORD new_size;

      new_size.X = 80;
      new_size.Y = 1000;
      mswindows_ensure_console_allocated ();
      mswindows_console_buffer =
	CreateConsoleScreenBuffer (GENERIC_WRITE, 0, NULL,
				   CONSOLE_TEXTMODE_BUFFER, NULL);
      SetConsoleScreenBufferSize (mswindows_console_buffer, new_size);
      SetConsoleActiveScreenBuffer (mswindows_console_buffer);
      mswindows_console_buffered = 1;
    }
}

int mswindows_message_outputted;

int
mswindows_output_console_string (const Ibyte *ptr, Bytecount len)
{
  DWORD num_written;

  mswindows_message_outputted = 1;
  mswindows_ensure_console_buffered ();
  mswindows_show_console ();

  if (initialized && !inhibit_non_essential_conversion_operations)
    {
      const Extbyte *extptr;
      Bytecount extlen;
      TO_EXTERNAL_FORMAT (DATA, (ptr, len),
			  ALLOCA, (extptr, extlen),
			  Qmswindows_tstr);
      return qxeWriteConsole (mswindows_console_buffer, extptr,
			      extlen / XETCHAR_SIZE, &num_written, NULL);
    }
  else
#ifdef NON_ASCII_INTERNAL_FORMAT
#error Do something here
#endif
    return WriteConsoleA (mswindows_console_buffer, (Chbyte *) ptr, len,
			  &num_written, NULL);
}

DEFUN ("mswindows-debugging-output", Fmswindows_debugging_output, 1, 1, 0, /*
Write CHAR-OR-STRING to the Windows debugger, using OutputDebugString().
This function can be used as the STREAM argument of Fprint() or the like.
*/
       (char_or_string))
{
  if (STRINGP (char_or_string))
    /* It's safe to pass in string data because TO_EXTERNAL_FORMAT
       inhibits GC. */
    write_string_to_mswindows_debugging_output
      (XSTRING_DATA (char_or_string), XSTRING_LENGTH (char_or_string));
  else
    {
      Ibyte str[MAX_ICHAR_LEN];
      Bytecount len;

      CHECK_CHAR_COERCE_INT (char_or_string);
      len = set_itext_ichar (str, XCHAR (char_or_string));
      write_string_to_mswindows_debugging_output (str, len);
    }

  return char_or_string;
}

void
write_string_to_mswindows_debugging_output (const Ibyte *str, Bytecount len)
{
  const Extbyte *extptr;
  if (initialized && !inhibit_non_essential_conversion_operations)
    {
      TO_EXTERNAL_FORMAT (DATA, (str, len),
			  C_STRING_ALLOCA, extptr, Qmswindows_tstr);
      qxeOutputDebugString (extptr);
    }
  else
    {
#ifdef NON_ASCII_INTERNAL_FORMAT
#error Do something here
#endif
      /* STR may not be null-terminated so make it that way. */
      Extbyte *ext = alloca_extbytes (len + 1);
      memcpy (ext, str, len);
      ext[len] = '\0';
      OutputDebugStringA (ext);
    }
}

#ifdef DEBUG_XEMACS

/*
 * Random helper functions for debugging.
 * Intended for use in the MSVC "Watch" window which doesn't like
 * the aborts that the error_check_foo() functions can make.
 */
struct lrecord_header *DHEADER (Lisp_Object obj);
struct lrecord_header *
DHEADER (Lisp_Object obj)
{
  return LRECORDP (obj) ? XRECORD_LHEADER (obj) : NULL;
}

void *DOPAQUE_DATA (Lisp_Object obj);
void *
DOPAQUE_DATA (Lisp_Object obj)
{
  return OPAQUEP (obj) ? OPAQUE_DATA (XOPAQUE (obj)) : NULL;
}

Lisp_Event *DEVENT (Lisp_Object obj);
Lisp_Event *
DEVENT (Lisp_Object obj)
{
  return EVENTP (obj) ? XEVENT (obj) : NULL;
}

Lisp_Cons *DCONS (Lisp_Object obj);
Lisp_Cons *
DCONS (Lisp_Object obj)
{
  return CONSP (obj) ? XCONS (obj) : NULL;
}

Lisp_Cons *DCONSCDR (Lisp_Object obj);
Lisp_Cons *
DCONSCDR (Lisp_Object obj)
{
  return (CONSP (obj) && CONSP (XCDR (obj))) ? XCONS (XCDR (obj)) : 0;
}

Ibyte *DSTRING (Lisp_Object obj);
Ibyte *
DSTRING (Lisp_Object obj)
{
  return STRINGP (obj) ? XSTRING_DATA (obj) : NULL;
}

Lisp_Vector *DVECTOR (Lisp_Object obj);
Lisp_Vector *
DVECTOR (Lisp_Object obj)
{
  return VECTORP (obj) ? XVECTOR (obj) : NULL;
}

Lisp_Symbol *DSYMBOL (Lisp_Object obj);
Lisp_Symbol *
DSYMBOL (Lisp_Object obj)
{
  return SYMBOLP (obj) ? XSYMBOL (obj) : NULL;
}

Ibyte *DSYMNAME (Lisp_Object obj);
Ibyte *
DSYMNAME (Lisp_Object obj)
{
  return SYMBOLP (obj) ? XSTRING_DATA (XSYMBOL (obj)->name) : NULL;
}

#endif /* DEBUG_XEMACS */

DEFUN ("mswindows-message-box", Fmswindows_message_box, 1, 3, 0, /*
Pop up an MS Windows message box.
MESSAGE is the string to display.  Optional argument FLAG controls
what appears in the box and how it behaves; it is a symbol or list of
symbols, described below.  Second optional argument TITLE controls the
title bar; if omitted, a standard title bar will be used, probably
displaying "XEmacs".

Possible flags are


-- To specify the buttons in the message box:

abortretryignore 
  The message box contains three push buttons: Abort, Retry, and Ignore. 
ok 
  The message box contains one push button: OK. This is the default. 
okcancel 
  The message box contains two push buttons: OK and Cancel. 
retrycancel 
  The message box contains two push buttons: Retry and Cancel. 
yesno 
  The message box contains two push buttons: Yes and No. 
yesnocancel 
  The message box contains three push buttons: Yes, No, and Cancel. 


-- To display an icon in the message box:
 
iconexclamation, iconwarning
  An exclamation-point icon appears in the message box. 
iconinformation, iconasterisk
  An icon consisting of a lowercase letter i in a circle appears in
  the message box. 
iconquestion
  A question-mark icon appears in the message box. 
iconstop, iconerror, iconhand
  A stop-sign icon appears in the message box. 


-- To indicate the default button: 

defbutton1
  The first button is the default button.  This is the default.
defbutton2
  The second button is the default button. 
defbutton3
  The third button is the default button. 
defbutton4
  The fourth button is the default button. 


-- To indicate the modality of the dialog box:
 
applmodal
  The user must respond to the message box before continuing work in
  the window identified by the hWnd parameter. However, the user can
  move to the windows of other applications and work in those windows.
  Depending on the hierarchy of windows in the application, the user
  may be able to move to other windows within the application. All
  child windows of the parent of the message box are automatically
  disabled, but popup windows are not.  This is the default.
systemmodal
  Same as applmodal except that the message box has the WS_EX_TOPMOST
  style. Use system-modal message boxes to notify the user of serious,
  potentially damaging errors that require immediate attention (for
  example, running out of memory). This flag has no effect on the
  user's ability to interact with windows other than those associated
  with hWnd.
taskmodal
  Same as applmodal except that all the top-level windows belonging to
  the current task are disabled if the hWnd parameter is NULL. Use
  this flag when the calling application or library does not have a
  window handle available but still needs to prevent input to other
  windows in the current application without suspending other
  applications.


In addition, you can specify the following flags: 

default-desktop-only 
  The desktop currently receiving input must be a default desktop;
  otherwise, the function fails. A default desktop is one an
  application runs on after the user has logged on.
help 
  Adds a Help button to the message box. Choosing the Help button or
  pressing F1 generates a Help event.
right 
  The text is right-justified. 
rtlreading 
  Displays message and caption text using right-to-left reading order
  on Hebrew and Arabic systems.
setforeground 
  The message box becomes the foreground window. Internally, Windows
  calls the SetForegroundWindow function for the message box.
topmost 
  The message box is created with the WS_EX_TOPMOST window style. 
service-notification 
  Windows NT only: The caller is a service notifying the user of an
  event. The function displays a message box on the current active
  desktop, even if there is no user logged on to the computer.  If
  this flag is set, the hWnd parameter must be NULL. This is so the
  message box can appear on a desktop other than the desktop
  corresponding to the hWnd.



The return value is one of the following menu-item values returned by
the dialog box:
 
abort
  Abort button was selected. 
cancel
  Cancel button was selected. 
ignore
  Ignore button was selected. 
no
  No button was selected. 
ok
  OK button was selected. 
retry
  Retry button was selected. 
yes
  Yes button was selected. 

If a message box has a Cancel button, the function returns the
`cancel' value if either the ESC key is pressed or the Cancel button
is selected.  If the message box has no Cancel button, pressing ESC has
no effect.  */
       (message_, flags, title))
{
  Extbyte *msgout;
  Extbyte *titleout = 0;
  UINT sty = 0;

  if (!LISTP (flags))
    {
      CHECK_SYMBOL (flags);
      flags = list1 (flags);
    }

  CHECK_STRING (message_);
  msgout = LISP_STRING_TO_TSTR (message_);
  
  if (!NILP (title))
    {
      CHECK_STRING (title);
      titleout = LISP_STRING_TO_TSTR (title);
    }

  {
    EXTERNAL_LIST_LOOP_2 (st, flags)
      {
	CHECK_SYMBOL (st);
	if (0)
	  ;
#define FROB(sym, val) else if (EQ (st, sym)) sty |= val
	FROB (Qabortretryignore, MB_ABORTRETRYIGNORE);
	FROB (Qapplmodal, MB_APPLMODAL);
	FROB (Qdefault_desktop_only, MB_DEFAULT_DESKTOP_ONLY);
	FROB (Qdefbutton1, MB_DEFBUTTON1);
	FROB (Qdefbutton2, MB_DEFBUTTON2);
	FROB (Qdefbutton3, MB_DEFBUTTON3);
	FROB (Qdefbutton4, MB_DEFBUTTON4);
	FROB (Qhelp, MB_HELP);
	FROB (Qiconasterisk, MB_ICONASTERISK);
	FROB (Qiconexclamation, MB_ICONEXCLAMATION);
	FROB (Qiconhand, MB_ICONHAND);
	FROB (Qiconinformation, MB_ICONINFORMATION);
	FROB (Qiconquestion, MB_ICONQUESTION);
	FROB (Qiconstop, MB_ICONSTOP);
	FROB (Qok, MB_OK);
	FROB (Qokcancel, MB_OKCANCEL);
	FROB (Qretrycancel, MB_RETRYCANCEL);
	FROB (Qright, MB_RIGHT);
	FROB (Qrtlreading, MB_RTLREADING);
	FROB (Qservice_notification, MB_SERVICE_NOTIFICATION);
	FROB (Qsetforeground, MB_SETFOREGROUND);
	FROB (Qsystemmodal, MB_SYSTEMMODAL);
	FROB (Qtaskmodal, MB_TASKMODAL);
	FROB (Qtopmost, MB_TOPMOST);
	FROB (Qyesno, MB_YESNO);
	FROB (Qyesnocancel, MB_YESNOCANCEL);
#undef FROB

	else
	  invalid_constant ("Unrecognized flag", st);
      }
  }

  {
    int retval = qxeMessageBox (NULL, msgout, titleout, sty);

    if (retval == 0)
      out_of_memory ("When calling `mswindows-message-box'", Qunbound);

#define FROB(sym, val) if (retval == val) return sym
    FROB (Qabort, IDABORT);
    FROB (Qcancel, IDCANCEL);
    FROB (Qignore, IDIGNORE);
    FROB (Qno, IDNO);
    FROB (Qok, IDOK);
    FROB (Qretry, IDRETRY);
    FROB (Qyes, IDYES);
#undef FROB
    
    invalid_argument ("Unknown return value from MessageBox()",
		      make_int (retval));
  }

  return Qnil;
}

static Lisp_Object
msprinter_canonicalize_console_connection (Lisp_Object connection,
					   Error_Behavior errb)
{
  /* If nil connection is specified, transform it into the name
     of the default printer */
  if (NILP (connection))
    {
      connection = msprinter_default_printer ();
      if (NILP (connection))
	{
	  if (ERRB_EQ (errb, ERROR_ME))
	    invalid_state ("There is no default printer in the system",
			   Qunbound);
	  else
	    return Qunbound;
	}
    }

  CHECK_STRING (connection);
  return connection;
}

static Lisp_Object
msprinter_canonicalize_device_connection (Lisp_Object connection,
					  Error_Behavior errb)
{
  return msprinter_canonicalize_console_connection (connection, errb);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_console_mswindows (void)
{
  DEFSUBR (Fmswindows_debugging_output);

  DEFSYMBOL (Qabortretryignore);
  DEFSYMBOL (Qapplmodal);
  DEFSYMBOL (Qdefault_desktop_only);
  DEFSYMBOL (Qdefbutton1);
  DEFSYMBOL (Qdefbutton2);
  DEFSYMBOL (Qdefbutton3);
  DEFSYMBOL (Qdefbutton4);
  /* DEFSYMBOL (Qhelp); */
  DEFSYMBOL (Qiconasterisk);
  DEFSYMBOL (Qiconexclamation);
  DEFSYMBOL (Qiconhand);
  DEFSYMBOL (Qiconinformation);
  DEFSYMBOL (Qiconquestion);
  DEFSYMBOL (Qiconstop);
  /* DEFSYMBOL (Qok); */
  DEFSYMBOL (Qokcancel);
  DEFSYMBOL (Qretrycancel);
  /* DEFSYMBOL (Qright); */
  DEFSYMBOL (Qrtlreading);
  DEFSYMBOL (Qservice_notification);
  DEFSYMBOL (Qsetforeground);
  DEFSYMBOL (Qsystemmodal);
  DEFSYMBOL (Qtaskmodal);
  DEFSYMBOL (Qtopmost);
  DEFSYMBOL (Qyesno);
  DEFSYMBOL (Qyesnocancel);

  /* DEFSYMBOL (Qabort); */
  /* DEFSYMBOL (Qcancel); */
  /* DEFSYMBOL (Qignore); */
  /* DEFSYMBOL (Qno); */
  /* DEFSYMBOL (Qok); */
  /* DEFSYMBOL (Qretry); */
  /* DEFSYMBOL (Qyes); */

  DEFSUBR (Fmswindows_message_box);
}

void
console_type_create_mswindows (void)
{
  INITIALIZE_CONSOLE_TYPE (mswindows, "mswindows", "console-mswindows-p");

  /* console methods */
/*  CONSOLE_HAS_METHOD (mswindows, init_console); */
/*  CONSOLE_HAS_METHOD (mswindows, mark_console); */
  CONSOLE_HAS_METHOD (mswindows, initially_selected_for_input);
/*  CONSOLE_HAS_METHOD (mswindows, delete_console); */
  CONSOLE_HAS_METHOD (mswindows, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (mswindows, canonicalize_device_connection);
/*  CONSOLE_HAS_METHOD (mswindows, semi_canonicalize_console_connection); */
/*  CONSOLE_HAS_METHOD (mswindows, semi_canonicalize_device_connection); */
  CONSOLE_HAS_METHOD (mswindows, perhaps_init_unseen_key_defaults);

  INITIALIZE_CONSOLE_TYPE (msprinter, "msprinter", "console-msprinter-p");
  CONSOLE_HAS_METHOD (msprinter, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (msprinter, canonicalize_device_connection);
}

void
reinit_console_type_create_mswindows (void)
{
  REINITIALIZE_CONSOLE_TYPE (mswindows);
  REINITIALIZE_CONSOLE_TYPE (msprinter);
}

void
vars_of_console_mswindows (void)
{
  DEFVAR_LISP ("mswindows-seen-characters", &Vmswindows_seen_characters /*
Hash table of non-ASCII characters the MS Windows subsystem has seen.
*/ );
  Vmswindows_seen_characters = Qnil;
  Fprovide (Qmswindows);
}
