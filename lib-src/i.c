/* I-connector utility
   Copyright (C) 2000 Kirill M. Katsnelson
   Copyright (C) 2002, 2003 Ben Wing.

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

/* When run with an argument, i treats it as a command line, and pipes
command stdin, stdout and stderr to its own respective streams. How
silly it should sound, but windowed program in Win32 cannot do output
to the console from which it has been started, and should be run using
this utility.

This utility is for running [tx]emacs as part of make process so that
its output goes to the same console as the rest of the make output
does.  It can be used also when xemacs should be run as a batch
command ina script, especially when its standart output should be
obtained programmatically. */

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <tchar.h>

typedef struct
{
  HANDLE source;
  HANDLE drain;
} I_connector;

/* 
 * Make new handle as that pointed to by PH but
 * inheritable, substitute PH with it, and close the
 * original one
 */
static void
make_inheritable (HANDLE* ph)
{
  HANDLE htmp;
  DuplicateHandle (GetCurrentProcess(), *ph, GetCurrentProcess(), &htmp,
		   0, TRUE, DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
  *ph = htmp;
}

/*
 * Worker thread proc. Reads source, pumps into drain,
 * till either clogs.
 */
static DWORD CALLBACK
pump (LPVOID pv_i)
{
  I_connector* pi = (I_connector*) pv_i;
  BYTE buffer [256];
  DWORD really_read, unused;

  /* I said:
  
  [[ The docs for ReadFile claim:

  The ReadFile function returns when one of the following is true: a write
  operation completes on the write end of the pipe, the number of bytes
  requested has been read, or an error occurs.

  But this is just not true.  ReadFile never seems to block, and unless we
  Sleep(), we will chew up all the CPU time. --ben ]]

  But in fact

  [a] this does not appear to be the case any more [maybe a temporary
      bug in some versions of Win2000?]
  [b] it causes data lossage. [#### Why should this be?  Seems extremely
      fishy.  I tried commenting out the calls to close the standard
      handles at the bottom of the program, but it made no difference.
      Would we need some kind of additional handshaking?  If we get
      data loss with the sleep, then we are a race condition waiting
      to happen. */
  while (ReadFile (pi->source, buffer, sizeof (buffer), &really_read, NULL) &&
	 WriteFile (pi->drain, buffer, really_read, &unused, NULL))
    /* Sleep (100) */ ;

  return 0;
}

/*
 * Launch a pump for the given I-connector
 */
static void
start_pump (I_connector* pi)
{
  DWORD unused;
  HANDLE h_thread = CreateThread (NULL, 0, pump, (void*)pi, 0, &unused);
  CloseHandle (h_thread);
}

static HANDLE external_event;

static BOOL
ctrl_c_handler (unsigned long type)
{
  SetEvent (external_event);
  return FALSE;
}

/* Skip over the executable name in the given command line.  Correctly
   handles quotes in the name.  Return NULL upon error.  If
   REQUIRE_FOLLOWING is non-zero, it's an error if no argument follows the
   executable name. */

static LPTSTR
skip_executable_name (LPTSTR cl, int require_following)
{
  int ix;

  while (1)
    {
      ix = _tcscspn (cl, _T(" \t\""));
      if (cl[ix] == '\"')
	{
	  cl = _tcschr (cl + ix + 1, '\"');
	  if (cl == NULL)
	    return NULL; /* Unmatched quote */
	  cl++;
	}
      else
	{
	  cl += ix;
	  cl += _tcsspn (cl, _T(" \t"));
	  if (!require_following)
	    return cl;
	  return *cl ? cl : NULL;
	}
    }
}

/*
 * Brew coffee and bring snickers
 */
void
usage (void)
{
  fprintf (stderr,
   "\n"
   "usage: i command\n"
   "i executes the command and reroutes its standard handles to the calling\n"
   "console.  Good for seeing output of GUI programs that use standard output."
   "\n");
}

int
main (void)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  I_connector I_in, I_out, I_err;
  DWORD exit_code;
  LPTSTR command = skip_executable_name (GetCommandLine (), 1);
     
  if (command == NULL)
    {
      usage ();
      return 1;
    }

  ZeroMemory (&si, sizeof (si));
  si.dwFlags = STARTF_USESTDHANDLES;

  I_in.source = GetStdHandle (STD_INPUT_HANDLE);
  CreatePipe (&si.hStdInput, &I_in.drain, NULL, 0);
  make_inheritable (&si.hStdInput);

  I_out.drain = GetStdHandle (STD_OUTPUT_HANDLE);
  CreatePipe (&I_out.source, &si.hStdOutput, NULL, 0);
  make_inheritable (&si.hStdOutput);

  I_err.drain = GetStdHandle (STD_ERROR_HANDLE);
  CreatePipe (&I_err.source, &si.hStdError, NULL, 0);
  make_inheritable (&si.hStdError);

  {
    SECURITY_ATTRIBUTES sa;
    LPTSTR new_command =
      (LPTSTR) malloc (666 + sizeof (TCHAR) * _tcslen (command));
    LPTSTR past_exe;

    if (!new_command)
      {
	_ftprintf (stderr, _T ("Out of memory when launching `%s'\n"),
		   command);
	return 2;
      }

    past_exe = skip_executable_name (command, 0);
    if (!past_exe)
      {
	usage ();
	return 1;
      }

    /* Since XEmacs isn't a console application, it can't easily be
       terminated using ^C.  Therefore, we set up a communication path with
       it so that when a ^C is sent to us (using GenerateConsoleCtrlEvent),
       we in turn signals it to commit suicide. (This is cleaner than using
       TerminateProcess()).  This makes (e.g.) the "Stop Build" command
       from VC++ correctly terminate XEmacs.

       #### This will cause problems if i.exe is used for commands other
       than XEmacs.  We need to make behavior this a command-line
       option. */

    /* Create the event as inheritable so that we can use it to communicate
       with the child process */
    sa.nLength = sizeof (sa);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;
    external_event = CreateEvent (&sa, FALSE, FALSE, NULL);
    if (!external_event)
      {
	_ftprintf (stderr, _T ("Error %d creating signal event for `%s'\n"),
		   GetLastError (), command);
	return 2;
      }

    SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);
    _tcsncpy (new_command, command, past_exe - command);
    _stprintf (new_command + (past_exe - command),
	       /* start with space in case no args past command name */
	       " -mswindows-termination-handle %d ", (long) external_event);
    _tcscat (new_command, past_exe);
    
    if (CreateProcess (NULL, new_command, NULL, NULL, TRUE, 0,
		       NULL, NULL, &si, &pi) == 0)
      {
	_ftprintf (stderr, _T("Error %d launching `%s'\n"),
		   GetLastError (), command);
	return 2;
      }
    
    CloseHandle (pi.hThread);
  }


  /* Start pump in each I-connector */
  start_pump (&I_in);
  start_pump (&I_out);
  start_pump (&I_err);

  /* Wait for the process to complete */
  WaitForSingleObject (pi.hProcess, INFINITE);
  GetExitCodeProcess (pi.hProcess, &exit_code);
  CloseHandle (pi.hProcess);

  /* Make pump threads eventually die out. Looks rude, I agree */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  return exit_code;
}
