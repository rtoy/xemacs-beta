/* Old process support under MS Windows, soon to die.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 2001 Ben Wing.

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
Boston, MA 02111-1307, USA.

   Drew Bliss                   Oct 14, 1993
     Adapted from alarm.c by Tim Fleehart */

/* Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au> */
/* Synced with FSF Emacs 19.34.6 by Marc Paquette <marcpa@cam.org>
   (Note: Sync messages from Marc Paquette may indicate
   incomplete synching, so beware.)
 */

/* !!#### This piece of crap is not getting Mule-ized.  It will go away
   as soon as my process-stderr patches go in and there are a few stream
   device fixes, so my new call-process-written-using-start-process can
   always work. */

/* #### This ENTIRE file is only around because of callproc.c, which
   in turn is only used in batch mode.

   We only need two things to get rid of both this and callproc.c:

   -- my `stderr-proc' ws, which adds support for a separate stderr
      in asynch. subprocesses. (it's a feature in `old-call-process-internal'.)
   -- a noninteractive event loop that supports processes.
*/
#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-msw.h"
#include "process.h"

#ifdef HAVE_A_OUT_H
#include <a.out.h>
#endif
#include "sysfile.h"
#include "sysproc.h"
#include "syssignal.h"
#include "systime.h"
#include "syswait.h"


/* #### I'm not going to play with shit. */
// #pragma warning (disable:4013 4024 4090)

/* Control whether spawnve quotes arguments as necessary to ensure
   correct parsing by child process.  Because not all uses of spawnve
   are careful about constructing argv arrays, we make this behavior
   conditional (off by default). */
Lisp_Object Vwin32_quote_process_args;

/* Control whether create_child causes the process' window to be
   hidden.  The default is nil. */
Lisp_Object Vwin32_start_process_show_window;

/* Control whether create_child causes the process to inherit Emacs'
   console window, or be given a new one of its own.  The default is
   nil, to allow multiple DOS programs to run on Win95.  Having separate
   consoles also allows Emacs to cleanly terminate process groups.  */
Lisp_Object Vwin32_start_process_share_console;

/* Time to sleep before reading from a subprocess output pipe - this
   avoids the inefficiency of frequently reading small amounts of data.
   This is primarily necessary for handling DOS processes on Windows 95,
   but is useful for Win32 processes on both Win95 and NT as well.  */
Lisp_Object Vwin32_pipe_read_delay;

extern Lisp_Object Vlisp_EXEC_SUFFIXES;

/* child_process.status values */
enum {
  STATUS_READ_ERROR = -1,
  STATUS_READ_READY,
  STATUS_READ_IN_PROGRESS,
  STATUS_READ_FAILED,
  STATUS_READ_SUCCEEDED,
  STATUS_READ_ACKNOWLEDGED
};

/* This structure is used for both pipes and sockets; for
   a socket, the process handle in pi is NULL. */
typedef struct _child_process
{
  int                   fd;
  int                   pid;
  HANDLE                char_avail;
  HANDLE                char_consumed;
  HANDLE                thrd;
  HWND                  hwnd;
  PROCESS_INFORMATION   procinfo;
  volatile int          status;
  char                  chr;
} child_process;

#define MAX_CHILDREN  MAXDESC/2
#define CHILD_ACTIVE(cp) ((cp)->char_avail != NULL)

extern child_process * new_child (void);
extern void delete_child (child_process *cp);

/* parallel array of private info on file handles */
typedef struct
{
  unsigned         flags;
  HANDLE           hnd;
  child_process *  cp;
} filedesc;

extern filedesc fd_info [];

/* fd_info flag definitions */
#define FILE_READ    0x0001
#define FILE_WRITE   0x0002
#define FILE_BINARY  0x0010
#define FILE_LAST_CR            0x0020
#define FILE_AT_EOF             0x0040
#define FILE_SEND_SIGCHLD       0x0080
#define FILE_PIPE    0x0100
#define FILE_SOCKET  0x0200

/* #### This is an evil dirty hack. We must get rid of it.
   Word "munging" is not in XEmacs lexicon. - kkm */

/* parallel array of private info on file handles */
filedesc fd_info [ MAXDESC ];

#ifdef DEBUG_XEMACS
#define DebPrint(stuff) _DebPrint stuff
#else
#define DebPrint(stuff)
#endif

/* ------------------------------------------------------------------------- */

#ifndef DEBUG_XEMACS
__inline
#endif
void _DebPrint (const char *fmt, ...)
{
#ifdef DEBUG_XEMACS
  char buf[1024];
  va_list args;

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);
  OutputDebugStringA (buf);
#endif
}

/* sys_signal moved to nt.c. It's now called mswindows_signal... */

/* Child process management list.  */
int child_proc_count = 0;
child_process child_procs[ MAX_CHILDREN ];
child_process *dead_child = NULL;

DWORD WINAPI reader_thread (void *arg);

/* Find an unused process slot.  */
child_process *
new_child (void)
{
  child_process *cp;
  DWORD id;
  
  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (!CHILD_ACTIVE (cp))
      goto Initialize;
  if (child_proc_count == MAX_CHILDREN)
    return NULL;
  cp = &child_procs[child_proc_count++];

 Initialize:
  xzero (*cp);
  cp->fd = -1;
  cp->pid = -1;
  if (cp->procinfo.hProcess)
    CloseHandle(cp->procinfo.hProcess);
  cp->procinfo.hProcess = NULL;
  cp->status = STATUS_READ_ERROR;

  /* use manual reset event so that select() will function properly */
  cp->char_avail = CreateEventA (NULL, TRUE, FALSE, NULL);
  if (cp->char_avail)
    {
      cp->char_consumed = CreateEventA (NULL, FALSE, FALSE, NULL);
      if (cp->char_consumed)
        {
	  cp->thrd = CreateThread (NULL, 1024, reader_thread, cp, 0, &id);
	  if (cp->thrd)
	    return cp;
	}
    }
  delete_child (cp);
  return NULL;
}

void 
delete_child (child_process *cp)
{
  int i;

  /* Should not be deleting a child that is still needed. */
  for (i = 0; i < MAXDESC; i++)
    if (fd_info[i].cp == cp)
      abort ();

  if (!CHILD_ACTIVE (cp))
    return;

  /* reap thread if necessary */
  if (cp->thrd)
    {
      DWORD rc;

      if (GetExitCodeThread (cp->thrd, &rc) && rc == STILL_ACTIVE)
        {
	  /* let the thread exit cleanly if possible */
	  cp->status = STATUS_READ_ERROR;
	  SetEvent (cp->char_consumed);
	  if (WaitForSingleObject (cp->thrd, 1000) != WAIT_OBJECT_0)
	    {
	      DebPrint (("delete_child.WaitForSingleObject (thread) failed "
			 "with %lu for fd %ld\n", GetLastError (), cp->fd));
	      TerminateThread (cp->thrd, 0);
	    }
	}
      CloseHandle (cp->thrd);
      cp->thrd = NULL;
    }
  if (cp->char_avail)
    {
      CloseHandle (cp->char_avail);
      cp->char_avail = NULL;
    }
  if (cp->char_consumed)
    {
      CloseHandle (cp->char_consumed);
      cp->char_consumed = NULL;
    }

  /* update child_proc_count (highest numbered slot in use plus one) */
  if (cp == child_procs + child_proc_count - 1)
    {
      for (i = child_proc_count-1; i >= 0; i--)
	if (CHILD_ACTIVE (&child_procs[i]))
	  {
	    child_proc_count = i + 1;
	    break;
	  }
    }
  if (i < 0)
    child_proc_count = 0;
}

/* Find a child by pid.  */
static child_process *
find_child_pid (DWORD pid)
{
  child_process *cp;

  for (cp = child_procs+(child_proc_count-1); cp >= child_procs; cp--)
    if (CHILD_ACTIVE (cp) && pid == (DWORD) cp->pid)
      return cp;
  return NULL;
}

/* Function to do blocking read of one byte, needed to implement
   select.  It is only allowed on sockets and pipes. */
static int
_sys_read_ahead (int fd)
{
  child_process * cp;
  int rc = 0;

  if (fd < 0 || fd >= MAXDESC)
    return STATUS_READ_ERROR;

  cp = fd_info[fd].cp;

  if (cp == NULL || cp->fd != fd || cp->status != STATUS_READ_READY)
    return STATUS_READ_ERROR;

  if ((fd_info[fd].flags & (FILE_PIPE | FILE_SOCKET)) == 0
      || (fd_info[fd].flags & FILE_READ) == 0)
    {
      /* fd is not a pipe or socket */
      abort ();
    }
  
  cp->status = STATUS_READ_IN_PROGRESS;
  
  if (fd_info[fd].flags & FILE_PIPE)
    {
      rc = _read (fd, &cp->chr, sizeof (char));

      /* Give subprocess time to buffer some more output for us before
	 reporting that input is available; we need this because Win95
	 connects DOS programs to pipes by making the pipe appear to be
	 the normal console stdout - as a result most DOS programs will
	 write to stdout without buffering, ie.  one character at a
	 time.  Even some Win32 programs do this - "dir" in a command
	 shell on NT is very slow if we don't do this. */
      if (rc > 0)
	{
	  int wait = XINT (Vwin32_pipe_read_delay);

	  if (wait > 0)
	    Sleep (wait);
	  else if (wait < 0)
	    while (++wait <= 0)
	      /* Yield remainder of our time slice, effectively giving a
		 temporary priority boost to the child process. */
	      Sleep (0);
	}
    }

  if (rc == sizeof (char))
    cp->status = STATUS_READ_SUCCEEDED;
  else
    cp->status = STATUS_READ_FAILED;

  return cp->status;
}

/* Thread proc for child process and socket reader threads. Each thread
   is normally blocked until woken by select() to check for input by
   reading one char.  When the read completes, char_avail is signalled
   to wake up the select emulator and the thread blocks itself again. */
DWORD WINAPI 
reader_thread (void *arg)
{
  child_process *cp;
  
  /* Our identity */
  cp = (child_process *)arg;
  
  /* <matts@tibco.com> - I think the test below is wrong - we don't
     want to wait for someone to signal char_consumed, as we haven't
     read anything for them to consume yet! */

  /*
  if (cp == NULL ||
      WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
  */

  if (cp == NULL)
  {
      return 1;
  }

  for (;;)
    {
      int rc;

      rc = _sys_read_ahead (cp->fd);

      /* The name char_avail is a misnomer - it really just means the
	 read-ahead has completed, whether successfully or not. */
      if (!SetEvent (cp->char_avail))
        {
	  DebPrint (("reader_thread.SetEvent failed with %lu for fd %ld\n",
		     GetLastError (), cp->fd));
	  return 1;
	}

      if (rc == STATUS_READ_ERROR)
      {
        /* We are finished, so clean up handles and set to NULL so
           that CHILD_ACTIVE will see what is going on */
        if (cp->char_avail) {
          CloseHandle (cp->char_avail);
          cp->char_avail = NULL;
        }
        if (cp->thrd) {
          CloseHandle (cp->thrd);
          cp->thrd = NULL;
        }
        if (cp->char_consumed) {
          CloseHandle(cp->char_consumed);
          cp->char_consumed = NULL;
        }
        if (cp->procinfo.hProcess)
        {
          CloseHandle (cp->procinfo.hProcess);
          cp->procinfo.hProcess=NULL;
        }
        return 1;
      }
        
      /* If the read died, the child has died so let the thread die */
      if (rc == STATUS_READ_FAILED)
	break;
        
      /* Wait until our input is acknowledged before reading again */
      if (WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
        {
	  DebPrint (("reader_thread.WaitForSingleObject failed with "
		     "%lu for fd %ld\n", GetLastError (), cp->fd));
	  break;
        }
    }
  /* We are finished, so clean up handles and set to NULL so that
     CHILD_ACTIVE will see what is going on */
  if (cp->char_avail) {
    CloseHandle (cp->char_avail);
    cp->char_avail = NULL;
  }
  if (cp->thrd) {
    CloseHandle (cp->thrd);
    cp->thrd = NULL;
  }
  if (cp->char_consumed) {
    CloseHandle(cp->char_consumed);
    cp->char_consumed = NULL;
  }
  if (cp->procinfo.hProcess)
  {
    CloseHandle (cp->procinfo.hProcess);
    cp->procinfo.hProcess=NULL;
  }
  
  return 0;
}

/* This must die. */
static void
unixtodos_filename (char *p)
{
  while (*p)
    {
      if (*p == '/')
	*p = '\\';
      p++;
    }
}

/* To avoid Emacs changing directory, we just record here the directory
   the new process should start in.  This is set just before calling
   sys_spawnve, and is not generally valid at any other time.  */
static const char * process_dir;

static BOOL 
create_child (const char *exe, char *cmdline, char *env,
	      int * pPid, child_process *cp)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  SECURITY_DESCRIPTOR sec_desc;
  char dir[ PATH_MAX ];
  
  if (cp == NULL) abort ();
  
  xzero (start);
  start.cb = sizeof (start);
  
  if (NILP (Vwin32_start_process_show_window))
  start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  else
    start.dwFlags = STARTF_USESTDHANDLES;
  start.wShowWindow = SW_HIDE;

  start.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  start.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  start.hStdError = GetStdHandle (STD_ERROR_HANDLE);

  /* Explicitly specify no security */
  /* #### not supported under win98, but will go away */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_Fail;
  /* #### not supported under win98, but will go away */
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_Fail;
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = &sec_desc;
  sec_attrs.bInheritHandle = FALSE;
  
  strcpy (dir, process_dir);
  unixtodos_filename (dir);
  
  if (!CreateProcessA (exe, cmdline, &sec_attrs, NULL, TRUE,
		      (!NILP (Vwin32_start_process_share_console)
		       ? CREATE_NEW_PROCESS_GROUP
		       : CREATE_NEW_CONSOLE),
		      env, dir,
		      &start, &cp->procinfo))
    goto EH_Fail;

  cp->pid = (int) cp->procinfo.dwProcessId;

  CloseHandle (cp->procinfo.hThread);
  CloseHandle (cp->procinfo.hProcess);
  cp->procinfo.hThread=NULL;
  cp->procinfo.hProcess=NULL;

  /* pid must fit in a Lisp_Int */


  *pPid = cp->pid;
  
  return TRUE;
  
 EH_Fail:
  DebPrint (("create_child.CreateProcess failed: %ld\n", GetLastError()););
  return FALSE;
}

void
merge_and_sort_env (char **envp1, char **envp2, char **new_envp)
{
  char **optr, **nptr;
  int num;

  nptr = new_envp;
  optr = envp1;
  while (*optr)
    *nptr++ = *optr++;
  num = optr - envp1;

  optr = envp2;
  while (*optr)
    *nptr++ = *optr++;
  num += optr - envp2;

  qsort (new_envp, num, sizeof (char*), mswindows_compare_env);

  *nptr = NULL;
}

/* When a new child process is created we need to register it in our list,
   so intercept spawn requests.  */
int 
spawnve_will_die_soon (int mode, const Intbyte *cmdname,
		       const Intbyte * const *argv, const Intbyte *const *envp)
{
  Lisp_Object program, full;
  char *cmdline, *env, *parg, **targ;
  int arglen, numenv;
  int pid;
  child_process *cp;
  int is_dos_app, is_cygnus_app;
  int do_quoting = 0;
  char escape_char = 0;
  /* We pass our process ID to our children by setting up an environment
     variable in their environment.  */
  char ppid_env_var_buffer[64];
  char *extra_env[] = {ppid_env_var_buffer, NULL};
  struct gcpro gcpro1;
    
  /* We don't care about the other modes */
  if (mode != _P_NOWAIT)
    {
      errno = EINVAL;
      return -1;
    }

  /* Handle executable names without an executable suffix.  */
  program = build_string (cmdname);
  GCPRO1 (program);
  if (NILP (Ffile_executable_p (program)))
    {
      full = Qnil;
      locate_file (Vexec_path, program, Vlisp_EXEC_SUFFIXES, &full, 1);
      if (NILP (full))
	{
	  UNGCPRO;
	  errno = EINVAL;
	  return -1;
	}
      TO_EXTERNAL_FORMAT (LISP_STRING, full,
			  C_STRING_ALLOCA, cmdname,
			  Qfile_name);
    }
  else
    {
      cmdname = (char*)alloca (strlen (argv[0]) + 1);
      strcpy ((char*)cmdname, argv[0]);
    }
  UNGCPRO;

  /* make sure argv[0] and cmdname are both in DOS format */
  unixtodos_filename ((char*)cmdname);
  /* #### KLUDGE */
  ((const char**)argv)[0] = cmdname;

  /* Determine whether program is a 16-bit DOS executable, or a Win32
     executable that is implicitly linked to the Cygnus dll (implying it
     was compiled with the Cygnus GNU toolchain and hence relies on
     cygwin.dll to parse the command line - we use this to decide how to
     escape quote chars in command line args that must be quoted). */
  mswindows_executable_type (cmdname, &is_dos_app, &is_cygnus_app);

  /* On Windows 95, if cmdname is a DOS app, we invoke a helper
     application to start it by specifying the helper app as cmdname,
     while leaving the real app name as argv[0].  */
  if (is_dos_app)
    {
      cmdname = (char*) alloca (PATH_MAX);
      if (egetenv ("CMDPROXY"))
	strcpy ((char*)cmdname, egetenv ("CMDPROXY"));
      else
    {
	  strcpy ((char*)cmdname, XSTRING_DATA (Vinvocation_directory));
	  strcat ((char*)cmdname, "cmdproxy.exe");
	}
      unixtodos_filename ((char*)cmdname);
    }
  
  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.

     Additionally, zero-length args and args containing whitespace or
     quote chars need to be wrapped in double quotes - for this to work,
     embedded quotes need to be escaped as well.  The aim is to ensure
     the child process reconstructs the argv array we start with
     exactly, so we treat quotes at the beginning and end of arguments
     as embedded quotes.

     The Win32 GNU-based library from Cygnus doubles quotes to escape
     them, while MSVC uses backslash for escaping.  (Actually the MSVC
     startup code does attempt to recognize doubled quotes and accept
     them, but gets it wrong and ends up requiring three quotes to get a
     single embedded quote!)  So by default we decide whether to use
     quote or backslash as the escape character based on whether the
     binary is apparently a Cygnus compiled app.

     Note that using backslash to escape embedded quotes requires
     additional special handling if an embedded quote is already
     preceded by backslash, or if an arg requiring quoting ends with
     backslash.  In such cases, the run of escape characters needs to be
     doubled.  For consistency, we apply this special handling as long
     as the escape character is not quote.
   
     Since we have no idea how large argv and envp are likely to be we
     figure out list lengths on the fly and allocate them.  */
  
  if (!NILP (Vwin32_quote_process_args))
    {
      do_quoting = 1;
      /* Override escape char by binding win32-quote-process-args to
	 desired character, or use t for auto-selection.  */
      if (INTP (Vwin32_quote_process_args))
	escape_char = (char) XINT (Vwin32_quote_process_args);
      else
	escape_char = is_cygnus_app ? '"' : '\\';
    }
  
  /* do argv...  */
  arglen = 0;
  targ = (char**)argv;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;
      int escape_char_run = 0;

      if (*p == 0)
	need_quotes = 1;
      for ( ; *p; p++)
	{
	  if (*p == '"')
	  {
	      /* allow for embedded quotes to be escaped */
	    arglen++;
	      need_quotes = 1;
	      /* handle the case where the embedded quote is already escaped */
	      if (escape_char_run > 0)
		{
		  /* To preserve the arg exactly, we need to double the
		     preceding escape characters (plus adding one to
		     escape the quote character itself).  */
		  arglen += escape_char_run;
	  }
	    }
      else if (*p == ' ' || *p == '\t')
	    {
	      need_quotes = 1;
	    }

	  if (*p == escape_char && escape_char != '"')
	    escape_char_run++;
	  else
	    escape_char_run = 0;
	}
      if (need_quotes)
	{
	arglen += 2;
	  /* handle the case where the arg ends with an escape char - we
	     must not let the enclosing quote be escaped.  */
	  if (escape_char_run > 0)
	    arglen += escape_char_run;
	}
      arglen += strlen (*targ++) + 1;
    }
  cmdline = (char*) alloca (arglen);
  targ = (char**)argv;
  parg = cmdline;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;

      if (*p == 0)
	need_quotes = 1;

      if (do_quoting)
	{
	  for ( ; *p; p++)
	    if (*p == ' ' || *p == '\t' || *p == '"')
	      need_quotes = 1;
	}
      if (need_quotes)
	{
	  int escape_char_run = 0;
	  char * first;
	  char * last;

	  p = *targ;
	  first = p;
	  last = p + strlen (p) - 1;
	  *parg++ = '"';
#if 0
	  /* This version does not escape quotes if they occur at the
	     beginning or end of the arg - this could lead to incorrect
	     behavior when the arg itself represents a command line
	     containing quoted args.  I believe this was originally done
	     as a hack to make some things work, before
	     `win32-quote-process-args' was added.  */
	  while (*p)
	    {
	      if (*p == '"' && p > first && p < last)
		*parg++ = escape_char;	/* escape embedded quotes */
	      *parg++ = *p++;
	    }
#else
	  for ( ; *p; p++)
	    {
	      if (*p == '"')
		{
		  /* double preceding escape chars if any */
		  while (escape_char_run > 0)
		    {
		      *parg++ = escape_char;
		      escape_char_run--;
		    }
		  /* escape all quote chars, even at beginning or end */
		  *parg++ = escape_char;
		}
	      *parg++ = *p;

	      if (*p == escape_char && escape_char != '"')
		escape_char_run++;
	      else
		escape_char_run = 0;
	    }
	  /* double escape chars before enclosing quote */
	  while (escape_char_run > 0)
	    {
	      *parg++ = escape_char;
	      escape_char_run--;
	    }
#endif
	  *parg++ = '"';
	}
      else
	{
	  strcpy (parg, *targ);
	  parg += strlen (*targ);
	}
      *parg++ = ' ';
      targ++;
    }
  *--parg = '\0';
  
  /* and envp...  */
  arglen = 1;
  targ = (char**) envp;
  numenv = 1; /* for end null */
  while (*targ)
    {
      arglen += strlen (*targ++) + 1;
      numenv++;
    }
  /* extra env vars... */
  sprintf (ppid_env_var_buffer, "__PARENT_PROCESS_ID=%d", 
	   GetCurrentProcessId ());
  arglen += strlen (ppid_env_var_buffer) + 1;
  numenv++;

  /* merge env passed in and extra env into one, and sort it.  */
  targ = (char **) alloca (numenv * sizeof (char*));
  merge_and_sort_env ((char**) envp, extra_env, targ);

  /* concatenate env entries.  */
  env = (char*) alloca (arglen);
  parg = env;
  while (*targ)
    {
      strcpy (parg, *targ);
      parg += strlen (*targ++);
      *parg++ = '\0';
    }
  *parg++ = '\0';
  *parg = '\0';

  cp = new_child ();
  if (cp == NULL)
    {
      errno = EAGAIN;
      return -1;
    }
  
  /* Now create the process.  */
  if (!create_child (cmdname, cmdline, env, &pid, cp))
    {
      delete_child (cp);
      errno = ENOEXEC;
      return -1;
    }

  return pid;
}

/* Substitute for certain kill () operations */

static BOOL CALLBACK
find_child_console (HWND hwnd, child_process * cp)
{
  DWORD thread_id;
  DWORD process_id;

  thread_id = GetWindowThreadProcessId (hwnd, &process_id);
  if (process_id == cp->procinfo.dwProcessId)
    {
      char window_class[32];

      GetClassNameA (hwnd, window_class, sizeof (window_class));
      if (strcmp (window_class,
		  mswindows_windows9x_p
		  ? "tty"
		  : "ConsoleWindowClass") == 0)
	{
	  cp->hwnd = hwnd;
	  return FALSE;
	}
    }
  /* keep looking */
  return TRUE;
}

int 
kill_will_disappear_soon (int pid, int sig)
{
  child_process *cp;
  HANDLE proc_hand;
  int need_to_free = 0;
  int rc = 0;
  
  /* Only handle signals that will result in the process dying */
  if (sig != SIGINT && sig != SIGKILL && sig != SIGQUIT && sig != SIGHUP)
    {
      errno = EINVAL;
      return -1;
    }

  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      proc_hand = OpenProcess (PROCESS_TERMINATE, 0, pid);
      if (proc_hand == NULL)
        {
	  errno = EPERM;
	  return -1;
	}
      need_to_free = 1;
    }
  else
    {
      proc_hand = cp->procinfo.hProcess;
      pid = cp->procinfo.dwProcessId;

      /* Try to locate console window for process. */
      EnumWindows ((WNDENUMPROC)find_child_console, (LPARAM) cp);
    }
  
  if (sig == SIGINT)
    {
      if (NILP (Vwin32_start_process_share_console) && cp && cp->hwnd)
	{
	  BYTE control_scan_code = (BYTE) MapVirtualKeyA (VK_CONTROL, 0);
	  BYTE vk_break_code = VK_CANCEL;
	  BYTE break_scan_code = (BYTE) MapVirtualKeyA (vk_break_code, 0);
	  HWND foreground_window;

	  if (break_scan_code == 0)
	    {
	      /* Fake Ctrl-C if we can't manage Ctrl-Break. */
	      vk_break_code = 'C';
	      break_scan_code = (BYTE) MapVirtualKeyA (vk_break_code, 0);
	    }

	  foreground_window = GetForegroundWindow ();
	  if (foreground_window && SetForegroundWindow (cp->hwnd))
	    {
	      /* Generate keystrokes as if user had typed Ctrl-Break or Ctrl-C.  */
	      keybd_event (VK_CONTROL, control_scan_code, 0, 0);
	      keybd_event (vk_break_code, break_scan_code, 0, 0);
	      keybd_event (vk_break_code, break_scan_code, KEYEVENTF_KEYUP, 0);
	      keybd_event (VK_CONTROL, control_scan_code, KEYEVENTF_KEYUP, 0);

	      /* Sleep for a bit to give time for Emacs frame to respond
		 to focus change events (if Emacs was active app).  */
	      Sleep (10);

	      SetForegroundWindow (foreground_window);
	    }
	}
      /* Ctrl-Break is NT equivalent of SIGINT.  */
      else if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
	  DebPrint (("sys_kill.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }
  else
    {
      if (NILP (Vwin32_start_process_share_console) && cp && cp->hwnd)
	{
#if 1
	  if (mswindows_windows9x_p)
	    {
/*
   Another possibility is to try terminating the VDM out-right by
   calling the Shell VxD (id 0x17) V86 interface, function #4
   "SHELL_Destroy_VM", ie.

     mov edx,4
     mov ebx,vm_handle
     call shellapi

   First need to determine the current VM handle, and then arrange for
   the shellapi call to be made from the system vm (by using
   Switch_VM_and_callback).

   Could try to invoke DestroyVM through CallVxD.

*/
#if 0
	      /* On Win95, posting WM_QUIT causes the 16-bit subsystem
		 to hang when cmdproxy is used in conjunction with
		 command.com for an interactive shell.  Posting
		 WM_CLOSE pops up a dialog that, when Yes is selected,
		 does the same thing.  TerminateProcess is also less
		 than ideal in that subprocesses tend to stick around
		 until the machine is shutdown, but at least it
		 doesn't freeze the 16-bit subsystem.  */
	      PostMessage (cp->hwnd, WM_QUIT, 0xff, 0);
#endif
	      if (!TerminateProcess (proc_hand, 0xff))
		{
		  DebPrint (("sys_kill.TerminateProcess returned %d "
			     "for pid %lu\n", GetLastError (), pid));
		  errno = EINVAL;
		  rc = -1;
		}
	    }
	  else
#endif
	    PostMessageA (cp->hwnd, WM_CLOSE, 0, 0);
	}
      /* Kill the process.  On Win32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's not
	 used in every case.  */
      else if (!TerminateProcess (proc_hand, 0xff))
        {
	  DebPrint (("sys_kill.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }

  if (need_to_free)
    CloseHandle (proc_hand);

  return rc;
}

/* From callproc.c  */
extern Lisp_Object Vbinary_process_input;
extern Lisp_Object Vbinary_process_output;

/* Unix pipe() has only one arg */
/* Will die as soon as callproc.c dies */
int
pipe_will_die_soon (int *phandles)
{
  int rc;
  unsigned flags;

  /* make pipe handles non-inheritable; when we spawn a child, we
     replace the relevant handle with an inheritable one.  Also put
     pipes into binary mode; we will do text mode translation ourselves
     if required.  */
  rc = _pipe (phandles, 0, _O_NOINHERIT | _O_BINARY);

  if (rc == 0)
    {
      flags = FILE_PIPE | FILE_READ;
      if (!NILP (Vbinary_process_output))
	  flags |= FILE_BINARY;
      fd_info[phandles[0]].flags = flags;

      flags = FILE_PIPE | FILE_WRITE;
      if (!NILP (Vbinary_process_input))
	  flags |= FILE_BINARY;
      fd_info[phandles[1]].flags = flags;
    }

  return rc;
}

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  HANDLE parent;
  HANDLE newstdin, newstdout, newstderr;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);
  handles[2] = GetStdHandle (STD_ERROR_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent, 
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin, 
		       0, 
		       TRUE, 
		       DUPLICATE_SAME_ACCESS))
    mswindows_report_process_error ("Duplicating input handle for child",
				    Qunbound, GetLastError ());
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    mswindows_report_process_error ("Duplicating output handle for child",
				    Qunbound, GetLastError ());
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (err),
		       parent,
		       &newstderr,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    mswindows_report_process_error ("Duplicating error handle for child",
				    Qunbound, GetLastError ());

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin))
    mswindows_report_process_error ("Changing stdin handle",
				    Qunbound, GetLastError ());
  
  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout))
    mswindows_report_process_error ("Changing stdout handle",
				    Qunbound, GetLastError ());

  if (!SetStdHandle (STD_ERROR_HANDLE, newstderr))
    mswindows_report_process_error ("Changing stderr handle",
				    Qunbound, GetLastError ());
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  /* close the duplicated handles passed to the child */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle (STD_INPUT_HANDLE, handles[0]);
  SetStdHandle (STD_OUTPUT_HANDLE, handles[1]);
  SetStdHandle (STD_ERROR_HANDLE, handles[2]);
}

void
set_process_dir (const char * dir)
{
  process_dir = dir;
}


void
syms_of_ntproc (void)
{
}

void
vars_of_ntproc (void)
{
  DEFVAR_LISP ("win32-quote-process-args", &Vwin32_quote_process_args /*
    Non-nil enables quoting of process arguments to ensure correct parsing.
Because Windows does not directly pass argv arrays to child processes,
programs have to reconstruct the argv array by parsing the command
line string.  For an argument to contain a space, it must be enclosed
in double quotes or it will be parsed as multiple arguments.

If the value is a character, that character will be used to escape any
quote characters that appear, otherwise a suitable escape character
will be chosen based on the type of the program.
*/ );
  Vwin32_quote_process_args = Qt;

  DEFVAR_LISP ("win32-start-process-show-window",
	       &Vwin32_start_process_show_window /*
    When nil, processes started via start-process hide their windows.
When non-nil, they show their window in the method of their choice.
*/ );
  Vwin32_start_process_show_window = Qnil;

  DEFVAR_LISP ("win32-start-process-share-console",
	       &Vwin32_start_process_share_console /*
    When nil, processes started via start-process are given a new console.
When non-nil, they share the Emacs console; this has the limitation of
allowing only one DOS subprocess to run at a time (whether started directly
or indirectly by Emacs), and preventing Emacs from cleanly terminating the
subprocess group, but may allow Emacs to interrupt a subprocess that doesn't
otherwise respond to interrupts from Emacs.
*/ );
  Vwin32_start_process_share_console = Qt;

  DEFVAR_LISP ("win32-pipe-read-delay", &Vwin32_pipe_read_delay /*
    Forced delay before reading subprocess output.
This is done to improve the buffering of subprocess output, by
avoiding the inefficiency of frequently reading small amounts of data.

If positive, the value is the number of milliseconds to sleep before
reading the subprocess output.  If negative, the magnitude is the number
of time slices to wait (effectively boosting the priority of the child
process temporarily).  A value of zero disables waiting entirely.
*/ );
  Vwin32_pipe_read_delay = make_int (50);
}

/* end of ntproc.c */
