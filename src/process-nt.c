/* Asynchronous subprocess implementation for Win32
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002 Ben Wing.

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

/* Written by Kirill M. Katsnelson <kkm@kis.ru>, April 1998 */

/* Mule-ized as of 8-6-00 */

#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "hash.h"
#include "lstream.h"
#include "process.h"
#include "procimpl.h"

#include "sysfile.h"
#include "sysproc.h"
#include "syssignal.h"

/* Bound by win32-native.el */
Lisp_Object Qmswindows_construct_process_command_line;

/* Arbitrary size limit for code fragments passed to run_in_other_process */
#define FRAGMENT_CODE_SIZE 32

/* Implementation-specific data. Pointed to by Lisp_Process->process_data */
struct nt_process_data
{
  HANDLE h_process;
  DWORD dwProcessId;
  HWND hwnd; /* console window */
  int selected_for_exit_notify;
};

/* Control whether create_child causes the process to inherit Emacs'
   console window, or be given a new one of its own.  The default is
   nil, to allow multiple DOS programs to run on Win95.  Having separate
   consoles also allows Emacs to cleanly terminate process groups.  */
Lisp_Object Vmswindows_start_process_share_console;

/* Control whether create_child cause the process to inherit Emacs'
   error mode setting.  The default is t, to minimize the possibility of
   subprocesses blocking when accessing unmounted drives.  */
Lisp_Object Vmswindows_start_process_inherit_error_mode;

#define NT_DATA(p) ((struct nt_process_data *)((p)->process_data))

/*-----------------------------------------------------------------------*/
/* Process helpers							 */
/*-----------------------------------------------------------------------*/

/* These break process abstraction. Prototypes in console-msw.h,
   used by select_process method in event-msw.c.

   If called the first time on a process, return the process handle, so we
   can select on it and receive exit notification.  "First time only" so we
   don't select the same process multiple times if someone turns off and on
   the receipt of process data. */

HANDLE
get_nt_process_handle_only_first_time (Lisp_Process *p)
{
  if (NT_DATA (p)->selected_for_exit_notify)
    return INVALID_HANDLE_VALUE;
  NT_DATA (p)->selected_for_exit_notify = 1;
  return (NT_DATA (p)->h_process);
}

HANDLE
get_nt_process_handle (Lisp_Process *p)
{
  return (NT_DATA (p)->h_process);
}

static struct Lisp_Process *
find_process_from_pid (DWORD pid)
{
  Lisp_Object tail, proc;

  for (tail = Vprocess_list; CONSP (tail); tail = XCDR (tail))
    {
      proc = XCAR (tail);
      if (NT_DATA (XPROCESS (proc))->dwProcessId == pid)
	return XPROCESS (proc);
    }
  return 0;
}


/*-----------------------------------------------------------------------*/
/* Running remote threads. See Microsoft Systems Journal 1994 Number 5	 */
/* Jeffrey Richter, Load Your 32-bit DLL into Another Process's Address..*/
/*-----------------------------------------------------------------------*/

typedef struct
{
  HANDLE h_process;
  HANDLE h_thread;
  LPVOID address;
} process_memory;

/*
 * Allocate SIZE bytes in H_PROCESS address space. Fill in PMC used
 * further by other routines. Return nonzero if successful.
 *
 * The memory in other process is allocated by creating a suspended
 * thread. Initial stack of that thread is used as the memory
 * block. The thread entry point is the routine ExitThread in
 * kernel32.dll, so the allocated memory is freed just by resuming the
 * thread, which immediately terminates after that.
 */

static int
alloc_process_memory (HANDLE h_process, Bytecount size,
		      process_memory *pmc)
{
  LPTHREAD_START_ROUTINE adr_ExitThread =
    (LPTHREAD_START_ROUTINE)
    GetProcAddress (qxeGetModuleHandle (XETEXT ("kernel32")), "ExitThread");
  DWORD dw_unused;
  CONTEXT context;
  MEMORY_BASIC_INFORMATION mbi;

  pmc->h_process = h_process;
  pmc->h_thread = CreateRemoteThread (h_process, NULL, size,
				      adr_ExitThread, NULL,
				      CREATE_SUSPENDED, &dw_unused);
  if (pmc->h_thread == NULL)
    return 0;

  /* Get context, for thread's stack pointer */
  context.ContextFlags = CONTEXT_CONTROL;
  if (!GetThreadContext (pmc->h_thread, &context))
    goto failure;

  /* Determine base address of the committed range */
  if (sizeof(mbi) != VirtualQueryEx (h_process,
#if defined (_X86_)
				     (LPDWORD)context.Esp - 1,
#elif defined (_ALPHA_)
				     (LPDWORD)context.IntSp - 1,
#else
#error Unknown processor architecture
#endif
				     &mbi, sizeof(mbi)))
    goto failure;

  /* Change the page protection of the allocated memory to executable,
     read, and write. */
  if (!VirtualProtectEx (h_process, mbi.BaseAddress, size,
			 PAGE_EXECUTE_READWRITE, &dw_unused))
    goto failure;

  pmc->address = mbi.BaseAddress;
  return 1;

 failure:
  ResumeThread (pmc->h_thread);
  pmc->address = 0;
  return 0;
}

static void
free_process_memory (process_memory *pmc)
{
  ResumeThread (pmc->h_thread);
}

/*
 * Run ROUTINE in the context of process determined by H_PROCESS. The
 * routine is passed the address of DATA as parameter. The ROUTINE must
 * not be longer than ROUTINE_CODE_SIZE bytes. DATA_SIZE is the size of
 * DATA structure.
 *
 * Note that the code must be positionally independent, and compiled
 * without stack checks (they cause implicit calls into CRT so will
 * fail). DATA should not refer any data in calling process, as both
 * routine and its data are copied into remote process. Size of data
 * and code together should not exceed one page (4K on x86 systems).
 *
 * Return the value returned by ROUTINE, or (DWORD)-1 if call failed.
 */
static DWORD
run_in_other_process (HANDLE h_process,
		      LPTHREAD_START_ROUTINE routine,
		      LPVOID data, Bytecount data_size)
{
  process_memory pm;
  const Bytecount code_size = FRAGMENT_CODE_SIZE;
  /* Need at most 3 extra bytes of memory, for data alignment */
  Bytecount total_size = code_size + data_size + 3;
  LPVOID remote_data;
  HANDLE h_thread;
  DWORD dw_unused;

  /* Allocate memory */
  if (!alloc_process_memory (h_process, total_size, &pm))
    return (DWORD)-1;

  /* Copy code */
  if (!WriteProcessMemory (h_process, pm.address, (LPVOID)routine,
			   code_size, NULL))
    goto failure;

  /* Copy data */
  if (data_size)
    {
      remote_data = (LPBYTE)pm.address + ((code_size + 4) & ~3);
      if (!WriteProcessMemory (h_process, remote_data, data, data_size, NULL))
	goto failure;
    }
  else
    remote_data = NULL;

  /* Execute the remote copy of code, passing it remote data */
  h_thread = CreateRemoteThread (h_process, NULL, 0,
				 (LPTHREAD_START_ROUTINE) pm.address,
				 remote_data, 0, &dw_unused);
  if (h_thread == NULL)
    goto failure;

  /* Wait till thread finishes */
  WaitForSingleObject (h_thread, INFINITE);

  /* Free remote memory */
  free_process_memory (&pm);

  /* Return thread's exit code */
  {
    DWORD exit_code;
    GetExitCodeThread (h_thread, &exit_code);
    CloseHandle (h_thread);
    return exit_code;
  }

 failure:
  free_process_memory (&pm);
  return (DWORD)-1;
}

/*-----------------------------------------------------------------------*/
/* Sending signals							 */
/*-----------------------------------------------------------------------*/

/* ---------------------------- the NT way ------------------------------- */

/*
 * We handle the following signals:
 *
 * SIGKILL, SIGTERM, SIGQUIT, SIGHUP - These four translate to ExitProcess
 *    executed by the remote process
 * SIGINT - The remote process is sent CTRL_BREAK_EVENT
 *
 * The MSVC5.0 compiler feels free to re-order functions within a
 * compilation unit, so we have no way of finding out the size of the
 * following functions. Therefore these functions must not be larger than
 * FRAGMENT_CODE_SIZE.
 */

/*
 * Sending SIGKILL
 */
typedef struct
{
  void (WINAPI *adr_ExitProcess) (UINT);
} sigkill_data;

static DWORD WINAPI
sigkill_proc (sigkill_data *data)
{
  (*data->adr_ExitProcess)(255);
  return 1;
}

/*
 * Sending break or control c
 */
typedef struct
{
  BOOL (WINAPI *adr_GenerateConsoleCtrlEvent) (DWORD, DWORD);
  DWORD event;
} sigint_data;

static DWORD WINAPI
sigint_proc (sigint_data *data)
{
  return (*data->adr_GenerateConsoleCtrlEvent) (data->event, 0);
}

/*
 * Enabling signals
 */
typedef struct
{
  BOOL (WINAPI *adr_SetConsoleCtrlHandler) (LPVOID, BOOL);
} sig_enable_data;

static DWORD WINAPI
sig_enable_proc (sig_enable_data *data)
{
  (*data->adr_SetConsoleCtrlHandler) (NULL, FALSE);
  return 1;
}

/*
 * Send signal SIGNO to process H_PROCESS.
 * Return nonzero if successful.
 */

static int
send_signal_the_nt_way (struct nt_process_data *cp, int pid, int signo)
{
  HANDLE h_process;
  HMODULE h_kernel = qxeGetModuleHandle (XETEXT ("kernel32"));
  int close_process = 0;
  DWORD retval;

  assert (h_kernel != NULL);

  if (cp)
    {
      pid = cp->dwProcessId;
      h_process = cp->h_process;
    }
  else
    {
      close_process = 1;
      /* Try to open the process with required privileges */
      h_process = OpenProcess (PROCESS_CREATE_THREAD
			       | PROCESS_QUERY_INFORMATION
			       | PROCESS_VM_OPERATION
			       | PROCESS_VM_WRITE,
			       FALSE, pid);
      if (!h_process)
	return 0;
    }

  switch (signo)
    {
    case SIGKILL:
    case SIGTERM:
    case SIGQUIT:
    case SIGHUP:
      {
	sigkill_data d;

	d.adr_ExitProcess =
	  (void (WINAPI *) (UINT)) GetProcAddress (h_kernel, "ExitProcess");
	assert (d.adr_ExitProcess);
	retval = run_in_other_process (h_process,
				       (LPTHREAD_START_ROUTINE) sigkill_proc,
				       &d, sizeof (d));
	break;
      }
    case SIGINT:
      {
	sigint_data d;
	d.adr_GenerateConsoleCtrlEvent =
	  (BOOL (WINAPI *) (DWORD, DWORD))
	  GetProcAddress (h_kernel, "GenerateConsoleCtrlEvent");
	assert (d.adr_GenerateConsoleCtrlEvent);
	d.event = CTRL_C_EVENT;
	retval = run_in_other_process (h_process,
				       (LPTHREAD_START_ROUTINE) sigint_proc,
				       &d, sizeof (d));
	break;
      }
    default:
      assert (0);
    }

  if (close_process)
    CloseHandle (h_process);
  return (int)retval > 0 ? 1 : 0;
}

/*
 * Enable CTRL_C_EVENT handling in a new child process
 */
static void
enable_child_signals (HANDLE h_process)
{
  HMODULE h_kernel = qxeGetModuleHandle (XETEXT ("kernel32"));
  sig_enable_data d;

  assert (h_kernel != NULL);
  d.adr_SetConsoleCtrlHandler =
    (BOOL (WINAPI *) (LPVOID, BOOL))
    GetProcAddress (h_kernel, "SetConsoleCtrlHandler");
  assert (d.adr_SetConsoleCtrlHandler);
  run_in_other_process (h_process, (LPTHREAD_START_ROUTINE)sig_enable_proc,
			&d, sizeof (d));
}

/* ---------------------------- the 95 way ------------------------------- */

static BOOL CALLBACK
find_child_console (HWND hwnd, long putada)
{
  DWORD thread_id;
  DWORD process_id;
  struct nt_process_data *cp = (struct nt_process_data *) putada;

  thread_id = GetWindowThreadProcessId (hwnd, &process_id);
  if (process_id == cp->dwProcessId)
    {
      Extbyte window_class[32];

      /* GetClassNameA to avoid problems with Unicode return values */
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

static int
send_signal_the_95_way (struct nt_process_data *cp, int pid, int signo)
{
  HANDLE h_process;
  int close_process = 0;
  int rc = 1;

  if (cp)
    {
      pid = cp->dwProcessId;
      h_process = cp->h_process;

      /* Try to locate console window for process. */
      EnumWindows (find_child_console, (LPARAM) cp);
    }
  else
    {
      close_process = 1;
      /* Try to open the process with required privileges */
      h_process = OpenProcess (PROCESS_TERMINATE, FALSE, pid);
      if (!h_process)
	return 0;
    }

  if (signo == SIGINT)
    {
      if (NILP (Vmswindows_start_process_share_console) && cp && cp->hwnd)
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
	  if (foreground_window)
	    {
              /* NT 5.0, and apparently also Windows 98, will not allow
		 a Window to be set to foreground directly without the
		 user's involvement. The workaround is to attach
		 ourselves to the thread that owns the foreground
		 window, since that is the only thread that can set the
		 foreground window.  */
              DWORD foreground_thread, child_thread;
              foreground_thread =
		GetWindowThreadProcessId (foreground_window, NULL);
	      if (foreground_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         foreground_thread, TRUE))
                foreground_thread = 0;

              child_thread = GetWindowThreadProcessId (cp->hwnd, NULL);
	      if (child_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         child_thread, TRUE))
                child_thread = 0;

              /* Set the foreground window to the child.  */
              if (SetForegroundWindow (cp->hwnd))
                {
                  /* Generate keystrokes as if user had typed Ctrl-Break or
                     Ctrl-C.  */
                  keybd_event (VK_CONTROL, control_scan_code, 0, 0);
                  keybd_event (vk_break_code, break_scan_code,
		    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY), 0);
                  keybd_event (vk_break_code, break_scan_code,
                    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY)
                    | KEYEVENTF_KEYUP, 0);
                  keybd_event (VK_CONTROL, control_scan_code,
                               KEYEVENTF_KEYUP, 0);

                  /* Sleep for a bit to give time for Emacs frame to respond
                     to focus change events (if Emacs was active app).  */
                  Sleep (100);

                  SetForegroundWindow (foreground_window);
                }
              /* Detach from the foreground and child threads now that
                 the foreground switching is over.  */
              if (foreground_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   foreground_thread, FALSE);
              if (child_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   child_thread, FALSE);
            }
        }
      /* Ctrl-Break is NT equivalent of SIGINT.  */
      else if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
#if 0 /* FSF Emacs */
	  DebPrint (("sys_kill.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
#endif
	  rc = 0;
	}
    }
  else
    {
      if (NILP (Vmswindows_start_process_share_console) && cp && cp->hwnd)
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
	      qxePostMessage (cp->hwnd, WM_QUIT, 0xff, 0);
#endif
	      if (!TerminateProcess (h_process, 0xff))
		{
#if 0 /* FSF Emacs */
		  DebPrint (("sys_kill.TerminateProcess returned %d "
			     "for pid %lu\n", GetLastError (), pid));
		  errno = EINVAL;
#endif
		  rc = 0;
		}
	    }
	  else
#endif
	    qxePostMessage (cp->hwnd, WM_CLOSE, 0, 0);
	}
      /* Kill the process.  On W32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's not
	 used in every case.  */
      else if (!TerminateProcess (h_process, 0xff))
        {
#if 0 /* FSF Emacs */
	  DebPrint (("sys_kill.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
#endif
	  rc = 0;
        }
    }

  if (close_process)
    CloseHandle (h_process);

  return rc;
}

/* -------------------------- all-OS functions ---------------------------- */

static int
send_signal (struct nt_process_data *cp, int pid, int signo)
{
  return (!mswindows_windows9x_p && send_signal_the_nt_way (cp, pid, signo))
    || send_signal_the_95_way (cp, pid, signo);
}

/*
 * Signal error if SIGNO is not supported
 */
static void
validate_signal_number (int signo)
{
  if (signo != SIGKILL && signo != SIGTERM
      && signo != SIGQUIT && signo != SIGINT
      && signo != SIGHUP)
    invalid_constant ("Signal number not supported", make_int (signo));
}

/*-----------------------------------------------------------------------*/
/* Process methods							 */
/*-----------------------------------------------------------------------*/

/*
 * Allocate and initialize Lisp_Process->process_data
 */

static void
nt_alloc_process_data (Lisp_Process *p)
{
  p->process_data = xnew_and_zero (struct nt_process_data);
}

static void
nt_finalize_process_data (Lisp_Process *p, int for_disksave)
{
  assert (!for_disksave);
  /* If it's still in the list of processes we are waiting on delete
     it.  */
  mswindows_unwait_process (p);
  if (NT_DATA (p)->h_process)
    CloseHandle (NT_DATA (p)->h_process);
}

/*
 * Initialize XEmacs process implementation once
 */
static void
nt_init_process (void)
{
  /* Initialize winsock */
  WSADATA wsa_data;
  /* Request Winsock v1.1 Note the order: (minor=1, major=1) */
  WSAStartup (MAKEWORD (1,1), &wsa_data);
}

/*
 * Fork off a subprocess. P is a pointer to newly created subprocess
 * object. If this function signals, the caller is responsible for
 * deleting (and finalizing) the process object.
 *
 * The method must return PID of the new process, a (positive??? ####) number
 * which fits into Lisp_Int. No return value indicates an error, the method
 * must signal an error instead.
 */

static DOESNT_RETURN
mswindows_report_winsock_error (const char *string, Lisp_Object data,
				int errnum)
{
  report_file_type_error (Qnetwork_error, mswindows_lisp_error (errnum),
			  string, data);
}

static void
ensure_console_window_exists (void)
{
  if (mswindows_windows9x_p)
    mswindows_hide_console ();
}

int
mswindows_compare_env (const void *strp1, const void *strp2)
{
  const Ibyte *str1 = *(const Ibyte **)strp1,
    *str2 = *(const Ibyte **)strp2;

  while (*str1 && *str2 && *str1 != '=' && *str2 != '=')
    {
      if ((*str1) > (*str2))
	return 1;
      else if ((*str1) < (*str2))
	return -1;
      str1++, str2++;
    }

  if (*str1 == '=' && *str2 == '=')
    return 0;
  else if (*str1 == '=')
    return -1;
  else
    return 1;
}

/*
 * Fork off a subprocess. P is a pointer to newly created subprocess
 * object. If this function signals, the caller is responsible for
 * deleting (and finalizing) the process object.
 *
 * The method must return PID of the new process, a (positive??? ####) number
 * which fits into Lisp_Int. No return value indicates an error, the method
 * must signal an error instead.
 */

static int
nt_create_process (Lisp_Process *p,
		   Lisp_Object *argv, int nargv,
		   Lisp_Object program, Lisp_Object cur_dir,
		   int separate_err)
{
  /* Synched up with sys_spawnve in FSF 20.6.  Significantly different
     but still synchable. */
  HANDLE hmyshove, hmyslurp, hmyslurp_err, hprocin, hprocout, hprocerr;
  Extbyte *command_line;
  BOOL do_io, windowed;
  Extbyte *proc_env;

  /* No need to DOS-ize the filename; expand-file-name (called prior)
     already does this. */

  /* Find out whether the application is windowed or not */
  {
    /* SHGetFileInfo tends to return ERROR_FILE_NOT_FOUND on most
       errors. This leads to bogus error message. */
    DWORD image_type;
    if (mswindows_is_executable (XSTRING_DATA (program)))
      {
	Extbyte *progext;
	LISP_STRING_TO_TSTR (program, progext);
	image_type = qxeSHGetFileInfo (progext, 0, NULL, 0, SHGFI_EXETYPE);
      }
    else
      {
	DECLARE_EISTRING (progext);
	eicpy_lstr (progext, program);
	eicat_c (progext, ".exe");
	eito_external (progext, Qmswindows_tstr);
	image_type = qxeSHGetFileInfo (eiextdata (progext), 0, NULL, 0,
				       SHGFI_EXETYPE);
      }
    if (image_type == 0)
      mswindows_report_process_error
	("Determining executable file type",
	 program,
	 GetLastError () == ERROR_FILE_NOT_FOUND
	 ? ERROR_BAD_FORMAT : GetLastError ());
    windowed = HIWORD (image_type) != 0;
  }


  /* Decide whether to do I/O on process handles, or just mark the
     process exited immediately upon successful launching. We do I/O if the
     process is a console one, or if it is windowed but windowed_process_io
     is non-zero */
  do_io = !windowed || windowed_process_io ;

  if (do_io)
    {
      /* Create two unidirectional named pipes */
      HANDLE htmp;
      SECURITY_ATTRIBUTES sa;

      sa.nLength = sizeof (sa);
      sa.bInheritHandle = TRUE;
      sa.lpSecurityDescriptor = NULL;

      CreatePipe (&hprocin, &hmyshove, &sa, 0);
      CreatePipe (&hmyslurp, &hprocout, &sa, 0);

      if (separate_err)
	CreatePipe (&hmyslurp_err, &hprocerr, &sa, 0);
      else
	/* Duplicate the stdout handle for use as stderr */
	DuplicateHandle(GetCurrentProcess(), hprocout, GetCurrentProcess(),
			&hprocerr, 0, TRUE, DUPLICATE_SAME_ACCESS);

      /* Stupid Win32 allows to create a pipe with *both* ends either
	 inheritable or not. We need process ends inheritable, and local
	 ends not inheritable. */
      DuplicateHandle (GetCurrentProcess(), hmyshove, GetCurrentProcess(),
		       &htmp, 0, FALSE,
		       DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
      hmyshove = htmp;
      DuplicateHandle (GetCurrentProcess(), hmyslurp, GetCurrentProcess(),
		       &htmp, 0, FALSE,
		       DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
      hmyslurp = htmp;
      if (separate_err)
	{
	  DuplicateHandle (GetCurrentProcess(), hmyslurp_err,
			   GetCurrentProcess(), &htmp, 0, FALSE,
			   DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
	  hmyslurp_err = htmp;
	}
    }

  /* Convert an argv vector into Win32 style command line by a call to
     lisp function `mswindows-construct-process-command-line'
     (in win32-native.el) */
  {
    int i;
    Lisp_Object args_or_ret = Qnil;
    struct gcpro gcpro1;

    GCPRO1 (args_or_ret);

    for (i = 0; i < nargv; ++i)
      args_or_ret = Fcons (*argv++, args_or_ret);
    args_or_ret = Fnreverse (args_or_ret);
    args_or_ret = Fcons (program, args_or_ret);

    args_or_ret = call1 (Qmswindows_construct_process_command_line,
			 args_or_ret);

    if (!STRINGP (args_or_ret))
      /* Luser wrote his/her own clever version */
      invalid_argument
	("Bogus return value from `mswindows-construct-process-command-line'",
	 args_or_ret);

    LISP_STRING_TO_TSTR (args_or_ret, command_line);

    UNGCPRO; /* args_or_ret */
  }

  /* Set `proc_env' to a nul-separated array of the strings in
     Vprocess_environment terminated by 2 nuls.  */

  {
    Ibyte **env;
    REGISTER Lisp_Object tem;
    REGISTER Ibyte **new_env;
    REGISTER int new_length = 0, i;

    for (tem = Vprocess_environment;
 	 (CONSP (tem)
 	  && STRINGP (XCAR (tem)));
 	 tem = XCDR (tem))
      new_length++;

    /* FSF adds an extra env var to hold the current process ID of the
       Emacs process.  Apparently this is used only by emacsserver.c,
       which we have superseded by gnuserv.c. (#### Does it work under
       MS Windows?)

       sprintf (ppid_env_var_buffer, "EM_PARENT_PROCESS_ID=%d",
         GetCurrentProcessId ());
       arglen += strlen (ppid_env_var_buffer) + 1;
       numenv++;
    */

    /* new_length + 1 to include terminating 0.  */
    env = new_env = alloca_array (Ibyte *, new_length + 1);

    /* Copy the Vprocess_environment strings into new_env.  */
    for (tem = Vprocess_environment;
 	 (CONSP (tem)
 	  && STRINGP (XCAR (tem)));
 	 tem = XCDR (tem))
      {
	Ibyte **ep = env;
	Ibyte *string = XSTRING_DATA (XCAR (tem));
	/* See if this string duplicates any string already in the env.
	   If so, don't put it in.
	   When an env var has multiple definitions,
	   we keep the definition that comes first in process-environment.  */
	for (; ep != new_env; ep++)
	  {
	    Ibyte *p = *ep, *q = string;
	    while (1)
	      {
		if (*q == 0)
		  /* The string is malformed; might as well drop it.  */
		  goto duplicate;
		if (*q != *p)
		  break;
		if (*q == '=')
		  goto duplicate;
		p++, q++;
	      }
	  }
	*new_env++ = string;
      duplicate: ;
      }
    *new_env = 0;

    /* Sort the environment variables */
    new_length = new_env - env;
    qsort (env, new_length, sizeof (Ibyte *), mswindows_compare_env);

    {
      DECLARE_EISTRING (envout);

      for (i = 0; i < new_length; i++)
	{
	  eicat_raw (envout, env[i], qxestrlen (env[i]));
	  eicat_raw (envout, (Ibyte *) "\0", 1);
	}

      eicat_raw (envout, (Ibyte *) "\0", 1);
      eito_external (envout, Qmswindows_tstr);
      proc_env = eiextdata (envout);
    }
  }

#if 0
    /* #### we need to port this. */
    /* On Windows 95, if cmdname is a DOS app, we invoke a helper
       application to start it by specifying the helper app as cmdname,
       while leaving the real app name as argv[0].  */
    if (is_dos_app)
      {
	cmdname = (Ibyte *) ALLOCA (PATH_MAX);
	if (egetenv ("CMDPROXY"))
	  qxestrcpy (cmdname, egetenv ("CMDPROXY"));
	else
	  {
	    qxestrcpy (cmdname, XSTRING_DATA (Vinvocation_directory));
	    qxestrcat (cmdname, (Ibyte *) "cmdproxy.exe");
	  }
      }
#endif

  /* Create process */
  {
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    DWORD err;
    DWORD flags;

    xzero (si);
    si.dwFlags = STARTF_USESHOWWINDOW;
    si.wShowWindow = windowed ? SW_SHOWNORMAL : SW_HIDE;
    if (do_io)
      {
	si.hStdInput = hprocin;
	si.hStdOutput = hprocout;
	si.hStdError = hprocerr;
	si.dwFlags |= STARTF_USESTDHANDLES;
      }

    flags = CREATE_SUSPENDED;
    if (mswindows_windows9x_p)
      flags |= (!NILP (Vmswindows_start_process_share_console)
		? CREATE_NEW_PROCESS_GROUP
		: CREATE_NEW_CONSOLE);
    else
      flags |= CREATE_NEW_CONSOLE | CREATE_NEW_PROCESS_GROUP;
    if (NILP (Vmswindows_start_process_inherit_error_mode))
      flags |= CREATE_DEFAULT_ERROR_MODE;

    ensure_console_window_exists ();

    {
      Extbyte *curdirext;

      LISP_STRING_TO_TSTR (cur_dir, curdirext);

      err = (qxeCreateProcess (NULL, command_line, NULL, NULL, TRUE,
			       (XEUNICODE_P ?
				flags | CREATE_UNICODE_ENVIRONMENT :
				flags), proc_env,
			       curdirext, &si, &pi)
	     ? 0 : GetLastError ());
    }

    if (do_io)
      {
	/* These just have been inherited; we do not need a copy */
	CloseHandle (hprocin);
	CloseHandle (hprocout);
	CloseHandle (hprocerr);
      }

    /* Handle process creation failure */
    if (err)
      {
	if (do_io)
	  {
	    CloseHandle (hmyshove);
	    CloseHandle (hmyslurp);
	    if (separate_err)
	      CloseHandle (hmyslurp_err);
	  }
	mswindows_report_process_error
	  ("Error starting",
	   program, GetLastError ());
      }

    /* The process started successfully */
    if (do_io)
      {
	NT_DATA(p)->h_process = pi.hProcess;
	NT_DATA(p)->dwProcessId = pi.dwProcessId;
	init_process_io_handles (p, (void *) hmyslurp, (void *) hmyshove,
				 separate_err ? (void *) hmyslurp_err
				 : (void *) -1, 0);
      }
    else
      {
	/* Indicate as if the process has exited immediately. */
	p->status_symbol = Qexit;
	CloseHandle (pi.hProcess);
      }

    if (!windowed)
      enable_child_signals (pi.hProcess);

    ResumeThread (pi.hThread);
    CloseHandle (pi.hThread);

    return ((int)pi.dwProcessId);
  }
}

/*
 * This method is called to update status fields of the process
 * structure. If the process has not existed, this method is expected
 * to do nothing.
 *
 * The method is called only for real child processes.
 */

static void
nt_update_status_if_terminated (Lisp_Process *p)
{
  DWORD exit_code;
  if (GetExitCodeProcess (NT_DATA(p)->h_process, &exit_code)
      && exit_code != STILL_ACTIVE)
    {
      p->tick++;
      p->core_dumped = 0;
      /* The exit code can be a code returned by process, or an
	 NTSTATUS value. We cannot accurately handle the latter since
	 it is a full 32 bit integer */
      if (exit_code & 0xC0000000)
	{
	  p->status_symbol = Qsignal;
	  p->exit_code = exit_code & 0x1FFFFFFF;
	}
      else
	{
	  p->status_symbol = Qexit;
	  p->exit_code = exit_code;
	}
    }
}

/*
 * Stuff the entire contents of LSTREAM to the process output pipe
 */

/* #### If only this function could be somehow merged with
   unix_send_process... */

static void
nt_send_process (Lisp_Object proc, struct lstream *lstream)
{
  volatile Lisp_Object vol_proc = proc;
  Lisp_Process *volatile p = XPROCESS (proc);

  /* use a reasonable-sized buffer (somewhere around the size of the
     stream buffer) so as to avoid inundating the stream with blocked
     data. */
  Ibyte chunkbuf[512];
  Bytecount chunklen;

  while (1)
    {
      int writeret;

      chunklen = Lstream_read (lstream, chunkbuf, 512);
      if (chunklen <= 0)
	break; /* perhaps should abort() if < 0?
		  This should never happen. */

      /* Lstream_write() will never successfully write less than the
	 amount sent in.  In the worst case, it just buffers the
	 unwritten data. */
      writeret = Lstream_write (XLSTREAM (DATA_OUTSTREAM (p)), chunkbuf,
				chunklen);
      Lstream_flush (XLSTREAM (DATA_OUTSTREAM (p)));
      if (writeret < 0)
	{
	  p->status_symbol = Qexit;
	  p->exit_code = ERROR_BROKEN_PIPE;
	  p->core_dumped = 0;
	  p->tick++;
	  process_tick++;
	  deactivate_process (*((Lisp_Object *) (&vol_proc)));
	  invalid_operation ("Broken pipe error sending to process; closed it",
			     p->name);
	}

      {
	int wait_ms = 25;
	while (Lstream_was_blocked_p (XLSTREAM (p->pipe_outstream)))
	  {
	    /* Buffer is full.  Wait, accepting input; that may allow
	       the program to finish doing output and read more.  */
	    Faccept_process_output (Qnil, Qzero, make_int (wait_ms));
	    Lstream_flush (XLSTREAM (p->pipe_outstream));
	    wait_ms = min (1000, 2 * wait_ms);
	  }
      }
    }
}

/*
 * Send a signal number SIGNO to PROCESS.
 * CURRENT_GROUP means send to the process group that currently owns
 * the terminal being used to communicate with PROCESS.
 * This is used for various commands in shell mode.
 * If NOMSG is zero, insert signal-announcements into process's buffers
 * right away.
 *
 * If we can, we try to signal PROCESS by sending control characters
 * down the pty.  This allows us to signal inferiors who have changed
 * their uid, for which killpg would return an EPERM error.
 *
 * The method signals an error if the given SIGNO is not valid
 */

static void
nt_kill_child_process (Lisp_Object proc, int signo,
		       int UNUSED (current_group), int UNUSED (nomsg))
{
  Lisp_Process *p = XPROCESS (proc);

  /* Signal error if SIGNO cannot be sent */
  validate_signal_number (signo);

  /* Send signal */
  if (!send_signal (NT_DATA (p), 0, signo))
    invalid_operation ("Cannot send signal to process", proc);
}

/*
 * Kill any process in the system given its PID
 *
 * Returns zero if a signal successfully sent, or
 * negative number upon failure
 */
static int
nt_kill_process_by_pid (int pid, int signo)
{
  struct Lisp_Process *p;

  /* Signal error if SIGNO cannot be sent */
  validate_signal_number (signo);

  p = find_process_from_pid (pid);
  return send_signal (p ? NT_DATA (p) : 0, pid, signo) ? 0 : -1;
}

/*-----------------------------------------------------------------------*/
/* Sockets connections							 */
/*-----------------------------------------------------------------------*/
#ifdef HAVE_SOCKETS

/* #### Hey MS, how long Winsock 2 for '95 will be in beta? */

#define SOCK_TIMER_ID 666
#define XM_SOCKREPLY (WM_USER + 666)

/* Return 0 for success, or error code */

static int
get_internet_address (Lisp_Object host, struct sockaddr_in *address)
{
  Char_Binary buf[MAXGETHOSTSTRUCT];
  HWND hwnd;
  HANDLE hasync;
  int errcode = 0;

  address->sin_family = AF_INET;

  /* First check if HOST is already a numeric address */
  {
    Extbyte *hostext;
    unsigned long inaddr;

    LISP_STRING_TO_EXTERNAL (host, hostext, Qmswindows_host_name_encoding);
    inaddr = inet_addr (hostext);
    if (inaddr != INADDR_NONE)
      {
	address->sin_addr.s_addr = inaddr;
	return 0;
      }
  }

  /* Create a window which will receive completion messages */
  hwnd = qxeCreateWindow (XETEXT ("STATIC"), NULL, WS_OVERLAPPED, 0, 0, 1, 1,
			  NULL, NULL, NULL, NULL);
  assert (hwnd);

  /* Post name resolution request */
  {
    Extbyte *hostext;

    LISP_STRING_TO_EXTERNAL (host, hostext, Qmswindows_host_name_encoding);

    hasync = WSAAsyncGetHostByName (hwnd, XM_SOCKREPLY, hostext,
				    buf, sizeof (buf));
    if (hasync == NULL)
      {
	errcode = WSAGetLastError ();
	goto done;
      }
  }

  /* Set a timer to poll for quit every 250 ms */
  SetTimer (hwnd, SOCK_TIMER_ID, 250, NULL);

  while (1)
    {
      MSG msg;
      qxeGetMessage (&msg, hwnd, 0, 0);
      if (msg.message == XM_SOCKREPLY)
	{
	  /* Ok, got an answer */
	  errcode = WSAGETASYNCERROR (msg.lParam);
	  goto done;
	}
      else if (msg.message == WM_TIMER && msg.wParam == SOCK_TIMER_ID)
	{
	  if (QUITP)
	    {
	      WSACancelAsyncRequest (hasync);
	      KillTimer (hwnd, SOCK_TIMER_ID);
	      DestroyWindow (hwnd);
	      QUIT;
	    }
	}
      qxeDispatchMessage (&msg);
    }

 done:
  KillTimer (hwnd, SOCK_TIMER_ID);
  DestroyWindow (hwnd);
  if (!errcode)
    {
      /* BUF starts with struct hostent */
      struct hostent *he = (struct hostent *) buf;
      address->sin_addr.s_addr = * (unsigned long *) he->h_addr_list[0];
    }
  return errcode;
}

static Lisp_Object
nt_canonicalize_host_name (Lisp_Object host)
{
  struct sockaddr_in address;

  if (get_internet_address (host, &address)) /* error */
    return host;

  if (address.sin_family == AF_INET)
    return build_string (inet_ntoa (address.sin_addr));
  else
    return host;
}

/* open a TCP network connection to a given HOST/SERVICE.  Treated
   exactly like a normal process when reading and writing.  Only
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   deactivate and close it via delete-process */

static void
nt_open_network_stream (Lisp_Object name, Lisp_Object host,
			Lisp_Object service,
			Lisp_Object protocol, void **vinfd, void **voutfd)
{
  struct sockaddr_in address;
  SOCKET s;
  int port;
  int retval;
  int errnum;

  CHECK_STRING (host);

  if (!EQ (protocol, Qtcp))
    invalid_constant ("Unsupported protocol", protocol);

  if (INTP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      struct servent *svc_info;
      Extbyte *servext;

      CHECK_STRING (service);
      LISP_STRING_TO_EXTERNAL (service, servext,
			       Qmswindows_service_name_encoding);

      svc_info = getservbyname (servext, "tcp");
      if (svc_info == 0)
	invalid_argument ("Unknown service", service);
      port = svc_info->s_port;
    }

  retval = get_internet_address (host, &address);
  if (retval)
    mswindows_report_winsock_error ("Getting IP address", host,
				    retval);
  address.sin_port = port;

  s = socket (address.sin_family, SOCK_STREAM, 0);
  if (s < 0)
    mswindows_report_winsock_error ("Creating socket", name,
				    WSAGetLastError ());

  /* We don't want to be blocked on connect */
  {
    unsigned long nonblock = 1;
    ioctlsocket (s, FIONBIO, &nonblock);
  }

  retval = connect (s, (struct sockaddr *) &address, sizeof (address));
  if (retval != NO_ERROR && WSAGetLastError() != WSAEWOULDBLOCK)
    {
      errnum = WSAGetLastError ();
      goto connect_failed;
    }

#if 0 /* PUTA! I thought getsockopt() was failing, so I created the
	 following based on the code in get_internet_address(), but
	 it was my own fault down below.  Both versions should work. */
  /* Wait while connection is established */
  {
    HWND hwnd;

  /* Create a window which will receive completion messages */
    hwnd = qxeCreateWindow (XETEXT ("STATIC"), NULL, WS_OVERLAPPED, 0, 0, 1, 1,
			    NULL, NULL, NULL, NULL);
    assert (hwnd);

    /* Post request */
    if (WSAAsyncSelect (s, hwnd, XM_SOCKREPLY, FD_CONNECT))
      {
	errnum = WSAGetLastError ();
	goto done;
      }

    /* Set a timer to poll for quit every 250 ms */
    SetTimer (hwnd, SOCK_TIMER_ID, 250, NULL);

    while (1)
      {
	MSG msg;
	GetMessage (&msg, hwnd, 0, 0);
	if (msg.message == XM_SOCKREPLY)
	  {
	    /* Ok, got an answer */
	    errnum = WSAGETASYNCERROR (msg.lParam);
	    goto done;
	  }

	else if (msg.message == WM_TIMER && msg.wParam == SOCK_TIMER_ID)
	  {
	    if (QUITP)
	      {
		WSAAsyncSelect (s, hwnd, XM_SOCKREPLY, 0);
		KillTimer (hwnd, SOCK_TIMER_ID);
		DestroyWindow (hwnd);
		QUIT;
	      }
	  }
	DispatchMessage (&msg);
      }

  done:
    WSAAsyncSelect (s, hwnd, XM_SOCKREPLY, 0);
    KillTimer (hwnd, SOCK_TIMER_ID);
    DestroyWindow (hwnd);
    if (errnum)
      goto connect_failed;
  }

#else
  while (1)
    {
      fd_set fdwriteset, fdexceptset;
      struct timeval tv;
      int nsel;

      if (QUITP)
	{
	  closesocket (s);
	  QUIT;
	}

      /* Poll for quit every 250 ms */
      tv.tv_sec = 0;
      tv.tv_usec = 250 * 1000;

      FD_ZERO (&fdwriteset);
      FD_SET (s, &fdwriteset);
      FD_ZERO (&fdexceptset);
      FD_SET (s, &fdexceptset);
      nsel = select (0, NULL, &fdwriteset, &fdexceptset, &tv);

      if (nsel == SOCKET_ERROR)
	{
	  errnum = WSAGetLastError ();
	  goto connect_failed;
	}

      if (nsel > 0)
	{
	  /* Check: was connection successful or not? */
	  if (FD_ISSET (s, &fdwriteset))
	    break;
	  else if (FD_ISSET (s, &fdexceptset))
	    {
	      int store_me_harder = sizeof (errnum);
	      /* OK, we finally can get the REAL error code.  Any paths
		 in this code that lead to a call of WSAGetLastError()
		 indicate probable logic failure. */
	      if (getsockopt (s, SOL_SOCKET, SO_ERROR, (char *) &errnum,
			      &store_me_harder))
		errnum = WSAGetLastError ();
	      goto connect_failed;
	    }
	  else
	    {
	      signal_error (Qinternal_error,
			    "Porra, esse caralho de um sistema de operacao",
			    Qunbound);
	      break;
	    }
	}
    }
#endif

  /* We are connected at this point */
  *vinfd = (void *)s;
  DuplicateHandle (GetCurrentProcess(), (HANDLE)s,
		   GetCurrentProcess(), (LPHANDLE)voutfd,
		   0, FALSE, DUPLICATE_SAME_ACCESS);
  return;

 connect_failed:
  {
    closesocket (s);
    mswindows_report_winsock_error ("Connection failed",
				    list3 (Qunbound, host, service),
				    errnum);
  }
}

#endif


DEFUN ("mswindows-set-process-priority", Fmswindows_set_process_priority, 2, 2, "", /*
  Set the priority of PROCESS to PRIORITY.
If PROCESS is nil, the priority of Emacs is changed, otherwise the
priority of the process whose pid is PROCESS is changed.
PRIORITY should be one of the symbols high, normal, or low;
any other symbol will be interpreted as normal.

If successful, the return value is t, otherwise nil.
*/
       (process, priority))
{
  HANDLE proc_handle = GetCurrentProcess ();
  DWORD  priority_class = NORMAL_PRIORITY_CLASS;
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (priority);

  if (!NILP (process))
    {
      DWORD pid;
      struct Lisp_Process *p = 0;

      if (PROCESSP (process))
	{
	  CHECK_LIVE_PROCESS (process);
	  p = XPROCESS (process);
	  pid = NT_DATA (p)->dwProcessId;
	}
      else
	{
	  CHECK_INT (process);

	  /* Allow pid to be an internally generated one, or one obtained
	     externally.  This is necessary because real pids on Win95 are
	     negative.  */

	  pid = XINT (process);
	  p = find_process_from_pid (pid);
	  if (p != NULL)
	    pid = NT_DATA (p)->dwProcessId;
	}

      /* #### Should we be using the existing process handle from NT_DATA(p)?
	 Will we fail if we open it a second time? */
      proc_handle = OpenProcess (PROCESS_SET_INFORMATION, FALSE, pid);
    }

  if (EQ (priority, Qhigh))
    priority_class = HIGH_PRIORITY_CLASS;
  else if (EQ (priority, Qlow))
    priority_class = IDLE_PRIORITY_CLASS;

  if (proc_handle != NULL)
    {
      if (SetPriorityClass (proc_handle, priority_class))
	result = Qt;
      if (!NILP (process))
	CloseHandle (proc_handle);
    }

  return result;
}


/*-----------------------------------------------------------------------*/
/* Initialization							 */
/*-----------------------------------------------------------------------*/

void
process_type_create_nt (void)
{
  PROCESS_HAS_METHOD (nt, alloc_process_data);
  PROCESS_HAS_METHOD (nt, finalize_process_data);
  PROCESS_HAS_METHOD (nt, init_process);
  PROCESS_HAS_METHOD (nt, create_process);
  PROCESS_HAS_METHOD (nt, update_status_if_terminated);
  PROCESS_HAS_METHOD (nt, send_process);
  PROCESS_HAS_METHOD (nt, kill_child_process);
  PROCESS_HAS_METHOD (nt, kill_process_by_pid);
#ifdef HAVE_SOCKETS
  PROCESS_HAS_METHOD (nt, canonicalize_host_name);
  PROCESS_HAS_METHOD (nt, open_network_stream);
#ifdef HAVE_MULTICAST
#error I won't do this until '95 has winsock2
  PROCESS_HAS_METHOD (nt, open_multicast_group);
#endif
#endif
}

void
syms_of_process_nt (void)
{
  DEFSUBR (Fmswindows_set_process_priority);
  DEFSYMBOL (Qmswindows_construct_process_command_line);
}

void
vars_of_process_nt (void)
{
  DEFVAR_LISP ("mswindows-start-process-share-console",
	       &Vmswindows_start_process_share_console /*
When nil, new child processes are given a new console.
When non-nil, they share the Emacs console; this has the limitation of
allowing only one DOS subprocess to run at a time (whether started directly
or indirectly by Emacs), and preventing Emacs from cleanly terminating the
subprocess group, but may allow Emacs to interrupt a subprocess that doesn't
otherwise respond to interrupts from Emacs.
*/ );
  Vmswindows_start_process_share_console = Qnil;

  DEFVAR_LISP ("mswindows-start-process-inherit-error-mode",
	       &Vmswindows_start_process_inherit_error_mode /*
    "When nil, new child processes revert to the default error mode.
When non-nil, they inherit their error mode setting from Emacs, which stops
them blocking when trying to access unmounted drives etc.
*/ );
  Vmswindows_start_process_inherit_error_mode = Qt;
}
