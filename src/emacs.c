/* XEmacs -- Fully extensible Emacs, running on Unix and other platforms.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Synched up with: Mule 2.0, FSF 19.28. */

/* Note: It is necessary to specify <config.h> and not "config.h" in
   order for the --srcdir type of compilation to work properly.
   Otherwise the config.h from the srcdir, rather than the one from
   the build dir, will be used. */

#include <config.h>
#include "lisp.h"

#include "backtrace.h" /* run-emacs-from-temacs needs this */
#include "buffer.h"
#include "commands.h"
#include "console.h"
#include "process.h"
#include "sysdep.h"

#include <setjmp.h>
#include "syssignal.h" /* Always include before systty.h */
#include "systty.h"
#include "sysfile.h"
#include "systime.h"

#if defined (HAVE_LOCALE_H) && \
   (defined (I18N2) || defined (I18N3) || defined (I18N4))
#include <locale.h>
#endif

#ifdef TOOLTALK
#include <tt_c.h>
#endif

#ifdef VMS
#include <ssdef.h>
#endif

#ifdef APOLLO
#ifndef APOLLO_SR10
#include <default_acl.h>
#endif
#endif

extern void memory_warnings (void *, void (*warnfun) (CONST char *));

#ifndef SYSTEM_MALLOC
extern void *(*__malloc_hook)(size_t);
extern void *(*__realloc_hook)(void *, size_t);
extern void (*__free_hook)(void *);
#endif  /* not SYSTEM_MALLOC */

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* Set nonzero after XEmacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

/* Variable whose value is symbol giving operating system type. */
Lisp_Object Vsystem_type;

/* Variable whose value is string giving configuration built for.  */
Lisp_Object Vsystem_configuration;
  
/* The name under which XEmacs was invoked, with any leading directory
   names discarded.  */
Lisp_Object Vinvocation_name;

/* The directory name from which XEmacs was invoked.  */
Lisp_Object Vinvocation_directory;

#if 0 /* FSFmacs */
/* The directory name in which to find subdirs such as lisp and etc.
   nil means get them only from PATH_LOADSEARCH.  */
Lisp_Object Vinstallation_directory;
#endif

/* If nonzero, set XEmacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority. */
int emacs_priority;

/* If non-zero a filter or a sentinel is running.  Tested to save the match
   data on the first attempt to change it inside asynchronous code. */
int running_asynch_code;

/* If non-zero, a window-system was specified on the command line. */
int display_arg;

/* Type of display specified.  We cannot use a Lisp symbol here because
   Lisp symbols may not initialized at the time that we set this
   variable. */
CONST char *display_use;

/* If non-zero, then the early error handler will only print the error
   message and exit. */
int suppress_early_backtrace;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#ifdef USG_SHARED_LIBRARIES
/* If nonzero, this is the place to put the end of the writable segment
   at startup.  */

unsigned int bss_end = 0;
#endif

/* Number of bytes of writable memory we can expect to be able to get */
unsigned int lim_data;

/* Nonzero means running XEmacs without interactive terminal.  */

int noninteractive;

/* Value of Lisp variable `noninteractive'.
   Normally same as C variable `noninteractive'
   but nothing terrible happens if user sets this one.  */

int noninteractive1;

/* Save argv and argc.  */
char **initial_argv;
int initial_argc;

static void sort_args (int argc, char **argv);

extern int always_gc;           /* hack */

Lisp_Object Qkill_emacs_hook;
Lisp_Object Qsave_buffers_kill_emacs;


/* Signal code for the fatal signal that was received */
static int fatal_error_code;

/* Nonzero if handling a fatal error already */
static int fatal_error_in_progress;

static void shut_down_emacs (int sig, Lisp_Object stuff);

/* Handle bus errors, illegal instruction, etc. */
SIGTYPE
fatal_error_signal (int sig)
{
  fatal_error_code = sig;
  signal (sig, SIG_DFL);
  /* Unblock the signal so that if the same signal gets sent in the
     code below, we avoid a deadlock. */
  EMACS_UNBLOCK_SIGNAL (fatal_error_code);

  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (! fatal_error_in_progress)
    {
      fatal_error_in_progress = 1;
      shut_down_emacs (sig, Qnil);
      stderr_out("\nLisp backtrace follows:\n\n");
      Fbacktrace(Qexternal_debugging_output, Qt);
      
      /* Check for Sun-style stack printing via /proc */
      {
        CONST char *pstack = "/usr/proc/bin/pstack";
        if (access(pstack, X_OK) == 0)
          {
            char buf[100];
            stderr_out("\nC backtrace follows:\n"
                       "(A real debugger may provide better information)\n\n");
            sprintf(buf, "%s %d >&2", pstack, (int)getpid());
            system(buf);
          }
      }
    }
#ifdef VMS
  LIB$STOP (SS$_ABORT);
#else
  /* Signal the same code; this time it will really be fatal. */
  kill (getpid (), fatal_error_code);
#endif /* not VMS */
  SIGRETURN;
}


DOESNT_RETURN
fatal (CONST char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  fprintf (stderr, "\nXEmacs: ");
  vfprintf (stderr, GETTEXT (fmt), args);
  fprintf (stderr, "\n");

  va_end (args);
  fflush (stderr);
  exit (1);
}

/* #### The following two functions should be replaced with
   calls to emacs_doprnt_*() functions, with STREAM set to send out
   to stdout or stderr.  This is the only way to ensure that
   I18N3 works properly (many implementations of the *printf()
   functions, including the ones included in glibc, do not implement
   the %###$ argument-positioning syntax). */

/* exactly equivalent to fprintf (stderr, fmt, ...) except that it calls
   GETTEXT on the format string. */

int
stderr_out (CONST char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);

  retval = vfprintf (stderr, GETTEXT (fmt), args);

  va_end (args);
  /* fflush (stderr); */
  return retval;
}

/* exactly equivalent to fprintf (stdout, fmt, ...) except that it calls
   GETTEXT on the format string. */

int
stdout_out (CONST char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);

  retval = vfprintf (stdout, GETTEXT (fmt), args);

  va_end (args);
  return retval;
}

#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
SIGTYPE
memory_warning_signal (int sig)
{
  /* #### bad bad bad; this function shouldn't do anything except
     set a flag, or weird corruption could happen. */
  signal (sig, memory_warning_signal);

  malloc_warning
    (GETTEXT ("Operating system warns that virtual memory is running low.\n"));

  /* It might be unsafe to call do_auto_save now.  */
  force_auto_save_soon ();
}
#endif /* SIGDANGER */

/* Code for dealing with Lisp access to the Unix command line */

static Lisp_Object
make_arg_list_1 (int argc, char **argv, int skip_args)
{
  Lisp_Object result = Qnil;
  REGISTER int i;

  for (i = argc - 1; i >= 0; i--)
    {
      if (i == 0 || i > skip_args)
	result = Fcons (build_ext_string (argv [i], FORMAT_FILENAME), result);
    }
  return result;
}

Lisp_Object
make_arg_list (int argc, char **argv)
{
  return make_arg_list_1 (argc, argv, 0);
}

/* Calling functions are also responsible for calling free_argc_argv
   when they are done with the generated list. */
void
make_argc_argv (Lisp_Object argv_list, int *argc, char ***argv)
{
  Lisp_Object next;
  int n = XINT (Flength (argv_list));
  REGISTER int i;
  *argv = (char**) xmalloc ((n+1) * sizeof (char*));

  for (i = 0, next = argv_list; i < n; i++, next = XCDR (next))
    {
      CONST char *temp;
      CHECK_STRING (XCAR (next));
      
      GET_C_STRING_EXT_DATA_ALLOCA (XCAR (next), FORMAT_OS, temp);
      (*argv) [i] = xstrdup (temp);
    }
  (*argv) [n] = 0;
  *argc = i;
}

void
free_argc_argv (char **argv)
{
  int elt = 0;

  while (argv[elt])
    {
      xfree (argv[elt]);
      elt++;
    }
  xfree (argv);
}

static void
init_cmdargs (int argc, char **argv, int skip_args)
{
  initial_argv = argv;
  initial_argc = argc;

  Vcommand_line_args = make_arg_list_1 (argc, argv, skip_args);
}

DEFUN ("invocation-name", Finvocation_name, 0, 0, 0, /*
Return the program name that was used to run XEmacs.
Any directory names are omitted.
*/
       ())
{
  return Fcopy_sequence (Vinvocation_name);
}

DEFUN ("invocation-directory", Finvocation_directory, 0, 0, 0, /*
Return the directory name in which the Emacs executable was located.
*/
       ())
{
  return Fcopy_sequence (Vinvocation_directory);
}


#ifdef I18N4
				/* #### - don't know why I18N4 on SunOS/JLE
				   can't deal with this.  It's a potential
				   bug that needs to be looked at. */
# undef RUN_TIME_REMAP
#endif

#if defined (MULE) && defined (MSDOS) && defined (EMX)
/* Setup all of files be input/output'ed with binary translation mdoe. */
asm ("	.text");
asm ("L_setbinmode:");
asm ("	movl	$1, __fmode_bin");
asm ("	ret");
asm ("	.stabs	\"___CTOR_LIST__\", 23, 0, 0, L_setbinmode");
#endif

/* Test whether the next argument in ARGV matches SSTR or a prefix of
   LSTR (at least MINLEN characters).  If so, then if VALPTR is non-null
   (the argument is supposed to have a value) store in *VALPTR either
   the next argument or the portion of this one after the equal sign.
   ARGV is read starting at position *SKIPPTR; this index is advanced
   by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static int
argmatch (char **argv, int argc, char *sstr, char *lstr,
	  int minlen, char **valptr, int *skipptr)
{
  char *p;
  int arglen;
  char *arg;

  /* Don't access argv[argc]; give up in advance.  */
  if (argc <= *skipptr + 1)
    return 0;

  arg = argv[*skipptr+1];
  if (arg == NULL)
    return 0;
  if (strcmp (arg, sstr) == 0)
    {
      if (valptr != NULL)
	{
	  *valptr = argv[*skipptr+2];
	  *skipptr += 2;
	}
      else
	*skipptr += 1;
      return 1;
    }
  arglen = (valptr != NULL && (p = strchr (arg, '=')) != NULL
	    ? p - arg : strlen (arg));
  if (lstr == 0 || arglen < minlen || strncmp (arg, lstr, arglen) != 0)
    return 0;
  else if (valptr == NULL)
    {
      *skipptr += 1;
      return 1;
    }
  else if (p != NULL)
    {
      *valptr = p+1;
      *skipptr += 1;
      return 1;
    }
  else if (argv[*skipptr+2] != NULL)
    {
      *valptr = argv[*skipptr+2];
      *skipptr += 2;
      return 1;
    }
  else
    {
      return 0;
    }
}

static DOESNT_RETURN
main_1 (int argc, char **argv, char **envp)
{
  char stack_bottom_variable;
  int skip_args = 0;
  Lisp_Object load_me;
  int inhibit_window_system;

#ifndef SYSTEM_MALLOC
  /* Make sure that any libraries we link against haven't installed a 
     hook for a gmalloc of a potentially incompatible version. */
  __malloc_hook = NULL;
  __realloc_hook = NULL;
  __free_hook = NULL;
#endif /* not SYSTEM_MALLOC */

  noninteractive = 0;

#ifdef NeXT
  extern int malloc_cookie;
  
  /* 19-Jun-1995 -baw
   * NeXT secret magic, ripped from Emacs-for-NS by Carl Edman
   * <cedman@princeton.edu>.  Note that even Carl doesn't know what this
   * does; it was provided by NeXT, and it presumable makes NS's mallocator
   * work with dumping.  But malloc_jumpstart() and malloc_freezedry() in
   * unexnext.c are both completely undocumented, even in NS header files!
   * But hey, it solves all NS related memory problems, so who's
   * complaining? */
  if (initialized)
    if (malloc_jumpstart (malloc_cookie) != 0)
      printf ("malloc jumpstart failed!\n");
#endif /* NeXT */

#if defined (GNU_MALLOC) && defined (ERROR_CHECK_MALLOC)
#if 0
  if (!initialized)
    init_free_hook ();
#endif
#endif

  sort_args (argc, argv);

  /* Map in shared memory, if we are using that.  */
#ifdef HAVE_SHM
  if (argmatch (argv, argc, "-nl", "--no-shared-memory", 6, NULL, &skip_args))
    {
      map_in_data (0);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 1;
    }
  else
    {
      map_in_data (1);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 0;
    }
#endif /* HAVE_SHM */

#ifdef VMS
  /* If -map specified, map the data file in */
  if (argc > 2 && ! strcmp (argv[1], "-map"))
    {
      skip_args = 2;
      mapin_data (argv[2]);
    }

#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
  /* Bletcherous shared libraries! */
  if (!stdin)  stdin  = fdopen (0, "r");
  if (!stdout) stdout = fdopen (1, "w");
  if (!stderr) stderr = fdopen (2, "w");
  if (!environ) environ = envp;
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */
#if (defined (MSDOS) && defined (EMX)) || defined (WIN32) || defined (_SCO_DS)
  environ = envp;
#endif

  /* Record (approximately) where the stack begins.  */
  stack_bottom = &stack_bottom_variable;

#ifdef RUN_TIME_REMAP
  if (initialized)
    run_time_remap (argv[0]);
#endif

#ifdef USG_SHARED_LIBRARIES
  if (bss_end)
    brk ((void *) bss_end);
#endif

  clearerr (stdin);

#ifdef APOLLO
#ifndef APOLLO_SR10
  /* If USE_DOMAIN_ACLS environment variable exists,
     use ACLs rather than UNIX modes. */
  if (egetenv ("USE_DOMAIN_ACLS"))
    default_acl (USE_DEFACL);
#endif
#endif /* APOLLO */

#if defined (HAVE_MMAP) && defined (REL_ALLOC)
  /* ralloc can only be used if using the GNU memory allocator. */
  init_ralloc ();
#endif

#ifdef HAVE_SOCKS
  if (initialized)
    SOCKSinit (argv[0]);
#endif /* HAVE_SOCKS */

#ifndef SYSTEM_MALLOC
  if (!initialized)
    /* Arrange to get warning messages as memory fills up.  */
    memory_warnings (0, malloc_warning);
#endif	/* not SYSTEM_MALLOC */

#ifdef MSDOS
  /* We do all file input/output as binary files.  When we need to translate
     newlines, we do that manually.  */
  _fmode = O_BINARY;
  (stdin) ->_flag &= ~_IOTEXT;
  (stdout)->_flag &= ~_IOTEXT;
  (stderr)->_flag &= ~_IOTEXT;
#endif /* MSDOS */

#ifdef SET_EMACS_PRIORITY
  if (emacs_priority != 0)
    nice (-emacs_priority);
  setuid (getuid ());
#endif /* SET_EMACS_PRIORITY */

#ifdef EXTRA_INITIALIZE
  EXTRA_INITIALIZE;
#endif

#ifdef HAVE_WINDOW_SYSTEM
  inhibit_window_system = 0;
#else
  inhibit_window_system = 1;
#endif

  /* Handle the -t switch, which specifies filename to use as terminal */
  {
    char *term;
    if (argmatch (argv, argc, "-t", "--terminal", 4, &term, &skip_args))
      {
	close (0);
	close (1);
	if (open (term, O_RDWR, 2) < 0)
	  fatal ("%s: %s", term, strerror (errno));
	dup (0);
	if (! isatty (0))
	  fatal ("%s: not a tty", term);
 
	stderr_out ("Using %s", ttyname (0));
	inhibit_window_system = 1;	/* -t => -nw */
      }
  }

  /* Handle -nw switch */
  if (argmatch (argv, argc, "-nw", "--no-windows", 6, NULL, &skip_args))
    inhibit_window_system = 1;

  /* Handle the -batch switch, which means don't do interactive display.  */
  if (argmatch (argv, argc, "-batch", "--batch", 5, NULL, &skip_args))
    noninteractive = 1;

  /* Partially handle the -version and -help switches: they imply -batch,
     but are not removed from the list. */
  if (argmatch (argv, argc, "-help", "--help",   3, NULL, &skip_args) ||
      argmatch (argv, argc, "-?",     0,         2, NULL, &skip_args) ||
      argmatch (argv, argc, "-flags", "--flags", 6, NULL, &skip_args))
    noninteractive = 1, skip_args--;

  if (argmatch (argv, argc, "-version", "--version", 3, NULL, &skip_args) ||
      argmatch (argv, argc, "-V",    0,              2, NULL, &skip_args))
      noninteractive = 1, skip_args--;
  
  /* Now, figure out which type of console is our first console. */

  display_arg = 0;

  if (noninteractive)
    display_use = "stream";
  else
    display_use = "tty";

#ifndef HAVE_TTY
  if (inhibit_window_system)
    fatal ("Sorry, this XEmacs was not compiled with TTY support");
#endif

#ifdef HAVE_WINDOW_SYSTEM
  /* Stupid kludge to catch command-line display spec.  We can't
     handle this argument entirely in window-system-dependent code
     because we don't even know which window-system-dependent code
     to run until we've recognized this argument.  */
  if (!inhibit_window_system && !noninteractive)
    {
#ifdef HAVE_X_WINDOWS
      char *dpy = 0;
      int count_before = skip_args;
      
      if (argmatch (argv, argc, "-d", "--display", 3, &dpy, &skip_args) ||
	  argmatch (argv, argc, "-display", 0,     3, &dpy, &skip_args))
	{
	  display_arg = 1;
	  display_use = "x";
	}
      /* If we have the form --display=NAME,
	 convert it into  -d name.
	 This requires inserting a new element into argv.  */
      if (dpy != 0 && skip_args - count_before == 1)
	{
	  char **new = (char **) xmalloc (sizeof (char *) * (argc + 2));
	  int j;

	  for (j = 0; j < count_before + 1; j++)
	    new[j] = argv[j];
	  new[count_before + 1] = "-d";
	  new[count_before + 2] = dpy;
	  for (j = count_before + 2; j <argc; j++)
	    new[j + 1] = argv[j];
	  argv = new;
	  argc++;
	}
      /* Change --display to -d, when its arg is separate.  */
      else if (dpy != 0 && skip_args > count_before
	       && argv[count_before + 1][1] == '-')
	argv[count_before + 1] = "-d";

      /* Don't actually discard this arg.  */
      skip_args = count_before;

      /* If there is a non-empty environment var DISPLAY, set
         `display_use', but not `display_arg', which is only to be set
         if the display was specified on the command line. */
      if ((dpy = getenv ("DISPLAY")) && dpy[0])
	display_use = "x";
      
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NEXTSTEP
      if (argmatch (argv, argc, "-NXHost",     0, 6, 0, &skip_args) ||
	  argmatch (argv, argc, "-MachLaunch", 0, 9, 0, &skip_args))
	{
	  display_arg = 1;
	  display_use = "ns";
	}
#endif /* HAVE_NEXTSTEP */
    }
#endif /* HAVE_WINDOW_SYSTEM */

  noninteractive1 = noninteractive;

  /****** Now initialize everything *******/

  /* First, do really basic environment initialization -- catching signals
     and the like.  These functions have no dependence on any part of
     the Lisp engine and need to be done both at dump time and at run time. */

  init_signals_very_early ();
  init_data_very_early (); /* Catch math errors. */
#ifdef LISP_FLOAT_TYPE
  init_floatfns_very_early (); /* Catch floating-point math errors. */
#endif
  init_process_times_very_early (); /* Initialize our process timers.
				       As early as possible, of course,
				       so we can be fairly accurate. */
  init_intl_very_early (); /* set up the locale and domain for gettext and
			      such. */

  /* Now initialize the Lisp engine and the like.  Done only during
     dumping.  No dependence on anything that may be in the user's
     environment when the dumped XEmacs is run.

     We try to do things in an order that minimizes the non-obvious
     dependencies between functions. */

  if (!initialized)
    {
      /* Initialize things so that new Lisp objects
	 can be created and objects can be staticpro'd.
	 Must be basically the very first thing done
	 because pretty much all of the initialization
	 routines below create new objects. */
      init_alloc_once_early ();

      /* Initialize Qnil, Qt, Qunbound, and the
	 obarray.  After this, symbols can be
	 interned.  This depends on init_alloc_once(). */
      init_symbols_once_early ();

      /* Declare the basic symbols pertaining to errors,
	 So that deferror() can be called. */
      init_errors_once_early ();

      /* Make sure that opaque pointers can be created. */
      init_opaque_once_early ();

      /* Now declare all the symbols and define all the Lisp primitives.

	 The *only* thing that the syms_of_*() functions are allowed to do
	 is call one of the following three functions:

	 defsymbol()
	 defsubr() (i.e. DEFSUBR)
	 deferror()
	 defkeyword()

	 Order does not matter in these functions.
	 */

      syms_of_abbrev ();
      syms_of_alloc ();
      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_callproc ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_chartab ();
      syms_of_cmdloop ();
      syms_of_cmds ();
      syms_of_console ();
      syms_of_data ();
#ifdef DEBUG_XEMACS
      syms_of_debug ();
#endif /* DEBUG_XEMACS */
      syms_of_device ();
#ifdef HAVE_DIALOGS
      syms_of_dialog ();
#endif
      syms_of_dired ();
      syms_of_doc ();
      syms_of_editfns ();
      syms_of_elhash ();
      syms_of_emacs ();
      syms_of_eval ();
      syms_of_event_stream ();
      syms_of_events ();
      syms_of_extents ();
      syms_of_faces ();
      syms_of_fileio ();
#ifdef CLASH_DETECTION
      syms_of_filelock ();
#endif /* CLASH_DETECTION */
      syms_of_floatfns ();
      syms_of_fns ();
      syms_of_font_lock ();
      syms_of_frame ();
      syms_of_general ();
      syms_of_glyphs ();
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
      syms_of_gui ();
#endif
      syms_of_indent ();
      syms_of_intl ();
      syms_of_keymap ();
      syms_of_lread ();
      syms_of_macros ();
      syms_of_marker ();
      syms_of_md5 ();
#ifdef HAVE_DATABASE
      syms_of_dbm ();
#endif
#ifdef HAVE_MENUBARS
      syms_of_menubar ();
#endif
      syms_of_minibuf ();
#ifdef MOCKLISP_SUPPORT
      syms_of_mocklisp ();
#endif
      syms_of_objects ();
      syms_of_print ();
#if !defined (NO_SUBPROCESSES)
      syms_of_process ();
#endif
      syms_of_profile ();
#if defined (HAVE_MMAP) && defined (REL_ALLOC)
      syms_of_ralloc ();
#endif /* HAVE_MMAP && REL_ALLOC */
      syms_of_rangetab ();
      syms_of_redisplay ();
      syms_of_search ();
      syms_of_signal ();
      syms_of_sound ();
      syms_of_specifier ();
      syms_of_symbols ();
      syms_of_syntax ();
#ifdef HAVE_SCROLLBARS
      syms_of_scrollbar ();
#endif
#ifdef HAVE_TOOLBARS
      syms_of_toolbar ();
#endif
      syms_of_undo ();
      syms_of_window ();
#ifdef HAVE_TTY
      syms_of_console_tty ();
      syms_of_device_tty ();
      syms_of_objects_tty ();
#endif
#ifdef HAVE_X_WINDOWS
      syms_of_device_x ();
#ifdef HAVE_DIALOGS
      syms_of_dialog_x ();
#endif
      syms_of_event_Xt ();
      syms_of_frame_x ();
      syms_of_glyphs_x ();
      syms_of_objects_x ();
#ifdef HAVE_MENUBARS
      syms_of_menubar_x ();
#endif
      syms_of_xselect ();
#ifdef EPOCH
      syms_of_epoch ();
#endif
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
      syms_of_gui_x ();
#endif
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_NEXTSTEP
      syms_of_device_ns ();
      syms_of_frame_ns ();
      syms_of_glyphs_ns ();
      syms_of_objects_ns ();
#ifdef HAVE_MENUBARS
      syms_of_menubar_ns ();
#endif
      syms_of_nsselect ();
#ifdef HAVE_SCROLLBARS
      syms_of_scrollbar_ns ();
#endif
#endif /* HAVE_NEXTSTEP */

#ifdef MULE
      syms_of_mule ();
      syms_of_mule_ccl ();
      syms_of_mule_charset ();
      syms_of_mule_coding ();
#ifdef HAVE_WNN
      syms_of_mule_wnn ();
#endif
#ifdef HAVE_CANNA
      syms_of_mule_canna ();
#endif /* HAVE_CANNA */
#endif /* MULE */

#ifdef SYMS_SYSTEM
      SYMS_SYSTEM;
#endif

#ifdef SYMS_MACHINE
      SYMS_MACHINE;
#endif

#ifdef EMACS_BTL
      syms_of_btl (); 
#endif

#ifdef ENERGIZE
      syms_of_energize ();
#endif

#if defined (GNU_MALLOC) && defined (ERROR_CHECK_MALLOC)
#if 0
      syms_of_free_hook ();
#endif
#endif

#ifdef TOOLTALK
      syms_of_tooltalk ();
#endif

#ifdef SUNPRO
      syms_of_sunpro ();
#endif

      /* Now create the subtypes for the types that have them.
	 We do this before the vars_*() because more symbols
	 may get initialized here. */

      /* Now initialize the console types and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_CONSOLE_TYPE()
	 CONSOLE_HAS_METHOD()

	 For any given console type, the former macro must be called
	 before the any calls to the latter macro. */

      console_type_create ();

      console_type_create_stream ();

#ifdef HAVE_TTY
      console_type_create_tty ();
      console_type_create_device_tty ();
      console_type_create_frame_tty ();
      console_type_create_objects_tty ();
      console_type_create_redisplay_tty ();
#endif

#ifdef HAVE_X_WINDOWS
      console_type_create_x ();
      console_type_create_device_x ();
      console_type_create_frame_x ();
      console_type_create_glyphs_x ();
#ifdef HAVE_MENUBARS
      console_type_create_menubar_x ();
#endif
      console_type_create_objects_x ();
      console_type_create_redisplay_x ();
#ifdef HAVE_SCROLLBARS
      console_type_create_scrollbar_x ();
#endif
#ifdef HAVE_TOOLBARS
      console_type_create_toolbar_x ();
#endif
#endif /* HAVE_X_WINDOWS */

      /* Now initialize the specifier types and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_SPECIFIER_TYPE()
	 SPECIFIER_HAS_METHOD()

	 For any given specifier type, the former macro must be called
	 before the any calls to the latter macro. */

      specifier_type_create ();

      specifier_type_create_image ();
      specifier_type_create_objects ();
#ifdef HAVE_TOOLBARS
      specifier_type_create_toolbar ();
#endif

      /* Now initialize the structure types and associated symbols.
	 Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 define_structure_type()
	 define_structure_type_keyword()

	 */

      structure_type_create ();

      structure_type_create_chartab ();
      structure_type_create_faces ();
      structure_type_create_rangetab ();

      /* Now initialize the image instantiator formats and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_IMAGE_INSTANTIATOR_FORMAT()
	 IIFORMAT_HAS_METHOD()
	 IIFORMAT_VALID_KEYWORD()

	 For any given image instantiator format, the first macro must be
	 called before the any calls to the other macros. */

      image_instantiator_format_create ();
#ifdef HAVE_X_WINDOWS
      image_instantiator_format_create_glyphs_x ();
#endif /* HAVE_X_WINDOWS */

      /* Now initialize the lstream types and associated symbols.
	 Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 LSTREAM_HAS_METHOD()

	 */

      lstream_type_create ();
#ifdef MULE
      lstream_type_create_mule_coding ();
#endif
      lstream_type_create_print ();

      /* Now initialize most variables.

	 These functions may do exactly the following:

	 DEFVAR_INT()
	 DEFVAR_LISP()
	 DEFVAR_BOOL()
	 DEFER_GETTEXT()
	 Dynarr_*()
	 Blocktype_*()
	 staticpro()
	 Fprovide(symbol)
	 intern()
	 pure_put()
	 xmalloc()
	 defsymbol(), if it's absolutely necessary and you're sure that
	   the symbol isn't referenced anywhere else in the initialization
	   code
	 Fset() on a symbol that is unbound
	 assigning a symbol or constant value to a variable
	 using a global variable that has been initialized
	   earlier on in the same function

	 Any of the object-creating functions on alloc.c: e.g.

	 make_pure_*()
	 Fpurecopy()
	 make_string()
	 build_string()
	 make_vector()
	 make_int()
	 make_extent()
	 alloc_lcrecord()
	 Fcons()
	 listN()
	 make_opaque_ptr()
	 make_opaque_long()

	 perhaps a few others.
       */

      /* Now allow Fprovide() statements to be made. */
      init_provide_once ();

      vars_of_abbrev ();
      vars_of_alloc ();
      vars_of_buffer ();
      vars_of_bytecode ();
      vars_of_callint ();
      vars_of_callproc ();
      vars_of_cmdloop ();
      vars_of_cmds ();
      vars_of_console ();
      vars_of_data ();
#ifdef DEBUG_XEMACS
      vars_of_debug ();
#endif
      vars_of_console_stream ();
      vars_of_device ();
#ifdef HAVE_DIALOGS
      vars_of_dialog ();
#endif
      vars_of_dired ();
      vars_of_doc ();
      vars_of_editfns ();
      vars_of_elhash ();
      vars_of_emacs ();
      vars_of_eval ();
      vars_of_event_stream ();
      vars_of_events ();
      vars_of_extents ();
      vars_of_faces ();
      vars_of_fileio ();
#ifdef CLASH_DETECTION
      vars_of_filelock ();
#endif
      vars_of_floatfns ();
      vars_of_font_lock ();
      vars_of_frame ();
      vars_of_glyphs ();
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
      vars_of_gui ();
#endif
      vars_of_indent ();
      vars_of_insdel ();
      vars_of_intl ();
#ifdef HAVE_XIM
#ifdef XIM_MOTIF
      vars_of_input_method_motif ();
#else /* XIM_XLIB */
      vars_of_input_method_xlib ();
#endif
#endif /* HAVE_XIM */
      vars_of_keymap ();
      vars_of_lread ();
      vars_of_lstream ();
      vars_of_macros ();
      vars_of_md5 ();
#ifdef HAVE_DATABASE
      vars_of_dbm ();
#endif
#ifdef HAVE_MENUBARS
      vars_of_menubar ();
#endif
      vars_of_minibuf ();
#ifdef MOCKLISP_SUPPORT
      vars_of_mocklisp ();
#endif
      vars_of_objects ();
      vars_of_print ();
#ifndef NO_SUBPROCESSES
      vars_of_process ();
#endif
      vars_of_profile ();
#if defined (HAVE_MMAP) && defined (REL_ALLOC)
      vars_of_ralloc ();
#endif /* HAVE_MMAP && REL_ALLOC */
      vars_of_redisplay ();
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar ();
#endif
      vars_of_search ();
      vars_of_sound ();
      vars_of_specifier ();
      vars_of_symbols ();
      vars_of_syntax ();
#ifdef HAVE_TOOLBARS
      vars_of_toolbar ();
#endif
      vars_of_undo ();
      vars_of_window ();

#ifdef HAVE_TTY
      vars_of_console_tty ();
      vars_of_event_tty ();
      vars_of_frame_tty ();
      vars_of_objects_tty ();
#endif

#ifdef HAVE_X_WINDOWS
      vars_of_device_x ();
#ifdef HAVE_DIALOGS
      vars_of_dialog_x ();
#endif
      vars_of_event_Xt ();
      vars_of_frame_x ();
      vars_of_glyphs_x ();
#ifdef HAVE_MENUBARS
      vars_of_menubar_x ();
#endif
      vars_of_objects_x ();
      vars_of_xselect ();
#ifdef EPOCH
      vars_of_epoch ();
#endif
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar_x ();
#endif
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
      vars_of_gui_x ();
#endif
#endif

#ifdef MULE
      vars_of_mule ();
      vars_of_mule_charset ();
      vars_of_mule_coding ();
#ifdef HAVE_WNN
      vars_of_mule_wnn ();
#endif
#ifdef HAVE_CANNA
      vars_of_mule_canna ();
#endif /* HAVE_CANNA */
#endif /* MULE */

#ifdef ENERGIZE
      vars_of_energize ();
#endif

#ifdef TOOLTALK
      vars_of_tooltalk ();
#endif

#ifdef SUNPRO
      vars_of_sunpro ();
#endif

      /* Now initialize any specifier variables.  We do this later
	 because it has some dependence on the vars initialized
	 above.

	 These functions should *only* initialize specifier variables,
	 and may make use of the following functions/macros in addition
	 to the ones listed above:

	 DEFVAR_SPECIFIER()
	 Fmake_specifier()
	 set_specifier_fallback()
	 set_specifier_caching()
	 */

      specifier_vars_of_glyphs ();
#ifdef HAVE_MENUBARS
      specifier_vars_of_menubar ();
#endif
      specifier_vars_of_redisplay ();
#ifdef HAVE_SCROLLBARS
      specifier_vars_of_scrollbar ();
#endif
#ifdef HAVE_TOOLBARS
      specifier_vars_of_toolbar ();
#endif
      specifier_vars_of_window ();

      /* Now comes all the rest of the variables that couldn't
	 be handled above.  There may be dependencies on variables
	 initialized above, and dependencies between one complex_vars_()
	 function and another. */

      /* Calls Fmake_range_table(). */
      complex_vars_of_regex ();
      /* Calls Fmake_range_table(). */
      complex_vars_of_search ();

      /* Calls Fmake_hashtable(). */
      complex_vars_of_event_stream ();
      /* Calls make_lisp_hashtable(). */
      complex_vars_of_extents ();
      
      /* Depends on hashtables and specifiers. */
      complex_vars_of_faces ();

#ifdef MULE
      /* These two depend on hashtables and various variables declared
	 earlier.  The second may also depend on the first. */
      complex_vars_of_mule_charset ();
      complex_vars_of_mule_coding ();
#endif

      /* This calls allocate_glyph(), which creates specifiers
	 and also relies on a variable (Vthe_nothing_vector) initialized
	 above.  It also calls make_ext_string(), which under Mule
         could require that the charsets be initialized. */
      complex_vars_of_glyphs ();

      /* This relies on the glyphs just created in the previous function,
	 and calls Fadd_spec_to_specifier(), which relies on various
	 variables initialized above. */
#ifdef HAVE_X_WINDOWS
      complex_vars_of_glyphs_x ();
#endif

      /* This calls Fmake_glyph_internal(). */
      complex_vars_of_alloc ();

      /* This calls Fmake_glyph_internal(). */
#ifdef HAVE_MENUBARS
      complex_vars_of_menubar ();
#endif

      /* This calls Fmake_glyph_internal(). */
#ifdef HAVE_SCROLLBARS
      complex_vars_of_scrollbar ();
#endif

      /* This calls allocate_glyph(). */
      complex_vars_of_frame ();

      /* Depends on hashtables. */
#ifdef ENERGIZE
      complex_vars_of_energize ();
#endif

      /* This calls Fcopy_category_table() under Mule, which calls who
         knows what. */
      complex_vars_of_chartab ();

      /* This calls set_string_char(), which (under Mule) depends on the
	 charsets being initialized. */
      complex_vars_of_casetab ();

      /* This calls Fcopy_syntax_table(), which relies on char tables. */
      complex_vars_of_syntax ();

      /* This initializes buffer-local variables, sets things up so
	 that buffers can be created, and creates a couple of basic
	 buffers.  This depends on Vstandard_syntax_table and
	 Vstandard_category_table (initialized in the previous
	 functions), as well as a whole horde of variables that may
	 have been initialized above. */
      complex_vars_of_buffer ();

      /* This initializes console-local variables. */
      complex_vars_of_console ();

      /* This creates a couple more buffers, and depends on the
	 previous function. */
      complex_vars_of_minibuf ();

      /* These two might call Ffile_name_as_directory(), which
	 might depend on all sorts of things; I'm not sure. */
      complex_vars_of_callproc ();
#ifdef CLASH_DETECTION
      complex_vars_of_filelock ();
#endif /* CLASH_DETECTION */

      /* This creates a couple of basic keymaps and depends on Lisp
	 hashtables and Ffset() (both of which depend on some variables
	 initialized in the vars_of_*() section) and possibly other
	 stuff. */
      complex_vars_of_keymap ();

      if (always_gc)                /* purification debugging hack */
	garbage_collect_1 ();
    }

  /* CONGRATULATIONS!!!  We have successfully initialized the Lisp
     engine. */

  if (initialized)
    {
      /* Stuff that needs to be reset at run time.  Order below should
	 not matter. */
      reinit_alloc ();
      reinit_eval ();
#ifdef MULE_REGEXP
      reinit_mule_category ();
#endif
    }

  /* Now do further initialization/setup of stuff that is not needed by the
     syms_of_() routines.  This involves stuff that only is enabled in
     an interactive run (redisplay, user input, etc.) and stuff that is
     not needed until we start loading Lisp code (the reader).  A lot
     of this stuff involves querying the current environment and needs
     to be done both at dump time and at run time. */

  init_callproc ();	/* Set up the process environment (so that egetenv
			   works), the basic directory variables
			   (exec-directory and so on), and stuff
			   related to subprocesses.  This should be
			   first because many of the functions below
			   call egetenv() to get environment variables. */
  init_lread ();	/* Set up the Lisp reader. */
#ifdef MSDOS
  /* Call early 'cause init_environment needs it.  */
  init_dosfns ();
  /* Set defaults for several environment variables.  */
  init_environment (argc, argv, skip_args);
#endif
  init_cmdargs (argc, argv, skip_args);	/* Create list Vcommand_line_args */
  init_buffer (); /* Init default directory of *scratch* buffer */
#ifdef VMS
  init_vms_input ();	/* init_redisplay calls get_tty_device_size,
			   that needs this */
#endif /* VMS */
  init_redisplay ();      /* Determine terminal type.
			     init_sys_modes uses results */
#ifdef VMS
  init_vmsproc ();
  init_vmsfns ();
#endif /* VMS */
  init_event_stream (); /* Set up so we can get user input. */
  init_macros (); /* set up so we can run macros. */
  init_editfns (); /* Determine the name of the user we're running as */
  init_xemacs_process (); /* set up for calling subprocesses */
#ifdef SUNPRO
  init_sunpro (); /* Set up Sunpro usage tracking */
#endif
#if defined (HAVE_NATIVE_SOUND) && defined (hp9000s800)
  init_hpplay ();
#endif
#ifdef HAVE_TTY
  init_device_tty ();
#endif
#ifdef HAVE_GIF
  init_gif_err ();
#endif
  init_console_stream (); /* Create the first console */

  /* try to get the actually pathname of the exec file we are running */
  {
    Vinvocation_name = Fcar (Vcommand_line_args);
    Vinvocation_directory = Vinvocation_name;

    if (!NILP (Ffile_name_directory (Vinvocation_name)))
      /* invocation-name includes a directory component -- presumably it
         is relative to cwd, not $PATH */
      Vinvocation_directory = Fexpand_file_name (Vinvocation_name,
					   Qnil);
    else
      locate_file (Vexec_path, Vinvocation_name, EXEC_SUFFIXES,
		   &Vinvocation_directory, X_OK);

    if (NILP (Vinvocation_directory))
      Vinvocation_directory = Vinvocation_name;

    Vinvocation_name = Ffile_name_nondirectory (Vinvocation_directory);
    Vinvocation_directory = Ffile_name_directory (Vinvocation_directory);
  }

#if defined (LOCALTIME_CACHE) && defined (HAVE_TZSET)
  /* sun's localtime() has a bug.  it caches the value of the time
     zone rather than looking it up every time.  Since localtime() is
     called to bolt the undumping time into the undumped emacs, this
     results in localtime() ignoring the TZ environment variable.
     This flushes the new TZ value into localtime(). */
  tzset ();
#endif /* LOCALTIME_CACHE and TZSET */

  load_me = Qnil;
  if (!initialized)
    {
      /* Handle -l loadup-and-dump, args passed by Makefile. */
      if (argc > 2 + skip_args && !strcmp (argv[1 + skip_args], "-l"))
	load_me = build_string (argv[2 + skip_args]);
#ifdef CANNOT_DUMP
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (!(argc > 1 + skip_args && !strcmp (argv[1 + skip_args], "-nl")))
	load_me = build_string ("loadup.el");
#endif /* CANNOT_DUMP */
    }

#ifdef QUANTIFY
  if (initialized)
    quantify_start_recording_data ();
#endif /* QUANTIFY */

  initialized = 1;

  /* This never returns.  */
  initial_command_loop (load_me);
  /* NOTREACHED */
}


/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args
{
  CONST char * CONST name;
  CONST char * CONST longname;
  int priority;
  int nargs;
};

static struct standard_args standard_args[] =
{
  /* Handled by main_1 above: */
  { "-nl", "--no-shared-memory", 100, 0 },
#ifdef VMS
  { "-map", "--map-data", 100, 0 },
#endif
  { "-t", "--terminal", 95, 1 },
  { "-nw", "--no-windows", 90, 0 },
  { "-batch", "--batch", 85, 0 },
  { "-help", "--help", 80, 0 },
  { "-flags", "--flags", 80, 0 },
  { "-h", 0, 80, 0 },
  { "-?", 0, 80, 0 },
  { "-version", "--version", 75, 0 },
  { "-V", 0, 75, 0 },
  { "-d", "--display", 80, 1 },
  { "-display", 0, 80, 1 },
  { "-NXHost",  0, 79, 0 },
  { "-MachLaunch", 0, 79, 0},

  /* Handled by command-line-early in startup.el: */
  { "-q", "--no-init-file", 50, 0 },
  { "-unmapped", 0, 50, 0 },
  { "-no-init-file", 0, 50, 0 },
  { "-no-site-file", "--no-site-file", 40, 0 },
  { "-u", "--user", 30, 1 },
  { "-user", 0, 30, 1 },
  { "-debug-init", "--debug-init", 20, 0 },

  /* Xt options: */
  { "-i", "--icon-type", 15, 0 },
  { "-itype", 0, 15, 0 },
  { "-iconic", "--iconic", 15, 0 },
  { "-bg", "--background-color", 10, 1 },
  { "-background", 0, 10, 1 },
  { "-fg", "--foreground-color", 10, 1 },
  { "-foreground", 0, 10, 1 },
  { "-bd", "--border-color", 10, 1 },
  { "-bw", "--border-width", 10, 1 },
  { "-ib", "--internal-border", 10, 1 },
  { "-ms", "--mouse-color", 10, 1 },
  { "-cr", "--cursor-color", 10, 1 },
  { "-fn", "--font", 10, 1 },
  { "-font", 0, 10, 1 },
  { "-g", "--geometry", 10, 1 },
  { "-geometry", 0, 10, 1 },
  { "-T", "--title", 10, 1 },
  { "-title", 0, 10, 1 },
  { "-name", "--name", 10, 1 },
  { "-xrm", "--xrm", 10, 1 },
  { "-r", "--reverse-video", 5, 0 },
  { "-rv", 0, 5, 0 },
  { "-reverse", 0, 5, 0 },
  { "-hb", "--horizontal-scroll-bars", 5, 0 },
  { "-vb", "--vertical-scroll-bars", 5, 0 },
  
  /* These have the same priority as ordinary file name args,
     so they are not reordered with respect to those.  */
  { "-L", "--directory", 0, 1 },
  { "-directory", 0, 0, 1 },
  { "-l", "--load", 0, 1 },
  { "-load", 0, 0, 1 },
  { "-f", "--funcall", 0, 1 },
  { "-funcall", 0, 0, 1 },
  { "-eval", "--eval", 0, 1 },
  { "-insert", "--insert", 0, 1 },
  /* This should be processed after ordinary file name args and the like.  */
  { "-kill", "--kill", -10, 0 },
};

/* Reorder the elements of ARGV (assumed to have ARGC elements)
   so that the highest priority ones come first.
   Do not change the order of elements of equal priority.
   If an option takes an argument, keep it and its argument together.  */

static void
sort_args (int argc, char **argv)
{
  char **new = (char **) xmalloc (sizeof (char *) * argc);
  /* For each element of argv,
     the corresponding element of options is:
     0 for an option that takes no arguments,
     1 for an option that takes one argument, etc.
     -1 for an ordinary non-option argument.  */
  int *options  = (int *) xmalloc (sizeof (int) * argc);
  int *priority = (int *) xmalloc (sizeof (int) * argc);
  int to = 1;
  int from;
  int i;
  int end_of_options_p = 0;

  /* Categorize all the options,
     and figure out which argv elts are option arguments.  */
  for (from = 1; from < argc; from++)
    {
      options[from] = -1;
      priority[from] = 0;
      /* Pseudo options "--" and "run-temacs" indicate end of options */
      if (!strcmp (argv[from], "--") ||
	  !strcmp (argv[from], "run-temacs"))
	end_of_options_p = 1;
      if (!end_of_options_p && argv[from][0] == '-')
	{
	  int match, thislen;
	  char *equals;

	  /* Look for a match with a known old-fashioned option.  */
	  for (i = 0; i < countof (standard_args); i++)
	    if (!strcmp (argv[from], standard_args[i].name))
	      {
		options[from]  = standard_args[i].nargs;
		priority[from] = standard_args[i].priority;
		if (from + standard_args[i].nargs >= argc)
		  fatal ("Option `%s' requires an argument\n", argv[from]);
		from += standard_args[i].nargs;
		goto done;
	      }

	  /* Look for a match with a known long option.
	     MATCH is -1 if no match so far, -2 if two or more matches so far,
	     >= 0 (the table index of the match) if just one match so far.  */
	  if (argv[from][1] == '-')
	    {
	      match = -1;
	      thislen = strlen (argv[from]);
	      equals = strchr (argv[from], '=');
	      if (equals != 0)
		thislen = equals - argv[from];

	      for (i = 0; i < countof (standard_args); i++)
		if (standard_args[i].longname
		    && !strncmp (argv[from], standard_args[i].longname,
				 thislen))
		  {
		    if (match == -1)
		      match = i;
		    else
		      match = -2;
		  }

	      /* If we found exactly one match, use that.  */
	      if (match >= 0)
		{
		  options[from]  = standard_args[match].nargs;
		  priority[from] = standard_args[match].priority;
		  /* If --OPTION=VALUE syntax is used,
		     this option uses just one argv element.  */
		  if (equals != 0)
		    options[from] = 0;
		  if (from + options[from] >= argc)
		    fatal ("Option `%s' requires an argument\n", argv[from]);
		  from += options[from];
		}
	    }
	done: ;
	}
    }

  /* Copy the arguments, in order of decreasing priority, to NEW.  */
  new[0] = argv[0];
  while (to < argc)
    {
      int best = -1;
      int best_priority = -9999;

      /* Find the highest priority remaining option.
	 If several have equal priority, take the first of them.  */
      for (from = 1; from < argc; from++)
	{
	  if (argv[from] != 0 && priority[from] > best_priority)
	    {
	      best_priority = priority[from];
	      best = from;
	    }
	  /* Skip option arguments--they are tied to the options.  */
	  if (options[from] > 0)
	    from += options[from];
	}
	    
      if (best < 0)
	abort ();

      /* Copy the highest priority remaining option, with its args, to NEW.  */
      new[to++] = argv[best];
      for (i = 0; i < options[best]; i++)
	new[to++] = argv[best + i + 1];

      /* Clear out this option in ARGV.  */
      argv[best] = 0;
      for (i = 0; i < options[best]; i++)
	argv[best + i + 1] = 0;
    }

  memcpy (argv, new, sizeof (char *) * argc);
}

static JMP_BUF run_temacs_catch;

static int run_temacs_argc;
static char **run_temacs_argv;
static char *run_temacs_args;
static int run_temacs_argv_size;
static int run_temacs_args_size;

extern int gc_in_progress;

DEFUN ("running-temacs-p", Frunning_temacs_p, 0, 0, 0, /*
True if running temacs.  This means we are in the dumping stage.
This is false during normal execution of the `xemacs' program, and
becomes false once `run-emacs-from-temacs' is run.
*/
       ())
{
  return run_temacs_argc >= 0 ? Qt : Qnil;
}

DEFUN ("run-emacs-from-temacs", Frun_emacs_from_temacs, 0, MANY, 0, /*
Do not call this.  It will reinitialize your XEmacs.  You'll be sorry.
*/
/* If this function is called from startup.el, it will be possible to run
   temacs as an editor using 'temacs -batch -l loadup.el run-temacs', instead
   of having to dump an emacs and then run that (when debugging emacs itself,
   this can be much faster)). [Actually, the speed difference isn't that
   much as long as your filesystem is local, and you don't end up with
   a dumped version in case you want to rerun it.  This function is most
   useful when used as part of the `make all-elc' command. --ben]
   This will \"restart\" emacs with the specified command-line arguments.
 */
     (int nargs, Lisp_Object *args))
{
  int ac;
  Extbyte *wampum;
  int namesize;
  int total_len;
  Lisp_Object orig_invoc_name = Fcar (Vcommand_line_args);
  Extbyte **wampum_all = (Extbyte **) alloca (nargs * sizeof (Extbyte *));
  int *wampum_all_len = (int *) alloca (nargs * sizeof (int));

  assert (!gc_in_progress);

  if (run_temacs_argc < 0)
    error ("I've lost my temacs-hood.");

  /* Need to convert the orig_invoc_name and all of the arguments
     to external format. */
  
  GET_STRING_EXT_DATA_ALLOCA (orig_invoc_name, FORMAT_OS, wampum,
			      namesize);
  namesize++;

  for (ac = 0, total_len = namesize; ac < nargs; ac++)
    {
      CHECK_STRING (args[ac]);
      GET_STRING_EXT_DATA_ALLOCA (args[ac], FORMAT_OS,
				  wampum_all[ac],
				  wampum_all_len[ac]);
      wampum_all_len[ac]++;
      total_len += wampum_all_len[ac];
    }
  DO_REALLOC (run_temacs_args, run_temacs_args_size, total_len, char);
  DO_REALLOC (run_temacs_argv, run_temacs_argv_size, nargs+1, char *);

  memcpy (run_temacs_args, wampum, namesize);
  run_temacs_argv [0] = run_temacs_args;
  for (ac = 0; ac < nargs; ac++)
    {
      memcpy (run_temacs_args + namesize,
	      wampum_all[ac], wampum_all_len[ac]);
      run_temacs_argv [ac + 1] = run_temacs_args + namesize;
      namesize += wampum_all_len[ac];
    }
  run_temacs_argv [nargs + 1] = 0;
  catchlist = NULL; /* Important!  Otherwise free_cons() calls in
		       condition_case_unwind() may lead to GC death. */
  unbind_to (0, Qnil); /* this closes loadup.el */
  purify_flag = 0;
  run_temacs_argc = nargs + 1;
  report_pure_usage (1, 0);
  LONGJMP (run_temacs_catch, 1);
  return Qnil; /* not reached; warning suppression */
}

/* ARGSUSED */
DOESNT_RETURN
main (int argc, char **argv, char **envp)
{
  int     volatile vol_argc = argc;
  char ** volatile vol_argv = argv;
  char ** volatile vol_envp = envp;
#ifdef QUANTIFY
  quantify_stop_recording_data ();
  quantify_clear_data ();
#endif /* QUANTIFY */

  suppress_early_backtrace = 0;
  lim_data = 0; /* force reinitialization of this variable */

  /* Lisp_Object must fit in a word; check VALBITS and GCTYPEBITS */
  assert (sizeof (Lisp_Object) == sizeof (void *));

  if (!initialized)
    {
      run_temacs_argc = 0;
      if (! SETJMP (run_temacs_catch))
	main_1 (vol_argc, vol_argv, vol_envp);
      /* run-emacs-from-temacs called */
      vol_argc = run_temacs_argc;
      vol_argv = run_temacs_argv;
#ifdef _SCO_DS
      /* This makes absolutely no sense to anyone involved.  There are
	 several people using this stuff.  We've compared versions on
	 everything we can think of.  We can find no difference.
	 However, on both my systems environ is a plain old global
	 variable initialized to zero.  _environ is the one that
	 contains pointers to the actual environment.
	
	 Since we can't figure out the difference (and we're hours
	 away from a release), this takes a very cowardly approach and
	 is bracketed with both a system specific preprocessor test
	 and a runtime "do you have this problem" test
	
	 06/20/96 robertl@dgii.com */
      {
	extern char *_environ;
	if ((unsigned) environ == 0)
	  environ=_environ;
      }
#endif /* _SCO_DS */
      vol_envp = environ;
    }
  run_temacs_argc = -1;

  main_1 (vol_argc, vol_argv, vol_envp);
}


DEFUN ("kill-emacs", Fkill_emacs, 0, 1, "P", /*
Exit the XEmacs job and kill it.  Ask for confirmation, without argument.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.

The value of `kill-emacs-hook', if not void,
is a list of functions (of no args),
all of which are called before XEmacs is actually killed.
*/
       (arg))
{
  /* This function can GC */
  struct gcpro gcpro1;

  GCPRO1 (arg);

  if (feof (stdin))
    arg = Qt;

  if (!preparing_for_armageddon && !noninteractive)
    run_hook (Qkill_emacs_hook);

  /* make sure no quitting from now on!! */
  dont_check_for_quit = 1;
  Vinhibit_quit = Qt;

  if (!preparing_for_armageddon)
    {
      Lisp_Object concons;

      /* Normally, go ahead and delete all the consoles now.
	 Some unmentionably lame window systems (MS Wwwww...... eek,
	 I can't even say it) don't properly clean up after themselves,
	 and even for those that do, it might be cleaner this way.
	 If we're going down, however, we don't do this (might
	 be too dangerous), and if we get a crash somewhere within
	 this loop, we'll still autosave and won't try this again. */
      CONSOLE_LOOP (concons)
	delete_console_internal (XCONSOLE (XCAR (concons)), 1, 1, 0);
    }

  UNGCPRO;

  shut_down_emacs (0, ((STRINGP (arg)) ? arg : Qnil));

  exit ((INTP (arg)) ? XINT (arg)
#ifdef VMS
	: 1
#else
	: 0
#endif
	);
  /* NOTREACHED */
  return Qnil; /* I'm sick of the compiler warning */
}

/* Perform an orderly shutdown of XEmacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when XEmacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers, X protocol error handlers,
   and Fkill_emacs.  */
static void
shut_down_emacs (int sig, Lisp_Object stuff)
{
  /* This function can GC */
  /* Prevent running of hooks and other non-essential stuff
     from now on.  */
  preparing_for_armageddon = 1;

  /* In case frames or windows are screwed up, avoid assertion
     failures here */
  Vinhibit_quit = Qt;

#ifdef QUANTIFY
  quantify_stop_recording_data ();
#endif /* QUANTIFY */

  /* This is absolutely the most important thing to do, so make sure
     we do it now, before anything else.  We might have crashed and
     be in a weird inconsistent state, and potentially anything could
     set off another protection fault and cause us to bail out
     immediately. */
  Fdo_auto_save (Qt, Qnil); /* do this before anything hazardous */

  fflush (stdout);
  reset_all_consoles ();
  if (sig && sig != SIGTERM)
    {
      stderr_out ("\nFatal error (%d).\n", sig);
      stderr_out
	("Your files have been auto-saved.\n"
	 "Use `M-x recover-session' to recover them.\n"
	 "\n"
	 "Please report this bug to the address `crashes@xemacs.org'.\n"
	 "*MAKE SURE* to include as much configuration information as\n"
	 "possible; at the very least what OS and hardware you are running\n"
	 "on, and hopefully also what compiler and compiler options the\n"
	 "binary was compiled with, what options XEmacs was compiled with,\n"
	 "whether you are using a prebuilt binary from ftp.xemacs.org or\n"
	 "compiled XEmacs yourself for your system, etc.\n"
	 "\n"
	 "If at all possible, *please* try to obtain a C stack backtrace;\n"
	 "it will help us immensely in determining what went wrong.\n"
	 "To do this, locate the core file that was produced as a result\n"
	 "of this crash (it's usually called `core' and is located in the\n"
	 "directory in which you started XEmacs, or maybe in your home\n"
	 "directory), and type\n"
	 "\n"
	 "  gdb ");
      {
	CONST char *name;
	char *dir = 0;

	/* Now try to determine the actual path to the executable,
	   to try to make the backtrace-determination process as foolproof
	   as possible. */
	if (GC_STRINGP (Vinvocation_name))
	  name = (char *) XSTRING_DATA (Vinvocation_name);
	else
	  name = "xemacs";
	if (GC_STRINGP (Vinvocation_directory))
	  dir = (char *) XSTRING_DATA (Vinvocation_directory);
	if (!dir || dir[0] != '/')
	  stderr_out ("`which %s`", name);
	else if (dir[strlen (dir) - 1] != '/')
	  stderr_out ("%s/%s", dir, name);
	else
	  stderr_out ("%s%s", dir, name);
      }
      stderr_out
	(" core\n\n"
	 "then type `where' when the debugger prompt comes up.\n"
	 "(If you don't have GDB on your system, you might have DBX,\n"
	 "or XDB, or SDB.  A similar procedure should work for all of\n"
	 "these.  Ask your system administrator if you need more help.)\n");
    }

  stuff_buffered_input (stuff);

  kill_buffer_processes (Qnil);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif

#ifdef TOOLTALK
  tt_session_quit (tt_default_session ());
#if 0
  /* The following crashes when built on X11R5 and run on X11R6 */
  tt_close ();
#endif
#endif /* TOOLTALK */

#ifdef VMS
  kill_vms_processes ();
#endif
}


#ifndef CANNOT_DUMP
/* Nothing like this can be implemented on an Apollo.
   What a loss!  */

extern char my_edata[];

#ifdef HAVE_SHM

DEFUN ("dump-emacs-data", Fdump_emacs_data, 1, 1, 0, /*
Dump current state of XEmacs into data file FILENAME.
This function exists on systems that use HAVE_SHM.
*/
       (intoname))
{
  /* This function can GC */
  int opurify;
  struct gcpro gcpro1;
  GCPRO1 (intoname);

  CHECK_STRING (intoname);
  intoname = Fexpand_file_name (intoname, Qnil);

  opurify = purify_flag;
  purify_flag = 0;

  fflush (stderr);
  fflush (stdout);

  disksave_object_finalization ();
  release_breathing_space ();

  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (&my_edata, malloc_warning);
#endif
  UNGCPRO;
  map_out_data (XSTRING_DATA (intoname));

  purify_flag = opurify;

  return Qnil;
}

#else /* not HAVE_SHM */

DEFUN ("dump-emacs", Fdump_emacs, 2, 2, 0, /*
Dump current state of XEmacs into executable file FILENAME.
Take symbols from SYMFILE (presumably the file you executed to run XEmacs).
This is used in the file `loadup.el' when building XEmacs.

Remember to set `command-line-processed' to nil before dumping
if you want the dumped XEmacs to process its command line
and announce itself normally when it is run.
*/
       (intoname, symname))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;
  int opurify;

  GCPRO2 (intoname, symname);

#ifdef FREE_CHECKING
  Freally_free (Qnil);

  /* When we're dumping, we can't use the debugging free() */

  disable_free_hook ();
#endif

  CHECK_STRING (intoname);
  intoname = Fexpand_file_name (intoname, Qnil);
  if (!NILP (symname))
    {
      CHECK_STRING (symname);
      if (XSTRING_LENGTH (symname) > 0)
	symname = Fexpand_file_name (symname, Qnil);
      else
	symname = Qnil;
    }

  opurify = purify_flag;
  purify_flag = 0;

  report_pure_usage (1, 1);

  fflush (stderr);
  fflush (stdout);

  disksave_object_finalization ();
  release_breathing_space ();

#ifdef VMS
  mapout_data (XSTRING_DATA (intoname));
#else
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (&my_edata, malloc_warning);
#endif

  UNGCPRO;

#if defined (MSDOS) && defined (EMX)
  {
    int fd = open ((char *) XSTRING_DATA (intoname),
                   O_WRONLY|O_CREAT|O_TRUNC, S_IREAD|S_IWRITE);
    if (!fd) {
      error ("Failure operating on %s", XSTRING_DATA (intoname));
    } else {
      _core (fd);
      close (fd);
    }
  }
#else /* not MSDOS and EMX */
  {
    CONST char *intoname_ext;
    CONST char *symname_ext;

    GET_C_STRING_FILENAME_DATA_ALLOCA (intoname, intoname_ext);
    if (STRINGP (symname))
      GET_C_STRING_FILENAME_DATA_ALLOCA (symname, symname_ext);
    else
      symname_ext = 0;
    /* here we break our rule that the filename conversion should
       be performed at the actual time that the system call is made.
       It's a whole lot easier to do the conversion here than to
       modify all the unexec routines to ensure that filename
       conversion is applied everywhere.  Don't worry about memory
       leakage because this call only happens once. */
    unexec ((char *) intoname_ext, (char *) symname_ext,
	    (unsigned int) &my_edata,
	    0, 0);
  }
#endif /* not MSDOS and EMX */
#endif /* not VMS */

  purify_flag = opurify;

  return Qnil;
}

#endif /* not HAVE_SHM */

#endif /* not CANNOT_DUMP */

#ifndef SEPCHAR
#define SEPCHAR ':'
#endif

Lisp_Object
decode_env_path (CONST char *evarname, CONST char *defalt)
{
  REGISTER CONST char *path = 0;
  REGISTER CONST char *p;
  Lisp_Object lpath = Qnil;

  if (evarname)
    path = (char *) egetenv (evarname);
  if (!path)
    path = defalt;
  if (!path)
    return (Qnil);

#if defined (MSDOS) || defined (WIN32)
  dostounix_filename (path);
#endif

  while (1)
    {
      p = strchr (path, SEPCHAR);
      if (!p) p = path + strlen (path);
      lpath = Fcons (((p != path)
                      ? make_string ((CONST Bufbyte *) path, p - path)
                      : Qnil),
                     lpath);
      if (*p)
	path = p + 1;
      else
	break;
    }
  return Fnreverse (lpath);
}

DEFUN ("noninteractive", Fnoninteractive, 0, 0, 0, /*
Non-nil return value means XEmacs is running without interactive terminal.
*/
       ())
{
  return ((noninteractive) ? Qt : Qnil);
}

/* This flag is useful to define if you're under a debugger; this way, you
   can put a breakpoint of assert_failed() and debug multiple problems
   in one session without having to recompile. */
/* #define ASSERTIONS_DONT_ABORT */

#ifdef USE_ASSERTIONS
/* This highly dubious kludge ... shut up Jamie, I'm tired of your slagging. */

DOESNT_RETURN
assert_failed (CONST char *file, int line, CONST char *expr)
{
  stderr_out ("Fatal error: assertion failed, file %s, line %d, %s\n",
	      file, line, expr);
#undef abort	/* avoid infinite #define loop... */
#ifndef ASSERTIONS_DONT_ABORT
  abort ();
#endif
}
#endif /* USE_ASSERTIONS */

#ifdef QUANTIFY
DEFUN ("quantify-start-recording-data",
       Fquantify_start_recording_data, 0, 0, 0, /*
Start recording Quantify data.
*/
       ())
{
  quantify_start_recording_data ();
  return Qnil;
}

DEFUN ("quantify-stop-recording-data",
       Fquantify_stop_recording_data, 0, 0, 0, /*
Stop recording Quantify data.
*/
       ())
{
  quantify_stop_recording_data ();
  return Qnil;
}

DEFUN ("quantify-clear-data", Fquantify_clear_data, 0, 0, 0, /*
Clear all Quantify data.
*/
       ())
{
  quantify_clear_data ();
  return Qnil;
}
#endif /* QUANTIFY */

void
syms_of_emacs (void)
{
#ifndef CANNOT_DUMP
#ifdef HAVE_SHM
  DEFSUBR (Fdump_emacs_data);
#else
  DEFSUBR (Fdump_emacs);
#endif
#endif /* !CANNOT_DUMP */

  DEFSUBR (Frun_emacs_from_temacs);
  DEFSUBR (Frunning_temacs_p);
  DEFSUBR (Finvocation_name);
  DEFSUBR (Finvocation_directory);
  DEFSUBR (Fkill_emacs);
  DEFSUBR (Fnoninteractive);

#ifdef QUANTIFY
  DEFSUBR (Fquantify_start_recording_data);
  DEFSUBR (Fquantify_stop_recording_data);
  DEFSUBR (Fquantify_clear_data);
#endif /* QUANTIFY */

  defsymbol (&Qkill_emacs_hook, "kill-emacs-hook");
  defsymbol (&Qsave_buffers_kill_emacs, "save-buffers-kill-emacs");
}

void
vars_of_emacs (void)
{
  DEFVAR_BOOL ("suppress-early-error-handler-backtrace",
	       &suppress_early_backtrace /*
Non-nil means early error handler shouldn't print a backtrace
*/ );

  DEFVAR_LISP ("command-line-args", &Vcommand_line_args /*
Args passed by shell to XEmacs, as a list of strings.
*/ );

  DEFVAR_LISP ("invocation-name", &Vinvocation_name /*
The program name that was used to run XEmacs.
Any directory names are omitted.
*/ );

  DEFVAR_LISP ("invocation-directory", &Vinvocation_directory /*
The directory in which the XEmacs executable was found, to run it.
The value is simply the program name if that directory's name is not known.
*/ );

#if 0 /* FSFmacs */
  xxDEFVAR_LISP ("installation-directory", &Vinstallation_directory,
    "A directory within which to look for the `lib-src' and `etc' directories.\n"
"This is non-nil when we can't find those directories in their standard\n"
"installed locations, but we can find them\n"
"near where the XEmacs executable was found.");
#endif

  DEFVAR_LISP ("system-type", &Vsystem_type /*
Value is symbol indicating type of operating system you are using.
*/ );
  Vsystem_type = intern (SYSTEM_TYPE);

#ifndef EMACS_CONFIGURATION
# define EMACS_CONFIGURATION "UNKNOWN"
#endif
  DEFVAR_LISP ("system-configuration", &Vsystem_configuration /*
Value is string indicating configuration XEmacs was built for.
*/ );
  Vsystem_configuration = Fpurecopy (build_string (EMACS_CONFIGURATION));

  DEFVAR_BOOL ("noninteractive", &noninteractive1 /*
Non-nil means XEmacs is running without interactive terminal.
*/ );

  DEFVAR_INT ("emacs-priority", &emacs_priority /*
Priority for XEmacs to run at.
This value is effective only if set before XEmacs is dumped,
and only if the XEmacs executable is installed with setuid to permit
it to change priority.  (XEmacs sets its uid back to the real uid.)
Currently, you need to define SET_EMACS_PRIORITY in `config.h'
before you compile XEmacs, to enable the code for this feature.
*/ );
  emacs_priority = 0;
}
