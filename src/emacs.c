/* XEmacs -- Fully extensible Emacs, running on Unix and other platforms.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
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

/* Synched up with: Mule 2.0, FSF 19.28. */

/* Capsule summary of the various releases of Lucid Emacs/XEmacs and
   FSF/GNU Emacs. (Copied from the Internals Manual, where the
   canonical version lives.) Provided here for use in cross-referencing
   version releases and dates in comments, esp. in the authorship
   comments at the beginning of each file.  More information about
   history can be found in the beginning of the Internals Manual and
   in the About page.


-- A time line for Lucid Emacs/XEmacs is

version 19.0 shipped with Energize 1.0, April 1992.
version 19.1 released June 4, 1992.
version 19.2 released June 19, 1992.
version 19.3 released September 9, 1992.
version 19.4 released January 21, 1993.
version 19.5 was a repackaging of 19.4 with a few bug fixes and
  shipped with Energize 2.0.  Never released to the net.
version 19.6 released April 9, 1993.
version 19.7 was a repackaging of 19.6 with a few bug fixes and
  shipped with Energize 2.1.  Never released to the net.
version 19.8 released September 6, 1993.
version 19.9 released January 12, 1994.
version 19.10 released May 27, 1994.
version 19.11 (first XEmacs) released September 13, 1994.
version 19.12 released June 23, 1995.
version 19.13 released September 1, 1995.
version 19.14 released June 23, 1996.
version 20.0 released February 9, 1997.
version 19.15 released March 28, 1997.
version 20.1 (not released to the net) April 15, 1997.
version 20.2 released May 16, 1997.
version 19.16 released October 31, 1997.
version 20.3 (the first stable version of XEmacs 20.x) released November 30,
1997.
version 20.4 released February 28, 1998.
version 21.0.60 released December 10, 1998. (The version naming scheme was
changed at this point: [a] the second version number is odd for stable
versions, even for beta versions; [b] a third version number is added,
replacing the "beta xxx" ending for beta versions and allowing for
periodic maintenance releases for stable versions.  Therefore, 21.0 was
never "officially" released; similarly for 21.2, etc.)
version 21.0.61 released January 4, 1999.
version 21.0.63 released February 3, 1999.
version 21.0.64 released March 1, 1999.
version 21.0.65 released March 5, 1999.
version 21.0.66 released March 12, 1999.
version 21.0.67 released March 25, 1999.
version 21.1.2 released May 14, 1999; on comp.emacs, May 28. (This is
 the followup to 21.0.67.  The second version number was bumped to indicate
 the beginning of the "stable" series.)
version 21.1.3 released June 26, 1999.
version 21.1.4 released July 8, 1999.
version 21.1.6 released August 14, 1999. (There was no 21.1.5.)
version 21.1.7 released September 26, 1999.
version 21.1.8 released November 2, 1999.
version 21.1.9 released February 13, 2000.
version 21.1.10 released May 7, 2000.
version 21.1.10a released June 24, 2000.
version 21.1.11 released July 18, 2000.
version 21.1.12 released August 5, 2000.
version 21.1.13 released January 7, 2001.
version 21.1.14 released January 27, 2001.
version 21.2.9 released February 3, 1999.
version 21.2.10 released February 5, 1999.
version 21.2.11 released March 1, 1999.
version 21.2.12 released March 5, 1999.
version 21.2.13 released March 12, 1999.
version 21.2.14 released May 14, 1999.
version 21.2.15 released June 4, 1999.
version 21.2.16 released June 11, 1999.
version 21.2.17 released June 22, 1999.
version 21.2.18 released July 14, 1999.
version 21.2.19 released July 30, 1999.
version 21.2.20 released November 10, 1999.
version 21.2.21 released November 28, 1999.
version 21.2.22 released November 29, 1999.
version 21.2.23 released December 7, 1999.
version 21.2.24 released December 14, 1999.
version 21.2.25 released December 24, 1999.
version 21.2.26 released December 31, 1999.
version 21.2.27 released January 18, 2000.
version 21.2.28 released February 7, 2000.
version 21.2.29 released February 16, 2000.
version 21.2.30 released February 21, 2000.
version 21.2.31 released February 23, 2000.
version 21.2.32 released March 20, 2000.
version 21.2.33 released May 1, 2000.
version 21.2.34 released May 28, 2000.
version 21.2.35 released July 19, 2000.
version 21.2.36 released October 4, 2000.
version 21.2.37 released November 14, 2000.
version 21.2.38 released December 5, 2000.
version 21.2.39 released December 31, 2000.
version 21.2.40 released January 8, 2001.
version 21.2.41 "Polyhymnia" released January 17, 2001.
version 21.2.42 "Poseidon" released January 20, 2001.
version 21.2.43 "Terspichore" released January 26, 2001.
version 21.2.44 "Thalia" released February 8, 2001.
version 21.2.45 "Thelxepeia" released February 23, 2001.
version 21.2.46 "Urania" released March 21, 2001.
version 21.5.0 "alfalfa" released April 18, 2001.
version 21.5.1 "anise" released May 9, 2001.
version 21.5.2 "artichoke" released July 28, 2001.
version 21.5.3 "asparagus" released September 7, 2001.
version 21.5.4 "bamboo" released January 8, 2002.
version 21.5.5 "beets" released March 5, 2002.
version 21.5.6 "bok choi" released April 5, 2002.


-- A time line for GNU Emacs version 19 is

version 19.7 (beta) (first beta release) released ??????; prob. late May 1993.
version 19.8 (beta) released May 27, 1993.
version 19.9 (beta) released May 27, 1993.
version 19.10 (beta) released May 30, 1993.
version 19.11 (beta) released June 1, 1993.
version 19.12 (beta) released June 2, 1993.
version 19.13 (beta) released June 8, 1993.
version 19.14 (beta) released June 17, 1993.
version 19.15 (beta) released June 19, 1993.
version 19.16 (beta) released July 6, 1993.
version 19.17 (beta) released late July, 1993.
version 19.18 (beta) released August 9, 1993.
version 19.19 (beta) released August 15, 1993.
version 19.20 (beta) released November 17, 1993.
version 19.21 (beta) released November 17, 1993.
version 19.22 (beta) released November 28, 1993.
version 19.23 (beta) released on comp.emacs, May 17, 1994.
version 19.24 (beta) released May 16, 1994.
version 19.25 (beta) released June 3, 1994.
version 19.26 (beta) released September 11, 1994.
version 19.27 (beta) released September 14, 1994.
version 19.28 (first ``official'' release) released November 1, 1994.
version 19.29 released June 21, 1995.
version 19.30 released November 24, 1995.
version 19.31 released May 25, 1996.
version 19.32 released July 31, 1996.
version 19.33 released August 11, 1996.
version 19.34 released August 21, 1996; on comp.emacs, August 22.
version 19.34b released September 6, 1996.


-- A time line for GNU Emacs versions 20 and 21 is

version 20.1 released September 17, 1997.
version 20.2 released September 20, 1997.
version 20.3 released August 19, 1998.
version 20.4 released July 12, 1999; on comp.emacs, July 27.
version 21.1 released October 20, 2001.


-- A time line for GNU Emacs version 18 and older is

GNU Emacs version 15 (15.34) was released sometime in 1984 or 1985 and
  shared some code with a version of Emacs written by James Gosling (the
  same James Gosling who later created the Java language).
GNU Emacs version 16 (first released version was 16.56) was released on
  July 15, 1985.  All Gosling code was removed due to potential copyright
  problems with the code.
version 16.57: released on September 16, 1985.
versions 16.58, 16.59: released on September 17, 1985.
version 16.60: released on September 19, 1985.  These later version 16's
  incorporated patches from the net, esp. for getting Emacs to work under
  System V.
version 17.36 (first official v17 release) released on December 20, 1985.
  Included a TeX-able user manual.  First official unpatched version that
   worked on vanilla System V machines.
version 17.43 (second official v17 release) released on January 25, 1986.
version 17.45 released on January 30, 1986.
version 17.46 released on February 4, 1986.
version 17.48 released on February 10, 1986.
version 17.49 released on February 12, 1986.
version 17.55 released on March 18, 1986.
version 17.57 released on March 27, 1986.
version 17.58 released on April 4, 1986.
version 17.61 released on April 12, 1986.
version 17.63 released on May 7, 1986.
version 17.64 released on May 12, 1986.
version 18.24 (a beta version) released on October 2, 1986.
version 18.30 (a beta version) released on November 15, 1986.
version 18.31 (a beta version) released on November 23, 1986.
version 18.32 (a beta version) released on December 7, 1986.
version 18.33 (a beta version) released on December 12, 1986.
version 18.35 (a beta version) released on January 5, 1987.
version 18.36 (a beta version) released on January 21, 1987.
January 27, 1987: The Great Usenet Renaming.  net.emacs is now comp.emacs.
version 18.37 (a beta version) released on February 12, 1987.
version 18.38 (a beta version) released on March 3, 1987.
version 18.39 (a beta version) released on March 14, 1987.
version 18.40 (a beta version) released on March 18, 1987.
version 18.41 (the first ``official'' release) released on March 22, 1987.
version 18.45 released on June 2, 1987.
version 18.46 released on June 9, 1987.
version 18.47 released on June 18, 1987.
version 18.48 released on September 3, 1987.
version 18.49 released on September 18, 1987.
version 18.50 released on February 13, 1988.
version 18.51 released on May 7, 1988.
version 18.52 released on September 1, 1988.
version 18.53 released on February 24, 1989.
version 18.54 released on April 26, 1989.
version 18.55 released on August 23, 1989.  This is the earliest version
  that is still available by FTP.
version 18.56 released on January 17, 1991.
version 18.57 released late January, 1991.
version 18.58 released ?????.
version 18.59 released October 31, 1992.

*/

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
#include "redisplay.h"
#include "frame.h"
#include "sysdep.h"

#include "systty.h"
#include "sysfile.h"
#include "systime.h"
#include "sysproc.h" /* for qxe_getpid() */

#ifdef PDUMP
#include "dumper.h"
#endif

#ifdef QUANTIFY
#include <quantify.h>
#endif

#ifdef HAVE_SHLIB
#include "sysdll.h"
#endif

#ifdef TOOLTALK
#include TT_C_H_FILE
#endif

#if defined (WIN32_NATIVE) || defined (CYGWIN)
#include "syswindows.h"
#endif

/* For PATH_EXEC */
#include <paths.h>

#if defined (HEAP_IN_DATA) && !defined (PDUMP)
void report_sheap_usage (int die_if_pure_storage_exceeded);
#endif

#if !defined (SYSTEM_MALLOC) && !defined (DOUG_LEA_MALLOC)
extern void *(*__malloc_hook)(size_t);
extern void *(*__realloc_hook)(void *, size_t);
extern void (*__free_hook)(void *);
#endif  /* not SYSTEM_MALLOC && not DOUG_LEA_MALLOC */

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* Set nonzero after XEmacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

#ifdef DOUG_LEA_MALLOC
# include <malloc.h>
/* Preserves a pointer to the memory allocated that copies that
   static data inside glibc's malloc.  */
static void *malloc_state_ptr;
#endif /* DOUG_LEA_MALLOC */

# ifdef REL_ALLOC
void r_alloc_reinit (void);
# endif

/* Variable whose value is symbol giving operating system type. */
Lisp_Object Vsystem_type;

/* Variable whose value is string giving configuration built for.  */
Lisp_Object Vsystem_configuration;

/* Variable whose value is string containing the configuration options
   XEmacs was built with.  */
Lisp_Object Vsystem_configuration_options;

/* Version numbers and strings */
Lisp_Object Vemacs_major_version;
Lisp_Object Vemacs_minor_version;
Lisp_Object Vemacs_patch_level;
Lisp_Object Vemacs_beta_version;
Lisp_Object Vxemacs_codename;
#ifdef INFODOCK
Lisp_Object Vinfodock_major_version;
Lisp_Object Vinfodock_minor_version;
Lisp_Object Vinfodock_build_version;
#endif

/* The path under which XEmacs was invoked. */
Lisp_Object Vinvocation_path;

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

Lisp_Object Vemacs_program_name, Vemacs_program_version;
Lisp_Object Vexec_path;
Lisp_Object Vexec_directory, Vconfigure_exec_directory;
Lisp_Object Vlisp_directory, Vconfigure_lisp_directory;
Lisp_Object Vmule_lisp_directory, Vconfigure_mule_lisp_directory;
Lisp_Object Vmodule_directory, Vconfigure_module_directory;
Lisp_Object Vsite_module_directory, Vconfigure_site_module_directory;
Lisp_Object Vconfigure_package_path;
Lisp_Object Vdata_directory, Vconfigure_data_directory;
Lisp_Object Vdoc_directory, Vconfigure_doc_directory;
Lisp_Object Vconfigure_lock_directory;
Lisp_Object Vdata_directory_list;
Lisp_Object Vconfigure_info_directory;
Lisp_Object Vsite_directory, Vconfigure_site_directory;
Lisp_Object Vconfigure_info_path;
Lisp_Object Vinternal_error_checking;
Lisp_Object Vmail_lock_methods, Vconfigure_mail_lock_method;

/* The default base directory XEmacs is installed under. */
Lisp_Object Vconfigure_exec_prefix_directory, Vconfigure_prefix_directory;

/* If nonzero, set XEmacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority. */
Fixnum emacs_priority;

/* Some FSF junk with running_asynch_code, to preserve the match
   data.  Not necessary because we don't call process filters
   asynchronously (i.e. from within QUIT). */

/* If non-zero, a window-system was specified on the command line. */
int display_arg;

/* Type of display specified.  We cannot use a Lisp symbol here because
   Lisp symbols may not initialized at the time that we set this
   variable. */
const Char_ASCII *display_use;

/* If non-zero, then the early error handler will only print the error
   message and exit. */
int suppress_early_error_handler_backtrace;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#ifdef USG_SHARED_LIBRARIES
/* If nonzero, this is the place to put the end of the writable segment
   at startup.  */

uintptr_t bss_end = 0;
#endif

/* Number of bytes of writable memory we can expect to be able to get:
   Leave this as an unsigned int because it could potentially be 4G */
unsigned int lim_data;

/* WARNING!

   Some LISP-visible command-line options are set by XEmacs _before_ the
   data is dumped in building a --pdump XEmacs, but used _after_ it is
   restored in normal operation.  Thus the dump-time values overwrite the
   values XEmacs is getting at runtime.  Such variables must be saved
   before loading the dumpfile, and restored afterward.

   Therefore these variables may not be initialized in vars_of_emacs().

   The save/restore is done immediately before and after pdump_load() in
   main_1().  See that function for the current list of protected variables.

   Note that saving/restoring is only necessary for a few variables that are
     o command line arguments effective at runtime (as opposed to dump-time),
     o parsed before pdump_load, and
     o exported to Lisp via a DEFVAR.
*/

/* Nonzero means running XEmacs without interactive terminal.  */

int noninteractive;

/* Value of Lisp variable `noninteractive'.
   Normally same as C variable `noninteractive'
   but nothing terrible happens if user sets this one.

   Shadowed from the pdumper by `noninteractive'. */

int noninteractive1;

/* Nonzero means don't perform site-lisp searches at startup */
int inhibit_site_lisp;

/* Nonzero means don't perform site-modules searches at startup */
int inhibit_site_modules;

/* Nonzero means don't load user-init or site-start file */
int vanilla_inhibiting;

/* Nonzero means don't respect early packages at startup */
int inhibit_early_packages;

/* Nonzero means don't respect any packages at startup -- act as if they
   don't exist. */
int inhibit_all_packages;

/* Nonzero means don't load package autoloads at startup */
int inhibit_autoloads;

/* Nonzero means don't load the dump file (ignored if not PDUMP)  */

int nodumpfile;

/* Nonzero means print debug information about path searching */
int debug_paths;

/* Save argv and argc.  */
static Extbyte **initial_argv;	/* #### currently unused */
static int initial_argc;	/* #### currently unused */

static void sort_args (int argc, char **argv);

Lisp_Object Qkill_emacs_hook;
Lisp_Object Qsave_buffers_kill_emacs;

/* Nonzero if handling a fatal error already. */
int fatal_error_in_progress;

/* Nonzero means we're going down, so we better not run any hooks
   or do other non-essential stuff. */
int preparing_for_armageddon;

/* Nonzero means we're in an unstable situation and need to skip
   i18n conversions and such during printing. */
int inhibit_non_essential_printing_operations;

static JMP_BUF run_temacs_catch;

static int run_temacs_argc;
static Extbyte **run_temacs_argv;
static Extbyte *run_temacs_args;
static int run_temacs_argv_size;
static int run_temacs_args_size;

#ifdef _MSC_VER
static DWORD mswindows_handle_hardware_exceptions (DWORD code);
#endif

#ifdef WIN32_NATIVE
static DWORD CALLBACK wait_for_termination_signal (LPVOID handle);
#endif


/************************************************************************/
/*                   Functions to handle arguments                      */
/************************************************************************/

/* Code for dealing with Lisp access to the Unix command line */

static Lisp_Object
make_arg_list_1 (int argc, Extbyte **argv, int skip_args)
{
  Lisp_Object result = Qnil;
  REGISTER int i;

  for (i = argc - 1; i >= 0; i--)
    {
      if (i == 0 || i > skip_args)
	{
#ifdef WIN32_NATIVE
	  if (i == 0)
	    {
	      /* Do not trust to what crt0 has stuffed into argv[0] */
	      Extbyte *full_exe_path;
	      Lisp_Object fullpath;

	      full_exe_path = mswindows_get_module_file_name ();
	      assert (full_exe_path);
	      fullpath = build_tstr_string (full_exe_path);
	      xfree (full_exe_path);
	      result = Fcons (fullpath, result);
#ifdef HAVE_SHLIB
	      {
		Extbyte *fullpathext;

		/* Don't use full_exe_path directly because it's probably
		   in a different format. */
		LISP_STRING_TO_EXTERNAL (fullpath, fullpathext,
					 Qdll_filename_encoding);
		(void) dll_init (fullpathext);
	      }
#endif
	    }
	  else
#endif
	    result = Fcons (build_ext_string (argv[i],
					      Qcommand_argument_encoding),
			    result);
	}
    }
  return result;
}

Lisp_Object
make_arg_list (int argc, Extbyte **argv)
{
  return make_arg_list_1 (argc, argv, 0);
}

/* Calling functions are also responsible for calling free_argc_argv
   when they are done with the generated list. */
void
make_argc_argv (Lisp_Object argv_list, int *argc, Extbyte ***argv)
{
  Lisp_Object next;
  int n = XINT (Flength (argv_list));
  REGISTER int i;
  *argv = (Extbyte**) xmalloc ((n+1) * sizeof (Extbyte*));

  for (i = 0, next = argv_list; i < n; i++, next = XCDR (next))
    {
      const Extbyte *temp;
      CHECK_STRING (XCAR (next));

      LISP_STRING_TO_EXTERNAL (XCAR (next), temp, Qcommand_argument_encoding);
      (*argv) [i] = xstrdup (temp);
    }
  (*argv) [n] = 0;
  *argc = i;
}

void
free_argc_argv (Extbyte **argv)
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
init_cmdargs (int argc, Extbyte **argv, int skip_args)
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



/* Test whether the next argument in ARGV matches SSTR or a prefix of LSTR
   (at least MINLEN characters; if MINLEN is 0, set to size of LSTR).  If
   so, then if VALPTR is non-null (the argument is supposed to have a
   value) store in *VALPTR either the next argument or the portion of this
   one after the equal sign.  ARGV is read starting at position *SKIPPTR;
   this index is advanced by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static int
argmatch (char **argv, int argc, char *sstr, char *lstr,
	  int minlen, char **valptr, int *skipptr)
{
  char *p = NULL;
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
	    ? p - arg : (int) strlen (arg));
  if (lstr && !minlen)
    minlen = strlen (lstr);
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


/************************************************************************/
/*                   main and friends: XEmacs startup                   */
/************************************************************************/

/* Make stack traces always identify version + configuration */
#define main_1 STACK_TRACE_EYE_CATCHER

/* This function is not static, so that the compiler is less likely to
   inline it, which would make it not show up in stack traces.

   The restart argument is a flag that indicates that main_1 is now
   being called for the second time in this invocation of xemacs; this
   happens as a result of using `run-temacs' in the command line, when
   invoking a bare (without dumped data) XEmacs (i.e. `temacs' with
   the conventional dumper or `xemacs -nd' with the pdumper).  See
   Frun_emacs_from_temacs().

   restart interacts with initialized as follows (per Olivier Galibert):

     It's perverted.

     initialized==0 => temacs
     initialized!=0 && restart!=0 => run-temacs
     initialized!=0 && restart==0 => either xemacs after conventional dump,
                                     or xemacs post pdump_load()
*/
DECLARE_DOESNT_RETURN (main_1 (int, Extbyte **, Extbyte **, int));
DOESNT_RETURN
main_1 (int argc, Extbyte **argv, Extbyte **envp, int restart)
{
  char stack_bottom_variable;
  int skip_args = 0;
  Lisp_Object load_me;
  int inhibit_window_system;
#ifdef NeXT
  extern int malloc_cookie;
#endif

  /* !!#### Under MS Windows, this should all be rewritten to deal with
     Unicode arguments and environment.  We need to retrieve the command
     line with GetCommandLine and convert to argv format with
     CommandLineToArgvW.  Unfortunately we have a bootstrapping problem
     currently because we can't initialize the Unicode tables until we've
     computed the location of data-directory, which doesn't happen till
     startup.el, which is way late.  We need to be dumping the Unicode
     data, which means we need to fix pdump to correctly dump the "union"
     format used by the tables. */

#if (!defined (SYSTEM_MALLOC) && !defined (HAVE_LIBMCHECK)	\
     && !defined (DOUG_LEA_MALLOC))
  /* Make sure that any libraries we link against haven't installed a
     hook for a gmalloc of a potentially incompatible version. */
  /* If we're using libmcheck, the hooks have already been initialized, */
  /* don't touch them. -slb */
  __malloc_hook = NULL;
  __realloc_hook = NULL;
  __free_hook = NULL;
#endif /* not SYSTEM_MALLOC or HAVE_LIBMCHECK or DOUG_LEA_MALLOC */

  noninteractive = 0;
  inhibit_non_essential_printing_operations = 1;

#ifdef NeXT
  /* 19-Jun-1995 -baw
   * NeXT secret magic, ripped from Emacs-for-NS by Carl Edman
   * <cedman@princeton.edu>.  Note that even Carl doesn't know what this
   * does; it was provided by NeXT, and it presumably makes NS's mallocator
   * work with dumping.  But malloc_jumpstart() and malloc_freezedry() in
   * unexnext.c are both completely undocumented, even in NS header files!
   * But hey, it solves all NS related memory problems, so who's
   * complaining? */
  if (initialized && malloc_jumpstart (malloc_cookie) != 0)
    stderr_out ("malloc jumpstart failed!\n");
#endif /* NeXT */

  /*
#if defined (GNU_MALLOC) && \
    defined (ERROR_CHECK_MALLOC) && \
    !defined (HAVE_LIBMCHECK)
  */
#if defined(LOSING_GCC_DESTRUCTOR_FREE_BUG)
  /* Prior to XEmacs 21, this was `#if 0'ed out.  */
  /* I'm enabling this because it is the only reliable way I've found to */
  /* prevent a very annoying problem where GCC will attempt to free(3) */
  /* memory at exit() and cause a coredump. */
  init_free_hook ();
#endif

  sort_args (argc, argv);

#if defined (WIN32_NATIVE) || defined (_SCO_DS)
  environ = envp;
#endif

  /* Record (approximately) where the stack begins.  */
  stack_bottom = &stack_bottom_variable;

#ifdef USG_SHARED_LIBRARIES
  if (bss_end)
    brk ((void *) bss_end);
#endif

  clearerr (stdin);

#if defined (HAVE_MMAP) && defined (REL_ALLOC)
  /* ralloc can only be used if using the GNU memory allocator. */
  init_ralloc ();
#elif defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
  if (initialized)
    init_ralloc();
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

  /* NOTE NOTE NOTE: Keep the following args in sync with the big list of
     arguments below in standard_args[], with the help text in startup.el,
     and with the list of non-clobbered variables near where pdump_load()
     is called! */

  /* Handle the -sd/--show-dump-id switch, which means show the hex dump_id
     and quit */
  if (argmatch (argv, argc, "-sd", "--show-dump-id", 0, NULL, &skip_args))
    {
#ifdef PDUMP
      printf ("%08x\n", dump_id);
#else
      printf ("Portable dumper not configured; -sd just forces exit.\n");
#endif
      exit (0);
    }

  /* Handle the -t switch, which specifies filename to use as terminal */
  {
    Extbyte *term;
    if (argmatch (argv, argc, "-t", "--terminal", 0, &term, &skip_args))
      {
	retry_close (0);
	retry_close (1);
	if (open (term, O_RDWR | OPEN_BINARY, 2) < 0)
	  fatal ("%s: %s", term, strerror (errno));
	dup (0);
	if (! isatty (0))
	  fatal ("%s: not a tty", term);

#if 0
	stderr_out ("Using %s", ttyname (0));
#endif
	stderr_out ("Using %s", term);
	inhibit_window_system = 1;	/* -t => -nw */
      }
  }

  /* Handle the --no-dump-file/-nd switch, which means don't load the dump
     file (ignored when not using pdump) */
  if (argmatch (argv, argc, "-nd", "--no-dump-file", 0, NULL, &skip_args))
    nodumpfile = 1;

  /* Handle -nw switch */
  if (argmatch (argv, argc, "-nw", "--no-windows", 0, NULL, &skip_args))
    inhibit_window_system = 1;

  /* Handle the -batch switch, which means don't do interactive display.  */
  if (argmatch (argv, argc, "-batch", "--batch", 0, NULL, &skip_args))
    {
#if 0 /* I don't think this is correct. */
      inhibit_autoloads = 1;
#endif
      noninteractive = 1;
    }

#ifdef WIN32_NATIVE
  {
    /* Since we aren't a console application, we can't easily be terminated
       using ^C. (We aren't a console application to avoid Windows from
       automatically and unwantedly creating a console window for us.  If
       only the Windows designers had some sense in them and didn't create
       this artificial console/non-console distinction!) Therefore, we set
       up a communication path with i.exe so that when a ^C is sent to it
       (using GenerateConsoleCtrlEvent), it in turn signals us to commit
       suicide. (This is cleaner than using TerminateProcess()).  This
       makes (e.g.) the "Stop Build" command from VC++ correctly terminate
       XEmacs. */

    char *heventstr;
    if (argmatch (argv, argc, "-mswindows-termination-handle", 0, 0,
		  &heventstr, &skip_args))
      {
	HANDLE hevent = (HANDLE) atol (heventstr);
	DWORD unused;
	HANDLE h_thread = CreateThread (NULL, 0, wait_for_termination_signal,
					(void *) hevent, 0, &unused);
	CloseHandle (h_thread);
      }
  }

  /* Handle the -nuni switch, which forces XEmacs to use the ANSI
     versions of Unicode-split API's even on Windows NT, which has
     full Unicode support.  This helps flush out problems in the code
     we've written to convert between ANSI and Unicode. */
  if (argmatch (argv, argc, "-nuni", "--no-unicode-lib-calls", 0, NULL,
		&skip_args))
    no_mswin_unicode_lib_calls = 1;
#endif /* WIN32_NATIVE */

  if (argmatch (argv, argc, "-debug-paths", "--debug-paths",
		0, NULL, &skip_args))
    debug_paths = 1;

  /* Handle (maybe partially) some inhibiting flags.  Packages are searched
     prior to the rest of the command line being parsed in startup.el. */

  if (argmatch (argv, argc, "-no-packages", "--no-packages",
		0, NULL, &skip_args))
    {
      inhibit_all_packages = 1;
      inhibit_early_packages = 1;
      vanilla_inhibiting = 1;
    }

  if (argmatch (argv, argc, "-no-early-packages", "--no-early-packages",
		0, NULL, &skip_args))
    inhibit_early_packages = 1;

#ifdef HAVE_SHLIB
  if (argmatch (argv, argc, "-no-site-modules", "--no-site-modules",
		0, NULL, &skip_args))
#endif
    inhibit_site_modules = 1;

  if (argmatch (argv, argc, "-vanilla", "--vanilla",
		0, NULL, &skip_args))
    {
      inhibit_early_packages = 1;
      vanilla_inhibiting = 1;
    }

  if (argmatch (argv, argc, "-no-autoloads", "--no-autoloads",
		0, NULL, &skip_args))
    {
      inhibit_autoloads = 1;
      inhibit_early_packages = 1;
      vanilla_inhibiting = 1;
    }

  /* Partially handle the -version and -help switches: they imply -batch,
     but are not removed from the list. */
  if (argmatch (argv, argc, "-help", "--help",   3, NULL, &skip_args))
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
#ifdef HAVE_GTK
      {
	char *dpy = getenv ("DISPLAY");
	if (dpy && dpy[0])
	  display_use = "gtk";
      }
#endif
#ifdef HAVE_MS_WINDOWS
      if (strcmp (display_use, "x") != 0)
	display_use = "mswindows";
#endif /* HAVE_MS_WINDOWS */
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

#ifdef HAVE_WIN32_CODING_SYSTEMS
  init_win32_very_early ();
#endif
#ifdef HAVE_MS_WINDOWS
  /* Depends on XEUNICODE_P, only accurate after the previous call */
  init_mswindows_dde_very_early (); /* DDE needs to be initialized early so
				       that the client doesn't give up
				       waiting.  */
#endif

  /* Now initialize the Lisp engine and the like.  Done only during
     dumping.  No dependence on anything that may be in the user's
     environment when the dumped XEmacs is run.

     We try to do things in an order that minimizes the non-obvious
     dependencies between functions. */

  /* purify_flag 1 is correct even if CANNOT_DUMP.
   * loadup.el will set to nil at end. */

  purify_flag = 0;
#ifdef PDUMP
  if (restart)
    initialized = 1;
  else if (nodumpfile)
    {
      initialized = 0;
      purify_flag = 1;
    }
  else
    {

      /* Keep command options from getting stomped.

      Some LISP-visible options are changed by XEmacs _after_ the data is
      dumped in building a --pdump XEmacs, but _before_ it is restored in
      normal operation.  Thus the restored values overwrite the values
      XEmacs is getting at run-time.  Such variables must be saved here,
      and restored after loading the dumped data.

      (Remember: Only LISP-visible options that are set up to this point
      need to be listed here.)
      */

      /* noninteractive1 is saved in noninteractive, which isn't
	 LISP-visible */
      int inhibit_early_packages_save = inhibit_early_packages;
      int inhibit_autoloads_save      = inhibit_autoloads;
      int inhibit_all_packages_save   = inhibit_all_packages;
      int vanilla_inhibiting_save     = vanilla_inhibiting;
      int debug_paths_save            = debug_paths;
      int inhibit_site_lisp_save      = inhibit_site_lisp;
      int inhibit_site_modules_save   = inhibit_site_modules;

      initialized = pdump_load (argv[0]);

      /* Now unstomp everything */
      noninteractive1        = noninteractive;
      inhibit_early_packages = inhibit_early_packages_save;
      inhibit_autoloads      = inhibit_autoloads_save;
      inhibit_all_packages   = inhibit_all_packages_save;
      vanilla_inhibiting     = vanilla_inhibiting_save;
      debug_paths            = debug_paths_save;
      inhibit_site_lisp      = inhibit_site_lisp_save;
      inhibit_site_modules   = inhibit_site_modules_save;

      if (initialized)
	run_temacs_argc = -1;
      else
	purify_flag = 1;
    }
#else
  if (!initialized)
    purify_flag = 1;
#endif

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
	 interned.  This depends on init_alloc_once_early(). */
      init_symbols_once_early ();

      /* Declare the basic symbols pertaining to errors,
	 So that DEFERROR*() can be called. */
      init_errors_once_early ();

      /* Make sure that opaque pointers can be created. */
      init_opaque_once_early ();

      /* Make sure that hash tables can be created. */
      init_elhash_once_early ();

      /* Make sure that eistrings can be created. */
      init_eistring_once_early ();
    }

  /* The following will get called in raw-temacs, post-dump/pdump-load XEmacs,
     and run-temacs. */

  /* Initialize some vars that will also be reset post-dumping */
  init_alloc_early ();

  if (!initialized)
    {
      /* Now declare all the symbols and define all the Lisp primitives.

	 The *only* thing that the syms_of_*() functions are allowed to do
	 is call one of the following:

	 INIT_LRECORD_IMPLEMENTATION()
	 defsymbol(), DEFSYMBOL(), or DEFSYMBOL_MULTIWORD_PREDICATE()
	 defsubr() (i.e. DEFSUBR)
	 deferror(), DEFERROR(), or DEFERROR_STANDARD()
	 defkeyword() or DEFKEYWORD()
	 Fput()

	 Order does not matter in these functions.
	 */

      syms_of_abbrev ();
      syms_of_alloc ();
      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_chartab ();
      syms_of_cmdloop ();
      syms_of_cmds ();
      syms_of_console ();
      syms_of_data ();
#ifdef DEBUG_XEMACS
      syms_of_debug ();
      syms_of_tests ();
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
#ifdef HAVE_X_WINDOWS
      syms_of_event_Xt ();
#endif
#ifdef HAVE_GTK
      syms_of_event_gtk ();
#endif
#ifdef HAVE_DRAGNDROP
      syms_of_dragdrop ();
#endif
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
#ifdef USE_C_FONT_LOCK
      syms_of_font_lock ();
#endif /* USE_C_FONT_LOCK */
      syms_of_frame ();
      syms_of_general ();
      syms_of_glyphs ();
      syms_of_glyphs_eimage ();
      syms_of_glyphs_shared ();
      syms_of_glyphs_widget ();
      syms_of_gui ();
      syms_of_gutter ();
      syms_of_indent ();
      syms_of_intl ();
      syms_of_keymap ();
      syms_of_lread ();
      syms_of_macros ();
      syms_of_marker ();
      syms_of_md5 ();
#ifdef HAVE_DATABASE
      syms_of_database ();
#endif
#ifdef HAVE_MENUBARS
      syms_of_menubar ();
#endif
      syms_of_minibuf ();
#ifdef HAVE_SHLIB
      syms_of_module ();
#endif
      syms_of_objects ();
      syms_of_print ();
#if !defined (NO_SUBPROCESSES)
      syms_of_process ();
#ifdef HAVE_WIN32_PROCESSES
      syms_of_process_nt ();
#endif
#endif
      syms_of_profile ();
#if defined (HAVE_MMAP) && defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
      syms_of_ralloc ();
#endif /* HAVE_MMAP && REL_ALLOC */
      syms_of_rangetab ();
      syms_of_redisplay ();
      syms_of_search ();
      syms_of_select ();
      syms_of_signal ();
      syms_of_sound ();
      syms_of_specifier ();
      syms_of_symbols ();
      syms_of_syntax ();
#ifdef HAVE_SCROLLBARS
      syms_of_scrollbar ();
#endif
      syms_of_text ();
#ifdef HAVE_TOOLBARS
      syms_of_toolbar ();
#endif
      syms_of_undo ();
      syms_of_widget ();
      syms_of_window ();

#ifdef HAVE_TTY
      syms_of_console_tty ();
      syms_of_device_tty ();
      syms_of_frame_tty ();
      syms_of_objects_tty ();
#endif

#ifdef HAVE_GTK
      syms_of_device_gtk ();
      syms_of_frame_gtk ();
      syms_of_glyphs_gtk ();
      syms_of_objects_gtk ();
      syms_of_ui_gtk ();
      syms_of_select_gtk ();
#ifdef HAVE_DIALOGS
      syms_of_dialog_gtk ();
#endif
#ifdef HAVE_MENUBARS
      syms_of_menubar_gtk ();
#endif
      syms_of_select_gtk ();

#ifdef HAVE_GUI_OBJECTS
      syms_of_gui_gtk ();
#endif
#endif /* HAVE_GTK */

#ifdef HAVE_X_WINDOWS
#ifdef HAVE_BALLOON_HELP
      syms_of_balloon_x ();
#endif
      syms_of_device_x ();
#ifdef HAVE_X_DIALOGS
      syms_of_dialog_x ();
#endif
      syms_of_frame_x ();
      syms_of_glyphs_x ();
      syms_of_objects_x ();
#ifdef HAVE_MENUBARS
      syms_of_menubar_x ();
#endif
      syms_of_select_x ();
#ifdef HAVE_GUI_OBJECTS
      syms_of_gui_x ();
#endif
      syms_of_intl_x ();
#ifdef HAVE_XIM
#ifdef XIM_XLIB
      syms_of_input_method_xlib ();
#endif
#endif /* HAVE_XIM */
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_MS_WINDOWS
      syms_of_console_mswindows ();
      syms_of_device_mswindows ();
#ifdef HAVE_DIALOGS
      syms_of_dialog_mswindows ();
#endif
      syms_of_frame_mswindows ();
      syms_of_objects_mswindows ();
      syms_of_select_mswindows ();
      syms_of_glyphs_mswindows ();
#ifdef HAVE_GUI_OBJECTS
      syms_of_gui_mswindows ();
#endif
#ifdef HAVE_MENUBARS
      syms_of_menubar_mswindows ();
#endif
#ifdef HAVE_SCROLLBARS
      syms_of_scrollbar_mswindows ();
#endif
#endif	/* HAVE_MS_WINDOWS */
#ifdef HAVE_MSW_C_DIRED
      syms_of_dired_mswindows ();
#endif
#ifdef WIN32_NATIVE
      syms_of_nt ();
#endif
#if defined (WIN32_NATIVE) || defined (CYGWIN)
      syms_of_win32 ();
#endif

      syms_of_file_coding ();
      syms_of_unicode ();
#ifdef MULE
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

#ifdef HAVE_WIN32_CODING_SYSTEMS
      syms_of_intl_win32 ();
#endif

#ifdef SYMS_SYSTEM
      SYMS_SYSTEM;
#endif

#ifdef SYMS_MACHINE
      SYMS_MACHINE;
#endif

      /*
#if defined (GNU_MALLOC) && \
    defined (ERROR_CHECK_MALLOC) && \
    !defined (HAVE_LIBMCHECK)
      */
      /* Prior to XEmacs 21, this was `#if 0'ed out. -slb */
#if defined (LOSING_GCC_DESTRUCTOR_FREE_BUG)
      syms_of_free_hook ();
#endif

#ifdef TOOLTALK
      syms_of_tooltalk ();
#endif

#ifdef SUNPRO
      syms_of_sunpro ();
#endif

#ifdef HAVE_LDAP
      syms_of_eldap ();
#endif

#ifdef HAVE_GPM
      syms_of_gpmevent ();
#endif

#ifdef HAVE_POSTGRESQL
      syms_of_postgresql ();
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

#ifdef HAVE_GTK
      console_type_create_gtk ();
      console_type_create_select_gtk ();
      console_type_create_device_gtk ();
      console_type_create_frame_gtk ();
      console_type_create_objects_gtk ();
      console_type_create_glyphs_gtk ();
      console_type_create_redisplay_gtk ();
#ifdef HAVE_MENUBARS
      console_type_create_menubar_gtk ();
#endif
#ifdef HAVE_SCROLLBARS
      console_type_create_scrollbar_gtk ();
#endif
#ifdef HAVE_TOOLBARS
      console_type_create_toolbar_gtk ();
#endif
#ifdef HAVE_DIALOGS
      console_type_create_dialog_gtk ();
#endif
#endif /* HAVE_GTK */

#ifdef HAVE_X_WINDOWS
      console_type_create_x ();
      console_type_create_device_x ();
      console_type_create_frame_x ();
      console_type_create_glyphs_x ();
      console_type_create_select_x ();
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
#ifdef HAVE_X_DIALOGS
      console_type_create_dialog_x ();
#endif
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_MS_WINDOWS
      console_type_create_mswindows ();
      console_type_create_device_mswindows ();
      console_type_create_frame_mswindows ();
      console_type_create_objects_mswindows ();
      console_type_create_redisplay_mswindows ();
      console_type_create_glyphs_mswindows ();
      console_type_create_select_mswindows ();
# ifdef HAVE_SCROLLBARS
      console_type_create_scrollbar_mswindows ();
# endif
#ifdef HAVE_MENUBARS
      console_type_create_menubar_mswindows ();
#endif
#ifdef HAVE_TOOLBARS
      console_type_create_toolbar_mswindows ();
#endif
#ifdef HAVE_DIALOGS
      console_type_create_dialog_mswindows ();
#endif
#endif

      /* Now initialize the specifier types and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_SPECIFIER_TYPE()
	 SPECIFIER_HAS_METHOD()

	 For any given specifier type, the former macro must be called
	 before the any calls to the latter macro. */

      specifier_type_create ();

      specifier_type_create_image ();
      specifier_type_create_gutter ();
      specifier_type_create_objects ();
#ifdef HAVE_TOOLBARS
      specifier_type_create_toolbar ();
#endif

      /* Now initialize the coding system types and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_CODING_SYSTEM_TYPE()
	 CODING_SYSTEM_HAS_METHOD()

	 For any given coding system type, the former macro must be called
	 before the any calls to the latter macro. */

      coding_system_type_create ();
      coding_system_type_create_unicode ();
#ifdef HAVE_WIN32_CODING_SYSTEMS
      coding_system_type_create_intl_win32 ();
#endif
#ifdef MULE
      coding_system_type_create_mule_coding ();
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
      structure_type_create_hash_table ();

      /* Now initialize the image instantiator formats and associated symbols.
         Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 INITIALIZE_IMAGE_INSTANTIATOR_FORMAT()
	 IIFORMAT_HAS_METHOD()
	 IIFORMAT_VALID_KEYWORD()

	 For any given image instantiator format, the first macro must be
	 called before the any calls to the other macros. */

      image_instantiator_format_create ();
      image_instantiator_format_create_glyphs_eimage ();
      image_instantiator_format_create_glyphs_widget ();
#ifdef HAVE_TTY
      image_instantiator_format_create_glyphs_tty ();
#endif
#ifdef HAVE_X_WINDOWS
      image_instantiator_format_create_glyphs_x ();
#endif /* HAVE_X_WINDOWS */
#ifdef HAVE_MS_WINDOWS
      image_instantiator_format_create_glyphs_mswindows ();
#endif /* HAVE_MSWINDOWS_WINDOWS */
#ifdef HAVE_GTK
      image_instantiator_format_create_glyphs_gtk ();
#endif

      /* Now initialize the lstream types and associated symbols.
	 Other than the first function below, the functions may
	 make exactly the following function/macro calls:

	 LSTREAM_HAS_METHOD()

	 */

      lstream_type_create ();
      lstream_type_create_file_coding ();
#if defined (HAVE_MS_WINDOWS) && !defined (HAVE_MSG_SELECT)
      lstream_type_create_mswindows_selectable ();
#endif

      /* Initialize processes implementation.
	 The functions may make exactly the following function/macro calls:

	 PROCESS_HAS_METHOD()
      */
#ifdef HAVE_UNIX_PROCESSES
      process_type_create_unix ();
#endif
#ifdef HAVE_WIN32_PROCESSES
      process_type_create_nt ();
#endif

      /* Now initialize most variables.

	 These functions may do exactly the following:

	 -- assigning a symbol or constant value to a variable
	 -- using a global variable that has been initialized
	    earlier on in the same function
	 -- DEFVAR_INT()
	 -- DEFVAR_LISP()
	 -- DEFVAR_BOOL()
	 -- DEFER_GETTEXT()
	 -- staticpro*()
	 -- xmalloc*(), xnew*(), and friends
	 -- Dynarr_*()
	 -- Blocktype_*()
	 -- Fprovide(symbol)
	 -- intern()
	 -- Fput()
         -- dump_add_*()
         -- C library functions with no external dependencies, e.g. str*()
	 -- defsymbol(), if it's absolutely necessary and you're sure that
	    the symbol isn't referenced anywhere else in the initialization
	    code
	 -- Fset() on a symbol that is unbound
	 -- Any of the object-creating functions in alloc.c: e.g.
	    - make_string()
	    - build_intstring()
	    - build_string()
	    - make_vector()
	    - make_int()
	    - make_char()
	    - make_extent()
	    - alloc_lcrecord()
	    - Fcons()
	    - listN()
            - make_lcrecord_list()
	 -- make_opaque_ptr()
	 -- make_lisp_hash_table() (not allowed in 21.4!)
         -- certain specifier creation functions (but be careful; see
            glyphs.c for examples)

	 perhaps a few others.

	 NO EXTERNAL-FORMAT CONVERSIONS.

         NB:  Initialization or assignment should not be done here to certain
           variables settable from the command line.  See the comment above
           the call to pdump_load() in main_1().  This caveat should only
           apply to vars_of_emacs().
       */

      /* Now allow Fprovide() statements to be made. */
      init_provide_once ();

      /* Do that before any specifier creation (esp. vars_of_glyphs()) */
      vars_of_specifier ();

      vars_of_abbrev ();
      vars_of_alloc ();
      vars_of_buffer ();
      vars_of_bytecode ();
      vars_of_callint ();
      vars_of_chartab ();
      vars_of_cmdloop ();
      vars_of_cmds ();
      vars_of_console ();
      vars_of_data ();
#ifdef DEBUG_XEMACS
      vars_of_debug ();
      vars_of_tests ();
#endif
      vars_of_console_stream ();
      vars_of_device ();
#ifdef HAVE_DIALOGS
      vars_of_dialog ();
#endif
      vars_of_dired ();
      vars_of_doc ();
#ifdef HAVE_DRAGNDROP
      vars_of_dragdrop ();
#endif
      vars_of_editfns ();
      vars_of_emacs ();
      vars_of_eval ();
      init_eval_semi_early ();

#ifdef HAVE_X_WINDOWS
      vars_of_event_Xt ();
#endif
#if defined(HAVE_TTY) && (defined (DEBUG_TTY_EVENT_STREAM) || !defined (HAVE_X_WINDOWS))
      vars_of_event_tty ();
#endif
#ifdef HAVE_MS_WINDOWS
      vars_of_event_mswindows ();
#endif
      vars_of_event_stream ();

      vars_of_events ();
      vars_of_extents ();
      vars_of_faces ();
      vars_of_file_coding ();
      vars_of_fileio ();
#ifdef CLASH_DETECTION
      vars_of_filelock ();
#endif
      vars_of_floatfns ();
      vars_of_fns ();
#ifdef USE_C_FONT_LOCK
      vars_of_font_lock ();
#endif /* USE_C_FONT_LOCK */
      vars_of_frame ();
      vars_of_glyphs ();
      vars_of_glyphs_eimage ();
      vars_of_glyphs_widget ();
      vars_of_gui ();
      vars_of_gutter ();
      vars_of_indent ();
      vars_of_insdel ();
      vars_of_intl ();
#ifdef HAVE_WIN32_CODING_SYSTEMS
      vars_of_intl_win32 ();
#endif
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
      vars_of_database ();
#endif
#ifdef HAVE_MENUBARS
      vars_of_menubar ();
#endif
      vars_of_minibuf ();
#ifdef HAVE_SHLIB
      vars_of_module ();
#endif
#ifdef WIN32_NATIVE
      vars_of_nt ();
#endif
      vars_of_objects ();
      vars_of_print ();

#ifndef NO_SUBPROCESSES
      vars_of_process ();
#ifdef HAVE_UNIX_PROCESSES
      vars_of_process_unix ();
#endif
#ifdef HAVE_WIN32_PROCESSES
      vars_of_process_nt ();
#endif
#endif

      vars_of_profile ();
#if defined (HAVE_MMAP) && defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
      vars_of_ralloc ();
#endif /* HAVE_MMAP && REL_ALLOC */
      vars_of_redisplay ();
      vars_of_regex ();
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar ();
#endif
      vars_of_search ();
      vars_of_select ();
      vars_of_sound ();
      vars_of_symbols ();
      vars_of_syntax ();
      vars_of_text ();
#ifdef HAVE_TOOLBARS
      vars_of_toolbar ();
#endif
      vars_of_undo ();
      vars_of_window ();
#if defined (WIN32_NATIVE) || defined (CYGWIN)
      vars_of_win32 ();
#endif

#ifdef HAVE_TTY
      vars_of_console_tty ();
      vars_of_frame_tty ();
      vars_of_objects_tty ();
#endif

#ifdef HAVE_GTK
      vars_of_device_gtk ();
#ifdef HAVE_DIALOGS
      vars_of_dialog_gtk ();
#endif
      vars_of_event_gtk ();
      vars_of_frame_gtk ();
      vars_of_glyphs_gtk ();
      vars_of_ui_gtk ();
#ifdef HAVE_MENUBARS
      vars_of_menubar_gtk ();
#endif
      vars_of_objects_gtk ();
      vars_of_select_gtk ();
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar_gtk ();
#endif
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
      vars_of_gui_gtk ();
#endif
#endif /* HAVE_GTK */

#ifdef HAVE_X_WINDOWS
#ifdef HAVE_BALLOON_HELP
      vars_of_balloon_x ();
#endif
      vars_of_device_x ();
#ifdef HAVE_X_DIALOGS
      vars_of_dialog_x ();
#endif
      vars_of_frame_x ();
      vars_of_glyphs_x ();
#ifdef HAVE_MENUBARS
      vars_of_menubar_x ();
#endif
      vars_of_objects_x ();
      vars_of_select_x ();
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar_x ();
#endif
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_X_DIALOGS) || defined (HAVE_TOOLBARS)
      vars_of_gui_x ();
#endif
#endif /* HAVE_X_WINDOWS */


#ifdef HAVE_MS_WINDOWS
      vars_of_device_mswindows ();
      vars_of_console_mswindows ();
      vars_of_frame_mswindows ();
      vars_of_objects_mswindows ();
      vars_of_select_mswindows ();
      vars_of_glyphs_mswindows ();
#ifdef HAVE_SCROLLBARS
      vars_of_scrollbar_mswindows ();
#endif
#ifdef HAVE_MENUBARS
      vars_of_menubar_mswindows ();
#endif
#ifdef HAVE_MSW_C_DIRED
      vars_of_dired_mswindows ();
#endif
#ifdef HAVE_DIALOGS
      vars_of_dialog_mswindows ();
#endif
#endif	/* HAVE_MS_WINDOWS */

#ifdef MULE
      vars_of_mule_ccl ();
      vars_of_mule_charset ();
#endif
      vars_of_file_coding ();
      vars_of_unicode ();
#ifdef MULE
      vars_of_mule_coding ();
#ifdef HAVE_WNN
      vars_of_mule_wnn ();
#endif
#ifdef HAVE_CANNA
      vars_of_mule_canna ();
#endif /* HAVE_CANNA */
#endif /* MULE */

#ifdef TOOLTALK
      vars_of_tooltalk ();
#endif

#ifdef SUNPRO
      vars_of_sunpro ();
#endif

#ifdef HAVE_LDAP
      vars_of_eldap ();
#endif

#ifdef HAVE_POSTGRESQL
      vars_of_postgresql ();
#endif

#ifdef HAVE_GPM
      vars_of_gpmevent ();
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
      specifier_vars_of_gutter ();
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

#ifdef MULE
      /* This depends on vars initialized in vars_of_unicode(). */
      complex_vars_of_mule_charset ();
#endif
      /* This one doesn't depend on anything really, and could go into
	 vars_of_(), but lots of lots of code gets called and it's easily
	 possible that it could get changed to require being a
	 complex_vars_of_(), for example if a charset appears anywhere,
	 then we suddenly have dependence on the previous call. */
      complex_vars_of_file_coding ();
#ifdef HAVE_WIN32_CODING_SYSTEMS
      complex_vars_of_intl_win32 ();
#endif

      /* Depends on specifiers. */
      complex_vars_of_faces ();

      /* This calls allocate_glyph(), which creates specifiers
	 and also relies on a variable (Vthe_nothing_vector) initialized
	 above. */
      complex_vars_of_glyphs ();

      /* These rely on the glyphs just created in the previous function,
	 and call Fadd_spec_to_specifier(), which relies on various
	 variables initialized above. */
#ifdef HAVE_GTK
      complex_vars_of_glyphs_gtk ();
#endif
#ifdef HAVE_X_WINDOWS
      complex_vars_of_glyphs_x ();
#endif
#ifdef HAVE_MS_WINDOWS
      complex_vars_of_glyphs_mswindows ();
#endif

      /* This calls Fmake_glyph_internal(). */
      complex_vars_of_alloc ();

      /* This calls Fmake_glyph_internal(). */
#ifdef HAVE_MENUBARS
      complex_vars_of_menubar ();
#endif

#ifdef HAVE_SCROLLBARS
      /* This calls Fmake_glyph_internal(). */
      complex_vars_of_scrollbar ();
#endif

      /* This calls allocate_glyph(). */
      complex_vars_of_frame ();

      /* This calls Fcopy_category_table() under Mule, which calls who
         knows what. */
      complex_vars_of_chartab ();

      /* This calls Fput_char_table(), which (under Mule) depends on the
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
      complex_vars_of_emacs ();

      /* This creates a couple of basic keymaps and depends on Lisp
	 hash tables and Ffset() (both of which depend on some variables
	 initialized in the vars_of_*() section) and possibly other
	 stuff. */
      complex_vars_of_keymap ();

#ifdef ERROR_CHECK_GC
      {
	extern int always_gc;
	if (always_gc)                /* purification debugging hack */
	  garbage_collect_1 ();
      }
#endif
#ifdef PDUMP
    }
  else if (!restart)		      /* after successful pdump_load()
					 (note, we are inside ifdef PDUMP) */
    {
      reinit_alloc_once_early ();
      reinit_symbols_once_early ();
      reinit_opaque_once_early ();
      reinit_eistring_once_early ();

      reinit_console_type_create_stream ();
#ifdef HAVE_TTY
      reinit_console_type_create_tty ();
#endif
#ifdef HAVE_X_WINDOWS
      reinit_console_type_create_x ();
      reinit_console_type_create_device_x ();
#endif
#ifdef HAVE_MS_WINDOWS
      reinit_console_type_create_mswindows ();
#endif
#ifdef HAVE_GTK
      reinit_console_type_create_gtk ();
#endif

      reinit_specifier_type_create ();
      reinit_specifier_type_create_image ();
      reinit_specifier_type_create_gutter ();
      reinit_specifier_type_create_objects ();
#ifdef HAVE_TOOLBARS
      reinit_specifier_type_create_toolbar ();
#endif

      structure_type_create ();

      reinit_coding_system_type_create ();
      reinit_coding_system_type_create_unicode ();
#ifdef HAVE_WIN32_CODING_SYSTEMS
      reinit_coding_system_type_create_intl_win32 ();
#endif
#ifdef MULE
      reinit_coding_system_type_create_mule_coding ();
#endif

      structure_type_create_chartab ();
      structure_type_create_faces ();
      structure_type_create_rangetab ();
      structure_type_create_hash_table ();

      lstream_type_create ();
      lstream_type_create_file_coding ();
#if defined (HAVE_MS_WINDOWS) && !defined (HAVE_MSG_SELECT)
      lstream_type_create_mswindows_selectable ();
#endif
#ifdef HAVE_UNIX_PROCESSES
      process_type_create_unix ();
#endif
#ifdef HAVE_WIN32_PROCESSES
      process_type_create_nt ();
#endif

      reinit_vars_of_buffer ();
      reinit_vars_of_console ();
#ifdef DEBUG_XEMACS
      reinit_vars_of_debug ();
#endif
      reinit_vars_of_device ();
      reinit_vars_of_eval ();
#if defined(HAVE_TTY) && (defined (DEBUG_TTY_EVENT_STREAM) || !defined (HAVE_X_WINDOWS))
      reinit_vars_of_event_tty ();
#endif
      reinit_vars_of_event_stream ();
      reinit_vars_of_events ();
      reinit_vars_of_extents ();
      reinit_vars_of_file_coding ();
      reinit_vars_of_fileio ();
#ifdef USE_C_FONT_LOCK
      reinit_vars_of_font_lock ();
#endif /* USE_C_FONT_LOCK */
      reinit_vars_of_glyphs ();
      reinit_vars_of_glyphs_widget ();
      reinit_vars_of_insdel ();
      reinit_vars_of_lread ();
      reinit_vars_of_lstream ();
      reinit_vars_of_minibuf ();
#ifdef HAVE_SHLIB
      reinit_vars_of_module ();
#endif
      reinit_vars_of_objects ();
      reinit_vars_of_print ();
      reinit_vars_of_search ();
      reinit_vars_of_text ();
      reinit_vars_of_undo ();
      reinit_vars_of_unicode ();
      reinit_vars_of_window ();

#ifdef HAVE_MS_WINDOWS
      reinit_vars_of_event_mswindows ();
      reinit_vars_of_frame_mswindows ();
      reinit_vars_of_object_mswindows ();
#endif

#ifdef HAVE_GTK
      reinit_vars_of_event_gtk ();
      reinit_vars_of_menubar_gtk ();
#endif

#ifdef HAVE_X_WINDOWS
      reinit_vars_of_device_x ();
      reinit_vars_of_event_Xt ();
#ifdef HAVE_SCROLLBARS
      reinit_vars_of_scrollbar_x ();
#endif
#ifdef HAVE_MENUBARS
      reinit_vars_of_menubar_x ();
#endif
      reinit_vars_of_select_x ();
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_X_DIALOGS) || defined (HAVE_TOOLBARS)
      reinit_vars_of_gui_x ();
#endif
#endif /* HAVE_X_WINDOWS */

#ifdef MULE
      reinit_vars_of_mule_coding ();
#endif
#if defined (MULE) && defined (HAVE_WNN)
      reinit_vars_of_mule_wnn ();
#endif

      reinit_complex_vars_of_buffer_runtime_only ();
      reinit_complex_vars_of_console_runtime_only ();
      reinit_complex_vars_of_minibuf ();
#endif /* PDUMP */
    }

  /* CONGRATULATIONS!!!  We have successfully initialized the Lisp
     engine. */

  if (initialized)
    init_eval_semi_early ();

#ifdef MULE
  init_mule_charset ();
#endif

  /* Now do further initialization/setup of stuff that is not needed by the
     syms_of_() routines.  This involves stuff that only is enabled in
     an interactive run (redisplay, user input, etc.) and stuff that is
     not needed until we start loading Lisp code (the reader).  A lot
     of this stuff involves querying the current environment and needs
     to be done both at dump time and at run time.  Some will be done
     only at run time, by querying the `initialized' variable. */

#if defined (WIN32_NATIVE) || defined (CYGWIN)
  init_intl_win32 (); /* Under Windows, determine whether we use Unicode
			 or ANSI to call the system routines -- i.e.
			 determine what the coding system `mswindows-tstr'
			 is aliased to */
#endif
  init_buffer_1 ();	/* Create *scratch* buffer; init_intl() is going to
			   call Lisp code (the very first code we call),
			   and needs a current buffer */
#ifdef MULE
  init_intl (); /* Figure out the locale and set native and
		   file-name coding systems, initialize the Unicode tables
		   so that we will be able to process non-ASCII from here
		   on out! */
#endif

  init_xemacs_process (); /* Set up the process environment (so that
			     egetenv works), the basic directory variables
			     (exec-directory and so on), and stuff related
			     to subprocesses.  This should be first because
			     many of the functions below call egetenv() to
			     get environment variables. */

#ifdef WIN32_NATIVE
  /*
   * For Win32, call init_environment() to properly enter environment/registry
   * variables into Vprocess_environment.
   */
  init_mswindows_environment ();
#endif

  init_initial_directory ();		/* get the directory to use for the
					   "*scratch*" buffer, etc. */

  init_lread ();	/* Set up the Lisp reader. */
  init_cmdargs (argc, (Extbyte **) argv,
		skip_args);	/* Create list Vcommand_line_args */
  init_buffer_2 ();	/* Set default directory of *scratch* buffer */

#ifdef WIN32_NATIVE
  init_nt ();
  init_select_mswindows ();
#endif

  init_redisplay ();      /* Determine terminal type.
			     init_sys_modes uses results */
  init_frame ();
  init_event_stream (); /* Set up so we can get user input. */
  init_macros (); /* set up so we can run macros. */
  init_editfns (); /* Determine the name of the user we're running as */
#ifdef SUNPRO
  init_sunpro (); /* Set up Sunpro usage tracking */
#endif
#if defined (WIN32_NATIVE) || defined (CYGWIN)
  init_win32 ();
#endif
#if defined (HAVE_NATIVE_SOUND) && defined (hp9000s800)
  init_hpplay ();
#endif
#ifdef HAVE_POSTGRESQL
  /* Set some values taken from environment variables */
  init_postgresql_from_environment ();
#endif
#ifdef HAVE_TTY
  init_device_tty ();
#endif
  init_console_stream (restart); /* Create the first console */

  /* try to get the actual pathname of the exec file we are running */
  if (!restart)
    {
      Vinvocation_name = Fcar (Vcommand_line_args);
      if (XSTRING_DATA(Vinvocation_name)[0] == '-')
	{
	  /* XEmacs as a login shell, oh goody! */
	  Vinvocation_name = build_intstring (egetenv ("SHELL"));
	}
      Vinvocation_directory = Vinvocation_name;

      if (!NILP (Ffile_name_directory (Vinvocation_name)))
	{
	  /* invocation-name includes a directory component -- presumably it
	     is relative to cwd, not $PATH */
	  Vinvocation_directory = Fexpand_file_name (Vinvocation_name,
						     Qnil);
	  Vinvocation_path = Qnil;
	}
      else
	{
	  Vinvocation_path = split_env_path ("PATH", NULL);
	  locate_file (Vinvocation_path, Vinvocation_name,
		       Vlisp_EXEC_SUFFIXES,
		       &Vinvocation_directory, X_OK);
	}

      if (NILP (Vinvocation_directory))
	Vinvocation_directory = Vinvocation_name;

      Vinvocation_name = Ffile_name_nondirectory (Vinvocation_directory);
      Vinvocation_directory = Ffile_name_directory (Vinvocation_directory);
    }

#if defined(HAVE_SHLIB) && !defined(WIN32_NATIVE)
  /* This is Unix only.  MS Windows NT has a library call that does
     The Right Thing on that system.  Rumor has it, this must be
     called for GNU dld in temacs and xemacs.  */
  {
    char *buf = (char *)ALLOCA (XSTRING_LENGTH (Vinvocation_directory)
				+ XSTRING_LENGTH (Vinvocation_name)
				+ 2);
    sprintf (buf, "%s/%s", XSTRING_DATA (Vinvocation_directory),
	     XSTRING_DATA (Vinvocation_name));

    C_STRING_TO_EXTERNAL (buf, buf, Qfile_name);
    /* All we can do is cry if an error happens, so ignore it. */
    (void) dll_init (buf);
  }
#endif

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
	/* !!#### need to be Mule-translating this, but later */
	load_me = build_string (argv[2 + skip_args]);
    }

#ifdef QUANTIFY
  if (initialized)
    quantify_start_recording_data ();
#endif /* QUANTIFY */

  initialized = 1;
  inhibit_non_essential_printing_operations = 0;

  /* This never returns.  */
  initial_command_loop (load_me);
  /* NOTREACHED */
}


/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args
{
  const char *name;
  const char *longname;
  int priority;
  int nargs;
};

static const struct standard_args standard_args[] =
{
  /* Handled by main_1 above: Each must have its own priority and must be
     in the order mentioned in main_1. */
  { "-sd", "--show-dump-id", 105, 0 },
  { "-t", "--terminal", 100, 1 },
  { "-nd", "--no-dump-file", 95, 0 },
  { "-nw", "--no-windows", 90, 0 },
  { "-batch", "--batch", 88, 0 },
#ifdef WIN32_NATIVE
  { "-mswindows-termination-handle", 0, 84, 1 },
  { "-nuni", "--no-unicode-lib-calls", 83, 0 },
#endif /* WIN32_NATIVE */
  { "-debug-paths", "--debug-paths", 82, 0 },
  { "-no-packages", "--no-packages", 81, 0 },
  { "-no-early-packages", "--no-early-packages", 80, 0 },
  { "-no-site-modules", "--no-site-modules", 78, 0 },
  { "-vanilla", "--vanilla", 76, 0 },
  { "-no-autoloads", "--no-autoloads", 74, 0 },
  { "-help", "--help", 72, 0 },
  { "-version", "--version", 70, 0 },
  { "-V", 0, 68, 0 },
  { "-d", "--display", 66, 1 },
  { "-display", 0, 64, 1 },

  /* Handled by command-line-early in startup.el: */
  { "-q", "--no-init-file", 50, 0 },
  { "-no-init-file", 0, 50, 0 },
  { "-no-site-file", "--no-site-file", 50, 0 },
  { "-unmapped", "--unmapped", 50, 0 },
  { "-u", "--user", 50, 1 },
  { "-user", 0, 50, 1 },
  { "-user-init-file", "--user-init-file", 50, 1 },
  { "-user-init-directory", "--user-init-directory", 50, 1 },
  { "-debug-init", "--debug-init", 50, 0 },

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

  { "-eol", "--enable-eol-detection", 2, 0 },
  { "-enable-eol-detection", 0, 2, 0 },
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
  char **new_argv = xnew_array (char *, argc);
  /* For each element of argv,
     the corresponding element of options is:
     0 for an option that takes no arguments,
     1 for an option that takes one argument, etc.
     -1 for an ordinary non-option argument.  */
  int *options  = xnew_array (int, argc);
  int *priority = xnew_array (int, argc);
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

  /* Copy the arguments, in order of decreasing priority, to NEW_ARGV.  */
  new_argv[0] = argv[0];
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

      /* Copy the highest priority remaining option, with its args, to NEW_ARGV.  */
      new_argv[to++] = argv[best];
      for (i = 0; i < options[best]; i++)
	new_argv[to++] = argv[best + i + 1];

      /* Clear out this option in ARGV.  */
      argv[best] = 0;
      for (i = 0; i < options[best]; i++)
	argv[best + i + 1] = 0;
    }

  memcpy (argv, new_argv, sizeof (char *) * argc);
  xfree (new_argv);
  xfree (options);
  xfree (priority);
}

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
   This will "restart" emacs with the specified command-line arguments.

   Martin thinks this function is most useful when using debugging
   tools like Purify or tcov that get confused by XEmacs' dumping.  */
     (int nargs, Lisp_Object *args))
{
  int ac;
  const Extbyte *wampum;
  int namesize;
  int total_len;
  Lisp_Object orig_invoc_name = Fcar (Vcommand_line_args);
  const Extbyte **wampum_all = alloca_array (const Extbyte *, nargs);
  int *wampum_all_len  = alloca_array (int, nargs);

  assert (!gc_in_progress);

  if (run_temacs_argc < 0)
    invalid_operation ("I've lost my temacs-hood.", Qunbound);

  /* Need to convert the orig_invoc_name and all of the arguments
     to external format. */

  TO_EXTERNAL_FORMAT (LISP_STRING, orig_invoc_name,
		      ALLOCA, (wampum, namesize),
		      Qnative);
  namesize++;

  for (ac = 0, total_len = namesize; ac < nargs; ac++)
    {
      CHECK_STRING (args[ac]);
      TO_EXTERNAL_FORMAT (LISP_STRING, args[ac],
			  ALLOCA, (wampum_all[ac], wampum_all_len[ac]),
			  Qnative);
      wampum_all_len[ac]++;
      total_len += wampum_all_len[ac];
    }
  DO_REALLOC (run_temacs_args, run_temacs_args_size, total_len, char);
  DO_REALLOC (run_temacs_argv, run_temacs_argv_size, nargs+2, char *);

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
  unbind_to (0); /* this closes loadup.el */
  purify_flag = 0;
  run_temacs_argc = nargs + 1;
#if defined (HEAP_IN_DATA) && !defined(PDUMP)
  report_sheap_usage (0);
#endif
  LONGJMP (run_temacs_catch, 1);
  RETURN_NOT_REACHED (Qnil)
}

/* ARGSUSED */
int
main (int argc, char **argv, char **envp)
{

#ifdef _MSC_VER
  /* Under VC++, access violations and the like are not sent through
     the standard signal() mechanism.  Rather, they need to be handled
     using the Microsoft "structured exception handling" mechanism,
     which vaguely resembles the C++ mechanisms. */
  __try
  {
#endif

  int     volatile vol_argc = argc;
  char ** volatile vol_argv = argv;
  char ** volatile vol_envp = envp;
  /* This is hairy.  We need to compute where the XEmacs binary was invoked
     from because temacs initialization requires it to find the lisp
     directories.  The code that recomputes the path is guarded by the
     restarted flag.  There are three possible paths I've found so far
     through this:

     temacs -- When running temacs for basic build stuff, the first main_1
      will be the only one invoked.  It must compute the path else there
      will be a very ugly bomb in startup.el (can't find obvious location
      for doc-directory data-directory, etc.).

     temacs w/ run-temacs on the command line -- This is run to bytecompile
      all the out of date dumped lisp.  It will execute both of the main_1
      calls and the second one must not touch the first computation because
      argc/argv are hosed the second time through.

     xemacs -- Only the second main_1 is executed.  The invocation path must
      computed but this only matters when running in place or when running
      as a login shell.

     As a bonus for straightening this out, XEmacs can now be run in place
     as a login shell.  This never used to work.

     As another bonus, we can now guarantee that
     (concat invocation-directory invocation-name) contains the filename
     of the XEmacs binary we are running.  This can now be used in a
     definite test for out of date dumped files.  -slb */
  int restarted = 0;
#ifdef QUANTIFY
  quantify_stop_recording_data ();
  quantify_clear_data ();
#endif /* QUANTIFY */

  inhibit_non_essential_printing_operations = 1;
  suppress_early_error_handler_backtrace = 0;
  lim_data = 0; /* force reinitialization of this variable */

  /* Lisp_Object must fit in a word; check VALBITS and GCTYPEBITS */
  assert (sizeof (Lisp_Object) == sizeof (void *));

#ifdef LINUX_SBRK_BUG
  sbrk (1);
#endif

  if (!initialized)
    {
#ifdef DOUG_LEA_MALLOC
      mallopt (M_MMAP_MAX, 0);
#endif
      run_temacs_argc = 0;
      if (! SETJMP (run_temacs_catch))
	{
	  main_1 (vol_argc, vol_argv, vol_envp, 0);
	}
      /* run-emacs-from-temacs called */
      restarted = 1;
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
	extern char **_environ;
	if ((unsigned) environ == 0)
	  environ=_environ;
      }
#endif /* _SCO_DS */
      vol_envp = environ;
    }
#if defined (RUN_TIME_REMAP) && ! defined (PDUMP)
  else
    /* obviously no-one uses this because where it was before initialized was
     *always* true */
    run_time_remap (argv[0]);
#endif

#ifdef DOUG_LEA_MALLOC
  if (initialized && (malloc_state_ptr != NULL))
    {
      int rc = malloc_set_state (malloc_state_ptr);
      if (rc != 0)
	{
	  stderr_out ("malloc_set_state failed, rc = %d\n", rc);
	  abort ();
	}
#if 0
      free (malloc_state_ptr);
#endif
      /* mmap works in glibc-2.1, glibc-2.0 (Non-Mule only) and Linux libc5 */
#if (defined(__GLIBC__) && __GLIBC_MINOR__ >= 1) || \
    defined(_NO_MALLOC_WARNING_) || \
    (defined(__GLIBC__) && __GLIBC_MINOR__ < 1 && !defined(MULE)) || \
    defined(DEBUG_DOUG_LEA_MALLOC)
      mallopt (M_MMAP_MAX, 64);
#endif
#ifdef REL_ALLOC
      r_alloc_reinit ();
#endif
    }
#endif /* DOUG_LEA_MALLOC */

  run_temacs_argc = -1;

  main_1 (vol_argc, vol_argv, vol_envp, restarted);

#ifdef _MSC_VER
  }
  /* VC++ documentation says that
     GetExceptionCode() cannot be called inside the filter itself. */
  __except (mswindows_handle_hardware_exceptions (GetExceptionCode ())) {}
#endif

  RETURN_NOT_REACHED (0)
}


/************************************************************************/
/*                 dumping XEmacs (to a new EXE file)                   */
/************************************************************************/

#ifndef CANNOT_DUMP

#if !defined(PDUMP) || !defined(SYSTEM_MALLOC)
extern char my_edata[];
#endif

extern void disable_free_hook (void);

DEFUN ("dump-emacs", Fdump_emacs, 2, 2, 0, /*
Dump current state of XEmacs into executable file FILENAME.
Take symbols from SYMFILE (presumably the file you executed to run XEmacs).
This is used in the file `loadup.el' when building XEmacs.

Remember to set `command-line-processed' to nil before dumping
if you want the dumped XEmacs to process its command line
and announce itself normally when it is run.
*/
       (filename, symfile))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;
  int opurify;

  GCPRO2 (filename, symfile);

#ifdef FREE_CHECKING
  Freally_free (Qnil);

  /* When we're dumping, we can't use the debugging free() */
  disable_free_hook ();
#endif

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  if (!NILP (symfile))
    {
      CHECK_STRING (symfile);
      if (XSTRING_LENGTH (symfile) > 0)
	symfile = Fexpand_file_name (symfile, Qnil);
      else
	symfile = Qnil;
    }

  opurify = purify_flag;
  purify_flag = 0;

#if defined (HEAP_IN_DATA) && !defined(PDUMP)
  report_sheap_usage (1);
#endif

  clear_message ();

  fflush (stderr);
  fflush (stdout);

  disksave_object_finalization ();
  release_breathing_space ();

  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (my_edata, malloc_warning);
#endif

  garbage_collect_1 ();

#ifdef PDUMP
  pdump ();
#elif defined (WIN32_NATIVE)
  unexec (XSTRING_DATA (filename),
	  STRINGP (symfile) ? XSTRING_DATA (symfile) : 0,
	  (uintptr_t) my_edata, 0, 0);
#else
  {
    Extbyte *filename_ext;
    Extbyte *symfile_ext;

    LISP_STRING_TO_EXTERNAL (filename, filename_ext, Qfile_name);

    if (STRINGP (symfile))
      LISP_STRING_TO_EXTERNAL (symfile, symfile_ext, Qfile_name);
    else
      symfile_ext = 0;

# ifdef DOUG_LEA_MALLOC
    malloc_state_ptr = malloc_get_state ();
# endif
  /* here we break our rule that the filename conversion should
     be performed at the actual time that the system call is made.
     It's a whole lot easier to do the conversion here than to
     modify all the unexec routines to ensure that filename
     conversion is applied everywhere.  Don't worry about memory
     leakage because this call only happens once. */
    unexec (filename_ext, symfile_ext, (uintptr_t) my_edata, 0, 0);
# ifdef DOUG_LEA_MALLOC
    free (malloc_state_ptr);
# endif
  }
#endif /* not PDUMP, not WIN32_NATIVE */

  purify_flag = opurify;

  UNGCPRO;
  return Qnil;
}

#endif /* not CANNOT_DUMP */


/************************************************************************/
/*                  exiting XEmacs (intended or not)                    */
/************************************************************************/

/* Ben's capsule summary about expected and unexpected exits from XEmacs.

   Expected exits occur when the user directs XEmacs to exit, for example
   by pressing the close button on the only frame in XEmacs, or by typing
   C-x C-c.  This runs `save-buffers-kill-emacs', which saves any necessary
   buffers, and then exits using the primitive `kill-emacs'.

   However, unexpected exits occur in a few different ways:

     -- a memory access violation or other hardware-generated exception
        occurs.  This is the worst possible problem to deal with, because
        the fault can occur while XEmacs is in any state whatsoever, even
        quite unstable ones.  As a result, we need to be *extremely* careful
        what we do.
     -- we are using one X display (or if we've used more, we've closed the
        others already), and some hardware or other problem happens and
        suddenly we've lost our connection to the display.  In this situation,
	things are not so dire as in the last one; our code itself isn't
	trashed, so we can continue execution as normal, after having set
	things up so that we can exit at the appropriate time.  Our exit
	still needs to be of the emergency nature; we have no displays, so
	any attempts to use them will fail.  We simply want to auto-save
	(the single most important thing to do during shut-down), do minimal
	cleanup of stuff that has an independent existence outside of XEmacs,
	and exit.

	Currently, both unexpected exit scenarios described above set
	preparing_for_armageddon to indicate that nonessential and possibly
	dangerous things should not be done, specifically:

	-- no garbage collection.
	-- no hooks are run.
	-- no messages of any sort from autosaving.
	-- autosaving tries harder, ignoring certain failures.
	-- existing frames are not deleted.

	(Also, all places that set preparing_for_armageddon also
	set dont_check_for_quit.  This happens separately because it's
	also necessary to set other variables to make absolutely sure
	no quitting happens.)

	In the first scenario above (the access violation), we also set
	fatal_error_in_progress.  This causes more things to not happen:

	-- assertion failures do not abort.
	-- printing code does not do code conversion or gettext when
	   printing to stdout/stderr.
*/

/* ------------------------------- */
/*  low-level debugging functions  */
/* ------------------------------- */

#if defined (WIN32_NATIVE) && defined (DEBUG_XEMACS)
#define debugging_breakpoint() DebugBreak ()
#else
#define debugging_breakpoint()
#endif

void
debug_break (void)
{
  debugging_breakpoint ();
}

#if defined (WIN32_NATIVE) || defined (CYGWIN)

/* Return whether all bytes in the specified memory block can be read. */
int
debug_can_access_memory (void *ptr, Bytecount len)
{
  return !IsBadReadPtr (ptr, len);
}

#else /* !(defined (WIN32_NATIVE) || defined (CYGWIN)) */

/* #### There must be a better way!!!! */

static JMP_BUF memory_error_jump;

static SIGTYPE
debug_memory_error (int signum)
{
  EMACS_REESTABLISH_SIGNAL (signum, debug_memory_error);
  EMACS_UNBLOCK_SIGNAL (signum);
  LONGJMP (memory_error_jump, 1);
}

/* Return whether all bytes in the specified memory block can be read. */
int
debug_can_access_memory (void *ptr, Bytecount len)
{
  /* Use volatile to protect variables from being clobbered by longjmp. */
  SIGTYPE (*volatile old_sigbus) (int);
  SIGTYPE (*volatile old_sigsegv) (int);
  volatile int old_errno = errno;
  volatile int retval = 1;

  if (!SETJMP (memory_error_jump))
    {
      old_sigbus =
	(SIGTYPE (*) (int)) EMACS_SIGNAL (SIGBUS, debug_memory_error);
      old_sigsegv =
	(SIGTYPE (*) (int)) EMACS_SIGNAL (SIGSEGV, debug_memory_error);

      if (len > 1)
	/* If we can, try to avoid problems with super-optimizing compilers
	   that might decide that memcmp (ptr, ptr, len) can be optimized
	   away since its result is always 1. */
	memcmp (ptr, (char *) ptr + 1, len - 1);
      else
	memcmp (ptr, ptr, len);
    }
  else
    retval = 0;
  EMACS_SIGNAL (SIGBUS, old_sigbus);
  EMACS_SIGNAL (SIGSEGV, old_sigsegv);
  errno = old_errno;

  return retval;
}

#endif /* defined (WIN32_NATIVE) || defined (CYGWIN) */

#ifdef DEBUG_XEMACS

DEFUN ("force-debugging-signal", Fforce_debugging_signal, 0, 1, 0, /*
Cause XEmacs to enter the debugger.
On some systems, there may be no way to do this gracefully; if so,
nothing happens unless ABORT is non-nil, in which case XEmacs will
abort() -- a sure-fire way to immediately get back to the debugger,
but also a sure-fire way to kill XEmacs (and dump core on Unix
systems)!
*/
       (abort_))
{
  debugging_breakpoint ();
  if (!NILP (abort_))
    abort ();
  return Qnil;
}

#endif /* DEBUG_XEMACS */

/* ------------------------------- */
/*       some helper functions     */
/* ------------------------------- */

static void
ensure_no_quitting_from_now_on (void)
{
  /* make sure no quitting from now on!! */
  dont_check_for_quit = 1;
  Vinhibit_quit = Qt;
  Vquit_flag = Qnil;
}

#ifdef HAVE_MS_WINDOWS
static void
pause_so_user_can_read_messages (int allow_further)
{
  static int already_paused;

  if (already_paused || !noninteractive)
    return;
  if (!allow_further)
    already_paused = 1;
  /* If we displayed a message on the console, then we must allow the
     user to see this message.  This may be unnecessary, but can't hurt,
     and we can't necessarily check arg; e.g. xemacs --help kills with
     argument 0. */
  if (mswindows_message_outputted)
    Fmswindows_message_box
      (build_msg_string ("Messages outputted.  XEmacs is exiting."),
       Qnil, Qnil);
}
#endif

#ifdef WIN32_NATIVE

static DWORD CALLBACK
wait_for_termination_signal (LPVOID handle)
{
  HANDLE hevent = (HANDLE) handle;
  WaitForSingleObject (hevent, INFINITE);
  ExitProcess (0);
  return 0; /* not reached */
}

#endif
/* -------------------------------- */
/* a (more-or-less) normal shutdown */
/* -------------------------------- */

/* Perform an orderly shutdown of XEmacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when XEmacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers and Fkill_emacs.  It used to
   be called by X protocol error handlers, but instead they now call
   Fkill_emacs. */

static void
shut_down_emacs (int sig, Lisp_Object stuff, int no_auto_save)
{
  /* This function can GC */
  /* Prevent running of hooks and other non-essential stuff
     from now on.  */
  preparing_for_armageddon = 1;

  ensure_no_quitting_from_now_on ();

#ifdef QUANTIFY
  quantify_stop_recording_data ();
#endif /* QUANTIFY */

  /* This is absolutely the most important thing to do, so make sure
     we do it now, before anything else.  We might have crashed and
     be in a weird inconsistent state, and potentially anything could
     set off another protection fault and cause us to bail out
     immediately. */
  /* Steve writes the following:

     [[I'm not removing the code entirely, yet.  We have run up against
     a spate of problems in diagnosing crashes due to crashes within
     crashes.  It has very definitely been determined that code called
     during auto-saving cannot work if XEmacs crashed inside of GC.
     We already auto-save on an itimer so there cannot be too much
     unsaved stuff around, and if we get better crash reports we might
     be able to get more problems fixed so I'm disabling this.  -slb]]

     and DISABLES AUTO-SAVING ENTIRELY during crashes!  Way way bad idea.

     Instead let's just be more intelligent about avoiding crashing
     when possible, esp. nested crashes.
  */
  if (!no_auto_save)
    Fdo_auto_save (Qt, Qnil); /* do this before anything hazardous */

  fflush (stdout);
  reset_all_consoles ();
  if (sig && sig != SIGTERM)
    {
      if (sig == -1)
	stderr_out ("\nFatal error.\n\n");
      else
	stderr_out ("\nFatal error (%d).\n\n", sig);
      stderr_out
	("Your files have been auto-saved.\n"
	 "Use `M-x recover-session' to recover them.\n"
	 "\n"
         "If you have access to the PROBLEMS file that came with your\n"
         "version of XEmacs, please check to see if your crash is described\n"
         "there, as there may be a workaround available.\n"
#ifdef INFODOCK
	 "Otherwise, please report this bug by selecting `Report-Bug'\n"
         "in the InfoDock menu.\n"
#else
	 "Otherwise, please report this bug by running the send-pr\n"
         "script included with XEmacs, or selecting `Send Bug Report'\n"
         "from the help menu.\n"
	 "As a last resort send ordinary email to `crashes@xemacs.org'.\n"
#endif
	 "*MAKE SURE* to include the information in the command\n"
	 "M-x describe-installation.\n"
#ifndef _MSC_VER
	 "\n"
	 "If at all possible, *please* try to obtain a C stack backtrace;\n"
	 "it will help us immensely in determining what went wrong.\n"
	 "To do this, locate the core file that was produced as a result\n"
	 "of this crash (it's usually called `core' and is located in the\n"
	 "directory in which you started the editor, or maybe in your home\n"
	 "directory), and type\n"
	 "\n"
	 "  gdb "
#endif
	 );
#ifndef _MSC_VER
      {
	const char *name;
	char *dir = 0;

	/* Now try to determine the actual path to the executable,
	   to try to make the backtrace-determination process as foolproof
	   as possible. */
	if (STRINGP (Vinvocation_name))
	  name = (char *) XSTRING_DATA (Vinvocation_name);
	else
	  name = "xemacs";
	if (STRINGP (Vinvocation_directory))
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
#endif /* _MSC_VER */
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
}

/* Dumping apparently isn't supported by versions of GCC >= 2.8. */
/* The following needs conditionalization on whether either XEmacs or */
/* various system shared libraries have been built and linked with */
/* GCC >= 2.8.  -slb */
#if defined(GNU_MALLOC)
static void
voodoo_free_hook (void *mem)
{
  /* Disable all calls to free() when XEmacs is exiting and it doesn't */
  /* matter. */
  __free_hook =
#ifdef __GNUC__ /* prototype of __free_hook varies with glibc version */
    (__typeof__ (__free_hook))
#endif
    voodoo_free_hook;
}
#endif /* GNU_MALLOC */

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

  ensure_no_quitting_from_now_on ();

  if (!preparing_for_armageddon)
    {
      Lisp_Object concons, nextcons;

      /* Normally, go ahead and delete all the consoles now.
	 Some unmentionably lame window systems (MS Wwwww...... eek,
	 I can't even say it) don't properly clean up after themselves,
	 and even for those that do, it might be cleaner this way.
	 If we're going down, however, we don't do this (might
	 be too dangerous), and if we get a crash somewhere within
	 this loop, we'll still autosave and won't try this again. */

      LIST_LOOP_DELETING (concons, nextcons, Vconsole_list)
	{
	  /* There is very little point in deleting the stream console.
	     It uses stdio, which should flush any buffered output and
	     something can only go wrong. -slb */
	  /* I changed my mind.  There's a stupid hack in close to add
	     a trailing newline. */
	  /*if (!CONSOLE_STREAM_P (XCONSOLE (XCAR (concons))))*/
	    delete_console_internal (XCONSOLE (XCAR (concons)), 1, 1, 0);
	}
    }

  UNGCPRO;

#ifdef HAVE_MS_WINDOWS
  pause_so_user_can_read_messages (1);
#endif

  shut_down_emacs (0, STRINGP (arg) ? arg : Qnil, 0);

#if defined(GNU_MALLOC)
  __free_hook =
#ifdef __GNUC__ /* prototype of __free_hook varies with glibc version */
    (__typeof__ (__free_hook))
#endif
    voodoo_free_hook;
#endif

  exit (INTP (arg) ? XINT (arg) : 0);
  /* NOTREACHED */
  return Qnil; /* I'm sick of the compiler warning */
}

/* -------------------------------- */
/*   abnormal shutdowns: GP faults  */
/* -------------------------------- */

/* This is somewhat ad-hoc ...  figure out whether the user is developing
   XEmacs, which means (under MS Windows) they have a system debugger
   installed that catches GP faults in any application and lets them open
   up MS Dev Studio and start debugging the application -- similar to
   producing a core dump and then going back with a debugger to investigate
   the core dump, except that the program is still running.  When this is
   installed, it's better not to "pause so user gets messages" because the
   debugger will pause anyway; and in case we're currently with a menu
   popped up or somewhere else inside of an internal modal loop, we will
   get wedged when we output the "pause". (It seems that the two modal
   loops will fight each other and the return key will never be passed to
   the "pause" handler so that XEmacs's GPF handler can return, resignal
   the GPF, and properly go into the debugger.) */
#if defined (ERROR_CHECK_TYPES) || defined (ERROR_CHECK_TEXT) || defined (ERROR_CHECK_GC) || defined (ERROR_CHECK_STRUCTURES)
#define USER_IS_DEVELOPING_XEMACS
#endif


/* Handle bus errors, illegal instruction, etc: actual implementation. */
static void
guts_of_fatal_error_signal (int sig)
{
  fatal_error_in_progress++;
  inhibit_non_essential_printing_operations = 1;
  preparing_for_armageddon = 1;

  ensure_no_quitting_from_now_on ();

  /* Only try auto-saving first time through.  If we crash in auto-saving,
     don't do it again. */
  if (fatal_error_in_progress == 1)
    {
      Fdo_auto_save (Qt, Qnil); /* do this before anything hazardous */
      /* Do this so that the variable has the same value of 2 regardless of
	 whether we made it through auto-saving correctly. */
      fatal_error_in_progress++;
    }
  else if (fatal_error_in_progress == 2)
    stderr_out ("WARNING: Unable to auto-save your files properly.\n"
		"Some or all may in fact have been auto-saved.\n"
		"\n");

  /* Now, reset our signal handler, so the next time, we just die.
     Don't do this before auto-saving. */
  if (sig >= 0)
    EMACS_SIGNAL (sig, SIG_DFL);

  /* Keep in mind that there's more than one signal that we can crash
     on. */
  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (fatal_error_in_progress <= 2)
    {
      shut_down_emacs (sig, Qnil, 1);
      stderr_out ("\nLisp backtrace follows:\n\n");
      debug_backtrace ();
# if 0	/* This is evil, rarely useful, and causes grief in some cases. */
      /* Check for Sun-style stack printing via /proc */
      {
        const Char_ASCII *pstack = "/usr/proc/bin/pstack";
        if (access (pstack, X_OK) == 0)
          {
            Char_ASCII buf[100];
            stderr_out ("\nC backtrace follows:\n"
                       "(A real debugger may provide better information)\n\n");
            sprintf (buf, "%s %d >&2", pstack, (int)getpid());
            system (buf);
          }
      }
# endif
#if defined (HAVE_MS_WINDOWS) && !defined (USER_IS_DEVELOPING_XEMACS)
      pause_so_user_can_read_messages (0);
#endif
    }
}

/* This is called when a fatal signal (SIGBUS aka "bus error", SIGSEGV aka
   "segmentation violation", SIGILL aka "illegal instruction", and many
   others) is sent to the program.  This generally happens under Unix,
   not MS Windows. */
SIGTYPE
fatal_error_signal (int sig)
{
  /* Unblock the signal so that if the same signal gets sent in the
     code below, we avoid a deadlock. */
  EMACS_UNBLOCK_SIGNAL (sig);

  guts_of_fatal_error_signal (sig);

  /* Signal the same code; this time it will really be fatal. */
#ifdef WIN32_NATIVE
  raise (sig);
#else
  kill (qxe_getpid (), sig);
#endif
  SIGRETURN;
}

#ifdef _MSC_VER

#define STATUS_ASSERTION_FAILURE 0xE0000001

static DWORD
mswindows_handle_hardware_exceptions_1 (void)
{
  inhibit_non_essential_printing_operations = 1;
  preparing_for_armageddon = 1;
#if !defined (USER_IS_DEVELOPING_XEMACS)
  pause_so_user_can_read_messages (0);
#endif
  return EXCEPTION_EXECUTE_HANDLER;
}

/* This is called under MS Windows when an exception (this encompasses both
   user-defined exceptions and hardware exceptions such as GP faults aka
   SIGBUS or SIGSEGV) is triggered. */

static DWORD
mswindows_handle_hardware_exceptions (DWORD code)
{
  if (code != STATUS_ACCESS_VIOLATION && code != STATUS_ILLEGAL_INSTRUCTION
      && code != STATUS_PRIVILEGED_INSTRUCTION
      && code != STATUS_DATATYPE_MISALIGNMENT
      && code != STATUS_ASSERTION_FAILURE)
    return EXCEPTION_CONTINUE_SEARCH;

  /* I don't know if this filter is still wrapped in the outer __try, but
     it doesn't hurt to have another one, and it lets us control more
     exactly what we really want to do in such a situation.  What we do is
     pause, if we haven't already done so, so that the user can see what's
     output.  This is critical because otherwise the output is gone. */
  __try
    {
      guts_of_fatal_error_signal (-1);
    }
  /* VC++ documentation says that
     GetExceptionCode() cannot be called inside the filter itself. */

  /*  __except (mswindows_handle_hardware_exceptions (GetExceptionCode ())) {}

     The line above is original.  Unfortunately, when an error is tripped
     inside of the handler (e.g. during Fbacktrace()), and the handler for
     the handler is invoked, it correctly notices that something is amiss
     and it should just return -- but it returns EXCEPTION_CONTINUE_SEARCH,
     which causes the debugger to be invoked debugging the handler code in
     this function -- and WITH THE STACK UNWOUND so that you see main()
     calling mswindows_handle_hardware_exceptions(), calling Fbacktrace(),
     and a crash a couple of frames in -- AND NO SIGN OF THE ORIGINAL CRASH!

     There's some real weirdness going on in the stack handling -- unlike
     in Unix, where further crashes just keep adding to the stack, it seems
     that under the structured-exception-handling, the stack can actually
     bounce back and forth between the full stack at the location of the
     exception and the unwound stack at the place where the __try clause was
     established.  I don't completely understand it.  What I do know is that
     returning EXCEPTION_EXECUTE_HANDLER on nested crash has the effect of
     aborting execution of the handler and going back to the outer filter
     function, which returns EXCEPTION_CONTINUE_SEARCH and everything is
     hunky-dorey -- your debugger sees a crash at the right location with
     the right stack.

     I'm leaving in the trickier Unix-like code in the handler; someone who
     understands better than me how the stack works in these handlers could
     fix it up more.  As it is, it works pretty well, so I'm not likely to
     touch it more. --ben
  */

  __except (mswindows_handle_hardware_exceptions_1 ()) {}

  /* pretend we didn't handle this, so that the debugger is invoked and/or
     the normal GPF box appears. */
  return EXCEPTION_CONTINUE_SEARCH;
}

#endif /* _MSC_VER */

/* -------------------------------------- */
/* abnormal shutdowns: assertion failures */
/* -------------------------------------- */

/* This flag is useful to define if you're under a debugger; this way, you
   can put a breakpoint of assert_failed() and debug multiple problems
   in one session without having to recompile. */
/* #define ASSERTIONS_DONT_ABORT */

#ifdef USE_ASSERTIONS
/* This highly dubious kludge ... shut up Jamie, I'm tired of your slagging. */

/* Nonzero if handling an assertion failure. (Bumped by one each time
   we recursively hit such a failure.) */
static int in_assert_failed;

static const char *assert_failed_file;
static int assert_failed_line;
static const char *assert_failed_expr;

#ifdef fprintf
#undef fprintf
#endif

/* This is called when an assert() fails or when abort() is called -- both
   of those are defined in the preprocessor to an expansion involving
   assert_failed(). */
void
assert_failed (const char *file, int line, const char *expr)
{
  /* If we're already crashing, let's not crash again.  This might be
     critical to getting auto-saving working properly. */
  if (fatal_error_in_progress)
    return;

  /* We are extremely paranoid so we sensibly deal with recursive
     assertion failures. */
  in_assert_failed++;
  inhibit_non_essential_printing_operations = 1;

  if (in_assert_failed >= 4)
    _exit (-1);
  else if (in_assert_failed == 3)
    {
      debugging_breakpoint ();
      _exit (-1);
    }
  else if (in_assert_failed == 2)
    {
      /* Ultra-paranoia.  stderr_out() tries very hard not to do
	 anything during assertion failures that might trigger more
	 failures; but we might have messed up somewhere.  fprintf was
	 undeffed above, in case it was encapsulated. */
      fprintf (stderr,
	       "Fatal error: recursive assertion failure, "
	       "file %s, line %d, %s\n",
	       file, line, expr);
      fprintf (stderr,
	       "Original assertion failure: file %s, line %d, %s\n",
	       assert_failed_file, assert_failed_line, assert_failed_expr);
    }
  else
    {
      assert_failed_file = file;
      assert_failed_line = line;
      assert_failed_expr = expr;

      stderr_out ("\nFatal error: assertion failed, file %s, line %d, %s\n",
		  file, line, expr);
    }

  /* Enable the following if you want a breakpoint right away to the
     debugger, without the whole shutdown processing first.  This can be
     useful if you're afraid the shutdown processing will modify state that
     you're trying to debug (generally fairly unlikely); but you then don't
     get the auto-save behavior, which may be extremely important if you
     were in the middle of doing something */
  /* debugging_breakpoint (); */
#if !defined (ASSERTIONS_DONT_ABORT)
#ifdef _MSC_VER
  /* Calling abort() directly just seems to exit, in a way we can't
     trap. (#### The docs say it does raise(SIGABRT), which we should be
     able to trap.  Perhaps we're messing up somewhere?  Or perhaps MS is
     messed up.)

     So, instead we cause an exception and enter into the structured
     exception-handling mechanism, which is just like what happens when a
     GPF occurs, and is cleaner anyway. (If we entered into one of the
     signal handlers, a crash in there would enter anyway into the
     structured exception stuff, and you'd get some weird mixture.  Cleaner
     to keep it all in the expected way.)
     */
  /* Either of the following work in terms of causing an exception.  The
     second one looks cleaner but you get an odd message about "Unknown
     software exception ..." without the obvious "OK to terminate", "Cancel
     to debug"; instead, you just get OK/Cancel, which in fact do those
     same things. */
  * ((int *) 0) = 666;
  /* RaiseException (STATUS_ASSERTION_FAILURE, EXCEPTION_NONCONTINUABLE, 0,
	             0); */
#else
  really_abort ();
#endif /* _MSC_VER */
#endif /* !defined (ASSERTIONS_DONT_ABORT) */
  inhibit_non_essential_printing_operations = 0;
  in_assert_failed = 0;
}
#endif /* USE_ASSERTIONS */

/* -------------------------------------- */
/*        low-memory notification         */
/* -------------------------------------- */

#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
SIGTYPE
memory_warning_signal (int sig)
{
  /* #### bad bad bad; this function shouldn't do anything except
     set a flag, or weird corruption could happen. */
  EMACS_SIGNAL (sig, memory_warning_signal);

  malloc_warning
    (GETTEXT ("Operating system warns that virtual memory is running low.\n"));

  /* It might be unsafe to call do_auto_save now.  */
  force_auto_save_soon ();
}
#endif /* SIGDANGER */


/************************************************************************/
/*                              Miscellaneous                           */
/************************************************************************/

DEFUN ("noninteractive", Fnoninteractive, 0, 0, 0, /*
Non-nil return value means XEmacs is running without interactive terminal.
*/
       ())
{
  return noninteractive ? Qt : Qnil;
}

#ifdef QUANTIFY
DEFUN ("quantify-start-recording-data", Fquantify_start_recording_data,
       0, 0, "", /*
Start recording Quantify data.
*/
       ())
{
  quantify_start_recording_data ();
  return Qnil;
}

DEFUN ("quantify-stop-recording-data", Fquantify_stop_recording_data,
       0, 0, "", /*
Stop recording Quantify data.
*/
       ())
{
  quantify_stop_recording_data ();
  return Qnil;
}

DEFUN ("quantify-clear-data", Fquantify_clear_data, 0, 0, "", /*
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
  DEFSUBR (Fdump_emacs);
#endif /* !CANNOT_DUMP */

  DEFSUBR (Frun_emacs_from_temacs);
  DEFSUBR (Frunning_temacs_p);
  DEFSUBR (Finvocation_name);
  DEFSUBR (Finvocation_directory);
  DEFSUBR (Fkill_emacs);
  DEFSUBR (Fnoninteractive);

#ifdef DEBUG_XEMACS
  DEFSUBR (Fforce_debugging_signal);
#endif

#ifdef QUANTIFY
  DEFSUBR (Fquantify_start_recording_data);
  DEFSUBR (Fquantify_stop_recording_data);
  DEFSUBR (Fquantify_clear_data);
#endif /* QUANTIFY */

  DEFSYMBOL (Qkill_emacs_hook);
  DEFSYMBOL (Qsave_buffers_kill_emacs);
}

/* Yuck!  These variables may get set from command-line options when
   dumping; if we don't clear them, they will still be on once the dumped
   XEmacs reloads. (not an issue with pdump, as we kludge around this in
   main_1().) */

void
zero_out_command_line_status_vars (void)
{
  vanilla_inhibiting = 0;
  inhibit_early_packages = 0;
  inhibit_all_packages = 0;
  inhibit_autoloads = 0;
  debug_paths = 0;
#ifndef INHIBIT_SITE_LISP
  inhibit_site_lisp = 0;
#else
  inhibit_site_lisp = 1;
#endif
#ifndef INHIBIT_SITE_MODULES
  inhibit_site_modules = 0;
#else
  inhibit_site_modules = 1;
#endif
}

void
vars_of_emacs (void)
{
  DEFVAR_BOOL ("suppress-early-error-handler-backtrace",
	       &suppress_early_error_handler_backtrace /*
Non-nil means early error handler shouldn't print a backtrace.
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

  DEFVAR_LISP ("invocation-path", &Vinvocation_path /*
The path in which the XEmacs executable was found, to run it.
The value is simply the value of environment variable PATH on startup
if XEmacs was found there.
*/ );

#if 0 /* FSFmacs */
  xxDEFVAR_LISP ("installation-directory", &Vinstallation_directory /*
A directory within which to look for the `lib-src' and `etc' directories.
This is non-nil when we can't find those directories in their standard
installed locations, but we can find them ear where the XEmacs executable
was found.
*/ );
#endif

  DEFVAR_LISP ("system-type", &Vsystem_type /*
Symbol indicating type of operating system you are using.
*/ );
  Vsystem_type = intern (SYSTEM_TYPE);
  Fprovide (Vsystem_type);

#ifndef EMACS_CONFIGURATION
# define EMACS_CONFIGURATION "UNKNOWN"
#endif
  DEFVAR_LISP ("system-configuration", &Vsystem_configuration /*
String naming the configuration XEmacs was built for.
*/ );
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

#ifndef EMACS_CONFIG_OPTIONS
# define EMACS_CONFIG_OPTIONS "UNKNOWN"
#endif
  DEFVAR_LISP ("system-configuration-options", &Vsystem_configuration_options /*
String containing the configuration options XEmacs was built with.
*/ );
  Vsystem_configuration_options = build_string (EMACS_CONFIG_OPTIONS);

  DEFVAR_LISP ("emacs-major-version", &Vemacs_major_version /*
Major version number of this version of Emacs, as an integer.
Warning: this variable did not exist in Emacs versions earlier than:
  FSF Emacs:   19.23
  XEmacs:      19.10
*/ );
  Vemacs_major_version = make_int (EMACS_MAJOR_VERSION);

  DEFVAR_LISP ("emacs-minor-version", &Vemacs_minor_version /*
Minor version number of this version of Emacs, as an integer.
Warning: this variable did not exist in Emacs versions earlier than:
  FSF Emacs:   19.23
  XEmacs:      19.10
*/ );
  Vemacs_minor_version = make_int (EMACS_MINOR_VERSION);

  DEFVAR_LISP ("emacs-patch-level", &Vemacs_patch_level /*
The patch level of this version of Emacs, as an integer.
The value is non-nil if this version of XEmacs is part of a series of
stable XEmacsen, but has bug fixes applied.
Warning: this variable does not exist in FSF Emacs or in XEmacs versions
earlier than 21.1.1
*/ );
#ifdef EMACS_PATCH_LEVEL
  Vemacs_patch_level = make_int (EMACS_PATCH_LEVEL);
#else
  Vemacs_patch_level = Qnil;
#endif

    DEFVAR_LISP ("emacs-beta-version", &Vemacs_beta_version /*
Beta number of this version of Emacs, as an integer.
The value is nil if this is an officially released version of XEmacs.
Warning: this variable does not exist in FSF Emacs or in XEmacs versions
earlier than 20.3.
*/ );
#ifdef EMACS_BETA_VERSION
  Vemacs_beta_version = make_int (EMACS_BETA_VERSION);
#else
  Vemacs_beta_version = Qnil;
#endif

#ifdef INFODOCK
  DEFVAR_LISP ("infodock-major-version", &Vinfodock_major_version /*
Major version number of this InfoDock release.
*/ );
  Vinfodock_major_version = make_int (INFODOCK_MAJOR_VERSION);

  DEFVAR_LISP ("infodock-minor-version", &Vinfodock_minor_version /*
Minor version number of this InfoDock release.
*/ );
  Vinfodock_minor_version = make_int (INFODOCK_MINOR_VERSION);

  DEFVAR_LISP ("infodock-build-version", &Vinfodock_build_version /*
Build version of this InfoDock release.
*/ );
  Vinfodock_build_version = make_int (INFODOCK_BUILD_VERSION);
#endif

  DEFVAR_LISP ("xemacs-codename", &Vxemacs_codename /*
Codename of this version of Emacs (a string).
*/ );
#ifndef XEMACS_CODENAME
#define XEMACS_CODENAME "Noname"
#endif
  Vxemacs_codename = build_string (XEMACS_CODENAME);

  /* Lisp variables which contain command line flags.

     The portable dumper stomps on these; they must be saved and restored
     if they are processed before the call to pdump_load() in main_1().
  */
  DEFVAR_BOOL ("noninteractive", &noninteractive1 /*
Non-nil means XEmacs is running without interactive terminal.
*/ );

  DEFVAR_BOOL ("vanilla-inhibiting", &vanilla_inhibiting /*
Set to non-nil when the user-init and site-start files should not be loaded.
*/ );

  DEFVAR_BOOL ("inhibit-early-packages", &inhibit_early_packages /*
Set to non-nil when the early packages should not be respected at startup.
*/ );

  DEFVAR_BOOL ("inhibit-all-packages", &inhibit_all_packages /*
Set to non-nil when the no packages should not be respected at startup.
XEmacs will utterly ignore the packages -- not in load-path, not set up as
autoloads, nothing.
*/ );

  DEFVAR_BOOL ("inhibit-autoloads", &inhibit_autoloads /*
Set to non-nil when autoloads should not be loaded at startup.
*/ );

  DEFVAR_BOOL ("debug-paths", &debug_paths /*
Set to non-nil when debug information about paths should be printed.
*/ );

  DEFVAR_BOOL ("inhibit-site-lisp", &inhibit_site_lisp /*
Set to non-nil when the site-lisp should not be searched at startup.
*/ );
#ifdef INHIBIT_SITE_LISP
  inhibit_site_lisp = 1;
#endif

  DEFVAR_BOOL ("inhibit-site-modules", &inhibit_site_modules /*
Set to non-nil when site-modules should not be searched at startup.
*/ );
#ifdef INHIBIT_SITE_MODULES
  inhibit_site_modules = 1;
#endif

  DEFVAR_INT ("emacs-priority", &emacs_priority /*
Priority for XEmacs to run at.
This value is effective only if set before XEmacs is dumped,
and only if the XEmacs executable is installed with setuid to permit
it to change priority.  (XEmacs sets its uid back to the real uid.)
Currently, you need to define SET_EMACS_PRIORITY in `config.h'
before you compile XEmacs, to enable the code for this feature.
*/ );
  emacs_priority = 0;

  DEFVAR_CONST_LISP ("internal-error-checking", &Vinternal_error_checking /*
Internal error checking built-in into this instance of XEmacs.
This is a list of symbols, initialized at build-time.  Legal symbols
are:

extents		- check extents prior to each extent change;
types		- check types strictly;
malloc		- check operation of malloc;
gc		- check garbage collection;
text		- check text and buffer positions;
display		- check redisplay structure consistency;
glyphs		- check glyph structure consistency;
byte-code	- check byte-code consistency;.
structures	- check other structure consistency.

quick-build     - user has requested the "quick-build" configure option.
*/ );
  Vinternal_error_checking = Qnil;
#ifdef ERROR_CHECK_EXTENTS
  Vinternal_error_checking = Fcons (intern ("extents"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_TYPES
  Vinternal_error_checking = Fcons (intern ("types"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_MALLOC
  Vinternal_error_checking = Fcons (intern ("malloc"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_GC
  Vinternal_error_checking = Fcons (intern ("gc"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_TEXT
  Vinternal_error_checking = Fcons (intern ("text"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_DISPLAY
  Vinternal_error_checking = Fcons (intern ("display"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_GLYPHS
  Vinternal_error_checking = Fcons (intern ("glyphs"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_BYTE_CODE
  Vinternal_error_checking = Fcons (intern ("byte-code"),
				    Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_STRUCTURES
  Vinternal_error_checking = Fcons (intern ("structures"),
				    Vinternal_error_checking);
#endif
#ifdef QUICK_BUILD
  Vinternal_error_checking = Fcons (intern ("quick-build"),
				    Vinternal_error_checking);
#endif

  DEFVAR_CONST_LISP ("mail-lock-methods", &Vmail_lock_methods /*
Mail spool locking methods supported by this instance of XEmacs.
This is a list of symbols.  Each of the symbols is one of the
following: dot, lockf, flock, locking, mmdf.
*/ );
  {
    Vmail_lock_methods = Qnil;
    Vmail_lock_methods = Fcons (intern ("dot"), Vmail_lock_methods);
#ifdef HAVE_LOCKF
    Vmail_lock_methods = Fcons (intern ("lockf"), Vmail_lock_methods);
#endif
#ifdef HAVE_FLOCK
    Vmail_lock_methods = Fcons (intern ("flock"), Vmail_lock_methods);
#endif
#ifdef HAVE_MMDF
    Vmail_lock_methods = Fcons (intern ("mmdf"), Vmail_lock_methods);
#endif
#ifdef HAVE_LOCKING
    Vmail_lock_methods = Fcons (intern ("locking"), Vmail_lock_methods);
#endif
  }

  DEFVAR_CONST_LISP ("configure-mail-lock-method", &Vconfigure_mail_lock_method /*
Mail spool locking method suggested by configure.  This is one
of the symbols in MAIL-LOCK-METHODS.
*/ );
  {
#if defined(MAIL_LOCK_FLOCK) && defined(HAVE_FLOCK)
    Vconfigure_mail_lock_method = intern ("flock");
#elif defined(MAIL_LOCK_LOCKF) && defined(HAVE_LOCKF)
    Vconfigure_mail_lock_method = intern ("lockf");
#elif defined(MAIL_LOCK_MMDF) && defined(HAVE_MMDF)
    Vconfigure_mail_lock_method = intern ("mmdf");
#elif defined(MAIL_LOCK_LOCKING) && defined(HAVE_LOCKING)
    Vconfigure_mail_lock_method = intern ("locking");
#else
    Vconfigure_mail_lock_method = intern ("dot");
#endif
  }
}

void
complex_vars_of_emacs (void)
{
  /* This is all related to path searching. */

  DEFVAR_LISP ("emacs-program-name", &Vemacs_program_name /*
*Name of the Emacs variant.
For example, this may be \"xemacs\" or \"infodock\".
This is mainly meant for use in path searching.
*/ );
  Vemacs_program_name = build_ext_string (PATH_PROGNAME, Qfile_name);

  DEFVAR_LISP ("emacs-program-version", &Vemacs_program_version /*
*Version of the Emacs variant.
This typically has the form NN.NN-bNN.
This is mainly meant for use in path searching.
*/ );
  Vemacs_program_version = build_ext_string (PATH_VERSION, Qfile_name);

  DEFVAR_LISP ("exec-path", &Vexec_path /*
*List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or nil (try default directory).
*/ );
  Vexec_path = Qnil;

  DEFVAR_LISP ("exec-directory", &Vexec_directory /*
*Directory of architecture-dependent files that come with XEmacs,
especially executable programs intended for XEmacs to invoke.
*/ );
  Vexec_directory = Qnil;

  DEFVAR_LISP ("configure-exec-directory", &Vconfigure_exec_directory /*
For internal use by the build procedure only.
configure's idea of what `exec-directory' will be.
*/ );
#ifdef PATH_EXEC
  Vconfigure_exec_directory = Ffile_name_as_directory
    (build_ext_string (PATH_EXEC, Qfile_name));
#else
  Vconfigure_exec_directory = Qnil;
#endif

  DEFVAR_LISP ("lisp-directory", &Vlisp_directory /*
*Directory of core Lisp files that come with XEmacs.
*/ );
  Vlisp_directory = Qnil;

  DEFVAR_LISP ("configure-lisp-directory", &Vconfigure_lisp_directory /*
For internal use by the build procedure only.
configure's idea of what `lisp-directory' will be.
*/ );
#ifdef PATH_LOADSEARCH
  Vconfigure_lisp_directory = Ffile_name_as_directory
    (build_ext_string (PATH_LOADSEARCH, Qfile_name));
#else
  Vconfigure_lisp_directory = Qnil;
#endif

  DEFVAR_LISP ("mule-lisp-directory", &Vmule_lisp_directory /*
*Directory of Mule Lisp files that come with XEmacs.
*/ );
  Vmule_lisp_directory = Qnil;

  DEFVAR_LISP ("configure-mule-lisp-directory", &Vconfigure_mule_lisp_directory /*
For internal use by the build procedure only.
configure's idea of what `mule-lisp-directory' will be.
*/ );
#ifdef PATH_MULELOADSEARCH
  Vconfigure_mule_lisp_directory = Ffile_name_as_directory
    (build_string ((char *) PATH_MULELOADSEARCH));
#else
  Vconfigure_mule_lisp_directory = Qnil;
#endif

  DEFVAR_LISP ("module-directory", &Vmodule_directory /*
*Directory of core dynamic modules that come with XEmacs.
*/ );
  Vmodule_directory = Qnil;

  DEFVAR_LISP ("configure-module-directory", &Vconfigure_module_directory /*
For internal use by the build procedure only.
configure's idea of what `module-directory' will be.
*/ );
#ifdef PATH_MODULESEARCH
  Vconfigure_module_directory = Ffile_name_as_directory
    (build_ext_string (PATH_MODULESEARCH, Qfile_name));
#else
  Vconfigure_module_directory = Qnil;
#endif

  DEFVAR_LISP ("configure-package-path", &Vconfigure_package_path /*
For internal use by the build procedure only.
configure's idea of what the package path will be.
*/ );
#ifdef PATH_PACKAGEPATH
  Vconfigure_package_path = split_external_path (PATH_PACKAGEPATH);
#else
  Vconfigure_package_path = Qnil;
#endif

  DEFVAR_LISP ("data-directory", &Vdata_directory /*
*Directory of architecture-independent files that come with XEmacs,
intended for XEmacs to use.
Use of this variable in new code is almost never correct.  See the
functions `locate-data-file' and `locate-data-directory' and the variable
`data-directory-list'.
*/ );
  Vdata_directory = Qnil;

  DEFVAR_LISP ("configure-data-directory", &Vconfigure_data_directory /*
For internal use by the build procedure only.
configure's idea of what `data-directory' will be.
*/ );
#ifdef PATH_DATA
  Vconfigure_data_directory = Ffile_name_as_directory
    (build_ext_string (PATH_DATA, Qfile_name));
#else
  Vconfigure_data_directory = Qnil;
#endif

  DEFVAR_LISP ("data-directory-list", &Vdata_directory_list /*
*List of directories of architecture-independent files that come with XEmacs
or were installed as packages, and are intended for XEmacs to use.
*/ );
  Vdata_directory_list = Qnil;

  DEFVAR_LISP ("site-directory", &Vsite_directory /*
*Directory of site-specific Lisp files that come with XEmacs.
*/ );
  Vsite_directory = Qnil;

  DEFVAR_LISP ("configure-site-directory", &Vconfigure_site_directory /*
For internal use by the build procedure only.
configure's idea of what `site-directory' will be.
*/ );
#ifdef PATH_SITE
  Vconfigure_site_directory = Ffile_name_as_directory
    (build_ext_string (PATH_SITE, Qfile_name));
#else
  Vconfigure_site_directory = Qnil;
#endif

  DEFVAR_LISP ("site-module-directory", &Vsite_module_directory /*
*Directory of site-specific loadable modules that come with XEmacs.
*/ );
  Vsite_module_directory = Qnil;

  DEFVAR_LISP ("configure-site-module-directory", &Vconfigure_site_module_directory /*
For internal use by the build procedure only.
configure's idea of what `site-directory' will be.
*/ );
#ifdef PATH_SITE_MODULES
  Vconfigure_site_module_directory = Ffile_name_as_directory
    (build_ext_string (PATH_SITE_MODULES, Qfile_name));
#else
  Vconfigure_site_module_directory = Qnil;
#endif

  DEFVAR_LISP ("doc-directory", &Vdoc_directory /*
*Directory containing the DOC file that comes with XEmacs.
This is usually the same as `exec-directory'.
*/ );
  Vdoc_directory = Qnil;

  DEFVAR_LISP ("configure-doc-directory", &Vconfigure_doc_directory /*
For internal use by the build procedure only.
configure's idea of what `doc-directory' will be.
*/ );
#ifdef PATH_DOC
  Vconfigure_doc_directory = Ffile_name_as_directory
    (build_ext_string (PATH_DOC, Qfile_name));
#else
  Vconfigure_doc_directory = Qnil;
#endif

  DEFVAR_LISP ("configure-exec-prefix-directory", &Vconfigure_exec_prefix_directory /*
For internal use by the build procedure only.
configure's idea of what `exec-prefix-directory' will be.
*/ );
#ifdef PATH_EXEC_PREFIX
  Vconfigure_exec_prefix_directory = Ffile_name_as_directory
    (build_ext_string (PATH_EXEC_PREFIX, Qfile_name));
#else
  Vconfigure_exec_prefix_directory = Qnil;
#endif

  DEFVAR_LISP ("configure-prefix-directory", &Vconfigure_prefix_directory /*
For internal use by the build procedure only.
configure's idea of what `prefix-directory' will be.
*/ );
#ifdef PATH_PREFIX
  Vconfigure_prefix_directory = Ffile_name_as_directory
    (build_ext_string (PATH_PREFIX, Qfile_name));
#else
  Vconfigure_prefix_directory = Qnil;
#endif

  DEFVAR_LISP ("configure-info-directory", &Vconfigure_info_directory /*
For internal use by the build procedure only.
This is the name of the directory in which the build procedure installed
Emacs's info files; the default value for Info-default-directory-list
includes this.
*/ );
#ifdef PATH_INFO
  Vconfigure_info_directory =
    Ffile_name_as_directory (build_ext_string (PATH_INFO, Qfile_name));
#else
  Vconfigure_info_directory = Qnil;
#endif

  DEFVAR_LISP ("configure-info-path", &Vconfigure_info_path /*
The configured initial path for info documentation.
*/ );
#ifdef PATH_INFOPATH
  Vconfigure_info_path = split_external_path (PATH_INFOPATH);
#else
  Vconfigure_info_path = Qnil;
#endif
}

#if defined(__sgi) && !defined(PDUMP)
/* This is so tremendously ugly I'd puke. But then, it works.
 * The target is to override the static constructor from the
 * libiflPNG.so library which is masquerading as libz, and
 * cores on us when re-started from the dumped executable.
 * This will have to go for 21.1  -- OG.
 */
void __sti__iflPNGFile_c___ (void);
void
__sti__iflPNGFile_c___ (void)
{
}

#endif

#undef abort /* Get access to the real version of abort.  We put this all
		the way at the end to make sure that all calls to abort()
		anywhere in the above code go through assert_failed(). */

void
really_abort (void)
{
  abort ();
}
