/* ellcc.c - front-end for compiling Emacs modules
Copyright (C) 1998, 1999 J. Kean Johnston.
Copyright (C) 2002 Jerry James.

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

Author: J. Kean Johnston (jkj@sco.com).
Please mail bugs and suggestions to the XEmacs maintainer.
*/

/*
Here's the scoop. We would really like this to be a shell script, but
the various Windows platforms don't have reliable scripting that suits
our needs. We don't want to rely on perl or some other such language
so we have to roll our own executable to act as a front-end for the
compiler.

This program is used to invoke the compiler, the linker and to generate
the module specific documentation and initialization code.  We assume we
are in 'compile' mode unless we encounter an argument which tells us
that we're not.  We take all arguments and pass them on directly to the
compiler, except for a few which are specific to this program:

  --mode=VALUE      This sets the program mode. VALUE can be one of
                    compile, link, init or verbose.
  --mod-name=NAME   Sets the module name to the string NAME.
  --mod-title=TITLE Sets the module title to the string TITLE.
  --mod-version=VER Sets the module version to the string VER.

The idea is that Makefiles will use ellcc as the compiler for making
dynamic Emacs modules, and life should be as simple as:

  make CC=ellcc LD='ellcc --mode=link'

The only additional requirement is an entry in the Makefile to produce
the module initialization file, which will usually be something along
the lines of:

  modinit.c: $(SRCS)
             ellcc --mode=init --mod-name=\"$(MODNAME)\" \
               --mod-title=\"$(MODTITLE)\" --mod-version=\"$(MODVERSION)\" \
               -o $@ $(SRCS)

See the samples for more details.
*/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

#define EMODULES_GATHER_VERSION
#define EXEC_GROW_SIZE 4

#include <emodules.h>
#include <ellcc.h> /* Generated files must be included using <...> */
#include "compiler.h"

#ifndef HAVE_SHLIB
int
main (int argc, char *argv[])
{
  fprintf (stderr, "Dynamic modules not supported on this platform\n");
  return EXIT_FAILURE;
}
#else

/*
 * Try to figure out the commands we need to use to create shared objects,
 * and how to compile for PIC mode.
 */

static char *progname;
static char *mod_name = NULL;
static char *mod_version = NULL;
static char *mod_title = NULL;
static char *mod_output = NULL;
static int exec_argc = 1;
static int exec_length = 0;
static int *exec_args;
static int real_argc = 0;
static char **prog_argv;

/*
 * We allow the user to override things in the environment
 */
static char *ellcc, *ellld, *ellcflags, *ellldflags, *ellpicflags,
  *elldllflags;

#define OVERENV(STR,EVAR,DFLT)			\
do {						\
  STR = getenv (EVAR);				\
  if ((STR) == NULL) {				\
    STR = DFLT;					\
  }						\
} while(0)

/*
 *	xnew, xrnew -- allocate, reallocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 *		Type *xrnew (OldPointer, int n, Type);
 */
#ifdef chkmalloc
# include "chkmalloc.h"
# define xnew(n,Type)	  ((Type *) trace_malloc (__FILE__, __LINE__, \
						  (n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) trace_realloc (__FILE__, __LINE__, \
						   (op), (n) * sizeof (Type)))
#else
# define xnew(n,Type)	  ((Type *) xmalloc ((n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) xrealloc ((op), (n) * sizeof (Type)))
#endif

/*
 * Ellcc modes of operation
 */

enum mode { ELLCC_COMPILE_MODE, ELLCC_LINK_MODE, ELLCC_INIT_MODE };

#ifdef DEBUG
static const char *ellcc_mode_name (enum mode ellcc_mode) ATTRIBUTE_CONST;

static const char *
ellcc_mode_name (enum mode ellcc_mode)
{
  switch (ellcc_mode)
    {
    case ELLCC_COMPILE_MODE:
      return "compile";
    case ELLCC_LINK_MODE:
      return "link";
    case ELLCC_INIT_MODE:
      return "init";
    }
  return "";
}
#endif

/*
 * Function Prototypes
 */

static void *xmalloc (size_t size) ATTRIBUTE_MALLOC;
static void *xrealloc (void *ptr, size_t size) ATTRIBUTE_MALLOC;
static char *xstrdup (char *) ATTRIBUTE_MALLOC;
static DECLARE_DOESNT_RETURN (fatal (char *, ...)) PRINTF_ARGS (1, 2);
static char ** add_string (char **, char *);
static char ** add_to_argv (char **, const char *);
static char ** do_compile_mode (void);
static char ** do_link_mode (void);
static char ** do_init_mode (void);

#define SSTR(S) ((S) != NULL ? (S) : "")

int
main (int argc, char *argv[])
{
  char *tmp;
  char ** exec_argv;
  int i, done_mode = 0, verbose = 0;
  enum mode ellcc_mode;

  if (argc < 2)
    {
      /* FIXME: Print usage output instead */
      fatal ("too few arguments");
    }

  prog_argv = argv;

#if defined(WIN32_NATIVE)
  tmp = strrchr (argv[0], '\\');
  if (tmp != NULL)
    {
      tmp++;
    }
#elif !defined (VMS)
  tmp = strrchr (argv[0], '/');
  if (tmp != NULL)
    {
      tmp++;
    }
#else
  tmp = argv[0];
#endif

  progname = (tmp == NULL) ? argv[0] : tmp;

  tmp = &progname[strlen(progname)-2];
  if (strcmp (tmp, "cc") == 0)
    {
      ellcc_mode = ELLCC_COMPILE_MODE;
    }
  else if (strcmp (tmp, "ld") == 0)
    {
      ellcc_mode = ELLCC_LINK_MODE;
    }
  else if (strcmp (tmp, "it") == 0)
    {
      ellcc_mode = ELLCC_INIT_MODE;
    }
  else
    {
      ellcc_mode = ELLCC_COMPILE_MODE;
    }

  exec_args = xnew(argc, int);
  exec_args[0] = 0;
  for (i = 1; i < argc; i++)
    {
      exec_args[i] = -1;
    }

  for (i = 1; i < argc; i++)
    {
      if (strncmp (argv[i], "--mode=", (size_t)7) == 0)
        {
          char *modeopt = &argv[i][7];

          if (done_mode && strcmp (modeopt, "verbose") != 0)
	    {
	      fatal ("more than one mode specified");
	    }
          if (strcmp (modeopt, "link") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_LINK_MODE;
            }
          else if (strcmp (modeopt, "compile") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_COMPILE_MODE;
            }
          else if (strcmp (modeopt, "init") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_INIT_MODE;
            }
          else if (strcmp (modeopt, "verbose") == 0)
	    {
	      verbose++;
	    }
	  else
	    {
	      fatal ("Mode must be link, compile, init, or verbose");
	    }
        }
      else if (strcmp (argv[i], "--mod-location") == 0)
        {
          printf ("%s\n", ELLCC_MODDIR);
          exit (EXIT_SUCCESS);
        }
      else if (strcmp (argv[i], "--mod-site-location") == 0)
        {
          printf ("%s\n", ELLCC_SITEMODS);
          exit (EXIT_SUCCESS);
        }
      else if (strcmp (argv[i], "--mod-archdir") == 0)
        {
          printf ("%s\n", ELLCC_ARCHDIR);
          exit (EXIT_SUCCESS);
        }
      else if (strcmp (argv[i], "--mod-config") == 0)
        {
          printf ("%s\n", ELLCC_CONFIG);
          exit (EXIT_SUCCESS);
        }
      else if (strncmp (argv[i], "--mod-name=", (size_t)11) == 0)
	{
	  mod_name = &argv[i][11];
	}
      else if (strncmp (argv[i], "--mod-title=", (size_t)12) == 0)
	{
	  mod_title = &argv[i][12];
	}
      else if (strncmp (argv[i], "--mod-version=", (size_t)14) == 0)
	{
	  mod_version = &argv[i][14];
	}
      else if (strncmp (argv[i], "--mod-output=", (size_t)13) == 0)
	{
	  mod_output = &argv[i][13];
	}
      else
        {
          exec_args[exec_argc] = i;
          exec_argc++;
        }
    }

  if (ellcc_mode == ELLCC_LINK_MODE && mod_output == NULL)
    {
      fatal ("must specify --mod-output when linking");
    }
  if (ellcc_mode == ELLCC_INIT_MODE && mod_output == NULL)
    {
      fatal ("must specify --mod-output when creating init file");
    }
  if (ellcc_mode == ELLCC_INIT_MODE && mod_name == NULL)
    {
      fatal ("must specify --mod-name when creating init file");
    }

  /*
   * We now have the list of arguments to pass to the compiler or
   * linker (or to process for doc files).  We can do the real work now.
   */
  if (verbose)
    {
      printf ("ellcc driver version %s for EMODULES version %s (%ld)\n",
	      ELLCC_EMACS_VER, EMODULES_VERSION, EMODULES_REVISION);
    }

#ifdef DEBUG
  if (verbose >= 2)
    {
      printf ("              mode = %d (%s)\n", (int)ellcc_mode,
	      ellcc_mode_name (ellcc_mode));
      printf ("       module_name = \"%s\"\n", SSTR(mod_name));
      printf ("      module_title = \"%s\"\n", SSTR(mod_title));
      printf ("    module_version = \"%s\"\n", SSTR(mod_version));

      printf ("                CC = %s\n", ELLCC_CC);
      printf ("            CFLAGS = %s\n", ELLCC_CFLAGS);
      printf ("      CC PIC flags = %s\n", ELLCC_DLL_CFLAGS);
      printf ("                LD = %s\n", ELLCC_DLL_LD);
      printf ("           LDFLAGS = %s\n", ELLCC_DLL_LDFLAGS);
      printf ("      architecture = %s\n", ELLCC_CONFIG);
      printf (" Include directory = %s/include\n", ELLCC_ARCHDIR);
      printf ("\n");
    }
#endif

  if (exec_argc < 2)
    {
      /* FIXME: Print usage output instead */
      fatal ("too few arguments");
    }

  /*
   * Get the overrides from the environment
   */
  OVERENV(ellcc, "ELLCC", ELLCC_CC);
  OVERENV(ellld, "ELLLD", ELLCC_DLL_LD);
  OVERENV(ellcflags, "ELLCFLAGS", ELLCC_CFLAGS);
  OVERENV(ellldflags, "ELLLDFLAGS", ELLCC_LDFLAGS);
  OVERENV(elldllflags, "ELLDLLFLAGS", ELLCC_DLL_LDFLAGS);
  OVERENV(ellpicflags, "ELLPICFLAGS", ELLCC_DLL_CFLAGS);

  switch (ellcc_mode)
    {
    case ELLCC_COMPILE_MODE:
      exec_argv = do_compile_mode ();
      break;
    case ELLCC_LINK_MODE:
      exec_argv = do_link_mode ();
      break;
    default:
      exec_argv = do_init_mode ();
      break;
    }

  /*
   * The arguments to pass on to the desired program have now been set
   * up and we can run the program.
   */
  if (verbose)
    {
      for (i = 0; i < real_argc; i++)
	{
	  printf ("%s ", exec_argv[i]);
	}
      printf ("\n");
      (void)fflush (stdout);
    }

  /* Terminate argument list. */
  exec_argv = add_string (exec_argv, NULL);

  i = execvp (exec_argv[0], exec_argv);
  if (verbose)
    {
      printf ("%s exited with status %d\n", exec_argv[0], i);
    }
  exit (i);
}

/* Like malloc but get fatal error if memory is exhausted.  */
static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    {
      fatal ("virtual memory exhausted");
    }
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */
static void *
xrealloc (void *ptr, size_t size)
{
  void *result = realloc (ptr, size);
  if (result == NULL)
    {
      fatal ("virtual memory exhausted");
    }
  return result;
}

/* Like strdup but get fatal error if memory is exhausted.  */
static char *
xstrdup (char *s)
{
  char *result = strdup (s);
  if (result == NULL)
    {
      fatal ("virtual memory exhausted");
    }
  return result;
}

/* Print error message and exit.  */
static DOESNT_RETURN
fatal (char *format, ...)
{
  va_list ap;

  va_start (ap, format);
  (void)fprintf (stderr, "%s: ", progname);
  (void)vfprintf (stderr, format, ap);
  (void)fprintf (stderr, "\n");
  va_end (ap);
  exit (EXIT_FAILURE);
}

static char **
add_string (char **exec_argv, char *str)
{
  if (real_argc >= exec_length)
    {
      exec_length = real_argc + EXEC_GROW_SIZE;
      exec_argv = xrnew (exec_argv, exec_length, char *);
    }
  exec_argv[real_argc++] = str;
  return exec_argv;
}

/*
 * Add a string to the argument vector list that will be passed on down
 * to the compiler or linker. We need to split individual words into
 * arguments, taking quoting into account.
 */
static char **
add_to_argv (char **exec_argv, const char *str)
{
  /* Don't add nonexistent strings */
  if (str == NULL)
    {
      return exec_argv;
    }

  /* Skip leading whitespace */
  while (isspace (*str))
    {
      str++;
    }

  /* Don't add nonexistent strings */
  if (*str == '\0')
    {
      return exec_argv;
    }

  while (*str != '\0')
    {
      const char *s;
      char *arg;
      int l;

      s = str; /* Mark the start of THIS argument */

      /* Find contiguous nonwhitespace characters */
      while (*str != '\0' && !isspace(*str))
	{
	  if (*str == '\\')  /* Escaped character */
	    {
	      str++;
	      if (*str != '\0')
		{
		  str++;
		}
	    }
	  else if (*str == '\'')
	    {
	      str++;
	      while (*str != '\0' && *str != '\'')
		{
		  if (str[0] == '\\' && str[1] != '\0')
		    {
		      str += 2;
		    }
		  else
		    {
		      str++;
		    }
		}
	      if (*str == '\'')
		{
		  str++;
		}
	    }
	  else if (*str == '\"')
	    {
	      str++;
	      while (*str != '\0' && *str != '\"')
		{
		  if (str[0] == '\\' && str[1] != '\0')
		    {
		      str += 2;
		    }
		  else
		    {
		      str++;
		    }
		}
	      if (*str == '\"')
		{
		  str++;
		}
	    }
	  else
	    {
	      str++;   /* Normal character.  Advance the pointer. */
	    }
	}

      /* Reached the end of the argument. Add it. */
      l = str-s;
      arg = xnew(l+1, char);
      strncpy(arg, s, (size_t)l);
      arg[l] = '\0';
      exec_argv = add_string (exec_argv, arg);

      /* Skip trailing whitespace */
      while (isspace (*str))
	{
	  str++;
	}
    }

  return exec_argv;
}

/*
 * For compile mode, things are pretty straight forward. All we need to do
 * is build up the argument vector and exec() it. We must just make sure
 * that we get all of the required arguments in place.
 */
static char **
do_compile_mode (void)
{
  int i;
  char **exec_argv = xnew (exec_argc + 20, char *);

  exec_argv = add_to_argv (exec_argv, ellcc);
  exec_argv = add_to_argv (exec_argv, ELLCC_CF_ALL);
  exec_argv = add_to_argv (exec_argv, ellcflags);
  exec_argv = add_to_argv (exec_argv, ellpicflags);
  exec_argv = add_to_argv (exec_argv, "-DPIC");
  exec_argv = add_to_argv (exec_argv, "-DEMACS_MODULE");
#ifdef XEMACS
  /* Cover both cases */
  exec_argv = add_to_argv (exec_argv, "-DXEMACS_MODULE");
  exec_argv = add_to_argv (exec_argv, "-Dxemacs");
#endif
  exec_argv = add_to_argv (exec_argv, "-Demacs");
  for (i = 1; i < exec_argc; i++)
    {
      exec_argv = add_string (exec_argv, xstrdup (prog_argv[exec_args[i]]));
    }
  return exec_argv;
}

/*
 * For link mode, things are a little bit more complicated. We need to
 * insert the linker commands first, replace any occurrence of ELLSONAME
 * with the desired output file name, insert the output arguments, then
 * all of the provided arguments, then the final post arguments. Once
 * all of this has been done, the argument vector is ready to run.
 */
static char **
do_link_mode (void)
{
  int i,x;
  char *t, *ts;
  char **exec_argv = xnew(exec_argc + 10, char *);

  exec_argv = add_to_argv (exec_argv, ellld);
  exec_argv = add_to_argv (exec_argv, ellldflags);
  exec_argv = add_to_argv (exec_argv, elldllflags);
  exec_argv = add_to_argv (exec_argv, ELLCC_DLL_LDO);
  exec_argv = add_to_argv (exec_argv, mod_output);
  for (i = 1; i < exec_argc; i++)
    {
      exec_argv = add_string (exec_argv, xstrdup (prog_argv[exec_args[i]]));
    }
  exec_argv = add_to_argv (exec_argv, ELLCC_DLL_POST);

  /*
   * Now go through each argument and replace ELLSONAME with mod_output.
   */
  for (i = 0; i < real_argc; i++)
    {
      x = 0;
      ts = xnew (2 * strlen (exec_argv[i]), char);
      ts[0] = '\0';

      t = exec_argv[i];
      while (*t != '\0')
        {
          if (*t == 'E')
            {
              if (strncmp (t, "ELLSONAME", (size_t)9) == 0)
                {
                  strcat (ts, mod_output);
                  x += strlen (mod_output);
                  t += 8;
                }
              else
                {
                  ts[x] = *t;
                  x++;
                  ts[x] = '\0';
                }
            }
          else
            {
              ts[x] = *t;
              x++;
              ts[x] = '\0';
            }
          t++;
        }
      free (exec_argv[i]);
      exec_argv[i] = ts;
    }
  return exec_argv;
}

/*
 * In init mode, things are a bit easier. We assume that the only things
 * passed on the command line are the names of source files which the
 * make-doc program will be processing. We prepare the output file with
 * the header information first, as make-doc will append to the file by
 * special dispensation.
 */
static char **
do_init_mode (void)
{
  int i;
  char *ts, *mdocprog;
  char **exec_argv = xnew(exec_argc + 8, char *);
  FILE *mout = fopen (mod_output, "w");

  if (mout == NULL)
    {
      fatal ("Failed to open output file %s", mod_output);
    }
  fprintf (mout, "/* DO NOT EDIT - AUTOMATICALLY GENERATED */\n\n");
  fprintf (mout, "#include <emodules.h>\n\n");
  fprintf (mout, "#ifdef __cplusplus\n");
  fprintf (mout, "extern \"C\" {\n");
  fprintf (mout, "#endif\n");
  fprintf (mout, "extern const long emodule_compiler;\n");
  fprintf (mout, "extern const char *emodule_name, *emodule_version, *emodule_title;\n");
  fprintf (mout, "extern void docs_of_%s (void);\n", SSTR(mod_name));
  fprintf (mout, "#ifdef __cplusplus\n");
  fprintf (mout, "}\n");
  fprintf (mout, "#endif\n\n");
  fprintf (mout, "const long emodule_compiler = %ld;\n", EMODULES_REVISION);
  fprintf (mout, "const char *emodule_name = \"%s\";\n", SSTR(mod_name));
  fprintf (mout, "const char *emodule_version = \"%s\";\n", SSTR(mod_version));
  fprintf (mout, "const char *emodule_title = \"%s\";\n\n", SSTR(mod_title));
  fprintf (mout, "void docs_of_%s ()\n", SSTR(mod_name));
  if (fclose (mout) != 0)
    {
      fatal ("Failed to close output file %s", mod_output);
    }

  mdocprog = getenv ("ELLMAKEDOC");
  if (mdocprog == NULL)
    {
      mdocprog = xnew (14 + strlen (ELLCC_ARCHDIR), char);
      sprintf (mdocprog, "%s/make-docfile", ELLCC_ARCHDIR);
      exec_argv = add_to_argv (exec_argv, mdocprog);
      free (mdocprog);
    }
  else
    {
      exec_argv = add_to_argv (exec_argv, mdocprog);
    }
  ts = xnew (4 + strlen (mod_output), char);
  sprintf (ts, "-E %s", mod_output);
  exec_argv = add_to_argv (exec_argv, ts);
  free (ts);
  for (i = 1; i < exec_argc; i++)
    {
      exec_argv = add_string (exec_argv, xstrdup (prog_argv[exec_args[i]]));
    }
  return exec_argv;
}

#endif /* HAVE_SHLIB */
