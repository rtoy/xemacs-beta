/* Get the system load averages.
   Copyright (C) 1985-1989, 1991-1995, 1997, 2001-2008
       Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

   NOTE: The canonical source of this file is maintained with gnulib.
   Bugs can be reported to bug-gnulib@gnu.org.

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

/* Synced up with: FSF 23.1.92. */
/* Synced by: Ben Wing. */

/* Compile-time symbols that this file uses:

   HAVE_SYS_PSTAT_H		Defined if the pstat() function is available
				(HPUX only); the GNU version uses
                                pstat_getdynamic() and HAVE_PSTAT_GETDYNAMIC.

				[[ GNU says: The best way to get the
				definition is through the AC_FUNC_GETLOADAVG
				macro that comes with autoconf 2.13 or newer.
				If that isn't an option, then just put
				AC_CHECK_FUNCS(pstat_getdynamic) in your
				configure.in file. ]] But we don't do this,
				apparently we do the equivalent of
				AC_FUNC_GETLOADAVG dynamically.

   FIXUP_KERNEL_SYMBOL_ADDR()	Adjust address in returned struct nlist.
   KERNEL_FILE			Pathname of the kernel to nlist.
   LDAV_CVT()			Scale the load average from the kernel.
				Returns a double.
   LDAV_SYMBOL			Name of kernel symbol giving load average.
   LOAD_AVE_TYPE		Type of the load average array in the kernel.
				Must be defined; or we have libkstat;
				otherwise, no load average is available.
   NLIST_STRUCT			Include nlist.h, not a.out.h, and
				the nlist n_name element is a pointer,
				not an array.
   LINUX_LDAV_FILE		[__linux__]: File containing load averages.
   HAVE_LOCALE_H                locale.h is available.
   HAVE_SETLOCALE               The `setlocale' function is available.

   Specific system predefines this file uses, aside from setting
   default values if not emacs:

   BSD				Real BSD, not just BSD-like.
   hpux
   sgi
   WIN32_NATIVE			No-op for Windows9x/NT.
   CYGWIN			No-op for Cygwin.
   __linux__			Linux: assumes /proc filesystem mounted.
   				Support from Michael K. Johnson.
   __NetBSD__			NetBSD: assumes /kern filesystem mounted.
   __OpenBSD__			OpenBSD: ditto.

   In addition, to avoid nesting many #ifdefs, we internally set
   LDAV_DONE to indicate that the load average has been computed.

   We also #define LDAV_PRIVILEGED if a program will require
   special installation to be able to call getloadavg.  */

/* This should always be first.  */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "lisp.h"
#include "sysfile.h" /* for encapsulated open, close, read, write */

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
#ifndef HAVE_SETLOCALE
# define setlocale(Category, Locale) /* empty */
#endif

#ifndef HAVE_GETLOADAVG

/* The existing Emacs configuration files define a macro called
   LOAD_AVE_CVT, which accepts a value of type LOAD_AVE_TYPE, and
   returns the load average multiplied by 100.  What we actually want
   is a macro called LDAV_CVT, which returns the load average as an
   unmultiplied double.

   For backwards compatibility, we'll define LDAV_CVT in terms of
   LOAD_AVE_CVT, but future machine config files should just define
   LDAV_CVT directly.  */

# if !defined(LDAV_CVT) && defined(LOAD_AVE_CVT)
#  define LDAV_CVT(n) (LOAD_AVE_CVT (n) / 100.0)
# endif

#if defined (HAVE_KSTAT_H)
#include <kstat.h>
#endif /* HAVE_KSTAT_H */

/* Same issues as for NeXT apply to the HURD-based GNU system.  */
# ifdef __GNU__
#  undef BSD
#  undef FSCALE
# endif /* __GNU__ */

/* Set values that are different from the defaults, which are
   set a little farther down with #ifndef.  */


/* Some shorthands.  */

# if defined (HPUX) && !defined (hpux)
#  define hpux
# endif

# if defined (__hpux) && !defined (hpux)
#  define hpux
# endif

# if defined (__sun) && !defined (sun)
#  define sun
# endif

# if defined (__SVR4) && !defined (SVR4)
#  define SVR4
# endif

# if (defined(sun) && defined(SVR4)) || defined (SOLARIS2)
#  define SUNOS_5
# endif

# if defined (__osf__) && (defined (__alpha) || defined (__alpha__))
#  define OSF_ALPHA
#  /* XEmacs: First three below from FSF 23.1.92. */
#  include <sys/mbuf.h>
#  include <sys/socket.h>
#  include <net/route.h>
#  include <netdb.h>
#  include <netinet/in.h>		/* Needed for Digital UNIX V3 */
#  include <net/proto_net.h>
#  include <sys/table.h>
# endif

#if defined (__osf__) && (defined (mips) || defined (__mips__))
# define OSF_MIPS
# include <sys/table.h>
#endif


/* VAX C can't handle multi-line #ifs, or lines longer than 256 chars.  */
# ifndef LOAD_AVE_TYPE

#  ifdef sun
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef decstation
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef sgi
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef SVR4
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef sony_news
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef OSF_ALPHA
#   define LOAD_AVE_TYPE long
#  endif

#  if defined (ardent) && defined (titan)
#   define LOAD_AVE_TYPE long
#  endif

#  ifdef _AIX
#   define LOAD_AVE_TYPE long
#  endif

# endif /* No LOAD_AVE_TYPE.  */

# ifdef OSF_ALPHA
/* <sys/param.h> defines an incorrect value for FSCALE on Alpha OSF/1,
   according to ghazi@noc.rutgers.edu.  */
#  undef FSCALE
#  define FSCALE 1024.0
# endif

# ifndef	FSCALE

/* SunOS and some others define FSCALE in sys/param.h.  */

#  if defined(MIPS) || defined(SVR4) || defined(decstation)
#   define FSCALE 256
#  endif

#  if defined (sgi)
/* Sometimes both MIPS and sgi are defined, so FSCALE was just defined
   above under #ifdef MIPS.  But we want the sgi value.  */
#   undef FSCALE
#   define FSCALE 1000.0
#  endif

#  if defined (ardent) && defined (titan)
#   define FSCALE 65536.0
#  endif

#  ifdef _AIX
#   define FSCALE 65536.0
#  endif

# endif	/* Not FSCALE.  */

# if !defined (LDAV_CVT) && defined (FSCALE)
#  define LDAV_CVT(n) (((double) (n)) / FSCALE)
# endif

#if 0 /* XEmacs change ...  we check for nlist.h during configure and
         automatically set NLIST_STRUCT if so; we have no HAVE_NLIST_H. */
# ifndef NLIST_STRUCT
#  if HAVE_NLIST_H
#   define NLIST_STRUCT
#  endif
# endif
#endif

# if defined(sgi) || (defined(mips) && !defined(BSD))
#  define FIXUP_KERNEL_SYMBOL_ADDR(nl) ((nl)[0].n_value &= ~(1 << 31))
# endif

# if !defined (KERNEL_FILE) && defined (hpux)
#  define KERNEL_FILE "/hp-ux"
# endif

# if !defined(KERNEL_FILE) && (defined(MIPS) || defined(SVR4) || defined(ISC) || defined (sgi) || (defined (ardent) && defined (titan)))
#  define KERNEL_FILE "/unix"
# endif

# if !defined(LDAV_SYMBOL) && (defined(hpux) || defined(SVR4) || defined(ISC) || defined(sgi) || (defined (ardent) && defined (titan)) || defined (_AIX))
#  define LDAV_SYMBOL "avenrun"
# endif

# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

# include <stdio.h>

/* LOAD_AVE_TYPE should only get defined if we're going to use the
   nlist method.  */
# if !defined(LOAD_AVE_TYPE) && (defined(BSD) || defined(LDAV_CVT) || defined(KERNEL_FILE) || defined(LDAV_SYMBOL))
#  define LOAD_AVE_TYPE double
# endif

# ifdef LOAD_AVE_TYPE

#  ifndef __linux__
#   ifndef NLIST_STRUCT
#    include <a.out.h>
#   else /* NLIST_STRUCT */
#    include <nlist.h>
#   endif /* NLIST_STRUCT */

#   ifdef SUNOS_5
#    include <fcntl.h>
#    include <kvm.h>
#    include <kstat.h>
#   endif

   /* XEmacs: GNU says: defined (hpux) && defined (HAVE_PSTAT_GETDYNAMIC) */
#   if defined (HAVE_SYS_PSTAT_H)
#    include <sys/pstat.h>
#   endif /* HAVE_SYS_PSTAT_H (on HPUX) */

#   ifndef KERNEL_FILE
#    define KERNEL_FILE "/vmunix"
#   endif /* KERNEL_FILE */

#   ifndef LDAV_SYMBOL
#    define LDAV_SYMBOL "_avenrun"
#   endif /* LDAV_SYMBOL */
#  endif /* __linux__ */

#  ifndef LDAV_CVT
#   define LDAV_CVT(n) ((double) (n))
#  endif /* !LDAV_CVT */

# endif /* LOAD_AVE_TYPE */

# if defined(__GNU__) && !defined (NeXT)
/* XEmacs -- FSF added code for the HURD, which depends on some NeXT
   code that we deleted as outdated.  We hardly care about compiling
   on the HURD, but I'm leaving this stuff here just in case; if someone
   wants to get this working, figure out which NeXT code needs to be
   restored. */
# error "Code not working currently"
/* Note that NeXT Openstep defines __GNU__ even though it should not.  */
/* GNU system acts much like NeXT, for load average purposes,
   but not exactly.  */
#  define NeXT
#  define host_self mach_host_self
# endif /* defined(__GNU__) && !defined (NeXT) */

/* (ifdef NeXT ...  deleted) */

# ifdef sgi
#  include <sys/sysmp.h>
# endif /* sgi */


/* Avoid static vars inside a function since in HPUX they dump as pure.  */

#if !defined(HAVE_LIBKSTAT) && defined(LOAD_AVE_TYPE)
/* File descriptor open to /dev/kmem or VMS load ave driver.  */
static int channel;
/* Nonzero if channel is valid.  */
static int getloadavg_initialized;
/* Offset in kmem to seek to read load average, or 0 means invalid.  */
static long offset;

#  if !defined(sgi) && !defined(__linux__)
static struct nlist name_list[2];
#  endif /* Not Linux or sgi */

#  ifdef SUNOS_5
static kvm_t *kd;
#  endif /* SUNOS_5 */

#ifndef countof
# define countof(x) (sizeof (x) / sizeof (*(x)))
#endif

#endif /* LOAD_AVE_TYPE && !HAVE_LIBKSTAT */

/* Put the 1 minute, 5 minute and 15 minute load averages
   into the first NELEM elements of LOADAVG.
   Return the number written (never more than 3, but may be less than NELEM),
   or -1 if an error occurred.  */

int getloadavg (double loadavg[], int nelem);

int
getloadavg (double loadavg[], int nelem)
{
  int elem = 0;			/* Return value.  */

# ifdef NO_GET_LOAD_AVG
#  define LDAV_DONE
  /* Set errno to zero to indicate that there was no particular error;
     this function just can't work at all on this system.  */
  errno = 0;
  elem = -2;
# endif /* NO_GET_LOAD_AVG */

#if ! defined (LDAV_DONE) && defined (HAVE_KSTAT_H) && defined (HAVE_LIBKSTAT)
/* Use libkstat because we don't have to be root.  */
#  define LDAV_DONE
  kstat_ctl_t *kc;
  kstat_t *ksp;
  kstat_named_t *kn;

  kc = kstat_open ();
  if (kc == 0)
    return -1;
  ksp = kstat_lookup (kc, "unix", 0, "system_misc");
  if (ksp == 0 )
    return -1;
  if (kstat_read (kc, ksp, 0) == -1)
    return -1;


  kn = kstat_data_lookup (ksp, "avenrun_1min");
  if (kn == 0)
    {
      /* Return -1 if no load average information is available.  */
      nelem = 0;
      elem = -1;
    }

  if (nelem >= 1)
    loadavg[elem++] = (double) kn->value.ul/FSCALE;

  if (nelem >= 2)
    {
      kn = kstat_data_lookup (ksp, "avenrun_5min");
      if (kn != 0)
	{
	  loadavg[elem++] = (double) kn->value.ul/FSCALE;

	  if (nelem >= 3)
	    {
	      kn = kstat_data_lookup (ksp, "avenrun_15min");
	      if (kn != 0)
		loadavg[elem++] = (double) kn->value.ul/FSCALE;
	    }
	}
    }

  kstat_close (kc);
# endif /* HAVE_LIBKSTAT */
# if !defined (LDAV_DONE) && defined (HAVE_SYS_PSTAT_H)
        /* GNU says: .... && defined (hpux) && defined (HAVE_PSTAT_GETDYNAMIC) */
#  define LDAV_DONE
  /* This is totally undocumented, and is not guaranteed to work, but
     mayhap it might ....  If it does work, it will work only on HP-UX
     8.0 or later.  -- Darryl Okahata <darrylo@sr.hp.com> */
#  undef LOAD_AVE_TYPE
#  undef LOAD_AVE_CVT
#  undef LDAV_SYMBOL

  struct pst_dynamic dyn_info;
#if 0 /* GNU 23.1.92 */
  /* Use pstat_getdynamic() because we don't have to be root.  */
  if (pstat_getdynamic (&dyn_info, sizeof (dyn_info), 0, 0) < 0)
    return -1;
#else
  union pstun		statbuf;
  statbuf.pst_dynamic = &dyn_info;
  if (pstat (PSTAT_DYNAMIC, statbuf, sizeof (dyn_info), 0, 0) == -1)
    return -1;
#endif
  if (nelem > 0)
    loadavg[elem++] = dyn_info.psd_avg_1_min;
  if (nelem > 1)
    loadavg[elem++] = dyn_info.psd_avg_5_min;
  if (nelem > 2)
    loadavg[elem++] = dyn_info.psd_avg_15_min;

# endif /* HAVE_SYS_PSTAT_H (GNU: hpux && HAVE_PSTAT_GETDYNAMIC) */

# if !defined (LDAV_DONE) && defined (__linux__)
#  define LDAV_DONE
#  undef LOAD_AVE_TYPE

#  ifndef LINUX_LDAV_FILE
#   define LINUX_LDAV_FILE "/proc/loadavg"
#  endif

  char ldavgbuf[40];
  double load_ave[3];
  int fd, count;

  fd = retry_open (LINUX_LDAV_FILE, O_RDONLY);
  if (fd == -1)
    return -1;
  count = retry_read (fd, ldavgbuf, 40);
  (void) retry_close (fd);
  if (count <= 0)
    return -1;

  /* The following sscanf must use the C locale.  */
  setlocale (LC_NUMERIC, "C");
  count = sscanf (ldavgbuf, "%lf %lf %lf",
		  &load_ave[0], &load_ave[1], &load_ave[2]);
  setlocale (LC_NUMERIC, "");
  if (count < 1)
    return -1;

  for (elem = 0; elem < nelem && elem < count; elem++)
    loadavg[elem] = load_ave[elem];

  return elem;

# endif /* __linux__ */

# if !defined (LDAV_DONE) && (defined (__NetBSD__) || defined (__OpenBSD__))
#  define LDAV_DONE
#  undef LOAD_AVE_TYPE

#  ifndef NETBSD_LDAV_FILE
#   define NETBSD_LDAV_FILE "/kern/loadavg"
#  endif

  unsigned long int load_ave[3], scale;
  int count;
  FILE *fp;

  fp = retry_fopen (NETBSD_LDAV_FILE, "r");
  if (fp == NULL)
    return -1;
  count = fscanf (fp, "%lu %lu %lu %lu\n",
		  &load_ave[0], &load_ave[1], &load_ave[2],
		  &scale);
  (void) retry_fclose (fp);
  if (count != 4)
    return -1;

  for (elem = 0; elem < nelem; elem++)
    loadavg[elem] = (double) load_ave[elem] / (double) scale;
#endif /* __NetBSD__ or __OpenBSD__ */

#if !defined (LDAV_DONE) && defined (WIN32_ANY)
#define LDAV_DONE

  /* A faithful emulation is going to have to be saved for a rainy day.  */
  for ( ; elem < nelem; elem++)
    {
      loadavg[elem] = 0.0;
    }
#endif  /* WIN32_NATIVE or CYGWIN */

#if !defined (LDAV_DONE) && defined(LOAD_AVE_TYPE)

  /* UNIX-specific code -- read the average from /dev/kmem.  */

#  define LDAV_PRIVILEGED		/* This code requires special installation.  */

  LOAD_AVE_TYPE load_ave[3];

  /* Get the address of LDAV_SYMBOL.  */
  if (offset == 0)
    {
#  ifndef sgi
#   ifndef NLIST_STRUCT
      strcpy (name_list[0].n_name, LDAV_SYMBOL);
      strcpy (name_list[1].n_name, "");
#   else /* NLIST_STRUCT */
      /* #### XEmacs deletes HAVE_STRUCT_NLIST_N_UN_N_NAME code. Why? */
      name_list[0].n_name = (char *) LDAV_SYMBOL;
      name_list[1].n_name = 0;
#   endif /* NLIST_STRUCT */

#   ifndef SUNOS_5
      if (
#    if !(defined (_AIX) && !defined (ps2))
	  nlist (KERNEL_FILE, name_list)
#    else  /* _AIX */
	  knlist (name_list, 1, sizeof (name_list[0]))
#    endif
	  >= 0)
	  /* Omit "&& name_list[0].n_type != 0 " -- it breaks on Sun386i.  */
	  {
#    ifdef FIXUP_KERNEL_SYMBOL_ADDR
	    FIXUP_KERNEL_SYMBOL_ADDR (name_list);
#    endif
	    offset = name_list[0].n_value;
	  }
#   endif /* !SUNOS_5 */
#  else  /* sgi */
      int ldav_off;

      ldav_off = sysmp (MP_KERNADDR, MPKA_AVENRUN);
      if (ldav_off != -1)
	offset = (long) ldav_off & 0x7fffffff;
#  endif /* sgi */
    }

  /* Make sure we have /dev/kmem open.  */
  if (!getloadavg_initialized)
    {
#  ifndef SUNOS_5
      channel = retry_open ("/dev/kmem", 0);
      if (channel >= 0)
	{
	  /* Set the channel to close on exec, so it does not
	     litter any child's descriptor table.  */
#   ifdef F_SETFD
#    ifndef FD_CLOEXEC
#     define FD_CLOEXEC 1
#    endif
	  (void) fcntl (channel, F_SETFD, FD_CLOEXEC);
#   endif
	  getloadavg_initialized = 1;
	}
#  else /* SUNOS_5 */
      /* We pass 0 for the kernel, corefile, and swapfile names
	 to use the currently running kernel.  */
      kd = kvm_open (0, 0, 0, O_RDONLY, 0);
      if (kd != 0)
	{
	  /* nlist the currently running kernel.  */
	  kvm_nlist (kd, name_list);
	  offset = name_list[0].n_value;
	  getloadavg_initialized = 1;
	}
#  endif /* SUNOS_5 */
    }

  /* If we can, get the load average values.  */
  if (offset && getloadavg_initialized)
    {
      /* Try to read the load.  */
#  ifndef SUNOS_5
      if (lseek (channel, offset, 0) == -1L
	  || retry_read (channel, (char *) load_ave, sizeof (load_ave))
	  != sizeof (load_ave))
	{
	  retry_close (channel);
	  getloadavg_initialized = 0;
	}
#  else  /* SUNOS_5 */
      if (kvm_read (kd, offset, (char *) load_ave, sizeof (load_ave))
	  != sizeof (load_ave))
        {
          kvm_close (kd);
          getloadavg_initialized = 0;
	}
#  endif /* SUNOS_5 */
    }

  if (offset == 0 || !getloadavg_initialized)
    return -1;

  if (nelem > 0)
    loadavg[elem++] = LDAV_CVT (load_ave[0]);
  if (nelem > 1)
    loadavg[elem++] = LDAV_CVT (load_ave[1]);
  if (nelem > 2)
    loadavg[elem++] = LDAV_CVT (load_ave[2]);

#  define LDAV_DONE
# endif /* !LDAV_DONE && LOAD_AVE_TYPE */

  return elem;
}

#endif /* ! HAVE_GETLOADAVG */

#ifdef TEST
void
main (int argc, char **argv)
{
  int naptime = 0;

  if (argc > 1)
    naptime = atoi (argv[1]);

  while (1)
    {
      double avg[3];
      int loads;

      errno = 0;		/* Don't be misled if it doesn't set errno.  */
      loads = getloadavg (avg, 3);
      if (loads == -1)
	{
	  perror ("Error getting load average");
	  exit (1);
	}
      if (loads > 0)
	printf ("1-minute: %f  ", avg[0]);
      if (loads > 1)
	printf ("5-minute: %f  ", avg[1]);
      if (loads > 2)
	printf ("15-minute: %f  ", avg[2]);
      if (loads > 0)
	putchar ('\n');

      if (naptime == 0)
	break;
      sleep (naptime);
    }

  exit (0);
}
#endif /* TEST */

/* arch-tag: 2b37a242-6289-41f4-8cd5-0e73fd615db1
   (do not change this comment) */
