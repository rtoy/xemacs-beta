/* This file is part of XEmacs.

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

/* Authorship:

   JWZ: long ago.
 */

/* Sun's standard and GCC's header files leave out prototypes for
   all sorts of functions. */

#ifndef _XEMACS_BROKEN_SUN_H_
#define _XEMACS_BROKEN_SUN_H_

#ifdef __GNUC__
#include <stdlib.h>
#include <stddef.h>

/*********************** stdlib functions *********************/

/* extern void *	memchr (CONST void *, int, size_t); */

/* extern int	memcmp (CONST void *, CONST void *, size_t); */
/* extern void *	memcpy (void *, CONST void *, size_t); */
/* extern void *	memmove (void *, CONST void *, size_t);*/
/* extern void *	memset (void *, int, int); */
/* extern char *	strcat (char *, CONST char *); */
/* extern char *	strchr (CONST char *, int); */
/* extern int	strcmp (CONST char *, CONST char *); */
extern int	strcasecmp (char *, char *);
extern void	bzero (char *, int);
extern void	bcopy (char *, char *, int);

/* Yes, they even left these functions out! */
extern int      tolower (int);
extern int      toupper (int);

/*********************** stdio functions *********************/

#include <stdio.h> /* else can't declare FILE */

/* extern FILE	*fopen (CONST char *, CONST char *); */
/* extern FILE	*freopen (CONST char *, CONST char *, FILE *); */
extern FILE	*tmpfile (void);
extern int	fclose (FILE *);
extern char	*fgets (char *, int, FILE *);
extern int      fgetc (FILE *);
extern int      fflush (FILE *);
extern int      fprintf (FILE *, CONST char *, ...);
extern int      fputc (char, FILE *);
extern int      fputs (CONST char *, FILE *);
extern size_t   fread (void *, size_t, size_t, FILE *);
extern int      fscanf (FILE *, CONST char *, ...);
extern int	fgetpos (FILE *, long *);
extern int      fseek (FILE *, long, int);
extern int	fsetpos (FILE *, CONST long *);
extern long     ftell (FILE *);
extern size_t   fwrite (CONST void *, size_t, size_t, FILE *);
extern char	*gets (char *);
extern int	pclose (FILE *);
extern void     perror (CONST char *);
extern int      printf (CONST char *, ...);
extern int      puts (CONST char *);
extern int      remove (CONST char *);
extern int      rename (CONST char *, CONST char *);
extern int      rewind (FILE *);
extern int	scanf (CONST char *, ...);
extern int	sscanf (CONST char *, CONST char *, ...);
extern void 	setbuf (FILE *, char *);
extern int 	setvbuf (FILE *, char *, int, size_t);
extern int	ungetc (int, FILE *);
extern int	vprintf (CONST char *, void *);
extern int	vfprintf (FILE *, CONST char *, void *);
extern char	*vsprintf (char *, CONST char *, void *);

/*********************** signal functions *********************/

extern int	sigblock (int);
#ifndef sigmask
extern int	sigmask (int);
#endif
extern int	sigsetmask (int);
extern int	sigpause (int);

/*********************** time functions ***********************/

struct timeval;
struct timezone;

extern int	utimes (CONST char *, struct timeval *);
extern void	tzset (void);
extern time_t	time (time_t *);
extern int	gettimeofday (struct timeval *, struct timezone *);

/*********************** file-system functions *********************/

struct stat;
#include </usr/include/sys/types.h>

extern int	fsync (int);
extern int	lstat (CONST char *, struct stat *);
extern int	fchmod (int, mode_t);
extern char	*mktemp (char *);
/* extern int	creat (CONST char *, mode_t); better no decl than a conflicting one... */
extern int	symlink (CONST char *, CONST char *);
extern int	readlink (CONST char *, char *, int);
extern void	sync (void);
extern int	select (int, fd_set *, fd_set *, fd_set *, struct timeval *);
extern char *	getwd (char *);
/* extern int	lseek (int, long, int); better no decl than a conflicting one... */
extern int	_filbuf ();
extern int	_flsbuf ();

/**************** interprocess communication functions ******************/

extern int	recv (int, char *, int, int);
extern int	socket (int, int, int);
struct sockaddr;
extern int	connect (int, struct sockaddr *, int);
extern int	bind (int, struct sockaddr *, int);
extern int	listen (int, int);
extern int	accept (int, struct sockaddr *, int *);
extern int	gethostname (char *, int);
struct rusage;
extern int	wait3 (void *, int, struct rusage *);
extern int	nice (int);
extern int	killpg (int, int);
extern int	system (char *);


/*********************** low-level OS functions *********************/

extern int	ioctl (int, int, ...);
struct nlist;
extern int	nlist (CONST char *, struct nlist *);
extern int	munmap (void *, int);
extern int	brk (void *);
extern void *	sbrk (int);
struct rlimit;
extern int	getrlimit (int, struct rlimit *);
extern int	getpagesize (void);
extern int	shutdown (int, int);
extern int	mprotect (void *, int, int);

/*********************** miscellaneous functions *********************/

extern void	tputs (CONST char *cp, int affcnt, void (*)(int));
extern long	random (void);
extern int	srandom (int seed);

#endif /* __GNUC__ */

#endif /* _XEMACS_BROKEN_SUN_H_ */
