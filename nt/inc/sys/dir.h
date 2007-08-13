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

/* Synched up with: FSF 19.30. */

/*
	<dir.h> -- definitions for 4.2BSD-compatible directory access

	last edit:	09-Jul-1983	D A Gwyn
*/

#ifdef VMS
#ifndef FAB$C_BID
#include <fab.h>
#endif
#ifndef NAM$C_BID
#include <nam.h>
#endif
#ifndef RMS$_SUC
#include <rmsdef.h>
#endif
#include "vms-dir.h"
#endif /* VMS */

#define DIRBLKSIZ	512		/* size of directory block */
#ifdef VMS
#define MAXNAMLEN	(DIR$S_NAME + 7) /* 80 plus room for version #.  */
#define MAXFULLSPEC	NAM$C_MAXRSS /* Maximum full spec */
#else
#ifdef WINDOWSNT
#define MAXNAMLEN	255
#else  /* not WINDOWSNT */
#define MAXNAMLEN	15		/* maximum filename length */
#endif /* not WINDOWSNT */
#endif /* VMS */
	/* NOTE:  MAXNAMLEN must be one less than a multiple of 4 */

struct direct				/* data from readdir() */
	{
	long		d_ino;		/* inode number of entry */
	unsigned short	d_reclen;	/* length of this record */
	unsigned short	d_namlen;	/* length of string in d_name */
	char		d_name[MAXNAMLEN+1];	/* name of file */
	};

typedef struct
	{
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	dd_buf[DIRBLKSIZ];	/* directory block */
	}	DIR;			/* stream data from opendir() */

DIR *opendir (CONST char *filename);
void closedir (DIR *dirp);
struct direct *readdir (DIR *dirp);
struct direct *readdirver (DIR *dirp);
long telldir (DIR *dirp);
void seekdir (DIR *dirp, long loc);

#define rewinddir( dirp )	seekdir( dirp, 0L )
