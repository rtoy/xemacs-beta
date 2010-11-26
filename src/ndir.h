/* definitions for 4.2BSD-compatible directory access

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_ndir_h_
#define INCLUDED_ndir_h_

#define DIRBLKSIZ	512		/* size of directory block */
#ifdef WIN32_NATIVE
#define MAXNAMLEN	4095
#else  /* not WIN32_NATIVE */
#define MAXNAMLEN	15		/* maximum filename length */
#endif /* not WIN32_NATIVE */
	/* NOTE:  MAXNAMLEN must be one less than a multiple of 4 */

struct direct				/* data from readdir() */
{
  long			d_ino;		/* inode number of entry */
  unsigned short	d_reclen;	/* length of this record */
  unsigned short	d_namlen;	/* length of string in d_name */
  char			d_name[MAXNAMLEN+1];	/* name of file */
};

typedef struct
{
  int	dd_fd;			/* file descriptor */
  int	dd_loc;			/* offset in block */
  int	dd_size;		/* amount of valid data */
  char	dd_buf[DIRBLKSIZ];	/* directory block */
}	DIR;			/* stream data from opendir() */

#ifdef WIN32_NATIVE

DIR *mswindows_opendir (const Ibyte *filename);
int mswindows_closedir (DIR *dirp);
struct direct *mswindows_readdir (DIR *dirp);

#else /* not WIN32_NATIVE */

DIR *opendir (const Extbyte *filename);
int closedir (DIR *dirp);
struct direct *readdir (DIR *dirp);
struct direct *readdirver (DIR *dirp);
long telldir (DIR *dirp);
void seekdir (DIR *dirp, long loc);

#define rewinddir( dirp )	seekdir( dirp, 0L )

#endif /* WIN32_NATIVE */

#endif /* INCLUDED_ndir_h_ */
