/* Routines shared between window-system backends for glyph objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996 Ben Wing
   Copyright (C) 1995 Sun Microsystems
   Copyright (C) 1998, 1999, 2000 Andy Piper.

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

/* Synched up with: Not in FSF. */

/* taken from glyphs-*.c
   HINT HINT HINT Bill Perry: Please put code here and avoid massive
   duplication in *-gtk.c!!! */

#include <config.h>
#include "lisp.h"
#include "lstream.h"

#include "window.h"
#include "elhash.h"
#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"
#include "sysdep.h"
#include "sysfile.h"
#include "faces.h"
#include "imgproc.h"

Lisp_Object Q_resource_type, Q_resource_id;

void
shared_resource_validate (Lisp_Object instantiator)
{
  if ((NILP (find_keyword_in_vector (instantiator, Q_file))
       &&
       NILP (find_keyword_in_vector (instantiator, Q_resource_id)))
      ||
      NILP (find_keyword_in_vector (instantiator, Q_resource_type)))
    sferror ("Must supply :file, :resource-id and :resource-type",
	     instantiator);
}


Lisp_Object
shared_resource_normalize (Lisp_Object inst, Lisp_Object console_type,
			   Lisp_Object dest_mask, Lisp_Object tag)
{
  /* This function can call lisp */
  Lisp_Object file = Qnil;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object alist = Qnil;

  GCPRO2 (file, alist);

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data,
					     console_type);

  if (CONSP (file)) /* failure locating filename */
    signal_double_image_error ("Opening pixmap file",
			       "no such file or directory",
			       Fcar (file));

  if (NILP (file)) /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  {
    alist = remassq_no_quit (Q_file, alist);
    alist = Fcons (Fcons (Q_file, file), alist);
  }

  {
    Lisp_Object result = alist_to_tagged_vector (tag, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

/* Originally from xmu.c, but is now shared across X11, GTK, and MSW. */
/*
 * Based on an optimized version provided by Jim Becker, August 5, 1988.
 */


#ifndef BitmapSuccess
#define BitmapSuccess           0
#define BitmapOpenFailed        1
#define BitmapFileInvalid       2
#define BitmapNoMemory          3
#endif

#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];		/* conversion value */
static int hex_initialized;	/* easier to fill in at run time */


/*
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 */
static void initHexTable (void)
{
    /*
     * We build the table at run time for several reasons:
     *
     *     1.  portable to non-ASCII machines.
     *     2.  still reentrant since we set the init flag after setting table.
     *     3.  easier to extend.
     *     4.  less prone to bugs.
     */
    hexTable['0'] = 0;	hexTable['1'] = 1;
    hexTable['2'] = 2;	hexTable['3'] = 3;
    hexTable['4'] = 4;	hexTable['5'] = 5;
    hexTable['6'] = 6;	hexTable['7'] = 7;
    hexTable['8'] = 8;	hexTable['9'] = 9;
    hexTable['A'] = 10;	hexTable['B'] = 11;
    hexTable['C'] = 12;	hexTable['D'] = 13;
    hexTable['E'] = 14;	hexTable['F'] = 15;
    hexTable['a'] = 10;	hexTable['b'] = 11;
    hexTable['c'] = 12;	hexTable['d'] = 13;
    hexTable['e'] = 14;	hexTable['f'] = 15;

    /* delimiters of significance are flagged w/ negative value */
    hexTable[' '] = -1;	hexTable[','] = -1;
    hexTable['}'] = -1;	hexTable['\n'] = -1;
    hexTable['\t'] = -1;

    hex_initialized = 1;
}

/*
 *	read next hex value in the input stream, return -1 if EOF
 */
static int NextInt (FILE *fstream)
{
    int	ch;
    int	value = 0;
    int gotone = 0;
    int done = 0;

    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

    while (!done) {
	ch = getc(fstream);
	if (ch == EOF) {
	    value	= -1;
	    done++;
	} else {
	    /* trim high bits, check type and accumulate */
	    ch &= 0xff;
	    if (isascii(ch) && isxdigit(ch)) {
		value = (value << 4) + hexTable[ch];
		gotone++;
	    } else if ((hexTable[ch]) < 0 && gotone)
	      done++;
	}
    }
    return value;
}


/*
 * The data returned by the following routine is always in left-most byte
 * first and left-most bit first.  If it doesn't return BitmapSuccess then
 * its arguments won't have been touched.  This routine should look as much
 * like the Xlib routine XReadBitmapfile as possible.
 */
static int
read_bitmap_data (FILE *fstream, int *width, int *height, UChar_Binary **datap,
		  int *x_hot, int *y_hot)
{
    UChar_Binary *data = NULL;		/* working variable */
    Char_ASCII line[MAX_SIZE];		/* input line from file */
    int size;				/* number of bytes of data */
    Char_ASCII name_and_type[MAX_SIZE];	/* an input line */
    Char_ASCII *type;			/* for parsing */
    int value;				/* from an input line */
    int version10p;			/* boolean, old format */
    int padding;			/* to handle alignment */
    int bytes_per_line;			/* per scanline of data */
    int ww = 0;				/* width */
    int hh = 0;				/* height */
    int hx = -1;			/* x hotspot */
    int hy = -1;			/* y hotspot */

#ifndef Xmalloc
#define Xmalloc(size) malloc(size)
#endif

    /* first time initialization */
    if (!hex_initialized) initHexTable();

    /* error cleanup and return macro	*/
#define	RETURN(code) { if (data) free (data); return code; }

    while (fgets(line, MAX_SIZE, fstream)) {
	if (strlen(line) == MAX_SIZE-1) {
	    RETURN (BitmapFileInvalid);
	}
	if (sscanf(line,"#define %s %d",name_and_type,&value) == 2) {
	    if (!(type = strrchr(name_and_type, '_')))
	      type = name_and_type;
	    else
	      type++;

	    if (!strcmp("width", type))
	      ww = value;
	    if (!strcmp("height", type))
	      hh = value;
	    if (!strcmp("hot", type)) {
		if (type-- == name_and_type || type-- == name_and_type)
		  continue;
		if (!strcmp("x_hot", type))
		  hx = value;
		if (!strcmp("y_hot", type))
		  hy = value;
	    }
	    continue;
	}

	if (sscanf(line, "static short %s = {", name_and_type) == 1)
	  version10p = 1;
	else if (sscanf(line,"static unsigned char %s = {",name_and_type) == 1)
	  version10p = 0;
	else if (sscanf(line, "static char %s = {", name_and_type) == 1)
	  version10p = 0;
	else
	  continue;

	if (!(type = strrchr(name_and_type, '_')))
	  type = name_and_type;
	else
	  type++;

	if (strcmp("bits[]", type))
	  continue;

	if (!ww || !hh)
	  RETURN (BitmapFileInvalid);

	if ((ww % 16) && ((ww % 16) < 9) && version10p)
	  padding = 1;
	else
	  padding = 0;

	bytes_per_line = (ww+7)/8 + padding;

	size = bytes_per_line * hh;
	data = (UChar_Binary *) Xmalloc ((unsigned int) size);
	if (!data)
	  RETURN (BitmapNoMemory);

	if (version10p) {
	    UChar_Binary *ptr;
	    int bytes;

	    for (bytes=0, ptr=data; bytes<size; (bytes += 2)) {
		if ((value = NextInt(fstream)) < 0)
		  RETURN (BitmapFileInvalid);
		*(ptr++) = value;
		if (!padding || ((bytes+2) % bytes_per_line))
		  *(ptr++) = value >> 8;
	    }
	} else {
	    UChar_Binary *ptr;
	    int bytes;

	    for (bytes=0, ptr=data; bytes<size; bytes++, ptr++) {
		if ((value = NextInt(fstream)) < 0)
		  RETURN (BitmapFileInvalid);
		*ptr=value;
	    }
	}
	break;
    }					/* end while */

    if (data == NULL) {
	RETURN (BitmapFileInvalid);
    }

    *datap = data;
    data = NULL;
    *width = ww;
    *height = hh;
    if (x_hot) *x_hot = hx;
    if (y_hot) *y_hot = hy;

    RETURN (BitmapSuccess);
}


int
read_bitmap_data_from_file (const char *filename,
			    /* Remaining args are RETURNED */
			    int *width,
			    int *height,
			    UChar_Binary **datap,
			    int *x_hot, int *y_hot)
{
    FILE *fstream;
    int status;

    if ((fstream = fopen (filename, "r")) == NULL)
      return BitmapOpenFailed;
    status = read_bitmap_data (fstream, width, height, datap, x_hot, y_hot);
    fclose (fstream);
    return status;
}

void
syms_of_glyphs_shared (void)
{
  DEFKEYWORD (Q_resource_id);
  DEFKEYWORD (Q_resource_type);
}
