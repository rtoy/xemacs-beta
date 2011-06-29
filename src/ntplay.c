/* Sound in windows nt XEmacs.
   Copyright (C) 1998 Andy Piper.
   Copyright (C) 2001 Ben Wing.

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

/* Synched up with: Not in FSF. */

/* This file Mule-ized by Ben Wing, 5-15-01. */

#include <config.h>
#include "lisp.h"

#include "sound.h"
#include "syswindows.h"

#include "sysfile.h"

static int play_sound_data_1 (Binbyte *data, int length,
			      int volume, int convert);

void
nt_play_sound_file (Lisp_Object path, int UNUSED (volume))
{
  DWORD flags = SND_ASYNC | SND_NODEFAULT | SND_FILENAME;
  Lisp_Object fname = Ffile_name_nondirectory (path);
  Extbyte *fnameext;

  fnameext = LISP_STRING_TO_TSTR (fname);

  if (qxeSearchPath (NULL, fnameext, NULL, 0, NULL, NULL) == 0)
    {
      /* file isn't in the path so read it as data */
      int size;
      Binbyte *data;
      int ofd = qxe_open (XSTRING_DATA (path), O_RDONLY | OPEN_BINARY, 0);
      
      if (ofd < 0)
	return;

      size = lseek (ofd, 0, SEEK_END);
      data = xnew_binbytes (size);
      lseek (ofd, 0, SEEK_SET);
      
      if (!data)
	{
	  retry_close (ofd);
	  return;
	}

      if (retry_read (ofd, data, size) != size)
	{
	  retry_close (ofd);
	  xfree (data);
	  return;
	}
      retry_close (ofd);
      
      play_sound_data_1 (data, size, 100, FALSE);
    }
  else 
    qxePlaySound (fnameext, NULL, flags);
}

/* mswindows can't cope with playing a sound from alloca space so we
   have to convert if necessary */
static int
play_sound_data_1 (Binbyte *data, int length, int UNUSED (volume),
		   int convert_to_malloc)
{
  DWORD flags = SND_ASYNC | SND_MEMORY | SND_NODEFAULT;
  static Binbyte *sound_data = 0;
  if (sound_data)
    {
      qxePlaySound (NULL, NULL, flags);
      xfree (sound_data);
      sound_data = 0;
    }

  if (convert_to_malloc)
    {
      sound_data = xnew_binbytes (length);
      memcpy (sound_data, data, length);
    }
  else
    sound_data = data;

  qxePlaySound ((Extbyte *) sound_data, NULL, flags);

  /* #### Error handling? */ 
  return 1;
}

int
play_sound_data (Binbyte *data, int length, int volume)
{
  return play_sound_data_1 (data, length, volume, TRUE);
}
