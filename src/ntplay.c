/* Sound in windows nt XEmacs.
   Copyright (C) 1998 Andy Piper.

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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.*/

#include <windows.h>
#undef CONST
#include <config.h>

#ifdef __CYGWIN32__
extern BOOL WINAPI PlaySound(LPCSTR,HMODULE,DWORD);
#endif

void play_sound_file (char *sound_file, int volume);
void play_sound_file (char *sound_file, int volume)
{
  DWORD flags = SND_ASYNC | SND_NODEFAULT | SND_FILENAME;
  char* dst=0;
#ifdef __CYGWIN32__
  CYGWIN_WIN32_PATH(sound_file, dst);
  sound_file=dst;
#endif
  if (PlaySound(sound_file, NULL, flags)==FALSE)
    {
      perror(sound_file);
    }
  return;
}

/* Call "linux_play_data_or_file" with the appropriate parameters for
   playing pre-loaded data */
void play_sound_data (unsigned char *data, int length, int volume);
void play_sound_data (unsigned char *data, int length, int volume)
{
  DWORD flags = SND_ASYNC | SND_MEMORY | SND_NODEFAULT;
  if (PlaySound(data, NULL, flags)==FALSE)
    {
      perror("couldn't play sound file");
    }
  return;
}
