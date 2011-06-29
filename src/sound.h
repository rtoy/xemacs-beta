/* Sound functions.
   Copyright (C) 2000 Free Software Foundation, Inc.
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

#include "buffer.h"

/* Defined in *play.c */
void play_sound_file (Extbyte *name, int volume);
void nt_play_sound_file (Lisp_Object name, int volume);
int play_sound_data (Binbyte *data, int length, int volume);

# define sound_perror(string)						 \
do {									 \
  Ibyte *errmess;							 \
  Ibyte *string_int;							 \
  GET_STRERROR (errmess, errno);					 \
  string_int = EXTERNAL_TO_ITEXT (string, Qerror_message_encoding);	 \
  warn_when_safe (Qsound, Qerror, "audio: %s, %s", string_int, errmess); \
} while (0)
# define sound_warn(string)					\
do {								\
  Ibyte *string_int;						\
  string_int = EXTERNAL_TO_ITEXT (GETTEXT (string), Qerror_message_encoding);		\
  warn_when_safe (Qsound, Qwarning, "audio: %s", string_int);	\
} while (0)
