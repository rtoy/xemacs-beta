/* esd.c - play a sound over ESD

Copyright 1999 Robert Bihlmeyer <robbe@orcus.priv.at>

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

#include "miscplay.h"
#include "sound.h"

#include "sysfile.h"

#include <esd.h>


/* the name given to ESD - I think this should identify ourselves */
#define ESD_NAME "xemacs"

int esd_play_sound_file (Extbyte *file, int vol);
int
esd_play_sound_file (Extbyte *file, int UNUSED (vol))
{                              /* #### FIXME: vol is ignored */
  return esd_play_file(ESD_NAME, file, 0);
}

int esd_play_sound_data (Binbyte *data, size_t length, int vol);
int
esd_play_sound_data (Binbyte *data, size_t length, int UNUSED (vol))
{                              /* #### FIXME: vol is ignored */
  size_t         (*parsesndfile)(void **dayta,size_t *sz,void **outbuf);
  size_t         (*sndcnv)(void **dayta,size_t *sz,void **);
  fmtType        ffmt;
  int            fmt,speed,tracks;
  unsigned char *pptr,*optr,*cptr,*sptr;
  Bytecount      wrtn, crtn;
  size_t         prtn;
  int flags, sock;

  /* analyze_format needs at least this many bytes to work with */
  if (length < HEADERSZ)
    return 0;

  ffmt = analyze_format(data,&fmt,&speed,&tracks,&parsesndfile);

  if (ffmt != fmtRaw && ffmt != fmtSunAudio && ffmt != fmtWave) {
    sound_warn("Unsupported file format (neither RAW, nor Sun/DECAudio, nor WAVE)");
      return 0;
  }

  /* convert header information into ESD flags */
  flags = ESD_STREAM|ESD_PLAY;
  sndcnv = sndcnvnop;
  switch (fmt)
    {
    case AFMT_MU_LAW:
      sndcnv = sndcnvULaw_2linear;
      flags |= ESD_BITS8;
      break;
    case AFMT_S8:
      sndcnv = sndcnv2unsigned;        /* ESD needs unsigned bytes */
    case AFMT_U8:
      flags |= ESD_BITS8;
      break;
    case AFMT_S16_BE:
      sndcnv = sndcnv16swap;   /* ESD wants little endian */
    case AFMT_S16_LE:
      flags |= ESD_BITS16;
      break;
    default:
      {
	Extbyte warn_buf[255];
	sprintf (warn_buf, "byte format %d unimplemented", fmt);
	sound_warn (warn_buf);
	return 0;
      }
    }
  switch (tracks)
    {
    case 1: flags |= ESD_MONO; break;
    case 2: flags |= ESD_STEREO; break;
    default:
      {
	Extbyte warn_buf[255];
	sprintf (warn_buf, "%d channels - only 1 or 2 supported", tracks);
	sound_warn (warn_buf);
	return 0;
      }
    }

  sock = esd_play_stream(flags, speed, NULL, "xemacs");
  if (sock < 0)
    return 0;

  reset_parsestate();

  for (pptr = data; (prtn = parsesndfile((void **)&pptr,&length,
                                        (void **)&optr)) > 0; )
    for (cptr = optr; (crtn = sndcnv((void **)&cptr,&prtn,
                                    (void **)&sptr)) > 0; )
      {
	if ((wrtn = write(sock,sptr,crtn)) < 0)
	  {
	    sound_perror ("write error");
	    goto END_OF_PLAY;
	  }
	if (wrtn != crtn)
	  {
	    Extbyte warn_buf[255];
	    sprintf (warn_buf, "only wrote %ld of %ld bytes", wrtn, crtn);
	    sound_warn (warn_buf);
	    goto END_OF_PLAY;
	  }
      }

  if (ffmt == fmtWave)
    parse_wave_complete();

END_OF_PLAY:
  /* Now cleanup all used resources */

  close(sock);
  return 1;
}
