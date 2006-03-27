/* Play sound with the ALSA library
   Copyright (C) 2006 Jerry James.

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

/* TODO: Support asynchronous sound playing; see the NAS support in sound.c */

#include <config.h>
#include "lisp.h"
#include "sound.h"
#include "sysfile.h"

/* We can't just include <alsa/asoundlib.h> because it tries to redefine
 * several symbols defined by the previous header files.
 */
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/conf.h>
#include <alsa/global.h>
#include <alsa/pcm.h>
#include <alsa/error.h>
#include <alsa/hwdep.h>
#include <alsa/rawmidi.h>
#include <alsa/control.h>
#include <alsa/mixer.h>

struct mixer_state
{
  snd_mixer_t *mixer;
  snd_mixer_elem_t *vol_ctl;

  /* Which channels need the old volume restored */
  int reset_front_left;
  int reset_front_center;
  int reset_front_right;
  int reset_rear_left;
  int reset_rear_center;
  int reset_rear_right;
  int reset_side_left;
  int reset_side_right;
  int reset_woofer;

  /* Old volumes for the channels */
  long front_left_vol;
  long front_center_vol;
  long front_right_vol;
  long rear_left_vol;
  long rear_center_vol;
  long rear_right_vol;
  long side_left_vol;
  long side_right_vol;
  long woofer_vol;
};

/* Assemble a machine half-word in little-endian order */
#define HALF_LE(arr,start) (arr[start] + (arr[start + 1] << 8))

/* Assemble a machine word in little-endian order */
#define WORD_LE(arr,start) (arr[start] + (arr[start + 1] << 8) +	\
			    (arr[start + 2] << 16) + (arr[start + 3] << 24))

/* Assemble a machine word in big-endian order */
#define WORD_BE(arr,start) ((arr[start] << 24) + (arr[start + 1] << 16) + \
			    (arr[start + 2] << 8) + arr[start + 3])

/* This function was inspired by miscplay.c.
 * Examine sound data to determine its format.
 *
 * TODO: Detect other formats that ALSA can play, such as GSM and MPEG.
 */
static snd_pcm_format_t
analyze_format (const Binbyte *format, int *speed, int *tracks)
{
  if (!memcmp (format, "Creative Voice File\x1A\x1A\x00", 22) &&
      HALF_LE (format, 22) == ((0x1233 - HALF_LE (format, 24)) & 0xFFFF))
    {
      /* VOC */
      *speed = 8000;
      *tracks = 2;
      return SND_PCM_FORMAT_U8;
    }
  else if (!memcmp (format, "RIFF", 4) && !memcmp (format + 8, "WAVEfmt ", 8))
    {
      /* WAVE */
      *speed = WORD_LE (format, 24);
      *tracks = format[22];
      return format[32] / format[22] == 1
	? SND_PCM_FORMAT_U8
	: SND_PCM_FORMAT_S16_LE;
    }
  else if (!memcmp (format, ".snd", 4))
    {
      /* Sun/NeXT Audio (big endian) */
      if (WORD_BE (format, 4) < 24)
	{
	  *speed = 8000;
	  *tracks = 1;
	  return SND_PCM_FORMAT_MU_LAW;
	}
      *speed = WORD_BE (format, 16);
      *tracks = format[23];
      if (!memcmp (format + 12, "\000\000\000", 3))
	{
	  switch (format[15])
	    {
	    case 1:
	    case 17:
	    case 29:
	      return SND_PCM_FORMAT_MU_LAW;
	    case 2:
	      return SND_PCM_FORMAT_S8;
	    case 3:
	      return SND_PCM_FORMAT_S16_BE;
	    case 4:
	      return SND_PCM_FORMAT_S24_BE;
	    case 5:
	      return SND_PCM_FORMAT_S32_BE;
	    case 23:
	    case 24:
	    case 25:
	    case 26:
	      return SND_PCM_FORMAT_IMA_ADPCM;
	    case 27:
	      return SND_PCM_FORMAT_A_LAW;
	    default:
	      break;
	    }
	}
      return SND_PCM_FORMAT_UNKNOWN;
    }
  else if (!memcmp (format, ".sd", 4))
    {
      /* DEC Audio (little endian) */
      if (WORD_LE (format, 4) < 24)
	{
	  *speed = 8000;
	  *tracks = 1;
	  return SND_PCM_FORMAT_MU_LAW;
	}
      *speed = WORD_LE (format, 16);
      *tracks = format[20];
      if (!memcmp (format + 13, "\000\000\000", 3))
	{
	  switch (format[12])
	    {
	    case 1:
	    case 17:
	    case 29:
	      return SND_PCM_FORMAT_MU_LAW;
	    case 2:
	      return SND_PCM_FORMAT_S8;
	    case 3:
	      return SND_PCM_FORMAT_S16_LE;
	    case 4:
	      return SND_PCM_FORMAT_S24_LE;
	    case 5:
	      return SND_PCM_FORMAT_S32_LE;
	    case 23:
	    case 24:
	    case 25:
	    case 26:
	      return SND_PCM_FORMAT_IMA_ADPCM;
	    case 27:
	      return SND_PCM_FORMAT_A_LAW;
	    default:
	      break;
	    }
	}
      return SND_PCM_FORMAT_UNKNOWN;
    }
  else
    {
      /* We don't know what it is.  Guess that it is mono audio in unsigned
       * byte format.  Maybe we should error if we reach this point.
       */
      *speed = 8000;
      *tracks = 1;
      return SND_PCM_FORMAT_U8;
    }
}

/* Set the volume: if any errors occur, we accept the existing volume */
static void
set_volume (struct mixer_state *mix, int volume)
{
  snd_mixer_selem_id_t *volume_id;
  long min_vol, max_vol, dev_vol;

  if (snd_mixer_open (&mix->mixer, 0) < 0)
    return;

  if (snd_mixer_attach (mix->mixer, "default") < 0)
    return;

  if (snd_mixer_selem_register (mix->mixer, NULL, NULL) < 0)
    return;

  if (snd_mixer_load (mix->mixer) < 0)
    return;

  snd_mixer_selem_id_alloca (&volume_id);
  snd_mixer_selem_id_set_name (volume_id, "PCM");

  if ((mix->vol_ctl = snd_mixer_find_selem (mix->mixer, volume_id)) == NULL)
    {
      snd_mixer_selem_id_set_name (volume_id, "Master");
      if ((mix->vol_ctl = snd_mixer_find_selem (mix->mixer, volume_id))
	  == NULL)
	return;
    }

  /* Translate the Lisp volume range to the device volume range */
  if (snd_mixer_selem_get_playback_volume_range (mix->vol_ctl, &min_vol,
						 &max_vol) < 0)
    return;

  dev_vol = volume * (max_vol - min_vol) / 100 + min_vol;

  /* Record the old volumes */
  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_LEFT, &mix->front_left_vol) >= 0)
    mix->reset_front_left = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_CENTER, &mix->front_center_vol) >= 0)
    mix->reset_front_center = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_RIGHT, &mix->front_right_vol) >= 0)
    mix->reset_front_right = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_LEFT, &mix->rear_left_vol) >= 0)
    mix->reset_rear_left = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_CENTER, &mix->rear_center_vol) >= 0)
    mix->reset_rear_center = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_RIGHT, &mix->rear_right_vol) >= 0)
    mix->reset_rear_right = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_SIDE_LEFT, &mix->side_left_vol) >= 0)
    mix->reset_side_left = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_SIDE_RIGHT, &mix->side_right_vol) >= 0)
    mix->reset_side_right = 1;

  if (snd_mixer_selem_get_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_WOOFER, &mix->woofer_vol) >= 0)
    mix->reset_woofer = 1;

  /* Set the volume */
  snd_mixer_selem_set_playback_volume_all (mix->vol_ctl, dev_vol);
}

static void
reset_volume (const struct mixer_state *mix)
{
  if (mix->reset_front_left)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_LEFT, mix->front_left_vol);

  if (mix->reset_front_center)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_CENTER, mix->front_center_vol);

  if (mix->reset_front_right)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_FRONT_RIGHT, mix->front_right_vol);

  if (mix->reset_rear_left)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_LEFT, mix->rear_left_vol);

  if (mix->reset_rear_center)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_CENTER, mix->rear_center_vol);

  if (mix->reset_rear_right)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_REAR_RIGHT, mix->rear_right_vol);

  if (mix->reset_side_left)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_SIDE_LEFT, mix->side_left_vol);

  if (mix->reset_side_right)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_SIDE_RIGHT, mix->side_right_vol);

  if (mix->reset_woofer)
    snd_mixer_selem_set_playback_volume
      (mix->vol_ctl, SND_MIXER_SCHN_WOOFER, mix->woofer_vol);

  snd_mixer_close (mix->mixer);
}

int
alsa_play_sound_data (const Binbyte *data, int length, int volume)
{
  snd_pcm_t *pcm_handle;
  snd_pcm_hw_params_t *hwparams;
  snd_pcm_format_t format;
  struct mixer_state mix;
  int speed, tracks, err;

  /* Set the PCM parameters */
  if ((err = snd_pcm_open (&pcm_handle, "default", SND_PCM_STREAM_PLAYBACK,
			   0)) < 0)
    goto error_pcm_open;

  snd_pcm_hw_params_alloca (&hwparams);

  if ((err = snd_pcm_hw_params_any (pcm_handle, hwparams)) < 0)
    goto error_pcm;

  format = analyze_format (data, &speed, &tracks);

  if ((err = snd_pcm_hw_params_set_access (pcm_handle, hwparams,
					   SND_PCM_ACCESS_RW_INTERLEAVED)) < 0)
    goto error_pcm;

  if ((err = snd_pcm_hw_params_set_format (pcm_handle, hwparams, format)) < 0)
    goto error_pcm;

  if ((err = snd_pcm_hw_params_set_rate (pcm_handle, hwparams, speed, 0)) < 0)
    goto error_pcm;

  if ((err = snd_pcm_hw_params_set_channels (pcm_handle, hwparams, tracks))
      < 0)
    goto error_pcm;

  if ((err = snd_pcm_hw_params (pcm_handle, hwparams)) < 0)
    goto error_pcm;

  /* Set the volume */
  memset (&mix, 0, sizeof (mix));
  set_volume (&mix, volume);
  
  /* Play the sound */
  if ((err = snd_pcm_writei (pcm_handle, data, length)) < 0)
    goto error_mixer;

  /* Put the volume back the way it used to be */
  reset_volume (&mix);

  /* Release resources */
  snd_pcm_close (pcm_handle);
  return 1;

 error_mixer:
  reset_volume (&mix);
 error_pcm:
  snd_pcm_close (pcm_handle);
 error_pcm_open:
  sound_perror (snd_strerror (err));
  return 0;
}

/* Read the sound file into an internal buffer, then call
 * alsa_play_sound_data.
 */
int
alsa_play_sound_file (const Extbyte *sound_file, int volume)
{
  Binbyte *data;
  int fd, retval;
  struct stat st;

  fd = retry_open (sound_file, O_RDONLY, 0);
  if (fd < 0) {
    sound_perror (sound_file);
    return 0;
  }

  qxe_fstat (fd, &st);
  data = xnew_array (Binbyte, st.st_size);
  retry_read (fd, data, st.st_size);
  retry_close (fd);
  retval = alsa_play_sound_data (data, st.st_size, volume);
  xfree (data, Binbyte);
  return retval;
}
