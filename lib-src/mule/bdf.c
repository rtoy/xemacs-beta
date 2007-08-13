/* bdfstaff Ver.2.2 -- BDF utilities
   Copyright (C) 1992 Free Software Foundation, Inc. */

/* This file is part of Mule (MULtilingual Enhancement of GNU Emacs).

Mule is free software distributed in the form of patches to GNU Emacs.
You can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

Mule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* 92.10.14  written by K.Handa <handa@etl.go.jp> */
/* 92.10.21  modified for Mule 0.9.6 with DOS support
		by M.Higashida <manabu@sigmath.osaka-u.ac.jp> */
/* 92.11.3  modified for Mule Ver.0.9.7
                by T.Matsuzawa <mzw_t@hpujisa.yhp.co.jp>
        Support NONSPACING characters and send smaller bitmap. */
/* 92.11.10 modified for Mule Ver.0.9.7
		by K.Sakaeda <saka@tomorose.trad.pfu.fujitsu.co.jp>
	Modified for SystemV. */
/* 92.11.24 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Modified to reduce memory. */
/* 92.12.15 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Look FONTPROPERTIES. */
/* 93.1.13  modified for Mule Ver.0.9.7.1
   			by T.Furuhata <furuhata@fujita3.iis.u-tokyo.ac.jp>
	Modified for AIX. */
/* 93.2.10  modified for Mule Ver.0.9.7.1 by K.Handa <handa@etl.go.jp>
	In bdf_fopen(), declaration of 'path' is changed. */
/* 93.3.15  modified for Mule Ver.0.9.7.1 by K.Handa <handa@etl.go.jp>
	For AIX, now we don't need "#pragma alloca". */
/* 93.5.7   modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
	In bdf_load_font(), bug fixed in handling private char-set. */
/* 93.5.7   modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
	In bdf_load_font(), use value of PIXEL_SIZE instead of BBH.
	Support right-to-left character.
	Support Big5. */
/* 93.7.19  modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
	Stop including "codeconv.h". */
/* 94.3.9   modified for Mule Ver.1.1 by Y.Niibe <gniibe@oz.etl.go.jp>
	In bdf_load_font(), fontp->extra->fs should not be set in emacs. */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/file.h>
#include "mulelib.h"
#ifdef USG
#include <string.h>
#else
#include <strings.h>
#endif

static int in_emacs;

char *bdf_path;

font_struct *font;
glyph_struct glyph;
static char *line, *dummy;
#ifndef LINE_BUF_SIZE
#define LINE_BUF_SIZE 256
#endif

bdf_reset_font(fontp)
     font_struct *fontp;
{
  int size1, size2;
  font_extra *ext;

  if (fontp->bytes == 1) size1 = size2 = 256;
  else size1 = 0x10000, size2 = size1 / 32;

  if (fontp->offset == NULL) {
    fontp->offset = (long *)malloc((sizeof (long)) * size1);
    bzero(fontp->offset, (sizeof (long)) * size1);
    if (in_emacs) {
      fontp->extra = (char **)malloc((sizeof (char *)) * size1);
      bzero(fontp->extra, (sizeof (char *)) * size1);
    } else {
      fontp->extra = (char **)malloc(sizeof *ext);
      ext = (font_extra *)(fontp->extra);
      ext->defined = (unsigned int *)malloc((sizeof (int)) * size2);
    }
  }
  if (!in_emacs) {
    bzero(ext->defined, (sizeof (int)) * size2);
    bzero(ext->code, (sizeof (int)) * 256);
    bzero(ext->count, (sizeof (int)) * 256);
    bzero(ext->new, (sizeof (char)) * 256);
  }
}

bdf_init_font()
{
  int i, lc;

  font = (font_struct *)malloc((sizeof (font_struct)) * 128);
  for (i = 0; i < 128; i++) {
    font[i].fp = NULL;
    font[i].loaded = 0;
    font[i].offset = NULL;
    font[i].bytes = char_type[i == 0 ? i : i + 128] < TYPE94N ? 1 : 2;
    font[i].filename = font_name[i];
    font[i].encoding = font_encoding[i];
    lc = (i == 0) ? i : i | 0x80;
  }
}

bdf_init_glyph(glyphp)
     glyph_struct *glyphp;
{
  glyphp->bitmap_size = 100;
  glyphp->bitmap = (char *)malloc(glyph.bitmap_size * (sizeof (char)));
}

bdf_initialize(bp, _emacs)
     char *bp;
     int _emacs;
{
  in_emacs = _emacs;
  line = (char *)malloc(LINE_BUF_SIZE);
  dummy = (char *)malloc(LINE_BUF_SIZE);
  bdf_path = (bp != NULL ? bp : getenv("BDFPATH"));
  if (bdf_path == NULL) bdf_path = BDF_PATH; 
  bdf_init_font();
  bdf_init_glyph(&glyph);
}

bdf_proceed_line(fp, str)
     FILE *fp;
     char *str;
{
  int len = strlen(str);
  do {
    if (fgets(line, LINE_BUF_SIZE, fp) == NULL)
      return 0;
  } while (strncmp(line, str, len));
  return 1;
}

bdf_proceed_line2(fp, str, stop)
     FILE *fp;
     char *str, *stop;
{
  int len1 = strlen(str), len2 = strlen(stop);
  do {
    if (fgets(line, LINE_BUF_SIZE, fp) == NULL
	|| !strncmp(line, stop, len2))
      return 0;
  } while (strncmp(line, str, len1));
  return 1;
}

bdf_load_font(lc)
     int lc;
{
  font_struct *fontp = &font[lc & 0x7F];
  int i, j, bbw, bbh, bbox, bboy;
  unsigned int idx;

  if (fontp->filename == NULL
      || (fontp->fp = open_file(bdf_path, fontp->filename)) == NULL) {
    if (lc == 0)
      fatal1("Font for ASCII not found.\n");
    warning2("Font(%d:%s) not found.  Substituted by ASCII font.\n",
	     lc, fontp->filename);
    *fontp = font[0];
    fontp->loaded = -1;
    return 0;
  }

  bdf_reset_font(fontp);

  bdf_proceed_line(fontp->fp, "FONTBOUNDINGBOX ");
  sscanf(line, "%s %d %d %d %d", dummy, &bbw, &bbh, &bbox, &bboy);
  fontp->llx = bbox, fontp->lly = bboy;
  fontp->urx = bbw + bbox, fontp->ury = bbh + bboy;
  fontp->yoffset = 0;
  fontp->relative_compose = 0;
  if (!in_emacs)
    ((font_extra *)fontp->extra)->fs = bbh;
  if (bdf_proceed_line2(fontp->fp, "STARTPROPERTIES ", "CHARS ")) {
    do {
      /* If there are properties of PIXEL_SIZE, FONT_ASCENT, FONT_DESCENT,
	 we believe them rather than FONTBOUNDINGBOX.
	 In addition, we also look private properties _MULE_BASELINE_OFFSET
	 and _MULE_RELATIVE_COMPOSE. */
      if (fgets(line, LINE_BUF_SIZE, fontp->fp) == NULL)
	return 0;
      if (!strncmp(line, "PIXEL_SIZE ", 11)) { /* 93.5.7 by K.Handa */
	if (!in_emacs) {	/* 94.3.9 by Y.Niibe */
	  sscanf(line, "%s %d", dummy, &i);
	  ((font_extra *)fontp->extra)->fs = i;
	}
      } else if (!strncmp(line, "FONT_ASCENT ", 12)) {
	sscanf(line, "%s %d", dummy, &fontp->ury);
      } else if (!strncmp(line, "FONT_DESCENT ", 13)) {
	sscanf(line, "%s %d", dummy, &i);
	fontp->lly = - i;
      } else if (!strncmp(line, "_MULE_BASELINE_OFFSET ", 22)) {
	sscanf(line, "%s %d", dummy, &i);
	fontp->yoffset = - i;
      } else if (!strncmp(line, "_MULE_RELATIVE_COMPOSE ", 23)) {
	sscanf(line, "%s %d", dummy, &fontp->relative_compose);
      }
    } while (strncmp(line, "ENDPROPERTIES", 13));
    bdf_proceed_line(fontp->fp, "CHARS ");
  }
  sscanf(line, "%s %d", dummy, &(fontp->chars));
  fontp->last_offset = ftell(fontp->fp);
  fontp->loaded = 1;
  if (fontp->bytes == 2) {
    fontp->yoffset
      = (((fontp->ury + fontp->lly) - (font[0].ury + font[0].lly - font[0].yoffset * 2)) / 2)
	* ((font_extra *)font[0].extra)->fs / ((font_extra *)fontp->extra)->fs;
    printf("%%yoffset of %d(%d) is %d\n", lc, ((font_extra *)font[0].extra)->fs, fontp->yoffset);

  }
  return 1;
}

bdf_load_glyph(lc, idx, glyph)
     int lc, idx;
     glyph_struct *glyph;
{
  font_struct *fontp = &font[lc & 0x7F];
  int i, j;
  int width, size;
  int h0, h1, k;

  if (fontp->fp == NULL) goto glyph_not_found;

  if (fontp->offset[idx]) {
    fseek(fontp->fp, fontp->offset[idx], 0);
  } else {
    fseek(fontp->fp, fontp->last_offset, 0);
    i = 0;
    while (fontp->chars-- > 0) {
      bdf_proceed_line(fontp->fp, "ENCODING ");
      sscanf(line, "%s %d", dummy, &i);
      fontp->offset[i] = fontp->last_offset = ftell(fontp->fp);
      if (i == idx) break;
    }
    if (i != idx) {
      if (fontp->offset[idx & 0x7F7F]) {
	fontp->offset[idx] = fontp->offset[idx & 0x7F7F];
	fseek(fontp->fp, fontp->offset[idx], 0);
      } else
	goto glyph_not_found;
    }
  }

  bdf_proceed_line(fontp->fp, "DWIDTH ");
  sscanf(line, "%s %d", dummy, &(glyph->dwidth));
  bdf_proceed_line(fontp->fp, "BBX ");
  sscanf(line, "%s %d %d %d %d",
	 dummy, &(glyph->bbw), &(glyph->bbh), &(glyph->bbox), &(glyph->bboy));
  bdf_proceed_line(fontp->fp, "BITMAP");
  width = ((glyph->bbw + 7) / 8) * 2;
  size = width * glyph->bbh + 1;
  if (glyph->bitmap_size < size)
    glyph->bitmap_size = size, glyph->bitmap = (char *)realloc(glyph->bitmap, size);

  h0 = h1 = -1;
  j = 0;
  for ( i = 0; i < glyph->bbh; i++ ) {
    if (!fgets(line, LINE_BUF_SIZE, fontp->fp)) goto glyph_not_found;
    line[width] = 0;
    for ( k = 0; k < width; k++ ) {
      if ( line[k] != '0' ) {
	if ( h0 < 0 ) {
	  h0 = i;
	}
	if ( h1 < i ) {
	  h1 = i;
	}
	break;
      }
    }
    if ( h0 < 0 ) {
      continue;
    }
    sprintf(glyph->bitmap + j, "%s", line);
    j += width;
  }
  glyph->bitmap[j] = 0;
  if ( h0 < 0 && h1 < 0 ) {
    h0 = 0;
  }
  glyph->bboy += ( glyph->bbh - h1 - 1) - fontp->yoffset;
  glyph->bbh = ( h1 - h0 + 1 );
  glyph->bitmap_size = width * glyph->bbh + 1;
  glyph->bitmap = (char *)realloc(glyph->bitmap, size );
  glyph->bitmap[glyph->bitmap_size - 1] = 0;
  return 1;

 glyph_not_found:
  warning3("Glyph of char(%d) for font(%d:%s) not found.\n",
	   idx, lc, fontp->filename);
  return 0;
}
