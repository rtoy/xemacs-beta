/* Header for bdf.c
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

/* 92.10.14 written by K.Handa <handa@etl.go.jp> */
/* 92.11.3  modified for Mule Ver.0.9.7
                by T.Matsuzawa <mzw_t@hpujisa.yhp.co.jp>
        Support NONSPACING characters. */
/* 92.11.24 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Modified to reduce memory. */
/* 92.12.14 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Support for composite character. */
/* 93.5.7   modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
	Support right-to-left character.
	Definitions of BDF_xxxx are deleted. */

#ifndef _BDF_H
#define _BDF_H

/* Structure of glyph information of one character.  */
typedef struct {
  int dwidth;			/* width in pixels */
  int bbw, bbh, bbox, bboy;	/* bounding box in pixels */
  int bitmap_size;		/* byte lengh of the following slots */
  char *bitmap;			/*  */
} glyph_struct;

extern glyph_struct glyph;

typedef struct {
  char *filename;
  int bytes, encoding, direction; /* 93.5.7 by K.Handa */
  FILE *fp;
  int loaded;			/* flag */
  long last_offset;
  long *offset;			/* offset to STARTCHAR */
  int chars;			/* CHARS */
  int llx, lly, urx, ury;	/* FontBBox */
  /* Baseline offset -- value of private property _MULE_BASELINE_OFFSET */
  int yoffset;
  int relative_compose;
  char **extra;
} font_struct;

typedef struct {
  unsigned int *defined;	/* if 1, the character is defined. */
  int fs;			/* PIXEL size */
  int code[256];		/* encoding -> character code mapping */
  unsigned int count[256];	/* How often the character appeared. */
  unsigned char new[256];
} font_extra;

extern font_struct *font;

#define FONT_LOADED(lc) font[(lc) & 0x7F].loaded

#define DEFINED1(lc,idx) \
  (((font_extra *)(font[(lc) & 0x7F].extra))->defined[idx])
#define DEFINE1(lc,idx) \
  (((font_extra *)(font[(lc) & 0x7F].extra))->defined[idx] = 1)

#define DEFINED2(lc,idx) \
  (((font_extra *)(font[(lc) & 0x7F].extra))->defined[(idx) / 32] & (1 << ((idx) % 32)))
#define DEFINE2(lc,idx) \
  (((font_extra *)(font[(lc) & 0x7F].extra))->defined[(idx) / 32] |= (1 << ((idx) % 32)))
#define UNDEFINE2(lc,idx) \
  (((font_extra *)(font[(lc) & 0x7F].extra))->defined[(idx) / 32] &= ~(1 << ((idx) % 32)))

extern FILE *bdf_fopen();

#ifndef BDF_PATH
#define BDF_PATH "/usr/share/fonts/X11/ETL,/usr/share/fonts/X11/Chinese,/usr/share/fonts/X11/Japanese,/usr/share/fonts/X11/Korean"
#endif

#endif /* _BDF_H */
