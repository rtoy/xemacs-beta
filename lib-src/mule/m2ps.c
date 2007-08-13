/* m2ps Ver.2.2 -- Convert multilingual text to PostScript
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

/* 92.10.8  written by K.Handa <handa@etl.go.jp> */
/* 92.11.3  modified for Mule Ver.0.9.7
                by T.Matsuzawa <mzw_t@hpujisa.yhp.co.jp>
        Support NONSPACING characters and send smaller bitmap. */
/* 92.11.6  modified for Mule Ver.0.9.7 by K.Shibata <shibata@sgi.co.jp>
	Modified for ANSI-C. */
/* 92.11.10 modified for Mule Ver.0.9.7
		by K.Sakaeda <saka@tomorose.trad.pfu.fujitsu.co.jp>
	Modified for SystemV. */
/* 92.11.24 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Modified to reduce memory. */
/* 92.12.14 modified for Mule Ver.0.9.7 by K.Handa <handa@etl.go.jp>
	Support composite character. */
/* 93.5.7   modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
	Bug in handling missing font fixed.
	Support Big5. */
/* 93.5.29  modified for Mule Ver.0.9.8 by Y.Niibe <gniibe@mri.co.jp>
	Include config.h in src directory */  
/* 93.6.4   modified for Mule Ver.0.9.8 by K.Shibata <shibata@sgi.co.jp>
	Modified for ANSI-C. */
/* 93.12.25 modified for Mule Ver.1.0 by K.Handa <handa@etl.go.jp>
	In textmode(), bug of right-to-left handling fixed  */
/* 94.3.8   modified for Mule Ver.1.1 by K.Handa <handa@etl.go.jp>
	Bug in find_encoding() fixed. */
/* See `mule/ChangeLog.Mule' for the recent change log */

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include "mulelib.h"

static char *m2ps_version = "2.2";

#ifndef PSHeader
#define PSHeader "m2ps.ps"
#endif

#ifndef DPI
#define DPI 300
#endif

#ifndef FontScale
#define FontScale 10
#endif

#ifndef LinePitch
#define LinePitch 14
#endif

/* Parameters for A4 paper */
#define A4_MaxLine 56
#define A4_TopMargin 800

/* Parameters for US-LETTER paper */
#define US_MaxLine 50
#define US_TopMargin 746

#ifndef USLETTER

#ifndef MaxLine
#define MaxLine A4_MaxLine
#endif

#ifndef TopMargin
#define TopMargin A4_TopMargin
#endif

#else  /* US-LETTER */

#ifndef MaxLine
#define MaxLine US_MaxLine
#endif

#ifndef TopMargin
#define TopMargin US_TopMargin
#endif

#endif /* US-LETTER */

#ifndef LeftMargin
#define LeftMargin 30	/* To widen left margin, specify larger value. */
#endif

#ifndef ShortMemory
#define ShortMemory 0
#endif

char *psheader = PSHeader;
int dpi = DPI;
int fontscale = FontScale;
int linepitch = LinePitch;
int maxline = MaxLine;
int topmargin = TopMargin;
int leftmargin = LeftMargin;
int shortmemory = ShortMemory;

/***************/
/* Main staffs */
/***************/

/* GLOBAL VARIABLES */
int clm, row, current_lc;

set_font(lc)
     int lc;
{				/* 93.5.7, 94.11.29 by K.Handa -- Big change */
  if (FONT_LOADED (lc) == 0) {
    if (bdf_load_font(lc))
      ps_newfont(lc);
    else
      lc = 0;
  } else if (FONT_LOADED (lc) < 0)
    lc = 0;
  if (lc != current_lc) {
    ps_setfont(lc);
    current_lc = lc;
  }
}

renew_font(lc)
     int lc;
{
  bzero(((font_extra *)font[lc & 0x7F].extra)->new, 256 * (sizeof (char)));
}

/* Load specified glyph, then return the index to glyph.  Note the index
   is within the range of [0,255].  Return -1 if error. */
set_glyph1(lc, c)
     int lc, c;
{
  if (!DEFINED1(lc, c)) {
    if (bdf_load_glyph(lc, c, &glyph)) {
      ps_newglyph(c, &glyph);
      DEFINE1(lc, c);
    } else
      return -1;
  }
  return c;
}

find_encoding(fontp, lc, c)
     font_struct *fontp;
     int lc, c;
{
  font_extra *ext = (font_extra *)fontp->extra;
  int i, min_count = ext->count[0], j = 0;

  if (min_count > 0) {
    min_count = 0x7FFF;		/* 94.3.8 by K.Handa */
    for (i = 0; i < 256; i++) {
      if (ext->count[i] <= min_count && !ext->new[i]) {
	min_count = ext->count[i], j = i;
	if (min_count == 0) break;
      }
    }
  }
  if (min_count > 0)
    UNDEFINE2(lc, ext->code[j]);

  ext->count[j] = ext->new[j] = 1;
  ext->code[j] = c;
  DEFINE2(lc, ext->code[j]);
  return j;
}

/* Load specified glyph, replacing previously loaded glyph if necessary,
   then return the index to glyph.  Note the index is within the range
   of [0,255].  Return -1 if error. */
set_glyph2(lc, c)
     int lc, c;
{
  int code;
  font_struct *fontp = &font[lc & 0x7F];
  font_extra *ext = (font_extra *)fontp->extra;

  if (!DEFINED2(lc, c)) {
    if (bdf_load_glyph(lc, c, &glyph)) {
      code = find_encoding(fontp, lc, c);
      ps_newglyph(code, &glyph);
    } else
      return -1;
  } else {
    for (code = 0; code < 256; code++) {
      if (ext->code[code] == c) break;
    }
    ext->new[code] = 1;
    ext->count[code]++;
  }
  return code;
}

swap_buf(buf, from, to)
     unsigned char *buf;
     int from, to;
{
  int i, j;
  unsigned char buf2[1024], *p = buf2;

  i = j = to;
  while (--i >= from) {
    if (NONASCII_P(buf[i])) continue;
    bcopy(buf + i, p, j - i);
    p += j - i;
    j = i;
  }
  bcopy(buf2, buf + from, to - from);
}

textmode()
{
  register int i, j, k, c, lc;
  char buf[1024];		/* 92.11.6 by K.Shibata */
  unsigned char line[1024], *p;
  int cmp_flag = 0, len, r2l_chars;

  ps_bot(); ps_bop();
  clm = row = 0; current_lc = -1;
  /* set_font(0); */
  while ((len = get_line(line, 1024, stdin)) >= 0) { /* 93.6.4 by K.Shibata */
    /* We should process line by line to handle r2l direction. */
    j = 0; r2l_chars = 0;

    /* At first, re-order characters of r2l direction. */
    while (j < len) {
      p = line + j;
      if (*p <= 0x7F) {
	lc = 0;
	p++;
      } else {
	lc = *p++;
	if (lc == LCCMP) lc = *p++ - 0x20;
	if (lc >= LCPRV11) lc = *p++;
      }
      while (NONASCII_P(*p)) p++;
      if (char_direction[lc]) {
	if (!r2l_chars) k = j;
	r2l_chars++;
      } else {
	if (r2l_chars > 1) 
	  swap_buf(line, k, j);
	r2l_chars = 0;
      }
      j = p - line;
    }
    if (r2l_chars > 1) swap_buf(line, k, j); /* 93.12.25 by K.Handa */

    /* Then output characters. */
    for (j = 0; j < len;) {
      c = line[j];

      if (c == LCCMP) {		/* start of composite characters */
	cmp_flag = 1;
	j++;
	continue;
      }
      if (cmp_flag) {		/* composed characters have special encoding */
	if (c == 0xA0) c = line[++j] & 0x7F;
	else if (c > 0xA0) c -= 0x20;
	else cmp_flag = 0;
      }

      if (c < 0x20 || c == 0x7F) { /* Control code */
	switch (c) {
	case '\t':
	  i = 8 - (clm % 8);
	  clm += i;
	  set_font(0);
	  if (set_glyph1(0, ' ') >= 0) {
	    putchar('(');
	    while (i--) putchar(' ');
	    printf(") s ");
	  }
	  break;
	case '\f': 
	  ps_eop(), ps_bop();
	  break;
	default:
	  if (c < ' ') control_char(c);
	  else invalid_char(c);
	}
	j++;
      } else if (c < 0x7F) {	/* ASCII */
	set_font(0);
	i = 0;
	do {
	  if ((c = set_glyph1(0, c)) >= 0) {
	    if (c == '\\' || c == '(' || c == ')')
	      buf[i++] = '\\', buf[i++] = c;
	    else
	      buf[i++] = c;
	    clm++;
	  }
	  c = line[++j];
	} while (!cmp_flag	/* process composite character one by one */
		 && 0x20 <= c && c < 0x7F);
	buf[i] = 0;
	switch (cmp_flag) {
	case 0:			/* not composite character */
	  printf("(%s) s ", buf);
	  break;
	case 1:			/* first of composite character */
	  printf("(%s) cs1 ", buf);
	  cmp_flag = 2;
	  break;
	default:		/* case 2: rest of composite character*/
	  if (font[0].relative_compose)
	    printf("(%s) cs2 ", buf);
	  else
	    printf("(%s) cs3 ", buf);
	}
      } else if (c < LCINV) {	/* Multilingual character */
	int cur_lc = c < LCPRV11 ? c : line[j + 1];
	CCL_PROGRAM *ccl = x_ccl_programs[cur_lc & 0x7F];
	unsigned char c1, c2;

	set_font(cur_lc);
	i = 0, lc = cur_lc;
	while (1) {
	  if (lc >= LCPRV11) lc = line[j + 1];
	  if (lc != cur_lc)
	    break;
	  if (lc < LCJPOLD) c1 = 0, c2 = line[j + 1], j += 2;
	  else if (lc < LCPRV11) c1 = line[j + 1], c2 = line[j + 2], j += 3;
	  else if (lc < LCPRV21EXT) c1 = 0, c2 = line[j + 2], j += 3;
	  else  c1 = line[j + 2], c2 = line[j + 3], j += 4;
	  if (!ccl) {
	    c = c1 * 256 + c2;
	  } else {
	    ccl->reg[0] = c1; ccl->reg[1] = c2;
	    ccl_driver (ccl, NULL, NULL, 0, 0);
	    c = ccl->reg[0] * 256 + ccl->reg[1];
	  }
	  if (current_lc != lc) {
	    /* Font is not found.  Use ASCII to write the code. */
	    c &= 0x7F;
	    if (c >= ' ' && c <= 126)
	      c = set_glyph1(0, c & 0x7F);
	    else
	      c = set_glyph1(0, ' ');
	  } else if (char_type[lc] < TYPE94N)
	    c = set_glyph1(lc, c);
	  else
	    c = set_glyph2(lc, c);
	  if (c >= 0) {
	    if (c == '\\' || c == '(' || c == ')')
	      buf[i++] = '\\', buf[i++] = c;
	    else if (c >= ' ' && c < 127)
	      buf[i++] = c;
	    else
	      sprintf(buf + i, "\\%03o", c), i += 4;
	    clm += lc < 0x90 || (lc >= LCPRV11EXT && lc < LCPRV21EXT) ? 1 : 2;
	  }
	  if (cmp_flag) break;
	  lc = line[j];
	}
	buf[i] = 0;
	switch (cmp_flag) {
	case 0:
	  printf("(%s) s ", buf);
	  break;
	case 1:
	  printf("(%s) cs1 ", buf);
	  cmp_flag = 2;
	  break;
	case 2:
	default:
	  if (font[lc & 0x7F].relative_compose)
	    printf("(%s) cs2 ", buf);
	  else
	    printf("(%s) cs3 ", buf);
	}
	renew_font(cur_lc);
      } else {
	invalid_char(c);
      }
    }
    printf("n\n");
    row++, clm = 0;
    if (row >= maxline) ps_eop(), ps_bop();
  }

  ps_eop();
  ps_eot();
}

control_char(c)
     int c;
{
  c += '@';
  set_font(0);
  if ((set_glyph1(0, '^') >= 0) && (set_glyph1(0, c) >= 0)) {
    printf("(^%c) s ", c);
    clm += 2;
  }
}    

invalid_char(c)
     int c;
{
  int i;

  set_font(0);
  if (set_glyph1(0, '\\') >= 0) {
    for (i = '0'; i <= '9'; i++) {
      if (set_glyph1(0, i) < 0) break;
    }
    if (i > '9') {
      printf("(\\\\%03o) s ", c);
      clm += 4;
    }
  }
}

main(argc, argv)
     int argc;
     char *argv[];
{
  register int i = 1;
  char *bdf_path = NULL, *charsets = NULL;

  while (i < argc) {
    if (!strcmp(argv[i], "-ps") && (i + 1) < argc) {
      psheader = argv[i + 1];
      i += 2;
    } else if (!strcmp(argv[i], "-fp") && (i + 1) < argc) {
      bdf_path = argv[i + 1];
      i += 2;
    } else if (!strcmp(argv[i], "-cs") && (i + 1) < argc) {
      charsets = argv[i + 1];
      i += 2;
    } else if (!strcmp(argv[i], "-dpi") && (i + 1) < argc) {
      dpi = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-fs") && (i + 1) < argc) {
      fontscale = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-lp") && (i + 1) < argc) {
      linepitch = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-ml") && (i + 1) < argc) {
      maxline = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-tm") && (i + 1) < argc) {
      topmargin = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-lm") && (i + 1) < argc) {
      leftmargin = atoi(argv[i + 1]);
      i += 2;
    } else if (!strcmp(argv[i], "-sm")) {
      shortmemory = 1;
      i++;
    } else if (!strcmp(argv[i], "-bm")) {
      shortmemory = 0;
      i++;
    } else if (!strcmp(argv[i], "-a4") || !strcmp(argv[i], "-A4")) {
      maxline = A4_MaxLine;
      topmargin = A4_TopMargin;
      i++;
    } else if (!strcmp(argv[i], "-us") || !strcmp(argv[i], "-US")) {
      maxline = US_MaxLine;
      topmargin = US_TopMargin;
      i++;
    } else if (!strncmp(argv[i], "-ver", 4)) {
      printf("m2ps Ver.%s\n", m2ps_version);
      exit(0);
    } else {
      fprintf(stderr, "%s: Invalid argument: %s\n", argv[0], argv[i]);
      exit(1);
    }
  }

  mulelib_initialize(argc, argv, charsets, NULL);
  bdf_initialize(bdf_path, 0);
  textmode();
  exit(0);
}

/*********************/
/* PostScript staffs */
/*********************/

ps_bot()
{
  int c;
  FILE *fp;

  if ((fp = open_file(PATH_DATA, psheader)) == NULL)
    fatal1("PostScript header file not found.");

  while ((c = getc(fp)) != EOF) putchar(c);
  fclose(fp);
  printf("Mydict begin\n");
  printf("/DPI %d def\n", dpi);
  printf("/FontScale %d def\n", fontscale);
  printf("/LinePitch %d def\n", linepitch);
  printf("/TopMargin %d def\n", topmargin);
  printf("/LeftMargin %d def\n", leftmargin);
  printf("/ShortMemory %s def\n", (shortmemory ? "true" : "false"));
}

ps_eot()
{
  printf("end\n");
}

/* Define new PS font for a leading char LC.
   No_cache flag is for the fonts be modified (replacing the glyphs, etc.)
   at execution time. */
ps_newfont(lc)
     int lc;
{
  font_struct *fontp = &font[lc & 0x7F];

  printf("/F%02x /FF%02x %d [%d %d %d %d] %d %s nf\n",
	 lc, lc, ((font_extra *)fontp->extra)->fs,
	 fontp->llx, fontp->lly - fontp->yoffset,
	 fontp->urx, fontp->ury - fontp->yoffset,
	 fontp->relative_compose,
	 (fontp->bytes == 1 ? "true" : "false"));
}

ps_setfont(lc)
     int lc;
{
  printf("F%02x f\n", lc);
}

ps_newglyph(encoding, glyph)
     int encoding;
     glyph_struct *glyph;
{
  char *bitmap = glyph->bitmap;

  if (*bitmap == '\0')
    bitmap = "00";

  printf("/C%d%s[ %d %d %d %d %d %d %d %d %d <%s> ] g\n",
	 encoding, (encoding < 10 ? "XX " : encoding < 100 ? "X " : " "),
	 glyph->dwidth,
	 glyph->bbox, glyph->bboy,
	 glyph->bbw + glyph->bbox, glyph->bbh + glyph->bboy,
	 glyph->bbw, glyph->bbh,
	 glyph->bbox, glyph->bbh + glyph->bboy,
	 bitmap);
}

ps_bop()
{
  int lc;

  row = clm = 0;
  printf("bp\n");
  if (shortmemory) {
    current_lc = -1;
    for (lc = 0; lc < 256; lc++) { /* 93.5.7 by K.Handa */
      if (FONT_LOADED (lc) == 1) FONT_LOADED (lc) = 0;
    }
  }
}

ps_eop()
{
  printf("ep\n");
}
