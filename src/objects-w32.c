/* win32-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1997 Jonathan Harris.

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

/* Authorship:

   Jamie Zawinski, Chuck Thompson, Ben Wing
   Rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
 */


/* TODO: palette handling */

#include <config.h>
#include "lisp.h"

#include "console-w32.h"
#include "objects-w32.h"

#ifdef MULE
#include "mule-charset.h"
#endif

#include "buffer.h"
#include "device.h"
#include "insdel.h"

#include "windows.h"

typedef struct colormap_t 
{
  char *name;
  COLORREF colorref;
} colormap_t;

static colormap_t w32_X_color_map[] = 
{
  {"snow"			, PALETTERGB (255,250,250)},
  {"ghost white"		, PALETTERGB (248,248,255)},
  {"GhostWhite"			, PALETTERGB (248,248,255)},
  {"white smoke"		, PALETTERGB (245,245,245)},
  {"WhiteSmoke"			, PALETTERGB (245,245,245)},
  {"gainsboro"			, PALETTERGB (220,220,220)},
  {"floral white"		, PALETTERGB (255,250,240)},
  {"FloralWhite"		, PALETTERGB (255,250,240)},
  {"old lace"			, PALETTERGB (253,245,230)},
  {"OldLace"			, PALETTERGB (253,245,230)},
  {"linen"			, PALETTERGB (250,240,230)},
  {"antique white"		, PALETTERGB (250,235,215)},
  {"AntiqueWhite"		, PALETTERGB (250,235,215)},
  {"papaya whip"		, PALETTERGB (255,239,213)},
  {"PapayaWhip"			, PALETTERGB (255,239,213)},
  {"blanched almond"		, PALETTERGB (255,235,205)},
  {"BlanchedAlmond"		, PALETTERGB (255,235,205)},
  {"bisque"			, PALETTERGB (255,228,196)},
  {"peach puff"			, PALETTERGB (255,218,185)},
  {"PeachPuff"			, PALETTERGB (255,218,185)},
  {"navajo white"		, PALETTERGB (255,222,173)},
  {"NavajoWhite"		, PALETTERGB (255,222,173)},
  {"moccasin"			, PALETTERGB (255,228,181)},
  {"cornsilk"			, PALETTERGB (255,248,220)},
  {"ivory"			, PALETTERGB (255,255,240)},
  {"lemon chiffon"		, PALETTERGB (255,250,205)},
  {"LemonChiffon"		, PALETTERGB (255,250,205)},
  {"seashell"			, PALETTERGB (255,245,238)},
  {"honeydew"			, PALETTERGB (240,255,240)},
  {"mint cream"			, PALETTERGB (245,255,250)},
  {"MintCream"			, PALETTERGB (245,255,250)},
  {"azure"			, PALETTERGB (240,255,255)},
  {"alice blue"			, PALETTERGB (240,248,255)},
  {"AliceBlue"			, PALETTERGB (240,248,255)},
  {"lavender"			, PALETTERGB (230,230,250)},
  {"lavender blush"		, PALETTERGB (255,240,245)},
  {"LavenderBlush"		, PALETTERGB (255,240,245)},
  {"misty rose"			, PALETTERGB (255,228,225)},
  {"MistyRose"			, PALETTERGB (255,228,225)},
  {"white"			, PALETTERGB (255,255,255)},
  {"black"			, PALETTERGB (	0,  0,	0)},
  {"dark slate gray"		, PALETTERGB ( 47, 79, 79)},
  {"DarkSlateGray"		, PALETTERGB ( 47, 79, 79)},
  {"dark slate grey"		, PALETTERGB ( 47, 79, 79)},
  {"DarkSlateGrey"		, PALETTERGB ( 47, 79, 79)},
  {"dim gray"			, PALETTERGB (105,105,105)},
  {"DimGray"			, PALETTERGB (105,105,105)},
  {"dim grey"			, PALETTERGB (105,105,105)},
  {"DimGrey"			, PALETTERGB (105,105,105)},
  {"slate gray"			, PALETTERGB (112,128,144)},
  {"SlateGray"			, PALETTERGB (112,128,144)},
  {"slate grey"			, PALETTERGB (112,128,144)},
  {"SlateGrey"			, PALETTERGB (112,128,144)},
  {"light slate gray"		, PALETTERGB (119,136,153)},
  {"LightSlateGray"		, PALETTERGB (119,136,153)},
  {"light slate grey"		, PALETTERGB (119,136,153)},
  {"LightSlateGrey"		, PALETTERGB (119,136,153)},
  {"gray"			, PALETTERGB (190,190,190)},
  {"grey"			, PALETTERGB (190,190,190)},
  {"light grey"			, PALETTERGB (211,211,211)},
  {"LightGrey"			, PALETTERGB (211,211,211)},
  {"light gray"			, PALETTERGB (211,211,211)},
  {"LightGray"			, PALETTERGB (211,211,211)},
  {"midnight blue"		, PALETTERGB ( 25, 25,112)},
  {"MidnightBlue"		, PALETTERGB ( 25, 25,112)},
  {"navy"			, PALETTERGB (	0,  0,128)},
  {"navy blue"			, PALETTERGB (	0,  0,128)},
  {"NavyBlue"			, PALETTERGB (	0,  0,128)},
  {"cornflower blue"		, PALETTERGB (100,149,237)},
  {"CornflowerBlue"		, PALETTERGB (100,149,237)},
  {"dark slate blue"		, PALETTERGB ( 72, 61,139)},
  {"DarkSlateBlue"		, PALETTERGB ( 72, 61,139)},
  {"slate blue"			, PALETTERGB (106, 90,205)},
  {"SlateBlue"			, PALETTERGB (106, 90,205)},
  {"medium slate blue"		, PALETTERGB (123,104,238)},
  {"MediumSlateBlue"		, PALETTERGB (123,104,238)},
  {"light slate blue"		, PALETTERGB (132,112,255)},
  {"LightSlateBlue"		, PALETTERGB (132,112,255)},
  {"medium blue"		, PALETTERGB (	0,  0,205)},
  {"MediumBlue"			, PALETTERGB (	0,  0,205)},
  {"royal blue"			, PALETTERGB ( 65,105,225)},
  {"RoyalBlue"			, PALETTERGB ( 65,105,225)},
  {"blue"			, PALETTERGB (	0,  0,255)},
  {"dodger blue"		, PALETTERGB ( 30,144,255)},
  {"DodgerBlue"			, PALETTERGB ( 30,144,255)},
  {"deep sky blue"		, PALETTERGB (	0,191,255)},
  {"DeepSkyBlue"		, PALETTERGB (	0,191,255)},
  {"sky blue"			, PALETTERGB (135,206,235)},
  {"SkyBlue"			, PALETTERGB (135,206,235)},
  {"light sky blue"		, PALETTERGB (135,206,250)},
  {"LightSkyBlue"		, PALETTERGB (135,206,250)},
  {"steel blue"			, PALETTERGB ( 70,130,180)},
  {"SteelBlue"			, PALETTERGB ( 70,130,180)},
  {"light steel blue"		, PALETTERGB (176,196,222)},
  {"LightSteelBlue"		, PALETTERGB (176,196,222)},
  {"light blue"			, PALETTERGB (173,216,230)},
  {"LightBlue"			, PALETTERGB (173,216,230)},
  {"powder blue"		, PALETTERGB (176,224,230)},
  {"PowderBlue"			, PALETTERGB (176,224,230)},
  {"pale turquoise"		, PALETTERGB (175,238,238)},
  {"PaleTurquoise"		, PALETTERGB (175,238,238)},
  {"dark turquoise"		, PALETTERGB (	0,206,209)},
  {"DarkTurquoise"		, PALETTERGB (	0,206,209)},
  {"medium turquoise"		, PALETTERGB ( 72,209,204)},
  {"MediumTurquoise"		, PALETTERGB ( 72,209,204)},
  {"turquoise"			, PALETTERGB ( 64,224,208)},
  {"cyan"			, PALETTERGB (	0,255,255)},
  {"light cyan"			, PALETTERGB (224,255,255)},
  {"LightCyan"			, PALETTERGB (224,255,255)},
  {"cadet blue"			, PALETTERGB ( 95,158,160)},
  {"CadetBlue"			, PALETTERGB ( 95,158,160)},
  {"medium aquamarine"		, PALETTERGB (102,205,170)},
  {"MediumAquamarine"		, PALETTERGB (102,205,170)},
  {"aquamarine"			, PALETTERGB (127,255,212)},
  {"dark green"			, PALETTERGB (	0,100,	0)},
  {"DarkGreen"			, PALETTERGB (	0,100,	0)},
  {"dark olive green"		, PALETTERGB ( 85,107, 47)},
  {"DarkOliveGreen"		, PALETTERGB ( 85,107, 47)},
  {"dark sea green"		, PALETTERGB (143,188,143)},
  {"DarkSeaGreen"		, PALETTERGB (143,188,143)},
  {"sea green"			, PALETTERGB ( 46,139, 87)},
  {"SeaGreen"			, PALETTERGB ( 46,139, 87)},
  {"medium sea green"		, PALETTERGB ( 60,179,113)},
  {"MediumSeaGreen"		, PALETTERGB ( 60,179,113)},
  {"light sea green"		, PALETTERGB ( 32,178,170)},
  {"LightSeaGreen"		, PALETTERGB ( 32,178,170)},
  {"pale green"			, PALETTERGB (152,251,152)},
  {"PaleGreen"			, PALETTERGB (152,251,152)},
  {"spring green"		, PALETTERGB (	0,255,127)},
  {"SpringGreen"		, PALETTERGB (	0,255,127)},
  {"lawn green"			, PALETTERGB (124,252,	0)},
  {"LawnGreen"			, PALETTERGB (124,252,	0)},
  {"green"			, PALETTERGB (	0,255,	0)},
  {"chartreuse"			, PALETTERGB (127,255,	0)},
  {"medium spring green"	, PALETTERGB (	0,250,154)},
  {"MediumSpringGreen"		, PALETTERGB (	0,250,154)},
  {"green yellow"		, PALETTERGB (173,255, 47)},
  {"GreenYellow"		, PALETTERGB (173,255, 47)},
  {"lime green"			, PALETTERGB ( 50,205, 50)},
  {"LimeGreen"			, PALETTERGB ( 50,205, 50)},
  {"yellow green"		, PALETTERGB (154,205, 50)},
  {"YellowGreen"		, PALETTERGB (154,205, 50)},
  {"forest green"		, PALETTERGB ( 34,139, 34)},
  {"ForestGreen"		, PALETTERGB ( 34,139, 34)},
  {"olive drab"			, PALETTERGB (107,142, 35)},
  {"OliveDrab"			, PALETTERGB (107,142, 35)},
  {"dark khaki"			, PALETTERGB (189,183,107)},
  {"DarkKhaki"			, PALETTERGB (189,183,107)},
  {"khaki"			, PALETTERGB (240,230,140)},
  {"pale goldenrod"		, PALETTERGB (238,232,170)},
  {"PaleGoldenrod"		, PALETTERGB (238,232,170)},
  {"light goldenrod yellow"	, PALETTERGB (250,250,210)},
  {"LightGoldenrodYellow"	, PALETTERGB (250,250,210)},
  {"light yellow"		, PALETTERGB (255,255,224)},
  {"LightYellow"		, PALETTERGB (255,255,224)},
  {"yellow"			, PALETTERGB (255,255,	0)},
  {"gold"			, PALETTERGB (255,215,	0)},
  {"light goldenrod"		, PALETTERGB (238,221,130)},
  {"LightGoldenrod"		, PALETTERGB (238,221,130)},
  {"goldenrod"			, PALETTERGB (218,165, 32)},
  {"dark goldenrod"		, PALETTERGB (184,134, 11)},
  {"DarkGoldenrod"		, PALETTERGB (184,134, 11)},
  {"rosy brown"			, PALETTERGB (188,143,143)},
  {"RosyBrown"			, PALETTERGB (188,143,143)},
  {"indian red"			, PALETTERGB (205, 92, 92)},
  {"IndianRed"			, PALETTERGB (205, 92, 92)},
  {"saddle brown"		, PALETTERGB (139, 69, 19)},
  {"SaddleBrown"		, PALETTERGB (139, 69, 19)},
  {"sienna"			, PALETTERGB (160, 82, 45)},
  {"peru"			, PALETTERGB (205,133, 63)},
  {"burlywood"			, PALETTERGB (222,184,135)},
  {"beige"			, PALETTERGB (245,245,220)},
  {"wheat"			, PALETTERGB (245,222,179)},
  {"sandy brown"		, PALETTERGB (244,164, 96)},
  {"SandyBrown"			, PALETTERGB (244,164, 96)},
  {"tan"			, PALETTERGB (210,180,140)},
  {"chocolate"			, PALETTERGB (210,105, 30)},
  {"firebrick"			, PALETTERGB (178, 34, 34)},
  {"brown"			, PALETTERGB (165, 42, 42)},
  {"dark salmon"		, PALETTERGB (233,150,122)},
  {"DarkSalmon"			, PALETTERGB (233,150,122)},
  {"salmon"			, PALETTERGB (250,128,114)},
  {"light salmon"		, PALETTERGB (255,160,122)},
  {"LightSalmon"		, PALETTERGB (255,160,122)},
  {"orange"			, PALETTERGB (255,165,	0)},
  {"dark orange"		, PALETTERGB (255,140,	0)},
  {"DarkOrange"			, PALETTERGB (255,140,	0)},
  {"coral"			, PALETTERGB (255,127, 80)},
  {"light coral"		, PALETTERGB (240,128,128)},
  {"LightCoral"			, PALETTERGB (240,128,128)},
  {"tomato"			, PALETTERGB (255, 99, 71)},
  {"orange red"			, PALETTERGB (255, 69,	0)},
  {"OrangeRed"			, PALETTERGB (255, 69,	0)},
  {"red"			, PALETTERGB (255,  0,	0)},
  {"hot pink"			, PALETTERGB (255,105,180)},
  {"HotPink"			, PALETTERGB (255,105,180)},
  {"deep pink"			, PALETTERGB (255, 20,147)},
  {"DeepPink"			, PALETTERGB (255, 20,147)},
  {"pink"			, PALETTERGB (255,192,203)},
  {"light pink"			, PALETTERGB (255,182,193)},
  {"LightPink"			, PALETTERGB (255,182,193)},
  {"pale violet red"		, PALETTERGB (219,112,147)},
  {"PaleVioletRed"		, PALETTERGB (219,112,147)},
  {"maroon"			, PALETTERGB (176, 48, 96)},
  {"medium violet red"		, PALETTERGB (199, 21,133)},
  {"MediumVioletRed"		, PALETTERGB (199, 21,133)},
  {"violet red"			, PALETTERGB (208, 32,144)},
  {"VioletRed"			, PALETTERGB (208, 32,144)},
  {"magenta"			, PALETTERGB (255,  0,255)},
  {"violet"			, PALETTERGB (238,130,238)},
  {"plum"			, PALETTERGB (221,160,221)},
  {"orchid"			, PALETTERGB (218,112,214)},
  {"medium orchid"		, PALETTERGB (186, 85,211)},
  {"MediumOrchid"		, PALETTERGB (186, 85,211)},
  {"dark orchid"		, PALETTERGB (153, 50,204)},
  {"DarkOrchid"			, PALETTERGB (153, 50,204)},
  {"dark violet"		, PALETTERGB (148,  0,211)},
  {"DarkViolet"			, PALETTERGB (148,  0,211)},
  {"blue violet"		, PALETTERGB (138, 43,226)},
  {"BlueViolet"			, PALETTERGB (138, 43,226)},
  {"purple"			, PALETTERGB (160, 32,240)},
  {"medium purple"		, PALETTERGB (147,112,219)},
  {"MediumPurple"		, PALETTERGB (147,112,219)},
  {"thistle"			, PALETTERGB (216,191,216)},
  {"gray0"			, PALETTERGB (	0,  0,	0)},
  {"grey0"			, PALETTERGB (	0,  0,	0)},
  {"dark grey"			, PALETTERGB (169,169,169)},
  {"DarkGrey"			, PALETTERGB (169,169,169)},
  {"dark gray"			, PALETTERGB (169,169,169)},
  {"DarkGray"			, PALETTERGB (169,169,169)},
  {"dark blue"			, PALETTERGB (	0,  0,139)},
  {"DarkBlue"			, PALETTERGB (	0,  0,139)},
  {"dark cyan"			, PALETTERGB (	0,139,139)},
  {"DarkCyan"			, PALETTERGB (	0,139,139)},
  {"dark magenta"		, PALETTERGB (139,  0,139)},
  {"DarkMagenta"		, PALETTERGB (139,  0,139)},
  {"dark red"			, PALETTERGB (139,  0,	0)},
  {"DarkRed"			, PALETTERGB (139,  0,	0)},
  {"light green"		, PALETTERGB (144,238,144)},
  {"LightGreen"			, PALETTERGB (144,238,144)},
};

static COLORREF
w32_string_to_color(CONST char *name)
{
  int color, i;

  if (*name == '#')
  {
    /* w32 numeric names look like "#BBGGRR" */
    if (strlen(name)!=7)
      return (-1);
    for (i=1; i<7; i++)
      if (!isxdigit(name[i]))
	return(-1);
    if (sscanf(name+1, "%x", &color) == 1)
      return(0x02000000 | color);	/* See PALETTERGB in docs */
  }
  else
  {
    for(i=0; i<(sizeof(w32_X_color_map)/sizeof(colormap_t)); i++)
      if (!stricmp(name, w32_X_color_map[i].name))
	return (w32_X_color_map[i].colorref);
  }
  return(-1);
}

static int
w32_initialize_color_instance (struct Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object device, Error_behavior errb)
{
  CONST char *extname;
  COLORREF color;

  GET_C_STRING_CTEXT_DATA_ALLOCA (name, extname);
  color = w32_string_to_color(extname);
  if (color != -1)
    {
      c->data = xnew (struct w32_color_instance_data);
      COLOR_INSTANCE_W32_COLOR (c) = color;
      COLOR_INSTANCE_W32_BRUSH (c) = CreateSolidBrush (color);
      return 1;
    }
  maybe_signal_simple_error ("unrecognized color", name, Qcolor, errb);
  return(0);
}

static void
w32_mark_color_instance (struct Lisp_Color_Instance *c,
			 void (*markobj) (Lisp_Object))
{
}

static void
w32_print_color_instance (struct Lisp_Color_Instance *c,
			  Lisp_Object printcharfun,
			  int escapeflag)
{
  char buf[32];
  COLORREF color = COLOR_INSTANCE_W32_COLOR (c);
  sprintf (buf, " %06ld=(%02X,%02X,%02X)", color & 0xffffff,
	   GetRValue(color), GetGValue(color), GetBValue(color));
  write_c_string (buf, printcharfun);
}

static void
w32_finalize_color_instance (struct Lisp_Color_Instance *c)
{
  if (c->data)
    {
      DeleteObject (COLOR_INSTANCE_W32_BRUSH (c));
      xfree (c->data);
      c->data = 0;
    }
}

static int
w32_color_instance_equal (struct Lisp_Color_Instance *c1,
			  struct Lisp_Color_Instance *c2,
			  int depth)
{
  return (COLOR_INSTANCE_W32_COLOR(c1) == COLOR_INSTANCE_W32_COLOR(c2));
}

static unsigned long
w32_color_instance_hash (struct Lisp_Color_Instance *c, int depth)
{
  return LISP_HASH (COLOR_INSTANCE_W32_COLOR(c));
}

static Lisp_Object
w32_color_instance_rgb_components (struct Lisp_Color_Instance *c)
{
  COLORREF color = COLOR_INSTANCE_W32_COLOR (c);
  return (list3 (make_int (GetRValue(color)),
		 make_int (GetGValue(color)),
		 make_int (GetBValue(color))));
}

static int
w32_valid_color_name_p (struct device *d, Lisp_Object color)
{
  CONST char *extname;

  GET_C_STRING_CTEXT_DATA_ALLOCA (color, extname);
  return (w32_string_to_color(extname)!=-1);
}



static void
w32_finalize_font_instance (struct Lisp_Font_Instance *f)
{
  if (f->data)
    {
      DeleteObject(f->data);
      f->data=0;
    }
}

static int
w32_initialize_font_instance (struct Lisp_Font_Instance *f, Lisp_Object name,
			      Lisp_Object device, Error_behavior errb)
{
  CONST char *extname;
  LOGFONT logfont;
  int fields;
  int pt;
  char fontname[LF_FACESIZE], weight[32], *style, points[8], effects[32], charset[32];

  GET_C_STRING_CTEXT_DATA_ALLOCA (f->name, extname);

  /*
   * w32 fonts look like:
   *	fontname[:[weight ][style][:pointsize[:effects[:charset]]]]
   * The font name field shouldn't be empty.
   * XXX Windows will substitute a default (monospace) font if the font name
   * specifies a non-existent font. We don't catch this.
   * effects and charset are currently ignored.
   *
   * ie:
   *	Lucida Console:Regular:10
   * minimal:
   *	Courier New
   * maximal:
   *	Courier New:Bold Italic:10:underline strikeout:ansi
   */
  fields = sscanf (extname, "%31[^:]:%31[^:]:%7[^:]:%31[^:]:%31s",
		   fontname, weight, points, effects, charset);

  if (fields<0)
  {
    maybe_signal_simple_error ("Invalid font", f->name, Qfont, errb);
    return (0);
  }

  if (fields>0 && strlen(fontname))
  {
    strncpy (logfont.lfFaceName, fontname, LF_FACESIZE);
    logfont.lfFaceName[LF_FACESIZE-1] = 0;
  }
  else
  {
    maybe_signal_simple_error ("Must specify a font name", f->name, Qfont, errb);
    return (0);
  }

  if (fields > 1 && strlen(weight))
  {
    char *c;
    /* Maybe split weight into weight and style */
    if (c=strchr(weight, ' '))
    {
      *c = '\0';
      style = c+1;
    }
    else
      style = NULL;

    /* weight: Most-often used (maybe) first */
    if (stricmp (weight,"regular") == 0)
      logfont.lfWeight = FW_REGULAR;
    else if (stricmp (weight,"normal") == 0)
      logfont.lfWeight = FW_NORMAL;
    else if (stricmp (weight,"bold") == 0)
      logfont.lfWeight = FW_BOLD;
    else if (stricmp (weight,"medium") == 0)
      logfont.lfWeight = FW_MEDIUM;
    else if (stricmp (weight,"italic") == 0)	/* Hack for early exit */
    {
      logfont.lfWeight = FW_NORMAL;
      style=weight;
    }
    /* the rest */
    else if (stricmp (weight,"black") == 0)
      logfont.lfWeight = FW_BLACK;
    else if (stricmp (weight,"heavy") == 0)
      logfont.lfWeight = FW_HEAVY;
    else if (stricmp (weight,"ultrabold") == 0)
      logfont.lfWeight = FW_ULTRABOLD;
    else if (stricmp (weight,"extrabold") == 0)
      logfont.lfWeight = FW_EXTRABOLD;
    else if (stricmp (weight,"demibold") == 0)
      logfont.lfWeight = FW_SEMIBOLD;
    else if (stricmp (weight,"semibold") == 0)
      logfont.lfWeight = FW_SEMIBOLD;
    else if (stricmp (weight,"light") == 0)
      logfont.lfWeight = FW_LIGHT;
    else if (stricmp (weight,"ultralight") == 0)
      logfont.lfWeight = FW_ULTRALIGHT;
    else if (stricmp (weight,"extralight") == 0)
      logfont.lfWeight = FW_EXTRALIGHT;
    else if (stricmp (weight,"thin") == 0)
      logfont.lfWeight = FW_THIN;
    else
    {
      logfont.lfWeight = FW_NORMAL;
      if (!style)
	style = weight;	/* May have specified a style without a weight */
      else
      {
        maybe_signal_simple_error ("Invalid font weight", f->name, Qfont, errb);
	return (0);	/* Invalid weight */
      }
    }

    if (style)
    {
      /* XXX what about oblique? */
      if (stricmp (style,"italic") == 0)
	logfont.lfItalic = TRUE;
      else if (stricmp (style,"roman") == 0)
	logfont.lfItalic = FALSE;
      else
      {
        maybe_signal_simple_error ("Invalid font weight or style", f->name, Qfont, errb);
	return (0);	/* Invalid weight or style */
      }
    }
    else
    {
      logfont.lfItalic = FALSE;
    }

  }
  else
  {
    logfont.lfWeight = FW_NORMAL;
    logfont.lfItalic = FALSE;
  }

  /* XXX Should we reject strings that don't specify a size? */
  if (fields < 3 || !strlen(points) || (pt=atoi(points))==0)
    pt = 10;

  /* Formula for pointsize->height from LOGFONT docs in MSVC5 Platform SDK */
  logfont.lfHeight = -MulDiv(pt, DEVICE_W32_LOGPIXELSY(XDEVICE (device)), 72);
  logfont.lfWidth = 0;

  /* Default to monospaced if the specified font name is not found */
  logfont.lfPitchAndFamily = FF_MODERN;

  /* XXX: FIXME? */
  logfont.lfUnderline = FALSE;
  logfont.lfStrikeOut = FALSE;

  /* XXX: FIXME: we ignore charset */
  logfont.lfCharSet = DEFAULT_CHARSET;

  /* Misc crud */
  logfont.lfEscapement = logfont.lfOrientation = 0;
#if 1
  logfont.lfOutPrecision = OUT_DEFAULT_PRECIS;
  logfont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
  logfont.lfQuality = DEFAULT_QUALITY;
#else
  logfont.lfOutPrecision = OUT_STROKE_PRECIS;
  logfont.lfClipPrecision = CLIP_STROKE_PRECIS;
  logfont.lfQuality = PROOF_QUALITY;
#endif

  if ((f->data = CreateFontIndirect(&logfont)) == NULL)
  {
    maybe_signal_simple_error ("Couldn't create font", f->name, Qfont, errb);
    return 0;
  }

  /* Have to apply Font to a GC to get its values.
   * We'll borrow the desktop window becuase its the only window that we
   * know about that is guaranteed to exist when this gets called
   */ 
  {
    HWND hwnd;
    HDC hdc;
    HFONT holdfont;
    TEXTMETRIC metrics;

    hwnd = GetDesktopWindow();
    assert(hdc = GetDC(hwnd));	/* XXX FIXME: can this temporarily fail? */
    holdfont = SelectObject(hdc, f->data);
    if (!holdfont)
    {
      w32_finalize_font_instance (f);
      maybe_signal_simple_error ("Couldn't map font", f->name, Qfont, errb);
      return 0;
    }
    GetTextMetrics(hdc, &metrics);
    SelectObject(hdc, holdfont);
    ReleaseDC(hwnd, hdc);
    f->width = metrics.tmAveCharWidth;
    f->height = metrics.tmHeight;
    f->ascent = metrics.tmAscent;
    f->descent = metrics.tmDescent;
    f->proportional_p = (metrics.tmPitchAndFamily & TMPF_FIXED_PITCH);
  }

  return 1;
}

static void
w32_mark_font_instance (struct Lisp_Font_Instance *f,
			void (*markobj) (Lisp_Object))
{
}

static void
w32_print_font_instance (struct Lisp_Font_Instance *f,
			 Lisp_Object printcharfun,
			 int escapeflag)
{
}

static Lisp_Object
w32_list_fonts (Lisp_Object pattern, Lisp_Object device)
{
  /* XXX Implement me */
  return list1 (build_string ("Courier New:Regular:10"));
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_w32 (void)
{
}

void
console_type_create_objects_w32 (void)
{
  /* object methods */
  CONSOLE_HAS_METHOD (w32, initialize_color_instance);
/*  CONSOLE_HAS_METHOD (w32, mark_color_instance); */
  CONSOLE_HAS_METHOD (w32, print_color_instance);
  CONSOLE_HAS_METHOD (w32, finalize_color_instance);
  CONSOLE_HAS_METHOD (w32, color_instance_equal);
  CONSOLE_HAS_METHOD (w32, color_instance_hash);
  CONSOLE_HAS_METHOD (w32, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (w32, valid_color_name_p);

  CONSOLE_HAS_METHOD (w32, initialize_font_instance);
/*  CONSOLE_HAS_METHOD (w32, mark_font_instance); */
  CONSOLE_HAS_METHOD (w32, print_font_instance);
  CONSOLE_HAS_METHOD (w32, finalize_font_instance);
/*  CONSOLE_HAS_METHOD (w32, font_instance_truename); */
  CONSOLE_HAS_METHOD (w32, list_fonts);
#ifdef MULE
  CONSOLE_HAS_METHOD (w32, font_spec_matches_charset);
  CONSOLE_HAS_METHOD (w32, find_charset_font);
#endif
}

void
vars_of_objects_w32 (void)
{
}
