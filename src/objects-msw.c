/* mswindows-specific Lisp objects.
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
   Rewritten for mswindows by Jonathan Harris, November 1997 for 20.4.
 */


/* TODO: palette handling */

#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "objects-msw.h"

#ifdef MULE
#include "mule-charset.h"
#endif

#include "buffer.h"
#include "device.h"
#include "insdel.h"

#include "windows.h"
#ifdef __CYGWIN32__
#define stricmp strcasecmp
#endif

typedef struct colormap_t 
{
  char *name;
  COLORREF colorref;
} colormap_t;

static CONST colormap_t mswindows_X_color_map[] = 
{
  {"Black"			, PALETTERGB (  0,  0,  0)},
  {"Gray0"			, PALETTERGB (  0,  0,  0)},
  {"Grey0"			, PALETTERGB (  0,  0,  0)},
  {"Transparent"			, PALETTERGB (  0,  0,  1)},
  {"NavyBlue"			, PALETTERGB (  0,  0,128)},
  {"navy"			, PALETTERGB (  0,  0,128)},
  {"blue4"			, PALETTERGB (  0,  0,139)},
  {"MediumBlue"			, PALETTERGB (  0,  0,205)},
  {"blue3"			, PALETTERGB (  0,  0,205)},
  {"blue2"			, PALETTERGB (  0,  0,238)},
  {"Blue"			, PALETTERGB (  0,  0,255)},
  {"blue1"			, PALETTERGB (  0,  0,255)},
  {"DarkGreen"			, PALETTERGB (  0, 86, 45)},
  {"DeepSkyBlue4"			, PALETTERGB (  0,104,139)},
  {"turquoise4"			, PALETTERGB (  0,134,139)},
  {"green4"			, PALETTERGB (  0,139,  0)},
  {"SpringGreen4"			, PALETTERGB (  0,139, 69)},
  {"cyan4"			, PALETTERGB (  0,139,139)},
  {"MediumAquamarine"			, PALETTERGB (  0,147,143)},
  {"DeepSkyBlue3"			, PALETTERGB (  0,154,205)},
  {"DarkTurquoise"			, PALETTERGB (  0,166,166)},
  {"LimeGreen"			, PALETTERGB (  0,175, 20)},
  {"DeepSkyBlue2"			, PALETTERGB (  0,178,238)},
  {"DeepSkyBlue"			, PALETTERGB (  0,191,255)},
  {"DeepSkyBlue1"			, PALETTERGB (  0,191,255)},
  {"turquoise3"			, PALETTERGB (  0,197,205)},
  {"green3"			, PALETTERGB (  0,205,  0)},
  {"SpringGreen3"			, PALETTERGB (  0,205,102)},
  {"cyan3"			, PALETTERGB (  0,205,205)},
  {"MediumTurquoise"			, PALETTERGB (  0,210,210)},
  {"turquoise2"			, PALETTERGB (  0,229,238)},
  {"green2"			, PALETTERGB (  0,238,  0)},
  {"SpringGreen2"			, PALETTERGB (  0,238,118)},
  {"cyan2"			, PALETTERGB (  0,238,238)},
  {"turquoise1"			, PALETTERGB (  0,245,255)},
  {"MediumSpringGreen"			, PALETTERGB (  0,250,154)},
  {"Green"			, PALETTERGB (  0,255,  0)},
  {"green1"			, PALETTERGB (  0,255,  0)},
  {"SpringGreen"			, PALETTERGB (  0,255,127)},
  {"SpringGreen1"			, PALETTERGB (  0,255,127)},
  {"Cyan"			, PALETTERGB (  0,255,255)},
  {"cyan1"			, PALETTERGB (  0,255,255)},
  {"Gray1"			, PALETTERGB (  3,  3,  3)},
  {"Grey1"			, PALETTERGB (  3,  3,  3)},
  {"Gray2"			, PALETTERGB (  5,  5,  5)},
  {"Grey2"			, PALETTERGB (  5,  5,  5)},
  {"Gray3"			, PALETTERGB (  8,  8,  8)},
  {"Grey3"			, PALETTERGB (  8,  8,  8)},
  {"Gray4"			, PALETTERGB ( 10, 10, 10)},
  {"Grey4"			, PALETTERGB ( 10, 10, 10)},
  {"Gray5"			, PALETTERGB ( 13, 13, 13)},
  {"Grey5"			, PALETTERGB ( 13, 13, 13)},
  {"Gray6"			, PALETTERGB ( 15, 15, 15)},
  {"Grey6"			, PALETTERGB ( 15, 15, 15)},
  {"DodgerBlue4"			, PALETTERGB ( 16, 78,139)},
  {"Gray7"			, PALETTERGB ( 18, 18, 18)},
  {"Grey7"			, PALETTERGB ( 18, 18, 18)},
  {"Gray8"			, PALETTERGB ( 20, 20, 20)},
  {"Grey8"			, PALETTERGB ( 20, 20, 20)},
  {"Gray9"			, PALETTERGB ( 23, 23, 23)},
  {"Grey9"			, PALETTERGB ( 23, 23, 23)},
  {"DodgerBlue3"			, PALETTERGB ( 24,116,205)},
  {"MidnightBlue"			, PALETTERGB ( 25, 25,112)},
  {"Turquoise"			, PALETTERGB ( 25,204,223)},
  {"Gray10"			, PALETTERGB ( 26, 26, 26)},
  {"Grey10"			, PALETTERGB ( 26, 26, 26)},
  {"Gray11"			, PALETTERGB ( 28, 28, 28)},
  {"Grey11"			, PALETTERGB ( 28, 28, 28)},
  {"DodgerBlue2"			, PALETTERGB ( 28,134,238)},
  {"DodgerBlue"			, PALETTERGB ( 30,144,255)},
  {"DodgerBlue1"			, PALETTERGB ( 30,144,255)},
  {"Gray12"			, PALETTERGB ( 31, 31, 31)},
  {"Grey12"			, PALETTERGB ( 31, 31, 31)},
  {"LightSeaGreen"			, PALETTERGB ( 32,178,170)},
  {"Gray13"			, PALETTERGB ( 33, 33, 33)},
  {"Grey13"			, PALETTERGB ( 33, 33, 33)},
  {"Indigo2"			, PALETTERGB ( 33,136,104)},
  {"CornflowerBlue"			, PALETTERGB ( 34, 34,152)},
  {"ForestGreen"			, PALETTERGB ( 34,139, 34)},
  {"Gray14"			, PALETTERGB ( 36, 36, 36)},
  {"Grey14"			, PALETTERGB ( 36, 36, 36)},
  {"Gray15"			, PALETTERGB ( 38, 38, 38)},
  {"Grey15"			, PALETTERGB ( 38, 38, 38)},
  {"RoyalBlue4"			, PALETTERGB ( 39, 64,139)},
  {"Gray16"			, PALETTERGB ( 41, 41, 41)},
  {"Grey16"			, PALETTERGB ( 41, 41, 41)},
  {"Gray17"			, PALETTERGB ( 43, 43, 43)},
  {"Grey17"			, PALETTERGB ( 43, 43, 43)},
  {"Gray18"			, PALETTERGB ( 46, 46, 46)},
  {"Grey18"			, PALETTERGB ( 46, 46, 46)},
  {"SeaGreen"			, PALETTERGB ( 46,139, 87)},
  {"SeaGreen4"			, PALETTERGB ( 46,139, 87)},
  {"DarkSlateGray"			, PALETTERGB ( 47, 79, 79)},
  {"DarkSlateGrey"			, PALETTERGB ( 47, 79, 79)},
  {"Gray19"			, PALETTERGB ( 48, 48, 48)},
  {"Grey19"			, PALETTERGB ( 48, 48, 48)},
  {"MediumForestGreen"			, PALETTERGB ( 50,129, 75)},
  {"Aquamarine"			, PALETTERGB ( 50,191,193)},
  {"YellowGreen"			, PALETTERGB ( 50,216, 56)},
  {"Gray20"			, PALETTERGB ( 51, 51, 51)},
  {"Grey20"			, PALETTERGB ( 51, 51, 51)},
  {"MediumSeaGreen"			, PALETTERGB ( 52,119,102)},
  {"Gray21"			, PALETTERGB ( 54, 54, 54)},
  {"Grey21"			, PALETTERGB ( 54, 54, 54)},
  {"SteelBlue4"			, PALETTERGB ( 54,100,139)},
  {"Gray22"			, PALETTERGB ( 56, 56, 56)},
  {"Grey22"			, PALETTERGB ( 56, 56, 56)},
  {"DarkSlateBlue"			, PALETTERGB ( 56, 75,102)},
  {"RoyalBlue3"			, PALETTERGB ( 58, 95,205)},
  {"Gray23"			, PALETTERGB ( 59, 59, 59)},
  {"Grey23"			, PALETTERGB ( 59, 59, 59)},
  {"Gray24"			, PALETTERGB ( 61, 61, 61)},
  {"Grey24"			, PALETTERGB ( 61, 61, 61)},
  {"Gray25"			, PALETTERGB ( 64, 64, 64)},
  {"Grey25"			, PALETTERGB ( 64, 64, 64)},
  {"RoyalBlue"			, PALETTERGB ( 65,105,225)},
  {"Gray26"			, PALETTERGB ( 66, 66, 66)},
  {"Grey26"			, PALETTERGB ( 66, 66, 66)},
  {"RoyalBlue2"			, PALETTERGB ( 67,110,238)},
  {"SeaGreen3"			, PALETTERGB ( 67,205,128)},
  {"Gray27"			, PALETTERGB ( 69, 69, 69)},
  {"Grey27"			, PALETTERGB ( 69, 69, 69)},
  {"chartreuse4"			, PALETTERGB ( 69,139,  0)},
  {"aquamarine4"			, PALETTERGB ( 69,139,116)},
  {"SteelBlue"			, PALETTERGB ( 70,130,180)},
  {"SlateBlue4"			, PALETTERGB ( 71, 60,139)},
  {"Gray28"			, PALETTERGB ( 71, 71, 71)},
  {"Grey28"			, PALETTERGB ( 71, 71, 71)},
  {"RoyalBlue1"			, PALETTERGB ( 72,118,255)},
  {"Gray29"			, PALETTERGB ( 74, 74, 74)},
  {"Grey29"			, PALETTERGB ( 74, 74, 74)},
  {"SkyBlue4"			, PALETTERGB ( 74,112,139)},
  {"Indigo"			, PALETTERGB ( 75,  0,130)},
  {"Gray30"			, PALETTERGB ( 77, 77, 77)},
  {"Grey30"			, PALETTERGB ( 77, 77, 77)},
  {"SeaGreen2"			, PALETTERGB ( 78,238,148)},
  {"Gray31"			, PALETTERGB ( 79, 79, 79)},
  {"Grey31"			, PALETTERGB ( 79, 79, 79)},
  {"SteelBlue3"			, PALETTERGB ( 79,148,205)},
  {"Gray32"			, PALETTERGB ( 82, 82, 82)},
  {"Grey32"			, PALETTERGB ( 82, 82, 82)},
  {"DarkSlateGray4"			, PALETTERGB ( 82,139,139)},
  {"CadetBlue4"			, PALETTERGB ( 83,134,139)},
  {"DimGray"			, PALETTERGB ( 84, 84, 84)},
  {"DimGrey"			, PALETTERGB ( 84, 84, 84)},
  {"Gray33"			, PALETTERGB ( 84, 84, 84)},
  {"Grey33"			, PALETTERGB ( 84, 84, 84)},
  {"PaleGreen4"			, PALETTERGB ( 84,139, 84)},
  {"SeaGreen1"			, PALETTERGB ( 84,255,159)},
  {"purple4"			, PALETTERGB ( 85, 26,139)},
  {"DarkOliveGreen"			, PALETTERGB ( 85, 86, 47)},
  {"Gray34"			, PALETTERGB ( 87, 87, 87)},
  {"Grey34"			, PALETTERGB ( 87, 87, 87)},
  {"Gray35"			, PALETTERGB ( 89, 89, 89)},
  {"Grey35"			, PALETTERGB ( 89, 89, 89)},
  {"Gray36"			, PALETTERGB ( 92, 92, 92)},
  {"Grey36"			, PALETTERGB ( 92, 92, 92)},
  {"SteelBlue2"			, PALETTERGB ( 92,172,238)},
  {"MediumPurple4"			, PALETTERGB ( 93, 71,139)},
  {"Gray37"			, PALETTERGB ( 94, 94, 94)},
  {"Grey37"			, PALETTERGB ( 94, 94, 94)},
  {"CadetBlue"			, PALETTERGB ( 95,146,158)},
  {"LightSkyBlue4"			, PALETTERGB ( 96,123,139)},
  {"Gray38"			, PALETTERGB ( 97, 97, 97)},
  {"Grey38"			, PALETTERGB ( 97, 97, 97)},
  {"Gray39"			, PALETTERGB ( 99, 99, 99)},
  {"Grey39"			, PALETTERGB ( 99, 99, 99)},
  {"SteelBlue1"			, PALETTERGB ( 99,184,255)},
  {"Gray40"			, PALETTERGB (102,102,102)},
  {"Grey40"			, PALETTERGB (102,102,102)},
  {"PaleTurquoise4"			, PALETTERGB (102,139,139)},
  {"chartreuse3"			, PALETTERGB (102,205,  0)},
  {"aquamarine3"			, PALETTERGB (102,205,170)},
  {"DarkOrchid4"			, PALETTERGB (104, 34,139)},
  {"LightBlue4"			, PALETTERGB (104,131,139)},
  {"SlateBlue3"			, PALETTERGB (105, 89,205)},
  {"Gray41"			, PALETTERGB (105,105,105)},
  {"Grey41"			, PALETTERGB (105,105,105)},
  {"OliveDrab4"			, PALETTERGB (105,139, 34)},
  {"DarkSeaGreen4"			, PALETTERGB (105,139,105)},
  {"SlateBlue"			, PALETTERGB (106, 90,205)},
  {"MediumSlateBlue"			, PALETTERGB (106,106,141)},
  {"IndianRed"			, PALETTERGB (107, 57, 57)},
  {"Gray42"			, PALETTERGB (107,107,107)},
  {"Grey42"			, PALETTERGB (107,107,107)},
  {"OliveDrab"			, PALETTERGB (107,142, 35)},
  {"SlateGray4"			, PALETTERGB (108,123,139)},
  {"SkyBlue3"			, PALETTERGB (108,166,205)},
  {"Gray43"			, PALETTERGB (110,110,110)},
  {"Grey43"			, PALETTERGB (110,110,110)},
  {"LightSteelBlue4"			, PALETTERGB (110,123,139)},
  {"DarkOliveGreen4"			, PALETTERGB (110,139, 61)},
  {"Gray44"			, PALETTERGB (112,112,112)},
  {"Grey44"			, PALETTERGB (112,112,112)},
  {"SlateGray"			, PALETTERGB (112,128,144)},
  {"SlateGrey"			, PALETTERGB (112,128,144)},
  {"SkyBlue"			, PALETTERGB (114,159,255)},
  {"Gray45"			, PALETTERGB (115,115,115)},
  {"Grey45"			, PALETTERGB (115,115,115)},
  {"PaleGreen"			, PALETTERGB (115,222,120)},
  {"Gray46"			, PALETTERGB (117,117,117)},
  {"Grey46"			, PALETTERGB (117,117,117)},
  {"chartreuse2"			, PALETTERGB (118,238,  0)},
  {"aquamarine2"			, PALETTERGB (118,238,198)},
  {"LightSlateGray"			, PALETTERGB (119,136,153)},
  {"LightSlateGrey"			, PALETTERGB (119,136,153)},
  {"Gray47"			, PALETTERGB (120,120,120)},
  {"Grey47"			, PALETTERGB (120,120,120)},
  {"DarkSlateGray3"			, PALETTERGB (121,205,205)},
  {"MediumOrchid4"			, PALETTERGB (122, 55,139)},
  {"SlateBlue2"			, PALETTERGB (122,103,238)},
  {"Gray48"			, PALETTERGB (122,122,122)},
  {"Grey48"			, PALETTERGB (122,122,122)},
  {"LightCyan4"			, PALETTERGB (122,139,139)},
  {"CadetBlue3"			, PALETTERGB (122,197,205)},
  {"LightSteelBlue"			, PALETTERGB (124,152,211)},
  {"PaleGreen3"			, PALETTERGB (124,205,124)},
  {"LawnGreen"			, PALETTERGB (124,252,  0)},
  {"purple3"			, PALETTERGB (125, 38,205)},
  {"Gray49"			, PALETTERGB (125,125,125)},
  {"Grey49"			, PALETTERGB (125,125,125)},
  {"Gray"			, PALETTERGB (126,126,126)},
  {"Grey"			, PALETTERGB (126,126,126)},
  {"SkyBlue2"			, PALETTERGB (126,192,238)},
  {"Gray50"			, PALETTERGB (127,127,127)},
  {"Grey50"			, PALETTERGB (127,127,127)},
  {"chartreuse"			, PALETTERGB (127,255,  0)},
  {"chartreuse1"			, PALETTERGB (127,255,  0)},
  {"aquamarine1"			, PALETTERGB (127,255,212)},
  {"Gray51"			, PALETTERGB (130,130,130)},
  {"Grey51"			, PALETTERGB (130,130,130)},
  {"SlateBlue1"			, PALETTERGB (131,111,255)},
  {"honeydew4"			, PALETTERGB (131,139,131)},
  {"azure4"			, PALETTERGB (131,139,139)},
  {"LightSlateBlue"			, PALETTERGB (132,112,255)},
  {"Gray52"			, PALETTERGB (133,133,133)},
  {"Grey52"			, PALETTERGB (133,133,133)},
  {"Gray53"			, PALETTERGB (135,135,135)},
  {"Grey53"			, PALETTERGB (135,135,135)},
  {"LightSkyBlue"			, PALETTERGB (135,206,250)},
  {"SkyBlue1"			, PALETTERGB (135,206,255)},
  {"MediumPurple3"			, PALETTERGB (137,104,205)},
  {"BlueViolet"			, PALETTERGB (138, 43,226)},
  {"Gray54"			, PALETTERGB (138,138,138)},
  {"Grey54"			, PALETTERGB (138,138,138)},
  {"red4"			, PALETTERGB (139,  0,  0)},
  {"magenta4"			, PALETTERGB (139,  0,139)},
  {"DeepPink4"			, PALETTERGB (139, 10, 80)},
  {"firebrick4"			, PALETTERGB (139, 26, 26)},
  {"maroon4"			, PALETTERGB (139, 28, 98)},
  {"DarkOrchid"			, PALETTERGB (139, 32,139)},
  {"VioletRed4"			, PALETTERGB (139, 34, 82)},
  {"brown4"			, PALETTERGB (139, 35, 35)},
  {"OrangeRed4"			, PALETTERGB (139, 37,  0)},
  {"tomato4"			, PALETTERGB (139, 54, 38)},
  {"IndianRed4"			, PALETTERGB (139, 58, 58)},
  {"HotPink4"			, PALETTERGB (139, 58, 98)},
  {"coral4"			, PALETTERGB (139, 62, 47)},
  {"DarkOrange4"			, PALETTERGB (139, 69,  0)},
  {"SaddleBrown"			, PALETTERGB (139, 69, 19)},
  {"chocolate4"			, PALETTERGB (139, 69, 19)},
  {"sienna4"			, PALETTERGB (139, 71, 38)},
  {"PaleVioletRed4"			, PALETTERGB (139, 71, 93)},
  {"orchid4"			, PALETTERGB (139, 71,137)},
  {"salmon4"			, PALETTERGB (139, 76, 57)},
  {"LightSalmon4"			, PALETTERGB (139, 87, 66)},
  {"orange4"			, PALETTERGB (139, 90,  0)},
  {"tan4"			, PALETTERGB (139, 90, 43)},
  {"LightPink4"			, PALETTERGB (139, 95,101)},
  {"pink4"			, PALETTERGB (139, 99,108)},
  {"DarkGoldenrod4"			, PALETTERGB (139,101,  8)},
  {"plum4"			, PALETTERGB (139,102,139)},
  {"goldenrod4"			, PALETTERGB (139,105, 20)},
  {"RosyBrown4"			, PALETTERGB (139,105,105)},
  {"burlywood4"			, PALETTERGB (139,115, 85)},
  {"gold4"			, PALETTERGB (139,117,  0)},
  {"PeachPuff4"			, PALETTERGB (139,119,101)},
  {"NavajoWhite4"			, PALETTERGB (139,121, 94)},
  {"thistle4"			, PALETTERGB (139,123,139)},
  {"bisque4"			, PALETTERGB (139,125,107)},
  {"MistyRose4"			, PALETTERGB (139,125,123)},
  {"wheat4"			, PALETTERGB (139,126,102)},
  {"LightGoldenrod4"			, PALETTERGB (139,129, 76)},
  {"AntiqueWhite4"			, PALETTERGB (139,131,120)},
  {"LavenderBlush4"			, PALETTERGB (139,131,134)},
  {"khaki4"			, PALETTERGB (139,134, 78)},
  {"seashell4"			, PALETTERGB (139,134,130)},
  {"cornsilk4"			, PALETTERGB (139,136,120)},
  {"LemonChiffon4"			, PALETTERGB (139,137,112)},
  {"snow4"			, PALETTERGB (139,137,137)},
  {"yellow4"			, PALETTERGB (139,139,  0)},
  {"LightYellow4"			, PALETTERGB (139,139,122)},
  {"ivory4"			, PALETTERGB (139,139,131)},
  {"Gray55"			, PALETTERGB (140,140,140)},
  {"Grey55"			, PALETTERGB (140,140,140)},
  {"LightSkyBlue3"			, PALETTERGB (141,182,205)},
  {"DarkSlateGray2"			, PALETTERGB (141,238,238)},
  {"Firebrick"			, PALETTERGB (142, 35, 35)},
  {"CadetBlue2"			, PALETTERGB (142,229,238)},
  {"Maroon"			, PALETTERGB (143,  0, 82)},
  {"Gray56"			, PALETTERGB (143,143,143)},
  {"Grey56"			, PALETTERGB (143,143,143)},
  {"DarkSeaGreen"			, PALETTERGB (143,188,143)},
  {"PaleGreen2"			, PALETTERGB (144,238,144)},
  {"purple2"			, PALETTERGB (145, 44,238)},
  {"Gray57"			, PALETTERGB (145,145,145)},
  {"Grey57"			, PALETTERGB (145,145,145)},
  {"MediumPurple"			, PALETTERGB (147,112,219)},
  {"DarkViolet"			, PALETTERGB (148,  0,211)},
  {"Gray58"			, PALETTERGB (148,148,148)},
  {"Grey58"			, PALETTERGB (148,148,148)},
  {"Sienna"			, PALETTERGB (150, 82, 45)},
  {"Gray59"			, PALETTERGB (150,150,150)},
  {"Grey59"			, PALETTERGB (150,150,150)},
  {"PaleTurquoise3"			, PALETTERGB (150,205,205)},
  {"DarkSlateGray1"			, PALETTERGB (151,255,255)},
  {"CadetBlue1"			, PALETTERGB (152,245,255)},
  {"Gray60"			, PALETTERGB (153,153,153)},
  {"Grey60"			, PALETTERGB (153,153,153)},
  {"DarkOrchid3"			, PALETTERGB (154, 50,205)},
  {"LightBlue3"			, PALETTERGB (154,192,205)},
  {"OliveDrab3"			, PALETTERGB (154,205, 50)},
  {"PaleGreen1"			, PALETTERGB (154,255,154)},
  {"purple1"			, PALETTERGB (155, 48,255)},
  {"DarkSeaGreen3"			, PALETTERGB (155,205,155)},
  {"Violet"			, PALETTERGB (156, 62,206)},
  {"Gray61"			, PALETTERGB (156,156,156)},
  {"Grey61"			, PALETTERGB (156,156,156)},
  {"Gray62"			, PALETTERGB (158,158,158)},
  {"Grey62"			, PALETTERGB (158,158,158)},
  {"MediumPurple2"			, PALETTERGB (159,121,238)},
  {"SlateGray3"			, PALETTERGB (159,182,205)},
  {"purple"			, PALETTERGB (160, 32,240)},
  {"Gray63"			, PALETTERGB (161,161,161)},
  {"Grey63"			, PALETTERGB (161,161,161)},
  {"LightSteelBlue3"			, PALETTERGB (162,181,205)},
  {"DarkOliveGreen3"			, PALETTERGB (162,205, 90)},
  {"Gray64"			, PALETTERGB (163,163,163)},
  {"Grey64"			, PALETTERGB (163,163,163)},
  {"LightSkyBlue2"			, PALETTERGB (164,211,238)},
  {"Brown"			, PALETTERGB (165, 42, 42)},
  {"Gray65"			, PALETTERGB (166,166,166)},
  {"Grey65"			, PALETTERGB (166,166,166)},
  {"Gray66"			, PALETTERGB (168,168,168)},
  {"Grey66"			, PALETTERGB (168,168,168)},
  {"LightGray"			, PALETTERGB (168,168,168)},
  {"LightGrey"			, PALETTERGB (168,168,168)},
  {"MediumPurple1"			, PALETTERGB (171,130,255)},
  {"Gray67"			, PALETTERGB (171,171,171)},
  {"Grey67"			, PALETTERGB (171,171,171)},
  {"Gray68"			, PALETTERGB (173,173,173)},
  {"Grey68"			, PALETTERGB (173,173,173)},
  {"LightBlue"			, PALETTERGB (173,216,230)},
  {"GreenYellow"			, PALETTERGB (173,255, 47)},
  {"PaleTurquoise2"			, PALETTERGB (174,238,238)},
  {"PaleTurquoise"			, PALETTERGB (175,238,238)},
  {"Gray69"			, PALETTERGB (176,176,176)},
  {"Grey69"			, PALETTERGB (176,176,176)},
  {"PowderBlue"			, PALETTERGB (176,224,230)},
  {"LightSkyBlue1"			, PALETTERGB (176,226,255)},
  {"DarkOrchid2"			, PALETTERGB (178, 58,238)},
  {"LightBlue2"			, PALETTERGB (178,223,238)},
  {"Khaki"			, PALETTERGB (179,179,126)},
  {"Gray70"			, PALETTERGB (179,179,179)},
  {"Grey70"			, PALETTERGB (179,179,179)},
  {"OliveDrab2"			, PALETTERGB (179,238, 58)},
  {"MediumOrchid3"			, PALETTERGB (180, 82,205)},
  {"LightCyan3"			, PALETTERGB (180,205,205)},
  {"DarkSeaGreen2"			, PALETTERGB (180,238,180)},
  {"Gray71"			, PALETTERGB (181,181,181)},
  {"Grey71"			, PALETTERGB (181,181,181)},
  {"DarkGoldenrod"			, PALETTERGB (184,134, 11)},
  {"Gray72"			, PALETTERGB (184,184,184)},
  {"Grey72"			, PALETTERGB (184,184,184)},
  {"SlateGray2"			, PALETTERGB (185,211,238)},
  {"MediumOrchid"			, PALETTERGB (186, 85,211)},
  {"Gray73"			, PALETTERGB (186,186,186)},
  {"Grey73"			, PALETTERGB (186,186,186)},
  {"PaleTurquoise1"			, PALETTERGB (187,255,255)},
  {"RosyBrown"			, PALETTERGB (188,143,143)},
  {"LightSteelBlue2"			, PALETTERGB (188,210,238)},
  {"DarkOliveGreen2"			, PALETTERGB (188,238,104)},
  {"DarkKhaki"			, PALETTERGB (189,183,107)},
  {"Gray74"			, PALETTERGB (189,189,189)},
  {"Grey74"			, PALETTERGB (189,189,189)},
  {"DarkOrchid1"			, PALETTERGB (191, 62,255)},
  {"Gray75"			, PALETTERGB (191,191,191)},
  {"Grey75"			, PALETTERGB (191,191,191)},
  {"LightBlue1"			, PALETTERGB (191,239,255)},
  {"OliveDrab1"			, PALETTERGB (192,255, 62)},
  {"honeydew3"			, PALETTERGB (193,205,193)},
  {"azure3"			, PALETTERGB (193,205,205)},
  {"DarkSeaGreen1"			, PALETTERGB (193,255,193)},
  {"Gray76"			, PALETTERGB (194,194,194)},
  {"Grey76"			, PALETTERGB (194,194,194)},
  {"Gray77"			, PALETTERGB (196,196,196)},
  {"Grey77"			, PALETTERGB (196,196,196)},
  {"Plum"			, PALETTERGB (197, 72,155)},
  {"SlateGray1"			, PALETTERGB (198,226,255)},
  {"MediumVioletRed"			, PALETTERGB (199, 21,133)},
  {"Gray78"			, PALETTERGB (199,199,199)},
  {"Grey78"			, PALETTERGB (199,199,199)},
  {"Gray79"			, PALETTERGB (201,201,201)},
  {"Grey79"			, PALETTERGB (201,201,201)},
  {"LightSteelBlue1"			, PALETTERGB (202,225,255)},
  {"DarkOliveGreen1"			, PALETTERGB (202,255,112)},
  {"Gray80"			, PALETTERGB (204,204,204)},
  {"Grey80"			, PALETTERGB (204,204,204)},
  {"red3"			, PALETTERGB (205,  0,  0)},
  {"magenta3"			, PALETTERGB (205,  0,205)},
  {"DeepPink3"			, PALETTERGB (205, 16,118)},
  {"firebrick3"			, PALETTERGB (205, 38, 38)},
  {"maroon3"			, PALETTERGB (205, 41,144)},
  {"VioletRed3"			, PALETTERGB (205, 50,120)},
  {"brown3"			, PALETTERGB (205, 51, 51)},
  {"OrangeRed3"			, PALETTERGB (205, 55,  0)},
  {"tomato3"			, PALETTERGB (205, 79, 57)},
  {"IndianRed3"			, PALETTERGB (205, 85, 85)},
  {"coral3"			, PALETTERGB (205, 91, 69)},
  {"HotPink3"			, PALETTERGB (205, 96,144)},
  {"DarkOrange3"			, PALETTERGB (205,102,  0)},
  {"chocolate3"			, PALETTERGB (205,102, 29)},
  {"sienna3"			, PALETTERGB (205,104, 57)},
  {"PaleVioletRed3"			, PALETTERGB (205,104,137)},
  {"orchid3"			, PALETTERGB (205,105,201)},
  {"salmon3"			, PALETTERGB (205,112, 84)},
  {"LightSalmon3"			, PALETTERGB (205,129, 98)},
  {"orange3"			, PALETTERGB (205,133,  0)},
  {"peru"			, PALETTERGB (205,133, 63)},
  {"tan3"			, PALETTERGB (205,133, 63)},
  {"LightPink3"			, PALETTERGB (205,140,149)},
  {"pink3"			, PALETTERGB (205,145,158)},
  {"DarkGoldenrod3"			, PALETTERGB (205,149, 12)},
  {"plum3"			, PALETTERGB (205,150,205)},
  {"goldenrod3"			, PALETTERGB (205,155, 29)},
  {"RosyBrown3"			, PALETTERGB (205,155,155)},
  {"burlywood3"			, PALETTERGB (205,170,125)},
  {"gold3"			, PALETTERGB (205,173,  0)},
  {"PeachPuff3"			, PALETTERGB (205,175,149)},
  {"NavajoWhite3"			, PALETTERGB (205,179,139)},
  {"thistle3"			, PALETTERGB (205,181,205)},
  {"bisque3"			, PALETTERGB (205,183,158)},
  {"MistyRose3"			, PALETTERGB (205,183,181)},
  {"wheat3"			, PALETTERGB (205,186,150)},
  {"LightGoldenrod3"			, PALETTERGB (205,190,112)},
  {"AntiqueWhite3"			, PALETTERGB (205,192,176)},
  {"LavenderBlush3"			, PALETTERGB (205,193,197)},
  {"seashell3"			, PALETTERGB (205,197,191)},
  {"khaki3"			, PALETTERGB (205,198,115)},
  {"cornsilk3"			, PALETTERGB (205,200,177)},
  {"LemonChiffon3"			, PALETTERGB (205,201,165)},
  {"snow3"			, PALETTERGB (205,201,201)},
  {"yellow3"			, PALETTERGB (205,205,  0)},
  {"LightYellow3"			, PALETTERGB (205,205,180)},
  {"ivory3"			, PALETTERGB (205,205,193)},
  {"Gray81"			, PALETTERGB (207,207,207)},
  {"Grey81"			, PALETTERGB (207,207,207)},
  {"VioletRed"			, PALETTERGB (208, 32,144)},
  {"MediumOrchid2"			, PALETTERGB (209, 95,238)},
  {"MediumGoldenrod"			, PALETTERGB (209,193,102)},
  {"Gray82"			, PALETTERGB (209,209,209)},
  {"Grey82"			, PALETTERGB (209,209,209)},
  {"LightCyan2"			, PALETTERGB (209,238,238)},
  {"chocolate"			, PALETTERGB (210,105, 30)},
  {"tan"			, PALETTERGB (210,180,140)},
  {"Gray83"			, PALETTERGB (212,212,212)},
  {"Grey83"			, PALETTERGB (212,212,212)},
  {"Gray84"			, PALETTERGB (214,214,214)},
  {"Grey84"			, PALETTERGB (214,214,214)},
  {"Thistle"			, PALETTERGB (216,191,216)},
  {"Gray85"			, PALETTERGB (217,217,217)},
  {"Grey85"			, PALETTERGB (217,217,217)},
  {"orchid"			, PALETTERGB (218,112,214)},
  {"goldenrod"			, PALETTERGB (218,165, 32)},
  {"Gold"			, PALETTERGB (218,170,  0)},
  {"PaleVioletRed"			, PALETTERGB (219,112,147)},
  {"Gray86"			, PALETTERGB (219,219,219)},
  {"Grey86"			, PALETTERGB (219,219,219)},
  {"Crimson"			, PALETTERGB (220, 20, 60)},
  {"gainsboro"			, PALETTERGB (220,220,220)},
  {"burlywood"			, PALETTERGB (222,184,135)},
  {"Gray87"			, PALETTERGB (222,222,222)},
  {"Grey87"			, PALETTERGB (222,222,222)},
  {"MediumOrchid1"			, PALETTERGB (224,102,255)},
  {"Gray88"			, PALETTERGB (224,224,224)},
  {"Grey88"			, PALETTERGB (224,224,224)},
  {"honeydew2"			, PALETTERGB (224,238,224)},
  {"azure2"			, PALETTERGB (224,238,238)},
  {"LightCyan"			, PALETTERGB (224,255,255)},
  {"LightCyan1"			, PALETTERGB (224,255,255)},
  {"Gray89"			, PALETTERGB (227,227,227)},
  {"Grey89"			, PALETTERGB (227,227,227)},
  {"Gray90"			, PALETTERGB (229,229,229)},
  {"Grey90"			, PALETTERGB (229,229,229)},
  {"lavender"			, PALETTERGB (230,230,250)},
  {"Gray91"			, PALETTERGB (232,232,232)},
  {"Grey91"			, PALETTERGB (232,232,232)},
  {"DarkSalmon"			, PALETTERGB (233,150,122)},
  {"Salmon"			, PALETTERGB (233,150,122)},
  {"Gray92"			, PALETTERGB (235,235,235)},
  {"Grey92"			, PALETTERGB (235,235,235)},
  {"Gray93"			, PALETTERGB (237,237,237)},
  {"Grey93"			, PALETTERGB (237,237,237)},
  {"red2"			, PALETTERGB (238,  0,  0)},
  {"magenta2"			, PALETTERGB (238,  0,238)},
  {"DeepPink2"			, PALETTERGB (238, 18,137)},
  {"firebrick2"			, PALETTERGB (238, 44, 44)},
  {"maroon2"			, PALETTERGB (238, 48,167)},
  {"VioletRed2"			, PALETTERGB (238, 58,140)},
  {"brown2"			, PALETTERGB (238, 59, 59)},
  {"OrangeRed2"			, PALETTERGB (238, 64,  0)},
  {"tomato2"			, PALETTERGB (238, 92, 66)},
  {"IndianRed2"			, PALETTERGB (238, 99, 99)},
  {"coral2"			, PALETTERGB (238,106, 80)},
  {"HotPink2"			, PALETTERGB (238,106,167)},
  {"DarkOrange2"			, PALETTERGB (238,118,  0)},
  {"chocolate2"			, PALETTERGB (238,118, 33)},
  {"sienna2"			, PALETTERGB (238,121, 66)},
  {"PaleVioletRed2"			, PALETTERGB (238,121,159)},
  {"orchid2"			, PALETTERGB (238,122,233)},
  {"salmon2"			, PALETTERGB (238,130, 98)},
  {"LightSalmon2"			, PALETTERGB (238,149,114)},
  {"orange2"			, PALETTERGB (238,154,  0)},
  {"tan2"			, PALETTERGB (238,154, 73)},
  {"LightPink2"			, PALETTERGB (238,162,173)},
  {"pink2"			, PALETTERGB (238,169,184)},
  {"DarkGoldenrod2"			, PALETTERGB (238,173, 14)},
  {"plum2"			, PALETTERGB (238,174,238)},
  {"goldenrod2"			, PALETTERGB (238,180, 34)},
  {"RosyBrown2"			, PALETTERGB (238,180,180)},
  {"burlywood2"			, PALETTERGB (238,197,145)},
  {"gold2"			, PALETTERGB (238,201,  0)},
  {"PeachPuff2"			, PALETTERGB (238,203,173)},
  {"NavajoWhite2"			, PALETTERGB (238,207,161)},
  {"thistle2"			, PALETTERGB (238,210,238)},
  {"bisque2"			, PALETTERGB (238,213,183)},
  {"MistyRose2"			, PALETTERGB (238,213,210)},
  {"wheat2"			, PALETTERGB (238,216,174)},
  {"LightGoldenrod2"			, PALETTERGB (238,220,130)},
  {"LightGoldenrod"			, PALETTERGB (238,221,130)},
  {"AntiqueWhite2"			, PALETTERGB (238,223,204)},
  {"LavenderBlush2"			, PALETTERGB (238,224,229)},
  {"seashell2"			, PALETTERGB (238,229,222)},
  {"khaki2"			, PALETTERGB (238,230,133)},
  {"PaleGoldenrod"			, PALETTERGB (238,232,170)},
  {"cornsilk2"			, PALETTERGB (238,232,205)},
  {"LemonChiffon2"			, PALETTERGB (238,233,191)},
  {"snow2"			, PALETTERGB (238,233,233)},
  {"yellow2"			, PALETTERGB (238,238,  0)},
  {"LightYellow2"			, PALETTERGB (238,238,209)},
  {"ivory2"			, PALETTERGB (238,238,224)},
  {"LightCoral"			, PALETTERGB (240,128,128)},
  {"Gray94"			, PALETTERGB (240,240,240)},
  {"Grey94"			, PALETTERGB (240,240,240)},
  {"AliceBlue"			, PALETTERGB (240,248,255)},
  {"honeydew"			, PALETTERGB (240,255,240)},
  {"honeydew1"			, PALETTERGB (240,255,240)},
  {"azure"			, PALETTERGB (240,255,255)},
  {"azure1"			, PALETTERGB (240,255,255)},
  {"Gray95"			, PALETTERGB (242,242,242)},
  {"Grey95"			, PALETTERGB (242,242,242)},
  {"SandyBrown"			, PALETTERGB (244,164, 96)},
  {"Wheat"			, PALETTERGB (245,222,179)},
  {"beige"			, PALETTERGB (245,245,220)},
  {"Gray96"			, PALETTERGB (245,245,245)},
  {"Grey96"			, PALETTERGB (245,245,245)},
  {"WhiteSmoke"			, PALETTERGB (245,245,245)},
  {"MintCream"			, PALETTERGB (245,255,250)},
  {"Gray97"			, PALETTERGB (247,247,247)},
  {"Grey97"			, PALETTERGB (247,247,247)},
  {"GhostWhite"			, PALETTERGB (248,248,255)},
  {"AntiqueWhite"			, PALETTERGB (250,235,215)},
  {"linen"			, PALETTERGB (250,240,230)},
  {"LightGoldenrodYellow"			, PALETTERGB (250,250,210)},
  {"Gray98"			, PALETTERGB (250,250,250)},
  {"Grey98"			, PALETTERGB (250,250,250)},
  {"Gray99"			, PALETTERGB (252,252,252)},
  {"Grey99"			, PALETTERGB (252,252,252)},
  {"OldLace"			, PALETTERGB (253,245,230)},
  {"Red"			, PALETTERGB (255,  0,  0)},
  {"red1"			, PALETTERGB (255,  0,  0)},
  {"Magenta"			, PALETTERGB (255,  0,255)},
  {"magenta1"			, PALETTERGB (255,  0,255)},
  {"DeepPink"			, PALETTERGB (255, 20,147)},
  {"DeepPink1"			, PALETTERGB (255, 20,147)},
  {"firebrick1"			, PALETTERGB (255, 48, 48)},
  {"maroon1"			, PALETTERGB (255, 52,179)},
  {"VioletRed1"			, PALETTERGB (255, 62,150)},
  {"brown1"			, PALETTERGB (255, 64, 64)},
  {"OrangeRed"			, PALETTERGB (255, 69,  0)},
  {"OrangeRed1"			, PALETTERGB (255, 69,  0)},
  {"tomato"			, PALETTERGB (255, 99, 71)},
  {"tomato1"			, PALETTERGB (255, 99, 71)},
  {"HotPink"			, PALETTERGB (255,105,180)},
  {"IndianRed1"			, PALETTERGB (255,106,106)},
  {"HotPink1"			, PALETTERGB (255,110,180)},
  {"Coral"			, PALETTERGB (255,114, 86)},
  {"coral1"			, PALETTERGB (255,114, 86)},
  {"DarkOrange1"			, PALETTERGB (255,127,  0)},
  {"chocolate1"			, PALETTERGB (255,127, 36)},
  {"sienna1"			, PALETTERGB (255,130, 71)},
  {"PaleVioletRed1"			, PALETTERGB (255,130,171)},
  {"orchid1"			, PALETTERGB (255,131,250)},
  {"Orange"			, PALETTERGB (255,135,  0)},
  {"DarkOrange"			, PALETTERGB (255,140,  0)},
  {"salmon1"			, PALETTERGB (255,140,105)},
  {"LightSalmon"			, PALETTERGB (255,160,122)},
  {"LightSalmon1"			, PALETTERGB (255,160,122)},
  {"orange1"			, PALETTERGB (255,165,  0)},
  {"tan1"			, PALETTERGB (255,165, 79)},
  {"LightPink1"			, PALETTERGB (255,174,185)},
  {"Pink"			, PALETTERGB (255,181,197)},
  {"pink1"			, PALETTERGB (255,181,197)},
  {"LightPink"			, PALETTERGB (255,182,193)},
  {"DarkGoldenrod1"			, PALETTERGB (255,185, 15)},
  {"plum1"			, PALETTERGB (255,187,255)},
  {"goldenrod1"			, PALETTERGB (255,193, 37)},
  {"RosyBrown1"			, PALETTERGB (255,193,193)},
  {"burlywood1"			, PALETTERGB (255,211,155)},
  {"gold1"			, PALETTERGB (255,215,  0)},
  {"PeachPuff"			, PALETTERGB (255,218,185)},
  {"PeachPuff1"			, PALETTERGB (255,218,185)},
  {"NavajoWhite"			, PALETTERGB (255,222,173)},
  {"NavajoWhite1"			, PALETTERGB (255,222,173)},
  {"thistle1"			, PALETTERGB (255,225,255)},
  {"moccasin"			, PALETTERGB (255,228,181)},
  {"bisque"			, PALETTERGB (255,228,196)},
  {"bisque1"			, PALETTERGB (255,228,196)},
  {"MistyRose"			, PALETTERGB (255,228,225)},
  {"MistyRose1"			, PALETTERGB (255,228,225)},
  {"wheat1"			, PALETTERGB (255,231,186)},
  {"BlanchedAlmond"			, PALETTERGB (255,235,205)},
  {"LightGoldenrod1"			, PALETTERGB (255,236,139)},
  {"PapayaWhip"			, PALETTERGB (255,239,213)},
  {"AntiqueWhite1"			, PALETTERGB (255,239,219)},
  {"LavenderBlush"			, PALETTERGB (255,240,245)},
  {"LavenderBlush1"			, PALETTERGB (255,240,245)},
  {"seashell"			, PALETTERGB (255,245,238)},
  {"seashell1"			, PALETTERGB (255,245,238)},
  {"khaki1"			, PALETTERGB (255,246,143)},
  {"cornsilk"			, PALETTERGB (255,248,220)},
  {"cornsilk1"			, PALETTERGB (255,248,220)},
  {"LemonChiffon"			, PALETTERGB (255,250,205)},
  {"LemonChiffon1"			, PALETTERGB (255,250,205)},
  {"FloralWhite"			, PALETTERGB (255,250,240)},
  {"snow"			, PALETTERGB (255,250,250)},
  {"snow1"			, PALETTERGB (255,250,250)},
  {"Yellow"			, PALETTERGB (255,255,  0)},
  {"yellow1"			, PALETTERGB (255,255,  0)},
  {"LightYellow"			, PALETTERGB (255,255,224)},
  {"LightYellow1"			, PALETTERGB (255,255,224)},
  {"ivory"			, PALETTERGB (255,255,240)},
  {"ivory1"			, PALETTERGB (255,255,240)},
  {"Gray100"			, PALETTERGB (255,255,255)},
  {"Grey100"			, PALETTERGB (255,255,255)},
  {"White"			, PALETTERGB (255,255,255)}
};

static int
hexval (char c) 
{
  if (c >= 'a' && c <= 'f')
    return c-'a' + 10;
  else if (c >= 'A' && c <= 'Z')
    return c-'A' + 10;
  else
    return c-'0';
}

    
static COLORREF
mswindows_string_to_color(CONST char *name)
{
  int i;
  unsigned int r, g, b;
  
  if (*name == '#')
  {
    /* numeric names look like "#RRGGBB" */
    /* XXX accept other formats? */
    if (strlen(name)!=7)
      return (-1);
    for (i=1; i<7; i++)
      {
	if (!isxdigit(name[i]))
	  return(-1);
      }
    r = hexval(name[1]) * 16 + hexval(name[2]);
    g = hexval(name[3]) * 16 + hexval(name[4]);
    b = hexval(name[5]) * 16 + hexval(name[6]);
    return (PALETTERGB (r, g, b));
  }
  else
  {
    for(i=0; i<(sizeof(mswindows_X_color_map)/sizeof(colormap_t)); i++)
      if (!stricmp(name, mswindows_X_color_map[i].name))
	return (mswindows_X_color_map[i].colorref);
  }
  return(-1);
}

static int
mswindows_initialize_color_instance (struct Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object device, Error_behavior errb)
{
  CONST char *extname;
  COLORREF color;

  GET_C_STRING_CTEXT_DATA_ALLOCA (name, extname);
  color = mswindows_string_to_color(extname);
  if (color != -1)
    {
      c->data = xnew (struct mswindows_color_instance_data);
      COLOR_INSTANCE_MSWINDOWS_COLOR (c) = color;
      COLOR_INSTANCE_MSWINDOWS_BRUSH (c) = CreateSolidBrush (color);
      return 1;
    }
  maybe_signal_simple_error ("unrecognized color", name, Qcolor, errb);
  return(0);
}

#if 0
static void
mswindows_mark_color_instance (struct Lisp_Color_Instance *c,
			 void (*markobj) (Lisp_Object))
{
}
#endif

static void
mswindows_print_color_instance (struct Lisp_Color_Instance *c,
			  Lisp_Object printcharfun,
			  int escapeflag)
{
  char buf[32];
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  sprintf (buf, " %06ld=(%04X,%04X,%04X)", color & 0xffffff,
	   GetRValue(color)<<8, GetGValue(color)<<8, GetBValue(color)<<8);
  write_c_string (buf, printcharfun);
}

static void
mswindows_finalize_color_instance (struct Lisp_Color_Instance *c)
{
  if (c->data)
    {
      DeleteObject (COLOR_INSTANCE_MSWINDOWS_BRUSH (c));
      xfree (c->data);
      c->data = 0;
    }
}

static int
mswindows_color_instance_equal (struct Lisp_Color_Instance *c1,
			  struct Lisp_Color_Instance *c2,
			  int depth)
{
  return (COLOR_INSTANCE_MSWINDOWS_COLOR(c1) == COLOR_INSTANCE_MSWINDOWS_COLOR(c2));
}

static unsigned long
mswindows_color_instance_hash (struct Lisp_Color_Instance *c, int depth)
{
  return LISP_HASH (COLOR_INSTANCE_MSWINDOWS_COLOR(c));
}

static Lisp_Object
mswindows_color_instance_rgb_components (struct Lisp_Color_Instance *c)
{
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  /* This used to say GetXValue(color)<<8, but that made, e.g. white
     show as (#xff00 #xff00 #xff00) instead of (#xffff #xffff #xffff).
     This slightly kludgier variant gives the expected results for
     black and white, while hopefully not introducing too much error.  */
  return list3 (make_int (GetRValue (color) * 257),
		make_int (GetGValue (color) * 257),
		make_int (GetBValue (color) * 257));
}

static int
mswindows_valid_color_name_p (struct device *d, Lisp_Object color)
{
  CONST char *extname;

  GET_C_STRING_CTEXT_DATA_ALLOCA (color, extname);
  return (mswindows_string_to_color(extname)!=-1);
}



static void
mswindows_finalize_font_instance (struct Lisp_Font_Instance *f)
{
  if (f->data)
    {
      DeleteObject(f->data);
      f->data=0;
    }
}

static int
mswindows_initialize_font_instance (struct Lisp_Font_Instance *f, Lisp_Object name,
			      Lisp_Object device, Error_behavior errb)
{
  CONST char *extname;
  LOGFONT logfont;
  int fields;
  int pt;
  char fontname[LF_FACESIZE], weight[32], *style, points[8], effects[32], charset[32];

  GET_C_STRING_CTEXT_DATA_ALLOCA (f->name, extname);

  /*
   * mswindows fonts look like:
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
    if ((c=strchr(weight, ' ')))
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
      logfont.lfItalic = TRUE;
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
  logfont.lfHeight = -MulDiv(pt, DEVICE_MSWINDOWS_LOGPIXELSY(XDEVICE (device)), 72);
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
      mswindows_finalize_font_instance (f);
      maybe_signal_simple_error ("Couldn't map font", f->name, Qfont, errb);
      return 0;
    }
    GetTextMetrics(hdc, &metrics);
    SelectObject(hdc, holdfont);
    ReleaseDC(hwnd, hdc);
    f->width = (unsigned short) metrics.tmAveCharWidth;
    f->height = (unsigned short) metrics.tmHeight;
    f->ascent = (unsigned short) metrics.tmAscent;
    f->descent = (unsigned short) metrics.tmDescent;
    f->proportional_p = (metrics.tmPitchAndFamily & TMPF_FIXED_PITCH);
  }

  return 1;
}

#if 0
static void
mswindows_mark_font_instance (struct Lisp_Font_Instance *f,
			void (*markobj) (Lisp_Object))
{
}
#endif

static void
mswindows_print_font_instance (struct Lisp_Font_Instance *f,
			 Lisp_Object printcharfun,
			 int escapeflag)
{
}

static Lisp_Object
mswindows_list_fonts (Lisp_Object pattern, Lisp_Object device)
{
  /* XXX Implement me */
  return list1 (build_string ("Courier New:Regular:10"));
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_mswindows (void)
{
}

void
console_type_create_objects_mswindows (void)
{
  /* object methods */
  CONSOLE_HAS_METHOD (mswindows, initialize_color_instance);
/*  CONSOLE_HAS_METHOD (mswindows, mark_color_instance); */
  CONSOLE_HAS_METHOD (mswindows, print_color_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_color_instance);
  CONSOLE_HAS_METHOD (mswindows, color_instance_equal);
  CONSOLE_HAS_METHOD (mswindows, color_instance_hash);
  CONSOLE_HAS_METHOD (mswindows, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (mswindows, valid_color_name_p);

  CONSOLE_HAS_METHOD (mswindows, initialize_font_instance);
/*  CONSOLE_HAS_METHOD (mswindows, mark_font_instance); */
  CONSOLE_HAS_METHOD (mswindows, print_font_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_font_instance);
/*  CONSOLE_HAS_METHOD (mswindows, font_instance_truename); */
  CONSOLE_HAS_METHOD (mswindows, list_fonts);
#ifdef MULE
  CONSOLE_HAS_METHOD (mswindows, font_spec_matches_charset);
  CONSOLE_HAS_METHOD (mswindows, find_charset_font);
#endif
}

void
vars_of_objects_mswindows (void)
{
}
