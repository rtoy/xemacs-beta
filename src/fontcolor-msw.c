/* mswindows-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2004, 2005, 2010 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1997 Jonathan Harris.

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

/* Authorship:

   This file created by Jonathan Harris, November 1997 for 21.0; based
   heavily on fontcolor-x.c (see authorship there).  Much further work
   by Ben Wing.
 */

/* This function Mule-ized by Ben Wing, 3-24-02. */

/* TODO: palette handling */

#include <config.h>
#include "lisp.h"

#include "console-msw-impl.h"
#include "fontcolor-msw-impl.h"

#include "buffer.h"
#include "charset.h"
#include "device-impl.h"
#include "elhash.h"
#include "insdel.h"
#include "opaque.h"

typedef struct colormap_t
{
  const Ascbyte *name;
  COLORREF colorref;
} colormap_t;

/* Colors from X11R6 "XConsortium: rgb.txt,v 10.41 94/02/20 18:39:36 rws Exp" */
/* MSWindows tends to round up the numbers in its palette, ie where X uses
 * 127, MSWindows uses 128. Colors commented as "Adjusted" are tweaked to
 * match the Windows standard palette to increase the likelihood of
 * mswindows_color_to_string() finding a named match.

Sorted case-insensitively by the string name of the color.

 */
static const colormap_t mswindows_X_color_map[] =
{
  {"AliceBlue"		, PALETTERGB (240, 248, 255) },
  {"AntiqueWhite"	, PALETTERGB (250, 235, 215) },
  {"AntiqueWhite1"	, PALETTERGB (255, 239, 219) },
  {"AntiqueWhite2"	, PALETTERGB (238, 223, 204) },
  {"AntiqueWhite3"	, PALETTERGB (205, 192, 176) },
  {"AntiqueWhite4"	, PALETTERGB (139, 131, 120) },
  {"aquamarine"		, PALETTERGB (127, 255, 212) },
  {"aquamarine1"	, PALETTERGB (127, 255, 212) },
  {"aquamarine2"	, PALETTERGB (118, 238, 198) },
  {"aquamarine3"	, PALETTERGB (102, 205, 170) },
  {"aquamarine4"	, PALETTERGB (69, 139, 116) },
  {"azure"		, PALETTERGB (240, 255, 255) },
  {"azure1"		, PALETTERGB (240, 255, 255) },
  {"azure2"		, PALETTERGB (224, 238, 238) },
  {"azure3"		, PALETTERGB (193, 205, 205) },
  {"azure4"		, PALETTERGB (131, 139, 139) },
  {"beige"		, PALETTERGB (245, 245, 220) },
  {"bisque"		, PALETTERGB (255, 228, 196) },
  {"bisque1"		, PALETTERGB (255, 228, 196) },
  {"bisque2"		, PALETTERGB (238, 213, 183) },
  {"bisque3"		, PALETTERGB (205, 183, 158) },
  {"bisque4"		, PALETTERGB (139, 125, 107) },
  {"black"		, PALETTERGB (0, 0, 0) },
  {"BlanchedAlmond"	, PALETTERGB (255, 235, 205) },
  {"blue"		, PALETTERGB (0, 0, 255) },
  {"blue1"		, PALETTERGB (0, 0, 255) },
  {"blue2"		, PALETTERGB (0, 0, 238) },
  {"blue3"		, PALETTERGB (0, 0, 205) },
  {"blue4"		, PALETTERGB (0, 0, 139) },
  {"BlueViolet"		, PALETTERGB (138, 43, 226) },
  {"brown"		, PALETTERGB (165, 42, 42) },
  {"brown1"		, PALETTERGB (255, 64, 64) },
  {"brown2"		, PALETTERGB (238, 59, 59) },
  {"brown3"		, PALETTERGB (205, 51, 51) },
  {"brown4"		, PALETTERGB (139, 35, 35) },
  {"burlywood"		, PALETTERGB (222, 184, 135) },
  {"burlywood1"		, PALETTERGB (255, 211, 155) },
  {"burlywood2"		, PALETTERGB (238, 197, 145) },
  {"burlywood3"		, PALETTERGB (205, 170, 125) },
  {"burlywood4"		, PALETTERGB (139, 115, 85) },
  {"CadetBlue"		, PALETTERGB (95, 158, 160) },
  {"CadetBlue1"		, PALETTERGB (152, 245, 255) },
  {"CadetBlue2"		, PALETTERGB (144, 220, 240) },	/* Adjusted */
  {"CadetBlue3"		, PALETTERGB (122, 197, 205) },
  {"CadetBlue4"		, PALETTERGB (83, 134, 139) },
  {"chartreuse"		, PALETTERGB (127, 255, 0) },
  {"chartreuse1"	, PALETTERGB (127, 255, 0) },
  {"chartreuse2"	, PALETTERGB (118, 238, 0) },
  {"chartreuse3"	, PALETTERGB (102, 205, 0) },
  {"chartreuse4"	, PALETTERGB (69, 139, 0) },
  {"chocolate"		, PALETTERGB (210, 105, 30) },
  {"chocolate1"		, PALETTERGB (255, 127, 36) },
  {"chocolate2"		, PALETTERGB (238, 118, 33) },
  {"chocolate3"		, PALETTERGB (205, 102, 29) },
  {"chocolate4"		, PALETTERGB (139, 69, 19) },
  {"coral"		, PALETTERGB (255, 127, 80) },
  {"coral1"		, PALETTERGB (255, 114, 86) },
  {"coral2"		, PALETTERGB (238, 106, 80) },
  {"coral3"		, PALETTERGB (205, 91, 69) },
  {"coral4"		, PALETTERGB (139, 62, 47) },
  {"CornflowerBlue"	, PALETTERGB (100, 149, 237) },
  {"cornsilk"		, PALETTERGB (255, 248, 220) },
  {"cornsilk1"		, PALETTERGB (255, 248, 220) },
  {"cornsilk2"		, PALETTERGB (238, 232, 205) },
  {"cornsilk3"		, PALETTERGB (205, 200, 177) },
  {"cornsilk4"		, PALETTERGB (139, 136, 120) },
  {"cyan"		, PALETTERGB (0, 255, 255) },
  {"cyan1"		, PALETTERGB (0, 255, 255) },
  {"cyan2"		, PALETTERGB (0, 238, 238) },
  {"cyan3"		, PALETTERGB (0, 205, 205) },
  {"cyan4"		, PALETTERGB (0, 139, 139) },
  {"DarkBlue"		, PALETTERGB (0, 0, 128) },	/* Adjusted == Navy */
  {"DarkCyan"		, PALETTERGB (0, 128, 128) },	/* Adjusted */
  {"DarkGoldenrod"	, PALETTERGB (184, 134, 11) },
  {"DarkGoldenrod1"	, PALETTERGB (255, 185, 15) },
  {"DarkGoldenrod2"	, PALETTERGB (238, 173, 14) },
  {"DarkGoldenrod3"	, PALETTERGB (205, 149, 12) },
  {"DarkGoldenrod4"	, PALETTERGB (139, 101, 8) },
  {"DarkGray"		, PALETTERGB (169, 169, 169) },
  {"DarkGreen"		, PALETTERGB (0, 128, 0) },	/* Adjusted */
  {"DarkGrey"		, PALETTERGB (169, 169, 169) },
  {"DarkKhaki"		, PALETTERGB (189, 183, 107) },
  {"DarkMagenta"	, PALETTERGB (128, 0, 128) },	/* Adjusted */
  {"DarkOliveGreen"	, PALETTERGB (85, 107, 47) },
  {"DarkOliveGreen1"	, PALETTERGB (202, 255, 112) },
  {"DarkOliveGreen2"	, PALETTERGB (188, 238, 104) },
  {"DarkOliveGreen3"	, PALETTERGB (162, 205, 90) },
  {"DarkOliveGreen4"	, PALETTERGB (110, 139, 61) },
  {"DarkOrange"		, PALETTERGB (255, 140, 0) },
  {"DarkOrange1"	, PALETTERGB (255, 127, 0) },
  {"DarkOrange2"	, PALETTERGB (238, 118, 0) },
  {"DarkOrange3"	, PALETTERGB (205, 102, 0) },
  {"DarkOrange4"	, PALETTERGB (139, 69, 0) },
  {"DarkOrchid"		, PALETTERGB (153, 50, 204) },
  {"DarkOrchid1"	, PALETTERGB (191, 62, 255) },
  {"DarkOrchid2"	, PALETTERGB (178, 58, 238) },
  {"DarkOrchid3"	, PALETTERGB (154, 50, 205) },
  {"DarkOrchid4"	, PALETTERGB (104, 34, 139) },
  {"DarkRed"		, PALETTERGB (128, 0, 0) },	/* Adjusted */
  {"DarkSalmon"		, PALETTERGB (233, 150, 122) },
  {"DarkSeaGreen"	, PALETTERGB (143, 188, 143) },
  {"DarkSeaGreen1"	, PALETTERGB (193, 255, 193) },
  {"DarkSeaGreen2"	, PALETTERGB (180, 238, 180) },
  {"DarkSeaGreen3"	, PALETTERGB (155, 205, 155) },
  {"DarkSeaGreen4"	, PALETTERGB (105, 139, 105) },
  {"DarkSlateBlue"	, PALETTERGB (72, 61, 139) },
  {"DarkSlateGray"	, PALETTERGB (47, 79, 79) },
  {"DarkSlateGray1"	, PALETTERGB (151, 255, 255) },
  {"DarkSlateGray2"	, PALETTERGB (141, 238, 238) },
  {"DarkSlateGray3"	, PALETTERGB (121, 205, 205) },
  {"DarkSlateGray4"	, PALETTERGB (82, 139, 139) },
  {"DarkSlateGrey"	, PALETTERGB (47, 79, 79) },
  {"DarkTurquoise"	, PALETTERGB (0, 206, 209) },
  {"DarkViolet"		, PALETTERGB (148, 0, 211) },
  {"DarkYellow"		, PALETTERGB (128, 128, 0) },
  {"DeepPink"		, PALETTERGB (255, 20, 147) },
  {"DeepPink1"		, PALETTERGB (255, 20, 147) },
  {"DeepPink2"		, PALETTERGB (238, 18, 137) },
  {"DeepPink3"		, PALETTERGB (205, 16, 118) },
  {"DeepPink4"		, PALETTERGB (139, 10, 80) },
  {"DeepSkyBlue"	, PALETTERGB (0, 191, 255) },
  {"DeepSkyBlue1"	, PALETTERGB (0, 191, 255) },
  {"DeepSkyBlue2"	, PALETTERGB (0, 178, 238) },
  {"DeepSkyBlue3"	, PALETTERGB (0, 154, 205) },
  {"DeepSkyBlue4"	, PALETTERGB (0, 104, 139) },
  {"DimGray"		, PALETTERGB (105, 105, 105) },
  {"DimGrey"		, PALETTERGB (105, 105, 105) },
  {"DodgerBlue"		, PALETTERGB (30, 144, 255) },
  {"DodgerBlue1"	, PALETTERGB (30, 144, 255) },
  {"DodgerBlue2"	, PALETTERGB (28, 134, 238) },
  {"DodgerBlue3"	, PALETTERGB (24, 116, 205) },
  {"DodgerBlue4"	, PALETTERGB (16, 78, 139) },
  {"firebrick"		, PALETTERGB (178, 34, 34) },
  {"firebrick1"		, PALETTERGB (255, 48, 48) },
  {"firebrick2"		, PALETTERGB (238, 44, 44) },
  {"firebrick3"		, PALETTERGB (205, 38, 38) },
  {"firebrick4"		, PALETTERGB (139, 26, 26) },
  {"FloralWhite"	, PALETTERGB (255, 250, 240) },
  {"ForestGreen"	, PALETTERGB (34, 139, 34) },
  {"gainsboro"		, PALETTERGB (220, 220, 220) },
  {"GhostWhite"		, PALETTERGB (248, 248, 255) },
  {"gold"		, PALETTERGB (255, 215, 0) },
  {"gold1"		, PALETTERGB (255, 215, 0) },
  {"gold2"		, PALETTERGB (238, 201, 0) },
  {"gold3"		, PALETTERGB (205, 173, 0) },
  {"gold4"		, PALETTERGB (139, 117, 0) },
  {"goldenrod"		, PALETTERGB (218, 165, 32) },
  {"goldenrod1"		, PALETTERGB (255, 193, 37) },
  {"goldenrod2"		, PALETTERGB (238, 180, 34) },
  {"goldenrod3"		, PALETTERGB (205, 155, 29) },
  {"goldenrod4"		, PALETTERGB (139, 105, 20) },
  {"gray"		, PALETTERGB (190, 190, 190) },
  {"gray0"		, PALETTERGB (0, 0, 0) },
  {"gray1"		, PALETTERGB (3, 3, 3) },
  {"gray10"		, PALETTERGB (26, 26, 26) },
  {"gray100"		, PALETTERGB (255, 255, 255) },
  {"gray11"		, PALETTERGB (28, 28, 28) },
  {"gray12"		, PALETTERGB (31, 31, 31) },
  {"gray13"		, PALETTERGB (33, 33, 33) },
  {"gray14"		, PALETTERGB (36, 36, 36) },
  {"gray15"		, PALETTERGB (38, 38, 38) },
  {"gray16"		, PALETTERGB (41, 41, 41) },
  {"gray17"		, PALETTERGB (43, 43, 43) },
  {"gray18"		, PALETTERGB (46, 46, 46) },
  {"gray19"		, PALETTERGB (48, 48, 48) },
  {"gray2"		, PALETTERGB (5, 5, 5) },
  {"gray20"		, PALETTERGB (51, 51, 51) },
  {"gray21"		, PALETTERGB (54, 54, 54) },
  {"gray22"		, PALETTERGB (56, 56, 56) },
  {"gray23"		, PALETTERGB (59, 59, 59) },
  {"gray24"		, PALETTERGB (61, 61, 61) },
  {"gray25"		, PALETTERGB (64, 64, 64) },
  {"gray26"		, PALETTERGB (66, 66, 66) },
  {"gray27"		, PALETTERGB (69, 69, 69) },
  {"gray28"		, PALETTERGB (71, 71, 71) },
  {"gray29"		, PALETTERGB (74, 74, 74) },
  {"gray3"		, PALETTERGB (8, 8, 8) },
  {"gray30"		, PALETTERGB (77, 77, 77) },
  {"gray31"		, PALETTERGB (79, 79, 79) },
  {"gray32"		, PALETTERGB (82, 82, 82) },
  {"gray33"		, PALETTERGB (84, 84, 84) },
  {"gray34"		, PALETTERGB (87, 87, 87) },
  {"gray35"		, PALETTERGB (89, 89, 89) },
  {"gray36"		, PALETTERGB (92, 92, 92) },
  {"gray37"		, PALETTERGB (94, 94, 94) },
  {"gray38"		, PALETTERGB (97, 97, 97) },
  {"gray39"		, PALETTERGB (99, 99, 99) },
  {"gray4"		, PALETTERGB (10, 10, 10) },
  {"gray40"		, PALETTERGB (102, 102, 102) },
  {"gray41"		, PALETTERGB (105, 105, 105) },
  {"gray42"		, PALETTERGB (107, 107, 107) },
  {"gray43"		, PALETTERGB (110, 110, 110) },
  {"gray44"		, PALETTERGB (112, 112, 112) },
  {"gray45"		, PALETTERGB (115, 115, 115) },
  {"gray46"		, PALETTERGB (117, 117, 117) },
  {"gray47"		, PALETTERGB (120, 120, 120) },
  {"gray48"		, PALETTERGB (122, 122, 122) },
  {"gray49"		, PALETTERGB (125, 125, 125) },
  {"gray5"		, PALETTERGB (13, 13, 13) },
  {"gray50"		, PALETTERGB (128, 128, 128) },	/* Adjusted */
  {"gray51"		, PALETTERGB (130, 130, 130) },
  {"gray52"		, PALETTERGB (133, 133, 133) },
  {"gray53"		, PALETTERGB (135, 135, 135) },
  {"gray54"		, PALETTERGB (138, 138, 138) },
  {"gray55"		, PALETTERGB (140, 140, 140) },
  {"gray56"		, PALETTERGB (143, 143, 143) },
  {"gray57"		, PALETTERGB (145, 145, 145) },
  {"gray58"		, PALETTERGB (148, 148, 148) },
  {"gray59"		, PALETTERGB (150, 150, 150) },
  {"gray6"		, PALETTERGB (15, 15, 15) },
  {"gray60"		, PALETTERGB (153, 153, 153) },
  {"gray61"		, PALETTERGB (156, 156, 156) },
  {"gray62"		, PALETTERGB (158, 158, 158) },
  {"gray63"		, PALETTERGB (161, 161, 161) },
  {"gray64"		, PALETTERGB (163, 163, 163) },
  {"gray65"		, PALETTERGB (166, 166, 166) },
  {"gray66"		, PALETTERGB (168, 168, 168) },
  {"gray67"		, PALETTERGB (171, 171, 171) },
  {"gray68"		, PALETTERGB (173, 173, 173) },
  {"gray69"		, PALETTERGB (176, 176, 176) },
  {"gray7"		, PALETTERGB (18, 18, 18) },
  {"gray70"		, PALETTERGB (179, 179, 179) },
  {"gray71"		, PALETTERGB (181, 181, 181) },
  {"gray72"		, PALETTERGB (184, 184, 184) },
  {"gray73"		, PALETTERGB (186, 186, 186) },
  {"gray74"		, PALETTERGB (189, 189, 189) },
  {"gray75"		, PALETTERGB (192, 192, 192) },	/* Adjusted */
  {"gray76"		, PALETTERGB (194, 194, 194) },
  {"gray77"		, PALETTERGB (196, 196, 196) },
  {"gray78"		, PALETTERGB (199, 199, 199) },
  {"gray79"		, PALETTERGB (201, 201, 201) },
  {"gray8"		, PALETTERGB (20, 20, 20) },
  {"gray80"		, PALETTERGB (204, 204, 204) },
  {"gray81"		, PALETTERGB (207, 207, 207) },
  {"gray82"		, PALETTERGB (209, 209, 209) },
  {"gray83"		, PALETTERGB (212, 212, 212) },
  {"gray84"		, PALETTERGB (214, 214, 214) },
  {"gray85"		, PALETTERGB (217, 217, 217) },
  {"gray86"		, PALETTERGB (219, 219, 219) },
  {"gray87"		, PALETTERGB (222, 222, 222) },
  {"gray88"		, PALETTERGB (224, 224, 224) },
  {"gray89"		, PALETTERGB (227, 227, 227) },
  {"gray9"		, PALETTERGB (23, 23, 23) },
  {"gray90"		, PALETTERGB (229, 229, 229) },
  {"gray91"		, PALETTERGB (232, 232, 232) },
  {"gray92"		, PALETTERGB (235, 235, 235) },
  {"gray93"		, PALETTERGB (237, 237, 237) },
  {"gray94"		, PALETTERGB (240, 240, 240) },
  {"gray95"		, PALETTERGB (242, 242, 242) },
  {"gray96"		, PALETTERGB (245, 245, 245) },
  {"gray97"		, PALETTERGB (247, 247, 247) },
  {"gray98"		, PALETTERGB (250, 250, 250) },
  {"gray99"		, PALETTERGB (252, 252, 252) },
  {"green"		, PALETTERGB (0, 255, 0) },
  {"green1"		, PALETTERGB (0, 255, 0) },
  {"green2"		, PALETTERGB (0, 238, 0) },
  {"green3"		, PALETTERGB (0, 205, 0) },
  {"green4"		, PALETTERGB (0, 139, 0) },
  {"GreenYellow"	, PALETTERGB (173, 255, 47) },
  {"grey"		, PALETTERGB (190, 190, 190) },
  {"grey0"		, PALETTERGB (0, 0, 0) },
  {"grey1"		, PALETTERGB (3, 3, 3) },
  {"grey10"		, PALETTERGB (26, 26, 26) },
  {"grey100"		, PALETTERGB (255, 255, 255) },
  {"grey11"		, PALETTERGB (28, 28, 28) },
  {"grey12"		, PALETTERGB (31, 31, 31) },
  {"grey13"		, PALETTERGB (33, 33, 33) },
  {"grey14"		, PALETTERGB (36, 36, 36) },
  {"grey15"		, PALETTERGB (38, 38, 38) },
  {"grey16"		, PALETTERGB (41, 41, 41) },
  {"grey17"		, PALETTERGB (43, 43, 43) },
  {"grey18"		, PALETTERGB (46, 46, 46) },
  {"grey19"		, PALETTERGB (48, 48, 48) },
  {"grey2"		, PALETTERGB (5, 5, 5) },
  {"grey20"		, PALETTERGB (51, 51, 51) },
  {"grey21"		, PALETTERGB (54, 54, 54) },
  {"grey22"		, PALETTERGB (56, 56, 56) },
  {"grey23"		, PALETTERGB (59, 59, 59) },
  {"grey24"		, PALETTERGB (61, 61, 61) },
  {"grey25"		, PALETTERGB (64, 64, 64) },
  {"grey26"		, PALETTERGB (66, 66, 66) },
  {"grey27"		, PALETTERGB (69, 69, 69) },
  {"grey28"		, PALETTERGB (71, 71, 71) },
  {"grey29"		, PALETTERGB (74, 74, 74) },
  {"grey3"		, PALETTERGB (8, 8, 8) },
  {"grey30"		, PALETTERGB (77, 77, 77) },
  {"grey31"		, PALETTERGB (79, 79, 79) },
  {"grey32"		, PALETTERGB (82, 82, 82) },
  {"grey33"		, PALETTERGB (84, 84, 84) },
  {"grey34"		, PALETTERGB (87, 87, 87) },
  {"grey35"		, PALETTERGB (89, 89, 89) },
  {"grey36"		, PALETTERGB (92, 92, 92) },
  {"grey37"		, PALETTERGB (94, 94, 94) },
  {"grey38"		, PALETTERGB (97, 97, 97) },
  {"grey39"		, PALETTERGB (99, 99, 99) },
  {"grey4"		, PALETTERGB (10, 10, 10) },
  {"grey40"		, PALETTERGB (102, 102, 102) },
  {"grey41"		, PALETTERGB (105, 105, 105) },
  {"grey42"		, PALETTERGB (107, 107, 107) },
  {"grey43"		, PALETTERGB (110, 110, 110) },
  {"grey44"		, PALETTERGB (112, 112, 112) },
  {"grey45"		, PALETTERGB (115, 115, 115) },
  {"grey46"		, PALETTERGB (117, 117, 117) },
  {"grey47"		, PALETTERGB (120, 120, 120) },
  {"grey48"		, PALETTERGB (122, 122, 122) },
  {"grey49"		, PALETTERGB (125, 125, 125) },
  {"grey5"		, PALETTERGB (13, 13, 13) },
  {"grey50"		, PALETTERGB (128, 128, 128) },	/* Adjusted */
  {"grey51"		, PALETTERGB (130, 130, 130) },
  {"grey52"		, PALETTERGB (133, 133, 133) },
  {"grey53"		, PALETTERGB (135, 135, 135) },
  {"grey54"		, PALETTERGB (138, 138, 138) },
  {"grey55"		, PALETTERGB (140, 140, 140) },
  {"grey56"		, PALETTERGB (143, 143, 143) },
  {"grey57"		, PALETTERGB (145, 145, 145) },
  {"grey58"		, PALETTERGB (148, 148, 148) },
  {"grey59"		, PALETTERGB (150, 150, 150) },
  {"grey6"		, PALETTERGB (15, 15, 15) },
  {"grey60"		, PALETTERGB (153, 153, 153) },
  {"grey61"		, PALETTERGB (156, 156, 156) },
  {"grey62"		, PALETTERGB (158, 158, 158) },
  {"grey63"		, PALETTERGB (161, 161, 161) },
  {"grey64"		, PALETTERGB (163, 163, 163) },
  {"grey65"		, PALETTERGB (166, 166, 166) },
  {"grey66"		, PALETTERGB (168, 168, 168) },
  {"grey67"		, PALETTERGB (171, 171, 171) },
  {"grey68"		, PALETTERGB (173, 173, 173) },
  {"grey69"		, PALETTERGB (176, 176, 176) },
  {"grey7"		, PALETTERGB (18, 18, 18) },
  {"grey70"		, PALETTERGB (179, 179, 179) },
  {"grey71"		, PALETTERGB (181, 181, 181) },
  {"grey72"		, PALETTERGB (184, 184, 184) },
  {"grey73"		, PALETTERGB (186, 186, 186) },
  {"grey74"		, PALETTERGB (189, 189, 189) },
  {"grey75"		, PALETTERGB (192, 192, 192) },	/* Adjusted */
  {"grey76"		, PALETTERGB (194, 194, 194) },
  {"grey77"		, PALETTERGB (196, 196, 196) },
  {"grey78"		, PALETTERGB (199, 199, 199) },
  {"grey79"		, PALETTERGB (201, 201, 201) },
  {"grey8"		, PALETTERGB (20, 20, 20) },
  {"grey80"		, PALETTERGB (204, 204, 204) },
  {"grey81"		, PALETTERGB (207, 207, 207) },
  {"grey82"		, PALETTERGB (209, 209, 209) },
  {"grey83"		, PALETTERGB (212, 212, 212) },
  {"grey84"		, PALETTERGB (214, 214, 214) },
  {"grey85"		, PALETTERGB (217, 217, 217) },
  {"grey86"		, PALETTERGB (219, 219, 219) },
  {"grey87"		, PALETTERGB (222, 222, 222) },
  {"grey88"		, PALETTERGB (224, 224, 224) },
  {"grey89"		, PALETTERGB (227, 227, 227) },
  {"grey9"		, PALETTERGB (23, 23, 23) },
  {"grey90"		, PALETTERGB (229, 229, 229) },
  {"grey91"		, PALETTERGB (232, 232, 232) },
  {"grey92"		, PALETTERGB (235, 235, 235) },
  {"grey93"		, PALETTERGB (237, 237, 237) },
  {"grey94"		, PALETTERGB (240, 240, 240) },
  {"grey95"		, PALETTERGB (242, 242, 242) },
  {"grey96"		, PALETTERGB (245, 245, 245) },
  {"grey97"		, PALETTERGB (247, 247, 247) },
  {"grey98"		, PALETTERGB (250, 250, 250) },
  {"grey99"		, PALETTERGB (252, 252, 252) },
  {"honeydew"		, PALETTERGB (240, 255, 240) },
  {"honeydew1"		, PALETTERGB (240, 255, 240) },
  {"honeydew2"		, PALETTERGB (224, 238, 224) },
  {"honeydew3"		, PALETTERGB (193, 205, 193) },
  {"honeydew4"		, PALETTERGB (131, 139, 131) },
  {"HotPink"		, PALETTERGB (255, 105, 180) },
  {"HotPink1"		, PALETTERGB (255, 110, 180) },
  {"HotPink2"		, PALETTERGB (238, 106, 167) },
  {"HotPink3"		, PALETTERGB (205, 96, 144) },
  {"HotPink4"		, PALETTERGB (139, 58, 98) },
  {"IndianRed"		, PALETTERGB (205, 92, 92) },
  {"IndianRed1"		, PALETTERGB (255, 106, 106) },
  {"IndianRed2"		, PALETTERGB (238, 99, 99) },
  {"IndianRed3"		, PALETTERGB (205, 85, 85) },
  {"IndianRed4"		, PALETTERGB (139, 58, 58) },
  {"ivory"		, PALETTERGB (255, 255, 240) },
  {"ivory1"		, PALETTERGB (255, 255, 240) },
  {"ivory2"		, PALETTERGB (240, 240, 208) },	/* Adjusted */
  {"ivory3"		, PALETTERGB (205, 205, 193) },
  {"ivory4"		, PALETTERGB (139, 139, 131) },
  {"khaki"		, PALETTERGB (240, 230, 140) },
  {"khaki1"		, PALETTERGB (255, 246, 143) },
  {"khaki2"		, PALETTERGB (238, 230, 133) },
  {"khaki3"		, PALETTERGB (205, 198, 115) },
  {"khaki4"		, PALETTERGB (139, 134, 78) },
  {"lavender"		, PALETTERGB (230, 230, 250) },
  {"LavenderBlush"	, PALETTERGB (255, 240, 245) },
  {"LavenderBlush1"	, PALETTERGB (255, 240, 245) },
  {"LavenderBlush2"	, PALETTERGB (238, 224, 229) },
  {"LavenderBlush3"	, PALETTERGB (205, 193, 197) },
  {"LavenderBlush4"	, PALETTERGB (139, 131, 134) },
  {"LawnGreen"		, PALETTERGB (124, 252, 0) },
  {"LemonChiffon"	, PALETTERGB (255, 250, 205) },
  {"LemonChiffon1"	, PALETTERGB (255, 250, 205) },
  {"LemonChiffon2"	, PALETTERGB (238, 233, 191) },
  {"LemonChiffon3"	, PALETTERGB (205, 201, 165) },
  {"LemonChiffon4"	, PALETTERGB (139, 137, 112) },
  {"LightBlue"		, PALETTERGB (173, 216, 230) },
  {"LightBlue1"		, PALETTERGB (191, 239, 255) },
  {"LightBlue2"		, PALETTERGB (178, 223, 238) },
  {"LightBlue3"		, PALETTERGB (154, 192, 205) },
  {"LightBlue4"		, PALETTERGB (104, 131, 139) },
  {"LightCoral"		, PALETTERGB (240, 128, 128) },
  {"LightCyan"		, PALETTERGB (224, 255, 255) },
  {"LightCyan1"		, PALETTERGB (224, 255, 255) },
  {"LightCyan2"		, PALETTERGB (209, 238, 238) },
  {"LightCyan3"		, PALETTERGB (180, 205, 205) },
  {"LightCyan4"		, PALETTERGB (122, 139, 139) },
  {"LightGoldenrod"	, PALETTERGB (238, 221, 130) },
  {"LightGoldenrod1"	, PALETTERGB (255, 236, 139) },
  {"LightGoldenrod2"	, PALETTERGB (238, 220, 130) },
  {"LightGoldenrod3"	, PALETTERGB (205, 190, 112) },
  {"LightGoldenrod4"	, PALETTERGB (139, 129, 76) },
  {"LightGoldenrodYellow", PALETTERGB (250, 250, 210) },
  {"LightGray"		, PALETTERGB (211, 211, 211) },
  {"LightGreen"		, PALETTERGB (144, 238, 144) },
  {"LightGrey"		, PALETTERGB (211, 211, 211) },
  {"LightPink"		, PALETTERGB (255, 182, 193) },
  {"LightPink1"		, PALETTERGB (255, 174, 185) },
  {"LightPink2"		, PALETTERGB (238, 162, 173) },
  {"LightPink3"		, PALETTERGB (205, 140, 149) },
  {"LightPink4"		, PALETTERGB (139, 95, 101) },
  {"LightSalmon"	, PALETTERGB (255, 160, 122) },
  {"LightSalmon1"	, PALETTERGB (255, 160, 122) },
  {"LightSalmon2"	, PALETTERGB (238, 149, 114) },
  {"LightSalmon3"	, PALETTERGB (205, 129, 98) },
  {"LightSalmon4"	, PALETTERGB (139, 87, 66) },
  {"LightSeaGreen"	, PALETTERGB (32, 178, 170) },
  {"LightSkyBlue"	, PALETTERGB (135, 206, 250) },
  {"LightSkyBlue1"	, PALETTERGB (176, 226, 255) },
  {"LightSkyBlue2"	, PALETTERGB (164, 211, 238) },
  {"LightSkyBlue3"	, PALETTERGB (141, 182, 205) },
  {"LightSkyBlue4"	, PALETTERGB (96, 123, 139) },
  {"LightSlateBlue"	, PALETTERGB (132, 112, 255) },
  {"LightSlateGray"	, PALETTERGB (119, 136, 153) },
  {"LightSlateGrey"	, PALETTERGB (119, 136, 153) },
  {"LightSteelBlue"	, PALETTERGB (176, 196, 222) },
  {"LightSteelBlue1"	, PALETTERGB (202, 225, 255) },
  {"LightSteelBlue2"	, PALETTERGB (188, 210, 238) },
  {"LightSteelBlue3"	, PALETTERGB (162, 181, 205) },
  {"LightSteelBlue4"	, PALETTERGB (110, 123, 139) },
  {"LightYellow"	, PALETTERGB (255, 255, 224) },
  {"LightYellow"	, PALETTERGB (255, 255, 225) },	/* Adjusted */
  {"LightYellow1"	, PALETTERGB (255, 255, 224) },
  {"LightYellow2"	, PALETTERGB (238, 238, 209) },
  {"LightYellow3"	, PALETTERGB (205, 205, 180) },
  {"LightYellow4"	, PALETTERGB (139, 139, 122) },
  {"LimeGreen"		, PALETTERGB (50, 205, 50) },
  {"linen"		, PALETTERGB (250, 240, 230) },
  {"magenta"		, PALETTERGB (255, 0, 255) },
  {"magenta1"		, PALETTERGB (255, 0, 255) },
  {"magenta2"		, PALETTERGB (238, 0, 238) },
  {"magenta3"		, PALETTERGB (205, 0, 205) },
  {"magenta4"		, PALETTERGB (139, 0, 139) },
  {"maroon"		, PALETTERGB (176, 48, 96) },
  {"maroon1"		, PALETTERGB (255, 52, 179) },
  {"maroon2"		, PALETTERGB (238, 48, 167) },
  {"maroon3"		, PALETTERGB (205, 41, 144) },
  {"maroon4"		, PALETTERGB (139, 28, 98) },
  {"MediumAquamarine"	, PALETTERGB (102, 205, 170) },
  {"MediumBlue"		, PALETTERGB (0, 0, 205) },
  {"MediumOrchid"	, PALETTERGB (186, 85, 211) },
  {"MediumOrchid1"	, PALETTERGB (224, 102, 255) },
  {"MediumOrchid2"	, PALETTERGB (209, 95, 238) },
  {"MediumOrchid3"	, PALETTERGB (180, 82, 205) },
  {"MediumOrchid4"	, PALETTERGB (122, 55, 139) },
  {"MediumPurple"	, PALETTERGB (147, 112, 219) },
  {"MediumPurple1"	, PALETTERGB (171, 130, 255) },
  {"MediumPurple2"	, PALETTERGB (159, 121, 238) },
  {"MediumPurple3"	, PALETTERGB (137, 104, 205) },
  {"MediumPurple4"	, PALETTERGB (93, 71, 139) },
  {"MediumSeaGreen"	, PALETTERGB (60, 179, 113) },
  {"MediumSlateBlue"	, PALETTERGB (123, 104, 238) },
  {"MediumSpringGreen"	, PALETTERGB (0, 250, 154) },
  {"MediumTurquoise"	, PALETTERGB (72, 209, 204) },
  {"MediumVioletRed"	, PALETTERGB (199, 21, 133) },
  {"MidnightBlue"	, PALETTERGB (25, 25, 112) },
  {"MintCream"		, PALETTERGB (245, 255, 250) },
  {"MistyRose"		, PALETTERGB (255, 228, 225) },
  {"MistyRose1"		, PALETTERGB (255, 228, 225) },
  {"MistyRose2"		, PALETTERGB (238, 213, 210) },
  {"MistyRose3"		, PALETTERGB (205, 183, 181) },
  {"MistyRose4"		, PALETTERGB (139, 125, 123) },
  {"moccasin"		, PALETTERGB (255, 228, 181) },
  {"NavajoWhite"	, PALETTERGB (255, 222, 173) },
  {"NavajoWhite1"	, PALETTERGB (255, 222, 173) },
  {"NavajoWhite2"	, PALETTERGB (238, 207, 161) },
  {"NavajoWhite3"	, PALETTERGB (205, 179, 139) },
  {"NavajoWhite4"	, PALETTERGB (139, 121, 94) },
  {"navy"		, PALETTERGB (0, 0, 128) },
  {"NavyBlue"		, PALETTERGB (0, 0, 128) },
  {"OldLace"		, PALETTERGB (253, 245, 230) },
  {"OliveDrab"		, PALETTERGB (107, 142, 35) },
  {"OliveDrab1"		, PALETTERGB (192, 255, 62) },
  {"OliveDrab2"		, PALETTERGB (179, 238, 58) },
  {"OliveDrab3"		, PALETTERGB (154, 205, 50) },
  {"OliveDrab4"		, PALETTERGB (105, 139, 34) },
  {"orange"		, PALETTERGB (255, 165, 0) },
  {"orange1"		, PALETTERGB (255, 165, 0) },
  {"orange2"		, PALETTERGB (238, 154, 0) },
  {"orange3"		, PALETTERGB (205, 133, 0) },
  {"orange4"		, PALETTERGB (139, 90, 0) },
  {"OrangeRed"		, PALETTERGB (255, 69, 0) },
  {"OrangeRed1"		, PALETTERGB (255, 69, 0) },
  {"OrangeRed2"		, PALETTERGB (238, 64, 0) },
  {"OrangeRed3"		, PALETTERGB (205, 55, 0) },
  {"OrangeRed4"		, PALETTERGB (139, 37, 0) },
  {"orchid"		, PALETTERGB (218, 112, 214) },
  {"orchid1"		, PALETTERGB (255, 131, 250) },
  {"orchid2"		, PALETTERGB (238, 122, 233) },
  {"orchid3"		, PALETTERGB (205, 105, 201) },
  {"orchid4"		, PALETTERGB (139, 71, 137) },
  {"PaleGoldenrod"	, PALETTERGB (238, 232, 170) },
  {"PaleGreen"		, PALETTERGB (152, 251, 152) },
  {"PaleGreen1"		, PALETTERGB (154, 255, 154) },
  {"PaleGreen2"		, PALETTERGB (144, 238, 144) },
  {"PaleGreen3"		, PALETTERGB (124, 205, 124) },
  {"PaleGreen4"		, PALETTERGB (84, 139, 84) },
  {"PaleTurquoise"	, PALETTERGB (175, 238, 238) },
  {"PaleTurquoise1"	, PALETTERGB (187, 255, 255) },
  {"PaleTurquoise2"	, PALETTERGB (174, 238, 238) },
  {"PaleTurquoise3"	, PALETTERGB (150, 205, 205) },
  {"PaleTurquoise4"	, PALETTERGB (102, 139, 139) },
  {"PaleVioletRed"	, PALETTERGB (219, 112, 147) },
  {"PaleVioletRed1"	, PALETTERGB (255, 130, 171) },
  {"PaleVioletRed2"	, PALETTERGB (238, 121, 159) },
  {"PaleVioletRed3"	, PALETTERGB (205, 104, 137) },
  {"PaleVioletRed4"	, PALETTERGB (139, 71, 93) },
  {"PaleYellow"		, PALETTERGB (255, 255, 128) },
  {"PapayaWhip"		, PALETTERGB (255, 239, 213) },
  {"PeachPuff"		, PALETTERGB (255, 218, 185) },
  {"PeachPuff1"		, PALETTERGB (255, 218, 185) },
  {"PeachPuff2"		, PALETTERGB (238, 203, 173) },
  {"PeachPuff3"		, PALETTERGB (205, 175, 149) },
  {"PeachPuff4"		, PALETTERGB (139, 119, 101) },
  {"peru"		, PALETTERGB (205, 133, 63) },
  {"pink"		, PALETTERGB (255, 192, 203) },
  {"pink1"		, PALETTERGB (255, 181, 197) },
  {"pink2"		, PALETTERGB (238, 169, 184) },
  {"pink3"		, PALETTERGB (205, 145, 158) },
  {"pink4"		, PALETTERGB (139, 99, 108) },
  {"plum"		, PALETTERGB (221, 160, 221) },
  {"plum1"		, PALETTERGB (255, 187, 255) },
  {"plum2"		, PALETTERGB (238, 174, 238) },
  {"plum3"		, PALETTERGB (205, 150, 205) },
  {"plum4"		, PALETTERGB (139, 102, 139) },
  {"PowderBlue"		, PALETTERGB (176, 224, 230) },
  {"purple"		, PALETTERGB (160, 32, 240) },
  {"purple1"		, PALETTERGB (155, 48, 255) },
  {"purple2"		, PALETTERGB (145, 44, 238) },
  {"purple3"		, PALETTERGB (125, 38, 205) },
  {"purple4"		, PALETTERGB (85, 26, 139) },
  {"red"		, PALETTERGB (255, 0, 0) },
  {"red1"		, PALETTERGB (255, 0, 0) },
  {"red2"		, PALETTERGB (238, 0, 0) },
  {"red3"		, PALETTERGB (205, 0, 0) },
  {"red4"		, PALETTERGB (139, 0, 0) },
  {"RosyBrown"		, PALETTERGB (188, 143, 143) },
  {"RosyBrown1"		, PALETTERGB (255, 193, 193) },
  {"RosyBrown2"		, PALETTERGB (238, 180, 180) },
  {"RosyBrown3"		, PALETTERGB (205, 155, 155) },
  {"RosyBrown4"		, PALETTERGB (139, 105, 105) },
  {"RoyalBlue"		, PALETTERGB (65, 105, 225) },
  {"RoyalBlue1"		, PALETTERGB (72, 118, 255) },
  {"RoyalBlue2"		, PALETTERGB (67, 110, 238) },
  {"RoyalBlue3"		, PALETTERGB (58, 95, 205) },
  {"RoyalBlue4"		, PALETTERGB (39, 64, 139) },
  {"SaddleBrown"	, PALETTERGB (139, 69, 19) },
  {"salmon"		, PALETTERGB (250, 128, 114) },
  {"salmon1"		, PALETTERGB (255, 140, 105) },
  {"salmon2"		, PALETTERGB (238, 130, 98) },
  {"salmon3"		, PALETTERGB (205, 112, 84) },
  {"salmon4"		, PALETTERGB (139, 76, 57) },
  {"SandyBrown"		, PALETTERGB (244, 164, 96) },
  {"SeaGreen"		, PALETTERGB (46, 139, 87) },
  {"SeaGreen1"		, PALETTERGB (84, 255, 159) },
  {"SeaGreen2"		, PALETTERGB (78, 238, 148) },
  {"SeaGreen3"		, PALETTERGB (67, 205, 128) },
  {"SeaGreen4"		, PALETTERGB (46, 139, 87) },
  {"seashell"		, PALETTERGB (255, 245, 238) },
  {"seashell1"		, PALETTERGB (255, 245, 238) },
  {"seashell2"		, PALETTERGB (238, 229, 222) },
  {"seashell3"		, PALETTERGB (205, 197, 191) },
  {"seashell4"		, PALETTERGB (139, 134, 130) },
  {"sienna"		, PALETTERGB (160, 82, 45) },
  {"sienna1"		, PALETTERGB (255, 130, 71) },
  {"sienna2"		, PALETTERGB (238, 121, 66) },
  {"sienna3"		, PALETTERGB (205, 104, 57) },
  {"sienna4"		, PALETTERGB (139, 71, 38) },
  {"SkyBlue"		, PALETTERGB (135, 206, 235) },
  {"SkyBlue1"		, PALETTERGB (135, 206, 255) },
  {"SkyBlue2"		, PALETTERGB (126, 192, 238) },
  {"SkyBlue3"		, PALETTERGB (108, 166, 205) },
  {"SkyBlue4"		, PALETTERGB (74, 112, 139) },
  {"SlateBlue"		, PALETTERGB (106, 90, 205) },
  {"SlateBlue1"		, PALETTERGB (131, 111, 255) },
  {"SlateBlue2"		, PALETTERGB (122, 103, 238) },
  {"SlateBlue3"		, PALETTERGB (105, 89, 205) },
  {"SlateBlue4"		, PALETTERGB (71, 60, 139) },
  {"SlateGray"		, PALETTERGB (112, 128, 144) },
  {"SlateGray1"		, PALETTERGB (198, 226, 255) },
  {"SlateGray2"		, PALETTERGB (185, 211, 238) },
  {"SlateGray3"		, PALETTERGB (159, 182, 205) },
  {"SlateGray4"		, PALETTERGB (108, 123, 139) },
  {"SlateGrey"		, PALETTERGB (112, 128, 144) },
  {"snow"		, PALETTERGB (255, 250, 250) },
  {"snow1"		, PALETTERGB (255, 250, 250) },
  {"snow2"		, PALETTERGB (238, 233, 233) },
  {"snow3"		, PALETTERGB (205, 201, 201) },
  {"snow4"		, PALETTERGB (139, 137, 137) },
  {"SpringGreen"	, PALETTERGB (0, 255, 127) },
  {"SpringGreen1"	, PALETTERGB (0, 255, 127) },
  {"SpringGreen2"	, PALETTERGB (0, 238, 118) },
  {"SpringGreen3"	, PALETTERGB (0, 205, 102) },
  {"SpringGreen4"	, PALETTERGB (0, 139, 69) },
  {"SteelBlue"		, PALETTERGB (70, 130, 180) },
  {"SteelBlue1"		, PALETTERGB (99, 184, 255) },
  {"SteelBlue2"		, PALETTERGB (92, 172, 238) },
  {"SteelBlue3"		, PALETTERGB (79, 148, 205) },
  {"SteelBlue4"		, PALETTERGB (54, 100, 139) },
  {"tan"		, PALETTERGB (210, 180, 140) },
  {"tan1"		, PALETTERGB (255, 165, 79) },
  {"tan2"		, PALETTERGB (238, 154, 73) },
  {"tan3"		, PALETTERGB (205, 133, 63) },
  {"tan4"		, PALETTERGB (139, 90, 43) },
  {"thistle"		, PALETTERGB (216, 191, 216) },
  {"thistle1"		, PALETTERGB (255, 225, 255) },
  {"thistle2"		, PALETTERGB (238, 210, 238) },
  {"thistle3"		, PALETTERGB (205, 181, 205) },
  {"thistle4"		, PALETTERGB (139, 123, 139) },
  {"tomato"		, PALETTERGB (255, 99, 71) },
  {"tomato1"		, PALETTERGB (255, 99, 71) },
  {"tomato2"		, PALETTERGB (238, 92, 66) },
  {"tomato3"		, PALETTERGB (205, 79, 57) },
  {"tomato4"		, PALETTERGB (139, 54, 38) },
  {"turquoise"		, PALETTERGB (64, 224, 208) },
  {"turquoise1"		, PALETTERGB (0, 245, 255) },
  {"turquoise2"		, PALETTERGB (0, 229, 238) },
  {"turquoise3"		, PALETTERGB (0, 197, 205) },
  {"turquoise4"		, PALETTERGB (0, 134, 139) },
  {"violet"		, PALETTERGB (238, 130, 238) },
  {"VioletRed"		, PALETTERGB (208, 32, 144) },
  {"VioletRed1"		, PALETTERGB (255, 62, 150) },
  {"VioletRed2"		, PALETTERGB (238, 58, 140) },
  {"VioletRed3"		, PALETTERGB (205, 50, 120) },
  {"VioletRed4"		, PALETTERGB (139, 34, 82) },
  {"wheat"		, PALETTERGB (245, 222, 179) },
  {"wheat1"		, PALETTERGB (255, 231, 186) },
  {"wheat2"		, PALETTERGB (238, 216, 174) },
  {"wheat3"		, PALETTERGB (205, 186, 150) },
  {"wheat4"		, PALETTERGB (139, 126, 102) },
  {"white"		, PALETTERGB (255, 255, 255) },
  {"WhiteSmoke"		, PALETTERGB (245, 245, 245) },
  {"yellow"		, PALETTERGB (255, 255, 0) },
  {"yellow1"		, PALETTERGB (255, 255, 0) },
  {"yellow2"		, PALETTERGB (238, 238, 0) },
  {"yellow3"		, PALETTERGB (205, 205, 0) },
  {"yellow4"		, PALETTERGB (139, 139, 0) },
  {"YellowGreen"	, PALETTERGB (154, 205, 50) }
};


typedef struct fontmap_t
{
  const Ascbyte *name;
  int value;
} fontmap_t;

/* Default weight first, preferred names listed before synonyms */
static const fontmap_t fontweight_map[] =
{
  {"Regular"		, FW_REGULAR},	/* The standard font weight */
  {"Thin"		, FW_THIN},
  {"Extra Light"	, FW_EXTRALIGHT},
  {"Ultra Light"	, FW_ULTRALIGHT},
  {"Light"		, FW_LIGHT},
  {"Normal"		, FW_NORMAL},
  {"Medium"		, FW_MEDIUM},
  {"Semi Bold"		, FW_SEMIBOLD},
  {"Demi Bold"		, FW_DEMIBOLD},
  {"Bold"		, FW_BOLD},	/* The standard bold font weight */
  {"Extra Bold"		, FW_EXTRABOLD},
  {"Ultra Bold"		, FW_ULTRABOLD},
  {"Heavy"		, FW_HEAVY},
  {"Black"		, FW_BLACK}
};

/* Default charset must be listed first, no synonyms allowed because these
 * names are matched against the names reported by win32 by match_font() */
static const fontmap_t charset_map[] =
{
  {"Western"		, ANSI_CHARSET}, /* Latin 1 */
  {"Central European"	, EASTEUROPE_CHARSET},
  {"Cyrillic"		, RUSSIAN_CHARSET},
  {"Greek"		, GREEK_CHARSET},
  {"Turkish"		, TURKISH_CHARSET},
  {"Hebrew"		, HEBREW_CHARSET},
  {"Arabic"		, ARABIC_CHARSET},
  {"Baltic"		, BALTIC_CHARSET},
  {"Viet Nam"		, VIETNAMESE_CHARSET},
  {"Thai"		, THAI_CHARSET},
  {"Japanese"		, SHIFTJIS_CHARSET},
  {"Korean"		, HANGEUL_CHARSET},
  {"Simplified Chinese"	, GB2312_CHARSET},
  {"Traditional Chinese", CHINESEBIG5_CHARSET},

  {"Symbol"		, SYMBOL_CHARSET},
  {"Mac"		, MAC_CHARSET},
  {"Korean Johab"	, JOHAB_CHARSET},
  {"OEM/DOS"		, OEM_CHARSET}
};

#ifdef MULE

typedef struct unicode_subrange_raw_t
{
  int subrange_bit;
  int start; /* first Unicode codepoint */
  int end; /* last Unicode codepoint */
} unicode_subrange_raw_t;

/* This table comes from MSDN, Unicode Subset Bitfields [Platform SDK
   Documentation, Base Services, International Features, Unicode and
   Character Sets, Unicode and Character Set Reference, Unicode and
   Character Set Constants].  We preprocess it at startup time into an
   array of unicode_subrange_t.
   */

static const unicode_subrange_raw_t unicode_subrange_raw_map[] =
{
  {0, 0x0020, 0x007e}, /* Basic Latin */
  {1, 0x00a0, 0x00ff}, /* Latin-1 Supplement */
  {2, 0x0100, 0x017f}, /* Latin Extended-A */
  {3, 0x0180, 0x024f}, /* Latin Extended-B */
  {4, 0x0250, 0x02af}, /* IPA Extensions */
  {5, 0x02b0, 0x02ff}, /* Spacing Modifier Letters */
  {6, 0x0300, 0x036f}, /* Combining Diacritical Marks */
  {7, 0x0370, 0x03ff}, /* Basic Greek */
  /* 8  Reserved */
  {9, 0x0400, 0x04ff}, /* Cyrillic */
  {10, 0x0530, 0x058f}, /* Armenian */
  {11, 0x0590, 0x05ff}, /* Basic Hebrew */
  /* 12   Reserved */
  {13, 0x0600, 0x06ff}, /* Basic Arabic */
  /* 14   Reserved */
  {15, 0x0900, 0x097f}, /* Devanagari */
  {16, 0x0980, 0x09ff}, /* Bengali */
  {17, 0x0a00, 0x0a7f}, /* Gurmukhi */
  {18, 0x0a80, 0x0aff}, /* Gujarati */
  {19, 0x0b00, 0x0b7f}, /* Oriya */
  {20, 0x0b80, 0x0bff}, /* Tamil */
  {21, 0x0c00, 0x0c7f}, /* Telugu */
  {22, 0x0c80, 0x0cff}, /* Kannada */
  {23, 0x0d00, 0x0d7f}, /* Malayalam */
  {24, 0x0e00, 0x0e7f}, /* Thai */
  {25, 0x0e80, 0x0eff}, /* Lao */
  {26, 0x10a0, 0x10ff}, /* Basic Georgian */
  /* 27   Reserved */
  {28, 0x1100, 0x11ff}, /* Hangul Jamo */
  {29, 0x1e00, 0x1eff}, /* Latin Extended Additional */
  {30, 0x1f00, 0x1fff}, /* Greek Extended */
  {31, 0x2000, 0x206f}, /* General Punctuation */
  {32, 0x2070, 0x209f}, /* Subscripts and Superscripts */
  {33, 0x20a0, 0x20cf}, /* Currency Symbols */
  {34, 0x20d0, 0x20ff}, /* Combining Diacritical Marks for Symbols */
  {35, 0x2100, 0x214f}, /* Letter-like Symbols */
  {36, 0x2150, 0x218f}, /* Number Forms */
  {37, 0x2190, 0x21ff}, /* Arrows */
  {38, 0x2200, 0x22ff}, /* Mathematical Operators */
  {39, 0x2300, 0x23ff}, /* Miscellaneous Technical */
  {40, 0x2400, 0x243f}, /* Control Pictures */
  {41, 0x2440, 0x245f}, /* Optical Character Recognition */
  {42, 0x2460, 0x24ff}, /* Enclosed Alphanumerics */
  {43, 0x2500, 0x257f}, /* Box Drawing */
  {44, 0x2580, 0x259f}, /* Block Elements */
  {45, 0x25a0, 0x25ff}, /* Geometric Shapes */
  {46, 0x2600, 0x26ff}, /* Miscellaneous Symbols */
  {47, 0x2700, 0x27bf}, /* Dingbats */
  {48, 0x3000, 0x303f}, /* Chinese, Japanese, and Korean (CJK) Symbols and Punctuation */
  {49, 0x3040, 0x309f}, /* Hiragana */
  {50, 0x30a0, 0x30ff}, /* Katakana */
  {51, 0x3100, 0x312f}, /* Bopomofo */
  {51, 0x31a0, 0x31bf}, /* Extended Bopomofo */
  {52, 0x3130, 0x318f}, /* Hangul Compatibility Jamo */
  {53, 0x3190, 0x319f}, /* CJK Miscellaneous */
  {54, 0x3200, 0x32ff}, /* Enclosed CJK Letters and Months */
  {55, 0x3300, 0x33ff}, /* CJK Compatibility */
  {56, 0xac00, 0xd7a3}, /* Hangul */
  {57, 0xd800, 0xdfff}, /* Surrogates. Note that setting this bit implies that there is at least one codepoint beyond the Basic Multilingual Plane that is supported by this font.  */
  /* 58   Reserved */
  {59, 0x4e00, 0x9fff}, /* CJK Unified Ideographs */
  {59, 0x2e80, 0x2eff}, /* CJK Radicals Supplement */
  {59, 0x2f00, 0x2fdf}, /* Kangxi Radicals */
  {59, 0x2ff0, 0x2fff}, /* Ideographic Description */
  {59, 0x3400, 0x4dbf}, /* CJK Unified Ideograph Extension A */
  {60, 0xe000, 0xf8ff}, /* Private Use Area */
  {61, 0xf900, 0xfaff}, /* CJK Compatibility Ideographs */
  {62, 0xfb00, 0xfb4f}, /* Alphabetic Presentation Forms */
  {63, 0xfb50, 0xfdff}, /* Arabic Presentation Forms-A */
  {64, 0xfe20, 0xfe2f}, /* Combining Half Marks */
  {65, 0xfe30, 0xfe4f}, /* CJK Compatibility Forms */
  {66, 0xfe50, 0xfe6f}, /* Small Form Variants */
  {67, 0xfe70, 0xfefe}, /* Arabic Presentation Forms-B */
  {68, 0xff00, 0xffef}, /* Halfwidth and Fullwidth Forms */
  {69, 0xfff0, 0xfffd}, /* Specials */
  {70, 0x0f00, 0x0fcf}, /* Tibetan */
  {71, 0x0700, 0x074f}, /* Syriac */
  {72, 0x0780, 0x07bf}, /* Thaana */
  {73, 0x0d80, 0x0dff}, /* Sinhala */
  {74, 0x1000, 0x109f}, /* Myanmar */
  {75, 0x1200, 0x12bf}, /* Ethiopic */
  {76, 0x13a0, 0x13ff}, /* Cherokee */
  {77, 0x1400, 0x14df}, /* Canadian Aboriginal Syllabics */
  {78, 0x1680, 0x169f}, /* Ogham */
  {79, 0x16a0, 0x16ff}, /* Runic */
  {80, 0x1780, 0x17ff}, /* Khmer */
  {81, 0x1800, 0x18af}, /* Mongolian */
  {82, 0x2800, 0x28ff}, /* Braille */
  {83, 0xa000, 0xa48c}, /* Yi, Yi Radicals */
  /* 84-122   Reserved */
  /* 123   Windows 2000/XP: Layout progress: horizontal from right to left */
  /* 124   Windows 2000/XP: Layout progress: vertical before horizontal */
  /* 125   Windows 2000/XP: Layout progress: vertical bottom to top */
  /* 126   Reserved; must be 0 */
  /* 127   Reserved; must be 1 */
};

typedef struct unicode_subrange_t
{
  int no_subranges;
  const unicode_subrange_raw_t *subranges;
} unicode_subrange_t;

unicode_subrange_t *unicode_subrange_table;

/* Hash table mapping font specs (strings) to font signature data
   (FONTSIGNATURE structures stored in opaques), as determined by
   GetTextCharsetInfo().  I presume this is somewhat expensive because it
   involves creating a font object.  At the very least, with no hashing, it
   definitely took awhile (a few seconds) when encountering characters from
   charsets needing stage 2 processing. */
Lisp_Object Vfont_signature_data;

#endif /* MULE */


/************************************************************************/
/*                               helpers                                */
/************************************************************************/

static int
hexval (Ibyte c)
{
  /* assumes ASCII and isxdigit (c) */
  if (c >= 'a')
    return c - 'a' + 10;
  else if (c >= 'A')
    return c - 'A' + 10;
  else
    return c - '0';
}

static int
colormap_t_compare (const void *a, const void *b)
{
  return ascii_strcasecmp (((colormap_t *)a)->name,
                           ((colormap_t *)b)->name);
}

COLORREF
mswindows_string_to_color (const Ibyte *name)
{
  int i;

  if (*name == '#')
    {
      /* numeric names look like "#RRGGBB", "#RRRGGGBBB" or "#RRRRGGGGBBBB"
	 or "rgb:rrrr/gggg/bbbb" */
      unsigned int r, g, b;

      for (i = 1; i < qxestrlen (name); i++)
	{
	  if (!byte_ascii_p (name[i]) || !isxdigit ((int) name[i]))
	    return (COLORREF) -1;
	}
      if (qxestrlen (name) == 7)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[3]) * 16 + hexval (name[4]);
	  b = hexval (name[5]) * 16 + hexval (name[6]);
	  return (PALETTERGB (r, g, b));
	}
      else if (qxestrlen (name) == 10)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[4]) * 16 + hexval (name[5]);
	  b = hexval (name[7]) * 16 + hexval (name[8]);
	  return (PALETTERGB (r, g, b));
	}
      else if (qxestrlen (name) == 13)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[5]) * 16 + hexval (name[6]);
	  b = hexval (name[9]) * 16 + hexval (name[10]);
	  return (PALETTERGB (r, g, b));
	}
    }
  else if (!qxestrncmp_ascii (name, "rgb:", 4))
    {
      unsigned int r, g, b;

      if (sscanf ((CIbyte *) name, "rgb:%04x/%04x/%04x", &r, &g, &b) == 3)
	{
	  int len = qxestrlen (name);
	  if (len == 18)
	    {
	      r /= 257;
	      g /= 257;
	      b /= 257;
	    }
	  else if (len == 15)
	    {
	      r /= 17;
	      g /= 17;
	      b /= 17;
	    }
	  return (PALETTERGB (r, g, b));
	}
      else
	return (COLORREF) -1;
    }
  else if (*name)	/* Can't be an empty string */
    {
      colormap_t key = { alloca_ascbytes (qxestrlen (name) + 1),
                         PALETTERGB (255, 255, 255) }, *res;
      Ascbyte *c = (Ascbyte *)(key.name);

      while (*name)
        {
          if (*name != ' ')
            {
              if (!byte_ascii_p (*name))
                {
                  return (COLORREF) -1;                  
                }

              *c++ = *name++;
            }
          else
            {
              name++;
            }
        }
      *c = '\0';
      
      if ((res = (colormap_t *) bsearch (&key, mswindows_X_color_map,
                                         countof (mswindows_X_color_map),
                                         sizeof (mswindows_X_color_map[0]),
                                         colormap_t_compare)) != NULL)
        {
          return res->colorref;
        }
    }
  return (COLORREF) -1;
}

Lisp_Object
mswindows_color_to_string (COLORREF color)
{
  int i;
  Ascbyte buf[8];
  COLORREF pcolor = PALETTERGB (GetRValue (color), GetGValue (color),
				GetBValue (color));

  for (i = 0; i < countof (mswindows_X_color_map); i++)
    if (pcolor == (mswindows_X_color_map[i].colorref))
      return  build_ascstring (mswindows_X_color_map[i].name);

  sprintf (buf, "#%02X%02X%02X",
	   GetRValue (color), GetGValue (color), GetBValue (color));
  return build_ascstring (buf);
}

/*
 * Returns non-zero if the two supplied font patterns match.
 * If they match and fontname is not NULL, copies the logical OR of the
 * patterns to fontname (which is assumed to be at least MSW_FONTSIZE in size).
 *
 * The patterns 'match' iff for each field that is not blank in either pattern,
 * the corresponding field in the other pattern is either identical or blank.
 */
static int
match_font (Ibyte *pattern1, Ibyte *pattern2,
	    Ibyte *fontname)
{
  Ibyte *c1 = pattern1, *c2 = pattern2, *e1 = 0, *e2 = 0;
  int i;

  if (fontname)
    fontname[0] = '\0';

  for (i = 0; i < 5; i++)
    {
      if (c1 && (e1 = qxestrchr (c1, ':')))
        *(e1) = '\0';
      if (c2 && (e2 = qxestrchr (c2, ':')))
        *(e2) = '\0';

      if (c1 && c1[0] != '\0')
        {
	  if (c2 && c2[0] != '\0' && qxestrcasecmp (c1, c2))
	    {
	      if (e1) *e1 = ':';
	      if (e2) *e2 = ':';
	      return 0;
	    }
	  else if (fontname)
	    qxestrcat_ascii (qxestrcat (fontname, c1), ":");
	}
      else if (fontname)
        {
	  if (c2 && c2[0] != '\0')
	    qxestrcat_ascii (qxestrcat (fontname, c2), ":");
	  else
	    qxestrcat_ascii (fontname, ":");
	}

      if (e1) *(e1++) = ':';
      if (e2) *(e2++) = ':';
      c1 = e1;
      c2 = e2;
    }

  if (fontname)
    fontname[qxestrlen (fontname) - 1] = '\0';	/* Trim trailing ':' */
  return 1;
}


/************************************************************************/
/*                                 exports                              */
/************************************************************************/

struct font_enum_t
{
  HDC hdc;
  Lisp_Object list;
};

static int CALLBACK
font_enum_callback_2 (ENUMLOGFONTEXW *lpelfe, NEWTEXTMETRICEXW *lpntme,
		      int FontType, struct font_enum_t *font_enum)
{
  Ibyte fontname[MSW_FONTSIZE * 2 * MAX_ICHAR_LEN]; /* should be enough :)*/
  Lisp_Object fontname_lispstr;
  int i;
  Ibyte *facename;

  /*
   * The enumerated font weights are not to be trusted because:
   *  a) lpelfe->elfStyle is only filled in for TrueType fonts.
   *  b) Not all Bold and Italic styles of all fonts (including some Vector,
   *     Truetype and Raster fonts) are enumerated.
   * I guess that fonts for which Bold and Italic styles are generated
   * 'on-the-fly' are not enumerated. It would be overly restrictive to
   * disallow Bold And Italic weights for these fonts, so we just leave
   * weights unspecified. This means that we have to weed out duplicates of
   * those fonts that do get enumerated with different weights.
   */
  facename = TSTR_TO_ITEXT (lpelfe->elfLogFont.lfFaceName);
  if (itext_ichar (facename) == '@')
    /* This is a font for writing vertically. We ignore it. */
    return 1;

  if (FontType == 0 /*vector*/ || FontType & TRUETYPE_FONTTYPE)
    /* Scalable, so leave pointsize blank */
    qxesprintf (fontname, "%s::::", facename);
  else
    /* Formula for pointsize->height from LOGFONT docs in Platform SDK */
    qxesprintf (fontname, "%s::%d::", facename,
		MulDiv (lpntme->ntmTm.tmHeight -
			lpntme->ntmTm.tmInternalLeading,
			72, GetDeviceCaps (font_enum->hdc, LOGPIXELSY)));

  /*
   * The enumerated font character set strings are not to be trusted because
   * lpelfe->elfScript is returned in the host language and not in English.
   * We can't know a priori the translations of "Western", "Central European"
   * etc into the host language, so we must use English. The same argument
   * applies to the font weight string when matching fonts.
   */
  for (i = 0; i < countof (charset_map); i++)
    if (lpelfe->elfLogFont.lfCharSet == charset_map[i].value)
      {
	qxestrcat_ascii (fontname, charset_map[i].name);
	break;
      }
  if (i == countof (charset_map))
    return 1;

  /* Add the font name to the list if not already there */
  fontname_lispstr = build_istring (fontname);
  if (NILP (Fassoc (fontname_lispstr, font_enum->list)))
    font_enum->list =
      Fcons (Fcons (fontname_lispstr,
		    /* TMPF_FIXED_PITCH is backwards from what you expect!
		       If set, it means NOT fixed pitch. */
		    (lpntme->ntmTm.tmPitchAndFamily & TMPF_FIXED_PITCH) ?
		    Qnil : Qt),
	     font_enum->list);

  return 1;
}

static int CALLBACK
font_enum_callback_1 (ENUMLOGFONTEXW *lpelfe,
		      NEWTEXTMETRICEXW *UNUSED (lpntme),
		      int UNUSED (FontType), struct font_enum_t *font_enum)
{
  /* This function gets called once per facename per character set.
   * We call a second callback to enumerate the fonts in each facename */
  return qxeEnumFontFamiliesEx (font_enum->hdc, &lpelfe->elfLogFont,
				(FONTENUMPROCW) font_enum_callback_2,
				(LPARAM) font_enum, 0);
}

/* Function for sorting lists of fonts as obtained from
   mswindows_enumerate_fonts().  These come in a known format:
   "family::::charset" for TrueType fonts, "family::size::charset"
   otherwise. */

static Boolint
sort_font_list_function (Lisp_Object UNUSED (pred), Lisp_Object UNUSED (key),
			 Lisp_Object obj1, Lisp_Object obj2)
{
  Ibyte *font1, *font2;
  Ibyte *c1, *c2;
  int t1, t2;

  /*
    1. fixed over proportional.
    2. Western over other charsets.
    3. TrueType over non-TrueType.
    4. Within non-TrueType, sizes closer to 10pt over sizes farther from 10pt.
    5. Courier New over other families.
  */

  /* The sort function should return non-zero if OBJ1 < OBJ2, zero
     otherwise. */

  t1 = !NILP (XCDR (obj1));
  t2 = !NILP (XCDR (obj2));

  if (t1 && !t2)
    return 1;
  if (t2 && !t1)
    return 0;

  font1 = XSTRING_DATA (XCAR (obj1));
  font2 = XSTRING_DATA (XCAR (obj2));

  c1 = qxestrrchr (font1, ':');
  c2 = qxestrrchr (font2, ':');

  t1 = !qxestrcasecmp_ascii (c1 + 1, "western");
  t2 = !qxestrcasecmp_ascii (c2 + 1, "western");

  if (t1 && !t2)
    return 1;
  if (t2 && !t1)
    return 0;

  c1 -= 2;
  c2 -= 2;
  t1 = *c1 == ':';
  t2 = *c2 == ':';

  if (t1 && !t2)
    return 1;
  if (t2 && !t1)
    return 0;

  if (!t1 && !t2)
    {
      while (isdigit (*c1))
	c1--;
      while (isdigit (*c2))
	c2--;

      t1 = qxeatoi (c1 + 1) - 10;
      t2 = qxeatoi (c2 + 1) - 10;

      if (abs (t1) < abs (t2))
	return 1;
      else if (abs (t2) < abs (t1))
	return 0;
      else if (t1 < t2)
	/* Prefer a smaller font over a larger one just as far away
	   because the smaller one won't upset the total line height if it's
	   just a few chars. */
	return 1;
    }

  t1 = !qxestrncasecmp_ascii (font1, "courier new:", 12);
  t2 = !qxestrncasecmp_ascii (font2, "courier new:", 12);

  if (t1 && !t2)
    return 1;
  if (t2 && !t1)
    return 0;

  return 0;
}

/*
 * Enumerate the available on the HDC fonts and return a list of string
 * font names.
 */
Lisp_Object
mswindows_enumerate_fonts (HDC hdc)
{
  /* This cannot GC */
  LOGFONTW logfont;
  struct font_enum_t font_enum;

  assert (hdc != NULL);
  logfont.lfCharSet = DEFAULT_CHARSET;
  logfont.lfFaceName[0] = '\0';
  logfont.lfPitchAndFamily = DEFAULT_PITCH;
  font_enum.hdc = hdc;
  font_enum.list = Qnil;
  /* EnumFontFamilies seems to enumerate only one charset per font, which
     is not what we want.  We aren't supporting NT 3.5x, so no need to
     worry about this not existing. */
  qxeEnumFontFamiliesEx (hdc, &logfont, (FONTENUMPROCW) font_enum_callback_1,
			 (LPARAM) (&font_enum), 0);

  return list_sort (font_enum.list, sort_font_list_function, Qnil, Qidentity);
}

static HFONT
mswindows_create_font_variant (Lisp_Font_Instance *f,
			       int under, int strike)
{
  /* Cannot GC */
  LOGFONTW lf;
  HFONT hfont;

  assert (FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike) == NULL);

  if (qxeGetObject (FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0),
		    sizeof (lf), (void *) &lf) == 0)
    {
      hfont = MSWINDOWS_BAD_HFONT;
    }
  else
    {
      lf.lfUnderline = under;
      lf.lfStrikeOut = strike;

      hfont = qxeCreateFontIndirect (&lf);
      if (hfont == NULL)
	hfont = MSWINDOWS_BAD_HFONT;
    }

  FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike) = hfont;
  return hfont;
}

HFONT
mswindows_get_hfont (Lisp_Font_Instance *f,
		     int under, int strike)
{
  /* Cannot GC */
  HFONT hfont = FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike);

  if (hfont == NULL)
    hfont = mswindows_create_font_variant (f, under, strike);

  /* If strikeout/underline variant of the font could not be
     created, then use the base version of the font */
  if (hfont == MSWINDOWS_BAD_HFONT)
    hfont = FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0);

  assert (hfont != NULL && hfont != MSWINDOWS_BAD_HFONT);

  return hfont;
}

/************************************************************************/
/*                               methods                                */
/************************************************************************/

static int
mswindows_initialize_color_instance (Lisp_Color_Instance *c, Lisp_Object name,
				     Lisp_Object UNUSED (device),
				     Error_Behavior errb)
{
  COLORREF color;

  color = mswindows_string_to_color (XSTRING_DATA (name));
  if (color != (COLORREF) -1)
    {
      c->data = xnew (struct mswindows_color_instance_data);
      COLOR_INSTANCE_MSWINDOWS_COLOR (c) = color;
      return 1;
    }
  maybe_signal_error (Qinvalid_constant,
		      "Unrecognized color", name, Qcolor, errb);
  return(0);
}

#if 0
static void
mswindows_mark_color_instance (Lisp_Color_Instance *c)
{
}
#endif

static void
mswindows_print_color_instance (Lisp_Color_Instance *c,
				Lisp_Object printcharfun,
				int UNUSED (escapeflag))
{
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  write_fmt_string (printcharfun,
		    " %06ld=(%04X,%04X,%04X)", color & 0xffffff,
		    GetRValue (color) * 257, GetGValue (color) * 257,
		    GetBValue (color) * 257);
}

static void
mswindows_finalize_color_instance (Lisp_Color_Instance *c)
{
  if (c->data)
    {
      xfree (c->data);
      c->data = 0;
    }
}

static int
mswindows_color_instance_equal (Lisp_Color_Instance *c1,
				Lisp_Color_Instance *c2,
				int UNUSED (depth))
{
  return (COLOR_INSTANCE_MSWINDOWS_COLOR (c1) ==
	  COLOR_INSTANCE_MSWINDOWS_COLOR (c2));
}

static Hashcode
mswindows_color_instance_hash (Lisp_Color_Instance *c, int UNUSED (depth))
{
  return (unsigned long) COLOR_INSTANCE_MSWINDOWS_COLOR (c);
}

static Lisp_Object
mswindows_color_instance_rgb_components (Lisp_Color_Instance *c)
{
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  return list3 (make_int (GetRValue (color) * 257),
		make_int (GetGValue (color) * 257),
		make_int (GetBValue (color) * 257));
}

static int
mswindows_valid_color_name_p (struct device *UNUSED (d), Lisp_Object color)
{
  return (mswindows_string_to_color (XSTRING_DATA (color)) != (COLORREF) -1);
}



static void
mswindows_finalize_font_instance (Lisp_Font_Instance *f);

/* Parse the font spec in NAMESTR.  Maybe issue errors, according to ERRB;
   NAME_FOR_ERRORS is the Lisp string to use when issuing errors.  Store
   the five parts of the font spec into the given strings, which should be
   declared as

   Ibyte fontname[LF_FACESIZE], weight[LF_FACESIZE], points[8];
   Ibyte effects[LF_FACESIZE], charset[LF_FACESIZE];

   If LOGFONT is given, store the necessary information in LOGFONT to
   create a font object.  If LOGFONT is given, HDC must also be given;
   else, NULL can be given for both.
   
   Return 1 if ok, 0 if error.
   */
static int
parse_font_spec (const Ibyte *namestr,
		 HDC hdc,
		 Lisp_Object name_for_errors,
		 Error_Behavior errb,
		 LOGFONTW *logfont,
		 Ibyte *fontname,
		 Ibyte *weight,
		 Ibyte *points,
		 Ibyte *effects,
		 Ibyte *charset)
{
  int fields, i;
  int pt;
  Ibyte *style;
  Ibyte *c;

  /*
   * mswindows fonts look like:
   *	fontname[:[weight ][style][:pointsize[:effects]]][:charset]
   * The font name field shouldn't be empty.
   *
   * ie:
   *	Lucida Console:Regular:10
   * minimal:
   *	Courier New
   * maximal:
   *	Courier New:Bold Italic:10:underline strikeout:western
   */

  fontname[0] = 0;
  weight[0] = 0;
  points[0] = 0;
  effects[0] = 0;
  charset[0] = 0;

  if (logfont)
    xzero (*logfont);

  fields = sscanf ((CIbyte *) namestr, "%31[^:]:%31[^:]:%7[^:]:%31[^:]:%31s",
		   fontname, weight, points, effects, charset);

  /* This function is implemented in a fairly ad-hoc manner.
   * The general idea is to validate and canonicalize each of the above fields
   * at the same time as we build up the win32 LOGFONT structure. This enables
   * us to use match_font() on a canonicalized font string to check the
   * availability of the requested font */

  if (fields < 0)
    {
      maybe_signal_error (Qinvalid_argument, "Invalid font", name_for_errors,
			  Qfont, errb);
      return 0;
    }

  if (fields > 0 && qxestrlen (fontname))
    {
      Extbyte *extfontname;

      extfontname = ITEXT_TO_TSTR (fontname);
      if (logfont)
	{
          qxetcsncpy ((Extbyte *) logfont->lfFaceName, extfontname,
	             LF_FACESIZE - 1);
	  logfont->lfFaceName[LF_FACESIZE - 1] = 0;
	}
    }

  /* weight */
  if (fields < 2)
    qxestrcpy_ascii (weight, fontweight_map[0].name);

  /* Maybe split weight into weight and style */
  if ((c = qxestrchr (weight, ' ')))
    {
      *c = '\0';
      style = c + 1;
    }
  else
    style = NULL;

  for (i = 0; i < countof (fontweight_map); i++)
    if (!qxestrcasecmp_ascii (weight, fontweight_map[i].name))
      {
	if (logfont)
	  logfont->lfWeight = fontweight_map[i].value;
	break;
      }
  if (i == countof (fontweight_map))	/* No matching weight */
    {
      if (!style)
	{
	  if (logfont)
	    logfont->lfWeight = FW_REGULAR;
	  style = weight;	/* May have specified style without weight */
	}
      else
	{
	  maybe_signal_error (Qinvalid_constant, "Invalid font weight",
			      name_for_errors, Qfont, errb);
	  return 0;
	}
    }

  if (style)
    {
      /* #### what about oblique? */
      if (qxestrcasecmp_ascii (style, "italic") == 0)
	{
	  if (logfont)
	    logfont->lfItalic = TRUE;
	}
      else
	{
	  maybe_signal_error (Qinvalid_constant,
			      "Invalid font weight or style",
			      name_for_errors, Qfont, errb);
	  return 0;
      }

      /* Glue weight and style together again */
      if (weight != style)
        *c = ' ';
    }
  else if (logfont)
    logfont->lfItalic = FALSE;

  if (fields < 3 || !qxestrcmp_ascii (points, ""))
    ;
  else if (points[0] == '0' ||
	   qxestrspn (points, "0123456789") < qxestrlen (points))
    {
      maybe_signal_error (Qinvalid_argument, "Invalid font pointsize",
			  name_for_errors, Qfont, errb);
      return 0;
    }
  else
    {
      pt = qxeatoi (points);

      if (logfont)
	{
	  /* Formula for pointsize->height from LOGFONT docs in MSVC5 Platform
	     SDK */
	  logfont->lfHeight = -MulDiv (pt, GetDeviceCaps (hdc, LOGPIXELSY),
				       72);
	  logfont->lfWidth = 0;
	}
    }

  /* Effects */
  if (logfont)
    {
      logfont->lfUnderline = FALSE;
      logfont->lfStrikeOut = FALSE;
    }

  if (fields >= 4 && effects[0] != '\0')
    {
      Ibyte *effects2;
      int underline = FALSE, strikeout = FALSE;

      /* Maybe split effects into effects and effects2 */
      if ((c = qxestrchr (effects, ' ')))
        {
          *c = '\0';
          effects2 = c + 1;
        }
      else
        effects2 = NULL;

      if (qxestrcasecmp_ascii (effects, "underline") == 0)
	underline = TRUE;
      else if (qxestrcasecmp_ascii (effects, "strikeout") == 0)
	strikeout = TRUE;
      else
        {
          maybe_signal_error (Qinvalid_constant, "Invalid font effect",
			      name_for_errors, Qfont, errb);
	  return 0;
	}

      if (effects2 && effects2[0] != '\0')
	{
	  if (qxestrcasecmp_ascii (effects2, "underline") == 0)
	    underline = TRUE;
	  else if (qxestrcasecmp_ascii (effects2, "strikeout") == 0)
	    strikeout = TRUE;
	  else
	    {
	      maybe_signal_error (Qinvalid_constant, "Invalid font effect",
				  name_for_errors, Qfont, errb);
	      return 0;
	    }
        }

      /* Regenerate sanitized effects string */
      if (underline)
	{
	  if (strikeout)
	    qxestrcpy_ascii (effects, "underline strikeout");
	  else
	    qxestrcpy_ascii (effects, "underline");
	}
      else if (strikeout)
	qxestrcpy_ascii (effects, "strikeout");

      if (logfont)
	{
	  logfont->lfUnderline = underline;
	  logfont->lfStrikeOut = strikeout;
	}
    }

  /* Charset */
  /* charset can be specified even if earlier fields haven't been */
  if (fields < 5)
    {
      if ((c = qxestrchr (namestr, ':')) && (c = qxestrchr (c + 1, ':')) &&
	  (c = qxestrchr (c + 1, ':')) && (c = qxestrchr (c + 1, ':')))
	{
	  qxestrncpy (charset, c + 1, LF_FACESIZE);
	  charset[LF_FACESIZE - 1] = '\0';
	}
    }

  /* NOTE: If you give a blank charset spec, we will normally not get here
     under Mule unless we explicitly call `make-font-instance'!  This is
     because the C code instantiates fonts using particular charsets, by
     way of specifier_matching_instance().  Before instantiating the font,
     font_instantiate() calls the devmeth find_matching_font(), which gets
     a truename font spec with the registry (i.e. the charset spec) filled
     in appropriately to the charset. */
  if (!qxestrcmp_ascii (charset, ""))
    ;
  else
    {
      for (i = 0; i < countof (charset_map); i++)
	if (!qxestrcasecmp_ascii (charset, charset_map[i].name))
	  {
	    if (logfont)
	      logfont->lfCharSet = charset_map[i].value;
	    break;
	  }

      if (i == countof (charset_map))	/* No matching charset */
	{
	  maybe_signal_error (Qinvalid_argument, "Invalid charset",
			      name_for_errors, Qfont, errb);
	  return 0;
	}
    }

  if (logfont)
    {
      /* Misc crud */
#if 1
      logfont->lfOutPrecision = OUT_DEFAULT_PRECIS;
      logfont->lfClipPrecision = CLIP_DEFAULT_PRECIS;
      logfont->lfQuality = DEFAULT_QUALITY;
#else
      logfont->lfOutPrecision = OUT_STROKE_PRECIS;
      logfont->lfClipPrecision = CLIP_STROKE_PRECIS;
      logfont->lfQuality = PROOF_QUALITY;
#endif
      /* Default to monospaced if the specified fontname doesn't exist. */
      logfont->lfPitchAndFamily = FF_MODERN;
    }

  return 1;
}

/*
  mswindows fonts look like:
  	[fontname[:style[:pointsize[:effects]]]][:charset]
   A maximal mswindows font spec looks like:
  	Courier New:Bold Italic:10:underline strikeout:Western

  A missing weight/style field is the same as Regular, and a missing
  effects field is left alone, and means no effects; but a missing
  fontname, pointsize or charset field means any will do.  We prefer
  Courier New, 10, Western.  See sort function above. */

static HFONT
create_hfont_from_font_spec (const Ibyte *namestr,
			     HDC hdc,
			     Lisp_Object name_for_errors,
			     Lisp_Object device_font_list,
			     Error_Behavior errb,
			     Lisp_Object *truename_ret)
{
  LOGFONTW logfont;
  HFONT hfont;
  Ibyte fontname[LF_FACESIZE], weight[LF_FACESIZE], points[8];
  Ibyte effects[LF_FACESIZE], charset[LF_FACESIZE];
  Ibyte truename[MSW_FONTSIZE];
  Ibyte truername[MSW_FONTSIZE];

  /* Windows will silently substitute a default font if the fontname
     specifies a non-existent font.  This is bad for screen fonts because
     it doesn't allow higher-level code to see the error and to act
     appropriately.  For instance complex_vars_of_faces() sets up a
     fallback list of fonts for the default face.  Instead, we look at all
     the possibilities and pick one that works, handling missing pointsize
     and charset fields appropriately.

     For printer fonts, we used to go ahead and let Windows choose the
     font, and for those devices, then, DEVICE_FONT_LIST would be nil.
     However, this causes problems with the font-matching code below, which
     needs a list of fonts so it can pick the right one for Mule.

     Thus, the code below to handle a nil DEVICE_FONT_LIST is not currently
     used. */

  if (!NILP (device_font_list))
    {
      Lisp_Object fonttail = Qnil;

      if (!parse_font_spec (namestr, 0, name_for_errors,
			    errb, 0, fontname, weight, points,
			    effects, charset))
	return 0;

      /* The fonts in the device font list always specify fontname and
	 charset, but often times not the size; so if we don't have the
	 size specified either, do a round with size 10 so we'll always end
	 up with a size in the truename (if we fail this one but succeed
	 the next one, we'll have chosen a non-TrueType font, and in those
	 cases the size is specified in the font list item. */

      if (!points[0])
	{
	  qxesprintf (truename, "%s:%s:10:%s:%s",
		      fontname, weight, effects, charset);

	  LIST_LOOP (fonttail, device_font_list)
	    {
	      if (match_font (XSTRING_DATA (XCAR (XCAR (fonttail))),
			      truename, truername))
		break;
	    }
	}

      if (NILP (fonttail))
	{
	  qxesprintf (truename, "%s:%s:%s:%s:%s",
		      fontname, weight, points, effects, charset);

	  LIST_LOOP (fonttail, device_font_list)
	    {
	      if (match_font (XSTRING_DATA (XCAR (XCAR (fonttail))),
			      truename, truername))
		break;
	    }
	}

      if (NILP (fonttail))
	{
	  maybe_signal_error (Qinvalid_argument, "No matching font",
			      name_for_errors, Qfont, errb);
	  return 0;
	}

      if (!parse_font_spec (truername, hdc, name_for_errors,
			    ERROR_ME_DEBUG_WARN, &logfont, fontname, weight,
			    points, effects, charset))
	signal_error (Qinternal_error, "Bad value in device font list?",
		      build_istring (truername));
    }
  else if (!parse_font_spec (namestr, hdc, name_for_errors,
			     errb, &logfont, fontname, weight, points,
			     effects, charset))
    return 0;

  if ((hfont = qxeCreateFontIndirect (&logfont)) == NULL)
    {
      maybe_signal_error (Qgui_error, "Couldn't create font",
			  name_for_errors, Qfont, errb);
      return 0;
    }

  /* #### Truename will not have all its fields filled in when we have no
     list of fonts.  Doesn't really matter now, since we always have one.
     See above. */
  qxesprintf (truename, "%s:%s:%s:%s:%s", fontname, weight,
	      points, effects, charset);
  
  *truename_ret = build_istring (truename);
  return hfont;
}

/*
 * This is a work horse for both mswindows_initialize_font_instance and
 * msprinter_initialize_font_instance.
 */
static int
initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
			  Lisp_Object device_font_list, HDC hdc,
			  Error_Behavior errb)
{
  HFONT hfont, hfont2;
  TEXTMETRICW metrics;
  Ibyte *namestr = XSTRING_DATA (name);
  Lisp_Object truename;

  hfont = create_hfont_from_font_spec (namestr, hdc, name, device_font_list,
				       errb, &truename);
  if (!hfont)
    return 0;
  f->truename = truename;
  f->data = xnew_and_zero (struct mswindows_font_instance_data);
  FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0) = hfont;

  /* Some underlined fonts have the descent of one pixel more than their
     non-underlined counterparts. Font variants though are assumed to have
     identical metrics. So get the font metrics from the underlined variant
     of the font */
  hfont2 = mswindows_create_font_variant (f, 1, 0);
  if (hfont2 != MSWINDOWS_BAD_HFONT)
    hfont = hfont2;

  hfont2 = (HFONT) SelectObject (hdc, hfont);
  if (!hfont2)
    {
      mswindows_finalize_font_instance (f);
      maybe_signal_error (Qgui_error, "Couldn't map font", name, Qfont, errb);
      return 0;
    }
  qxeGetTextMetrics (hdc, &metrics);
  SelectObject (hdc, hfont2);

  f->width = (unsigned short) metrics.tmAveCharWidth;
  f->height = (unsigned short) metrics.tmHeight;
  f->ascent = (unsigned short) metrics.tmAscent;
  f->descent = (unsigned short) metrics.tmDescent;
  f->proportional_p = (metrics.tmPitchAndFamily & TMPF_FIXED_PITCH);

  return 1;
}

static int
mswindows_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
				    Lisp_Object device, Error_Behavior errb)
{
  HDC hdc = CreateCompatibleDC (NULL);
  Lisp_Object font_list = DEVICE_MSWINDOWS_FONTLIST (XDEVICE (device));
  int res = initialize_font_instance (f, name, font_list, hdc, errb);
  DeleteDC (hdc);
  return res;
}

static int
msprinter_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
				    Lisp_Object device, Error_Behavior errb)
{
  HDC hdc = DEVICE_MSPRINTER_HDC (XDEVICE (device));
  Lisp_Object font_list = DEVICE_MSPRINTER_FONTLIST (XDEVICE (device));
  return initialize_font_instance (f, name, font_list, hdc, errb);
}

static void
mswindows_finalize_font_instance (Lisp_Font_Instance *f)
{
  int i;

  if (f->data)
    {
      for (i = 0; i < MSWINDOWS_NUM_FONT_VARIANTS; i++)
	{
	  if (FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i) != NULL
	      && FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i) != MSWINDOWS_BAD_HFONT)
	    DeleteObject (FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i));
	}

      xfree (f->data);
      f->data = 0;
   }
}

#if 0
static void
mswindows_mark_font_instance (Lisp_Font_Instance *f)
{
}
#endif

static void
mswindows_print_font_instance (Lisp_Font_Instance *f,
			       Lisp_Object printcharfun,
			       int UNUSED (escapeflag))
{
  write_fmt_string (printcharfun, " 0x%lx",
		    (unsigned long)
		    FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0));

}

static Lisp_Object
mswindows_font_list (Lisp_Object pattern, Lisp_Object device,
		      Lisp_Object UNUSED (maxnumber))
{
  struct device *d = XDEVICE (device);
  Lisp_Object font_list = Qnil, fonttail, result = Qnil;

  if (DEVICE_MSWINDOWS_P (d))
    font_list = DEVICE_MSWINDOWS_FONTLIST (d);
  else if (DEVICE_MSPRINTER_P (d))
    font_list = DEVICE_MSPRINTER_FONTLIST (d);
  else
    ABORT ();

  LIST_LOOP (fonttail, font_list)
    {
      Ibyte fontname[MSW_FONTSIZE];

      if (match_font (XSTRING_DATA (XCAR (XCAR (fonttail))),
		      XSTRING_DATA (pattern),
		      fontname))
	result = Fcons (build_istring (fontname), result);
    }

  return Fnreverse (result);
}

static Lisp_Object
mswindows_font_instance_truename (Lisp_Font_Instance *f,
				  Error_Behavior UNUSED (errb))
{
  return f->truename;
}

#ifdef MULE

static int
mswindows_font_spec_matches_charset_stage_1 (struct device *UNUSED (d),
					     Lisp_Object charset,
					     const Ibyte *nonreloc,
					     Lisp_Object reloc,
					     Bytecount offset,
					     Bytecount length)
{
  int i;
  Lisp_Object charset_registry;
  const Ibyte *font_charset;
  const Ibyte *the_nonreloc = nonreloc;
  const Ibyte *c;
  Bytecount the_length = length;

  if (NILP (charset))
    return 1;

  if (!the_nonreloc)
    the_nonreloc = XSTRING_DATA (reloc);
  fixup_internal_substring (nonreloc, reloc, offset, &the_length);
  the_nonreloc += offset;

  c = the_nonreloc;
  for (i = 0; i < 4; i++)
    {
      Ibyte *newc = (Ibyte *) memchr (c, ':', the_length);
      if (!newc)
	break;
      newc++;
      the_length -= (newc - c);
      c = newc;
    }

  if (i < 4)
    return 0;

  font_charset = c;

  /* For border-glyph use */
  if (!qxestrcasecmp_ascii (font_charset, "symbol"))
    font_charset = (const Ibyte *) "western";

  /* Get code page for the charset */
  charset_registry = Fmswindows_charset_registry (charset);
  if (!STRINGP (charset_registry))
    return 0;

  return !qxestrcasecmp (XSTRING_DATA (charset_registry), font_charset);
}

/*

#### The following comment is old and probably not applicable any longer.

1. handle standard mapping and inheritance vectors properly in Face-frob-property.
2. finish impl of mswindows-charset-registry.
3. see if everything works under fixup, now that i copied the stuff over.
4. consider generalizing Face-frob-property to frob-specifier.
5. maybe extract some of the flets out of Face-frob-property as useful specifier frobbing. 
6. eventually this stuff's got to be checked in!!!!
*/

static int
mswindows_font_spec_matches_charset_stage_2 (struct device *d,
					     Lisp_Object charset,
					     const Ibyte *nonreloc,
					     Lisp_Object reloc,
					     Bytecount offset,
					     Bytecount length)
{
  const Ibyte *the_nonreloc = nonreloc;
  FONTSIGNATURE fs;
  FONTSIGNATURE *fsp = &fs;
  struct gcpro gcpro1;
  Lisp_Object fontsig;
  Bytecount the_length = length;
  int i;

  if (NILP (charset))
    return 1;

  if (!the_nonreloc)
    the_nonreloc = XSTRING_DATA (reloc);
  fixup_internal_substring (nonreloc, reloc, offset, &the_length);
  the_nonreloc += offset;

  /* Get the list of Unicode subranges corresponding to the font.  This
     is contained inside of FONTSIGNATURE data, obtained by calling
     GetTextCharsetInfo on a font object, which we need to create from the
     spec.  See if the FONTSIGNATURE data is already cached.  If not, get
     it and cache it. */
  if (!STRINGP (reloc) || the_nonreloc != XSTRING_DATA (reloc))
    reloc = build_istring (the_nonreloc);
  GCPRO1 (reloc);
  fontsig = Fgethash (reloc, Vfont_signature_data, Qunbound);

  if (!UNBOUNDP (fontsig))
    {
      fsp = (FONTSIGNATURE *) XOPAQUE_DATA (fontsig);
      UNGCPRO;
    }
  else
    {
      HDC hdc = CreateCompatibleDC (NULL);
      Lisp_Object font_list = Qnil, truename; 
      HFONT hfont;

      if (DEVICE_TYPE_P (d, mswindows))
	{
	  font_list = DEVICE_MSWINDOWS_FONTLIST (d);
	}
      else if (DEVICE_TYPE_P (d, msprinter))
	{
	  font_list = DEVICE_MSPRINTER_FONTLIST (d);
	}
      else
	{
	  assert(0);
	}

      hfont = create_hfont_from_font_spec (the_nonreloc, hdc, Qnil,
					   font_list,
					   ERROR_ME_DEBUG_WARN,
					   &truename);

      if (!hfont || !(hfont = (HFONT) SelectObject (hdc, hfont)))
	{
	nope:
	  DeleteDC (hdc);
	  UNGCPRO;
	  return 0;
	}
      
      if (GetTextCharsetInfo (hdc, &fs, 0) == DEFAULT_CHARSET)
	{
	  SelectObject (hdc, hfont);
	  goto nope;
	}
      SelectObject (hdc, hfont);
      DeleteDC (hdc);
      Fputhash (reloc, make_opaque (&fs, sizeof (fs)), Vfont_signature_data);
      UNGCPRO;
    }

  {
    int lowlim, highlim;
    int dim, j, cp = -1;

    /* Try to find a Unicode char in the charset.  #### This is somewhat
       bogus.  See below.

       #### Cache me baby!!!!!!!!!!!!!
    */
    get_charset_limits (charset, &lowlim, &highlim);
    dim = XCHARSET_DIMENSION (charset);

    if (dim == 1)
      {
	for (i = lowlim; i <= highlim; i++)
	  if ((cp = ichar_to_unicode (make_ichar (charset, i, 0))) >= 0)
	    break;
      }
    else
      {
	for (i = lowlim; i <= highlim; i++)
	  for (j = lowlim; j <= highlim; j++)
	    if ((cp = ichar_to_unicode (make_ichar (charset, i, j))) >= 0)
	      break;
      }
      
    if (cp < 0)
      return 0;

    /* Check to see, for each subrange supported by the font,
       whether the Unicode char is within that subrange.  If any match,
       the font supports the char (whereby, the charset, bogusly). */
      
    for (i = 0; i < 128; i++)
      {
	if (fsp->fsUsb[i >> 5] & (1 << (i & 32)))
	  {
	    for (j = 0; j < unicode_subrange_table[i].no_subranges; j++)
	      if (cp >= unicode_subrange_table[i].subranges[j].start &&
		  cp <= unicode_subrange_table[i].subranges[j].end)
		return 1;
	  }
      }

    return 0;
  }
}

/*
  Given a truename font spec, does it match CHARSET?

  We try two stages:

  -- First see if the charset corresponds to one of the predefined Windows
  charsets; if so, we see if the registry (that's the last element of the
  font spec) is that same charset.  If so, this means that the font is
  specifically designed for the charset, and we prefer it.

  -- However, there are only a limited number of defined Windows charsets,
  and new ones aren't being defined; so if we fail the first stage, we find
  a character from the charset with a Unicode equivalent, and see if the
  font can display this character.  we do that by retrieving the Unicode
  ranges that the font supports, to see if the character comes from that
  subrange.

  #### Note: We really want to be doing all these checks at the character
  level, not the charset level.  There's no guarantee that a charset covers
  a single Unicode range.  Furthermore, this is extremely wasteful.  We
  should be doing this when we're about to redisplay and already have the
  Unicode codepoints in hand.
*/

static int
mswindows_font_spec_matches_charset (struct device *d, Lisp_Object charset,
				     const Ibyte *nonreloc,
				     Lisp_Object reloc,
				     Bytecount offset, Bytecount length,
				     enum font_specifier_matchspec_stages stage)
{
  return stage == STAGE_FINAL ?
     mswindows_font_spec_matches_charset_stage_2 (d, charset, nonreloc,
						  reloc, offset, length)
    : mswindows_font_spec_matches_charset_stage_1 (d, charset, nonreloc,
						   reloc, offset, length);
}


/* Find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */

static Lisp_Object
mswindows_find_charset_font (Lisp_Object device, Lisp_Object font,
			     Lisp_Object charset,
			     enum font_specifier_matchspec_stages stage)
{
  Lisp_Object fontlist, fonttail;

  /* If FONT specifies a particular charset, this will only list fonts with
     that charset; otherwise, it will list fonts with all charsets. */
  fontlist = mswindows_font_list (font, device, Qnil);

  if (stage == STAGE_INITIAL)
    {
      LIST_LOOP (fonttail, fontlist)
	{
	  if (mswindows_font_spec_matches_charset_stage_1
	      (XDEVICE (device), charset, 0, XCAR (fonttail), 0, -1))
	    return XCAR (fonttail);
	}
    }
  else
    {
      LIST_LOOP (fonttail, fontlist)
	{
	  if (mswindows_font_spec_matches_charset_stage_2
	      (XDEVICE (device), charset, 0, XCAR (fonttail), 0, -1))
	    return XCAR (fonttail);
	}
    }

  return Qnil;
}

#endif /* MULE */


/************************************************************************/
/*                             non-methods                              */
/************************************************************************/

static Lisp_Object
mswindows_color_list (void)
{
  Lisp_Object result = Qnil;
  int i;

  for (i = countof (mswindows_X_color_map); i != 0;)
    result = Fcons (build_ascstring (mswindows_X_color_map[--i].name), result);

  return result;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_fontcolor_mswindows (void)
{
}

void
console_type_create_fontcolor_mswindows (void)
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
  CONSOLE_HAS_METHOD (mswindows, color_list);

  CONSOLE_HAS_METHOD (mswindows, initialize_font_instance);
/*  CONSOLE_HAS_METHOD (mswindows, mark_font_instance); */
  CONSOLE_HAS_METHOD (mswindows, print_font_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_font_instance);
  CONSOLE_HAS_METHOD (mswindows, font_instance_truename);
  CONSOLE_HAS_METHOD (mswindows, font_list);
#ifdef MULE
  CONSOLE_HAS_METHOD (mswindows, font_spec_matches_charset);
  CONSOLE_HAS_METHOD (mswindows, find_charset_font);
#endif

  /* Printer methods - delegate most to windows methods,
     since graphical objects behave the same way. */

  CONSOLE_INHERITS_METHOD (msprinter, mswindows, initialize_color_instance);
/*  CONSOLE_INHERITS_METHOD (msprinter, mswindows, mark_color_instance); */
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, print_color_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, finalize_color_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_equal);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_hash);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_rgb_components);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, valid_color_name_p);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_list);

  CONSOLE_HAS_METHOD (msprinter, initialize_font_instance);
/*  CONSOLE_INHERITS_METHOD (msprinter, mswindows, mark_font_instance); */
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, print_font_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, finalize_font_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, font_instance_truename);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, font_list);
#ifdef MULE
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, font_spec_matches_charset);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, find_charset_font);
#endif
}

void
reinit_vars_of_fontcolor_mswindows (void)
{
#ifdef MULE
  int i;

  unicode_subrange_table = xnew_array_and_zero (unicode_subrange_t, 128);
  for (i = 0; i < countof (unicode_subrange_raw_map); i++)
    {
      const unicode_subrange_raw_t *el = &unicode_subrange_raw_map[i];
      if (unicode_subrange_table[el->subrange_bit].subranges == 0)
	unicode_subrange_table[el->subrange_bit].subranges = el;
      unicode_subrange_table[el->subrange_bit].no_subranges++;
    }

  Fclrhash (Vfont_signature_data);
#endif /* MULE */
}

void
vars_of_fontcolor_mswindows (void)
{
#ifdef MULE
  Vfont_signature_data =
    make_lisp_hash_table (100, HASH_TABLE_NON_WEAK, Qequal);
  staticpro (&Vfont_signature_data);
#endif /* MULE */
}
