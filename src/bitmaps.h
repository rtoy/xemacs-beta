/* This file is part of XEmacs.

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

   JWZ (?): 1992?.
 */

#ifndef INCLUDED_bitmaps_h_
#define INCLUDED_bitmaps_h_

#if 0
/* A gnu, like on the back of the emacs manual, for icons. */
#include "../etc/gnu.xbm"

/* The kitchen-sink icon. */
#include "../etc/sink.xbm"
#endif /* 0 */

#include "../etc/xemacs.xbm"

#if 0
/* Vertical bars */
#define compress_width 16
#define compress_height 10
static unsigned char compress_bits[] = {
   0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
   0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66};

/* Bracketed dash */
#define compress_width 15
#define compress_height 10
static unsigned char compress_bits[] = {
   0x1f, 0x7c, 0x1f, 0x7c, 0x03, 0x60, 0x03, 0x60, 0xe3, 0x63, 0xe3, 0x63,
   0x03, 0x60, 0x03, 0x60, 0x1f, 0x7c, 0x1f, 0x7c};
#endif

#if 0
/* Rectangled dash */
#define compress_width 15
#define compress_height 10
static unsigned char compress_bits[] = {
   0xff, 0x7f, 0xff, 0x7f, 0x03, 0x60, 0x03, 0x60, 0xe3, 0x63, 0xe3, 0x63,
   0x03, 0x60, 0x03, 0x60, 0xff, 0x7f, 0xff, 0x7f};
#endif

#if 0
#define extent_begin_width 18
#define extent_begin_height 10
static unsigned char extent_begin_bits[] = {
   0x0c, 0x00, 0x00, 0xcc, 0x00, 0x00, 0xcc, 0x0c, 0x00, 0xcc, 0xcc, 0x00,
   0xcc, 0xcc, 0x00, 0xcc, 0xcc, 0x00, 0xcc, 0xcc, 0x00, 0xcc, 0x0c, 0x00,
   0xcc, 0x00, 0x00, 0x0c, 0x00, 0x00};

#define extent_end_width 18
#define extent_end_height 10
static unsigned char extent_end_bits[] = {
   0x00, 0xc0, 0x00, 0x00, 0xcc, 0x00, 0xc0, 0xcc, 0x00, 0xcc, 0xcc, 0x00,
   0xcc, 0xcc, 0x00, 0xcc, 0xcc, 0x00, 0xcc, 0xcc, 0x00, 0xc0, 0xcc, 0x00,
   0x00, 0xcc, 0x00, 0x00, 0xc0, 0x00};
#endif

#if 0
/* A diamond. */
#define continuer_width 8
#define continuer_height 10
static unsigned char continuer_bits[] = {
   0x18, 0x18, 0x34, 0x34, 0x62, 0x62, 0x34, 0x34, 0x18, 0x18};

/* A left-pointing triangle. */
#define truncator_width 8
#define truncator_height 10
static unsigned char truncator_bits[] = {
   0x40, 0x60, 0x70, 0x78, 0x7c, 0x7c, 0x78, 0x70, 0x60, 0x40};
#endif

/* An arrow pointing to the next line */
#define continuer_width 7
#define continuer_height 10
static unsigned char continuer_bits[] = {
   0x00, 0xbc, 0xfc, 0xe0, 0xe0, 0x72, 0x3e, 0x1e, 0x1e, 0x3e};

#if 0
/* Three dots indicating truncation */
#define truncator_width 7
#define truncator_height 8
static unsigned char truncator_bits[] = {
   0x06, 0x06, 0x00, 0x18, 0x18, 0x00, 0x60, 0x60};
#endif

/* A Right pointing Arrow */
#define truncator_width 8
#define truncator_height 10
static unsigned char truncator_bits[] = {
  0x00, 0x18, 0x30, 0x60, 0xff, 0xff, 0x60, 0x30, 0x18, 0x00};

/* A Left pointing Arrow */
#define hscroll_width 8
#define hscroll_height 10
static unsigned char hscroll_bits[] = {
  0x00, 0x18, 0x0c, 0x06, 0xff, 0xff, 0x06, 0x0c, 0x18, 0x00};

#if 0
#define rarrow_width 12
#define rarrow_height 10
static unsigned char rarrow_bits[] = {
   0x40, 0x00, 0xc0, 0x00, 0x80, 0x01, 0x80, 0x03, 0xfe, 0x07, 0xfe, 0x07,
   0x80, 0x03, 0x80, 0x01, 0xc0, 0x00, 0x40, 0x00};
#endif

/* Stipples */

#if 0
/* A stipple for hilighting. */
#define selection_width 16
#define selection_height 16
static unsigned char selection_bits[] = {
   0x04, 0x84, 0x80, 0x00, 0x00, 0x20, 0x02, 0x04, 0x40, 0x00, 0x08, 0x82,
   0x00, 0x10, 0x40, 0x00, 0x02, 0x40, 0x00, 0x02, 0x10, 0x00, 0x80, 0x80,
   0x00, 0x08, 0x08, 0x00, 0x01, 0x02, 0x40, 0x20};

#define secondary_selection_width 16
#define secondary_selection_height 16
static unsigned char secondary_selection_bits[] = {
   0x08, 0x08, 0x04, 0x04, 0x02, 0x02, 0x01, 0x01, 0x80, 0x80, 0x40, 0x40,
   0x20, 0x20, 0x10, 0x10, 0x08, 0x08, 0x04, 0x04, 0x02, 0x02, 0x01, 0x01,
   0x80, 0x80, 0x40, 0x40, 0x20, 0x20, 0x10, 0x10};

#define overlap_selection_width 16
#define overlap_selection_height 16
static unsigned char overlap_selection_bits[] = {
   0x09, 0x88, 0x84, 0x04, 0x02, 0x22, 0x01, 0x05, 0x80, 0x80, 0x48, 0x42,
   0x20, 0x20, 0x50, 0x10, 0x0a, 0x48, 0x04, 0x04, 0x12, 0x02, 0x01, 0x01,
   0x80, 0x88, 0x48, 0x40, 0x21, 0xa2, 0x50, 0x10};

#define default0_stipple_width 16
#define default0_stipple_height 16
static unsigned char default0_stipple_bits[] = {
   0x00, 0x00, 0x66, 0x66, 0x66, 0x66, 0x00, 0x00, 0x00, 0x00, 0x66, 0x66,
   0x66, 0x66, 0x00, 0x00, 0x00, 0x00, 0x66, 0x66, 0x66, 0x66, 0x00, 0x00,
   0x00, 0x00, 0x66, 0x66, 0x66, 0x66, 0x00, 0x00};

#define default1_stipple_width 16
#define default1_stipple_height 16
static unsigned char default1_stipple_bits[] = {
   0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22,
   0x22, 0x22, 0x00, 0x00, 0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00,
   0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x00, 0x00};
#endif

#endif /* INCLUDED_bitmaps_h_ */
