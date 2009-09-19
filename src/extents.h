/* Copyright (c) 1994, 1995 Free Software Foundation.
   Copyright (c) 1995, 1996, 2002 Ben Wing.

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

#ifndef INCLUDED_extents_h_
#define INCLUDED_extents_h_

DECLARE_LRECORD (extent, struct extent);
#define XEXTENT(x) XRECORD (x, extent, struct extent)
#define wrap_extent(p) wrap_record (p, extent)
#define EXTENTP(x) RECORDP (x, extent)
#define CHECK_EXTENT(x) CHECK_RECORD (x, extent)
#define CONCHECK_EXTENT(x) CONCHECK_RECORD (x, extent)

struct extent_auxiliary;

DECLARE_LRECORD (extent_auxiliary, struct extent_auxiliary);
#define XEXTENT_AUXILIARY(x) \
  XRECORD (x, extent_auxiliary, struct extent_auxiliary)
#define wrap_extent_auxiliary(p) wrap_record (p, extent_auxiliary)
#define EXTENT_AUXILIARYP(x) RECORDP (x, extent_auxiliary)
#define CHECK_EXTENT_AUXILIARY(x) CHECK_RECORD (x, extent_auxiliary)
#define CONCHECK_EXTENT_AUXILIARY(x) CONCHECK_RECORD (x, extent_auxiliary)

struct extent_info;

DECLARE_LRECORD (extent_info, struct extent_info);
#define XEXTENT_INFO(x) XRECORD (x, extent_info, struct extent_info)
#define wrap_extent_info(p) wrap_record (p, extent_info)
#define EXTENT_INFOP(x) RECORDP (x, extent_info)
#define CHECK_EXTENT_INFO(x) CHECK_RECORD (x, extent_info)
#define CONCHECK_EXTENT_INFO(x) CONCHECK_RECORD (x, extent_info)

#ifdef NEW_GC
struct gap_array_marker;

DECLARE_LRECORD (gap_array_marker, struct gap_array_marker);
#define XGAP_ARRAY_MARKER(x) \
  XRECORD (x, gap_array_marker, struct gap_array_marker)
#define wrap_gap_array_marker(p) wrap_record (p, gap_array_marker)
#define GAP_ARRAY_MARKERP(x) RECORDP (x, gap_array_marker)
#define CHECK_GAP_ARRAY_MARKER(x) CHECK_RECORD (x, gap_array_marker)
#define CONCHECK_GAP_ARRAY_MARKER(x) CONCHECK_RECORD (x, gap_array_marker)

struct gap_array;

DECLARE_LRECORD (gap_array, struct gap_array);
#define XGAP_ARRAY(x) XRECORD (x, gap_array, struct gap_array)
#define wrap_gap_array(p) wrap_record (p, gap_array)
#define GAP_ARRAYP(x) RECORDP (x, gap_array)
#define CHECK_GAP_ARRAY(x) CHECK_RECORD (x, gap_array)
#define CONCHECK_GAP_ARRAY(x) CONCHECK_RECORD (x, gap_array)

struct extent_list_marker;

DECLARE_LRECORD (extent_list_marker, struct extent_list_marker);
#define XEXTENT_LIST_MARKER(x) \
  XRECORD (x, extent_list_marker, struct extent_list_marker)
#define wrap_extent_list_marker(p) wrap_record (p, extent_list_marker)
#define EXTENT_LIST_MARKERP(x) RECORDP (x, extent_list_marker)
#define CHECK_EXTENT_LIST_MARKER(x) CHECK_RECORD (x, extent_list_marker)
#define CONCHECK_EXTENT_LIST_MARKER(x) CONCHECK_RECORD (x, extent_list_marker)

struct extent_list;

DECLARE_LRECORD (extent_list, struct extent_list);
#define XEXTENT_LIST(x) XRECORD (x, extent_list, struct extent_list)
#define wrap_extent_list(p) wrap_record (p, extent_list)
#define EXTENT_LISTP(x) RECORDP (x, extent_list)
#define CHECK_EXTENT_LIST(x) CHECK_RECORD (x, extent_list)
#define CONCHECK_EXTENT_LIST(x) CONCHECK_RECORD (x, extent_list)

struct stack_of_extents;

DECLARE_LRECORD (stack_of_extents, struct stack_of_extents);
#define XSTACK_OF_EXTENTS(x) \
  XRECORD (x, stack_of_extents, struct stack_of_extents)
#define wrap_stack_of_extents(p) wrap_record (p, stack_of_extents)
#define STACK_OF_EXTENTSP(x) RECORDP (x, stack_of_extents)
#define CHECK_STACK_OF_EXTENTS(x) CHECK_RECORD (x, stack_of_extents)
#define CONCHECK_STACK_OF_EXTENTS(x) CONCHECK_RECORD (x, stack_of_extents)
#endif /* NEW_GC */

/* the layouts for glyphs (extent->flags.glyph_layout).  Must fit in 2 bits. */
typedef enum glyph_layout
{
  GL_TEXT,
  GL_OUTSIDE_MARGIN,
  GL_INSIDE_MARGIN,
  GL_WHITESPACE
} glyph_layout;

struct extent;

void set_extent_endpoints (EXTENT extent, Bytexpos s, Bytexpos e,
			   Lisp_Object object);


void flush_cached_extent_info (Lisp_Object extent_info);

void set_extent_glyph (EXTENT extent, Lisp_Object glyph, int endp,
		       glyph_layout layout);


/* flags for map_extents() and friends */
#define ME_END_CLOSED (1 << 0)
#define ME_START_OPEN (1 << 1)
#define ME_ALL_EXTENTS_CLOSED (1 << 2)
#define ME_ALL_EXTENTS_OPEN (2 << 2)
#define ME_ALL_EXTENTS_CLOSED_OPEN (3 << 2)
#define ME_ALL_EXTENTS_OPEN_CLOSED (4 << 2)
#define ME_ALL_EXTENTS_MASK (7 << 2)
#define ME_START_IN_REGION (1 << 5)
#define ME_END_IN_REGION (2 << 5)
#define ME_START_AND_END_IN_REGION (3 << 5)
#define ME_START_OR_END_IN_REGION (4 << 5)
#define ME_IN_REGION_MASK (7 << 5)
#define ME_NEGATE_IN_REGION (1 << 8)
/* the following flags are internal-only */
#define ME_INCLUDE_INTERNAL (1 << 9)
#define ME_MIGHT_THROW (1 << 10)
#define ME_MIGHT_MODIFY_TEXT (1 << 11)
#define ME_MIGHT_MODIFY_EXTENTS (1 << 12)
#define ME_MIGHT_MOVE_SOE (1 << 13)
#define ME_MIGHT_CALL_ELISP (ME_MIGHT_THROW | ME_MIGHT_MODIFY_TEXT | \
			     ME_MIGHT_MODIFY_EXTENTS | ME_MIGHT_MOVE_SOE)


extern int inside_undo;
extern int in_modeline_generation;

extern Fixnum mouse_highlight_priority;

EXFUN (Fextent_at, 5);
EXFUN (Fextent_property, 3);
EXFUN (Fput_text_property, 5);

EXFUN (Fdetach_extent, 1);
EXFUN (Fextent_end_position, 1);
EXFUN (Fextent_object, 1);
EXFUN (Fextent_properties, 1);
EXFUN (Fextent_start_position, 1);
EXFUN (Fget_char_property, 4);
EXFUN (Fmake_extent, 3);
EXFUN (Fnext_extent_change, 2);
EXFUN (Fprevious_extent_change, 2);
EXFUN (Fprevious_single_char_property_change, 4);
EXFUN (Fset_extent_endpoints, 4);
EXFUN (Fset_extent_parent, 2);
EXFUN (Fset_extent_property, 3);
EXFUN (Fset_extent_priority, 2);
EXFUN (Fset_extent_face, 2);
EXFUN (Fmap_extents, 8);

enum extent_at_flag
{
  EXTENT_AT_DEFAULT = 0,
  EXTENT_AT_AFTER = 0,
  EXTENT_AT_BEFORE,
  EXTENT_AT_AT
};

Bytexpos extent_endpoint_byte (EXTENT extent, int endp);
Charxpos extent_endpoint_char (EXTENT extent, int endp);
Bytexpos next_previous_single_property_change (Bytexpos pos, Lisp_Object prop,
					       Lisp_Object object,
					       Bytexpos limit,
					       Boolint next,
					       Boolint text_props_only);
Lisp_Object get_char_property (Bytexpos position, Lisp_Object prop,
			       Lisp_Object object, enum extent_at_flag fl,
			       int text_props_only);
void adjust_extents (Lisp_Object object, Memxpos from,
		     Memxpos to, int amount);
void adjust_extents_for_deletion (Lisp_Object object, Bytexpos from,
				  Bytexpos to, int gapsize,
				  int numdel, int movegapsize);
void verify_extent_modification (Lisp_Object object, Bytexpos from,
				 Bytexpos to,
				 Lisp_Object inhibit_read_only_value);
void process_extents_for_insertion (Lisp_Object object,
				    Bytexpos opoint, Bytecount length);
void process_extents_for_deletion (Lisp_Object object, Bytexpos from,
				   Bytexpos to, int destroy_them);
/* Note the following function is in Charbpos's */
void report_extent_modification (Lisp_Object buffer, Charbpos start,
				 Charbpos end, int afterp);
void add_string_extents (Lisp_Object string, struct buffer *buf,
			 Bytexpos opoint, Bytecount length);
void splice_in_string_extents (Lisp_Object string, struct buffer *buf,
			       Bytexpos opoint, Bytecount length,
			       Bytecount pos);
void copy_string_extents (Lisp_Object new_string,
			  Lisp_Object old_string,
			  Bytecount new_pos, Bytecount old_pos,
			  Bytecount length);
void detach_all_extents (Lisp_Object object);
Lisp_Object extent_at (Bytexpos position, Lisp_Object object,
		       Lisp_Object property, EXTENT before,
		       enum extent_at_flag at_flag, int all_extents);


struct extent_fragment *extent_fragment_new (Lisp_Object buffer_or_string,
					     struct frame *frm);
face_index extent_fragment_update (struct window *w,
				   struct extent_fragment *ef,
				   Bytexpos pos, Lisp_Object last_glyph);
void extent_fragment_delete (struct extent_fragment *ef);

/* from alloc.c */
struct extent *allocate_extent (void);

void allocate_extent_auxiliary (EXTENT ext);
void init_buffer_extents (struct buffer *b);
void uninit_buffer_extents (struct buffer *b);

#ifdef ERROR_CHECK_EXTENTS
void sledgehammer_extent_check (Lisp_Object obj);
#endif

#ifdef MEMORY_USAGE_STATS
int compute_buffer_extent_usage (struct buffer *b,
				 struct overhead_stats *ovstats);
#endif

#endif /* INCLUDED_extents_h_ */
