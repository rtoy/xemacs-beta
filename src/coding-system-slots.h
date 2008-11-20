/* Definitions of marked slots in coding systems
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

/* Synched up with: ????.  Split out of file-coding.h. */

/* We define the Lisp_Objects in the coding system structure in a separate
   file because there are numerous places we want to iterate over them,
   such as when defining them in the structure, initializing them, or
   marking them.

   To use, define MARKED_SLOT before including this file.  In the structure
   definition, you also need to define CODING_SYSTEM_SLOT_DECLARATION.  No
   need to undefine either value; that happens automatically.  */

#ifndef MARKED_SLOT_ARRAY
#ifdef CODING_SYSTEM_SLOT_DECLARATION
#define MARKED_SLOT_ARRAY(slot, size) MARKED_SLOT(slot[size])
#else
#define MARKED_SLOT_ARRAY(slot, size) do {		\
    int mslotidx;					\
    for (mslotidx = 0; mslotidx < size; mslotidx++)	\
      {							\
	MARKED_SLOT (slot[mslotidx])			\
      }							\
  } while (0);
#endif
#endif /* not MARKED_SLOT_ARRAY */

  /* Name and description of this coding system.  The description
     should be suitable for a menu entry. */
  MARKED_SLOT (name)
  MARKED_SLOT (description)

  /* Mnemonic string displayed in the modeline when this coding
     system is active for a particular buffer. */
  MARKED_SLOT (mnemonic)

  /* Long documentation on the coding system. */
  MARKED_SLOT (documentation)
  /* Functions to handle additional conversion after reading or before
     writing. #### This mechanism should be replaced by the ability to
     simply create new coding system types. */
  MARKED_SLOT (post_read_conversion)
  MARKED_SLOT (pre_write_conversion)

  /* If this coding system is not of the correct type for text file
     conversion (i.e. decodes byte->char), we wrap it with appropriate
     char<->byte converters.  This is created dynamically, when it's
     needed, and cached here. */
  MARKED_SLOT (text_file_wrapper)

  /* ------------------------ junk to handle EOL -------------------------
     I had hoped that we could handle this without lots of special-case
     code, but it appears not to be the case if we want to maintain
     compatibility with the existing way.  However, at least with the way
     we do things now, we avoid EOL junk in most of the coding system
     methods themselves, or in the decode/encode functions.  The EOL
     special-case code is limited to coding-system creation and to the
     convert-eol and undecided coding system types. */

  /* If this coding system wants autodetection of the EOL type, then at the
     appropriate time we wrap this coding system with
     convert-eol-autodetect. (We do NOT do this at creation time because
     then we end up with multiple convert-eols wrapped into the final
     result -- esp. with autodetection using `undecided' -- leading to a
     big mess.) We cache the wrapped coding system here. */
  MARKED_SLOT (auto_eol_wrapper)
  
  /* Subsidiary coding systems that specify a particular type of EOL
     marking, rather than autodetecting it.  These will only be non-nil
     if (eol_type == EOL_AUTODETECT).  These are chains. */
  MARKED_SLOT_ARRAY (eol, 3)
  /* If this coding system is a subsidiary, this element points back to its
     parent. */
  MARKED_SLOT (subsidiary_parent)

  /* At decoding or encoding time, we use the following coding system, if
     it exists, in place of the coding system object.  This is how we
     handle coding systems with EOL types of CRLF or CR.  Formerly, we did
     the canonicalization at creation time, returning a chain in place of
     the original coding system; but that interferes with
     `coding-system-property' and causes other complications.  CANONICAL is
     used when determining the end types of a coding system.
     canonicalize-after-coding also consults CANONICAL (it has to, because
     the data in the lstream is based on CANONICAL, not on the original
     coding system). */
  MARKED_SLOT (canonical)

  MARKED_SLOT (safe_charsets)

  MARKED_SLOT (safe_chars)

#undef MARKED_SLOT
#undef MARKED_SLOT_ARRAY
#undef CODING_SYSTEM_SLOT_DECLARATION
