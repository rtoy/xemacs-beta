/* unexapollo.c -- COFF File UNEXEC for XEmacs on Apollo SR10.x
   Copyright (C) 1988, 1994 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/* Written by Leonard N. Zubkoff.  */

#include <config.h>
#include <fcntl.h>


#include <a.out.h>
#include <sys/file.h>
#include <apollo/base.h>
#include <apollo/ios.h>
#include <apollo/type_uids.h>
#include <apollo/dst.h>


#define DST_RECORD_HDR_SIZE	2
#define LONG_ALIGN(X)		(((X)+3)&(~3))


void
unexec (target_file_name, source_file_name)
     char *target_file_name, *source_file_name;
{
  struct filehdr file_header;
  struct aouthdr domain_header;
  struct scnhdr *section, *sections, *sections_limit;
  struct scnhdr *first_data_section, *last_data_section;
  struct scnhdr *rwdi_section, *blocks_section;
  struct reloc reloc_entry;
  unsigned long data_size, old_data_section_size, source_file_offset_past_rwdi;
  unsigned char buffer[4096];
  long delta_before_rwdi, delta_after_rwdi, byte_count;
  long first_changed_vaddr, old_rwdi_vaddr, i;
  ios_$id_t target_file, source_file;
  status_$t status;
  /* Open the Source File. */
  if ((source_file = open (source_file_name, O_RDONLY)) < 0)
    signal_error (Qfile_error, "cannot open source file for input", Qunbound);
  /* Read the File Header. */
  if (read (source_file, &file_header, sizeof (file_header)) != sizeof (file_header))
    signal_error (Qfile_error, "cannot read file header", Qunbound);
  
  /* Read the Domain Header. */
  if (read (source_file, &domain_header, sizeof (domain_header))
      != sizeof (domain_header))
    signal_error (Qfile_error, "cannot read domain header", Qunbound);
  /* Read the Section Headers. */
  sections =
    (struct scnhdr *) malloc (file_header.f_nscns*sizeof (struct scnhdr));
  if (sections == (struct scnhdr *) 0)
    out_of_memory ("cannot allocate section header storage", Qunbound);
  sections_limit = sections + file_header.f_nscns;
  if (read (source_file, sections, file_header.f_nscns*sizeof (struct scnhdr))
      != file_header.f_nscns*sizeof (struct scnhdr))
    signal_error (Qfile_error, "cannot read section headers", Qunbound);
  /* Compute the new Size of the Data Section. */
  data_size = sbrk (0) - domain_header.data_start;
  delta_before_rwdi = delta_after_rwdi = data_size - domain_header.dsize;
  old_rwdi_vaddr = 0;
  /* Find and Deallocate the .rwdi Section Information. */
  for (rwdi_section = sections; rwdi_section != sections_limit; rwdi_section++)
    if (strcmp (rwdi_section->s_name, ".rwdi") == 0)
      {
	/* If there are relocation entries, we cannot "unrelocate" them. */
	if (rwdi_section->s_nreloc > 0)
	  signal_error (Qio_error, ".rwdi section needs relocation - cannot dump Emacs", Qunbound);
	delta_after_rwdi = delta_before_rwdi - rwdi_section->s_size;
	old_rwdi_vaddr = rwdi_section->s_vaddr;
	rwdi_section->s_paddr = 0;
	rwdi_section->s_vaddr = 0;
	rwdi_section->s_scnptr = 0;
	rwdi_section->s_size = 0;
	source_file_offset_past_rwdi = (rwdi_section+1)->s_scnptr;
	break;
      }
  /* Skip over the Text Section Headers. */
  for (section = sections; (section->s_flags & STYP_TEXT) != 0; section++) ;
  /*
    Find the First and Last Data Sections and Fixup
    Section Header Relocation Pointers.
    */
  first_data_section = last_data_section = (struct scnhdr *) 0;
  for (; section != sections_limit; section++)
    {
      if ((section->s_flags & STYP_DATA) != 0)
	{
	  if (first_data_section == (struct scnhdr *) 0)
	    first_data_section = section;
	  last_data_section = section;
	}
      if (section->s_relptr != 0)
	section->s_relptr += delta_after_rwdi;
    }
  /* Increment the Size of the Last Data Section. */
  old_data_section_size = last_data_section->s_size;
  last_data_section->s_size += delta_before_rwdi;
  
  /* Update the File Header and Domain Header. */
  file_header.f_symptr += delta_after_rwdi;
  domain_header.dsize = data_size;
  domain_header.bsize = 0;
  /* Skip over subsequent Bss Section Headers. */
  for (section = last_data_section+1;
       (section->s_flags & STYP_BSS) != 0; section++) ;
  /* Update the remaining Section Headers. */
  blocks_section = (struct scnhdr *) 0;
  first_changed_vaddr = 0;
  for (; section != sections_limit; section++)
    {
      long delta = (section < rwdi_section ? delta_before_rwdi : delta_after_rwdi);
      if (section->s_paddr != 0)
	section->s_paddr += delta;
      if (section->s_vaddr != 0)
	{
	  if (first_changed_vaddr == 0)
	    first_changed_vaddr = section->s_vaddr;
	  section->s_vaddr += delta;
	}
      if (section->s_scnptr != 0)
	section->s_scnptr += delta;
      if (strcmp (section->s_name, ".blocks") == 0)
	blocks_section = section;
      else if (strcmp (section->s_name, ".sri") == 0 &&
	       domain_header.o_sri != 0)
	domain_header.o_sri += delta;
      else if (strcmp (section->s_name, ".inlib") == 0 &&
	       domain_header.o_inlib != 0)
	domain_header.o_inlib += delta;
    }
  /* Open the Target File. */
  ios_$create (target_file_name, strlen (target_file_name), coff_$uid,
	       ios_$recreate_mode, ios_$write_opt, &target_file, &status);
  if (status.all != status_$ok)
    signal_error (Qfile_error, "cannot open target file for output", Qunbound);
  /* Write the File Header. */
  if (write (target_file, &file_header, sizeof (file_header)) != sizeof (file_header))
    signal_error (Qfile_error, "cannot write file header", Qunbound);
  /* Write the Domain Header. */
  if (write (target_file, &domain_header, sizeof (domain_header))
      != sizeof (domain_header))
    signal_error (Qfile_error, "cannot write domain header", Qunbound);
  /* Write the Section Headers. */
  if (write (target_file, sections, file_header.f_nscns*sizeof (struct scnhdr))
      != file_header.f_nscns*sizeof (struct scnhdr))
    signal_error (Qfile_error, "cannot write section headers", Qunbound);
  /* Copy the Allocated Sections. */
  for (section = sections; section != first_data_section; section++)
    if (section->s_scnptr != 0)
      CopyData (target_file, source_file, LONG_ALIGN(section->s_size));
  /* Write the Expanded Data Segment. */
  if (write (target_file, first_data_section->s_vaddr, data_size) != data_size)
    signal_error (Qfile_error, "cannot write new data section", Qunbound);
  
  /* Skip over the Last Data Section and Copy until the .rwdi Section. */
  if (lseek (source_file, last_data_section->s_scnptr
	     +old_data_section_size, L_SET) == -1)
    signal_error (Qfile_error, "cannot seek past data section", Qunbound);
  for (section = last_data_section+1; section != rwdi_section; section++)
    if (section->s_scnptr != 0)
      CopyData (target_file, source_file, LONG_ALIGN(section->s_size));
  /* Skip over the .rwdi Section and Copy Remainder of Source File. */
  if (lseek (source_file, source_file_offset_past_rwdi, L_SET) == -1)
    signal_error (Qfile_error, "cannot seek past .rwdi section", Qunbound);
  while ((byte_count = read (source_file, buffer, sizeof (buffer))) > 0)
    if (write (target_file, buffer, byte_count) != byte_count)
      signal_error (Qfile_error, "cannot write data", Qunbound);
  /* Unrelocate .data references to Global Symbols. */
  for (section = first_data_section; section <= last_data_section; section++)
    for (i = 0; i < section->s_nreloc; i++)
      {
	if  (lseek (source_file, section->s_relptr
		    +i*sizeof (struct reloc)-delta_after_rwdi, L_SET) == -1)
	  signal_error (Qfile_error, "cannot seek to relocation info", Qunbound);
	if (read (source_file, &reloc_entry, sizeof (reloc_entry))
	    != sizeof (reloc_entry))
	  signal_error (Qfile_error, "cannot read reloc entry", Qunbound);
	if (lseek (source_file, reloc_entry.r_vaddr-section->s_vaddr
		   +section->s_scnptr, L_SET) == -1)
	  signal_error (Qfile_error, "cannot seek to data element", Qunbound);
	if (lseek (target_file, reloc_entry.r_vaddr-section->s_vaddr
		   +section->s_scnptr, L_SET) == -1)
	  signal_error (Qfile_error, "cannot seek to data element", Qunbound);
	if (read (source_file, buffer, 4) != 4)
	  signal_error (Qfile_error, "cannot read data element", Qunbound);
	if (write (target_file, buffer, 4) != 4)
	  signal_error (Qfile_error, "cannot write data element", Qunbound);
      }
  
  /* Correct virtual addresses in .blocks section. */
  if (blocks_section != (struct scnhdr *) 0)
    {
      dst_rec_t dst_record;
      dst_rec_comp_unit_t *comp_unit;
      unsigned short number_of_sections;
      unsigned long section_base;
      unsigned long section_offset = 0;
      /* Find section tables and update section base addresses. */
      while (section_offset < blocks_section->s_size)
	{
	  if (lseek (target_file,
		     blocks_section->s_scnptr+section_offset, L_SET) == -1)
	    signal_error (Qfile_error, "cannot seek to comp unit record", Qunbound);
	  /* Handle pad records before the comp unit record. */
	  if (read (target_file, &dst_record, DST_RECORD_HDR_SIZE)
	      != DST_RECORD_HDR_SIZE)
	    signal_error (Qfile_error, "cannot read dst record tag", Qunbound);
	  if (dst_record.rec_type == dst_typ_pad)
	    section_offset += DST_RECORD_HDR_SIZE;
	  else if (dst_record.rec_type == dst_typ_comp_unit)
	    {
	      comp_unit = &dst_record.rec_data.comp_unit_;
	      if  (read (target_file, comp_unit, sizeof (*comp_unit))
		   != sizeof (*comp_unit))
		signal_error (Qfile_error, "cannot read comp unit record", Qunbound);
	      if (lseek (target_file, blocks_section->s_scnptr
			 +section_offset
#if dst_version_major == 1 && dst_version_minor < 4
			 +comp_unit->section_table
#else
			 +comp_unit->section_table.rel_offset
#endif
			 +DST_RECORD_HDR_SIZE,
			 L_SET) == -1)
		signal_error (Qfile_error, "cannot seek to section table", Qunbound);
	      if (read (target_file, &number_of_sections, sizeof (number_of_sections))
		  != sizeof (number_of_sections))
		signal_error (Qfile_error, "cannot read section table size", Qunbound);
	      for (i = 0; i < number_of_sections; i++)
		{
		  if (read (target_file, &section_base, sizeof (section_base))
		      != sizeof (section_base))
		    signal_error (Qfile_error, "cannot read section base value", Qunbound);
		  if (section_base < first_changed_vaddr)
		    continue;
		  else if (section_base < old_rwdi_vaddr)
		    section_base += delta_before_rwdi;
		  else section_base += delta_after_rwdi;
		  if (lseek (target_file, -sizeof (section_base), L_INCR) == -1)
		    signal_error (Qfile_error, "cannot seek to section base value", Qunbound);
		  if (write (target_file, &section_base, sizeof (section_base))
		      != sizeof (section_base))
		    signal_error (Qfile_error, "cannot write section base", Qunbound);
		}
	      section_offset += comp_unit->data_size;
	    }
	  else signal_error (Qfile_error, "unexpected dst record type", Qunbound);
	}
    }
  
  if (close (source_file) == -1)
    signal_error (Qfile_error, "cannot close source file", Qunbound);
  if (close (target_file) == -1)
    signal_error (Qfile_error, "cannot close target file", Qunbound);
}


static
CopyData (target_file, source_file, total_byte_count)
     int target_file, source_file;
     long total_byte_count;
{
  unsigned char buffer[4096];
  long byte_count;
  while (total_byte_count > 0)
    {
      if (total_byte_count > sizeof (buffer))
	byte_count = sizeof (buffer);
      else byte_count = total_byte_count;
      if (read (source_file, buffer, byte_count) != byte_count)
	signal_error (Qfile_error, "cannot read data", Qunbound);
      if (write (target_file, buffer, byte_count) != byte_count)
	signal_error (Qfile_error, "cannot write data", Qunbound);
      total_byte_count -= byte_count;
    }
}
