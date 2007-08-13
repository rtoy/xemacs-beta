/* unexec for GNU Emacs on Cygwin32.
   Copyright (C) 1994, 1998 Free Software Foundation, Inc.

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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

*/

/* Adapted from unexnt.c Andy Piper (andyp@parallax.co.uk) 13-1-98 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <config.h>
#include <string.h>

#define PERROR(arg) perror(arg);exit(-1) 

#ifndef HAVE_COFF_H
extern "C" void
unexec (char *, char *, void *, void *,	void *)
{
  PERROR("cannot unexec() coff.h not installed");
}

extern "C" void run_time_remap (char *)
{}
#else

#include <windows.h>
/* 
 * unfortunately we need this c++ to get at the internals of cygwin
 */
class pinfo;
class per_process
{
 public:
  char *initial_sp;

  /* The offset of these 3 values can never change.  */
  /* magic_biscuit is the size of this class and should never change.  */
  int magic_biscuit;
  int version_major;
  int version_minor;

  struct _reent **impure_ptr_ptr;
  char ***envptr;

  /* Used to point to the memory machine we should use -
     usually points back into the dll, but can be overridden by the user.  */
  void *(*malloc)(size_t);
  void (*free)(void *);
  void *(*realloc)(void *, size_t);

  int *fmode_ptr;

  int (*main)(int, char **, char **);
  void (**ctors)();
  void (**dtors)();

  /* For fork */
  void *data_start;
  void *data_end;
  void *bss_start;
  void *bss_end;

  /* For future expansion of values set by the app.  */
  void *public_reserved[4];

  /* The rest are *internal* to cygwin.dll.
     Those that are here because we want the child to inherit the value from
     the parent (which happens when bss is copied) are marked as such.  */

  /* FIXME: Which of these can go elsewhere?  */

  /* FIXME: Delete, make `self' a global.  */
  pinfo *self;		/* pointer only valid in self process */

  /* non-zero of ctors have been run.  Inherited from parent.  */
  int run_ctors_p;

  /* These will be non-zero if the above (malloc,free,realloc) have been
     overridden.  */
  /* FIXME: not currently used */
  int __imp_malloc;
  int __imp_free;
  int __imp_realloc;

  /* Heap management.  Inherited from parent.  */
  void *base;			/* bottom of the heap */
  void *ptr;			/* current index into heap */
  int  size;			/* current size of heap */

  /* Mask of what to trace.  Inherited from parent.
     See sys/strace.h for details.  The value of this is changeable from other
     tasks via the `cygwin' utility so we want this in the shared data area
     (and thus the process table since there's one of these per task).
     However, we also want to turn on stracing as soon as possible and
     therefore before we know which process table entry to use.  So we put it
     here, and have a pointer to it in the process table.  */
  int strace_mask;

  /* Non-zero means the task was forked.  The value is the pid.
     Inherited from parent.  */
  int forkee;

  void *hmodule;

  void* /*HANDLE*/ signal_arrived;
  /* For future expansion, so apps won't have to be relinked if we
     add an item.  */
  void *internal_reserved[9];

  /*  struct file_queue *dq;	 !!! this may need to be nuked ? */
};

extern per_process cygwin_statu; /* pointer into the application's static data */

#include <coff.h>

#define ALLOC_UNIT 0xFFFF
#define ALLOC_MASK ~((unsigned long)(ALLOC_UNIT))
#define ALIGN_ALLOC(addr) \
((((unsigned long)addr) + ALLOC_UNIT) & ALLOC_MASK)
#define SIZEOF_PER_PROCESS (42 * 4)

/*
 * Heap related stuff.
 */
#define get_reserved_heap_size()	(*heap_size)
#define get_committed_heap_size()	\
(int)((unsigned char*)(*heap_index)-(unsigned char*)(*heap_base))
#define get_heap_start()		(*heap_base)
#define get_heap_end()			(*heap_index)

extern "C" {
void** heap_base = &cygwin_statu.base;
void** heap_index = &cygwin_statu.ptr;
int*   heap_size = &cygwin_statu.size;
int*   heap_flag = &cygwin_statu.forkee;
void*  per_process_data = &cygwin_statu;
/* To prevent zero-initialized variables from being placed into the bss
   section, use non-zero values to represent an uninitialized state.  */
#define UNINIT_PTR ((void *) 0xF0A0F0A0)
#define UNINIT_LONG (0xF0A0F0A0L)

void* local_heap_base=UNINIT_PTR;
void* local_heap_index=UNINIT_PTR;
unsigned long local_heap_size=UNINIT_LONG;

/* Recreate the heap created during dumping.  */

enum {
  HEAP_UNINITIALIZED = 1,
  HEAP_UNLOADED,
  HEAP_LOADED
};

/* Basically, our "initialized" flag.  */
int heap_state = HEAP_UNINITIALIZED;

/* So we can find our heap in the file to recreate it.  */
unsigned long heap_index_in_executable = UNINIT_LONG;

static void get_section_info (int a_out, char* a_name);
static void copy_executable_and_dump_data_section (int a_out, int a_new);
static void dump_heap (int a_out, int a_new);
static void dup_file_area(int a_out, int a_new, long size);

/* Cached info about the .data section in the executable.  */
void* data_start_va = UNINIT_PTR;
unsigned long  data_size = UNINIT_LONG;

/* Cached info about the .bss section in the executable.  */
void* bss_start = UNINIT_PTR;
unsigned long  bss_size = UNINIT_LONG;
FILHDR f_hdr;
PEAOUTHDR f_ohdr;
SCNHDR f_data, f_bss, f_text, f_idata;
}
#define PERROR(arg) perror(arg);exit(-1) 
#define CHECK_AOUT_POS(a) \
if (lseek(a_out, 0, SEEK_CUR) != a) \
{ \
  printf("we are at %lx, should be at %lx\n", \
	 lseek(a_out, 0, SEEK_CUR), a); \
  exit(-1); \
}

/* Dump out .data and .bss sections into a new executable.  */
extern "C" void
unexec (char *out_name, char *in_name, void *start_data, void *,	void *)
{
  /* ugly nt hack - should be in lisp */
  char new_name[MAX_PATH], a_name[MAX_PATH];
  char *ptr;
  
  /* Make sure that the input and output filenames have the
     ".exe" extension...patch them up if they don't.  */
  strcpy (a_name, in_name);
  ptr = a_name + strlen (a_name) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (a_name, ".exe");

  strcpy (new_name, out_name);
  ptr = new_name + strlen (new_name) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (new_name, ".exe");
  /* save heap info in our data segment so that we can recreate after
     dumping */

  local_heap_base = *heap_base;
  local_heap_size = *heap_size;
  local_heap_index = *heap_index;
  
  /* We need to round off our heap to NT's allocation unit (64KB).  */
  /* round_heap (get_allocation_unit ()); */

  int a_new, a_out = -1;

  if (a_name && (a_out = open (a_name, O_RDONLY)) < 0)
    {
      PERROR (a_name);
    }
  if ((a_new = creat (new_name, 0666)) < 0)
    {
      PERROR (new_name);
    }

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (a_out, a_name);

  /* Set the flag (before dumping).  */
  heap_state = HEAP_UNLOADED;

  copy_executable_and_dump_data_section (a_out, a_new);
  dump_heap (a_out, a_new);

  close(a_out);
  close(a_new);
}

/* Flip through the executable and cache the info necessary for dumping.  */
static void get_section_info (int a_out, char* a_name)
{
  if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (a_name);
    }

  if (f_hdr.e_magic != DOSMAGIC) 
    {
      PERROR("unknown exe header");
    }

  /* Check the NT header signature ...  */
  if (f_hdr.nt_signature != NT_SIGNATURE) 
    {
      PERROR("invalid nt header");
    }

  /* Flip through the sections for .data and .bss ...  */
  if (f_hdr.f_opthdr > 0)
    {
      if (read (a_out, &f_ohdr, AOUTSZ) != AOUTSZ)
	{
	  PERROR (a_name);
	}
    }
  /* Loop through .data & .bss section headers, copying them in */
  lseek (a_out, sizeof (f_hdr) + f_hdr.f_opthdr, 0);

  if (read (a_out, &f_text, sizeof (f_text)) != sizeof (f_text)
      &&
      strcmp (f_text.s_name, ".text"))
    {
      PERROR ("no .text section");
    }

  /* The .bss section.  */
  if (read (a_out, &f_bss, sizeof (f_bss)) != sizeof (f_bss)
      &&
      strcmp (f_bss.s_name, ".bss"))
    {
      PERROR ("no .bss section");
    }
  extern int my_ebss;
  bss_start = (void *) ((char*)f_ohdr.ImageBase + f_bss.s_vaddr);
  bss_size = (unsigned long)((char*)&my_ebss-(char*)bss_start);
  
  /* must keep bss data that we want to be blank as blank */
  printf("found bss - keeping %lx of %lx bytes\n", bss_size, f_ohdr.bsize);

  /* The .data section.  */
  if (read (a_out, &f_data, sizeof (f_data)) != sizeof (f_data)
      &&
      strcmp (f_data.s_name, ".data"))
    {
      PERROR ("no .data section");
    }

  /* From lastfile.c  */
  extern char my_edata[];

  /* The .data section.  */
  data_start_va = (void *) ((char*)f_ohdr.ImageBase + f_data.s_vaddr);

  /* We want to only write Emacs data back to the executable,
     not any of the library data (if library data is included,
     then a dumped Emacs won't run on system versions other
     than the one Emacs was dumped on).  */
  data_size = my_edata - data_start_va;

  /* The .idata section.  */
  if (read (a_out, &f_idata, sizeof (f_idata)) != sizeof (f_idata)
      &&
      strcmp (f_idata.s_name, ".idata"))
    {
      PERROR ("no .idata section");
    }
}

/* The dump routines.  */

static void
copy_executable_and_dump_data_section (int a_out, int a_new)
{
  long size=0;
  unsigned long new_data_size, new_bss_size, f_data_s_vaddr,
    file_sz_change, f_data_s_scnptr, bss_padding;
  int i;
  SCNHDR section;
  /* calculate new sizes f_ohdr.dsize is the total initalized data
     size on disk which is f_data.s_size + f_idata.s_size. 
     f_ohdr.data_start is the base addres of all data and so should 
     not be changed. *.s_vaddr is the virtual address of the start
     of the section normalzed from f_ohdr.ImageBase. *.s_paddr
     appears to be the number of bytes in the section actually used
     (whereas *.s_size is aligned).

     bsize is now 0 since subsumed into .data
     dsize is dsize + (f_data.s_vaddr - f_bss.s_vaddr)
     f_data.s_vaddr is f_bss.s_vaddr
     f_data.s_size is new dsize maybe.
     what about s_paddr & s_scnptr?  */
  /* this is the amount the file increases in size */
  *heap_flag=1;			// kludge to get mem to remap
  new_bss_size=f_data.s_vaddr - f_bss.s_vaddr;
  file_sz_change=new_bss_size;
  new_data_size=f_ohdr.dsize + new_bss_size;
  f_data_s_scnptr = f_data.s_scnptr;
  f_data_s_vaddr = f_data.s_vaddr;
  f_data.s_vaddr = f_bss.s_vaddr;
  f_data.s_paddr += new_bss_size;

  if (f_data.s_size + f_idata.s_size != f_ohdr.dsize)
    {
      printf("section size doesn't tally with dsize %lx != %lx\n", 
	     f_data.s_size + f_idata.s_size, f_ohdr.dsize);
    }
  f_data.s_size += new_bss_size;
  lseek (a_new, 0, SEEK_SET);
  /* write file header */
  f_hdr.f_symptr += file_sz_change;
  f_hdr.f_nscns--;
  printf("writing file header\n");
  if (write(a_new, &f_hdr, sizeof(f_hdr)) != sizeof(f_hdr))
    {
      PERROR("failed to write file header");
    }
  /* write optional header fixing dsize & bsize*/
  printf("writing optional header\n");
  printf("new data size is %lx, >= %lx\n", new_data_size,
	 f_ohdr.dsize + f_ohdr.bsize);
  if (new_data_size < f_ohdr.dsize + f_ohdr.bsize )
    {
      PERROR("new data size is < approx");
    }
  f_ohdr.dsize=new_data_size;
  f_ohdr.bsize=0;
  if (write(a_new, &f_ohdr, sizeof(f_ohdr)) != sizeof(f_ohdr))
    {
      PERROR("failed to write optional header");
    }
  /* write text as is */
  printf("writing text header (unchanged)\n");

  if (write(a_new, &f_text, sizeof(f_text)) != sizeof(f_text))
    {
      PERROR("failed to write text header");
    }

  /* write new data header */
  printf("writing .data header\n");

  if (write(a_new, &f_data, sizeof(f_data)) != sizeof(f_data))
    {
      PERROR("failed to write data header");
    }
  
  printf("writing .idata header\n");
  f_idata.s_scnptr += file_sz_change;
  if (f_idata.s_lnnoptr != 0) f_idata.s_lnnoptr += file_sz_change;
  if (f_idata.s_relptr != 0) f_idata.s_relptr += file_sz_change;
  if (write(a_new, &f_idata, sizeof(f_idata)) != sizeof(f_idata))
    {
      PERROR("failed to write idata header");
    }
  
  /* copy other section headers adjusting the file offset */
  for (i=0; i<(f_hdr.f_nscns-3); i++)
    {
      if (read (a_out, &section, sizeof (section)) != sizeof (section))
	{
	  PERROR ("no .data section");
	}
      
      section.s_scnptr += file_sz_change;
      if (section.s_lnnoptr != 0) section.s_lnnoptr += file_sz_change;
      if (section.s_relptr != 0) section.s_relptr += file_sz_change;

      if (write(a_new, &section, sizeof(section)) != sizeof(section))
	{
	  PERROR("failed to write data header");
	}
    }

  /* dump bss to maintain offsets */
  memset(&f_bss, 0, sizeof(f_bss));
  if (write(a_new, &f_bss, sizeof(f_bss)) != sizeof(f_bss))
    {
      PERROR("failed to write bss header");
    }

  
  size=lseek(a_new, 0, SEEK_CUR);
  CHECK_AOUT_POS(size);

  /* copy eveything else until start of data */
  size = f_data_s_scnptr - lseek (a_out, 0, SEEK_CUR);

  printf ("copying executable up to data section ... %lx bytes\n", 
	  size);
  dup_file_area(a_out, a_new, size);

  CHECK_AOUT_POS(f_data_s_scnptr);

  /* dump bss + padding between sections */
  printf ("dumping .bss into executable... %lx bytes\n", bss_size);
  if (write(a_new, bss_start, bss_size) != (int)bss_size)
    {
      PERROR("failed to write bss section");
    }
  /* pad, needs to be zero */
  bss_padding = new_bss_size - bss_size;
  printf ("padding .bss ... %lx bytes\n", bss_padding);
  void* empty_space = malloc(bss_padding);
  memset(empty_space, 0, bss_padding);
  if (write(a_new, empty_space, bss_padding) != (int)bss_padding)
    {
      PERROR("failed to write bss section");
    }
  free(empty_space);

  /* Get a pointer to the raw data in our address space.  */
  printf ("dumping .data section... %lx bytes\n", data_size);
  if (write(a_new, data_start_va, data_size) != (int)data_size)
    {
      PERROR("failed to write data section");
    }
  
  lseek(a_out, f_data_s_scnptr + data_size, SEEK_SET);

  size = (((unsigned long)per_process_data-f_ohdr.ImageBase)-f_data_s_vaddr)
    - data_size;
  /* write rest of .data to cygwin per process */
  printf ("copying from .data to cygwin per_process... %lx bytes\n", size);
  dup_file_area(a_out, a_new, size);

  /* now write out the per process information */
  printf ("dumping to cygwin per_process... %x bytes at %p\n", 
	  SIZEOF_PER_PROCESS, per_process_data);

  per_process newpp;
  memset(&newpp, 0, SIZEOF_PER_PROCESS);
  newpp.base = cygwin_statu.base;
  newpp.ptr = cygwin_statu.ptr;
  newpp.size = cygwin_statu.size;

  if (write(a_new, &newpp, SIZEOF_PER_PROCESS)
      != (int)SIZEOF_PER_PROCESS)
    {
      PERROR("failed to write per_process info");
    }
  free(empty_space);

  /* dump the rest */
  size = lseek(a_out, SIZEOF_PER_PROCESS, SEEK_CUR);
  size = f_idata.s_scnptr - size;
  dup_file_area(a_out, a_new, size);

  //  lseek(a_out, f_idata.s_scnptr, SEEK_CUR);
  CHECK_AOUT_POS(f_idata.s_scnptr);
  /* now dump - idata don't need to do this cygwin ds is in .data! */
  printf ("dumping .idata section... %lx bytes\n", f_idata.s_size);

  dup_file_area(a_out,a_new,f_idata.s_size);

  /* write rest of file */
  printf ("writing rest of file\n");
  size = lseek(a_out, 0, SEEK_END);
  size = size - (f_idata.s_scnptr + f_idata.s_size); /* length remaining in a_out */
  lseek(a_out, f_idata.s_scnptr + f_idata.s_size, SEEK_SET);

  dup_file_area(a_out, a_new, size);
}

/*
 * copy from aout to anew
 */
static void dup_file_area(int a_out, int a_new, long size)
{
  char page[BUFSIZ];
  long n;
  for (; size > 0; size -= sizeof (page))
    {
      n = size > sizeof (page) ? sizeof (page) : size;
      if (read (a_out, page, n) != n || write (a_new, page, n) != n)
	{
	  PERROR ("dump_out()");
	}
    }
}

static void dump_heap (int a_out, int a_new)
{
    void *heap_data;
    unsigned long heap_size;

    printf ("Dumping heap into executable...\n");

    heap_size = get_committed_heap_size ();
    heap_data = get_heap_start ();

    printf ("heap start in process - %p \n", heap_data);
    printf ("heap size in bytes %lx\n", heap_size);
    
    /* nt version rounds heap start - don't see why we should */    
    heap_index_in_executable = lseek(a_new, 0, SEEK_CUR);

    if (write(a_new, heap_data, heap_size) != (int)heap_size)
      {
	PERROR("failed to write data section");
      }
}

extern "C" void run_time_remap (char *a_name)
{
  int a_out=-1;

  if ((*heap_base)!=local_heap_base
      ||(*heap_index)<local_heap_index
      ||(*heap_size)<local_heap_size)
    {
      PERROR("heap parameters not in bss");
    }
  
  if (a_name && (a_out = open (a_name, O_RDONLY)) < 0)
    {
      PERROR (a_name);
    }

  /* load the heap */
  lseek(a_out, heap_index_in_executable, SEEK_SET);
  
  if (read (a_out, get_heap_start(), 
	    (int)((unsigned char*)(local_heap_index)
		  -(unsigned char*)(local_heap_base))) < 0)
    {
      PERROR (a_name);
    }
  close(a_out);

  /* switch to new heap */
  heap_state=HEAP_LOADED;
  *heap_flag=0;
  
  close(a_out);
}

#endif /* HAVE_COFF_H */
