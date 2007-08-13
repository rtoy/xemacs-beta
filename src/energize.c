/****************************************************************************
 ***
 ***	Copyright (c) 1990 by Sun/Lucid,	All Rights Reserved.
 ***	Copyright (c) 1991-1993 by Lucid, Inc.  All Rights Reserved.
 ***
 *****************************************************************************/

/* Synched up with: Not in FSF. */

#include <config.h>

#ifdef ENERGIZE		/* whole file */

#include "lisp.h"

/* Display Context for the icons */
#include "console-x.h"
#include <Xm/DialogS.h>
#include "lwlib.h"
#include "objects-x.h"

#include "events.h"
#include "opaque.h"
#include "buffer.h"
#include "extents.h"
#include "process.h"
#include "insdel.h"
#include "window.h"
#include "faces.h"

/* Energize editor requests and I/O operations */
#include "energize.h"

#include "systime.h"
#include "sysfile.h"
#include "syssignal.h"

#ifndef CBFileYourself
/* This means that emacs is being compiled against the connection library
   and headers that go with an Energize protocol less than 0.10.  We need
   to do some slightly different things in this file because of that.

   Note that if Emacs is compiled against the 3.0 version of the connection
   library and associated headers, it can still talk to 2.1- or 2.5-level
   servers.  But the opposite is not true.
 */
# define ENERGIZE_V2_HEADERS
#endif

/* The Connection library
 */
extern void CWriteQueryChoicesRequest ();
extern void CWriteExecuteChoicesRequest ();
extern void CWriteSheetRequest ();
extern void CWriteSetControlRequest ();
extern void CWriteChoice ();
extern void CWriteProtocol ();
extern int  CGetPortNumber ();


/************** Typedefs and Structs ***********************/

/* structure argument used by the next mapping function */
typedef struct
{
  BufferInfo *binfo;
  int n_extents;
} binfo_and_n_extents;

typedef struct
{
  BufferInfo*	binfo;
  int		state;
  int		tell_energize;
} binfo_and_state;

struct reply_wait
{
  int		serial;
  EId		objectId;
  EId		genericId;
  EId		itemId;
  char		answered_p;
  char		status;
  char*		message;
  Lisp_Object	menu_result;
  Lisp_Object	only_name;
  struct reply_wait*	next;
};

static struct reply_wait *global_reply_wait;

Lisp_Object Venergize_kernel_busy;
Lisp_Object Qenergize_kernel_busy;
Lisp_Object Venergize_attributes_mapping;
int energize_extent_gc_threshold;
Lisp_Object Venergize_kernel_busy_hook;
Lisp_Object Qenergize_kernel_busy_hook;
Lisp_Object Venergize_menu_update_hook;
Lisp_Object Qenergize_menu_update_hook;
Lisp_Object Qenergize;
Lisp_Object Qenergize_auto_revert_buffer;


static void set_energize_extent_data (EXTENT extent, void *data);

static char *kernel_buffer_type_to_elisp_type (char *kernel_type);

static CONST void *get_object (EId id, BufferInfo *binfo);
static void put_object (EId id, BufferInfo *binfo, void *object);
static void remove_object (EId id, BufferInfo *binfo);
static void free_GDataclass (GDataClass *cl, BufferInfo *binfo);

static void free_GenericData (GenericData *gen, BufferInfo *binfo);

static void free_Energize_Extent_Data (Energize_Extent_Data *, BufferInfo *,
				       enum Energize_Object_Free_Type);

static BufferInfo *get_buffer_info_for_emacs_buffer (Lisp_Object emacs_buf,
						     Editor *editor);
static void put_buffer_info (EId id, Lisp_Object emacs_buf,
			     BufferInfo *binfo, Editor *editor);

static void handle_sheet_control_change (Widget, EId sheet_id, void* arg);
static Connection *make_energize_connection (Editor *editor,
					     int fdin, int fdout);
static void close_energize_connection (void);
Lisp_Object Fclose_connection_to_energize (void);
static void mark_all_extents_as_unmodified (BufferInfo *binfo);
Lisp_Object Fenergize_barf_if_buffer_locked (void);
Lisp_Object Fconnected_to_energize_p (void);
static int get_energize_connection_and_buffer_id (Lisp_Object buffer,
						  void **conn_ptr,
						  long *buffer_id_ptr);

void restore_energize_extent_state (EXTENT);


/**************************** Variables *****************************/

/* debugging variable */
int ignore_kernel;

Lisp_Object Venergize_kernel_modification_hook;
Lisp_Object Venergize_create_buffer_hook;
Lisp_Object Qenergize_create_buffer_hook;

Lisp_Object Qenergize_buffer_modified_hook;
Lisp_Object Qbuffer_locked_by_energize;
Lisp_Object Qenergize_user_input_buffer_mark;
Lisp_Object Qenergize_make_many_buffers_visible;

int inside_parse_buffer;

/* List of all buffers currently managed by Energize.  This is
Staticpro'ed so that they don't get GC'ed from under us. */
static Lisp_Object Venergize_buffers_list;

static Editor *energize_connection;
static protocol_edit_options *peo;

static int request_serial_number;

extern int current_debuggerpanel_exposed_p;
extern int desired_debuggerpanel_exposed_p;
extern int debuggerpanel_sheet;

/**************************** Macros *****************************/

#define xnew(type) ((type*)xmalloc (sizeof (type)))

#define BUFFER_NOTIFY_BACKGROUND_BIT_SET_P(buffer) 1

#define get_extent_data(id,binfo) (Energize_Extent_Data*)get_object(id, binfo)
#define get_class(id,binfo) (GDataClass*)get_object(id, binfo)
#define get_generic(id,binfo) (GenericData*)get_object(id, binfo)

#define put_extent_data(id,binfo,obj) put_object(id, binfo, obj)
#define put_class(id,binfo,obj) put_object(id, binfo, obj)
#define put_generic(id,binfo,obj) put_object(id, binfo, obj)

#define remove_extent_data(id,binfo) remove_object(id, binfo)
#define remove_class(id,binfo) remove_object(id, binfo)
#define remove_generic(id,binfo) remove_object(id, binfo)

#define DEBUGGER_PSHEET_NAME  "DEBUGGER_P_SHEET"

/* special graphics attribute meaning "use what anyone else's attributes" */
#define GA_NO_CHANGE 0
/* this number should be bigger than any of the "real" GA's */
#define GA_MAX 0x1000

/**************************** Utilities *****************************/

static int
emacs_CWriteRequestBuffer (Connection* conn)
{
  int result;
  /* don't kill emacs with SIGPIPE */
  SIGTYPE (*old_sigpipe)() =
    (SIGTYPE (*) ()) signal (SIGPIPE, SIG_IGN);

  result = CWriteRequestBuffer (conn);	/* the real one; macroized later */
  signal (SIGPIPE, old_sigpipe);
  return result;
}

#define CWriteRequestBuffer emacs_CWriteRequestBuffer


static Energize_Extent_Data *
extent_to_data (Lisp_Object extent_obj)
{
  Energize_Extent_Data *ext = 0;

  if (!EXTENTP (extent_obj))
    return 0;
  else
    ext = energize_extent_data (XEXTENT (extent_obj));

  if (ext)
    {
      if (EQ (ext->extent, extent_obj))
	return ext;
      else
	abort ();
    }
  else
    return 0;
}


static Lisp_Object
data_to_extent (Energize_Extent_Data *ext)
{
  Lisp_Object extent = ext->extent;
  assert (EXTENTP (extent));
  return extent;
}

/* duplicate a string */
static char*
copy_string (char *s)
{
  if (!s)
    return 0;
  else
    {
      int len = strlen (s);
      char *res = (char *) xmalloc (len + 1);
      return strcpy (res, s);
    }
}

/* Get objects from the hashtables */
static CONST void *
get_object_internal (EId id, c_hashtable table)
{
  void *res;
  CONST void *found = gethash ((void*)id, table, &res);

  if (found) CHECK_OBJECT (res);

  return found ? res : 0;
}

static CONST void *
get_object (EId id, BufferInfo *binfo)
{
  return get_object_internal (id, binfo->id_to_object);
}

static void
put_object_internal (EId id, c_hashtable table, void *object)
{
  if (!PUT_ABLE_OBJECT (object))
    error ("Can't put 0x%x in table", object);
  CHECK_OBJECT (object);
  puthash ((void*)id, object, table);
}

static void
put_object (EId id, BufferInfo *binfo, void *object)
{
  put_object_internal (id, binfo->id_to_object, object);
  return;
}

static void
remove_object_internal (EId id, c_hashtable table)
{
  void *res;

  if (gethash ((void*)id, table, &res))
    {
      if (OBJECT_FREE (res))
	error ("Free'd object 0x%x still in table!", res);
      remhash ((void*)id, table);
    }
  else if (id)
    /* #### If this happens for Energize_Extent_Data as a result of extent
       finalization, this aborts (because gc_in_progress).  These errors are
       awfully bad, so probably they should just be abort()s anyway... */
    error ("EId %d not in table!", id);
}

static void
remove_object (EId id, BufferInfo *binfo)
{
  remove_object_internal (id, binfo->id_to_object);
  return;
}

/* maphash_function called by free_buffer_info */
static void
free_object (void *key, void *contents, void *arg)
{
  BufferInfo *binfo = arg;

  if (contents)
    {
      switch (OBJECT_SEAL (contents))
	{
	case BUF_INFO_SEAL:
	  break;
	case EXTENT_SEAL:
	  free_Energize_Extent_Data ((Energize_Extent_Data *) contents,
				     binfo, OFT_MAPHASH);
	  break;
	case GDATA_CLASS_SEAL:
	  free_GDataclass ((GDataClass *) contents, binfo);
	  break;
	case GDATA_SEAL:
	  free_GenericData ((GenericData *) contents, binfo);
	  break;
	default:
	  error ("Bad argument 0x%x to free_object()", contents);
	  return;
	}
    }
}

static GDataClass *
alloc_GDataclass (EId id, BufferInfo *binfo)
{
  GDataClass *cl = xnew (GDataClass);
  memset (cl, 0, sizeof (GDataClass));
  cl->seal = GDATA_CLASS_SEAL;
  cl->id = id;
  put_class (cl->id, binfo, cl);
  return cl;
}

static void
free_GDataclass (GDataClass *cl, BufferInfo *binfo)
{
  if (cl)
    {
      remove_class (cl->id, binfo);
      SET_OBJECT_FREE (cl);
    }
  return;
}


static GenericData *
alloc_GenericData (EId id, GDataClass *cl, BufferInfo *binfo)
{
  GenericData *gen = xnew (GenericData);
  gen->seal = GDATA_SEAL;
  gen->id = id;
  gen->cl = cl;
/*  gen->image = 0;*/
  gen->flags = 0;
  gen->modified_state = 0;
  put_generic (gen->id, binfo, gen);
  return gen;
}

static void
free_GenericData (GenericData *gen, BufferInfo *binfo)
{
  if (gen)
    {
      remove_generic (gen->id, binfo);
      gen->cl = 0;
      SET_OBJECT_FREE (gen);
    }
  return;
}

/* Called to flush the extent from the hash table when Energize tells us to
   lose the extent.  This is NOT called from the extent GC finalization method,
   because there would be a period before the next GC when we still had an
   Energize ID that the server thought was dead, and could concievably reuse.

   Since we protect extents from GC until Energize says they're done, if an
   extent still has Energize data by the time it gets collected, something is
   fucked.
 */
static void
free_Energize_Extent_Data (Energize_Extent_Data *ext, BufferInfo *binfo,
			   enum Energize_Object_Free_Type free_type)
{
  if (ext)
    {
      Lisp_Object extent_obj = data_to_extent (ext);

      /* Remove the extent, remove the extent's pointer to the data,
	 and the data's pointer to the extent. */
      Fdetach_extent (extent_obj);
      set_energize_extent_data (XEXTENT (extent_obj), 0);
      ext->extent = Qnil; /* at this point, refs will abort */

      /* Remove the data from the hash table, and mark it as dead. */
      remove_extent_data (ext->id, binfo);
      ext->id = 0;

      /* don't free this "sub-guy" via maphash, as it will get taken care
	 of during the course of the maphash without our doing anything */
      if (free_type != OFT_MAPHASH)
	{
	  if (ext->extentType == CEGeneric)
	    free_GenericData (ext->u.generic.gData, binfo);
	}

      SET_OBJECT_FREE (ext);
    }
  return;
}

static BufferInfo *
alloc_BufferInfo (EId id, Lisp_Object name, Lisp_Object filename,
		  char *class_str, Editor *editor, Window win, int nobjects)
{
  BufferInfo *binfo = xnew (BufferInfo);
  Widget nw = 0;
  Lisp_Object buffer = Qnil;

  if (win)
    {
      char win_as_string [16];
      nw = XtWindowToWidget (get_x_display (Qnil), win);
      if (nw)
	nw = XtParent (nw);
      
      if (nw)
	sprintf (win_as_string, "w%x", nw);
      else
	sprintf (win_as_string, "0x%x", win);
      binfo->frame =
	Fx_create_frame (Qnil, Qnil, build_string (win_as_string));
    }
  else
    binfo->frame = Qnil;

  /* try to re-use a buffer with the same file name if one already exists.
   * If a buffer already exists but is modified we should do a dialog and
   * ask the user what to do.  For now I'll just use a new buffer in that case.
   * ParseBuffer will erase the buffer.
   */
  if (!NILP (filename))
    {
      int offct = find_file_compare_truenames;
      find_file_compare_truenames = 1;
      buffer = Fget_file_buffer (filename);
      find_file_compare_truenames = offct;

      if (!NILP (buffer) && !NILP (Fbuffer_modified_p (buffer)))
	buffer = Qnil;
    }

  if (NILP (buffer))
    buffer = Fget_buffer_create (Fgenerate_new_buffer_name (name, Qnil));

  binfo->seal = BUF_INFO_SEAL;
  binfo->id = id;
  binfo->flags = 0;
  binfo->editor = editor;
  binfo->id_to_object = make_hashtable (nobjects);
  binfo->emacs_buffer = buffer;
  binfo->modified_state = 0;
  binfo->editable = 0;
  binfo->output_mark = Qnil;
  binfo->buffer_type = kernel_buffer_type_to_elisp_type (class_str);
  binfo->p_sheet_ids = 0;
  binfo->n_p_sheets = 0;
  binfo->note_ids = 0;
  binfo->n_notes = 0;
#ifdef I18N4
  binfo->wcmap.valid = 0;
  binfo->wcmap.modiff_stamp = -1;
  binfo->wcmap.map = NULL;
#endif   
  put_buffer_info (id, binfo->emacs_buffer, binfo, editor);

  Venergize_buffers_list = Fcons (buffer, Venergize_buffers_list);

#if 0
 *  if (nw){
 *    Lisp_Object window = Fframe_selected_window (binfo->frame);
 *    Fset_window_buffer (window, binfo->emacs_buffer);
 *    set_text_widget ((NoteWidget)nw,
 *		     FRAME_X_SHELL_WIDGET (XFRAME (binfo->frame)));
 *  }
#endif

  return binfo;
}

/* free a buffer_info */
static void
free_buffer_info (BufferInfo *binfo)
{
  maphash (free_object, binfo->id_to_object, (void *)binfo);
  free_hashtable (binfo->id_to_object);
  if (energize_connection && energize_connection->binfo_hash)
    {
      if (binfo->id)
	remhash ((void *)binfo->id, energize_connection->binfo_hash);
      if (!NILP (binfo->emacs_buffer))
	{
	  remhash (LISP_TO_VOID (binfo->emacs_buffer),
		   energize_connection->binfo_hash);
	}
    }
  binfo->id = 0;
  binfo->emacs_buffer = Qnil;
#ifdef I18N4
  if (binfo->wcmap.valid) {
    binfo->wcmap.valid= 0;
    xfree(binfo->wcmap.map);
  }
#endif 
  SET_OBJECT_FREE (binfo);
}

/* hash for BufferInfo structures */
static BufferInfo*
get_buffer_info_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor)
{
  BufferInfo *res;
  if (!editor || !editor->binfo_hash)
    return 0;
  else
    {
      void *vbuf;
      /* #### Probably should use a Lisp hash table for this; what if the last
	 pointer to the buffer was in the editor struct? */
      return (gethash (LISP_TO_VOID (emacs_buf),
		       editor->binfo_hash,
		       (void *) &res)
	      ? res : 0);
    }
}



/* Called by mark_buffer.  It is possible for the last remaining pointer to
   an extent object to be in an Energize_Extent_Data structure that is pointed
   at by the binfo->id_to_object table.  Since Energize may still reference
   this object by its id (in fact, I think it may even "ressurect" a detached
   extent) we must prevent the extent from being garbage collected.  Aside 
   from the obvious lossage (that the extent itself would be trashed) this
   would also cause us to free the Energize_Extent_Data which the server still
   believes we have.  The buffers all get marked because they're on the
   `Venergize_buffers_list'.

   So, an Energize extent or buffer only ever gets collected when the server
   has claimed that it is done with it (or when the connection is closed).

   Of course, by keeping pointers to lisp objects in C structs under non-lisp
   hash tables, we again build in the assumption that GC never relocates.
 */

/* FUCK!!  It's not standard-conforming to cast pointers to functions
   to or from void*.  Give me a fucking break! */
struct markobj_kludge_fmh 
{
  void (*markobj) (Lisp_Object);
};

static void
mark_energize_buffer_data_extent_mapper (void *key, void *val, void *arg)
{
  if (OBJECT_SEAL (val) == EXTENT_SEAL)
    {
      struct markobj_kludge_fmh *fmh = arg;
      struct Energize_Extent_Data *ext = (Energize_Extent_Data *) val;
      /* Lisp_Object extent = data_to_extent (ext);  (will abort if marked) */
      Lisp_Object extent = ext->extent;
      assert (GC_EXTENTP (extent));
      ((*fmh->markobj) (extent));
    }
}

void
mark_energize_buffer_data (struct buffer *b,
			   void (*markobj) (Lisp_Object))
{
  struct markobj_kludge_fmh fmh;
  Lisp_Object buffer;
  BufferInfo *binfo;
  if (!energize_connection || !energize_connection->binfo_hash)
    return;
  XSETBUFFER (buffer, b);
  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
  if (! binfo)
    return;
  fmh.markobj = markobj;
  maphash (mark_energize_buffer_data_extent_mapper, binfo->id_to_object, &fmh);
}



struct buffer_and_sheet_ids
{
  EId	buffer_id;
  EId	sheet_id;
};

static void
find_sheet_id (void* key, void* contents, void* arg)
{
  BufferInfo* binfo = (BufferInfo*)contents;
  EId buffer_id = (EId)key;
  struct buffer_and_sheet_ids* result = (struct buffer_and_sheet_ids*)arg;
  int i;

  if (!result->buffer_id)
    for (i = 0; i < binfo->n_p_sheets; i++)
      if (binfo->p_sheet_ids [i] == result->sheet_id)
	{
	  result->buffer_id = buffer_id;
	  return;
	}
}

static EId
buffer_id_of_sheet (EId id)
{
  Editor *editor = energize_connection;
  struct buffer_and_sheet_ids basi;
  if (!energize_connection)
    return 0;

  basi.buffer_id = 0;
  basi.sheet_id = id;
  maphash (find_sheet_id, editor->binfo_hash, (void*)&basi);
  return basi.buffer_id;
}

static long
get_energize_buffer_id (Lisp_Object emacs_buf)
{
  Editor *editor = energize_connection;
  BufferInfo *res;

  if (!editor || !editor->binfo_hash)
    return -1;
  else if (!gethash (LISP_TO_VOID (emacs_buf), editor->binfo_hash, (void *)&res))
    return -1;
  else
    return (long) res->id;
}

static char *
kernel_buffer_type_to_elisp_type (char *kernel_type)
{
  struct buffer_type_struct *bts =
    kernel_buffer_types_to_elisp_buffer_types_vector;
  char *elisp_type = 0;

  if (!kernel_type)
    return UNINITIALIZED_BUFFER_TYPE;

  while (bts->kernel_name)
    {
      if (!strcmp (bts->kernel_name, kernel_type))
	{
	  elisp_type = bts->elisp_name;
	  break;
	}
      bts++;
    }

  if (!elisp_type)
    return kernel_type;
  else
    return elisp_type;
}

static Lisp_Object
get_buffer_type_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor)
{
  BufferInfo *binfo;
  if (!(binfo = get_buffer_info_for_emacs_buffer (emacs_buf, editor)))
    return Qnil;
  else
    {
      if (!binfo->buffer_type) binfo->buffer_type =
	UNINITIALIZED_BUFFER_TYPE;

      return intern (binfo->buffer_type);
    }
}

static Lisp_Object
set_buffer_type_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor,
				    Lisp_Object type)
{
  BufferInfo *binfo;
  if (!(binfo = get_buffer_info_for_emacs_buffer (emacs_buf, editor)))
    return Qnil;
  else
    {
      char *type_string;

      if (NILP (type)) return Qnil;

      if (SYMBOLP (type))
	XSETSTRING (type, XSYMBOL (type)->name);

      if (STRINGP (type))
	type_string = (char *)XSTRING_DATA (type);

      type_string = copy_string (type_string);

      if (!type_string) return Qnil;

      binfo->buffer_type = type_string;

      return intern (binfo->buffer_type);
    }
}

static BufferInfo*
get_buffer_info_for_id (EId id, Editor *editor)
{
  BufferInfo *res;
  return (gethash ((void *)id, editor->binfo_hash, (void *)&res))?res:0;
}

static void
put_buffer_info (EId id, Lisp_Object emacs_buf, BufferInfo *binfo,
		 Editor *editor)
{
  puthash ((void *)id, binfo, editor->binfo_hash);
  puthash (LISP_TO_VOID (emacs_buf), binfo, editor->binfo_hash);
}

static void
remove_buffer_info (EId id, Lisp_Object emacs_buf, Editor *editor)
{
  void *vbuf;
  remhash ((void *)id, editor->binfo_hash);
  remhash (LISP_TO_VOID (emacs_buf), editor->binfo_hash);
}


/* Conversion between Energize and Emacs buffer positions */

#if defined(I18N4)

/* An emacs position is an index into the buffer...  In I18N, foreign
   characters take up the same amount of space as ASCII characters.  Emacs
   is using wide characters.  The first position is 1.

   An energize position is a straight byte offset into the file when it's
   saved onto disk.  Foreign characters take up more than one byte.  The
   first position is 0. */

#define WCMAP_SETSIZE(wcmap,n) {					      \
  (wcmap).mapsize = (n);						      \
  (wcmap).map = (WCharMapRec *) xrealloc((wcmap).map,			      \
					 (n) * sizeof(WCharMapRec));	      \
}
#define WCMAP_ENLARGE(wcmap) WCMAP_SETSIZE(wcmap, 2*(wcmap.mapsize))

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 10		/* arbitrarily large enough */
#endif 

static char wcsize_buf[MB_LEN_MAX];
#define WCSIZE(wc)	     (isascii(wc) ? 1 : wctomb(wcsize_buf,wc))

#define SANITY_CHECK_NOT
#ifdef SANITY_CHECK
static int sanity=0;
#endif 

static void
sync_buffer_widechar_map (BufferInfo *binfo)
{
  /* #### - check this: */
  assert (XBUFFER (binfo->emacs_buffer) == current_buffer);

  if (!binfo->wcmap.valid)
    {
      binfo->wcmap.valid= 1;
      binfo->wcmap.modiff_stamp= -1;
      binfo->wcmap.map= NULL;
      WCMAP_SETSIZE (binfo->wcmap, 1);
    }

  if (binfo->wcmap.modiff_stamp == BUF_MODIFF (current_buffer))
    {
      return;
    }
  else
    {
      int nbytes, maxpos,
      pos = 0,			/* start at zero instead of 1 */
      adjustment = 0,
      mapindex= 0;
      wchar_t *buf, t;

#ifdef SANITY_CHECK
      stderr_out ("rebuilding widechar map for %s\n", XSTRING_DATA (current_buffer->name));
#endif
      
      /* #### this is not gonna compile.  move_gap() is now a private function
	 inside of insdel.c and it should stay that way. */
      if (BUF_BEGV (current_buffer) < GPT && BUF_ZV (current_buffer) > GPT)
	move_gap (current_buffer, BUF_BEGV (current_buffer));
      binfo->wcmap.modiff_stamp = BUF_MODIFF (current_buffer);
      
      buf = BUF_BEG_ADDR (current_buffer);
      maxpos= (BUF_Z (current_buffer) - 1);
      wctomb (NULL, 0);		/* reset shift state of wctomb() */
      binfo->wcmap.map[mapindex].pos= pos;
      binfo->wcmap.map[mapindex].eucsize=
	((pos<maxpos) ? (nbytes= WCSIZE(buf[pos])) : 1);
      do {
	while ((pos < maxpos) && (nbytes == WCSIZE(t = buf[pos])))
	  ++pos;
	binfo->wcmap.map[mapindex++].endpos= pos;
	if (mapindex == binfo->wcmap.mapsize)
	  WCMAP_ENLARGE(binfo->wcmap);
	if (pos < maxpos)
	  {
	    binfo->wcmap.map[mapindex].pos= pos;
	    binfo->wcmap.map[mapindex].eucsize= nbytes= WCSIZE(t);
	  }
      } while (pos < maxpos);
      WCMAP_SETSIZE(binfo->wcmap, mapindex);
    }
}

static EnergizePos
EnergizePosForBufpos (Bufpos char_pos, BufferInfo *binfo)
{
  int mapindex;
  WCharMapRec map;
  EnergizePos byte_pos;

  sync_buffer_widechar_map (binfo);

  --char_pos;			/* preadjust emacs position */

  mapindex=0, byte_pos=0;
  while ((mapindex < binfo->wcmap.mapsize) &&
	 (char_pos > (map= binfo->wcmap.map[mapindex++]).pos)) {
    if (char_pos > map.endpos) {
      byte_pos += ((map.endpos - map.pos) * map.eucsize);
    } else {
      byte_pos += ((char_pos - map.pos) * map.eucsize);
    }
  }
  /* there should be a sanity check here */
#ifdef CHECK_SANITY
  if (!sanity) {
    Bufpos check_pos;
    sanity=1;
    check_pos= BufposForEnergizePos(byte_pos, binfo);
    sanity=0;

    if (check_pos != char_pos) {
      stderr_out ("ezpos(%d) = %d, Bufpos(%d) = %d\n",
	      char_pos, byte_pos, byte_pos, check_pos);
    }
  }
#endif 
  return byte_pos;
}

static Bufpos
BufposForEnergizePos (EnergizePos ez_pos, BufferInfo *binfo)
{
  int mapindex, x;
  WCharMapRec map;
  Bufpos char_pos;
  EnergizePos byte_pos;

  sync_buffer_widechar_map(binfo);

  mapindex=0, byte_pos=0;
  while ((mapindex < binfo->wcmap.mapsize) &&
	 (byte_pos <= ez_pos)) {
    map= binfo->wcmap.map[mapindex++];
    x= (map.eucsize*(map.endpos-map.pos));
    if (ez_pos > (byte_pos + x)) {
      byte_pos += x;
      char_pos = map.endpos;
    } else {
      char_pos = (map.pos + ((ez_pos - byte_pos)/map.eucsize));
      break;
    }
  }
  char_pos= (char_pos >= (1 << VALBITS)) ? BUF_Z (current_buffer) :
    (char_pos + 1);
#ifdef CHECK_SANITY
  if (!sanity) {
    EnergizePos check_pos;
    sanity=1;
    check_pos= EnergizePosForBufpos(char_pos);
    sanity=0;
    
    if (check_pos != ez_pos) {
      stderr_out ("Bufpos(%d) = %d, EnergizePosForBufpos(%d) = %d\n",
	      ez_pos, char_pos, char_pos, check_pos);
    }
  }
#endif 
  return (char_pos);
}

#else /* !I18N4 */

static Bufpos
BufposForEnergizePos (EnergizePos energizePos, BufferInfo *binfo)
{
  return ((energizePos >= (1 << VALBITS)) ? BUF_Z (current_buffer) : 
	  (energizePos + 1));
}

static EnergizePos
EnergizePosForBufpos (Bufpos emacs_pos, BufferInfo *binfo)
{
  return (emacs_pos - 1);
}

#endif /* !I18N4 */


DEFUN ("energize-update-menubar", Fenergize_update_menubar, 0, 1, 0, /*
obsolete
*/
       (frame))
{
  return Qnil;
}


DEFUN ("energize-extent-menu-p", Fenergize_extent_menu_p, 1, 1, 0, /*
Whether the extent has a set of commands defined by Energize.
*/
       (extent_obj))
{
  CHECK_EXTENT (extent_obj);

  if (NILP (Fconnected_to_energize_p ()))
    return Qnil;
  else
    {
      Energize_Extent_Data *ext = extent_to_data (extent_obj);
      return (ext && ext->extentType == CEGeneric) ? Qt : Qnil;
    }
}


/* Do what is needed so that the delayed requests will be notified by
** the event loop */

extern void mark_process_as_being_ready (struct Lisp_Process* process);

static void
notify_delayed_requests (void)
{
  if (energize_connection
      && !NILP (energize_connection->proc)
      && energize_connection->conn
      && CRequestDelayedP (energize_connection->conn))
    this function no longer exists. 
    (Replaced by mark_what_as_being_ready, with different arguments.)
    Rewrite this.
    mark_process_as_being_ready (XPROCESS (energize_connection->proc));
}


/******************* IMAGE storage maintenance *******************/

extern GLYPH image_instance_to_glyph (Lisp_Object);

static c_hashtable image_cache;

extern char *strdup ();
extern Lisp_Object Fbuffer_file_name (Lisp_Object);


extern Lisp_Object Fmake_face (Lisp_Object name);
extern Lisp_Object Fface_foreground (Lisp_Object face, Lisp_Object frame);
extern Lisp_Object Fface_background (Lisp_Object face, Lisp_Object frame);

/* Don't let any of these get GCed, since we hold their GLYPH ids in
   a non-lisp hash table (image_cache) . */
static Lisp_Object Vall_energize_pixmaps;

/* Parses an image from the image language */
static GLYPH
read_energize_image_data (Connection *conn, BufferInfo *binfo)
{
  ReqLen l;
  char *s = CGetVstring (conn, &l);
  char pix_name [255];
  char buf [255];
  int attr_number, pix_len;
  char *file;
  GLYPH result = 0;
  /* It is bad news to pass the address of a short to gethash. */
  int hashed = 0;

  if (s[0] != 'f')
    return 0;

  if (gethash ((void *) s, image_cache, (void *) &hashed))
    /* If we have already parsed this image spec (string=) then just return
       the old glyph, instead of calling the lisp code, x_get_pixmap, and
       XtGetSubResources again.	 The result may be 0 if there is no pixmap
       file name in the resource database.
     */
    return (GLYPH) hashed;

  if (3 != sscanf (s, "f %d p %d %s", &attr_number, &pix_len, pix_name))
    {
      sprintf (buf, "unparsable image: \"%s\"", s);
      error (buf);
    }

  assert (pix_len == strlen (pix_name));

  /* Read the pixmap file name for this image from the resource db */
  {
    XtResource resource [1];
    resource[0].resource_name = pix_name;
    resource[0].resource_class = XtCBitmap;
    resource[0].resource_type = XtRString;
    resource[0].resource_size = sizeof (char *);
    resource[0].resource_offset = 0;
    resource[0].default_type = XtRImmediate;
    resource[0].default_addr = 0;
    file = 0;
    XtGetSubresources (FRAME_X_SHELL_WIDGET (XFRAME (Fselected_frame (Qnil))),
		       (XtPointer) &file, "image", "Image", resource, 1, NULL,
		       0);
  }

  if (! file)
    result = 0;
  else
    {
      Lisp_Object lfile = Qnil;
      Lisp_Object p = Qnil;
      struct gcpro gcpro1, gcpro2;
      sprintf (buf, "attribute%d", attr_number);
      GCPRO2 (lfile, p);
      lfile = build_string (file);
      p = Fmake_image_instance (lfile, Qnil); /* may gc */
      result = image_instance_to_glyph (p);
      Vall_energize_pixmaps = Fcons (Fcons (make_int (result), p),
				     Vall_energize_pixmaps);
      if (!EQ (p, glyph_to_image_instance (result)))
	abort ();
      UNGCPRO;

      if (XIMAGE_INSTANCE (p)->depth == 0)
	/* if depth is >0 then this is an XPM, and its colors are not
	   controlled by the fg/bg of a face.  So don't bother making a
	   face for it. */
	{
	  Lisp_Object face, fg, bg;
	  struct face *c_face;
	  /* #### review this */
	  face = Fmake_face (intern (buf));
	  fg = FACE_FOREGROUND (face, Qnil);
	  bg = FACE_BACKGROUND (face, Qnil);
	  Fcolorize_image_instance (p, fg, bg);
	}
    }

  /* CGetVstring returns a pointer into the connection buffer; we need to
     copy it to use it as a hash key. */
  s = strdup (s);

  hashed = result;
  puthash ((void *) s, (void *) hashed, image_cache);
  return result;
}


/* Parses Classes from the connection buffer.  Defines them for
 * the buffer given as argument */
static void
read_energize_class_data (Connection *conn, unsigned int number,
			  BufferInfo *binfo, unsigned int modify_ok)
{
  CClass *ptr;			/* pointer to class data in buffer */
  GDataClass *cl;		/* unmodified class data */
  GLYPH g;
  int i;

  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CClass);
      g = read_energize_image_data (conn, binfo);
      cl = get_class (ptr->classId, binfo);

      if (!cl)
	cl = alloc_GDataclass (ptr->classId, binfo);
      else if (!modify_ok)
	message("Attempt to create class with existing Id %8x", ptr->classId);

      if (ignore_kernel) continue;

      /* if it did exist, we just clobber it */
      if (cl->flags != ptr->flags)
	{
	  cl->flags = ptr->flags;
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
	}
      if (cl->glyph != g)
	{
	  cl->glyph = g;
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
	}
    }
}

/* Parse GenericData form the connection buffer.  Defines them for the buffer
 * given as argument */
static void
read_energize_generic_data (Connection *conn, unsigned int number,
		 BufferInfo *binfo, unsigned int modify_ok)
{
  CGeneric *ptr;
  GenericData *gen;
  GDataClass *cl;
  GLYPH g;
  int i;

  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CGeneric);
      g = read_energize_image_data (conn, binfo);
      gen = get_generic (ptr->genericId, binfo);
      cl = get_class (ptr->classId, binfo);

      if (!gen)
	{
	  /* create generic if it didn't already exist */

	  if (!cl)
	    {
	      message ("Attempt to create generic %8x with undefined class %8x",
		     ptr->genericId, ptr->classId);
	      continue;
	    }

	  gen = alloc_GenericData (ptr->genericId, cl, binfo);
	  gen->glyph = g;
	  if (ptr->flags != 0xff) gen->flags = ptr->flags;
	  gen->attribute = ptr->attribute;
	}
      else if (!modify_ok)
	message("Attempt to create generic with existing id %8x",
	      ptr->genericId);
      else{
	/* modify the generic */
	int modified = 0;
	if (cl != gen->cl)
	  {
	    modified = 1;
	    gen->cl = cl;
	  }
	if (gen->glyph != g)
	  {
	    modified = 1;
	    gen->glyph = g;
	  }
	if (ptr->flags != 0xff)
	  {
	    modified = 1;
	    gen->flags = ptr->flags;
	  }
	if (gen->attribute != ptr->attribute)
	  {
	    modified = 1;
	    gen->attribute = ptr->attribute;
	  }
	if (modified)
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
      }
    }
}


static void
insert_one_extent (CExtent* ptr, BufferInfo* binfo, int modify_ok)
{
  Energize_Extent_Data *ext;
  GenericData *gen;
  Bufpos extent_start;
  Bufpos extent_end;
  int set_endpoints_p = 1;
  int created_ext_data = 0;
  Lisp_Object buffer = binfo->emacs_buffer;

  ext = get_extent_data (ptr->extentId, binfo);

  if (!ext)
    {
      ext = (Energize_Extent_Data *) xmalloc (sizeof (Energize_Extent_Data));
      created_ext_data = 1;
      ext->seal = EXTENT_SEAL;
      ext->id = ptr->extentId;
      ext->extentType = -1;
      ext->extent = Qnil;	/* not a normal value: set before we return */
      ext->start_pixmap = 0;
      ext->end_pixmap = 0;
      ext->warn_modify = 0;
      put_extent_data (ext->id, binfo, ext);
    }
  else if (!modify_ok)
    message ("Creating extent with existing id %8x", ptr->extentId);

  ext->extentType = ptr->extentType;

  switch (ptr->extentType)
    {
    case CEAttribute:
      ext->u.attr.attrValue = ptr->data;
      break;

    case CEAbbreviation:
      ext->u.abbrev.isOpened = ptr->data;
      break;

    case CEWriteProtect:
      break;

    case CEGeneric:
      gen = get_generic (ptr->data, binfo);
      if (!gen)
	{
	  message ("NewExtents: Nonexistent generic data %8x", ptr->data);
	  return;
	}
      ext->u.generic.gData = gen;
      break;

    default:
      message ("Unknown extent type %d", ptr->extentType);
      break;
    }

  /* instruct redisplay to recompute the frame */
  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;

  /* ptr->startPosition == ptr->endPosition == ~0 means to not change
   * the extent endpoints */
  if (ptr->startPosition == ~0 && ptr->endPosition == ~0)
    {
      set_endpoints_p = 0;
      extent_start = ~0;
      extent_end = ~0;
    }
  else
    {
      struct buffer *b = XBUFFER (buffer);
      extent_start = BufposForEnergizePos (ptr->startPosition, binfo);
      extent_end = BufposForEnergizePos (ptr->endPosition, binfo);

      /* We have to be careful to create the extents with endpoints
	 which are in the buffer.

	 Under certain obscure conditions involving changes made outside
	 of Emacs (bug 19983), the server and the editor can have different
	 ideas about where the extents are, so these numbers can be off
	 temporarily (during the window between read_energize_extent_data
	 and Qenergize_auto_revert_buffer in read_energize_buffer_data
	 from ModifyBufferRType).
       */

      /* Don't allow 0-length extents, as they tend to disappear. */
      if (extent_start >= extent_end)
	extent_end = extent_start + 1;

      /* Don't let them outside the buffer (even if we grew them). */
      if (extent_start >= BUF_Z (b))  extent_start = BUF_Z (b) - 1;
      if (extent_end   >= BUF_Z (b))  extent_end   = BUF_Z (b) - 1;
      if (extent_start < BUF_BEG (b)) extent_start = BUF_BEG (b);
      if (extent_end   < BUF_BEG (b)) extent_end   = BUF_BEG (b);

      /* If they're 0-length again, then the extent must be at point-max.
	 In that case, extent it backward (if possible) instead of forward.
       */
      if (extent_start >= extent_end
	  && BUF_BEG (b) != BUF_Z (b))
	extent_start = extent_end - 1;
    }

  /* no zero width extent */
  if (set_endpoints_p && extent_start == extent_end)
    extent_end += 1;

  /* Now create the extent for the extent-data.  There is a 1:1 mapping between
     these, and an extent-data points at an extent (and that extent points
     back) until energize tells us to delete the extent.  This is the only
     function in which ext->extent is ever not an extent. */
  if (created_ext_data)
    {
      ext->extent = Fmake_extent (make_int (extent_start),
				  make_int (extent_end), buffer);
      set_energize_extent_data (XEXTENT (ext->extent), ext);
      restore_energize_extent_state (XEXTENT (ext->extent));
    }
  else
    {
      if (!EQ (buffer, extent_buffer (XEXTENT (ext->extent))))
	signal_simple_error_2 ("extent not part of buffer", ext->extent,
			       buffer);

      if (set_endpoints_p)
	Fset_extent_endpoints (ext->extent, make_int (extent_start),
			       make_int (extent_end), Qnil);
      restore_energize_extent_state (XEXTENT (ext->extent));
    }

  if (energize_extent_data (XEXTENT (ext->extent)) != ext)
    abort ();

  extent_duplicable_p (XEXTENT (ext->extent)) = 1;
  extent_unique_p (XEXTENT (ext->extent)) = 1;
}


/* Parse GenericData from the connection buffer.  Defines them for the buffer
 * given as argument.  Creates the Emacs extents while parsing.
 * Energize sends the extents ordered by increasing starting position.
 I don't think the following is true any more:
 * Emacs is __much__ faster at inserting them in decreasing starting position
 * also for overlaps to work correctly the outmost extents have to be
 * inserted first.  This is what the recursive function is trying to do.
 */
static void
read_energize_extent_data (Connection *conn, unsigned int number,
			   BufferInfo *binfo, unsigned int modify_ok,
			   int extent_offset)
{
  CExtent* all_extents;
  int i;

  /* Gets the extents from the connection */
  all_extents = CGetN (conn, CExtent, number);

  /* adjusts the endpoints with the offset */
  for (i = 0; i < number; i++)
    {
      if (all_extents [i].startPosition != ~0)
	all_extents [i].startPosition += extent_offset;
      if (all_extents [i].endPosition != ~0)
	all_extents [i].endPosition += extent_offset;
    }

  /* inserts them all */
  for (i = number - 1; i >= 0; i--)
    {
      insert_one_extent (all_extents + i, binfo, modify_ok);
    }
}

/* Parses a CBuffer in the connection stream. If (delete_from != delete_to)
   all characters in this range must be deleted.
   */

static int
string_buffer_compare (char *string, int string_len,
		       struct buffer *buf, Bufpos bufpos)
{
  /* !!#### needs to be rewritten for Mule */
  Bufpos first_section_end = BUF_CEILING_OF (buf, bufpos);

  /* degenerate case, which we consider to be "true" */
  if (string_len == 0) return 0;

  /* string won't fit in the buffer, so comparison fails */
  if (BUF_Z (buf) < (bufpos + string_len)) return -1;

  /* bad starting position, so comparison fails */
  if (bufpos < BUF_BEG (buf)) return -1;

  {
    char *first_section_chars = (char *) BUF_BYTE_ADDRESS (buf, bufpos);
    int comp = strncmp (string, first_section_chars,
			first_section_end - bufpos);
    
    if (comp) return comp;
  }

  if (first_section_end < BUF_ZV (buf))
    /* there is a second section */
    {
      char *second_section_chars =
	(char *) BUF_BYTE_ADDRESS (buf, first_section_end);
      int comp = strncmp (string + (first_section_end - bufpos),
			  second_section_chars,
			  BUF_ZV (buf) - first_section_end);

      if (comp) return comp;
    }

  return 0;
}

/* called by unwind protect, from within ParseBuffer and HandleRemoveExtents */
static Lisp_Object
restore_buffer_state (Lisp_Object state_cons)
{
  BufferInfo *binfo;
  Lisp_Object bufferId_obj = Fcar (state_cons);
  unsigned int bufferId = (unsigned int) get_opaque_ptr (bufferId_obj);
  Lisp_Object buffer_modified_state = Fcar (Fcdr (state_cons));
  Lisp_Object clear_undo_list = Fcdr (Fcdr (state_cons));

  if (bufferId != 0)
    {
      if (energize_connection
	  && (binfo = get_buffer_info_for_id (bufferId, energize_connection))
	  && !NILP (binfo->emacs_buffer))
	{
	  /* Always ignore what Energize tells us about the buffer read-only
	     state.  For files Emacs knows better and for non-file buffers
	     Emacs is hacking the read-only state anyway so let it be. */
	  XBUFFER (binfo->emacs_buffer)->read_only = buffer_modified_state;
	  if (!NILP (clear_undo_list))
	    XBUFFER (binfo->emacs_buffer)->undo_list = Qnil;
	}
    }
  else
    /* this is just temporary */
    message ("Bad bufferId cons cell!");
  return Qnil;
}

/* #### this shit should be using generate-new-buffer */
static void
rename_the_buffer (Lisp_Object new_name)
{
  int count = 0;
  char number [8];
  struct gcpro gcpro1;

  Lisp_Object name = new_name;
  GCPRO1 (name);
  while (!NILP (Fget_buffer (name)))
    {
      sprintf (number, "<%d>", ++count);
      name = concat2 (new_name, build_string (number));
    }
  Frename_buffer (name, Qnil);
  UNGCPRO;
}

static int
destroy_if_energize_extent (EXTENT e, void* arg)
{
  struct Energize_Extent_Data *ext = energize_extent_data (e);
  if (ext)
    {
      Lisp_Object extent;
      Lisp_Object buffer;
      BufferInfo *binfo = 0;
      XSETEXTENT (extent, e);
      buffer = extent_buffer (XEXTENT (extent));
      Fdelete_extent (extent);
      if (BUFFERP (buffer))
	binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
      if (binfo)
	free_Energize_Extent_Data (ext, binfo, OFT_GC);
      else
	{
	  /* #### partly duplicated in free_Energize_Extent_Data() */
	  set_energize_extent_data (e, 0);
	  ext->extent = Qnil;
	}
    }
  return 0;
}

static void
destroy_all_energize_extents (struct buffer *buf)
{
  map_extents (BUF_BEG (buf), BUF_Z (buf), destroy_if_energize_extent,
	       NULL, make_buffer (buf), 0,
	       ME_END_CLOSED | ME_MIGHT_MODIFY_EXTENTS);
}

static Lisp_Object
restore_inside_parse_buffer (Lisp_Object val)
{
  inside_parse_buffer = XINT (val);
  return (val);
}

static void
hack_window_point (Lisp_Object window,
		   Lisp_Object old_point,
		   Lisp_Object old_start,
		   int keep_start_p,
		   BufferInfo *binfo)
     /* If we've reverted a buffer, sometimes we want to make sure that
	the window-point doesn't move. */
{
  if (NILP (window))
    return;

  Fset_marker (XWINDOW (window)->pointm, old_point, binfo->emacs_buffer);
  if (NILP (binfo->output_mark) && keep_start_p)
    {
      Fset_marker (XWINDOW (window)->start, old_start, binfo->emacs_buffer);
      XWINDOW (window)->force_start = 1;
    }
}

static void
read_energize_buffer_data (Connection *conn, CBuffer *cbu, Editor *editor,
			   EnergizePos delete_from, EnergizePos delete_to,
			   Window win, int relative_p)
{
  char *name;
  ReqLen name_len;
  char *pathname_str;
  ReqLen pathname_len;
  char *buffer_class_str;
  ReqLen buffer_class_len;
  Lisp_Object pathname = Qnil;
  Lisp_Object pathname_directory = Qnil;
  Lisp_Object buffer_name = Qnil;
  Lisp_Object filename = Qnil;
#if 1
  Lisp_Object display_window = Qnil;
#endif
  BufferInfo *binfo;
  int modifying_p = 0;
  Bufpos previous_point;
  Bufpos from;
  Bufpos to;
#if 1
  Bufpos display_start = 1;
#endif
  char *text;
  ReqLen text_len;
  int get_chars_from_file = 0;
  Lisp_Object modified_buffer_flag;
  int speccount = specpdl_depth ();
  int extent_offset;
  Lisp_Object restore_buffer_state_cons;
  int should_keep_window_start = 1;
  int no_text_deleted = 0;

  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  /* For some reason calling the GC before parsing the buffer data
     makes a better usage of memory and emacs leaks less when 
     creating/deleting LE browser buffers.  
     However you don't want to call GC all the tiem so we only do it if the request
     will create more than a given number of extents. */
  if (cbu->nExtent > energize_extent_gc_threshold)
    garbage_collect_1 ();

  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  Fwiden (Fcurrent_buffer ());

  GCPRO4 (buffer_name, pathname, pathname_directory, filename);

  name = CGetVstring (conn, &name_len);

  /* read the pathname and buffer-class -- Editor Protocol > 0 */
  pathname_str = CGetVstring (conn, &pathname_len);
  buffer_class_str = CGetVstring (conn, &buffer_class_len);

  if (name_len)
    buffer_name = build_string (name);
  if (pathname_len)
    pathname = build_string (pathname_str);

  /* set up pathname_directory */
  if (!NILP (pathname))
    {
      if (NILP (Ffile_directory_p (pathname)))
	pathname_directory = Ffile_name_directory (pathname);
      else
	pathname_directory = pathname;
    }

  /* make sure that pathname_directory ends with a '/', if it exists */
  if (!NILP (pathname_directory))
    {
      Bufbyte *str = XSTRING_DATA (pathname_directory);
      Bytecount size = XSTRING_LENGTH (pathname_directory);
      if (str[size - 1] != '/')
	{
	  Lisp_Object tmp = make_string (str, size + 1);
	  set_string_byte (XSTRING (tmp), size, '/');
	  pathname_directory = tmp;
	}
    }


  /* get or create the BufferInfo */
  if (binfo = get_buffer_info_for_id (cbu->bufferId, editor))
    modifying_p = 1;
  else
    {
      if (NILP (buffer_name))
	{
	  char *dummy = "*Unnamed " IDENTITY_CRISIS " Buffer*";
	  buffer_name = build_string (dummy);
	}
      /* create new buffer */
      binfo = alloc_BufferInfo (cbu->bufferId, buffer_name, pathname,
				buffer_class_str, editor, win,
				cbu->nExtent + cbu->nClass + cbu->nGeneric);
      XBUFFER (binfo->emacs_buffer)->read_only =
	cbu->flags == CBReadOnly ? Qt : Qnil;
    }

  /* remember where we were in which buffer before we change things */
  if (current_buffer != XBUFFER (binfo->emacs_buffer))
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (binfo->emacs_buffer);
    }

  /* set default-directory */
  if (!NILP (pathname_directory))
    {
      if (!NILP (Ffile_directory_p (pathname_directory))
	  && !NILP (Ffile_executable_p (pathname_directory)))
	Fset (Qdefault_directory, pathname_directory);
      /* Never set this to nil, that loses badly. */
/*      else
	Fset (Qdefault_directory, Qnil); */
    }

  /* set file name unless it's a directory */
  if (!NILP (pathname) && NILP (Ffile_directory_p (pathname)))
    {
      filename = Fexpand_file_name (pathname, Qnil);
      Fset (Qbuffer_file_name, filename);
    }

  /* set buffer name */
  if (!NILP (buffer_name))
    {
      if (modifying_p
	  && strcmp ((char*)XSTRING_DATA (buffer_name),
		     (char*)
		     string_data (XSTRING (XBUFFER (binfo->emacs_buffer)->name))))
	rename_the_buffer (buffer_name);
    }

  if (modifying_p)
    {
      run_hook (Venergize_kernel_modification_hook);
      /* Make sure buffer is current after the hook */
      Fset_buffer (binfo->emacs_buffer);
    }

  modified_buffer_flag = Fbuffer_modified_p (binfo->emacs_buffer);

  /* enables buffer edits */
  restore_buffer_state_cons =
    Fcons (make_opaque_ptr ((void *) cbu->bufferId),
	   Fcons (XBUFFER (binfo->emacs_buffer)->read_only, Qt));
  record_unwind_protect (restore_buffer_state, restore_buffer_state_cons);
  XBUFFER (binfo->emacs_buffer)->read_only = Qnil;

  /* any changes here should take place "underneath" these hooks, I think */
  specbind (Qenergize_buffer_modified_hook, Qnil);
  specbind (Qfirst_change_hook, Qnil);
  specbind (Qbefore_change_functions, Qnil);
  specbind (Qbefore_change_function, Qnil); /* #### */
  /* As energize does not use the after-change-function it's not useful to
     bind it to NIL */
  /* specbind (Qafter_change_functions, Qnil); */
  /* specbind (Qafter_change_function, Qnil); #### */
  specbind (Qinhibit_read_only, Qt);
  record_unwind_protect (restore_inside_parse_buffer,
			 make_int (inside_parse_buffer));
  inside_parse_buffer = 1;
  specbind (Qbuffer_undo_list, Qt);

  XBUFFER (binfo->emacs_buffer)->undo_list = Qt;

  /* BufposForEnergizePos uses the current-buffer */
  from = BufposForEnergizePos (delete_from, binfo);
  to = BufposForEnergizePos (delete_to, binfo);

  /* See if we should get the characters from the file directly.
     Only protocol 0.10+ will do this.
   */
#ifdef ENERGIZE_V2_HEADERS
  get_chars_from_file = 0;
#else
  if (cbu->flags != 0xff)
    get_chars_from_file = cbu->flags &  CBFileYourself;
  else
    get_chars_from_file = binfo->flags &  CBFileYourself;
#endif
  
  /* Even when we get the chars from a file there is an empty text string */
  if (get_chars_from_file)
    {
      text = CGetVstring (conn, &text_len);
      text = NULL;
      text_len = 0;
    }
  else
    {
      text = CGetVstring (conn, &text_len);
    }
  
  /* updates the visited file modtime */
  if (modifying_p && (from != to || text_len)
      /* but only when we do not read the file ourselves */
      && !get_chars_from_file)
    Fset_buffer_modtime (binfo->emacs_buffer, Qnil);

  if (!modifying_p)
    {
      /* clears the buffer in case we re-use a non-energize buffer */
      previous_point = 1;
      Fset_buffer (binfo->emacs_buffer);
      buffer_delete_range (current_buffer, BUF_BEG (current_buffer),
			   BUF_Z (current_buffer), 0);
    }
  else
    {
#if 1
      display_window = Fget_buffer_window (binfo->emacs_buffer, Qnil, Qnil);
#endif
      previous_point = BUF_PT (current_buffer);

#if 1
      if (!NILP (display_window))
	display_start =
	  XINT (Fmarker_position (XWINDOW (display_window)->start));
#endif

      if (from != to)
	{
	  struct buffer *buf = XBUFFER (binfo->emacs_buffer);

	  Fset_buffer (binfo->emacs_buffer);
	  Fwiden (Fcurrent_buffer ());
	  if (!NILP (binfo->output_mark)
	      && marker_position (binfo->output_mark) >= from)
	    Fset_marker (binfo->output_mark, make_int (from),
			 binfo->emacs_buffer);
	  if (((to - from) == text_len) && !get_chars_from_file &&
	      !string_buffer_compare (text, text_len, buf, from))
	    /* the new text is the same as the old text, don't clear
	       the undo list*/
	    {
	      Fsetcdr (Fcdr (restore_buffer_state_cons), Qnil);
	      no_text_deleted = 1;
	      destroy_all_energize_extents (buf);
	    }
	  else
	    {
	      /* Do not keep window start if we actually delete text */
	      should_keep_window_start = 0;
	      Fset_buffer (binfo->emacs_buffer);
	      destroy_all_energize_extents (buf);
	      if (!get_chars_from_file)
		buffer_delete_range (current_buffer, from, to, 0);
	    }

	  /* Do not clear the undo list if getting the chars from the file */
	  if (get_chars_from_file)
	    Fsetcdr (Fcdr (restore_buffer_state_cons), Qnil);
	}
      else if (!text_len && !get_chars_from_file)
	/* if there is no text and we didn't delete anything,
	   don't clear the undo_list slot */
	Fsetcdr (Fcdr (restore_buffer_state_cons), Qnil);

    }

  /* buffer type */
  if (cbu->flags != 0xff && cbu->flags != binfo->flags)
    {
      if (!modifying_p)
	{
	  if (cbu->flags == CBUserInput)
	    {
	      Lisp_Object buffer_local_variable_name =
		Qenergize_user_input_buffer_mark;
	      binfo->output_mark = Fmake_marker ();
	      Fset_marker (binfo->output_mark, make_int (1),
			   binfo->emacs_buffer);
	      /* make sure that this guy doesn't get GC'd out from under us */
	      Fmake_local_variable (buffer_local_variable_name);
	      Fput (buffer_local_variable_name, Qpermanent_local, Qt);
	      Fset (buffer_local_variable_name, binfo->output_mark);
	      /* Make sure buffer is current after the hook */
	      Fset_buffer (binfo->emacs_buffer);
	    }
	}
      binfo->flags = cbu->flags;
    }

  if (get_chars_from_file && text_len != 0)
    /* I think this is always true, but let's make sure - jwz */
    abort ();

  if (text_len)
    {
      if (!NILP (binfo->output_mark))
	{
	  Fset_buffer (binfo->emacs_buffer);
	  if (XMARKER (binfo->output_mark)->buffer)
	    Fgoto_char (binfo->output_mark, Fcurrent_buffer ());
	  else
	    /* This should not happen */
	    Fgoto_char (make_int (BUF_ZV (XBUFFER (binfo->emacs_buffer))),
			Fcurrent_buffer ());

	  if (BUF_PT (current_buffer) <= previous_point)
	    {
#if 1
	      display_start += text_len;
#endif
	      previous_point += text_len;
	    }
	  buffer_insert_raw_string (current_buffer, text, text_len);
	  Fset_marker (binfo->output_mark, make_int (BUF_PT (current_buffer)),
		       binfo->emacs_buffer);
	}
      else if (modifying_p)
	{
	  Fgoto_char (make_int (from), Fcurrent_buffer ());
	  if (!no_text_deleted)
	    buffer_insert_raw_string (current_buffer, text, text_len);
	}
      else
	buffer_insert_raw_string (current_buffer, text, text_len);

      previous_point = XINT (Fgoto_char (make_int (previous_point)),
			     Fcurrent_buffer ());
    }
  else if (get_chars_from_file && !modifying_p)
    {
      /* !modifying_p means the buffer is being created - read the text
	 from the file. */
      Finsert_file_contents_internal (Fbuffer_file_name (binfo->emacs_buffer),
				      /* #### coding system not correct */
				      Qt, Qnil, Qnil, Qnil, Qnil, Qnil));
    }

  if (!relative_p)
    extent_offset = 0;
  else if (!NILP (binfo->output_mark))
    extent_offset = EnergizePosForBufpos (XINT (Fmarker_position
						     (binfo->output_mark)),
					   binfo);
  else
    extent_offset = EnergizePosForBufpos (BUF_Z(XBUFFER(binfo->emacs_buffer)),
					   binfo);
  
#if 1
  if (text_len || !text)
    hack_window_point (display_window,
		       make_int (previous_point),
		       make_int (display_start),
		       should_keep_window_start,
		       binfo);
#endif


  /* Classes, generics and extents */
  /* make sure that we have enough room in the hash table */
  expand_hashtable (binfo->id_to_object,
		    cbu->nClass + cbu->nGeneric + cbu->nExtent);
  read_energize_class_data (conn, cbu->nClass, binfo, modifying_p);
  read_energize_generic_data (conn, cbu->nGeneric, binfo, modifying_p);
  read_energize_extent_data (conn, cbu->nExtent, binfo, modifying_p, extent_offset);

  /* Restore the modified bit */
  Fset_buffer_modified_p (modified_buffer_flag, binfo->emacs_buffer);

  if (get_chars_from_file && modifying_p)
    {
      /* modifying_p means the buffer already exists and the extents are
	 being re-written.  It may be that the file has changed on disk,
	 and the extents no longer correspond to the text in the buffer,
	 which would be bad.  So, check the file on disk, and if it has
	 changed, offer to revert.

	 As this runs lisp code which may prompt the user, and consequently
	 may accept process output, be careful to do this after we have
	 finished reading the current request from the Energize connection.
       */
     if (NILP (Fverify_visited_file_modtime (binfo->emacs_buffer)))
       {
	 call1 (Qenergize_auto_revert_buffer, binfo->emacs_buffer);
	 hack_window_point (display_window,
			    make_int (previous_point),
			    make_int (display_start),
			    1,
			    binfo);
       }
   }


  /* restore modified hooks and globals, and return the previous buffer */
  UNGCPRO;
  unbind_to (speccount, Qnil);
}


static void
cleanly_destroy_all_widgets (int count, LWLIB_ID *ids)
{
  /* This just calls lw_destroy_all_widgets, but is careful to make sure that
     this doesn't cause the frames to shrink.  If one deletes psheets
     (children of the "control" area of the MainWindow) without first
     unmanaging the MainWindow, the frame resizes.  So first unmanage all
     the MainWindows of all applicable frames, then remanage them.  This is
     nasty, but...
   */
  Lisp_Object frmcons, devcons, concons;
  int i, j;

  if (count == 0)
    return;

  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
    {
      Lisp_Object frame = XCAR (frmcons);
      struct frame *f = XFRAME (frame);

      if (!FRAME_X_P (f))
	continue;
      /* Optimization: only unmanage the MainWindow if this frame is
	 displaying one of the psheets in question.  (Special casing
	 the debugger panel as usual...)
       */
      for (i = 0; i < count; i++)
	if (ids [i] == debuggerpanel_sheet)
	  {
	    XtUnmanageChild (FRAME_X_CONTAINER_WIDGET (f));
	    goto next_frame;
	  }
	else
	  for (j = 0; j < FRAME_X_CURRENT_PSHEET_COUNT (f); j++)
	    if (ids [i] == FRAME_X_CURRENT_PSHEETS (f) [j])
	      {
		XtUnmanageChild (FRAME_X_CONTAINER_WIDGET (f));
		goto next_frame;
	      }
    next_frame: ;
    }

  for (i = 0; i < count; i++)
    {
      lw_destroy_all_widgets (ids [i]);
      if (ids [i] == debuggerpanel_sheet)
	{
	  debuggerpanel_sheet = 0;
	  desired_debuggerpanel_exposed_p = 0;
	}
    }

  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
    {
      Lisp_Object frame = XCAR (frmcons);
      struct frame *f = XFRAME (frame);

      if (!FRAME_X_P (f))
	continue;
      XtManageChild (FRAME_X_CONTAINER_WIDGET (f));
    }
}


/* kill an Energize buffer */
static void
forget_buffer (BufferInfo *binfo)
{
  int i;
  Lisp_Object buffer = binfo->emacs_buffer;

  remove_buffer_info (binfo->id, buffer, binfo->editor);
  Venergize_buffers_list = Fdelq (buffer, Venergize_buffers_list);

  /* if there was an associated frame */
  if (!NILP (binfo->frame))
    Fdelete_frame (binfo->frame, Qt);

  if (binfo->n_p_sheets > 0)
    {
      /* Also delete the dialog boxes associated with the buffer. */
      cleanly_destroy_all_widgets (binfo->n_p_sheets,
				   (LWLIB_ID *) binfo->p_sheet_ids);
    }

  free_buffer_info (binfo);

  XBUFFER (buffer)->undo_list = Qnil;
  /* flush the buffer SOE before flushing the extents */
  free_buffer_cached_stack (XBUFFER (buffer));
  XBUFFER (buffer)->extents = Qnil;
}

/********************** Request-related utilities ************************/

/* outputs a single extent in the connection buffer */
static void
write_energize_extent_data (Connection *conn, Energize_Extent_Data *ext,
			    unsigned int start, unsigned int end)
{
  switch (ext->extentType)
    {
    case CEAttribute:
      CWriteExtent (conn, CEAttribute, ext->id, start, end,
		    (EId)ext->u.attr.attrValue);
      break;

    case CEAbbreviation:
      CWriteExtent (conn, CEAbbreviation, ext->id, start, end,
		    (EId)ext->u.abbrev.isOpened);
      break;

    case CEGeneric:
      CWriteExtent (conn, CEGeneric, ext->id, start, end, 0);
      break;

    case CEWriteProtect:
      CWriteExtent (conn, CEWriteProtect, ext->id, start, end, 0);
      break;
    }
}

/* Function called by map_extents in SaveBufferToEnergize. Outputs the
   extents for the extents corresponding to Energize objects, and
   increments the n_extents count. */

static int
write_energize_extent_data_mapper (EXTENT extent, void *arg)
{
  binfo_and_n_extents *bane = (binfo_and_n_extents*)arg;
  Lisp_Object extent_obj;
  Energize_Extent_Data *ext;

  XSETEXTENT (extent_obj, extent);
  ext = extent_to_data (extent_obj);
  if (ext)
    {
      Bufpos first = XINT (Fextent_start_position (extent_obj));
      Bufpos last = XINT (Fextent_end_position (extent_obj));
      write_energize_extent_data (bane->binfo->editor->conn, ext,
				  EnergizePosForBufpos (first, bane->binfo),
				  EnergizePosForBufpos (last, bane->binfo));
      bane->n_extents += 1;
    }
  return 0;
}

/* Sends a BufferSaved request to energize for binfo */
static void
write_energize_buffer_data (BufferInfo *binfo)
{
  Connection *conn = binfo->editor->conn;
  EId bufferId = binfo->id;
  CBuffer *cbu;
  CEditorRequest *req;
  struct buffer *cur_buff = current_buffer;
  int speccount = specpdl_depth ();
  Lisp_Object file_name;

  binfo_and_n_extents bane;

  /* selects the buffer as current */
  Fset_buffer (binfo->emacs_buffer);

  /* write header */
  cbu = CWriteBufferSavedHeader (conn);
  cbu->bufferId = bufferId;
  cbu->flags = 0;
  cbu->nClass = 0;
  cbu->nGeneric = 0;

  /* file name */
  file_name = current_buffer->filename;
  if (STRINGP (file_name))
    CWriteVstring0 (conn, XSTRING_DATA (file_name));
  else
    CWriteVstring0 (conn, "");
  CWriteVstring0 (conn, "");	/* directory name */
  CWriteVstring0 (conn, "");	/* buffer name */

  /* write the text */
#ifndef ENERGIZE_V2_HEADERS
  if (binfo->flags & CBFileYourself)
    {
      /* Only the 0.10+ protocol will ask us to write the file directly. */
      Lisp_Object start;
      Lisp_Object end;
      XSETINT (start, BUF_BEG (current_buffer));
      XSETINT (end, BUF_Z (current_buffer));
      Fwrite_region_internal (start, end,
			      Fbuffer_file_name (binfo->emacs_buffer),
			      /* #### coding system not correct */
			      Qnil, Qt, Qnil);
      CNeedOutputSize (conn, 9);
      CWriteVstringLen (conn, NULL, 0);
    }
  else
#endif /*  ENERGIZE_V2_HEADERS */
    {
      Lisp_Object string = make_string_from_buffer (current_buffer,
						    BUF_BEG (current_buffer),
						    BUF_Z (current_buffer));
      CNeedOutputSize (conn, XSTRING_LENGTH (string) + 9);
      CWriteVstringLen (conn, XSTRING_DATA (string),
			XSTRING_LENGTH (string));
    }

  /* write the extents */
  bane.binfo = binfo;
  bane.n_extents = 0;

  /* Only write the extents when not filing ourselves */
#ifndef ENERGIZE_V2_HEADERS
  if (!(binfo->flags & CBFileYourself))
#endif
    {
      map_extents (BUF_BEG (current_buffer), BUF_Z (current_buffer),
		   write_energize_extent_data_mapper, &bane,
		   binfo->emacs_buffer, 0, ME_END_CLOSED);
      
    }

  /* update nextent in request's header */
  req = (CEditorRequest *)conn->header;
  req->buffersaved.buffer.nExtent = bane.n_extents;
  CWriteLength (conn);
  CWriteRequestBuffer (conn);

  /* sets the flags so that we will warn Energize about more modifications */
  binfo->modified_state = 0;

  /* Mark the buffer as non editable so that we will ask Energize about it
     before modifying it again */
  binfo->editable = 0;

  /* restores the buffer as current */
  set_buffer_internal (cur_buff);
  unbind_to (speccount, Qnil);
}

static unsigned long
energize_extent_data_id (Energize_Extent_Data *ext)
{
  return ext ? ext->id : 0;
}


/********************** Menu ("keywords") operations **********************/

static int
something_answered_p (void* arg)
{
  struct reply_wait* rw = (struct reply_wait*)arg;
  return rw->answered_p || !energize_connection || !energize_connection->conn;
}


static void
push_wait (struct reply_wait* rw)
{
  rw->next = global_reply_wait;
  global_reply_wait = rw;
}

static Lisp_Object
remove_wait (Lisp_Object obj)
{
  struct reply_wait* gw;
  struct reply_wait* previous;
  struct reply_wait* rw = (struct reply_wait *) get_opaque_ptr (obj);

  for (previous = 0, gw = global_reply_wait;
       gw != rw;
       previous = gw, gw = gw->next);
  if (previous)
    previous->next = gw->next;
  else
    global_reply_wait = gw->next;
  return Qnil;
}

static struct reply_wait*
find_wait_reply (int serial)
{
  struct reply_wait* gw;
  for (gw = global_reply_wait; gw && gw->serial != serial; gw = gw->next);
  return gw;
}


static int
wait_for_reply (struct reply_wait* rw)
{
  int speccount = specpdl_depth ();
  rw->answered_p = 0;
  push_wait (rw);
  record_unwind_protect (remove_wait, make_opaque_ptr (rw));
  wait_delaying_user_input (something_answered_p, rw);
  unbind_to (speccount, Qnil);
  return rw->answered_p;
}

/* gets the menu for the buffer/extent pair at the head of the request buffer.
   returns the propose choice request if succeeds, nil otherwise (kernel
   connection closed, or not connected)
 */

static Lisp_Object
get_energize_menu (Lisp_Object buffer, Lisp_Object extent_obj, int selection_p,
		   Lisp_Object only_name)
{
  Connection*	conn;
  EId	buffer_id;
  EId	extent_id;
  Lisp_Object result;
  struct reply_wait rw;
  struct gcpro gcpro1, gcpro2;

  if (!get_energize_connection_and_buffer_id (buffer,
					      (void **) &conn,
					      (long *) &buffer_id))
    return Qnil;

  if (EXTENTP (extent_obj))
    extent_id = energize_extent_data_id (extent_to_data (extent_obj));
  else
    extent_id = 0;

  CWriteQueryChoicesRequest (conn, buffer_id, extent_id);
  conn->header->data =
    selection_p ? CEChasCharSelection | CEChasObjectSelection : 0;
  conn->header->serial = ++request_serial_number;
  CWriteRequestBuffer (conn);

  /* wait for the acknowledge */
  rw.serial = request_serial_number;
  rw.objectId = buffer_id;
  rw.genericId = extent_id;
  rw.menu_result = Qnil;
  rw.only_name = only_name;

  GCPRO2 (rw.menu_result, rw.only_name);
  wait_for_reply (&rw);
  result = rw.menu_result;
  UNGCPRO;
  return result;
}


static void
execute_energize_menu (Lisp_Object buffer, Energize_Extent_Data* ext,
		       char* name, EId item_id, EId flags,
		       Lisp_Object selection, Lisp_Object no_confirm)
{
  Connection*	conn;
  EId	buffer_id;
  EId	extent_id;
  BufferInfo*	binfo;
  struct reply_wait rw;

  if (!get_energize_connection_and_buffer_id (buffer, (void**)&conn,
					      (long*)&buffer_id))
    return;

  extent_id = energize_extent_data_id (ext);

  if ((flags & CKBuffer) && !NILP (Fbuffer_modified_p (buffer)))
    {
      /* saves buffer if requested and needed */
      binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
      if (binfo)
	write_energize_buffer_data (binfo);
    }

  CWriteExecuteChoicesRequest (conn, buffer_id, extent_id, item_id, 0, 0);
  /* send the menu name */
  if (energize_connection->minor >= 7)
    CWriteVstring0 (conn, name);
  conn->header->serial = ++request_serial_number;
  conn->header->data = 0;
  if (STRINGP (selection))
    {
      conn->header->data |= CEChasCharSelection;
      CWriteVstringLen (conn, XSTRING_DATA (selection),
			XSTRING_LENGTH (selection));
    }
  else if (VECTORP (selection))
    {
      int i;
      EId data;
      conn->header->data |= CEChasObjectSelection;

      /* writes the length */
      data = vector_length (XVECTOR (selection));
      CWrite (conn, EId, &data);

      /* writes the elements */
      for (i = 0; i < vector_length (XVECTOR (selection)); i++)
	{
	  if (CONSP (vector_data (XVECTOR (selection)) [i]))
	    data = lisp_to_word (vector_data (XVECTOR (selection)) [i]);
	  else
	    data = XINT (vector_data (XVECTOR (selection)) [i]);
	  CWrite (conn, EId, &data);
	}
    }
  else if (CONSP (selection))
    {
      Lisp_Object type = Fcar (selection);
      Lisp_Object value = Fcdr (selection);
      if (EQ (type, intern ("ENERGIZE_OBJECT"))
	  && STRINGP (value))
	{
	  conn->header->data |= CEChasObjectSelection;
	  CWriteN (conn, char, XSTRING_DATA (value),
		   XSTRING_LENGTH (value));
	}
    }
  else if (!NILP (selection))
    error ("unrecognized energize selection");
  
  if (!NILP (no_confirm))
    conn->header->data |= CECnoConfirm;
  CWriteLength (conn);
  CWriteRequestBuffer (conn);

  /* wait for the acknowledge */
  rw.serial = request_serial_number;
  rw.objectId = buffer_id;
  rw.genericId = extent_id;
  rw.itemId = item_id;
  rw.message = 0;

  if (wait_for_reply (&rw) && !rw.status)
    {
      char message [128];
      if (energize_connection && energize_connection->conn)
	sprintf (message, IDENTITY_CRISIS " command failed: %.80s",
		 (rw.message ? rw.message : "(null)"));
      else
	sprintf (message, "Connection to " IDENTITY_CRISIS " was closed.");
      if (rw.message)
	xfree (rw.message);
      error (message);
    }
  else
    {
      if (rw.message)
	xfree (rw.message);
      if (!energize_connection)
	error ("Connection to " IDENTITY_CRISIS " was closed.");
    }
 }

/* Returns a list of vectors representing the menu choices.  Next request
   in connection must be a ProposeChoices.  The list is
   (buffer extent <item1> ... <itemN>).	 <itemI> is (name id1 id2 flags).
   Idi is (high .  low).  We build the list in reverse order and nreverse
   it.	If (only_name != 0), we only return the item of named only_name as
   a vector.  */

static Lisp_Object
list_choices (Lisp_Object buffer, Lisp_Object extent_obj,
	      Lisp_Object only_name, CProposeChoicesRequest* creq)
{
  Connection *conn;
  int i;
  Lisp_Object item_list;
  Lisp_Object item;
  struct Lisp_Vector *v;
  struct gcpro gcpro1, gcpro2, gcpro3;
  CChoice *choice;
  ReqLen name_length;
  char *name;
  char *arg_name;

  if (energize_connection && energize_connection->conn)
    conn = energize_connection->conn;
  else
    return Qnil;

  if (!creq || creq->head.reqType != ProposeChoicesRType)
    {
      CSkipRequest (conn);
      return Qnil;
    }

  item = Qnil;
  item_list = Qnil;

  GCPRO3 (only_name, item_list, item);

  for (i = 0; i < (int)(creq->nChoices); i++)
    {
      choice = CGet (conn, CChoice);
      name = CGetVstring (conn, &name_length);
      if (!name_length)
	continue;

      /* the argument, if passed, is another string after the NUL (!)
       * this is a quick hack to provide cheap arguments to menus entries */
      arg_name = strchr (name, 0240);
      if (arg_name)
	{
	  *arg_name= 0;
	  arg_name += 1;
	}

      if (!NILP (only_name))
	{
	  if (!strcmp ((char*) XSTRING_DATA (only_name), name))
	    {
	      if (NILP (item))
		{
		  item = make_vector (5, Qnil);
		  v = XVECTOR (item);
		  v->contents [0] = only_name;
		}
	      v->contents [1] = word_to_lisp (choice->choiceId);
	      v->contents [2] = Qnil;
	      v->contents [3] = make_int (choice->flags);
	      v->contents [4] = arg_name ? build_string (arg_name) : Qnil;
	    }
	}
      else
	{
	  item = make_vector (5, Qnil);
	  v = XVECTOR (item);
	  v->contents [0] = build_string (name);
	  v->contents [1] = word_to_lisp (choice->choiceId);
	  v->contents [2] = Qnil;
	  v->contents [3] = make_int (choice->flags);
	  v->contents [4] = arg_name ? build_string (arg_name) : Qnil;
	  item_list = Fcons (item, item_list); /* pushes in the list */
	}
    }

  if (NILP (only_name))
    item_list = Fcons (buffer, Fcons (extent_obj, Fnreverse (item_list)));
  UNGCPRO;

  return NILP (only_name) ? item_list : item;
}

DEFUN ("energize-list-menu", Fenergize_list_menu, 3, 4, 0, /*
Request the set of menu options from the Energize server that are
appropriate to the buffer and the extent.  Extent can be (), in which case
the options are requested for the whole buffer.	 Selection-p tells
if the selection is available on the dislpay emacs is using. 
Returns the options as
a list that can be passed to energize-activate-menu.  Items
in the list can also be passed to energize-execute-menu-item.
The list is (buffer extent or () <item1> ... <itemN>).
where <itemI> is (name id1 id2 flags); idI is (high . low).
If optional argument only-name is provided only the item with name only-name
is returned, or () if no such item exists.
*/
       (buffer, extent_obj, selection_p, only_name))
{
  Lisp_Object res;
  CHECK_BUFFER (buffer);

  if (!energize_connection || !energize_connection->conn) return Qnil;

  if (!NILP (only_name))
    CHECK_STRING (only_name);

  res = get_energize_menu (buffer, extent_obj, !NILP (selection_p),
			   only_name);
  notify_delayed_requests ();
  return res;
}

DEFUN ("energize-execute-menu-item", Fenergize_execute_menu_item, 3, 5, 0, /*
Item is a vector received by energize-list-menu.  Sends a request to
execute the code associated to this menu inside the Energize server.
Optional fourth argument is a string or a vector to be used as the selection
for entry disabled because they need the selection.
Optional fifth argument, if non NIL, tells Energize to not request 
confirmation before executing the command.
*/
       (buffer, extent_obj, item, selection, no_confirm))
{
  struct Lisp_Vector *v;

  if (!energize_connection || !energize_connection->conn) return Qnil;

  CHECK_BUFFER (buffer);
  CHECK_VECTOR (item);
  v = XVECTOR (item);

  if (vector_length (v) != 4)
    error ("Bad menu item to energize-execute-menu-item");

  /* ignore the flags for now */
  execute_energize_menu (buffer, extent_to_data (extent_obj),
			 (char*)XSTRING_DATA (v->contents [0]),
			 lisp_to_word (v->contents [1]),
			 XINT (v->contents [3]),
			 selection,
			 no_confirm);

  return Qt;
}

DEFUN ("energize-execute-command-internal",
       Fenergize_execute_command_internal, 3, 5, 0, /*
Command is a string naming an energize command.	 Sends a request to
execute this command inside the Energize server.
Optional fourth argument is a string or a vector to be used as the selection.
Optional fifth argument, if non NIL, tells Energize to not request 
confirmation before executing the command.

See also 'energize-list-menu'.
*/
       (buffer, extent_obj, command, selection, no_confirm))
{
  if (!energize_connection || !energize_connection->conn) return Qnil;

  CHECK_BUFFER (buffer);
  CHECK_STRING (command);

  execute_energize_menu (buffer, extent_to_data (extent_obj),
			 (char*)XSTRING_DATA (command), 0, 0, selection,
			 no_confirm);

  return Qt;
}

/********************************* kill buffer interface ****************/

DEFUN ("energize-buffer-type-internal", Fenergize_buffer_type, 1, 1, 0, /*
Return a symbol denoting the buffer type if buffer is an Energize
buffer, else it returns NIL.
*/
       (buffer))
{
  if (!energize_connection) return Qnil;

  CHECK_BUFFER (buffer);
  return get_buffer_type_for_emacs_buffer (buffer, energize_connection);
}

DEFUN ("set-energize-buffer-type-internal", Fset_energize_buffer_type_internal, 2, 2, 0, /*
Return the type symbol which is the new buffer-type, if the buffer is
an Energize buffer and the type is non-NIL symbol, else it returns NIL.
*/
       (buffer, type))
{
  BufferInfo *binfo;

  if (!energize_connection || (NILP (type))) return Qnil;

  CHECK_BUFFER (buffer);
  CHECK_SYMBOL (type);

  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;
  else
    return
      set_buffer_type_for_emacs_buffer (buffer, energize_connection, type);
}

DEFUN ("energize-buffer-p", Fenergize_buffer_p, 1, 1, 0, /*
Whether buffer is an Energize buffer.
*/
       (buffer))
{
  BufferInfo *binfo;

  if (!energize_connection) return Qnil;

  CHECK_BUFFER (buffer);
  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;
  else
    return Qt;
}

DEFUN ("energize-buffer-id", Fenergize_buffer_id, 1, 1, 0, /*
Return (high . low) if buffer is an Energize buffer, otherwise nil.
*/
       (buffer))
{
  BufferInfo *binfo;

  if (!energize_connection) return Qnil;

  CHECK_BUFFER (buffer);
  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;
  else
    return word_to_lisp (binfo->id);
}

DEFUN ("energize-request-kill-buffer", Fenergize_request_kill_buffer, 1, 1, 0, /*
Sends a request to energize for killing buffer.
*/
       (buffer))
{
  BufferInfo *binfo;

  if (!energize_connection) return Qnil;

  CHECK_BUFFER (buffer);
  if (!(binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;

  /* Tell Energize about it if connected */
  if (energize_connection->conn)
    {
      CWriteKillBufferHeader (energize_connection->conn, binfo->id);
      CWriteRequestBuffer (energize_connection->conn);
    }

  /* Clears the internal state */
  forget_buffer (binfo);

  return Qnil;
}

/******************** Handle requests from the kernel *********************/

#ifdef EMACS_BTL
#include "cadillac-btl-extern.h"
#endif

/* turn logging on or off, etc. */
static void
handle_logging_request (Editor *editor, CLoggingRequest *creq)
     /* I'm a lumberjack and I'm ok... */
{
  ReqLen name_len;
  char* data_filename = CGetVstring (editor->conn, &name_len);

#ifdef EMACS_BTL
  {
    char *execname =
      (STRINGP (Vinvocation_directory))?
	((char *) XSTRING_DATA (Vinvocation_directory)):0;

    switch (creq->type)
      {
      case CLRInitBTL:
	cadillac_terminate_logging (); /* #### rename me */
	cadillac_initialize_backtrace_logging /* #### rename me */
	  (data_filename, execname, (long) creq->limit, (long) creq->interval);
	break;

      case CLRInitPCL:
	cadillac_terminate_logging (); /* #### rename me */
	cadillac_initialize_pc_logging /* #### rename me */
	  (data_filename, execname, (long) creq->limit, (long) creq->interval);
	break;

      case CLRStart:
	cadillac_start_logging (); /* #### rename me */
	break;

      case CLRStop:
	cadillac_stop_logging (); /* #### rename me */
	break;

      case CLRTerminate:
	cadillac_terminate_logging (); /* #### rename me */
	break;

      case CLRSetLogSignal:
	cadillac_set_log_signal (creq->signal); /* #### rename me */
	break;

      default:
	error ("Bad logging request type %d", creq->type);
      }
  }
#else
  message ("Logging request, but no such code in image.");
#endif
}



/* creates a new buffer */
static void
handle_new_buffer_request (Editor *editor, CNewBufferRequest *creq)
{
  read_energize_buffer_data (editor->conn, &creq->buffer, editor, 0, 0,
			     creq->transientId, 0);
  if (!NILP (Venergize_create_buffer_hook))
    {
      CBuffer *cbu = &creq->buffer;
      BufferInfo *binfo = get_buffer_info_for_id (cbu->bufferId, editor);
      Lisp_Object buffer;
      if (binfo)
	{
	  Lisp_Object prev_frame;
	  buffer = binfo->emacs_buffer;
	  if (!NILP (binfo->frame))
	    {
	      prev_frame = Fselected_frame (Qnil);
	      Fselect_frame (binfo->frame);
	    }
	  va_run_hook_with_args (Qenergize_create_buffer_hook, 1, buffer);
	  if (!NILP (binfo->frame))
	    Fselect_frame (prev_frame);
	}
    }
}

/* Modifies the contents of a buffer */
static void
handle_modify_buffer_request (Editor *editor, CModifyBufferRequest *creq)
{
  read_energize_buffer_data (editor->conn, &creq->newData, editor,
			     creq->startPosition, creq->endPosition,
			     0, creq->head.data);
}

static void
make_buffer_and_extent_visible (Lisp_Object list, Lisp_Object go_there)
{
  call2 (Qenergize_make_many_buffers_visible, list, go_there);
}

/* pops a buffer and scroll to a extent: calls to lisp */
static void
handle_ensure_visible_request (Editor *editor, CEnsureVisibleRequest *creq)
{
  BufferInfo *binfo;
  Energize_Extent_Data *ext;
  Lisp_Object buffer_extent_list;
  struct gcpro gcpro1;

  buffer_extent_list = Qnil;
  GCPRO1 (buffer_extent_list);

  binfo = get_buffer_info_for_id (creq->bufferId, editor);
  if (!binfo)
    {
      message ("EnsureVisibleRequest: unknown buffer");
      goto finished;
    }

  if (!NILP (binfo->frame))
    {
      /* ignore ensure visible for postit note buffers */
      goto finished;
    }

  if (creq->extentId)
    {
      ext = get_extent_data (creq->extentId, binfo);
      if (!ext)
	message ("EnsureVisibleRequest: ignoring unknown extent");
    }
  else
    ext = 0;

  buffer_extent_list = Fcons (ext ? data_to_extent (ext) : Qnil, Qnil);
  buffer_extent_list = Fcons (binfo->emacs_buffer, buffer_extent_list);

  make_buffer_and_extent_visible (buffer_extent_list, creq->head.data ? Qt : Qnil);

 finished:
  CSkipRequest (editor->conn);
  UNGCPRO;
}

static void
handle_ensure_many_visible_request (Editor *editor,
				    CEnsureManyVisibleRequest *creq)
{
  BufferInfo *binfo;
  Energize_Extent_Data *ext;
  Lisp_Object buffer_extent_list;
  int n;
  EId buffer_id;
  EId extent_id;
  struct gcpro gcpro1;

  buffer_extent_list = Qnil;
  GCPRO1 (buffer_extent_list);

  for (n = creq->head.data,
       buffer_id = creq->bufferId,
       extent_id = creq->extentId;
       n;
       n--,
       buffer_id = n ? *(CGet (editor->conn, EId)) : 0,
       extent_id = n ? *(CGet (editor->conn, EId)) : 0)
    {
      binfo = get_buffer_info_for_id (buffer_id, editor);
      if (!binfo)
	{
	  message ("EnsureManyVisibleRequest: ignoring unknown buffer");
	  continue;
	}

      if (!NILP (binfo->frame))
	{
	  /* silently ignore ensure visible for postit note buffers */
	  continue;
	}

      if (extent_id)
	{
	  ext = get_extent_data (extent_id, binfo);
	  if (!ext)
	    message ("EnsureManyVisibleRequest: ignoring unknown extent");
	}
      else
	ext = 0;

      /* cons in reverse order and reverse the list before
	 calling make_buffer_and_extent_visible */
      buffer_extent_list = Fcons (binfo->emacs_buffer, buffer_extent_list);
      buffer_extent_list = Fcons (ext ? data_to_extent (ext) : Qnil,
				  buffer_extent_list);
    }
  buffer_extent_list = Fnreverse (buffer_extent_list);
  make_buffer_and_extent_visible (buffer_extent_list, Qt);

  UNGCPRO;
}

/* Update the cached menus, ie update the menubar for now. */
static void
handle_propose_choices_request (Editor *editor, CProposeChoicesRequest *req)
{
  BufferInfo* binfo;
  Lisp_Object buffer = Qnil;
  Lisp_Object extent = Qnil;
  Lisp_Object choices = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct reply_wait* rw;

  GCPRO3 (buffer, extent, choices);

  /* get the buffer */
  binfo = get_buffer_info_for_id (req->objectId, editor);
  if (binfo)
    buffer = binfo->emacs_buffer;
  else
    buffer = Qnil;

  /* get the extent */
  if (binfo && req->genericId)
    {
      Energize_Extent_Data* ext = get_extent_data (req->genericId, binfo);
      if (ext)
	extent = data_to_extent (ext);
      else
	extent = Qnil;
    }
  else
    extent = Qnil;

  /* find if we were waiting for a reply */
  rw = find_wait_reply (req->head.serial);

  /* handle the request */
  if (rw && rw->objectId == req->objectId && rw->genericId == req->genericId)
    {
      /* It's a reply for a get_energize_menu call */
      rw->answered_p = True;
      rw->status = 1;
      rw->menu_result = list_choices (buffer, extent, rw->only_name, req);
    }
  else
    {
      /* It's a menu update, call the hook */
      choices = list_choices (buffer, extent, Qnil, req);
      va_run_hook_with_args (Qenergize_menu_update_hook, 1, choices);
    }
  UNGCPRO;
}

/* Kills a buffer */
static void
unmodify_buffer_and_kill_it (Lisp_Object buffer)
{
  int speccount = specpdl_depth ();

  if (!BUFFER_LIVE_P (XBUFFER (buffer)))
    return;

  Fset_buffer_modified_p (Qnil, buffer);

  /* kill it.  This will call the Energize hook to do the right thing */
  Fkill_buffer (buffer);
}

static void
handle_kill_buffer_request (Editor *editor, CKillBufferRequest *creq)
{
  BufferInfo *binfo;

  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      message ("KillBufferVisibleRequest: unregistered buffer");
      return;
    }

  unmodify_buffer_and_kill_it (binfo->emacs_buffer);
}

static void
handle_remove_extents_request (Editor *editor, CRemoveExtentsRequest *creq)
{
  BufferInfo *binfo;
  int i;
  EId *ids;
  Lisp_Object restore_buffer_state_cons;
  int speccount = specpdl_depth ();

  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      message ("RemoveExtentsRequest: unregistered buffer");
      CSkipRequest (editor->conn);
      return;
    }

  /* enable buffer edits */
  restore_buffer_state_cons =
    Fcons (make_opaque_ptr ((void *) creq->bufferId),
	   Fcons (XBUFFER (binfo->emacs_buffer)->read_only, Qnil));

  record_unwind_protect (restore_buffer_state, restore_buffer_state_cons);

  XBUFFER (binfo->emacs_buffer)->read_only = Qnil;

  /* save old hook values */
  specbind (Qenergize_buffer_modified_hook, Qnil);

  ids = CGetN (editor->conn, EId, creq->nExtent);
  for (i = 0; i < creq->nExtent; i++)
    {
      Energize_Extent_Data *ext = get_extent_data (ids [i], binfo);
      if (ext)
	free_Energize_Extent_Data (ext, binfo, OFT_STANDALONE);
    }

  /* restore modified hooks and globals */
  unbind_to (speccount, Qnil);
}

#ifndef ENERGIZE_V2_HEADERS
static Lisp_Object
save_to_energize_unwind (Lisp_Object closure)
{
  BITS32 buffer_id = (BITS32) cons_to_long (closure);
  /* If the buffer ID is not 0, then the call to save-buffer
     didn't complete normally - so tell Energize the save was aborted. */
  if (buffer_id)
    {
      Editor *editor = energize_connection;
      if (editor && editor->conn)  /* Maybe the kernel has gone away. */
	{
	  CWriteBufferSaveAbortedHeader (editor->conn, buffer_id);
	  CWriteRequestBuffer (editor->conn);
	}
    }
  return Qnil;
}
#endif /*  ENERGIZE_V2_HEADERS */


/* handles a request to save a buffer from the kernel */
static void
handle_save_buffer_request (Editor *editor, CSaveBufferRequest *creq)
{
  BufferInfo *binfo;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;
  Lisp_Object closure = Qnil;

  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      message ("Server attempt to save a non registered buffer");
      return;
    }

  if (!EQ (binfo->emacs_buffer, Fcurrent_buffer ()))
    {
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (binfo->emacs_buffer);
    }

  GCPRO1 (closure);
  if (creq->head.data == CSExecuteSave)
    {
#ifndef ENERGIZE_V2_HEADERS
      Lisp_Object closure = make_opaque_ptr ((void *) creq->bufferId);
      record_unwind_protect (save_to_energize_unwind, closure);
#endif /*  ENERGIZE_V2_HEADERS */
      call0 (intern ("save-buffer"));
#ifndef ENERGIZE_V2_HEADERS
      /* clear out the id to tell the unwind-protect form that the save
	 completed normally. */
      set_opaque_ptr (closure, 0);
#endif /*  ENERGIZE_V2_HEADERS */
    }
  else
    write_energize_buffer_data (binfo);

  UNGCPRO;
  unbind_to (speccount, Qnil);
}

static void
handle_set_modified_flag_request (Editor* editor,
				  CSetModifiedFlagRequest* creq)
{
  BufferInfo *binfo;
  int speccount = specpdl_depth ();

  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      message ("Server attempt to set modified flag of a non registered buffer");
      return;
    }

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
  specbind (Qenergize_buffer_modified_hook, Qnil);

  /* Only set buffer modified time in older protocols
     as we handle the file timestamps ourselves now for
     CBFileYourself buffers. */
#ifndef ENERGIZE_V2_HEADERS
  if ((energize_connection->minor < 10) && !(binfo->flags &  CBFileYourself))
#endif
    {
      Fset_buffer_modtime (binfo->emacs_buffer, Qnil);
    }

  Fset_buffer_modified_p (creq->state ? Qt : Qnil, binfo->emacs_buffer);
  binfo->modified_state = creq->state;
  /* Mark the buffer so that we ask permission to Energize when the
   * user tries to modify it again */
  binfo->editable = 0;
  if (!creq->state)
    mark_all_extents_as_unmodified (binfo);
  unbind_to (speccount, Qnil);
}


/* handles requests regarding p_sheet associated to buffers */
static void
add_in_list_of_ids (int** ids, int* n_ids, int id)
{
  if (*n_ids == 0)
    {
      *n_ids = 1;
      *ids = (int*)xmalloc (sizeof (int));
    }
  else
    {
      *n_ids += 1;
      *ids = (int*)xrealloc (*ids, sizeof (int) * (*n_ids));
    }
  (*ids) [(*n_ids) - 1] = id;
}

static void
remove_from_list_of_ids (int** ids, int* n_ids, int id)
{
  int i;
  if (*n_ids)
    {
      /* look for id in *ids */
      for (i = 0; i < (*n_ids) && (*ids) [i] != id; i++);
      /* shift the remaining ones */
      for (; i < (*n_ids) - 1; i++)
	(*ids) [i] = (*ids) [i + 1];
      /* decrease the count */
      *n_ids -= 1;
      /* free array if empty */
      if (!*n_ids)
	{
	  xfree (*ids);
	  *ids = 0;
	}
    }
}

extern void make_psheets_desired (struct frame *, Lisp_Object);

static void
handle_buffer_sheet_request (Editor *editor, CSheetRequest *sreq,
			     EId buffer_id)
{
  BufferInfo *binfo;
  char *name;
  Connection *conn = editor->conn;

  if (!(binfo = get_buffer_info_for_id (buffer_id, editor)))
    {
      message ("Server attempt to use p_sheet in a non registered buffer");
      CSkipRequest (conn);
      return;
    }

  name = CGetVstring (conn, (ReqLen *) 0);
  switch ((CSheetRSubtype) sreq->head.data)
    {
    case CSCreate:
      lw_register_widget (name, name, sreq->sheetId, NULL, NULL,
			  handle_sheet_control_change, NULL);
      add_in_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			  sreq->sheetId);
      if (!strcmp (name, DEBUGGER_PSHEET_NAME))
	debuggerpanel_sheet = sreq->sheetId;
      break;

    case CSDelete:
      remove_from_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			       sreq->sheetId);
      cleanly_destroy_all_widgets (1, &sreq->sheetId);
      if (sreq->sheetId == debuggerpanel_sheet)
	{
	  desired_debuggerpanel_exposed_p = 0;
	  debuggerpanel_sheet = 0;
	}
      break;

    case CSHide:
      {
	Lisp_Object frmcons, devcons, concons;

	if (sreq->sheetId == debuggerpanel_sheet)
	  desired_debuggerpanel_exposed_p = 0;
	else
	  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	    {
	      struct frame *frame = XFRAME (Fcar (frmcons));
	      if (FRAME_X_P (frame))
		make_psheets_desired (frame, Qnil);
	    }
      }
      break;

    case CSShow:
      if (sreq->sheetId == debuggerpanel_sheet)
	desired_debuggerpanel_exposed_p = 1;
      else
	{
	  Lisp_Object frmcons, devcons, concons;

	  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	    {
	      struct frame *frame = XFRAME (Fcar (frmcons));
	      if (FRAME_X_P (frame))
		{
		  struct window *window =
		    XWINDOW (FRAME_SELECTED_WINDOW (frame));
		  if (EQ (window->buffer, binfo->emacs_buffer))
		    make_psheets_desired (frame, binfo->emacs_buffer);
		}
	    }
	}
      break;
    }
}



/* show busy */

static void
show_all_menubars_busy (int busy)
{
  Lisp_Object frmcons, devcons, concons;

  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
    {
      struct frame *f = XFRAME (XCAR (frmcons));
      if (FRAME_X_P (f))
	{
	  if (FRAME_X_MENUBAR_WIDGET (f))
	    lw_show_busy (FRAME_X_MENUBAR_WIDGET (f), busy);
	}
    }
}

static void
handle_show_busy_request (Editor *editor, CGenericRequest *preq)
{
  /* call the show busy routine of the	library for the menubar of
   * all frames */
  ReqLen len;

  char* why = CGetVstring (editor->conn, &len);

  show_all_menubars_busy (preq->head.data);
  Venergize_kernel_busy = preq->head.data ? Qt : Qnil;
  va_run_hook_with_args (Qenergize_kernel_busy_hook, 1, build_string (why));
}

/* This request creates, destroys, raises, or lowers a psheet or dialog box.
 */
static void
handle_sheet_request (Connection* conn, CSheetRequest* sreq, Widget parent)
{
  char* name = CGetVstring (conn, NULL);

  switch ((CSheetRSubtype)sreq->head.data)
    {
    case CSCreate:
      lw_create_widget (name, name, sreq->sheetId, 0, parent,
			!sreq->bufferId, 0, handle_sheet_control_change, 0);
      break;
    case CSDelete:
      cleanly_destroy_all_widgets (1, &sreq->sheetId);
      break;

    case CSShow:
      lw_pop_up_all_widgets (sreq->sheetId);
      break;

    case CSHide:
      lw_pop_down_all_widgets (sreq->sheetId);
      break;
    }
}

/* This request changes slot values in the psheets/dialog boxes. */
static void
handle_set_control_request (Connection* conn, CGenericRequest* creq)
{
  CSetControlRequest* sreq = &creq->setcontrol;
  widget_value val;
  widget_value* contents;

  unsigned long i;
  unsigned long n = sreq->nChoices;

  if (n > 0)
    {
      contents = (widget_value *) xmalloc (n * sizeof (widget_value));
      memset (contents, 0, (n * sizeof (widget_value)));
    }
  else
    contents = NULL;
  memset (&val, 0, sizeof (val));
  val.name = CGetVstring (conn, NULL);
  val.enabled = !(sreq->flags & CKInactive);
  val.selected = !!(sreq->flags & CKSelected);
  val.change = VISIBLE_CHANGE;
  val.contents = contents;

  for (i = 0; i < n; i++)
    {
      widget_value* cur = &contents [i];
      CChoice* choice = CGet (conn, CChoice);
      cur->name = CGetVstring (conn, NULL);
      cur->value = cur->name;
      cur->key = NULL;
      cur->enabled = !(choice->flags & CKInactive);
      cur->selected = !!(choice->flags & CKSelected);
      cur->change = VISIBLE_CHANGE;
      cur->contents = NULL;
      cur->call_data = NULL;
      cur->next = i == n - 1 ? NULL : &contents [i + 1];
      cur->toolkit_data = NULL;
      if ((i == 0 && n == 1) || cur->selected)
	{
	  val.value = cur->name;
	  if (!*val.value)
	    val.value = NULL;
	}
    }
  lw_modify_all_widgets (sreq->sheetId, &val, True);

  if (contents)
    xfree (contents);
}

static void
handle_sheet_control_change (Widget widget, EId sheet_id, void* arg)
{
  Connection*	conn;
  widget_value* val;
  widget_value* cur;
  widget_value* this_val = NULL;
  widget_value* cancel = NULL;
  char*		this_name;
  int delete_window_p = (((int) arg) == -1);


  if (!energize_connection)
    return;

  conn = energize_connection->conn;
  if (!conn)
    return;

  this_name = XtName (widget);
  val = lw_get_all_values (sheet_id);

  if (delete_window_p)
    /* Complete and utter kludge.  If this dbox was dismissed with the
       WM close box (WM_DELETE_WINDOW, meaning the widget was destroyed)
       then we look for a likely "cancel" button and pretend the user
       clicked on that.	 Really the protocol should be extended for this.
     */
    for (cur = val; cur; cur = cur->next)
      {
	char *v = cur->value;
	if (v &&
	    ((strlen (v) >= 6 && !strncmp (v, "cancel", 6)) ||
	     (strlen (v) >= 5 && !strncmp (v, "abort", 5))))
	  cancel = cur;
      }

  /* first send all the edited widgets */
  for (cur = val; cur; cur = cur->next)
    {
      /* do not send the widget that ran the callback */
      if (!strcmp (cur->name, this_name))
	this_val = cur;
      else if (cur == cancel)
	;
      /* send the edited widgets */
      else if (cur->edited)
	{
	  char* value = cur->value;
	  unsigned int flags = 0;

	  if (!cur->enabled)
	    flags |= CKInactive;
	  if (cur->selected)
	    flags |= CKSelected;

	  /* the kernel is brain dead and expect "1" and "0" as values
	     for the checkbox objects.	So if value is NULL, make it be "0"
	     or "1" depending on the selected state.  This is until we fix
	     the kernel. */
	  if (!value)
	    value = cur->selected ? "1" : "0";

	  CWriteSetControlRequest (conn, sheet_id, 0, cur->name, 1);
	  CWriteChoice (conn, 0, flags, value, 0);
	  CWriteLength (conn);
	}
    }

  if (delete_window_p && !this_val)
    {
      this_val = cancel;
/*	if (! this_val) abort (); */
    }

  /* Then send the widget that ran the callback */
  if (this_val)
    {
      CWriteSetControlRequest (conn, sheet_id, 0, this_val->name, 1);
      CWriteChoice (conn, 0, 0, this_val->value, 0);
      CWriteLength (conn);
      CWriteRequestBuffer (conn);
    }
}

/******************** Low level connection stuff ************************/
static void
add_in_connection_input_buffer (Connection *conn, char *s, int l)
{
  /* Should be in connection.c */
  if (conn->inread >= conn->infill)
    conn->inread = conn->infill = conn->inbuffer;

  CNeedInputSize (conn, l);
  memcpy (conn->infill, s, l);
  conn->infill += l;
}

static Lisp_Object
process_one_energize_request (void)
{
  Editor *editor = energize_connection;
  CEditorRequest *req;
  int res = 0;

  if (!editor) return make_int (res);

  if (!editor->conn)
    {
      close_energize_connection ();
      return make_int (res);
    }

  req = CReadEditorRequest (editor->conn);
  if (!req)
    {
      switch (errno)
	{
	case EWOULDBLOCK:
	  /* message ("ProcessEnergizeRequest: internal error EWOULDBLOCK"); */
	  res = -1;
	  break;

	case 0:
	case ECONNRESET:
	  message ("Connection to " IDENTITY_CRISIS " was closed.");
	  close_energize_connection ();
	  break;

	default:
	  message
	    ("System error on connection to " IDENTITY_CRISIS ", closing.");
	  close_energize_connection ();
	  break;
	}
    }
  else
    {
      res = 1;
      switch (req->head.reqType)
	{
	case RefuseConnectionRType:
	  message (IDENTITY_CRISIS " connection refused");
	  close_energize_connection ();
	  break;

	case AcceptConnectionRType:
	  {
	    CProtocol* proto = CGet (editor->conn, CProtocol);
	    editor->major = proto->major;
	    editor->minor = proto->minor;
	    message (IDENTITY_CRISIS " connection accepted");
	    CSkipRequest (editor->conn);
	  }
	  break;

	case NewBufferRType:
	  handle_new_buffer_request (editor, &req->newbuffer);
	  break;

	case QueryBufferRType:
	  {
	    EId buffer_id;
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CGetVstring (editor->conn, 0); /* skip directory */
	    CGetVstring (editor->conn, 0); /* skip file */
	    buffer_id = *CGet (editor->conn, EId);
	    if (rw)
	      {
		rw->answered_p = 1;
		rw->status = req->head.data;
		rw->objectId = buffer_id;
	      }
	  }
	  break;

	case EnsureVisibleRType:
	  handle_ensure_visible_request (editor, &req->ensurevisible);
	  break;

	case EnsureManyVisibleRType:
	  handle_ensure_many_visible_request (editor, &req->ensuremanyvisible);
	  break;

	case ModifyBufferRType:
	  handle_modify_buffer_request (editor, &req->modifybuffer);
	  break;

	case ProposeChoicesRType:
	  handle_propose_choices_request (editor,
					  &req->generic.proposechoices);
	  break;

	case ChoiceExecutedRType:
	  {
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CChoiceExecutedRequest* ce = &req->generic.choiceexecuted;
	    if (rw)
	      {
		rw->answered_p = 1;
		rw->status = ce->head.data;
		rw->message = CMakeVstring (editor->conn, 0);
	      }
	  }
	  break;

	case KillBufferRType:
	  handle_kill_buffer_request (editor, &req->killbuffer);
	  break;

	case ModifiedBufferRType:
	  {
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    if (rw)
	      {
		rw->answered_p = 1;
		if (rw->objectId == req->modifiedbuffer.bufferId)
		  rw->status = req->modifiedbuffer.state;
		else
		  rw->status = CMBufferLocked;
	      }
	  }
	  break;

	case SetModifiedFlagRType:
	  handle_set_modified_flag_request (editor, &req->setmodifiedflag);
	  break;

	case RemoveExtentsRType:
	  handle_remove_extents_request (editor, &req->removeextents);
	  break;

	case RenumberExtentsRType:
	  /* HandleDuplicateExtentRequest (editor, req); */
	  break;

#if 0
	case DialogRType:
	  /* HandleDialogRequest (editor, req, CurrentBuffer (editor)); */
	  break;
#endif

	case SaveBufferRType:
	  handle_save_buffer_request (editor, &req->savebuffer);
	  break;

	case SheetRType:{
	  EId buffer_id = req->generic.sheet.bufferId;
	  if (!buffer_id)
	    buffer_id = buffer_id_of_sheet (req->generic.sheet.sheetId);
	  if (buffer_id)
	    handle_buffer_sheet_request (editor, &req->generic.sheet,
					 buffer_id);
	  else
	    {
	      CSheetRSubtype type = (CSheetRSubtype)req->head.data;
	      if (type == CSDelete || type ==CSHide)
		/* #### ??? this does nothing. */
		Fselect_frame (Fselected_frame (Qnil));
	      handle_sheet_request (editor->conn, &req->generic.sheet,
				    FRAME_X_SHELL_WIDGET
				    (XFRAME (Fselected_frame (Qnil))));
	    }
	}
	  break;

	case SetControlRType:
	  handle_set_control_request (editor->conn, (CGenericRequest*) req);
	  break;

	case OpenPostitRType:
	case KillPostitRType:
	  message ("Don't know what to do with postit requests.");
	  break;

	case ShowBusyRType:
	  handle_show_busy_request (editor, (CGenericRequest*)req);
	  break;

	case LoggingRType:
	  handle_logging_request (editor, (CLoggingRequest*)req);
	  break;

#ifndef ENERGIZE_V2_HEADERS
	case KernelEventRType:
	  CSkipRequest (editor->conn);
	  break;
#endif

	default:
	  message("ProcessEnergizeRequest: can't handle request of type %d",
		req->head.reqType);
	}

    }

  return make_int (res);
}

static int inside_process_energize_request_1;

/* this must be called ONLY by unwind_protect in process_energize_request_1 */
static Lisp_Object
post_handle_request (Lisp_Object ignored)
{
  if (inside_process_energize_request_1 <= 0)
    abort ();
  inside_process_energize_request_1--;
  if (energize_connection && energize_connection->conn)
    CSkipRequest (energize_connection->conn);
  return Qnil;
}

static Lisp_Object
pop_conn (Lisp_Object arg)
{
  Connection *old_conn = (Connection *) get_opaque_ptr (arg);
  if (! old_conn)
    abort ();
  if (! energize_connection)
    return Qnil;
  if (energize_connection->conn == old_conn)
    abort ();

  if (CRequestDelayedP (energize_connection->conn))
    /* We don't call the CWait* functions any more so this shouldn't happen.
       But if it does someday, then we need to either copy the remaining
       bits from new_conn to old_conn, or loop processing requests until
       new_conn is drained.
     */
    abort ();

  DeleteConnection (energize_connection->conn);
  energize_connection->conn = old_conn;

  return Qnil;
}

static Lisp_Object
process_energize_request_1 ()
{
  Lisp_Object result;
  int speccount = specpdl_depth ();

  if (inside_process_energize_request_1)
    {
      /* When the energize process filter is called recursively, push a new
	 connection object.  The read-pointer of the connection buffer could
	 be in the middle of a request.	 However, we know that the fd itself
	 is always pointing between requests.  So making a new connection is
	 a way of skipping past the one request we were in the process of
	 reading when we allowed process output to be handled recursively.
       */
      Connection *old_conn = energize_connection->conn;
      Connection *new_conn =
	make_energize_connection ((void *) energize_connection,
				  old_conn->fdin, old_conn->fdout);
      energize_connection->conn = new_conn;
      record_unwind_protect (pop_conn, make_opaque_ptr (old_conn));
    }

  /* this must come after pop_conn() to get the right connection object */
  record_unwind_protect (post_handle_request, Qnil);

  inside_process_energize_request_1++;

  result = process_one_energize_request ();
  notify_delayed_requests ();

  /* decrements inside_process_energize_request_1 and possibly replaces
     energize_connection->conn with old_conn.
   */
  unbind_to (speccount, Qnil);

  return result;
}


/******** Initialize Energize-related state and set up connection ********/

static void
setup_connection (Editor *ed, unsigned int id1, unsigned int id2)
{
  CEditorRequest *req = CWriteEditorRequest (ed->conn, QueryConnectionRType);

  /* these 2 slots are ignored */
  req->generic.queryconnection.major = 0;
  req->generic.queryconnection.minor = 0;

  req->generic.queryconnection.cadillacId1 = id1;
  req->generic.queryconnection.cadillacId2 = id2;
  req->generic.queryconnection.nProtocols = 1;
  /* first numerical arg is major protocol number, second is minor */
  CWriteProtocol (ed->conn, 0, 10, "editor");
  CWriteLength (ed->conn);
  CWriteRequestBuffer (ed->conn);
}

/* this is used as the readMethod of the energize connection, so that
   the connection library won't do some buffering that messes us up.
   It does this buffering only if conn->readMethod == read, so using
   another function turns it off.
 */
static int
my_read (int fd, char *buf, int nb)
{
  return read (fd, buf, nb);
}

static Connection *
make_energize_connection (Editor *editor, int fdin, int fdout)
{
  Connection *conn = NewConnection ((void *)editor, fdin, fdout);
  if (conn)
    conn->readMethod = my_read;
  return conn;
}

DEFUN ("handle-energize-request", Fhandle_energize_request, 2, 2, 0, /*
Filter called when a request is available from Energize.
*/
       (proc, string))
{
  if (!NILP (string))
    CHECK_STRING (string);

  if (!energize_connection || !energize_connection->conn)
    {
      /* no need for a message here, Energize is dead */
      return make_int (0);
    }
  if (!energize_connection || (!EQ (energize_connection->proc, proc)))
    {
      message ("Got " IDENTITY_CRISIS " request but not from current connection ");
      return make_int (0);
    }

  if (!NILP (string))
    add_in_connection_input_buffer (energize_connection->conn,
				    (char *) XSTRING_DATA (string),
				    XSTRING_LENGTH (string));

  return process_energize_request_1 ();
}


Lisp_Object Venergize_process;

/* Opens a network connection to Energize.
 * server is a string.	It can end up with :<uid> or :<username>
 * in which case the uid is added to the TCP port to get the connection */
static void
connect_to_energize (char *server_str, char *arg)
{
  struct Lisp_Process *proc;
  Lisp_Object lp;
  Lisp_Object fil;
  char *host;
  unsigned int port;
  long flags;
  int id1;
  int id2;

  if (CGetPortNumber (server_str, &host, &port))
    {

      lp = Fopen_network_stream_internal (build_string ("energize"),
					  Qnil,
					  build_string (host),
					  make_int (port));
      if (!NILP (lp))
	{
	  int infd, outfd;
	  /* Don't ask the user for confirmation when exiting Emacs */
	  Fprocess_kill_without_query (lp, Qnil);
	  proc = XPROCESS (lp);
	  energize_connection = xnew (Editor);
	  get_process_file_descriptors (proc, &infd, &outfd);
	  energize_connection->conn =
	    make_energize_connection (energize_connection, infd, outfd);
	  energize_connection->proc = lp;
	  energize_connection->binfo_hash = make_hashtable (10);
	  energize_connection->image_table = 0;
	  energize_connection->gc_save = Qnil;
	  energize_connection->major = 0;
	  energize_connection->minor = 0;
	  peo = allocate_edit_options (10);
	  request_serial_number = 0;
	  global_reply_wait = 0;

	  if ((flags = fcntl (energize_connection->conn->fdin, F_GETFL, 0))
	      == -1)
	    abort ();

#ifdef O_NONBLOCK
	  if (fcntl (energize_connection->conn->fdin, F_SETFL,
		     flags & ~O_NONBLOCK)
	      == -1)
#else
	  if (fcntl (energize_connection->conn->fdin, F_SETFL,
		     flags & ~O_NDELAY)
	      == -1)
#endif
	    abort ();

	  XSETSUBR (fil, &SFhandle_energize_request);
	  set_process_filter (lp, fil, 1);

	  Venergize_kernel_busy = Qnil;

	  id1 = 0;
	  id2 = 0;
	  if (arg)
	    sscanf (arg, "%x,%x", &id1, &id2);

	  Venergize_buffers_list = Qnil;

	  setup_connection (energize_connection, id1, id2);

	  Venergize_process = lp;
	}
      else
	error ("couldn't connect to " IDENTITY_CRISIS " server");
    }
  else
    error ("couldn't determine " IDENTITY_CRISIS " server port number");


#ifdef ENERGIZE_V2_HEADERS
  if (energize_connection->minor > 9)
    {
      close_energize_connection ();
      error ("This Emacs doesn't understand " IDENTITY_CRISIS " version 3.");
    }

#endif /* ENERGIZE_V2_HEADERS */

}


/* Close the connection to energize.
 * Kills all the energize related buffer */
static void
close_energize_connection ()
{
  Editor *ed = energize_connection;

  if (ed)
    /* make this function as paranoid as we can */
    {
      /* cleanup the busy state */
      show_all_menubars_busy (False);
      Venergize_kernel_busy = Qnil;
      /* destroy all pop_up boxes */
      lw_destroy_all_pop_ups ();

      if (ed->conn)
	DeleteConnection (ed->conn);
      ed->conn = 0;

      if (ed->binfo_hash)
	{
	  int speccount = specpdl_depth ();

	  /* we are flushing everything, so we just ignore any change
	     hooks and don't make an effort to delete extents since they
	     are all going away */
	  specbind (Qenergize_buffer_modified_hook, Qnil);
	  specbind (Qinhibit_quit, Qt);
	  call0 (intern ("de-energize-all-buffers"));
	  unbind_to (speccount, Qnil);

	  free_hashtable (ed->binfo_hash);
	  ed->binfo_hash = 0;
	}

      /* Do this after de-energize-all-buffers or frame sizes thrash. */
      debuggerpanel_sheet = 0;
      desired_debuggerpanel_exposed_p = 0;

      free_edit_options (peo);

      if (ZEROP (ed->proc)) abort ();

      if (!NILP (ed->proc))
	Fdelete_process (ed->proc);
      ed->proc = Qnil;

      Venergize_buffers_list = Qnil;

      /* now kill buffers created to satisfy requests on old connection */
      xfree (ed);
    }

  /* mark as closed */
  energize_connection = 0;
  Venergize_process = Qnil;
}


DEFUN ("connect-to-energize-internal", Fconnect_to_energize_internal, 0, 2, 0, /*
Usage: (connect-to-energize-internal <server-name> <energizearg>)
Energizearg representing two 32 bit Energize ids that will be passed
to the Energize server when opening the Energize connection.
Only one connection can be open at a time.
*/
       (server_name, energize_arg))
{
  unsigned char *server;
  unsigned char *arg;

  if (!NILP (energize_arg))
    {
      CHECK_STRING (energize_arg);
      arg = XSTRING_DATA (energize_arg);
    }
  else
    arg = 0;

  if (!NILP (server_name))
    {
      CHECK_STRING (server_name);
      server = XSTRING_DATA (server_name);
    }
  else
    server = 0;

  /* since we are going ahead with this, make sure that we are
     really and truly disconnected first */
  Fclose_connection_to_energize ();

  connect_to_energize ((char *)server, (char *)arg);
  return Qnil;
}

DEFUN ("close-connection-to-energize", Fclose_connection_to_energize, 0, 0, 0, /*
Close the open Energize connection, if any.
*/
       ())
{
  if (!energize_connection) return Qnil;

  close_energize_connection ();
  return Qnil;
}


/* Extents stuff; this used to be in extents.c */

static void
set_extent_flags (EXTENT extent, Energize_Extent_Data *ext)
{
  /* clear every flag */
  if (!EXTENT_LIVE_P (extent))
    return;
  extent_start_open_p (extent) = 0;
  extent_end_open_p (extent) = 1;
  extent_read_only_p (extent) = 0;
  set_extent_mouse_face (extent, Qnil);
  extent_unique_p (extent) = 0;
  extent_duplicable_p (extent) = 0;
  extent_invisible_p (extent) = 0;

  set_extent_glyph (extent, 0, 0, GL_TEXT);
  set_extent_glyph (extent, 0, 1, GL_TEXT);

  if (ext)
    {
      ext->warn_modify = 0;

      switch (ext->extentType)
	{
	case CEAttribute:
	  break;

	case CEAbbreviation:
	  break;

	case CEWriteProtect:
	  extent_read_only_p (extent) = 1;
	  break;

	case CEGeneric:
	  {
	    GLYPH begin_glyph = 0;	/* always the class glyph */
	    GLYPH end_glyph = 0;	/* always the instance glyph */

	    /* if (ext->u.generic.gData->id)
	         SET_EXTENT_FLAG (extent, EF_MENU);*/

	    if (ext->u.generic.gData->glyph)
	      end_glyph = ext->u.generic.gData->glyph;
	    if (ext->u.generic.gData->cl && ext->u.generic.gData->cl->glyph)
	      begin_glyph = ext->u.generic.gData->cl->glyph;

#if 0
	    if (begin_glyph && end_glyph)
	      {
		begin_glyph = end_glyph;
		end_glyph = 0;
	      }
#endif

	    if (begin_glyph)
	      set_extent_glyph (extent, begin_glyph, 0, GL_TEXT);
	    if (end_glyph)
	      set_extent_glyph (extent, end_glyph, 1, GL_TEXT);

	    if (ext->u.generic.gData->cl &&
		(ext->u.generic.gData->cl->flags & CCElectric))
	      set_extent_mouse_face (extent, Qhighlight);
	    if (ext->u.generic.gData->cl &&
		(ext->u.generic.gData->cl->flags & CCWarnModified))
	      ext->warn_modify = 1;
#if 0 /* #### some day (soon?)... */
	    if (ext->u.generic.gData->cl &&
		(ext->u.generic.gData->cl->flags & CCColumn))
	      SET_EXTENT_FLAG (extent, EF_COLUMN);
#endif
	    extent_duplicable_p (extent) = 1;
	    extent_unique_p (extent) = 1;
	    break;
	  }

	default:
	  break;
	}
    }
}

extern Lisp_Object Fset_extent_face (Lisp_Object extent, Lisp_Object name);

static void
set_extent_attributes_index (EXTENT extent, Energize_Extent_Data *ext)
{
  int graphic_attributes;

  if (!ext)
    extent_face_id (extent) = -1;
  else
    {
      switch (ext->extentType)
	{
	case CEAttribute:
	  graphic_attributes = ext->u.attr.attrValue;
	  break;

	case CEGeneric:
	  graphic_attributes = ext->u.generic.gData->attribute;
	  break;

	case CEWriteProtect:
	  /* this type has NO display attributes */
	  extent_face_id (extent) = -1;
	  return;

	default:
	  graphic_attributes = GA_NO_CHANGE;
	}

      if (graphic_attributes >= GA_MAX)
	graphic_attributes = GA_NO_CHANGE;

      {
	/* The Venergize_attributes_mapping global is an alist of the form
	   ( (<kernel-attribute-number> . <emacs-face-name> ) ... )
	   */
	Lisp_Object face = Fcdr (Fassq (make_int (graphic_attributes),
					Venergize_attributes_mapping));
	Lisp_Object e;
	XSETEXTENT (e, extent);
	if (NILP (face))
	  message ("Unknown Energize attribute %d", graphic_attributes);
	else if (EQ (face, Qdefault))
	  Fset_extent_face (e, Qnil);
	else
	  Fset_extent_face (e, face);
      }
    }
}

void
restore_energize_extent_state (EXTENT extent)
{
  Energize_Extent_Data *ext = energize_extent_data (extent);
  if (!ext) return;
  set_extent_flags (extent, ext);
  set_extent_attributes_index (extent, ext);
}

DEFUN ("extent-to-generic-id", Fextent_to_generic_id, 1, 1, 0, /*
Return Energize ID of buffer of EXTENT.
*/
       (extent_obj))
{
  CHECK_EXTENT (extent_obj);
  return word_to_lisp (energize_extent_data_id
		       (energize_extent_data (XEXTENT (extent_obj))));
}



/* buffer modified routines */
static void
send_buffer_modification_state (Editor *editor, BufferInfo *binfo, int flag)
{
  Connection *conn = editor->conn;
  EId bufferId = binfo->id;

  if (conn)
    {
      int state = (flag)?(CMBufferSetModified):(CMBufferSetUnmodified);
      CWriteModifiedBufferHeader (conn, bufferId, state);
      CWriteRequestBuffer (conn);
    }
}

/* returns 1 if buffer is locked (non-editable),
   0 if it isn't (editable), and -1 if it can't tell
 */
static int
check_buffer_lock (Editor *editor, BufferInfo *binfo)
{
  Connection *conn = editor->conn;
  struct reply_wait rw;

  /* If permision already granted by kernel dont' ask again */
  if (binfo->editable)
    return 0;

  /* If can't ask say we do not know */
  if (!conn)
    return -1;

  /* Ask the kernel */
  CWriteModifiedBufferHeader (conn, binfo->id, CMBufferQueryLock);
  conn->header->serial = ++request_serial_number;
  CWriteRequestBuffer (conn);

  rw.serial = request_serial_number;
  rw.objectId = binfo->id;
  rw.status = -1;

  if (!wait_for_reply (&rw))
    return -1;

  if (rw.status == CMBufferLocked)
    {
      /* Buffer is locked by kernel so we cannot edit it */
      return 1;
    }
  else if (rw.status == CMBufferUnlocked)
    {
      /* Buffer is unlocked by kernel: edit permission granted! */
      binfo->editable = 1;
      return 0;
    }
  else
    {
      /* This should never happen */
      return -1;
    }
}


/* Ask the kernel if BUFFER is currently locked -- waits for answer */
static Lisp_Object
buffer_locked_p (Lisp_Object buffer)
{
  BufferInfo *binfo;

  if (!energize_connection)
    return Qnil;

  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);

  CHECK_BUFFER (buffer);

  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);

  if (!binfo)
    {
      /* Not an Energize buffer, return Qnil: can edit buffer */
      return Qnil;
    }
  else
    {
      /* Energize buffer, check if editable */
      return check_buffer_lock (energize_connection, binfo) == 0 ? Qnil : Qt;
    }
}



/* Called by map_extents function called by Fenergize_send_buffer_modified
 */
static int
notify_extent_modified (EXTENT extent, void *arg)
{
  /* arg is a binfo_and_state */
  binfo_and_state *bans = (binfo_and_state*)arg;
  Energize_Extent_Data *ext;
  BufferInfo *binfo = bans->binfo;
  Connection *conn = binfo->editor->conn;
  EId *extent_id;
  Lisp_Object extent_obj;

  XSETEXTENT (extent_obj, extent);
  if ((ext = extent_to_data (extent_obj))
      && ext->warn_modify
      && ext->extentType == CEGeneric
      && ext->u.generic.gData
      && ext->u.generic.gData->cl
      && (ext->u.generic.gData->cl->flags & CCWarnModified)
      && ext->u.generic.gData->modified_state != bans->state)
    {
      if (bans->tell_energize)
	{
	  CWriteModifiedExtentsHeader (conn, binfo->id, bans->state, 1);
	  extent_id = CPut (conn, EId);
	  *extent_id = ext->id;
	  CWriteLength (conn);
	  CWriteRequestBuffer (conn);
	}
      ext->u.generic.gData->modified_state = bans->state;
    }
  return 0;
}

static int
ceiwme_lower_mf (EXTENT e, void *arg)
{
  Lisp_Object extent;
  Energize_Extent_Data *ext;
  XSETEXTENT (extent, e);
  ext = extent_to_data (extent);
  if (ext && ext->warn_modify)
    *((EXTENT *) arg) = e;
  return 0;
}

static int
ceiwme_upper_mf (EXTENT e, void *arg)
{
  Lisp_Object extent;
  Energize_Extent_Data *ext;
  XSETEXTENT (extent, e);
  ext = extent_to_data (extent);
  if (ext && ext->warn_modify)
    {
      *((EXTENT *) arg) = e;
      return 1;
    }
  else
    return 0;
}

static void
coerce_endpoints_to_be_inside_warn_on_modify_extents (Bufpos *from_ptr,
						      Bufpos *to_ptr,
						      struct buffer *b)
{
  EXTENT lower_extent = 0;
  EXTENT upper_extent = 0;
  Bufpos lower_bound = *from_ptr;
  Bufpos upper_bound = *to_ptr;
  Lisp_Object tmp;

  /* make sure that from <= to */
  {
    Bufpos tmp_int = max (lower_bound, upper_bound);
    *from_ptr = lower_bound = min (lower_bound, upper_bound);
    *to_ptr = upper_bound = tmp_int;
  }

  if (!BUFFER_NOTIFY_BACKGROUND_BIT_SET_P (b))
    return;

  map_extents (BUF_BEG (b), lower_bound, ceiwme_lower_mf,
	       (void *) &lower_extent, make_buffer (b), 0, ME_END_CLOSED);
  if (!lower_extent)
    {
      lower_bound = BUF_BEG (b);
      map_extents (upper_bound, BUF_Z (b), ceiwme_upper_mf,
		   (void *) &upper_extent, make_buffer (b), 0, ME_END_CLOSED);
      if (!upper_extent)
	upper_bound = BUF_Z (b);
      else
	{
	  Bufpos xstart;
	  XSETEXTENT (tmp, upper_extent);
	  xstart = XINT (Fextent_start_position (tmp));
	  upper_bound = max (upper_bound, xstart);
	}
    }
  /* I forget why, but if we found an lower bound, we don't need to find
     an upper bound */
  else
    {
      Bufpos xstart;
      XSETEXTENT (tmp, lower_extent);
      xstart = XINT (Fextent_start_position (tmp));
      lower_bound = min (lower_bound, xstart);
    }

  *from_ptr = lower_bound;
  *to_ptr = upper_bound;
  return;
}

static void
mark_all_extents_as_unmodified (BufferInfo *binfo)
{
  binfo_and_state bans;
  bans.binfo = binfo;
  bans.state = FALSE;
  bans.tell_energize = FALSE;

  map_extents (BUF_BEG (XBUFFER (binfo->emacs_buffer)),
	       BUF_Z (XBUFFER (binfo->emacs_buffer)),
	       notify_extent_modified, &bans,
	       binfo->emacs_buffer, 0, ME_END_CLOSED);
}

/* Send the BufferModified events for the current buffer.
 * Handles both global buffer modified and extents modified. */
DEFUN ("energize-send-buffer-modified", Fenergize_send_buffer_modified, 3, 3, 0, /*
Send a BufferModified request for the current buffer.
*/
       (state, from, to))
{
  int modifiedp = NILP (state)? 0 : 1;
  Lisp_Object buffer;
  BufferInfo *binfo;
  Bufpos from_int = XINT (from);
  Bufpos to_int = XINT (to);

  if (!energize_connection || !energize_connection->conn) return Qnil;

  XSETBUFFER (buffer, current_buffer);

  Fenergize_barf_if_buffer_locked ();

  if (binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection))
    {
      /* now make sure that from and to
	 are inside some warn_on_modify extents somewhere */
      coerce_endpoints_to_be_inside_warn_on_modify_extents
	(&from_int, &to_int, current_buffer);
      XSETINT (from, from_int);
      XSETINT (to, to_int);

      if (binfo->modified_state != modifiedp)
	{
	  send_buffer_modification_state (energize_connection, binfo,
					  modifiedp);
	  binfo->modified_state = modifiedp;
	}
#ifndef ENERGIZE_V2_HEADERS
      if (!(binfo->flags & CBFileYourself))
#endif
	{
	  if (modifiedp)
	    {
	      binfo_and_state bans;
	      bans.binfo = binfo;
	      bans.state = 1;
	      bans.tell_energize = 1;
	      map_extents (XINT (from), XINT (to),
			   notify_extent_modified, &bans,
			   binfo->emacs_buffer, 0,
			   ME_END_CLOSED);
	    }
	  else
	    mark_all_extents_as_unmodified (binfo);
	}
    }
  return Qnil;
}

DEFUN ("energize-barf-if-buffer-locked",
       Fenergize_barf_if_buffer_locked, 0, 0, 0, /*
Error if the buffer is locked.
*/
       ())
{
  Lisp_Object buffer;
  XSETBUFFER (buffer, current_buffer);

  if (!energize_connection || !energize_connection->conn)
    return Qnil;

  while (!NILP (buffer_locked_p (buffer)))
    {
      notify_delayed_requests ();
      Fsignal (Qbuffer_locked_by_energize, (Fcons (buffer, Qnil)));
    }
  return Qnil;
}


DEFUN ("energize-send-region", Fenergize_send_region, 2, 2, 0, /*
Send region as user input
*/
       (start, end))
{
  BufferInfo *binfo;
  Lisp_Object b;
  CEditorRequest *req;

  if (!energize_connection || !energize_connection->conn)
    error ("Not connected to " IDENTITY_CRISIS);

  XSETBUFFER (b, current_buffer);
  if (binfo = get_buffer_info_for_emacs_buffer (b, energize_connection))
    {
      Bufpos st, en;
      Bufpos ceil;

      get_buffer_range_char (current_buffer, start, end, &st, &en);

      do
	{
	  ceil = BUF_CEILING_OF (current_buffer, st);

	  req = CWriteEditorRequest (energize_connection->conn,
				     UserTypedSomethingRType);
	  req->usertypedsomething.bufferId = binfo->id;
	  CWriteVstringLen
	    (energize_connection->conn,
	     (char *) BUF_BYTE_ADDRESS (current_buffer, st), ceil - st);
	  CWriteLength (energize_connection->conn);
	  CWriteRequestBuffer (energize_connection->conn);
	  st = ceil;
	} while (st < en);
      return Qnil;
    }
  return Qnil;
}

DEFUN ("connected-to-energize-p", Fconnected_to_energize_p, 0, 0, 0, /*
Return nil if no connection to Energize.
*/
       ())
{
  if (!energize_connection ||
      !energize_connection->conn ||
      !energize_connection->binfo_hash ||
      !PROCESSP (energize_connection->proc))
    return Qnil;
  else
    return Qt;
}

DEFUN ("energize-user-input-buffer-mark",
       Fenergize_user_input_buffer_mark, 0, 1, 0, /*
Return the mark associated to the given Energize buffer.
*/
       (buffer))
{
  BufferInfo *binfo;

  XSETBUFFER (buffer, decode_buffer (buffer, 0));
  if (!energize_connection) return Qnil;
  if ((binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return binfo->output_mark;
  else
    return Qnil;
}

Lisp_Object
energize_get_buffer_process (Lisp_Object buf)
{
  BufferInfo *binfo;

  if (!BUFFERP (buf)) return Qnil;
  if (!energize_connection) return Qnil;
  binfo = get_buffer_info_for_emacs_buffer (buf, energize_connection);
  if (!binfo) return Qnil;
  if (! binfo->buffer_type) return Qnil;
  if (!strcmp (binfo->buffer_type, "energize-debugger-buffer") ||
      !strcmp (binfo->buffer_type, "energize-log-file-buffer"))
    return Venergize_process;
  return Qnil;
}


static int
get_energize_connection_and_buffer_id (Lisp_Object buffer, void **conn_ptr,
				       long *buffer_id_ptr)
{
  BufferInfo *binfo;

  if (!energize_connection || !energize_connection->conn) return 0;

  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);

  *conn_ptr = (void *) energize_connection->conn;
  *buffer_id_ptr = (long) binfo ? binfo->id : 0;
  return 1;
}

static int
get_energize_connection_and_current_buffer_id (void **conn_ptr,
					       long *buffer_id_ptr)
{
  Lisp_Object lisp_buffer;
  XSETBUFFER (lisp_buffer, current_buffer);

  return get_energize_connection_and_buffer_id (lisp_buffer, conn_ptr,
						buffer_id_ptr);
}

int *
get_psheets_for_buffer (Lisp_Object buffer, int *count_ptr)
{
  BufferInfo *binfo;

  if (!energize_connection || !energize_connection->conn) return 0;

  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
  if (!binfo) return 0;

  if (count_ptr) *count_ptr = binfo->n_p_sheets;
  return binfo->p_sheet_ids;
}

void
notify_energize_sheet_hidden (EId id)
{
  EId buffer_id = buffer_id_of_sheet (id);
  if (!buffer_id)
    return;

  if (buffer_id && energize_connection && energize_connection->conn)
    {
      CWriteSheetRequest (energize_connection->conn,
			  CSHide, id, buffer_id, "");
      CWriteRequestBuffer (energize_connection->conn);
    }
}

DEFUN ("energize-query-buffer", Fenergize_query_buffer, 1, 2, 0, /*
Ask Energize to create a buffer containing the file filename.
Returns the buffer or NIL if Energize cannot create the buffer.
If second argument just-ask is T, just ask if Energize
already knows about the file and returns T if yes, NIL otherwise.
*/
       (filename, just_ask))
{
  struct Lisp_String *filename_str;
  CEditorRequest *creq;
  char *dir_sep;
  struct reply_wait rw;

  if (!energize_connection || !energize_connection->conn)
    return Qnil;

  filename = Fexpand_file_name (filename, Qnil);
  filename_str = XSTRING (filename);

  dir_sep = (char *) strrchr ((char *) string_data (filename_str), '/');

  creq = CWriteEditorRequest (energize_connection->conn, QueryBufferRType);
  creq->head.data = !NILP (just_ask);
  creq->head.serial = ++request_serial_number;
  CWriteVstringLen (energize_connection->conn, (char *) string_data (filename_str),
		    (dir_sep)? (dir_sep - (char *) string_data (filename_str)): 0);
  CWriteVstringLen (energize_connection->conn,
		    (char *) string_data (filename_str), string_length (filename_str));
  CWriteLength (energize_connection->conn);
  CWriteRequestBuffer (energize_connection->conn);

  rw.serial = request_serial_number;
  rw.objectId = 0;

  if (!wait_for_reply (&rw))
    return Qnil;

  if (rw.status)
    {
      if (rw.objectId)
	{
	  BufferInfo* binfo = get_buffer_info_for_id (rw.objectId,
						      energize_connection);
	  return binfo ? binfo->emacs_buffer : Qt;
	}
      else
	return Qt;
    }
  else
    return Qnil;
}


DEFUN ("energize-protocol-level", Fenergize_protocol_level, 0, 0, 0, /*
Return the Energize protocol level.
*/
       ())
{
  return
    energize_connection
      ? Fcons (make_int (energize_connection->major),
	       make_int (energize_connection->minor))
	: Qnil;
}


DEFUN ("energize-psheets-visible-p", Fenergize_psheets_visible_p, 0, 1, 0, /*
Whether the (optional) frame currently has open psheets.
*/
       (frame))
{
  if (NILP (frame))
    XSETFRAME (frame, XFRAME(Fselected_frame(Qnil)));
  CHECK_FRAME (frame);
  if (FRAME_X_CURRENT_PSHEETS (XFRAME (frame)))
    return Qt;
  return Qnil;
}

DEFUN ("energize-buffer-has-psheets-p", Fenergize_buffer_has_psheets_p, 0, 1, 0, /*
Whether the buffer has psheets associated with it.
*/
       (buf))
{
  int count;
  if (NILP (buf))
    buf = Fcurrent_buffer ();
  CHECK_BUFFER (buf);
  if (get_psheets_for_buffer (buf, &count))
    return Qt;
  return Qnil;
}


void
make_psheets_desired (struct frame *f, Lisp_Object buffer)
{
  int count;
  int *psheets;

  if (NILP (buffer) || !(psheets = get_psheets_for_buffer (buffer, &count)))
    {
      FRAME_X_DESIRED_PSHEETS (f) = 0;
      FRAME_X_DESIRED_PSHEET_COUNT (f) = 0;
      FRAME_X_DESIRED_PSHEET_BUFFER (f) = Qnil;
    }
  else
    {
      /* Do not show the debugger panel in this function.  The
       * debugger panel should never be listed in the visible psheets. */
      extern int debuggerpanel_sheet;
      
      if (count == 1 && psheets [0] == debuggerpanel_sheet)
	return;

      FRAME_X_DESIRED_PSHEETS (f) = psheets;
      FRAME_X_DESIRED_PSHEET_COUNT (f) = count;
      FRAME_X_DESIRED_PSHEET_BUFFER (f) = buffer;
    }

  /* Garbage the frame so that the sheets get recomputed right away and not
     the next time some display change happens.  Possibly redisplay should
     notice this on its own without the garbaged flag.  But once redisplay
     gets smarter about such things, all garbagers should be revisited.
   */
  MARK_FRAME_CHANGED (f);
}

Lisp_Object
desired_psheet_buffer (struct frame *f)
{
  if (FRAME_X_P (f))
    return FRAME_X_DESIRED_PSHEET_BUFFER (f);
  else
    return Qnil;
}

/* This function is invoked when the user clicks on the "sheet" button.
 */
DEFUN ("energize-toggle-psheet", Fenergize_toggle_psheet, 0, 0, "", /*

*/
       ())
{
  struct frame *frame = XFRAME(Fselected_frame(Qnil));
  Lisp_Object buffer = Fwindow_buffer (Fselected_window (Qnil));
  if (EQ (buffer, desired_psheet_buffer (frame)))
    make_psheets_desired (frame, Qnil);
  else
    make_psheets_desired (frame, buffer);
  return Qnil;
}


static void energize_show_menubar_of_buffer (Lisp_Object frame,
					     Lisp_Object buffer,
					     Lisp_Object psheets_too);

/* This is called when a buffer becomes visible in some window.

   Show the menubar associated with this buffer, and show the psheets as
   well if this buffer is the last buffer whose psheets were visible in
   this frame.
 */
void
energize_buffer_shown_hook (struct window *window)
{
  struct frame* frame = XFRAME (window->frame);
  Lisp_Object buffer = window->buffer;
  Lisp_Object pbuf;

  if (! FRAME_X_P (frame)) return;
  pbuf = desired_psheet_buffer (frame);

  if (!MINI_WINDOW_P (window))
    energize_show_menubar_of_buffer (window->frame, buffer,
				     (EQ (buffer, pbuf) ? Qt : Qnil));
}


static int
find_buffer_in_different_window (window, buffer, not_in)
     struct window* window;
     Lisp_Object buffer;
     struct window* not_in;
{
  Lisp_Object child;
  if (!NILP (window->buffer))
    {
      /* a leaf window */
      return (EQ (window->buffer, buffer) && (window != not_in));
    }
  else
    {
      /* a non leaf window, visit either the hchild or the vchild */
      for (child = !NILP (window->vchild) ? window->vchild : window->hchild;
	   !NILP (child);
	   child = XWINDOW (child)->next)
	{
	  if (find_buffer_in_different_window (XWINDOW (child), buffer,
					       not_in))
	    return 1;
	}
      return 0;
    }
}

/* returns 1 if the buffer is only visible in window on frame f */
static int
buffer_only_visible_in_this_window_p (Lisp_Object buffer, 
				      struct frame* f, 
				      struct window* window)
{
  return !find_buffer_in_different_window (XWINDOW (f->root_window), buffer,
					   window);
}

/* This is called just before a buffer which is visible becomes invisible,
   either because some other buffer is about to be made visible in its window,
   or because that window is being deleted.

   If this buffer's psheets are visible, hide them.
 */
void
energize_buffer_hidden_hook (struct window *window)
{
  struct frame *f = XFRAME (window->frame);

  if (! FRAME_X_P (f)) return;

  /* hides the p_sheet if we are changing the buffer of the
   * selected window of the frame and the p_sheet where displayed */
  if (EQ (window->buffer, desired_psheet_buffer (f))
      && buffer_only_visible_in_this_window_p (window->buffer, f, window))
    make_psheets_desired (f, Qnil);
}


/* This is called just before the selected window is no longer the selected
   window because some other window is being selected.  The given window is
   not being deleted, it is merely no longer the selected one.

   This doesn't do anything right now.
 */
void
energize_window_deselected_hook (struct window *window)
{
}


/* This is called just after a window has been selected.

   Show the menubar associated with this buffer; leave the psheets as
   they are.
 */
void
energize_window_selected_hook (struct window *window)
{
  struct frame* frame = XFRAME (window->frame);
  Lisp_Object buffer = window->buffer;

  if (FRAME_X_P (frame) && !MINI_WINDOW_P (window))
    energize_show_menubar_of_buffer (window->frame, buffer, Qnil);
}



int current_debuggerpanel_exposed_p;
int desired_debuggerpanel_exposed_p;
int debuggerpanel_sheet;

static void
energize_show_menubar_of_buffer (Lisp_Object frame,
				 Lisp_Object buffer,
				 Lisp_Object psheets_too)
{
  struct frame *f = decode_x_frame (frame);

  if (! NILP (psheets_too))
    {
      Lisp_Object buffer;
      XSETBUFFER (buffer, current_buffer);
      make_psheets_desired (f, buffer);
    }
}


/* edit-mode dialog box
   This stuff really sucks
 */

static struct editmode {
  int ok, external, view, edit, window, split;
  char *other;
} editmode;

static void
edit_mode_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  widget_value *data;
  char *name = (char *) client_data;

  if ((int) client_data == -1) name = "cancel";	/* WM_DELETE_WINDOW */

  if (!strcmp (XtName (widget), "otherText")) /* screw it */
    ;
  else if (!strcmp (name, "externalBox"))
    {
      /* make the text slot be active only if "other" is selected */
      data = malloc_widget_value ();
      data->name = "externalOther";
      if (! lw_get_some_values (id, data)) abort ();
      data->enabled = data->selected;
      data->name = "otherText";
      lw_modify_all_widgets (id, data, True);
      free_widget_value (data);
    }
  else if (!strcmp (name, "cancel"))
    {
      editmode.ok = -1;
      lw_destroy_all_widgets (id);
    }
  else if (!strcmp (name, "help"))
    {
      Lisp_Object v = Fmake_vector (make_int (3), Qt);
      vector_data (XVECTOR (v)) [0] = build_string ("ok");
      vector_data (XVECTOR (v)) [1] = list1 (Qignore);
      Fpopup_dialog_box (list2 (build_string ("dbx_editmode_help"), v));
    }
  else if (!strcmp (name, "ok"))
    {
      editmode.ok = 1;
      data = malloc_widget_value ();
      data->name = "externalEmacs";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 0;
      data->name = "externalViXterm";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 1;
      data->name = "externalViCmdtool";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 2;
      data->name = "externalOther";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 3;
      data->name = "otherText";
      if (! lw_get_some_values (id, data)) abort ();
      editmode.other = data->value;

      data->name = "emacsView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 0;
      data->name = "viView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 1;
      data->name = "lessView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 2;

      data->name = "editEmacs";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.edit = 0;
      data->name = "editVi";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.edit = 1;

      data->name = "windowOne";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 0;
      data->name = "windowSeveral";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 1;
      data->name = "windowMany";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 2;

      data->name = "splitScreens";
      if (! lw_get_some_values (id, data)) abort ();
      editmode.split = !!data->selected;

      free_widget_value (data);
      lw_destroy_all_widgets (id);
    }
  else
    {
      abort ();
    }
}

static int
editmode_done (void *arg)
{
  return (editmode.ok != 0);
}

extern LWLIB_ID new_lwlib_id (void);

DEFUN ("energize-edit-mode-prompt", Fenergize_edit_mode_prompt, 6, 6, 0, /*

*/
       (external, edit_mode, view_mode, other_text, window, split))
{
  int dbox_id;
  struct frame *f = selected_frame ();
  widget_value *data;
  Widget parent, dbox;
  Lisp_Object frame = Qnil;

  XSETFRAME (frame, f);
  CHECK_X_FRAME (frame);
  parent = FRAME_X_SHELL_WIDGET (f);

  CHECK_INT (external);
  CHECK_INT (edit_mode);
  CHECK_INT (view_mode);
  CHECK_INT (window);
  CHECK_INT (split);
  CHECK_STRING (other_text);

  editmode.ok = 0;
  editmode.external = XINT (external);
  editmode.view = XINT (view_mode);
  editmode.edit = XINT (edit_mode);
  editmode.window = XINT (window);
  editmode.split = XINT (split);
  editmode.other = 0;

  data = malloc_widget_value ();
  data->name = "editmode";
  data->value = "editmode";
  data->enabled = 1;

  dbox_id = new_lwlib_id ();
  dbox = lw_create_widget ("editmode", "editmode", dbox_id, data, parent,
			   1, 0, edit_mode_callback, 0);
  data->value = 0;

  data->name = "button1"; data->call_data = data->value = "ok";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button2"; data->call_data = data->value = "cancel";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button3"; data->call_data = data->value = "help";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "externalBox";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "otherText"; data->call_data = "otherText";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "message"; data->value = "editmode";
  lw_modify_all_widgets (dbox_id, data, True);

  data->selected = 1;
  switch (editmode.external)
    {
    case 0: data->name = "externalEmacs"; break;
    case 1: data->name = "externalViXterm"; break;
    case 2: data->name = "externalViCmdtool"; break;
    case 3: data->name = "externalOther"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.view)
    {
    case 0: data->name = "emacsView"; break;
    case 1: data->name = "viView"; break;
    case 2: data->name = "lessView"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.edit)
    {
    case 0: data->name = "editEmacs"; break;
    case 1: data->name = "editVi"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.window)
    {
    case 0: data->name = "windowOne"; break;
    case 1: data->name = "windowSeveral"; break;
    case 2: data->name = "windowMany"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);

  data->name = "otherText";
  data->selected = 0;
  data->value = (char *) XSTRING_DATA (other_text);
  data->enabled = (editmode.external == 3);
  lw_modify_all_widgets (dbox_id, data, True);

  data->name = "splitScreens";
  data->enabled = 1;
  data->selected = editmode.split;
  data->value = 0;
  lw_modify_all_widgets (dbox_id, data, True);

  free_widget_value (data);

  lw_pop_up_all_widgets (dbox_id);

  wait_delaying_user_input (editmode_done, 0);

  if (editmode.ok == -1)
    return Fcons (external,
		  list5 (edit_mode, view_mode, other_text, window, split));
  else if (editmode.ok == 1)
    return Fcons (make_int (editmode.external),
		  list5 (make_int (editmode.view),
			 make_int (editmode.edit),
			 build_string (editmode.other ? editmode.other : ""),
			 make_int (editmode.window),
			 make_int (editmode.split)));
  else
    abort ();
}

static LWLIB_ID search_id;
static int last_search_up_p;

static void
hide_search_dialog (Widget w, LWLIB_ID id)
{
#if 0
  /* I'd like to do this, but the widget occasionally gets FUCKED */
  XUnmapWindow (XtDisplay (w), XtWindow (w));
#else
  lw_destroy_all_widgets (id);
#endif
}


static void
search_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  Widget parent = widget;
  widget_value *data;
  char *name = (char *) client_data;
  Lisp_Object search, replace;
  Lisp_Object case_sensitive_p, regexp_p, direction, match_word_p;
  Lisp_Object device = Qnil;

  if ((int) client_data == -1) name = "done";	/* WM_DELETE_WINDOW */

  while (parent && XtClass (parent) != xmDialogShellWidgetClass)
    parent = XtParent (parent);
  if (! parent) abort ();

  if (!strcmp (name, "done"))
    {
      hide_search_dialog (parent, id);
      return;
    }
#if 0
  else if (!strcmp (name, "help"))
    {
      Lisp_Object v = Fmake_vector (3, Qt);
      vector_data (XVECTOR (v)) [0] = build_string ("ok");
      vector_data (XVECTOR (v)) [1] = list1 (Qignore);
      Fpopup_dialog_box (list2 (build_string ("dbx_search_help"), v));
      return;
    }
#endif

  {
    struct device *d = get_device_from_display (XtDisplay (widget));
    XSETDEVICE (device, d);
    DEVICE_X_MOUSE_TIMESTAMP (d) = DEVICE_X_GLOBAL_MOUSE_TIMESTAMP (d);
  }

  if (!strcmp (name, "gotoStart"))
    {
      signal_special_Xt_user_event (device, Qcall_interactively,
				    Qbeginning_of_buffer);
    }
  else if (!strcmp (name, "gotoEnd"))
    {
      signal_special_Xt_user_event (device, Qcall_interactively,
				    Qend_of_buffer);
    }
  else if (!strcmp (name, "scrollForward"))
    {
      signal_special_Xt_user_event (device, Qcall_interactively,
				    Qscroll_up);
    }
  else if (!strcmp (name, "scrollBack"))
    {
      signal_special_Xt_user_event (device, Qcall_interactively,
				    Qdown_up);
    }
  else
    {
      data = malloc_widget_value ();
      data->name = "searchText";
      if (! lw_get_some_values (id, data)) abort ();
      search = build_string (data->value);
      data->name = "replaceText";
      if (! lw_get_some_values (id, data)) abort ();
      replace = build_string (data->value);
      data->name = "regexpSearch";
      if (! lw_get_some_values (id, data)) abort ();
      regexp_p = (data->selected ? Qt : Qnil);
      data->name = "caseSearch";
      if (! lw_get_some_values (id, data)) abort ();
      case_sensitive_p = (data->selected ? Qt : Qnil);
      data->name = "matchWord";
      if (! lw_get_some_values (id, data)) abort ();
      match_word_p = (data->selected ? Qt : Qnil);
      
      data->name = "directionForward";
      if (! lw_get_some_values (id, data)) abort ();
      direction = data->selected ? Qt : Qnil;
      
      if (!strcmp (name, "search"))
	replace = Qnil;
      else if (!strcmp (name, "replace"))
	;
      else if (!strcmp (name, "replace_all"))
	{
	  replace = list1 (replace);
	  /*	hide_search_dialog (parent, id); */
	}
      else
	abort ();
      
      free_widget_value (data);
      
      signal_special_Xt_user_event (device,
				    intern ("energize-search-internal"),
				 (NILP (replace)
				  ? list5 (case_sensitive_p, match_word_p,
					   regexp_p, direction, search)
				  : list6 (case_sensitive_p, match_word_p,
					   regexp_p, direction, search,
					   replace)));
    }
}


DEFUN ("energize-search", Fenergize_search, 0, 0, "", /*
Pop up the search-and-replace dialog box.
*/
       ())
{
  int dbox_id;
  struct frame *f = selected_frame ();
  widget_value *data;
  Widget parent, dbox;
  Lisp_Object frame = Qnil;

  XSETFRAME (frame, f);
  CHECK_X_FRAME (frame);
  parent = FRAME_X_SHELL_WIDGET (f);

  data = malloc_widget_value ();

  dbox_id = (search_id ? search_id : new_lwlib_id());
  dbox = lw_create_widget ("search", "search", dbox_id, NULL, parent,
			   1, 0, search_callback, 0);
  data->enabled = 1;
  data->value = 0;

  data->name = "button1"; data->value = data->call_data = "search";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button2"; data->value = data->call_data = "replace";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button3"; data->value = data->call_data = "replace_all";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button4"; data->value = data->call_data = "done";
  lw_modify_all_widgets (dbox_id, data, True);

  data->value = 0;
  data->name = data->call_data = "gotoStart";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "gotoEnd";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "scrollBack";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "scrollForward";
  lw_modify_all_widgets (dbox_id, data, True);

  data->value = 0;
  data->name = data->call_data = "caseSearch";
  data->selected = NILP (current_buffer->case_fold_search);
  lw_modify_all_widgets (dbox_id, data, True);

  data->name = data->call_data = "directionForward";
  data->selected = 1;
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "directionBackward";
  data->selected = 0;
  lw_modify_all_widgets (dbox_id, data, True);

  free_widget_value (data);

  lw_pop_up_all_widgets (dbox_id);
  last_search_up_p = 0;
  if (search_id)
    {
      Widget w = lw_get_widget (dbox_id, parent, True);
      if (! w) abort ();
      XMapRaised (XtDisplay (w), XtWindow (w));
    }
  else
    {
      search_id = dbox_id;
    }

  return Qnil;
}



/*************** Definition of Emacs Lisp-callable functions ***************/

void
syms_of_energize (void)
{
  DEFSUBR (Fenergize_send_buffer_modified);
  DEFSUBR (Fenergize_list_menu);
  DEFSUBR (Fenergize_execute_menu_item);
  DEFSUBR (Fenergize_execute_command_internal);
  DEFSUBR (Fconnect_to_energize_internal);
  DEFSUBR (Fconnected_to_energize_p);
  DEFSUBR (Fclose_connection_to_energize);
  DEFSUBR (Fhandle_energize_request);
  DEFSUBR (Fenergize_buffer_p);
  DEFSUBR (Fenergize_buffer_type);
  DEFSUBR (Fset_energize_buffer_type_internal);
  DEFSUBR (Fenergize_buffer_id);
  DEFSUBR (Fenergize_request_kill_buffer);
  DEFSUBR (Fenergize_send_region);
  DEFSUBR (Fenergize_user_input_buffer_mark);
  DEFSUBR (Fenergize_update_menubar);
  DEFSUBR (Fenergize_extent_menu_p);
  DEFSUBR (Fenergize_query_buffer);
  DEFSUBR (Fenergize_barf_if_buffer_locked);
  DEFSUBR (Fenergize_psheets_visible_p);
  DEFSUBR (Fenergize_buffer_has_psheets_p);
  DEFSUBR (Fenergize_toggle_psheet);
  DEFSUBR (Fenergize_protocol_level);
  DEFSUBR (Fenergize_edit_mode_prompt);
  DEFSUBR (Fenergize_search);
  DEFSUBR (Fextent_to_generic_id);

  defsymbol (&Qenergize_create_buffer_hook, "energize-create-buffer-hook");
  defsymbol (&Qenergize_buffer_modified_hook, "energize-buffer-modified-hook");

  defsymbol (&Qenergize_kernel_busy, "energize-kernel-busy");

  defsymbol (&Qenergize_kernel_busy_hook, "energize-kernel-busy-hook");
  defsymbol (&Qenergize_menu_update_hook, "energize-menu-update-hook");
  defsymbol (&Qbuffer_locked_by_energize, "buffer-locked-by-energize");
  defsymbol (&Qenergize_user_input_buffer_mark,
	     "energize-user-input-buffer-mark");
  defsymbol (&Qenergize_make_many_buffers_visible,
	     "energize-make-many-buffers-visible");
  defsymbol (&Qenergize, "energize");
  defsymbol (&Qenergize_auto_revert_buffer, "energize-auto-revert-buffer");
}

void
vars_of_energize (void)
{
  energize_connection = 0;
  inside_process_energize_request_1 = 0;

  staticpro (&Venergize_buffers_list);
  Venergize_buffers_list = Qnil;

  staticpro (&Vall_energize_pixmaps);
  Vall_energize_pixmaps = Qnil;

  Fprovide (intern ("energize"));

  search_id = 0;

  DEFVAR_LISP ("energize-process", &Venergize_process /*
The Lisp object representing the Energize connection, or nil
*/ );
  Venergize_process = Qnil;

  DEFVAR_LISP ("energize-create-buffer-hook", &Venergize_create_buffer_hook /*
Hook called when buffer is created by energize; takes 
BUFFER as its only argument.
*/ );
  Venergize_create_buffer_hook = Qnil;


  DEFVAR_LISP ("energize-kernel-modification-hook",
	       &Venergize_kernel_modification_hook /*
Hook called when a buffer is being modified by energize;
takes no arguments.
*/ );
  Venergize_kernel_modification_hook = Qnil;

  DEFVAR_BOOL ("ignore-kernel",
	       &ignore_kernel /*
Set when the kernel should be ignored -- for debugging.
*/ );
  ignore_kernel = 0;

  DEFVAR_LISP ("energize-kernel-busy", &Venergize_kernel_busy /*
True if the Energize kernel is busy.
*/ );
  Venergize_kernel_busy = Qnil;
  DEFVAR_LISP ("energize-kernel-busy-hook", &Venergize_kernel_busy_hook /*
Hook called when the Energize kernel becomes busy or non busy.
*/ );
  Venergize_kernel_busy_hook = Qnil;

  DEFVAR_LISP ("energize-menu-update-hook", &Venergize_menu_update_hook /*
Hook called when the Energize kernel updates the menubar.
*/ );
  Venergize_menu_update_hook = Qnil;

  DEFVAR_LISP ("energize-attributes-mapping", &Venergize_attributes_mapping /*
A-list to map kernel attributes indexes to Emacs attributes
*/ );
  Venergize_attributes_mapping = Qnil;

  DEFVAR_INT ("energize-extent-gc-threshold", &energize_extent_gc_threshold /*
Number of  extents in a ModifyBuffer request above which to do a GC
*/ );
  energize_extent_gc_threshold = 20;
  
  pure_put (Qbuffer_locked_by_energize, Qerror_conditions,
	    list2 (Qbuffer_locked_by_energize, Qerror));
  pure_put (Qbuffer_locked_by_energize, Qerror_message,
	    build_string ("Buffer is currently locked by kernel"));
}

void
complex_vars_of_energize (void)
{
  image_cache = make_strings_hashtable (50);
}

#endif /* ENERGIZE */
