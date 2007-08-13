/* Database access routines
   Copyright (C) 1996, William M. Perry

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

/* Written by Bill Perry */

#include <config.h>
#include "lisp.h"
#include <errno.h>

#ifdef HAVE_DATABASE
#include <database.h>         /* Our include file     */
#ifdef HAVE_BERKELEY_DB
#include <db.h>                       /* Berkeley db access   */
#endif
#ifdef HAVE_DBM
#include <ndbm.h>
#endif

Lisp_Object Qdatabasep;
#ifdef HAVE_DBM
Lisp_Object Qdbm;
#endif
#ifdef HAVE_BERKELEY_DB
Lisp_Object Qberkeley_db;
Lisp_Object Qhash;
Lisp_Object Qbtree;
Lisp_Object Qrecno;
#endif

typedef enum { DB_DBM, DB_BERKELEY, DB_UNKNOWN } XEMACS_DB_TYPE;

struct database_struct;

typedef struct _DB_FUNCS
{
  CONST char * (*get_subtype) (struct database_struct *);
  CONST char * (*get_type) (struct database_struct *);
  void * (*open_file) (CONST char *, Lisp_Object, int, int);
  Lisp_Object (*get) (struct database_struct *, Lisp_Object);
  int (*put) (struct database_struct *, Lisp_Object, Lisp_Object, Lisp_Object);
  int (*rem) (struct database_struct *, Lisp_Object);
  void (*map) (struct database_struct *, Lisp_Object);
  Lisp_Object (*get_lisp_type) (struct database_struct *);
  void (*close) (struct database_struct *);
  Lisp_Object (*last_error) (struct database_struct *);
} DB_FUNCS;

struct database_struct
{
  struct lcrecord_header header;
  Lisp_Object fname;
  XEMACS_DB_TYPE type;
  int mode;
  int ackcess;
  int errno;
  void *db_handle;
  DB_FUNCS *funcs;
};

#define XDATABASE(x) XRECORD (x, database, struct database_struct)
#define XSETDATABASE(x, p) XSETRECORD (x, p, database)
#define DATABASEP(x) RECORDP (x, database)
#define GC_DATABASEP(x) GC_RECORDP (x, database)
#define CHECK_DATABASE(x) CHECK_RECORD (x, database)
#define DATABASE_LIVE_P(x) (x->db_handle)
static Lisp_Object mark_database (Lisp_Object, void (*) (Lisp_Object));
static void print_database (Lisp_Object, Lisp_Object, int);
static void finalize_database (void *, int);
DEFINE_LRECORD_IMPLEMENTATION ("database", database,
                               mark_database, print_database,
			       finalize_database, 0, 0,
			       struct database_struct);

static struct database_struct *
new_database (void)
{
  struct database_struct *dbase
    = alloc_lcrecord (sizeof (struct database_struct), lrecord_database);

  dbase->fname = Qnil;
  dbase->db_handle = NULL;
  dbase->ackcess = 0;
  dbase->mode = 0;
  dbase->errno = 0;
  dbase->type = DB_UNKNOWN;
  return (dbase);
}

static Lisp_Object
mark_database (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct database_struct *dbase = XDATABASE (obj);

  ((markobj) (dbase->fname));
  return (Qnil);
}

static void
print_database (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct database_struct *dbase = XDATABASE (obj);
  char buf[200];

  if (print_readably)
    {
      error ("printing unreadable object #<database 0x%x>", dbase->header.uid);
    }
  else
    {
      CONST char *type;
      CONST char *subtype;
      CONST char *perms;
      
      perms = (!dbase->db_handle)        ? "closed"    :
	(dbase->ackcess & O_WRONLY) ? "writeonly" :
	  (dbase->ackcess & O_RDWR)   ? "readwrite" : "readonly";
      
      type = dbase->funcs->get_type (dbase);
      subtype = dbase->funcs->get_subtype (dbase);
      
      sprintf (buf, "#<database %s (%s/%s/%s) 0x%x>",
	       string_data (XSTRING (dbase->fname)), type, subtype, perms,
	       dbase->header.uid);
      write_c_string (buf, printcharfun);
    }
}

static void
finalize_database (void *header, int for_disksave)
{
  struct database_struct *db = (struct database_struct *) header;

  if (for_disksave)
    {
      Lisp_Object obj; 
      XSETOBJ (obj, Lisp_Record, (void *) db);
      
      signal_simple_error
	("Can't dump an emacs containing window system objects", obj);
    }
  db->funcs->close (db);
}

DEFUN ("close-database", Fdatabase_close, Sdatabase_close, 1, 1, 0 /*
Close database OBJ.
*/ )
  (obj)
  Lisp_Object obj;
{
  struct database_struct *db;
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);

  if (DATABASE_LIVE_P (db))
    db->funcs->close (db);
  else
    signal_simple_error ("Attempting to access closed database", obj);

  return (Qnil);
}

DEFUN ("database-type", Fdatabase_type, Sdatabase_type, 1, 1, 0 /*
Return the type of database OBJ.
*/)
  (obj)
  Lisp_Object obj;
{
  struct database_struct *db;
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);

  return db->funcs->get_lisp_type (db);
}

DEFUN ("database-subtype", Fdatabase_subtype, Sdatabase_subtype, 1, 1, 0 /*
Return the subtype of database OBJ, if any.
*/ )
  (obj)
  Lisp_Object obj;
{
  struct database_struct *db;
  
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);
  
  return (intern (db->funcs->get_subtype (db)));
}

DEFUN ("database-live-p", Fdatabase_live_p, Sdatabase_live_p, 1, 1, 0 /*
Return t iff OBJ is an active database, else nil.
*/ )
  (obj)
  Lisp_Object (obj);
{
  struct database_struct *db;
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);

  return (DATABASE_LIVE_P (db) ? Qt : Qnil);
}

DEFUN ("database-file-name", Fdatabase_file_name, Sdatabase_file_name,
       1, 1, 0 /*
Return the filename associated with the database OBJ.
*/)
  (obj)
  Lisp_Object obj;
{
  struct database_struct *db;
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);
  return (db->fname);
}

DEFUN ("databasep", Fdatabasep, Sdatabasep, 1, 1, 0 /*
Return t iff OBJ is a database, else nil.
*/ )
  (obj)
  Lisp_Object obj;
{
  return ((DATABASEP (obj)) ? Qt : Qnil);
}

#ifdef HAVE_DBM
static void
dbm_map (struct database_struct *db, Lisp_Object func)
{
  datum keydatum, valdatum;
  DBM *handle = (DBM *)db->db_handle;
  Lisp_Object key, val;

  for (keydatum = dbm_firstkey (handle);
       keydatum.dptr != NULL;
       keydatum = dbm_nextkey (handle))
    {
      valdatum = dbm_fetch (handle, keydatum);
      key = make_string ((unsigned char *) keydatum.dptr, keydatum.dsize);
      val = make_string ((unsigned char *) valdatum.dptr, valdatum.dsize);
      call2 (func, key, val);
    }
}

static Lisp_Object
dbm_get (struct database_struct *db, Lisp_Object key)
{
  datum keydatum, valdatum;
  DBM *handle = (DBM *)db->db_handle;
  keydatum.dptr = (char *) string_data (XSTRING (key));
  keydatum.dsize = string_length (XSTRING (key));
  valdatum = dbm_fetch (handle, keydatum);

  return (valdatum.dptr
	  ? make_string ((unsigned char *) valdatum.dptr, valdatum.dsize)
	  : Qnil);
}

static int
dbm_put (struct database_struct *db,
	 Lisp_Object key,
	 Lisp_Object val,
	 Lisp_Object replace)
{
  DBM *handle = (DBM *)db->db_handle;
  datum keydatum, valdatum;
  
  valdatum.dptr = (char *) string_data (XSTRING (val));
  valdatum.dsize = string_length (XSTRING (val));
  keydatum.dptr = (char *) string_data (XSTRING (key));
  keydatum.dsize = string_length (XSTRING (key));

  return (!dbm_store (handle, keydatum, valdatum,
		      (NILP (replace)) ? DBM_INSERT : DBM_REPLACE));
}

static int
dbm_remove (struct database_struct *db, Lisp_Object key)
{
  datum keydatum;
  keydatum.dptr = (char *) string_data (XSTRING (key));
  keydatum.dsize = string_length (XSTRING (key));
  return (dbm_delete (db->db_handle, keydatum));
}

static Lisp_Object
dbm_lisp_type (struct database_struct *db)
{
  return (Qdbm);
}

static CONST char *
dbm_type (struct database_struct *db)
{
  return ("dbm");
}

static CONST char *
dbm_subtype (struct database_struct *db)
{
  return ("nil");
}

static void *
new_dbm_file (CONST char *file, Lisp_Object subtype, int ackcess, int mode)
{
  DBM *db = NULL;
  db = dbm_open ((char *) file, ackcess, mode);
  return (void *) db;
}

static Lisp_Object
dbm_lasterr (struct database_struct *dbp)
{
  char *temp = strerror (dbp->errno);
  return (make_string ((unsigned char *) temp, strlen (temp)));
}

static void
dbm_closeit (struct database_struct *db)
{
  if (db->db_handle)
    dbm_close ((DBM *)db->db_handle);
  db->db_handle = NULL;
}

static DB_FUNCS ndbm_func_block =
{
  dbm_subtype,
  dbm_type,
  new_dbm_file,
  dbm_get,
  dbm_put,
  dbm_remove,
  dbm_map,
  dbm_lisp_type,
  dbm_closeit,
  dbm_lasterr
};
#endif

#ifdef HAVE_BERKELEY_DB
static Lisp_Object
berkdb_lisp_type (struct database_struct *db)
{
  return (Qberkeley_db);
}

static CONST char *
berkdb_type (struct database_struct *db)
{
  return ("berkeley");
}

static CONST char *
berkdb_subtype (struct database_struct *db)
{
  DB *temp = (DB *)db->db_handle;

  if (!temp)
    return ("nil");
  
  switch (temp->type)
    {
    case DB_BTREE:
      return ("btree");
    case DB_HASH:
      return ("hash");
    case DB_RECNO:
      return ("recno");
    }
  return ("unknown");
}

static void *
berkdb_open (CONST char *file, Lisp_Object subtype, int ackcess, int mode)
{
  DB *db;
  DBTYPE real_subtype;

  if (EQ (subtype, Qhash) || NILP (subtype))
    real_subtype = DB_HASH;
  else if (EQ (subtype, Qbtree))
    real_subtype = DB_BTREE;
  else if (EQ (subtype, Qrecno))
    real_subtype = DB_RECNO;
  else
    signal_simple_error ("Unsupported subtype", subtype);

  db = dbopen (file, ackcess, mode, real_subtype, NULL);

  return (void *) db;
}

static Lisp_Object
berkdb_lasterr (struct database_struct *dbp)
{
  char *temp = strerror (dbp->errno);
  return (make_string ((unsigned char *) temp, strlen (temp)));
}

static Lisp_Object
berkdb_get (struct database_struct *db, Lisp_Object key)
{
  DBT keydatum, valdatum;
  DB *dbp = (DB *) db->db_handle;
  int status = 0;

  keydatum.data = string_data (XSTRING (key));
  keydatum.size = string_length (XSTRING (key));
  
  status = dbp->get (dbp, &keydatum, &valdatum, 0);

  if (!status)
    return (make_string (valdatum.data, valdatum.size));

  db->errno = (status == 1) ? -1 : errno;
  return (Qnil);
}

static int
berkdb_put (struct database_struct *db,
	    Lisp_Object key,
	    Lisp_Object val,
	    Lisp_Object replace)
{
  DBT keydatum, valdatum;
  DB *dbp = (DB *) db->db_handle;
  int status = 0;

  keydatum.data = string_data (XSTRING (key));
  keydatum.size = string_length (XSTRING (key));
  valdatum.data = string_data (XSTRING (val));
  valdatum.size = string_length (XSTRING (val));
  status = dbp->put (dbp, &keydatum, &valdatum, NILP (replace)
		     ? R_NOOVERWRITE : 0);
  db->errno = (status == 1) ? -1 : errno;
  return status;
}

static int
berkdb_remove (struct database_struct *db, Lisp_Object key)
{
  DBT keydatum;
  DB *dbp = (DB *) db->db_handle;
  int status;

  keydatum.data = string_data (XSTRING (key));
  keydatum.size = string_length (XSTRING (key));
  
  status = dbp->del (dbp, &keydatum, 0);
  if (!status)
    return 0;
  
  db->errno = (status == 1) ? -1 : errno;
  return 1;
}

static void
berkdb_map (struct database_struct *db, Lisp_Object func)
{
  DBT keydatum, valdatum;
  Lisp_Object key, val;
  DB *dbp = (DB *) db->db_handle;
  int status;

  for (status = dbp->seq (dbp, &keydatum, &valdatum, R_FIRST);
       status == 0;
       status = dbp->seq (dbp, &keydatum, &valdatum, R_NEXT))
    {
      key = make_string (keydatum.data, keydatum.size);
      val = make_string (valdatum.data, valdatum.size);
      call2 (func, key, val);
    }
}

static void
berkdb_close (struct database_struct *db)
{
  DB *dbp = (DB *)db->db_handle;
  if (dbp)
    {
      dbp->sync (dbp, 0);
      dbp->close (dbp);
    }
  db->db_handle = NULL;
}

static DB_FUNCS berk_func_block =
{
  berkdb_subtype,
  berkdb_type,
  berkdb_open,
  berkdb_get,
  berkdb_put,
  berkdb_remove,
  berkdb_map,
  berkdb_lisp_type,
  berkdb_close,
  berkdb_lasterr
};
#endif

DEFUN ("database-last-error", Fdatabase_error, Sdatabase_error, 0, 1, 0 /*
Return the last error associated with database OBJ.
*/ )
  (obj)
  Lisp_Object obj;
{
  struct database_struct *db;

  if (NILP (obj))
    {
      char *temp = strerror (errno);
      return (make_string ((unsigned char *) temp, strlen (temp)));
    }
  
  CHECK_DATABASE (obj);
  db = XDATABASE (obj);
  return (db->funcs->last_error (db));
}

DEFUN ("open-database", Fmake_database, Smake_database, 1, 5, 0 /*
Open database FILE, using database method TYPE and SUBTYPE, with
access rights ACCESS and permissions MODE.  ACCESS can be any
combination of 'r' 'w' and '+', for read, write, and creation flags.
*/ )
  (file, type, subtype, ackcess, mode)
  Lisp_Object file, type, subtype, ackcess, mode;
{
  Lisp_Object retval = Qnil;
  int modemask;
  int accessmask = 0;
  XEMACS_DB_TYPE the_type;
  DB_FUNCS *funcblock;
  struct database_struct *dbase = NULL;
  void *db = NULL;

  CHECK_STRING (file);

  if (NILP (ackcess))
    {
      accessmask = O_RDWR | O_CREAT;
    }
  else
    {
      char *acc;
      CHECK_STRING (ackcess);
      acc = (char *) string_data (XSTRING (ackcess));
      
      if (strchr (acc, '+'))
	accessmask |= O_CREAT;
      
      if (strchr (acc, 'r') && strchr (acc, 'w'))
	{
	  accessmask |= O_RDWR;
	}
      else if (strchr (acc, 'w'))
	{
	  accessmask |= O_WRONLY;
	}
      else
	{
	  accessmask |= O_RDONLY;
	}
    }

  if (NILP (mode))
    {
      modemask = 493;           /* rwxr-xr-x */
    }
  else
    {
      CHECK_INT (mode);
      modemask = XINT (mode);
    }

#ifdef HAVE_DBM
  if (NILP (type) || EQ (type, Qdbm))
    {
      the_type = DB_DBM;
      funcblock = &ndbm_func_block;
      goto db_done;
    }
#endif

#ifdef HAVE_BERKELEY_DB
  if (NILP (type) || EQ (type, Qberkeley_db))
    {

      funcblock = &berk_func_block;
      the_type = DB_BERKELEY;
      goto db_done;
    }
#endif
  
  signal_simple_error ("Unsupported database type", type);
  return (Qnil);

 db_done:
  db = funcblock->open_file ((char *) string_data (XSTRING (file)), subtype,
			     accessmask, modemask);
  
  if (!db)
    {
      return (Qnil);
    }
  
  dbase = new_database ();
  dbase->fname = file;
  dbase->type = the_type;
  dbase->mode = modemask;
  dbase->ackcess = accessmask;
  dbase->db_handle = db;
  dbase->funcs = funcblock;
  XSETDATABASE (retval, dbase);

  return (retval);
}

DEFUN ("put-database", Fputdatabase, Sputdatabase, 3, 4, 0 /*
Store KEY and VAL in DATABASE.  If optinal fourth arg REPLACE is
non-nil, replace any existing entry in the database.
*/ )
     (key, val, dbase, replace)
     Lisp_Object key, val, dbase, replace;
{
  struct database_struct *db;
  int status;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  
  CHECK_DATABASE (dbase);
  CHECK_STRING (key);
  db = XDATABASE (dbase);
  if (!DATABASE_LIVE_P (db))
    signal_simple_error ("Attempting to access closed database", dbase);

  GCPRO4 (key, val, dbase, replace);
  status = db->funcs->put (db, key, val, replace);
  UNGCPRO;
  return status ? Qt : Qnil;
}

DEFUN ("remove-database", Fremdatabase, Sremdatabase, 2, 2, 0 /*
Remove KEY from DATABASE.
*/ )
  (key, dbase)
  Lisp_Object key, dbase;
{
  struct database_struct *db;
  CHECK_DATABASE (dbase);
  CHECK_STRING (key);

  db = XDATABASE (dbase);
  if (!DATABASE_LIVE_P (db))
    signal_simple_error ("Attempting to access closed database", dbase);
  return db->funcs->rem (db, key) ? Qt : Qnil;
}
     
DEFUN ("get-database", Fgetdatabase, Sgetdatabase, 2, 3, 0 /*
Find value for KEY in DATABASE.
If there is no corresponding value, return DEFAULT (defaults to nil).
*/ )
  (key, dbase, defalt)
  Lisp_Object key, dbase, defalt; /* One can't even spell correctly in C */
{
  Lisp_Object retval;
  struct database_struct *db;

  CHECK_DATABASE (dbase);
  CHECK_STRING (key);
  db = XDATABASE (dbase);
  if (!DATABASE_LIVE_P (db))
    signal_simple_error ("Attempting to access closed database", dbase);

  retval = db->funcs->get (db, key);

  return (NILP (retval) ? defalt : retval);
}

DEFUN ("map-database", Fmapdatabase, Smapdatabase, 2, 2, 0 /*
Map FUNCTION over entries in DATABASE, calling it with two args,
each key and value in the database.
*/ )
  (function, dbase)
  Lisp_Object function, dbase;
{
  struct gcpro gcpro1, gcpro2;
  struct database_struct *db;

  CHECK_DATABASE (dbase);
  GCPRO2 (dbase, function);

  db = XDATABASE (dbase);
  if (!DATABASE_LIVE_P (db))
    signal_simple_error ("Attempting to access closed database", dbase);
  db->funcs->map (db, function);
  UNGCPRO;
  return Qnil;
}

void
syms_of_dbm (void)
{
  defsymbol (&Qdatabasep, "databasep");
#ifdef HAVE_DBM
  defsymbol (&Qdbm, "dbm");
#endif
#ifdef HAVE_BERKELEY_DB
  defsymbol (&Qberkeley_db, "berkeley-db");
  defsymbol (&Qhash, "hash");
  defsymbol (&Qbtree, "btree");
  defsymbol (&Qrecno, "recno");
#endif

  defsubr (&Smake_database);
  defsubr (&Sdatabasep);
  defsubr (&Smapdatabase);
  defsubr (&Sputdatabase);
  defsubr (&Sgetdatabase);
  defsubr (&Sremdatabase);
  defsubr (&Sdatabase_type);
  defsubr (&Sdatabase_subtype);
  defsubr (&Sdatabase_error);
  defsubr (&Sdatabase_live_p);
  defsubr (&Sdatabase_file_name);
  defsubr (&Sdatabase_close);
}

void
vars_of_dbm (void)
{
#ifdef HAVE_DBM
  Fprovide (Qdbm);
#endif
#ifdef HAVE_BERKELEY_DB
  Fprovide (Qberkeley_db);
#endif
}
#endif /* HAVE_DATABASE */
