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
/* Substantially rewritten by Martin Buchholz */
/* db 2.x support added by Andreas Jaeger */

#include <config.h>
#include "lisp.h"
#include <errno.h>

#ifndef HAVE_DATABASE
#error database.c being compiled, but HAVE_DATABASE not defined!
#endif /* HAVE_DATABASE */

#include "database.h"         /* Our include file */

#ifdef HAVE_BERKELEY_DB
/* Work around Berkeley DB's use of int types which are defined
   slightly differently in the not quite yet standard <inttypes.h>.
   See db.h for details of why we're resorting to this... */
/* glibc 2.1 doesn't have this problem with DB 2.x */
#if !(defined __GLIBC__ && __GLIBC_MINOR__ >= 1)
#ifdef HAVE_INTTYPES_H
#define __BIT_TYPES_DEFINED__
#include <inttypes.h>
typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
#ifdef WE_DONT_NEED_QUADS
typedef uint64_t u_int64_t;
#endif /* WE_DONT_NEED_QUADS */
#endif /* HAVE_INTTYPES_H */
#endif /* !(defined __GLIBC__ && __GLIBC_MINOR__ >= 1) */
#include DB_H_PATH              /* Berkeley db's header file */
#ifndef DB_VERSION_MAJOR
# define DB_VERSION_MAJOR 1
#endif /* DB_VERSION_MAJOR */
Lisp_Object Qberkeley_db;
Lisp_Object Qhash;
Lisp_Object Qbtree;
Lisp_Object Qrecno;
#endif /* HAVE_BERKELEY_DB */

#ifdef HAVE_DBM
#include <ndbm.h>
Lisp_Object Qdbm;
#endif /* HAVE_DBM */

Lisp_Object Qdatabasep;

typedef enum { DB_DBM, DB_BERKELEY, DB_IS_UNKNOWN } XEMACS_DB_TYPE;

struct database;
typedef struct database database;

typedef struct
{
  CONST char * (*get_subtype) (struct database *);
  CONST char * (*get_type) (struct database *);
  Lisp_Object (*get) (struct database *, Lisp_Object);
  int (*put) (struct database *, Lisp_Object, Lisp_Object, Lisp_Object);
  int (*rem) (struct database *, Lisp_Object);
  void (*map) (struct database *, Lisp_Object);
  Lisp_Object (*get_lisp_type) (struct database *);
  void (*close) (struct database *);
  Lisp_Object (*last_error) (struct database *);
} DB_FUNCS;

struct database
{
  struct lcrecord_header header;
  Lisp_Object fname;
  XEMACS_DB_TYPE type;
  int mode;
  int access_;
  int dberrno;
  int live_p;
#ifdef HAVE_DBM
  DBM *dbm_handle;
#endif
#ifdef HAVE_BERKELEY_DB
  DB *db_handle;
#endif
  DB_FUNCS *funcs;
#ifdef MULE
  Lisp_Object coding_system;
#endif
};

#define XDATABASE(x) XRECORD (x, database, struct database)
#define XSETDATABASE(x, p) XSETRECORD (x, p, database)
#define DATABASEP(x) RECORDP (x, database)
#define GC_DATABASEP(x) GC_RECORDP (x, database)
#define CHECK_DATABASE(x) CHECK_RECORD (x, database)
#define DATABASE_LIVE_P(x) (x->live_p)
static Lisp_Object mark_database (Lisp_Object, void (*) (Lisp_Object));
static void print_database (Lisp_Object, Lisp_Object, int);
static void finalize_database (void *, int);
DEFINE_LRECORD_IMPLEMENTATION ("database", database,
                               mark_database, print_database,
			       finalize_database, 0, 0,
			       struct database);

#define CHECK_LIVE_DATABASE(db) do {					\
  CHECK_DATABASE(db);							\
  if (!DATABASE_LIVE_P (XDATABASE(db)))					\
    signal_simple_error ("Attempting to access closed database", db);	\
} while (0)


static struct database *
new_database (void)
{
  struct database *dbase =
    alloc_lcrecord_type (struct database, lrecord_database);

  dbase->fname = Qnil;
  dbase->live_p = 0;
#ifdef HAVE_BERKELEY_DB
  dbase->db_handle = NULL;
#endif
#ifdef HAVE_DBM
  dbase->dbm_handle = NULL;
#endif
  dbase->access_ = 0;
  dbase->mode = 0;
  dbase->dberrno = 0;
  dbase->type = DB_IS_UNKNOWN;
#ifdef MULE
  dbase->coding_system = Fget_coding_system (Qbinary);
#endif
  return dbase;
}

static Lisp_Object
mark_database (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct database *dbase = XDATABASE (obj);

  ((markobj) (dbase->fname));
  return Qnil;
}

static void
print_database (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct database *dbase = XDATABASE (obj);
  char buf[200];

  if (print_readably)
    {
      error ("printing unreadable object #<database 0x%x>", dbase->header.uid);
    }
  else
    {
      sprintf (buf, "#<database \"%s\" (%s/%s/%s) 0x%x>",
	       XSTRING_DATA (dbase->fname),
	       dbase->funcs->get_type (dbase),
	       dbase->funcs->get_subtype (dbase),
	       (!DATABASE_LIVE_P (dbase)    ? "closed"    :
		(dbase->access_ & O_WRONLY) ? "writeonly" :
		(dbase->access_ & O_RDWR)   ? "readwrite" : "readonly"),
	       dbase->header.uid);
      write_c_string (buf, printcharfun);
    }
}

static void
finalize_database (void *header, int for_disksave)
{
  struct database *db = (struct database *) header;

  if (for_disksave)
    {
      Lisp_Object obj;
      XSETOBJ (obj, Lisp_Type_Record, (void *) db);

      signal_simple_error
	("Can't dump an emacs containing window system objects", obj);
    }
  db->funcs->close (db);
}

DEFUN ("close-database", Fclose_database, 1, 1, 0, /*
Close database OBJ.
*/
       (obj))
{
  CHECK_LIVE_DATABASE (obj);
  XDATABASE (obj)->funcs->close (XDATABASE (obj));
  XDATABASE (obj)->live_p = 0;
  return Qnil;
}

DEFUN ("database-type", Fdatabase_type, 1, 1, 0, /*
Return the type of database OBJ.
*/
       (obj))
{
  CHECK_DATABASE (obj);

  return XDATABASE (obj)->funcs->get_lisp_type (XDATABASE (obj));
}

DEFUN ("database-subtype", Fdatabase_subtype, 1, 1, 0, /*
Return the subtype of database OBJ, if any.
*/
       (obj))
{
  CHECK_DATABASE (obj);

  return intern (XDATABASE (obj)->funcs->get_subtype (XDATABASE (obj)));
}

DEFUN ("database-live-p", Fdatabase_live_p, 1, 1, 0, /*
Return t iff OBJ is an active database, else nil.
*/
       (obj))
{
  CHECK_DATABASE (obj);

  return DATABASE_LIVE_P (XDATABASE (obj)) ? Qt : Qnil;
}

DEFUN ("database-file-name", Fdatabase_file_name, 1, 1, 0, /*
Return the filename associated with the database OBJ.
*/
       (obj))
{
  CHECK_DATABASE (obj);

  return XDATABASE (obj)->fname;
}

DEFUN ("databasep", Fdatabasep, 1, 1, 0, /*
Return t iff OBJ is a database, else nil.
*/
       (obj))
{
  return DATABASEP (obj) ? Qt : Qnil;
}

#ifdef HAVE_DBM
static void
dbm_map (struct database *db, Lisp_Object func)
{
  datum keydatum, valdatum;
  Lisp_Object key, val;

  for (keydatum = dbm_firstkey (db->dbm_handle);
       keydatum.dptr != NULL;
       keydatum = dbm_nextkey (db->dbm_handle))
    {
      valdatum = dbm_fetch (db->dbm_handle, keydatum);
      key = make_string ((unsigned char *) keydatum.dptr, keydatum.dsize);
      val = make_string ((unsigned char *) valdatum.dptr, valdatum.dsize);
      call2 (func, key, val);
    }
}

static Lisp_Object
dbm_get (struct database *db, Lisp_Object key)
{
  datum keydatum, valdatum;

  keydatum.dptr = (char *) XSTRING_DATA (key);
  keydatum.dsize = XSTRING_LENGTH (key);
  valdatum = dbm_fetch (db->dbm_handle, keydatum);

  return (valdatum.dptr
	  ? make_string ((unsigned char *) valdatum.dptr, valdatum.dsize)
	  : Qnil);
}

static int
dbm_put (struct database *db,
	 Lisp_Object key, Lisp_Object val, Lisp_Object replace)
{
  datum keydatum, valdatum;

  valdatum.dptr = (char *) XSTRING_DATA (val);
  valdatum.dsize = XSTRING_LENGTH (val);
  keydatum.dptr = (char *) XSTRING_DATA (key);
  keydatum.dsize = XSTRING_LENGTH (key);

  return !dbm_store (db->dbm_handle, keydatum, valdatum,
		     NILP (replace) ? DBM_INSERT : DBM_REPLACE);
}

static int
dbm_remove (struct database *db, Lisp_Object key)
{
  datum keydatum;

  keydatum.dptr = (char *) XSTRING_DATA (key);
  keydatum.dsize = XSTRING_LENGTH (key);

  return dbm_delete (db->dbm_handle, keydatum);
}

static Lisp_Object
dbm_lisp_type (struct database *db)
{
  return Qdbm;
}

static CONST char *
dbm_type (struct database *db)
{
  return "dbm";
}

static CONST char *
dbm_subtype (struct database *db)
{
  return "nil";
}

static Lisp_Object
dbm_lasterr (struct database *dbp)
{
  return lisp_strerror (dbp->dberrno);
}

static void
dbm_closeit (struct database *db)
{
  if (db->dbm_handle)
    {
      dbm_close (db->dbm_handle);
      db->dbm_handle = NULL;
    }
}

static DB_FUNCS ndbm_func_block =
{
  dbm_subtype,
  dbm_type,
  dbm_get,
  dbm_put,
  dbm_remove,
  dbm_map,
  dbm_lisp_type,
  dbm_closeit,
  dbm_lasterr
};
#endif /* HAVE_DBM */

#ifdef HAVE_BERKELEY_DB
static Lisp_Object
berkdb_lisp_type (struct database *db)
{
  return Qberkeley_db;
}

static CONST char *
berkdb_type (struct database *db)
{
  return "berkeley";
}

static CONST char *
berkdb_subtype (struct database *db)
{
  if (!db->db_handle)
    return "nil";

  switch (db->db_handle->type)
    {
    case DB_BTREE: return "btree";
    case DB_HASH:  return "hash";
    case DB_RECNO: return "recno";
    default:       return "unknown";
    }
}

static Lisp_Object
berkdb_lasterr (struct database *dbp)
{
  return lisp_strerror (dbp->dberrno);
}

static Lisp_Object
berkdb_get (struct database *db, Lisp_Object key)
{
  /* #### Needs mule-izing */
  DBT keydatum, valdatum;
  int status = 0;

#if DB_VERSION_MAJOR == 2
  /* Always initialize keydatum, valdatum. */
  memset(&keydatum, 0, sizeof(keydatum));
  memset(&valdatum, 0, sizeof(valdatum));
#endif /* DV_VERSION_MAJOR = 2 */

  keydatum.data = XSTRING_DATA (key);
  keydatum.size = XSTRING_LENGTH (key);

#if DB_VERSION_MAJOR == 1
  status = db->db_handle->get (db->db_handle, &keydatum, &valdatum, 0);
#else
  status = db->db_handle->get (db->db_handle, NULL, &keydatum, &valdatum, 0);
#endif /* DB_VERSION_MAJOR */

  if (!status)
    return make_string ((Bufbyte *) valdatum.data, valdatum.size);

#if DB_VERSION_MAJOR == 1
  db->dberrno = (status == 1) ? -1 : errno;
#else
  db->dberrno = (status < 0) ? -1 : errno;
#endif /* DB_VERSION_MAJOR */

  return Qnil;
}

static int
berkdb_put (struct database *db,
	    Lisp_Object key,
	    Lisp_Object val,
	    Lisp_Object replace)
{
  DBT keydatum, valdatum;
  int status = 0;

#if DB_VERSION_MAJOR == 2
  /* Always initalize keydatum, valdatum. */
  memset(&keydatum, 0, sizeof(keydatum));
  memset(&valdatum, 0, sizeof(valdatum));
#endif /* DV_VERSION_MAJOR = 2 */

  keydatum.data = XSTRING_DATA   (key);
  keydatum.size = XSTRING_LENGTH (key);
  valdatum.data = XSTRING_DATA   (val);
  valdatum.size = XSTRING_LENGTH (val);
#if DB_VERSION_MAJOR == 1
  status = db->db_handle->put (db->db_handle, &keydatum, &valdatum,
 			       NILP (replace) ? R_NOOVERWRITE : 0);
  db->dberrno = (status == 1) ? -1 : errno;
#else
  status = db->db_handle->put (db->db_handle, NULL, &keydatum, &valdatum,
			       NILP (replace) ? DB_NOOVERWRITE : 0);
  db->dberrno = (status < 0) ? -1 : errno;
#endif/* DV_VERSION_MAJOR = 2 */

  return status;
}

static int
berkdb_remove (struct database *db, Lisp_Object key)
{
  DBT keydatum;
  int status;

#if DB_VERSION_MAJOR == 2
  /* Always initialize keydatum. */
  memset(&keydatum, 0, sizeof(keydatum));
#endif /* DV_VERSION_MAJOR = 2 */

  keydatum.data = XSTRING_DATA   (key);
  keydatum.size = XSTRING_LENGTH (key);

#if DB_VERSION_MAJOR == 1
  status = db->db_handle->del (db->db_handle, &keydatum, 0);
#else
  status = db->db_handle->del (db->db_handle, NULL, &keydatum, 0);
#endif /* DB_VERSION_MAJOR */

  if (!status)
    return 0;

#if DB_VERSION_MAJOR == 1
  db->dberrno = (status == 1) ? -1 : errno;
#else
  db->dberrno = (status < 0) ? -1 : errno;
#endif /* DB_VERSION_MAJOR */

  return 1;
}

static void
berkdb_map (struct database *db, Lisp_Object func)
{
  DBT keydatum, valdatum;
  Lisp_Object key, val;
  DB *dbp = db->db_handle;
  int status;

#if DB_VERSION_MAJOR == 1
  for (status = dbp->seq (dbp, &keydatum, &valdatum, R_FIRST);
       status == 0;
       status = dbp->seq (dbp, &keydatum, &valdatum, R_NEXT))
    {
      /* ### Needs mule-izing */
      key = make_string ((Bufbyte *) keydatum.data, keydatum.size);
      val = make_string ((Bufbyte *) valdatum.data, valdatum.size);
      call2 (func, key, val);
    }
#else
  DBC *dbcp;
  /* Initialize the key/data pair so the flags aren't set. */
  memset(&keydatum, 0, sizeof(keydatum));
  memset(&valdatum, 0, sizeof(valdatum));

  status = dbp->cursor (dbp, NULL, &dbcp);
  for (status = dbcp->c_get (dbcp, &keydatum, &valdatum, DB_FIRST);
       status == 0;
       status = dbcp->c_get (dbcp, &keydatum, &valdatum, DB_NEXT))
    {
      /* ### Needs mule-izing */
      key = make_string ((Bufbyte *) keydatum.data, keydatum.size);
      val = make_string ((Bufbyte *) valdatum.data, valdatum.size);
      call2 (func, key, val);
    }
  dbcp->c_close (dbcp);
#endif /* DB_VERSION_MAJOR */
}

static void
berkdb_close (struct database *db)
{
  if (db->db_handle)
    {
#if DB_VERSION_MAJOR == 1
      db->db_handle->sync  (db->db_handle, 0);
      db->db_handle->close (db->db_handle);
#else
      db->db_handle->sync  (db->db_handle, 0);
      db->db_handle->close (db->db_handle, 0);
#endif /* DB_VERSION_MAJOR */
      db->db_handle = NULL;
    }
}

static DB_FUNCS berk_func_block =
{
  berkdb_subtype,
  berkdb_type,
  berkdb_get,
  berkdb_put,
  berkdb_remove,
  berkdb_map,
  berkdb_lisp_type,
  berkdb_close,
  berkdb_lasterr
};
#endif /* HAVE_BERKELEY_DB */

DEFUN ("database-last-error", Fdatabase_last_error, 0, 1, 0, /*
Return the last error associated with database OBJ.
*/
       (obj))
{
  if (NILP (obj))
    return lisp_strerror (errno);

  CHECK_DATABASE (obj);

  return XDATABASE (obj)->funcs->last_error (XDATABASE (obj));
}

DEFUN ("open-database", Fopen_database, 1, 5, 0, /*
Open database FILE, using database method TYPE and SUBTYPE, with
access rights ACCESS and permissions MODE.  ACCESS can be any
combination of 'r' 'w' and '+', for read, write, and creation flags.
*/
       (file, type, subtype, access_, mode))
{
  /* This function can GC */
  Lisp_Object retval = Qnil;
  int modemask;
  int accessmask = 0;
  struct database *dbase = NULL;
  char *filename;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (file);
  GCPRO2 (file, access_);
  file = Fexpand_file_name (file, Qnil);
  UNGCPRO;
  filename = (char *) XSTRING_DATA (file);

  if (NILP (access_))
    {
      accessmask = O_RDWR | O_CREAT;
    }
  else
    {
      char *acc;
      CHECK_STRING (access_);
      acc = (char *) XSTRING_DATA (access_);

      if (strchr (acc, '+'))
	accessmask |= O_CREAT;

      if (strchr (acc, 'r') && strchr (acc, 'w'))
	  accessmask |= O_RDWR;
      else if (strchr (acc, 'w'))
	  accessmask |= O_WRONLY;
      else
	  accessmask |= O_RDONLY;
    }

  if (NILP (mode))
    {
      modemask = 0755;		/* rwxr-xr-x */
    }
  else
    {
      CHECK_INT (mode);
      modemask = XINT (mode);
    }

#ifdef HAVE_DBM
  if (NILP (type) || EQ (type, Qdbm))
    {
      DBM *dbm = dbm_open (filename, accessmask, modemask);
      if (!dbm)
	return Qnil;

      dbase = new_database ();
      dbase->dbm_handle = dbm;
      dbase->type = DB_DBM;
      dbase->funcs = &ndbm_func_block;
      goto db_done;
    }
#endif /* HAVE_DBM */

#ifdef HAVE_BERKELEY_DB
  if (NILP (type) || EQ (type, Qberkeley_db))
    {
      DBTYPE real_subtype;
      DB *db;
      int status;
      
      if (EQ (subtype, Qhash) || NILP (subtype))
	real_subtype = DB_HASH;
      else if (EQ (subtype, Qbtree))
	real_subtype = DB_BTREE;
      else if (EQ (subtype, Qrecno))
	real_subtype = DB_RECNO;
      else
	signal_simple_error ("Unsupported subtype", subtype);

#if DB_VERSION_MAJOR == 1
      db = dbopen (filename, accessmask, modemask, real_subtype, NULL);
      if (!db)
	return Qnil;
#else
      /* Berkeley DB Version 2 has only the accessmask DB_CREATE and DB_RDONLY,
	 other flags shouldn't be set */
      if (NILP (access_))
	{
	  accessmask = DB_CREATE;
	}
      else
	{
	  char *acc;
	  CHECK_STRING (access_);
	  acc = (char *) XSTRING_DATA (access_);
	  accessmask = 0;
	  
	  if (strchr (acc, '+'))
	    accessmask |= DB_CREATE;

	  if (strchr (acc, 'r') && !strchr (acc, 'w'))
	    accessmask |= DB_RDONLY;
	}
      status = db_open (filename, real_subtype, accessmask, modemask, NULL , NULL, &db);
      if (status)
	return Qnil;
#endif /* DB_VERSION_MAJOR */
   
      dbase = new_database ();
      dbase->db_handle = db;
      dbase->type = DB_BERKELEY;
      dbase->funcs = &berk_func_block;
      goto db_done;
    }
#endif /* HAVE_BERKELEY_DB */

  signal_simple_error ("Unsupported database type", type);
  return Qnil;

 db_done:
  dbase->live_p = 1;
  dbase->fname = file;
  dbase->mode = modemask;
  dbase->access_ = accessmask;
  XSETDATABASE (retval, dbase);

  return retval;
}

DEFUN ("put-database", Fput_database, 3, 4, 0, /*
Store KEY and VAL in DATABASE.  If optional fourth arg REPLACE is
non-nil, replace any existing entry in the database.
*/
       (key, val, dbase, replace))
{
  CHECK_LIVE_DATABASE (dbase);
  CHECK_STRING (key);
  CHECK_STRING (val);

  {
    int status =
      XDATABASE (dbase)->funcs->put (XDATABASE (dbase), key, val, replace);
    return status ? Qt : Qnil;
  }
}

DEFUN ("remove-database", Fremove_database, 2, 2, 0, /*
Remove KEY from DATABASE.
*/
       (key, dbase))
{
  CHECK_LIVE_DATABASE (dbase);
  CHECK_STRING (key);

  return XDATABASE (dbase)->funcs->rem (XDATABASE (dbase), key) ? Qt : Qnil;
}

DEFUN ("get-database", Fget_database, 2, 3, 0, /*
Find value for KEY in DATABASE.
If there is no corresponding value, return DEFAULT (defaults to nil).
*/
       (key, dbase, default_))
{

  CHECK_LIVE_DATABASE (dbase);
  CHECK_STRING (key);

  {
    Lisp_Object retval =
      XDATABASE (dbase)->funcs->get (XDATABASE (dbase), key);
    return NILP (retval) ? default_ : retval;
  }
}

DEFUN ("map-database", Fmapdatabase, 2, 2, 0, /*
Map FUNCTION over entries in DATABASE, calling it with two args,
each key and value in the database.
*/
       (function, dbase))
{
  CHECK_LIVE_DATABASE (dbase);

  XDATABASE (dbase)->funcs->map (XDATABASE (dbase), function);

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

  DEFSUBR (Fopen_database);
  DEFSUBR (Fdatabasep);
  DEFSUBR (Fmapdatabase);
  DEFSUBR (Fput_database);
  DEFSUBR (Fget_database);
  DEFSUBR (Fremove_database);
  DEFSUBR (Fdatabase_type);
  DEFSUBR (Fdatabase_subtype);
  DEFSUBR (Fdatabase_last_error);
  DEFSUBR (Fdatabase_live_p);
  DEFSUBR (Fdatabase_file_name);
  DEFSUBR (Fclose_database);
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
