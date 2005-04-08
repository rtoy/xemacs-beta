/*
  postgresql.h -- Emacs Lisp binding to libpq.so
  Copyright (C) 2000 Electrotechnical Laboratory, JAPAN.
  Licensed to the Free Software Foundation.

  Author:  SL Baur <steve@beopen.com>
  Maintainer:  SL Baur <steve@beopen.com>

Please send patches to this file to me first before submitting them to
xemacs-patches.
*/

#ifndef INCLUDED_postgresql_h_
#define INCLUDED_postgresql_h_ 1

#define message message_ /* Yuck */
#include LIBPQ_FE_H_FILE /* main PostgreSQL header file */
#undef message

#define BLCKSZ 8192 /* size of a default Postgres disk block */
/*
  This file contains the GCC bug workaround code for the private
  LRECORD types.
*/

/* PGconn is an opaque object and we need to be able to store them in
   Lisp code because libpq supports multiple connections.
*/
struct Lisp_PGconn
{
#ifdef MC_ALLOC
  struct lrecord_header header;
#else /* not MC_ALLOC */
  struct lcrecord_header header;
#endif /* not MC_ALLOC */
  PGconn *pgconn;
};
typedef struct Lisp_PGconn Lisp_PGconn;

DECLARE_LRECORD (pgconn, Lisp_PGconn);

#define XPGCONN(x) XRECORD (x, pgconn, Lisp_PGconn)
#define wrap_pgconn(p) wrap_record (p, pgconn)
#define PGCONNP(x) RECORDP (x, pgconn)
#define CHECK_PGCONN(x) CHECK_RECORD (x, pgconn)
#define CONCHECK_PGCONN(x) CONCHECK_RECORD (x, pgconn)

/****/

/* PGresult is an opaque object and we need to be able to store them in
   Lisp code.
*/
struct Lisp_PGresult
{
#ifdef MC_ALLOC
  struct lrecord_header header;
#else /* not MC_ALLOC */
  struct lcrecord_header header;
#endif /* not MC_ALLOC */
  PGresult *pgresult;
};
typedef struct Lisp_PGresult Lisp_PGresult;

DECLARE_LRECORD (pgresult, Lisp_PGresult);

#define XPGRESULT(x) XRECORD (x, pgresult, Lisp_PGresult)
#define wrap_pgresult(p) wrap_record (p, pgresult)
#define PGRESULTP(x) RECORDP (x, pgresult)
#define CHECK_PGRESULT(x) CHECK_RECORD (x, pgresult)
#define CONCHECK_PGRESULT(x) CONCHECK_RECORD (x, pgresult)

#endif /* INCLUDED_postgresql_h_ */
