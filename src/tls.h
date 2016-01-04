/* Transport Layer Security implementation -- header file.
   Copyright (C) 2014 Jerry James

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

/* Written by Jerry James. */

#ifndef INCLUDED_tls_h_
#define INCLUDED_tls_h_

extern Lisp_Object Qtls_error;

void syms_of_tls (void);
void vars_of_tls (void);
void init_tls (void);

#ifdef WITH_TLS

#ifdef HAVE_NSS
#include <prio.h>

#define TLS_SETUP_SOCK 0

typedef struct tls_state
{
  PRFileDesc *tls_file_desc;
  int tls_refcount;
} tls_state_t;
#endif

#ifdef HAVE_GNUTLS
#include <gnutls/gnutls.h>

#define TLS_SETUP_SOCK 1

typedef struct tls_state
{
  gnutls_session_t tls_session;
  int tls_refcount;
} tls_state_t;
#endif

#ifdef HAVE_OPENSSL
# include <openssl/ssl.h>

#define TLS_SETUP_SOCK 1

typedef struct tls_state
{
  SSL *tls_connection;
  int tls_refcount;
} tls_state_t;
#endif

tls_state_t *tls_open (int, const Extbyte *);
tls_state_t *tls_negotiate (int, const Extbyte *, Lisp_Object);
void tls_close_connection (tls_state_t *);
Lisp_Object make_tls_output_stream (tls_state_t *);
Lisp_Object make_tls_input_stream (tls_state_t *);
int tls_get_fd (tls_state_t *);
Bytecount tls_read (tls_state_t *, unsigned char *, Bytecount, unsigned int);
Bytecount tls_write (tls_state_t *, const unsigned char *, Bytecount,
		     unsigned int);
int tls_close (tls_state_t *);
#else /* WITH_TLS */
typedef int tls_state_t;
#define TLS_SETUP_SOCK 1
#define tls_open(x,y) ({						\
      signal_error (Qtls_error, "TLS support unavailable", Qnil);	\
      NULL; })
#define tls_negotiate(x,y,z) NULL
#define make_tls_input_stream(x) ({					\
      signal_error (Qtls_error, "TLS support unavailable", Qnil);	\
      NULL; })
#define make_tls_output_stream(x) ({					\
      signal_error (Qtls_error, "TLS support unavailable", Qnil);	\
      NULL; })
#define tls_get_fd(x)		-1
#define tls_read(w,x,y,z)	-1
#define tls_write(w,x,y,z)	-1
#define tls_close(x)		-1
#endif /* WITH_TLS */

#endif /* INCLUDED_tls_h_ */
