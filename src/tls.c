/* Transport Layer Security implementation.
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

#include <config.h>
#include "lisp.h"
#include "lstream.h"
#include "tls.h"
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

static Lisp_Object prompt;
static Lisp_Object Qread_password;
Lisp_Object Qtls_error;

#ifdef HAVE_NSS
#include <prinit.h>
#include <private/pprio.h>
#include <nss.h>
#include <pk11pub.h>
#include <secerr.h>
#include <secmod.h>
#include <ssl.h>

#define NSS_ERRSTR build_extstring (PR_ErrorToName (PR_GetError ()), Qnative)

/* 0 == initialization of NSPR or NSS failed
 * 1 == the NSPR and NSS libraries have been initialized successfully
 */
static int nss_inited;

/* The model file descriptor */
static PRFileDesc *nss_model;

/* The PEM module */
static SECMODModule *nss_pem_module;

/* CA and trust objects go into slot 0.  User certificates start in slot 1. */
static CK_SLOT_ID nss_slot_count = 1;

int
tls_get_fd (tls_state_t *state)
{
  return PR_FileDesc2NativeHandle (state->tls_file_desc);
}

Bytecount
tls_read (tls_state_t *state, unsigned char *data, Bytecount size,
	  unsigned int allow_quit)
{
  if (allow_quit)
    QUIT;
  return (Bytecount) PR_Recv (state->tls_file_desc, data, size, 0, 0);
}

Bytecount
tls_write (tls_state_t *state, const unsigned char *data, Bytecount size,
	   unsigned int allow_quit)
{
  if (allow_quit)
    QUIT;
  return (Bytecount) PR_Send (state->tls_file_desc, data, size, 0, 0);
}

int
tls_close (tls_state_t *state)
{
  if (--state->tls_refcount == 0)
    {
      PRStatus status = PR_Shutdown (state->tls_file_desc, PR_SHUTDOWN_BOTH);
      PR_Close (state->tls_file_desc);
      xfree (state);
      return (int) status;
     }
  return 0;
}

tls_state_t *
tls_open (int s, const Extbyte *hostname)
{
  struct sockaddr *addr;
  socklen_t addrlen;
  PRNetAddr pr_addr;
  tls_state_t *nspr;
  const int val = 1;

  /* Disable Nagle's algorithm */
  setsockopt (s, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));

  if (!nss_inited)
    {
      warn_when_safe (Qtls_error, Qerror, "Cannot use NSS functions");
      return NULL;
    }

  /* Get the socket address */
  addrlen = 256;
  addr = (struct sockaddr *) xmalloc (addrlen);
  if (getsockname (s, addr, &addrlen) == 0 && addrlen > 256)
    {
      addr = (struct sockaddr *) xrealloc (addr, addrlen);
      getsockname (s, addr, &addrlen);
    }

  /* Create the socket */
  nspr = xmalloc (sizeof (*nspr));
  nspr->tls_refcount = 2;
  nspr->tls_file_desc =
    SSL_ImportFD (nss_model, PR_OpenTCPSocket (addr->sa_family));
  if (nspr->tls_file_desc == NULL)
    {
      xfree (addr);
      xfree (nspr);
      warn_when_safe (Qtls_error, Qerror, "NSS unable to open socket: %s",
		      PR_ErrorToName (PR_GetError ()));
      return NULL;
    }

  /* Connect to the server */
  memset (&pr_addr, 0, sizeof (pr_addr));
  if (addr->sa_family == AF_INET)
    {
      struct sockaddr_in *in_addr = (struct sockaddr_in *) addr;
      pr_addr.inet.family = in_addr->sin_family;
      pr_addr.inet.port = in_addr->sin_port;
      pr_addr.inet.ip = in_addr->sin_addr.s_addr;
    }
  else
    {
      struct sockaddr_in6 *in_addr = (struct sockaddr_in6 *) addr;
      pr_addr.ipv6.family = in_addr->sin6_family;
      pr_addr.ipv6.port = in_addr->sin6_port;
      pr_addr.ipv6.flowinfo = in_addr->sin6_flowinfo;
      memcpy (pr_addr.ipv6.ip.pr_s6_addr, in_addr->sin6_addr.s6_addr,
	      sizeof (pr_addr.ipv6.ip.pr_s6_addr));
      pr_addr.ipv6.scope_id = in_addr->sin6_scope_id;
    }
  xfree (addr);
  if (PR_Connect (nspr->tls_file_desc, &pr_addr, PR_INTERVAL_NO_TIMEOUT)
		 != PR_SUCCESS)
    {
      if (PR_GetError () == PR_IN_PROGRESS_ERROR)
	{
	  PRPollDesc pollset[2];

	  pollset[0].in_flags = PR_POLL_WRITE | PR_POLL_EXCEPT;
	  pollset[0].out_flags = 0;
	  pollset[0].fd = nspr->tls_file_desc;
	  for (;;)
	    {
	      PRInt32 num_fds = PR_Poll (pollset, 1, PR_INTERVAL_NO_TIMEOUT);
	      if (num_fds < 0)
		{
		  PR_Close (nspr->tls_file_desc);
		  xfree (nspr);
		  warn_when_safe (Qtls_error, Qerror,
				  "NSS unable to connect: %s",
				  PR_ErrorToName (PR_GetError ()));
		  return NULL;
		}
	      if (PR_GetConnectStatus (pollset) == PR_SUCCESS)
		break;
	    }
	}
      else
	{
	  PR_Close (nspr->tls_file_desc);
	  xfree (nspr);
	  warn_when_safe (Qtls_error, Qerror, "NSS unable to connect: %s",
			  PR_ErrorToName (PR_GetError ()));
	  return NULL;
	}
    }

  /* Perform the handshake */
  if (SSL_ResetHandshake (nspr->tls_file_desc, PR_FALSE) != SECSuccess)
    {
      PR_Close (nspr->tls_file_desc);
      xfree (nspr);
      warn_when_safe (Qtls_error, Qerror, "NSS unable to reset handshake: %s",
		      PR_ErrorToName (PR_GetError ()));
      errno = EACCES;
      return NULL;
    }
  if (hostname != NULL &&
      SSL_SetURL (nspr->tls_file_desc, hostname) != SECSuccess)
    {
      PR_Close (nspr->tls_file_desc);
      xfree (nspr);
      warn_when_safe (Qtls_error, Qerror, "NSS unable to set URL (%s): %s",
		      hostname, PR_ErrorToName (PR_GetError ()));
      errno = EACCES;
      return NULL;
    }
  if (SSL_ForceHandshake (nspr->tls_file_desc) != SECSuccess)
    {
      PR_Close (nspr->tls_file_desc);
      xfree (nspr);
      warn_when_safe (Qtls_error, Qerror,
		      "NSS unable to complete handshake: %s",
		      PR_ErrorToName (PR_GetError ()));
      errno = EACCES;
      return NULL;
    }
  return nspr;
}

/* Set the key and certificate files to use */
static void
tls_set_x509_key_file (const Extbyte *certfile, const Extbyte *keyfile)
{
  char name[32];
  void *proto_win = NULL;
  PK11SlotInfo *slot = NULL;
  PK11GenericObject *obj;
  CERTCertificate *cert;
  CK_ATTRIBUTE attrs[4];
  CK_BBOOL cktrue = CK_TRUE, ckfalse = CK_FALSE;
  CK_OBJECT_CLASS objClass = CKO_PRIVATE_KEY;
  CK_SLOT_ID slot_id = nss_slot_count++;
  int retry_count = 0;

  /* Load the PEM module if it hasn't already been loaded */
  if (nss_pem_module == NULL)
    {
      nss_pem_module = SECMOD_LoadUserModule ("library=%s name=PEM parameters=\"\"", NULL, PR_FALSE);
      if (nss_pem_module == NULL)
	signal_error (Qtls_error, "Cannot find NSS PEM module", NSS_ERRSTR);
      if (!nss_pem_module->loaded)
	signal_error (Qtls_error, "Cannot load NSS PEM module", NSS_ERRSTR);
    }

  snprintf (name, 32U, "PEM_Token %ld", slot_id);
  slot = PK11_FindSlotByName (name);
  if (slot == NULL)
    signal_error (Qtls_error, "Error finding NSS slot", NSS_ERRSTR);

  /* Set up the attributes for the keyfile */
  attrs[0].type = CKA_CLASS;
  attrs[0].pValue = &objClass;
  attrs[0].ulValueLen = sizeof (objClass);
  attrs[1].type = CKA_TOKEN;
  attrs[1].pValue = &cktrue;
  attrs[1].ulValueLen = sizeof (CK_BBOOL);
  attrs[2].type = CKA_LABEL;
  attrs[2].pValue = (void *) keyfile;
  attrs[2].ulValueLen = strlen (keyfile) + 1U;

  /* When adding an encrypted key, the PKCS#11 will be set as removed. */
  obj = PK11_CreateGenericObject (slot, attrs, 3, PR_FALSE);
  if (obj == NULL)
    {
      PR_SetError (SEC_ERROR_BAD_KEY, 0);
      signal_error (Qtls_error, "Bad key file", NSS_ERRSTR);
    }

  /* This will force the token to be seen as reinserted */
  SECMOD_WaitForAnyTokenEvent (nss_pem_module, 0, 0);
  PK11_IsPresent (slot);

  if (PK11_Authenticate (slot, PR_TRUE, &retry_count) != SECSuccess)
    signal_error (Qtls_error, "NSS: Unable to authenticate", NSS_ERRSTR);

  /* Set up the attributes for the certfile */
  objClass = CKO_CERTIFICATE;
  attrs[2].pValue = (void *) certfile;
  attrs[2].ulValueLen = strlen (certfile) + 1U;
  attrs[3].type = CKA_TRUST;
  attrs[3].pValue = &ckfalse;
  attrs[3].ulValueLen = sizeof (CK_BBOOL);

  obj = PK11_CreateGenericObject (slot, attrs, 4, PR_FALSE);
  PK11_FreeSlot (slot);
  if (obj == NULL)
    signal_error (Qtls_error, "Bad certificate file", NSS_ERRSTR);
  cert = PK11_FindCertFromNickname (name, proto_win);
  if (cert == NULL)
    signal_error (Qtls_error, "Cannot find certificate nickname", NSS_ERRSTR);
  CERT_DestroyCertificate (cert);
}

/* Function that gathers passwords for PKCS #11 tokens. */
static char *
nss_pk11_password (PK11SlotInfo *slot, PRBool retry, void * UNUSED (arg))
{
  Lisp_Object lsp_password, args[2];
  Extbyte *c_password, *nss_password;
  const Extbyte *token_name;

  if (retry)
    return NULL;

  token_name = PK11_GetTokenName (slot);
  if (token_name == NULL)
    token_name = "security token";
  lsp_password =
    call1 (Qread_password, concat2 (prompt,
				    build_extstring (token_name, Qnative)));
  c_password = LISP_STRING_TO_EXTERNAL (lsp_password, Qnative);
  nss_password = PL_strdup (c_password);

  /* Wipe out the password on the stack and in the Lisp string */
  args[0] = lsp_password;
  args[1] = make_char ('*');
  Ffill (2, args);
  memset (c_password, '*', strlen (c_password));
  return nss_password;
}

void
init_tls (void)
{
  SECMODModule *module;

  /* Check that we are using compatible versions */
  if (PR_VersionCheck(PR_VERSION) == PR_FALSE)
    signal_error (Qinternal_error,
		  "NSPR version mismatch: expected " PR_VERSION, Qnil);
  if (NSS_VersionCheck(NSS_VERSION) == PR_FALSE)
    signal_error (Qinternal_error,
		  "NSS version mismatch: expected " NSS_VERSION, Qnil);

  /* Basic initialization of both libraries */
  PR_Init (PR_USER_THREAD, PR_PRIORITY_NORMAL, 0);
  if (NSS_Init ("sql:/etc/pki/nssdb") != SECSuccess)
    signal_error (Qtls_error, "Error initializing NSS", NSS_ERRSTR);

  /* Set the cipher suite policy */
  if (NSS_SetDomesticPolicy() != SECSuccess)
    signal_error (Qtls_error, "NSS unable to set policy", NSS_ERRSTR);

  /* Load the root certificates */
  module = SECMOD_LoadUserModule ("library=libnssckbi.so name=\"Root Certs\"",
				  NULL, PR_FALSE);
  if (module == NULL || !module->loaded)
    signal_error (Qtls_error, "NSS unable to load root certificates",
		  NSS_ERRSTR);

  /* Setup password gathering */
  PK11_SetPasswordFunc (nss_pk11_password);

  /* Create the model file descriptors */
  nss_model = SSL_ImportFD (NULL, PR_OpenTCPSocket (PR_AF_INET));
  if (nss_model == NULL)
    {
      nss_model = SSL_ImportFD (NULL, PR_OpenTCPSocket (PR_AF_INET6));
      if (nss_model == NULL)
	signal_error (Qtls_error, "NSS cannot create model socket",
		      NSS_ERRSTR);
    }

  /* Set options on the model socket */
  if (SSL_OptionSet (nss_model, SSL_SECURITY, PR_TRUE) != SECSuccess)
    signal_error (Qtls_error, "NSS cannot enable model socket", NSS_ERRSTR);
  if (SSL_OptionSet (nss_model, SSL_ENABLE_SSL2, PR_FALSE) != SECSuccess)
    signal_error (Qtls_error, "NSS unable to disable SSLv2", NSS_ERRSTR);
  if (SSL_OptionSet (nss_model, SSL_V2_COMPATIBLE_HELLO, PR_FALSE)
      != SECSuccess)
    signal_error (Qtls_error, "NSS unable to disable SSLv2 handshake",
		  NSS_ERRSTR);
  if (SSL_OptionSet (nss_model, SSL_ENABLE_DEFLATE, PR_FALSE) != SECSuccess)
    signal_error (Qtls_error, "NSS unable to disable deflate", NSS_ERRSTR);
  if (SSL_OptionSet (nss_model, SSL_HANDSHAKE_AS_CLIENT, PR_TRUE)
      != SECSuccess)
    signal_error (Qtls_error, "NSS unable to ensable handshake as client",
		  NSS_ERRSTR);

  nss_inited = 1;
}
#endif /* HAVE_NSS */

#ifdef HAVE_GNUTLS
#include <gnutls/pkcs11.h>
#include <gnutls/x509.h>
#include "sysfile.h"

#define GNUTLS_ERRSTR(err) build_extstring (gnutls_strerror (err), Qnative)

/* The global credentials object */
static gnutls_certificate_credentials_t global_cred;

int
tls_get_fd (tls_state_t *state)
{
  return (int)(unsigned long)gnutls_transport_get_ptr (state->tls_session);
}

Bytecount
tls_read (tls_state_t *state, unsigned char *data, Bytecount size,
	  unsigned int allow_quit)
{
  ssize_t bytes;

 again:
  do
    {
      if (allow_quit)
	QUIT;
      bytes = gnutls_record_recv (state->tls_session, data, size);
    }
  while (bytes == GNUTLS_E_INTERRUPTED || bytes == GNUTLS_E_AGAIN);
  switch (bytes)
    {
    case GNUTLS_E_UNEXPECTED_PACKET_LENGTH:
      bytes = 0;
      break;
    case GNUTLS_E_REHANDSHAKE:
      {
	int err;

	do
	  err = gnutls_handshake (state->tls_session);
	while (err == GNUTLS_E_AGAIN || err == GNUTLS_E_INTERRUPTED);
	if (err == GNUTLS_E_SUCCESS)
	  goto again;
      }
      errno = EACCES;
      bytes = -1;
      break;
    default:
      if (bytes < 0 && errno == 0)
	{
	  errno = EPIPE;
	  bytes = -1;
	}
      break;
    }
  return (Bytecount) bytes;
}

Bytecount
tls_write (tls_state_t *state, const unsigned char *data, Bytecount size,
	   unsigned int allow_quit)
{
  ssize_t bytes;

  do
    {
      if (allow_quit)
	QUIT;
      bytes = gnutls_record_send (state->tls_session, data, size);
    }
  while (bytes == GNUTLS_E_INTERRUPTED || bytes == GNUTLS_E_AGAIN);
  if (bytes == GNUTLS_E_LARGE_PACKET)
    {
      errno = EMSGSIZE;
      bytes = -1;
    }
  else if (bytes < 0 && errno == 0)
    {
      errno = EPIPE;
      bytes = -1;
    }
  return (Bytecount) bytes;
}

int
tls_close (tls_state_t *state)
{
  if (--state->tls_refcount == 0)
    {
      int fd, err;

      fd = (int)(unsigned long)gnutls_transport_get_ptr (state->tls_session);
      gnutls_bye (state->tls_session, GNUTLS_SHUT_RDWR);
      err = retry_close (fd);
      gnutls_deinit (state->tls_session);
      xfree (state);
      return err;
     }
  return 0;
}

tls_state_t *
tls_open (int s, const Extbyte *hostname)
{
#ifndef HAVE_GNUTLS_CERTIFICATE_VERIFY_PEERS3
  gnutls_x509_crt_t cert;
#endif
  tls_state_t *gnutls;
  const char *errptr = NULL;
  const gnutls_datum_t *certs;
  unsigned int status, certslen = 0U;
  int err;
  const int val = 1;

  /* Disable Nagle's algorithm */
  setsockopt (s, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));

  /* Create the state object */
  gnutls = xmalloc (sizeof (*gnutls));
  gnutls->tls_refcount = 2;

  /* Initialize the session object */
  err = gnutls_init (&gnutls->tls_session, GNUTLS_CLIENT);
  if (err != GNUTLS_E_SUCCESS)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror, "GNUTLS error in gnutls_init: %s",
		      gnutls_strerror (err));
      errno = EACCES;
      return NULL;
    }

  /* Configure the cipher preferences */
  err = gnutls_priority_set_direct (gnutls->tls_session, "NORMAL", &errptr);
  if (err != GNUTLS_E_SUCCESS)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror,
		      "GNUTLS error in gnutls_priority_set_direct: %s at %s",
		      gnutls_strerror (err), errptr);
      errno = EACCES;
      return NULL;
    }

  /* Install the trusted certificates */
  err = gnutls_credentials_set (gnutls->tls_session, GNUTLS_CRD_CERTIFICATE,
				global_cred);
  if (err != GNUTLS_E_SUCCESS)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror,
		      "GNUTLS error in gnutls_credentials_set: %s",
		      gnutls_strerror (err));
      errno = EACCES;
      return NULL;
    }

  /* Associate the socket with the session object */
  gnutls_transport_set_ptr (gnutls->tls_session,
			    (gnutls_transport_ptr_t)(unsigned long)s);

  /* Set the server name */
  if (hostname != NULL)
    {
      err = gnutls_server_name_set (gnutls->tls_session, GNUTLS_NAME_DNS,
				    hostname, strlen (hostname));
      if (err != GNUTLS_E_SUCCESS)
	{
	  xfree (gnutls);
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS error in gnutls_server_name_set: %s",
			  gnutls_strerror (err));
	  errno = EACCES;
	  return NULL;
	}
    }

  /* Perform the handshake */
  do
    err = gnutls_handshake (gnutls->tls_session);
  while (err == GNUTLS_E_AGAIN || err == GNUTLS_E_INTERRUPTED);
  if (err != GNUTLS_E_SUCCESS)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror,
		      "GNUTLS error in gnutls_handshake: %s",
		      gnutls_strerror (err));
      errno = EACCES;
      return NULL;
    }

  /* Get the server certificate chain */
  certs = gnutls_certificate_get_peers (gnutls->tls_session, &certslen);
  if (certs == NULL || certslen == 0U)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror,
		      "GNUTLS could not get peer certificate: %s",
		      gnutls_strerror (err));
      errno = EACCES;
      return NULL;
    }

  /* Validate the server certificate chain */
  status = (unsigned int) -1;
#ifdef HAVE_GNUTLS_CERTIFICATE_VERIFY_PEERS3
  if (hostname != NULL)
    err = gnutls_certificate_verify_peers3 (gnutls->tls_session, hostname,
					    &status);
  else
#endif /* HAVE_GNUTLS_CERTIFICATE_VERIFY_PEERS3 */
    err = gnutls_certificate_verify_peers2 (gnutls->tls_session, &status);
  if (err != GNUTLS_E_SUCCESS)
    {
      xfree (gnutls);
      warn_when_safe (Qtls_error, Qerror,
		      "GNUTLS could not verify peer certificate: %s",
		      gnutls_strerror (err));
      errno = EACCES;
      return NULL;
    }
  if (status != 0U)
    {
      gnutls_datum_t msg;

#ifdef HAVE_GNUTLS_CERTIFICATE_VERIFICATION_STATUS_PRINT
      int type = gnutls_certificate_type_get (gnutls->tls_session);
      err =
	gnutls_certificate_verification_status_print (status, type, &msg, 0);
#else
      err = -1;
#endif /* HAVE_GNUTLS_CERTIFICATE_VERIFICATION_STATUS_PRINT */
      xfree (gnutls);
      if (err == 0)
	{
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS: certificate validation failed: %s",
			  msg.data);
	  gnutls_free(msg.data);
	  errno = EACCES;
	  return NULL;
	}
      else
	{
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS: certificate validation failed with code %u",
			  status);
	  errno = EACCES;
	  return NULL;
	}
    }

#ifndef HAVE_GNUTLS_CERTIFICATE_VERIFY_PEERS3
  if (hostname != NULL)
    {
      /* Match the peer certificate against the host name */
      err = gnutls_x509_crt_init (&cert);
      if (err != GNUTLS_E_SUCCESS)
	{
	  xfree (gnutls);
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS error in gnutls_x509_crt_init: %s",
			  gnutls_strerror (err));
	  errno = EACCES;
	  return NULL;
	}

      /* The peer certificate is the first certificate in the list */
      err = gnutls_x509_crt_import (cert, certs, GNUTLS_X509_FMT_DER);
      if (err != GNUTLS_E_SUCCESS)
	{
	  xfree (gnutls);
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS error in gnutls_x509_crt_import: %s",
			  gnutls_strerror (err));
	  gnutls_x509_crt_deinit (cert);
	  errno = EACCES;
	  return NULL;
	}

      err = gnutls_x509_crt_check_hostname (cert, hostname);
      if (err == 0)
	{
	  xfree (gnutls);
	  warn_when_safe (Qtls_error, Qerror,
			  "GNUTLS: hostname does not match certificate: %s",
			  gnutls_strerror (err));
	  gnutls_x509_crt_deinit (cert);
	  errno = EACCES;
	  return NULL;
	}
      gnutls_x509_crt_deinit (cert);
    }
#endif /* HAVE_GNUTLS_CERTIFICATE_VERIFY_PEERS3 */

  return gnutls;
}

/* Set the key and certificate files to use */
static void
tls_set_x509_key_file (const Extbyte *certfile, const Extbyte *keyfile)
{
  int err;

  err = gnutls_certificate_set_x509_key_file (global_cred, certfile, keyfile,
					      GNUTLS_X509_FMT_PEM);
  if (err < GNUTLS_E_SUCCESS)
    signal_error (Qtls_error, "gnutls_certificate_set_x509_key_file",
		  GNUTLS_ERRSTR (err));
}

/* Function that gathers PKCS #11 passwords. */
static int
gnutls_pk11_password (void * UNUSED (userdata), int UNUSED (attempt),
		      const char *token_url, const char *token_label,
		      unsigned int UNUSED (flags), char *pin, size_t pin_max)
{
  Lisp_Object lsp_password, args[5];
  Extbyte *c_password;
  size_t len;

  /* Get the password from the user */
  args[0] = prompt;
  args[1] = build_extstring (token_label, Qnative);
  args[2] = build_ascstring (" (");
  args[3] = build_extstring (token_url, Qnative);
  args[4] = build_ascstring (")");
  lsp_password = call1 (Qread_password, Fconcat (5, args));
  c_password = LISP_STRING_TO_EXTERNAL (lsp_password, Qnative);

  /* Insert the password */
  len = strlen (c_password);
  if (len > pin_max)
    len = pin_max;
  memcpy (pin, c_password, len);
  pin[len] = '\0';

  /* Wipe out the password on the stack and in the Lisp string */
  args[0] = lsp_password;
  args[1] = make_char ('*');
  Ffill (2, args);
  memset (c_password, '*', strlen (c_password));
  return GNUTLS_E_SUCCESS;
}

static void xfree_for_gnutls (void *ptr)
{
  /* GnuTLS sometimes tries to free NULL */
  if (ptr != NULL)
    xfree (ptr);
}

void
init_tls (void)
{
  int err = GNUTLS_E_SUCCESS;

  /* Tell gnutls to use our memory allocation functions */
  gnutls_global_set_mem_functions ((void * (*)(size_t)) xmalloc,
				   (void * (*)(size_t)) xmalloc,
				   NULL,
				   (void * (*)(void *, size_t)) xrealloc,
				   xfree_for_gnutls);

  /* Initialize the library */
  err = gnutls_global_init ();
  if (err != GNUTLS_E_SUCCESS)
    signal_error (Qtls_error, "gnutls_global_init", GNUTLS_ERRSTR (err));

  /* Load the trusted CA certificates */
  err = gnutls_certificate_allocate_credentials (&global_cred);
  if (err != GNUTLS_E_SUCCESS)
    signal_error (Qtls_error, "gnutls_certificate_allocate_credentials",
		  GNUTLS_ERRSTR (err));
  err = gnutls_certificate_set_x509_system_trust (global_cred);
  if (err == 0)
    signal_error (Qtls_error, "gnutls: no system certificates found", Qnil);
  if (err < 0)
    signal_error (Qtls_error, "gnutls_certificate_set_x509_system_trust",
		  GNUTLS_ERRSTR (err));

  /* Setup password gathering */
  gnutls_pkcs11_set_pin_function (gnutls_pk11_password, NULL);
}
#endif /* HAVE_GNUTLS */

#ifdef HAVE_OPENSSL
#include <unistd.h>
#include <openssl/conf.h>
#include <openssl/err.h>

/* The context used to create connections */
static SSL_CTX *ssl_ctx;

static Lisp_Object
openssl_error_string (void)
{
  Lisp_Object args[5];
  unsigned long err = ERR_get_error ();

  args[0] = build_ascstring (ERR_lib_error_string (err));
  args[1] = build_ascstring (":");
  args[2] = build_ascstring (ERR_func_error_string (err));
  args[3] = build_ascstring (":");
  args[4] = build_ascstring (ERR_reason_error_string (err));
  return Fconcat (5, args);
}

static unsigned long
openssl_report_error_stack (const char *msg, const SSL *ssl)
{
  unsigned long err = ERR_get_error ();
  if (err > 0UL)
    {
      if (ERR_GET_LIB (err) == ERR_LIB_SSL &&
	  ERR_GET_REASON (err) == SSL_R_CERTIFICATE_VERIFY_FAILED)
	{
	  long cert_err = SSL_get_verify_result (ssl);
	  warn_when_safe (Qtls_error, Qerror, "%s:%s", msg,
			  X509_verify_cert_error_string (cert_err));
	}
      else
	{
	  const char *lib = ERR_lib_error_string (err);
	  const char *func = ERR_func_error_string (err);
	  const char *reason = ERR_reason_error_string (err);
	  warn_when_safe (Qtls_error, Qerror, "%s:%s:%s:%s", msg,
			  lib == NULL ? "<unknown>" : lib,
			  func == NULL ? "<unknown>" : func,
			  reason == NULL ? "<unknown>" : reason);
	}
    }
  return err;
}

/* Return values:
 * -1 = fatal error, caller should exit
 *  0 = no error, caller should continue
 *  1 = nonfatal error, caller should retry
 */
static int
openssl_report_error_num (const char *msg, const SSL *ssl, int ret, int retry)
{
  int errno_copy = errno;
  int ssl_error = SSL_get_error (ssl, ret);
  int err;

  switch (ssl_error)
    {
    case SSL_ERROR_NONE:
    case SSL_ERROR_ZERO_RETURN:
      err = 0;
      break;
    case SSL_ERROR_WANT_READ:
    case SSL_ERROR_WANT_WRITE:
      err = retry;
      break;
    case SSL_ERROR_WANT_CONNECT:
    case SSL_ERROR_WANT_ACCEPT:
    case SSL_ERROR_WANT_X509_LOOKUP:
      err = 1;
      break;
    case SSL_ERROR_SYSCALL:
      if (openssl_report_error_stack (msg, ssl) == 0UL && ret < 0)
	warn_when_safe (Qtls_error, Qerror, "%s: %s", msg,
			strerror (errno_copy));
      err = ret;
      break;
    case SSL_ERROR_SSL:
      openssl_report_error_stack (msg, ssl);
      err = -1;
      break;
    default:
      warn_when_safe (Qtls_error, Qerror, "%s: error %d", msg, ssl_error);
      err = -1;
      break;
    }
  errno = errno_copy;
  return err;
}

int
tls_get_fd (tls_state_t *state)
{
  return SSL_get_fd (state->tls_connection);
}

Bytecount
tls_read (tls_state_t *state, unsigned char *data, Bytecount size,
	  unsigned int allow_quit)
{
  int action, bytes;

  if (SSL_get_shutdown (state->tls_connection))
    return 0;

  bytes = SSL_read (state->tls_connection, data, size);
  action = (bytes > 0) ? 0
    : openssl_report_error_num ("SSL_read", state->tls_connection, bytes, 0);
  while (bytes <= 0 && action > 0)
    {
      if (allow_quit)
	QUIT;
      bytes = SSL_read (state->tls_connection, data, size);
      action = (bytes > 0) ? 0
	: openssl_report_error_num ("SSL_read", state->tls_connection,
				    bytes, 0);
    }
  return (Bytecount) bytes;
}

Bytecount
tls_write (tls_state_t *state, const unsigned char *data, Bytecount size,
	   unsigned int allow_quit)
{
  int action, bytes;

  if (SSL_get_shutdown (state->tls_connection))
    return 0;

  bytes = SSL_write (state->tls_connection, data, size);
  action = (bytes > 0) ? 0
    : openssl_report_error_num ("SSL_write", state->tls_connection, bytes, 0);
  while (bytes <= 0 && action > 0)
    {
      if (allow_quit)
	QUIT;
      bytes = SSL_write (state->tls_connection, data, size);
      action = (bytes > 0) ? 0
	: openssl_report_error_num ("SSL_write", state->tls_connection,
				    bytes, 0);
    }
  return (Bytecount) bytes;
}

int
tls_close (tls_state_t *state)
{
  if (--state->tls_refcount == 0)
    {
      int err, fd;

      fd = SSL_get_fd (state->tls_connection);
      if (SSL_get_shutdown (state->tls_connection) == 0)
	{
	  err = SSL_shutdown (state->tls_connection);
	  if (err < 0 && errno == EBADF)
	    err = 0;
	  if (err < 0)
	    openssl_report_error_num ("SSL_shutdown failed",
				      state->tls_connection, err, 0);
	}
      else
	{
	  err = 0;
	}
      close (fd);
      SSL_free (state->tls_connection);
      xfree (state);
      return err > 0 ? 0 : err;
     }
  return 0;
}

tls_state_t *
tls_open (int s, const Extbyte *hostname)
{
  tls_state_t *openssl;
  X509 *peer_cert = NULL;
  const int val = 1;
  int err;
  long cert_err;

  /* Disable Nagle's algorithm */
  setsockopt (s, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));

  /* Create the state object */
  openssl = xmalloc (sizeof (*openssl));
  openssl->tls_refcount = 2;

  /* Create the connection object */
  openssl->tls_connection = SSL_new (ssl_ctx);
  if (openssl->tls_connection == NULL)
    {
      openssl_report_error_stack ("SSL_new failed", NULL);
      goto error;
    }
  if (SSL_set_fd (openssl->tls_connection, s) == 0)
    {
      openssl_report_error_stack ("SSL_set_fd", openssl->tls_connection);
      goto error;
    }

  /* Enable the ServerNameIndication extension */
  if (hostname != NULL &&
      !SSL_set_tlsext_host_name (openssl->tls_connection, hostname))
    {
      openssl_report_error_stack ("SSL_set_tlsext_host_name failed",
				  openssl->tls_connection);
      goto error;
    }

  /* Perform the handshake */
  err = SSL_connect (openssl->tls_connection);
  while (err != 1)
    {
      int action = openssl_report_error_num ("SSL_connect failed",
					     openssl->tls_connection, err, 1);
      if (action < 0)
	goto error;
      err = SSL_connect (openssl->tls_connection);
    }

  /* Get the server certificate */
  peer_cert = SSL_get_peer_certificate (openssl->tls_connection);
  if (peer_cert == NULL)
    {
      warn_when_safe (Qtls_error, Qerror,
		      "Peer did not present a certificate");
      goto error;
    }

  cert_err = SSL_get_verify_result (openssl->tls_connection);
  if (cert_err != X509_V_OK)
    {
      warn_when_safe (Qtls_error, Qerror,
		      "Peer certificate verification failure:%s",
		      X509_verify_cert_error_string (cert_err));
      goto error;
    }

#ifdef HAVE_X509_CHECK_HOST
  err = X509_check_host (peer_cert, (const unsigned char *) hostname,
			 strlen (hostname), 0);
  if (err < 0)
    {
      warn_when_safe (Qtls_error, Qerror,
		      "Out of memory while checking certificate");
      goto error;
    }
  if (err == 0)
    {
      warn_when_safe (Qtls_error, Qerror,
		      "Peer certificate verification failure");
      goto error;
    }
#endif
  X509_free (peer_cert);

  return openssl;

 error:
  if (openssl->tls_connection != NULL)
    SSL_free (openssl->tls_connection);
  xfree (openssl);
  errno = EACCES;
  return NULL;
}

/* Set the key and certificate files to use */
static void
tls_set_x509_key_file (const Extbyte *certfile, const Extbyte *keyfile)
{
  int err;

  err = SSL_CTX_use_PrivateKey_file (ssl_ctx, keyfile, SSL_FILETYPE_PEM);
  if (err <= 0)
    signal_error (Qtls_error, "SSL_CTX_use_PrivateKey_file",
		  openssl_error_string ());
  err = SSL_CTX_use_certificate_file (ssl_ctx, certfile, SSL_FILETYPE_PEM);
  if (err <= 0)
    signal_error (Qtls_error, "SSL_CTX_use_certificate_file",
		  openssl_error_string ());
}

/* Function that gathers passwords for PKCS #11 tokens. */
static int
openssl_password (char *buf, int size, int UNUSED (rwflag),
		  void *UNUSED (userdata))
{
  Lisp_Object lsp_password, args[2];
  Extbyte *c_password;

  lsp_password =
    call1 (Qread_password, concat2 (prompt, build_ascstring ("PEM")));
  c_password = LISP_STRING_TO_EXTERNAL (lsp_password, Qnative);
  strncpy (buf, c_password, size);

  /* Wipe out the password on the stack and in the Lisp string */
  args[0] = lsp_password;
  args[1] = make_char ('*');
  Ffill (2, args);
  memset (c_password, '*', strlen (c_password));
  return (int) strlen (buf);
}

void
init_tls (void)
{
  /* Load the default configuration */
  OPENSSL_config (NULL);

  /* Tell openssl to use our memory allocation functions */
  CRYPTO_set_mem_functions ((void * (*)(size_t)) xmalloc,
			    (void * (*)(void *, size_t)) xrealloc,
			    xfree_1);

  /* Load human-readable error messages */
  SSL_load_error_strings ();

  /* Initialize the library */
  SSL_library_init ();

  /* Configure a client connection context, and send a handshake for the
   * highest supported TLS version. */
  ssl_ctx = SSL_CTX_new (SSLv23_client_method ());
  if (ssl_ctx == NULL)
    signal_error (Qtls_error, "SSL_CTX_new failed", openssl_error_string ());

  /* Disallow SSLv2 and disable compression. */
  SSL_CTX_set_options (ssl_ctx, SSL_OP_NO_SSLv2 | SSL_OP_NO_COMPRESSION);

  /* Set various useful mode bits */
  SSL_CTX_set_mode (ssl_ctx, SSL_MODE_ENABLE_PARTIAL_WRITE |
		    SSL_MODE_AUTO_RETRY | SSL_MODE_RELEASE_BUFFERS);

  /* Let the system select the ciphers */
  if (SSL_CTX_set_cipher_list (ssl_ctx, "DEFAULT") != 1)
    signal_error (Qtls_error, "SSL_CTX_set_cipher_list failed",
		  openssl_error_string ());

  /* Load the set of trusted root certificates. */
  if (!SSL_CTX_set_default_verify_paths (ssl_ctx))
    signal_error (Qtls_error, "SSL_CTX_set_default_verify_paths failed",
		  openssl_error_string ());

  /* Setup password gathering */
  SSL_CTX_set_default_passwd_cb (ssl_ctx, openssl_password);
}
#endif /* HAVE_OPENSSL */

#ifdef WITH_TLS
tls_state_t *
tls_negotiate (int fd, const Extbyte *host, Lisp_Object keylist)
{
  Lisp_Object tail;

  for (tail = keylist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object keyfile = Fcar (XCAR (tail));
      Lisp_Object certfile = Fcar (Fcdr (XCAR (tail)));
      Extbyte *c_keyfile, *c_certfile;

      if (!STRINGP (keyfile))
	invalid_argument ("Keyfile must be a filename", keyfile);
      if (!STRINGP (certfile))
	invalid_argument ("Certfile must be a filename", certfile);

      c_keyfile = LISP_STRING_TO_EXTERNAL (keyfile, Qfile_name);
      c_certfile = LISP_STRING_TO_EXTERNAL (certfile, Qfile_name);
      tls_set_x509_key_file (c_certfile, c_keyfile);
    }
  return tls_open (fd, host);
}
#endif /* WITH_TLS */

#ifndef WITH_TLS
void
init_tls (void)
{
}
#endif /* !WITH_TLS */

void
syms_of_tls (void)
{
#ifdef WITH_TLS
  DEFSYMBOL (Qread_password);
#endif
  DEFERROR (Qtls_error, "TLS error", Qerror);
}

void
vars_of_tls (void)
{
#ifdef WITH_TLS
  staticpro (&prompt);
  prompt = build_ascstring ("Password for ");
  Fprovide (intern ("tls"));
#ifdef HAVE_NSS
  Fprovide (intern ("tls-nss"));
#endif
#ifdef HAVE_GNUTLS
  Fprovide (intern ("tls-gnutls"));
#endif
#ifdef HAVE_OPENSSL
  Fprovide (intern ("tls-openssl"));
#endif
#endif
}
