/* LDAP client interface for XEmacs.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* Author: Oscar Figueiredo */

/* This file provides lisp primitives for access to an LDAP library
   conforming to the API defined in RFC 1823.
   It has been tested with:
   - UMich LDAP 3.3 (http://www.umich.edu/~dirsvcs/ldap/)
   - Netscape's LDAP SDK 1.0 (http://developer.netscape.com) */


#include <config.h>
#include "lisp.h"

#include <errno.h>
#include <lber.h>
#include <ldap.h>

#include "eldap.h"

#ifdef HAVE_NS_LDAP
#define HAVE_LDAP_SET_OPTION 1
#define HAVE_LDAP_GET_ERRNO 1
#else
#undef HAVE_LDAP_SET_OPTION
#undef HAVE_LDAP_GET_ERRNO
#endif



static int ldap_default_port;
static Lisp_Object Vldap_default_base;

/* ldap-open plist keywords */
static Lisp_Object Qport, Qauth, Qbinddn, Qpasswd, Qderef, Qtimelimit,
  Qsizelimit;
/* Search scope limits */
static Lisp_Object Qbase, Qonelevel, Qsubtree;
/* Authentication methods */
#ifdef LDAP_AUTH_KRBV41
static Lisp_Object Qkrbv41;
#endif
#ifdef LDAP_AUTH_KRBV42
static Lisp_Object Qkrbv42;
#endif
/* Deref policy */
static Lisp_Object Qnever, Qalways, Qfind;

static Lisp_Object Qldapp;


/************************************************************************/
/*                         Utility Functions                            */
/************************************************************************/

static void
signal_ldap_error (LDAP *ld)
{
#if HAVE_LDAP_GET_ERRNO
  signal_simple_error
    ("LDAP error",
     build_string (ldap_err2string (ldap_get_lderrno (ld, NULL, NULL))));
#else
  signal_simple_error ("LDAP error",
                       build_string (ldap_err2string (ld->ld_errno)));
#endif
}


/************************************************************************/
/*                          The ldap Lisp object                        */
/************************************************************************/

/*
 * Structure records pertinent information about an open LDAP connection.
 */

struct Lisp_LDAP
{
  /* lcrecord header */
  struct lcrecord_header header;
  /* The LDAP connection handle used by the LDAP API */
  LDAP *ld;
  /* Name of the host we connected to */
  Lisp_Object host;
  /* Status of the LDAP connection.
     This is a symbol: open or closed */
  Lisp_Object status_symbol;
};



static Lisp_Object
mark_ldap (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_LDAP *ldap = XLDAP (obj);
  ((markobj) (ldap->host));
  return ldap->status_symbol;
}

static void
print_ldap (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[16];

  struct Lisp_LDAP *ldap = XLDAP (obj);

  if (print_readably)
    error ("printing unreadable object #<ldap %s>",
           XSTRING_DATA (ldap->host));

  if (!escapeflag)
    {
      print_internal (ldap->host, printcharfun, 0);
    }
  else
    {
      write_c_string (GETTEXT ("#<ldap "), printcharfun);
      print_internal (ldap->host, printcharfun, 1);
      write_c_string (" state:",printcharfun);
      print_internal (ldap->status_symbol, printcharfun, 1);
      sprintf (buf, " 0x%x>", ldap);
      write_c_string (buf, printcharfun);
    }
}

static struct Lisp_LDAP *
allocate_ldap (void)
{
  struct Lisp_LDAP *ldap =
    alloc_lcrecord_type (struct Lisp_LDAP, lrecord_ldap);

  ldap->ld = (LDAP *) NULL;
  ldap->host = Qnil;
  ldap->status_symbol = Qnil;
  return ldap;
}

DEFINE_LRECORD_IMPLEMENTATION ("ldap", ldap,
                               mark_ldap, print_ldap, NULL,
                               NULL, NULL, struct Lisp_LDAP);




/************************************************************************/
/*                        Basic ldap accessors                          */
/************************************************************************/

DEFUN ("ldapp", Fldapp, 1, 1, 0, /*
Return t if OBJECT is a LDAP connection.
*/
       (object))
{
  return LDAPP (object) ? Qt : Qnil;
}


DEFUN ("ldap-host", Fldap_host, 1, 1, 0, /*
Return the server host of the connection LDAP, as a string.
*/
       (ldap))
{
  CHECK_LDAP (ldap);
  return (XLDAP (ldap))->host;
}



DEFUN ("ldap-status", Fldap_status, 1, 1, 0, /*
Return the status of the connection LDAP.
This is a symbol, one of these:

open   -- for a LDAP connection that is open.
closed -- for a LDAP connection that is closed.
*/
       (ldap))
{
  CHECK_LDAP (ldap);
  return (XLDAP (ldap))->status_symbol;
}



/************************************************************************/
/*                  Opening/Closing a LDAP connection                   */
/************************************************************************/


DEFUN ("ldap-open", Fldap_open, 1, 2, 0, /*
Open a LDAP connection to HOST.
PLIST is a plist containing additional parameters for the connection.
Valid keys in that list are:
  `port' the TCP port to use for the connection if different from
`ldap-default-port'.
  `auth' is the authentication method to use, possible values depend on
the LDAP library XEmacs was compiled with: `simple', `krbv41' and `krbv42'.
  `binddn' is the distinguished name of the user to bind as (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return.
*/
       (host, plist))
{
  /* This function can call lisp */

  struct Lisp_LDAP *lisp_ldap;
  LDAP *ld;
  int   ldap_port = 0;
  int  ldap_auth = LDAP_AUTH_SIMPLE;
  char *ldap_binddn = NULL;
  char *ldap_passwd = NULL;
  int  ldap_deref = LDAP_DEREF_NEVER;
  int  ldap_timelimit = 0;
  int  ldap_sizelimit = 0;
  int err;

  Lisp_Object ldap, list, keyword, value;
  struct gcpro gcpro1;

  ldap =  Qnil;
  GCPRO1 (ldap);

  CHECK_STRING (host);

  EXTERNAL_PROPERTY_LIST_LOOP(list, keyword, value, plist)
    {
      /* TCP Port */
      if (EQ (keyword, Qport))
        {
          CHECK_INT (value);
          ldap_port = XINT (value);
        }
      /* Authentication method */
      if (EQ (keyword, Qauth))
        {
          CHECK_SYMBOL (value);

          if (EQ (value, Qsimple))
            ldap_auth = LDAP_AUTH_SIMPLE;
#ifdef LDAP_AUTH_KRBV41
          else if (EQ (value, Qkrbv41))
            ldap_auth = LDAP_AUTH_KRBV41;
#endif
#ifdef LDAP_AUTH_KRBV42
          else if (EQ (value, Qkrbv42))
            ldap_auth = LDAP_AUTH_KRBV42;
#endif
          else
            signal_simple_error ("Invalid authentication method", value);
        }
      /* Bind DN */
      else if (EQ (keyword, Qbinddn))
        {
          CHECK_STRING (value);
          ldap_binddn = alloca (XSTRING_LENGTH (value) + 1);
          strcpy (ldap_binddn, (char *)XSTRING_DATA (value));
        }
      /* Password */
      else if (EQ (keyword, Qpasswd))
        {
          CHECK_STRING (value);
          ldap_passwd = alloca (XSTRING_LENGTH (value) + 1);
          strcpy (ldap_passwd, (char *)XSTRING_DATA (value));
        }
      /* Deref */
      else if (EQ (keyword, Qderef))
        {
          CHECK_SYMBOL (value);
          if (EQ (value, Qnever))
            ldap_deref = LDAP_DEREF_NEVER;
          else if (EQ (value, Qsearch))
            ldap_deref = LDAP_DEREF_SEARCHING;
          else if (EQ (value, Qfind))
            ldap_deref = LDAP_DEREF_FINDING;
          else if (EQ (value, Qalways))
            ldap_deref = LDAP_DEREF_ALWAYS;
          else
            signal_simple_error ("Invalid deref value", value);
        }
      /* Timelimit */
      else if (EQ (keyword, Qtimelimit))
        {
          CHECK_INT (value);
          ldap_timelimit = XINT (value);
        }
      /* Sizelimit */
      else if (EQ (keyword, Qsizelimit))
        {
          CHECK_INT (value);
          ldap_sizelimit = XINT (value);
        }
    }

  if (ldap_port == 0)
    {
      ldap_port = ldap_default_port;
    }

  /* Connect to the server and bind */
  ld = ldap_open ((char *)XSTRING_DATA (host), ldap_port);
  if (ld == NULL )
    signal_simple_error_2 ("Failed connecting to host",
                           host,
                           lisp_strerror (errno));


#if HAVE_LDAP_SET_OPTION
  if (ldap_set_option (ld, LDAP_OPT_DEREF, (void *)&ldap_deref) != LDAP_SUCCESS)
    signal_ldap_error (ld);
  if (ldap_set_option (ld, LDAP_OPT_TIMELIMIT,
                       (void *)&ldap_timelimit) != LDAP_SUCCESS)
    signal_ldap_error (ld);
  if (ldap_set_option (ld, LDAP_OPT_SIZELIMIT,
                       (void *)&ldap_sizelimit) != LDAP_SUCCESS)
    signal_ldap_error (ld);
  if (ldap_set_option (ld, LDAP_OPT_REFERRALS, LDAP_OPT_ON) != LDAP_SUCCESS)
    signal_ldap_error (ld);
#else /* HAVE_LDAP_SET_OPTION */
  ld->ld_deref = ldap_deref;
  ld->ld_timelimit = ldap_timelimit;
  ld->ld_sizelimit = ldap_sizelimit;
#ifdef LDAP_REFERRALS
  ld->ld_options = LDAP_OPT_REFERRALS;
#else /* LDAP_REFERRALS */
  ld->ld_options = 0;
#endif /* LDAP_REFERRALS */
#endif /* HAVE_LDAP_SET_OPTION */

  /* ldap_bind_s calls select and may be wedged by spurious signals */
  slow_down_interrupts ();
  err = ldap_bind_s (ld, ldap_binddn, ldap_passwd, ldap_auth);
  speed_up_interrupts ();
  if (err != LDAP_SUCCESS)
    signal_simple_error ("Failed binding to the server",
                         build_string (ldap_err2string (err)));

  lisp_ldap = allocate_ldap ();
  lisp_ldap->ld = ld;
  lisp_ldap->host = host;
  lisp_ldap->status_symbol = Qopen;
  XSETLDAP (ldap,lisp_ldap);

  UNGCPRO;
  return ldap;
}



DEFUN ("ldap-close", Fldap_close, 1, 1, 0, /*
Close an LDAP connection.
Return t if the connection was actually closed or nil if
it was already closed before the call
*/
      (ldap))
{
  CHECK_LDAP (ldap);
  if ( EQ ((XLDAP (ldap))->status_symbol, Qopen) )
    {
      ldap_unbind ((XLDAP (ldap))->ld);
      (XLDAP (ldap))->status_symbol = Qclosed;
      return Qt;
    }
  return Qnil;
}



/************************************************************************/
/*                  Working on a LDAP connection                        */
/************************************************************************/

DEFUN ("ldap-search-internal", Fldap_search_internal, 2, 6, 0, /*
Perform a search on an open LDAP connection.
LDAP is an LDAP connection object created with `ldap-open'.
FILTER is a filter string for the search as described in RFC 1558
BASE is the distinguished name at which to start the search
SCOPE is an integer or a symbol indicating the scope of the search
 Possible values are `ldap-scope-base', `ldap-scope-onelevel' or
 `ldap-scope-subtree'
ATTRS is a list of strings indicating which attributes to retrieve
 for each matching entry. If nil return all available attributes.
If ATTRSONLY is non-nil then only the attributes are retrieved, not
the associated values
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/values.
*/
       (ldap, filter, base, scope, attrs, attrsonly))
{
  /* This function can call lisp */

  /* Vars for query */
  LDAP *ld;
  LDAPMessage *res, *e;
  BerElement *ptr;
  char *a;
  int i, rc, err;

  char **vals = NULL;
  int  matches;

  int  ldap_scope = LDAP_SCOPE_SUBTREE;
  char **ldap_attributes = NULL;

  Lisp_Object list, entry, result;
  struct gcpro gcpro1, gcpro2, gcpro3;

  list = entry = result = Qnil;
  GCPRO3(list, entry, result);

  /* Do all the parameter checking  */
  CHECK_LIVE_LDAP (ldap);
  ld = (XLDAP (ldap))->ld;

  /* Filter */
  CHECK_STRING (filter);

  /* Search base */
  if (NILP (base))
    {
      base = Vldap_default_base;
    }
  if (!NILP (base))
    {
      CHECK_STRING (Vldap_default_base);
    }

  /* Search scope */
  if (!NILP (scope))
    {
      CHECK_SYMBOL (scope);
      if (EQ (scope, Qbase))
        ldap_scope = LDAP_SCOPE_BASE;
      else if (EQ (scope, Qonelevel))
        ldap_scope = LDAP_SCOPE_ONELEVEL;
      else if (EQ (scope, Qsubtree))
        ldap_scope = LDAP_SCOPE_SUBTREE;
      else
        signal_simple_error ("Invalid scope", scope);
    }

  /* Attributes to search */
  if (!NILP (attrs))
    {
      Lisp_Object attr_left = attrs;
      struct gcpro ngcpro1;

      NGCPRO1 (attr_left);
      CHECK_CONS (attrs);

      ldap_attributes = alloca ((XINT (Flength (attrs)) + 1)*sizeof (char *));

      for (i=0; !NILP (attr_left); i++) {
        CHECK_STRING (XCAR (attr_left));
        ldap_attributes[i] = alloca (XSTRING_LENGTH (XCAR (attr_left)) + 1);
        strcpy(ldap_attributes[i],
               (char *)(XSTRING_DATA( XCAR (attr_left))));
        attr_left = XCDR (attr_left);
      }
      ldap_attributes[i] = NULL;
      NUNGCPRO;
    }

  /* Attributes only ? */
  CHECK_SYMBOL (attrsonly);


  /* Perform the search */
  if (ldap_search (ld,
                   NILP (base) ? "" : (char *) XSTRING_DATA (base),
                   ldap_scope,
                   NILP (filter) ? "" : (char *) XSTRING_DATA (filter),
                   ldap_attributes,
                   NILP (attrsonly) ? 0 : 1)
       == -1)
    {
      signal_ldap_error (ld);
    }

  /* Build the results list */
  matches = 0;

  /* ldap_result calls select() and can get wedged by EINTR signals */
  slow_down_interrupts ();
  rc = ldap_result (ld, LDAP_RES_ANY, 0, NULL, &res);
  speed_up_interrupts ();
  while ( rc == LDAP_RES_SEARCH_ENTRY )
    {
      matches ++;
      e = ldap_first_entry (ld, res);
      message ("Parsing results... %d", matches);
      entry = Qnil;
      for (a= ldap_first_attribute (ld, e, &ptr);
           a != NULL;
           a= ldap_next_attribute (ld, e, ptr) )
        {
          list = Fcons (build_string (a), Qnil);
          vals = ldap_get_values (ld, e, a);
          if (vals != NULL)
            {
              for (i=0; vals[i]!=NULL; i++)
                {
                  list = Fcons (build_string (vals[i]),
                                list);
                }
            }
          entry = Fcons (Fnreverse (list),
                         entry);
          ldap_value_free (vals);
        }
      result = Fcons (Fnreverse (entry),
                      result);
      ldap_msgfree (res);

      slow_down_interrupts ();
      rc = ldap_result (ld, LDAP_RES_ANY, 0, NULL, &res);
      speed_up_interrupts ();
    }

  if (rc == -1)
    {
      signal_ldap_error (ld);
    }

  if ((rc = ldap_result2error (ld, res, 0)) != LDAP_SUCCESS)
    {
      signal_ldap_error (ld);
    }

  ldap_msgfree (res);
  message ("Done.");

  result = Fnreverse (result);
  clear_message ();

  UNGCPRO;
  return result;
}


void
syms_of_eldap (void)
{
  defsymbol (&Qldapp, "ldapp");

  DEFSUBR (Fldapp);
  DEFSUBR (Fldap_host);
  DEFSUBR (Fldap_status);
  DEFSUBR (Fldap_open);
  DEFSUBR (Fldap_close);
  DEFSUBR (Fldap_search_internal);
}

void
vars_of_eldap (void)
{
  Fprovide (intern ("ldap"));

  ldap_default_port = LDAP_PORT;
  Vldap_default_base =  Qnil;

  DEFVAR_INT ("ldap-default-port", &ldap_default_port /*
Default TCP port for LDAP connections.
Initialized from the LDAP library. Default value is 389.
*/ );

  DEFVAR_LISP ("ldap-default-base", &Vldap_default_base /*
Default base for LDAP searches.
This is a string using the syntax of RFC 1779.
For instance, "o=ACME, c=US" limits the search to the
Acme organization in the United States.
*/ );

}


