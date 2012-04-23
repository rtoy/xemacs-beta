/*

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

/* Synched up with: Not really in FSF. */

#ifndef WIN32_NATIVE

# include <pwd.h>

#else /* WIN32_NATIVE */

struct passwd
{
  char *pw_name;
  char *pw_passwd;
  int   pw_uid;
  int   pw_gid;
  int   pw_quota;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
};

#ifdef emacs

struct passwd *getpwuid (uid_t uid);
struct passwd *getpwnam (const Ibyte *name);
uid_t getuid (void);
uid_t geteuid (void);
gid_t getgid (void);
gid_t getegid (void);

#endif /* emacs */

#endif /* WIN32_NATIVE */

#ifdef emacs

struct passwd *qxe_getpwnam (const Ibyte *name);
struct passwd *qxe_getpwuid (uid_t uid);
struct passwd *qxe_getpwent (void);

#endif /* emacs */

