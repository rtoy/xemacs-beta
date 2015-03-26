/* system description file for cygwin32.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 2001 Ben Wing.

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

/* Building under cygwin
 *
 * The approach I have taken with this port is to use primarily the
 * UNIX code base adding stuff that is MS-Windows specific. This works
 * quite well, and is in keeping with my perception of the cygwin
 * philosophy.  Note that if you make changes to this file you do NOT
 * want to define WIN32_NATIVE (formerly "WINDOWSNT"), I repeat - do
 * not define this, it will break everything horribly. What does get
 * defined is HAVE_MS_WINDOWS, but this is done by configure and only
 * applies to the window system.
 *
 * When building make sure your HOME path is unix style - i.e. without
 * a drive letter.
 *
 * once you have done this, configure and make.
 *
 * Andy Piper <andy@xemacs.org> 8/1/98 
 * http://www.xemacs.freeserve.co.uk/ */

#include "cygwin-common.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "cygwin64"
