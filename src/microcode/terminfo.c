/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

$Id$

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

#include "config.h"

extern char * tparm (const char *, ...);

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.  */

char *
tparam (const char * string,
       char * outstring,
       int len,
       int arg1,
       int arg2,
       int arg3,
       int arg4,
       int arg5,
       int arg6,
       int arg7,
       int arg8,
       int arg9)
{
  char * temp = (tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
  if (outstring == 0)
    outstring = ((char *) (malloc ((strlen (temp)) + 1)));
  strcpy (outstring, temp);
  return (outstring);
}
