/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.
   Copyright (C) 1998-2001 Massachusetts Institute of Technology

$Id: terminfo.c,v 1.6 2001/03/03 02:00:09 cph Exp $

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

#ifdef STDC_HEADERS
#  include <stdlib.h>
#  include <string.h>
#endif

extern char * EXFUN (tparm, (CONST char *, ...));

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.  */

char *
DEFUN (tparam, (string, outstring, len, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9),
       CONST char * string AND
       char * outstring AND
       int len AND
       int arg1 AND
       int arg2 AND
       int arg3 AND
       int arg4 AND
       int arg5 AND
       int arg6 AND
       int arg7 AND
       int arg8 AND
       int arg9 AND)
{
  char * temp = (tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
  if (outstring == 0)
    outstring = ((char *) (malloc ((strlen (temp)) + 1)));
  strcpy (outstring, temp);
  return (outstring);
}
