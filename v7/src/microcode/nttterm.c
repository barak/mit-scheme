/* -*-C-*-

$Id: nttterm.c,v 1.5 2002/11/20 19:46:11 cph Exp $

Copyright (c) 1992-2000 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* termcap(3) interface for Scheme -- Only a subset needed for Win32. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"

extern char * EXFUN (tparam, (char *, char*, int, int, ...));
extern char * EXFUN (tgoto, (char *, int, int));
extern int EXFUN (tputs, (char *, int, void (*) (int)));
extern char * BC;
extern char * UP;
extern char PC;
extern short ospeed;

#ifndef TERMCAP_BUFFER_SIZE
#define TERMCAP_BUFFER_SIZE 2048
#endif

static char tputs_output [TERMCAP_BUFFER_SIZE];
static char * tputs_output_scan;

static void
DEFUN (tputs_write_char, (c), int c)
{
  (*tputs_output_scan++) = c;
  return;
}

DEFINE_PRIMITIVE ("TERMCAP-PARAM-STRING", Prim_termcap_param_string, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    char * s =
      (tparam ((STRING_ARG (1)), 0, 0,
	       (arg_nonnegative_integer (2)),
	       (arg_nonnegative_integer (3)),
	       (arg_nonnegative_integer (4)),
	       (arg_nonnegative_integer (5))));
    SCHEME_OBJECT result = (char_pointer_to_string ((unsigned char *) s));
    free (s);
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("TERMCAP-GOTO-STRING", Prim_termcap_goto_string, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    BC = (((ARG_REF (4)) == SHARP_F) ? 0 : (STRING_ARG (4)));
    UP = (((ARG_REF (5)) == SHARP_F) ? 0 : (STRING_ARG (5)));
    PRIMITIVE_RETURN
      (char_pointer_to_string
       ((unsigned char *)
	(tgoto ((STRING_ARG (1)),
		(arg_nonnegative_integer (2)),
		(arg_nonnegative_integer (3))))));
  }
}

DEFINE_PRIMITIVE ("TERMCAP-PAD-STRING", Prim_termcap_pad_string, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  ospeed = (arg_baud_index (3));
  PC = (((ARG_REF (4)) == SHARP_F) ? '\0' : ((STRING_ARG (4)) [0]));
  tputs_output_scan = tputs_output;
  tputs ((STRING_ARG (1)), (arg_nonnegative_integer (2)), tputs_write_char);
  PRIMITIVE_RETURN
    (memory_to_string ((tputs_output_scan - tputs_output),
		       ((unsigned char *) tputs_output)));
}
