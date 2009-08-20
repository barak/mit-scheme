/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* termcap(3) interface for Scheme -- Only a subset needed for Win32. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"

extern char * tparam (char *, char*, int, int, ...);
extern char * tgoto (char *, int, int);
extern int tputs (char *, int, void (*) (int));
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
tputs_write_char (int c)
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
    SCHEME_OBJECT result = (char_pointer_to_string (s));
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
      (char_pointer_to_string (tgoto ((STRING_ARG (1)),
				      (arg_nonnegative_integer (2)),
				      (arg_nonnegative_integer (3)))));
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
