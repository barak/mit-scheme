/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/tterm.c,v 1.1 1990/10/16 20:52:15 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* termcap(3) interface for Scheme. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"

extern int EXFUN (tgetent, (char *, char *));
extern int EXFUN (tgetnum, (char *));
extern int EXFUN (tgetflag, (char *));
extern char * EXFUN (tgetstr, (char *, char **));
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

static char termcap_buffer [TERMCAP_BUFFER_SIZE];
static char tgetstr_buffer [TERMCAP_BUFFER_SIZE];
static char * tgetstr_pointer;

static char tputs_output [TERMCAP_BUFFER_SIZE];
static char * tputs_output_scan;

static void
DEFUN (tputs_write_char, (c), int c)
{
  (*tputs_output_scan++) = c;
}

DEFINE_PRIMITIVE ("TERMCAP-INITIALIZE", Prim_termcap_initialize, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  tgetstr_pointer = tgetstr_buffer;
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT ((tgetent (termcap_buffer, (STRING_ARG (1)))) > 0));
}

DEFINE_PRIMITIVE ("TERMCAP-GET-NUMBER", Prim_termcap_get_number, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    int result = (tgetnum (STRING_ARG (1)));
    PRIMITIVE_RETURN ((result < 0) ? SHARP_F : (long_to_integer (result)));
  }
}

DEFINE_PRIMITIVE ("TERMCAP-GET-FLAG", Prim_termcap_get_flag, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((tgetflag (STRING_ARG (1))) != 0));
}

DEFINE_PRIMITIVE ("TERMCAP-GET-STRING", Prim_termcap_get_string, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    char * result = (tgetstr ((STRING_ARG (1)), (&tgetstr_pointer)));
    PRIMITIVE_RETURN
      ((result == 0) ? SHARP_F : (char_pointer_to_string (result)));
  }
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
      (char_pointer_to_string
       (tgoto ((STRING_ARG (1)),
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
    (memory_to_string ((tputs_output_scan - tputs_output), tputs_output));
}
