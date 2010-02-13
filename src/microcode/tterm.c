/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* termcap(3) interface for Scheme. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"

#if defined(HAVE_NCURSES_H) || defined(HAVE_CURSES_H)
/* ncurses' <curses.h> will define false and true, but in recent
   versions having them defined prior to including <curses.h> can cause
   a parsing error on GNU systems.  */
#  undef false
#  undef true
#  ifdef HAVE_TERMIOS_H
#    include <termios.h>
#  endif
#  if defined(HAVE_NCURSES_H)
#    include <ncurses.h>
#  elif defined(HAVE_CURSES_H)
#    include <curses.h>
#  endif
#endif

#if defined(HAVE_TERM_H)
#  include <term.h>
#else
   extern int tgetent (char *, const char *);
   extern int tgetnum (const char *);
   extern int tgetflag (const char *);
   extern char * tgetstr (const char *, char **);
   extern char * tgoto (const char *, int, int);
   extern int tputs (const char *, int, int (*) (int));
#endif

extern char * tparam (const char *, void *, int, ...);
extern char * BC;
extern char * UP;
extern char PC;
extern speed_t ospeed;

#ifndef TERMCAP_BUFFER_SIZE
#define TERMCAP_BUFFER_SIZE 2048
#endif

static char termcap_buffer [TERMCAP_BUFFER_SIZE];
static char tgetstr_buffer [TERMCAP_BUFFER_SIZE];
static char * tgetstr_pointer;

static char tputs_output [TERMCAP_BUFFER_SIZE];
static char * tputs_output_scan;

static int
tputs_write_char (int c)
{
  if (tputs_output_scan >= (tputs_output + TERMCAP_BUFFER_SIZE))
    error_external_return ();
  (*tputs_output_scan++) = c;
  return (c);
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
    PRIMITIVE_RETURN ((result == 0)
		      ? SHARP_F
		      : (char_pointer_to_string (result)));
  }
}

struct tc_env
{
  char *string_buffer;
  char *string_pointer;
};

static void
protect_tc (void *environment)
{
  struct tc_env *env = ((struct tc_env *) environment);
  char *pointer = (env -> string_pointer);
  if ((pointer != 0) && (pointer != (env -> string_buffer)))
    free (pointer);
}

DEFINE_PRIMITIVE ("TERMCAP-PARAM-STRING", Prim_termcap_param_string, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    char string_buffer [4096];
    struct tc_env env;
    SCHEME_OBJECT string = UNSPECIFIC;
    (env . string_buffer) = string_buffer;
    (env . string_pointer) = 0;
    transaction_begin ();
    dstack_protect ((&protect_tc), (&env));
    (env . string_pointer)
      = (tparam ((STRING_ARG (1)), string_buffer, (sizeof (string_buffer)),
		 (arg_nonnegative_integer (2)),
		 (arg_nonnegative_integer (3)),
		 (arg_nonnegative_integer (4)),
		 (arg_nonnegative_integer (5))));
    if ((env . string_pointer) == 0)
      error_external_return ();
    string = (char_pointer_to_string (env . string_pointer));
    transaction_commit ();
    PRIMITIVE_RETURN (string);
  }
}

static void
protect_free (void *environment)
{
  char *pointer = (* ((char **) environment));
  if (pointer != 0)
    free (pointer);
}

DEFINE_PRIMITIVE ("TERMCAP-GOTO-STRING", Prim_termcap_goto_string, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    char *string_pointer = 0;
    SCHEME_OBJECT string = UNSPECIFIC;
    BC = (((ARG_REF (4)) == SHARP_F) ? 0 : (STRING_ARG (4)));
    UP = (((ARG_REF (5)) == SHARP_F) ? 0 : (STRING_ARG (5)));
    transaction_begin ();
    dstack_protect ((&protect_free), (&string_pointer));
    string_pointer
      = (tgoto ((STRING_ARG (1)),
		(arg_nonnegative_integer (2)),
		(arg_nonnegative_integer (3))));
    if (string_pointer == 0)
      error_external_return ();
    string = (char_pointer_to_string (string_pointer));
    transaction_commit ();
    PRIMITIVE_RETURN (string);
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
