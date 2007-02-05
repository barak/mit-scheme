/* -*-C-*-

$Id: pruxio.c,v 1.12 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"
#include "ux.h"
#include "uxselect.h"
#include "uxproc.h"

#ifndef __hp9000s700
/* Blows up HP 9000/700 compiler (HP-UX 8.05)!  */
extern Tchannel EXFUN (arg_channel, (int arg_number));
extern int EXFUN (UX_channel_descriptor, (Tchannel channel));
#endif

static CONST char ** EXFUN (string_vector_arg, (int arg));
static int EXFUN (string_vector_p, (SCHEME_OBJECT vector));
static CONST char ** EXFUN (convert_string_vector, (SCHEME_OBJECT vector));

DEFINE_PRIMITIVE ("SELECT-REGISTRY-SIZE", Prim_selreg_size, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_select_registry_size ()));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-LUB", Prim_selreg_lub, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_select_registry_lub ()));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-CLEAR-ALL", Prim_selreg_clear_all, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  UX_select_registry_clear_all (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-SET", Prim_selreg_set, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  UX_select_registry_set ((STRING_ARG (1)), (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-CLEAR", Prim_selreg_clear, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  UX_select_registry_clear ((STRING_ARG (1)), (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-IS-SET?", Prim_selreg_is_set_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (UX_select_registry_is_set
      ((STRING_ARG (1)), (arg_nonnegative_integer (2)))));
}

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (UX_channel_descriptor (arg_channel (1))));
}

DEFINE_PRIMITIVE ("SELECT-DESCRIPTOR", Prim_select_descriptor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  switch (UX_select_descriptor ((arg_nonnegative_integer (1)),
				(BOOLEAN_ARG (2))))
    {
    case select_input_none:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case select_input_argument:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case select_input_process_status:
      PRIMITIVE_RETURN (LONG_TO_FIXNUM (-1));
    case select_input_interrupt:
      PRIMITIVE_RETURN (LONG_TO_FIXNUM (-2));
    default:
      error_external_return ();
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-TEST", Prim_selreg_test, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, VECTOR_P);
  {
    PTR position = dstack_position;
    unsigned int lub = (UX_select_registry_lub ());
    unsigned int * fds = (dstack_alloc ((sizeof (unsigned int)) * lub));
    unsigned int nfds;
    SCHEME_OBJECT result = SHARP_F;

    if ((VECTOR_LENGTH (ARG_REF (3))) != lub)
      error_bad_range_arg (3);
    switch (UX_select_registry_test ((STRING_ARG (1)), (BOOLEAN_ARG (2)),
				     fds, (&nfds)))
      {
      case select_input_none:
	result = (LONG_TO_UNSIGNED_FIXNUM (0));
	break;
      case select_input_argument:
	{
	  unsigned int * scan_fds = fds;
	  unsigned int * end_fds = (scan_fds + nfds);
	  SCHEME_OBJECT * scan_vector = (VECTOR_LOC ((ARG_REF (3)), 0));
	  while (scan_fds < end_fds)
	    (*scan_vector++) = (LONG_TO_UNSIGNED_FIXNUM (*scan_fds++));
	}
	result = (LONG_TO_UNSIGNED_FIXNUM (nfds));
	break;
      case select_input_process_status:
	result = (LONG_TO_FIXNUM (-1));
	break;
      case select_input_interrupt:
	result = (LONG_TO_FIXNUM (-2));
	break;
      default:
	error_external_return ();
	break;
      }
    dstack_set_position (position);
    PRIMITIVE_RETURN (result);
  }
}

#define PROCESS_CHANNEL_ARG(arg, type, channel)				\
{									\
  if ((ARG_REF (arg)) == SHARP_F)					\
    (type) = process_channel_type_none;					\
  else if ((ARG_REF (arg)) == (LONG_TO_FIXNUM (-1)))			\
    (type) = process_channel_type_inherit;				\
  else if ((ARG_REF (arg)) == (LONG_TO_FIXNUM (-2)))			\
    {									\
      if (ctty_type != process_ctty_type_explicit)			\
	error_bad_range_arg (arg);					\
      (type) = process_channel_type_ctty;				\
    }									\
  else									\
    {									\
      (type) = process_channel_type_explicit;				\
      (channel) = (arg_channel (arg));					\
    }									\
}

DEFINE_PRIMITIVE ("UX-MAKE-SUBPROCESS", Prim_UX_make_subprocess, 8, 8,
  "(FILENAME ARGV ENV WORK-DIR STDIN STDOUT STDERR)\n\
Create a subprocess.\n\
FILENAME is the program to run.\n\
ARGV is a vector of strings to pass to the program as arguments.\n\
ENV is a vector of strings to pass as the program's environment;\n\
  #F means inherit Scheme's environment.\n\
WORK-DIR is a string to pass as the program's working directory;\n\
  #F means inherit Scheme's working directory.\n\
CTTY specifies the program's controlling terminal:\n\
  #F means none;\n\
  -1 means use Scheme's controlling terminal in background;\n\
  -2 means use Scheme's controlling terminal in foreground;\n\
  string means open that terminal.\n\
STDIN is the input channel for the subprocess.\n\
STDOUT is the output channel for the subprocess.\n\
STDERR is the error channel for the subprocess.\n\
  Each channel arg can take these values:\n\
  #F means none;\n\
  -1 means use the corresponding channel from Scheme;\n\
  -2 means use the controlling terminal (valid only when CTTY is a string);\n\
  otherwise the argument must be a channel.")
{
  PRIMITIVE_HEADER (8);
  {
    PTR position = dstack_position;
    CONST char * filename = (STRING_ARG (1));
    CONST char ** argv = (string_vector_arg (2));
    CONST char ** env
      = (((ARG_REF (3)) == SHARP_F) ? 0 : (string_vector_arg (3)));
    CONST char * working_directory
      = (((ARG_REF (4)) == SHARP_F) ? 0 : (STRING_ARG (4)));
    enum process_ctty_type ctty_type;
    char * ctty_name = 0;
    enum process_channel_type channel_in_type;
    Tchannel channel_in = NO_CHANNEL;
    enum process_channel_type channel_out_type;
    Tchannel channel_out = NO_CHANNEL;
    enum process_channel_type channel_err_type;
    Tchannel channel_err = NO_CHANNEL;

    if ((ARG_REF (5)) == SHARP_F)
      ctty_type = process_ctty_type_none;
    else if ((ARG_REF (5)) == (LONG_TO_FIXNUM (-1)))
      ctty_type = process_ctty_type_inherit_bg;
    else if ((ARG_REF (5)) == (LONG_TO_FIXNUM (-2)))
      ctty_type = process_ctty_type_inherit_fg;
    else
      {
	ctty_type = process_ctty_type_explicit;
	ctty_name = (STRING_ARG (5));
      }
    PROCESS_CHANNEL_ARG (6, channel_in_type, channel_in);
    PROCESS_CHANNEL_ARG (7, channel_out_type, channel_out);
    PROCESS_CHANNEL_ARG (8, channel_err_type, channel_err);
    {
      Tprocess process =
	(OS_make_subprocess
	 (filename, argv, env, working_directory,
	  ctty_type, ctty_name,
	  channel_in_type, channel_in,
	  channel_out_type, channel_out,
	  channel_err_type, channel_err));
      dstack_set_position (position);
      PRIMITIVE_RETURN (long_to_integer (process));
    }
  }
}

static CONST char **
DEFUN (string_vector_arg, (arg), int arg)
{
  SCHEME_OBJECT vector = (ARG_REF (arg));
  if (!string_vector_p (vector))
    error_wrong_type_arg (arg);
  return (convert_string_vector (vector));
}

static int
DEFUN (string_vector_p, (vector), SCHEME_OBJECT vector)
{
  if (! (VECTOR_P (vector)))
    return (0);
  {
    unsigned long length = (VECTOR_LENGTH (vector));
    SCHEME_OBJECT * scan = (VECTOR_LOC (vector, 0));
    SCHEME_OBJECT * end = (scan + length);
    while (scan < end)
      if (! (STRING_P (*scan++)))
	return (0);
  }
  return (1);
}

static CONST char **
DEFUN (convert_string_vector, (vector), SCHEME_OBJECT vector)
{
  unsigned long length = (VECTOR_LENGTH (vector));
  char ** result = (dstack_alloc ((length + 1) * (sizeof (char *))));
  SCHEME_OBJECT * scan = (VECTOR_LOC (vector, 0));
  SCHEME_OBJECT * end = (scan + length);
  char ** scan_result = result;
  while (scan < end)
    (*scan_result++) = ((char *) (STRING_LOC ((*scan++), 0)));
  (*scan_result) = 0;
  return ((CONST char **) result);
}
