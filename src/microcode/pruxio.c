/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Primitives to make Unix subprocesses.  This file is misnamed for
   hysterical raisins -- it should be pruxproc.c.  */

#include "scheme.h"
#include "prims.h"
#include "osio.h"
#include "ux.h"
#include "uxproc.h"

extern int UX_channel_descriptor (Tchannel channel);

static const char ** string_vector_arg (int arg);
static int string_vector_p (SCHEME_OBJECT vector);
static const char ** convert_string_vector (SCHEME_OBJECT vector);

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (UX_channel_descriptor (arg_channel (1))));
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
    void * position = dstack_position;
    const char * filename = (STRING_ARG (1));
    const char ** argv = (string_vector_arg (2));
    const char ** env
      = (((ARG_REF (3)) == SHARP_F) ? 0 : (string_vector_arg (3)));
    const char * working_directory
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

static const char **
string_vector_arg (int arg)
{
  SCHEME_OBJECT vector = (ARG_REF (arg));
  if (!string_vector_p (vector))
    error_wrong_type_arg (arg);
  return (convert_string_vector (vector));
}

static int
string_vector_p (SCHEME_OBJECT vector)
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

static const char **
convert_string_vector (SCHEME_OBJECT vector)
{
  unsigned long length = (VECTOR_LENGTH (vector));
  char ** result = (dstack_alloc ((length + 1) * (sizeof (char *))));
  SCHEME_OBJECT * scan = (VECTOR_LOC (vector, 0));
  SCHEME_OBJECT * end = (scan + length);
  char ** scan_result = result;
  while (scan < end)
    (*scan_result++) = (STRING_POINTER (*scan++));
  (*scan_result) = 0;
  return ((const char **) result);
}
