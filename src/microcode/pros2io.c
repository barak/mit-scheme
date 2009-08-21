/* -*-C-*-

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

#include "scheme.h"
#include "prims.h"
#include "os2.h"
#include "os2proc.h"

extern qid_t OS2_channel_thread_descriptor (Tchannel);

DEFINE_PRIMITIVE ("OS2-SELECT-REGISTRY-LUB", Prim_OS2_select_registry_lub, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (QID_MAX + 1));
}

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    Tchannel channel = (arg_channel (1));
    PRIMITIVE_RETURN
      (LONG_TO_UNSIGNED_FIXNUM
       ((CHANNEL_ABSTRACT_P (channel))
	? (OS2_channel_thread_descriptor (channel))
	: QID_NONE));
  }
}

static qid_t
arg_qid (int arg_number)
{
  unsigned int qid = (arg_index_integer (arg_number, (QID_MAX + 1)));
  if (!OS2_qid_openp (qid))
    error_bad_range_arg (arg_number);
  return (qid);
}

DEFINE_PRIMITIVE ("OS2-SELECT-DESCRIPTOR", Prim_OS2_select_descriptor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  switch (OS2_message_availablep ((arg_qid (1)), (BOOLEAN_ARG (2))))
    {
    case mat_available:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case mat_not_available:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case mat_interrupt:
      if (OS_process_any_status_change ())
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
      else
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    default:
      error_external_return ();
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
}

DEFINE_PRIMITIVE ("OS2-SELECT-REGISTRY-TEST", Prim_OS2_select_registry_test, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, STRING_P);
  if ((STRING_LENGTH (ARG_REF (1))) != (QID_MAX + 1))
    error_bad_range_arg (1);
  CHECK_ARG (2, STRING_P);
  if ((STRING_LENGTH (ARG_REF (1))) != (QID_MAX + 1))
    error_bad_range_arg (2);
  {
    char * registry = (STRING_POINTER (ARG_REF (1)));
    char * results = (STRING_POINTER (ARG_REF (2)));
    int blockp = (BOOLEAN_ARG (3));
    int inputp = 0;
    int interruptp = 0;
    qid_t qid;

    while (1)
      {
	for (qid = 0; (qid <= QID_MAX); qid += 1)
	  {
	    (results [qid]) = 0;
	    if ((registry [qid]) != 0)
	      switch (OS2_message_availablep (qid, 0))
		{
		case mat_available:
		  inputp = 1;
		  (results [qid]) = 1;
		  break;
		case mat_interrupt:
		  interruptp = 1;
		  break;
		}
	  }
	if ((!blockp) || inputp || interruptp)
	  break;
	if ((OS2_scheme_tqueue_block ()) == mat_interrupt)
	  interruptp = 1;
      }
    if (inputp)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    else if (!interruptp)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    else if (!OS_process_any_status_change ())
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    else
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
  }
}

#define PROCESS_CHANNEL_ARG(arg, type, channel)				\
{									\
  if ((ARG_REF (arg)) == SHARP_F)					\
    (type) = process_channel_type_none;					\
  else if ((ARG_REF (arg)) == (LONG_TO_FIXNUM (-1)))			\
    (type) = process_channel_type_inherit;				\
  else									\
    {									\
      (type) = process_channel_type_explicit;				\
      (channel) = (arg_channel (arg));					\
    }									\
}

DEFINE_PRIMITIVE ("OS2-MAKE-SUBPROCESS", Prim_OS2_make_subprocess, 7, 7,
  "(FILENAME CMD-LINE ENV WORK-DIR STDIN STDOUT STDERR)\n\
Create a subprocess.\n\
FILENAME is the program to run.\n\
CMD-LINE a string containing the program's invocation.\n\
ENV is a string to pass as the program's environment;\n\
  #F means inherit Scheme's environment.\n\
WORK-DIR is a string to pass as the program's working directory;\n\
  #F means inherit Scheme's working directory.\n\
STDIN is the input channel for the subprocess.\n\
STDOUT is the output channel for the subprocess.\n\
STDERR is the error channel for the subprocess.\n\
  Each channel arg can take these values:\n\
  #F means none;\n\
  -1 means use the corresponding channel from Scheme;\n\
  otherwise the argument must be a channel.")
{
  PRIMITIVE_HEADER (7);
  {
    const char * filename = (STRING_ARG (1));
    const char * command_line = (STRING_ARG (2));
    const char * env = (((ARG_REF (3)) == SHARP_F) ? 0 : (STRING_ARG (3)));
    const char * working_directory
      = (((ARG_REF (4)) == SHARP_F) ? 0 : (STRING_ARG (4)));
    enum process_channel_type channel_in_type;
    Tchannel channel_in;
    enum process_channel_type channel_out_type;
    Tchannel channel_out;
    enum process_channel_type channel_err_type;
    Tchannel channel_err;

    PROCESS_CHANNEL_ARG (5, channel_in_type, channel_in);
    PROCESS_CHANNEL_ARG (6, channel_out_type, channel_out);
    PROCESS_CHANNEL_ARG (7, channel_err_type, channel_err);
    PRIMITIVE_RETURN
      (long_to_integer
       (OS2_make_subprocess
	(filename, command_line, env, working_directory,
	 channel_in_type, channel_in,
	 channel_out_type, channel_out,
	 channel_err_type, channel_err)));
  }
}
