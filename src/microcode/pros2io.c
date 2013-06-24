/* -*-C-*-

$Id: pros2io.c,v 1.9 2000/12/05 21:23:47 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
    if (! ((CHANNEL_ABSTRACT_P (channel)) && (CHANNEL_INPUTP (channel))))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN
      (LONG_TO_UNSIGNED_FIXNUM (OS2_channel_thread_descriptor (channel)));
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
    char * registry = (STRING_LOC ((ARG_REF (1)), 0));
    char * results = (STRING_LOC ((ARG_REF (2)), 0));
    int blockp = (BOOLEAN_ARG (3));
    int inputp = 0;
    int interruptp = 0;
    qid_t qid;
    int n;

    /* This first phase checks the qid subqueues and OS2_scheme_tqueue
       for any previously-queued input.  */
  check_for_input:
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
    /* This second phase waits for input if necessary.  It does not
       check the subqueues for previously-stored data, so it's
       important that we already did this.  Otherwise we could end up
       waiting for input when there was valid input ready.  */
    if (blockp)
      while (! (inputp || interruptp))
	{
	  for (qid = 0; (qid <= QID_MAX); qid += 1)
	    (OS2_scheme_tqueue_avail_map [qid]) = 0;
	  n = (OS2_tqueue_select (OS2_scheme_tqueue, blockp));
	  if (n == (-1))
	    /* If we're unblocked and there's no message in the
	       tqueue, go back and check for input again.  */
	    goto check_for_input;
	  if (n < 0)
	    interruptp = 1;
	  else
	    for (qid = 0; (qid <= QID_MAX); qid += 1)
	      if (((registry [qid]) != 0)
		  && (OS2_scheme_tqueue_avail_map [qid]))
		{
		  inputp = 1;
		  (results [qid]) = 1;
		}
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
