/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosio.c,v 1.3 1991/01/24 05:30:26 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"

#ifndef CLOSE_CHANNEL_HOOK
#define CLOSE_CHANNEL_HOOK(channel)
#endif

static Tchannel
DEFUN (arg_to_channel, (argument, arg_number),
       SCHEME_OBJECT argument AND
       int arg_number)
{
  if (! ((INTEGER_P (argument)) && (integer_to_long_p (argument))))
    error_wrong_type_arg (arg_number);
  {
    fast long channel = (integer_to_long (argument));
    if (! ((channel >= 0) || (channel < OS_channel_table_size)))
      error_wrong_type_arg (arg_number);
    return (channel);
  }
}

Tchannel
DEFUN (arg_channel, (arg_number), int arg_number)
{
  fast Tchannel channel =
    (arg_to_channel ((ARG_REF (arg_number)), arg_number));
  if (! (OS_channel_open_p (channel)))
    error_bad_range_arg (arg_number);
  return (channel);
}

Tchannel
DEFUN (arg_channel_old, (arg_number), int arg_number)
{
  fast SCHEME_OBJECT argument = (ARG_REF (arg_number));
  if ((OBJECT_TYPE (argument)) != TC_HUNK3)
    error_wrong_type_arg (arg_number);
  {
    fast Tchannel channel =
      (arg_to_channel ((MEMORY_REF (argument, 0)), arg_number));
    if (! (OS_channel_open_p (channel)))
      error_bad_range_arg (arg_number);
    return (channel);
  }
}

DEFINE_PRIMITIVE ("CHANNEL-CLOSE", Prim_channel_close, 1, 1,
  "Close file CHANNEL-NUMBER.")
{
  PRIMITIVE_HEADER (1);
  {
    fast Tchannel channel = (arg_to_channel ((ARG_REF (1)), 1));
    if (OS_channel_open_p (channel))
      {
	CLOSE_CHANNEL_HOOK (channel);
	OS_channel_close (channel);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CHANNEL-TABLE", Prim_channel_table, 0, 0,
  "Return a vector of all channels in the channel table.")
{
  PRIMITIVE_HEADER (0);
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      if (OS_channel_open_p (channel))
	obstack_grow ((&scratch_obstack), (&channel), (sizeof (Tchannel)));
  }
  {
    unsigned int n_channels =
      ((obstack_object_size ((&scratch_obstack))) / (sizeof (Tchannel)));
    if (n_channels == 0)
      PRIMITIVE_RETURN (SHARP_F);
    {
      Tchannel * channels = (obstack_finish (&scratch_obstack));
      Tchannel * scan_channels = channels;
      SCHEME_OBJECT vector =
	(allocate_marked_vector (TC_VECTOR, n_channels, 1));
      SCHEME_OBJECT * scan_vector = (VECTOR_LOC (vector, 0));
      SCHEME_OBJECT * end_vector = (scan_vector + n_channels);
      while (scan_vector < end_vector)
	(*scan_vector++) = (long_to_integer (*scan_channels++));
      obstack_free ((&scratch_obstack), channels);
      PRIMITIVE_RETURN (vector);
    }
  }
}

DEFINE_PRIMITIVE ("CHANNEL-TYPE", Prim_channel_type, 1, 1,
  "Return (as a nonnegative integer) the type of CHANNEL.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer ((long) (OS_channel_type (arg_channel (1)))));
}

DEFINE_PRIMITIVE ("CHANNEL-READ", Prim_channel_read, 4, 4,
  "Read characters from CHANNEL, storing them in STRING.\n\
Third and fourth args START and END specify the substring to use.\n\
Attempt to fill that substring unless end-of-file is reached.\n\
Return the number of characters actually read from CHANNEL.")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT buffer = (ARG_REF (2));
    long length = (STRING_LENGTH (buffer));
    long end = (arg_index_integer (4, (length + 1)));
    long start = (arg_index_integer (3, (end + 1)));
    long nread =
      (OS_channel_read ((arg_channel (1)),
			(STRING_LOC (buffer, start)),
			(end - start)));
    PRIMITIVE_RETURN ((nread < 0) ? SHARP_F : (long_to_integer (nread)));
  }
}

DEFINE_PRIMITIVE ("CHANNEL-WRITE", Prim_channel_write, 4, 4,
  "Write characters to CHANNEL, reading them from STRING.\n\
Third and fourth args START and END specify the substring to use.")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT buffer = (ARG_REF (2));
    long length = (STRING_LENGTH (buffer));
    long end = (arg_index_integer (4, (length + 1)));
    long start = (arg_index_integer (3, (end + 1)));
    long nwritten =
      (OS_channel_write ((arg_channel (1)),
			 (STRING_LOC (buffer, start)),
			 (end - start)));
    PRIMITIVE_RETURN ((nwritten < 0) ? SHARP_F : (long_to_integer (nwritten)));
  }
}

DEFINE_PRIMITIVE ("CHANNEL-BLOCKING?", Prim_channel_blocking_p, 1, 1,
  "Return #F iff CHANNEL is in non-blocking mode.\n\
Otherwise, CHANNEL is in blocking mode.\n\
If CHANNEL can be put in non-blocking mode, #T is returned.\n\
If it cannot, 0 is returned.")
{
  PRIMITIVE_HEADER (1);
  {
    int result = (OS_channel_nonblocking_p (arg_channel (1)));
    PRIMITIVE_RETURN
      ((result < 0)
       ? (LONG_TO_UNSIGNED_FIXNUM (0))
       : (BOOLEAN_TO_OBJECT (result == 0)));
  }
}

DEFINE_PRIMITIVE ("CHANNEL-NONBLOCKING", Prim_channel_nonblocking, 1, 1,
  "Put CHANNEL in non-blocking mode.")
{
  PRIMITIVE_HEADER (1);
  OS_channel_nonblocking (arg_channel (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CHANNEL-BLOCKING", Prim_channel_blocking, 1, 1,
  "Put CHANNEL in blocking mode.")
{
  PRIMITIVE_HEADER (1);
  OS_channel_blocking (arg_channel (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
