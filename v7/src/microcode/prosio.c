/* -*-C-*-

$Id: prosio.c,v 1.19 2002/11/20 19:46:13 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"

#ifndef CLOSE_CHANNEL_HOOK
#define CLOSE_CHANNEL_HOOK(channel)
#endif

Tchannel
DEFUN (arg_to_channel, (argument, arg_number),
       SCHEME_OBJECT argument AND
       int arg_number)
{
  if (! ((INTEGER_P (argument)) && (integer_to_long_p (argument))))
    error_wrong_type_arg (arg_number);
  {
    fast long channel = (integer_to_long (argument));
    if (! ((channel >= 0) || (channel < ((long) OS_channel_table_size))))
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

/* Must match definition of `enum channel_type' in "osio.h".  */
static char * channel_type_names [] =
{
  "unknown",
  "file",
  "unix-pipe",
  "unix-fifo",
  "terminal",
  "unix-pty-master",
  "unix-stream-socket",
  "tcp-stream-socket",
  "tcp-server-socket",
  "directory",
  "unix-character-device",
  "unix-block-device",
  "os/2-console",
  "os/2-unnamed-pipe",
  "os/2-named-pipe",
  "win32-anonymous-pipe",
  "win32-named-pipe"
};

DEFINE_PRIMITIVE ("CHANNEL-TYPE-NAME", Prim_channel_type_name, 1, 1,
  "Return (as a string) the type of CHANNEL.")
{
  enum channel_type type;
  unsigned int index;
  PRIMITIVE_HEADER (1);
  type = (OS_channel_type (arg_channel (1)));
  if (type == channel_type_unknown)
    PRIMITIVE_RETURN (SHARP_F);
  index = ((unsigned int) type);
  if (index >= ((sizeof (channel_type_names)) / (sizeof (char *))))
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN
    (char_pointer_to_string ((unsigned char *) (channel_type_names [index])));
}

DEFINE_PRIMITIVE ("CHANNEL-READ", Prim_channel_read, 4, 4,
  "Read characters from CHANNEL, storing them in STRING.\n\
Third and fourth args START and END specify the substring to use.\n\
Attempt to fill that substring unless end-of-file is reached.\n\
Return the number of characters actually read from CHANNEL.")
{
  PRIMITIVE_HEADER (4);
  {
    unsigned long length;
    char * buffer = (arg_extended_string (2, (&length)));
    unsigned long end = (arg_ulong_index_integer (4, (length + 1)));
    unsigned long start = (arg_ulong_index_integer (3, (end + 1)));
    long nread =
      (OS_channel_read ((arg_channel (1)),
			(buffer + start),
			(end - start)));
    PRIMITIVE_RETURN ((nread < 0) ? SHARP_F : (long_to_integer (nread)));
  }
}

DEFINE_PRIMITIVE ("CHANNEL-WRITE", Prim_channel_write, 4, 4,
  "Write characters to CHANNEL, reading them from STRING.\n\
Third and fourth args START and END specify the substring to use.")
{
  PRIMITIVE_HEADER (4);
  {
    unsigned long length;
    CONST char * buffer = (arg_extended_string (2, (&length)));
    unsigned long end = (arg_ulong_index_integer (4, (length + 1)));
    unsigned long start = (arg_ulong_index_integer (3, (end + 1)));
    long nwritten =
      (OS_channel_write ((arg_channel (1)),
			 (buffer + start),
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

DEFINE_PRIMITIVE ("MAKE-PIPE", Prim_make_pipe, 0, 0,
  "Return a cons of two channels, the reader and writer of a pipe.")
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT result = (cons (SHARP_F, SHARP_F));
    Tchannel reader;
    Tchannel writer;
    OS_make_pipe ((&reader), (&writer));
    SET_PAIR_CAR (result, (long_to_integer (reader)));
    SET_PAIR_CDR (result, (long_to_integer (writer)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("HAVE-SELECT?", Prim_have_select_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_have_select_p));
}
