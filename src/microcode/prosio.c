/* -*-C-*-

$Id: 42aafbe545d36dd6647680c3d6e536b39a6290a7 $

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

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"

#ifndef CLOSE_CHANNEL_HOOK
#define CLOSE_CHANNEL_HOOK(channel)
#endif

Tchannel
arg_to_channel (SCHEME_OBJECT argument, int arg_number)
{
  unsigned long channel = (arg_ulong_integer (arg_number));
  if (! (channel < OS_channel_table_size))
    error_wrong_type_arg (arg_number);
  return (channel);
}

Tchannel
arg_channel (int arg_number)
{
  Tchannel channel = (arg_to_channel ((ARG_REF (arg_number)), arg_number));
  if (!OS_channel_open_p (channel))
    error_bad_range_arg (arg_number);
  return (channel);
}

DEFINE_PRIMITIVE ("CHANNEL-CLOSE", Prim_channel_close, 1, 1,
  "Close file CHANNEL-NUMBER.")
{
  PRIMITIVE_HEADER (1);
  {
    Tchannel channel = (arg_to_channel ((ARG_REF (1)), 1));
    if (OS_channel_open_p (channel))
      {
	CLOSE_CHANNEL_HOOK (channel);
	OS_channel_close (channel);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CHANNEL-SYNCHRONIZE", Prim_channel_synchronize, 1, 1,
  "(CHANNEL)\n\
Synchronize CHANNEL with any permanent storage associated with it,\n\
forcing any buffered data to be written permanently.")
{
  PRIMITIVE_HEADER (1);
  OS_channel_synchronize (arg_channel (1));
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
  PRIMITIVE_RETURN (char_pointer_to_string (channel_type_names [index]));
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
    unsigned char * buffer = (arg_extended_string (2, (&length)));
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
    const unsigned char * buffer = (arg_extended_string (2, (&length)));
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

DEFINE_PRIMITIVE ("NEW-MAKE-PIPE", Prim_new_make_pipe, 2, 2,
  "Store the reader and writer of a new pipe in the cdrs of weak pairs.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, WEAK_PAIR_P);
  CHECK_ARG (2, WEAK_PAIR_P);
  {
    Tchannel reader;
    Tchannel writer;
    OS_make_pipe ((&reader), (&writer));
    SET_PAIR_CDR ((ARG_REF (1)), (long_to_integer (reader)));
    SET_PAIR_CDR ((ARG_REF (2)), (long_to_integer (writer)));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

/* Select registry */

static select_registry_t
arg_select_registry (int arg_number)
{
  return ((select_registry_t) (arg_ulong_integer (arg_number)));
}

static unsigned int
arg_sr_mode (int arg_number)
{
  unsigned long n = (arg_ulong_integer (arg_number));
  if (! ((n >= 1) && (n <= 3)))
    error_bad_range_arg (arg_number);
  return (n);
}

DEFINE_PRIMITIVE ("HAVE-SELECT?", Prim_have_select_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_have_select_p));
}

DEFINE_PRIMITIVE ("ALLOCATE-SELECT-REGISTRY", Prim_alloc_selreg, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (ulong_to_integer
     ((unsigned long) (OS_allocate_select_registry ())));
}

DEFINE_PRIMITIVE ("DEALLOCATE-SELECT-REGISTRY", Prim_dealloc_selreg, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_deallocate_select_registry (arg_select_registry (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ADD-TO-SELECT-REGISTRY", Prim_add_to_selreg, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS_add_to_select_registry ((arg_select_registry (1)),
			     (arg_nonnegative_integer (2)),
			     (arg_sr_mode (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("REMOVE-FROM-SELECT-REGISTRY", Prim_rem_from_selreg, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS_remove_from_select_registry ((arg_select_registry (1)),
				  (arg_nonnegative_integer (2)),
				  (arg_sr_mode (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-LENGTH", Prim_selreg_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS_select_registry_length (arg_select_registry (1))));
}

DEFINE_PRIMITIVE ("TEST-SELECT-REGISTRY", Prim_test_selreg, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    select_registry_t r = (arg_select_registry (1));
    unsigned int rl = (OS_select_registry_length (r));
    int blockp = (BOOLEAN_ARG (2));
    SCHEME_OBJECT vfd = (VECTOR_ARG (3));
    SCHEME_OBJECT vmode = (VECTOR_ARG (4));
    int result;

    if ((VECTOR_LENGTH (vfd)) < rl)
      error_bad_range_arg (3);
    if ((VECTOR_LENGTH (vmode)) < rl)
      error_bad_range_arg (4);
    result = (OS_test_select_registry (r, blockp));
    if (result > 0)
      {
	unsigned int i = 0;
	unsigned int iv = 0;
	while (i < rl)
	  {
	    int fd;
	    unsigned int mode;

	    OS_select_registry_result (r, i, (&fd), (&mode));
	    if (mode > 0)
	      {
		VECTOR_SET (vfd, iv, (long_to_integer (fd)));
		VECTOR_SET (vmode, iv, (ulong_to_integer (mode)));
		iv += 1;
	      }
	    i += 1;
	  }
      }
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}

DEFINE_PRIMITIVE ("TEST-SELECT-DESCRIPTOR", Prim_test_sel_desc, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (long_to_integer
     (OS_test_select_descriptor ((arg_nonnegative_integer (1)),
				 (BOOLEAN_ARG (2)),
				 (arg_sr_mode (3)))));
}
