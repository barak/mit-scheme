/* -*-C-*-

$Id: prosfile.c,v 1.11 2003/02/14 18:28:23 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osfile.h"

extern Tchannel EXFUN (arg_channel, (int));

#ifndef OPEN_FILE_HOOK
#define OPEN_FILE_HOOK(channel)
#endif

#define NEW_OPEN_FILE_PRIMITIVE(OS_open_file)				\
{									\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (2, WEAK_PAIR_P);						\
  {									\
    Tchannel channel = (OS_open_file (STRING_ARG (1)));			\
    OPEN_FILE_HOOK (channel);						\
    SET_PAIR_CDR ((ARG_REF (2)), (long_to_integer (channel)));		\
    PRIMITIVE_RETURN (SHARP_T);						\
  }									\
}

DEFINE_PRIMITIVE ("NEW-FILE-OPEN-INPUT-CHANNEL", Prim_new_file_open_input_channel, 2, 2,
  "Open an input file called FILENAME.\n\
The channel number is saved in the cdr of WEAK-PAIR.")
  NEW_OPEN_FILE_PRIMITIVE (OS_open_input_file)

DEFINE_PRIMITIVE ("NEW-FILE-OPEN-OUTPUT-CHANNEL", Prim_new_file_open_output_channel, 2, 2,
  "Open an output file called FILENAME.\n\
The channel number is saved in the cdr of WEAK-PAIR.\n\
If the file exists, it is rewritten.")
  NEW_OPEN_FILE_PRIMITIVE (OS_open_output_file)

DEFINE_PRIMITIVE ("NEW-FILE-OPEN-IO-CHANNEL", Prim_new_file_open_io_channel, 2, 2,
  "Open a file called FILENAME.\n\
The channel number is saved in the cdr of WEAK-PAIR.\n\
The file is opened for both input and output.\n\
If the file exists, its contents are not disturbed.")
  NEW_OPEN_FILE_PRIMITIVE (OS_open_io_file)

DEFINE_PRIMITIVE ("NEW-FILE-OPEN-APPEND-CHANNEL", Prim_new_file_open_append_channel, 2, 2,
  "Open an output file called FILENAME.\n\
The channel number is saved in the cdr of WEAK-PAIR.\n\
If the file exists, output is appended to its contents.")
  NEW_OPEN_FILE_PRIMITIVE (OS_open_append_file)

#define OPEN_FILE_PRIMITIVE(OS_open_file)				\
{									\
  PRIMITIVE_HEADER (1);							\
  {									\
    Tchannel channel = (OS_open_file (STRING_ARG (1)));			\
    OPEN_FILE_HOOK (channel);						\
    PRIMITIVE_RETURN (long_to_integer (channel));			\
  }									\
}

DEFINE_PRIMITIVE ("FILE-OPEN-INPUT-CHANNEL", Prim_file_open_input_channel, 1, 1,
  "Open an input file called FILENAME, returning a channel number.")
  OPEN_FILE_PRIMITIVE (OS_open_input_file)

DEFINE_PRIMITIVE ("FILE-OPEN-OUTPUT-CHANNEL", Prim_file_open_output_channel, 1, 1,
  "Open an output file called FILENAME, returning a channel number.\n\
If the file exists, it is rewritten.")
  OPEN_FILE_PRIMITIVE (OS_open_output_file)

DEFINE_PRIMITIVE ("FILE-OPEN-IO-CHANNEL", Prim_file_open_io_channel, 1, 1,
  "Open a file called FILENAME, returning a channel number.\n\
The file is opened for both input and output.\n\
If the file exists, its contents are not disturbed.")
  OPEN_FILE_PRIMITIVE (OS_open_io_file)

DEFINE_PRIMITIVE ("FILE-OPEN-APPEND-CHANNEL", Prim_file_open_append_channel, 1, 1,
  "Open an output file called FILENAME, returning a channel number.\n\
If the file exists, output is appended to its contents.")
  OPEN_FILE_PRIMITIVE (OS_open_append_file)

DEFINE_PRIMITIVE ("FILE-LENGTH-NEW", Prim_file_length_new, 1, 1,
  "Return the length of CHANNEL in characters.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_file_length (arg_channel (1))));
}

DEFINE_PRIMITIVE ("FILE-POSITION", Prim_file_position, 1, 1,
  "Return the position of CHANNEL's file-pointer.\n\
This is a non-negative number strictly less than the file's length.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_file_position (arg_channel (1))));
}

DEFINE_PRIMITIVE ("FILE-SET-POSITION", Prim_file_set_position, 2, 2,
  "Set the file-pointer of CHANNEL to POSITION.\n\
POSITION must be a non-negative number strictly less than the file's length.")
{
  PRIMITIVE_HEADER (1);
  OS_file_set_position ((arg_channel (1)), (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
