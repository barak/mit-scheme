/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosfile.c,v 1.4 1992/04/14 18:36:17 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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
#include "osfile.h"

extern Tchannel EXFUN (arg_channel, (int));

#ifndef OPEN_FILE_HOOK
#define OPEN_FILE_HOOK(channel)
#endif

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

DEFINE_PRIMITIVE ("FILE-OPEN-BINARY-INPUT-CHANNEL", Prim_file_open_binary_input_channel, 1, 1,
  "Open an input file called FILENAME, in binary mode, returning a channel number.")
  OPEN_FILE_PRIMITIVE (OS_open_load_file)

DEFINE_PRIMITIVE ("FILE-OPEN-BINARY-OUTPUT-CHANNEL", Prim_file_open_binary_output_channel, 1, 1,
  "Open an output file called FILENAME, in binary mode,\n\
returning a channel number.  If the file exists, it is rewritten.")
  OPEN_FILE_PRIMITIVE (OS_open_dump_file)

DEFINE_PRIMITIVE ("FILE-OPEN-IO-CHANNEL", Prim_file_open_io_channel, 1, 1,
  "Open a file called FILENAME, returning a channel number.\n\
The file is opened for both input and output.\n\
If the file exists, its contents are not disturbed.")
  OPEN_FILE_PRIMITIVE (OS_open_io_file)

DEFINE_PRIMITIVE ("FILE-OPEN-APPEND-CHANNEL", Prim_file_open_append_channel, 1, 1,
  "Open an output file called FILENAME, returning a channel number.\n\
If the file exists, output is appended to its contents.")
  OPEN_FILE_PRIMITIVE (OS_open_append_file)

DEFINE_PRIMITIVE ("FILE-OPEN-CHANNEL", Prim_file_open_channel, 2, 2,
  "This is an obsolete primitive.\n\
Open a file called FILENAME, returning a channel number.\n\
Second argument MODE says how to open the file:\n\
  #F        ==> open for input;\n\
  #T        ==> open for output, rewriting file if it exists;\n\
  otherwise ==> open for output, appending to existing file.")
{
  PRIMITIVE_HEADER (2);
  {
    CONST char * filename = (STRING_ARG (1));
    fast SCHEME_OBJECT mode = (ARG_REF (2));
    fast Tchannel channel =
      ((mode == SHARP_F)
       ? (OS_open_input_file (filename))
       : (mode == SHARP_T)
       ? (OS_open_output_file (filename))
       : (OS_open_append_file (filename)));
    OPEN_FILE_HOOK (channel);
    PRIMITIVE_RETURN (long_to_integer (channel));
  }
}

DEFINE_PRIMITIVE ("FILE-LENGTH-NEW", Prim_file_length, 1, 1,
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
