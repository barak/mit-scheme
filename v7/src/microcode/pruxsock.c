/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxsock.c,v 1.1 1990/06/20 19:38:47 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

/* Primitives for socket control. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"

#ifdef HAVE_SOCKETS

#include "uxsock.h"

DEFINE_PRIMITIVE ("GET-SERVICE-BY-NAME", Prim_get_service_by_name, 2, 2,
  "Given SERVICE-NAME and PROTOCOL-NAME, return a port number.\n\
The result is a nonnegative integer, or #F if no such service exists.")
{
  PRIMITIVE_HEADER (2);
  {
    int result = (OS_get_service_by_name ((STRING_ARG (1)), (STRING_ARG (2))));
    return ((result < 0) ? SHARP_F : (long_to_integer (result)));
  }
}

DEFINE_PRIMITIVE ("GET-HOST-BY-NAME", Prim_get_host_by_name, 1, 1,
  "Given HOST-NAME, return its internet host numbers.\n\
The result is a vector of nonnegative integers, or #F if no such host exists.")
{
  PRIMITIVE_HEADER (1);
  {
    struct host_addresses * result = (OS_get_host_by_name (STRING_ARG (1)));
    if (result == 0)
      PRIMITIVE_RETURN (SHARP_F);
    {
      int length = (result -> address_length);
      char ** scan = (result -> addresses);
      char ** end = scan;
      while ((*end++) != 0) ;
      end -= 1;
      {
	SCHEME_OBJECT result =
	  (allocate_marked_vector (TC_VECTOR, (end - scan), 1));
	SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
	while (scan < end)
	  (*scan_result++) = (memory_to_string (length, (*scan++)));
	PRIMITIVE_RETURN (result);
      }
    }
  }
}

DEFINE_PRIMITIVE ("OPEN-TCP-STREAM-SOCKET", Prim_open_tcp_stream_socket, 2, 2,
  "Given HOST-ADDRESS and PORT-NUMBER, open and return a TCP stream socket.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (long_to_integer
     (OS_open_tcp_stream_socket ((STRING_ARG (1)),
				 (arg_nonnegative_integer (2)))));
}

#ifdef HAVE_UNIX_SOCKETS

DEFINE_PRIMITIVE ("OPEN-UNIX-STREAM-SOCKET", Prim_open_unix_stream_socket, 1, 1,
  "Open the unix stream socket FILENAME.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (OS_open_unix_stream_socket (STRING_ARG (1))));
}

#endif /* HAVE_UNIX_SOCKETS */

#endif /* HAVE_SOCKETS */
