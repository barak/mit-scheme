/* -*-C-*-

$Id: pruxsock.c,v 1.27 2007/01/20 23:49:18 riastradh Exp $

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

/* Primitives for socket control. */

#include "scheme.h"
#include "prims.h"

/* This obtains the HAVE_SOCKETS definition.  */
#ifdef __unix__
#  include "ux.h"
#endif

/* Under OS/2, socket support is the default but can be disabled.  */
#ifdef __OS2__
#  ifndef DISABLE_SOCKET_SUPPORT
#    define HAVE_SOCKETS 1
#    define HAVE_UNIX_SOCKETS 1
#  endif
#endif

/* Under Win32, socket support is the default but can be disabled.  */
#ifdef __WIN32__
#  ifndef DISABLE_SOCKET_SUPPORT
#    define HAVE_SOCKETS 1
#    undef HAVE_UNIX_SOCKETS
#  endif
#endif

#ifdef HAVE_SOCKETS

#include "uxsock.h"
#define SOCKET_CODE(code) code

static PTR
DEFUN (arg_host, (arg), unsigned int arg)
{
  CHECK_ARG (arg, STRING_P);
  if ((STRING_LENGTH (ARG_REF (arg))) != (OS_host_address_length ()))
    error_bad_range_arg (arg);
  return (STRING_LOC ((ARG_REF (arg)), 0));
}

static Tchannel
DEFUN (arg_client_socket, (arg), unsigned int arg)
{
  Tchannel socket = (arg_nonnegative_integer (arg));
  if (! (((OS_channel_type (socket)) == channel_type_tcp_stream_socket)
	 || ((OS_channel_type (socket)) == channel_type_unix_stream_socket)))
    error_bad_range_arg (arg);
  return (socket);
}

static Tchannel
DEFUN (arg_server_socket, (arg), unsigned int arg)
{
  Tchannel server_socket = (arg_nonnegative_integer (arg));
  if ((OS_channel_type (server_socket)) != channel_type_tcp_server_socket)
    error_bad_range_arg (arg);
  return (server_socket);
}

#else /* not HAVE_SOCKETS */

#define SOCKET_CODE(code)						\
{									\
  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);		\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

#endif /* not HAVE_SOCKETS */

DEFINE_PRIMITIVE ("GET-SERVICE-BY-NAME", Prim_get_service_by_name, 2, 2,
  "Given SERVICE-NAME and PROTOCOL-NAME, return a port number.\n\
The result is a nonnegative integer, or #F if no such service exists.")
{
  PRIMITIVE_HEADER (2);
  SOCKET_CODE
    ({
      int result
	= (OS_get_service_by_name ((STRING_ARG (1)), (STRING_ARG (2))));
      PRIMITIVE_RETURN ((result < 0) ? SHARP_F : (long_to_integer (result)));
     });
}

DEFINE_PRIMITIVE ("GET-SERVICE-BY-NUMBER", Prim_get_service_by_number, 1, 1,
  "Given PORT-NUMBER, return it in the network encoding.")
{
  PRIMITIVE_HEADER (1);
  SOCKET_CODE
    ({
      PRIMITIVE_RETURN
	(ulong_to_integer (OS_get_service_by_number (arg_ulong_integer (1))));
     });
}

DEFINE_PRIMITIVE ("HOST-ADDRESS-LENGTH", Prim_host_address_length, 0, 0,
  "The length of a host address string, in characters.")
{
  PRIMITIVE_HEADER (0);
  SOCKET_CODE
    ({
      PRIMITIVE_RETURN (long_to_integer (OS_host_address_length ()));
    });
}

DEFINE_PRIMITIVE ("GET-HOST-BY-NAME", Prim_get_host_by_name, 1, 1,
  "Given HOST-NAME, return its internet host numbers.\n\
The result is a vector of strings, or #F if no such host exists.")
{
  PRIMITIVE_HEADER (1);
  SOCKET_CODE
    ({
      char ** addresses = (OS_get_host_by_name (STRING_ARG (1)));
      if (addresses == 0)
	PRIMITIVE_RETURN (SHARP_F);
      {
	char ** end = addresses;
	while ((*end++) != 0) ;
	end -= 1;
	{
	  SCHEME_OBJECT result =
	    (allocate_marked_vector (TC_VECTOR, (end - addresses), 1));
	  SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
	  unsigned int length = (OS_host_address_length ());
	  while (addresses < end)
	    (*scan_result++) =
	      (memory_to_string (length, ((unsigned char *) (*addresses++))));
	  PRIMITIVE_RETURN (result);
	}
      }
    });
}

DEFINE_PRIMITIVE ("GET-HOST-NAME", Prim_get_host_name, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  SOCKET_CODE
    ({
      CONST char * host_name = (OS_get_host_name ());
      if (host_name == 0)
	PRIMITIVE_RETURN (SHARP_F);
      {
	SCHEME_OBJECT result = (char_pointer_to_string (host_name));
	OS_free ((PTR) host_name);
	PRIMITIVE_RETURN (result);
      }
    });
}

DEFINE_PRIMITIVE ("CANONICAL-HOST-NAME", Prim_canonical_host_name, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  SOCKET_CODE
    ({
      CONST char * host_name = (OS_canonical_host_name (STRING_ARG (1)));
      if (host_name == 0)
	PRIMITIVE_RETURN (SHARP_F);
      {
	SCHEME_OBJECT result = (char_pointer_to_string (host_name));
	OS_free ((PTR) host_name);
	PRIMITIVE_RETURN (result);
      }
    });
}

DEFINE_PRIMITIVE ("GET-HOST-BY-ADDRESS", Prim_get_host_by_address, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  SOCKET_CODE
    ({
      CONST char * host_name = (OS_get_host_by_address (STRING_ARG (1)));
      if (host_name == 0)
	PRIMITIVE_RETURN (SHARP_F);
      {
	SCHEME_OBJECT result = (char_pointer_to_string (host_name));
	OS_free ((PTR) host_name);
	PRIMITIVE_RETURN (result);
      }
    });
}

DEFINE_PRIMITIVE ("HOST-ADDRESS-ANY", Prim_host_address_any, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  SOCKET_CODE
    ({
      SCHEME_OBJECT result = (allocate_string (OS_host_address_length ()));
      OS_host_address_any (STRING_LOC (result, 0));
      PRIMITIVE_RETURN (result);
    });
}

DEFINE_PRIMITIVE ("HOST-ADDRESS-LOOPBACK", Prim_host_address_loopback, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  SOCKET_CODE
    ({
      SCHEME_OBJECT result = (allocate_string (OS_host_address_length ()));
      OS_host_address_loopback (STRING_LOC (result, 0));
      PRIMITIVE_RETURN (result);
    });
}

DEFINE_PRIMITIVE ("NEW-OPEN-TCP-STREAM-SOCKET", Prim_new_open_tcp_stream_socket, 3, 3,
  "Given HOST-ADDRESS and PORT-NUMBER, open a TCP stream socket.\n\
The opened socket is stored in the cdr of WEAK-PAIR.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  SOCKET_CODE
    ({
      SET_PAIR_CDR
	((ARG_REF (3)),
	 (long_to_integer
	  (OS_open_tcp_stream_socket ((arg_host (1)),
				      (arg_nonnegative_integer (2))))));
      PRIMITIVE_RETURN (SHARP_T);
    });
}

DEFINE_PRIMITIVE ("NEW-OPEN-UNIX-STREAM-SOCKET", Prim_new_open_unix_stream_socket, 2, 2,
  "Open the unix stream socket FILENAME.\n\
The opened socket is stored in the cdr of WEAK-PAIR.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
#ifdef HAVE_UNIX_SOCKETS
  SET_PAIR_CDR
    ((ARG_REF (2)),
     (long_to_integer (OS_open_unix_stream_socket (STRING_ARG (1)))));
#else
  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);
#endif
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("SHUTDOWN-SOCKET", Prim_shutdown_socket, 2, 2, "")
{
  PRIMITIVE_HEADER (2);
  SOCKET_CODE
    ({
      OS_shutdown_socket ((arg_client_socket (1)),
			  (arg_integer_in_range (2, 1, 4)));
      PRIMITIVE_RETURN (UNSPECIFIC);
    });
}

DEFINE_PRIMITIVE ("NEW-OPEN-TCP-SERVER-SOCKET", Prim_new_open_tcp_server_socket, 2, 2,
  "Given PORT-NUMBER, open TCP server socket.\n\
The opened socket is stored in the cdr of WEAK-PAIR.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
  SOCKET_CODE
    ({
      Tchannel channel = (OS_create_tcp_server_socket ());
      PTR address = (OS_malloc (OS_host_address_length ()));
      OS_host_address_any (address);
      OS_bind_tcp_server_socket
	(channel, address, (arg_nonnegative_integer (1)));
      OS_free (address);
      OS_listen_tcp_server_socket (channel);
      SET_PAIR_CDR ((ARG_REF (2)), (long_to_integer (channel)));
      PRIMITIVE_RETURN (SHARP_T);
    });
}

DEFINE_PRIMITIVE ("CREATE-TCP-SERVER-SOCKET", Prim_create_tcp_server_socket, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  SOCKET_CODE
    ({
      PRIMITIVE_RETURN (long_to_integer (OS_create_tcp_server_socket ()));
    });
}

DEFINE_PRIMITIVE ("BIND-TCP-SERVER-SOCKET", Prim_bind_tcp_server_socket, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  SOCKET_CODE
    ({
      OS_bind_tcp_server_socket ((arg_server_socket (1)),
				 (arg_host (2)),
				 (arg_nonnegative_integer (3)));
      PRIMITIVE_RETURN (UNSPECIFIC);
    });
}

DEFINE_PRIMITIVE ("LISTEN-TCP-SERVER-SOCKET", Prim_listen_tcp_server_socket, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  SOCKET_CODE
    ({
      OS_listen_tcp_server_socket (arg_server_socket (1));
      PRIMITIVE_RETURN (UNSPECIFIC);
    });
}

DEFINE_PRIMITIVE ("NEW-TCP-SERVER-CONNECTION-ACCEPT", Prim_new_tcp_server_connection_accept, 3, 3,
  "Poll SERVER-SOCKET for a connection.\n\
If a connection is available, it is opened and #T is returned;\n\
the opened socket is stored in the cdr of WEAK-PAIR.\n\
Otherwise, if SERVER-SOCKET is non-blocking, returns #F.\n\
Second argument PEER-ADDRESS, if not #F, must be a host address string.\n\
It is filled with the peer's address if given.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  SOCKET_CODE
    ({
      Tchannel server_socket = (arg_server_socket (1));
      PTR peer_host = (((ARG_REF (2)) == SHARP_F) ? 0 : (arg_host (2)));
      Tchannel connection =
	(OS_server_connection_accept (server_socket, peer_host, 0));
      if (connection == NO_CHANNEL)
	PRIMITIVE_RETURN (SHARP_F);
      SET_PAIR_CDR ((ARG_REF (3)), (long_to_integer (connection)));
      PRIMITIVE_RETURN (SHARP_T);
    });
}
