/* -*-C-*-

$Id: ntsock.c,v 1.1 1997/10/26 08:03:37 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

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

/* This conditional encompasses the entire file.  */
#ifndef DISABLE_SOCKET_SUPPORT

#include "scheme.h"
#include "nt.h"
#include "ntio.h"
#include "uxsock.h"

#include <winsock.h>

channel_class_t * NT_channel_class_tcp_stream_socket;
channel_class_t * NT_channel_class_tcp_server_socket;

#define VOID_SOCKET_CALL(proc, args)					\
{									\
  if ((proc args) == SOCKET_ERROR)					\
    NT_error_api_call ((WSAGetLastError ()), apicall_ ## proc);		\
}

#define SOCKET_SOCKET_CALL(proc, args, var)				\
{									\
  var = (proc args);							\
  if (var == INVALID_SOCKET)						\
    NT_error_api_call ((WSAGetLastError ()), apicall_ ## proc);		\
}

#define VALUE_SOCKET_CALL(proc, args, var)				\
{									\
  var = (proc args);							\
  if (var == SOCKET_ERROR)						\
    NT_error_api_call ((WSAGetLastError ()), apicall_ ## proc);		\
}

#define CHANNEL_SOCKET(c) ((SOCKET) (CHANNEL_HANDLE (c)))

#define RETURN_SOCKET(s, class)						\
{									\
  Tchannel channel = (NT_make_channel (((HANDLE) (s)), (class)));	\
  transaction_commit ();						\
  return (channel);							\
}

static void
socket_close_on_abort_1 (void * sp)
{
  (void) closesocket (* ((SOCKET *) sp));
}

static void
socket_close_on_abort (SOCKET s)
{
  SOCKET * sp = (dstack_alloc (sizeof (SOCKET)));
  (*sp) = s;
  transaction_record_action (tat_abort, socket_close_on_abort_1, sp);
}

Tchannel
OS_open_tcp_stream_socket (char * host, int port)
{
  SOCKET s;
  struct sockaddr_in address;

  SOCKET_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  transaction_begin ();
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (struct in_addr)));
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (connect, (s,
	       ((struct sockaddr *) (&address)),
	       (sizeof (address))));
  RETURN_SOCKET (s, NT_channel_class_tcp_stream_socket);
}

int
OS_get_service_by_name (const char * service_name, const char * protocol_name)
{
  struct servent * entry = (getservbyname (service_name, protocol_name));
  return ((entry == 0) ? (-1) : (entry -> s_port));
}

unsigned int
OS_host_address_length (void)
{
  return (sizeof (struct in_addr));
}

char **
OS_get_host_by_name (const char * host_name)
{
  struct hostent * entry = (gethostbyname (host_name));
  return ((entry == 0) ? 0 : (entry -> h_addr_list));
}

#ifndef SOCKET_LISTEN_BACKLOG
#define SOCKET_LISTEN_BACKLOG 5
#endif

Tchannel
OS_open_server_socket (unsigned int port, int arg_number)
{
  SOCKET s;
  struct sockaddr_in address;
  {
    unsigned int nb_port = (sizeof (((struct sockaddr_in *) 0) -> sin_port));
    if (((sizeof (unsigned int)) > nb_port)
	&& (port >= (1 << (CHAR_BIT * nb_port))))
      error_bad_range_arg (arg_number);
  }
  transaction_begin ();
  SOCKET_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  (address . sin_addr . s_addr) = INADDR_ANY;
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (bind, (s,
	    ((struct sockaddr *) (& address)),
	    (sizeof (struct sockaddr_in))));
  VOID_SOCKET_CALL (listen, (s, SOCKET_LISTEN_BACKLOG));
  RETURN_SOCKET (s, NT_channel_class_tcp_server_socket);
}

Tchannel
OS_server_connection_accept (Tchannel channel,
			     char * peer_host, int * peer_port)
{
  static struct sockaddr_in address;
  SOCKET s;

  {
    unsigned long nonblockp = (CHANNEL_NONBLOCKING (channel));
    VOID_SOCKET_CALL
      (ioctlsocket, ((CHANNEL_SOCKET (channel)), FIONBIO, (&nonblockp)));
  }
  {
    int address_length = (sizeof (struct sockaddr_in));
    s = (accept ((CHANNEL_SOCKET (channel)),
		 ((struct sockaddr *) (&address)),
		 (&address_length)));
  }
  if (s == INVALID_SOCKET)
    {
      DWORD code = (WSAGetLastError ());
      if (code == WSAEWOULDBLOCK)
	return (NO_CHANNEL);
      NT_error_api_call (code, apicall_accept);
    }
  transaction_begin ();
  socket_close_on_abort (s);
  if (peer_host != 0)
    memcpy (peer_host, (& (address . sin_addr)), (sizeof (struct in_addr)));
  if (peer_port != 0)
    (*peer_port) = (address . sin_port);
  RETURN_SOCKET (s, NT_channel_class_tcp_stream_socket);
}

static long
socket_channel_read (Tchannel channel, void * buffer, unsigned long n_bytes)
{
  int n_read;
  if (CHANNEL_NONBLOCKING (channel))
    {
      long n = (NT_channel_n_read (channel));
      if (n <= 0)
	return (n);
    }
  VALUE_SOCKET_CALL
    (recv, ((CHANNEL_SOCKET (channel)), buffer, n_bytes, 0), n_read);
  return (n_read);
}

static long
socket_channel_write (Tchannel channel, const void * buffer,
		      unsigned long n_bytes)
{
  int n_written;
  VALUE_SOCKET_CALL
    (send, ((CHANNEL_SOCKET (channel)), buffer, n_bytes, 0), n_written);
  return (n_written);
}

static void
socket_channel_close (Tchannel channel, int errorp)
{
  if (((closesocket (CHANNEL_SOCKET (channel))) == SOCKET_ERROR) && errorp)
    NT_error_api_call ((GetLastError ()), apicall_closesocket);
}

static long
socket_channel_n_read (Tchannel channel)
{
  unsigned long n;
  VOID_SOCKET_CALL (ioctlsocket, ((CHANNEL_SOCKET (channel)), FIONREAD, (&n)));
  /* Zero bytes available means "read would block", so return -1.  */
  return ((n == 0) ? (-1) : n);
}

static long
server_channel_read (Tchannel channel, void * buffer, unsigned long n_bytes)
{
  error_external_return ();
  return (0);
}

static long
server_channel_write (Tchannel channel, const void * buffer,
		      unsigned long n_bytes)
{
  error_external_return ();
  return (0);
}

static long
server_channel_n_read (Tchannel channel)
{
  error_external_return ();
  return (0);
}

void
NT_initialize_sockets (void)
{
  {
    channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
    (CHANNEL_CLASS_TYPE (class)) = channel_type_tcp_stream_socket;
    (CHANNEL_CLASS_OP_READ (class)) = socket_channel_read;
    (CHANNEL_CLASS_OP_WRITE (class)) = socket_channel_write;
    (CHANNEL_CLASS_OP_CLOSE (class)) = socket_channel_close;
    (CHANNEL_CLASS_OP_N_READ (class)) = socket_channel_n_read;
    NT_channel_class_tcp_stream_socket = class;
  }
  {
    channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
    (CHANNEL_CLASS_TYPE (class)) = channel_type_tcp_server_socket;
    (CHANNEL_CLASS_OP_READ (class)) = server_channel_read;
    (CHANNEL_CLASS_OP_WRITE (class)) = server_channel_write;
    (CHANNEL_CLASS_OP_CLOSE (class)) = socket_channel_close;
    (CHANNEL_CLASS_OP_N_READ (class)) = server_channel_n_read;
    NT_channel_class_tcp_server_socket = class;
  }
  {
    WSADATA wsa_data;
    WSAStartup ((MAKEWORD (2, 0)), (&wsa_data));
  }
}

#endif /* not DISABLE_SOCKET_SUPPORT */
