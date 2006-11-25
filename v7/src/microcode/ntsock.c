/* -*-C-*-

$Id: ntsock.c,v 1.20 2006/11/25 05:11:48 cph Exp $

Copyright 1997,1998,1999,2001,2002,2003 Massachusetts Institute of Technology
Copyright 2006 Massachusetts Institute of Technology

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

/* This conditional encompasses the entire file.  */
#ifndef DISABLE_SOCKET_SUPPORT

#include "scheme.h"
#include "prims.h"
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
OS_open_tcp_stream_socket (void * host, unsigned int port)
{
  SOCKET s;
  struct sockaddr_in address;

  SOCKET_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  transaction_begin ();
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (address . sin_addr)));
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

unsigned long
OS_get_service_by_number (const unsigned long port_number)
{
  return ((unsigned long) (htons ((unsigned short) port_number)));
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

const char *
OS_get_host_name (void)
{
  unsigned int name_length = 128;
  char * name = (OS_malloc (name_length));
  while (1)
    {
      if ((gethostname (name, name_length)) != SOCKET_ERROR)
	break;
      {
	DWORD code = (WSAGetLastError ());
	if (code != WSAEFAULT)
	  {
	    OS_free (name);
	    NT_error_api_call (code, apicall_gethostname);
	  }
      }
      name_length *= 2;
      name = (OS_realloc (name, name_length));
    }
  return (OS_realloc (name, ((strlen (name)) + 1)));
}

const char *
OS_canonical_host_name (const char * host_name)
{
  struct hostent * entry = (gethostbyname (host_name));
  if (entry == 0)
    return (0);
  {
    char * result = (OS_malloc ((strlen (entry -> h_name)) + 1));
    strcpy (result, (entry -> h_name));
    return (result);
  }
}

const char *
OS_get_host_by_address (const char * host_addr)
{
  struct hostent * entry
    = (gethostbyaddr (host_addr, (OS_host_address_length ()), AF_INET));
  if (entry == 0)
    return (0);
  {
    char * result = (OS_malloc ((strlen (entry -> h_name)) + 1));
    strcpy (result, (entry -> h_name));
    return (result);
  }
}

void
OS_host_address_any (void * addr)
{
  (((struct in_addr *) addr) -> s_addr) = (htonl (INADDR_ANY));
}

void
OS_host_address_loopback (void * addr)
{
  (((struct in_addr *) addr) -> s_addr) = (htonl (INADDR_LOOPBACK));
}

Tchannel
OS_create_tcp_server_socket (void)
{
  SOCKET s;
  transaction_begin ();
  SOCKET_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  RETURN_SOCKET (s, NT_channel_class_tcp_server_socket);
}

void
OS_bind_tcp_server_socket (Tchannel channel, void * host, unsigned int port)
{
  struct sockaddr_in address;
  BOOL one = 1;

  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (address . sin_addr)));
  (address . sin_port) = port;

  VOID_SOCKET_CALL
    (setsockopt, ((CHANNEL_SOCKET (channel)),
		  SOL_SOCKET,
		  SO_REUSEADDR,
		  ((void *) (&one)),
		  (sizeof (one))));
  VOID_SOCKET_CALL
    (bind, ((CHANNEL_SOCKET (channel)),
	    ((struct sockaddr *) (&address)),
	    (sizeof (struct sockaddr_in))));
}

#ifndef SOCKET_LISTEN_BACKLOG
#  define SOCKET_LISTEN_BACKLOG 5
#endif

void
OS_listen_tcp_server_socket (Tchannel channel)
{
  VOID_SOCKET_CALL
    (listen, ((CHANNEL_SOCKET (channel)), SOCKET_LISTEN_BACKLOG));
}

Tchannel
OS_server_connection_accept (Tchannel channel,
			     void * peer_host, unsigned int * peer_port)
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
    memcpy (peer_host,
	    (& (address . sin_addr)),
	    (sizeof (address . sin_addr)));
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

void
OS_shutdown_socket (Tchannel channel, unsigned long how)
{
  VOID_SOCKET_CALL
    (shutdown,
     ((CHANNEL_SOCKET (channel)),
      ((how == 1) ? SD_RECEIVE : (how == 2) ? SD_SEND : SD_BOTH)));
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
  return ((n == 0) ? CHANNEL_N_READ_WOULD_BLOCK : n);
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

/* The runtime system uses this procedure to decide whether an
   accept() call will block.  So test the socket with select() and
   return a one-bit answer.  */

static long
server_channel_n_read (Tchannel channel)
{
  fd_set fds;
  struct timeval tv;
  int ret;

  FD_ZERO (&fds);
  FD_SET ((CHANNEL_SOCKET (channel)), (&fds));
  (tv . tv_sec) = 0;
  (tv . tv_usec) = 0;
  VALUE_SOCKET_CALL (select, (1, (&fds), 0, 0, (&tv)), ret);
  /* Zero bytes available means "accept would block", so return -1.  */
  return ((ret == 0) ? (-1) : 1);
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
