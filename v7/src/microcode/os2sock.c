/* -*-C-*-

$Id: os2sock.c,v 1.16 2001/06/02 01:05:12 cph Exp $

Copyright (c) 1990-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* This conditional encompasses the entire file.  */
#ifndef DISABLE_SOCKET_SUPPORT

/* This definition is necessary for compilation with newer versions of
   the Developer's Toolkit for OS/2.  The newer version default to
   using TCP/IP 4.1, but this code was designed for TCP/IP 4.0.  */
#define TCPV40HDRS

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"
#include "os2.h"
#include "uxsock.h"

#include <types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/un.h>
#include <sys/socket.h>

static Tchannel initialize_stream_socket (int, enum channel_type);
static msg_t * stream_socket_reader (LHANDLE, qid_t, msg_t *, int *);
static void stream_socket_operator
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);

#define VOID_SOCKET_CALL(proc, args)					\
{									\
  while ((proc args) < 0)						\
    {									\
      if ((sock_errno ()) != SOCEINTR)					\
	OS2_error_system_call ((sock_errno ()), syscall_ ## proc);	\
      deliver_pending_interrupts ();					\
    }									\
}

#define VALUE_SOCKET_CALL(proc, args, var)				\
{									\
  while (1)								\
    {									\
      int rc = (proc args);						\
      if (rc >= 0)							\
	{								\
	  var = rc;							\
	  break;							\
	}								\
      if ((sock_errno ()) != SOCEINTR)					\
	OS2_error_system_call ((sock_errno ()), syscall_ ## proc);	\
      deliver_pending_interrupts ();					\
    }									\
}

static void
socket_close_on_abort_1 (void * sp)
{
  (void) soclose (* ((int *) sp));
}

static void
socket_close_on_abort (int s)
{
  int * sp = (dstack_alloc (sizeof (int)));
  (*sp) = s;
  transaction_record_action (tat_abort, socket_close_on_abort_1, sp);
}

Tchannel
OS_open_tcp_stream_socket (void * host, unsigned int port)
{
  int s;
  struct sockaddr_in address;

  transaction_begin ();
  VALUE_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (struct in_addr)));
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (connect, (s,
	       ((struct sockaddr *) (&address)),
	       (sizeof (address))));
  return (initialize_stream_socket (s, channel_type_tcp_stream_socket));
}

Tchannel
OS_open_unix_stream_socket (const char * filename)
{
  int s;
  struct sockaddr_un address;

  transaction_begin ();
  VALUE_SOCKET_CALL (socket, (PF_OS2, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sun_family) = AF_OS2;
  strncpy ((address . sun_path), filename, (sizeof (address . sun_path)));
  VOID_SOCKET_CALL
    (connect, (s,
	       ((struct sockaddr *) (&address)),
	       (sizeof (address))));
  return (initialize_stream_socket (s, channel_type_unix_stream_socket));
}

int
OS_get_service_by_name (const char * service_name, const char * protocol_name)
{
  struct servent * entry
    = (getservbyname (((char *) service_name),
		      ((char *) protocol_name)));
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
  struct hostent * entry = (gethostbyname ((char *) host_name));
  return ((entry == 0) ? 0 : (entry -> h_addr_list));
}

#define HOSTNAMESIZE 1024

const char *
OS_get_host_name (void)
{
  char host_name [HOSTNAMESIZE];
  if (gethostname (host_name, HOSTNAMESIZE))
    OS2_error_anonymous ();
  {
    char * result = (OS_malloc ((strlen (host_name)) + 1));
    strcpy (result, host_name);
    return (result);
  }
}

const char *
OS_canonical_host_name (const char * host_name)
{
  struct hostent * entry = (gethostbyname ((char *) host_name));
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
    = (gethostbyaddr (((char *) host_addr),
		      (OS_host_address_length ()),
		      AF_INET));
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
  (((struct in_addr *) addr) -> s_addr) = INADDR_ANY;
}

void
OS_host_address_loopback (void * addr)
{
  (((struct in_addr *) addr) -> s_addr) = INADDR_LOOPBACK;
}

Tchannel
OS_create_tcp_server_socket (void)
{
  int s;
  VALUE_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  return (initialize_stream_socket (s, channel_type_tcp_server_socket));
}

void
OS_bind_tcp_server_socket (Tchannel channel, void * host, unsigned int port)
{
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (address . sin_addr)));
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (bind, (((int) (CHANNEL_HANDLE (channel))),
	    ((struct sockaddr *) (&address)),
	    (sizeof (struct sockaddr_in))));
}

#ifndef SOCKET_LISTEN_BACKLOG
#define SOCKET_LISTEN_BACKLOG 5
#endif

void
OS_listen_tcp_server_socket (Tchannel channel)
{
  VOID_SOCKET_CALL
    (listen, (((int) (CHANNEL_HANDLE (channel))), SOCKET_LISTEN_BACKLOG));
}

Tchannel
OS_server_connection_accept (Tchannel channel,
			     void * peer_host, unsigned int * peer_port)
{
  static struct sockaddr_in address;
  int s;

  transaction_begin ();
  while (1)
    {
      int address_length = (sizeof (struct sockaddr_in));
      s = (accept (((int) (CHANNEL_HANDLE (channel))),
		   ((struct sockaddr *) (&address)),
		   (&address_length)));
      if (s >= 0)
	break;
      if ((sock_errno ()) == SOCEWOULDBLOCK)
	return (NO_CHANNEL);
      if ((sock_errno ()) != SOCEINTR)
	OS2_error_system_call ((sock_errno ()), syscall_accept);
      deliver_pending_interrupts ();
    }
  socket_close_on_abort (s);
  if (peer_host != 0)
    memcpy (peer_host, (& (address . sin_addr)), (sizeof (struct in_addr)));
  if (peer_port != 0)
    (*peer_port) = (address . sin_port);
  return (initialize_stream_socket (s, channel_type_tcp_stream_socket));
}

static Tchannel
initialize_stream_socket (int s, enum channel_type type)
{
  Tchannel channel = (OS2_allocate_channel ());
  OS2_initialize_channel (channel, s, (CHANNEL_READ | CHANNEL_WRITE), type);
  OS2_start_channel_thread (channel,
			    stream_socket_reader,
			    stream_socket_operator);
  transaction_commit ();
  return (channel);
}

static msg_t *
stream_socket_reader (LHANDLE handle, qid_t qid, msg_t * message, int * eofp)
{
  int nread;
  do
    nread = (recv (((int) handle),
		   (SM_READAHEAD_DATA (message)),
		   (sizeof (SM_READAHEAD_DATA (message))),
		   0));
  while ((nread < 0) && ((sock_errno ()) == SOCEINTR));
  if (nread >= 0)
    {
      (SM_READAHEAD_SIZE (message)) = nread;
      (*eofp) = (nread == 0);
      return (message);
    }
  OS2_destroy_message (message);
  if ((sock_errno ()) == SOCENOTSOCK)
    /* Socket was closed on us -- no need to do anything else.  */
    return (0);
  (*eofp) = 0;
  return (OS2_make_syscall_error ((sock_errno ()), syscall_recv));
}

static void
stream_socket_operator (Tchannel channel, chop_t operation,
			choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  switch (operation)
    {
    case chop_read:
      OS2_channel_thread_read_op (channel, arg1, arg2, arg3);
      break;
    case chop_write:
      VALUE_SOCKET_CALL
	(send, (((int) (CHANNEL_HANDLE (channel))),
		((char *) arg1),
		((size_t) arg2),
		0),
	 (* ((long *) arg3)));
      break;
    case chop_close:
      OS2_channel_thread_close (channel);
      VOID_SOCKET_CALL (soclose, ((int) (CHANNEL_HANDLE (channel))));
      break;
    default:
      OS2_logic_error ("Unknown operation for stream socket.");
      break;
    }
}

#endif /* not DISABLE_SOCKET_SUPPORT */
